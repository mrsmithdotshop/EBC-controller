{ simple parser to parse the script file for ebcc
  Armin Diehl <ad@ardiehl.de> 09/2023
  May be somewhat inadequate since i have not programmed in pascal for more
  than 25 years }

{$mode objfpc}
unit lineparser;

// if set, any chars (a..z) appended to numbers (w/o white spaces) will be
// ignored, e.g. Cells=6Peaces or curr=6Amps
// or even curr=6Amps.And23Milliamps
{$define AllowAppendCharsToNumber}

interface

uses sysutils,classes;

const
  TK_ERR          = -100;
  TK_ERR_COMMENT  = -101;
  TK_EOL          = -102;
  TK_COMMENT      = -103;
  TK_LAST         = -99;
  TK_EQUAL        = -104;
  TK_VALUE        = -105;
  TK_UNKNOWN      = -1;


type
  TTokenType = (TTokenNone,TTokenCommand,TTokenParam);

TLineParserException = Class(Exception)
Constructor create (aLineNumber, aLinePos:integer; aMessage: string);
public
  err:integer;
  LinePos:integer;
  LineNo:integer;
end;

TLineParserObject=class
  constructor create(aName:string; aValue:integer; aValueRequired : boolean);
public
  name :string;
  value:integer;
  valueRequired : boolean;
end;

TLineParserObjectList=class(TFPList)
destructor destroy; override;
public
  function findName (name : string) : integer;
  function requiresValue (name : string) : boolean;
end;


TLineParser=class
constructor create;
destructor destroy; override;
private
//public
  commands : TLineParserObjectList;
  params   : TLineParserObjectList;
  line     : string;
  lineNo   : integer;
  linePos  : integer;
  lineLen  : integer;
  currWord : string;
public
  tkType  : TTokenType;
  tkValue : extended;

  procedure raiseException(Message : string);
  function getTokenText : string;
  procedure addCommand (name : string; value : integer);        // add command names and values
  procedure addParam   (name : string; value : integer;         // add parameter names and values
                        valueRequired : boolean);               // true if param=number is expected
  procedure addParamAlias (currName,aliasName : string);
  procedure beginLine  (aLineNumber : integer; aLine : string);                        // start parsing a new line
  function findCommand (name : string) : integer;
  function findParam (name : string) : integer;
  function  next : integer;                                     // returns next token
  function expectCommandOrEOL : integer;                        // returns command or raises exception
  function expectParamOrEOL : integer;                          // expects param=value, value in tkValue
  function expectParamOrValueOrEOL : integer;
  function expectValue : integer;
  function expectEOL : integer;

private
  function  nextWord : integer;
  function skipNoise : boolean;
end;

implementation


constructor TLineParserException.create(aLineNumber, aLinePos:integer; aMessage : string);
begin
    inherited create(aMessage);
  LinePos := aLinePos;
  LineNo := aLineNumber;
end;


constructor TLineParserObject.create(aName:string; aValue:integer; aValueRequired : boolean);
begin
  inherited create;
  name := aName;
  value := aValue;
  valueRequired := aValueRequired;
end;

//******************************************************************************

destructor TLineParserObjectList.destroy;
var i : integer;
begin
  for i:= 0 to count-1 do
  begin
    TLineParserObject(items[i]).free;
    items[i] := NIL;
  end;
  inherited;
end;

function TLineParserObjectList.findName (name : string) : integer;
var o : TLineParserObject;
    nameU : string;
begin
  nameU := UpperCase(name);
  for pointer(o) in self do
    if nameU = o.name then exit(o.value);
  result := TK_ERR;
end;

function TLineParserObjectList.requiresValue (name : string) : boolean;
var o : TLineParserObject;
    nameU : string;
begin
  nameU := UpperCase(name);
  for pointer(o) in self do
    if nameU = o.name then exit(o.valueRequired);
  result := false;
end;

//*****************************************************************************
const
  cNoise   : set of char = [' ',#9];
  cWordSep : set of char = [' ',#9,'=','/','{','}'];

constructor TLineParser.create;
begin
  commands := TLineParserObjectList.create;
  params := TLineParserObjectList.create;
end;

destructor TLineParser.destroy;
begin
  commands.free;
  params.free;
  inherited;
end;

function TLineParser.getTokenText : string;
begin
  result := currWord;
end;

procedure TLineParser.addCommand (name : string; value : integer);
begin
  commands.add(TLineParserObject.create(UpperCase(name),value,false));
end;

procedure TLineParser.addParam (name : string; value : integer; valueRequired : boolean);
begin
  params.add(TlineParserObject.create(UpperCase(name),value,valueRequired));
end;

procedure TLineParser.addParamAlias (currName,aliasName : string);
var i : integer;
    nameU : string;
begin
  nameU := UpperCase(currName);
  for i := 0 to params.count-1 do
    if TlineParserObject(params.items[i]).name = nameU then
    begin
	  params.add(TlineParserObject.create(upperCase(aliasName),TlineParserObject(params.items[i]).value,TlineParserObject(params.items[i]).valueRequired));
      exit;
    end;
    raise (TLineParserException.create(0, 0,format('unable to add alias, "%s" not found',[currName])));
end;

procedure TLineParser.beginLine  (aLineNumber : integer; aLine : string);
begin
  Line := aLine;
  LinePos := 1;
  LineLen := length(aLine);
  LineNo := aLineNumber;
end;

function TLineParser.findCommand (name : string) : integer;
begin
  result := commands.findName(name);
end;

function TLineParser.findParam (name : string) : integer;
begin
  result := params.findName(name);
end;

function TLineParser.nextWord : integer;
var i : integer;
    DecimalSeparatorSave : char;
    convOk : boolean;
{$ifdef AllowAppendCharsToNumber}
    tempS : string;
{$endif}
begin
  currWord := '';
  if not skipNoise then exit(TK_EOL);
  if LinePos > LineLen then exit (TK_EOL);
  if Line[LinePos] = '=' then
  begin
    inc(LinePos); exit (TK_EQUAL);
  end;
  while (LinePos <= LineLen) and (not (Line[LinePos] in cWordSep)) do
  begin
    currWord := currWord + Line[LinePos];
    inc(LinePos);
  end;
  result := TK_UNKNOWN;

  if length(currWord) = 0 then exit;
  if (currWord[1] < '0') or (currWord[1] > '9') then exit; // not a number
{$ifdef AllowAppendCharsToNumber}
  tempS := currWord; currWord := '';
  for i := 1 to length(tempS) do
    //if tempS[i] in ['0'..'9','.'] then currWord := currWord + tempS[i];
    if ((tempS[i] >= '0') and (tempS[i] <= '9')) or (tempS[i] = '.') then currWord := currWord + tempS[i];
{$else}
  for i:=1 to length(currWord) do  // check for number
    if not (currWord[i] in ['0'..'9','.']) then exit;
{$endif}
  // convert to number
  convOk := TryStrToFloat(currWord,tkValue);
  if not convOk then
  begin
    DecimalSeparatorSave := FormatSettings.DecimalSeparator;
    if (DecimalSeparatorSave <> '.') then
    begin
      FormatSettings.DecimalSeparator := '.';
      convOk := TryStrToFloat(currWord,tkValue);
      FormatSettings.DecimalSeparator := DecimalSeparatorSave;
    end;
  end;
  if not convOk then
    raise (TLineParserException.create(LineNo, LinePos,format('unable to convert "%s" to a number',[currWord])));
  result := TK_VALUE;
end;

procedure TLineParser.raiseException(Message : string);
begin
  raise (TLineParserException.create(LineNo, LinePos,Message));
end;

function TLineParser.skipNoise : boolean;
var lastWasComment : boolean;
begin

  repeat
    lastWasComment := false;
    if LinePos <= LineLen then
    begin
      while (LinePos < LineLen) and (Line[LinePos] in cNoise) do inc(LinePos);
      if LinePos < LineLen then
        if Line[LinePos] = '{' then  // {} comment
        begin
          inc(LinePos);
          while (LinePos <= LineLen) and (Line[LinePos] <> '}') do
            inc(LinePos);
          if (LinePos > LineLen) then raise (TLineParserException.create(LineNo, LinePos,'comment not closed, } missing'));
          inc(LinePos);
          lastWasComment := true;
        end;
      if LinePos < LineLen then  // // comment
        if Line[LinePos] = '/' then
        begin
          inc(LinePos);
          if LinePos < LineLen then
            if Line[LinePos] = '/' then exit(false);
          raise (TLineParserException.create(LineNo, LinePos,'/ expected'));
        end;
    end else
      exit(false);
  until (not lastWasComment);
  result := (LinePos <= LineLen);
end;

function TLineParser.next : integer;
var tk : integer;
begin
  tkType := TTokenNone;
  tkValue := 0;
  tk := nextWord;
  if tk < TK_LAST then exit (tk);

  // search for a command or a parameter
  tk := findCommand(currWord);
  if tk >= 0 then
  begin
    tkType := TTokenCommand; exit (tk);
  end;
  tk := findParam(currWord);
  if tk >= 0 then
    tkType := TTokenParam;

  exit (tk);
end;


function TLineParser.expectCommandOrEOL : integer;
var tk:integer;
begin
  tk := next;
  if (tk = TK_EOL) or ((tkType = TTokenCommand) and (tk >= 0)) then exit(tk);
  raise (TLineParserException.create(LineNo, LinePos,'command expected'));
end;

function TLineParser.expectParamOrEOL : integer;
var tk : integer;
    tkNameBuf : string;
begin
  result := next;
  if (result = TK_EOL) then exit(result);
  if tkType <> TTokenParam then
    raise (TLineParserException.create(LineNo, LinePos,'parameter name expected'));
  tkNameBuf := currWord;

  if params.requiresValue(tkNameBuf) then
  begin
    tk := next;
    if tk <> TK_EQUAL then
      raise (TLineParserException.create(LineNo, LinePos,'= expected'));
    tk := next;
    if tk <> TK_VALUE then

    raise (TLineParserException.create(LineNo, LinePos,'value expected'));
    currWord := tkNameBuf;
  end else
  begin
    skipNoise;
    currWord := tkNameBuf;
    if LinePos <= LineLen then
      if Line[LinePos] = '=' then
        raise (TLineParserException.create(LineNo, LinePos,format('parameter %s does not support a value',[currWord])));
  end;
end;


function TLineParser.expectParamOrValueOrEOL : integer;
var tk : integer;
    tkNameBuf : string;
begin
  result := next;
  if (result = TK_EOL) then exit(result);
  if (result = TK_VALUE) then exit (result);
  if tkType <> TTokenParam then
    raise (TLineParserException.create(LineNo, LinePos,'parameter name or value expected'));
  tkNameBuf := currWord;

  if params.requiresValue(tkNameBuf) then
  begin
    tk := next;
    if tk <> TK_EQUAL then
      raise (TLineParserException.create(LineNo, LinePos,'= expected'));
    tk := next;
    if tk <> TK_VALUE then
      raise (TLineParserException.create(LineNo, LinePos,'value expected'));
    currWord := tkNameBuf;
  end else
  begin
    skipNoise;
    currWord := tkNameBuf;
    if LinePos <= LineLen then
      if Line[LinePos] = '=' then
        raise (TLineParserException.create(LineNo, LinePos,format('parameter %s does not support a value',[currWord])));
  end;
end;


function TLineParser.expectValue : integer;
begin
  result := next;
  if result <> TK_VALUE then
    raise (TLineParserException.create(LineNo, LinePos,'value expected'));
end;

function TLineParser.expectEOL : integer;
begin
  result := next;
  if (result = TK_EOL) then exit(result);
  raise (TLineParserException.create(LineNo,LinePos,'end of line expected'));
end;

end.
