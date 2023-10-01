unit MyIniFile;
{$mode objfpc}
interface

uses
  IniFiles, SysUtils, Classes, Forms, Types, strutils;

type
  TMyIniFile = class(TIniFile)
  private
    function StrToHex(s: string): string;
    function HexToStr(s: string): string;
  public
    procedure ReadStrings(const Section, Ident: string; var AStrings: TStringList; ADefault: string = '');
    procedure WriteStrings(const Section, Ident: string; const AStrings: TStringList);
    procedure WriteHexString(const Section, Ident, Value: string);
    function ReadHexString(const Section, Ident: string; const ADefault: string = ''): string;
    procedure ReadHexStrings(const Section, Ident: string; var AStrings: TStringList; ADefault: string = '');
    procedure WriteHexStrings(const Section, Ident: string; const AStrings: TStringList);
    // AD 10/2023: support for Ident=int,int...
    procedure ReadIntegers(const Section, Ident: string; var ints: TIntegerDynArray);
    procedure WriteIntegers(const Section, Ident: string; const ints:TIntegerDynArray);
  end;

function RowIdent(const Ident: string; I: Integer): string;

implementation
{
  MBY 2008

  Denna unit implementerar extra funktioner hos inifiler.
  WriteHexStr och ReadHexStr konverterar en sträng till en hexsträng så
  att godtyckliga bytes (t.ex. ascii #0 och #26) kan skrivas/läsas.
}

function RowIdent(const Ident: string; I: Integer): string;
begin
  Result := Ident + '_' + IntToStr(I);
end;
{ TMyIniFile }


function TMyIniFile.HexToStr(s: string): string;
var
  I, V: Integer;
begin
  I := 1;
  V := 1;
  Result := '';
  SetLength(Result, Length(s) div 2);
  while I < Length(s) do
  begin
    Result[V] := Chr(StrToIntDef('$' + Copy(s, I, 2), 0));
    Inc(V);
    Inc(I, 2);
  end;
end;

function TMyIniFile.ReadHexString(const Section, Ident: string; const ADefault: string = ''): string;
begin
  Result := HexToStr(ReadString(Section, Ident, ADefault));
end;

procedure TMyIniFile.WriteStrings(const Section, Ident: string; const AStrings: TStringList);
var
  I: Integer;
begin
  for I := 0 to AStrings.Count - 1 do
  begin
    WriteString(Section, RowIdent(Ident, I), AStrings.Strings[I]);
  end;
end;

procedure TMyIniFile.ReadStrings(const Section, Ident: string; var AStrings: TStringList; ADefault: string = '');
var
  I: Integer;
begin
//  AStrings.Clear;
  I := 0;
  while ValueExists(Section, RowIdent(Ident, I)) do
  begin
    AStrings.Add(ReadString(Section, RowIdent(Ident, I), ADefault));
    Inc(I);
  end;
end;

function TMyIniFile.StrToHex(s: string): string;
var
  I, V: Integer;
  h: string;
begin
  Result := '';
  SetLength(Result, 2 * Length(s));
  V := 1;
  for I := 1 to Length(s) do
  begin
    h := IntToHex(Ord(s[I]), 2);
    Result[V] := h[1];
    Result[V + 1] := h[2];
    Inc(V, 2);
  end;
end;

procedure TMyIniFile.WriteHexString(const Section, Ident, Value: String);
begin
  WriteString(Section, Ident, StrToHex(Value));
end;

procedure TMyIniFile.ReadHexStrings(const Section, Ident: string;
  var AStrings: TStringList; ADefault: string);
var
  I: Integer;
begin
  AStrings.Clear;
  I := 0;
  while ValueExists(Section, RowIdent(Ident, I)) do
  begin
    AStrings.Add(ReadHexString(Section, RowIdent(Ident, I), ADefault));
    Inc(I);
  end;
end;

procedure TMyIniFile.WriteHexStrings(const Section, Ident: string;
  const AStrings: TStringList);
var
  I: Integer;
begin
  for I := 0 to AStrings.Count - 1 do
  begin
    WriteHexString(Section, RowIdent(Ident, I), AStrings.Strings[I]);
  end;
end;

procedure TMyIniFile.ReadIntegers(const Section, Ident: string; var ints:TIntegerDynArray);
var
   ss: TStringDynArray;
   i,size : Integer;
   s : string;
begin
  setLength(ints,0);
  s := ReadString(Section, Ident, '');
  if length(s) < 1 then exit;
  ss := SplitString(s,',');
  size := 0;
  for i := low(ss) to high(ss) do
  begin
    inc(size); setLength(ints,size);
    ints[size-1] := StrToInt(ss[i]);
  end;
end;

procedure TMyIniFile.WriteIntegers(const Section, Ident: string; const ints:TIntegerDynArray);
var
   s: string;
   i : Integer;
begin
  s := '';
  for i in ints do
  begin
    if length(s) > 0 then s := s + ',';
    s := s + IntToStr(i);
  end;
  WriteString(Section,Ident,s);
end;

end.
