unit StepForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  SynEdit, SynHighlighterAny, lineparser, LCLType;

const
  cNaN = 10E999;
  cCmdCCCV = 'C_CCCV';
//  cSettingsFile = 'Settings.conf';

type
  TRunMode = (rmNone, rmMonitor, rmCharging, rmDischarging, rmDischargingCR, rmWait, rmLoop, rmEnd);

{
  TBreakPtr = ^TBreak;
  TBreak = record
    Time: TDateTime;
    Duration: TDateTime;
    Current: Extended;
    Voltage: Extended;
    Capacity: Extended;
    Energy: Extended;
    Next: TBreakPtr;
  end;

  TProgram = record
    Token: Integer;
    Cells: Integer;
    Current: Extended;
    Voltage: Extended;
    BreakPtr: TBreakPtr;
  end;}

  TSteps = record
    Mode: TRunMode;
//    ModeText: string;
    Command: string;
    Cells: Integer;
    TestVal: Extended;
    CutVolt: Extended;
    CutAmp: Extended;
    CutTime: Integer;
    CutAmpTime: Integer;  // Cut N minutes after current < CutAmp
    CutCap: Extended;
    CutEnergy: Extended;
    Loop: Integer;
    LoopCounter: Integer;
    CapI: Boolean;        // Loop as long as capacity increases
    EneI: Boolean;        // Loop as long as energy increases
    CV: Extended;         // Constant voltage for c_CCCV mode
  end;

  { TfrmStep }

  TfrmStep = class(TForm)
    OkButton: TButton;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    N3: TMenuItem;
    N2: TMenuItem;
    MenuItem11: TMenuItem;
    mniNew: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniSaveAs: TMenuItem;
    mniExit: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    memStep: TSynEdit;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    SynStepHighlighter: TSynAnySyn;
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memStepChange(Sender: TObject);
    procedure mniExitClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure mniSaveAsClick(Sender: TObject);
  private
    FCompiled: Boolean;
    FInitialDirIsSet: Boolean;
  public
    procedure Compile;
    property Compiled: Boolean read FCompiled;
    procedure SetInitialDir(ADir: string);
  end;

var
  FSteps: array of TSteps;
  frmStep: TfrmStep;

implementation

{$R *.lfm}

function TestStr(AStr: string; ATest: string): Boolean;
begin
  Result := (Pos(ATest, AStr) = 1);
end;

function MyVal(AStr: string): Extended;
var
  I: Integer;
  s: string;
begin
  Result := 0;
  s := '';
  for I := 1 to Length(AStr) do
  begin
    if AStr[I] in ['0'..'9', '.'] then
    begin
      s := s + AStr[I];
    end;
  end;
  if Length(s) > 0 then
  begin
    Result := StrToFloat(s);
  end;
end;

function GetParam(AStr: string; AParam: Integer): string;
var
  I, S: Integer;
  sl: string;
begin
  S := 1;
  Result := '';
  sl := AStr + ' ';
  for I := 1 to Length(sl) do
  begin
    if sl[I] in [' '] then
    begin
      Dec(AParam);
      if AParam = 0 then
      begin
        Result := Copy(sl, S, I - S);
        Break;
      end;
      S := I + 1;
    end;
  end;
end;

{
Charge:    c_[NiMH|NiCd|LiPo|LiFe|Ac|PB|CCCV] Cells=n Curr=I Volt=U [CutA=I|Cap=C|Energy=W|Time=t|CutAt=t]
Discharge: d_[CC|CP|CR] [Curr=I|Power=P|Res=R] CutV=U [Cap=C|Energy=W|Time=t]
Wait:      Wait t (minutes)
Loop:      Loop [CapI|EneI] n - run steps n more times. CapI/EneI - run as long dis. capacity increases
Cutoffs:   [c/d] CutA=current(A) CutC=capacity(Ah) Energy=energy(Wh) Time=(minutes) CutAt=(minutes)
Examples:  c_NiMH Cells=4 Curr=0.75 CutA=0.2 Time=180
           d_CR Res=10 CutV=2.8 Energy=6 Time=360
           end
}
type
  p_Ids  = (id_p_value = TK_VALUE, id_p_volt = 100,id_p_cells, id_p_curr, id_p_cuta, id_p_cap, id_p_energy, id_p_time, id_p_cutat, id_p_power, id_p_res, id_p_cutv, id_p_capi, id_p_enei, id_p_LAST);
const
  p_Names : array of string =
           ('volt', 'cells', 'curr', 'cuta', 'cap', 'energy', 'time', 'cutat', 'power', 'res', 'cutv', 'capi', 'enei');
  p_ValuesRequired : array of boolean =
           (true  , true   , true  , true  , true , true    , true  , true   , true   , true , true  , false , false);

type
  c_Ids  = (id_c_EOL=TK_EOL, id_c_nimh=0, id_c_nicd, id_c_lipo, id_c_life, id_c_pb, id_c_cccv, id_d_cc, id_d_cp, id_d_cr, id_c_wait, id_c_loop, id_c_end, id_c_stop, id_c_LAST);
const
  c_Names : array of string =
           ('c_nimh','c_nicd', 'c_lipo', 'c_life', 'c_pb', 'c_cccv', 'd_cc', 'd_cp', 'd_cr', 'wait', 'loop', 'end', 'stop');


procedure TfrmStep.Compile;
var
  pa : TLineParser;
  i,tk,n,pm : integer;
  duplicateValue : boolean;
begin
  SetLength(FSteps, memStep.Lines.Count);
  for I := 0 to Length(FSteps) - 1 do
  begin
    FSteps[I].Command := 'END';
    FSteps[I].Mode := rmEND;
  end;

  // init parser
  pa := TLineParser.create;
  n := 0;
  // TODO: fetch commands for c/d from ini file
  for i:= integer(id_c_nimh) to integer(id_c_LAST)-1 do
  begin
    pa.addCommand(c_names[n],i); inc(n);
  end;
  n := 0;
  for i:= integer(id_p_volt) to integer(id_p_last)-1 do
  begin
    pa.addParam(p_names[n],i,p_valuesRequired[n]); inc(n);
  end;
  pa.addParamAlias('volt','voltage');  // // alias, was different in different step files

  try
    try
      N := 0;
      for I := 0 to memStep.Lines.Count - 1 do
      begin
        pa.beginLine(i+1,memStep.Lines.Strings[I]);
        tk := pa.expectCommandOrEOL;
        if tk>=0 then
        begin
          FSteps[N].Mode := rmCharging;
          FSteps[N].Cells := 1;
          FSteps[N].TestVal := 0.1;
          FSteps[N].CutAmp := 0;
          FSteps[N].CutTime := 0;
          FSteps[N].CutVolt := 0.4;
          FSteps[N].CutCap := 0;
          FSteps[N].CutEnergy := 0;
          FSteps[N].CapI := False;
          FSteps[N].EneI := False;
          FSteps[N].LoopCounter := 0;
          FSteps[N].Command:= upperCase(pa.getTokenText);
          FSteps[N].CV := 0;
          case c_Ids(tk) of
            id_c_nimh, id_c_nicd, id_c_lipo, id_c_life, id_c_pb, id_c_cccv: begin
              pm := pa.expectParamOrEOL;
              while(pm>=0) do
              begin
                case p_Ids(pm) of
                  id_p_cap    : FSteps[N].cutCap := pa.tkValue;
                  id_p_energy : FSteps[N].CutEnergy := pa.tkValue;
                  id_p_time   : FSteps[N].CutTime := trunc(pa.tkValue);
                  id_p_cells  : FSteps[N].Cells := trunc(pa.tkValue);
                  id_p_curr   : FSteps[N].TestVal := pa.tkValue;
                  id_p_cutat  : FSteps[N].CutAmpTime := trunc(pa.tkValue);
                  id_p_cuta   : FSteps[N].CutAmp := pa.tkValue;
                  id_p_volt   : FSteps[N].CV := pa.tkValue;
                otherwise pa.raiseException ('unexpected parameter for charge');
                end;
                pm := pa.expectParamOrEOL;
              end;
              inc(n);
            end;
            id_d_cc, id_d_cp, id_d_cr: begin
              if pm = integer(id_d_cr) then
                FSteps[N].Mode := rmDischargingCR
              else
                FSteps[N].Mode := rmDischarging;
              pm := pa.expectParamOrEOL;
              FSteps[N].Cells := 1;
        	  FSteps[N].TestVal := 0.1;
        	  FSteps[N].CutAmp := cNaN;
        	  FSteps[N].CutTime := 0;
        	  FSteps[N].CutVolt := cNaN;

              while(pm>=0) do
              begin
                case p_Ids(pm) of
                  id_p_cap    : FSteps[N].cutCap := pa.tkValue;
                  id_p_energy : FSteps[N].CutEnergy := pa.tkValue;
                  id_p_time   : FSteps[N].CutTime := trunc(pa.tkValue);
                  id_p_cutv   : FSteps[N].CutVolt := pa.tkValue;
                  id_p_res,
                  id_p_power,
                  id_p_curr	: FSteps[N].TestVal := pa.tkValue;
                otherwise pa.raiseException (format('unexpected parameter for discharge (%s)',[pa.getTokenText]));
                end;
                pm := pa.expectParamOrEOL;
              end;
              inc(n);
            end;
            id_c_wait: begin
              pa.expectValue;
              FSteps[N].CutTime := trunc(pa.tkValue);
              inc(n);
            end;
            id_c_loop: begin
              duplicateValue := false;
              FSteps[N].Mode := rmLoop;
              FSteps[N].CapI := false;
              FSteps[N].EneI := false;
              FSteps[N].Loop := 0;
              pm := pa.expectParamOrValueOrEOL;

              while(pm>=0) or (pm = TK_VALUE) do
              begin
                case p_Ids(pm) of
                  id_p_capi   : FSteps[N].capi := true;
                  id_p_enei   : FSteps[N].enei := true;
                  id_p_value  : begin
                    		  	  if duplicateValue then pa.raiseException('Loop value already specified');
                  			  	  FSteps[N].Loop := trunc(pa.tkValue);
                                  duplicateValue := true;
                                end;
                  otherwise pa.raiseException ('unexpected parameter for loop');
                end;
                pm := pa.expectParamOrValueOrEOL;
              end;
              if FSteps[N].Loop = 0 then FSteps[N].Loop := 999999;
              inc(n);
            end;
            id_c_end, id_c_stop: begin
              FSteps[N].Mode := rmEnd;
              inc(n);
            end;

          id_c_eol:; // empty line or comments only
          otherwise pa.raiseException ('unexpected command');

          end;
        end;
      end;
      FCompiled := True;

    except
      on e:TLineParserException do
      begin
        Application.MessageBox(pchar(format('(%d,%d): %s',[e.LineNo, e.LinePos,e.Message])), 'Error in script file', MB_ICONERROR);
        //if visible then
        //begin
           memStep.CaretX:=e.LinePos;
           memStep.CaretY:=e.LineNo;
        //end;
      end;
    end;

  finally
    pa.free;
  end;

end;

{
procedure TfrmStep.Compile;
var
  I, J, N: Integer;
  s, r: string;
begin
  SetLength(FSteps, memStep.Lines.Count);
  for I := 0 to Length(FSteps) - 1 do
  begin
    FSteps[I].Command := 'END';
    FSteps[I].Mode := rmEND;
  end;
  N := 0;
  for I := 0 to memStep.Lines.Count - 1 do
  begin
    s := UpperCase(memStep.Lines.Strings[I]);
    if (Length(s) > 1) and (Copy(s, 1, 2) = '//') then
    begin
      FSteps[N].Mode := rmNone;
      Continue;
    end;
    if Length(GetParam(s, 1)) > 0 then
    begin
      FSteps[N].Mode := rmCharging;
      FSteps[N].Cells := 1;
      FSteps[N].TestVal := 0.1;
      FSteps[N].CutAmp := 0;
      FSteps[N].CutTime := 0;
      FSteps[N].CutVolt := 0.4;
      FSteps[N].CutCap := 0;
      FSteps[N].CutEnergy := 0;
      FSteps[N].CapI := False;
      FSteps[N].EneI := False;
      FSteps[N].LoopCounter := 0;
      J := 2;
      while True do
      begin
        r := GetParam(s, J);
        if Length(r) > 0 then
        begin
          if TestStr(r, 'CAP') then
          begin
            FSteps[N].CutCap := MyVal(r);
          end else if TestStr(r, 'ENERGY') then
          begin
            FSteps[N].CutEnergy := MyVal(r);
          end else if TestStr(r, 'TIME') then
          begin
            FSteps[N].CutTime := Round(MyVal(r));
          end;
        end else
        begin
          Break;
        end;
        Inc(J);
      end;
      r := GetParam(s, 1);
      FSteps[N].Command := r;
      if Copy(r, 1, 2) = 'C_' then
      begin
        J := 2;
        while True do
        begin
          r := GetParam(s, J);
          if Length(r) > 0 then
          begin
            if TestStr(r, 'CELLS') then
            begin
              FSteps[N].Cells := Round(MyVal(r));
            end else if TestStr(r, 'CURR') then
            begin
              FSteps[N].TestVal := MyVal(r);
            end else if TestStr(r, 'CUTAT') then
            begin
              FSteps[N].CutAmpTime := Round(MyVal(r));
            end else if TestStr(r, 'CUTA') then
            begin
              FSteps[N].CutAmp := MyVal(r);
            end else if TestStr(r, 'VOLT') then
            begin
              FSteps[N].CV:= MyVal(r);
            end;
          end else
          begin
            Break;
          end;
          Inc(J);
        end;
      end else if Copy(r, 1, 2) = 'D_' then
      begin
        if r = 'D_CR' then
        begin
          FSteps[N].Mode := rmDischargingCR;
        end else
        begin
          FSteps[N].Mode := rmDischarging;
        end;
        FSteps[N].Cells := 1;
        FSteps[N].TestVal := 0.1;
        FSteps[N].CutAmp := cNaN;
        FSteps[N].CutTime := 0;
        FSteps[N].CutVolt := cNaN;
        J := 2;
        while True do
        begin
          r := GetParam(s, J);
          if Length(r) > 0 then
          begin
            if TestStr(r, 'CUTV') then
            begin
              FSteps[N].CutVolt := MyVal(r);
            end else if TestStr(r, 'CURR') or TestStr(r, 'RES') or TestStr(r, 'POW') then
            begin
              FSteps[N].TestVal := MyVal(r);
            end;
          end else
          begin
            Break;
          end;
          Inc(J);
        end;
      end else if r = 'WAIT' then
      begin
        FSteps[N].Mode := rmWait;
        r := GetParam(s, 2);
        FSteps[N].CutTime := Round(MyVal(r));
      end else if r = 'LOOP' then
      begin
        FSteps[N].Mode := rmLoop;
        J := 2;
        while True do
        begin
          r := GetParam(s, J);
          if r > '' then
          begin
            if TestStr(r, 'CAPI') then
            begin
              FSteps[N].CapI := True;
            end else if TestStr(r, 'ENEI') then
            begin
              FSteps[N].EneI := True;
            end else
            begin
              FSteps[N].Loop := Round(MyVal(r));
            end;
          end else
          begin
            Break;
          end;
          Inc(J);
        end;
        if FSteps[N].Loop = 0 then FSteps[N].Loop := 999999;
      end else if (Copy(r, 1, 4) = 'STOP') or (Copy(r, 1, 3) = 'END') then
      begin
        FSteps[N].Mode := rmEnd;
      end else
      begin
        Dec(N);
      end;
      Inc(N);
    end;
  end;
  FCompiled := True;
end;
}

procedure TfrmStep.SetInitialDir(ADir: string);
begin
  if not FInitialDirIsSet then
  begin
    sdSave.InitialDir := ADir;
    odOpen.InitialDir := ADir;
    FInitialDirIsSet := True;
  end;
end;

procedure TfrmStep.FormCreate(Sender: TObject);
begin
  FCompiled := False;
  FInitialDirIsSet := False;
end;

procedure TfrmStep.OkButtonClick(Sender: TObject);
begin
  // AD: compile and return to editor on error if not in view mode
  if frmStep.memStep.Enabled then
  begin
    compile;
    if FCompiled then modalResult := mrOK
    else ActiveControl := memStep;
  end else
    modalResult := mrOK;
end;

procedure TfrmStep.memStepChange(Sender: TObject);
begin
  FCompiled := False;
end;

procedure TfrmStep.mniExitClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStep.mniNewClick(Sender: TObject);
begin
  memStep.Lines.Clear;
  frmStep.Caption := 'Unsaved';
end;

procedure TfrmStep.mniOpenClick(Sender: TObject);
begin
  if odOpen.Execute then
  begin
    memStep.Lines.LoadFromFile(odOpen.FileName);
    frmStep.Caption := odOpen.FileName;
    sdSave.FileName := odOpen.FileName;
    compile;
  end;
end;

procedure TfrmStep.mniSaveClick(Sender: TObject);
begin
  if sdSave.FileName > '' then
  begin
    memStep.Lines.SaveToFile(sdSave.FileName);
    frmStep.Caption := sdSave.FileName;
  end else
  begin
    mniSaveAsClick(Sender);
  end;
end;

procedure TfrmStep.mniSaveAsClick(Sender: TObject);
begin
  if sdSave.Execute then
  begin
    memStep.Lines.SaveToFile(sdSave.FileName);
    frmStep.Caption := sdSave.FileName;
  end;
end;

end.

