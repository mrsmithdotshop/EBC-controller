unit shortcuthelpform;
(*
 Show shortcuts as defined in various controls
 ad@ardiehl.de 28.10.2023 *)
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IpHtml,
  menus,lclproc,ExtCtrls,StdCtrls;

type

  { TfrmShortcuts }

  TfrmShortcuts = class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmShortcuts: TfrmShortcuts;

implementation

uses main;

{$R *.lfm}

{ TfrmShortcuts }

const lineBreak = char(10);
procedure TfrmShortcuts.FormCreate(Sender: TObject);
var
  h: string;
  inTab : boolean;
  //f : text;

  procedure endTab;
  begin
    if inTab then
    begin
      inTab := false;
      h := h + '</table>'+lineBreak;
    end;
  end;

  procedure beginTab;
  begin
    if not inTab then
    begin
      inTab := true;
      h := h + '<table>'+lineBreak;
    end;
  end;

  procedure addHead(s: string);
  begin
    endTab;
    h := h + '<h3>'+s+'</h3>'+lineBreak;
  end;

  procedure addKey(key,func: string);
  begin
    beginTab;
    h := h + '<tr><td>'+key+'</td><td>'+func+'</td></tr>'+lineBreak;
  end;

  procedure addKeyC2(Caption,Hint: string; shortCut: tShortCut);
  var
    short,txt,cap : string;
    ch : char;
    p : integer;
  begin
    short := '';
    txt := Hint;
    ch := #0;
    if ShortCut > 0 then
      short := ShortCutToText(ShortCut);

    cap := Caption;
    p := pos('&',cap);
    if (p > 0) then
    begin
      ch := cap[p+1];
      system.delete(cap,p,1);
    end;

    if length(txt) < 1 then
      txt := cap;

    if length(short) < 1 then
      if ch <> #0 then
        short := 'Alt-'+upcase(ch);

    if length(short)>0 then
      addKey(short,txt);
  end;

  procedure addKeyC(c : TComponent);
  begin
    if c is TMenuItem then
      with TMenuItem(c) do
        addKeyC2(Caption,Hint,ShortCut)
    else if c is TButton then
      with TButton(c) do
        addKeyC2(Caption,Hint,0)
    else if c is TCustomLabeledEdit then
      with TCustomLabeledEdit(c) do
        addKeyC2(EditLabel.Caption,EditLabel.Hint,0)
    else if c is TControl then
      with TControl(c) do
        addKeyC2(Caption,Hint,0)
  end;

begin
  h := '<html><head><title>Shortcut keys</title></head><body>'+lineBreak;
  inTab := false;
  addHead('Connect');
  AddKeyC(frmMain.mm_Connect);
  AddKeyC(frmMain.mm_Disconnect);

  addHead('Tabs');
  addKeyC(frmMain.tsCharge);
  addKeyC(frmMain.tsDischarge);
  addKeyC(frmMain.tsProgram);
  addKeyC(frmMain.tsConsole);

  addHead('Data');
  addKeyC(frmMain.mm_savePng);
  addKeyC(frmMain.mm_saveCsv);
  addKeyC(frmMain.mm_setCsvLogFile);
  addKeyC(frmMain.mm_AutoLog);

  addHead('Steps / Program');
  addKeyC(frmMain.mm_stepLoad);
  addKeyC(frmMain.mm_stepEdit);
  addKeyC(frmMain.mm_skipStep);

  addHead('Settings');
  addKeyC(frmMain.mm_Settings);
  addKeyC(frmMain.mm_taskBarName);
  addKeyC(frmMain.mm_LogFileDir);

  addHead('Values');
  addKeyC(frmMain.edtCells);
  addKeyC(frmMain.edtChargeV);
  addKeyC(frmMain.edtCutA);
  addKeyC(frmMain.edtCutM);
  addKeyC(frmMain.edtCutTime);
  addKeyC(frmMain.edtCutEnergy);
  addKeyC(frmMain.edtCutCap);

  addHead('Run');
  addKeyC(frmMain.btnStart);
  addKeyC(frmMain.btnStop);
  addKeyC(frmMain.btnCont);
  addKeyC(frmMain.btnAdjust);
  addKeyC(frmMain.tbxMonitor);

  endTab;
  h := h + '</body>';

  (*system.assign(f,'x.html');
  rewrite(f);
  system.write(f,h);
  system.Close(f);*)

  IpHtmlPanel1.SetHtmlFromStr(h);
end;

end.

