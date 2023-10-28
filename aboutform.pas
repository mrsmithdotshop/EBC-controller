unit aboutForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IpFileBroker, IpHtml, SynHighlighterHTML, lclintf, LazVersion;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Button1: TButton;
    IpHtmlPanel1: TIpHtmlPanel;
    TopPannel: TPanel;
    BottomPannel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    procedure IpHtmlPanel1HotURL(Sender: TObject; const URL: String);
  private
     currURL : string;
  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

uses main;

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  IpHtmlPanel1.SetHtmlFromStr(Format(
  '<html><head>'+
     '<title>About</title>'+
     '</head><body>'+
     '<h1>EBC-Controller</h1>'+
     '<p>A GUI Software for Linux (and Windows) to control ZTE Tech EBC series battery testers and electronic loads intailly written by <a href="https://github.com/JOGAsoft/EBC-controller">JOGAsoft</a></p>'+
     '<p>This <a href="https://github.com/ardiehl/EBC-controller">enhanced version</a> supports the A20 as well as the A40.</p>'+
     '<tt>Version &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; : '+cVersion+'</tt><br>'+
     '<tt>Compiled &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: ' + {$I %DATE%} + ' ' + {$I %TIME%} + '</tt><br>' +
     '<tt>FPC Version &nbsp;&nbsp;&nbsp;: '+  {$I %FPCVERSION%} + '</tt><br>'+
     '<tt>Lazarus Version: %s</tt>' +
     '</body></html>',[laz_version]));
end;

procedure TfrmAbout.IpHtmlPanel1HotClick(Sender: TObject);
begin
  openUrl(currURL);
end;

procedure TfrmAbout.IpHtmlPanel1HotURL(Sender: TObject; const URL: String);
begin
  currURL:=URL;
end;


end.

