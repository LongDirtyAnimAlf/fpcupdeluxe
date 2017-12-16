unit mainservergui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynCommons,
  mORMot,              // for TSQLLog
  SynLog,              // logging features
  mORMotHttpServer,
  mormotdatamodelserver;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnStartServer: TButton;
    btnStopServer: TButton;
    Memo1: TMemo;
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
  private
    DataServer: TDataServer;
    HTTPServer: TSQLHttpServer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartServerClick(Sender: TObject);
begin
  {$ifndef DELPHI5OROLDER}
  TSynLogTestLog := TSQLLog; // share the same log file with whole mORMot
  {$endif}
  DataServer := TDataServer.Create(ExtractFilePath(paramstr(0))+'data');
  HTTPServer := TSQLHttpServer.Create(InttoStr(PORT_DEFAULT),[DataServer]);
  {$ifdef WITHLOG}
  TSQLLog.Family.EchoToConsole := LOG_VERBOSE;
  {$endif}
  //HTTPServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
  Memo1.Lines.Append('Background server is running.');
  //Memo1.Lines.Append('Cross-Platform wrappers are available at http://localhost:'+InttoStr(PORT_DEFAULT)+'/root/wrapper');
  Memo1.Lines.Append('Background server is running at http://localhost:'+InttoStr(PORT_DEFAULT)+'/'+DataServer.Model.Root);
end;

procedure TForm1.btnStopServerClick(Sender: TObject);
begin
  Memo1.Lines.Append('HTTP server shutdown ... please wait');
  Application.ProcessMessages;
  HTTPServer.Free;
  DataServer.Free;
  memo1.Lines.Append('Shutdown ready.');
end;

end.

