/// RESTful ORM server
program testserver;

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 SQLITE3_FASTCALL

uses
  {$ifdef Linux}
  cthreads,
  {$endif}
  {$ifdef MSWINDOWS}
  Windows,             // for AllocConsole
  {$endif}
  Classes,
  SysUtils,
  SynCommons,
  mORMot,              // for TSQLLog
  SynLog,              // logging features
  mORMotHttpServer,
  mormotdatamodelserver;

var DataServer: TDataServer;
    HTTPServer: TSQLHttpServer;
begin
  {$ifdef MSWINDOWS}
  AllocConsole;
  {$endif}
  {$ifndef DELPHI5OROLDER}
  TSynLogTestLog := TSQLLog; // share the same log file with whole mORMot
  {$endif}
  DataServer := TDataServer.Create(ExtractFilePath(paramstr(0))+'data');
  try
    //HTTPServer := TSQLHttpServer.Create(HTTP_PORT,ORMServer
    //          {$ifndef ONLYUSEHTTPSOCKET},'+',useHttpApiRegisteringURI{$endif});
    HTTPServer := TSQLHttpServer.Create(InttoStr(PORT_DEFAULT),[DataServer]);
    try
     {$ifdef WITHLOG}
      TSQLLog.Family.EchoToConsole := LOG_VERBOSE;
     {$endif}
     //HTTPServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
     writeln('Background server is running.'#10);
     writeln('Cross-Platform wrappers are available at http://localhost:',
            PORT_DEFAULT,'/root/wrapper'#10);
      writeln(#13#10'Background server is running at http://localhost:',PORT_DEFAULT,#13#10);
      WriteLn(#13#10'Done - Press ENTER to Exit');
      ConsoleWaitForEnterKey;
      writeln('HTTP server shutdown...');
    finally
      HTTPServer.Free;
    end;
  finally
    DataServer.Free;
  end;
end.
