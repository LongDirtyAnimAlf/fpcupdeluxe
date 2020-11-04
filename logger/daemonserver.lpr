program daemonserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  mORMotHttpServer,
  mormotdatamodelserver,
  daemonapp;

Type

  { TTestDaemon }

  TTestDaemon = Class(TCustomDaemon)
  Private
    fDataServer: TDataServer;
    fHTTPServer: TSQLHttpServer;
  public
    Function Start : Boolean; override;
    Function Stop : Boolean; override;
    Function Pause : Boolean; override;
    Function Continue : Boolean; override;
    Function Execute : Boolean; override;
    Function ShutDown : Boolean; override;
    Function Install : Boolean; override;
    Function UnInstall: boolean; override;
  end;

{ TTestThread }

Procedure AWriteln(MSg : String; B : Boolean);
begin
  Application.Log(etcustom,Msg+BoolToStr(B));
end;

{ TTestDaemon }

function TTestDaemon.Start: Boolean;
begin
  Result:=inherited Start;
  AWriteln('Daemon Start',Result);
  AWriteln('Dir: '+GetAppConfigDir(False)+'data',True);
  if not DirectoryExists(GetAppConfigDir(False))
     then CreateDir(GetAppConfigDir(False));
  fDataServer := TDataServer.Create(GetAppConfigDir(False)+'data');
  // Use apache as proxy ... only listen to localhost !!
  fHTTPServer := TSQLHttpServer.Create({'127.0.0.1:'+}InttoStr(PORT_DEFAULT_SERVER),[fDataServer]);
end;

function TTestDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  AWriteln('Daemon Stop: ',Result);
  FreeAndNil(fHTTPServer);
  FreeAndNil(fDataServer);
end;

function TTestDaemon.Pause: Boolean;
begin
  Result:=inherited Pause;
  AWriteln('Daemon pause: ',Result);
end;

function TTestDaemon.Continue: Boolean;
begin
  Result:=inherited Continue;
  AWriteln('Daemon continue: ',Result);
end;

function TTestDaemon.Execute: Boolean;
begin
  Result:=inherited Execute;
  AWriteln('Daemon execute: ',Result);
end;

function TTestDaemon.ShutDown: Boolean;
begin
  Result:=inherited ShutDown;
  AWriteln('Daemon Shutdown: ',Result);
  Try
    Try
      FreeAndNil(fHTTPServer);
    Except
    End;
  Finally
    FreeAndNil(fDataServer);
  End;
end;

function TTestDaemon.Install: Boolean;
begin
  Result:=inherited Install;
  AWriteln('Daemon Install: ',Result);
end;

function TTestDaemon.UnInstall: boolean;
begin
  Result:=inherited UnInstall;
  AWriteln('Daemon UnInstall: ',Result);
end;

Type

  { TTestDaemonMapper }

  TTestDaemonMapper = Class(TCustomDaemonMapper)
    Constructor Create(AOwner : TComponent); override;
  end;

{ TTestDaemonMapper }

constructor TTestDaemonMapper.Create(AOwner: TComponent);

Var
  D : TDaemonDef;

begin
  inherited Create(AOwner);
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='Test daemon';
  D.Name:='TestDaemon';
  D.DaemonClassName:='TTestDaemon';
  D.WinBindings.ServiceType:=stWin32;
end;

begin
  RegisterDaemonClass(TTestDaemon);
  RegisterDaemonMapper(TTestDaemonMapper);
  Application.Title:='Daemon test application';
  Application.Run;
end.
