unit installer;
{
Gets/updates/compiles/installs FPC/Lazarus sources
Uses updater unit to get/update the sources.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, updater;

type

  { TInstaller }

  TInstaller = class(TObject)
  private
    FUpdater: TUpdater;
    function GetFpcDirectory: string;
    function GetLazarusDirectory: string;
    procedure SetFPCDirectory(Directory: string);
    procedure SetLazarusDirectory(Directory: string);
  public
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TInstaller }

function Tinstaller.GetFpcDirectory: String;
begin
  result:=FUpdater.FPCDirectory;
end;

function Tinstaller.GetLazarusDirectory: String;
begin
  result:=FUpdater.LazarusDirectory;
end;

procedure Tinstaller.Setfpcdirectory(Directory: String);
begin
  FUpdater.FPCDirectory:=Directory;
end;

procedure Tinstaller.Setlazarusdirectory(Directory: String);
begin
  FUpdater.LazarusDirectory:=Directory;
end;

function Tinstaller.Getfpc: Boolean;
var
  OperationSucceeded: boolean;
begin
  if FUpdater.UpdateFPC=true then
  begin
    OperationSucceeded:=true;
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    // todo: remove hardcoded make
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe', '--directory='+FPCDirectory + ' UPXPROG=echo COPYTREE=echo all', []);
    // Install
    // todo: check where to install
    // todo: specify ppcxx compiler
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe', '--directory='+FPCDirectory + ' UPXPROG=echo COPYTREE=echo install', []);
    // todo: create fpc.cfg
  end;
  //todo: error handling
  result:=true;
end;

function Tinstaller.Getlazarus: Boolean;
var
  OperationSucceeded: boolean;
begin
  OperationSucceeded:=true;
  // Download Lazarus source:
  if OperationSucceeded=true then OperationSucceeded:=FUpdater.UpdateLazarus;
  // Make (compile)
    // todo: remove hardcoded make
  // todo: specify ppcxx compiler
  if OperationSucceeded then
  begin
    if (SysUtils.ExecuteProcess(
      'C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe',
      '--directory='+LazarusDirectory+ ' UPXPROG=echo COPYTREE=echo all',
      []))<>0 then OperationSucceeded:=false;
  end;
  //todo: setup primary config path, dir etc.
  Result:=OperationSucceeded;
end;

constructor Tinstaller.Create;
begin
  FUpdater:=TUpdater.Create;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

