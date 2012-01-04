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
    FBootstrapCompiler: string;
    FCompiler: string;
    FUpdater: TUpdater;
    function GetFpcDirectory: string;
    function GetLazarusDirectory: string;
    procedure SetFPCDirectory(Directory: string);
    procedure SetLazarusDirectory(Directory: string);
  public
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler;
    //Compiler used to compile compiler sources
    property Compiler: string read FCompiler;
    //Full path to FPC compiler that is installed
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TInstaller }


function Tinstaller.GetFpcDirectory: string;
begin
  Result := FUpdater.FPCDirectory;
end;

function Tinstaller.GetLazarusDirectory: string;
begin
  Result := FUpdater.LazarusDirectory;
end;

procedure Tinstaller.Setfpcdirectory(Directory: string);
begin
  FUpdater.FPCDirectory := Directory;
end;

procedure Tinstaller.Setlazarusdirectory(Directory: string);
begin
  FUpdater.LazarusDirectory := Directory;
end;



function Tinstaller.Getfpc: boolean;
var
  OperationSucceeded: boolean;
begin
  OperationSucceeded:=true;
  if FUpdater.UpdateFPC = false then
  begin
    OperationSucceeded := false;
  end;
  if OperationSucceeded then
  begin
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    // todo: remove hardcoded make
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe',
      '--directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' all', [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Install
    // todo: check where to install
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make',
      '--directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' UPXPROG=echo COPYTREE=echo' + ' install', [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Make crosscompiler for Windows X64:
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make',
      '--directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' OS_TARGET=win64 CPU_TARGET=x86_64' + ' all', [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Install crosscompiler
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make',
      '--directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' UPXPROG=echo COPYTREE=echo' + ' OS_TARGET=win64 CPU_TARGET=x86_64' +
      ' install', [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Create fpc.cfg
    //todo: replace -o path with bin path for resulting compiler; we'll need it for compilation/make above, anyway
    //todo: only generate when it doesn't exist yet
    if
    SysUtils.ExecuteProcess(FPCDirectory + DirectorySeparator + 'bin' +
      DirectorySeparator + 'i386-win32' + DirectorySeparator + 'fpcmkcfg',
      ' -d basepath="' + FPCDirectory + '"' + ' -o "' + FPCDirectory +
      DirectorySeparator + 'bin' + DirectorySeparator + 'i386-win32' + DirectorySeparator +
      'fpc.cfg"', [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
var
  OperationSucceeded: boolean;
begin
  OperationSucceeded := True;
  // Download Lazarus source:
  if OperationSucceeded = True then
    OperationSucceeded := FUpdater.UpdateLazarus;
  // Make (compile)
  // todo: remove hardcoded make, ppc compiler
  if OperationSucceeded then
  begin
    // Make clean, all
    if (SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe',
      '--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'fpc' + ' clean all', [])) <> 0 then
      OperationSucceeded := False;
  end;
  //todo: setup primary config path, dir etc.
  //todo: make shortcut on desktop, maybe start menu
  Result := OperationSucceeded;
end;

constructor Tinstaller.Create;
begin
  FBootstrapCompiler := '';
  FCompiler := '';
  FUpdater := TUpdater.Create;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

