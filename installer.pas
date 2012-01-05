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
    FMake: string;
    FUpdater: TUpdater;
    function GetFpcDirectory: string;
    function GetFPCUrl: string;
    function GetLazarusDirectory: string;
    function GetLazarusUrl: string;
    procedure SetFPCDirectory(Directory: string);
    procedure SetFPCUrl(AValue: string);
    procedure SetLazarusDirectory(Directory: string);
    procedure SetLazarusUrl(AValue: string);
  public
    property Compiler: string read FCompiler; //Full path to FPC compiler that is installed by this program
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler; //Compiler used to compile compiler sources
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property FPCURL: string read GetFPCUrl write SetFPCUrl; //SVN URL for FPC
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    property LazarusURL: string read GetLazarusUrl write SetLazarusUrl; //SVN URL for Lazarus
    property Make: string read FMake write FMake;
    constructor Create;
    destructor Destroy; override;
  end;
  procedure debugln(Message: string); //Uses writeln for now, and waits a bit afterwards so output is hopefully not garbled

implementation

procedure debugln(Message: string);
begin
  writeln('Debug: '+Message);
  sleep(100); //allow output to be written
end;

{ TInstaller }


function Tinstaller.GetFpcDirectory: string;
begin
  Result := FUpdater.FPCDirectory;
end;

function TInstaller.GetFPCUrl: string;
begin
  result:=FUpdater.FPCURL;
end;

function Tinstaller.GetLazarusDirectory: string;
begin
  Result := FUpdater.LazarusDirectory;
end;

function TInstaller.GetLazarusUrl: string;
begin
  result:=FUpdater.LazarusURL;
end;

procedure Tinstaller.SetFPCDirectory(Directory: string);
begin
  FUpdater.FPCDirectory := Directory;
end;

procedure TInstaller.SetFPCUrl(AValue: string);
begin
  FUpdater.FPCURL:=AValue;
end;

procedure Tinstaller.SetLazarusDirectory(Directory: string);
begin
  FUpdater.LazarusDirectory := Directory;
end;

procedure TInstaller.SetLazarusUrl(AValue: string);
begin
  FUpdater.LazarusURL:=AValue;
end;

function Tinstaller.Getfpc: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded:=true;
  if FUpdater.UpdateFPC = false then
  begin
    OperationSucceeded := false;
  end;
  if OperationSucceeded then
  begin
    // Make clean using bootstrap compiler
    // Note no error on failure, might be recoverable
    Executable:=FMake;
    Params:= ' FPC='+FBootstrapCompiler+
      ' --directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' clean';
    debugln('Running make clean for fpc:');
    debugln(Executable + ' ' + Params);
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    SysUtils.ExecuteProcess(Executable, params, []);
  end;

  if OperationSucceeded then
  begin
    // Make (clean & all) using bootstrap compiler
    Executable:=FMake;
    Params:= ' FPC='+FBootstrapCompiler+
      ' --directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' all';
    debugln('debug: running make for fpc:');
    debugln(Executable + ' ' + Params);
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    if
    SysUtils.ExecuteProcess(Executable, params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Install using newly compiled compiler
    // todo: check where to install
    Executable:=FMake;
    Params:= ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' --directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' UPXPROG=echo COPYTREE=echo' + ' install';
    debugln('debug: running make install for fpc:');
    debugln(Executable + ' ' + Params);
    if SysUtils.ExecuteProcess(Executable,Params, []) <>0    then     OperationSucceeded:=false;
  end;
  { //don't know if this is needed
  if OperationSucceeded then
  begin
    // Make crosscompiler for Windows X64
    Executable:=FMake;
    Params:=' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      '--directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' OS_TARGET=win64 CPU_TARGET=x86_64' + ' all'
    debugln('Running Make crosscompile:');
    debugln(Executable + ' ' + Params);
    if
    SysUtils.ExecuteProcess(Executable,Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  }
  if OperationSucceeded then
  begin
    // Install crosscompiler
    Executable:=FMake;
    debugln('Running Make crossinstall:');
    debugln(Executable + ' ' + Params);
    Params:='--directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' UPXPROG=echo COPYTREE=echo' + ' OS_TARGET=win64 CPU_TARGET=x86_64' +
      ' crossinstall';
    if
    SysUtils.ExecuteProcess(Executable,Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Create fpc.cfg
    //todo: replace -o path with bin path for resulting compiler; we'll need it for compilation/make above, anyway
    //todo: only generate when it doesn't exist yet
    //todo: seems to generate 64 bit config!??!
    Executable:=FPCDirectory + DirectorySeparator + 'bin' +
      DirectorySeparator + 'i386-win32' + DirectorySeparator + 'fpcmkcfg';
    Params:= ' -d basepath="' + FPCDirectory + '"' + ' -o "' + FPCDirectory +
      DirectorySeparator + 'bin' + DirectorySeparator + 'i386-win32' + DirectorySeparator +
      'fpc.cfg"';
    debugln('Debug: Running fpcmkcfg: ');
    debugln(Executable + ' ' + Params);
    if
    SysUtils.ExecuteProcess(Executable,
      Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded := True;
  // Download Lazarus source:
  if OperationSucceeded = True then
    OperationSucceeded := FUpdater.UpdateLazarus;
  if OperationSucceeded then debugln('debug: lazarus ok') else debugln('debug: lazarus not ok');
  // Make (compile)
  // todo: remove hardcoded make, ppc compiler
  if OperationSucceeded then
  begin
    // Make clean; failure here might be recoverable, so no fiddling with OperationSucceeded
    //todo: fix for linux
    // Note: you apparently can't pass FPC in the FPC= statement.
    Executable:=FMake;
    Params:='--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386.exe' + ' clean';
    debugln('Lazarus: running make clean:');
    debugln(Executable + ' ' + Params);
    (SysUtils.ExecuteProcess(Executable,
      Params, []));
  end;
  if OperationSucceeded then
  begin
    // Make all
    Executable:=FMake;
    Params:='--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386.exe' + ' all';
    debugln('Lazarus: running make all:');
    debugln(Executable + ' ' + Params);
    if (SysUtils.ExecuteProcess(Executable,
      Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Build data desktop, nice example of building with lazbuild
    Executable:=LazarusDirectory+DirectorySeparator+'lazbuild';
    Params:=
    '--pcp=C:\Users\Reinier\AppData\Local\lazarusdev\'+
    ' '+LazarusDirectory+DirectorySeparator+'tools'+DirectorySeparator+'lazdatadesktop'+DirectorySeparator+'lazdatadesktop.lpr';
    debugln('Lazarus: compiling data desktop:');
    debugln(Executable + ' ' + Params);
    if (SysUtils.ExecuteProcess(Executable,
      Params, [])) <> 0 then
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
  FMake:='';
  FUpdater := TUpdater.Create;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

