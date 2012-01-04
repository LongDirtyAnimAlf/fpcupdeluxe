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
  Params: string;
begin
  OperationSucceeded:=true;
  if FUpdater.UpdateFPC = false then
  begin
    OperationSucceeded := false;
  end;
  if OperationSucceeded then
  begin
    // Make (clean & all) using bootstrap compiler
    Params:= ' FPC='+FBootstrapCompiler+
      ' --directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' clean all';
    writeln('debug: running make for fpc, params:');
    writeln(Params);
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    // todo: remove hardcoded make
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe', params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Install using newly compiled compiler
    // todo: check where to install
    Params:= ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' --directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' UPXPROG=echo COPYTREE=echo' + ' install';
    writeln('debug: running make install for fpc, params:');
    writeln(Params);
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make',Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Make crosscompiler for Windows X64; do the 64 bit stuff first
    // so we compile for i386 as the last item, hopefully fpc.cfg will
    // default to i386 then.
    if
    SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make',
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
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
      ' crossinstall', [])
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
    Params:= ' -d basepath="' + FPCDirectory + '"' + ' -o "' + FPCDirectory +
      DirectorySeparator + 'bin' + DirectorySeparator + 'i386-win32' + DirectorySeparator +
      'fpc.cfg"';
    writeln('Debug: Running fpcmkcfg, params: ');
    writeln(Params);
    if
    SysUtils.ExecuteProcess(FPCDirectory + DirectorySeparator + 'bin' +
      DirectorySeparator + 'i386-win32' + DirectorySeparator + 'fpcmkcfg',
      Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
var
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded := True;
  // Download Lazarus source:
  if OperationSucceeded = True then
    OperationSucceeded := FUpdater.UpdateLazarus;
  if OperationSucceeded then writeln('debug: lazarus ok') else writeln('debug: lazarus not ok');
  // Make (compile)
  // todo: remove hardcoded make, ppc compiler
  if OperationSucceeded then
  begin
    // Make clean
    //todo: fix for linux
    // Note: you apparently can't pass FPC in the FPC= statement.
    Params:='--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386.exe' + ' clean';
    writeln('debug: lazarus params:');
    writeln(Params);sleep(100);
    if (SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe',
      Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Make all
    Params:='--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386.exe' + ' all';
    writeln('debug: lazarus params:');
    writeln(Params);sleep(100);
    if (SysUtils.ExecuteProcess('C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe',
      Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Build data desktop
    Params:=
    '--pcp=C:\Users\Reinier\AppData\Local\lazarusdev\'+
    ' '+LazarusDirectory+DirectorySeparator+'tools'+DirectorySeparator+'lazdatadesktop'+DirectorySeparator+'lazdatadesktop.lpr';
    writeln('debug: lazarus params:');
    writeln(Params);sleep(100);
    if (SysUtils.ExecuteProcess(LazarusDirectory+DirectorySeparator+'lazbuild',
      Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  //todo: setup primary config path, dir etc.
  //todo: make shortcut on desktop, maybe start menu
  Result := OperationSucceeded;
end;

constructor Tinstaller.Create;
begin
  FBootstrapCompiler := 'c:\lazarus\fpc\2.5.1\bin\i386-win32\ppc386.exe';
  FCompiler := '';
  FUpdater := TUpdater.Create;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

