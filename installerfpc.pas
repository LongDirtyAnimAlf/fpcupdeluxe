unit installerFpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller;

Const
  Sequences=
// convention: FPC sequences start with 'FPC'. Used in SetLCL.
//standard fpc build
    'Declare FPC;'+
    'Cleanmodule FPC;'+
    // Create the link early so invalid previous
    // versions are overwritten:
    'Exec CreateFpcupScript;'+
    'Getmodule FPC;'+
    'Buildmodule FPC;'+
    'End;'+

//standard uninstall
    'Declare FPCuninstall;'+
    'Uninstallmodule FPC;'+
    'End;'+

//standard clean
    'Declare FPCclean;'+
    'cleanmodule FPC;'+
    'End;'+

//selective actions triggered with --only=SequenceName
    'Declare FPCCleanOnly;'+
    'Cleanmodule FPC;'+
    'End;'+

    'Declare FPCGetOnly;'+
    'Getmodule FPC;'+
    'End;'+

    'Declare FPCBuildOnly;'+
    'Buildmodule FPC;'+
    'End';



type
  { TFPCInstaller }

  TFPCInstaller = class(TInstaller)
  private
    BinPath:string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
    InitDone:boolean;
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // Retrieves compiler version string
    function GetCompilerVersion(CompilerPath: string): string;
    function CreateFPCScript:boolean;
    function DownloadBootstrapCompiler: boolean;
    function GetFPCVersion: string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
  public
    //Directory that has compiler needed to compile compiler sources. If compiler doesn't exist, it will be downloaded
    property BootstrapCompilerDirectory: string write FBootstrapCompilerDirectory;
    //Optional; URL from which to download bootstrap FPC compiler if it doesn't exist yet.
    property BootstrapCompilerURL: string write FBootstrapCompilerURL;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    function ConfigModule(ModuleName:string): boolean; override;
    // Install update sources
    function GetModule(ModuleName:string): boolean; override;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TFPCNativeInstaller }

  TFPCNativeInstaller = class(TFPCInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TFPCCrossInstaller }

  TFPCCrossInstaller = class(TFPCInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses fpcuputil,fileutil,processutils
  {$IFDEF UNIX}
    ,baseunix
  {$ENDIF UNIX}
  ;
{ TFPCCrossInstaller }

function TFPCCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  CrossInstaller:TCrossInstaller;
  Options:String;
begin
  result:=true;
  // Make crosscompiler using new compiler
  { Note: command line equivalents for Win32=>Win64 cross compiler:
  set path=c:\development\fpc\bin\i386-win32;c:\development\fpcbootstrap
  make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo all OS_TARGET=win64 CPU_TARGET=x86_64
  rem already gives compiler\ppcrossx64.exe, compiler\ppcx64.exe
  make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo crossinstall OS_TARGET=win64 CPU_TARGET=x86_64
  rem gives bin\i386-win32\ppcrossx64.exe

  Note: make install CROSSINSTALL=1 apparently installs, but does NOT install utilities (ld etc?) for that
  platform; see posting Jonas Maebe http://lists.freepascal.org/lists/fpc-pascal/2011-August/030084.html
  make all install CROSSCOMPILE=1??? find out?
  }
  CrossInstaller:=GetCrossInstaller;
  if assigned(CrossInstaller) then
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils', etError)
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries', etError)
    else
      begin
      ProcessEx.Executable := Make;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      infoln('Running Make all (FPC crosscompiler):',etinfo);
      ProcessEx.Parameters.Add('FPC='+FCompiler);
      ProcessEx.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      if CrossInstaller.BinUtilsPath<>'' then
        ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      //putting all before target might help!?!?
      ProcessEx.Parameters.Add('all');
      ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
      ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
      Options:=FCompilerOptions;
      if CrossInstaller.LibsPath<>''then
        Options:=Options+' -Xd -Fl'+CrossInstaller.LibsPath;
      if CrossInstaller.BinUtilsPrefix<>'' then
        begin
        Options:=Options+' -XP'+CrossInstaller.BinUtilsPrefix;
        ProcessEx.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
        end;
      if Options<>'' then
        ProcessEx.Parameters.Add('OPT='+Options);
      ProcessEx.Execute;

      if ProcessEx.ExitStatus = 0 then
        begin
          // Install crosscompiler
          ProcessEx.Executable := Make;
          ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
          ProcessEx.Parameters.Clear;
          infoln('Running Make crossinstall (FPC crosscompiler):', etinfo);
          ProcessEx.Parameters.Add('FPC='+FCompiler);
          ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
          {$IFDEF UNIX}
          ProcessEx.Parameters.Add('INSTALL_BINDIR='+BinPath);
          {$ENDIF UNIX}
          if CrossInstaller.BinUtilsPath<>'' then
            ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(FBaseDirectory));
          ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
          ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
          //putting crossinstall before target might help!?!?
          ProcessEx.Parameters.Add('crossinstall');
          ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target); //cross compile for different OS...
          ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target); // and processor.
          if CrossInstaller.BinUtilsPrefix<>'' then
            begin
            ProcessEx.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
            end;

          // Note: consider this as an optional item, so don't fail the function if this breaks.
          ProcessEx.Execute;
          if ProcessEx.ExitStatus<>0 then
          begin
            infoln('Problem compiling/installing crosscompiler. Continuing regardless.', etwarning);
            FCompiler:='////\\\Error trying to compile FPC\|!';
            {$ifndef win32}
            //fail if this is not crosswin32-64
            result:=false;
            {$endif win32}
            {$ifndef win64}
            //fail if this is not crosswin64-32
            result:=false;
            {$endif win64}
          end
          else
            begin
          {$IFDEF UNIX}
              result:=CreateFPCScript;
          {$ENDIF UNIX}
              GetCompiler;
            end;
        end;
    end
  else
    infoln('Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target,etwarning);
end;


constructor TFPCCrossInstaller.Create;
begin
  inherited create;
end;

destructor TFPCCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TFPCNativeInstaller }

function TFPCNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  OperationSucceeded:boolean;
  FileCounter:integer;
begin
  // Make all/install, using bootstrap compiler.
  // Make all should use generated compiler internally for unit compilation
  {$IFDEF UNIX}
  // the long way: make all, see where to install, install
  ProcessEx.Executable := Make;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  if FCompilerOptions<>'' then
    ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
  ProcessEx.Parameters.Add('all');
  infoln('Running make all for FPC:',etinfo);
  ProcessEx.Execute;
  if ProcessEx.ExitStatus <> 0 then
    OperationSucceeded := False;

  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('INSTALL_BINDIR='+BinPath);
  ProcessEx.Parameters.Add('install');
  infoln('Running make install for FPC:',etinfo);
  ProcessEx.Execute;
  if ProcessEx.ExitStatus <> 0 then
    OperationSucceeded := False;
  {$ELSE UNIX}
  ProcessEx.Executable := Make;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  if FCompilerOptions <>'' then
    ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
  ProcessEx.Parameters.Add('all');
  ProcessEx.Parameters.Add('install');
  infoln('Running make all install for FPC:',etinfo);
  ProcessEx.Execute;
  if ProcessEx.ExitStatus <> 0 then
    OperationSucceeded := False;
  {$ENDIF UNIX}

  {$IFDEF UNIX}
  if OperationSucceeded then
    OperationSucceeded:=CreateFPCScript;
  {$ENDIF UNIX}

  // Let everyone know of our shiny new compiler:
  if OperationSucceeded then
  begin
    GetCompiler;
  end
  else
  begin
    FCompiler:='////\\\Error trying to compile FPC\|!';
  end;

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    //Copy over binutils to new CompilerName bin directory
    try
      for FileCounter:=0 to FBinUtils.Count-1 do
      begin
        FileUtil.CopyFile(IncludeTrailingPathDelimiter(FMakeDir)+FBinUtils[FileCounter], IncludeTrailingPathDelimiter(BinPath)+FBinUtils[FileCounter]);
      end;
      // Also, we can change the make/binutils path to our new environment
      // Will modify fmake as well.
      FMakeDir:=BinPath;
    except
      on E: Exception do
      begin
        infoln('Error copying binutils: '+E.Message,eterror);
        OperationSucceeded:=false;
      end;
    end;
  end;
  {$ENDIF MSWINDOWS}
  result:=OperationSucceeded;
end;

constructor TFPCNativeInstaller.Create;
begin
  inherited create;
end;

destructor TFPCNativeInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TFPCInstaller }

function TFPCInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
result:=true;
end;

function TFPCInstaller.GetCompilerVersion(CompilerPath: string): string;
var
  Output: string;
  ResultCode: longint;
begin
  Output:='';
  ResultCode:=ExecuteCommand(CompilerPath+ ' -iW', Output, FVerbose);
  Output:=StringReplace(Output,LineEnding,'',[rfReplaceAll,rfIgnoreCase]);
  Result:=Output;
end;

function TFPCInstaller.CreateFPCScript: boolean;
var
  FPCScript:string;
  TxtFile:Text;
begin
  {$IFDEF UNIX}
  // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
  // If this fails, Lazarus compilation will fail...
  FPCScript := IncludeTrailingPathDelimiter(BinPath) + 'fpc.sh';
  if FileExists(FPCScript) then
  begin
    infoln('fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.',etinfo);
    sysutils.DeleteFile(FPCScript);
  end;
  AssignFile(TxtFile,FPCScript);
  Rewrite(TxtFile);
  writeln(TxtFile,'#!/bin/sh');
  writeln(TxtFile,'# This script starts the fpc compiler installed by fpcup');
  writeln(TxtFile,'# and ignores any system-wide fpc.cfg files');
  writeln(TxtFile,'# Note: maintained by fpcup; do not edit directly, your edits will be lost.');
  writeln(TxtFile,IncludeTrailingPathDelimiter(BinPath),'fpc  -n @',
       IncludeTrailingPathDelimiter(BinPath),'fpc.cfg -FD'+
       IncludeTrailingPathDelimiter(BinPath)+' "$@"');
  CloseFile(TxtFile);
  Result:=(FPChmod(FPCScript,&700)=0); //Make executable; fails if file doesn't exist=>Operationsucceeded update
  if Result then
  begin
    infoln('Created launcher script for FPC:'+FPCScript,etinfo);
  end
  else
  begin
    infoln('Error creating launcher script for FPC:'+FPCScript,eterror);
  end;
  {$ENDIF UNIX}
end;

function TFPCInstaller.DownloadBootstrapCompiler: boolean;
// Should be done after we have unzip executable (on Windows: in FMakePath)
var
ArchiveDir: string;
BootstrapArchive: string;
CompilerName:string;
ExtractedCompiler: string;
OperationSucceeded: boolean;
begin
OperationSucceeded:=true;
if OperationSucceeded then
begin
  OperationSucceeded:=ForceDirectoriesUTF8(FBootstrapCompilerDirectory);
  if OperationSucceeded=false then infoln('DownloadBootstrapCompiler error: could not create directory '+FBootstrapCompilerDirectory,eterror);
end;

BootstrapArchive := SysUtils.GetTempFileName;
if OperationSucceeded then
begin
  OperationSucceeded:=Download(FBootstrapCompilerURL, BootstrapArchive);
  if FileExists(BootstrapArchive)=false then OperationSucceeded:=false;
end;

if OperationSucceeded then
begin
  {$IFDEF MSWINDOWS}
  ArchiveDir := ExtractFilePath(BootstrapArchive);
  CompilerName:=ExtractFileName(FBootstrapCompiler);
  //Extract zip, overwriting without prompting
  if ExecuteCommand(FUnzip+' -o -d '+ArchiveDir+' '+BootstrapArchive,FVerbose) <> 0 then
    begin
      infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
      OperationSucceeded := False;
    end
    else
    begin
      OperationSucceeded := True; // Spelling it out can't hurt sometimes
    end;
  // Move CompilerName to proper directory
  if OperationSucceeded = True then
  begin
    infoln('Going to rename/move ' + ArchiveDir + CompilerName + ' to ' + FBootstrapCompiler, etinfo);
    renamefile(ArchiveDir + CompilerName, FBootstrapCompiler);
  end;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  //Extract bz2, overwriting without prompting
  if ExecuteCommand(FBunzip2+' -d -f -q '+BootstrapArchive,FVerbose) <> 0 then
    begin
      infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
      OperationSucceeded := False;
    end
    else
    begin
      ExtractedCompiler:=BootstrapArchive+'.out'; //default bzip2 output filename
      OperationSucceeded := True; // Spelling it out can't hurt sometimes
    end;
  // Move compiler to proper directory; note bzip2 will append .out to file
  if OperationSucceeded = True then
  begin
    infoln('Going to move ' + ExtractedCompiler + ' to ' + FBootstrapCompiler,etinfo);
    OperationSucceeded:=MoveFile(ExtractedCompiler,FBootstrapCompiler);
  end;
  if OperationSucceeded then
  begin
    //Make executable
    OperationSucceeded:=(fpChmod(FBootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootstrapCompiler,etwarning);
  end;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  CompilerName:=ExtractFileName(FBootstrapCompiler);
  //Extract .tar.bz2, overwriting without prompting
  CompilerName:=ExtractFileName(FBootstrapCompiler);
  if ExecuteCommand(FTar+' -x -v -j -f '+BootstrapArchive,FVerbose) <> 0 then
  begin
    infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
    OperationSucceeded := False;
  end
  else
  begin
    OperationSucceeded := True; // Spelling it out can't hurt sometimes
  end;
  // Move compiler to proper directory; note bzip2 will append .out to file
  if OperationSucceeded = True then
  begin
    //todo: currently tar spits out uncompressed file in current dir...
    //which might not have proper permissions to actually create file...!?
    infoln('Going to rename/move '+CompilerName+' to '+FBootStrapCompiler,etwarning);
    sysutils.DeleteFile(FBootStrapCompiler); //ignore errors
    // We might be moving files across partitions so we cannot use renamefile
    OperationSucceeded:=FileUtil.CopyFile(CompilerName, FBootStrapCompiler);
    sysutils.DeleteFile(CompilerName);
  end;
  if OperationSucceeded then
  begin
    //Make executable
    OperationSucceeded:=(fpChmod(FBootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootStrapCompiler,eterror);
  end;
  {$ENDIF DARWIN}
end;
if OperationSucceeded = True then
begin
  SysUtils.DeleteFile(BootstrapArchive);
end
else
begin
  infoln('Error getting/extracting bootstrap compiler. Archive: '+BootstrapArchive, eterror);
end;
Result := OperationSucceeded;
end;

function TFPCInstaller.GetFPCVersion: string;
var testcompiler:string;
begin
  testcompiler:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler'+DirectorySeparator+'ppc1';
  if not FileExistsUTF8(testcompiler) then
    begin //darwin
    testcompiler:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler'+DirectorySeparator+'ppc';
    end;
  ExecuteCommand(testcompiler+' -iV',result,FVerbose);
  //Remove trailing LF(s) and other control codes:
  while (length(result)>0) and (ord(result[length(result)])<$20) do
    delete(result,length(result),1);
end;

function TFPCInstaller.InitModule:boolean;
var
  BootstrapVersion: string;
  Output: string;
begin
  result:=true;
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  infoln('TFPCInstaller: initialising...',etDebug);
  if FBootstrapCompiler='' then
    begin  // may need to download it
    {$IFDEF MSWINDOWS}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL:=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-win32-ppc386.zip';
    {$ifdef win64}
    //There is no win64 bootstrap compiler, yet
    //Each time we build, we'll make our own starting with the ppc386.exe bootstrap compiler
    //This should eliminate issues with the wrong RTL etc.
    //FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64.exe';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
    {$ELSE}
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
    {$endif win64}
    {$ENDIF MSWINDOWS}
    {$IFDEF Linux}
    //If compiled for x86 32 bit, install 32 bit
    //If compiled for x64, install x64 only.
    {$IFDEF CPU386}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL :=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-linux-ppc386.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-linux-ppc386-1';
    {$ELSE}
    {$IFDEF cpuarmel}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL :=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/arm-linux-ppcarm.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'arm-linux-ppcarm';
    {$ELSE} // Assume x64 (could also be PowerPC, SPARC I suppose)
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL :=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/x86_64-linux-ppcx64.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-linux-ppcx64';
    {$ENDIF cpuarmel}
    {$ENDIF CPU386}
    {$ENDIF Linux}
    {$IFDEF Darwin}
    //OSX
    //ppcuniversal is not a good bootstrap compiler since it creates a compiler that doesn't handle generics !?!?!?
    //We'll make our own ppc386 starting with the ppcuniversal bootstrap compiler
    //If we made it already pick it up here
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386';
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL:=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/universal-darwin-ppcuniversal.tar.bz2';
    {$ENDIF Darwin}
    end;
  // Only download bootstrap compiler if we can't find a valid one
  if CheckExecutable(FBootstrapCompiler, '-h', 'Free Pascal Compiler') then
    begin
      infoln('Found bootstrap compiler version '+GetCompilerVersion(FBootstrapCompiler),etinfo);
      result:=CheckAndGetNeededExecutables;
    end
    else
    begin
      {$ifdef win64}
      //don't have a win64 bootstrap. Will have to build one later in TFPCInstaller.BuildModule
      // For that, we need to download the i386 compiler.
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
      {$endif win64}
      {$ifdef darwin}
      //don't have a ppc386 bootstrap. Will have to build one later in TFPCInstaller.BuildModule
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
      {$endif win64}
      result:=CheckAndGetNeededExecutables and DownloadBootstrapCompiler;
    end;
  if FCompiler='' then   //!!!Don't use Compiler here. GetCompiler returns installed compiler.
    FCompiler:=FBootstrapCompiler;
  WritelnLog('TFPCInstaller init:',false);
  WritelnLog('Bootstrap compiler dir: '+ExtractFilePath(FCompiler),false);
  WritelnLog('FPC URL:                '+FURL,false);
  WritelnLog('FPC options:            '+FCompilerOptions,false);
  WritelnLog('FPC directory:          '+FBaseDirectory,false);
  {$IFDEF MSWINDOWS}
  WritelnLog('Make/binutils path:     '+FMakeDir,false);
  {$ENDIF MSWINDOWS}
  BinPath:=IncludeTrailingPathDelimiter(FBaseDirectory)+'bin'+DirectorySeparator+GetFPCTarget(true);
  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  // add fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(FBootstrapCompilerDirectory+PathSeparator+
    FMakeDir+PathSeparator+
    FSVNDirectory+PathSeparator+
    IncludeTrailingPathDelimiter(FBaseDirectory)+'utils'+PathSeparator+
    BinPath,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  //add fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(BinPath+PathSeparator+IncludeTrailingPathDelimiter(FBaseDirectory)+'utils',true);
  {$ENDIF UNIX}
  InitDone:=result;
end;

function TFPCInstaller.BuildModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  FPCCfg: string;
  OperationSucceeded: boolean;
  SearchRec:TSearchRec;
  s:string;
  TxtFile:Text;  //cpuarmel
const
  COMPILERNAMES='ppc386,ppcm68k,ppcalpha,ppcpowerpc,ppcpowerpc64,ppcarm,ppcsparc,ppcia64,ppcx64'+
    'ppcross386,ppcrossm68k,ppcrossalpha,ppcrosspowerpc,ppcrosspowerpc64,ppcrossarm,ppcrosssparc,ppcrossia64,ppcrossx64';


begin
  result:=InitModule;
  if not result then exit;
  infoln('TFPCInstaller: building module '+ModuleName+'...',etinfo);
  {$ifdef win64}
  // On win64, we need to build the PPCX64 bootstrap compiler from our
  // PPC386.exe.
  // In cleanmodule which should have run before this, we remove the temp PPCX64
  // bootstrap compiler so we're (almost) guaranteed it builds...
  if pos('ppc386.exe',FCompiler)>0 then //need to build ppcx64 before
    begin
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler';
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler');
    ProcessEx.Parameters.Add('OS_TARGET=win64');
    ProcessEx.Parameters.Add('CPU_TARGET=x86_64');
    ProcessEx.Parameters.Add('cycle');
    infoln('Running make cycle for FPC64:',etinfo);
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      begin
      result := False;
      WritelnLog('FPC: Failed to build ppcx64 bootstrap compiler ');
      exit;
    end;
    FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/ppcx64.exe',
     ExtractFilePath(FCompiler)+'ppcx64.exe');
    // Now we can change the compiler from the i386 to the x64 compiler:
    FCompiler:=ExtractFilePath(FCompiler)+'ppcx64.exe';
    end;
  {$endif win64}
  {$ifdef darwin}
  if pos('ppcuniversal',FCompiler)>0 then //need to build ppc386 before
    begin
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler';
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler');
    ProcessEx.Parameters.Add('CPU_TARGET=i386');
    ProcessEx.Parameters.Add('cycle');
    infoln('Running make cycle for FPC i386:',etinfo);
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      begin
      result := False;
      WritelnLog('FPC: Failed to build ppc386 bootstrap compiler ');
      exit;
    end;
    FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/ppc386',
     ExtractFilePath(FCompiler)+'ppc386');
    FCompiler:=ExtractFilePath(FCompiler)+'ppc386';
    fpChmod(FCompiler,&755);
    end;
  {$endif darwin}
  OperationSucceeded:=BuildModuleCustom(ModuleName);
  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
  // copy the freshly created compiler to the bin/$fpctarget directory so that
  // fpc can find it
  if FindFirst(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/ppc*',faAnyFile,SearchRec)=0 then
    repeat
      s:=SearchRec.Name;
      if (length(s)>4) and (pos(s,COMPILERNAMES) >0) then  //length(s)>4 skips ppc3
        begin
        OperationSucceeded:=OperationSucceeded and
          FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s,
           IncludeTrailingPathDelimiter(BinPath)+s);
        OperationSucceeded:=OperationSucceeded and
          (0=fpChmod(IncludeTrailingPathDelimiter(BinPath)+s,&755));
        end;
    until FindNext(SearchRec)<>0;
  FindClose(SearchRec);
  // create link 'units' below FBaseDirectory to <somewhere>/lib/fpc/$fpcversion/units
  DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'units');
  fpSymlink(pchar(IncludeTrailingPathDelimiter(FBaseDirectory)+'lib/fpc/'+GetFPCVersion+'/units'),
  pchar(IncludeTrailingPathDelimiter(FBaseDirectory)+'units'));
  end;

  {$ENDIF UNIX}

  //todo: after fpcmkcfg create a config file for fpkpkg or something
  if OperationSucceeded then
  begin
    // Create fpc.cfg if needed
    FPCCfg := IncludeTrailingPathDelimiter(BinPath) + 'fpc.cfg';
    if FileExists(FPCCfg) = False then
    begin
      ProcessEx.Executable := IncludeTrailingPathDelimiter(BinPath) + 'fpcmkcfg';
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.clear;
      ProcessEx.Parameters.Add('-d');
      ProcessEx.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('-o');
      ProcessEx.Parameters.Add('' + FPCCfg + '');
      infoln('Creating fpc.cfg:',etinfo);
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
        OperationSucceeded := False;
    {$IFDEF UNIX}
    {$IFDEF cpuarmel}
      // Need to add multiarch library search path
      AssignFile(TxtFile,FPCCfg);
      Append(TxtFile);
      Writeln(TxtFile,'# multiarch library search path');
      Writeln(TxtFile,'-Fl/usr/lib/$fpctarget-*');
      CloseFile(TxtFile);
    {$ENDIF armelcpu}
    {$ENDIF UNIX}
    end
    else
    begin
      infoln('fpc.cfg already exists; leaving it alone.',etinfo);
    end;
  end;

  if OperationSucceeded then
    WritelnLog('FPC: update succeeded at revision number '+ AfterRevision,false);
  Result := OperationSucceeded;
end;

function TFPCInstaller.CleanModule(ModuleName: string): boolean;
// Currently, this function implements "nuclear" cleaning: it removes .ppu files
// etc
// However, it is much faster than running make distclean and avoids fpmake bugs
//todo: add an svn up to the current local revision before running clean. This should restore behaviour that --clean gives the same effect as make clean (i.e. situation after say svn co)
var
  DeleteList: TStringList;
  CPU_OSSignature:string;
begin
  result:=InitModule;
  if not result then exit;
  // Fool make into thinking it's clean. Well, fool?!?
  if (FCrossOS_Target='') and (FCrossCPU_Target='') then
  begin
    infoln('FPC: running make distclean equivalent:',etinfo);
    CPU_OSSignature:=GetFPCTarget(true);
  end
  else
  begin
    infoln('FPC: running make distclean equivalent (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+'):',etinfo);
    CPU_OSSignature:=FCrossCPU_Target+'-'+FCrossOS_Target;
  end;
  DeleteFileUTF8(IncludeTrailingPathDelimiter(FBaseDirectory)+'build-stamp.'+CPU_OSSignature);
  DeleteFileUTF8(IncludeTrailingPathDelimiter(FBaseDirectory)+'base.build-stamp.'+CPU_OSSignature);

  // Clean ppus etc in any directory that has our cpu-os signature.
  // This may miss some, but straight compiles will not interfere with cross compile
  // units and the other way round
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('ppu');
    DeleteList.Add('a');
    DeleteList.Add('o');
    DeleteFilesExtensionsSubdirs(ExcludeTrailingPathDelimiter(FBaseDirectory),DeleteList,CPU_OSSignature);

    //Delete all fpcmade.i38-win32 etc marker files:
    DeleteList.Clear;
    DeleteList.Add('fpcmade.'+CPU_OSSignature);
    DeleteFilesSubDirs(ExcludeTrailingPathDelimiter(FBaseDirectory),DeleteList,'');
  finally
    DeleteList.Free;
  end;

  // Delete any existing fpc.cfg files; ignore success or failure
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.cfg');
  {$IFDEF WIN64}
  // Delete  bootstrap compiler; will be regenerated later with new
  // version:
  infoln('TFPCInstaller: deleting bootstrap x64 compiler (will be rebuilt using x86 compiler)',etinfo);
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'ppcx64.exe');
  {$ENDIF WIN64}
  {$IFDEF UNIX}
  // Delete any fpc.sh shell scripts
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.sh');
  {$ENDIF UNIX}

  // Even if we cleaned imperfectly, allow continuing to the next step
  result:=true;
end;

function TFPCInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=true;
end;

function TFPCInstaller.GetModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  UpdateWarnings: TStringList;
begin
  result:=InitModule;
  if not result then exit;
  infoln('Checking out/updating FPC sources...',etinfo);
  UpdateWarnings:=TStringList.Create;
  try
   FSVNClient.Verbose:=FVerbose;
   result:=DownloadFromSVN(ModuleName,BeforeRevision, AfterRevision, UpdateWarnings);
   if UpdateWarnings.Count>0 then
   begin
     WritelnLog(UpdateWarnings.Text);
   end;
  finally
    UpdateWarnings.Free;
  end;
  infoln('FPC was at: '+BeforeRevision,etinfo);
  if FSVNUpdated then infoln('FPC is now at: '+AfterRevision,etinfo) else infoln('No updates for FPC found.',etinfo);
end;

function TFPCInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  //sanity check
  if FileExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'Makefile') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'compiler') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'rtl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FBaseDirectory)) then
    begin
    if DeleteDirectoryEx(FBaseDirectory)=false then
    begin
      WritelnLog('Error deleting FPC directory '+FBaseDirectory);
      result:=false;
    end
    else
    result:=true;
    end
  else
  begin
    WritelnLog('Invalid FPC directory :'+FBaseDirectory);
    result:=false;
  end;
end;

constructor TFPCInstaller.Create;
begin
  inherited create;

FCompiler := '';
FSVNDirectory := '';
FMakeDir :='';

InitDone:=false;
end;

destructor TFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

