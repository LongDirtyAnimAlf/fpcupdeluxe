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
    'Getmodule FPC;'+
    'Buildmodule FPC;'+
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
    function CreateFPCScript:boolean;
    function DownloadBootstrapCompiler: boolean;
    function GetFPCVersion: string;
    // internal initialisation, called from BuildModule,CLeanModule,GetModule
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
  // Make crosscompiler using new compiler
  CrossInstaller:=GetCrossInstaller;
  if assigned(CrossInstaller) then
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils')
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries')
    else
      begin
      ProcessEx.Executable := Make;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      infoln('Running Make all (FPC crosscompiler):');
      { Note: command line equivalents for Win32=>Win64 cross compiler:
      set path=c:\development\fpc\bin\i386-win32;c:\development\fpcbootstrap
      make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo all OS_TARGET=win64 CPU_TARGET=x86_64
      rem already gives compiler\ppcrossx64.exe, compiler\ppcx64.exe
      make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo crossinstall OS_TARGET=win64 CPU_TARGET=x86_64
      rem gives bin\i386-win32\ppcrossx64.exe
      }
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
      infoln('Running Make crossinstall for FPC:');
      ProcessEx.Execute;

      if ProcessEx.ExitStatus = 0 then
        begin
          // Install crosscompiler using new CompilerName - todo: only for Windows!?!?
          // make all and make crossinstall perhaps equivalent to
          // make all install CROSSCOMPILE=1??? todo: find out
          ProcessEx.Executable := Make;
          ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
          infoln('Running Make crossinstall for FPC:');
          ProcessEx.Parameters.Clear;
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
            infoln('Problem compiling/installing crosscompiler. Continuing regardless.');
            FCompiler:='////\\\Error trying to compile FPC\|!';
            {$ifndef win32}
            //fail if this is not WINCROSSX64
            result:=false;
            {$endif win32}
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
    infoln('Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target);
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
  {$IF DEFINED(DARWIN) AND NOT DEFINED(CPUPOWERPC)}
  // On Intel x86/x64 OSX, force FPC to compile as i386. This is in line with
  // the way the FPC installer works. Later we can crosscompile an x64 version.
  ProcessEx.Parameters.Add('CPU_TARGET=i386');
  {$ENDIF DEFINED(DARWIN) AND NOT DEFINED(CPUPOWERPC)}
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  if FCompilerOptions<>'' then
    ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
  ProcessEx.Parameters.Add('all');
  infoln('Running make all for FPC:');
  ProcessEx.Execute;
  if ProcessEx.ExitStatus <> 0 then
    OperationSucceeded := False;

  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  {$IF DEFINED(DARWIN) AND NOT DEFINED(CPUPOWERPC)}
  // On Intel x86/x64 OSX, force FPC to compile as i386. This is in line with
  // the way the FPC installer works. Later we can crosscompile an x64 version.
  ProcessEx.Parameters.Add('CPU_TARGET=i386');
  {$ENDIF DEFINED(DARWIN) AND NOT DEFINED(CPUPOWERPC)}
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('INSTALL_BINDIR='+BinPath);
  ProcessEx.Parameters.Add('install');
  infoln('Running make install for FPC:');
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
  infoln('Running make all install for FPC:');
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
        FileUtil.CopyFile(FMakeDir+FBinUtils[FileCounter], IncludeTrailingBackslash(BinPath)+FBinUtils[FileCounter]);
      end;
      // Also, we can change the make/binutils path to our new environment
      // Will modify fmake as well.
      FMakeDir:=BinPath;
    except
      on E: Exception do
      begin
        infoln('Error copying binutils: '+E.Message);
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
    infoln('fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.');
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
       IncludeTrailingPathDelimiter(BinPath)+' $*');
  CloseFile(TxtFile);
  Result:=(FPChmod(FPCScript,&700)=0); //Make executable; fails if file doesn't exist=>Operationsucceeded update
  if Result then
  begin
    infoln('Created launcher script for FPC:'+FPCScript);
  end
  else
  begin
    infoln('Error creating launcher script for FPC:'+FPCScript);
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
  OperationSucceeded:=ForceDirectories(FBootstrapCompilerDirectory);
  if OperationSucceeded=false then infoln('DownloadBootstrapCompiler error: could not create directory '+FBootstrapCompilerDirectory);
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
  if ExecuteCommandHidden(FUnzip,'-o -d '+ArchiveDir+' '+BootstrapArchive,FVerbose) <> 0 then
    begin
      infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
      OperationSucceeded := False;
    end
    else
    begin
      OperationSucceeded := True; // Spelling it out can't hurt sometimes
    end;
  // Move CompilerName to proper directory
  if OperationSucceeded = True then
  begin
    infoln('Going to rename/move ' + ArchiveDir + CompilerName + ' to ' + FBootstrapCompiler);
    renamefile(ArchiveDir + CompilerName, FBootstrapCompiler);
  end;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  //Extract bz2, overwriting without prompting
  if ExecuteCommandHidden(FBunzip2,'-d -f -q '+BootstrapArchive,FVerbose) <> 0 then
    begin
      infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
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
    infoln('Going to move ' + ExtractedCompiler + ' to ' + FBootstrapCompiler);
    OperationSucceeded:=MoveFile(ExtractedCompiler,FBootstrapCompiler);
  end;
  if OperationSucceeded then
  begin
    //Make executable
    OperationSucceeded:=(fpChmod(FBootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootstrapCompiler);
  end;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  //Extract .tar.bz2, overwriting without prompting
  if ExecuteCommandHidden(FTar,'-x -v -j -f '+BootstrapArchive,Verbose) <> 0 then
  begin
    infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
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
    infoln('Going to rename/move '+CompilerName+' to '+BootstrapCompiler);
    sysutils.DeleteFile(BootstrapCompiler); //ignore errors
    // We might be moving files across partitions so we cannot use renamefile
    OperationSucceeded:=FileUtil.CopyFile(CompilerName, BootstrapCompiler);
    sysutils.DeleteFile(CompilerName);
  end;
  if OperationSucceeded then
  begin
    //Make executable
    OperationSucceeded:=(fpChmod(BootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+BootstrapCompiler);
  end;
  {$ENDIF DARWIN}
end;
if OperationSucceeded = True then
begin
  SysUtils.DeleteFile(BootstrapArchive);
end
else
begin
  infoln('Error getting/extracting bootstrap compiler. Archive: '+BootstrapArchive);
end;
Result := OperationSucceeded;
end;

function TFPCInstaller.GetFPCVersion: string;
begin
  ExecuteCommandHidden(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler'+DirectorySeparator+'ppc1','-iV',result,FVerbose);
  //Remove trailing LF(s) and other control codes:
  while (length(result)>0) and (ord(result[length(result)])<$20) do
    delete(result,length(result),1);
end;

function TFPCInstaller.InitModule:boolean;
begin
  result:=true;
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  infoln('Module FPC: Getting/compiling FPC...');
  if FBootstrapCompiler='' then
    begin  // need to download it
    {$IFDEF MSWINDOWS}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL:=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-win32-ppc386.zip';
    FBootstrapCompiler := FBootstrapCompilerDirectory +'ppc386.exe';
    {$ENDIF MSWINDOWS}
    {$IFDEF Linux}
    //If compiled for x86 32 bit, install 32 bit
    //If compiled for x64, install x64 only.//todo: cross compiler!?!
    {$IFDEF CPU386}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL :=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-linux-ppc386.bz2';
    FBootstrapCompiler := FBootstrapCompilerDirectory +'i386-linux-ppc386-1';
    {$ELSE}
    {$IFDEF cpuarmel}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL :=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/arm-linux-ppcarm.bz2';
    FBootstrapCompiler := FBootstrapCompilerDirectory +'arm-linux-ppcarm';
    FFPCPlatform:='arm-linux';
    {$ELSE} // Assume x64 (could also be PowerPC, SPARC I suppose)
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL :=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/x86_64-linux-ppcx64.bz2';
    FBootstrapCompiler := FBootstrapCompilerDirectory +'x86_64-linux-ppcx64';
    {$ENDIF cpuarmel}
    {$ENDIF CPU386}
    {$ENDIF Linux}
    {$IFDEF Darwin}
    //OSX
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL:=
      'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/universal-darwin-ppcuniversal.tar.bz2';
    FBootstrapCompiler := FBootstrapCompilerDirectory +'ppcuniversal';
    {$ENDIF Darwin}
    end;
  if FCompiler='' then   //!!!Don't use Compiler here. GetCompiler returns installed compiler.
    FCompiler:=FBootstrapCompiler;
  result:=CheckAndGetNeededExecutables and DownloadBootstrapCompiler;

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
  SetPath(FBootstrapCompilerDirectory+PathSeparator+
    FMakeDir+PathSeparator+
    FSVNDirectory+PathSeparator+
    FBaseDirectory,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  SetPath(BinPath,true);
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
const
  COMPILERNAMES='ppc386,ppcm68k,ppcalpha,ppcpowerpc,ppcpowerpc64,ppcarm,ppcsparc,ppcia64,ppcx64'+
    'ppcross386,ppcrossm68k,ppcrossalpha,ppcrosspowerpc,ppcrosspowerpc64,ppcrossarm,ppcrosssparc,ppcrossia64,ppcrossx64';


begin
  if not InitModule then exit;
  result:=false;
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
      infoln('Creating fpc.cfg:');
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
      infoln('fpc.cfg already exists; leaving it alone.');
    end;
  end;

  if OperationSucceeded then
    WritelnLog('FPC: update succeeded at revision number '+ AfterRevision,false);
  Result := OperationSucceeded;
end;

function TFPCInstaller.CleanModule(ModuleName: string): boolean;
var
  oldlog:TErrorMethod;
begin
  if not InitModule then exit;
  ProcessEx.OnErrorM:=nil;  //don't want to log errors in distclean
  ProcessEx.Executable := Make;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  if Self is TFPCCrossInstaller then
    begin  // clean out the correct compiler
    ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
    ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
    end;
  ProcessEx.Parameters.Add('distclean');
  infoln('FPC: running make distclean before checkout/update:');
  ProcessEx.Execute;
  ProcessEx.OnErrorM:=oldlog;
  result:=ProcessEx.ExitStatus=0;
end;

function TFPCInstaller.GetModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  UpdateWarnings: TStringList;
begin
  if not InitModule then exit;
  infoln('Checking out/updating FPC sources...');
  UpdateWarnings:=TStringList.Create;
  try
   result:=DownloadFromSVN(ModuleName,BeforeRevision, AfterRevision, UpdateWarnings);
   if UpdateWarnings.Count>0 then
   begin
     WritelnLog(UpdateWarnings.Text);
   end;
  finally
    UpdateWarnings.Free;
  end;
  infoln('FPC was at revision: '+BeforeRevision);
  if FSVNUpdated then infoln('FPC is now at revision: '+AfterRevision) else infoln('No updates for FPC found.');
end;

function TFPCInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  infoln('Module FPC: cleanup...');
  try
    // SVN revert FPC directory
     FSVNClient.LocalRepository := FBaseDirectory;
     FSVNClient.Repository := FURL;
     FSVNClient.Revert; //Remove local changes

    // Delete any existing fpc.cfg files
    Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.cfg');

    {$IFDEF UNIX}
    // Delete any fpc.sh shell scripts
    Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.sh');
    {$ENDIF UNIX}
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog('FPC clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      result:=false;
    end;
  end;
end;

constructor TFPCInstaller.Create;
var
  LogFileName:string;
begin
  inherited create;

// Binutils needed for compilation
CreateBinutilsList;

FCompiler := '';
FSVNDirectory := '';
FMakeDir :='';

TextRec(FLogVerboseFile).Mode:=0;  //class variables should have been 0
  InitDone:=false;
end;

destructor TFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

