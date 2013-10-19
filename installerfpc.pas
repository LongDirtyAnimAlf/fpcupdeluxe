unit installerFpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils;

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
    FBinPath: string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
    FBootstrapCompilerOverrideVersionCheck: boolean; //Indicate to make we really want to compile with this version (e.g. trunk compiler), even if it is not the latest stable version
    InitDone: boolean;
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // Retrieves compiler version string
    function GetCompilerVersion(CompilerPath: string): string;
    // Creates fpc proxy script that masks general fpc.cfg
    function CreateFPCScript:boolean;
    // Downloads bootstrap compiler for relevant platform, reports result.
    function DownloadBootstrapCompiler: boolean;
    // Another way to get the compiler version string
    //todo: choose either GetCompilerVersion or GetFPCVersion
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
    // If yes, an override option will be passed to make (OVERRIDEVERSIONCHECK=1)
    // If no, latest stable FPC bootstrap compiler needs to be used.
    // This is required information for setting make file optioins
    property CompilerOverrideVersionCheck: boolean read FBootstrapCompilerOverrideVersionCheck;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TFPCNativeInstaller }

  TFPCNativeInstaller = class(TFPCInstaller)
  protected
    // Build module descendant customisation. Runs make all/install for native FPC
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

uses fpcuputil,fileutil
  {$IFDEF UNIX}
    ,baseunix
  {$ENDIF UNIX}
  ;
{ TFPCCrossInstaller }
const
  Win64FallBackUsingCrossCompiler=false; //Set to true to download i386 boostrap compiler and cross compile. Leave to use native win x64 compiler

function InsertFPCCFGSnippet(FPCCFG,FirstLine,Snippet: string): boolean;
// Adds snippet to fpc.cfg file if firstline is not already there
// Returns success (snippet inserted or already exists) or failure
var
  ConfigText: TStringList;
begin
  result:=false;
  ConfigText:=TStringList.Create;
  try
    ConfigText.LoadFromFile(FPCCFG);
    if pos(FirstLine+LineEnding,ConfigText.Text)>0 then
    begin
      infoln('fpc.cfg: not inserting snippet as '+FirstLine+' already exists in '+FPCCFG,etWarning);
    end
    else
    begin
      ConfigText.Add(LineEnding);
      ConfigText.Add(Snippet);
    end;
    ConfigText.SaveToFile(FPCCFG);
    result:=true;
  finally
    ConfigText.Free;
  end;
end;

function TFPCCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
// Runs make/make install for crosss compiler.
// Error out on problems; unless module considered optional, i.e. in
// crosswin32-64 and crosswin64-32 steps.
var
  FPCCfg:String; //path+filename of the fpc.cfg configuration file
  CrossInstaller:TCrossInstaller;
  MagicLine:String; //use this to find fpcup-modified sections in fpc.cfg
  Options:String;
begin
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
  result:=false; //fail by default
  CrossInstaller:=GetCrossInstaller;
  if assigned(CrossInstaller) then
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils', etError)
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries', etError)
    else
      begin
      // Make all
      ProcessEx.Executable := Make;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      infoln('Running Make all (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')',etinfo);
      ProcessEx.Parameters.Add('FPC='+FCompiler);
      ProcessEx.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      // Tell make where to find the target binutils if cross-compiling:
      if CrossInstaller.BinUtilsPath<>'' then
        ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
      ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      // Don't really know if this is necessary, but it can't hurt:
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
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
      try
        ProcessEx.Execute;
        result:=(ProcessEx.ExitStatus=0);
      except
        on E: Exception do
          begin
          WritelnLog('FPC: Running cross compiler fpc make all failed with an exception!'+LineEnding+
            'Details: '+E.Message,true);
          exit(false);
          end;
      end;

      if not(Result) then
        begin
        // Not an error but warning for optional modules: crosswin32-64 and crosswin64-32
        // These modules need to be optional because FPC 2.6.2 gives an error crosscompiling regarding fpdoc.css or something.
        {$ifdef win32}
        // if this is crosswin32-64, ignore error as it is optional
        if (CrossInstaller.TargetCPU='x86_64') and ((CrossInstaller.TargetOS='win64') or (CrossInstaller.TargetOS='win32')) then
          result:=true;
        {$endif win32}
        {$ifdef win64}
        // if this is crosswin64-32, ignore error as it is optional
        if (CrossInstaller.TargetCPU='i386') and (CrossInstaller.TargetOS='win32') then
          result:=true;
        {$endif win64}
        FCompiler:='////\\\Error trying to compile FPC\|!';
        if result then
          infoln('FPC: Running cross compiler fpc make all for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed with an error code. Optional module; continuing regardless.', etInfo)
        else
          infoln('FPC: Running cross compiler fpc make all for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed with an error code.',etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(result);
        end
      else
        begin
        // Install crosscompiler: make crossinstall
        ProcessEx.Executable := Make;
        ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
        ProcessEx.Parameters.Clear;
        infoln('Running Make crossinstall (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')', etinfo);
        ProcessEx.Parameters.Add('FPC='+FCompiler);
        ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
        {$IFDEF UNIX}
        ProcessEx.Parameters.Add('INSTALL_BINDIR='+FBinPath);
        {$ENDIF UNIX}
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath<>'' then
          ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
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

        try
          ProcessEx.Execute;
        except
          on E: Exception do
            begin
            WritelnLog('FPC: Running cross compiler fpc make crossinstall failed with an exception!'+LineEnding+
              'Details: '+E.Message,true);
            result:=false;
            end;
        end;

        if ProcessEx.ExitStatus<>0 then
          begin
          // If anything else than crosswin32-64 or crosswin64-32, fail:
          result:=false;
          {$ifdef win32}
          // if this is crosswin32-64, ignore error as it is optional
          if (CrossInstaller.TargetCPU='x86_64') and ((CrossInstaller.TargetOS='win64') or (CrossInstaller.TargetOS='win32')) then
            result:=true;
          {$endif win32}
          {$ifdef win64}
          // if this is crosswin64-32, ignore error as it is optional
          if (CrossInstaller.TargetCPU='i386') and (CrossInstaller.TargetOS='win32') then
            result:=true;
          {$endif win64}
          if result then
            infoln('FPC: Problem installing crosscompiler for '+FCrossCPU_Target+'-'+FCrossOS_Target+'. Optional module; continuing regardless.', etInfo)
          else
            infoln('FPC: Problem installing crosscompiler for '+FCrossCPU_Target+'-'+FCrossOS_Target+'.', etError);
          FCompiler:='////\\\Error trying to compile FPC\|!';
          end
        else
          begin
          if CrossInstaller.FPCCFGSnippet<>'' then
            begin
            // Modify fpc.cfg
            FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.cfg';
            //todo: is this enough - shouldn't we also check for target OS not only target CPU? What about arm linux and arm wince?
            MagicLine:='# fpcup do not remove '+FCrossCPU_target+'-'+FCrossOS_target;
            InsertFPCCFGSnippet(FPCCfg,
              MagicLine,
              MagicLine+LineEnding+
              '#cross compile settings dependent on both target OS and target CPU'+LineEnding+
              '#IFDEF CPU'+uppercase(FCrossCPU_Target+LineEnding)+
              '#IFDEF '+uppercase(FCrossOS_Target)+LineEnding+
              '# Inserted by fpcup '+TimeToStr(Now)+LineEnding+
              CrossInstaller.FPCCFGSnippet+LineEnding+
              '#ENDIF'+LineEnding+
              '#ENDIF');
            end;
        {$IFDEF UNIX}
          result:=CreateFPCScript;
        {$ENDIF UNIX}
          GetCompiler;
          end;
        end;
    end
  else
    begin
    infoln('FPC: Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target,etwarning);
    result:=false;
    end;
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
  OperationSucceeded:=true;
  // Make all/install, using bootstrap compiler.
  // Make all should use generated compiler internally for unit compilation
  {$IFDEF UNIX}
  // the long way: make all, see where to install, install
  FErrorLog.Clear;
  ProcessEx.Executable := Make;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  // Override makefile checks that checks for stable compiler in FPC trunk
  if FBootstrapCompilerOverrideVersionCheck then
    ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
  if FCompilerOptions<>'' then
    ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
  ProcessEx.Parameters.Add('all');
  infoln('Running make all for FPC:',etinfo);
  try
    // At least on 2.7.1 we get access violations running fpc make
    // perhaps this try..except isolates that
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      begin
      OperationSucceeded := False;
      WritelnLog('FPC: Running fpc make all failed with exit code '+inttostr(ProcessEx.ExitStatus)+LineEnding+
        'Details: '+FErrorLog.Text,true);
      end;
  except
    on E: Exception do
      begin
      OperationSucceeded := False;
      WritelnLog('FPC: Running fpc make all failed with an exception!'+LineEnding+
        'Details: '+E.Message,true);
      end;
  end;

  if OperationSucceeded then
  begin
    ProcessEx.Parameters.Clear;
    FErrorLog.Clear;
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
    ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
    ProcessEx.Parameters.Add('INSTALL_BINDIR='+FBinPath);
    ProcessEx.Parameters.Add('install');
    infoln('Running make install for FPC:',etinfo);
    try
      // At least on 2.7.1 Windows we get access violations running fpc make
      // perhaps this try..except isolates that
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
        begin
        OperationSucceeded := False;
        WritelnLog('FPC: Running fpc make install failed with exit code '+inttostr(ProcessEx.ExitStatus)+LineEnding+
          'Details: '+FErrorLog.Text,true);
        end;
    except
      on E: Exception do
        begin
        OperationSucceeded := False;
        WritelnLog('FPC: Running fpc make install failed with an exception!'+LineEnding+
          'Details: '+E.Message,true);
        end;
    end;
  end;

  {$ELSE UNIX} // Windows

  ProcessEx.Executable := Make;
  FErrorLog.Clear;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  // Override makefile checks that checks for stable compiler in FPC trunk
  if FBootstrapCompilerOverrideVersionCheck then
    ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
  if FCompilerOptions <>'' then
    ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
  ProcessEx.Parameters.Add('all');
  ProcessEx.Parameters.Add('install');
  infoln('Running make all install for FPC:',etinfo);
  try
    // At least on 2.7.1 we get access violations running fpc make
    // perhaps this try..except isolates that
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      begin
      OperationSucceeded := False;
      WritelnLog('FPC: Running fpc make all install failed with exit code '+inttostr(ProcessEx.ExitStatus)+LineEnding+
        'Details: '+FErrorLog.Text,true);
      end;
  except
    on E: Exception do
      begin
      OperationSucceeded := False;
      WritelnLog('FPC: Running fpc make all install failed with an exception!'+LineEnding+
        'Details: '+E.Message,true);
      end;
  end;
  {$ENDIF UNIX}


  {$IFDEF UNIX}
  if OperationSucceeded then
    begin
    if FVerbose then
      infoln('Creating fpc script:',etInfo)
    else
      infoln('Creating fpc script:',etDebug);
    OperationSucceeded:=CreateFPCScript;
    end;
  {$ENDIF UNIX}

  // Let everyone know of our shiny new compiler:
  if OperationSucceeded then
    begin
    GetCompiler;
    // Verify it exists
    if not(FileExistsUTF8(FCompiler)) then
      begin
      WritelnLog('FPC: error: could not find compiler '+FCompiler+' that should have been created.',true);
      OperationSucceeded:=false;
      end;
    end
  else
    FCompiler:='////\\\Error trying to compile FPC\|!';


  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
    begin
    //Copy over binutils to new CompilerName bin directory
    try
      for FileCounter:=0 to FBinUtils.Count-1 do
        begin
        FileUtil.CopyFile(IncludeTrailingPathDelimiter(FMakeDir)+FBinUtils[FileCounter], IncludeTrailingPathDelimiter(FBinPath)+FBinUtils[FileCounter]);
        end;
      // Also, we can change the make/binutils path to our new environment
      // Will modify fmake as well.
      FMakeDir:=FBinPath;
    except
      on E: Exception do
        begin
        writelnlog('FPC: Error copying binutils: '+E.Message,true);
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
  {$IFDEF UNIX}
var
  FPCScript:string;
  TxtFile:Text;
  {$ENDIF UNIX}
begin
  {$IFDEF UNIX}
  // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
  // If this fails, Lazarus compilation will fail...
  FPCScript := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.sh';
  if FileExists(FPCScript) then
  begin
    infoln('fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.',etinfo);
    if not(sysutils.DeleteFile(FPCScript)) then
    begin
      infoln('Error deleting existing launcher script for FPC:'+FPCScript,eterror);
      Exit(false);
    end;
  end;
  AssignFile(TxtFile,FPCScript);
  Rewrite(TxtFile);
  writeln(TxtFile,'#!/bin/sh');
  writeln(TxtFile,'# This script starts the fpc compiler installed by fpcup');
  writeln(TxtFile,'# and ignores any system-wide fpc.cfg files');
  writeln(TxtFile,'# Note: maintained by fpcup; do not edit directly, your edits will be lost.');
  writeln(TxtFile,IncludeTrailingPathDelimiter(FBinPath),'fpc  -n @',
       IncludeTrailingPathDelimiter(FBinPath),'fpc.cfg -FD'+
       IncludeTrailingPathDelimiter(FBinPath)+' "$@"');
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
CompilerName:string; // File name of compiler in bootstrap archive
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
  {$IFDEF BSD} //*BSD, OSX
  {$IF defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  //todo: test parameters
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
  {$ENDIF defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  {$IFDEF DARWIN}
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
  {$ENDIF BSD}
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
const
  // Common path used to get bootstrap compilers.
  //todo: replace when enough compilers are available via 2.6.2
  FTPPath='ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/';
  FTP262Path='ftp.freepascal.org/pub/fpc/dist/2.6.2/bootstrap/';
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
    // We assume we're using the regular latest stable compiler, so don't
    // suggest to make to override version checks.
    // Useful if we forget to update compiler versions when a new stable is released.
    FBootstrapCompilerOverrideVersionCheck:=false;
    {$IFDEF MSWINDOWS}
    {$ifdef win64}
    if Win64FallBackUsingCrossCompiler then
      begin
      //There is no win64 bootstrap compiler, yet
      //Each time we build, we'll make our own starting with the ppc386.exe bootstrap compiler
      //This should eliminate issues with the wrong RTL etc.
      if FBootstrapCompilerURL='' then
        FBootstrapCompilerURL := FTP262Path+'i386-win32-ppc386.zip';
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
      FBootstrapCompilerOverrideVersionCheck:=true;
      end
      else
      begin
      // Use regular x64 compiler
      if FBootstrapCompilerURL='' then
        FBootstrapCompilerURL := FTP262Path+'x86_64-win64-ppcx64.zip';
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64.exe';
      end;
    {$ELSE}
    // Win32
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'i386-win32-ppc386.zip';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
    {$endif win64}
    {$ENDIF MSWINDOWS}
    {$IFDEF Linux}
    //If compiled for x86 32 bit, install 32 bit
    //If compiled for x64, install x64 only.
    {$IFDEF CPU386}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'i386-linux-ppc386.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-linux-ppc386-1';
    {$ELSE}
    {$IFDEF cpuarmel}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'arm-linux-ppcarm.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'arm-linux-ppcarm';
    {$ELSE} // Assume x64 (could also be PowerPC, SPARC I suppose)
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'x86_64-linux-ppcx64.bz2';
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
      FBootstrapCompilerURL := FTPPath+'universal-darwin-ppcuniversal.tar.bz2';
    // If we're using an old compiler to build trunk, we need this:
    FBootstrapCompilerOverrideVersionCheck:=true;
    {$ENDIF Darwin}
    {$IFDEF FREEBSD}
    {$IFDEF CPU386}
    // Assuming user has FreeBSD 9...
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'i386-freebsd9-ppc386.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-freebsd9-ppc386';
    {$ENDIF CPU386}
    {$IFDEF CPUX86_64}
    // Assuming user has FreeBSD 9...
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'x86_64-freebsd9.ppcx64.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-freebsd9.ppcx64';
    {$ENDIF CPUX86_64}
    {$ENDIF FREEBSD}
    {$IFDEF NETBSD}
    {$IFDEF CPU386}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'i386-netbsd-ppc386.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-netbsd-ppc386';
    {$ENDIF CPU386}
    {$IFDEF CPUX86_64}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'x86_64-netbsd-ppcx64.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-netbsd-ppcx64';
    {$ENDIF CPUX86_64}
    {$ENDIF NETBSD}
    {$IFDEF OPENBSD}
    {$IFDEF CPU386}
    // No bootstrap compiler available
    raise Exception.Create('No bootstrap compiler available for this operating system.');
    {$ENDIF CPU386}
    {$IFDEF CPUX86_64}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP262Path+'x86_64-openbsd-ppcx64.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-openbsd-ppcx64';
    {$ENDIF CPUX86_64}
    {$ENDIF OPENBSD}
    end;

  // Only download bootstrap compiler if we can't find a valid one
  if CheckExecutable(FBootstrapCompiler, '-h', 'Free Pascal Compiler') then
    begin
      infoln('Found bootstrap compiler version '+GetCompilerVersion(FBootstrapCompiler),etinfo);
      result:=CheckAndGetNeededExecutables;
    end
    else
    begin
      {$ifdef darwin}
      // Force use of universal bootstrap compiler regardless of what user said as fpc ftp
      //doesn't have a ppc386 bootstrap. Will have to build one later in TFPCInstaller.BuildModule
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
      // Ensure make doesn't care if we build an i386 compiler with an old stable compiler:
      FBootstrapCompilerOverrideVersionCheck:=true;
      {$endif darwin}
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
  FBinPath:=IncludeTrailingPathDelimiter(FBaseDirectory)+'bin'+DirectorySeparator+GetFPCTarget(true);
  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  // add fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(FBootstrapCompilerDirectory+PathSeparator+
    FMakeDir+PathSeparator+
    FSVNDirectory+PathSeparator+
    IncludeTrailingPathDelimiter(FBaseDirectory)+'utils'+PathSeparator+
    FBinPath,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  //add fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(FBinPath+PathSeparator+IncludeTrailingPathDelimiter(FBaseDirectory)+'utils',true);
  {$ENDIF UNIX}
  InitDone:=result;
end;

function TFPCInstaller.BuildModule(ModuleName: string): boolean;
var
  FPCCfg: string;
  FPCMkCfg: string; //path+file of fpcmkcfg
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
  // Deals dynamically with either ppc386.exe or native ppcx64.exe
  if pos('ppc386.exe',FCompiler)>0 then //need to build ppcx64 before
    begin
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler';
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler');
    ProcessEx.Parameters.Add('OS_TARGET=win64');
    ProcessEx.Parameters.Add('CPU_TARGET=x86_64');
    // Override makefile checks that checks for stable compiler in FPC trunk
    if FBootstrapCompilerOverrideVersionCheck then
      ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
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
    // Override makefile checks that checks for stable compiler in FPC trunk
    if FBootstrapCompilerOverrideVersionCheck then
      ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
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
  if not (OperationSucceeded) then
    infoln('Error running BuildModuleCustom for module '+ModuleName,etError);

  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
  // copy the freshly created compiler to the bin/$fpctarget directory so that
  // fpc can find it
  if FindFirst(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/ppc*',faAnyFile,SearchRec)=0 then
    repeat
      s:=SearchRec.Name;
      if (length(s)>4) and (pos(s,COMPILERNAMES)>0) then  //length(s)>4 skips ppc3
        begin
        OperationSucceeded:=OperationSucceeded and
          FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s,
           IncludeTrailingPathDelimiter(FBinPath)+s);
        OperationSucceeded:=OperationSucceeded and
          (0=fpChmod(IncludeTrailingPathDelimiter(FBinPath)+s,&755));
        end;
    until FindNext(SearchRec)<>0;
  FindClose(SearchRec);
  if not (OperationSucceeded) then
    infoln('Error copying over compiler to '+IncludeTrailingPathDelimiter(FBinPath),etError);

  // create link 'units' below FBaseDirectory to <somewhere>/lib/fpc/$fpcversion/units
  DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'units');
  fpSymlink(pchar(IncludeTrailingPathDelimiter(FBaseDirectory)+'lib/fpc/'+GetFPCVersion+'/units'),
  pchar(IncludeTrailingPathDelimiter(FBaseDirectory)+'units'));
  end;
  {$ENDIF UNIX}

  // Find out where fpcmkcfg lives - only if necessary.
  if OperationSucceeded and
    (FileExists(FPCCfg)=false) then
  begin
    fpcmkcfg:=IncludeTrailingPathDelimiter(FBinPath) + 'fpcmkcfg';
    if not(CheckExecutable(fpcmkcfg,'-h','fpcmkcfg')) then
    begin
      // Some 2.7 trunk dump fpcmkcfg in bin itself
      fpcmkcfg:=IncludeTrailingPathDelimiter(FBaseDirectory)+
        'bin'+DirectorySeparator+'fpcmkcfg';
      if not(CheckExecutable(fpcmkcfg,'-h','fpcmkcfg')) then
      begin
        fpcmkcfg:='';
        infoln('Could not find fpcmkcfg. Aborting.',etError);
        OperationSucceeded:=false;
      end
      else
      begin
        infoln('Found valid fpcmkcfg executable: '+fpcmkcfg,etInfo);
      end;
    end;
  end;

  //todo: after fpcmkcfg create a config file for fpkpkg or something
  if OperationSucceeded then
  begin
    // Create fpc.cfg if needed
    FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.cfg';
    if FileExists(FPCCfg) = False then
    begin
      ProcessEx.Executable := fpcmkcfg;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('-d');
      ProcessEx.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('-o');
      ProcessEx.Parameters.Add('' + FPCCfg + '');
      infoln('Creating fpc.cfg:',etinfo);
      try
        ProcessEx.Execute;
      except
        on E: Exception do
          begin
          WritelnLog('FPC: Running fpcmkcfg failed with an exception!'+LineEnding+
            'Details: '+E.Message,true);
          OperationSucceeded := False;
          end;
      end;

      if ProcessEx.ExitStatus <> 0 then
        begin
        OperationSucceeded := False;
        WritelnLog('FPC: Running fpcmkcfg failed with exit code '+inttostr(ProcessEx.ExitStatus),true);
        end;
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
    WritelnLog('FPC: update succeeded.',false);
  Result := OperationSucceeded;
end;

function TFPCInstaller.CleanModule(ModuleName: string): boolean;
// Make distclean is unreliable; at least for FPC.
// Running it twice apparently can fix a lot of problems; see FPC ML message
// by Jonas Maebe, 1 November 2012
// On Windows, removing fpmake.exe, see Build FAQ (Nov 2011), 2.5
var
  oldlog: TErrorMethod;
  CrossCompiling: boolean;
  DeleteList: TStringList;
  CPU_OSSignature:string;
begin
  result:=InitModule;
  if not result then exit;
  oldlog:=ProcessEx.OnErrorM; //current error handler, if any
  CrossCompiling:=(FCrossOS_Target<>'') and (FCrossCPU_Target<>'');
  if CrossCompiling then
    CPU_OSSignature:=FCrossCPU_Target+'-'+FCrossOS_Target
  else
    CPU_OSSignature:=GetFPCTarget(true);

  {$IFDEF MSWINDOWS}
  // Remove all fpmakes
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('fpmake.exe');
    DeleteFilesSubDirs(IncludeTrailingPathDelimiter(FBaseDirectory),DeleteList,CPU_OSSignature);
  finally
    DeleteList.Free;
  end;

  // At least on Windows, compiling dbtestframework yourself may lead to problems compiling fpc later on,
  // so clean compiled files from both packages and test
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('.a');
    DeleteList.Add('.o');
    DeleteList.Add('.ppu');
    //todo: check if all these dirs are required - probably the units one is not needed
    // For some reason base has no cpu subdir - what is this used for!?!?
    // is this only done by the test framework!?!?
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'packages'+DirectorySeparator+
      'fcl-db'+DirectorySeparator+
      'src'+DirectorySeparator+
      'base',DeleteList,'');
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'packages'+DirectorySeparator+
      'fcl-db'+DirectorySeparator+
      'units',DeleteList,CPU_OSSignature);
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'packages'+DirectorySeparator+
      'fcl-db'+DirectorySeparator+
      'tests',DeleteList,'');
    //crazy experiment: also delete the db*.ppu from the units directory in case that's looked for, too
    //C:\Development\fpctrunk\units
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'units'+DirectorySeparator+
      CPU_OSSignature+DirectorySeparator+
      'fcl-db',DeleteList,'');
  finally
    DeleteList.Free;
  end;
  {$ENDIF}

  ProcessEx.OnErrorM:=nil;  //don't want to log errors in distclean
  try
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
    if (FCrossOS_Target='') and (FCrossCPU_Target='') then
      begin
      infoln('FPC: running make distclean:',etinfo);
      end
    else
      begin
      infoln('FPC: running make distclean (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+'):',etinfo);
      end;
    try
      ProcessEx.Execute;
      Sleep(100); //now do it again
      ProcessEx.Execute;
    except
      on E: Exception do
      begin
        result:=false;
        WritelnLog('FPC: running make distclean failed with an exception!'+LineEnding+
          'Details: '+E.Message,true);
      end;
    end;
  finally
    ProcessEx.OnErrorM:=oldlog; //restore previous logging
  end;

  // Delete any existing fpc.cfg files
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.cfg');
  {$IFDEF WIN64}
  if Win64FallBackUsingCrossCompiler then
    begin
    // Only if we're using an i386 stable "bootstrap bootstrap" to create
    // the x64 bootstrap:
    // Delete bootstrap compiler; will be regenerated later with new version:
    infoln('TFPCInstaller: deleting bootstrap x64 compiler (will be rebuilt using x86 compiler)',etinfo);
    Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'ppcx64.exe');
    end;
  {$ENDIF WIN64}
  {$IFDEF UNIX}
  // Delete any fpc.sh shell scripts
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.sh');
  {$ENDIF UNIX}

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
  if FRepositoryUpdated then infoln('FPC is now at: '+AfterRevision,etinfo) else infoln('No updates for FPC found.',etinfo);
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

