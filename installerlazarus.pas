unit installerLazarus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller,dynlibs;


Const
  Sequences=
//standard lazarus build
    //Note: we only do a getmodule/cleanmodule once here in this sequence,
    //otherwise we keep running distclean and svn
    'Declare lazarus;'+
    'Cleanmodule lazarus;'+
    'Getmodule lazarus;'+
    //config lazarus so we can use lazbuild in the build step:
    'ConfigModule lazarus;'+
    // Cross compile support at least requires lazbuild.
    // we're building it here directly to avoid circular
    // dependencies (don't know if these are a problem)
    'Buildmodule lazbuild;'+
    'Buildmodule lazarus;'+
    //Config again to fix any wrong settings introduced:
    'ConfigModule lazarus;'+
    // Make sure the user can use the IDE:
    'Exec CreateLazarusScript;'+
    'End;'+

//standard bigide build
    'Declare BIGIDE;'+
    //This requires Lazarus sources, but we're not
    //going to Require lazarus as that will run make etc. to compile the regular IDE+LCL
    //Instead, just get lazarus sources
    //'Cleanmodule lazarus;'+
    'Getmodule lazarus;'+
    'Buildmodule BIGIDE;'+
    //config lazarus, in case we only run BIGIDE (without lazarus):
    'ConfigModule lazarus;'+
    // Make sure the user can use the IDE:
    'Exec CreateLazarusScript;'+
    'End;'+

//Nogui widgetset+Lazbuild:
    'Declare lazbuild;'+
    //Same story as in BIGIDE
    'Getmodule lazarus;'+
    'Buildmodule lazbuild;'+
    //config lazarus, so lazbuild will work:
    'ConfigModule lazarus;'+
    'End;'+

//standard IDE build with user-selected packages
    'Declare USERIDE;'+
    //Run lazbuild first for building
    'Requires lazbuild;'+
    'Buildmodule USERIDE;'+
    // Make sure the user can use the IDE:
    'Exec CreateLazarusScript;'+
    'End;'+

//standard uninstall
    'Declare lazarusuninstall;'+
    'Uninstallmodule lazarus;'+
    'Exec DeleteLazarusScript;'+
    'End;'+

//standard clean
    'Declare lazarusclean;'+
    'cleanmodule lazarus;'+
    'End;'+

    'Declare BIGIDEclean;'+
    'cleanmodule BIGIDE;'+
    'End;'+


//selective actions triggered with --only=SequenceName
    'Declare LazarusCleanOnly;'+
    'Cleanmodule lazarus;'+
    'End;'+

    'Declare LazarusGetOnly;'+
    'Getmodule lazarus;'+
    'End;'+

    'Declare LazarusBuildOnly;'+
    'Buildmodule lazarus;'+
    'End;'+

    'Declare LazarusConfigOnly;'+
    'Configmodule lazarus;'+
    'End';

type

  { TLazarusInstaller }

  TLazarusInstaller = class(TInstaller)
  private
    BinPath:string;
    FCrossLCL_Platform: string;
    FPrimaryConfigPath: string;
    InitDone:boolean;
  protected
    FFPCDir:string;
    FInstalledLazarus:string;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
  public
    // LCL widget set to be built
    property CrossLCL_Platform:string write FCrossLCL_Platform;
    // FPC base directory
    property FPCDir:string write FFPCDir;
    // Lazarus primary config path
    property PrimaryConfigPath:string write FPrimaryConfigPath;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Create configuration in PrimaryConfigPath
    function ConfigModule(ModuleName:string): boolean; override;
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

  { TLazarusNativeInstaller }

  TLazarusNativeInstaller = class(TLazarusInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TLazarusCrossInstaller }

  TLazarusCrossInstaller = class(TLazarusInstaller)
  protected
  public
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;



implementation
uses fpcuputil, fileutil, processutils, updatelazconfig
  {$IFDEF UNIX}
    ,baseunix
  {$ENDIF UNIX}
  ;

{ TLazarusCrossInstaller }

function TLazarusCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  BuildMethod:String;
  CrossInstaller:TCrossInstaller;
  Options:String;
begin
  CrossInstaller:=GetCrossInstaller;
  infoln('TLazarusCrossInstaller: building module '+ModuleName+'...',info);
  if Assigned(CrossInstaller) then
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils',error)
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries',error)
    else if not CrossInstaller.GetLibsLCL(FCrossLCL_Platform,FBaseDirectory) then
      infoln('Failed to get LCL cross libraries',error)
    else
    begin
    // If we're "crosscompiling" with the native compiler and binutils - "cross compiling lite" - use lazbuild.
    // Advantages:
    // - dependencies are taken care of
    // - it won't trigger a rebuild of the LCL when the user compiles his first cross project.
    // Otherwise, use make; advantages:
    // - can deal with various bin tools
    // - can deal with compiler options
    // - doesn't need existing lazbuild(+nogui LCL)
    // todo: how to reliably detect which version we're using???
    if FCrossLCL_Platform<>'' then
      begin
      {
      Lazarus mailing list, message by Mattias Gaertner, 10 April 2012
      Changes of make, part III
      Precompiling a second LCL platform:
      Do not use "make lcl LCL_PLATFORM=qt", as this will update
      lclbase.compiled and this tells the IDE to rebuild the
      existing ppu of the first platform.
      Use instead:
      make -C lcl/interfaces/qt
      *note*: -C is the same option as --directory=
      => however, this would require us to split compilation into
      - prerequisites
      - LCL
      }
      BuildMethod:='make';
      ProcessEx.Executable := Make;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('FPC='+FCompiler);
      ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('FPCDIR='+FFPCDir); //Make sure our FPC units can be found by Lazarus
      if CrossInstaller.BinUtilsPath<>'' then
        ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(FFPCDir));
      ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      if FCrossLCL_Platform <>'' then
        ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
      ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
      ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
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
      //Per April 2012, LCL requires lazutils which requires registration
      //http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
      ProcessEx.Parameters.Add('registration');
      ProcessEx.Parameters.Add('lazutils');
      ProcessEx.Parameters.Add('lcl');
      end
    else
      begin
      // Use lazbuild for cross compiling lite:
      BuildMethod:='lazbuild';
      ProcessEx.Executable := IncludeTrailingPathDelimiter(FBaseDirectory)+'lazbuild'+GetExeExt;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--pcp='+FPrimaryConfigPath);
      ProcessEx.Parameters.Add('--cpu='+FCrossCPU_Target);
      ProcessEx.Parameters.Add('--os='+FCrossOS_Target);
      if FCrossLCL_Platform<>'' then
        ProcessEx.Parameters.Add('--widgetset='+FCrossLCL_Platform);
      ProcessEx.Parameters.Add('lcl\interfaces\lcl.lpk');
      end;
    if FCrossLCL_Platform='' then
      infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' using '+BuildMethod,info)
    else
      infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+'/'+FCrossLCL_Platform+' using '+BuildMethod,info);
    ProcessEx.Execute;
    result:= ProcessEx.ExitStatus =0;
    if not result then
      WritelnLog( 'Lazarus: error compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' '+FCrossLCL_Platform);
    end
  else
    infoln('Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target,error);
end;

constructor TLazarusCrossInstaller.Create;
begin
  inherited Create;
end;

destructor TLazarusCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusNativeInstaller }

function TLazarusNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
  infoln('TLazarusNativeInstaller: building module '+ModuleName+'...',info);
  if ModuleName<>'USERIDE' then
  begin
    // Make all (should include lcl & ide)
    // distclean was already run; otherwise specify make clean all
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
    ProcessEx.Parameters.Add('FPCDIR='+FFPCDir); //Make sure our FPC units can be found by Lazarus
    ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    if FCrossLCL_Platform <>'' then
      ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
    if FCompilerOptions<>'' then
      ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
    case UpperCase(ModuleName) of
      'BIGIDE':
      begin
        ProcessEx.Parameters.Add('bigide');
        infoln(ModuleName+': running make bigide:',info);
      end;
      'LAZARUS':
      begin
        ProcessEx.Parameters.Add('all');
        infoln(ModuleName+': running make all:',info);
      end;
      'LAZBUILD':
      begin
        ProcessEx.Parameters.Add('lazbuild');
        infoln(ModuleName+': running make lazbuild:',info);
      end;
      'LCL':
      begin
        // April 2012: lcl now requires lazutils and registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        ProcessEx.Parameters.Add('registration');
        ProcessEx.Parameters.Add('lazutils');
        ProcessEx.Parameters.Add('lcl');
        infoln(ModuleName+': running make registration lazutils lcl:', info);
      end
    else //raise error;
      begin
        ProcessEx.Parameters.Add('--help'); // this should render make harmless
        WritelnLog('BuildModule: Invalid module name '+ModuleName+' specified! Please fix the code.', true);
        FInstalledLazarus:= '//*\\error//\\'; //todo: check if this really is an invalid filename. it should be.
        result:=false;
        exit;
      end;
    end;
    ProcessEx.Execute;
  end
  else
  begin
    // useride; using lazbuild. Note: in recent Lazarus we could also run make lazbuild useride
    // todo: look at implementing this; it will simplify program structure and get rid of some dependencies
    // in sequences
    ProcessEx.Executable := IncludeTrailingPathDelimiter(FBaseDirectory)+'lazbuild'+GetExeExt;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('--pcp='+FPrimaryConfigPath);
    ProcessEx.Parameters.Add('--build-ide=');
    if FCrossLCL_Platform <>'' then
      ProcessEx.Parameters.Add('os='+FCrossLCL_Platform );
    infoln('Lazarus: running lazbuild to get IDE with user-specified packages:',info);
    ProcessEx.Execute;
  end;

  if ProcessEx.ExitStatus <> 0 then
  begin
    result := False;
    FInstalledLazarus:= '//*\\error//\\'; //todo: check if this really is an invalid filename. it should be.
  end
  else
  begin
    FInstalledLazarus:=IncludeTrailingPathDelimiter(FBaseDirectory)+'lazarus'+GetExeExt;
  end;
end;

constructor TLazarusNativeInstaller.Create;
begin
  inherited create;
end;

destructor TLazarusNativeInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusInstaller }

function TLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
end;

function TLazarusInstaller.InitModule: boolean;
begin
  result:=true;
  infoln('TLazarusInstaller: initialising...',Debug);
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  WritelnLog('TLazarusInstaller init:',false);
  WritelnLog('Lazarus directory:      '+FBaseDirectory,false);
  WritelnLog('Lazarus URL:            '+FURL,false);
  WritelnLog('Lazarus options:        '+FCompilerOptions,false);
  result:=CheckAndGetNeededExecutables;
  // Look for make etc in the current compiler directory:
  BinPath:=ExtractFilePath(FCompiler);
  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  SetPath(BinPath+PathSeparator+
    FMakeDir+PathSeparator+
    FSVNDirectory+PathSeparator+
    FBaseDirectory,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  SetPath(BinPath,true);
  {$ENDIF UNIX}
  InitDone:=result;
end;

function TLazarusInstaller.BuildModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=BuildModuleCustom(ModuleName);
end;

function TLazarusInstaller.ConfigModule(ModuleName:string): boolean;
var
  LazarusConfig: TUpdateLazConfig;
begin
  if DirectoryExistsUTF8(FPrimaryConfigPath)=false then
  begin
    if ForceDirectoriesUTF8(FPrimaryConfigPath) then
      infoln('Created Lazarus primary config directory: '+FPrimaryConfigPath,info);
  end;
  // Set up a minimal config so we can use LazBuild
  LazarusConfig:=TUpdateLazConfig.Create(FPrimaryConfigPath);
  try
    try
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/LazarusDirectory/Value', FBaseDirectory);
      {$IFDEF MSWINDOWS}
      // FInstalledCompiler could be something like c:\bla\ppc386.exe, e.g.
      // the platform specific compiler. In order to be able to cross compile
      // we'd rather use fpc
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/CompilerFilename/Value',ExtractFilePath(FCompiler)+'fpc'+GetExeExt);
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',IncludeTrailingPathDelimiter(FMakeDir)+'gdb'+GetExeExt);
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/MakeFilename/Value',IncludeTrailingPathDelimiter(FMakeDir)+'make'+GetExeExt);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      // On Unix, FInstalledCompiler should be set to our fpc.sh proxy if installed
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/CompilerFilename/Value',FCompiler);
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',which('gdb')); //assume in path
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/MakeFilename/Value',which('make')); //assume in path
      {$ENDIF UNIX}
      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/FPCSourceDirectory/Value',FFPCDir);
    except
      on E: Exception do
      begin
        result:=false;
        infoln('Error setting Lazarus config: '+E.ClassName+'/'+E.Message,error);
      end;
    end;
  finally
    LazarusConfig.Free;
  end;
end;

function TLazarusInstaller.CleanModule(ModuleName: string): boolean;
var
  oldlog:TErrorMethod;
begin
  result:=InitModule;
  if not result then exit;
  // Make distclean; we don't care about failure (e.g. directory might be empty etc)
  oldlog:=ProcessEx.OnErrorM;
  ProcessEx.OnErrorM:=nil;  //don't want to log errors in distclean
  ProcessEx.Executable := Make;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler+'');
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  if FCrossLCL_Platform <>'' then
    ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
  if (Self is TLazarusCrossInstaller) then
  begin  // clean out the correct compiler
    ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
    ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
    infoln('Lazarus: running make distclean (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+'):',info);
  end
  else
  begin
    infoln('Lazarus: running make distclean:',info);
  end;
  ProcessEx.Parameters.Add('distclean');
  // Note: apparently, you can't specify certain modules to clean, like lcl, bigide...
  ProcessEx.Execute;
  ProcessEx.OnErrorM:=oldlog;
  result:=true;
end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  UpdateWarnings: TStringList;
begin
  if not InitModule then exit;
  infoln('Checking out/updating Lazarus sources:',info);
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
  infoln('Lazarus was at revision: '+BeforeRevision,info);
  if FSVNUpdated then infoln('Lazarus is now at revision: '+AfterRevision,info) else infoln('No updates for Lazarus found.',info);
end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
const
  LookForConfigFile='environmentoptions.xml';
begin
  if not InitModule then exit;
  infoln('Module Lazarus: Uninstall...',info);
  //sanity check
  if FileExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'Makefile') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'ide') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'lcl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FBaseDirectory)) then
    begin
    if DeleteDirectoryEx(FBaseDirectory)=false then
    begin
      WritelnLog('Error deleting Lazarus directory '+FBaseDirectory);
      result:=false;
    end
    else
    result:=true;
    end
  else
  begin
    WritelnLog('Error: invalid Lazarus directory :'+FBaseDirectory);
    result:=false;
  end;
  // Sanity check so we don't try to delete random directories
  // Assume Lazarus has been configured/run once so enviroronmentoptions.xml exists.
  if result and FileExistsUTF8(IncludeTrailingBackslash(FPrimaryConfigPath)+LookForConfigFile) and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FPrimaryConfigPath)) then
    begin
    if DeleteDirectoryEx(FPrimaryConfigPath)=false then
    begin
      WritelnLog('Error deleting Lazarus PrimaryConfigPath directory '+FPrimaryConfigPath);
      result:=false;
    end
    else
    result:=true;
    end
  else
  begin
    WritelnLog('Error: invalid Lazarus FPrimaryConfigPath: '+FPrimaryConfigPath);
    result:=false;
  end;
end;

constructor TLazarusInstaller.Create;
begin
  inherited create;
  InitDone:=false;
end;

destructor TLazarusInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

