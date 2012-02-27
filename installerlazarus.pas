unit installerLazarus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller;


Const
  Sequences=
//standard lazarus build
    'Declare lazarus;'+
    'Cleanmodule lazarus;'+
    'Getmodule lazarus;'+
    'Buildmodule lazarus;'+
    'ConfigModule lazarus;'+
    'End;'+
//standard bigide build
    'Declare BIGIDE;'+
    'Requires lazarus;'+
    'Cleanmodule BIGIDE;'+
    'Buildmodule BIGIDE;'+
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
    // internal initialisation, called from BuildModule,CLeanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
  public
    // LCL widget set to be build
    property CrossLCL_Platform:string write FCrossLCL_Platform;
    // FPC base directory
    property FPCDir:string write FFPCDir;
    // lazarus pimary config path
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
  CrossInstaller:TCrossInstaller;
  Options:String;
begin
  CrossInstaller:=GetCrossInstaller;
  if Assigned(CrossInstaller) then
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils')
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries')
    else if not CrossInstaller.GetLibsLCL(FCrossLCL_Platform,FBaseDirectory) then
      infoln('Failed to get LCL cross libraries')
    else
    begin
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
    ProcessEx.Parameters.Add('packager/registration');
    ProcessEx.Parameters.Add('lazutils');
    ProcessEx.Parameters.Add('lcl');
    infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' '+FCrossLCL_Platform);
    ProcessEx.Execute;
    result:= ProcessEx.ExitStatus =0;
    if not result then
      WritelnLog( 'Lazarus: error compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' '+FCrossLCL_Platform);
  end
else
  infoln('Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target);
end;

constructor TLazarusCrossInstaller.Create;
begin
end;

destructor TLazarusCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusNativeInstaller }

function TLazarusNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
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
  if ModuleName='BIGIDE' then
    ProcessEx.Parameters.Add('bigide')
  else
    ProcessEx.Parameters.Add('all');
  infoln('Lazarus: running make all:');
  ProcessEx.Execute;
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
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  infoln('Module LAZARUS: Getting/compiling Lazarus...');
  WritelnLog('Lazarus directory:      '+FBaseDirectory,false);
  WritelnLog('Lazarus URL:            '+FURL,false);
  WritelnLog('Lazarus options:        '+FCompilerOptions,false);
  result:=CheckAndGetNeededExecutables;
  BinPath:=IncludeTrailingPathDelimiter(FBaseDirectory)+'bin'+DirectorySeparator+GetFPCTarget(true);
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
  if not InitModule then exit;
  result:=BuildModuleCustom(ModuleName);
end;

function TLazarusInstaller.ConfigModule(ModuleName:string): boolean;
var
  LazarusConfig: TUpdateLazConfig;
begin
  Result:=ForceDirectories(FPrimaryConfigPath);
  infoln('Created Lazarus primary config directory: '+FPrimaryConfigPath);
    // Set up a minimal config so we can use LazBuild
  LazarusConfig:=TUpdateLazConfig.Create(FPrimaryConfigPath);
  try
    try
      LazarusConfig.LazarusDirectory:=FBaseDirectory;
      {$IFDEF MSWINDOWS}
      // FInstalledCompiler could be something like c:\bla\ppc386.exe, e.g.
      // the platform specific compiler. In order to be able to cross compile
      // we'd rather use fpc
      LazarusConfig.CompilerFilename:=ExtractFilePath(FCompiler)+'fpc'+GetExeExt;
      LazarusConfig.DebuggerFilename:=FMakeDir+'gdb'+GetExeExt;
      LazarusConfig.MakeFilename:=FMakeDir+'make'+GetExeExt;
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      // On Unix, FInstalledCompiler should be set to our fpc.sh proxy if installed
      LazarusConfig.CompilerFilename:=FCompiler;
      LazarusConfig.DebuggerFilename:=which('gdb'); //assume in path
      LazarusConfig.MakeFilename:=which('make'); //assume in path
      {$ENDIF UNIX}
      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.FPCSourceDirectory:=FFPCDir;
    except
      on E: Exception do
      begin
        result:=false;
        infoln('Error setting Lazarus config: '+E.ClassName+'/'+E.Message);
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
  if not InitModule then exit;
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
  if Self is TLazarusCrossInstaller then
  begin  // clean out the correct compiler
    ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
    ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
  end;
  ProcessEx.Parameters.Add('distclean');
  if ModuleName='BIGIDE' then
  begin
    ProcessEx.Parameters.Add('bigideclean');
    infoln('Lazarus: running make distclean bigideclean before checkout/update:');
  end
  else
  begin
    infoln('Lazarus: running make distclean before checkout/update:');
  end;
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
  infoln('Checking out/updating Lazarus sources...');
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
  infoln('Lazarus was at revision: '+BeforeRevision);
  if FSVNUpdated then infoln('Lazarus is now at revision: '+AfterRevision) else infoln('No updates for Lazarus found.');
end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  infoln('Module Lazarus: cleanup...');
  try
    // SVN revert Lazarus directory
     FSVNClient.LocalRepository := FBaseDirectory;
     FSVNClient.Repository := FURL;
     FSVNClient.Revert; //Remove local changes
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog('FPC clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      result:=false;
    end;
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

