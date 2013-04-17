unit installerLazarus;

{ Installs Lazarus+LCL }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller,dynlibs,processutils;
//todo: use processex callback to report on errors like it's done in installerfpc


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
    // bigide config includes Lazarus config:
    'ConfigModule BIGIDE;'+
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
    //We need lazbuild, but we can check for it in our USERIDE code.
    //If we Require it here, it will kick off a lazbuild build cycle that
    //may already have been done.
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
    FBinPath:string;
    FCrossLCL_Platform: string;
    FPrimaryConfigPath: string;
    FRevision: string;
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
    // Local revision of source
    property Revision:string read FRevision write FRevision;
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
uses fpcuputil, fileutil, updatelazconfig
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
  infoln('TLazarusCrossInstaller: building module '+ModuleName+'...',etinfo);
  FErrorLog.Clear;
  if Assigned(CrossInstaller) then
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils',eterror)
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries',eterror)
    else if not CrossInstaller.GetLibsLCL(FCrossLCL_Platform,FBaseDirectory) then
      infoln('Failed to get LCL cross libraries',eterror)
    else
    // Cross compiling prerequisites in place. Let's compile.
    begin
      // If we're "crosscompiling" with the native compiler and binutils - "cross compiling lite" - use lazbuild.
      // Advantages:
      // - dependencies are taken care of
      // - it won't trigger a rebuild of the LCL when the user compiles his first cross project.
      // Otherwise, use make; advantages:
      // - can deal with various bin tools
      // - can deal with compiler options
      // - doesn't need existing lazbuild (+nogui LCL)
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
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath<>'' then
          ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
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
        infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' using '+BuildMethod,etinfo)
      else
        infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+'/'+FCrossLCL_Platform+' using '+BuildMethod,etinfo);

      try
        ProcessEx.Execute;
        result:= ProcessEx.ExitStatus =0;
        if not result then
          WritelnLog('Lazarus: error compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' '+FCrossLCL_Platform+LineEnding+
            'Details: '+FErrorLog.Text,true);
      except
        on E: Exception do
        begin
          result:=false;
          WritelnLog('Lazarus: exception compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+LineEnding+
            'Details: '+E.Message,true);
        end;
      end;
    end //valid cross compile setup
  else
    infoln('Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target,eterror);
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
var
  DebuggerPath: string;
  FileCounter: integer;
  LazBuildApp: string;
  OperationSucceeded: boolean;
begin
  OperationSucceeded:=true;
  infoln('TLazarusNativeInstaller: building module '+ModuleName+'...',etinfo);
  if ModuleName<>'USERIDE' then
  begin
    // Make all (should include lcl & ide)
    // distclean was already run; otherwise specify make clean all
    FErrorLog.Clear;
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
        infoln(ModuleName+': running make bigide:',etinfo);
      end;
      'LAZARUS':
      begin
        ProcessEx.Parameters.Add('all');
        infoln(ModuleName+': running make all:',etinfo);
      end;
      'LAZBUILD':
      begin
        ProcessEx.Parameters.Add('lazbuild');
        infoln(ModuleName+': running make lazbuild:',etinfo);
      end;
      'LCL':
      begin
        // April 2012: lcl now requires lazutils and registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        ProcessEx.Parameters.Add('registration');
        ProcessEx.Parameters.Add('lazutils');
        ProcessEx.Parameters.Add('lcl');
        infoln(ModuleName+': running make registration lazutils lcl:', etinfo);
      end
    else //raise error;
      begin
        ProcessEx.Parameters.Add('--help'); // this should render make harmless
        WritelnLog('BuildModule: Invalid module name '+ModuleName+' specified! Please fix the code.', true);
        FInstalledLazarus:= '//*\\error/ / \ \';
        OperationSucceeded:=false;
        result:=false;
        exit;
      end;
    end;
    try
      ProcessEx.Execute; //check for exitcode takes place further below
    except
      on E: Exception do
      begin
        OperationSucceeded:=false;
        result:=false;
        WritelnLog('Lazarus: exception running make!'+LineEnding+
          'Details: '+E.Message,true);
      end;
    end;

    // Set up debugger if building the IDE
    {$IFDEF MSWINDOWS}
    if (
      (UpperCase(ModuleName)='LAZARUS')
      or (UpperCase(ModuleName)='BIGIDE')
      or (UpperCase(ModuleName)='USERIDE')
      )
      and (FCrossLCL_Platform='')
      and (FCrossCPU_Target='')
      and (FCrossOS_Target='') then
    begin
      if OperationSucceeded then
      begin
        DebuggerPath:=IncludeTrailingPathDelimiter(FBaseDirectory)+'mingw\bin\'+GetFPCTarget(true)+'\';
        ForceDirectoriesUTF8(DebuggerPath);
        //Copy over binutils, all dlls, all manifests to new Debuggerpath directory
        //todo: we should really deal with the debuggerfiles here. Rather than using multiple stringlists, can't we use an array or object list with custom objects/records or something?
        try
          for FileCounter:=0 to FBinUtils.Count-1 do
          begin
            if (LowerCase(ExtractFileExt(FBinUtils[FileCounter]))='.dll') or
              (LowerCase(ExtractFileExt(FBinUtils[FileCounter]))='.manifest') or
              (LowerCase(FBinUtils[FileCounter])='gdb.exe') then
              FileUtil.CopyFile(IncludeTrailingPathDelimiter(FMakeDir)+FBinUtils[FileCounter], IncludeTrailingPathDelimiter(DebuggerPath)+FBinUtils[FileCounter]);
          end;
        except
          on E: Exception do
          begin
            infoln('Error copying debugger files: '+E.Message,eterror);
            infoln('Hint: perhaps you have gdb running at the moment - please kill it.',etwarning);
            OperationSucceeded:=false;
          end;
        end;
      end;
    end;
    {$ENDIF MSWINDOWS}
  end
  else
  begin
    // useride; using lazbuild. Note: in recent Lazarus we could also run make lazbuild useride
    // ... but that apparently calls lazbuild internally anyway.

    // Check for valid lazbuild.
    // Note: we don't check if we have a valid primary config path, but that will come out
    // in the next steps.
    LazBuildApp:=IncludeTrailingPathDelimiter(FBaseDirectory)+'lazbuild'+GetExeExt;
    if CheckExecutable(LazBuildApp, '--help', 'lazbuild')=false then
    begin
      writelnlog('Lazarus: lazbuild could not be found, so cannot build USERIDE.',true);
      result:=false;
      FInstalledLazarus:= '//*\\error/ / \ \ no valid lazbuild found';
      exit;
    end
    else
    begin
      ProcessEx.Executable := LazBuildApp;
      FErrorLog.Clear;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--pcp='+FPrimaryConfigPath);
      // Support keeping userdefined installed packages when building.
      // Assume new Laz version on failure
      if strtointdef(Revision,38971)>=38971 then
      begin
        ProcessEx.Parameters.Add('--build-ide=-dKeepInstalledPackages');
        ProcessEx.Parameters.Add('--build-mode=');
      end
      else
      begin
        //Language dependent fallback
        // We can specify a build mode; otherwise probably the latest build mode will be used
        // which could well be a stripped IDE
        ProcessEx.Parameters.Add('--build-ide=');
        ProcessEx.Parameters.Add('--build-mode=Normal IDE');
      end;

      if FCrossLCL_Platform <>'' then
        ProcessEx.Parameters.Add('os='+FCrossLCL_Platform );
      infoln('Lazarus: running lazbuild to get IDE with user-specified packages:',etinfo);
      try
        ProcessEx.Execute;
        if ProcessEx.ExitStatus <> 0 then
        begin
          writelnlog('Lazarus: buildmodulecustom: make/lazbuild returned error code '+inttostr(ProcessEx.ExitStatus)+LineEnding+
            'Details: '+FErrorLog.Text,true);
          OperationSucceeded:= false;
          FInstalledLazarus:= '//*\\error/ / \ \';
        end
        else
        begin
          FInstalledLazarus:=IncludeTrailingPathDelimiter(FBaseDirectory)+'lazarus'+GetExeExt;
        end;
      except
        on E: Exception do
        begin
          OperationSucceeded:=false;
          WritelnLog('Lazarus: exception running lazbuild to get IDE with user-specified packages!'+LineEnding+
            'Details: '+E.Message,true);
        end;
      end;
    end;
  end;

  result:=OperationSucceeded;
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
  infoln('TLazarusInstaller: initialising...',etDebug);
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
  FBinPath:=ExcludeTrailingPathDelimiter(ExtractFilePath(FCompiler));
  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
  // at least one ; to be present in the path. If you only have one entry, you
  // can add PathSeparator without problems.
  // http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
  SetPath(FBinPath+PathSeparator+
    FMakeDir+PathSeparator+
    FSVNDirectory+PathSeparator+
    FBaseDirectory,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  SetPath(FBinPath,true);
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
const
  StaticPackagesFile='staticpackages.inc';
var
  DebuggerPath: string;
  LazarusConfig: TUpdateLazConfig;
  StaticPackages: TStringList;
begin
  if DirectoryExistsUTF8(FPrimaryConfigPath)=false then
  begin
    if ForceDirectoriesUTF8(FPrimaryConfigPath) then
      infoln('Created Lazarus primary config directory: '+FPrimaryConfigPath,etinfo);
  end;
  // Set up a minimal config so we can use LazBuild
  LazarusConfig:=TUpdateLazConfig.Create(FPrimaryConfigPath);
  try
    try
      // Force English language
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Language/ID', 'en');
      // Set Lazarus directory
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/LazarusDirectory/Value', FBaseDirectory);
      {$IFDEF MSWINDOWS}
      // FInstalledCompiler could be something like c:\bla\ppc386.exe, e.g.
      // the platform specific compiler. In order to be able to cross compile
      // we'd rather use fpc
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/CompilerFilename/Value',ExtractFilePath(FCompiler)+'fpc'+GetExeExt);
      // Post r38092, Lazarus supports this path: $(LazarusDir)\mingw\bin\$(TargetCPU)-$(TargetOS)\gdb.exe
      // or perhaps $(LazarusDir)\mingw\$(TargetCPU)-$(TargetOS)\bin\gdb.exe
      // check for this and fallback to make/binutils directory
      if FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory)+'mingw\bin\'+GetFPCTarget(true)+'\gdb'+GetExeExt) then
        LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value','$(LazarusDir)\mingw\bin\$(TargetCPU)-$(TargetOS)\gdb'+GetExeExt)
      else if FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory)+'mingw\'+GetFPCTarget(true)+'\bin\gdb.exe') then
        LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value','$(LazarusDir)\mingw\$(TargetCPU)-$(TargetOS)\bin\gdb'+GetExeExt)
      else
        LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',IncludeTrailingPathDelimiter(FMakeDir)+'gdb'+GetExeExt);
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/MakeFilename/Value',IncludeTrailingPathDelimiter(FMakeDir)+'make'+GetExeExt);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      // On Unix, FInstalledCompiler should be set to our fpc.sh proxy if installed
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/CompilerFilename/Value',FCompiler);

      {$IFDEF FREEBSD}
      // Check for newer user-installed debugger (e.g. from ports tree
      // The system gdb is ancient (gdb 6.1.1 in FreeBSD 9) and does not work well with Laz
      DebuggerPath:='/usr/local/bin/';
      if CheckExecutable(DebuggerPath+'gdb','--version','GNU gdb') then
        DebuggerPath:=LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',DebuggerPath+'gdb')
      else
        LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',which('gdb')); //system gdb; assume in path
      {$ELSE} //other *nix
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',which('gdb')); //assume in path
      {$ENDIF FREEBSD}

      {$IFDEF BSD}
      {$IFDEF DARWIN}
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/MakeFilename/Value',which('make')); //assume in path
      {$ELSE} //*BSD: FreeBSD, NetBSD, OpenBSD
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/MakeFilename/Value',which('gmake')); //GNU make; assume in path
      {$ENDIF DARWIN}
      {$ENDIF BSD}
      {$ENDIF UNIX}

      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/FPCSourceDirectory/Value',FFPCDir);
      // Debugger type needs to be specified at least since Lazarus 1.1
      LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/Debugger/Class','TGDBMIDebugger');
      // Add <lazarus>\docs\xml to fpdoc editor paths
      LazDocPathAdd(IncludeTrailingPathDelimiter(FBaseDirectory)+'docs\xml',LazarusConfig);
    except
      on E: Exception do
      begin
        result:=false;
        infoln('Error setting Lazarus config: '+E.ClassName+'/'+E.Message,eterror);
      end;
    end;
  finally
    LazarusConfig.Free;
  end;

  if UpperCase(ModuleName)='BIGIDE' then
  begin
    // We might need to add a default staticpackages if it doesn't exist yet.
    // Otherwise adding our own packages could be a mess.
    //todo: note: experimental; don't know if this actually is needed.
    // todo: if possible, let Lazarus generate this somehow? Could be by registering a package, but that is not part of the Lazarus install
    if FileSizeUTF8(IncludeTrailingPathDelimiter(FPrimaryConfigPath)+StaticPackagesFile)<=0 then
    begin
      // Alternative? Try to generate staticpackages list as a byproduct of registering a package that is in bigide anyway:: chmhelpkg
      //infoln('TESTING: not writing static package list. please fix this before release.',eterror);
      StaticPackages:=TStringList.Create;
      try
        // Based on list when adding a new package to a Lazarus SVN (1.1) BIGIDE (April 2012)
        StaticPackages.Add('sqldblaz,');
        StaticPackages.Add('runtimetypeinfocontrols,');
        StaticPackages.Add('printers4lazide,');
        StaticPackages.Add('leakview,');
        StaticPackages.Add('memdslaz,');
        StaticPackages.Add('instantfpclaz,');
        StaticPackages.Add('externhelp,');
        StaticPackages.Add('turbopoweripro,');
        StaticPackages.Add('jcfidelazarus,');
        StaticPackages.Add('chmhelppkg,');
        StaticPackages.Add('fpcunitide,');
        StaticPackages.Add('projtemplates,');
        StaticPackages.Add('tachartlazaruspkg,');
        StaticPackages.Add('todolistlaz,');
        StaticPackages.Add('dbflaz,');
        StaticPackages.Add('printer4lazarus,');
        StaticPackages.Add('sdflaz,');
        StaticPackages.Add(''); //empty line at end occurs in my installed Lazarus
        StaticPackages.SaveToFile(IncludeTrailingPathDelimiter(FPrimaryConfigPath)+StaticPackagesFile);
      finally
        StaticPackages.Free;
      end;
    end;
  end;
end;

function TLazarusInstaller.CleanModule(ModuleName: string): boolean;
// Make distclean is unreliable; at least for FPC.
// Running it twice apparently can fix a lot of problems; see FPC ML message
// by Jonas Maebe, 1 November 2012
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
    infoln('Lazarus: running make distclean (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+'):',etinfo);
  end
  else
  begin
    infoln('Lazarus: running make distclean:',etinfo);
  end;
  ProcessEx.Parameters.Add('distclean');
  try
    // Note: apparently, you can't specify certain modules to clean, like lcl, bigide...
    ProcessEx.Execute;
    sleep(100); //now do it again:
    ProcessEx.Execute;
    ProcessEx.OnErrorM:=oldlog;
    result:=true;
  except
    on E: Exception do
    begin
      result:=false;
      WritelnLog('Lazarus: running make distclean failed with an exception!'+LineEnding+
        'Details: '+E.Message,true);
    end;
  end;
end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  UpdateWarnings: TStringList;
begin
  if not InitModule then exit;
  infoln('Checking out/updating Lazarus sources:',etinfo);
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

  infoln('Lazarus was at: '+BeforeRevision,etinfo);
  if FRepositoryUpdated then
  begin
    Revision:=AfterRevision;
    infoln('Lazarus is now at: '+AfterRevision,etinfo);
  end
  else
  begin
    Revision:=BeforeRevision;
    infoln('No updates for Lazarus found.',etinfo);
  end;
end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
const
  LookForConfigFile='environmentoptions.xml';
begin
  if not InitModule then exit;
  infoln('Module Lazarus: Uninstall...',etinfo);
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

