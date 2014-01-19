unit installerLazarus;
{ Lazarus/LCL installer/updater module
Copyright (C) 2012-2014 Reinier Olislagers, Ludo Brands

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller,processutils, strutils;
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

//Nogui widgetset+Lazbuild:
    'Declare lazbuild;'+
    'Getmodule lazarus;'+
    'Buildmodule lazbuild;'+
    //config lazarus, so lazbuild will work:
    'ConfigModule lazarus;'+
    'End;'+

//standard IDE build with user-selected packages
    // assumes/requires that Laz svn has already been updated
    // also we need lazbuild, but we can check for it in our USERIDE code.
    // If we Require it here, it will kick off a lazbuild build cycle that
    // may already have been done.
    'Declare USERIDE;'+
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
    'End;'+

// Crosscompile build
    'Declare LazarusCrossWin32-64;'+
    // Needs to be run after regular compile because of CPU/OS switch
    'SetCPU x86_64;'+
    'SetOS win64;'+
    // Getmodule has already been done
    'Cleanmodule LCL;'+
    'Buildmodule LCL;'+
    'End;'+

// Crosscompile only LCL (needs to be run at end
    'Declare LCLCross;'+
    'ResetLCL;'+ //module code itself will select proper widgetset
    // Getmodule must already have been done
    'CleanModule LCL;'+
    'Buildmodule LCL;'+
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
    // LCL widget set to be built (NOT OS/CPU combination)
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
    // Install update sources, Qt bindings if needed
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
  infoln('TLazarusCrossInstaller: building module '+ModuleName+'...',etInfo);
  FErrorLog.Clear;
  if Assigned(CrossInstaller) then
  begin
    // Actually not using crossopts - they're only for building an FPC compiler; the
    // relevant options should have been written as a snippet to fpc.cfg and picked
    // up from there.
    CrossInstaller.SetCrossOpt(CrossOPT); //pass on user-requested cross compile options
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
        // Since April 2012, LCL requires lazutils which requires registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
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
        infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' using '+BuildMethod,etInfo)
      else
        infoln('Lazarus: compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+'/'+FCrossLCL_Platform+' using '+BuildMethod,etInfo);

      try
        ProcessEx.Execute;
        result:= ProcessEx.ExitStatus=0;
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
      if not(result) then
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
        if result then
          infoln('Lazarus: Cross compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed. Optional module; continuing regardless.', etInfo)
        else
          infoln('Lazarus: Cross compiling LCL for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed.', etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(result);
      end;
    end; //prereqs in place
  end //valid cross compile setup
  else
    infoln('Lazarus: can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target,eterror);
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
  ExitCode: integer;
  FileCounter: integer;
  LazBuildApp: string;
  OperationSucceeded: boolean;
begin
  OperationSucceeded:=true;
  infoln('TLazarusNativeInstaller: building module '+ModuleName+'...',etInfo);
  if ModuleName<>'USERIDE' then
  begin
    // Make all (should include lcl & ide), lazbuild, lcl etc
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
    { Do not do this - only allow useride to be built for native widgetset.
    LCL /can/ be built using different widgetset
    if FCrossLCL_Platform <> '' then
      ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
    }
    if FCompilerOptions<>'' then
      ProcessEx.Parameters.Add('OPT='+FCompilerOptions);
    case UpperCase(ModuleName) of
      'LAZARUS':
      begin
        ProcessEx.Parameters.Add('all');
        infoln(ModuleName+': running make all:',etInfo);
      end;
      'LAZBUILD':
      begin
        ProcessEx.Parameters.Add('lazbuild');
        infoln(ModuleName+': running make lazbuild:',etInfo);
      end;
      'LCL', 'LCLCROSS':
      begin
        // April 2012: lcl now requires lazutils and registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        ProcessEx.Parameters.Add('registration');
        ProcessEx.Parameters.Add('lazutils');
        ProcessEx.Parameters.Add('lcl');
        if (Uppercase(ModuleName)='LCLCROSS') then
          if FCrossLCL_Platform='' then
          begin
            // Nothing to be done as we're compiling natively. Gracefully exit
            infoln(ModuleName+': empty LCL platform specified. No need to cross compile LCL. Stopping.',etInfo);
            OperationSucceeded:=true; //belts and braces
            result:=true;
            exit;
          end
          else
            ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
        infoln(ModuleName+': running make registration lazutils lcl:', etInfo);
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
      ProcessEx.Execute;
      ExitCode:=ProcessEx.ExitStatus;
      if ExitCode<>0 then
      begin
        OperationSucceeded:=false;
        result:=false;
        WritelnLog('Lazarus: error running make!'+LineEnding+
          'Details: exit code '+inttostr(ExitCode),true);
      end;
    except
      on E: Exception do
      begin
        OperationSucceeded:=false;
        result:=false;
        WritelnLog('Lazarus: exception running make!'+LineEnding+
          'Details: '+E.Message,true);
      end;
    end;

    //Special check for lazbuild as that is known to go wrong
    if (OperationSucceeded) and (UpperCase(ModuleName)='LAZBUILD') then
    begin
      if CheckExecutable(IncludeTrailingPathDelimiter(FBaseDirectory)+'lazbuild'+GetExeExt,
        '--help', 'lazbuild')=false then
      begin
        writelnlog('Lazarus: lazbuild could not be found, so cannot build USERIDE.',true);
        result:=false;
        FInstalledLazarus:= '//*\\error/ / \ \ no valid lazbuild found';
        exit;
      end
    end;

    // Set up debugger if building the IDE with native widgetset
    {$IFDEF MSWINDOWS}
    if (UpperCase(ModuleName)='LAZARUS')
      and (FCrossLCL_Platform='')
      and (FCrossCPU_Target='')
      and (FCrossOS_Target='') then
    begin
      if OperationSucceeded then
      begin
        DebuggerPath:=IncludeTrailingPathDelimiter(FBaseDirectory)+'mingw\bin\'+GetFPCTarget(true)+'\';
        ForceDirectoriesUTF8(DebuggerPath);
        //Copy over debugger files to new Debuggerpath directory
        try
          for FileCounter:=low(FUtilFiles) to high(FUtilFiles) do
          begin
            if (FUtilFiles[FileCounter].Category=ucDebugger) then
              FileUtil.CopyFile(IncludeTrailingPathDelimiter(FMakeDir)+FUtilFiles[FileCounter].FileName,
                IncludeTrailingPathDelimiter(DebuggerPath)+FUtilFiles[FileCounter].FileName);
          end;
        except
          on E: Exception do
          begin
            infoln('Error copying debugger files: '+E.Message,etError);
            infoln('Hint: perhaps you have gdb running at the moment - please kill it.',etWarning);
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
      FInstalledLazarus:= '//*\\error/ / \ \ no valid lazbuild found';
      exit(false);
    end
    else
    begin
      ProcessEx.Executable := LazBuildApp;
      FErrorLog.Clear;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--pcp='+FPrimaryConfigPath);
      // Support keeping userdefined installed packages when building.
      // Compile with selected compiler options
      // Assume new Laz version on failure getting revision
      if strtointdef(Revision,38971)>=38971 then
      begin
        ProcessEx.Parameters.Add('--build-ide=-dKeepInstalledPackages '+FCompilerOptions);
        ProcessEx.Parameters.Add('--build-mode=');
      end
      else
      begin
        // Fallback - depends on hardcoded "Normal IDE" build mode being present
        // We can specify a build mode; otherwise probably the latest build mode will be used
        // which could well be a stripped IDE
        // Let's see how/if FCompilerOptions clashes with the settings in normal build mode
        writelnlog('LazBuild: building UserIDE but falling back to --build-mode=Normal IDE',true);
        ProcessEx.Parameters.Add('--build-ide= '+FCompilerOptions);
        ProcessEx.Parameters.Add('--build-mode=Normal IDE');
      end;

      if FCrossLCL_Platform<>'' then
        ProcessEx.Parameters.Add('--widgetset='+FCrossLCL_Platform );
      infoln('Lazarus: running lazbuild to get IDE with user-specified packages:',etInfo);
      try
        ProcessEx.Execute;
        if ProcessEx.ExitStatus <> 0 then
        begin
          writelnlog('Lazarus: buildmodulecustom: make/lazbuild returned error code '+inttostr(ProcessEx.ExitStatus)+LineEnding+
            'Details: '+FErrorLog.Text,true);
          OperationSucceeded:=false;
          FInstalledLazarus:='//*\\error/ / \ \';
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
    FBaseDirectory,false,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  SetPath(FBinPath,true,false);
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
  LazarusCFG='lazarus.cfg'; //file to store primary config argument in
  StaticPackagesFile='staticpackages.inc';
var
  DebuggerPath: string;
  LazarusConfig: TUpdateLazConfig;
  MajorVersion, MinorVersion, ReleaseVersion: integer;
  PCPSnippet: TStringList;
  StaticPackages: TStringList;
  VersionSnippet:string;
  VersionList: TStringList;
begin
  if DirectoryExistsUTF8(FPrimaryConfigPath)=false then
  begin
    if ForceDirectoriesUTF8(FPrimaryConfigPath) then
      infoln('Created Lazarus primary config directory: '+FPrimaryConfigPath,etInfo);
  end;
  // Set up a minimal config so we can use LazBuild
  // Parse URLs; expect e.g. ..../lazarus_1_0_14.
  // Doesn't take into account release candidates or trunk
  MajorVersion:=-1;
  MinorVersion:=-1;
  ReleaseVersion:=-1;
  if RPos('/',FURL)=Length(FURL) then
    VersionSnippet:=Copy(FURL,1,Length(FURL)-1)
  else
    VersionSnippet:=Copy(FURL,1,MaxInt);
  // Strip out release candidate markings (lazarus1.4.8RCxxx)
  if (RPos('RC',FURL)>1) and (RPos('RC',FURL)>RPos('/',FURL)) then
    VersionSnippet:=Copy(FURL,1,RPos('RC',FURL)-1);
  // Now strip out solitary trailing _
  if RPos('_',FURL)>1 then
    VersionSnippet:=Copy(FURL,1,RPos('_',FURL)-1);
  // Finally remove everything before the last /
  if RPos('/',FURL)>0 then
    VersionSnippet:=Copy(FURL,RPos('/',FURL),MaxInt);
  VersionList:=TStringList.Create;
  try
    VersionList.Delimiter:='_';
    VersionList.QuoteChar:='/'; //quotechar irrelevant here
    VersionList.StrictDelimiter:=true;
    VersionList.DelimitedText:=VersionSnippet;
    // We now have lazarus_1_0_12 or similar
    case VersionList.Count of
      2:
      begin
        MajorVersion:=StrToIntDef(VersionList[1],-1);
        MinorVersion:=0;
        ReleaseVersion:=0;
      end;
      3:
      begin
        MajorVersion:=StrToIntDef(VersionList[1],-1);
        MinorVersion:=StrToIntDef(VersionList[2],-1);
        ReleaseVersion:=0;
      end;
      >3:
      begin
        MajorVersion:=StrToIntDef(VersionList[1],-1);
        MinorVersion:=StrToIntDef(VersionList[2],-1);
        ReleaseVersion:=StrToIntDef(VersionList[3],-1);
      end;
    end;
  finally
    VersionList.Free;
  end;

  LazarusConfig:=TUpdateLazConfig.Create(FPrimaryConfigPath,MajorVersion,MinorVersion,ReleaseVersion);
  try
    try
      // Lazarus 1.2RC1+ and trunk support specifying the primary-config-path that should be used
      // inside the lazarus directory itself.
      PCPSnippet:=TStringList.Create;
      try
        // Martin Friebe mailing list January 2014: no quotes allowed, no trailing blanks
        PCPSnippet.Add('--primary-config-path='+trim(ExcludeTrailingPathDelimiter(FPrimaryConfigPath)));
        if not(FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory)+LazarusCFG)) then
          PCPSnippet.SaveToFile(IncludeTrailingPathDelimiter(FBaseDirectory)+LazarusCFG);
      finally
        PCPSnippet.Free;
      end;
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
        LazarusConfig.SetVariable(EnvironmentConfig,'EnvironmentOptions/DebuggerFilename/Value',DebuggerPath+'gdb')
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
end;

function TLazarusInstaller.CleanModule(ModuleName: string): boolean;
// Make distclean is unreliable; at least for FPC.
// Running it twice apparently can fix a lot of problems; see FPC ML message
// by Jonas Maebe, 1 November 2012
var
  oldlog: TErrorMethod;
begin
  result:=InitModule;
  if not result then exit;
  // If cleaning primary config:
  if (FCrossLCL_Platform='') and (FCrossCPU_Target='') then
    infoln('Lazarus: if your primary config dir has changed, you may want to remove '+IncludeTrailingPathDelimiter(FBaseDirectory)+'lazarus.cfg which points to the primary config path.',etInfo);
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
    infoln('Lazarus: running make distclean (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+'):',etInfo);
  end
  else
  begin
    infoln('Lazarus: running make distclean:',etInfo);
  end;
  ProcessEx.Parameters.Add('distclean');
  try
    // Note: apparently, you can't specify certain modules to clean, like lcl.
    ProcessEx.Execute;
    sleep(100); //now do it again:
    ProcessEx.Execute;
    result:=true;
  except
    on E: Exception do
    begin
      result:=false;
      WritelnLog('Lazarus: running make distclean failed with an exception!'+LineEnding+
        'Details: '+E.Message,true);
    end;
  end;
  ProcessEx.OnErrorM:=oldlog; //restore previous logging
end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  Counter: integer;
  Errors: integer;
  UpdateWarnings: TStringList;
begin
  result:=InitModule;
  if not result then exit;
  infoln('Checking out/updating Lazarus sources:',etInfo);
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
  infoln('Lazarus was at: '+BeforeRevision,etInfo);

  if FRepositoryUpdated then
  begin
    Revision:=AfterRevision;
    infoln('Lazarus is now at: '+AfterRevision,etInfo);
  end
  else
  begin
    Revision:=BeforeRevision;
    infoln('No updates for Lazarus found.',etInfo);
  end;

  // Download Qt bindings if not present yet
  Errors:=0;
  if (result) and (Uppercase(FCrossLCL_Platform)='QT') then
  begin
    for Counter := low(FUtilFiles) to high(FUtilFiles) do
    begin
      if (FUtilFiles[Counter].Category=ucQtFile) and
        not(FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory)+FUtilFiles[Counter].FileName)) then
      begin
        infoln('Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + FBaseDirectory,etInfo);
        try
          if Download(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,
            IncludeTrailingPathDelimiter(FBaseDirectory) + FUtilFiles[Counter].FileName,
            FHTTPProxyHost,
            inttostr(FHTTPProxyPort),
            FHTTPProxyUser,
            FHTTPProxyPassword) = false then
          begin
            Errors := Errors + 1;
            infoln('Error downloading Qt-related file to ' + IncludeTrailingPathDelimiter(FBaseDirectory) + FUtilFiles[Counter].FileName,eterror);
          end;
        except
          on E: Exception do
          begin
            Result := false;
            infoln('Error downloading Qt-related files: ' + E.Message,etError);
            exit; //out of function.
          end;
        end;
      end;
    end;

    if Errors > 0 then
    begin
      Result := false;
      WritelnLog('TLazarusNativeInstaller.GetModule('+ModuleName+'): ' + IntToStr(Errors) + ' errors downloading Qt-related files.', true);
    end;
  end;
end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
const
  LookForConfigFile='environmentoptions.xml';
begin
  if not InitModule then exit;
  infoln('Module Lazarus: Uninstall...',etInfo);
  //sanity check
  if FileExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'Makefile') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'ide') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'lcl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FBaseDirectory)) then
  begin
    result:=DeleteDirectoryEx(FBaseDirectory);
    if not(result) then
      WritelnLog('Error deleting Lazarus directory '+FBaseDirectory);
  end
  else
  begin
    WritelnLog('Error: invalid Lazarus directory :'+FBaseDirectory);
    result:=false;
  end;

  // Sanity check so we don't try to delete random directories
  // Assume Lazarus has been configured/run once so enviroronmentoptions.xml exists.
  if result and
    FileExistsUTF8(IncludeTrailingBackslash(FPrimaryConfigPath)+LookForConfigFile) and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FPrimaryConfigPath)) then
  begin
    result:=DeleteDirectoryEx(FPrimaryConfigPath)=false;
    if not(result) then
      WritelnLog('Error deleting Lazarus PrimaryConfigPath directory '+FPrimaryConfigPath);
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

