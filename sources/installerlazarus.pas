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

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils, strutils;
//todo: use processex callback to report on errors like it's done in installerfpc

const
  Sequences =
    //standard lazarus build
    _DECLARE+_LAZARUS+_SEP +
    _EXECUTE+_CHECKDEVLIBS+_SEP+
    _CLEANMODULE+_LAZARUS+_SEP +
    _CHECKMODULE+_LAZARUS+_SEP +
    _GETMODULE+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    _DO+_USERIDE+_SEP +
    _BUILDMODULE+_STARTLAZARUS+_SEP +
    _DO+_UNIVERSALDEFAULT+_SEP+
    _DO+_HELPLAZARUS+_SEP+
    _BUILDMODULE+_INSTALLLAZARUS+_SEP +
    _EXECUTE+_CREATELAZARUSSCRIPT+_SEP +
    _END +

    _DECLARE+_LAZARUSSIMPLE+_SEP +
    _EXECUTE+_CHECKDEVLIBS+_SEP+
    _CLEANMODULE+_LAZARUS+_SEP +
    _CHECKMODULE+_LAZARUS+_SEP +
    _GETMODULE+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    _BUILDMODULE+_LAZARUS+_SEP +
    _BUILDMODULE+_STARTLAZARUS+_SEP +
    _BUILDMODULE+_INSTALLLAZARUS+_SEP +
    _EXECUTE+_CREATELAZARUSSCRIPT+_SEP +
    _END +

    //standard clean
    _DECLARE+_LAZARUS+_CLEAN+_SEP+
    _CLEANMODULE+_LAZARUS+_SEP +
    _END +

    //standard uninstall
    _DECLARE+_LAZARUS+_UNINSTALL+_SEP+
    //_CLEANMODULE+_LAZARUS+_SEP+
    _UNINSTALLMODULE+_LAZARUS+_SEP +
    _EXECUTE+_DELETELAZARUSSCRIPT+_SEP +
    _END +

    //selective actions triggered with --only=SequenceName
    _DECLARE+_LAZARUS+_CHECK+_ONLY+_SEP + _CHECKMODULE+_LAZARUS+_SEP + _END +
    _DECLARE+_LAZARUS+_CLEAN+_ONLY+_SEP + _CLEANMODULE+_LAZARUS+_SEP + _END +
    _DECLARE+_LAZARUS+_GET+_ONLY+_SEP + _GETMODULE+_LAZARUS+_SEP + _END +
    _DECLARE+_LAZARUS+_BUILD+_ONLY+_SEP + _BUILDMODULE+_LAZARUS+_SEP + _END +
    _DECLARE+_LAZARUS+_CONFIG+_ONLY+_SEP + _CONFIGMODULE+_LAZARUS+_SEP + _END +

    _DECLARE+_LAZARUSCLEANBUILDONLY+_SEP +
    _CLEANMODULE+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    _DO+_USERIDE+_SEP +
    _BUILDMODULE+_STARTLAZARUS+_SEP +
    _DO+_UNIVERSALDEFAULT+_SEP+
    _EXECUTE+_CREATELAZARUSSCRIPT+_SEP +
    _END +

    _DECLARE+_LAZARUSREMOVEONLY+_SEP +
    _CLEANMODULE+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    //_UNINSTALLMODULE+_LAZARUS+_SEP +
    _END +

    // Compile only LCL
    _DECLARE+_LCL+_SEP +
    _CLEANMODULE+_LCL+_SEP +
    _BUILDMODULE+_LCL+_SEP +
    _END +

    // Clean (remove only LCL
    _DECLARE+_LCLREMOVEONLY+_SEP +
    _CLEANMODULE+_LCL+_SEP +
    _UNINSTALLMODULE+_LCL+_SEP +
    _END +

    // Clean (remove only components)
    _DECLARE+_COMPONENTSREMOVEONLY+_SEP +
    _CLEANMODULE+_COMPONENTS+_SEP +
    _UNINSTALLMODULE+_COMPONENTS+_SEP +
    _END +

    // Clean (remove only packager)
    _DECLARE+_PACKAGERREMOVEONLY+_SEP +
    _CLEANMODULE+_PACKAGER+_SEP +
    _UNINSTALLMODULE+_PACKAGER+_SEP +
    _END +

    _DECLARE+_LCLALLREMOVEONLY+_SEP +
    _DO+_LCLREMOVEONLY+_SEP +
    _DO+_COMPONENTSREMOVEONLY+_SEP +
    _DO+_PACKAGERREMOVEONLY+_SEP +
    _END +

    //standard lazbuild build
    _DECLARE+_LAZBUILD+_SEP +
    _BUILDMODULE+_LAZBUILD+_SEP +
    _END +

    //special lazbuild standalone build (for docker use)
    _DECLARE+_LAZBUILD+_ONLY+_SEP +
    _CLEANMODULE+_LAZBUILD+_SEP +
    _CHECKMODULE+_LAZBUILD+_SEP +
    _GETMODULE+_LAZBUILD+_SEP +
    _CONFIGMODULE+_LAZBUILD+_SEP +
    _BUILDMODULE+_LAZBUILD+_SEP +
    _END +

    //standard useride build
    _DECLARE+_USERIDE+_SEP +
    _BUILDMODULE+_LAZBUILD+_SEP +
    //_BUILDMODULE+'IdeDebugger'+_SEP +
    _BUILDMODULE+_USERIDE+_SEP +
    _END +

    {$ifdef mswindows}
    {$ifdef win32}
    // Crosscompile build
    _DECLARE+_LAZARUS+_CROSSWIN+_SEP +
    _SETCPU+'x86_64'+_SEP + _SETOS+'win64'+_SEP +
    _DO+_LCL+_SEP+
    _SETCPU+'i386'+_SEP+ _SETOS+'win32'+_SEP+
    _END +
    {$endif}
    {$ifdef win64}
    _DECLARE+_LAZARUS+_CROSSWIN+_SEP +
    _SETCPU+'i386'+_SEP+ _SETOS+'win32'+_SEP+
    _DO+_LCL+_SEP+
    _SETCPU+'x86_64'+_SEP + _SETOS+'win64'+_SEP +
    _END +
    {$endif}
    {$endif mswindows}

    // Crosscompile only LCL native widgetset (needs to be run at end)
    _DECLARE+_LCLCROSS+_SEP +
    _RESETLCL+_SEP + //module code itself will select proper widgetset
    _CLEANMODULE+_LCLCROSS+_SEP+
    _BUILDMODULE+_LCLCROSS+_SEP +
    _END+

    _DECLARE+_CONFIG+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    _END +

    _DECLARE+_MAKEFILECHECKLAZARUS+_SEP+
    _BUILDMODULE+_MAKEFILECHECKLAZARUS+_SEP+

    _ENDFINAL;

  DEFAULTLPI =
    '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding+
    '<CONFIG>'+LineEnding+
    '  <ProjectOptions>'+LineEnding+
    '    <General>'+LineEnding+
    '      <SessionStorage Value="InProjectDir"/>'+LineEnding+
    '      <MainUnit Value="0"/>'+LineEnding+
    '      <Title Value="project1"/>'+LineEnding+
    '    </General>'+LineEnding+
    '    <BuildModes Count="1">'+LineEnding+
    '      <Item1 Name="Default" Default="True"/>'+LineEnding+
    '    </BuildModes>'+LineEnding+
    '    <Units Count="1">'+LineEnding+
    '      <Unit0>'+LineEnding+
    '        <Filename Value="project1.lpr"/>'+LineEnding+
    '        <IsPartOfProject Value="True"/>'+LineEnding+
    '      </Unit0>'+LineEnding+
    '    </Units>'+LineEnding+
    '  </ProjectOptions>'+LineEnding+
    '</CONFIG>';

  DEFAULTLPR =
    'program project1;'+LineEnding+
    ''+LineEnding+
    'begin'+LineEnding+
    '  writeln(''Hello world from fpcupdeluxe !'');'+LineEnding+
    'end.';

  LAZARUSCFG = 'lazarus.cfg'; //file to store primary config argument in

type

  { TLazarusInstaller }

  TLazarusInstaller = class(TBaseLazarusInstaller)
  private
    FLCL_Platform: string;
    InitDone: boolean;
    function LCLCrossActionNeeded:boolean;
  protected
    function GetVersionFromSource:string;override;
    function GetVersionFromUrl(aUrl:string):string;override;
    function GetReleaseCandidateFromSource:integer;override;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; virtual;
    function GetLazarusVersion: string;
    function IsCross:boolean;override;
    function InitModule: boolean;
  public
    // LCL widget set to be built (NOT OS/CPU combination)
    property LCL_Platform: string write FLCL_Platform;
    function BuildModule(ModuleName: string): boolean; override;
    // Create configuration in FLazarusPrimaryConfigPath
    function ConfigModule(ModuleName: string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName: string): boolean; override;
    // Install update sources, Qt bindings if needed
    function GetModule(ModuleName: string): boolean; override;
    // Perform some checks on the sources
    function CheckModule(ModuleName: string): boolean; override;
    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLazarusNativeInstaller }

  TLazarusNativeInstaller = class(TLazarusInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TLazarusCrossInstaller }

  TLazarusCrossInstaller = class(TLazarusInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; override;
  public
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$ifdef Unix}
  BaseUnix,
  {$ifdef LCLQT5}
  LazFileUtils,
  {$endif}
  {$endif}
  FileUtil,
  fpcuputil,
  repoclient,
  updatelazconfig;

{ TLazarusCrossInstaller }

function TLazarusCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  Options      : string;
  LazBuildApp  : string;
begin
  Result:=inherited;

  FErrorLog.Clear;

  if Assigned(CrossInstaller) then
  begin
    //No need to Reset
    //Just use the values as available
    //CrossInstaller.Reset;

    // Actually not using crossopts - they're only for building an FPC compiler; the
    // relevant options should have been written as a snippet to fpc.cfg and picked
    // up from there.
    CrossInstaller.SetFPCVersion(CompilerVersion(FCompiler));
    CrossInstaller.SetCrossOpt(CrossOPT); //pass on user-requested cross compile options
    CrossInstaller.SetSubArch(CrossOS_SubArch);
    CrossInstaller.SetABI(CrossOS_ABI);

    if (ieBins in FErrorCodes) then
      Infoln(infotext+'Failed to get crossbinutils', etError)
    else
    if(ieLibs in FErrorCodes) then
      Infoln(infotext+'Failed to get cross libraries', etError)
    else
    if not CrossInstaller.GetLibsLCL(FLCL_Platform, BaseDirectory) then
      Infoln(infotext+'Failed to get LCL cross libraries', etError)
    else
    begin
      // Cross compiling prerequisites in place. Let's compile.
      // If we're "crosscompiling" with the native compiler and binutils - "cross compiling [lite]" - use lazbuild.
      // Advantages:
      // - dependencies are taken care of
      // - it won't trigger a rebuild of the LCL when the user compiles his first cross project.
      // Otherwise, use make; advantages:
      // - can deal with various bin tools
      // - can deal with compiler options
      // - doesn't need existing lazbuild (+nogui LCL)

      LazBuildApp := IncludeTrailingPathDelimiter(InstallDirectory) + LAZBUILDNAME + GetExeExt;
      if CheckExecutable(LazBuildApp, ['--help'], LAZBUILDNAME) = false then
      begin
        WritelnLog(etWarning, infotext+'Lazbuild could not be found ... using make to cross-build '+ModuleName, true);
        LazBuildApp := '';
      end;

      // Since April 2012, LCL requires lazutils which requires registration
      // https://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
      //https://lists.lazarus-ide.org/pipermail/lazarus/2012-April/138168.html

      if Length(LazBuildApp)=0 then
      begin
        // Use make for cross compiling
        // Check unwanted forced update through ViaMakefile and .compiled

        Processor.Executable := Make;
        Processor.Process.Parameters.Clear;
        {$IFDEF MSWINDOWS}
        if Length(Shell)>0 then Processor.SetParamData('SHELL='+Shell);
        {$ENDIF}
        Processor.Process.CurrentDirectory := SourceDirectory;
        Processor.SetParamData('--directory='+Processor.Process.CurrentDirectory);

        {$IF DEFINED(CPUARM) AND DEFINED(LINUX)}
        Processor.SetParamData('--jobs=1');
        {$ELSE}
        //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
        //if (NOT FNoJobs) then
        //  Processor.SetParamData('--jobs='+IntToStr(FCPUCount));
        {$ENDIF}

        Processor.SetParamData('USESVN2REVISIONINC=0');

        Processor.SetParamMakefilePathData('FPC',FCompiler);
        Processor.SetParamMakefilePathData('PP',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));

        Processor.SetParamMakefilePathData('PREFIX',InstallDirectory);
        Processor.SetParamMakefilePathData('INSTALL_PREFIX',InstallDirectory);
        Processor.SetParamMakefilePathData('LAZARUS_INSTALL_DIR',InstallDirectory);

        //Make sure our FPC units can be found by Lazarus
        Processor.SetParamMakefilePathData('FPCDIR',FFPCSourceDir);
        //Processor.SetParamMakefilePathData('FPCDIR',FFPCInstallDir);
        //Processor.SetParamMakefilePathData('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(false)]));

        //Make sure Lazarus does not pick up these tools from other installs
        Processor.SetParamMakefilePathData('FPCMAKE',FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt);
        Processor.SetParamMakefilePathData('PPUMOVE',FPCBinDir+DirectorySeparator+'ppumove'+GetExeExt);

        {$ifdef MsWindows}
        Processor.SetParamData('UPXPROG=echo');      //Don't use UPX
        (*
        // do we have a stray shell in the path ...
        if StrayShell then
        begin
          s1:=ExtractFilePath(Make)+'gecho.exe';
          if FileExists(s1) then Processor.SetParamMakefilePathData('ECHOREDIR',s1);
        end;
        *)
        {$else}
        //Processor.SetParamMakefilePathData('INSTALL_BINDIR',FBinPath);
        {$endif}

        Processor.SetParamData('OS_SOURCE=' + GetSourceOS);
        Processor.SetParamData('CPU_SOURCE=' + GetSourceCPU);

        Processor.SetParamData('OS_TARGET=' + CrossInstaller.TargetOSName);
        Processor.SetParamData('CPU_TARGET=' + CrossInstaller.TargetCPUName);

        //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
        //Todo: to be investigated
        //Processor.SetParamMakefilePathData('FPCFPMAKE',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));

        //Set standard options
        Options := STANDARDCOMPILERVERBOSITYOPTIONS;

        //Always limit the search for fpc.cfg to our own fpc.cfg
        //Only needed on Windows. On Linux, we have already our own config dir for fpc.cfg
        {$ifdef Windows}
        Options := Options+' -n @'+GetFPCConfigPath(FPCCONFIGFILENAME);
        {$endif}

        // Add remaining options
        Options := Options+' '+FCompilerOptions;

        while Pos('  ',Options)>0 do
        begin
          Options:=StringReplace(Options,'  ',' ',[rfReplaceAll]);
        end;
        Options:=Trim(Options);
        if Length(Options)>0 then Processor.SetParamData('OPT='+Options);

        if FLCL_Platform <> '' then
          Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);

        //Processor.SetParamData('all');

        Processor.SetParamData('registration');
        Processor.SetParamData('lazutils');
        Processor.SetParamData('lcl');
        Processor.SetParamData('basecomponents');

      end
      else
      begin
        // Use lazbuild for cross compiling
        Processor.Executable := LazBuildApp;
        Processor.Process.CurrentDirectory := SourceDirectory;
        Processor.Process.Parameters.Clear;
        {$IFDEF DEBUG}
        Processor.SetParamData('--verbose');
        {$ELSE}
        // See compileroptions.pp
        // Quiet:=ConsoleVerbosity<=-3;
        Processor.SetParamData('--quiet');
        {$ENDIF}

        Processor.SetParamData('--pcp=' + DoubleQuoteIfNeeded(FLazarusPrimaryConfigPath));

        // Apparently, the .compiled file, that are used to check for a rebuild, do not contain a cpu setting if cpu and cross-cpu do not differ !!
        // So, use this test to prevent a rebuild !!!
        if (GetSourceCPU<>CrossInstaller.TargetCPUName) then
          Processor.SetParamData('--cpu=' + CrossInstaller.TargetCPUName);

        // See above: the same for OS !
        if (GetSourceOS<>CrossInstaller.TargetOSName) then
          Processor.SetParamData('--os=' + CrossInstaller.TargetOSName);

        if FLCL_Platform <> '' then
          Processor.SetParamData('--ws=' + FLCL_Platform);

        Processor.SetParamData(ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('packager'),'registration'])+DirectorySeparator+'fcl.lpk');
        Processor.SetParamData(ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('components'),'lazutils'])+DirectorySeparator+'lazutils.lpk');
        Processor.SetParamData(ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('lcl'),'interfaces'])+DirectorySeparator+'lcl.lpk');
        // Also add the basecomponents !
        Processor.SetParamData(ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('components'),'synedit'])+DirectorySeparator+'synedit.lpk');
        Processor.SetParamData(ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('components'),'lazcontrols'])+DirectorySeparator+'lazcontrols.lpk');
        Processor.SetParamData(ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('components'),'ideintf'])+DirectorySeparator+'ideintf.lpk');
      end;

      if FLCL_Platform = '' then
        Infoln(infotext+'Compiling LCL for ' + GetFPCTarget(false) + ' using ' + ExtractFileName(Processor.Executable), etInfo)
      else
        Infoln(infotext+'Compiling LCL for ' + GetFPCTarget(false) + '/' + FLCL_Platform + ' using ' + ExtractFileName(Processor.Executable), etInfo);


      // Add binutils path to path if necessary
      if CrossInstaller.BinUtilsPathInPath then
         SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),false,true);

      try
        ProcessorResult:=Processor.ExecuteAndWait;
        Result := (ProcessorResult = 0);
        if (not Result) then
          WritelnLog(etError,infotext+'Error compiling LCL for ' + GetFPCTarget(false) + ' ' + FLCL_Platform + LineEnding +
            'Details: ' + FErrorLog.Text, true);
      except
        on E: Exception do
        begin
          Result := false;
          WritelnLog(etError,infotext+'Exception compiling LCL for ' + GetFPCTarget(false) + LineEnding +
            'Details: ' + E.Message, true);
        end;
      end;


      if not (Result) then
      begin
        // Not an error but warning for optional modules: crosswin32-64 and crosswin64-32
        // These modules need to be optional because FPC 2.6.2 gives an error crosscompiling regarding fpdoc.css or something.
        {$ifdef win32}
        // if this is crosswin32-64, ignore error as it is optional
        if (CrossInstaller.TargetCPU=TCPU.x86_64) and ((CrossInstaller.TargetOS=TOS.win64) or (CrossInstaller.TargetOS=TOS.win32)) then
          Result := true;
        {$endif win32}
        {$ifdef win64}
        // if this is crosswin64-32, ignore error as it is optional
        if (CrossInstaller.TargetCPU=TCPU.i386) and (CrossInstaller.TargetOS=TOS.win32) then
          Result := true;
        {$endif win64}
        if Result then
          Infoln(infotext+'Cross compiling LCL for ' + GetFPCTarget(false) +
            ' failed. Optional module; continuing regardless.', etWarning)
        else
          Infoln(infotext+'Cross compiling LCL for ' + GetFPCTarget(false) + ' failed.', etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(Result);
      end;
    end; //prereqs in place

  end    //valid cross compile setup
  else
    Infoln(infotext+'Can''t find cross installer for ' + GetFPCTarget(false), etError);
end;

function TLazarusCrossInstaller.UnInstallModule(ModuleName:string): boolean;
var
  aDir:string;
begin
  result:=true; //succeed by default

  if not DirectoryExists(InstallDirectory) then
  begin
    Infoln(infotext+'No Lazarus install [yet] ... nothing to be done',etDebug);
  end;
  if CheckDirectory(InstallDirectory) then exit;

  Result := InitModule;
  if not Result then exit;

  FErrorLog.Clear;

  if assigned(CrossInstaller) AND (Length(BaseDirectory)>0) AND (NOT CheckDirectory(BaseDirectory)) then
  begin
    if ((CrossInstaller.TargetCPU=TCPU.cpuNone) OR (CrossInstaller.TargetOS=TOS.osNone)) then exit;

    CrossInstaller.Reset;
    CrossInstaller.SetFPCVersion(CompilerVersion(FCompiler));

    case ModuleName of
      _LCL:
      begin
        aDir:=IncludeTrailingPathDelimiter(InstallDirectory)+'lcl'+DirectorySeparator+'units'+DirectorySeparator+GetFPCTarget(false);
        if DirectoryExists(aDir) then if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+aDir);
        end;
      end;
      _PACKAGER:
      begin
        aDir:=IncludeTrailingPathDelimiter(InstallDirectory)+'packager'+DirectorySeparator+'units'+DirectorySeparator+GetFPCTarget(false);
        if DirectoryExists(aDir) then if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+aDir);
        end;
      end;
      _COMPONENTS:
      begin
        aDir:=IncludeTrailingPathDelimiter(InstallDirectory)+'components'+DirectorySeparator+'lazutils'+DirectorySeparator+'lib'+DirectorySeparator+GetFPCTarget(false);
        if DirectoryExists(aDir) then if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+aDir);
        end;
      end;
      _LCLCROSS:
      begin
      end;
    end;
  end;
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
  i,ExitCode                     : integer;
  s1,s2,LazBuildApp,FPCDirStore  : string;
  {$ifdef MSWindows}
  OldPath                        : string;
  {$endif}
  OperationSucceeded             : boolean;
begin
  Result:=inherited;

  OperationSucceeded := true;

  //Get Freetype and Zlib for ao fpreport ... just to be sure
  {$IFDEF MSWINDOWS}
  //DownloadFreetype;
  //DownloadZlib;
  {$ENDIF}


  if ((ModuleName=_LAZARUS) OR (ModuleName=_LAZBUILD) OR (ModuleName=_USERIDE)) then
  begin
    if (Length(ActualRevision)=0) OR (ActualRevision='failure') then
    begin
      s2:=GetRevision(ModuleName);
      if (Length(s2)=0) then
      begin
        // In case we only do a rebuild of the available sources, get the revision while it might be missing
        // And presume that we have git sources, which might be not true in all cases, but anyhow
        GitClient.LocalRepository:=SourceDirectory;
        if GitClient.LocalRepositoryExists then s2:=GitClient.LocalRevision;
      end;
      if Length(s2)>0 then FActualRevision:=s2;
    end;
    if ((ModuleName=_LAZARUS) OR (ModuleName=_USERIDE)) then
    begin
      Infoln(infotext+'Now building '+ModuleName,etInfo);
      s1:=ConcatPaths([SourceDirectory,'ide'])+DirectorySeparator+REVINCFILENAME;
      // If not there, store the revision in the appropriate location
      if (NOT FileExists(s1)) then
      begin
        s2:=ActualRevision;
        if (Length(s2)=0) OR (s2='failure') then s2:='unknown';
        CreateRevision(_LAZARUS,s2);
      end;
    end;
  end;

  //Note: available in more recent Lazarus : use "make lazbuild useride" to build ide with installed packages
  {$ifdef FORCELAZBUILD}
  if (ModuleName<>_USERIDE) then
  {$else}
  if ((ModuleName<>_USERIDE) OR (SourceVersionNum>=CalculateFullVersion(1,6,2))) then
  {$endif}
  begin
    // Make all (should include lcl & ide), lazbuild, lcl etc
    // distclean was already run; otherwise specify make clean all
    FErrorLog.Clear;
    Processor.Executable := Make;
    Processor.Process.Parameters.Clear;
    {$IFDEF MSWINDOWS}
    if Length(Shell)>0 then Processor.SetParamData('SHELL='+Shell);
    {$ENDIF}
    Processor.Process.CurrentDirectory := SourceDirectory;
    Processor.SetParamData('--directory='+Processor.Process.CurrentDirectory);

    {$IF DEFINED(CPUARM) AND DEFINED(LINUX)}
    Processor.SetParamData('--jobs=1');
    {$ELSE}
    //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
    //if (NOT FNoJobs) then
    //  Processor.SetParamData('--jobs='+IntToStr(FCPUCount));
    {$ENDIF}

    Processor.SetParamData('USESVN2REVISIONINC=0');

    Processor.SetParamMakefilePathData('FPC',FCompiler);
    Processor.SetParamMakefilePathData('PP',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));

    Processor.SetParamMakefilePathData('PREFIX',InstallDirectory);
    Processor.SetParamMakefilePathData('INSTALL_PREFIX',InstallDirectory);
    Processor.SetParamMakefilePathData('INSTALL_BASEDIR',InstallDirectory);
    Processor.SetParamMakefilePathData('LAZARUS_INSTALL_DIR',IncludeTrailingPathDelimiter(InstallDirectory));

    //Make sure our FPC units can be found by Lazarus
    Processor.SetParamMakefilePathData('FPCDIR',FFPCSourceDir);
    //Processor.SetParamMakefilePathData('FPCDIR',FFPCInstallDir);
    //Processor.SetParamMakefilePathData('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true)]));

    //Make sure Lazarus does not pick up these tools from other installs
    Processor.SetParamMakefilePathData('FPCMAKE',FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt);
    Processor.SetParamMakefilePathData('PPUMOVE',FPCBinDir+DirectorySeparator+'ppumove'+GetExeExt);

    {$ifdef MsWindows}
    Processor.SetParamData('UPXPROG=echo');      //Don't use UPX
    (*
    // do we have a stray shell in the path ...
    if StrayShell then
    begin
      s1:=ExtractFilePath(Make)+'gecho.exe';
      if FileExists(s1) then Processor.SetParamMakefilePathData('ECHOREDIR',s1);
    end;
    *)
    {$else}
    //Processor.SetParamMakefilePathData('INSTALL_BINDIR',FBinPath);
    {$endif}

    //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
    //Todo: to be investigated
    //Processor.SetParamMakefilePathData('FPCFPMAKE',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));

    if FLCL_Platform <> '' then
      Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);

    //Set standard options
    s1:=STANDARDCOMPILERVERBOSITYOPTIONS;

    //Always limit the search for fpc.cfg to our own fpc.cfg
    //Only needed on Windows. On Linux, we have already our own config dir for fpc.cfg
    {$ifdef MsWindows}
    //s1:=s1+' -n @'+GetFPCConfigPath(FPCCONFIGFILENAME);
    {$endif}

    // Add remaining options
    s1:=s1+' '+FCompilerOptions;

    //Lazbuild MUST be build without giving any extra optimization options
    //At least on Linux anything else gives errors when trying to use lazbuild ... :-(
    if ModuleName=_LAZBUILD then
    begin
      i:=Pos('-O',s1);
      if i>0 then
      begin
        if s1[i+2] in ['0'..'9'] then
        begin
          Delete(s1,i,3);
        end;
      end;
    end;

    {$ifdef Unix}
      {$ifndef Darwin}
        {$ifdef LCLQT}
        {$endif}
        {$ifdef LCLQT5}
        if LibWhich(LIBQT5,s2) then
        begin
          s1:=s1+' -Fl'+ExcludeTrailingPathDelimiter(s2);
        end
        else
        begin
          // Did we copy the QT5 libs ??
          // If so, add some linker help.
          if (FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5)) then
          begin
            s1:=s1+' -k"-rpath=./"';
            s1:=s1+' -k"-rpath=$$ORIGIN"';
            s1:=s1+' -k"-rpath=\\$$$$$\\ORIGIN"';
            s1:=s1+' -Fl'+InstallDirectory;
          end;
        end;
        {$endif}
      {$endif}
    {$endif}

    // remove double spaces
    while Pos('  ',s1)>0 do
    begin
      s1:=StringReplace(s1,'  ',' ',[rfReplaceAll]);
    end;
    s1:=Trim(s1);

    if Length(s1)>0 then Processor.SetParamData('OPT='+s1);

    case ModuleName of
      _USERIDE:
      begin
        {$ifdef DISABLELAZBUILDJOBS}
        Processor.SetParamData('LAZBUILDJOBS=1');//prevent runtime 217 errors
        {$else}
        Processor.SetParamData('LAZBUILDJOBS='+IntToStr(FCPUCount));
        {$endif}
        Processor.SetParamData('useride');

        s1:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+DefaultIDEMakeOptionFilename;

        //if FileExists(s1) then
          //Processor.SetParamData('CFGFILE=' + s1);
          Processor.SetParamMakefilePathData('CFGFILE',s1);

        Infoln(infotext+'Running: make useride', etInfo);

        (*
        s1:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+DefaultIDEMakeOptionFilename;
        if FileExists(s1) then
        begin
          // this uses lazbuild as per definition in the Lazarus Makefile
          Processor.SetParamData('LAZBUILDJOBS='+IntToStr(FCPUCount));
          // Add the ide config build file when it is there
          Processor.SetParamData('CFGFILE=' + s1);
          Processor.SetParamData('useride');
          Infoln(infotext+'Running: make useride', etInfo);
        end
        else
        begin
          // sometimes, we get an error 217 when buidling lazarus for the first time.
          // the below tries to prevent this by not using lazbuild on a fresh install.
          Processor.SetParamData('registration');
          Processor.SetParamData('lazutils');
          Processor.SetParamData('lcl');
          Processor.SetParamData('basecomponents');
          Processor.SetParamData('ide');
          Infoln(infotext+'Running: make registration lazutils lcl basecomponents ide', etInfo);
        end;
        *)
      end;
      _IDE:
      begin
        Processor.SetParamData('idepkg');
        Infoln(infotext+'Running: make idepkg', etInfo);
      end;
      _BIGIDE:
      begin
        Processor.SetParamData('idebig');
        Infoln(infotext+'Running: make idebig', etInfo);
      end;
      _LAZARUS:
      begin
        Processor.SetParamData('all');
        Infoln(infotext+'Running: make all', etInfo);
      end;
      _STARTLAZARUS:
      begin
        if FileExists(IncludeTrailingPathDelimiter(SourceDirectory) + 'startlazarus' + GetExeExt) then
        begin
          Infoln(infotext+'StartLazarus already available ... skip building it.', etInfo);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
        Processor.SetParamData('starter');
        Infoln(infotext+'Running: make starter', etInfo);
      end;
      _LAZBUILD:
      begin
        if FileExists(IncludeTrailingPathDelimiter(SourceDirectory) + LAZBUILDNAME + GetExeExt) then
        begin
          Infoln(infotext+'Lazbuild already available ... skip building it.', etInfo);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
        Processor.SetParamData('lazbuild');
        Infoln(infotext+'Running: make lazbuild', etInfo);
      end;
      _LCL:
      begin
        // April 2012: lcl now requires lazutils and registration
        // https://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        // https://lists.lazarus-ide.org/pipermail/lazarus/2012-April/138168.html
        Processor.SetParamData('registration');
        Processor.SetParamData('lazutils');
        Processor.SetParamData('lcl');
        // always build standard LCL for native system ... other widgetsets to be done by LCLCROSS: see below
        //if FCrossLCL_Platform<>'' then Processor.SetParamData('LCL_PLATFORM=' + FCrossLCL_Platform);
        Infoln(infotext+'Running: make registration lazutils lcl', etInfo);
      end;
      _LCLCROSS:
      begin
        if LCLCrossActionNeeded then
        begin
          Processor.SetParamData('-C '+ConcatPaths([SourceDirectory,'lcl']));
          Processor.SetParamData('intf');
          Infoln(infotext+'Running: make -C lcl intf', etInfo);
        end
        else
        begin
          // nothing to be done: exit graceously
          Infoln(infotext+'No extra LCL_PLATFORM defined ... nothing to be done', etDebug);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
      end;
      _INSTALLLAZARUS:
      begin
        if ((SourceVersionNum<>0) AND (SourceVersionNum<CalculateFullVersion(1,8,0))) then
        begin
          Infoln(infotext+'Deleting '+FPCDefines+' to force rescan of FPC sources.', etInfo);
          s1:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+FPCDefines;
          SysUtils.DeleteFile(s1);
        end;
        if (InstallDirectory<>SourceDirectory) then
        begin
          Processor.SetParamData('install');
          Infoln(infotext+'Running: make install', etInfo);
        end
        else
        begin
          Processor.SetParamData('--help'); // this should render make harmless
          WritelnLog(etInfo, infotext+'Skipping install step: Lazarus source and install locations are the same.', true);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
      end;
      _MAKEFILECHECKLAZARUS:
      begin
        Processor.SetParamData('fpc_baseinfo');
        Infoln(infotext+'Running: make fpc_baseinfo', etInfo);
      end
      else //raise error;
      begin
        Processor.SetParamData('--help'); // this should render make harmless
        WritelnLog(etError, infotext+'Invalid module name ' + ModuleName + ' specified! Please fix the code.', true);
        OperationSucceeded := false;
        Result := false;
        exit;
      end;
      if FLCL_Platform<>'' then Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);
    end;

    try
      {$ifdef MSWindows}
      //Prepend FPC binary directory to PATH to prevent pickup of strange tools
      OldPath:=Processor.Environment.GetVar(PATHVARNAME);
      s1:=FPCBinDir;
      if OldPath<>'' then
         Processor.Environment.SetVar(PATHVARNAME, s1+PathSeparator+OldPath)
      else
        Processor.Environment.SetVar(PATHVARNAME, s1);
      {$endif}

      ProcessorResult:=Processor.ExecuteAndWait;
      ExitCode := ProcessorResult;

      if ExitCode <> 0 then
      begin
        WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' returned exit status #'+IntToStr(ExitCode), true);
        OperationSucceeded := false;
        Result := false;
      end;

      {$ifdef MSWindows}
      Processor.Environment.SetVar(PATHVARNAME, OldPath);
      {$endif}

    except
      on E: Exception do
      begin
        WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' exception.'+LineEnding+'Exception details: '+E.Message, true);
        OperationSucceeded := false;
        Result := false;
      end;
    end;

    //Special check for lazbuild as that is known to go wrong
    if (OperationSucceeded) and (ModuleName=_LAZBUILD) then
    begin
      if CheckExecutable(IncludeTrailingPathDelimiter(SourceDirectory) + LAZBUILDNAME + GetExeExt, ['--help'], LAZBUILDNAME) = false then
      begin
        WritelnLog(etError, infotext+'Lazbuild could not be found, so cannot build USERIDE.', true);
        Result := false;
        exit;
      end;
    end;

  end
  else
  begin
    // For building useride for Lazarus versions
    // useride; using lazbuild.
    // Check for valid lazbuild.
    // Note: we don't check if we have a valid primary config path, but that will come out
    // in the next steps.
    //LazBuildApp:=IncludeTrailingPathDelimiter(SourceDirectory)+LAZBUILDNAME+GetExeExt;
    LazBuildApp:=IncludeTrailingPathDelimiter(InstallDirectory)+LAZBUILDNAME+GetExeExt;
    if CheckExecutable(LazBuildApp, ['--help'], LAZBUILDNAME) = false then
    begin
      WritelnLog(etError, infotext+'Lazbuild could not be found, so cannot build USERIDE.', true);
      Result := false;
      exit;
    end
    else
    begin
      // First build IDE using lazbuild... then...
      Processor.Executable := LazBuildApp;
      FErrorLog.Clear;
      Processor.Process.CurrentDirectory := SourceDirectory;
      Processor.Process.Parameters.Clear;
      //SysUtils.GetEnvironmentVariable('FPCDIR');
      //Makefile could pickup this FPCDIR setting, so try to set it for fpcupdeluxe
      FPCDirStore:=Processor.Environment.GetVar('FPCDIR');
      Processor.Environment.SetVar('FPCDIR',FFPCSourceDir);
      //Processor.Environment.SetVar('FPCDIR',FFPCInstallDir);
      //Processor.Environment.SetVar('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true)]));
      {$IFDEF DEBUG}
      Processor.SetParamData('--verbose');
      {$ELSE}
      // See compileroptions.pp
      // Quiet:=ConsoleVerbosity<=-3;
      Processor.SetParamData('--quiet');
      {$ENDIF}

      Processor.SetParamData('--pcp=' + DoubleQuoteIfNeeded(FLazarusPrimaryConfigPath));
      Processor.SetParamData('--cpu=' + GetSourceCPU);
      Processor.SetParamData('--os=' + GetSourceOS);

      if FLCL_Platform <> '' then
        Processor.SetParamData('--ws=' + FLCL_Platform);

      // Support keeping userdefined installed packages when building.
      // Compile with selected compiler options
      // Assume new Laz version on failure getting revision
      if StrToIntDef(ActualRevision, 38971) >= 38971 then
      begin
        Processor.SetParamData('--build-ide="-dKeepInstalledPackages '+FCompilerOptions+'"');
      end
      else
      begin
        // Fallback - depends on hardcoded "Normal IDE" build mode being present
        // We can specify a build mode; otherwise probably the latest build mode will be used
        // which could well be a stripped IDE
        // Let's see how/if CompilerOptions clashes with the settings in normal build mode
        WritelnLog(infotext+'LazBuild: building UserIDE but falling back to --build-mode="Normal IDE"', true);
        Processor.SetParamData('--build-ide="'+FCompilerOptions+'"');
        Processor.SetParamData('--build-mode="Normal IDE"');
      end;

      // Run first time...
      if OperationSucceeded then
      begin
        Infoln(infotext+'Running lazbuild to get IDE with user-specified packages', etInfo);
        try
          ProcessorResult:=Processor.ExecuteAndWait;
          //Restore FPCDIR environment variable ... could be trivial, but batter safe than sorry
          Processor.Environment.SetVar('FPCDIR',FPCDirStore);
          if ProcessorResult <> 0 then
          begin
            WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' returned error code ' + IntToStr(ProcessorResult) + LineEnding +
              'Details: ' + FErrorLog.Text, true);
            OperationSucceeded := false;
          end;
        except
          on E: Exception do
          begin
            OperationSucceeded := false;
            WritelnLog(etError, infotext+'Exception running '+ExtractFileName(Processor.Executable)+' to get IDE with user-specified packages!' + LineEnding +
              'Details: ' + E.Message, true);
          end;
        end;
      end;

      (*
      // ... build startlazarus if it doesn't exist
      // (even an old version left over by make distclean is probably ok)
      if OperationSucceeded then
      begin
        if FileExists(IncludeTrailingPathDelimiter(InstallDirectory) + 'startlazarus' + GetExeExt) then
        begin
          Infoln(infotext+'Startlazarus exists already. Not compiling again.', etdebug);
        end
        else
        begin
          Processor.Executable := LazBuildApp;
          FErrorLog.Clear;
          Processor.Process.CurrentDirectory := SourceDirectory;
          Processor.Process.Parameters.Clear;
          //Makefile could pickup this FPCDIR setting, so try to set it for fpcupdeluxe
          FPCDirStore:=Processor.Environment.GetVar('FPCDIR');
          Processor.Environment.SetVar('FPCDIR',FFPCSourceDir);
          //Processor.Environment.SetVar('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true)]));
          {$IFDEF DEBUG}
          Processor.SetParamData('--verbose');
          {$ELSE}
          Processor.SetParamData('--quiet');
          {$ENDIF}

          Processor.SetParamData('--pcp=' + DoubleQuoteIfNeeded(FLazarusPrimaryConfigPath));
          Processor.SetParamData('--cpu=' + GetSourceCPU);
          Processor.SetParamData('--os=' + GetSourceOS);

          if FLCL_Platform <> '' then
            Processor.SetParamData('--ws=' + FLCL_Platform);

          Processor.SetParamData(DoubleQuoteIfNeeded(IncludeTrailingPathDelimiter(SourceDirectory)+
            'ide'+DirectorySeparator+'startlazarus.lpi'));

          Infoln(infotext+'Compiling startlazarus to make sure it is present:', etInfo);
          try
            ProcessorResult:=Processor.ExecuteAndWait;
            //Restore FPCDIR environment variable ... could be trivial, but batter safe than sorry
            Processor.Environment.SetVar('FPCDIR',FPCDirStore);
            if ProcessorResult <> 0 then
            begin
              Writelnlog(etError, infotext+'Lazbuild startlazarus returned error code ' + IntToStr(ProcessorResult) + LineEnding +
                'Details: ' + FErrorLog.Text, true);
              OperationSucceeded := false;
            end;
          except
            on E: Exception do
            begin
              OperationSucceeded := false;
              WritelnLog(etError, infotext+'Exception running lazbuild to get startlazarus!' + LineEnding +
                'Details: ' + E.Message, true);
            end;
          end;
        end;
      end;
      *)

    end;
  end;

  if (ModuleName=_MAKEFILECHECKLAZARUS) then exit;

  if OperationSucceeded then
  begin

    if (ModuleName=_STARTLAZARUS) then
    begin
      //Make new symlinks !!
      {$ifdef Darwin}
      s1:=ConcatPaths([InstallDirectory,'startlazarus']);
      if FileExists(s1) then
      begin
        s2:=ConcatPaths([InstallDirectory,'startlazarus.app','Contents','MacOS','startlazarus']);
        SysUtils.DeleteFile(s2);
        fpSymlink(PChar('./../../../startlazarus'),PChar(s2));
      end;
      {$endif}
    end;

    if (ModuleName=_USERIDE) OR (ModuleName=_LAZARUS) then
    begin
      //Make new symlinks !!
      {$ifdef Darwin}
      s1:=ConcatPaths([InstallDirectory,'lazarus']);
      if FileExists(s1) then
      begin
        s2:=ConcatPaths([InstallDirectory,'lazarus.app','Contents','MacOS','lazarus']);
        SysUtils.DeleteFile(s2);
        fpSymlink(PChar('./../../../lazarus'),PChar(s2));
      end;
      s1:=ConcatPaths([InstallDirectory,'startlazarus']);
      if FileExists(s1) then
      begin
        s2:=ConcatPaths([InstallDirectory,'startlazarus.app','Contents','MacOS','startlazarus']);
        SysUtils.DeleteFile(s2);
        fpSymlink(PChar('./../../../startlazarus'),PChar(s2));
      end;
      {$endif}
    end;
  end;

  Result := OperationSucceeded;
end;

constructor TLazarusNativeInstaller.Create;
begin
  inherited Create;
end;

destructor TLazarusNativeInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusInstaller }

function TLazarusInstaller.GetVersionFromSource:string;
const
  VERSIONMAGIC='LazarusVersionStr';
  VERSIONMAGIC2='laz_version';
var
  s,aFileName:string;
  TxtFile:Text;
  x,y:integer;
begin
  result:='0.0.0';

  if result='0.0.0' then
  begin
    aFileName:=ConcatPaths([SourceDirectory,'ide'])+DirectorySeparator+'version.inc';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);
      Readln(TxtFile,s);
      // remove quotes from string
      //VersionSnippet:=DelChars(s, '''');
      s:=TrimSet(s, [#39]);
      s:=Trim(s);
      //x:=Length(s);
      //while (x>0) AND (NOT (s[x] in ['0'..'9','.'])) do Dec(x);
      //if (x<Length(s)) then Delete(S,x,MaxInt);
      if Length(s)>0 then result:=s;
      CloseFile(TxtFile);
    end;
  end;

  if result='0.0.0' then
  begin
    aFileName:=ConcatPaths([SourceDirectory,'ide','packages','ideconfig'])+DirectorySeparator+'version.inc';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);
      Readln(TxtFile,s);
      // remove quotes from string
      //VersionSnippet:=DelChars(s, '''');
      s:=TrimSet(s, [#39]);
      s:=Trim(s);
      //x:=Length(s);
      //while (x>0) AND (NOT (s[x] in ['0'..'9','.'])) do Dec(x);
      //if (x<Length(s)) then Delete(S,x,MaxInt);
      if Length(s)>0 then result:=s;
      CloseFile(TxtFile);
    end;
  end;

  if result='0.0.0' then
  begin
    aFileName:=IncludeTrailingPathDelimiter(SourceDirectory) + 'ide' + DirectorySeparator + 'aboutfrm.pas';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);

      while NOT EOF (TxtFile) do
      begin
        Readln(TxtFile,s);
        x:=Pos(VERSIONMAGIC,s);
        if x>0 then
        begin
          x:=x+Length(VERSIONMAGIC);
          while (Length(s)>=x) AND (s[x]<>'''') do Inc(x);
          y:=x+1;
          while (Length(s)>=y) AND (s[y]<>'''') do Inc(y);
          if (y>(x+1)) then
          begin
            s:=Copy(s,x,(y-x+1));
            s:=TrimSet(s, [#39]);
            if Length(s)>0 then result:=s;
          end;
          break;
        end;
      end;
      CloseFile(TxtFile);
    end;
  end;

  if result='0.0.0' then
  begin
    aFileName:=IncludeTrailingPathDelimiter(SourceDirectory) + 'components' + DirectorySeparator + 'lazutils' + DirectorySeparator  + 'lazversion.pas';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);

      while NOT EOF (TxtFile) do
      begin
        Readln(TxtFile,s);
        x:=Pos(VERSIONMAGIC2,s);
        if x>0 then
        begin
          x:=x+Length(VERSIONMAGIC2);
          while (Length(s)>=x) AND (s[x]<>'''') do Inc(x);
          y:=x+1;
          while (Length(s)>=y) AND (s[y]<>'''') do Inc(y);
          if (y>(x+1)) then
          begin
            s:=Copy(s,x,(y-x+1));
            s:=TrimSet(s, [#39]);
            if Length(s)>0 then result:=s;
          end;
          break;
        end;
      end;
      CloseFile(TxtFile);
    end;
  end;

end;

function TLazarusInstaller.GetReleaseCandidateFromSource:integer;
const
  VERSIONMAGIC='LazarusVersionStr';
  //VERSIONMAGIC2='laz_patch';
var
  s,aFileName:string;
  TxtFile:Text;
  x,y:integer;
begin
  result:=-1;

  if result=-1 then
  begin
    aFileName:=IncludeTrailingPathDelimiter(SourceDirectory) + 'ide' + DirectorySeparator + 'version.inc';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);
      Readln(TxtFile,s);
      // remove quotes from string
      //VersionSnippet:=DelChars(s, '''');
      s:=TrimSet(s, [#39]);
      s:=Trim(s);
      if Length(s)>0 then
      begin
        x:=Pos('RC',s);
        if (x>0) then
        begin
          s:=Copy(s,(x+2),MaxInt);
          result:=StrToIntDef(s,-1);
        end;
      end;
      CloseFile(TxtFile);
    end;
  end;

  if result=-1 then
  begin
    aFileName:=ConcatPaths([SourceDirectory,'ide','packages','ideconfig'])+DirectorySeparator+'version.inc';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);
      Readln(TxtFile,s);
      // remove quotes from string
      //VersionSnippet:=DelChars(s, '''');
      s:=TrimSet(s, [#39]);
      s:=Trim(s);
      if Length(s)>0 then
      begin
        x:=Pos('RC',s);
        if (x>0) then
        begin
          s:=Copy(s,(x+2),MaxInt);
          result:=StrToIntDef(s,-1);
        end;
      end;
      CloseFile(TxtFile);
    end;
  end;

  if result=-1 then
  begin
    aFileName:=IncludeTrailingPathDelimiter(SourceDirectory) + 'ide' + DirectorySeparator + 'aboutfrm.pas';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);

      while NOT EOF (TxtFile) do
      begin
        Readln(TxtFile,s);
        x:=Pos(VERSIONMAGIC,s);
        if x>0 then
        begin
          x:=x+Length(VERSIONMAGIC);
          while (Length(s)>=x) AND (s[x]<>'''') do Inc(x);
          y:=x+1;
          while (Length(s)>=y) AND (s[y]<>'''') do Inc(y);
          if (y>(x+1)) then
          begin
            s:=Copy(s,x,(y-x+1));
            s:=TrimSet(s, [#39]);
            if Length(s)>0 then
            begin
              x:=Pos('RC',s);
              if x>0 then
              begin
                s:=Copy(s,x,MaxInt);
                result:=StrToIntDef(s,-1);
              end;
            end;
          end;
          break;
        end;
      end;
      CloseFile(TxtFile);
    end;
  end;

  {
  if result=-1 then
  begin
    aFileName:=IncludeTrailingPathDelimiter(aSourcePath) + 'components' + DirectorySeparator + 'lazutils' + DirectorySeparator  + 'lazversion.pas';
    if FileExists(aFileName) then
    begin
      AssignFile(TxtFile,aFileName);
      Reset(TxtFile);

      while NOT EOF (TxtFile) do
      begin
        Readln(TxtFile,s);
        x:=Pos(VERSIONMAGIC2,s);
        if x>0 then
        begin
          x:=x+Length(VERSIONMAGIC2);
          // move towards first numerical
          while (Length(s)>=x) AND (NOT (s[x] in ['0'..'9'])) do Inc(x);
          // get RC version
          y:=0;
          while (Length(s)>=x) AND (s[x] in ['0'..'9']) do
          begin
            y:=y*10+Ord(s[x])-$30;
            Inc(x);
          end;
          // a valid or usefull RC is always >0
          if y>0 then result:=y;
          break;
        end;
      end;
      CloseFile(TxtFile);
    end;
  end;
  }

end;

function TLazarusInstaller.GetVersionFromUrl(aUrl:string):string;
var
  aVersion: string;
begin
  aVersion:=VersionFromUrl(aUrl);
  if aVersion='trunk' then
    result:=LAZARUSTRUNKVERSION
  else
    result:=aVersion;
end;

function TLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  Result := true;
  infotext:=InitInfoText(' (BuildModuleCustom: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);
end;

function TLazarusInstaller.GetLazarusVersion: string;
var
  aFileName:string;
begin
  result:='0.0.0';

  aFileName:=IncludeTrailingPathDelimiter(InstallDirectory) + LAZBUILDNAME + GetExeExt;
  if FileExists(aFileName) then
  begin
    Processor.Executable := aFileName;
    Processor.Process.CurrentDirectory := SourceDirectory;
    Processor.Process.Parameters.Clear;
    Processor.SetParamData('--version');
    try
      ProcessorResult:=Processor.ExecuteAndWait;
      if ProcessorResult = 0 then
      begin
        if Processor.WorkerOutput.Count>0 then
        begin
          // lazbuild outputs version info as last line
          result:=Processor.WorkerOutput.Strings[Processor.WorkerOutput.Count-1];
        end;
      end;
    except
      on E: Exception do
      begin
        WritelnLog(etError, infotext+'Getting lazbuild version info failed with an exception!'+LineEnding+'Details: '+E.Message,true);
      end;
    end;
  end;

  if result='0.0.0' then result:=GetVersionFromSource;
  if result='0.0.0' then result:=GetVersionFromUrl(URL);
end;

function TLazarusInstaller.IsCross:boolean;
begin
  result:=(Self is TLazarusCrossInstaller);
end;

function TLazarusInstaller.InitModule: boolean;
var
  PlainBinDir: string; //the directory above e.g. c:\development\fpc\bin\i386-win32
  {$IFDEF MSWINDOWS}
  aPath,s:string;
  {$ENDIF}
begin
  Result := true;

  if InitDone then exit;

  localinfotext:=InitInfoText(' (InitModule): ');

  PlainBinDir := SafeExpandFileName(FPCBinDir+DirectorySeparator+'..'+DirectorySeparator+'..');

  Infoln(localinfotext+'Entering ...',etDebug);

  WritelnLog(localinfotext+'Lazarus directory:      ' + SourceDirectory, false);
  WritelnLog(localinfotext+'Lazarus URL:            ' + URL, false);
  WritelnLog(localinfotext+'Lazarus options:        ' + FCompilerOptions, false);
  result:=(CheckAndGetTools) AND (CheckAndGetNeededBinUtils);

  if result then
  begin
    if Assigned(CrossInstaller) then
    begin
      CrossInstaller.SolarisOI:=FSolarisOI;
      CrossInstaller.MUSL:=FMUSL;
      CrossInstaller.LinuxLegacy:=FLinuxLegacy;
    end;
  end;

  if result then
  begin
    {$IFDEF MSWINDOWS}
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
    // at least one ; to be present in the path. If you only have one entry, you
    // can add PathSeparator without problems.
    // https://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
    aPath:='';
    if Assigned(SVNClient) AND SVNClient.ValidClient then
    begin
      s:=SVNClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      s:=ExtractFileDir(s);
      // Only add path if there is no stray shell (sh.exe) laying around in this path
      if (NOT FileExists(s+DirectorySeparator+'sh'+GetExeExt)) then
        aPath:=aPath+PathSeparator+s;
    end;
    if Assigned(GITClient) AND GITClient.ValidClient then
    begin
      s:=GITClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      s:=ExtractFileDir(s);
      // Only add path if there is no stray shell (sh.exe) laying around in this path
      if (NOT FileExists(s+DirectorySeparator+'sh'+GetExeExt)) then
        aPath:=aPath+PathSeparator+s;
    end;
    if Assigned(HGClient) AND HGClient.ValidClient then
    begin
      s:=HGClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      s:=ExtractFileDir(s);
      // Only add path if there is no stray shell (sh.exe) laying around in this path
      if (NOT FileExists(s+DirectorySeparator+'sh'+GetExeExt)) then
        aPath:=aPath+PathSeparator+s;
    end;
    SetPath(
      FPCBinDir + PathSeparator +
      PlainBinDir + PathSeparator +
      FMakeDir + PathSeparator +
      InstallDirectory+
      aPath,
      false, false);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    SetPath(FPCBinDir+PathSeparator+
    {$IFDEF DARWIN}
    // pwd is located in /bin ... the makefile needs it !!
    // tools are located in /usr/bin ... the makefile needs it !!
    // don't ask, but this is needed when fpcupdeluxe runs out of an .app package ... quirk solved this way .. ;-)
    '/bin'+PathSeparator+'/usr/bin'+PathSeparator+
    {$ENDIF}
    PlainBinDir, true, false);
    {$ENDIF UNIX}
  end;

  GetVersion;
  InitDone := Result;
end;

function TLazarusInstaller.BuildModule(ModuleName: string): boolean;
var
  s,s2,VersionSnippet:string;
begin
  Result := inherited;
  Result := InitModule;
  if not Result then exit;

  s:=IncludeTrailingPathDelimiter(SourceDirectory) + MAKEFILENAME;
  if (NOT FileExists(s)) then
  begin
    Infoln(infotext+s+' not found. Severe error. Should not happen. Aborting build '+ModuleName+'.',etError);
    exit(false);
  end;

  VersionSnippet:=SourceVersionStr;
  if (VersionSnippet<>'0.0.0') then
  begin
    // only report once
    if (ModuleName=_LAZBUILD) OR (ModuleName=_LAZARUS) OR (IsCross AND (ModuleName=_LCL)) then
    begin
      if (IsCross) then
      begin
        s:='Lazarus '+TLazarusCrossInstaller(Self).CrossInstaller.RegisterName+' cross-builder: ';
      end
      else
      begin
        s:='Lazarus native builder: ';
      end;
      Infoln(s+'Detected source version Lazarus: '+VersionSnippet, etInfo);
      s2:=CompilerVersion(FCompiler);
      Infoln(s+'Using FPC compiler with version: '+s2, etInfo);
    end;
  end;

  Result := BuildModuleCustom(ModuleName);
end;

function TLazarusInstaller.ConfigModule(ModuleName: string): boolean;
{$IFDEF DARWIN}
const
 CONFIGRENAMEMAGIC='ConfigRenameNeeded';
{$ENDIF DARWIN}
var
  GDBPath         : string;
  {$IFDEF DARWIN}
  LLDBPath       : string;
  RenameNeeded   : boolean;
  {$ENDIF DARWIN}
  LazarusConfig  : TUpdateLazConfig;
  ConfigSwitch   : boolean;
  PCPSnippet     : TStringList;
  aFileName      : string;
  s,s2           : string;
  j              : integer;
begin
  Result := inherited;
  Result := true;

  s:=GetVersion;
  if ((Length(s)=0) OR (s='0.0.0')) then exit;

  if DirectoryExists(FLazarusPrimaryConfigPath) = false then
  begin
    if ForceDirectoriesSafe(FLazarusPrimaryConfigPath) then
      Infoln(infotext+'Created Lazarus primary config directory: ' + FLazarusPrimaryConfigPath, etInfo);
  end;

  ForceDirectoriesSafe(InstallDirectory);

  // Lazarus 1.2RC1+ and higher support specifying the primary-config-path that should be used
  // inside the lazarus directory itself.
  PCPSnippet := TStringList.Create;
  {$IF FPC_FULLVERSION > 30100}
  //PCPSnippet.DefaultEncoding:=TEncoding.ASCII;
  {$ENDIF}
  try
    PCPSnippet.Add('--primary-config-path=' + FLazarusPrimaryConfigPath);
    aFileName:=IncludeTrailingPathDelimiter(InstallDirectory) + LAZARUSCFG;
    if (NOT FileExists(aFileName)) then PCPSnippet.SaveToFile(aFileName);
  finally
    PCPSnippet.Free;
  end;

  {$IFDEF DARWIN}
  RenameNeeded:=False;
  {$ENDIF DARWIN}

  // Set up a minimal config so we can use LazBuild
  LazarusConfig := TUpdateLazConfig.Create(FLazarusPrimaryConfigPath, FMajorVersion, FMinorVersion, FReleaseVersion, FPatchVersion);
  try
    try
      // Force English language
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Language/ID', 'en');
      // Set Lazarus directory
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/LazarusDirectory/Value', InstallDirectory);

      // On Unix, FInstalledCompiler should be set to our fpc.sh proxy if installed
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/CompilerFilename/Value', FCompiler);

      if (LazarusConfig.IfNewFile(EnvironmentConfig)) then
      begin
        if Ultibo then
        begin
          s:=LazarusConfig.GetVariable(EnvironmentConfig, 'EnvironmentOptions/Version/Lazarus');
          s:=s+'U';
          LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Version/Lazarus',s);
        end;

        {$IFDEF MSWINDOWS}
        // On Windows, we provide our own GDB
        GDBPath:=ConcatPaths([FMakeDir,'gdb',GetSourceCPUOS])+DirectorySeparator+'gdb.exe';
        if FileExists(GDBPath) then
        begin
          if (SourceVersionNum>=CalculateFullVersion(0,9,31)) then
            GDBPath:=ConcatPaths([FMakeDir,'gdb','$(TargetCPU)-$(TargetOS)'])+DirectorySeparator+'gdb.exe'
        end
        else
          GDBPath:='';
        {$ELSE}
        {$IF (defined(FREEBSD)) or (defined(Darwin))}
        // Check for newer user-installed debugger (e.g. from ports tree
        // The system gdb is ancient (gdb 6.1.1 in FreeBSD 9) and does not work well with Laz
        GDBPath := '/usr/local/bin/gdb';
        if (NOT FileExists(GDBPath)) OR (NOT CheckExecutable(GDBPath, ['--version'], 'GNU gdb')) then GDBPath := '/usr/libexec/gdb';
        if (NOT FileExists(GDBPath)) OR (NOT CheckExecutable(GDBPath, ['--version'], 'GNU gdb')) then GDBPath := which('gdb');
        {$ELSE}//other *nix
        GDBPath := which('gdb');  //assume in path
        {$ENDIF}
        if FileExists(GDBPath) then
        begin
          s:=ConcatPaths([FMakeDir,'gdb',GetSourceCPUOS]);
          ForceDirectoriesSafe(s);
          s:=s+DirectorySeparator+'gdb';
          if (fpSymlink(pchar(GDBPath),pchar(s))=0) then
          begin
            s:=ConcatPaths([FMakeDir,'gdb','$(TargetCPU)-$(TargetOS)']);
            s:=s+DirectorySeparator+'gdb';
          end;
          GDBPath:=s;
        end else GDBPath:='';
        {$ENDIF MSWINDOWS}

        if (Length(GDBPath)>0) then
        begin
          if (SourceVersionNum<CalculateFullVersion(2,1,0)) then
          begin
            LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Class', 'TGDBMIDebugger');
          end
          else
          begin
            ConfigSwitch:=LazarusConfig.IsLegacyList(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/');
            s:='EnvironmentOptions/Debugger/Configs/'+LazarusConfig.GetListItemXPath(EnvironmentConfig,'Config',0,ConfigSwitch,True)+'/';
            LazarusConfig.SetVariable(EnvironmentConfig, s+'ConfigName', 'Standard GDB');
            LazarusConfig.SetVariable(EnvironmentConfig, s+'ConfigClass', 'TGDBMIDebugger');
            LazarusConfig.SetVariable(EnvironmentConfig, s+'DebuggerFilename',GDBPath);
            LazarusConfig.SetVariable(EnvironmentConfig, s+'Active',True);
            {$IFDEF DARWIN}
            //Available in latest trunk: extra gdb settings
            LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/DisableStartupShell', 'True');
            LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/WarnOnTimeOut', 'False');
            LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/Debugger_Startup_Options', '--eval-command="set startup-with-shell off"');
            {$ENDIF DARWIN}
          end;
          LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value',GDBPath);
          LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/TGDBMIDebugger/History/Count',1);
          LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/TGDBMIDebugger/History/Item1/Value',GDBPath);
        end;

        {$IFDEF DARWIN}
        Infoln(infotext+'Looking for LLDB debugger for Lazarus.', etInfo);
        LLDBPath:='/Library/Developer/CommandLineTools/usr/bin/lldb';
        if NOT FileExists(LLDBPath) then LLDBPath:='/usr/bin/lldb';
        if NOT FileExists(LLDBPath) then LLDBPath:=which('lldb'); // assume in path

        if FileExists(LLDBPath) then
        begin
          if (SourceVersionNum<CalculateFullVersion(2,1,0)) then
          begin
            LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Class', 'TLldbDebugger');
          end
          else
          begin
            if LazarusConfig.GetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/Config/Active',false) then
            begin
              // We have already GDB
              // Make LLDB the preferred debugger and prepare for GDB as second debugger
              // Disable gdb as primary debugger
              LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/Config/Active',False);
              LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/TGDBMIDebugger/History/Count',2);
              LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/TGDBMIDebugger/History/Item2/Value',GDBPath);
              // Perpare for dirty trick
              s:=CONFIGRENAMEMAGIC;
              RenameNeeded:=True;
            end
            else
            begin
              s:='Config';
            end;
            LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/'+s+'/ConfigName', 'Standard LLDB');
            LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/'+s+'/ConfigClass', 'TLldbDebugger');
            LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/'+s+'/DebuggerFilename',LLDBPath);
            LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Configs/'+s+'/Active',True);
          end;

          LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value',LLDBPath);
          if NOT RenameNeeded then LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/TLldbDebugger/History/Count',1);
          LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/TLldbDebugger/History/Item1/Value',LLDBPath);
        end;
        {$ENDIF DARWIN}
      end;

      {$IFDEF MSWINDOWS}
      s:=ExtractFilePath(FCompiler)+'make.exe';
      if FileExists(s)
         then LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', s)
         else LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', Make);
      {$ELSE}
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', Make);
      {$ENDIF MSWINDOWS}

      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/FPCSourceDirectory/Value', FFPCSourceDir);
      // Add <lazarus>\docs\xml to fpdoc editor paths
      LazDocPathAdd(IncludeTrailingPathDelimiter(InstallDirectory) + 'docs'+DirectorySeparator+'xml', LazarusConfig);

      // Enable IDE Coolbar for default docked desktop for (NewPascal) Lazarus with docking
      if LazarusConfig.GetVariable(EnvironmentConfig,'Desktops/Desktop2/Name')='default docked' then
         LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop2/IDECoolBarOptions/Visible/Value', 'True');

      {$IFDEF MSWINDOWS}
      // needed while running Lazarus adds a personal directory that is not valid for other users.
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Count', '2');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item1/Value', 'C:\Windows\Temp\');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item2/Value', 'C:\Users\Public\Documents');
      {$ENDIF MSWINDOWS}

      // Set message filter to none to be able to detect linking errors.
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'MsgView/Filters/Filter1/MinUrgency', 'None');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'MsgView/Filters/Filter1/FilterNotesWithoutPos', 'False');

      // add default projects path
      GDBPath := IncludeTrailingPathDelimiter(BaseDirectory) + 'projects';
      ForceDirectoriesSafe(GDBPath);
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/Value', IncludeTrailingPathDelimiter(GDBPath));

      {$IFDEF UNIX}
      {$IFNDEF DARWIN}
      {$IFDEF LCLQT5}
      if (NOT LibWhich(LIBQT5)) then
      begin
        s:=IncludeTrailingPathDelimiter(SafeGetApplicationPath)+LIBQT5;
        if FileExists(s) then
        begin
          // Strange: running needs a .so.1 file .... but linking needs a .so file ...
          s2:=IncludeTrailingPathDelimiter(GDBPath)+LIBQT5;
          if (NOT FileExists(s2)) then FileCopy(s,s2);
          s2:=IncludeTrailingPathDelimiter(GDBPath)+LIBQT5VERSION;
          if (NOT FileExists(s2)) then FileCopy(s,s2);
        end;
      end;
      {$ENDIF LCLQT5}
      {$ENDIF DARWIN}
      {$ENDIF UNIX}

      // Set file history towards default project directory
      LazarusConfig.SetVariableIfNewFile(History, 'InputHistory/FileDialog/InitialDir', IncludeTrailingPathDelimiter(GDBPath));

      //Setup basic fppkg things
      s2 := IncludeTrailingPathDelimiter(BaseDirectory)+PACKAGESCONFIGDIR;
      s  := IncludeTrailingPathDelimiter(s2)+FPCPKGCONFIGFILENAME;
      if (LazarusConfig.IfNewFile(EnvironmentConfig)) then
      begin
        if FileExists(s) then LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/FppkgConfigFile/Value', s);
      end;

      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Count', '2');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop1/Name', 'default');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop2/Name', 'default docked');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop2/DockMaster', 'TIDEAnchorDockMaster');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/ActiveDesktop', 'default');

      {$ifdef LCLQT5}
      //Set default sizes and position
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Left', '10');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Top', '30');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Width', '900');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Height', '60');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/Visible/Value', 'True');
      {$endif}

      {$ifdef Haiku}
      //Set default font
      LazarusConfig.SetVariableIfNewFile(EditorConfig, 'EditorOptions/Display/DoNotWarnForFont', 'Noto Mono');
      LazarusConfig.SetVariableIfNewFile(EditorConfig, 'EditorOptions/Display/EditorFont', 'Noto Mono');
      LazarusConfig.SetVariableIfNewFile(EditorConfig, 'EditorOptions/Display/EditorFontSize', '8');
      {$endif}

      // set default positions of object, source and message windows
      {$ifdef Darwin}
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Left', '10');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Top', '120');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Width', '230');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Height', '560');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/Visible/Value', 'True');

      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Left', '250');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Top', '120');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Width', '600');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Height', '440');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/Visible/Value', 'True');

      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Left', '250');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Top', '600');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Width', '600');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Height', '100');
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/Visible/Value', 'True');
      {$else}
      LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/Visible/Value', 'True');
      {$endif}

      for ConfigSwitch in boolean do
      begin
        if (NOT ConfigSwitch) then s:='Desktop1';
        if (ConfigSwitch) then s:='Desktop2';

        j:=LazarusConfig.GetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/Count',0);
        if (j=0) then
        begin
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/Count', 2);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/Width/Value', 14*19);

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Version', 1);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Count', 14);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button1/Name', 'NewUnit');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button2/Name', 'NewForm');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button3/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button4/Name', 'Open');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button5/Name', 'Save');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button6/Name', 'SaveAll');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button7/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button8/Name', 'Toggle between Unit and Form');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button9/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button10/Name', 'Find in files');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button11/Name', 'General environment options');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button12/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button13/Name', 'Project Inspector');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button14/Name', 'View project options');

          {$ifdef Darwin}
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/Width/Value', 15*19);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Count', 15);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar1/Button15/Name', 'View project source');
          {$endif}

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Version', 1);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Count', 11);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Break/Value', True);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button1/Name', 'View Units');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button2/Name', 'View Forms');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button3/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button4/Name', 'Change build mode');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button5/Name', 'Run without debugging');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button6/Name', 'Run program');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button7/Name', 'Pause program');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button8/Name', 'Stop program');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button9/Name', 'Step over');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button10/Name', 'Step into');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button11/Name', 'Step out');

          {$ifdef Darwin}
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Count', 13);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button12/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/'+s+'/IDECoolBarOptions/ToolBar2/Button13/Name', 'Clean up and build');
          {$endif}

        end;
      end;

      // set defaults for pas2js
      LazarusConfig.SetVariableIfNewFile(Pas2jsConfig, 'compiler/value', ExtractFilePath(FCompiler)+'pas2js'+GetExeExt);
      LazarusConfig.SetVariableIfNewFile(Pas2jsConfig, 'webserver/value',  ExtractFilePath(FCompiler)+'compileserver'+GetExeExt);
      //LazarusConfig.SetVariableIfNewFile(Pas2jsConfig, 'webserver/startatport/value', '3000');

    except
      on E: Exception do
      begin
        Result := false;
        Infoln('Error setting Lazarus config: ' + E.ClassName + '/' + E.Message, eterror);
      end;
    end;
  finally
    LazarusConfig.Free;
  end;

  {$IFDEF DARWIN}
  if RenameNeeded then
  begin
    // Rename second degugger node .... very tricky ... :-(
    PCPSnippet:=TStringList.Create;
    try
      PCPSnippet.LoadFromFile(IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+EnvironmentConfig);
      s:=PCPSnippet.Text;
      PCPSnippet.Text:=StringReplace(s,CONFIGRENAMEMAGIC,'Config',[]);
      PCPSnippet.SaveToFile(IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+EnvironmentConfig);
    finally
      PCPSnippet.Free;
    end;
  end;
  {$ENDIF DARWIN}

end;

function TLazarusInstaller.CleanModule(ModuleName: string): boolean;
var
  {$ifdef MSWINDOWS}
  CrossWin: boolean;
  LHelpTemp: string; // LHelp gets copied to this temp file
  {$endif}
  CleanCommand,CleanDirectory:string;
  CrossCompiling: boolean;
  RunTwice: boolean;
  s:string;
  {
  DeleteList: TStringList;
  CPUOS_Signature:string;
  }
begin
  Result := inherited;

  // if no sources, then exit;
  if result then exit;

  Result := InitModule;

  if not Result then exit;

  CrossCompiling:=(IsCross AND Assigned(CrossInstaller));

  if (ModuleName=_LAZARUS) then
  begin
    if CrossCompiling then
    begin
      CrossInstaller.SetFPCVersion(CompilerVersion(FCompiler));
      CrossInstaller.SetCrossOpt(CrossOPT);
      CrossInstaller.SetSubArch(CrossOS_SubArch);
      CrossInstaller.SetABI(CrossOS_ABI);
    end
    else
    begin
      //Infoln(infotext+'If your primary config path has changed, you may want to remove ' + IncludeTrailingPathDelimiter(
      //  InstallDirectory) + 'lazarus.cfg which points to the primary config path.', etInfo);
      Infoln(infotext+'Deleting Lazarus primary config file ('+LAZARUSCFG+').', etInfo);
      DeleteFile(IncludeTrailingPathDelimiter(InstallDirectory) + LAZARUSCFG);
    end;
  end;

  {$ifdef MSWINDOWS}
  // If doing crosswin32-64 or crosswin64-32, make distclean will not only clean the LCL
  // but also existing lhelp.exe if present. Temporarily copy that so we can restore it later.
  // failure here does not influence result
  LHelpTemp:='';
  CrossWin:=false;

  if CrossCompiling then
  begin
    {$ifdef win32}
    if (CrossInstaller.TargetCPU=TCPU.x86_64) and ((CrossInstaller.TargetOS=TOS.win64) or (CrossInstaller.TargetOS=TOS.win32)) then
      CrossWin := true;
    {$endif win32}
    {$ifdef win64}
    // if this is crosswin64-32, ignore error as it is optional
    if (CrossInstaller.TargetCPU=TCPU.i386) and (CrossInstaller.TargetOS=TOS.win32) then
      CrossWin := true;
    {$endif win64}
    if CrossWin then
    begin
      LHelpTemp:=GetTempFileNameExt('','');
      try
        FileCopy(
          IncludeTrailingPathDelimiter(InstallDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
          LHelpTemp,[cffOverWriteFile]);
      except
        Infoln(infotext+'Non-fatal error copying lhelp to temp file '+LHelpTemp,etInfo);
      end;
    end;
  end;
  {$endif MSWINDOWS}

  for RunTwice in boolean do
  begin
    // Make distclean; we don't care about failure (e.g. directory might be empty etc)
    Processor.Executable := Make;
    Processor.Process.Parameters.Clear;
    {$IFDEF MSWINDOWS}
    if Length(Shell)>0 then Processor.SetParamData('SHELL='+Shell);
    {$ENDIF}
    Processor.Process.CurrentDirectory := SourceDirectory;

    {$IF DEFINED(CPUARM) AND DEFINED(LINUX)}
    Processor.SetParamData('--jobs=1');
    {$ELSE}
    //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
    //if (NOT FNoJobs) then
    //  Processor.SetParamData('--jobs='+IntToStr(FCPUCount));
    {$ENDIF}

    Processor.SetParamMakefilePathData('FPC',FCompiler);
    Processor.SetParamMakefilePathData('PP',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));

    Processor.SetParamMakefilePathData('PREFIX',InstallDirectory);
    Processor.SetParamMakefilePathData('INSTALL_PREFIX',InstallDirectory);
    Processor.SetParamMakefilePathData('LAZARUS_INSTALL_DIR',IncludeTrailingPathDelimiter(InstallDirectory));

    {$ifdef Windows}
    Processor.SetParamData('UPXPROG=echo');      //Don't use UPX
    {$else}
    //Processor.SetParamMakefilePathData('INSTALL_BINDIR',FBinPath);
    {$endif}

    Processor.SetParamData('OS_SOURCE=' + GetSourceOS);
    Processor.SetParamData('CPU_SOURCE=' + GetSourceCPU);

    if (CrossCompiling) then
    begin
      Processor.SetParamData('OS_TARGET=' + CrossInstaller.TargetOSName);
      Processor.SetParamData('CPU_TARGET=' + CrossInstaller.TargetCPUName);
      if (CrossInstaller.SubArch<>TSubarch.saNone) then Processor.SetParamData('SUBARCH='+CrossInstaller.SubArchName);
    end
    else
    begin
      Processor.SetParamData('OS_TARGET=' + GetSourceOS);
      Processor.SetParamData('CPU_TARGET=' + GetSourceCPU);
    end;

    CleanDirectory:='';
    CleanCommand:='';

    case ModuleName of
      _LAZBUILD:
      begin
        Processor.SetParamData('LCL_PLATFORM=nogui');
        CleanCommand:='clean';
        CleanDirectory:='ide';
      end;
      _IDE:
      begin
        CleanCommand:='cleanide';
        //CleanCommand:='distclean';
        CleanDirectory:='ide';
      end;
      _BIGIDE: CleanCommand:='cleanbigide';
      _LAZARUS: CleanCommand:='distclean';
      _LCL:
      begin
        CleanDirectory:='lcl';
        if (CrossCompiling) AND (FLCL_Platform <> '') then
        begin
          Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);
          CleanCommand:='cleanintf';
        end
        else
        begin
          CleanCommand:='clean';
        end;
      end;
      _COMPONENTS:
      begin
        CleanDirectory:='components';
        if (IsCross) AND (FLCL_Platform <> '') then
        begin
          Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);
        end;
        CleanCommand:='clean';
      end;
      _PACKAGER:
      begin
        CleanDirectory:='packager';
        if (IsCross) AND (FLCL_Platform <> '') then
        begin
          Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);
        end;
        CleanCommand:='clean';
      end;
      _LCLCROSS:
      begin
        CleanDirectory:='lcl';
        if (LCLCrossActionNeeded) then
        begin
          Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);
          CleanCommand:='cleanintf';
        end
        else
        begin
          Infoln(infotext+'No extra LCL_PLATFORM defined ... nothing to be done', etDebug);
          Result := true;
          exit;
        end;
      end;
      else //raise error;
      begin
        WritelnLog(etError, infotext+'Invalid module name [' + ModuleName + '] specified! Please fix the code.', true);
      end;
    end;

    if Length(CleanDirectory)>0 then
      CleanDirectory:=ConcatPaths([SourceDirectory,CleanDirectory])
    else
      CleanDirectory:=SourceDirectory;
    Processor.SetParamData('--directory=' + CleanDirectory);

    Processor.SetParamData(CleanCommand);

    if (NOT RunTwice) then
    begin
      if (IsCross) then
        Infoln(infotext+'Running "make '+CleanCommand+'" twice inside '+CleanDirectory+' for target '+TLazarusCrossInstaller(Self).CrossInstaller.RegisterName,etInfo)
      else
        Infoln(infotext+'Running "make '+CleanCommand+'" twice inside '+CleanDirectory,etInfo);
    end;

    try
      ProcessorResult:=Processor.ExecuteAndWait;
      result:=(ProcessorResult=0);
      if result then
        Sleep(200)
      else
      begin
        // Do not fail if we are cleaning Lazarus itself or the Packager or LCL or Cpmponents
        if (ModuleName=_LAZARUS) OR (ModuleName=_PACKAGER) OR (ModuleName=_LCL) OR (ModuleName=_COMPONENTS) then result:=true;
        break;
      end;
    except
      on E: Exception do
      begin
        result := false;
        WritelnLog(infotext+'Failed with an exception!' + LineEnding + 'Details: ' + E.Message, true);
      end;
    end;
  end;

  {$ifdef MSWINDOWS}
  // Now try to restore lhelp
  if LHelpTemp<>'' then
  begin
    try
      FileCopy(
        LHelpTemp,
        IncludeTrailingPathDelimiter(InstallDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
        [cffOverWriteFile]);
    except
      Infoln(infotext+'Non-fatal error restoring lhelp from temp file '+LHelpTemp,etInfo);
    end;
  end;
  {$endif MSWINDOWS}

  // finally ... if something is still still still floating around ... delete it !!
  if (NOT CrossCompiling) then
  begin
    s:=ConcatPaths([SourceDirectory,'ide'])+DirectorySeparator+REVINCFILENAME;
    SysUtils.DeleteFile(s);

    s:=ConcatPaths([InstallDirectory,'lazbuild']);
    if FileExists(s+GetExeExt) then
    begin
      FileCopy(s+GetExeExt,s+'.old'+GetExeExt);
      SysUtils.DeleteFile(s+GetExeExt);
    end;
    s:=ConcatPaths([InstallDirectory,'lazarus']);
    if FileExists(s+GetExeExt) then
    begin
      FileCopy(s+GetExeExt,s+'.old'+GetExeExt);
      SysUtils.DeleteFile(s+GetExeExt);
    end;
    s:=ConcatPaths([InstallDirectory,'startlazarus']);
    if FileExists(s+GetExeExt) then
    begin
      FileCopy(s+GetExeExt,s+'.old'+GetExeExt);
      SysUtils.DeleteFile(s+GetExeExt);
    end;
  end;

  {
  if CrossCompiling then
    CPUOS_Signature:=GetFPCTarget(false)
  else
    CPUOS_Signature:=GetFPCTarget(true);
  DeleteList := TStringList.Create;
  try
    DeleteList.Add('.ppu');
    DeleteList.Add('.a');
    DeleteList.Add('.o');
    DeleteList.Add('.compiled');
    DeleteFilesExtensionsSubdirs(SourceDirectory,DeleteList,CPUOS_Signature);
  finally
    DeleteList.Free;
  end;
  }
end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
const
 VERSIONEXPRESSION='$FPC_VERSION';
 CPUEXPRESSION='$CPU_TARGET';
 OSEXPRESSION='$OS_TARGET';
 REGEXPACKAGE =
   '[package]'+LineEnding+
   'name=regexpr'+LineEnding+
   'version='+VERSIONEXPRESSION+LineEnding+
   '[require]'+LineEnding+
   'packages_'+OSEXPRESSION+'_'+CPUEXPRESSION+'='+LineEnding;
{$ifdef Darwin}
{$ifdef LCLQT5}
function CreateQT5Symlinks(aApp:string):boolean;
var
  DirectoriesFoundList,FilesFoundList : TStringList;
  DirCounter,FileCounter:integer;
  FrameworkDir,FrameworkName,FileToLink:string;
  success:boolean;
begin
  // create symlinks for Frameworks to save space
  result:=true;
  DirectoriesFoundList := FindAllDirectories(aApp,False);
  try
    for DirCounter := 0 to DirectoriesFoundList.Count -1 do
    begin
      FrameworkDir := ExcludeTrailingPathDelimiter(DirectoriesFoundList.Strings[DirCounter]);
      FrameworkName := ExtractFileNameOnly(FrameworkDir);
      FilesFoundList := FindAllFiles(FrameworkDir+'/Versions');
      try
        for FileCounter := 0 to FilesFoundList.Count -1 do
        begin
          FileToLink := FilesFoundList.Strings[FileCounter];
          if ExtractFileName(FileToLink) = FrameworkName then
          begin
            FileToLink:=CreateRelativePath(FileToLink,FrameworkDir);

            // do we already have some sort of file ?
            if (FileExists(FrameworkDir+'/'+FrameworkName)) then
            begin
              // if its not a link, then delete file !! tricky ...
              if (fpReadLink(FrameworkDir+'/'+FrameworkName) = '') then DeleteFile(FrameworkDir+'/'+FrameworkName);
            end;

            if (NOT FileExists(FrameworkDir+'/'+FrameworkName)) then
            begin
              // create the symlink towards the base framework library
              success:=(fpSymlink(PChar(FileToLink),PChar(FrameworkDir+'/'+FrameworkName))=0);
              if NOT success then
              begin
                result:=false;
                Infoln(infotext+'Symlink creation failure for '+FrameworkName,etError);
              end;
            end;

          end;
        end;
      finally
        FilesFoundList.Free;
      end;
    end;
  finally
    DirectoriesFoundList.Free;
  end;
end;
{$endif}
{$endif}
var
  UpdateWarnings : TStringList;
  aRepoClient    : TRepoClient;
  s              : string;
  SourceVersion  : string;
  SourceInfo     : TRevision;
  FilePath       : string;
  aIndex         : integer;
begin
  result:=inherited;
  result:=InitModule;

  if (not result) then exit;

  FPreviousRevision:='unknown';

  SourceVersion:='0.0.0';

  aRepoClient:=GetSuitableRepoClient;

  if aRepoClient=nil then
  begin
    result:=true;
    Infoln(infotext+'Downloading ' + ModuleName + ' sources.',etInfo);
    result:=DownloadFromURL(ModuleName);
    FActualRevision:=FPreviousRevision;

    if result and Ultibo then
    begin
      FilePath:=ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true),'regexpr'])+PathDelim+'Package.fpc';
      //if FileExists(FilePath) then SysUtils.DeleteFile(FilePath);
      if (NOT FileExists(FilePath)) then
      begin
        s:=REGEXPACKAGE;
        s:=StringReplace(s,VERSIONEXPRESSION,CompilerVersion(FCompiler),[]);
        s:=StringReplace(s,CPUEXPRESSION,GetSourceCPU,[]);
        s:=StringReplace(s,OSEXPRESSION,GetSourceOS,[]);

        UpdateWarnings:=TStringList.Create;
        try
          UpdateWarnings.Text:=s;
          UpdateWarnings.SaveToFile(FilePath);
        finally
          UpdateWarnings.Free;
        end;
      end;

      Processor.Executable := FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt;
      Processor.Process.Parameters.Clear;

      s:=Processor.Environment.GetVar('FPCDIR');
      try
        //Processor.Environment.SetVar('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true)]));
        Processor.Environment.SetVar('FPCDIR',ConcatPaths([FFPCSourceDir]));
        Processor.SetParamData('-T' + GetSourceCPU + '-' + GetSourceOS);

        Processor.Process.CurrentDirectory := SourceDirectory;
        ProcessorResult := Processor.ExecuteAndWait;

        Processor.Process.CurrentDirectory := ConcatPaths([SourceDirectory,'ide']);
        ProcessorResult := Processor.ExecuteAndWait;

        Processor.Process.CurrentDirectory := ConcatPaths([SourceDirectory,'components']);
        ProcessorResult := Processor.ExecuteAndWait;

        Processor.Process.CurrentDirectory := ConcatPaths([SourceDirectory,'tools']);
        ProcessorResult := Processor.ExecuteAndWait;
      finally
        Processor.Environment.SetVar('FPCDIR',s);
      end;

      //FActualRevision:='';
      //FPreviousRevision:=FActualRevision;

    end;
  end
  else
  begin
    Infoln(infotext+'Start checkout/update of ' + ModuleName + ' sources.',etInfo);

    //git svn clone -r HEAD https://svn.freepascal.org/svn/lazarus/tags/lazarus_2_0_12

    UpdateWarnings:=TStringList.Create;
    try
      if (aRepoClient.ClassType=FGitClient.ClassType)
         then result:=DownloadFromGit(ModuleName, FPreviousRevision, FActualRevision, UpdateWarnings)
         else result:=DownloadFromSVN(ModuleName, FPreviousRevision, FActualRevision, UpdateWarnings);

      if UpdateWarnings.Count>0 then
      begin
        WritelnLog(UpdateWarnings);
      end;
    finally
      UpdateWarnings.Free;
    end;
  end;

  if result then
  begin
    SourceVersion:=GetVersion;
    if (SourceVersion<>'0.0.0') then
    begin
      s:=GetRevisionFromVersion(ModuleName,SourceVersion);
      if (Length(s)>0) then
      begin
        //FActualRevision:=s;
        //FPreviousRevision:=s;
      end;
    end
    else
    begin
      Infoln(infotext+'Could not get version of ' + ModuleName + ' sources. Expect severe errors.',etError);
    end;

    if Assigned(aRepoClient) then
    begin
      if (aRepoClient.ClassType=FSVNClient.ClassType) then SourceInfo.SVNRevision:=aRepoClient.LocalRevision;
      if (aRepoClient.ClassType=FGitClient.ClassType) then SourceInfo.GITHash:=aRepoClient.LocalRevision;
    end;

    if FRepositoryUpdated then
    begin
      Infoln(infotext+ModuleName + ' was at revision/hash: '+PreviousRevision,etInfo);
      Infoln(infotext+ModuleName + ' is now at revision/hash: '+ActualRevision,etInfo);
    end
    else
    begin
      Infoln(infotext+ModuleName + ' is at revision/hash: '+ActualRevision,etInfo);
      Infoln(infotext+'No updates for ' + ModuleName + ' found.',etInfo);
    end;
    UpdateWarnings:=TStringList.Create;
    try
      s:=SafeExpandFileName(IncludeTrailingPathDelimiter(BaseDirectory)+REVISIONSLOG);
      if FileExists(s) then
        UpdateWarnings.LoadFromFile(s)
      else
      begin
        UpdateWarnings.Add('New install.');
        UpdateWarnings.Add('Date: '+DateTimeToStr(now));
        UpdateWarnings.Add('Location: '+BaseDirectory);
        UpdateWarnings.Add('');
      end;
      UpdateWarnings.Add(LAZDATEMAGIC+DateTimeToStr(now));
      if Assigned(aRepoClient) AND (aRepoClient.ClassType=FGitClient.ClassType) then
        UpdateWarnings.Add(LAZNAMEMAGIC+aRepoClient.GetCommitName);
      if Assigned(aRepoClient) then UpdateWarnings.Add(ModuleName+' URL: '+aRepoClient.Repository);
      UpdateWarnings.Add(ModuleName+' previous rev/hash: '+PreviousRevision);
      if Length(SourceInfo.SVNRevision)>0 then
        UpdateWarnings.Add(LAZREVMAGIC+SourceInfo.SVNRevision)
      else
      if Length(SourceInfo.GITHash)>0 then
        UpdateWarnings.Add(LAZHASHMAGIC+SourceInfo.GITHash)
      else
      if Length(ActualRevision)>0 then
        UpdateWarnings.Add(LAZREVMAGIC+ActualRevision);
      UpdateWarnings.Add('');
      UpdateWarnings.SaveToFile(s);
    finally
      UpdateWarnings.Free;
    end;

    CreateRevision(ModuleName,ActualRevision);

    if (SourceVersion<>'0.0.0') then PatchModule(ModuleName);

    if Ultibo OR ( (SourceVersion<>'0.0.0') AND (CompareVersionStrings(SourceVersion,'2.0.10')<=0) ) then
    begin
      // Prevent lazbuild crash !!
      FilePath:=ConcatPaths([SourceDirectory,'components','ideintf'])+PathDelim+'ideexterntoolintf.pas';
      if (FileExists(FilePath)) then
      begin
        UpdateWarnings:=TStringList.Create;
        try
          UpdateWarnings.LoadFromFile(FilePath);
          aIndex:=StringListContains(UpdateWarnings,'FWorkerMessages.EnterCriticalSection;');
          if (aIndex<>-1) then
          begin
            s:=UpdateWarnings.Strings[aIndex-1];
            if (Pos('FPCUP:',s)=0) AND (Pos('Sleep',s)=0) then
            begin
              UpdateWarnings.Insert(aIndex,'  Sleep(1); // FPCUP: force context switch to prevent occational crash ... issue #36318, #37883 etc.');
              UpdateWarnings.SaveToFile(FilePath);
            end;
          end;
          aIndex:=StringListContains(UpdateWarnings,'FWorkerMessages.LeaveCriticalSection;');
          if (aIndex<>-1) then
          begin
            s:=UpdateWarnings.Strings[aIndex-1];
            if (Pos('FPCUP:',s)=0) AND (Pos('Sleep',s)=0) then
            begin
              UpdateWarnings.Insert(aIndex,'  Sleep(1); // FPCUP: force context switch to prevent occational crash ... issue #36318, #37883 etc.');
              UpdateWarnings.SaveToFile(FilePath);
            end;
          end;
        finally
          UpdateWarnings.Free;
        end;
      end;
    end;

    {$ifdef Darwin}
    {$ifdef LCLQT5}
    // Only for Darwin
    // Get Qt bindings if not present yet
    // I know that this involves a lot of trickery and some dirty work, but it gives the user an öut-of-the-box" experience !
    // And fpcupdeluxe is there to make the user-experience of FPC and Lazarus an easy one
    // Note:
    // Do not fail on error : could be that the fpcupdeluxe user has installed QT5 by himself
    // ToDo : check if this presumption is correct

    FilePath:=ExcludeTrailingPathDelimiter(SafeGetApplicationName);
    Infoln(infotext+'Adding QT5 binary sources (QT5 + QT5Pas Frameworks + libqcocoa) from fpcupdeluxe.app itself.',etInfo);

    // copy QT5 frameworks to Lazarus source directory for future use.
    if DirCopy(FilePath+'/Contents/Frameworks',BaseDirectory+'/Frameworks') then
    begin
      CreateQT5Symlinks(BaseDirectory+'/Frameworks');
      Infoln(infotext+'Adding QT5 Frameworks to ' + BaseDirectory+'/Frameworks' + ' success.',etInfo);
    end else Infoln(infotext+'Adding QT5 Frameworks to ' + BaseDirectory+'/Frameworks' + ' failure.',etInfo);

    // copy QT5 frameworks to lazarus.app ... a bit redundant ... :-(
    if DirCopy(FilePath+'/Contents/Frameworks',SourceDirectory+'/lazarus.app/Contents/Frameworks') then
    begin
      CreateQT5Symlinks(SourceDirectory+'/lazarus.app/Contents/Frameworks');
      Infoln(infotext+'Adding QT5 Frameworks to lazarus.app success.',etInfo);
    end else Infoln(infotext+'Adding QT5 Frameworks to lazarus.app failure.',etInfo);

    // copy QT5 frameworks to startlazarus.app ... a bit redundant ... :-(
    if DirCopy(FilePath+'/Contents/Frameworks',SourceDirectory+'/startlazarus.app/Contents/Frameworks') then
    begin
      CreateQT5Symlinks(SourceDirectory+'/startlazarus.app/Contents/Frameworks');
      Infoln(infotext+'Adding QT5 Frameworks to startlazarus.app success.',etInfo);
    end else Infoln(infotext+'Adding QT5 Frameworks to startlazarus.app failure.',etInfo);
    CreateQT5Symlinks(SourceDirectory+'/lazarus.app');

    (*
    // QT5 quirk: copy QT5 libqcocoa.dylib to lazarus.app
    if DirCopy(FilePath+'/Contents/Plugins',SourceDirectory+'/lazarus.app/Contents/Plugins')
      then Infoln(infotext+'Adding QT5 libqcocoa.dylib success.',etInfo)
      else Infoln(infotext+'Adding QT5 libqcocoa.dylib failure.',etInfo);

    // QT5 quirk: copy QT5 libqcocoa.dylib to startlazarus.app
    if DirCopy(FilePath+'/Contents/Plugins',SourceDirectory+'/startlazarus.app/Contents/Plugins')
      then Infoln(infotext+'Adding QT5 libqcocoa.dylib success.',etInfo)
      else Infoln(infotext+'Adding QT5 libqcocoa.dylib failure.',etInfo);
    *)

    // copy QT5 plugins to lazarus.app ... a bit redundant ... :-(
    if DirCopy(FilePath+'/Contents/Plugins',SourceDirectory+'/lazarus.app/Contents/Plugins')
      then Infoln(infotext+'Adding QT5 plugins success.',etInfo)
      else Infoln(infotext+'Adding QT5 plugins failure.',etInfo);

    // copy QT5 plugins to startlazarus.app ... a bit redundant ... :-(
    if DirCopy(FilePath+'/Contents/Plugins',SourceDirectory+'/startlazarus.app/Contents/Plugins')
      then Infoln(infotext+'Adding QT5 plugins success.',etInfo)
      else Infoln(infotext+'Adding QT5 plugins failure.',etInfo);

    {$endif}
    {$endif}

    {$ifdef Unix}
    {$ifndef Darwin}
    {$ifdef LCLQT5}
    // Only for Haiku
    // Get/copy Qt libs if not present yet
    // I know that this involves a lot of trickery and some dirty work, but it gives the user an öut-of-the-box" experience !
    // And fpcupdeluxe is there to make the user-experience of FPC and Lazarus an easy one
    // Note:
    // Do not fail on error : could be that the fpcupdeluxe user has installed QT5 by himself
    // ToDo : check if this presumption is correct

    if LibWhich(LIBQT5) then
      Infoln(infotext+'System wide libQT5Pas found. No trickery needed !!',etInfo)
    else
      Infoln(infotext+'QT5 trickery needed: adding QT5Pas library from fpcupdeluxe itself.',etInfo);

    FilePath:=SafeGetApplicationPath;

    {$ifdef Haiku}
    if (NOT LibWhich(LIBQT5)) then
    begin
      {$ifdef CPUX86}
      s:='/boot/system/non-packaged/lib/x86/';
      {$else}
      s:='/boot/system/non-packaged/lib/';
      {$endif}
      ForceDirectoriesSafe(s);
      if FileExists(FilePath+LIBQT5VERSION) then
      begin
        if (NOT FileExists(s+LIBQT5VERSION)) then
          FileCopy(FilePath+LIBQT5VERSION,s+LIBQT5VERSION);
        if (NOT FileExists(s+LIBQT5)) then
          FileCopy(FilePath+LIBQT5VERSION,s+LIBQT5);
      end;
    end;
    {$endif}

    {$ifdef Unix}
    if (NOT LibWhich(LIBQT5)) then
    begin
      s:='/usr/local/lib/';
      if DirectoryExists(s) then
      begin
        if FileExists(FilePath+LIBQT5VERSION) then
        begin
          if (NOT FileExists(s+LIBQT5VERSION)) then
            FileCopy(FilePath+LIBQT5VERSION,s+LIBQT5VERSION);
          if (NOT FileExists(s+LIBQT5)) then
            FileCopy(FilePath+LIBQT5VERSION,s+LIBQT5);
        end;
      end;
    end;
    {$endif}

    if (NOT LibWhich(LIBQT5)) then
    begin
      s:='.1.2.13';
      if (NOT FileExists(FilePath+LIBQT5+s)) then s:='.1.2.9';
      if (NOT FileExists(FilePath+LIBQT5+s)) then s:='.1.2.8';
      if (NOT FileExists(FilePath+LIBQT5+s)) then s:='.1.2.7';
      if (NOT FileExists(FilePath+LIBQT5+s)) then s:='.1.2.6';
      if FileExists(FilePath+LIBQT5+s) then
      begin
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5+s)) then
          FileCopy(FilePath+LIBQT5+s,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5+s);
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5VERSION)) then
          FileCopy(FilePath+LIBQT5+s,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5VERSION);
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5)) then
          FileCopy(FilePath+LIBQT5+s,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5);
      end;

      //The below can be trivial, but just in case
      if FileExists(FilePath+LIBQT5VERSION) then
      begin
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5VERSION)) then
          FileCopy(FilePath+LIBQT5VERSION,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5VERSION);
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5)) then
          FileCopy(FilePath+LIBQT5VERSION,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5);
      end;
      if FileExists(FilePath+LIBQT5) then
      begin
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5)) then
          FileCopy(FilePath+LIBQT5,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5);
        if (NOT FileExists(IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5)) then
          FileCopy(FilePath+LIBQT5,IncludeTrailingPathDelimiter(InstallDirectory)+LIBQT5);
      end;
    end;

    {$endif}
    {$endif}
    {$endif}

    (*
    Errors := 0;
    if (Result) and (Uppercase(FCrossLCL_Platform) = 'QT') then
    begin
      for Counter := low(FUtilFiles) to high(FUtilFiles) do
      begin
        if (FUtilFiles[Counter].Category = ucQtFile) and not
          (FileExists(IncludeTrailingPathDelimiter(SourceDirectory) + FUtilFiles[Counter].FileName)) then
        begin
          Infoln(infotext+'Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + SourceDirectory, etDebug);
          try
            if Download(FUseWget, FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName, IncludeTrailingPathDelimiter(SourceDirectory) +
              FUtilFiles[Counter].FileName, FHTTPProxyHost, FHTTPProxyPort, FHTTPProxyUser,
              FHTTPProxyPassword) = false then
            begin
              Errors := Errors + 1;
              Infoln(infotext+'Error downloading Qt-related file to ' + IncludeTrailingPathDelimiter(SourceDirectory) +
                FUtilFiles[Counter].FileName, eterror);
            end;
          except
            on E: Exception do
            begin
              Result := false;
              Infoln(infotext+'Error downloading Qt-related files: ' + E.Message, etError);
              exit; //out of function.
            end;
          end;
        end;
      end;

      if Errors > 0 then
      begin
        Result := false;
        WritelnLog(infotext+IntToStr(Errors) + ' errors downloading Qt-related files.', true);
      end;
    end;
    *)

    {$ifdef BSD}
    FilePath:=ConcatPaths([SourceDirectory,'ide','include'])+DirectorySeparator;
    if (DirectoryExists(FilePath+'netbsd')) then
    begin
      if (NOT DirectoryExists(FilePath+'dragonfly')) then
      begin
        if DirCopy(FilePath+'netbsd',FilePath+'dragonfly')
          then Infoln(infotext+'Adding dragonfly include file for IDE.',etInfo)
          else Infoln(infotext+'Adding dragonfly include file for IDE failure.',etError);
      end;
    end;
    {$endif}
  end
  else
  begin
    Infoln(infotext+'Checkout/update of ' + ModuleName + ' sources failure.',etError);
  end;
end;

function TLazarusInstaller.CheckModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=inherited;
end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  Result := inherited;

  if not DirectoryExists(SourceDirectory) then
  begin
    Infoln(infotext+'No Lazarus sources [yet] ... nothing to be done',etDebug);
  end;
  if CheckDirectory(SourceDirectory) then exit;

  Result := InitModule;
  if not Result then exit;

  FErrorLog.Clear;

  // Sanity check so we don't try to delete random directories
  if FileExists(IncludeTrailingPathDelimiter(SourceDirectory) + MAKEFILENAME) and DirectoryExists(
    IncludeTrailingPathDelimiter(SourceDirectory) + 'ide') and DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory) + 'lcl') and
    ParentDirectoryIsNotRoot(IncludeTrailingPathDelimiter(SourceDirectory)) then
  begin
    Result := DeleteDirectoryEx(SourceDirectory);
    if not (Result) then
      WritelnLog(infotext+'Error deleting Lazarus directory ' + SourceDirectory);
  end
  else
  begin
    WritelnLog(infotext+'Error: invalid Lazarus directory :' + SourceDirectory);
    Result := false;
  end;

  if result then
  begin
    // Sanity check so we don't try to delete random directories
    // Assume Lazarus has been configured/run once so enviroronmentoptions.xml exists.
    if FileExists(IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath) + EnvironmentConfig) and
      ParentDirectoryIsNotRoot(IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)) then
    begin
      Result := DeleteDirectoryEx(FLazarusPrimaryConfigPath) = false;
      if not (Result) then
        WritelnLog(infotext+'Error deleting Lazarus FLazarusPrimaryConfigPath directory ' + FLazarusPrimaryConfigPath);
    end
    else
    begin
      WritelnLog(infotext+'Error: invalid Lazarus FLazarusPrimaryConfigPath: ' + FLazarusPrimaryConfigPath);
      Result := false;
    end;
  end;

end;

function TLazarusInstaller.LCLCrossActionNeeded:boolean;
var
  NothingToBeDone:boolean;
begin
  NothingToBeDone:=true;
  if FLCL_Platform<>'' then
  begin
    NothingToBeDone:=false;
    {$ifdef Darwin}
      {$ifdef LCLCARBON}
        NothingToBeDone:=(FLCL_Platform='carbon');
      {$endif}
      {$ifdef LCLCOCOA}
        NothingToBeDone:=(FLCL_Platform='cocoa');
      {$endif}
      {$ifdef CPU64}
        {$ifndef LCLQT5}
          NothingToBeDone:=(FLCL_Platform='cocoa');
        {$endif}
      {$endif}
    {$endif}
    {$ifdef LCLQT}
      NothingToBeDone:=(FLCL_Platform='qt');
    {$endif}
    {$ifdef LCLQT5}
      NothingToBeDone:=(FLCL_Platform='qt5');
    {$endif}
  end;
  result:=(NOT NothingToBeDone);
end;

constructor TLazarusInstaller.Create;
begin
  inherited Create;
  InitDone := false;
end;

destructor TLazarusInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

