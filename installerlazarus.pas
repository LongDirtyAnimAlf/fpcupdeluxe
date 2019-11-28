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

{.$define lazarus_parallel_make} {for make --jobs= support; for now Lazarus does not seem to support parallel make -> error 512}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils, strutils;
//todo: use processex callback to report on errors like it's done in installerfpc

const
  Sequences =
    //standard lazarus build
    _DECLARE+_LAZARUS+_SEP +
    _CLEANMODULE+_LAZARUS+_SEP +
    _CHECKMODULE+_LAZARUS+_SEP +
    _GETMODULE+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    _DO+_USERIDE+_SEP +
    _BUILDMODULE+_STARTLAZARUS+_SEP +
    _DO+_UNIVERSALDEFAULT+_SEP+
    _DO+_HELPLAZARUS+_SEP+
    _EXECUTE+_CREATELAZARUSSCRIPT+_SEP +
    _END +

    _DECLARE+_LAZARUSSIMPLE+_SEP +
    _CLEANMODULE+_LAZARUS+_SEP +
    _CHECKMODULE+_LAZARUS+_SEP +
    _GETMODULE+_LAZARUS+_SEP +
    _CONFIGMODULE+_LAZARUS+_SEP +
    _BUILDMODULE+_LAZARUS+_SEP +
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

    //standard useride build
    _DECLARE+_USERIDE+_SEP +
    _BUILDMODULE+_LAZBUILD+_SEP +
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

  TLazarusInstaller = class(TInstaller)
  private
    FBinPath: string; //path where compiler lives
    FCrossLCL_Platform: string;
    FPrimaryConfigPath: string;
    FRevision: string;
    InitDone: boolean;
    function GetLazarusVersionFromSource(aSourceDirectory:string):string;
    function GetLazarusVersionFromUrl(aURL:string):string;
    function GetLazarusReleaseCandidateFromSource(aSourceDirectory:string):integer;
    function GetLazarusReleaseCandidateFromUrl(aURL:string):integer;
    function LCLCrossActionNeeded:boolean;
  protected
    FFPCInstallDir: string;
    FFPCSourceDir: string;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; virtual;
    function GetLazarusVersion: string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule: boolean;
  public
    // LCL widget set to be built (NOT OS/CPU combination)
    property CrossLCL_Platform: string write FCrossLCL_Platform;
    // FPC base directory
    property FPCInstallDir: string write FFPCInstallDir;
    // FPC source directory
    property FPCSourceDir: string write FFPCSourceDir;
    // Lazarus primary config path
    property PrimaryConfigPath: string write FPrimaryConfigPath;
    // Local revision of source
    property Revision: string read FRevision write FRevision;
    // Build module
    function BuildModule(ModuleName: string): boolean; override;
    // Create configuration in PrimaryConfigPath
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

type

  { TLazarusNativeInstaller }

  TLazarusNativeInstaller = class(TLazarusInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type

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
  fpcuputil, fileutil,
  repoclient,
  updatelazconfig
  {$ifdef Darwin}
  {$ifdef LCLQT5}
  ,baseunix
  ,LazFileUtils
  {$endif LCLQT5}
  {$endif Darwin}
  ;

{ TLazarusCrossInstaller }

function TLazarusCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  Options: string;
  LazBuildApp: string;
begin
  Result:=inherited;

  FErrorLog.Clear;

  if Assigned(CrossInstaller) then
  begin
    // Actually not using crossopts - they're only for building an FPC compiler; the
    // relevant options should have been written as a snippet to fpc.cfg and picked
    // up from there.
    CrossInstaller.SetCrossOpt(CrossOPT); //pass on user-requested cross compile options
    CrossInstaller.SetSubArch(CrossOS_SubArch);
    CrossInstaller.SolarisOI:=FSolarisOI;
    CrossInstaller.MUSL:=FMUSL;
    if not CrossInstaller.GetBinUtils(FFPCInstallDir) then
      infoln(infotext+'Failed to get crossbinutils', etError)
    else if not CrossInstaller.GetLibs(FFPCInstallDir) then
      infoln(infotext+'Failed to get cross libraries', etError)
    else if not CrossInstaller.GetLibsLCL(FCrossLCL_Platform, FInstallDirectory) then
      infoln(infotext+'Failed to get LCL cross libraries', etError)
    else
      // Cross compiling prerequisites in place. Let's compile.
    begin
      // If we're "crosscompiling" with the native compiler and binutils - "cross compiling [lite]" - use lazbuild.
      // Advantages:
      // - dependencies are taken care of
      // - it won't trigger a rebuild of the LCL when the user compiles his first cross project.
      // Otherwise, use make; advantages:
      // - can deal with various bin tools
      // - can deal with compiler options
      // - doesn't need existing lazbuild (+nogui LCL)

      LazBuildApp := IncludeTrailingPathDelimiter(FInstallDirectory) + LAZBUILDNAME + GetExeExt;
      if CheckExecutable(LazBuildApp, '--help', LAZBUILDNAME) = false then
      begin
        writelnlog(etWarning, infotext+'Lazbuild could not be found ... using make to cross-build '+ModuleName, true);
        LazBuildApp := '';
      end;

      // Since April 2012, LCL requires lazutils which requires registration
      // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
      //http://lists.lazarus-ide.org/pipermail/lazarus/2012-April/138168.html

      if Length(LazBuildApp)=0 then
      begin
        // Use make for cross compiling
        Processor.Executable := Make;
        Processor.Parameters.Clear;
        {$IFDEF MSWINDOWS}
        if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
        {$ENDIF}
        Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
        {
        //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
        if (FNoJobs) then
          Processor.Parameters.Add('--jobs=1')
        else
          Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));}
        Processor.Parameters.Add('FPC=' + FCompiler);
        Processor.Parameters.Add('PP=' + ExtractFilePath(FCompiler)+GetCompilerName(GetTargetCPU));
        Processor.Parameters.Add('USESVN2REVISIONINC=0');
        Processor.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
        Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));

        //Make sure our FPC units can be found by Lazarus
        Processor.Parameters.Add('FPCDIR=' + ExcludeTrailingPathDelimiter(FFPCSourceDir));
        //Make sure Lazarus does not pick up these tools from other installs
        Processor.Parameters.Add('FPCMAKE=' + IncludeTrailingPathDelimiter(FFPCInstallDir)+'bin'+DirectorySeparator+GetFPCTarget(true)+DirectorySeparator+'fpcmake'+GetExeExt);
        Processor.Parameters.Add('PPUMOVE=' + IncludeTrailingPathDelimiter(FFPCInstallDir)+'bin'+DirectorySeparator+GetFPCTarget(true)+DirectorySeparator+'ppumove'+GetExeExt);

        Options:=IncludeTrailingPathDelimiter(FPrimaryConfigPath)+DefaultIDEMakeOptionFilename;
        if FileExists(Options) then Processor.Parameters.Add('CFGFILE=' + Options);

        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath <> '' then
          Processor.Parameters.Add('CROSSBINDIR=' + ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));

        {$ifdef Windows}
        Processor.Parameters.Add('UPXPROG=echo');      //Don't use UPX
        Processor.Parameters.Add('COPYTREE=echo');     //fix for examples in Win svn, see build FAQ
        {$endif}

        Processor.Parameters.Add('OS_SOURCE=' + GetTargetOS);
        Processor.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);
        Processor.Parameters.Add('CPU_TARGET=' + CrossCPU_Target);
        Processor.Parameters.Add('OS_TARGET=' + CrossOS_Target);

        if FCrossLCL_Platform <> '' then
          Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);

        //Set options
        Options := STANDARDCOMPILERVERBOSITYOPTIONS+' '+FCompilerOptions;
        if CrossInstaller.LibsPath <> '' then
          Options := Options + ' -Xd -Fl' + CrossInstaller.LibsPath;
        if CrossInstaller.BinUtilsPrefix <> '' then
        begin
          Options := Options + ' -XP' + CrossInstaller.BinUtilsPrefix;
          Processor.Parameters.Add('BINUTILSPREFIX=' + CrossInstaller.BinUtilsPrefix);
        end;
        while Pos('  ',Options)>0 do
        begin
          Options:=StringReplace(Options,'  ',' ',[rfReplaceAll]);
        end;
        Options:=Trim(Options);
        if Length(Options)>0 then Processor.Parameters.Add('OPT='+Options);

        Processor.Parameters.Add('registration');
        Processor.Parameters.Add('lazutils');
        Processor.Parameters.Add('lcl');
        Processor.Parameters.Add('basecomponents');
      end
      else
      begin
        // Use lazbuild for cross compiling
        Processor.Executable := LazBuildApp;
        Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
        Processor.Parameters.Clear;
        {$IFDEF DEBUG}
        Processor.Parameters.Add('--verbose');
        {$ELSE}
        // See compileroptions.pp
        // Quiet:=ConsoleVerbosity<=-3;
        Processor.Parameters.Add('--quiet');
        {$ENDIF}
        if (FNoJobs) then
          Processor.Parameters.Add('--max-process-count=1')
        else
          Processor.Parameters.Add('--max-process-count='+InttoStr(GetLogicalCpuCount));
        Processor.Parameters.Add('--pcp=' + DoubleQuoteIfNeeded(FPrimaryConfigPath));

        // Apparently, the .compiled file, that are used to check for a rebuild, do not contain a cpu setting if cpu and cross-cpu do not differ !!
        // So, use this test to prevent a rebuild !!!
        if (GetTargetCPU<>CrossCPU_Target) then
          Processor.Parameters.Add('--cpu=' + CrossCPU_Target);

        // See above: the same for OS !
        if (GetTargetOS<>CrossOS_Target) then
          Processor.Parameters.Add('--os=' + CrossOS_Target);

        if FCrossLCL_Platform <> '' then
          Processor.Parameters.Add('--ws=' + FCrossLCL_Platform);

        Processor.Parameters.Add('packager'+DirectorySeparator+'registration'+DirectorySeparator+'fcl.lpk');
        Processor.Parameters.Add('components'+DirectorySeparator+'lazutils'+DirectorySeparator+'lazutils.lpk');
        Processor.Parameters.Add('lcl'+DirectorySeparator+'interfaces'+DirectorySeparator+'lcl.lpk');
        // Also add the basecomponents !
        Processor.Parameters.Add('components'+DirectorySeparator+'synedit'+DirectorySeparator+'synedit.lpk');
        Processor.Parameters.Add('components'+DirectorySeparator+'lazcontrols'+DirectorySeparator+'lazcontrols.lpk');
        Processor.Parameters.Add('components'+DirectorySeparator+'ideintf'+DirectorySeparator+'ideintf.lpk');
      end;

      if FCrossLCL_Platform = '' then
        infoln(infotext+'Compiling LCL for ' + GetFPCTarget(false) + ' using ' + ExtractFileName(Processor.Executable), etInfo)
      else
        infoln(infotext+'Compiling LCL for ' + GetFPCTarget(false) + '/' + FCrossLCL_Platform + ' using ' + ExtractFileName(Processor.Executable), etInfo);

      try
        writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
        Processor.Execute;
        Result := Processor.ExitStatus = 0;
        if not Result then
          WritelnLog(etError,infotext+'Error compiling LCL for ' + GetFPCTarget(false) + ' ' + FCrossLCL_Platform + LineEnding +
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
        if (CrossInstaller.TargetCPU = 'x86_64') and ((CrossInstaller.TargetOS = 'win64') or (CrossInstaller.TargetOS = 'win32')) then
          Result := true;
        {$endif win32}
        {$ifdef win64}
        // if this is crosswin64-32, ignore error as it is optional
        if (CrossInstaller.TargetCPU = 'i386') and (CrossInstaller.TargetOS = 'win32') then
          Result := true;
        {$endif win64}
        if Result then
          infoln(infotext+'Cross compiling LCL for ' + GetFPCTarget(false) +
            ' failed. Optional module; continuing regardless.', etWarning)
        else
          infoln(infotext+'Cross compiling LCL for ' + GetFPCTarget(false) + ' failed.', etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(Result);
      end;
    end; //prereqs in place

  end    //valid cross compile setup
  else
    infoln(infotext+'Can''t find cross installer for ' + GetFPCTarget(false), etError);
end;

function TLazarusCrossInstaller.UnInstallModule(ModuleName:string): boolean;
var
  aDir:string;
begin
  Result:=true;

  FErrorLog.Clear;

  if Assigned(CrossInstaller) then
  begin

    //CrossInstaller.Reset;

    case ModuleName of
      _LCL:
      begin
        aDir:=IncludeTrailingPathDelimiter(FInstallDirectory)+'lcl'+DirectorySeparator+'units'+DirectorySeparator+GetFPCTarget(false);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+aDir);
        end;
      end;
      _PACKAGER:
      begin
        aDir:=IncludeTrailingPathDelimiter(FInstallDirectory)+'packager'+DirectorySeparator+'units'+DirectorySeparator+GetFPCTarget(false);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+aDir);
        end;
      end;
      _COMPONENTS:
      begin
        aDir:=IncludeTrailingPathDelimiter(FInstallDirectory)+'components'+DirectorySeparator+'lazutils'+DirectorySeparator+'lib'+DirectorySeparator+GetFPCTarget(false);
        if DeleteDirectoryEx(aDir)=false then
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
  i,j,ExitCode: integer;
  s,LazBuildApp,FPCDirStore: string;
  OperationSucceeded: boolean;
  LazarusConfig: TUpdateLazConfig;
begin
  Result:=inherited;

  OperationSucceeded := true;

  //Get Freetype and Zlib for ao fpreport ... just to be sure
  {$IFDEF MSWINDOWS}
  //DownloadFreetype;
  //DownloadZlib;
  {$ENDIF}

  LazBuildApp := IncludeTrailingPathDelimiter(FInstallDirectory) + LAZBUILDNAME + GetExeExt;

  //Note: available in more recent Lazarus : use "make lazbuild useride" to build ide with installed packages
  if ((ModuleName<>_USERIDE) OR (NumericalVersion>=CalculateFullVersion(1,6,2))) then
  begin
    // Make all (should include lcl & ide), lazbuild, lcl etc
    // distclean was already run; otherwise specify make clean all
    FErrorLog.Clear;
    Processor.Executable := Make;
    Processor.Parameters.Clear;
    {$IFDEF MSWINDOWS}
    if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
    {$ENDIF}
    Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
    {
    //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
    if (FNoJobs) then
      Processor.Parameters.Add('--jobs=1')
    else
      Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));}
    Processor.Parameters.Add('FPC=' + FCompiler);
    Processor.Parameters.Add('PP=' + ExtractFilePath(FCompiler)+GetCompilerName(GetTargetCPU));
    Processor.Parameters.Add('USESVN2REVISIONINC=0');
    //Processor.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
    Processor.Parameters.Add('--directory=.');
    Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
    //Make sure our FPC units can be found by Lazarus
    Processor.Parameters.Add('FPCDIR=' + ExcludeTrailingPathDelimiter(FFPCSourceDir));
    //Make sure Lazarus does not pick up these tools from other installs
    Processor.Parameters.Add('FPCMAKE=' + IncludeTrailingPathDelimiter(FFPCInstallDir)+'bin'+DirectorySeparator+GetFPCTarget(true)+DirectorySeparator+'fpcmake'+GetExeExt);
    Processor.Parameters.Add('PPUMOVE=' + IncludeTrailingPathDelimiter(FFPCInstallDir)+'bin'+DirectorySeparator+GetFPCTarget(true)+DirectorySeparator+'ppumove'+GetExeExt);

    {$ifdef Windows}
    Processor.Parameters.Add('UPXPROG=echo');      //Don't use UPX
    Processor.Parameters.Add('COPYTREE=echo');     //fix for examples in Win svn, see build FAQ
    {$endif}

    if FCrossLCL_Platform <> '' then
      Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);

    //Set options
    s:=STANDARDCOMPILERVERBOSITYOPTIONS+' '+FCompilerOptions;

    //Lazbuild MUST be build without giving any extra optimization options
    //At least on Linux anything else gives errors when trying to use lazbuild ... :-(
    if ModuleName=_LAZBUILD then
    begin
      i:=Pos('-O',s);
      if i>0 then
      begin
        if s[i+2] in ['0'..'9'] then
        begin
          Delete(s,i,3);
        end;
      end;
    end;

    // remove double spaces
    while Pos('  ',s)>0 do
    begin
      s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
    end;

    s:=Trim(s);

    if Length(s)>0 then Processor.Parameters.Add('OPT='+s);

    case ModuleName of
      _USERIDE:
      begin
        s:=IncludeTrailingPathDelimiter(FPrimaryConfigPath)+DefaultIDEMakeOptionFilename;
        if FileExists(s) then
        begin
          //Set config-file
          Processor.Parameters.Add('LAZBUILDJOBS='+IntToStr(FCPUCount));
          Processor.Parameters.Add('CFGFILE=' + s);
          Processor.Parameters.Add('useride');
          infoln(infotext+'Running: make useride', etInfo);
        end
        else
        begin
          // sometimes, we get an error 217 when buidling lazarus for the first time.
          // the below tries to prevent this by not using lazbuild on a fresh install.
          Processor.Parameters.Add('registration');
          Processor.Parameters.Add('lazutils');
          Processor.Parameters.Add('lcl');
          Processor.Parameters.Add('basecomponents');
          Processor.Parameters.Add('ide');
          infoln(infotext+'Running: make registration lazutils lcl basecomponents ide', etInfo);
        end;
      end;
      _IDE:
      begin
        Processor.Parameters.Add('idepkg');
        infoln(infotext+'Running: make idepkg', etInfo);
      end;
      _BIGIDE:
      begin
        Processor.Parameters.Add('idebig');
        infoln(infotext+'Running: make idebig', etInfo);
      end;
      _LAZARUS:
      begin
        Processor.Parameters.Add('all');
        //Processor.Parameters.Add('install');
        infoln(infotext+'Running: make all', etInfo);
      end;
      _STARTLAZARUS:
      begin
        if FileExists(IncludeTrailingPathDelimiter(FInstallDirectory) + 'startlazarus' + GetExeExt) then
        begin
          infoln(infotext+'StartLazarus already available ... skip building it.', etInfo);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
        Processor.Parameters.Add('starter');
        infoln(infotext+'Running: make starter', etInfo);
      end;
      _LAZBUILD:
      begin
        if FileExists(IncludeTrailingPathDelimiter(FInstallDirectory) + LAZBUILDNAME + GetExeExt) then
        begin
          infoln(infotext+'Lazbuild already available ... skip building it.', etInfo);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
        Processor.Parameters.Add('lazbuild');
        infoln(infotext+'Running: make lazbuild', etInfo);
      end;
      _LCL:
      begin
        // April 2012: lcl now requires lazutils and registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        // http://lists.lazarus-ide.org/pipermail/lazarus/2012-April/138168.html
        Processor.Parameters.Add('registration');
        Processor.Parameters.Add('lazutils');
        Processor.Parameters.Add('lcl');
        // always build standard LCL for native system ... other widgetsets to be done by LCLCROSS: see below
        //if FCrossLCL_Platform<>'' then Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
        infoln(infotext+'Running: make registration lazutils lcl', etInfo);
      end;
      _LCLCROSS:
      begin
        if LCLCrossActionNeeded then
        begin
          // first: Processor.Parameters.Add('-C lcl'+DirectorySeparator+'interfaces'+DirectorySeparator+FCrossLCL_Platform);
          // followed by: make ideintf basecomponents bigidecomponents LCL_PLATFORM=qt
          Processor.Parameters.Add('-C lcl');
          Processor.Parameters.Add('intf');
          //Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
          infoln(infotext+'Running: make -C lcl intf', etInfo);
        end
        else
        begin
          // nothing to be done: exit graceously
          infoln(infotext+'No extra LCL_PLATFORM defined ... nothing to be done', etInfo);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
      end;
      _MAKEFILECHECKLAZARUS:
      begin
        Processor.Parameters.Add('fpc_baseinfo');
        infoln(infotext+'Running: make fpc_baseinfo', etInfo);
      end
      else //raise error;
      begin
        Processor.Parameters.Add('--help'); // this should render make harmless
        WritelnLog(etError, infotext+'Invalid module name ' + ModuleName + ' specified! Please fix the code.', true);
        OperationSucceeded := false;
        Result := false;
        exit;
      end;
      if FCrossLCL_Platform<>'' then Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
    end;

    try
      WritelnLog(infotext+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
      Processor.Execute;
      ExitCode := Processor.ExitStatus;
      if ExitCode <> 0 then
      begin
        WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' returned error code #'+IntToStr(ExitCode), true);
        OperationSucceeded := false;
        Result := false;
      end;
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
      if CheckExecutable(IncludeTrailingPathDelimiter(FInstallDirectory) + LAZBUILDNAME + GetExeExt, '--help', LAZBUILDNAME) = false then
      begin
        WritelnLog(etError, infotext+'Lazbuild could not be found, so cannot build USERIDE.', true);
        Result := false;
        exit;
      end;
    end;

  end
  else
  begin
    // For building useride for Lazarus versions < 1.6.2
    // useride; using lazbuild. Note: in recent Lazarus we use make
    // Check for valid lazbuild.
    // Note: we don't check if we have a valid primary config path, but that will come out
    // in the next steps.
    if CheckExecutable(LazBuildApp, '--help', LAZBUILDNAME) = false then
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
      Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Parameters.Clear;
      //SysUtils.GetEnvironmentVariable('FPCDIR');
      //Makefile could pickup this FPCDIR setting, so try to set it for fpcupdeluxe
      FPCDirStore:=Processor.Environment.GetVar('FPCDIR');
      Processor.Environment.SetVar('FPCDIR',ExcludeTrailingPathDelimiter(FFPCSourceDir));
      {$IFDEF DEBUG}
      Processor.Parameters.Add('--verbose');
      {$ELSE}
      // See compileroptions.pp
      // Quiet:=ConsoleVerbosity<=-3;
      Processor.Parameters.Add('--quiet');
      {$ENDIF}
      if (FNoJobs) then
        Processor.Parameters.Add('--max-process-count=1')
      else
        Processor.Parameters.Add('--max-process-count='+InttoStr(GetLogicalCpuCount));
      Processor.Parameters.Add('--pcp=' + DoubleQuoteIfNeeded(FPrimaryConfigPath));
      Processor.Parameters.Add('--cpu=' + GetTargetCPU);
      Processor.Parameters.Add('--os=' + GetTargetOS);

      if FCrossLCL_Platform <> '' then
        Processor.Parameters.Add('--ws=' + FCrossLCL_Platform);

      // Support keeping userdefined installed packages when building.
      // Compile with selected compiler options
      // Assume new Laz version on failure getting revision
      if StrToIntDef(Revision, 38971) >= 38971 then
      begin
        Processor.Parameters.Add('--build-ide=-dKeepInstalledPackages ' + FCompilerOptions);
      end
      else
      begin
        // Fallback - depends on hardcoded "Normal IDE" build mode being present
        // We can specify a build mode; otherwise probably the latest build mode will be used
        // which could well be a stripped IDE
        // Let's see how/if CompilerOptions clashes with the settings in normal build mode
        WritelnLog(infotext+'LazBuild: building UserIDE but falling back to --build-mode="Normal IDE"', true);
        Processor.Parameters.Add('--build-ide= ' + FCompilerOptions);
        Processor.Parameters.Add('--build-mode="Normal IDE"');
      end;

      // Run first time...
      if OperationSucceeded then
      begin
        infoln(infotext+'Running lazbuild to get IDE with user-specified packages', etInfo);
        try
          WritelnLog(infotext+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
          Processor.Execute;
          Processor.Environment.SetVar('FPCDIR',FPCDirStore);
          if Processor.ExitStatus <> 0 then
          begin
            WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' returned error code ' + IntToStr(Processor.ExitStatus) + LineEnding +
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

      // ... build startlazarus if it doesn't exist
      // (even an old version left over by make distclean is probably ok)
      if OperationSucceeded then
      begin
        if FileExists(IncludeTrailingPathDelimiter(FInstallDirectory) + 'startlazarus' + GetExeExt) then
        begin
          infoln(infotext+'Startlazarus exists already. Not compiling again.', etdebug);
        end
        else
        begin
          Processor.Executable := LazBuildApp;
          FErrorLog.Clear;
          Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
          Processor.Parameters.Clear;
          //Makefile could pickup this FPCDIR setting, so try to set it for fpcupdeluxe
          FPCDirStore:=Processor.Environment.GetVar('FPCDIR');
          Processor.Environment.SetVar('FPCDIR',ExcludeTrailingPathDelimiter(FFPCSourceDir));
          {$IFDEF DEBUG}
          Processor.Parameters.Add('--verbose');
          {$ELSE}
          Processor.Parameters.Add('--quiet');
          {$ENDIF}
          if (FNoJobs) then
            Processor.Parameters.Add('--max-process-count=1')
          else
            Processor.Parameters.Add('--max-process-count='+InttoStr(GetLogicalCpuCount));
          Processor.Parameters.Add('--pcp=' + DoubleQuoteIfNeeded(FPrimaryConfigPath));
          Processor.Parameters.Add('--cpu=' + GetTargetCPU);
          Processor.Parameters.Add('--os=' + GetTargetOS);

          if FCrossLCL_Platform <> '' then
            Processor.Parameters.Add('--ws=' + FCrossLCL_Platform);

          Processor.Parameters.Add(DoubleQuoteIfNeeded(IncludeTrailingPathDelimiter(FSourceDirectory)+
            'ide'+DirectorySeparator+'startlazarus.lpi'));

          infoln(infotext+'Compiling startlazarus to make sure it is present:', etInfo);
          try
            writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
            Processor.Execute;
            Processor.Environment.SetVar('FPCDIR',FPCDirStore);
            if Processor.ExitStatus <> 0 then
            begin
              Writelnlog(etError, infotext+'Lazbuild startlazarus returned error code ' + IntToStr(Processor.ExitStatus) + LineEnding +
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
    end;
  end;

  if (ModuleName=_MAKEFILECHECKLAZARUS) then exit;

  if (ModuleName=_USERIDE) OR (ModuleName=_LAZARUS) then
  begin
    if OperationSucceeded then
    begin
      LazarusConfig:=TUpdateLazConfig.Create(FPrimaryConfigPath);
      try
        {$ifdef LCLQT5}
        //Set default sizes and position
        LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Left', '10');
        LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Top', '30');
        LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Width', '900');
        LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Height', '60');
        LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/Visible/Value', 'True');
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

        j:=LazarusConfig.GetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/Count',0);
        if j=0 then
        begin
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/Count', 2);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/Width/Value', 260);

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Version', 1);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Count', 12);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button1/Name', 'NewUnit');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button2/Name', 'NewForm');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button3/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button4/Name', 'Open');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button5/Name', 'Save');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button6/Name', 'SaveAll');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button7/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button8/Name', 'Toggle between Unit and Form');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button9/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button10/Name', 'Find in files');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button11/Name', 'General environment options');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar1/Button12/Name', 'View project options');

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Version', 1);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Count', 11);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Break/Value', True);
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button1/Name', 'View Units');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button2/Name', 'View Forms');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button3/Name', '---------------');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button4/Name', 'Change build mode');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button5/Name', 'Run without debugging');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button6/Name', 'Run program');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button7/Name', 'Pause program');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button8/Name', 'Stop program');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button9/Name', 'Step over');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button10/Name', 'Step into');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/ToolBar2/Button11/Name', 'Step out');
        end;

        // set defaults for pas2js
        LazarusConfig.SetVariableIfNewFile(Pas2jsConfig, 'compiler/value', ExtractFilePath(FCompiler)+'pas2js'+GetExeExt);
        LazarusConfig.SetVariableIfNewFile(Pas2jsConfig, 'webserver/value',  ExtractFilePath(FCompiler)+'compileserver'+GetExeExt);
        //LazarusConfig.SetVariableIfNewFile(Pas2jsConfig, 'webserver/startatport/value', '8000');

      finally
        LazarusConfig.Free;
      end;
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

function TLazarusInstaller.GetLazarusVersionFromSource(aSourceDirectory:string):string;
const
  VERSIONMAGIC='LazarusVersionStr';
  VERSIONMAGIC2='laz_version';
var
  s,aFileName:string;
  TxtFile:Text;
  x,y:integer;
begin
  result:='0.0.0';

  aFileName:=IncludeTrailingPathDelimiter(aSourceDirectory) + 'ide' + DirectorySeparator + 'version.inc';
  if FileExists(aFileName) then
  begin
    AssignFile(TxtFile,aFileName);
    Reset(TxtFile);
    Readln(TxtFile,s);
    // remove quotes from string
    //VersionSnippet:=DelChars(s, '''');
    s:=TrimSet(s, [#39]);
    s:=Trim(s);
    if Length(s)>0 then result:=s;
    CloseFile(TxtFile);
  end;

  if result='0.0.0' then
  begin
    aFileName:=IncludeTrailingPathDelimiter(aSourceDirectory) + 'ide' + DirectorySeparator + 'aboutfrm.pas';
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
    aFileName:=IncludeTrailingPathDelimiter(aSourceDirectory) + 'components' + DirectorySeparator + 'lazutils' + DirectorySeparator  + 'lazversion.pas';
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

function TLazarusInstaller.GetLazarusReleaseCandidateFromSource(aSourceDirectory:string):integer;
const
  VERSIONMAGIC='LazarusVersionStr';
  //VERSIONMAGIC2='laz_patch';
var
  s,aFileName:string;
  TxtFile:Text;
  x,y:integer;
begin
  result:=-1;

  aFileName:=IncludeTrailingPathDelimiter(aSourceDirectory) + 'ide' + DirectorySeparator + 'version.inc';
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
      if x>0 then
      begin
        s:=Copy(s,x,MaxInt);
        result:=StrToIntDef(s,-1);
      end;
    end;
    CloseFile(TxtFile);
  end;

  if result=-1 then
  begin
    aFileName:=IncludeTrailingPathDelimiter(aSourceDirectory) + 'ide' + DirectorySeparator + 'aboutfrm.pas';
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
    aFileName:=IncludeTrailingPathDelimiter(aSourceDirectory) + 'components' + DirectorySeparator + 'lazutils' + DirectorySeparator  + 'lazversion.pas';
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

function TLazarusInstaller.GetLazarusVersionFromUrl(aURL:string):string;
var
  aVersion: string;
begin
  aVersion:=GetVersionFromUrl(aUrl);
  if aVersion='trunk' then
    result:=LAZARUSTRUNKVERSION
  else
    result:=aVersion;
end;

function TLazarusInstaller.GetLazarusReleaseCandidateFromUrl(aURL:string):integer;
begin
  result:=GetReleaseCandidateFromUrl(aURL);
end;


function TLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  Result := true;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (BuildModuleCustom: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
end;

function TLazarusInstaller.GetLazarusVersion: string;
var
  aFileName:string;
begin
  result:='0.0.0';

  aFileName:=IncludeTrailingPathDelimiter(FInstallDirectory) + LAZBUILDNAME + GetExeExt;
  if FileExists(aFileName) then
  begin
    Processor.Executable := aFileName;
    Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
    Processor.Parameters.Clear;
    Processor.Parameters.Add('--version');
    try
      Processor.Execute;
      if Processor.ExitStatus = 0 then
      begin
        if Processor.OutputStrings.Count>0 then
        begin
          // lazbuild outputs version info as last line
          result:=Processor.OutputStrings.Strings[Processor.OutputStrings.Count-1];
        end;
      end;
    except
      on E: Exception do
      begin
        WritelnLog(etError, infotext+'Getting lazbuild version info failed with an exception!'+LineEnding+'Details: '+E.Message,true);
      end;
    end;
  end;

  if result='0.0.0' then result:=GetLazarusVersionFromSource(FSourceDirectory);
  if result='0.0.0' then result:=GetLazarusVersionFromUrl(FURL);
end;


function TLazarusInstaller.InitModule: boolean;
var
  PlainBinPath: string; //the directory above e.g. c:\development\fpc\bin\i386-win32
begin
  Result := true;

  if InitDone then exit;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (InitModule): ';

  infoln(localinfotext+'Entering ...',etDebug);

  if FVerbose then Processor.OnOutputM := @DumpOutput;

  WritelnLog(localinfotext+'Lazarus directory:      ' + FSourceDirectory, false);
  WritelnLog(localinfotext+'Lazarus URL:            ' + FURL, false);
  WritelnLog(localinfotext+'Lazarus options:        ' + FCompilerOptions, false);
  result:=(CheckAndGetTools) AND (CheckAndGetNeededBinUtils);
  if Result then
  begin
    // Look for make etc in the current compiler directory:
    FBinPath := ExcludeTrailingPathDelimiter(ExtractFilePath(FCompiler));
    PlainBinPath := SafeExpandFileName(IncludeTrailingPathDelimiter(FBinPath) + '..'+DirectorySeparator+'..');
    {$IFDEF MSWINDOWS}
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
    // at least one ; to be present in the path. If you only have one entry, you
    // can add PathSeparator without problems.
    // http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
    SetPath(FBinPath + PathSeparator + PlainBinPath + PathSeparator + FMakeDir + PathSeparator +
      ExcludeTrailingPathDelimiter(FSVNDirectory) + PathSeparator + ExcludeTrailingPathDelimiter(FInstallDirectory), false, false);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    SetPath(FBinPath+PathSeparator+
    {$IFDEF DARWIN}
    // pwd is located in /bin ... the makefile needs it !!
    // tools are located in /usr/bin ... the makefile needs it !!
    // don't ask, but this is needed when fpcupdeluxe runs out of an .app package ... quirk solved this way .. ;-)
    '/bin'+PathSeparator+'/usr/bin'+PathSeparator+
    {$ENDIF}
    PlainBinPath, true, false);
    {$ENDIF UNIX}
  end;
  InitDone := Result;
end;

function TLazarusInstaller.BuildModule(ModuleName: string): boolean;
var
  s,CompilerVersion,VersionSnippet:string;
begin
  Result := inherited;
  Result := InitModule;
  if not Result then exit;

  s:=IncludeTrailingPathDelimiter(FSourceDirectory) + MAKEFILENAME;
  if (NOT FileExists(s)) then
  begin
    infoln(infotext+s+' not found. Severe error. Should not happen. Aborting.',etError);
    exit(false);
  end;

  CompilerVersion:=GetCompilerVersion(FCompiler);
  VersionSnippet:=GetLazarusVersionFromSource(FSourceDirectory);
  if VersionSnippet='0.0.0' then VersionSnippet:=GetLazarusVersionFromUrl(FURL);
  if VersionSnippet<>'0.0.0' then
  begin
    FMajorVersion:=0;
    FMinorVersion:=0;
    FReleaseVersion:=0;
    GetVersionFromString(VersionSnippet,FMajorVersion,FMinorVersion,FReleaseVersion);
    FPatchVersion:=GetLazarusReleaseCandidateFromSource(FSourceDirectory);

    // only report once
    if (ModuleName=_LAZBUILD) OR (ModuleName=_LAZARUS) OR ((Self is TLazarusCrossInstaller) AND (ModuleName=_LCL)) then
    begin
      if (Self is TLazarusCrossInstaller) then
      begin
        s:='Lazarus '+CrossCPU_Target+'-'+CrossOS_Target+' cross-builder: ';
      end
      else
      begin
        s:='Lazarus native builder: ';
      end;
      infoln(s+'Detected source version Lazarus: '+VersionSnippet, etInfo);
      infoln(s+'Using FPC compiler with version: '+CompilerVersion, etInfo);
    end;
  end;
  Result := BuildModuleCustom(ModuleName);
end;

function TLazarusInstaller.ConfigModule(ModuleName: string): boolean;
var
  DebuggerPath,DebuggerType: string;
  VersionSnippet: string;
  LazarusConfig: TUpdateLazConfig;
  PCPSnippet: TStringList;
  i,j:integer;
  aFileName:string;
begin
  Result := inherited;
  Result := true;

  if not DirectoryExists(FInstallDirectory) then
  begin
    infoln(infotext+'No Lazarus install directory.',etError);
    exit;
  end;

  //Set GDB as standard debugger
  DebuggerType:='TGDBMIDebugger';

  if DirectoryExists(FPrimaryConfigPath) = false then
  begin
    if ForceDirectoriesSafe(FPrimaryConfigPath) then
      infoln(infotext+'Created Lazarus primary config directory: ' + FPrimaryConfigPath, etInfo);
  end;

  // Lazarus 1.2RC1+ and higher support specifying the primary-config-path that should be used
  // inside the lazarus directory itself.
  PCPSnippet := TStringList.Create;
  try
    // Martin Friebe mailing list January 2014: no quotes allowed, no trailing blanks
    PCPSnippet.Add('--primary-config-path=' + Trim(ExcludeTrailingPathDelimiter(FPrimaryConfigPath)));
    aFileName:=IncludeTrailingPathDelimiter(FInstallDirectory) + LAZARUSCFG;
    if (NOT FileExists(aFileName)) then PCPSnippet.SaveToFile(aFileName);
  finally
    PCPSnippet.Free;
  end;

  // Set up a minimal config so we can use LazBuild
  LazarusConfig := TUpdateLazConfig.Create(FPrimaryConfigPath, FMajorVersion, FMinorVersion, FReleaseVersion, FPatchVersion);
  try
    try
      // Force English language
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Language/ID', 'en');
      // Set Lazarus directory
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/LazarusDirectory/Value', FInstallDirectory);

      {$IFDEF MSWINDOWS}
      // FInstalledCompiler could be something like c:\bla\ppc386.exe, e.g.
      // the platform specific compiler. In order to be able to cross compile
      // we'd rather use fpc
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/CompilerFilename/Value', ExtractFilePath(FCompiler) + 'fpc' + GetExeExt);

      // do we supply GDB in the installdir from mingw for win32 and/or win64
      if FileExists(IncludeTrailingPathDelimiter(FInstallDirectory) + '..\mingw\' + GetFPCTarget(true) + '\bin\gdb.exe') then
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value',
          '$(LazarusDir)\..\mingw\$(TargetCPU)-$(TargetOS)\bin\gdb.exe')

      // have we downloaded GDB in the makedir for win32 and/or win64
      else if FileExists(IncludeTrailingPathDelimiter(FMakeDir) + 'gdb\' + GetFPCTarget(true) + '\gdb.exe') then
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value',
          IncludeTrailingPathDelimiter(FMakeDir) + 'gdb\' + '$(TargetCPU)-$(TargetOS)\gdb.exe')
      else
      begin
        // if no debugger found, just set some default paths
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value',
          IncludeTrailingPathDelimiter(FMakeDir) + 'gdb.exe');
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerSearchPath/Value',
          //'$(LazarusDir)\..\mingw\$(TargetCPU)-$(TargetOS)\bin');
          IncludeTrailingPathDelimiter(FMakeDir) + 'gdb\' + '$(TargetCPU)-$(TargetOS)');
      end;

      if FileExists(ExtractFilePath(FCompiler) + 'make' + GetExeExt)
         then LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', ExtractFilePath(FCompiler) + 'make' + GetExeExt)
         else LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', IncludeTrailingPathDelimiter(FMakeDir) + 'make' + GetExeExt);
      {$ENDIF MSWINDOWS}

      {$IFDEF UNIX}
      // On Unix, FInstalledCompiler should be set to our fpc.sh proxy if installed
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/CompilerFilename/Value', FCompiler);

      {$IF (defined(FREEBSD)) or (defined(Darwin))}
      // Check for newer user-installed debugger (e.g. from ports tree
      // The system gdb is ancient (gdb 6.1.1 in FreeBSD 9) and does not work well with Laz
      DebuggerPath := '/usr/local/bin/gdb';
      if (NOT FileExists(DebuggerPath)) OR (NOT CheckExecutable(DebuggerPath, '--version', 'GNU gdb')) then DebuggerPath := which('gdb');

      {$IF (defined(Darwin))}
      if (NumericalVersion>=CalculateFullVersion(2,0,0)) then
      begin
        if Length(DebuggerPath)=0 then
        begin
          //Check for newest lldb debugger ... does work !!
          DebuggerPath:='/Library/Developer/CommandLineTools/usr/bin/lldb';
          if FileExists(DebuggerPath) then
            DebuggerType:='TLldbDebugger'
          else
            DebuggerPath:='';
        end;
      end;
      {$endif}

      {$ELSE}//other *nix
      DebuggerPath := which('gdb');  //assume in path
      {$ENDIF FREEBSD}

      if Length(DebuggerPath)>0 then
      begin
        LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value', DebuggerPath)
      end
      else
      begin
        infoln(infotext+'No debugger found.' + FPrimaryConfigPath, etWarning);
      end;

      {$IFDEF BSD}
      {$IFDEF DARWIN}
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('make')); //assume in path

      //Available in latest trunk:
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/DisableStartupShell', 'True');

      // extra gdb settings
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/WarnOnTimeOut', 'False');
      // for newer versions Mac OSX versions (>=10.8) perhaps needed:
      //LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerOptions/DebuggerResetAfterRun', 'True');
      if length(DebuggerPath)>0 then
      begin
        // we have a gdb ... check version
        Processor.Executable := DebuggerPath;
        Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);

        Processor.Parameters.Clear;
        Processor.Parameters.Add('--version');
        Processor.Execute;
        if Processor.ExitStatus = 0 then
        begin
          i:=Processor.OutputStrings.Count;
          if i>0 then
          begin
            // gdb outputs version info on first line
            VersionSnippet:=Processor.OutputStrings.Strings[0];
            infoln(infotext+'GDB --version output: ' + VersionSnippet, etInfo);
            // e.g. GNU gdb (GDB) 7.7.1-kjhkjh
            i:=1;
            // move towards first numerical
            while (Length(VersionSnippet)>=i) AND (NOT (VersionSnippet[i] in ['0'..'9'])) do Inc(i);
            j:=0;
            // get only major version
            while (Length(VersionSnippet)>=i) AND (VersionSnippet[i] in ['0'..'9']) do
            begin
              j:=j*10+Ord(VersionSnippet[i])-$30;
              Inc(i);
            end;
            infoln(infotext+'GDB major version: ' + InttoStr(j), etInfo);
            // for newer versions Mac OSX versions (>=10.11) and GDB >= 8.0 [perhaps] needed:
            if j>=8 then LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/Debugger_Startup_Options', '--eval-command="set startup-with-shell off"');
          end;
        end;
      end;
      {$ELSE}//*BSD: FreeBSD, NetBSD, OpenBSD
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('gmake')); //GNU make; assume in path
      {$ENDIF DARWIN}
      {$ENDIF BSD}
      {$IFDEF Solaris}
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('gmake')); //GNU make; assume in path
      {$ENDIF}
      {$ENDIF UNIX}

      // Debugger type needs to be specified at least since Lazarus 1.1
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Debugger/Class', DebuggerType);

      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/FPCSourceDirectory/Value', FFPCSourceDir);
      // Add <lazarus>\docs\xml to fpdoc editor paths
      LazDocPathAdd(IncludeTrailingPathDelimiter(FInstallDirectory) + 'docs'+DirectorySeparator+'xml', LazarusConfig);

      // Enable IDE Coolbar for default docked desktop for (NewPascal) Lazarus with docking
      if LazarusConfig.GetVariable(EnvironmentConfig,'Desktops/Desktop2/Name')='default docked' then
         LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'Desktops/Desktop2/IDECoolBarOptions/Visible/Value', 'True');

      {$IFDEF MSWINDOWS}
      // needed while running Lazarus adds a personal directory that is not valid for other users.
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Count', '2');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item1/Value', 'C:\Windows\Temp\');
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item2/Value', 'C:\Users\Public\Documents');
      {$ENDIF MSWINDOWS}

      // add default projects path
      DebuggerPath := IncludeTrailingPathDelimiter(FBaseDirectory) + 'projects';
      ForceDirectoriesSafe(DebuggerPath);
      LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/Value', IncludeTrailingPathDelimiter(DebuggerPath));

      // Set file history towards default project directory
      LazarusConfig.SetVariableIfNewFile(History, 'InputHistory/FileDialog/InitialDir', IncludeTrailingPathDelimiter(DebuggerPath));


      {$IFDEF DARWIN}
      {$IFDEF CPUX86_64}
      {$IFDEF LCLCOCOA}
      // Prevent crash on Darwin Cocoa: set and make available initial project
      aFileName:=IncludeTrailingPathDelimiter(DebuggerPath)+'project1.lpi';

      // Create a default project
      SysUtils.DeleteFile(aFileName);
      SysUtils.DeleteFile(ChangeFileExt(aFileName,'.lpr'));
      PCPSnippet:=TStringList.Create;
      try
        PCPSnippet.Clear;
        PCPSnippet.Text:=DEFAULTLPI;
        PCPSnippet.SaveToFile(aFileName);
        PCPSnippet.Clear;
        PCPSnippet.Text:=DEFAULTLPR;
        PCPSnippet.SaveToFile(ChangeFileExt(aFileName,'.lpr'));
      finally
        PCPSnippet.Free;
      end;

      if FileExists(aFileName) then
      begin
        LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Recent/AlreadyPopulated', 'True');
        LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Recent/ProjectFiles/Count', '1');
        LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/Recent/ProjectFiles/Item1/Value', aFileName);
        LazarusConfig.SetVariableIfNewFile(EnvironmentConfig, 'EnvironmentOptions/AutoSave/LastSavedProjectFile', aFileName);
      end;
      {$ENDIF LCLCOCOA}
      {$ENDIF CPUX86_64}
      {$ENDIF DARWIN}

    except
      on E: Exception do
      begin
        Result := false;
        infoln('Error setting Lazarus config: ' + E.ClassName + '/' + E.Message, eterror);
      end;
    end;
  finally
    LazarusConfig.Free;
  end;
end;

function TLazarusInstaller.CleanModule(ModuleName: string): boolean;
var
  {$ifdef MSWINDOWS}
  CrossWin: boolean;
  LHelpTemp: string; // LHelp gets copied to this temp file
  {$endif}
  oldlog: TErrorMethod;
  CleanCommand,CleanDirectory:string;
  CrossCompiling: boolean;
  {
  DeleteList: TStringList;
  CPUOS_Signature:string;
  }
begin
  Result := inherited;

  if not DirectoryExists(FSourceDirectory) then
  begin
    infoln(infotext+'No Lazarus source [yet] ... nothing to be done',etInfo);
    exit(true);
  end;

  Result := InitModule;

  if not Result then exit;

  CrossCompiling:=(Self is TLazarusCrossInstaller);
  //CrossCompiling:=Assigned(CrossInstaller);

  // If cleaning primary config:
  if ((NOT CrossCompiling) and (ModuleName=_LAZARUS)) then
  begin
    //infoln(infotext+'If your primary config path has changed, you may want to remove ' + IncludeTrailingPathDelimiter(
    //  FInstallDirectory) + 'lazarus.cfg which points to the primary config path.', etInfo);
    infoln(infotext+'Deleting Lazarus primary config file ('+LAZARUSCFG+').', etInfo);
    DeleteFile(IncludeTrailingPathDelimiter(FInstallDirectory) + LAZARUSCFG);
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
    if (CrossInstaller.TargetCPU = 'x86_64') and ((CrossInstaller.TargetOS = 'win64') or (CrossInstaller.TargetOS = 'win32')) then
      CrossWin := true;
    {$endif win32}
    {$ifdef win64}
    // if this is crosswin64-32, ignore error as it is optional
    if (CrossInstaller.TargetCPU = 'i386') and (CrossInstaller.TargetOS = 'win32') then
      CrossWin := true;
    {$endif win64}
    if CrossWin then
    begin
      LHelpTemp:=GetTempFileName('','');
      try
        CopyFile(
          IncludeTrailingPathDelimiter(FInstallDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
          LHelpTemp,[cffOverWriteFile]);
      except
        infoln(infotext+'Non-fatal error copying lhelp to temp file '+LHelpTemp,etInfo);
      end;
    end;
  end;
  {$endif MSWINDOWS}

  // Make distclean; we don't care about failure (e.g. directory might be empty etc)
  oldlog := Processor.OnErrorM;
  Processor.OnErrorM := nil;  //don't want to log errors in distclean

  Processor.Executable := Make;
  Processor.Parameters.Clear;
  {$IFDEF MSWINDOWS}
  if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
  {$ENDIF}
  Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
  {
  //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
  if (FNoJobs) then
    Processor.Parameters.Add('--jobs=1')
  else
    Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));}
  Processor.Parameters.Add('FPC=' + FCompiler);
  Processor.Parameters.Add('PP=' + ExtractFilePath(FCompiler)+GetCompilerName(GetTargetCPU));
  Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
  {$ifdef Windows}
  Processor.Parameters.Add('UPXPROG=echo');      //Don't use UPX
  Processor.Parameters.Add('COPYTREE=echo');     //fix for examples in Win svn, see build FAQ
  {$endif}
  Processor.Parameters.Add('OS_SOURCE=' + GetTargetOS);
  Processor.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);

  CleanDirectory:='';
  CleanCommand:='';

  case ModuleName of
    _IDE:
    begin
      CleanCommand:='cleanide';
      CleanDirectory:=DirectorySeparator+'ide';
    end;
    _BIGIDE: CleanCommand:='cleanbigide';
    _LAZARUS: CleanCommand:='distclean';
    _LCL:
    begin
      CleanDirectory:=DirectorySeparator+'lcl';
      if (Self is TLazarusCrossInstaller) AND (FCrossLCL_Platform <> '') then
      begin
        Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
        CleanCommand:='cleanintf';
      end
      else
      begin
        CleanCommand:='clean';
      end;
    end;
    _COMPONENTS:
    begin
      CleanDirectory:=DirectorySeparator+'components';
      if (Self is TLazarusCrossInstaller) AND (FCrossLCL_Platform <> '') then
      begin
        Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
      end;
      CleanCommand:='clean';
    end;
    _PACKAGER:
    begin
      CleanDirectory:=DirectorySeparator+'packager';
      if (Self is TLazarusCrossInstaller) AND (FCrossLCL_Platform <> '') then
      begin
        Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
      end;
      CleanCommand:='clean';
    end;
    _LCLCROSS:
    begin
      CleanDirectory:=DirectorySeparator+'lcl';
      if (LCLCrossActionNeeded) then
      begin
        Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
        CleanCommand:='cleanintf';
      end
      else
      begin
        infoln(infotext+'No extra LCL_PLATFORM defined ... nothing to be done', etInfo);
        Result := true;
        exit;
      end;
    end;
    else //raise error;
    begin
      WritelnLog(etError, infotext+'Invalid module name [' + ModuleName + '] specified! Please fix the code.', true);
    end;
  end;

  if (Self is TLazarusCrossInstaller) then
  begin
    Processor.Parameters.Add('OS_TARGET=' + CrossOS_Target);
    Processor.Parameters.Add('CPU_TARGET=' + CrossCPU_Target);
  end
  else
  begin
    Processor.Parameters.Add('OS_TARGET=' + GetTargetOS);
    Processor.Parameters.Add('CPU_TARGET=' + GetTargetCPU);
  end;

  CleanDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory)+CleanDirectory;

  Processor.Parameters.Add('--directory=' + CleanDirectory);
  Processor.Parameters.Add(CleanCommand);

  if (Self is TLazarusCrossInstaller) then
    infoln(infotext+'Running "make '+CleanCommand+'" twice inside '+CleanDirectory+' for OS_TARGET='+CrossOS_Target+' and CPU_TARGET='+CrossCPU_Target,etInfo)
  else
    infoln(infotext+'Running "make '+CleanCommand+'" twice inside '+CleanDirectory,etInfo);

  try
    writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
    Processor.Execute;
    sleep(100); //now do it again:
    writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
    Processor.Execute;
    Result := true;
  except
    on E: Exception do
    begin
      Result := false;
      WritelnLog(infotext+'Failed with an exception!' + LineEnding + 'Details: ' + E.Message, true);
    end;
  end;
  Processor.OnErrorM := oldlog; //restore previous logging

  {$ifdef MSWINDOWS}
  // Now try to restore lhelp
  if LHelpTemp<>'' then
  begin
    try
      CopyFile(
        LHelpTemp,
        IncludeTrailingPathDelimiter(FInstallDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
        [cffOverWriteFile]);
    except
      infoln(infotext+'Non-fatal error restoring lhelp from temp file '+LHelpTemp,etInfo);
    end;
  end;
  {$endif MSWINDOWS}

  {
  // finally ... if something is still still still floating around ... delete it !!
  CrossCompiling:=(Self is TLazarusCrossInstaller);
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
    DeleteFilesExtensionsSubdirs(FSourceDirectory,DeleteList,CPUOS_Signature);
  finally
    DeleteList.Free;
  end;
  }
end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
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
              if (FpReadLink(FrameworkDir+'/'+FrameworkName) = '') then DeleteFile(FrameworkDir+'/'+FrameworkName);
            end;

            if (NOT FileExists(FrameworkDir+'/'+FrameworkName)) then
            begin
              // create the symlink towards the base framework library
              success:=(FPSymLink(PChar(FileToLink),PChar(FrameworkDir+'/'+FrameworkName))=0);
              if NOT success then
              begin
                result:=false;
                infoln(infotext+'Symlink creation failure for '+FrameworkName,etError);
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

const
  // needs to be exactly the same as used by Lazarus !!!
  //RevisionIncComment = '// Created by FPCLAZUP';
  RevisionIncComment = '// Created by Svn2RevisionInc';
  ConstName = 'RevisionStr';
  RevisionIncFileName = 'revision.inc';

  LAZBUILDHACKMAGIC=
    'useride: '+LineEnding+
    'ifdef LAZBUILDJOBS'+LineEnding+
    'ifdef LCL_PLATFORM'+LineEnding+
    #9+'./lazbuild$(SRCEXEEXT) --max-process-count=$(LAZBUILDJOBS) --lazarusdir=. --build-ide= --ws=$(LCL_PLATFORM)'+LineEnding+
    'else'+LineEnding+
    #9+'./lazbuild$(SRCEXEEXT) --max-process-count=$(LAZBUILDJOBS) --lazarusdir=. --build-ide='+LineEnding+
    'endif'+LineEnding+
    'else'+LineEnding+
    'ifdef LCL_PLATFORM'+LineEnding+
    #9+'./lazbuild$(SRCEXEEXT) --lazarusdir=. --build-ide= --ws=$(LCL_PLATFORM)'+LineEnding+
    'else'+LineEnding+
    #9+'./lazbuild$(SRCEXEEXT) --lazarusdir=. --build-ide='+LineEnding+
    'endif'+LineEnding+
    'endif';

var
  AfterRevision: string;
  BeforeRevision: string;
  UpdateWarnings: TStringList;
  HackMagic:TStringList;
  RevisionIncText: Text;
  ConstStart: string;
  aRepoClient:TRepoClient;
  VersionSnippet:string;
  MakeFilePath:string;
  aIndex,aHackIndex:integer;
  s1,s2:string;
  {$ifdef BSD}
  FilePath:string;
  {$endif}
begin
  Result := inherited;
  Result := InitModule;

  if not Result then exit;

  aRepoClient:=GetSuitableRepoClient;

  infoln(infotext+'Start checkout/update of ' + ModuleName + ' sources.',etInfo);

  UpdateWarnings := TStringList.Create;
  try
    aRepoClient.Verbose:=FVerbose;
    aRepoClient.ExportOnly:=FExportOnly;
    aRepoClient.ModuleName:=ModuleName;
    if aRepoClient=FGitClient
       then result:=DownloadFromGit(ModuleName,BeforeRevision, AfterRevision, UpdateWarnings)
       else result:=DownloadFromSVN(ModuleName,BeforeRevision, AfterRevision, UpdateWarnings);
  finally
    UpdateWarnings.Free;
  end;

  if NOT aRepoClient.ExportOnly then
  begin
    infoln(infotext+'Lazarus was at: ' + BeforeRevision, etInfo);

    if FRepositoryUpdated then
    begin
      Revision := AfterRevision;
      infoln(infotext+'Lazarus is now at: ' + AfterRevision, etInfo);
    end
    else
    begin
      Revision := BeforeRevision;
      infoln(infotext+'No updates for Lazarus found.', etInfo);
    end;
  end
  else
  begin
    Revision := AfterRevision;
    infoln(infotext+'Lazarus is now at: ' + AfterRevision, etInfo);
  end;

  if (Result) then
  begin
    // update revision.inc;
    infoln(infotext+'Updating Lazarus version info.', etInfo);
    AssignFile(RevisionIncText, IncludeTrailingPathDelimiter(FSourceDirectory)+'ide'+PathDelim+RevisionIncFileName);
    try
      Rewrite(RevisionIncText);
      writeln(RevisionIncText, RevisionIncComment);
      ConstStart := Format('const %s = ''', [ConstName]);
      writeln(RevisionIncText, ConstStart, aRepoClient.LocalRevision, ''';');
    finally
      CloseFile(RevisionIncText);
    end;
  end;

  if (NOT Result) then
    infoln(infotext+'Checkout/update of ' + ModuleName + ' sources failure.',etError);

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
  infoln(infotext+'Adding QT5 binary sources (QT5 + QT5Pas Frameworks + libqcocoa) from fpcupdeluxe.app itself.',etInfo);

  // copy QT5 frameworks to Lazarus source directory for future use.
  if DirCopy(FilePath+'/Contents/Frameworks',ExcludeTrailingPathDelimiter(FBaseDirectory)+'/Frameworks') then
  begin
    CreateQT5Symlinks(ExcludeTrailingPathDelimiter(FBaseDirectory)+'/Frameworks');
    infoln(infotext+'Adding QT5 Frameworks to ' + ExcludeTrailingPathDelimiter(FBaseDirectory)+'/Frameworks' + ' success.',etInfo);
  end else infoln(infotext+'Adding QT5 Frameworks to ' + ExcludeTrailingPathDelimiter(FBaseDirectory)+'/Frameworks' + ' failure.',etError);

  // copy QT5 frameworks to lazarus.app ... a bit redundant ... :-(
  if DirCopy(FilePath+'/Contents/Frameworks',ExcludeTrailingPathDelimiter(FSourceDirectory)+'/lazarus.app/Contents/Frameworks') then
  begin
    CreateQT5Symlinks(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/lazarus.app/Contents/Frameworks');
    infoln(infotext+'Adding QT5 Frameworks to lazarus.app success.',etInfo);
  end else infoln(infotext+'Adding QT5 Frameworks to lazarus.app failure.',etError);

  // copy QT5 frameworks to startlazarus.app ... a bit redundant ... :-(
  if DirCopy(FilePath+'/Contents/Frameworks',ExcludeTrailingPathDelimiter(FSourceDirectory)+'/startlazarus.app/Contents/Frameworks') then
  begin
    CreateQT5Symlinks(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/startlazarus.app/Contents/Frameworks');
    infoln(infotext+'Adding QT5 Frameworks to startlazarus.app success.',etInfo);
  end  else infoln(infotext+'Adding QT5 Frameworks to startlazarus.app failure.',etError);
  CreateQT5Symlinks(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/lazarus.app');

  // QT5 quirk: copy QT5 libqcocoa.dylib to lazarus.app
  if DirCopy(FilePath+'/Contents/Plugins',ExcludeTrailingPathDelimiter(FSourceDirectory)+'/lazarus.app/Contents/Plugins')
    then infoln(infotext+'Adding QT5 libqcocoa.dylib success.',etInfo)
    else infoln(infotext+'Adding QT5 libqcocoa.dylib failure.',etError);

  // QT5 quirk: copy QT5 libqcocoa.dylib to startlazarus.app
  if DirCopy(FilePath+'/Contents/Plugins',ExcludeTrailingPathDelimiter(FSourceDirectory)+'/startlazarus.app/Contents/Plugins')
    then infoln(infotext+'Adding QT5 libqcocoa.dylib success.',etInfo)
    else infoln(infotext+'Adding QT5 libqcocoa.dylib failure.',etError);
  {$endif}
  {$endif}

  (*
  Errors := 0;
  if (Result) and (Uppercase(FCrossLCL_Platform) = 'QT') then
  begin
    for Counter := low(FUtilFiles) to high(FUtilFiles) do
    begin
      if (FUtilFiles[Counter].Category = ucQtFile) and not
        (FileExists(IncludeTrailingPathDelimiter(FSourceDirectory) + FUtilFiles[Counter].FileName)) then
      begin
        infoln(infotext+'Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + FSourceDirectory, etDebug);
        try
          if Download(FUseWget, FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName, IncludeTrailingPathDelimiter(FSourceDirectory) +
            FUtilFiles[Counter].FileName, FHTTPProxyHost, FHTTPProxyPort, FHTTPProxyUser,
            FHTTPProxyPassword) = false then
          begin
            Errors := Errors + 1;
            infoln(infotext+'Error downloading Qt-related file to ' + IncludeTrailingPathDelimiter(FSourceDirectory) +
              FUtilFiles[Counter].FileName, eterror);
          end;
        except
          on E: Exception do
          begin
            Result := false;
            infoln(infotext+'Error downloading Qt-related files: ' + E.Message, etError);
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
  FilePath:=IncludeTrailingPathDelimiter(FSourceDirectory)+'ide/include/';
  if (NOT DirectoryExists(FilePath+'dragonfly')) then
  begin
    if DirCopy(FilePath+'netbsd',FilePath+'dragonfly')
      then infoln(infotext+'Adding dragonfly include file for IDE.',etInfo)
      else infoln(infotext+'Adding dragonfly include file for IDE failure.',etError);
  end;
  {$endif}

  if result then
  begin
    VersionSnippet:=GetLazarusVersionFromSource(FSourceDirectory);
    if VersionSnippet='0.0.0' then GetLazarusVersionFromURL(FURL);
    if VersionSnippet<>'0.0.0' then
    begin
      FMajorVersion:=0;
      FMinorVersion:=0;
      FReleaseVersion:=0;
      GetVersionFromString(VersionSnippet,FMajorVersion,FMinorVersion,FReleaseVersion);
      FPatchVersion:=GetLazarusReleaseCandidateFromSource(FSourceDirectory);
    end;
    PatchModule(ModuleName);
  end;

  //if false then
  if result then
  begin
    MakeFilePath:=IncludeTrailingPathDelimiter(FSourceDirectory)+MAKEFILENAME;
    if FileExists(MakeFilePath) then
    begin
      UpdateWarnings:=TStringList.Create;
      try
        UpdateWarnings.LoadFromFile(MakeFilePath);
        aIndex:=(UpdateWarnings.Count-1);
        while (aIndex>=0) do
        begin
          s1:=ExtractWord(1,UpdateWarnings.Strings[aIndex],[' ']);
          s1:=TrimRight(s1);
          if s1='useride:' then
          begin
            //remove everything of the useride macro definition
            while (aIndex<UpdateWarnings.Count) do
            begin
              UpdateWarnings.Delete(aIndex);
              s1:=ExtractWord(1,UpdateWarnings.Strings[aIndex],[' ']);
              s1:=TrimRight(s1);
              //this will stop at the next macro definition [starter:]
              if s1[Length(s1)]=':' then break;
            end;
            break;
          end;
          Dec(aIndex);
        end;

        aHackIndex:=aIndex;

        //replace the useride macro with the new one
        HackMagic:=TStringList.Create;
        try
          HackMagic.Text:=LAZBUILDHACKMAGIC;
          for aIndex:=(HackMagic.Count-1) downto 0 do
          begin
            UpdateWarnings.Insert(aHackIndex,HackMagic.Strings[aIndex]);
          end;
        finally
          HackMagic.Free;
        end;

        UpdateWarnings.SaveToFile(MakeFilePath);

      finally
        UpdateWarnings.Free;
      end;

    end;
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
  Result := InitModule;

  if not Result then exit;

  //sanity check
  if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory) + MAKEFILENAME) and DirectoryExists(
    IncludeTrailingPathDelimiter(FSourceDirectory) + 'ide') and DirectoryExists(IncludeTrailingPathDelimiter(FSourceDirectory) + 'lcl') and
    ParentDirectoryIsNotRoot(IncludeTrailingPathDelimiter(FSourceDirectory)) then
  begin
    Result := DeleteDirectoryEx(FSourceDirectory);
    if not (Result) then
      WritelnLog(infotext+'Error deleting Lazarus directory ' + FSourceDirectory);
  end
  else
  begin
    WritelnLog(infotext+'Error: invalid Lazarus directory :' + FSourceDirectory);
    Result := false;
  end;

  // Sanity check so we don't try to delete random directories
  // Assume Lazarus has been configured/run once so enviroronmentoptions.xml exists.
  if Result and FileExists(IncludeTrailingPathDelimiter(FPrimaryConfigPath) + EnvironmentConfig) and
    ParentDirectoryIsNotRoot(IncludeTrailingPathDelimiter(FPrimaryConfigPath)) then
  begin
    Result := DeleteDirectoryEx(FPrimaryConfigPath) = false;
    if not (Result) then
      WritelnLog(infotext+'Error deleting Lazarus PrimaryConfigPath directory ' + FPrimaryConfigPath);
  end
  else
  begin
    WritelnLog(infotext+'Error: invalid Lazarus FPrimaryConfigPath: ' + FPrimaryConfigPath);
    Result := false;
  end;
end;

function TLazarusInstaller.LCLCrossActionNeeded:boolean;
var
  NothingToBeDone:boolean;
begin
  NothingToBeDone:=true;
  if FCrossLCL_Platform<>'' then
  begin
    NothingToBeDone:=false;
    {$ifdef Darwin}
      {$ifdef LCLCARBON}
        NothingToBeDone:=(FCrossLCL_Platform='carbon');
      {$endif}
      {$ifdef LCLCOCOA}
        NothingToBeDone:=(FCrossLCL_Platform='cocoa');
      {$endif}
      {$ifdef CPU64}
        {$ifndef LCLQT5}
          NothingToBeDone:=(FCrossLCL_Platform='cocoa');
        {$endif}
      {$endif}
    {$endif}
    {$ifdef LCLQT}
      NothingToBeDone:=(FCrossLCL_Platform='qt');
    {$endif}
    {$ifdef LCLQT5}
      NothingToBeDone:=(FCrossLCL_Platform='qt5');
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

