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
    'Declare lazarus;' +
    'Cleanmodule lazarus;' +
    'Checkmodule lazarus;' +
    'Getmodule lazarus;' +
    'Buildmodule lazbuild;' +
    'ConfigModule lazarus;' +
    'Do helplazarus;'+
    'Do UniversalDefault;'+
    'Do USERIDE;'+
    'Exec CreateLazarusScript;' +
    'End;' +

    'Declare oldlazarus;' +
    'Cleanmodule lazarus;' +
    'Checkmodule lazarus;' +
    'Getmodule lazarus;' +
    'Buildmodule lazarus;' +
    'ConfigModule lazarus;' +
    'Exec CreateLazarusScript;' +
    'End;' +

    //Nogui widgetset+Lazbuild:
    'Declare lazbuild;' +
    'Cleanmodule lazarus;' +
    'Checkmodule lazarus;' +
    'Getmodule lazarus;' +
    'Buildmodule lazbuild;' +
    //config lazarus, so lazbuild will work:
    'ConfigModule lazarus;' +
    'End;' +

    //standard IDE build with user-selected packages
    // assumes/requires that Laz svn has already been updated
    // also we need lazbuild, but we can check for it in our USERIDE code.
    // If we Require it here, it will kick off a lazbuild build cycle that
    // may already have been done.
    'Declare USERIDE;' +
    'Buildmodule USERIDE;' +
    // Make sure the user can use the IDE:
    'Exec CreateLazarusScript;' + 'End;' +

    //standard uninstall
    'Declare lazarusuninstall;' + 'Uninstallmodule lazarus;' + 'Exec DeleteLazarusScript;' + 'End;' +

    //standard clean
    'Declare lazarusclean;' + 'Cleanmodule lazarus;' + 'End;' +

    //selective actions triggered with --only=SequenceName
    'Declare LazarusCheckOnly;' + 'Checkmodule lazarus;' + 'End;' +
    'Declare LazarusCleanOnly;' + 'Cleanmodule lazarus;' + 'End;' +
    'Declare LazarusGetOnly;' + 'Getmodule lazarus;' + 'End;' +
    'Declare LazarusBuildOnly;' + 'Buildmodule lazarus;' + 'End;' +
    'Declare LazarusConfigOnly;' + 'Configmodule lazarus;' + 'End;' +
    'Declare LazCleanAndBuildOnly;' +
    'Cleanmodule lazarus;' +
    'Buildmodule lazbuild;' +
    'ConfigModule lazarus;' +
    'Do UniversalDefault;'+
    'Do USERIDE;'+
    'End;' +

    // Compile only LCL
    'Declare LCL;' +
    'CleanModule LCL;'+
    'Buildmodule LCL;' +
    'End;' +

    {$ifdef win32}
    // Crosscompile build
    'Declare LazarusCrossWin32-64;' +
    'SetCPU x86_64;' + 'SetOS win64;' +
    'Do LCL;'+
    'End;' +
    {$endif}
    {$ifdef win64}
    'Declare LazarusCrossWin64-32;' +
    'SetCPU i386;'+ 'SetOS win32;'+
    'Do LCL;'+
    'End;' +
    {$endif}

    // Crosscompile only LCL native widgetset (needs to be run at end
    'Declare LCLCross;' +
    'ResetLCL;' + //module code itself will select proper widgetset
    'CleanModule LCLCross;'+
    'Buildmodule LCLCross;' +
    'End';

type

  { TLazarusInstaller }

  TLazarusInstaller = class(TInstaller)
  private
    FBinPath: string; //path where compiler lives
    FCrossLCL_Platform: string;
    FPrimaryConfigPath: string;
    FRevision: string;
    InitDone: boolean;
  protected
    FFPCInstallDir: string;
    FFPCSourceDir: string;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; virtual;
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
  public
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;



implementation

uses
  fpcuputil, fileutil,
  repoclient,
  lazfileutils {utf8 file functions},
  updatelazconfig
  {$IFDEF UNIX}
  , baseunix
  {$ENDIF UNIX}  ;

{ TLazarusCrossInstaller }

function TLazarusCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  CrossInstaller: TCrossInstaller;
  Options: string;
  LazBuildApp: string;
begin
  Result:=inherited;

  CrossInstaller := GetCrossInstaller;

  FErrorLog.Clear;

  if Assigned(CrossInstaller) then
  begin
    // Actually not using crossopts - they're only for building an FPC compiler; the
    // relevant options should have been written as a snippet to fpc.cfg and picked
    // up from there.
    CrossInstaller.SetCrossOpt(CrossOPT); //pass on user-requested cross compile options
    CrossInstaller.SetSubArch(CrossOS_SubArch);
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

      LazBuildApp := IncludeTrailingPathDelimiter(FInstallDirectory) + 'lazbuild' + GetExeExt;
      if CheckExecutable(LazBuildApp, '--help', 'lazbuild') = false then
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
        Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
        Processor.Parameters.Clear;
        {$IFDEF lazarus_parallel_make}
        if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
        {$ENDIF}
        Processor.Parameters.Add('FPC=' + FCompiler);
        Processor.Parameters.Add('USESVN2REVISIONINC=0');
        Processor.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
        Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
        Processor.Parameters.Add('FPCDIR=' + FFPCSourceDir); //Make sure our FPC units can be found by Lazarus
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath <> '' then
          Processor.Parameters.Add('CROSSBINDIR=' + ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
        Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX

        Processor.Parameters.Add('OS_SOURCE=' + GetTargetOS);
        Processor.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);
        Processor.Parameters.Add('CPU_TARGET=' + FCrossCPU_Target);
        Processor.Parameters.Add('OS_TARGET=' + FCrossOS_Target);

        if FCrossLCL_Platform <> '' then
          Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);

        Options := FCompilerOptions;
        if CrossInstaller.LibsPath <> '' then
          Options := Options + ' -Xd -Fl' + CrossInstaller.LibsPath;
        if CrossInstaller.BinUtilsPrefix <> '' then
        begin
          Options := Options + ' -XP' + CrossInstaller.BinUtilsPrefix;
          Processor.Parameters.Add('BINUTILSPREFIX=' + CrossInstaller.BinUtilsPrefix);
        end;
        Options:=StringReplace(Options,'  ',' ',[rfReplaceAll]);
        Options:=Trim(Options);
        Processor.Parameters.Add('OPT=' + STANDARDCOMPILEROPTIONS + ' ' + Options);
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
        Processor.Parameters.Add('--pcp=' + FPrimaryConfigPath);

        // Apparently, the .compiled file, that are used to check for a rebuild, do not contain a cpu setting if cpu and cross-cpu do not differ !!
        // So, use this test to prevent a rebuild !!!
        if (GetTargetCPU<>FCrossCPU_Target) then
          Processor.Parameters.Add('--cpu=' + FCrossCPU_Target);

        // See above: the same for OS !
        if (GetTargetOS<>FCrossOS_Target) then
          Processor.Parameters.Add('--os=' + FCrossOS_Target);

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
        infoln(infotext+'Compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + ' using ' + ExtractFileName(Processor.Executable), etInfo)
      else
        infoln(infotext+'Compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + '/' + FCrossLCL_Platform + ' using ' + ExtractFileName(Processor.Executable), etInfo);

      try
        writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
        Processor.Execute;
        Result := Processor.ExitStatus = 0;
        if not Result then
          WritelnLog(etError,infotext+'Error compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + ' ' + FCrossLCL_Platform + LineEnding +
            'Details: ' + FErrorLog.Text, true);
      except
        on E: Exception do
        begin
          Result := false;
          WritelnLog(etError,infotext+'Exception compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + LineEnding +
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
          infoln(infotext+'Cross compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target +
            ' failed. Optional module; continuing regardless.', etWarning)
        else
          infoln(infotext+'Cross compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + ' failed.', etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(Result);
      end;
    end; //prereqs in place

  end    //valid cross compile setup
  else
    infoln(infotext+'Can''t find cross installer for ' + FCrossCPU_Target + '-' + FCrossOS_Target, etError);
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
  LazBuildApp: string;
  OperationSucceeded: boolean;
  sCmpOpt: string;
  NothingToBeDone:boolean;
  LazarusConfig: TUpdateLazConfig;
begin
  Result:=inherited;

  OperationSucceeded := true;

  if ModuleName <> 'USERIDE' then
  begin
    // Make all (should include lcl & ide), lazbuild, lcl etc
    // distclean was already run; otherwise specify make clean all
    FErrorLog.Clear;
    Processor.Executable := Make;
    Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
    Processor.Parameters.Clear;
    {$IFDEF lazarus_parallel_make}
    if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
    {$ENDIF}
    Processor.Parameters.Add('FPC=' + FCompiler);
    Processor.Parameters.Add('USESVN2REVISIONINC=0');
    Processor.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
    Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
    Processor.Parameters.Add('FPCDIR=' + FFPCSourceDir); //Make sure our FPC units can be found by Lazarus
    Processor.Parameters.Add('UPXPROG=echo');      //Don't use UPX
    Processor.Parameters.Add('COPYTREE=echo');     //fix for examples in Win svn, see build FAQ
    // replace -g by -gw if encountered: http://lists.lazarus.freepascal.org/pipermail/lazarus/2015-September/094238.html
    sCmpOpt:=StringReplace(FCompilerOptions,'-g ','-gw ',[]);
    sCmpOpt:=StringReplace(sCmpOpt,'  ',' ',[rfReplaceAll]);
    sCmpOpt:=Trim(sCmpOpt);
    Processor.Parameters.Add('OPT=' + STANDARDCOMPILEROPTIONS + ' ' + sCmpOpt);

    case UpperCase(ModuleName) of
      'IDE':
      begin
        Processor.Parameters.Add('idepkg');
        infoln(infotext+'Running make idepkg', etInfo);
      end;
      'BIGIDE':
      begin
        Processor.Parameters.Add('idebig');
        infoln(infotext+'Running make idebig', etInfo);
      end;
      'LAZARUS':
      begin
        Processor.Parameters.Add('all');
        infoln(infotext+'Running make all', etInfo);
      end;
      'LAZBUILD':
      begin
        Processor.Parameters.Add('lazbuild');
        infoln(infotext+'Running make lazbuild', etInfo);
      end;
      'LCL':
      begin
        // April 2012: lcl now requires lazutils and registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        // http://lists.lazarus-ide.org/pipermail/lazarus/2012-April/138168.html
        Processor.Parameters.Add('registration');
        Processor.Parameters.Add('lazutils');
        Processor.Parameters.Add('lcl');
        // always build standard LCL for native system ... other widgetsets to be done by LCLCROSS: see below
        //if FCrossLCL_Platform<>'' then Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
        infoln(infotext+'Running make registration lazutils lcl', etInfo);
      end;
      'LCLCROSS':
      begin
        NothingToBeDone:=true;
        if FCrossLCL_Platform<>'' then
        begin
          NothingToBeDone:=false;
          {$ifdef Darwin}
            {$ifdef LCLCOCOA}
              NothingToBeDone:=(FCrossLCL_Platform='cocoa');
            {$else}
              {$ifdef CPUX64}
                {$ifdef LCLQT5}
                  NothingToBeDone:=(FCrossLCL_Platform='qt5');
                {$else}
                  NothingToBeDone:=(FCrossLCL_Platform='cocoa');
                {$endif}
              {$endif}
            {$endif}
          {$endif}
        end;
        if (NOT NothingToBeDone) then
        begin
          // first: Processor.Parameters.Add('-C lcl'+DirectorySeparator+'interfaces'+DirectorySeparator+FCrossLCL_Platform);
          // followed by: make ideintf basecomponents bigidecomponents LCL_PLATFORM=qt
          Processor.Parameters.Add('-C lcl');
          Processor.Parameters.Add('intf');
          //Processor.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
          infoln(infotext+'Running make -C lcl intf', etInfo);
        end
        else
        begin
          // nothing to be done: exit graceously
          infoln(infotext+'No extra LCL_PLATFORM defined ... nothing to be done', etInfo);
          OperationSucceeded := true;
          Result := true;
          exit;
        end;
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
      writelnlog(infotext+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
      Processor.Execute;
      ExitCode := Processor.ExitStatus;
      if ExitCode <> 0 then
      begin
        OperationSucceeded := false;
        Result := false;
        WritelnLog(etError, infotext+'Error running make!' + LineEnding + 'Details: exit code ' + IntToStr(ExitCode), true);
      end;
    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        Result := false;
        WritelnLog(etError, infotext+'Exception running make!' + LineEnding + 'Details: ' + E.Message, true);
      end;
    end;

    //Special check for lazbuild as that is known to go wrong
    if (OperationSucceeded) and (UpperCase(ModuleName) = 'LAZBUILD') then
    begin
      if CheckExecutable(IncludeTrailingPathDelimiter(FInstallDirectory) + 'lazbuild' + GetExeExt, '--help', 'lazbuild') = false then
      begin
        writelnlog(etError, infotext+'Lazbuild could not be found, so cannot build USERIDE.', true);
        Result := false;
        exit;
      end;
    end;

  end
  else
  begin
    // useride; using lazbuild. Note: in recent Lazarus we could also run make lazbuild useride
    // ... but that apparently calls lazbuild internally anyway.

    // Check for valid lazbuild.
    // Note: we don't check if we have a valid primary config path, but that will come out
    // in the next steps.
    LazBuildApp := IncludeTrailingPathDelimiter(FInstallDirectory) + 'lazbuild' + GetExeExt;
    if CheckExecutable(LazBuildApp, '--help', 'lazbuild') = false then
    begin
      writelnlog(etError, infotext+'Lazbuild could not be found, so cannot build USERIDE.', true);
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
      {$IFDEF DEBUG}
      Processor.Parameters.Add('--verbose');
      {$ELSE}
      // See compileroptions.pp
      // Quiet:=ConsoleVerbosity<=-3;
      Processor.Parameters.Add('--quiet');
      {$ENDIF}
      Processor.Parameters.Add('--pcp=' + FPrimaryConfigPath);
      Processor.Parameters.Add('--cpu=' + GetTargetCPU);
      Processor.Parameters.Add('--os=' + GetTargetOS);

      if FCrossLCL_Platform <> '' then
        Processor.Parameters.Add('--ws=' + FCrossLCL_Platform);

      // Support keeping userdefined installed packages when building.
      // Compile with selected compiler options
      // Assume new Laz version on failure getting revision
      if strtointdef(Revision, 38971) >= 38971 then
      begin
        Processor.Parameters.Add('--build-ide=-dKeepInstalledPackages ' + FCompilerOptions);
      end
      else
      begin
        // Fallback - depends on hardcoded "Normal IDE" build mode being present
        // We can specify a build mode; otherwise probably the latest build mode will be used
        // which could well be a stripped IDE
        // Let's see how/if FCompilerOptions clashes with the settings in normal build mode
        writelnlog(infotext+'LazBuild: building UserIDE but falling back to --build-mode="Normal IDE"', true);
        Processor.Parameters.Add('--build-ide= ' + FCompilerOptions);
        Processor.Parameters.Add('--build-mode="Normal IDE"');
      end;

      // Run first time...
      if OperationSucceeded then
      begin
        infoln(infotext+'Running lazbuild to get IDE with user-specified packages', etInfo);
        try
          writelnlog(infotext+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
          Processor.Execute;
          if Processor.ExitStatus <> 0 then
          begin
            writelnlog(etError, infotext+'Make/lazbuild returned error code ' + IntToStr(Processor.ExitStatus) + LineEnding +
              'Details: ' + FErrorLog.Text, true);
            OperationSucceeded := false;
          end;
        except
          on E: Exception do
          begin
            OperationSucceeded := false;
            WritelnLog(etError, infotext+'Exception running lazbuild to get IDE with user-specified packages!' + LineEnding +
              'Details: ' + E.Message, true);
          end;
        end;
      end;

      if OperationSucceeded then
      begin
        // Change the build modes to reflect the default LCL widget set.
        // Somewhat strange that this is necessary: should be done by lazbuild with widgetset defined ...
        LazarusConfig:=TUpdateLazConfig.Create(FPrimaryConfigPath);
        try
          {$ifdef Darwin}
          i:=LazarusConfig.GetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Count',0);
          if i=0
            then infoln(infotext+'No build profiles in '+MiscellaneousConfig, etWarning)
            else infoln(infotext+'Changing default LCL_platforms for build-profiles in '+MiscellaneousConfig, etInfo);

          {$ifdef LCLQT5}
          for j:=0 to (i-1) do
          begin
            LazarusConfig.SetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Profile'+InttoStr(j)+'/LCLPlatform/Value', 'qt5');
          end;

          // also set default sizes and position
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Left', '10');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Top', '30');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Width', '900');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/CustomPosition/Height', '60');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MainIDE/Visible/Value', 'True');

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Left', '20');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Top', '120');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Width', '230');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/CustomPosition/Height', '500');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/ObjectInspectorDlg/Visible/Value', 'True');

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Left', '270');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Top', '600');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Width', '600');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/CustomPosition/Height', '100');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/MessagesView/Visible/Value', 'True');

          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Left', '270');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Top', '120');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Width', '600');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/CustomPosition/Height', '440');
          LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/SourceNotebook/Visible/Value', 'True');
          {$endif}

          {$ifdef LCLCOCOA}
          for j:=0 to (i-1) do
          begin
            LazarusConfig.SetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Profile'+InttoStr(j)+'/LCLPlatform/Value', 'cocoa');
          end;
          {$endif}

          {$endif}

          j:=LazarusConfig.GetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/Count',0);
          if j=0 then
          begin
            LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/Count', 2);
            LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop1/IDECoolBarOptions/Width/Value', 250);

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


        finally
          LazarusConfig.Destroy;
        end;
      end;


      // ... build startlazarus if it doesn't exist
      // (even an old version left over by make distclean is probably ok)
      if OperationSucceeded then
      begin
        if FileExistsUTF8(IncludeTrailingPathDelimiter(FInstallDirectory) + 'startlazarus' + GetExeExt) then
        begin
          infoln(infotext+'Startlazarus exists already. Not compiling again.', etdebug);
        end
        else
        begin
          Processor.Executable := LazBuildApp;
          FErrorLog.Clear;
          Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
          Processor.Parameters.Clear;
          {$IFDEF DEBUG}
          Processor.Parameters.Add('--verbose');
          {$ELSE}
          Processor.Parameters.Add('--quiet');
          {$ENDIF}
          Processor.Parameters.Add('--pcp=' + FPrimaryConfigPath);
          Processor.Parameters.Add('--cpu=' + GetTargetCPU);
          Processor.Parameters.Add('--os=' + GetTargetOS);

          if FCrossLCL_Platform <> '' then
            Processor.Parameters.Add('--ws=' + FCrossLCL_Platform);

          Processor.Parameters.Add(IncludeTrailingPathDelimiter(FSourceDirectory)+
            'ide'+DirectorySeparator+'startlazarus.lpi');

          infoln(infotext+'Compiling startlazarus to make sure it is present:', etInfo);
          try
            writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
            Processor.Execute;
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

function TLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  Result := true;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (BuildModuleCustom: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
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
    PlainBinPath := resolveDots(SafeExpandFileName(IncludeTrailingPathDelimiter(FBinPath) + '..'));
    {$IFDEF MSWINDOWS}
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
    // at least one ; to be present in the path. If you only have one entry, you
    // can add PathSeparator without problems.
    // http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
    SetPath(FBinPath + PathSeparator + PlainBinPath + PathSeparator + FMakeDir + PathSeparator +
      ExcludeTrailingPathDelimiter(FSVNDirectory) + PathSeparator + FInstallDirectory, false, false);
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
begin
  Result := inherited;
  Result := InitModule;
  if not Result then exit;
  Result := BuildModuleCustom(ModuleName);
end;

function TLazarusInstaller.ConfigModule(ModuleName: string): boolean;
const
  LazarusCFG = 'lazarus.cfg'; //file to store primary config argument in
  VALIDVERSIONCHARS = ['0'..'9','.',','];
var
  DebuggerPath: string;
  LazarusConfig: TUpdateLazConfig;
  PCPSnippet: TStringList;
  VersionSnippet: string;
  VersionList: TStringList;
  i,j:integer;
  LazBuildApp:string;
  TxtFile:Text;
  VersionFile:string;
begin
  Result := inherited;
  Result := true;

  if DirectoryExistsUTF8(FPrimaryConfigPath) = false then
  begin
    if ForceDirectoriesUTF8(FPrimaryConfigPath) then
      infoln(infotext+'Created Lazarus primary config directory: ' + FPrimaryConfigPath, etInfo);
  end;
  // Set up a minimal config so we can use LazBuild
  // Parse URLs; expect e.g. ..../lazarus_1_0_14.
  // Doesn't take into account release candidates or trunk
  FMajorVersion := -1;
  FMinorVersion := -1;
  FReleaseVersion := -1;

  VersionSnippet:='';

  LazBuildApp := IncludeTrailingPathDelimiter(FInstallDirectory) + 'lazbuild' + GetExeExt;
  Processor.Executable := LazBuildApp;
  Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);

  Processor.Parameters.Clear;
  Processor.Parameters.Add('--version');
  Processor.Execute;
  if Processor.ExitStatus = 0 then
  begin
    i:=Processor.OutputStrings.Count;
    if i>0 then
    begin
      // lazbuild outputs version info
      VersionSnippet:=Processor.OutputStrings.Strings[i-1];
      VersionSnippet:=StringReplace(VersionSnippet,'.',',',[rfReplaceAll]);
    end;
  end;

  if Length(VersionSnippet)=0 then
  begin
    VersionFile:=IncludeTrailingPathDelimiter(FSourceDirectory) + 'ide' + DirectorySeparator + 'version.inc';
    if FileExists(VersionFile) then
    begin
      AssignFile(TxtFile,VersionFile);
      Reset(TxtFile);
      Readln(TxtFile,VersionSnippet);
      // remove quotes from string
      //VersionSnippet:=DelChars(VersionSnippet, '''');
      VersionSnippet:=TrimSet(VersionSnippet, [#39]);
      VersionSnippet:=StringReplace(VersionSnippet,'.',',',[rfReplaceAll]);
      CloseFile(TxtFile);
    end;
  end;

  if Length(VersionSnippet)=0 then
  begin
    VersionSnippet:=GetVersionFromUrl(FURL);
    VersionSnippet:=StringReplace(VersionSnippet,'.',',',[rfReplaceAll]);
  end;

  if Length(VersionSnippet)>0 then
  begin
    // remove extensions like RC### from version
    j:=0;
    for i:=1 to length(VersionSnippet) do
    begin
      if NOT (VersionSnippet[i] in VALIDVERSIONCHARS) then
      begin
        j:=i;
        break;
      end;
    end;
    if j>0 then Delete(VersionSnippet,j,MaxInt);

    VersionList := TStringList.Create;
    try
      VersionList.CommaText := VersionSnippet;
      if VersionList.Count>0 then
      begin
        case VersionList.Count of
          1:
          begin
            FMajorVersion := StrToIntDef(VersionList[0], -1);
            //FMinorVersion := 0;
            //FReleaseVersion := 0;
          end;
          2:
          begin
            FMajorVersion := StrToIntDef(VersionList[0], -1);
            FMinorVersion := StrToIntDef(VersionList[1], -1);
            //FReleaseVersion := 0;
          end;
          else
          begin
            FMajorVersion := StrToIntDef(VersionList[0], -1);
            FMinorVersion := StrToIntDef(VersionList[1], -1);
            FReleaseVersion := StrToIntDef(VersionList[2], -1);
          end;
        end;
      end;
    finally
      VersionList.Free;
    end;
  end;

  LazarusConfig := TUpdateLazConfig.Create(FPrimaryConfigPath, FMajorVersion, FMinorVersion, FReleaseVersion);
  try
    try
      // Lazarus 1.2RC1+ and trunk support specifying the primary-config-path that should be used
      // inside the lazarus directory itself.
      PCPSnippet := TStringList.Create;
      try
        // Martin Friebe mailing list January 2014: no quotes allowed, no trailing blanks
        PCPSnippet.Add('--primary-config-path=' + trim(ExcludeTrailingPathDelimiter(FPrimaryConfigPath)));
        if not (FileExistsUTF8(IncludeTrailingPathDelimiter(FInstallDirectory) + LazarusCFG)) then
          PCPSnippet.SaveToFile(IncludeTrailingPathDelimiter(FInstallDirectory) + LazarusCFG);
      finally
        PCPSnippet.Free;
      end;
      // Force English language
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Language/ID', 'en');
      // Set Lazarus directory
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/LazarusDirectory/Value', FInstallDirectory);
      {$IFDEF MSWINDOWS}
      // FInstalledCompiler could be something like c:\bla\ppc386.exe, e.g.
      // the platform specific compiler. In order to be able to cross compile
      // we'd rather use fpc
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/CompilerFilename/Value', ExtractFilePath(FCompiler) + 'fpc' + GetExeExt);

      // do we supply GDB in the installdir from mingw for win32 and/or win64
      if FileExistsUTF8(IncludeTrailingPathDelimiter(FInstallDirectory) + '..\mingw\' + GetFPCTarget(true) + '\bin\gdb.exe') then
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value',
          '$(LazarusDir)\..\mingw\$(TargetCPU)-$(TargetOS)\bin\gdb.exe')

      // have we downloaded GDB in the makedir for win32 and/or win64
      else if FileExistsUTF8(IncludeTrailingPathDelimiter(FMakeDir) + 'gdb\' + GetFPCTarget(true) + '\gdb.exe') then
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
      {$ELSE}//other *nix
      DebuggerPath := which('gdb');  //assume in path
      {$ENDIF FREEBSD}
      if Length(DebuggerPath)>0
         then LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value', DebuggerPath)
         else infoln(infotext+'No GNU gdb found.' + FPrimaryConfigPath, etWarning);

      {$IFDEF BSD}
      {$IFDEF DARWIN}
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('make')); //assume in path

      // extra gdb settings
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/WarnOnTimeOut', 'False');
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
            i:=0;
            // move towards first numerical
            while (Length(VersionSnippet)>i) AND (NOT (VersionSnippet[i] in ['0'..'9'])) do Inc(i);
            j:=0;
            // get only major version
            while (Length(VersionSnippet)>i) AND (VersionSnippet[i] in ['0'..'9']) do
            begin
              j:=j*10+Ord(VersionSnippet[i])-$30;
              Inc(i);
            end;
            infoln(infotext+'GDB major version: ' + InttoStr(j), etInfo);
            // for newer versions Mac OSX versions (>=10.11) and GDB >= 8.0 [perhaps] needed:
            if j>=8 then LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/ClassTGDBMIDebugger/Properties/Debugger_Startup_Options', '--eval-command="set startup-with-shell off"');
          end;
        end;
      end;
      {$ELSE}//*BSD: FreeBSD, NetBSD, OpenBSD
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('gmake')); //GNU make; assume in path
      {$ENDIF DARWIN}
      {$ENDIF BSD}
      {$ENDIF UNIX}

      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/FPCSourceDirectory/Value', FFPCSourceDir);
      // Debugger type needs to be specified at least since Lazarus 1.1
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Class', 'TGDBMIDebugger');
      // Add <lazarus>\docs\xml to fpdoc editor paths
      LazDocPathAdd(IncludeTrailingPathDelimiter(FInstallDirectory) + 'docs'+DirectorySeparator+'xml', LazarusConfig);

      // Enable IDE Coolbar for default docked desktop for (NewPascal) Lazarus with docking
      if LazarusConfig.GetVariable(EnvironmentConfig,'Desktops/Desktop2/Name')='default docked' then
         LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop2/IDECoolBarOptions/Visible/Value', 'True');

      {$IFDEF MSWINDOWS}
      // needed while running Lazarus adds a personal directory that is not valid for other users.
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Count', '2');
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item1/Value', 'C:\Windows\Temp\');
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item2/Value', 'C:\Users\Public\Documents');
      {$ENDIF MSWINDOWS}

      // add default projects path
      DebuggerPath := IncludeTrailingPathDelimiter(FBaseDirectory) + 'projects';
      ForceDirectoriesUTF8(DebuggerPath);
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/Value', IncludeTrailingPathDelimiter(DebuggerPath));
      // Set file history towards default project directory
      LazarusConfig.SetVariable(History, 'InputHistory/FileDialog/InitialDir', IncludeTrailingPathDelimiter(DebuggerPath));

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
  // Make distclean is unreliable; at least for FPC.
  // Running it twice apparently can fix a lot of problems; see FPC ML message
  // by Jonas Maebe, 1 November 2012
var
  {$ifdef MSWINDOWS}
  CrossInstaller: TCrossInstaller;
  CrossWin: boolean;
  LHelpTemp: string; // LHelp gets copied to this temp file
  {$endif}
  oldlog: TErrorMethod;
  CleanCommand,CleanDirectory:string;
  NothingToBeDone:boolean;
begin
  Result := inherited;

  Result := InitModule;
  if not Result then exit;

  if not DirectoryExistsUTF8(FSourceDirectory) then exit;

  // If cleaning primary config:
  if (FCrossLCL_Platform = '') and (FCrossCPU_Target = '') then
    infoln(infotext+'If your primary config path has changed, you may want to remove ' + IncludeTrailingPathDelimiter(
      FInstallDirectory) + 'lazarus.cfg which points to the primary config path.', etInfo);

  {$ifdef MSWINDOWS}
  // If doing crosswin32-64 or crosswin64-32, make distclean will not only clean the LCL
  // but also existing lhelp.exe if present. Temporarily copy that so we can restore it later.
  // failure here does not influence result
  LHelpTemp:='';
  CrossWin:=false;

  CrossInstaller := GetCrossInstaller;
  if Assigned(CrossInstaller) then
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
  end;

  if CrossWin then
  begin
    LHelpTemp:=GetTempFileNameUTF8('','');
    try
      CopyFile(
        IncludeTrailingPathDelimiter(FInstallDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
        LHelpTemp,[cffOverWriteFile]);
    except
      infoln(infotext+'Non-fatal error copying lhelp to temp file '+LHelpTemp,etInfo);
    end;
  end;
  {$endif MSWINDOWS}

  // Make distclean; we don't care about failure (e.g. directory might be empty etc)
  oldlog := Processor.OnErrorM;
  Processor.OnErrorM := nil;  //don't want to log errors in distclean

  Processor.Executable := Make;
  Processor.CurrentDirectory := ExcludeTrailingPathDelimiter(FSourceDirectory);
  Processor.Parameters.Clear;
  {$IFDEF lazarus_parallel_make}
  if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
  {$ENDIF}
  Processor.Parameters.Add('FPC=' + FCompiler + '');
  Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
  Processor.Parameters.Add('UPXPROG=echo');  //Don't use UPX
  Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  Processor.Parameters.Add('OS_SOURCE=' + GetTargetOS);
  Processor.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);

  CleanDirectory:='';
  CleanCommand:='';
  case UpperCase(ModuleName) of
    'IDE':
    begin
      CleanCommand:='cleanide';
      CleanDirectory:=DirectorySeparator+'ide';
    end;
    'BIGIDE': CleanCommand:='cleanbigide';
    'LAZARUS': CleanCommand:='distclean';
    'LCL':
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
    'LCLCROSS':
    begin
      CleanDirectory:=DirectorySeparator+'lcl';
      NothingToBeDone:=true;
      if FCrossLCL_Platform<>'' then
      begin
        NothingToBeDone:=false;
        {$ifdef Darwin}
          {$ifdef LCLCOCOA}
            NothingToBeDone:=(FCrossLCL_Platform='cocoa');
          {$else}
            {$ifdef CPUX64}
              {$ifdef LCLQT5}
                NothingToBeDone:=(FCrossLCL_Platform='qt5');
              {$else}
                NothingToBeDone:=(FCrossLCL_Platform='cocoa');
              {$endif}
            {$endif}
          {$endif}
        {$endif}
      end;
      if (NOT NothingToBeDone) then
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
    Processor.Parameters.Add('OS_TARGET=' + FCrossOS_Target);
    Processor.Parameters.Add('CPU_TARGET=' + FCrossCPU_Target);
  end
  else
  begin
    Processor.Parameters.Add('OS_TARGET=' + GetTargetOS);
    Processor.Parameters.Add('CPU_TARGET=' + GetTargetCPU);
  end;

  Processor.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FSourceDirectory)+CleanDirectory);
  Processor.Parameters.Add(CleanCommand);
  if (Self is TLazarusCrossInstaller) then
    infoln(infotext+'Running "make '+CleanCommand+'" twice inside .'+CleanDirectory+' for OS_TARGET='+FCrossOS_Target+' and CPU_TARGET='+FCrossCPU_Target,etInfo)
  else
    infoln(infotext+'Running "make '+CleanCommand+'" twice inside .'+CleanDirectory,etInfo);

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
var
  AfterRevision: string;
  BeforeRevision: string;
  Counter: integer;
  Errors: integer;
  UpdateWarnings: TStringList;
  FilePath:string;
  LocalPatchCmd:string;
  Output: string = '';
  ReturnCode,i: integer;
  RevisionIncText: Text;
  ConstStart: string;
  aRepoClient:TRepoClient;
  {$ifdef Darwin}
  {$ifdef LCLQT5}
  fs:TMemoryStream;
  {$endif}
  {$endif}
begin
  Result := inherited;
  Result := InitModule;

  if not Result then exit;

  // not so elegant check to see what kind of client we need ...
  if ( {(Pos('GITHUB',UpperCase(FURL))>0) OR} (Pos('.GIT',UpperCase(FURL))>0) )
     then aRepoClient:=FGitClient
     else aRepoClient:=FSVNClient;

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

  // Replace applicationbundle.pas with a version that adds the necessary files into the app-bundle
  // This is dirty
  fs:=TMemoryStream.Create;
  try
    with TResourceStream.Create(hInstance, 'APPLICATIONBUNDLE', RT_RCDATA) do
    try
      Savetostream(fs);
    finally
      Free;
    end;
    fs.Position:=0;
    SysUtils.DeleteFile(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/ide/applicationbundle.pas');
    if (NOT FileExists(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/ide/applicationbundle.pas')) then
       fs.SaveToFile(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/ide/applicationbundle.pas');
  finally
    fs.Free;
  end;

  // Replace debugmanager.pas with a version that always creates the app-bundle
  // This is very dirty
  fs:=TMemoryStream.Create;
  try
    with TResourceStream.Create(hInstance, 'DEBUGMANAGER', RT_RCDATA) do
    try
      Savetostream(fs);
    finally
      Free;
    end;
    fs.Position:=0;
    SysUtils.DeleteFile(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/ide/debugmanager.pas');
    if (NOT FileExists(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/ide/debugmanager.pas')) then
       fs.SaveToFile(ExcludeTrailingPathDelimiter(FSourceDirectory)+'/ide/debugmanager.pas');
  finally
    fs.Free;
  end;

  {$endif}
  {$endif}

  (*
  Errors := 0;
  if (Result) and (Uppercase(FCrossLCL_Platform) = 'QT') then
  begin
    for Counter := low(FUtilFiles) to high(FUtilFiles) do
    begin
      if (FUtilFiles[Counter].Category = ucQtFile) and not
        (FileExistsUTF8(IncludeTrailingPathDelimiter(FSourceDirectory) + FUtilFiles[Counter].FileName)) then
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

  if result then
  begin
    if Length(FSourcePatches)>0 then
    begin
      infoln(infotext+'Found Lazarus patch file(s).',etInfo);
      UpdateWarnings:=TStringList.Create;
      try
        UpdateWarnings.CommaText := FSourcePatches;
        for i:=0 to (UpdateWarnings.Count-1) do
        begin
          infoln(infotext+'Trying to patch Lazarus with '+UpdateWarnings[i],etInfo);
          FilePath:=SafeExpandFileName(UpdateWarnings[i]);
          if NOT FileExists(FilePath) then FilePath:=SafeExpandFileName(SafeGetApplicationPath+UpdateWarnings[i]);
          if NOT FileExists(FilePath) then FilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchlazarus'+DirectorySeparator+UpdateWarnings[i]);
          if FileExists(FilePath) then
          begin
            // check for default values
            if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
              {$IF defined(BSD) and not defined(DARWIN)}
              then LocalPatchCmd:=FPatchCmd + ' -p0 -N -i '
              {$else}
              then LocalPatchCmd:=FPatchCmd + ' -p0 -N --no-backup-if-mismatch -i '
              {$endif}
               else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
            {$IFDEF MSWINDOWS}
            ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + FilePath, FSourceDirectory, Output, True);
            {$ELSE}
            ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + FilePath, FSourceDirectory, Output, True);
            {$ENDIF}
            if ReturnCode=0
               then infoln(infotext+'Lazarus has been patched successfully with '+UpdateWarnings[i],etInfo)
               else
               begin
                 writelnlog(infotext+'ERROR: Patching Lazarus with ' + UpdateWarnings[i] + ' failed.', true);
                 writelnlog(infotext+'Patch output: ' + Output, true);
               end;
          end
          else
          begin
            infoln(infotext+'Strange: could not find patchfile '+FilePath, etWarning);
            writelnlog(etError, infotext+'Patching Lazarus with ' + UpdateWarnings[i] + ' failed due to missing patch file.', true);
          end;
        end;
      finally
        UpdateWarnings.Free;
      end;
    end else infoln(infotext+'No Lazarus patches defined.',etInfo);
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
  if FileExistsUTF8(IncludeTrailingBackslash(FSourceDirectory) + 'Makefile') and DirectoryExistsUTF8(
    IncludeTrailingBackslash(FSourceDirectory) + 'ide') and DirectoryExistsUTF8(IncludeTrailingBackslash(FSourceDirectory) + 'lcl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FSourceDirectory)) then
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
  if Result and FileExistsUTF8(IncludeTrailingBackslash(FPrimaryConfigPath) + EnvironmentConfig) and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FPrimaryConfigPath)) then
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

