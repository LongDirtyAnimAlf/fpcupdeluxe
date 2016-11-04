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
    //Note: we only do a getmodule/cleanmodule once here in this sequence,
    //otherwise we keep running distclean and svn
    'Declare lazarus;' + 'Cleanmodule lazarus;' + 'Getmodule lazarus;' +
    //config lazarus so we can use lazbuild in the build step:
    'ConfigModule lazarus;' +
    // Cross compile support at least requires lazbuild.
    // we're building it here directly to avoid circular
    // dependencies
    'Buildmodule lazbuild;' + 'Buildmodule lazarus;' +
    //Config again to (possibly) fix any wrong settings introduced:
    'ConfigModule lazarus;' +
    // Make sure the user can use the IDE:
    'Exec CreateLazarusScript;' + 'End;' +


    'Declare oldlazarus;' +
    'Cleanmodule lazarus;' + 'Getmodule lazarus;' + 'Buildmodule lazarus;' +
    'ConfigModule lazarus;' +
    'Exec CreateLazarusScript;' + 'End;' +

    {
    'Declare lazscripttest;' +
    'Exec CreateLazarusScript;' +
    'End;' +
    }

    'Declare LazCleanAndBuildOnly;' +
    'Cleanmodule lazarus;' +
    'ConfigModule lazarus;' +
    'Buildmodule lazbuild;' +
    'Buildmodule lazarus;' +
    'ConfigModule lazarus;' +
    'Exec CreateLazarusScript;' +
    'End;' +

    //Nogui widgetset+Lazbuild:
    'Declare lazbuild;' + 'Getmodule lazarus;' + 'Buildmodule lazbuild;' +
    //config lazarus, so lazbuild will work:
    'ConfigModule lazarus;' + 'End;' +

    //standard IDE build with user-selected packages
    // assumes/requires that Laz svn has already been updated
    // also we need lazbuild, but we can check for it in our USERIDE code.
    // If we Require it here, it will kick off a lazbuild build cycle that
    // may already have been done.
    'Declare USERIDE;' + 'Buildmodule USERIDE;' +
    // Make sure the user can use the IDE:
    'Exec CreateLazarusScript;' + 'End;' +

    //standard uninstall
    'Declare lazarusuninstall;' + 'Uninstallmodule lazarus;' + 'Exec DeleteLazarusScript;' + 'End;' +

    //standard clean
    'Declare lazarusclean;' + 'Cleanmodule lazarus;' + 'End;' +

    //selective actions triggered with --only=SequenceName
    'Declare LazarusCleanOnly;' + 'Cleanmodule lazarus;' + 'End;' + 'Declare LazarusGetOnly;' +
    'Getmodule lazarus;' + 'End;' + 'Declare LazarusBuildOnly;' + 'Buildmodule lazarus;' + 'End;' +
    'Declare LazarusConfigOnly;' + 'Configmodule lazarus;' + 'End;' +

    // Crosscompile build
    'Declare LazarusCrossWin32-64;' +
    // Needs to be run after regular compile because of CPU/OS switch
    'SetCPU x86_64;' + 'SetOS win64;' +
    // Getmodule has already been done
    // Don't use cleanmodule; make distclean will remove lazbuild.exe etc
    //'Cleanmodule LCL;' +
    'Buildmodule LCL;' + 'End;' +

    // Crosscompile only LCL (needs to be run at end
    'Declare LCLCross;' + 'ResetLCL;' + //module code itself will select proper widgetset
    // Getmodule must already have been done
    // If we call CleanModule, currently Lazbuild.exe etc will also be removed!?!?!, so do not do that
    // 'CleanModule LCL;'+
    'Buildmodule LCL;' + 'End';

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
    FFPCDir: string;
    FInstalledLazarus: string;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName: string): boolean; virtual;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule: boolean;
  public
    // LCL widget set to be built (NOT OS/CPU combination)
    property CrossLCL_Platform: string write FCrossLCL_Platform;
    // FPC base directory
    property FPCDir: string write FFPCDir;
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

uses fpcuputil, fileutil,
  lazfileutils {utf8 file functions},
  updatelazconfig
  {$IFDEF UNIX}
  , baseunix
  {$ENDIF UNIX}  ;

{ TLazarusCrossInstaller }

function TLazarusCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  BuildMethod: string;
  CrossInstaller: TCrossInstaller;
  Options: string;
begin
  CrossInstaller := GetCrossInstaller;
  infoln('TLazarusCrossInstaller: building module ' + ModuleName + '...', etInfo);
  FErrorLog.Clear;
  if Assigned(CrossInstaller) then
  begin
    // Actually not using crossopts - they're only for building an FPC compiler; the
    // relevant options should have been written as a snippet to fpc.cfg and picked
    // up from there.
    CrossInstaller.SetCrossOpt(CrossOPT); //pass on user-requested cross compile options
    if not CrossInstaller.GetBinUtils(FBaseDirectory) then
      infoln('Failed to get crossbinutils', eterror)
    else if not CrossInstaller.GetLibs(FBaseDirectory) then
      infoln('Failed to get cross libraries', eterror)
    else if not CrossInstaller.GetLibsLCL(FCrossLCL_Platform, FBaseDirectory) then
      infoln('Failed to get LCL cross libraries', eterror)
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
      if FCrossLCL_Platform <> '' then
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
        BuildMethod := 'make';
        ProcessEx.Executable := Make;
        ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
        ProcessEx.Parameters.Clear;
        {$IFDEF lazarus_parallel_make}
        if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
        {$ENDIF}
        ProcessEx.Parameters.Add('FPC=' + FCompiler);
        ProcessEx.Parameters.Add('USESVN2REVISIONINC=0');
        ProcessEx.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FBaseDirectory));
        ProcessEx.Parameters.Add('FPCDIR=' + FFPCDir); //Make sure our FPC units can be found by Lazarus
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath <> '' then
          ProcessEx.Parameters.Add('CROSSBINDIR=' + ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
        ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
        if FCrossLCL_Platform <> '' then
          ProcessEx.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
        ProcessEx.Parameters.Add('CPU_TARGET=' + FCrossCPU_Target);
        ProcessEx.Parameters.Add('OS_TARGET=' + FCrossOS_Target);
        Options := FCompilerOptions;
        if CrossInstaller.LibsPath <> '' then
          Options := Options + ' -Xd -Fl' + CrossInstaller.LibsPath;
        if CrossInstaller.BinUtilsPrefix <> '' then
        begin
          Options := Options + ' -XP' + CrossInstaller.BinUtilsPrefix;
          ProcessEx.Parameters.Add('BINUTILSPREFIX=' + CrossInstaller.BinUtilsPrefix);
        end;
        ProcessEx.Parameters.Add('OPT=-vi-n-h- ' + Options);
        // Since April 2012, LCL requires lazutils which requires registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        ProcessEx.Parameters.Add('registration');
        ProcessEx.Parameters.Add('lazutils');
        ProcessEx.Parameters.Add('lcl');
      end
      else
      begin
        // Use lazbuild for cross compiling lite:
        BuildMethod := 'lazbuild';
        ProcessEx.Executable := IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazbuild' + GetExeExt;
        ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
        ProcessEx.Parameters.Clear;
        ProcessEx.Parameters.Add('--quiet');
        ProcessEx.Parameters.Add('--quiet');
        ProcessEx.Parameters.Add('--pcp=' + FPrimaryConfigPath);
        ProcessEx.Parameters.Add('--cpu=' + FCrossCPU_Target);
        ProcessEx.Parameters.Add('--os=' + FCrossOS_Target);
        if FCrossLCL_Platform <> '' then
          ProcessEx.Parameters.Add('--widgetset=' + FCrossLCL_Platform);
        ProcessEx.Parameters.Add('lcl'+DirectorySeparator+'interfaces'+DirectorySeparator+'lcl.lpk');
      end;

      if FCrossLCL_Platform = '' then
        infoln('Lazarus: compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + ' using ' + BuildMethod, etInfo)
      else
        infoln('Lazarus: compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + '/' + FCrossLCL_Platform + ' using ' + BuildMethod, etInfo);

      try
        writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
        ProcessEx.Execute;
        Result := ProcessEx.ExitStatus = 0;
        if not Result then
          WritelnLog('Lazarus: error compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + ' ' + FCrossLCL_Platform + LineEnding +
            'Details: ' + FErrorLog.Text, true);
      except
        on E: Exception do
        begin
          Result := false;
          WritelnLog('Lazarus: exception compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + LineEnding +
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
          infoln('Lazarus: Cross compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target +
            ' failed. Optional module; continuing regardless.', etInfo)
        else
          infoln('Lazarus: Cross compiling LCL for ' + FCrossCPU_Target + '-' + FCrossOS_Target + ' failed.', etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(Result);
      end;
    end; //prereqs in place
  end    //valid cross compile setup
  else
    infoln('Lazarus: can''t find cross installer for ' + FCrossCPU_Target + '-' + FCrossOS_Target, eterror);
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
  sCmpOpt: string;
begin
  OperationSucceeded := true;
  infoln('TLazarusNativeInstaller: building module ' + ModuleName + '...', etInfo);

  if ModuleName <> 'USERIDE' then
  begin
    // Make all (should include lcl & ide), lazbuild, lcl etc
    // distclean was already run; otherwise specify make clean all
    FErrorLog.Clear;
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
    ProcessEx.Parameters.Clear;
    {$IFDEF lazarus_parallel_make}
    if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
    {$ENDIF}
    ProcessEx.Parameters.Add('FPC=' + FCompiler);
    ProcessEx.Parameters.Add('USESVN2REVISIONINC=0');
    ProcessEx.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FBaseDirectory));
    ProcessEx.Parameters.Add('FPCDIR=' + FFPCDir); //Make sure our FPC units can be found by Lazarus
    //ProcessEx.Parameters.Add('CPU_TARGET=' + lowercase({$i %FPCTARGETCPU%}));
    //ProcessEx.Parameters.Add('OS_TARGET=' + lowercase({$i %FPCTARGETOS%}));
    ProcessEx.Parameters.Add('UPXPROG=echo');      //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo');     //fix for examples in Win svn, see build FAQ
    { Do not do this - only allow useride to be built for native widgetset.
    LCL /can/ be built using different widgetset
    if FCrossLCL_Platform <> '' then
      ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
    }
    // replace -g by -gw if encountered: http://lists.lazarus.freepascal.org/pipermail/lazarus/2015-September/094238.html
    sCmpOpt:=StringReplace(FCompilerOptions,'-g ','-gw ',[]);
    ProcessEx.Parameters.Add('OPT=-vi-n-h- ' + sCmpOpt);

    case UpperCase(ModuleName) of
      'LAZARUS':
      begin
        ProcessEx.Parameters.Add('all');
        infoln(ModuleName + ': running make all:', etInfo);
      end;
      'LAZBUILD':
      begin
        ProcessEx.Parameters.Add('lazbuild');
        infoln(ModuleName + ': running make lazbuild:', etInfo);
      end;
      'LCL', 'LCLCROSS':
      begin
        // April 2012: lcl now requires lazutils and registration
        // http://wiki.lazarus.freepascal.org/Getting_Lazarus#Make_targets
        ProcessEx.Parameters.Add('registration');
        ProcessEx.Parameters.Add('lazutils');
        ProcessEx.Parameters.Add('lcl');
        if (Uppercase(ModuleName) = 'LCLCROSS') then
          if FCrossLCL_Platform = '' then
          begin
            // Nothing to be done as we're compiling natively. Gracefully exit
            infoln(ModuleName + ': empty LCL platform specified. No need to cross compile LCL. Stopping.', etInfo);
            OperationSucceeded := true; //belts and braces
            Result := true;
            exit;
          end
          else
            ProcessEx.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
        infoln(ModuleName + ': running make registration lazutils lcl:', etInfo);
      end
      else //raise error;
      begin
        ProcessEx.Parameters.Add('--help'); // this should render make harmless
        WritelnLog('BuildModule: Invalid module name ' + ModuleName + ' specified! Please fix the code.', true);
        FInstalledLazarus := '//*\\error/ / \ \';
        OperationSucceeded := false;
        Result := false;
        exit;
      end;
    end;
    try
      writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
      ProcessEx.Execute;
      ExitCode := ProcessEx.ExitStatus;
      if ExitCode <> 0 then
      begin
        OperationSucceeded := false;
        Result := false;
        WritelnLog('Lazarus: error running make!' + LineEnding + 'Details: exit code ' + IntToStr(ExitCode), true);
      end;
    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        Result := false;
        WritelnLog('Lazarus: exception running make!' + LineEnding + 'Details: ' + E.Message, true);
      end;
    end;

    //Special check for lazbuild as that is known to go wrong
    if (OperationSucceeded) and (UpperCase(ModuleName) = 'LAZBUILD') then
    begin
      if CheckExecutable(IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazbuild' + GetExeExt, '--help', 'lazbuild') = false then
      begin
        writelnlog('Lazarus: lazbuild could not be found, so cannot build USERIDE.', true);
        Result := false;
        FInstalledLazarus := '//*\\error/ / \ \ no valid lazbuild found';
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
    LazBuildApp := IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazbuild' + GetExeExt;
    if CheckExecutable(LazBuildApp, '--help', 'lazbuild') = false then
    begin
      writelnlog('Lazarus: lazbuild could not be found, so cannot build USERIDE.', true);
      FInstalledLazarus := '//*\\error/ / \ \ no valid lazbuild found';
      exit(false);
    end
    else
    begin
      // First build IDE using lazbuild... then...
      ProcessEx.Executable := LazBuildApp;
      FErrorLog.Clear;
      ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--quiet');
      ProcessEx.Parameters.Add('--quiet');
      ProcessEx.Parameters.Add('--pcp=' + FPrimaryConfigPath);
      // Support keeping userdefined installed packages when building.
      // Compile with selected compiler options
      // Assume new Laz version on failure getting revision
      if strtointdef(Revision, 38971) >= 38971 then
      begin
        ProcessEx.Parameters.Add('--build-ide=-dKeepInstalledPackages ' + FCompilerOptions);
        ProcessEx.Parameters.Add('--build-mode=');
      end
      else
      begin
        // Fallback - depends on hardcoded "Normal IDE" build mode being present
        // We can specify a build mode; otherwise probably the latest build mode will be used
        // which could well be a stripped IDE
        // Let's see how/if FCompilerOptions clashes with the settings in normal build mode
        writelnlog('LazBuild: building UserIDE but falling back to --build-mode=Normal IDE', true);
        ProcessEx.Parameters.Add('--build-ide= ' + FCompilerOptions);
        ProcessEx.Parameters.Add('--build-mode=Normal IDE');
      end;

      if FCrossLCL_Platform <> '' then
        ProcessEx.Parameters.Add('--widgetset=' + FCrossLCL_Platform);
      // Run first time...
      if OperationSucceeded then
      begin
        infoln('Lazarus: running lazbuild to get IDE with user-specified packages:', etInfo);
        try
          writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
          ProcessEx.Execute;
          if ProcessEx.ExitStatus <> 0 then
          begin
            writelnlog('Lazarus: buildmodulecustom: make/lazbuild returned error code ' + IntToStr(ProcessEx.ExitStatus) + LineEnding +
              'Details: ' + FErrorLog.Text, true);
            OperationSucceeded := false;
            FInstalledLazarus := '//*\\error/ / \ \';
          end
          else
          begin
            FInstalledLazarus := IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazarus' + GetExeExt;
          end;
        except
          on E: Exception do
          begin
            OperationSucceeded := false;
            WritelnLog('Lazarus: exception running lazbuild to get IDE with user-specified packages!' + LineEnding +
              'Details: ' + E.Message, true);
          end;
        end;
      end;

      //todo: debug: test without this fix (without external modules to see if this workaround is still needed
      {
      // ... and another time to fix an apparent bug that does not install packages
      // marked for installation
      if OperationSucceeded then
      begin
        infoln('Lazarus: running lazbuild again to install user-specified packages:', etInfo);
        try
          writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
          ProcessEx.Execute;
          if ProcessEx.ExitStatus <> 0 then
          begin
            writelnlog('Lazarus: buildmodulecustom: lazbuild returned error code ' + IntToStr(ProcessEx.ExitStatus) + LineEnding +
              'Details: ' + FErrorLog.Text, true);
            OperationSucceeded := false;
          end;
        except
          on E: Exception do
          begin
            OperationSucceeded := false;
            WritelnLog('Lazarus: exception running lazbuild to install user-specified packages!' + LineEnding +
              'Details: ' + E.Message, true);
          end;
        end;
      end;
      }

      // ... build startlazarus if it doesn't exist
      // (even an old version left over by make distclean is probably ok)
      if OperationSucceeded then
      begin
        if FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory) + 'startlazarus' + GetExeExt) then
        begin
          infoln('Lazarus: startlazarus exists already. Not compiling again.', etdebug);
        end
        else
        begin
          ProcessEx.Executable := LazBuildApp;
          FErrorLog.Clear;
          ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
          ProcessEx.Parameters.Clear;
          ProcessEx.Parameters.Add('--quiet');
          ProcessEx.Parameters.Add('--quiet');
          ProcessEx.Parameters.Add('--pcp=' + FPrimaryConfigPath);
          ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(FBaseDirectory)+
            'ide'+DirectorySeparator+'startlazarus.lpi');

          infoln('Lazarus: compiling startlazarus to make sure it is present:', etInfo);
          try
            writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
            ProcessEx.Execute;
            if ProcessEx.ExitStatus <> 0 then
            begin
              Writelnlog('Lazarus: buildmodulecustom: lazbuild startlazarus returned error code ' + IntToStr(ProcessEx.ExitStatus) + LineEnding +
                'Details: ' + FErrorLog.Text, true);
              OperationSucceeded := false;
            end;
          except
            on E: Exception do
            begin
              OperationSucceeded := false;
              WritelnLog('Lazarus: exception running lazbuild to get startlazarus!' + LineEnding +
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
end;

function TLazarusInstaller.InitModule: boolean;
var
  PlainBinPath: string; //the directory above e.g. c:\development\fpc\bin\i386-win32
begin
  Result := true;
  infoln('TLazarusInstaller: initialising...', etDebug);
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM := @DumpOutput;
  WritelnLog('TLazarusInstaller init:', false);
  WritelnLog('Lazarus directory:      ' + FBaseDirectory, false);
  WritelnLog('Lazarus URL:            ' + FURL, false);
  WritelnLog('Lazarus options:        ' + FCompilerOptions, false);
  result:=(CheckAndGetNeededExecutables) AND (CheckAndGetNeededBinUtils);
  if Result then
  begin
    // Look for make etc in the current compiler directory:
    FBinPath := ExcludeTrailingPathDelimiter(ExtractFilePath(FCompiler));
    PlainBinPath := SafeExpandFileName(IncludeTrailingPathDelimiter(FBinPath) + '..');
    {$IFDEF MSWINDOWS}
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
    // at least one ; to be present in the path. If you only have one entry, you
    // can add PathSeparator without problems.
    // http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
    SetPath(FBinPath + PathSeparator + PlainBinPath + PathSeparator + FMakeDir + PathSeparator +
      ExcludeTrailingPathDelimiter(FSVNDirectory) + PathSeparator + FBaseDirectory, false, false);
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
  Result := InitModule;
  if not Result then
    exit;
  Result := BuildModuleCustom(ModuleName);
end;

function TLazarusInstaller.ConfigModule(ModuleName: string): boolean;
const
  LazarusCFG = 'lazarus.cfg'; //file to store primary config argument in
  StaticPackagesFile = 'staticpackages.inc';
var
  DebuggerPath: string;
  LazarusConfig: TUpdateLazConfig;
  PCPSnippet: TStringList;
  StaticPackages: TStringList;
  VersionSnippet: string;
  VersionList: TStringList;
  i:integer;
  LazBuildApp:string;
  Output:string;
  FReturnCode: integer;
  TxtFile:Text;
  VersionFile:string;
begin

  Result := true;


  if DirectoryExistsUTF8(FPrimaryConfigPath) = false then
  begin
    if ForceDirectoriesUTF8(FPrimaryConfigPath) then
      infoln('Created Lazarus primary config directory: ' + FPrimaryConfigPath, etInfo);
  end;
  // Set up a minimal config so we can use LazBuild
  // Parse URLs; expect e.g. ..../lazarus_1_0_14.
  // Doesn't take into account release candidates or trunk
  FMajorVersion := -1;
  FMinorVersion := -1;
  FReleaseVersion := -1;

  VersionSnippet:='';

  LazBuildApp := IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazbuild' + GetExeExt;
  ProcessEx.Executable := LazBuildApp;
  ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('--version');
  ProcessEx.Execute;
  if ProcessEx.ExitStatus = 0 then
  begin
    i:=ProcessEx.OutputStrings.Count;
    if i>0 then
    begin
      // lazbuild outputs version info
      VersionSnippet:=ProcessEx.OutputStrings.Strings[i-1];
      VersionSnippet:=StringReplace(VersionSnippet,'.',',',[rfReplaceAll]);
    end;
  end;

  if Length(VersionSnippet)=0 then
  begin
    VersionFile:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'ide' + DirectorySeparator + 'version.inc';
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
    VersionList := TStringList.Create;
    try
      VersionList.CommaText := VersionSnippet;
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
        3..maxint:
        begin
          FMajorVersion := StrToIntDef(VersionList[0], -1);
          FMinorVersion := StrToIntDef(VersionList[1], -1);
          FReleaseVersion := StrToIntDef(VersionList[2], -1);
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
        if not (FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory) + LazarusCFG)) then
          PCPSnippet.SaveToFile(IncludeTrailingPathDelimiter(FBaseDirectory) + LazarusCFG);
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
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/CompilerFilename/Value', ExtractFilePath(FCompiler) + 'fpc' + GetExeExt);

      // do we supply GDB in the installdir from mingw for win32 and/or win64
      if FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory) + '..\mingw\' + GetFPCTarget(true) + '\bin\gdb.exe') then
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

      {$IFDEF FREEBSD}
      // Check for newer user-installed debugger (e.g. from ports tree
      // The system gdb is ancient (gdb 6.1.1 in FreeBSD 9) and does not work well with Laz
      DebuggerPath := '/usr/local/bin/';
      if CheckExecutable(DebuggerPath + 'gdb', '--version', 'GNU gdb') then
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value', DebuggerPath + 'gdb')
      else
        LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value', which('gdb')); //system gdb; assume in path
      {$ELSE}//other *nix
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/DebuggerFilename/Value', which('gdb')); //assume in path
      {$ENDIF FREEBSD}

      {$IFDEF BSD}
      {$IFDEF DARWIN}
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('make')); //assume in path
      {$ELSE}//*BSD: FreeBSD, NetBSD, OpenBSD
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/MakeFilename/Value', which('gmake')); //GNU make; assume in path
      {$ENDIF DARWIN}
      {$ENDIF BSD}
      {$ENDIF UNIX}

      // Source dir in stock Lazarus on windows is something like
      // $(LazarusDir)fpc\$(FPCVer)\source\
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/FPCSourceDirectory/Value', FFPCDir);
      // Debugger type needs to be specified at least since Lazarus 1.1
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/Debugger/Class', 'TGDBMIDebugger');
      // Add <lazarus>\docs\xml to fpdoc editor paths
      LazDocPathAdd(IncludeTrailingPathDelimiter(FBaseDirectory) + 'docs'+DirectorySeparator+'xml', LazarusConfig);

      // Enable IDE Coolbar for default docked desktop for (NewPascal) Lazarus with docking
      if LazarusConfig.GetVariable(EnvironmentConfig,'Desktops/Desktop2/Name')='default docked' then
         LazarusConfig.SetVariable(EnvironmentConfig, 'Desktops/Desktop2/IDECoolBarOptions/Visible/Value', 'True');

      // add default projects path
      DebuggerPath := ExpandFileName(IncludeTrailingPathDelimiter(FBaseDirectory) + '..');
      DebuggerPath := IncludeTrailingPathDelimiter(DebuggerPath)+'projects';
      ForceDirectoriesUTF8(DebuggerPath);
      //LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/AutoSave/LastSavedProjectFile', IncludeTrailingPathDelimiter(DebuggerPath)+'project1.lpi');
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/Value', IncludeTrailingPathDelimiter(DebuggerPath));

      {$IFDEF MSWINDOWS}
      // needed while running Lazarus adds a personal directory that is not valid for other users.
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Count', '2');
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item1/Value', 'C:\Windows\Temp\');
      LazarusConfig.SetVariable(EnvironmentConfig, 'EnvironmentOptions/TestBuildDirectory/History/Item2/Value', 'C:\Users\Public\Documents');
      {$ENDIF MSWINDOWS}

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
  CrossInstaller: TCrossInstaller;
  CrossWin: boolean;
  LHelpTemp: string; // LHelp gets copied to this temp file
  oldlog: TErrorMethod;
begin
  Result := InitModule;

  if not Result then
    exit;
  // Check for valid basedirectory to avoid deleting in random locations or
  // hitting bug 26706: OSX TProcess.Execute fails on next call with invalid
  // current directory
  if not DirectoryExistsUTF8(FBaseDirectory) then
  begin
    infoln('Lazarus CleanModule: directory '+FBaseDirectory+' does not exist. Exiting CleanModule.',etWarning);
    exit;
  end;

  CrossInstaller := GetCrossInstaller;
  // If cleaning primary config:
  if (FCrossLCL_Platform = '') and (FCrossCPU_Target = '') then
    infoln('Lazarus: if your primary config path has changed, you may want to remove ' + IncludeTrailingPathDelimiter(
      FBaseDirectory) + 'lazarus.cfg which points to the primary config path.', etInfo);

  // If doing crosswin32-64 or crosswin64-32, make distclean will not only clean the LCL
  // but also existing lhelp.exe if present. Temporarily copy that so we can restore it later.
  // failure here does not influence result
  LHelpTemp:='';
  CrossWin:=false;
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
    LHelpTemp:=GetTempFileNameUTF8('','');
    try
      CopyFile(
        IncludeTrailingPathDelimiter(FBaseDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
        LHelpTemp,[cffOverWriteFile]);
    except
      infoln('Lazarus CleanModule: non-fatal error copying lhelp to temp file '+LHelpTemp,etInfo);
    end;
  end;

  // Make distclean; we don't care about failure (e.g. directory might be empty etc)
  oldlog := ProcessEx.OnErrorM;
  ProcessEx.OnErrorM := nil;  //don't want to log errors in distclean
  ProcessEx.Executable := Make;
  ProcessEx.CurrentDirectory := ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  {$IFDEF lazarus_parallel_make}
  if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
  {$ENDIF}
  ProcessEx.Parameters.Add('FPC=' + FCompiler + '');
  ProcessEx.Parameters.Add('--directory=' + ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('UPXPROG=echo');  //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  if FCrossLCL_Platform <> '' then
    ProcessEx.Parameters.Add('LCL_PLATFORM=' + FCrossLCL_Platform);
  if (Self is TLazarusCrossInstaller) then
  begin  // clean out the correct compiler
    ProcessEx.Parameters.Add('OS_TARGET=' + FCrossOS_Target);
    ProcessEx.Parameters.Add('CPU_TARGET=' + FCrossCPU_Target);
    infoln('Lazarus: running make distclean (OS_TARGET=' + FCrossOS_Target + '/CPU_TARGET=' + FCrossCPU_Target + '):', etInfo);
  end
  else
  begin
    infoln('Lazarus: running make distclean:', etInfo);
  end;
  ProcessEx.Parameters.Add('distclean');
  try
    // Note: apparently, you can't specify certain modules to clean, like lcl.
    writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
    ProcessEx.Execute;
    sleep(100); //now do it again:
    writelnlog('Execute: '+ProcessEx.Executable+'. Params: '+ProcessEx.Parameters.CommaText, true);
    ProcessEx.Execute;
    Result := true;
  except
    on E: Exception do
    begin
      Result := false;
      WritelnLog('Lazarus: running make distclean failed with an exception!' + LineEnding + 'Details: ' + E.Message, true);
    end;
  end;
  ProcessEx.OnErrorM := oldlog; //restore previous logging

  // Now try to restore lhelp
  if LHelpTemp<>'' then
  begin
    try
      CopyFile(
        LHelpTemp,
        IncludeTrailingPathDelimiter(FBaseDirectory)+'components'+DirectorySeparator+'chmhelp'+DirectorySeparator+'lhelp'+DirectorySeparator+'lhelp'+GetExeExt,
        [cffOverWriteFile]);
    except
      infoln('Lazarus CleanModule: non-fatal error restoring lhelp from temp file '+LHelpTemp,etInfo);
    end;
  end;
end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
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
  PatchFilePath:string;
  LocalPatchCmd:string;
  Output: string = '';
  ReturnCode,i: integer;
  RevisionIncText: Text;
  ConstStart: string;
begin
  Result := InitModule;
  if not Result then
    exit;
  infoln('Checking out/updating Lazarus sources:', etInfo);
  UpdateWarnings := TStringList.Create;
  try
    FSVNClient.Verbose:=FVerbose;
    FSVNClient.ExportOnly:=FExportOnly;
    Result := DownloadFromSVN(ModuleName, BeforeRevision, AfterRevision, UpdateWarnings);
    if UpdateWarnings.Count > 0 then
    begin
      WritelnLog(UpdateWarnings.Text);
    end;
  finally
    UpdateWarnings.Free;
  end;

  if NOT FSVNClient.ExportOnly then
  begin
    infoln('Lazarus was at: ' + BeforeRevision, etInfo);

    if FRepositoryUpdated then
    begin
      Revision := AfterRevision;
      infoln('Lazarus is now at: ' + AfterRevision, etInfo);
    end
    else
    begin
      Revision := BeforeRevision;
      infoln('No updates for Lazarus found.', etInfo);
    end;
  end
  else
  begin
    Revision := AfterRevision;
    infoln('Lazarus is now at: ' + AfterRevision, etInfo);
  end;

  if (Result) then
  begin
    // update revision.inc;
    infoln('Updating Lazarus version info.', etInfo);
    AssignFile(RevisionIncText, IncludeTrailingPathDelimiter(FBaseDirectory)+'ide'+PathDelim+RevisionIncFileName);
    try
      Rewrite(RevisionIncText);
      writeln(RevisionIncText, RevisionIncComment);
      ConstStart := Format('const %s = ''', [ConstName]);
      writeln(RevisionIncText, ConstStart, FSVNClient.LocalRevision, ''';');
    finally
      CloseFile(RevisionIncText);
    end;
  end;

  if Result
     then infoln('Checking out/updating Lazarus sources ok', etInfo)
     else infoln('Checking out/updating Lazarus sources failure', etError);

  // Download Qt bindings if not present yet
  Errors := 0;
  if (Result) and (Uppercase(FCrossLCL_Platform) = 'QT') then
  begin
    for Counter := low(FUtilFiles) to high(FUtilFiles) do
    begin
      if (FUtilFiles[Counter].Category = ucQtFile) and not
        (FileExistsUTF8(IncludeTrailingPathDelimiter(FBaseDirectory) + FUtilFiles[Counter].FileName)) then
      begin
        infoln('Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + FBaseDirectory, etDebug);
        try
          if Download(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName, IncludeTrailingPathDelimiter(FBaseDirectory) +
            FUtilFiles[Counter].FileName, FHTTPProxyHost, IntToStr(FHTTPProxyPort), FHTTPProxyUser,
            FHTTPProxyPassword) = false then
          begin
            Errors := Errors + 1;
            infoln('Error downloading Qt-related file to ' + IncludeTrailingPathDelimiter(FBaseDirectory) +
              FUtilFiles[Counter].FileName, eterror);
          end;
        except
          on E: Exception do
          begin
            Result := false;
            infoln('Error downloading Qt-related files: ' + E.Message, etError);
            exit; //out of function.
          end;
        end;
      end;
    end;

    if Errors > 0 then
    begin
      Result := false;
      WritelnLog('TLazarusNativeInstaller.GetModule(' + ModuleName + '): ' + IntToStr(Errors) + ' errors downloading Qt-related files.', true);
    end;
  end;

  if result then
  begin
    if Length(FSourcePatches)>0 then
    begin
      infoln('Found Lazarus patch file(s).',etInfo);
      UpdateWarnings:=TStringList.Create;
      try
        UpdateWarnings.CommaText := FSourcePatches;
        for i:=0 to (UpdateWarnings.Count-1) do
        begin
          infoln('Trying to patch Lazarus with '+UpdateWarnings[i],etInfo);
          PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchlazarus'+DirectorySeparator+UpdateWarnings[i]);
          if NOT FileExists(PatchFilePath) then
          begin
            infoln('Could not find patchfile '+PatchFilePath,etInfo);
            infoln('Trying current app directory.',etInfo);
            PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+UpdateWarnings[i]);
          end;
          if FileExists(PatchFilePath) then
          begin
            // check for default values
            if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
               then LocalPatchCmd:=FPatchCmd + ' -p0 -N --no-backup-if-mismatch -i '
               else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
            {$IFDEF MSWINDOWS}
            ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + PatchFilePath, FBaseDirectory, Output, True);
            {$ELSE}
            ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + PatchFilePath, FBaseDirectory, Output, True);
            {$ENDIF}
            if ReturnCode=0
               then infoln('Lazarus has been patched successfully with '+UpdateWarnings[i],etInfo)
               else
               begin
                 writelnlog(ModuleName+' ERROR: Patching Lazarus with ' + UpdateWarnings[i] + ' failed.', true);
                 writelnlog(ModuleName+' patch output: ' + Output, true);
               end;
          end
          else
          begin
            infoln('Strange error: could not find patchfile '+PatchFilePath,etInfo);
          end;
        end;
      finally
        UpdateWarnings.Free;
      end;
    end else infoln('No Lazarus patches defined.',etInfo);
  end;

end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
const
  LookForConfigFile = 'environmentoptions.xml';
begin
  if not InitModule then
    exit;
  infoln('Module Lazarus: Uninstall...', etInfo);
  //sanity check
  if FileExistsUTF8(IncludeTrailingBackslash(FBaseDirectory) + 'Makefile') and DirectoryExistsUTF8(
    IncludeTrailingBackslash(FBaseDirectory) + 'ide') and DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory) + 'lcl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FBaseDirectory)) then
  begin
    Result := DeleteDirectoryEx(FBaseDirectory);
    if not (Result) then
      WritelnLog('Error deleting Lazarus directory ' + FBaseDirectory);
  end
  else
  begin
    WritelnLog('Error: invalid Lazarus directory :' + FBaseDirectory);
    Result := false;
  end;

  // Sanity check so we don't try to delete random directories
  // Assume Lazarus has been configured/run once so enviroronmentoptions.xml exists.
  if Result and FileExistsUTF8(IncludeTrailingBackslash(FPrimaryConfigPath) + LookForConfigFile) and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FPrimaryConfigPath)) then
  begin
    Result := DeleteDirectoryEx(FPrimaryConfigPath) = false;
    if not (Result) then
      WritelnLog('Error deleting Lazarus PrimaryConfigPath directory ' + FPrimaryConfigPath);
  end
  else
  begin
    WritelnLog('Error: invalid Lazarus FPrimaryConfigPath: ' + FPrimaryConfigPath);
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

