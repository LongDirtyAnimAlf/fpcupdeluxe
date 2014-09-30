{ Help installer/uninstaller unit for fpcup
Copyright (C) 2012-2014 Ludo Brands, Reinier Olislagers

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
unit installerHelp;

{This class installs, configures and uninstalls FPC and Lazarus help.
It is called by the state machine in installerManager.

When installing, the class downloads FPC RTL/FCL/reference .CHM files,
because compiling them from source is very complicated, and FPC help is
fairly static.
An LCL help CHM is generated from the Lazarus sources and cross-reference
information in the FPC help.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore;
Const
  Sequences=
// Convention: help modules start with help
//FPC .CHM download
    'Declare helpfpc;'+
    {Not using cleanmodule as we're downloading;
    getmodule will detect existing docs and not
    redownload them}
    //'Cleanmodule helpfpc;'+
    'Getmodule helpfpc;'+
    'Buildmodule helpfpc;'+
    'End;'+
    //Remove FPC help:
    'Declare helpfpcuninstall;'+
    'CleanModule helpfpc;'+
    'UninstallModule helpfpc;'+
    'End;'+
    //Lazarus help
    {Note: we don't use helpfpc because that will put the
    help files in the FPC base directory, not in the
    Lazarus base directory
    }
    'Declare helplazarus;'+
    {Recent Lazarus compiles lhelp
    on demand once F1 is pressed. So we could disable it}
    'Requires lazbuild;'+
    {Not using cleanmodule as we're downloading;
    getmodule will detect existing docs and not
    redownload them}
    //'CleanModule helplazarus;'+
    'GetModule helplazarus;'+
    'BuildModule helplazarus;'+
    'ConfigModule helplazarus;'+
    'End;'+
    //Remove Lazarus help:
    'Declare helplazarusuninstall;'+
    'CleanModule helplazarus;'+
    'UninstallModule helplazarus;'+
    'End;'+
    //selective actions triggered with --only=SequenceName
    'Declare HelpFPCCleanOnly;'+
    'Cleanmodule helpfpc;'+
    'End;'+

    'Declare helplazarusclean;'+
    // This cleaning sequence will be called by --clean
    'Cleanmodule helplazarus;'+
    'End;'+

    'Declare HelpFPCGetOnly;'+
    'Getmodule helpfpc;'+
    'End;'+

    'Declare HelpLazarusGetOnly;'+
    'Getmodule helplazarus;'+
    'End;'+

    'Declare HelpFPCBuildOnly;'+
    'Buildmodule helpfpc;'+
    'End;'+

    'Declare HelpFPCBuildOnly;'+
    'Buildmodule helpfpc;'+
    'End;'+

    'Declare HelpLazarusBuildOnly;'+
    'Buildmodule helplazarus;'+
    'End;'+

    'Declare HelpFPCConfigOnly;'+
    'Configmodule helpfpc;'+
    'End;'+

    'Declare HelpLazarusConfigOnly;'+
    'Configmodule helplazarus;'+
    'End';
type

{ THelpInstaller }

THelpInstaller = class(TInstaller)
private
  InitDone:boolean;
  // Directory where help files are placed
  FTargetDirectory: string;
  // Directory where build_lcl_docs.exe is placed
  FBuildLCLDocsExeDirectory: string;
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; virtual;
  // internal initialisation, called from BuildModule,CleanModule,GetModule
  // and UnInstallModule but executed only once
  function InitModule:boolean; virtual;
  // Directory where docs will be installed.
  property TargetDirectory: string read FTargetDirectory;
public
  // Build module
  function BuildModule(ModuleName:string): boolean; override;
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Configure FPC or Lazarus to use the help
  function ConfigModule(ModuleName:string): boolean; override;
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  // Uninstall module
  function UnInstallModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

{ THelpFPCInstaller }

THelpFPCInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
  function InitModule:boolean; override;
public
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Configure FPC to use the help
  function ConfigModule(ModuleName:string): boolean; override;
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

{ THelpLazarusInstaller }

THelpLazarusInstaller = class(THelpInstaller)
private
  FFPCDirectory: string;
  FLazarusPrimaryConfigPath: string;
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
  function InitModule:boolean; override;
public
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Configure Lazarus to use the help
  function ConfigModule(ModuleName:string): boolean; override;
  // Root directory of FPC; needed for finding fpdoc tool
  property FPCDirectory: string write FFPCDirectory;
  // Configuration for Lazarus; required for configuration
  property LazarusPrimaryConfigPath: string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
  // Uninstall module
  function UnInstallModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

implementation

uses fpcuputil, processutils, FileUtil, updatelazconfig, dateutils;

{ THelpInstaller }

function THelpInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
end;

function THelpInstaller.InitModule: boolean;
var
  BinPath: string; //path where compiler is
  PlainBinPath: string; //the directory above e.g. c:\development\fpc\bin\i386-win32
begin
  result:=CheckAndGetNeededExecutables;
  if result then
  begin
    // Look for make etc in the current compiler directory:
    BinPath:=ExcludeTrailingPathDelimiter(ExtractFilePath(FCompiler));
    PlainBinPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BinPath)+'..');
    {$IFDEF MSWINDOWS}
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
    // at least one ; to be present in the path. If you only have one entry, you
    // can add PathSeparator without problems.
    // http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
    SetPath(BinPath+PathSeparator+
      PlainBinPath+PathSeparator+
      FMakeDir+PathSeparator+
      FSVNDirectory+PathSeparator+
      FBaseDirectory,false,false);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    SetPath(BinPath+PathSeparator+
    PlainBinPath,
    true,false);
    {$ENDIF UNIX}
  end;
end;

function THelpInstaller.BuildModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=BuildModuleCustom(ModuleName);
end;

function THelpInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
end;

function THelpInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=true;
end;

function THelpInstaller.GetModule(ModuleName: string): boolean;
const
  // Location of FPC CHM help zip
  // Link to 2.6 documentation: rtl, chm, and reference manuals, including .xct files, and as a bonus: lcl files:
  // http://sourceforge.net/projects/lazarus/files/Lazarus%20Documentation/Lazarus%201.0RC1/fpc-lazarus-doc-chm-1.0RC1.zip/download
  //
  // Older:
  // http://sourceforge.net/projects/freepascal/files/Documentation/2.6.0/doc-chm.zip/download
  // which links to
  // http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip
  //
  // Even older file on
  // http://sourceforge.net/projects/freepascal/files/Documentation/
  // that includes the lcl file
  // Snapshot alternative... that changes name... and is a .tar.bz2
  // ftp://freepascal.dfmk.hu/pub/lazarus/snapshots/fpc-lazarus-doc-chm-20120622.tar.bz2
  //Laz 1.0 version
  //FPC_CHM_URL='http://sourceforge.net/projects/lazarus/files/Lazarus%20Documentation/Lazarus%201.0/fpc-lazarus-doc-chm-1.0.zip/download';
  // Laz 1.2 version:
  FPC_CHM_URL='http://sourceforge.net/projects/lazarus/files/Lazarus%20Documentation/Lazarus%201.0.12/fpc-lazarus-doc-chm-1.0.12.zip/download';
var
  DocsZip: string;
  OperationSucceeded: boolean;
  ResultCode: longint;
begin
  result:=InitModule;
  if not result then exit;

  if FileExistsUTF8(FTargetDirectory+'fcl.chm') and
    FileExistsUTF8(FTargetDirectory+'rtl.chm') then
  begin
    OperationSucceeded:=true;
    infoln(ModuleName+': skipping docs download: FPC rtl.chm and fcl.chm already present in docs directory '+FTargetDirectory,etInfo);
  end
  else
  begin
    // Download FPC CHM docs zip into TargetDirectory.
    {Possible alternatives
    1. make chm -> requires latex!!!
    2. or
    c:\development\fpc\utils\fpdoc\fpdoc.exe --content=rtl.xct --package=rtl --descr=rtl.xml --output=rtl.chm --auto-toc --auto-index --make-searchable --css-file=C:\Development\fpc\utils\fpdoc\fpdoc.css  --format=chm
    ... but we'd need to include the input files extracted from the Make file.
    }
    OperationSucceeded:=true;
    ForceDirectoriesUTF8(FTargetDirectory);
    DocsZip := SysUtils.GetTempFileName + '.zip';
    try
      OperationSucceeded:=Download(FPC_CHM_URL,DocsZip);
    except
      on E: Exception do
      begin
        // Deal with timeouts, wrong URLs etc
        OperationSucceeded:=false;
        infoln(ModuleName+': Download failed. URL: '+FPC_CHM_URL+LineEnding+
          'Exception: '+E.ClassName+'/'+E.Message, etWarning);
      end;
    end;

    if OperationSucceeded then
    begin
      // Extract, overwrite, flatten path/junk paths
      // todo: test with spaces in path
      ResultCode:=ExecuteCommand(FUnzip+' -o -j -d '+IncludeTrailingPathDelimiter(FTargetDirectory)+' '+DocsZip,FVerbose);
      if ResultCode = 0 then
      begin
        SysUtils.deletefile(DocsZip); //Get rid of temp zip if not more needed for troubleshooting.
      end
      else
      begin
        OperationSucceeded := False;
        infoln(ModuleName+': unzip failed with resultcode: '+IntToStr(ResultCode),etwarning);
      end;
    end
    else
    begin
      infoln(ModuleName+': download failed. FPC_CHM_URL: '+FPC_CHM_URL,eterror);
    end;
  end;
  Result := OperationSucceeded;
end;

function THelpInstaller.UnInstallModule(ModuleName: string): boolean;
begin
//todo: implement help uninstall
  result:=true;
end;

constructor THelpInstaller.Create;
begin
  inherited Create;
end;

destructor THelpInstaller.Destroy;
begin
  inherited Destroy;
end;

{ THelpFPCInstaller }

function THelpFPCInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  // A no op right now...
  result:=true;
end;

function THelpFPCInstaller.InitModule: boolean;
begin
  infoln('THelpFPCInstaller: initialising...',etDebug);
  result:=false;
  if inherited InitModule then
  begin
    //todo: check with FreeVision FPCIDE to see if this is a sensible location.
    FTargetDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'doc'+DirectorySeparator+
      'ide'+DirectorySeparator; ;
    infoln('Module FPCHELP: documentation directory: '+FTargetDirectory,etInfo);
    result:=true;
  end;
end;

function THelpFPCInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited CleanModule(ModuleName);
  // Check for valid directory
  if not DirectoryExistsUTF8(FTargetDirectory) then
  begin
    infoln('HelpFPCInstaller CleanModule: directory '+FTargetDirectory+' does not exist. Exiting CleanModule.',etInfo);
    exit;
  end;
  if result then
  try
    { Delete .chm files and .xct (cross reference) files
      that could have been downloaded in FPC docs or created by fpcup }
    sysutils.DeleteFile(FTargetDirectory+'fcl.chm');
    sysutils.DeleteFile(FTargetDirectory+'fpdoc.chm');
    sysutils.DeleteFile(FTargetDirectory+'prog.chm');
    sysutils.DeleteFile(FTargetDirectory+'ref.chm');
    sysutils.DeleteFile(FTargetDirectory+'rtl.chm');
    sysutils.DeleteFile(FTargetDirectory+'toc.chm');
    sysutils.DeleteFile(FTargetDirectory+'user.chm');
    // Cross reference (.xct) files:
    sysutils.DeleteFile(FTargetDirectory+'fcl.xct');
    sysutils.DeleteFile(FTargetDirectory+'fpdoc.xct');
    sysutils.DeleteFile(FTargetDirectory+'prog.xct');
    sysutils.DeleteFile(FTargetDirectory+'ref.xct');
    sysutils.DeleteFile(FTargetDirectory+'rtl.xct');
    sysutils.DeleteFile(FTargetDirectory+'toc.xct');
    sysutils.DeleteFile(FTargetDirectory+'user.xct');
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog(ModuleName+' clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')',true);
      result:=false;
    end;
  end;
end;

function THelpFPCInstaller.ConfigModule(ModuleName: string): boolean;
begin
  Result:=inherited ConfigModule(ModuleName);
  //todo: implement config for fpide
end;

function THelpFPCInstaller.GetModule(ModuleName: string): boolean;
begin
  Result:=inherited GetModule(ModuleName);
end;

constructor THelpFPCInstaller.Create;
begin
  inherited Create;
end;

destructor THelpFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

{ THelpLazarusInstaller }

function THelpLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  BuildLCLDocsExe: string;
  BuildResult: integer;
  ExistingLCLHelp: string;
  FPDocExe: string;
  FPDocExes: TStringList;
  GeneratedLCLHelp: string;
  LazbuildExe: string;
  LCLDate: TDateTime;
  LHelpDirectory: string;
  OperationSucceeded:boolean;
begin
  // lhelp viewer is needed which Lazarus builds that on first run
  // However, it can be prebuilt by enabling it as an external module in fpcup.ini
  OperationSucceeded:=true;
  // The locations of the LCL.chm we generate and the existing one we can overwrite:
  ExistingLCLHelp:=FTargetDirectory+'lcl.chm';
  GeneratedLCLHelp:=FTargetDirectory+'lcl'+DirectorySeparator+'lcl.chm';

  if OperationSucceeded then
  begin
    // A safe, old value
    LCLDate:=EncodeDate(1910,01,01);
    try
      if FileExistsUTF8(ExistingLCLHelp) then
        LCLDate:=FileDateToDateTime(FileAgeUTF8(ExistingLCLHelp));
    except
      // Ignore exceptions, leave old date as is
    end;

    // Only consider building if lcl.chm does not exist
    // or is not read-only.
    // Then it should be old (> 7 days) or empty.
    // We assume that readonly means the user doesn't want to
    // overwrite.
    // Note: this still does not seem to go right. On Linux
    // without lcl.chm it detects the file as readonly...
    if FileExistsUTF8(ExistingLCLHelp) then
      infoln('Check if '+ExistingLCLHelp+' exists? Yes.',etInfo)
    else
      infoln('Check if '+ExistingLCLHelp+' exists? No.',etInfo);
    if (FileExistsUTF8(ExistingLCLHelp)=false) or
      (
      (FileUtil.FileIsReadOnlyUTF8(ExistingLCLHelp)=false)
      and
      ((DaysBetween(Now,LCLDate)>7)
      or (FileSize(ExistingLCLHelp)=0))
      )
      then
    begin
      BuildLCLDocsExe:=FBuildLCLDocsExeDirectory+'build_lcl_docs'+GetExeExt;
      if OperationSucceeded then
      begin
        // Only recompile build_lcl_docs.exe if needed
        if CheckExecutable(BuildLCLDocsExe, '--help', 'build_lcl_docs')=false then
        begin
          // Check for valid lazbuild.
          // Note: we don't check if we have a valid primary config path, but that will come out
          // in the next steps.
          LazbuildExe:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazbuild'+GetExeExt;;
          if CheckExecutable(LazbuildExe, '--help','lazbuild')=false then
          begin
            writelnlog(ModuleName+': No valid lazbuild executable found. Aborting.', true);
            OperationSucceeded:=false;
          end;

          if OperationSucceeded then
          begin
            // We have a working lazbuild; let's hope it works with primary config path as well
            // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
            ProcessEx.Executable := LazBuildExe;
            ProcessEx.Parameters.Clear;
            ProcessEx.Parameters.Add('--primary-config-path='+LazarusPrimaryConfigPath+'');
            ProcessEx.Parameters.Add(FBuildLCLDocsExeDirectory+'build_lcl_docs.lpr');
            infoln(ModuleName+': compiling build_lcl_docs help compiler:',etInfo);
            ProcessEx.Execute;
            if ProcessEx.ExitStatus <> 0 then
            begin
              writelnlog(ModuleName+': error compiling build_lcl_docs docs builder. Aborting.', true);
              OperationSucceeded := False;
            end;
          end;
        end;
      end;

      // Check for proper fpdoc
      { Preferably use the fpdoc in ./utils/fpdoc/ }
      FPDocExe:=IncludeTrailingPathDelimiter(FFPCDirectory)+
        'utils'+DirectorySeparator+
        'fpdoc'+DirectorySeparator+
        'fpdoc'+GetExeExt;
      if (CheckExecutable(FPDocExe, '--help', 'FPDoc')=false) then
      begin
        // Try again, in bin directory; newer FPC releases may have migrated to this
        FPDocExes:=FindAllFiles(IncludeTrailingPathDelimiter(FFPCDirectory)+'bin'+DirectorySeparator,
          'fpdoc'+GetExeExt,true);
        try
          if FPDocExes.Count>0 then FPDocExe:=FPDocExes[0]; //take only the first
          if (CheckExecutable(FPDocExe, '--help', 'FPDoc')=false) then
          begin
            writelnlog(ModuleName+': no valid fpdoc executable found ('+FPDocExe+'). Please recompile fpc.', true);
            OperationSucceeded := False;
          end
          else
          begin
            infoln(ModuleName+': found valid fpdoc executable.',etInfo);
          end;
        finally
          FPDocExes.Free;
        end;
      end;

      if OperationSucceeded then
      begin
        // Compile Lazarus LCL CHM help
        ProcessEx.Executable := BuildLCLDocsExe;
        // Make sure directory switched to that of the FPC docs,
        // otherwise paths to source files will not work.
        ProcessEx.CurrentDirectory:=FTargetDirectory;
        ProcessEx.Parameters.Clear;
        // Instruct build_lcl_docs to cross-reference FPC documentation by specifying
        // the directory that contains the fcl and rtl .xct files.
        // If those .xct files are not present, FPC 2.7.1 fpdoc will throw an exception
        ProcessEx.Parameters.Add('--fpcdocs');
        ProcessEx.Parameters.Add(FTargetDirectory);
        // Let build_lcl_docs know which fpdoc application to use:
        ProcessEx.Parameters.Add('--fpdoc');
        ProcessEx.Parameters.Add(FPDocExe);
        // Newer versions of fpc mess up the .css file location;
        // Exception at 00441644: Exception:
        // Can't find CSS file "..\fpdoc.css".
        //
        // So specify path explicitly
        // --css-file argument available since r42283
        ProcessEx.Parameters.Add('--css-file='+IncludeTrailingPathDelimiter(FFPCDirectory)+
          'utils'+DirectorySeparator+'fpdoc'+DirectorySeparator+'fpdoc.css');

        ProcessEx.Parameters.Add('--outfmt');
        ProcessEx.Parameters.Add('chm');
        { this will give a huge amount of warnings which should be fixed by
        fpdoc and/or the .chm files so are rather useless
        ProcessEx.Parameters.Add('--warnings'); //let tool show warnings as well
        }
        // Show application output if desired:
        if FVerbose then ProcessEx.OnOutput:=@DumpConsole;
        infoln(ModuleName+': compiling chm help docs:',etInfo);
        { The CHM file gets output into <lazarusdir>/docs/chm/lcl/lcl.chm
        Though that may work when adjusting the baseurl option in Lazarus for each
        CHM file, it's easier to move them to <lazarusdir>/docs/chm,
        which is picked up by the default Lazarus settings.
        The generated .xct file is an index file for fpdoc cross file links,
        used if you want to link to the chm from other chms.}
        ProcessEx.Execute;
        BuildResult:=ProcessEx.ExitStatus;
        if BuildResult <> 0 then
        begin
          writelnlog(ModuleName+': error creating chm help docs. build_lcl_docs exit status: '+inttostr(BuildResult), true);
          OperationSucceeded := False;
        end;
      end;

      if OperationSucceeded then
      begin
        // Move files if required
        if FileExistsUTF8(GeneratedLCLHelp) then
        begin
          if FileSize(GeneratedLCLHelp)>0 then
          begin
            infoln(ModuleName+': moving lcl.chm to docs directory',etInfo);
            OperationSucceeded:=MoveFile(GeneratedLCLHelp,ExistingLCLHelp);
          end
          else
          begin
            // File exists, but is empty. We might have an older file still present
            writelnlog(ModuleName+': WARNING: '+GeneratedLCLHelp+
            ' was created but is empty (perhaps due to FPC bugs). Lcl.chm may be out of date! Try running with --verbose to see build_lcl_docs error messages.', true);
            // Todo: change this once fixes for fpdoc chm generation are in fixes_26:
            OperationSucceeded:=true;
          end;
        end;
      end;
    end
    else
    begin
      // Indicate reason for not creating lcl.chm
      if FileUtil.FileIsReadOnlyUTF8(ExistingLCLHelp) then
        infoln(ModuleName+': not building LCL.chm as it is read only.',etInfo)
      else
        infoln(ModuleName+': not building LCL.chm as it is quite recent: '+FormatDateTime('YYYYMMDD',LCLDate),etInfo);
    end;
  end;
  result:=OperationSucceeded;
end;

function THelpLazarusInstaller.InitModule: boolean;
begin
  result:=false;
  infoln('THelpLazarusInstaller: initialising...',etDebug);
  if inherited InitModule then
  begin
    // This must be the directory of the build_lcl_docs project, otherwise
    // build_lcl_docs will fail; at least it won't pick up the FPC help files for cross references
    FTargetDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'docs'+DirectorySeparator+
      'chm'+DirectorySeparator;
    infoln('helplazarus: documentation directory: '+FTargetDirectory,etInfo);
    FBuildLCLDocsExeDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'docs'+DirectorySeparator+
      'html'+DirectorySeparator;;
    infoln('helplazarus: FBuildLCLDocsExeDirectory: '+FTargetDirectory,etDebug);
    result:=true;
  end;
end;

function THelpLazarusInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited CleanModule(ModuleName);
  // Check for valid directory
  if not DirectoryExistsUTF8(FTargetDirectory) then
  begin
    infoln('HelpLazarusInstaller CleanModule: directory '+FTargetDirectory+' does not exist. Exiting CleanModule.',etInfo);
    exit;
  end;
  if result then
  try
    { Delete .chm files and .xct (cross reference) files
      that could have been downloaded in FPC docs or created by fpcup }
    sysutils.DeleteFile(FTargetDirectory+'fcl.chm');
    sysutils.DeleteFile(FTargetDirectory+'fpdoc.chm');
    sysutils.DeleteFile(FTargetDirectory+'prog.chm');
    sysutils.DeleteFile(FTargetDirectory+'ref.chm');
    sysutils.DeleteFile(FTargetDirectory+'rtl.chm');
    sysutils.DeleteFile(FTargetDirectory+'lcl.chm');
    sysutils.DeleteFile(FTargetDirectory+'toc.chm');
    sysutils.DeleteFile(FTargetDirectory+'user.chm');
    // Cross reference (.xct) files:
    sysutils.DeleteFile(FTargetDirectory+'fcl.xct');
    sysutils.DeleteFile(FTargetDirectory+'fpdoc.xct');
    sysutils.DeleteFile(FTargetDirectory+'prog.xct');
    sysutils.DeleteFile(FTargetDirectory+'ref.xct');
    sysutils.DeleteFile(FTargetDirectory+'rtl.xct');
    sysutils.DeleteFile(FTargetDirectory+'lcl.xct');
    sysutils.DeleteFile(FTargetDirectory+'toc.xct');
    sysutils.DeleteFile(FTargetDirectory+'user.xct');
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog(ModuleName+' clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')',true);
      result:=false;
    end;
  end;
end;

function THelpLazarusInstaller.ConfigModule(ModuleName: string): boolean;
var
  LazarusConfig: TUpdateLazConfig;
begin
  result:=inherited ConfigModule(ModuleName);
  if result then
  begin
    result:=ForceDirectoriesUTF8(FLazarusPrimaryConfigPath);
  end
  else
  begin
    writelnlog('Lazarus help: error: could not create primary config path '+FLazarusPrimaryConfigPath);
  end;
  if result then
  begin
    LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
    try
      try
        {
        We don't need to set explicit paths as long as we use the defaults, e.g.
        $(LazarusDir)\docs\html and $(LazarusDir)\docs\chm
        http://wiki.lazarus.freepascal.org/Installing_Help_in_the_IDE#Installing_CHM_help_.28Lazarus_1.0RC1_and_later.29
        We could set it explicitly with
        LazarusConfig.SetVariable(HelpConfig,
          'Viewers/TChmHelpViewer/CHMHelp/FilesPath',
          IncludeTrailingPathDelimiter(FBaseDirectory)+'docs'+DirectorySeparator+'chm'+DirectorySeparator
          );
        }
        result:=true;
      except
        on E: Exception do
        begin
          result:=false;
          writelnlog('Lazarus help: Error setting Lazarus config: '+E.ClassName+'/'+E.Message, true);
        end;
      end;
    finally
      LazarusConfig.Free;
    end;
  end;
end;

function THelpLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
var
  LazarusConfig: TUpdateLazConfig;
begin
  Result:=inherited UnInstallModule(ModuleName);
  // Removing config not needed anymore since we use the default
end;

constructor THelpLazarusInstaller.Create;
begin
  inherited Create;
end;

destructor THelpLazarusInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

