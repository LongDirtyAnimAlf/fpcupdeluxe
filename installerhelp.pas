{ Help installer/uninstaller unit for fpcup
Copyright (C) 2012 Ludo Brands, Reinier Olislagers

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
    'Cleanmodule helpfpc;'+
    'Getmodule helpfpc;'+
    'Buildmodule helpfpc;'+
    'End;'+
    //Lazarus help
    {Note: we don't use helpfpc because that will put the
    help files in the FPC base directory, not in the
    Lazarus base directory
    }
    //todo: replace help with lazhelp in main state machine
    'Declare helplazarus;'+
    'Requires BIGIDE;'+
    'Requires lhelp;'+
    'CleanModule helplazarus;'+
    'BuildModule helplazarus;'+
    'ConfigModule helplazarus;'+
    'End;'+
    //selective actions triggered with --only=SequenceName
    'Declare HelpFPCCleanOnly;'+
    'Cleanmodule helpfpc;'+
    'End;'+

    'Declare HelpLazarusCleanOnly;'+
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
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; virtual;
  // internal initialisation, called from BuildModule,CLeanModule,GetModule
  // and UnInstallModule but executed only once
  function InitModule:boolean; virtual;
  // Directory where docs will be installed.
  property TargetDirectory: string read FTargetDirectory;
public
  // Build module
  function BuildModule(ModuleName:string): boolean; override;
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
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
  // Root directory of FPC; needed for finding fpdoc tool
  property FPCDirectory: string read FFPCDirectory write FFPCDirectory;
  // Configuration for Lazarus; required for building lhelp, as well as configuration
  property LazarusPrimaryConfigPath: string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
  constructor Create;
  destructor Destroy; override;
end;

implementation

uses fpcuputil, processutils, FileUtil;

{ THelpInstaller }

function THelpInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
end;

function THelpInstaller.InitModule: boolean;
begin
  result:=true;
end;

function THelpInstaller.BuildModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  result:=true;
end;

function THelpInstaller.CleanModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  result:=true;
end;

function THelpInstaller.GetModule(ModuleName: string): boolean;
const
  // Location of FPC CHM help zip
  // Link to 2.6 documentation: rtl, chm, and reference manuals, including .xct files
  // http://sourceforge.net/projects/freepascal/files/Documentation/2.6.0/doc-chm.zip/download
  // which links to
  // http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip
  //
  // Note: there's also an older file on
  // http://sourceforge.net/projects/freepascal/files/Documentation/
  // that includes the lcl file
  FPC_CHM_URL='http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip';
var
  DocsZip: string;
  OperationSucceeded: boolean;
  ResultCode: longint;
begin
  if not InitModule then exit;
  // Download FPC CHM docs zip into TargetDirectory.
  {Possible alternatives
  1. make chm -> requires latex!!!
  2. or
  c:\development\fpc\utils\fpdoc\fpdoc.exe --content=rtl.xct --package=rtl --descr=rtl.xml --output=rtl.chm --auto-toc --auto-index --make-searchable --css-file=C:\Development\fpc\utils\fpdoc\fpdoc.css  --format=chm
  ... but we'd need to include the input files extracted from the Make file.
  }
  OperationSucceeded:=true;
  ForceDirectories(TargetDirectory);
  DocsZip := SysUtils.GetTempFileName + '.zip';
  try
    OperationSucceeded:=Download(FPC_CHM_URL,DocsZip);
  except
    on E: Exception do
    begin
      // Deal with timeouts, wrong URLs etc
      OperationSucceeded:=false;
      infoln(ModuleName+': Download failed. URL: '+FPC_CHM_URL+LineEnding+
        'Exception: '+E.ClassName+'/'+E.Message);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite, flatten path/junk paths
    // todo: test with spaces in path
    if ExecuteCommandHidden(FUnzip,'-o -j -d '+IncludeTrailingPathDelimiter(TargetDirectory)+' '+DocsZip,FVerbose)= 0 then
    begin
      SysUtils.deletefile(DocsZip); //Get rid of temp zip if not more needed for troubleshooting.
    end
    else
    begin
      OperationSucceeded := False;
      infoln(ModuleName+': unzip failed with resultcode: '+IntToStr(ResultCode));
    end;
  end
  else
  begin
    infoln(ModuleName+': download failed. FPC_CHM_URL: '+FPC_CHM_URL);
  end;

  if UpperCase(ModuleName)='HELPLAZARUS' then
  begin
    // Additionally, build LCL help using FPC sources
  end;
  Result := OperationSucceeded;
end;

function THelpInstaller.UnInstallModule(ModuleName: string): boolean;
begin

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

end;

function THelpFPCInstaller.InitModule: boolean;
begin
  result:=false;
  if inherited InitModule then
  begin
    //todo: check with FreeVision FPCIDE to see if this is a sensible location.
    //todo: why is the BaseDirectory property write-only? Why use FBaseDirectory?
    FTargetDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'doc'+DirectorySeparator+
      'ide'+DirectorySeparator; ;
    result:=true;
  end;
end;

function THelpFPCInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited CleanModule(ModuleName);
  if result then
  try
    { Delete .chm files and .xct (cross reference) files
      that could have been downloaded in FPC docs or created by fpcup }
    sysutils.DeleteFile(TargetDirectory+'fcl.chm');
    sysutils.DeleteFile(TargetDirectory+'fpdoc.chm');
    sysutils.DeleteFile(TargetDirectory+'prog.chm');
    sysutils.DeleteFile(TargetDirectory+'ref.chm');
    sysutils.DeleteFile(TargetDirectory+'rtl.chm');
    sysutils.DeleteFile(TargetDirectory+'toc.chm');
    sysutils.DeleteFile(TargetDirectory+'user.chm');
    // Cross reference (.xct) files:
    sysutils.DeleteFile(TargetDirectory+'fcl.xct');
    sysutils.DeleteFile(TargetDirectory+'fpdoc.xct');
    sysutils.DeleteFile(TargetDirectory+'prog.xct');
    sysutils.DeleteFile(TargetDirectory+'ref.xct');
    sysutils.DeleteFile(TargetDirectory+'rtl.xct');
    sysutils.DeleteFile(TargetDirectory+'toc.xct');
    sysutils.DeleteFile(TargetDirectory+'user.xct');
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog(ModuleName+' clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')',true);
      result:=false;
    end;
  end;
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
  OperationSucceeded:boolean;
begin
  OperationSucceeded:=true;
  if OperationSucceeded then
  begin
    // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
    ProcessEx.Executable := IncludeTrailingPathDelimiter(FBaseDirectory) + 'lazbuild';
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('--primary-config-path='+LazarusPrimaryConfigPath+'');
    ProcessEx.Parameters.Add(TargetDirectory+'build_lcl_docs.lpr');
    infoln(ModuleName+': compiling build_lcl_docs help compiler:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    // Compile Lazarus LCL CHM help
    ProcessEx.Executable := TargetDirectory+'build_lcl_docs'+GetExeExt;
    // Make sure directory switched to that of build_lcl_docs,
    // otherwise paths to source files will not work.
    ProcessEx.CurrentDirectory:=TargetDirectory;
    ProcessEx.Parameters.Clear;
    // Instruct build_lcl_docs to cross-reference FPC documentation by specifying
    // the directory that contains the fcl and rtl .xct files:
    ProcessEx.Parameters.Add('--fpcdocs');
    ProcessEx.Parameters.Add(TargetDirectory);
    // Let build_lcl_docs know which fpdoc application to use:
    ProcessEx.Parameters.Add('--fpdoc');
    { Use the fpdoc in ./utils/fpdoc/, as the compiler directory
    can be different between Unix+Windows }
    ProcessEx.Parameters.Add(FPCDirectory+
    'utils'+DirectorySeparator+
    'fpdoc'+DirectorySeparator+
    'fpdoc'+GetExeExt);
    ProcessEx.Parameters.Add('--outfmt');
    ProcessEx.Parameters.Add('chm');
    infoln('Lazarus: compiling chm help docs:');
    { The CHM file gets output into <lazarusdir>/docs/html/lcl/lcl.chm
    Though that may work when adjusting the baseurl option in Lazarus for each
    CHM file, it's easier to move them to <lazarusdir>/docs/html,
    which is also suggested by the wiki.
    The generated .xct file is an index file for fpdoc cross file links,
    used if you want to link to the chm from other chms.}
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    // Move files if required
    if FileExistsUTF8(TargetDirectory+
      'lcl'+DirectorySeparator+
      'lcl.chm') then
    begin
      infoln(ModuleName+': moving lcl.chm to docs directory');
      // Move help file to doc directory
      OperationSucceeded:=MoveFile(TargetDirectory+
        'lcl'+DirectorySeparator+
        'lcl.chm',
        TargetDirectory+
        'lcl.chm');
    end;
  end;
  result:=OperationSucceeded;
end;

function THelpLazarusInstaller.InitModule: boolean;
begin
  result:=false;
  if inherited InitModule then
  begin
    // This must be the directory of the build_lcl_docs project, otherwise
    // build_lcl_docs will fail; at least it won't pick up the FPC help files for cross references
    FTargetDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'docs'+DirectorySeparator+
      'html'+DirectorySeparator; ;
    result:=true;
  end;
end;

function THelpLazarusInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited CleanModule(ModuleName);
  if result then
  try
    { Delete .chm files and .xct (cross reference) files
      that could have been downloaded in FPC docs or created by fpcup }
    sysutils.DeleteFile(TargetDirectory+'fcl.chm');
    sysutils.DeleteFile(TargetDirectory+'fpdoc.chm');
    sysutils.DeleteFile(TargetDirectory+'prog.chm');
    sysutils.DeleteFile(TargetDirectory+'ref.chm');
    sysutils.DeleteFile(TargetDirectory+'rtl.chm');
    sysutils.DeleteFile(TargetDirectory+'lcl.chm');
    sysutils.DeleteFile(TargetDirectory+'toc.chm');
    sysutils.DeleteFile(TargetDirectory+'user.chm');
    // Cross reference (.xct) files:
    sysutils.DeleteFile(TargetDirectory+'fcl.xct');
    sysutils.DeleteFile(TargetDirectory+'fpdoc.xct');
    sysutils.DeleteFile(TargetDirectory+'prog.xct');
    sysutils.DeleteFile(TargetDirectory+'ref.xct');
    sysutils.DeleteFile(TargetDirectory+'rtl.xct');
    sysutils.DeleteFile(TargetDirectory+'lcl.xct');
    sysutils.DeleteFile(TargetDirectory+'toc.xct');
    sysutils.DeleteFile(TargetDirectory+'user.xct');
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog(ModuleName+' clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')',true);
      result:=false;
    end;
  end;
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

