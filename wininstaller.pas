unit winInstaller;

{ wininstaller: Windows installer creator
  See $(LazarusDir)\tools\install\win\readme.txt for manual install instructions
  which this module tries to follow as much as possible.

  Copyright (c) 2012 Reinier Olislagers
  Licensed at your choice as MIT or modified LGPL - see below.
}
{
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
{
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

{$IFDEF MSWINDOWS}
// for now, keep this Windows-only (should still run on Wine)
uses
  Classes, SysUtils, installerCore,registry {Requires LCL}, fpcuputil;

type

  { TWinInstaller }

  TWinInstaller = class(TInstaller)
  private
    FFPCBuildDir: string; //Location of fpcbuild sources
    FFPCDir: string; //Location of FPC sources
    FLazarusBinaryDir: string; //Location of Lazarus binaries repository
    FInstallerBuildDir: string; //Directory where the installer script builds the installer (which must not exist yet)
    FInnoSetupCompiler: string; //Path to the command line Inno Ssetup compiler (required)
    FLazarusDir: string;
    FLazarusPrimaryConfigPath: string;
    procedure FindInno;
  public
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    property InnoSetupCompiler: string write FInnoSetupCompiler; //Path to the command line Inno Ssetup compiler (required)
    // Directory with FPC build files repository
    property FPCBuildDir:string write FFPCBuildDir;
    // FPC base directory
    property FPCDir:string read FFPCDir write FFPCDir;
    // Directory with Lazarus binaries repository
    property LazarusBinaryDir:string write FLazarusBinaryDir;
    // Lazarus base directory
    property LazarusDir:string read FLazarusDir write FLazarusDir;
    // Lazarus primary config path
    property LazarusPrimaryConfigPath:string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
    constructor Create(InstallDirectory, FPCCompiler: string; Verbosity: boolean);
    destructor Destroy; override;
  end;
{$ENDIF MSWINDOWS}

implementation
{$IFDEF MSWINDOWS}
uses
  FileUtil, LazFileUtils, LazUTF8;

const
  ClassName = 'TWinInstaller';
{ TWinInstaller }

procedure TWinInstaller.FindInno;
// Finds Inno Compiler command line compiler executable location.
// Note: this is iscc, different from the GUI Inno executable
// Note: if you want to support Win9x, you'll need to use an ANSI Inno Setup
// version lower than 5.5.0
const
  CommandLineCompiler='iscc.exe';
var
  CompileCommand: string='';
  ProgramFiles: string;
  ProgramFilesx86: string;

  Registry: TRegistry;
begin
  ProgramFiles:=GetEnvironmentVariable('ProgramFiles');
  ProgramFilesx86:=GetEnvironmentVariable('ProgramFiles(x86)');
  Registry := TRegistry.Create;
  try
    // Navigate to proper "directory":
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('\SOFTWARE\Classes\InnoSetupScriptFile\shell\Compile\Command') then
      CompileCommand:=Registry.ReadString(''); //read the value of the default name
    // Often something like
    //"c:\Program Files (x86)\Inno Setup 5\Compil32.exe" /cc "%1"
    if CompileCommand<>'' then
      CompileCommand:=Copy(CompileCommand,1,pos('.EXE',uppercase(CompileCommand))+3);
    if Copy(CompileCommand,1,1)='"' then
      CompileCommand:=Copy(CompileCommand,2,length(CompileCommand));
    // Replace GUI exe with command line compiler
    if CompileCommand<>'' then
      CompileCommand:=StringReplace(UpperCase(CompileCommand),'COMPIL32','ISCC',[rfReplaceAll]);
    if (CompileCommand='') then
      CompileCommand:=FindDefaultExecutablePath('Compil32.exe');
    if (CompileCommand='') and (fileexistsutf8(ProgramFiles+'\Inno Setup 5\'+CommandLineCompiler)) then
      CompileCommand:=ProgramFiles+'\Inno Setup 5\'+CommandLineCompiler;
    if (CompileCommand='') and (fileexistsutf8(ProgramFilesx86+'\Inno Setup 5\'+CommandLineCompiler)) then
      CompileCommand:=ProgramFilesx86+'\Inno Setup 5\'+CommandLineCompiler;
    if CompileCommand<>'' then
    begin
      FInnoSetupCompiler:=CompileCommand;
    end;
  finally
    Registry.Free;
  end;
end;

function TWinInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  HelpFileDir: string;
  InstallerBatchDir: string; //directory where installer batch script is; will contain log and output dir with installer
begin
  // todo: split up, move to config, perhaps make dirs properties etc
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  FSVNClient.ModuleName:=ModuleName;
  FSVNClient.Verbose:=FVerbose;
  FSVNClient.ExportOnly:=FExportOnly;
  infoln('TWinInstaller: creating Lazarus installer. This may take a while...',etInfo);

  // Basedirectory = install directory from fpcup.ini/universal module.
  // We use it to put SVN repos needed for building.
  if FFPCBuildDir='' then
    FFPCBuildDir:=IncludeTrailingPathDelimiter(FBaseDirectory)+'fpcbuild';
  if FLazarusBinaryDir='' then
    FLazarusBinaryDir:=IncludeTrailingPathDelimiter(FBaseDirectory)+'lazbin';

  InstallerBatchDir:=IncludeTrailingPathDelimiter(FLazarusDir)+'tools\install\win';

  //checkout fpc build sources svn checkout
  //This repository includes the full FPC sources as well...
  if FVerbose then WritelnLog(ClassName+': Getting FPC build repository',true);
  ForceDirectory(FFPCBuildDir);
  FSVNClient.LocalRepository:=FFPCBuildDir;
  // Using the fixes version of FPC hardcoded; probably we officially need latest stable FPC...
  //todo: perhaps link this to the actual version of FPC used in the regular install?
  FSVNClient.Repository:='http://svn.freepascal.org/svn/fpcbuild/branches/fixes_3_0';
  FSVNClient.CheckOutOrUpdate;

  //checkout laz binaries
  if FVerbose then WritelnLog(ClassName+': Getting Lazarus binaries repository',true);
  ForceDirectories(FLazarusBinaryDir);
  FSVNClient.LocalRepository:=FLazarusBinaryDir;
  // Will have at least i386, x64 and arm-wince subfolders
  FSVNClient.Repository:='http://svn.freepascal.org/svn/lazarus/binaries/';
  FSVNClient.CheckOutOrUpdate;

  // Lazbuilddir may not exist (or should be empty) - so if it is there, remove it
  FInstallerBuildDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'lazinstaller';
  if DirectoryExistsUTF8(FInstallerBuildDir) then
  begin
    infoln('Deleting temporary Lazarus installer build directory '+FInstallerBuildDir+' before running installer creator.',etInfo);
    DeleteDirectory(FInstallerBuildDir,false);
  end;

  //Basically a copy from the help installer - without trailing delimiter
  HelpFileDir:=IncludeTrailingPathDelimiter(FLazarusDir)+
      'docs'+DirectorySeparator+
      'chm';

  // Feed this environment to the batch file. In older Laz revisions, double quoting is required
  // Setup compiler exe:
  ProcessEx.Environment.SetVar('ISCC','"'+FInnoSetupCompiler+'"');
  // SVN executable:
  ProcessEx.Environment.SetVar('SVN','"'+FSVNClient.RepoExecutable+'"');
  // svnversion exe:
  ProcessEx.Environment.SetVar('SVNVER','"'+ExtractFilePath(FSVNClient.RepoExecutable)+'svnversion'+GetExeExt+'"');

  // Provide this dir without quotes; may work with them but output looks weird
  ProcessEx.Environment.SetVar('LAZTEMPBUILDDIR',ExcludeLeadingPathDelimiter(FInstallerBuildDir));

  {
  create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC
  or
  create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC IDE_WIDGETSET PATCHFILE CHMHELPFILES
  where:
  FPCSVNDIR: Path to the fpc sources checked out of svn (see A.3) => i.e. the BUILD sources, not regular FPC source
  LAZSVNDIR: Path to the lazarus sources checked out of svn => FLazarusDir
  LAZSVNBINDIR: Path to the svn lazarus binaries (see A.5)
  RELEASE_PPC: Path to the FPC compiler required to start the build of fpc it FPCSVNDIR (see A.6)
  IDE_WIDGETSET: Optional: IDE widgetset to be created. If not needed: don't enter it or use ""
  PATCHFILE: Optional: name of FPC patch file for the FPC sources. If not needed: don't enter it or use ""
  CHMHELPFILES: Optional: directory containing CHM help files to be included in the installer (see A.7). If not needed: don't enter it or use ""
  }
  ProcessEx.Executable := IncludeTrailingPathDelimiter(InstallerBatchDir)+'create_installer.bat';
  // MUST be set to create_installer.bat otherwise it can't find the fpcbuild/lazbuild scripts
  ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(InstallerBatchDir);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(FFPCBuildDir)); //FPCSVNDIR
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(FLazarusDir)); //LAZSVNDIR
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(FLazarusBinaryDir)); //LAZSVNBINDIR
  // Should officially be a bootstrap compiler but should work with current compiler:
  ProcessEx.Parameters.Add(FCompiler); //RELEASE_PPC
  ProcessEx.Parameters.Add('""'); //an empty parameter as IDE_WIDGETSET
  ProcessEx.Parameters.Add('""'); //an empty parameter as PATCHFILE
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(HelpFileDir)); //CHMHELPFILES
  if FVerbose then WritelnLog(ClassName+': Running '+ProcessEx.Executable,true);
  ProcessEx.Execute;

  if ProcessEx.ExitStatus <> 0 then
  begin
    result := False;
    WritelnLog(ClassName+': Failed to create installer; '+ProcessEx.Executable+' returned '+inttostr(ProcessEx.ExitStatus)+LineEnding+
      'Installer log at '+IncludeTrailingPathDelimiter(InstallerBatchDir)+'installer.log',true);
  end
  else
  begin
    // Batch file ended, but no idea if it actually was succesful because it does not return result codes.
    // So removing the log
    //DeleteFile(IncludeTrailingPathDelimiter(InstallerBatchDir)+'installer.log');
    infoln('TWinInstaller: finished running the installer creator. If it worked, the installer is in '+IncludeTrailingPathDelimiter(InstallerBatchDir)+'output',etInfo);
    result := True;
  end;
end;

constructor TWinInstaller.Create(InstallDirectory, FPCCompiler: string; Verbosity: boolean);
begin
  inherited Create;
  FBaseDirectory:=InstallDirectory;
  FVerbose:=Verbosity;
  FCompiler:=FPCCompiler;
  // Sensible default for an x64 Windows:
  FindInno;
  if FInnoSetupCompiler='' then
    FInnoSetupCompiler:='C:\Program Files (x86)\Inno Setup 5\Compil32.exe';
end;

destructor TWinInstaller.Destroy;
begin
  inherited Destroy;
end;
{$ENDIF MSWINDOWS}
end.

