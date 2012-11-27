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

uses
  Classes, SysUtils, installerCore {$IFDEF MSWINDOWS},registry{$ENDIF}, FileUtil {Requires LCL};

type

  { TWinInstaller }

  TWinInstaller = class(TInstaller)
  private
    FFPCBuildDir: string; //Location of fpcbuild sources
    FFPCDir: string;
    FLazarusBinaryDir: string; //Location of Lazarus binaries
    FInstallerBuildDir: string; //Directory where the installer script builds the installer
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
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  ClassName = 'TWinInstaller';
{ TWinInstaller }

procedure TWinInstaller.FindInno;
// Finds Inno Compiler executable location.
// Note: if you want to support Win9x, you'll need to use an ANSI Inno Setup
// version lower than 5.5.0
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
    if CompileCommand<>'' then
      // Often something like
      //"c:\Program Files (x86)\Inno Setup 5\Compil32.exe" /cc "%1"
      CompileCommand:=Copy(CompileCommand,1,pos('.EXE',uppercase(CompileCommand))+3);
    if Copy(CompileCommand,1,1)='"' then
      CompileCommand:=Copy(CompileCommand,2,length(CompileCommand));
    if (CompileCommand='') then
      CompileCommand:=FindDefaultExecutablePath('Compil32.exe');
    if (CompileCommand='') and (fileexistsutf8(ProgramFiles+'\Inno Setup 5\Compil32.exe')) then
      CompileCommand:=ProgramFiles+'\Inno Setup 5\Compil32.exe';
    if (CompileCommand='') and (fileexistsutf8(ProgramFilesx86+'\Inno Setup 5\Compil32.exe')) then
      CompileCommand:=ProgramFilesx86+'\Inno Setup 5\Compil32.exe';
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
  TempDir: string; //use for building installer, current dir for batch etc.
begin
  // todo: split up, move to config, perhaps make dirs properties etc
  FSVNClient.Verbose:=FVerbose;

  TempDir:=IncludeTrailingPathDelimiter(GetTempDir(false));

  //checkout fpc build sources svn checkout
  //This repository includes the full FPC sources as well...
  if FVerbose then WritelnLog(ClassName+': Getting FPC build repository',true);
  ForceDirectory(FFPCBuildDir);
  FSVNClient.LocalRepository:=FFPCBuildDir;
  FSVNClient.Repository:='http://svn.freepascal.org/svn/fpcbuild/branches/fixes_2_6_0';
  FSVNClient.CheckOutOrUpdate;

  //checkout laz binaries
  if FVerbose then WritelnLog(ClassName+': Getting Lazarus binaries repository',true);
  ForceDirectories(FLazarusBinaryDir);
  FSVNClient.LocalRepository:=FLazarusBinaryDir;
  //todo: adapt for win64 (x86_64-win64/)
  FSVNClient.Repository:='http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/';
  FSVNClient.CheckOutOrUpdate;

  FInstallerBuildDir:=TempDir+'lazinstaller';
  ForceDirectories(FInstallerBuildDir);

  //Basically a copy from the help installer - without trailing delimiter
  HelpFileDir:=IncludeTrailingPathDelimiter(FLazarusDir)+
      'docs'+DirectorySeparator+
      'chm';

  // Feed this environment to the batch file:
  // Setting iscc will go wrong in the batch file (bug 23385),
  // so a solution is to add it to the path:
  //ProcessEx.Environment.SetVar('ISCC',FInnoSetupCompiler);
  SetPath(ExtractFileDir(FInnoSetupCompiler),true);
  ProcessEx.Environment.SetVar('ISCC','');

  //Same goes for svn:
  SetPath(ExtractFileDir(FSVNClient.SVNExecutable),true);
  ProcessEx.Environment.SetVar('SVN','');

  ProcessEx.Environment.SetVar('LAZTEMPBUILDDIR',ExcludeLeadingPathDelimiter(FInstallerBuildDir));

  {
  create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC
  or
  create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC IDE_WIDGETSET PATCHFILE CHMHELPFILES
  where:
  FPCSVNDIR: Path to the fpc sources checked out of svn (see A.3)
  LAZSVNDIR: Path to the lazarus sources checked out of svn => FLazarusDir
  LAZSVNBINDIR: Path to the svn lazarus binaries (see A.5)
  RELEASE_PPC: Path to the FPC compiler required to start the build of fpc it FPCSVNDIR (see A.6)
  IDE_WIDGETSET: Optional: IDE widgetset to be created. If not needed: don't enter it or use ""
  PATCHFILE: Optional: name of FPC patch file for the FPC sources. If not needed: don't enter it or use ""
  CHMHELPFILES: Optional: directory containing CHM help files to be included in the installer (see A.7). If not needed: don't enter it or use ""
  }
  ProcessEx.Executable := IncludeTrailingPathDelimiter(FLazarusDir)+'tools\install\win\create_installer.bat';
  ProcessEx.CurrentDirectory:=TempDir;
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(FFPCDir)); //FPCSVNDIR
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(FLazarusDir)); //LAZSVNDIR
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(FLazarusBinaryDir)); //LAZSVNBINDIR
  // Should officially be a bootstrap compiler but should work with current compiler:
  ProcessEx.Parameters.Add(FCompiler); //RELEASE_PPC
  ProcessEx.Parameters.Add(''); //IDE_WIDGETSET
  ProcessEx.Parameters.Add(''); //PATCHFILE
  ProcessEx.Parameters.Add(ExcludeTrailingPathDelimiter(HelpFileDir)); //CHMHELPFILES
  if FVerbose then WritelnLog(ClassName+': Running '+ProcessEx.Executable,true);
  ProcessEx.Execute;

  //todo: Copy over installer from output subdir
  if ProcessEx.ExitStatus <> 0 then
  begin
    result := False;
    WritelnLog(ClassName+': Failed to create installer; '+ProcessEx.Executable+' returned '+inttostr(ProcessEx.ExitStatus)+LineEnding+
      'Installer log at '+TempDir+'installer.log',true);
  end
  else
  begin
    result := True;
  end;

  {check installer.log}
end;

constructor TWinInstaller.Create;
begin
  inherited Create;
  // Sensible default for an x64 Windows:
  FindInno;
  if FInnoSetupCompiler='' then
    FInnoSetupCompiler:='C:\Program Files (x86)\Inno Setup 5\Compil32.exe';
  // Some defaults:
  FFPCBuildDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'fpcbuild';
  FLazarusBinaryDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'lazbin';
end;

destructor TWinInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

