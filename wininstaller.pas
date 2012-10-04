unit winInstaller;

{ wininstaller: Windows installer creator

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
  Classes, SysUtils, installerUniversal {$IFDEF MSWINDOWS},registry{$ENDIF}, FileUtil {Requires LCL};

type

  { TWinInstaller }

  TWinInstaller = class(TUniversalInstaller)
    //todo: or descend from installermanager!?! we need probably laz dir+fpc dir
    //better option is to write it as a new module with our own properties=>we then
    //need to pass the universal installer more properties etc. that does seem clearest
  private
    FCreateInstallerBatch: string; //Location of create_installer.bat, which needs editing according to our situation
    FInnoSetupCompiler: string; //Path to the command line Inno Ssetup compiler (required)
    procedure FindInno;
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
  public
    property InnoSetupCompiler: string write FInnoSetupCompiler; //Path to the command line Inno Ssetup compiler (required)
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TWinInstaller }

procedure TWinInstaller.FindInno;
var
  CompileCommand: string='';
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    // Navigate to proper "directory":
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('\SOFTWARE\Classes\InnoSetupScriptFile\shell\Compile\Command') then
      CompileCommand:=Registry.ReadString(''); //read the value of the default name
    if CompileCommand<>'' then
      // Often something like
      //"d:\Program Files (x86)\Inno Setup 5\Compil32.exe" /cc "%1"
      CompileCommand:=Copy(CompileCommand,1,pos(uppercase(CompileCommand),'.EXE')+3);
    if Copy(CompileCommand,1,1)='"' then
      CompileCommand:=Copy(CompileCommand,2,length(CompileCommand));
    if (CompileCommand='') then CompileCommand:=FindDefaultExecutablePath('Compil32.exe');
    if (CompileCommand='') and (fileexistsutf8('C:\Program Files (x86)\Inno Setup 5\Compil32.exe')) then CompileCommand:='C:\Program Files (x86)\Inno Setup 5\Compil32.exe';
    if (CompileCommand='') and (fileexistsutf8('C:\Program Files\Inno Setup 5\Compil32.exe')) then CompileCommand:='C:\Program Files\Inno Setup 5\Compil32.exe';
    if CompileCommand<>'' then
    begin
      FInnoSetupCompiler:=CompileCommand;
    end;
  finally
    Registry.Free;
  end;
end;

function TWinInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  //edit fcreateinstallerbatch
  //todo: move to configmodule?!?
  {set environment vars
  SET ISCC="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
  SET LAZTEMPBUILDDIR="c:\temp\lazarusbuild"
  SET SVN="C:\Program Files\Subversion\bin\svn.exe"
  }

  {
  create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC
  or
  create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC IDE_WIDGETSET PATCHFILE CHMHELPFILES
  }

  {check installer.log}
end;

function TWinInstaller.InitModule: boolean;
begin

end;

constructor TWinInstaller.Create;
begin
  // Sensible default for an x64 Windows:
  FindInno;
  if FInnoSetupCompiler='' then
    FInnoSetupCompiler:='C:\Program Files (x86)\Inno Setup 5\Compil32.exe';
  //todo: set FCreateInstallerBatch depending on laz dir, check location
end;

destructor TWinInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

