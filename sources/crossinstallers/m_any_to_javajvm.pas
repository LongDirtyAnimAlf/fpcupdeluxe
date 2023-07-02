unit m_any_to_javajvm;

{ Cross compiles from e.g. Linux 64 bit (or any other OS with relevant binutils/libs) to Java JVM
Copyright (C) 2014 Reinier Olislagers

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

{
Debian: adding i386 libs/architecture support on e.g. x64 system
dpkg --add-architecture i386

Adapt (add) for other setups
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ Tany_javajvm }
Tany_javajvm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs({%H-}Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_javajvm }

function Tany_javajvm.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FLibsPath:='';

  ShowInfo('Libspath ignored; jasmin.jar will be downloaded into normal bin-dir.',etInfo);
  result:=True;

  if result then
  begin
    FLibsFound:=True;
    //This has to be added, but unfortunately, we do not know the location of it right here : do it in TFPCCrossInstaller.BuildModuleCustom
    //AddFPCCFGSnippet('-Fu'+'InstallDir\fpc\units\jvm-java\rtl\org\freepascal\rtl');
  end
  else
  begin
    //no libs yet: go on without them
    ShowInfo('Libspath ignored; it is optional for this cross compiler.',etInfo);
    FLibsFound:=True;
    result:=True;
  end;

end;

{$ifndef FPCONLY}
function Tany_javajvm.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_javajvm.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:='';
  FBinUtilsPath:='';

  result:=CheckJava;


  if result then
  begin
    FBinsFound:=true;
    // On Windows, Java often resides inside a directory with spaces; FPC does not like spaces, so use DOS-names.
    FBinUtilsPath:=ExtractShortPathNameUTF8(ExtractFilePath(GetJava));
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    SearchBinUtilsInfo(result);
  end
  else
  begin
    FAlreadyWarned:=true;
    ShowInfo('Java is needed. Please install java.');
  end;
end;

constructor Tany_javajvm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.jvm;
  FTargetOS:=TOS.java;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_javajvm.Destroy;
begin
  inherited Destroy;
end;

var
  any_javajvm:Tany_javajvm;

initialization
  any_javajvm:=Tany_javajvm.Create;
  RegisterCrossCompiler(any_javajvm.RegisterName,any_javajvm);
finalization
  any_javajvm.Destroy;

end.

