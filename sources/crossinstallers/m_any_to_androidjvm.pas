unit m_any_to_androidjvm;

{ Cross compiles from e.g. Linux 64 bit (or any other OS with relevant binutils/libs) to Android JVM
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

{ Tany_androidjvm }
Tany_androidjvm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs({%H-}Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_androidjvm }

function Tany_androidjvm.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FLibsPath:='';

  //FLibsPath:='where is jasmin.jar'
  //for now, jasmin.jar will be downloaded into normal bin-dir !!

  result:=True;
  FLibsFound:=True;
end;

function Tany_androidjvm.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

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

constructor Tany_androidjvm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.jvm;
  FTargetOS:=TOS.android;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_androidjvm.Destroy;
begin
  inherited Destroy;
end;

var
  any_androidjvm:Tany_androidjvm;

initialization
  any_androidjvm:=Tany_androidjvm.Create;
  RegisterCrossCompiler(any_androidjvm.RegisterName,any_androidjvm);

finalization
  any_androidjvm.Destroy;

end.

