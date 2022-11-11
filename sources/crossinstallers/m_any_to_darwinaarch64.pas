unit m_any_to_darwinaarch64;

{ Cross compiles to Darwin ARM64
Copyright (C) 2014 Reinier Olislagers / DonAlfredo

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
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_apple_base, fpcuputil;

type

{ Tany_darwinaarch64 }
Tany_darwinaarch64 = class(Tany_apple)
protected
  function GetOSName:string;override;
  function GetLibName:string;override;
  function GetTDBLibName:string;override;
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_darwinaarch64 }

function Tany_darwinaarch64.GetOSName:string;
var
  j:dword;
begin
  j:=CalculateNumericalVersion(FPCVersion);
  if (j<>0) AND (j<CalculateFullVersion(3,2,2)) then
    result:=iOSNAME
  else
    result:=macOSNAME;
end;
function Tany_darwinaarch64.GetLibName:string;
begin
  result:='libc.dylib';
end;
function Tany_darwinaarch64.GetTDBLibName:string;
begin
  result:='libc.tbd';
end;

function Tany_darwinaarch64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function Tany_darwinaarch64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  //cpu_armv84a
end;

constructor Tany_darwinaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor Tany_darwinaarch64.Destroy;
begin
  inherited Destroy;
end;

{$ifndef Darwin}
var
  any_darwinaarch64:Tany_darwinaarch64;

initialization
  any_darwinaarch64:=Tany_darwinaarch64.Create;
  RegisterCrossCompiler(any_darwinaarch64.RegisterName,any_darwinaarch64);

finalization
  any_darwinaarch64.Destroy;

{$endif Darwin}
end.

