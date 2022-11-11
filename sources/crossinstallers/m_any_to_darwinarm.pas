unit m_any_to_darwinarm;

{ Cross compiles to Darwin ARM
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

{ Tany_darwinarm }
Tany_darwinarm = class(Tany_apple)
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

{ Tany_darwinarm }

function Tany_darwinarm.GetOSName:string;
begin
  result:=iOSNAME
end;
function Tany_darwinarm.GetLibName:string;
begin
  result:='libc.dylib';
end;
function Tany_darwinarm.GetTDBLibName:string;
begin
  result:='libc.tbd';
end;

function Tany_darwinarm.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function Tany_darwinarm.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor Tany_darwinarm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor Tany_darwinarm.Destroy;
begin
  inherited Destroy;
end;

{$ifndef Darwin}
var
  any_darwinarm:Tany_darwinarm;

initialization
  any_darwinarm:=Tany_darwinarm.Create;
  RegisterCrossCompiler(any_darwinarm.RegisterName,any_darwinarm);

finalization
  any_darwinarm.Destroy;

{$endif Darwin}
end.

