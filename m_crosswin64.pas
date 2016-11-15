unit m_crosswin64;
{ Cross compiles from Windows x86/32 bit to Windows x64
Copyright (C) 2012-2013 Reinier Olislagers

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
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ TWin64 }

TWin64 = class(TCrossInstaller)
private

public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin64 }

function TWin64.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='';
  result:=true;
end;

{$ifndef FPCONLY}
function TWin64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function TWin64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:='';  
  result:=true;
  FBinsFound:=true;
end;

constructor TWin64.Create;
begin
  inherited Create;
  FCrossModuleName:='Win64';
  FTargetCPU:='x86_64';
  FTargetOS:='win64';
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  infoln('TWin64 crosscompiler loading',etDebug);
end;

destructor TWin64.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF WIN32}
var
  Win64:TWin64;

initialization
  Win64:=TWin64.Create;
  RegisterExtension(Win64.TargetCPU+'-'+Win64.TargetOS,Win64);
finalization
  Win64.Destroy;
{$ENDIF WIN32}
end.

