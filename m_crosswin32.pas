unit m_crosswin32;
{ Cross compiles from Windows x64 to Windows x86
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

{ TWin32 }

TWin32 = class(TCrossInstaller)
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

{ TWin32 }

function TWin32.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='';
  result:=true;
end;

{$ifndef FPCONLY}
function TWin32.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function TWin32.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:='';
  result:=true;
  FBinsFound:=true;
end;

constructor TWin32.Create;
begin
  inherited Create;
  FCrossModuleName:='Win32';
  FBinUtilsPath:=''; //override parent that has a warning text here
  FBinUtilsPrefix:=''; //override parent that has a warning text here
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  FLibsPath:='';
  FTargetCPU:='i386';
  FTargetOS:='win32';
  infoln('TWin32 crosscompiler loading',etDebug);
end;

destructor TWin32.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF WIN64}
var
  Win32:TWin32;

initialization
  Win32:=TWin32.Create;
  RegisterExtension(Win32.TargetCPU+'-'+Win32.TargetOS,Win32);
finalization
  Win32.Destroy;
{$ENDIF}
end.

