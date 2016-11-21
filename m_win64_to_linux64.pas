unit m_win64_to_linux64;
{ Cross compiles from Windows 32 to Linux x86/32 bit
Copyright (C) 2013 Reinier Olislagers

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
Setup: currently aimed at using the crossfpc supplied binaries/libs
Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the directory layout provided by the crossfpc project there, so you get

...
c:\development\cross\bin\i386-linux\i386-linux-ar.exe
c:\development\cross\bin\i386-linux\i386-linux-as.exe
...
c:\development\cross\lib\x86_64-linux\libc.a
c:\development\cross\lib\x86_64-linux\libc.so
...
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ Twin64_linux64 }
Twin64_linux64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  function TargetSignature: string;
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Twin64_linux64 }
function Twin64_linux64.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function Twin64_linux64.GetLibs(Basepath:string): boolean;
const
  DirName='x86_64-linux';
  LibName='libc.so';
begin
  result:=FLibsFound;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName);

  if result then
  begin
    FLibsFound:=true;
    infoln('Twin64_linux64: found libspath '+FLibsPath,etInfo);
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    '-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    //'-FL/lib64/ld-linux-x86-64.so.2' {buildfaq 3.3.1: the name of the dynamic linker on the target};
  end;
end;

{$ifndef FPCONLY}
function Twin64_linux64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to ffpccfgsnippet
  // todo: perhaps move checkdevlibs routines here
  result:=true;
end;
{$endif}

function Twin64_linux64.GetBinUtils(Basepath:string): boolean;
const
  DirName='x86_64-linux';
var
  AsFile: string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as.exe';

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if not result then
  begin
    infoln(FCrossModuleName+ ': failed: searched binutil '+AsFile+' without results. ',etInfo);
    FAlreadyWarned:=true;
  end;
  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding+ {Prepend the binutils names}
    '-Tlinux'; {target operating system}
  end;
end;

constructor Twin64_linux64.Create;
begin
  inherited Create;
  FCrossModuleName:='win64_linux64';
  FBinUtilsPrefix:='x86_64-linux-'; //crossfpc nomenclature
  FBinUtilsPath:='';
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FTargetCPU:='x86_64';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('Twin64_linux64 crosscompiler loading',etDebug);
end;

destructor Twin64_linux64.Destroy;
begin
  inherited Destroy;
end;

var
  win64_linux64:Twin64_linux64;

{$IF (DEFINED (WIN32)) OR (DEFINED(WIN64))}
// Even though it's officially for win64, crossbin are  x86 binaries, so allow it.
initialization
  win64_linux64:=Twin64_linux64.Create;
  RegisterExtension(win64_linux64.TargetCPU+'-'+win64_linux64.TargetOS,win64_linux64);
finalization
  win64_linux64.Destroy;
{$ENDIF}
end.

