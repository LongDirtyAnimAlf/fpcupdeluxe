unit m_linuxx64_to_linux386;
{ Cross compiles from Linux 64 bit to Linux 32 bit
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
Written with gnu binutils in mind: getting binutils:
dpkg --add-architecture i386
apt-get install binutils:i386

Adapt (add) for other setups
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
const
  CrossModuleName='Tlinuxx64_linux386';

type

{ Tlinuxx64_linux386 }
Tlinuxx64_linux386 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  function TargetSignature: string;
public
  function GetLibs(Basepath:string):boolean;override;
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tlinuxx64_linux386 }
function Tlinuxx64_linux386.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function Tlinuxx64_linux386.GetLibs(Basepath:string): boolean;
const
  DirName='i386-linux';
begin
//todo add support for separate cross dire  
  FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib'+DirectorySeparator+DirName);
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln('Tlinuxx64_linux386: failed: searched libspath '+FLibsPath,etInfo);
    FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..'+DirectorySeparator+
      'cross'+DirectorySeparator+
      'lib'+DirectorySeparator+
      DirName);
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln('Tlinuxx64_linux386: failed: searched libspath '+FLibsPath,etInfo);
  end;
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    '-Xr/usr/lib'+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    '-FL/usr/lib/ld-linux.so.2' {buildfaq 3.3.1: the name of the dynamic linker on the target};
    infoln('Tlinuxx64_linux386: found libspath '+FLibsPath,etInfo);
  end;
end;

function Tlinuxx64_linux386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;

function Tlinuxx64_linux386.GetBinUtils(Basepath:string): boolean;
const
  DirName='i386-linux';
var
  AsFile: string;
begin
  inherited;
  AsFile:=FBinUtilsPrefix+'as';
  result:=false;

  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  {$IFDEF UNIX}
  if not result then { try /usr/local/bin/<dirprefix>/ }
    result:=SearchBinUtil('/usr/local/bin/'+DirName,
      AsFile);

  if not result then { try /usr/local/bin/ }
    result:=SearchBinUtil('/usr/local/bin',
      AsFile);

  if not result then { try /bin/ }
    result:=SearchBinUtil('/bin',
      AsFile);
  {$ENDIF}

  if result then
  begin
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding {Prepend the binutils names};
  end;
end;

constructor Tlinuxx64_linux386.Create;
begin
  inherited Create;
  FCrossModuleName:='linuxx64_linux386';
  FBinUtilsPrefix:=''; //try with none
  FBinUtilsPath:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='i386';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('Tlinuxx64_linux386 crosscompiler loading',etDebug);
end;

destructor Tlinuxx64_linux386.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF LINUX)}
var
  linuxx64_linux386:Tlinuxx64_linux386;

// Even though it's officially for x64, other architectures may work
initialization
  linuxx64_linux386:=Tlinuxx64_linux386.Create;
  RegisterExtension(linuxx64_linux386.TargetCPU+'-'+linuxx64_linux386.TargetOS,linuxx64_linux386);
finalization
  linuxx64_linux386.Destroy;
{$ENDIF}
end.

