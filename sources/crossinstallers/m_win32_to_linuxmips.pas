unit m_win32_to_linuxmips;
{ Cross compiles from Windows 32 to mips 32 bit (Big Endian/mipseb)
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
based on cross binaries from
http://svn.freepascal.org/svn/fpcbuild/binaries/i386-win32/

Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the binaries in c:\development\cross\bin\mips-linux
Binaries include
mips-linux-ar.exe
mips-linux-as.exe
mips-linux-ld.exe
mips-linux-nm.exe
mips-linux-objcopy.exe
mips-linux-objdump.exe
mips-linux-strip.exe

Earlier tested with Sourcery CodeBench Lite GNU/Linux
http://www.mentor.com/embedded-software/sourcery-tools/sourcery-codebench/editions/lite-edition/mips-gnu-linux
e.g. mips-2013.05-36-mips-linux-gnu.exe
See page 15 of the getting started manual for the layout of lib and relation to architecture/gcc compiler options

- Adapt (add) for other setups
- Note that the libs may not match your actual system. If so, replace them
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation
type

{ Twin32_linuxmips }
Twin32_linuxmips = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Twin32_linuxmips }

function Twin32_linuxmips.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  if result then
  begin
    FLibsFound:=true;
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    AddFPCCFGSnippet('-Xr/usr/lib');
    ShowInfo('Found libspath '+FLibsPath,etInfo);
  end;
end;

{$ifndef FPCONLY}
function Twin32_linuxmips.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Twin32_linuxmips.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as.exe';

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath); {search this directory for compiler utilities}
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names}
  end;
end;

constructor Twin32_linuxmips.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.mips;
  FTargetOS:=TOS.linux;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Twin32_linuxmips.Destroy;
begin
  inherited Destroy;
end;

var
  Win32_linuxmips:Twin32_linuxmips;

initialization
  Win32_linuxmips:=Twin32_linuxmips.Create;
  RegisterCrossCompiler(Win32_linuxmips.RegisterName,Win32_linuxmips);

finalization
  Win32_linuxmips.Destroy;

end.

