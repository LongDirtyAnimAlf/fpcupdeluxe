unit m_any_to_go32v2i386;
{ Cross compiles from Windows 32/Windows 64 to Go32V2 (DOS extender) on i386
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
Remember to distribute cwsdpmi.exe with your programs.
http://homer.rice.edu/~sandmann/cwsdpmi/index.html
download
http://homer.rice.edu/~sandmann/cwsdpmi/csdpmi7b.zip
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

type

{ TAny_go32v2i386 }
TAny_go32v2i386 = class(TCrossInstaller)
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

{ TAny_go32v2i386 }

function TAny_go32v2i386.GetLibs(Basepath:string): boolean;
const
  LibName='';
begin
  result:=inherited;
  if result then exit;

  // first search local paths based on libbraries provided for or adviced by fpc itself
  result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if result then
  begin
    FLibsFound:=true;
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    SearchLibraryInfo(result);
  end;
  if not result then
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.',etInfo);
    FLibsPath:='';
    result:=true;
  end;
end;

{$ifndef FPCONLY}
function TAny_go32v2i386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  ShowInfo('No support for LCL platform '+LCL_Platform,etInfo);
  result:=inherited;
end;
{$endif}

function TAny_go32v2i386.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
begin  
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if not result then
  begin
    FBinUtilsPrefix:=TargetCPUName+'-go32-';
    AsFile:=FBinUtilsPrefix+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath); {search this directory for compiler utilities}
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names}
  end
  else
  begin
    FAlreadyWarned:=true;
    FBinUtilsPrefix:=''; //use built in assembler, linker
    FBinUtilsPath:='';
    ShowInfo('Binutil path ignored; it is optional *IF* compiling with > FPC 2.7.1');
  end;
end;

constructor TAny_go32v2i386.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.go32v2;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_go32v2i386.Destroy;
begin
  inherited Destroy;
end;

// Even though it's officially for Win32, win64 can run x86 binaries without problem, so allow it.
var
  Any_go32v2i386:TAny_go32v2i386;

initialization
  Any_go32v2i386:=TAny_go32v2i386.Create;
  RegisterCrossCompiler(Any_go32v2i386.RegisterName,Any_go32v2i386);

finalization
  Any_go32v2i386.Destroy;

end.

