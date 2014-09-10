unit m_any_to_aixpowerpc;
{ Cross compiles from any platform with correct binutils to AIX on 32 bit powerpc
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
See
http://wiki.lazarus.freepascal.org/FPC_AIX_Port
- Get Windows binutils from ftp://ftp.freepascal.org/pub/fpc/contrib/aix/fpc-2.7.1.powerpc-aix-win32.zip
powerpc-aix-ar.exe
powerpc-aix-as.exe
powerpc-aix-ld.exe
powerpc-aix-nm.exe
powerpc-aix-strip.exe

- *nix: compile cross binutils yourself; see wiki page above
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil,fileutil;

implementation
type

{ TAny_AIXPowerPC }
TAny_AIXPowerPC = class(TCrossInstaller)
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

{ TAny_AIXPowerPC }
function TAny_AIXPowerPC.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TAny_AIXPowerPC.GetLibs(Basepath:string): boolean;
const
  DirName='powerpc-aix';
begin

  // Using crossfpc directory naming
  FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib\'+DirName);
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln(FCrossModuleName+ ': failed: searched libspath '+FLibsPath,etInfo);
    FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\lib\'+DirName);
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln(FCrossModuleName+ ': failed: searched libspath '+FLibsPath,etInfo);
  end;
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    //todo: implement -Xr for other platforms if this setup works
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
      '-Xd'+LineEnding+ {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
      '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
      '-Xr/usr/lib'; {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    infoln(FCrossModuleName+ ': found libspath '+FLibsPath,etInfo);
  end
  else
  begin
    infoln(FCrossModuleName+ ': no libspath found. For simple programs that do not call (C) libraries, this is not necessary. However, you MAY want to copy your /usr/lib from your AIX machine to your cross lib directory.',etInfo);
  end;
  result:=true; //this step is optional at least for simple hello world programs
end;

function TAny_AIXPowerPC.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  infoln(FCrossModuleName+ ': implement lcl libs path from basepath '+BasePath+' for platform '+LCL_Platform,etdebug);
  result:=true;
end;

function TAny_AIXPowerPC.GetBinUtils(Basepath:string): boolean;
const
  DirName='powerpc-aix';
var
  AsFile: string;
begin
  inherited;
  // Start with any names user may have given
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
  result:=false;

  // Using crossfpc directory naming
  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil((IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName),
      AsFile);

  // Also allow for crossfpc naming
  if not result then
  begin
    FBinUtilsPrefix:='powerpc-aix-';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
  end;

  if not result then
    result:=SearchBinUtil(FBinUtilsPath,AsFile);

  // Using crossfpc directory naming
  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  // Also allow for crossbinutils without prefix
  if not result then
  begin
    FBinUtilsPrefix:='';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
  end;
  if not result then
    result:=SearchBinUtil(FBinUtilsPath,AsFile);

  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  if not result then
  begin
    infoln(FCrossModuleName+ ': failed: searched binutil '+AsFile+' without results. ',etInfo);
    infoln(FCrossModuleName+ ': suggestion for cross binutils: please check http://wiki.lazarus.freepascal.org/FPC_AIX_Port.',etInfo);
    FAlreadyWarned:=true;
  end;
  if result then
  begin
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix; {Prepend the binutils names}
  end;
end;

constructor TAny_AIXPowerPC.Create;
begin
  inherited Create;
  FCrossModuleName:='Any_AIXPowerPC';
  FBinUtilsPrefix:='powerpc-aix-'; //crossfpc nomenclature; module will also search for no prefix crossbinutils
  FBinUtilsPath:='';
  FCompilerUsed:=ctBootstrap;
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FTargetCPU:='powerpc'; //32 bit powerpc; will run on 64 bit powerpc as well
  FTargetOS:='aix';
  FAlreadyWarned:=false;
  infoln(FCrossModuleName+ ': crosscompiler loading',etDebug);
end;

destructor TAny_AIXPowerPC.Destroy;
begin
  inherited Destroy;
end;

var
  Any_AIXPowerPC:TAny_AIXPowerPC;

initialization
  Any_AIXPowerPC:=TAny_AIXPowerPC.Create;
  RegisterExtension(Any_AIXPowerPC.TargetCPU+'-'+Any_AIXPowerPC.TargetOS,Any_AIXPowerPC);
finalization
  Any_AIXPowerPC.Destroy;
end.

