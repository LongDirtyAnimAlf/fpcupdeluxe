unit m_any_to_solarisx64;
{ Cross compiles from any platform (with supported crossbin utils0 to Solaris AMD 64 bit (x64)
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, fpcuputil;

type

{ TAny_Solarisx64 }
TAny_Solarisx64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TAny_Solarisx64 }

function TAny_Solarisx64.GetLibs(Basepath:string): boolean;
var
  aDirName:string;
begin
  result:=FLibsFound;
  if result then exit;

  if FSolarisOI then
  begin
    aDirName:=DirName+'-oi';
  end
  else
  begin
    aDirName:=DirName;
  end;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCNAME);

  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,aDirName,LIBCNAME);

  if result then
  begin
    FLibsFound:=true;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    AddFPCCFGSnippet('-Xr/usr/lib'); {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    SearchLibraryInfo(result);
  end;
end;

function TAny_Solarisx64.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry: string;
  aDirName:string;
begin
  result:=inherited;
  if result then exit;

  if FSolarisOI then
  begin
    aDirName:=DirName+'-oi';
  end
  else
  begin
    aDirName:=DirName;
  end;

  // Start with any names user may have given
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);

  // Also allow for crossbinutils without prefix
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    ShowInfo('Suggestion for cross binutils: please check http://wiki.lazarus.freepascal.org/Solaris_Port.',etInfo);
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)); {search this directory for compiler utilities}
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names}
  end;
end;

constructor TAny_Solarisx64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.x86_64;
  FTargetOS:=TOS.solaris;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_Solarisx64.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Solarisx64:TAny_Solarisx64;

initialization
  Any_Solarisx64:=TAny_Solarisx64.Create;
  RegisterCrossCompiler(Any_Solarisx64.RegisterName,Any_Solarisx64);

finalization
  Any_Solarisx64.Destroy;
end.

