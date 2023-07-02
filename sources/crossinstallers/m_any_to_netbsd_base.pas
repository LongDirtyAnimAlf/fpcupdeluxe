unit m_any_to_netbsd_base;

{
Cross compiles from any platform (with supported crossbin utils to NetBSD
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
  Classes, SysUtils, FileUtil, m_crossinstaller, fpcuputil;

type
  Tany_netbsd_base = class(TCrossInstaller)
  private
    FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
end;


implementation

function Tany_netbsd_base.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=true;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //AddFPCCFGSnippet('-XR'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    AddFPCCFGSnippet('-k"-rpath=/usr/X11R6/lib"',false);
    AddFPCCFGSnippet('-k"-rpath=/usr/X11R7/lib"',false);
    AddFPCCFGSnippet('-k"-rpath=/usr/pkg/lib"',false);
    //AddFPCCFGSnippet('-Xr/usr/lib'); {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
  end;
  {
  else
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.');
    FLibsFound:=true;
    FLibsPath:='';
    result:=true;
  end;
  }
end;

function Tany_netbsd_base.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  // Preset to default prefix name
  BinPrefixTry:=BinUtilsPrefix;

  // Start with any names user may have given
  AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if (not result) then
  begin
    if (TargetCPU=TCPU.i386) then
    begin
      BinPrefixTry:='i686-'+TargetOSName+'-';
      AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
      result:=SearchBinUtil(BasePath,AsFile);
      if (not result) then
        result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    end;
  end;

  // Also allow for crossbinutils without prefix
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  if result then FBinUtilsPrefix:=BinPrefixTry;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    ShowInfo('Suggestion for cross binutils: please check http://wiki.lazarus.freepascal.org/NetBSD.',etInfo);
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath); {search this directory for compiler utilities}
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names}
  end;
end;

constructor Tany_netbsd_base.Create;
begin
  inherited Create;
  FTargetOS:=TOS.netbsd;
  FAlreadyWarned:=false;
end;

destructor Tany_netbsd_base.Destroy;
begin
  inherited Destroy;
end;

end.

