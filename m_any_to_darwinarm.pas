unit m_any_to_darwinarm;

{ Cross compiles to Darwin 32 bit
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
  Classes, SysUtils, m_crossinstaller, fileutil;

implementation

uses
  LazFileUtils;

const
  ARCH='arm';
  OS='darwin';

type

{ Tany_darwinarm }
Tany_darwinarm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_darwinarm }

function Tany_darwinarm.GetLibs(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
  LibName='libc.dylib';
var
  s:string;
  i,j,k:integer;
  found:boolean;
begin

  result:=FLibsFound;
  if result then exit;

  // begin simple: check presence of library file in basedir
  if not result then
    result:=SearchLibrary(Basepath,LibName);

  // for osxcross with special libs: search also for libc.tbd
  if not result then
    result:=SearchLibrary(Basepath,'libc.tbd');

  if not result then
    result:=SearchLibrary(IncludeTrailingPathDelimiter(Basepath)+'usr'+DirectorySeparator+'lib',LibName);

  // for osxcross with special libs: search also for libc.tbd
  if not result then
    result:=SearchLibrary(IncludeTrailingPathDelimiter(Basepath)+'usr'+DirectorySeparator+'lib','libc.tbd');

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,'libc.tbd');

  if not result then
    result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+'usr'+DirectorySeparator+'lib',LibName);

  if not result then
    result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+'usr'+DirectorySeparator+'lib','libc.tbd');

  {
  // also for cctools
  if not result then
  begin
    found:=false;
    for i:=10 downto 1 do
    begin
      if found then break;
      for j:=15 downto -1 do
      begin
        if found then break;
        for k:=15 downto -1 do
        begin
          if found then break;

          s:=InttoStr(i);
          if j<>-1 then
          begin
            s:=s+'.'+InttoStr(j);
            if k<>-1 then s:=s+'.'+InttoStr(k);
          end;
          s:='MacIOS'+s+'.sdk';

          s:=DirName+DirectorySeparator+s+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=SimpleSearchLibrary(BasePath,s,LibName);
          if not result then
             result:=SimpleSearchLibrary(BasePath,s,'libc.tbd');
          if result then found:=true;
        end;
      end;
    end;
  end;
  }

  if not result then
  begin
    {$IFDEF UNIX}
    FLibsPath:='/usr/lib/arm-darwin-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    ShowInfo('Searched but not found libspath '+FLibsPath);
    {$ENDIF}
  end;

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath);

    // specialities for osxcross
    //if Pos('osxcross',FLibsPath)>0 then
    begin
      s:=IncludeTrailingPathDelimiter(FLibsPath)+'..'+DirectorySeparator+'..'+DirectorySeparator;
      s:=ResolveDots(s);
      s:=ExcludeTrailingBackslash(s);
      FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
      '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+'system'+DirectorySeparator+LineEnding+
      '-k-framework'+LineEnding+
      '-kFoundation'+LineEnding+
      '-k-framework'+LineEnding+
      '-kCoreFoundation'+LineEnding+
      // -XRx is needed for fpc : prepend <x> to all linker search paths
      //'-XR'+ExcludeTrailingPathDelimiter(Basepath);
      '-XR'+s;
    end;

    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
  end;
end;

function Tany_darwinarm.GetBinUtils(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
var
  AsFile: string;
  BinPrefixTry: string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for (cross)binutils from https://github.com/tpoechtrager/cctools
  BinPrefixTry:='arm-apple-darwin';

  for i:=15 downto 10 do
  begin
    if not result then
    begin
      AsFile:=BinPrefixTry+InttoStr(i)+'-'+'as'+GetExeExt;
      result:=SearchBinUtil(BasePath,AsFile);
      if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
      if result then
      begin
        FBinUtilsPrefix:=BinPrefixTry+InttoStr(i)+'-';
        break;
      end;
    end;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    //'-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    '-XX'+LineEnding+
    '-XP'+FBinUtilsPrefix+LineEnding {Prepend the binutils names};
  end;
end;

constructor Tany_darwinarm.Create;
begin
  inherited Create;
  FTargetCPU:=ARCH;
  FTargetOS:=OS;
  FBinUtilsPrefix:=ARCH+'-'+OS+'-';
  FBinUtilsPath:='';
  //FBinutilsPathInPath:=true;
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_darwinarm.Destroy;
begin
  inherited Destroy;
end;

var
  any_darwinarm:Tany_darwinarm;

initialization
  any_darwinarm:=Tany_darwinarm.Create;
  RegisterExtension(any_darwinarm.TargetCPU+'-'+any_darwinarm.TargetOS,any_darwinarm);
finalization
  any_darwinarm.Destroy;

end.

