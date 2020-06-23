unit m_any_to_darwin386;

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
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ Tany_darwin386 }
Tany_darwin386 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_darwin386 }

function Tany_darwin386.GetLibs(Basepath:string): boolean;
const
  LibName='libc.dylib';
var
  s:string;
  i,j:integer;
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

  // also for osxcross
  if not result then
  begin
    for i:=15 downto 3 do
    begin
      s:='MacOSX10.'+InttoStr(i);
      result:=SimpleSearchLibrary(BasePath,'x86-darwin'+DirectorySeparator+s+'.sdk'+DirectorySeparator+'usr'+DirectorySeparator+'lib',LibName);
      if not result then
         result:=SimpleSearchLibrary(BasePath,'x86-darwin'+DirectorySeparator+s+'.sdk'+DirectorySeparator+'usr'+DirectorySeparator+'lib','libc.tbd');

      if result then
      begin
        {$ifndef Darwin}
        j:=StringListStartsWith(FCrossOpts,'-WM');
        if j=-1 then
        begin
          // any SDK version setting >8 gives errors ... possible FPC bug ?
          j:=i;
          if j>8 then j:=8;
          s:='-WM'+'10.'+InttoStr(j);
          FCrossOpts.Add(s+' ');
          ShowInfo('Did not find any -WM; using '+s+'.',etInfo);
        end else s:=Trim(FCrossOpts[j]);
        AddFPCCFGSnippet(s);
        {$endif}
        break;
      end;

    end;
  end;

  if not result then
  begin
    {$IFDEF UNIX}
    FLibsPath:='/usr/lib/i386-darwin-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    ShowInfo('Searched but not found libspath '+FLibsPath);
    {$ENDIF}
  end;

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));

    s:=IncludeTrailingPathDelimiter(FLibsPath)+'..'+DirectorySeparator+'..'+DirectorySeparator;
    s:=ExpandFileName(s);
    s:=ExcludeTrailingBackslash(s);

    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+'system'+DirectorySeparator);
    AddFPCCFGSnippet('-k-framework -kAppKit');
    AddFPCCFGSnippet('-k-framework -kFoundation');
    AddFPCCFGSnippet('-k-framework -kCoreFoundation');
    AddFPCCFGSnippet('-XR'+s);
  end;
end;

function Tany_darwin386.GetBinUtils(Basepath:string): boolean;
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

  // Also allow for (cross)binutils from https://github.com/tpoechtrager/osxcross
  // fpc version from https://github.com/LongDirtyAnimalf/osxcross
  {$IFDEF MSWINDOWS}
  if IsWindows64
     then BinPrefixTry:='x86_64'
     else BinPrefixTry:='i386';
  {$else}
  BinPrefixTry:=GetTargetCPU;
  //BinPrefixTry:='i386';
  {$endif}
  BinPrefixTry:=BinPrefixTry+'-apple-darwin';

  for i:=15 downto 10 do
  begin
    if not result then
    begin
      AsFile:=BinPrefixTry+InttoStr(i)+'-'+'as'+GetExeExt;
      result:=SearchBinUtil(BasePath,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,'x86-darwin',AsFile);
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
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)); {search this directory for compiler utilities}
    AddFPCCFGSnippet('-XX');
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names};
  end;
end;

constructor Tany_darwin386.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.darwin;
  Reset;
  FBinutilsPathInPath:=true;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_darwin386.Destroy;
begin
  inherited Destroy;
end;

var
  any_darwin386:Tany_darwin386;

initialization
  any_darwin386:=Tany_darwin386.Create;
  RegisterCrossCompiler(any_darwin386.RegisterName,any_darwin386);

finalization
  any_darwin386.Destroy;

end.

