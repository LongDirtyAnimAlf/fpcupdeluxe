unit m_any_to_darwinaarch64;

{ Cross compiles to Darwin 64 bit arm
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

{ Tany_darwinaarch64 }
Tany_darwinaarch64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_darwinaarch64 }

function Tany_darwinaarch64.GetLibs(Basepath:string): boolean;
const
  OSNAME='MacOSX';
  LibName='libc.dylib';
var
  s:string;
  SDKVersion:string;
  i,j,k:integer;
  found:boolean;
begin
  result:=FLibsFound;

  if result then exit;

  found:=false;

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

  // also for cctools
  if not result then
  begin
    for i:=MAXOSXVERSION downto MINOSXVERSION do
    begin
      if found then break;
      for j:=16 downto -1 do
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
          SDKVersion:=s;

          s:=ConcatPaths([DirName,OSNAME+SDKVersion+'.sdk','usr','lib']);
          result:=SimpleSearchLibrary(BasePath,s,LibName);
          if not result then
             result:=SimpleSearchLibrary(BasePath,s,'libc.tbd');

          // universal libs : also search in x86-targetos
          if (not result) then
          begin
            s:=ConcatPaths(['x86-'+TargetOSName,OSNAME+SDKVersion+'.sdk','usr','lib']);
            result:=SimpleSearchLibrary(BasePath,s,LibName);
            if not result then
               result:=SimpleSearchLibrary(BasePath,s,'libc.tbd');
          end;

          // universal libs : also search in all-targetos
          if (not result) then
          begin
            s:=ConcatPaths(['all-'+TargetOSName,OSNAME+SDKVersion+'.sdk','usr','lib']);
            result:=SimpleSearchLibrary(BasePath,s,LibName);
            if not result then
               result:=SimpleSearchLibrary(BasePath,s,'libc.tbd');
          end;

          if result then found:=true;
        end;
      end;
    end;
  end;

  if not result then
  begin
    {$IFDEF UNIX}
    FLibsPath:='/usr/lib/x86_64-darwin-gnu'; //debian Jessie+ convention
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
  end
  else
  begin
    ShowInfo('Hint: https://github.com/phracker/MacOSX-SDKs');
    ShowInfo('Hint: https://github.com/alexey-lysiuk/macos-sdk');
    ShowInfo('Hint: https://github.com/sirgreyhat/MacOSX-SDKs/releases');
  end;
end;

function Tany_darwinaarch64.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  S,BinPrefixTry,PresetBinPath: string;
  i:integer;
begin
  result:=inherited;

  if result then exit;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for (cross)binutils from https://github.com/tpoechtrager/cctools
  // fpc version from https://github.com/LongDirtyAnimalf/cctools
  BinPrefixTry:=TargetCPUName+'-apple-darwin';

  // See https://en.wikipedia.org/wiki/Darwin_%28operating_system%29#Release_history
  // Shows relation between macOS and Darwin versions

  for i:=20 downto 11 do
  begin
    if not result then
    begin
      AsFile:=BinPrefixTry+InttoStr(i)+'-'+'as'+GetExeExt;
      result:=SearchBinUtil(BasePath,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,'all-'+TargetOSName,AsFile);
      if result then
      begin
        FBinUtilsPrefix:=BinPrefixTry+InttoStr(i)+'-';
        break;
      end;
    end;
  end;

  if (not result) then
  begin
    // do a brute force search of correct binutils
    PresetBinPath:=ConcatPaths([BasePath,CROSSPATH,'bin',TargetCPUName+'-'+TargetOSName]);
    if DirectoryExists(PresetBinPath) then
    begin
      for i:=20 downto 10 do
      begin
        if i=10 then
          AsFile:=BinPrefixTry+'-'+'as'+GetExeExt
        else
          AsFile:=BinPrefixTry+InttoStr(i)+'-'+'as'+GetExeExt;
        S:=FindFileInDir(AsFile,PresetBinPath);
        if (Length(S)>0) then
        begin
          PresetBinPath:=ExtractFilePath(S);
          result:=SearchBinUtil(PresetBinPath,AsFile);
          if result then break;
        end;
      end;
    end;
    PresetBinPath:=ConcatPaths([BasePath,CROSSPATH,'bin','all-'+TargetOSName]);
    if DirectoryExists(PresetBinPath) then
    begin
      for i:=20 downto 10 do
      begin
        if i=10 then
          AsFile:=BinPrefixTry+'-'+'as'+GetExeExt
        else
          AsFile:=BinPrefixTry+InttoStr(i)+'-'+'as'+GetExeExt;
        S:=FindFileInDir(AsFile,PresetBinPath);
        if (Length(S)>0) then
        begin
          PresetBinPath:=ExtractFilePath(S);
          result:=SearchBinUtil(PresetBinPath,AsFile);
          if result then break;
        end;
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

constructor Tany_darwinaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.darwin;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_darwinaarch64.Destroy;
begin
  inherited Destroy;
end;

{$ifndef Darwin}
var
  any_darwinaarch64:Tany_darwinaarch64;

initialization
  any_darwinaarch64:=Tany_darwinaarch64.Create;
  RegisterCrossCompiler(any_darwinaarch64.RegisterName,any_darwinaarch64);

finalization
  any_darwinaarch64.Destroy;
{$endif}

end.

