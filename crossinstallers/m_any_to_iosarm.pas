unit m_any_to_iosarm;

{ Cross compiles to ios 32 bit arm
Copyright (C) 2020 Reinier Olislagers / DonAlfredo

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

{ Tany_iosarm }
Tany_iosarm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Tany_iosarm }

function Tany_iosarm.GetLibs(Basepath:string): boolean;
const
  LibName='libc.dylib';
var
  s:string;
  i,j,k:integer;
  found:boolean;
  SDKVersion,SDKMajor,SDKMinor:string;
begin
  result:=FLibsFound;

  if result then exit;

  found:=false;
  SDKMajor := '';
  SDKMinor := '';

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
    result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,'usr','lib']),LibName);

  if not result then
    result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,'usr','lib']),'libc.tbd');

  // also for cctools
  if not result then
  begin
    for i:=15 downto 8 do
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
          SDKVersion:=s
          ;
          s:=ConcatPaths([DirName,'iPhoneOS'+SDKVersion+'.sdk','usr','lib']);
          result:=SimpleSearchLibrary(BasePath,s,LibName);
          if not result then
             result:=SimpleSearchLibrary(BasePath,s,'libc.tbd');

          // universal libs : also search in all-ios
          if (not result) then
          begin
            s:=ConcatPaths(['all-ios','iPhoneOS'+SDKVersion+'.sdk','usr','lib']);
            result:=SimpleSearchLibrary(BasePath,s,LibName);
            if not result then
               result:=SimpleSearchLibrary(BasePath,s,'libc.tbd');
          end;

          if result then
          begin
            found:=true;
            SDKMajor := InttoStr(i);
            SDKMinor := InttoStr(j);
          end;
        end;
      end;
    end;
  end;

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
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
    AddFPCCFGSnippet('-Fl'+ConcatPaths([FLibsPath,'system'])+DirectorySeparator);

    s:=IncludeTrailingPathDelimiter(FLibsPath)+'..'+DirectorySeparator+'..'+DirectorySeparator;
    s:=ExpandFileName(s);
    s:=ExcludeTrailingBackslash(s);

    AddFPCCFGSnippet('-Fl'+ConcatPaths([s,'System','Library','Frameworks'])+DirectorySeparator);

    AddFPCCFGSnippet('-k-framework -kFoundation');
    AddFPCCFGSnippet('-k-framework -kCoreFoundation');
    AddFPCCFGSnippet('-XR'+s);

    //Add minimal iOS version
    {
    if found then
    begin
      s:='-WP'+SDKMajor+'.'+SDKMinor;
      AddFPCCFGSnippet(s);
      FCrossOpts.Add(s+' ');
    end;
    }
  end
  else
  begin
    ShowInfo('Hint: https://github.com/xybp888/iOS-SDKs');
    ShowInfo('Hint: https://github.com/theos/sdks');
  end;
end;

function Tany_iosarm.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  S,BinPrefixTry,PresetBinPath: string;
  aOption:string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for (cross)binutils from https://github.com/tpoechtrager/cctools
  BinPrefixTry:=TargetCPUName+'-apple-darwin';

  {
  10.4  = darwin8
  10.5  = darwin9
  10.6  = darwin10
  10.7  = darwin11
  10.8  = darwin12
  10.9  = darwin13
  10.10 = darwin14
  10.11 = darwin15
  10.12 = darwin16
  }

  for i:=MAXDARWINVERSION downto MINDARWINVERSION do
  begin
    if not result then
    begin
      AsFile:=BinPrefixTry+InttoStr(i)+'-'+'as'+GetExeExt;
      result:=SearchBinUtil(BasePath,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,'all-ios',AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath+DirectorySeparator+'bin','all-ios',AsFile);
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
    for i:=MAXDARWINVERSION downto MINDARWINVERSION do
    begin
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

constructor Tany_iosarm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  FTargetOS:=TOS.ios;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_iosarm.Destroy;
begin
  inherited Destroy;
end;

{$ifndef Darwin}
var
  any_iosarm:Tany_iosarm;

initialization
  any_iosarm:=Tany_iosarm.Create;
  RegisterCrossCompiler(any_iosarm.RegisterName,any_iosarm);

finalization
  any_iosarm.Destroy;
{$endif}

end.
