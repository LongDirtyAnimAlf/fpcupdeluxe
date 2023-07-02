unit m_any_to_ultiboarm;
{ Cross compiles from any platform with correct binutils to linux ARM
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
For BeagleBone Black, the crossfpc binaries work (see fpcup site for a mirror)

Also looks for android cross compiler bin and bin without any prefix

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ Tany_ultiboarm }
Tany_ultiboarm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
end;

{ Tany_ultiboarm }

function Tany_ultiboarm.GetLibs(Basepath:string): boolean;
const
  LibName='libc.a';
var
  aSubarchName:string;
begin
  result:=inherited;

  if result then exit;

  if (FSubArch<>TSUBARCH.saNone) then
  begin
    aSubarchName:=GetSubarch(FSubArch);
    ShowInfo('Cross-libs: We have a subarch: '+aSubarchName);
  end
  else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // local paths based on libraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LibName);
  // also check in the gnueabi directory
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName+'-gnueabi',LibName);
  // also check in the gnueabihf directory
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName+'-gnueabihf',LibName);

  // search local paths based on libraries provided for or adviced by fpc itself
  if not result then
     if (FSubArch<>TSUBARCH.saNone) then result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+aSubarchName,LibName);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd');
    AddFPCCFGSnippet('-Fl'+LibsPath);
    if DirectoryExists(IncludeTrailingPathDelimiter(FLibsPath)+'vc4') then
      AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(LibsPath)+'vc4');
  end;
end;

function Tany_ultiboarm.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry:string;
begin
  // binaries:
  // https://github.com/messense/homebrew-macos-cross-toolchains
  // https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/downloads

  result:=inherited;
  if result then exit;

  BinPrefixTry:=BinUtilsPrefix;

  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;
  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for crossfpc naming
  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-linux-eabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the eabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-eabi',AsFile);
  end;

  // Also allow for baremetal crossfpc naming
  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-none-eabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the eabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-eabi',AsFile);
  end;

  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-linux-gnueabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnueabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabi',AsFile);
  end;

  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-none-gnueabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnueabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabi',AsFile);
  end;

  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-unknown-linux-gnu-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-unknown-linux-gnueabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // Also allow for android crossbinutils
  if not result then
  begin
    BinPrefixTry:=TargetCPUName+'-linux-androideabi-';//standard eg in Android NDK 9
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // Last resort: also allow for crossbinutils without prefix, but in correct directory
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    // also check in the gnueabi directory
    if (not result) then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabi',AsFile);
  end;

  if result then FBinUtilsPrefix:=BinPrefixTry;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix);
  end;
end;

constructor Tany_ultiboarm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  FTargetOS:=TOS.ultibo;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

var
  any_ultiboarm:Tany_ultiboarm;

initialization
  any_ultiboarm:=Tany_ultiboarm.Create;
  RegisterCrossCompiler(any_ultiboarm.RegisterName,any_ultiboarm);

finalization
  any_ultiboarm.Destroy;
end.

