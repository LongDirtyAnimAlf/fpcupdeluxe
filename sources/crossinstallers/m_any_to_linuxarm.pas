unit m_any_to_linuxarm;
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

{ Tany_linuxarm }
Tany_linuxarm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
end;

{ Tany_linuxarm }

function Tany_linuxarm.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // local paths based on libraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);
  // also check in the gnueabi directory
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName+'-gnueabi',LIBCFILENAME);
  // also check in the gnueabihf directory
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName+'-gnueabihf',LIBCFILENAME);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd');
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    AddFPCCFGSnippet('-Xr/usr/lib');
  end
  else
  begin
    ShowInfo('You MAY want to copy your /lib, /usr/lib, /usr/lib/arm-linux-gnueabihf (Raspberry Pi Raspbian) from your device to your cross lib directory.');
  end;
end;

{$ifndef FPCONLY}
function Tany_linuxarm.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Implement lcl libs path from basepath '+BasePath+' for platform '+LCL_Platform,etDebug);
  result:=inherited;
end;
{$endif}

function Tany_linuxarm.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry:string;
  hardfloat:boolean;
  requirehardfloat:boolean;
  {$ifdef Darwin}
  i:integer;
  {$endif}
begin
  result:=inherited;
  if result then exit;

  hardfloat:=false;
  requirehardfloat:=(StringListStartsWith(FCrossOpts,'-CaEABIHF')>-1);

  if (NOT requirehardfloat) then
  begin
    AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // Also allow for crossfpc naming
  if (not result) AND (NOT requirehardfloat) then
  begin
    BinPrefixTry:='arm-linux-eabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the eabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-eabi',AsFile);
  end;

  // Also allow for baremetal crossfpc naming
  if (not result) AND (NOT requirehardfloat) then
  begin
    BinPrefixTry:='arm-none-eabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the eabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-eabi',AsFile);
  end;

  if (not result) AND (NOT requirehardfloat) then
  begin
    BinPrefixTry:='arm-linux-gnueabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnueabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabi',AsFile);
  end;

  if (not result) AND (NOT requirehardfloat) then
  begin
    BinPrefixTry:='arm-none-gnueabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnueabi directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabi',AsFile);
  end;


  {$ifdef Darwin}
  if not result then
  begin
    // some special binutils, also working for RPi2 !!
    BinPrefixTry:='armv8-rpi3-linux-gnueabihf-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then
    begin
      hardfloat:=true;
      // remove floating point option, if any, as this toolchain does not like them
      // tricky !
      i:=StringListStartsWith(FCrossOpts,'-CfVFPV');
      if i>-1 then
      begin
        FCrossOpts.Delete(i);
      end;
      i:=StringListStartsWith(FCrossOpts,'-OoFASTMATH');
      if i>-1 then
      begin
        FCrossOpts.Delete(i);
      end;
      i:=StringListStartsWith(FCrossOpts,'-CaEABIHF');
      if i>-1 then
      begin
        FCrossOpts.Delete(i);
      end;
    end;
  end;
  {$endif}

  // Also allow for hardfloat crossbinutils
  if not result then
  begin
    BinPrefixTry:='arm-linux-gnueabihf-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnueabihf directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabihf',AsFile);
    if result then hardfloat:=true;
  end;

  // baremetal
  if not result then
  begin
    BinPrefixTry:='arm-none-gnueabihf-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnueabihf directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabihf',AsFile);
    if result then hardfloat:=true;
  end;

  // Also allow for android crossbinutils
  if not result then
  begin
    BinPrefixTry:='arm-linux-androideabi-';//standard eg in Android NDK 9
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
    if (not result) AND (NOT requirehardfloat) then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabi',AsFile);
    // also check in the gnueabihf directory
    if not result then
    begin
      result:=SimpleSearchBinUtil(BasePath,DirName+'-gnueabihf',AsFile);
      if result then hardfloat:=true;
    end;
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
    //if hardfloat then ShowInfo('Found hardfloat binary utilities. Please make sure you did NOT specified -dFPC_ARMEL in your FPCOPT !',etWarning);

    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix);
  end;
end;

constructor Tany_linuxarm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  FTargetOS:=TOS.linux;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

var
  any_linuxarm:Tany_linuxarm;

initialization
  any_linuxarm:=Tany_linuxarm.Create;
  RegisterCrossCompiler(any_linuxarm.RegisterName,any_linuxarm);

finalization
  any_linuxarm.Destroy;
end.

