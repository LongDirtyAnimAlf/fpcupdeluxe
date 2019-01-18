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
  Classes, SysUtils, m_crossinstaller, fileutil, fpcuputil;

implementation
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
  procedure Reset;override;
end;

{ Tany_linuxarm }

function Tany_linuxarm.GetLibs(Basepath:string): boolean;
const
  DirName='arm-linux';
//var
//  requirehardfloat:boolean;
begin
  result:=FLibsFound;
  if result then exit;

  //requirehardfloat:=(StringListStartsWith(FCrossOpts,'-CaEABIHF')>-1);

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCNAME);

  // local paths based on libraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCNAME);
  // also check in the gnueabi directory
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName+'-gnueabi',LIBCNAME);
  // also check in the gnueabihf directory
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName+'-gnueabihf',LIBCNAME);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    //todo: implement -Xr for other platforms if this setup works

    AddFPCCFGSnippet('-Xd');
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
    AddFPCCFGSnippet('-Xr/usr/lib');

    {
    Actually leaving this out seems to work ok on the target system.
    if StringListStartsWith(FCrossOpts,'-FL')=-1 then
    begin
      infoln(FCrossModuleName+ ': you did not specify any -FL option in your crossopts. You MAY want to specify e.g. -FL/usr/lib/ld-linux.so.3',etInfo);
      Let's not get too zealous and leave choices up to the user. Perhaps the default is good, too.
      FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
        '-FL/usr/lib/ld-linux.so.3' //buildfaq 3.3.1: the name of the dynamic linker on the target
      maybe for older situation:
        '-FL/usr/lib/ld-linux.so.2'
    end;
    }

    { Note: bug 21554 and checked on raspberry pi wheezy: uses armhf /lib/arm-linux-gnueabihf/ld-linux.so.3}
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
const
  DirName='arm-linux';
var
  AsFile,aOption: string;
  BinPrefixTry:string;
  i:integer;
  hardfloat:boolean;
  requirehardfloat:boolean;
begin
  result:=inherited;
  if result then exit;

  hardfloat:=false;
  requirehardfloat:=(StringListStartsWith(FCrossOpts,'-CaEABIHF')>-1);

  if (NOT requirehardfloat) then
  begin
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // Also allow for crossfpc naming
  if (not result) AND (NOT requirehardfloat) then
  begin
    BinPrefixTry:='arm-linux-eabi-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;

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
    AsFile:=BinPrefixTry+'as'+GetExeExt;

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
    AsFile:=BinPrefixTry+'as'+GetExeExt;

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
    AsFile:=BinPrefixTry+'as'+GetExeExt;

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
    AsFile:=BinPrefixTry+'as'+GetExeExt;
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
    AsFile:=BinPrefixTry+'as'+GetExeExt;

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
    AsFile:=BinPrefixTry+'as'+GetExeExt;

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
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // Last resort: also allow for crossbinutils without prefix, but in correct directory
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
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
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix);

    // Set some defaults if user hasn't specified otherwise
    // Architecture: e.g. ARMv6, ARMv7,...
    i:=StringListStartsWith(FCrossOpts,'-Cp');
    if i=-1 then
    begin
      aOption:='-CpARMV6';  // Raspberry Pi 1 and up
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find any -Cp architecture parameter; using '+aOption+' (RPi default).');
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);

    // Warn user to check things
    if (StringListStartsWith(FCrossOpts,'-CaEABIHF')>-1) AND (NOT hardfloat) then
    begin
      // Source: http://forum.lazarus.freepascal.org/index.php/topic,23075.msg137838.html#msg137838
      // http://lists.freepascal.org/lists/fpc-devel/2013-May/032093.html
      // -dFPC_ARMHF is only used for (cross) compiler generation, not useful when compiling end user
      ShowInfo('Found -CaEABIHF cross compile option. Please make sure you specified -dFPC_ARMHF in your FPCOPT in order to build a hard-float cross-compiler.',etWarning);
    end;
  end;
end;

procedure Tany_linuxarm.Reset;
begin
  inherited Reset;
  FBinUtilsPrefix:='arm-linux-';
  FTargetCPU:='arm';
  FTargetOS:='linux';
  ShowInfo;
end;

var
  any_linuxarm:Tany_linuxarm;

initialization
  any_linuxarm:=Tany_linuxarm.Create;
  RegisterExtension(any_linuxarm.TargetCPU+'-'+any_linuxarm.TargetOS,any_linuxarm);
finalization
  any_linuxarm.Destroy;
end.

