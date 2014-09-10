unit m_any_to_linuxarm;
{ Cross compiles from any platform with correct binutils to Linux ARM
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
  Classes, SysUtils, m_crossinstaller,fpcuputil,fileutil;

implementation
type

{ TAny_Linuxarm }
TAny_Linuxarm = class(TCrossInstaller)
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

{ TAny_Linuxarm }
function TAny_Linuxarm.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TAny_Linuxarm.GetLibs(Basepath:string): boolean;
const
  DirName='arm-linux';
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
    infoln(FCrossModuleName+ ': found libspath '+FLibsPath,etInfo);
  end
  else
  begin
    infoln(FCrossModuleName+ ': you MAY want to copy your /lib, /usr/lib, /usr/lib/arm-linux-gnueabihf (Raspberry Pi Raspbian) from your device to your cross lib directory.',etInfo);
  end;
end;

function TAny_Linuxarm.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  infoln(FCrossModuleName+ ': implement lcl libs path from basepath '+BasePath+' for platform '+LCL_Platform,etdebug);
  result:=true;
end;

function TAny_Linuxarm.GetBinUtils(Basepath:string): boolean;
const
  DirName='arm-linux';
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
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  // Also allow for crossfpc naming
  if not result then
  begin
    FBinUtilsPrefix:='arm-linux-';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
  end;

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

  // Using crossfpc directory naming
  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  // Also allow for android crossbinutils
  if not result then
  begin
    FBinUtilsPrefix:='arm-linux-androideabi-';//standard eg in Android NDK 9
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
  end;

  // Using crossfpc directory naming
  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  if not result then
  begin
    infoln(FCrossModuleName+ ': failed: searched binutil '+AsFile+' without results. ',etInfo);
    {$ifdef mswindows}
    infoln(FCrossModuleName+ ': suggestion for cross binutils: the crossfpc binutils, mirrored at the fpcup download site.',etInfo);
    {$endif}
    FAlreadyWarned:=true;
  end;
  if result then
  begin
    { for raspberry pi look into
    instruction set
    -CpARMV6Z (not 7)
    ABI
    -CaEABI (versus DEFAULT) => not -caEABIHF/-dFPC_ARMHFhardfloat unless
    FPU coprocessor
    -CfVFPV2
    if using android cross compiler binutils: EABI0
    }
    { for FPC 2.7.1, you can use -OoFASTMATH to enable faster floating point calcs for all architectures }

    // Architecture: e.g. ARMv6, ARMv7,...
    if StringListStartsWith(FCrossOpts,'-Cp')=-1 then
    begin
      FCrossOpts.Add('-CpARMV6'); //apparently earlier instruction sets unsupported by Android
      infoln(FCrossModuleName+ ': did not find any -Cp architecture parameter; using -CpARMV6.',etInfo);
    end;

    // Warn user to check things
    if StringListStartsWith(FCrossOpts,'-CaEABIHF')>-1 then
    begin
      // Source: http://forum.lazarus.freepascal.org/index.php/topic,23075.msg137838.html#msg137838
      // http://lists.freepascal.org/lists/fpc-devel/2013-May/032093.html
      // -dFPC_ARMHF is only used for (cross) compiler generation, not useful when compiling end user
      infoln(FCrossModuleName+ ': found -CaEABIHF cross compile option. Please make sure you specified -dFPC_ARMHF in your FPCOPT in order to build a hard-float cross-compiler.',etWarning);
    end;

    // Configuration snippet for FPC
    //http://wiki.freepascal.org/Setup_Cross_Compile_For_ARM#Make_FPC_able_to_cross_compile_for_arm-linux
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix; {Prepend the binutils names}
    { don't know if this is still relevant for 2.7.x and for which linker
    '-darm'+LineEnding+ {pass arm to linker}
    }
  end;
end;

constructor TAny_Linuxarm.Create;
begin
  inherited Create;
  FCrossModuleName:='Any_Linuxarm';
  FBinUtilsPrefix:='arm-linux-'; //crossfpc nomenclature; module will also search for android crossbinutils
  FBinUtilsPath:='';
  FCompilerUsed:=ctBootstrap;
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FTargetCPU:='arm';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln(FCrossModuleName+ ': crosscompiler loading',etDebug);
end;

destructor TAny_Linuxarm.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Linuxarm:TAny_Linuxarm;

initialization
  Any_Linuxarm:=TAny_Linuxarm.Create;
  RegisterExtension(Any_Linuxarm.TargetCPU+'-'+Any_Linuxarm.TargetOS,Any_Linuxarm);
finalization
  Any_Linuxarm.Destroy;
end.

