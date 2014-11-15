unit m_any_to_androidarm;
{ Cross compiles from any platform (with supported crossbin utils0 to Android/Linux ARM 32 bit (Little Endian)
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

{ Following Leledumbo's tutorial:
http://pascalgeek.blogspot.com/2013/10/android-programming-with-lazarus.html
... and this bug report
http://bugs.freepascal.org/view.php?id=25399
that suggests android cross compilers can be used for mipsel linux

}
{
Another build script from bug report:
http://bugs.freepascal.org/view.php?id=25574
make clean crossall crossinstall OS_TARGET=android CPU_TARGET=arm CROSSOPT="-CfVFPV3 -OoFASTMATH -CpARMV6" INSTALL_PREFIX=C:\Develop\fpc\fpctrunk PP=C:\Develop\fpc\fpctrunk\bin\i386-win32\fpc.exe BINDIR=C:\Android\android-ndk-r9\toolchains\arm-linux-androideabi-4.8\prebuilt\windows-x86_64\arm-linux-androideabi\bin CROSSBINDIR=C:\Android\android-ndk-r9\toolchains\arm-linux-androideabi-4.8\prebuilt\windows-x86_64\bin BINUTILSPREFIX=arm-linux-androideabi-
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil,fileutil;

implementation


type

{ TAny_ARMAndroid }
TAny_ARMAndroid = class(TCrossInstaller)
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

{ TAny_ARMAndroid }
function TAny_ARMAndroid.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TAny_ARMAndroid.GetLibs(Basepath:string): boolean;
{ Example files in lib directory - may or may not match your requirements:
crtbegin_dynamic.o
crtbegin_so.o
crtbegin_static.o
crtend_android.o
crtend_so.o
libandroid.so
libc.a
libc.so
libdl.so
libEGL.so
libGLESv1_CM.so
libGLESv2.so
libGLESv3.so
libjnigraphics.so
liblog.so
libm.a
libm.so
libm_hard.a
libOpenMAXAL.so
libOpenSLES.so
libstdc++.a
libstdc++.so
libthread_db.so
libz.so
rs
e.g. from an Android NDK:
android-ndk-r9c\platforms\android-19\arch-arm\usr\lib
}
const
  DirName='arm-android';
begin
  FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib'+DirectorySeparator+DirName);
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etInfo);
    FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..'+DirectorySeparator+
      'cross'+DirectorySeparator+
      'lib'+DirectorySeparator+
      DirName);
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etInfo);
  end;
  if result then
  begin
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Xd'+LineEnding+ {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    '-FLlibdl.so'; {buildfaq 3.3.1: the name of the dynamic linker on the target}
    {
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    '-XR'+IncludeTrailingPathDelimiter(FLibsPath);
    }
    //todo: possibly adapt for android:
    {'-Xr/usr/lib'+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    }
    infoln(FCrossModuleName + ': found libspath '+FLibsPath,etInfo);
  end
  else
  begin
    infoln(FCrossModuleName + ': could not find libspath. Please fill '+SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib'+DirectorySeparator+DirName)+
    ' with Android libs, e.g. from the Android NDK. See http://wiki.lazarus.freepascal.org/Android.'
    ,etError);
    FAlreadyWarned:=true;
  end;
end;

function TAny_ARMAndroid.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // Android does not support Gtk/Qt. To do: how to figure out how to do custom drawn?
  result:=false;
  { Any libs from Android SDK, e.g. r22 (no need for ADT), e.g.
  http://dl.google.com/android/android-sdk_r22.3-windows.zip
  ?

  todo: add ant tools etc?
  http://pascalgeek.blogspot.com/2013/10/android-programming-with-lazarus.html
  }
end;

function TAny_ARMAndroid.GetBinUtils(Basepath:string): boolean;
const
  DirName='arm-android';
var
  AsFile: string;
begin
  inherited;
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
  result:=false;

  if not result then { try $(fpcdir)/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName,
      AsFile);

  if not result then { try cross/bin/<dirprefix>/ }
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName,
      AsFile);

  {$IFDEF MSWINDOWS}
  // Try some NDK paths; note: androideabi-4.7 can be 4.4.3 or 4.6 or 4.8 as well
  //http://dl.google.com/android/ndk/android-ndk-r9c-windows-x86_64.zip
  //also windows may be windows-x86_64...
  if not result then
    result:=SearchBinUtil('C:\Program Files\Android SDK\android-ndk-r9c\toolchains\arm-linux-androideabi-4.8\prebuilt\windows\bin',AsFile);

  if not result then
    result:=SearchBinUtil('C:\Program Files\Android SDK\android-ndk-r8e\toolchains\arm-linux-androideabi-4.7\prebuilt\windows\bin',AsFile);

  if not result then
    result:=SearchBinUtil('C:\Program Files\Android SDK\android-ndk-r8d\toolchains\arm-linux-androideabi-4.7\prebuilt\windows\bin',AsFile);
  {$ENDIF}

  {$IFDEF UNIX}
  // Try some NDK paths first; note: androideabi-4.7 can be 4.4.3 or 4.6 as well
  // Also, the NDK can be installed basically anywhere... including in the user's dir
  if not result then
    result:=SearchBinUtil('/usr/local/android-ndk-r9c/toolchains/arm-linux-androideabi-4.4.7/prebuilt/linux-x86/bin',AsFile);

  if not result then
    result:=SearchBinUtil('/usr/local/android-ndk-r8d/toolchains/arm-linux-androideabi-4.4.7/prebuilt/linux-x86/bin',AsFile);

  if not result then
    result:=SearchBinUtil('/usr/local/android-ndk-r8e/toolchains/arm-linux-androideabi-4.4.7/prebuilt/linux-x86/bin',AsFile);


  // User may also have placed them into their regular search path:
  if not result then { try /usr/local/bin/<dirprefix>/ }
    result:=SearchBinUtil('/usr/local/bin/'+DirName,
      AsFile);

  if not result then { try /usr/local/bin/ }
    result:=SearchBinUtil('/usr/local/bin',
      AsFile);

  if not result then { try /usr/bin/ }
    result:=SearchBinUtil('/usr/bin',
      AsFile);

  if not result then { try /bin/ }
    result:=SearchBinUtil('/bin',
      AsFile);
  {$ENDIF}

  if result then
  begin
    // Set some defaults if user hasn't specified otherwise
    // Architecture: e.g. ARMv6, ARMv7,...
    if StringListStartsWith(FCrossOpts,'-Cp')=-1 then
    begin
      FCrossOpts.Add('-CpARMV6'); //apparently earlier instruction sets unsupported by Android
      infoln(FCrossModuleName+ ': did not find any -Cp architecture parameter; using -CpARMV6.',etInfo);
    end;

    // By default, use software FPU/softfloat for ARM.
    // Hardfloat: set CROSSOPT="-CfVFPV3"
    // Warn user to check things
    if StringListStartsWith(FCrossOpts,'-CaEABIHF')>-1 then
    begin
      // Source: http://forum.lazarus.freepascal.org/index.php/topic,23075.msg137838.html#msg137838
      // http://lists.freepascal.org/lists/fpc-devel/2013-May/032093.html
      // -dFPC_ARMHF is only used for cross compiler generation, not useful when compiling end user
      infoln(FCrossModuleName+ ': found -CaEABIHF cross compile option. Please make sure you specified -dFPC_ARMHF in your FPCOPT in order to build a hard-float cross-compiler.',etWarning);
    end;


    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
      '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+
      '-XP'+FBinUtilsPrefix; {Prepend the binutils names};
  end
  else
  begin
    infoln(FCrossModuleName + ': could not find bin path. Please fill '+IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName+
    ' with cross binutils for Android ARM, e.g. from the Android NDK.'+LineEnding+
    'See http://wiki.lazarus.freepascal.org/Android.'
    ,etError);
    FAlreadyWarned:=true;
  end;
end;

constructor TAny_ARMAndroid.Create;
begin
  inherited Create;
  FCrossModuleName:='Any_ARMAndroid';
  // Invoke like
  // fpc -Parm -Tandroid
  // Note: the compiler does NOT define LINUX!
  // It defines UNIX and ANDROID though.

  // This prefix is HARDCODED into the compiler so should match (or be empty, actually)
  FBinUtilsPrefix:='arm-linux-androideabi-';//standard eg in Android NDK 9
  FBinUtilsPath:='';
  FCompilerUsed:=ctInstalled; //Use current trunk compiler to build, not stable bootstrap
  FCrossModuleName:='TAny_ARMAndroid'; //used in messages to user
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='arm';
  FTargetOS:='android';
  FAlreadyWarned:=false;
  infoln('TAny_ARMAndroid crosscompiler loading',etDebug);
end;

destructor TAny_ARMAndroid.Destroy;
begin
  inherited Destroy;
end;

var
  Any_ARMAndroid:TAny_ARMAndroid;

initialization
  Any_ARMAndroid:=TAny_ARMAndroid.Create;
  RegisterExtension(Any_ARMAndroid.TargetCPU+'-'+Any_ARMAndroid.TargetOS,Any_ARMAndroid);
finalization
  Any_ARMAndroid.Destroy;
end.

