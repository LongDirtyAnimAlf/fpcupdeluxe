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
  Classes, SysUtils,
  StrUtils,
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}
  m_crossinstaller,fpcuputil,fileutil;

implementation

const
  NDKVERSIONBASENAME='android-ndk-r';
  NDKVERSIONNAMES:array[0..16] of string = ('7','7b','7c','8','8b','8c','8d','8e','9','9b','9c','9d','10','10b','10c','10d','10e');
  NDKTOOLCHAINVERSIONS:array[0..3] of string = ('arm-linux-androideabi-4.4.7','arm-linux-androideabi-4.6','arm-linux-androideabi-4.8','arm-linux-androideabi-4.9');
  NDKARCHDIRNAME='arch-arm';
  PLATFORMVERSIONBASENAME='android-';
  PLATFORMVERSIONSNUMBERS:array[0..13] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22); //23 does not yet work due to text allocations


type

{ TAny_ARMAndroid }
TAny_ARMAndroid = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  function TargetSignature: string;
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
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
const
  DirName='arm-android';
  // we presume, libc.so has to be present in a cross-library for arm
  LibName='libc.so';
  // we presume, libandroid.so has to be present in a cross-library for arm
  //LibName='libandroid.so';
var
  delphiversion,ndkversion,platform:byte;
  PresetLibPath:string;
  AsFile: string;
begin
  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // if binaries already found, search for library belonging to these binaries !!
  if (not result) AND (Length(FBinUtilsPath)>0) then
  begin
    ndkversion:=Pos(NDKVERSIONBASENAME,FBinUtilsPath);
    if ndkversion>0 then
    begin
      ndkversion:=PosEx(DirectorySeparator,FBinUtilsPath,ndkversion);
      if ndkversion>0 then
      begin
        PresetLibPath:=LeftStr(FBinUtilsPath,ndkversion);
        for platform:=High(PLATFORMVERSIONSNUMBERS) downto Low(PLATFORMVERSIONSNUMBERS) do
        begin
          FLibsPath := IncludeTrailingPathDelimiter(PresetLibPath)+'platforms'+DirectorySeparator+
                       PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+DirectorySeparator+NDKARCHDIRNAME+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=DirectoryExists(FLibsPath);
          if not result
             then infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etDebug)
             else break;
        end;
      end;
    end;
  end;

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName);

  // search for a library provide by a standard android libraries install

  //C:\Users\<username>\AppData\Local\Android\sdk

  if not result then
  begin
    for ndkversion:=High(NDKVERSIONNAMES) downto Low(NDKVERSIONNAMES) do
    begin
      if not result then
      begin
        for platform:=High(PLATFORMVERSIONSNUMBERS) downto Low(PLATFORMVERSIONSNUMBERS) do
        begin
          // check libs in userdir\
          FLibsPath := IncludeTrailingPathDelimiter(GetUserDir)+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+DirectorySeparator+'platforms'+DirectorySeparator+
                       PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+DirectorySeparator+NDKARCHDIRNAME+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=DirectoryExists(FLibsPath);
          if not result then
          begin
            infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etDebug)
          end else break;
          // check libs in userdir\Andoid
          FLibsPath := IncludeTrailingPathDelimiter(GetUserDir)+'Android'+DirectorySeparator+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+DirectorySeparator+'platforms'+DirectorySeparator+
                       PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+DirectorySeparator+NDKARCHDIRNAME+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=DirectoryExists(FLibsPath);
          if not result then
          begin
            infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etDebug)
          end else break;
          // check libs in userdir\AppData\Local\Andoid
          FLibsPath := IncludeTrailingPathDelimiter(GetUserDir)+'AppData\Local\Android'+DirectorySeparator+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+DirectorySeparator+'platforms'+DirectorySeparator+
                       PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+DirectorySeparator+NDKARCHDIRNAME+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=DirectoryExists(FLibsPath);
          if not result then
          begin
            infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etDebug)
          end else break;

        end;
      end else break;
    end;
  end;

  {$IFDEF MSWINDOWS}
  // find Delphi android libs
  if not result then
  begin
    infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etDebug);
    for delphiversion:=17 downto 12 do
    begin
      if not result then
      begin
        for ndkversion:=High(NDKVERSIONNAMES) downto Low(NDKVERSIONNAMES) do
        begin
          if not result then
          begin
            for platform:=High(PLATFORMVERSIONSNUMBERS) downto Low(PLATFORMVERSIONSNUMBERS) do
            begin
              FLibsPath:='C:\Users\Public\Documents\Embarcadero\Studio\'+InttoStr(delphiversion)+
              '.0\PlatformSDKs\'+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+'\platforms\'+PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+'\'+NDKARCHDIRNAME+'\usr\lib';
              result:=DirectoryExists(FLibsPath);
              if not result
                 then infoln(FCrossModuleName + ': failed: searched libspath '+FLibsPath,etDebug)
                 else break;
            end;
          end else break;
        end;
      end else break;
    end;
  end;
  {$ENDIF}

  SearchLibraryInfo(result);
  if result then
  begin
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Xd'+LineEnding+ {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //'-XR'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+
    '-FLlibdl.so'; {buildfaq 3.3.1: the name of the dynamic linker on the target}
    //'-FLlibandroid.so'; {buildfaq 3.3.1: the name of the dynamic linker on the target}

    {
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    '-XR'+IncludeTrailingPathDelimiter(FLibsPath);
    }
    //todo: possibly adapt for android:
    {'-Xr/usr/lib'+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    }
  end
  else
  begin
    infoln(FCrossModuleName + ': Please fill '+SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib'+DirectorySeparator+DirName)+
    ' with Android libs, e.g. from the Android NDK. See http://wiki.lazarus.freepascal.org/Android.'
    ,etError);
    FAlreadyWarned:=true;
  end;
end;

{$ifndef FPCONLY}
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
{$endif}

function TAny_ARMAndroid.GetBinUtils(Basepath:string): boolean;
const
  DirName='arm-android';
  {
  NDKVERSIONBASENAME='android-ndk-r';
  NDKVERSIONNAMES:array[0..16] of string = ('7','7b','7c','8','8b','8c','8d','8e','9','9b','9c','9d','10','10b','10c','10d','10e');
  NDKTOOLCHAINVERSIONS:array[0..2] of string = ('arm-linux-androideabi-4.6','arm-linux-androideabi-4.8','arm-linux-androideabi-4.9');
  NDKARCHDIRNAME='arch-arm';
  PLATFORMVERSIONBASENAME='android-';
  PLATFORMVERSIONSNUMBERS:array[0..13] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22); //23 does not yet work due to text allocations
  }

var
  AsFile: string;
  PresetBinPath:string;
  ndkversion,delphiversion,toolchain:byte;
begin
  inherited;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(Basepath,AsFile);

  // if libs already found, search for binutils belonging to this lib !!
  if (not result) AND (Length(FLibsPath)>0) then
  begin
    ndkversion:=Pos(NDKVERSIONBASENAME,FLibsPath);
    if ndkversion>0 then
    begin
      ndkversion:=PosEx(DirectorySeparator,FLibsPath,ndkversion);
      if ndkversion>0 then
      begin
        PresetBinPath:=LeftStr(FLibsPath,ndkversion);
        for toolchain:=High(NDKTOOLCHAINVERSIONS) downto Low(NDKTOOLCHAINVERSIONS) do
        begin
          PresetBinPath:=IncludeTrailingPathDelimiter(PresetBinPath)+'toolchains'+DirectorySeparator+NDKTOOLCHAINVERSIONS[toolchain]+DirectorySeparator+'prebuilt'+DirectorySeparator;
          PresetBinPath:=IncludeTrailingPathDelimiter(PresetBinPath)+
          {$IFDEF MSWINDOWS}
          {$IFDEF CPU64}
          'windows-x86_64'+
          {$ELSE}
          'windows'+
          {$ENDIF}
          {$ENDIF}
          {$IFDEF LINUX}
          {$IFDEF CPU64}
          'linux-x86_64'+
          {$ELSE}
          'linux-x86'+
          {$ENDIF}
          {$ENDIF}
          {$IFDEF DARWIN}
          {$IFDEF CPU64}
          'darwin-x86_64'+
          {$ELSE}
          'darwin-x86'+
          {$ENDIF}
          {$ENDIF}
          DirectorySeparator+'bin';
          result:=SearchBinUtil(PresetBinPath,AsFile);
          if result then break;
        end;
      end;
    end;
  end;

  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if not result then
  begin
    for ndkversion:=High(NDKVERSIONNAMES) downto Low(NDKVERSIONNAMES) do
    begin
      if not result then
      begin
        for toolchain:=High(NDKTOOLCHAINVERSIONS) downto Low(NDKTOOLCHAINVERSIONS) do
        begin
          PresetBinPath:=IncludeTrailingPathDelimiter(GetUserDir);
          {$IFDEF LINUX}
          if FpGetEUid=0 then PresetBinPath:='/usr/local/';
          {$ENDIF}
          PresetBinPath:=NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+DirectorySeparator+'toolchains'+DirectorySeparator+NDKTOOLCHAINVERSIONS[toolchain]+DirectorySeparator+'prebuilt'+DirectorySeparator;
          PresetBinPath:=IncludeTrailingPathDelimiter(PresetBinPath)+
          {$IFDEF MSWINDOWS}
          {$IFDEF CPU64}
          'windows-x86_64'+
          {$ELSE}
          'windows'+
          {$ENDIF}
          {$ENDIF}
          {$IFDEF LINUX}
          {$IFDEF CPU64}
          'linux-x86_64'+
          {$ELSE}
          'linux-x86'+
          {$ENDIF}
          {$ENDIF}
          {$IFDEF DARWIN}
          {$IFDEF CPU64}
          'darwin-x86_64'+
          {$ELSE}
          'darwin-x86'+
          {$ENDIF}
          {$ENDIF}
          DirectorySeparator+'bin';
          result:=SearchBinUtil(PresetBinPath,AsFile);
          if result then break;
        end;
      end else break;
    end;
  end;


  {$IFDEF MSWINDOWS}
  // Try some SDK/NDK paths; note: androideabi-4.7 can be 4.4.3 or 4.6 or 4.8 as well
  //http://dl.google.com/android/ndk/android-ndk-r9c-windows-x86_64.zip
  //also windows may be windows-x86_64...

  if not result then
  begin
    for ndkversion:=High(NDKVERSIONNAMES) downto Low(NDKVERSIONNAMES) do
    begin
      if not result then
      begin
        for toolchain:=High(NDKTOOLCHAINVERSIONS) downto Low(NDKTOOLCHAINVERSIONS) do
        begin
          if not result then
          begin
            {$IFDEF CPU64}
            result:=SearchBinUtil(IncludeTrailingPathDelimiter(GetEnvironmentVariable('ProgramFiles(x86)'))+
            'Android\'+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+'\toolchains\'+NDKTOOLCHAINVERSIONS[toolchain]+
            '\prebuilt\windows\bin',AsFile);
            if result then break else
            {$ENDIF}
            begin
              result:=SearchBinUtil(IncludeTrailingPathDelimiter(GetEnvironmentVariable('ProgramFiles'))+
              'Android\'+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+'\toolchains\'+NDKTOOLCHAINVERSIONS[toolchain]+
              '\prebuilt\windows\bin',AsFile);
              if result then break;
            end;
          end else break;
        end;
      end else break;
    end;
  end;

  // check Delphi auto installed android libraries
  if not result then
  begin
    for delphiversion:=17 downto 12 do
    begin
      if not result then
      begin
        for ndkversion:=High(NDKVERSIONNAMES) downto Low(NDKVERSIONNAMES) do
        begin
          if not result then
          begin
            for toolchain:=High(NDKTOOLCHAINVERSIONS) downto Low(NDKTOOLCHAINVERSIONS) do
            begin
              if not result then
              begin
                result:=SearchBinUtil(
                'C:\Users\Public\Documents\Embarcadero\Studio\'+InttoStr(delphiversion)+
                '.0\PlatformSDKs\'+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+
                '\toolchains\'+NDKTOOLCHAINVERSIONS[toolchain]+'\prebuilt\windows\bin',AsFile);
                if result then break;
              end else break;
            end;
          end else break;
        end;
      end else break;
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Also, the NDK can be installed basically anywhere...
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

  SearchBinUtilsInfo(result);
  if result then
  begin
    // Set some defaults if user hasn't specified otherwise
    // Architecture: e.g. ARMv6, ARMv7,...
    if StringListStartsWith(FCrossOpts,'-Cp')=-1 then
    begin
      AsFile:='-CpARMV7A -CfVFPV3 -OoFASTMATH';
      FCrossOpts.Add(AsFile); //apparently earlier instruction sets unsupported by Android
      infoln(FCrossModuleName+ ': did not find any -Cp architecture parameter; using '+AsFile+'.',etInfo);
      AsFile:=StringReplace(AsFile,' ',LineEnding,[rfReplaceAll]);
      FFPCCFGSnippet:=FFPCCFGSnippet+AsFile;
    end;

    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
      '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+
      '-XP'+FBinUtilsPrefix; {Prepend the binutils names};
  end
  else
  begin
    infoln(FCrossModuleName + ': Please fill '+IncludeTrailingPathDelimiter(BasePath)+'..'+DirectorySeparator+'cross'+DirectorySeparator+'bin'+DirectorySeparator+DirName+
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
  infoln(FCrossModuleName+': crosscompiler loading',etDebug);
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

