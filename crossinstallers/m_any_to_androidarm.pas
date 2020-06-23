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
  Classes, SysUtils;

implementation

uses
  StrUtils,
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}
  FileUtil, m_crossinstaller, fpcuputil;

const
  ARCH='arm';
  OS='android';
  NDKVERSIONBASENAME=OS+'-ndk-r';
  NDKTOOLCHAINVERSIONS:array[0..3] of string = (ARCH+'-linux-'+OS+'eabi-4.4.7',ARCH+'-linux-'+OS+'eabi-4.6',ARCH+'-linux-'+OS+'eabi-4.8',ARCH+'-linux-'+OS+'eabi-4.9');
  NDKARCHDIRNAME='arch-'+ARCH;
  PLATFORMVERSIONBASENAME=OS+'-';


type

{ TAny_AndroidARM }
TAny_AndroidARM = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TAny_AndroidARM }

function TAny_AndroidARM.GetLibs(Basepath:string): boolean;
  // we presume, libc.so has to be present in a cross-library for arm
  // we presume, libandroid.so has to be present in a cross-library for arm
  //LibName='libandroid.so';
var
  delphiversion,ndkversion,platform:byte;
  PresetLibPath:string;
begin

  result:=FLibsFound;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCNAME);

  // local paths based on libraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCNAME);

  // if binaries already found, search for library belonging to these binaries !!
  if (not result) AND (Length(FBinUtilsPath)>0) AND (SearchModeUsed=smAuto) then
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
             then ShowInfo('Searched but not found libspath '+FLibsPath,etDebug)
             else break;
        end;
      end;
    end;
  end;

  // search for a library provide by a standard android libraries install
  if (not result) AND (SearchModeUsed=smAuto) then
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
            ShowInfo('Searched but not found libspath '+FLibsPath,etDebug)
          end else break;
          // check libs in userdir\Andoid
          FLibsPath := IncludeTrailingPathDelimiter(GetUserDir)+'Android'+DirectorySeparator+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+DirectorySeparator+'platforms'+DirectorySeparator+
                       PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+DirectorySeparator+NDKARCHDIRNAME+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=DirectoryExists(FLibsPath);
          if not result then
          begin
            ShowInfo('Searched but not found libspath '+FLibsPath,etDebug)
          end else break;
          // check libs in userdir\AppData\Local\Andoid
          FLibsPath := IncludeTrailingPathDelimiter(GetUserDir)+'AppData\Local\Android'+DirectorySeparator+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+DirectorySeparator+'platforms'+DirectorySeparator+
                       PLATFORMVERSIONBASENAME + InttoStr(PLATFORMVERSIONSNUMBERS[platform])+DirectorySeparator+NDKARCHDIRNAME+DirectorySeparator+'usr'+DirectorySeparator+'lib';
          result:=DirectoryExists(FLibsPath);
          if not result then
          begin
            ShowInfo('Searched but not found libspath '+FLibsPath,etDebug)
          end else break;

        end;
      end else break;
    end;
  end;

  {$IFDEF MSWINDOWS}
  // find Delphi android libs
  if (not result) AND (SearchModeUsed=smAuto) then
  begin
    ShowInfo('Searched but not found libspath '+FLibsPath,etDebug);
    for delphiversion:=MAXDELPHIVERSION downto MINDELPHIVERSION do
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
                 then ShowInfo('Searched but not found libspath '+FLibsPath,etDebug)
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
    FLibsFound:=true;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    AddFPCCFGSnippet('-FLlibdl.so'); {buildfaq 3.3.1: the name of the dynamic linker on the target}
  end
  else
  begin
    //Infoln(FCrossModuleName + ': Please fill '+SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib'+DirectorySeparator+DirName)+
    //' with Android libs, e.g. from the Android NDK. See http://wiki.lazarus.freepascal.org/Android.'
    //,etError);
    FAlreadyWarned:=true;
  end;
end;

function TAny_AndroidARM.GetBinUtils(Basepath:string): boolean;
var
  AsFile,aOption: string;
  PresetBinPath:string;
  ndkversion,toolchain:byte;
  i:integer;
  {$IFDEF MSWINDOWS}
  delphiversion:byte;
  {$ENDIF}
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:=TargetCPUName+'-linux-'+TargetOSName+'eabi-'; //standard eg in Android NDK 9

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(Basepath,AsFile);

  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // if libs already found, search for binutils belonging to this lib !!
  if (not result) AND (Length(FLibsPath)>0) AND (SearchModeUsed=smAuto) then
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

  if (not result) AND (SearchModeUsed=smAuto) then
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

  if (not result) AND (SearchModeUsed=smAuto) then
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
            UppercaseFirstChar(OS)+'\'+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+'\toolchains\'+NDKTOOLCHAINVERSIONS[toolchain]+
            '\prebuilt\windows\bin',AsFile);
            if result then break else
            {$ENDIF}
            begin
              result:=SearchBinUtil(IncludeTrailingPathDelimiter(GetEnvironmentVariable('ProgramFiles'))+
              UppercaseFirstChar(OS)+'\'+NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion]+'\toolchains\'+NDKTOOLCHAINVERSIONS[toolchain]+
              '\prebuilt\windows\bin',AsFile);
              if result then break;
            end;
          end else break;
        end;
      end else break;
    end;
  end;

  // check Delphi auto installed android libraries
  if (not result) AND (SearchModeUsed=smAuto) then
  begin
    for delphiversion:=MAXDELPHIVERSION downto MINDELPHIVERSION do
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

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;

    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names};

    // Set some defaults if user hasn't specified otherwise
    // Architecture: e.g. ARMv6, ARMv7,...
    i:=StringListStartsWith(FCrossOpts,'-Cp');
    if i=-1 then
    begin
      aOption:='-Cp'+DEFAULTARMCPU;
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find any [-Cp] architecture parameter; using '+aOption+'.');
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);

    {
    i:=StringListStartsWith(FCrossOpts,'-Cf');
    if i=-1 then
    begin
      aOption:='-CfVFPV3_D16';
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find any [-Cf] FPU instruction set setting; using '+aOption+'.');
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);
    }

  end
  else
  begin
    FAlreadyWarned:=true;
  end;
end;

constructor TAny_AndroidARM.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  FTargetOS:=TOS.android;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_AndroidARM.Destroy;
begin
  inherited Destroy;
end;

var
  Any_AndroidARM:TAny_AndroidARM;

initialization
  Any_AndroidARM:=TAny_AndroidARM.Create;
  RegisterCrossCompiler(Any_AndroidARM.RegisterName,Any_AndroidARM);

finalization
  Any_AndroidARM.Destroy;
end.

