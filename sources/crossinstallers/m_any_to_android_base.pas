unit m_any_to_android_base;
{ Cross compiles from any platform (with supported crossbin utils to Android
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
  Classes, SysUtils, m_crossinstaller;

type

  { Tany_android }
  Tany_android = class(TCrossInstaller)
  private
    FBuildArch:string;
  protected
    FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
    ARCH:string;
    ARCHSHORT:string;
    OS:string;
    NDKVERSIONBASENAME:string;
    NDKTOOLCHAINVERSIONS:array of string;
    NDKARCHDIRNAME:string;
    PLATFORMVERSIONBASENAME:string;
    property BuildArch:string read FBuildArch;
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}
  installerBase,
  FileUtil, fpcuputil;

const
  SEARCHFOR ='platforms'+DirectorySeparator+'android-';

// sort descending
function StringListSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1,s2:string;
  x:integer;
begin
  x:=Pos(SEARCHFOR,List[Index1]);
  if x>0 then
    s1:=Copy(List[Index1],x+Length(SEARCHFOR),2)
  else
    s1:=List[Index1];
  x:=Pos(SEARCHFOR,List[Index2]);
  if x>0 then
    s2:=Copy(List[Index2],x+Length(SEARCHFOR),2)
  else
    s2:=List[Index2];
  if List.CaseSensitive
    then Result := AnsiCompareStr(s2,s1)
    else Result := AnsiCompareText(s2,s1);
end;

{ Tany_android }

function Tany_android.GetLibs(Basepath:string): boolean;
  // we presume, libc.so has to be present in a cross-library for arm
  // we presume, libandroid.so has to be present in a cross-library for arm
  //LibName='libandroid.so';
var
  delphiversion,ndkversion,platform:byte;
  s:string;
  PresetLibPath,aOption:string;
  FilesFound,FilesFoundFiltered: TStringList;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // local paths based on libraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  // if binaries already found, search for library belonging to these binaries !!
  if (not result) AND (Length(FBinUtilsPath)>0) AND (Pos('Error:',FBinUtilsPath)=0) {AND (SearchModeUsed=TSearchSetting.ssAuto)} then
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
          FLibsPath := ConcatPaths([PresetLibPath,'platforms',PLATFORMVERSIONBASENAME+InttoStr(PLATFORMVERSIONSNUMBERS[platform]),NDKARCHDIRNAME,'usr','lib']);
          result:=DirectoryExists(FLibsPath);
          if (NOT result) then
          begin
            FLibsPath := ConcatPaths([PresetLibPath,'toolchains','llvm','prebuilt',BuildArch,'sysroot','usr','lib']);
            FLibsPath:=FLibsPath+DirectorySeparator;
            if TargetCPU=TCPU.i386 then
              FLibsPath:=FLibsPath+'i686-linux-android'
            else
              FLibsPath:=FLibsPath+ARCH+'-linux-android';
            if TargetCPU=TCPU.arm then FLibsPath:=FLibsPath+'eabi';
            FLibsPath:=FLibsPath+DirectorySeparator+InttoStr(PLATFORMVERSIONSNUMBERS[platform]);
          end;
          result:=DirectoryExists(FLibsPath);
          if not result
             then ShowInfo('Searched but not found libspath '+FLibsPath,etDebug)
             else break;
        end;
      end;
    end;
  end;

  // search for a library provide by a standard android libraries install
  if (not result) AND (SearchModeUsed=TSearchSetting.ssAuto) then
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
  if (not result) AND (SearchModeUsed=TSearchSetting.ssAuto) then
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

  if (NOT result) then
  begin
    //Perform a brute force search

    (*
    PresetLibPath:=IncludeTrailingPathDelimiter(GetUserDir);
    {$ifdef Darwin}
    PresetLibPath:=ConcatPaths([PresetLibPath,'Library','Android']);
    {$endif}
    {$ifdef Unix}
    {$ifndef Darwin}
    PresetLibPath:=ConcatPaths([PresetLibPath,'Android']);
    {$endif}
    {$endif}
    {$ifdef Windows}
    PresetLibPath:=ConcatPaths([PresetLibPath,'AppData','Local','Android']);
    {$endif}
    *)

    PresetLibPath:=GetAndroidSDKDir;

    aOption:=ConcatPaths([PresetLibPath,'ndk-bundle']);
    if (NOT DirectoryExists(aOption)) then
      aOption:=ConcatPaths([PresetLibPath,'ndk']);
    if (NOT DirectoryExists(aOption)) then
      aOption:=GetAndroidNDKDir;

    if DirectoryExists(aOption) then
      PresetLibPath:=aOption;

    if TargetCPUName='i386' then
      aOption:='i686-linux-'+OS
    else
      aOption:=TargetCPUName+'-linux-'+OS;

    FilesFound:=FindAllFiles(PresetLibPath,LIBCFILENAME);
    FilesFoundFiltered:=TStringList.Create;
    try
      for s in FilesFound do
      begin
        if ((Pos(NDKARCHDIRNAME,s)=0) AND (Pos(SEARCHFOR,s)=0) AND (Pos(aOption,s)=0)) then continue;
        FilesFoundFiltered.Append(s);
      end;
      FilesFoundFiltered.CustomSort(@StringListSortCompare);
      for s in FilesFoundFiltered do
      begin
        // Get the first ... we were sorting from highest version to lowest
        PresetLibPath:=ExtractFileDir(s);
        result:=SearchLibrary(PresetLibPath,LIBCFILENAME);
        break;
      end;
    finally
      FreeAndNil(FilesFoundFiltered);
      FreeAndNil(FilesFound);
    end;
  end;

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=true;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //if using the llvm sysroot (NDK version >= 22), also add the base directory for static libs
    s:=DirectorySeparator+ConcatPaths(['sysroot','usr','lib']);
    if ( (Pos('llvm',FLibsPath)>0) AND (Pos(s,FLibsPath)>0) ) then
    begin
      s:=IncludeTrailingPathDelimiter(FLibsPath)+'..'+DirectorySeparator;
      s:=ExpandFileName(s);
      if FileExists(s+'libc.a') then
        AddFPCCFGSnippet('-Fl'+ExcludeTrailingPathDelimiter(s));
    end;
    //AddFPCCFGSnippet('-FLlibdl.so',false); {buildfaq 3.3.1: the name of the dynamic linker on the target}
  end;
end;

function Tany_android.GetBinUtils(Basepath:string): boolean;
var
  BinPrefixTry: string;
  AsFiles:TStringList;
  ndkversion,toolchain:byte;
  s:string;
  AsFile,aOption: string;
  PresetBinPath:string;
  i:integer;
  {$IFDEF MSWINDOWS}
  delphiversion:byte;
  {$ENDIF}
begin
  result:=inherited;
  if result then exit;

  BinPrefixTry:=BinUtilsPrefix;

  AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(Basepath,AsFile);

  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if not result then
    result:=SimpleSearchBinUtil(BasePath,'all-'+TargetOSName,AsFile);

  if not result then
  begin
    BinPrefixTry:=TargetCPUName+'-linux-'+TargetOSName+'-';
    if (TargetCPU=TCPU.i386) then BinPrefixTry:='i686-linux-'+TargetOSName+'-';
    if (TargetCPU=TCPU.arm) then BinPrefixTry:=TargetCPUName+'-linux-'+TargetOSName+'eabi-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if (not result) then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // The newest Android tools are based on clang and llvm.
  // FPC is not yet prepared to use these.
  // So skip search for them

  (*

  // if libs already found, search for binutils belonging to this lib !!
  if (not result) AND (Length(FLibsPath)>0) AND (Pos('Error:',FLibsPath)=0){ AND (SearchModeUsed=TSearchSetting.ssAuto)} then
  begin
    ndkversion:=Pos(NDKVERSIONBASENAME,FLibsPath);
    if ndkversion>0 then
    begin
      ndkversion:=PosEx(DirectorySeparator,FLibsPath,ndkversion);
      if ndkversion>0 then
      begin
        s:=LeftStr(FLibsPath,ndkversion);
        for toolchain:=High(NDKTOOLCHAINVERSIONS) downto Low(NDKTOOLCHAINVERSIONS) do
        begin
          PresetBinPath := ConcatPaths([s,'toolchains',NDKTOOLCHAINVERSIONS[toolchain],'prebuilt',BuildArch,'bin']);
          result:=SearchBinUtil(PresetBinPath,AsFile);
          if result then
            break;
        end;
      end;
    end;
  end;

  if (not result) AND (SearchModeUsed=TSearchSetting.ssAuto) then
  begin
    s:=IncludeTrailingPathDelimiter(GetUserDir);
    {$IFDEF LINUX}
    if FpGetEUid=0 then s:='/usr/local/';
    {$ENDIF}
    for ndkversion:=High(NDKVERSIONNAMES) downto Low(NDKVERSIONNAMES) do
    begin
      if (not result) then
      begin
        for toolchain:=High(NDKTOOLCHAINVERSIONS) downto Low(NDKTOOLCHAINVERSIONS) do
        begin
          PresetBinPath := ConcatPaths([s,NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion],'toolchains',NDKTOOLCHAINVERSIONS[toolchain],'prebuilt',BuildArch,'bin']);
          result:=SearchBinUtil(PresetBinPath,AsFile);
          if result then
            break;
        end;
      end else break;
    end;
  end;


  {$IFDEF MSWINDOWS}
  // Try some SDK/NDK paths; note: androideabi-4.7 can be 4.4.3 or 4.6 or 4.8 as well
  //http://dl.google.com/android/ndk/android-ndk-r9c-windows-x86_64.zip
  //also windows may be windows-x86_64...

  if (not result) AND (SearchModeUsed=TSearchSetting.ssAuto) then
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
            s:=ConcatPaths([GetEnvironmentVariable('ProgramFiles(x86)'),UppercaseFirstChar(OS),NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion],'toolchains',NDKTOOLCHAINVERSIONS[toolchain],'prebuilt','windows','bin']);
            result:=SearchBinUtil(s,AsFile);
            if result then break else
            {$ENDIF}
            begin
              s:=ConcatPaths([GetEnvironmentVariable('ProgramFiles'),UppercaseFirstChar(OS),NDKVERSIONBASENAME+NDKVERSIONNAMES[ndkversion],'toolchains',NDKTOOLCHAINVERSIONS[toolchain],'prebuilt','windows','bin']);
              result:=SearchBinUtil(s,AsFile);
              if result then
                break;
            end;
          end else break;
        end;
      end else break;
    end;
  end;

  // check Delphi auto installed android tools
  if (not result) AND (SearchModeUsed=TSearchSetting.ssAuto) then
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

  if (NOT result) then
  begin
    //Perform a brute force search

    (*
    PresetBinPath:=IncludeTrailingPathDelimiter(GetUserDir);
    {$ifdef Darwin}
    PresetBinPath:=ConcatPaths([PresetBinPath,'Library','Android']);
    {$endif}
    {$ifdef Unix}
    {$ifndef Darwin}
    PresetBinPath:=ConcatPaths([PresetBinPath,'Android']);
    {$endif}
    {$endif}
    {$ifdef Windows}
    PresetBinPath:=ConcatPaths([PresetBinPath,'AppData','Local','Android']);
    {$endif}
    *)

    PresetBinPath:=GetAndroidSDKDir;

    aOption:=ConcatPaths([PresetBinPath,'ndk-bundle']);
    if (NOT DirectoryExists(aOption)) then
      aOption:=ConcatPaths([PresetBinPath,'ndk']);
    if (NOT DirectoryExists(aOption)) then
      aOption:=GetAndroidNDKDir;

    if DirectoryExists(aOption) then
      PresetBinPath:=aOption;

    AsFiles := FindAllFiles(PresetBinPath, AsFile, true);
    try
      if (AsFiles.Count>0) then
      begin
        for PresetBinPath in AsFiles do
        begin
          // This need a fix:
          // https://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=49498
          // So, only trunk or newer.
          if (CalculateNumericalVersion(FPCVersion)<CalculateFullVersion(3,3,1)) then
          begin
            if (Pos(DirectorySeparator+'llvm'+DirectorySeparator,PresetBinPath)=0) then break;
          end
          else
          begin
            if (Pos(DirectorySeparator+'llvm'+DirectorySeparator,PresetBinPath)>0) then break;
          end;
        end;
      end;
    finally
      AsFiles.Free;
    end;

    //PresetBinPath:=FindFileInDir(AsFile,PresetBinPath);
    if (Length(PresetBinPath)>0) then
    begin
      PresetBinPath:=ExtractFilePath(PresetBinPath);
      result:=SearchBinUtil(PresetBinPath,AsFile);
    end;
  end;

  *)

  if result then FBinUtilsPrefix:=BinPrefixTry;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names};
  end
  else
  begin
    FAlreadyWarned:=true;
  end;
end;

constructor Tany_android.Create;
{$IFDEF MSWINDOWS}
var
  WinPath:string;
{$ENDIF}
begin
  inherited Create;
  FTargetOS:=TOS.android;
  FAlreadyWarned:=false;

  {$IFDEF MSWINDOWS}
  if IsWindows64
     then WinPath:='windows-x86_64'
     else WinPath:='windows';
  {$ENDIF}

  FBuildArch:=
  {$IFDEF MSWINDOWS}
  WinPath+
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
  '';

  if (Length(FBuildArch)=0) then
  begin
    FBuildArch:=GetSourceOS+'-';
    if GetSourceCPU='i386' then
      FBuildArch:=FBuildArch+'x86'
    else
      FBuildArch:=FBuildArch+GetSourceCPU;
  end;
  OS:=TargetOSName;
  NDKVERSIONBASENAME:=OS+'-ndk-r';
  PLATFORMVERSIONBASENAME:=OS+'-';
end;

destructor Tany_android.Destroy;
begin
  SetLength(NDKVERSIONBASENAME,0);
  inherited Destroy;
end;

end.

