unit m_any_to_embeddedavr;
{ Cross compiles from any platform with correct binutils to Embedded AVR
Copyright (C) 2017 Alf

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
Setup: based on cross binaries from
http://svn.freepascal.org/svn/fpcbuild/binaries/i386-win32/
with binutils 2.22

Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the binaries in c:\development\cross\bin\avr-embedded
Binaries include
avr-embedded-ar.exe
avr-embedded-as.exe
avr-embedded-ld.exe
avr-embedded-objcopy.exe
avr-embedded-objdump.exe
avr-embedded-strip.exe
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ TAny_Embeddedavr }
TAny_Embeddedavr = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TAny_Embeddedavr }

function TAny_Embeddedavr.GetLibs(Basepath:string): boolean;
const
  LibName='libc.a';
  {$ifdef unix}
  UnixAVRLibDirs :array[0..3] of string = ('/usr/local/lib/avr/lib','/usr/lib/avr/lib','/usr/lib/avr','/lib/avr');
  {$endif}
var
  aSubarchName:string;
  {$ifdef unix}
  i:integer;
  {$endif}
begin
  // AVR-embedded does not need libs by default, but user can add them.
  result:=FLibsFound;

  if result then exit;

  if (FSubArch<>TSUBARCH.saNone) then
  begin
    aSubarchName:=GetEnumNameSimple(TypeInfo(TSUBARCH),Ord(FSubArch));
    ShowInfo('Cross-libs: We have a subarch: '+aSubarchName);
  end
  else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);
  // search local paths based on libraries provided for or adviced by fpc itself
  if not result then
     if (FSubArch<>TSUBARCH.saNone) then result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+aSubarchName,LibName);
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then
  begin
    if (FSubArch<>TSUBARCH.saNone) then
    begin
      for i:=Low(UnixAVRLibDirs) to High(UnixAVRLibDirs) do
      begin
        result:=SearchLibrary(IncludeTrailingPathDelimiter(UnixAVRLibDirs[i])+aSubarchName,LibName);
        if result then break;
      end;
    end;
  end;
  if not result then
  begin
    for i:=Low(UnixAVRLibDirs) to High(UnixAVRLibDirs) do
    begin
      result:=SearchLibrary(UnixAVRLibDirs[i],LibName);
      if result then break;
    end;
  end;
  {$endif unix}


  if result then
  begin
    FLibsFound:=True;

    if (FSubArch<>TSUBARCH.saNone) then
    begin
      if (Pos(aSubarchName,FLibsPath)>0) then
        // we have a libdir with a subarch inside: make it universal !!
        FLibsPath:=StringReplace(FLibsPath,aSubarchName,'$FPCSUBARCH',[]);
    end;

    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    SearchLibraryInfo(result);
  end;
  if not result then
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.');
    FLibsPath:='';
    FLibsFound:=True;
    result:=true;
  end;
end;

{$ifndef FPCONLY}
function TAny_Embeddedavr.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Todo: implement lcl libs path from basepath '+BasePath,etdebug);
  result:=inherited;
end;
{$endif}

function TAny_Embeddedavr.GetBinUtils(Basepath:string): boolean;
var
  AsFile,aOption: string;
  BinPrefixTry: string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  // Start with any names user may have given
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then
  begin
    for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
    begin
      result:=SearchBinUtil(IncludeTrailingPathDelimiter(UnixBinDirs[i])+DirName, AsFile);
      if not result then result:=SearchBinUtil(UnixBinDirs[i], AsFile);
      if result then break;
    end;
  end;
  {$endif unix}

  // Now also allow for avr- binutilsprefix
  if not result then
  begin
    BinPrefixTry:='avr-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    {$ifdef unix}
    // User may also have placed them into their regular search path:
    if not result then
    begin
      for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
      begin
        result:=SearchBinUtil(IncludeTrailingPathDelimiter(UnixBinDirs[i])+DirName, AsFile);
        if not result then result:=SearchBinUtil(UnixBinDirs[i], AsFile);
        if result then break;
      end;
    end;
    {$endif unix}
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for empty binutilsprefix in the right directory:
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    {$ifdef mswindows}
    ShowInfo(CrossWindowsSuggestion);
    {$endif}
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names};
  end;
end;

constructor TAny_Embeddedavr.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.avr;
  FTargetOS:=TOS.embedded;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_Embeddedavr.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Embeddedavr:TAny_Embeddedavr;

initialization
  Any_Embeddedavr:=TAny_Embeddedavr.Create;
  RegisterCrossCompiler(Any_Embeddedavr.RegisterName,Any_Embeddedavr);

finalization
  Any_Embeddedavr.Destroy;
end.

