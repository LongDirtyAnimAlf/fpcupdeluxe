unit m_any_to_embeddedarm;
{ Cross compiles from any platform with correct binutils to Embedded ARM
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller, fileutil, fpcuputil;

implementation

const
  ARCH='arm';
  OS='embedded';

type

{ TAny_Embeddedarm }
TAny_Embeddedarm = class(TCrossInstaller)
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

{ TAny_Embeddedarm }

function TAny_Embeddedarm.GetLibs(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
  LibName='libgcc.a';  // is this correct ??
begin
  // Arm-embedded does not need libs by default, but user can add them.

  result:=FLibsFound;
  if result then exit;

  if length(FSubArch)>0
     then ShowInfo('Cross-libs: We have a subarch: '+FSubArch)
     else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);
  // search local paths based on libraries provided for or adviced by fpc itself
  if not result then
     if length(FSubArch)>0 then result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+FSubArch,LibName);
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    SearchLibraryInfo(result);
  end;
  if not result then
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.');
    FLibsPath:='';
    result:=true;
  end;
end;

{$ifndef FPCONLY}
function TAny_Embeddedarm.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Todo: implement lcl libs path from basepath '+BasePath,etdebug);
  result:=inherited;
end;
{$endif}

function TAny_Embeddedarm.GetBinUtils(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
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

  // Now also allow for arm-none-eabi- binutilsprefix (e.g. launchpadlibrarian)
  if not result then
  begin
    BinPrefixTry:='arm-none-eabi-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
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
    ShowInfo('Suggestion for cross binutils: the crossfpc binutils (arm-embedded) at http://svn.freepascal.org/svn/fpcbuild/binaries/i386-win32/.');
    {$else}
    ShowInfo('Suggestion for cross binutils: the crossfpc binutils (arm-embedded) at https://launchpad.net/gcc-arm-embedded.');
    {$endif}
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;

    if length(FSubArch)>0 then
    begin
      ShowInfo('Cross-bins: We have a subarch: '+FSubArch);
      i:=StringListStartsWith(FCrossOpts,'-Cp');
      if i=-1 then
      begin
        aOption:='-Cp'+FSubArch;
        FCrossOpts.Add(aOption+' ');
        ShowInfo('Did not find any -Cp architecture parameter; using '+aOption);
      end else aOption:=Trim(FCrossOpts[i]);
      AddFPCCFGSnippet(aOption);
    end else ShowInfo('Cross-bins: No subarch defined. Expect fatal errors.',etError);

    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names};

    (*
    if length(FSubArch)=0 then
    begin
      aOption:='armv6m';
      ShowInfo('Did not find any subarch definition; using '+aOption+' (cortex-m0/embed default).');
      FSubArch:=aOption;
      aOption:='-Cp'+aOption;
      FCrossOpts.Add(aOption+' ');
      AddFPCCFGSnippet(aOption);
    end;
    *)

    (*
    // Set some defaults if user hasn't specified otherwise
    // Architecture: e.g. ARMv6, ARMv7,...
    i:=StringListStartsWith(FCrossOpts,'-Cp');
    if i=-1 then
    begin
      aOption:='-CpARMV7M';  // cortex-m3/embed default
      FCrossOpts.Add(aOption+' ');
      // When compiling for arm-embedded, a sub-architecture (e.g. SUBARCH=armv4t or SUBARCH=armv7m) must be defined)
      FSubArch:='armv7m';
      ShowInfo('Did not find any -Cp architecture parameter; using '+aOption+' (cortex-m3/embed default).');
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);
    *)
  end;
end;

constructor TAny_Embeddedarm.Create;
begin
  inherited Create;
  FTargetCPU:=ARCH;
  FTargetOS:=OS;
  FBinUtilsPrefix:=ARCH+'-'+OS+'-'; //crossfpc nomenclature; module will also search for android crossbinutils
  FBinUtilsPath:='';
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_Embeddedarm.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Embeddedarm:TAny_Embeddedarm;

initialization
  Any_Embeddedarm:=TAny_Embeddedarm.Create;
  RegisterExtension(Any_Embeddedarm.TargetCPU+'-'+Any_Embeddedarm.TargetOS,Any_Embeddedarm);
finalization
  Any_Embeddedarm.Destroy;
end.

