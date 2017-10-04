unit m_any_to_msdosi8086;
{ Cross compiles to DOS on the Intel 8086 and higher processor
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
Setup: see help text in ShowInstallationInstructions below
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller, fileutil, fpcuputil;

implementation

const
  ARCH='i8086';
  OS='msdos';

type

{ TAny_msdosi8086 }
TAny_msdosi8086 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  procedure ShowInstallationInstructions;
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TAny_msdosi8086 }

procedure TAny_msdosi8086.ShowInstallationInstructions;
begin
  // this is old info ... do not show it anymore ... only use trunk
  ShowInfo('Binutils installation instructions:'+LineEnding+
    'For now, uses binutils from Marco v.d. Voort''s post at:' + LineEnding +
    'http://www.bttr-software.de/forum/forum_entry.php?id=12985' + LineEnding +
    '' + LineEnding +
    'Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)' + LineEnding +
    'Then place the binaries in c:\development\cross\bin\i8086-msdos' + LineEnding +
    'Binaries include' + LineEnding +
    'msdos-nasm.exe the NASM assembler' + LineEnding +
    'nasm.exe the NASM assembler' + LineEnding +
    'msdos-wlink.exe the OpenWatcom linker WLINK' + LineEnding +
    'wlinkd.dll' + LineEnding +
    'msdos-wlink.exe the OpenWatcom WLIB tool' + LineEnding +
    'wlibd.dll' + LineEnding +
    'wlsystem.lnk',etInfo);
  FAlreadyWarned:=true;
end;

function TAny_msdosi8086.GetLibs(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
  LibName='';
begin
  // DOS8086 does not need libs by default, but user can add them.

  result:=FLibsFound;
  if result then exit;

  // search local paths based on libraries provided for or adviced by fpc itself
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if result then
  begin
    FLibsFound:=true;
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    SearchLibraryInfo(result);
  end;
  if not result then
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.',etInfo);
    FLibsPath:='';
    result:=true;
  end;
end;

{$ifndef FPCONLY}
function TAny_msdosi8086.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  ShowInfo('No support for LCL platform '+LCL_Platform,etInfo);
  result:=inherited;
end;
{$endif}

function TAny_msdosi8086.GetBinUtils(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'nasm'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for outdated naming
  if (not result) then
  begin
    BinPrefixTry:=OS+'-';
    AsFile:=BinPrefixTry+'nasm'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;

    if StringListStartsWith(FCrossOpts,'-Wm')=-1 then
    begin
      // -WmSmall, -WmTiny, -WmMedium, -WmCompact, -WmLarge, -WmHuge
      ShowInfo('This compiler requires -Wm (memory model) !',etWarning);
      {$IFDEF DARWIN}
      ShowInfo('Added -WmLarge to CROSSOPT.',etWarning);
      FCrossOpts.Add('-WmLarge');
      {$ELSE}
      ShowInfo('Added -WmMedium to CROSSOPT.',etWarning);
      FCrossOpts.Add('-WmMedium');
      {$ENDIF DARWIN}
    end;

    // Always use smartlinking on i8086, because the system unit exceeds the 64kb code limit

    if StringListStartsWith(FCrossOpts,'-CX')=-1 then
    begin
      ShowInfo('This compiler requires -CX (create smartlinked libraries). Added it to CROSSOPT.',etWarning);
      FCrossOpts.Add('-CX');
    end;
    if (StringListStartsWith(FCrossOpts,'-XXs')=-1) OR (StringListStartsWith(FCrossOpts,'-XX')=-1) then
    begin
      ShowInfo('This compiler requires -XX (smartlinking). Added it (and stripping) to CROSSOPT.',etWarning);
      FCrossOpts.Add('-XX');
      FCrossOpts.Add('-Xs');
    end;

    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));{search this directory for compiler utilities}
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix);{Prepend the binutils names}
    // these are required ... see above.
    AddFPCCFGSnippet('-XX');{Smartlink}
    AddFPCCFGSnippet('-CX');{Smartlink libraries}
    AddFPCCFGSnippet('-Xs');{Strip}
  end
  else
  begin
    FAlreadyWarned:=true;
    //ShowInstallationInstructions;
  end;
end;

constructor TAny_msdosi8086.Create;
begin
  inherited Create;
  FTargetCPU:=ARCH;
  FTargetOS:=OS;
  FBinUtilsPrefix:='';
  FBinUtilsPath:='';
  {Add binutils directory to path when cross compiling.
  Works around faulty makefile in some versions of fpc that call nasm without
  specifying the directory it is in}
  FBinutilsPathInPath:=true;
  FCompilerUsed:=ctInstalled;
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_msdosi8086.Destroy;
begin
  inherited Destroy;
end;

var
  Any_msdosi8086:TAny_msdosi8086;

initialization
  Any_msdosi8086:=TAny_msdosi8086.Create;
  RegisterExtension(Any_msdosi8086.TargetCPU+'-'+Any_msdosi8086.TargetOS,Any_msdosi8086);
finalization
  Any_msdosi8086.Destroy;
end.

