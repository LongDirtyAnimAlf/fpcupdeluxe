unit m_any_to_linuxm68k;
{ Cross compiles from any platform with correct binutils to linux m68k
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
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ TAny_Linuxm68k }
TAny_Linuxm68k = class(TCrossInstaller)
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

{ TAny_Linuxm68k }

function TAny_Linuxm68k.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+LibsPath);
    AddFPCCFGSnippet('-Xr/usr/lib');
  end
  else
  begin
    //no libs yet: go on without them
    ShowInfo('Libspath ignored; it is optional for this cross compiler.',etInfo);
    FLibsPath:='';
    FLibsFound:=True;
    result:=True;
  end;
end;

{$ifndef FPCONLY}
function TAny_Linuxm68k.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Todo: implement lcl libs path from basepath '+BasePath,etdebug);
  result:=inherited;
end;
{$endif}

function TAny_Linuxm68k.GetBinUtils(Basepath:string): boolean;
var
  AsFile        : string;
  BinPrefixTry  : string;
  {$ifdef UNIX}
  i             : integer;
  {$endif UNIX}
begin
  result:=inherited;
  if result then exit;

  // Start with any names user may have given
  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Now also allow for avr- binutilsprefix
  if not result then
  begin
    BinPrefixTry:='m68k-elf-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    {$ifdef UNIX}
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
    {$endif UNIX}
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;


  SearchBinUtilsInfo(result);

  if not result then
  begin
    {$ifdef MSWINDOWS}
    ShowInfo(CrossWindowsSuggestion);
    {$endif MSWINDOWS}
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names};
  end;
end;

constructor TAny_Linuxm68k.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.m68k;
  FTargetOS:=TOS.linux;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_Linuxm68k.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Linuxm68k:TAny_Linuxm68k;

initialization
  Any_Linuxm68k:=TAny_Linuxm68k.Create;
  RegisterCrossCompiler(Any_Linuxm68k.RegisterName,Any_Linuxm68k);

finalization
  Any_Linuxm68k.Destroy;
end.

