unit m_freebsd_to_linux64;
{ Cross compiles from FreeBSD x64 to Linux
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
Notes: first tried /linux/compat emulation but does not work due to missing binutils (at least)
Solution would be to get the relevant binutils/libs from a working Linux environment - which though? - in some directory below baspath

Tried ports/packages:
emulators/linux_base
something like /usr/ports/emulators/linux_dist* however that does not seem to work in PCBSD right now
note: gentoo will install in /usr/local/gentoo-stage3/ and not overwrite the base linux system compat, which ais as it should.

# For freebsd, these 2 ports may well be needed. They are installed by default on PC-BSD
cd /usr/ports/emulators/linux_base-f10
make -DBATCH install distclean
cd /usr/ports/x11/linux-f10-xorg-libs/
make -DBATCH install distclean
These did not work (marked broken) in PC BSD 9 release
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ TFreeBSD_Linux64 }

TFreeBSD_Linux64 = class(TCrossInstaller)
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

{ TFreeBSD_Linux64 }
function TFreeBSD_Linux64.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TFreeBSD_Linux64.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='/compat/linux/lib';
  result:=DirectoryExists(FLibsPath);
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
  end;
end;

{$ifndef FPCONLY}
function TFreeBSD_Linux64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;
{$endif}

function TFreeBSD_Linux64.GetBinUtils(Basepath:string): boolean;
const
  BinutilsDownloadURL='http://ftp.gnu.org/gnu/binutils/binutils-2.23.1.tar.gz';
  {or bz2:
  http://ftp.gnu.org/gnu/binutils/binutils-2.23.1.tar.bz2
  }
begin
  result:=inherited;
  if result then exit;

  //todo: remove once done
  infoln('TFreeBSD_Linux64: Experimental, not finished. Stopping now.', etError);
  result:=false;

  //todo: use conditional compilation for hostcpu, hostos; determine what to do depending on that
  FBinUtilsPrefix:='';
  FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'/cross/bin/'+TargetSignature; //these do not contain as etc though
  if not FileExists(FBinUtilsPath+'/as') then
  begin
    // Check for and get Linux binutils.
    if not(ForceDirectories(FBinUtilsPath)) then
    begin
      infoln('TFreeBSD_Linux64: Could not create binutils directory '+FBinUtilsPath,etError);
      FAlreadyWarned:=true;
      exit(false);
    end;
    // Get gnu binutils
    //todo: check for gunzip/tar executable=>installercore checkandget?
    Download(BinutilsDownloadURL,GetTempDir); //todo: proper temp directory
    //todo: extract tar.gz in place
    {make example for arm
    ./configure --target=arm-linux --disable-werror
    make
    }
    //todo: make install to fbinutilspath
  end;
  result:=FileExists(FBinUtilsPath+'/as'); // let the assembler be our coalmine canary
  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding+ {Prepend the binutils names}
    '-Tlinux'; {target operating system}
  end;
end;

constructor TFreeBSD_Linux64.Create;
begin
  inherited Create;
  FCrossModuleName:='FreeBSD_Linux64';
  FBinUtilsPath:='';
  FBinUtilsPrefix:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='x86_64';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('TFreeBSD_Linux64 crosscompiler loading',etDebug);
end;

destructor TFreeBSD_Linux64.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF FREEBSD}
var
  FreeBSD_Linux64:TFreeBSD_Linux64;

initialization
  FreeBSD_Linux64:=TFreeBSD_Linux64.Create;
  RegisterExtension(FreeBSD_Linux64.TargetCPU+'-'+FreeBSD_Linux64.TargetOS,FreeBSD_Linux64);
finalization
  FreeBSD_Linux64.Destroy;
{$ENDIF FREEBSD}
end.

