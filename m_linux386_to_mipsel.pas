unit m_linux386_to_mipsel;
{ Cross compiles from Linux 32 to mipsel 32 bit (Little Endian)
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
Written with gnu binutils in mind: getting binutils:
#following
#http://wiki.freepascal.org/Native_MIPS_Systems#Mainline_MIPS_Port
#on Debian linux x86, as regular user
cd ~
# Replace with your desired version:
wget http://ftp.gnu.org/gnu/binutils/binutils-2.23.1.tar.bz2
tar xjf binutils-2.23.1.tar.bz2
cd ~/binutils-2.23.1
make distclean
./config.sub mipsel-linux-gnu
./configure --prefix=/usr/local/mipsel-linux mipsel-linux-gnu
make
# make gives error compiling in bfd for binutils-2.20.1/2.20.1a
sudo make install
sudo ln -s /usr/local/mipsel-linux/bin/as /usr/local/bin/mipsel-linux-as
sudo ln -s /usr/local/mipsel-linux/bin/ld /usr/local/bin/mipsel-linux-ld
sudo ln -s /usr/local/mipsel-linux/bin/ar /usr/local/bin/mipsel-linux-ar
sudo ln -s /usr/local/mipsel-linux/bin/objdump /usr/local/bin/mipsel-linux-objdump
sudo ln -s /usr/local/mipsel-linux/bin/objcopy /usr/local/bin/mipsel-linux-objcopy
sudo ln -s /usr/local/mipsel-linux/bin/strip /usr/local/bin/mipsel-linux-strip
mipsel-linux-ld -V

# copy over for self-contained fpcup setup:
mkdir -p ~/development/cross/bin/mipsel-linux
cp /usr/local/bin/mipsel-linux-as ~/development/cross/bin/mipsel-linux
cp /usr/local/bin/mipsel-linux-ld ~/development/cross/bin/mipsel-linux
cp /usr/local/bin/mipsel-linux-ar ~/development/cross/bin/mipsel-linux
cp /usr/local/bin/mipsel-linux-objdump ~/development/cross/bin/mipsel-linux
cp /usr/local/bin/mipsel-linux-objcopy ~/development/cross/bin/mipsel-linux
cp /usr/local/bin/mipsel-linux-strip ~/development/cross/bin/mipsel-linux

Download your libraries into ~/development/cross/lib/mipsel-linux

Adapt (add) for other setups
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation


type

{ TLinux386_mipsel }
TLinux386_mipsel = class(TCrossInstaller)
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

{ TLinux386_mipsel }
function TLinux386_mipsel.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TLinux386_mipsel.GetLibs(Basepath:string): boolean;
const
  DirName='mipsel-linux';
  LibName='libc.so';
begin

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName);

  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    '-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    //'-FL/usr/lib/ld-linux.so.2' {buildfaq 3.3.1: the name of the dynamic linker on the target};
    infoln(FCrossModuleName + ': found libspath '+FLibsPath,etInfo);
  end;
end;

{$ifndef FPCONLY}
function TLinux386_mipsel.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;
{$endif}

function TLinux386_mipsel.GetBinUtils(Basepath:string): boolean;
const
  DirName='mipsel-linux';
var
  AsFile: string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as';

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if result then
  begin
    FBinsFound:=true;
    infoln(FCrossModuleName + ': found binutils '+FBinUtilsPath,etInfo);
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding {Prepend the binutils names};
    infoln(FCrossModuleName + ': found binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
  end;
end;

constructor TLinux386_mipsel.Create;
begin
  inherited Create;
  FCrossModuleName:='Linux386_mipsel';
  FBinUtilsPrefix:='mipsel-linux-';
  FBinUtilsPath:='';
  FCrossModuleName:='TLinux386_mipsel'; //used in messages to user
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='mipsel';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('TLinux386_mipsel crosscompiler loading',etDebug);
end;

destructor TLinux386_mipsel.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF LINUX)}
var
  Linux386_mipsel:TLinux386_mipsel;

// Even though it's officially for x86, x64 may work
initialization
  Linux386_mipsel:=TLinux386_mipsel.Create;
  RegisterExtension(Linux386_mipsel.TargetCPU+'-'+Linux386_mipsel.TargetOS,Linux386_mipsel);
finalization
  Linux386_mipsel.Destroy;
{$ENDIF}
end.

