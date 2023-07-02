unit m_freebsd_to_linux386;

{ Cross compiles from FreeBSD x64 and presumably x86 to Linux using the emulator libraries in /compat
Needed ports/packages:
emulators/linux_base
something like /usr/ports/emulators/linux_dist* however that does not seem to work in PCBSD right now
note: gentoo will install in /usr/local/gentoo-stage3/ and not overwrite the base linux system compat, which ais as it should.


# For freebsd, these 2 ports may well be needed. They are installed by default on PC-BSD
cd /usr/ports/emulators/linux_base-f10
make -DBATCH install distclean
cd /usr/ports/x11/linux-f10-xorg-libs/
make -DBATCH install distclean

Another solution would be to get the relevant binutils/libs from a working Linux environment - which though? - to /cmpat/linux
}

//todo: replace paths below once we've got a working BSD Linux compat layer with dev tools.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller, fpcuputil;

implementation
type

{ TFreeBSD_Linux386 }

TFreeBSD_Linux386 = class(TCrossInstaller)
private

public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function TFreeBSD_Linux386.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FLibsPath:='/compat/linux/lib';
  result:=DirectoryExists(FLibsPath);
  if result then
  begin
    FLibsFound:=true;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
  end;
  {
  perhaps these?!? todo: check.
  -Fl/compat/linux/usr/lib
  -Fl/compat/linux/usr/X11R6/lib
  }
end;

function TFreeBSD_Linux386.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='/compat/linux/bin'; //these do not contain as etc though
  FBinUtilsPrefix:='';
  result:=FileExists(FBinUtilsPath+'/as'); // let the assembler be our coalmine canary
  if result then
  begin
    FBinsFound:=true;
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix);
  end;
end;

constructor TFreeBSD_Linux386.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.linux;
  Reset;
  ShowInfo;
end;

destructor TFreeBSD_Linux386.Destroy;
begin
  inherited Destroy;
end;

{$if defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
var
  FreeBSD_Linux386:TFreeBSD_Linux386;

initialization
  FreeBSD_Linux386:=TFreeBSD_Linux386.Create;
  RegisterCrossCompiler(FreeBSD_Linux386.RegisterName,FreeBSD_Linux386);
finalization
  FreeBSD_Linux386.Destroy;
{$endif}

end.

