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
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function TFreeBSD_Linux386.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;

  FLibsPath:='/compat/linux/lib';
  result:=DirectoryExists(FLibsPath);
  if result then
  begin
    FLibsFound:=true;
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Xr'+IncludeTrailingPathDelimiter(FLibsPath); //set linker's rlink path
  end;
  {
  perhaps these?!? todo: check.
  -Fl/compat/linux/lib
  -Fl/compat/linux/usr/lib
  -Fl/compat/linux/usr/X11R6/lib
  }
end;

{$ifndef FPCONLY}
function TFreeBSD_Linux386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function TFreeBSD_Linux386.GetBinUtils(Basepath:string): boolean;
var
  i:integer;
begin
  result:=inherited;
  if result then exit;

  //todo: remove once done
  infoln('TFreeBSD_Linux386: Experimental, not finished. Stopping now.',etError);
  result:=false;

  FBinUtilsPath:='/compat/linux/bin'; //these do not contain as etc though
  FBinUtilsPrefix:='';
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

constructor TFreeBSD_Linux386.Create;
begin
  inherited Create;
  FCrossModuleName:='FreeBSD_Linux386';
  FBinUtilsPath:='';
  FBinUtilsPrefix:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='i386';
  FTargetOS:='linux';
  infoln('TFreeBSD_Linux386 crosscompiler loading',etDebug);
end;

destructor TFreeBSD_Linux386.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF FREEBSD}
var
  FreeBSD_Linux386:TFreeBSD_Linux386;

initialization
  FreeBSD_Linux386:=TFreeBSD_Linux386.Create;
  RegisterExtension(FreeBSD_Linux386.TargetCPU+'-'+FreeBSD_Linux386.TargetOS,FreeBSD_Linux386);
finalization
  FreeBSD_Linux386.Destroy;
{$ENDIF FREEBSD}

end.

