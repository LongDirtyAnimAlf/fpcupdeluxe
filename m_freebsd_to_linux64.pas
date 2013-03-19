unit m_freebsd_to_linux64;

{
Cross compiles from FreeBSD x64 to Linux using the emulator libraries in /compat

Does not work yet due to missing binutils (at least)
Solution would be to get the relevant binutils/libs from a working Linux environment - which though? - in some directory below baspath
}
//todo: replace paths below once we've got a working BSD Linux compat layer with dev tools.
{
These approaches did not work; leave in as future reference
Needed ports/packages:
emulators/linux_base
something like /usr/ports/emulators/linux_dist* however that does not seem to work in PCBSD right now
note: gentoo will install in /usr/local/gentoo-stage3/ and not overwrite the base linux system compat, which ais as it should.


# For freebsd, these 2 ports may well be needed. They are installed by default on PC-BSD
cd /usr/ports/emulators/linux_base-f10
make -DBATCH install distclean
cd /usr/ports/x11/linux-f10-xorg-libs/
make -DBATCH install distclean
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
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
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
end;

function TFreeBSD_Linux64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;

function TFreeBSD_Linux64.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'/cross/bin/'+TargetSignature; //these do not contain as etc though
  if not(ForceDirectories(FBinUtilsPath)) then
  begin
    infoln('TFreeBSD_Linux64: Could not create binutils directory '+FBinUtilsPath,etError);
    FAlreadyWarned:=true;
    exit(false);
  end;
  FBinUtilsPrefix:='';
  // Check for and get Linux binutils. We do need Linux compatibility on FreeBSD, otherwise this won't work

  result:=FileExists(FBinUtilsPath+'/as'); // let the assembler be our coalmine canary
end;

constructor TFreeBSD_Linux64.Create;
begin
  inherited Create;
  FTargetCPU:='x86_64';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
end;

destructor TFreeBSD_Linux64.Destroy;
begin
  inherited Destroy;
end;

var
  FreeBSD_Linux64:TFreeBSD_Linux64;

initialization
  FreeBSD_Linux64:=TFreeBSD_Linux64.Create;
  RegisterExtension(FreeBSD_Linux64.TargetCPU+'-'+FreeBSD_Linux64.TargetOS,FreeBSD_Linux64);
finalization
  FreeBSD_Linux64.Destroy;
end.

