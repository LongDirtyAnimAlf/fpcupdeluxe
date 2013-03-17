unit m_freebsd_to_linux386;

{ Cross compiles from FreeBSD x64 and presumably x86 to Linux using the emulator libraries in /compat
Needed ports/packages:
emulators/linux_base
something like /usr/ports/emulators/linux_dist* however that does not seem to work in PCBSD right now
note: gentoo will install in /usr/local/gentoo-stage3/ and not overwrite the base linux system compat, which ais as it should.

Another solution would be to get the relevant binutils/libs from a working Linux environment - which though? - to /cmpat/linux
}
//todo: replace paths below once we've got a working BSD Linux compat layer with dev tools.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation
type

{ TFreeBSD_Linux386 }

TFreeBSD_Linux386 = class(TCrossInstaller)
private

public
  function GetLibs(Basepath:string):boolean;override;
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function TFreeBSD_Linux386.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='/compat/linux/lib';
  result:=true;
end;

function TFreeBSD_Linux386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;

function TFreeBSD_Linux386.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPath:='/compat/linux/bin'; //these do not contain as etc though
  FBinUtilsPrefix:='';
  result:=FileExists(FBinUtilsPath+'as');
end;

constructor TFreeBSD_Linux386.Create;
begin
  inherited Create;
  FTargetCPU:='i386';
  FTargetOS:='linux';
end;

destructor TFreeBSD_Linux386.Destroy;
begin
  inherited Destroy;
end;

var
  FreeBSD_Linux386:TFreeBSD_Linux386;

initialization
  FreeBSD_Linux386:=TFreeBSD_Linux386.Create;
  RegisterExtension(FreeBSD_Linux386.TargetCPU+'-'+FreeBSD_Linux386.TargetOS,FreeBSD_Linux386);
finalization
  FreeBSD_Linux386.Destroy;
end.

