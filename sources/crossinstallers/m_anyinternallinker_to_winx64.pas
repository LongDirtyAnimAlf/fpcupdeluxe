unit m_anyinternallinker_to_winx64;

{ Cross compiles from Linux, FreeBSD,... to Windows x86_64 code (winx64)
Requirements: FPC should have an internal linker
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

type

{ Tanyinternallinker_winx64 }

Tanyinternallinker_winx64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs({%H-}Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function Tanyinternallinker_winx64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

function Tanyinternallinker_winx64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix

  result:=true;
  FBinsFound:=true;
end;

constructor Tanyinternallinker_winx64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.x86_64;
  FTargetOS:=TOS.win64;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tanyinternallinker_winx64.Destroy;
begin
  inherited Destroy;
end;

var
  Anyinternallinker_winx64:Tanyinternallinker_winx64;

initialization
  Anyinternallinker_winx64:=Tanyinternallinker_winx64.Create;
  RegisterCrossCompiler(Anyinternallinker_winx64.RegisterName,Anyinternallinker_winx64);

finalization
  Anyinternallinker_winx64.Destroy;
end.

