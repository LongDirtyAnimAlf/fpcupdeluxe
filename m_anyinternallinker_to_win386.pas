unit m_anyinternallinker_to_win386;

{ Cross compiles from Linux, FreeBSD,... to Windows i386 code (win32)
Requirements: FPC should have an internal linker
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller, fpcuputil;

implementation
const
  ErrorNotFound='An error occurred getting cross compiling binutils/libraries.'+LineEnding+
    'todo: specify what exactly is missing';

type

{ Tanyinternallinker_win386 }

Tanyinternallinker_win386 = class(TCrossInstaller)
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

{ TWin32 }

function Tanyinternallinker_win386.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

{$ifndef FPCONLY}
function Tanyinternallinker_win386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function Tanyinternallinker_win386.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;
end;

constructor Tanyinternallinker_win386.Create;
begin
  inherited Create;
  FCrossModuleName:='anyinternallinker_win386';
  FTargetCPU:='i386';
  FTargetOS:='win32';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  infoln('Tanyinternallinker_win386 crosscompiler loading',etDebug);
end;

destructor Tanyinternallinker_win386.Destroy;
begin
  inherited Destroy;
end;

var
  Anyinternallinker_win386:Tanyinternallinker_win386;

initialization
  Anyinternallinker_win386:=Tanyinternallinker_win386.Create;
  RegisterExtension(Anyinternallinker_win386.TargetCPU+'-'+Anyinternallinker_win386.TargetOS,Anyinternallinker_win386);
finalization
  Anyinternallinker_win386.Destroy;
end.

