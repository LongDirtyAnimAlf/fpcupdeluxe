unit m_anyinternallinker_to_darwin64;

{ Cross compiles from Linux, FreeBSD,... to Darwin x86_64 code
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

{ Tanyinternallinker_darwin64 }

Tanyinternallinker_darwin64 = class(TCrossInstaller)
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

function Tanyinternallinker_darwin64.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='';
  result:=true;
  if (result=false) and (FAlreadyWarned=false) then
  begin
    infoln(ErrorNotFound,etError);
    FAlreadyWarned:=true;
  end;
end;

{$ifndef FPCONLY}
function Tanyinternallinker_darwin64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function Tanyinternallinker_darwin64.GetBinUtils(Basepath:string): boolean;
begin
  inherited;
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  if (result=false) and (FAlreadyWarned=false) then
  begin
    infoln(ErrorNotFound,etError);
    FAlreadyWarned:=true;
  end;
end;

constructor Tanyinternallinker_darwin64.Create;
begin
  inherited Create;
  FCrossModuleName:='anyinternallinker_darwin64';
  FTargetCPU:='x86_64';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  infoln('Tanyinternallinker_darwin64 crosscompiler loading',etDebug);
end;

destructor Tanyinternallinker_darwin64.Destroy;
begin
  inherited Destroy;
end;


var
  Anyinternallinker_darwin64:Tanyinternallinker_darwin64;

initialization
  Anyinternallinker_darwin64:=Tanyinternallinker_darwin64.Create;
  RegisterExtension(Anyinternallinker_darwin64.TargetCPU+'-'+Anyinternallinker_darwin64.TargetOS,Anyinternallinker_darwin64);
finalization
  Anyinternallinker_darwin64.Destroy;
end.

