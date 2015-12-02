unit m_anyinternallinker_to_darwin32;

{ Cross compiles to Darwin x86 code
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

{ Tanyinternallinker_darwin32 }

Tanyinternallinker_darwin32 = class(TCrossInstaller)
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

{ Tanyinternallinker_darwin32 }

function Tanyinternallinker_darwin32.GetLibs(Basepath:string): boolean;
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
function Tanyinternallinker_darwin32.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function Tanyinternallinker_darwin32.GetBinUtils(Basepath:string): boolean;
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

constructor Tanyinternallinker_darwin32.Create;
begin
  inherited Create;
  FCrossModuleName:='anyinternallinker_darwin32';
  FTargetCPU:='i386';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  infoln('Tanyinternallinker_darwin32 crosscompiler loading',etDebug);
end;

destructor Tanyinternallinker_darwin32.Destroy;
begin
  inherited Destroy;
end;


var
  Anyinternallinker_darwin32:Tanyinternallinker_darwin32;

initialization
  Anyinternallinker_darwin32:=Tanyinternallinker_darwin32.Create;
  RegisterExtension(Anyinternallinker_darwin32.TargetCPU+'-'+Anyinternallinker_darwin32.TargetOS,Anyinternallinker_darwin32);
finalization
  Anyinternallinker_darwin32.Destroy;
end.

