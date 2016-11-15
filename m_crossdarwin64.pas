unit m_crossdarwin64;

{  Cross compiles from Darwin x86/32 bit to Darwin x86_64 code
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

{ TDarwin64 }

TDarwin64 = class(TCrossInstaller)
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

function TDarwin64.GetLibs(Basepath:string): boolean;
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
function TDarwin64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function TDarwin64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwin64.Create;
begin
  inherited Create;
  FCrossModuleName:='darwin64';
  FTargetCPU:='x86_64';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  infoln('TDarwin64 crosscompiler loading',etDebug);
end;

destructor TDarwin64.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
{$IFNDEF CPUX86_64}

var
  darwin64:TDarwin64;

initialization
  darwin64:=TDarwin64.Create;
  RegisterExtension(darwin64.TargetCPU+'-'+darwin64.TargetOS,darwin64);
finalization
  darwin64.Destroy;
{$ENDIF}
{$ENDIF}
end.

