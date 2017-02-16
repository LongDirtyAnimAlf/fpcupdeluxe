unit m_crossdarwin64;

{  Cross compiles from Darwin x86/32 bit to Darwin x86_64 code
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

type

{ TDarwin64 }

TDarwin64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function TDarwin64.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;

  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

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
  FCrossModuleNamePrefix:='TDarwin32';
  FTargetCPU:='x86_64';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  ShowInfo;
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

