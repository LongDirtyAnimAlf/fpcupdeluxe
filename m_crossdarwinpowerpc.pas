unit m_crossdarwinpowerpc;

{ Cross compiles from Darwin i386 to Darwin powerpc
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

type

{ TDarwinpowerpc }

TDarwinpowerpc = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwinpowerpc }

function TDarwinpowerpc.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

function TDarwinpowerpc.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwinpowerpc.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwin32';
  FTargetCPU:='powerpc';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  ShowInfo;
end;

destructor TDarwinpowerpc.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
{$IFDEF CPUi386}

var
  Darwinpowerpc:TDarwinpowerpc;

initialization
  Darwinpowerpc:=TDarwinpowerpc.Create;
  RegisterExtension(Darwinpowerpc.TargetCPU+'-'+Darwinpowerpc.TargetOS,Darwinpowerpc);
finalization
  Darwinpowerpc.Destroy;
{$ENDIF}
{$ENDIF}
end.

