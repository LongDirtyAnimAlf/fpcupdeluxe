unit m_crossdarwinarm;

{ Cross compiles from Darwin i386 to Darwin arm
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

{ TDarwinarm }

TDarwinarm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwinarm }

function TDarwinarm.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

function TDarwinarm.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwinarm.Create;
begin
  inherited Create;
  FCrossModuleName:='Darwinarm';
  FTargetCPU:='arm';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  infoln('TDarwinarm crosscompiler loading',etDebug);
end;

destructor TDarwinarm.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
{$IFDEF CPUi386}

var
  Darwinarm:TDarwinarm;

initialization
  Darwinarm:=TDarwinarm.Create;
  RegisterExtension(Darwinarm.TargetCPU+'-'+Darwinarm.TargetOS,Darwinarm);
finalization
  Darwinarm.Destroy;
{$ENDIF}
{$ENDIF}
end.

