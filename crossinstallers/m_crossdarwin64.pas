unit m_crossdarwin64;

{  Cross compiles from Darwin x86/32 bit to Darwin x86_64 code
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  fpcuputil;

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
var
  aOption:string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:='';

  result:=true;
  FBinsFound:=true;

  aOption:=GetDarwinSDKVersion('macosx');
  if Length(aOption)>0 then
  begin
    if CompareVersionStrings(aOption,'10.8')>=0 then
    begin
      aOption:='10.8';
    end;
    AddFPCCFGSnippet('-WM'+aOption);
  end;
end;

constructor TDarwin64.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwin32';
  FTargetCPU:=TCPU.x86_64;
  FTargetOS:=TOS.darwin;
  Reset;
  FAlreadyWarned:=false;
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
  RegisterCrossCompiler(darwin64.RegisterName,darwin64);

finalization
  darwin64.Destroy;
{$ENDIF}
{$ENDIF}
end.

