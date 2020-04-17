unit m_crossdarwin32;

{ Cross compiles from Darwin x86_64 code to Darwin x86/32 bit
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  fpcuputil;

type

{ TDarwin32 }

TDarwin32 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwin32 }

function TDarwin32.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

function TDarwin32.GetBinUtils(Basepath:string): boolean;
var
  aOption:string;
begin
  result:=inherited;
  if result then exit;
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;

  aOption:=GetSDKVersion('macosx');
  if Length(aOption)>0 then
  begin
    if CompareVersionStrings(aOption,'10.8')>=0 then
    begin
      aOption:='10.8';
    end;
    AddFPCCFGSnippet('-WM'+aOption);
  end;

end;

constructor TDarwin32.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwin64';
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.darwin;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TDarwin32.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
{$IFDEF CPUX86_64}

var
  Darwin32:TDarwin32;

initialization
  Darwin32:=TDarwin32.Create;
  RegisterCrossCompiler(Darwin32.RegisterName,Darwin32);

finalization
  Darwin32.Destroy;
{$ENDIF}
{$ENDIF}
end.

