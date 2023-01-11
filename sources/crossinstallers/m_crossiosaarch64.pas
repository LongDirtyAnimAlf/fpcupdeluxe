unit m_crossiosaarch64;

{ Cross compiles from Darwin to iSO aarch64 (iphone)
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TiOSaarch64 = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TiOSaarch64 }

function TiOSaarch64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TiOSaarch64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TiOSaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.ios;
  Reset;
  ShowInfo;
end;

destructor TiOSaarch64.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
var
  iOSaarch64:TiOSaarch64;

initialization
  iOSaarch64:=TiOSaarch64.Create;
  RegisterCrossCompiler(iOSaarch64.RegisterName,iOSaarch64);

finalization
  iOSaarch64.Destroy;

{$endif Darwin}
end.

