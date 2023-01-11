unit m_crossdarwinaarch64;

{ Cross compiles from Darwin to Darwin aarch64 (Apple Silicon)
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwinaarch64 = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwinaarch64 }

function TDarwinaarch64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwinaarch64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwinaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor TDarwinaarch64.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
{$ifndef CPUAARCH64}
var
  Darwinaarch64:TDarwinaarch64;

initialization
  Darwinaarch64:=TDarwinaarch64.Create;
  RegisterCrossCompiler(Darwinaarch64.RegisterName,Darwinaarch64);

finalization
  Darwinaarch64.Destroy;

{$endif CPUAARCH64}
{$endif Darwin}
end.

