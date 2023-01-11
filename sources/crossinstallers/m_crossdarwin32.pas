unit m_crossdarwin32;

{ Cross compiles from Darwin to Darwin i386
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwin32 = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwin32 }

function TDarwin32.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwin32.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwin32.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor TDarwin32.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
{$ifndef CPUX86}
var
  Darwin32:TDarwin32;

initialization
  Darwin32:=TDarwin32.Create;
  RegisterCrossCompiler(Darwin32.RegisterName,Darwin32);

finalization
  Darwin32.Destroy;

{$endif CPUX86}
{$endif Darwin}
end.

