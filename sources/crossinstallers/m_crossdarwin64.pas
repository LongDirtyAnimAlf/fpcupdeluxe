unit m_crossdarwin64;

{ Cross compiles from Darwin to Darwin x86_64
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwin64 = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwin64 }

function TDarwin64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwin64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwin64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.x86_64;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor TDarwin64.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
{$ifndef CPUX86_64}
var
  Darwin64:TDarwin64;

initialization
  Darwin64:=TDarwin64.Create;
  RegisterCrossCompiler(Darwin64.RegisterName,Darwin64);

finalization
  Darwin64.Destroy;

{$endif CPUX86}
{$endif Darwin}
end.

