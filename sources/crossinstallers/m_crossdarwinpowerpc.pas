unit m_crossdarwinpowerpc;

{ Cross compiles from Darwin to Darwin powerpc
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwinpowerpc = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwinpowerpc }

function TDarwinpowerpc.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwinpowerpc.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwinpowerpc.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.powerpc;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor TDarwinpowerpc.Destroy;
begin
  inherited Destroy;
end;

{$ifdef DARWIN}
{$IF (NOT DEFINED(CPUPOWERPC)) OR (DEFINED(CPUPOWERPC64))}

var
  Darwinpowerpc:TDarwinpowerpc;

initialization
  Darwinpowerpc:=TDarwinpowerpc.Create;
  RegisterCrossCompiler(Darwinpowerpc.RegisterName,Darwinpowerpc);

finalization
  Darwinpowerpc.Destroy;

{$endif CPUPOWERPC}
{$endif DARWIN}
end.

