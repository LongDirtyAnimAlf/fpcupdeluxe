unit m_crossdarwinpowerpc64;

{ Cross compiles from Darwin to Darwin powerpc64
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwinpowerpc64 = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwinpowerpc64 }

function TDarwinpowerpc64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwinpowerpc64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwinpowerpc64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.powerpc64;
  FTargetOS:=TOS.darwin;
  Reset;
  ShowInfo;
end;

destructor TDarwinpowerpc64.Destroy;
begin
  inherited Destroy;
end;

{$ifdef DARWIN}
{$ifndef CPUPOWERPC64}
var
  Darwinpowerpc64:TDarwinpowerpc64;

initialization
  Darwinpowerpc64:=TDarwinpowerpc64.Create;
  RegisterCrossCompiler(Darwinpowerpc64.RegisterName,Darwinpowerpc64);

finalization
  Darwinpowerpc64.Destroy;

{$endif CPUPOWERPC64}
{$endif DARWIN}
end.

