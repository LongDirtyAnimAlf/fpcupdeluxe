unit m_crossdarwin386iphonesim;

{ Cross compiles from Darwin to iOS i386 (iphonesim)
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwin386iphonesim = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwin386iphonesim }

function TDarwin386iphonesim.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwin386iphonesim.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwin386iphonesim.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.iphonesim;
  Reset;
  ShowInfo;
end;

destructor TDarwin386iphonesim.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
{$ifdef CPUX86}
var
  Darwin386iphonesim:TDarwin386iphonesim;

initialization
  Darwin386iphonesim:=TDarwin386iphonesim.Create;
  RegisterCrossCompiler(Darwin386iphonesim.RegisterName,Darwin386iphonesim);

finalization
  Darwin386iphonesim.Destroy;

{$endif CPUX86}
{$endif Darwin}
end.

