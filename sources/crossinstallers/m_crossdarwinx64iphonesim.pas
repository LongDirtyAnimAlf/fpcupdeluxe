unit m_crossdarwinx64iphonesim;

{ Cross compiles from Darwin to iOS x86_64 (iphonesim)
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TDarwin64iphonesim = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TDarwin64iphonesim }

function TDarwin64iphonesim.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TDarwin64iphonesim.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TDarwin64iphonesim.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.x86_64;
  FTargetOS:=TOS.iphonesim;
  Reset;
  ShowInfo;
end;

destructor TDarwin64iphonesim.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
{$ifdef CPUX86_64}
var
  Darwin64iphonesim:TDarwin64iphonesim;

initialization
  Darwin64iphonesim:=TDarwin64iphonesim.Create;
  RegisterCrossCompiler(Darwin64iphonesim.RegisterName,Darwin64iphonesim);

finalization
  Darwin64iphonesim.Destroy;

{$endif CPUX86_64}
{$endif Darwin}
end.

