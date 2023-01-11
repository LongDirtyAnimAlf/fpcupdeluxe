unit m_crossiosarm;

{ Cross compiles from Darwin to iOS arm (iphone)
}

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_darwin_to_apple_base;

type
  TiOSarm = class(Tdarwin_apple)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TiOSarm }

function TiOSarm.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TiOSarm.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TiOSarm.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  FTargetOS:=TOS.ios;
  Reset;
  ShowInfo;
end;

destructor TiOSarm.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
var
  iOSarm:TiOSarm;

initialization
  iOSarm:=TiOSarm.Create;
  RegisterCrossCompiler(iOSarm.RegisterName,iOSarm);

finalization
  iOSarm.Destroy;

{$endif Darwin}
end.

