unit m_any_to_linuxaarch64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_linux_base;

type
  Tany_linuxaarch64 = class(Tany_linux)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ Tany_linuxaarch64 }

function Tany_linuxaarch64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function Tany_linuxaarch64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor Tany_linuxaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  Reset;
  ShowInfo;
end;

destructor Tany_linuxaarch64.Destroy;
begin
  inherited Destroy;
end;

var
  any_linuxaarch64:Tany_linuxaarch64;

initialization
  any_linuxaarch64:=Tany_linuxaarch64.Create;
  RegisterCrossCompiler(any_linuxaarch64.RegisterName,any_linuxaarch64);

finalization
  any_linuxaarch64.Destroy;

end.

