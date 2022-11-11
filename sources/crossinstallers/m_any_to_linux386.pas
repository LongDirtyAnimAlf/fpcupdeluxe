unit m_any_to_linux386;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_linux_base;

type
  Tany_linux386 = class(Tany_linux)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ Tany_linux386 }

function Tany_linux386.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function Tany_linux386.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor Tany_linux386.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  Reset;
  ShowInfo;
end;

destructor Tany_linux386.Destroy;
begin
  inherited Destroy;
end;

var
  any_linux386:Tany_linux386;

initialization
  any_linux386:=Tany_linux386.Create;
  RegisterCrossCompiler(any_linux386.RegisterName,any_linux386);

finalization
  any_linux386.Destroy;

end.

