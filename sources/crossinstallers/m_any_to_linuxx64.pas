unit m_any_to_linuxx64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_linux_base;

type
  Tany_linux64 = class(Tany_linux)
  public
    constructor Create;
  end;

{ Tany_linux64 }
constructor Tany_linux64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.x86_64;
  Reset;
  ShowInfo;
end;

var
  any_linux64:Tany_linux64;

initialization
  any_linux64:=Tany_linux64.Create;
  RegisterCrossCompiler(any_linux64.RegisterName,any_linux64);

finalization
  any_linux64.Destroy;

end.

