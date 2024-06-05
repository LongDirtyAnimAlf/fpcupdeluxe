unit m_any_to_linuxriscv64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_linux_base;

type
  Tany_linuxriscv64 = class(Tany_linux)
  public
    constructor Create;
  end;

{ Tany_linuxriscv64 }
constructor Tany_linuxriscv64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.riscv64;
  Reset;
  ShowInfo;
end;

var
  any_linuxriscv64:Tany_linuxriscv64;

initialization
  any_linuxriscv64:=Tany_linuxriscv64.Create;
  RegisterCrossCompiler(any_linuxriscv64.RegisterName,any_linuxriscv64);

finalization
  any_linuxriscv64.Destroy;

end.

