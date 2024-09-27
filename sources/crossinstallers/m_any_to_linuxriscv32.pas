unit m_any_to_linuxriscv32;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  installerBase, m_crossinstaller, m_any_to_linux_base;

type
  Tany_linuxriscv32 = class(Tany_linux)
  public
    constructor Create;
  end;

{ Tany_linuxriscv32 }
constructor Tany_linuxriscv32.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.riscv32;
  Reset;
  ShowInfo;
end;

var
  any_linuxriscv32:Tany_linuxriscv32;

initialization
  any_linuxriscv32:=Tany_linuxriscv32.Create;
  RegisterCrossCompiler(any_linuxriscv32.RegisterName,any_linuxriscv32);

finalization
  any_linuxriscv32.Destroy;

end.

