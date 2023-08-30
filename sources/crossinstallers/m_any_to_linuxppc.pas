unit m_any_to_linuxppc;

{ Cross compiles from any (or any other OS with relevant binutils/libs) to Linux ppc 32 bit
Copyright (C) 2014 Reinier Olislagers

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_linux_base;

type

{ Tany_linuxppc }
Tany_linuxppc = class(Tany_linux)
public
  constructor Create;
end;

{ Tany_linuxppc }

constructor Tany_linuxppc.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.powerpc;
  Reset;
  ShowInfo;
end;

var
  any_linuxppc:Tany_linuxppc;

initialization
  any_linuxppc:=Tany_linuxppc.Create;
  RegisterCrossCompiler(any_linuxppc.RegisterName,any_linuxppc);

finalization
  any_linuxppc.Destroy;

end.

