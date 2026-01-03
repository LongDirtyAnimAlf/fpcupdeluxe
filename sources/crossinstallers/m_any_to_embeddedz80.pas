unit m_any_to_embeddedz80;
{ Cross compiles from any platform to embedded z80
Copyright (C) 2025 Alf

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
  Classes;

implementation

uses
  installerBase, m_crossinstaller, m_any_to_all_z80;

type
  TAny_EmbeddedZ80 = class(TAny_AllZ80)
  public
    constructor Create;
  end;

constructor TAny_EmbeddedZ80.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.z80;
  FTargetOS:=TOS.embedded;
  Reset;
  ShowInfo;
end;

var
  Any_EmbeddedZ80:TAny_EmbeddedZ80;

initialization
  Any_EmbeddedZ80:=TAny_EmbeddedZ80.Create;
  RegisterCrossCompiler(Any_EmbeddedZ80.RegisterName,Any_EmbeddedZ80);

finalization
  Any_EmbeddedZ80.Destroy;
end.

