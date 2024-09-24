{ FPC and Lazarus installer/updater
Copyright (C) 2012-2014 Reinier Olislagers, Ludo Brands

Recent updates by Alfred, with the help of the fpc / lazarus community
BigChimp icon by Taazz
Revised icons by sesvena

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
program fpcupdeluxe;

{$mode objfpc}{$H+}
{$ifdef Windows}
{$APPTYPE GUI}
{$endif}

{$warn 5023 off : no warning about unused units}
(*
  The Initial Developer of the FPCUPdeluxe code is:
  Alfred Glänzer (donalfredo, aog)

  The Initial Developers of the Original FPCUP code are:
  Ludo Brands
  Reinier Olieslagers (bigchimp), may he rest in peace.

  Icon by Taazz

  Windows Dark Theme by 0x4A4D00

  Contributor(s):
    Denis Grinyuk (arvur)
    Maciej Izak (hnb)
    Michalis Kamburelis
    Marius Maximus
    Josh (alternateui)
    Ondrej Kelle
    Marco van de Voort (marcov)
    Olly (ollydev)
*)

uses
  {$IFDEF UNIX}
  cthreads,
  //BaseUnix,
  {$ENDIF}
  {$ifdef LCL}
  Interfaces, // this includes the LCL widgetset
  Forms,
  {$endif}
  Classes,
  {$ifndef LCL}
  SysUtils, Strings,
  FileUtil, LazFileUtils,
  synautil, // for rpos ... could also use strutil
  installerManager,
  installerCore,
  installerUniversal,
  checkoptions, fpcuputil,
  {$endif}
  {$ifdef LCL}
  {$ifdef windows}
  uMetaDarkStyle,
  uDarkStyleSchemes,
  uDarkStyleParams,
  {$endif Windows}
  {$ifdef READER}
  fpcupdeluxemainformreader,
  {$else}
  fpcupdeluxemainform,
  {$endif}
  {$endif}
  m_crossinstaller,
  m_any_to_androidarm,
  m_any_to_androidjvm,
  m_any_to_androidaarch64,
  m_any_to_androidx64,
  m_any_to_android386,
  m_any_to_linux386,
  m_any_to_linuxx64,
  m_any_to_linuxarm,
  m_any_to_linuxmips,
  m_any_to_linuxmipsel,
  m_any_to_linuxppc,
  m_any_to_linuxpowerpc64,
  m_any_to_linuxaarch64,
  m_any_to_linuxloongarch64,
  m_any_to_linuxriscv32,
  m_any_to_linuxriscv64,
  m_any_to_aros386,
  m_any_to_arosx64,
  m_any_to_arosarm,
  m_any_to_amigam68k,
  m_any_to_atarim68k,
  m_any_to_morphospowerpc,
  m_any_to_haiku386,
  m_any_to_haikux64,
  m_any_to_dragonflyx64,
  m_any_to_embeddedaarch64,
  m_any_to_embeddedarm,
  m_any_to_embeddedavr,
  m_any_to_embeddedmipsel,
  m_any_to_embeddedriscv32,
  m_any_to_javajvm,
  m_any_to_aixpowerpc,
  m_any_to_aixpowerpc64,
  m_any_to_solarisx64,
  m_any_to_solarissparc,
  m_any_to_msdosi8086,
  m_any_to_go32v2i386,
  m_any_to_linuxxtensa,
  m_any_to_linuxm68k,
  m_any_to_freertosxtensa,
  m_any_to_freertosarm,
  m_any_to_ultiboarm,
  m_any_to_ultiboaarch64,
  {$ifdef LINUX}
  //{$ifdef CPUX86}
  m_linux386_to_mips,
  m_linux386_to_wincearm,
  //{$endif}
  {$endif}
  {$ifdef Darwin}
  m_crossdarwin386,
  m_crossdarwinx64,
  m_crossdarwinaarch64,
  m_crossdarwin386iphonesim,
  m_crossdarwinx64iphonesim,
  m_crossiosarm,
  m_crossiosaarch64,
  m_crossdarwinpowerpc,
  m_crossdarwinpowerpc64,
  {$else}
  m_any_to_darwin386,
  m_any_to_darwinx64,
  m_any_to_darwinarm,
  m_any_to_darwinaarch64,
  m_any_to_iosarm,
  m_any_to_iosaarch64,
  m_any_to_darwinpowerpc,
  m_any_to_darwinpowerpc64,
  {$endif}
  {$if defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  {$if defined(FREEBSD) AND defined(CPU64)}
  m_freebsd64_to_freebsd32,
  {$endif}
  {$else}
  m_any_to_netbsd386,
  m_any_to_netbsdx64,
  m_any_to_freebsdx64,
  m_any_to_freebsdaarch64,
  m_any_to_freebsd386,
  m_any_to_openbsd386,
  m_any_to_openbsdx64,
  {$endif}
  {$ifdef MSWINDOWS}
  // Even though it's officially for Win32, win64 can run x86 binaries without problem, so allow it.
  m_win32_to_linuxmips,
  m_win32_to_wincearm,
  {$ifdef win64}
  m_crosswin32,
  {$ifdef CPUX86_64}
  m_crosswinarm64,
  {$endif}
  {$ifdef CPUAARCH64}
  m_crosswinx64,
  {$endif}
  {$endif win64}
  {$ifdef win32}
  m_crosswinx64,
  m_crosswinarm64,
  {$endif win32}
  {$else}
  m_anyinternallinker_to_win386,
  m_anyinternallinker_to_winarm64,
  m_anyinternallinker_to_winx64,
  {$endif MSWINDOWS}
  m_any_to_wasi_wasm32,
  m_any_to_embedded_wasm32;

{$R up.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$ifdef windows}
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$endif windows}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

