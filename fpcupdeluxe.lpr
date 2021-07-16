program fpcupdeluxe;

{$mode objfpc}{$H+}
{$ifdef Windows}
{$APPTYPE GUI}
{$endif}

(*
  The Initial Developer of the FPCUPdeluxe code is:
  Alfred Glänzer (donalfredo, aog)

  The Initial Developers of the Original FPCUP code are:
  Ludo Brands
  Reinier Olieslagers (bigchimp), may he rest in peace.

  Icon by Taazz

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
(*
//static QT5 on Linux
{$ifdef Linux}
  {$ifdef LCLQT5}
    //{$linklib libc_nonshared.a}
    {$L libgcc_s.so.1}
    {$L libstdc++.so.6}
    {$L libQt5PrintSupport.so.5}
    {$L libQt5Widgets.so.5}
    {$L libQt5Gui.so.5}
    {$L libQt5Network.so.5}
    {$L libQt5Core.so.5}
    {$L libQt5X11Extras.so.5}
    {$linklib libQt5Pas.a}
  {$endif}
{$endif}
*)

uses
  {$IFDEF UNIX}
  cthreads,
  //cmem,  // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Classes, SysUtils,
  //extrasettings,
  //installerManager, installerUniversal,
  //modulesettings, unzipprogress, infounit, fpcuputil,
  m_crossinstaller,
  m_any_to_androidarm,
  m_any_to_androidjvm,
  m_any_to_androidaarch64,
  m_any_to_androidx64,
  m_any_to_android386,
  m_any_to_linuxarm,
  m_any_to_linuxmips,
  m_any_to_linuxmipsel,
  m_any_to_linuxpowerpc64,
  m_any_to_linuxaarch64,
  m_any_to_aros386,
  m_any_to_arosx64,
  m_any_to_arosarm,
  m_any_to_amigam68k,
  m_any_to_morphospowerpc,
  m_any_to_haiku386,
  m_any_to_haikux64,
  m_any_to_dragonflyx64,
  m_any_to_embeddedaarch64,
  m_any_to_embeddedarm,
  m_any_to_embeddedavr,
  m_any_to_embeddedmipsel,
  m_any_to_javajvm,
  m_any_to_aixpowerpc,
  m_any_to_aixpowerpc64,
  m_any_to_solarisx64,
  m_any_to_solarissparc,
  m_any_to_msdosi8086,
  m_any_to_go32v2i386,
  m_any_to_linuxxtensa,
  m_any_to_freertosxtensa,
  m_any_to_freertosarm,
  m_any_to_ultiboarm,
  {$ifdef LINUX}
  //{$ifdef CPUX86}
  m_linux386_to_mips,
  m_linux386_to_wincearm,
  //{$endif}
  {$endif}
  {$ifdef Darwin}
  {$ifndef CPUX86_64}
  m_crossdarwin64,
  {$endif}
  {$ifndef CPUX86}
  m_crossdarwin32,
  {$endif}
  {$ifndef CPUAARCH64}
  m_crossdarwinaarch64,
  {$endif}
  {$ifdef CPUX86}
  m_crossdarwinpowerpc,
  m_crossdarwin386iphonesim,
  {$endif}
  {$ifdef CPUX86_64}
  m_crossdarwinx64iphonesim,
  {$endif}
  m_crossiosarm,
  m_crossiosaarch64,
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
  {$IF defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  m_freebsd_to_linux386,
  {$ifdef CPU64}
  m_freebsd64_to_freebsd32,
  {$endif CPU64}
  m_freebsd_to_linux64,
  {$else}
  m_any_to_linux386,
  m_any_to_linuxx64,
  m_any_to_netbsdx64,
  m_any_to_freebsdx64,
  m_any_to_freebsd386,
  m_any_to_openbsd386,
  m_any_to_openbsdx64,
  {$endif}
  {$ifdef MSWINDOWS}
  m_win32_to_linuxmips, m_win32_to_wincearm,
  {$ifdef win64}
  m_crosswin32,
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
  m_any_to_embedded_wasm32,
  fpcupdeluxemainform;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

