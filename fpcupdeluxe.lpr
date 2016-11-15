program fpcupdeluxe;

{$mode objfpc}{$H+}

//{$APPTYPE GUI}

// todo: give info about the link that must be used to start Lazarus that is created by fpcupdeluxe !!
// or make a button or popup or something for this link.
// todo: add some means of help

{$IFDEF LINUX}
  {$IFDEF FPC_CROSSCOMPILING}
    {$linklib libc_nonshared.a}
    {$IFDEF CPUARM}
      {$linklib GLESv2}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fpcupdeluxemainform, extrasettings,
  Classes, sysutils, strings,
  FileUtil, LazFileUtils,
  synautil, // for rpos ... could also use strutil
  fpcuputil, commandline, installerUniversal, installerManager,
  m_crossinstaller,
  m_linux386_to_mips,
  m_any_to_aixpowerpc, m_any_to_androidarm, m_any_to_linuxarm,
  m_any_to_embeddedarm, m_any_to_linuxmipsel, m_any_to_linux386,
  m_any_to_linuxaarch64,
  m_any_to_freebsdx64,
  m_any_to_freebsd386,
  m_any_to_androidjvm,m_any_to_javajvm,
  m_any_to_darwin386,
  {$IFDEF Darwin}
  m_crossdarwin64,
  m_crossdarwin32,
  {$endif}
  {$IF defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  m_freebsd_to_linux386, m_freebsd64_to_freebsd32, m_freebsd_to_linux64,
  {$endif}
  {$IFDEF MSWINDOWS}
  m_crosswin32, m_crosswin64,
  m_win32_to_linuxmips, m_win32_to_msdosi8086, m_win32_to_go32v2i386, m_win32_to_wincearm, m_win32_to_linux386,
  m_win64_to_linux64,
  {$endif}
  m_anyinternallinker_to_win386,
  m_anyinternallinker_to_win64
  ;

{$i revision.inc}

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

