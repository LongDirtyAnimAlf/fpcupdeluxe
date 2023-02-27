{ FPC and Lazarus installer/updater
Copyright (C) 2012-2014 Reinier Olislagers, Ludo Brands

Recent updates by Alfred, with the help of the fpc / lazarus community
Icon by Taazz

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
program fpcup;

{ Command line interface to installing/updating FPC/Lazarus instances }
{ Code conventions in this project:
- comment as much as you can
- all variables with directories contain:
  no trailing delimiter (/ or \) and
  absolute paths
}
{$mode objfpc}{$H+}

{$warn 5023 off : no warning about unused units}

uses
  {$IFDEF UNIX}
  cthreads,
  BaseUnix,
  {$ENDIF}
  Classes, sysutils, strings,
  FileUtil, LazFileUtils,
  synautil, // for rpos ... could also use strutil
  installerManager, installerUniversal,
  checkoptions, fpcuputil,
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
  m_any_to_linuxloongarch64,
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
  {$if defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  m_freebsd_to_linux386,
  {$if defined(FREEBSD) AND defined(CPU64)}
  m_freebsd64_to_freebsd32,
  {$endif}
  m_freebsd_to_linux64,
  {$else}
  m_any_to_linux386,
  m_any_to_linuxx64,
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

// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}
{$I fpcuplprbase.inc}

var
  FPCupManager:TFPCupManager;
  res:integer;
  BinsName,LibsName:string;
  BinsURL,LibsURL:string;
  BinsPath,LibsPath:string;
  ToolTargetFile,ToolTargetPath:string;
  UpOk,BinsOk,LibsOk:boolean;
  {$ifdef Unix}
  aList:TStringList;
  i:integer;
  {$endif Unix}

{$R fpcup.res}

begin
  {$ifndef FPCONLY}
  writeln('Fpclazup, a FPC/Lazarus downloader/updater/installer');
  {$else}
  writeln('Fpcup, a FPC downloader/updater/installer');
  {$endif}
  writeln('Original by BigChimp: https://bitbucket.org/reiniero/fpcup');
  writeln('This version: https://github.com/LongDirtyAnimAlf/Reiniero-fpcup');
  writeln('');
  {$ifndef FPCONLY}
  writeln('Fpclazup will download the FPC and Lazarus sources');
  writeln('from the source SVN repositories, and compile, and install.');
  writeln('Result: you get a fresh, up-to-date Lazarus/FPC installation.');
  {$else}
  writeln('Fpcup will download the FPC sources');
  writeln('from the source SVN repositories, and compile, and install.');
  writeln('Result: you get a fresh, up-to-date FPC installation.');
  {$endif}
  writeln('');
  writeversion;

  try
    FPCupManager:=TFPCupManager.Create;

    res:=CheckFPCUPOptions(FPCupManager); //Process command line arguments
    if res=CHECKOPTIONS_SUCCESS then
    begin
      // Get/update/compile selected modules
      UpOk:=FPCupManager.Run;
      if (NOT UpOk) then
      begin
        {$ifndef FPCONLY}
        writeln('Fpclazup failed.');
        {$else}
        writeln('Fpcup failed.');
        {$endif}
        if ((FPCupManager.CrossOS_Target<>TOS.osNone) OR (FPCupManager.CrossCPU_Target<>TCPU.cpuNone)) then
        begin
          FPCupManager.GetCrossToolsFileName({%H-}BinsName,{%H-}LibsName);
          FPCupManager.GetCrossToolsPath({%H-}BinsPath,{%H-}LibsPath);
          BinsOk:=FPCupManager.GetCrossBinsURL({%H-}BinsURL,BinsName);
          if (BinsOk AND FPCupManager.AutoTools) then
          begin
            writeln('Found correct online binutils at: '+BinsURL);
            writeln('Going to download the cross-bins. Can (will) take some time !');
            ToolTargetFile := IncludeTrailingPathDelimiter(FPCupManager.TempDirectory)+BinsName;
            SysUtils.DeleteFile(ToolTargetFile);
            BinsOk:=DownLoad(FPCupManager.UseWget,BinsURL,ToolTargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            if BinsOk then
            begin
              writeln('Download successfull. Unpacking archive.');
              ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory);
              {$ifndef MSWINDOWS}
              ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+BinsPath+DirectorySeparator;
              {$endif}
              ForceDirectoriesSafe(ToolTargetPath);
              with TNormalUnzipper.Create do
              begin
                try
                  BinsOk:=DoUnZip(ToolTargetFile,ToolTargetPath,[]);
                finally
                  Free;
                end;
              end;
              if BinsOk then SysUtils.DeleteFile(ToolTargetFile);
            end;
            if BinsOk then
            begin
              {$IFDEF UNIX}
              aList:=FindAllFiles(ToolTargetPath);
              try
                if (aList.Count > 0) then
                begin
                  for i:=0 to Pred(aList.Count) do
                  begin
                    fpChmod(aList.Strings[i],&755);
                  end;
                end;
              finally
                aList.Free;
              end;
              {$ENDIF}
            end;
          end;

          LibsOk:=FPCupManager.GetCrossLibsURL({%H-}LibsURL,LibsName);
          if (BinsOk AND FPCupManager.AutoTools) then
          begin
            writeln('Found correct online libraries at: '+LibsURL);
            writeln('Going to download the cross-libs. Can (will) take some time !');
            ToolTargetFile := IncludeTrailingPathDelimiter(FPCupManager.TempDirectory)+LibsName;
            SysUtils.DeleteFile(ToolTargetFile);
            LibsOk:=DownLoad(FPCupManager.UseWget,LibsURL,ToolTargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            if LibsOk then
            begin
              writeln('Download successfull. Unpacking archive.');
              ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory);
              //ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+LibsPath+DirectorySeparator;
              ForceDirectoriesSafe(ToolTargetPath);
              with TNormalUnzipper.Create do
              begin
                try
                  LibsOk:=DoUnZip(ToolTargetFile,ToolTargetPath,[]);
                finally
                  Free;
                end;
              end;
              if LibsOk then SysUtils.DeleteFile(ToolTargetFile);
            end;
          end;
          // as libraries for embedded are not always needed, end with success even if the above has failed
          if (NOT LibsOk) then
          begin
            if (FPCupManager.CrossOS_Target=TOS.embedded) then
            begin
              LibsURL:='';
              LibsOk:=true;
            end;
          end;
        end;

        if (BinsOK AND LibsOk) then
        begin
          if FPCupManager.AutoTools then
          begin
            writeln('Got cross-tools. Retry cross-install.');
            FPCupManager.Sequencer.ResetAllExecuted;
            UpOk:=FPCupManager.Run;
          end
          else
          begin
            if (BinsOK) then writeln('The correct binary tools can be found @ '+BinsURL);
            if (LibsOK) then writeln('The correct libraries can be found @ '+LibsURL);
          end;
        end;
      end;

      if (NOT UpOk) then
      begin
        ShowErrorHints;
        res:=ERROR_FPCUP_BUILD_FAILED;
      end;

    end
    else
    begin
      if (res=ERROR_WRONG_OPTIONS) or (res=FPCUP_GETHELP) then WriteHelp(FPCupManager.ModulePublishedList,FPCupManager.ModuleEnabledList);
      if (res=FPCUP_GETHELP) then res:=OK_IGNORE;
    end;
  finally
    FPCupManager.free;
  end;
  if res<>CHECKOPTIONS_SUCCESS then
    halt(res);
end.
