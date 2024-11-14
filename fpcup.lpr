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
(*
  The Initial Developer of the FPCUPdeluxe code is:
  Alfred Gl�nzer (donalfredo, aog)

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
  installerBase,
  installerCore,
  installerUniversal,
  checkoptions, fpcuputil,
  {$endif}
  {$ifdef LCL}
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

{$i revision.inc}

procedure WriteVersion;
begin
  writeln('Version: based on commit fpcup'+RevisionStr+' ('+versiondate+')');
  writeln('Build date: '+{$INCLUDE %DATE%}+' '+{$INCLUDE %TIME%});
  writeln('Compiled for CPU: '+lowercase({$INCLUDE %FPCTARGETCPU%})+' on '+lowercase({$INCLUDE %FPCTARGETOS%}));
  writeln('');
  {$IFDEF DEBUG}
  writeln('*** DEBUG BUILD ***');
  writeln('');
  {$ENDIF}
end;

procedure WriteHelp(ModuleList,ModuleEnabledList:TStringList);
var
  i:integer;
  SortedModules: TStringList;
begin
  writeln('DON''T PANIC!');
  writeln('Everything below is optional...');
  writeln('');
  writeln('fpcup can work with modules - see "only", "include", "skip" below');
  writeln('List of all modules:');
  SortedModules:=TStringList.Create;
  try
    SortedModules.Sorted:=true;
    SortedModules.AddStrings(ModuleList);
    for i:=0 to SortedModules.Count-1 do
      begin
      writeln(SortedModules[i]);
      end;
  finally
    SortedModules.Free;
  end;
  writeln('');
  writeln('The following modules run by default:');
  For i:=0 to ModuleEnabledList.Count-1 do
    begin
    writeln(ModuleEnabledList[i]);
    end;
  writeln('');
  writeln('fpcup --<option> --<option>...');
  writeln('');
  writeln('Options are not required; they include:');
  writeln(' help                  Show this text');
  writeln('');
  writeln(' fpcURL=<URL>          Repo URL from which to download');
  writeln('                       Accepts shortcuts: ');
  writeln('                       '+installerUniversal.GetAlias('fpcURL','list')+',gitlab');
  {$ifndef FPCONLY}
  writeln(' lazURL=<URL>          Repo URL from which to download');
  writeln('                       Accepts shortcuts: ');
  writeln('                       '+installerUniversal.GetAlias('lazURL','list')+',gitlab');
  {$endif}
  writeln(' fpcVersion=<version>  FPC version to use; default: stable');
  writeln('                       Accepts shortcuts: ');
  writeln('                       '+installerUniversal.GetAlias('fpcBRANCH','list'));
  writeln('                       '+installerUniversal.GetAlias('fpcTAG','list'));
  {$ifndef FPCONLY}
  writeln(' lazVersion=<version>  Lazarus version to use; default: stable');
  writeln('                       Accepts shortcuts: ');
  writeln('                       '+installerUniversal.GetAlias('lazBRANCH','list'));
  writeln('                       '+installerUniversal.GetAlias('lazTAG','list'));
  {$endif}
  writeln(' fpcBranch=<name>      Get desired FPC branch/tag <name>');
  {$ifndef FPCONLY}
  writeln(' lazBranch=<name>      Get desired Lazarus branch/tag <name>');
  {$endif}
  writeln(' fpcRevision=<name>    Get desired FPC revision/hash <name>');
  {$ifndef FPCONLY}
  writeln(' lazRevision=<name>    Get desired Lazarus revision/hash <name>');
  {$endif}
  writeln(' cputarget=<name>      CPU target for cross_compiling.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       i386,m68k,alpha,powerpc,powerpc64,');
  writeln('                       armeb,arm,sparc,x86_64,ia64');
  writeln(' ostarget=<name>       OS target for cross-compiling.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       darwin,freebsd,linux,netbsd,openbsd,os2,');
  writeln('                       solaris,wince,win32,win64');
  writeln(' subarch=<name>        Subarch target for cross-compiling embedded target.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       armv7m (for Cortex M3),armv7em (for Teensy),armv4,');
  writeln(' fpcOPT=<options>      Options passed on to the FPC make as OPT=options.');
  writeln('                       E.g.: --fpcOPT="-gl -dSAX_HTML_DEBUG -dUSE_MINGW_GDB"');
  writeln(' crossOPT=<options>    Options to be passed to the cross compiler.');
  writeln('                       Corresponds to the CROSSOPT argument in make');
  writeln('                       crosscompiler.');
  writeln('                       E.g. --crossOPT="-CpARMV7 -CfVFPV3" for ARM');
  writeln(' crossbindir=<dir>     Directory where crosscompile toolchain can be found.');
  writeln('                       If target is non-win, fpcup will look for as.');
  writeln('                       If not defined, fpcup tries to find the corect chain.');
  writeln(' crosslibdir=<dir>     Directory where crosscompile libraries can be found.');
  writeln('                       If target is non-win, fpcup will look for libc.so.');
  writeln('                       If not defined, fpcup tries to find the correct library.');
  {$ifndef FPCONLY}
  writeln(' lazOPT=<options>      Options passed on to the Lazarus make as OPT=options.');
  writeln(' lclplatform=<name>    LCL widget set. <name> has to be one of the following:');
  writeln('                       carbon,fpgui,gtk,gtk2,qt,win32,wince');
  {$endif}
  writeln(' installdir=<dir>      Base installation dir. Leads to these subdirs:');
  {$ifndef FPCONLY}
  writeln('                       <dir>\config_lazarus\ Lazarus primary config path');
  {$endif}
  writeln('                       <dir>\cross\          crosscompiling bins/libs');
  writeln('                       <dir>\extras\         extra modules');
  writeln('                       <dir>\fpc\            FPC');
  writeln('                       <dir>\fpcbootstrap\   (Windows) bootstrap compiler+utils');
  {$ifndef FPCONLY}
  writeln('                       <dir>\installerlazwin (Windows) generated installer if');
  writeln('                                             using module installerlazwin');
  writeln('                       <dir>\lazarus\        Lazarus');
  writeln('                       See fpcdir, lazdir, fpcbootstrapdir, binutilsdir');
  writeln('                       primary-config-path');
  {$else}
  writeln('                       See fpcdir, fpcbootstrapdir, binutilsdir');
  {$endif}
  writeln('                       for the defaults when installdir is not specified.');
  writeln('                       You can also use these to override the defaults given');
  writeln('                       by installdir.');
  writeln(' fpcdir=<dir>          Target FPC dir, default c:\development\fpc\');
  writeln('                       or ~\fpc\');
  {$ifndef FPCONLY}
  writeln(' lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\');
  writeln('                       or ~\lazarus\');
  {$endif}
  writeln(' fpcbootstrapdir=<dir> An existing FPC compiler is needed to compile the FPC');
  writeln('                       sources. Specify location with this option; if no');
  writeln('                       compiler found here, FPCUp will download one there.');
  writeln('                       Make sure it is not in the fpcdir directory');
  writeln('                       Default: c:\development\fpcbootstrap\');
  writeln('                       or ~\fpcbootstrap\');
  writeln(' binutilsdir=<dir>     Windows only:');
  writeln('                       Directory where make, patch etc');
  writeln('                       (the binutils) are located. If make does not');
  writeln('                       exist, binutils will be downloaded there.');
  writeln('                       Default c:\development\fpcbootstrap\');
  writeln('                       Note: the binutils are copied to the');
  writeln('                       FPC directory for use by FPC. This gives');
  writeln('                       a more standard FPC environment.');
  writeln('                       Make sure it is not in the fpcdir directory');
  {$ifndef FPCONLY}
  writeln(' primary-config-path=<dir>');
  writeln('                       Analogous to Lazarus primary-config-path (pcp) parameter.');
  writeln('                       Determines where fpcup will create or use as primary');
  writeln('                       configuration path for the Lazarus it installs/updates.');
  writeln('                       Default: empty (=an OS dependent configuration');
  writeln('                       path is used). However, if installdir is specified,');
  writeln('                       the pcp path will be below it.');
  {$endif}
  writeln(' httpproxy=<username:password@host:port> username, password: optional');
  writeln(' httpproxy=<http://username:password@host:port> username, password: optional');
  writeln('                       Use HTTP proxy for http downloads,');
  writeln('                       svn over http, hg over http (but not git over http)');
  writeln('                       On Unix/Linux: if the http_proxy environment variable');
  writeln('                       is set, this option is automatically filled in.');
  writeln(' moduleconfig=<file>   Load external module definition file from <file>.');
  writeln('                       Default: '+CONFIGFILENAME+' in the program directory.');
  writeln(' inifile=<file>        Reads in ini file with options.');
  writeln('                       Example ini file: see '+SETTTINGSFILENAME);
  writeln('                       Options can be overwritten by command line parameters.');
  writeln(' inisection=<sec>      Section name to be used if an ini file is specified.');
  writeln('                       If not given, use [General]');
  writeln(' onlyupbootstrappers   Only try to download bootstrappers from up itself');
  writeln('                       Usefull in case the FPC ftp server is down.');
  writeln(' keeplocalchanges      Keep locally modified files (normally these would be');
  writeln('                       backed up as .diff files before doing repo revert.');
  writeln(' reapplylocalchanges   Back up locally modified files into .diff file and');
  writeln('                       reapply the diff with patch or command specified in ');
  writeln('                       parameter patchcmd.');
  writeln(' fpcuplinkname=<name>  Name of the shortcut to the fpcup script.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: fpcup_update');
  writeln('                         or <lazlinkname>_update if lazlinkname specified');
  {$ifndef FPCONLY}
  writeln(' lazlinkname=<name>    Name of the shortcut to the Lazarus install.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: depends on Lazarus directory');
  {$endif}
  writeln(' include=<values>      Update/build or clean the modules specified as well ');
  writeln('                       as the default ones.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' only=<values>         Update/build or clean only the modules specified.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' skip=<values>         Do not update/build or clean specified modules.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' fpcPATCH=<values>     Patch the FPC source with the specified patches.');
  writeln('                       The patch list is separated by commas.');
  writeln('                       E.g. --patch="fpcrtti.patch".');
  {$ifndef FPCONLY}
  writeln(' lazPATCH=<values>     Patch the Lazarus source with the specified patches.');
  writeln('                       The patch list is separated by commas.');
  writeln('                       E.g. --patch="docmanager.patch".');
  {$endif}
  writeln(' clean                 Remove files created with build. ');
  writeln('                       Can be combined with skip and only options.');
  writeln(' patchcmd              Command to use to reapply local changes backed up with');
  writeln('                       repo diff command. The diff file is passed as the only');
  writeln('                       parameter. Add any extra paremeters needed.');
  writeln('                       Default: "patch" ');
  writeln(' uninstall             Uninstall sources and all generated files');
  writeln('                       If no skip/only options given:');
  {$ifndef FPCONLY}
  writeln('                       DELETE entire Lazarus/FPC directories');
  {$else}
  writeln('                       DELETE entire FPC directories');
  {$endif}
  writeln('                       Else: uninstall only certain modules.');
  writeln(' logfilename=<file>    Location of log file. If nothing specified,');
  writeln('                       fpcup.log in the current directory.');
  writeln(' getfullrepo           Get full repositories (enabled by default, so a dummy).');
  writeln('                       Depreciated, still here for backward compatibility');
  writeln(' getfilesonly          Just get files. No repos. Uses "repo export"');
  writeln(' disablejobs           Do not use (parallel) jobs when building FPC.');
  writeln(' usewget               Use wget on Linux as downloader.');
  writeln(' includehelp           Include help. Will be time and space consuming.');
  writeln(' fpcsplit              Use different directory for source and binaries.');
  writeln(' autotools             Automagic download of cross-libs and cross-bins.');
  writeln(' noconfirm             No confirmation asked. For batch operation.');
  writeln(' verbose               Show output from all commands.');
  writeln(' version               Show version info and quit.');
  writeln('');
  writeln('Share and enjoy!');
  writeln('');
end;

procedure ShowErrorHints();
begin
  writeln('Please check log for details. Possible troubleshooting steps:');
  writeln('First action :');
  writeln('Run fpcup again with --verbose and check for make, lazbuild errors etc.');
  writeln('Run fpcup again also in case of incomple download due to repo errors.');
  writeln;
  {$IFNDEF MSWINDOWS}
  writeln('- make sure there''s a valid git executable in your path.');
  writeln('- make sure the GNU binutils (make etc), windres, subversion client are installed');
  writeln('  e.g. on Debian/Ubuntu: aptitude install build-essential mingw32-binutils subversion ');
  writeln('  ln -s /usr/bin/i586-mingw32msvc-windres /usr/bin/windres');
  writeln('  see http://wiki.lazarus.freepascal.org/Lazarus_Resources#Checking_you_have_windres');
  {$ENDIF MSWINDOWS}
  writeln('If that does not work ... last resort action :');
  writeln('Remove all files from install-directory, and try again');
end;

var
  FPCupManager:TFPCupManager;
  res:integer;
  BinsName,LibsName:string;
  BinsURL,LibsURL:string;
  BinsPath,LibsPath:string;
  ToolTargetFile,ToolTargetPath:string;
  UpOk,BinsOk,LibsOk:boolean;

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
          BinsOk:=(NOT (ieBins in FPCupManager.InstallerErrors));
          LibsOk:=(NOT (ieLibs in FPCupManager.InstallerErrors));
          FPCupManager.GetCrossToolsFileName({%H-}BinsName,{%H-}LibsName);
          FPCupManager.GetCrossToolsPath({%H-}BinsPath,{%H-}LibsPath);
          if (NOT BinsOk) then
          begin
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
                ToolTargetPath:=ConcatPaths([FPCupManager.BaseDirectory,BinsPath])+DirectorySeparator;
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
            end;
          end;
          if (NOT LibsOk) then
          begin
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
        end;

        if (BinsOK AND LibsOk) then
        begin
          if FPCupManager.AutoTools then
          begin
            writeln('Got cross-tools. Retry cross-install.');
            if Assigned(FPCupManager.Sequencer) then FPCupManager.Sequencer.ResetAllExecuted;
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
