{ FPC/Lazarus installer/updater
Copyright (C) 2012 Reinier Olislagers, Ludo Brands

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
{$mode objfpc}{$H+}

{
Possible additional verifications: check existing fpc locations, versions

Command: tfplist or something containing log records with timestamp, sequence description

Add something like fpcup.config in the settings or installed fpc/lazarus dir so we know for which fpc/laz combo this dir is used
}

//todo: check out build-cross.bat in win dir for lazarus for crosscompiling setup instructions

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,Forms,Interfaces {these 2 for application options},
  installer, svnclient, updatelazconfig, ftpsend, sysutils, processutils,
  fpcuputil, m_crossinstaller, m_crosswin64, m_crosswin32, synacode, synafpc, synaip,
  synautil, synsock, blcksock, topologicalsort;

//{$R *.res} //Keep it simple, no resources
procedure WriteHelp;
begin
  writeln('DON''T PANIC!');
  writeln('Everything below is optional...');
  writeln('');
  writeln('fpcup --<options>');
  writeln('');
  writeln('fpcup can work with modules - see "only", "skip" below');
  writeln('List of modules and standard installation action:');
  writeln('Std Name           Description');
  writeln('on  FPC            Free Pascal compiler');
  writeln('on  lazarus        LCL (and IDE if not crosscompiling)');
  writeln('off bigide         IDE with extra components. Selects lazarus.');
  writeln('on  help           Lazarus+FPC help. Also selects bigide.');
  writeln('on  wincrossx64    Lazarus Win32=>Win64 cross compiler.');
  writeln('                   Note: also installs lazarus 32 bit IDE.');
  writeln('on  doceditor      Lazarus Doceditor. Also selects lazarus.');
  writeln('on  lazdatadesktop Data desktop tool. Also selects lazarus.');
  writeln('Because help is installed by default, it pulls in bigide and');
  writeln('the results that all modules above get installed.');
  writeln('(except wincrossx64 if not on Windows, of course)');
  //todo: add crossinstaller module names
  writeln('');
  writeln('Options are not required; they include:');
  writeln(' help                  Show this text');
  writeln(' binutilsdir=<dir>     Windows only:');
  writeln('                       Directory where make, patch etc');
  writeln('                       (the binutils) are located. If make does not');
  writeln('                       exist, binutils will be downloaded there.');
  writeln('                       Default c:\development\fpcbootstrap\');
  writeln('                       Note: the binutils are copied to the');
  writeln('                       FPC directory for use by FPC. This gives');
  writeln('                       a more standard FPC environment.');
  writeln(' clean                 Don''t build.');
  writeln('                       If no skip and only options given:');
  writeln('                       DELETE entire Lazarus/FPC directories');
  writeln('                       Else: clean up only certain modules,');
  writeln('                       only deleting temporary items.');
  writeln(' cputarget=<name>      CPU target for cross_compiling.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       i386,m68k,alpha,powerpc,powerpc64,');
  writeln('                       armeb,arm,sparc,x86_64,ia64');
  writeln(' fpcbootstrapdir=<dir> An existing FPC compiler is needed to compile the FPC');
  writeln('                       sources. Specify location with this option; if no');
  writeln('                       compiler found here, FPCUp will download one there.');
  writeln('                       Default: c:\development\fpcbootstrap\');
  writeln('                       or ~\fpcbootstrap\');
  writeln(' fpcdir=<dir>          Target FPC dir, default c:\development\fpc\');
  writeln('                       or ~\fpc\');
  writeln(' fpcURL=<URL>          SVN URL from which to download; default: fixes_2.6:');
  writeln('                       http://svn.freepascal.org/svn/fpc/branches/fixes_2_6');
  writeln(' fpcOPT=<options>      Options passed on to the FPC make as OPT=options.');
  writeln('                       E.g.: --fpcOPT="-gl -dSAX_HTML_DEBUG -dUSE_MINGW_GDB"');
  writeln(' fpcrevision=<number>  Revert to FPC SVN revision <number>');
  writeln(' fpcuplinkname=<name>  Name of the shortcut to the fpcup script.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: fpcup_update');
  writeln('                         or <lazlinkname>_update if lazlinkname specified');
  writeln(' lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\');
  writeln('                       or ~\lazarus\');
  writeln(' lazlinkname=<name>    Name of the shortcut to the Lazarus install.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: Lazarus_trunk');
  writeln(' lazrevision=<number>  Revert to Lazarus SVN revision <number>');
  writeln(' lazURL=<URL>          SVN URL from which to download; default: ');
  writeln('                       trunk (newest version):');
  writeln('                       http://svn.freepascal.org/svn/lazarus/trunk');
  writeln(' lazOPT=<options>      Options passed on to the Lazarus make as OPT=options.');
  writeln(' lclplatform=<name>    LCL widget set. <name> has to be one of the following:');
  writeln('                       carbon,fpgui,gtk,gtk2,qt,win32,wince');
  writeln(' noconfirm             No confirmation asked. For batch operation.');
  writeln(' only=<values>         Update/build or clean only the modules specified.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' ostarget=<name>       OS target for cross_compiling.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       darwin,freebsd,linux,netbsd,openbsd,os2,');
  writeln('                       solaris,wince,win32,win64');
  writeln(' primary-config-path=<dir>');
  writeln('                       Analogous to Lazarus primary-config-path parameter.');
  writeln('                       Determines where fpcup will create or use as primary');
  writeln('                       configuration path for the Lazarus it installs/updates.');
  writeln('                       Default: empty; then a OS dependent configuration');
  writeln('                       path is used; directory name lazarusdevsettings.');
  writeln(' skip=<values>         Do not update/build or clean modules.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' verbose               Show output from svn and make');
  writeln('');
  writeln('Share and enjoy!');
  writeln('');
end;

function CheckOptions(FInstaller: TInstaller):integer;
const
  //Parameter names:
  BinutilsDir='binutilsdir';
  Clean='clean';
  CPUTarget='cputarget';
  FPCBootstrapDir='fpcbootstrapdir';
  FPCDir='fpcdir';
  FPCURL='fpcURL';
  FPCOPT='fpcOPT';
  Help='help';
  LazDir='lazdir';
  LazLinkName='lazlinkname';
  FpcupLinkName='fpcuplinkname';
  LazURL='lazURL';
  LazOPT='lazOPT';
  LCLPlatform='lclplatform';
  LazRevision='lazrevision';
  FPCRevision='fpcrevision';
  OSTarget='ostarget';
  PrimaryConfigPath='primary-config-path';
  Skip='skip';
  Only='only';
  NoConfirm='noconfirm';
  Verbose='verbose';
var
  ErrorMessage: string;
  AllOptions,FPCUpLink:string;
  bNoConfirm:boolean;
  sconfirm:string;
begin
  result:=-1; //no error
  // Default values
  FInstaller.ShortCutName:='Lazarus_trunk';
  FInstaller.ShortCutNameFpcup:='fpcup_update';
  FInstaller.FPCURL := 'http://svn.freepascal.org/svn/fpc/branches/fixes_2_6';
  FInstaller.FPCOPT:='';
  FInstaller.LazarusPrimaryConfigPath:=''; //Let installer figure out default value
  FInstaller.LazarusURL := 'http://svn.freepascal.org/svn/lazarus/trunk';
  //svn2 seems to lag behind a lot, so don't use that.
  FInstaller.LazarusOPT:='';
  {$IFDEF MSWINDOWS}
  FInstaller.BootstrapCompilerDirectory := 'c:\development\fpcbootstrap\';
  FInstaller.FPCDirectory := 'c:\development\fpc';
  FInstaller.LazarusDirectory := 'c:\development\lazarus';
  FInstaller.MakeDirectory := 'C:\development\fpcbootstrap\';
  {$ENDIF MSWINDOWS}
  {$IFNDEF MSWINDOWS}
  FInstaller.BootstrapCompilerDirectory := '~/fpcbootstrap';
  FInstaller.FPCDirectory := '~/fpc';
  FInstaller.LazarusDirectory := '~/lazarus';
  FInstaller.MakeDirectory:='';
  {$ENDIF MSWINDOWS}

  Application.CaseSensitiveOptions:=false; //Our Windows users will like this.
  ErrorMessage := Application.CheckOptions(
    'h', Binutilsdir+': '+Clean+' '+FPCBootstrapDir+': '+FPCDir+': '+FPCURL+': '+FPCOPT+': '+
    Help+' '+LazDir+': '+LazOPT+': '+ LazRevision+': '+FPCRevision+': '+
    Skip+': '+Only+': '+NoConfirm+' '+ Verbose+' '+
    CPUTarget+': '+LCLPlatform+': '+OSTarget+': '+
    LazLinkName+': '+FpcupLinkName+': '+LazURL+': '+PrimaryConfigPath+': ');
  // todo: check for parameters given without --
  // these might be typos and should result in halting as well.
  if Length(ErrorMessage) > 0 then
  begin
    writeln('Error: wrong command line options given:');
    writeln(ErrorMessage);
    WriteHelp;
    result:=13; //Quit with error resultcode
    exit;
  end;

  AllOptions:='';

  if Application.HasOption(BinutilsDir) then
  begin
    FInstaller.MakeDirectory:=Application.GetOptionValue(BinutilsDir);
    AllOptions:=AllOptions+BinutilsDir+'="'+FInstaller.MakeDirectory+'" ';
    {$IFNDEF MSWINDOWS}
    writeln('The '+BinutilsDir+' parameter is not necessary or supported on this system.');
    writeln('The parameter will be ignored.');
    FInstaller.MakeDirectory:='';
    {$ENDIF MSWINDOWS}
  end;

  if Application.HasOption(Clean) then
  begin
    FInstaller.Clean:=true;
  end;

  if Application.HasOption(CPUTarget) then
  begin
    FInstaller.CrossCPU_Target:=Application.GetOptionValue(CPUTarget);
    AllOptions:=AllOptions+'--'+CPUTarget+'='+FInstaller.CrossCPU_Target;
  end;

  if Application.HasOption(FPCBootstrapDir) then
  begin
    FInstaller.BootstrapCompilerDirectory:=Application.GetOptionValue(FPCBootstrapDir);
    AllOptions:=AllOptions+'--'+FPCBootstrapDir+'="'+FInstaller.BootstrapCompilerDirectory+'" ';
  end;

  if Application.HasOption(FPCDir) then
  begin
    FInstaller.FPCDirectory:=Application.GetOptionValue(FPCDir);
    AllOptions:=AllOptions+'--'+FPCDir+'="'+FInstaller.FPCDirectory+'" ';
  end;

  if Application.HasOption(FPCOPT) then
  begin
    FInstaller.FPCOPT:=Application.GetOptionValue(FPCOPT);
    AllOptions:=AllOptions+'--'+FPCOPT+'="'+FInstaller.FPCOPT+'" ';
  end;

  if Application.HasOption(FPCRevision) then
  begin
    FInstaller.FPCDesiredRevision:=Application.GetOptionValue(FPCRevision);
    //don't store this in alloptions !!!
  end;

  if Application.HasOption(FPCURL) then
  begin
    FInstaller.FPCURL:=Application.GetOptionValue(FPCURL);
    AllOptions:=AllOptions+'--'+FPCURL+'="'+FInstaller.FPCURL+'" ';
  end;

  if Application.HasOption('h', Help) then
  begin
    writehelp;
    result:=0; //quit without error
    exit;
  end;

  if Application.HasOption(LazDir) then
  begin
    FInstaller.LazarusDirectory:=Application.GetOptionValue(LazDir);
    AllOptions:=AllOptions+'--'+LazDir+'="'+FInstaller.LazarusDirectory+'" ';
  end;

  if Application.HasOption(LazLinkName) then
  begin
    FInstaller.ShortCutName:=Application.GetOptionValue(LazLinkName);
    AllOptions:=AllOptions+'--'+LazLinkName+'="'+FInstaller.ShortCutName+'" ';
  end;

  if Application.HasOption(LazOPT) then
  begin
    FInstaller.LazarusOPT:=Application.GetOptionValue(LazOPT);
    AllOptions:=AllOptions+'--'+LazOPT+'="'+FInstaller.LazarusOPT+'" ';
  end;

  if Application.HasOption(LazRevision) then
  begin
    FInstaller.LazarusDesiredRevision:=Application.GetOptionValue(LazRevision);
    //don't store this in alloptions !!!
  end;

  if Application.HasOption(LazURL) then
  begin
    FInstaller.LazarusURL:=Application.GetOptionValue(LazURL);
    AllOptions:=AllOptions+'--'+LazURL+'="'+FInstaller.LazarusURL+'" ';
  end;

  if Application.HasOption(LCLPlatform) then
  begin
    FInstaller.CrossLCL_Platform:=Application.GetOptionValue(LCLPlatform);
    AllOptions:=AllOptions+'--'+LCLPlatform+'='+FInstaller.CrossLCL_Platform;
  end;

  if Application.HasOption(Skip) then
  begin
    //todo: let installer.pas show list of modules at compile time. Select from these for
    // skip and only options
    FInstaller.SkipModules:=Application.GetOptionValue(Skip);
  end;

  if Application.HasOption(Only) then
  begin
    FInstaller.OnlyModules:=Application.GetOptionValue(Only);
  end;

  if Application.HasOption(OSTarget) then
  begin
    FInstaller.CrossOS_Target:=Application.GetOptionValue(OSTarget);
    AllOptions:=AllOptions+'--'+OSTarget+'='+FInstaller.CrossOS_Target;
  end;

  if Application.HasOption(PrimaryConfigPath) then
  begin
    // Only change if there's actually a valid value
    if (Application.GetOptionValue(PrimaryConfigPath)<>'') then
    begin
      FInstaller.LazarusPrimaryConfigPath:=Application.GetOptionValue(PrimaryConfigPath);
    end;
    AllOptions:=AllOptions+'--'+PrimaryConfigPath+'="'+Application.GetOptionValue(PrimaryConfigPath)+'" ';
  end;

  FInstaller.Verbose:=Application.HasOption(Verbose);
  bNoConfirm:=Application.HasOption(NoConfirm);

  // FpcupLinkName has to be the last since here we store AllOptions !!
  // AllOptions is rebuilt in this clumsy way because we lost the quotes in paramstr()
  // and need them for option sequences, weird paths, etc.

  if Application.HasOption(FpcupLinkName) then
  begin
    FpcupLink:=Application.GetOptionValue(FPCUpLinkName);
    FInstaller.ShortCutNameFpcup:=FPCUpLink;
    AllOptions:=AllOptions+'--'+FpcupLinkName+'="'+FPCUpLink+'" ';
  end
  else
  if Application.HasOption(LazLinkName) and (FInstaller.ShortCutName<>'') then
    FInstaller.ShortCutNameFpcup:=FInstaller.ShortCutName+'_Update';

  FInstaller.AllOptions:=AllOptions;

  writeln('');
  writeln('Options:');
  if FInstaller.Clean then
  begin
    writeln('Running --clean: cleaning environment.');
  end;
  writeln('Bootstrap compiler dir: '+FInstaller.BootstrapCompilerDirectory);
  writeln('Lazarus shortcut name:  '+FInstaller.ShortCutName);
  writeln('Shortcut fpcup name:    '+FInstaller.ShortCutNameFpcup);
  writeln('FPC URL:                '+FInstaller.FPCURL);
  writeln('FPC options:            '+FInstaller.FPCOPT);
  writeln('FPC directory:          '+FInstaller.FPCDirectory);
  writeln('Lazarus directory:      '+FInstaller.LazarusDirectory);
  writeln('Lazarus primary config path:');
  writeln('(Lazarus settings path) '+FInstaller.LazarusPrimaryConfigPath);
  writeln('Lazarus URL:            '+FInstaller.LazarusURL);
  writeln('Lazarus options:        '+FInstaller.LazarusOPT);
  writeln('Parameter list:         '+FInstaller.AllOptions);
  {$IFDEF MSWINDOWS}
  writeln('Make/binutils path:     '+FInstaller.MakeDirectory);
  {$ENDIF MSWINDOWS}
  writeln('');
  if (FInstaller.FPCDesiredRevision<>'') then
    writeln('WARNING: Reverting FPC to revision '+FInstaller.FPCDesiredRevision);
  if (FInstaller.LazarusDesiredRevision<>'') then
    writeln('WARNING: Reverting Lazarus to revision '+FInstaller.LazarusDesiredRevision);
  if FInstaller.SkipModules<>'' then
    writeln('WARNING: Skipping installation/update of '+FInstaller.SkipModules);
  if FInstaller.OnlyModules<>'' then
    writeln('WARNING: Limiting installation/update to '+FInstaller.OnlyModules);
  writeln('');
  if not bNoConfirm then
    begin
    write('Continue (Y/n): ');
    readln(sconfirm);
    if uppercase(copy(sconfirm,1,1))='N' then
      begin
      result:=0; //quit without error
      end;
    end;
end;

procedure ShowErrorHints();
begin
  writeln('Please check program output for details. Possible troubleshooting steps:');
  writeln('- make sure there''s a valid SVN executable in your path.');
  {$IFNDEF MSWINDOWS}
  writeln('- make sure the GNU binutils (make etc), windres, subversion client are installed');
  writeln('  e.g. on Debian/Ubuntu: aptitude install build-essential subversion');
  //todo: how to get windres => mingw32-binutils? failing that, debian binutils-mingw-w64 => for both x86 and x64 windows apparently?
  {$ENDIF MSWINDOWS}
  writeln('- try removing all local changes in your SVN repository by running fpcup with the --clean option');
  writeln('- remove the generated fpc.cfg in the installed FPC directory. fpcup will recreate it with default settings on the next run.');
end;

var
  FInstaller: TInstaller;
  res:integer;
begin
  writeln('fpcup');
  writeln('An FPC/Lazarus downloader/updater/installer');
  writeln('Open source freeware (modified LGPL/BSD), see:');
  writeln('https://bitbucket.org/reiniero/fpcup');
  writeln('');
  writeln('This program will download the FPC and Lazarus sources');
  writeln('from the source Subversion/SVN repositories,');
  writeln('compile, and install.');
  writeln('Result: you get a fresh, up-to-date Lazarus/FPC installation.');
  writeln('fpcup compiled on '+{$INCLUDE %DATE%}+' '+{$INCLUDE %TIME%}+' with FPC '+{$INCLUDE %FPCVERSION%});
  writeln('for CPU: '+{$INCLUDE %FPCTARGETCPU%}+' on '+{$INCLUDE %FPCTARGETOS%});

  try
    FInstaller := TInstaller.Create;
    res:=CheckOptions(FInstaller); //Process command line arguments
    if res=-1 then
      // Get/update/compile selected modules
      if FInstaller.Run=false then
      begin
        writeln('fpcup failed.');
        ShowErrorHints;
      end;
  finally
    FInstaller.Free;
  end;
  writeln('FPCUp finished.');
  if res<>-1 then
    halt(res);
end.
