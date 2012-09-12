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


uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,Forms,Interfaces {these 2 for application options},
  svnclient, updatelazconfig, ftpsend, sysutils, processutils, fileutil,
  fpcuputil, m_crossinstaller, m_crosswin64, m_crosswin32, synacode, synafpc,
  synaip, synautil, synsock, blcksock, installerCore,
  installerfpc, installerLazarus, installerHelp, installerUniversal,
  installerManager,commandline;

//{$R *.res} //Keep it simple, no resources

// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}

procedure WriteVersion;
begin
  writeln('Version: based on commit '+RevisionStr);
  writeln('Build date: '+versiondate);
  writeln('Compiled for CPU: '+{$INCLUDE %FPCTARGETCPU%}+' on '+{$INCLUDE %FPCTARGETOS%});
  writeln('');
end;

procedure WriteHelp(ModuleList,ModuleEnabledList:TStringList);
var i:integer;
begin
  writeln('DON''T PANIC!');
  writeln('Everything below is optional...');
  writeln('');
  writeln('fpcup --<options>');
  writeln('');
  writeln('fpcup can work with modules - see "only", "skip" below');
  writeln('List of all modules:');
  For i:=0 to ModuleList.Count-1 do
    begin
    writeln(ModuleList[i]);
    end;
  writeln('');
  writeln('The following modules run by default:');
  For i:=0 to ModuleEnabledList.Count-1 do
    begin
    writeln(ModuleEnabledList[i]);
    end;
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
  writeln('                       Make sure it is not in the fpcdir directory');
  writeln(' clean                 Remove files created with build. ');
  writeln('                       Can be combined with skip and only options.');
  writeln(' configfile=<filename> Load module definition file from <filename>.');
  writeln('                       Default: fpcup.ini in the program directory.');
  writeln(' cputarget=<name>      CPU target for cross_compiling.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       i386,m68k,alpha,powerpc,powerpc64,');
  writeln('                       armeb,arm,sparc,x86_64,ia64');
  writeln(' fpcbootstrapdir=<dir> An existing FPC compiler is needed to compile the FPC');
  writeln('                       sources. Specify location with this option; if no');
  writeln('                       compiler found here, FPCUp will download one there.');
  writeln('                       Make sure it is not in the fpcdir directory');
  writeln('                       Default: c:\development\fpcbootstrap\');
  writeln('                       or ~\fpcbootstrap\');
  writeln(' fpcdir=<dir>          Target FPC dir, default c:\development\fpc\');
  writeln('                       or ~\fpc\');
  writeln(' fpcuplinkname=<name>  Name of the shortcut to the fpcup script.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: fpcup_update');
  writeln('                         or <lazlinkname>_update if lazlinkname specified');
  writeln(' fpcURL=<URL>          SVN URL from which to download; default: fixes_2.6:');
  writeln('                       http://svn.freepascal.org/svn/fpc/branches/fixes_2_6');
  writeln(' fpcOPT=<options>      Options passed on to the FPC make as OPT=options.');
  writeln('                       E.g.: --fpcOPT="-gl -dSAX_HTML_DEBUG -dUSE_MINGW_GDB"');
  writeln(' fpcrevision=<number>  Revert to FPC SVN revision <number>');
  writeln(' keeplocalchanges      Keep locally modified files (normally these would be');
  writeln('                       backed up as .diff files before doing svn revert.');
  writeln(' installdir=<dir>      Base installation dir. FPC will install in <dir>\fpc\,');
  writeln('                       Lazarus in <dir>\lazarus\, bootstrap compiler in ');
  writeln('                       <dir>\fpcbootstrap\, (Windows only) binutils in ');
  writeln('                       <dir>\fpcbootstrap\, extra modules in <dir>\extras\ ');
  writeln('                       and configuration in <dir>\config\. See fpcdir, lazdir,');
  writeln('                       fpcbootstrapdir, binutilsdir, primary-config-path');
  writeln('                       for the defaults when installdir is not specified.');
  writeln('                       You can also use these to override the defaults given');
  writeln('                       by installdir.');
  writeln(' lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\');
  writeln('                       or ~\lazarus\');
  writeln(' lazlinkname=<name>    Name of the shortcut to the Lazarus install.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: Lazarus_trunk');
  writeln(' lazOPT=<options>      Options passed on to the Lazarus make as OPT=options.');
  writeln(' lazrevision=<number>  Revert to Lazarus SVN revision <number>');
  writeln(' lazURL=<URL>          SVN URL from which to download; default: ');
  writeln('                       trunk (newest version):');
  writeln('                       http://svn.freepascal.org/svn/lazarus/trunk');
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
  writeln(' uninstall             uninstall sources and all generated files');
  writeln('                       If no skip and only options given:');
  writeln('                       DELETE entire Lazarus/FPC directories');
  writeln('                       Else: uninstall only certain modules.');
  writeln(' verbose               Show output from svn and make.');
  writeln(' version               Show version info and quit.');
  writeln('');
  writeln('Share and enjoy!');
  writeln('');
end;

function CheckOptions(FInstaller: TFPCupManager):integer;
var
  {$IFNDEF MSWINDOWS}AllOptions,FPCUpLink:string;{$ENDIF}
  bNoConfirm,bHelp,bVersion:boolean;
  sconfirm:string;
  Options:TCommandLineOptions;
  sInstallDir,s:string;
  bHaveInstalldir:boolean;
begin
  Options:=TCommandLineOptions.create;
  try
  result:=-1; //no error
  Options.CaseSensitive:=false;
  try
  {$IFDEF MSWINDOWS}
  sInstallDir:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','installdir','C:\development')));
  bHaveInstalldir:=sInstallDir<>'C:\development';
  FInstaller.MakeDirectory:=Options.GetOption('','binutilsdir',sInstallDir+'\fpcbootstrap\');
  FInstaller.BootstrapCompilerDirectory:=Options.GetOption('','fpcbootstrapdir',sInstallDir+'\fpcbootstrap\');
  FInstaller.FPCDirectory:=Options.GetOption('','fpcdir',sInstallDir+'\fpc\');
  FInstaller.LazarusDirectory:=Options.GetOption('','lazdir',sInstallDir+'\lazarus\');
  {$ELSE}
  // Use expandfilenameUTF8 to expand ~ to correct directory
  sInstallDir:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','installdir','~/development')));
  bHaveInstalldir:=sInstallDir<>ExpandFileNameUTF8('~/development');
  FInstaller.MakeDirectory:=Options.GetOption('','binutilsdir','');
  FInstaller.BootstrapCompilerDirectory:=Options.GetOption('','fpcbootstrapdir',sInstallDir+'/fpcbootstrap');
  FInstaller.FPCDirectory:=Options.GetOption('','fpcdir',sInstallDir+'/fpc');
  FInstaller.LazarusDirectory:=Options.GetOption('','lazdir',sInstallDir+'/lazarus');
  {$ENDIF MSWINDOWS}
  FInstaller.Clean:=Options.GetOptionNoParam('','clean',false);
  FInstaller.ConfigFile:=Options.GetOption('','configfile',ExtractFilePath(ParamStr(0))+installerUniversal.CONFIGFILENAME);
  FInstaller.CrossCPU_Target:=Options.GetOption('','cputarget','');
  FInstaller.ShortCutNameFpcup:=Options.GetOption('','fpcuplinkname','fpcup_update');
  if (FInstaller.ShortCutNameFpcup='fpcup_update') and bHaveInstalldir then
    FInstaller.ShortCutNameFpcup:=ExtractFileName(sInstallDir)+'_update';  // sInstallDir has no terminating pathdelimiter!!
  FInstaller.FPCOPT:=Options.GetOption('','fpcOPT','');
  FInstaller.FPCDesiredRevision:=Options.GetOption('','fpcrevision','',false);
  //svn2 seems to lag behind a lot, so don't use that.
  FInstaller.FPCURL:=Options.GetOption('','fpcURL','http://svn.freepascal.org/svn/fpc/branches/fixes_2_6');
  bHelp:=Options.GetOptionNoParam('h','help',false);
  FInstaller.KeepLocalChanges:=Options.GetOptionNoParam('','keeplocalchanges');
  FInstaller.ShortCutName:=Options.GetOption('','lazlinkname','Lazarus_trunk');
  if (FInstaller.ShortCutName='Lazarus_trunk') and bHaveInstalldir then
    FInstaller.ShortCutName:='Lazarus_'+ExtractFileName(sInstallDir);  // sInstallDir has no terminating pathdelimiter!!
  FInstaller.LazarusOPT:=Options.GetOption('','lazOPT','');
  FInstaller.LazarusDesiredRevision:=Options.GetOption('','lazrevision','',false);
  //svn2 seems to lag behind a lot, so don't use that.
  FInstaller.LazarusURL:=Options.GetOption('','lazURL','http://svn.freepascal.org/svn/lazarus/trunk');
  FInstaller.CrossLCL_Platform:=Options.GetOption('','lclplatform','');
  FInstaller.SkipModules:=Options.GetOption('','skip','',false);
  FInstaller.OnlyModules:=Options.GetOption('','only','',false);
  FInstaller.CrossOS_Target:=Options.GetOption('','ostarget','');
  s:=Options.GetOption('','primary-config-path','');
  if (s='') and bHaveInstalldir then
    FInstaller.LazarusPrimaryConfigPath:=sInstallDir+DirectorySeparator+'config'+DirectorySeparator
  else
    FInstaller.LazarusPrimaryConfigPath:=s;
  FInstaller.Uninstall:=Options.GetOptionNoParam('','uninstall');
  FInstaller.Verbose:=Options.GetOptionNoParam('','verbose',false);
  bVersion:=Options.GetOptionNoParam('','version',false);
  bNoConfirm:=Options.GetOptionNoParam('','noconfirm');
  except
    on E:Exception do
    begin
    writeln('Error: wrong command line options given:');
    writeln(E.Message);
    WriteHelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList);
    result:=13; //Quit with error resultcode
    end
  end;
  FInstaller.LoadModuleList;

  if Options.ValidateOptions<>'' then
    begin
    writeln('Error: wrong command line options given:');
    writeln(Options.ValidateOptions);
    WriteHelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList);
    result:=13; //Quit with error resultcode
    end
  else if bHelp then
    begin
    writehelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList);
    result:=0; //quit without error
    end
  else if bVersion then
    begin
    //writeversion; //version will be written anyway
    result:=0; //quit without error
    end
  else
    begin
    if ('fpcup_update' = FInstaller.ShortCutNameFpcup) and ('Lazarus_trunk'<>FInstaller.ShortCutName) then
      FInstaller.ShortCutNameFpcup:=FInstaller.ShortCutName+'_Update';

    {$IFNDEF MSWINDOWS}
    if FInstaller.MakeDirectory<>'' then
      begin
      writeln('The "binutilsdir" parameter (currently set to '+FInstaller.MakeDirectory+') is not necessary or supported on this system.'+LineEnding+
        'The parameter will be ignored.');
      FInstaller.MakeDirectory:='';
      end;
    {$ENDIF MSWINDOWS}

    FInstaller.AllOptions:=Options.AllOptions;

    writeln('');
    writeln('Options:');
    if FInstaller.Clean then
    begin
      writeln('Running --clean: cleaning environment.');
    end;
    {$IFDEF MSWINDOWS}
    // Makes no sense on other platforms
    writeln('Binutils/make dir:      '+FInstaller.MakeDirectory);
    {$ENDIF MSWINDOWS}
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
    if FInstaller.KeepLocalChanges then
    begin
      writeln('Keep local changes:     yes');
    end
    else
    begin
      writeln('Keep local changes:     no');
    end;
    writeln('Parameter list:         '+FInstaller.AllOptions);

    // Show warnings to the user:
    writeln('');

    // Note: we don't have a unicode version of ExpandFileName; investigate consequences for Unicode paths!??!?
    // User could have specified relative paths so we're normalizing them.
    if ExpandFileName(FInstaller.LazarusDirectory)=ExpandFileName(FInstaller.FPCDirectory) then
      writeln('WARNING: FPC and Lazarus directories are the same ('+FInstaller.FPCDirectory+'). This will not work!');
    if (FInstaller.FPCDesiredRevision<>'') then
      writeln('WARNING: Reverting FPC to revision '+FInstaller.FPCDesiredRevision);
    if (FInstaller.LazarusDesiredRevision<>'') then
      writeln('WARNING: Reverting Lazarus to revision '+FInstaller.LazarusDesiredRevision);
    if FInstaller.SkipModules<>'' then
      writeln('WARNING: Skipping installation/update of '+FInstaller.SkipModules);
    if FInstaller.OnlyModules<>'' then
      writeln('WARNING: Limiting installation/update to '+FInstaller.OnlyModules);
    writeln('');
    if FInstaller.Uninstall then
      writeln('WARNING: UNINSTALLING !!!')
    else if FInstaller.Clean then
      writeln('WARNING: CLEANING !!!');
    writeln('');

    // Get user confirmation unless otherwise specified
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
  finally
    Options.free;
  end;
end;

procedure ShowErrorHints();
begin
  writeln('Please check log for details. Possible troubleshooting steps:');
  writeln('- make sure there''s a valid SVN executable in your path.');
  {$IFNDEF MSWINDOWS}
  writeln('- make sure the GNU binutils (make etc), windres, subversion client are installed');
  writeln('  e.g. on Debian/Ubuntu: aptitude install build-essential subversion');
  //todo: how to get windres => mingw32-binutils? failing that, debian binutils-mingw-w64 => for both x86 and x64 windows apparently?
  {$ENDIF MSWINDOWS}
  writeln('- try removing all intermediate files by running fpcup with the --clean option');
  writeln('- if that does not work: use the --uninstall option to remove all files (including your FPC and lazarus directories)');
  writeln('- remove the bootstrap compiler. fpcup will download it if required.');
end;

var
  FPCupManager:TFPCupManager;
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
  writeln('');
  writeversion;
  try
    FPCupManager:=TFPCupManager.Create;
    res:=CheckOptions(FPCupManager); //Process command line arguments
    if res=-1 then
      // Get/update/compile selected modules
      if FPCupManager.Run=false then
      begin
        writeln('FPCUp failed.');
        ShowErrorHints;
      end;
  finally
    FPCupManager.free;
  end;
  if res<>-1 then
    halt(res);
end.