{ FPC/Lazarus installer/updater
Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands

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

{
Possible additional verifications: check existing fpc locations, versions

Command: tfplist or something containing log records with timestamp, sequence description

Add something like fpcup.config in the settings or installed fpc/lazarus dir so we know for which fpc/laz combo this dir is used
}


uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes, svnclient, updatelazconfig, ftpsend, sysutils, processutils,
  fileutil, fpcuputil, m_crossinstaller, m_crosswin64, m_crosswin32,
  m_freebsd_to_linux386, m_freebsd64_to_freebsd32, m_freebsd_to_linux64,
  m_anyinternallinker_to_win386, synacode, synafpc, synaip, synautil, synsock,
  blcksock, installerCore, installerfpc, installerLazarus, installerHelp,
  installerUniversal, installerManager, wininstaller, commandline, hgclient,
  repoclient, gitclient;


//{$R *.res} //Keep it simple, no resources

// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}

procedure WriteVersion;
begin
  writeln('Version: based on commit '+RevisionStr+' ('+versiondate+')');
  writeln('Build date: '+{$INCLUDE %DATE%}+' '+{$INCLUDE %TIME%});
  writeln('Compiled for CPU: '+lowercase({$INCLUDE %FPCTARGETCPU%})+' on '+lowercase({$INCLUDE %FPCTARGETOS%}));
  writeln('');
end;

procedure WriteHelp(ModuleList,ModuleEnabledList:TStringList;ConfigFile:string);
var
  i:integer;
  SortedModules: TStringList;
begin
  writeln('DON''T PANIC!');
  writeln('Everything below is optional...');
  writeln('');
  writeln('fpcup --<options>');
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
  writeln('Options are not required; they include:');
  writeln(' help                  Show this text');
  writeln('');
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
  writeln('                       svn://svn.freepascal.org/svn/fpc/branches/fixes_2_6');
  writeln('                       Accepts shortcuts: '+installerUniversal.GetAlias(ConfigFile,'fpcURL','list'));
  writeln(' fpcOPT=<options>      Options passed on to the FPC make as OPT=options.');
  writeln('                       E.g.: --fpcOPT="-gl -dSAX_HTML_DEBUG -dUSE_MINGW_GDB"');
  writeln(' fpcrevision=<number>  Revert to FPC SVN revision <number>');
  writeln(' include=<values>      Update/build or clean the modules specified as well ');
  writeln('                       as the default ones.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' inifile=<file>        Reads in ini file with options.');
  writeln('                       Example ini file: see settings.ini');
  writeln('                       Options can be overwritten by command line parameters.');
  writeln(' inisection=<sec>      Section name to be used if an ini file is specified.');
  writeln('                       If not given, use [General]');
  writeln(' installdir=<dir>      Base installation dir. FPC will install in <dir>\fpc\,');
  writeln('                       Lazarus in <dir>\lazarus\, bootstrap compiler in ');
  writeln('                       <dir>\fpcbootstrap\, (Windows only) binutils in ');
  writeln('                       <dir>\fpcbootstrap\, extra modules in <dir>\extras\ ');
  writeln('                       and configuration in <dir>\config\. See fpcdir, lazdir,');
  writeln('                       fpcbootstrapdir, binutilsdir, primary-config-path');
  writeln('                       for the defaults when installdir is not specified.');
  writeln('                       You can also use these to override the defaults given');
  writeln('                       by installdir.');
  writeln(' keeplocalchanges      Keep locally modified files (normally these would be');
  writeln('                       backed up as .diff files before doing svn revert.');
  writeln(' moduleconfig=<file>   Load external module definition file from <file>.');
  writeln('                       Default: fpcup.ini in the program directory.');
  writeln(' lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\');
  writeln('                       or ~\lazarus\');
  writeln(' lazlinkname=<name>    Name of the shortcut to the Lazarus install.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: depends on Lazarus directory');
  writeln(' lazOPT=<options>      Options passed on to the Lazarus make as OPT=options.');
  writeln(' lazrevision=<number>  Revert to Lazarus SVN revision <number>');
  writeln(' lazURL=<URL>          SVN URL from which to download; default: ');
  writeln('                       trunk (newest version):');
  writeln('                       svn://svn.freepascal.org/svn/lazarus/trunk');
  writeln('                       Accepts shortcuts: '+installerUniversal.GetAlias(ConfigFile,'lazURL','list'));
  writeln(' lclplatform=<name>    LCL widget set. <name> has to be one of the following:');
  writeln('                       carbon,fpgui,gtk,gtk2,qt,win32,wince');
  writeln(' logfilename=<file>    Location of log file. If nothing specified,');
  writeln('                       fpcup.log in the current directory.');
  writeln(' noconfirm             No confirmation asked. For batch operation.');
  writeln(' only=<values>         Update/build or clean only the modules specified.');
  writeln('                       The module list is separated by commas.');
  writeln('                       See above for a list of modules.');
  writeln(' ostarget=<name>       OS target for cross_compiling.');
  writeln('                       <name> has to be one of the following:');
  writeln('                       darwin,freebsd,linux,netbsd,openbsd,os2,');
  writeln('                       solaris,wince,win32,win64');
  writeln(' patchcmd              Command to use to reapply local changes backed up with');
  writeln('                       svn diff command. The diff file is passed as the only');
  writeln('                       parameter. Add any extra paremeters needed.');
  writeln('                       Default: "patch -p0 -i" ');
  writeln(' primary-config-path=<dir>');
  writeln('                       Analogous to Lazarus primary-config-path parameter.');
  writeln('                       Determines where fpcup will create or use as primary');
  writeln('                       configuration path for the Lazarus it installs/updates.');
  writeln('                       Default: empty; then a OS dependent configuration');
  writeln('                       path is used; directory name lazarusdevsettings.');
  writeln(' reapplylocalchanges   Back up locally modified files into .diff file and');
  writeln('                       reapply the diff with patch or command specified in ');
  writeln('                       parameter patchexe');
  writeln(' skip=<values>         Do not update/build or clean specified modules.');
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
  i, iCurrentOption: integer;
  sConfirm:string;
  Options:TCommandLineOptions;
  sIniFile: string;
  sInstallDir,s: string; // Root installation directory
  bHaveInstalldir: boolean; //Has user specified a non-standard install dir?
  sLogFile: string; //Filename for log
  LeftOverOptions: TStringList; //Options left over after processing; may contain module=0 options
begin
  // First check for settings.ini; it might not be present but specified anyway.
  // In any case, we need to extract it from the resource sometime unless we
  // want to create an installer for each platform.
  if not FileExistsUTF8(ExtractFilePath(ParamStr(0))+'settings.ini') then
    SaveInisFromResource('settings.ini','settings_ini');

  Options:=TCommandLineOptions.Create;
  try
    result:=-1; //no error
    try
      sIniFile:=Options.GetOption('','inifile','');
      if sIniFile<>'' then
      begin
        Options.IniFileSection:=Options.GetOption('','inisection','General');
        Options.CaseSensitive:=false; //easier when dealing with ini files
        // Setting this property loads the file:
        try
          Options.IniFile:=sIniFile;
        except
          on E:ECommandLineError do
          begin
            // Empty file, invalid section name etc
            Options.IniFile:='';
            infoln('Specified ini file '+sIniFile+' cannot be read or does not have section '+Options.IniFileSection+'. Aborting.',etError);
            halt(3);
          end;
          on F:Exception do
          begin
            infoln('Error reading specified ini file '+sIniFile+'. Exception: '+F.Message+'. Aborting!',etError);
            halt(3);
          end;
        end;

      end;

      {$IFDEF MSWINDOWS}
      // All directories specified: absolute paths without trailing delimiter
      sInstallDir:=Options.GetOption('','installdir','');
      if sInstallDir='' then begin
        sInstallDir:='C:\development';
        bHaveInstalldir:=false;
      end
      else begin
        sInstallDir:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FInstaller.MakeDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','binutilsdir',sInstallDir+'\fpcbootstrap')));
      FInstaller.BootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','fpcbootstrapdir',sInstallDir+'\fpcbootstrap')));
      FInstaller.FPCDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','fpcdir',sInstallDir+'\fpc')));
      FInstaller.LazarusDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','lazdir',sInstallDir+'\lazarus')));
      {$ELSE} //*nix
      //todo: don't expand home dirs here, do it if possible after we've saved the options to the shortcut so it can be used with other users as well
      sInstallDir:=Options.GetOption('','installdir','');
      if sInstallDir='' then begin
        sInstallDir:=ExpandFileNameUTF8('~/development');
        bHaveInstalldir:=false;
      end
      else begin
        sInstallDir:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FInstaller.MakeDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','binutilsdir','')));
      FInstaller.BootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','fpcbootstrapdir',sInstallDir+'/fpcbootstrap')));
      FInstaller.FPCDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','fpcdir',sInstallDir+'/fpc')));
      FInstaller.LazarusDirectory:=ExcludeTrailingPathDelimiter(ExpandFileNameUTF8(Options.GetOption('','lazdir',sInstallDir+'/lazarus')));
      {$ENDIF MSWINDOWS}

      sLogFile:=Options.GetOption('','logfilename','',true);
      if sLogFile='' then
        {$IFDEF MSWINDOWS}
        FInstaller.LogFileName:='fpcup.log'
        {$ELSE}
        FInstaller.LogFileName:=ExpandFileNameUTF8('~/fpcup.log')
        {$ENDIF MSWINDOWS}
      else
        FInstaller.LogFileName:=sLogFile;
      // Deal with options coming from ini (e.g. Clean=true)
      try
        FInstaller.Clean:=Options.GetOption('','clean',false);
      except
        on E: ECommandLineError do begin
          // option quite probably did not have an argument
          FInstaller.Clean:=Options.GetOptionNoParam('','clean',false);
        end;
      end;
      FInstaller.ConfigFile:=Options.GetOption('','moduleconfig',ExtractFilePath(ParamStr(0))+installerUniversal.CONFIGFILENAME);
      FInstaller.CrossCPU_Target:=Options.GetOption('','cputarget','');
      FInstaller.ShortCutNameFpcup:=Options.GetOption('','fpcuplinkname',DirectorySeparator);
      // Find out if the user specified --fpcuplinkname= to explicitly block creation of a link, or just didn't specify anything.
      if FInstaller.ShortcutNameFPCup=DirectorySeparator then
        if bHaveInstallDir then
          FInstaller.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update'  // sInstallDir has no terminating pathdelimiter!!
        else
          FInstaller.ShortCutNameFpcup:='fpcup_update'; //Nothing to go on, so use default
      FInstaller.FPCOPT:=Options.GetOption('','fpcOPT','');
      {$IF (defined(BSD)) and (not defined(Darwin))}
      //todo: check for other BSDs
      if pos('-Fl/usr/local/lib/',FInstaller.FPCOPT)=0 then
      begin
        writeln('FPC options: FreeBSD needs -Fl/usr/local/lib as options; adding it. For details, see '+LineEnding+
          'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4');
        FInstaller.FPCOPT:=FInstaller.FPCOPT+' -Fl/usr/local/lib';
      end;
      {$ENDIF defined(BSD) and not defined(Darwin)}
      FInstaller.FPCDesiredRevision:=Options.GetOption('','fpcrevision','',false);
      FInstaller.PatchCmd:=Options.GetOption('','patchcmd','patch -p0 -i',false);
      // Deal with options coming from ini (e.g. Help=true)
      try
        bHelp:=Options.GetOption('h','help',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        bHelp:=Options.GetOptionNoParam('h','help',false);
        end;
      end;
      try
        FInstaller.KeepLocalChanges:=Options.GetOption('','keeplocalchanges',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FInstaller.KeepLocalChanges:=Options.GetOptionNoParam('','keeplocalchanges');
        end;
      end;
      try
        FInstaller.ReApplyLocalChanges:=Options.GetOption('','reapplylocalchanges',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FInstaller.reapplylocalchanges:=Options.GetOptionNoParam('','reapplylocalchanges');
        end;
      end;
      FInstaller.ShortCutNameLazarus:=Options.GetOption('','lazlinkname',DirectorySeparator);
      // Find out if the user specified --shortcutnamelazarus= to explicitly block creation of a link, or just didn't specify anything.
      if (FInstaller.ShortCutNameLazarus=DirectorySeparator) then
        if bHaveInstalldir then
          FInstaller.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir)  // sInstallDir has no terminating pathdelimiter!!
        else if UpperCase(ExtractFileName(FInstaller.LazarusDirectory))='LAZARUS' then
          FInstaller.ShortCutNameLazarus:='Lazarus_fpcup' // default installdir, default lazarus dir
        else
          FInstaller.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(FInstaller.LazarusDirectory);

      FInstaller.LazarusOPT:=Options.GetOption('','lazOPT','');
      {$IF (defined(BSD)) and (not defined(Darwin))}
      //todo: check for other BSDs
      if (pos('-Fl/usr/local/lib/',FInstaller.LazarusOPT)=0) then
      begin
        writeln('Lazarus options: FreeBSD needs -Fl/usr/local/lib as options; adding it. For details, see '+LineEnding+
          'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4');
        FInstaller.LazarusOpt:=FInstaller.LazarusOPT+' -Fl/usr/local/lib';
      end;
      if (pos('-Fl/usr/X11R6/lib',FInstaller.LazarusOPT)=0) then
      begin
        writeln('Lazarus options: FreeBSD needs -Fl/usr/X11R6/lib as options; adding it. For details, see '+LineEnding+
          'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4');
        FInstaller.LazarusOpt:=FInstaller.LazarusOPT+' -Fl/usr/X11R6/lib';
      end;
      {
      todo: when building linux cross compiler use these
-Fl/compat/linux/lib
-Fl/compat/linux/usr/lib
-Fl/compat/linux/usr/X11R6/lib
      }
      {$ENDIF defined(BSD) and not defined(Darwin)}
      FInstaller.LazarusDesiredRevision:=Options.GetOption('','lazrevision','',false);
      FInstaller.CrossLCL_Platform:=Options.GetOption('','lclplatform','');
      FInstaller.IncludeModules:=Options.GetOption('','include','',false);
      FInstaller.SkipModules:=Options.GetOption('','skip','',false);
      FInstaller.OnlyModules:=Options.GetOption('','only','',false);
      FInstaller.CrossOS_Target:=Options.GetOption('','ostarget','');
      s:=Options.GetOption('','primary-config-path','');
      if (s='') then
        // If we have no input from the user, let's create a name based on the directory where
        // Lazarus is to be installed
        FInstaller.LazarusPrimaryConfigPath:=
          IncludeTrailingPathDelimiter(sInstallDir)+'config_'+ExtractFileName(ExcludeTrailingPathDelimiter(FInstaller.LazarusDirectory))
      else
        FInstaller.LazarusPrimaryConfigPath:=ExcludeTrailingPathDelimiter(s);

      // Deal with options coming from ini (e.g. Help=true)
      try
        FInstaller.Uninstall:=Options.GetOption('','uninstall',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FInstaller.Uninstall:=Options.GetOptionNoParam('','uninstall');
        end;
      end;

      try
        // do not add to default options
        FInstaller.Verbose:=Options.GetOption('','verbose',false,false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FInstaller.Verbose:=Options.GetOptionNoParam('','verbose',false);
        end;
      end;
      try
        //do not add to default options
        bVersion:=Options.GetOption('','version',false,false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        bVersion:=Options.GetOptionNoParam('','version',false);
        end;
      end;
      try
        bNoConfirm:=Options.GetOption('','noconfirm',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        bNoConfirm:=Options.GetOptionNoParam('','noconfirm');
        end;
      end;
    except
      on E:Exception do
      begin
      writeln('Error: wrong command line options given:');
      writeln(E.Message);
      WriteHelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList,FInstaller.ConfigFile);
      result:=13; //Quit with error resultcode
      exit;
      end
    end;
    FInstaller.LoadFPCUPConfig;
    //svn2 seems to lag behind a lot, so don't use that.
    //load URL's after LoadFPCUPConfig so that we have loaded the aliases
    try
      FInstaller.FPCURL:=Options.GetOption('','fpcURL','svn://svn.freepascal.org/svn/fpc/branches/fixes_2_6');
      FInstaller.LazarusURL:=Options.GetOption('','lazURL','svn://svn.freepascal.org/svn/lazarus/trunk');
    except
      on E:Exception do
      begin
      writeln('Error: wrong command line options given:');
      writeln(E.Message);
      result:=13; //Quit with error resultcode
      exit;
      end;
    end;

    if Options.ValidateOptions<>'' then
      begin
      // settings.ini can contain include=fpspreadsheet,mupdf but also
      // the fpcup.ini style
      // fpspreadsheet=1
      // mupdf=0
      // Process those modules now
      try
        LeftOverOptions:=TStringList.Create;
        for i:=0 to Options.RestArguments.Count-1 do begin
          iCurrentOption:=LeftOverOptions.Add(copy(Options.RestArguments[i],length('--')+1,length(Options.RestArguments[i])));
          if (FInstaller.ModulePublishedList.IndexOf(LeftOverOptions.Names[iCurrentOption])<>-1) then
            case (uppercase(LeftOverOptions.ValueFromIndex[iCurrentOption])) of
              '-1','1','TRUE','YES','INSTALL','ENABLE', 'ON': begin
                FInstaller.IncludeModules:=FInstaller.IncludeModules+','+LeftOverOptions.Names[iCurrentOption];
                LeftOverOptions.Delete(iCurrentOption);
              end;
              '0','FALSE','NO','UNINSTALL','REMOVE','DISABLE', 'OFF': begin
                FInstaller.SkipModules:=FInstaller.SkipModules+','+LeftOverOptions.Names[iCurrentOption];
                LeftOverOptions.Delete(iCurrentOption);
              end
            else
              // Invalid option. leave LeftOverOptions[iCurrentOption] for the error handling below.
            end;
        end;
        // Fix up any added initial commas
        if copy(FInstaller.IncludeModules,1,1)=',' then
          FInstaller.IncludeModules:=copy(FInstaller.IncludeModules,2,Length(FInstaller.IncludeModules));
        if copy(FInstaller.SkipModules,1,1)=',' then
          FInstaller.SkipModules:=copy(FInstaller.SkipModules,2,Length(FInstaller.SkipModules));
        if LeftOverOptions.Count>0 then begin
          writeln('Error: wrong command line options given:');
          writeln(LeftOverOptions.Text);
          WriteHelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList,FInstaller.ConfigFile);
          result:=13; //Quit with error resultcode
          exit;
        end;
      finally
        LeftOverOptions.Free;
      end;
      end; //end of options validations

    if bHelp then
      begin
      writehelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList,FInstaller.ConfigFile);
      result:=0; //quit without error
      end
    else if bVersion then
      begin
      //writeversion; //version will be written anyway
      result:=0; //quit without error
      end
    else
      begin
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
      writeln('Lazarus shortcut name:  '+FInstaller.ShortCutNameLazarus);
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
      writeln('Log file name:          '+FInstaller.LogFileName);
      writeln('Additional modules:     '+FInstaller.IncludeModules);
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
        readln(sConfirm);
        if uppercase(copy(sConfirm,1,1))='N' then
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
  writeln('- run again with --verbose and check for make, lazbuild errors etc.');
  writeln('- make sure there''s a valid SVN executable in your path.');
  {$IFNDEF MSWINDOWS}
  writeln('- make sure the GNU binutils (make etc), windres, subversion client are installed');
  writeln('  e.g. on Debian/Ubuntu: aptitude install build-essential mingw32-binutils subversion ');
  writeln('  ln -s /usr/bin/i586-mingw32msvc-windres /usr/bin/windres');
  writeln('  see http://wiki.lazarus.freepascal.org/Lazarus_Resources#Checking_you_have_windres');
  {$ENDIF MSWINDOWS}
  writeln('- if compiling Lazarus, make sure your lhelp is closed.');
  writeln('- try removing all intermediate files by running fpcup with the --clean option');
  writeln('  and/or manually deleting all *.ppu/*.a/*.o followed by svn up');
  writeln('- if that does not work: use the --uninstall option to remove all files ');
  writeln('  including your FPC and Lazarus directories');
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
