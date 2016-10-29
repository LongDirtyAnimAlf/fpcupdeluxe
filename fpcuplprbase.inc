procedure WriteVersion;
begin
  writeln('Version: based on commit '+RevisionStr+' ('+versiondate+')');
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
  writeln(' fpcURL=<URL>          SVN URL from which to download; default: fixes_3.0:');
  writeln('                       http://svn.freepascal.org/svn/fpc/branches/fixes_3_0');
  writeln('                       Accepts shortcuts: ');
  writeln('                       '+installerUniversal.GetAlias('fpcURL','list'));
  {$ifndef FPCONLY}
  writeln(' lazURL=<URL>          SVN URL from which to download; default: 1.4');
  writeln('                       trunk (newest version):');
  writeln('                       http://svn.freepascal.org/svn/lazarus/trunk');
  writeln('                       Accepts shortcuts: ');
  writeln('                       '+installerUniversal.GetAlias('lazURL','list'));
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
  writeln(' fpcrevision=<number>  Revert to FPC SVN revision <number>');
  {$ifndef FPCONLY}
  writeln(' lazrevision=<number>  Revert to Lazarus SVN revision <number>');
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
  writeln(' keeplocalchanges      Keep locally modified files (normally these would be');
  writeln('                       backed up as .diff files before doing svn revert.');
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
  writeln(' svnexe=<path>         Full path to svn executable (handy if you have');
  writeln('                       multiple versions.');
  writeln(' patchcmd              Command to use to reapply local changes backed up with');
  writeln('                       svn diff command. The diff file is passed as the only');
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
  writeln(' getfilesonly          Just get files. No repos. Uses "svn export"');
  writeln(' disablejobs           Do not use (parallel) jobs when building FPC.');
  writeln(' noconfirm             No confirmation asked. For batch operation.');
  writeln(' verbose               Show output from svn and make.');
  writeln(' version               Show version info and quit.');
  writeln('');
  writeln('Share and enjoy!');
  writeln('');
end;

function CheckOptions(FInstaller: TFPCupManager):integer;
// Returns -1 for success and further execution of fpcup
// 0 for success but fpcup should stop (after showing help etc)
// other codes are error codes
var
  {$IFNDEF MSWINDOWS}
  //Linux, Unix,...
  FPCUpLink:string;
  {$ENDIF}
  bNoConfirm,bHelp,bVersion:boolean;
  i, iCurrentOption: integer;
	sAllParameters:string;
  sConfirm:string;
  Options:TCommandLineOptions;
  sIniFile: string;
  sInstallDir: string; // Root installation directory
  bHaveInstalldir: boolean; //Has user explicitly specified a non-standard install dir?
  sLogFile: string; //Filename for log
  {$ifndef FPCONLY}
  s:string;
  {$endif}
  LeftOverOptions: TStringList; //Options left over after processing; may contain module=0 options
begin
  // First check for settings.ini; it might not be present but specified anyway.
  // In any case, we need to extract it from the resource sometime unless we
  // want to create an installer for each platform.
  if not FileExistsUTF8(SafeGetApplicationPath+SETTTINGSFILENAME) then
    SaveInisFromResource(SafeGetApplicationPath+SETTTINGSFILENAME,'settings_ini');

  Options:=TCommandLineOptions.Create;
  try
    result:=CHECKOPTIONS_SUCCESS; //no error
    try
      sIniFile:=(Options.GetOption('','inifile',''));
      if sIniFile<>'' then
      begin
        // Get setting, converting relative paths (including e.g. ~/bla.ini) to
        // absolute paths.
        sIniFile:=SafeExpandFileNameUTF8(sIniFile);
        Options.IniFileSection:=Options.GetOption('','inisection','General');
        Options.CaseSensitive:=false; //easier when dealing with ini files
        try
          // Setting this property loads the file:
          Options.IniFile:=sIniFile;

          // Strip arguments from options that normally don't take an argument:
          LeftOverOptions:=TStringList.Create;
          LeftOverOptions.Add('noconfirm');
          LeftOverOptions.Add('uninstall');
          LeftOverOptions.Add('getfullrepo');
          LeftOverOptions.Add('getfilesonly');
          LeftOverOptions.Add('disablejobs');
          LeftOverOptions.Add('verbose');
          LeftOverOptions.Add('version');
          try
            for i:=Options.Params.Count-1 downto 0 do
            begin
              for iCurrentOption:=0 to LeftOverOptions.Count-1 do
              begin
                // Found the parameter
                if pos('--'+lowercase(LeftOverOptions[iCurrentOption]),
                  lowercase(Options.Params[i]))=1 then
                begin
                  case (uppercase(Options.Params.ValueFromIndex[i])) of
                    '-1','1','TRUE','YES','INSTALL','ENABLE', 'ON': begin
                      // Rewrite without argument
                      Options.Params[i]:='--'+LeftOverOptions[iCurrentOption];
                    end;
                    '0','FALSE','NO','UNINSTALL','REMOVE','DISABLE', 'OFF': begin
                      // Silently remove false option
                      Options.Params.Delete(i);
                    end;
                  end;
                end;
              end;
            end;
          finally
            LeftOverOptions.Free;
          end;
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
			
      // Save all passed parameters, including any in ini file
      // before the params are removed again by Options.GetOption calls
      sAllParameters:=Options.Params.Text;

      {$IFDEF MSWINDOWS}
      // All directories specified: absolute paths without trailing delimiter
      sInstallDir:=Options.GetOption('','installdir','');
      if sInstallDir='' then begin
        sInstallDir:='C:\development';
        bHaveInstalldir:=false;
      end
      else begin
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FInstaller.MakeDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','binutilsdir',sInstallDir+'\fpcbootstrap')));
      FInstaller.BootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','fpcbootstrapdir',sInstallDir+'\fpcbootstrap')));
      FInstaller.FPCDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','fpcdir',sInstallDir+'\fpc')));
      {$ifndef FPCONLY}
      FInstaller.LazarusDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','lazdir',sInstallDir+'\lazarus')));
      {$endif}
      {$ELSE} //*nix
      sInstallDir:=Options.GetOption('','installdir','');
      if sInstallDir='' then begin
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8('~/development')); //fallback default
        bHaveInstalldir:=false;
      end
      else begin
        // Expand home dir etc
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FInstaller.MakeDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','binutilsdir','')));
      FInstaller.BootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','fpcbootstrapdir',sInstallDir+'/fpcbootstrap')));
      FInstaller.FPCDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','fpcdir',sInstallDir+'/fpc')));
      {$ifndef FPCONLY}
      FInstaller.LazarusDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','lazdir',sInstallDir+'/lazarus')));
      {$endif}
      {$ENDIF MSWINDOWS}
      FInstaller.SVNExecutable := ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','svnexe','')));

      FInstaller.CrossToolsDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','crossbindir','')));
      FInstaller.CrossLibraryDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','crosslibdir','')));

      sLogFile:=Options.GetOption('','logfilename','',true);
      if sLogFile='' then
        {$IFDEF MSWINDOWS}
        FInstaller.LogFileName:='fpcup.log'
        {$ELSE}
        FInstaller.LogFileName:=SafeExpandFileNameUTF8('~/fpcup.log')
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
      FInstaller.ConfigFile:=Options.GetOption('','moduleconfig',ProgramDirectory+installerUniversal.CONFIGFILENAME);
      FInstaller.CrossCPU_Target:=Options.GetOption('','cputarget','');
      FInstaller.CrossOS_SubArch:=Options.GetOption('','subarch','');
      FInstaller.CrossOPT:=Options.GetOption('','crossopt','');
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

      {$IF defined(BSD) and not defined(DARWIN)}
      FInstaller.PatchCmd:=Options.GetOption('','patchcmd','gpatch',false);
      {$ELSE}
      FInstaller.PatchCmd:=Options.GetOption('','patchcmd','patch',false);
      {$ENDIF MSWINDOWS}

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
        FInstaller.ReApplyLocalChanges:=Options.GetOptionNoParam('','reapplylocalchanges');
        end;
      end;

      // changes can only be reapplied (true) when they are stored in a diff when KeepLocalChanges=false
      if FInstaller.KeepLocalChanges then FInstaller.reapplylocalchanges:=False;
      {$ifndef FPCONLY}
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
      {$endif}
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
      {$ENDIF defined(BSD) and not defined(Darwin)}
      {$ifndef FPCONLY}
      FInstaller.LazarusDesiredRevision:=Options.GetOption('','lazrevision','',false);
      FInstaller.CrossLCL_Platform:=Options.GetOption('','lclplatform','');
      {$endif}
      FInstaller.IncludeModules:=Options.GetOption('','include','',false);
      FInstaller.SkipModules:=Options.GetOption('','skip','',false);
      FInstaller.OnlyModules:=Options.GetOption('','only','',false);

      FInstaller.FPCPatches:=Options.GetOption('','fpcpatch','',false);
      {$ifndef FPCONLY}
      FInstaller.LazarusPatches:=Options.GetOption('','lazpatch','',false);
      {$endif}
      FInstaller.CrossOS_Target:=Options.GetOption('','ostarget','');
      {$ifndef FPCONLY}
      s:=Options.GetOption('','primary-config-path','');
      if (s='') then
        // If we have no input from the user, let's create a name based on the directory where
        // Lazarus is to be installed
        FInstaller.LazarusPrimaryConfigPath:=
          IncludeTrailingPathDelimiter(sInstallDir)+'config_'+ExtractFileName(ExcludeTrailingPathDelimiter(FInstaller.LazarusDirectory))
      else
        FInstaller.LazarusPrimaryConfigPath:=ExcludeTrailingPathDelimiter(s);
      {$endif}
      FInstaller.Uninstall:=Options.GetOptionNoParam('','uninstall',true);
      // do not add to default options:
      FInstaller.Verbose:=Options.GetOptionNoParam('','verbose',false);
      {$ifdef DEBUG}
      FInstaller.Verbose:=True;
      {$endif}

      // getfullrepo is a depreciated option ... left here for compatibility only
      Options.GetOptionNoParam('','getfullrepo',false);
      FInstaller.ExportOnly:=(Options.GetOptionNoParam('','getfilesonly'));
      FInstaller.NoJobs:=Options.GetOptionNoParam('','disablejobs');
      FInstaller.UseGitClient:=Options.GetOptionNoParam('','usegitclient',false);
      // do not add to default options:
      bVersion:=Options.GetOptionNoParam('','version',false);
      bNoConfirm:=Options.GetOptionNoParam('','noconfirm',true);
    except
      on E:Exception do
      begin
      writeln('Error: wrong command line options given:');
      writeln(E.Message);
      writeln('Press enter to see a list of all available command line options.');
      readln;
      WriteHelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList);
      result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
      exit;
      end
    end;
    FInstaller.LoadFPCUPConfig;
    //svn2 seems to lag behind a lot, so don't use that.
    //load URLs after LoadFPCUPConfig so we're sure we have loaded/parsed the URL aliases
    try
      FInstaller.FPCURL:=Options.GetOption('','fpcURL',installerUniversal.GetAlias('fpcURL','default'));
      {$ifndef FPCONLY}
      FInstaller.LazarusURL:=Options.GetOption('','lazURL',installerUniversal.GetAlias('lazURL','default'));
      {$endif}
    except
      on E:Exception do
      begin
      writeln('Error: wrong command line options given:');
      writeln(E.Message);
      result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
      exit;
      end;
    end;

    // HTTP proxy settings, including support for environment variables
    // Environment variables like:
    //http_proxy=http://username:password@myproxy.ril.com:port/
    //https_proxy=https://username:password@myproxy.ril.com:port/
    //ftp_proxy=ftp://username:password@myproxy.ril.com:port/
    try
      // Get option from specified options
      FInstaller.HTTPProxyHost:=Options.GetOption('','httpproxy','',true);

      // If no option specified, try environment variable
      // Note we don't save these options to persistent options -
      // they should remain part of the environment
      if (FInstaller.HTTPProxyHost='') and (GetEnvironmentVariable('http_proxy')<>'') then
      begin
        FInstaller.HTTPProxyHost:=GetEnvironmentVariable('http_proxy');
      end;

      // Strip out trailing /
      if copy(FInstaller.HTTPProxyHost,length(FInstaller.HTTPProxyHost),1)='/' then
        FInstaller.HTTPProxyHost:=copy(FInstaller.HTTPProxyHost,1,length(FInstaller.HTTPProxyHost)-1);

      // Extract port - search backwards to allow passwords with :
      i:=rpos(':',FInstaller.HTTPProxyHost);
      // Don't pick up : from any username:password segment
      if (i=0) or
        (rpos('@',FInstaller.HTTPProxyHost)>i) then
        if pos('https://',FInstaller.HTTPProxyHost)=1 then
          FInstaller.HTTPProxyPort:=443
        else
          FInstaller.HTTPProxyPort:=8080 {seems like a good default}
      else
      begin
        FInstaller.HTTPProxyPort:=strtointdef(copy(FInstaller.HTTPProxyHost,i+1,length(FInstaller.HTTPProxyHost)),8080);
        FInstaller.HTTPProxyHost:=copy(FInstaller.HTTPProxyHost,1,i-1);
      end;

      // Strip out http/https
      if pos('https://',FInstaller.HTTPProxyHost)=1 then
        FInstaller.HTTPProxyHost:=copy(Finstaller.HTTPProxyHost,length('https://')+1,length(FInstaller.HTTPProxyHost));
      if pos('http://',FInstaller.HTTPProxyHost)=1 then
        FInstaller.HTTPProxyHost:=copy(Finstaller.HTTPProxyHost,length('http://')+1,length(FInstaller.HTTPProxyHost));

      // Extract out username/password
      // Search from ending of string to front to catch last @ in case password has @
      i:=rpos('@',FInstaller.HTTPProxyHost);
      if i>0 then
      begin
        FInstaller.HTTPProxyUser:=copy(FInstaller.HTTPProxyHost,1,i-1);
        FInstaller.HTTPProxyHost:=copy(FInstaller.HTTPProxyHost,i+1,length(FInstaller.HTTPProxyHost));
        // Extract out password
        i:=pos(':',FInstaller.HTTPProxyUser);
        if i>0 then
        begin
          FInstaller.HTTPProxyPassword:=copy(FInstaller.HTTPProxyUser,i+1,length(FInstaller.HTTPProxyUser));
          FInstaller.HTTPProxyUser:=copy(FInstaller.HTTPProxyUser,1,i-1);
        end;
      end;
    except
      on E:Exception do
      begin
      writeln('Error: wrong command line options given:');
      writeln(E.Message);
      result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
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
                if CheckIncludeModule(LeftOverOptions.Names[iCurrentOption])
                   then FInstaller.IncludeModules:=FInstaller.IncludeModules+','+LeftOverOptions.Names[iCurrentOption]
                   else FInstaller.SkipModules:=FInstaller.SkipModules+','+LeftOverOptions.Names[iCurrentOption];
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
          writeln('Press enter to see a list of all available command line options.');
          readln;
          WriteHelp(FInstaller.ModulePublishedList,FInstaller.ModuleEnabledList);
          result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
          exit;
        end;
      finally
        LeftOverOptions.Free;
      end;
      end; //end of options validations

    if bHelp then
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
      {$IFNDEF MSWINDOWS}
      // Binutils should be in path on non-Windows...
      if FInstaller.MakeDirectory<>'' then
        FInstaller.MakeDirectory:='';
      {$ENDIF MSWINDOWS}

      FInstaller.PersistentOptions:=Options.PersistentOptions;

      writeln('Options:');
      if FInstaller.Clean then
      begin
        writeln('Running --clean: cleaning environment.');
      end;
      if FInstaller.HTTPProxyHost<>'' then
      begin
        writeln('HTTP proxy host:  '+FInstaller.HTTPProxyHost);
        writeln('HTTP proxy port:  '+inttostr(FInstaller.HTTPProxyPort));
        writeln('HTTP proxy user:  '+FInstaller.HTTPProxyUser);
        writeln('HTTP proxy pass:  <SECURITY:REDACTED>');
      end;
      {$IFDEF MSWINDOWS}
      // Makes no sense on other platforms
      writeln('Binutils/make dir:  '+FInstaller.MakeDirectory);
      {$ENDIF MSWINDOWS}
      writeln('Bootstrap dir:      '+FInstaller.BootstrapCompilerDirectory);
      writeln('FPC URL:            '+FInstaller.FPCURL);
      writeln('FPC options:        '+FInstaller.FPCOPT);
      writeln('FPC directory:      '+FInstaller.FPCDirectory);
      {$ifndef FPCONLY}
      writeln('Lazarus URL:        '+FInstaller.LazarusURL);
      writeln('Lazarus options:    '+FInstaller.LazarusOPT);
      writeln('Lazarus directory:  '+FInstaller.LazarusDirectory);
      {$endif}
      if FInstaller.KeepLocalChanges then
      begin
        writeln('Keep changes:       yes');
      end
      else
      begin
        writeln('Keep changes:       no');
      end;
      if FInstaller.ReApplyLocalChanges then
      begin
        writeln('Re-apply changes:   yes');
      end
      else
      begin
        writeln('Re-apply changes:   no');
      end;
      writeln('Log file name:      '+FInstaller.LogFileName);

      For i:=0 to FInstaller.ModuleEnabledList.Count-1 do
        begin
        writeln('Standard modules:   '+FInstaller.ModuleEnabledList[i]);
        end;
      if FInstaller.IncludeModules<>'' then
        writeln('Add. modules:       '+FInstaller.IncludeModules);

      if FInstaller.ExportOnly then
      begin
        writeln('');
        writeln('INFO: FPCUP will not download repos. It will only get the files !!!');
      end
      else
      begin
        writeln('');
        writeln('WARNING: FPCUP will download full repositories !!!');
      end;

      if (Pos('trunk',FInstaller.FPCURL)>0) {$ifndef FPCONLY}OR (Pos('trunk',FInstaller.LazarusURL)>0) {$endif} then
      begin
        writeln('');
        writeln('******************************************************************');
        {$ifndef FPCONLY}
        writeln(' You are now installing a bleeding edge version of [FPC/Lazarus].');
        {$else}
        writeln(' You are now installing a bleeding edge version of FPC.');
        {$endif}
        writeln(' Please be forewarned that things might not function,');
        writeln(' as you would expect from a stable release.');
        writeln(' Installing a stable release,');
        writeln(' will give you a stable development environment,');
        {$ifndef FPCONLY}
        writeln(' and is the preferred way of using [FPC/Lazarus].');
        {$else}
        writeln(' and is the preferred way of using FPC.');
        {$endif}
        writeln('');
        writeln(' PS: You can build Lazarus trunk with normal FPC stable !! <-----');
        writeln('     Just execute fpclazup --lazURL="trunk"                <-----');
        writeln('******************************************************************');
        writeln('');
      end;

      if (Pos('-g ',FInstaller.FPCOPT)>0)
         then writeln('FPC is compiled with debug (-g) !');
      {$ifndef FPCONLY}
      if (Pos('-g ',FInstaller.LazarusOPT)>0)
         then writeln('Lazarus is compiled with debug (-g) !');
      {$endif}
      writeln('');

      // Remove password from output
      if FInstaller.HTTPProxyPassword='' then
      begin
        writeln('Effective parameters:   ');
        writeln(trim(sAllParameters));
        {$IFDEF MSWINDOWS}
        writeln('Persistent parameters (can be saved in batch file):');
        {$ELSE}
        writeln('Persistent parameters (can be saved in shell script):');
        {$ENDIF}
        writeln(trim(FInstaller.PersistentOptions));
      end
      else
      begin
        writeln('Note: replaced proxy password in all parameters, ');
        writeln('so output may be unreliable:');
        writeln('');
        writeln('Effective parameters:   ');
        writeln(trim(StringReplace(sAllParameters,
          FInstaller.HTTPProxyPassword,
          '<SECURITY:REDACTED>',
          [rfReplaceAll,rfIgnoreCase])));
        writeln('Persistent parameters:  ');
        writeln(trim(StringReplace(FInstaller.PersistentOptions,
          FInstaller.HTTPProxyPassword,
          '<SECURITY:REDACTED>',
          [rfReplaceAll,rfIgnoreCase])));
        if FInstaller.Verbose then
        begin
          writeln('');
          writeln('WARNING: proxy password will appear in screen output!');
          writeln('');
        end;
      end;

      // Note: we don't have a unicode version of SafeExpandFileName; investigate consequences for Unicode paths!??!?
      // User could have specified relative paths so we're normalizing them.
      {$ifndef FPCONLY}
      if SafeExpandFileName(FInstaller.LazarusDirectory)=SafeExpandFileName(FInstaller.FPCDirectory) then
        writeln('WARNING: FPC and Lazarus directories are the same ('+FInstaller.FPCDirectory+'). This will not work!');
      {$endif}
      if (FInstaller.FPCDesiredRevision<>'') then
        writeln('WARNING: Reverting FPC to revision '+FInstaller.FPCDesiredRevision);
      {$ifndef FPCONLY}
      if (FInstaller.LazarusDesiredRevision<>'') then
        writeln('WARNING: Reverting Lazarus to revision '+FInstaller.LazarusDesiredRevision);
      {$endif}
      if FInstaller.SkipModules<>'' then
        writeln('WARNING: Skipping installation/update of '+FInstaller.SkipModules);
      if FInstaller.OnlyModules<>'' then
        writeln('WARNING: Limiting installation/update to '+FInstaller.OnlyModules);

      if FInstaller.Uninstall then
      begin
        writeln('');
        writeln('WARNING: UNINSTALLING !!!');
        writeln('');
      end
      else if FInstaller.Clean then
      begin
        writeln('');
        writeln('WARNING: CLEANING !!!');
        writeln('');
      end;

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
  {$IFNDEF MSWINDOWS}
  writeln('- make sure there''s a valid SVN executable in your path.');
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
  writeln('- remove the files in the bootstrap directory. fpcup will redownload if required.');
end;
