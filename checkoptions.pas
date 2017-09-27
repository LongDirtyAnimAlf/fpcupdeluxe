unit checkoptions;

interface

uses installerManager;

const
  OK_IGNORE=0;
  CHECKOPTIONS_SUCCESS=-1; //checkoptions ran ok
  ERROR_WRONG_OPTIONS=13; //user specified incorrect command line options
  FPCUP_GETHELP=32; //get help
  ERROR_FPCUP_BUILD_FAILED=64; //fpcup ran but build failed

function CheckFPCUPOptions(FInstaller: TFPCupManager):integer;

implementation

uses
 classes,
 sysutils,
 strutils,
 FileUtil,
 installerUniversal,
 fpcuputil,
 commandline;

function CheckFPCUPOptions(FInstaller: TFPCupManager):integer;
// Returns -1 for success and further execution of fpcup
// 0 for success but fpcup should stop (after showing help etc)
// other codes are error codes
var
  {$IFNDEF MSWINDOWS}
  //Linux, Unix,...
  FPCUpLink:string;
  {$ENDIF}
  bHelp,bVersion,bFPCsplit:boolean;
  {$ifndef FPCONLY}
  //bLazsplit:boolean;
  {$ENDIF}
  i, iCurrentOption: integer;
  sAllParameters:string;
  sConfirm:string;
  bNoConfirm:boolean;
  Options:TCommandLineOptions;
  sIniFile: string;
  sInstallDir: string; // Root installation directory
  bHaveInstalldir: boolean; //Has user explicitly specified a non-standard install dir?
  sLogFile: string; //Filename for log
  s:string;
  LeftOverOptions: TStringList; //Options left over after processing; may contain module=0 options
begin
  // First check for settings.ini; it might not be present but specified anyway.
  // In any case, we need to extract it from the resource sometime unless we
  // want to create an installer for each platform.
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
          LeftOverOptions.Add('usewget');
          LeftOverOptions.Add('fpcsplit');
          //LeftOverOptions.Add('lazsplit');
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

      sInstallDir:=Options.GetOption('','installdir','');
      {$IFDEF MSWINDOWS}
      if sInstallDir='' then
      begin
        sInstallDir:='C:\development';
        bHaveInstalldir:=false;
      end
      else
      begin
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FInstaller.MakeDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','binutilsdir',IncludeTrailingPathDelimiter(sInstallDir)+'fpcbootstrap')));
      {$ELSE} //*nix
      if sInstallDir='' then
      begin
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8('~/development')); //fallback default
        bHaveInstalldir:=false;
      end
      else
      begin
        // Expand home dir etc
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FInstaller.MakeDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','binutilsdir','')));
      {$ENDIF MSWINDOWS}

      FInstaller.BaseDirectory:=sInstallDir;
      FInstaller.BootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','fpcbootstrapdir',IncludeTrailingPathDelimiter(sInstallDir)+'fpcbootstrap')));
      FInstaller.FPCInstallDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','fpcdir',IncludeTrailingPathDelimiter(sInstallDir)+'fpc')));
      bFPCsplit:=Options.GetOptionNoParam('','fpcsplit');
      if bFPCsplit
               then FInstaller.FPCSourceDirectory:=FInstaller.FPCInstallDirectory+'src'
               else FInstaller.FPCSourceDirectory:=FInstaller.FPCInstallDirectory;
      {$ifndef FPCONLY}
      FInstaller.LazarusDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileNameUTF8(Options.GetOption('','lazdir',IncludeTrailingPathDelimiter(sInstallDir)+'lazarus')));
      {
      bLazsplit:=Options.GetOptionNoParam('','lazsplit');
      if bLazsplit
         then FInstaller.LazarusSourceDirectory:=FInstaller.LazarusDirectory+'src'
         else FInstaller.LazarusSourceDirectory:=FInstaller.LazarusDirectory;
      }
      {$endif}

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

      {$ifdef LCL}
      // do not create shortcut for fpc in case of GUI !!
      FInstaller.ShortCutNameFpcup:=EmptyStr;
      {$else}
      FInstaller.ShortCutNameFpcup:=Options.GetOption('','fpcuplinkname',DirectorySeparator);
      // Find out if the user specified --fpcuplinkname= to explicitly block creation of a link, or just didn't specify anything.
      if FInstaller.ShortcutNameFPCup=DirectorySeparator then
        if bHaveInstallDir then
          FInstaller.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update'  // sInstallDir has no terminating pathdelimiter!!
        else
          FInstaller.ShortCutNameFpcup:='fpcup_update'; //Nothing to go on, so use default
      {$endif}

      FInstaller.FPCOPT:=Options.GetOption('','fpcOPT','');
      {$IF (defined(BSD)) and (not defined(Darwin))}
      //todo: check for other BSDs
      if pos('-Fl/usr/local/lib/',FInstaller.FPCOPT)=0 then
      begin
        infoln('FPC options: FreeBSD needs -Fl/usr/local/lib as options; adding it. For details, see '+LineEnding+
          'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4',etInfo);
        FInstaller.FPCOPT:=FInstaller.FPCOPT+' -Fl/usr/local/lib';
      end;
      {$ENDIF defined(BSD) and not defined(Darwin)}
      FInstaller.FPCDesiredRevision:=Options.GetOption('','fpcrevision','',false);

      FInstaller.PatchCmd:=Options.GetOption('','patchcmd','patch',false);

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

      {$IF (defined(BSD)) and (not defined(Darwin))}
      //todo: check for other BSDs
      if (pos('-Fl/usr/local/lib/',FInstaller.LazarusOPT)=0) then
      begin
        infoln('Lazarus options: FreeBSD needs -Fl/usr/local/lib as options; adding it. For details, see '+LineEnding+
          'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4',etInfo);
        FInstaller.LazarusOpt:=FInstaller.LazarusOPT+' -Fl/usr/local/lib';
      end;
      if (pos('-Fl/usr/X11R6/lib',FInstaller.LazarusOPT)=0) then
      begin
        infoln('Lazarus options: FreeBSD needs -Fl/usr/X11R6/lib as options; adding it. For details, see '+LineEnding+
          'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4',etInfo);
        FInstaller.LazarusOpt:=FInstaller.LazarusOPT+' -Fl/usr/X11R6/lib';
      end;
      {$ENDIF defined(BSD) and not defined(Darwin)}
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
      FInstaller.UseWget:=Options.GetOptionNoParam('','usewget');
      // do not add to default options:
      bVersion:=Options.GetOptionNoParam('','version',false);
      bNoConfirm:=Options.GetOptionNoParam('','noconfirm',true);
    except
      on E:Exception do
      begin
        result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
        infoln('Error: wrong command line options given: '+E.Message,etError);
        {$ifndef LCL}
        writeln('Press enter to see a list of all available command line options.');
        readln;
        {$endif}
        exit;
      end
    end;
    FInstaller.LoadFPCUPConfig;
    //load URLs after LoadFPCUPConfig so we're sure we have loaded/parsed the URL aliases
    try
      FInstaller.FPCURL:=Options.GetOption('','fpcURL',installerUniversal.GetAlias('fpcURL','stable'));
      {$ifndef FPCONLY}
      FInstaller.LazarusURL:=Options.GetOption('','lazURL',installerUniversal.GetAlias('lazURL','stable'));
      {$endif}
    except
      on E:Exception do
      begin
        infoln('Error: wrong command line options given: '+E.Message,etError);
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
        infoln('Error: wrong command line options given: '+E.Message,etError);
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

        if LeftOverOptions.Count>0 then
        begin
          result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
          infoln('Error: wrong command line options given: '+LeftOverOptions.Text,etError);
          {$ifndef LCL}
          writeln('Press enter to see a list of all available command line options.');
          readln;
          {$endif}
          exit;
        end;
      finally
        LeftOverOptions.Free;
      end;
    end; //end of options validations

    if bHelp then
    begin
      result:=FPCUP_GETHELP; //show help
    end
    else if bVersion then
    begin
      //writeversion; //version will be written anyway
      result:=OK_IGNORE; //quit without error
    end
    else
    begin
      {$IFNDEF MSWINDOWS}
      // Binutils should be in path on non-Windows...
      if FInstaller.MakeDirectory<>'' then
        FInstaller.MakeDirectory:='';
      {$ENDIF MSWINDOWS}

      FInstaller.PersistentOptions:=Options.PersistentOptions;

      {$ifndef LCL}

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
      writeln('Base directory:     '+FInstaller.BaseDirectory);
      writeln('Bootstrap dir:      '+FInstaller.BootstrapCompilerDirectory);
      writeln('FPC URL:            '+FInstaller.FPCURL);
      writeln('FPC options:        '+FInstaller.FPCOPT);
      writeln('FPC directory:      '+FInstaller.FPCInstallDirectory);
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
          result:=OK_IGNORE; //quit without error
        end;
      end;
      {$endif}
    end;
  finally
    Options.free;
  end;
end;

end.
