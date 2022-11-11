unit checkoptions;

interface

uses installerManager;

const
  OK_IGNORE=0;
  CHECKOPTIONS_SUCCESS=-1; //checkoptions ran ok
  ERROR_WRONG_OPTIONS=13; //user specified incorrect command line options
  FPCUP_GETHELP=32; //get help
  ERROR_FPCUP_BUILD_FAILED=64; //fpcup ran but build failed

function CheckFPCUPOptions(FManager: TFPCupManager):integer;

implementation

uses
 Classes,
 SysUtils,
 StrUtils,
 m_crossinstaller,
 installerCore,
 installerUniversal,
 fpcuputil,
 commandline;

function CheckFPCUPOptions(FManager: TFPCupManager):integer;
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
  bLazsplit:boolean;
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
        sIniFile:=SafeExpandFileName(sIniFile);
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
          LeftOverOptions.Add('rebuildonly');
          LeftOverOptions.Add('disablejobs');
          LeftOverOptions.Add('usewget');
          LeftOverOptions.Add('includehelp');
          LeftOverOptions.Add('fpcsplit');
          LeftOverOptions.Add('lazsplit');
          LeftOverOptions.Add('autotools');
          LeftOverOptions.Add('verbose');
          LeftOverOptions.Add('version');
          try
            for i:=Options.Params.Count-1 downto 0 do
            begin
              for iCurrentOption:=0 to LeftOverOptions.Count-1 do
              begin
                // Found the parameter
                if Pos('--'+lowercase(LeftOverOptions[iCurrentOption]),
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
            {$ifndef LCL}
            writeln('Specified ini file '+sIniFile+' cannot be read or does not have section '+Options.IniFileSection+'. Aborting.');
            {$endif}
            halt(3);
          end;
          on F:Exception do
          begin
            {$ifndef LCL}
            writeln('Error reading specified ini file '+sIniFile+'. Exception: '+F.Message+'. Aborting!');
            {$endif}
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
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FManager.MakeDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','binutilsdir',IncludeTrailingPathDelimiter(sInstallDir)+'fpcbootstrap')));
      {$ELSE} //*nix
      if sInstallDir='' then
      begin
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName('~/development')); //fallback default
        bHaveInstalldir:=false;
      end
      else
      begin
        // Expand home dir etc
        sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName(sInstallDir));
        bHaveInstalldir:=true;
      end;
      FManager.MakeDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','binutilsdir','')));
      {$ENDIF MSWINDOWS}

      FManager.BaseDirectory:=sInstallDir;
      FManager.BootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','fpcbootstrapdir',IncludeTrailingPathDelimiter(sInstallDir)+'fpcbootstrap')));
      FManager.FPCInstallDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','fpcdir',IncludeTrailingPathDelimiter(sInstallDir)+'fpc')));
      bFPCsplit:=Options.GetOptionNoParam('','fpcsplit');
      if bFPCsplit
               then FManager.FPCSourceDirectory:=FManager.FPCInstallDirectory+'src'
               else FManager.FPCSourceDirectory:=FManager.FPCInstallDirectory;

      {$ifndef FPCONLY}
      FManager.LazarusInstallDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','lazdir',IncludeTrailingPathDelimiter(sInstallDir)+'lazarus')));
      bLazsplit:=Options.GetOptionNoParam('','lazsplit');
      if bLazsplit
         then FManager.LazarusSourceDirectory:=FManager.LazarusInstallDirectory+'src'
         else FManager.LazarusSourceDirectory:=FManager.LazarusInstallDirectory;
      {$endif}

      FManager.AutoTools:=Options.GetOptionNoParam('','autotools',false);

      FManager.CrossToolsDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','crossbindir','')));
      FManager.CrossLibraryDirectory:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Options.GetOption('','crosslibdir','')));

      sLogFile:=Options.GetOption('','logfilename','',true);
      if sLogFile='' then
        {$IFDEF MSWINDOWS}
        FManager.LogFileName:='fpcup.log'
        {$ELSE}
        FManager.LogFileName:=SafeExpandFileName('~/fpcup.log')
        {$ENDIF MSWINDOWS}
      else
        FManager.LogFileName:=sLogFile;
      // Deal with options coming from ini (e.g. Clean=true)
      try
        FManager.Clean:=Options.GetOption('','clean',false);
      except
        on E: ECommandLineError do begin
          // option quite probably did not have an argument
          FManager.Clean:=Options.GetOptionNoParam('','clean',false);
        end;
      end;
      FManager.ConfigFile:=Options.GetOption('','moduleconfig',SafeGetApplicationPath+installerUniversal.CONFIGFILENAME);
      s:=Options.GetOption('','cputarget','');
      if (s<>'') then FManager.CrossCPU_Target:=GetTCPU(s);
      FManager.CrossOS_SubArch:=GetTSubarch(Options.GetOption('','subarch',''));
      FManager.CrossOPT:=Options.GetOption('','crossopt','');

      {$ifdef LCL}
      // do not create shortcut for fpc in case of GUI !!
      //FInstaller.ShortCutNameFpcup:=EmptyStr;
      {$else}
      FManager.ShortCutNameFpcup:=Options.GetOption('','fpcuplinkname',DirectorySeparator);
      // Find out if the user specified --fpcuplinkname= to explicitly block creation of a link, or just didn't specify anything.
      if FManager.ShortcutNameFPCup=DirectorySeparator then
        if bHaveInstallDir then
          FManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update'  // sInstallDir has no terminating pathdelimiter!!
        else
          FManager.ShortCutNameFpcup:='fpcup_update'; //Nothing to go on, so use default
      {$endif}

      FManager.FPCOPT:=Options.GetOption('','fpcOPT','');
      {$IF (defined(BSD)) and (not defined(Darwin))}
      //todo: check for other BSDs
      if Pos('-Fl/usr/local/lib/',FManager.FPCOPT)=0 then
      begin
        //Infoln('FPC options: FreeBSD needs -Fl/usr/local/lib as options; adding it. For details, see '+LineEnding+
        //  'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4',etInfo);
        FManager.FPCOPT:=FManager.FPCOPT+' -Fl/usr/local/lib';
      end;
      {$ENDIF defined(BSD) and not defined(Darwin)}
      FManager.FPCBranch:=Options.GetOption('','fpcBranch','');

      FManager.PatchCmd:=Options.GetOption('','patchcmd','patch',false);

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
        FManager.NativeFPCBootstrapCompiler:=(NOT Options.GetOption('','onlyupbootstrappers',true));
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FManager.NativeFPCBootstrapCompiler:=(NOT Options.GetOptionNoParam('','onlyupbootstrappers'));
        end;
      end;

      try
        FManager.KeepLocalChanges:=Options.GetOption('','keeplocalchanges',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FManager.KeepLocalChanges:=Options.GetOptionNoParam('','keeplocalchanges');
        end;
      end;

      try
        FManager.ReApplyLocalChanges:=Options.GetOption('','reapplylocalchanges',false);
      except
        on E: ECommandLineError do begin
        // option did not have an argument
        FManager.ReApplyLocalChanges:=Options.GetOptionNoParam('','reapplylocalchanges');
        end;
      end;

      // changes can only be reapplied (true) when they are stored in a diff when KeepLocalChanges=false
      if FManager.KeepLocalChanges then FManager.reapplylocalchanges:=False;
      {$ifndef FPCONLY}
      FManager.ShortCutNameLazarus:=Options.GetOption('','lazlinkname',DirectorySeparator);
      // Find out if the user specified --shortcutnamelazarus= to explicitly block creation of a link, or just didn't specify anything.
      if (FManager.ShortCutNameLazarus=DirectorySeparator) then
        if bHaveInstalldir then
          FManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir)  // sInstallDir has no terminating pathdelimiter!!
        else if UpperCase(ExtractFileName(FManager.LazarusInstallDirectory))='LAZARUS' then
          FManager.ShortCutNameLazarus:='Lazarus_fpcup' // default installdir, default lazarus dir
        else
          FManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(FManager.LazarusInstallDirectory);

      FManager.LazarusOPT:=Options.GetOption('','lazOPT','');

      {$IF (defined(BSD)) and (not defined(Darwin))}
      //todo: check for other BSDs
      if (pos('-Fl/usr/local/lib/',FManager.LazarusOPT)=0) then
      begin
        //Infoln('Lazarus options: FreeBSD needs -Fl/usr/local/lib as options; adding it. For details, see '+LineEnding+
        //  'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4',etInfo);
        FManager.LazarusOpt:=FManager.LazarusOPT+' -Fl/usr/local/lib';
      end;
      if (pos('-Fl/usr/X11R6/lib',FManager.LazarusOPT)=0) then
      begin
        //Infoln('Lazarus options: FreeBSD needs -Fl/usr/X11R6/lib as options; adding it. For details, see '+LineEnding+
        //  'http://www.stack.nl/~marcov/buildfaq/#toc-Subsection-1.6.4',etInfo);
        FManager.LazarusOpt:=FManager.LazarusOPT+' -Fl/usr/X11R6/lib -Fl/usr/X11R7/lib';
      end;
      {$ENDIF defined(BSD) and not defined(Darwin)}
      FManager.LazarusBranch:=Options.GetOption('','lazBranch','');
      FManager.LCL_Platform:=Options.GetOption('','lclplatform','');
      {$endif}
      FManager.IncludeModules:=Options.GetOption('','include','',false);
      FManager.SkipModules:=Options.GetOption('','skip','',false);
      FManager.OnlyModules:=Options.GetOption('','only','',false);

      if (NOT Options.GetOptionNoParam('','includehelp')) then
      begin
        if Length(FManager.SkipModules)>0 then FManager.SkipModules:=FManager.SkipModules+',';
        FManager.SkipModules:=FManager.SkipModules+_HELPFPC;
        {$ifndef FPCONLY}
        FManager.SkipModules:=FManager.SkipModules+','+_HELPLAZARUS;
        {$endif}
      end
      else
      begin
        if Length(FManager.IncludeModules)>0 then FManager.IncludeModules:=FManager.IncludeModules+',';
        FManager.IncludeModules:=FManager.IncludeModules+_LHELP;
      end;

      FManager.FPCPatches:=Options.GetOption('','fpcpatch','',false);
      {$ifndef FPCONLY}
      FManager.LazarusPatches:=Options.GetOption('','lazpatch','',false);
      {$endif}
      s:=Options.GetOption('','ostarget','');
      if (s<>'') then FManager.CrossOS_Target:=GetTOS(s);
      {$ifndef FPCONLY}
      s:=Options.GetOption('','primary-config-path','');
      if (s='') then
        // If we have no input from the user, let's create a name based on the directory where
        // Lazarus is to be installed
        FManager.LazarusPrimaryConfigPath:=
          IncludeTrailingPathDelimiter(sInstallDir)+'config_'+ExtractFileName(ExcludeTrailingPathDelimiter(FManager.LazarusInstallDirectory))
      else
        FManager.LazarusPrimaryConfigPath:=ExcludeTrailingPathDelimiter(s);
      {$endif}
      FManager.Uninstall:=Options.GetOptionNoParam('','uninstall',true);
      // do not add to default options:
      FManager.Verbose:=Options.GetOptionNoParam('','verbose',false);
      {$ifdef DEBUG}
      //FInstaller.Verbose:=True;
      {$endif}

      // getfullrepo is a depreciated option ... left here for compatibility only
      Options.GetOptionNoParam('','getfullrepo',false);
      FManager.ExportOnly:=(Options.GetOptionNoParam('','getfilesonly'));
      if (Options.GetOptionNoParam('','rebuildonly')) then
      begin
        FManager.OnlyModules:='FPCCleanAndBuildOnly,LazCleanAndBuildOnly';
      end;
      FManager.NoJobs:=Options.GetOptionNoParam('','disablejobs');
      FManager.UseGitClient:=Options.GetOptionNoParam('','usegitclient',false);
      FManager.UseWget:=Options.GetOptionNoParam('','usewget');
      // do not add to default options:
      bVersion:=Options.GetOptionNoParam('','version',false);
      bNoConfirm:=Options.GetOptionNoParam('','noconfirm',true);
    except
      on E:Exception do
      begin
        result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
        {$ifndef LCL}
        writeln('Error: wrong command line options given: '+E.Message);
        writeln('Press enter to see a list of all available command line options.');
        readln;
        {$endif}
        exit;
      end
    end;
    FManager.LoadFPCUPConfig;
    //load URLs after LoadFPCUPConfig so we're sure we have loaded/parsed the URL aliases

    try
      s:=Options.GetOption('','fpcVersion','');
      if (Length(s)>0) then
      begin
        if AnsiEndsText(GITLABEXTENSION,s) then
          FManager.FPCTag:=s
        else
          FManager.FPCURL:=s;
      end
      else
      begin
        s:=Options.GetOption('','fpcURL','');
        if (Length(s)>0) then
          FManager.FPCURL:=s
        else
        begin
          s:='stable.gitlab';
          FManager.FPCTag:=s;
          Options.PersistentOptions:=trim(Options.PersistentOptions+' --fpcVersion="'+s+'"')
        end;
      end;
      if (Pos('github.com/LongDirtyAnimAlf',FManager.FPCURL)>0) then FManager.FPCBranch:='master';

      FManager.FPCDesiredRevision:=Options.GetOption('','fpcRevision','',false);

      {$ifndef FPCONLY}
      s:=Options.GetOption('','lazVersion','');
      if (Length(s)>0) then
      begin
        if AnsiEndsText(GITLABEXTENSION,s) then
          FManager.LazarusTag:=s
        else
          FManager.LazarusURL:=s;
      end
      else
      begin
        s:=Options.GetOption('','lazURL','');
        if (Length(s)>0) then
          FManager.LazarusURL:=s
        else
        begin
          s:='stable.gitlab';
          FManager.LazarusTag:=s;
          Options.PersistentOptions:=trim(Options.PersistentOptions+' --lazVersion="'+s+'"')
        end;
      end;
      if (Pos('github.com/LongDirtyAnimAlf',FManager.LazarusURL)>0) then FManager.LazarusBranch:='upstream';
      if (Pos('github.com/LongDirtyAnimAlf/lazarussource',FManager.LazarusURL)>0) then FManager.LazarusBranch:='master';

      FManager.LazarusDesiredRevision:=Options.GetOption('','lazRevision','',false);
      {$endif}

    except
      on E:Exception do
      begin
        {$ifndef LCL}
        writeln('Error: wrong command line options given: '+E.Message);
        {$endif}
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
      FManager.HTTPProxyHost:=Options.GetOption('','httpproxy','',true);

      // If no option specified, try environment variable
      // Note we don't save these options to persistent options -
      // they should remain part of the environment
      if (FManager.HTTPProxyHost='') and (GetEnvironmentVariable('http_proxy')<>'') then
      begin
        FManager.HTTPProxyHost:=GetEnvironmentVariable('http_proxy');
      end;

      // Strip out trailing /
      if copy(FManager.HTTPProxyHost,length(FManager.HTTPProxyHost),1)='/' then
        FManager.HTTPProxyHost:=copy(FManager.HTTPProxyHost,1,length(FManager.HTTPProxyHost)-1);

      // Extract port - search backwards to allow passwords with :
      i:=rpos(':',FManager.HTTPProxyHost);
      // Don't pick up : from any username:password segment
      if (i=0) or
        (rpos('@',FManager.HTTPProxyHost)>i) then
        if Pos('https://',FManager.HTTPProxyHost)=1 then
          FManager.HTTPProxyPort:=443
        else
          FManager.HTTPProxyPort:=8080 {seems like a good default}
      else
      begin
        FManager.HTTPProxyPort:=strtointdef(copy(FManager.HTTPProxyHost,i+1,length(FManager.HTTPProxyHost)),8080);
        FManager.HTTPProxyHost:=copy(FManager.HTTPProxyHost,1,i-1);
      end;

      // Strip out http/https
      if Pos('https://',FManager.HTTPProxyHost)=1 then
        FManager.HTTPProxyHost:=copy(FManager.HTTPProxyHost,length('https://')+1,length(FManager.HTTPProxyHost));
      if Pos('http://',FManager.HTTPProxyHost)=1 then
        FManager.HTTPProxyHost:=copy(FManager.HTTPProxyHost,length('http://')+1,length(FManager.HTTPProxyHost));

      // Extract out username/password
      // Search from ending of string to front to catch last @ in case password has @
      i:=rpos('@',FManager.HTTPProxyHost);
      if i>0 then
      begin
        FManager.HTTPProxyUser:=copy(FManager.HTTPProxyHost,1,i-1);
        FManager.HTTPProxyHost:=copy(FManager.HTTPProxyHost,i+1,length(FManager.HTTPProxyHost));
        // Extract out password
        i:=pos(':',FManager.HTTPProxyUser);
        if i>0 then
        begin
          FManager.HTTPProxyPassword:=copy(FManager.HTTPProxyUser,i+1,length(FManager.HTTPProxyUser));
          FManager.HTTPProxyUser:=copy(FManager.HTTPProxyUser,1,i-1);
        end;
      end;
    except
      on E:Exception do
      begin
        {$ifndef LCL}
        writeln('Error: wrong command line options given: '+E.Message);
        {$endif}
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
          if (FManager.ModulePublishedList.IndexOfName(LeftOverOptions.Names[iCurrentOption])<>-1) then
            case (uppercase(LeftOverOptions.ValueFromIndex[iCurrentOption])) of
              '-1','1','TRUE','YES','INSTALL','ENABLE', 'ON': begin
                if CheckIncludeModule(LeftOverOptions.Names[iCurrentOption])
                   then FManager.IncludeModules:=FManager.IncludeModules+','+LeftOverOptions.Names[iCurrentOption]
                   else FManager.SkipModules:=FManager.SkipModules+','+LeftOverOptions.Names[iCurrentOption];
                LeftOverOptions.Delete(iCurrentOption);
              end;
              '0','FALSE','NO','UNINSTALL','REMOVE','DISABLE', 'OFF': begin
                FManager.SkipModules:=FManager.SkipModules+','+LeftOverOptions.Names[iCurrentOption];
                LeftOverOptions.Delete(iCurrentOption);
              end
            else
              // Invalid option. leave LeftOverOptions[iCurrentOption] for the error handling below.
            end;
        end;
        // Fix up any added initial commas
        if copy(FManager.IncludeModules,1,1)=',' then
          FManager.IncludeModules:=copy(FManager.IncludeModules,2,Length(FManager.IncludeModules));
        if copy(FManager.SkipModules,1,1)=',' then
          FManager.SkipModules:=copy(FManager.SkipModules,2,Length(FManager.SkipModules));

        if LeftOverOptions.Count>0 then
        begin
          result:=ERROR_WRONG_OPTIONS; //Quit with error resultcode
          {$ifndef LCL}
          writeln('Error: wrong command line options given: '+LeftOverOptions.Text);
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
      if FManager.MakeDirectory<>'' then
        FManager.MakeDirectory:='';
      {$ENDIF MSWINDOWS}

      FManager.PersistentOptions:=Options.PersistentOptions;

      {$ifndef LCL}
      writeln('Options:');
      if FManager.Clean then
      begin
        writeln('Running --clean: cleaning environment.');
      end;
      if FManager.HTTPProxyHost<>'' then
      begin
        writeln('HTTP proxy host:  '+FManager.HTTPProxyHost);
        writeln('HTTP proxy port:  '+IntToStr(FManager.HTTPProxyPort));
        writeln('HTTP proxy user:  '+FManager.HTTPProxyUser);
        writeln('HTTP proxy pass:  <SECURITY:REDACTED>');
      end;
      {$IFDEF MSWINDOWS}
      // Makes no sense on other platforms
      writeln('Binutils/make dir:  '+FManager.MakeDirectory);
      {$ENDIF MSWINDOWS}
      writeln('Base directory:     '+FManager.BaseDirectory);
      writeln('Bootstrap dir:      '+FManager.BootstrapCompilerDirectory);
      writeln('FPC URL:            '+FManager.FPCURL);
      writeln('FPC options:        '+FManager.FPCOPT);
      writeln('FPC directory:      '+FManager.FPCInstallDirectory);
      {$ifndef FPCONLY}
      writeln('Lazarus URL:        '+FManager.LazarusURL);
      writeln('Lazarus options:    '+FManager.LazarusOPT);
      writeln('Lazarus directory:  '+FManager.LazarusInstallDirectory);
      {$endif}
      if FManager.KeepLocalChanges then
      begin
        writeln('Keep changes:       yes');
      end
      else
      begin
        writeln('Keep changes:       no');
      end;
      if FManager.ReApplyLocalChanges then
      begin
        writeln('Re-apply changes:   yes');
      end
      else
      begin
        writeln('Re-apply changes:   no');
      end;
      writeln('Log file name:      '+FManager.LogFileName);

      For i:=0 to FManager.ModuleEnabledList.Count-1 do
        begin
        writeln('Standard modules:   '+FManager.ModuleEnabledList[i]);
        end;
      if FManager.IncludeModules<>'' then
        writeln('Add. modules:       '+FManager.IncludeModules);

      if FManager.ExportOnly then
      begin
        writeln('');
        writeln('INFO: FPCUP will not download repos. It will only get the files !!!');
      end
      else
      begin
        writeln('');
        writeln('WARNING: FPCUP will download full repositories !!!');
      end;

      if (Pos('trunk',FManager.FPCURL)>0) {$ifndef FPCONLY}OR (Pos('trunk',FManager.LazarusURL)>0) {$endif} then
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

      if (Pos('-g ',FManager.FPCOPT)>0)
         then writeln('FPC is compiled with debug (-g) !');
      {$ifndef FPCONLY}
      if (Pos('-g ',FManager.LazarusOPT)>0)
         then writeln('Lazarus is compiled with debug (-g) !');
      {$endif}
      writeln('');

      // Remove password from output
      if FManager.HTTPProxyPassword='' then
      begin
        writeln('Effective parameters:   ');
        writeln(trim(sAllParameters));
        {$IFDEF MSWINDOWS}
        writeln('Persistent parameters (can be saved in batch file):');
        {$ELSE}
        writeln('Persistent parameters (can be saved in shell script):');
        {$ENDIF}
        writeln(trim(FManager.PersistentOptions));
      end
      else
      begin
        writeln('Note: replaced proxy password in all parameters, ');
        writeln('so output may be unreliable:');
        writeln('');
        writeln('Effective parameters:   ');
        writeln(trim(StringReplace(sAllParameters,
          FManager.HTTPProxyPassword,
          '<SECURITY:REDACTED>',
          [rfReplaceAll,rfIgnoreCase])));
        writeln('Persistent parameters:  ');
        writeln(trim(StringReplace(FManager.PersistentOptions,
          FManager.HTTPProxyPassword,
          '<SECURITY:REDACTED>',
          [rfReplaceAll,rfIgnoreCase])));
        if FManager.Verbose then
        begin
          writeln('');
          writeln('WARNING: proxy password will appear in screen output!');
          writeln('');
        end;
      end;

      // User could have specified relative paths so we're normalizing them.
      if (FManager.FPCBranch<>'') then
        writeln('Getting FPC branch: '+FManager.FPCBranch);
      {$ifndef FPCONLY}
      if (FManager.LazarusBranch<>'') then
        writeln('Getting Lazarus branch: '+FManager.LazarusBranch);
      {$endif}
      if FManager.SkipModules<>'' then
        writeln('WARNING: Skipping installation/update of '+FManager.SkipModules);
      if FManager.OnlyModules<>'' then
        writeln('WARNING: Limiting installation/update to '+FManager.OnlyModules);

      if FManager.Uninstall then
      begin
        writeln('');
        writeln('WARNING: UNINSTALLING !!!');
        writeln('');
      end
      else if FManager.Clean then
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
      {$endif LCL}
    end;
  finally
    Options.free;
  end;
end;

end.
