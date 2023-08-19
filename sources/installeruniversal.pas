unit installerUniversal;

{ Universal (external) installer unit driven by .ini file directives
Copyright (C) 2012-2013 Ludo Brands, Reinier Olislagers

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

{$mode objfpc}{$H+}

{$modeswitch advancedrecords}

{$warn 6058 off}

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils
  {$ifndef FPCONLY}
  ,updatelazconfig
  {$endif}
  ;


type
  {$ifndef FPCONLY}
  TAPkgVersion = record
  private
    FName:string;
    FFileVersion:longint;
    FMajor: integer;
    FMinor: integer;
    FRelease: integer;
  FBuild: integer;
  public
    function AsString: string;
    procedure GetVersion(alpkdoc:TConfig;key:string);
    property Name: string read FName write FName;
    property FileVersion: longint read FFileVersion write FFileVersion;
    //property Major: integer read FMajor;
    //property Minor: integer read FMinor;
    //property Release: integer read FRelease;
    //property Build: integer read FBuild;
  end;
{$endif}

  { TUniversalInstaller }

  TUniversalInstaller = class(TBaseUniversalInstaller)
  private
    FPath:string; //Path to be used within this session (e.g. including compiler path)
    InitDone:boolean;
    {$ifndef FPCONLY}
    // Compiler options chosen by user to build Lazarus. There is a CompilerOptions property,
    // but let's leave that for use with FPC.
    FLazarusCompilerOptions:string;
    // Keep track of whether Lazarus needs to be rebuilt after package installation
    // or running lazbuild with an .lpk
    FLazarusNeedsRebuild:boolean;
    FLazarusVersion:string;
    // LCL widget set to be built
    FLCL_Platform: string;
    function RebuildLazarus:boolean;
    {$endif}
  protected
    // Scans for and adds all packages specified in a (module's) stringlist with commands:
    function AddPackages(sl:TStringList): boolean;
    // Get a value for a key=value pair. Case-insensitive for keys. Expands macros in values.
    function GetValueFromKey(Key:string;sl:TStringList;recursion:integer=0):string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
    {$ifndef FPCONLY}
    // Installs a single package:
    function InstallPackage(PackagePath, WorkingDir: string; RegisterOnly:boolean; {%H-}Silent:boolean=false): boolean;
    // Scans for and removes all packages specfied in a (module's) stringlist with commands:
    function RemovePackages(sl:TStringList): boolean;
    // Uninstall a single package:
    function UnInstallPackage(PackagePath, WorkingDir: string): boolean;
    function GetVersionFromUrl({%H-}aUrl: string): string;override;
    function GetVersionFromSource: string;override;
    function GetReleaseCandidateFromSource:integer;override;
    {$endif}
    // Filters (a module's) sl stringlist and runs all <Directive> commands:
    function RunCommands(Directive:string;sl:TStringList):boolean;
  public
    {$ifndef FPCONLY}
    // Compiler options user chose to compile Lazarus with (coming from fpcup).
    property LazarusCompilerOptions: string write FLazarusCompilerOptions;
    // LCL widget set to be built
    property LCL_Platform: string write FLCL_Platform;
    property LazarusVersion:string read FLazarusVersion;
    {$endif}
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    // Configure module
    function ConfigModule(ModuleName:string): boolean; override;
    // Install/update sources (e.g. via svn)
    function GetModule(ModuleName:string): boolean; override;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; override;
  end;

  { TmORMotPXLInstaller }
  TmORMotPXLInstaller = class(TUniversalInstaller)
  public
    function BuildModule(ModuleName: string): boolean; override;
  end;

  { TAWGGInstaller }
  TAWGGInstaller = class(TUniversalInstaller)
  public
    function BuildModule(ModuleName: string): boolean; override;
  end;

  { TPas2jsInstaller }
  TPas2jsInstaller = class(TUniversalInstaller)
  public
    function BuildModule(ModuleName: string): boolean; override;
  end;

  { TInternetToolsInstaller }
  TInternetToolsInstaller = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;

  { TDeveltools4FPCInstaller }
  TDeveltools4FPCInstaller = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;

  TXTensaTools4FPCInstaller = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;

  { TMBFFreeRTOSWioInstaller }
  TMBFFreeRTOSWioInstaller = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;

  { TmORMot2Installer }
  TmORMot2Installer = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;

  { TWSTInstaller }
  TWSTInstaller = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;

  { TCEFInstaller }
  TCEFInstaller = class(TUniversalInstaller)
  public
    function GetModule(ModuleName: string): boolean; override;
  end;


  // Gets the list of modules enabled in ConfigFile. Appends to existing TStringList
  function GetModuleEnabledList(var ModuleList:TStringList):boolean;
  // Gets the sequence representation for all modules in the ini file
  // Used to pass on to higher level code for selection, display etc.
  //todo: get Description field into module list
  function GetModuleList:string;
  // gets keywords for alias in Dictionary.
  function GetKeyword(aDictionary,aAlias: string): string;
  // gets alias for keywords in Dictionary.
  //The keyword 'list' is reserved and returns the list of keywords as commatext
  function GetAlias(aDictionary,aKeyword: string): string;
  function SetAlias(aDictionary,aKeyWord,aValue: string):boolean;
  // check if enabled modules are allowed !
  function CheckIncludeModule(ModuleName: string):boolean;
  function SetConfigFile(aConfigFile: string):boolean;

var
  sequences:string;
  UniModuleList:TStringList=nil;

Const
  CONFIGFILENAME='fpcup.ini';
  SETTTINGSFILENAME='settings.ini';
  DELUXEFILENAME='fpcupdeluxe.ini';

  INIKEYWORD_NAME='Name';
  INIKEYWORD_CATEGORY='Category';
  INIKEYWORD_DESCRIPTION='Description';

implementation

uses
  StrUtils, typinfo,inifiles, process, fpjson,
  FileUtil,
  fpcuputil;

Const
  MAXSYSMODULES=300;
  MAXUSERMODULES=20;
  // Allow enough instructions per module:
  MAXINSTRUCTIONS=255;
  MAXEMPTYINSTRUCTIONS=5;
  MAXRECURSIONS=10;
  LOCATIONMAGIC='Workingdir';
  INSTALLMAGIC='Installdir';

var
  CurrentConfigFile:string;
  IniGeneralSection:TStringList=nil;
  UniModuleEnabledList:TStringlist=nil;

{$ifndef FPCONLY}
function TAPkgVersion.AsString: string;
var
  AddValues:boolean;
begin
  result:='';
  AddValues:=(FBuild>0);
  if AddValues then Result:='.'+IntToStr(FBuild)+Result else AddValues:=(FRelease>0);
  if AddValues then Result:='.'+IntToStr(FRelease)+Result else AddValues:=(FMinor>0);
  if AddValues then Result:='.'+IntToStr(FMinor)+Result;
  Result:=IntToStr(FMajor)+Result;
end;

procedure TAPkgVersion.GetVersion(alpkdoc:TConfig;key:string);
begin
  FMajor:=alpkdoc.GetValue(key+'Major',0);
  FMinor:=alpkdoc.GetValue(key+'Minor',0);
  FRelease:=alpkdoc.GetValue(key+'Release',0);
  FBuild:=alpkdoc.GetValue(key+'Build',0);
end;
{$endif}

{ TUniversalInstaller }

{$ifndef FPCONLY}
function TUniversalInstaller.RebuildLazarus:boolean;
var
  OldPath,s,s2:string;
  LazarusConfig: TUpdateLazConfig;
  i,j:integer;
begin
  result:=false;
  FLazarusNeedsRebuild:=false;

  Infoln(infotext+'Going to rebuild Lazarus.',etInfo);

  Processor.Process.Parameters.Clear;
  Processor.Process.CurrentDirectory := FLazarusSourceDir;

  {$push}
  {$warn 6018 off}

  {$ifdef FORCELAZBUILD}
  if false then
  {$else}
  if true then
  {$endif}
  begin
    Processor.Executable := Make;
    Processor.SetParamData('--directory=' + Processor.Process.CurrentDirectory);

    {$IFDEF MSWINDOWS}
    if Length(Shell)>0 then Processor.SetParamData('SHELL='+Shell);
    {$ENDIF}

    {$IF DEFINED(CPUARM) AND DEFINED(LINUX)}
    Processor.SetParamData('--jobs=1');
    {$ELSE}
    //Still not clear if jobs can be enabled for Lazarus make builds ... :-|
    //if (NOT FNoJobs) then
    //  Processor.SetParamData('--jobs='+IntToStr(FCPUCount));
    {$ENDIF}

    Processor.SetParamData('USESVN2REVISIONINC=0');

    Processor.SetParamMakefilePathData('FPC',FCompiler);
    Processor.SetParamMakefilePathData('PP',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));

    Processor.SetParamMakefilePathData('PREFIX',FLazarusInstallDir);
    Processor.SetParamMakefilePathData('INSTALL_PREFIX',FLazarusInstallDir);
    Processor.SetParamMakefilePathData('LAZARUS_INSTALL_DIR',IncludeTrailingPathDelimiter(FLazarusInstallDir));

    //Make sure our FPC units can be found by Lazarus
    Processor.SetParamMakefilePathData('FPCDIR',FFPCSourceDir);
    //Processor.SetParamMakefilePathData('FPCDIR',FFPCInstallDir);
    //Processor.SetParamMakefilePathData('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true)]));

    //Make sure Lazarus does not pick up these tools from other installs
    Processor.SetParamMakefilePathData('FPCMAKE',FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt);
    Processor.SetParamMakefilePathData('PPUMOVE',FPCBinDir+DirectorySeparator+'ppumove'+GetExeExt);

    s:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+DefaultIDEMakeOptionFilename;
    //if FileExists(s) then
      Processor.SetParamMakefilePathData('CFGFILE',s);

    {$IFDEF MSWINDOWS}
    Processor.SetParamData('UPXPROG=echo');      //Don't use UPX
    {$else}
    //Processor.SetParamMakefilePathData('INSTALL_BINDIR',FBinPath);
    {$ENDIF MSWINDOWS}

    if FLCL_Platform <> '' then Processor.SetParamData('LCL_PLATFORM=' + FLCL_Platform);

    //Set options
    s := FLazarusCompilerOptions;

    // This code is copied from the native Lazarus installer.
    // This must be improved: no code duplicated please ... ;-)
    {$ifdef Unix}
      {$ifndef Darwin}
        {$ifdef LCLQT}
        {$endif}
        {$ifdef LCLQT5}
        if LibWhich(LIBQT5,s2) then
        begin
          s:=s+' -Fl'+ExcludeTrailingPathDelimiter(s2);
        end
        else
        begin
          // Did we copy the QT5 libs ??
          // If so, add some linker help.
          if (FileExists(IncludeTrailingPathDelimiter(FLazarusInstallDir)+LIBQT5)) then
          begin
            s:=s+' -k"-rpath=./"';
            s:=s+' -k"-rpath=$$ORIGIN"';
            s:=s+' -k"-rpath=\\$$$$$\\ORIGIN"';
            s:=s+' -Fl'+FLazarusInstallDir;
          end;
        end;
        {$endif}
      {$endif}
    {$endif}

    while Pos('  ',s)>0 do
    begin
      s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
    end;
    s:=Trim(s);

    if Length(s)>0 then Processor.SetParamData('OPT='+s);

    {$ifdef DISABLELAZBUILDJOBS}
    Processor.SetParamData('LAZBUILDJOBS=1');//prevent runtime 217 errors
    {$else}
    Processor.SetParamData('LAZBUILDJOBS='+IntToStr(FCPUCount));
    {$endif}

    Processor.SetParamData('useride');

    try
      {$ifdef MSWindows}
      //Prepend FPC binary directory to PATH to prevent pickup of strange tools
      OldPath:=Processor.Environment.GetVar(PATHVARNAME);
      s:=FPCBinDir;
      if OldPath<>'' then
         Processor.Environment.SetVar(PATHVARNAME, s+PathSeparator+OldPath)
      else
        Processor.Environment.SetVar(PATHVARNAME, s);
      {$endif}

      ProcessorResult:=Processor.ExecuteAndWait;
      result := (ProcessorResult=0);
      if result then
      begin
        Infoln(infotext+'Lazarus rebuild succeeded',etDebug);
      end
      else
        WritelnLog(etError,infotext+'Failure trying to rebuild Lazarus. '+LineEnding+
          'Details: '+FErrorLog.Text,true);

      {$ifdef MSWindows}
      Processor.Environment.SetVar(PATHVARNAME, OldPath);
      {$endif}

    except
      on E: Exception do
      begin
        result:=false;
        WritelnLog(etError, infotext+'Exception trying to rebuild Lazarus '+LineEnding+
          'Details: '+E.Message,true);
      end;
    end;

  end
  else
  begin
    Processor.Executable := IncludeTrailingPathDelimiter(FLazarusInstallDir)+LAZBUILDNAME+GetExeExt;

    OldPath:=Processor.Environment.GetVar('FPCDIR');
    //Processor.Environment.SetVar('FPCDIR',ConcatPaths([FFPCInstallDir,'units',GetFPCTarget(true)]));
    Processor.Environment.SetVar('FPCDIR',FFPCSourceDir);
    {$IFDEF DEBUG}
    Processor.SetParamData('--verbose');
    {$ELSE}
    // See compileroptions.pp
    // Quiet:=ConsoleVerbosity<=-3;
    Processor.SetParamData('--quiet');
    {$ENDIF}

    {$ifdef DISABLELAZBUILDJOBS}
    Processor.SetParamData('--max-process-count=1');
    {$else}
    Processor.SetParamData('--max-process-count='+IntToStr(FCPUCount));
    {$endif}

    Processor.SetParamData('--pcp=' + DoubleQuoteIfNeeded(FLazarusPrimaryConfigPath));
    Processor.SetParamData('--cpu=' + GetSourceCPU);
    Processor.SetParamData('--os=' + GetSourceOS);

    if FLCL_Platform <> '' then
      Processor.SetParamData('--ws=' + FLCL_Platform);

    Processor.SetParamData('--build-ide="-dKeepInstalledPackages '+FLazarusCompilerOptions+'"');
    //Processor.SetParamData('--build-ide="'+FLazarusCompilerOptions+'"');
    //Processor.SetParamData('--build-mode="Normal IDE"');

    Infoln(infotext+'Running lazbuild to get IDE with user-specified packages', etInfo);
    try
      ProcessorResult:=Processor.ExecuteAndWait;
      result := (ProcessorResult=0);

      //Restore FPCDIR environment variable ... could be trivial, but better safe than sorry
      Processor.Environment.SetVar('FPCDIR',OldPath);
      if (NOT result) then
      begin
        WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' returned error code ' + IntToStr(ProcessorResult) + LineEnding +
          'Details: ' + FErrorLog.Text, true);
      end;
    except
      on E: Exception do
      begin
        result := false;
        WritelnLog(etError, infotext+'Exception running '+ExtractFileName(Processor.Executable)+' to get IDE with user-specified packages!' + LineEnding +
          'Details: ' + E.Message, true);
      end;
    end;

  end;
  {$pop}

  //We now have, for certain, a miscellaneousoptions.xml file.
  //This file has been generated by lazbuild..
  //Edit it to reflect our own settings, if needed.
  LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
  try
    i:=LazarusConfig.GetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Count',0);
    if i>0 then
    begin
      // Change the build modes to reflect the options set.
      j:=LazarusConfig.GetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Profile0/Options/Count', 0);
      s:=Trim(FLazarusCompilerOptions);
      if ((j=0) AND (Length(s)>0)) then
      begin
        LazarusConfig.SetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Profile0/Options/Count', 1);
        LazarusConfig.SetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Profile0/Options/Item1/Value', Trim(FLazarusCompilerOptions));
      end;
      if Length(FLCL_Platform)>0 then
      begin
        // Change the build modes to reflect the default LCL widget set.
        for j:=0 to (i-1) do
        begin
          Infoln(infotext+'Changing default LCL_platforms for build-profiles in '+MiscellaneousConfig+' to build for '+FLCL_Platform, etInfo);
          LazarusConfig.SetVariable(MiscellaneousConfig, 'MiscellaneousOptions/BuildLazarusOptions/Profiles/Profile'+InttoStr(j)+'/LCLPlatform/Value', FLCL_Platform);
        end;
      end;
    end;
  finally
    LazarusConfig.Free;
  end;
end;

{$endif}

function TUniversalInstaller.GetValueFromKey(Key: string; sl: TStringList;
  recursion: integer): string;
// Look for entries with Key and process macros etc in value
var
  i,len:integer;
  s,macro:string;
  doublequote:boolean;
begin
  Key:=UpperCase(Key);
  s:='';
  if recursion=MAXRECURSIONS then
    exit;
  for i:=0 to sl.Count-1 do
    begin
    s:=sl[i];
    if (copy(UpperCase(s),1, length(Key))=Key) and ((s[length(Key)+1]='=') or (s[length(Key)+1]=' ')) then
      begin
      if Pos('=',s)>0 then
        s:=trim(copy(s,pos('=',s)+1,length(s)));
      break;
      end;
    s:='';
    end;
  if s='' then //search general section
    for i:=0 to IniGeneralSection.Count-1 do
      begin
      s:=IniGeneralSection[i];
      if (copy(UpperCase(s),1, length(Key))=Key) and ((s[length(Key)+1]='=') or
        (s[length(Key)+1]=' ')) then
        begin
        if Pos('=',s)>0 then
          s:=trim(copy(s,pos('=',s)+1,length(s)));
        break;
        end;
      s:='';
      end;
//expand macros
  doublequote:=true;
  if s<>'' then
    while Pos('$(',s)>0 do
    begin
      i:=pos('$(',s);
      macro:=copy(s,i+2,length(s));
      if Pos(')',macro)>0 then
        begin
        delete(macro,pos(')',macro),length(macro));
        macro:=UpperCase(macro);
        len:=length(macro)+3; // the brackets
        // For the directory macros, the user expects to add path separators himself in fpcup.ini,
        // so strip them out if they are there.
        if macro='BASEDIR' then
          macro:=BaseDirectory
        else if macro='FPCDIR' then
          macro:=FFPCInstallDir
        else if macro='FPCBINDIR' then
            macro:=FPCBinDir
        else if macro='FPCBIN' then
            macro:=FCompiler
        else if macro='TOOLDIR' then
          {$IFDEF MSWINDOWS}
          // make is a binutil and should be located in the make dir
          macro:=FMakeDir
          {$ENDIF}
          {$IFDEF UNIX}
          // Strip can be anywhere in the path
          macro:=ExtractFileDir(Which('make'))
          {$ENDIF}
        else if macro='GETEXEEXT' then
          macro:=GetExeExt
        {$ifndef FPCONLY}
        else if macro='LAZARUSDIR' then
          macro:=FLazarusInstallDir
        else if macro='LAZARUSPRIMARYCONFIGPATH' then
          macro:=FLazarusPrimaryConfigPath
        {$endif}
        else if macro='STRIPDIR' then
          {$IFDEF MSWINDOWS}
          // Strip is a binutil and should be located in the make dir
          macro:=FMakeDir
          {$ENDIF}
          {$IFDEF UNIX}
          // Strip can be anywhere in the path
          macro:=ExtractFileDir(Which('strip'))
          {$ENDIF}
        else if macro='REMOVEDIRECTORY' then
        begin
          doublequote:=false;
          {$IFDEF MSWINDOWS}
          macro:='cmd /c rmdir /s /q';
          {$ENDIF}
          {$IFDEF UNIX}
          macro:='rm -Rf';
          {$ENDIF}
        end
        else if macro='TERMINAL' then
        begin
          doublequote:=false;
          {$ifdef  MSWINDOWS}
          macro:=GetEnvironmentVariable('COMSPEC');
          if NOT FileExists(macro) then macro:='c:\windows\system32\cmd.exe';
          if NOT FileExists(macro) then macro:='' else macro:=macro+' /c';
          {$endif  MSWINDOWS}
          {$ifdef UNIX}
          macro := '/bin/sh';
          if NOT FileExists(macro) then macro:='' else macro:=macro+' -c';
          {$endif UNIX}
        end
        else if macro='REMOVEINSTALLDIRECTORY' then
        begin
          doublequote:=false;
          {$IFDEF MSWINDOWS}
          macro:='cmd /c rmdir '+'$(Installdir)'+' /s /q';
          {$ENDIF}
          {$IFDEF UNIX}
          macro:='rm -Rf '+'$(Installdir)';
          {$ENDIF}
        end
        else macro:=GetValueFromKey(macro,sl,recursion+1); //user defined value
        // quote if containing spaces
        if doublequote then
        begin
          //if Pos(' ',macro)>0 then macro:='"'+macro+'"';
          macro:=MaybeQuoted(macro);
        end;
        delete(s,i,len);
        insert(macro,s,i);
      end;
    end;
  // correct path delimiter
  if (pos('URL',Key)<=0) and (pos('ADDTO',Key)<>1)then
  begin
    {$IFDEF MSWINDOWS}
    len:=2;
    {$ELSE}
    len:=1;
    {$ENDIF}
    //DoDirSeparators(s);
    for i:=len to length(s) do
      if (s[i] in ['/','\']){$IFDEF MSWINDOWS} AND (s[i-1]<>' '){$ENDIF} then
        s[i]:=DirectorySeparator;
  end;
  result:=s;
end;

function TUniversalInstaller.InitModule: boolean;
begin
  result:=true;
  localinfotext:=InitInfoText(' (InitModule): ');

  Infoln(localinfotext+'Entering ...',etDebug);
  if InitDone then exit;

  // While getting svn etc may help a bit, if Lazarus isn't installed correctly,
  // it probably won't help for normal use cases.
  // However, in theory, we could run only external modules and
  // only download some SVN repositories
  // So.. enable this.
  result:=(CheckAndGetTools) AND (CheckAndGetNeededBinUtils);

  if not(result) then
    Infoln(localinfotext+'Missing required executables. Aborting.',etError);

  // Need to remember because we don't always use ProcessEx
  FPath:=FPCBinDir+PathSeparator+
  {$IFDEF MSWINDOWS}
  FMakeDir+PathSeparator+
  {$ENDIF MSWINDOWS}
  {$IFDEF DARWIN}
  // pwd is located in /bin ... the makefile needs it !!
  // tools are located in /usr/bin ... the makefile needs it !!
  // don't ask, but this is needed when fpcupdeluxe runs out of an .app package ... quirk solved this way .. ;-)
  '/bin'+PathSeparator+'/usr/bin'+PathSeparator+
  {$ENDIF}
  ExcludeTrailingPathDelimiter(FFPCInstallDir)+PathSeparator;
  SetPath(FPath,true,false);
  // No need to build Lazarus IDE again right now; will
  // be changed by buildmodule/configmodule installexecute/
  // installpackage
  {$ifndef FPCONLY}
  FLazarusNeedsRebuild:=false;
  FLazarusVersion:=GetVersion;
  {$endif}

  InitDone:=result;
end;

{$ifndef FPCONLY}
function TUniversalInstaller.InstallPackage(PackagePath, WorkingDir: string; RegisterOnly:boolean; Silent:boolean=false): boolean;
var
  PackageName,PackageAbsolutePath: string;
  Path: String;
  lpkdoc:TConfig;
  lpkversion:TAPkgVersion;
  TxtFile:TextFile;
  RegisterPackageFeature:boolean;
  i,ReqCount:integer;
  ReqPackage:string;
  PackageFiles: TStringList;
begin
  result:=false;
  localinfotext:=InitInfoText(' (InstallPackage): ');

  PackageName:=FileNameWithoutExt(PackagePath);

  // Convert any relative path to absolute path, if it's not just a file/package name:
  if ExtractFileName(PackagePath)=PackagePath then
    // we have a relative path or packagename: let Lazarus handle it
    PackageAbsolutePath:=PackagePath
  else
   // Just use absolute path
    PackageAbsolutePath:=SafeExpandFileName(PackagePath);

  // find a package component, if any
  // all other packages will be ignored
  if ( (NOT FileExists(PackageAbsolutePath)) AND (Length(WorkingDir)>0) AND DirectoryExists(WorkingDir) ) then
  begin
    PackageFiles:=FindAllFiles(WorkingDir, PackageName+'.lpk' , true);
    if PackageFiles.Count>0 then PackageAbsolutePath:=PackageFiles.Strings[0];
    PackageFiles.Free;
  end;

  // find a Lazarus component, if any
  // all other packages will be ignored
  if (NOT FileExists(PackageAbsolutePath)) then
  begin
    PackageFiles:=FindAllFiles(IncludeTrailingPathDelimiter(FLazarusInstallDir)+'components', PackageName+'.lpk' , true);
    if PackageFiles.Count>0 then PackageAbsolutePath:=PackageFiles.Strings[0];
    PackageFiles.Free;
  end;

  // find an OPM component, if any
  // all other packages will be ignored
  if (NOT FileExists(PackageAbsolutePath)) then
  begin
    PackageFiles:=FindAllFiles(IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+'onlinepackagemanager'+DirectorySeparator+'packages', PackageName+'.lpk' , true);
    if PackageFiles.Count>0 then PackageAbsolutePath:=PackageFiles.Strings[0];
    PackageFiles.Free;
  end;

  // find a fpcupdeluxe ccr component, if any
  // all other packages will be ignored
  {
  Path:=IncludeTrailingPathDelimiter(BaseDirectory)+'ccr';
  if ( (NOT FileExists(PackageAbsolutePath)) AND DirectoryExists(Path) ) then
  begin
    PackageFiles:=FindAllFiles(Path, PackageName+'.lpk' , true);
    if PackageFiles.Count>0 then PackageAbsolutePath:=PackageFiles.Strings[0];
    PackageFiles.Free;
  end;
  }

  lpkversion.Name:='unknown';
  if FileExists(PackageAbsolutePath) then
  begin
    lpkdoc:=TConfig.Create(PackageAbsolutePath);
    try
      // if only a filename (without path) is given, then lazarus will handle everything by itself
      // set lpkversion.Name to 'unknown' to flag this case
      // if not, get some extra info from package file !!
      if (ExtractFileName(PackagePath)<>PackagePath) then
      begin
        Path:='Package/';
        lpkversion.FileVersion:=lpkdoc.GetValue(Path+'Version',0);
        Path:='Package/Name/';
        lpkversion.Name:=lpkdoc.GetValue(Path+'Value','unknown');
        Path:='Package/Version/';
        lpkversion.GetVersion(lpkdoc,Path);
      end;

      // get package requirements
      Path:='Package/RequiredPkgs/';
      ReqCount:=lpkdoc.GetValue(Path+'Count',0);
      for i:=1 to ReqCount do
      begin
        Path:='Package/RequiredPkgs/';
        ReqPackage:=lpkdoc.GetValue(Path+'Item'+InttoStr(i)+'/PackageName/Value','unknown');
        // try to auto-resolve dependencies, but skip trivial packages
        // not very elegant, but working
        if (ReqPackage<>'unknown') AND
           (ReqPackage<>'LCL') AND
           (ReqPackage<>'LazControls') AND
           (ReqPackage<>'IDEIntf') AND
           (ReqPackage<>'FCL') AND
           (ReqPackage<>'LCLBase') AND
           (ReqPackage<>'LazControlDsgn') AND
           (ReqPackage<>'LazUtils') AND
           (ReqPackage<>'cairocanvas_pkg') AND
           (ReqPackage<>'SynEdit') AND
           (ReqPackage<>'DebuggerIntf') AND
           (ReqPackage<>'LazDebuggerGdbmi') AND
           (ReqPackage<>'CodeTools') then
        begin
          InstallPackage(ReqPackage, WorkingDir, RegisterOnly, true);
        end;
      end;
    finally
      lpkdoc.Free;
    end;
  end;

  //if (NOT Silent){ OR FVerbose} then
  begin
    if lpkversion.Name='unknown'
       then WritelnLog(localinfotext+'Installing '+PackageName,True)
       else WritelnLog(localinfotext+'Installing '+PackageName+' version '+lpkversion.AsString,True);
  end;

  Processor.Executable := IncludeTrailingPathDelimiter(FLazarusInstallDir)+LAZBUILDNAME+GetExeExt;

  RegisterPackageFeature:=false;
  // get lazbuild version to see if we can register packages (available from version 1.7 and up)
  Processor.Process.Parameters.Clear;
  Processor.SetParamData('--version');
  try
    ProcessorResult:=Processor.ExecuteAndWait;
    result := (ProcessorResult=0);
    if result then
    begin
      if Processor.WorkerOutput.Count>0 then
        RegisterPackageFeature:=(CalculateNumericalVersion(Processor.WorkerOutput.Strings[Processor.WorkerOutput.Count-1])>=CalculateFullVersion(1,7,0));
    end;
  except
    on E: Exception do
    begin
      result:=false;
      WritelnLog(localinfotext+'Exception trying to getting lazbuild version. Details: '+E.Message,true);
    end;
  end;

  if (NOT result) then
  begin
    WritelnLog(localinfotext+'Error trying to add package '+PackageName,true);
    exit;
  end;

  if RegisterPackageFeature then RegisterPackageFeature:=RegisterOnly;

  Processor.Process.Parameters.Clear;
  FErrorLog.Clear;
  if WorkingDir<>'' then
    Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(WorkingDir);
  Processor.Process.Parameters.Clear;
  {$IFDEF DEBUG}
  Processor.SetParamData('--verbose');
  {$ELSE}
  Processor.SetParamData('--quiet');
  {$ENDIF}

  Processor.SetParamData('--pcp=' + DoubleQuoteIfNeeded(FLazarusPrimaryConfigPath));
  Processor.SetParamData('--cpu=' + GetSourceCPU);
  Processor.SetParamData('--os=' + GetSourceOS);
  if FLCL_Platform <> '' then
            Processor.SetParamData('--ws=' + FLCL_Platform);
  if RegisterPackageFeature then
    Processor.SetParamData('--add-package-link')
  else
    Processor.SetParamData('--add-package');
  Processor.SetParamData(DoubleQuoteIfNeeded(PackageAbsolutePath));
  try
    ProcessorResult:=Processor.ExecuteAndWait;
    result := (ProcessorResult=0);

    // runtime packages will return false, but output will have info about package being "only for runtime"
    if result then
    begin
      if (NOT RegisterPackageFeature) then
      begin
        Infoln('Marking Lazarus for rebuild based on package install for '+PackageAbsolutePath,etDebug);
        FLazarusNeedsRebuild:=true; //Mark IDE for rebuild
      end;
    end
    else
    begin
      // if the package is only for runtime, just add an lpl file to inform Lazarus of its existence and location ->> set result to true
      if (Pos('only for runtime',Processor.WorkerOutput.Text)>0) OR (RegisterPackageFeature) OR (ProcessorResult=4)
         then result:=True
         else WritelnLog(localinfotext+'Error trying to add package '+PackageName+'. Details: '+FErrorLog.Text,true);
    end;
  except
    on E: Exception do
    begin
      WritelnLog(localinfotext+'Exception trying to add package '+PackageName+'. Details: '+E.Message,true);
    end;
  end;

  // all ok AND a filepath is given --> check / add lpl file to inform Lazarus of package excistence and location
  // if only a filename (without path) is given, then lazarus will handle everything (including lpl) by itself
  // in fact, we cannot do anything in that case : we do not know anything about the package !
  if (result) AND (lpkversion.Name<>'unknown') then
  begin
    if FVerbose then WritelnLog(localinfotext+'Checking lpl file for '+PackageName,true);
    Path := ConcatPaths([FLazarusInstallDir,'packager','globallinks'])+DirectorySeparator+LowerCase(lpkversion.Name)+'-'+lpkversion.AsString+'.lpl';
    if NOT FileExists(Path) then
    begin
      AssignFile(TxtFile,Path);
      try
        Rewrite(TxtFile);
        writeln(TxtFile,PackageAbsolutePath);
      finally
        CloseFile(TxtFile);
      end;
      if FVerbose then WritelnLog(localinfotext+'Created lpl file ('+Path+') with contents: '+PackageAbsolutePath,true);
    end;
  end;
end;

function TUniversalInstaller.RemovePackages(sl: TStringList): boolean;
const
  // The command that will be processed:
  Directive='AddPackage';
var
  Failure: boolean;
  i:integer;
  RealDirective:string;
  PackagePath:string;
  Workingdir:string;
  BaseWorkingdir:string;
  RegisterOnly:boolean;
begin
  Failure:=false;
  localinfotext:=InitInfoText(' (RemovePackages): ');

  BaseWorkingdir:=GetValueFromKey(LOCATIONMAGIC,sl);
  if BaseWorkingdir='' then BaseWorkingdir:=GetValueFromKey(INSTALLMAGIC,sl);
  BaseWorkingdir:=FixPath(BaseWorkingdir);

  Workingdir:=BaseWorkingdir;

  for RegisterOnly:=false to true do
  begin

    // Go backward; reverse order to deal with any dependencies
    for i:=MAXINSTRUCTIONS downto -1 do
    begin
      if RegisterOnly then
        RealDirective:=Directive+'Link'
      else
        RealDirective:=Directive;

      if i>=0 then
      begin
        RealDirective:=RealDirective+IntToStr(i);
        Workingdir:=GetValueFromKey(LOCATIONMAGIC+IntToStr(i),sl);
        Workingdir:=FixPath(Workingdir);
      end else
      begin
        Workingdir:=BaseWorkingdir;
      end;

      PackagePath:=GetValueFromKey(RealDirective,sl);
      PackagePath:=FixPath(PackagePath);

      // Skip over missing data or if no AddPackage is defined
      if (PackagePath='') then continue;

      if Workingdir='' then Workingdir:=BaseWorkingdir;

      if NOT FileExists(PackagePath) then
      begin
        Infoln(localinfotext+'Package '+ExtractFileName(PackagePath)+' not found ... skipping.',etInfo);
        UnInstallPackage(PackagePath, WorkingDir);
        continue;
      end;
      // Try to uninstall everything, even if some of these fail.
      if UnInstallPackage(PackagePath, WorkingDir)=false then Failure:=true;
    end;
    result:=Failure;
  end;
end;
{$endif}

function TUniversalInstaller.AddPackages(sl:TStringList): boolean;
const
  // The command that will be processed:
  Directive='AddPackage';
  NAMEMAGIC='Name';
var
  i:integer;
  s,s2:string;
  PackagePath:string;
  ModuleName:string;
  Workingdir:string;
  BaseWorkingdir:string;
  RealDirective:string;
  RegisterOnly:boolean;
  ReadyCounter:integer;
  {$ifndef FPCONLY}
  LazarusConfig:TUpdateLazConfig;
  LegacyList:boolean;
  ListCount:integer;
  {$endif}
begin
  BaseWorkingdir:=GetValueFromKey(LOCATIONMAGIC,sl);
  if BaseWorkingdir='' then BaseWorkingdir:=GetValueFromKey(INSTALLMAGIC,sl);;
  BaseWorkingdir:=FixPath(BaseWorkingdir);
  Workingdir:=BaseWorkingdir;
  ModuleName:=GetValueFromKey(NAMEMAGIC,sl);

  localinfotext:=InitInfoText(' (AddPackages of '+ModuleName+'): ');

  for RegisterOnly:=false to true do
  begin
    //Reset ready counter
    ReadyCounter:=0;

    // trick: run from -1 to allow the above basic statements to be processed first
    for i:=-1 to MAXINSTRUCTIONS do
    begin
      if RegisterOnly then
        RealDirective:=Directive+'Link'
      else
        RealDirective:=Directive;

      if (i>=0) then
      begin
        RealDirective:=RealDirective+IntToStr(i);
        Workingdir:=GetValueFromKey(LOCATIONMAGIC+IntToStr(i),sl);
        Workingdir:=FixPath(Workingdir);
      end
      else
      begin
        Workingdir:=BaseWorkingdir;
      end;

      PackagePath:=GetValueFromKey(RealDirective,sl);
      PackagePath:=FixPath(PackagePath);

      if Workingdir='' then Workingdir:=BaseWorkingdir;

      {$ifndef FPCONLY}
      if (LowerCase(ModuleName)='lamw-gradle') then
      begin
        //perform some auto magic install stuff
        s:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+'LAMW.ini';
        with TIniFile.Create(s) do
        try
          s:=ExcludeTrailingPathDelimiter(WorkingDir);
          s2:=ReadString('NewProject','PathToGradle','');
          if (s<>s2) then WriteString('NewProject','PathToGradle',s);
        finally
          Free;
        end;
      end;
      if (LowerCase(ModuleName)='lamw-ant') then
      begin
        //perform some auto magic install stuff
        s:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+'LAMW.ini';
        with TIniFile.Create(s) do
        try
          s:=ConcatPaths([WorkingDir,'bin']);
          s2:=ReadString('NewProject','PathToAntBin','');
          if (s<>s2) then WriteString('NewProject','PathToAntBin',s);
        finally
          Free;
        end;
      end;
      {$endif}

      //Limit iterration;
      if ReadyCounter>MAXEMPTYINSTRUCTIONS then break;

      // Skip over missing data or if no AddPackage is defined
      if (PackagePath='') then
      begin
        Inc(ReadyCounter);
        continue;
      end else ReadyCounter:=0;

      if NOT FileExists(PackagePath) then
      begin
        Infoln(localinfotext+'Package '+ExtractFileName(PackagePath)+' not found ... skipping.',etInfo);
        {$ifndef FPCONLY}
        UnInstallPackage(PackagePath,Workingdir);
        {$endif}
        continue;
      end;

      //Suggested packages are added by fpcupdeluxe itself
      //So, take responsibility of correct install
      //All other packages are users responsibility !

      if ModuleName=_SUGGESTED then
      begin

        {$ifdef Darwin}
        {$ifdef CPUX64}

        // some packages are not suitable [yet] for Darwin x64 !
        // so skip them in case they are included.
        if
          (Pos('editormacroscript',PackagePath)>0) then
        begin
          Infoln(localinfotext+'Incompatible package '+ExtractFileName(PackagePath)+' skipped.',etInfo);
          continue;
        end;

        {$endif CPUX64}
        {$else}
        {$ifdef BSD}
        // these packages are not suitable for OpenBSD: their FPC units are not included by default !
        // so skip them in case they are included.
        if (
            (Pos('sqldblaz',PackagePath)>0)
            OR
            (Pos('dbflaz',PackagePath)>0)
            OR
            (Pos('leakview',PackagePath)>0)
            OR
            (Pos('lazdatadict',PackagePath)>0)
            OR
            (Pos('lazdbexport',PackagePath)>0)
           ) then
        begin
          Infoln(localinfotext+'Incompatible package '+ExtractFileName(PackagePath)+' skipped.',etInfo);
          continue;
        end;
        {$endif}

        {$endif Darwin}

        {$if (NOT defined(CPUI386)) AND (NOT defined(CPUX86_64)) AND (NOT defined(CPUARM))}
        // the package PascalScript is only suitable for i386, x86_64 and arm !
        // so skip in case package was included.
        if (Pos('pascalscript',PackagePath)>0) then
        begin
          Infoln(localinfotext+'Incompatible package '+ExtractFileName(PackagePath)+' skipped.',etInfo);
          continue;
        end;
        {$endif}

        {$if (NOT defined(CPUI386)) AND (NOT defined(CPUX86_64)) AND (NOT defined(CPUARM))}
        // the package macroscript (depending on PascalScript) is only suitable for i386, x86_64 and arm !
        // so skip in case package was included.
        if (Pos('editormacroscript',PackagePath)>0) then
        begin
          Infoln(localinfotext+'Incompatible package '+ExtractFileName(PackagePath)+' skipped.',etInfo);
          continue;
        end;
        {$endif}

        {
        if (NOT FileExists(PackagePath)) OR (PackagePath='') then
        begin
          for j:=0 to sl.Count-1 do
          begin
            if (Pos(RealDirective+'=',StrUtils.DelSpace(sl[j]))>0) then
            begin
              sl.Delete(j);
              break;
            end;
          end;
          continue;
        end;
        }

      end;

      if Workingdir='' then Workingdir:=BaseWorkingdir;

      {$ifndef FPCONLY}

      if (LowerCase(ModuleName)='fpdebug') then
      begin
        // only install fpdebug on windows and linux intel.
        {$if NOT defined(MSWindows) AND NOT defined(Linux)}
        continue;
        {$else}
        {$if NOT defined(CPUX64) AND NOT defined(CPUX86)}
        continue;
        {$endif}
        {$endif}

        // only install fpdebug on Lazarus 2.2.0 and better.
        // ALF
        if (CompareVersionStrings(LazarusVersion,'2.2.0')<0) then
        begin
          WritelnLog(localinfotext+'Skipping '+ModuleName, True);
          continue;
        end;
      end;

      result:=InstallPackage(PackagePath,WorkingDir,RegisterOnly);
      if not result then
      begin
        Infoln(localinfotext+'Error while installing package '+PackagePath+'.',etWarning);
        if FVerbose then WritelnLog(localinfotext+'Error while installing package '+PackagePath+'.',false);
        break;
      end;

      if result then
      begin

        if (LowerCase(ModuleName)='fpdebug') then
        begin
          LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
          try
            s2:='EnvironmentOptions/Debugger/Configs/';
            LegacyList:=LazarusConfig.IsLegacyList(EnvironmentConfig, s2);
            ListCount:=LazarusConfig.GetListItemCount(EnvironmentConfig, s2, 'Config', LegacyList);

            // Lookup previous setting, if any
            while (ListCount>0) do
            begin
              Dec(ListCount);
              s:=s2+LazarusConfig.GetListItemXPath(EnvironmentConfig,'Config',ListCount,LegacyList,True)+'/';
              if AnsiSameText('TFpDebugDebugger',LazarusConfig.GetVariable(EnvironmentConfig, s+'ConfigClass')) then
              begin
                // Previous config setting found. Set flag.
                ListCount:=-1;
                break;
              end;
            end;

            // Flag not set. Nothing found. Add new config.
            if (ListCount<>-1) then
            begin
              // Add fpdebug settings and activate
              ListCount:=LazarusConfig.GetListItemCount(EnvironmentConfig, s2, 'Config', LegacyList);
              s:=s2+LazarusConfig.GetListItemXPath(EnvironmentConfig,'Config',ListCount,LegacyList,True)+'/';
              LazarusConfig.SetVariable(EnvironmentConfig, s+'ConfigName', 'FpDebug');
              LazarusConfig.SetVariable(EnvironmentConfig, s+'ConfigClass', 'TFpDebugDebugger');
              // Activate fpdebug
              LazarusConfig.SetVariable(EnvironmentConfig, s+'Active',True);
              // Deativate other debugger(s)
              while (ListCount>0) do
              begin
                Dec(ListCount);
                s:=s2+LazarusConfig.GetListItemXPath(EnvironmentConfig,'Config',ListCount,LegacyList,True)+'/';
                LazarusConfig.SetVariable(EnvironmentConfig, s+'Active',False);
              end;
            end;
          finally
            LazarusConfig.Free;
          end;
        end;

        if (LowerCase(ModuleName)='lamw') AND (Pos('amw_ide_tools',LowerCase(PackagePath))>0) then
        begin
          //perform some auto magic install stuff
          s:=IncludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)+'LAMW.ini';
          with TIniFile.Create(s) do
          try
            s:='PathToSmartDesigner';
            s2:=ReadString('NewProject',s,ConcatPaths([WorkingDir,'android_wizard','smartdesigner']));
            WriteString('NewProject',s,s2);

            s:='PathToJavaTemplates';
            s2:=ReadString('NewProject',s,ConcatPaths([WorkingDir,'android_wizard','smartdesigner','java']));
            WriteString('NewProject',s,s2);

            s:='PathToJavaJDK';
            s2:=ReadString('NewProject',s,SafeExpandFileName(ExtractFilePath(GetJavac)+'..'));
            if Length(s2)>0 then WriteString('NewProject',s,s2);

            s:='PathToWorkspace';
            s2:=ReadString('NewProject',s,ConcatPaths([BaseDirectory,'projects','LAMWProjects']));
            ForceDirectoriesSafe(s2);
            WriteString('NewProject',s,s2);

            s:='InstructionSet';
            s2:=ReadString('NewProject',s,'2');
            WriteString('NewProject',s,s2);

            s:='PathToAndroidSDK';
            s2:=GetAndroidSDKDir;
            s2:=ReadString('NewProject',s,s2);
            if DirectoryExists(s2) then WriteString('NewProject',s,s2);

            s:='PathToAndroidNDK';
            s2:=GetAndroidNDKDir;
            s2:=ReadString('NewProject',s,s2);
            if DirectoryExists(s2) then WriteString('NewProject',s,s2);

          finally
            Free;
          end;
        end;
      end;

      {$endif}
    end;
  end;

end;

function TUniversalInstaller.RunCommands(Directive: string;sl:TStringList): boolean;
var
  i,j:integer;
  exec:string;
  s:string;
  BaseWorkingdir:string;
  Workingdir:string;
  ReadyCounter:integer;
begin
  localinfotext:=InitInfoText(' (RunCommands: '+Directive+'): ');

  result:=true; //not finding any instructions at all should not be a problem.
  BaseWorkingdir:=GetValueFromKey('Workingdir',sl);
  BaseWorkingdir:=FixPath(BaseWorkingdir);

  ReadyCounter:=0;

  for i:=0 to MAXINSTRUCTIONS do
  begin
    if i=0
       then exec:=GetValueFromKey(Directive,sl)
       else exec:=GetValueFromKey(Directive+IntToStr(i),sl);

    //Limit iterration;
    if ReadyCounter>MAXEMPTYINSTRUCTIONS then break;

    // Skip over missing data or if no exec is defined
    if (exec='') then
    begin
      Inc(ReadyCounter);
      continue;
    end else ReadyCounter:=0;

    exec:=FixPath(exec);

    if (Pos('fpgui',exec)>0) then
     begin
       {$ifdef MSWindows}
       if (Pos('x11',exec)>0) then continue;
       {$else}
       if (Pos('gdi',exec)>0) then continue;
       {$endif}
     end;

    {$ifndef FPCONLY}
    j:=Pos(LAZBUILDNAME,lowerCase(exec));
    if j>0 then
    begin
      {$IFDEF MSWINDOWS}
      j:=Pos(LAZBUILDNAME+GetExeExt,lowerCase(exec));
      if (j<1) then exec:=StringReplace(exec,LAZBUILDNAME,LAZBUILDNAME+GetExeExt,[rfIgnoreCase]);
      {$ENDIF}

      //Set lazbuild options
      {$IFDEF DEBUG}
      s:='--verbose';
      {$ELSE}
      s:='--quiet';
      {$ENDIF}

      if FLCL_Platform<>'' then s:=s+' --ws=' + FLCL_Platform;
      exec:=StringReplace(exec,LAZBUILDNAME+GetExeExt,LAZBUILDNAME+GetExeExt+' '+s,[rfIgnoreCase]);
    end;
    {$endif}

    Workingdir:=GetValueFromKey('Workingdir'+IntToStr(i),sl);
    Workingdir:=FixPath(Workingdir);
    if Workingdir='' then Workingdir:=BaseWorkingdir;

    try
      s:='';
      result:=false;
      j:=-1;

      Processor.Process.Parameters.Clear;
      CommandToList(exec,Processor.Process.Parameters);
      If Processor.Process.Parameters.Count>0 then
      begin
        Processor.Executable:=Processor.Process.Parameters[0];
        Processor.Process.Parameters.Delete(0);
      end;

      Infoln(localinfotext+'Running command. '+Processor.GetExeInfo,etInfo);

      ProcessorResult:=Processor.ExecuteAndWait;
      s:=Processor.WorkerOutput.Text;
      j:=ProcessorResult;

      if j=0 then
      begin
        result:=true;
        {$ifndef FPCONLY}
        // If it is likely user used lazbuid to compile a package, assume
        // it is design-time (except when returning an runtime message) and mark IDE for rebuild
        if (pos(LAZBUILDNAME+GetExeExt,lowerCase(exec))>0) and
          (pos('.lpk',lowercase(exec))>0) and
          (pos('only for runtime',lowercase(s))=0)
        then
        begin
          Infoln(localinfotext+'Marking Lazarus for rebuild based on exec line '+exec,etDebug);
          FLazarusNeedsRebuild:=true;
        end;
        {$endif}
      end
      else
      begin
        WritelnLog(etWarning, localinfotext+'Running '+exec+' returned with an error.',true);
        WritelnLog(etWarning, localinfotext+'Error-code: '+InttoStr(j),true);
        if Length(s)>0 then WritelnLog(etWarning, localinfotext+'Error message: '+s,true);
      end;
    except
      on E: Exception do
      begin
        WritelnLog(etError, localinfotext+'Exception trying to execute '+exec+LineEnding+'Details: '+E.Message,true);
      end;
    end;
  end;
end;

{$ifndef FPCONLY}
function TUniversalInstaller.GetVersionFromSource:string;
var
  aFileName:string;
begin
  result:='0.0.0';
  aFileName:=IncludeTrailingPathDelimiter(FLazarusInstallDir) + LAZBUILDNAME + GetExeExt;
  if FileExists(aFileName) then
  begin
    Processor.Executable := aFileName;
    Processor.Process.Parameters.Clear;
    Processor.SetParamData('--version');
    try
      ProcessorResult:=Processor.ExecuteAndWait;
      if ProcessorResult = 0 then
      begin
        if Processor.WorkerOutput.Count>0 then
        begin
          // lazbuild outputs version info as last line
          result:=Processor.WorkerOutput.Strings[Processor.WorkerOutput.Count-1];
        end;
      end;
    except
      on E: Exception do
      begin
        WritelnLog(etError, infotext+'Getting lazbuild version info failed with an exception!'+LineEnding+'Details: '+E.Message,true);
      end;
    end;
  end;
end;

function TUniversalInstaller.GetVersionFromURL(aUrl:string):string;
begin
  result:='0.0.0';
end;

function TUniversalInstaller.GetReleaseCandidateFromSource:integer;
begin
  result:=0;
end;

function TUniversalInstaller.UnInstallPackage(PackagePath, WorkingDir: string): boolean;
const
  PACKAGE_KEYSTART='UserPkgLinks/';
  MISC_KEYSTART='MiscellaneousOptions/BuildLazarusOptions/StaticAutoInstallPackages/';
var
  cnt, i: integer;
  key,value:string;
  LazarusConfig: TUpdateLazConfig;
  PackageName,PackageAbsolutePath: string;
  xmlfile: string;
  lpkdoc:TConfig;
  lpkversion:TAPkgVersion;
  ReqCount:integer;
  ReqPackage,Path:string;
begin
  result:=false;

  PackageName:=FileNameWithoutExt(PackagePath);

  localinfotext:=InitInfoText(' (UnInstallPackage: '+PackageName+'): ');

  Infoln(localinfotext+'Entering ...',etDebug);

  Infoln(localinfotext+'Removing package from config-files',etInfo);

  // Convert any relative path to absolute path, if it's not just a file/package name:
  if ExtractFileName(PackagePath)=PackagePath then
    PackageAbsolutePath:=PackagePath
  else
    PackageAbsolutePath:=SafeExpandFileName(PackagePath);
  if FVerbose then WritelnLog(localinfotext+'Going to uninstall package',true);

  if (ExtractFileName(PackagePath)<>PackagePath) then
  begin
    if FileExists(PackageAbsolutePath) then
    begin
      lpkdoc:=TConfig.Create(PackageAbsolutePath);
      try
        // get package requirements
        Path:='Package/RequiredPkgs/';
        ReqCount:=lpkdoc.GetValue(Path+'Count',0);
        for i:=1 to ReqCount do
        begin
          Path:='Package/RequiredPkgs/';
          ReqPackage:=lpkdoc.GetValue(Path+'Item'+InttoStr(i)+'/PackageName/Value','unknown');
          // try to auto-resolve dependencies
          // not very elegant, but working
          if (ReqPackage<>'unknown') then
          begin
            ReqPackage:=ChangeFileExt(ExtractFileName(ReqPackage), '.lpk');
            ReqPackage:=FindFileInDir(ReqPackage,WorkingDir);
            if FileExists(ReqPackage) then UnInstallPackage(ReqPackage, WorkingDir);
          end;
        end;
      finally
        lpkdoc.Free;
      end;
    end;
  end;

  LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
  try
    try
      xmlfile:=PackageConfig;
      cnt:=LazarusConfig.GetVariable(xmlfile, PACKAGE_KEYSTART+'Count', 0);
      // check if package is already registered
      i:=cnt;
      while i>0 do
      begin
        // Ignore package name casing
        if UpperCase(LazarusConfig.GetVariable(xmlfile, PACKAGE_KEYSTART+'Item'+IntToStr(i)+'/'
          +'Name/Value'))
          =UpperCase(PackageName) then
            break;
        i:=i-1;
      end;
      if i>1 then // found
      begin
        Infoln(localinfotext+'Found the package as item '+IntToStr(i)+' ... removing it from '+xmlfile,etInfo);
        FLazarusNeedsRebuild:=true;
        while i<cnt do
        begin
          LazarusConfig.MovePath(
            xmlfile,
            PACKAGE_KEYSTART+'Item'+IntToStr(i+1)+'/',
            PACKAGE_KEYSTART+'Item'+IntToStr(i)+'/');
          i:=i+1;
        end;
        LazarusConfig.DeletePath(xmlfile, PACKAGE_KEYSTART+'Item'+IntToStr(cnt)+'/');
        LazarusConfig.SetVariable(xmlfile, PACKAGE_KEYSTART+'Count', cnt-1);
      end;

      xmlfile:=MiscellaneousConfig;
      cnt:=LazarusConfig.GetVariable(xmlfile, MISC_KEYSTART+'Count', 0);
      // check if package is already registered
      i:=cnt;
      while i>0 do
      begin
        // Ignore package name casing
        if UpperCase(LazarusConfig.GetVariable(xmlfile, MISC_KEYSTART+'Item'+IntToStr(i)+'/Value'))=UpperCase(PackageName) then break;
        i:=i-1;
      end;
      if i>1 then // found
      begin
        Infoln(localinfotext+'Found the package as item '+IntToStr(i)+' ... removing it from '+xmlfile,etInfo);
        FLazarusNeedsRebuild:=true;
        while i<cnt do
        begin
          value:=LazarusConfig.GetVariable(xmlfile, MISC_KEYSTART+'Item'+IntToStr(i+1)+'/Value');
          LazarusConfig.SetVariable(xmlfile, MISC_KEYSTART+'Item'+IntToStr(i)+'/Value', value);
          // Move does mot work. ToDo !
          //Infoln(localinfotext+'Moving '+MISC_KEYSTART+'Item'+IntToStr(i+1)+' towards '+MISC_KEYSTART+'Item'+IntToStr(i),etDebug);
          //LazarusConfig.MovePath(xmlfile,
          //  MISC_KEYSTART+'Item'+IntToStr(i+1)+'/',
          //  MISC_KEYSTART+'Item'+IntToStr(i)+'/');
          i:=i+1;
        end;
        Infoln(localinfotext+'Deleting duplicate '+MISC_KEYSTART+'Item'+IntToStr(cnt),etDebug);
        LazarusConfig.DeletePath(xmlfile, MISC_KEYSTART+'Item'+IntToStr(cnt)+'/');
        Infoln(localinfotext+'Setting '+MISC_KEYSTART+'Count to '+IntToStr(cnt-1),etDebug);
        LazarusConfig.SetVariable(xmlfile, MISC_KEYSTART+'Count', cnt-1);
      end;

    except
      on E: Exception do
      begin
        Result := false;
        Infoln(localinfotext+'Failure setting Lazarus config: ' + E.ClassName + '/' + E.Message, etError);
      end;
    end;
  finally
    LazarusConfig.Free;
  end;

  if (ExtractFileName(PackagePath)<>PackagePath) then
  begin
    if FVerbose then WritelnLog(localinfotext+'Checking lpl file for '+ExtractFileName(PackagePath),true);
    if FileExists(PackageAbsolutePath) then
    begin
      lpkdoc:=TConfig.Create(PackageAbsolutePath);
      try
        key:='Package/';
        try
          lpkversion.FileVersion:=lpkdoc.GetValue(key+'Version',0);
        except
          lpkversion.FileVersion:=2;// On error assume version 2.
        end;
        key:='Package/Name/';
        lpkversion.Name:=lpkdoc.GetValue(key+'Value','');
        if (length(lpkversion.Name)>0) then
        begin
          key:='Package/Version/';
          lpkversion.GetVersion(lpkdoc,key);
          PackageAbsolutePath := IncludeTrailingPathDelimiter(FLazarusInstallDir)+
                                 'packager'+DirectorySeparator+
                                 'globallinks'+DirectorySeparator+
                                 LowerCase(lpkversion.Name)+'-'+lpkversion.AsString+'.lpl';
          if SysUtils.DeleteFile(PackageAbsolutePath) then
            Infoln(localinfotext+'Package '+PackageAbsolutePath+' deleted',etInfo);
        end;
      finally
        lpkdoc.Free;
      end;
    end;
  end;

  result:=true;
end;
{$endif}

// Runs all InstallExecute<n> commands inside a specified module
{ todo: Note that for some reason the installpackage etc commands are processed in configmodule.
Shouldn't this be changed? }
function TUniversalInstaller.BuildModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;
begin
  result:=inherited;
  result:=InitModule;
  if not result then exit;

  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    // Run all InstallExecute<n> commands:
    // More detailed logging only if verbose or debug:
    if FVerbose then WritelnLog(infotext+'Going to build '+ModuleName+' running all InstallExecute commands in: '+LineEnding+
      sl.CommaText,true);
    result:=RunCommands('InstallExecute',sl);
    end
  else
    result:=false;
end;

function TUniversalInstaller.CleanModule(ModuleName: string): boolean;
var
  idx:integer;
  PackageSettings:TStringList;
begin
  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    PackageSettings:=TStringList(UniModuleList.Objects[idx]);
    SourceDirectory:=GetValueFromKey('InstallDir',PackageSettings);
    SourceDirectory:=FixPath(SourceDirectory);
  end;
  result:=inherited;
  result:=InitModule;
  if not result then exit;
end;

// Processes a single module (i.e. section in fpcup.ini)
function TUniversalInstaller.ConfigModule(ModuleName: string): boolean;
{$ifndef FPCONLY}
var
  idx,cnt,i:integer;
  sl:TStringList;
  LazarusConfig:TUpdateLazConfig;
  directive,xmlfile,key:string;

function AddToLazXML(xmlfile:string):boolean;
var
  i,j,k:integer;
  exec,key,counter,oldcounter,filename:string;
  count:integer;
  ReadyCounter:integer;
begin
  result:=true;
  //filename:=xmlfile;
  //if rightstr(filename,4)<>'.xml' then
  filename:=xmlfile+'.xml';
  oldcounter:='';
  ReadyCounter:=0;

  for i:=0 to MAXINSTRUCTIONS do
  begin
    // Read command, e.g. AddToHelpOptions1
    // and deduce which XML settings file to update
    if i=0
       then exec:=GetValueFromKey('AddTo'+xmlfile,sl)
       else exec:=GetValueFromKey('AddTo'+xmlfile+IntToStr(i),sl);

    //Limit iterration;
    if ReadyCounter>MAXEMPTYINSTRUCTIONS then break;

    // Skip over missing data or if no exec is defined
    if (exec='') then
    begin
      Inc(ReadyCounter);
      continue;
    end else ReadyCounter:=0;

    //split off key and value
    j:=1;
    while j<=length(exec) do
    begin
      j:=j+1;
      if exec[j]=':' then break;
    end;
    key:=trim(copy(exec,1,j-1));
    { Use @ as a prefix in your keys to indicate a counter of subsections.
    The key afterwards is used to determine the variable that keeps the count.
    Example:
    <ExternalTools Count="2">
      <Tool1>
        <Format Version="2"/>
        <Title Value="LazDataDesktop"/>
        <Filename Value="C:\Lazarus\tools\lazdatadesktop\lazdatadesktop.exe"/>
      </Tool1>
    => use @Count in your key to match ExternalTools Count="2"
    }
    k:=pos('@',key);
    if k<=0 then
      LazarusConfig.SetVariable(filename,key,trim(copy(exec,j+1,length(exec))))
    else //we got a counter
    begin
      counter:= trim(copy(key,k+1,length(key)));
      key:=trim(copy(key,1,k-1));
      if oldcounter<>counter then //read write counter only once
      begin
        count:=LazarusConfig.GetVariable(filename,counter,0)+1;
        LazarusConfig.SetVariable(filename,counter,count);
        oldcounter:=counter;
      end;
      k:=pos('#',key);
      while k>0 do
      begin //replace # with current count
        delete(key,k,1);
        insert(IntToStr(count),key,k);
        k:=pos('#',key);
      end;
      LazarusConfig.SetVariable(filename,key,trim(copy(exec,j+1,length(exec))));
    end;
    if (not result) then break;
  end;
end;
{$endif}
begin
// Add values to lazarus config files. Syntax:
// AddTo<filename><number>=key[@counter]:value
// filename: xml file to update in --primary-config-path. The list of files is limited to the list below for security reasons.
// number: command number, starting from 1 for every file. The numbers have to be sequential. Scanning stops at the first missing number.
// key: the attribute to change in the format aa/bb/cc
// counter: the attribute key for the counter used to keep track of lists. Used to insert a new value in a list. Read and incremented by 1;
//          When using a counter, <key> can use a the '#' character as a placeholder for the new count written to <counter>
// value:  the string value to store in <key>.
  result:=inherited;
  result:=InitModule;
  if not result then exit;
  {$ifndef FPCONLY}
  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
      sl:=TStringList(UniModuleList.Objects[idx]);
      // Process AddPackage
      // Compile a package and add it to the list of user-installed packages.
      // Usage:
      // AddPackage<n>=<path to package>\<package.lpk>
      // As this will modify config values, we keep it out the section below.
      AddPackages(sl);

      LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
      try
        try
          // For security reasons, the files below are the only files we allow adding to/modifying:
          AddToLazXML(ExtractFileName(EditorConfig)); //editor options
          AddToLazXML(ExtractFileName(EnvironmentConfig)); //general options
          AddToLazXML(ExtractFileName(HelpConfig));
          AddToLazXML(ExtractFileName(MiscellaneousConfig)); //e.g. list of packages to be installed on recompile
          AddToLazXML(ExtractFileName(PackageConfig)); //e.g. list of available packages
          // Process special directives
          Directive:=GetValueFromKey('RegisterExternalTool',sl);
          if Directive<>'' then
          begin
            xmlfile:=EnvironmentConfig;
            key:='EnvironmentOptions/ExternalTools/Count';
            cnt:=LazarusConfig.GetVariable(xmlfile,key,0);
            // check if tool is already registered
            i:=cnt;
            while i>0 do
            begin
              if LazarusConfig.GetVariable(xmlfile,'EnvironmentOptions/ExternalTools/Tool'+IntToStr(i)+'/Title/Value')=ModuleName then break;
              i:=i-1;
            end;
            if i<1 then //not found
            begin
              cnt:=cnt+1;
              LazarusConfig.SetVariable(xmlfile,key,cnt);
            end
            else
              cnt:=i;
            key:='EnvironmentOptions/ExternalTools/Tool'+IntToStr(cnt)+'/';
            LazarusConfig.SetVariable(xmlfile,key+'Format/Version','2');
            LazarusConfig.SetVariable(xmlfile,key+'Title/Value',ModuleName);
            Infoln(infotext+'Going to register external tool '+Directive+GetExeExt,etDebug);
            LazarusConfig.SetVariable(xmlfile,key+'Filename/Value',Directive+GetExeExt);

            // If we're registering external tools, we should look for associated/
            // detailed directives as well:
            Directive:=GetValueFromKey('RegisterExternalToolCmdLineParams',sl);
            if Directive<>'' then
              LazarusConfig.SetVariable(xmlfile,key+'CmdLineParams/Value',Directive);
            Directive:=GetValueFromKey('RegisterExternalToolWorkingDirectory',sl);
            if Directive<>'' then
              LazarusConfig.SetVariable(xmlfile,key+'WorkingDirectory/Value',Directive);
            Directive:=GetValueFromKey('RegisterExternalToolScanOutputForFPCMessages',sl);
            if (Directive<>'') and (Directive<>'0') then // default = false
              LazarusConfig.SetVariable(xmlfile,key+'ScanOutputForFPCMessages/Value','True')
            else
              LazarusConfig.DeleteVariable(xmlfile,key+'ScanOutputForFPCMessages/Value');
            Directive:=GetValueFromKey('RegisterExternalToolScanOutputForMakeMessages',sl);
            if (Directive<>'') and (Directive<>'0') then // default = false
              LazarusConfig.SetVariable(xmlfile,key+'ScanOutputForMakeMessages/Value','True')
            else
              LazarusConfig.DeleteVariable(xmlfile,key+'ScanOutputForMakeMessages/Value');
            Directive:=GetValueFromKey('RegisterExternalToolHideMainForm',sl);
            if Directive='0' then // default = true
              LazarusConfig.SetVariable(xmlfile,key+'HideMainForm/Value','False')
            else
              LazarusConfig.DeleteVariable(xmlfile,key+'HideMainForm/Value');
          end;

          Directive:=GetValueFromKey('RegisterHelpViewer',sl);
          if Directive<>'' then
            begin
            xmlfile:=HelpConfig;
            key:='Viewers/TChmHelpViewer/CHMHelp/Exe';
            Infoln(infotext+'Going to register help viewer '+Directive+GetExeExt,etDebug);
            LazarusConfig.SetVariable(xmlfile,key,Directive+GetExeExt);
            end;

          // Register path to help source if given
          Directive:=GetValueFromKey('RegisterLazDocPath',sl);
          if Directive<>'' then
            begin
            Infoln(infotext+'Going to add docpath '+Directive,etDebug);
            LazDocPathAdd(Directive, LazarusConfig);
            end;
        except
          on E: Exception do
          begin
            if Directive='' then
              WritelnLog(etError,infotext+'Exception '+E.ClassName+'/'+E.Message+' configuring module: '+ModuleName, true)
            else
              WritelnLog(etError,infotext+'Exception '+E.ClassName+'/'+E.Message+' configuring module: '+ModuleName+' (parsing directive:'+Directive+')', true);
          end;
        end;
      finally
        LazarusConfig.Destroy;
      end;

      // If Lazarus was marked for rebuild, do so:
      if FLazarusNeedsRebuild then
      begin
        Infoln(infotext+'Packages were installed.',etInfo);
        result:=RebuildLazarus;
      end;
  end
  else
  begin
    // Could not find module in module list
    WritelnLog(etError, infotext+'Could not find specified module '+ModuleName,true);
    result:=false;
  end;
  {$endif}
end;

// Download from SVN, hg, git for module
function TUniversalInstaller.GetModule(ModuleName: string): boolean;
var
  idx,i:integer;
  PackageSettings:TStringList;
  RemoteURL:string;
  BeforeRevision: string='';
  AfterRevision: string='';
  UpdateWarnings: TStringList;
  FilesList: TStringList;
  aFile:string;
  ResultCode: longint;
  SourceOK:boolean;
  aName:string;
begin
  result:=inherited;
  result:=InitModule;
  if not result then exit;

  RemoteURL:='';

  SourceOK:=false;
  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    PackageSettings:=TStringList(UniModuleList.Objects[idx]);

    WritelnLog(infotext+'Getting module '+ModuleName,True);
    SourceDirectory:=GetValueFromKey('InstallDir',PackageSettings);
    SourceDirectory:=FixPath(SourceDirectory);

    if (SourceDirectory<>'') then
    begin
      // Common keywords for all repo methods
      DesiredRevision:=GetValueFromKey('Revision',PackageSettings);
      Branch:=GetValueFromKey('Branch',PackageSettings);
      TAG:=GetValueFromKey('Tag',PackageSettings);

      // Handle Git URLs
      RemoteURL:=GetValueFromKey('GITURL',PackageSettings);
      if (RemoteURL<>'') AND (NOT SourceOK) then
      begin
        Infoln(infotext+'Going to download/update from GIT repository '+RemoteURL,etInfo);
        Infoln(infotext+'Please wait: this can take some time (if repo is big or has a large history).',etInfo);
        UpdateWarnings:=TStringList.Create;
        try
          URL:=RemoteURL;
          GitClient.ModuleName := ModuleName;
          GitClient.ExportOnly := FExportOnly;
          result:=DownloadFromGit(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          SourceOK:=(result) AND (DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory)+'.git') OR FExportOnly);
          if UpdateWarnings.Count>0 then
          begin
            WritelnLog(UpdateWarnings.Text);
          end;
        finally
          UpdateWarnings.Free;
        end;
        if SourceOK
           then Infoln(infotext+'Download/update from GIT repository ok.',etInfo)
           else Infoln(infotext+'Getting GIT repo failed. Trying another source, if available.',etWarning)
      end;

      // Handle SVN urls
      RemoteURL:=GetValueFromKey('SVNURL',PackageSettings);
      if (RemoteURL<>'') AND (NOT SourceOK) then
      begin
        Infoln(infotext+'Going to download/update from SVN repository '+RemoteURL,etInfo);
        Infoln(infotext+'Please wait: this can take some time (if repo is big or has a large history).',etInfo);
        UpdateWarnings:=TStringList.Create;
        try
          URL:=RemoteURL;
          SVNClient.ModuleName := ModuleName;
          SVNClient.ExportOnly := FExportOnly;
          SVNClient.UserName   := GetValueFromKey('UserName',PackageSettings);
          SVNClient.Password   := GetValueFromKey('Password',PackageSettings);
          result:=DownloadFromSVN(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          SourceOK:=(result) AND (DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory)+'.svn') OR FExportOnly);
          if UpdateWarnings.Count>0 then
          begin
            WritelnLog(UpdateWarnings.Text);
          end;
          // hack for pascalscada (if needed)
          if ModuleName='pascalscada' then
          begin
            aFile:=IncludeTrailingPathDelimiter(SourceDirectory)+'pascalscada.lrs';
            if (NOT FileExists(aFile)) then
            begin
              try
                result:=Download(FUseWget,'https://sourceforge.net/p/pascalscada/code/HEAD/tree/trunk/pascalscada.lrs?format=raw', aFile);
              except
              end;
            end;
          end;
        finally
          UpdateWarnings.Free;
        end;
        if SourceOK
           then Infoln(infotext+'Download/update from SVN repository ok.',etInfo)
           else Infoln(infotext+'Getting SVN repo failed. Trying another source, if available.',etWarning)
      end;

      // Handle HG URLs
      RemoteURL:=GetValueFromKey('HGURL',PackageSettings);
      if (RemoteURL<>'') AND (NOT SourceOK) then
      begin
        Infoln(infotext+'Going to download/update from HG repository '+RemoteURL,etInfo);
        Infoln(infotext+'Please wait: this can take some time (if repo is big or has a large history).',etInfo);
        UpdateWarnings:=TStringList.Create;
        try
          Url:=RemoteURL;
          HGClient.ModuleName:=ModuleName;
          HGClient.ExportOnly:=FExportOnly;
          result:=DownloadFromHG(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          SourceOK:=result;
          if result=false then
            WritelnLog(infotext+'HG error downloading from '+RemoteURL+'. Continuing regardless.',true);
          if UpdateWarnings.Count>0 then
          begin
            WritelnLog(UpdateWarnings.Text);
          end;
        finally
          UpdateWarnings.Free;
        end;
        if SourceOK
           then Infoln(infotext+'Download/update from HG repository ok.',etInfo)
           else Infoln(infotext+'Getting HG repo failed. Trying another source, if available.',etWarning)
      end;

      RemoteURL:=GetValueFromKey('ArchiveURL',PackageSettings);
      if (RemoteURL<>'') AND (NOT SourceOK) then
      begin
        if (NOT DirectoryIsEmpty(SourceDirectory)) then
        begin
          Infoln(localinfotext+ModuleName+' sources are already there. Using these. Skipping download.',etWarning);
          Infoln(localinfotext+ModuleName+' sources are already there.',etInfo);
          Infoln(localinfotext+'Sources: '+SourceDirectory,etInfo);
          Infoln(localinfotext+'Build-process will continue with existing sources.',etInfo);
          Infoln(localinfotext+'Delete directory yourself if new sources are desired.',etInfo);
          SourceOK:=True;
        end;
      end;

      if (RemoteURL<>'') AND (NOT SourceOK) then
      begin
        Infoln(infotext+'Going to download from archive '+RemoteURL,etInfo);
        aName:=FileNameFromURL(RemoteURL);
        if Length(aName)>0 then
        begin
          aName:=SysUtils.ExtractFileExt(aName);
          if Length(aName)>0 then
          begin
            if aName[1]='.' then Delete(aName,1,1);
          end;
        end;
        //If no extension, assume zip
        if Length(aName)=0 then aName:='zip';
        aFile := GetTempFileNameExt('FPCUPTMP',aName);
        WritelnLog(infotext+'Going to download '+RemoteURL+' into '+aFile,false);
        try
          result:=Download(FUseWget, RemoteURL, aFile);
          if result then result:=FileExists(aFile);
        except
          on E: Exception do
          begin
           result:=false;
          end;
        end;

        if result=false then
           WritelnLog(etError,infotext+'Error downloading from '+RemoteURL+'. Continuing regardless.',True);

        if result then
        begin
          WritelnLog(infotext+'Download ok',True);

          if (ModuleName='lamw-gradle') OR (ModuleName='mORMot-gradle') then
          begin
            //store info
            aName:=infotext;
            //try to stop gradle deamons
            UnInstallModule(ModuleName);
            //wait a bit after stopping daemons
            sleep(2000);
            //restore info
            infotext:=aName;
          end;

          //Delete existing files from install directory
          if DirectoryExists(SourceDirectory) then DeleteDirectoryEx(SourceDirectory);
          //Sometimes, we need to do this twice ... :-(
          if DirectoryExists(SourceDirectory) then DeleteDirectoryEx(SourceDirectory);

          // Extract, overwrite
          case UpperCase(SysUtils.ExtractFileExt(aFile)) of
             '.ZIP','.TMP':
                begin
                  with TNormalUnzipper.Create do
                  begin
                    try
                      ResultCode:=Ord(NOT DoUnZip(aFile,IncludeTrailingPathDelimiter(SourceDirectory),[]));
                    finally
                      Free;
                    end;
                  end;
                end;
             '.7Z':
                begin
                  ResultCode:=ExecuteCommand(F7zip+' x -o"'+IncludeTrailingPathDelimiter(SourceDirectory)+'" '+aFile,FVerbose);
                  {$ifdef MSWINDOWS}
                  // try winrar
                  if ResultCode <> 0 then
                  begin
                    ResultCode:=ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+aFile+' "'+IncludeTrailingPathDelimiter(SourceDirectory)+'"',FVerbose);
                  end;
                  {$endif}
                  if ResultCode <> 0 then
                  begin
                    ResultCode:=ExecuteCommand('7z'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(SourceDirectory)+'" '+aFile,FVerbose);
                  end;
                  if ResultCode <> 0 then
                  begin
                    ResultCode:=ExecuteCommand('7za'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(SourceDirectory)+'" '+aFile,FVerbose);
                  end;
                end;
             '.rar':
                begin
                  ResultCode:=ExecuteCommand(FUnrar+' x "'+aFile+'" "'+IncludeTrailingPathDelimiter(SourceDirectory)+'"',FVerbose);
                  {$ifdef MSWINDOWS}
                  // try winrar
                  if ResultCode <> 0 then
                  begin
                    ResultCode:=ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+aFile+' "'+IncludeTrailingPathDelimiter(SourceDirectory)+'"',FVerbose);
                  end;
                  {$endif}
                end;

             else {.tar and all others}
                ResultCode:=ExecuteCommand(FTar+' -xf '+aFile +' -C '+SourceDirectory,FVerbose);
             end;
          if ResultCode <> 0 then
          begin
            result := False;
            Infoln(infotext+'Unpack of '+aFile+' failed with resultcode: '+IntToStr(ResultCode),etWarning);
          end;
        end;
        SysUtils.Deletefile(aFile); //Get rid of temp file.
        SourceOK:=result;
        if SourceOK then
        begin
          Infoln(infotext+'Download from archive ok.',etInfo);

          // Check specials : sometimes, an extra path is added when unpacking, installing
          // Move files up ... tricky, but necessary unfortunately ...
          //if ((Pos('github.com',RemoteURL)>0) AND (Pos('/archive/',RemoteURL)>0) OR (Pos('sourceforge.net',RemoteURL)>0)) then
          begin
            //There should be a single directory !
            aName:='';
            FilesList:=FindAllDirectories(SourceDirectory,False);
            if FilesList.Count=1 then aName:=FilesList[0];
            FreeAndNil(FilesList);
            if (Length(aName)>0) AND (DirectoryExists(aName)) then
            begin
              Infoln(infotext+'Moving files due to extra path. Please wait.',etInfo);
              FilesList:=FindAllFiles(aName, '', True);
              for i:=0 to (FilesList.Count-1) do
              begin
                aFile:=FilesList[i];
                aFile:=StringReplace(aFile,aName,aName+DirectorySeparator+'..',[]);
                aFile:=SafeExpandFileName(aFile);
                ForceDirectoriesSafe(ExtractFileDir(aFile));
                SysUtils.RenameFile(FilesList[i],aFile);
              end;
              if (NOT CheckDirectory(aName)) then DeleteDirectory(aName,False);
              FreeAndNil(FilesList);
            end;
          end;
        end else Infoln(infotext+'Getting archive failed. Trying another source, if available.',etInfo)
      end;

      RemoteURL:=GetValueFromKey('ArchivePATH',PackageSettings);
      if (RemoteURL<>'') AND (NOT SourceOK) then
      begin
        Infoln(infotext+'Going to download from archive path '+RemoteURL,etInfo);
        aFile := RemoteURL;
        case UpperCase(SysUtils.ExtractFileExt(aFile)) of
           '.ZIP':
           begin
             with TNormalUnzipper.Create do
             begin
               try
                 ResultCode:=Ord(NOT DoUnZip(aFile,IncludeTrailingPathDelimiter(SourceDirectory),[]));
               finally
                 Free;
               end;
             end;
           end;
           '.7Z':
           begin
             ResultCode:=ExecuteCommand(F7zip+' x -o"'+IncludeTrailingPathDelimiter(SourceDirectory)+'" '+aFile,FVerbose);
             {$ifdef MSWINDOWS}
             // try winrar
             if ResultCode <> 0 then
             begin
               ResultCode:=ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+aFile+' "'+IncludeTrailingPathDelimiter(SourceDirectory)+'"',FVerbose);
             end;
             {$endif}
             if ResultCode <> 0 then
             begin
               ResultCode:=ExecuteCommand('7z'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(SourceDirectory)+'" '+aFile,FVerbose);
             end;
             if ResultCode <> 0 then
             begin
               ResultCode:=ExecuteCommand('7za'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(SourceDirectory)+'" '+aFile,FVerbose);
             end;
           end;
           else {.tar and all others}
              ResultCode:=ExecuteCommand(FTar+' -xf '+aFile +' -C '+ExcludeTrailingPathDelimiter(SourceDirectory),FVerbose);
        end;

        if ResultCode <> 0 then
        begin
          result := False;
          Infoln(infotext+'Unpack of '+aFile+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
        end;

        if result then Infoln(infotext+'Download from archive path ok.',etInfo);

        // todo patch package if correct patch is available in patch directory

      end;
    end
    else
    begin
      Infoln(infotext+'No source directory defined. Skipping fetching of external sources.',etDebug);
    end;
  end
  else
    result:=false;

  if result then PatchModule(ModuleName);
end;

// Runs all UnInstallExecute<n> commands inside a specified module
function TUniversalInstaller.UnInstallModule(ModuleName: string): boolean;
{$ifndef FPCONLY}
var
  idx,cnt,i:integer;
  sl:TStringList;
  Directive,xmlfile,key:string;
  LazarusConfig:TUpdateLazConfig;
{$endif}
begin
  result:=inherited;

  {
  if not DirectoryExists(SourceDirectory) then
  begin
    Infoln(infotext+'No '+ModuleName+' source directory ('+SourceDirectory+') found [yet] ... nothing to be done',etDebug);
    exit(true);
  end;
  }

  result:=InitModule;
  if not result then exit;

  {$ifndef FPCONLY}
  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    if FVerbose then WritelnLog(infotext+'Going to remove '+ModuleName+' running all UnInstallExecute commands in: '+LineEnding+
      sl.CommaText,true);
    result:=RunCommands('UnInstallExecute',sl);
    // Process all AddPackage<n> directives in reverse.
    // As this changes config files, we keep it outside
    // the section where LazarusConfig is modified
    RemovePackages(sl);

    LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
    try
      // Process specials
      Directive:=GetValueFromKey('RegisterExternalTool',sl);
      if Directive<>'' then
      begin
        xmlfile:=EnvironmentConfig;
        key:='EnvironmentOptions/ExternalTools/Count';
        cnt:=LazarusConfig.GetVariable(xmlfile,key,0);
        // check if tool is registered
        i:=cnt;
        while i>0 do
        begin
          if LazarusConfig.GetVariable(xmlfile,'EnvironmentOptions/ExternalTools/Tool'+IntToStr(i)+'/Title/Value')
            =ModuleName then
              break;
          i:=i-1;
        end;
        if i>=1 then // found
        begin
          LazarusConfig.SetVariable(xmlfile,key,cnt-1);
          key:='EnvironmentOptions/ExternalTools/Tool'+IntToStr(i)+'/';
          while i<cnt do
          begin
            LazarusConfig.MovePath(xmlfile,'EnvironmentOptions/ExternalTools/Tool'+IntToStr(i+1)+'/',
               'EnvironmentOptions/ExternalTools/Tool'+IntToStr(i)+'/');
            i:=i+1;
          end;
          LazarusConfig.DeletePath(xmlfile,'EnvironmentOptions/ExternalTools/Tool'+IntToStr(cnt)+'/');
        end;
      end;

      Directive:=GetValueFromKey('RegisterHelpViewer',sl);
      if Directive<>'' then
      begin
        xmlfile:=HelpConfig;
        key:='Viewers/TChmHelpViewer/CHMHelp/Exe';
        // Setting the variable to empty should be enough to disable the help viewer.
        LazarusConfig.SetVariable(xmlfile,key,'');
      end;
    finally
      LazarusConfig.Destroy;
    end;

    // If Lazarus was marked for rebuild, do so:
    if FLazarusNeedsRebuild then
    begin
      Infoln(infotext+'Packages were removed.',etInfo);
      result:=RebuildLazarus;
    end;
  end
  else
    result:=false;
  {$endif}
end;

{ TmORMotPXLInstaller }

function TmORMotPXLInstaller.BuildModule(ModuleName: string): boolean;
var
  Workingdir,aFile,SDKDir,NDKDir:string;
  s:string;
  idx:integer;
  sl:TStringList;
  FilesList:TStringList;
  FileContents:TStrings;
begin
  result:=inherited;
  result:=InitModule;
  if not result then exit;

  //Perform some extra magic for this module

  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    sl:=TStringList(UniModuleList.Objects[idx]);

    Workingdir:=GetValueFromKey(LOCATIONMAGIC,sl);
    if Workingdir='' then Workingdir:=GetValueFromKey(INSTALLMAGIC,sl);
    Workingdir:=FixPath(Workingdir);

    if DirectoryExists(Workingdir) then
    begin
      FilesList:=TStringList.Create;

      try

        //Process Java JDK settings
        s:=SafeExpandFileName(ExtractFilePath(GetJavac)+'..');
        if DirectoryExists(s) then
        begin
          s:=StringReplace(s,'\','/',[rfReplaceAll]);
          FilesList.Clear;
          FindAllFiles(FilesList,Workingdir, 'gradle.properties', true);
          for idx:=0 to Pred(FilesList.Count) do
          begin
            aFile:=FilesList[idx];
            Infoln(infotext+'Processing file: '+aFile,etInfo);
            FileContents:=TStringList.Create;
            try
              FileContents.LoadFromFile(aFile);
              FileContents.Values['org.gradle.java.home']:=s;
              FileContents.SaveToFile(aFile);
            finally
              FileContents.Free;
            end;
          end;
        end;

        SDKDir:=GetAndroidSDKDir;
        NDKDir:=GetAndroidNDKDir;

        FilesList.Clear;
        FindAllFiles(FilesList,Workingdir, 'gradle-local-*.bat;gradle-local-*.sh', true);
        for idx:=0 to Pred(FilesList.Count) do
        begin
          aFile:=FilesList[idx];
          Infoln(infotext+'Processing file: '+aFile,etInfo);
          FileContents:=TStringList.Create;
          try
            FileContents.LoadFromFile(aFile);

            //Process Gradle settings
            s:=ConcatPaths([Workingdir,'..','mORMot-gradle','gradle-6.5']);
            s:=SafeExpandFileName(s);
            if DirectoryExists(s) then
            begin
              FileContents.Values['set GRADLE_HOME']:=s;
              s:='%PATH%;%GRADLE_HOME%\bin'
            end else s:='%PATH%';

            //Process SDK settings
            if DirectoryExists(SDKDir) then
              FileContents.Values['set PATH']:=s+';'+ConcatPaths([SDKDir,'platform-tools'])
            else
              FileContents.Values['set PATH']:=s;

            FileContents.SaveToFile(aFile);
          finally
            FileContents.Free;
          end;
        end;

        FilesList.Clear;
        FindAllFiles(FilesList,Workingdir, 'local.properties', true);
        for idx:=0 to Pred(FilesList.Count) do
        begin
          aFile:=FilesList[idx];
          Infoln(infotext+'Processing file: '+aFile,etInfo);
          FileContents:=TStringList.Create;
          try
            FileContents.LoadFromFile(aFile);

            //Process SDK settings
            if DirectoryExists(SDKDir) then
            begin
              s:=IncludeTrailingPathDelimiter(SDKDir);
              {$ifdef MSWindows}
              s:=StringReplace(s,'\','\\',[rfReplaceAll]);
              s:=StringReplace(s,':','\:',[]);
              {$endif}
              FileContents.Values['sdk.dir']:=s;
            end;

            //Process NDK settings
            if DirectoryExists(NDKDir) then
            begin
              s:=IncludeTrailingPathDelimiter(NDKDir);
              {$ifdef MSWindows}
              s:=StringReplace(s,'\','\\',[rfReplaceAll]);
              s:=StringReplace(s,':','\:',[]);
              {$endif}
              FileContents.Values['ndk.dir']:=s;
            end;

            FileContents.SaveToFile(aFile);
          finally
            FileContents.Free;
          end;
        end;
      finally
        FilesList.Free;
      end;

    end;
  end;
end;

function TAWGGInstaller.BuildModule(ModuleName: string): boolean;
var
  Workingdir,versionitis_exe:string;
  idx:integer;
  sl:TStringList;
begin
  result:=inherited;
  if not result then exit;

  {$ifndef FPCONLY}

  //Perform some extra magic for this module

  Workingdir:='';

  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    Workingdir:=GetValueFromKey(LOCATIONMAGIC,sl);
    if Workingdir='' then Workingdir:=GetValueFromKey(INSTALLMAGIC,sl);
    Workingdir:=FixPath(Workingdir);
    Workingdir:=Workingdir+DirectorySeparator+'src';
  end;

  Processor.Process.Parameters.Clear;
  Processor.Executable := IncludeTrailingPathDelimiter(FLazarusInstallDir)+LAZBUILDNAME+GetExeExt;
  Processor.Process.CurrentDirectory := ExcludeTrailingPathDelimiter(Workingdir);
  Processor.SetParamData('--primary-config-path='+FLazarusPrimaryConfigPath);
  Processor.SetParamData('--recursive');

  Processor.SetParamData(Workingdir+DirectorySeparator+'versionitis.lpi');
  Processor.SetParamData('--build-mode=default');

  Infoln(infotext+Processor.GetExeInfo,etDebug);
  ProcessorResult:=Processor.ExecuteAndWait;
  result := (ProcessorResult=0);

  if not result then exit;

  {$ifdef Windows}
  versionitis_exe:=Workingdir+DirectorySeparator+'win-versionitis'+GetExeExt;
  FileCopy(Workingdir+DirectorySeparator+'versionitis'+GetExeExt,versionitis_exe);
  {$else}
  versionitis_exe:=Workingdir+DirectorySeparator+'versionitis'+GetExeExt;
  {$endif}

  // Tricky: copy awgg.lpi to prevent failure of versionitis
  ForceDirectoriesSafe(Workingdir+DirectorySeparator+'src');
  FileCopy(Workingdir+DirectorySeparator+'awgg.lpi',Workingdir+DirectorySeparator+'src'+DirectorySeparator+'awgg.lpi');
  FileCopy(Workingdir+DirectorySeparator+'src'+DirectorySeparator+'versionitis.pas',Workingdir+DirectorySeparator+'versionitis.pas');

  Processor.Process.Parameters.Clear;
  Processor.Executable := versionitis_exe;
  Processor.Process.CurrentDirectory := ExcludeTrailingPathDelimiter(Workingdir);
  Processor.SetParamData('-verbose');

  Infoln(infotext+Processor.GetExeInfo,etDebug);
  ProcessorResult:=Processor.ExecuteAndWait;
  result := (ProcessorResult=0);

  if not result then exit;

  Processor.Process.Parameters.Clear;
  Processor.Executable := IncludeTrailingPathDelimiter(FLazarusInstallDir)+LAZBUILDNAME+GetExeExt;
  Processor.Process.CurrentDirectory := ExcludeTrailingPathDelimiter(Workingdir);
  Processor.SetParamData('--primary-config-path='+FLazarusPrimaryConfigPath);
  Processor.SetParamData('--recursive');

  Processor.SetParamData(Workingdir+DirectorySeparator+'awgg.lpr');
  Processor.SetParamData('--build-mode=default');

  Infoln(infotext+Processor.GetExeInfo,etDebug);
  ProcessorResult:=Processor.ExecuteAndWait;
  result := (ProcessorResult=0);

  {$endif}
end;

function TPas2jsInstaller.BuildModule(ModuleName: string): boolean;
var
  Workingdir,FilePath:string;
  idx:integer;
  sl:TStringList;
  {$ifndef FPCONLY}
  LazarusConfig: TUpdateLazConfig;
  {$endif}
begin
  result:=inherited;
  if not result then exit;

  //Perform some extra magic for this module

  Workingdir:='';

  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    Workingdir:=GetValueFromKey(LOCATIONMAGIC,sl);
    if Workingdir='' then Workingdir:=GetValueFromKey(INSTALLMAGIC,sl);
    Workingdir:=FixPath(Workingdir);
  end;

  if (NOT DirectoryExists(Workingdir)) then exit;

  FilePath:=ConcatPaths([WorkingDir,'units',GetFPCTarget(true)]);
  if DirectoryExists(FilePath) then DeleteDirectoryEx(FilePath);

  FilePath:=ConcatPaths([WorkingDir,'bin',GetFPCTarget(true)]);
  if DirectoryExists(FilePath) then DeleteDirectoryEx(FilePath);

  FilePath:=ConcatPaths([WorkingDir,'fpmake'+GetExeExt]);
  if FileExists(FilePath) then SysUtils.DeleteFile(FilePath);

  FilePath:=ConcatPaths([WorkingDir,'fpmake.o']);
  if FileExists(FilePath) then SysUtils.DeleteFile(FilePath);

  Processor.Process.Parameters.Clear;
  Processor.Executable:=Make;
  Processor.Process.CurrentDirectory := ExcludeTrailingPathDelimiter(Workingdir);
  //Processor.SetParamMakefilePathData('FPC',FCompiler);
  Processor.SetParamMakefilePathData('PP',FCompiler);
  //Processor.SetParamMakefilePathData('PP',ExtractFilePath(FCompiler)+GetCompilerName(GetSourceCPU));
  //Processor.SetParamMakefilePathData('FPCDIR',IncludeTrailingPathDelimiter(Workingdir)+'compiler');

  Processor.SetParamData('clean');
  Processor.SetParamData('all');

  Infoln(infotext+Processor.GetExeInfo,etDebug);

  try
    ProcessorResult:=Processor.ExecuteAndWait;
    result := (ProcessorResult=0);

    if (NOT result) then
    begin
      WritelnLog(etError, infotext+ExtractFileName(Processor.Executable)+' returned error code ' + IntToStr(ProcessorResult) + LineEnding +
        'Details: ' + FErrorLog.Text, true);
    end;
  except
    on E: Exception do
    begin
      result := false;
      WritelnLog(etError, infotext+'Exception running '+ExtractFileName(Processor.Executable)+' to build the pas2js compiler !' + LineEnding +
        'Details: ' + E.Message, true);
    end;
  end;

  if not result then exit;

  {$ifndef FPCONLY}

  LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
  try
    // set defaults for pas2js
    FilePath:=ConcatPaths([WorkingDir,'bin',GetFPCTarget(true),'pas2js'+GetExeExt]);
    LazarusConfig.SetVariable(Pas2jsConfig, 'compiler/value', FilePath);
    FilePath:=ConcatPaths([WorkingDir,'bin',GetFPCTarget(true),'compileserver'+GetExeExt]);
    LazarusConfig.SetVariable(Pas2jsConfig, 'webserver/value', FilePath);
    LazarusConfig.SetVariable(WebserverConfig, 'Server/Exe/Value', FilePath);
    {$ifdef Windowssssss}
    FilePath:='C:\Program Files\Google\Chrome\Application\chrome.exe';
    if FileExists(FilePath) then
    begin
      LazarusConfig.SetVariable(WebserverConfig, 'Browser/Kind', 'Chrome');
    end
    else
    begin
      FilePath:='C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe';
      if FileExists(FilePath) then
      begin
        LazarusConfig.SetVariable(WebserverConfig, 'Browser/Kind', 'Edge');
      end
    end;
    {$endif}
  finally
    LazarusConfig.Free;
  end;

  FilePath:=ConcatPaths([WorkingDir,'packages','rtl','pas2js_rtl.lpk']);
  result:=InstallPackage(FilePath,WorkingDir,True);
  if not result then exit;

  FilePath:=ConcatPaths([WorkingDir,'packages','fcl-base','fcl_base_pas2js.lpk']);
  result:=InstallPackage(FilePath,WorkingDir,True);
  if not result then exit;

  FilePath:=ConcatPaths([WorkingDir,'packages','fcl-db','pas2js_fcldb.lpk']);
  result:=InstallPackage(FilePath,WorkingDir,True);
  if not result then exit;

  FilePath:=ConcatPaths([WorkingDir,'packages','fpcunit','fpcunit_pas2js.lpk']);
  result:=InstallPackage(FilePath,WorkingDir,True);
  if not result then exit;

  FilePath:=ConcatPaths([FLazarusSourceDir,'components','pas2js','pas2jsdsgn.lpk']);
  result:=InstallPackage(FilePath,WorkingDir,False);
  if not result then exit;

  RebuildLazarus;

  {$endif}
end;

function TInternetToolsInstaller.GetModule(ModuleName: string): boolean;
var
  Workingdir,FLREDir:string;
  aSourceFile,aTargetFile:string;
  idx:integer;
  sl:TStringList;
begin
  result:=inherited;
  if not result then exit;

  //Perform some extra magic for this module
  //Copy some files from FLRE

  Workingdir:='';
  FLREDir:='';

  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    Workingdir:=GetValueFromKey(LOCATIONMAGIC,sl);
    if Workingdir='' then Workingdir:=GetValueFromKey(INSTALLMAGIC,sl);
    Workingdir:=FixPath(Workingdir);
  end;

  idx:=UniModuleList.IndexOf('flre');
  if idx>=0 then
  begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    FLREDir:=GetValueFromKey(LOCATIONMAGIC,sl);
    if FLREDir='' then FLREDir:=GetValueFromKey(INSTALLMAGIC,sl);
    FLREDir:=FixPath(FLREDir);
  end;

  if DirectoryExists(Workingdir) AND DirectoryExists(FLREDir) then
  begin
    aSourceFile:=ConcatPaths([FLREDir,'src'])+DirectorySeparator+'FLRE.pas';
    aTargetFile:=ConcatPaths([Workingdir,'data'])+DirectorySeparator+'FLRE.pas';
    if FileExists(aSourceFile) then
      FileCopy(aSourceFile,aTargetFile,[]);

    aSourceFile:=ConcatPaths([FLREDir,'src'])+DirectorySeparator+'PUCU.pas';
    aTargetFile:=ConcatPaths([Workingdir,'data'])+DirectorySeparator+'PUCU.pas';
    if FileExists(aSourceFile) then
      FileCopy(aSourceFile,aTargetFile,[]);
  end;
end;

function TDeveltools4FPCInstaller.GetModule(ModuleName: string): boolean;
var
  idx,iassets                         : integer;
  PackageSettings                     : TStringList;
  RemoteURL                           : string;
  aName,aFile,aURL,aContent,aVersion  : string;
  ResultCode                          : longint;
  Json                                : TJSONData;
  Release,Asset                       : TJSONObject;
  Assets                              : TJSONArray;
  ValidTarget                         : boolean;
begin
  result:=InitModule;
  if not result then exit;

  // There are only 4 versions of the tools
  // Windows 64bit, Linux AMD64, Darwin AMD64, Darwin ARM64
  // This check could also be skipped, but anyhow
  ValidTarget:=false;
  if (NOT ValidTarget) then ValidTarget:=(GetTOS(GetSourceOS) in [TOS.win32,TOS.win64]);
  if (NOT ValidTarget) then ValidTarget:=(GetTOS(GetSourceOS) in [TOS.linux]) AND (GetTCPU(GetSourceCPU) in [TCPU.x86_64]);
  if (NOT ValidTarget) then ValidTarget:=(GetTOS(GetSourceOS) in [TOS.darwin]) AND (GetTCPU(GetSourceCPU) in [TCPU.x86_64,TCPU.aarch64]);

  if (NOT ValidTarget) then exit;

  idx:=UniModuleList.IndexOf(ModuleName);
  if (idx>=0) then
  begin
    WritelnLog(infotext+'Getting module '+ModuleName,True);

    PackageSettings:=TStringList(UniModuleList.Objects[idx]);
    SourceDirectory:=GetValueFromKey('InstallDir',PackageSettings);
    SourceDirectory:=FixPath(SourceDirectory);

    if (SourceDirectory<>'') then
    begin

      ForceDirectoriesSafe(SourceDirectory);

      RemoteURL:=GetValueFromKey('GITURL',PackageSettings);
      if (RemoteURL<>'') then
      begin
        // Get latest release through api
        aURL:=StringReplace(RemoteURL,'//github.com','//api.github.com/repos',[]);
        aURL:=aURL+'/releases';

        aContent:=GetURLDataFromCache(aURL);
        result:=(Length(aContent)>0);

        if result then
        begin
          result:=false;
          try
            Json:=GetJSON(aContent);
          except
            Json:=nil;
          end;
          if JSON.IsNull then exit;

          try
            for idx:=0 to Pred(Json.Count) do
            begin
              Release := TJSONObject(Json.Items[idx]);
              aVersion:=Release.Get('tag_name');
              {$ifdef Windows}
              aFile:='develtools4fpc-x86_64-win64';
              {$else}
              aFile:='develtools4fpc-'+GetSourceCPUOS;
              {$endif}
              Assets:=Release.Get('assets',TJSONArray(nil));
              for iassets:=0 to Pred(Assets.Count) do
              begin
                Asset := TJSONObject(Assets[iassets]);
                aName:=Asset.Get('name');
                if (Pos(aFile,aName)=1) then
                begin
                  aURL:=Asset.Get('browser_download_url');
                  result:=true;
                end;
                if result then break;
              end;
              if result then break;
            end;
          finally
            Json.Free;
          end;
        end;

        if result then
        begin
          aName:=FileNameFromURL(aURL);
          Infoln(infotext+'Going to download '+aVersion+' of develtools4fpc ['+aName+'] from '+aURL,etInfo);
          if Length(aName)>0 then
          begin
            aName:=SysUtils.ExtractFileExt(aName);
            if Length(aName)>0 then
            begin
              if aName[1]='.' then Delete(aName,1,1);
            end;
          end;
          //If no extension, assume zip
          if Length(aName)=0 then aName:='zip';
          aFile := GetTempFileNameExt('FPCUPTMP',aName);
          WritelnLog(infotext+'Going to download '+aURL+' into '+aFile,false);
          try
            result:=Download(FUseWget, aURL, aFile);
            if result then result:=FileExists(aFile);
          except
            on E: Exception do
            begin
             result:=false;
            end;
          end;

          if result then
          begin
            if (FileSize(aFile)>5000) then
            begin
              ResultCode:=-1;
              WritelnLog(infotext+'Download ok',True);
              if DirectoryExists(SourceDirectory) then DeleteDirectoryEx(SourceDirectory);
              with TNormalUnzipper.Create do
              begin
                try
                  ResultCode:=Ord(NOT DoUnZip(aFile,IncludeTrailingPathDelimiter(SourceDirectory),[]));
                finally
                  Free;
                end;
              end;
              if (ResultCode<>0) then
              begin
                result := False;
                Infoln(infotext+'Unpack of '+aFile+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
              end;
            end;
          end;
          SysUtils.Deletefile(aFile); //Get rid of temp file.
        end;

        if (NOT result) then
        begin
          Infoln(infotext+'Getting develtools4fpc failure. Will continue anyhow.',etInfo);
        end;

      end;
    end;
  end;

  // Do not fail
  result:=true;
end;

function TXTensaTools4FPCInstaller.GetModule(ModuleName: string): boolean;
var
  idx,iassets                         : integer;
  PackageSettings                     : TStringList;
  aName,aRemoteURL,aContent,aVersion  : string;
  aBinFile,aBinURL                    : string;
  aLibFile,aLibURL                    : string;
  ResultCode                          : longint;
  Json                                : TJSONData;
  Release,Asset                       : TJSONObject;
  Assets                              : TJSONArray;
begin
  result:=InitModule;
  if not result then exit;

  localinfotext:=InitInfoText(' ('+Copy(ClassName,2,MaxInt)+': '+ModuleName+'): ');

  aBinURL:='';
  aLibURL:='';

  aBinFile:='xtensa-binutils-'+GetSourceCPUOS+'.zip';
  aLibFile:='xtensa-libs-'+GetSourceCPUOS+'.zip';

  idx:=UniModuleList.IndexOf(ModuleName);
  if (idx>=0) then
  begin
    WritelnLog(localinfotext+'Getting module '+ModuleName,True);

    PackageSettings:=TStringList(UniModuleList.Objects[idx]);

    begin
      aRemoteURL:=GetValueFromKey('GITURL',PackageSettings);
      if (aRemoteURL<>'') then
      begin

        // Get latest release through api
        aRemoteURL:=StringReplace(aRemoteURL,'//github.com','//api.github.com/repos',[]);
        aRemoteURL:=aRemoteURL+'/releases';

        aContent:=GetURLDataFromCache(aRemoteURL);
        result:=(Length(aContent)>0);

        aRemoteURL:=GetValueFromKey('GITURL',PackageSettings);

        if result then
        begin
          result:=false;

          try
            Json:=GetJSON(aContent);
          except
            Json:=nil;
          end;
          if JSON.IsNull then exit;

          try
            for idx:=0 to Pred(Json.Count) do
            begin
              Release := TJSONObject(Json.Items[idx]);
              aVersion:=Release.Get('tag_name');

              Assets:=Release.Get('assets',TJSONArray(nil));
              for iassets:=0 to Pred(Assets.Count) do
              begin
                Asset := TJSONObject(Assets[iassets]);
                aName:=Asset.Get('name');
                if (Pos(aBinFile,aName)=1) then
                  aBinURL:=Asset.Get('browser_download_url');
                if (Pos(aLibFile,aName)=1) then
                  aLibURL:=Asset.Get('browser_download_url');
                result:=( (Length(aBinURL)>0) AND (Length(aLibURL)>0) );
                if result then break;
              end;
              if result then break;
            end;
          finally
            Json.Free;
          end;
        end;

        if result then
        begin
          SourceDirectory:=ConcatPaths([BaseDirectory,CROSSBINPATH,GetCPU(TCPU.xtensa)+'-'+GetOS(TOS.freertos)]);
          if ( (NOT DirectoryExists(SourceDirectory)) OR (DirectoryIsEmpty(SourceDirectory)) ) then
          begin
            Infoln(localinfotext+'Going to download '+aVersion+' of xtensatools4fpc ['+aBinFile+'] from '+aRemoteURL,etInfo);
            try
              aName:=ConcatPaths([FTempDirectory,aBinFile]);
              result:=Download(FUseWget, aBinURL, aName);
              if result then result:=( FileExists(aName) AND (FileSize(aName)>5000) );
            except
              on E: Exception do
              begin
               result:=false;
              end;
            end;
            if result then
            begin
              ResultCode:=-1;
              WritelnLog(localinfotext+'Download ok',True);
              ForceDirectoriesSafe(SourceDirectory);
              with TNormalUnzipper.Create do
              begin
                try
                  ResultCode:=Ord(NOT DoUnZip(aName,IncludeTrailingPathDelimiter(SourceDirectory),[]));
                finally
                  Free;
                end;
              end;
              if (ResultCode<>0) then
              begin
                result := False;
                Infoln(localinfotext+'Unpack of '+aBinFile+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
              end;
            end;
            SysUtils.Deletefile(aName); //Get rid of temp file.
          end;

          SourceDirectory:=ConcatPaths([BaseDirectory,CROSSLIBPATH,GetCPU(TCPU.xtensa)+'-'+GetOS(TOS.freertos)]);
          if ( (NOT DirectoryExists(SourceDirectory)) OR (DirectoryIsEmpty(SourceDirectory)) ) then
          begin
            Infoln(localinfotext+'Going to download '+aVersion+' of xtensatools4fpc ['+aLibFile+'] from '+aRemoteURL,etInfo);
            try
              aName:=ConcatPaths([FTempDirectory,aLibFile]);
              result:=Download(FUseWget, aLibURL, aName);
              if result then result:=( FileExists(aName) AND (FileSize(aName)>5000) );
            except
              on E: Exception do
              begin
               result:=false;
              end;
            end;
            if result then
            begin
              ResultCode:=-1;
              WritelnLog(localinfotext+'Download ok',True);
              SourceDirectory:=BaseDirectory+DirectorySeparator+CROSSLIBPATH+DirectorySeparator+'xtensa-freertos';
              ForceDirectoriesSafe(SourceDirectory);
              with TNormalUnzipper.Create do
              begin
                try
                  ResultCode:=Ord(NOT DoUnZip(aName,IncludeTrailingPathDelimiter(SourceDirectory),[]));
                finally
                  Free;
                end;
              end;
              if (ResultCode<>0) then
              begin
                result := False;
                Infoln(localinfotext+'Unpack of '+aLibFile+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
              end;
            end;
            SysUtils.Deletefile(aName); //Get rid of temp file.
          end;
        end;

        if (NOT result) then
        begin
          Infoln(localinfotext+'Getting xtensatools4fpc failure. Will continue anyhow.',etInfo);
        end;

      end;
    end;
  end;

  // Do not fail
  result:=true;
end;


function TMBFFreeRTOSWioInstaller.GetModule(ModuleName: string): boolean;
var
  idx:integer;
  PackageSettings:TStringList;
  aList,aFileList:TStringList;
  aDir,aLine,aFile:string;
begin
  result:=inherited;

  // Ignore errors due to GitHub
  result:=true;

  if not result then exit;

  localinfotext:=Copy(ClassName,2,MaxInt)+' ('+Copy(ClassName,2,MaxInt)+': '+ModuleName+'): ';

  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
  begin
    WritelnLog(localinfotext+'Getting module '+ModuleName,True);

    PackageSettings:=TStringList(UniModuleList.Objects[idx]);
    SourceDirectory:=GetValueFromKey('InstallDir',PackageSettings);
    SourceDirectory:=FixPath(SourceDirectory);

    if (SourceDirectory<>'') then
    begin
      aList:=TStringList.Create;
      try
        aLine:='set CROSS=';
        //aDir:=ConcatPaths([SourceDirectory,'SamplesBoardSpecific','WioTerminal','Examples']);
        aDir:=ConcatPaths([SourceDirectory,'SamplesBoardSpecific','WioTerminal']);
        aFileList := TStringList.Create;
        try
          FindAllFiles(aFileList, aDir,'*.bat', true);
          for aFile in aFileList do
          begin
            aList.LoadFromFile(aFile);
            idx:=StringListStartsWith(aList,aLine);
            if (idx<>-1) then
            begin
              Infoln(localinfotext+'Setting correct path in '+ExtractFileName(aFile)+'.',etInfo);
              aList.Strings[idx]:='set CROSS='+ConcatPaths([BaseDirectory,CROSSDIRNAME]);
              aList.SaveToFile(aFile);
            end;
            aList.Clear;
          end;
        finally
          aFileList.Free;
        end;
      finally
        aList.Free;
      end;
    end;
  end;

  // Do not fail
  result:=true;
end;

function TmORMot2Installer.GetModule(ModuleName: string): boolean;
var
  idx,iassets                                    : integer;
  PackageSettings                                : TStringList;
  Output                                         : string;
  RemoteURL                                      : string;
  aName,aFile,aURL,aContent,aVersion             : string;
  ResultCode                                     : longint;
  Json                                           : TJSONData;
  Release,Asset                                  : TJSONObject;
  Assets                                         : TJSONArray;
  FilesList                                      : TStringList;
begin
  result:=inherited;
  if not result then exit;

  localinfotext:=Copy(ClassName,2,MaxInt)+' (GetModule: '+ModuleName+'): ';

  idx:=UniModuleList.IndexOf(ModuleName);
  if (idx>=0) then
  begin
    WritelnLog(localinfotext+'Getting static sqlite3 files.',True);

    PackageSettings:=TStringList(UniModuleList.Objects[idx]);
    SourceDirectory:=GetValueFromKey('InstallDir',PackageSettings);
    SourceDirectory:=FixPath(SourceDirectory);

    if (SourceDirectory<>'') then
    begin
      ForceDirectoriesSafe(SourceDirectory);

      result:=false;

      {
      if (NOT result) then
      begin
        aURL:='https://synopse.info/files/mormot2static.7z';
        aFile := GetTempFileNameExt('FPCUPTMP','7z');
        WritelnLog(infotext+'Going to download '+aURL+' into '+aFile,false);
        try
          result:=Download(FUseWget, aURL, aFile);
          if result then result:=FileExists(aFile);
          if result then result:=(FileSize(aFile)>5000);
        except
          on E: Exception do
          begin
           result:=false;
          end;
        end;
      end;
      }

      if (NOT result) then
      begin
        RemoteURL:=GetValueFromKey('GITURL',PackageSettings);
        if (RemoteURL<>'') then
        begin
          // Get latest release through api
          aURL:=StringReplace(RemoteURL,'//github.com','//api.github.com/repos',[]);
          aURL:=aURL+'/releases';

          aContent:=GetURLDataFromCache(aURL);
          result:=(Length(aContent)>0);

          if result then
          begin
            result:=false;
            try
              Json:=GetJSON(aContent);
            except
              Json:=nil;
            end;
            if JSON.IsNull then exit;

            try
              for idx:=0 to Pred(Json.Count) do
              begin
                Release := TJSONObject(Json.Items[idx]);
                aVersion:=Release.Get('tag_name');
                aFile:='mormot2static.7z';
                Assets:=Release.Get('assets',TJSONArray(nil));
                for iassets:=0 to Pred(Assets.Count) do
                begin
                  Asset := TJSONObject(Assets[iassets]);
                  aName:=Asset.Get('name');
                  if (Pos(aFile,aName)=1) then
                  begin
                    aURL:=Asset.Get('browser_download_url');
                    result:=true;
                  end;
                  if result then break;
                end;
                if result then break;
              end;
            finally
              Json.Free;
            end;
          end;

          if result then
          begin
            aName:=FileNameFromURL(aURL);
            Infoln(localinfotext+'Going to download '+aVersion+' of mormot sqlite3 static libs ['+aName+'] from '+aURL,etInfo);
            if Length(aName)>0 then
            begin
              aName:=SysUtils.ExtractFileExt(aName);
              if Length(aName)>0 then
              begin
                if aName[1]='.' then Delete(aName,1,1);
              end;
            end;
            //If no extension, assume zip
            if Length(aName)=0 then aName:='zip';
            aFile := GetTempFileNameExt('FPCUPTMP',aName);
            WritelnLog(localinfotext+'Going to download '+aURL+' into '+aFile,false);
            try
              result:=Download(FUseWget, aURL, aFile);
              if result then result:=FileExists(aFile);
            except
              on E: Exception do
              begin
               result:=false;
              end;
            end;
          end;
        end;
      end;

      if result then
      begin
        if (FileSize(aFile)>5000) then
        begin
          ResultCode:=-1;
          WritelnLog(localinfotext+'Download ok. Extracting files. Please wait.',True);

          SourceDirectory:=SourceDirectory+DirectorySeparator+'static';
          if DirectoryExists(SourceDirectory) then DeleteDirectoryEx(SourceDirectory);
          ForceDirectoriesSafe(SourceDirectory);

          RunCommandIndir(SourceDirectory,F7zip ,['x','-y','-bd',aFile],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});

          {$ifdef MSWINDOWS}
          // try winrar
          if (ResultCode<>0) then
          begin
            ResultCode:=ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+aFile+' "'+IncludeTrailingPathDelimiter(SourceDirectory)+'"',FVerbose);
          end;
          {$endif}
          if (ResultCode<>0) then
          begin
            RunCommandIndir(SourceDirectory,'7z' ,['x','-y','-bd',aFile],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          end;
          if (ResultCode<>0) then
          begin
            RunCommandIndir(SourceDirectory,'7za' ,['x','-y','-bd',aFile],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          end;

          if (ResultCode<>0) then
          begin
            result := False;
            Infoln(localinfotext+'Unpack of '+aFile+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
          end;

        end;
      end;
      SysUtils.Deletefile(aFile); //Get rid of temp file.

      if result then
      begin
        aName:='';
        FilesList:=FindAllDirectories(SourceDirectory,False);
        if FilesList.Count=1 then aName:=FilesList[0];
        FreeAndNil(FilesList);
        if (LowerCase(ExtractFileName(aName))='static') then
        begin
          Infoln(infotext+'Moving files due to extra path.',etInfo);
          //Infoln(infotext+'Also simultaneously correcting line-endings.',etInfo);
          Infoln(infotext+'This is time-consuming. Please wait.',etInfo);
          FilesList:=FindAllFiles(aName, '', True);
          for idx:=0 to (FilesList.Count-1) do
          begin
            aFile:=FilesList[idx];
            aFile:=StringReplace(aFile,aName,aName+DirectorySeparator+'..',[]);
            aFile:=SafeExpandFileName(aFile);
            ForceDirectoriesSafe(ExtractFileDir(aFile));
            SysUtils.RenameFile(FilesList[idx],aFile);
            {$ifdef UNIX}
            //Correct line endings
            //Diabled for now: sed consumes enormous amount of time
            //ExecuteCommand('sed -i '+'''s/\r//'''+' '+aFile,False);
            {$endif}
          end;
          DeleteDirectory(aName,False);
          FreeAndNil(FilesList);
        end;
      end;
    end;

    if (NOT result) then
    begin
      Infoln(localinfotext+'Getting mORMot2 (statics) failure. Will continue anyhow.',etInfo);
    end;

  end;

  // Do not fail
  result:=true;
end;

function TWSTInstaller.GetModule(ModuleName: string): boolean;
const
  PACKAGE_KEYSTART='Package/Files/';
var
  idx,cnt                      : integer;
  PackageSettings              : TStringList;
  xmlfile                      : string;
  {$ifndef FPCONLY}
  LazarusConfig                : TUpdateLazConfig;
  {$endif}
begin
  result:=inherited;
  if (not result) then exit;

  {$ifndef FPCONLY}

  idx:=UniModuleList.IndexOf(ModuleName);
  if (idx>=0) then
  begin
    WritelnLog(infotext+'Getting module '+ModuleName,True);

    PackageSettings:=TStringList(UniModuleList.Objects[idx]);
    SourceDirectory:=GetValueFromKey('InstallDir',PackageSettings);
    SourceDirectory:=FixPath(SourceDirectory);

    if (SourceDirectory<>'') then
    begin
      SourceDirectory:=ConcatPaths([SourceDirectory,'ide','lazarus']);
      LazarusConfig:=TUpdateLazConfig.Create(SourceDirectory);
      try
        try
          xmlfile:='wst_design.lpk';
          cnt:=LazarusConfig.GetVariable(xmlfile, PACKAGE_KEYSTART+'Count', 0);
          // check if package is already registered
          idx:=cnt;
          while (idx>0) do
          begin
            // Ignore package name casing
            if (LazarusConfig.GetVariable(xmlfile, PACKAGE_KEYSTART+'Item'+IntToStr(idx)+'/'+'UnitName/Value')='parserdefs') then
              break;
            Dec(idx);
          end;
          if (idx>1) then // found
          begin
            Infoln(localinfotext+'Found the package as item '+IntToStr(idx)+' ... removing it from '+xmlfile,etInfo);
            while (idx<cnt) do
            begin
              LazarusConfig.MovePath(
                xmlfile,
                PACKAGE_KEYSTART+'Item'+IntToStr(idx+1)+'/',
                PACKAGE_KEYSTART+'Item'+IntToStr(idx)+'/');
              Inc(idx);
            end;
            LazarusConfig.DeletePath(xmlfile, PACKAGE_KEYSTART+'Item'+IntToStr(cnt)+'/');
            LazarusConfig.SetVariable(xmlfile, PACKAGE_KEYSTART+'Count', cnt-1);
          end;

        except
          on E: Exception do
          begin
            Result := false;
            Infoln(localinfotext+'Failure setting Lazarus config: ' + E.ClassName + '/' + E.Message, etError);
          end;
        end;
      finally
        LazarusConfig.Free;
      end;
    end;
  end;

  // Do not fail
  result:=true;

  {$endif}
end;


function TCEFInstaller.GetModule(ModuleName: string): boolean;
type
  TCEFBuildsURL = record
    CPU:TCPU;
    OS:TOS;
    URL:string;
  end;
const
  CEFBuildsURLTable :array [0..5] of TCEFBuildsURL =
  (
  (CPU:i386;     OS:win32;   URL:'https://cef-builds.spotifycdn.com/cef_binary_109.1.11%2Bg6d4fdb2%2Bchromium-109.0.5414.87_windows32.tar.bz2'),
  (CPU:x86_64;   OS:win64;   URL:'https://cef-builds.spotifycdn.com/cef_binary_109.1.11%2Bg6d4fdb2%2Bchromium-109.0.5414.87_windows64.tar.bz2'),
  (CPU:x86_64;   OS:linux;   URL:'https://cef-builds.spotifycdn.com/cef_binary_109.1.11%2Bg6d4fdb2%2Bchromium-109.0.5414.87_linux64.tar.bz2'),
  (CPU:arm;      OS:linux;   URL:'https://cef-builds.spotifycdn.com/cef_binary_109.1.11%2Bg6d4fdb2%2Bchromium-109.0.5414.87_linuxarm.tar.bz2'),
  (CPU:aarch64;  OS:linux;   URL:'https://cef-builds.spotifycdn.com/cef_binary_109.1.11%2Bg6d4fdb2%2Bchromium-109.0.5414.87_linuxarm64.tar.bz2'),
  (CPU:x86_64;   OS:darwin;  URL:'https://cef-builds.spotifycdn.com/cef_binary_109.1.11%2Bg6d4fdb2%2Bchromium-109.0.5414.87_macosx64.tar.bz2')
  );
var
  aURL,aFile               : string;
  Output                   : string;
  URLData                  : TCEFBuildsURL;
  i,ResultCode             : integer;
  OperationSucceeded       : boolean;
begin
  result:=inherited;

  if (not result) then exit;

  for URLData in CEFBuildsURLTable do
  begin
    if ((URLData.CPU=GetTCPU(GetSourceCPU)) AND (URLData.OS=GetTOS(GetSourceOS))) then
    begin
      aURL:=URLData.URL;

      aFile:=GetTempFileNameExt('FPCUPTMP','tar.bz2');

      WritelnLog(localinfotext+'Going to download CEF libraries',true);
      WritelnLog(localinfotext+'Downloading '+aURL+' into '+aFile,true);

      try
        result:=Download(FUseWget, aURL, aFile);
        if result then result:=FileExists(aFile);
      except
        on E: Exception do
        begin
         result:=false;
        end;
      end;

      if result then
      begin
        if (FileSize(aFile)>5000) then
        begin
          ResultCode:=-1;
          WritelnLog(localinfotext+'Download ok. Extracting files. Please wait.',True);

          SourceDirectory:=SourceDirectory+DirectorySeparator+'libs';
          if DirectoryExists(SourceDirectory) then DeleteDirectoryEx(SourceDirectory);
          ForceDirectoriesSafe(SourceDirectory);

          // First, unpack the .bz2 to get the .tar
          RunCommandIndir(SourceDirectory,F7zip ,['x',aFile],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          SysUtils.Deletefile(aFile); //Get rid of temp file in temp-directory.
          if (ResultCode=0) then
          begin
            //We now have a .tar file in the target directory
            //Change extension and unpack tar-file
            aFile:=StringReplace(aFile,'.bz2','',[]);
            aFile:=SourceDirectory+DirectorySeparator+ExtractFileName(aFile);
            if ExtractFileExt(aFile)='.tar' then
            begin
              // First, get the name of the top-directory
              RunCommandIndir(SourceDirectory,F7zip ,['l',aFile,'-ba','-ttar','-x!*\*?'],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
              Output:=Trim(Output);
              i:=RPos(#32,Output);
              if (i>0) then
                aURL:=Copy(Output,i+1,MaxInt)
              else
                aURL:='*';
              aURL:=aURL+DirectorySeparator;
              // Now get a selection of files from the archive: only the files that are needed.
              OperationSucceeded:=True;
              if OperationSucceeded then OperationSucceeded:=RunCommand(F7zip ,['e','-aoa','-ttar',aFile,'-r',aURL+'Release\*'+GetLibExt,aURL+'Release\vk_swiftshader_icd.json',aURL+'Release\*.bin','-o'+SourceDirectory],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
              if OperationSucceeded then OperationSucceeded:=RunCommand(F7zip ,['e','-aoa','-ttar',aFile,'-r',aURL+'Resources\*.pak',aURL+'Resources\*.dat','-x!locales','-o'+SourceDirectory],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
              if OperationSucceeded then OperationSucceeded:=RunCommand(F7zip ,['e','-aoa','-ttar',aFile,'-r',aURL+'Resources\locales\*.pak','-o'+IncludeTrailingPathDelimiter(SourceDirectory)+'locales'],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});

              if (NOT OperationSucceeded) then
              begin
                ResultCode:=-1;
              end;

            end;
          end;

          (*
          {$ifdef UNIX}
          {$ifdef BSD}
          RunCommandIndir(SourceDirectory,FTar,['-jxf',aFile,'--include','*Release\*'+GetLibExt+'*','*Release\vk_swiftshader_icd.json','*Release\*.bin','*Resources\*.*'],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          {$else}
          RunCommandIndir(SourceDirectory,FTar,['-jxf',aFile,'--wildcards','--no-anchored','*Release\*'+GetLibExt+'*','*Release\vk_swiftshader_icd.json','*Release\*.bin','*Resources\*.*'],Output,ResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          {$endif}
          {$endif}
          *)

          if (ResultCode<>0) then
          begin
            result := False;
            Infoln(localinfotext+'Unpack of '+aFile+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
          end;

        end;
      end;
      SysUtils.Deletefile(aFile); //Get rid of temp file.

      break;
    end;
  end;

  // Do not fail
  result:=true;
end;

procedure ClearUniModuleList;
var
  i:integer;
begin
  for i:=0 to UniModuleList.Count -1 do
    TStringList(UniModuleList.Objects[i]).free;
end;

function GetKeyword(aDictionary,aAlias: string): string;
const
  ALIASMAGIC='ALIAS';
var
  ini:TMemIniFile;
  sl:TStringList;
  i:integer;
begin
  result:='';

  sl:=TStringList.Create;

  ini:=TMemIniFile.Create(CurrentConfigFile);
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  ini.Options:=ini.Options-[ifoCaseSensitive];
  {$ELSE}
  ini.CaseSensitive:=false;
  {$ENDIF}

  if ((aDictionary=FPCTAGLOOKUPMAGIC) OR (aDictionary=FPCBRANCHLOOKUPMAGIC)) AND AnsiStartsText(FPCTRUNKBRANCH,aAlias) then result:='trunk';
  if (aDictionary=FPCURLLOOKUPMAGIC) AND AnsiStartsText(FPCGITLABREPO,aAlias) then result:='gitlab';

  if ((aDictionary=LAZARUSTAGLOOKUPMAGIC) OR (aDictionary=LAZARUSBRANCHLOOKUPMAGIC)) AND AnsiStartsText(LAZARUSTRUNKBRANCH,aAlias) then result:='trunk';
  if (aDictionary=LAZARUSURLLOOKUPMAGIC) AND AnsiStartsText(LAZARUSGITLABREPO,aAlias) then result:='gitlab';

  if (Length(result)=0) then
  begin
    try
      ini.ReadSectionValues(ALIASMAGIC+aDictionary,sl);
      for i:=0 to Pred(sl.Count) do
      begin
        if sl.ValueFromIndex[i]=aAlias then
        begin
          result:=sl.Names[i];
          break;
        end;
      end;
    finally
      ini.Free;
      sl.free;
    end;
  end;

end;

function GetAlias(aDictionary,aKeyWord: string): string;
const
  ALIASMAGIC='ALIAS';
var
  ini:TMemIniFile;
  sl:TStringList;
  e:Exception;
begin
  sl:=TStringList.Create;

  ini:=TMemIniFile.Create(CurrentConfigFile);
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  ini.Options:=ini.Options-[ifoCaseSensitive];
  {$ELSE}
  ini.CaseSensitive:=false;
  {$ENDIF}

  try
    ini.ReadSection(ALIASMAGIC+aDictionary,sl);
    if Uppercase(aKeyWord)='LIST' then
    begin
      result:=sl.CommaText;
    end
    else
    begin
      result:=ini.ReadString(ALIASMAGIC+aDictionary,aKeyWord,'');

      if (aDictionary=FPCURLLOOKUPMAGIC) AND (aKeyWord='gitlab') then result:=FPCGITLABREPO;
      if (aDictionary=LAZARUSURLLOOKUPMAGIC) AND (aKeyWord='gitlab') then result:=LAZARUSGITLABREPO;

      if (result='') then
      begin
        if (Pos(FPCURLLOOKUPMAGIC,aDictionary)=1) OR (Pos(FPCTAGLOOKUPMAGIC,aDictionary)=1) OR (Pos(FPCBRANCHLOOKUPMAGIC,aDictionary)=1) then
        begin
          //if (aDictionary<>FPCURLLOOKUPMAGIC)    AND (result='') then result:=ini.ReadString(ALIASMAGIC+'fpcURL',   aKeyWord,'');
          //if (aDictionary=FPCTAGLOOKUPMAGIC)    AND (result='') then result:=ini.ReadString(ALIASMAGIC+FPCBRANCHLOOKUPMAGIC,   aKeyWord,'');
          //if (aDictionary=FPCBRANCHLOOKUPMAGIC) AND (result='') then result:=ini.ReadString(ALIASMAGIC+FPCTAGLOOKUPMAGIC,aKeyWord,'');
        end
        {$ifndef FPCONLY}
        else
        if (Pos(LAZARUSURLLOOKUPMAGIC,aDictionary)=1) OR (Pos(LAZARUSTAGLOOKUPMAGIC,aDictionary)=1) OR (Pos(LAZARUSBRANCHLOOKUPMAGIC,aDictionary)=1) then
        begin
          //if (aDictionary<>LAZARUSURLLOOKUPMAGIC)    AND (result='') then result:=ini.ReadString(ALIASMAGIC+'lazURL',   aKeyWord,'');
          //if (aDictionary=LAZARUSTAGLOOKUPMAGIC)    AND (result='') then result:=ini.ReadString(ALIASMAGIC+LAZARUSBRANCHLOOKUPMAGIC,   aKeyWord,'');
          //if (aDictionary=LAZARUSBRANCHLOOKUPMAGIC) AND (result='') then result:=ini.ReadString(ALIASMAGIC+LAZARUSTAGLOOKUPMAGIC,aKeyWord,'');
        end
        {$endif}
        else
        if (result='') then
        begin
          e:=Exception.CreateFmt('--%s=%s : Invalid keyword. Accepted keywords are: %s',[aDictionary,aKeyWord,sl.CommaText]);
          raise e;
        end;
      end;

      if ({(aDictionary=FPCTAGLOOKUPMAGIC) OR }(aDictionary=FPCBRANCHLOOKUPMAGIC)) AND (result='trunk') then result:=FPCTRUNKBRANCH;
      if ({(aDictionary=LAZARUSTAGLOOKUPMAGIC) OR }(aDictionary=LAZARUSBRANCHLOOKUPMAGIC)) AND (result='trunk') then result:=LAZARUSTRUNKBRANCH;

    end;
  finally
    ini.Free;
    sl.free;
  end;
end;

function SetAlias(aDictionary,aKeyWord,aValue: string):boolean;
var
  ini:TMemIniFile;
  s:string;
begin
  result:=false;
  ini:=TMemIniFile.Create(CurrentConfigFile);
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  ini.Options:=ini.Options-[ifoCaseSensitive];
  {$ELSE}
  ini.CaseSensitive:=false;
  {$ENDIF}
  try
    s:=ini.ReadString('ALIAS'+aDictionary,aKeyWord,'');
    if (length(s)=0) then
    begin
      ini.WriteString('ALIAS'+aDictionary,aKeyWord,aValue);
      ini.UpdateFile;
      result:=true;
    end;
  finally
    ini.Free;
  end;
end;

function GetModuleList: string;
var
  ini:TMemIniFile;
  i,j,maxmodules:integer;
  val,name:string;

  function LoadModule(ModuleName:string):boolean;
  var
    name:string;
    sl:TStringList;
    li:integer;
  begin
    name:=ini.ReadString(ModuleName,INIKEYWORD_NAME,'');
    result:=name<>'';
    if result then
    begin
      //if StrToBoolDef(ini.ReadString(ModuleName,'Enabled',''),false) then
      // skip all default modules when only installing FPC ... tricky but ok for now.
      {$ifndef FPCONLY}
      if ini.ReadBool(ModuleName,'Enabled',False) then
         UniModuleEnabledList.Add(name);
      {$endif}
      // store the section as is and attach as object to UniModuleList
      // TstringList cleared in finalization
      sl:=TstringList.Create;
      ini.ReadSectionRaw(ModuleName,sl);
      for li:=sl.Count-1 downto 0 do
      begin
        if (TrimLeft(sl.Strings[li])[1]=';') OR (TrimLeft(sl.Strings[li])[1]='#') then sl.Delete(li);
      end;
      with sl do
      begin
        if IndexOfName(INIKEYWORD_CATEGORY)=-1 then Add(Concat(INIKEYWORD_CATEGORY, NameValueSeparator, 'miscellaneous'));
      end;
      UniModuleList.AddObject(name,TObject(sl));
    end;
  end;

  function CreateModuleSequence(aModuleName:string;IsHidden:boolean=false):string;
  var
    ModuleName,Declaration,RequiredModules:string;
    RequiredModulesList:TStringList;
    li:integer;
  begin
    result:='';
    ModuleName:=ini.ReadString(aModuleName,INIKEYWORD_NAME,'');
    if ModuleName<>'' then
    begin
      if IsHidden then Declaration:=_DECLAREHIDDEN else Declaration:=_DECLARE;

      RequiredModules:='';
      RequiredModulesList:=TStringList.Create;
      try
        {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
        RequiredModulesList.AddCommaText(ini.ReadString(aModuleName,Trim(_REQUIRES),''));
        {$ELSE}
        RequiredModulesList.AddText(ini.ReadString(aModuleName,Trim(_REQUIRES),''));
        {$ENDIF}
        if (RequiredModulesList.Count>0) then
        begin
          for li:=0 to Pred(RequiredModulesList.Count) do
          begin
            RequiredModules:=RequiredModules+_REQUIRES+RequiredModulesList.Strings[li]+_SEP;
          end;
        end;
      finally
        RequiredModulesList.Free;
      end;

      result:=
          Declaration+ ModuleName + _SEP +
          RequiredModules +
          _CLEANMODULE + ModuleName +_SEP +
          _GETMODULE + ModuleName +_SEP +
          _CONFIGMODULE + ModuleName +_SEP +
          _BUILDMODULE + ModuleName +_SEP +
          _END +

          Declaration + ModuleName + _CLEAN + _SEP +
          _CLEANMODULE + ModuleName +_SEP +
          _END +

          Declaration + ModuleName + _BUILD + _ONLY + _SEP +
          _CLEANMODULE + ModuleName +_SEP +
          _CONFIGMODULE + ModuleName +_SEP +
          _BUILDMODULE + ModuleName +_SEP +
          _END+

          Declaration + ModuleName + _UNINSTALL + _SEP +
          _UNINSTALLMODULE + ModuleName +_SEP +
          _END;
    end;
  end;

begin
  result:='';
  ini:=TMemIniFile.Create(CurrentConfigFile);
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  Ini.Options:=[ifoStripQuotes]; //let ini handle e.g. lazopt="-g -gl -O1" for us
  {$ELSE}
  ini.StripQuotes:=true; //let ini handle e.g. lazopt="-g -gl -O1" for us
  {$ENDIF}
  //ini.CaseSensitive:=false;
  //ini.StripQuotes:=true; //helps read description lines

  // parse inifile
  try
    maxmodules:=ini.ReadInteger('General','MaxSysModules',MAXSYSMODULES);
    ini.ReadSectionRaw('General',IniGeneralSection);
    for i:=0 to maxmodules do
      if LoadModule('FPCUPModule'+IntToStr(i)) then
        result:=result+CreateModuleSequence('FPCUPModule'+IntToStr(i));
    maxmodules:=ini.ReadInteger('General','MaxUserModules',MAXUSERMODULES);
    for i:=0 to maxmodules do
      if LoadModule('UserModule'+IntToStr(i))then
        result:=result+CreateModuleSequence('UserModule'+IntToStr(i));
    for i:=0 to maxmodules do
      if LoadModule('HiddenModule'+IntToStr(i))then
        result:=result+CreateModuleSequence('HiddenModule'+IntToStr(i),true);

    // the overrides in the [general] section
    for i:=0 to UniModuleList.Count-1 do
      begin
      name:=UniModuleList[i];
      val:=ini.ReadString('General',name,'');
      if val='1' then
        begin //enable if not yet done
        if UniModuleEnabledList.IndexOf(name)<0 then
          UniModuleEnabledList.Add(name);
        end
      else if val='0' then
        begin //disable if enabled
        j:=UniModuleEnabledList.IndexOf(name);
        if j>=0 then
          UniModuleEnabledList.Delete(j);
        end;
      end;

    for i:=UniModuleEnabledList.Count-1 downto 0 do
      begin
      name:=UniModuleEnabledList[i];
      if NOT CheckIncludeModule(name) then
          UniModuleEnabledList.Delete(i);
      end;

    // create the sequences for default modules
    result:=result+_DECLAREHIDDEN+_UNIVERSALDEFAULT+_SEP;
    for i:=0 to UniModuleEnabledList.Count-1 do
        result:=result+_DO+UniModuleEnabledList[i]+_SEP;
    result:=result+_END;
    result:=result+_DECLAREHIDDEN+_UNIVERSALDEFAULT+_CLEAN+_SEP;
    for i:=0 to UniModuleEnabledList.Count-1 do
      result:=result+_DO+UniModuleEnabledList[i]+_CLEAN+_SEP;
    result:=result+_END;
    result:=result+_DECLAREHIDDEN+_UNIVERSALDEFAULT+_UNINSTALL+_SEP;
    for i:=0 to UniModuleEnabledList.Count-1 do
      result:=result+_DO+UniModuleEnabledList[i]+_UNINSTALL+_SEP;
    result:=result+_END;

  finally
    ini.Free;
  end;
end;

function CheckIncludeModule(ModuleName: string):boolean;
var
  ini:TMemIniFile;
  j:integer;
  os,cpu:string;
  AddModule,NegativeList:boolean;
  sl:TStringList;
  e:Exception;

  function GetValueSimple(Key: string; sl: TStringList): string;
  var
    i:integer;
    s:string;
  begin
    Key:=UpperCase(Key);
    s:='';
    for i:=0 to sl.Count-1 do
      begin
      s:=sl[i];
      if (copy(UpperCase(s),1, length(Key))=Key) and ((s[length(Key)+1]='=') or (s[length(Key)+1]=' ')) then
        begin
        if Pos('=',s)>0 then
          s:=trim(copy(s,pos('=',s)+1,length(s)));
        break;
        end;
      s:='';
      end;
    result:=s;
  end;

  function AND_OR_Values(V1,V2:boolean;setting:boolean):boolean;
  begin
    if setting
       then result:=(V1 AND V2)
       else result:=(V1 OR V2);
  end;

begin
  result:=False;

  ini:=TMemIniFile.Create(SafeGetApplicationPath+CONFIGFILENAME);
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  ini.Options:=ini.Options-[ifoCaseSensitive]+[ifoStripQuotes];
  {$ELSE}
  ini.CaseSensitive:=false;
  ini.StripQuotes:=true; //helps read description lines
  {$ENDIF}

  try
    AddModule:=True;

    j:=UniModuleList.IndexOf(ModuleName);

    if j=-1 then AddModule:=false;

    if AddModule=true then
    begin

      sl:=TStringList(UniModuleList.Objects[j]);

      os:=GetValueSimple('OS_OK',sl);
      if (os<>'') AND (AddModule) then
      begin
         NegativeList:=(Pos('-',os)>0);

         // simmple check of list
         // number of negative signs [-] must be one more than the number of list separators [,]
         if NegativeList AND (OccurrencesOfChar(os,'-')<>(OccurrencesOfChar(os,',')+1)) then
         begin
           e:=Exception.Create('Invalid os list. Check os definition of module '+ModuleName+' inside '+CONFIGFILENAME+'.');
           raise e;
         end;

         // if we have a negative define list, then default to true until a negative setting is encountered
         // if we have a positive define list, then default to false until a positive setting is encountered
         AddModule:=NegativeList;

         {$ifdef windows}
         if (Pos('mswindows',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-mswindows',os)=0),NegativeList) else
         begin
           if (Pos('windows',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-windows',os)=0),NegativeList) else
           begin
             {$ifdef win32}
             if (Pos(GetOS(TOS.win32),os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-win32',os)=0),NegativeList);
             {$endif}
             {$ifdef win64}
             if (Pos(GetOS(TOS.win64),os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-win64',os)=0),NegativeList);
             {$endif}
           end;
         end;
         {$else}
         if (Pos('unix',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-unix',os)=0),NegativeList);
         {$endif}

         {$ifdef linux}
         if (Pos('linux',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-linux',os)=0),NegativeList);
         {$endif}

         {$ifdef Darwin}
         if (Pos('darwin',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-darwin',os)=0),NegativeList);
         {$endif}

         {$ifdef OpenBSD}
         if (Pos('openbsd',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-openbsd',os)=0),NegativeList);
         {$endif}

         {$ifdef FreeBSD}
         if (Pos('freebsd',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-freebsd',os)=0),NegativeList);
         {$endif}
      end;

      cpu:=GetValueSimple('CPU_OK',sl);
      if (cpu<>'') AND (AddModule) then
      begin
         NegativeList:=(Pos('-',cpu)>0);

         // simmple check of list
         // number of negative signs [-] must be one more than the number of list separators [,]
         if NegativeList AND (OccurrencesOfChar(cpu,'-')<>(OccurrencesOfChar(cpu,',')+1)) then
         begin
           e:=Exception.Create('Invalid cpu list. Check cpu definition of module '+ModuleName+' inside '+CONFIGFILENAME+'.');
           raise e;
         end;

         // if we have a negative define list, then default to true until an negative setting is encountered
         // if we have a positive define list, then default to false until a positive setting is encountered
         AddModule:=NegativeList;

         {$ifdef CPU32}
         if (Pos('cpu32',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-cpu32',cpu)=0),NegativeList);
         {$endif}
         {$ifdef CPUI386}
         if (Pos('i386',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-i386',cpu)=0),NegativeList);
         {$endif}
         {$ifdef CPU64}
         if (Pos('cpu64',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-cpu64',cpu)=0),NegativeList);
         {$endif}
         {$ifdef CPUX86_64 }
         if (Pos('x86_64',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-x86_64',cpu)=0),NegativeList);
         {$endif}
         {$ifdef CPUARM}
         if (Pos('cpuarm',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-cpuarm',cpu)=0),NegativeList) else
         begin
           if (Pos('arm',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-arm',cpu)=0),NegativeList);
         end;
         {$endif}
         {$ifdef CPUAARCH64}
         if (Pos('cpuaarch64',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-cpuaarch64',cpu)=0),NegativeList) else
         begin
           if (Pos('aarch64',cpu)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-aarch64',cpu)=0),NegativeList);
         end;
         {$endif}
      end;
    end;

    result:=AddModule;

  finally
    ini.Free;
  end;
end;

function GetModuleEnabledList(var ModuleList: TStringList): boolean;
var i:integer;
begin
  result:=false;
  for i:=0 to UniModuleEnabledList.Count -1 do
    ModuleList.Add(UniModuleEnabledList[i]);
  result:=true;
end;

function SetConfigFile(aConfigFile: string):boolean;
begin
  result:=true;
  CurrentConfigFile:=aConfigFile;
  // Create fpcup.ini from resource if it doesn't exist yet
  if (CurrentConfigFile=SafeGetApplicationPath+CONFIGFILENAME) then
     result:=SaveInisFromResource(SafeGetApplicationPath+CONFIGFILENAME,'fpcup_ini');
end;

initialization
  IniGeneralSection:=TStringList.create;
  UniModuleList:=TStringList.create;
  UniModuleEnabledList:=TStringList.create;

finalization
  ClearUniModuleList;
  UniModuleList.Free;
  UniModuleEnabledList.Free;
  IniGeneralSection.Free;

end.

