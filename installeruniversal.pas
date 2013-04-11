unit installerUniversal;
{ Universal installer unit driven by .ini file directives
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

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller,processutils,updatelazconfig{$IFDEF MSWINDOWS}, wininstaller{$ENDIF};

{$IFDEF MSWINDOWS}
// On Windows, we can be certain a valid FPC install has
// windres, so use it.
{$R fpcup.rc}
{$ELSE}
// On other platforms we cannot be certain, so we trust either
// - a previous windows compile
// - manual windres invocation
// has updated fpcup.res
{$R fpcup.res}
{$ENDIF MSWINDOWS}

type
  { TUniversalInstaller }

  TUniversalInstaller = class(TInstaller)
  private
    FBinPath:string;
    //FPC base directory - directory where FPC is (to be) installed:
    FFPCDir:string;
    //Lazarus base directory - directory where Lazarus is (to be) installed:
    FLazarusDir:string;
    //Directory where configuration for Lazarus is stored:
    FLazarusPrimaryConfigPath:string;
    InitDone:boolean;
  protected
    // Scans for and adds all packages specified in a (module's) stringlist with commands:
    function AddPackages(sl:TStringList): boolean;
    {$IFDEF MSWINDOWS}
    // Filters (a module's) sl stringlist and creates all <Directive> installers.
    // Directive can now only be Windows/Windows32/Winx86 (synonyms)
    // For now Windows-only; could be extended to generic cross platform installer class once this works
    function CreateInstallers(Directive:string;sl:TStringList;ModuleName:string):boolean;
    {$ENDIF MSWINDOWS}
    function FirstSpaceAfterCommand(CommandLine: string): integer;
    // Get a value for a key=value pair. Case-insensitive for keys. Expands macros in values.
    function GetValue(Key:string;sl:TStringList;recursion:integer=0):string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
    // Installs a single package:
    function InstallPackage(PackagePath, WorkingDir: string): boolean;
    // Scans for and removes all packages specfied in a (module's) stringlist with commands:
    function RemovePackages(sl:TStringList): boolean;
    // Filters (a module's) sl stringlist and runs all <Directive> commands:
    function RunCommands(Directive:string;sl:TStringList):boolean;
    // Uninstall a single package:
    function UnInstallPackage(PackagePath,WorkingDir: string): boolean;
  public
    // FPC base directory
    property FPCDir:string read FFPCDir write FFPCDir;
    // Lazarus primary config path
    property LazarusPrimaryConfigPath:string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
    // Lazarus base directory
    property LazarusDir:string read FLazarusDir write FLazarusDir;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    // Configure module
    function ConfigModule(ModuleName:string): boolean; override;
    // Install update sources
    function GetModule(ModuleName:string): boolean; override;
    // Gets the list of required modules for ModuleName
    function GetModuleRequirements(ModuleName:string; var RequirementList:TStringList): boolean;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

  // Gets the list of modules enabled in ConfigFile. Appends to existing TStringList
  function GetModuleEnabledList(var ModuleList:TStringList):boolean;
  // Gets the sequence representation for all modules in the ini file
  // Used to pass on to higher level code for selection, display etc.
  //todo: get Description field into module list
  function GetModuleList(ConfigFile:string):string;
  // gets alias for keywords in Dictionary.
  //The keyword 'list' is reserved and returns the list of keywords as commatext
  function GetAlias(ConfigFile,Dictionary,keyword: string): string;


var sequences:string;

Const
  CONFIGFILENAME='fpcup.ini';

implementation

uses inifiles,fileutil,fpcuputil;

Const
  MAXSYSMODULES=100;
  MAXUSERMODULES=20;
  // Allow enough instructions per module:
  MAXINSTRUCTIONS=200;
  MAXRECURSIONS=10;

var
  IniGeneralSection:TStringList=nil;
  UniModuleList:TStringList=nil;
  UniModuleEnabledList:TStringlist=nil;

{ TUniversalInstaller }


function TUniversalInstaller.GetValue(Key: string; sl: TStringList;
  recursion: integer): string;
// Look for entries with Key and process macros etc in value
var
  i,len:integer;
  s,macro:string;
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
      if pos('=',s)>0 then
        s:=trim(copy(s,pos('=',s)+1,length(s)));
      break;
      end;
    s:='';
    end;
  if s='' then //search general section
    for i:=0 to IniGeneralSection.Count-1 do
      begin
      s:=IniGeneralSection[i];
      if (copy(UpperCase(s),1, length(Key))=Key) and ((s[length(Key)+1]='=') or (s[length(Key)+1]=' ')) then
        begin
        if pos('=',s)>0 then
          s:=trim(copy(s,pos('=',s)+1,length(s)));
        break;
        end;
      s:='';
      end;
//expand macros
  if s<>'' then
    while pos('$(',s)>0 do
      begin
      i:=pos('$(',s);
      macro:=copy(s,i+2,length(s));
      if pos(')',macro)>0 then
        begin
        delete(macro,pos(')',macro),length(macro));
        macro:=UpperCase(macro);
        len:=length(macro)+3; // the brackets
        // For the directory macros, the user expects to add path separators himself in fpcup.ini, so strip them
        // out if they are there.
        if macro='FPCDIR' then macro:=ExcludeTrailingPathDelimiter(FFPCDir)
        else if macro='LAZARUSDIR' then macro:=ExcludeTrailingPathDelimiter(FLazarusDir)
        else if macro='LAZARUSPRIMARYCONFIGPATH' then macro:=ExcludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)
        else macro:=GetValue(macro,sl,recursion+1); //user defined value
        // quote if containing spaces
        if pos(' ',macro)>0 then
          macro:='"'+macro+'"';
        delete(s,i,len);
        insert(macro,s,i);
        end;
      end;
  // correct path delimiter
  if (pos('URL',Key)<=0) and (pos('ADDTO',Key)<>1)then
    begin
    for i:=1 to length(s) do
      if (s[i]='/') or (s[i]='\') then
        s[i]:=DirectorySeparator;
    end;
  result:=s;
end;

function TUniversalInstaller.InitModule: boolean;
begin
  result:=true;
  infoln('TUniversalInstaller: initialising...',etDebug);
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  // While getting svn etc may help a bit, if Lazarus isn't installed correctly,
  // it probably won't help for normal use cases.
  // However, in theory, we could run only external modules and
  // only download some SVN repositories
  // So.. enable this.
  result:=CheckAndGetNeededExecutables;
  FBinPath:=ExcludeTrailingPathDelimiter(FFPCDir)+'bin'+DirectorySeparator+GetFPCTarget(true);
  InitDone:=result;
end;

function TUniversalInstaller.InstallPackage(PackagePath, WorkingDir: string): boolean;
var
  PackageAbsolutePath: string;
begin
  result:=false;
  // Convert any relative path to absolute path, if it's not just a file/package name:
  if ExtractFileName(PackagePath)=PackagePath then
    PackageAbsolutePath:=PackagePath
  else
    PackageAbsolutePath:=ExpandFilename(PackagePath);
  infoln('InstallPackage: packageabsolutepath: '+PackageAbsolutePath,etDebug);
  ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDir)+'lazbuild'+GetExeExt;
  FErrorLog.Clear;
  if WorkingDir<>'' then
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(WorkingDir);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('--pcp='+FLazarusPrimaryConfigPath);
  ProcessEx.Parameters.Add('--add-package');
  ProcessEx.Parameters.Add(PackageAbsolutePath);
  try
    ProcessEx.Execute;
    result := ProcessEx.ExitStatus=0;
    if not result then
      WritelnLog('InstallerUniversal: error trying to add package '+PackagePath+LineEnding+
        'Details: '+FErrorLog.Text,true);
  except
    on E: Exception do
      begin
      WritelnLog('InstallerUniversal: exception trying to add package '+PackagePath+LineEnding+
        'Details: '+E.Message,true);
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
  PackagePath:string;
  Workingdir:string;
begin
  Failure:=false;
  Workingdir:=GetValue('Workingdir',sl);
  // Go backward; reverse order to deal with any dependencies
  for i:=MAXINSTRUCTIONS downto 1 do
    begin
    PackagePath:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if PackagePath='' then continue;
    // Try to uninstall everything, even if some of these fail.
    if UnInstallPackage(PackagePath,WorkingDir)=false then Failure:=true;
    end;
  result:=Failure;
end;

function TUniversalInstaller.FirstSpaceAfterCommand(CommandLine: string): integer;
  var
    j: integer;
  begin
    //split off command and parameters
    j:=1;
    while j<=length(CommandLine) do
      begin
      if CommandLine[j]='"' then
        repeat  //skip until next quote
          j:=j+1;
        until (CommandLine[j]='"') or (j=length(CommandLine));
      j:=j+1;
      if CommandLine[j]=' ' then break;
      end;
    Result:=j;
  end;

function TUniversalInstaller.AddPackages(sl:TStringList): boolean;
const
  // The command that will be processed:
  Directive='AddPackage';
var
  i:integer;
  PackagePath:string;
  Workingdir:string;
begin
  Workingdir:=GetValue('Workingdir',sl);
  for i:=1 to MAXINSTRUCTIONS do
    begin
    PackagePath:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if PackagePath='' then continue;
    result:=InstallPackage(PackagePath,WorkingDir);
    if not result then
      begin
      infoln('TUniversalInstaller: error while installing package '+PackagePath+'. Stopping',eterror);
      if FVerbose then WritelnLog('TUniversalInstaller: error while installing package '+PackagePath+'. Stopping',false);
      break;
      end;
    end;
end;

{$IFDEF MSWINDOWS}
function TUniversalInstaller.CreateInstallers(Directive: string; sl: TStringList;ModuleName:string): boolean;
var
  i:integer;
  InstallDir,exec,output:string;
  Installer: TWinInstaller;
  Workingdir:string;
begin
  Workingdir:=GetValue('Workingdir',sl);
  for i:=1 to MAXINSTRUCTIONS do
    begin
    exec:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if exec='' then continue;
    case uppercase(exec) of
      'WINDOWS','WINDOWS32','WINX86': {good name};
      else
        begin
        writelnlog('TUniversalInstaller: unknown installer name '+exec+'. Ignoring',true);
        continue;
        end;
    end;

    if FVerbose then WritelnLog('TUniversalInstaller: running CreateInstallers for '+exec,true);
    // Convert any relative path to absolute path:
    InstallDir:=IncludeTrailingPathDelimiter(ExpandFileName(GetValue('InstallDir',sl)));
    if InstallDir<>'' then
      ForceDirectoriesUTF8(InstallDir);
    Installer:=TWinInstaller.Create(InstallDir,FCompiler,FVerbose);
    try
      //todo: make installer module-level; split out config from build part; would also require fixed svn dirs etc
      Installer.FPCDir:=FPCDir;
      Installer.LazarusDir:=FLazarusDir;
      // todo: following not strictly needed:?!?
      Installer.LazarusPrimaryConfigPath:=FLazarusPrimaryConfigPath;
      result:=Installer.BuildModuleCustom(ModuleName);
    finally
      Installer.Free;
    end;

    if not result then
      break;
    end;
end;
{$ENDIF MSWINDOWS}

function TUniversalInstaller.RunCommands(Directive: string;sl:TStringList): boolean;
var
  i:integer;
  exec,output:string;
  Workingdir:string;
begin
  result:=true; //not finding any instructions at all should not be a problem.
  Workingdir:=GetValue('Workingdir',sl);
  for i:=1 to MAXINSTRUCTIONS do
    begin
    exec:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if exec='' then continue;
    if FVerbose then WritelnLog('TUniversalInstaller: running ExecuteCommandInDir for '+exec,true);
    try
      result:=ExecuteCommandInDir(exec,Workingdir,output,FVerbose)=0;
      if not result then
        break;
    except
      on E: Exception do
        begin
        WritelnLog('InstallerUniversal: exception trying to execute '+exec+LineEnding+
          'Details: '+E.Message,true);
        end;
    end;
    end;
end;

function TUniversalInstaller.UnInstallPackage(PackagePath,WorkingDir: string): boolean;
// Todo: add support for workingdir
var
  cnt, i: integer;
  key: string;
  LazarusConfig: TUpdateLazConfig;
  PackageName: string;
  xmlfile: string;
begin
  result:=false;

  PackageName:=ExtractFileNameWithoutExt(ExtractFileNameOnly(PackagePath));
  if FVerbose then WritelnLog('TUniversalInstaller: going to uninstall package '+PackageName,true);
  xmlfile:=PackageConfig;
  key:='UserPkgLinks/Count';
  LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
  try
    cnt:=LazarusConfig.GetVariable(xmlfile, key, 0);
    // check if package is already registered
    i:=cnt;
    while i>0 do
      begin
      // Ignore package name casing
      if UpperCase(LazarusConfig.GetVariable(xmlfile, 'UserPkgLinks/Item'+IntToStr(i)+'/'
        +'Name/Value'))
        =UpperCase(PackageName) then
          break;
      i:=i-1;
      end;
    if i>1 then // found
      begin
      LazarusConfig.SetVariable(xmlfile, key, cnt-1);
      key:='UserPkgLinks/Item'+IntToStr(cnt)+'/';
      while i<cnt do
        begin
        LazarusConfig.MovePath(xmlfile, 'UserPkgLinks/Item'+IntToStr(i+1)+'/',
           'UserPkgLinks/Item'+IntToStr(i)+'/');
        i:=i+1;
        end;
      LazarusConfig.DeletePath(xmlfile, 'UserPkgLinks/Item'+IntToStr(cnt)+'/');
      end;
    xmlfile:='miscellaneousoptions.xml';
    key:='MiscellaneousOptions/BuildLazarusOptions/StaticAutoInstallPackages/'
      +'Count';
    cnt:=LazarusConfig.GetVariable(xmlfile, key, 0);
    // check if package is already registered
    i:=cnt;
    while i>0 do
      begin
      // Ignore package name casing
      if UpperCase(LazarusConfig.GetVariable(xmlfile, 'MiscellaneousOptions/'
        +'BuildLazarusOptions/StaticAutoInstallPackages/Item'+
        IntToStr(i)+'/Value'))
        =UpperCase(PackageName) then
          break;
      i:=i-1;
      end;
    if i>1 then // found
      begin
      LazarusConfig.SetVariable(xmlfile, key, cnt-1);
      key:='MiscellaneousOptions/BuildLazarusOptions/StaticAutoInstallPackages'
        +'/Item'+IntToStr(cnt)+'/';
      while i<cnt do
        begin
        LazarusConfig.MovePath(xmlfile, 'MiscellaneousOptions/'
          +'BuildLazarusOptions/StaticAutoInstallPackages/Item'+IntToStr(i+1)+
            '/',
           'MiscellaneousOptions/BuildLazarusOptions/StaticAutoInstallPackages'
             +'/Item'+IntToStr(i)+'/');
        i:=i+1;
        end;
      LazarusConfig.DeletePath(xmlfile, 'MiscellaneousOptions/'
        +'BuildLazarusOptions/StaticAutoInstallPackages/Item'+IntToStr(cnt)+'/'
          );
      end
  finally
    LazarusConfig.Free;
  end;
  result:=true;
end;

// Runs all InstallExecute<n> commands inside a specified module
function TUniversalInstaller.BuildModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;
begin
  result:=InitModule;
  if not result then exit;
  // Log to console only:
  infoln('TUniversalInstaller: building module '+ModuleName+'...',etInfo);
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);

    // Run all InstallExecute<n> commands:
    // More detailed logging only if verbose or debug:
    if FVerbose then WritelnLog('TUniversalInstaller: building module '+ModuleName+' running all InstallExecute commands in: '+LineEnding+
      sl.text,true);
    result:=RunCommands('InstallExecute',sl);

    // Run all CreateInstaller<n> commands; for now Windows only
    {$IFDEF MSWINDOWS}
    if FVerbose then WritelnLog('TUniversalInstaller: building module '+ModuleName+' running all CreateInstaller commands in: '+LineEnding+
      sl.text,true);
    result:=CreateInstallers('CreateInstaller',sl, ModuleName);
    {$ENDIF MSWINDOWS}
    end
  else
    result:=false;
end;

function TUniversalInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=true;
end;

// Processes a single module (i.e. section in fpcup.ini)
function TUniversalInstaller.ConfigModule(ModuleName: string): boolean;
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
  begin
  filename:=xmlfile+'.xml';
  oldcounter:='';
  for i:=1 to MAXINSTRUCTIONS do
    begin
    // Read command, e.g. AddToHelpOptions1
    // and deduce which XML settings file to update
    exec:=GetValue('AddTo'+xmlfile+IntToStr(i),sl);
    // Skip over missing numbers:
    if exec='' then continue;
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
        insert(inttostr(count),key,k);
        k:=pos('#',key);
        end;
      LazarusConfig.SetVariable(filename,key,trim(copy(exec,j+1,length(exec))));
      end;
    if not result then
      break;
    end;
  end;

begin
// Add values to lazarus config files. Syntax:
// AddTo<filename><number>=key[@counter]:value
// filename: xml file to update in --primary-config-path. The list of files is limited to the list below for security reasons.
// number: command number, starting from 1 for every file. The numbers have to be sequential. Scanning stops at the first missing number.
// key: the attribute to change in the format aa/bb/cc
// counter: the attribute key for the counter used to keep track of lists. Used to insert a new value in a list. Read and incremented by 1;
//          When using a counter, <key> can use a the '#' character as a placeholder for the new count written to <counter>
// value:  the string value to store in <key>.
  result:=InitModule;
  if not result then exit;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
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
          AddToLazXML('environmentoptions'); //general options
          AddToLazXML('helpoptions');
          AddToLazXML('miscellaneousoptions'); //e.g. list of packages to be installed on recompile
          AddToLazXML('packagefiles'); //e.g. list of available packages

          // Process specials
          Directive:=GetValue('RegisterExternalTool',sl);
          if Directive<>'' then
            begin
            xmlfile:=EnvironmentConfig;
            key:='EnvironmentOptions/ExternalTools/Count';
            cnt:=LazarusConfig.GetVariable(xmlfile,key,0);
            // check if tool is already registered
            i:=cnt;
            while i>0 do
              begin
              if LazarusConfig.GetVariable(xmlfile,'EnvironmentOptions/ExternalTools/Tool'+IntToStr(i)+'/Title/Value')
                =ModuleName then
                  break;
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
            infoln('Going to register external tool '+Directive+GetExeExt,etDebug);
            LazarusConfig.SetVariable(xmlfile,key+'Filename/Value',Directive+GetExeExt);

            // If we're registering external tools, we should look for associated/
            // detailed directives as well:
            Directive:=GetValue('RegisterExternalToolCmdLineParams',sl);
            if Directive<>'' then
              LazarusConfig.SetVariable(xmlfile,key+'CmdLineParams/Value',Directive);
            Directive:=GetValue('RegisterExternalToolWorkingDirectory',sl);
            if Directive<>'' then
              LazarusConfig.SetVariable(xmlfile,key+'WorkingDirectory/Value',Directive);
            Directive:=GetValue('RegisterExternalToolScanOutputForFPCMessages',sl);
            if (Directive<>'') and (Directive<>'0') then // default = false
              LazarusConfig.SetVariable(xmlfile,key+'ScanOutputForFPCMessages/Value','True')
            else
              LazarusConfig.DeleteVariable(xmlfile,key+'ScanOutputForFPCMessages/Value');
            Directive:=GetValue('RegisterExternalToolScanOutputForMakeMessages',sl);
            if (Directive<>'') and (Directive<>'0') then // default = false
              LazarusConfig.SetVariable(xmlfile,key+'ScanOutputForMakeMessages/Value','True')
            else
              LazarusConfig.DeleteVariable(xmlfile,key+'ScanOutputForMakeMessages/Value');
            Directive:=GetValue('RegisterExternalToolHideMainForm',sl);
            if Directive='0' then // default = true
              LazarusConfig.SetVariable(xmlfile,key+'HideMainForm/Value','False')
            else
              LazarusConfig.DeleteVariable(xmlfile,key+'HideMainForm/Value');
            end;

          Directive:=GetValue('RegisterHelpViewer',sl);
          if Directive<>'' then
            begin
            xmlfile:=HelpConfig;
            key:='Viewers/TChmHelpViewer/CHMHelp/Exe';
            infoln('Going to register help viewer '+Directive+GetExeExt,etDebug);
            LazarusConfig.SetVariable(xmlfile,key,Directive+GetExeExt);
            end;

          // Register path to help source if given
          Directive:=GetValue('RegisterLazDocPath',sl);
          if Directive<>'' then
            begin
            infoln('Going to add docpath '+Directive,etDebug);
            LazDocPathAdd(Directive, LazarusConfig);
            end;
        except
          on E: Exception do
          begin
            if Directive='' then
              writelnlog('ERROR: Universal installer: exception '+E.ClassName+'/'+E.Message+' configuring module: '+ModuleName, true)
            else
              writelnlog('ERROR: Universal installer: exception '+E.ClassName+'/'+E.Message+' configuring module: '+ModuleName+' (parsing directive:'+Directive+')', true);
          end;
        end;
      finally
        LazarusConfig.Destroy;
      end;
    end
  else
    result:=false;
end;

// Download from SVN, hg, git for module
function TUniversalInstaller.GetModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;
  RemoteURL,InstallDir:string;
  BeforeRevision, AfterRevision: string;
  UpdateWarnings: TStringList;
begin
  result:=InitModule;
  if not result then exit;
  result:=true;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    WritelnLog('Getting module '+ModuleName);
    InstallDir:=GetValue('InstallDir',sl);
    if InstallDir<>'' then
      ForceDirectoriesUTF8(InstallDir);
    // Handle SVN urls
    RemoteURL:=GetValue('SVNURL',sl);
    if RemoteURL<>'' then
      begin
      infoln('Going to download/update from SVN repository '+RemoteURL,etDebug);
      UpdateWarnings:=TStringList.Create;
        try
          FSVNClient.Verbose:=FVerbose;
          FBaseDirectory:=InstallDir;
          FUrl:=RemoteURL;
          result:=DownloadFromSVN(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          if UpdateWarnings.Count>0 then
          begin
            WritelnLog(UpdateWarnings.Text);
          end;
        finally
          UpdateWarnings.Free;
        end;
      end;

    // Handle HG URLs
    RemoteURL:=GetValue('HGURL',sl);
    if RemoteURL<>'' then
      begin
      UpdateWarnings:=TStringList.Create;
        try
          FHGClient.Verbose:=FVerbose;
          FBaseDirectory:=InstallDir;
          FUrl:=RemoteURL;
          result:=DownloadFromHG(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          if UpdateWarnings.Count>0 then
          begin
            WritelnLog(UpdateWarnings.Text);
          end;
        finally
          UpdateWarnings.Free;
        end;
      end;

    // Handle Git URLs
    RemoteURL:=GetValue('GITURL',sl);
    {todo: handle branches (e.g. tiopf doesn't use master branch), perhaps a space after the url and then branch name?
    Similar construction could be used for hg. Suggest leaving svn as is}
    if RemoteURL<>'' then
      begin
      UpdateWarnings:=TStringList.Create;
        try
          FGitClient.Verbose:=FVerbose;
          FBaseDirectory:=InstallDir;
          FUrl:=RemoteURL;
          result:=DownloadFromGit(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          if UpdateWarnings.Count>0 then
          begin
            WritelnLog(UpdateWarnings.Text);
          end;
        finally
          UpdateWarnings.Free;
        end;
      end;
    end
  else
    result:=false;
end;

function TUniversalInstaller.GetModuleRequirements(ModuleName: string;
  var RequirementList: TStringList): boolean;
begin
result:=InitModule;
if not result then exit;

end;

// Runs all UnInstallExecute<n> commands inside a specified module
function TUniversalInstaller.UnInstallModule(ModuleName: string): boolean;
var
  idx,cnt,i:integer;
  sl:TStringList;
  Directive,xmlfile,key,keyfrom:string;
  LazarusConfig:TUpdateLazConfig;
begin
  result:=InitModule;
  if not result then exit;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    WritelnLog('UnInstalling module '+ModuleName);
    result:=RunCommands('UnInstallExecute',sl);

    // Process all AddPackage<n> directives in reverse.
    // As this changes config files, we keep it outside
    // the section where LazarusConfig is modified
    RemovePackages(sl);

    LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
    try

    // Process specials
    Directive:=GetValue('RegisterExternalTool',sl);
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

    Directive:=GetValue('RegisterHelpViewer',sl);
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
    end
  else
    result:=false;
end;

constructor TUniversalInstaller.Create;
begin
  inherited Create;
end;

destructor TUniversalInstaller.Destroy;
begin
  inherited Destroy;
end;


procedure ClearUniModuleList;
var
  i:integer;
begin
  for i:=0 to UniModuleList.Count -1 do
    TStringList(UniModuleList.Objects[i]).free;
end;

procedure SaveIniFromResource(filename:string);
var fs:Tfilestream;
begin
with TResourceStream.Create(hInstance, 'fpcup_ini', 'file') do
try
  try
    fs:=Tfilestream.Create(Filename,fmCreate);
    savetostream(fs);
  finally
     fs.Free;
  end;
finally
  Free;
end;
end;

function GetAlias(ConfigFile,Dictionary,KeyWord: string): string;
var
  ini:TMemIniFile;
  sl:TStringList;
  e:Exception;
begin
sl:=TStringList.Create;
ini:=TMemIniFile.Create(ConfigFile);
ini.CaseSensitive:=false;
try
  ini.ReadSection('ALIAS'+Dictionary,sl);
  if Uppercase(KeyWord)='LIST' then
    result:=sl.CommaText
  else
    begin
    result:=ini.ReadString('ALIAS'+Dictionary,KeyWord,'');
    if result='' then
      begin
      e:=Exception.CreateFmt('--%s=%s : Invalid keyword. Accepted keywords are: %s',[Dictionary,KeyWord,sl.CommaText]);
      raise e;
      end;
    end;
finally
  ini.Free;
  sl.free;
end;
end;

function GetModuleList(ConfigFile: string): string;
var
  ini:TMemIniFile;
  i,j,maxmodules:integer;
  val,name,req:string;

  function LoadModule(ModuleName:string):boolean;
  var
    name,description,val:string;
    i:integer;
    sl:TStringList;
  begin
    name:=ini.ReadString(ModuleName,'Name','');
    result:=name<>'';
    if result then
      begin
      if ini.ReadString(ModuleName,'Enabled','')='1' then
        UniModuleEnabledList.Add(uppercase(name));
      // store the section as is and attach as object to UniModuleList
      // TstringList cleared in finalization
      sl:=TstringList.Create;
      ini.ReadSectionRaw(ModuleName,sl);
      UniModuleList.AddObject(name,TObject(sl));
      end;
  end;

  function CreateModuleSequence(ModuleName:string):string;
  var
    name,req:string;
  begin
    result:='';
    name:=ini.ReadString(ModuleName,'Name','');
    if name<>'' then
      begin
      req:=ini.ReadString(ModuleName,'requires','');
      if req<>'' then
        begin
        req:='Requires '+req+';';
        req:=StringReplace(req, ',', '; Requires ', [rfReplaceAll,rfIgnoreCase]);
        end;
      result:='Declare '+ name + ';' + req +
          'Cleanmodule '+ name +';' +
          'Getmodule '+ name +';' +
          'Buildmodule '+ name +';' +
          'Configmodule '+ name +';' +
          'End;'+
          'Declare '+ name + 'clean;'+
          'Cleanmodule '+ name +';' +
          'End;'+
          'Declare '+ name + 'uninstall;'+
          'Uninstallmodule '+ name +';' +
          'End;';
      end;
  end;

begin
  result:='';
  // Create fpcup.ini from resource if it doesn't exist yet
  if (ConfigFile=ExtractFilePath(ParamStr(0))+installerUniversal.CONFIGFILENAME)
    and not FileExistsUTF8(CONFIGFILENAME) then
    SaveIniFromResource(CONFIGFILENAME);
  ini:=TMemIniFile.Create(ConfigFile);
  ini.CaseSensitive:=false;
  ini.StripQuotes:=true; //helps read description lines

  // parse inifile
  try
    maxmodules:=ini.ReadInteger('General','MaxSysModules',MAXSYSMODULES);
    ini.ReadSectionRaw('General',IniGeneralSection);
    for i:=1 to maxmodules do
      if LoadModule('FPCUPModule'+IntToStr(i)) then
        result:=result+CreateModuleSequence('FPCUPModule'+IntToStr(i));
    maxmodules:=ini.ReadInteger('General','MaxUserModules',MAXUSERMODULES);
    for i:=1 to maxmodules do
      if LoadModule('UserModule'+IntToStr(i))then
        result:=result+CreateModuleSequence('UserModule'+IntToStr(i));
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
    // create the sequences for default modules
    result:=result+'DeclareHidden UniversalDefault;';
    for i:=0 to UniModuleEnabledList.Count-1 do
      result:=result+'Do '+UniModuleEnabledList[i]+';';
    result:=result+'End;';
    result:=result+'DeclareHidden UniversalDefaultClean;';
    for i:=0 to UniModuleEnabledList.Count-1 do
      result:=result+'Do '+UniModuleEnabledList[i]+'Clean;';
    result:=result+'End;';
    result:=result+'DeclareHidden UniversalDefaultUninstall;';
    for i:=0 to UniModuleEnabledList.Count-1 do
      result:=result+'Do '+UniModuleEnabledList[i]+'Uninstall;';
    result:=result+'End;';
  finally
    ini.Free;
  end;
end;

function GetModuleEnabledList(var ModuleList: TStringList): boolean;
var i:integer;
begin
  for i:=0 to UniModuleEnabledList.Count -1 do
    ModuleList.Add(UniModuleEnabledList[i]);
end;


initialization
 IniGeneralSection:=TStringList.create;
 UniModuleList:=TStringList.create;
 UniModuleEnabledList:=TStringList.create;
finalization
ClearUniModuleList;
UniModuleList.free;
UniModuleEnabledList.free;
IniGeneralSection.Free;
end.

