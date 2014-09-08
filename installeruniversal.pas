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

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils, updatelazconfig
  {$IFDEF MSWINDOWS}, wininstaller{$ENDIF};

{$IFDEF MSWINDOWS}
// On Windows, we can be certain a valid FPC install has
// windres, so use it.
{$R fpcup.rc}
{$ELSE}
// On other platforms we cannot be certain, so we hope either
// - a previous windows compile
// - manual windres invocation
// has updated fpcup.res
{$R fpcup.res}
{$ENDIF MSWINDOWS}

type
  { TUniversalInstaller }

  TUniversalInstaller = class(TInstaller)
  private
    FBinPath:string; //Path where compiler is
    // FPC base directory - directory where FPC is (to be) installed:
    FFPCDir:string;
    // Compiler options chosen by user to build Lazarus. There is a CompilerOptions property,
    // but let's leave that for use with FPC.
    FLazarusCompilerOptions:string;
    // Lazarus base directory - directory where Lazarus is (to be) installed:
    FLazarusDir:string;
    // Keep track of whether Lazarus needs to be rebuilt after package installation
    // or running lazbuild with an .lpk
    FLazarusNeedsRebuild:boolean;
    // Directory where configuration for Lazarus is stored:
    FLazarusPrimaryConfigPath:string;
    FPath:string; //Path to be used within this session (e.g. including compiler path)
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
    function UnInstallPackage(PackagePath: string): boolean;
  public
    // FPC base directory
    property FPCDir:string read FFPCDir write FFPCDir;
    // Compiler options user chose to compile Lazarus with (coming from fpcup).
    property LazarusCompilerOptions: string write FLazarusCompilerOptions;
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
    // Install/update sources (e.g. via svn)
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
      if (copy(UpperCase(s),1, length(Key))=Key) and ((s[length(Key)+1]='=') or
        (s[length(Key)+1]=' ')) then
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
        // For the directory macros, the user expects to add path separators himself in fpcup.ini,
        // so strip them out if they are there.
        if macro='FPCDIR' then //$(FPCDIR)
          macro:=ExcludeTrailingPathDelimiter(FFPCDir)
        else if macro='GETEXEEXT' then //$(GETEXEEXT)
          macro:=GetExeExt
        else if macro='LAZARUSDIR' then //$(LAZARUSDIR)
          macro:=ExcludeTrailingPathDelimiter(FLazarusDir)
        else if macro='LAZARUSPRIMARYCONFIGPATH' then //$(LAZARUSPRIMARYCONFIGPATH)
          macro:=ExcludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)
        else if macro='STRIPDIR' then //$(STRIPDIR)
          {$IFDEF MSWINDOWS}
          // Strip is a binutil and should be located in the make dir
          macro:=ExcludeTrailingPathDelimiter(FMakeDir)
          {$ENDIF}
          {$IFDEF UNIX}
          // Strip can be anywhere in the path
          macro:=ExcludeTrailingPathDelimiter(ExtractFilePath(Which('strip')))
          {$ENDIF}
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
var
  PlainBinPath: string; //the directory above e.g. c:\development\fpc\bin\i386-win32
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
  if not(result) then
    infoln('Universalinstaller: missing required executables. Aborting.',etError);
  if not(FileExistsUTF8(IncludeTrailingPathDelimiter(LazarusDir)+'lazbuild'+GetExeExt)) then
  begin
    result:=false;
    infoln('Universalinstaller: missing lazbuild. Aborting.',etError);
  end;

  // Add fpc architecture bin and plain paths
  FBinPath:=IncludeTrailingPathDelimiter(FFPCDir)+'bin'+DirectorySeparator+GetFPCTarget(true);
  PlainBinPath:=IncludeTrailingPathDelimiter(FFPCDir)+'bin';
  // Need to remember because we don't always use ProcessEx
  FPath:=FBinPath+PathSeparator+
    PlainBinPath+PathSeparator;
  SetPath(FPath,true,false);
  // No need to build Lazarus IDE again right now; will
  // be changed by buildmodule/configmodule installexecute/
  // installpackage
  FLazarusNeedsRebuild:=false;
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
    PackageAbsolutePath:=SafeExpandFileName(PackagePath);
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
    if result then
    begin
      infoln('Marking Lazarus for rebuild based on package install for '+PackageAbsolutePath,etDebug);
      FLazarusNeedsRebuild:=true; //Mark IDE for rebuild
    end
    else
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
    // Note: UninstallPackage used to have a WorkingDir parameter but
    // I'm wondering how to implement that as we have PackagePath already.
    if UnInstallPackage(PackagePath)=false then Failure:=true;
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
// Create installers
// For now only support WINDOWS/WINDOWS32/WIN32/WINX86, and ignore others
var
  i:integer;
  InstallDir,exec,output:string;
  Installer: TWinInstaller;
  Workingdir:string;
begin
  result:=true; //succeed by default
  Workingdir:=GetValue('Workingdir',sl);
  for i:=1 to MAXINSTRUCTIONS do
    begin
    exec:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if exec='' then continue;
    case uppercase(exec) of
      'WINDOWS','WINDOWS32','WIN32','WINX86': {good name};
      else
        begin
        writelnlog('TUniversalInstaller: ignoring unknown installer name '+exec+'.',true);
        continue;
        end;
    end;

    if FVerbose then WritelnLog('TUniversalInstaller: running CreateInstallers for '+exec,true);
    // Convert any relative path to absolute path:
    InstallDir:=IncludeTrailingPathDelimiter(SafeExpandFileName(GetValue('InstallDir',sl)));
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
      begin
      WritelnLog('TUniversalInstaller: CreateInstallers for '+exec+' failed. Stopping installer creation.',true);
      break; //fail on first installer failure
      end;
    end;
end;
{$ENDIF MSWINDOWS}

function TUniversalInstaller.RunCommands(Directive: string;sl:TStringList): boolean;
var
  i:integer;
  exec:string;
  output:string='';
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
      result:=ExecuteCommandInDir(exec,Workingdir,output,FVerbose,FPath)=0;
      if result then
      begin
        // If it is likely user used lazbuid to compile a package, assume
        // it is design-time (no way to check) and mark IDE for rebuild
        if (pos('lazbuild',lowerCase(exec))>0) and
          (pos('.lpk',lowercase(exec))>0) then
        begin
          infoln('Marking Lazarus for rebuild based on exec line '+exec,etDebug);
          FLazarusNeedsRebuild:=true;
        end;
      end
      else
      begin
        WritelnLog('InstallerUniversal: warning: running '+exec+' returned an error.',true);
        break;
      end;
    except
      on E: Exception do
        begin
        WritelnLog('InstallerUniversal: exception trying to execute '+exec+LineEnding+
          'Details: '+E.Message,true);
        end;
    end;
    end;
end;

function TUniversalInstaller.UnInstallPackage(PackagePath: string): boolean;
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
      FLazarusNeedsRebuild:=true;
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
      FLazarusNeedsRebuild:=true;
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
{ todo: Note that for some reason the installpackage etc commands are processed in configmodule.
Shouldn't this be changed? }
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

          // Process special directives
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

      // If Lazarus was marked for rebuild, do so:
      if FLazarusNeedsRebuild then
      begin
        infoln('InstallerUniversal: going to rebuild Lazarus because packages were installed.',etInfo);
        ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDir)+'lazbuild'+GetExeExt;
        FErrorLog.Clear;
        ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(LazarusDir);
        ProcessEx.Parameters.Clear;
        ProcessEx.Parameters.Add('--pcp='+FLazarusPrimaryConfigPath);
        ProcessEx.Parameters.Add('--build-ide=-dKeepInstalledPackages ' + FLazarusCompilerOptions);
        ProcessEx.Parameters.Add('--build-mode=');
        try
          ProcessEx.Execute;
          result := ProcessEx.ExitStatus=0;
          if result then
          begin
            infoln('InstallerUniversal: Lazarus rebuild succeeded',etDebug);
            FLazarusNeedsRebuild:=false;
          end
          else
            WritelnLog('InstallerUniversal: error trying to rebuild Lazarus. '+LineEnding+
              'Details: '+FErrorLog.Text,true);
        except
          on E: Exception do
            begin
            WritelnLog('InstallerUniversal: exception trying to rebuild Lazarus '+LineEnding+
              'Details: '+E.Message,true);
            result:=false;
            end;
        end;
      end;
    end
  else
    begin
    // Could not find module in module list
    writelnlog('ERROR: Universal installer: could not find specified module '+ModuleName,true);
    result:=false;
    end;
end;

// Download from SVN, hg, git for module
function TUniversalInstaller.GetModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;
  RemoteURL,InstallDir:string;
  PinRevision: string=''; //Pin at a certain revision number
  BeforeRevision: string='';
  AfterRevision: string='';
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
    // Common keywords for all repo methods
    PinRevision:=GetValue('REVISION',sl);
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
          if PinRevision<>'' then
            FSVNClient.DesiredRevision:=PinRevision;
          result:=DownloadFromSVN(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          if result=false then
            WritelnLog('SVN error downloading from '+RemoteURL+'. Continuing regardless.',true);
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
          if PinRevision<>'' then
            FHGClient.DesiredRevision:=PinRevision;
          result:=DownloadFromHG(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          if result=false then
            WritelnLog('hg error downloading from '+RemoteURL+'. Continuing regardless.',true);
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
          if PinRevision<>'' then
            FGitClient.DesiredRevision:=PinRevision;
          result:=DownloadFromGit(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
          if result=false then
            WritelnLog('git error downloading from '+RemoteURL+'. Continuing regardless.',true);
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
//todo: what are we supposed to do with Requirementslist?
  result:=InitModule;
  if not result then exit;
end;

// Runs all UnInstallExecute<n> commands inside a specified module
function TUniversalInstaller.UnInstallModule(ModuleName: string): boolean;
var
  idx,cnt,i:integer;
  sl:TStringList;
  Directive,xmlfile,key:string;
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

    // If Lazarus was marked for rebuild, do so:
    if FLazarusNeedsRebuild then
    begin
      infoln('InstallerUniversal: going to rebuild Lazarus because packages were uninstalled.',etInfo);
      ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDir)+'lazbuild'+GetExeExt;
      FErrorLog.Clear;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(LazarusDir);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--pcp='+FLazarusPrimaryConfigPath);
      ProcessEx.Parameters.Add('--build-ide=-dKeepInstalledPackages ' + FLazarusCompilerOptions);
      ProcessEx.Parameters.Add('--build-mode=');
      try
        ProcessEx.Execute;
        result := ProcessEx.ExitStatus=0;
        if result then
        begin
          infoln('InstallerUniversal: Lazarus rebuild succeeded',etDebug);
          FLazarusNeedsRebuild:=false;
        end
        else
          WritelnLog('InstallerUniversal: error trying to rebuild Lazarus. '+LineEnding+
            'Details: '+FErrorLog.Text,true);
      except
        on E: Exception do
          begin
          WritelnLog('InstallerUniversal: exception trying to rebuild Lazarus '+LineEnding+
            'Details: '+E.Message,true);
          result:=false;
          end;
      end;
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
  val,name:string;

  function LoadModule(ModuleName:string):boolean;
  var
    name:string;
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
    SaveInisFromResource(CONFIGFILENAME,'fpcup_ini');
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
  result:=false;
  for i:=0 to UniModuleEnabledList.Count -1 do
    ModuleList.Add(UniModuleEnabledList[i]);
  result:=true;
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

