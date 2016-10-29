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

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils
  {$ifndef FPCONLY}
  ,updatelazconfig
  {$endif}
  {$IFDEF MSWINDOWS}, wininstaller{$ENDIF};

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

  TUniversalInstaller = class(TInstaller)
  private
    FBinPath:string; //Path where compiler is
    // FPC base directory - directory where FPC is (to be) installed:
    FFPCDir:string;
    {$ifndef FPCONLY}
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
    {$endif}
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
    {$ifndef FPCONLY}
    // Installs a single package:
    function InstallPackage(PackagePath, WorkingDir: string): boolean;
    // Scans for and removes all packages specfied in a (module's) stringlist with commands:
    function RemovePackages(sl:TStringList): boolean;
    // Uninstall a single package:
    function UnInstallPackage(PackagePath: string): boolean;
    {$endif}
    // Filters (a module's) sl stringlist and runs all <Directive> commands:
    function RunCommands(Directive:string;sl:TStringList):boolean;
  public
    // FPC base directory
    property FPCDir:string read FFPCDir write FFPCDir;
    {$ifndef FPCONLY}
    // Compiler options user chose to compile Lazarus with (coming from fpcup).
    property LazarusCompilerOptions: string write FLazarusCompilerOptions;
    // Lazarus primary config path
    property LazarusPrimaryConfigPath:string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
    // Lazarus base directory
    property LazarusDir:string read FLazarusDir write FLazarusDir;
    // Build module
    {$endif}
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
  function GetModuleList:string;
  // gets alias for keywords in Dictionary.
  //The keyword 'list' is reserved and returns the list of keywords as commatext
  function GetAlias(Dictionary,keyword: string): string;
  // check if enabled modules are allowed !
  function CheckIncludeModule(ModuleName: string):boolean;
  procedure SetConfigFile(aConfigFile: string);

var
  sequences:string;

Const
  CONFIGFILENAME='fpcup.ini';
  SETTTINGSFILENAME='settings.ini';

implementation

uses
  StrUtils,inifiles, FileUtil, LazFileUtils, LazUTF8, fpcuputil;

Const
  MAXSYSMODULES=200;
  MAXUSERMODULES=20;
  // Allow enough instructions per module:
  MAXINSTRUCTIONS=200;
  MAXRECURSIONS=10;

var
  CurrentConfigFile:string;
  IniGeneralSection:TStringList=nil;
  UniModuleList:TStringList=nil;
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
        else if macro='FPCBINDIR' then //$(FPCBINDIR)
            macro:=ExcludeTrailingPathDelimiter(FBinPath)
        else if macro='TOOLDIR' then //$(TOOLDIR)
          {$IFDEF MSWINDOWS}
          // make is a binutil and should be located in the make dir
          macro:=ExcludeTrailingPathDelimiter(FMakeDir)
          {$ENDIF}
          {$IFDEF UNIX}
          // Strip can be anywhere in the path
          macro:=ExcludeTrailingPathDelimiter(ExtractFilePath(Which('make')))
          {$ENDIF}
        else if macro='GETEXEEXT' then //$(GETEXEEXT)
          macro:=GetExeExt
        {$ifndef FPCONLY}
        else if macro='LAZARUSDIR' then //$(LAZARUSDIR)
          macro:=ExcludeTrailingPathDelimiter(FLazarusDir)
        else if macro='LAZARUSPRIMARYCONFIGPATH' then //$(LAZARUSPRIMARYCONFIGPATH)
          macro:=ExcludeTrailingPathDelimiter(FLazarusPrimaryConfigPath)
        {$endif}
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
  result:=(CheckAndGetNeededExecutables) AND (CheckAndGetNeededBinUtils);
  if not(result) then
    infoln('Universalinstaller: missing required executables. Aborting.',etError);

  {$ifndef FPCONLY}
  if not(FileExistsUTF8(IncludeTrailingPathDelimiter(LazarusDir)+'lazbuild'+GetExeExt)) then
  begin
    result:=false;
    infoln('Universalinstaller: missing lazbuild. Aborting.',etError);
  end;
  {$endif}

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
  {$ifndef FPCONLY}
  FLazarusNeedsRebuild:=false;
  {$endif}
  InitDone:=result;
end;

{$ifndef FPCONLY}
function TUniversalInstaller.InstallPackage(PackagePath, WorkingDir: string): boolean;
var
  PackageName,PackageAbsolutePath: string;
  Path: String;
  lpkdoc:TConfig;
  lpkversion:TAPkgVersion;
  TxtFile:TextFile;
begin
  result:=false;
  PackageName:=ExtractFileNameWithoutExt(ExtractFileNameOnly(PackagePath));

  // Convert any relative path to absolute path, if it's not just a file/package name:
  if ExtractFileName(PackagePath)=PackagePath then
    PackageAbsolutePath:=PackagePath
  else
    PackageAbsolutePath:=SafeExpandFileName(PackagePath);

  // if only a filename (without path) is given, then lazarus will handle everything by itself
  // set lpkversion.Name to 'unknown' to flag this case
  // if not, get some extra info from package file !!
  lpkversion.Name:='unknown';
  if (ExtractFileName(PackagePath)<>PackagePath) then
  begin
    lpkdoc:=TConfig.Create(PackageAbsolutePath);
    try
      Path:='Package/';
      lpkversion.FileVersion:=lpkdoc.GetValue(Path+'Version',0);
      Path:='Package/Name/';
      lpkversion.Name:=lpkdoc.GetValue(Path+'Value','unknown');
      Path:='Package/Version/';
      lpkversion.GetVersion(lpkdoc,Path);
    finally
      lpkdoc.Free;
    end;
  end;

  if lpkversion.Name='unknown'
     then WritelnLog('Installing '+PackageName,True)
     else WritelnLog('Installing '+PackageName+' version '+lpkversion.AsString,True);

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
    // runtime packages will return false, but output will have info about package being "only for runtime"
    if result then
    begin
      infoln('Marking Lazarus for rebuild based on package install for '+PackageAbsolutePath,etDebug);
      FLazarusNeedsRebuild:=true; //Mark IDE for rebuild
    end
    else
    begin
      // if the package is only for runtime, just add an lpl file to inform Lazarus of its existence and location ->> set result to true
      if Pos('only for runtime',ProcessEx.OutputString)>0
         then result:=True
         else WritelnLog('InstallerUniversal: error trying to add package '+PackageName+LineEnding+'Details: '+FErrorLog.Text,true);
    end;
  except
    on E: Exception do
      begin
      WritelnLog('InstallerUniversal: exception trying to add package '+PackageName+LineEnding+
        'Details: '+E.Message,true);
      end;
  end;

  // all ok AND a filepath is given --> check / add lpl file to inform Lazarus of package excistence and location
  // if only a filename (without path) is given, then lazarus will handle everything (including lpl) by itself
  // in fact, we cannot do anything in that case : we do not know anything about the package !
  if (result) AND (lpkversion.Name<>'unknown') then
  begin
    if FVerbose then WritelnLog('TUniversalInstaller: checking lpl file for '+PackageName,true);
    Path := IncludeTrailingPathDelimiter(LazarusDir)+
            'packager'+DirectorySeparator+
            'globallinks'+DirectorySeparator+
            LowerCase(lpkversion.Name)+'-'+lpkversion.AsString+'.lpl';

    if NOT FileExists(Path) then
    begin
      AssignFile(TxtFile,Path);
      try
        Rewrite(TxtFile);
        writeln(TxtFile,PackageAbsolutePath);
      finally
        CloseFile(TxtFile);
      end;
      if FVerbose then WritelnLog('Created lpl file ('+Path+') with contents: '+PackageAbsolutePath,true);
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
  BaseWorkingdir:string;
begin
  Failure:=false;
  BaseWorkingdir:=GetValue('Workingdir',sl);
  // Go backward; reverse order to deal with any dependencies
  for i:=MAXINSTRUCTIONS downto 0 do
  begin
    if i=0
       then PackagePath:=GetValue(Directive,sl)
       else PackagePath:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if PackagePath='' then continue;
    if NOT FileExists(PackagePath) then continue;
    Workingdir:=GetValue('Workingdir'+IntToStr(i),sl);
    if Workingdir='' then Workingdir:=BaseWorkingdir;
    // Try to uninstall everything, even if some of these fail.
    // Note: UninstallPackage used to have a WorkingDir parameter but
    // I'm wondering how to implement that as we have PackagePath already.
    if UnInstallPackage(PackagePath)=false then Failure:=true;
  end;
  result:=Failure;
end;
{$endif}

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
  Location='Workingdir';
var
  i,j:integer;
  PackagePath:string;
  Workingdir:string;
  BaseWorkingdir:string;
  RealDirective:string;
begin

  RealDirective:=Directive;
  PackagePath:=GetValue(RealDirective,sl);
  BaseWorkingdir:=GetValue(Location,sl);

  // trick: run from -1 to allow the above basic statements to be processed first
  for i:=-1 to MAXINSTRUCTIONS do
  begin
    if i>=0 then
    begin
      RealDirective:=Directive+IntToStr(i);
      PackagePath:=GetValue(RealDirective,sl);
      Workingdir:=GetValue(Location+IntToStr(i),sl);
    end;
    // Skip over missing data:
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
    if Workingdir='' then Workingdir:=BaseWorkingdir;

    {$ifndef FPCONLY}
    result:=InstallPackage(PackagePath,WorkingDir);
    if not result then
    begin
      infoln('TUniversalInstaller: error while installing package '+PackagePath+'. Stopping',eterror);
      if FVerbose then WritelnLog('TUniversalInstaller: error while installing package '+PackagePath+'. Stopping',false);
      break;
    end;
    {$endif}
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
  BaseWorkingdir:string;
begin
  result:=true; //succeed by default
  BaseWorkingdir:=GetValue('Workingdir',sl);
  for i:=0 to MAXINSTRUCTIONS do
    begin
    if i=0
       then exec:=GetValue(Directive,sl)
       else exec:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if exec='' then continue;
    Workingdir:=GetValue('Workingdir'+IntToStr(i),sl);
    if Workingdir='' then Workingdir:=BaseWorkingdir;
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
      {$ifndef FPCONLY}
      Installer.LazarusDir:=FLazarusDir;
      // todo: following not strictly needed:?!?
      Installer.LazarusPrimaryConfigPath:=FLazarusPrimaryConfigPath;
      {$endif}
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
  BaseWorkingdir:string;
  Workingdir:string;
begin
  result:=true; //not finding any instructions at all should not be a problem.
  BaseWorkingdir:=GetValue('Workingdir',sl);
  for i:=0 to MAXINSTRUCTIONS do
    begin
    if i=0
       then exec:=GetValue(Directive,sl)
       else exec:=GetValue(Directive+IntToStr(i),sl);
    // Skip over missing numbers:
    if exec='' then continue;
    Workingdir:=GetValue('Workingdir'+IntToStr(i),sl);
    if Workingdir='' then Workingdir:=BaseWorkingdir;
    if FVerbose then WritelnLog('TUniversalInstaller: running ExecuteCommandInDir for '+exec,true);
    try
      result:=ExecuteCommandInDir(exec,Workingdir,output,FVerbose,FPath)=0;
      if result then
      begin
        {$ifndef FPCONLY}
        // If it is likely user used lazbuid to compile a package, assume
        // it is design-time (except when returning an runtime message) and mark IDE for rebuild
        if (pos('lazbuild',lowerCase(exec))>0) and
          (pos('.lpk',lowercase(exec))>0) and
          (pos('only for runtime',lowercase(output))=0)
        then
        begin
          infoln('Marking Lazarus for rebuild based on exec line '+exec,etDebug);
          FLazarusNeedsRebuild:=true;
        end;
        {$endif}
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

{$ifndef FPCONLY}
function TUniversalInstaller.UnInstallPackage(PackagePath: string): boolean;
var
  cnt, i: integer;
  key: string;
  LazarusConfig: TUpdateLazConfig;
  PackageName,PackageAbsolutePath: string;
  xmlfile: string;
  lpkdoc:TConfig;
  lpkversion:TAPkgVersion;
begin
  result:=false;
  PackageName:=ExtractFileNameWithoutExt(ExtractFileNameOnly(PackagePath));
  // Convert any relative path to absolute path, if it's not just a file/package name:
  if ExtractFileName(PackagePath)=PackagePath then
    PackageAbsolutePath:=PackagePath
  else
    PackageAbsolutePath:=SafeExpandFileName(PackagePath);
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

  if (ExtractFileName(PackagePath)<>PackagePath) then
  begin
    if FVerbose then WritelnLog('TUniversalInstaller: removing lpl file for '+ExtractFileName(PackagePath),true);
    lpkdoc:=TConfig.Create(PackageAbsolutePath);
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
      PackageAbsolutePath := IncludeTrailingPathDelimiter(LazarusDir)+
                             'packager'+DirectorySeparator+
                             'globallinks'+DirectorySeparator+
                             LowerCase(lpkversion.Name)+'-'+lpkversion.AsString+'.lpl';

      if FileExists(PackageAbsolutePath) then
      begin
        SysUtils.DeleteFile(PackageAbsolutePath);
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
  begin
  filename:=xmlfile+'.xml';
  oldcounter:='';
  for i:=0 to MAXINSTRUCTIONS do
    begin
    // Read command, e.g. AddToHelpOptions1
    // and deduce which XML settings file to update
    if i=0
       then exec:=GetValue('AddTo'+xmlfile,sl)
       else exec:=GetValue('AddTo'+xmlfile+IntToStr(i),sl);
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
  result:=InitModule;
  if not result then exit;
  {$ifndef FPCONLY}
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
      // Process AddPackage
      // Compile a package and add it to the list of user-installed packages.
      // Usage:
      // AddPackage<n>=<path to package>\<package.lpk>
      // As this will modify config values, we keep it out the section below.
      AddPackages(TStringList(UniModuleList.Objects[idx]));
      sl:=TStringList(UniModuleList.Objects[idx]);

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
  {$endif}
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
  TempArchive:string;
  ResultCode: longint;
  User,Pass:string;
  GotLocation:boolean;
  SourceOK:boolean;

  function CheckLocation:boolean;
  begin
    result:=GotLocation;
    if GotLocation
      then infoln('Warning !! Duplicate location !! Ignoring module location: '+RemoteURL,etInfo)
      else GotLocation:=True;
  end;

begin
  result:=InitModule;
  if not result then exit;
  GotLocation:=False;
  result:=true;
  SourceOK:=false;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    WritelnLog('Getting module '+ModuleName,True);
    InstallDir:=GetValue('InstallDir',sl);
    if InstallDir<>'' then
      ForceDirectoriesUTF8(InstallDir);
    // Common keywords for all repo methods
    PinRevision:=GetValue('REVISION',sl);

    // Handle SVN urls
    RemoteURL:=GetValue('SVNURL',sl);
    if RemoteURL<>'' then
    begin
      if CheckLocation then exit;
      infoln('Going to download/update from SVN repository '+RemoteURL,etDebug);
      UpdateWarnings:=TStringList.Create;
      try
        FBaseDirectory:=InstallDir;
        FUrl:=RemoteURL;
        User:=GetValue('UserName',sl);
        Pass:=GetValue('Password',sl);
        if PinRevision<>'' then
          FSVNClient.DesiredRevision:=PinRevision;
        FSVNClient.Verbose:=FVerbose;
        FSVNClient.ExportOnly:=FExportOnly;
        result:=DownloadFromSVN(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings,User,Pass);
        SourceOK:=result;
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
    if (RemoteURL<>'') AND (NOT SourceOK) then
    begin
      if CheckLocation then exit;
      UpdateWarnings:=TStringList.Create;
      try
        FBaseDirectory:=InstallDir;
        FUrl:=RemoteURL;
        if PinRevision<>'' then
          FHGClient.DesiredRevision:=PinRevision;
        FHGClient.Verbose:=FVerbose;
        FHGClient.ExportOnly:=FExportOnly;
        result:=DownloadFromHG(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
        SourceOK:=result;
        if result=false then
          WritelnLog('HG error downloading from '+RemoteURL+'. Continuing regardless.',true);
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
    if (RemoteURL<>'') AND (NOT SourceOK) then
    begin
      if CheckLocation then exit;
      UpdateWarnings:=TStringList.Create;
      try
        FBaseDirectory:=InstallDir;
        FUrl:=RemoteURL;
        if PinRevision<>'' then
          FGitClient.DesiredRevision:=PinRevision;
        FGitClient.Verbose:=FVerbose;
        FGitClient.ExportOnly:=FExportOnly;
        result:=DownloadFromGit(ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
        SourceOK:=result;
        if result=false then
          WritelnLog('GIT error downloading from '+RemoteURL+'. Continuing regardless.',true);
        if UpdateWarnings.Count>0 then
        begin
          WritelnLog(UpdateWarnings.Text);
        end;
      finally
        UpdateWarnings.Free;
      end;
    end;

    RemoteURL:=GetValue('ArchiveURL',sl);
    if (RemoteURL<>'') AND (NOT SourceOK) then
    begin
      if CheckLocation then exit;
      TempArchive := SysUtils.GetTempFileName+SysUtils.ExtractFileExt(GetFileNameFromURL(RemoteURL));
      WritelnLog('Going to download '+RemoteURL+' into '+TempArchive,True);
      try
        result:=Download(RemoteURL,TempArchive);
      except
        on E: Exception do
        begin
         result:=false;
        end;
      end;

      if result=false then
         WritelnLog('Error downloading from '+RemoteURL+'. Continuing regardless.',True);

      if result then
      begin
        WritelnLog('Download ok',True);
        // Extract, overwrite, flatten path/junk paths
        case UpperCase(sysutils.ExtractFileExt(TempArchive)) of
           '.ZIP':
              ResultCode:=ExecuteCommand(FUnzip+' -o -d '+IncludeTrailingPathDelimiter(InstallDir)+' '+TempArchive,FVerbose);
           '.7Z':
              begin
                ResultCode:=ExecuteCommand(F7zip+' x -o"'+IncludeTrailingPathDelimiter(InstallDir)+'" '+TempArchive,FVerbose);
                {$ifdef MSWINDOWS}
                // try winrar
                if ResultCode <> 0 then
                begin
                  ResultCode:=ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+TempArchive+' "'+IncludeTrailingPathDelimiter(InstallDir)+'"',FVerbose);
                end;
                {$endif}
              end;
           else {.tar and all others}
              ResultCode:=ExecuteCommand(FTar+' -xf '+TempArchive +' -C '+ExcludeTrailingPathDelimiter(InstallDir),FVerbose);
           end;
        if ResultCode <> 0 then
        begin
          result := False;
          infoln(ModuleName+': unpack of '+TempArchive+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
        end;
      end;
      SysUtils.Deletefile(TempArchive); //Get rid of temp file.
      SourceOK:=result;
    end;

    RemoteURL:=GetValue('ArchivePATH',sl);
    if (RemoteURL<>'') AND (NOT SourceOK) then
    begin
      if CheckLocation then exit;
      TempArchive := RemoteURL;
      case UpperCase(sysutils.ExtractFileExt(TempArchive)) of
         '.ZIP':
            ResultCode:=ExecuteCommand(FUnzip+' -o -d '+IncludeTrailingPathDelimiter(InstallDir)+' '+TempArchive,FVerbose);
         '.7Z':
         begin
           ResultCode:=ExecuteCommand('7z'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(InstallDir)+'" '+TempArchive,FVerbose);
           if ResultCode <> 0 then
           begin
             ResultCode:=ExecuteCommand('7za'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(InstallDir)+'" '+TempArchive,FVerbose);
           end;
         end;
         else {.tar and all others}
            ResultCode:=ExecuteCommand(FTar+' -xf '+TempArchive +' -C '+ExcludeTrailingPathDelimiter(InstallDir),FVerbose);
         end;
      if ResultCode <> 0 then
      begin
        result := False;
        infoln(ModuleName+': unpack of '+TempArchive+' failed with resultcode: '+IntToStr(ResultCode),etwarning);
      end;

      // todo patch package if correct patch is available in patch directory

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
{$ifndef FPCONLY}
var
  idx,cnt,i:integer;
  sl:TStringList;
  Directive,xmlfile,key:string;
  LazarusConfig:TUpdateLazConfig;
{$endif}
begin
  result:=InitModule;
  if not result then exit;
  {$ifndef FPCONLY}
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
  {$endif}
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

function GetAlias(Dictionary,KeyWord: string): string;
var
  ini:TMemIniFile;
  sl:TStringList;
  e:Exception;
begin
sl:=TStringList.Create;
ini:=TMemIniFile.Create(CurrentConfigFile);
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
        // added because older fpc.ini or equivalent may not have the default keyword !!
        if Uppercase(KeyWord)='DEFAULT' then
        begin
          infoln('InstallerUniversal: no default source alias found: using fpcup default',etInfo);
          if Dictionary='fpcURL' then result:='http://svn.freepascal.org/svn/fpc/tags/release_3_0_0';
          {$ifndef FPCONLY}
          if Dictionary='lazURL' then result:='http://svn.freepascal.org/svn/lazarus/tags/lazarus_1_6';
          {$endif}
        end;
        if result='' then
        begin
          e:=Exception.CreateFmt('--%s=%s : Invalid keyword. Accepted keywords are: %s',[Dictionary,KeyWord,sl.CommaText]);
          raise e;
        end;
      end;
    end;
finally
  ini.Free;
  sl.free;
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
    name:=ini.ReadString(ModuleName,'Name','');
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
  ini:=TMemIniFile.Create(CurrentConfigFile);
  ini.Options:=[ifoStripQuotes];
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

function CheckIncludeModule(ModuleName: string):boolean;
var
  ini:TMemIniFile;
  j,k:integer;
  os,cpu,s:string;
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
        if pos('=',s)>0 then
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

  function OccurrencesOfChar(const ContentString: string;
    const CharToCount: char): integer;
  var
    C: Char;
  begin
    result := 0;
    for C in ContentString do
      if C = CharToCount then
        Inc(result);
  end;

begin
  result:=False;

  ini:=TMemIniFile.Create(SafeGetApplicationPath+CONFIGFILENAME);
  try
    ini.CaseSensitive:=false;
    ini.StripQuotes:=true; //helps read description lines

    AddModule:=True;

    j:=UniModuleList.IndexOf(ModuleName);

    if j=-1 then AddModule:=false else
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
             if (Pos('win32',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-win32',os)=0),NegativeList);
             {$endif}
             {$ifdef win64}
             if (Pos('win64',os)>0) then AddModule:=AND_OR_Values(AddModule,(Pos('-win64',os)=0),NegativeList);
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

procedure SetConfigFile(aConfigFile: string);
var
  ConfigFile: Text;
  CurrentConfigFileName:string;
begin
  CurrentConfigFile:=aConfigFile;
  // Create fpcup.ini from resource if it doesn't exist yet
  if (CurrentConfigFile=SafeGetApplicationPath+CONFIGFILENAME)
    and not FileExistsUTF8(SafeGetApplicationPath+CONFIGFILENAME) then
    SaveInisFromResource(SafeGetApplicationPath+CONFIGFILENAME,'fpcup_ini');
  {
  // Create fpcup.ini from resource
  // Save old version if existing
  if (CurrentConfigFile=SafeGetApplicationPath+CONFIGFILENAME) then
  begin
    if FileExists(CurrentConfigFile) then
    begin
      CurrentConfigFileName:=CurrentConfigFile;
      while FileExists(CurrentConfigFileName) do
        CurrentConfigFileName := CurrentConfigFileName + 'i';
      FileUtil.CopyFile(CurrentConfigFile,CurrentConfigFileName);
      infoln('Old configfile('+CurrentConfigFile+') saved as '+CurrentConfigFileName+ '!!',etInfo);
    end;
    SysUtils.Deletefile(CurrentConfigFile);
    SaveInisFromResource(CurrentConfigFile,'fpcup_ini');
  end;
  }
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

