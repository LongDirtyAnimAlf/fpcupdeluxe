{ Installer unit for FPCUp
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
unit installer;

{
Gets/updates/compiles/installs FPC/Lazarus sources
Uses updater unit to get/update the sources.

General remarks:
- For TProcess.Params, don't use (double) quotes even though this would be required in the shell
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, processutils, m_crossinstaller,
  installerFpc,installerLazarus,installerUniversal;




type
  { TOldInstaller }
  TOldInstaller = class(TObject)
  private
    FAllOptions: string; //Options/command line arguments chosen by user
    FBinUtils: TStringlist; //binutils such as make.exe, as.exe, needed for compilation
    FBunzip2: string; //Location or name of bunzip2 executable
    FBootstrapCompilerDirectory: string; //Directory where bootstrap compiler is
    FBootstrapCompilerFTP: string;
    FBootstrapCompilerName: string; //OS specific compiler name (e.g. ppcuniversal for OSX)
    FClean: boolean; //User selected clean or build
    FCrossCPU_Target: string;
    FCrossOS_Target: string;
    FCrossLCL_Platform:string;
    FFPCDesiredRevision: string;
    FFPCDirectory: string;
    FFPCOPT: string;
    FFPCURL: string;
    FLazarusDirectory: string;
    FLazarusDesiredRevision: string;
    FLazarusOPT: string;
    FLazarusURL: string;
    FOnlyModules: string;
    FShortcutName: string; //Name for shortcut/shell script pointing to newly installed Lazarus
    FExecutableExtension: string; //.exe on Windows
    FFPCPlatform: string; //Identification for platform in compiler path (e.g. i386-win32)
    FInstalledCompiler: string; //Complete path to installed FPC compiler; used to compile Lazarus
    FInstalledCompilerName: string; //Name only of installed PPC compiler (e.g. ppcx64 on 64 bit Intel OSX)
    FLazarusPrimaryConfigPath: string; //Primary config path used in our custom Lazarus install
    FLogFile:Text;
    FLogVerboseFile:Text;
    FMake: string; //Location or name of make executable
    FShortCutNameFpcup: string;
    {$IFDEF MSWINDOWS}
    FMakeDir: string; //Directory where binutils (as.exe,ar.exe,make.exe,...) reside
    {$ENDIF}
    FSkipModules: string;
    FSVNDirectory: string; //Unpack SVN files in this directory. Actual SVN exe may be below this directory.
    //todo: check if we shouldn't rather use FSVNExecutable, extract dir from that.
    FTar: string; //Location or name of tar executable
    FUnzip: string; //Location or name of unzip executable
    FVerbose: boolean;
    function DownloadFPCHelp(URL, TargetDirectory: string): boolean;
    procedure EnvironmentWithOurPath(var EnvironmentList: TStringList; const NewPath: string);
    // Return complete environment except replace path with our own value
    function GetBootstrapCompiler: string;
    function GetCompilerName: string;
    procedure LogError(Sender:TProcessEx;IsException:boolean);
    function ModuleEnabled(Name:string):boolean;
    procedure SetAllOptions(AValue: string);
    procedure SetCrossCPU_Target(AValue: string);
    procedure SetCrossLCL_Platform(AValue: string);
    procedure SetCrossOS_Target(AValue: string);
    procedure SetFPCDesiredRevision(AValue: string);
    procedure SetLazarusDesiredRevision(AValue: string);
    procedure SetLazarusPrimaryConfigPath(AValue: string);
    procedure SetOnlyModules(AValue: string);
    procedure SetShortCutNameFpcup(AValue: string);
    procedure SetSkipFPC(AValue: boolean);
    procedure SetSkipLazarus(AValue: boolean);
    procedure SetSkipLazarusHelp(AValue: boolean);
    procedure SetSkipModules(AValue: string);
    procedure SetVerbose(AValue: boolean);
    function GetMakePath: string;
    procedure SetBootstrapCompilerDirectory(AValue: string);
    procedure SetFPCDirectory(Directory: string);
    procedure SetFPCOPT(AValue: string);
    procedure SetFPCUrl(AValue: string);
    procedure SetLazarusDirectory(Directory: string);
    procedure SetLazarusOPT(AValue: string);
    procedure SetLazarusUrl(AValue: string);
    procedure SetMakePath(AValue: string);
  public
    property ShortCutName: string read FShortcutName write FShortcutName; //Name of the shortcut to Lazarus. If empty, no shortcut is generated.
    property ShortCutNameFpcup:string read FShortCutNameFpcup write SetShortCutNameFpcup;
    // Name for shortcut/shellscript linking to fpcup, useful to update without retyping options
    property CompilerName: string read GetCompilerName;
    //Name only of installed compiler
    property AllOptions:string read FAllOptions write SetAllOptions;
    property BootstrapCompiler: string read GetBootstrapCompiler;
    //Full path to FPC compiler used to compile the downloaded FPC compiler sources
    property BootstrapCompilerDirectory: string
      read FBootstrapCompilerDirectory write SetBootstrapCompilerDirectory;
    //Directory that has compiler needed to compile compiler sources. If compiler doesn't exist, it will be downloaded
    property BootstrapCompilerFTP: string read FBootstrapCompilerFTP
      write FBootstrapCompilerFTP;
    //Optional; URL from which to download bootstrap FPC compiler if it doesn't exist yet.
    property Clean: boolean read FClean write FClean;
    // Switch between cleanup (svn revert etc) and build modes
    function CleanFPC: boolean;
    // Clean up FPC environment
    function CleanLazarus: boolean;
    // Clean up Lazarus environment
    function CleanLazarusHelp: boolean;
    // Clean up help environment
    property CrossCPU_Target:string read FCrossCPU_Target write SetCrossCPU_Target;
    property CrossLCL_Platform:string read FCrossLCL_Platform write SetCrossLCL_Platform;
    property CrossOS_Target:string read FCrossOS_Target write SetCrossOS_Target;
    property FPCDirectory: string read FFPCDirectory write FFPCDirectory;
    property FPCURL: string read FFPCURL write SetFPCURL; //SVN URL for FPC
    property FPCOPT: string read FFPCOPT write SetFPCOPT;
    property FPCDesiredRevision:string read FFPCDesiredRevision write SetFPCDesiredRevision;
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    function GetLazarusHelp: boolean; //Create/get/compile Lazarus help
    property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
    property LazarusPrimaryConfigPath: string
      read FLazarusPrimaryConfigPath write SetLazarusPrimaryConfigPath;
    //The directory where the configuration for this Lazarus instance must be stored.
    property LazarusURL: string read FLazarusUrl write FLazarusUrl;
    //SVN URL for Lazarus
    property LazarusOPT:string read FLazarusOPT write SetLazarusOPT;
    property LazarusDesiredRevision:string read FLazarusDesiredRevision write SetLazarusDesiredRevision;
    procedure WriteLog(msg:string;ToConsole:boolean=true);
    procedure WritelnLog(msg:string;ToConsole:boolean=true);
    property MakeDirectory: string read GetMakePath write SetMakePath;
    //Directory of make executable and other binutils. If it doesn't exist, make and binutils will be downloaded
    function Run: boolean;
    // Main entry point to the class: perform all selected actions. Returns success or failure result.
    property SkipModules:string read FSkipModules write SetSkipModules;
    property OnlyModules:string read FOnlyModules write SetOnlyModules;
    property Verbose:boolean read FVerbose write SetVerbose;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  FileUtil {Requires LCL}
{$IFDEF UNIX}
  ,baseunix
{$ENDIF UNIX}
  ,updatelazconfig, fpcuputil;


























function TOldInstaller.DownloadFPCHelp(URL, TargetDirectory: string): boolean;
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  DocsZip: string;
begin
  // Download FPC CHM docs zip into TargetDirectory.
  OperationSucceeded:=true;
  ForceDirectories(TargetDirectory);
  DocsZip := SysUtils.GetTempFileName + '.zip';
  try
    OperationSucceeded:=Download(URL,DocsZip);
  except
    on E: Exception do
    begin
      // Deal with timeouts, wrong URLs etc
      OperationSucceeded:=false;
      infoln('DownloadFPCHelp: HTTP download failed. URL: '+URL+LineEnding+
        'Exception: '+E.ClassName+'/'+E.Message);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite, flatten path/junk paths
    // todo: test with spaces in path
    if ExecuteCommandHidden(FUnzip,'-o -j -d '+IncludeTrailingPathDelimiter(TargetDirectory)+' '+DocsZip,Verbose)<> 0 then
    begin
      OperationSucceeded := False;
      infoln('DownloadFPCHelp: unzip failed with resultcode: '+IntToStr(ResultCode));
    end;
  end
  else
  begin
    infoln('DownloadFPCHelp: HTTP download failed. URL: '+URL);
  end;

  if OperationSucceeded then
      SysUtils.deletefile(DocsZip); //Get rid of temp zip if success.
  Result := OperationSucceeded;
end;

procedure TOldInstaller.EnvironmentWithOurPath(
  var EnvironmentList: TStringList; const NewPath: String);
const
  {$IFDEF MSWINDOWS}
  LookFor='Path=';
  {$ELSE MSWINDOWS}
  LookFor='PATH=';
  {$ENDIF MSWINDOWS}
var
  Counter: integer;
  CustomPath: string;
  SingleLine: string;
begin
  // GetEnvironmentVariableCount is 1 based
  for Counter:=1 to GetEnvironmentVariableCount do
  begin
    // Clean up path we're given
    CustomPath:=NewPath;
    // If missing, add PATH= or path=:
    if ansipos(AnsiUpperCase(LookFor), AnsiUpperCase(CustomPath))=0 then
    begin
      CustomPath:=(LookFor+CustomPath);
    end;
    // Get rid of empty parts; don't know if required but clearer for log output
    CustomPath:=StringReplace(CustomPath, PathSeparator+PathSeparator, PathSeparator, [rfReplaceAll,rfIgnoreCase]);

    SingleLine:=GetEnvironmentString(Counter);
    if AnsiPos(LookFor, SingleLine)>0 then
    begin
      //We found the PATH variable; replace it
      EnvironmentList.Add(CustomPath);
    end
    else
    begin
      EnvironmentList.Add(SingleLine);
    end;
  end;
end;

function TOldInstaller.GetBootstrapCompiler: string;
begin
  Result := BootstrapCompilerDirectory + FBootstrapCompilerName;
end;

function TOldInstaller.GetCompilerName: string;
begin
  // Return installed CompilerName or bootstrap CompilerName as fallback
  // Note: we can't use BootstrapCompiler property otherwise endless loop
  if FInstalledCompilerName<>EmptyStr then
    result:=FInstalledCompilerName
  else
    result:=FBootstrapCompilerName;
end;


function TOldInstaller.GetLazarusHelp(): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  BuildLCLDocsDirectory: string;
  CustomPath: string;
  LazarusConfig: TUpdateLazConfig;
  OperationSucceeded: boolean;
  ProcessEx:TProcessEx;
begin
{  infoln('Module HELP: getting/compiling Lazarus help...');

  //Make sure we have the proper tools.
  OperationSucceeded := CheckAndGetNeededExecutables;

  // If we haven't installed FPC, this won't be set:
  if FInstalledCompiler = '' then
  begin
    //Assume we've got a working compiler. This will link through to the
    //platform-specific compiler, e.g. our fpc.sh proxy on Unix
    SetCompilerToInstalledCompiler;
  end;

  ProcessEx:=TProcessEx.Create(nil);
  if Verbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  ProcessEx.OnErrorM:=@LogError;

  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  CustomPath:=BootstrapCompilerDirectory+PathSeparator+
    MakeDirectory+PathSeparator+
    FSVNDirectory+PathSeparator+
    FPCDirectory+PathSeparator+
    LazarusDirectory;
  ProcessEx.Environment.SetVar('Path',CustomPath);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ProcessEx.Environment.SetVar('PATH',ExtractFilePath(FInstalledCompiler)+':'+ProcessEx.Environment.GetVar('PATH'));
  {$ENDIF UNIX}
  if CustomPath<>EmptyStr then
    writelnLog('External program path:  '+CustomPath,false);

  // Location of build_lcl_docs.lpr, and also of the help files to be installed.
  BuildLCLDocsDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
      'docs'+DirectorySeparator+
      'html'+DirectorySeparator;

  if OperationSucceeded then
  begin
    // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
    ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
    ProcessEx.Parameters.Add(BuildLCLDocsDirectory+'build_lcl_docs.lpr');
    infoln('Lazarus: compiling build_lcl_docs help compiler:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    // Download FPC CHM (rtl.chm and fcl.chm) if necessary
    {Possible alternatives
    1. make chm -> requires latex!!!
    2. or
    c:\development\fpc\utils\fpdoc\fpdoc.exe --content=rtl.xct --package=rtl --descr=rtl.xml --output=rtl.chm --auto-toc --auto-index --make-searchable --css-file=C:\Development\fpc\utils\fpdoc\fpdoc.css  --format=chm
    ... but we'd need to include the input files extracted from the Make file.
    }
    infoln('Module HELP: downloading FPC RTL/CHM help...');
    if FileExistsUTF8(BuildLCLDocsDirectory+'fcl.chm') and
    FileExistsUTF8(BuildLCLDocsDirectory+'rtl.chm') then
    begin
      infoln('Skipping download: FPC rtl.chm and fcl.chm already present in docs directory '+BuildLCLDocsDirectory);
    end
    else
    begin
    // Link to 2.6 documentation: rtl, chm, and reference manuals, including .xct files
    // http://sourceforge.net/projects/freepascal/files/Documentation/2.6.0/doc-chm.zip/download
    // which links to
    // http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip
    //
    // Note: there's also an older file on
    // http://sourceforge.net/projects/freepascal/files/Documentation/
    // that includes the lcl file
    // Download and extract zip contents into build_lcl_docs directory
    // todo: replace with main sourceforge download instead of mirror, but mirror code needs to be fixed
    OperationSucceeded:=DownloadFPCHelp('http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip',
      BuildLCLDocsDirectory);
    end;
  end;

  if OperationSucceeded then
  begin
    // Compile Lazarus CHM help

    // First remove any existing cross reference to LCL.chm; I think that might confuse fpcdoc
    DeleteFileUTF8(BuildLCLDocsDirectory+'lcl.xct');

    ProcessEx.Executable := BuildLCLDocsDirectory+'build_lcl_docs'+FExecutableExtension;
    // Make sure directory switched to that of build_lcl_docs,
    // otherwise paths to source files will not work.
    ProcessEx.CurrentDirectory:=BuildLCLDocsDirectory;
    ProcessEx.Parameters.Clear;
    // Instruct build_lcl_docs to cross-reference FPC documentation by specifying
    // the directory that contains the fcl and rtl .xct files:
    ProcessEx.Parameters.Add('--fpcdocs');
    ProcessEx.Parameters.Add(BuildLCLDocsDirectory);
    ProcessEx.Parameters.Add('--fpdoc');
    // Use the fpdoc in ./utils/fpdoc/, as the compiler directory is now different between
    // Unix+Windows
    ProcessEx.Parameters.Add(FPCDirectory+
    'utils'+DirectorySeparator+
    'fpdoc'+DirectorySeparator+
    'fpdoc'+FExecutableExtension); //fpdoc gets called by build_lcl_docs
    ProcessEx.Parameters.Add('--outfmt');
    ProcessEx.Parameters.Add('chm');
    infoln('Lazarus: compiling chm help docs:');
    { The CHM file gets output into <lazarusdir>/docs/html/lcl/lcl.chm
    Though that may work when adjusting the baseurl option in Lazarus for each
    CHM file, it's easier to move them to <lazarusdir>/docs/html,
    which is also suggested by the wiki.
    The generated .xct file is an index file for fpdoc cross file links,
    used if you want to link to the chm from other chms.}
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    if FileExistsUTF8(BuildLCLDocsDirectory+
      'lcl'+DirectorySeparator+
      'lcl.chm') then
    begin
      infoln('Lazarus: moving lcl.chm to docs directory');
      // Move help file to doc directory
      OperationSucceeded:=MoveFile(BuildLCLDocsDirectory+
        'lcl'+DirectorySeparator+
        'lcl.chm',
        BuildLCLDocsDirectory+
        'lcl.chm');
    end;
  end;

  // Finish up
  ProcessEx.Free;
  result:=OperationSucceeded;
}end;

procedure TOldInstaller.WriteLog(msg: string; ToConsole: boolean);
begin
  Write(FLogFile,msg);
  if ToConsole then
    InfoLn(msg);
end;

procedure TOldInstaller.WritelnLog(msg: string; ToConsole: boolean);
begin
  WriteLog(msg+LineEnding,false); //infoln adds alread a lf
  if ToConsole then
    InfoLn(msg);
end;

function TOldInstaller.Run: boolean;
var
  OperationSucceeded:boolean;
begin
  WritelnLog('Running fpcup with parameters: '+FAllOptions+' --only='+FOnlyModules+' --skip='+FSkipModules,false);
  OperationSucceeded:=true;
  if Clean then
  begin
    // Clean; can be either "nuclear" clean if no other options given
    // or limited clean if only some modules selected or some modules are
    // skipped.
    // Note: test is simplistic: it only checks if any --only= or --skip==
    // modules are given, not e.g. whether the resulting module set
    // matches the standard set.
    if (FOnlyModules='') and (FSkipModules='') then
    begin
      // Nuclear cleaning: delete entire FPC+Lazarus directories, but
      // don't remove primary config path or shortcuts
      WritelnLog('User selected --clean without options. Total cleanup started.');
      if FPCDirectory='' then
      begin
        WritelnLog('Error: FPC directory not known. Not cleaning FPC directory.');
      end
      else
      begin
        if DeleteDirectoryEx(FPCDirectory)=false then
        begin
          WritelnLog('Error deleting FPC directory '+FPCDirectory);
        end;
      end;
      if LazarusDirectory='' then
      begin
        WritelnLog('Error: Lazarus directory not known. Not cleaning Lazarus directory.');
      end
      else
      begin
        if DeleteDirectoryEx(LazarusDirectory)=false then
        begin
          WritelnLog('Error deleting Lazarus directory '+LazarusDirectory);
        end;
      end;
      // Nuclear cleaning finished
    end
    else
    begin
      // Clean only selected modules
      if ModuleEnabled('FPC') then
      begin
        if OperationSucceeded then OperationSucceeded:=CleanFPC;
      end
      else
      begin
        WritelnLog('FPC cleanup skipped by user.');
      end;

      if ModuleEnabled('LAZARUS') or ModuleEnabled('HELP')
        or ModuleEnabled('DOCEDITOR') or ModuleEnabled('BIGIDE') then
      begin
        if OperationSucceeded then OperationSucceeded:=CleanLazarus;
      end
      else
      begin
        WritelnLog('Module LAZARUS: cleanup skipped by user.');
      end;

      if ModuleEnabled('HELP') then
      begin
         if OperationSucceeded then OperationSucceeded:=CleanLazarusHelp;
      end
      else
      begin
        WritelnLog('Lazarus cleanup skipped by user.');
      end;
    end;
  end
  else
  begin
    // Build.
    // Link to fpcup itself, with all options as passed when invoking it:
    if ShortCutNameFpcup<>EmptyStr then
    begin
     {$IFDEF MSWINDOWS}
      CreateDesktopShortCut(paramstr(0),AllOptions,ShortCutNameFpcup);
     {$ELSE}
      FAllOptions:=FAllOptions+' $*';
      CreateHomeStartLink('"'+paramstr(0)+'"',FAllOptions,ShortCutNameFpcup);
     {$ENDIF MSWINDOWS}
    end;

    if ModuleEnabled('FPC') then
    begin
      if OperationSucceeded then OperationSucceeded:=GetFPC;
    end
    else
    begin
      WritelnLog('FPC installation/update skipped by user.');
    end;

    if ModuleEnabled('LAZARUS') or ModuleEnabled('HELP')
      or ModuleEnabled('DOCEDITOR') or ModuleEnabled('BIGIDE') then
    begin
      if OperationSucceeded then OperationSucceeded:=GetLazarus;
    end
    else
    begin
      WritelnLog('Module LAZARUS: installation/update skipped by user.');
    end;

    if ModuleEnabled('HELP') then
    begin
       if OperationSucceeded then OperationSucceeded:=GetLazarusHelp;
    end
    else
    begin
      WritelnLog('Lazarus help skipped by user.');
    end;
  end;
  result:=OperationSucceeded;
end;


procedure TOldInstaller.LogError(Sender: TProcessEx; IsException: boolean);
var
  TempFileName:string;
begin
  TempFileName:=SysUtils.GetTempFileName;
  if IsException then
  begin
    WritelnLog('Exception raised running ' + Sender.ResultingCommand, true);
    WritelnLog(Sender.ExceptionInfo, true);
  end
  else
  begin
    infoln('Command: '+LineEnding+
      Sender.ResultingCommand+LineEnding+
      'returned non-zero ExitStatus: '+IntToStr(Sender.ExitStatus)+'. Output:'+LineEnding+
      Sender.OutputString);
    WritelnLog('ERROR running '+Sender.ResultingCommand,false);
    Sender.OutputStrings.SaveToFile(TempFileName);
    WritelnLog('Output logged in '+TempFileName,false);
  end;
end;

function TOldInstaller.ModuleEnabled(Name: string): boolean;
begin
  result:=(((FOnlyModules='') and (FSkipModules=''))
          or ((FOnlyModules<>'') and (Pos(Name,FOnlyModules)>0)))
          or ((FSkipModules<>'') and (Pos(Name,FSkipModules)<=0))
end;

procedure TOldInstaller.SetAllOptions(AValue: string);
begin
  if FAllOptions=AValue then Exit;
  FAllOptions:=AValue;
end;

procedure TOldInstaller.SetCrossCPU_Target(AValue: string);
begin
  if FCrossCPU_Target=AValue then Exit;
  FCrossCPU_Target:=AValue;
end;

procedure TOldInstaller.SetCrossLCL_Platform(AValue: string);
begin
  if FCrossLCL_Platform=AValue then Exit;
  FCrossLCL_Platform:=AValue;
end;

procedure TOldInstaller.SetCrossOS_Target(AValue: string);
begin
  if FCrossOS_Target=AValue then Exit;
  FCrossOS_Target:=AValue;
end;

procedure TOldInstaller.SetFPCDesiredRevision(AValue: string);
begin
  if FFPCDesiredRevision=AValue then Exit;
  FFPCDesiredRevision:=AValue;
end;

procedure TOldInstaller.SetLazarusDesiredRevision(AValue: string);
begin
  if FLazarusDesiredRevision=AValue then Exit;
  FLazarusDesiredRevision:=AValue;
end;



function TOldInstaller.GetMakePath: string;
begin
  {$IFDEF MSWINDOWS}
  Result := FMakeDir;
  {$ELSE}
  Result := ''; //dummy value, done for compatibility
  {$ENDIF MSWINDOWS}
end;



procedure TOldInstaller.SetBootstrapCompilerDirectory(AValue: string);
begin
  FBootstrapCompilerDirectory:=IncludeTrailingPathDelimiter(ExpandFileName(AValue));
end;

procedure TOldInstaller.SetFPCDirectory(Directory: string);
begin
  FFPCDirectory:=Directory;
end;


procedure TOldInstaller.SetFPCOPT(AValue: string);
begin
  if FFPCOPT=AValue then Exit;
  FFPCOPT:=AValue;
end;

procedure TOldInstaller.SetFPCUrl(AValue: string);
begin
  FFPCURL:=AValue;
end;

procedure TOldInstaller.SetLazarusDirectory(Directory: string);
begin
  FLazarusDirectory:=Directory;
end;


procedure TOldInstaller.SetLazarusOPT(AValue: string);
begin
  if FLazarusOPT=AValue then Exit;
  FLazarusOPT:=AValue;
end;

procedure TOldInstaller.SetLazarusUrl(AValue: string);
begin
  FLazarusURL:=AValue;
end;

procedure TOldInstaller.SetLazarusPrimaryConfigPath(AValue: string);
const
  DefaultPCPSubdir='lazarusdevsettings'; //Include the name lazarus for easy searching Caution: shouldn't be the same name as Lazarus dir itself.
begin
  //Directory where Lazarus installation config will end up (primary config path)
  if AValue=EmptyStr then
  begin
    {$IFDEF MSWINDOWS}
    // Somewhere in local appdata special folder
    FLazarusPrimaryConfigPath := IncludeTrailingPathDelimiter(GetLocalAppDataPath())+DefaultPCPSubdir;
    {$ELSE}
    //Note: normsl GetAppConfigDir gets ~/.config/fpcup/.lazarusdev or something
    LazarusPrimaryConfigPath:=IncludeTrailingPathDelimiter(XdgConfigHome)+DefaultPCPSubdir;
    {$ENDIF MSWINDOWS}
  end
  else
  begin
    FLazarusPrimaryConfigPath:=AValue;
  end;
end;


procedure TOldInstaller.SetOnlyModules(AValue: string);
begin
  if FOnlyModules=AValue then Exit;
  FOnlyModules:=Uppercase(AValue);
end;

procedure TOldInstaller.SetShortCutNameFpcup(AValue: string);
begin
  if FShortCutNameFpcup=AValue then Exit;
  FShortCutNameFpcup:=AValue;
end;

procedure TOldInstaller.SetSkipFPC(AValue: boolean);
begin

end;

procedure TOldInstaller.SetSkipLazarus(AValue: boolean);
begin

end;

procedure TOldInstaller.SetSkipLazarusHelp(AValue: boolean);
begin

end;

procedure TOldInstaller.SetSkipModules(AValue: string);
begin
  if FSkipModules=AValue then Exit;
  FSkipModules:=UpperCase(AValue);
end;

procedure TOldInstaller.SetVerbose(AValue: boolean);
begin
  if FVerbose=AValue then Exit;
  FVerbose:=AValue;
end;


procedure TOldInstaller.SetMakePath(AValue: string);
begin
  {$IFDEF MSWINDOWS}
  // Make sure there's a trailing delimiter
  FMakeDir:=IncludeTrailingPathDelimiter(AValue);
  FMake:=IncludeTrailingPathDelimiter(FMakeDir)+'make'+FExecutableExtension;
  {$ELSE}
  FMake:='make'; //assume in path
  {$ENDIF MSWINDOWS}
end;

function TOldInstaller.CleanFPC: boolean;
begin
//moved to TFPCInstaller.UnInstall
end;

function TOldInstaller.CleanLazarus: boolean;
begin
  //moved to TLazarusInstaller.UnInstall
end;

function TOldInstaller.CleanLazarusHelp: boolean;
begin
  //moved to TInstallerLazarusHelp.Clean/TInstallerFPCHelp.Clean
end;



function TOldInstaller.GetFPC: boolean;
{
In this function, we try to deal with existing system wide fpc.cfg (Unix)
and the wrong compilers/binutils being in the path (Windows, mostly).

When running MAKE and other external tools:
- The --FPC= argument passed to MAKE should allow the proper compiler to be picked up.
- We switch the current directory to the makefile's directory, allowing MAKE to find
  the makefile and source files
- On Windows, we tweak the path and place our binutils directory first, allowing
  the system to find make.exe etc.; then the FPC bootstrap directory (though
  redundant: see --FPC=)
- On Unix, we tweak the path to include our FPC bootstrap directory first.

A similar effect could be gained by using --CROSSBINDIR=<makedirectory> or
OPT=-FD<makedirectory>, and use --directory=<makefiledir> but that would be
more verbose / verbose and also works when calling
other tools than make
}

var
  FPCInstaller:TFPCInstaller;

CONST
  MODULE='FPC';
begin
// which TFPCInstaller are we going to create
  if (FCrossCPU_Target<>'') or (FCrossOS_Target<>'') then
    begin
    FPCInstaller:=TFPCCrossInstaller.Create;
    FPCInstaller.CrossOS_Target:=FCrossOS_Target;
    FPCInstaller.CrossCPU_Target:=FCrossCPU_Target;
    end
  else
    FPCInstaller:=TFPCNativeInstaller.Create;
  try
    FPCInstaller.BaseDirectory:=FFPCDirectory;
    FPCInstaller.BootstrapCompilerDirectory:=FBootstrapCompilerDirectory;
    FPCInstaller.BootstrapCompilerURL:=FBootstrapCompilerFTP;
    FPCInstaller.Compiler:='';  //bootstrap used
    FPCInstaller.CompilerOptions:=FPCOPT;
    FPCInstaller.DesiredRevision:=FPCDesiredRevision;
    FPCInstaller.LogFile:=FLogFile;
    {$IFDEF MSWINDOWS}
    FPCInstaller.MakeDirectory:=FMakeDir;
    {$ENDIF}
    FPCInstaller.URL:=FPCURL;
    FPCInstaller.Verbose:=Verbose;
    result:= FPCInstaller.CleanModule(MODULE) and
             FPCInstaller.GetModule(MODULE) and
             FPCInstaller.BuildModule(MODULE);
    FInstalledCompiler:=FPCInstaller.Compiler;
  finally
    FPCInstaller.Free;
  end;

end;

function TOldInstaller.GetLazarus: boolean;
{
This function does depend on a properly installed FPC but does not
check if there is one.
Assumptions: binutils in FPC directory or in path.

In this function, we try to deal with existing system wide fpc.cfg (Unix)
and the wrong compilers/binutils being in the path (Windows, mostly).

When running MAKE and other external tools:
- The --FPC= argument passed to MAKE should allow the proper compiler to be picked up.
- We switch the current directory to the makefile's (Lazarus) directory, allowing MAKE to find
  the makefile and source files.
  If using lazbuild to build .lprs, we switch the directory to the project directory.
- On Windows, we tweak the path and place our custom FPC compiler directory first.
  The binutils (make.exe etc) should also be found there.
  Then the Lazarus directory, for any utilities that Lazarus may place there.
  Then the make/binutils and subversion client directories.
  Finally the FPC bootstrap directory, as a backup.
- On Unix, we tweak the path to include our custom FPC compiler directory first.

A similar effect could be gained by using --CROSSBINDIR=<makedirectory> or
OPT=-FD<makedirectory>, and use --directory=<makefiledir> but that would be
more verbose / verbose and also works when calling
other tools than make
}
var
  LazarusInstaller:TLazarusInstaller;

CONST
  MODULE='Lazarus';
begin
// which TLazarusInstaller are we going to create
  if (FCrossCPU_Target<>'') or (FCrossOS_Target<>'') then
    begin
    LazarusInstaller:=TLazarusCrossInstaller.Create;
    LazarusInstaller.CrossOS_Target:=FCrossOS_Target;
    LazarusInstaller.CrossCPU_Target:=FCrossCPU_Target;
    end
  else
    LazarusInstaller:=TLazarusNativeInstaller.Create;
  try
    LazarusInstaller.BaseDirectory:=LazarusDirectory;
    if FInstalledCompiler='' then
      FInstalledCompiler:=LazarusInstaller.GetCompilerInDir(FPCDirectory);
    LazarusInstaller.Compiler:=FInstalledCompiler;
    LazarusInstaller.CompilerOptions:=LazarusOPT;
    LazarusInstaller.DesiredRevision:=LazarusDesiredRevision;
    LazarusInstaller.FPCDir:=FPCDirectory;
    LazarusInstaller.LogFile:=FLogFile;
    {$IFDEF MSWINDOWS}
    LazarusInstaller.MakeDirectory:=FMakeDir;
    {$ENDIF}
    LazarusInstaller.URL:=LazarusURL;
    LazarusInstaller.Verbose:=Verbose;
    result:= LazarusInstaller.CleanModule(MODULE) and
             LazarusInstaller.GetModule(MODULE) and
             LazarusInstaller.BuildModule(MODULE) and
             LazarusInstaller.ConfigLazarus(FLazarusPrimaryConfigPath);
  finally
    LazarusInstaller.Free;
  end;
end;


{var
  AfterRevision: string;
  BeforeRevision: string;
  CrossInstaller:TCrossInstaller;
  CustomPath: string;
  LazarusConfig: TUpdateLazConfig;
  UpdateWarnings:TStringList;
  OperationSucceeded: boolean;
  Options:string;
  ProcessEx:TProcessEx;


begin
      if OperationSucceeded then
      begin
        // Right now, we have a minimally working Lazarus directory, enough
        // to justify creating a shortcut to it.
        // For Windows, a desktop shortcut. For Unixy systems, a script in ~
        {$IFDEF MSWINDOWS}
        if ShortCutName<>EmptyStr then
        begin
          infoln('Lazarus: creating desktop shortcut:');
          try
            //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded
            //DO pass quotes here (it's not TProcess.Params)
            // To installed lazarus
            CreateDesktopShortCut(FInstalledLazarus,'--pcp="'+FLazarusPrimaryConfigPath+'"',ShortCutName);
          finally
            //Ignore problems creating shortcut
          end;
        end;
        {$ENDIF MSWINDOWS}
        {$IFDEF UNIX}
        if ShortCutName<>EmptyStr then
        begin
          infoln('Lazarus: creating shortcut in your home directory');
          try
            //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded
            //DO pass quotes here (it's not TProcess.Params)
            CreateHomeStartLink(FInstalledLazarus,'--pcp="'+FLazarusPrimaryConfigPath+'"',ShortcutName);
          finally
            //Ignore problems creating shortcut
          end;
        end;
        {$ENDIF UNIX}
      end;
      if OperationSucceeded then
        if (ModuleEnabled('BIGIDE')=false) and (ModuleEnabled('HELP')=false) then
        begin
          //todo: find out if lhelp support can be realized by just compiling
          //package chmhelppkg in some way
          OperationSucceeded:=true;  //continue with whatever we do next
          WritelnLog('Module BIGIDE: skipped by user.');
        end
        else
        begin
          if ModuleEnabled('BIGIDE')=false then
          begin
            WritelnLog('Module BIGIDE: required by module: HELP');
          end;
          // Make bigide: ide with additional packages as specified by user (in primary config path?)
          // this should also make the lhelp package needed for CHM Help.
          ProcessEx.Executable := FMake;
          ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(LazarusDirectory);
          ProcessEx.Parameters.Clear;
          ProcessEx.Parameters.Add('FPC='+FInstalledCompiler);
          ProcessEx.Parameters.Add('--directory='+LazarusDirectory+'');
          ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
          ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
          ProcessEx.Parameters.Add('bigide');
          infoln('Lazarus: running make bigide:');
          ProcessEx.Execute;
          if ProcessEx.ExitStatus <> 0 then
          begin
            OperationSucceeded := False;
          end;
      end;


      if OperationSucceeded then
      begin
        if not ModuleEnabled('HELP') then
        begin
          OperationSucceeded:=true;  //continue with whatever we do next
          WritelnLog('Module HELP: skipped by user; not building lhelp help viewer.');
        end
        else
        begin
          // Build lhelp chm help viewer
          ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
          // Set directory to item we're compiling:
          ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
            'components'+DirectorySeparator+
            'chmhelp'+DirectorySeparator+
            'lhelp';
          ProcessEx.Parameters.Clear;
          ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
          ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
            'components'+DirectorySeparator+
            'chmhelp'+DirectorySeparator+
            'lhelp'+DirectorySeparator+
            'lhelp.lpr');
          infoln('Lazarus: compiling lhelp help viewer:');
          ProcessEx.Execute;
          if ProcessEx.ExitStatus <> 0 then
          begin
            WritelnLog('Lazarus: error compiling lhelp help viewer.');
            OperationSucceeded := False;
          end;
        end;
      end;

      if OperationSucceeded then
        if not ModuleEnabled('LAZDATADESKTOP') then
        begin
          OperationSucceeded:=true;  //continue with whatever we do next
          WritelnLog('Module LAZDATADESKTOP: skipped by user.');
        end
        else
        begin
          // Build data desktop, nice example of building with lazbuild
          ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
          // Set directory to item we're compiling:
          ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
            'tools'+DirectorySeparator+
            'lazdatadesktop';
          ProcessEx.Parameters.Clear;
          ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
          ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
            'tools'+DirectorySeparator+
            'lazdatadesktop'+DirectorySeparator+
            'lazdatadesktop.lpr');
          infoln('Lazarus: compiling data desktop:');
          ProcessEx.Execute;
          if ProcessEx.ExitStatus <> 0 then
            OperationSucceeded := False;
        end;

      if OperationSucceeded then
        if not ModuleEnabled('DOCEDITOR') then
        begin
          OperationSucceeded:=true;  //continue with whatever we do next
          WritelnLog('Module DOCEDITOR: skipped by user.');
        end
        else
        begin
          // Build Lazarus Doceditor
          ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
          // Set directory to item we're compiling:
          ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
            'doceditor';
          ProcessEx.Parameters.Clear;
          ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
          ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
            'doceditor'+DirectorySeparator+
            'lazde.lpr');
          infoln('Lazarus: compiling doc editor:');
          ProcessEx.Execute;
          if ProcessEx.ExitStatus <> 0 then
            OperationSucceeded := False;
        end;
    end; //native build
  if OperationSucceeded then
    WritelnLog('Lazarus update succeeded at revision number '+ AfterRevision,false);
  ProcessEx.Free;
  Result := OperationSucceeded;
end;

}

constructor TOldInstaller.Create;
var
  LogFileName: string;
begin
  // We'll set the bootstrap compiler to a file in the temp dir.
  // This won't exist so the CheckAndGetNeededExecutables code will download it for us.
  // User can specify an existing CompilerName later on, if she wants to.
  FBootstrapCompilerDirectory := SysUtils.GetTempDir;
  {$IFDEF MSWINDOWS}
  // On Windows, we can always compile 32 bit with a 64 bit cross compiler, regardless
  // of actual architecture (x86 or x64)
  FBootstrapCompilerFTP :=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-win32-ppc386.zip';
  FBootstrapCompilerName := 'ppc386.exe';
  FFPCPlatform:='i386-win32';
  {$ENDIF MSWINDOWS}
  {$IFDEF Linux}
  //If compiled for x86 32 bit, install 32 bit
  //If compiled for x64, install x64 only.//todo: cross compiler!?!
  {$IFDEF CPU386}
  FBootstrapCompilerFTP :=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-linux-ppc386.bz2';
  FBootstrapCompilerName := 'i386-linux-ppc386-1';
  FFPCPlatform:='i386-linux';
  {$ELSE}
  {$IFDEF cpuarmel}
  FBootstrapCompilerFTP :=
  'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/arm-linux-ppcarm.bz2';
  FBootstrapCompilerName := 'arm-linux-ppcarm';
  FFPCPlatform:='arm-linux';
  {$ELSE} // Assume x64 (could also be PowerPC, ARM I suppose)
  FBootstrapCompilerFTP :=
  'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/x86_64-linux-ppcx64.bz2';
  FBootstrapCompilerName := 'x86_64-linux-ppcx64';
  FFPCPlatform:='x86_64-linux';
  {$ENDIF cpuarmel}
  {$ENDIF CPU386}
  {$ENDIF Linux}
  {$IFDEF Darwin}
  //OSX
  FBootstrapCompilerFTP:=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/universal-darwin-ppcuniversal.tar.bz2';
  FBootstrapCompilerName := 'ppcuniversal';
  FFPCPlatform:='x86_64-darwin';
  {$ENDIF Darwin}
  FClean:=false; //Build, not clean, by default

  {$IFDEF MSWINDOWS}
  FExecutableExtension := '.exe';
  {$ELSE}
  FExecutableExtension := '';
  {$ENDIF MSWINDOWS}
  FShortcutName:='Lazarus_trunk'; //Default shortcut name; if it's not empty, shortcut will be written.
  FShortCutNameFpcup:='fpcup_update'; //Default shortcut name; if it's not empty, shortcut will be written.

  FInstalledCompiler := '';
  FSVNDirectory := '';
  SetLazarusPrimaryConfigPath(''); //Let property set up platform-dependent default
  SetMakePath('');

  {$IFDEF MSWINDOWS}
  LogFileName:='fpcup.log'; //current directory
  {$ELSE}
  LogFileName:=ExpandFileNameUTF8('~')+DirectorySeparator+'fpcup.log'; //In home directory
  {$ENDIF MSWINDOWS}
  try
   AssignFile(FLogFile,LogFileName);
   if FileExistsUTF8(LogFileName) then
     Append(FLogFile)
   else
     Rewrite(FLogFile);
  except
    infoln('Error: could not open log file '+LogFileName+' for writing.');
    infoln('This may be caused by another fpcup currently running.');
    infoln('Aborting.');
    halt(2); //Is there a nicer way to do this?
  end;
  WritelnLog(DateTimeToStr(now)+': fpcup started.',false);
  TextRec(FLogVerboseFile).Mode:=0;  //class variables should have been 0
end;

destructor TOldInstaller.Destroy;
begin
  WritelnLog(DateTimeToStr(now)+': fpcup finished.',false);
  WritelnLog('------------------------------------------------',false);
  CloseFile(FLogFile);
  if TextRec(FLogVerboseFile).Mode<>0 then
    CloseFile(FLogVerboseFile);
  FBinUtils.Free;
  inherited Destroy;
end;

end.

