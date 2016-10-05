unit installerCore;
{ Core fpcup installer code
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
  Classes, SysUtils,
  repoclient, GitClient, HGClient, SvnClient,
  processutils, m_crossinstaller, fpcuputil, cpucount;

const
  FPCSVNURL = 'http://svn.freepascal.org/svn';
  BINUTILSURL = FPCSVNURL + '/fpcbuild';
  DEFAULTBINUTILSVERSION = '3.0.0';

type
  TUtilCategory = (ucBinutil {regular binutils like as.exe},
    ucDebugger {Debugger (support) files},
    ucQtFile {e.g. Qt binding},
    ucOther {unknown});

  // Keeps track of downloadable files, e.g. binutils
  TUtilsList= record
    FileName: string;
    //OS as determined by FPC (e.g. WIN32). Blank for all
    OS: string; // For now, OS field is not used as compiler defines manage filling the initial list
    RootURL: string; //URL including trailing / but without filename
    Category: TUtilCategory;
  end;

  { TInstaller }

  TInstaller = class(TObject)
  private
    FKeepLocalChanges: boolean;
    FReApplyLocalChanges: boolean;
    procedure SetURL(value:string);
    function GetMake: string;
    procedure SetHTTPProxyHost(AValue: string);
    procedure SetHTTPProxyPassword(AValue: string);
    procedure SetHTTPProxyPort(AValue: integer);
    procedure SetHTTPProxyUser(AValue: string);
    function DownloadFromBase(aClient:TRepoClient; ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList; const aUserName:string=''; const aPassword:string=''): boolean;
  protected
    FBaseDirectory: string; //Top directory for a product (FPC, Lazarus)
    FBunzip2: string;
    FCompiler: string; // Compiler executable
    FCompilerOptions: string; //options passed when compiling (FPC or Lazarus currently)
    FCPUCount: integer; //logical cpu count (i.e. hyperthreading=2cpus)
    FCrossCPU_Target: string; //When cross-compiling: CPU, e.g. x86_64
    FCrossOPT: string; //options passed (only) when cross-compiling
    FCrossOS_Target: string; //When cross-compiling: OS, e.g. win64
    FCrossOS_SubArch: string; //When cross-compiling for embedded: CPU, e.g. for Teensy SUBARCH=armv7em
    FCrossToolsDirectory: string;
    FCrossLibraryDirectory: string;
    FDesiredRevision: string;
    // Stores tprocessex exception info:
    FErrorLog: TStringList;
    FGitClient: TGitClient;
    FHGClient: THGClient;
    FHTTPProxyHost: string;
    FHTTPProxyPassword: string;
    FHTTPProxyPort: integer;
    FHTTPProxyUser: string;
    FLog: TLogger;
    FLogVerbose: TLogger; // Log file separate from main fpcup.log, for verbose logging
    FMake: string;
    FMakeDir: string; //Binutils/make/patch directory
    FPatchCmd: string;
    FNeededExecutablesChecked: boolean;
    FSVNClient: TSVNClient;
    FSVNDirectory: string;
    FRepositoryUpdated: boolean;
    FURL: string;
    FSourcePatches: string;
    FMajorVersion: integer; //major part of the version number, e.g. 1 for 1.0.8, or -1 if unknown
    FMinorVersion: integer; //minor part of the version number, e.g. 0 for 1.0.8, or -1 if unknown
    FReleaseVersion: integer; //release part of the version number, e.g. 8 for 1.0.8, or -1 if unknown
    FCandidateVersion: integer; //RC part of the version number, e.g. 2 for 1.0.8RC2, or -1 if unknown
    FUtilFiles: array of TUtilsList; //Keeps track of binutils etc download locations, filenames...
    FExportOnly: boolean;
    FNoJobs: boolean;
    FVerbose: boolean;
    FTar: string;
    FUnzip: string;
    ProcessEx: TProcessEx;
    property Make: string read GetMake;
    // Check for existence of required executables; if not there, get them if possible
    function CheckAndGetNeededExecutables: boolean;
    // Check for existence of required binutils; if not there, get them if possible
    function CheckAndGetNeededBinUtils: boolean;
    // Check executable is the right one: run Executable with Parameters
    // and look for ExpectOutput. Returns false if no match.
    function CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
    // Make a list (in FUtilFiles) of all binutils that can be downloaded
    procedure CreateBinutilsList(aVersion:string='');
    // Get a diff of all modified files in and below the directory and save it
    procedure CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
    // Download make.exe, patch.exe etc into the make directory (only implemented for Windows):
    function DownloadBinUtils: boolean;
    // Clone/update using HG; use FBaseDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromHG(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Clone/update using Git; use FBaseDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromGit(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Checkout/update using SVN; use FBaseDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromSVN(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList;const aUserName:string='';const aPassword:string=''): boolean;
    // Download SVN client and set FSVNClient.SVNExecutable if succesful.
    {$IFDEF MSWINDOWS}
    function DownloadSVN: boolean;
    function DownloadOpenSSL: boolean;
    {$ENDIF}
    function DownloadJasmin: boolean;
    procedure DumpOutput(Sender: TProcessEx; output: string);
    // Looks for SVN client in subdirectories and sets FSVNClient.SVNExecutable if found.
    {$IFDEF MSWINDOWS}
    function FindSVNSubDirs: boolean;
    {$ENDIF}
    // Finds compiler in fpcdir path if TFPCInstaller descendant
    function GetCompiler: string;
    function GetCrossInstaller: TCrossInstaller;
    // Returns CPU-OS in the format used by the FPC bin directory, e.g. x86_64-win64:
    function GetFPCTarget(Native: boolean): string;
    // Get currently set path
    function GetPath: string;
    procedure LogError(Sender: TProcessEx; IsException: boolean);
    // Sets the search/binary path to NewPath or adds NewPath before or after existing path:
    procedure SetPath(NewPath: string; Prepend: boolean; Append: boolean);
  public
    property SVNClient: TSVNClient read FSVNClient;
    // Get processerrors and put them into FErrorLog
    procedure ProcessError(Sender:TProcessEx;IsException:boolean);
    // Base directory for installation (fpcdir, lazdir,... option)
    property BaseDirectory: string write FBaseDirectory;
    // Compiler to use for building. Specify empty string when using bootstrap compiler.
    property Compiler: string read GetCompiler write FCompiler;
    // Compiler options passed on to make as OPT=
    property CompilerOptions: string write FCompilerOptions;
    // CPU for the target (together with CrossOS_Target the cross compile equivalent to GetFPCTarget)
    property CrossCPU_Target: string read FCrossCPU_Target write FCrossCPU_Target;
    // Options for cross compiling. User can specify his own, but cross compilers can set defaults, too
    property CrossOPT: string read FCrossOPT write FCrossOPT;
    // OS for target (together with CrossCPU_Target the cross compile equivalent to GetFPCTarget)
    property CrossOS_Target: string read FCrossOS_Target write FCrossOS_Target;
    // SubArch for target embedded
    property CrossOS_SubArch: string read FCrossOS_SubArch write FCrossOS_SubArch;
    property CrossToolsDirectory:string read FCrossToolsDirectory write FCrossToolsDirectory;
    property CrossLibraryDirectory:string read FCrossLibraryDirectory write FCrossLibraryDirectory;
    // SVN revision override. Default is HEAD/latest revision
    property DesiredRevision: string write FDesiredRevision;
    // If using HTTP proxy: host
    property HTTPProxyHost: string read FHTTPProxyHost write SetHTTPProxyHost;
    // If using HTTP proxy: port (optional, default 8080)
    property HTTPProxyPort: integer read FHTTPProxyPort write SetHTTPProxyPort;
    // If using HTTP proxy: username (optional)
    property HTTPProxyUser: string read FHTTPProxyUser write SetHTTPProxyUser;
    // If using HTTP proxy: password (optional)
    property HTTPProxyPassword: string read FHTTPProxyPassword write SetHTTPProxyPassword;
    // Whether or not to let locally modified files remain or back them up to .diff and svn revert before compiling
    property KeepLocalChanges: boolean write FKeepLocalChanges;
    property Log: TLogger write FLog;
    // Directory where make (and the other binutils on Windows) is located
    property MakeDirectory: string write FMakeDir;
    // Patch utility to use. Defaults to 'patch'
    property PatchCmd:string write FPatchCmd;
    // Whether or not to back up locale changes to .diff and reapply them before compiling
    property ReApplyLocalChanges: boolean write FReApplyLocalChanges;
    // URL for download. HTTP, ftp or svn
    property URL: string write SetURL;
    // patches
    property SourcePatches: string read FSourcePatches write FSourcePatches;
    // do not download the repo itself, but only get the files (of master)
    property ExportOnly: boolean write FExportOnly;
    property NoJobs: boolean write FNoJobs;
    // display and log in temp log file all sub process output
    property Verbose: boolean write FVerbose;
    // append line ending and write to log and, if specified, to console
    procedure WritelnLog(msg: string; ToConsole: boolean = true);
    // Build module
    function BuildModule(ModuleName: string): boolean; virtual; abstract;
    // Clean up environment
    function CleanModule(ModuleName: string): boolean; virtual; abstract;
    // Config module
    function ConfigModule(ModuleName: string): boolean; virtual; abstract;
    // Constructs compiler path from directory and architecture
    // Corrects for use of our fpc.sh launcher on *nix
    // Does not verify compiler actually exists.
    function GetCompilerInDir(Dir: string): string;
    // Install update sources
    function GetModule(ModuleName: string): boolean; virtual; abstract;
    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  installerfpc,
  FileUtil, LazFileUtils, LazUTF8,
  ssl_openssl
  // for runtime init of openssl
  {$IFDEF MSWINDOWS}
  ,blcksock, ssl_openssl_lib
  {$ENDIF}
  ;

{ TInstaller }

function TInstaller.GetCompiler: string;
begin
  if (Self is TFPCNativeInstaller) or (Self is TFPCInstaller) then
    Result := GetCompilerInDir(FBaseDirectory)
  else
    Result := FCompiler;
end;

function TInstaller.GetCrossInstaller: TCrossInstaller;
var
  idx: integer;
  target: string;
begin
  Result := nil;
  target := GetFPCTarget(false);
  if assigned(CrossInstallers) then
    for idx := 0 to CrossInstallers.Count - 1 do
      if CrossInstallers[idx] = target then
      begin
        Result := TCrossInstaller(CrossInstallers.Objects[idx]);
        break;
      end;
end;

procedure TInstaller.SetURL(value:string);
begin
  FURL:=value;
  FMajorVersion := -1;
  FMinorVersion := -1;
  FReleaseVersion := -1;
  FCandidateVersion := -1;
end;

function TInstaller.GetMake: string;
begin
  if FMake = '' then
    {$IFDEF MSWINDOWS}
    FMake := IncludeTrailingPathDelimiter(FMakeDir) + 'make' + GetExeExt;
    {$ELSE}
    {$IF defined(BSD) and not defined(DARWIN)}
    FMake := 'gmake'; //GNU make; assume in path
    {$else}
    // Linux, OSX
    FMake := 'make'; //assume in path
    {$ENDIF}
    {$ENDIF MSWINDOWS}
  Result := FMake;
end;

procedure TInstaller.SetHTTPProxyHost(AValue: string);
begin
  if FHTTPProxyHost=AValue then Exit;
  FHTTPProxyHost:=AValue;
  FGitClient.HTTPProxyHost:=FHTTPProxyHost;
  FHGClient.HTTPProxyHost:=FHTTPProxyHost;
  FSVNClient.HTTPProxyHost:=FHTTPProxyHost;
end;

procedure TInstaller.SetHTTPProxyPassword(AValue: string);
begin
  if FHTTPProxyPassword=AValue then Exit;
  FHTTPProxyPassword:=AValue;
  FGitClient.HTTPProxyPassword:=FHTTPProxyPassword;
  FHGClient.HTTPProxyPassword:=FHTTPProxyPassword;
  FSVNClient.HTTPProxyPassword:=FHTTPProxyPassword;
end;

procedure TInstaller.SetHTTPProxyPort(AValue: integer);
begin
  if FHTTPProxyPort=AValue then Exit;
  FHTTPProxyPort:=AValue;
  FGitClient.HTTPProxyPort:=FHTTPProxyPort;
  FHGClient.HTTPProxyPort:=FHTTPProxyPort;
  FSVNClient.HTTPProxyPort:=FHTTPProxyPort;
end;

procedure TInstaller.SetHTTPProxyUser(AValue: string);
begin
  if FHTTPProxyUser=AValue then Exit;
  FHTTPProxyUser:=AValue;
  FGitClient.HTTPProxyUser:=FHTTPProxyUser;
  FHGClient.HTTPProxyUser:=FHTTPProxyUser;
  FSVNClient.HTTPProxyUser:=FHTTPProxyUser;
end;

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  AllThere: boolean;
  i: integer;
  OperationSucceeded: boolean;
  Output: string;
{$IFDEF MSWINDOWS}
function GetFile(aFile:string):boolean;
var
  aFilename:string;
begin
  result:=true;
  aFilename:=ExtractFileName(aFile);
  if (NOT FileExists(aFile)) then
  begin
    infoln('Downloading ' + aFilename + ' into ' + FMakeDir,etDebug);
    try
      if Download(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTBINUTILSVERSION,'.','_',[rfReplaceAll])+'/install/binw32/'+aFilename,
            aFile,
            FHTTPProxyHost,
            inttostr(FHTTPProxyPort),
            FHTTPProxyUser,
            FHTTPProxyPassword) = false then
          begin
            Result := false;
            infoln('Error downloading '  + aFilename + ' to ' + FMakeDir,eterror);
          end;
    except
      on E: Exception do
      begin
        Result := false;
        infoln('Error downloading '  + aFilename + ' : ' + E.Message,eterror);
        exit; //out of function.
      end;
    end;
  end;
end;
{$ENDIF}
begin
  OperationSucceeded := true;
  if not FNeededExecutablesChecked then
  begin
    // The extractors used depend on the bootstrap compiler URL/file we download
    // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)
    {$IFDEF MSWINDOWS}
    // Need to do it here so we can pick up make path.
    FBunzip2 := '';
    FTar := '';
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FBunzip2 := 'bunzip2';
    FTar := 'tar';
    FUnzip := 'unzip'; //unzip needed at least for FPC chm help
    {$ENDIF LINUX}
    {$IFDEF BSD} //OSX, *BSD
    {$IFDEF DARWIN}
    FBunzip2 := ''; //not really necessary now
    FTar := 'bsdtar'; //gnutar is not available by default on Mavericks
    FUnzip := 'unzip'; //unzip needed at least for FPC chm help
    {$ELSE} //FreeBSD, OpenBSD, NetBSD
    FBunzip2 := 'bunzip2';
    FTar := 'tar'; //At least FreeBSD tar apparently takes some gnu tar options nowadays.
    FUnzip := 'unzip'; //unzip needed at least for FPC chm help
    {$ENDIF DARWIN}
    {$ENDIF BSD}

    {$IFDEF MSWINDOWS}
    ForceDirectoriesUTF8(FMakeDir);

    // Get unzip binary from default binutils URL
    FUnzip := IncludeTrailingPathDelimiter(FMakeDir) + 'unzip.exe';
    GetFile(FUnzip);

    // Get patch binary from default binutils URL
    GetFile(IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe');
    GetFile(IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe.manifest');
    {$ENDIF}

    {$IF defined(LINUX) or (defined(BSD) and (not defined(DARWIN)))} //Linux,FreeBSD,NetBSD,OpenBSD, but not OSX
    //todo: check if we need as on OSX as well
    if OperationSucceeded then
    begin
      // Check for proper assembler
      try
        if ExecuteCommand('as --version', Output, FVerbose) <> 0 then
        begin
          infoln('Missing assembler as. Please install the developer tools.',eterror);
          OperationSucceeded := false;
        end;
      except
        OperationSucceeded := false;
        // ignore errors, this is only an extra check
      end;
    end;
    {$ENDIF defined(LINUX) or (defined(BSD) and (not defined(DARWIN)))}

    if OperationSucceeded then
    begin
      // Check for valid unzip executable, if it is needed
      if FUnzip <> EmptyStr then
      begin
        {$IF (defined(BSD)) and (not defined(Darwin))}
        // FreeBSD doesn't have an unzip applicationt that responds without needing a zip file
        // No motivation to go feed it a zip file just to test it
        OperationSucceeded := true;
        {$ELSE}
        // OSes with a normal unzip
        OperationSucceeded := CheckExecutable(FUnzip, '-v', '');
        {$ENDIF (defined(BSD)) and (not defined(Darwin))}
      end;
    end;

    {$IFDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      // check availability of OpenSSL libraries. Just continue in case oof error
      if FileExists(SafeGetApplicationPath+'libeay32.dll') AND FileExists(ExtractFilePath(ParamStr(0))+'ssleay32.dll') then
      begin
        infoln('Found OpenSLL library files.',etInfo);
      end
      else
      begin
        infoln('No OpenSLL library files available. Going to download them',etWarning);
        DownloadOpenSSL;
      end;
    end;
    {$ENDIF}


    if OperationSucceeded then
    begin
      // Look for (ini-file) default SVN executable
      AllThere:=FSVNClient.ValidClient;

      {$IFDEF MSWINDOWS}
      if NOT AllThere then
      begin
        // look local (only for Windows: we could have downloaded a SVN client earlier)
        // will look in and below FSVNDirectory
        FSVNDirectory := IncludeTrailingPathDelimiter(FMakeDir) + FSVNClient.RepoExecutableName + DirectorySeparator;
        AllThere:=FindSVNSubDirs;
      end;
      {$ENDIF}

      if NOT AllThere then
      begin
        // look system default
        FSVNDirectory := '';
        AllThere:=Length(FSVNClient.FindRepoExecutable)<>0;
        //AllThere:=Length(FSVNClient.RepoExecutable)<>0;
      end;

      {$IFDEF MSWINDOWS}
      if NOT AllThere then
      begin
        infoln('Going to download SVN',etInfo);
        // Download will look in and below FSVNDirectory
        // and set FSVNClient.SVNExecutable if succesful
        FSVNDirectory := IncludeTrailingPathDelimiter(FMakeDir) + FSVNClient.RepoExecutableName + DirectorySeparator;
        AllThere := DownloadSVN;
      end;
      {$ENDIF}

      // Regardless of platform, SVN should now be either set up correctly or we should give up.
      if NOT AllThere then
      begin
        OperationSucceeded := false;
        infoln('Could not find SVN executable. Please make sure it is installed.',eterror);
      end else WritelnLog('SVN client found: ' + FSVNClient.FindRepoExecutable, true);
    end;

    if OperationSucceeded then
    begin
      // Check for valid bunzip2 executable, if it is needed
      if FBunzip2 <> EmptyStr then
      begin
        { Used to use bunzip2 --version, but on e.g. Fedora Core
        that returns an error message e.g. can't read from cp
        }
        OperationSucceeded := CheckExecutable(FBunzip2, '--help', '');
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid tar executable, if it is needed
      if FTar <> EmptyStr then
      begin
        OperationSucceeded := CheckExecutable(FTar, '--version', '');
      end;
    end;

    FNeededExecutablesChecked:=OperationSucceeded;
  end;
  Result := OperationSucceeded;
end;

function TInstaller.CheckAndGetNeededBinUtils: boolean;
var
  AllThere: boolean;
  i: integer;
  OperationSucceeded: boolean;
  Output: string;
begin
  OperationSucceeded := true;

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Download if needed, including unzip - needed for SVN download
    // Check for binutils directory
    AllThere:=true;
    if DirectoryExists(FMakeDir) = false then
    begin
      infoln('Make path ' + FMakeDir + ' does not exist. Going to download binutils.',etInfo);
      AllThere:=false;
    end
    else
    begin
      // Check all binutils in directory
      for i:=low(FUtilFiles) to high(FUtilFiles) do
      begin
        if FUtilFiles[i].Category=ucBinutil then
        begin
          if not(FileExists(IncludeTrailingPathDelimiter(FMakeDir)+FUtilFiles[i].FileName)) then
          begin
            AllThere:=false;
            infoln('Make path ' + FMakeDir + ' does not have (all) binutils. Going to download needed binutils.',etInfo);
            break;
          end;
        end;
      end;
    end;
    if not(AllThere) then OperationSucceeded := DownloadBinUtils;
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded then
  begin
    // Check for proper make executable
    try
      ExecuteCommand(Make + ' -v', Output, FVerbose);
      if Ansipos('GNU Make', Output) = 0 then
      begin
        infoln('Found make executable, but it is not GNU Make.',etError);
        OperationSucceeded := false;
      end;
    except
      // ignore errors, this is only an extra check
    end;
  end;

  Result := OperationSucceeded;
end;


function TInstaller.CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
var
  ResultCode: longint;
  OperationSucceeded: boolean;
  ExeName: string;
  Output: string;
begin
  try

    ExeName := ExtractFileName(Executable);
    ResultCode := ExecuteCommand(Executable + ' ' + Parameters, Output, FVerbose);
    if ResultCode >= 0 then //Not all non-0 result codes are errors. There's no way to tell, really
    begin
      if (ExpectOutput <> '') and (Ansipos(ExpectOutput, Output) = 0) then
      begin
        // This is not a warning/error message as sometimes we can use multiple different versions of executables
        infoln(Executable + ' is not a valid ' + ExeName + ' application. ' +
          ExeName + ' exists but shows no (' + ExpectOutput + ') in its output.',etDebug);
        OperationSucceeded := false;
      end
      else
      begin
        // We're not looking for any specific output so we're happy
        OperationSucceeded := true;
      end;
    end
    else
    begin
      // This is not a warning/error message as sometimes we can use multiple different versions of executables
      infoln(Executable + ' is not a valid ' + ExeName + ' application (' + ExeName + ' result code was: ' + IntToStr(ResultCode) + ')',etDebug);
      OperationSucceeded := false;
    end;
  except
    on E: Exception do
    begin
      // This is not a warning/error message as sometimes we can use multiple different versions of executables
      infoln(Executable + ' is not a valid ' + ExeName + ' application (' + 'Exception: ' + E.ClassName + '/' + E.Message + ')', etDebug);
      OperationSucceeded := false;
    end;
  end;
  if OperationSucceeded then
    infoln('Found valid ' + ExeName + ' application.',etDebug);
  Result := OperationSucceeded;
end;

procedure TInstaller.CreateBinutilsList(aVersion:string);
// Windows-centric for now; doubt if it
// can be used in Unixy systems anyway
const
  SourceURL_gdb = FPCSVNURL+'/lazarus/binaries/i386-win32/gdb/bin/';
  SourceURL64_gdb = FPCSVNURL+'/lazarus/binaries/x86_64-win64/gdb/bin/';
  SourceURL_Qt = FPCSVNURL+'/lazarus/binaries/i386-win32/qt/';

  procedure AddNewUtil(FileName, RootURL, OS: string; Category: TUtilCategory);
  var
    i: integer;
  begin
    SetLength(FUtilFiles, 2+high(FUtilFiles)-low(FUtilFiles));
    i:=high(FUtilFiles);
    FUtilFiles[i].FileName:=FileName;
    FUtilFiles[i].RootURL:=RootURL;
    FUtilFiles[i].OS:=OS;
    FUtilFiles[i].Category:=Category;
  end;
var
  aSourceURL:string;
  {$ifdef win64}
  aSourceURL64:string;
  {$endif}
  aTag:string;
begin


  SetLength(FUtilFiles,0); //clean out any cruft

  {$IFDEF MSWINDOWS}

  // default
  if aVersion='' then aVersion:=DEFAULTBINUTILSVERSION;

  //trunk
  if aVersion='3.1.1'
     then aTag:='trunk'
     else aTag:='tags/release_'+StringReplace(aVersion,'.','_',[rfReplaceAll]);

  aSourceURL:=BINUTILSURL+'/'+aTag+'/install/binw32/';
  {$ifdef win64}
  aSourceURL64:=BINUTILSURL+'/'+aTag+'/install/binw64/';
  {$endif}

  // Common to both 32 and 64 bit windows (i.e. 32 bit files)
  AddNewUtil('cpp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('dlltool' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('fp32.ico',aSourceURL,'',ucBinutil);
  AddNewUtil('gcc' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('grep' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('windres' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('windres.h',aSourceURL,'',ucBinutil);
  {$ifdef win32}
  AddNewUtil('ar' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('as' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('cmp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('cp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('diff' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('gdate' + GetExeExt,aSourceURL,'',ucBinutil);
  //AddNewUtil('gdb' + GetExeExt,aSourceURL_gdb,'',ucDebugger);
  AddNewUtil('gdb' + GetExeExt,aSourceURL,'',ucDebugger);
  AddNewUtil('libexpat-1.dll',aSourceURL,'',ucDebugger);
  AddNewUtil('gecho' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt + '.manifest',aSourceURL,'',ucBinutil);
  AddNewUtil('gmkdir' + GetExeExt,aSourceURL,'',ucBinutil);
  //AddNewUtil('GoRC' + GetExeExt,aSourceURL,'',ucBinutil);
  {
  http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
  only has libexpat-1, so no need for these:
  AddNewUtil('libgcc_s_dw2-1.dll',aSourceURL,'',ucBinutil);
  AddNewUtil('libiconv-2.dll',aSourceURL,'',ucBinutil);
  AddNewUtil('libintl-8.dll',aSourceURL,'',ucBinutil);
  }
  AddNewUtil('ld' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('make' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('mv' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('objdump' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('pwd' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('rm' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('strip' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('Qt4Pas5.dll',SourceURL_Qt,'',ucQtFile);
  {$endif win32}
  {$ifdef win64}
  AddNewUtil('ar' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('as' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('cmp' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('cp' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('diff' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('gdate' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('gdb' + GetExeExt,SourceURL64_gdb,'',ucDebugger);
  AddNewUtil('libiconv-2.dll',SourceURL64_gdb,'',ucDebugger);
  AddNewUtil('gecho' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('ginstall.exe.manifest',aSourceURL64,'',ucBinutil);
  AddNewUtil('gmkdir' + GetExeExt,aSourceURL64,'',ucBinutil);
  //AddNewUtil('GoRC' + GetExeExt,aSourceURL64,'',ucBinutil);

  AddNewUtil('ld' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('make' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('mv' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('objdump' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('pwd' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('rm' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('strip' + GetExeExt,aSourceURL64,'',ucBinutil);
  //No equivalent for Win64...
  //AddNewUtil('Qt4Pas5.dll',SourceURL64_Qt,'',ucBinding);
  {$endif win64}
  {$ENDIF MSWINDOWS}
end;

procedure TInstaller.CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
var
  DiffFile: Text;
begin
  while FileExists(DiffFileName) do
    DiffFileName := DiffFileName + 'f';
  AssignFile(DiffFile, DiffFileName);
  Rewrite(DiffFile);
  if assigned(RepoClass) then
  begin
    if RepoClass is THGClient then
    begin
      Write(DiffFile, FHGClient.GetDiffAll);
    end
    else if RepoClass is TGitClient then
    begin
      Write(DiffFile, FGitClient.GetDiffAll);
    end
    else if RepoClass is TSVNClient then
    begin
      Write(DiffFile, FSVNClient.GetDiffAll);
    end
    else raise Exception.CreateFmt('Error writing diff file. Technical details: unknown repository object %s passed. Please fix the code.',[RepoClass.ClassName]);
  end
  else
  begin
    raise Exception.Create('Error writing diff file. Technical details: invalid/no repository object passed. Please fix the code.');
  end;
  CloseFile(DiffFile);
  UpdateWarnings.Add('Diff with last revision stored in ' + DiffFileName);
end;

function TInstaller.DownloadBinUtils: boolean;
  // Download binutils. For now, only makes sense on Windows...
var
  Counter: integer;
  Errors: integer = 0;
  RetryAttempt:integer;
  DownloadSuccess:boolean;
begin
  //Parent directory of files. Needs trailing backslash.
  ForceDirectoriesUTF8(FMakeDir);
  Result := true;
  for Counter := low(FUtilFiles) to high(FUtilFiles) do
  begin
    if (FUtilFiles[Counter].Category=ucBinutil) or (FUtilFiles[Counter].Category=ucDebugger) then
    begin
      if (FileExists(IncludeTrailingPathDelimiter(FMakeDir)+FUtilFiles[Counter].FileName)) then continue;
      infoln('Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + FMakeDir,etDebug);
      RetryAttempt:=0;
      repeat
        try
          if (RetryAttempt>0) then infoln('Error downloading binutil: ' + FUtilFiles[Counter].FileName + ' to ' + FMakeDir + '. Retrying.',etError);
          DownloadSuccess:=Download(FUtilFIles[Counter].RootURL + FUtilFiles[Counter].FileName,
            IncludeTrailingPathDelimiter(FMakeDir) + FUtilFiles[Counter].FileName,
            FHTTPProxyHost,
            inttostr(FHTTPProxyPort),
            FHTTPProxyUser,
            FHTTPProxyPassword);
          if NOT DownloadSuccess then
          begin
            Inc(RetryAttempt);
            sleep(100*RetryAttempt);
          end;
        except
          on E: Exception do
          begin
            Result := false;
            infoln('Fatal error downloading binutils: ' + E.Message+'. Exit.',etError);
            exit; //out of function.
          end;
        end;

      until ( (DownloadSuccess) OR (RetryAttempt>5) );

      if NOT DownloadSuccess then Errors := Errors + 1;

    end;
  end;

  if Errors > 0 then
  begin
    Result := false;
    WritelnLog('DownloadBinUtils: ' + IntToStr(Errors) + ' error(s) downloading binutils.', true);
  end;
end;

function TInstaller.DownloadFromBase(aClient:TRepoClient; ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList; const aUserName:string=''; const aPassword:string=''): boolean;
var
  BeforeRevisionShort: string; //Basically the branch revision number
  ReturnCode: integer;
  aClientName:string;
  DiffFile: String;
  LocalPatchCmd : string;
  DiffFileSL:TStringList;
  Output: string = '';
begin

  aClientName:=Copy(aClient.ClassName,2,200);

  //todo: check if we need to add forcedirectoriesutf8 to create local repo dir if it doesn't exist
  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  aClient.LocalRepository := FBaseDirectory;
  aClient.Repository := FURL;

  BeforeRevision := 'revision '+aClient.LocalRevision;
  BeforeRevisionShort:=aClient.LocalRevision;

  if BeforeRevisionShort<>FRET_UNKNOWN_REVISION then
  begin
    aClient.LocalModifications(UpdateWarnings); //Get list of modified files
    if UpdateWarnings.Count > 0 then
    begin
      UpdateWarnings.Insert(0, ModuleName + ': WARNING: found modified files.');
      if FKeepLocalChanges=false then
      begin
        DiffFile:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'REV' + BeforeRevisionShort + '.diff';
        CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,aClient);
        UpdateWarnings.Add(ModuleName + ': reverting to original before updating.');
        aClient.Revert; //Remove local changes
      end else UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
    end;
  end;

  aClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  aClient.CheckOutOrUpdate;
  ReturnCode := aClient.ReturnCode;
  case ReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog(aClientName+' ERROR: repository URL in local directory and remote repository don''t match.', true);
      writelnlog('Local directory: ' + aClient.LocalRepository, true);
      infoln('Have you specified the wrong directory or a directory with an old repository checkout?',etDebug);
    end;
    else
    begin
      // For now, assume it worked even with non-zero result code. We can because
      // we do the AfterRevision check as well.
      Result := true;

      AfterRevision := 'revision '+aClient.LocalRevision;
      if (aClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (BeforeRevisionShort <> aClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      if FReApplyLocalChanges and (DiffFile<>'') then
      begin
         UpdateWarnings.Add(ModuleName + ': reapplying local changes.');

         // check for default values
         if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
            then LocalPatchCmd:=FPatchCmd + ' -p0 -i '
            else LocalPatchCmd:=Trim(FPatchCmd) + ' ';

         ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FBaseDirectory, Output, FVerbose);

         {$IFNDEF MSWINDOWS}
         if ReturnCode<>0 then
         begin
           // Patching can go wrong when line endings are not compatible
           // This happens e.g. with bgracontrols that have CRLF in the source files
           // Try to circumvent this problem by trick below (replacing line enddings)
           if Pos('different line endings',Output)>0 then
           begin
             ReturnCode:=ExecuteCommandInDir('unix2dos '+DiffFile, FBaseDirectory, FVerbose);
             if ReturnCode<>0 then
             begin
               DiffFileSL:=TStringList.Create();
               try
                 DiffFileSL.LoadFromFile(DiffFile);
                 DiffFileSL.TextLineBreakStyle:=tlbsCRLF;
                 DiffFileSL.SaveToFile(DiffFile);
                 ReturnCode:=0;
               finally
                 DiffFileSL.Free();
               end;
               //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, FBaseDirectory, FVerbose);
               //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, FBaseDirectory, FVerbose);
             end;
             if ReturnCode=0 then
             begin
               // check for default values
               if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
                  then LocalPatchCmd:=FPatchCmd + ' -p0 --binary -i '
                  else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
               ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FBaseDirectory, Output, FVerbose);
             end;
           end;
         end;
         {$ENDIF}
         // Report error, but continue !
         if ReturnCode<>0 then
         begin
           writelnlog(aClientName+' ERROR: Patching with ' + DiffFile + ' failed.', true);
           writelnlog(aClientName+' output: ' + Output, true);
           writelnlog('Verify the state of the source, correct and rebuild with make.', true);
         end;
       end;
    end;
  end;
end;

function TInstaller.DownloadFromHG(ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  result:=DownloadFromBase(FHGClient,ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
end;

function TInstaller.DownloadFromGit(ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  result:=DownloadFromBase(FGitClient,ModuleName,BeforeRevision,AfterRevision,UpdateWarnings);
end;

function TInstaller.DownloadFromSVN(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList;const aUserName:string='';const aPassword:string=''): boolean;
var
  BeforeRevisionShort: string; //Basically the branch revision number
  CheckoutOrUpdateReturnCode: integer;
  DiffFile: String;
  RepoExists: boolean;
  LocalPatchCmd : string;
  Output: string = '';
  DiffFileSL:TStringList;
begin
  result:=true;

  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  FSVNClient.LocalRepository := FBaseDirectory;
  FSVNClient.Repository := FURL;
  FSVNClient.UserName:=aUserName;
  FSVNClient.Password:=aPassword;
  RepoExists:=FSVNClient.LocalRepositoryExists;
  if RepoExists then
  begin
    if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
      BeforeRevision := 'revision '+FSVNClient.LocalRevisionWholeRepo
    else
      BeforeRevision := 'branch revision '+FSVNClient.LocalRevision+' (repository revision '+FSVNClient.LocalRevisionWholeRepo+')';
    BeforeRevisionShort:=FSVNClient.LocalRevision;
  end
  else
  begin
    // We could insist on the repo existing, but then we wouldn't be able to checkout!!
    writelnlog('INFO: directory ' + FBaseDirectory + ' is not an SVN repository (or a repository with the wrong remote URL).');
    if not(DirectoryExistsUTF8(FSVNClient.LocalRepository)) then
    begin
      writelnlog('INFO: creating directory '+FBaseDirectory+' for SVN checkout.');
      ForceDirectoriesUTF8(FBaseDirectory);
    end;
  end;

  if (FSVNClient.LocalRevisionWholeRepo = FRET_UNKNOWN_REVISION) and (FSVNClient.Returncode=FRET_WORKING_COPY_TOO_OLD) then
  begin
    writelnlog('ERROR: The working copy in ' + FBaseDirectory + ' was created with an older, incompatible version of svn.', true);
    writelnlog('  Run svn upgrade in the directory or make sure the original svn executable is the first in the search path.', true);
    result := false;  //fail
    exit;
  end;

  if RepoExists then
  begin
    FSVNClient.LocalModifications(UpdateWarnings); //Get list of modified files
    DiffFile:='';
    if UpdateWarnings.Count > 0 then
    begin
      UpdateWarnings.Insert(0, ModuleName + ': WARNING: found modified files.');
      if FKeepLocalChanges=false then
      begin
        DiffFile:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'REV' + BeforeRevisionShort + '.diff';
        CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,FSVNClient);
        UpdateWarnings.Add(ModuleName + ': reverting before updating.');
        FSVNClient.Revert; //Remove local changes
      end
      else
      begin
        UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
      end;
    end;
  end;

  FSVNClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  infoln('Running SVN checkout or update.',etInfo);
  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  FSVNClient.CheckOutOrUpdate;
  CheckoutOrUpdateReturnCode := FSVNClient.ReturnCode;
  case CheckoutOrUpdateReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog('ERROR: repository URL in local directory and remote repository don''t match.', true);
      writelnlog('Local directory: ' + FSVNClient.LocalRepository, true);
      infoln('Have you specified the wrong directory or a directory with an old repository checkout?',etDebug);
    end;
    else
    begin
      // If there are svn errors, return a false result.
      // We used to do a check for the revision, but that does not check the integrity
      // or existence of all files in the svn repo.
      if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
        AfterRevision := 'revision '+FSVNClient.LocalRevisionWholeRepo
      else
        AfterRevision := 'branch revision '+FSVNClient.LocalRevision+' (repository revision '+FSVNClient.LocalRevisionWholeRepo+')';
      if (FSVNClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (BeforeRevisionShort <> FSVNClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      // Only return success if svn returned return code 0
      Result := (CheckoutOrUpdateReturnCode=0);

      if not Result then
      begin
        writelnlog('DownloadFromSVN: SVN gave error code: '+inttostr(CheckoutOrUpdateReturnCode));
        writelnlog('DownloadFromSVN: SVN gave error message: '+FSVNClient.ReturnOutput);
      end;

      if Result and FReApplyLocalChanges and (DiffFile<>'') then
      begin
        UpdateWarnings.Add(ModuleName + ': reapplying local changes.');

        if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
           then LocalPatchCmd:=FPatchCmd + ' -p0 -i '
           else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
        CheckoutOrUpdateReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FBaseDirectory, Output, FVerbose);

        {$IFNDEF MSWINDOWS}
        if CheckoutOrUpdateReturnCode<>0 then
        begin
          // Patching can go wrong when line endings are not compatible
          // This happens e.g. with bgracontrols that have CRLF in the source files
          // Try to circumvent this problem by trick below (replacing line enddings)
          if Pos('different line endings',Output)>0 then
          begin
            CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('unix2dos '+DiffFile, FBaseDirectory, FVerbose);
            if CheckoutOrUpdateReturnCode<>0 then
            begin
              DiffFileSL:=TStringList.Create();
              try
                DiffFileSL.LoadFromFile(DiffFile);
                DiffFileSL.TextLineBreakStyle:=tlbsCRLF;
                DiffFileSL.SaveToFile(DiffFile);
                CheckoutOrUpdateReturnCode:=0;
              finally
                DiffFileSL.Free();
              end;
              //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, FBaseDirectory, FVerbose);
              //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, FBaseDirectory, FVerbose);
            end;
            if CheckoutOrUpdateReturnCode=0 then
            begin
              if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
                 then LocalPatchCmd:=FPatchCmd + ' -p0 --binary -i '
                 else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
              CheckoutOrUpdateReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FBaseDirectory, Output, FVerbose);
            end;
          end;
        end;
        {$ENDIF}
        // Report error, but continue !
        if CheckoutOrUpdateReturnCode<>0 then
        begin
          writelnlog('SVNClient ERROR: Patching with ' + DiffFile + ' failed.', true);
          writelnlog('SVNClient output: ' + Output, true);
          writelnlog('Verify the state of the source, correct and rebuild with make.', true);
        end;
      end;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
function TInstaller.DownloadSVN: boolean;
const
  //SourceURL = 'http://www.visualsvn.com/files/Apache-Subversion-1.8.4.zip';
  // Changed to https
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.8.4.zip';
  //SourceURL = 'http://sourceforge.net/projects/win32svn/files/1.8.4/apache24/svn-win32-1.8.4-ap24.zip/download';
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.8.13.zip';
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.8.14.zip';
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.9.0.zip';
  SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.9.1.zip';
  //SourceURL = 'https://sourceforge.net/projects/win32svn/files/1.8.15/apache24/svn-win32-1.8.15-ap24.zip/download';
  // confirmed by winetricks bug report that this is the only one left...
  // this link seems down 'http://download.microsoft.com/download/vc60pro/update/1/w9xnt4/en-us/vc6redistsetup_enu.exe';
var
  MajorVersion,MinorVersion,BuildNumber: integer;
  OperationSucceeded: boolean;
  ResultCode: longint;
  SVNZip: string;
begin
  // Download SVN in make path. Not required for making FPC/Lazarus, but when downloading FPC/Lazarus from... SVN ;)
  { Alternative 1: sourceforge packaged
  This won't work, we'd get an .msi:
  http://sourceforge.net/projects/win32svn/files/latest/download?source=files
  We don't want msi/Windows installer - this way we can hopefully support Windows 2000, so use:
  http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip
  This one requires msvcp60.dll which is horrific to install
  }

  {Alternative 2: use
  http://www.visualsvn.com/files/Apache-Subversion-1.8.4.zip
  with subdirs bin and licenses. No further subdirs
  However, doesn't work on Windows 2K.
  Decided to use this anyway.}
  OperationSucceeded := true;

  // This svn version won't work on windows 2K
  if GetWin32Version(MajorVersion,MinorVersion,BuildNumber) and (MajorVersion=5) and (Minorversion=0) then
  begin
    writelnlog('ERROR: it seems this PC is running Windows 2000. Cannot install svn.exe. Please manually install e.g. TortoiseSVN first.', true);
    exit(false);
  end;

  ForceDirectoriesUTF8(FSVNDirectory);

  SVNZip := SysUtils.GetTempFileName + '.zip';
  try
    if OperationSucceeded then
    begin
      OperationSucceeded := Download(
        SourceURL,
        SVNZip,
        FHTTPProxyUser,
        inttostr(FHTTPProxyPort),
        FHTTPProxyUser,
        FHTTPProxyPassword);
    end;
  except
    // Deal with timeouts, wrong URLs etc
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog('ERROR: exception ' + E.ClassName + '/' + E.Message + ' downloading SVN client from ' + SourceURL, true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    resultcode:=ExecuteCommand(FUnzip + ' -o -d ' + FSVNDirectory + ' ' + SVNZip, FVerbose);
    if resultcode <> 0 then
    begin
      OperationSucceeded := false;
      writelnlog('DownloadSVN: ERROR: unzip returned result code: ' + IntToStr(ResultCode));
    end;
  end
  else
  begin
    writelnlog('ERROR downloading SVN client from ' + SourceURL, true);
  end;

  if OperationSucceeded then
  begin
    WritelnLog('SVN download and unpacking ok. Not going to search SVN client itself in ' + FSVNDirectory, true);
    OperationSucceeded := FindSVNSubDirs;
    if OperationSucceeded then
      SysUtils.Deletefile(SVNZip); //Get rid of temp zip if success.
  end;
  Result := OperationSucceeded;
end;

function TInstaller.DownloadOpenSSL: boolean;
const
  {$ifdef win64}
  SourceURL = 'http://indy.fulgan.com/SSL/openssl-1.0.2e-x64_86-win64.zip';
  {$endif}
  {$ifdef win32}
  SourceURL = 'http://indy.fulgan.com/SSL/openssl-1.0.2e-i386-win32.zip';
  {$endif}
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  OpenSSLZip: string;
begin
  OperationSucceeded := true;

  ForceDirectoriesUTF8(SafeGetApplicationPath);

  OpenSSLZip := SysUtils.GetTempFileName + '.zip';
  try
    if OperationSucceeded then
    begin
      OperationSucceeded := Download(
        SourceURL,
        OpenSSLZip,
        FHTTPProxyUser,
        inttostr(FHTTPProxyPort),
        FHTTPProxyUser,
        FHTTPProxyPassword);
    end;
  except
    // Deal with timeouts, wrong URLs etc
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog('ERROR: exception ' + E.ClassName + '/' + E.Message + ' downloading OpenSSL library from ' + SourceURL, true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    resultcode:=ExecuteCommand(FUnzip + ' -o -d ' + SafeGetApplicationPath + ' ' + OpenSSLZip + ' libeay32.dll ssleay32.dll' , FVerbose);
    if resultcode <> 0 then
    begin
      OperationSucceeded := false;
      writelnlog('Download OpenSSL: ERROR: unzip returned result code: ' + IntToStr(ResultCode));
    end;
  end;

  if OperationSucceeded then
  begin
    WritelnLog('OpenSLL download and unpacking ok.', true);
    SysUtils.Deletefile(OpenSSLZip); //Get rid of temp zip if success.
  end;
  Result := OperationSucceeded;

  //SslLibraryInit;
  if InitSSLInterface then
      SSLImplementation := TSSLOpenSSL;
end;

{$ENDIF}

function TInstaller.DownloadJasmin: boolean;
const
  SourceURL = 'http://sourceforge.net/projects/jasmin/files/jasmin/jasmin-2.4/jasmin-2.4.zip/download';
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  JasminZip,JasminDir: string;
begin
  JasminDir:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator;
  if NOT FileExists(JasminDir+'jasmin.jar') then
  begin
    OperationSucceeded := true;
    JasminZip := SysUtils.GetTempFileName + '.zip';
    try
      if OperationSucceeded then
      begin
        OperationSucceeded := Download(
          SourceURL,
          JasminZip,
          FHTTPProxyUser,
          inttostr(FHTTPProxyPort),
          FHTTPProxyUser,
          FHTTPProxyPassword);
      end;
    except
      // Deal with timeouts, wrong URLs etc
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog('ERROR: exception ' + E.ClassName + '/' + E.Message + ' downloading Jasmin assembler from ' + SourceURL, true);
      end;
    end;

    // for now, just put jasmin.jar in bin directory ... easy and simple and working
    JasminDir:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator;

    if OperationSucceeded then
    begin
      // Extract, overwrite
      resultcode:=ExecuteCommand(FUnzip + ' -o -d ' + SysUtils.GetTempDir + ' ' + JasminZip, FVerbose);
      if resultcode = 0 then
      begin
        OperationSucceeded := MoveFile(IncludeTrailingPathDelimiter(SysUtils.GetTempDir)+IncludeTrailingPathDelimiter(fpcuputil.ExtractFileNameOnly(GetFileNameFromURL(SourceURL)))+'jasmin.jar',JasminDir+'jasmin.jar');
        if NOT OperationSucceeded then
        begin
          writelnlog('Could not move jasmin.jar into '+JasminDir);
        end;
      end
      else
      begin
        OperationSucceeded := false;
        writelnlog('DownloadJasmin: ERROR: unzip returned result code: ' + IntToStr(ResultCode));
      end;
    end
    else
    begin
      writelnlog('ERROR downloading Jasmin assembler from ' + SourceURL, true);
    end;

    if OperationSucceeded then
    begin
      WritelnLog('Jasmin assembler download and unpacking ok.', true);
      if OperationSucceeded then
      begin
        //Get rid of temp zip and dir if success.
        SysUtils.Deletefile(JasminZip);
        DeleteDirectoryEx(IncludeTrailingPathDelimiter(SysUtils.GetTempDir)+IncludeTrailingPathDelimiter(fpcuputil.ExtractFileNameOnly(GetFileNameFromURL(SourceURL))));
      end;
    end;
    Result := OperationSucceeded;
  end else Result:=True;
end;

procedure TInstaller.DumpOutput(Sender: TProcessEx; output: string);
begin
  if FVerbose then
  begin
    // Set up initial output
    if Assigned(FLogVerbose)=false then
    begin
      FLogVerbose:=TLogger.Create;
      FLogVerbose.LogFile:=SysUtils.GetTempFileName;
      WritelnLog('Verbose output saved to ' + FLogVerbose.LogFile, false);
    end;
    FLogVerbose.WriteLog(Output,false);
  end;
  DumpConsole(Sender, output);
end;

{$IFDEF MSWINDOWS}
function TInstaller.FindSVNSubDirs: boolean;
var
  SVNFiles: TStringList;
  OperationSucceeded: boolean;
begin
  SVNFiles := FindAllFiles(FSVNDirectory, FSVNClient.RepoExecutableName + GetExeExt, true);
  try
    if SVNFiles.Count > 0 then
    begin
      // Just get first result.
      FSVNClient.RepoExecutable := SVNFiles.Strings[0];
      OperationSucceeded := true;
    end
    else
    begin
      infoln('Could not find svn executable in or under ' + FSVNDirectory,etwarning);
      OperationSucceeded := false;
    end;
  finally
    SVNFiles.Free;
  end;
  Result := OperationSucceeded;
end;
{
// experiments
function TInstaller.FindSVNSubDirs: boolean;
procedure FileSearch(const dirName:string);
var
  searchResult: TSearchRec;
begin
  result:='';
  WritelnLog('Going to search for SVN client in ' + IncludeTrailingBackSlash(dirName)+'*');
  if FindFirst(IncludeTrailingBackSlash(dirName)+'*', faAnyFile, searchResult)=0 then
  begin
    try
      repeat
        if (searchResult.Attr and faDirectory)=0 then
        begin
          if SameText(searchResult.Name, FSVNClient.RepoExecutableName + GetExeExt) then
          begin
            FSVNClient.RepoExecutable:=IncludeTrailingBackSlash(dirName)+searchResult.Name;
          end;
        end else if (searchResult.Name<>'.') and (searchResult.Name<>'..') then
        begin
          FileSearch(IncludeTrailingBackSlash(dirName)+searchResult.Name);
        end;
      until ( (FindNext(searchResult)<>0) OR (Length(FSVNClient.RepoExecutable)<>0) );
    finally
      FindClose(searchResult);
    end;
  end;
end;
begin
  FSVNClient.RepoExecutable := '';
  FSVNClient.RepoExecutable := FileSearch(FSVNDirectory);
  WritelnLog('SVN search finished. Found: ' + FSVNClient.RepoExecutable);
  result:=Length(FSVNClient.RepoExecutable)>0;
  if result
     then WritelnLog('SVN search finished. Found: ' + FSVNClient.RepoExecutable)
     else WritelnLog('SVN search failed');
end;
}
{$ENDIF}

function TInstaller.GetFPCTarget(Native: boolean): string;
var
  processorname, os: string;
begin
  processorname := 'notfound';
  os := processorname;
  os := {$I %FPCTARGETOS%};
  processorname := {$I %FPCTARGETCPU%};
  os := LowerCase(os); //match usage in makefiles
  processorname := LowerCase(processorname); //match usage in makefiles

  if not Native then
  begin
    if FCrossCPU_Target <> '' then
      processorname := FCrossCPU_Target;
    if FCrossOS_Target <> '' then
      os := FCrossOS_Target;
  end;
  Result := processorname + '-' + os;
end;

function TInstaller.GetPath: string;
begin
  result:=ProcessEx.Environment.GetVar(PATHVARNAME);
end;

procedure TInstaller.LogError(Sender: TProcessEx; IsException: boolean);
var
  TempFileName: string;
begin
  TempFileName := SysUtils.GetTempFileName;
  if IsException then
  begin
    WritelnLog('Exception raised running ' + Sender.ResultingCommand, true);
    WritelnLog(Sender.ExceptionInfo, true);
  end
  else
  begin
    writelnlog('ERROR running ' + Sender.ResultingCommand, true);
    writelnlog('Command returned non-zero ExitStatus: ' + IntToStr(Sender.ExitStatus), true);
    writelnlog('Command path set to: ' + Sender.Environment.GetVar(PATHVARNAME), true);
    writelnlog('Command current directory: ' + Sender.CurrentDirectory, true);
    writelnlog('Command output:', true);
    // Dump command output to screen and detailed log
    infoln(Sender.OutputString,etDebug);
    try
      Sender.OutputStrings.SaveToFile(TempFileName);
    except
      on E: Exception do
      begin
        // Preferably continue if we can but do inform user of problems
        infoln('LogError: Error writing verbose output to '+TempFileName+': '+E.Message,etError);
      end;
    end;
    WritelnLog('  output logged in ' + TempFileName, false);
  end;
end;

procedure TInstaller.SetPath(NewPath: string; Prepend: boolean; Append: boolean);
var
  OldPath: string;
  ResultingPath: string;
begin
  OldPath := ProcessEx.Environment.GetVar(PATHVARNAME);
  if Prepend and (OldPath<>'') then
    ResultingPath := NewPath + PathSeparator + OldPath
  else if Append and (OldPath<>'') then
    ResultingPath := OldPath + PathSeparator + NewPath
  else
    ResultingPath := NewPath;

  ProcessEx.Environment.SetVar(PATHVARNAME, ResultingPath);
  if ResultingPath <> EmptyStr then
  begin
    WritelnLog('External program path:  ' + ResultingPath, false);
  end;
  if FVerbose then
    infoln('Set path to: ' + ResultingPath,etdebug);
end;

procedure TInstaller.ProcessError(Sender: TProcessEx; IsException: boolean);
begin
  // Add exception info generated from processex
  FErrorLog.AddStrings(Sender.ExceptionInfoStrings);
end;

procedure TInstaller.WritelnLog(msg: string; ToConsole: boolean);
begin
  if Assigned(FLog) then
  begin
    FLog.WriteLog(msg,ToConsole);
  end;
end;


function TInstaller.GetCompilerInDir(Dir: string): string;
var
  ExeName: string;
begin
  ExeName := 'fpc' + GetExeExt;
  Result := IncludeTrailingBackslash(Dir) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator + ExeName;
  {$IFDEF UNIX}
  if FileExistsUTF8(Result + '.sh') then
  begin
    //Use our proxy if it is installed
    Result := Result + '.sh';
  end;
  {$ENDIF UNIX}
end;

constructor TInstaller.Create;
begin
  inherited Create;
  ProcessEx := TProcessEx.Create(nil);
  ProcessEx.OnErrorM := @LogError;
  FCPUCount := GetLogicalCpuCount;

  FGitClient := TGitClient.Create;
  FHGClient := THGClient.Create;
  FSVNClient := TSVNClient.Create;

  // List of binutils that can be downloaded:
  // CreateBinutilsList;
  FNeededExecutablesChecked:=false;
  // Set up verbose log: will be done in dumpoutput
  // as it depends on verbosity etc
  //FLogVerbose: TLogger.Create;
  FErrorLog := TStringList.Create;
  ProcessEx.OnErrorM:=@(ProcessError);
end;

destructor TInstaller.Destroy;
begin
  if Assigned(FLogVerbose) then
    FLogVerbose.Free;
  if Assigned(FErrorLog) then
    FErrorLog.Free;
  if Assigned(ProcessEx.OnErrorM) then
    ProcessEx.OnErrorM:=nil;
  if Assigned(ProcessEx.OnError) then
    ProcessEx.OnError:=nil;
  ProcessEx.Free;
  FGitClient.Free;
  FHGClient.Free;
  FSVNClient.Free;
  inherited Destroy;
end;


end.

