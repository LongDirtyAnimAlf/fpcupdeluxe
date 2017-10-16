unit installerCore;
{
    Core fpc(laz)up(deluxe) installer code

    Copyright (C) 2012-2014 Ludo Brands, Reinier Olislagers
    Copyright (C) 2015-2017 Alfred Glänzer

    This file is part of fpc(laz)up(deluxe).

    Fpc(laz)up(deluxe) is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Fpc(laz)up(deluxe) is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with fpc(laz)up(deluxe).  If not, see <http://www.gnu.org/licenses/>
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  repoclient, GitClient, HGClient, SvnClient,
  processutils, m_crossinstaller, fpcuputil, cpucount;

const
  NEWPASCALGITREPO='https://github.com/newpascal';
  FPCUPGITREPO=NEWPASCALGITREPO+'/fpcupdeluxe';

  {$ifdef MSWINDOWS}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/wincrossbins_v1.0';
  {$endif}
  {$ifdef Linux}
  {$ifdef CPUX86}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/linuxi386crossbins_v1.0';
  {$endif CPUX86}
  {$ifdef CPUX64}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/linuxx64crossbins_v1.0';
  {$endif CPUX64}
  {$ifdef CPUARM}
  FPCUPBINSURL='';
  {$endif CPUARM}
  {$ifdef CPUAARCH64}
  FPCUPBINSURL='';
  {$endif CPUAARCH64}
  {$endif}
  {$ifdef FreeBSD}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/freebsdx64crossbins_v1.0';
  {$endif}
  {$ifdef OpenBSD}
  FPCUPBINSURL='';
  {$endif}
  {$ifdef Darwin}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/darwinx64crossbins_v1.0';
  {$endif}
  {$ifdef Haiku}
  FPCUPBINSURL='';
  {$endif}

  FPCUPLIBSURL=FPCUPGITREPO+'/releases/download/crosslibs_v1.0';

  FPCTRUNKVERSION  = '3.1.1';
  LAZARUSTRUNKVERSION  = '1.9';

  FPCSVNURL = 'https://svn.freepascal.org/svn';
  FPCFTPURL = 'ftp://ftp.freepascal.org/pub/fpc/dist';

  {$IFDEF MSWINDOWS}
  BINUTILSURL = FPCSVNURL + '/fpcbuild';
  //FPC prebuilt binaries of the GNU Binutils
  PREBUILTBINUTILSURL = BINUTILSURL + '/binaries/i386-win32';
  {$ENDIF}

  CHM_URL_LATEST_SVN = FPCSVNURL + '/lazarus/binaries/docs/chm';

  DEFAULTFPCVERSION = '3.0.2';
  DEFAULTLAZARUSVERSION = '1.6.4';

  {$IFDEF DEBUG}
  STANDARDCOMPILEROPTIONS='-vewh';
  //STANDARDCOMPILEROPTIONS='-va';
  {$ELSE}
  //STANDARDCOMPILEROPTIONS='-vw-n-h-i-l-d-u-t-p-c-x-';
  STANDARDCOMPILEROPTIONS='-vw-n-h-l-d-u-t-p-c-';
  {$ENDIF}

  NASMURL='http://www.nasm.us/pub/nasm/releasebuilds/2.13.01';

  SnipMagicBegin='# begin fpcup do not remove '; //look for this/add this in fpc.cfg cross-compile snippet. Note: normally followed by FPC CPU-os code
  SnipMagicEnd='# end fpcup do not remove'; //denotes end of fpc.cfg cross-compile snippet

type
  TUtilCategory = (ucBinutil {regular binutils like as.exe},
    ucDebugger32 {Debugger (support) files 32bit},
    ucDebugger64 {Debugger (support) files 64bit},
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
    FBaseDirectory: string; //Base directory for fpc(laz)up(deluxe) itself
    FSourceDirectory: string; //Top source directory for a product (FPC, Lazarus)
    FInstallDirectory: string; //Top install directory for a product (FPC, Lazarus)
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
    FActualRevision: string;
    FDesiredBranch: string;
    // Stores tprocessex exception info:
    FErrorLog: TStringList;
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
    FGitClient: TGitClient;
    FHGClient: THGClient;
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
    FUseWget: boolean;
    FTar: string;
    FBunzip2: string;
    F7zip: string;
    FUnrar: string;
    FGit: string;
    FProcessEx: TProcessEx;
    FSwitchURL: boolean;
    property Make: string read GetMake;
    // Check for existence of required executables; if not there, get them if possible
    function CheckAndGetTools: boolean;
    // Check for existence of required binutils; if not there, get them if possible
    function CheckAndGetNeededBinUtils: boolean;
    // Make a list (in FUtilFiles) of all binutils that can be downloaded
    procedure CreateBinutilsList(aVersion:string='');
    // Get a diff of all modified files in and below the directory and save it
    procedure CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
    // Download make.exe, patch.exe etc into the make directory (only implemented for Windows):
    function DownloadBinUtils: boolean;
    // Clone/update using HG; use FSourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromHG(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Clone/update using Git; use FSourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromGit(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Checkout/update using SVN; use FSourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromSVN(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
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
    function GetFile(aURL,aFile:string; forceoverwrite:boolean=false):boolean;
  public
    InfoText: string;
    LocalInfoText: string;
    property SVNClient: TSVNClient read FSVNClient;
    // Get processor for termination of running processes
    property Processor: TProcessEx read FProcessEx;
    // Get processerrors and put them into FErrorLog
    procedure ProcessError(Sender:TProcessEx;IsException:boolean);
    // Source directory for installation (fpcdir, lazdir,... option)
    property SourceDirectory: string write FSourceDirectory;
    //Base directory for fpc(laz)up(deluxe) itself
    property BaseDirectory: string write FBaseDirectory;
    // Source directory for installation (fpcdir, lazdir,... option)
    property InstallDirectory: string write FInstallDirectory;
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
    property ActualRevision: string read FActualRevision;
    property DesiredBranch: string write FDesiredBranch;
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
    // auto switchover SVN URL
    property SwitchURL: boolean write FSwitchURL;
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
    // use wget as downloader ??
    property UseWget: boolean write FUseWget;
    // append line ending and write to log and, if specified, to console
    procedure WritelnLog(msg: string; ToConsole: boolean = true);overload;
    procedure WritelnLog(EventType: TEventType; msg: string; ToConsole: boolean = true);overload;
    // Build module
    function BuildModule(ModuleName: string): boolean; virtual;
    // Clean up environment
    function CleanModule(ModuleName: string): boolean; virtual;
    // Config module
    function ConfigModule(ModuleName: string): boolean; virtual;
    // Constructs compiler path from directory and architecture
    // Corrects for use of our fpc.sh launcher on *nix
    // Does not verify compiler actually exists.
    function GetCompilerInDir(Dir: string): string;
    // Install update sources
    function GetModule(ModuleName: string): boolean; virtual;
    // Perform some checks on the sources
    function CheckModule(ModuleName: string): boolean; virtual;
    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; virtual;
    constructor Create;
    destructor Destroy; override;
  end;

  TFPCNativeInstaller = class(TInstaller);
  TFPCInstaller = class(TInstaller);

implementation

uses
  FileUtil, LazFileUtils, LazUTF8
  {$ifndef Haiku}
  ,ssl_openssl
  // for runtime init of openssl
  {$IFDEF MSWINDOWS}
  ,blcksock, ssl_openssl_lib
  {$ENDIF}
  {$ENDIF}
  ;

{ TInstaller }


function TInstaller.GetCompiler: string;
begin
  if (Self is TFPCNativeInstaller) or (Self is TFPCInstaller) then
    Result := GetCompilerInDir(FInstallDirectory)
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
    //FMake := FindDefaultExecutablePath('gmake');
    {$else}
    // Linux, OSX
    FMake := 'make'; //assume in path
    //FMake := FindDefaultExecutablePath('make');
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

function TInstaller.CheckAndGetTools: boolean;
var
  AllThere: boolean;
  OperationSucceeded: boolean;
  Output: string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (CheckAndGetTools): ';

  OperationSucceeded := true;
  if not FNeededExecutablesChecked then
  begin
    // The extractors used depend on the bootstrap compiler URL/file we download
    // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)

    {$IFDEF MSWINDOWS}
    // Need to do it here so we can pick up make path.
    FBunzip2 := '';
    FTar := '';
    FUnrar := '';
    F7zip := '';
    FGit := '';
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FBunzip2 := 'bunzip2';
    FTar := 'tar';
    F7zip := '7za';
    FUnrar := 'unrar';
    FGit := 'git';
    {$ENDIF LINUX}
    {$IFDEF BSD} //OSX, *BSD
    {$IFDEF DARWIN}
    FBunzip2 := ''; //not really necessary now
    FTar := 'bsdtar'; //gnutar is not available by default on Mavericks
    F7zip := '7za';
    FUnrar := 'unrar';
    FGit := 'git';
    {$ELSE} //FreeBSD, OpenBSD, NetBSD
    FBunzip2 := 'bunzip2';
    FTar := 'tar'; //At least FreeBSD tar apparently takes some gnu tar options nowadays.
    F7zip := '7za';
    FUnrar := 'unrar';
    FGit := 'git';
    {$ENDIF DARWIN}
    {$ENDIF BSD}

    {$IFDEF MSWINDOWS}
    ForceDirectoriesUTF8(FMakeDir);

    (*
    // check if we have make ... otherwise get it from standard URL
    GetFile(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+
            '/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/'+ExtractFileName(Make),Make);
    *)

    {$ifdef win64}
    // the standard make by FPC does not work when Git is present (and in the path), but this one works ??!!
    // (but the FPC installer sets its own path to isolate itself from the system, so FPC make still works)
    // strange, but do not enable (yet) !!
    // Download('ftp://ftp.equation.com/make/'+{$ifdef win64}'64'{$else}'32'{$endif}+'/'+ExtractFileName(Make), Make);
    {$endif}

    if OperationSucceeded then
    begin
      // check availability of OpenSSL libraries. Just continue in case of error
      if FileExists(SafeGetApplicationPath+'libeay32.dll') AND FileExists(SafeGetApplicationPath+'ssleay32.dll') then
      begin
        infoln(localinfotext+'Found OpenSLL library files.',etDebug);
      end
      else
      begin
        infoln(localinfotext+'No OpenSLL library files available. Going to download them',etWarning);
        DownloadOpenSSL;
      end;
    end;

    // Get patch binary from default binutils URL
    GetFile(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw32/patch.exe',IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe');
    GetFile(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw32/patch.exe.manifest',IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe.manifest');

    F7zip:=Which('7z');
    if Not FileExists(F7zip) then Which('7za');
    if Not FileExists(F7zip) then F7zip := IncludeTrailingPathDelimiter(FMakeDir) + '\7Zip\7za.exe';
    if Not FileExists(F7zip) then
    begin
      ForceDirectoriesUTF8(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip');
      // this version of 7Zip is the last version that does not need installation ... so we can silently get it !!
      Output:='7za920.zip';
      OperationSucceeded:=GetFile('http://downloads.sourceforge.net/project/sevenzip/7-Zip/9.20/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
      if OperationSucceeded then
      begin
        // sometimes, souceforge has a redirect error, returning a successfull download, but without the datafile itself
        if (FileSize(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output)<50000) then
        begin
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
          OperationSucceeded:=false;
        end;
      end;
      //OperationSucceeded:=GetFile('https://freefr.dl.sourceforge.net/project/sevenzip/7-Zip/9.20/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
      if NOT OperationSucceeded then
      begin
        // try one more time
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
        OperationSucceeded:=GetFile('http://downloads.sourceforge.net/project/sevenzip/7-Zip/9.20/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
        // sometimes, souceforge has a redirect error, returning a successfull download, but without the datafile itself
        if (FileSize(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output)<50000) then
        begin
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
          OperationSucceeded:=false;
        end;
      end;
      if NOT OperationSucceeded then
      begin
        // try one more time on different URL
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
        OperationSucceeded:=GetFile('http://7-zip.org/a/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
      end;

      if OperationSucceeded then
      begin
        with TNormalUnzipper.Create do
        begin
          try
            OperationSucceeded:=DoUnZip(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\',['7za.exe']);
          finally
            Free;
          end;
        end;
        if OperationSucceeded then
        begin
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
          OperationSucceeded:=FileExists(F7zip);
        end;
      end;
      // do not fail ... perhaps there is another 7zip available in the path
      OperationSucceeded:=True;
    end;

    FUnrar := IncludeTrailingPathDelimiter(FMakeDir) + 'unrar\bin\unrar.exe';
    if Not FileExists(FUnrar) then
    begin
      ForceDirectoriesUTF8(IncludeTrailingPathDelimiter(FMakeDir)+'unrar');
      //ForceDirectoriesUTF8(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\bin');
      // this version of unrar does not need installation ... so we can silently get it !!
      Output:='unrar-3.4.3-bin.zip';
      OperationSucceeded:=GetFile('http://downloads.sourceforge.net/project/gnuwin32/unrar/3.4.3/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
      // sometimes, souceforge has a redirect error, returning a successfull download, but without the datafile itself
      if (FileSize(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output)<50000) then
      begin
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
        OperationSucceeded:=false;
      end;
      if NOT OperationSucceeded then
      begin
        // try one more time
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
        OperationSucceeded:=GetFile('http://downloads.sourceforge.net/project/gnuwin32/unrar/3.4.3/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
        // sometimes, souceforge has a redirect error, returning a successfull download, but without the datafile itself
        if (FileSize(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output)<50000) then
        begin
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
          OperationSucceeded:=false;
        end;
      end;
      if OperationSucceeded then
      begin
        with TNormalUnzipper.Create do
        begin
          try
            //OperationSucceeded:=DoUnZip(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'unrar\',['bin\unrar.exe','\bin\unrar3.dll']);
            OperationSucceeded:=DoUnZip(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'unrar\',[]);
          finally
            Free;
          end;
        end;

        if OperationSucceeded then
        begin
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
          OperationSucceeded:=FileExists(FUnrar);
        end;
      end;
      // do not fail ... perhaps there is another unrar available in the path ... or use 7zip on windows
      OperationSucceeded:=True;
    end;

    if Assigned(FGitClient)
       then FGit:=FGitClient.RepoExecutableName
       else FGit:=Which('git');
    if Not FileExists(FGit) then FGit := IncludeTrailingPathDelimiter(FMakeDir) + 'git\bin\git.exe';
    if Not FileExists(FGit) then
    begin
      //Source:
      //https://github.com/git-for-windows/git/releases/download/v2.13.2.windows.1/Git-2.13.2-32-bit.exe
      ForceDirectoriesUTF8(IncludeTrailingPathDelimiter(FMakeDir)+'git');
      {$ifdef win32}
      Output:='git32.7z';
      {$else}
      Output:='git64.7z';
      {$endif}
      infoln(localinfotext+'GIT not found. Download it (may take time) from '+FPCUPGITREPO+'/releases/download/Git-2.13.2',etInfo);
      OperationSucceeded:=GetFile(FPCUPGITREPO+'/releases/download/Git-2.13.2/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
      if NOT OperationSucceeded then
      begin
        // try one more time
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
        OperationSucceeded:=GetFile(FPCUPGITREPO+'/releases/download/Git-2.13.2/'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
      end;
      if OperationSucceeded then
      begin
        infoln(localinfotext+'GIT download ready: unpacking (may take time).',etInfo);
        OperationSucceeded:=(ExecuteCommand(F7zip+' x -o"'+IncludeTrailingPathDelimiter(FMakeDir)+'git\'+'" '+IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output,FVerbose)=0);
        if NOT OperationSucceeded then
        begin
          OperationSucceeded:=(ExecuteCommand('7z'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(FMakeDir)+'git\'+'" '+IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output,FVerbose)=0);
        end;
        if NOT OperationSucceeded then
        begin
          OperationSucceeded:=(ExecuteCommand('7za'+GetExeExt+' x -o"'+IncludeTrailingPathDelimiter(FMakeDir)+'git\'+'" '+IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output,FVerbose)=0);
        end;
        if OperationSucceeded then
        begin
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
          OperationSucceeded:=FileExists(FGit);
        end;
      end;
      // do not fail ... perhaps there is another git available in the path
      OperationSucceeded:=True;
    end;
    if FileExists(FGit) AND Assigned(FGitClient) then FGitClient.RepoExecutable:=FGit;
    {$ENDIF}

    {$IF defined(LINUX) or (defined(BSD) and (not defined(DARWIN)))} //Linux,FreeBSD,NetBSD,OpenBSD, but not OSX
    //todo: check if we need as on OSX as well
    if OperationSucceeded then
    begin
      // Check for proper assembler
      try
        if ExecuteCommand('as --version', Output, False) <> 0 then
        begin
          ExecuteCommand('as --version', Output, True);
          infoln(localinfotext+'Missing assembler as. Please install the developer tools.',etError);
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
        AllThere:=Length(FSVNClient.RepoExecutable)<>0;
      end;

      {$IFDEF MSWINDOWS}
      if NOT AllThere then
      begin
        infoln(localinfotext+'Going to download SVN',etInfo);
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
        infoln(localinfotext+'Could not find SVN executable. Please make sure it is installed.',etError);
      end else infoln(localinfotext+'SVN client found: ' + FSVNClient.RepoExecutable+'.',etDebug);
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

    if OperationSucceeded then
    begin
      // Check for valid tar executable, if it is needed
      if FGit <> EmptyStr then
      begin
        // check exe, but do not fail: GIT is not always needed
        CheckExecutable(FGit, '--version', '');
      end;
    end;

    {$IFNDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      OperationSucceeded := CheckExecutable(Make, '-v', '');
      // expand make path ... not needed
      //if OperationSucceeded then FMake:=FindDefaultExecutablePath(Make);
      {
      try
        ExecuteCommand(Make + ' -v', Output, true);
        if Ansipos('GNU Make', Output) = 0 then
        begin
          infoln(localinfotext+'Found make executable, but it is not GNU Make.',etError);
          OperationSucceeded := false;
        end else OperationSucceeded := true;
      except
        // ignore errors, this is only an extra check
      end;
      }
    end;
    {$ENDIF}

    FNeededExecutablesChecked:=OperationSucceeded;
  end;
  Result := OperationSucceeded;
end;

function TInstaller.CheckAndGetNeededBinUtils: boolean;
var
  {$IFDEF MSWINDOWS}
  AllThere: boolean;
  i: integer;
  {$ENDIF MSWINDOWS}
  OperationSucceeded: boolean;
  Output: string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBinUtils): ';

  OperationSucceeded := true;

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Download if needed, including unzip - needed for SVN download
    // Check for binutils directory
    AllThere:=true;
    if DirectoryExists(FMakeDir) = false then
    begin
      infoln(localinfotext+'Make path ' + FMakeDir + ' does not exist. Going to download binutils.',etInfo);
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
            break;
          end;
        end;
      end;
    end;
    if not(AllThere) then
    begin
      infoln(localinfotext+'Make path [' + FMakeDir + '] does not have (all) binutils. Going to download needed binutils.',etInfo);
      //infoln(localinfotext+'Some binutils missing: going to get them.',etInfo);
      OperationSucceeded := DownloadBinUtils;
    end;
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded then
  begin

    {$IFDEF MSWINDOWS}
    // check if we have make ... otherwise get it from standard URL
    GetFile(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+
            '/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/'+ExtractFileName(Make),Make);
    {$ENDIF MSWINDOWS}

    // Check for proper make executable
    try
      ExecuteCommand(Make + ' -v', Output, False);
      if Ansipos('GNU Make', Output) = 0 then
      begin
        ExecuteCommand(Make + ' -v', Output, True);
        infoln(localinfotext+'Found make executable, but it is not GNU Make.',etError);
        OperationSucceeded := false;
      end;
    except
      // ignore errors, this is only an extra check
    end;
  end;

  Result := OperationSucceeded;
end;

procedure TInstaller.CreateBinutilsList(aVersion:string);
// Windows-centric
const
  SourceURL_gdb = FPCSVNURL+'/lazarus/binaries/i386-win32/gdb/bin/';
  //SourceURL_gdb = 'https://github.com/newpascal/fpcupdeluxe/releases/download/gdb-7.11.1/GDB-i386-win32.zip';
  SourceURL64_gdb = FPCSVNURL+'/lazarus/binaries/x86_64-win64/gdb/bin/';
  //SourceURL64_gdb = 'https://github.com/newpascal/fpcupdeluxe/releases/download/gdb-7.11.1/GDB-x86_64-win64.zip';
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
  aMajor,aMinor,aBuild : Integer;
begin
  SetLength(FUtilFiles,0); //clean out any cruft

  {$IFDEF MSWINDOWS}

  // default
  if aVersion='' then aVersion:=DEFAULTFPCVERSION;

  GetWin32Version(aMajor,aMinor,aBuild);
  // if Win7 or higher: use modern (2.4.0 and higher) binutils
  if aMajor>5 then
  begin
    if (GetNumericalVersion(aVersion)<(2*10000+4*100+0)) then
       aVersion:='2.4.0';
  end;

  //trunk is special
  if aVersion=FPCTRUNKVERSION
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
  AddNewUtil('patch' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('patch' + GetExeExt + '.manifest',aSourceURL,'',ucBinutil);
  AddNewUtil('unzip' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('windres' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('windres.h',aSourceURL,'',ucBinutil);
  AddNewUtil('zip' + GetExeExt,aSourceURL,'',ucBinutil);

  // add win32/64 gdb from lazarus
  AddNewUtil('gdb' + GetExeExt,SourceURL_gdb,'',ucDebugger32);
  AddNewUtil('gdb' + GetExeExt,SourceURL64_gdb,'',ucDebugger64);
  AddNewUtil('libiconv-2.dll',SourceURL64_gdb,'',ucDebugger64);


  {$ifdef win32}
  AddNewUtil('ar' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('as' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('cmp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('cp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('diff' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('gdate' + GetExeExt,aSourceURL,'',ucBinutil);
  //AddNewUtil('gdb' + GetExeExt,aSourceURL_gdb,'',ucDebugger);
  // just add default 32 bit debugger for all usercases as a binutil !
  AddNewUtil('gdb' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('libexpat-1.dll',aSourceURL,'',ucBinutil);
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
  // just add default 64 bit debugger for all usercases as a binutil !
  AddNewUtil('gdb' + GetExeExt,SourceURL64_gdb,'',ucBinutil);
  AddNewUtil('libiconv-2.dll',SourceURL64_gdb,'',ucBinutil);
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
  //AddNewUtil('Qt4Pas5.dll',SourceURL64_Qt,'',ucQtFile);
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
  DownloadSuccess:boolean;
  InstallPath:string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBinUtils): ';
  //Parent directory of files. Needs trailing backslash.
  ForceDirectoriesUTF8(FMakeDir);
  Result := true;
  for Counter := low(FUtilFiles) to high(FUtilFiles) do
  begin
    if (FUtilFiles[Counter].Category=ucBinutil) or (FUtilFiles[Counter].Category=ucDebugger32) or (FUtilFiles[Counter].Category=ucDebugger64) then
    begin
      InstallPath:=IncludeTrailingPathDelimiter(FMakeDir);

      if (FUtilFiles[Counter].Category=ucDebugger32) or (FUtilFiles[Counter].Category=ucDebugger64) then
      begin
        if (FUtilFiles[Counter].Category=ucDebugger32) then InstallPath:=InstallPath+'gdb\i386-win32\';
        if (FUtilFiles[Counter].Category=ucDebugger64) then InstallPath:=InstallPath+'gdb\x86_64-win64\';
        ForceDirectoriesUTF8(InstallPath);
      end;

      InstallPath:=InstallPath+FUtilFiles[Counter].FileName;

      if (FileExists(InstallPath)) then continue;

      DownloadSuccess:=GetFile(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,InstallPath);

      if NOT DownloadSuccess then
      begin
        infoln(localinfotext+'Error downloading binutil: ' + FUtilFiles[Counter].FileName + ' to ' + ExtractFileDir(InstallPath) + '. Retrying.',etError);
        Errors := Errors + 1;
      end else infoln(localinfotext+'Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + ' success.',etInfo);

    end;

  end;

  if Errors > 0 then
  begin
    Result := false;
    WritelnLog(localinfotext+IntToStr(Errors) + ' error(s) downloading binutils.', true);
  end;
end;

function TInstaller.DownloadFromBase(aClient:TRepoClient; ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList; const aUserName:string=''; const aPassword:string=''): boolean;
var
  BeforeRevisionShort: string; //Basically the branch revision number
  ReturnCode: integer;
  DiffFile: String;
  LocalPatchCmd : string;
  DiffFileSL:TStringList;
  Output: string = '';
begin
  Result := false;

  // check if we do have a client !!
  if NOT aClient.ValidClient then exit;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' ('+Copy(aClient.ClassName,2,MaxInt)+': '+ModuleName+'): ';

  //todo: check if we need to add forcedirectoriesutf8 to create local repo dir if it doesn't exist
  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  aClient.LocalRepository := FSourceDirectory;
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
        DiffFile:=IncludeTrailingPathDelimiter(FSourceDirectory) + 'REV' + BeforeRevisionShort + '.diff';
        CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,aClient);
        UpdateWarnings.Add(ModuleName + ': reverting to original before updating.');
        aClient.Revert; //Remove local changes
      end else UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
    end;
  end;

  aClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  aClient.DesiredBranch := FDesiredBranch; //We want to update to this specific branch

  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  aClient.CheckOutOrUpdate;

  ReturnCode := aClient.ReturnCode;
  case ReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog(etError, localinfotext+'Repository URL in local directory and remote repository don''t match.', true);
      writelnlog(localinfotext+'Local directory: ' + aClient.LocalRepository, true);
      infoln(localinfotext+'Have you specified the wrong directory or a directory with an old repository checkout?',etDebug);
    end;
    else
    begin
      // For now, assume it worked even with non-zero result code. We can because
      // we do the AfterRevision check as well.
      Result := true;

      if FExportOnly then
        AfterRevision := FDesiredRevision
      else
        AfterRevision := aClient.LocalRevision;

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

         ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FSourceDirectory, Output, FVerbose);

         {$IFNDEF MSWINDOWS}
         if ReturnCode<>0 then
         begin
           // Patching can go wrong when line endings are not compatible
           // This happens e.g. with bgracontrols that have CRLF in the source files
           // Try to circumvent this problem by trick below (replacing line enddings)
           if Pos('different line endings',Output)>0 then
           begin
             ReturnCode:=ExecuteCommandInDir('unix2dos '+DiffFile, FSourceDirectory, FVerbose);
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
               //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, FSourceDirectory, FVerbose);
               //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, FSourceDirectory, FVerbose);
             end;
             if ReturnCode=0 then
             begin
               // check for default values
               if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
                  then LocalPatchCmd:=FPatchCmd + ' -p0 --binary -i '
                  else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
               ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FSourceDirectory, Output, FVerbose);
             end;
           end;
         end;
         {$ENDIF}
         // Report error, but continue !
         if ReturnCode<>0 then
         begin
           writelnlog(etError, localinfotext+'Patching with ' + DiffFile + ' failed.', true);
           writelnlog(localinfotext+'Output: ' + Output, true);
           writelnlog(localinfotext+'Verify the state of the source, correct and rebuild with make.', true);
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

function TInstaller.DownloadFromSVN(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
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

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadFromSVN: '+ModuleName+'): ';

  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  FSVNClient.ModuleName:=ModuleName;
  FSVNClient.LocalRepository := FSourceDirectory;
  FSVNClient.Repository := FURL;
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
    writelnlog('Directory ' + FSourceDirectory + ' is not an SVN repository (or a repository with the wrong remote URL).');
    if not(DirectoryExistsUTF8(FSVNClient.LocalRepository)) then
    begin
      writelnlog(localinfotext+'Creating directory '+FSourceDirectory+' for SVN checkout.');
      ForceDirectoriesUTF8(FSourceDirectory);
    end;
  end;

  if (FSVNClient.LocalRevisionWholeRepo = FRET_UNKNOWN_REVISION) and (FSVNClient.Returncode=FRET_WORKING_COPY_TOO_OLD) then
  begin
    writelnlog(etError, localinfotext+'The working copy in ' + FSourceDirectory + ' was created with an older, incompatible version of svn.', true);
    writelnlog(etError, localinfotext+'Run svn upgrade in the directory or make sure the original svn executable is the first in the search path.', true);
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
        DiffFile:=IncludeTrailingPathDelimiter(FSourceDirectory) + 'REV' + BeforeRevisionShort + '.diff';
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
  infoln(localinfotext+'Running SVN checkout or update.',etInfo);

  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  FSVNClient.CheckOutOrUpdate;

  CheckoutOrUpdateReturnCode := FSVNClient.ReturnCode;
  case CheckoutOrUpdateReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog(etError, localinfotext+'Repository URL in local directory and remote repository don''t match.', true);
      writelnlog(localinfotext+'Local directory: ' + FSVNClient.LocalRepository, true);
      infoln(localinfotext+'Have you specified the wrong directory or a directory with an old repository checkout?',etDebug);
    end;
    else
    begin
      // If there are svn errors, return a false result.
      // We used to do a check for the revision, but that does not check the integrity
      // or existence of all files in the svn repo.

      if FExportOnly then AfterRevision := FDesiredRevision else
      begin
        if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
          AfterRevision := FSVNClient.LocalRevisionWholeRepo
        else
          AfterRevision := FSVNClient.LocalRevision;
      end;

      if (FSVNClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (BeforeRevisionShort <> FSVNClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      // Only return success if svn returned return code 0
      Result := (CheckoutOrUpdateReturnCode=0);

      if not Result then
      begin
        writelnlog(localinfotext+'SVN gave error code: '+inttostr(CheckoutOrUpdateReturnCode));
        writelnlog(localinfotext+'SVN gave error message: '+FSVNClient.ReturnOutput);
      end;

      if Result and FReApplyLocalChanges and (DiffFile<>'') then
      begin
        UpdateWarnings.Add(ModuleName + ': reapplying local changes.');

        if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
           then LocalPatchCmd:=FPatchCmd + ' -p0 -i '
           else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
        CheckoutOrUpdateReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FSourceDirectory, Output, FVerbose);

        {$IFNDEF MSWINDOWS}
        if CheckoutOrUpdateReturnCode<>0 then
        begin
          // Patching can go wrong when line endings are not compatible
          // This happens e.g. with bgracontrols that have CRLF in the source files
          // Try to circumvent this problem by trick below (replacing line enddings)
          if Pos('different line endings',Output)>0 then
          begin
            CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('unix2dos '+DiffFile, FSourceDirectory, FVerbose);
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
              //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, FSourceDirectory, FVerbose);
              //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, FSourceDirectory, FVerbose);
            end;
            if CheckoutOrUpdateReturnCode=0 then
            begin
              if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
                 then LocalPatchCmd:=FPatchCmd + ' -p0 --binary -i '
                 else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
              CheckoutOrUpdateReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, FSourceDirectory, Output, FVerbose);
            end;
          end;
        end;
        {$ENDIF}
        // Report error, but continue !
        if CheckoutOrUpdateReturnCode<>0 then
        begin
          writelnlog(etError, localinfotext+'Patching with ' + DiffFile + ' failed.', true);
          writelnlog(localinfotext+'Output: ' + Output, true);
          writelnlog(localinfotext+'Verify the state of the source, correct and rebuild with make.', true);
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
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.9.1.zip';
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.9.4.zip';
  //SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.9.5.zip';
  SourceURL = 'https://www.visualsvn.com/files/Apache-Subversion-1.9.7.zip';
  //SourceURL = 'https://sourceforge.net/projects/win32svn/files/1.8.15/apache24/svn-win32-1.8.15-ap24.zip/download';
  // confirmed by winetricks bug report that this is the only one left...
  // this link seems down 'http://download.microsoft.com/download/vc60pro/update/1/w9xnt4/en-us/vc6redistsetup_enu.exe';
var
  MajorVersion,MinorVersion,BuildNumber: integer;
  OperationSucceeded: boolean;
  SVNZip: string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadSVN): ';

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
    writelnlog(etError, localinfotext + 'It seems this PC is running Windows 2000. Cannot install svn.exe. Please manually install e.g. TortoiseSVN first.', true);
    exit(false);
  end;

  ForceDirectoriesUTF8(FSVNDirectory);

  SVNZip := SysUtils.GetTempFileName + '.zip';
  try
    if OperationSucceeded then
    begin
      OperationSucceeded := Download(
        FUseWget,
        SourceURL,
        SVNZip,
        FHTTPProxyUser,
        FHTTPProxyPort,
        FHTTPProxyUser,
        FHTTPProxyPassword);
    end;
  except
    // Deal with timeouts, wrong URLs etc
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading SVN client from ' + SourceURL, true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    with TNormalUnzipper.Create do
    begin
      try
        DeleteDirectoryEx(FSVNDirectory);
        OperationSucceeded:=DoUnZip(SVNZip,FSVNDirectory,[]);
      finally
        Free;
      end;
    end;
  end
  else
  begin
    writelnlog(etError, localinfotext + 'Downloading SVN client from ' + SourceURL, true);
  end;

  if OperationSucceeded then
  begin
    WritelnLog(localinfotext + 'SVN download and unpacking ok. Not going to search SVN client itself in ' + FSVNDirectory, true);
    OperationSucceeded := FindSVNSubDirs;
    if OperationSucceeded then
      SysUtils.Deletefile(SVNZip); //Get rid of temp zip if success.
  end;
  Result := OperationSucceeded;
end;

function TInstaller.DownloadOpenSSL: boolean;
const
  {$ifdef win64}
  SourceURL = 'http://indy.fulgan.com/SSL/openssl-1.0.2j-x64_86-win64.zip';
  SourceURLfailsafe = 'http://packages.lazarus-ide.org/openssl-1.0.2j-x64_86-win64.zip';
  {$endif}
  {$ifdef win32}
  SourceURL = 'http://indy.fulgan.com/SSL/openssl-1.0.2j-i386-win32.zip';
  SourceURLfailsafe = 'http://packages.lazarus-ide.org/openssl-1.0.2j-i386-win32.zip';
  {$endif}
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  OpenSSLZip: string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadOpenSSL): ';

  OperationSucceeded := false;

  ForceDirectoriesUTF8(SafeGetApplicationPath);

  OpenSSLZip := SysUtils.GetTempFileName + '.zip';

  try
    OperationSucceeded:=GetFile(SourceURL,OpenSSLZip);
    if (NOT OperationSucceeded) then
    begin
      // try one more time
      SysUtils.DeleteFile(OpenSSLZip);
      OperationSucceeded:=GetFile(SourceURL,OpenSSLZip);
      if (NOT OperationSucceeded) then
      begin
        // try one more time on failsafe URL
        SysUtils.DeleteFile(OpenSSLZip);
        OperationSucceeded:=GetFile(SourceURLfailsafe,OpenSSLZip);
      end;
    end;
  except
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading OpenSSL library', true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract
    with TNormalUnzipper.Create do
    begin
      try
        resultcode:=1;
        if DoUnZip(OpenSSLZip,SafeGetApplicationPath,['libeay32.dll','ssleay32.dll']) then resultcode:=0;
      finally
        Free;
      end;
    end;

    if resultcode <> 0 then
    begin
      OperationSucceeded := false;
      writelnlog(etError, localinfotext + 'Download OpenSSL error: unzip returned result code: ' + IntToStr(ResultCode));
    end;
  end;

  if OperationSucceeded then
  begin
    WritelnLog(localinfotext + 'OpenSLL download and unpacking ok.', true);
    SysUtils.Deletefile(OpenSSLZip); //Get rid of temp zip if success.
  end else infoln(localinfotext+'Could not install openssl library', etError);
  Result := OperationSucceeded;

  //SslLibraryInit;
  if InitSSLInterface then
      SSLImplementation := TSSLOpenSSL;
end;

{$ENDIF}

function TInstaller.DownloadJasmin: boolean;
const
  JasminVersion = '2.4';
  SourceURL = 'http://sourceforge.net/projects/jasmin/files/jasmin/jasmin-'+JasminVersion+'/jasmin-'+JasminVersion+'.zip/download';
  SourceURLfailsafe = 'https://github.com/davidar/jasmin/archive/'+JasminVersion+'.zip';
  //SourceURL = 'http://svn.freepascal.org/svn/fpcbuild/branches/fixes_3_0/install/jvm/jasmin.jar';
  //SourceURL = 'http://svn.freepascal.org/svn/fpcbuild/trunk/install/jvm/jasmin.jar';
var
  OperationSucceeded: boolean;
  JasminZip,JasminDir: string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadJasmin): ';

  // for now, just put jasmin.jar in bin directory ... easy and simple and working
  JasminDir:=IncludeTrailingPathDelimiter(FInstallDirectory) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator;

  if NOT FileExists(JasminDir+'jasmin.jar') then
  begin
    OperationSucceeded := false;
    JasminZip := SysUtils.GetTempFileName + '.zip';
    try
      OperationSucceeded:=GetFile(SourceURL,JasminZip);
      if (NOT OperationSucceeded) then
      begin
        // try one more time
        SysUtils.DeleteFile(JasminZip);
        OperationSucceeded:=GetFile(SourceURL,JasminZip);
        if (NOT OperationSucceeded) then
        begin
          // try one more time on failsafe URL
          SysUtils.DeleteFile(JasminZip);
          OperationSucceeded:=GetFile(SourceURLfailsafe,JasminZip);
        end;
      end;
    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading jasmin.jar', true);
      end;
    end;

    if OperationSucceeded then
    begin
      // Extract, overwrite
      with TNormalUnzipper.Create do
      begin
        try
          OperationSucceeded:=DoUnZip(JasminZip,SysUtils.GetTempDir,[]);
        finally
          Free;
        end;
      end;

      if OperationSucceeded then
      begin
        OperationSucceeded := MoveFile(IncludeTrailingPathDelimiter(SysUtils.GetTempDir) + 'jasmin-' + JasminVersion + DirectorySeparator + 'jasmin.jar',JasminDir+'jasmin.jar');
        //MoveFile
        if NOT OperationSucceeded then
        begin
          writelnlog(etError, localinfotext + 'Could not move jasmin.jar into '+JasminDir);
        end;
      end
      else
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'DownloadJasmin error: unzip failed due to unknown error.');
      end;
    end
    else
    begin
      writelnlog(etError, localinfotext + 'Downloading Jasmin assembler from ' + SourceURL + ' failed.', true);
    end;

    if OperationSucceeded then
    begin
      WritelnLog(localinfotext + 'Jasmin assembler download and unpacking ok.', true);
      if OperationSucceeded then
      begin
        //Get rid of temp zip and dir if success.
        SysUtils.Deletefile(JasminZip);
        DeleteDirectoryEx(IncludeTrailingPathDelimiter(SysUtils.GetTempDir) + 'jasmin-' + JasminVersion + DirectorySeparator);
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
      WritelnLog(localinfotext+'Verbose output saved to ' + FLogVerbose.LogFile, false);
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
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (FindSVNSubDirs): ';
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
      infoln(localinfotext+'Could not find svn executable in or under ' + FSVNDirectory,etWarning);
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
  os := GetTargetOS;
  processorname := GetTargetCPU;

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
  result:=Processor.Environment.GetVar(PATHVARNAME);
  //result:=GetEnvironmentVariable(PATHVARNAME);
end;

procedure TInstaller.LogError(Sender: TProcessEx; IsException: boolean);
var
  TempFileName: string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+': ';
  TempFileName := SysUtils.GetTempFileName;
  if IsException then
  begin
    WritelnLog(etError, localinfotext+'Exception raised running ' + Sender.ResultingCommand, true);
    WritelnLog(etError, localinfotext+Sender.ExceptionInfo, true);
  end
  else
  begin
    writelnlog(etError, localinfotext+'Running ' + Sender.ResultingCommand + ' failed.', true);
    writelnlog(etError, localinfotext+'Command returned non-zero ExitStatus: ' + IntToStr(Sender.ExitStatus), true);
    writelnlog(localinfotext+'Command path set to: ' + Sender.Environment.GetVar(PATHVARNAME), true);
    writelnlog(localinfotext+'Command current directory: ' + Sender.CurrentDirectory, true);
    writelnlog(localinfotext+'Command output:', true);
    // Dump command output to screen and detailed log
    infoln(localinfotext+Sender.OutputString,etDebug);
    try
      Sender.OutputStrings.SaveToFile(TempFileName);
    except
      on E: Exception do
      begin
        // Preferably continue if we can but do inform user of problems
        infoln(localinfotext+'Error writing verbose output to '+TempFileName+': '+E.Message,etError);
      end;
    end;
    WritelnLog(localinfotext+'Output logged in ' + TempFileName, false);
  end;
end;

procedure TInstaller.SetPath(NewPath: string; Prepend: boolean; Append: boolean);
var
  OldPath: string;
  ResultingPath: string;
begin
  OldPath := Processor.Environment.GetVar(PATHVARNAME);
  //OldPath := GetEnvironmentVariable(PATHVARNAME);
  if Prepend and (OldPath<>'') then
    ResultingPath := NewPath + PathSeparator + OldPath
  else if Append and (OldPath<>'') then
    ResultingPath := OldPath + PathSeparator + NewPath
  else
    ResultingPath := NewPath;

  Processor.Environment.SetVar(PATHVARNAME, ResultingPath);
  if ResultingPath <> EmptyStr then
  begin
    WritelnLog(Copy(Self.ClassName,2,MaxInt)+' (SetPath): External program path:  ' + ResultingPath, false);
  end;
  if FVerbose then
    infoln(Copy(Self.ClassName,2,MaxInt)+' (SetPath): Set path to: ' + ResultingPath,etDebug);
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

procedure TInstaller.WritelnLog(EventType: TEventType; msg: string; ToConsole: boolean);
begin
  if Assigned(FLog) then
  begin
    FLog.WriteLog(EventType,msg,ToConsole);
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

function TInstaller.BuildModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (BuildModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
end;
function TInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (CleanModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
end;
function TInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (ConfigModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
end;
function TInstaller.GetModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (GetModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
end;

function TInstaller.CheckModule(ModuleName: string): boolean;
var
  aRepoClient:TRepoClient;
  aEvent:TEventType;
begin
  result:=true;

  infotext:=Copy(Self.ClassName,2,MaxInt)+' (CheckModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);

  if NOT DirectoryExists(FSourceDirectory) then exit;
  if FExportOnly then exit;

  if FSwitchURL then
    aEvent:=etWarning
  else
    aEvent:=etError;

  // not so elegant check to see what kind of client we need ...
  aRepoClient:=nil;
  if (Pos(FPCSVNURL,LowerCase(FURL))>0) then aRepoClient:=FSVNClient;
  if aRepoClient=nil then if ( {(Pos('GITHUB',UpperCase(FURL))>0) OR} (Pos('.GIT',UpperCase(FURL))>0) ) then aRepoClient:=FGitClient;
  if aRepoClient=nil then if ( (Pos('hg.code.sf.net',LowerCase(FURL))>0) ) then aRepoClient:=FHGClient;
  if aRepoClient=nil then aRepoClient:=FSVNClient;

  infoln(infotext+'checking ' + ModuleName + ' sources with '+aRepoClient.ClassName,etInfo);

  aRepoClient.Verbose:=FVerbose;
  aRepoClient.ExportOnly:=FExportOnly;
  aRepoClient.ModuleName:=ModuleName;
  aRepoClient.LocalRepository:=FSourceDirectory;
  aRepoClient.Repository:=FURL;

  aRepoClient.LocalRepositoryExists;
  result:=(aRepoClient.ReturnCode<>FRET_LOCAL_REMOTE_URL_NOMATCH);

  if result then
    infoln(infotext+'sources ok.',etInfo)
  else
  begin
    infoln(infotext+'sources error (URL mismatch).',aEvent);
    infoln(infotext+'desired URL='+FURL,aEvent);
    infoln(infotext+'source URL='+aRepoClient.Repository,aEvent);

    if ((FSwitchURL) AND (NOT result)) then
    begin
      result:=true;

      infoln(infotext+'switching source URL',etInfo);

      FSVNClient.Verbose:=FVerbose;
      FSVNClient.ExportOnly:=FExportOnly;
      FSVNClient.ModuleName:=ModuleName;
      FSVNClient.LocalRepository:=FSourceDirectory;
      FSVNClient.Repository:=FURL;

      FSVNClient.SwitchURL;

      result:=true;
    end;
  end;
end;
function TInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (UnInstallModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
end;

constructor TInstaller.Create;
begin
  inherited Create;
  FProcessEx := TProcessEx.Create(nil);
  Processor.OnErrorM := @LogError;
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
  Processor.OnErrorM:=@(ProcessError);
end;

function TInstaller.GetFile(aURL,aFile:string; forceoverwrite:boolean=false):boolean;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (GetFile): ';
  result:=((FileExists(aFile)) AND (NOT forceoverwrite) AND (FileSize(aFile)>0));
  if (NOT result) then
  begin
    if ((forceoverwrite) AND (SysUtils.FileExists(aFile))) then SysUtils.DeleteFile(aFile);
    result:=Download(FUseWget,aURL,aFile,FHTTPProxyHost,FHTTPProxyPort,FHTTPProxyUser,FHTTPProxyPassword);
    if (NOT result) then infoln(localinfotext+'Could not download file with URL ' + aURL +' into ' + ExtractFileDir(aFile) + ' (filename: ' + ExtractFileName(aFile) + ')',etError);
  end;
end;


destructor TInstaller.Destroy;
begin
  if Assigned(FLogVerbose) then
    FLogVerbose.Free;
  if Assigned(FErrorLog) then
    FErrorLog.Free;
  if Assigned(Processor.OnErrorM) then
    Processor.OnErrorM:=nil;
  if Assigned(Processor.OnError) then
    Processor.OnError:=nil;
  FProcessEx.Free;
  FGitClient.Free;
  FHGClient.Free;
  FSVNClient.Free;
  inherited Destroy;
end;


end.

