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
  processutils, m_crossinstaller, fpcuputil;

const
  DEFAULTFPCVERSION     = '3.0.4';
  DEFAULTLAZARUSVERSION = '2.0.6';

  FPCTRUNKVERSION       = '3.3.1';
  FPCTRUNKBOOTVERSION   = '3.0.4';
  LAZARUSTRUNKVERSION   = '2.1.0';

  LAZBUILDNAME          = 'lazbuild';

  MAKEFILENAME          = 'Makefile';
  FPCMAKEFILENAME       = MAKEFILENAME+'.fpc';

  FPCSVNURL = 'https://svn.freepascal.org/svn';
  FPCFTPURL = 'ftp://ftp.freepascal.org/pub/fpc';

  BINUTILSURL = FPCSVNURL + '/fpcbuild';

  {$IFDEF MSWINDOWS}
  //FPC prebuilt binaries of the GNU Binutils
  PREBUILTBINUTILSURL = BINUTILSURL + '/binaries/i386-win32';
  PREBUILTBINUTILSURLWINCE = BINUTILSURL + '/tags/release_3_0_4/install/crossbinwce';
  {$ENDIF}

  LAZARUSBINARIES = FPCSVNURL + '/lazarus/binaries';

  CHM_URL_LATEST_SVN = LAZARUSBINARIES + '/docs/chm';

  {$ifdef win64}
  OPENSSL_URL_LATEST_SVN = LAZARUSBINARIES + '/x86_64-win64/openssl';
  {$endif}
  {$ifdef win32}
  OPENSSL_URL_LATEST_SVN = LAZARUSBINARIES + '/i386-win32/openssl';
  {$endif}

  {$IFDEF DEBUG}
  STANDARDCOMPILERVERBOSITYOPTIONS='-vewh';
  //STANDARDCOMPILERVERBOSITYOPTIONS='-va';
  {$ELSE}
  //STANDARDCOMPILERVERBOSITYOPTIONS='-vw-n-h-i-l-d-u-t-p-c-x-';
  STANDARDCOMPILERVERBOSITYOPTIONS='-vw-n-h-l-d-u-t-p-c-';
  {$ENDIF}

  //NASMWIN32URL='https://www.nasm.us/pub/nasm/releasebuilds/2.13/win32/nasm-2.13-win32.zip';
  //NASMWIN64URL='https://www.nasm.us/pub/nasm/releasebuilds/2.13/win64/nasm-2.13-win64.zip';
  NASMWIN32URL='https://www.nasm.us/pub/nasm/releasebuilds/2.14/win32/nasm-2.14-win32.zip';
  NASMWIN64URL='https://www.nasm.us/pub/nasm/releasebuilds/2.14/win64/nasm-2.14-win64.zip';
  NASMFPCURL=BINUTILSURL + '/trunk/install/crossbinmsdos/nasm.exe';

  {$IF (defined(OpenBSD)) and (defined(CPU64))}
  // 2.6.2 and older do not work anymore on newer OpenBSD64 versions
  FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION=(2*10000+6*100+2);
  {$else}
  // 2.2.4 and older have no official FPC bootstrapper available online
  FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION=(2*10000+2*100+4);
  {$endif}

  {$ifdef win64}
  OpenSSLSourceURL : array [0..4] of string = (
    'https://indy.fulgan.com/SSL/openssl-1.0.2t-x64_86-win64.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2r-x64_86-win64.zip',
    'http://wiki.overbyte.eu/arch/openssl-1.0.2r-win64.zip',
    'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win64.zip',
    'https://indy.fulgan.com/SSL/Archive/openssl-1.0.2p-x64_86-win64.zip'
    );
  {$endif}
  {$ifdef win32}
  OpenSSLSourceURL : array [0..4] of string = (
    'https://indy.fulgan.com/SSL/openssl-1.0.2t-i386-win32.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2r-i386-win32.zip',
    'http://wiki.overbyte.eu/arch/openssl-1.0.2r-win32.zip',
    'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win32.zip',
    'https://indy.fulgan.com/SSL/Archive/openssl-1.0.2p-i386-win32.zip'
    );
  {$endif}

  SnipMagicBegin='# begin fpcup do not remove '; //look for this/add this in fpc.cfg cross-compile snippet. Note: normally followed by FPC CPU-os code
  SnipMagicEnd='# end fpcup do not remove'; //denotes end of fpc.cfg cross-compile snippet

  //Sequence contants for statemachine

  _SEP                     = ';';

  _FPC                     = 'FPC';
  _LAZARUS                 = 'Lazarus';
  _LAZARUSSIMPLE           = 'LazarusSimple';

  _MAKEFILECHECK           = 'MakefileCheck';
  _MAKEFILECHECKFPC        = _MAKEFILECHECK+_FPC;
  _MAKEFILECHECKLAZARUS    = _MAKEFILECHECK+_LAZARUS;

  _DEFAULT                 = 'Default';
  _DEFAULTSIMPLE           = 'DefaultSimple';

  _CLEAN                   = 'Clean';
  _CHECK                   = 'Check';
  _GET                     = 'Get';
  _CONFIG                  = 'Config';
  _BUILD                   = 'Build';
  _UNINSTALL               = 'Uninstall';
  _RESET                   = 'Reset';
  _ONLY                    = 'Only';

  _DO                      = 'Do ';
  _DECLARE                 = 'Declare ';
  _EXECUTE                 = 'Exec ';
  _SETCPU                  = 'SetCPU ';
  _SETOS                   = 'SetOS ';
  _REQUIRES                = 'Requires ';
  _DECLAREHIDDEN           = 'DeclareHidden ';
  _MODULE                  = 'Module ';

  _CLEANMODULE             = _CLEAN+_MODULE;
  _CHECKMODULE             = _CHECK+_MODULE;
  _GETMODULE               = _GET+_MODULE;
  _CONFIGMODULE            = _CONFIG+_MODULE;
  _BUILDMODULE             = _BUILD+_MODULE;
  _UNINSTALLMODULE         = _UNINSTALL+_MODULE;

  _CREATEFPCUPSCRIPT       = 'CreateFpcupScript';
  _CREATELAZARUSSCRIPT     = 'CreateLazarusScript';
  _DELETELAZARUSSCRIPT     = 'DeleteLazarusScript';
  _CHECKDEVLIBS            = 'CheckDevLibs';

  _LAZBUILD                = 'Lazbuild';
  _STARTLAZARUS            = 'StartLazarus';
  _LCL                     = 'LCL';
  _COMPONENTS              = 'Components';
  _PACKAGER                = 'Packager';
  _IDE                     = 'IDE';
  _BIGIDE                  = 'BigIDE';
  _USERIDE                 = 'UserIDE';
  _OLDLAZARUS              = 'OldLazarus';
  _PAS2JS                  = 'Pas2JS';

  _UNIVERSALDEFAULT        = 'Universal'+_DEFAULT;
  _FPCCLEANBUILDONLY       = _FPC+_CLEAN+_BUILD+_ONLY;
  _FPCREMOVEONLY           = _FPC+_CLEAN+_UNINSTALL+_ONLY;
  _LAZARUSCLEANBUILDONLY   = _LAZARUS+_CLEAN+_BUILD+_ONLY;
  _LAZARUSREMOVEONLY       = _LAZARUS+_CLEAN+_UNINSTALL+_ONLY;
  _LCLALLREMOVEONLY        = _LCL+'ALL'+_CLEAN+_ONLY;
  _LCLREMOVEONLY           = _LCL+_CLEAN+_ONLY;
  _COMPONENTSREMOVEONLY    = _COMPONENTS+_CLEAN+_ONLY;
  _PACKAGERREMOVEONLY      = _PACKAGER+_CLEAN+_ONLY;

  _HELP                    = 'Help';
  _HELPFPC                 = _HELP+_FPC;
  _HELPLAZARUS             = _HELP+_LAZARUS;

  {$ifdef mswindows}
  {$ifdef win32}
  _CROSSWIN                = 'CrossWin32-64';
  {$endif}
  {$ifdef win64}
  _CROSSWIN                = 'CrossWin64-32';
  {$endif}
  {$endif}

  _RESETLCL                = 'ResetLCL';
  _LCLCROSS                = 'LCLCross';

  _ENDFINAL                = 'End';
  _END                     = _ENDFINAL+_SEP;


type
  TCPU = (i386,x86_64,arm,aarch64,powerpc,powerpc64,mips,mipsel,avr,jvm,i8086,sparc);
  TOS  = (windows,linux,android,darwin,freebsd,openbsd,aix,wince,iphonesim,embedded,java,msdos,haiku,solaris,dragonfly,netbsd);
  TARMARCH  = (default,armel,armeb,armhf);

  TCPUOS = record
    CPU:TCPU;
    OS:TOS;
  end;

  //TTargetSet=array[tcpu,tos] of boolean;

const
  CpuStr : array[TCPU] of string=(
    'i386','x86_64','arm','aarch64','powerpc','powerpc64', 'mips', 'mipsel','avr','jvm','i8086','sparc'
  );

  ppcSuffix : array[TCPU] of string=(
    '386','x64','arm','a64','ppc','ppc64', 'mips', 'mipsel','avr','jvm','8086','sparc'
  );

  OSStr : array[TOS] of string=(
    'windows'{,'win32','win64'},'linux', 'android','darwin','freebsd','openbsd','aix','wince','iphonesim','embedded','java', 'msdos','haiku','solaris','dragonfly','netbsd'
  );

  ARMArchFPCStr : array[TARMARCH] of string=(
    '','-dFPC_ARMEL','-dFPC_ARMEB','-dFPC_ARMHF'
  );

type
  TUtilCategory = (ucBinutil {regular binutils like as.exe},
    ucDebugger32 {Debugger (support) files 32bit},
    ucDebugger64 {Debugger (support) files 64bit},
    ucDebuggerWince {Debugger (support) files for wince},
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
    FCrossCPU_Target: string; //When cross-compiling: CPU, e.g. x86_64
    FCrossOS_Target: string; //When cross-compiling: OS, e.g. win64
    FCrossOS_SubArch: string; //When cross-compiling for embedded: CPU, e.g. for Teensy SUBARCH=ARMV7EM
    procedure SetURL(value:string);
    procedure SetSourceDirectory(value:string);
    function GetShell: string;
    function GetMake: string;
    procedure SetHTTPProxyHost(AValue: string);
    procedure SetHTTPProxyPassword(AValue: string);
    procedure SetHTTPProxyPort(AValue: integer);
    procedure SetHTTPProxyUser(AValue: string);
    function DownloadFromBase(aClient:TRepoClient; ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList; const aUserName:string=''; const aPassword:string=''): boolean;
    // Get fpcup registred cross-compiler, if any, if not, return nil
    function GetCrossInstaller: TCrossInstaller;
    function GetFullVersion:dword;
    function GetDefaultCompilerFilename(const TargetCPU: string; Cross: boolean): string;
  protected
    FCleanModuleSuccess: boolean;
    FBaseDirectory: string; //Base directory for fpc(laz)up(deluxe) install itself
    FSourceDirectory: string; //Top source directory for a product (FPC, Lazarus)
    FInstallDirectory: string; //Top install directory for a product (FPC, Lazarus)
    FCompiler: string; // Compiler executable
    FCompilerOptions: string; //options passed when compiling (FPC or Lazarus currently)
    FCPUCount: integer; //logical cpu count (i.e. hyperthreading=2cpus)
    FCrossOPT: string; //options passed (only) when cross-compiling
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
    FShell: string;
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
    FPatchVersion: integer; //release candidate part of the version number, e.g. 3 for 1.0.8_RC3, or -1 if unknown
    FUtilFiles: array of TUtilsList; //Keeps track of binutils etc download locations, filenames...
    FExportOnly: boolean;
    FNoJobs: boolean;
    FVerbose: boolean;
    FUseWget: boolean;
    FTar: string;
    FBunzip2: string;
    F7zip: string;
    FWget: string;
    FUnrar: string;
    //FGit: string;
    FProcessEx: TProcessEx;
    FSwitchURL: boolean;
    FSolarisOI: boolean;
    FMUSL: boolean;
    FMUSLLinker: string;
    property Shell: string read GetShell;
    property Make: string read GetMake;
    // Check for existence of required executables; if not there, get them if possible
    function CheckAndGetTools: boolean;
    // Check for existence of required binutils; if not there, get them if possible
    function CheckAndGetNeededBinUtils: boolean;
    // Make a list (in FUtilFiles) of all binutils that can be downloaded
    procedure CreateBinutilsList(aVersion:string='');
    // Get a diff of all modified files in and below the directory and save it
    procedure CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
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
    // Download make.exe, patch.exe etc into the make directory (only implemented for Windows):
    function DownloadBinUtils: boolean;
    function DownloadSVN: boolean;
    function DownloadOpenSSL: boolean;
    function DownloadWget: boolean;
    function DownloadFreetype: boolean;
    function DownloadZlib: boolean;
    {$ENDIF}
    function DownloadJasmin: boolean;
    procedure DumpOutput(Sender: TProcessEx; output: string);
    // Looks for SVN client in subdirectories and sets FSVNClient.SVNExecutable if found.
    {$IFDEF MSWINDOWS}
    function FindSVNSubDirs: boolean;
    {$ENDIF}
    // Finds compiler in fpcdir path if TFPCInstaller descendant
    function GetCompiler: string;
    // Returns CPU-OS in the format used by the FPC bin directory, e.g. x86_64-win64:
    function GetFPCTarget(Native: boolean): string;
    procedure LogError(Sender: TProcessEx; IsException: boolean);
    // Sets the search/binary path to NewPath or adds NewPath before or after existing path:
    procedure SetPath(NewPath: string; Prepend: boolean; Append: boolean);
    // Get currently set path
    function GetPath: string;
    function GetFile(aURL,aFile:string; forceoverwrite:boolean=false; forcenative:boolean=false):boolean;
  public
    InfoText: string;
    LocalInfoText: string;
    property SVNClient: TSVNClient read FSVNClient;
    property GitClient: TGitClient read FGitClient;
    property HGClient: THGClient read FHGClient;
    // Get processor for termination of running processes
    property Processor: TProcessEx read FProcessEx;
    // Get processerrors and put them into FErrorLog
    procedure ProcessError(Sender:TProcessEx; {%H-}IsException:boolean);
    // Source directory for installation (fpcdir, lazdir,... option)
    property SourceDirectory: string write SetSourceDirectory;
    //Base directory for fpc(laz)up(deluxe) itself
    property BaseDirectory: string write FBaseDirectory;
    // Source directory for installation (fpcdir, lazdir,... option)
    property InstallDirectory: string write FInstallDirectory;
    // Compiler to use for building. Specify empty string when using bootstrap compiler.
    property Compiler: string {read GetCompiler} write FCompiler;
    // Compiler options passed on to make as OPT= or FPCOPT=
    property CompilerOptions: string write FCompilerOptions;
    // CPU for the target (together with CrossOS_Target the cross compile equivalent to GetFPCTarget)
    property CrossCPU_Target: string read FCrossCPU_Target;
    // Options for cross compiling. User can specify his own, but cross compilers can set defaults, too
    property CrossOPT: string read FCrossOPT write FCrossOPT;
    // OS for target (together with CrossCPU_Target the cross compile equivalent to GetFPCTarget)
    property CrossOS_Target: string read FCrossOS_Target;
    // SubArch for target embedded
    property CrossOS_SubArch: string read FCrossOS_SubArch;
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
    // do we have OpenIndiana instead of plain Solaris
    property SolarisOI: boolean write FSolarisOI;
    // do we have musl instead of libc
    property MUSL: boolean write FMUSL;
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
    property SourcePatches: string write FSourcePatches;
    // do not download the repo itself, but only get the files (of master)
    property ExportOnly: boolean write FExportOnly;
    property NoJobs: boolean write FNoJobs;
    // display and log in temp log file all sub process output
    property Verbose: boolean write FVerbose;
    // use wget as downloader ??
    property UseWget: boolean write FUseWget;
    // get cross-installer
    property CrossInstaller:TCrossInstaller read GetCrossInstaller;
    // set cross-target
    property NumericalVersion:dword read GetFullVersion;
    function GetCompilerName(Cpu_Target:string):string;
    function GetCrossCompilerName(Cpu_Target:string):string;
    procedure SetTarget(aCPU,aOS,aSubArch:string);virtual;
    // append line ending and write to log and, if specified, to console
    procedure WritelnLog(msg: string; ToConsole: boolean = true);overload;
    procedure WritelnLog(EventType: TEventType; msg: string; ToConsole: boolean = true);overload;
    function GetSuitableRepoClient:TRepoClient;
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
    // Patch sources
    function PatchModule(ModuleName: string): boolean; virtual;
    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; virtual;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  FileUtil
  {$IFNDEF HAIKU}
  //,ssl_openssl
  // for runtime init of openssl
  {$IFDEF MSWINDOWS}
  //,blcksock, ssl_openssl_lib
  ,openssl
  {$ENDIF}
  {$IF FPC_FULLVERSION > 30300}
  ,opensslsockets
  {$ENDIF}
  {$ENDIF}
  ;

{ TInstaller }

function TInstaller.GetCompiler: string;
begin
  if (Self.ClassNameIs('TFPCNativeInstaller')) or (Self.ClassNameIs('TFPCInstaller'))
    then Result := GetCompilerInDir(FInstallDirectory)
    else Result := FCompiler;
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
  FPatchVersion := -1;
end;

procedure TInstaller.SetSourceDirectory(value:string);
begin
  FSourceDirectory:=value;
  FMajorVersion := -1;
  FMinorVersion := -1;
  FReleaseVersion := -1;
  FPatchVersion := -1;
end;

function TInstaller.GetMake: string;
begin
  if FMake = '' then
    {$IFDEF MSWINDOWS}
    FMake := IncludeTrailingPathDelimiter(FMakeDir) + 'make' + GetExeExt;
    {$ELSE}
    {$IF (defined(BSD) and not defined(DARWIN)) or (defined(Solaris))}
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

function TInstaller.GetShell: string;
var
  output:string;
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF CPUX86}
  if FShell = '' then
  begin
    // disable for now .... not working 100%
    {
    // do we have a stray sh.exe in the path ...
    if (Length(Which('sh.exe'))>0) then
    begin
      ExecuteCommand('cmd.exe /C echo %COMSPEC%', output, False);
      FShell := Trim(output);
      if FShell = '' then
      begin
        //for older Windows versions
        ExecuteCommand('ECHO %COMSPEC%', output, False);
        FShell := Trim(output);
      end;
    end;
    }
  end;
  {$ENDIF CPU32}
  {$ENDIF MSWINDOWS}
  Result := FShell;
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
  aURL,aLocalClientBinary,Output: string;
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
    FWget := '';
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FBunzip2 := 'bunzip2';
    if FMUSL then FBunzip2 := 'unzip';
    FTar := 'tar';
    F7zip := '7za';
    FWget := 'wget';
    FUnrar := 'unrar';
    {$ENDIF LINUX}
    {$IFDEF BSD} //OSX, *BSD
    {$IFDEF DARWIN}
    FBunzip2 := ''; //not really necessary now
    FTar := 'bsdtar'; //gnutar is not available by default on Mavericks
    F7zip := '7za';
    FWget := 'wget';
    FUnrar := 'unrar';
    {$ELSE} //FreeBSD, OpenBSD, NetBSD
    FBunzip2 := 'bunzip2';
    FTar := 'tar'; //At least FreeBSD tar apparently takes some gnu tar options nowadays.
    F7zip := '7za';
    FWget := 'wget';
    FUnrar := 'unrar';
    {$ENDIF DARWIN}
    {$ENDIF BSD}

    {$IFDEF MSWINDOWS}
    ForceDirectoriesSafe(FMakeDir);

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
      // always get ssl libs if they are not there: sometimes system wide libs do not work
      if (NOT FileExists(SafeGetApplicationPath+'libeay32.dll')) OR (NOT FileExists(SafeGetApplicationPath+'ssleay32.dll')) then
      begin
        infoln(localinfotext+'Getting OpenSLL library files.',etInfo);
        DownloadOpenSSL;
        DestroySSLInterface; // disable ssl and release libs
      end
      else
      begin
        infoln(localinfotext+'Found OpenSLL library files.',etDebug);
        infoln(localinfotext+'Checking for correct signature.',etDebug);
        if (NOT CheckFileSignature(SafeGetApplicationPath+'libeay32.dll')) OR (NOT CheckFileSignature(SafeGetApplicationPath+'ssleay32.dll')) then
        begin
          infoln(localinfotext+'OpenSLL library files have wrong CPU signature.',etWarning);
          DeleteFile(SafeGetApplicationPath+'libeay32.dll');
          DeleteFile(SafeGetApplicationPath+'ssleay32.dll');
          infoln(localinfotext+'Getting correct OpenSLL library files.',etInfo);
          DownloadOpenSSL;
          DestroySSLInterface; // disable ssl and release libs
        end;
      end;
      if (NOT IsSSLloaded) then
      begin
        InitSSLInterface;
      end;
    end;

    if (NOT IsSSLloaded) then
      infoln(localinfotext+'Could not init SSL interface.',etWarning)
    else
      infoln(localinfotext+'Init SSL interface success.',etInfo);


    FWget:=Which('wget');
    if Not FileExists(FWget) then FWget := IncludeTrailingPathDelimiter(FMakeDir) + 'wget\wget.exe';
    if Not FileExists(FWget) then
    begin
      infoln(localinfotext+'Getting Wget.',etInfo);
      DownloadWget;
      OperationSucceeded:=FileExists(FWget);
      // do not fail
      OperationSucceeded:=True;
    end;
    //Set static wget binary location
    TUseWGetDownloader.WGETBinary:=FWget;

    // Get patch binary from default binutils URL
    GetFile(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw32/patch.exe',IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe');
    GetFile(BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw32/patch.exe.manifest',IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe.manifest');

    F7zip:=Which('7z');
    if Not FileExists(F7zip) then Which('7za');
    if Not FileExists(F7zip) then F7zip := IncludeTrailingPathDelimiter(FMakeDir) + '\7Zip\7za.exe';
    if Not FileExists(F7zip) then
    begin
      ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip');
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
      ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'unrar');
      //ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\bin');
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

    with FGitClient do
    begin
      OperationSucceeded:=False;
      aLocalClientBinary:=IncludeTrailingPathDelimiter(FMakeDir)+'git\cmd\git.exe';
      // try to find systemwide GIT
      if (NOT ForceLocal) then
      begin
        RepoExecutable:=Which(RepoExecutableName+'.exe');
        OperationSucceeded:=FileExists(RepoExecutable);
      end;
      // try to find fpcupdeluxe GIT
      if (NOT OperationSucceeded) then
      begin
        OperationSucceeded:=FileExists(aLocalClientBinary);
        if OperationSucceeded then RepoExecutable:=aLocalClientBinary;
      end;
      if (NOT OperationSucceeded) then
      begin
        //Source:
        //https://github.com/git-for-windows
        //install GIT only on Windows Vista and higher
        if CheckWin32Version(6,0) then
        begin
          ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'git');
          {$ifdef win32}
          //Output:='git32.7z';
          Output:='git32.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.17.1.windows.2/MinGit-2.17.1.2-32-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.18.0.windows.1/MinGit-2.18.0-32-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.19.0.windows.1/MinGit-2.19.0-32-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.21.0.windows.1/MinGit-2.21.0-32-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.22.0.windows.1/MinGit-2.22.0-32-bit.zip';
          aURL:='https://github.com/git-for-windows/git/releases/download/v2.23.0.windows.1/MinGit-2.23.0-32-bit.zip';
          {$else}
          //Output:='git64.7z';
          Output:='git64.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.17.1.windows.2/MinGit-2.17.1.2-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.18.0.windows.1/MinGit-2.18.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.19.0.windows.1/MinGit-2.19.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.21.0.windows.1/MinGit-2.21.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.22.0.windows.1/MinGit-2.22.0-64-bit.zip';
          aURL:='https://github.com/git-for-windows/git/releases/download/v2.23.0.windows.1/MinGit-2.23.0-64-bit.zip';
          {$endif}
          //aURL:=FPCUPGITREPO+'/releases/download/Git-2.13.2/'+Output;
          infoln(localinfotext+'GIT not found. Downloading it (may take time) from '+aURL,etInfo);
          OperationSucceeded:=GetFile(aURL,IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
          if NOT OperationSucceeded then
          begin
            // try one more time
            SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
            OperationSucceeded:=GetFile(aURL,IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
          end;
          if OperationSucceeded then
          begin
            infoln(localinfotext+'GIT client download ready: unpacking (may take time).',etInfo);
            with TNormalUnzipper.Create do
            begin
              try
                OperationSucceeded:=DoUnZip(IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'git\',[]);
              finally
                Free;
              end;
            end;
            if OperationSucceeded then
            begin
              SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'git\'+Output);
              OperationSucceeded:=FileExists(aLocalClientBinary);
            end;
          end;
          if OperationSucceeded then RepoExecutable:=aLocalClientBinary else RepoExecutable:=RepoExecutableName+'.exe';
        end;
      end;
      if RepoExecutable <> EmptyStr then
      begin
        // check exe, but do not fail: GIT is not 100% essential !
        CheckExecutable(RepoExecutable, '--version', '');
      end;
      // do not fail: GIT is not 100% essential !
      OperationSucceeded:=True;
    end;

    with FHGClient do
    begin
      OperationSucceeded:=False;
      aLocalClientBinary:=IncludeTrailingPathDelimiter(FMakeDir)+'hg\hg.exe';
      // try to find systemwide HG
      if (NOT ForceLocal) then
      begin
        RepoExecutable:=Which(RepoExecutableName+'.exe');
        OperationSucceeded:=FileExists(RepoExecutable);
      end;
      // try to find fpcupdeluxe HG
      if (NOT OperationSucceeded) then
      begin
        OperationSucceeded:=FileExists(aLocalClientBinary);
        if OperationSucceeded then RepoExecutable:=aLocalClientBinary;
      end;
      if (NOT OperationSucceeded) then
      begin
        //original source from : https://www.mercurial-scm.org/
        {$ifdef win32}
        Output:='hg32.zip';
        {$else}
        Output:='hg64.zip';
        {$endif}
        aURL:=FPCUPGITREPO+'/releases/download/HG-4.7/'+Output;
        ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'hg');
        infoln(localinfotext+'HG (mercurial) client not found. Downloading it (may take time) from '+aURL,etInfo);
        OperationSucceeded:=GetFile(aURL,IncludeTrailingPathDelimiter(FMakeDir)+'hg\'+Output);
        if NOT OperationSucceeded then
        begin
          // try one more time
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'hg\'+Output);
          OperationSucceeded:=GetFile(aURL,IncludeTrailingPathDelimiter(FMakeDir)+'hg\'+Output);
        end;
        if OperationSucceeded then
        begin
          infoln(localinfotext+'HG download ready: unpacking (may take time).',etInfo);
          with TNormalUnzipper.Create do
          begin
            try
              OperationSucceeded:=DoUnZip(IncludeTrailingPathDelimiter(FMakeDir)+'hg\'+Output,IncludeTrailingPathDelimiter(FMakeDir)+'hg\',[]);
            finally
              Free;
            end;
          end;
          if OperationSucceeded then
          begin
            SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'hg\'+Output);
            OperationSucceeded:=FileExists(aLocalClientBinary);
          end;
        end;
        if OperationSucceeded then RepoExecutable:=aLocalClientBinary else RepoExecutable:=RepoExecutableName+'.exe';
      end;
      if RepoExecutable <> EmptyStr then
      begin
        // check exe, but do not fail: HG is not 100% essential !
        CheckExecutable(RepoExecutable, '--version', '');
      end;
      // do not fail: HG is not 100% essential !
      OperationSucceeded:=True;
    end;

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
      if (NOT AllThere) OR (FSVNClient.ForceLocal) then
      begin
        FSVNDirectory := IncludeTrailingPathDelimiter(FMakeDir) + FSVNClient.RepoExecutableName + DirectorySeparator;
        AllThere:=FindSVNSubDirs;
        if (NOT AllThere) then
        begin
          infoln(localinfotext+'Going to download SVN',etInfo);
          // Download will look in and below FSVNDirectory
          // and set FSVNClient.SVNExecutable if succesful
          AllThere := DownloadSVN;
        end;
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
        if (NOT OperationSucceeded) then infoln(localinfotext+FBunzip2+' not found.',etDebug);
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid tar executable, if it is needed
      if FTar <> EmptyStr then
      begin
        OperationSucceeded := CheckExecutable(FTar, '--version', '');
        if (NOT OperationSucceeded) then infoln(localinfotext+FTar+' not found.',etDebug);
      end;
    end;

    {$IFNDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      OperationSucceeded := CheckExecutable(Make, '-v', '');
      if (NOT OperationSucceeded) then infoln(localinfotext+Make+' not found.',etError);
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
  s1,s2: string;
begin
  s2:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBinUtils): ';

  OperationSucceeded := true;

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Download if needed, including unzip - needed for SVN download
    // Check for binutils directory
    AllThere:=true;
    if DirectoryExists(FMakeDir) = false then
    begin
      infoln(s2+'Make path ' + FMakeDir + ' does not exist. Going to download binutils.',etInfo);
      AllThere:=false;
    end
    else
    begin
      // Check all binutils in directory
      for i:=low(FUtilFiles) to high(FUtilFiles) do
      begin
        if FUtilFiles[i].Category=ucBinutil then
        begin
          if (NOT FileExists(IncludeTrailingPathDelimiter(FMakeDir)+FUtilFiles[i].FileName)) then
          begin
            AllThere:=false;
            break;
          end;
        end;
      end;
    end;
    if not(AllThere) then
    begin
      infoln(s2+'Make path [' + FMakeDir + '] does not have (all) binutils. Going to download needed binutils.',etInfo);
      //infoln(s2+'Some binutils missing: going to get them.',etInfo);
      OperationSucceeded := DownloadBinUtils;
    end;
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded then
  begin

    {$IFDEF MSWINDOWS}
    // check if we have make ... otherwise get it from standard URL
    if (NOT FileExists(Make)) then
    begin
      s1:=BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/'+ExtractFileName(Make);
      infoln(s2+'Make binary not found. Getting it from: '+s1+'.',etInfo);
      GetFile(s1,Make);
      OperationSucceeded:=FileExists(Make);
    end;
    {$ENDIF MSWINDOWS}

    // Check for proper make executable
    if OperationSucceeded then
    try
      ExecuteCommand(Make + ' -v', s1, False);
      if AnsiPos('GNU Make', s1) = 0 then
      begin
        ExecuteCommand(Make + ' -v', s1, True);
        infoln(s2+'Found make binary here: '+Make+'. But it is not GNU Make.',etError);
        OperationSucceeded := false;
      end
      else
      begin
        infoln(s2+'Found GNU make binary here: '+Make+'.',etInfo);
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
  SourceURL_gdb = LAZARUSBINARIES+'/i386-win32/gdb/bin/';
  //SourceURL_gdb = 'https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Alternative%20GDB/GDB%208.1/gdb.exe/download';
  //SourceURL_gdbserver = 'https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Alternative%20GDB/GDB%208.1/gdbserver.exe/download';
  //SourceURL_gdb = 'https://github.com/newpascal/fpcupdeluxe/releases/download/gdb-7.11.1/GDB-i386-win32.zip';
  SourceURL64_gdb = LAZARUSBINARIES+'/x86_64-win64/gdb/bin/';
  //SourceURL64_gdb = 'https://github.com/newpascal/fpcupdeluxe/releases/download/gdb-7.11.1/GDB-x86_64-win64.zip';
  SourceURL_QT = LAZARUSBINARIES+'/i386-win32/qt/';
  SourceURL_QT5 = LAZARUSBINARIES+'/i386-win32/qt5/';

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

  {$ifdef MSWINDOWS}

  // default
  if aVersion='' then aVersion:=DEFAULTFPCVERSION;

  // if Win Vista or higher: use modern (2.4.0 and higher) binutils
  if CheckWin32Version(6,0) then
  begin
    if (GetNumericalVersion(aVersion)<CalculateFullVersion(2,4,0)) then
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
  AddNewUtil('nm' + GetExeExt,aSourceURL,'',ucBinutil);

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

  AddNewUtil('Qt4Pas5.dll',SourceURL_QT,'',ucQtFile);
  AddNewUtil('Qt5Pas1.dll',SourceURL_QT5,'',ucQtFile);
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
  {$endif win64}

  // add wince gdb
  AddNewUtil('gdb-6.4-win32-arm-wince.zip',FPCFTPURL+'/contrib/cross/','',ucDebuggerWince);

  {$endif MSWINDOWS}
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

function TInstaller.DownloadFromBase(aClient:TRepoClient; ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList; const aUserName:string=''; const aPassword:string=''): boolean;
var
  BeforeRevisionShort: string; //Basically the branch revision number
  ReturnCode: integer;
  DiffFile,DiffFileCorrectedPath: String;
  LocalPatchCmd : string;
  DiffFileSL:TStringList;
  Output: string = '';
begin
  Result := false;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' ('+Copy(aClient.ClassName,2,MaxInt)+': '+ModuleName+'): ';

  // check if we do have a client !!
  if NOT aClient.ValidClient then
  begin
    infoln(localinfotext+aClient.RepoExecutableName+' is needed, but cannot be found on the system !!',etWarning);
    exit;
  end;

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

  //add a dummy newline for better output parsing of command results
  writeln;

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

         if ReturnCode<>0 then
         begin
           // Patching can go wrong when line endings are not compatible
           // This happens e.g. with bgracontrols that have CRLF in the source files
           // Try to circumvent this problem by replacing line enddings
           if Pos('different line endings',Output)>0 then
           begin
             //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, FSourceDirectory, FVerbose);
             //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, FSourceDirectory, FVerbose);

             DiffFileCorrectedPath:=SysUtils.GetTempDir+ExtractFileName(DiffFile);
             if FileCorrectLineEndings(DiffFile,DiffFileCorrectedPath) then
             begin
               if FileExists(DiffFileCorrectedPath) then
               begin
                 ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFileCorrectedPath, FSourceDirectory, Output, FVerbose);
                 DeleteFile(DiffFileCorrectedPath);
               end;
             end;
           end;
         end;

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
  {$IFNDEF MSWINDOWS}
  DiffFileSL:TStringList;
  {$ENDIF}
begin
  result:=true;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadFromSVN: '+ModuleName+'): ';

  // check if we do have a client !!
  if NOT FSVNClient.ValidClient then
  begin
    infoln(localinfotext+FSVNClient.RepoExecutableName+' is needed, but cannot be found on the system !!',etWarning);
    exit;
  end;

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
    if not(DirectoryExists(FSVNClient.LocalRepository)) then
    begin
      writelnlog(localinfotext+'Creating directory '+FSourceDirectory+' for SVN checkout.');
      ForceDirectoriesSafe(FSourceDirectory);
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

      if FExportOnly then
      begin
        AfterRevision := FDesiredRevision;
        if Trim(AfterRevision)='' then AfterRevision := FSVNClient.LocalRevisionWholeRepo;
      end
      else
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
        writelnlog(localinfotext+'SVN gave error code: '+IntToStr(CheckoutOrUpdateReturnCode));
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
function TInstaller.DownloadBinUtils: boolean;
var
  Counter: integer;
  Errors: integer = 0;
  DownloadSuccess:boolean;
  InstallPath:string;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBinUtils): ';
  //Parent directory of files. Needs trailing backslash.
  ForceDirectoriesSafe(FMakeDir);
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
        ForceDirectoriesSafe(InstallPath);
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

{$ifdef win64444}
function TInstaller.DownloadBinUtils: boolean;
var
  SourceURL,BinsZip:string;
  OperationSucceeded:boolean;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBinUtils): ';
  //Parent directory of files. Needs trailing backslash.
  ForceDirectoriesSafe(FMakeDir);
  Result := true;
  OperationSucceeded := false;
  SourceURL:=FPCUPGITREPO+'/releases/download/wincrossbins_v1.0/Win64Bins.zip';
  BinsZip := GetTempFileNameExt('','FPCUPTMP','zip');
  try
    OperationSucceeded := Download(
      FUseWget,
      SourceURL,
      BinsZip,
      FHTTPProxyUser,
      FHTTPProxyPort,
      FHTTPProxyUser,
      FHTTPProxyPassword);
  except
    // Deal with timeouts, wrong URLs etc
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading Win64 binutils from ' + SourceURL, true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    with TNormalUnzipper.Create do
    begin
      try
        OperationSucceeded:=DoUnZip(BinsZip,IncludeTrailingPathDelimiter(FMakeDir),[]);
      finally
        Free;
      end;
    end;
  end;

  if OperationSucceeded then
  begin
    WritelnLog(localinfotext + 'Win64 binutils download and unpacking ok.', true);
    OperationSucceeded := FindSVNSubDirs;
    if OperationSucceeded then
      SysUtils.Deletefile(BinsZip); //Get rid of temp zip if success.
  end;

  Result := OperationSucceeded;
end;
{$endif}

function TInstaller.DownloadSVN: boolean;
const
  NewSourceURL : array [0..4] of string = (
    'https://www.visualsvn.com/files/Apache-Subversion-1.12.2.zip',
    'https://www.visualsvn.com/files/Apache-Subversion-1.11.1.zip',
    'https://www.visualsvn.com/files/Apache-Subversion-1.10.3.zip',
    'https://www.visualsvn.com/files/Apache-Subversion-1.10.2.zip',
    'https://sourceforge.net/projects/win32svn/files/1.8.17/apache24/svn-win32-1.8.17-ap24.zip/download'
    );
var
  OperationSucceeded: boolean;
  SVNZip,aSourceURL: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadSVN): ';

  OperationSucceeded := false;

  SVNZip := GetTempFileNameExt('','FPCUPTMP','zip');

  ForceDirectoriesSafe(FSVNDirectory);

  for i:=0 to (Length(NewSourceURL)-1) do
  try
    aSourceURL:=NewSourceURL[i];
    //always get this file with the native downloader !!
    OperationSucceeded:=GetFile(aSourceURL,SVNZip,true,true);
    if (NOT OperationSucceeded) then
    begin
      // try one more time
      SysUtils.DeleteFile(SVNZip);
      OperationSucceeded:=GetFile(aSourceURL,SVNZip,true,true);
    end;
    if (NOT OperationSucceeded) then
      SysUtils.DeleteFile(SVNZip)
    else
      break;

  except
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading SVN client', true);
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
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  OpenSSLFileName,Output,aSourceURL: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadOpenSSL): ';

  infoln(localinfotext+'No OpenSLL library files available for SSL. Going to download them.',etWarning);

  OperationSucceeded := false;

  OpenSSLFileName := GetTempFileNameExt('','FPCUPTMP','zip');

  for i:=0 to (Length(OpenSSLSourceURL)-1) do
  try
    aSourceURL:=OpenSSLSourceURL[i];
    //always get this file with the native downloader !!
    OperationSucceeded:=GetFile(aSourceURL,OpenSSLFileName,true,true);
    if (NOT OperationSucceeded) then
    begin
      // try one more time
      SysUtils.DeleteFile(OpenSSLFileName);
      OperationSucceeded:=GetFile(aSourceURL,OpenSSLFileName,true,true);
    end;
    if (NOT OperationSucceeded) then
      SysUtils.DeleteFile(OpenSSLFileName)
    else
      break;

  except
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading OpenSSL library', true);
    end;
  end;

  if (NOT OperationSucceeded) then
  begin
    infoln(localinfotext+'Could not download/install openssl library the normal way', etInfo);
    infoln(localinfotext+'Now going to use BitsAdmin (may be slow)', etInfo);

    for i:=0 to (Length(OpenSSLSourceURL)-1) do
    try
      aSourceURL:=OpenSSLSourceURL[i];
      SysUtils.DeleteFile(OpenSSLFileName);
      OperationSucceeded:=DownloadByBitsAdmin(aSourceURL,OpenSSLFileName);
      if (NOT OperationSucceeded) then
        SysUtils.DeleteFile(OpenSSLFileName)
      else
        break;

    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading OpenSSL library by BitsAdmin', true);
      end;
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract
    with TNormalUnzipper.Create do
    begin
      try
        resultcode:=2;
        SysUtils.Deletefile(SafeGetApplicationPath+'libeay32.dll');
        if GetLastOSError<>5 then // no access denied
        begin
          SysUtils.Deletefile(SafeGetApplicationPath+'ssleay32.dll');
          if GetLastOSError<>5 then // no access denied
          begin
            resultcode:=1;
            if DoUnZip(OpenSSLFileName,SafeGetApplicationPath,['libeay32.dll','ssleay32.dll']) then resultcode:=0;
          end;
        end;
      finally
        Free;
      end;
    end;

    if resultcode <> 0 then
    begin
      OperationSucceeded := false;
      if resultcode=2 then writelnlog(etWarning, localinfotext + 'Download OpenSSL error: could not delete/overwrite existing files.');
      if resultcode=1 then writelnlog(etError, localinfotext + 'Download OpenSSL error: could not unzip files.');
    end;
  end;

  if OperationSucceeded
     then infoln(localinfotext+'OpenSLL library files download and unpacking from '+aSourceURL+' ok.',etWarning)
     else infoln(localinfotext+'Could not download/install openssl library archive.', etError);
  SysUtils.Deletefile(OpenSSLFileName); //Get rid of temp zip if success.

  //Real last resort: get OpenSSL from from Lazarus binaries
  if (NOT OperationSucceeded) then
  begin
    OpenSSLFileName:=SafeGetApplicationPath+'libeay32.dll';
    OperationSucceeded:=GetFile(OPENSSL_URL_LATEST_SVN+'/libeay32.dll',OpenSSLFileName,true,true);
    if OperationSucceeded then
    begin
      OpenSSLFileName:=SafeGetApplicationPath+'ssleay32.dll';
      OperationSucceeded:=GetFile(OPENSSL_URL_LATEST_SVN+'/ssleay32.dll',OpenSSLFileName,true,true);
    end;
    if OperationSucceeded
       then infoln(localinfotext+'OpenSLL library files download from '+OPENSSL_URL_LATEST_SVN+'s ok.',etWarning)
       else infoln(localinfotext+'Could not download/install openssl library from '+OPENSSL_URL_LATEST_SVN+'.', etError);
  end;

  Result := OperationSucceeded;
 end;

function TInstaller.DownloadWget: boolean;
const
  {$ifdef win64}
  NewSourceURL : array [0..0] of string = (
    //'https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/zlib/wget-64.zip',
    //'https://eternallybored.org/misc/wget/1.19.4/64/wget.exe'
    'https://eternallybored.org/misc/wget/1.20/64/wget.exe'
    );
  {$endif}
  {$ifdef win32}
  NewSourceURL : array [0..0] of string = (
    //'https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/zlib/wget-32.zip',
    //'https://eternallybored.org/misc/wget/1.19.4/32/wget.exe'
    'https://eternallybored.org/misc/wget/1.20/32/wget.exe'
    );
  {$endif}
var
  OperationSucceeded: boolean;
  WgetFile,WgetExe,WgetZip: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadWget): ';

  infoln(localinfotext+'No Wget found. Going to download it.',etInfo);

  OperationSucceeded := false;

  if ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'wget') then
  begin
    WgetExe := IncludeTrailingPathDelimiter(FMakeDir)+'wget'+DirectorySeparator+'wget.exe';

    WgetZip := GetTempFileNameExt('','FPCUPTMP','zip');

    for i:=0 to (Length(NewSourceURL)-1) do
    try
      WgetFile:=GetFileNameFromURL(NewSourceURL[i]);

      //always get this file with the native downloader !!
      OperationSucceeded:=GetFile(NewSourceURL[i],WgetExe,true,true);
      if (NOT OperationSucceeded) then
      begin
        // try one more time
        SysUtils.DeleteFile(WgetExe);
        OperationSucceeded:=GetFile(NewSourceURL[i],WgetExe,true,true);
      end;
      if (NOT OperationSucceeded) then
        SysUtils.DeleteFile(WgetExe)
      else
        break;

    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading Wget', true);
      end;
    end;

  end;

  if NOT OperationSucceeded then SysUtils.Deletefile(WgetExe);
  Result := OperationSucceeded;
end;

function TInstaller.DownloadFreetype: boolean;
const
  NewSourceURL : array [0..0] of string = (
    'https://sourceforge.net/projects/gnuwin32/files/freetype/2.3.5-1/freetype-2.3.5-1-bin.zip/download'
    );
var
  OperationSucceeded: boolean;
  FreetypeDir,FreetypeBin,FreetypZip,FreetypZipDir: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadFreetype): ';

  infoln(localinfotext+'No Freetype found. Going to download it.',etInfo);

  OperationSucceeded := false;

  FreetypeDir:=IncludeTrailingPathDelimiter(FInstallDirectory);
  FreetypeBin:=FreetypeDir+'freetype-6.dll';

  if NOT FileExists(FreetypeBin) then
  begin

    FreetypZip := GetTempFileNameExt('','FPCUPTMP','zip');

    for i:=0 to (Length(NewSourceURL)-1) do
    try
      //always get this file with the native downloader !!
      OperationSucceeded:=GetFile(NewSourceURL[i],FreetypZip,true,true);
      if (NOT OperationSucceeded) then
      begin
        // try one more time
        SysUtils.DeleteFile(FreetypZip);
        OperationSucceeded:=GetFile(NewSourceURL[i],FreetypZip,true,true);
      end;
      if (NOT OperationSucceeded) then
        SysUtils.DeleteFile(FreetypZip)
      else
        break;

    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading Freetype', true);
      end;
    end;

  end;

  if OperationSucceeded then
  begin
    FreetypZipDir:=IncludeTrailingPathDelimiter(SysUtils.GetTempDir)+'Freetype';
    // Extract
    with TNormalUnzipper.Create do
    begin
      try
        OperationSucceeded:=DoUnZip(FreetypZip,FreetypZipDir,[]);
      finally
        Free;
      end;
    end;
  end;

  if OperationSucceeded then
  begin
    //MoveFile
    OperationSucceeded := MoveFile(FreetypZipDir+DirectorySeparator+'bin'+DirectorySeparator+'freetype6.dll',FreetypeBin);
    if NOT OperationSucceeded then
    begin
      writelnlog(etError, localinfotext + 'Could not move freetype6.dll into '+FreetypeBin);
    end
    else OperationSucceeded := FileExists(FreetypeBin);
  end;

  SysUtils.Deletefile(FreetypZip);
  DeleteDirectoryEx(FreetypZipDir+DirectorySeparator);
  Result:=true; //never fail
end;

function TInstaller.DownloadZlib: boolean;
const
  TARGETNAME='zlib1.dll';
  SOURCEURL : array [0..0] of string = (
    'https://sourceforge.net/projects/gnuwin32/files/zlib/1.2.3/zlib-1.2.3-bin.zip/download'
    );
var
  OperationSucceeded: boolean;
  TargetDir,TargetBin,SourceBin,SourceZip,ZipDir: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (Download '+TARGETNAME+'): ';

  infoln(localinfotext+'No '+TARGETNAME+' found. Going to download it.');

  OperationSucceeded := false;

  TargetDir:=IncludeTrailingPathDelimiter(FInstallDirectory);
  TargetBin:=TargetDir+TARGETNAME;

  if NOT FileExists(TargetBin) then
  begin

    SourceZip := GetTempFileNameExt('','FPCUPTMP','zip');

    for i:=0 to (Length(SOURCEURL)-1) do
    try
      //always get this file with the native downloader !!
      OperationSucceeded:=GetFile(SOURCEURL[i],SourceZip,true,true);
      if (NOT OperationSucceeded) then
      begin
        // try one more time
        SysUtils.DeleteFile(SourceZip);
        OperationSucceeded:=GetFile(SOURCEURL[i],SourceZip,true,true);
      end;
      if (NOT OperationSucceeded) then
        SysUtils.DeleteFile(SourceZip)
      else
        break;

    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading ' + TARGETNAME, true);
      end;
    end;

  end;

  if OperationSucceeded then
  begin
    ZipDir:=GetTempDirName('','FPCUPTMP');
    // Extract
    with TNormalUnzipper.Create do
    begin
      try
        OperationSucceeded:=DoUnZip(SourceZip,ZipDir,[]);
      finally
        Free;
      end;
    end;

    if OperationSucceeded then
    begin
      //MoveFile
      SourceBin:=ZipDir+DirectorySeparator+'bin'+DirectorySeparator+TARGETNAME;
      OperationSucceeded := MoveFile(SourceBin,TargetBin);
      if (NOT OperationSucceeded) then
      begin
        writelnlog(etError, localinfotext + 'Could not move ' + SourceBin + ' towards '+TargetBin);
      end
      else OperationSucceeded := FileExists(TargetBin);
    end;
  end;

  SysUtils.Deletefile(SourceZip);
  DeleteDirectoryEx(ZipDir+DirectorySeparator);
  Result:=true; //never fail
end;

{$ENDIF}

function TInstaller.DownloadJasmin: boolean;
const
  JASMINVERSION = '2.4';
  TARGETNAME='jasmin.jar';
  SOURCEURL : array [0..1] of string = (
    'https://sourceforge.net/projects/jasmin/files/jasmin/jasmin-'+JASMINVERSION+'/jasmin-'+JASMINVERSION+'.zip/download',
    'https://github.com/davidar/jasmin/archive/'+JASMINVERSION+'.zip'
    //http://www.java2s.com/Code/JarDownload/jasmin/jasmin.jar.zip
    //http://www.java2s.com/Code/JarDownload/jasmin/jasmin-3.0.3.jar.zip
    );
var
  OperationSucceeded: boolean;
  TargetDir,TargetBin,SourceBin,SourceZip,ZipDir: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (Download '+TARGETNAME+'): ';

  OperationSucceeded := false;

  // for now, just put jasmin.jar in FPC bin-directory ... easy and simple and working
  TargetDir:=IncludeTrailingPathDelimiter(FInstallDirectory) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator;
  TargetBin:=TargetDir+TARGETNAME;

  if (NOT FileExists(TargetBin)) then
  begin
    infoln(localinfotext+'No '+TARGETNAME+' found. Going to download it.');

    SourceZip := GetTempFileNameExt('','FPCUPTMP','zip');

    for i:=0 to (Length(SOURCEURL)-1) do
    try
      //always get this file with the native downloader !!
      OperationSucceeded:=GetFile(SOURCEURL[i],SourceZip,true,true);
      if (NOT OperationSucceeded) then
      begin
        // try one more time
        SysUtils.DeleteFile(SourceZip);
        OperationSucceeded:=GetFile(SOURCEURL[i],SourceZip,true,true);
      end;
      if (NOT OperationSucceeded) then
        SysUtils.DeleteFile(SourceZip)
      else
        break;

    except
      on E: Exception do
      begin
        OperationSucceeded := false;
        writelnlog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading ' + TARGETNAME, true);
      end;
    end;

  end;

  if OperationSucceeded then
  begin
    ZipDir:=GetTempDirName('','FPCUPTMP');
    // Extract
    with TNormalUnzipper.Create do
    begin
      try
        OperationSucceeded:=DoUnZip(SourceZip,ZipDir,[]);
      finally
        Free;
      end;
    end;

    if OperationSucceeded then
    begin
      //MoveFile
      SourceBin:=ZipDir+DirectorySeparator+'jasmin-' + JASMINVERSION + DirectorySeparator+TARGETNAME;
      OperationSucceeded := MoveFile(SourceBin,TargetBin);
      if (NOT OperationSucceeded) then
      begin
        writelnlog(etError, localinfotext + 'Could not move ' + SourceBin + ' towards '+TargetBin);
      end
      else OperationSucceeded := FileExists(TargetBin);
    end;
  end;

  SysUtils.Deletefile(SourceZip);
  DeleteDirectoryEx(ZipDir+DirectorySeparator);
  Result:=true; //never fail
end;

procedure TInstaller.DumpOutput(Sender: TProcessEx; output: string);
begin
  if FVerbose then
  begin
    // Set up initial output
    if (NOT Assigned(FLogVerbose)) then
    begin
      FLogVerbose:=TLogger.Create;
      FLogVerbose.LogFile:=GetTempFileNameExt('','FPCUPLOG','log');
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
      infoln(localinfotext+'Could not find svn executable in or under ' + FSVNDirectory,etInfo);
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
  WritelnLog('Going to search for SVN client in ' + IncludeTrailingPathDelimiter(dirName)+'*');
  if SysUtils.FindFirst(IncludeTrailingPathDelimiter(dirName)+'*', faAnyFile, searchResult)=0 then
  begin
    try
      repeat
        if (searchResult.Attr and faDirectory)=0 then
        begin
          if SameText(searchResult.Name, FSVNClient.RepoExecutableName + GetExeExt) then
          begin
            FSVNClient.RepoExecutable:=IncludeTrailingPathDelimiter(dirName)+searchResult.Name;
          end;
        end else if (searchResult.Name<>'.') and (searchResult.Name<>'..') then
        begin
          FileSearch(IncludeTrailingPathDelimiter(dirName)+searchResult.Name);
        end;
      until ( (SysUtils.FindNext(searchResult)<>0) OR (Length(FSVNClient.RepoExecutable)<>0) );
    finally
      SysUtils.FindClose(searchResult);
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
begin
  result:=GetFPCTargetCPUOS(FCrossCPU_Target,FCrossOS_Target,Native);
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
  TempFileName := GetTempFileNameExt('','FPCUPDUMP','dump');
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
begin
  Result := IncludeTrailingPathDelimiter(Dir) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator + 'fpc' + GetExeExt;
  {$IFDEF UNIX}
  if FileExists(Result + '.sh') then
  begin
    //Use our proxy if it is installed
    Result := Result + '.sh';
  end;
  {$ENDIF UNIX}
end;

procedure TInstaller.SetTarget(aCPU,aOS,aSubArch:string);
begin
  FCrossCPU_Target:=aCPU;
  FCrossOS_Target:=aOS;
  FCrossOS_SubArch:=aSubArch;
end;

function TInstaller.GetSuitableRepoClient:TRepoClient;
begin
  // not so elegant check to see what kind of client we need ...
  if ( {(Pos('GITHUB',UpperCase(FURL))>0) OR} (Pos('.GIT',UpperCase(FURL))>0) )
     then result:=FGitClient
     else result:=FSVNClient;
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
  FCleanModuleSuccess:=false;
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

      aRepoClient.Verbose:=FVerbose;
      aRepoClient.ExportOnly:=FExportOnly;
      aRepoClient.ModuleName:=ModuleName;
      aRepoClient.LocalRepository:=FSourceDirectory;
      aRepoClient.Repository:=FURL;

      aRepoClient.SwitchURL;

      result:=true;
    end;
  end;
end;

function TInstaller.PatchModule(ModuleName: string): boolean;
const
  STRIPMAGIC='fpcupstrip';
var
  PatchList:TStringList;
  PatchFilePath,PatchFileCorrectedPath,PatchDirectory:string;
  LocalPatchCmd:string;
  Output: string = '';
  ReturnCode,i,j: integer;
  LocalSourcePatches:string;
  PatchFPC:boolean;
  {$ifndef FPCONLY}
  PatchLaz:boolean;
  {$endif}
begin
  result:=false;

  PatchFPC:=(ModuleName=_FPC);
  {$ifndef FPCONLY}
  PatchLaz:=(ModuleName=_LAZARUS);
  {$endif}

  if PatchFPC then PatchDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'patchfpc' else
     {$ifndef FPCONLY}
     if PatchLaz then PatchDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'patchlazarus' else
     {$endif}
        PatchDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'patchfpcup';

  // always remove the previous fpcupdeluxe patches when updating the source !!!
  if (DirectoryExists(PatchDirectory)) then
  begin
    //DeleteDirectoryEx(PatchDirectory);
    PatchList := FindAllFiles(PatchDirectory, '*.patch;*.diff', false);
    try
      if (PatchList.Count>0) then
      begin
        for i:=0 to (PatchList.Count-1) do
        begin
          PatchFilePath:=PatchList.Strings[i];
          j:=-1;
          if PatchFPC then j:=Pos('_FPCPATCH',PatchFilePath) else
             {$ifndef FPCONLY}
             if PatchLaz then j:=Pos('_LAZPATCH',PatchFilePath) else
             {$endif}
                j:=Pos('_FPCUPPATCH',PatchFilePath);
          if (j<>-1) then DeleteFile(PatchFilePath);
        end;
      end;
    finally
      PatchList.Free;
    end;
  end;

  LocalSourcePatches:=FSourcePatches;

  if resourcefiles.Count>0 then
  begin
    for i:=0 to resourcefiles.Count-1 do
    begin
      PatchFilePath:=resourcefiles[i];
      if PatchFPC then j:=Pos('_FPCPATCH',PatchFilePath) else
         {$ifndef FPCONLY}
         if PatchLaz then j:=Pos('_LAZPATCH',PatchFilePath) else
         {$endif}
            j:=Pos('_FPCUPPATCH',PatchFilePath);

      {$if defined(Darwin) and defined(LCLQT5)}
      //disable big hack for now
      if Pos('DARWINQT5HACK_LAZPATCH',PatchFilePath)>0 then j:=0;
      {$else}
      if Pos('DARWINQT5',PatchFilePath)>0 then j:=0;
      {$endif}

      {$ifndef MSWindows}
      //only patch the Haiku build process on Windows
      if Pos('HAIKU_FPCPATCH',PatchFilePath)>0 then j:=0;
      {$endif}

      {$ifndef Haiku}
      //only patch the Haiku FPU exception mask on Haiku
      if Pos('HAIKUFPU_FPCPATCH',PatchFilePath)>0 then j:=0;
      {$endif}

      // In general, only patch trunk !
      // This can be changed to take care of versions ... but not for now !
      // Should be removed in future fpcup versions !!
      if PatchFPC then
      begin
        if (Pos('FREEBSDFIXES',PatchFilePath)>0) then
        begin
          if (GetFullVersion>=GetNumericalVersion(FPCTRUNKVERSION)) then j:=0;
        end
        else
        begin
          if (GetFullVersion<GetNumericalVersion(FPCTRUNKVERSION)) then j:=0;
        end;
      end;
      {$ifndef FPCONLY}
      if PatchLaz then
      begin
        if (FMUSL AND (Pos('MUSLTRUNK',PatchFilePath)>0)) then
        begin
          if (GetFullVersion<GetNumericalVersion('2.0.0')) then j:=0;
        end
        else
        begin
          if (GetFullVersion<GetNumericalVersion(LAZARUSTRUNKVERSION)) then j:=0;
        end;
      end;
      {$endif}

      if (j>0) then
      begin
        ForceDirectoriesSafe(PatchDirectory);
        SaveFileFromResource(PatchDirectory+DirectorySeparator+PatchFilePath+'.patch',resourcefiles[i]);
      end;
    end;
  end;

  if (DirectoryExists(PatchDirectory)) then
  begin
    PatchList := FindAllFiles(PatchDirectory, '*.patch;*.diff', false);
    try
      if (PatchList.Count>0) then
      begin
        //add standard patches by fpcup(deluxe)
        if Length(LocalSourcePatches)>0
           then LocalSourcePatches:=LocalSourcePatches+','+PatchList.CommaText
           else LocalSourcePatches:=PatchList.CommaText;
      end;
    finally
      PatchList.Free;
    end;
  end;

  if Length(LocalSourcePatches)>0 then
  begin
    PatchList:=TStringList.Create;
    try
      PatchList.CommaText := LocalSourcePatches;
      for i:=0 to (PatchList.Count-1) do
      begin
        infoln(infotext+'Trying to patch ' + ModuleName + ' with '+PatchList[i],etInfo);
        PatchFilePath:=SafeExpandFileName(PatchList[i]);
        if NOT FileExists(PatchFilePath) then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+PatchList[i]);
        if NOT FileExists(PatchFilePath) then
        begin
          if PatchFPC then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchfpc'+DirectorySeparator+PatchList[i])
             {$ifndef FPCONLY}
             else if PatchLaz then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchlazarus'+DirectorySeparator+PatchList[i])
             {$endif}
                else PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchfpcup'+DirectorySeparator+PatchList[i]);
        end;
        if FileExists(PatchFilePath) then
        begin

          j:=Pos(STRIPMAGIC,PatchFilePath);
          if j>0 then
          begin
            j:=StrToIntDef(PatchFilePath[j+Length(STRIPMAGIC)],0);
          end else j:=0;

          // check for default values
          if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
            {$IF defined(BSD) and not defined(DARWIN)}
            then LocalPatchCmd:=FPatchCmd + ' -p' + InttoStr(j) + ' -N -i '
            {$else}
            then LocalPatchCmd:=FPatchCmd + ' -p' + InttoStr(j) + ' -N --no-backup-if-mismatch -i '
            {$endif}
             else LocalPatchCmd:=Trim(FPatchCmd) + ' ';

          // always correct for line-endings while patch is very sensitive for that
          PatchFileCorrectedPath:=SysUtils.GetTempDir+ExtractFileName(PatchFilePath);
          if FileCorrectLineEndings(PatchFilePath,PatchFileCorrectedPath) then
          begin
            // revert to original file in case of file not found
            if (NOT FileExists(PatchFileCorrectedPath)) then PatchFileCorrectedPath:=PatchFilePath;
            {$IFDEF MSWINDOWS}
            ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + PatchFileCorrectedPath, FSourceDirectory, Output, True);
            {$ELSE}
            ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + PatchFileCorrectedPath, FSourceDirectory, Output, True);
            {$ENDIF}
            // remove the temporary file
            if (PatchFileCorrectedPath<>PatchFilePath) then DeleteFile(PatchFileCorrectedPath);
            if ReturnCode=0  then
            begin
              result:=true;
              writelnlog(etInfo, infotext+ModuleName+ ' has been patched successfully with '+PatchList[i] + '.', true);
            end
            else
            begin
              writelnlog(etError, infotext+ModuleName+' patching with ' + PatchList[i] + ' failed.', true);
              writelnlog(etError, infotext+ModuleName+' patch output: ' + Output, true);
            end;
          end;
        end
        else
        begin
          result:=true;
          writelnlog(etWarning, infotext+ModuleName+ ' patching with ' + PatchList[i] + ' failed due to missing patch file ('+PatchFilePath+').', true);
        end;
      end;
    finally
      PatchList.Free;
    end;
  end
  else
  begin
    result:=true;
    infoln(infotext+'No ' + ModuleName + ' patches defined.',etInfo);
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

  FProcessEx:=TProcessEx.Create(nil);
  FProcessEx.OnErrorM := @LogError;

  FCPUCount  := GetLogicalCpuCount;

  FSVNClient := TSVNClient.Create;
  FGitClient := TGitClient.Create;
  FHGClient  := THGClient.Create;

  FShell := '';

  // List of binutils that can be downloaded:
  // CreateBinutilsList;
  FNeededExecutablesChecked:=false;
  FCleanModuleSuccess:=false;

  // Set up verbose log: will be done in dumpoutput
  // as it depends on verbosity etc
  //FLogVerbose: TLogger.Create;
  FErrorLog := TStringList.Create;
  FProcessEx.OnErrorM:=@(ProcessError);

  FCrossCPU_Target:='invalid';
  FCrossOS_Target:='invalid';
  FCrossOS_SubArch:='';

  FMajorVersion := -1;
  FMinorVersion := -1;
  FReleaseVersion := -1;
  FPatchVersion := -1;

  {$ifdef Linux}
  FMUSLLinker:='/lib/ld-musl-'+GetTargetCPU+'.so.1';
  FMUSL:=FileExists(FMUSLLinker);
  if FMUSL then infoln('Fpcupdeluxe: We have a MUSL Linux version !',etInfo);
  {$else}
  FMUSL:=false;
  {$endif}

  {$ifdef Solaris}
  FSolarisOI:=false;
  if FSolarisOI then infoln('Fpcupdeluxe: We have an OpenIndiana Solaris version !',etInfo);
  {$else}
  FSolarisOI:=false;
  {$endif}


end;

function TInstaller.GetFile(aURL,aFile:string; forceoverwrite:boolean=false; forcenative:boolean=false):boolean;
var
  aUseWget:boolean;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (GetFile): ';
  aUseWget:=FUseWget;
  if forcenative then aUseWget:=false;
  result:=((FileExists(aFile)) AND (NOT forceoverwrite) AND (FileSize(aFile)>0));
  if (NOT result) then
  begin
    if ((forceoverwrite) AND (SysUtils.FileExists(aFile))) then SysUtils.DeleteFile(aFile);
    infoln(localinfotext+'Downloading ' + aURL);
    result:=Download(aUseWget,aURL,aFile,FHTTPProxyHost,FHTTPProxyPort,FHTTPProxyUser,FHTTPProxyPassword);
    if (NOT result) then infoln(localinfotext+'Could not download file with URL ' + aURL +' into ' + ExtractFileDir(aFile) + ' (filename: ' + ExtractFileName(aFile) + ')');
  end;
end;

function TInstaller.GetFullVersion:dword;
begin
  result:=CalculateFullVersion(Self.FMajorVersion,Self.FMinorVersion,Self.FReleaseVersion);
end;

function TInstaller.GetDefaultCompilerFilename(const TargetCPU: string; Cross: boolean): string;
var
  aCPU:TCPU;
  s:string;
begin
  s:='fpc';
  if TargetCPU<>'' then
  begin
    for aCPU:=Low(TCPU) to High(TCPU) do
    begin
      if TargetCPU=CpuStr[aCPU] then
      begin
        if Cross then
          s:='ppcross'+ppcSuffix[aCPU]
        else
          s:='ppc'+ppcSuffix[aCPU];
        break;
      end;
    end;
  end;
  Result:=s+GetExeExt;
end;

function TInstaller.GetCompilerName(Cpu_Target:string):string;
begin
  result:=GetDefaultCompilerFilename(Cpu_Target,false);
end;

function TInstaller.GetCrossCompilerName(Cpu_Target:string):string;
begin
  if Cpu_Target<>'jvm'
     then result:=GetDefaultCompilerFilename(Cpu_Target,true)
     else result:=GetDefaultCompilerFilename(Cpu_Target,false);
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

