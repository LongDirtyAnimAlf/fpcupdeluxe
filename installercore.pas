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
    along with fpc(laz)up(deluxe).  If not, see <https://www.gnu.org/licenses/>
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  repoclient, GitClient, HGClient, SvnClient,
  processutils, m_crossinstaller,
  fpcuputil;

{$i revision.inc}

const
  DEFAULTFPCVERSION     = '3.0.4';
  DEFAULTLAZARUSVERSION = '2.0.6';

  FPCTRUNKVERSION       = '3.3.1';
  FPCTRUNKBOOTVERSION   = '3.0.4';
  LAZARUSTRUNKVERSION   = '2.1.0';

  DEFAULTFREEBSDVERSION = 11;

  LAZBUILDNAME          = 'lazbuild';

  MAKEFILENAME          = 'Makefile';
  FPCMAKEFILENAME       = MAKEFILENAME+'.fpc';

  FPCMAKECONFIG         = 'fpcmkcfg';

  QT5LIBNAME            = 'libQt5Pas.so.1';

  FPCPKGFILENAME        = 'fppkg';
  FPCPKGCONFIGFILENAME  = 'fppkg.cfg';

  FPFILENAME            = 'fp';
  FPCONFIGFILENAME      = 'fp.cfg';
  FPINIFILENAME         = 'fp.ini';

  FPCPKGCOMPILERTEMPLATE= 'default'; // fppkg default compiler template

  FPCCONFIGFILENAME     = 'fpc.cfg';

  SVNBASEHTTP           = 'https://svn.';
  SVNBASESVN            = 'svn://svn.';
  FTPBASEHTTP           = 'https://ftp.';
  FTPBASEFTP            = 'ftp://ftp.';

  FPCBASESVNURL         = SVNBASEHTTP+'freepascal.org';
  FTPBASEURL            = FTPBASEFTP+'freepascal.org/pub/';
  FPCFTPURL             = FTPBASEURL+'fpc/';
  LAZARUSFTPURL         = FTPBASEURL+'lazarus/';

  FPCFTPSNAPSHOTURL     = FPCFTPURL+'snapshot/';

  LAZARUSFTPSNAPSHOTURL = LAZARUSFTPURL+'snapshot/';

  BINUTILSURL           = FPCBASESVNURL + '/svn/fpcbuild';

  PACKAGESLOCATION      = 'packages.fppkg';
  PACKAGESCONFIGDIR     = 'fpcpkgconfig';
  //PACKAGESCONFIGDIR     = PACKAGESLOCATION+DirectorySeparator+'fpcpkgconfig';

  REVINCFILENAME        = 'revision.inc';

  {$IFDEF WINDOWS}
  //FPC prebuilt binaries of the GNU Binutils
  PREBUILTBINUTILSURL      = BINUTILSURL + '/binaries/i386-win32';
  PREBUILTBINUTILSURLWINCE = BINUTILSURL + '/tags/release_3_0_4/install/crossbinwce';
  {$ENDIF}

  LAZARUSBINARIES = FPCBASESVNURL + '/svn/lazarus/binaries';

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
  OpenSSLSourceURL : array [0..5] of string = (
    'https://indy.fulgan.com/SSL/openssl-1.0.2u-x64_86-win64.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2t-x64_86-win64.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2r-x64_86-win64.zip',
    'http://wiki.overbyte.eu/arch/openssl-1.0.2r-win64.zip',
    'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win64.zip',
    'https://indy.fulgan.com/SSL/Archive/openssl-1.0.2p-x64_86-win64.zip'
    );
  {$endif}
  {$ifdef win32}
  OpenSSLSourceURL : array [0..5] of string = (
    'https://indy.fulgan.com/SSL/openssl-1.0.2u-i386-win32.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2t-i386-win32.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2r-i386-win32.zip',
    'http://wiki.overbyte.eu/arch/openssl-1.0.2r-win32.zip',
    'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win32.zip',
    'https://indy.fulgan.com/SSL/Archive/openssl-1.0.2p-i386-win32.zip'
    );
  {$endif}

  Seriousness: array [TEventType] of string = ('custom:', 'info:', 'WARNING:', 'ERROR:', 'debug:');

  SnipMagicBegin='# begin fpcup do not remove '; //look for this/add this in fpc.cfg cross-compile snippet. Note: normally followed by FPC CPU-os code
  SnipMagicEnd='# end fpcup do not remove'; //denotes end of fpc.cfg cross-compile snippet
  FPCSnipMagic='# If you don''t want so much verbosity use'; //denotes end of standard fpc.cfg

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

  _CREATESCRIPT            = 'CreateScript';
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
  _FPCREMOVEONLY           = _FPC+_UNINSTALL+_ONLY;
  _LAZARUSCLEANBUILDONLY   = _LAZARUS+_CLEAN+_BUILD+_ONLY;
  _LAZARUSREMOVEONLY       = _LAZARUS+_UNINSTALL+_ONLY;
  _LCLALLREMOVEONLY        = _LCL+'ALL'+_CLEAN+_ONLY;
  _LCLREMOVEONLY           = _LCL+_CLEAN+_ONLY;
  _COMPONENTSREMOVEONLY    = _COMPONENTS+_CLEAN+_ONLY;
  _PACKAGERREMOVEONLY      = _PACKAGER+_CLEAN+_ONLY;

  _HELP                    = 'Help';
  _HELPFPC                 = _HELP+_FPC;
  _HELPLAZARUS             = _HELP+_LAZARUS;
  _LHELP                   = 'lhelp';

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
  TARMARCH  = (default,armel,armeb,armhf);

  //TTargetSet=array[tcpu,tos] of boolean;

const
  ppcSuffix : array[TCPU] of string=(
    'none','386','x64','arm','a64','ppc','ppc64', 'mips', 'mipsel','avr','jvm','8086','sparc','sparc64','rv32','rv64','68k'
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

  //TBaseFPCInstaller = class;
  //TBaseLazarusInstaller = class;

  TInstaller = class(TObject)
  private
    FKeepLocalChanges: boolean;
    FReApplyLocalChanges: boolean;
    FCrossInstaller:TCrossInstaller;
    FCrossCPU_Target: TCPU; //When cross-compiling: CPU, e.g. x86_64
    FCrossOS_Target: TOS; //When cross-compiling: OS, e.g. win64
    FCrossOS_SubArch: string; //When cross-compiling for embedded: CPU, e.g. for Teensy SUBARCH=ARMV7EM
    procedure SetURL(value:string);
    procedure SetSourceDirectory(value:string);
    function GetShell: string;
    function GetMake: string;
    procedure SetVerbosity(aValue:boolean);
    procedure SetHTTPProxyHost(AValue: string);
    procedure SetHTTPProxyPassword(AValue: string);
    procedure SetHTTPProxyPort(AValue: integer);
    procedure SetHTTPProxyUser(AValue: string);
    function DownloadFromBase(aClient:TRepoClient; ModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Get fpcup registred cross-compiler, if any, if not, return nil
    function GetCrossInstaller: TCrossInstaller;
    function GetCrossCompilerPresent:boolean;
    function GetFullVersion:dword;
    function GetDefaultCompilerFilename(const TargetCPU: TCPU; Cross: boolean): string;
    function GetInstallerClass(aClassToFind:TClass):boolean;
    function IsFPCInstaller:boolean;
    function IsLazarusInstaller:boolean;
    function IsUniversalInstaller:boolean;
  protected
    FBinPath: string; //path where compiler lives
    FCleanModuleSuccess: boolean;
    FBaseDirectory: string; //Base directory for fpc(laz)up(deluxe) install itself
    FSourceDirectory: string; //Top source directory for a product (FPC, Lazarus)
    FInstallDirectory: string; //Top install directory for a product (FPC, Lazarus)
    FTempDirectory: string; //For storing temp files and logs
    FCompiler: string; // Compiler executable
    FCompilerOptions: string; //options passed when compiling (FPC or Lazarus currently)
    FCPUCount: integer; //logical cpu count (i.e. hyperthreading=2cpus)
    FCrossOPT: string; //options passed (only) when cross-compiling
    FCrossToolsDirectory: string;
    FCrossLibraryDirectory: string;
    FPreviousRevision: string;
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
    FSoftFloat: boolean;
    FOnlinePatching: boolean;
    FVerbose: boolean;
    FUseWget: boolean;
    FTar: string;
    FBunzip2: string;
    F7zip: string;
    FWget: string;
    FUnrar: string;
    //FGit: string;
    FExternalTool: TExternalTool;
    FExternalToolResult: integer;
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
    function DownloadFromHG(ModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Clone/update using Git; use FSourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromGit(ModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Checkout/update using SVN; use FSourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromSVN(ModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    function SimpleExportFromSVN(ModuleName: string; aFileURL,aLocalPath:string): boolean;
    // Download SVN client and set FSVNClient.SVNExecutable if succesful.
    function DownloadFromFTP(ModuleName: string): boolean;
    // Clone/update using Git; use FSourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
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
    // Looks for SVN client in subdirectories and sets FSVNClient.SVNExecutable if found.
    {$IFDEF MSWINDOWS}
    function FindSVNSubDirs: boolean;
    {$ENDIF}
    // Finds compiler in fpcdir path if TFPCInstaller descendant
    function GetCompiler: string;
    // Returns CPU-OS in the format used by the FPC bin directory, e.g. x86_64-win64:
    function GetFPCTarget(Native: boolean): string;
    // Sets the search/binary path to NewPath or adds NewPath before or after existing path:
    procedure SetPath(NewPath: string; Prepend: boolean; Append: boolean);
    // Get currently set path
    function GetPath: string;
    function GetFile(aURL,aFile:string; forceoverwrite:boolean=false; forcenative:boolean=false):boolean;
    function GetSanityCheck:boolean;

    function {%H-}GetVersionFromSource(aSourcePath:string):string;virtual;abstract;
    function {%H-}GetVersionFromURL(aUrl:string):string;virtual;abstract;
    function {%H-}GetReleaseCandidateFromSource(aSourcePath:string):integer;virtual;abstract;
    function GetVersion:string;

  public
    InfoText: string;
    LocalInfoText: string;
    property SVNClient: TSVNClient read FSVNClient;
    property GitClient: TGitClient read FGitClient;
    property HGClient: THGClient read FHGClient;
    // Get processor for termination of running processes
    property Processor: TExternalTool read FExternalTool;
    property ProcessorResult: integer read FExternalToolResult write FExternalToolResult;
    // Source directory for installation (fpcdir, lazdir,... option)
    property SourceDirectory: string write SetSourceDirectory;
    //Base directory for fpc(laz)up(deluxe) itself
    property BaseDirectory: string write FBaseDirectory;
    // Source directory for installation (fpcdir, lazdir,... option)
    property InstallDirectory: string write FInstallDirectory;
    //Base directory for fpc(laz)up(deluxe) itself
    property TempDirectory: string write FTempDirectory;
    // Compiler to use for building. Specify empty string when using bootstrap compiler.
    property Compiler: string {read GetCompiler} write FCompiler;
    // Compiler options passed on to make as OPT= or FPCOPT=
    property CompilerOptions: string write FCompilerOptions;
    // SubArch for target embedded
    property CrossOS_SubArch: string read FCrossOS_SubArch;
    // Options for cross compiling. User can specify his own, but cross compilers can set defaults, too
    property CrossOPT: string read FCrossOPT write FCrossOPT;
    property CrossToolsDirectory:string read FCrossToolsDirectory write FCrossToolsDirectory;
    property CrossLibraryDirectory:string read FCrossLibraryDirectory write FCrossLibraryDirectory;
    // SVN revision override. Default is HEAD/latest revision
    property PreviousRevision: string read FPreviousRevision;
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
    property SoftFloat: boolean write FSoftFloat;
    property OnlinePatching: boolean write FOnlinePatching;
    // display and log in temp log file all sub process output
    property Verbose: boolean write SetVerbosity;
    // use wget as downloader ??
    property UseWget: boolean write FUseWget;
    // get cross-installer
    property CrossInstaller:TCrossInstaller read GetCrossInstaller;
    property CrossCompilerPresent: boolean read GetCrossCompilerPresent;
    property NumericalVersion:dword read GetFullVersion;
    property SanityCheck:boolean read GetSanityCheck;
    function GetCompilerName(Cpu_Target:TCPU):string;overload;
    function GetCompilerName(Cpu_Target:string):string;overload;
    function GetCrossCompilerName(Cpu_Target:TCPU):string;
    procedure SetTarget(aCPU:TCPU;aOS:TOS;aSubArch:string);virtual;
    // append line ending and write to log and, if specified, to console
    procedure WritelnLog(msg: string; ToConsole: boolean = true);overload;
    procedure WritelnLog(EventType: TEventType; msg: string; ToConsole: boolean = true);overload;
    function GetSuitableRepoClient:TRepoClient;
    function GetTempFileNameExt(Prefix,Ext : String) : String;
    function GetTempDirName(Prefix: String='fpcup') : String;
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
    function PatchModule(ModuleName: string): boolean;
    function CreateRevision(ModuleName,aRevision:string): boolean;
    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; virtual;
    procedure infoln(Message: string; const Level: TEventType=etInfo);
    function ExecuteCommand(Commandline: string; Verbosity:boolean): integer; overload;
    function ExecuteCommand(Commandline: string; out Output:string; Verbosity:boolean): integer; overload;
    function ExecuteCommandInDir(Commandline, Directory: string; Verbosity:boolean): integer; overload;
    function ExecuteCommandInDir(Commandline, Directory: string; out Output:string; Verbosity:boolean): integer; overload;
    function ExecuteCommandInDir(Commandline, Directory: string; out Output:string; PrependPath: string; Verbosity:boolean): integer; overload;
    constructor Create;
    destructor Destroy; override;
  end;

  TBaseUniversalInstaller = class(TInstaller);
  TBaseFPCInstaller = class(TInstaller);
  TBaseLazarusInstaller = class(TInstaller);
  TBaseHelpInstaller = class(TInstaller);
  TBaseWinInstaller = class(TInstaller);

implementation

uses
  {$ifdef LCL}
  //For messaging to MainForm: no writeln
  Forms,
  //LMessages,
  LCLIntf,
  {$endif}
  FileUtil,
  process
  {$IFDEF UNIX}
  ,LazFileUtils
  {$ENDIF UNIX}
  {$IF NOT DEFINED(HAIKU) AND NOT DEFINED(AROS) AND NOT DEFINED(MORPHOS)}
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
  target := GetFPCTarget(false);
  if (NOT Assigned(FCrossInstaller)) OR (FCrossInstaller.RegisterName<>target) then
  begin
    FCrossInstaller:=nil;
    if assigned(CrossInstallers) then
      for idx := 0 to Pred(CrossInstallers.Count) do
        if CrossInstallers[idx] = target then
        begin
          FCrossInstaller:=TCrossInstaller(CrossInstallers.Objects[idx]);
          break;
        end;
  end;
  result:=FCrossInstaller;
end;

function TInstaller.GetCrossCompilerPresent:boolean;
var
  FPCCfg,aDir,s   : string;
  ConfigText      : TStringList;
  SnipBegin,i     : integer;
  aCPU,aOS,aArch  : string;
begin
  result:=false;

  if (NOT DirectoryExists(FInstallDirectory)) then exit;
  if CheckDirectory(FInstallDirectory) then exit;

  if ((FCrossCPU_Target=TCPU.cpuNone) OR (FCrossOS_Target=TOS.osNone)) then exit;

  //if (Self is TFPCCrossInstaller) then
  begin
    // check for existing cross-dirs
    if (NOT result) then
    begin
      aDir:=ConcatPaths([FInstallDirectory,'bin',GetFPCTarget(false)]);
      result:=DirectoryExists(aDir);
    end;
    if (NOT result) then
    begin
      aDir:=ConcatPaths([FInstallDirectory,'units',GetFPCTarget(false)]);
      {$ifdef UNIX}
      if FileIsSymlink(aDir) then
      begin
        try
          aDir:=GetPhysicalFilename(aDir,pfeException);
        except
        end;
      end;
      {$endif}
      result:=DirectoryExists(aDir);
    end;

    if result then exit;

    // Check FPC config-file

    aCPU:='';
    aOS:='';
    aArch:='';

    FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + FPCCONFIGFILENAME;

    if (NOT FileExists(FPCCfg)) then exit;

    ConfigText:=TStringList.Create;
    try
      ConfigText.LoadFromFile(FPCCFG);
      SnipBegin:=0;
      while (SnipBegin<ConfigText.Count) do
      begin
        if Pos(SnipMagicBegin,ConfigText.Strings[SnipBegin])>0 then
        begin
          s:=ConfigText.Strings[SnipBegin];
          Delete(s,1,Length(SnipMagicBegin));
          i:=Pos('-',s);
          if i>0 then
          begin
            aCPU:=Copy(s,1,i-1);
            aOS:=Trim(Copy(s,i+1,MaxInt));
            // try to distinguish between different ARM CPU versons ... very experimental and [therefor] only for Linux
            if (UpperCase(aCPU)='ARM') AND (UpperCase(aOS)='LINUX') then
            begin
              for i:=SnipBegin to SnipBegin+5 do
              begin
                if Pos('#IFDEF CPU',ConfigText.Strings[i])>0 then
                begin
                  s:=ConfigText.Strings[i];
                  Delete(s,1,Length('#IFDEF CPU'));
                  aArch:=s;
                  break;
                end;
              end;
            end;
          end;
        end;
        Inc(SnipBegin);
      end;

      result:=((GetCPU(FCrossCPU_Target)=aCPU) AND (GetOS(FCrossOS_Target)=aOS));

    finally
      ConfigText.Free;
    end;
  end;

  //if (Self is TLazarusCrossInstaller) then
  begin
    if (NOT result) then
    begin
      aDir:=ConcatPaths([FInstallDirectory,'lcl','units',GetFPCTarget(false)]);
      {$ifdef UNIX}
      if FileIsSymlink(aDir) then
      begin
        try
          aDir:=GetPhysicalFilename(aDir,pfeException);
        except
        end;
      end;
      {$endif}
      result:=DirectoryExists(aDir);
    end;
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
      FShell := GetEnvironmentVariable('COMSPEC');
      //ExecuteCommand('cmd.exe /C echo %COMSPEC%', output, False);
      //FShell := Trim(output);
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

procedure TInstaller.SetVerbosity(aValue:boolean);
begin
  FVerbose:=aValue;
  if Assigned(Processor) then Processor.Verbose:=FVerbose;
  if Assigned(FSVNClient) then FSVNClient.Verbose:=FVerbose;
  if Assigned(FGitClient) then FGitClient.Verbose:=FVerbose;
  if Assigned(FHGClient) then FHGClient.Verbose:=FVerbose;
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
    aURL:=BINUTILSURL+'/tags/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw32/';

    OperationSucceeded:=false;
    aLocalClientBinary:=FPatchCmd;
    if Not FileExists(aLocalClientBinary) then
      aLocalClientBinary:=IncludeTrailingPathDelimiter(FMakeDir) + FPatchCmd;
    if Not FileExists(aLocalClientBinary) then
    begin
      if FSVNClient.ValidClient then
        OperationSucceeded:=SimpleExportFromSVN('CheckAndGetTools',aURL+'patch.exe',ExcludeTrailingPathDelimiter(FMakeDir));
      if (NOT OperationSucceeded) then
        OperationSucceeded:=GetFile(aURL+'patch.exe',IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe');

      OperationSucceeded:=false;
      if FSVNClient.ValidClient then
        OperationSucceeded:=SimpleExportFromSVN('CheckAndGetTools',aURL+'patch.exe.manifest',ExcludeTrailingPathDelimiter(FMakeDir));
      if (NOT OperationSucceeded) then
        OperationSucceeded:=GetFile(aURL+'patch.exe.manifest',IncludeTrailingPathDelimiter(FMakeDir) + 'patch.exe.manifest');
    end;

    // do not fail
    OperationSucceeded:=True;

    F7zip:=Which('7z');
    if Not FileExists(F7zip) then Which('7za');
    if Not FileExists(F7zip) then F7zip := IncludeTrailingPathDelimiter(FMakeDir) + '\7Zip\7za.exe';
    if Not FileExists(F7zip) then
    begin
      ForceDirectoriesSafe(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip');
      // this version of 7Zip is the last version that does not need installation ... so we can silently get it !!
      Output:='7za920.zip';
      SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
      aURL:='https://downloads.sourceforge.net/project/sevenzip/7-Zip/9.20/';
      OperationSucceeded:=GetFile(aURL+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
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
        // try another URL
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
        aURL:='https://osdn.net/frs/redir.php?m=acc&f=sevenzip%2F64455%2F';
        OperationSucceeded:=GetFile(aURL+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
      end;
      if NOT OperationSucceeded then
      begin
        // try one more time on different URL
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
        aURL:='http://7-zip.org/a/';
        OperationSucceeded:=GetFile(aURL+Output,IncludeTrailingPathDelimiter(FMakeDir)+'7Zip\'+Output);
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
      SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
      aURL:='https://downloads.sourceforge.net/project/gnuwin32/unrar/3.4.3/';
      OperationSucceeded:=GetFile(aURL+Output,IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
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
        aURL:='https://downloads.sourceforge.net/project/gnuwin32/unrar/3.4.3/';
        OperationSucceeded:=GetFile(aURL+Output,IncludeTrailingPathDelimiter(FMakeDir)+'unrar\'+Output);
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
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.24.1.windows.2/MinGit-2.24.1.2-32-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.25.1.windows.1/MinGit-2.25.1-32-bit.zip';
          {$else}
          //Output:='git64.7z';
          Output:='git64.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.17.1.windows.2/MinGit-2.17.1.2-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.18.0.windows.1/MinGit-2.18.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.19.0.windows.1/MinGit-2.19.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.21.0.windows.1/MinGit-2.21.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.22.0.windows.1/MinGit-2.22.0-64-bit.zip';
          aURL:='https://github.com/git-for-windows/git/releases/download/v2.23.0.windows.1/MinGit-2.23.0-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.24.1.windows.2/MinGit-2.24.1.2-64-bit.zip';
          //aURL:='https://github.com/git-for-windows/git/releases/download/v2.25.1.windows.1/MinGit-2.25.1-64-bit.zip';
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
              //Copy certificate ... might be necessary
              //aURL:=IncludeTrailingPathDelimiter(FMakeDir)+'git\mingw32\';
              //if (NOT FileExists(aURL+'bin\curl-ca-bundle.crt')) then FileUtil.CopyFile(aURL+'ssl\certs\ca-bundle.crt',aURL+'bin\curl-ca-bundle.crt');
            end;
          end;
          if OperationSucceeded then RepoExecutable:=aLocalClientBinary else RepoExecutable:=RepoExecutableName+'.exe';
        end;
      end;
      if RepoExecutable <> EmptyStr then
      begin
        // check exe, but do not fail: GIT is not 100% essential !
        CheckExecutable(RepoExecutable, ['--version'], '');
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
        CheckExecutable(RepoExecutable, ['--version'], '');
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
        OperationSucceeded := CheckExecutable(FBunzip2, ['--help'], '');
        if (NOT OperationSucceeded) then infoln(localinfotext+FBunzip2+' not found.',etDebug);
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid tar executable, if it is needed
      if FTar <> EmptyStr then
      begin
        OperationSucceeded := CheckExecutable(FTar, ['--version'], '');
        if (NOT OperationSucceeded) then infoln(localinfotext+FTar+' not found.',etDebug);
      end;
    end;

    {$IFNDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      OperationSucceeded := CheckExecutable(Make, ['-v'], '');
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
  s1,s2,s3: string;
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

    {$IF (defined(UNIX)) and (not defined(Darwin))}
    // Check for proper ld executable
    if OperationSucceeded then
    try
      s3:=Which('ld');
      ExecuteCommand(s3+ ' -v', s1, False);
      if AnsiPos('GNU ld', s1) = 0 then
      begin
        ExecuteCommand(s3 + ' -v', s1, True);
        infoln(s2+'Found ld binary here: '+s3+'. But it is not GNU ld. Expect errors',etWarning);
        s3:=Which('ld.bfd');
        ExecuteCommand(s3+ ' -v', s1, False);
        if AnsiPos('GNU ld', s1) = 1 then
        begin
          infoln(s2+'Found GNU ld.bfd binary here: '+s3+'. Could be used through symlinking.',etWarning);
        end;
        OperationSucceeded := true;
      end
      else
      begin
        infoln(s2+'Found GNU ld binary here: '+s1+'.',etInfo);
      end;
    except
      // ignore errors, this is only an extra check
    end;
    {$ENDIF UNIX}
  end;

  Result := OperationSucceeded;
end;

procedure TInstaller.CreateBinutilsList(aVersion:string);
// Windows-centric
const
  //SourceURL_gdb = LAZARUSBINARIES+'/i386-win32/gdb/bin/';
  //SourceURL_gdb = 'https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Alternative%20GDB/GDB%208.1/gdb.exe/download';
  //SourceURL_gdbserver = 'https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Alternative%20GDB/GDB%208.1/gdbserver.exe/download';
  SourceURL_gdb = FPCUPGITREPO+'/releases/download/gdb/';
  //SourceURL64_gdb = LAZARUSBINARIES+'/x86_64-win64/gdb/bin/';
  SourceURL64_gdb = FPCUPGITREPO+'/releases/download/gdb/';
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
    if (CalculateNumericalVersion(aVersion)<CalculateFullVersion(2,4,0)) then
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
  //AddNewUtil('gdb' + GetExeExt,SourceURL_gdb,'',ucDebugger32);
  //AddNewUtil('gdb' + GetExeExt,SourceURL64_gdb,'',ucDebugger64);
  //AddNewUtil('libiconv-2.dll',SourceURL64_gdb,'',ucDebugger64);

  // add win32/64 gdb from fpcup
  AddNewUtil('i386-win32-gdb.zip',SourceURL_gdb,'',ucDebugger32);
  AddNewUtil('x86_64-win64-gdb.zip',SourceURL64_gdb,'',ucDebugger64);

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
  https://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
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
  //AddNewUtil('libiconv-2.dll',SourceURL64_gdb,'',ucBinutil);
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
  AddNewUtil('gdb-6.4-win32-arm-wince.zip',FPCFTPURL+'contrib/cross/','',ucDebuggerWince);

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

function TInstaller.DownloadFromBase(aClient:TRepoClient; ModuleName: string; var aBeforeRevision,
  aAfterRevision: string; UpdateWarnings: TStringList): boolean;
var
  ReturnCode: integer;
  DiffFile,DiffFileCorrectedPath: String;
  LocalPatchCmd : string;
  DiffFileSL:TStringList;
  Output:string;
begin
  Result := false;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' ('+Copy(aClient.ClassName,2,MaxInt)+': '+ModuleName+'): ';

  // check if we do have a client !!
  if NOT aClient.ValidClient then
  begin
    infoln(localinfotext+aClient.RepoExecutableName+' is needed, but cannot be found on the system !!',etWarning);
    exit;
  end;

  aBeforeRevision := 'failure';
  aAfterRevision := 'failure';
  aClient.LocalRepository := FSourceDirectory;
  aClient.Repository      := FURL;
  aClient.ExportOnly      := FExportOnly;
  aClient.Verbose         := FVerbose;

  aBeforeRevision:=aClient.LocalRevision;

  if ((ModuleName=_FPC) OR (ModuleName=_LAZARUS)) AND (aClient is TGitClient)  then
  begin
    Output:=(aClient as TGitClient).GetSVNRevision;
    if (Length(Output)>0) then
    begin
      aBeforeRevision := Output;
    end;
  end;

  if aBeforeRevision<>FRET_UNKNOWN_REVISION then
  begin
    aClient.LocalModifications(UpdateWarnings); //Get list of modified files
    if UpdateWarnings.Count > 0 then
    begin
      UpdateWarnings.Insert(0, ModuleName + ': WARNING: found modified files.');
      if FKeepLocalChanges=false then
      begin
        DiffFile:=IncludeTrailingPathDelimiter(FSourceDirectory) + 'REV' + aBeforeRevision + '.diff';
        CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,aClient);
        UpdateWarnings.Add(ModuleName + ': reverting to original before updating.');
        aClient.Revert; //Remove local changes
      end else UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
    end;
  end;

  aClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  aClient.DesiredBranch := FDesiredBranch; //We want to update to this specific branch
  Output:=localinfotext+'Running '+UpperCase(aClient.RepoExecutableName)+' checkout or update';
  if Length(aClient.DesiredRevision)>0 then
    Output:=Output+' of revision '+aClient.DesiredRevision;
  Output:=Output+'.';
  infoln(Output,etInfo);


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
    AbortedExitCode:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog(etError, localinfotext+'Download aborted.', true);
    end;
    else
    begin
      // For now, assume it worked even with non-zero result code. We can because
      // we do the AfterRevision check as well.
      Result := true;

      if FExportOnly then
        aAfterRevision := FDesiredRevision
      else
      begin
        aAfterRevision := aClient.LocalRevision;
        if ((ModuleName=_FPC) OR (ModuleName=_LAZARUS)) AND (aClient is TGitClient)  then
        begin
          Output:=(aClient as TGitClient).GetSVNRevision;
          if (Length(Output)>0) then
          begin
            aAfterRevision:=Output;
          end;
        end;
      end;

      if (aClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (aBeforeRevision <> aClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      if FReApplyLocalChanges and (DiffFile<>'') then
      begin
         Output:='';

         UpdateWarnings.Add(ModuleName + ': reapplying local changes.');

         // check for default values
         if ((FPatchCmd='patch'+GetExeExt) OR (FPatchCmd='gpatch'+GetExeExt))
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

             DiffFileCorrectedPath:=IncludeTrailingPathDelimiter(GetTempDirName)+ExtractFileName(DiffFile);
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

function TInstaller.DownloadFromHG(ModuleName: string; var aBeforeRevision,
  aAfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  result:=DownloadFromBase(FHGClient,ModuleName,aBeforeRevision,aAfterRevision,UpdateWarnings);
end;

function TInstaller.DownloadFromGit(ModuleName: string; var aBeforeRevision,
  aAfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  result:=DownloadFromBase(FGitClient,ModuleName,aBeforeRevision,aAfterRevision,UpdateWarnings);
end;

function TInstaller.DownloadFromSVN(ModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
var
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

  aBeforeRevision              := 'failure';
  aAfterRevision               := 'failure';
  FSVNClient.ModuleName        := ModuleName;
  FSVNClient.LocalRepository   := FSourceDirectory;
  FSVNClient.Repository        := FURL;
  FSVNClient.ExportOnly        := FExportOnly;
  FSVNClient.Verbose           := FVerbose;

  RepoExists:=FSVNClient.LocalRepositoryExists;
  if RepoExists then
  begin
    if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
      aBeforeRevision := FSVNClient.LocalRevisionWholeRepo
    else
      aBeforeRevision := FSVNClient.LocalRevision;
  end
  else
  begin
    // We could insist on the repo existing, but then we wouldn't be able to checkout!!
    writelnlog('Directory ' + FSourceDirectory + ' is not an SVN repository (or a repository with the wrong remote URL).');
    if not(DirectoryExists(FSVNClient.LocalRepository)) then
    begin
      writelnlog(localinfotext+'Creating directory '+FSVNClient.LocalRepository+' for SVN checkout.');
      ForceDirectoriesSafe(FSVNClient.LocalRepository);
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
        DiffFile:=IncludeTrailingPathDelimiter(FSourceDirectory) + 'REV' + aBeforeRevision + '.diff';
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

  Output:=localinfotext+'Running '+UpperCase(FSVNClient.RepoExecutableName)+' checkout or update';
  if Length(FSVNClient.DesiredRevision)>0 then
    Output:=Output+' of revision '+FSVNClient.DesiredRevision;
  Output:=Output+'.';
  infoln(Output,etInfo);

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
    AbortedExitCode:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog(etError, localinfotext+'Download aborted.', true);
    end;
    else
    begin
      // If there are svn errors, return a false result.
      // We used to do a check for the revision, but that does not check the integrity
      // or existence of all files in the svn repo.

      if FExportOnly then
      begin
        aAfterRevision := FDesiredRevision;
        if Trim(aAfterRevision)='' then aAfterRevision := FSVNClient.LocalRevisionWholeRepo;
      end
      else
      begin
        if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
          aAfterRevision := FSVNClient.LocalRevisionWholeRepo
        else
          aAfterRevision := FSVNClient.LocalRevision;
      end;

      if (FSVNClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (aBeforeRevision <> FSVNClient.LocalRevision) then
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
        Output:='';

        UpdateWarnings.Add(ModuleName + ': reapplying local changes.');

        if ((FPatchCmd='patch'+GetExeExt) OR (FPatchCmd='gpatch'+GetExeExt))
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
              if ((FPatchCmd='patch'+GetExeExt) OR (FPatchCmd='gpatch'+GetExeExt))
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

function TInstaller.SimpleExportFromSVN(ModuleName: string; aFileURL,aLocalPath:string): boolean;
begin
  result:=false;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (SimpleExportFromSVN: '+ModuleName+'): ';

  // check if we do have a client !!
  if NOT FSVNClient.ValidClient then
  begin
    infoln(localinfotext+FSVNClient.RepoExecutableName+' is needed, but cannot be found on the system !!',etWarning);
    exit;
  end;

  FSVNClient.ModuleName       := ModuleName;
  FSVNClient.LocalRepository  := aLocalPath;
  FSVNClient.Repository       := aFileURL;
  FSVNClient.ExportOnly       := true;
  FSVNClient.DesiredRevision  := '';
  FSVNClient.DesiredBranch    := '';

  if (Length(FSVNClient.LocalRepository)>0) then
  begin
    if not(DirectoryExists(FSVNClient.LocalRepository)) then
    begin
      writelnlog(localinfotext+'Creating directory '+FSVNClient.LocalRepository+' for SVN checkout/export.');
      ForceDirectoriesSafe(FSVNClient.LocalRepository);
    end;
    FSVNClient.CheckOutOrUpdate;
    result:=(FSVNClient.ReturnCode=0);
  end
  else
  begin
    //only report validity of remote URL
    result:=FSVNClient.CheckURL;
  end;

end;

function TInstaller.DownloadFromFTP(ModuleName: string): boolean;
var
  i:integer;
  FilesList:TStringList;
  FPCArchive,aName,aFile:string;
begin
  result:=false;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadFromFTP: '+ModuleName+'): ';

  if (NOT DirectoryIsEmpty(ExcludeTrailingPathDelimiter(FSourceDirectory))) then
  begin
    infoln(localinfotext+ModuleName+' sources are already there.',etWarning);
    infoln(localinfotext+ModuleName+' build-process will continue with existing sources.',etWarning);
    exit(true);
  end;

  infoln(localinfotext+'Getting '+ModuleName+' sources.',etInfo);

  FPCArchive := GetTempFileNameExt('FPCUPTMP','zip');
  result:=GetFile(FURL,FPCArchive,true);
  if (result AND (NOT FileExists(FPCArchive))) then result:=false;

  if result then
  begin
    //Delete existing files from source directory
    //DeleteDirectory(FSourceDirectory,True);

    with TNormalUnzipper.Create do
    begin
      try
        result:=DoUnZip(FPCArchive,FSourceDirectory,[]);
      finally
        Free;
      end;
    end;
  end;

  if result then
  begin
    aName:='';
    FilesList:=FindAllDirectories(FSourceDirectory,False);
    if FilesList.Count=1 then aName:=FilesList[0];
    FreeAndNil(FilesList);
    if Pos(LowerCase(ModuleName),LowerCase(ExtractFileName(aName)))>0 then
    //if LowerCase(ExtractFileName(aName))=LowerCase(ModuleName) then
    begin
      infoln(infotext+'Moving files due to extra path. Please wait.',etInfo);
      FilesList:=FindAllFiles(aName, '', True);
      for i:=0 to (FilesList.Count-1) do
      begin
        aFile:=FilesList[i];
        aFile:=StringReplace(aFile,aName,aName+DirectorySeparator+'..',[]);
        aFile:=SafeExpandFileName(aFile);
        if NOT DirectoryExists(ExtractFileDir(aFile)) then ForceDirectoriesSafe(ExtractFileDir(aFile));
        SysUtils.RenameFile(FilesList[i],aFile);
      end;
      DeleteDirectory(aName,False);
      FreeAndNil(FilesList);
    end;
  end;

  SysUtils.Deletefile(FPCArchive); //Get rid of temp file.

end;

{$IFDEF MSWINDOWS}
function TInstaller.DownloadBinUtils: boolean;
var
  Counter: integer;
  Errors: integer = 0;
  DownloadSuccess:boolean;
  InstallPath:string;
  RemotePath:string;
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

      if (FileExists(InstallPath+FUtilFiles[Counter].FileName)) then continue;

      RemotePath:=FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName;

      DownloadSuccess:=false;

      // FPC owned binutils are always served by SVN, so use SVN client and related.
      if (FSVNClient.ValidClient) AND (Pos(FPCBASESVNURL,RemotePath)>0) then
      begin
        //first check remote URL
        DownloadSuccess:=SimpleExportFromSVN('DownloadBinUtils',RemotePath,'');
        if DownloadSuccess then
        begin
          DownloadSuccess:=SimpleExportFromSVN('DownloadBinUtils',RemotePath,InstallPath);
          DownloadSuccess:=DownloadSuccess AND FileExists(InstallPath+FUtilFiles[Counter].FileName);
        end;
      end;

      if (NOT DownloadSuccess) then
      begin
        infoln(localinfotext+'Downloading: ' + FUtilFiles[Counter].FileName + ' with SVN failed. Now trying normal download.',etInfo);
        DownloadSuccess:=GetFile(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,InstallPath+FUtilFiles[Counter].FileName);
      end;

      if NOT DownloadSuccess then
      begin
        infoln(localinfotext+'Error downloading binutil: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + '.',etError);
        Inc(Errors);
      end
      else
      begin
        infoln(localinfotext+'Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + ' success.',etInfo);

        if ExtractFileExt(FUtilFiles[Counter].FileName)='.zip' then
        begin
          with TNormalUnzipper.Create do
          begin
            try
              if DoUnZip(InstallPath+FUtilFiles[Counter].FileName,InstallPath,[]) then
                infoln(localinfotext+'Unpacking: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + ' success.',etInfo);
            finally
              Free;
            end;
          end;
        end;

      end;
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

  SVNZip := GetTempFileNameExt('FPCUPTMP','zip');

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
  OpenSSLFileName,aSourceURL: string;
  i:integer;
begin
  result:=false;
  OperationSucceeded := false;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadOpenSSL): ';

  infoln(localinfotext+'No OpenSLL library files available for SSL. Going to download them.',etWarning);


  //if (NOT CheckWin32Version(6,2)) then
  begin
    if FSVNClient.ValidClient then
    begin
      OpenSSLFileName:='libeay32.dll';
      // First check remote URL
      OperationSucceeded:=SimpleExportFromSVN('DownloadOpenSSL',OPENSSL_URL_LATEST_SVN+'/'+OpenSSLFileName,'');
      // Perform download
      if OperationSucceeded then
        OperationSucceeded:=SimpleExportFromSVN('DownloadOpenSSL',OPENSSL_URL_LATEST_SVN+'/'+OpenSSLFileName,SafeGetApplicationPath);

      OpenSSLFileName:='ssleay32.dll';
      // First check remote URL
      if OperationSucceeded then
        OperationSucceeded:=SimpleExportFromSVN('DownloadOpenSSL',OPENSSL_URL_LATEST_SVN+'/'+OpenSSLFileName,'');
      // Perform download
      if OperationSucceeded then
        OperationSucceeded:=SimpleExportFromSVN('DownloadOpenSSL',OPENSSL_URL_LATEST_SVN+'/'+OpenSSLFileName,SafeGetApplicationPath);
    end;

    if OperationSucceeded
       then infoln(localinfotext+'SVN OpenSLL library files download from '+OPENSSL_URL_LATEST_SVN+' ok.',etWarning)
  end;

  if (NOT OperationSucceeded) then
  begin
    OpenSSLFileName := GetTempFileNameExt('FPCUPTMP','zip');

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
  end;

  // Real last resort: direct download OpenSSL from from Lazarus binaries
  if (NOT OperationSucceeded) AND (NOT FSVNClient.ValidClient) then
  begin
    OpenSSLFileName:='libeay32.dll';
    OperationSucceeded:=GetFile(OPENSSL_URL_LATEST_SVN+'/'+OpenSSLFileName,SafeGetApplicationPath+OpenSSLFileName,true,true);
    if OperationSucceeded then
    begin
      OpenSSLFileName:='ssleay32.dll';
      OperationSucceeded:=GetFile(OPENSSL_URL_LATEST_SVN+'/'+OpenSSLFileName,SafeGetApplicationPath+OpenSSLFileName,true,true);
    end;

    if OperationSucceeded
       then infoln(localinfotext+'Direct OpenSLL library files download from '+OPENSSL_URL_LATEST_SVN+' ok.',etWarning)
  end;

  result := OperationSucceeded;
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

    WgetZip := GetTempFileNameExt('FPCUPTMP','zip');

    for i:=0 to (Length(NewSourceURL)-1) do
    try
      WgetFile:=FileNameFromURL(NewSourceURL[i]);

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

    FreetypZip := GetTempFileNameExt('FPCUPTMP','zip');

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
    FreetypZipDir:=IncludeTrailingPathDelimiter(GetTempDirName)+'Freetype';
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

    SourceZip := GetTempFileNameExt('FPCUPTMP','zip');

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
    ZipDir:=GetTempDirName;
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
  TargetBin,SourceBin,SourceZip,ZipDir: string;
  i:integer;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (Download '+TARGETNAME+'): ';

  OperationSucceeded := false;

  // for now, just put jasmin.jar in FPC bin-directory ... easy and simple and working

  TargetBin:=ConcatPaths([FInstallDirectory,'bin',GetFPCTarget(true)])+PathDelim+TARGETNAME;

  if (NOT FileExists(TargetBin)) then
  begin
    infoln(localinfotext+'No '+TARGETNAME+' found. Going to download it.');

    SourceZip := GetTempFileNameExt('FPCUPTMP','zip');

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
    ZipDir:=GetTempDirName;
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
  if Native then
    result:=GetTargetCPU+'-'+GetTargetOS
  else
    result:=GetCPU(FCrossCPU_Target)+'-'+GetOS(FCrossOS_Target);
end;

function TInstaller.GetPath: string;
begin
  if Assigned(Processor) then
    result:=Processor.Environment.GetVar(PATHVARNAME)
  else
    result:=GetEnvironmentVariable(PATHVARNAME);
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

procedure TInstaller.SetTarget(aCPU:TCPU;aOS:TOS;aSubArch:string);
begin
  FCrossCPU_Target:=aCPU;
  FCrossOS_Target:=aOS;
  FCrossOS_SubArch:=aSubArch;
end;

function TInstaller.GetSuitableRepoClient:TRepoClient;
begin
  result:=nil;

  if result=nil then if DirectoryExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'.svn') then result:=FSVNClient;
  if result=nil then if (Pos('https://svn.',LowerCase(FURL))=1) then result:=FSVNClient;
  if result=nil then if (Pos('http://svn.',LowerCase(FURL))=1) then result:=FSVNClient;

  if result=nil then if DirectoryExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'.git') then result:=FGitClient;
  if result=nil then if ( {(Pos('GITHUB',UpperCase(FURL))>0) OR} (Pos('.GIT',UpperCase(FURL))>0) ) then result:=FGitClient;
end;

function TInstaller.GetTempFileNameExt(Prefix,Ext : String) : String;
Var
  I : Integer;
  Start,Extension : String;
begin
  Start:=FTempDirectory;
  Start:=Start+DirectorySeparator;
  if (Prefix='') then
    Start:=Start+'fpcup'
  else
    Start:=Start+Prefix;
  if (Ext='') then
    Extension:='tmp'
  else
    Extension:=Ext;
  if Extension[1]='.' then Delete(Extension,1,1);
  i:=0;
  repeat
    Result:=Format('%s%.5d.'+Extension,[Start,i]);
    Inc(i);
  until not FileExists(Result);
end;

function TInstaller.GetTempDirName(Prefix: String) : String;
Var
  I : Integer;
  Start : String;
begin
  Start:=FTempDirectory;
  Start:=Start+DirectorySeparator+Prefix;
  i:=0;
  repeat
    Result:=Format('%s%.5d',[Start,i]);
    Inc(i);
  until not DirectoryExists(Result);
  ForceDirectoriesSafe(Result);
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

  if not DirectoryExists(FSourceDirectory) then
  begin
    infoln(infotext+'No '+ModuleName+' source directory ('+FSourceDirectory+') found [yet] ... nothing to be done',etInfo);
    exit(true);
  end;
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

  aRepoClient:=nil;

  // not so elegant check to see what kind of client we need ...
  if aRepoClient=nil then if (Pos(SVNBASEHTTP,LowerCase(FURL))>0) then aRepoClient:=FSVNClient;
  if aRepoClient=nil then if (Pos(SVNBASESVN,LowerCase(FURL))>0) then aRepoClient:=FSVNClient;
  //if aRepoClient=nil then if (Pos(FTPBASEFTP,LowerCase(FURL))>0) then aRepoClient:=FFTPClient;
  //if aRepoClient=nil then if (Pos(FTPBASEHTTP,LowerCase(FURL))>0) then aRepoClient:=FFTPClient;
  if aRepoClient=nil then if ( {(Pos('GITHUB',UpperCase(FURL))>0) OR} (Pos('.GIT',UpperCase(FURL))>0) ) then aRepoClient:=FGitClient;
  if aRepoClient=nil then if ( (Pos('hg.code.sf.net',LowerCase(FURL))>0) ) then aRepoClient:=FHGClient;
  if aRepoClient=nil then if ( (Pos('bitbucket.org',LowerCase(FURL))>0) ) then aRepoClient:=FHGClient;

  //if aRepoClient=nil then aRepoClient:=FSVNClient;

  // No repo client ...
  if aRepoClient=nil then
  begin
    infoln(infotext+'Could not determine what repoclient to use for ' + ModuleName + ' sources !',etWarning);
    if (IsFPCInstaller OR IsLazarusInstaller) then
    begin
      exit;
    end
    else
    begin
      // Make a best quess
      infoln(infotext+'Using SVNClient for ' + ModuleName + ' sources !',etWarning);
      aRepoClient:=FSVNClient;
    end;
  end;

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
  DARWINCHECKMAGIC='useride: ';
  DARWINHACKMAGIC='./lazbuild$(SRCEXEEXT) --lazarusdir=. --build-ide= --ws=$(LCL_PLATFORM)';
var
  PatchList:TStringList;
  PatchFilePath,PatchFileCorrectedPath,PatchDirectory:string;
  LocalPatchCmd:string;
  s: string = '';
  ReturnCode,i,j: integer;
  LocalSourcePatches:string;
  PatchFPC,PatchUniversal,PatchAccepted:boolean;
  {$ifndef FPCONLY}
  PatchLaz:boolean;
  {$endif}
  PatchVersion,TrunkVersion:dword;
begin
  result:=false;

  PatchFPC:=(ModuleName=_FPC);
  {$ifndef FPCONLY}
  PatchLaz:=(ModuleName=_LAZARUS);
  {$endif}

  PatchUniversal:=(NOT PatchFPC);
  {$ifndef FPCONLY}
  PatchUniversal:=(PatchUniversal AND (NOT PatchLaz));
  {$endif};

  if PatchFPC then TrunkVersion:=CalculateNumericalVersion(FPCTRUNKVERSION);
  {$ifndef FPCONLY}
  if PatchLaz then TrunkVersion:=CalculateNumericalVersion(LAZARUSTRUNKVERSION);
  {$endif}

  if PatchFPC then PatchDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'patchfpc';
  {$ifndef FPCONLY}
  if PatchLaz then PatchDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'patchlazarus';
  {$endif}
  if PatchUniversal then PatchDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+'patchfpcup';

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
          PatchAccepted:=false;
          if PatchFPC then
          begin
            PatchAccepted:=((Pos('_FPCPATCH',PatchFilePath)>0) OR PatchAccepted);
            PatchAccepted:=((Pos('fpcpatch_',PatchFilePath)>0) OR PatchAccepted);
          end
          else
          {$ifndef FPCONLY}
          if PatchLaz then
          begin
            PatchAccepted:=((Pos('_LAZPATCH',PatchFilePath)>0) OR PatchAccepted);
            PatchAccepted:=((Pos('lazpatch_',PatchFilePath)>0) OR PatchAccepted);
          end
          else
          {$endif}
          if PatchUniversal then
          begin
            PatchAccepted:=((Pos('_FPCUPPATCH',PatchFilePath)>0) OR PatchAccepted);
            PatchAccepted:=((Pos('fpcuppatch_',PatchFilePath)>0) OR PatchAccepted);
          end;
          if (PatchAccepted) then DeleteFile(PatchFilePath);
        end;
      end;
    finally
      PatchList.Free;
    end;
  end;

  LocalSourcePatches:=FSourcePatches;

  // only patch if we want to and if we do not have a release candidate
  if (FOnlinePatching AND (FPatchVersion<>-1) AND (NOT PatchUniversal)) then
  begin
    infoln(localinfotext+'No online patching: we have a release candidate !');
  end;

  if (FOnlinePatching AND ((FPatchVersion=-1) OR PatchUniversal)) then
  begin
    PatchList:=TStringList.Create;
    try
      PatchList.Clear;
      try
        GetGitHubFileList(FPCUPGITREPOSOURCEPATCHESAPI,PatchList,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
      except
        on E : Exception do
        begin
          infoln(localinfotext+E.ClassName+' error raised, with message : '+E.Message, etError);
        end;
      end;

      for i:=0 to Pred(PatchList.Count) do
      begin
        infoln(localinfotext+'Found online patch: '+PatchList[i],etDebug);

        PatchFilePath:=PatchList[i];

        PatchAccepted:=false;
        if PatchFPC then PatchAccepted:=(Pos('fpcpatch',PatchFilePath)>0);
        {$ifndef FPCONLY}
        if PatchLaz then PatchAccepted:=(Pos('lazpatch',PatchFilePath)>0);
        {$endif}
        if PatchUniversal then
        begin
          PatchAccepted:=(Pos('fpcuppatch',PatchFilePath)>0);
          if PatchAccepted then
          begin
            PatchAccepted:=(Pos(LowerCase(ModuleName),LowerCase(FileNameFromURL(PatchFilePath)))>0);
          end;
        end;

        if (NOT PatchAccepted) then continue;

        infoln(infotext+'Using '+ExtractFileName(PatchFilePath)+ 'for '+ModuleName,etDebug);

        if NOT PatchUniversal then
        begin
          s:=FileNameFromURL(PatchFilePath);
          s:=ExtractFileNameOnly(s);
          s:=VersionFromUrl(s);
          PatchVersion:=CalculateNumericalVersion(s);

          if (s='trunk') or (PatchVersion=0) then
          begin
            //only patch trunk in case no version is given
            PatchVersion:=TrunkVersion;
          end;

          infoln(localinfotext+'Found online patch: '+PatchFilePath+' with version '+InttoStr(PatchVersion),etDebug);

          {$if defined(Darwin) and defined(LCLQT5)}
          //disable big hack for now
          if Pos('lazpatch_darwin_qt5hack',PatchFilePath)>0 then PatchAccepted:=False;
          {$else}
          if Pos('darwin_qt5',PatchFilePath)>0 then PatchAccepted:=False;
          {$endif}

          {$if not defined(MSWindows) and not defined(Haiku)}
          //only patch the Haiku build process on Windows and Haiku itself
          if Pos('fpcpatch_haiku.patch',PatchFilePath)>0 then PatchAccepted:=False;
          {$endif}

          {$ifndef Haiku}
          //only patch the Haiku FPU exception mask on Haiku itself
          if Pos('fpcpatch_haikufpu.patch',PatchFilePath)>0 then PatchAccepted:=False;
          {$endif}

          {$ifndef OpenBSD}
          //only patch the openbsd mask on OpenBSD itself
          if Pos('fpcpatch_openbsd',PatchFilePath)>0 then PatchAccepted:=False;
          {$endif}

          {$ifndef Darwin}
          //only patch the packages Makefile on Darwin itself
          if Pos('fpcpatch_darwin_makepackages_',PatchFilePath)>0 then PatchAccepted:=False;
          {$endif}

          {$ifndef FPCONLY}
          //only patch lazarus for musl on musl itself
          if Pos('lazpatch_musllibc',PatchFilePath)>0 then
          begin
            {$ifdef Linux}
            if (NOT FMUSL) then PatchAccepted:=False;
            {$else}
            PatchAccepted:=False;
            {$endif}
          end;

          //skip makefile patch lazarus ... we do it ourselves again.
          if Pos('lazpatch_useride',PatchFilePath)>0 then
          begin
            PatchAccepted:=False;
          end;
          {$endif}

          // In general, only patch trunk !
          // This can be changed to take care of versions ... but not for now !
          // Should be removed in future fpcup versions !!
          if PatchFPC {$ifndef FPCONLY}OR PatchLaz{$endif} then
          begin
            if NumericalVersion<>PatchVersion then PatchAccepted:=False;
            if (PatchVersion=TrunkVersion) then
            begin
              // only patch trunk when HEAD is requested
              if (FDesiredRevision <> '') AND (Uppercase(trim(FDesiredRevision)) <> 'HEAD') then
              begin
                PatchAccepted:=False;
              end;
            end;
          end;
        end;

        if (PatchAccepted) then
        begin
          infoln(infotext+'Online '+ExtractFileName(PatchFilePath)+ ' for '+ModuleName+' wil be applied !',etInfo);
          ForceDirectoriesSafe(PatchDirectory);
          s:=FileNameFromURL(PatchFilePath);
          GetFile(PatchFilePath,PatchDirectory+DirectorySeparator+s,true);
        end
        else
        begin
          infoln(infotext+'Online '+ExtractFileName(PatchFilePath)+ ' for '+ModuleName+' wil not be applied !',etDebug);
        end;
      end;
    finally
      PatchList.Free;
    end;
  end;

  // we will hack into fpc itself for better isolation
  // needs more testing
  (*
  if FOnlinePatching then
  begin
    if PatchFPC then
    begin
      PatchList:=TStringList.Create;
      try
        PatchList.Clear;
        PatchFilePath:=ConcatPaths([FSourceDirectory,'compiler','utils'])+DirectorySeparator+'fpc.pp';
        PatchList.LoadFromFile(PatchFilePath);

        // are we able to patch
        PatchAccepted:=False;
        for i:=0 to (PatchList.Count-1) do
        begin
          s:=PatchList.Strings[i];
          if (Pos('fpcupdeluxe',s)>0) then break; // we were here already ... ;-)
          if (Pos('call ppcXXX',s)>0) then PatchAccepted:=True;
          if PatchAccepted then
          begin
            // store correct position
            j:=i;
            break;
          end;
        end;

        if PatchAccepted then
        begin
          // do we have an array as ppccommandline
          PatchAccepted:=False;
          for i:=0 to (PatchList.Count-1) do
          begin
            s:=PatchList.Strings[i];
            if (Pos('SetLength(ppccommandline',s)>0) then PatchAccepted:=True;
            if PatchAccepted then break;
          end;

          if PatchAccepted then
          begin
            s:='ppccommandline[ppccommandlinelen+1]:=''@''+splitpath(paramstr(0))+''fpc.cfg'';';
            PatchList.Insert(j-1,'     '+s);
            s:='ppccommandline[ppccommandlinelen]:=''-n'';';
            PatchList.Insert(j-1,'     '+s);
            s:='SetLength(ppccommandline,ppccommandlinelen+2);';
            PatchList.Insert(j-1,'     '+s);
            s:='// Patched by fpcupdeluxe for better isolation';
            PatchList.Insert(j-1,'     '+s);
          end
          else
          begin
            s:='''-n @''+splitpath(paramstr(0))+''fpc.cfg''';
            PatchList.Insert(j-1,'     '+'ppccommandline:=ppccommandline+'+s+'+'' '';');
            s:='// Patched by fpcupdeluxe for better isolation';
            PatchList.Insert(j-1,'     '+s);
          end;
          PatchList.SaveToFile(PatchFilePath);
        end;

      finally
        PatchList.Free;
      end;
      {
      PatchFilePath:=IncludeTrailingPathDelimiter(PatchDirectory)+FPCPROXYPATCH;
      if FileExists(PatchFilePath) then SysUtils.DeleteFile(PatchFilePath);
      if NOT FileExists(PatchFilePath) then SaveInisFromResource(PatchFilePath,'FPCPROXY');
      }
    end;
  end;
  *)

  // we will hack into Lazarus makefile for better handling of useride
  {$ifndef FPCONLY}
  // only patch if we want to and if we do not have a release candidate
  // if (FOnlinePatching AND (FPatchVersion=-1)) then
  begin
    if PatchLaz then
    begin
      PatchList:=TStringList.Create;
      try
        PatchList.Clear;

        PatchFilePath:=IncludeTrailingPathDelimiter(FSourceDirectory)+MAKEFILENAME;

        PatchList.LoadFromFile(PatchFilePath);

        // are we able to patch
        j:=-1;
        PatchAccepted:=True;
        for i:=0 to (PatchList.Count-1) do
        begin
          s:=PatchList.Strings[i];
          if (Pos(DARWINHACKMAGIC,s)>0) then
          begin
            PatchAccepted:=False;
            break; // we were here already ... ;-)
          end;
          if (Pos(DARWINCHECKMAGIC,s)>0) then j:=i; //store position
        end;

        if (PatchAccepted AND (j<>-1)) then
        begin
          Inc(j);
          PatchList.Insert(j+1,'endif');
          PatchList.Insert(j,'else');
          PatchList.Insert(j,#9+DARWINHACKMAGIC);
          PatchList.Insert(j,'ifdef LCL_PLATFORM');
          PatchList.SaveToFile(PatchFilePath);
        end;

      finally
        PatchList.Free;
      end;
    end;
  end;
  {$endif FPCONLY}


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
    infoln(infotext+'Going to patch ' + ModuleName + ' sources !!',etWarning);
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
          if ((FPatchCmd='patch'+GetExeExt) OR (FPatchCmd='gpatch'+GetExeExt))
            {$IF defined(BSD) and not defined(DARWIN)}
            then LocalPatchCmd:=FPatchCmd + ' -p' + InttoStr(j) + ' -N -i '
            {$else}
            then LocalPatchCmd:=FPatchCmd + ' -p' + InttoStr(j) + ' -N --no-backup-if-mismatch -i '
            {$endif}
             else LocalPatchCmd:=Trim(FPatchCmd) + ' ';

          // always correct for line-endings while patch is very sensitive for that
          PatchFileCorrectedPath:=IncludeTrailingPathDelimiter(GetTempDirName)+ExtractFileName(PatchFilePath);
          if FileCorrectLineEndings(PatchFilePath,PatchFileCorrectedPath) then
          begin
            // revert to original file in case of file not found
            if (NOT FileExists(PatchFileCorrectedPath)) then PatchFileCorrectedPath:=PatchFilePath;
            {$IFDEF MSWINDOWS}
            ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + PatchFileCorrectedPath, FSourceDirectory,s, True);
            {$ELSE}
            ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + PatchFileCorrectedPath, FSourceDirectory, s, True);
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
              writelnlog(etError, infotext+ModuleName+' patch output: ' + s, true);
            end;
          end;
          DeleteDirectoryEx(ExtractFileDir(PatchFileCorrectedPath));
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

function TInstaller.CreateRevision(ModuleName,aRevision:string): boolean;
const
  // needs to be exactly the same as used by Lazarus !!!
  //RevisionIncComment = '// Created by FPCLAZUP';
  RevisionIncComment = '// Created by Svn2RevisionInc';
  ConstName = 'RevisionStr';
var
  RevisionIncText: Text;
  RevFileName,ConstStart: string;
  RevisionStringList:TStringList;
begin
  result:=false;
  // update revision.inc;

  RevFileName:='';

  if ModuleName=_LAZARUS then RevFileName:=IncludeTrailingPathDelimiter(FSourceDirectory)+'ide'+PathDelim+REVINCFILENAME;
  if ModuleName=_FPC then RevFileName:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+PathDelim+REVINCFILENAME;

  if Length(RevFileName)>0 then
  begin
    DeleteFile(RevFileName);
    RevisionStringList:=TStringList.Create;
    try
      if ModuleName=_LAZARUS then
      begin
        RevisionStringList.Add(RevisionIncComment);
        ConstStart := Format('const %s = ''', [ConstName]);
        RevisionStringList.Add(ConstStart+aRevision+''';');
      end;
      if ModuleName=_FPC then
      begin
        RevisionStringList.Add(''''+aRevision+'''');
      end;
      RevisionStringList.SaveToFile(RevFileName);
      result:=true;
    finally
      RevisionStringList.Free;
    end;


    (*
    //infoln(infotext+'Updating '+ModuleName+' '+RevFileName+'. Setting current revision:'+aRevision+'.', etInfo);
    AssignFile(RevisionIncText, RevFileName);
    try
      Rewrite(RevisionIncText);
      if ModuleName=_LAZARUS then
      begin
        writeln(RevisionIncText, RevisionIncComment);
        ConstStart := Format('const %s = ''', [ConstName]);
        writeln(RevisionIncText, ConstStart, aRevision, ''';');
      end;
      if ModuleName=_FPC then
      begin
        writeln(RevisionIncText, '''',aRevision,'''');
      end;
      result:=true;
    finally
      CloseFile(RevisionIncText);
    end;
    *)
  end;

end;

function TInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (UnInstallModule: '+ModuleName+'): ';
  infoln(infotext+'Entering ...',etDebug);
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

function TInstaller.GetSanityCheck:boolean;
begin
  result:=false;
  if IsFPCInstaller then
  begin

  end;
  if IsLazarusInstaller then
  begin

  end;
  if IsUniversalInstaller then
  begin

  end;
end;

function TInstaller.GetVersion:string;
var
  s:string;
begin
  s:=GetVersionFromSource(FSourceDirectory);
  if s='0.0.0' then s:=GetVersionFromURL(FURL);
  if s<>'0.0.0' then
  begin
    FMajorVersion := -1;
    FMinorVersion := -1;
    FReleaseVersion := -1;
    FPatchVersion := -1;
    VersionFromString(s,FMajorVersion,FMinorVersion,FReleaseVersion);
    FPatchVersion:=GetReleaseCandidateFromSource(FSourceDirectory);
    if FPatchVersion=-1 then FPatchVersion:=ReleaseCandidateFromUrl(FURL);
  end;
  result:=s;
end;

function TInstaller.GetInstallerClass(aClassToFind:TClass):boolean;
var
  aClass:TClass;
begin
  result:=false;
  aClass:=Self.ClassType;
  while aClass<>nil do
  begin
    if aClass=aClassToFind then
    begin
      result:=True;
      break;
    end;
    aClass:=aClass.ClassParent;
  end;
end;

function TInstaller.IsFPCInstaller:boolean;
begin
  result:=GetInstallerClass(TBaseFPCInstaller);
end;

function TInstaller.IsLazarusInstaller:boolean;
begin
  result:=GetInstallerClass(TBaseLazarusInstaller);
end;

function TInstaller.IsUniversalInstaller:boolean;
begin
  result:=GetInstallerClass(TBaseUniversalInstaller);
end;

function TInstaller.GetDefaultCompilerFilename(const TargetCPU: TCPU; Cross: boolean): string;
var
  s:string;
begin
  s:='fpc';
  if TargetCPU<>TCPU.cpuNone then
  begin
    if Cross then
      s:='ppcross'+ppcSuffix[TargetCPU]
    else
      s:='ppc'+ppcSuffix[TargetCPU];
  end;
  Result:=s+GetExeExt;
end;

function TInstaller.GetCompilerName(Cpu_Target:TCPU):string;
begin
  result:=GetDefaultCompilerFilename(Cpu_Target,false);
end;

function TInstaller.GetCompilerName(Cpu_Target:string):string;
var
  aCPU:TCPU;
begin
  result:='fpc'+GetExeExt;
  if Cpu_Target<>'' then
  begin
    for aCPU:=Low(TCPU) to High(TCPU) do
    begin
      if (Cpu_Target=GetCPU(aCPU)) then
      begin
        result:=GetDefaultCompilerFilename(aCPU,false);
      end;
    end;
  end;
end;

function TInstaller.GetCrossCompilerName(Cpu_Target:TCPU):string;
begin
  if Cpu_Target<>TCPU.jvm
     then result:=GetDefaultCompilerFilename(Cpu_Target,true)
     else result:=GetDefaultCompilerFilename(Cpu_Target,false);
end;

procedure TInstaller.infoln(Message: string; const Level: TEventType=etInfo);
begin
  // Note: these strings should remain as is so any fpcupgui highlighter can pick it up
  if (Level<>etDebug) then
    begin
      if AnsiPos(LineEnding, Message)>0 then ThreadLog(''); //Write an empty line before multiline messagse
      ThreadLog(BeginSnippet+' '+Seriousness[Level]+' '+ Message); //we misuse this for info output
      {$IFDEF MSWINDOWS}
      Sleep(1);
      {$ENDIF}
    end
  else
    begin
    {$IFDEF DEBUG}
    {DEBUG conditional symbol is defined using
    Project Options/Other/Custom Options using -dDEBUG}
    if AnsiPos(LineEnding, Message)>0 then ThreadLog(''); //Write an empty line before multiline messagse
    ThreadLog(BeginSnippet+' '+Seriousness[Level]+' '+ Message); //we misuse this for info output
    {$IFDEF MSWINDOWS}
    Sleep(1);
    {$ENDIF}
    {$ENDIF}
    end;
end;


function TInstaller.ExecuteCommand(Commandline: string; Verbosity: boolean): integer;
var
  s:string='';
begin
  Result:=ExecuteCommandInDir(Commandline,'',s,Verbosity);
end;

function TInstaller.ExecuteCommand(Commandline: string; out Output: string;
  Verbosity: boolean): integer;
begin
  Result:=ExecuteCommandInDir(Commandline,'',Output,Verbosity);
end;

function TInstaller.ExecuteCommandInDir(Commandline, Directory: string; Verbosity: boolean
  ): integer;
var
  s:string='';
begin
  Result:=ExecuteCommandInDir(Commandline,Directory,s,Verbosity);
end;

function TInstaller.ExecuteCommandInDir(Commandline, Directory: string;
  out Output: string; Verbosity: boolean): integer;
begin
  Result:=ExecuteCommandInDir(CommandLine,Directory,Output,'',Verbosity);
end;

function TInstaller.ExecuteCommandInDir(Commandline, Directory: string;
  out Output: string; PrependPath: string; Verbosity: boolean): integer;
var
  OldPath: string;
  OldVerbosity:boolean;
  i:integer;
  aTool:TExternalTool;
  FParameters:TStrings;
begin

  result:=0;

  if Assigned(Processor) then
    aTool:=Processor
  else
    aTool:=TExternalTool.Create(nil);

  try
    aTool.Process.Executable:='';
    aTool.Process.Parameters.Clear;

    //aTool.Process.CommandLine:=Commandline;

    FParameters:=TStringList.Create;
    try
      CommandToList(Commandline,FParameters);
      if FParameters.Count>0 then
      begin
        aTool.Process.Executable:=FParameters[0];
        repeat
          i:=FParameters.IndexOf('emptystring');
          if (i<>-1) then FParameters[i]:='""';
        until (i=-1);
        for i:=1 to Pred(FParameters.Count) do
          aTool.Process.Parameters.Add(FParameters[i]);
      end;
    finally
      FParameters.Free;
    end;

    if (Length(aTool.Process.Executable)>0) then
    begin
      OldVerbosity:=aTool.Verbose;
      aTool.Verbose:=Verbosity;

      if Directory<>'' then
        aTool.Process.CurrentDirectory:=Directory;

      // Prepend specified PrependPath if needed:
      if PrependPath<>'' then
      begin
        OldPath:=aTool.Environment.GetVar(PATHVARNAME);
        if OldPath<>'' then
           aTool.Environment.SetVar(PATHVARNAME, PrependPath+PathSeparator+OldPath)
        else
          aTool.Environment.SetVar(PATHVARNAME, PrependPath);
      end;

      result:=aTool.ExecuteAndWait;

      Output:=aTool.WorkerOutput.Text;

      aTool.Environment.SetVar(PATHVARNAME, OldPath);
      aTool.Verbose:=OldVerbosity;
    end;

  finally
    if NOT Assigned(Processor) then
      aTool.Free;
  end;
end;

constructor TInstaller.Create;
begin
  inherited Create;

  FCrossInstaller:=nil;

  FExternalTool:=TExternalTool.Create(nil);
  FExternalToolResult:=0;

  FCPUCount  := GetLogicalCpuCount;

  FSVNClient := TSVNClient.Create(Self);
  FGitClient := TGitClient.Create(Self);
  FHGClient  := THGClient.Create(Self);

  FShell := '';

  // List of binutils that can be downloaded:
  // CreateBinutilsList;
  FNeededExecutablesChecked:=false;
  FCleanModuleSuccess:=false;

  // Set up verbose log: will be done in dumpoutput
  // as it depends on verbosity etc
  //FLogVerbose: TLogger.Create;
  FErrorLog := TStringList.Create;

  FCrossCPU_Target:=TCPU.cpuNone;
  FCrossOS_Target:=TOS.osNone;
  FCrossOS_SubArch:='';

  FMajorVersion := -1;
  FMinorVersion := -1;
  FReleaseVersion := -1;
  FPatchVersion := -1;

  FMUSL:=false;
  FSolarisOI:=false;

  {$ifdef Linux}
  FMUSLLinker:='/lib/ld-musl-'+GetTargetCPU+'.so.1';
  FMUSL:=(FileExists(FMUSLLinker) AND IsLinuxMUSL);
  if FMUSL then infoln('Fpcupdeluxe: We have a MUSL Linux version !',etInfo);
  {$endif}

  GetSanityCheck;
end;

destructor TInstaller.Destroy;
begin
  if Assigned(FLogVerbose) then
    FLogVerbose.Free;
  if Assigned(FErrorLog) then
    FErrorLog.Free;
  FExternalTool.Free;
  FGitClient.Free;
  FHGClient.Free;
  FSVNClient.Free;
  FCrossInstaller:=nil;
  inherited Destroy;
end;

end.

