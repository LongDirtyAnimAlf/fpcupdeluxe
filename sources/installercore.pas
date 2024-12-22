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

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils,
  repoclient, GitClient, HGClient, SvnClient,
  installerBase, m_crossinstaller,
  fpcuputil, processutils;

{$i revision.inc}

const
  DEFAULTFPCVERSION     = '3.2.2';

  FPCTRUNKVERSION       = '3.3.1';
  FPCTRUNKBOOTVERSION   = '3.2.2';

  LAZARUSTRUNKVERSION   = '4.99';

  DEFAULTFREEBSDVERSION = 12;

  LAZBUILDNAME          = 'lazbuild';

  MAKEFILENAME          = 'Makefile';
  FPCMAKEFILENAME       = MAKEFILENAME+'.fpc';

  FPCMAKECONFIG         = 'fpcmkcfg';

  LIBQT4                = 'libQt4Pas.so';
  LIBQT4VERSION         = LIBQT4+'.1';
  LIBQT5                = 'libQt5Pas.so';
  LIBQT5VERSION         = LIBQT5+'.1';
  LIBQT6                = 'libQt6Pas.so';
  LIBQT6VERSION         = LIBQT6+'.6';

  FPCPKGFILENAME        = 'fppkg';
  FPCPKGCONFIGFILENAME  = 'fppkg.cfg';

  FPFILENAME            = 'fp';
  FPCONFIGFILENAME      = 'fp.cfg';
  FPINIFILENAME         = 'fp.ini';

  FPCPKGCOMPILERTEMPLATE= 'default'; // fppkg default compiler template

  FPCCONFIGFILENAME     = 'fpc.cfg';
  FPCCONFIGUNICODE      = 'fpc-unicodertl.cfg';

  DEFAULTINSTALLDIR     = 'development';
  DEFAULTBOOTSTRAPDIR   = 'fpcbootstrap';

  GITLAB                = 'https://gitlab.com/freepascal.org/';

  FPCGITLAB             = GITLAB + 'fpc';
  FPCGITLABREPO         = FPCGITLAB + '/source';
  FPCGITLABBUILDBINARIES= FPCGITLAB + '/build';
  FPCTRUNKBRANCH        = 'main';
  FPCTRUNKBINARIES      = FPCGITLABBUILDBINARIES + '/-/raw/'+FPCTRUNKBRANCH;
  {$ifdef win32}
  FPCGITLABBINARIES     = FPCGITLAB + '/binaries';
  {$endif}

  LAZARUSGITLAB         = GITLAB + 'lazarus';
  LAZARUSGITLABREPO     = LAZARUSGITLAB + '/lazarus';
  LAZARUSGITLABBINARIES = LAZARUSGITLAB + '/binaries';
  LAZARUSTRUNKBRANCH    = 'main';
  LAZARUSBINARIES       = LAZARUSGITLABBINARIES + '/-/raw/'+LAZARUSTRUNKBRANCH;

  PACKAGESLOCATION      = 'packages.fppkg';
  PACKAGESCONFIGDIR     = 'fpcpkgconfig';
  //PACKAGESCONFIGDIR     = PACKAGESLOCATION+DirectorySeparator+'fpcpkgconfig';

  REVINCFILENAME        = 'revision.inc';

  {$IFDEF WINDOWS}
  PREBUILTBINUTILSURLWINCE = FPCTRUNKBINARIES+'/install/crossbinwce';
  {$ENDIF}

  {$ifdef win64}
  OPENSSL_URL_LATEST = LAZARUSBINARIES + '/x86_64-win64/openssl';
  {$endif}
  {$ifdef win32}
  OPENSSL_URL_LATEST = LAZARUSBINARIES + '/i386-win32/openssl';
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
  NASMFPCURL=FPCTRUNKBINARIES+'/install/crossbinmsdos/nasm.exe';

  GITREPO='https://github.com/LongDirtyAnimAlf';
  FPCUPGITREPO=GITREPO+'/fpcupdeluxe';

  BOOTSTRAPPERVERSION='bootstrappers_v1.0';
  FPCUPGITREPOBOOTSTRAPPER=FPCUPGITREPO+'/releases/download/'+BOOTSTRAPPERVERSION;

  TOOLSVERSION='Tools_v1.0';
  FPCUPTOOLS=FPCUPGITREPO+'/releases/download/'+TOOLSVERSION;

  FPCUPGITREPOAPI='https://api.github.com/repos/LongDirtyAnimAlf/fpcupdeluxe';
  FPCUPGITREPOAPIRELEASES=FPCUPGITREPOAPI+'/releases';
  FPCUPGITREPOBOOTSTRAPPERAPI=FPCUPGITREPOAPIRELEASES+'/tags/'+BOOTSTRAPPERVERSION;

  SOURCEPATCHES='patches_v1.0';
  FPCUPGITREPOSOURCEPATCHESAPI=FPCUPGITREPOAPIRELEASES+'/tags/'+SOURCEPATCHES;

  FPCUP_ACKNOWLEDGE='acknowledgement_fpcup.txt';

  {$IF (defined(OpenBSD)) and (defined(CPU64))}
  // 2.6.2 and older do not work anymore on newer OpenBSD64 versions
  FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION=(2*10000+6*100+2);
  {$else}
  // 2.2.4 and older have no official FPC bootstrapper available online
  FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION=(2*10000+2*100+4);
  {$endif}

  {$ifdef WINDOWS}
  Crypto_DLL_Name = 'libeay32';
  SSL_DLL_Name    = 'ssleay32';

  {$ifdef win64}
    SSL_DLL_Names:    array[1..3] of string = ({'libssl-3-x64',}    'libssl-1_1-x64',    SSL_DLL_Name, 'libssl32');
    Crypto_DLL_Names: array[1..3] of string = ({'libcrypto-3-x64',} 'libcrypto-1_1-x64', Crypto_DLL_Name, Crypto_DLL_Name);
  {$endif}
  {$ifdef win32}
    SSL_DLL_Names:    array[1..3] of string = ({'libssl-3',}    'libssl-1_1',    SSL_DLL_Name, 'libssl32');
    Crypto_DLL_Names: array[1..3] of string = ({'libcrypto-3',} 'libcrypto-1_1', Crypto_DLL_Name, Crypto_DLL_Name);
  {$endif}

  {$ifdef win64}
  OpenSSLSourceURL : array [0..4] of string = (
    //'https://indy.fulgan.com/SSL/openssl-1.0.2u-x64_86-win64.zip',
    'https://github.com/IndySockets/OpenSSL-Binaries/raw/master/openssl-1.0.2u-x64_86-win64.zip',
    'http://wiki.overbyte.eu/arch/openssl-1.0.2u-win64.zip',
    'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win64.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2u-x64_86-win64.zip',
    'https://indy.fulgan.com/SSL/Archive/openssl-1.0.2p-x64_86-win64.zip'
    );
  {$endif}
  {$ifdef win32}
  OpenSSLSourceURL : array [0..4] of string = (
    //'https://indy.fulgan.com/SSL/openssl-1.0.2u-i386-win32.zip',
    'https://github.com/IndySockets/OpenSSL-Binaries/raw/master/openssl-1.0.2u-i386-win32.zip',
    'http://wiki.overbyte.eu/arch/openssl-1.0.2u-win32.zip',
    'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win32.zip',
    'https://indy.fulgan.com/SSL/openssl-1.0.2u-i386-win32.zip',
    'https://indy.fulgan.com/SSL/Archive/openssl-1.0.2p-i386-win32.zip'
    );
  {$endif}
  {$endif}

  //LINUXLEGACYPATCH = 'glibc_compat_3_2_2.patch';

  REVISIONSLOG     = 'fpcuprevisions.log';

  SnipMagicBegin = '# begin fpcup do not remove '; //look for this/add this in fpc.cfg cross-compile snippet. Note: normally followed by FPC CPU-os code
  SnipMagicEnd   = '# end fpcup do not remove'; //denotes end of fpc.cfg cross-compile snippet
  FPCSnipMagic   = '# If you don''t want so much verbosity use'; //denotes end of standard fpc.cfg

  FPCREVMAGIC   = 'FPC new revision: ';
  LAZREVMAGIC   = 'Lazarus new revision: ';

  FPCDATEMAGIC  = 'FPC update at: ';
  LAZDATEMAGIC  = 'Lazarus update at: ';

  FPCHASHMAGIC   = 'FPC new GIT hash: ';
  LAZHASHMAGIC   = 'Lazarus new GIT hash: ';

  FPCNAMEMAGIC   = 'FPC symbolic name: ';
  LAZNAMEMAGIC   = 'Lazarus symbolic name: ';

  FPCURLLOOKUPMAGIC            = 'fpcURL';
  FPCTAGLOOKUPMAGIC            = 'fpcTAG';
  FPCBRANCHLOOKUPMAGIC         = 'fpcBRANCH';

  LAZARUSURLLOOKUPMAGIC        = 'lazURL';
  LAZARUSTAGLOOKUPMAGIC        = 'lazTAG';
  LAZARUSBRANCHLOOKUPMAGIC     = 'lazBRANCH';

  GITLABEXTENSION              = '.gitlab';

  DIFFMAGIC                    = 'revhash_';

  NEWLIBSTAG                   = 'crosslibs_all';
  OLDLIBSTAG                   = 'crosslibs';

  OLDBINSTAG                   = 'crossbins';
  NEWBINSTAG                   = 'crossbins_all';

  //WINDOWS_ALL_NEWBINSTAG       = 'windows_'+NEWBINSTAG;
  //LINUXAMD64_NEWBINSTAG        = 'linux_amd64_'+NEWBINSTAG;
  //FREEBSDAMD64_NEWBINSTAG      = 'freebsd_amd64_'+NEWBINSTAG;

  //Sequence contants for statemachine

  _SEP                     = ';';

  _FPC                     = 'FPC';
  _LAZARUS                 = 'Lazarus';
  _LAZARUSSIMPLE           = _LAZARUS+'Simple';

  _UNICODEFPC              = 'Unicode'+_FPC;

  _REVISIONFPC             = 'Revision'+_FPC;
  _REVISIONLAZARUS         = 'Revision'+_LAZARUS;

  _MAKEFILECHECK           = 'MakefileCheck';
  _MAKEFILECHECKFPC        = _MAKEFILECHECK+_FPC;
  _MAKEFILECHECKLAZARUS    = _MAKEFILECHECK+_LAZARUS;

  _NATIVECROSSFPC          = 'Native'+_FPC;

  _DEFAULT                 = 'Default';
  _DEFAULTSIMPLE           = 'DefaultSimple';

  _CLEAN                   = 'Clean';
  _CHECK                   = 'Check';
  _GET                     = 'Get';
  _CONFIG                  = 'Config';
  _BUILD                   = 'Build';
  _INSTALL                 = 'Install';
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

  _DOCKER                  = 'Docker';

  _SUGGESTED               = 'suggestedpackages';
  _SUGGESTEDADD            = _SUGGESTED+'add';

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

  _INSTALLLAZARUS          = _INSTALL+_LAZARUS;

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

  URL_ERROR                = 'sources error (URL mismatch)';

type
  TInstallerError          = (ieLibs,ieBins);
  TInstallerErrors         = set of TInstallerError;

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

  TRevision= record
    SVNRevision: string;
    GITHash: string;
  end;

  { TInstaller }

  TInstaller = class(TObject)
  strict private
    FLinuxLegacy: boolean;
    FInstallDirectory:string;
    FBaseDirectory:string;
    FSourceDirectory:string;
    FFPCBinDir:string;
    FMakePath:string;
    FExternalTool: TExternalTool;
    FExternalToolResult: integer;
    {$IFDEF UNIX}
    FUseCompilerWrapper        : boolean;
    {$ENDIF}
    FQTTrickeryNeeded          : boolean;
    FQTLibs                    : string;
    FQTLibsVersioned           : string;
    function GetDefaultCompilerFilename(const TargetCPU: TCPU; const Cross: boolean): string;
    procedure SetLinuxLegacy(value:boolean);
    function  GetLinuxLegacy:boolean;
  private
    FURL                       : string;
    FUltibo                    : boolean;
    FKeepLocalChanges          : boolean;
    FReApplyLocalChanges       : boolean;
    FCrossInstaller            : TCrossInstaller;
    FCrossCPU_Target           : TCPU; //When cross-compiling: CPU, e.g. x86_64
    FCrossOS_Target            : TOS; //When cross-compiling: OS, e.g. win64
    FCrossOS_SubArch           : TSUBARCH; //When cross-compiling for embedded: CPU, e.g. for Teensy SUBARCH=ARMV7EM
    FCrossOS_ABI               : TABI; //When cross-compiling for arm: hardfloat or softfloat calling convention
    FCrossToolsDirectory       : string;
    FCrossLibraryDirectory     : string;
    procedure SetURL(value:string);
    procedure SetSourceDirectory(value:string);
    procedure SetBaseDirectory(value:string);
    procedure SetInstallDirectory(value:string);
    procedure SetFPCSourceDirectory(value:string);
    procedure SetLazarusInstallDirectory(value:string);
    procedure SetLazarusSourceDirectory(value:string);
    procedure SetLazarusPrimaryConfigDirectory(value:string);
    procedure SetMakeDirectory(value:string);
    procedure SetCompiler(value:string);
    function GetShell: string;
    function GetMake: string;
    procedure SetVerbosity(aValue:boolean);
    procedure SetHTTPProxyHost(AValue: string);
    procedure SetHTTPProxyPassword(AValue: string);
    procedure SetHTTPProxyPort(AValue: integer);
    procedure SetHTTPProxyUser(AValue: string);
    function DownloadFromBase(aClient:TRepoClient; aModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Get fpcup registred cross-compiler, if any, if not, return nil
    function GetCrossInstaller: TCrossInstaller;
    function GetFullVersionString:string;
    function GetFullVersion:dword;
    function GetInstallerClass(aClassToFind:TClass):boolean;
    function IsFPCInstaller:boolean;
    function IsLazarusInstaller:boolean;
    function IsHelpInstaller:boolean;
    function IsUniversalInstaller:boolean;
    {$IFDEF MSWINDOWS}
    function GetStrayShell: boolean;
    {$ENDIF MSWINDOWS}
  protected
    FErrorCodes                : TInstallerErrors;
    FFPCInstallDir             : string;
    FFPCSourceDir              : string;
    FLazarusInstallDir         : string;
    FLazarusSourceDir          : string;
    FLazarusPrimaryConfigPath  : string;
    FTempDirectory             : string; //For storing temp files and logs
    FCleanModuleSuccess: boolean;
    FNeededExecutablesChecked: boolean;
    FCompiler: string; // Compiler executable
    FCompilerOptions: string; //options passed when compiling (FPC or Lazarus currently)
    FCPUCount: integer; //logical cpu count (i.e. hyperthreading=2cpus)
    FCrossOPT: string; //options passed (only) when cross-compiling
    FPreviousRevision: string;
    FDesiredRevision: string;
    FActualRevision: string;
    FBranch: string;
    FTAG: string;
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
    FGitClient: TGitClient;
    FHGClient: THGClient;
    FSVNClient: TSVNClient;
    FRepositoryUpdated: boolean;
    FSourcePatches: string;
    FPreInstallScriptPath: string;
    FPostInstallScriptPath: string;
    FMajorVersion: integer; //major part of the version number, e.g. 1 for 1.0.8, or -1 if unknown
    FMinorVersion: integer; //minor part of the version number, e.g. 0 for 1.0.8, or -1 if unknown
    FReleaseVersion: integer; //release part of the version number, e.g. 8 for 1.0.8, or -1 if unknown
    FPatchVersion: integer; //release candidate part of the version number, e.g. 3 for 1.0.8_RC3, or -1 if unknown
    {$IFDEF MSWINDOWS}
    FUtilFiles: array of TUtilsList; //Keeps track of binutils etc download locations, filenames...
    {$ENDIF MSWINDOWS}
    FExportOnly: boolean;
    FNoJobs: boolean;
    FOnlinePatching: boolean;
    FVerbose: boolean;
    FUseWget: boolean;
    FTar: string;
    FGunzip: string;
    F7zip: string;
    FWget: string;
    FUnrar: string;
    //FGit: string;
    FSolarisOI: boolean;
    FMUSL: boolean;
    FFPCUnicode: boolean;
    FMUSLLinker: string;
    property Shell: string read GetShell;
    property Make: string read GetMake;
    procedure SetFPCInstallDirectory(value:string);virtual;
    // Check for existence of required executables; if not there, get them if possible
    function CheckAndGetTools: boolean;
    // Check for existence of required binutils; if not there, get them if possible
    function CheckAndGetNeededBinUtils: boolean;
    // Get a diff of all modified files in and below the directory and save it
    procedure CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
    // Clone/update using HG; use SourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromHG(aModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Clone/update using Git; use SourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromGit(aModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Checkout/update using SVN; use SourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    function DownloadFromSVN(aModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
    function DownloadFromURL(ModuleName: string): boolean;
    // Clone/update using Git; use SourceDirectory as local repository
    // Any generated warnings will be added to UpdateWarnings
    {$IFDEF MSWINDOWS}
    // Make a list (in FUtilFiles) of all binutils that can be downloaded
    procedure CreateBinutilsList({%H-}aVersion:string='');
    // Download make.exe, patch.exe etc into the make directory (only implemented for Windows):
    function DownloadBinUtils: boolean;
    function DownloadSVN: boolean;
    {$ifndef USEONLYCURL}
    function DownloadOpenSSL: boolean;
    {$endif}
    function DownloadWget: boolean;
    function DownloadFreetype: boolean;
    function DownloadZlib: boolean;
    {$ENDIF}
    function DownloadJasmin: boolean;
    // Looks for SVN client in subdirectories and sets FSVNClient.SVNExecutable if found.
    {$IFDEF MSWINDOWS}
    function FindSVNSubDirs: boolean;
    {$ENDIF}
    // Returns CPU-OS in the format used by the FPC bin directory, e.g. x86_64-win64:
    function GetFPCTarget(Native: boolean): string;
    // Sets the search/binary path to NewPath or adds NewPath before or after existing path:
    procedure SetPath(NewPath: string; Prepend: boolean; Append: boolean);
    // Get currently set path
    function GetPath: string;
    function GetFile(aURL,aFile:string; forceoverwrite:boolean=false; forcenative:boolean=false):boolean;
    function GetSanityCheck:boolean;
    function GetVersionFromSource:string;virtual;
    function GetVersionFromURL({%H-}aUrl:string):string;virtual;
    function GetReleaseCandidateFromSource:integer;virtual;
    function GetVersion:string;
    function InitInfoText(const ExtraInfo:string=''):string;
    function IsCross:boolean;virtual;
  public
    InfoText: string;
    LocalInfoText: string;
    property SVNClient: TSVNClient read FSVNClient;
    property GitClient: TGitClient read FGitClient;
    property HGClient: THGClient read FHGClient;
    // Get processor for termination of running processes
    property Processor: TExternalTool read FExternalTool;
    property ProcessorResult: integer read FExternalToolResult write FExternalToolResult;
    //Base directory for this fpc(laz)up(deluxe) install
    property BaseDirectory: string  read FBaseDirectory write SetBaseDirectory;
    // Generic tmp directory for this installation
    property TempDirectory: string write FTempDirectory;
    // Directory where make (and the other binutils on Windows) is located
    property MakeDirectory: string write SetMakeDirectory;
    // Path where make (and the other binutils on Windows) is located
    property MakePath: string read FMakePath;
    // Generic source directory for installation (fpcdir, lazdir,... option)
    property SourceDirectory: string read FSourceDirectory write SetSourceDirectory;
    // Generic install directory
    property InstallDirectory: string read FInstallDirectory write SetInstallDirectory;
    // FPC source directory
    property FPCSourceDir: string write SetFPCSourceDirectory;
    // FPC install directory
    property FPCInstallDir: string write SetFPCInstallDirectory;
    // FPC binaries install locations
    property FPCBinDir:string read FFPCBinDir;
    // Lazarus install directory
    property LazarusInstallDir: string write SetLazarusInstallDirectory;
    // Lazarus source directory
    property LazarusSourceDir: string write SetLazarusSourceDirectory;
    // Lazarus primary config path
    property LazarusPrimaryConfigPath:string write SetLazarusPrimaryConfigDirectory;
    // Compiler to use for building. Specify empty string when using bootstrap compiler.
    property Compiler: string {read GetCompiler} write SetCompiler;
    // Compiler options passed on to make as OPT= or FPCOPT=
    property CompilerOptions: string write FCompilerOptions;
    property CrossCPU_Target: TCPU read FCrossCPU_Target; //When cross-compiling: CPU, e.g. x86_64
    property CrossOS_Target: TOS read FCrossOS_Target; //When cross-compiling: OS, e.g. win64
    property CrossOS_SubArch: TSUBARCH read FCrossOS_SubArch;// SubArch for target embedded
    property CrossOS_ABI: TABI read FCrossOS_ABI;    // When cross-compiling for arm: hardfloat or softfloat calling convention
    // Options for cross compiling. User can specify his own, but cross compilers can set defaults, too
    property CrossOPT: string read FCrossOPT write FCrossOPT;
    property CrossToolsDirectory:string read FCrossToolsDirectory write FCrossToolsDirectory;
    property CrossLibraryDirectory:string read FCrossLibraryDirectory write FCrossLibraryDirectory;
    // SVN revision override. Default is HEAD/latest revision
    property PreviousRevision: string read FPreviousRevision;
    property DesiredRevision: string write FDesiredRevision;
    property ActualRevision: string read FActualRevision;
    property Branch: string write FBranch;
    property TAG: string write FTAG;
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
    // Whether or not to back up locale changes to .diff and reapply them before compiling
    property ReApplyLocalChanges: boolean write FReApplyLocalChanges;
    // do we have OpenIndiana instead of plain Solaris
    property SolarisOI: boolean write FSolarisOI;
    // do we have musl instead of libc
    property MUSL: boolean write FMUSL;
    // do we have a legacy linux as target
    property LinuxLegacy: boolean read GetLinuxLegacy write SetLinuxLegacy;
    // do we need to build the FPC Unicode version of FPC
    property FPCUnicode: boolean write FFPCUnicode;
    // Are we installing Ultibo
    property Ultibo: boolean read FUltibo write FUltibo;
    property Log: TLogger write FLog;
    // Patch utility to use. Defaults to 'patch'
    property PatchCmd:string write FPatchCmd;
    // URL for download. HTTP, ftp or svn or git or hg
    property URL: string read FURL write SetURL;
    // patches
    property SourcePatches: string write FSourcePatches;
    property PreInstallScriptPath: string write FPreInstallScriptPath;
    property PostInstallScriptPath: string write FPostInstallScriptPath;
    // do not download the repo itself, but only get the files (of master)
    property ExportOnly: boolean write FExportOnly;
    property NoJobs: boolean write FNoJobs;
    property OnlinePatching: boolean write FOnlinePatching;
    // display and log in temp log file all sub process output
    property Verbose: boolean write SetVerbosity;
    // use wget as downloader ??
    property UseWget: boolean write FUseWget;
    // get cross-installer
    property CrossInstaller:TCrossInstaller read GetCrossInstaller;
    property SourceVersionStr:string read GetFullVersionString;
    property SourceVersionNum:dword read GetFullVersion;
    property SanityCheck:boolean read GetSanityCheck;
    {$IFDEF MSWINDOWS}
    property StrayShell: boolean read GetStrayShell;
    {$ENDIF MSWINDOWS}
    property ErrorCodes : TInstallerErrors read FErrorCodes;
    {$IFDEF UNIX}
    property UseCompilerWrapper : boolean read FUseCompilerWrapper;
    {$ENDIF}

    property QTLibs : string read FQTLibs;
    property QTLibsVersioned : string read FQTLibsVersioned;
    property QTTrickeryNeeded : boolean read FQTTrickeryNeeded;

    // FPC config directory
    function GetFPCConfigPath(const aCFG:string):string;
    function GetCompilerName(Cpu_Target:TCPU):string;overload;
    function GetCompilerName(Cpu_Target:string):string;overload;
    function GetCrossCompilerName(Cpu_Target:TCPU):string;
    procedure SetTarget(aCPU:TCPU;aOS:TOS;aSubArch:TSUBARCH);virtual;
    procedure SetABI(aABI:TABI);
    // append line ending and write to log and, if specified, to console
    procedure WritelnLog(msg: TStrings; ToConsole: boolean = true);overload;
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
    // Constructs FPC compiler path from install directory and architecture
    // Corrects for use of our fpc.sh launcher on *nix
    // Does not verify compiler actually exists.
    function GetFPCInBinDir: string;
    // Install update sources
    function GetModule(ModuleName: string): boolean; virtual;
    // Perform some checks on the sources
    function CheckModule(ModuleName: string): boolean; virtual;
    // Patch sources
    function PatchModule(ModuleName: string): boolean;
    //Source revision
    function CreateRevision(ModuleName,aRevision:string): boolean;
    function GetRevision(ModuleName:string): string;

    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; virtual;
    procedure Infoln(const Message: string; const Level: TEventType=etInfo);

    function ExecuteCommand(const Commandline: string; Verbosity:boolean): integer; overload;
    function ExecuteCommand(const Commandline: string; out Output:string; Verbosity:boolean): integer; overload;
    function ExecuteCommand(const ExeName:String;const Arguments:array of String;Verbosity:boolean):integer;
    function ExecuteCommand(const ExeName:String;const Arguments:array of String;out Output:string;Verbosity:boolean):integer;overload;
    function ExecuteCommandInDir(const Commandline, Directory: string; Verbosity:boolean): integer; overload;
    function ExecuteCommandInDir(const Commandline, Directory: string; out Output:string; Verbosity:boolean): integer; overload;
    function ExecuteCommandInDir(const Commandline, Directory: string; out Output:string; PrependPath: string; Verbosity:boolean): integer; overload;
    function ExecuteCommandInDir(const ExeName:String;const Arguments:array of String;const Directory:String;out Output:string; PrependPath: string;Verbosity:boolean):integer;overload;

    constructor Create;
    destructor Destroy; override;
  end;

  TBaseFPCInstaller        = class(TInstaller);
  {$ifndef FPCONLY}
  TBaseLCLInstaller    = class(TInstaller)
  private
    FLCL_Platform: LCL_TYPE;
  public
    // LCL widget set to be built (NOT OS/CPU combination)
    property LCL_Platform: LCL_TYPE read FLCL_Platform write FLCL_Platform;
  end;
  {$else}
  TBaseLCLInstaller    = class(TInstaller);
  {$endif}
  TBaseLazarusInstaller    = class(TBaseLCLInstaller);
  TBaseUniversalInstaller  = class(TBaseLCLInstaller);
  TBaseHelpInstaller       = class(TBaseLCLInstaller);
  //TBaseWinInstaller        = class(TInstaller);

  function GetMinimumFPCVersion(aCPU:TCPU=TCPU.cpuNone;aOS:TOS=TOS.osNone): string;

implementation

uses
  StrUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF UNIX}
  {$ifdef LCL}
  //For messaging to MainForm: no writeln
  Forms,
  //LMessages,
  LCLIntf,
  {$endif}
  process,
  FileUtil,
  LazFileUtils
  {$IF NOT DEFINED(HAIKU) AND NOT DEFINED(AROS) AND NOT DEFINED(MORPHOS)}
  //,ssl_openssl
  // for runtime init of openssl
  {$ifndef USEONLYCURL}
  {$IFDEF MSWINDOWS}
  //,blcksock, ssl_openssl_lib
  ,openssl
  {$ENDIF}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
  ,opensslsockets
  {$ENDIF}
  {$ENDIF}
  {$endif}
  ;


function GetMinimumFPCVersion(aCPU:TCPU=TCPU.cpuNone;aOS:TOS=TOS.osNone): string;
var
  localCPU:TCPU;
  localOS:TOS;
procedure SetResult(const aNewVersion:string; var aVersion:string);
begin
  if CalculateNumericalVersion(aNewVersion)>CalculateNumericalVersion(aVersion) then aVersion:=aNewVersion;
end;
begin
  result:='0.0.0';
  localCPU:=aCPU;
  localOS:=aOS;
  if ((localCPU=TCPU.cpuNone) AND (localOS=TOS.osNone)) then
  begin
    // We build native, so get native CPU and OS.
    localCPU:=GetTCPU(GetSourceCPU);
    localOS:=GetTOS(GetSourceOS);
  end;
  if (localCPU in [TCPU.loongarch64,TCPU.xtensa,TCPU.wasm32]) then SetResult('3.3.1',result);
  if (localOS in [TOS.freertos,TOS.wasi]) then SetResult('3.3.1',result);
  if (localOS in [TOS.aix]) then SetResult('3.0.0',result);
  if (localCPU=TCPU.aarch64) then
  begin
    SetResult('3.2.0',result);
    if (localOS in [TOS.darwin]) then SetResult('3.2.2',result);
    if (localOS in [TOS.win64,TOS.embedded,TOS.freebsd]) then SetResult('3.3.1',result);
  end;
  if (localOS=TOS.haiku) then
  begin
    if (localCPU=TCPU.i386) then SetResult('3.0.0',result);
    if (localCPU=TCPU.x86_64) then SetResult('3.2.0',result);
  end;
  if (localOS=TOS.ios) then
  begin
    if (localCPU in [TCPU.arm,TCPU.aarch64]) then SetResult('3.2.2',result);
  end;
  if (localOS=TOS.android) then
  begin
    if (localCPU in [TCPU.x86_64,TCPU.aarch64]) then SetResult('3.2.2',result);
  end;
  {$IF DEFINED(FPC_ABI_ELFV2)}
  if (localCPU=TCPU.powerpc64) then SetResult('3.2.0',result);
  {$ENDIF}
  {$IFDEF CPUZ80}
  SetResult('3.3.1',result);
  {$ENDIF}
  {$IF DEFINED(CPUMIPS64) OR DEFINED(CPUMIPS64EL)}
  SetResult('3.3.1',result);
  {$ENDIF}
end;

{ TInstaller }

function TInstaller.GetCrossInstaller: TCrossInstaller;
var
  idx: integer;
  target: string;
begin
  result:=nil;
  if ((CrossCPU_Target<>TCPU.cpuNone) AND (CrossOS_Target<>TOS.osNone)) then
  begin
    if (NOT Assigned(FCrossInstaller)) OR ((FCrossInstaller.TargetCPU<>CrossCPU_Target)  OR (FCrossInstaller.TargetOS<>CrossOS_Target)) then
    begin
      target := GetFPCTarget(false);
      FCrossInstaller:=nil;
      if assigned(CrossInstallers) then
        for idx := 0 to Pred(CrossInstallers.Count) do
          if CrossInstallers[idx] = target then
          begin
            FCrossInstaller:=TCrossInstaller(CrossInstallers.Objects[idx]);
            break;
          end;
    end;
    if (NOT Assigned(FCrossInstaller)) then
    begin
      Infoln(localinfotext+'Could not find crosscompiler logic for '+target+' !!',etError);
      Infoln(localinfotext+'This is a fatal error. Exception will be created.',etError);
      Infoln(localinfotext+'Please file a bug-report.',etError);
      raise Exception.CreateFmt('%s fpcup cross-logic not found. Please report this issue.',[target]);
    end
    else
    begin
      result:=FCrossInstaller;
    end;
  end;
end;

procedure TInstaller.SetURL(value:string);
begin
  FURL:=value;
  if (FURL <> '') and (FURL[Length(FURL)] <> '/') then
    FURL := FURL + '/';
  if (IsFPCInstaller OR IsLazarusInstaller) then
  begin
    FMajorVersion := -1;
    FMinorVersion := -1;
    FReleaseVersion := -1;
    FPatchVersion := -1;
  end;
end;

procedure TInstaller.SetSourceDirectory(value:string);
begin
  FSourceDirectory:=ExcludeTrailingPathDelimiter(value);
  if (IsFPCInstaller OR IsLazarusInstaller) then
  begin
    FMajorVersion := -1;
    FMinorVersion := -1;
    FReleaseVersion := -1;
    FPatchVersion := -1;
  end;
end;

procedure TInstaller.SetBaseDirectory(value:string);
begin
  FBaseDirectory:=value;
  if (Length(value)>0) then
  begin
    FBaseDirectory:=ExcludeTrailingPathDelimiter(value);
    ForceDirectoriesSafe(BaseDirectory);
    // Create default patch directories
    ForceDirectoriesSafe(ConcatPaths([BaseDirectory,'ccr']));
    ForceDirectoriesSafe(ConcatPaths([BaseDirectory,'patches']));
    ForceDirectoriesSafe(ConcatPaths([BaseDirectory,'patches','patchfpc']));
    ForceDirectoriesSafe(ConcatPaths([BaseDirectory,'patches','patchlazarus']));
    ForceDirectoriesSafe(ConcatPaths([BaseDirectory,'patches','patchuniversal']));
  end;
end;

procedure TInstaller.SetInstallDirectory(value:string);
begin
  FInstallDirectory:=ExcludeTrailingPathDelimiter(value);
  if (IsFPCInstaller OR IsLazarusInstaller) then
  begin
    ForceDirectoriesSafe(FInstallDirectory);
  end;
end;

procedure TInstaller.SetFPCInstallDirectory(value:string);
begin
  FFPCInstallDir:=ExcludeTrailingPathDelimiter(value);
  FFPCBinDir:=ConcatPaths([FFPCInstallDir,'bin',GetFPCTarget(true)]);
  if (IsFPCInstaller) then
    SetInstallDirectory(FFPCInstallDir);
end;

procedure TInstaller.SetFPCSourceDirectory(value:string);
begin
  FFPCSourceDir:=ExcludeTrailingPathDelimiter(value);
  if (IsFPCInstaller) then
    SetSourceDirectory(FFPCSourceDir);
end;

procedure TInstaller.SetLazarusInstallDirectory(value:string);
begin
  FLazarusInstallDir:=ExcludeTrailingPathDelimiter(value);
  if ((IsLazarusInstaller) OR (IsHelpInstaller)) then
    SetInstallDirectory(FLazarusInstallDir);
end;

procedure TInstaller.SetLazarusSourceDirectory(value:string);
begin
  FLazarusSourceDir:=ExcludeTrailingPathDelimiter(value);
  if ((IsLazarusInstaller) OR (IsHelpInstaller)) then
    SetSourceDirectory(FLazarusSourceDir);
end;

procedure TInstaller.SetLazarusPrimaryConfigDirectory(value:string);
begin
  // Martin Friebe mailing list January 2014: no quotes allowed, no trailing blanks
  FLazarusPrimaryConfigPath:=Trim(ExcludeTrailingPathDelimiter(value));
end;

procedure TInstaller.SetMakeDirectory(value:string);
begin
  FMakeDir:=ExcludeTrailingPathDelimiter(value);
  FMakePath:=FMakeDir+DirectorySeparator;
end;

procedure TInstaller.SetCompiler(value:string);
begin
  FCompiler:=value;
end;

procedure TInstaller.SetLinuxLegacy(value:boolean);
begin
  // Disable for now
  // To be further investigated
  // Libc versioned linking will be included in FPC sometime in 2024
  // Implementation by the FPC devs themselves
  //FLinuxLegacy:=value;
end;

function TInstaller.GetLinuxLegacy:boolean;
var
  aCPU:TCPU;
  aOS:TOS;
begin
  result:=false;
  if IsCross then
  begin
    aCPU:=CrossCPU_Target;
    aOS:=CrossOS_Target;
  end
  else
  begin
    aCPU:=GetTCPU(GetSourceCPU);
    aOS:=GetTOS(GetSourceOS);
  end;
  if ((aOS=TOS.linux) AND (aCPU in LEGACYCPU)) then
    result:=FLinuxLegacy;
end;

function TInstaller.GetMake: string;
const
  {$if (defined(BSD) and not defined(DARWIN)) or (defined(Solaris))}
  GNUMake='gmake';
  {$else}
  GNUMake='make';
  {$endif}
begin
  if FMake = '' then
    {$IFDEF MSWINDOWS}
    //Only use our own make !!
    FMake := MakePath + GNUMake + '.exe';
    {$ELSE}
    FMake:=Which(GNUMake);
    if FMake='' then
    begin
      Infoln(localinfotext+'Could not find '+GNUMake+' executable.',etError);
      Infoln(localinfotext+'This is a fatal error. Exception will be created.',etError);
      Infoln(localinfotext+'Please make sure it is installed.',etError);
      raise Exception.CreateFmt('%s not found. Please install %s',[GNUMake,GNUMake]);
    end;
    {$ENDIF MSWINDOWS}
  Result := FMake;
end;

function TInstaller.GetShell: string;
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF CPUX86}
  if FShell = '' then
  begin
    // disable for now .... not working 100%
    {
    // do we have a stray sh.exe in the path ...
    if StrayShell then
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

{$IFDEF MSWINDOWS}
function TInstaller.GetStrayShell: boolean;
begin
  result:=false;
  {$ifdef win32}
  //result:=(NOT CheckExecutable('echo',['''mytestrevision'''],'''mytestrevision''',false));
  {$endif}
  {$ifdef win64}
  //result:=(NOT CheckExecutable('echo',['''mytestrevision'''],'''mytestrevision''',false));
  {$endif}
  //if result then Infoln(infotext+'Found stray echo in path. Using gecho.exe command from custom binaries !',etWarning);
  result:=(Length(Which('sh.exe'))>0);
  if result then Infoln(infotext+'Found stray shell in path. Adjusting path !',etDebug);
end;
{$ENDIF MSWINDOWS}

procedure TInstaller.SetVerbosity(aValue:boolean);
begin
  FVerbose:=aValue;
  if Assigned(Processor) then Processor.Verbose:=FVerbose;
  {
  if Assigned(SVNClient) then SVNClient.Verbose:=FVerbose;
  if Assigned(GitClient) then GitClient.Verbose:=FVerbose;
  if Assigned(HGClient) then HGClient.Verbose:=FVerbose;
  }
end;

procedure TInstaller.SetHTTPProxyHost(AValue: string);
begin
  if FHTTPProxyHost=AValue then Exit;
  FHTTPProxyHost:=AValue;
  if Assigned(GitClient) then GitClient.HTTPProxyHost:=FHTTPProxyHost;
  if Assigned(HGClient) then HGClient.HTTPProxyHost:=FHTTPProxyHost;
  if Assigned(SVNClient) then SVNClient.HTTPProxyHost:=FHTTPProxyHost;
end;

procedure TInstaller.SetHTTPProxyPassword(AValue: string);
begin
  if FHTTPProxyPassword=AValue then Exit;
  FHTTPProxyPassword:=AValue;
  if Assigned(GitClient) then GitClient.HTTPProxyPassword:=FHTTPProxyPassword;
  if Assigned(HGClient) then HGClient.HTTPProxyPassword:=FHTTPProxyPassword;
  if Assigned(SVNClient) then SVNClient.HTTPProxyPassword:=FHTTPProxyPassword;
end;

procedure TInstaller.SetHTTPProxyPort(AValue: integer);
begin
  if FHTTPProxyPort=AValue then Exit;
  FHTTPProxyPort:=AValue;
  if Assigned(GitClient) then GitClient.HTTPProxyPort:=FHTTPProxyPort;
  if Assigned(HGClient) then HGClient.HTTPProxyPort:=FHTTPProxyPort;
  if Assigned(SVNClient) then SVNClient.HTTPProxyPort:=FHTTPProxyPort;
end;

procedure TInstaller.SetHTTPProxyUser(AValue: string);
begin
  if FHTTPProxyUser=AValue then Exit;
  FHTTPProxyUser:=AValue;
  if Assigned(GitClient) then GitClient.HTTPProxyUser:=FHTTPProxyUser;
  if Assigned(HGClient) then HGClient.HTTPProxyUser:=FHTTPProxyUser;
  if Assigned(SVNClient) then SVNClient.HTTPProxyUser:=FHTTPProxyUser;
end;

function TInstaller.InitInfoText(const ExtraInfo:string):string;
begin
  result:=TrimLeft(Copy(UnCamel(Self.ClassName),2,MaxInt))+ExtraInfo;
  //if (Length(ExtraInfo)>0) then result:=result+' '+ExtraInfo;
end;

function TInstaller.CheckAndGetTools: boolean;
var
  OperationSucceeded: boolean;
  s: string;
  aDir: string;
  {$ifdef MSWINDOWS}
  aURL,aFile: string;
  i:integer;
  {$endif}
  {$ifndef USEONLYCURL}
  CryptoSucceeded: boolean;
  {$endif}
begin
  OperationSucceeded := true;
  localinfotext:=InitInfoText(' (CheckAndGetTools): ');
  if (not FNeededExecutablesChecked) then
  begin
    // The extractors used depend on the bootstrap compiler URL/file we download
    // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)

    {$IFDEF MSWINDOWS}
    // Need to do it here so we can pick up make path.
    FGunzip := '';
    FTar := '';
    FUnrar := '';
    F7zip := '';
    FWget := '';
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FGunzip := 'gunzip';
    if FMUSL then
    begin
      FGunzip := 'unzip';
    end;
    FTar := 'tar';
    F7zip := '7za';
    FWget := 'wget';
    FUnrar := 'unrar';
    {$ENDIF LINUX}
    {$IFDEF BSD} //OSX, *BSD
    {$IFDEF DARWIN}
    FGunzip := ''; //not really necessary now
    FTar := 'bsdtar'; //gnutar is not available by default on Mavericks
    F7zip := '7za';
    FWget := 'wget';
    FUnrar := 'unrar';
    {$ELSE} //FreeBSD, OpenBSD, NetBSD
    FGunzip := 'gunzip';
    FTar := 'tar'; //At least FreeBSD tar apparently takes some gnu tar options nowadays.
    F7zip := '7za';
    FWget := 'wget';
    FUnrar := 'unrar';
    {$ENDIF DARWIN}
    {$ENDIF BSD}

    if (Length(FMakeDir)>0) then ForceDirectoriesSafe(FMakeDir);

    {$IFDEF MSWINDOWS}

    {$ifdef win64}
    // the standard make by FPC does not work when Git is present (and in the path), but this one works ??!!
    // (but the FPC installer sets its own path to isolate itself from the system, so FPC make still works)
    // strange, but do not enable (yet) !!
    // Download('ftp://ftp.equation.com/make/'+{$ifdef win64}'64'{$else}'32'{$endif}+'/'+ExtractFileName(Make), Make);
    {$endif}

    {$ifndef USEONLYCURL}
    if OperationSucceeded then
    begin

      CryptoSucceeded:=IsSSLloaded;
      if (NOT CryptoSucceeded) then
      begin
        InitSSLInterface;
        CryptoSucceeded:=IsSSLloaded;
      end;

      if (NOT CryptoSucceeded) then
      begin
        i:=Low(SSL_DLL_Names);
        while ( (not CryptoSucceeded) AND (i<=High(SSL_DLL_Names)) ) do
        begin
          CryptoSucceeded:=(FileExists(SafeGetApplicationPath+Crypto_DLL_Names[i]+GetLibExt)) AND (FileExists(SafeGetApplicationPath+SSL_DLL_Names[i]+GetLibExt));
          Inc(i);
        end;
        if (NOT CryptoSucceeded) then
        begin
          Infoln(localinfotext+'Getting OpenSSL library files.',etInfo);
          DownloadOpenSSL;
          DestroySSLInterface; // disable ssl and release libs
        end;
        if (NOT IsSSLloaded) then
        begin
          InitSSLInterface;
          CryptoSucceeded:=IsSSLloaded;
        end;
      end;
    end;

    if (NOT CryptoSucceeded) then
      Infoln(localinfotext+'Could not init SSL interface.',etWarning);
    {$endif}

    with SVNClient do
    begin
      OperationSucceeded:=False;
      // try to find systemwide SVN
      if (NOT ForceLocal) then
      begin
        OperationSucceeded:=ValidClient;
      end;
      // try to find fpcupdeluxe SVN
      if (NOT OperationSucceeded) then
        OperationSucceeded:=FindSVNSubDirs;
      if (NOT OperationSucceeded) then
      begin
        Infoln(localinfotext+'Going to download SVN',etInfo);
        // Download will look in and below FSVNDirectory
        // and set FSVNClient.SVNExecutable if succesful
        OperationSucceeded:=DownloadSVN;
      end;
      if (OperationSucceeded AND (RepoExecutable<>EmptyStr)) then
      begin
        OperationSucceeded:=CheckExecutable(RepoExecutable, ['--version'], '');
      end;
      // do not fail: SVN is not essential anymore !
      OperationSucceeded:=True;
    end;

    // Regardless of platform, SVN should now be either set up correctly or we should give up.
    if (NOT OperationSucceeded) then
      Infoln(localinfotext+'Could not find SVN executable. Please make sure it is installed.',etError)
    else
      Infoln(localinfotext+'SVN client found: ' + SVNClient.RepoExecutable+'.',etDebug);

    {$ifndef USEONLYCURL}
    FWget:=Which('wget'+GetExeExt);
    if Not FileExists(FWget) then FWget := MakePath + 'wget\wget.exe';
    {$ifdef win32}
    if Not FileExists(FWget) then FWget := MakePath + 'wget\wget-openssl-x86.exe';
    {$endif}
    {$ifdef win64}
    if Not FileExists(FWget) then FWget := MakePath + 'wget\wget-openssl-x64.exe';
    {$endif}
    if Not FileExists(FWget) then
    begin
      Infoln(localinfotext+'Getting Wget.',etInfo);
      DownloadWget;
      OperationSucceeded:=FileExists(FWget);
      // do not fail
      OperationSucceeded:=True;
    end;
    //Set static wget binary location
    TUseWGetDownloader.WGETBinary:=FWget;
    {$endif}

    OperationSucceeded:=false;

    // Get patch binary from default binutils URL
    aFile:=MakePath + 'patch.exe';
    if (Not FileExists(aFile)) then
    begin
      //aURL:=FPCGITLABBUILDBINARIES+'/-/raw/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/';
      aURL:=FPCGITLABBUILDBINARIES+'/-/raw/main/install/binw32/';
      GetFile(aURL+'patch.exe',aFile);
      GetFile(aURL+'patch.exe.manifest',aFile + '.manifest');
    end;
    if FileExists(aFile) then FPatchCmd:=aFile else
    begin
      aFile:=ExtractFilePathSafe(FPatchCmd);
      if (Not FileExists(aFile)) then
        aFile:=Which('patch.exe');
      if FileExists(aFile) then FPatchCmd:=aFile;
    end;

    // Get pwd binary from default binutils URL. If its not there, the make clean command will fail
    aFile:=MakePath + 'pwd.exe';
    if (Not FileExists(aFile)) then
    begin
      //aURL:=FPCGITLABBUILDBINARIES+'/-/raw/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/';
      aURL:=FPCGITLABBUILDBINARIES+'/-/raw/main/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/';
      GetFile(aURL+'pwd.exe',aFile);
    end;

    // do not fail
    OperationSucceeded:=True;

    {$ENDIF MSWINDOWS}

    if (Length(FMakeDir)>0) then
      aDir:=ConcatPaths([FMakeDir,'7Zip'])
    else
      aDir:=ConcatPaths([BaseDirectory,DEFAULTBOOTSTRAPDIR,'7Zip']);

    F7zip:=Which('7z');
    if Not FileExists(F7zip) then Which('7za');

    if GetTOS(GetSourceOS) in [TOS.darwin] then
    begin
      if Not FileExists(F7zip) then Which('7zx');
      if Not FileExists(F7zip) then Which('7zX');
    end;

    if Not FileExists(F7zip) then Which('7zz');
    s:='7za';

    if Not FileExists(F7zip) then F7zip := ConcatPaths([aDir,s+GetExeExt]);

    if (NOT (GetTOS(GetSourceOS) in WINDOWS_OS)) then
    begin
      s:='7zz';
      if Not FileExists(F7zip) then F7zip := ConcatPaths([aDir,s]);
    end;

    if (NOT FileExists(F7zip)) then
    begin
      if ForceDirectoriesSafe(ExtractFileDir(F7zip)) then
      begin
        {$IFDEF DARWIN}
        {$IFDEF CPUPOWERPC}
        s:='';
        {$ELSE}
        s:=s+'_all-apple';
        {$ENDIF CPUPOWERPC}
        {$ELSE DARWIN}
        {$IF defined(LINUX) OR defined(WINDOWS)}
        s:=s+'_'+GetSourceCPUOS+GetExeExt;
        {$ELSE}
        s:='';
        {$ENDIF}
        {$ENDIF DARWIN}
        if (Length(s)>0) then
        begin
          Infoln(localinfotext+'Missing 7z unzipper. Trying to get it and install in '+ExtractFileDir(F7zip),etInfo);
          OperationSucceeded:=GetFile(FPCUPTOOLS+'/'+s,F7zip);
          if OperationSucceeded then OperationSucceeded:=FileExists(F7zip);
          {$IFDEF UNIX}
          if OperationSucceeded then fpChmod(F7zip,&755);
          {$ENDIF UNIX}
          if (NOT OperationSucceeded) then F7zip:='7za'+GetExeExt;
          // do not fail ... perhaps there is another 7zip available in the path
        end;
        OperationSucceeded:=True;
      end;
    end;

    {$IFDEF MSWINDOWS}

    if (NOT Ultibo) then
    begin
      //Unrar, HG and GIT not needed for Ultibo

      FUnrar := MakePath + 'unrar\bin\unrar.exe';
      if Not FileExists(FUnrar) then
      begin
        ForceDirectoriesSafe(MakePath+'unrar');
        //ForceDirectoriesSafe(MakePath+'unrar\bin');
        // this version of unrar does not need installation ... so we can silently get it !!
        s:='unrar-3.4.3-bin.zip';
        SysUtils.DeleteFile(MakePath+'unrar\'+s);
        aURL:=FPCUPGITREPO+'/releases/download/windowsi386bins_v1.0/';
        OperationSucceeded:=GetFile(aURL+s,MakePath+'unrar\'+s);
        // sometimes, souceforge has a redirect error, returning a successfull download, but without the datafile itself
        if (FileSize(MakePath+'unrar\'+s)<50000) then
        begin
          SysUtils.DeleteFile(MakePath+'unrar\'+s);
          OperationSucceeded:=false;
        end;
        if NOT OperationSucceeded then
        begin
          // try one more time
          SysUtils.DeleteFile(MakePath+'unrar\'+s);
          aURL:='https://downloads.sourceforge.net/project/gnuwin32/unrar/3.4.3/';
          OperationSucceeded:=GetFile(aURL+s,MakePath+'unrar\'+s);
          // sometimes, souceforge has a redirect error, returning a successfull download, but without the datafile itself
          if (FileSize(MakePath+'unrar\'+s)<50000) then
          begin
            SysUtils.DeleteFile(MakePath+'unrar\'+s);
            OperationSucceeded:=false;
          end;
        end;
        if OperationSucceeded then
        begin
          with TNormalUnzipper.Create do
          begin
            try
              OperationSucceeded:=DoUnZip(MakePath+'unrar\'+s,MakePath+'unrar\',[]);
            finally
              Free;
            end;
          end;

          if OperationSucceeded then
          begin
            SysUtils.DeleteFile(MakePath+'unrar\'+s);
            OperationSucceeded:=FileExists(FUnrar);
          end;
        end;
        // do not fail ... perhaps there is another unrar available in the path ... or use 7zip on windows
        OperationSucceeded:=True;
      end;


      with GitClient do
      begin
        OperationSucceeded:=False;
        aFile:=MakePath+'git'+DirectorySeparator+'cmd'+DirectorySeparator+RepoExecutableName+GetExeExt;
        // try to find systemwide GIT
        if (NOT ForceLocal) then
        begin
          OperationSucceeded:=ValidClient;
        end;
        // try to find fpcupdeluxe GIT
        if (NOT OperationSucceeded) then
        begin
          OperationSucceeded:=FileExists(aFile);
          if OperationSucceeded then RepoExecutable:=aFile;
        end;
        if (NOT OperationSucceeded) then
        begin
          //Source:
          //https://github.com/git-for-windows
          //install GIT only on Windows Vista and higher
          if CheckWin32Version(6,0) then
          begin
            ForceDirectoriesSafe(MakePath+'git');
            {$ifdef win32}
            //s:='git32.7z';
            s:='git32.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.17.1.windows.2/MinGit-2.17.1.2-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.18.0.windows.1/MinGit-2.18.0-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.19.0.windows.1/MinGit-2.19.0-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.21.0.windows.1/MinGit-2.21.0-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.22.0.windows.1/MinGit-2.22.0-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.23.0.windows.1/MinGit-2.23.0-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.24.1.windows.2/MinGit-2.24.1.2-32-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.25.1.windows.1/MinGit-2.25.1-32-bit.zip';
            aURL:='https://github.com/git-for-windows/git/releases/download/v2.33.0.windows.2/MinGit-2.33.0.2-32-bit.zip';
            {$else}
            //s:='git64.7z';
            s:='git64.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.17.1.windows.2/MinGit-2.17.1.2-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.18.0.windows.1/MinGit-2.18.0-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.19.0.windows.1/MinGit-2.19.0-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.21.0.windows.1/MinGit-2.21.0-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.22.0.windows.1/MinGit-2.22.0-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.23.0.windows.1/MinGit-2.23.0-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.24.1.windows.2/MinGit-2.24.1.2-64-bit.zip';
            //aURL:='https://github.com/git-for-windows/git/releases/download/v2.25.1.windows.1/MinGit-2.25.1-64-bit.zip';
            aURL:='https://github.com/git-for-windows/git/releases/download/v2.33.0.windows.2/MinGit-2.33.0.2-64-bit.zip';
            {$endif}
            Infoln(localinfotext+'GIT client not found. Downloading it',etInfo);
            Infoln(localinfotext+'GIT client download (may take time) from '+aURL,etDebug);
            OperationSucceeded:=GetFile(aURL,MakePath+'git\'+s);
            if NOT OperationSucceeded then
            begin
              // try one more time
              SysUtils.DeleteFile(MakePath+'git\'+s);
              OperationSucceeded:=GetFile(aURL,MakePath+'git\'+s);
            end;
            if OperationSucceeded then
            begin
              Infoln(localinfotext+'GIT client download ready: unpacking (may take time).',etInfo);
              with TNormalUnzipper.Create do
              begin
                try
                  OperationSucceeded:=DoUnZip(MakePath+'git\'+s,MakePath+'git\',[]);
                finally
                  Free;
                end;
              end;
              if OperationSucceeded then
              begin
                SysUtils.DeleteFile(MakePath+'git\'+s);
                OperationSucceeded:=FileExists(aFile);
                //Copy certificate ... might be necessary
                //aURL:=MakePath+'git\mingw32\';
                //if (NOT FileExists(aURL+'bin\curl-ca-bundle.crt')) then FileCopy(aURL+'ssl\certs\ca-bundle.crt',aURL+'bin\curl-ca-bundle.crt');
              end;
            end;
            if OperationSucceeded then RepoExecutable:=aFile else RepoExecutable:=RepoExecutableName+GetExeExt;
          end;
        end;
        if (OperationSucceeded AND (RepoExecutable<>EmptyStr)) then
        begin
          // check exe
          OperationSucceeded:=CheckExecutable(RepoExecutable, ['--version'], '');
        end;
      end;

      with HGClient do
      begin
        OperationSucceeded:=False;
        aFile:=MakePath+'hg'+DirectorySeparator+RepoExecutableName+GetExeExt;
        // try to find systemwide HG
        if (NOT ForceLocal) then
        begin
          OperationSucceeded:=ValidClient;
        end;
        // try to find fpcupdeluxe HG
        if (NOT OperationSucceeded) then
        begin
          OperationSucceeded:=FileExists(aFile);
          if OperationSucceeded then RepoExecutable:=aFile;
        end;
        if (NOT OperationSucceeded) then
        begin
          //original source from : https://www.mercurial-scm.org/
          {$ifdef win32}
          s:='hg32.zip';
          aURL:=FPCUPGITREPO+'/releases/download/windowsi386bins_v1.0/'+s;
          {$else}
          s:='hg64.zip';
          aURL:=FPCUPGITREPO+'/releases/download/windowsx64bins_v1.0/'+s;
          {$endif}
          ForceDirectoriesSafe(MakePath+'hg');
          Infoln(localinfotext+'HG (mercurial) client not found. Downloading it',etInfo);
          Infoln(localinfotext+'HG (mercurial) client download (may take time) from '+aURL,etDebug);
          OperationSucceeded:=GetFile(aURL,MakePath+'hg\'+s);
          if NOT OperationSucceeded then
          begin
            // try one more time
            SysUtils.DeleteFile(MakePath+'hg\'+s);
            OperationSucceeded:=GetFile(aURL,MakePath+'hg\'+s);
          end;
          if OperationSucceeded then
          begin
            Infoln(localinfotext+'HG download ready: unpacking (may take time).',etInfo);
            with TNormalUnzipper.Create do
            begin
              try
                OperationSucceeded:=DoUnZip(MakePath+'hg\'+s,MakePath+'hg\',[]);
              finally
                Free;
              end;
            end;
            if OperationSucceeded then
            begin
              OperationSucceeded:=FileExists(aFile);
            end;
          end;
          SysUtils.DeleteFile(MakePath+'hg\'+s);
          if OperationSucceeded then RepoExecutable:=aFile else RepoExecutable:=RepoExecutableName+GetExeExt;
        end;
        if RepoExecutable <> EmptyStr then
        begin
          // check exe, but do not fail: HG is not 100% essential !
          CheckExecutable(RepoExecutable, ['--version'], '');
        end;
        // do not fail: HG is not 100% essential !
        OperationSucceeded:=True;
      end;

    end;

    {$ENDIF MSWINDOWS}


    {$IF defined(LINUX) or (defined(BSD) and (not defined(DARWIN)))} //Linux,FreeBSD,NetBSD,OpenBSD, but not OSX
    //todo: check if we need as on OSX as well
    if OperationSucceeded then
    begin
      // Check for proper assembler
      try
        if NOT CheckExecutable('as', ['--version'], '') then
        begin
          Infoln(localinfotext+'Missing assembler as. Please install the developer tools.',etError);
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
      // Check for valid gunzip executable, if it is needed
      if FGunzip <> EmptyStr then
      begin
        if (NOT FMUSL) then
        begin
          OperationSucceeded := CheckExecutable(FGunzip, ['--help'], '');
          if (NOT OperationSucceeded) then
          begin
            Infoln(localinfotext+FGunzip+' not found.',etDebug);
            FGunzip:='gzip';
            OperationSucceeded := CheckExecutable(FGunzip, ['--help'], '');
            if (NOT OperationSucceeded) then
            begin
              Infoln(localinfotext+'No .gz uncompressor found.',etInfo);
            end;
          end;
        end;
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid tar executable, if it is needed
      if FTar <> EmptyStr then
      begin
        OperationSucceeded := CheckExecutable(FTar, ['--version'], '');
        if (NOT OperationSucceeded) then Infoln(localinfotext+FTar+' not found.',etDebug);
      end;
    end;

    if OperationSucceeded then
    begin
    {$IFDEF MSWINDOWS}
      // check if we have make ... otherwise get it from standard URL
      if (NOT FileExists(Make)) then
      begin
        //aURL:=FPCGITLABBUILDBINARIES+'/-/raw/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/';
        //aURL:=FPCTRUNKBINARIES+'/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/'+ExtractFileName(Make);
        aURL:=FPCGITLABBUILDBINARIES+'/-/raw/main/install/binw'+{$ifdef win64}'64'{$else}'32'{$endif}+'/';
        Infoln(localinfotext+'Make binary not found. Getting it.',etInfo);
        Infoln(localinfotext+'Make binary download from: '+aURL+'.',etDebug);
        GetFile(aURL+ExtractFileName(Make),Make);
        OperationSucceeded:=FileExists(Make);
      end;
    {$ELSE}
      OperationSucceeded := CheckExecutable(Make, ['-v'], '');
      if (NOT OperationSucceeded) then Infoln(localinfotext+Make+' not found.',etError);
    {$ENDIF}
    end;

    FNeededExecutablesChecked:=OperationSucceeded;
  end;
  Result := OperationSucceeded;
  localinfotext:=infotext;
end;

function TInstaller.CheckAndGetNeededBinUtils: boolean;
var
  {$IFDEF MSWINDOWS}
  AllThere           : boolean;
  i                  : integer;
  InstallPath        : string;
  {$ENDIF MSWINDOWS}
  OperationSucceeded : boolean;
  {$IFDEF LINUX}
  s1                 : string;
  {$ENDIF}
begin
  OperationSucceeded := true;
  localinfotext:=InitInfoText(' (DownloadBinUtils): ');
  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Download if needed, including unzip - needed for SVN download
    // Check for binutils directory
    AllThere:=true;
    if DirectoryExists(FMakeDir) = false then
    begin
      AllThere:=false;
    end
    else
    begin
      // Check all binutils in directory
      for i:=low(FUtilFiles) to high(FUtilFiles) do
      begin
        if FUtilFiles[i].Category in [ucBinutil,ucDebugger32,ucDebugger64] then
        begin
          InstallPath:=MakePath;
          if (FUtilFiles[i].Category in [ucDebugger32,ucDebugger64]) then
          begin
            if (FUtilFiles[i].Category=ucDebugger32) then InstallPath:=InstallPath+'gdb\i386-win32\';
            if (FUtilFiles[i].Category=ucDebugger64) then InstallPath:=InstallPath+'gdb\x86_64-win64\';
          end;
          if (NOT FileExists(InstallPath+FUtilFiles[i].FileName)) then
          begin
            AllThere:=false;
            break;
          end;
          if (FileSize(InstallPath+FUtilFiles[i].FileName)=0) then
          begin
            AllThere:=false;
            break;
          end;
        end;
      end;
    end;
    if not(AllThere) then
    begin
      Infoln(localinfotext+'Make path [' + FMakeDir + '] does not have (all) binutils.',etDebug);
      Infoln(localinfotext+'Going to download needed binutils.',etInfo);
      //Infoln(localinfotext+'Some binutils missing: going to get them.',etInfo);
      OperationSucceeded := DownloadBinUtils;
    end;
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded then
  begin

    // Check for proper make executable
    if OperationSucceeded then
    try
      if (NOT CheckExecutable(Make, ['-v'], 'GNU Make')) then
      begin
        if CheckExecutable(Make, ['-v'], '') then
        begin
          Infoln(localinfotext+'Found make binary here: '+Make+'. But it is not GNU Make.',etError);
          OperationSucceeded := false;
        end;
      end;
    except
      // ignore errors, this is only an extra check
    end;

    {$IFDEF LINUX}
    // Check for proper ld executable
    if OperationSucceeded then
    try
      s1:=Which('ld');
      if (NOT CheckExecutable(s1, ['-v'], 'GNU ld')) then
      begin
        Infoln(localinfotext+'Found ld binary here: '+s1+'. But it is not GNU ld. Expect errors',etWarning);
        s1:=Which('ld.bfd');
        if (NOT CheckExecutable(s1, ['-v'], 'GNU ld')) then
        begin
          Infoln(localinfotext+'Found GNU ld.bfd binary here: '+s1+'. Could be used through symlinking.',etWarning);
        end;
        OperationSucceeded := true;
      end;
    except
      // ignore errors, this is only an extra check
    end;
    {$ENDIF LINUX}
  end;

  Result := OperationSucceeded;
  localinfotext:=infotext;
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
      Write(DiffFile, HGClient.GetDiffAll);
    end
    else if RepoClass is TGitClient then
    begin
      Write(DiffFile, GitClient.GetDiffAll);
    end
    else if RepoClass is TSVNClient then
    begin
      Write(DiffFile, SVNClient.GetDiffAll);
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

function TInstaller.DownloadFromBase(aClient:TRepoClient; aModuleName: string; var aBeforeRevision,
  aAfterRevision: string; UpdateWarnings: TStringList): boolean;
const
  MAXFPCREVISION      = 49000;
  MAXLAZARUSREVISION  = 65000;
var
  ReturnCode: integer;
  DiffFile,DiffFileCorrectedPath: String;
  LocalPatchCmd : string;
  NoPatchStrip:boolean;
  s,Output:string;
  SVNRevision:integer;

begin
  Result := false;

  localinfotext:=InitInfoText(' ('+Copy(aClient.ClassName,2,MaxInt)+': '+aModuleName+'): ');

  // check if we do have a client !!
  if NOT aClient.ValidClient then
  begin
    Infoln(localinfotext+aClient.RepoExecutableName+' is needed, but cannot be found on the system !!',etWarning);
    exit;
  end;

  //aBeforeRevision         := 'failure';
  aAfterRevision          := 'failure';
  //aClient.Verbose         := FVerbose;
  aClient.LocalRepository := SourceDirectory;
  aClient.Repository      := FURL;
  aClient.ModuleName      := aModuleName;

  //aBeforeRevision:=aClient.LocalRevision;

  if ((aModuleName=_FPC) OR (aModuleName=_LAZARUS)) AND (aClient is TGitClient)  then
  begin
    Output:=(aClient as TGitClient).GetSVNRevision;
    SVNRevision:=StrToIntDef(Output,0);
    if (SVNRevision=0) then Output:='';
    if (aModuleName=_FPC) AND (SVNRevision>MAXFPCREVISION) then Output:='';
    if (aModuleName=_LAZARUS) AND (SVNRevision>MAXLAZARUSREVISION) then Output:='';
    if (Length(Output)>0) then
      aBeforeRevision := Output
    else
      begin
        Output:=(aClient as TGitClient).LocalRevision;
        if (Length(Output)>0) then
          aBeforeRevision := Output;
      end;
  end;

  if Assigned(UpdateWarnings) then
  begin
    if (aBeforeRevision<>FRET_UNKNOWN_REVISION) then
    begin
      aClient.LocalModifications(UpdateWarnings); //Get list of modified files
      if UpdateWarnings.Count > 0 then
      begin
        UpdateWarnings.Insert(0, {BeginSnippet+' '+}aModuleName + ': WARNING: found modified files.');
        if FKeepLocalChanges=false then
        begin
          DiffFile:=IncludeTrailingPathDelimiter(SourceDirectory) + DIFFMAGIC + aBeforeRevision + '.diff';
          CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,aClient);
          UpdateWarnings.Add({BeginSnippet+' '+}aModuleName + ': reverting to original before updating.');
          aClient.Revert; //Remove local changes
        end
        else UpdateWarnings.Add({BeginSnippet+' '+}aModuleName + ': leaving modified files as is before updating.');
      end;
    end;
  end;

  aClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  aClient.DesiredBranch := FBranch; //We want to update to this specific branch
  aClient.DesiredTag := FTAG; //We want to update to this specific tag

  // Setting a DesiredTag also set ExportOnly when using GIT.
  // Override this setting if false with global setting
  if (NOT aClient.ExportOnly) then aClient.ExportOnly:=FExportOnly;

  Output:=localinfotext+'Running '+UpperCase(aClient.RepoExecutableName)+' checkout or update';
  ReturnCode:=Length(aClient.DesiredRevision);
  if (ReturnCode>0) then Output:=Output+' of revision '+aClient.DesiredRevision;
  ReturnCode:=Length(aClient.DesiredBranch);
  if (ReturnCode>0) then Output:=Output+' of branch '+aClient.DesiredBranch;
  ReturnCode:=Length(aClient.DesiredTag);
  if (ReturnCode>0) then Output:=Output+' of tag '+aClient.DesiredTag;
  Output:=Output+'.';
  Infoln(Output,etInfo);

  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  aClient.CheckOutOrUpdate;

  ReturnCode := aClient.ReturnCode;
  case ReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      WritelnLog(etError, localinfotext+'Repository URL in local directory and remote repository don''t match.', true);
      WritelnLog(localinfotext+'Local directory: ' + aClient.LocalRepository, true);
      Infoln(localinfotext+'Have you specified the wrong directory or a directory with an old repository checkout?',etDebug);
    end;
    AbortedExitCode:
    begin
      FRepositoryUpdated := false;
      Result := false;
      WritelnLog(etError, localinfotext+'Download aborted.', true);
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
        if ((aModuleName=_FPC) OR (aModuleName=_LAZARUS)) AND (aClient is TGitClient)  then
        begin
          Output:=(aClient as TGitClient).GetCommitName;
          if (Length(Output)>0) then Infoln(localinfotext+'Current commit name: '+Output,etInfo);
          Output:=(aClient as TGitClient).GetCommitMessage;
          if (Length(Output)>0) then Infoln(localinfotext+'Current commit message: '+Output,etInfo);

          // Process SVN inof or settings, if any
          // Bit tricky
          // To be removed
          Output:=(aClient as TGitClient).GetSVNRevision;
          SVNRevision:=StrToIntDef(Output,0);
          if (SVNRevision=0) then Output:='';
          if (aModuleName=_FPC) AND (SVNRevision>MAXFPCREVISION) then Output:='';
          if (aModuleName=_LAZARUS) AND (SVNRevision>MAXLAZARUSREVISION) then Output:='';
          if (Length(Output)>0) then
          begin
            if (Length(FDesiredRevision)>0) AND (Length(FDesiredRevision)<7) AND (Output<>FDesiredRevision) then
            begin
              aClient.DesiredRevision:=FDesiredRevision;
              s:=(aClient as TGitClient).GetGitHash;
              if (Length(s)>0) then
              begin
                aClient.DesiredTag := s;
                aClient.CheckOutOrUpdate;
              end;
            end;
            Output:=(aClient as TGitClient).GetSVNRevision;
            if (Length(Output)>0) then aAfterRevision:=Output;
          end;

        end;
      end;

      if (aClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (aBeforeRevision <> aAfterRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      if FReApplyLocalChanges and (DiffFile<>'') then
      begin
         Output:='';

         if Assigned(UpdateWarnings) then UpdateWarnings.Add(aModuleName + ': reapplying local changes.');

         for NoPatchStrip in boolean do
         begin

           s:=ExtractFileNameSafe(FPatchCmd);
           if ((s='patch'+GetExeExt) OR (s='gpatch'+GetExeExt)) then
           begin
             if NoPatchStrip then
               LocalPatchCmd:=FPatchCmd + ' -t -p0 -i '
             else
               LocalPatchCmd:=FPatchCmd + ' -t -p1 -i ';
           end
           else
           begin
             if NoPatchStrip then continue;
             LocalPatchCmd:=Trim(FPatchCmd) + ' ';
           end;

           ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, SourceDirectory, Output, FVerbose);

           if (ReturnCode=0) then break;

           // Patching can go wrong when line endings are not compatible
           // This happens e.g. with bgracontrols that have CRLF in the source files
           // Try to circumvent this problem by replacing line enddings
           if Pos('different line endings',Output)>0 then
           begin
             //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, SourceDirectory, FVerbose);
             //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, SourceDirectory, FVerbose);
             DiffFileCorrectedPath:=IncludeTrailingPathDelimiter(GetTempDirName)+ExtractFileName(DiffFile);
             if FileCorrectLineEndings(DiffFile,DiffFileCorrectedPath) then
             begin
               if FileExists(DiffFileCorrectedPath) then
               begin
                 ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFileCorrectedPath, SourceDirectory, Output, FVerbose);
                 DeleteFile(DiffFileCorrectedPath);
               end;
             end;
           end;

         end;

         // Report error, but continue !
         if ReturnCode<>0 then
         begin
           WritelnLog(etError, localinfotext+'Patching with ' + DiffFile + ' failed.', true);
           WritelnLog(localinfotext+'Output: ' + Output, true);
           WritelnLog(localinfotext+'Verify the state of the source, correct and rebuild with make.', true);
         end;
       end;
    end;
  end;
end;

function TInstaller.DownloadFromHG(aModuleName: string; var aBeforeRevision,
  aAfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  result:=DownloadFromBase(HGClient,aModuleName,aBeforeRevision,aAfterRevision,UpdateWarnings);
end;

function TInstaller.DownloadFromGit(aModuleName: string; var aBeforeRevision,
  aAfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  result:=DownloadFromBase(GitClient,aModuleName,aBeforeRevision,aAfterRevision,UpdateWarnings);
end;

function TInstaller.DownloadFromSVN(aModuleName: string; var aBeforeRevision, aAfterRevision: string; UpdateWarnings: TStringList): boolean;
var
  CheckoutOrUpdateReturnCode: integer;
  DiffFile: String;
  RepoExists: boolean;
  LocalPatchCmd : string;
  s,Output: string;
  {$IFNDEF MSWINDOWS}
  DiffFileSL:TStringList;
  {$ENDIF}
begin
  result:=true;

  localinfotext:=InitInfoText(' (DownloadFromSVN: '+aModuleName+'): ');

  // check if we do have a client !!
  if NOT SVNClient.ValidClient then
  begin
    Infoln(localinfotext+SVNClient.RepoExecutableName+' is needed, but cannot be found on the system !!',etWarning);
    exit;
  end;

  //aBeforeRevision             := 'failure';
  aAfterRevision              := 'failure';
  //SVNClient.Verbose           := FVerbose;
  SVNClient.LocalRepository   := SourceDirectory;
  SVNClient.Repository        := FURL;
  SVNClient.ExportOnly        := FExportOnly;
  SVNClient.ModuleName        := aModuleName;

  RepoExists:=SVNClient.LocalRepositoryExists;
  if RepoExists then
  begin
    if SVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
      aBeforeRevision := SVNClient.LocalRevisionWholeRepo
    else
      aBeforeRevision := FSVNClient.LocalRevision;
  end
  else
  begin
    if (SVNClient.ReturnCode=FRET_LOCAL_REMOTE_URL_NOMATCH) then
    begin
    end;

    // We could insist on the repo existing, but then we wouldn't be able to checkout!!
    WritelnLog('Directory ' + SourceDirectory + ' is not an SVN repository (or a repository with the wrong remote URL).');
    if not(DirectoryExists(SVNClient.LocalRepository)) then
    begin
      WritelnLog(localinfotext+'Creating directory '+SVNClient.LocalRepository+' for SVN checkout.');
      ForceDirectoriesSafe(SVNClient.LocalRepository);
    end;
  end;

  if (SVNClient.LocalRevisionWholeRepo = FRET_UNKNOWN_REVISION) and (SVNClient.Returncode=FRET_WORKING_COPY_TOO_OLD) then
  begin
    WritelnLog(etError, localinfotext+'The working copy in ' + SourceDirectory + ' was created with an older, incompatible version of svn.', true);
    WritelnLog(etError, localinfotext+'Run svn upgrade in the directory or make sure the original svn executable is the first in the search path.', true);
    result := false;  //fail
    exit;
  end;

  if RepoExists then
  begin
    SVNClient.LocalModifications(UpdateWarnings); //Get list of modified files
    DiffFile:='';
    if UpdateWarnings.Count > 0 then
    begin
      UpdateWarnings.Insert(0, {BeginSnippet+' '+}aModuleName + ': WARNING: found modified files.');
      if FKeepLocalChanges=false then
      begin
        DiffFile:=IncludeTrailingPathDelimiter(SourceDirectory) + DIFFMAGIC + aBeforeRevision + '.diff';
        CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,FSVNClient);
        UpdateWarnings.Add({BeginSnippet+' '+}aModuleName + ': WARNING: reverting before updating.');
        SVNClient.Revert; //Remove local changes
      end
      else UpdateWarnings.Add({BeginSnippet+' '+}aModuleName + ': WARNING: leaving modified files as is before updating.');
    end;
  end;

  SVNClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision

  Output:=localinfotext+'Running '+UpperCase(SVNClient.RepoExecutableName)+' checkout or update';
  if Length(SVNClient.DesiredRevision)>0 then
    Output:=Output+' of revision '+SVNClient.DesiredRevision;
  Output:=Output+'.';
  Infoln(Output,etInfo);

  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  SVNClient.CheckOutOrUpdate;

  CheckoutOrUpdateReturnCode := SVNClient.ReturnCode;
  case CheckoutOrUpdateReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      WritelnLog(etError, localinfotext+'Repository URL in local directory and remote repository don''t match.', true);
      WritelnLog(localinfotext+'Local directory: ' + SVNClient.LocalRepository, true);
      Infoln(localinfotext+'Have you specified the wrong directory or a directory with an old repository checkout?',etDebug);
    end;
    AbortedExitCode:
    begin
      FRepositoryUpdated := false;
      Result := false;
      WritelnLog(etError, localinfotext+'Download aborted.', true);
    end;
    else
    begin
      // If there are svn errors, return a false result.
      // We used to do a check for the revision, but that does not check the integrity
      // or existence of all files in the svn repo.

      if FExportOnly then
      begin
        aAfterRevision := FDesiredRevision;
        if Trim(aAfterRevision)='' then aAfterRevision := SVNClient.LocalRevisionWholeRepo;
      end
      else
      begin
        if SVNClient.LocalRevision=SVNClient.LocalRevisionWholeRepo then
          aAfterRevision := SVNClient.LocalRevisionWholeRepo
        else
          aAfterRevision := SVNClient.LocalRevision;
      end;

      if (SVNClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (aBeforeRevision <> SVNClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      // Only return success if svn returned return code 0
      Result := (CheckoutOrUpdateReturnCode=0);

      if not Result then
      begin
        WritelnLog(localinfotext+'SVN gave error code: '+IntToStr(CheckoutOrUpdateReturnCode));
        WritelnLog(localinfotext+'SVN gave error message: '+SVNClient.ReturnOutput);
      end;

      if Result and FReApplyLocalChanges and (DiffFile<>'') then
      begin
        Output:='';

        if Assigned(UpdateWarnings) then UpdateWarnings.Add(aModuleName + ': reapplying local changes.');

        s:=ExtractFileNameSafe(FPatchCmd);
        if ((s='patch'+GetExeExt) OR (s='gpatch'+GetExeExt))
           then LocalPatchCmd:=FPatchCmd + ' -t -p0 -i '
           else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
        CheckoutOrUpdateReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, SourceDirectory, Output, FVerbose);

        {$IFNDEF MSWINDOWS}
        if CheckoutOrUpdateReturnCode<>0 then
        begin
          // Patching can go wrong when line endings are not compatible
          // This happens e.g. with bgracontrols that have CRLF in the source files
          // Try to circumvent this problem by trick below (replacing line enddings)
          if Pos('different line endings',Output)>0 then
          begin
            CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('unix2dos '+DiffFile, SourceDirectory, FVerbose);
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
              //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed '+''''+'s/$'+''''+'"/`echo \\\r`/" '+DiffFile+' > '+DiffFile, SourceDirectory, FVerbose);
              //CheckoutOrUpdateReturnCode:=ExecuteCommandInDir('sed -i '+''''+'s/$/\r/'+''''+' '+DiffFile, SourceDirectory, FVerbose);
            end;
            if CheckoutOrUpdateReturnCode=0 then
            begin
              s:=ExtractFileNameSafe(FPatchCmd);
              if ((s='patch'+GetExeExt) OR (s='gpatch'+GetExeExt))
                 then LocalPatchCmd:=FPatchCmd + ' -t -p0 --binary -i '
                 else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
              CheckoutOrUpdateReturnCode:=ExecuteCommandInDir(LocalPatchCmd + DiffFile, SourceDirectory, Output, FVerbose);
            end;
          end;
        end;
        {$ENDIF}
        // Report error, but continue !
        if CheckoutOrUpdateReturnCode<>0 then
        begin
          WritelnLog(etError, localinfotext+'Patching with ' + DiffFile + ' failed.', true);
          WritelnLog(localinfotext+'Output: ' + Output, true);
          WritelnLog(localinfotext+'Verify the state of the source, correct and rebuild with make.', true);
        end;
      end;
    end;
  end;
end;

function TInstaller.DownloadFromURL(ModuleName: string): boolean;
var
  i:integer;
  FilesList:TStringList;
  FPCArchive,FPCArchiveDir,aName,aFile:string;
begin
  result:=false;

  localinfotext:=InitInfoText(' (DownloadFromURL: '+ModuleName+'): ');

  if (Length(FURL)=0) then exit;

  if (NOT DirectoryIsEmpty(SourceDirectory)) then
  begin
    Infoln(localinfotext+ModuleName+' sources are already there.',etWarning);
    Infoln(localinfotext+ModuleName+' sources will be replaced.',etWarning);
    //Infoln(localinfotext+ModuleName+' build-process will continue with existing sources.',etWarning);
    //exit(true);
  end;

  Infoln(localinfotext+'Getting '+ModuleName+' sources.',etInfo);

  if (FURL[Length(FURL)]='/') then SetLength(FURL,Pred(Length(FURL)));

  FPCArchive := GetTempFileNameExt('FPCUPTMP','zip');
  result:=GetFile(FURL,FPCArchive);
  if (result AND (NOT FileExists(FPCArchive))) then result:=false;

  FPCArchiveDir := GetTempDirName('FPCUPTMP');

  if result then
  begin
    //Delete existing files from source directory
    //DeleteDirectory(SourceDirectory,True);

    with TNormalUnzipper.Create do
    begin
      try
        result:=DoUnZip(FPCArchive,FPCArchiveDir,[]);
      finally
        Free;
      end;
    end;
  end;

  if result then
  begin
    aName:='';
    FilesList:=FindAllDirectories(FPCArchiveDir,False);
    if FilesList.Count=1 then aName:=FilesList[0];
    FreeAndNil(FilesList);
    if (
      (Pos(LowerCase(ModuleName),LowerCase(ExtractFileName(aName)))>0)
      OR
      ((ModuleName=_FPC) AND (Pos('source-release',LowerCase(ExtractFileName(aName)))>0))
      OR
      ((ModuleName=_LAZARUS) AND (Pos('lazarus-lazarus',LowerCase(ExtractFileName(aName)))>0))
      )
    then
    //if LowerCase(ExtractFileName(aName))=LowerCase(ModuleName) then
    begin
      Infoln(infotext+'Moving files due to extra path.',etInfo);
      //Infoln(infotext+'Also simultaneously correcting line-endings.',etInfo);
      Infoln(infotext+'This is time-consuming. Please wait.',etInfo);
      FilesList:=FindAllFiles(aName, '', True);
      for i:=0 to (FilesList.Count-1) do
      begin
        aFile:=FilesList[i];
        aFile:=StringReplace(aFile,aName,aName+DirectorySeparator+'..',[]);
        aFile:=SafeExpandFileName(aFile);
        ForceDirectoriesSafe(ExtractFileDir(aFile));
        SysUtils.RenameFile(FilesList[i],aFile);
        {$ifdef UNIX}
        //Correct line endings
        //Diabled for now: sed consumes enormous amount of time
        //ExecuteCommand('sed -i '+'''s/\r//'''+' '+aFile,False);
        {$endif}
      end;
      DeleteDirectory(aName,False);
      FreeAndNil(FilesList);
    end;

    // We now have all files.
    // Move-copy them to the source directory
    Infoln(infotext+'Moving files towards desired source directory.',etInfo);
    Infoln(infotext+'This is time-consuming. Please wait.',etInfo);
    FilesList:=FindAllFiles(FPCArchiveDir, '', True);
    for i:=0 to (FilesList.Count-1) do
    begin
      aFile:=FilesList[i];
      aName:=ConcatPaths([SourceDirectory,ExtractRelativePath(IncludeTrailingPathDelimiter(FPCArchiveDir),ExtractFilePath(aFile))]);
      ForceDirectoriesSafe(aName);
      MoveFile(aFile,IncludeTrailingPathDelimiter(aName)+ExtractFileName(aFile));
    end;
    FreeAndNil(FilesList);

  end;
  DeleteDirectory(FPCArchiveDir,False);
  SysUtils.Deletefile(FPCArchive); //Get rid of temp file.
end;

{$IFDEF MSWINDOWS}
procedure TInstaller.CreateBinutilsList(aVersion:string);
const
  SourceURL_gdb_default = LAZARUSBINARIES+'/i386-win32/gdb/bin/';
  SourceURL64_gdb_default = LAZARUSBINARIES+'/x86_64-win64/gdb/bin/';
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
begin
  SetLength(FUtilFiles,0); //clean out any cruft

  //aSourceURL:=FPCTRUNKBINARIES+'/install/binw32/';
  aSourceURL:=FPCGITLABBUILDBINARIES+'/-/raw/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw32/';
  {$ifdef win64}
  //aSourceURL64:=FPCBINARIES+'/install/binw64/';
  aSourceURL64:=FPCGITLABBUILDBINARIES+'/-/raw/release_'+StringReplace(DEFAULTFPCVERSION,'.','_',[rfReplaceAll])+'/install/binw64/';
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
  AddNewUtil('gdb' + GetExeExt,SourceURL_gdb_default,'',ucDebugger32);
  AddNewUtil('gdb' + GetExeExt,SourceURL64_gdb_default,'',ucDebugger64);
  //AddNewUtil('libiconv-2'+GetLibExt,SourceURL64_gdb_default,'',ucDebugger64);

  {$ifdef win32}
  AddNewUtil('ar' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('as' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('cmp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('cp' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('diff' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('gdate' + GetExeExt,aSourceURL,'',ucBinutil);
  // just add default 32 bit debugger for all usercases as a binutil !
  AddNewUtil('gdb' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('libexpat-1'+GetLibExt,aSourceURL,'',ucBinutil);
  AddNewUtil('gecho' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt + '.manifest',aSourceURL,'',ucBinutil);
  AddNewUtil('gmkdir' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('ld' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('make' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('mv' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('objdump' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('pwd' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('rm' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('strip' + GetExeExt,aSourceURL,'',ucBinutil);

  AddNewUtil('Qt4Pas5'+GetLibExt,SourceURL_QT,'',ucQtFile);
  AddNewUtil('Qt5Pas1'+GetLibExt,SourceURL_QT5,'',ucQtFile);

  // Add special versions for crosss-compiling towards win64

  aSourceURL:=FPCGITLABBINARIES+'/-/raw/main/i386-win32/';
  AddNewUtil('x86_64-win64-ar' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('x86_64-win64-as' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('x86_64-win64-ld' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('x86_64-win64-nm' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('x86_64-win64-objcopy' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('x86_64-win64-objdump' + GetExeExt,aSourceURL,'',ucBinutil);
  AddNewUtil('x86_64-win64-strip' + GetExeExt,aSourceURL,'',ucBinutil);
  {$endif win32}

  {$ifdef win64}
  AddNewUtil('ar' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('as' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('cmp' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('cp' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('diff' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('gdate' + GetExeExt,aSourceURL64,'',ucBinutil);
  // just add default 64 bit debugger for all usercases as a binutil !
  AddNewUtil('gdb' + GetExeExt,SourceURL64_gdb_default,'',ucBinutil);
  //AddNewUtil('libiconv-2'+GetLibExt,SourceURL64_gdb_default,'',ucBinutil);
  AddNewUtil('gecho' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt,aSourceURL64,'',ucBinutil);
  AddNewUtil('ginstall' + GetExeExt + '.manifest',aSourceURL64,'',ucBinutil);
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
  //AddNewUtil('gdb-6.4-win32-arm-wince.zip',FPCFTPURL+'contrib/cross/','',ucDebuggerWince);
  aSourceURL:=FPCGITLABBUILDBINARIES+'/-/raw/main/install/crossbinwce/';
  AddNewUtil('arm-wince-gdb.exe',aSourceURL,'',ucDebuggerWince);
end;

function TInstaller.DownloadBinUtils: boolean;
var
  Counter: integer;
  Errors: integer = 0;
  DownloadSuccess:boolean;
  InstallPath:string;
begin
  localinfotext:=InitInfoText(' (DownloadBinUtils): ');
  //Parent directory of files. Needs trailing backslash.
  ForceDirectoriesSafe(FMakeDir);
  Result := true;

  for Counter := low(FUtilFiles) to high(FUtilFiles) do
  begin
    if (FUtilFiles[Counter].Category in [ucBinutil,ucDebugger32,ucDebugger64]) then
    begin
      InstallPath:=MakePath;
      if (FUtilFiles[Counter].Category in [ucDebugger32,ucDebugger64]) then
      begin
        if (FUtilFiles[Counter].Category=ucDebugger32) then InstallPath:=InstallPath+'gdb\i386-win32\';
        if (FUtilFiles[Counter].Category=ucDebugger64) then InstallPath:=InstallPath+'gdb\x86_64-win64\';
        ForceDirectoriesSafe(InstallPath);
      end;

      if (FileExists(InstallPath+FUtilFiles[Counter].FileName)) then continue;

      //if (FUtilFiles[Counter].FileName='libiconv-2'+GetLibExt) then continue;

      DownloadSuccess:=GetFile(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,InstallPath+FUtilFiles[Counter].FileName);

      if (NOT DownloadSuccess) then
      begin
        Infoln(localinfotext+'Error downloading binutil: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + '.',etError);
        Inc(Errors);
      end
      else
      begin
        Infoln(localinfotext+'Downloading: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + ' success.',etDebug);

        if ExtractFileExt(FUtilFiles[Counter].FileName)='.zip' then
        begin
          with TNormalUnzipper.Create do
          begin
            try
              if DoUnZip(InstallPath+FUtilFiles[Counter].FileName,InstallPath,[]) then
                Infoln(localinfotext+'Unpacking: ' + FUtilFiles[Counter].FileName + ' into ' + ExtractFileDir(InstallPath) + ' success.',etInfo);
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
  localinfotext:=InitInfoText(' (DownloadBinUtils): ';
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
      WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading Win64 binutils from ' + SourceURL, true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    with TNormalUnzipper.Create do
    begin
      try
        OperationSucceeded:=DoUnZip(BinsZip,MakePath,[]);
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
  SVNZip,SVNDir,aSourceURL: string;
  i:integer;
begin
  localinfotext:=InitInfoText(' (DownloadSVN): ');

  OperationSucceeded := false;

  SVNZip := GetTempFileNameExt('FPCUPTMP','zip');

  SVNDir := MakePath+'svn';

  if ForceDirectoriesSafe(SVNDir) then
  begin
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
        WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading SVN client', true);
      end;
    end;

    if OperationSucceeded then
    begin
      // Extract, overwrite
      with TNormalUnzipper.Create do
      begin
        try
          DeleteDirectoryEx(SVNDir);
          OperationSucceeded:=DoUnZip(SVNZip,SVNDir,[]);
        finally
          Free;
        end;
      end;
    end;
  end;

  if OperationSucceeded then
  begin
    WritelnLog(etDebug,localinfotext + 'SVN download and unpacking ok', true);
    OperationSucceeded := FindSVNSubDirs;
    if OperationSucceeded then
      SysUtils.Deletefile(SVNZip); //Get rid of temp zip if success.
  end;
  Result := OperationSucceeded;
end;

{$ifndef USEONLYCURL}
function TInstaller.DownloadOpenSSL: boolean;
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  OpenSSLFileName,aSourceURL: string;
  i:integer;
begin
  result:=false;

  OperationSucceeded := false;

  localinfotext:=InitInfoText(' (DownloadOpenSSL): ');

  Infoln(localinfotext+'No OpenSSL library files available for SSL. Going to download them. Might take some time.',etWarning);

  // Direct download OpenSSL from from Lazarus binaries
  if (NOT OperationSucceeded) then
  begin
    i:=Low(SSL_DLL_Names);
    while ( (not OperationSucceeded) AND (i<=High(SSL_DLL_Names)) ) do
    begin
      OpenSSLFileName:=Crypto_DLL_Names[i]+GetLibExt;
      OperationSucceeded:=GetFile(OPENSSL_URL_LATEST+'/'+OpenSSLFileName,SafeGetApplicationPath+OpenSSLFileName,true,true);
      if OperationSucceeded then
      begin
        OpenSSLFileName:=SSL_DLL_Names[i]+GetLibExt;
        OperationSucceeded:=GetFile(OPENSSL_URL_LATEST+'/'+OpenSSLFileName,SafeGetApplicationPath+OpenSSLFileName,true,true);
      end;
      Inc(i);
    end;
  end;

  // Direct download OpenSSL from public sources
  if (NOT OperationSucceeded) then
  begin
    localinfotext:=InitInfoText(' (DownloadOpenSSL): ');

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
        WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading OpenSSL library', true);
      end;
    end;

    if OperationSucceeded then
    begin
      // Extract
      with TNormalUnzipper.Create do
      begin
        try
          resultcode:=2;
          //DLLExt;
          GetExeExt;
          SysUtils.Deletefile(SafeGetApplicationPath+Crypto_DLL_Name+GetLibExt);
          if GetLastOSError<>5 then // no access denied
          begin
            SysUtils.Deletefile(SafeGetApplicationPath+SSL_DLL_Name+GetLibExt);
            if GetLastOSError<>5 then // no access denied
            begin
              resultcode:=1;
              if DoUnZip(OpenSSLFileName,SafeGetApplicationPath,[Crypto_DLL_Name+GetLibExt,SSL_DLL_Name+GetLibExt]) then resultcode:=0;
            end;
          end;
        finally
          Free;
        end;
      end;

      if resultcode <> 0 then
      begin
        OperationSucceeded := false;
        if resultcode=2 then WritelnLog(etWarning, localinfotext + 'Download OpenSSL error: could not delete/overwrite existing files.');
        if resultcode=1 then WritelnLog(etError, localinfotext + 'Download OpenSSL error: could not unzip files.');
      end;
    end;

    if OperationSucceeded
       then Infoln(localinfotext+'OpenSSL library files download and unpacking from '+aSourceURL+' ok.',etWarning)
       else Infoln(localinfotext+'Could not download/install openssl library archive.', etError);

    SysUtils.Deletefile(OpenSSLFileName); //Get rid of temp zip if success.
  end;

  result := OperationSucceeded;
end;
{$endif}

function TInstaller.DownloadWget: boolean;
const
  {$ifdef win64}
  NewSourceURL : array [0..2] of string = (
    FPCUPGITREPO+'/releases/download/windowsx64bins_v1.0/wget-openssl-x64.exe',
    // From https://github.com/webfolderio/wget-windows/releases/download/v1.21.3.june.19.2022/wget-openssl-x64.exe
    FPCUPGITREPO+'/releases/download/windowsx64bins_v1.0/wget.exe',
    //'https://eternallybored.org/misc/wget/1.19.4/64/wget.exe'
    //'https://eternallybored.org/misc/wget/1.20/64/wget.exe'
    'https://eternallybored.org/misc/wget/1.20.3/64/wget.exe'
    );
  {$endif}
  {$ifdef win32}
  NewSourceURL : array [0..2] of string = (
    FPCUPGITREPO+'/releases/download/windowsi386bins_v1.0/wget-openssl-x86.exe',
    // From https://github.com/webfolderio/wget-windows/releases/download/v1.21.3.june.19.2022/wget-openssl-x86.exe
    FPCUPGITREPO+'/releases/download/windowsi386bins_v1.0/wget.exe',
    //'https://eternallybored.org/misc/wget/1.19.4/32/wget.exe'
    //'https://eternallybored.org/misc/wget/1.20/32/wget.exe'
    'https://eternallybored.org/misc/wget/1.20.3/32/wget.exe'
    );
  {$endif}
var
  OperationSucceeded: boolean;
  WgetDir,WgetExe: string;
  //WgetFile,WgetZip: string;
  i:integer;
begin
  OperationSucceeded := false;
  Infoln(localinfotext+'No Wget found. Going to download it.',etInfo);

  WgetDir:=ConcatPaths([FMakeDir,'wget']);
  WgetExe:=WgetDir+DirectorySeparator+'wget'+GetExeExt;

  if ForceDirectoriesSafe(WgetDir) then
  begin
    for i:=0 to (Length(NewSourceURL)-1) do
    try
      WgetExe:=WgetDir+DirectorySeparator+FileNameFromURL(NewSourceURL[i]);

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
        WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading Wget', true);
      end;
    end;

  end;
  if OperationSucceeded then FWget:=WgetExe;
  Result := OperationSucceeded;
end;

function TInstaller.DownloadFreetype: boolean;
const
  {$ifdef win64}
  NewSourceURL : array [0..1] of string = (
      'https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/windowsx64bins_v1.0/freetypewin64.zip',
      'https://sourceforge.net/projects/gnuwin32/files/freetype/2.3.5-1/freetype-2.3.5-1-bin.zip/download'
    );
  {$endif}
  {$ifdef win32}
  NewSourceURL : array [0..1] of string = (
      'https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/windowsi386bins_v1.0/freetypewin32.zip',
      'https://sourceforge.net/projects/gnuwin32/files/freetype/2.3.5-1/freetype-2.3.5-1-bin.zip/download'
    );
  {$endif}
var
  OperationSucceeded: boolean;
  FreetypeDir,FreetypeBin,FreetypZip,FreetypZipDir: string;
  i:integer;
begin
  localinfotext:=InitInfoText(' (DownloadFreetype): ');

  Infoln(localinfotext+'No Freetype found. Going to download it.',etInfo);

  OperationSucceeded := false;

  FreetypeDir:=IncludeTrailingPathDelimiter(FInstallDirectory);

  FreetypeBin:='freetype-6'+GetLibExt;
  if NOT FileExists(FreetypeDir+FreetypeBin) then
    FreetypeBin:='freetype'+GetLibExt;

  if NOT FileExists(FreetypeDir+FreetypeBin) then
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
        WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading Freetype', true);
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
    FreetypeBin:=FindFileInDir(FreetypeBin,ExcludeTrailingPathDelimiter(FreetypZipDir));
    OperationSucceeded := MoveFile(FreetypeBin,FreetypeDir+ExtractFileName(FreetypeBin));
    if NOT OperationSucceeded then
    begin
      WritelnLog(etError, localinfotext + 'Could not move '+FreetypeBin+' into '+FreetypeDir+ExtractFileName(FreetypeBin));
    end
    else OperationSucceeded := FileExists(FreetypeDir+ExtractFileName(FreetypeBin));
  end;

  SysUtils.Deletefile(FreetypZip);
  DeleteDirectoryEx(FreetypZipDir+DirectorySeparator);
  Result:=true; //never fail
end;

function TInstaller.DownloadZlib: boolean;
const
  TARGETNAME='zlib1'+GetLibExt;
  SOURCEURL : array [0..1] of string = (
    FPCUPGITREPO+'/releases/download/windowsi386bins_v1.0/zlib-1.2.3-bin.zip',
    'https://sourceforge.net/projects/gnuwin32/files/zlib/1.2.3/zlib-1.2.3-bin.zip/download'
    );
var
  OperationSucceeded: boolean;
  TargetDir,TargetBin,SourceBin,SourceZip,ZipDir: string;
  i:integer;
begin
  localinfotext:=InitInfoText(' (Download '+TARGETNAME+'): ');

  Infoln(localinfotext+'No '+TARGETNAME+' found. Going to download it.');

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
        WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading ' + TARGETNAME, true);
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
        WritelnLog(etError, localinfotext + 'Could not move ' + SourceBin + ' towards '+TargetBin);
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
  SOURCEURL : array [0..4] of string = (
    'ftp://downloads.freepascal.org/pub/fpc/contrib/jvm/fpcjvmutilities.zip',
    'https://sourceforge.net/projects/jasmin/files/jasmin/jasmin-'+JASMINVERSION+'/jasmin-'+JASMINVERSION+'.zip/download',
    'https://github.com/davidar/jasmin/archive/'+JASMINVERSION+'.zip',
    'https://www.java2s.com/Code/JarDownload/jasmin/jasmin.jar.zip',
    'https://www.java2s.com/Code/JarDownload/jasmin/jasmin-3.0.3.jar.zip'
    );
var
  OperationSucceeded: boolean;
  TargetBin,SourceBin,SourceZip,ZipDir: string;
  i:integer;
begin
  localinfotext:=InitInfoText(' (Download '+TARGETNAME+'): ');

  OperationSucceeded := false;

  // for now, just put jasmin.jar in FPC bin-directory ... easy and simple and working

  TargetBin:=FPCBinDir+DirectorySeparator+TARGETNAME;

  if (NOT FileExists(TargetBin)) then
  begin
    Infoln(localinfotext+'No '+TARGETNAME+' found. Going to download it.');

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
        WritelnLog(etError, localinfotext + 'Exception ' + E.ClassName + '/' + E.Message + ' downloading ' + TARGETNAME, true);
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
        //OperationSucceeded:=DoUnZip(SourceZip,ZipDir,[]);
        OperationSucceeded:=DoUnZip(SourceZip,ZipDir,[TARGETNAME]);
      finally
        Free;
      end;
    end;

    if OperationSucceeded then
    begin
      //MoveFile
      SourceBin:=ZipDir{+DirectorySeparator+'jasmin-' + JASMINVERSION} + DirectorySeparator+TARGETNAME;
      OperationSucceeded := MoveFile(SourceBin,TargetBin);
      if (NOT OperationSucceeded) then
      begin
        WritelnLog(etError, localinfotext + 'Could not move ' + SourceBin + ' towards '+TargetBin);
      end
      else OperationSucceeded := FileExists(TargetBin);
    end;

    SysUtils.Deletefile(SourceZip);
    DeleteDirectoryEx(ZipDir+DirectorySeparator);

  end;

  Result:=true; //never fail
end;

{$IFDEF MSWINDOWS}
function TInstaller.FindSVNSubDirs: boolean;
var
  SVNDir:string;
  SVNFiles: TStringList;
  OperationSucceeded: boolean;
begin
  SVNDir := MakePath+'svn';
  SVNFiles := FindAllFiles(SVNDir, SVNClient.RepoExecutableName + GetExeExt, true);
  try
    if SVNFiles.Count > 0 then
    begin
      // Just get first result.
      SVNClient.RepoExecutable := SVNFiles.Strings[0];
      OperationSucceeded := true;
    end
    else
    begin
      Infoln(localinfotext+'Could not find svn executable in or under ' + SVNDir,etInfo);
      OperationSucceeded := false;
    end;
  finally
    SVNFiles.Free;
  end;
  Result := OperationSucceeded;
end;
{$ENDIF}

function TInstaller.GetFPCTarget(Native: boolean): string;
begin
  if Native then
    result:=GetSourceCPU+'-'+GetSourceOS
  else
    result:=GetCPU(CrossCPU_Target)+'-'+GetOS(CrossOS_Target);
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
    WritelnLog(InitInfoText(' (SetPath): External program path:  ' + ResultingPath), false);
  end;
  if FVerbose then
    Infoln(InitInfoText(' (SetPath): Set path to: ' + ResultingPath),etDebug);
end;

procedure TInstaller.WritelnLog(msg: TStrings; ToConsole: boolean = true);
var
  idx:integer;
begin
  if (msg.Count>0) then
  begin
    for idx:=0 to Pred(msg.Count) do WritelnLog(msg.Strings[idx],ToConsole);
  end;
end;

procedure TInstaller.WritelnLog(msg: string; ToConsole: boolean);
begin
  if Assigned(FLog) then
  begin
    FLog.WriteLog(msg);
  end;
  if ToConsole then Infoln(msg);
end;

procedure TInstaller.WritelnLog(EventType: TEventType; msg: string; ToConsole: boolean);
begin
  if Assigned(FLog) then
  begin
    FLog.WriteLog(EventType,msg);
  end;
  if ToConsole then Infoln(msg,EventType);
end;

function TInstaller.GetFPCInBinDir: string;
var
  proxy:string;
begin
  result := FPCBinDir+DirectorySeparator+'fpc'+GetExeExt;

  {$IFDEF UNIX}
  if LinuxLegacy then
    proxy:=result+'compat.sh'
  else
    proxy:=result+'.sh';

  if FileExists(proxy) then
  begin
    //Use our proxy if it is installed
    result := proxy;
  end;
  {$ENDIF UNIX}

  if (NOT FileExists(result)) then
  begin
    if ( (NOT IsFPCInstaller) OR (IsFPCInstaller AND Assigned(CrossInstaller)) ) then
    begin
      Infoln('FPC compiler '+result+' not found. Fatal.',etError);
      raise Exception.CreateFmt('FPC compiler "%s" not found.', [result]);
    end;
    result:='';
  end;
end;

procedure TInstaller.SetTarget(aCPU:TCPU;aOS:TOS;aSubArch:TSUBARCH);
begin
  FCrossCPU_Target:=aCPU;
  FCrossOS_Target:=aOS;
  if ((CrossOS_Target in SUBARCH_OS) AND (CrossCPU_Target in SUBARCH_CPU)) then
    FCrossOS_SubArch:=aSubArch
  else
    FCrossOS_SubArch:=TSUBARCH.saNone;
end;

procedure TInstaller.SetABI(aABI:TABI);
begin
  //
  if (CrossCPU_Target=TCPU.arm) then
    FCrossOS_ABI:=aABI
  else
  begin
    if (CrossOS_Target=TOS.ios) then
    begin
      FCrossOS_ABI:=aABI;
      if (aABI<>TABI.default) AND (aABI<>TABI.aarch64ios) then
        raise Exception.CreateFmt('Invalid ARM ABI "%s" for SetABI for iOS.', [aABI]);
    end
    else
    begin
      FCrossOS_ABI:=TABI.default;
      if (aABI<>TABI.default) then
        raise Exception.CreateFmt('Invalid ARM ABI "%s" for SetABI.', [aABI]);
    end;
  end;
end;

function TInstaller.GetSuitableRepoClient:TRepoClient;
begin
  result:=nil;

  // Do we need a GIT client or nothing when we need a zip archive from a GIT repo
  if result=nil then
  begin
    if ( AnsiContainsText(FURL,'github.com') OR AnsiContainsText(FURL,'gitlab.com') ) then
    begin
      if AnsiContainsText(FURL,'/archive/') then
        exit //// We do NOT need any repo client !!
      else
        result:=GitClient; //We need GIT
    end;
  end;

  // Do we need GIT more
  if result=nil then if DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory)+'.git') then result:=GitClient;
  if result=nil then if ( (AnsiEndsText('.git',FURL)) OR (AnsiEndsText('.git/',FURL)) ) then result:=GitClient;

  // Do we need SVN
  if result=nil then if DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory)+'.svn') then result:=SVNClient;
  if result=nil then if (Pos('http://svn.',LowerCase(FURL))=1) then result:=SVNClient;

  // Do we need HG
  if result=nil then if ( (Pos('hg.code.sf.net',LowerCase(FURL))>0) ) then result:=HGClient;
  if result=nil then if ( (Pos('bitbucket.org',LowerCase(FURL))>0) ) then result:=HGClient;

  // Do we need FTP
  //if result=nil then if (Pos(FTPBASEFTP,LowerCase(FURL))>0) then result:=FTPClient;
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
  infotext:=InitInfoText(' (BuildModule: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);
end;

function TInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=false;
  FCleanModuleSuccess:=false;
  infotext:=InitInfoText(' (CleanModule: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);

  if (not DirectoryExists(SourceDirectory)) then
  begin
    Infoln(infotext+'No '+ModuleName+' source directory ('+SourceDirectory+') found [yet] ... nothing to be done',etDebug);
    exit(true);
  end;
  if DirectoryIsEmpty(SourceDirectory) then
  begin
    Infoln(infotext+'No '+ModuleName+' files found in source directory ('+SourceDirectory+') ... nothing to be done',etDebug);
    exit(true);
  end;
end;

function TInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=InitInfoText(' (ConfigModule: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);
end;

function TInstaller.GetModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=InitInfoText(' (GetModule: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);

  ForceDirectoriesSafe(SourceDirectory);
end;

function TInstaller.CheckModule(ModuleName: string): boolean;
var
  aRepoClient:TRepoClient;
begin
  result:=true;

  infotext:=InitInfoText(' (CheckModule: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);

  if NOT DirectoryExists(SourceDirectory) then exit;

  // Do not check the sources when crsoss-compiling
  // They must be ok !!
  if IsCross then exit;

  aRepoClient:=GetSuitableRepoClient;

  // No repo client ...
  if aRepoClient=nil then
  begin
    // We might have zip-sources or custom sources.
    // Just exit silently ... no extra checking needed.
    if (IsFPCInstaller OR IsLazarusInstaller) then
    begin
      exit;
    end
    else
    begin
      // Make a best quess
      Infoln(infotext+'Using SVNClient for ' + ModuleName + ' sources !',etWarning);
      aRepoClient:=SVNClient;
    end;
  end;

  Infoln(infotext+'Checking ' + ModuleName + ' sources with '+TrimLeft(Copy(UnCamel(aRepoClient.ClassName),2,MaxInt)),etInfo);

  aRepoClient.Verbose          := FVerbose;
  aRepoClient.ModuleName       := ModuleName;
  aRepoClient.LocalRepository  := SourceDirectory;
  aRepoClient.Repository       := FURL;
  aRepoClient.DesiredTag       := FTAG;
  aRepoClient.DesiredBranch    := FBranch;
  aRepoClient.DesiredRevision  := FDesiredRevision;

  // Setting a DesiredTag also set ExportOnly when using GIT.
  // Override this setting if false with global setting
  if (NOT aRepoClient.ExportOnly) then aRepoClient.ExportOnly := FExportOnly;

  if (NOT DirectoryExists(aRepoClient.LocalRepository)) OR (DirectoryIsEmpty(aRepoClient.LocalRepository)) then
  begin
    result:=true;
    exit;
  end;

  result:=aRepoClient.LocalRepositoryExists;

  if (NOT result) then
  begin

    if (aRepoClient is TGitClient) then
    begin
      if (aRepoClient.ReturnCode=FRET_LOCAL_REMOTE_TAG_NOMATCH) then
      begin
        Infoln(infotext+'Repo tag and desired tag do not match'+'.',etError);
        Infoln(infotext+'The desired '+aRepoClient.DesiredTag+' is different from local repository-tag.',etError);
      end;
    end;

    if (aRepoClient.ReturnCode=FRET_LOCAL_REMOTE_URL_NOMATCH) then
    begin
      Infoln(infotext+URL_ERROR+'.',etError);
      Infoln(infotext+'Desired URL='+FURL,etError);
      Infoln(infotext+'Source URL='+aRepoClient.Repository,etError);
    end;
  end;

  if result then Infoln(infotext+'Sources in repo ok.',etDebug)
end;

function TInstaller.PatchModule(ModuleName: string): boolean;
const
  STRIPMAGIC='fpcupstrip';
  {$ifndef FPCONLY}
  DARWINCHECKMAGIC='useride: ';
  DARWINHACKMAGIC='./lazbuild$(SRCEXEEXT) --lazarusdir=. --build-ide= --ws=$(LCL_PLATFORM)';
  {$ifdef Haiku}
  HAIKUHACKMAGIC='$(DIFF) $(TEMPNAME3) $(EXENAME)';
  {$endif}
  {$endif}
var
  PatchList:TStringList;
  PatchFilePath,PatchFileCorrectedPath,PatchDirectory:string;
  s: string = '';
  Output: string = '';
  ReturnCode,i,j: integer;
  LocalSourcePatches:string;
  StripLevel:integer;
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

  TrunkVersion:=0;
  if PatchFPC then TrunkVersion:=CalculateNumericalVersion(FPCTRUNKVERSION);
  {$ifndef FPCONLY}
  if PatchLaz then TrunkVersion:=CalculateNumericalVersion(LAZARUSTRUNKVERSION);
  {$endif}

  if PatchFPC then PatchDirectory:=ConcatPaths([BaseDirectory,'patches','patchfpc']);
  {$ifndef FPCONLY}
  if PatchLaz then PatchDirectory:=ConcatPaths([BaseDirectory,'patches','patchlazarus']);
  {$endif}
  if PatchUniversal then PatchDirectory:=ConcatPaths([BaseDirectory,'patches','patchuniversal']);

  LocalSourcePatches:=FSourcePatches;

  {$ifdef Haiku}
  // we will hack into FPC itself to prevent FPU crash on Haiku
  //if FOnlinePatching then
  begin
    if PatchFPC then
    begin
      PatchList:=TStringList.Create;
      try
        PatchList.Clear;
        PatchFilePath:=ConcatPaths([SourceDirectory,'rtl','inc'])+DirectorySeparator+'mathh.inc';
        PatchList.LoadFromFile(PatchFilePath);
        for i:=0 to (PatchList.Count-1) do
        begin
          s:=PatchList.Strings[i];
          if (Pos('fpcupdeluxe',s)>0) then break; // we were here already ... ;-)
          if ((Pos('Default8087CW',s)>0) AND (Pos('$1332;',s)>0)) then
          begin
            //PatchList.Strings[i]:=StringReplace(s,'$1332;','$1333; // Patched by fpcupdeluxe to prevent FPU crash',[]);
            PatchList.Strings[i]:=StringReplace(s,'$1332;','$137F; // Patched by fpcupdeluxe to prevent FPU crash',[]);
            PatchList.SaveToFile(PatchFilePath);
            break;
          end;
        end;
      finally
        PatchList.Free;
      end;
    end;

    PatchList:=TStringList.Create;
    try
      PatchList.Clear;
      PatchFilePath:=ConcatPaths([SourceDirectory,'compiler'])+DirectorySeparator+MAKEFILENAME;

      if (FileExists(PatchFilePath)) then
      begin
        PatchList.LoadFromFile(PatchFilePath);
        j:=-1;
        for i:=0 to (PatchList.Count-1) do
        begin
          s:=PatchList.Strings[i];
          if (Pos(HAIKUHACKMAGIC,s)>0) then
          begin
            // check if we were already there
            if (Pos('haiku',PatchList.Strings[i-1]) = 0) then
            begin
              j:=i;
            end;
            break;
          end;
        end;
        if (j<>-1) then
        begin
          Inc(j);
          PatchList.Insert(j,'endif');
          PatchList.Insert(j-1,'ifneq ($(OS_TARGET), haiku)');
          PatchList.SaveToFile(PatchFilePath);
        end;
      end;

    finally
      PatchList.Free;
    end;
  end;
  {$endif}

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

        PatchFilePath:=IncludeTrailingPathDelimiter(SourceDirectory)+MAKEFILENAME;

        if (FileExists(PatchFilePath)) then
        begin
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
        for i:=Pred(PatchList.Count) downto 0 do
        begin
          PatchAccepted:=False;
          PatchFilePath:=SafeExpandFileName(PatchList[i]);

          if PatchUniversal then
          begin
            // patches must have the same name as the module to be installed
            s:=FileNameWithoutExt(PatchFilePath);
            PatchAccepted:=(LowerCase(ModuleName)=LowerCase(s));
          end;

          if (NOT PatchUniversal) then
          begin
            s:=FileNameFromURL(PatchFilePath);
            s:=FileNameWithoutExt(s);
            s:=VersionFromUrl(s);
            PatchVersion:=CalculateNumericalVersion(s);

            if (s='trunk') or (PatchVersion=0) then
            begin
              //only patch trunk in case no version is given
              PatchVersion:=TrunkVersion;
            end;

            s:=localinfotext+'Found patch: '+ExtractFileName(PatchFilePath)+' with version '+InttoStr(PatchVersion);
            if FVerbose then Infoln(s,etInfo) else Infoln(s,etDebug);

            if (SourceVersionNum=PatchVersion) then PatchAccepted:=True;
            if (PatchVersion=TrunkVersion) then
            begin
              // only patch trunk when HEAD is requested
              if (FDesiredRevision<>'') AND (Uppercase(trim(FDesiredRevision))<>'HEAD') then
              begin
                PatchAccepted:=False;
              end;
            end;
          end;

          if (NOT PatchAccepted) then PatchList.Delete(i);
        end;
      end;

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
    Infoln(infotext+'Going to patch ' + ModuleName + ' sources !!',etWarning);
    PatchList:=TStringList.Create;
    try
      PatchList.CommaText := LocalSourcePatches;
      for i:=0 to (PatchList.Count-1) do
      begin
        Infoln(infotext+'Trying to patch ' + ModuleName + ' with '+PatchList[i],etInfo);
        PatchFilePath:=SafeExpandFileName(PatchList[i]);
        if (NOT FilenameIsAbsolute(PatchFilePath)) then
        begin
          if (NOT FileExists(PatchFilePath)) then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+PatchList[i]);
          if NOT FileExists(PatchFilePath) then
          begin
            if PatchFPC then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchfpc'+DirectorySeparator+PatchList[i])
               {$ifndef FPCONLY}
               else if PatchLaz then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchlazarus'+DirectorySeparator+PatchList[i])
               {$endif}
                  else PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchuniversal'+DirectorySeparator+PatchList[i]);
          end;
        end;

        if FileExists(PatchFilePath) then
        begin
          StripLevel:=1;
          j:=Pos(STRIPMAGIC,PatchFilePath);
          if (j>0) then StripLevel:=StrToIntDef(PatchFilePath[j+Length(STRIPMAGIC)],StripLevel);

          Processor.Executable := FPatchCmd;
          Processor.Process.Parameters.Clear;
          Processor.Process.CurrentDirectory := SourceDirectory;

          // check for default values
          s:=ExtractFileNameSafe(FPatchCmd);
          if ((s='patch'+GetExeExt) OR (s='gpatch'+GetExeExt)) then
          begin
            Processor.SetParamData('-t');
            Processor.SetParamData('-p'+InttoStr(StripLevel));
            Processor.SetParamData('-N');
            {$IF not defined(BSD) or defined(DARWIN)}
            Processor.SetParamData('--no-backup-if-mismatch');
            {$endif}
            Processor.SetParamData('-i');
          end;

          // always correct for line-endings while patch is very sensitive for that
          PatchFileCorrectedPath:=IncludeTrailingPathDelimiter(GetTempDirName)+ExtractFileName(PatchFilePath);
          if FileCorrectLineEndings(PatchFilePath,PatchFileCorrectedPath) then
          begin
            // revert to original file in case of file not found
            if (NOT FileExists(PatchFileCorrectedPath)) then PatchFileCorrectedPath:=PatchFilePath;
            Processor.SetParamData(PatchFileCorrectedPath);

            //Execute patch command

            // we could use [patch --dry-run] first to test for -p0 and -p1 and -p2

            ReturnCode:=Processor.ExecuteAndWait;

            if ( (ReturnCode<>0) AND (StripLevel=1) ) then
            begin
              //try again with different [0] strip option
              j:=Processor.Process.Parameters.IndexOf('-p1');
              if (j<>-1) then
              begin
                Processor.Process.Parameters.Strings[j]:='-p0';
                ReturnCode:=Processor.ExecuteAndWait;
              end;
            end;

            // remove the temporary file
            if (PatchFileCorrectedPath<>PatchFilePath) then DeleteFile(PatchFileCorrectedPath);

            if (ReturnCode=0)  then
            begin
              result:=true;
              WritelnLog(etInfo, infotext+ModuleName+ ' has been patched successfully with '+PatchList[i] + '.', true);
            end
            else
            begin
              WritelnLog(etError, infotext+ModuleName+' patching with ' + PatchList[i] + ' failed.', true);
              WritelnLog(etError, infotext+ModuleName+' patch output: ' + Output, true);
            end;
          end;
          DeleteDirectoryEx(ExtractFileDir(PatchFileCorrectedPath));
        end
        else
        begin
          result:=true;
          WritelnLog(etWarning, infotext+ModuleName+ ' patching with ' + PatchList[i] + ' failed due to missing patch file ('+PatchFilePath+').', true);
        end;
      end;
    finally
      PatchList.Free;
    end;
  end
  else
  begin
    result:=true;
    Infoln(infotext+'No ' + ModuleName + ' patches defined.',etInfo);
  end;
end;

function TInstaller.CreateRevision(ModuleName,aRevision:string): boolean;
const
  // needs to be exactly the same as used by Lazarus !!!
  //RevisionIncComment = '// Created by FPCLAZUP';
  RevisionIncComment = '// Created by Svn2RevisionInc';
  ConstName = 'RevisionStr';
var
  //RevisionIncText: Text;
  RevFilePath,ConstStart: string;
  RevisionStringList:TStringList;
  //NumRevision:Longint;
begin
  result:=false;

  if (Pos(' ',aRevision)>0) then exit;

  // Only handle Lazarus !
  // if (ModuleName<>_LAZARUS) then exit;

  //if TryStrToInt(aRevision,NumRevision) then
  begin
    RevFilePath:='';

    if (ModuleName=_LAZARUS) then RevFilePath:=IncludeTrailingPathDelimiter(SourceDirectory)+'ide';
    if (ModuleName=_FPC) then RevFilePath:=IncludeTrailingPathDelimiter(SourceDirectory)+'compiler';

    if (Length(RevFilePath)>0) then
    begin
      if (NOT DirectoryExists(RevFilePath)) then exit;
      RevFilePath:=RevFilePath+PathDelim+REVINCFILENAME;
      DeleteFile(RevFilePath);
      if (Length(aRevision)>0) then
      begin
        RevisionStringList:=TStringList.Create;
        try
          if (ModuleName=_LAZARUS) then
          begin
            RevisionStringList.Add(RevisionIncComment);
            ConstStart := Format('const %s = ''', [ConstName]);
            //RevisionStringList.Add(ConstStart+InttoStr(NumRevision)+''';');
            RevisionStringList.Add(ConstStart+aRevision+''';');
          end;
          if (ModuleName=_FPC) then
          begin
            //RevisionStringList.Add(''''+InttoStr(NumRevision)+'''');
            RevisionStringList.Add(''''+aRevision+'''');
          end;
          RevisionStringList.SaveToFile(RevFilePath);
          result:=true;
        finally
          RevisionStringList.Free;
        end;
      end;
    end;
  end;
end;

function TInstaller.GetRevision(ModuleName:string): string;
var
  RevFileName,RevString: string;
  RevisionStringList:TStringList;
  idx:integer;
begin
  result:='';

  RevString:='';
  RevFileName:='';

  // First, use try to get revision from compiler.
  if ModuleName=_FPC then RevString:=CompilerRevision(GetFPCInBinDir);

  // Second, try to get revision from rev file.
  if (Length(RevString)=0) then
  begin
    if (ModuleName=_LAZARUS) OR (ModuleName=_LAZBUILD) then RevFileName:=ConcatPaths([SourceDirectory,'ide',REVINCFILENAME]);
    if ModuleName=_FPC then RevFileName:=ConcatPaths([SourceDirectory,'compiler',REVINCFILENAME]);
    //if ModuleName=_LAZARUS then RevFileName:=IncludeTrailingPathDelimiter(SourceDirectory)+'ide'+PathDelim+REVINCFILENAME;
    //if ModuleName=_FPC then RevFileName:=IncludeTrailingPathDelimiter(SourceDirectory)+'compiler'+PathDelim+REVINCFILENAME;

    if FileExists(RevFileName) then
    begin
      RevisionStringList:=TStringList.Create;
      try
        RevisionStringList.LoadFromFile(RevFileName);
        if (RevisionStringList.Count>0) then
        begin

          if ModuleName=_FPC then
          begin
            RevString:=Trim(RevisionStringList.Strings[0]);
          end;

          if (ModuleName=_LAZARUS) OR (ModuleName=_LAZBUILD) then
          begin
            idx:=StringListStartsWith(RevisionStringList,'const RevisionStr');
            if (idx<>-1) then
            begin
              RevString:=RevisionStringList.Strings[idx];
              idx:=Pos('=',RevString);
              if (idx>0) then
              begin
                RevString:=Copy(RevString,idx+1,MaxInt);
                idx:=Pos(';',RevString);
                if (idx>0) then
                begin
                  Delete(RevString,idx,1);
                end;
                RevString:=Trim(RevString);
              end
              else RevString:='';
            end;
          end;
          RevString:=UnQuote(RevString);
        end;
      finally
        RevisionStringList.Free;
      end;
    end;
  end;

  result:=RevString;
end;

function TInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  result:=false;
  infotext:=InitInfoText(' (UnInstallModule: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);
end;

function TInstaller.GetFile(aURL,aFile:string; forceoverwrite:boolean=false; forcenative:boolean=false):boolean;
var
  aUseWget:boolean;
begin
  localinfotext:=InitInfoText(' (GetFile): ');
  aUseWget:=FUseWget;
  if forcenative then aUseWget:=false;
  result:=((FileExists(aFile)) AND (NOT forceoverwrite) AND (FileSize(aFile)>0));
  if (NOT result) then
  begin
    if ((forceoverwrite) AND (SysUtils.FileExists(aFile))) then SysUtils.DeleteFile(aFile);
    Infoln(localinfotext+'Downloading ' + ExtractFileName(aFile));
    Infoln(localinfotext+'Downloading from ' + aURL,etDebug);
    result:=Download(aUseWget,aURL,aFile,FHTTPProxyHost,FHTTPProxyPort,FHTTPProxyUser,FHTTPProxyPassword);
    if (NOT result) then Infoln(localinfotext+'Could not download file with URL ' + aURL +' into ' + ExtractFileDir(aFile) + ' (filename: ' + ExtractFileName(aFile) + ')');
  end;
end;

function TInstaller.GetFullVersionString:string;
begin
  if ((Self.FMajorVersion=-1) OR (Self.FMinorVersion=-1){ OR (Self.FReleaseVersion=-1)}) then
    raise Exception.Create('Fatal: wrong version number(s) !!');
  result:=InttoStr(Self.FMajorVersion)+'.'+InttoStr(Self.FMinorVersion);
  if (Self.FReleaseVersion<>-1) then result:=result+'.'+InttoStr(Self.FReleaseVersion)
end;

function TInstaller.GetFullVersion:dword;
begin
  if ((Self.FMajorVersion=-1) OR (Self.FMinorVersion=-1){ OR (Self.FReleaseVersion=-1)}) then
    raise Exception.Create('Fatal: wrong version number(s) !!');
  if (Self.FReleaseVersion<>-1) then
    result:=CalculateFullVersion(Self.FMajorVersion,Self.FMinorVersion,Self.FReleaseVersion)
  else
    result:=CalculateFullVersion(Self.FMajorVersion,Self.FMinorVersion,0);
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

function TInstaller.GetVersionFromSource:string;
begin
  result:='';
  raise Exception.Create('TInstaller descendants must implement this function themselves.');
end;
function TInstaller.GetVersionFromURL(aUrl:string):string;
begin
  result:='';
  raise Exception.Create('TInstaller descendants must implement this function themselves.');
end;
function TInstaller.GetReleaseCandidateFromSource:integer;
begin
  result:=0;
  raise Exception.Create('TInstaller descendants must implement this function themselves.');
end;

function TInstaller.GetVersion:string;
var
  s:string;
begin
  s:=GetVersionFromSource;
  if s='0.0.0' then s:=GetVersionFromURL(FURL);
  if (s<>'0.0.0') then
  begin
    FMajorVersion := -1;
    FMinorVersion := -1;
    FReleaseVersion := -1;
    FPatchVersion := -1;
    VersionFromString(s,FMajorVersion,FMinorVersion,FReleaseVersion,FPatchVersion);
    if (FPatchVersion=-1) then FPatchVersion:=GetReleaseCandidateFromSource;
    if (FPatchVersion=-1) then FPatchVersion:=ReleaseCandidateFromUrl(FURL);
  end;
  result:=s;
end;

function TInstaller.IsCross:boolean;
begin
  result:=false;
end;

function TInstaller.GetInstallerClass(aClassToFind:TClass):boolean;
begin
  result:=false;
  if (Self.InheritsFrom(aClassToFind) OR (Self.ClassName=aClassToFind.ClassName)) then
  begin
    result:=true;
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

function TInstaller.IsHelpInstaller:boolean;
begin
  result:=GetInstallerClass(TBaseHelpInstaller);
end;

function TInstaller.IsUniversalInstaller:boolean;
begin
  result:=GetInstallerClass(TBaseUniversalInstaller);
end;

function TInstaller.GetFPCConfigPath(const aCFG:string):string;
{$IFDEF UNIX}
var
  aCfgFile:string;
{$ENDIF}
begin
  {$IFDEF UNIX}
  // Due to changes in the FPC sources (3.3.1 and newer), the FPC configs need to be created/moved into a new (local) config directory
  result:=ExpandFileName(FPCBinDir+'/../etc/');
  if (NOT UseCompilerWrapper) AND DirectoryExists(result) then
  begin
    // Copy existing configs into this new config directory and delete the old config
    aCfgFile:=FPCBinDir+DirectorySeparator+aCFG;
    if FileExists(aCfgFile) then
    begin
      FileCopy(aCfgFile,result+aCFG);
      SysUtils.DeleteFile(aCfgFile);
    end;
  end
  else
  {$ENDIF}
  begin
   result:=IncludeTrailingPathDelimiter(FPCBinDir);
  end;

  result:=result+aCFG;
end;

function TInstaller.GetDefaultCompilerFilename(const TargetCPU: TCPU; const Cross: boolean): string;
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
  result:=s+GetExeExt;
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
      if aCPU=TCPU.cpuNone then continue;
      if (Cpu_Target=GetCPU(aCPU)) then
      begin
        result:=GetDefaultCompilerFilename(aCPU,false);
      end;
    end;
  end;
end;

function TInstaller.GetCrossCompilerName(Cpu_Target:TCPU):string;
begin
  if (Cpu_Target<>TCPU.jvm)
     then result:=GetDefaultCompilerFilename(Cpu_Target,true)
     else result:=GetDefaultCompilerFilename(Cpu_Target,false);
end;

procedure TInstaller.Infoln(const Message: string; const Level: TEventType=etInfo);
const
  cDot : array[boolean] of string = ('.','');
begin
  // Note: these strings should remain as is so any fpcupgui highlighter can pick it up
  if (Level<>etDebug) then
  begin
    if AnsiPos(LineEnding, Message)>0 then ThreadLog(''); //Write an empty line before multiline messagse
    ThreadLog(BeginSnippet+' '+Seriousness[Level]+' '+ Message+cDot[Message[Length(Message)] in ['.','!','?']]); //we misuse this for info output
  end
  else
  begin
    {$IFDEF DEBUG}
    {DEBUG conditional symbol is defined using
    Project Options/Other/Custom Options using -dDEBUG}
    if AnsiPos(LineEnding, Message)>0 then ThreadLog(''); //Write an empty line before multiline messagse
    ThreadLog(BeginSnippet+' '+Seriousness[Level]+' '+ Message); //we misuse this for info output
    {$ENDIF}
  end;
 {$ifdef LCL}
 Application.ProcessMessages;
 {$else}
 Sleep(0);
 {$endif}
end;

function TInstaller.ExecuteCommand(const Commandline: string; Verbosity: boolean): integer;
var
  s:string='';
begin
  Result:=ExecuteCommandInDir(Commandline,'',s,Verbosity);
end;

function TInstaller.ExecuteCommand(const Commandline: string; out Output: string;
  Verbosity: boolean): integer;
begin
  Result:=ExecuteCommandInDir(Commandline,'',Output,Verbosity);
end;

function TInstaller.ExecuteCommand(const ExeName:String;const Arguments:array of String;Verbosity:boolean):integer;
var
  s:string='';
begin
  result:=ExecuteCommandInDir(ExeName,Arguments,'',s,'',Verbosity);
end;

function TInstaller.ExecuteCommand(const ExeName:String;const Arguments:array of String;out Output:string;Verbosity:boolean):integer;
begin
  result:=ExecuteCommandInDir(ExeName,Arguments,'',Output,'',Verbosity);
end;

function TInstaller.ExecuteCommandInDir(const Commandline, Directory: string; Verbosity: boolean
  ): integer;
var
  s:string='';
begin
  Result:=ExecuteCommandInDir(Commandline,Directory,s,Verbosity);
end;

function TInstaller.ExecuteCommandInDir(const Commandline, Directory: string;
  out Output: string; Verbosity: boolean): integer;
begin
  Result:=ExecuteCommandInDir(CommandLine,Directory,Output,'',Verbosity);
end;

function TInstaller.ExecuteCommandInDir(const Commandline, Directory: string;
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

      WritelnLog(infotext+aTool.GetExeInfo, false);
      if Verbosity then
        ThreadLog(aTool.GetExeInfo,etCustom);

      result:=aTool.ExecuteAndWait;
      Output:=aTool.WorkerOutput.Text;

      if PrependPath<>'' then
        aTool.Environment.SetVar(PATHVARNAME, OldPath);

      aTool.Verbose:=OldVerbosity;
    end;

  finally
    if NOT Assigned(Processor) then
      aTool.Free;
  end;
end;

function TInstaller.ExecuteCommandInDir(const ExeName:String;const Arguments:array of String;const Directory:String;out Output:string; PrependPath: string;Verbosity:boolean):integer;
var
  OldPath: string;
  OldVerbosity:boolean;
  i:integer;
  aTool:TExternalTool;
begin
  result:=0;

  if Assigned(Processor) then
    aTool:=Processor
  else
    aTool:=TExternalTool.Create(nil);

  try
    aTool.Process.Executable:=ExeName;
    aTool.Process.Parameters.Clear;
    if (Length(Arguments)>0) then aTool.Process.Parameters.AddStrings(Arguments);

    repeat
      i:=aTool.Process.Parameters.IndexOf('emptystring');
      if (i<>-1) then aTool.Process.Parameters[i]:='""';
    until (i=-1);

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

      WritelnLog(infotext+aTool.GetExeInfo, false);
      if Verbosity then
        ThreadLog(aTool.GetExeInfo,etCustom);

      result:=aTool.ExecuteAndWait;
      Output:=aTool.WorkerOutput.Text;

      if PrependPath<>'' then
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

  FErrorCodes := [];

  // Default: set missing libs and bins
  // Availability will be checked by cross-installer
  if (IsFPCInstaller AND IsCross) then
  begin
    Include(FErrorCodes,ieBins);
    Include(FErrorCodes,ieLibs);
  end;

  FCrossInstaller:=nil;

  FExternalTool:=TExternalTool.Create(nil);
  FExternalToolResult:=0;

  FCPUCount  := GetLogicalCpuCount;

  FSVNClient := TSVNClient.Create(Self);
  FGitClient := TGitClient.Create(Self);
  FHGClient  := THGClient.Create(Self);

  FShell        := '';
  FMakeDir      := '';
  FMakePath     := '';

  FNeededExecutablesChecked:=false;
  FCleanModuleSuccess:=false;

  // Set up verbose log: will be done in dumpoutput
  // as it depends on verbosity etc
  //FLogVerbose: TLogger.Create;
  FErrorLog := TStringList.Create;

  FCrossCPU_Target:=TCPU.cpuNone;
  FCrossOS_Target:=TOS.osNone;
  FCrossOS_SubArch:=TSUBARCH.saNone;
  FCrossOS_ABI:=TABI.default;

  FMajorVersion   := -1;
  FMinorVersion   := -1;
  FReleaseVersion := -1;
  FPatchVersion   := -1;

  FMUSL           := false;
  FLinuxLegacy    := false;
  FSolarisOI      := false;
  FUltibo         := false;
  FFPCUnicode     := false;

  {$ifdef Linux}
  FMUSLLinker:='/lib/ld-musl-'+GetSourceCPU+'.so.1';
  FMUSL:=(FileExists(FMUSLLinker) AND IsLinuxMUSL);
  if FMUSL then Infoln('Fpcupdeluxe: We have a MUSL Linux version !',etInfo);
  {$endif}

  {$ifdef UNIX}
  {$ifdef DISABLE_PPC_CONFIG_PATH}
  FUseCompilerWrapper:=false;
  {$else}
  FUseCompilerWrapper:=true;
  //FUseCompilerWrapper:=(Length(GetEnvironmentVariable('PPC_CONFIG_PATH'))=0);
  {$endif}
  {$endif}

  FQTTrickeryNeeded:=false;
  {$IF DEFINED(LCLQt5) OR DEFINED(LCLQt6)}
  {$ifdef LCLQT5}
  FQTLibs:=LIBQT5;
  FQTLibsVersioned:=LIBQT5VERSION;
  {$endif}
  {$ifdef LCLQT6}
  FQTLibs:=LIBQT6;
  FQTLibsVersioned:=LIBQT6VERSION;
  {$endif}
  FQTTrickeryNeeded:=(NOT LibWhich(FQTLibs));
  {$ENDIF}

  SanityCheck;
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

