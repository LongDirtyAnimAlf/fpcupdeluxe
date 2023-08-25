unit m_crossinstaller;
{
General crossinstaller/updater module

Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands
Copyright (C) 2015-2017 Alfred Glänzer

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

{
About GLIBC
NEWS for version 2.34
=====================

Major new features:

* In order to support smoother in-place-upgrades and to simplify
  the implementation of the runtime all functionality formerly
  implemented in the libraries libpthread, libdl, libutil, libanl has
  been integrated into libc.  New applications do not need to link with
  -lpthread, -ldl, -lutil, -lanl anymore.  For backwards compatibility,
  empty static archives libpthread.a, libdl.a, libutil.a, libanl.a are
  provided, so that the linker options keep working.  Applications which
  have been linked against glibc 2.33 or earlier continue to load the
  corresponding shared objects (which are now empty).  The integration
  of those libraries into libc means that additional symbols become
  available by default.  This can cause applications that contain weak
  references to take unexpected code paths that would only have been
  used in previous glibc versions when e.g. preloading libpthread.so.0,
  potentially exposing application bugs.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ErrorNotFound='An error occurred getting cross compiling binutils/libraries.'+LineEnding+
    'todo: specify what exactly is missing';

  CrossWindowsSuggestion = 'Suggestion for cross binutils: the crossfpc binutils at https://gitlab.com/freepascal.org/fpc/binaries/-/tree/main/i386-win32.';

  MAXDARWINVERSION=20;
  MINDARWINVERSION=7;

  MAXOSVERSION=15;
  MINOSVERSION=8;

  MAXDELPHIVERSION=22;
  MINDELPHIVERSION=12;
  NDKVERSIONNAMES:array[0..31] of string = ('7','7b','7c','8','8b','8c','8d','8e','9','9b','9c','9d','10','10b','10c','10d','10e','11','11b','11c','12','12b','13b','14b','15c','16b','17c','18b','19c','20b','21e','22b');
  //PLATFORMVERSIONSNUMBERS:array[0..13] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22); //23 does not yet work due to text allocations
  PLATFORMVERSIONSNUMBERS:array[0..22] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31);
  {$ifdef unix}
  UnixBinDirs :array[0..2] of string = ('/usr/local/bin','/usr/bin','/bin');
  UnixLibDirs :array[0..2] of string = ('/usr/local/lib','/usr/lib','/lib');
  {$endif}
  DEFAULTARMCPU  = 'ARMV7A';

  CROSSDIRNAME      = 'cross';
  CROSSLIBDIRNAME   = 'lib';
  CROSSBINDIRNAME   = 'bin';
  CROSSLIBPATH      = CROSSDIRNAME+DirectorySeparator+CROSSLIBDIRNAME;
  CROSSBINPATH      = CROSSDIRNAME+DirectorySeparator+CROSSBINDIRNAME;

  LIBCFILENAME   = 'libc.so';
  LDFILENAME     = 'ld';
  ASFILENAME     = 'as';

type
  TCPU      = (cpuNone,i386,x86_64,arm,aarch64,powerpc,powerpc64,mips,mipsel,avr,jvm,i8086,sparc,sparc64,riscv32,riscv64,m68k,xtensa,wasm32,loongarch64);
  TOS       = (osNone,win32,win64,linux,android,darwin,freebsd,openbsd,aix,wince,iphonesim,embedded,java,msdos,haiku,solaris,dragonfly,netbsd,morphos,aros,amiga,go32v2,freertos,ios,ultibo,wasi,atari);
  TSUBARCH  = (saNone,armv4,armv4t,armv6,armv6m,armv7a,armv7em,armv7m,armv8,armv8a,avr1,avr2,avr25,avr35,avr4,avr5,avr51,avr6,avrtiny,avrxmega3,pic32mx,rv32imac,rv32ima,rv32im,rv32i,rv64imac,rv64ima,rv64im,rv64i,lx6,lx106);
  //TABI      = (default,sysv,aix,darwin,elfv2,eabi,armeb,eabihf,oldwin32gnu,aarch64ios,riscvhf,linux386_sysv,windowed,call0);
  TABI      = (default,eabi,eabihf,aarch64ios,riscvhf,windowed,call0);
  TARMARCH  = (none,armel,armeb,armhf);

  TSUBARCHS = set of TSUBARCH;
  TABIS     = set of TABI;

const
  LCL_OS             = [TOS.win32,TOS.win64,TOS.linux,TOS.darwin,TOS.freebsd,TOS.openbsd,TOS.aix,TOS.wince,TOS.haiku,TOS.solaris,TOS.dragonfly,TOS.netbsd,TOS.morphos,TOS.aros,TOS.amiga];
  SUBARCH_OS         = [TOS.embedded,TOS.freertos,TOS.ultibo];
  SUBARCH_CPU        = [TCPU.arm,TCPU.aarch64,TCPU.avr,TCPU.mipsel,TCPU.riscv32,TCPU.riscv64,TCPU.xtensa]; //for Ultibo added TCPU.aarch64
  SUBARCH_ARM        = [TSUBARCH.armv4..TSUBARCH.armv7m];
  SUBARCH_AARCH64    = [TSUBARCH.armv8a];
  SUBARCH_AVR        = [TSUBARCH.avr1..TSUBARCH.avrxmega3];
  SUBARCH_MIPSEL     = [TSUBARCH.pic32mx];
  SUBARCH_RISCV32    = [TSUBARCH.rv32imac..TSUBARCH.rv32i];
  SUBARCH_RISCV64    = [TSUBARCH.rv64imac..TSUBARCH.rv64i];
  SUBARCH_XTENSA     = [TSUBARCH.lx6..TSUBARCH.lx106];
  SUBARCH_ULTIBO     = [TSUBARCH.armv6,TSUBARCH.armv7a,TSUBARCH.armv8];

  ABI_ARM            = [TABI.default,TABI.eabi,TABI.eabihf];
  ABI_XTENSA         = [TABI.default,TABI.windowed,TABI.call0];
  ABI_RISCV64        = [TABI.default,TABI.riscvhf];

  CPUADDRSIZE_64     = [TCPU.aarch64,TCPU.powerpc64,TCPU.riscv64,TCPU.sparc64,TCPU.x86_64,TCPU.loongarch64{,TCPU.ia64]}];
  CPUADDRSIZE_32     = [TCPU.i386,TCPU.arm,TCPU.powerpc,TCPU.mips,TCPU.mipsel,TCPU.sparc,TCPU.riscv32,TCPU.m68k,TCPU.xtensa,TCPU.wasm32];

type
  TSearchSetting = (ssUp,ssAuto,ssCustom);

const
  DEFINE_FPC_USE_LIBC      = 'FPC_USE_LIBC';

  ARMArchFPCStr : array[TARMARCH] of string = (
    '','-dFPC_ARMEL','-dFPC_ARMEB','-dFPC_ARMHF'
  );
  FPCUP_AUTO_MAGIC      = 'FPCUP_AUTO';

  FPC_TARGET_MAGIC      = '$fpctarget';
  FPC_SUBARCH_MAGIC     = '$fpcsubarch';
  FPC_ABI_MAGIC         = '$fpcabi';

  DEFAULTSEARCHSETTING  = TSearchSetting.ssUp;
  DEFAULTARMARCH        = TARMARCH.none;

type
  TCrossUtil = record
    Setting:TSearchSetting;
    LibDir:string;
    BinDir:string;
    CrossBuildOptions:string;
    CrossARMArch:TARMARCH;
    Compiler:string;
    //Available:boolean;
  end;

  //TFPCTargetValid = array[TCPU,TOS] of boolean;
  TCrossUtils = array[TCPU,TOS,TSUBARCH] of TCrossUtil;

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  private
    function GetLibsPath:string;
    function GetBinutilsPath:string;
    function GetCrossModuleName:string;
    function GetSourceCPUName:string;
    function GetSourceOSName:string;
    function GetSubarchName:string;
    function GetABIName:string;
  protected
    FFPCVersion: string;
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinutilsPathInPath: boolean;
    FUtilsDirectoryID: string; //where to find the utils (tools/libs) themselves
    FSearchMode: TSearchSetting;
    FCrossModuleNamePrefix: string; //used for identifying module to user in messages
    FCrossOpts: TStringList; //Options to be added to CROSSOPT by the calling code. XP= (binutils prefix) is already done, no need to add it
    FFPCCFGSnippet: string; //snippet to be added to fpc.cfg in order to find binutils/libraries etc
    FLibsPath: string; //path for target environment libraries
    FTargetCPU: TCPU; //cpu for the target environment. Follows FPC names
    FTargetOS: TOS; //operating system for the target environment. Follows FPC names
    FSubArch: TSUBARCH; //optional subarch for embedded targets
    FABI: TABI; //optional subarch for embedded targets
    FRegisterName: string;
    FLibsFound,FBinsFound,FCrossOptsAdded:boolean;
    FSolarisOI:boolean;
    FMUSL:boolean;
    FLinuxLegacy:boolean;
    function PerformLibraryPathMagic(out LibraryPath:string):boolean;
    function SearchLibrary(Directory, LookFor: string): boolean;
    function SimpleSearchLibrary(BasePath,DirName: string; const LookFor:string): boolean;
    function SearchBinUtil(Directory, LookFor: string): boolean;
    function SimpleSearchBinUtil(BasePath,DirName: string; const LookFor:string): boolean;
    procedure SearchLibraryInfo(found:boolean; const extrainfo:string='');
    procedure SearchBinUtilsInfo(found:boolean; const extrainfo:string='');
    function SearchUtil(Directory, LookFor: string; LibsOrBins:boolean): boolean;
    function FPCUPToolsSearch(BasePath,DirName: string; LibsOrBins:boolean; const LookFor:string): boolean;
  public
    // In your descendent, implement this function: you can download libraries or check for their existence for normal cross compile libs:
    function GetLibs(Basepath:string):boolean;virtual;
    // In your descendent, implement this function: you can download cross compile binutils or check for their existence
    function GetBinUtils(Basepath:string):boolean;virtual;
    {$ifndef FPCONLY}
    // In your descendent, implement this function when needed: you can download libraries or check for their existence for Lazarus LCL cross compile libs:
    // Note: the libraries should be presumably under the basepath using the Lazarus naming convention??
    function GetLibsLCL({%H-}LCL_Platform:string; {%H-}Basepath:string):boolean;virtual;
    {$endif}
    procedure AddFPCCFGSnippet(const aSnip: string; const AddToCrossOptions:boolean=true);
    procedure AddCrossOption(const aOption: string);
    procedure ReplaceFPCCFGSnippet(aOldSnip,aNewSnip: string);
    procedure SetFPCVersion(aVersion: string);
    // Parses space-delimited crossopt parameters and sets the CrossOpt property
    procedure SetCrossOpt(CrossOpts: string);
    // Pass subarch if any
    procedure SetSubArch(SubArch: TSUBARCH);
    // Pass ABI if any
    procedure SetABI(ABI: TABI);
    procedure ShowInfo(info: string = ''; Level: TEventType = etInfo);
    // Reset some variables to default values
    procedure Reset; virtual;
    property SearchModeUsed: TSearchSetting read FSearchMode write FSearchMode;
    property CrossModuleName: string read GetCrossModuleName;
    // Represents arguments for CROSSOPT parameter
    // No need to add XP= (binutils prefix): calling code will do this
    // CROSSOPT: Compiler makefile allows to specify compiler options that are only used during the actual crosscompiling phase (i.e. not during the initial bootstrap cycle)
    // Also used in fpc.cfg snippet to set options when compiling for cross target
    property CrossOpt: TStringList read FCrossOpts;
    property FPCVersion: string read FFPCVersion;
    // Conditional define snippet for fpc.cfg used to specify library locations etc
    // Can be empty
    // Does not include the #IFDEF CPU<x> and #ENDIF parts where the target cpu is filled in
    property FPCCFGSnippet: string read FFPCCFGSnippet;
    // Path where libraries used for target systems are. May be empty if not needed.
    property LibsPath:string read GetLibsPath;
    // Path where binutils used for target systems are. May be empty if not used.
    property BinUtilsPath:string read GetBinutilsPath;
    // Indicates if binutils directory is used as the last entry in PATH when cross compiling.
    // Can be useful if make scripts forget to include the complete path to the binutils path
    // (e.g. some versions of the DOS crosscompiler)
    property BinUtilsPathInPath: boolean read FBinutilsPathInPath;
    // Prefix used before executable names for binutils (e.g. before as.exe). May be empty.
    property BinUtilsPrefix:string read FBinUtilsPrefix;
    property DirName:string read FUtilsDirectoryID;
    property TargetCPU:TCPU read FTargetCPU;
    property TargetOS:TOS read FTargetOS;
    property SubArch:TSUBARCH read FSubArch;
    property ABI:TABI read FABI;
    property TargetCPUName: string read GetSourceCPUName;
    property TargetOSName: string read GetSourceOSName;
    property SubArchName:string read GetSubarchName;
    property ABIName:string read GetABIName;
    property RegisterName:string read FRegisterName;
    property SolarisOI: boolean write FSolarisOI;
    property MUSL: boolean write FMUSL;
    property LinuxLegacy: boolean write FLinuxLegacy;

    constructor Create;
    destructor Destroy; override;
  end;

function GetCPU(aCPU:TCPU):string;
function GetCPUCase(aCPU:TCPU):string;
function GetTCPU(aCPU:string):TCPU;
function GetOS(aOS:TOS):string;
function GetOSCase(aOS:TOS):string;
function GetTOS(aOS:string):TOS;
function GetSubarch(aSubarch:TSUBARCH):string;
function GetTSubarch(aSubarch:string):TSUBARCH;
function GetSubarchs(aCPU:TCPU;aOS:TOS):TSUBARCHS;
function GetARMArch(aARMarch:TARMARCH):string;
function GetTARMArch(aARMArch:string):TARMARCH;
function GetARMArchFPCDefine(aARMArch:TARMARCH):string;
function GetABI(aABI:TABI):string;
function GetTABI(aABI:string):TABI;
function GetABIs(aCPU:TCPU;aOS:TOS):TABIS;
function IsCPUOSComboValid(CPU:TCPU;OS:TOS):boolean;
{$ifdef LCL}
function  GetSelectedSubArch(aCPU:TCPU;aOS:TOS):TSUBARCH;
procedure SetSelectedSubArch(aCPU:TCPU;aOS:TOS;aSUBARCH:TSUBARCH);
{$endif LCL}
procedure GetCrossToolsDir(const CrossCPU_Target:TCPU;const CrossOS_Target:TOS; const MUSL,SolarisOI,LinuxLegacy:boolean; out BinPath,LibPath:string);
procedure RegisterCrossCompiler(Platform:string;aCrossInstaller:TCrossInstaller);
function GetExeExt(const aOS:TOS=TOS.osNone): string;

var
  //FPCTargetValid:TFPCTargetValid;
  {$ifdef LCL}
  CrossUtils:TCrossUtils;
  SUBARCHStore:array[TCPU,TOS] of TSUBARCH;
  {$endif LCL}
  CrossInstallers:TStringList=nil;

implementation

uses
  StrUtils,
  processutils,// for ThreadLog
  fpcuputil;

const
  MULTIDEFCOMPILERSWITCHES : array [0..7] of string = (
  '-Fi',
  '-Fl',
  '-Fu',
  '-Ff',
  '-FN',
  '-Fo',
//  '-d',
//  '-u',
  '-I',
  '-k'
  );

function GetCPU(aCPU:TCPU):string;
begin
  if (aCPU<Low(TCPU)) OR (aCPU>High(TCPU)) OR (aCPU=TCPU.cpuNone) then
    raise Exception.Create('Invalid CPU for GetCPU.');
  result:=GetEnumNameSimple(TypeInfo(TCPU),Ord(aCPU));
end;

function GetCPUCase(aCPU:TCPU):string;
begin
  if aCPU=TCPU.arm then result:='ARM' else
    if aCPU=TCPU.aarch64 then result:='AArch64' else
      if aCPU=TCPU.i386 then result:='i386' else
        if aCPU=TCPU.x86_64 then result:='AMD64' else
          if aCPU=TCPU.powerpc then result:='PowerPC' else
            if aCPU=TCPU.powerpc64 then result:='PowerPC64' else
              if aCPU=TCPU.avr then result:='AVR' else
                if aCPU=TCPU.m68k then result:='m68k' else
                  result:=UppercaseFirstChar(GetCPU(aCPU));
end;

function GetTCPU(aCPU:string):TCPU;
var
  xCPU:TCPU;
begin
  result:=TCPU.cpuNone;
  if length(aCPU)>0 then
  begin
    if aCPU='ppc' then xCPU:=TCPU.powerpc
    else
    if aCPU='ppc64' then xCPU:=TCPU.powerpc64
    else
    if aCPU='x8664' then xCPU:=TCPU.x86_64
    else
    begin
      xCPU:=TCPU(GetEnumValueSimple(TypeInfo(TCPU),aCPU));
      if Ord(xCPU) < 0 then
        raise Exception.CreateFmt('Invalid CPU name "%s" for GetCPU.', [aCPU]);
    end;
    result:=xCPU;
  end;
end;

function GetOS(aOS:TOS):string;
begin
  if (aOS<Low(TOS)) OR (aOS>High(TOS)) OR (aOS=TOS.osNone) then
    raise Exception.Create('Invalid OS for GetOS.');
  result:=GetEnumNameSimple(TypeInfo(TOS),Ord(aOS));
end;

function GetOSCase(aOS:TOS):string;
begin
  if aOS=TOS.morphos then result:='MorphOS' else
    if aOS=TOS.aros then result:='ArOS' else
      if aOS=TOS.freebsd then result:='FreeBSD' else
        if aOS=TOS.dragonfly then result:='DragonFlyBSD' else
          if aOS=TOS.openbsd then result:='OpenBSD' else
            if aOS=TOS.netbsd then result:='NetBSD' else
              if aOS=TOS.aix then result:='AIX' else
                if aOS=TOS.msdos then result:='MSDos' else
                  if aOS=TOS.freertos then result:='FreeRTOS' else
                    if aOS=TOS.win32 then result:='Windows' else
                      if aOS=TOS.win64 then result:='Windows' else
                        if aOS=TOS.ios then result:='IOS' else
                        result:=UppercaseFirstChar(GetOS(aOS));
end;

function GetTOS(aOS:string):TOS;
var
  xOS:TOS;
begin
  result:=TOS.osNone;
  if length(aOS)>0 then
  begin
    if (Pos('windows',aOS)>0) then xOS:=TOS.win32
    else
    if (Pos('linux',aOS)>0) then xOS:=TOS.linux
    else
    if (Pos('solaris',aOS)>0) then xOS:=TOS.solaris
    else
    if aOS='windows' then xOS:=TOS.win32
    else
    if aOS='i-sim' then xOS:=TOS.iphonesim
    else
    if aOS='i-simulator' then xOS:=TOS.iphonesim
    else
    if aOS='iphone-simulator' then xOS:=TOS.iphonesim
    else
    if aOS='iphonesimulator' then xOS:=TOS.iphonesim
    else
    begin
      xOS:=TOS(GetEnumValueSimple(TypeInfo(TOS),aOS));
      if Ord(xOS) < 0 then
        raise Exception.CreateFmt('Invalid OS name "%s" for GetOS.', [aOS]);
    end;
    result:=xOS;
  end;
end;

function GetSubarch(aSubarch:TSUBARCH):string;
begin
  if (aSubarch<Low(TSUBARCH)) OR (aSubarch>High(TSUBARCH)) then
    raise Exception.Create('Invalid Subarch for GetSubarch.');
  result:=GetEnumNameSimple(TypeInfo(TSUBARCH),Ord(aSubarch));
end;

function GetTSubarch(aSubarch:string):TSUBARCH;
var
  xSubarch:TSUBARCH;
begin
  result:=TSUBARCH.saNone;
  if (Length(aSubarch)>0) then
  begin
    xSubarch:=TSUBARCH(GetEnumValueSimple(TypeInfo(TSUBARCH),aSubarch));
    if Ord(xSubarch) < 0 then
      raise Exception.CreateFmt('Invalid Subarch name "%s" for GetSubarch.', [xSubarch]);
    result:=xSubarch;
  end;
end;

function GetSubarchs(aCPU:TCPU;aOS:TOS):TSUBARCHS;
begin
  result:=[TSUBARCH.saNone];
  if ((aOS in SUBARCH_OS) AND (aCPU in SUBARCH_CPU)) then
  begin
    case aCPU of
      TCPU.arm:      if (aOS<>TOS.ultibo) then result:=SUBARCH_ARM;
      TCPU.aarch64:  if (aOS=TOS.embedded) then result:=SUBARCH_AARCH64;
      TCPU.avr:      if (aOS=TOS.embedded) then result:=SUBARCH_AVR;
      TCPU.mipsel:   if (aOS=TOS.embedded) then result:=SUBARCH_MIPSEL;
      TCPU.riscv32:  if (aOS=TOS.embedded) then result:=SUBARCH_RISCV32;
      TCPU.riscv64:  if (aOS=TOS.embedded) then result:=SUBARCH_RISCV64;
      TCPU.xtensa:   if (aOS<>TOS.ultibo) then result:=SUBARCH_XTENSA;
    end;
    // Limit some special targets
    if (aOS=TOS.ultibo) then
    begin
      case aCPU of
        TCPU.arm:      result:=[TSUBARCH.armv6,TSUBARCH.armv7a];
        TCPU.aarch64:  result:=[TSUBARCH.armv8];
      end;
    end;
    if ((aOS=TOS.freertos) AND (aCPU=TCPU.arm)) then result:=[TSUBARCH.armv6m,TSUBARCH.armv7em,TSUBARCH.armv7m];
  end;
end;

function GetARMArch(aARMarch:TARMARCH):string;
begin
  if (aARMarch<Low(TARMARCH)) OR (aARMarch>High(TARMARCH)) then
    raise Exception.Create('Invalid ARMarch for GetARMarch.');
  result:=GetEnumNameSimple(TypeInfo(TARMARCH),Ord(aARMarch));
end;

function GetTARMArch(aARMArch:string):TARMARCH;
begin
  if Length(aARMArch)=0 then
    result:=DEFAULTARMARCH
  else
  if aARMArch='default' then
    result:=DEFAULTARMARCH
  else
    result:=TARMARCH(GetEnumValueSimple(TypeInfo(TARMARCH),aARMArch));
  if Ord(result) < 0 then
    raise Exception.CreateFmt('Invalid ARM Arch name "%s" for GetARMArch.', [aARMArch]);
end;

function GetARMArchFPCDefine(aARMArch:TARMARCH):string;
begin
  result:=ARMArchFPCStr[aARMArch];
end;

function GetABI(aABI:TABI):string;
begin
  if aABI=TABI.default then
    result:=''
  else
  begin
    if (aABI<Low(TABI)) OR (aABI>High(TABI)) then
      raise Exception.Create('Invalid ABI for GetABI.');
    result:=GetEnumNameSimple(TypeInfo(TABI),Ord(aABI));
  end;
end;

function GetTABI(aABI:string):TABI;
begin
  if Length(aABI)=0 then
    result:=TABI.default
  else
    result:=TABI(GetEnumValueSimple(TypeInfo(TABI),aABI));
  if Ord(result) < 0 then
    raise Exception.CreateFmt('Invalid ARM ABI name "%s" for GetTABI.', [aABI]);
end;

function GetABIs(aCPU:TCPU;aOS:TOS):TABIS;
begin
  result:=[TABI.default];
  if ((aOS in SUBARCH_OS) AND (aCPU in SUBARCH_CPU)) then
  begin
    case aCPU of
      TCPU.arm:      if (aOS<>TOS.ultibo) then result:=ABI_ARM;
      TCPU.xtensa:   if (aOS<>TOS.ultibo) then result:=ABI_XTENSA;
      TCPU.riscv64:  if (aOS<>TOS.ultibo) then result:=ABI_RISCV64;
    end;
    //if ((aOS=TOS.ultibo) AND (aCPU=TCPU.arm)) then result:=TABI.eabi;
  end;
end;

{$ifdef LCL}
function GetSelectedSubArch(aCPU:TCPU;aOS:TOS):TSUBARCH;
begin
  result:=SUBARCHStore[aCPU,aOS];
end;

procedure SetSelectedSubArch(aCPU:TCPU;aOS:TOS;aSUBARCH:TSUBARCH);
begin
  SUBARCHStore[aCPU,aOS]:=aSUBARCH;
end;
{$endif LCL}

function IsCPUOSComboValid(CPU:TCPU;OS:TOS):boolean;
begin
  // This is very static.
  // Might be replaced by a scan of systems.inc, as done by TFPCupManager.CheckValidCPUOS

  result:=false;

  if CPU=cpuNone then exit;
  if OS=osNone then exit;

  if (OS=morphos) AND (CPU<>powerpc) then exit;
  if ((OS=java) AND (CPU<>jvm)) OR ((CPU=jvm) AND (OS<>java) AND (OS<>android)) then exit;
  if (OS=ultibo) AND ((CPU<>arm) AND (CPU<>aarch64)) then exit;
  if (OS=android) AND ((CPU<>arm) AND (CPU<>aarch64) AND (CPU<>jvm) AND (CPU<>mipsel)) then exit;
  if (OS=iphonesim) AND ((CPU<>i386) AND (CPU<>x86_64)) then exit;
  if (OS=wince) AND (CPU<>arm) then exit;
  if (OS=win32) AND ((CPU<>i386) AND (CPU<>x86_64)) then exit;
  if (OS=win64) AND ((CPU<>i386) AND (CPU<>x86_64) AND (CPU<>aarch64)) then exit;
  if (OS=haiku) AND ((CPU<>i386) AND (CPU<>x86_64) {AND (CPU<>arm)}) then exit;
  if (OS=solaris) AND ((CPU<>x86_64) AND (CPU<>sparc)) then exit;
  if (OS=ios) AND ((CPU<>arm) AND (CPU<>aarch64)) then exit;
  if ((OS=wasi) AND (CPU<>wasm32)) then exit;
  if ((OS=atari) AND (CPU<>m68k)) then exit;

  if (CPU=xtensa) AND ((OS<>linux) AND (OS<>freertos)) then exit;
  if (CPU=m68k) AND ((OS<>linux) AND (OS<>amiga)) then exit;
  if (CPU=powerpc) AND ((OS<>aix) AND (OS<>linux) AND (OS<>darwin)) then exit;
  if (CPU=powerpc64) AND ((OS<>aix) AND (OS<>linux) AND (OS<>darwin)) then exit;
  if (CPU=mips) AND (OS<>linux) then exit;
  if (CPU=mipsel) AND ((OS<>linux) AND (OS<>android) AND (OS<>embedded)) then exit;
  if (CPU=avr) AND (OS<>embedded) then exit;
  if (CPU=sparc64) AND (OS<>linux) then exit;
  if ((CPU=riscv32) OR (CPU=riscv64)) AND ((OS<>linux) AND (OS<>embedded)) then exit;
  if (CPU=wasm32) AND ((OS<>wasi) AND (OS<>embedded)) then exit;
  if (CPU=loongarch64) AND (OS<>linux) then exit;

  result:=true;
end;

function GetExeExt(const aOS:TOS=TOS.osNone): string;
begin
  if (aOS=TOS.osNone) then
  begin
    {$IFDEF WINDOWS}
    result:='.exe';
    {$ELSE}
    result:='';
    {$ENDIF}
  end
  else
  begin
    if aOS in [TOS.win32,TOS.win64,TOS.wince] then
      result:='.exe'
    else
      result:='';
  end;
end;

procedure GetCrossToolsDir(const CrossCPU_Target:TCPU;const CrossOS_Target:TOS; const MUSL,SolarisOI,LinuxLegacy:boolean; out BinPath,LibPath:string);
begin
  // Setting the location of libs and bins on our system, so they can be found by fpcupdeluxe
  LibPath:=GetCPU(CrossCPU_Target)+'-'+GetOS(CrossOS_Target);
  BinPath:=GetCPU(CrossCPU_Target)+'-'+GetOS(CrossOS_Target);

  if SolarisOI then
  begin
    LibPath:=LibPath+'-oi';
    BinPath:=BinPath+'-oi';
  end
  else
  if MUSL then
  begin
    LibPath:=LibPath+'-musl';
    BinPath:=BinPath+'-musl';
  end
  else
  if LinuxLegacy then
  begin
    LibPath:=LibPath+'-legacy';
  end;

  {$IF (defined(Windows)) OR (defined(Linux))}
  // Set special Bins directory for universal tools for Darwin based on clang
  if (
    ((CrossOS_Target=TOS.darwin) AND (CrossCPU_Target in [TCPU.i386,TCPU.x86_64,TCPU.aarch64]))
    OR
    ((CrossOS_Target=TOS.ios) AND (CrossCPU_Target in [TCPU.arm,TCPU.aarch64]))
    ) then
  begin
    BinPath:=StringReplace(BinPath,GetCPU(CrossCPU_Target),'all',[]);
    BinPath:=StringReplace(BinPath,GetOS(CrossOS_Target),'apple',[]);
  end;

  // Set special Bins directory for universal tools for Android based on clang
  if CrossOS_Target=TOS.android then
  begin
    BinPath:=StringReplace(BinPath,GetCPU(CrossCPU_Target),'all',[]);
  end;
  {$endif}

  // Set special Bins directory for universal tools for wasm32
  if CrossCPU_Target=TCPU.wasm32 then
  begin
    BinPath:=StringReplace(BinPath,GetOS(CrossOS_Target),'all',[]);
  end;

  if CrossOS_Target=TOS.darwin then
  begin
    // Darwin is special: combined binaries and libs for i386 and x86_64 with osxcross
    if (CrossCPU_Target=TCPU.i386) OR (CrossCPU_Target=TCPU.x86_64) OR (CrossCPU_Target=TCPU.aarch64) then
    begin
      BinPath:=StringReplace(BinPath,GetCPU(CrossCPU_Target),'all',[]);
      LibPath:=StringReplace(LibPath,GetCPU(CrossCPU_Target),'all',[]);
    end;
    if (CrossCPU_Target=TCPU.powerpc) OR (CrossCPU_Target=TCPU.powerpc64) then
    begin
      BinPath:=StringReplace(BinPath,GetCPU(CrossCPU_Target),GetCPU(TCPU.powerpc),[]);
      LibPath:=StringReplace(LibPath,GetCPU(CrossCPU_Target),GetCPU(TCPU.powerpc),[]);
    end;
  end;

  if CrossOS_Target=TOS.ios then
  begin
    // iOS is special: combined libs for arm and aarch64
    if (CrossCPU_Target=TCPU.arm) OR (CrossCPU_Target=TCPU.aarch64) then
    begin
      BinPath:=StringReplace(BinPath,GetCPU(CrossCPU_Target),'all',[]);
      LibPath:=StringReplace(LibPath,GetCPU(CrossCPU_Target),'all',[]);
    end;
  end;

  if CrossOS_Target=TOS.aix then
  begin
    // AIX is special: combined binaries and libs for ppc and ppc64 with osxcross
    if (CrossCPU_Target=TCPU.powerpc) OR (CrossCPU_Target=TCPU.powerpc64) then
    begin
      BinPath:=StringReplace(BinPath,GetCPU(CrossCPU_Target),GetCPU(TCPU.powerpc),[]);
      LibPath:=StringReplace(LibPath,GetCPU(CrossCPU_Target),GetCPU(TCPU.powerpc),[]);
    end;
  end;

  //Put all windows stuff (not that much) in a single windows directory
  if (CrossOS_Target=TOS.win32) OR (CrossOS_Target=TOS.win64) then
  begin
    BinPath:=StringReplace(BinPath,GetOS(CrossOS_Target),'windows',[]);
    LibPath:=StringReplace(LibPath,GetOS(CrossOS_Target),'windows',[]);
  end;
end;

procedure RegisterCrossCompiler(Platform:string;aCrossInstaller:TCrossInstaller);
begin
  if not assigned(CrossInstallers) then
    CrossInstallers:=TStringList.Create;
  CrossInstallers.AddObject(Platform,TObject(aCrossInstaller));
end;

{ TCrossInstaller }

function TCrossInstaller.GetLibsPath:string;
begin
  result:=ExcludeTrailingPathDelimiter(FLibsPath);
end;

function TCrossInstaller.GetBinutilsPath:string;
begin
  result:=ExcludeTrailingPathDelimiter(FBinUtilsPath);
end;

function TCrossInstaller.GetCrossModuleName:string;
begin
  result:=FCrossModuleNamePrefix+'_'+GetSourceOSName+'-'+GetSourceCPUName;
end;

function TCrossInstaller.GetSourceCPUName:string;
begin
  result:=GetCPU(TargetCPU);
end;

function TCrossInstaller.GetSourceOSName:string;
begin
  result:=GetOS(TargetOS);
end;

function TCrossInstaller.GetSubarchName:string;
begin
  result:=GetSubarch(Subarch);
end;

function TCrossInstaller.GetABIName:string;
begin
  result:=GetABI(ABI);
end;

procedure TCrossInstaller.AddFPCCFGSnippet(const aSnip: string; const AddToCrossOptions:boolean);
var
  aSnippd:string;
  compilerswitch:string;
  CheckValidOption:boolean;
begin
  aSnippd:=Trim(aSnip);

  if Length(aSnippd)=0 then exit;

  if (Pos('-Xr',aSnippd)=1) then
  begin
    // Do not add -Xr on platforms that do not support it
    CheckValidOption:=false;
    if (TargetOS in [TOS.linux,TOS.solaris,TOS.android,TOS.openbsd]) then CheckValidOption:=true;
    if ((TargetOS in [TOS.haiku]) AND (TargetCPU in [TCPU.i386,TCPU.x86_64])) then CheckValidOption:=true;
    if ((TargetOS in [TOS.amiga]) AND (TargetCPU in [TCPU.m68k])) then CheckValidOption:=true;
    if (NOT CheckValidOption) then exit;
  end;

  if AddToCrossOptions then AddCrossOption(aSnippd);

  aSnippd:=StringReplace(aSnippd,'#IFDEF ','#IFDEF_',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#IFNDEF ','#IFNDEF_',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#ENDIF ','#ENDIF_',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,' ',LineEnding,[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#IFDEF_','#IFDEF ',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#IFNDEF_','#IFNDEF ',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#ENDIF_','#ENDIF ',[rfReplaceAll]);

  // Check for duplicates
  CheckValidOption:=false;
  if (NOT CheckValidOption) then CheckValidOption:=(Pos('#IFDEF',aSnippd)>0);
  if (NOT CheckValidOption) then CheckValidOption:=(Pos('#IFNDEF',aSnippd)>0);
  if (NOT CheckValidOption) then CheckValidOption:=(Pos('#ENDIF',aSnippd)>0);

  if (NOT CheckValidOption) then
  begin
    for compilerswitch in MULTIDEFCOMPILERSWITCHES do
    begin
      CheckValidOption:=(Pos(compilerswitch,aSnippd)=1);
      if CheckValidOption then break;
    end;
  end;

  if (NOT CheckValidOption) then
  begin
    if (Pos(aSnippd,FFPCCFGSnippet)>0) then exit;
  end;

  if Length(FPCCFGSnippet)>0 then
  begin
    if RPos(LineEnding,FFPCCFGSnippet)<Length(FFPCCFGSnippet) then FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding;
    FFPCCFGSnippet:=FFPCCFGSnippet+aSnippd;
  end
  else FFPCCFGSnippet:=aSnippd;
end;

procedure TCrossInstaller.AddCrossOption(const aOption: string);
var
  index:integer;
  compileroption,compilerswitch:string;
  ReplaceOption:boolean;
begin
  compileroption:=Trim(aOption);
  if (Length(compileroption)<3) then exit;
  if compileroption[1]='-' then
  begin
    if (compileroption[2] in ['d','k','u','I']) then
      compilerswitch:=Copy(compileroption,1,2)
    else
      compilerswitch:=Copy(compileroption,1,3);

    index:=StringListStartsWith(FCrossOpts,compilerswitch,0,True);

    // Check for duplicates
    ReplaceOption:=(index<>-1);

    if ReplaceOption then
    begin
      for compilerswitch in MULTIDEFCOMPILERSWITCHES do
      begin
        ReplaceOption:=(NOT (Pos(compilerswitch,compileroption)=1));
        if (NOT ReplaceOption) then break;
      end;
    end;

    if (ReplaceOption) then
      FCrossOpts.Strings[index]:=compileroption
    else
      FCrossOpts.Add(compileroption);
  end;

end;

procedure TCrossInstaller.ReplaceFPCCFGSnippet(aOldSnip,aNewSnip: string);
begin
  if Length(Trim(aOldSnip))=0 then exit;
  if (Pos(aOldSnip,FFPCCFGSnippet)>0) then
  begin
    FFPCCFGSnippet:=StringReplace(FFPCCFGSnippet,aOldSnip,aNewSnip,[rfIgnoreCase]);
    // Remove double line-endings, if any
    FFPCCFGSnippet:=StringReplace(FFPCCFGSnippet,LineEnding+LineEnding,LineEnding,[rfReplaceAll]);
  end;
end;

procedure TCrossInstaller.SearchLibraryInfo(found:boolean; const extrainfo:string='');
begin
  if found then
  begin
    ThreadLog(BeginSnippet+' '+CrossModuleName + ': Found correct library in directory '+FLibsPath, etInfo);
    //Infoln(CrossModuleName + ': Found correct library in directory '+FLibsPath, etInfo)
  end;
  if (NOT found) then
  begin
    //ThreadLog(CrossModuleName + ': Searched but did not find any library !!', etError);
    //Infoln(CrossModuleName + ': Searched but did not find any library !!', etError);
  end;
  //if Length(extrainfo)>0 then Infoln(CrossModuleName + ' libs : '+extrainfo, etInfo);
end;

procedure TCrossInstaller.SearchBinUtilsInfo(found:boolean; const extrainfo:string='');
begin
  if found then
  begin
    ThreadLog(BeginSnippet+' '+CrossModuleName + ': Found correct binary utilities in directory '+FBinUtilsPath, etInfo);
    //Infoln(CrossModuleName + ': Found correct binary utilities in directory '+FBinUtilsPath, etInfo)
  end;
  if (NOT found) then
  begin
    //ThreadLog(CrossModuleName + ': Searched but did not find any binary utilities !!', etError);
    //Infoln(CrossModuleName + ': Searched but did not find any binary utilities !!', etError);
  end;
  //if Length(extrainfo)>0 then Infoln(CrossModuleName + ' bins : '+extrainfo, etInfo);
end;

function TCrossInstaller.PerformLibraryPathMagic(out LibraryPath:string):boolean;
var
  aPath:TStringArray;
  aIndex:integer;
  aABI:TABI;
begin
  result:=false;

  LibraryPath:=FLibsPath;

  if (NOT (Self.TargetOS in SUBARCH_OS)) then exit;
  if (NOT (Self.TargetCPU in SUBARCH_CPU)) then exit;

  // Skip for some combo's until we have structured libs
  if (Self.TargetOS=TOS.embedded) AND (NOT (Self.TargetCPU in [TCPU.arm,TCPU.avr{,TCPU.aarch64}])) then exit;
  if (Self.TargetOS=TOS.ultibo) then exit;
  if (Self.TargetOS=TOS.freertos) AND (NOT (Self.TargetCPU in [TCPU.arm{,TCPU.avr,TCPU.aarch64}])) then exit;

  aPath:=FLibsPath.Split(DirectorySeparator);

  // Perform Subarch magic for CROSSLIBDIRNAME
  if (FSubArch<>TSUBARCH.saNone) then
  begin
    aIndex:=StringsSame(aPath,SubArchName);
    if (aIndex<>-1) then
    begin
      aPath[aIndex]:=FPC_SUBARCH_MAGIC;
      result:=true;
    end;
  end;

  // Perform ABI magic for CROSSLIBDIRNAME
  aIndex:=StringsSame(aPath,RegisterName);
  if (aIndex<>-1) then
  begin
    for aABI in TABI do
    begin
      if aABI=TABI.default then continue;
      aIndex:=StringsSame(aPath,GetABI(aABI));
      if (aIndex<>-1) then
      begin
        aPath[aIndex]:=FPC_ABI_MAGIC;
        result:=true;
        break;
      end;
    end;
  end;

  if result then LibraryPath:=ConcatPaths(aPath);
end;

function TCrossInstaller.SearchLibrary(Directory, LookFor: string): boolean;
begin
  result:=SearchUtil(Directory, LookFor, true);
  if NOT result then
  begin
    if LookFor=LIBCFILENAME then result:=SearchUtil(Directory, LIBCFILENAME+'.6', true);
  end;
  if NOT result then
  begin
    if LookFor=LIBCFILENAME then result:=SearchUtil(Directory, LIBCFILENAME+'.7', true);
  end;
end;

function TCrossInstaller.SimpleSearchLibrary(BasePath,DirName: string; const LookFor:string): boolean;
begin
  result:=FPCUPToolsSearch(BasePath,DirName,true,LookFor);
  if NOT result then
  begin
    if LookFor=LIBCFILENAME then result:=FPCUPToolsSearch(BasePath,DirName,true,LIBCFILENAME+'.6');
  end;
  if NOT result then
  begin
    if LookFor=LIBCFILENAME then result:=FPCUPToolsSearch(BasePath,DirName,true,LIBCFILENAME+'.7');
  end;
end;

function TCrossInstaller.SearchBinUtil(Directory, LookFor: string): boolean;
begin
  result:=SearchUtil(Directory, LookFor, false);
end;

function TCrossInstaller.SimpleSearchBinUtil(BasePath,DirName: string; const LookFor:string): boolean;
begin
  result:=FPCUPToolsSearch(BasePath,DirName,false,LookFor);
end;

function TCrossInstaller.SearchUtil(Directory, LookFor: string; LibsOrBins:boolean): boolean;
var
  sd:string;
  info:string;
begin
  sd:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Directory));

  if LibsOrBins then
  begin
    FLibsPath:=sd;
    info:='Cross-library: ';
  end
  else
  begin
    FBinUtilsPath:=sd;
    info:='Cross-binutil(s): ';
  end;

  if Length(LookFor)=0 then
  begin
    result:=DirectoryExists(sd);
    info:=info+'looking for directory ['+sd+'].';
  end
  else
  begin
    result:=FileExists(IncludeTrailingPathDelimiter(sd)+LookFor);
    info:=info+'looking for file ['+IncludeTrailingPathDelimiter(sd)+LookFor+'].';
  end;

  {$ifdef DEBUGGG}
  if (NOT result) then
    ThreadLog('Toolsearch failure. '+info,etDebug)
  else
    ThreadLog('Toolsearch success !!. '+info,etDebug);
  {$endif}
end;


function TCrossInstaller.FPCUPToolsSearch(BasePath,DirName: string; LibsOrBins:boolean; const LookFor:string): boolean;
var
  sd:string;
begin
  result:=false;

  if SearchModeUsed=TSearchSetting.ssCustom then exit;

  // first search local paths based on libraries provided for or adviced by fpc itself
  sd:=IncludeTrailingPathDelimiter(BasePath);
  if LibsOrBins
     then sd:=sd+CROSSLIBDIRNAME
     else sd:=sd+CROSSBINDIRNAME;
  if Length(DirName)>0 then sd:=sd+DirectorySeparator+DirName;
  sd:=SafeExpandFileName(sd);
  result:=SearchUtil(sd, LookFor, LibsOrBins);
  if ((NOT result) AND (NOT LibsOrBins)) then
  begin
    sd:=sd+DirectorySeparator+CROSSBINDIRNAME;
    result:=SearchUtil(sd, LookFor, LibsOrBins);
  end;

  if not result then
  begin
    sd:=IncludeTrailingPathDelimiter(BasePath)+CROSSDIRNAME+DirectorySeparator;
    if LibsOrBins
       then sd:=sd+CROSSLIBDIRNAME
       else sd:=sd+CROSSBINDIRNAME;
    if Length(DirName)>0 then sd:=sd+DirectorySeparator+DirName;
    sd:=SafeExpandFileName(sd);
    result:=SearchUtil(sd, LookFor, LibsOrBins);
    if ((NOT result) AND (NOT LibsOrBins)) then
    begin
      sd:=sd+DirectorySeparator+'bin';
      result:=SearchUtil(sd, LookFor, LibsOrBins);
    end;
  end;

  if not result then
  begin
    sd:=SafeGetApplicationPath+CROSSDIRNAME+DirectorySeparator;
    if LibsOrBins
       then sd:=sd+CROSSLIBDIRNAME
       else sd:=sd+CROSSBINDIRNAME;
    if (Length(DirName)>0) then sd:=sd+DirectorySeparator+DirName;
    sd:=SafeExpandFileName(sd);
    result:=SearchUtil(sd, LookFor, LibsOrBins);
    if ((NOT result) AND (NOT LibsOrBins)) then
    begin
      sd:=sd+DirectorySeparator+CROSSBINDIRNAME;
      result:=SearchUtil(sd, LookFor, LibsOrBins);
    end;
  end;

  {$IFDEF UNIX}
  if (SearchModeUsed=TSearchSetting.ssAuto) then
  begin
    if LibsOrBins
       then sd:=CROSSLIBDIRNAME
       else sd:=CROSSBINDIRNAME;

    if not result then
      if Length(DirName)>0 then result:=SearchUtil('/usr/local/'+sd+'/'+DirName,
        LookFor, LibsOrBins);

    // extend search, but not for libraries !!
    if (NOT LibsOrBins) then
    begin
      if not result then
        result:=SearchUtil('/usr/local/'+sd,
          LookFor, LibsOrBins);

      if not result then
        result:=SearchUtil('/usr/'+sd,
          LookFor, LibsOrBins);

      if not result then
        result:=SearchUtil('/'+sd,
          LookFor, LibsOrBins);
    end;

  end;
  {$ENDIF}

end;

{$ifndef FPCONLY}
function TCrossInstaller.GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;
begin
  result:=true;
end;
{$endif}

function TCrossInstaller.GetLibs(Basepath:string):boolean;
var
  BinDir,LibDir:string;
begin
  result:=FLibsFound;
  if (NOT result) then
  begin
    GetCrossToolsDir(TargetCPU,TargetOS,FMUSL,FSolarisOI,FLinuxLegacy,BinDir,LibDir);
    FUtilsDirectoryID:=LibDir;
  end;
end;

function TCrossInstaller.GetBinUtils(Basepath: string): boolean;
var
  i:integer;
  BinDir,LibDir:string;
begin
  result:=FBinsFound;

  if (NOT result) then
  begin
    GetCrossToolsDir(TargetCPU,TargetOS,FMUSL,FSolarisOI,FLinuxLegacy,BinDir,LibDir);
    FUtilsDirectoryID:=BinDir;
  end;

  // only add options once !
  if FCrossOptsAdded then exit;

  // Add user-selected CROSSOPT to fpc.cfg snippet
  // Descendents can add more fpc.cfg snippets but shouldn't remove what the user chose
  if (SubArch<>TSUBARCH.saNone) then
    AddFPCCFGSnippet('#IFDEF CPU'+UpperCase(Self.SubArchName));
  for i:=0 to FCrossOpts.Count-1 do
  begin
    if ((SubArch<>TSUBARCH.saNone) AND AnsiContainsText(FCrossOpts[i],'-Cp'+Self.SubArchName)) then continue;
    AddFPCCFGSnippet(FCrossOpts[i]);
  end;
  if (SubArch<>TSUBARCH.saNone) then
    AddFPCCFGSnippet('#ENDIF CPU'+UpperCase(Self.SubArchName));

  FCrossOptsAdded:=true;
end;

procedure TCrossInstaller.SetFPCVersion(aVersion: string);
begin
  FFPCVersion:=aVersion;
end;

procedure TCrossInstaller.SetCrossOpt(CrossOpts: string);
// A bit rough-and-ready but hopefully there won't be too many quoting etc problems
var
  Parser: TStringList;
begin
  Parser:=TStringList.Create;
  try
    Parser.Delimiter:=' ';
    Parser.QuoteChar:=''''; //single '. Assume entire CROSSOPT argument is surround by double quotes; indifividual parameters by single.
    Parser.StrictDelimiter:=false; //ignore quoting characters
    Parser.DelimitedText:=CrossOpts;
    FCrossOpts.Clear;
    FCrossOpts.AddStrings(Parser);
  finally
    Parser.Free;
  end;
end;

procedure TCrossInstaller.SetSubArch(SubArch: TSUBARCH);
begin
  FSubArch:=SubArch;
end;

procedure TCrossInstaller.SetABI(ABI: TABI);
begin
  FABI:=ABI;
end;

procedure TCrossInstaller.ShowInfo(info: string = ''; Level: TEventType = etInfo);
begin
  //if (Length(info)>0) then ThreadLog(CrossModuleName+': '+info,Level)
end;

procedure TCrossInstaller.Reset;
begin
  FFPCCFGSnippet:='';
  FLibsFound:=false;
  FBinsFound:=false;
  FCrossOptsAdded:=false;
  FCrossOpts.Clear;
  FSubArch:=TSUBARCH.saNone;
  FABI:=TABI.default;

  FCrossModuleNamePrefix:='T'+GetSourceCPUOS;

  FRegisterName:=TargetCPUName+'-'+TargetOSName;
  FUtilsDirectoryID:=FRegisterName;

  case TargetOS of
    TOS.android: FBinUtilsPrefix:=TargetCPUName+'-linux-'+TargetOSName+'-'; //standard eg in Android NDK 9
    TOS.darwin,TOS.ios:FBinUtilsPrefix:=TargetCPUName+'-apple-'+TargetOSName+'-'; //standard Apple triplet
  else
    FBinUtilsPrefix:=TargetCPUName+'-'+TargetOSName+'-'; //normal binutils prefix name
  end;

  case TargetCPU of
    TCPU.jvm: FBinUtilsPrefix:='';
  end;

  FBinutilsPathInPath:=false; //don't add binutils directory to path when cross compiling

  FBinUtilsPath:='Error: cross compiler extension must set FBinUtilsPath: the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.';
  FLibsPath:='Error: cross compiler extension must set FLibsPath: path for target environment libraries';
end;

constructor TCrossInstaller.Create;
begin
  FCrossOpts:=TStringList.Create;

  FFPCVersion:='Error: FPC version unknown to cross-compiler';

  FTargetCPU:=TCPU.cpuNone;
  FTargetOS:=TOS.osNone;

  FBinUtilsPrefix:='Error: cross compiler extension must set FBinUtilsPrefix: can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it';
  FCrossModuleNamePrefix:='TAny';
  FUtilsDirectoryID:='none';
  FRegisterName:='unknown';
end;

destructor TCrossInstaller.Destroy;
begin
  FCrossOpts.Free;
  inherited Destroy;
end;

{$ifdef LCL}
procedure InitDefaultCrossSettings;
const
  ARMLESSOS: set of TOS = [TOS.win32..TOS.atari] - [TOS.android,TOS.win32,TOS.win64,TOS.iphonesim,TOS.java,TOS.msdos,TOS.solaris,TOS.morphos,TOS.aros,TOS.amiga,TOS.go32v2,TOS.wasi,TOS.atari];
var
  CPU:TCPU;
  OS:TOS;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  aCrossOptionSetting:string;
  aARMABISetting:TARMARCH;
begin
  for OS := Low(TOS) to High(TOS) do
  begin
    if OS=TOS.osNone then continue;

    for CPU := Low(TCPU) to High(TCPU) do
    begin
      if CPU=TCPU.cpuNone then continue;

      SetSelectedSubArch(CPU,OS,TSUBARCH.saNone);

      Subarchs:=GetSubarchs(CPU,OS);

      for SUBARCH in Subarchs do
      begin
        CrossUtils[CPU,OS,SUBARCH].Setting:=DEFAULTSEARCHSETTING;
        CrossUtils[CPU,OS,SUBARCH].LibDir:='';
        CrossUtils[CPU,OS,SUBARCH].BinDir:='';
        CrossUtils[CPU,OS,SUBARCH].Compiler:='';

        aCrossOptionSetting:='';
        aARMABISetting:=DEFAULTARMARCH;

        // Set defaults for CrossBuildOptions

        //arm (unix, non-android) predefined settings
        if (
          (CPU=TCPU.arm)
          AND (NOT (OS in SUBARCH_OS))
          AND (OS in ARMLESSOS)
          ) then
        begin
          // default: armhf
          aARMABISetting:=TARMARCH.armhf;

          if (OS=TOS.wince) then
          begin
          end
          else
          if ((OS=TOS.darwin) OR (OS=TOS.ios)) then
          begin
            aCrossOptionSetting:='-CfVFPV3 -CaEABI ';
          end
          else
          begin
            aCrossOptionSetting:='-CfVFPV3 -OoFASTMATH -CaEABIHF ';
          end;
        end;

        //android predefined settings
        if (OS=TOS.android) then
        begin
          if (CPU=TCPU.i386) then
          begin
            aCrossOptionSetting:='-CfSSSE3 ';
          end;
          if (CPU=TCPU.x86_64) then
          begin
            aCrossOptionSetting:='-CfSSE42 ';
          end;
          if (CPU=TCPU.arm) then
          begin
            // default: armhf
            // don't worry: this -dFPC_ARMHF option will still build a normal ppcrossarm (armel) for Android
            // adding this option will allow ppcrossarm compiler to generate ARMHF when needed
            // but I stand corrected if this assumption is wrong
            aARMABISetting:=TARMARCH.armhf;

            // Use hard floats, using armeabi-v7a Android ABI.
            // Note: do not use -CaEABIHF on Android, to not use
            // armeabi-v7a-hard ABI. Reasons:
            // - armeabi-v7a-hard ABI is not adviced anymore by Google,
            //   see "ARM Hard Float ABI Removal" on
            //   https://android.googlesource.com/platform/ndk/+/353e653824b79c43b948429870d0abeedebde386/docs/HardFloatAbi.md
            //   and
            //   https://developer.android.com/ndk/guides/abis#v7a
            // - it prevents calling functions from libraries not using
            //   armeabi-v7a-hard ABI (but only using armeabi-v7a) like
            //   http://repo.or.cz/openal-soft/android.git or
            //   https://github.com/michaliskambi/tremolo-android .
            aCrossOptionSetting:='-CfVFPV3 -OoFASTMATH -CaEABI ';
          end;
        end;

        //freertos and embedded predefined settings
        if (OS in [TOS.embedded,TOS.freertos]) then
        begin

          if ((CPU=TCPU.avr) AND (OS=TOS.embedded)) then
          begin
            // for Uno (ATMega328P) use avr5
            // for Mega (ATMega2560) use avr6
          end;

          if (CPU=TCPU.xtensa) then
          begin
            if SUBARCH=TSubarch.lx6 then
              aCrossOptionSetting:='-Cfhard ';
          end;

          if (CPU=TCPU.arm) then
          begin
            aARMABISetting:=TARMARCH.armhf;
            if (SUBARCH<>TSubarch.armv7em) then
              aCrossOptionSetting:='-CaEABI '
            else
              aCrossOptionSetting:='-CfFPV4_SP_D16 -OoFASTMATH -CaEABIHF '
          end;

          if ((CPU=TCPU.mipsel) AND (OS=TOS.embedded)) then
          begin
          end;

        end;

        //ultibo predefined settings
        if (OS=TOS.ultibo) then
        begin
          if (CPU=TCPU.arm) then
          begin
            // Always hardfloat !!
            aARMABISetting:=TARMARCH.armhf;

            if SUBARCH=TSubarch.armv6 then
              aCrossOptionSetting:='-CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH ';
            if SUBARCH=TSubarch.armv7a then
              aCrossOptionSetting:='-CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH ';
          end;
          if (CPU=TCPU.aarch64) then
          begin
            if SUBARCH=TSubarch.armv8 then
              aCrossOptionSetting:='-CfVFP -OoFASTMATH ';
          end;
        end;

        //msdos predefined settings
        if (OS=TOS.msdos) then
        begin
          if (CPU=TCPU.i8086) then
          begin
            {$IFDEF DARWIN}
            aCrossOptionSetting:='-WmLarge ';
            {$ELSE}
            aCrossOptionSetting:='-WmMedium ';
            {$ENDIF DARWIN}
          end;
        end;

        //atari predefined settings
        if (OS=TOS.atari) then
        begin
          if (CPU=TCPU.m68k) then
          begin
            aCrossOptionSetting:='-XV -Avasm -Cp68000 ';
          end;
        end;

        //ppc64 predefined settings
        if (CPU=TCPU.powerpc64) then
        begin
          if ((OS=TOS.linux)) then
          begin
            // for now, little endian only on Linux (IBM CPU's) !!
            aCrossOptionSetting:='-Cb- -Caelfv2 ';
          end;
        end;

        //mips[el] predefined settings
        if (CPU in [TCPU.mips,TCPU.mipsel]) then
        begin
          if ((OS=TOS.linux)) then
          begin
            aCrossOptionSetting:='-CfSOFT ';
          end;
        end;

        //freebsd predefined settings
        if (OS=TOS.freebsd) then
        begin
          //This is already done in the FPC installer itself.
          //To be checked if that is the right choice.
          //aCrossOptionSetting:='-d'+DEFINE_FPC_USE_LIBC+' ';
        end;

        //Store predefined setting.
        CrossUtils[CPU,OS,SUBARCH].CrossBuildOptions:=aCrossOptionSetting;

        // Set defaults for ARM ABI
        if CPU=TCPU.arm then
        begin
          CrossUtils[CPU,OS,SUBARCH].CrossARMArch:=aARMABISetting;
        end;

      end;
    end;
  end;
end;

initialization
  InitDefaultCrossSettings;
{$endif LCL}

finalization
  if assigned(CrossInstallers) then
    CrossInstallers.Destroy;

end.

