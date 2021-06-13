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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ErrorNotFound='An error occurred getting cross compiling binutils/libraries.'+LineEnding+
    'todo: specify what exactly is missing';

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
  DEFAULTARMCPU = 'ARMV7A';

  LIBCNAME='libc.so';

  CROSSPATH      = 'cross';
  CROSSBINPATH   = CROSSPATH+DirectorySeparator+'bin';
  CROSSLIBPATH   = CROSSPATH+DirectorySeparator+'lib';

  LDSEARCHFILE  = 'ld';
  SEARCHFILE    = 'as';

type
  TCPU      = (cpuNone,i386,x86_64,arm,aarch64,powerpc,powerpc64,mips,mipsel,avr,jvm,i8086,sparc,sparc64,riscv32,riscv64,m68k,xtensa,wasm32);
  TOS       = (osNone,win32,win64,linux,android,darwin,freebsd,openbsd,aix,wince,iphonesim,embedded,java,msdos,haiku,solaris,dragonfly,netbsd,morphos,aros,amiga,go32v2,freertos,ios,ultibo,wasi);
  TSUBARCH  = (saNone,armv4,armv4t,armv6,armv6m,armv7a,armv7em,armv7m,avr1,avr2,avr25,avr35,avr4,avr5,avr51,avr6,avrtiny,avrxmega3,pic32mx,rv32imac,rv32ima,rv32im,rv32i,rv64imac,rv64ima,rv64im,rv64i,lx6,lx106);
  //TABI      = (default,sysv,aix,darwin,elfv2,eabi,armeb,eabihf,oldwin32gnu,aarch64ios,riscvhf,linux386_sysv,windowed,call0);
  TABI      = (default,eabi,eabihf,aarch64ios,riscvhf,windowed,call0);
  TARMARCH  = (none,armel,armeb,armhf);

  TSUBARCHS = set of TSUBARCH;
  TABIS     = set of TABI;

const
  SUBARCH_OS         = [TOS.embedded,TOS.freertos,TOS.ultibo];
  SUBARCH_CPU        = [TCPU.arm,TCPU.avr,TCPU.mipsel,TCPU.riscv32,TCPU.riscv64,TCPU.xtensa];
  SUBARCH_ARM        = [TSUBARCH.armv4..TSUBARCH.armv7m];
  SUBARCH_AVR        = [TSUBARCH.avr1..TSUBARCH.avrxmega3];
  SUBARCH_MIPSEL     = [TSUBARCH.pic32mx];
  SUBARCH_RISCV32    = [TSUBARCH.rv32imac..TSUBARCH.rv32i];
  SUBARCH_RISCV64    = [TSUBARCH.rv64imac..TSUBARCH.rv64i];
  SUBARCH_XTENSA     = [TSUBARCH.lx6..TSUBARCH.lx106];

  ABI_ARM            = [TABI.default,TABI.eabi,TABI.eabihf];
  ABI_XTENSA         = [TABI.default,TABI.windowed,TABI.call0];
  ABI_RISCV64        = [TABI.default,TABI.riscvhf];

type
  TSearchSetting = (ssUp,ssAuto,ssCustom);

const
  ARMArchFPCStr : array[TARMARCH] of string=(
    '','-dFPC_ARMEL','-dFPC_ARMEB','-dFPC_ARMHF'
  );
  FPCUP_AUTO_MAGIC  = 'FPCUP_AUTO';

  FPC_SUBARCH_MAGIC = '$FPCSUBARCH';
  FPC_ABI_MAGIC     = '$FPCABI';

type
  TCrossUtil = record
    Setting:TSearchSetting;
    LibDir:string;
    BinDir:string;
    CrossBuildOptions:string;
    CrossARMArch:TARMARCH;
    Compiler:string;
    Available:boolean;
  end;

  TCrossUtils = array[TCPU,TOS,TSUBARCH] of TCrossUtil;

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  private
    function GetCrossModuleName:string;
    function GetTargetCPUName:string;
    function GetTargetOSName:string;
    function GetSubarchName:string;
    function GetABIName:string;
  protected
    FFPCVersion: string;
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinutilsPathInPath: boolean;
    FBinUtilsDirectoryID: string; //where to find the binutils themselves
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
    function PerformLibraryPathMagic:boolean;
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
    function GetLibs(Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download cross compile binutils or check for their existence
    function GetBinUtils(Basepath:string):boolean;virtual;
    {$ifndef FPCONLY}
    // In your descendent, implement this function when needed: you can download libraries or check for their existence for Lazarus LCL cross compile libs:
    // Note: the libraries should be presumably under the basepath using the Lazarus naming convention??
    function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;virtual;
    {$endif}
    procedure AddFPCCFGSnippet(aSnip: string);
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
    property LibsPath:string read FLibsPath;
    // Path where binutils used for target systems are. May be empty if not used.
    property BinUtilsPath:string read FBinUtilsPath;
    // Indicates if binutils directory is used as the last entry in PATH when cross compiling.
    // Can be useful if make scripts forget to include the complete path to the binutils path
    // (e.g. some versions of the DOS crosscompiler)
    property BinUtilsPathInPath: boolean read FBinutilsPathInPath;
    // Prefix used before executable names for binutils (e.g. before as.exe). May be empty.
    property BinUtilsPrefix:string read FBinUtilsPrefix;
    property DirName:string read FBinUtilsDirectoryID;
    property TargetCPU:TCPU read FTargetCPU;
    property TargetOS:TOS read FTargetOS;
    property SubArch:TSUBARCH read FSubArch;
    property ABI:TABI read FABI;
    property TargetCPUName: string read GetTargetCPUName;
    property TargetOSName: string read GetTargetOSName;
    property SubArchName:string read GetSubarchName;
    property ABIName:string read GetABIName;
    property RegisterName:string read FRegisterName;
    property SolarisOI: boolean write FSolarisOI;
    property MUSL: boolean write FMUSL;

    constructor Create;
    destructor Destroy; override;
  end;

function GetCPU(aCPU:TCPU):string;
function GetTCPU(aCPU:string):TCPU;
function GetOS(aOS:TOS):string;
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
{$ifdef LCL}
function  GetSelectedSubArch(aCPU:TCPU;aOS:TOS):TSUBARCH;
procedure SetSelectedSubArch(aCPU:TCPU;aOS:TOS;aSUBARCH:TSUBARCH);
{$endif LCL}

procedure RegisterCrossCompiler(Platform:string;aCrossInstaller:TCrossInstaller);
function GetExeExt: string;

var
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

function GetCPU(aCPU:TCPU):string;
begin
  if (aCPU<Low(TCPU)) OR (aCPU>High(TCPU)) then
    raise Exception.Create('Invalid CPU for GetCPU.');
  result:=GetEnumNameSimple(TypeInfo(TCPU),Ord(aCPU));
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
  if (aOS<Low(TOS)) OR (aOS>High(TOS)) then
    raise Exception.Create('Invalid OS for GetOS.');
  result:=GetEnumNameSimple(TypeInfo(TOS),Ord(aOS));
end;

function GetTOS(aOS:string):TOS;
var
  xOS:TOS;
begin
  result:=TOS.osNone;
  if length(aOS)>0 then
  begin
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
      TCPU.avr:      if (aOS=TOS.embedded) then result:=SUBARCH_AVR;
      TCPU.mipsel:   if (aOS=TOS.embedded) then result:=SUBARCH_MIPSEL;
      TCPU.riscv32:  if (aOS=TOS.embedded) then result:=SUBARCH_RISCV32;
      TCPU.riscv64:  if (aOS=TOS.embedded) then result:=SUBARCH_RISCV64;
      TCPU.xtensa:   if (aOS<>TOS.ultibo) then result:=SUBARCH_XTENSA;
    end;
    // Limit some special targets
    if (aOS=TOS.ultibo) then result:=[TSUBARCH.armv6,TSUBARCH.armv7a];
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
    result:=TARMARCH.none
  else
  if aARMArch='default' then
    result:=TARMARCH.none
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

function GetExeExt: string;
begin
  {$IFDEF WINDOWS}
  Result:='.exe';
  {$ELSE}
  Result:='';
  {$ENDIF}
end;

{ TCrossInstaller }
procedure RegisterCrossCompiler(Platform:string;aCrossInstaller:TCrossInstaller);
begin
  if not assigned(CrossInstallers) then
    CrossInstallers:=TStringList.Create;
  CrossInstallers.AddObject(Platform,TObject(aCrossInstaller));
end;

function TCrossInstaller.GetCrossModuleName:string;
begin
  result:=FCrossModuleNamePrefix+'_'+GetTargetOSName+'-'+GetTargetCPUName;
end;

function TCrossInstaller.GetTargetCPUName:string;
begin
  result:=GetCPU(TargetCPU);
end;

function TCrossInstaller.GetTargetOSName:string;
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

procedure TCrossInstaller.AddFPCCFGSnippet(aSnip: string);
var
  aSnippd:string;
begin
  if Length(Trim(aSnip))=0 then exit;

  aSnippd:=aSnip;

  aSnippd:=StringReplace(aSnippd,'#IFDEF ','#IFDEF_',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#ENDIF ','#ENDIF_',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,' ',LineEnding,[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#IFDEF_','#IFDEF ',[rfReplaceAll]);
  aSnippd:=StringReplace(aSnippd,'#ENDIF_','#ENDIF ',[rfReplaceAll]);

  // Check for duplicates
  if (Pos('#IFDEF',aSnippd)=0) AND (Pos('#ENDIF',aSnippd)=0) then
  begin
    if (Pos(aSnippd,FFPCCFGSnippet)>0) then
      exit;
  end;

  if Length(FPCCFGSnippet)>0 then
  begin
    if RPos(LineEnding,FFPCCFGSnippet)<Length(FFPCCFGSnippet) then FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding;
    FFPCCFGSnippet:=FFPCCFGSnippet+aSnippd;
  end
  else FFPCCFGSnippet:=aSnippd;

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
    ThreadLog('fpcupdeluxe: '+CrossModuleName + ': Found correct library in directory '+FLibsPath, etInfo);
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
    ThreadLog('fpcupdeluxe: '+CrossModuleName + ': Found correct binary utilities in directory '+FBinUtilsPath, etInfo);
    //Infoln(CrossModuleName + ': Found correct binary utilities in directory '+FBinUtilsPath, etInfo)
  end;
  if (NOT found) then
  begin
    //ThreadLog(CrossModuleName + ': Searched but did not find any binary utilities !!', etError);
    //Infoln(CrossModuleName + ': Searched but did not find any binary utilities !!', etError);
  end;
  //if Length(extrainfo)>0 then Infoln(CrossModuleName + ' bins : '+extrainfo, etInfo);
end;

function TCrossInstaller.PerformLibraryPathMagic:boolean;
var
  aPath:TStringArray;
  aIndex:integer;
  aABI:TABI;
begin
  result:=false;

  // Skip for some combo's until we have structured libs
  if (Self.TargetOS=TOS.embedded) AND (Self.TargetCPU<>TCPU.arm) then exit;
  if (Self.TargetOS=TOS.ultibo) then exit;
  if (Self.TargetOS=TOS.freertos) AND (Self.TargetCPU<>TCPU.arm) then exit;

  aPath:=FLibsPath.Split(DirectorySeparator);

  // Perform Subarch magic for libpath
  if (FSubArch<>TSUBARCH.saNone) then
  begin
    aIndex:=StringsSame(aPath,SubArchName);
    if (aIndex<>-1) then
    begin
      aPath[aIndex]:=FPC_SUBARCH_MAGIC;
      result:=true;
    end;
  end;

  // Perform ABI magic for libpath
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

  if result then FLibsPath:=ConcatPaths(aPath);
end;

function TCrossInstaller.SearchLibrary(Directory, LookFor: string): boolean;
begin
  result:=SearchUtil(Directory, LookFor, true);
  if NOT result then
  begin
    if LookFor=LIBCNAME then result:=SearchUtil(Directory, LIBCNAME+'.6', true);
  end;
  if NOT result then
  begin
    if LookFor=LIBCNAME then result:=SearchUtil(Directory, LIBCNAME+'.7', true);
  end;
end;

function TCrossInstaller.SimpleSearchLibrary(BasePath,DirName: string; const LookFor:string): boolean;
begin
  result:=FPCUPToolsSearch(BasePath,DirName,true,LookFor);
  if NOT result then
  begin
    if LookFor=LIBCNAME then result:=FPCUPToolsSearch(BasePath,DirName,true,LIBCNAME+'.6');
  end;
  if NOT result then
  begin
    if LookFor=LIBCNAME then result:=FPCUPToolsSearch(BasePath,DirName,true,LIBCNAME+'.7');
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
     then sd:=sd+'lib'
     else sd:=sd+'bin';
  if Length(DirName)>0 then sd:=sd+DirectorySeparator+DirName;
  sd:=SafeExpandFileName(sd);
  result:=SearchUtil(sd, LookFor, LibsOrBins);
  if ((NOT result) AND (NOT LibsOrBins)) then
  begin
    sd:=sd+DirectorySeparator+'bin';
    result:=SearchUtil(sd, LookFor, LibsOrBins);
  end;

  if not result then
  begin
    sd:=IncludeTrailingPathDelimiter(BasePath)+CROSSPATH+DirectorySeparator;
    if LibsOrBins
       then sd:=sd+'lib'
       else sd:=sd+'bin';
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
    sd:=SafeGetApplicationPath+CROSSPATH+DirectorySeparator;
    if LibsOrBins
       then sd:=sd+'lib'
       else sd:=sd+'bin';
    if (Length(DirName)>0) then sd:=sd+DirectorySeparator+DirName;
    sd:=SafeExpandFileName(sd);
    result:=SearchUtil(sd, LookFor, LibsOrBins);
    if ((NOT result) AND (NOT LibsOrBins)) then
    begin
      sd:=sd+DirectorySeparator+'bin';
      result:=SearchUtil(sd, LookFor, LibsOrBins);
    end;
  end;

  {$IFDEF UNIX}
  if (SearchModeUsed=TSearchSetting.ssAuto) then
  begin
    if LibsOrBins
       then sd:='lib'
       else sd:='bin';

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

function TCrossInstaller.GetBinUtils(Basepath: string): boolean;
var
  i:integer;
begin
  result:=FBinsFound;

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

  FRegisterName:=TargetCPUName+'-'+TargetOSName;
  FBinUtilsDirectoryID:=FRegisterName;

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

  FTargetCPU:=TCPU.cpuNone;
  FTargetOS:=TOS.osNone;

  FBinUtilsPrefix:='Error: cross compiler extension must set FBinUtilsPrefix: can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it';

  FCrossModuleNamePrefix:='TAny';

  FBinUtilsDirectoryID:='none';
end;

destructor TCrossInstaller.Destroy;
begin
  FCrossOpts.Free;
  inherited Destroy;
end;

{$ifdef LCL}
procedure InitDefaultCrossSettings;
var
  CPU:TCPU;
  OS:TOS;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  s1,s2:string;
  aCrossOptionSetting:string;
  aARMABISetting:TARMARCH;
begin
  for OS := Low(TOS) to High(TOS) do
  begin

    for CPU := Low(TCPU) to High(TCPU) do
    begin

      SetSelectedSubArch(CPU,OS,TSUBARCH.saNone);

      s1:=GetCPU(CPU)+'-'+GetOS(OS);

      Subarchs:=GetSubarchs(CPU,OS);

      for SUBARCH in Subarchs do
      begin
        if (SUBARCH<>TSUBARCH.saNone) then
          s2:=s1+'-'+GetSubarch(SUBARCH)
        else
          s2:=s1;

        CrossUtils[CPU,OS,SUBARCH].Setting:=TSearchSetting.ssUp;
        CrossUtils[CPU,OS,SUBARCH].LibDir:='';
        CrossUtils[CPU,OS,SUBARCH].BinDir:='';
        CrossUtils[CPU,OS,SUBARCH].Compiler:='';

        aCrossOptionSetting:='';
        aARMABISetting:=TARMARCH.none;

        // Set defaults for CrossBuildOptions

        //arm (unix, non-android) predefined settings
        if (
          (CPU=TCPU.arm)
          AND (NOT (OS in SUBARCH_OS))
          AND (NOT (OS in [TOS.android,TOS.win32,TOS.win64,TOS.iphonesim,TOS.java,TOS.msdos,TOS.solaris,TOS.morphos,TOS.aros,TOS.amiga,TOS.go32v2]))
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
            // - it prevents calling functions from libraries not using
            //   armeabi-v7a-hard ABI (but only using armeabi-v7a) like
            //   http://repo.or.cz/openal-soft/android.git or
            //   https://github.com/michaliskambi/tremolo-android .
            aCrossOptionSetting:='-CfVFPV3 -CaEABI ';
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
          //aCrossOptionSetting:='-dFPC_USE_LIBC ';
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

