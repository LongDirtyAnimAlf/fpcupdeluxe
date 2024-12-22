unit installerBase;
{
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
  Classes,
  {$ifndef FPCONLY}
  InterfaceBase,
  LCLPlatformDef,
  {$endif}
  SysUtils;

const
  ErrorNotFound='An error occurred getting cross compiling binutils/libraries.'+LineEnding+
    'todo: specify what exactly is missing';

  CrossWindowsSuggestion = 'Suggestion for cross binutils: the crossfpc binutils at https://gitlab.com/freepascal.org/fpc/binaries/-/tree/main/i386-win32.';

  MAXDARWINVERSION=24;
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
  TSUBARCH  = (saNone,armv4,armv4t,armv6,armv6m,armv7a,armv7em,armv7m,armv8,armv8a,avr1,avr2,avr25,avr35,avr4,avr5,avr51,avr6,avrtiny,avrxmega3,pic32mx,rv32ec,rv32e,rv32imac{,rv32ima,rv32im},rv32i,rv64imac{,rv64ima,rv64im},rv64i,lx6,lx106);
  //TABI      = (default,sysv,aix,darwin,elfv2,eabi,armeb,eabihf,oldwin32gnu,aarch64ios,riscvhf,linux386_sysv,windowed,call0);
  TABI      = (default,eabi,eabihf,aarch64ios,riscvhf,windowed,call0);
  TARMARCH  = (none,armel,armeb,armhf);

  TSUBARCHS = set of TSUBARCH;
  TABIS     = set of TABI;

  {$ifndef FPCONLY}
  LCL_TYPE  = TLCLPlatform;
  {$endif}

const
  LCL_OS             = [TOS.win32,TOS.win64,TOS.linux,TOS.darwin,TOS.freebsd,TOS.openbsd,TOS.aix,TOS.wince,TOS.haiku,TOS.solaris,TOS.dragonfly,TOS.netbsd,TOS.morphos,TOS.aros,TOS.amiga];
  WINDOWS_OS         = [TOS.win32,TOS.win64];
  BSD_OS             = [TOS.freebsd,TOS.netbsd,TOS.openbsd,TOS.dragonfly];
  SUBARCH_OS         = [TOS.embedded,TOS.freertos,TOS.ultibo];
  SUBARCH_CPU        = [TCPU.arm,TCPU.aarch64,TCPU.avr,TCPU.mipsel,TCPU.riscv32,TCPU.riscv64,TCPU.xtensa]; //for Ultibo added TCPU.aarch64
  SUBARCH_ARM        = [TSUBARCH.armv4..TSUBARCH.armv7m];
  SUBARCH_AARCH64    = [TSUBARCH.armv8a];
  SUBARCH_AVR        = [TSUBARCH.avr1..TSUBARCH.avrxmega3];
  SUBARCH_MIPSEL     = [TSUBARCH.pic32mx];
  SUBARCH_RISCV32    = [TSUBARCH.rv32ec..TSUBARCH.rv32i];
  SUBARCH_RISCV64    = [TSUBARCH.rv64imac..TSUBARCH.rv64i];
  SUBARCH_XTENSA     = [TSUBARCH.lx6..TSUBARCH.lx106];
  SUBARCH_ULTIBO     = [TSUBARCH.armv6,TSUBARCH.armv7a,TSUBARCH.armv8];

  ABI_ARM            = [TABI.default,TABI.eabi,TABI.eabihf];
  ABI_XTENSA         = [TABI.default,TABI.windowed,TABI.call0];
  ABI_RISCV64        = [TABI.default,TABI.riscvhf];

  CPUADDRSIZE_64     = [TCPU.aarch64,TCPU.powerpc64,TCPU.sparc64,TCPU.x86_64,TCPU.loongarch64,TCPU.riscv64{,TCPU.ia64]}];
  CPUADDRSIZE_32     = [TCPU.i386,TCPU.arm,TCPU.powerpc,TCPU.mips,TCPU.mipsel,TCPU.sparc,TCPU.m68k,TCPU.xtensa,TCPU.wasm32,TCPU.riscv32];

  LEGACYLIBS             :array[0..4] of string = ('libanl.so','libdl.so','librt.so','libresolv.so','libpthread.so');
  LEGACYLIBSVERSIONED    :array[0..4] of string = ('libanl.so.1','libdl.so.2','librt.so.1','libresolv.so.2','libpthread.so.0');
  LEGACYCPU              = [TCPU.i386,TCPU.x86_64,TCPU.arm,TCPU.aarch64,TCPU.powerpc,TCPU.powerpc64];
  TODOCPU                = [TCPU.sparc,TCPU.sparc64,TCPU.m68k];

type
  TSearchSetting = (ssUp,ssAuto,ssCustom);

const
  ppcSuffix : array[TCPU] of string=(
    'none','386','x64','arm','a64','ppc','ppc64', 'mips', 'mipsel','avr','jvm','8086','sparc','sparc64','rv32','rv64','68k','xtensa','wasm32','loongarch64'
  );

  LINUXTYPE : array[boolean] of ansistring = ('gnu','musl');

  DEFINE_FPC_LIBC   = 'FPC_USE_LIBC';
  DEFINE_FPC_DOTTED     = 'FPC_DOTTEDUNITS';

  ARMArchFPCStr : array[TARMARCH] of string = (
    '','-dFPC_ARMEL','-dFPC_ARMEB','-dFPC_ARMHF'
  );
  FPCUP_AUTO_MAGIC      = 'FPCUP_AUTO';

  FPC_TARGET_MAGIC      = '$fpctarget';
  FPC_SUBARCH_MAGIC     = '$fpcsubarch';
  FPC_ABI_MAGIC         = '$fpcabi';

  DEFAULTSEARCHSETTING  = TSearchSetting.ssUp;
  DEFAULTARMARCH        = TARMARCH.none;

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
function GetExeExt(const aOS:TOS=TOS.osNone): string;
{$ifndef FPCONLY}
function GetLCLName(LCLType:LCL_TYPE):string;
function GetLCLType(LCLName:string):LCL_TYPE;
{$endif}
procedure GetCrossToolsDir(const CrossCPU_Target:TCPU;const CrossOS_Target:TOS; const MUSL,SolarisOI:boolean; out BinPath,LibPath:string);

{$ifdef LCL}
var
  SUBARCHStore:array[TCPU,TOS] of TSUBARCH;
{$endif LCL}

implementation

uses
  StrUtils,
  processutils,// for ThreadLog
  fpcuputil;

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
                  if aCPU=TCPU.riscv32 then result:='riscv32' else
                    if aCPU=TCPU.riscv64 then result:='riscv64' else
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

{$ifndef FPCONLY}
function GetLCLName(LCLType:LCL_TYPE):string;
begin
  result:=LCLPlatformDirNames[LCLType];
end;


function GetLCLType(LCLName:string):LCL_TYPE;
var
  LCLType:LCL_TYPE;
begin
  result:=BuildLCLWidgetType;
  for LCLType in TLCLPlatforms do
  begin
    if (LCLName=GetLCLName(LCLType)) then
    begin
      result:=LCLType;
      break;
    end;
  end;
end;
{$endif}

procedure GetCrossToolsDir(const CrossCPU_Target:TCPU;const CrossOS_Target:TOS; const MUSL,SolarisOI:boolean; out BinPath,LibPath:string);
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

end.

