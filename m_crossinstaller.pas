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
  NDKVERSIONNAMES:array[0..21] of string = ('7','7b','7c','8','8b','8c','8d','8e','9','9b','9c','9d','10','10b','10c','10d','10e','11','11b','11c','12','12b');
  //PLATFORMVERSIONSNUMBERS:array[0..13] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22); //23 does not yet work due to text allocations
  PLATFORMVERSIONSNUMBERS:array[0..17] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26);
  {$ifdef unix}
  UnixBinDirs :array[0..2] of string = ('/usr/local/bin','/usr/bin','/bin');
  UnixLibDirs :array[0..2] of string = ('/usr/local/lib','/usr/lib','/lib');
  {$endif}
  DEFAULTARMCPU = 'ARMV7A';

  NEWPASCALGITREPO='https://github.com/LongDirtyAnimAlf';
  FPCUPGITREPO=NEWPASCALGITREPO+'/fpcupdeluxe';

  BOOTSTRAPPERVERSION='bootstrappers_v1.0';
  FPCUPGITREPOBOOTSTRAPPER=FPCUPGITREPO+'/releases/download/'+BOOTSTRAPPERVERSION;
  FPCUPGITREPOAPI='https://api.github.com/repos/LongDirtyAnimAlf/fpcupdeluxe/releases';
  FPCUPGITREPOBOOTSTRAPPERAPI=FPCUPGITREPOAPI+'/tags/'+BOOTSTRAPPERVERSION;

  SOURCEPATCHES='patches_v1.0';
  FPCUPGITREPOSOURCEPATCHESAPI=FPCUPGITREPOAPI+'/tags/'+SOURCEPATCHES;

  FPCUPPRIVATEGITREPO='https://www.consulab.nl/git/Alfred/FPCbootstrappers/raw/master';

  FPCUP_ACKNOWLEDGE='acknowledgement_fpcup.txt';

  LIBCNAME='libc.so';

  {
  LINKERNAMEBASE='ld-linux.so.2';
  LINKERNAMEARM='ld-linux-arm.so.2';
  LINKERNAMEARMHF='ld-linux-armhf.so.2';
  LINKERNAMECPUX64='ld-linux-x86-64.so.2';
  LINKERNAMECPUAARCH64='ld-linux-aarch64.so.2';
  }

  CROSSPATH      = 'cross';
  CROSSBINPATH   = CROSSPATH+DirectorySeparator+'bin';
  CROSSLIBPATH   = CROSSPATH+DirectorySeparator+'lib';

  LDSEARCHFILE='ld';
  SEARCHFILE='as';

type
  TCPU = (cpuNone,i386,x86_64,arm,aarch64,powerpc,powerpc64,mips,mipsel,avr,jvm,i8086,sparc,sparc64,riscv32,riscv64,m68k,xtensa);
  TOS  = (osNone,win32,win64,linux,android,darwin,freebsd,openbsd,aix,wince,iphonesim,embedded,java,msdos,haiku,solaris,dragonfly,netbsd,morphos,aros,amiga,go32v2,freertos,ios,ultibo);
  TSUBARCH = (saNone,armv4,armv4t,armv6,armv6m,armv7a,armv7em,armv7m,avr1,avr2,avr25,avr35,avr4,avr5,avr51,avr6,avrtiny,avrxmega3,pic32mx,rv32imac,lx6,lx106);

  TSUBARCHS = set of TSUBARCH;

const
  SUBARCH_OS         = [{TOS.osNone,}TOS.embedded,TOS.freertos,TOS.ultibo];
  SUBARCH_CPU        = [{TCPU.cpuNone,}TCPU.arm,TCPU.avr,TCPU.mipsel,TCPU.riscv32,TCPU.xtensa];
  SUBARCH_ARM        = [armv4..armv7m];
  SUBARCH_AVR        = [avr1..avrxmega3];
  SUBARCH_MIPSEL     = [pic32mx];
  SUBARCH_RISCV32    = [rv32imac];
  SUBARCH_XTENSA     = [lx6..lx106];

type
  TSearchSetting = (ssUp,ssAuto,ssCustom);
  TARMARCH  = (default,armel,armeb,armhf);

const
  ARMArchFPCStr : array[TARMARCH] of string=(
    '','-dFPC_ARMEL','-dFPC_ARMEB','-dFPC_ARMHF'
  );
  FPCUP_AUTO_MAGIC = 'FPCUP_AUTO';

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

  TCompilerType=(ctBootstrap,ctInstalled);

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  private
    function GetCrossModuleName:string;
    function GetTargetCPUName:string;
    function GetTargetOSName:string;
    function GetSubarchName:string;
  protected
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinutilsPathInPath: boolean;
    FBinUtilsDirectoryID: string; //where to find the binutils themselves
    FCompilerUsed: TCompilerType;
    FSearchMode: TSearchSetting;
    FCrossModuleNamePrefix: string; //used for identifying module to user in messages
    FCrossOpts: TStringList; //Options to be added to CROSSOPT by the calling code. XP= (binutils prefix) is already done, no need to add it
    FFPCCFGSnippet: string; //snippet to be added to fpc.cfg in order to find binutils/libraries etc
    FLibsPath: string; //path for target environment libraries
    FTargetCPU: TCPU; //cpu for the target environment. Follows FPC names
    FTargetOS: TOS; //operating system for the target environment. Follows FPC names
    FSubArch: TSUBARCH; //optional subarch for embedded targets
    FRegisterName: string;
    FLibsFound,FBinsFound,FCrossOptsAdded:boolean;
    FSolarisOI:boolean;
    FMUSL:boolean;
    // Sets FBinutilspath if file LookFor found in Directory. Returns true if found.
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
    // Parses space-delimited crossopt parameters and sets the CrossOpt property
    procedure SetCrossOpt(CrossOpts: string);
    // Pass subarch if any
    procedure SetSubArch(SubArch: TSUBARCH);
    procedure ShowInfo(info: string = ''; Level: TEventType = etInfo);
    // Reset some variables to default values
    procedure Reset; virtual;
    // Which compiler should be used for cross compilation.
    // Normally the bootstrap compiler, but cross compilers may need the installed compiler
    // (often a trunk version, though there's no tests yet that check trunk is installed)
    property CompilerUsed: TCompilerType read FCompilerUsed;
    property SearchModeUsed: TSearchSetting read FSearchMode write FSearchMode;
    property CrossModuleName: string read GetCrossModuleName;
    // Represents arguments for CROSSOPT parameter
    // No need to add XP= (binutils prefix): calling code will do this
    // CROSSOPT: Compiler makefile allows to specify compiler options that are only used during the actual crosscompiling phase (i.e. not during the initial bootstrap cycle)
    // Also used in fpc.cfg snippet to set options when compiling for cross target
    property CrossOpt: TStringList read FCrossOpts;
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
    property TargetCPUName: string read GetTargetCPUName;
    property TargetOSName: string read GetTargetOSName;
    property SubArchName:string read GetSubarchName;
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

procedure RegisterCrossCompiler(Platform:string;aCrossInstaller:TCrossInstaller);
function GetExeExt: string;

var
  CrossUtils:TCrossUtils;
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
    if aCPU='armv6' then xCPU:=TCPU.arm
    else
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
      TCPU.xtensa:   if (aOS<>TOS.ultibo) then result:=SUBARCH_XTENSA;
    end;
    if (aOS=TOS.ultibo) then result:=[TSUBARCH.armv6,TSUBARCH.armv7a];
    if (aOS=TOS.freertos) then result:=[TSUBARCH.armv6m,TSUBARCH.armv7em,TSUBARCH.armv7m];
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
    result:=TARMARCH.default
  else
    result:=TARMARCH(GetEnumValueSimple(TypeInfo(TARMARCH),aARMArch));
  if Ord(result) < 0 then
    raise Exception.CreateFmt('Invalid ARM Arch name "%s" for GetARMArch.', [aARMArch]);
end;



function GetARMArchFPCDefine(aARMArch:TARMARCH):string;
begin
  result:=ARMArchFPCStr[aARMArch];
end;


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

procedure TCrossInstaller.AddFPCCFGSnippet(aSnip: string);
var
  aSnippd:string;
  i:integer;
begin
  if Length(Trim(aSnip))=0 then exit;

  aSnippd:=StringReplace(aSnip,' ',LineEnding,[rfReplaceAll]);
  if (Pos(aSnippd,FFPCCFGSnippet)>0) then exit;

  if Length(FPCCFGSnippet)>0 then
  begin
    if RPos(LineEnding,FFPCCFGSnippet)<Length(FFPCCFGSnippet) then FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding;
    FFPCCFGSnippet:=FFPCCFGSnippet+aSnippd;
  end
  else FFPCCFGSnippet:=aSnippd;

end;

procedure TCrossInstaller.SearchLibraryInfo(found:boolean; const extrainfo:string='');
begin
  //if found then
  //  Infoln(CrossModuleName + ': Found correct library in directory '+FLibsPath, etInfo)
  //else
  //  Infoln(CrossModuleName + ': Searched but did not find any library !!', etError);
  //if Length(extrainfo)>0 then Infoln(CrossModuleName + ' libs : '+extrainfo, etInfo);
end;

procedure TCrossInstaller.SearchBinUtilsInfo(found:boolean; const extrainfo:string='');
begin
  //if found then
  //  Infoln(CrossModuleName + ': Found correct binary utilities in directory '+FBinUtilsPath, etInfo)
  //else
  //  Infoln(CrossModuleName + ': Searched but did not find any binary utilities !!', etError);
  //if Length(extrainfo)>0 then Infoln(CrossModuleName + ' bins : '+extrainfo, etInfo);
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
  for i:=0 to FCrossOpts.Count-1 do AddFPCCFGSnippet(FCrossOpts[i]);
  FCrossOptsAdded:=true;
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

procedure TCrossInstaller.ShowInfo(info: string = ''; Level: TEventType = etInfo);
begin
  (*
  if Length(info)>0 then Infoln(CrossModuleName+': '+info,Level)
  {$ifndef LCL}
  else Infoln(CrossModuleName+' crosscompiler loading',etDebug);
  {$else}
  ;
  {$endif}
  *)
end;

procedure TCrossInstaller.Reset;
begin
  FFPCCFGSnippet:='';
  FLibsFound:=false;
  FBinsFound:=false;
  FCrossOptsAdded:=false;
  FCrossOpts.Clear;
  FSubArch:=TSUBARCH.saNone;

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

  // use installed source compiler for cross compiling by default
  // bootstrap compiler was only usefull in some cornercases
  // see: http://lists.freepascal.org/pipermail/fpc-devel/2018-August/039494.html
  FCompilerUsed:=ctInstalled;

  FCrossModuleNamePrefix:='TAny';

  FBinUtilsDirectoryID:='none';
end;

destructor TCrossInstaller.Destroy;
begin
  FCrossOpts.Free;
  inherited Destroy;
end;

finalization
  if assigned(CrossInstallers) then
    CrossInstallers.Destroy;
end.

