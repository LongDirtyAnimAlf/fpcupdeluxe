unit installerFpc;

{ FPC installer/updater module
Copyright (C) 2012-2014 Ludo Brands, Reinier Olislagers

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

{$I fpcupdefines.inc}

interface

uses
  Classes, SysUtils, installerBase, installerCore,
  m_crossinstaller;

Const
  Sequences=
// convention: FPC sequences start with 'FPC' [constant _FPC].
//standard fpc build
    _DECLARE+_FPC+_SEP+
    _CLEANMODULE+_FPC+_SEP+
    // Create the link early so invalid previous
    // versions are overwritten:
    _EXECUTE+_CREATEFPCUPSCRIPT+_SEP+
    _CHECKMODULE+_FPC+_SEP+
    _GETMODULE+_FPC+_SEP+
    _BUILDMODULE+_FPC+_SEP+
    _END+

//standard uninstall
    _DECLARE+_FPC+_UNINSTALL+_SEP+
    //_CLEANMODULE+_FPC+_SEP+
    _UNINSTALLMODULE+_FPC+_SEP+
    _END+

    {$ifdef mswindows}
    {$ifdef win32}
    // Crosscompile build
    _DECLARE+_FPC+_CROSSWIN+_SEP+
    _SETCPU+'x86_64'+_SEP +_SETOS+'win64'+_SEP +
    // Getmodule has already been done
    _CLEANMODULE+_FPC+_SEP+
    _BUILDMODULE+_FPC+_SEP+
    _SETCPU+'i386'+_SEP+_SETOS+'win32'+_SEP+
    _END+
    {$endif}

    {$ifdef win64}
    // Crosscompile build
    _DECLARE+_FPC+_CROSSWIN+_SEP+
    _SETCPU+'i386'+_SEP+_SETOS+'win32'+_SEP+
    // Getmodule has already been done
    _CLEANMODULE+_FPC+_SEP+
    _BUILDMODULE+_FPC+_SEP+
    _SETCPU+'x86_64'+_SEP+_SETOS+'win64'+_SEP+
    _END+
    {$endif}
    {$endif mswindows}

    //selective actions triggered with --only=SequenceName
    _DECLARE+_FPC+_CHECK+_ONLY+_SEP+_CHECKMODULE+_FPC+_SEP+_END+
    _DECLARE+_FPC+_CLEAN+_ONLY+_SEP+_CLEANMODULE+_FPC+_SEP+_END+
    _DECLARE+_FPC+_GET+_ONLY+_SEP+_GETMODULE+_FPC+_SEP+_END+
    _DECLARE+_FPC+_BUILD+_ONLY+_SEP+_BUILDMODULE+_FPC+_SEP+_END+

    //standard clean
    _DECLARE+_FPC+_CLEAN+_SEP+
    _CLEANMODULE+_FPC+_SEP+
    _END+

    _DECLARE+_FPCCLEANBUILDONLY+_SEP+
    _CLEANMODULE+_FPC+_SEP+
    _BUILDMODULE+_FPC+_SEP+
    _END+

    _DECLARE+_FPCREMOVEONLY+_SEP+
    _CLEANMODULE+_FPC+_SEP+
    _UNINSTALLMODULE+_FPC+_SEP+
    _END+

    _DECLARE+_MAKEFILECHECKFPC+_SEP+
    _BUILDMODULE+_MAKEFILECHECKFPC+_SEP+
    _END+

    _DECLARE+_NATIVECROSSFPC+_SEP+
    _CLEANMODULE+_NATIVECROSSFPC+_SEP+
    _BUILDMODULE+_NATIVECROSSFPC+_SEP+

    _ENDFINAL;

type
  { TFPCInstaller }

  TFPCInstaller = class(TBaseFPCInstaller)
  strict private
    FFPCBaseDir      : string;
    FFPCUnitDir      : string;
    FFPCLibraryDir   : string;
    FFPCShareDir     : string;
    FFPCDataDir      : string;
    FFPCDocDir       : string;
    FFPCExampleDir   : string;
    FFPCMessageDir   : string;
    FTargetCompilerName                     : string;
    FBootstrapCompiler                      : string;
    FBootstrapCompilerDirectory             : string;
    FBootstrapCompilerURL                   : string;
    FBootstrapCompilerOverrideVersionCheck  : boolean;
    InitDone                                : boolean;
  private
    FSoftFloat       : boolean;
    FDelphiRTTI      : boolean;
    FUseLibc         : boolean;
    FUseRevInc : boolean;
    function GetCompilerVersionNumber(aVersion: string; const index:byte=0): integer;
    function CleanExtra(aCPU:TCPU=TCPU.cpuNone;aOS:TOS=TOS.osNone):boolean;
  protected
    // FPC components install locations
    property FPCBaseDir:string read FFPCBaseDir;
    property FPCUnitDir:string read FFPCUnitDir;
    property FPCLibraryDir:string read FFPCLibraryDir;
    property FPCShareDir:string read FFPCShareDir;
    property FPCDataDir:string read FFPCDataDir;
    property FPCDocDir:string read FFPCDocDir;
    property FPCExampleDir:string read FFPCExampleDir;
    property FPCMessageDir:string read FFPCMessageDir;
    procedure SetFPCInstallDirectory(value:string);override;
    function GetUnitsInstallDirectory(const WithMagic:boolean=false):string;
    function GetVersionFromUrl(aUrl: string): string;override;
    function GetVersionFromSource: string;override;
    function GetReleaseCandidateFromSource:integer;override;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // Retrieves compiler version string
    function GetCompilerTargetOS(CompilerPath: string): string;
    function GetCompilerTargetCPU(CompilerPath: string): string;
    function GetBootstrapCompilerVersionFromVersion(aVersion: string): string;
    function GetBootstrapCompilerVersionFromSource(aSourcePath: string; GetLowestRequirement:boolean=false): string;
    // Creates fpc proxy script that masks general fpc.cfg
    function CreateFPCScript:boolean;
    // Downloads bootstrap compiler for relevant platform, reports result.
    function DownloadBootstrapCompiler: boolean;
    function GetFPCRevision: string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function IsCross:boolean;override;
    function InitModule(DesiredBootstrapVersion:string=''):boolean;
  public
    property UseLibc: boolean read FUseLibc;
    property SoftFloat: boolean write FSoftFloat;
    property DelphiRTTI: boolean write FDelphiRTTI;
    //Directory that has compiler needed to compile compiler sources. If compiler doesn't exist, it will be downloaded
    property BootstrapCompilerDirectory: string write FBootstrapCompilerDirectory;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    function ConfigModule(ModuleName:string): boolean; override;
    // Install update sources
    function GetModule(ModuleName:string): boolean; override;
    // Perform some checks on the sources
    function CheckModule(ModuleName: string): boolean; override;
    function UnInstallModule(ModuleName:string): boolean; override;
    // If yes, an override option will be passed to make (OVERRIDEVERSIONCHECK=1)
    // If no, the FPC make script enforces that the latest stable FPC bootstrap compiler is used.
    // This is required information for setting make file options
    property BootstrapCompilerOverrideVersionCheck: boolean read FBootstrapCompilerOverrideVersionCheck;
    property TargetCompilerName: string read FTargetCompilerName;
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TFPCNativeInstaller }

  TFPCNativeInstaller = class(TFPCInstaller)
  protected
    // Build module descendant customisation. Runs make all/install for native FPC
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TFPCCrossInstaller }

  TFPCCrossInstaller = class(TFPCInstaller)
  strict private
    FCrossCompilerName: string;
    FFPCCrossCompilerName: string;
  private
    {$ifndef crosssimple}
    function CompilerUpdateNeeded:boolean;
    function PackagesNeeded:boolean;
    {$endif}
    function InsertFPCCFGSnippet(FPCCFG,Snippet: string): boolean;
    property CrossCompilerName: string read FCrossCompilerName;
    property FPCCrossCompilerName: string read FFPCCrossCompilerName;
  protected
    function SubarchTarget:boolean;
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function UnInstallModule(ModuleName:string): boolean; override;
    procedure SetTarget(aCPU:TCPU;aOS:TOS;aSubArch:TSUBARCH);override;
  end;


implementation

uses
  StrUtils,
  FileUtil,
  fpcuputil,
  repoclient,
  processutils,
  {$IFDEF UNIX}
  baseunix,
  LazFileUtils,
  {$ENDIF UNIX}
  math;

const
  {$ifndef FPC_HAS_TYPE_EXTENDED}
  DEFINE_SOFT_FPUX80 = 'FPC_SOFT_FPUX80';
  {$endif}
  DEFINE_DELPHI_RTTI = 'ENABLE_DELPHI_RTTI';

{ TFPCCrossInstaller }

constructor TFPCCrossInstaller.Create;
begin
  inherited Create;
  FCrossCompilerName:='invalid';
  FFPCCrossCompilerName:='invalid';
end;

destructor TFPCCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

function TFPCCrossInstaller.SubarchTarget:boolean;
begin
  result:=false;
  if (NOT Assigned(CrossInstaller)) then exit;
  result:=((CrossInstaller.TargetCPU<>TCPU.cpuNone) AND (CrossInstaller.TargetOS<>TOS.osNone) AND (CrossInstaller.TargetOS in SUBARCH_OS) AND (CrossInstaller.TargetCPU in SUBARCH_CPU));
end;

{$ifndef crosssimple}
function TFPCCrossInstaller.CompilerUpdateNeeded:boolean;
var
  NativeVersion,CrossVersion:string;
  NativeCompiler,CrossCompiler:string;
  NativeAge,CrossAge:Longint;
begin
  {$ifdef Darwin}
  if CrossInstaller.TargetCPU=GetTCPU(GetSourceCPU) then
  begin
    // On Darwin, native compiler and cross-compiler share the same name.
    // And also are the same (in theory at least)
    // So, always skip the compiler when building cross
    exit(false);
  end;
  {$endif}

  result:=true;
  NativeCompiler:=GetFPCInBinDir;
  if (FileExists(NativeCompiler)) then
  begin
    CrossCompiler:=ExtractFilePath(NativeCompiler)+FPCCrossCompilerName;
    if FileExists(CrossCompiler) then
    begin
      // Look at version and revision
      NativeVersion:=CompilerVersion(NativeCompiler);
      CrossVersion:=CompilerVersion(CrossCompiler);
      if  (Length(CrossVersion)>0) AND (CrossVersion<>'0.0.0') AND (CompareVersionStrings(CrossVersion,NativeVersion)=0) then
      begin
        NativeVersion:=CompilerRevision(NativeCompiler);
        CrossVersion:=CompilerRevision(CrossCompiler);
        if (Length(CrossVersion)>0) AND (CrossVersion=NativeVersion) then result:=false;
      end;
      if (NOT result) then
      begin
        // Look at fileage
        NativeAge:=FileAge(NativeCompiler);
        CrossAge:=FileAge(CrossCompiler);
        result:=(NativeAge>CrossAge);
      end;
    end;
  end;
end;

function TFPCCrossInstaller.PackagesNeeded:boolean;
begin
  result:=true;

  // registry.pp does not build for arm-freertos, so disable
  if (CrossInstaller.TargetCPU=TCPU.arm) AND (CrossInstaller.TargetOS=TOS.freertos) then result:=false;

  // disable by default for avr-embedded (due to unicode not compiled-in)
  if (CrossInstaller.TargetCPU=TCPU.avr) AND (CrossInstaller.TargetOS=TOS.embedded) then result:=false;

  // Safeguards
  if (CrossInstaller.TargetCPU=TCPU.arm) AND (CrossInstaller.TargetOS=TOS.embedded) then result:=false;

  // Safeguards
  if (CrossInstaller.TargetCPU=TCPU.xtensa) AND (CrossInstaller.TargetOS=TOS.freertos) then result:=false;
end;
{$endif}

function TFPCCrossInstaller.InsertFPCCFGSnippet(FPCCFG,Snippet: string): boolean;
// Adds snippet to fpc.cfg file or replaces if if first line of snippet is present
// Returns success (snippet inserted or added) or failure
const
  FPCCFGINFOTEXT='FPCCrossInstaller (InsertFPCCFGSnippet: '+FPCCONFIGFILENAME+'): ';
var
  ConfigText: TStringList;
  i,j,k:integer;
  SnipBegin,SnipEnd: integer;
  SnippetText,SubarchText: TStringList;
  NewSnipped:boolean;
  IOSProtection:boolean;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  LocalTargetCPUName:string;
begin
  result:=true;

  if (NOT Assigned(CrossInstaller)) OR (NOT FileExists(FPCCFG)) then
  begin
    exit(false);
  end;

  NewSnipped:=false;

  {
  When compiling for iOS, FPC itself also defines DARWIN.
  So, we must make sure that the Darwin settings are only used when NOT targetting iOS.
  In that case, add
  #IFNDEF IOS
  #ENDIF IOS
  }
  IOSProtection:=((CrossInstaller.TargetOS=TOS.darwin) AND (CrossInstaller.TargetCPU in [TCPU.aarch64,TCPU.arm]));

  //Set CPU
  //Needed to differentiate between 32 and 64 bit powerpc
  if (CrossInstaller.TargetCPU=TCPU.powerpc) then
    LocalTargetCPUName:='powerpc32' //Distinguish between 32 and 64 bit powerpc
  else
    LocalTargetCPUName:=CrossInstaller.TargetCPUName;

  ConfigText:=TStringList.Create;
  {$IF FPC_FULLVERSION > 30100}
  //ConfigText.DefaultEncoding:=TEncoding.ASCII;
  {$ENDIF}
  SnippetText:=TStringList.Create;

  SubarchText:=TStringList.Create;

  try

    if Length(Snippet)>0 then SnippetText.Text:=Snippet;

    ConfigText.LoadFromFile(FPCCFG);

    SnipBegin:=StringListSame(ConfigText,SnipMagicBegin+CrossInstaller.RegisterName);
    if (SnipBegin<>-1) then
    begin
      // Now look for the end of the snipped
      SnipEnd:=StringListSame(ConfigText,SnipMagicEnd{+CrossInstaller.RegisterName},SnipBegin);
      // Also look for the end of the snipped of an old config file
      //if (SnipEnd=-1) then
      //  SnipEnd:=StringListSame(ConfigText,SnipMagicEnd,SnipBegin);
      if (SnipEnd=-1) then
      begin
        Infoln(FPCCFGINFOTEXT+'Existing snippet was not closed correct. Will continue, but please check your '+FPCCONFIGFILENAME+'.',etWarning);
        // Snipped not closed correct.
        // Look for start of next snipped in any
        SnipEnd:=StringListSame(ConfigText,SnipMagicBegin,SnipBegin);
        if (SnipEnd=-1) then
        begin
          // Snipped not closed at all
          // We could error out.
          // But, for now, set end to end of config file.
          // To be improved.
          SnipEnd:=ConfigText.Count;
          //result:=false;
          //Infoln(FPCCFGINFOTEXT+'Existing snippet was not closed at all. Please check your '+FPCCONFIGFILENAME+' for '+SnipMagicEnd+'.',etError);
        end
        else
        begin
          Dec(SnipEnd);
        end;
      end;
    end
    else
    begin
      // Snipped not found.
      NewSnipped:=true;
      if SnippetText.Count>0 then
      begin
        // Add empty line if needed
        if ConfigText[ConfigText.Count-1]<>'' then ConfigText.Append('');

        // Simple: new snipped to be appended
        ConfigText.Append(SnipMagicBegin+CrossInstaller.RegisterName);
        ConfigText.Append('# Inserted by up v'+DELUXEVERSION+' on '+DateTimeToStr(Now));
        ConfigText.Append('# Cross compile settings dependent on both target OS and target CPU');
        ConfigText.Append('#IFDEF FPC_CROSSCOMPILING');
        ConfigText.Append('#IFDEF '+UpperCase(CrossInstaller.TargetOSName));
        if IOSProtection then ConfigText.Append('#IFNDEF IOS');
        ConfigText.Append('#IFDEF CPU'+UpperCase(LocalTargetCPUName));
        // Just add new snipped
        if SnippetText.Count>0 then
        begin
          for i:=0 to (SnippetText.Count-1) do
            ConfigText.Append(SnippetText.Strings[i]);
        end;
        ConfigText.Append('#ENDIF CPU'+UpperCase(LocalTargetCPUName));
        if IOSProtection then ConfigText.Append('#ENDIF IOS');
        ConfigText.Append('#ENDIF '+UpperCase(CrossInstaller.TargetOSName));
        ConfigText.Append('#ENDIF FPC_CROSSCOMPILING');
        ConfigText.Append(SnipMagicEnd{+CrossInstaller.RegisterName});
      end;
    end;

    if result AND (NOT NewSnipped) then
    begin

      if (SnippetText.Count=0) then
      begin
        // Remove config for this target
        for k:=0 to (SnipEnd-SnipBegin) do if (SnipBegin<ConfigText.Count) then ConfigText.Delete(SnipBegin);
        while (SnipBegin<ConfigText.Count) AND (ConfigText[SnipBegin]='') do ConfigText.Delete(SnipBegin);
      end
      else
      begin
        // Existing snipped !! The hard part.

        // First, locate real config snipped inside config
        j:=StringListSame(ConfigText,'#IFDEF CPU'+UpperCase(LocalTargetCPUName),SnipBegin);
        if (j>SnipEnd) then j:=-1;
        if (j=-1) then
        begin
          // This is a severe error and should never happen
          // Do not yet know how to handle it
          result:=false;
        end
        else
        begin
          i:=StringListSame(ConfigText,'#ENDIF CPU'+UpperCase(LocalTargetCPUName),j);
          k:=StringListSame(ConfigText,'#ENDIF',j);
          if (i=-1) then i:=k;
          if (k=-1) then k:=i;
          i:=Min(i,k);
          if (i=-1) then
          begin
            // This is a severe error and should never happen
            // Do not yet know how to handle it
            result:=false;
          end
          else
          begin
            Inc(j);
            Dec(i);
            SnipBegin:=j;
            SnipEnd:=i;
          end;
        end;

        // SnipBegin and SnipEnd now point to space between #IFDEFS of target CPU
        // So everything in between handles general settings and subarch settings

        if result then
        begin

          // Save all subarch settings except for current subarch that needs to be replaced

          Subarchs:=GetSubarchs(CrossInstaller.TargetCPU,CrossInstaller.TargetOS);
          for SUBARCH in Subarchs do
          begin
            if (SUBARCH=TSUBARCH.saNone) then continue;

            repeat
              // Do we have a config with a Subarch define
              j:=StringListSame(ConfigText,'#IFDEF CPU'+UpperCase(GetSubarch(SUBARCH)),SnipBegin);
              if (j>SnipEnd) then j:=-1;
              if (j=-1) then break;
              i:=StringListSame(ConfigText,'#ENDIF CPU'+UpperCase(GetSubarch(SUBARCH)),j);
              if (i>SnipEnd) then i:=-1;
              if (i=-1) then
              begin
                // This is a severe error and should never happen
                // Do not yet know how to handle it
                // Just quit
                result:=false;
                break;
              end
              else
              begin
                // Found subarch part
                for k:=0 to (i-j) do
                begin
                  // Save non-matching subarch settings
                  if (SUBARCH<>CrossInstaller.SubArch) then
                    SubarchText.Append(ConfigText.Strings[j]);
                  // Delete this subarch part from config
                  ConfigText.Delete(j);
                  Dec(SnipEnd);
                end;
              end;
            until false;

          end;

          if result then
          begin

            // Add new subarch settings if any
            if (CrossInstaller.SubArch<>TSUBARCH.saNone) then
            begin
              NewSnipped:=false;
              SubarchText.Append('#IFDEF CPU'+UpperCase(CrossInstaller.SubArchName));
              repeat
                // Do we have a config with a Subarch define
                j:=StringListSame(SnippetText,'#IFDEF CPU'+UpperCase(CrossInstaller.SubArchName),0);
                if (j=-1) then break;
                // We have a subarch
                // Now, add only subarch part.
                i:=StringListSame(SnippetText,'#ENDIF CPU'+UpperCase(CrossInstaller.SubArchName),j);
                if (i=-1) then
                begin
                  // This is a severe error and should never happen
                  // Do not yet know how to handle it
                  // Just quit
                  result:=false;
                  break;
                end
                else
                begin
                  // Found subarch part
                  NewSnipped:=true;
                  // Delete source #ENDIF CPUXXXXX
                  SnippetText.Delete(i);
                  // Delete source #IFDEF CPUXXXXX
                  SnippetText.Delete(j);
                  Dec(i,2);
                  for k:=j to i do
                  begin
                    SubarchText.Append(SnippetText.Strings[j]);
                    SnippetText.Delete(j);
                  end;
                end;
              until false;
              SubarchText.Append('#ENDIF CPU'+UpperCase(CrossInstaller.SubArchName));

              // Bit tricky: remove defines if not used anymore
              if (NOT NewSnipped) then
              begin
                SubarchText.Delete(Pred(SubarchText.Count));
                SubarchText.Delete(Pred(SubarchText.Count));
              end;
            end;

            if result then
            begin
              // We now have a subarchtext with all new and previous defines.

              for k:=0 to (SnipEnd-SnipBegin) do ConfigText.Delete(SnipBegin);

              // We now have a  a cleaned up general config

              // Add new subarch snipped into config file
              if (SubarchText.Count>0) then
              begin
                for k:=0 to (SubarchText.Count-1) do
                begin
                  ConfigText.Insert(SnipBegin,SubarchText.Strings[k]);
                  Inc(SnipBegin);
                end;
              end;

              // Add new config snipped into config file

              if (SnippetText.Count>0) then
              begin
                for k:=0 to (SnippetText.Count-1) do
                begin
                  ConfigText.Insert(SnipBegin,SnippetText.Strings[k]);
                  Inc(SnipBegin);
                end;
              end;

              // All ok and done !!
            end;
          end;
        end;
      end;
    end;

    if (SnippetText.Count>0) then
    begin
      //{$ifndef Darwin}
      {$ifdef MSWINDOWS}
      // remove pipeline assembling for Darwin when cross-compiling !!
      // for FPC >= rev 42302 this is not needed anymore: DoPipe:=false; by default on non-unix !!
      SnipBegin:=ConfigText.IndexOf('# use pipes instead of temporary files for assembling');
      if SnipBegin>-1 then
      begin
        if ConfigText.Strings[SnipBegin+1]<>'#IFNDEF FPC_CROSSCOMPILING' then
        begin
          ConfigText.Insert(SnipBegin+1,'#IFNDEF FPC_CROSSCOMPILING');
          ConfigText.Insert(SnipBegin+3,'#ENDIF');
        end;
      end;
      {$endif}
    end;

    ConfigText.SaveToFile(FPCCFG);

    result:=true;
  finally
    SubarchText.Free;
    ConfigText.Free;
    SnippetText.Free;
  end;

  Infoln(FPCCFGINFOTEXT+'Inserting snippet in '+FPCCFG+' done.',etInfo);
end;

procedure TFPCCrossInstaller.SetTarget(aCPU:TCPU;aOS:TOS;aSubArch:TSUBARCH);
begin
  inherited;
  if Assigned(CrossInstaller) then
  begin
    FCrossCompilerName:=GetCrossCompilerName(CrossInstaller.TargetCPU);
    {$ifdef Darwin}
    FFPCCrossCompilerName:=GetCompilerName(CrossInstaller.TargetCPU);
    {$else}
    FFPCCrossCompilerName:=FCrossCompilerName;
    {$endif}
  end
  else
  begin
    FCrossCompilerName:='invalid';
    FFPCCrossCompilerName:=FCrossCompilerName;
    raise Exception.Create('Invalid crosscompiler name. Cause: cross-installer not assigned.');
  end;
end;

function TFPCCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
// Runs make/make install for cross compiler.
// Error out on problems; unless module considered optional, i.e. in
// crosswin32-64 and crosswin64-32 steps.
type
  {$ifdef crosssimple}
  TSTEPS = (st_Start,st_MakeAll,st_RtlInstall,st_PackagesInstall,st_NativeCompiler,st_Finished);
  {$else}
  TSTEPS = (st_Start,st_Compiler,st_CompilerInstall,st_RtlBuild,st_RtlInstall,st_PackagesBuild,st_PackagesInstall,st_NativeCompiler,st_Finished);
  {$endif}
var
  FPCCfg:String; //path+filename of the fpc.cfg configuration file
  NativeCompilerOptions:String;
  CrossCompilerOptions:String;
  i,j:integer;
  TxtFile:Text;
  s1,s2:string;
  UnitSearchPath:string;
  MakeCycle:TSTEPS;
  //ARMArch:TARMARCH;
  SupportedList:TStringList;
  {$ifdef MSWINDOWS}
  Counter:integer;
  {$endif}
begin
  result:=inherited;
  result:=false; //fail by default

  if Assigned(CrossInstaller) then
  begin
    //CrossInstaller.Reset;

    {$ifdef win32}
    // Skip cross-builing towards win64 for old versions of FPC
    if (CrossInstaller.TargetCPU=TCPU.x86_64) and ((CrossInstaller.TargetOS=TOS.win64) or (CrossInstaller.TargetOS=TOS.win32)) then
    begin
      if (SourceVersionNum<CalculateFullVersion(2,4,2)) then
      begin
        result:=true;
        exit;
      end;
    end;
    {$endif win32}

    if CrossInstaller.TargetCPU=TCPU.jvm then DownloadJasmin;

    CrossInstaller.SetFPCVersion(SourceVersionStr);
    //CrossInstaller.SetCrossOpt(CrossOPT);
    //CrossInstaller.SetSubArch(CrossOS_SubArch);
    //CrossInstaller.SetABI(CrossOS_ABI);

    result:=(NOT ((ieLibs in FErrorCodes) OR (ieBins in FErrorCodes)));

    if (NOT result) then Infoln(infotext+'Missing cross tools and/or libs', etError);

    if result then
    begin
      result:=false;

      s1:=CompilerVersion(FCompiler);

      if (s1<>'0.0.0') then
        Infoln('FPC '+CrossInstaller.TargetCPUName+'-'+CrossInstaller.TargetOSName+' cross-builder: Using compiler with version: '+s1, etInfo)
      else
        Infoln(infotext+'FPC compiler ('+FCompiler+') version error: '+s1+' ! Should never happen: expect many errors !!', etError);

      {$ifdef MSWINDOWS}
      CreateBinutilsList(CrossInstaller.FPCVersion);
      {$endif MSWINDOWS}

      FPCCfg:=GetFPCConfigPath(FPCCONFIGFILENAME);

      begin
        // Add binutils path to path if necessary
        if CrossInstaller.BinUtilsPathInPath then
           SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),false,true);

        for MakeCycle:=Low(TSTEPS) to High(TSTEPS) do
        begin
          if ((ModuleName=_NATIVECROSSFPC) AND (MakeCycle<>st_NativeCompiler)) then continue;
          if ((ModuleName<>_NATIVECROSSFPC) AND (MakeCycle=st_NativeCompiler)) then continue;

          {$ifndef crosssimple}

          // ARMHF crosscompiler build option
          if (MakeCycle=st_Compiler) then
          begin
            if (CrossInstaller.TargetCPU=TCPU.arm) then
            begin

              (*
              // what to do ...
              // always build hardfloat for ARM ?
              // or default to softfloat for ARM ?
              // FPC sources default (ppcarm.lpi): ARMHF for versions >= 3.2.0
              // decision: always build hardfloat for FPC >= 3.2.0
              j:=CalculateNumericalVersion(CrossInstaller.FPCVersion);
              if (j<>0) AND (j>=CalculateFullVersion(3,2,0)) then
              begin
                s2:=ARMArchFPCStr[TARMARCH.armhf];
                for ARMArch := Low(TARMARCH) to High(TARMARCH) do
                begin
                  s1:=ARMArchFPCStr[ARMArch];
                  if (Length(s1)>0) and (Pos(s1,FCompilerOptions)>0) then
                  begin
                    s2:='';
                    break;
                  end;
                end;
                if (Length(s2)>0) then
                begin
                  Infoln('Adding ARMHF compiler option for FPC >= 3.2.0 !',etWarning);
                  FCompilerOptions:=FCompilerOptions+' '+s2;
                end;
              end;
              *)

            end;
          end;

          if (MakeCycle=st_RtlBuild) then
          begin

            if (CrossInstaller.TargetCPU=TCPU.arm) then
            begin

              //Check for EABI + FPC_ARMHF combo that is invalid for everything < 3.3
              //This is tricky
              s2:='-CaEABI';
              i:=StringListSame(CrossInstaller.CrossOpt,s2);
              if (i<>-1) then
              begin
                // Get the correct name of the cross-compiler in source-directory
                s1:=ConcatPaths([SourceDirectory,'compiler',CrossCompilerName]);
                // Get the correct name of the cross-compiler in install-directory
                if (NOT FileExists(s1)) then
                  s1:=FPCBinDir+DirectorySeparator+FPCCrossCompilerName;
                if FileExists(s1) then
                begin
                  // Get compiler ABI's
                  s1:=CompilerABI(s1);
                  if (Length(s1)>0) then
                  begin
                    SupportedList:=TStringList.Create;
                    try
                      SupportedList.Text:=s1;
                      j:=StringListSame(SupportedList,'EABI');
                      if (j=-1) then
                      begin
                        // -CaEABI not allowed: remove it from config !!
                        Infoln('Removing '+s2+' crosscompiler option: not allowed for ARMHF FPC '+CrossInstaller.FPCVersion+' !',etWarning);
                        CrossInstaller.CrossOpt.Delete(i);
                        // The cfg snipped might also contains this define: remove it
                        // Bit tricky
                        CrossInstaller.ReplaceFPCCFGSnippet(s2,'');
                      end;
                    finally
                      SupportedList.Free;
                    end;
                  end;
                end;
              end;

              //Check for FPV4_SP_D16 that is invalid for everything < 3.3
              //This is tricky
              s2:='-CfFPV4_SP_D16';
              i:=StringListSame(CrossInstaller.CrossOpt,s2);
              if (i<>-1) then
              begin
                // Get the correct name of the cross-compiler in source-directory
                s1:=ConcatPaths([SourceDirectory,'compiler',CrossCompilerName]);
                // Get the correct name of the cross-compiler in install-directory
                if (NOT FileExists(s1)) then
                  s1:=FPCBinDir+DirectorySeparator+FPCCrossCompilerName;
                if FileExists(s1) then
                begin
                  // Get compiler FPU's
                  s1:=CompilerFPU(s1);
                  if (Length(s1)>0) then
                  begin
                    SupportedList:=TStringList.Create;
                    try
                      SupportedList.Text:=s1;
                      j:=StringListSame(SupportedList,'FPV4_SP_D16');
                      if (j=-1) then
                      begin
                        // Rename this option: not allowed for FPC < 3.3
                        s1:='-CfVFPV3_D16';
                        Infoln('Renaming '+s2+' crosscompiler option to '+s1+' for FPC '+CrossInstaller.FPCVersion+' !',etWarning);
                        CrossInstaller.CrossOpt[i]:=s1;
                        // The cfg snipped might also contains this define: rename it
                        // Bit tricky
                        CrossInstaller.ReplaceFPCCFGSnippet(s2,s1);
                      end;
                    finally
                      SupportedList.Free;
                    end;
                  end;
                end;
              end;

            end;

          end;
          {$endif crosssimple}

          // Remove fpc.cfg config for target
          if (MakeCycle=Low(TSTEPS)) then
          begin
            Infoln(infotext+'Removing '+FPCCONFIGFILENAME+' config snippet for target '+CrossInstaller.RegisterName,etInfo);
            InsertFPCCFGSnippet(FPCCfg,'');

            continue;
          end;

          // Modify fpc.cfg
          // always add this, to be able to detect which cross-compilers are installed
          // helpfull for later bulk-update of all cross-compilers
          if (MakeCycle=High(TSTEPS)) then
          begin
            s1:='';

            Infoln(infotext+'Adding '+FPCCONFIGFILENAME+' config snippet for target '+CrossInstaller.RegisterName,etInfo);

            if CrossInstaller.FPCCFGSnippet<>'' then
              s1:=s1+CrossInstaller.FPCCFGSnippet+LineEnding;

            if (CrossInstaller.TargetOS=TOS.java) then
              //s1:=s1+'-Fu'+ConcatPaths([InstallDirectory,'units',FPC_TARGET_MAGIC,'rtl','org','freepascal','rtl'])+LineEnding;
              s1:=s1+'-Fu'+ConcatPaths([GetUnitsInstallDirectory,'rtl','org','freepascal','rtl'])+LineEnding;

            if (SubarchTarget) then
            begin
              UnitSearchPath:=GetUnitsInstallDirectory(true);
              s1:=s1+'-Fu'+UnitSearchPath+DirectorySeparator+'rtl'+LineEnding;
              s1:=s1+'-Fu'+UnitSearchPath+DirectorySeparator+'packages'+LineEnding;

              if (CrossInstaller.TargetOS<>TOS.ultibo) then
              begin
                // Lazarus gives an error when units are located in a non-standard directory.
                // Therefor: create a dummy system.ppu
                // Tricky ... :-| ... !!!
                //{$ifdef MSWINDOWS}
                if (CrossInstaller.TargetOS in [TOS.embedded,TOS.freertos]) then
                  FileCreate(ConcatPaths([GetUnitsInstallDirectory,'system.ppu']));
                //{$endif MSWINDOWS}
              end;
            end;

            if (Length(s1)=0) then s1:='# Dummy (blank) config for auto-detect cross-compilers'+LineEnding;

            InsertFPCCFGSnippet(FPCCfg,s1);

            if (CrossInstaller.TargetOS=TOS.ultibo) then
            begin
              // Creating Ultibo configuration files
              if (CrossInstaller.TargetCPU=TCPU.arm) then
              begin
                s1 := GetFPCConfigPath('RPI.CFG');
                if (NOT FileExists(s1)) then
                begin
                  //create RPI.CFG
                  AssignFile(TxtFile,s1);
                  Rewrite(TxtFile);
                  try
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'# Raspberry Pi (A/B/A+/B+/Zero) specific config file');
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'-CfVFPV2');
                    writeln(TxtFile,'-CIARM');
                    writeln(TxtFile,'-CaEABIHF');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI');
                    writeln(TxtFile,'-dBCM2708');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv6),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                  finally
                    CloseFile(TxtFile);
                  end;
                end
                else
                begin
                  Infoln(infotext+'Found existing '+ExtractFileName(s1)+' in '+ExtractFileDir(s1));
                end;

                s1 := GetFPCConfigPath('RPI2.CFG');
                if (NOT FileExists(s1)) then
                begin
                  //create RPI2.CFG
                  AssignFile(TxtFile,s1);
                  Rewrite(TxtFile);
                  try
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'# Raspberry Pi 2B specific config file');
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'-CfVFPV3');
                    writeln(TxtFile,'-CIARM');
                    writeln(TxtFile,'-CaEABIHF');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI2');
                    writeln(TxtFile,'-dBCM2709');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv7a),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                  finally
                    CloseFile(TxtFile);
                  end;
                end
                else
                begin
                  Infoln(infotext+'Found existing '+ExtractFileName(s1)+' in '+ExtractFileDir(s1));
                end;

                s1 := GetFPCConfigPath('RPI3.CFG');
                if (NOT FileExists(s1)) then
                 begin
                  //create RPI3.CFG
                  AssignFile(TxtFile,s1);
                  Rewrite(TxtFile);
                  try
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'# Raspberry Pi 3B/3B+/3A+/CM3/Zero2W specific config file');
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'#IFDEF CPUARM');
                    writeln(TxtFile,'-CfVFPV3');
                    writeln(TxtFile,'-CIARM');
                    writeln(TxtFile,'-CaEABIHF');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI3');
                    writeln(TxtFile,'-dBCM2710');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv7a),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                    writeln(TxtFile,'#IFDEF CPUAARCH64');
                    writeln(TxtFile,'-CfVFP');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI3');
                    writeln(TxtFile,'-dBCM2710');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv8),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                  finally
                    CloseFile(TxtFile);
                  end;
                end
                else
                begin
                  Infoln(infotext+'Found existing '+ExtractFileName(s1)+' in '+ExtractFileDir(s1));
                end;

                s1 := GetFPCConfigPath('RPI4.CFG');
                if (NOT FileExists(s1)) then
                 begin
                  //create RPI4.CFG
                  AssignFile(TxtFile,s1);
                  Rewrite(TxtFile);
                  try
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'# Raspberry Pi 4B/400/CM4 specific config file');
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'#IFDEF CPUARM');
                    writeln(TxtFile,'-CfVFPV3');
                    writeln(TxtFile,'-CIARM');
                    writeln(TxtFile,'-CaEABIHF');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI4');
                    writeln(TxtFile,'-dBCM2711');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv7a),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                    writeln(TxtFile,'#IFDEF CPUAARCH64');
                    writeln(TxtFile,'-CfVFP');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI4');
                    writeln(TxtFile,'-dBCM2711');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv8),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                  finally
                    CloseFile(TxtFile);
                  end;
                end
                else
                begin
                  Infoln(infotext+'Found existing '+ExtractFileName(s1)+' in '+ExtractFileDir(s1));
                end;

                s1 := GetFPCConfigPath('QEMUVPB.CFG');
                if (NOT FileExists(s1)) then
                begin
                  //create QEMUVPB.CFG
                  AssignFile(TxtFile,s1);
                  Rewrite(TxtFile);
                  try
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'# QEMU VersatilePB specific config file');
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'#IFDEF CPUARM');
                    writeln(TxtFile,'-CfVFPV3');
                    writeln(TxtFile,'-CIARM');
                    writeln(TxtFile,'-CaEABIHF');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dQEMUVPB');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv7a),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                    writeln(TxtFile,'#IFDEF CPUAARCH64');
                    writeln(TxtFile,'-CfVFP');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dQEMUVPB');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv8),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                  finally
                    CloseFile(TxtFile);
                  end;
                end
                else
                begin
                  Infoln(infotext+'Found existing '+ExtractFileName(s1)+' in '+ExtractFileDir(s1));
                end;
              end;

              if ((CrossInstaller.TargetCPU=TCPU.arm) OR (CrossInstaller.TargetCPU=TCPU.aarch64)) then
              begin
                s1 := GetFPCConfigPath('RPI4.CFG');
                if (NOT FileExists(s1)) then
                begin
                  //create RPI4.CFG
                  AssignFile(TxtFile,s1);
                  Rewrite(TxtFile);
                  try
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'# Raspberry Pi 4B/400/CM4 specific config file');
                    writeln(TxtFile,'#');
                    writeln(TxtFile,'#IFDEF CPUARM');
                    writeln(TxtFile,'-CfVFPV3');
                    writeln(TxtFile,'-CIARM');
                    writeln(TxtFile,'-CaEABIHF');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI4');
                    writeln(TxtFile,'-dBCM2711');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv7a),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                    writeln(TxtFile,'#IFDEF CPUAARCH64');
                    writeln(TxtFile,'-CfVFP');
                    writeln(TxtFile,'-OoFASTMATH');
                    writeln(TxtFile,'-dRPI4');
                    writeln(TxtFile,'-dBCM2711');
                    s2:=GetUnitsInstallDirectory(true);
                    s2:=StringReplace(s2,FPC_SUBARCH_MAGIC,GetSubarch(TSUBARCH.armv8),[rfIgnoreCase]);
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
                    writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
                    writeln(TxtFile,'#ENDIF');
                  finally
                    CloseFile(TxtFile);
                  end;
                end
                else
                begin
                  Infoln(infotext+'Found existing '+ExtractFileName(s1)+' in '+ExtractFileDir(s1));
                end;
              end;
            end; // End creating Ultibo configuration files

            {$ifdef UNIX}
            //Correct for some case errors on Unixes
            if (CrossInstaller.TargetOS=TOS.java) then
            begin
              s1:=ConcatPaths([GetUnitsInstallDirectory,'rtl','org','freepascal','rtl']);
              s2:=IncludeTrailingPathDelimiter(s1)+'System.class';
              s1:=IncludeTrailingPathDelimiter(s1)+'system.class';
              if (NOT FileExists(s1)) then FileCopy(s2,s1);
            end;
            {$endif}

            continue;
          end;

          Processor.Executable := Make;
          Processor.Process.Parameters.Clear;
          {$IFDEF MSWINDOWS}
          if Length(Shell)>0 then Processor.SetParamNameData('SHELL',Shell);
          {$ENDIF}
          Processor.Process.CurrentDirectory:=SourceDirectory;

          //Still not clear if jobs can be enabled for crosscompiler builds ... :-|
          //However, on Windows, erroros occur frequently due to more jobs.
          //So, again, disabling for the time being, except for building the packages.
          if (MakeCycle in [st_PackagesBuild,st_PackagesInstall]) then
          begin
            if (NOT FNoJobs) then
            begin
              Processor.SetParamNameData('--jobs',IntToStr(FCPUCount));
              Processor.SetParamNameData('FPMAKEOPT','--threads='+IntToStr(FCPUCount));
            end;
          end;

          Processor.SetParamNameData('--directory',SourceDirectory);

          Processor.SetParamNamePathData('FPCDIR',SourceDirectory);

          {$IFDEF DEBUG}
          //To debug Makefile itself
          //Processor.SetParamData('-d');
          {$ENDIF}

          Processor.SetParamNamePathData('FPCMAKE',FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt);
          Processor.SetParamNamePathData('PPUMOVE',FPCBinDir+DirectorySeparator+'ppumove'+GetExeExt);
          Processor.SetParamNamePathData('PREFIX',InstallDirectory);

          Processor.SetParamNamePathData('INSTALL_PREFIX',InstallDirectory);
          Processor.SetParamNamePathData('INSTALL_SOURCEDIR',SourceDirectory);

          Processor.SetParamNamePathData('INSTALL_BASEDIR',FPCBaseDir);

          Processor.SetParamNamePathData('INSTALL_UNITDIR',GetUnitsInstallDirectory);
          Processor.SetParamNamePathData('INSTALL_BINDIR',FPCBinDir);

          Processor.SetParamNamePathData('INSTALL_LIBDIR',FPCLibraryDir);
          Processor.SetParamNamePathData('INSTALL_SHAREDDIR',FPCShareDir);
          Processor.SetParamNamePathData('INSTALL_DATADIR',FPCDataDir);
          Processor.SetParamNamePathData('INSTALL_DOCDIR',FPCDocDir);
          Processor.SetParamNamePathData('INSTALL_EXAMPLEDIR',FPCExampleDir);


          Processor.SetParamNameData('CPU_SOURCE',GetSourceCPU);
          Processor.SetParamNameData('OS_SOURCE',GetSourceOS);

          Processor.SetParamNameData('OS_TARGET',CrossInstaller.TargetOSName);
          Processor.SetParamNameData('CPU_TARGET',CrossInstaller.TargetCPUName);

          if (CrossInstaller.SubArch<>TSubarch.saNone) then Processor.SetParamNameData('SUBARCH',CrossInstaller.SubArchName);

          Processor.SetParamNameData('CROSSINSTALL','1');

          // The below is needed due to changes in the Makefile of FPC 3.3.1
          // Has been reported on the Core Mailing list, but no action taken
          if (CrossInstaller.TargetOS in [TOS.darwin]) then
          begin
            Processor.SetParamNamePathData('SYSTEMDIR',ConcatPaths([SourceDirectory,'rtl','bsd']));
            Processor.SetParamNamePathData('DOSDIR',ConcatPaths([SourceDirectory,'rtl','unix']));
            Processor.SetParamNamePathData('SYSUTILSDIR',ConcatPaths([SourceDirectory,'rtl','unix']));
            Processor.SetParamNamePathData('CLASSESDIR',ConcatPaths([SourceDirectory,'rtl','unix']));
            Processor.SetParamNamePathData('TTHREADINCDIR',ConcatPaths([SourceDirectory,'rtl','unix']));
          end;

          if (CrossInstaller.TargetOS in [TOS.android]) then
          begin
            Processor.SetParamNamePathData('DOSDIR',ConcatPaths([SourceDirectory,'rtl','unix']));
          end;

          if (MakeCycle in [st_RtlInstall,st_PackagesInstall]) then
          begin
            UnitSearchPath:=GetUnitsInstallDirectory+DirectorySeparator;
            if (MakeCycle=st_RtlInstall) then UnitSearchPath:=UnitSearchPath+'rtl';
            if (MakeCycle=st_PackagesInstall) then
            {$ifdef Windows}
            UnitSearchPath:=UnitSearchPath+'$$(packagename)';
            {$else}
            UnitSearchPath:=UnitSearchPath+'\$$\(packagename\)';
            {$endif}
            Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath);
          end;

          {$IFDEF MSWINDOWS}
          Processor.SetParamNameData('UPXPROG','echo'); //Don't use UPX
          (*
          // do we have a stray shell in the path ...
          if StrayShell then
          begin
            s1:=ExtractFilePath(Make)+'gecho.exe';
            if FileExists(s1) then Processor.SetParamNamePathData('ECHOREDIR',s1);
          end;
          *)
          //Processor.SetParamNameData('COPYTREE','echo'); //fix for examples in Win svn, see build FAQ
          // If we have a (forced) local GIT client, set GIT to prevent picking up a stray git in the path
          s1:=GitClient.RepoExecutable;
          if (Length(s1)>0) then Processor.SetParamNamePathData('GIT',s1);
          {$ENDIF}

          // Tell make where to find the target binutils if cross-compiling:
          // Not strictly necessary: the cross-options have this already:
          if (CrossInstaller.BinUtilsPath<>'') then
             Processor.SetParamNamePathData('CROSSBINDIR',CrossInstaller.BinUtilsPath);
          if (CrossInstaller.BinUtilsPrefix<>'') then
             Processor.SetParamNamePathData('BINUTILSPREFIX',CrossInstaller.BinUtilsPrefix);

          //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
          //Todo: to be investigated
          Processor.SetParamNamePathData('FPCFPMAKE',FCompiler);

          {$ifdef crosssimple}
          Processor.SetParamNamePathData('FPC',FCompiler);
          case MakeCycle of
            st_MakeAll:
            begin
              Processor.SetParamData('all');
            end;
            st_RtlInstall:
            begin
              Processor.SetParamData('installbase');
            end;
            st_PackagesInstall:
            begin
              Processor.SetParamData('installother');
            end;
          end;
          {$else crosssimple}
          case MakeCycle of
            st_Compiler:
            begin
              Processor.SetParamNamePathData('FPC',FCompiler);
              Processor.SetParamData('compiler_cycle');
            end;
            st_CompilerInstall:
            begin
              {$if (defined(Linux))}
              {$if (defined(CPUAARCH64)) OR (defined(CPUX86_64))}
              if FMUSL then
              begin
                // copy over the [cross-]compiler
                // is this still needed ?
                // to be investigated
                s1:=IncludeTrailingPathDelimiter(SourceDirectory)+'compiler/'+GetCompilerName(CrossInstaller.TargetCPU);
                s2:=IncludeTrailingPathDelimiter(SourceDirectory)+'compiler/'+CrossCompilerName;
                if FileExists(s1) then
                begin
                  Infoln(infotext+'Copy [cross-]compiler ('+ExtractFileName(s1)+') into: '+ExtractFilePath(s2),etInfo);
                  FileCopy(s1,s2);
                  fpChmod(s2,&755);
                end;
              end;
              {$endif}
              {$endif}
              Processor.SetParamNamePathData('FPC',FCompiler);
              Processor.SetParamData('compiler_install');
            end;
            st_RtlBuild,st_RtlInstall,st_PackagesBuild,st_PackagesInstall:
            begin
              s2:=FPCBinDir+DirectorySeparator+FPCCrossCompilerName;
              if (NOT FileExists(s2)) then
                s2:=ConcatPaths([SourceDirectory,'compiler',CrossCompilerName]);
              Processor.SetParamNamePathData('FPC',s2);
              case MakeCycle of
                st_RtlBuild           : s2:='rtl_all';
                st_RtlInstall         : s2:='rtl_install';
                st_PackagesBuild      : s2:='packages_all';
                st_PackagesInstall    : s2:='packages_install';
              end;
              Processor.SetParamData(s2);
            end;
            st_NativeCompiler:
            begin
              if (
                //Only native compiler if we have libs of if we do not need libs !!
                ( (CrossInstaller.LibsPath<>'') OR (CrossInstaller.TargetOS=TOS.win32) OR (CrossInstaller.TargetOS=TOS.win64))
                AND
                //Only native compiler for these OS
                (CrossInstaller.TargetOS in [TOS.win32,TOS.win64,TOS.linux,TOS.darwin,TOS.freebsd,TOS.openbsd,TOS.aix,TOS.haiku,TOS.solaris,TOS.dragonfly,TOS.netbsd,TOS.android])
                )
                then
              begin
                Infoln(infotext+'Building native compiler for '+CrossInstaller.TargetCPUName+'-'+CrossInstaller.TargetOSName+'.',etInfo);
                Processor.SetParamNamePathData('FPC',FCompiler);
                Processor.SetParamData('-C');
                Processor.SetParamData('compiler');
                Processor.SetParamData('compiler');
              end
              else
              begin
                Infoln(infotext+'Building native compiler for '+CrossInstaller.TargetCPUName+'-'+CrossInstaller.TargetOSName+' not implemented.',etInfo);
                continue;
              end;
            end;
            st_Start,st_Finished:
            begin
              // make compiler happy
            end;
          end;

          if (MakeCycle in [st_Compiler,st_CompilerInstall]) then
          begin
            // Do we need a new cross-compiler ?
            if (NOT CompilerUpdateNeeded) then
            begin
              if (MakeCycle=st_Compiler) then Infoln(infotext+'Skipping cross-compiler build step: compiler seems to be up to date !!',etInfo);
              continue; // best guess: compilers stem from identical sources, so do not build the cross-compiler again
            end;
          end;

          if (MakeCycle in [st_PackagesBuild,st_PackagesInstall{,st_NativeCompiler}]) then
          begin
            if (NOT PackagesNeeded) then continue;
          end;

          {$endif crosssimple}

          //Processor.SetParamNameData('OSTYPE',CrossInstaller.TargetOS);
          Processor.SetParamNameData('NOGDBMI','1'); // prevent building of IDE to be 100% sure

          NativeCompilerOptions:=FCompilerOptions;

          // Error checking for some known problems with cross compilers
          //todo: this really should go to the cross compiler unit itself but would require a rewrite
          if (CrossInstaller.TargetCPU=TCPU.i8086) and
            (CrossInstaller.TargetOS=TOS.msdos) then
          begin
            if (pos('-g',NativeCompilerOptions)>0) then
            begin
              Infoln(infotext+'Specified debugging FPC options: '+NativeCompilerOptions+'... However, this cross compiler does not support debug symbols. Aborting.',etError);
              exit(false);
            end;
          end;

          if ((CrossInstaller.TargetCPU=TCPU.wasm32) OR ((CrossInstaller.TargetCPU=TCPU.aarch64) AND (CrossInstaller.TargetOS=TOS.win64))) then
          begin
            // wasm only works with -O-
            i:=pos('-O',NativeCompilerOptions);
            if (i>0) then
            begin
              s2:=Copy(NativeCompilerOptions,i,3);
              if s2[3]<>'-' then
              begin
                Infoln(infotext+'Specified optimization: '+s2+'. Must be -O- for this target. Replacing.',etInfo);
                NativeCompilerOptions[i+2]:='-';
              end;
            end
            else
            begin
              NativeCompilerOptions:=NativeCompilerOptions+' -O-';
            end;
          end;

          if (CrossInstaller.TargetCPU=TCPU.wasm32) then
          begin
            // wasm: remove debugging settings
            i:=pos('-g',NativeCompilerOptions);
            while (i<>0) do
            begin
              s2:=Trim(Copy(NativeCompilerOptions,i,3));
              Infoln(infotext+'Specified debug option: '+s2+'. Removing for this target.',etInfo);
              Delete(NativeCompilerOptions,i,3);
              i:=pos('-g',NativeCompilerOptions);
            end;
            // wasm: remove assembler file settings
            i:=pos('-a',NativeCompilerOptions);
            while (i<>0) do
            begin
              s2:=Trim(Copy(NativeCompilerOptions,i,3));
              Infoln(infotext+'Specified assembler option: '+s2+'. Removing for this target.',etInfo);
              Delete(NativeCompilerOptions,i,3);
              i:=pos('-a',NativeCompilerOptions);
            end;
          end;

          {$ifdef FORCEREVISION}
          s2:=GetRevision(ModuleName);
          s2:=AnsiDequotedStr(s2,'''');
          if ( (Length(s2)>1) AND (s2<>'failure') AND (Pos(' ',s2)=0) ) then
          begin
            Processor.SetParamNameData('REVSTR',s2);
            Processor.SetParamNameData('REVINC','force');
          end;
          {$endif FORCEREVISION}

          {$ifdef solaris}
          {$IF defined(CPUX64) OR defined(CPUX86)}
          //Still not sure if this is needed
          //To be checked
          //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
          if (MakeCycle in [st_Compiler,st_CompilerInstall]) then
            NativeCompilerOptions:=NativeCompilerOptions+' -Xn';
          {$endif}
          {$endif}

          {$ifdef linux}
          if FMUSL then
          begin
            //if FileExists(IncludeTrailingPathDelimiter(CrossInstaller.LibsPath)+FMUSLLinker) then NativeCompilerOptions:=NativeCompilerOptions+' -FL'+FMUSLLinker;
          end;
          {$endif}

          {$ifdef Darwin}
          NativeCompilerOptions:=NativeCompilerOptions+' -ap';
          {$endif}

          {$ifndef FPC_HAS_TYPE_EXTENDED}
          // soft 80 bit float if available
          //if ((GetSourceCPU=GetCPU(TCPU.x86_64)) OR (GetSourceCPU=GetCPU(TCPU.aarch64))) then
          if (GetSourceCPU=GetCPU(TCPU.x86_64)) then
          begin
            if ( (CrossInstaller.TargetCPU=TCPU.i386) OR (CrossInstaller.TargetCPU=TCPU.i8086)  OR (CrossInstaller.TargetCPU=TCPU.x86_64) ) then
            begin
              if FSoftFloat then
              begin
                Infoln(infotext+'Adding -d'+DEFINE_SOFT_FPUX80+' to compiler options to enable 80bit (soft)float support.',etInfo);
                if (GetSourceCPU=GetCPU(TCPU.x86_64)) then Infoln(infotext+'This is needed due to the fact that FPC itself is also build with this option enabled.',etInfo);
                NativeCompilerOptions:=NativeCompilerOptions+' -d'+DEFINE_SOFT_FPUX80;
              end;
            end;
          end;
          {$endif}

          if fDelphiRTTI then
          begin
            Infoln(infotext+'Adding -d'+DEFINE_DELPHI_RTTI+' to compiler option to Delphi RTTI support (trunk only).',etInfo);
            NativeCompilerOptions:=NativeCompilerOptions+' -d'+DEFINE_DELPHI_RTTI;
          end;

          while Pos('  ',NativeCompilerOptions)>0 do
          begin
            NativeCompilerOptions:=StringReplace(NativeCompilerOptions,'  ',' ',[rfReplaceAll]);
          end;
          NativeCompilerOptions:=Trim(NativeCompilerOptions);

          NativeCompilerOptions:=STANDARDCOMPILERVERBOSITYOPTIONS+' '+NativeCompilerOptions;

          {$ifdef DEBUG}
          //NativeCompilerOptions:=NativeCompilerOptions+' -g -gl -dEXTDEBUG'; //-va+
          //NativeCompilerOptions:=NativeCompilerOptions+' -dEXTDEBUG'; //-va+
          {$endif}

          {$ifdef DARWIN}
          //{$if (defined(CPUAARCH64)) AND (defined(DARWIN))}
          s2:=GetDarwinSDKVersion('macosx');
          if  (Length(s2)=0) OR (CompareVersionStrings(s2,'10.14')>=0) then
          begin
            //if MakeCycle in [st_Compiler,st_Rtl,st_Packages] then
            begin
              s2:=GetDarwinSDKLocation;
              if Length(s2)>0 then
              begin
                NativeCompilerOptions:='-XR'+s2+' '+NativeCompilerOptions;
                NativeCompilerOptions:='-Fl'+s2+'/usr/lib '+NativeCompilerOptions;
              end;
            end;
          end;
          {$ENDIF}

          {$ifndef crosssimple}
          if (MakeCycle=st_NativeCompiler) then
          begin
            UnitSearchPath:=GetUnitsInstallDirectory;
            //NativeCompilerOptions:=NativeCompilerOptions+' -Fu'+UnitSearchPath;
            //NativeCompilerOptions:=NativeCompilerOptions+' -Fu'+UnitSearchPath+DirectorySeparator+'rtl';
            {$ifdef DEBUG}
            //NativeCompilerOptions:=NativeCompilerOptions+' -gw3 -gl';
            {$endif}
          end;
          {$endif}

          CrossCompilerOptions:='';
          {$ifndef crosssimple}
          if (MakeCycle<>st_NativeCompiler) then
          {$endif}
          begin
            //if LinuxLegacy then CrossInstaller.AddCrossOption('-XLC');
            if LinuxLegacy then CrossInstaller.AddFPCCFGSnippet('-XLC',True);
            // During a native install with libc, we add this define into the fpc.cfg
            // So, add it also as cross-config, however not 100% necessary
            //if UseLibc then CrossInstaller.AddCrossOption('-d'+DEFINE_FPC_LIBC);
            if UseLibc then CrossInstaller.AddFPCCFGSnippet('-d'+DEFINE_FPC_LIBC,True);
            for i:=0 to CrossInstaller.CrossOpt.Count-1 do
              CrossCompilerOptions:=CrossCompilerOptions+Trim(CrossInstaller.CrossOpt[i])+' ';
            CrossCompilerOptions:=TrimRight(CrossCompilerOptions);
          end;

          NativeCompilerOptions:=Trim(NativeCompilerOptions);
          if (Length(NativeCompilerOptions)>0) then Processor.SetParamNameData('OPT',{MaybeQuotedSpacesOnly}(NativeCompilerOptions));

          CrossCompilerOptions:=Trim(CrossCompilerOptions);
          if (Length(CrossCompilerOptions)>0) then Processor.SetParamNameData('CROSSOPT',{MaybeQuotedSpacesOnly}(CrossCompilerOptions));

          try
            s1:=infotext+'Running make ['+UnCamel(GetEnumNameSimple(TypeInfo(TSTEPS),Ord(MakeCycle)))+'] (FPC crosscompiler: '+CrossInstaller.RegisterName+')';
            if (Length(CrossCompilerOptions)>0) then s1:=s1+' with CROSSOPT: '+CrossCompilerOptions;
            Infoln(s1,etInfo);

            Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);

            ProcessorResult:=Processor.ExecuteAndWait;
            result:=(ProcessorResult=0);

            if ProcessorResult=AbortedExitCode then break;
          except
            on E: Exception do
            begin
              WritelnLog(infotext+'Running cross compiler fpc '+Processor.Executable+' generated an exception!'+LineEnding+'Details: '+E.Message,true);
              result:=false;
            end;
          end;

          if (not result) then break;

        end;// loop over MakeCycle

        if result then Infoln(infotext+'Building cross-compiler for '+GetFPCTarget(false)+' finished.',etInfo);

        if (not result) then
        begin
          // Not an error but warning for optional modules: crosswin32-64 and crosswin64-32
          // These modules need to be optional because FPC 2.6.2 gives an error crosscompiling regarding fpdoc.css or something.
          {$ifdef win32}
          // if this is crosswin32-64, ignore error as it is optional
          if (CrossInstaller.TargetCPU=TCPU.x86_64) and ((CrossInstaller.TargetOS=TOS.win64) or (CrossInstaller.TargetOS=TOS.win32)) then
            result:=true;
          {$endif win32}
          {$ifdef win64}
          // if this is crosswin64-32, ignore error as it is optional
          if (CrossInstaller.TargetCPU=TCPU.i386) and (CrossInstaller.TargetOS=TOS.win32) then
            result:=true;
          {$endif win64}
          if result then
            Infoln(infotext+'Running cross compiler fpc '+Processor.Executable+' for '+GetFPCTarget(false)+' failed with an error code. Optional module; continuing regardless.', etInfo)
          else
          begin
            Compiler := '////\\\Error trying to build cross-compiler \|!';
            Infoln(infotext+'Running cross compiler fpc '+Processor.Executable+' for '+GetFPCTarget(false)+' failed with an error code.',etError);
            // If we were building a crosscompiler itself when the failure occured, remove all fpc.cfg settings of this target
            {$ifdef crosssimple}
            if MakeCycle in [st_MakeAll] then
            {$else}
            if MakeCycle in [st_Compiler] then
            {$endif}
            begin
              Infoln(infotext+'Removing all '+GetFPCTarget(false)+' compiler settings from fpc.cfg.',etError);
              InsertFPCCFGSnippet(FPCCfg,'');
            end;
          end;

        end
        else
        begin
          if (ModuleName=_NATIVECROSSFPC) then
          begin
            s1:=GetCompilerName(CrossInstaller.TargetCPU);
            s1:=ChangeFileExt(s1,installerBase.GetExeExt(CrossInstaller.TargetOS));
            s2:=ConcatPaths([SourceDirectory,'compiler',s1]);
            if FileExists(s2) then
            begin
              s1:=ConcatPaths([FPCBinDir,'native_'+GetFPCTarget(false)+'_'+GetCompilerName(CrossInstaller.TargetCPU)]);
              s1:=ChangeFileExt(s1,installerBase.GetExeExt(CrossInstaller.TargetOS));
              SysUtils.DeleteFile(s1);
              FileCopy(s2,s1);
              //SysUtils.DeleteFile(s2);
            end;
          end;

          if (ModuleName=_FPC) then
          begin
            {$ifdef Darwin}
            if FPCCrossCompilerName=GetCompilerName(GetSourceCPU) then
            begin
              Infoln(infotext+'Cross-compiler and native compiler share the same name: '+FPCCrossCompilerName+'.',etInfo);
              Infoln(infotext+'Skipping manual compiler-rename.',etInfo);
              // Perhaps this compiler build-step needs to be skipped on Darwin if source and target CPUs are the same
              // Perhaps we need to contruct a "fat" binary
              // lipo -create s1 CrossCompilerName -output s1
              // TODO
            end
            else
            if (FPCCrossCompilerName<>CrossCompilerName) then
            begin
              // FPC itself uses a different naming scheme on Darwin
              // So we need to rename the cross-compiler to accomodate for this

              // Get the correct name of the cross-compiler in install-directory
              s1:=FPCBinDir+DirectorySeparator+FPCCrossCompilerName;
              // Get the name of the cross-compiler in install-directory
              s2:=FPCBinDir+DirectorySeparator+CrossCompilerName;
              // rename the cross-compiler.
              Infoln(infotext+'Rename cross-compiler ('+CrossCompilerName+') into '+FPCCrossCompilerName,etInfo);
              SysUtils.DeleteFile(s1);
              SysUtils.RenameFile(s2,s1);
              fpChmod(s1,&755);
            end;
            {$endif Darwin}
            // Get the name of the cross-compiler in source-directory
            s2:=ConcatPaths([SourceDirectory,'compiler',CrossCompilerName]);
            // delete cross-compiler in source-directory
            SysUtils.DeleteFile(s2);

            {$IFDEF UNIX}
            result:=CreateFPCScript;
            {$ENDIF UNIX}

            {$ifdef MSWINDOWS}
            // get wince debugger
            if (CrossInstaller.TargetCPU=TCPU.arm) AND (CrossInstaller.TargetOS=TOS.wince) then
            begin
              for Counter := low(FUtilFiles) to high(FUtilFiles) do
              begin
                if (FUtilFiles[Counter].Category=ucDebuggerWince) then
                begin
                  if NOT FileExists(MakePath+'gdb\arm-wince\gdb.exe') then
                  begin
                    s1:=GetTempFileNameExt('FPCUPTMP','zip');
                    if GetFile(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,s1) then
                    begin
                      with TNormalUnzipper.Create do
                      begin
                        try
                          if DoUnZip(s1,MakePath+'gdb\arm-wince\',[]) then
                            Infoln(localinfotext+'Downloading and installing GDB debugger (' + FUtilFiles[Counter].FileName + ') for WinCE success.',etInfo);
                        finally
                          Free;
                        end;
                      end;
                    end;
                    SysUtils.Deletefile(s1);
                  end;
                end;
              end;
            end;

            {$endif}

            // move debugger, if any
            //if (CrossInstaller.TargetCPU in TCPU.arm) AND (CrossInstaller.TargetOS=TOS.embedded) then
            begin
              if NOT FileExists(ConcatPaths([FMakeDir,'gdb',CrossInstaller.RegisterName,'gdb'+GetExeExt])) then
              begin
                //Get cross-binaries directory
                i:=Pos('-FD',CrossInstaller.FPCCFGSnippet);
                if i>0 then
                begin
                  j:=Pos(#13,CrossInstaller.FPCCFGSnippet,i);
                  if j=0 then j:=Pos(#10,CrossInstaller.FPCCFGSnippet,i);
                  s1:=Copy(CrossInstaller.FPCCFGSnippet,i+3,j-(i+3));
                  s1:=IncludeTrailingPathDelimiter(s1);
                  //Get cross-binaries prefix
                  i:=Pos('-XP',CrossInstaller.FPCCFGSnippet);
                  if i>0 then
                  begin
                    j:=Pos(#13,CrossInstaller.FPCCFGSnippet,i);
                    if j=0 then j:=Pos(#10,CrossInstaller.FPCCFGSnippet,i);
                    s2:=Copy(CrossInstaller.FPCCFGSnippet,i+3,j-(i+3));
                    s1:=s1+s2+'gdb'+GetExeExt;
                    if FileExists(s1) then
                    begin
                      s2:=ConcatPaths([FMakeDir,'gdb',CrossInstaller.RegisterName])+DirectorySeparator;
                      ForceDirectoriesSafe(s2);
                      {$ifdef Darwin}
                      SysUtils.RenameFile(s1,s2+ExtractFileName(s1));
                      s1:=s2+ExtractFileName(s1);
                      s2:=s2+'gdb'+GetExeExt;
                      fpSymlink(pchar(s1),pchar(s2));
                      {$else}
                      FileCopy(s1,s2+'gdb'+GetExeExt);
                      {$endif}
                    end;
                  end;
                end;
              end;
            end;
          end;

        end;
      end;
    end;

  end
  else
  begin
    Infoln(infotext+'Can''t find cross installer for '+GetFPCTarget(false)+' !!!',etError);
    result:=false;
  end;

end;

function TFPCCrossInstaller.UnInstallModule(ModuleName: string): boolean;
var
  aDir,FPCCfg :string;
  DirectoryAvailable:boolean;
begin
  result:=true; //succeed by default

  FErrorLog.Clear;

  if (NOT DirectoryExists(InstallDirectory)) then exit;
  if CheckDirectory(InstallDirectory) then exit;


  if assigned(CrossInstaller) AND (Length(BaseDirectory)>0) AND (NOT CheckDirectory(BaseDirectory,false)) then
  begin
    if ((CrossInstaller.TargetCPU=TCPU.cpuNone) OR (CrossInstaller.TargetOS=TOS.osNone)) then exit;

    CrossInstaller.Reset;
    CrossInstaller.SetFPCVersion(SourceVersionStr);

    DirectoryAvailable:=CrossInstaller.GetBinUtils(BaseDirectory);
    if DirectoryAvailable then
    begin
      aDir:=CrossInstaller.BinUtilsPath;
      if DirectoryExists(aDir) then
      begin
        if FileExists(IncludeTrailingPathDelimiter(aDir)+FPCUP_ACKNOWLEDGE) then
        begin
          // Only allow cross directories inside our own install te be deleted
          if (Pos(BaseDirectory,aDir)=1) AND  (Pos(CROSSBINPATH,aDir)>0) then
          begin
            Infoln(infotext+'Deleting '+ModuleName+' bin tools directory '+aDir);
            if DeleteDirectoryEx(aDir)=false then
            begin
              WritelnLog(infotext+'Error deleting '+ModuleName+' bin tools directory '+aDir);
            end;
          end;
        end;
      end;
    end;

    {
    DirectoryAvailable:=CrossInstaller.GetLibs(BaseDirectory);
    if DirectoryAvailable then
    begin
      aDir:=CrossInstaller.LibsPath;
      if DirectoryExists(aDir) then
      begin
        if FileExists(IncludeTrailingPathDelimiter(aDir)+FPCUP_ACKNOWLEDGE) then
        begin
          // Only allow cross directories inside our own install te be deleted
          if (Pos(BaseDirectory,aDir)=1) AND  (Pos(CROSSLIBPATH,aDir)>0) then
          begin
            Infoln(infotext+'Deleting '+ModuleName+' libs directory '+aDir);
            if DeleteDirectoryEx(aDir)=false then
            begin
              WritelnLog(infotext+'Error deleting '+ModuleName+' libs directory '+aDir);
            end;
          end;
        end;
      end;
    end;
    }

    FPCCfg:=GetFPCConfigPath(FPCCONFIGFILENAME);

    InsertFPCCFGSnippet(FPCCfg,'');

    aDir:=IncludeTrailingPathDelimiter(InstallDirectory)+'bin'+DirectorySeparator+GetFPCTarget(false);
    if DirectoryExists(aDir) then
    begin
      // Only allow binary directories inside our own install te be deleted
      if (Pos(BaseDirectory,aDir)=1) then
      begin
        Infoln(infotext+'Deleting '+ModuleName+' binary directory '+aDir);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' binary directory '+aDir);
        end;
      end;
    end;

    aDir:=GetUnitsInstallDirectory;
    if DirectoryExists(aDir) then
    begin
      // Only allow unit directories inside our own install te be deleted
      if (Pos(BaseDirectory,aDir)=1) then
      begin
        Infoln(infotext+'Deleting '+ModuleName+' unit directory '+aDir);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' unit directory '+aDir);
        end;
      end;
    end;

    // Delete dummy system.ppu
    //if (CrossInstaller.TargetOS in [TOS.embedded,TOS.freertos]) then
    //  SysUtils.DeleteFile(ConcatPaths([GetUnitsInstallDirectory,'system.ppu']));

  end;
end;

{ TFPCNativeInstaller }
function TFPCNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
const
  YYLEX='yylex.cod';
  YYPARSE='yyparse.cod';
var
  OperationSucceeded:boolean;
  i,MakeCommandIndex:integer;
  UnitSearchPath:string;
  FPCBuildOptions:string;
  s1,s2:string;
  {$IFDEF UNIX}
  //s3:string;
  {$ENDIF}
  //FPCDirStore:string;
begin
  result:=inherited;
  OperationSucceeded:=true;

  s1:=CompilerVersion(FCompiler);

  if (ModuleName=_FPC) then
  begin
    if (s1<>'0.0.0')
      then Infoln(infotext+'Using FPC bootstrap compiler with version: '+s1, etInfo)
      else Infoln(infotext+'FPC bootstrap version error: '+s1+' ! Should never happen: expect many errors !!', etError);
  end;

  //if clean failed (due to missing compiler), try again !
  if (NOT FCleanModuleSuccess) then
  begin
    if ((ModuleName=_FPC) OR (ModuleName=_PAS2JS)) then
    begin
      Infoln(infotext+'Running CleanModule once more, due to previous clean failure.',etInfo);
      CleanModule(ModuleName);
      // Restore infotext
      infotext:=InitInfoText(' (BuildModule: '+ModuleName+'): ');
    end;
  end;

  if (ModuleName=_FPC) then
  //if (false) then
  begin
    //Sometimes, during build, we get an error about missing yylex.cod and yyparse.cod.
    //Copy them now, just to be sure
    ForceDirectoriesSafe(FPCBinDir);
    s2:=IncludeTrailingPathDelimiter(SourceDirectory)+'utils'+DirectorySeparator+'tply';
    s1:=FPCBinDir+DirectorySeparator+YYLEX;
    if (NOT FileExists(s1)) then FileCopy(s2+DirectorySeparator+YYLEX,s1);
    s1:=FPCBinDir+DirectorySeparator+YYPARSE;
    if (NOT FileExists(s1)) then FileCopy(s2+DirectorySeparator+YYPARSE,s1);

    {$IFDEF UNIX}
    (*
    s1:=ConcatPaths([InstallDirectory,'lib','fpc',SourceVersionStr]);
    ForceDirectoriesSafe(s1);
    s1:=s1+'/lexyacc';
    DeleteFile(s1);
    s2:=IncludeTrailingPathDelimiter(InstallDirectory)+'lib/fpc/lexyacc';
    ForceDirectoriesSafe(s2);
    fpSymlink(pchar(s2),pchar(s1));

    s1:=IncludeTrailingPathDelimiter(SourceDirectory)+'utils'+DirectorySeparator+'tply';
    s3:=s2+DirectorySeparator+YYLEX;
    if (NOT FileExists(s3)) then FileCopy(s1+DirectorySeparator+YYLEX,s3);
    s3:=s2+DirectorySeparator+YYPARSE;
    if (NOT FileExists(s3)) then FileCopy(s1+DirectorySeparator+YYPARSE,s3);
    *)
    {$ENDIF UNIX}
  end;

  if (ModuleName=_FPC) then
  begin
    if (Length(ActualRevision)=0) OR (ActualRevision='failure') then
    begin
      s1:=GetRevision(ModuleName);
      if Length(s1)>0 then FActualRevision:=s1;
    end;
    Infoln(infotext+'Now building '+ModuleName,etInfo);
  end;

  Processor.Executable := Make;
  Processor.Process.Parameters.Clear;
  {$IFDEF MSWINDOWS}
  if Length(Shell)>0 then Processor.SetParamNameData('SHELL',Shell);
  {$ENDIF}
  FErrorLog.Clear;


  if (NOT FNoJobs) then
  begin
    {$ifndef win64}
    Processor.SetParamNameData('--jobs',IntToStr(FCPUCount));
    {$endif win64}
    Processor.SetParamNameData('FPMAKEOPT','--threads='+IntToStr(FCPUCount));
  end;

  //Processor.SetParamNamePathData('FPC',FCompiler);
  Processor.SetParamNamePathData('PP',FCompiler);

  //Sometimes, during build, we get an error about missing yylex.cod and yyparse.cod.
  //The paths are fixed in the FPC sources. Try to set the default path here [FPCDIR], so yylex.cod and yyparse.cod can be found.
  Processor.SetParamNamePathData('FPCDIR',SourceDirectory);

  {$IFDEF DEBUG}
  //To debug Makefile itself
  //Processor.SetParamData('-d');
  {$ENDIF}

  Processor.SetParamNamePathData('FPCMAKE',FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt);
  Processor.SetParamNamePathData('PPUMOVE',FPCBinDir+DirectorySeparator+'ppumove'+GetExeExt);
  Processor.SetParamNamePathData('PREFIX',InstallDirectory);

  Processor.SetParamNamePathData('INSTALL_PREFIX',InstallDirectory);
  Processor.SetParamNamePathData('INSTALL_SOURCEDIR',SourceDirectory);

  Processor.SetParamNamePathData('INSTALL_BASEDIR',FPCBaseDir);

  Processor.SetParamNamePathData('INSTALL_UNITDIR',GetUnitsInstallDirectory);
  Processor.SetParamNamePathData('INSTALL_BINDIR',FPCBinDir);

  Processor.SetParamNamePathData('INSTALL_LIBDIR',FPCLibraryDir);
  Processor.SetParamNamePathData('INSTALL_SHAREDDIR',FPCShareDir);
  Processor.SetParamNamePathData('INSTALL_DATADIR',FPCDataDir);
  Processor.SetParamNamePathData('INSTALL_DOCDIR',FPCDocDir);
  Processor.SetParamNamePathData('INSTALL_EXAMPLEDIR',FPCExampleDir);


  Processor.SetParamNameData('OS_SOURCE',GetSourceOS);
  Processor.SetParamNameData('CPU_SOURCE',GetSourceCPU);

  Processor.SetParamNameData('OS_TARGET',GetSourceOS);
  Processor.SetParamNameData('CPU_TARGET',GetSourceCPU);

  {$IFDEF MSWINDOWS}
  Processor.SetParamNameData('UPXPROG','echo'); //Don't use UPX
  //Processor.SetParamNameData('COPYTREE','echo'); //fix for examples in Win svn, see build FAQ

  // If we have a (forced) local GIT client, set GIT to prevent picking up a stray git in the path
  s1:=GitClient.RepoExecutable;
  if (Length(s1)>0) then Processor.SetParamNamePathData('GIT',s1);
  {$ENDIF}

  if (SourceVersionNum<CalculateFullVersion(2,4,4)) then
    Processor.SetParamNameData('DATA2INC','echo');
  {else
    Processor.SetParamNamePathData('DATA2INC',FPCBinPath+'data2inc'+GetExeExt);}

  if BootstrapCompilerOverrideVersionCheck then
  begin
    Processor.SetParamNameData('OVERRIDEVERSIONCHECK','1');
    if (ModuleName=_FPC) then Infoln(infotext+'Adding OVERRIDEVERSIONCHECK=1 due to wrong version of bootstrapper !!',etWarning);
  end;

  FPCBuildOptions:=STANDARDCOMPILERVERBOSITYOPTIONS+' '+FCompilerOptions;
  while Pos('  ',FPCBuildOptions)>0 do
  begin
    FPCBuildOptions:=StringReplace(FPCBuildOptions,'  ',' ',[rfReplaceAll]);
  end;
  FPCBuildOptions:=Trim(FPCBuildOptions);

  {$IFDEF UNIX}
  FPCBuildOptions:='-Sg '+FPCBuildOptions;
  {$IFDEF SOLARIS}
  {$IF defined(CPUX64) OR defined(CPUX86)}
  //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
  FPCBuildOptions:='-Xn '+FPCBuildOptions;
  {$endif}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
  if FMUSL then FPCBuildOptions:='-FL'+FMUSLLinker+' '+FPCBuildOptions;
  {$ENDIF}

  {$IFDEF DARWIN}
  //Add minimum required OSX version to prevent "crti not found" errors.
  s2:=GetDarwinSDKVersion('macosx');
  if CompareVersionStrings(s2,'10.9')>=0 then
  begin
    s2:='10.9';
  end;
  if Length(s2)>0 then
  begin
    FPCBuildOptions:='-WM'+s2+' '+FPCBuildOptions;
    {
    if CompareVersionStrings(s2,'10.14')>=0 then
    begin
      FPCBuildOptions:='-Fl/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib '+FPCBuildOptions;
    end;
    }
  end;

  s2:=Which('codesign');
  if (NOT FileExists(s2)) then Processor.SetParamNameData('CODESIGN','/usr/bin/true');

  s2:=GetDarwinSDKLocation;
  if Length(s2)>0 then
  begin
    FPCBuildOptions:='-XR'+s2+' '+FPCBuildOptions;
    FPCBuildOptions:='-Fl'+s2+'/usr/lib '+FPCBuildOptions;
  end
  else
  begin
    // always add the default library location
    FPCBuildOptions:='-Fl'+'/usr/lib '+FPCBuildOptions;
  end;
  {$ENDIF}

  {$ifdef FORCEREVISION}
  if (ModuleName<>_REVISIONFPC) then
  begin
    if FUseRevInc then
    begin
      s2:=ConcatPaths([SourceDirectory,'compiler'])+DirectorySeparator+REVINCFILENAME;
      if FileExists(s2) then FPCBuildOptions:=FPCBuildOptions+' -dREVINC';
    end
    else
    begin
      s2:=Trim(ActualRevision);
      s2:=AnsiDequotedStr(s2,'''');
      if ( (Length(s2)>1) AND (s2<>'failure') AND (Pos(' ',s2)=0) ) then
      begin
        Processor.SetParamNameData('REVSTR',s2);
        Processor.SetParamNameData('REVINC','force');
      end;
    end;
  end;
  {$endif FORCEREVISION}

  {$ifndef FPC_HAS_TYPE_EXTENDED}
  if (GetSourceCPU=GetCPU(TCPU.x86_64)) then
  begin
    if FSoftFloat then
    begin
      // soft 80 bit float if available
      if (ModuleName=_FPC) then // only info when building real FPC
        Infoln(infotext+'Adding -d'+DEFINE_SOFT_FPUX80+' to compiler option to enable 80bit (soft)float support (trunk only).',etInfo);
      FPCBuildOptions:=FPCBuildOptions+' -d'+DEFINE_SOFT_FPUX80;
    end;
  end;
  {$endif}

  if fDelphiRTTI then
  begin
    if (ModuleName=_FPC) then // only info when building real FPC
      Infoln(infotext+'Adding -d'+DEFINE_DELPHI_RTTI+' to compiler option to Delphi RTTI support (trunk only).',etInfo);
    FPCBuildOptions:=FPCBuildOptions+' -d'+DEFINE_DELPHI_RTTI;
  end;

  {$ifdef BSD}
    {$ifndef DARWIN}
      FPCBuildOptions:=FPCBuildOptions+' -Fl/usr/pkg/lib';
    {$endif}
  {$endif}

  if UseLibc then FPCBuildOptions:=FPCBuildOptions+' -d'+DEFINE_FPC_LIBC;

  {$ifdef Haiku}
    s2:='';
    {$ifdef CPUX86}
    s2:='/x86';
    {$endif}
    FPCBuildOptions:=FPCBuildOptions+' -XR/boot/system/lib'+s2+' -FD/boot/system/bin'+s2+'/ -Fl/boot/system/develop/lib'+s2;
  {$endif}

  FPCBuildOptions:=Trim(FPCBuildOptions);
  Processor.SetParamNameData('OPT',FPCBuildOptions);

  Processor.Process.CurrentDirectory:='';
  case ModuleName of
    _REVISIONFPC:
    begin
      Processor.Process.CurrentDirectory:=ConcatPaths([SourceDirectory,'compiler']);
    end;
    _UNICODEFPC:
    begin
      Processor.Process.CurrentDirectory:=ConcatPaths([SourceDirectory,'rtl']);
    end;
    _PAS2JS:
    begin
      Processor.Process.CurrentDirectory:=ConcatPaths([SourceDirectory,'utils','pas2js']);
      // first run fpcmake to generate correct makefile
      // is this still needed !!?? No !!
      //SysUtils.DeleteFile(IncludeTrailingPathDelimiter(Processor.Process.CurrentDirectory)+'fpmake'+GetExeExt);
      //ExecuteCommandInDir(FPCBinPath+'fpcmake'+GetExeExt,Processor.Process.CurrentDirectory,FVerbose);
    end
    else
    begin
      Processor.Process.CurrentDirectory:=SourceDirectory;
    end;
  end;

  if (Length(Processor.Process.CurrentDirectory)=0) OR (NOT DirectoryExists(Processor.Process.CurrentDirectory)) then
  begin
    Processor.SetParamData('--help'); // this should render make harmless
    WritelnLog(etError, infotext+'Invalid module name [' + ModuleName + '] specified! Please fix the code.', true);
    OperationSucceeded := false;
    Result := false;
    exit;
  end;

  Processor.SetParamNameData('--directory',Processor.Process.CurrentDirectory);

  if ModuleName=_MAKEFILECHECKFPC then
  begin
    Processor.SetParamData('fpc_baseinfo');
  end
  else
  if ModuleName=_REVISIONFPC then
  begin
    Processor.SetParamData('revision');
  end
  else
  if ModuleName=_UNICODEFPC then
  begin
    Processor.SetParamNameData('SUB_TARGET','unicodertl');
    Processor.SetParamData('all');
  end
  else
  begin
    Processor.SetParamData('all');
  end;

  Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);

  try
    ProcessorResult:=Processor.ExecuteAndWait;
    if (ProcessorResult<>0) then
    begin
      OperationSucceeded := False;
      WritelnLog(etError, infotext+'Error running '+Processor.Executable+' for '+ModuleName+' failed with exit code '+IntToStr(ProcessorResult)+LineEnding+'. Details: '+FErrorLog.Text,true);
    end;
  except
    on E: Exception do
    begin
      OperationSucceeded := False;
      WritelnLog(etError, infotext+'Running fpc '+Processor.Executable+' for '+ModuleName+' failed with an exception!'+LineEnding+'. Details: '+E.Message,true);
    end;
  end;

  // Building of FPC finished

  if (OperationSucceeded AND ((ModuleName=_FPC) OR (ModuleName=_UNICODEFPC))) then
  begin

    // The last command added into the parameters is the make instruction
    // We need to change this instruction without changing anything else
    // So get its MakeCommandIndex. Bit tricky.
    MakeCommandIndex:=Pred(Processor.Process.Parameters.Count);

    // Building of FPC succeeded
    // Now install all binaries and units
    UnitSearchPath:=GetUnitsInstallDirectory+DirectorySeparator;
    if OperationSucceeded then
    begin
      Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath+'rtl');
      Processor.Process.Parameters.Strings[MakeCommandIndex]:='installbase';
      Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
      ProcessorResult:=Processor.ExecuteAndWait;
      OperationSucceeded:=(ProcessorResult=0);
    end;

    {$ifdef UNIX}
    // On Unix, the messages are now installed into a undesired directory (fpcbindir).
    // Move them to the root dir where they are expected to be by the auto-generated fpc.cfg
    if DirectoryExists(FPCMessageDir) then
    begin
      DirCopy(FPCMessageDir,InstallDirectory+DirectorySeparator+'msg');
      if (NOT CheckDirectory(FPCMessageDir)) then DeleteDirectory(FPCMessageDir,False);
    end;
    {$endif}

    if OperationSucceeded then
    begin
      {$ifdef Windows}
      Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath+'$$(packagename)');
      {$else}
      Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath+'\$$\(packagename\)');
      {$endif}
      Processor.Process.Parameters.Strings[MakeCommandIndex]:='installother';
      Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
      ProcessorResult:=Processor.ExecuteAndWait;
      OperationSucceeded:=(ProcessorResult=0);
    end;

    {$IFDEF UNIX}
    if OperationSucceeded then OperationSucceeded:=CreateFPCScript;
    {$ENDIF UNIX}

    // Let everyone know of our shiny new compiler:
    if OperationSucceeded then
    begin
      Compiler:=GetFPCInBinDir;
      // Verify it exists
      if not(FileExists(FCompiler)) then
      begin
        WritelnLog(etError, infotext+'Could not find compiler '+FCompiler+' that should have been created.',true);
        OperationSucceeded:=false;
      end;
    end
    else
    begin
      Infoln(infotext+'Error trying to compile FPC.',etDebug);
      Compiler:='////\\\Error trying to compile FPC\|!';
    end;

    if OperationSucceeded then
    begin
      if LinuxLegacy then
      begin
        Infoln(infotext+'Rebuilding RTL and Packages for legacy Linux.',etInfo);

        // Change some settings for this special legacy linking
        Processor.SetParamNamePathData('PP',ConcatPaths([FFPCSourceDir,'compiler',GetCompilerName(GetSourceCPU)]));
        Processor.SetParamNameData('OPT','-XLC '+'-d'+DEFINE_FPC_LIBC+' '+FPCBuildOptions);

        // Cleanup rtl and packages for legacy GLIBC
        Processor.Process.Parameters.Strings[MakeCommandIndex]:='rtl_clean';
        ProcessorResult:=Processor.ExecuteAndWait;
        OperationSucceeded:=(ProcessorResult=0);
        Processor.Process.Parameters.Strings[MakeCommandIndex]:='packages_clean';
        ProcessorResult:=Processor.ExecuteAndWait;
        OperationSucceeded:=(ProcessorResult=0);

        // Rebuild rtl and packages
        Processor.Process.Parameters.Strings[MakeCommandIndex]:='rtl_all';
        Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
        ProcessorResult:=Processor.ExecuteAndWait;
        OperationSucceeded:=(ProcessorResult=0);
        Processor.Process.Parameters.Strings[MakeCommandIndex]:='packages_all';
        Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
        ProcessorResult:=Processor.ExecuteAndWait;
        OperationSucceeded:=(ProcessorResult=0);

        if OperationSucceeded then
        begin
          Infoln(infotext+'Installing RTL and Packages for legacy Linux.',etInfo);

          UnitSearchPath:=GetUnitsInstallDirectory+'_legacy'+DirectorySeparator;

          Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath+'rtl');
          Processor.Process.Parameters.Strings[MakeCommandIndex]:='rtl_install';
          Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
          ProcessorResult:=Processor.ExecuteAndWait;
          OperationSucceeded:=(ProcessorResult=0);

          {$ifdef Windows}
          Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath+'$$(packagename)');
          {$else}
          Processor.SetParamNamePathData('INSTALL_UNITDIR',UnitSearchPath+'\$$\(packagename\)');
          {$endif}
          Processor.Process.Parameters.Strings[MakeCommandIndex]:='installother';
          Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
          ProcessorResult:=Processor.ExecuteAndWait;
          OperationSucceeded:=(ProcessorResult=0);
        end;
      end;
    end;

    {$IFDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      //Copy over binutils to new CompilerName bin directory
      try
        for i:=low(FUtilFiles) to high(FUtilFiles) do
        begin
          if FUtilFiles[i].Category=ucBinutil then
            FileCopy(MakePath+FUtilFiles[i].FileName,
              FPCBinDir+DirectorySeparator+FUtilFiles[i].FileName);
        end;
        // Also, we can change the make/binutils path to our new environment
        // Will modify fmake as well.
        MakeDirectory:=FPCBinDir;
      except
        on E: Exception do
        begin
          WritelnLog(infotext+'Error copying binutils: '+E.Message,true);
          OperationSucceeded:=false;
        end;
      end;
    end;
    {$ENDIF MSWINDOWS}

  end;

  result:=OperationSucceeded;
end;

constructor TFPCNativeInstaller.Create;
begin
  inherited Create;
end;

destructor TFPCNativeInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TFPCInstaller }

function TFPCInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
  infotext:=InitInfoText(' (BuildModuleCustom: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);
end;

function TFPCInstaller.GetCompilerTargetOS(CompilerPath: string): string;
var
  Output: string;
begin
  Result:='unknown';
  if CompilerPath='' then exit;
  try
    Output:='';
    if (ExecuteCommand(CompilerPath+ ' -iTO', Output, FVerbose)=0) then
    begin
      Output:=TrimRight(Output);
      if Length(Output)>0 then Result:=Output;
    end;
  except
  end;
end;

function TFPCInstaller.GetCompilerTargetCPU(CompilerPath: string): string;
var
  Output: string;
begin
  Result:='unknown';
  if CompilerPath='' then exit;
  try
    Output:='';
    if (ExecuteCommand(CompilerPath+ ' -iTP', Output, FVerbose)=0) then
    begin
      Output:=TrimRight(Output);
      if Length(Output)>0 then Result:=Output;
    end;
  except
  end;
end;

function TFPCInstaller.GetCompilerVersionNumber(aVersion: string; const index:byte=0): integer;
var
  Major,Minor,Build,Patch: Integer;
begin
  result:=-1;
  Major:=-1;
  Minor:=-1;
  Build:=-1;
  Patch:=-1;
  VersionFromString(aVersion,Major,Minor,Build,Patch);
  if index=0 then result:=Major;
  if index=1 then result:=Minor;
  if index=2 then result:=Build;
end;

function TFPCInstaller.CleanExtra(aCPU:TCPU;aOS:TOS):boolean;
var
  aDir,sArch,sOS  : string;
  DeleteList      : TStringList;
begin
  result:=true;

  if ((aCPU=TCPU.cpuNone) AND (aOS=TOS.osNone)) then
  begin
    sArch:=GetFPCTarget(true);
    sOS:=GetSourceOS;
  end
  else
  if ((aCPU<>TCPU.cpuNone) AND (aOS<>TOS.osNone)) then
  begin
    sArch:=GetCPU(aCPU)+'-'+GetOS(aOS);
    sOS:=GetOS(aOS);
  end
  else
  begin
    Infoln(infotext+'CleanExtra: wrong CPU or OS.');
    exit;
  end;

  //if (SourceDirectory<>InstallDirectory) then
  begin
    Infoln(infotext+'Removal of stale build files and directories for '+sArch+'. May take a while.');

    DeleteFilesNameSubdirs(SourceDirectory,'.stackdump');
    DeleteFilesNameSubdirs(SourceDirectory,'.core');

    // patch residues
    //DeleteFilesNameSubdirs(SourceDirectory,'.rej');
    //DeleteFilesNameSubdirs(SourceDirectory,'.orig');

    aDir:=IncludeTrailingPathDelimiter(SourceDirectory)+'utils';
    DeleteFilesNameSubdirs(aDir,sArch+'.fpm');
    DeleteFilesNameSubdirs(aDir,'-'+sOS+'.fpm');
    aDir:=IncludeTrailingPathDelimiter(SourceDirectory)+'packages';
    DeleteFilesNameSubdirs(aDir,sArch+'.fpm');
    DeleteFilesNameSubdirs(aDir,'-'+sOS+'.fpm');

    // Delete stray compilers, if any !!
    aDir:=ConcatPaths([SourceDirectory,'compiler']);
    if (IsCross) then
    begin
      if Assigned(CrossInstaller) then DeleteFile(aDir+DirectorySeparator+GetCrossCompilerName(CrossInstaller.TargetCPU));
    end
    else
      DeleteFile(aDir+DirectorySeparator+GetCompilerName(GetSourceCPU));

    DeleteFile(aDir+DirectorySeparator+'ppc'+GetExeExt);
    DeleteFile(aDir+DirectorySeparator+'ppc1'+GetExeExt);
    DeleteFile(aDir+DirectorySeparator+'ppc2'+GetExeExt);
    DeleteFile(aDir+DirectorySeparator+'ppc3'+GetExeExt);

    DeleteFile(aDir+DirectorySeparator+'ppcwpo1'+GetExeExt);
    DeleteFile(aDir+DirectorySeparator+'ppcwpo2'+GetExeExt);

    DeleteFile(aDir+DirectorySeparator+'pp1.wpo');
    DeleteFile(aDir+DirectorySeparator+'pp2.wpo');

    aDir:=ConcatPaths([SourceDirectory,'utils','bin']);
    DeleteDirectoryEx(aDir);
    aDir:=ConcatPaths([SourceDirectory,'packages','ide','bin']);
    DeleteDirectoryEx(aDir);

    DeleteList := TStringList.Create;
    try
      DeleteList.Add('.ppu');
      DeleteList.Add('.a');
      DeleteList.Add('.o');
      DeleteList.Add('.rsj');
      {$ifdef MSWINDOWS}
      DeleteList.Add('.exe');
      {$ENDIF}
      if (Pos('-a',FCompilerOptions)=0) then DeleteList.Add('.s');
      aDir:=ConcatPaths([SourceDirectory,'compiler','utils']);
      DeleteFilesExtensionsSubdirs(aDir,DeleteList,'units'+DirectorySeparator+sArch);
      aDir:=ConcatPaths([SourceDirectory,'packages']);
      DeleteFilesExtensionsSubdirs(aDir,DeleteList,'units'+DirectorySeparator+sArch);
      aDir:=ConcatPaths([SourceDirectory,'rtl']);
      DeleteFilesExtensionsSubdirs(aDir,DeleteList,'units'+DirectorySeparator+sArch);
      aDir:=ConcatPaths([SourceDirectory,'utils']);
      DeleteFilesExtensionsSubdirs(aDir,DeleteList,'units'+DirectorySeparator+sArch);
      DeleteFilesExtensionsSubdirs(aDir,DeleteList,'bin'+DirectorySeparator+sArch);

      {$ifdef MSWINDOWS}
      // On Unix, its hard to find executables, as they (might) have no extension
      // So skip this on Unix
      DeleteList.Clear;
      DeleteList.Add('.exe');
      aDir:=ConcatPaths([SourceDirectory,'compiler','utils']);
      DeleteFilesExtensionsSubdirs(aDir,DeleteList,'');
      {$ENDIF}

    finally
      DeleteList.Free;
    end;
  end;
  Infoln(infotext+'Search and removal of stale build files and directories ready.',etDebug);
  WritelnLog(infotext+'Update/build/config succeeded.',false);
end;

procedure TFPCInstaller.SetFPCInstallDirectory(value:string);
begin
  inherited;
  // Extra settings, mostly for the FPC makefile
  FFPCUnitDir:=ConcatPaths([FFPCInstallDir,'units']);
  FFPCLibraryDir:=ConcatPaths([FFPCInstallDir,'lib']);
  FFPCShareDir:=ConcatPaths([FFPCInstallDir,'share']);
  FFPCDataDir:=ConcatPaths([FFPCInstallDir,'data']);
  {$ifdef Windows}
  FFPCBaseDir:=FFPCInstallDir;
  FFPCDocDir:='';
  FFPCExampleDir:='';
  {$else}
  FFPCBaseDir:=FPCBinDir;
  FFPCDocDir:=ConcatPaths([FFPCInstallDir,'doc']);
  FFPCExampleDir:=ConcatPaths([FFPCInstallDir,'examples']);
  {$endif}
  FFPCMessageDir:=ConcatPaths([FFPCBaseDir,'msg']);
end;

function TFPCInstaller.GetUnitsInstallDirectory(const WithMagic:boolean):string;
var
  aDir:string;
  ABIMagic:string;
  SUBARCHMagic:string;
begin
  // Standard directory
  aDir:=ConcatPaths([FFPCInstallDir,'units']);

  {$ifdef UNIX}
  if FileIsSymlink(aDir) then
  begin
    try
      aDir:=GetPhysicalFilename(aDir,pfeException);
    except
    end;
  end;
  {$endif UNIX}

  result:=ConcatPaths([aDir,GetFPCTarget(NOT IsCross)]);

  if (NOT IsCross) then exit;

  with (Self as TFPCCrossInstaller) do
  // Specials
  if (SubarchTarget) then
  begin
    if WithMagic then
    begin
      ABIMagic:=FPC_ABI_MAGIC;
      SUBARCHMagic:=FPC_SUBARCH_MAGIC;
    end
    else
    begin
      ABIMagic:=CrossInstaller.ABIName;
      SUBARCHMagic:=CrossInstaller.SubArchName;
    end;

    if (CrossInstaller.TargetOS=TOS.ultibo) then
      result:=ConcatPaths([aDir,SUBARCHMagic+'-'+CrossInstaller.TargetOSName])
      //result:=ConcatPaths([result,SUBARCHMagic])
    else
    begin
      if CrossInstaller.TargetCPU=TCPU.arm then
        result:=ConcatPaths([result,SUBARCHMagic,ABIMagic])
      else
        result:=ConcatPaths([result,SUBARCHMagic]);
    end;
  end;
end;


function TFPCInstaller.GetVersionFromUrl(aUrl: string): string;
var
  aVersion: string;
begin
  aVersion:=VersionFromUrl(aUrl);
  if aVersion='trunk' then
    result:=FPCTRUNKVERSION
  else
    result:=aVersion;
end;

function TFPCInstaller.GetVersionFromSource: string;
const
  VNO  = 'version_nr';
  RNO  = 'release_nr';
  PNO  = 'patch_nr';
  //MPNO = 'minorpatch';
  MAKEVERSION='version=';
  //MAKEVERSION='PACKAGE_VERSION=';
var
  TxtFile:Text;
  version_nr:string;
  release_nr:string;
  build_nr:string;
  //minorbuild_nr:string;
  found_version_nr:boolean;
  found_release_nr:boolean;
  found_build_nr:boolean;
  //found_minorbuild_nr:boolean;
  s:string;
  x,y:integer;
begin
  result := '0.0.0';

  if (NOT DirectoryExists(SourceDirectory)) then exit;
  if DirectoryIsEmpty(SourceDirectory) then exit;

  version_nr:='';
  release_nr:='';
  build_nr:='';
  //minorbuild_nr:='';

  found_version_nr:=false;
  found_release_nr:=false;
  found_build_nr:=false;
  //found_minorbuild_nr:=false;

  s:=IncludeTrailingPathDelimiter(SourceDirectory) + 'compiler' + DirectorySeparator + 'version.pas';
  if FileExists(s) then
  begin

    AssignFile(TxtFile,s);
    Reset(TxtFile);
    while NOT EOF (TxtFile) do
    begin
      Readln(TxtFile,s);

      x:=Pos(VNO,s);
      if x>0 then
      begin
        y:=x+Length(VNO);
        // move towards first numerical
        while (Length(s)>=y) AND (NOT (s[y] in ['0'..'9'])) do Inc(y);
        // get version
        while (Length(s)>=y) AND (s[y] in ['0'..'9']) do
        begin
          version_nr:=version_nr+s[y];
          found_version_nr:=true;
          Inc(y);
        end;
      end;

      x:=Pos(RNO,s);
      if x>0 then
      begin
        y:=x+Length(RNO);
        // move towards first numerical
        while (Length(s)>=y) AND (NOT (s[y] in ['0'..'9'])) do Inc(y);
        // get version
        while (Length(s)>=y) AND (s[y] in ['0'..'9']) do
        begin
          release_nr:=release_nr+s[y];
          found_release_nr:=true;
          Inc(y);
        end;
      end;

      x:=Pos(PNO,s);
      if x>0 then
      begin
        y:=x+Length(PNO);
        // move towards first numerical
        while (Length(s)>=y) AND (NOT (s[y] in ['0'..'9'])) do Inc(y);
        // get version
        while (Length(s)>=y) AND (s[y] in ['0'..'9']) do
        begin
          build_nr:=build_nr+s[y];
          found_build_nr:=true;
          Inc(y);
        end;
      end;

      {
      x:=Pos(MPNO,s);
      if x>0 then
      begin
        y:=x+Length(MPNO);
        // move towards first numerical
        while (Length(s)>=y) AND (NOT (s[y] in ['0'..'9'])) do Inc(y);
        // get version
        while (Length(s)>=y) AND (s[y] in ['0'..'9']) do
        begin
          minorbuild_nr:=minorbuild_nr+s[y];
          found_minorbuild_nr:=true;
          Inc(y);
        end;
      end;
      }

      // check if ready
      if found_version_nr AND found_release_nr AND found_build_nr then break;
    end;

    CloseFile(TxtFile);

    if found_version_nr then
    begin
      result:=version_nr;
      if found_release_nr then result:=result+'.'+release_nr;
      if found_build_nr then result:=result+'.'+build_nr;
      //result:=Format('%d%.02d%.02d',[StrToInt(version_nr),StrToInt(release_nr),StrToInt(build_nr)]))
    end;

  end
  else
  begin
    Infoln('Tried to get FPC version from version.pas, but no version.pas found',etError);
    // fail-over ... not very reliable however
    s:=IncludeTrailingPathDelimiter(SourceDirectory) + FPCMAKEFILENAME;
    if FileExists(s) then
    begin
      AssignFile(TxtFile,s);
      Reset(TxtFile);
      while NOT EOF (TxtFile) do
      begin
        Readln(TxtFile,s);
        x:=Pos(MAKEVERSION,s);
        if x>0 then
        begin
          Delete(s,1,x+Length(MAKEVERSION)-1);
          y:=1;
          while ((y<=Length(s)) AND (s[y] in ['0'..'9','.'])) do Inc(y);
          if (y<=Length(s)) then Delete(s,y,MaxInt);
          result:=s;
        end;
      end;
      CloseFile(TxtFile);
    end else Infoln('Tried to get FPC version from '+FPCMAKEFILENAME+', but no '+FPCMAKEFILENAME+' found',etError);

  end;
end;

function TFPCInstaller.GetReleaseCandidateFromSource:integer;
const
  MPNO = 'minorpatch';
var
  TxtFile:Text;
  minorbuild_nr:string;
  found_minorbuild_nr:boolean;
  s:string;
  x,y:integer;
begin
  result := 0;

  minorbuild_nr:='';
  found_minorbuild_nr:=false;

  s:=IncludeTrailingPathDelimiter(SourceDirectory) + 'compiler' + DirectorySeparator + 'version.pas';
  if FileExists(s) then
  begin

    AssignFile(TxtFile,s);
    Reset(TxtFile);
    while NOT EOF (TxtFile) do
    begin
      Readln(TxtFile,s);

      x:=Pos(MPNO,s);
      if x>0 then
      begin
        y:=x+Length(MPNO);
        // move towards first numerical
        while (Length(s)>=y) AND (NOT (s[y] in ['0'..'9'])) do Inc(y);
        // get version
        while (Length(s)>=y) AND (s[y] in ['0'..'9']) do
        begin
          minorbuild_nr:=minorbuild_nr+s[y];
          found_minorbuild_nr:=true;
          Inc(y);
        end;
      end;

      // check if ready
      if found_minorbuild_nr then break;
    end;

    CloseFile(TxtFile);

    if found_minorbuild_nr then
    begin
      result:=StrToIntDef(minorbuild_nr,0);
    end;

  end;
end;

function TFPCInstaller.GetBootstrapCompilerVersionFromVersion(aVersion: string): string;
var
  s:string;
begin
  s:=aVersion;

  result:='0.0.0';

  if s=FPCTRUNKVERSION then result:=FPCTRUNKBOOTVERSION
  else if s='3.2.4' then result:='3.2.2'
  else if s='3.2.3' then result:='3.2.2'
  else if s='3.2.2' then result:='3.2.0'
  else if s='3.2.0' then result:='3.0.4'
  else if ((s='3.0.5') OR (s='3.0.4')) then result:='3.0.2'
  else if ((s='3.0.3') OR (s='3.0.2') OR (s='3.0.1')) then result:='3.0.0'
  else if s='3.0.0' then result:='2.6.4'
  else if s='2.6.5' then result:='2.6.2'
  else if s='2.6.4' then result:='2.6.2'
  else if s='2.6.2' then result:='2.6.0'
  else if s='2.6.0' then result:='2.4.4'
  else if s='2.4.4' then result:='2.4.2'
  else if s='2.4.2' then result:='2.4.0'
  else if s='2.4.0' then result:='2.2.4'
  else if s='2.2.4' then result:='2.2.2'
  else if s='2.2.2' then result:='2.2.0'
  else if s='2.2.0' then result:='2.1.4'
  else if s='2.1.4' then result:='2.1.2'
  else if s='2.1.2' then result:='2.0.4'
  else if s='2.0.4' then result:='2.0.2'
  else if s='2.0.2' then result:='2.0.0'
  else if s='2.0.0' then result:='1.9.8'
  else if s='1.9.8' then result:='1.9.6'
  else if s='1.9.6' then result:='1.9.4'
  else if s='1.9.4' then result:='1.9.2'
  else if s='1.9.2' then result:='1.9.0'
  else if s='1.9.0' then result:='1.0.10'
  else if s='1.0.10' then result:='1.0.8'
  else if s='1.0.8' then result:='1.0.6'
  else if s='1.0.6' then result:='1.0.4'
  else if s='1.0.4' then result:='1.0.2'
  else if s='1.0.2' then result:='1.0.0'
  else if s='1.0.0' then result:='0.99.14'
  else if s='0.99.14' then result:='0.99.12'
  else if s='0.99.12' then result:='0.99.10'
  else if s='0.99.10' then result:='0.99.8';
end;

function TFPCInstaller.GetBootstrapCompilerVersionFromSource(aSourcePath: string; GetLowestRequirement:boolean=false): string;
const
  REQ1='REQUIREDVERSION=';
  REQ2='REQUIREDVERSION2=';
var
  TxtFile:Text;
  s:string;
  x:integer;
  FinalVersion,RequiredVersion,RequiredVersion2:integer;
begin
  result:='0.0.0';

  s:=IncludeTrailingPathDelimiter(aSourcePath) + MAKEFILENAME;

  if FileExists(s) then
  begin
    RequiredVersion:=0;
    RequiredVersion2:=0;

    AssignFile(TxtFile,s);
    Reset(TxtFile);
    while NOT EOF (TxtFile) do
    begin
      Readln(TxtFile,s);

      x:=Pos(REQ1,s);
      if x>0 then
      begin
        Delete(s,1,x+Length(REQ1)-1);
        RequiredVersion:=CalculateNumericalVersion(s);
      end;
      x:=Pos(REQ2,s);
      if x>0 then
      begin
        Delete(s,1,x+Length(REQ2)-1);
        RequiredVersion2:=CalculateNumericalVersion(s);
      end;

      if ((RequiredVersion>0) AND (RequiredVersion2>0)) then break;
    end;

    CloseFile(TxtFile);

    if (RequiredVersion2=0) then
      FinalVersion:=RequiredVersion
    else
      begin
        if GetLowestRequirement then
        begin
          if RequiredVersion < RequiredVersion2 then
            FinalVersion := RequiredVersion
          else
            FinalVersion := RequiredVersion2;
        end
        else
        begin
          if RequiredVersion > RequiredVersion2 then
            FinalVersion := RequiredVersion
          else
            FinalVersion := RequiredVersion2;
        end;
      end;

    result:=InttoStr(FinalVersion DIV 10000);
    FinalVersion:=FinalVersion MOD 10000;
    result:=result+'.'+InttoStr(FinalVersion DIV 100);
    FinalVersion:=FinalVersion MOD 100;
    result:=result+'.'+InttoStr(FinalVersion);

  end else Infoln('Tried to get required bootstrap compiler version from '+MAKEFILENAME+', but no '+MAKEFILENAME+' found',etError);
end;

function TFPCInstaller.CreateFPCScript: boolean;
{$IFDEF UNIX}
var
  FPCScript:string;
  TxtFile:Text;
  FPCCompiler:String;
  compat:boolean;
{$ENDIF}
begin
  result:=true;

  {$IFDEF UNIX}
  if (NOT UseCompilerWrapper) then exit;

  localinfotext:=InitInfoText(' (CreateFPCScript): ');

  if FVerbose then
    Infoln(localinfotext+'Creating fpc script:',etInfo)
  else
    Infoln(localinfotext+'Creating fpc script:',etDebug);

  FPCCompiler := FPCBinDir+DirectorySeparator+'fpc'+GetExeExt;

  for compat in boolean do
  begin
    if (compat AND (NOT LinuxLegacy)) then continue;

    // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
    // If this fails, Lazarus compilation will fail...

    if (NOT compat) then FPCScript := FPCBinDir+DirectorySeparator + 'fpc.sh';
    if (compat) then FPCScript := FPCBinDir+DirectorySeparator + 'fpccompat.sh';
    if FileExists(FPCScript) then
    begin
      Infoln(localinfotext+'fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.',etInfo);
      if not(SysUtils.DeleteFile(FPCScript)) then
      begin
        Infoln(localinfotext+'Error deleting existing launcher script for FPC:'+FPCScript,etError);
        Exit(false);
      end;
    end;
      AssignFile(TxtFile,FPCScript);
        Rewrite(TxtFile);
        writeln(TxtFile,'#!/bin/sh');
        writeln(TxtFile,'# This script starts the fpc compiler installed by fpcup');
        writeln(TxtFile,'# and ignores any system-wide fpc.cfg files');
        writeln(TxtFile,'# Note: maintained by fpcup; do not edit directly, your edits will be lost.');
        {$ifdef DISABLE_PPC_CONFIG_PATH}
        writeln(TxtFile,'unset PPC_CONFIG_PATH');
        {$endif}
        if (NOT compat) then writeln(TxtFile,FPCCompiler,' -n @',FPCBinDir,DirectorySeparator,FPCCONFIGFILENAME+' '+'"$@"');
        if (compat) then writeln(TxtFile,FPCCompiler,' -XLC -n @',FPCBinDir,DirectorySeparator,FPCCONFIGFILENAME+' '+'"$@"');
        CloseFile(TxtFile);
    Result:=(FPChmod(FPCScript,&755)=0); //Make executable; fails if file doesn't exist=>Operationsucceeded update
    if Result then
    begin
      // To prevent unneccessary rebuilds of FCL, LCL and others:
      // Set fileage the same as the FPC binary itself
      Result:=(FileSetDate(FPCScript,FileAge(FPCCompiler))=0);
    end;
    if Result then
    begin
      Infoln(localinfotext+'Created launcher script for FPC:'+FPCScript,etInfo);
    end
    else
    begin
      Infoln(localinfotext+'Error creating launcher script for FPC:'+FPCScript,etError);
    end;
  end;


  {$ENDIF}
end;

function TFPCInstaller.DownloadBootstrapCompiler: boolean;
var
  BootstrapFileArchiveDir: string;
  BootstrapFilePath,BootstrapFileExt: string;
  CompilerName:string;
  OperationSucceeded: boolean;
begin
  localinfotext:=InitInfoText(' (DownloadBootstrapCompiler): ');

  OperationSucceeded:=true;

  CompilerName:=ExtractFileName(FBootstrapCompiler);

  if FBootstrapCompilerURL='' then
  begin
    Infoln(localinfotext+'No URL supplied. Fatal error. Should not happen !', etError);
    exit(false);
  end;

  if OperationSucceeded then
  begin
    OperationSucceeded:=ForceDirectoriesSafe(FBootstrapCompilerDirectory);
    if OperationSucceeded=false then Infoln(localinfotext+'Could not create directory '+FBootstrapCompilerDirectory,etError);
  end;

  BootstrapFileArchiveDir:=GetTempDirName;

  if OperationSucceeded then
  begin
    BootstrapFileArchiveDir:=IncludeTrailingPathDelimiter(BootstrapFileArchiveDir);
    BootstrapFilePath:=BootstrapFileArchiveDir+FileNameFromURL(FBootstrapCompilerURL);

    // Delete old compiler in archive directory (if any)
    SysUtils.DeleteFile(BootstrapFileArchiveDir+CompilerName);
    if (NOT FileExists(BootstrapFilePath)) then OperationSucceeded:=GetFile(FBootstrapCompilerURL,BootstrapFilePath);
    if OperationSucceeded then OperationSucceeded:=FileExists(BootstrapFilePath);
  end;

  if OperationSucceeded then
  begin
    //Download was successfull
    //Process result

    BootstrapFileExt:=FileNameAllExt(BootstrapFilePath);

    case BootstrapFileExt of
        '.zip':
        begin
          with TNormalUnzipper.Create do
          begin
            try
              OperationSucceeded:=DoUnZip(BootstrapFilePath,ExcludeTrailingPathDelimiter(BootstrapFileArchiveDir),[]);
              if OperationSucceeded then BootstrapFilePath:=StringReplace(BootstrapFilePath,'.zip','',[]);
            finally
              Free;
            end;
          end;
        end;

        '.bz2':
        begin
          with TNormalUnzipper.Create do
          begin
            try
              OperationSucceeded:=DoBUnZip2(BootstrapFilePath,IncludeTrailingPathDelimiter(BootstrapFileArchiveDir)+CompilerName);
              if OperationSucceeded then BootstrapFilePath:=IncludeTrailingPathDelimiter(BootstrapFileArchiveDir)+CompilerName;
            finally
              Free;
            end;
          end;
        end;

        {$ifdef MSWINDOWS}
        '.tar.gz','.tar.bz2':
        begin
          //& cmd.exe '/C 7z x "somename.tar.gz" -so | 7z e -aoa -si -ttar -o"somename"'
          OperationSucceeded:=(ExecuteCommand(F7zip+' x -o"'+BootstrapFileArchiveDir+'" '+BootstrapFilePath,FVerbose)=0);
          if OperationSucceeded then
          begin
            //We now have a .tar file, so remove extension
            BootstrapFilePath:=StringReplace(BootstrapFilePath,'.gz','',[]);
            BootstrapFilePath:=StringReplace(BootstrapFilePath,'.bz2','',[]);
            if ExtractFileExt(BootstrapFilePath)='.tar' then
            begin
              OperationSucceeded:=(ExecuteCommand(F7zip+' e -aoa -ttar -o"'+BootstrapFileArchiveDir+'" '+BootstrapFilePath+' '+CompilerName+' -r',FVerbose)=0);
              if OperationSucceeded then BootstrapFilePath:=StringReplace(BootstrapFilePath,'.tar','',[]);
            end;
          end;
        end;
        {$endif MSWINDOWS}

        {$ifdef UNIX}

        '.tbz2','.tbz','.tar.bz2':
        begin
          {$ifdef BSD}
          OperationSucceeded:=(ExecuteCommand(FTar,['-jxf',BootstrapFilePath,'-C',BootstrapFileArchiveDir,'--include','*'+CompilerName],FVerbose)=0);
          {$else}
          OperationSucceeded:=(ExecuteCommand(FTar,['-jxf',BootstrapFilePath,'-C',BootstrapFileArchiveDir,'--wildcards','--no-anchored',CompilerName],FVerbose)=0);
          {$endif}
        end;

        '.tar.gz':
        begin
          {$ifdef BSD}
          OperationSucceeded:=(ExecuteCommand(FTar,['-zxf',BootstrapFilePath,'-C',BootstrapFileArchiveDir,'--include','*'+CompilerName],FVerbose)=0);
          {$else}
          OperationSucceeded:=(ExecuteCommand(FTar,['-zxf',BootstrapFilePath,'-C',BootstrapFileArchiveDir,'--wildcards','--no-anchored',CompilerName],FVerbose)=0);
          {$endif}
        end;

        '.gz':
        begin
         OperationSucceeded:=(ExecuteCommand(FGunzip,['-d',BootstrapFilePath],FVerbose)=0);
         if OperationSucceeded then BootstrapFilePath:=StringReplace(BootstrapFilePath,'.gz','',[]);
        end;

        {$endif UNIX}

    end;

    // Find a bootstrapper somewhere inside the download directory
    if (NOT FileExists(BootstrapFilePath)) then
      BootstrapFilePath:=FindFileInDirWildCard('*'+CompilerName,ExcludeTrailingPathDelimiter(BootstrapFileArchiveDir));

    if ExtractFileExt(BootstrapFilePath)=GetExeExt then
    begin
      if (ExtractFileName(BootstrapFilePath)<>CompilerName) then
      begin
        // Give the bootstrapper its correct name
        if FileExists(BootstrapFilePath) then
        begin
          //FileCopy(BootstrapFilePath, BootstrapFileArchiveDir+CompilerName);
          SysUtils.DeleteFile(BootstrapFileArchiveDir+CompilerName);
          SysUtils.RenameFile(BootstrapFilePath,BootstrapFileArchiveDir+CompilerName);
        end;
      end;
    end;

    BootstrapFilePath:=BootstrapFileArchiveDir+CompilerName;
    if (NOT FileExists(BootstrapFilePath)) then
    begin
      // Get the bootstrapper somewhere inside the temporary directory
      BootstrapFilePath:=FindFileInDir(ExtractFileName(FBootstrapCompiler),ExcludeTrailingPathDelimiter(BootstrapFileArchiveDir));
    end;

    if OperationSucceeded then
    begin
      if FileExists(BootstrapFilePath) AND (ExtractFileExt(BootstrapFilePath)=GetExeExt) then
      begin
        Infoln(localinfotext+'Success. Going to copy '+BootstrapFilePath+' to '+FBootstrapCompiler,etDebug);
        SysUtils.DeleteFile(FBootstrapCompiler); //ignore errors

        // We might be moving files across partitions so we cannot use renamefile
        // However, this gives errors on Darwin due to the copied file not being signed.
        // So, use rename and fall-over to copy in case of error
        //OperationSucceeded:=FileCopy(BootstrapFilePath, FBootstrapCompiler);
        OperationSucceeded:=SysUtils.RenameFile(BootstrapFilePath,FBootstrapCompiler);
        if (NOT OperationSucceeded) then OperationSucceeded:=FileCopy(BootstrapFilePath, FBootstrapCompiler);

        //Sysutils.DeleteFile(ArchiveDir + CompilerName);
      end else OperationSucceeded:=False;
    end;
  end;

  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
    // Make executable
    OperationSucceeded:=(fpChmod(FBootstrapCompiler, &755)=0); //rwxr-xr-x
    if OperationSucceeded=false then Infoln('Bootstrap compiler: chmod failed for '+FBootstrapCompiler,eterror);
  end;
  {$ENDIF UNIX}

  if OperationSucceeded = True then
  begin
    SysUtils.DeleteFile(BootstrapFilePath);
    DeleteDirectoryEx(BootstrapFileArchiveDir);
  end
  else
  begin
    Infoln(localinfotext+'Getting/extracting bootstrap compiler failed. File: '+BootstrapFilePath, etError);
  end;

  Result := OperationSucceeded;
end;

function TFPCInstaller.GetFPCRevision: string;
var
  testcompiler:string;
begin
  result:='unknown';

  testcompiler:=IncludeTrailingPathDelimiter(SourceDirectory)+'compiler'+DirectorySeparator+'ppc1'+GetExeExt;

  if not FileExists(testcompiler) then
    testcompiler:=IncludeTrailingPathDelimiter(SourceDirectory)+'compiler'+DirectorySeparator+'ppc'+GetExeExt;

  if not FileExists(testcompiler) then
    testcompiler:=GetFPCInBinDir;

  if FileExists(testcompiler) then
  begin
    result:=CompilerRevision(testcompiler);
  end;
end;

function TFPCInstaller.IsCross:boolean;
begin
  result:=(Self is TFPCCrossInstaller);
end;

function TFPCInstaller.InitModule(DesiredBootstrapVersion:string):boolean;
var
  aCompilerList:TStringList;
  i:integer;
  aCompilerFound:boolean;
  {$IFDEF FREEBSD}
  j,k,l:integer;
  FreeBSDVersion:integer;
  {$ENDIF}
  s:string;
  {$ifdef MSWindows}
  aPath:string;
  {$endif}
  {$ifdef Darwin}
  s1:string;
  {$endif}
  aBootstrapVersion,aBootstrapURL,aBootstrapperFilename:string;
  aDownLoader: TBasicDownLoader;
begin
  result := true;

  if (InitDone) AND (DesiredBootstrapVersion='') then exit;

  localinfotext:=InitInfoText(' (InitModule): ');

  result:=CheckAndGetTools;

  WritelnLog(localinfotext+'Init:', false);
  WritelnLog(localinfotext+'FPC directory:      ' + SourceDirectory, false);
  WritelnLog(localinfotext+'FPC URL:            ' + URL, false);
  WritelnLog(localinfotext+'FPC options:        ' + FCompilerOptions, false);

  aBootstrapVersion:=DesiredBootstrapVersion;

  if (aBootstrapVersion<>'') then
  begin
    // Check if we have an override compiler !!
    if FileExists(FCompiler) then
    begin
      FBootstrapCompiler:=FCompiler;
      FCompiler:='';
    end;
  end;

  // set standard bootstrap compilername
  if (NOT FileExists(FBootstrapCompiler)) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetSourceCPUOS+'-'+GetCompilerName(GetSourceCPU);
  if (NOT FileExists(FBootstrapCompiler)) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetSourceCPU);

  {$IFDEF Darwin}
    {$IFDEF CPU32}
      if NOT FileExists(FBootstrapCompiler) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
    {$ENDIF CPU32}
  {$ENDIF Darwin}

  if (aBootstrapVersion<>'') then
  begin
    aCompilerFound:=false;

    if FUseWget
       then aDownLoader:=TWGetDownLoader.Create
       else aDownLoader:=TNativeDownLoader.Create;

    try
      if (NOT aCompilerFound) then
      begin
        Infoln(localinfotext+'Looking for a bootstrap compiler from Github FPCUP(deluxe) releases.',etInfo);

        aBootstrapURL:='';

        // Get file-list from Github
        aCompilerList:=TStringList.Create;
        try
          // Get bootstrappers filenames from Github
          // Might fail due to throttling
          aCompilerList.Clear;
          try
            GetGitHubFileList(FPCUPGITREPOBOOTSTRAPPERAPI,aCompilerList,FUseWget,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
          except
            on E : Exception do
            begin
              aCompilerList.Clear;
              Infoln(localinfotext+'Getting list of bootstrappers from Github failed', etError);
              Infoln(localinfotext+E.ClassName+' error raised, with message : '+E.Message, etError);
            end;
          end;

          while ((NOT aCompilerFound) AND (CalculateNumericalVersion(aBootstrapVersion)>0)) do
          begin
            // Construct bootstrapper name
            s:=StringReplace(aBootstrapVersion,'.','_',[rfReplaceAll]);
            aBootstrapperFilename:='fpcup-'+s+'-'+GetSourceCPU;
            {$ifdef CPUARMHF}
            aBootstrapperFilename:=aBootstrapperFilename+'hf';
            {$endif CPUARMHF}
            {$IF DEFINED(CPUPOWERPC64) AND DEFINED(LINUX) AND DEFINED(FPC_ABI_ELFV2)}
            aBootstrapperFilename:=aBootstrapperFilename+'le';
            {$ENDIF}
            aBootstrapperFilename:=aBootstrapperFilename+'-';
            {$ifdef LINUX}
            if FMUSL then aBootstrapperFilename:=aBootstrapperFilename+'musl';
            {$endif LINUX}
            aBootstrapperFilename:=aBootstrapperFilename+GetSourceOS;
            {$ifdef Solaris}
            //perhaps needed for special Solaris OpenIndiana bootstrapper
            //if FSolarisOI then aBootstrapperFilename:=aBootstrapperFilename+'oi';
            {$endif Solaris}

            aBootstrapperFilename:=aBootstrapperFilename+'-'+GetCompilerName(GetSourceCPU);

            Infoln(localinfotext+'Looking online for a FPCUP(deluxe) bootstrapper with name: '+aBootstrapperFilename,etInfo);

            // We have successfully downloaded a list with available compilers through the API of GitHub.
            // However, this might have failed due to throttling, so we need a fallback method.
            if (aCompilerList.Count>0) then
            begin
              for i:=0 to Pred(aCompilerList.Count) do
              begin
                aBootstrapURL:=aBootstrapperFilename;

                aCompilerFound:=(Pos(aBootstrapURL,aCompilerList[i])>0);

                {$ifdef FREEBSD}
                if (NOT aCompilerFound) then
                begin
                  j:=GetFreeBSDVersion;
                  if j=0 then j:=DEFAULTFREEBSDVERSION; // Use FreeBSD default version when GetFreeBSDVersion does not give a result
                  aBootstrapURL:=StringReplace(aBootstrapperFilename,'-'+GetSourceOS,'-'+GetSourceOS+InttoStr(j),[]);
                  aCompilerFound:=(Pos(aBootstrapURL,aCompilerList[i])>0);
                  if (NOT aCompilerFound) then
                  begin
                    //try other versions if available
                    for j:=14 downto 9 do
                    begin
                      aBootstrapURL:=StringReplace(aBootstrapperFilename,'-'+GetSourceOS,'-'+GetSourceOS+InttoStr(j),[]);
                      aCompilerFound:=(Pos(aBootstrapURL,aCompilerList[i])>0);
                      if aCompilerFound then break;
                    end;
                  end;
                end;
                {$endif}

                if aCompilerFound then break;
              end;

              if aCompilerFound then
              begin
                // Make URL a real URL by adding the path to the URL
                aBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+aBootstrapURL;
              end

            end
            else
            begin
              // Directly try the URL in case we did NOT receive a file list
              aBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+aBootstrapperFilename;
              aCompilerFound:=aDownLoader.checkURL(aBootstrapURL);
              {$ifdef FREEBSD}
              if (NOT aCompilerFound) then
              begin
                j:=GetFreeBSDVersion;
                if j=0 then j:=DEFAULTFREEBSDVERSION; // Use FreeBSD default version when GetFreeBSDVersion does not give a result
                aBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+StringReplace(aBootstrapperFilename,'-'+GetSourceOS,'-'+GetSourceOS+InttoStr(j),[]);
                aCompilerFound:=aDownLoader.checkURL(aBootstrapURL);
              end;
              {$endif}
            end;

            if aCompilerFound then
            begin
              Infoln(localinfotext+'Success: found a FPCUP(deluxe) bootstrapper with version '+aBootstrapVersion,etInfo);
              break;
            end
            else
            begin
              // look for a previous (fitting) compiler if not found
              s:=GetBootstrapCompilerVersionFromVersion(aBootstrapVersion);
              if aBootstrapVersion<>s
                 then aBootstrapVersion:=s
                 else break;
            end;

          end;

        finally
          aCompilerList.Free;
        end;

        // found a bootstrapper !
        if (aCompilerFound) then
        begin
          if FBootstrapCompilerURL='' then
          begin
            aCompilerFound:=true;
            Infoln(localinfotext+'Got a bootstrap compiler from FPCUP(deluxe) provided bootstrapper binaries.',etInfo);
            FBootstrapCompilerURL := aBootstrapURL;
            // set standard bootstrap compilername
            FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetSourceCPU);
          end;
        end;
      end;

      // go ahead with compiler found !!
      // get compiler version (if any)
      s:=CompilerVersion(FCompiler);

      // we did not find any suitable bootstrapper
      // check if we have a manual installed bootstrapper
      if (NOT aCompilerFound) AND (FBootstrapCompilerURL='') then
      begin
        if (s='0.0.0') then
        begin
          Compiler:=FBootstrapCompiler;
          s:=CompilerVersion(FCompiler);
        end;
        if (s<>'0.0.0') then
        begin
          Infoln(localinfotext+'No correct bootstrapper. But going to use the available one with version ' + s,etInfo);
          result:=true;
        end;
      end;

      if (aCompilerFound) AND (FBootstrapCompilerURL<>'') then
      begin
        // final check ... do we have the correct (as in version) compiler already ?
        Infoln(localinfotext+'Check if we already have a bootstrap compiler with version '+ aBootstrapVersion,etInfo);
        if s<>aBootstrapVersion then
        begin
          Infoln(localinfotext+'No correct bootstrapper. Going to download new bootstrapper',etInfo);
          Infoln(localinfotext+'Downloading bootstrapper from '+ FBootstrapCompilerURL,etDebug);
          result:=DownloadBootstrapCompiler;
          // always use the newly downloaded bootstrapper !!
          if result then
          begin
            Compiler:=FBootstrapCompiler;
            s:=CompilerVersion(FCompiler);
          end;
        end;
      end;

    finally
      aDownLoader.Free
    end;

  end;

  if FCompiler='' then   //!!!Don't use Compiler here. GetCompiler returns installed compiler.
    Compiler:=FBootstrapCompiler;

  WritelnLog(localinfotext+'Init:',false);
  WritelnLog(localinfotext+'Bootstrap compiler dir: '+ExtractFilePath(FCompiler),false);
  WritelnLog(localinfotext+'FPC URL:                '+URL,false);
  WritelnLog(localinfotext+'FPC options:            '+FCompilerOptions,false);
  WritelnLog(localinfotext+'FPC source directory:   '+SourceDirectory,false);
  WritelnLog(localinfotext+'FPC install directory:  '+InstallDirectory,false);
  {$IFDEF MSWINDOWS}
  WritelnLog(localinfotext+'Make/binutils path:     '+FMakeDir,false);
  {$ENDIF MSWINDOWS}

  if result then
  begin
    if assigned(CrossInstaller) then
    begin
      CrossInstaller.SolarisOI:=FSolarisOI;
      CrossInstaller.MUSL:=FMUSL;
      CrossInstaller.LL:=LinuxLegacy;
    end;
  end;

  if result then
  begin
    {$IFDEF MSWINDOWS}
    aPath:='';
    if Assigned(SVNClient) AND SVNClient.ValidClient then
    begin
      s:=SVNClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      s:=ExtractFileDir(s);
      // Only add path if there is no stray shell (sh.exe) laying around in this path
      if (NOT FileExists(s+DirectorySeparator+'sh'+GetExeExt)) then
        aPath:=aPath+PathSeparator+s;
    end;
    if Assigned(GITClient) AND GITClient.ValidClient then
    begin
      s:=GITClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      s:=ExtractFileDir(s);
      // Only add path if there is no stray shell (sh.exe) laying around in this path
      if (NOT FileExists(s+DirectorySeparator+'sh'+GetExeExt)) then
        aPath:=aPath+PathSeparator+s;
    end;
    if Assigned(HGClient) AND HGClient.ValidClient then
    begin
      s:=HGClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      s:=ExtractFileDir(s);
      // Only add path if there is no stray shell (sh.exe) laying around in this path
      if (NOT FileExists(s+DirectorySeparator+'sh'+GetExeExt)) then
        aPath:=aPath+PathSeparator+s;
    end;
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // add install/fpc/utils to solve data2inc not found by fpcmkcfg
    // also add src/fpc/utils to solve data2inc not found by fpcmkcfg
    SetPath(
      FPCBinDir+PathSeparator+ {compiler for current architecture}
      FMakeDir+PathSeparator+
      FBootstrapCompilerDirectory+PathSeparator+
      InstallDirectory+PathSeparator+
      IncludeTrailingPathDelimiter(InstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
      IncludeTrailingPathDelimiter(InstallDirectory)+'utils'+PathSeparator+
      SourceDirectory+PathSeparator+
      IncludeTrailingPathDelimiter(SourceDirectory)+'compiler'+PathSeparator+
      IncludeTrailingPathDelimiter(SourceDirectory)+'utils'+
      aPath,
      false,false);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    // add install/fpc/utils to solve data2inc not found by fpcmkcfg
    // also add src/fpc/utils to solve data2inc not found by fpcmkcfg
    s:='';
    {$ifdef FreeBSD}
    // for the local GNU binary utilities
    s:='/usr/local/bin'+PathSeparator;
    {$endif}
    {$ifdef Darwin}
    // for the suitable XCode binary utilities
    s1:=GetDarwinSDKVersion('macosx');
    if CompareVersionStrings(s1,'10.14')>=0 then
    begin
      s:='/Library/Developer/CommandLineTools/usr/bin'+PathSeparator;
    end;
    {$endif}
    SetPath(
      FPCBinDir+PathSeparator+
      FBootstrapCompilerDirectory+PathSeparator+
      InstallDirectory+PathSeparator+
      IncludeTrailingPathDelimiter(InstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
      IncludeTrailingPathDelimiter(InstallDirectory)+'utils'+PathSeparator+
      SourceDirectory+PathSeparator+
      IncludeTrailingPathDelimiter(SourceDirectory)+'compiler'+PathSeparator+
      IncludeTrailingPathDelimiter(SourceDirectory)+'utils'+PathSeparator+
      s+
      // pwd is located in /bin ... the makefile needs it !!
      // tools are located in /usr/bin ... the makefile needs it !!
      '/bin'+PathSeparator+
      '/usr/bin',
      true,false);
    {$ENDIF UNIX}
  end;

  GetVersion;
  InitDone:=result;
end;


function TFPCInstaller.BuildModule(ModuleName: string): boolean;
const
  FPCUPMAGIC    = ': base settings';
  REVINCERROR   = 'error: empty';
var
  RequiredBootstrapVersion        : string;
  RequiredBootstrapVersionLow     : string;
  RequiredBootstrapVersionHigh    : string;
  RequiredBootstrapVersionMinimum : string;
  FPCCfg                          : string;
  FPCMkCfg                        : string; //path+file of fpcmkcfg
  ConfigText,ConfigTextStore      : TStringList;
  OperationSucceeded              : boolean;
  PlainBinPath                    : string; //directory above the architecture-dependent FBinDir
  VersionSnippet                  : string;
  TxtFile                         : Text;
  s,s2                            : string;
  x,y                             : integer;

  function CheckFPCMkCfgOption(aOption:string):boolean;
  var
    aIndex:integer;
  begin
    aIndex:=-1;
    Processor.Process.Parameters.Clear;
    Processor.SetParamData('-h');
    try
      ProcessorResult:=Processor.ExecuteAndWait;
      //if ProcessorResult = 0 then
      begin
        if Processor.WorkerOutput.Count>0 then
        begin
          aIndex:=StringListStartsWith(Processor.WorkerOutput,Trim(aOption));
        end;
      end;
    except
      on E: Exception do
      begin
        WritelnLog(etError,infotext+'Running [CheckFPCMkCfgOption] failed with an exception!'+LineEnding+'Details: '+E.Message,true);
      end;
    end;
    result:=(aIndex<>-1);
  end;


  function RunFPCMkCfgOption(aFile:string):boolean;
  begin
    result:=false;
    Processor.SetParamData('-d');
    Processor.SetParamData('basepath='+FFPCInstallDir);
    Processor.SetParamData('-o');
    Processor.SetParamData('' + aFile + '');
    Infoln(infotext+'Creating '+{ExtractFileName}(aFile));
    try
      ProcessorResult:=Processor.ExecuteAndWait;
      result:=(ProcessorResult=0);
    except
      on E: Exception do
      begin
        WritelnLog(etError, infotext+'Running fpcmkcfg failed with an exception!'+LineEnding+'Details: '+E.Message,true);
        result:=false;
      end;
    end;
  end;

begin
  result:=inherited;

  result:=InitModule;

  if not result then exit;

  Infoln(infotext+'Going to build '+ModuleName,etInfo);

  // Assume the bootstrap version is correct, unset OVERRIDEVERSIONCHECK
  FBootstrapCompilerOverrideVersionCheck:=false;

  s:=IncludeTrailingPathDelimiter(SourceDirectory) + MAKEFILENAME;
  if (NOT FileExists(s)) then
  begin
    Infoln(infotext+s+' not found. Severe error. Should not happen. Aborting.',etError);
    exit(false);
  end;

  VersionSnippet:=SourceVersionStr;
  if (IsCross) then
  begin
    Compiler:=GetFPCInBinDir;
    s2:=CrossInstaller.RegisterName+' cross-builder: Detected source version FPC (source): '
  end
  else
  begin
    s2:='Detected source version FPC (source): ';
    if (VersionSnippet='0.0.0') then
    begin
      VersionSnippet:=CompilerVersion(GetFPCInBinDir);
      if VersionSnippet<>'0.0.0' then
        s2:='Detected source version FPC (compiler): ';
    end;
  end;

  if (VersionSnippet<>'0.0.0') then
    Infoln(infotext+s2+VersionSnippet, etInfo);

  // if cross-compiling, skip a lot of code
  // trust the previous work done by this code for the native installer!
  if (NOT (IsCross)) then
  begin
    RequiredBootstrapVersion:='0.0.0';

    RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromSource(SourceDirectory,True);
    RequiredBootstrapVersionHigh:=GetBootstrapCompilerVersionFromSource(SourceDirectory,False);

    // There is no Makefile or no info inside the Makefile to determine bootstrap version
    // So, try something else !
    if RequiredBootstrapVersionLow='0.0.0' then RequiredBootstrapVersionHigh:='0.0.0';
    if RequiredBootstrapVersionLow='0.0.0' then
       RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromVersion(GetVersion);
    if RequiredBootstrapVersionLow='0.0.0' then
    begin
      Infoln(infotext+'Could not determine required bootstrap compiler version. Should not happen. Aborting.',etError);
      exit(false);
    end;
    if RequiredBootstrapVersionHigh='0.0.0' then
    begin
      // Only a single bootstrap version found
      Infoln(infotext+'To compile this FPC, we use a compiler with version : '+RequiredBootstrapVersionLow,etInfo);
      // we always build with the highest bootstrapper, so, in this case (trick), make high = low !!
      RequiredBootstrapVersionHigh:=RequiredBootstrapVersionLow;
    end
    else
    begin
      Infoln(infotext+'To compile this FPC, we need (required) a compiler with version '+RequiredBootstrapVersionLow+' or '+RequiredBootstrapVersionHigh,etInfo);
    end;

    OperationSucceeded:=false;

    if NOT FileExists(FCompiler) then
    begin
      // can we use a FPC bootstrapper that is already on the system (somewhere in the path) ?
      if true then
      //if NativeFPCBootstrapCompiler then
      begin
        s:=GetCompilerName(GetSourceCPU);
        s:=Which(s);
        {$ifdef Darwin}
        // Due to codesigning, do not copy, but just use it.
        if FileExists(s) then Compiler:=s;
        {$else}
        //Copy the compiler to our bootstrap directory
        if FileExists(s) then FileCopy(s,FCompiler);
        {$endif}
        if NOT FileExists(s) then
        begin
          s:='fpc'+GetExeExt;
          s:=Which(s);
          if FileExists(s) then Compiler:=s;
        end;

        if FileExists(FCompiler) then
        begin
          Infoln(infotext+'FPCUP bootstrapper was not available. Found another one. Going to it: '+FCompiler,etInfo);
        end;
      end;
    end;

    // do we already have a suitable compiler somewhere ?
    if FileExists(FCompiler) then
    begin
      OperationSucceeded:=(CompilerVersion(FCompiler)=RequiredBootstrapVersionLow);
      if OperationSucceeded
        then RequiredBootstrapVersion:=RequiredBootstrapVersionLow
        else
        begin
          // check if higher compiler version is available
          if (RequiredBootstrapVersionLow<>RequiredBootstrapVersionHigh) then
          begin
            OperationSucceeded:=(CompilerVersion(FCompiler)=RequiredBootstrapVersionHigh);
            if OperationSucceeded then RequiredBootstrapVersion:=RequiredBootstrapVersionHigh;
          end;
        end;
    end;

    if OperationSucceeded then
    begin
      Infoln(infotext+'To compile this FPC, we will use the (already available) compiler with version : '+RequiredBootstrapVersion,etInfo);
    end
    else
    begin
      // get the bootstrapper, among other things (binutils)
      // start with the highest requirement ??!!
      RequiredBootstrapVersion:=RequiredBootstrapVersionHigh;

      // Check for specials
      RequiredBootstrapVersionMinimum:=GetMinimumFPCVersion;
      if CalculateNumericalVersion(RequiredBootstrapVersion)>CalculateNumericalVersion(RequiredBootstrapVersionMinimum) then RequiredBootstrapVersionMinimum:=RequiredBootstrapVersion;
      result:=InitModule(RequiredBootstrapVersionMinimum);

      {
      if (NOT result) then
      begin
        // Retry with the lowest requirement
        //RequiredBootstrapVersion:=RequiredBootstrapVersionLow;
        result:=InitModule(RequiredBootstrapVersionLow);
      end;
      }

      if (CompilerVersion(FCompiler)=RequiredBootstrapVersion)
      then
      begin
        Infoln(infotext+'To compile this FPC, we will use a fresh compiler with version : '+RequiredBootstrapVersion,etInfo);
      end
      else
      begin
        // check if we have a lower acceptable requirement for the bootstrapper
        if (CompilerVersion(FCompiler)=RequiredBootstrapVersionLow) then
        begin
          // if so, set bootstrapper to lower one !!
          RequiredBootstrapVersion:=RequiredBootstrapVersionLow;
          Infoln(infotext+'To compile this FPC, we can also (and will) use (required) a fresh compiler with version : '+RequiredBootstrapVersion,etInfo);
        end
        else
        begin
          // As the bootstrap version is incorrect, set OVERRIDEVERSIONCHECK
          FBootstrapCompilerOverrideVersionCheck:=true;
        end;
      end;
    end;

    // get the correct binutils (Windows only)
    {$IFDEF MSWINDOWS}
    if (Pos('/branches/',URL)>0) then
    begin
      CreateBinutilsList(RequiredBootstrapVersion);
    end
    else
    begin
      s:=VersionSnippet;
      x:=GetReleaseCandidateFromSource;
      if (x<>0) then
        s:=s+'.rc'+InttoStr(x);
      CreateBinutilsList(s);
    end;
    {$ENDIF MSWINDOWS}

    result:=CheckAndGetNeededBinUtils;

    {$ifdef Solaris}
    //sometimes, gstrip does not exist on Solaris ... just copy it to a place where it can be found ... tricky
    if (NOT FileExists('/usr/bin/gstrip')) AND (FileExists('/usr/bin/strip')) then
    begin
      if DirectoryExists(FPCBinDir) then
      begin
        s:=FPCBinDir+DirectorySeparator+'gstrip';
        if (NOT FileExists(s)) then FileCopy('/usr/bin/strip',s);
      end
      else
      begin
        ForceDirectoriesSafe(InstallDirectory);
        s:=IncludeTrailingPathDelimiter(InstallDirectory)+'gstrip';
        if (NOT FileExists(s)) then FileCopy('/usr/bin/strip',s);
      end;
    end;
    {$endif}
  end;//(NOT (IsCross))

  // Do we need to force the use of libc :
  FUseLibc:=False;

  if (IsCross) then
  begin
    // This might also be done in the cross-compilers themselves.
    if LinuxLegacy then FUseLibc:=True;
    if (CrossInstaller.TargetOS=TOS.dragonfly) then FUseLibc:=True;
    if (CrossInstaller.TargetOS=TOS.freebsd) then FUseLibc:=True;
    if (CrossInstaller.TargetOS=TOS.openbsd) AND (SourceVersionNum>CalculateNumericalVersion('3.2.0')) then FUseLibc:=True;
  end
  else
  begin
    if (GetSourceOS=GetOS(TOS.dragonfly)) then FUseLibc:=True;
    if (GetSourceOS=GetOS(TOS.freebsd)) then FUseLibc:=True;
    if (GetSourceOS=GetOS(TOS.openbsd)) AND (SourceVersionNum>CalculateNumericalVersion('3.2.0')) then FUseLibc:=True;
  end;

  {$ifdef FORCEREVISION}
  //if ((ModuleName<>_MAKEFILECHECKFPC) AND (NOT IsCross )) then
  if ((ModuleName=_FPC) AND (NOT IsCross )) then
  //if (NOT IsCross ) then
  begin
    FUseRevInc:=true;
    if (SourceVersionNum<>0) then if (SourceVersionNum<CalculateFullVersion(3,2,3)) then FUseRevInc:=false;
    if FUseRevInc then
    begin
      Infoln('FPC builder: Checking auto-generated (Makefile) revision.inc for errors', etDebug);
      FUseRevInc:=false;
      // Generate revision.inc through Makefile to check its contents
      s:=ConcatPaths([SourceDirectory,'compiler'])+DirectorySeparator+REVINCFILENAME;
      if BuildModuleCustom(_REVISIONFPC) then
      begin
        // Check revision.inc for errors
        if FileExists(s) then
        begin
          VersionSnippet:=REVINCERROR;
          ConfigText:=TStringList.Create;
          try
            ConfigText.LoadFromFile(s);
            if (ConfigText.Count>0) then
            begin
              VersionSnippet:=ConfigText.Strings[0];
              if (OccurrencesOfChar(VersionSnippet,'''')=2) then
              begin
                VersionSnippet:=AnsiDequotedStr(VersionSnippet,'''');
                VersionSnippet:=AnsiDequotedStr(VersionSnippet,'"');
                if (Length(VersionSnippet)>0) AND (Pos(' ',VersionSnippet)=0) AND (ContainsDigit(VersionSnippet)) then FUseRevInc:=true;
              end;
            end;
          finally
            ConfigText.Free;
          end;
          if (VersionSnippet=REVINCERROR) then FUseRevInc:=false;
          if (NOT FUseRevInc) then
          begin
            Infoln(infotext+'Contents of auto-generated (Makefile) revision.inc incorrect.', etDebug);
            Infoln(infotext+'Contents detected: '+VersionSnippet, etDebug);
            Infoln(infotext+'Deleting revision.inc and preventing use !', etDebug);
            DeleteFile(s);
          end;
        end;
      end;
    end;
  end;
  {$endif FORCEREVISION}

  // Now: the real build of FPC !!!
  OperationSucceeded:=BuildModuleCustom(ModuleName);

  // Restore infotext
  infotext:=InitInfoText(' (BuildModule: '+ModuleName+'): ');

  if ModuleName=_MAKEFILECHECKFPC then exit;

  //if ModuleName<>_FPC then exit;

  // only create fpc.cfg and other configs with fpcmkcfg when NOT crosscompiling !

  s:=FPCBinDir;
  if (OperationSucceeded) AND (NOT (IsCross)) AND DirectoryExists(s) then
  begin
    // Find out where fpcmkcfg lives
    if (OperationSucceeded) then
    begin
      FPCMkCfg:=FPCBinDir+DirectorySeparator+FPCMAKECONFIG+GetExeExt;
      OperationSucceeded:=CheckExecutable(FPCMkCfg,['-h'],FPCMAKECONFIG);
      if (NOT OperationSucceeded) then
      begin
        Infoln(infotext+'Did not find '+FPCMAKECONFIG+GetExeExt+' in '+ExtractFileDir(FPCMkCfg),etDebug);
        FPCMkCfg:=ConcatPaths([InstallDirectory,'bin',FPCMAKECONFIG+GetExeExt]);
        OperationSucceeded:=CheckExecutable(FPCMkCfg,['-h'],FPCMAKECONFIG);
        if (NOT OperationSucceeded) then
        begin
          Infoln(infotext+'Did not find '+FPCMAKECONFIG+GetExeExt+' in '+ExtractFileDir(FPCMkCfg),etDebug);
        end;
      end;
      if OperationSucceeded then
      begin
        Infoln(infotext+'Found valid '+FPCMAKECONFIG+GetExeExt+' executable in '+ExtractFileDir(FPCMkCfg),etInfo);
      end
      else
      begin
        Infoln(infotext+'Could not find '+FPCMAKECONFIG+GetExeExt+' executable. Aborting.',etError);
        FPCMkCfg:='';
      end;
    end;

    {$IFDEF UNIX}
    // FPC trunk, starting from hash c9453164 does not like the use of fpc.sh anymore
    // So, our trick to isolate the UNIX install has become obsolete
    // Use configpath as a replacement
    // See: compiler/options.pp function: check_configfile
    // Please note: might NOT work when the environment defines a "PPC_CONFIG_PATH" !!!
    // See: {$define DISABLE_PPC_CONFIG_PATH}
    // Do this for all FPC versions from now on (so, do not check version) !!
    if (NOT UseCompilerWrapper) then
    begin
      // if (CalculateNumericalVersion(CompilerVersion(GetFPCInBinDir))>=CalculateNumericalVersion('3.3.1')) then
      begin
        s:=ExpandFileName(FPCBinDir+'/../etc/');
        if (NOT DirectoryExists(s)) then ForceDirectories(s);
      end;
    end;
    {$ENDIF}

    FPCCfg:=GetFPCConfigPath(FPCCONFIGFILENAME);

    if (OperationSucceeded) then
    begin
      Processor.Executable:=FPCMkCfg;
      Processor.Process.CurrentDirectory:=InstallDirectory;

      s2:= ExtractFilePath(FPCMkCfg)+FPFILENAME+GetExeExt;
      if FileExists(s2) then
      begin
        s := FPCBinDir+DirectorySeparator+FPCONFIGFILENAME;
        if (NOT FileExists(s)) then
        begin
          //create fp.cfg
          //if CheckFPCMkCfgOption('-1') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.SetParamData('-1');
            Processor.SetParamData('-d');
            Processor.SetParamData('fpctargetos='+GetSourceOS);

            {$IFDEF UNIX}
            //s2:=GetStartupObjects;
            //if Length(s2)>0 then
            //begin
            //  Processor.SetParamData('-d');
            //  Processor.SetParamData('GCCLIBPATH= -Fl'+s2);
            //end;
            {$ENDIF UNIX}

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s));
        end;

        s := FPCBinDir+DirectorySeparator+FPINIFILENAME;
        if (NOT FileExists(s)) then
        begin
          //create fp.ini
          //if CheckFPCMkCfgOption('-2') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.SetParamData('-2');

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s));
        end;
      end;

      // Check if we have the correct executable to gemerate the config files
      s2:= ExtractFilePath(FPCMkCfg)+FPCPKGFILENAME+GetExeExt;
      if FileExists(s2) then
      begin

        s2 := ConcatPaths([BaseDirectory,PACKAGESCONFIGDIR]);
        ForceDirectoriesSafe(s2);

        s  := IncludeTrailingPathDelimiter(s2)+FPCPKGCONFIGFILENAME;
        if (NOT FileExists(s)) then
        begin
          //Create package configuration fppkg.cfg
          //if CheckFPCMkCfgOption('-3') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.SetParamData('-3');

            Processor.SetParamData('-d');
            Processor.SetParamData('LocalRepository='+ConcatPaths([BaseDirectory,PACKAGESLOCATION])+DirectorySeparator);

            Processor.SetParamData('-d');
            Processor.SetParamData('CompilerConfigDir={LocalRepository}'+'config'+DirectorySeparator);

            Processor.SetParamData('-d');
            Processor.SetParamData('GlobalPath='+IncludeTrailingPathDelimiter(FFPCInstallDir));

            Processor.SetParamData('-d');
            Processor.SetParamData('GlobalPrefix='+FFPCInstallDir);

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s));
        end;

        // Make packages directory
        s2:=ConcatPaths([BaseDirectory,PACKAGESLOCATION]);
        ForceDirectoriesSafe(s2);

        //Make config directory for compiler defaults
        s2:=ConcatPaths([BaseDirectory,PACKAGESLOCATION,'config']);
        ForceDirectoriesSafe(s2);

        s := IncludeTrailingPathDelimiter(s2)+FPCPKGCOMPILERTEMPLATE;
        if (NOT FileExists(s)) then
        begin
          //Create default compiler template
          //if CheckFPCMkCfgOption('-4') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.SetParamData('-4');

            (*
            Processor.SetParamData('-d');
            Processor.SetParamData('GlobalPrefix='+IncludeTrailingPathDelimiter(BaseDirectory));
            Processor.SetParamData('-d');
            Processor.SetParamData('LocalPrefix={LocalRepository}');
            Processor.SetParamData('-d');
            Processor.SetParamData('GlobalInstallDir={GlobalPrefix}'+'fpc'+DirectorySeparator);
            Processor.SetParamData('-d');
            Processor.SetParamData('LocalInstallDir={LocalPrefix}'+'fpc'+DirectorySeparator);
            *)

            Processor.SetParamData('-d');
            Processor.SetParamData('fpcbin='+FCompiler);

            Processor.SetParamData('-d');
            Processor.SetParamData('fpcversion='+GetVersion);

            Processor.SetParamData('-d');
            Processor.SetParamData('fpctargetos='+GetSourceOS);

            Processor.SetParamData('-d');
            Processor.SetParamData('fpctargetcpu='+GetSourceCPU);

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s));
        end;
      end;

      s := FPCCfg;
      if (NOT FileExists(s)) then
      begin
        //create fpc.cfg
        Processor.Process.Parameters.Clear;
        Processor.SetParamData('-d');
        Processor.SetParamData('sharepath='+FPCShareDir);
        Processor.SetParamData('-d');
        Processor.SetParamData('localbasepath='+ConcatPaths([FFPCInstallDir,PACKAGESLOCATION,'units',FPC_TARGET_MAGIC])+'/*');
        RunFPCMkCfgOption(s);
      end
      else
      begin
        Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s));
      end;

    end;

    // if, for one reason or another, there is no cfg file, create a minimal one by ourselves
    if (NOT FileExists(FPCCfg)) then
    begin
      AssignFile(TxtFile,FPCCfg);
      Rewrite(TxtFile);
      try
        writeln(TxtFile,'# Minimal FPC config file generated by fpcup(deluxe).');
        writeln(TxtFile,'');
        writeln(TxtFile,'# For a release compile with optimizes and strip debuginfo');
        writeln(TxtFile,'#IFDEF RELEASE');
        writeln(TxtFile,'  -O2');
        writeln(TxtFile,'  -Xs');
        writeln(TxtFile,'  #WRITE Compiling Release Version');
        writeln(TxtFile,'#ENDIF');
        writeln(TxtFile,'');
        writeln(TxtFile,'# For a debug version compile with debuginfo and all codegeneration checks on');
        writeln(TxtFile,'#IFDEF DEBUG');
        writeln(TxtFile,'  -glh');
        writeln(TxtFile,'  -Crtoi');
        writeln(TxtFile,'  #WRITE Compiling Debug Version');
        writeln(TxtFile,'#ENDIF');
        writeln(TxtFile,'');
        writeln(TxtFile,'# Allow goto, inline, C-operators, C-vars');
        writeln(TxtFile,'-Sgic');
        writeln(TxtFile,'');
        writeln(TxtFile,'# searchpath for units and other system dependent things');
        writeln(TxtFile,'-Fu'+IncludeTrailingPathDelimiter(InstallDirectory)+'units/'+FPC_TARGET_MAGIC+'/');
        writeln(TxtFile,'-Fu'+IncludeTrailingPathDelimiter(InstallDirectory)+'units/'+FPC_TARGET_MAGIC+'/*');
        writeln(TxtFile,'-Fu'+IncludeTrailingPathDelimiter(InstallDirectory)+'units/'+FPC_TARGET_MAGIC+'/rtl');
        writeln(TxtFile,'');
        writeln(TxtFile,'# searchpath for tools');
        writeln(TxtFile,'-FD'+IncludeTrailingPathDelimiter(InstallDirectory)+'bin/'+FPC_TARGET_MAGIC);
        writeln(TxtFile,'');
        writeln(TxtFile,'# binutils prefix for cross compiling');
        writeln(TxtFile,'#IFDEF FPC_CROSSCOMPILING');
        writeln(TxtFile,'#IFDEF NEEDCROSSBINUTILS');
        writeln(TxtFile,'  -XP'+FPC_TARGET_MAGIC+'-');
        writeln(TxtFile,'#ENDIF');
        writeln(TxtFile,'#ENDIF');
        writeln(TxtFile,'');
        writeln(TxtFile,'# Always strip debuginfo from the executable');
        writeln(TxtFile,'-Xs');
        writeln(TxtFile,'');
        writeln(TxtFile,'# assembling');
        writeln(TxtFile,'#IFDEF Darwin');
        writeln(TxtFile,'# use pipes instead of temporary files for assembling');
        writeln(TxtFile,'-ap');
        writeln(TxtFile,'#ENDIF');
        writeln(TxtFile,'');
        writeln(TxtFile,'# Write always a nice FPC logo ;)');
        writeln(TxtFile,'-l');
        writeln(TxtFile,'');
        writeln(TxtFile,'# Display Info, Warnings and Notes and supress Hints');
        writeln(TxtFile,'-viwnh-');
        writeln(TxtFile,'');
      finally
        CloseFile(TxtFile);
      end;
    end;

    OperationSucceeded:=FileExists(FPCCfg);

    if OperationSucceeded then
    begin
      Infoln(infotext+'Creating/checking default configuration file(s) success.');
      Infoln(infotext+'Going to tune fpc.cfg to our needs.');
    end
    else
      Infoln(infotext+'No fpc.cfg file created or found. Should not happen. Severe error !!!',etError);

    // at this point, a default fpc.cfg should exist
    // modify it to suit fpcup[deluxe]
    if OperationSucceeded then
    begin
      ConfigText:=TStringList.Create;
      {$IF FPC_FULLVERSION > 30100}
      //ConfigText.DefaultEncoding:=TEncoding.ASCII;
      {$ENDIF}
      ConfigTextStore:=TStringList.Create;
      {$IF FPC_FULLVERSION > 30100}
      //ConfigTextStore.DefaultEncoding:=TEncoding.ASCII;
      {$ENDIF}
      try
        ConfigText.LoadFromFile(FPCCfg);

        //Try to find the end of the normal vanilla FPC config file
        y:=StringListStartsWith(ConfigText,FPCSnipMagic);
        if (y<>-1) then
        begin
          while (y<ConfigText.Count) AND (Length(ConfigText.Strings[y])>0) do Inc(y);
        end
        else y:=ConfigText.Count;

        // cleanup previous fpcup settings
        repeat
          x:=StringListStartsWith(ConfigText,'# fpcup:');
          if x=-1 then x:=StringListStartsWith(ConfigText,'# Fpcup[deluxe]:');
          if x=-1 then x:=StringListStartsWith(ConfigText,SnipMagicBegin+FPCUPMAGIC);

          if x<>-1 then
          begin
            // save position
            y:=x;

            // delete previous settings by fpcup[deluxe] by looking for some magic ... ;-)

            // remove beginmagic if any
            ConfigText.Delete(x);

            // remove all until endmagic if any
            while (x<ConfigText.Count) do
            begin
              if (ConfigText.Strings[x]=SnipMagicEnd) then
              begin
                ConfigText.Delete(x);
                break;
              end;
              if (Pos(SnipMagicBegin,ConfigText.Strings[x])=1) then break;
              ConfigText.Delete(x);
            end;

            // remove stray empty lines if any
            while (x<ConfigText.Count) AND (Length(ConfigText.Strings[x])=0) do ConfigText.Delete(x);
          end;
        until x=-1;

        if y=ConfigText.Count then
          //add empty line
          ConfigText.Append('')
        else
          begin
            // store tail of ConfigText
            for x:=y to (ConfigText.Count-1) do
              ConfigTextStore.Append(ConfigText.Strings[x]);

            // delete tail of ConfigText
            for x:=(ConfigText.Count-1) downto y do
              ConfigText.Delete(x);
          end;

        // add magic
        ConfigText.Append(SnipMagicBegin+FPCUPMAGIC);

        // add settings
        if Ultibo then
        begin
          ConfigText.Append('#ifdef ULTIBO');
          ConfigText.Append('# Search for CPU specific units');
          s:=ConcatPaths([InstallDirectory,'units',FPC_TARGET_MAGIC,FPC_SUBARCH_MAGIC]);
          ConfigText.Append('-Fu'+s);
          ConfigText.Append('-Fu'+s+DirectorySeparator+'*');
          ConfigText.Append('-Fu'+s+DirectorySeparator+'rtl');

          ConfigText.Append('#ifdef CPUARMV6');
          ConfigText.Append('# Search for CPU specific units');
          s:=ConcatPaths([InstallDirectory,'units','armv6-'+GetOS(TOS.ultibo)]);
          ConfigText.Append('-Fu'+s);
          ConfigText.Append('-Fu'+s+DirectorySeparator+'*');
          ConfigText.Append('-Fu'+s+DirectorySeparator+'rtl');
          ConfigText.Append('#endif');

          ConfigText.Append('#ifdef CPUARMV7A');
          ConfigText.Append('# Search for CPU specific units');
          s:=ConcatPaths([InstallDirectory,'units','armv7a-'+GetOS(TOS.ultibo)]);
          ConfigText.Append('-Fu'+s);
          ConfigText.Append('-Fu'+s+DirectorySeparator+'*');
          ConfigText.Append('-Fu'+s+DirectorySeparator+'rtl');
          ConfigText.Append('#endif');

          ConfigText.Append('#ifdef CPUARMV8');
          ConfigText.Append('# Search for CPU specific units');
          s:=ConcatPaths([InstallDirectory,'units','armv8-'+GetOS(TOS.ultibo)]);
          ConfigText.Append('-Fu'+s);
          ConfigText.Append('-Fu'+s+DirectorySeparator+'*');
          ConfigText.Append('-Fu'+s+DirectorySeparator+'rtl');
          //s:=ConcatPaths([InstallDirectory,'units',GetCPU(TCPU.aarch64)+'-'+GetOS(TOS.ultibo)]);
          //ConfigText.Append('-Fu'+s);
          //ConfigText.Append('-Fu'+s+DirectorySeparator+'*');
          //ConfigText.Append('-Fu'+s+DirectorySeparator+'rtl');
          ConfigText.Append('#endif');
          ConfigText.Append('#endif');
        end;

        ConfigText.Append('# Adding binary tools paths to');
        ConfigText.Append('# plain bin dir and architecture bin dir so');
        ConfigText.Append('# fpc 3.1+ fpcres etc can be found.');

        // On *nix FPC 3.1.x, both "architecture bin" and "plain bin" may contain tools like fpcres.
        // Adding this won't hurt on Windows.
        // Adjust for that
        PlainBinPath:=SafeExpandFileName(SafeExpandFileName(FPCBinDir+DirectorySeparator+'..'+DirectorySeparator+'..'));
        s:='-FD'+FPCBinDir+';'+ExcludeTrailingPathDelimiter(PlainBinPath);
        ConfigText.Append(s);
        {$IFDEF UNIX}
        // Need to add appropriate library search path
        // where it is e.g /usr/lib/arm-linux-gnueabihf...
        ConfigText.Append('# library search path');
        s:='-Fl/usr/lib/'+FPC_TARGET_MAGIC+';'+'/usr/lib/'+FPC_TARGET_MAGIC+'-gnu'+';'+'/lib/'+FPC_TARGET_MAGIC+';'+'/lib/'+FPC_TARGET_MAGIC+'-gnu';
        {$IFDEF cpuarm}
        {$IFDEF CPUARMHF}
        s:=s+';'+'/usr/lib/'+FPC_TARGET_MAGIC+'-gnueabihf';
        {$ELSE}
        s:=s+';'+'/usr/lib/'+FPC_TARGET_MAGIC+'-gnueabi';
        {$ENDIF CPUARMHF}
        {$ENDIF cpuarm}
        ConfigText.Append(s);
        {$ENDIF UNIX}

        ConfigText.Append('#IFNDEF FPC_CROSSCOMPILING');

        // Add the native compiler options as given by the user, to make them permanent
        if (Length(FCompilerOptions)>0) then ConfigText.AddDelimitedText(FCompilerOptions,' ',true);

        {$IFDEF UNIX}
        s:=GetStartupObjects;
        if Length(s)>0 then
        begin
          ConfigText.Append('-Fl'+s);
        end;

        {$ifdef Linux}
        if FMUSL then ConfigText.Append('-FL'+FMUSLLinker);

        ConfigText.Append('#IFDEF FPC_LINK_COMPAT');
        if (NOT UseLibc) then ConfigText.Append('-d'+DEFINE_FPC_LIBC);
        s:=ConcatPaths([InstallDirectory,'units',FPC_TARGET_MAGIC])+'_legacy';
        ConfigText.Append('-Fu'+s);
        ConfigText.Append('-Fu'+s+DirectorySeparator+'*');
        ConfigText.Append('-Fu'+s+DirectorySeparator+'rtl');
        ConfigText.Append('-k--no-as-needed');
        for s in LEGACYLIBSVERSIONED do
        begin
          if LibWhich(s) then ConfigText.Append('-k-l:'+s);
        end;
        ConfigText.Append('-k--as-needed');
        ConfigText.Append('#ENDIF');
        {$endif}

        //if (NOT LinuxLegacy) then
        if UseLibc then ConfigText.Append('-d'+DEFINE_FPC_LIBC);

        {$IF (defined(BSD)) and (not defined(Darwin))}
        s:='-Fl/usr/local/lib'+';'+'/usr/pkg/lib';
        {$ifndef FPCONLY}
        //VersionSnippet:=GetEnvironmentVariable('X11BASE');
        //if Length(VersionSnippet)>0 then s:=s+';'+VersionSnippet
        s:=s+';'+'/usr/X11R6/lib'+';'+'/usr/X11R7/lib';
        {$endif FPCONLY}
        ConfigText.Append(s);
        {$endif}

        {$ifdef FreeBSD}
        ConfigText.Append('-FD/usr/local/bin');
        {$endif}

        {$IF (defined(NetBSD)) and (not defined(Darwin))}
        {$ifndef FPCONLY}
        ConfigText.Append('-k"-rpath=/usr/X11R6/lib"');
        ConfigText.Append('-k"-rpath=/usr/X11R7/lib"');
        {$endif}
        ConfigText.Append('-k"-rpath=/usr/pkg/lib"');
        {$endif}

        {$ifdef Haiku}
          s:='';
          {$ifdef CPUX86}
          s:='/x86';
          {$endif}
          ConfigText.Append('-XR/boot/system/lib'+s);
          ConfigText.Append('-FD/boot/system/bin'+s+'/');
          ConfigText.Append('-Fl/boot/system/develop/lib'+s);
          ConfigText.Append('-Fl/boot/system/non-packaged/lib'+s);
        {$endif}

        {$ifdef solaris}
        {$IF defined(CPUX64) OR defined(CPUX86)}
        //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
        ConfigText.Append('-Xn');
        {$endif}
        {$endif}

        {$ENDIF UNIX}

        ConfigText.Append('#ENDIF');

        {$IFDEF DARWIN}
        ConfigText.Append('');
        ConfigText.Append('# Add some extra OSX options, if any');

        // Not needed anymore.
        (*
        if (Pos('-WM',FCompilerOptions)=0) then
        begin
          ConfigText.Append('#IFDEF DARWIN');
          s:=GetDarwinSDKVersion('macosx');
          if Length(s)>0 then
          begin
            ConfigText.Append('# Prevents crti not found linking errors');
            ConfigText.Append('#IFNDEF FPC_CROSSCOMPILING');
            //ConfigText.Append('#IFDEF CPU'+UpperCase(GetSourceCPU));
            if CompareVersionStrings(s,'10.9')>=0 then
              ConfigText.Append('-WM10.9')
            else
              ConfigText.Append('-WM'+s);
            ConfigText.Append('#ENDIF');
          end;
          ConfigText.Append('#ENDIF');
        end;
        *)

        s:=GetDarwinSDKVersion('macosx');
        if  (Length(s)=0) OR (CompareVersionStrings(s,'10.14')>=0) then
        begin
          ConfigText.Append('');
          if (Length(s)>0) then
            ConfigText.Append('# MacOS 10.14 Mojave and newer have libs and tools in new, yet non-standard directory');

          s:=GetDarwinSDKLocation;
          if (Length(s)>0) AND (DirectoryExists(s)) then
          begin
            ConfigText.Append('#IFDEF DARWIN');
            ConfigText.Append('-XR'+s);
            ConfigText.Append('#ENDIF');
            ConfigText.Append('-Fl'+s+'/usr/lib');
          end
          else
          begin
            // always add the default library location
            ConfigText.Append('-Fl/usr/lib');
          end;

          s:=GetDarwinToolsLocation;
          if (Length(s)>0) AND (DirectoryExists(s)) then
            ConfigText.Append('-FD'+s);
        end;
        {$ENDIF DARWIN}

        // add end magic
        ConfigText.Append(SnipMagicEnd);

        // add empty line
        ConfigText.Append('');

        // add tail of ConfigText
        for x:=0 to (ConfigTextStore.Count-1) do
          ConfigText.Append(ConfigTextStore.Strings[x]);

        x:=ConfigText.IndexOf('# searchpath for fppkg user-specific packages');
        if x>-1 then
        begin
          ConfigText.Strings[x+1]:='-Fu'+ConcatPaths([BaseDirectory,PACKAGESLOCATION,'units',FPC_TARGET_MAGIC])+'/*';
        end;

        ConfigText.SaveToFile(FPCCfg);
      finally
        ConfigText.Free;
        ConfigTextStore.Free;
      end;

      Infoln(infotext+'Tuning of fpc.cfg ready.',etDebug);
    end;

    // do not build pas2js [yet]: separate install ... use the module with rtl
    // if OperationSucceeded then BuildModuleCustom('PAS2JS');
  end;

  if OperationSucceeded then
  begin

    if FFPCUnicode then
    begin
      Infoln(infotext+'Building FPC Unicode RTL.',etInfo);

      s:=GetFPCConfigPath(FPCCONFIGUNICODE);
      if FileExists(s) then
      begin
        Infoln(localinfotext+'Unicode config already exists ('+s+'); trying to overwrite it.',etInfo);
        SysUtils.DeleteFile(s);
      end;
      AssignFile(TxtFile,s);
      try
        Rewrite(TxtFile);
        writeln(TxtFile,'-dUNICODERTL');
        writeln(TxtFile,'-Municodestrings');
      finally
        CloseFile(TxtFile);
      end;

      if BuildModuleCustom(_UNICODEFPC) then
      begin

      end;
    end;

    if (IsCross) then
    begin
      if Assigned(CrossInstaller) then result:=CleanExtra(CrossInstaller.TargetCPU,CrossInstaller.TargetOS);
    end;
    CleanExtra;
  end;
  Result := OperationSucceeded;
end;

function TFPCInstaller.CleanModule(ModuleName: string): boolean;
// Make distclean is unreliable; at least for FPC.
// Running it twice apparently can fix a lot of problems; see FPC ML message
// by Jonas Maebe, 1 November 2012
// On Windows, removing fpmake.exe, see Build FAQ (Nov 2011), 2.5
var
  CrossCompiling                         : boolean;
  ToolAvailable                          : boolean;
  CPUOS_Signature                        : string;
  aCleanupCompiler,aCleanupCommand,aPath : string;
  aStrList                               : TStringList;
  RunTwice                               : boolean;
  aCPU                                   : TCPU;
  {$IFDEF MSWINDOWS}
  DeleteList                             : TStringList;
  {$ENDIF}
  {$IFDEF UNIX}
  index                                  : integer;
  {$ENDIF}
begin
  result:=inherited;

  // if no sources, then exit;
  if result then exit;

  result:=InitModule;

  if (NOT result) then exit;

  CrossCompiling:=(IsCross AND Assigned(CrossInstaller));

  if CrossCompiling then
  begin
    CrossInstaller.Reset;

    CPUOS_Signature:=GetFPCTarget(false);
    // Delete any existing buildstamp file
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(SourceDirectory)+'build-stamp.'+CPUOS_Signature);
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(SourceDirectory)+'base.build-stamp.'+CPUOS_Signature);

    CrossInstaller.SetCrossOpt(CrossOPT);
    CrossInstaller.SetSubArch(CrossOS_SubArch);
    CrossInstaller.SetABI(CrossOS_ABI);

    Infoln(infotext+'Looking for crosstools and crosslibs on system. Please wait.',etInfo);

    // first, get/set cross binary utils !!
    ToolAvailable:=(NOT (ieBins in FErrorCodes));
    if (NOT ToolAvailable) then
    begin
      CrossInstaller.SearchModeUsed:=DEFAULTSEARCHSETTING;
      if Length(CrossToolsDirectory)>0 then
      begin
        // we have a crosstools setting
        if (CrossToolsDirectory=FPCUP_AUTO_MAGIC)
           then CrossInstaller.SearchModeUsed:=TSearchSetting.ssAuto
           else CrossInstaller.SearchModeUsed:=TSearchSetting.ssCustom;
      end;
      if CrossInstaller.SearchModeUsed=TSearchSetting.ssCustom
         then ToolAvailable:=CrossInstaller.GetBinUtils(CrossToolsDirectory)
         else ToolAvailable:=CrossInstaller.GetBinUtils(BaseDirectory);
      if (not ToolAvailable) then Infoln('Failed to get crossbinutils', etError);
      if (ToolAvailable) then
      begin
        Exclude(FErrorCodes,ieBins);

        // If we have our own binutils, set bins as executable on UNIX
        {$IFDEF UNIX}
        if (Pos(BaseDirectory,CrossInstaller.BinUtilsPath)=1) AND  (Pos(CROSSBINPATH,CrossInstaller.BinUtilsPath)>0) then
        begin
          aStrList:=FindAllFiles(CrossInstaller.BinUtilsPath,'',false);
          try
            if (aStrList.Count > 0) then
            begin
              for index:=0 to Pred(aStrList.Count) do
              begin
                fpChmod(aStrList.Strings[index],&755);
              end;
            end;
          finally
            aStrList.Free;
          end;
        end;
        {$ENDIF}
      end;
    end;

    // second, get/set cross libraries !!
    ToolAvailable:=(NOT (ieLibs in FErrorCodes));
    if (NOT ToolAvailable) then
    begin
      CrossInstaller.SearchModeUsed:=DEFAULTSEARCHSETTING;
      if Length(CrossLibraryDirectory)>0 then
      begin
        // we have a crosslibrary setting
        if (CrossToolsDirectory=FPCUP_AUTO_MAGIC)
           then CrossInstaller.SearchModeUsed:=TSearchSetting.ssAuto
           else CrossInstaller.SearchModeUsed:=TSearchSetting.ssCustom;
      end;
      if CrossInstaller.SearchModeUsed=TSearchSetting.ssCustom
        then ToolAvailable:=CrossInstaller.GetLibs(CrossLibraryDirectory)
        else ToolAvailable:=CrossInstaller.GetLibs(BaseDirectory);
      if (not ToolAvailable) then Infoln('Failed to get crosslibrary', etError);
      if (ToolAvailable) then Exclude(FErrorCodes,ieLibs);
    end;

    result:=(NOT ((ieLibs in FErrorCodes) OR (ieBins in FErrorCodes)));

  end else CPUOS_Signature:=GetFPCTarget(true);

  if (NOT result) then exit;

  {$IFDEF MSWINDOWS}
  // Remove all fpmakes
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(SourceDirectory)+'utils'+DirectorySeparator+'fpmake'+GetExeExt);
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(SourceDirectory)+'packages'+DirectorySeparator+'fpmake'+GetExeExt);
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(SourceDirectory)+'ide'+DirectorySeparator+'fpmake'+GetExeExt);
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('fpmake'+GetExeExt);
    DeleteFilesSubDirs(IncludeTrailingPathDelimiter(SourceDirectory),DeleteList,CPUOS_Signature);
  finally
    DeleteList.Free;
  end;
  {$ENDIF}

  aCleanupCompiler:='';
  if (SourceDirectory<>InstallDirectory) then
    aCleanupCompiler:=FPCBinDir+DirectorySeparator+GetCompilerName(GetSourceCPU);

  if (NOT FileExists(aCleanupCompiler)) then
  begin
    if FileExists(FCompiler)
       then aCleanupCompiler:=FCompiler
       else aCleanupCompiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetSourceCPU);
  end;

  if FileExists(aCleanupCompiler) then
  begin
    if CrossCompiling then
      CrossInstaller.SetFPCVersion(aCleanupCompiler);

    Processor.Executable:=Make;
    Processor.Process.Parameters.Clear;
    {$IFDEF MSWINDOWS}
    if Length(Shell)>0 then Processor.SetParamData('SHELL='+Shell);
    {$ENDIF}
    Processor.Process.CurrentDirectory:=SourceDirectory;
    if (NOT FNoJobs) then
    begin
      {$ifndef win64}
      Processor.SetParamData('--jobs='+IntToStr(FCPUCount));
      {$endif win64}
      Processor.SetParamData('FPMAKEOPT=--threads='+IntToStr(FCPUCount));
    end;
    Processor.SetParamData('--directory='+SourceDirectory);

    Processor.SetParamNamePathData('FPC',aCleanupCompiler);
    Processor.SetParamNamePathData('FPCMAKE',FPCBinDir+DirectorySeparator+'fpcmake'+GetExeExt);
    Processor.SetParamNamePathData('PPUMOVE',FPCBinDir+DirectorySeparator+'ppumove'+GetExeExt);
    Processor.SetParamNamePathData('FPCDIR',SourceDirectory);
    Processor.SetParamNamePathData('PREFIX',InstallDirectory);
    Processor.SetParamNamePathData('INSTALL_PREFIX',InstallDirectory);

    Processor.SetParamData('CPU_SOURCE='+GetSourceCPU);
    Processor.SetParamData('OS_SOURCE='+GetSourceOS);
    {$IFDEF MSWINDOWS}
    Processor.SetParamData('UPXPROG=echo'); //Don't use UPX
    //Processor.SetParamData('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    {$ENDIF}
    if (IsCross) then
    begin  // clean out the correct compiler
      Processor.SetParamData('CPU_TARGET='+CrossInstaller.TargetCPUName);
      Processor.SetParamData('OS_TARGET='+CrossInstaller.TargetOSName);
      if (CrossInstaller.SubArch<>TSubarch.saNone) then Processor.SetParamData('SUBARCH='+CrossInstaller.SubArchName);
    end
    else
    begin
      Processor.SetParamData('CPU_TARGET='+GetSourceCPU);
      Processor.SetParamData('OS_TARGET='+GetSourceOS);
    end;

    aStrList:=TStringList.Create;
    try
      aStrList.Append('distclean');

      if CrossCompiling then
      begin
        aStrList.Clear;
        if (ModuleName=_NATIVECROSSFPC) then
        begin
          //aStrList.Append('-C');
          //aStrList.Append('compiler');
          //aStrList.Append('clean');
          aStrList.Append('compiler_clean');
        end
        else
        begin
          {$ifndef crosssimple}
          if (Self AS TFPCCrossInstaller).CompilerUpdateNeeded then
            aStrList.Append('compiler_distclean')
          else
            Infoln(infotext+'Skipping cross-compiler clean step: compiler seems to be up to date !!',etInfo);
          aStrList.Append('rtl_distclean');
          if (Self AS TFPCCrossInstaller).PackagesNeeded then aStrList.Append('packages_distclean');
          {$else}
          aStrList.Append('clean');
          {$endif}
        end;
      end;

      if ModuleName=_UNICODEFPC then
        aStrList.Append('SUB_TARGET=unicodertl');

      for aCleanupCommand in aStrList do
        Processor.SetParamData(aCleanupCommand);

      aCleanupCommand:=aStrList.CommaText;

    finally
      aStrList.Free;
    end;

    for RunTwice in boolean do
    begin
      if (NOT RunTwice) then
      begin
        if (NOT CrossCompiling) then
          Infoln(infotext+'Running make '+aCleanupCommand+' twice',etInfo)
        else
          Infoln(infotext+'Running make '+aCleanupCommand+' twice for target '+CrossInstaller.RegisterName,etInfo);
      end;
      Infoln(infotext+'Running command. '+Processor.GetExeInfo,etDebug);
      try
        ProcessorResult:=Processor.ExecuteAndWait;
        result:=(ProcessorResult=0);
        if result then
          Sleep(200)
        else
          break;
      except
        on E: Exception do
        begin
          result:=false;
          WritelnLog(etError, infotext+'Running '+Processor.Executable+' distclean failed with an exception!'+LineEnding+'Details: '+E.Message,true);
        end;
      end;
    end;
    if result then FCleanModuleSuccess:=true;
  end
  else
  begin
    result:=true;
    Infoln(infotext+'Running '+Processor.Executable+' distclean failed: could not find cleanup compiler. Will try again later',etInfo);
  end;

  if FCleanModuleSuccess then
  begin
    if (ModuleName=_FPC) then
    begin
      if (NOT CrossCompiling) then
      begin
        //Infoln(infotext+'Deleting some FPC package config files.', etInfo);
        //DeleteFile(ConcatPaths([BaseDirectory,PACKAGESCONFIGDIR])+DirectorySeparator+FPCPKGCONFIGFILENAME);
        //DeleteFile(ConcatPaths([BaseDirectory,PACKAGESCONFIGDIR])+DirectorySeparator+FPCPKGCOMPILERTEMPLATE);
        {$ifdef FORCEREVISION}
        //Infoln(infotext+'Deleting '+REVINCFILENAME, etInfo);
        //aPath:=ConcatPaths([SourceDirectory,'compiler']);
        //DeleteFile(aPath+DirectorySeparator+REVINCFILENAME);
        {$endif FORCEREVISION}

        if DirectoryExists(FPCBinDir) then
        begin
          // Delete FPC binary
          aPath:=FPCBinDir+DirectorySeparator+'fpc'+GetExeExt;
          if FileExists(aPath) then
          begin
            Infoln(infotext+'Deleting '+ExtractFileName(aPath)+' executable.', etInfo);
            Sysutils.DeleteFile(aPath);
          end;

          // Delete any fpc.sh shell scripts
          {$IFDEF UNIX}
          aPath:=FPCBinDir+DirectorySeparator+'fpccompat.sh';
          Sysutils.DeleteFile(aPath);
          aPath:=FPCBinDir+DirectorySeparator+'fpc.sh';
          if FileExists(aPath) then
          begin
            Infoln(infotext+'Deleting '+ExtractFileName(aPath)+' script.', etInfo);
            Sysutils.DeleteFile(aPath);
            if (NOT UseCompilerWrapper) then
            begin
              // If we did delete fpc.sh, we also need to redefine/update the compiler path inside fpcpackageconfig
              // So, delete the config file to force creating of a new and correct one
              aPath := ConcatPaths([BaseDirectory,PACKAGESCONFIGDIR])+DirectorySeparator+FPCPKGCOMPILERTEMPLATE;
              if FileExists(aPath) then
              begin
                Infoln(infotext+'Deleting '+ExtractFileName(aPath)+' FPC package compiler configuration.', etInfo);
                Sysutils.DeleteFile(aPath);
              end;
            end;
          end;
          {$ENDIF UNIX}

          // Delete compiler binary
          aCPU:=GetTCPU(GetSourceCPU);
          if (aCPU<>TCPU.cpuNone) then
          begin
            aPath:=ConcatPaths([FPCBinDir,GetCompilerName(aCPU)]);
            if FileExists(aPath) then
            begin
              Infoln(infotext+'Deleting '+ExtractFileName(aPath)+' compiler.', etInfo);
              Sysutils.DeleteFile(aPath);
            end;
          end;

          // Delete all [cross-]compiler binaries
          // This is up to discussion
          // The cross-compiler settings are still inside fpc.cfg
          // And the cross-units are there also
          // This will result in an error when trying to cross-compile
          // But might be good: force update of cross-compilers by the user
          for aCPU in TCPU do
          begin
            if (aCPU=TCPU.cpuNone) then continue;
            aPath:=ConcatPaths([FPCBinDir,GetCrossCompilerName(aCPU)]);
            if FileExists(aPath) then
            begin
              Infoln(infotext+'Deleting '+ExtractFileName(aPath)+' cross-compiler.', etInfo);
              Sysutils.DeleteFile(aPath);
            end;
          end;

        end;
      end;

      // Delete all installed units
      // Alf: is it still needed: todo check
      aPath:=GetUnitsInstallDirectory;
      if DirectoryExists(aPath) then
      begin
        // Only allow unit directories inside our own install te be deleted
        if (Pos(BaseDirectory,aPath)=1) then
        begin
          Infoln(infotext+'Deleting '+aPath+' directory.', etInfo);
          DeleteDirectoryEx(aPath);
        end;
      end;
    end;

    // Final cleansing of source directory
    if CrossCompiling then
      CleanExtra(CrossInstaller.TargetCPU,CrossInstaller.TargetOS)
    else
      CleanExtra;
  end;

end;

function TFPCInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=true;

  GetVersion;
end;

function TFPCInstaller.GetModule(ModuleName: string): boolean;
var
  UpdateWarnings : TStringList;
  FilesList      : TStringList;
  aRepoClient    : TRepoClient;
  s              : string;
  SourceVersion  : string;
  aSF,aDP,aAD    : string;
  SourceInfo     : TRevision;
  i              : integer;
begin
  result:=inherited;
  result:=InitModule;

  if (not result) then exit;

  FPreviousRevision:=GetFPCRevision;

  SourceVersion:='0.0.0';

  //if Ultibo then
  //  SourceDirectory:=StringReplace(SourceDirectory,DirectorySeparator+'source','',[]);

  aRepoClient:=GetSuitableRepoClient;

  if aRepoClient=nil then
  begin
    result:=true;
    Infoln(infotext+'Downloading ' + ModuleName + ' sources.',etInfo);
    result:=DownloadFromURL(ModuleName);
    FActualRevision:=FPreviousRevision;
    if result and Ultibo then
    begin
      // Get Ultibo Core also
      s:=URL;
      URL:=StringReplace(URL,'/FPC','/Core',[]);
      Infoln(infotext+'Downloading Ultibo Core sources.',etInfo);
      result:=DownloadFromURL('Core');
      URL:=s;
      FActualRevision:='32846';
      FPreviousRevision:=FActualRevision;
    end;
  end
  else
  begin
    Infoln(infotext+'Start checkout/update of ' + ModuleName + ' sources.',etInfo);

    //git svn clone -r HEAD https://svn.freepascal.org/svn/fpc/tags/release_3_2_2

    UpdateWarnings:=TStringList.Create;
    try
      if (aRepoClient.ClassType=FGitClient.ClassType)
         then result:=DownloadFromGit(ModuleName, FPreviousRevision, FActualRevision, UpdateWarnings)
         else result:=DownloadFromSVN(ModuleName, FPreviousRevision, FActualRevision, UpdateWarnings);

      if UpdateWarnings.Count>0 then
      begin
        WritelnLog(UpdateWarnings);
      end;
    finally
      UpdateWarnings.Free;
    end;

    if result and Ultibo then
    begin
      // Get Ultibo Core also
      Infoln(infotext+'Downloading Ultibo Core sources.',etInfo);
      s := GetTempFileNameExt('FPCUPTMP','zip');
      aAD:=ExtractFileDir(s);
      result:=GetFile('https://github.com/ultibohub/Core/archive/refs/heads/master.zip',s);
      if (result AND (NOT FileExists(s))) then result:=false;
      if result then
      begin
        with TNormalUnzipper.Create do
        begin
          try
            result:=DoUnZip(s,aAD,[]);
          finally
            Free;
          end;
        end;
      end;
      if result then SysUtils.Deletefile(s); //Get rid of archive file file.
      if result then
      begin

        s:=ConcatPaths([aAD,'Core-master','source']);
        FilesList:=FindAllFiles(s, '', True);
        for i:=0 to (FilesList.Count-1) do
        begin
          aSF:=FilesList[i];
          aDP:=ConcatPaths([SourceDirectory,ExtractRelativePath(IncludeTrailingPathDelimiter(s),ExtractFilePath(aSF))]);
          ForceDirectoriesSafe(aDP);
          MoveFile(aSF,IncludeTrailingPathDelimiter(aDP)+ExtractFileName(aSF));
        end;
        FreeAndNil(FilesList);

        s:=ConcatPaths([aAD,'Core-master','units','armv6-'+GetOS(TOS.ultibo),'lib']);
        FilesList:=FindAllFiles(s, '', True);
        for i:=0 to (FilesList.Count-1) do
        begin
          aSF:=FilesList[i];
          aDP:=ConcatPaths([ConcatPaths([BaseDirectory,{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}(CROSSLIBPATH),GetCPU(TCPU.arm)+'-'+GetOS(TOS.ultibo),'armv6']),ExtractRelativePath(IncludeTrailingPathDelimiter(s),ExtractFilePath(aSF))]);
          ForceDirectoriesSafe(aDP);
          MoveFile(aSF,IncludeTrailingPathDelimiter(aDP)+ExtractFileName(aSF));
        end;
        FreeAndNil(FilesList);

        s:=ConcatPaths([aAD,'Core-master','units','armv7-'+GetOS(TOS.ultibo),'lib']);
        FilesList:=FindAllFiles(s, '', True);
        for i:=0 to (FilesList.Count-1) do
        begin
          aSF:=FilesList[i];
          aDP:=ConcatPaths([ConcatPaths([BaseDirectory,{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}(CROSSLIBPATH),GetCPU(TCPU.arm)+'-'+GetOS(TOS.ultibo),'armv7a']),ExtractRelativePath(IncludeTrailingPathDelimiter(s),ExtractFilePath(aSF))]);
          ForceDirectoriesSafe(aDP);
          MoveFile(aSF,IncludeTrailingPathDelimiter(aDP)+ExtractFileName(aSF));
        end;
        FreeAndNil(FilesList);

        s:=ConcatPaths([aAD,'Core-master','units','armv8-'+GetOS(TOS.ultibo),'lib']);
        FilesList:=FindAllFiles(s, '', True);
        for i:=0 to (FilesList.Count-1) do
        begin
          aSF:=FilesList[i];
          aDP:=ConcatPaths([ConcatPaths([BaseDirectory,{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}(CROSSLIBPATH),GetCPU(TCPU.aarch64)+'-'+GetOS(TOS.ultibo),'armv8']),ExtractRelativePath(IncludeTrailingPathDelimiter(s),ExtractFilePath(aSF))]);
          ForceDirectoriesSafe(aDP);
          MoveFile(aSF,IncludeTrailingPathDelimiter(aDP)+ExtractFileName(aSF));
        end;
        FreeAndNil(FilesList);

      end;

      DeleteDirectory(aAD,False);

    end;

  end;

  //if Ultibo then
  //  SourceDirectory:=IncludeTrailingPathDelimiter(SourceDirectory)+'source';

  if result then
  begin
    SourceVersion:=GetVersion;
    if (SourceVersion='0.0.0') then
    begin
      Infoln(infotext+'Could not get version of ' + ModuleName + ' sources. Expect severe errors.',etError);
    end;

    if Assigned(aRepoClient) then
    begin
      if (aRepoClient.ClassType=FSVNClient.ClassType) then SourceInfo.SVNRevision:=aRepoClient.LocalRevision;
      if (aRepoClient.ClassType=FGitClient.ClassType) then SourceInfo.GITHash:=aRepoClient.LocalRevision;
    end;

    if FRepositoryUpdated then
    begin
      Infoln(infotext+ModuleName + ' was at revision/hash: '+PreviousRevision,etInfo);
      Infoln(infotext+ModuleName + ' is now at revision/hash: '+ActualRevision,etInfo);
    end
    else
    begin
      Infoln(infotext+ModuleName + ' is at revision/hash: '+ActualRevision,etInfo);
      Infoln(infotext+'No updates for ' + ModuleName + ' found.',etInfo);
    end;
    UpdateWarnings:=TStringList.Create;
    try
      s:=SafeExpandFileName(IncludeTrailingPathDelimiter(BaseDirectory)+REVISIONSLOG);
      if FileExists(s) then
        UpdateWarnings.LoadFromFile(s)
      else
      begin
        UpdateWarnings.Add('New install.');
        UpdateWarnings.Add('Date: '+DateTimeToStr(now));
        UpdateWarnings.Add('Location: '+BaseDirectory);
        UpdateWarnings.Add('');
      end;
      UpdateWarnings.Add(FPCDATEMAGIC+DateTimeToStr(now));
      if Assigned(aRepoClient) AND (aRepoClient.ClassType=FGitClient.ClassType) then
        UpdateWarnings.Add(FPCNAMEMAGIC+aRepoClient.GetCommitName);
      if Assigned(aRepoClient) then UpdateWarnings.Add(ModuleName+' URL: '+aRepoClient.Repository);
      UpdateWarnings.Add(ModuleName+' previous rev/hash: '+PreviousRevision);
      if Length(SourceInfo.SVNRevision)>0 then
        UpdateWarnings.Add(FPCREVMAGIC+SourceInfo.SVNRevision)
      else
      if Length(SourceInfo.GITHash)>0 then
        UpdateWarnings.Add(FPCHASHMAGIC+SourceInfo.GITHash)
      else
      if Length(ActualRevision)>0 then
        UpdateWarnings.Add(FPCREVMAGIC+ActualRevision);
      UpdateWarnings.Add('');
      UpdateWarnings.SaveToFile(s);
    finally
      UpdateWarnings.Free;
    end;

    {$ifdef FORCEREVISION}
    CreateRevision(ModuleName,ActualRevision);
    {$endif FORCEREVISION}

    if (SourceVersion<>'0.0.0') then
    begin
      // Patch the FPC sources, if there are patches available
      PatchModule(ModuleName);
    end;
  end
  else
  begin
    Infoln(infotext+'Checkout/update of ' + ModuleName + ' sources failure.',etError);
  end;
end;

function TFPCInstaller.CheckModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=inherited;
end;

function TFPCInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=InitModule;

  if not result then exit;

  //sanity check
  if FileExists(IncludeTrailingPathDelimiter(SourceDirectory)+MAKEFILENAME) and
    DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory)+'compiler') and
    DirectoryExists(IncludeTrailingPathDelimiter(SourceDirectory)+'rtl') and
    ParentDirectoryIsNotRoot(IncludeTrailingPathDelimiter(SourceDirectory)) then
    begin
    if DeleteDirectoryEx(SourceDirectory)=false then
    begin
      WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+SourceDirectory);
      result:=false;
    end
    else
    result:=true;
    end
  else
  begin
    WritelnLog(infotext+'Invalid '+ModuleName+' directory :'+SourceDirectory);
    result:=false;
  end;
end;

constructor TFPCInstaller.Create;
begin
  inherited Create;

  FTargetCompilerName:=GetCompilerName(GetSourceCPU);

  FCompiler                    := '';
  FUseLibc                     := false;
  FUseRevInc                   := false;

  InitDone   := false;
end;

destructor TFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

