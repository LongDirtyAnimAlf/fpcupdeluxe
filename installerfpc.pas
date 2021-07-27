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

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils;

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

    _ENDFINAL;

type
  { TFPCInstaller }

  TFPCInstaller = class(TBaseFPCInstaller)
  private
    FSoftFloat  : boolean;
    FUseLibc    : boolean;
    FTargetCompilerName: string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
    FBootstrapCompilerOverrideVersionCheck: boolean; //Indicate to make we really want to compile with this version (e.g. trunk compiler), even if it is not the latest stable version
    FNativeFPCBootstrapCompiler: boolean;
    InitDone: boolean;
    function GetCompilerVersionNumber(aVersion: string; const index:byte=0): integer;
  protected
    function GetVersionFromUrl(aUrl: string): string;override;
    function GetVersionFromSource(aSourcePath: string): string;override;
    function GetReleaseCandidateFromSource({%H-}aSourcePath:string):integer;override;
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
    function InitModule(aBootstrapVersion:string=''):boolean;
  public
    property UseLibc: boolean read FUseLibc;
    property SoftFloat: boolean write FSoftFloat;
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
    // If yes, an override option will be passed to make (OVERRIDEVERSIONCHECK=1)
    // If no, the FPC make script enforces that the latest stable FPC bootstrap compiler is used.
    // This is required information for setting make file options
    property CompilerOverrideVersionCheck: boolean read FBootstrapCompilerOverrideVersionCheck;
    //Indicate to use FPC bootstrappers from FTP server
    property NativeFPCBootstrapCompiler: boolean read FNativeFPCBootstrapCompiler write FNativeFPCBootstrapCompiler;

    property TargetCompilerName: string read FTargetCompilerName;

    function UnInstallModule(ModuleName:string): boolean; override;
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
  private
    FCrossCompilerName: string;
    function SubarchTarget:boolean;
    function CompilerUpdateNeeded:boolean;
    function PackagesNeeded:boolean;
    function InsertFPCCFGSnippet(FPCCFG,Snippet: string): boolean;
    property CrossCompilerName: string read FCrossCompilerName;
  protected
    function GetUnitsInstallDirectory(WithMagic:boolean):string;
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
  repoclient
  {$IFDEF UNIX}
    ,baseunix
    ,LazFileUtils
  {$ENDIF UNIX}
  {$IFDEF BSD}
    ,math
  {$ENDIF}
  ;

// remove stale compiled files
procedure RemoveStaleBuildDirectories(aBaseDir,aCPU,aOS:string);
var
  OldPath:string;
  FileInfo: TSearchRec;
  //DeleteList:TStringList;
  aArch:string;
begin

  aArch:=aCPU+'-'+aOS;

  {
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('.fpm');
    OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'utils';
    DeleteFilesExtensionsSubdirs(OldPath,DeleteList,aArch);
    OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'packages';
    DeleteFilesExtensionsSubdirs(OldPath,DeleteList,aArch);
  finally
    DeleteList.Free;
  end;
  }
  OldPath:=IncludeTrailingPathDelimiter(aBaseDir);
  DeleteFilesNameSubdirs(OldPath,'.stackdump');
  DeleteFilesNameSubdirs(OldPath,'.core');

  // patch residues
  //DeleteFilesNameSubdirs(OldPath,'.rej');
  //DeleteFilesNameSubdirs(OldPath,'.orig');

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'utils';
  DeleteFilesNameSubdirs(OldPath,aArch+'.fpm');
  DeleteFilesNameSubdirs(OldPath,'-'+aOS+'.fpm');
  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'packages';
  DeleteFilesNameSubdirs(OldPath,aArch+'.fpm');
  DeleteFilesNameSubdirs(OldPath,'-'+aOS+'.fpm');

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'utils'+DirectorySeparator+'bin';
  DeleteDirectoryEx(OldPath);
  RemoveDir(IncludeTrailingPathDelimiter(aBaseDir)+'utils'+DirectorySeparator+'bin');

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'utils'+DirectorySeparator+'units'+DirectorySeparator+aArch;
  DeleteDirectoryEx(OldPath);
  RemoveDir(IncludeTrailingPathDelimiter(aBaseDir)+'utils'+DirectorySeparator+'units');

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'rtl'+DirectorySeparator+'units'+DirectorySeparator+aArch;
  DeleteDirectoryEx(OldPath);
  RemoveDir(IncludeTrailingPathDelimiter(aBaseDir)+'rtl'+DirectorySeparator+'units');

  DeleteDirectoryEx(IncludeTrailingPathDelimiter(aBaseDir)+'ide'+DirectorySeparator+'units'+DirectorySeparator+aArch);
  RemoveDir(IncludeTrailingPathDelimiter(aBaseDir)+'ide'+DirectorySeparator+'units');

  DeleteDirectoryEx(IncludeTrailingPathDelimiter(aBaseDir)+'ide'+DirectorySeparator+'bin'+DirectorySeparator+aArch);
  RemoveDir(IncludeTrailingPathDelimiter(aBaseDir)+'ide'+DirectorySeparator+'bin');

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'packages'+DirectorySeparator;
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        if (FileInfo.Attr and faDirectory) = faDirectory then
        begin
          DeleteDirectoryEx(OldPath+FileInfo.Name+DirectorySeparator+'units'+DirectorySeparator+aArch);
          RemoveDir(OldPath+FileInfo.Name+DirectorySeparator+'units');
        end;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
    SysUtils.FindClose(FileInfo);
  end;

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'utils'+DirectorySeparator;
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        if (FileInfo.Attr and faDirectory) = faDirectory then
        begin
          DeleteDirectoryEx(OldPath+FileInfo.Name+DirectorySeparator+'units'+DirectorySeparator+aArch);
          RemoveDir(OldPath+FileInfo.Name+DirectorySeparator+'units');

          DeleteDirectoryEx(OldPath+FileInfo.Name+DirectorySeparator+'bin'+DirectorySeparator+aArch);
          RemoveDir(OldPath+FileInfo.Name+DirectorySeparator+'bin');
        end;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
    SysUtils.FindClose(FileInfo);
  end;

  // for (very) old versions of FPC : fcl and fv directories
  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'fcl'+DirectorySeparator;
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        if (FileInfo.Attr and faDirectory) = faDirectory then
        begin
          DeleteDirectoryEx(OldPath+FileInfo.Name+DirectorySeparator+'units'+DirectorySeparator+aArch);
          RemoveDir(OldPath+FileInfo.Name+DirectorySeparator+'units');
        end;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
    SysUtils.FindClose(FileInfo);
  end;
  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'fv'+DirectorySeparator;
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        if (FileInfo.Attr and faDirectory) = faDirectory then
        begin
          DeleteDirectoryEx(OldPath+FileInfo.Name+DirectorySeparator+'units'+DirectorySeparator+aArch);
          RemoveDir(OldPath+FileInfo.Name+DirectorySeparator+'units');
        end;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
    SysUtils.FindClose(FileInfo);
  end;

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'compiler'+DirectorySeparator;
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        if (FileInfo.Attr and faDirectory) = faDirectory then
        begin
          DeleteDirectoryEx(OldPath+FileInfo.Name+DirectorySeparator+'units'+DirectorySeparator+aArch);
          RemoveDir(OldPath+FileInfo.Name+DirectorySeparator+'units');
        end;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
    SysUtils.FindClose(FileInfo);
  end;

end;

{ TFPCCrossInstaller }

constructor TFPCCrossInstaller.Create;
begin
  inherited Create;
  //if Self is TFPCCrossInstaller then
  //Self.
  FCrossCompilerName:='invalid';
end;

destructor TFPCCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

function TFPCCrossInstaller.GetUnitsInstallDirectory(WithMagic:boolean):string;
var
  aDir:string;
  ABIMagic:string;
  SUBARCHMagic:string;
begin
  if NOT Assigned(CrossInstaller) then exit;

  // Standard directory
  aDir:=ConcatPaths([FInstallDirectory,'units']);
  {$ifdef UNIX}
  if FileIsSymlink(aDir) then
  begin
    try
      aDir:=GetPhysicalFilename(aDir,pfeException);
    except
    end;
  end;
  {$endif UNIX}
  result:=ConcatPaths([aDir,CrossInstaller.RegisterName]);
  //result:=ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName]);

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
      result:=ConcatPaths([FInstallDirectory,'units',CrossInstaller.TargetOSName+'-'+SUBARCHMagic])
    else
    begin
      if CrossInstaller.TargetCPU=TCPU.arm then
        result:=ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName,SUBARCHMagic,ABIMagic])
      else
        result:=ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName,SUBARCHMagic]);
    end;
  end;
end;

function TFPCCrossInstaller.SubarchTarget:boolean;
begin
  result:=false;
  if (NOT Assigned(CrossInstaller)) then exit;
  result:=((CrossInstaller.TargetCPU<>TCPU.cpuNone) AND (CrossInstaller.TargetOS<>TOS.osNone) AND (CrossInstaller.TargetOS in SUBARCH_OS) AND (CrossInstaller.TargetCPU in SUBARCH_CPU));
end;

function TFPCCrossInstaller.CompilerUpdateNeeded:boolean;
var
  NativeVersion,CrossVersion:string;
  NativeCompiler,CrossCompiler:string;
  NativeAge,CrossAge:Longint;
begin
  result:=true;
  NativeCompiler:=GetFPCInBinDir;
  if (FileExists(NativeCompiler)) then
  begin
    CrossCompiler:=ExtractFilePath(NativeCompiler)+CrossCompilerName;
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
        if (NativeAge>=CrossAge) then result:=true
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

  if (CrossInstaller.TargetCPU=TCPU.wasm32) then result:=false;
end;

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
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
begin
  result:=true;

  if (NOT Assigned(CrossInstaller)) OR (NOT FileExists(FPCCFG)) then
  begin
    exit(false);
  end;

  NewSnipped:=false;

  //Set CPU
  //s2:=UpperCase(CrossInstaller.TargetCPUName);
  //if (CrossInstaller.TargetCPU=TCPU.powerpc) then
  //begin
  //  s2:='POWERPC32'; //Distinguish between 32 and 64 bit powerpc
  //end;

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
        ConfigText.Append('# Inserted by fpcup '+DateTimeToStr(Now));
        ConfigText.Append('# Cross compile settings dependent on both target OS and target CPU');
        ConfigText.Append('#IFDEF FPC_CROSSCOMPILING');
        ConfigText.Append('#IFDEF '+UpperCase(CrossInstaller.TargetOSName));
        ConfigText.Append('#IFDEF CPU'+UpperCase(CrossInstaller.TargetCPUName));

        // Just add new snipped
        if SnippetText.Count>0 then
        begin
          for i:=0 to (SnippetText.Count-1) do
            ConfigText.Append(SnippetText.Strings[i]);
        end;

        ConfigText.Append('#ENDIF CPU'+UpperCase(CrossInstaller.TargetCPUName));
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
        j:=StringListSame(ConfigText,'#IFDEF CPU'+CrossInstaller.TargetCPUName,SnipBegin);
        if (j>SnipEnd) then j:=-1;
        if (j=-1) then
        begin
          // This is a severe error and should never happen
          // Do not yet know how to handle it
          result:=false;
        end
        else
        begin
          i:=StringListSame(ConfigText,'#ENDIF CPU'+CrossInstaller.TargetCPUName,j);
          if (i=-1) then
          begin
            // Old versions of fpcupdeluxe used a simple 3 x #ENDIF ... check for this !!
            i:=StringListSame(ConfigText,'#ENDIF',j);
            if (i<>-1) then
            begin
              if ((i+2)>Pred(ConfigText.Count)) OR (ConfigText[i+1]<>'#ENDIF') OR (ConfigText[i+2]<>'#ENDIF') then
                i:=-1;
            end;
          end;
          if (i>SnipEnd) then i:=-1;
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
              j:=StringListSame(ConfigText,'#IFDEF CPU'+GetSubarch(SUBARCH),SnipBegin);
              if (j>SnipEnd) then j:=-1;
              if (j=-1) then break;
              i:=StringListSame(ConfigText,'#ENDIF CPU'+GetSubarch(SUBARCH),j);
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
                j:=StringListSame(SnippetText,'#IFDEF CPU'+CrossInstaller.SubArchName,0);
                if (j=-1) then break;
                // We have a subarch
                // Now, add only subarch part.
                i:=StringListSame(SnippetText,'#ENDIF CPU'+CrossInstaller.SubArchName,j);
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
    FCrossCompilerName:=GetCrossCompilerName(CrossInstaller.TargetCPU);
end;

function TFPCCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
// Runs make/make install for cross compiler.
// Error out on problems; unless module considered optional, i.e. in
// crosswin32-64 and crosswin64-32 steps.
type
  {$ifdef crosssimple}
  TSTEPS = (st_MakeAll,st_MakeCrossInstall);
  {$else}
  TSTEPS = (st_Compiler,st_CompilerInstall,st_Rtl,st_RtlInstall,st_Packages,st_PackagesInstall,st_NativeCompiler);
  {$endif}
var
  FPCCfg:String; //path+filename of the fpc.cfg configuration file
  CrossOptions:String;
  i,j:integer;
  OldPath:String;
  Options:String;
  s1,s2:string;
  UnitSearchPath:string;
  LibsAvailable,BinsAvailable:boolean;
  MakeCycle:TSTEPS;
  //ARMArch:TARMARCH;
  SupportedList:TStringList;
  {$ifdef MSWINDOWS}
  Counter:integer;
  {$endif}
begin
  result:=inherited;
  result:=false; //fail by default

  if assigned(CrossInstaller) then
  begin
    CrossInstaller.Reset;

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
    CrossInstaller.SetCrossOpt(CrossOPT);
    CrossInstaller.SetSubArch(CrossOS_SubArch);
    CrossInstaller.SetABI(CrossOS_ABI);

    Infoln(infotext+'Looking for crosstools and crosslibs on system. Please wait.',etInfo);

    // first, get/set cross binary utils !!
    BinsAvailable:=false;
    CrossInstaller.SearchModeUsed:=TSearchSetting.ssUp; // default;
    if Length(CrossToolsDirectory)>0 then
    begin
      // we have a crosstools setting
      if (CrossToolsDirectory=FPCUP_AUTO_MAGIC)
         then CrossInstaller.SearchModeUsed:=TSearchSetting.ssAuto
         else CrossInstaller.SearchModeUsed:=TSearchSetting.ssCustom;
    end;
    if CrossInstaller.SearchModeUsed=TSearchSetting.ssCustom
       then BinsAvailable:=CrossInstaller.GetBinUtils(CrossToolsDirectory)
       else BinsAvailable:=CrossInstaller.GetBinUtils(FBaseDirectory);
    if (not BinsAvailable) then Infoln('Failed to get crossbinutils', etError);

    // second, get/set cross libraries !!
    LibsAvailable:=false;
    CrossInstaller.SearchModeUsed:=TSearchSetting.ssUp;
    if Length(CrossLibraryDirectory)>0 then
    begin
      // we have a crosslibrary setting
      if (CrossLibraryDirectory=FPCUP_AUTO_MAGIC)
         then CrossInstaller.SearchModeUsed:=TSearchSetting.ssAuto
         else CrossInstaller.SearchModeUsed:=TSearchSetting.ssCustom;
    end;
    if CrossInstaller.SearchModeUsed=TSearchSetting.ssCustom
      then LibsAvailable:=CrossInstaller.GetLibs(CrossLibraryDirectory)
      else LibsAvailable:=CrossInstaller.GetLibs(FBaseDirectory);
    if (not LibsAvailable) then Infoln('Failed to get crosslibrary', etError);

    result:=(BinsAvailable AND LibsAvailable);

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

      FPCCfg := FFPCCompilerBinPath + FPCCONFIGFILENAME;

      // Add binutils path to path if necessary
      OldPath:=GetPath;
      try
        if CrossInstaller.BinUtilsPathInPath then
           SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),false,true);

        //SetPath(ConcatPaths([FSourceDirectory,'rtl',CrossInstaller.TargetOSName]),true,false);
        //SetPath(ConcatPaths([FSourceDirectory,'rtl',CrossInstaller.TargetOSName,CrossInstaller.TargetCPUName]),true,false);

        // Use own tools first
        {$ifdef MSWINDOWS}
        s2:=Which('echo.exe');
        if (Length(s2)=0) then s2:=Which('sh.exe');
        if (Length(s2)>0) then
        begin
          // We may have a stray shell (msys among others ... remove from path
          s1:=GetPath;
          s2:=ExtractFileDir(s2);
          s1:=StringReplace(s1,s2+';','',[]);
          s1:=StringReplace(s1,s2+DirectorySeparator+';','',[]);
          s1:=StringReplace(s1,s2,'',[]);
          SetPath(s1,false,false);
        end;
        {$endif MSWINDOWS}

        for MakeCycle:=Low(TSTEPS) to High(TSTEPS) do
        begin

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

          if (MakeCycle=st_Rtl) then
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
                s1:=ConcatPaths([FSourceDirectory,'compiler','ppcross'+ppcSuffix[CrossInstaller.TargetCPU]]);
                // Get the correct name of the cross-compiler in install-directory
                if (NOT FileExists(s1)) then
                  s1:=FFPCCompilerBinPath+CrossCompilerName;
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
                s1:=ConcatPaths([FSourceDirectory,'compiler','ppcross'+ppcSuffix[CrossInstaller.TargetCPU]]);
                // Get the correct name of the cross-compiler in install-directory
                if (NOT FileExists(s1)) then
                  s1:=FFPCCompilerBinPath+CrossCompilerName;
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

          // Modify fpc.cfg
          // always add this, to be able to detect which cross-compilers are installed
          // helpfull for later bulk-update of all cross-compilers

          if (MakeCycle=Low(TSTEPS)) OR (MakeCycle=High(TSTEPS)) then
          begin
            //Set basic config text
            s1:='# Dummy (blank) config just to replace dedicated settings during build of cross-compiler'+LineEnding;

            //Remove dedicated settings of config snippet
            if MakeCycle=Low(TSTEPS) then
              Infoln(infotext+'Removing '+FPCCONFIGFILENAME+' config snippet for target '+CrossInstaller.RegisterName,etInfo);

            //Add config snippet
            if (MakeCycle=High(TSTEPS)) then
            begin
              s1:='';
              Infoln(infotext+'Adding '+FPCCONFIGFILENAME+' config snippet for target '+CrossInstaller.RegisterName,etInfo);

              if CrossInstaller.FPCCFGSnippet<>'' then
                s1:=s1+CrossInstaller.FPCCFGSnippet+LineEnding;

              if (CrossInstaller.TargetOS=TOS.java) then
                //s1:=s1+'-Fu'+ConcatPaths([FInstallDirectory,'units','$FPCTARGET','rtl','org','freepascal','rtl'])+LineEnding;
                s1:=s1+'-Fu'+ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName,'rtl','org','freepascal','rtl'])+LineEnding;

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
                    FileCreate(ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName,'system.ppu']));
                  //{$endif MSWINDOWS}
                end;
              end;

              if (Length(s1)=0) then s1:='# Dummy (blank) config for auto-detect cross-compilers'+LineEnding;
            end;

            //Edit dedicated settings of config snippet
            InsertFPCCFGSnippet(FPCCfg,s1);

            {$ifdef UNIX}
            //Correct for some case errors on Unixes
            if (CrossInstaller.TargetOS=TOS.java) then
            begin
              s1:=ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName,'rtl','org','freepascal','rtl']);
              s2:=IncludeTrailingPathDelimiter(s1)+'System.class';
              s1:=IncludeTrailingPathDelimiter(s1)+'system.class';
              if (NOT FileExists(s1)) then FileUtil.CopyFile(s2,s1);
            end;
            {$endif}

          end;

          Processor.Executable := Make;
          Processor.Process.Parameters.Clear;
          {$IFDEF MSWINDOWS}
          if Length(Shell)>0 then Processor.Process.Parameters.Add('SHELL='+Shell);
          {$ENDIF}
          Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);

          //Still not clear if jobs can be enabled for crosscompiler builds ... :-|
          //However, on Windows, erroros occur frequently due to more jobs.
          //So, again, disabling for the time being.
          {
          if (NOT FNoJobs) then
          begin
            Processor.Process.Parameters.Add('--jobs='+IntToStr(FCPUCount));
            Processor.Process.Parameters.Add('FPMAKEOPT=--threads='+IntToStr(FCPUCount));
          end;
          }

          Processor.Process.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FSourceDirectory));
          Processor.Process.Parameters.Add('FPCMAKE=' + FFPCCompilerBinPath+'fpcmake'+GetExeExt);
          Processor.Process.Parameters.Add('PPUMOVE=' + FFPCCompilerBinPath+'ppumove'+GetExeExt);
          Processor.Process.Parameters.Add('PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
          Processor.Process.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));

          (*
          {$IFDEF UNIX}
          s1:=ConcatPaths([FInstallDirectory,'lib','fpc',GetFPCVersion]);
          {$ELSE}
          s1:=ExcludeTrailingPathDelimiter(FSourceDirectory);
          {$ENDIF UNIX}
          *)
          s1:=ExcludeTrailingPathDelimiter(FSourceDirectory);
          Processor.Process.Parameters.Add('FPCDIR=' + s1);

          {$IFDEF MSWINDOWS}
          //Processor.Process.Parameters.Add('ECHO='+ExtractFilePath(Make)+'gecho.exe');
          Processor.Process.Parameters.Add('UPXPROG=echo'); //Don't use UPX
          //Processor.Process.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
          {$ELSE}
          Processor.Process.Parameters.Add('INSTALL_BINDIR='+ExcludeTrailingPathDelimiter(FFPCCompilerBinPath));
          {$ENDIF}
          // Tell make where to find the target binutils if cross-compiling:
          // Not strictly necessary: the cross-options have this already:
          if CrossInstaller.BinUtilsPath<>'' then
             Processor.Process.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));

          Options:=FCompilerOptions;

          //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
          //Todo: to be investigated
          Processor.Process.Parameters.Add('FPCFPMAKE='+FCompiler);

          {$ifdef crosssimple}
          Processor.Process.Parameters.Add('FPC='+FCompiler);
          case MakeCycle of
            st_MakeAll:
            begin
              Processor.Process.Parameters.Add('all');
            end;
            st_MakeCrossInstall:
            begin
              Processor.Process.Parameters.Add('crossinstall');
            end;
          end;
          {$else crosssimple}
          case MakeCycle of
            st_Compiler:
            begin
              Processor.Process.Parameters.Add('FPC='+FCompiler);
              Processor.Process.Parameters.Add('compiler_cycle');
            end;
            st_CompilerInstall:
            begin
              {$if (defined(Linux))}
              {$if (defined(CPUAARCH64)) OR (defined(CPUX86_64))}
              if FMUSL then
              begin
                // copy over the [cross-]compiler
                s1:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+GetCompilerName(CrossInstaller.TargetCPU);
                s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+CrossCompilerName;
                if FileExists(s1) then
                begin
                  Infoln(infotext+'Copy [cross-]compiler ('+ExtractFileName(s1)+') into: '+ExtractFilePath(s2),etInfo);
                  FileUtil.CopyFile(s1,s2);
                  fpChmod(s2,&755);
                end;
              end;
              {$endif}
              {$endif}
              Processor.Process.Parameters.Add('FPC='+FCompiler);
              Processor.Process.Parameters.Add('compiler_install');
            end;
            st_Rtl:
            begin
              s1:=CrossCompilerName;
              s2:=FFPCCompilerBinPath+s1;
              if (NOT FileExists(s2)) then
                s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1;
              Processor.Process.Parameters.Add('FPC='+s2);
              Processor.Process.Parameters.Add('rtl_all');
            end;
            st_RtlInstall:
            begin
              s1:=CrossCompilerName;
              s2:=FFPCCompilerBinPath+s1;
              if (NOT FileExists(s2)) then
                s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1;
              Processor.Process.Parameters.Add('FPC='+s2);
              Processor.Process.Parameters.Add('rtl_install');
            end;
            st_Packages:
            begin
              s1:=CrossCompilerName;
              s2:=FFPCCompilerBinPath+s1;
              if (NOT FileExists(s2)) then
                s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1;
              Processor.Process.Parameters.Add('FPC='+s2);
              Processor.Process.Parameters.Add('packages_all');
            end;
            st_PackagesInstall:
            begin
              s1:=CrossCompilerName;
              s2:=FFPCCompilerBinPath+s1;
              if (NOT FileExists(s2)) then
                s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1;
              Processor.Process.Parameters.Add('FPC='+s2);
              Processor.Process.Parameters.Add('packages_install');
            end;
            st_NativeCompiler:
            begin
              {$ifdef buildnative}
              if (
                //Only native compiler if we have libs of if we do not need libs !!
                ( (CrossInstaller.LibsPath<>'') OR (CrossInstaller.TargetOS=TOS.win32) OR (CrossInstaller.TargetOS=TOS.win64))
                AND
                //Only native compiler for these OS
                (CrossInstaller.TargetOS in [TOS.win32,TOS.win64,TOS.linux,TOS.darwin,TOS.freebsd,TOS.openbsd,TOS.aix,TOS.haiku,TOS.solaris,TOS.dragonfly,TOS.netbsd])
                )
                then
              begin
                Infoln(infotext+'Building native compiler for '+CrossInstaller.TargetCPUName+'-'+CrossInstaller.TargetOSName+'.',etInfo);
                Processor.Process.Parameters.Add('FPC='+FCompiler);
                //s1:=CrossCompilerName;
                //Processor.Process.Parameters.Add('FPC='+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1);
                Processor.Process.Parameters.Add('-C');
                Processor.Process.Parameters.Add('compiler');
                Processor.Process.Parameters.Add('compiler');
              end else continue;
              {$else}
              continue;
              {$endif}
            end;
          end;

          Processor.Process.Parameters.Add('CROSSINSTALL=1');

          if (MakeCycle in [st_RtlInstall,st_PackagesInstall]) then
          begin
            if (SubarchTarget) then
            begin
              UnitSearchPath:=GetUnitsInstallDirectory(false);
              if (MakeCycle=st_RtlInstall) then
                Processor.Process.Parameters.Add('INSTALL_UNITDIR='+UnitSearchPath+DirectorySeparator+'rtl');
              if (MakeCycle=st_PackagesInstall) then
                Processor.Process.Parameters.Add('INSTALL_UNITDIR='+UnitSearchPath+DirectorySeparator+'packages');
            end;
          end;

          if (MakeCycle in [st_Compiler,st_CompilerInstall]) then
          begin
            // Do we need a new cross-compiler ?
            if (NOT CompilerUpdateNeeded) then
            begin
              if (MakeCycle=st_Compiler) then Infoln({infotext+}'Skipping cross-compiler build step: compiler seems to be up to date !!',etWarning);
              continue; // best guess: compilers stem from identical sources, so do not build the cross-compiler again
            end;
          end;

          if (MakeCycle in [st_Packages,st_PackagesInstall,st_NativeCompiler]) then
          begin
            if (NOT PackagesNeeded) then continue;
          end;

          {$endif crosssimple}

          Processor.Process.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
          Processor.Process.Parameters.Add('OS_SOURCE='+GetTargetOS);
          Processor.Process.Parameters.Add('OS_TARGET='+CrossInstaller.TargetOSName); //cross compile for different OS...
          Processor.Process.Parameters.Add('CPU_TARGET='+CrossInstaller.TargetCPUName); // and processor.
          if (CrossInstaller.SubArch<>TSubarch.saNone) then Processor.Process.Parameters.Add('SUBARCH='+CrossInstaller.SubArchName);

          //Processor.Process.Parameters.Add('OSTYPE='+CrossInstaller.TargetOS);
          Processor.Process.Parameters.Add('NOGDBMI=1'); // prevent building of IDE to be 100% sure

          // Error checking for some known problems with cross compilers
          //todo: this really should go to the cross compiler unit itself but would require a rewrite
          if (CrossInstaller.TargetCPU=TCPU.i8086) and
            (CrossInstaller.TargetOS=TOS.msdos) then
          begin
            if (pos('-g',Options)>0) then
            begin
              Infoln(infotext+'Specified debugging FPC options: '+Options+'... However, this cross compiler does not support debug symbols. Aborting.',etError);
              exit(false);
            end;
          end;

          if (CrossInstaller.TargetCPU=TCPU.wasm32) then
          begin
            // wasm only works with -O-
            i:=pos('-O',Options);
            if (i>0) then
            begin
              s2:=Copy(Options,i,3);
              if s2[3]<>'-' then
              begin
                Infoln(infotext+'Specified optimization: '+s2+'. Must be -O- for this target. Replacing.',etInfo);
                Options[i+2]:='-';
              end;
            end
            else
            begin
              Options:=Options+' -O-';
            end;
            // wasm: remove debugging settings
            i:=pos('-g',Options);
            while (i<>0) do
            begin
              s2:=Trim(Copy(Options,i,3));
              Infoln(infotext+'Specified debug option: '+s2+'. Removing for this target.',etInfo);
              Delete(Options,i,3);
              i:=pos('-g',Options);
            end;
          end;

          if ((SourceVersionNum<>0) AND (SourceVersionNum>=CalculateFullVersion(2,6,0))) then
          begin
            s2:=GetRevision(ModuleName);
            s2:=AnsiDequotedStr(s2,'''');
            if ( (Length(s2)>1) AND (s2<>'failure') AND (Pos(' ',s2)=0) ) then
            begin
              Processor.Process.Parameters.Add('REVSTR='+s2);
              Processor.Process.Parameters.Add('REVINC=force');
            end;
          end;

          {$ifdef solaris}
          {$IF defined(CPUX64) OR defined(CPUX86)}
          //Still not sure if this is needed
          //To be checked
          //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
          if (MakeCycle in [st_Compiler,st_CompilerInstall]) then
            Options:=Options+' -Xn';
          {$endif}
          {$endif}

          {$ifdef linux}
          if FMUSL then
          begin
            //if FileExists(IncludeTrailingPathDelimiter(CrossInstaller.LibsPath)+FMUSLLinker) then Options:=Options+' -FL'+FMUSLLinker;
          end;
          {$endif}

          CrossOptions:='';

          for i:=0 to CrossInstaller.CrossOpt.Count-1 do
            CrossOptions:=CrossOptions+Trim(CrossInstaller.CrossOpt[i])+' ';
          CrossOptions:=TrimRight(CrossOptions);

          if UseLibc then CrossOptions:=CrossOptions+' -dFPC_USE_LIBC';

          if ((CrossInstaller.TargetCPU=TCPU.mipsel) AND (CrossInstaller.TargetOS=TOS.embedded)) then
          begin
            // prevents the addition of .module nomips16 pseudo-op : not all assemblers can handle this
            CrossOptions:=CrossOptions+' -a5';
          end;

          if CrossInstaller.BinUtilsPrefix<>'' then
          begin
            // Earlier, we used regular OPT; using CROSSOPT is apparently more precise
            CrossOptions:=CrossOptions+' -XP'+CrossInstaller.BinUtilsPrefix;
            Processor.Process.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
          end;

          if CrossInstaller.LibsPath<>''then
          begin
            {$ifndef Darwin}
            CrossOptions:=CrossOptions+' -Xd';
            CrossOptions:=CrossOptions+' -Fl'+ExcludeTrailingPathDelimiter(CrossInstaller.LibsPath);
            {$endif}
          end;

          {$ifdef Darwin}
          Options:=Options+' -ap';
          {$endif}

          CrossOptions:=Trim(CrossOptions);

          if (
            (CrossOptions<>'')
            {$ifndef crosssimple}
            //Do not add cross-options when building native compiler
            //Correct options will be taken from fpc.cfg
            AND
            (MakeCycle<>st_NativeCompiler)
            {$endif}
            ) then
          begin
            Processor.Process.Parameters.Add('CROSSOPT='+CrossOptions);
          end;

          {$if (NOT defined(FPC_HAS_TYPE_EXTENDED)) AND (defined (CPUX86_64))}
          // soft 80 bit float if available
          if FSoftFloat then
          begin
            if ( (CrossInstaller.TargetCPU=TCPU.i386) OR (CrossInstaller.TargetCPU=TCPU.i8086)  OR (CrossInstaller.TargetCPU=TCPU.x86_64) ) then
            begin
              Infoln(infotext+'Adding -dFPC_SOFT_FPUX80 compiler option to enable 80bit (soft)float support (trunk only).',etInfo);
              Infoln(infotext+'This is needed due to the fact that FPC itself is also build with this option enabled.',etInfo);
              Options:=Options+' -dFPC_SOFT_FPUX80';
            end;
          end;
          {$endif}

          while Pos('  ',Options)>0 do
          begin
            Options:=StringReplace(Options,'  ',' ',[rfReplaceAll]);
          end;
          Options:=Trim(Options);

          s1:=STANDARDCOMPILERVERBOSITYOPTIONS+' '+Options;

          {$ifdef DEBUG}
          //s:=s+' -g -gl -dEXTDEBUG'; //-va+
          //s:=s+' -dEXTDEBUG'; //-va+
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
                s1:='-XR'+s2+' '+s1;
                s1:='-Fl'+s2+'/usr/lib '+s1;
              end;
            end;
          end;
          {$ENDIF}

          Processor.Process.Parameters.Add('OPT='+s1);

          try
            if CrossOptions='' then
               Infoln(infotext+'Running '+Processor.Executable+' [step # '+GetEnumNameSimple(TypeInfo(TSTEPS),Ord(MakeCycle))+'] (FPC crosscompiler: '+CrossInstaller.RegisterName+')',etInfo)
            else
              Infoln(infotext+'Running '+Processor.Executable+' [step # '+GetEnumNameSimple(TypeInfo(TSTEPS),Ord(MakeCycle))+'] (FPC crosscompiler: '+CrossInstaller.RegisterName+') with CROSSOPT: '+CrossOptions,etInfo);

            ProcessorResult:=Processor.ExecuteAndWait;
            result:=(ProcessorResult=0);

            if ProcessorResult=AbortedExitCode then break;

            {$ifndef crosssimple}
            if ((NOT result) AND (MakeCycle=st_Packages)) then
            begin
              //Sometimes rerun gives good results (on AIX 32bit especially).
              //Infoln(infotext+'Running '+Processor.Executable+' stage again ... could work !',etInfo);
              //ProcessorResult:=Processor.ExecuteAndWait;
              //result:=(ProcessorResult=0);
            end;

            {$IFDEF UNIX}
            if (result) AND (MakeCycle=st_CompilerInstall) then
            begin
              // Get the correct name of the cross-compiler in source-directory
              s1:='ppcross'+ppcSuffix[CrossInstaller.TargetCPU];
              s2:=ConcatPaths([FSourceDirectory,'compiler',s1]);
              if FileExists(s2) then
              begin
                {$ifdef Darwin}
                if CrossCompilerName=GetCompilerName(GetTargetCPU) then
                begin
                  Infoln(infotext+'Cross-compiler and native compiler share the same name: '+CrossCompilerName+'.',etInfo);
                  Infoln(infotext+'Skipping manual compiler-copy to bin-directory ! To be investigated.',etInfo);
                  // Perhaps we need to contruct a "fat" binary
                  // lipo -create s1 CrossCompilerName -output s1
                  // TODO
                  // I do not know what to do at the moment
                end
                else
                {$endif Darwin}
                begin
                  // copy over / rename the cross-compiler towards the FPC bin-directory, with the right compilername.
                  Infoln(infotext+'Copy cross-compiler ('+s1+') into: '+ExcludeTrailingPathDelimiter(FFPCCompilerBinPath),etInfo);
                  s1:=FFPCCompilerBinPath+CrossCompilerName;
                  //FileUtil.CopyFile(s2,s1);
                  SysUtils.DeleteFile(s1);
                  SysUtils.RenameFile(s2,s1);
                  fpChmod(s1,&755);
                end;
              end;
            end;
            {$ENDIF UNIX}
            {$endif crosssimple}

          except
            on E: Exception do
            begin
              WritelnLog(infotext+'Running cross compiler fpc '+Processor.Executable+' generated an exception!'+LineEnding+'Details: '+E.Message,true);
              result:=false;
            end;
          end;

          if (not result) then break;

        end;// loop over MakeCycle

        if result then Infoln(infotext+'Building native compiler for '+GetFPCTarget(false)+' finished.',etInfo);

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
            Compiler := '////\\\Error trying to compile FPC\|!';
            Infoln(infotext+'Running cross compiler fpc '+Processor.Executable+' for '+GetFPCTarget(false)+' failed with an error code.',etError);
            // If we were building a crosscompiler itself when the failure occured, remove all fpc.cfg settings of this target
            if MakeCycle in [st_Compiler,st_CompilerInstall] then
            begin
              Infoln(infotext+'Removing all '+GetFPCTarget(false)+' compiler settings from fpc.cfg.',etError);
              InsertFPCCFGSnippet(FPCCfg,'');
            end;
          end;

        end
        else
        begin

          // Get the correct name of the cross-compiler in source-directory
          s1:='ppcross'+ppcSuffix[CrossInstaller.TargetCPU];
          s2:=ConcatPaths([FSourceDirectory,'compiler',s1]);
          {$ifdef crosssimple}
          {$IFDEF UNIX}
          if FileExists(s2) then
          begin
            {$ifdef Darwin}
            if CrossCompilerName=GetCompilerName(GetTargetCPU) then
            begin
              Infoln(infotext+'Cross-compiler and native compiler share the same name: '+CrossCompilerName+'.',etInfo);
              Infoln(infotext+'Skipping manual compiler-copy to bin-directory ! To be investigated.',etInfo);
              // Perhaps we need to contruct a "fat" binary
              // lipo -create s1 CrossCompilerName -output s1
              // TODO
              // I do not know what to do at the moment
            end
            else
            {$endif Darwin}
            begin
              // copy over / rename the cross-compiler towards the FPC bin-directory, with the right compilername.
              Infoln(infotext+'Copy cross-compiler ('+s1+') into: '+FBinPath,etInfo);
              s1:=ConcatPaths([FBinPath,CrossCompilerName]);
              //FileUtil.CopyFile(s2,s1);
              SysUtils.DeleteFile(s1);
              SysUtils.RenameFile(s2,s1);
              fpChmod(s1,&755);
            end;
          end;
          {$ENDIF}
          {$endif crosssimple}

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
                if NOT FileExists(IncludeTrailingPathDelimiter(FMakeDir)+'gdb\arm-wince\gdb.exe') then
                begin
                  s1:=GetTempFileNameExt('FPCUPTMP','zip');
                  if GetFile(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,s1) then
                  begin
                    with TNormalUnzipper.Create do
                    begin
                      try
                        if DoUnZip(s1,IncludeTrailingPathDelimiter(FMakeDir)+'gdb\arm-wince\',[]) then
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
                    FileUtil.CopyFile(s1,s2+'gdb'+GetExeExt);
                    {$endif}
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        SetPath(OldPath,false,false);
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

  if (NOT DirectoryExists(FInstallDirectory)) then exit;
  if CheckDirectory(FInstallDirectory) then exit;


  if assigned(CrossInstaller) AND (Length(FBaseDirectory)>0) AND (NOT CheckDirectory(FBaseDirectory)) then
  begin
    if ((CrossInstaller.TargetCPU=TCPU.cpuNone) OR (CrossInstaller.TargetOS=TOS.osNone)) then exit;

    CrossInstaller.Reset;
    CrossInstaller.SetFPCVersion(SourceVersionStr);

    DirectoryAvailable:=CrossInstaller.GetBinUtils(FBaseDirectory);
    if DirectoryAvailable then
    begin
      aDir:=CrossInstaller.BinUtilsPath;
      if DirectoryExists(aDir) then
      begin
        if FileExists(IncludeTrailingPathDelimiter(aDir)+FPCUP_ACKNOWLEDGE) then
        begin
          // Only allow cross directories inside our own install te be deleted
          if (Pos(FBaseDirectory,aDir)=1) AND  (Pos(CROSSBINPATH,aDir)>0) then
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

    DirectoryAvailable:=CrossInstaller.GetLibs(FBaseDirectory);
    if DirectoryAvailable then
    begin
      aDir:=CrossInstaller.LibsPath;
      if DirectoryExists(aDir) then
      begin
        if FileExists(IncludeTrailingPathDelimiter(aDir)+FPCUP_ACKNOWLEDGE) then
        begin
          // Only allow cross directories inside our own install te be deleted
          if (Pos(FBaseDirectory,aDir)=1) AND  (Pos(CROSSLIBPATH,aDir)>0) then
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

    FPCCfg := FFPCCompilerBinPath + FPCCONFIGFILENAME;
    InsertFPCCFGSnippet(FPCCfg,'');

    aDir:=IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+DirectorySeparator+GetFPCTarget(false);
    if DirectoryExists(aDir) then
    begin
      // Only allow binary directories inside our own install te be deleted
      if (Pos(FBaseDirectory,aDir)=1) then
      begin
        Infoln(infotext+'Deleting '+ModuleName+' binary directory '+aDir);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' binary directory '+aDir);
        end;
      end;
    end;

    aDir:=GetUnitsInstallDirectory(false);
    if DirectoryExists(aDir) then
    begin
      // Only allow unit directories inside our own install te be deleted
      if (Pos(FBaseDirectory,aDir)=1) then
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
    //  SysUtils.DeleteFile(ConcatPaths([FInstallDirectory,'units',CrossInstaller.RegisterName,'system.ppu']));

  end;
end;

{ TFPCNativeInstaller }
function TFPCNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
const
  YYLEX='yylex.cod';
  YYPARSE='yyparse.cod';
var
  OperationSucceeded:boolean;
  {$IFDEF MSWINDOWS}
  FileCounter:integer;
  {$ENDIF}
  s1,s2:string;
  {$IFDEF UNIX}
  s3:string;
  {$ENDIF}
  //FPCDirStore:string;
begin
  result:=inherited;
  OperationSucceeded:=true;

  s1:=CompilerVersion(FCompiler);
  if (s1<>'0.0.0')
    then Infoln('FPC native builder: Using FPC bootstrap compiler with version: '+s1, etInfo)
    else Infoln(infotext+'FPC bootstrap version error: '+s1+' ! Should never happen: expect many errors !!', etError);

  //if clean failed (due to missing compiler), try again !
  if (NOT FCleanModuleSuccess) then
  begin
    if ((ModuleName=_FPC) OR (ModuleName=_PAS2JS)) then
    begin
      Infoln(infotext+'Running CleanModule once more before building FPC from sources, due to previous CleanModule failure.',etInfo);
      CleanModule(ModuleName);
    end;
  end;

  if (ModuleName=_FPC) then
  //if (false) then
  begin
    //Sometimes, during build, we get an error about missing yylex.cod and yyparse.cod.
    //Copy them now, just to be sure

    ForceDirectoriesSafe(FFPCCompilerBinPath);
    s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'tply';
    s1:=FFPCCompilerBinPath+YYLEX;
    if (NOT FileExists(s1)) then FileUtil.CopyFile(s2+DirectorySeparator+YYLEX,s1);
    s1:=FFPCCompilerBinPath+YYPARSE;
    if (NOT FileExists(s1)) then FileUtil.CopyFile(s2+DirectorySeparator+YYPARSE,s1);

    {$IFDEF UNIX}
    s1:=ConcatPaths([FInstallDirectory,'lib','fpc',SourceVersionStr]);
    ForceDirectoriesSafe(s1);
    s1:=s1+'/lexyacc';
    DeleteFile(s1);
    s2:=IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc/lexyacc';
    ForceDirectoriesSafe(s2);
    fpSymlink(pchar(s2),pchar(s1));

    s1:=IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'tply';
    s3:=s2+DirectorySeparator+YYLEX;
    if (NOT FileExists(s3)) then FileUtil.CopyFile(s1+DirectorySeparator+YYLEX,s3);
    s3:=s2+DirectorySeparator+YYPARSE;
    if (NOT FileExists(s3)) then FileUtil.CopyFile(s1+DirectorySeparator+YYPARSE,s3);
    {$ENDIF UNIX}
  end;

  if (ModuleName=_FPC) then
  begin
    if (Length(ActualRevision)=0) OR (ActualRevision='failure') then
    begin
      s1:=GetRevision(ModuleName);
      if Length(s1)>0 then FActualRevision:=s1;
    end;
    Infoln(infotext+'Now building '+ModuleName+' revision '+ActualRevision,etInfo);
  end;

  Processor.Executable := Make;
  Processor.Process.Parameters.Clear;
  {$IFDEF MSWINDOWS}
  if Length(Shell)>0 then Processor.Process.Parameters.Add('SHELL='+Shell);
  {$ENDIF}
  FErrorLog.Clear;

  if (NOT FNoJobs) then
  begin
    Processor.Process.Parameters.Add('--jobs='+IntToStr(FCPUCount));
    Processor.Process.Parameters.Add('FPMAKEOPT=--threads='+IntToStr(FCPUCount));
  end;

  //Processor.Process.Parameters.Add('FPC='+FCompiler);
  Processor.Process.Parameters.Add('PP='+FCompiler);

  {$IFDEF DEBUG}
  //To debug Makefile itself
  //Processor.Process.Parameters.Add('-d');
  {$ENDIF}
  Processor.Process.Parameters.Add('FPCMAKE=' + FFPCCompilerBinPath+'fpcmake'+GetExeExt);
  Processor.Process.Parameters.Add('PPUMOVE=' + FFPCCompilerBinPath+'ppumove'+GetExeExt);
  Processor.Process.Parameters.Add('PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
  Processor.Process.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));

  //Sometimes, during build, we get an error about missing yylex.cod and yyparse.cod.
  //The paths are fixed in the FPC sources. Try to set the default path here [FPCDIR], so yylex.cod and yyparse.cod can be found.
  (*
  {$IFDEF UNIX}
  s1:=ConcatPaths([FInstallDirectory,'lib','fpc',GetFPCVersion]);
  {$ELSE}
  s1:=ExcludeTrailingPathDelimiter(FSourceDirectory);
  {$ENDIF UNIX}
  *)
  s1:=ExcludeTrailingPathDelimiter(FSourceDirectory);
  Processor.Process.Parameters.Add('FPCDIR=' + s1);

  //Makefile could pickup FPCDIR setting, so try to set it for fpcupdeluxe
  //FPCDirStore:=Processor.Environment.GetVar('FPCDIR');
  //Processor.Environment.SetVar('FPCDIR',IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc');

  //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
  //Todo: to be investigated
  //Processor.Process.Parameters.Add('FPCFPMAKE='+ChosenCompiler);

  {$IFDEF UNIX}
  Processor.Process.Parameters.Add('INSTALL_BINDIR='+ExcludeTrailingPathDelimiter(FFPCCompilerBinPath));
  {$ELSE}

  if (ModuleName<>_FPC) then
  begin
    s1:=FFPCCompilerBinPath+FPCCONFIGFILENAME;
    if FileExists(s1) then
    begin
      //Processor.Process.Parameters.Add('CFGFILE=' + s1);
    end;
  end;

  Processor.Process.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  //Processor.Process.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  {$ENDIF}
  Processor.Process.Parameters.Add('OS_SOURCE=' + GetTargetOS);
  Processor.Process.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);
  Processor.Process.Parameters.Add('OS_TARGET=' + GetTargetOS);
  Processor.Process.Parameters.Add('CPU_TARGET=' + GetTargetCPU);

  if (SourceVersionNum<CalculateFullVersion(2,4,4)) then
    Processor.Process.Parameters.Add('DATA2INC=echo');
  {else
    Processor.Process.Parameters.Add('DATA2INC=' + FFPCCompilerBinPath+'data2inc'+GetExeExt);}

  if FBootstrapCompilerOverrideVersionCheck then
    Processor.Process.Parameters.Add('OVERRIDEVERSIONCHECK=1');
  s1:=STANDARDCOMPILERVERBOSITYOPTIONS+' '+FCompilerOptions;
  while Pos('  ',s1)>0 do
  begin
    s1:=StringReplace(s1,'  ',' ',[rfReplaceAll]);
  end;
  s1:=Trim(s1);

  {$IFDEF UNIX}
  s1:='-Sg '+s1;
    {$IFDEF SOLARIS}
    {$IF defined(CPUX64) OR defined(CPUX86)}
    //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
    s1:='-Xn '+s1;
    {$endif}
    {$ENDIF}
  if FMUSL then s1:='-FL'+FMUSLLinker+' '+s1;
  {$ENDIF}

  {$IFDEF DARWIN}
  //Add minimum required OSX version to prevent "crti not found" errors.
  s2:=GetDarwinSDKVersion('macosx');
  if CompareVersionStrings(s2,'10.8')>=0 then
  begin
    s2:='10.8';
  end;
  if Length(s2)>0 then
  begin
    s1:='-WM'+s2+' '+s1;
    {
    if CompareVersionStrings(s2,'10.14')>=0 then
    begin
      s1:='-Fl/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib '+s1;
    end;
    }
  end;

  s2:=GetDarwinSDKLocation;
  if Length(s2)>0 then
  begin
    s1:='-XR'+s2+' '+s1;
    s1:='-Fl'+s2+'/usr/lib '+s1;
  end;
  {$ENDIF}

  if ((SourceVersionNum<>0) AND (SourceVersionNum>=CalculateFullVersion(2,6,0))) then
  begin
    s2:=Trim(ActualRevision);
    s2:=AnsiDequotedStr(s2,'''');
    if ( (Length(s2)>1) AND (s2<>'failure') AND (Pos(' ',s2)=0) ) then
    begin
      Processor.Process.Parameters.Add('REVSTR='+s2);
      Processor.Process.Parameters.Add('REVINC=force');
    end;
  end;

  {$if (NOT defined(FPC_HAS_TYPE_EXTENDED)) AND (defined (CPUX86_64))}
  if FSoftFloat then
  begin
    // soft 80 bit float if available
    Infoln(infotext+'Adding -dFPC_SOFT_FPUX80 compiler option to enable 80bit (soft)float support (trunk only).',etInfo);
    s1:=s1+' -dFPC_SOFT_FPUX80';
  end;
  {$endif}

  {$ifdef BSD}
    {$ifndef DARWIN}
      s1:=s1+' -Fl/usr/pkg/lib';
    {$endif}
  {$endif}

  if UseLibc then s1:=s1+' -dFPC_USE_LIBC';

  {$ifdef Haiku}
    s2:='';
    {$ifdef CPUX86}
    s2:='/x86';
    {$endif}
    s1:=s1+' -XR/boot/system/lib'+s2+' -FD/boot/system/bin'+s2+'/ -Fl/boot/system/develop/lib'+s2;
  {$endif}

  s1:=Trim(s1);
  Processor.Process.Parameters.Add('OPT='+s1);

  Processor.Process.CurrentDirectory:='';
  case ModuleName of
    _FPC,_MAKEFILECHECKFPC:
    begin
      Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
    end;
    _PAS2JS:
    begin
      Processor.Process.CurrentDirectory:=IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'pas2js';
      // first run fpcmake to generate correct makefile
      // is this still needed !!?? No !!
      //SysUtils.DeleteFile(IncludeTrailingPathDelimiter(Processor.Process.CurrentDirectory)+'fpmake'+GetExeExt);
      //ExecuteCommandInDir(FFPCCompilerBinPath+'fpcmake'+GetExeExt,Processor.Process.CurrentDirectory,FVerbose);
    end;
  end;

  if (Length(Processor.Process.CurrentDirectory)=0) OR (NOT DirectoryExists(Processor.Process.CurrentDirectory)) then
  begin
    Processor.Process.Parameters.Add('--help'); // this should render make harmless
    WritelnLog(etError, infotext+'Invalid module name [' + ModuleName + '] specified! Please fix the code.', true);
    OperationSucceeded := false;
    Result := false;
    exit;
  end;

  Processor.Process.Parameters.Add('--directory='+Processor.Process.CurrentDirectory);

  if ModuleName=_MAKEFILECHECKFPC then
  begin
    Processor.Process.Parameters.Add('fpc_baseinfo');
  end
  else
  begin
    Processor.Process.Parameters.Add('all');
    //If we have separate source and install, always use the install command
    //if (FInstallDirectory<>FSourceDirectory) then
    Processor.Process.Parameters.Add('install');
  end;

  try
    ProcessorResult:=Processor.ExecuteAndWait;
    //Restore FPCDIR environment variable ... could be trivial, but batter safe than sorry
    //Processor.Environment.SetVar('FPCDIR',FPCDirStore);
    if ProcessorResult <> 0 then
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

  if ModuleName=_FPC then
  begin
    {$IFDEF UNIX}
    if OperationSucceeded then
    begin
      if FVerbose then
        Infoln(infotext+'Creating fpc script:',etInfo)
      else
        Infoln(infotext+'Creating fpc script:',etDebug);
      OperationSucceeded:=CreateFPCScript;
    end;
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

    {$IFDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      //Copy over binutils to new CompilerName bin directory
      try
        for FileCounter:=low(FUtilFiles) to high(FUtilFiles) do
        begin
          if FUtilFiles[FileCounter].Category=ucBinutil then
            FileUtil.CopyFile(IncludeTrailingPathDelimiter(FMakeDir)+FUtilFiles[FileCounter].FileName,
              FFPCCompilerBinPath+FUtilFiles[FileCounter].FileName);
        end;
        // Also, we can change the make/binutils path to our new environment
        // Will modify fmake as well.
        FMakeDir:=ExcludeTrailingPathDelimiter(FFPCCompilerBinPath);
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
  infotext:=Copy(Self.ClassName,2,MaxInt)+' (BuildModuleCustom: '+ModuleName+'): ';
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

function TFPCInstaller.GetVersionFromSource(aSourcePath: string): string;
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

  if (NOT DirectoryExists(aSourcePath)) then exit;
  if DirectoryIsEmpty(aSourcePath) then exit;

  version_nr:='';
  release_nr:='';
  build_nr:='';
  //minorbuild_nr:='';

  found_version_nr:=false;
  found_release_nr:=false;
  found_build_nr:=false;
  //found_minorbuild_nr:=false;

  s:=IncludeTrailingPathDelimiter(aSourcePath) + 'compiler' + DirectorySeparator + 'version.pas';
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
    s:=IncludeTrailingPathDelimiter(aSourcePath) + FPCMAKEFILENAME;
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

function TFPCInstaller.GetReleaseCandidateFromSource(aSourcePath:string):integer;
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

  s:=IncludeTrailingPathDelimiter(aSourcePath) + 'compiler' + DirectorySeparator + 'version.pas';
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
  else if s='1.9.0' then result:='0.0.0';


  {$IFDEF CPUAARCH64}
  // we need at least 3.2.0 for aarch64
  if CalculateNumericalVersion(result)<CalculateNumericalVersion('3.2.0') then result:='3.2.0';
  {$IFDEF DARWIN}
  if CalculateNumericalVersion(result)<CalculateNumericalVersion(FPCTRUNKVERSION) then result:=FPCTRUNKVERSION;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HAIKU}
  {$IFDEF CPUX64}
  // we need at least 3.2.0 for Haiku x86_64
  if CalculateNumericalVersion(result)<CalculateNumericalVersion('3.2.0') then result:='3.2.0';
  //if CalculateNumericalVersion(result)<CalculateNumericalVersion(FPCTRUNKVERSION) then result:=FPCTRUNKVERSION;
  {$ENDIF}
  {$IFDEF CPUX32}
  // we need at least 3.0.0 for Haiku x32
  if CalculateNumericalVersion(result)<CalculateNumericalVersion('3.0.0') then result:='3.0.0';
  {$ENDIF}
  {$ENDIF}

  {$IF DEFINED(CPUPOWERPC64) AND DEFINED(FPC_ABI_ELFV2)}
  // we need at least 3.2.0 for ppc64le
  if CalculateNumericalVersion(result)<CalculateNumericalVersion('3.2.0') then result:='3.2.0';
  {$ENDIF}
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

    {$IFDEF CPUAARCH64}
    // we need at least 3.2.0 for aarch64
    if FinalVersion<CalculateNumericalVersion('3.2.0') then FinalVersion:=CalculateNumericalVersion('3.2.0');
    {$ENDIF}

    {$IF DEFINED(CPUPOWERPC64) AND DEFINED(FPC_ABI_ELFV2)}
    // we need at least 3.2.0 for ppc64le
    if FinalVersion<CalculateNumericalVersion('3.2.0') then FinalVersion:=CalculateNumericalVersion('3.2.0');
    {$ENDIF}

    //3.3.1 allows 3.0.4 but 3.0.4 does not work anymore for 3.3.1 ... so only allow 3.2.0 or higher
    if (SourceVersionNum=CalculateNumericalVersion('3.3.1')) then
    begin
      RequiredVersion:=CalculateNumericalVersion('3.2.0');
      if (FinalVersion<RequiredVersion) then FinalVersion:=RequiredVersion;
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
{$ENDIF UNIX}
begin
  result:=true;
  {$IFDEF UNIX}
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (CreateFPCScript): ';
  FPCCompiler := FFPCCompilerBinPath+'fpc'+GetExeExt;

  // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
  // If this fails, Lazarus compilation will fail...
  FPCScript := IncludeTrailingPathDelimiter(ExtractFilePath(FPCCompiler)) + 'fpc.sh';
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
      writeln(TxtFile,FPCCompiler,' -n @',
        IncludeTrailingPathDelimiter(ExtractFilePath(FPCCompiler)),FPCCONFIGFILENAME+' '+
        '"$@"');
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
  {$ENDIF UNIX}
end;

function TFPCInstaller.DownloadBootstrapCompiler: boolean;
var
  BootstrapFileArchiveDir: string;
  BootstrapFilePath,BootstrapFileExt: string;
  CompilerName:string;
  OperationSucceeded: boolean;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBootstrapCompiler): ';

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
          //FileUtil.CopyFile(BootstrapFilePath, BootstrapFileArchiveDir+CompilerName);
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
        Infoln(localinfotext+'Success. Going to copy '+BootstrapFilePath+' to '+FBootstrapCompiler,etInfo);
        SysUtils.DeleteFile(FBootstrapCompiler); //ignore errors

        // We might be moving files across partitions so we cannot use renamefile
        // However, this gives errors on Darwin due to the copied file not being signed.
        // So, use rename and fall-over to copy in case of error
        //OperationSucceeded:=FileUtil.CopyFile(BootstrapFilePath, FBootstrapCompiler);
        OperationSucceeded:=SysUtils.RenameFile(BootstrapFilePath,FBootstrapCompiler);
        if (NOT OperationSucceeded) then OperationSucceeded:=FileUtil.CopyFile(BootstrapFilePath, FBootstrapCompiler);

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

  testcompiler:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc1'+GetExeExt;

  if not FileExists(testcompiler) then
    testcompiler:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc'+GetExeExt;

  if not FileExists(testcompiler) then
    testcompiler:=GetFPCInBinDir;

  if FileExists(testcompiler) then
  begin
    result:=CompilerRevision(testcompiler);
  end;
end;

function TFPCInstaller.InitModule(aBootstrapVersion:string):boolean;
var
  aCompilerList:TStringList;
  i,j,k,l:integer;
  aCompilerArchive,aStandardCompilerArchive:string;
  aCompilerFound,aFPCUPCompilerFound, aLookForBetterAlternative:boolean;
  {$IFDEF FREEBSD}
  FreeBSDVersion:integer;
  {$ENDIF}
  s:string;
  {$ifdef Darwin}
  s1:string;
  {$endif}
  aLocalBootstrapVersion,aLocalFPCUPBootstrapVersion:string;
  aFPCUPBootstrapURL,aFPCUPBootstrapperName:string;
  aDownLoader: TBasicDownLoader;
begin
  result := true;

  if (InitDone) AND (aBootstrapVersion='') then exit;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (InitModule): ';

  result:=CheckAndGetTools;

  WritelnLog(localinfotext+'Init:', false);
  WritelnLog(localinfotext+'FPC directory:      ' + FSourceDirectory, false);
  WritelnLog(localinfotext+'FPC URL:            ' + URL, false);
  WritelnLog(localinfotext+'FPC options:        ' + FCompilerOptions, false);

  // set standard bootstrap compilername
  FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetTargetCPUOS+'-'+GetCompilerName(GetTargetCPU);
  if NOT FileExists(FBootstrapCompiler) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);

  {$IFDEF Darwin}
    {$IFDEF CPU32}
      if NOT FileExists(FBootstrapCompiler) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
    {$ENDIF CPU32}
  {$ENDIF Darwin}

  {$ifdef Linux}
  if FMUSL then NativeFPCBootstrapCompiler:=false;
  {$endif}

  if (aBootstrapVersion<>'') then
  begin
    FBootstrapCompilerOverrideVersionCheck:=false;

    aStandardCompilerArchive:=GetTargetCPUOS+'-'+GetCompilerName(GetTargetCPU);
    // remove file extension
    aStandardCompilerArchive:=ChangeFileExt(aStandardCompilerArchive,'');
    {$IFDEF MSWINDOWS}
    aStandardCompilerArchive:=aStandardCompilerArchive+'.zip';
    {$ELSE}
    {$IFDEF Darwin}
    aStandardCompilerArchive:=aStandardCompilerArchive+'.tar.bz2';
    {$ELSE}
    aStandardCompilerArchive:=aStandardCompilerArchive+'.bz2';
    {$ENDIF}
    {$ENDIF}

    aLocalBootstrapVersion:=aBootstrapVersion;
    aCompilerFound:=false;
    aFPCUPCompilerFound:=false;
    aLookForBetterAlternative:=false;

    if FUseWget
       then aDownLoader:=TWGetDownLoader.Create
       else aDownLoader:=TNativeDownLoader.Create;

    try
      if NativeFPCBootstrapCompiler then
      begin
        // first, try official FPC binaries
        aCompilerList:=TStringList.Create;
        try
          while ((NOT aCompilerFound) AND (CalculateNumericalVersion(aLocalBootstrapVersion)>(FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION))) do
          begin
            Infoln(localinfotext+'Looking for official FPC bootstrapper with version '+aLocalBootstrapVersion,etInfo);

            // set initial standard achive name
            aCompilerArchive:=aStandardCompilerArchive;

            // handle specialities for achive name
            {$IFDEF Darwin}
            // URL: ftp://ftp.freepascal.org/pub/fpc/dist/2.2.2/source/
            if aLocalBootstrapVersion='2.2.2' then aCompilerArchive:='fpc-2.2.2.universal-darwin.bootstrap.tar.bz2'; //ppcuniversal

            // URL: ftp://ftp.freepascal.org/pub/fpc/dist/2.2.4/source/
            if aLocalBootstrapVersion='2.2.4' then aCompilerArchive:='fpc-2.2.4.universal-darwin.bootstrap.tar.bz2'; //ppcuniversal

            // URL: standard
            if aLocalBootstrapVersion='2.4.0' then aCompilerArchive:='fpc-2.4.0.universal-darwin.bootstrap.tar.bz2'; //ppcuniversal
            if aLocalBootstrapVersion='2.4.2' then aCompilerArchive:='universal-darwin-ppcuniversal.tar.bz2'; //ppcuniversal
            if aLocalBootstrapVersion='2.4.4' then aCompilerArchive:='universal-darwin-ppcuniversal.tar.bz2'; //ppcuniversal
            if aLocalBootstrapVersion='2.6.0' then aCompilerArchive:='universal-darwin-ppcuniversal.tar.bz2'; //ppcuniversal
            if aLocalBootstrapVersion='2.6.4' then aCompilerArchive:='universal-macosx-10.5-ppcuniversal.tar.bz2'; //ppcuniversal
            {$IF defined(CPUX86_64) OR defined(CPUPOWERPC64)}
            if aLocalBootstrapVersion='3.0.0' then aCompilerArchive:='x86_64-macosx-10.7-ppcx64.tar.bz2'; // ppcx64
            if aLocalBootstrapVersion='3.0.4' then aCompilerArchive:='x86_64-macosx-10.9-ppcx64.tar.bz2'; // ppcx64
            {$ENDIF}
            {$ENDIF}
            {$IFDEF win32}
            if aLocalBootstrapVersion='3.0.2' then aCompilerArchive:='ppc386-i386-win32.zip';
            {$endif}

            s:=FPCFTPURL+'dist/'+aLocalBootstrapVersion+'/bootstrap/';

            Infoln(localinfotext+'Looking for (online) bootstrapper '+aCompilerArchive + ' in ' + s,etDebug);

            aCompilerList.Clear;

            result:=aDownLoader.getFTPFileList(s,aCompilerList);
            if (NOT result) then
            begin
              Infoln(localinfotext+'Could not get compiler list from ' + s + '. Trying again. Final try.',etInfo);
              sleep(500);
              aCompilerList.Clear;
              result:=aDownLoader.getFTPFileList(s,aCompilerList);
            end;

            if result then
            begin

              if FVerbose then
              begin
                if aCompilerList.Count>0 then Infoln(localinfotext+'Found FPC v'+aLocalBootstrapVersion+' online bootstrappers: '+aCompilerList.CommaText,etDebug);
              end;

              {$IFDEF FREEBSD}
              // FreeBSD : special because of versions
              FreeBSDVersion:=-1;
              s:=GetTargetCPUOS;
              for i:=0 to Pred(aCompilerList.Count) do
              begin
                Infoln(localinfotext+'Found online '+aLocalBootstrapVersion+' bootstrap compiler: '+aCompilerList[i],etDebug);
                j:=Pos(s,aCompilerList[i]);
                if j>0 then
                begin
                  aCompilerFound:=True;
                  k:=j+Length(s);
                  l:=0;
                  while (Length(aCompilerList[i])>=k) AND (aCompilerList[i][k] in ['0'..'9']) do
                  begin
                    l:=l*10+Ord(aCompilerList[i][k])-$30;
                    Inc(k);
                  end;
                  if l>FreeBSDVersion then
                  begin
                    // get the highest version available ... this will not always be ok, but fpcupdeluxe = bleeding edge ... ;-)
                    aCompilerArchive:=aCompilerList[i];
                    FreeBSDVersion:=l;
                  end;
                end;
              end;
              if (aCompilerFound) then
              begin
                Infoln(localinfotext+'Got a correct bootstrap compiler from official FPC bootstrap sources',etDebug);
                break;
              end;
              {$ELSE}
              for i:=0 to Pred(aCompilerList.Count) do
              begin
                Infoln(localinfotext+'Found online '+aLocalBootstrapVersion+' bootstrap compiler: '+aCompilerList[i],etDebug);
                aCompilerFound:=(aCompilerList[i]=aCompilerArchive);
                if aCompilerFound then
                begin
                  Infoln(localinfotext+'Found a correct bootstrap compiler from official FPC bootstrap binaries.',etDebug);
                  break;
                end;
              end;
              {$ENDIF}

            end;

            // look for a previous compiler if not found, and use overrideversioncheck
            if (NOT aCompilerFound) then
            begin
              FBootstrapCompilerOverrideVersionCheck:=true;
              s:=GetBootstrapCompilerVersionFromVersion(aLocalBootstrapVersion);
              if aLocalBootstrapVersion<>s
                 then aLocalBootstrapVersion:=s
                 else break;
            end;

          end; // while

        finally
          aCompilerList.Free;
        end;

        // found an official FPC bootstrapper !
        if (aCompilerFound) then
        begin
          if FBootstrapCompilerURL='' then
          begin
            Infoln(localinfotext+'Got a V'+aLocalBootstrapVersion+' bootstrap compiler from official FPC bootstrap sources.',etInfo);
            FBootstrapCompilerURL := FPCFTPURL+'dist/'+aLocalBootstrapVersion+'/bootstrap/'+aCompilerArchive;
          end;
        end;

      end;

      aLookForBetterAlternative:=false;
      {$ifdef Darwin}
      {$ifdef CPUX64}
      // Catalina does not like the multi-arch bootstrappers from FPC itself.
      // Try to get a better one from fpcupdeluxe bootstrapper repository itself.
      if (aCompilerFound) then aLookForBetterAlternative:=true;
      {$endif}
      {$endif}

      // second, try the FPCUP binaries from release, perhaps it is a better version
      if (NOT aCompilerFound) OR (FBootstrapCompilerOverrideVersionCheck) OR (aLookForBetterAlternative) then
      begin

        if (NOT FBootstrapCompilerOverrideVersionCheck) then
        begin
          if NativeFPCBootstrapCompiler then
          begin
            Infoln(localinfotext+'Slight panic: No official FPC bootstrapper found.',etError);
            Infoln(localinfotext+'Now looking for last resort bootstrap compiler from Github FPCUP(deluxe) releases.',etError);
          end;
        end
        else
        begin
          Infoln(localinfotext+'Now looking for a better [version] bootstrap compiler from Github FPCUP(deluxe) releases.',etInfo);
        end;

        aFPCUPBootstrapURL:='';
        aLocalFPCUPBootstrapVersion:=aBootstrapVersion;
        aFPCUPCompilerFound:=false;

        aCompilerList:=TStringList.Create;
        try
          aCompilerList.Clear;
          try
            result:=GetGitHubFileList(FPCUPGITREPOBOOTSTRAPPERAPI,aCompilerList,FUseWget,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
          except
            on E : Exception do
            begin
              result:=false;
              Infoln(localinfotext+E.ClassName+' error raised, with message : '+E.Message, etError);
            end;
          end;

          {$ifdef DEBUG}
          if ((result) AND (aCompilerList.Count>0)) then
          begin
            for i:=0 to Pred(aCompilerList.Count) do
              Infoln(localinfotext+'Found online bootstrap compiler: '+aCompilerList[i],etDebug);
          end;
          {$endif}

          while ((NOT aFPCUPCompilerFound) AND (CalculateNumericalVersion(aLocalFPCUPBootstrapVersion)>0)) do
          begin

            // Construct FPCUP bootstrapper name
            aFPCUPBootstrapperName:='fpcup-';
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+StringReplace(aLocalFPCUPBootstrapVersion,'.','_',[rfReplaceAll]);
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'-';
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+GetTargetCPU;
            {$ifdef CPUARMHF}
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'hf';
            {$endif CPUARMHF}
            {$IF DEFINED(CPUPOWERPC64) AND DEFINED(LINUX) AND DEFINED(FPC_ABI_ELFV2)}
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'le';
            {$ENDIF}
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'-';
            {$ifdef LINUX}
            if FMUSL then aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'musl';
            {$endif LINUX}
            {$ifdef Solaris}
            //perhaps needed for special Solaris OpenIndiana bootstrapper
            //if FSolarisOI then aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'OI';
            {$endif Solaris}
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+GetTargetOS;
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+'-';
            aFPCUPBootstrapperName:=aFPCUPBootstrapperName+GetCompilerName(GetTargetCPU);

            Infoln(localinfotext+'Looking online for a FPCUP(deluxe) bootstrapper with name: '+aFPCUPBootstrapperName,etInfo);

            // We have successfully downloaded a list with available compilers through the API of GitHub.
            // However, this might fail due to throttling, so we need a fallback method.
            if ((result) AND (aCompilerList.Count>0)) then
            begin
              for i:=0 to Pred(aCompilerList.Count) do
              begin
                aFPCUPBootstrapURL:=aFPCUPBootstrapperName;

                aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);

                {$ifdef FREEBSD}
                if (NOT aFPCUPCompilerFound) then
                begin
                  j:=GetFreeBSDVersion;
                  if j=0 then j:=DEFAULTFREEBSDVERSION; // Use FreeBSD default version when GetFreeBSDVersion does not give a result
                  aFPCUPBootstrapURL:=StringReplace(aFPCUPBootstrapperName,'-'+GetTargetOS,'-'+GetTargetOS+InttoStr(j),[]);
                  aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);
                  if (NOT aFPCUPCompilerFound) then
                  begin
                    //try other versions if available
                    for j:=14 downto 9 do
                    begin
                      aFPCUPBootstrapURL:=StringReplace(aFPCUPBootstrapperName,'-'+GetTargetOS,'-'+GetTargetOS+InttoStr(j),[]);
                      aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);
                      if aFPCUPCompilerFound then break;
                    end;
                  end;
                end;
                {$endif}

                if aFPCUPCompilerFound then
                begin
                  aFPCUPBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+aFPCUPBootstrapURL;
                  break;
                end;
              end;
            end;

            // Fallback method : manually check the URL
            if (NOT aFPCUPCompilerFound) then
            begin
              aFPCUPBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+aFPCUPBootstrapperName;
              aFPCUPCompilerFound:=aDownLoader.checkURL(aFPCUPBootstrapURL);
              {$ifdef FREEBSD}
              if (NOT aFPCUPCompilerFound) then
              begin
                j:=GetFreeBSDVersion;
                if j=0 then j:=DEFAULTFREEBSDVERSION; // Use FreeBSD default version when GetFreeBSDVersion does not give a result
                aFPCUPBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+StringReplace(aFPCUPBootstrapperName,'-'+GetTargetOS,'-'+GetTargetOS+InttoStr(j),[]);
                aFPCUPCompilerFound:=aDownLoader.checkURL(aFPCUPBootstrapURL);
              end;
              {$endif}
            end;

            if aFPCUPCompilerFound then
            begin
              Infoln(localinfotext+'Success: found a FPCUP(deluxe) bootstrapper with version '+aLocalFPCUPBootstrapVersion,etInfo);
            end
            else
            begin
              // look for a previous (fitting) compiler if not found, and use overrideversioncheck
              FBootstrapCompilerOverrideVersionCheck:=true;
              s:=GetBootstrapCompilerVersionFromVersion(aLocalFPCUPBootstrapVersion);
              if aLocalFPCUPBootstrapVersion<>s
                 then aLocalFPCUPBootstrapVersion:=s
                 else break;
            end;

          end;

        finally
          aCompilerList.Free;
        end;

        // found a less official FPCUP bootstrapper !
        if (aFPCUPCompilerFound) then
        begin
          if FBootstrapCompilerURL='' then
          begin
            aCompilerFound:=true;
            Infoln(localinfotext+'Got a bootstrap compiler from FPCUP(deluxe) provided bootstrapper binaries.',etInfo);
            FBootstrapCompilerURL := aFPCUPBootstrapURL;
            // set standard bootstrap compilername
            FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);
          end
          else
          begin
            if (
              ( CalculateNumericalVersion(aLocalFPCUPBootstrapVersion)>CalculateNumericalVersion(aLocalBootstrapVersion) )
              OR
              ( (CalculateNumericalVersion(aLocalFPCUPBootstrapVersion)=CalculateNumericalVersion(aLocalBootstrapVersion)) AND aLookForBetterAlternative )
              ) then
            begin
              aCompilerFound:=true;
              Infoln(localinfotext+'Got a better [version] bootstrap compiler from FPCUP(deluxe) bootstrap binaries.',etInfo);
              if aLookForBetterAlternative then Infoln(localinfotext+'This is important for OSX Cataline, that dislikes FPC multi-arch binaries.',etInfo);
              aLocalBootstrapVersion:=aLocalFPCUPBootstrapVersion;
              FBootstrapCompilerURL:=aFPCUPBootstrapURL;
              // set standard bootstrap compilername
              FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);
            end;
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
          // Panic last resort ... ;-)
          // Try to get bootstrapper from a FPC snapshot binary
          if NativeFPCBootstrapCompiler then
          begin
            aCompilerList:=TStringList.Create;
            try
              aCompilerList.Clear;
              VersionFromString(aBootstrapVersion,i,j,k,{%H-}l);
              s:=FPCFTPSNAPSHOTURL+'/v'+InttoStr(i)+InttoStr(j)+'/'+GetTargetCPUOS+'/';
              result:=aDownLoader.getFTPFileList(s,aCompilerList);
              if result then
              begin
                for i:=0 to Pred(aCompilerList.Count) do
                begin
                  if (Pos(GetTargetCPUOS+'-fpc-',aCompilerList[i])=1) then
                  begin
                    // we got a snapshot
                    Infoln(localinfotext+'Found a official FPC snapshot achive.',etDebug);
                    aLocalBootstrapVersion:=aBootstrapVersion;
                    FBootstrapCompilerURL:=s+aCompilerList[i];
                    aCompilerFound:=True;
                    // set standard bootstrap compilername
                    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);
                    // as we do not know exactly what we get, set override
                    FBootstrapCompilerOverrideVersionCheck:=true;
                    break;
                  end;
                end;
              end;
            finally
              aCompilerList.Free;
            end;
          end;
          if NOT aCompilerFound then
          begin
            Infoln(localinfotext+'No bootstrapper local and online. Fatal. Stopping.',etError);
            exit(false);
          end;
        end
        else
        begin
          // there is a bootstrapper available: just use it !!
          Infoln(localinfotext+'No correct bootstrapper. But going to use the available one with version ' + s,etInfo);
          FBootstrapCompilerOverrideVersionCheck:=true;
          result:=true;
        end;
      end;

      if (aCompilerFound) AND (FBootstrapCompilerURL<>'') then
      begin
        // final check ... do we have the correct (as in version) compiler already ?
        Infoln(localinfotext+'Check if we already have a bootstrap compiler with version '+ aLocalBootstrapVersion,etInfo);
        if s<>aLocalBootstrapVersion then
        begin
          Infoln(localinfotext+'No correct bootstrapper. Going to download bootstrapper from '+ FBootstrapCompilerURL,etInfo);
          result:=DownloadBootstrapCompiler;
          // always use the newly downloaded bootstrapper !!
          if result then
          begin
            Compiler:=FBootstrapCompiler;
            s:=CompilerVersion(FCompiler);
            //Check if version is correct: if so, disable overrideversioncheck !
            if s=aBootstrapVersion then FBootstrapCompilerOverrideVersionCheck:=false;
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
  WritelnLog(localinfotext+'FPC source directory:   '+FSourceDirectory,false);
  WritelnLog(localinfotext+'FPC install directory:  '+FInstallDirectory,false);
  {$IFDEF MSWINDOWS}
  WritelnLog(localinfotext+'Make/binutils path:     '+FMakeDir,false);
  {$ENDIF MSWINDOWS}

  if result then
  begin

    if assigned(CrossInstaller) then
    begin
      CrossInstaller.SolarisOI:=FSolarisOI;
      CrossInstaller.MUSL:=FMUSL;
    end;

    {$IFDEF MSWINDOWS}
    s:='';
    if Length(FSVNDirectory)>0
       then s:=PathSeparator+ExcludeTrailingPathDelimiter(FSVNDirectory);
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // add install/fpc/utils to solve data2inc not found by fpcmkcfg
    // also add src/fpc/utils to solve data2inc not found by fpcmkcfg
    SetPath(
      ExcludeTrailingPathDelimiter(FFPCCompilerBinPath)+PathSeparator+ {compiler for current architecture}
      FMakeDir+PathSeparator+
      FBootstrapCompilerDirectory+PathSeparator+
      ExcludeTrailingPathDelimiter(FInstallDirectory)+PathSeparator+
      IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
      IncludeTrailingPathDelimiter(FInstallDirectory)+'utils'+PathSeparator+
      ExcludeTrailingPathDelimiter(FSourceDirectory)+PathSeparator+
      IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+PathSeparator+
      IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+
      s,
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
      ExcludeTrailingPathDelimiter(FFPCCompilerBinPath)+PathSeparator+
      FBootstrapCompilerDirectory+PathSeparator+
      ExcludeTrailingPathDelimiter(FInstallDirectory)+PathSeparator+
      IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
      IncludeTrailingPathDelimiter(FInstallDirectory)+'utils'+PathSeparator+
      ExcludeTrailingPathDelimiter(FSourceDirectory)+PathSeparator+
      IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+PathSeparator+
      IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+PathSeparator+
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
  FPCUPMAGIC=': base settings';
var
  RequiredBootstrapVersion:string;
  RequiredBootstrapVersionLow:string;
  RequiredBootstrapVersionHigh:string;
  FPCCfg: string;
  FPCMkCfg: string; //path+file of fpcmkcfg
  ConfigText,ConfigTextStore:TStringList;
  OperationSucceeded: boolean;
  PlainBinPath: string; //directory above the architecture-dependent FBinDir
  s,s2:string;
  TxtFile:Text;
  x,y:integer;
  VersionSnippet:string;

  function CheckFPCMkCfgOption(aOption:string):boolean;
  var
    aIndex:integer;
  begin
    aIndex:=-1;
    Processor.Process.Parameters.Clear;
    Processor.Process.Parameters.Add('-h');
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
        WritelnLog(etError, infotext+'Running [CheckFPCMkCfgOption] failed with an exception!'+LineEnding+'Details: '+E.Message,true);
      end;
    end;
    result:=(aIndex<>-1);
  end;


  function RunFPCMkCfgOption(aFile:string):boolean;
  begin
    result:=false;
    Processor.Process.Parameters.Add('-d');
    Processor.Process.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FInstallDirectory));
    Processor.Process.Parameters.Add('-o');
    Processor.Process.Parameters.Add('' + aFile + '');
    Infoln(infotext+'Creating '+ExtractFileName(aFile));
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

  Infoln(infotext+'Building module '+ModuleName+'...',etInfo);

  s:=IncludeTrailingPathDelimiter(FSourceDirectory) + MAKEFILENAME;
  if (NOT FileExists(s)) then
  begin
    Infoln(infotext+s+' not found. Severe error. Should not happen. Aborting.',etError);
    exit(false);
  end;

  VersionSnippet:=SourceVersionStr;
  if (Self is TFPCCrossInstaller) then
  begin
    Compiler:=GetFPCInBinDir;
    s2:='FPC '+CrossInstaller.RegisterName+' cross-builder: Detected source version FPC (source): '
  end
  else
  begin
    s2:='FPC native builder: Detected source version FPC (source): ';
    if (VersionSnippet='0.0.0') then
    begin
      VersionSnippet:=CompilerVersion(GetFPCInBinDir);
      if VersionSnippet<>'0.0.0' then
        s2:='FPC native builder: Detected source version FPC (compiler): ';
    end;
  end;

  if (VersionSnippet<>'0.0.0') then
    Infoln(s2+VersionSnippet, etInfo);

  // if cross-compiling, skip a lot of code
  // trust the previous work done by this code for the native installer!
  if (NOT (Self is TFPCCrossInstaller)) then
  begin
    RequiredBootstrapVersion:='0.0.0';

    RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromSource(FSourceDirectory,True);
    RequiredBootstrapVersionHigh:=GetBootstrapCompilerVersionFromSource(FSourceDirectory,False);

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
      if NativeFPCBootstrapCompiler then
      begin
        s:=GetCompilerName(GetTargetCPU);
        s:=Which(s);
        {$ifdef Darwin}
        // Due to codesigning, do not copy, but just use it.
        if FileExists(s) then Compiler:=s;
        {$else}
        //Copy the compiler to our bootstrap directory
        if FileExists(s) then FileUtil.CopyFile(s,FCompiler);
        {$endif}
        if NOT FileExists(s) then
        begin
          s:='fpc'+GetExeExt;
          s:=Which(s);
          if FileExists(s) then Compiler:=s;
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
      result:=InitModule(RequiredBootstrapVersion);

      {
      if (NOT result) then
      begin
        // Retry with the lowest requirement
        //RequiredBootstrapVersion:=RequiredBootstrapVersionLow;
        result:=InitModule(RequiredBootstrapVersionLow);
      end;
      }

      if (CompilerVersion(FCompiler)=RequiredBootstrapVersion)
        then Infoln(infotext+'To compile this FPC, we will use a fresh compiler with version : '+RequiredBootstrapVersion,etInfo)
        else
        begin
          // check if we have a lower acceptable requirement for the bootstrapper
          if (CompilerVersion(FCompiler)=RequiredBootstrapVersionLow) then
          begin
            // if so, set bootstrapper to lower one !!
            RequiredBootstrapVersion:=RequiredBootstrapVersionLow;
            Infoln(infotext+'To compile this FPC, we can also (and will) use (required) a fresh compiler with version : '+RequiredBootstrapVersion,etInfo);
          end;
        end;
    end;

    {$IFDEF CPUAARCH64}
    // we build with >=3.2.0 , while aarch64 is not available for FPC < 3.2.0
    FBootstrapCompilerOverrideVersionCheck:=true;
    {$ENDIF CPUAARCH64}
    {$IF DEFINED(CPUPOWERPC64) AND DEFINED(FPC_ABI_ELFV2)}
    // we build with >=3.2.0 , while ppc64le is not available for FPC < 3.2.0
    FBootstrapCompilerOverrideVersionCheck:=true;
    {$ENDIF}

    // get the correct binutils (Windows only)
    if (Pos('/branches/',URL)>0) then
    begin
      CreateBinutilsList(RequiredBootstrapVersion);
    end
    else
    begin
      s:=VersionSnippet;
      x:=GetReleaseCandidateFromSource(FSourceDirectory);
      if (x<>0) then
        s:=s+'.rc'+InttoStr(x);
      CreateBinutilsList(s);
    end;

    result:=CheckAndGetNeededBinUtils;

    {$ifdef Solaris}
    //sometimes, gstrip does not exist on Solaris ... just copy it to a place where it can be found ... tricky
    if (NOT FileExists('/usr/bin/gstrip')) AND (FileExists('/usr/bin/strip')) then
    begin
      if DirectoryExists(FFPCCompilerBinPath) then
      begin
        s:=FFPCCompilerBinPath+'gstrip';
        if (NOT FileExists(s)) then FileUtil.CopyFile('/usr/bin/strip',s);
      end
      else
      begin
        ForceDirectoriesSafe(FInstallDirectory);
        s:=IncludeTrailingPathDelimiter(FInstallDirectory)+'gstrip';
        if (NOT FileExists(s)) then FileUtil.CopyFile('/usr/bin/strip',s);
      end;
    end;
    {$endif}


    //if not result then exit;

    {$ifdef win64}
    // Deals dynamically with either ppc386.exe or native ppcx64.exe
    if (Pos('ppc386.exe',FCompiler)>0) OR (GetCompilerTargetOS(FCompiler)='win32') then //need to build ppcx64 before
    begin
      Infoln('We have ppc386. We need ppcx64. So make it !',etInfo);
      Processor.Executable := Make;
      Processor.Process.Parameters.Clear;
      {$IFDEF MSWINDOWS}
      if Length(Shell)>0 then Processor.Process.Parameters.Add('SHELL='+Shell);
      {$ENDIF}
      Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Process.Parameters.Add('compiler_cycle');
      if (NOT FNoJobs) then
      begin
        Processor.Process.Parameters.Add('--jobs='+IntToStr(FCPUCount));
        Processor.Process.Parameters.Add('FPMAKEOPT=--threads='+IntToStr(FCPUCount));
      end;
      Processor.Process.Parameters.Add('FPC='+FCompiler);
      Processor.Process.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Process.Parameters.Add('OS_SOURCE=win32');
      Processor.Process.Parameters.Add('CPU_SOURCE=i386');
      Processor.Process.Parameters.Add('OS_TARGET=win64');
      Processor.Process.Parameters.Add('CPU_TARGET=x86_64');
      Processor.Process.Parameters.Add('OPT='+STANDARDCOMPILERVERBOSITYOPTIONS);
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        Processor.Process.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      Infoln(infotext+'Perform compiler cycle for Windows FPC64.',etInfo);
      ProcessorResult:=Processor.ExecuteAndWait;
      if ProcessorResult <> 0 then
      begin
        result := False;
        WritelnLog(etError, infotext+'Failed to build ppcx64 bootstrap compiler.');
        exit;
      end;
      // Now we can change the compiler from the i386 to the x64 compiler:
      Compiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64.exe';
      FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\ppcx64.exe',FCompiler,[cffOverwriteFile]);
      FBootstrapCompilerOverrideVersionCheck:=True;
    end;
    {$endif win64}
    {$ifdef darwin}
    if Pos('ppcuniversal',FCompiler)>0 then //need to build ppcxxx before
    begin
      Infoln(infotext+'We have ppcuniversal. We need '+TargetCompilerName+'. So make it !',etInfo);
      Processor.Executable := Make;
      Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Process.Parameters.Clear;
      Processor.Process.Parameters.Add('compiler_cycle');
      if (NOT FNoJobs) then
      begin
        Processor.Process.Parameters.Add('--jobs='+IntToStr(FCPUCount));
        Processor.Process.Parameters.Add('FPMAKEOPT=--threads='+IntToStr(FCPUCount));
      end;
      Processor.Process.Parameters.Add('FPC='+FCompiler);
      Processor.Process.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Process.Parameters.Add('OS_SOURCE=' + GetTargetOS);
      Processor.Process.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);
      Processor.Process.Parameters.Add('OS_TARGET=' + GetTargetOS);
      Processor.Process.Parameters.Add('CPU_TARGET=' + GetTargetCPU);
      Processor.Process.Parameters.Add('OPT='+STANDARDCOMPILERVERBOSITYOPTIONS);
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        Processor.Process.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      Infoln(infotext+'Perform compiler cycle for Darwin.',etInfo);
      ProcessorResult:=Processor.ExecuteAndWait;
      if ProcessorResult <> 0 then
      begin
        result := False;
        WritelnLog(etError, infotext+'Failed to build '+s+' bootstrap compiler.');
        exit;
      end;

      // copy over the fresh bootstrapper, if any
      if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName) then
      begin
        // Now we can change the compiler from the ppcuniversal to the target compiler:
        Compiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+TargetCompilerName;
        Infoln(infotext+'Copy fresh compiler ('+TargetCompilerName+') into: '+ExtractFilePath(FCompiler),etDebug);
        FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName,
          FCompiler);
        fpChmod(FCompiler,&755);
        FBootstrapCompilerOverrideVersionCheck:=True;
      end;
    end;
    {$endif darwin}
  end;//(NOT (Self is TFPCCrossInstaller))

  // Do we need to force the use of libc :
  FUseLibc:=False;

  if (Self is TFPCCrossInstaller) then
  begin
    if (CrossInstaller.TargetOS=TOS.dragonfly) then FUseLibc:=True;
    if (CrossInstaller.TargetOS=TOS.freebsd) then FUseLibc:=True;
    if (CrossInstaller.TargetOS=TOS.openbsd) AND (SourceVersionNum>CalculateNumericalVersion('3.2.0')) then FUseLibc:=True;
  end
  else
  begin
    if (GetTargetOS=GetOS(TOS.dragonfly)) then FUseLibc:=True;
    if (GetTargetOS=GetOS(TOS.freebsd)) then FUseLibc:=True;
    if (GetTargetOS=GetOS(TOS.openbsd)) AND (SourceVersionNum>CalculateNumericalVersion('3.2.0')) then FUseLibc:=True;
  end;

  // Now: the real build of FPC !!!
  OperationSucceeded:=BuildModuleCustom(ModuleName);

  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
    // copy the freshly created compiler to the bin/$fpctarget directory so that
    // fpc can find it
    if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName) then
    begin
      Infoln(infotext+'Copy compiler ('+TargetCompilerName+') into: '+ExcludeTrailingPathDelimiter(FFPCCompilerBinPath),etDebug);
      s:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName;
      s2:=FFPCCompilerBinPath+TargetCompilerName;
      //FileUtil.CopyFile(s,s2);
      SysUtils.DeleteFile(s2);
      SysUtils.RenameFile(s,s2);
      fpChmod(s2,&755);
    end;

    // create link 'units' below FInstallDirectory to
    // <somewhere>/lib/fpc/$fpcversion/units
    s:=IncludeTrailingPathDelimiter(FInstallDirectory)+'units';
    DeleteFile(s);
    fpSymlink(pchar(IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc/'+SourceVersionStr+'/units'),
      pchar(s));
  end;
  {$ENDIF UNIX}

  // only create fpc.cfg and other configs with fpcmkcfg when NOT crosscompiling !
  if (OperationSucceeded) AND (NOT (Self is TFPCCrossInstaller)) then
  begin
    // Find out where fpcmkcfg lives
    if (OperationSucceeded) then
    begin
      FPCMkCfg:=FFPCCompilerBinPath+FPCMAKECONFIG+GetExeExt;
      OperationSucceeded:=CheckExecutable(FPCMkCfg,['-h'],FPCMAKECONFIG);
      if (NOT OperationSucceeded) then
      begin
        Infoln(infotext+'Did not find '+FPCMAKECONFIG+GetExeExt+' in '+ExtractFileDir(FPCMkCfg),etDebug);
        FPCMkCfg:=ConcatPaths([FInstallDirectory,'bin',FPCMAKECONFIG+GetExeExt]);
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

    FPCCfg := FFPCCompilerBinPath + FPCCONFIGFILENAME;

    if (OperationSucceeded) then
    begin
      Processor.Executable:=FPCMkCfg;
      Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FInstallDirectory);

      s2:= ExtractFilePath(FPCMkCfg)+FPFILENAME+GetExeExt;
      if FileExists(s2) then
      begin
        s := FFPCCompilerBinPath + FPCONFIGFILENAME;
        if (NOT FileExists(s)) then
        begin
          //create fp.cfg
          //if CheckFPCMkCfgOption('-1') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.Process.Parameters.Add('-1');
            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('fpctargetos='+GetTargetOS);

            {$IFDEF UNIX}
            //s2:=GetStartupObjects;
            //if Length(s2)>0 then
            //begin
            //  Processor.Process.Parameters.Add('-d');
            //  Processor.Process.Parameters.Add('GCCLIBPATH= -Fl'+s2);
            //end;
            {$ENDIF UNIX}

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := FFPCCompilerBinPath + FPINIFILENAME;
        if (NOT FileExists(s)) then
        begin
          //create fp.ini
          //if CheckFPCMkCfgOption('-2') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.Process.Parameters.Add('-2');

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;
      end;

      s2:= ExtractFilePath(FPCMkCfg)+FPCPKGFILENAME+GetExeExt;
      if FileExists(s2) then
      begin

        s2:=ConcatPaths([FBaseDirectory,PACKAGESLOCATION]);
        ForceDirectoriesSafe(s2);

        s2 := ConcatPaths([FBaseDirectory,PACKAGESCONFIGDIR]);
        ForceDirectoriesSafe(s2);

        s  := IncludeTrailingPathDelimiter(s2)+FPCPKGCONFIGFILENAME;
        if (NOT FileExists(s)) then
        begin
          ForceDirectoriesSafe(s2);
          //Create package configuration fppkg.cfg
          //if CheckFPCMkCfgOption('-3') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.Process.Parameters.Add('-3');

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('LocalRepository='+ConcatPaths([FBaseDirectory,PACKAGESLOCATION])+PathDelim);

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('CompilerConfigDir='+IncludeTrailingPathDelimiter(s2));

            Processor.Process.Parameters.Add('-d');
            {$ifdef MSWINDOWS}
            Processor.Process.Parameters.Add('GlobalPath='+IncludeTrailingPathDelimiter(FInstallDirectory));
            {$ELSE}
            Processor.Process.Parameters.Add('GlobalPath='+ConcatPaths([FInstallDirectory,'lib','fpc'])+PathDelim+'{CompilerVersion}'+PathDelim);
            {$ENDIF}

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('GlobalPrefix='+ExcludeTrailingPathDelimiter(FInstallDirectory));

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('UserPathSuffix=users');

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := IncludeTrailingPathDelimiter(s2)+FPCPKGCOMPILERTEMPLATE;
        if (NOT FileExists(s)) then
        begin
          ForceDirectoriesSafe(s2);
          //Create default compiler template
          //if CheckFPCMkCfgOption('-4') then
          begin
            Processor.Process.Parameters.Clear;
            Processor.Process.Parameters.Add('-4');

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('GlobalPrefix='+ExcludeTrailingPathDelimiter(FInstallDirectory));

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('fpcbin='+FCompiler);

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('fpctargetos='+GetTargetOS);

            Processor.Process.Parameters.Add('-d');
            Processor.Process.Parameters.Add('fpctargetcpu='+GetTargetCPU);

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;
      end;

      s := FPCCfg;
      if (NOT FileExists(s)) then
      begin
        //create fpc.cfg
        Processor.Process.Parameters.Clear;
        RunFPCMkCfgOption(s);
      end
      else
      begin
        Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
      end;

      if Ultibo then
      begin
        // Creating Ultibo configuration files

        s := FFPCCompilerBinPath + 'RPI.CFG';
        if (NOT FileExists(s)) then
        begin
          //create RPI.CFG
          AssignFile(TxtFile,s);
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
            writeln(TxtFile,'-XParm-none-eabi-');
            s2:=ConcatPaths([FInstallDirectory,'units','ultibo-'+FPC_SUBARCH_MAGIC]);
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib'+DirectorySeparator+'vc4');
          finally
            CloseFile(TxtFile);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := FFPCCompilerBinPath + 'RPI2.CFG';
        if (NOT FileExists(s)) then
        begin
          //create RPI2.CFG
          AssignFile(TxtFile,s);
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
            writeln(TxtFile,'-XParm-none-eabi-');
            s2:=ConcatPaths([FInstallDirectory,'units','ultibo-'+FPC_SUBARCH_MAGIC]);
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib'+DirectorySeparator+'vc4');
          finally
            CloseFile(TxtFile);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := FFPCCompilerBinPath + 'RPI3.CFG';
        if (NOT FileExists(s)) then
        begin
          //create RPI3.CFG
          AssignFile(TxtFile,s);
          Rewrite(TxtFile);
          try
            writeln(TxtFile,'#');
            writeln(TxtFile,'# Raspberry Pi 3B specific config file');
            writeln(TxtFile,'#');
            writeln(TxtFile,'-CfVFPV3');
            writeln(TxtFile,'-CIARM');
            writeln(TxtFile,'-CaEABIHF');
            writeln(TxtFile,'-OoFASTMATH');
            writeln(TxtFile,'-dRPI3');
            writeln(TxtFile,'-XParm-none-eabi-');
            s2:=ConcatPaths([FInstallDirectory,'units','ultibo-'+FPC_SUBARCH_MAGIC]);
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib'+DirectorySeparator+'vc4');
          finally
            CloseFile(TxtFile);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := FFPCCompilerBinPath + 'QEMUVPB.CFG';
        if (NOT FileExists(s)) then
        begin
          //create QEMUVPB.CFG
          AssignFile(TxtFile,s);
          Rewrite(TxtFile);
          try
            writeln(TxtFile,'#');
            writeln(TxtFile,'# QEMU VersatilePB specific config file');
            writeln(TxtFile,'#');
            writeln(TxtFile,'-CfVFPV3');
            writeln(TxtFile,'-CIARM');
            writeln(TxtFile,'-CaEABIHF');
            writeln(TxtFile,'-OoFASTMATH');
            writeln(TxtFile,'-dQEMUVPB');
            writeln(TxtFile,'-XParm-none-eabi-');
            s2:=ConcatPaths([FInstallDirectory,'units','ultibo-'+FPC_SUBARCH_MAGIC]);
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'rtl');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'packages');
            writeln(TxtFile,'-Fu'+s2+DirectorySeparator+'lib');
          finally
            CloseFile(TxtFile);
          end;
        end
        else
        begin
          Infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

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
        writeln(TxtFile,'-Fu'+IncludeTrailingPathDelimiter(FInstallDirectory)+'units/$FPCTARGET/');
        writeln(TxtFile,'-Fu'+IncludeTrailingPathDelimiter(FInstallDirectory)+'units/$FPCTARGET/*');
        writeln(TxtFile,'-Fu'+IncludeTrailingPathDelimiter(FInstallDirectory)+'units/$FPCTARGET/rtl');
        writeln(TxtFile,'');
        writeln(TxtFile,'# searchpath for tools');
        writeln(TxtFile,'-FD'+IncludeTrailingPathDelimiter(FInstallDirectory)+'bin/$FPCTARGET');
        writeln(TxtFile,'');
        writeln(TxtFile,'# binutils prefix for cross compiling');
        writeln(TxtFile,'#IFDEF FPC_CROSSCOMPILING');
        writeln(TxtFile,'#IFDEF NEEDCROSSBINUTILS');
        writeln(TxtFile,'  -XP$FPCTARGET-');
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
        ConfigText.Append('# Adding binary tools paths to');
        ConfigText.Append('# plain bin dir and architecture bin dir so');
        ConfigText.Append('# fpc 3.1+ fpcres etc can be found.');

        // On *nix FPC 3.1.x, both "architecture bin" and "plain bin" may contain tools like fpcres.
        // Adding this won't hurt on Windows.
        // Adjust for that
        PlainBinPath:=SafeExpandFileName(SafeExpandFileName(FFPCCompilerBinPath+'..'+DirectorySeparator+'..'));
        s:='-FD'+IncludeTrailingPathDelimiter(FFPCCompilerBinPath)+';'+IncludeTrailingPathDelimiter(PlainBinPath);
        ConfigText.Append(s);
        {$IFDEF UNIX}
        // Need to add appropriate library search path
        // where it is e.g /usr/lib/arm-linux-gnueabihf...
        ConfigText.Append('# library search path');
        s:='-Fl/usr/lib/$FPCTARGET'+';'+'/usr/lib/$FPCTARGET-gnu'+';'+'/lib/$FPCTARGET'+';'+'/lib/$FPCTARGET-gnu';
        {$IFDEF cpuarm}
        {$IFDEF CPUARMHF}
        s:=s+';'+'/usr/lib/$FPCTARGET-gnueabihf';
        {$ELSE}
        s:=s+';'+'/usr/lib/$FPCTARGET-gnueabi';
        {$ENDIF CPUARMHF}
        {$ENDIF cpuarm}
        ConfigText.Append(s);

        ConfigText.Append('#IFNDEF FPC_CROSSCOMPILING');

        s:=GetStartupObjects;
        if Length(s)>0 then
        begin
          ConfigText.Append('-Fl'+s);
        end;

        {$ifdef Linux}
        if FMUSL then ConfigText.Append('-FL'+FMUSLLinker);
        {$endif}

        {$IF (defined(BSD)) and (not defined(Darwin))}
        s:='-Fl/usr/local/lib'+';'+'/usr/pkg/lib';
        {$ifndef FPCONLY}
        //VersionSnippet:=GetEnvironmentVariable('X11BASE');
        //if Length(VersionSnippet)>0 then s:=s+';'+VersionSnippet
        s:=s+';'+'/usr/X11R6/lib'+';'+'/usr/X11R7/lib';
        {$endif FPCONLY}
        ConfigText.Append(s);
        {$endif}

        if UseLibc then ConfigText.Append('-dFPC_USE_LIBC');
        {$ifdef freebsd}
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

        ConfigText.Append('#ENDIF');

        {$ifdef solaris}
        {$IF defined(CPUX64) OR defined(CPUX86)}
        //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
        ConfigText.Append('-Xn');
        {$endif}
        {$endif}

        {$ENDIF UNIX}

        {$IFDEF DARWIN}
        ConfigText.Append('');
        ConfigText.Append('# Add some extra OSX options');
        ConfigText.Append('#IFDEF DARWIN');
        s:=GetDarwinSDKVersion('macosx');
        if Length(s)>0 then
        begin
          ConfigText.Append('# Prevents crti not found linking errors');
          ConfigText.Append('#IFNDEF FPC_CROSSCOMPILING');
          //ConfigText.Append('#IFDEF CPU'+UpperCase(GetTargetCPU));
          if CompareVersionStrings(s,'10.8')>=0 then
            ConfigText.Append('-WM10.8')
          else
            ConfigText.Append('-WM'+s);
          ConfigText.Append('#ENDIF');
        end;
        ConfigText.Append('#ENDIF');

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
          end;
          s:=GetDarwinToolsLocation;
          if (Length(s)>0) AND (DirectoryExists(s)) then
            ConfigText.Append('-FD'+s);
        end;
        {$ENDIF DARWIN}

        {$ifndef FPCONLY}
        {$ifdef LCLQT5}
        ConfigText.Append('#IFNDEF FPC_CROSSCOMPILING');
        ConfigText.Append('# Adding some standard paths for QT5 locations ... bit dirty, but works ... ;-)');
        {$ifdef Darwin}
        ConfigText.Append('-Fl'+IncludeTrailingPathDelimiter(FBaseDirectory)+'Frameworks');
        ConfigText.Append('-k-F'+IncludeTrailingPathDelimiter(FBaseDirectory)+'Frameworks');

        ConfigText.Append('-k-rpath');
        ConfigText.Append('-k@executable_path/../Frameworks');

        ConfigText.Append('-k-rpath');
        ConfigText.Append('-k'+IncludeTrailingPathDelimiter(FBaseDirectory)+'Frameworks');

        (*
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kQt5Pas');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kQtPrintSupport');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kQtWidgets');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kQtGui');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kQtNetwork');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kQtCore');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kOpenGL');
        ConfigText.Append('-k-framework');
        ConfigText.Append('-kAGL');
        *)

        {$else Darwin}
        {$ifdef Unix}
        //ConfigText.Append('-k"-rpath=./"');

        //For runtime
        ConfigText.Append('-k-rpath');
        ConfigText.Append('-k./');
        ConfigText.Append('-k-rpath');
        ConfigText.Append('-k$$ORIGIN');

        //For linktime
        //ConfigText.Append('-k-rpath-link');
        //ConfigText.Append('-k./');

        //ConfigText.Append('-k"-rpath=/usr/local/lib"');
        //ConfigText.Append('-k"-rpath=$$ORIGIN"');
        //ConfigText.Append('-k"-rpath=\\$$$$$\\ORIGIN"');
        //ConfigText.Append('-k-rpath');
        //ConfigText.Append('-k\\$$$$$\\ORIGIN');
        {$endif}
        {$endif Darwin}
        ConfigText.Append('#ENDIF');
        {$endif FPCONLY}
        {$endif LCLQT5}

        // add magic
        ConfigText.Append(SnipMagicEnd);

        // add empty line
        ConfigText.Append('');

        // add tail of ConfigText
        for x:=0 to (ConfigTextStore.Count-1) do
          ConfigText.Append(ConfigTextStore.Strings[x]);

        x:=ConfigText.IndexOf('# searchpath for fppkg user-specific packages');
        if x>-1 then
        begin
          ConfigText.Strings[x+1]:='-Fu'+ConcatPaths([FBaseDirectory,PACKAGESLOCATION,'units','$FPCTARGET'])+'/*';
        end;

        ConfigText.SaveToFile(FPCCfg);
      finally
        ConfigText.Free;
        ConfigTextStore.Free;
      end;

      Infoln(infotext+'Tuning of fpc.cfg ready.');
    end;

    // do not build pas2js [yet]: separate install ... use the module with rtl
    // if OperationSucceeded then BuildModuleCustom('PAS2JS');
  end;

  if OperationSucceeded then
  begin
    Infoln(infotext+'Start search and removal of stale build files and directories. May take a while.');
    RemoveStaleBuildDirectories(FSourceDirectory,GetTargetCPU,GetTargetOS);
    Infoln(infotext+'Search and removal of stale build files and directories ready.');
    WritelnLog(infotext+'Update/build/config succeeded.',false);
  end;
  Result := OperationSucceeded;
end;

function TFPCInstaller.CleanModule(ModuleName: string): boolean;
// Make distclean is unreliable; at least for FPC.
// Running it twice apparently can fix a lot of problems; see FPC ML message
// by Jonas Maebe, 1 November 2012
// On Windows, removing fpmake.exe, see Build FAQ (Nov 2011), 2.5
var
  CrossCompiling: boolean;
  FileCounter:integer;
  DeleteList: TStringList;
  CPUOS_Signature:string;
  aCleanupCompiler,aCleanupCommand,aDir:string;
  aCleanupCommandList:TStringList;
  RunTwice:boolean;
begin
  result:=inherited;

  // if no sources, then exit;
  if result then exit;

  result:=InitModule;

  if not result then exit;

  CrossCompiling:=(Self is TFPCCrossInstaller);

  if CrossCompiling then
  begin
    CPUOS_Signature:=GetFPCTarget(false);
    // Delete any existing buildstamp file
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'build-stamp.'+CPUOS_Signature);
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'base.build-stamp.'+CPUOS_Signature);

    CrossInstaller.SetCrossOpt(CrossOPT);
    CrossInstaller.SetSubArch(CrossOS_SubArch);
    CrossInstaller.SetABI(CrossOS_ABI);
  end else CPUOS_Signature:=GetFPCTarget(true);

  {$IFDEF MSWINDOWS}
  // Remove all fpmakes
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'fpmake'+GetExeExt);
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'packages'+DirectorySeparator+'fpmake'+GetExeExt);
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'ide'+DirectorySeparator+'fpmake'+GetExeExt);
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('fpmake'+GetExeExt);
    DeleteFilesSubDirs(IncludeTrailingPathDelimiter(FSourceDirectory),DeleteList,CPUOS_Signature);
  finally
    DeleteList.Free;
  end;
  {$ENDIF}

  aCleanupCompiler:='';
  if (FSourceDirectory<>FInstallDirectory) then
    aCleanupCompiler:=IncludeTrailingPathDelimiter(FFPCCompilerBinPath)+GetCompilerName(GetTargetCPU);

  if (NOT FileExists(aCleanupCompiler)) then
  begin
    if FileExists(FCompiler)
       then aCleanupCompiler:=FCompiler
       else aCleanupCompiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);
  end;

  if FileExists(aCleanupCompiler) then
  begin
    if CrossCompiling then
      CrossInstaller.SetFPCVersion(aCleanupCompiler);

    Processor.Executable:=Make;
    Processor.Process.Parameters.Clear;
    {$IFDEF MSWINDOWS}
    if Length(Shell)>0 then Processor.Process.Parameters.Add('SHELL='+Shell);
    {$ENDIF}
    Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
    if (NOT FNoJobs) then
    begin
      Processor.Process.Parameters.Add('--jobs='+IntToStr(FCPUCount));
      Processor.Process.Parameters.Add('FPMAKEOPT=--threads='+IntToStr(FCPUCount));
    end;
    Processor.Process.Parameters.Add('FPC='+aCleanupCompiler);
    Processor.Process.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
    Processor.Process.Parameters.Add('FPCMAKE=' + FFPCCompilerBinPath+'fpcmake'+GetExeExt);
    Processor.Process.Parameters.Add('PPUMOVE=' + FFPCCompilerBinPath+'ppumove'+GetExeExt);
    Processor.Process.Parameters.Add('FPCDIR=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
    Processor.Process.Parameters.Add('PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
    Processor.Process.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
    {$IFDEF UNIX}
    Processor.Process.Parameters.Add('INSTALL_BINDIR='+ExcludeTrailingPathDelimiter(FFPCCompilerBinPath));
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Processor.Process.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    //Processor.Process.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    Processor.Process.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
    Processor.Process.Parameters.Add('OS_SOURCE='+GetTargetOS);
    {$ENDIF}
    if Self is TFPCCrossInstaller then
    begin  // clean out the correct compiler
      Processor.Process.Parameters.Add('OS_TARGET='+CrossInstaller.TargetOSName);
      Processor.Process.Parameters.Add('CPU_TARGET='+CrossInstaller.TargetCPUName);
      if (CrossInstaller.SubArch<>TSubarch.saNone) then Processor.Process.Parameters.Add('SUBARCH='+CrossInstaller.SubArchName);
    end
    else
    begin
      Processor.Process.Parameters.Add('CPU_TARGET='+GetTargetCPU);
      Processor.Process.Parameters.Add('OS_TARGET='+GetTargetOS);
    end;

    aCleanupCommandList:=TStringList.Create;
    try
      aCleanupCommandList.Append('distclean');

      if CrossCompiling then
      begin
        aCleanupCommandList.Clear;
        if (Self AS TFPCCrossInstaller).CompilerUpdateNeeded then
          aCleanupCommandList.Append('compiler_distclean')
        else
          Infoln({infotext+}'Skipping cross-compiler clean step: compiler seems to be up to date !!',etWarning);
        aCleanupCommandList.Append('rtl_distclean');
        if (Self AS TFPCCrossInstaller).PackagesNeeded then aCleanupCommandList.Append('packages_distclean');
      end;

      for aCleanupCommand in aCleanupCommandList do
        Processor.Process.Parameters.Add(aCleanupCommand);

      aCleanupCommand:=aCleanupCommandList.CommaText;

    finally
      aCleanupCommandList.Free;
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
    if (NOT CrossCompiling) then
    begin
      Infoln(infotext+'Deleting some FPC package config files.', etInfo);
      //DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+PACKAGESCONFIGDIR+DirectorySeparator+FPCPKGFILENAME);
      DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+PACKAGESCONFIGDIR+DirectorySeparator+FPCPKGCOMPILERTEMPLATE);
      {$IFDEF UNIX}
      // Delete any fpc.sh shell scripts
      Infoln(infotext+'Deleting fpc.sh script.', etInfo);
      Sysutils.DeleteFile(FFPCCompilerBinPath+'fpc.sh');
      {$ENDIF UNIX}
      Infoln(infotext+'Deleting revision.inc.', etInfo);
      aDir:=ConcatPaths([FSourceDirectory,'compiler']);
      DeleteFile(aDir+DirectorySeparator+'revision.inc');
    end;

    // Delete units
    // Alf: is it still needed: todo check

    if CrossCompiling then
    begin
     aDir:=(Self AS TFPCCrossInstaller).GetUnitsInstallDirectory(false);
    end
    else
    begin
      aDir:=ConcatPaths([FInstallDirectory,'units']);
      {$IFDEF UNIX}
      if FileIsSymlink(aDir) then
      begin
        try
          aDir:=GetPhysicalFilename(aDir,pfeException);
        except
        end;
      end;
      {$ENDIF UNIX}
      aDir:=aDir+DirectorySeparator+CPUOS_Signature;
    end;

    if DirectoryExists(aDir) then
    begin
      // Only allow unit directories inside our own install te be deleted
      if (Pos(FBaseDirectory,aDir)=1) then
        DeleteDirectoryEx(aDir);
    end;

    {$IFDEF MSWINDOWS}
    // delete the units directory !!
    // this is needed due to the fact that make distclean will not cleanout this units directory
    // make distclean will only remove the results of a make, not a make install
    aDir:=ConcatPaths([FSourceDirectory,'units',CPUOS_Signature]);
    // Only allow unit directories inside our own install te be deleted
    if (Pos(FBaseDirectory,aDir)=1) then
      DeleteDirectoryEx(aDir);
    {$ENDIF}

    // finally ... if something is still still still floating around ... delete it !!
    DeleteList := TStringList.Create;
    try
      (*
      FindAllFiles(DeleteList,FSourceDirectory, '*.ppu; *.a; *.o', True);
      if DeleteList.Count > 0 then
      begin
        for FileCounter := 0 to (DeleteList.Count-1) do
        begin
          if Pos(CPUOS_Signature,DeleteList.Strings[FileCounter])>0 then DeleteFile(DeleteList.Strings[FileCounter]);
        end;
      end;
      *)

      // delete stray unit and (static) object files, if any !!
      DeleteList.Add('.ppu');
      DeleteList.Add('.a');
      DeleteList.Add('.o');
      DeleteFilesExtensionsSubdirs(FSourceDirectory,DeleteList,CPUOS_Signature);

      // Delete stray compilers, if any !!
      FindAllFiles(DeleteList,IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler', '*'+GetExeExt, False);
      // But do not delete the PPC executable ... :-)
      FileCounter:=DeleteList.IndexOf(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc'+GetExeExt);
      if (FileCounter<>-1) then DeleteList.Delete(FileCounter);

      // delete stray executables, if any !!
      if (NOT CrossCompiling) then
      begin
        aDir:=ConcatPaths([FSourceDirectory,'compiler','utils']);
        FindAllFiles(DeleteList,aDir, '*'+GetExeExt, False);
        aDir:=ConcatPaths([FSourceDirectory,'utils']);
        FindAllFiles(DeleteList,aDir, '*'+GetExeExt, True);
      end;
      if DeleteList.Count > 0 then
      begin
        for FileCounter := 0 to (DeleteList.Count-1) do
        begin
          if IsExecutable(DeleteList.Strings[FileCounter]) then
          begin
            if Pos(MAKEFILENAME,DeleteList.Strings[FileCounter])=0 then
            begin
              Infoln(infotext+'Deleting [stray] executable: '+DeleteList.Strings[FileCounter],etInfo);
              DeleteFile(DeleteList.Strings[FileCounter]);
            end;
          end;
        end;
      end;

    finally
      DeleteList.Free;
    end;
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
  aRepoClient    : TRepoClient;
  s              : string;
  SourceVersion  : string;
begin
  result:=inherited;
  result:=InitModule;

  if (not result) then exit;

  FPreviousRevision:=GetFPCRevision;

  SourceVersion:='0.0.0';

  if Ultibo then
    FSourceDirectory:=StringReplace(FSourceDirectory,DirectorySeparator+'source','',[]);

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

  end;

  if Ultibo then
    FSourceDirectory:=IncludeTrailingPathDelimiter(FSourceDirectory)+'source';

  if result then
  begin
    SourceVersion:=GetVersion;
    if (SourceVersion<>'0.0.0') then
    begin
      s:=GetRevisionFromVersion(ModuleName,SourceVersion);
      if (Length(s)>0) then
      begin
        FActualRevision:=s;
        FPreviousRevision:=s;
      end;
    end
    else
    begin
      Infoln(infotext+'Could not get version of ' + ModuleName + ' sources. Expect severe errors.',etError);
    end;

    if FRepositoryUpdated then
    begin
      Infoln(infotext+ModuleName + ' was at revision: '+PreviousRevision,etInfo);
      Infoln(infotext+ModuleName + ' is now at revision: '+ActualRevision,etInfo);
    end
    else
    begin
      Infoln(infotext+ModuleName + ' is at revision: '+ActualRevision,etInfo);
      Infoln(infotext+'No updates for ' + ModuleName + ' found.',etInfo);
    end;
    UpdateWarnings:=TStringList.Create;
    try
      s:=SafeExpandFileName(IncludeTrailingPathDelimiter(FBaseDirectory)+REVISIONSLOG);
      if FileExists(s) then
        UpdateWarnings.LoadFromFile(s)
      else
      begin
        UpdateWarnings.Add('New install.');
        UpdateWarnings.Add('Date: '+DateTimeToStr(now));
        UpdateWarnings.Add('Location: '+FBaseDirectory);
        UpdateWarnings.Add('');
      end;
      UpdateWarnings.Add(FPCDATEMAGIC+DateTimeToStr(now));
      if aRepoClient<>nil then UpdateWarnings.Add(ModuleName+' URL: '+aRepoClient.Repository);
      UpdateWarnings.Add(ModuleName+' previous revision: '+PreviousRevision);
      UpdateWarnings.Add(FPCREVMAGIC+ActualRevision);
      if (aRepoClient.ClassType=FGitClient.ClassType) then
        UpdateWarnings.Add(FPCHASHMAGIC+aRepoClient.LocalRevision);
      UpdateWarnings.Add('');
      UpdateWarnings.SaveToFile(s);
    finally
      UpdateWarnings.Free;
    end;

    CreateRevision(ModuleName,ActualRevision);

    if (SourceVersion<>'0.0.0') then PatchModule(ModuleName);
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
  if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+MAKEFILENAME) and
    DirectoryExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler') and
    DirectoryExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'rtl') and
    ParentDirectoryIsNotRoot(IncludeTrailingPathDelimiter(FSourceDirectory)) then
    begin
    if DeleteDirectoryEx(FSourceDirectory)=false then
    begin
      WritelnLog(infotext+'Error deleting '+ModuleName+' directory '+FSourceDirectory);
      result:=false;
    end
    else
    result:=true;
    end
  else
  begin
    WritelnLog(infotext+'Invalid '+ModuleName+' directory :'+FSourceDirectory);
    result:=false;
  end;
end;

constructor TFPCInstaller.Create;
begin
  inherited Create;

  FTargetCompilerName:=GetCompilerName(GetTargetCPU);

  FCompiler := '';
  FUseLibc  := false;

  InitDone:=false;
end;

destructor TFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

