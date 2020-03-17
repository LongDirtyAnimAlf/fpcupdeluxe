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

{.$DEFINE crosssimple}
{$IFDEF WINDOWS}
{.$DEFINE buildnative}
{$ENDIF WINDOWS}

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
    FTargetCompilerName: string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
    FBootstrapCompilerOverrideVersionCheck: boolean; //Indicate to make we really want to compile with this version (e.g. trunk compiler), even if it is not the latest stable version
    FNativeFPCBootstrapCompiler: boolean;
    InitDone: boolean;
    function GetCompilerVersionNumber(aVersion: string; const index:byte=0): integer;
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // Retrieves compiler version string
    function GetCompilerTargetOS(CompilerPath: string): string;
    function GetCompilerTargetCPU(CompilerPath: string): string;
    function GetFPCVersionFromUrl(aUrl: string): string;
    function GetFPCVersionFromSource(aSourcePath: string): string;
    function GetBootstrapCompilerVersionFromVersion(aVersion: string): string;
    function GetBootstrapCompilerVersionFromSource(aSourcePath: string; GetLowestRequirement:boolean=false): string;
    // Creates fpc proxy script that masks general fpc.cfg
    function CreateFPCScript:boolean;
    // Downloads bootstrap compiler for relevant platform, reports result.
    function DownloadBootstrapCompiler: boolean;
    // Another way to get the compiler version string
    //todo: choose either GetCompilerVersion or GetFPCVersion
    function GetFPCVersion: string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule(aBootstrapVersion:string=''):boolean;
  public
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
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
    procedure SetTarget(aCPU,aOS,aSubArch:string);override;
    property CrossCompilerName: string read FCrossCompilerName;
  end;


implementation

uses
  StrUtils,
  fpcuputil,
  repoclient,
  FileUtil
  {$IFDEF UNIX}
    ,baseunix
    ,LazFileUtils
  {$ENDIF UNIX}
  {$IFDEF BSD}
    ,math
  {$ENDIF}
  ;

function InsertFPCCFGSnippet(FPCCFG,Snippet: string): boolean;
// Adds snippet to fpc.cfg file or replaces if if first line of snippet is present
// Returns success (snippet inserted or added) or failure
const
  INFOTEXT='FPCCrossInstaller (InsertFPCCFGSnippet: '+FPCCONFIGFILENAME+'): ';
var
  ConfigText: TStringList;
  i:integer;
  SnipBegin,SnipEnd,SnipEndLastResort: integer;
  SnippetText: TStringList;
begin
  result:=false;

  ConfigText:=TStringList.Create;
  {$IF FPC_FULLVERSION > 30100}
  //ConfigText.DefaultEncoding:=TEncoding.ASCII;
  {$ENDIF}
  SnippetText:=TStringList.Create;
  try
    SnippetText.Text:=Snippet;
    ConfigText.LoadFromFile(FPCCFG);


    // Look for exactly this string (first snippet-line always contains Magic + OS and CPU combo):
    i:=StringListStartsWith(ConfigText,SnippetText.Strings[0]);

    if (i<>-1) then
    begin
      SnipBegin:=i;

      SnipEnd:=MaxInt;
      SnipEndLastResort:=MaxInt;

      for i:=(SnipBegin+1) to ConfigText.Count-1 do
      begin
        // Once again, look exactly for this text:
        if ConfigText.Strings[i]=SnipMagicEnd then
        begin
          SnipEnd:=i;
          break;
        end;
        // in case of failure, find beginning of next (magic) config segment
        if Pos(SnipMagicBegin,ConfigText.Strings[i])>0 then
        begin
           SnipEndLastResort:=i-1;
           break;
        end;
      end;
      if SnipEnd=MaxInt then
      begin
        //apparently snippet was not closed correct
        if SnipEndLastResort<>MaxInt then
        begin
          SnipEnd:=SnipEndLastResort;
          infoln(INFOTEXT+'Existing snippet was not closed correct. Please check your '+FPCCONFIGFILENAME+'.',etWarning);
        end;
      end;
      if SnipEnd=MaxInt then
      begin
        //apparently snippet was not closed at all: severe error
        infoln(INFOTEXT+'Existing snippet was not closed at all. Please check your '+FPCCONFIGFILENAME+' for '+SnipMagicEnd+'.',etError);
        exit;
      end;

      if (SnippetText.Count>1) then
      begin
        // found end of OS and CPU snippet ; now check detailed CPU setting
        for i:=SnipBegin to SnipEnd do
        begin
          // do we have a CPU define ...
          if Pos('#IFDEF CPU',ConfigText.Strings[i])>0 then
          begin
            if (Pos(ConfigText.Strings[i]+LineEnding,Snippet)>0) then
            begin
              // we have exactly the same CPU type: delete snipped from config-file to replace !!!
              result:=true;
            end;
            break;
          end;
        end;
      end else result:=true;
    end;

    if result then
    begin
      // Replace snippet
      infoln(INFOTEXT+'Found existing snippet in '+FPCCFG+'. Replacing it with new version.',etInfo);
      for i:=SnipBegin to SnipEnd do ConfigText.Delete(SnipBegin);
    end
    else
    begin
      // Add snippet
      infoln(INFOTEXT+'Adding settings into '+FPCCFG+'.',etInfo);
      if ConfigText[ConfigText.Count-1]<>'' then ConfigText.Add('');
      SnipBegin:=ConfigText.Count;
    end;

    if (SnippetText.Count>1) then
    begin
      for i:=0 to (SnippetText.Count-1) do
      begin
        ConfigText.Insert(SnipBegin,SnippetText.Strings[i]);
        Inc(SnipBegin);
      end;

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
    ConfigText.Free;
    SnippetText.Free;
  end;

  infoln(INFOTEXT+'Inserting snippet in '+FPCCFG+' done.',etInfo);
end;

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
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
  if SysUtils.FindFirst(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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

procedure TFPCCrossInstaller.SetTarget(aCPU,aOS,aSubArch:string);
begin
  inherited;
  if Assigned(CrossInstaller) then FCrossCompilerName:=GetCrossCompilerName(CrossInstaller.TargetCPU);
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
  ChosenCompiler:String; //Compiler to be used for cross compiling
  i,j:integer;
  OldPath:String;
  Options:String;
  s1,s2:string;
  Counter:integer;
  LibsAvailable,BinsAvailable:boolean;
  MakeCycle:TSTEPS;
  ARMArch:TARMARCH;
  Minimum_OSX,Minimum_iOS:string;
begin
  result:=inherited;
  // Make crosscompiler using new compiler
  { Note: command line equivalents for Win32=>Win64 cross compiler:
  set path=c:\development\fpc\bin\i386-win32;c:\development\fpcbootstrap
  make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo all OS_TARGET=win64 CPU_TARGET=x86_64
  rem already gives compiler\ppcrossx64.exe, compiler\ppcx64.exe
  make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo crossinstall OS_TARGET=win64 CPU_TARGET=x86_64
  rem gives bin\i386-win32\ppcrossx64.exe

  Note: make install CROSSINSTALL=1 apparently installs, but does NOT install utilities (ld etc?) for that
  platform; see posting Jonas Maebe https://lists.freepascal.org/lists/fpc-pascal/2011-August/030084.html
  make all install CROSSCOMPILE=1??? find out?
  }

  result:=false; //fail by default

  if assigned(CrossInstaller) then
  begin
    CrossInstaller.Reset;

    {$ifdef win32}
    if (CrossInstaller.TargetCPU='x86_64') and ((CrossInstaller.TargetOS='win64') or (CrossInstaller.TargetOS='win32')) then
    begin
      if (GetNumericalVersion(GetFPCVersion)<CalculateFullVersion(2,4,2)) then
      begin
        result:=true;
        exit;
      end;
    end;
    {$endif win32}

    if CrossInstaller.TargetCPU='jvm' then DownloadJasmin;

    //pass on user-requested cross compile options
    CrossInstaller.SetCrossOpt(CrossOPT);
    CrossInstaller.SetSubArch(CrossOS_SubArch);

    // get/set cross binary utils !!
    BinsAvailable:=false;
    CrossInstaller.SearchModeUsed:=smFPCUPOnly; // default;
    if Length(CrossToolsDirectory)>0 then
    begin
      // we have a crosstools setting
      if (CrossToolsDirectory='FPCUP_AUTO')
         then CrossInstaller.SearchModeUsed:=smAuto
         else CrossInstaller.SearchModeUsed:=smManual;
    end;
    if CrossInstaller.SearchModeUsed=smManual
       then BinsAvailable:=CrossInstaller.GetBinUtils(CrossToolsDirectory)
       else BinsAvailable:=CrossInstaller.GetBinUtils(FBaseDirectory);
    if not BinsAvailable then infoln('Failed to get crossbinutils', etError);

    // get/set cross libraries !!
    LibsAvailable:=false;
    CrossInstaller.SearchModeUsed:=smFPCUPOnly;
    if Length(CrossLibraryDirectory)>0 then
    begin
      // we have a crosslibrary setting
      if (CrossLibraryDirectory='FPCUP_AUTO')
         then CrossInstaller.SearchModeUsed:=smAuto
         else CrossInstaller.SearchModeUsed:=smManual;
    end;
    if CrossInstaller.SearchModeUsed=smManual
      then LibsAvailable:=CrossInstaller.GetLibs(CrossLibraryDirectory)
      else LibsAvailable:=CrossInstaller.GetLibs(FBaseDirectory);
    if not LibsAvailable then infoln('Failed to get crosslibrary', etError);

    result:=(BinsAvailable AND LibsAvailable);

    if result then
    begin
      result:=false;

      if CrossInstaller.CompilerUsed=ctInstalled then
      begin
        infoln(infotext+'Using FPC itself to compile and build the cross-compiler',etInfo);
        ChosenCompiler:=GetCompilerInDir(FInstallDirectory);
      end
      else //ctBootstrap
      begin
        infoln(infotext+'Using the original bootstrapper to compile and build the cross-compiler',etInfo);
        ChosenCompiler:=FCompiler;
      end;

      s1:=GetCompilerVersion(ChosenCompiler);

      if s1<>'0.0.0'
        then infoln('FPC '+CrossCPU_Target+'-'+CrossOS_Target+' cross-builder: Using compiler with version: '+s1, etInfo)
        else infoln(infotext+'FPC compiler ('+ChosenCompiler+') version error: '+s1+' ! Should never happen: expect many errors !!', etError);

      // Add binutils path to path if necessary
      OldPath:=GetPath;
      try
        if CrossInstaller.BinUtilsPathInPath then
           SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),false,true);

        //Get minimum Mac OS X deployment version: 10.4, 10.5.1, ... (Darwin)
        //Get minimum iOS deployment version: 8.0, 8.0.2, ... (iphonesim)
        //Add minimum required OSX/iOS version to prevent "crti not found" errors.
        Minimum_OSX:='';
        Minimum_iOS:='';

        // Check/set OSX deployment version
        i:=StringListStartsWith(CrossInstaller.CrossOpt,'-WM');
        if i=-1 then
        begin
          s1:='';
          {$ifdef Darwin}
          s1:=GetSDKVersion('macosx');
          if CompareVersionStrings(s1,'10.8')>=0 then
          begin
            s1:='10.8';
          end;
          {$endif}
          if Length(s1)>0 then
          begin
            Minimum_OSX:='-WM'+s1;
          end;
        end else Minimum_OSX:=CrossInstaller.CrossOpt[i];

        // Check/set iOS deployment version
        i:=StringListStartsWith(CrossInstaller.CrossOpt,'-WP');
        if i=-1 then
        begin
          s1:='';
          {$ifdef Darwin}
          if (CrossInstaller.TargetCPU='aarch64') OR (CrossInstaller.TargetCPU='arm') then
          begin
            s1:=GetSDKVersion('iphoneos');
          end
          else
          begin
            s1:=GetSDKVersion('iphonesimulator');
          end;
          {$endif}
          if Length(s1)>0 then
          begin
            Minimum_iOS:='-WP'+s1;
          end;
        end else Minimum_iOS:=CrossInstaller.CrossOpt[i];

        for MakeCycle:=Low(TSTEPS) to High(TSTEPS) do
        begin

          // Modify fpc.cfg
          // always add this, to be able to detect which cross-compilers are installed
          // helpfull for later bulk-update of all cross-compilers
          FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + FPCCONFIGFILENAME;

          if (MakeCycle=Low(TSTEPS)) OR (MakeCycle=High(TSTEPS)) then
          begin

            Options:=UpperCase(CrossCPU_Target);

            //Distinguish between 32 and 64 bit powerpc
            if (UpperCase(CrossCPU_Target)='POWERPC') then
            begin
              Options:='POWERPC32';
            end;

            //Remove dedicated settings of config snippet
            if MakeCycle=Low(TSTEPS) then
            begin
              infoln(infotext+'Removing '+FPCCONFIGFILENAME+' config snippet.',etInfo);
              s1:='# dummy (blank) config just to remove dedicated settings during build of cross-compiler'+LineEnding;
            end;

            //Add config snippet
            if (MakeCycle=High(TSTEPS)) then
            begin
              infoln(infotext+'Adding '+FPCCONFIGFILENAME+' config snippet.',etInfo);
              if CrossInstaller.FPCCFGSnippet<>''
                 then s1:=CrossInstaller.FPCCFGSnippet+LineEnding
                 else s1:='# dummy (blank) config for auto-detect cross-compilers'+LineEnding;
            end;

            //Edit dedicated settings of config snippet
            InsertFPCCFGSnippet(FPCCfg,
              SnipMagicBegin+CrossCPU_target+'-'+CrossOS_Target+LineEnding+
              '# cross compile settings dependent on both target OS and target CPU'+LineEnding+
              '#IFDEF FPC_CROSSCOMPILING'+LineEnding+
              '#IFDEF '+uppercase(CrossOS_Target)+LineEnding+
              '#IFDEF CPU'+Options+LineEnding+
              '# Inserted by fpcup '+DateTimeToStr(Now)+LineEnding+
              s1+
              '#ENDIF'+LineEnding+
              '#ENDIF'+LineEnding+
              '#ENDIF'+LineEnding+
              SnipMagicEnd);
          end;

          Processor.Executable := Make;
          Processor.Parameters.Clear;
          {$IFDEF MSWINDOWS}
          if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
          {$ENDIF}
          Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
          {
          //Still not clear if jobs can be enabled for crosscompiler builds ... :-|
          if (FNoJobs) then
            Processor.Parameters.Add('--jobs=1')
          else
            Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));
          }
          Processor.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FSourceDirectory));
          Processor.Parameters.Add('FPCMAKE=' + IncludeTrailingPathDelimiter(FBinPath)+'fpcmake'+GetExeExt);
          Processor.Parameters.Add('PPUMOVE=' + IncludeTrailingPathDelimiter(FBinPath)+'ppumove'+GetExeExt);
          Processor.Parameters.Add('FPCDIR=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
          Processor.Parameters.Add('PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
          Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));

          {$IFDEF MSWINDOWS}
          if ChosenCompiler=GetCompilerInDir(FInstallDirectory) then
          begin
            if FileExists(FPCCfg) then
            begin
              //Processor.Parameters.Add('CFGFILE=' + FPCCfg);
            end;
          end;
          Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
          Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
          {$ELSE}
          Processor.Parameters.Add('INSTALL_BINDIR='+FBinPath);
          {$ENDIF}
          // Tell make where to find the target binutils if cross-compiling:
          if CrossInstaller.BinUtilsPath<>'' then
             Processor.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));

          Options:=FCompilerOptions;

          //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
          //Todo: to be investigated
          //Processor.Parameters.Add('FPCFPMAKE='+ChosenCompiler);

          {$ifdef crosssimple}
          Processor.Parameters.Add('FPC='+ChosenCompiler);
          case MakeCycle of
            st_MakeAll:
            begin
              Processor.Parameters.Add('all');
            end;
            st_MakeCrossInstall:
            begin
              Processor.Parameters.Add('crossinstall');
            end;
          end;
          {$else crosssimple}
          case MakeCycle of
            st_Compiler:
            begin
              {$ifdef Darwin}
              if Length(Minimum_OSX)>0 then Options:=Options+' '+Minimum_OSX;
              {$endif}
              Processor.Parameters.Add('FPC='+ChosenCompiler);
              Processor.Parameters.Add('compiler_cycle');
            end;
            st_CompilerInstall:
            begin
              {$if (defined(Linux)) AND (defined (CPUX86_64))}
              if FMUSL then
              begin
                // copy over the [cross-]compiler
                s1:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+GetCompilerName(CrossInstaller.TargetCPU);
                s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+GetCrossCompilerName(CrossInstaller.TargetCPU);
                if FileExists(s1) then
                begin
                  infoln(infotext+'Copy [cross-]compiler ('+ExtractFileName(s1)+') into: '+ExtractFilePath(s2),etInfo);
                  FileUtil.CopyFile(s1,s2);
                  fpChmod(s2,&755);
                end;
              end;
              {$endif}

              {$ifdef Darwin}
              if Length(Minimum_OSX)>0 then Options:=Options+' '+Minimum_OSX;
              {$endif}
              Processor.Parameters.Add('FPC='+ChosenCompiler);
              Processor.Parameters.Add('compiler_install');
            end;
            st_Rtl:
            begin
              s1:=GetCrossCompilerName(CrossInstaller.TargetCPU);
              Processor.Parameters.Add('FPC='+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1);
              Processor.Parameters.Add('rtl');
            end;
            st_RtlInstall:
            begin
              s1:=GetCrossCompilerName(CrossInstaller.TargetCPU);
              Processor.Parameters.Add('FPC='+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1);
              Processor.Parameters.Add('rtl_install');
            end;
            st_Packages:
            begin
              {$ifdef Darwin}
              if Length(Minimum_OSX)>0 then Options:=Options+' '+Minimum_OSX;
              {$endif}
              s1:=GetCrossCompilerName(CrossInstaller.TargetCPU);
              Processor.Parameters.Add('FPC='+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1);
              Processor.Parameters.Add('packages');
            end;
            st_PackagesInstall:
            begin
              s1:=GetCrossCompilerName(CrossInstaller.TargetCPU);
              Processor.Parameters.Add('FPC='+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1);
              Processor.Parameters.Add('packages_install');
            end;
            st_NativeCompiler:
            begin
              {$ifdef buildnative}
              if (
                //Only native compiler if we have libs !!
                (CrossInstaller.LibsPath<>'')
                AND
                //Only native compiler for these OS
                (AnsiIndexText(CrossInstaller.TargetOS,['win32','win64','linux','freebsd','netbsd','openbsd','darwin','haiku','qnx','beos','solaris','aix'])<>-1)
                )
                then
              begin
                infoln(infotext+'Building native compiler for '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+'.',etInfo);
                Processor.Parameters.Add('FPC='+ChosenCompiler);
                //s1:=GetCrossCompilerName(CrossInstaller.TargetCPU);
                //Processor.Parameters.Add('FPC='+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+s1);
                Processor.Parameters.Add('-C');
                Processor.Parameters.Add('compiler');
                Processor.Parameters.Add('compiler');
              end else continue;
              {$else}
              continue;
              {$endif}
            end;
          end;

          Processor.Parameters.Add('CROSSINSTALL=1');

          if (CrossInstaller.TargetCPU='jvm') then
          begin
            if (MakeCycle in [st_Packages,st_PackagesInstall,st_NativeCompiler]) then
            begin
              infoln(infotext+'Skipping build step '+GetEnumNameSimple(TypeInfo(TSTEPS),Ord(MakeCycle))+' for '+CrossInstaller.TargetCPU+'.',etInfo);
              continue;
            end;
          end;

          {$endif crosssimple}

          Processor.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
          Processor.Parameters.Add('OS_SOURCE='+GetTargetOS);
          Processor.Parameters.Add('OS_TARGET='+CrossOS_Target); //cross compile for different OS...
          Processor.Parameters.Add('CPU_TARGET='+CrossCPU_Target); // and processor.
          if Length(CrossInstaller.SubArch)>0 then Processor.Parameters.Add('SUBARCH='+CrossInstaller.SubArch);

          //Processor.Parameters.Add('OSTYPE='+CrossInstaller.TargetOS);
          Processor.Parameters.Add('NOGDBMI=1'); // prevent building of IDE to be 100% sure

          // Error checking for some known problems with cross compilers
          //todo: this really should go to the cross compiler unit itself but would require a rewrite
          if (CrossInstaller.TargetCPU='i8086') and
            (CrossInstaller.TargetOS='msdos') then
          begin
            if (pos('-g',Options)>0) then
            begin
              infoln(infotext+'Specified debugging FPC options: '+Options+'... However, this cross compiler does not support debug symbols. Aborting.',etError);
              exit(false);
            end;
          end;

          if (CrossInstaller.TargetCPU='arm') then
          begin
            // what to do ...
            // always build hardfloat for ARM ?
            // or default to softfloat for ARM ?
            // if (Pos('-dFPC_ARMEL',Options)=0) then Options:=Options+' -dFPC_ARMEL';
            // decision: (nearly) always build hardfloat ... not necessary correct however !
            s2:=' -dFPC_ARMHF';
            for ARMArch := Low(TARMARCH) to High(TARMARCH) do
            begin
              s1:=ARMArchFPCStr[ARMArch];
              if (Length(s1)>0) and (Pos(s1,Options)>0) then
              begin
                s2:='';
                break;
              end;
            end;
            if Length(s2)>0 then Options:=Options+s2;
          end;

          {$ifdef freebsd}
          if (MakeCycle in [st_Compiler,st_CompilerInstall]) then
               Options:=Options+' -dFPC_USE_LIBC';
          {$endif}

          {
          s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+REVINCFILENAME;
          if FileExists(s2) then
          begin
            Processor.Parameters.Add('REVSTR='+ActualRevision);
            Options:=Options+' -dREVINC';
          end;
          }

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

          //Still not sure if this is needed
          //To be checked
          if (CrossInstaller.TargetOS='freebsd') then
          begin
            //if NOT (MakeCycle in [st_Compiler,st_CompilerInstall]) then
            begin
              //Processor.Parameters.Add('COMPILER_OPTIONS=-dFPC_USE_LIBC');
              CrossOptions:=CrossOptions+' -dFPC_USE_LIBC';
            end;
          end;

          if (CrossInstaller.TargetCPU='mipsel') AND (CrossInstaller.TargetOS='embedded') then
          begin
            // prevents the addition of .module nomips16 pseudo-op : not all assemblers can handle this
            CrossOptions:=CrossOptions+' -a5';
          end;

          if CrossInstaller.BinUtilsPrefix<>'' then
          begin
            // Earlier, we used regular OPT; using CROSSOPT is apparently more precise
            CrossOptions:=CrossOptions+' -XP'+CrossInstaller.BinUtilsPrefix;
            Processor.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
          end;

          if CrossInstaller.LibsPath<>''then
          begin
             // https://wiki.freepascal.org/FPC_AIX_Port#Cross-compiling
             if (CrossInstaller.TargetOS='aix') then
             begin
               CrossOptions:=CrossOptions+' -XR'+ExcludeTrailingPathDelimiter(CrossInstaller.LibsPath);
             end;

             {$ifndef Darwin}
             CrossOptions:=CrossOptions+' -Xd';
             CrossOptions:=CrossOptions+' -Fl'+ExcludeTrailingPathDelimiter(CrossInstaller.LibsPath);

             if (CrossInstaller.TargetOS='darwin') then
             begin
               // add extra libs located in ...\system for Mac SDK
               // does not do harm on other systems if they are not there
               CrossOptions:=CrossOptions+' -Fl'+IncludeTrailingPathDelimiter(CrossInstaller.LibsPath)+'system';
             end;
             {$endif}

             {$ifdef Darwin}
             if (CrossInstaller.TargetOS='darwin') OR (CrossInstaller.TargetOS='iphonesim') then
             begin
               s1:=SafeExpandFileName(IncludeTrailingPathDelimiter(CrossInstaller.LibsPath)+'..'+DirectorySeparator+'..');
               CrossOptions:=CrossOptions+' -XR'+ExcludeTrailingPathDelimiter(s1);
             end
             else
             begin
               CrossOptions:=CrossOptions+' -Xd';
               CrossOptions:=CrossOptions+' -Fl'+ExcludeTrailingPathDelimiter(CrossInstaller.LibsPath);
             end;
             {$endif}

            // if we have libs ... chances are +/-100% that we have bins, so set path to include bins !
            // but only in case we did not do it before
            // not sure if this is realy needed
            //if NOT CrossInstaller.BinUtilsPathInPath then
            //   SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),true,false);
          end;

          if CrossInstaller.BinUtilsPath<>''then
          begin
             {$ifdef Darwin}
             //if (CrossInstaller.TargetOS='iphonesim') then
             begin
               CrossOptions:=CrossOptions+' -FD'+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath);
             end;
             {$else}
             //just for testing/debugging
             //Options:=Options+' -sh -s-';
             {$endif}
          end;

          {$ifdef Darwin}
          Options:=Options+' -ap';
          {$endif}

          for i:=0 to CrossInstaller.CrossOpt.Count-1 do
          begin
            CrossOptions:=trimright(CrossOptions+' '+CrossInstaller.CrossOpt[i]);
          end;

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
            Processor.Parameters.Add('CROSSOPT='+CrossOptions);
          end;

          {$if (NOT defined(FPC_HAS_TYPE_EXTENDED)) AND (defined (CPUX86_64))}
          // soft 80 bit float if available
          if FSoftFloat then
          begin
            if ( (CrossInstaller.TargetCPU='i386') OR (CrossInstaller.TargetCPU='i8086')  OR (CrossInstaller.TargetCPU='x86_64') ) then
            begin
              infoln(infotext+'Adding -dFPC_SOFT_FPUX80 compiler option to enable 80bit (soft)float support (trunk only).',etInfo);
              infoln(infotext+'This is needed due to the fact that FPC itself is also build with this option enabled.',etInfo);
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
          Processor.Parameters.Add('OPT='+s1);

          try
            if CrossOptions='' then
               infoln(infotext+'Running '+Processor.Executable+' [step # '+GetEnumNameSimple(TypeInfo(TSTEPS),Ord(MakeCycle))+'] (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')',etInfo)
            else
              infoln(infotext+'Running '+Processor.Executable+' [step # '+GetEnumNameSimple(TypeInfo(TSTEPS),Ord(MakeCycle))+'] (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+') with CROSSOPT: '+CrossOptions,etInfo);

            Processor.Execute;
            result:=(Processor.ExitStatus=0);

            if ((NOT result) AND (MakeCycle=st_Packages)) then
            begin
              //Sometimes rerun gives good results (on AIX 32bit especially).
              infoln(infotext+'Running '+Processor.Executable+' stage again ... could work !',etInfo);
              Processor.Execute;
              result:=(Processor.ExitStatus=0);
            end;

          except
            on E: Exception do
            begin
              WritelnLog(infotext+'Running cross compiler fpc '+Processor.Executable+' generated an exception!'+LineEnding+'Details: '+E.Message,true);
              WritelnLog(infotext+'We are going to try again !',true);
              exit(false);
              //result:=false;
            end;
          end;

          if (not result) then break;

          if MakeCycle=st_NativeCompiler then
             infoln(infotext+'Building native compiler for finished.',etInfo)

        end;// loop over MakeCycle

        if not(Result) then
        begin
          // Not an error but warning for optional modules: crosswin32-64 and crosswin64-32
          // These modules need to be optional because FPC 2.6.2 gives an error crosscompiling regarding fpdoc.css or something.
          {$ifdef win32}
          // if this is crosswin32-64, ignore error as it is optional
          if (CrossInstaller.TargetCPU='x86_64') and ((CrossInstaller.TargetOS='win64') or (CrossInstaller.TargetOS='win32')) then
            result:=true;
          {$endif win32}
          {$ifdef win64}
          // if this is crosswin64-32, ignore error as it is optional
          if (CrossInstaller.TargetCPU='i386') and (CrossInstaller.TargetOS='win32') then
            result:=true;
          {$endif win64}
          FCompiler:='////\\\Error trying to compile FPC\|!';
          if result then
            infoln(infotext+'Running cross compiler fpc '+Processor.Executable+' for '+GetFPCTarget(false)+' failed with an error code. Optional module; continuing regardless.', etInfo)
          else
          infoln(infotext+'Running cross compiler fpc '+Processor.Executable+' for '+GetFPCTarget(false)+' failed with an error code.',etError);
          // No use in going on, but
          // do make sure installation continues if this happened with optional crosscompiler:
          exit(result);
        end
        else
        begin
          {$IFDEF UNIX}

          {$ifdef Darwin}
          // on Darwin, the normal compiler names are used for the final cross-target compiler !!
          // very tricky !
          s1:=GetCompilerName(CrossInstaller.TargetCPU);
          {$else}
          s1:=GetCrossCompilerName(CrossInstaller.TargetCPU);
          {$endif}

          // copy over the cross-compiler towards the FPC bin-directory, with the right compilername.
          if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+CrossCompilerName) then
          begin
            infoln(infotext+'Copy cross-compiler ('+CrossCompilerName+') into: '+FBinPath,etInfo);
            FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+CrossCompilerName,
              IncludeTrailingPathDelimiter(FBinPath)+s1);
            fpChmod(IncludeTrailingPathDelimiter(FBinPath)+s1,&755);
          end;

          {$ENDIF}

          // delete cross-compiler in source-directory
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+CrossCompilerName);

          {$IFDEF UNIX}
          result:=CreateFPCScript;
          {$ENDIF UNIX}
          FCompiler:=GetCompiler;

          {$ifdef MSWINDOWS}
          CreateBinutilsList(GetCompilerVersion(ChosenCompiler));

          // get wince debugger
          if (CrossCPU_Target='arm') AND (CrossOS_Target='wince') then
          begin
            for Counter := low(FUtilFiles) to high(FUtilFiles) do
            begin
              if (FUtilFiles[Counter].Category=ucDebuggerWince) then
              begin
                if NOT FileExists(IncludeTrailingPathDelimiter(FMakeDir)+'gdb\arm-wince\gdb.exe') then
                begin
                  s1:=GetTempFileNameExt('','FPCUPTMP','zip');
                  if GetFile(FUtilFiles[Counter].RootURL + FUtilFiles[Counter].FileName,s1) then
                  begin
                    with TNormalUnzipper.Create do
                    begin
                      try
                        if DoUnZip(s1,IncludeTrailingPathDelimiter(FMakeDir)+'gdb\arm-wince\',[]) then
                          infoln(localinfotext+'Downloading and installing GDB debugger (' + FUtilFiles[Counter].FileName + ') for WinCE success.',etInfo);
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

          // move arm-embedded debugger, if any
          if (CrossCPU_Target='arm') AND (CrossOS_Target='embedded') then
          begin
            if NOT FileExists(ConcatPaths([FMakeDir,'gdb','arm-embedded'])+PathDelim+'gdb'+GetExeExt) then
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
                    s2:=IncludeTrailingPathDelimiter(FMakeDir)+'gdb'+DirectorySeparator+'arm-embedded'+DirectorySeparator;
                    ForceDirectoriesSafe(s2);
                    FileUtil.CopyFile(s1,s2+'gdb'+GetExeExt);
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

    RemoveStaleBuildDirectories(FSourceDirectory,CrossCPU_Target,CrossOS_Target);

  end
  else
  begin
    infoln(infotext+'Can''t find cross installer for '+GetFPCTarget(false)+' !!!',etError);
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

  if ((CrossCPU_Target='') OR (CrossOS_Target='')) then exit;

  //if (NOT CrossCompilerPresent) then exit;

  if assigned(CrossInstaller) AND (Length(FBaseDirectory)>0) AND (NOT CheckDirectory(FBaseDirectory)) then
  begin
    CrossInstaller.Reset;

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
            infoln(infotext+'Deleting '+ModuleName+' bin tools directory '+aDir);
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
            infoln(infotext+'Deleting '+ModuleName+' libs directory '+aDir);
            if DeleteDirectoryEx(aDir)=false then
            begin
              WritelnLog(infotext+'Error deleting '+ModuleName+' libs directory '+aDir);
            end;
          end;
        end;
      end;
    end;

    FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + FPCCONFIGFILENAME;
    InsertFPCCFGSnippet(FPCCfg,SnipMagicBegin+CrossCPU_target+'-'+CrossOS_Target);

    aDir:=IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+DirectorySeparator+GetFPCTarget(false);
    if DirectoryExists(aDir) then
    begin
      // Only allow binary directories inside our own install te be deleted
      if (Pos(FBaseDirectory,aDir)=1) then
      begin
        infoln(infotext+'Deleting '+ModuleName+' binary directory '+aDir);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' binary directory '+aDir);
        end;
      end;
    end;

    aDir:=IncludeTrailingPathDelimiter(FInstallDirectory)+'units'+DirectorySeparator+GetFPCTarget(false);
    {$ifdef UNIX}
    if FileIsSymlink(aDir) then
    begin
      try
        aDir:=GetPhysicalFilename(aDir,pfeException);
      except
      end;
    end;
    {$endif}
    if DirectoryExists(aDir) then
    begin
      // Only allow unit directories inside our own install te be deleted
      if (Pos(FBaseDirectory,aDir)=1) then
      begin
        infoln(infotext+'Deleting '+ModuleName+' unit directory '+aDir);
        if DeleteDirectoryEx(aDir)=false then
        begin
          WritelnLog(infotext+'Error deleting '+ModuleName+' unit directory '+aDir);
        end;
      end;
    end;

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
  s1,s2,s3:string;
  FPCDirStore:string;
begin
  result:=inherited;
  OperationSucceeded:=true;

  s1:=GetCompilerVersion(FCompiler);
  if s1<>'0.0.0'
    then infoln('FPC native builder: Using FPC bootstrap compiler with version: '+s1, etInfo)
    else infoln(infotext+'FPC bootstrap version error: '+s1+' ! Should never happen: expect many errors !!', etError);

  //if clean failed (due to missing compiler), try again !
  if (NOT FCleanModuleSuccess) then
  begin
    if ((ModuleName=_FPC) OR (ModuleName=_PAS2JS)) then
    begin
      infoln(infotext+'Running CleanModule once more before building FPC from sources, due to previous CleanModule failure.',etInfo);
      CleanModule(ModuleName);
    end;
  end;

  if (ModuleName=_FPC) then
  //if (false) then
  begin
    //Sometimes, during build, we get an error about missing yylex.cod and yyparse.cod.
    //Copy them now, just to be sure

    ForceDirectoriesSafe(FBinPath);
    s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'tply';
    s1:=IncludeTrailingPathDelimiter(FBinPath)+YYLEX;
    if (NOT FileExists(s1)) then FileUtil.CopyFile(s2+DirectorySeparator+YYLEX,s1);
    s1:=IncludeTrailingPathDelimiter(FBinPath)+YYPARSE;
    if (NOT FileExists(s1)) then FileUtil.CopyFile(s2+DirectorySeparator+YYPARSE,s1);

    {$IFDEF UNIX}
    s1:=IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc/'+GetFPCVersion;
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
    infoln(infotext+'Now building '+ModuleName+' revision '+ActualRevision,etInfo);
  end;

  Processor.Executable := Make;
  Processor.Parameters.Clear;
  {$IFDEF MSWINDOWS}
  if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
  {$ENDIF}
  FErrorLog.Clear;
  if (FNoJobs) then
    Processor.Parameters.Add('--jobs=1')
  else
    Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));
  //Processor.Parameters.Add('FPC='+FCompiler);
  Processor.Parameters.Add('PP='+FCompiler);

  {$IFDEF DEBUG}
  Processor.Parameters.Add('-d');
  {$ENDIF}
  Processor.Parameters.Add('FPCMAKE=' + IncludeTrailingPathDelimiter(FBinPath)+'fpcmake'+GetExeExt);
  Processor.Parameters.Add('PPUMOVE=' + IncludeTrailingPathDelimiter(FBinPath)+'ppumove'+GetExeExt);
  Processor.Parameters.Add('FPCDIR=' + ExcludeTrailingPathDelimiter(FSourceDirectory));

  //Makefile could pickup FPCDIR setting, so try to set it for fpcupdeluxe
  //FPCDirStore:=Processor.Environment.GetVar('FPCDIR');
  //Processor.Environment.SetVar('FPCDIR',IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc');

  //Prevents the Makefile to search for the (native) ppc compiler which is used to do the latest build
  //Todo: to be investigated
  //Processor.Parameters.Add('FPCFPMAKE='+ChosenCompiler);

  Processor.Parameters.Add('PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
  Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
  {$IFDEF UNIX}
  Processor.Parameters.Add('INSTALL_BINDIR='+FBinPath);
  {$ELSE}

  if (ModuleName<>_FPC) then
  begin
    s1:=IncludeTrailingPathDelimiter(FBinPath)+FPCCONFIGFILENAME;
    if FileExists(s1) then
    begin
      //Processor.Parameters.Add('CFGFILE=' + s1);
    end;
  end;

  Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  {$ENDIF}
  Processor.Parameters.Add('OS_SOURCE=' + GetTargetOS);
  Processor.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);
  Processor.Parameters.Add('OS_TARGET=' + GetTargetOS);
  Processor.Parameters.Add('CPU_TARGET=' + GetTargetCPU);

  if (GetNumericalVersion(GetFPCVersion)<CalculateFullVersion(2,4,4)) then
  begin
    Processor.Parameters.Add('DATA2INC=echo');
  end;

  if FBootstrapCompilerOverrideVersionCheck then
    Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
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
  s2:=GetSDKVersion('macosx');
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
  {$ENDIF}

  s2:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+REVINCFILENAME;
  if FileExists(s2) then
  begin
    Processor.Parameters.Add('REVSTR='+ActualRevision);
    s1:=s1+' -dREVINC';
  end;

  {$if (NOT defined(FPC_HAS_TYPE_EXTENDED)) AND (defined (CPUX86_64))}
  if FSoftFloat then
  begin
    // soft 80 bit float if available
    infoln(infotext+'Adding -dFPC_SOFT_FPUX80 compiler option to enable 80bit (soft)float support (trunk only).',etInfo);
    s1:=s1+' -dFPC_SOFT_FPUX80';
  end;
  {$endif}

  {$ifdef BSD}
    {$ifndef DARWIN}
      s1:=s1+' -Fl/usr/pkg/lib';
    {$endif}
    {$ifdef FreeBSD}
      s1:=s1+' -dFPC_USE_LIBC';
    {$endif}
  {$endif}

  s1:=Trim(s1);
  Processor.Parameters.Add('OPT='+s1);

  Processor.CurrentDirectory:='';
  case ModuleName of
    _FPC,_MAKEFILECHECKFPC:
    begin
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
    end;
    _PAS2JS:
    begin
      Processor.CurrentDirectory:=IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'pas2js';
      // first run fpcmake to generate correct makefile
      // is this still needed !!?? No !!
      //SysUtils.DeleteFile(IncludeTrailingPathDelimiter(Processor.CurrentDirectory)+'fpmake'+GetExeExt);
      //ExecuteCommandInDir(IncludeTrailingPathDelimiter(FBinPath)+'fpcmake'+GetExeExt,Processor.CurrentDirectory,FVerbose);
    end;
  end;

  if (Length(Processor.CurrentDirectory)=0) OR (NOT DirectoryExists(Processor.CurrentDirectory)) then
  begin
    Processor.Parameters.Add('--help'); // this should render make harmless
    WritelnLog(etError, infotext+'Invalid module name [' + ModuleName + '] specified! Please fix the code.', true);
    OperationSucceeded := false;
    Result := false;
    exit;
  end;

  Processor.Parameters.Add('--directory='+Processor.CurrentDirectory);

  if ModuleName=_MAKEFILECHECKFPC then
  begin
    Processor.Parameters.Add('fpc_baseinfo');
    infoln(infotext+'Running '+Processor.Executable+' fpc_baseinfo for '+ModuleName,etInfo);
  end
  else
  begin
    Processor.Parameters.Add('all');
    Processor.Parameters.Add('install');
    infoln(infotext+'Running '+Processor.Executable+' all install for '+ModuleName,etInfo);
  end;

  try
    Processor.Execute;
    //Restore FPCDIR environment variable ... could be trivial, but batter safe than sorry
    //Processor.Environment.SetVar('FPCDIR',FPCDirStore);
    if Processor.ExitStatus <> 0 then
    begin
      OperationSucceeded := False;
      WritelnLog(etError, infotext+'Error running '+Processor.Executable+' for '+ModuleName+' failed with exit code '+IntToStr(Processor.ExitStatus)+LineEnding+'. Details: '+FErrorLog.Text,true);
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
        infoln(infotext+'Creating fpc script:',etInfo)
      else
        infoln(infotext+'Creating fpc script:',etDebug);
      OperationSucceeded:=CreateFPCScript;
    end;
    {$ENDIF UNIX}

    // Let everyone know of our shiny new compiler:
    if OperationSucceeded then
    begin
      FCompiler:=GetCompiler;
      // Verify it exists
      if not(FileExists(FCompiler)) then
      begin
        WritelnLog(etError, infotext+'Could not find compiler '+FCompiler+' that should have been created.',true);
        OperationSucceeded:=false;
      end;
    end
    else
    begin
      infoln(infotext+'Error trying to compile FPC.',etDebug);
      FCompiler:='////\\\Error trying to compile FPC\|!';
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
              IncludeTrailingPathDelimiter(FBinPath)+FUtilFiles[FileCounter].FileName);
        end;
        // Also, we can change the make/binutils path to our new environment
        // Will modify fmake as well.
        FMakeDir:=FBinPath;
      except
        on E: Exception do
        begin
          writelnlog(infotext+'Error copying binutils: '+E.Message,true);
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
  infoln(infotext+'Entering ...',etDebug);
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
  Major,Minor,Build: Integer;
begin
  result:=-1;
  Major:=-1;
  Minor:=-1;
  Build:=-1;
  GetVersionFromString(aVersion,Major,Minor,Build);
  if index=0 then result:=Major;
  if index=1 then result:=Minor;
  if index=2 then result:=Build;
end;

function TFPCInstaller.GetFPCVersionFromUrl(aUrl: string): string;
var
  aVersion: string;
begin
  aVersion:=GetVersionFromUrl(aUrl);
  if aVersion='trunk' then result:=FPCTRUNKVERSION else result:=aVersion;
end;

function TFPCInstaller.GetFPCVersionFromSource(aSourcePath: string): string;
const
  VNO='version_nr';
  RNO='release_nr';
  PNO='patch_nr';
  MAKEVERSION='version=';
  //MAKEVERSION='PACKAGE_VERSION=';
var
  TxtFile:Text;
  version_nr:string;
  release_nr:string;
  build_nr:string;
  found_version_nr:boolean;
  found_release_nr:boolean;
  found_build_nr:boolean;
  s:string;
  x,y:integer;
begin
  result := '0.0.0';

  version_nr:='';
  release_nr:='';
  build_nr:='';

  found_version_nr:=false;
  found_release_nr:=false;
  found_build_nr:=false;

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
    infoln('Tried to get FPC version from version.pas, but no version.pas found',etError);
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
    end else infoln('Tried to get FPC version from '+FPCMAKEFILENAME+', but no '+FPCMAKEFILENAME+' found',etError);

  end;
end;

function TFPCInstaller.GetBootstrapCompilerVersionFromVersion(aVersion: string): string;
var
  s:string;
begin
  s:=aVersion;

  result:='0.0.0';

  if s=FPCTRUNKVERSION then result:=FPCTRUNKBOOTVERSION
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
  if GetNumericalVersion(result)<GetNumericalVersion('3.2.0') then result:='3.2.0';
  {$ENDIF}

  {$IFDEF HAIKU}
  {$IFDEF CPUX64}
  // we need at least 3.2.0 for Haiku x86_64
  //if GetNumericalVersion(result)<GetNumericalVersion('3.2.0') then result:='3.2.0';
  if GetNumericalVersion(result)<GetNumericalVersion(FPCTRUNKVERSION) then result:=FPCTRUNKVERSION;
  {$ENDIF}
  {$IFDEF CPUX32}
  // we need at least 3.0.0 for Haiku x32
  if GetNumericalVersion(result)<GetNumericalVersion('3.0.0') then result:='3.0.0';
  {$ENDIF}
  {$ENDIF}

  {$IF DEFINED(CPUPOWERPC64) AND DEFINED(FPC_ABI_ELFV2)}
  // we need at least 3.2.0 for ppc64le
  if GetNumericalVersion(result)<GetNumericalVersion('3.2.0') then result:='3.2.0';
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

  s:=IncludeTrailingPathDelimiter(aSourcePath) + FPCMAKEFILENAME;

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
        RequiredVersion:=GetNumericalVersion(s);
      end;
      x:=Pos(REQ2,s);
      if x>0 then
      begin
        Delete(s,1,x+Length(REQ2)-1);
        RequiredVersion2:=GetNumericalVersion(s);
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
    if FinalVersion<GetNumericalVersion('3.2.0') then FinalVersion:=GetNumericalVersion('3.2.0');
    {$ENDIF}

    {$IF DEFINED(CPUPOWERPC64) AND DEFINED(FPC_ABI_ELFV2)}
    // we need at least 3.2.0 for ppc64le
    if FinalVersion<GetNumericalVersion('3.2.0') then FinalVersion:=GetNumericalVersion('3.2.0');
    {$ENDIF}

    result:=InttoStr(FinalVersion DIV 10000);
    FinalVersion:=FinalVersion MOD 10000;
    result:=result+'.'+InttoStr(FinalVersion DIV 100);
    FinalVersion:=FinalVersion MOD 100;
    result:=result+'.'+InttoStr(FinalVersion);

  end else infoln('Tried to get required bootstrap compiler version from '+FPCMAKEFILENAME+', but no '+FPCMAKEFILENAME+' found',etError);
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
  FPCCompiler := IncludeTrailingPathDelimiter(FBinPath)+'fpc'+GetExeExt;

  // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
  // If this fails, Lazarus compilation will fail...
  FPCScript := IncludeTrailingPathDelimiter(ExtractFilePath(FPCCompiler)) + 'fpc.sh';
  if FileExists(FPCScript) then
  begin
    infoln(localinfotext+'fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.',etInfo);
    if not(SysUtils.DeleteFile(FPCScript)) then
    begin
      infoln(localinfotext+'Error deleting existing launcher script for FPC:'+FPCScript,etError);
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
      infoln(localinfotext+'Created launcher script for FPC:'+FPCScript,etInfo);
    end
    else
    begin
    infoln(localinfotext+'Error creating launcher script for FPC:'+FPCScript,etError);
    end;
  {$ENDIF UNIX}
end;

function TFPCInstaller.DownloadBootstrapCompiler: boolean;
// Should be done after we have unzip executable (on Windows: in FMakePath)
var
  ArchiveDir: string;
  BootstrapArchive: string;
  CompilerName:string; // File name of compiler in bootstrap archive
  ExtractedCompiler: string;
  OperationSucceeded: boolean;
begin
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (DownloadBootstrapCompiler): ';

  OperationSucceeded:=true;

  if FBootstrapCompilerURL='' then
  begin
    infoln(localinfotext+'No URL supplied. Fatal error. Should not happen !', etError);
    exit(false);
  end;

  if OperationSucceeded then
  begin
    OperationSucceeded:=ForceDirectoriesSafe(FBootstrapCompilerDirectory);
    if OperationSucceeded=false then infoln(localinfotext+'Could not create directory '+FBootstrapCompilerDirectory,etError);
  end;

  BootstrapArchive := GetTempFileNameExt('','FPCUPTMP','zip');
  if OperationSucceeded then
  begin
    OperationSucceeded:=GetFile(FBootstrapCompilerURL,BootstrapArchive,true);
    if FileExists(BootstrapArchive)=false then OperationSucceeded:=false;
  end;

  if OperationSucceeded then
  begin
    ArchiveDir := ExtractFilePath(BootstrapArchive);
    CompilerName:=ExtractFileName(FBootstrapCompiler);

    if ExtractFileExt(GetFileNameFromURL(FBootstrapCompilerURL))<>GetExeExt then
    begin
      // assume we have an archive if the file extension differs from a normal executable extension

      {$IFDEF MSWINDOWS}
      // Extract zip, "overwriting"
      with TNormalUnzipper.Create do
      begin
        try
          SysUtils.DeleteFile(ArchiveDir + CompilerName);
          OperationSucceeded:=DoUnZip(BootstrapArchive,ArchiveDir,[]);
        finally
          Free;
        end;
      end;

      {
      // Extract zip, overwriting without prompting
      if ExecuteCommand(FUnzip+' -o -d '+ArchiveDir+' '+BootstrapArchive,FVerbose) <> 0 then
      begin
        infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
        OperationSucceeded := False;
      end
      else
      begin
        OperationSucceeded := True; // Spelling it out can't hurt sometimes
      end;
      }

      // Move CompilerName to proper directory
      if OperationSucceeded = True then
      begin
        infoln(localinfotext+'Going to rename/move ' + ArchiveDir + CompilerName + ' to ' + FBootstrapCompiler, etDebug);
        SysUtils.DeleteFile(FBootstrapCompiler);
        SysUtils.RenameFile(ArchiveDir + CompilerName, FBootstrapCompiler);
        //SysUtils.DeleteFile(ArchiveDir + CompilerName);
      end else infoln('Something went wrong while extracting bootstrap compiler. This will abort further processing.',etError);;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      // Extract bz2, overwriting without prompting
      if ExecuteCommand(FBunzip2+' -d -f -q '+BootstrapArchive,FVerbose) <> 0 then
      begin
        infoln(localinfotext+'Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',etError);
        OperationSucceeded := False;
      end
      else
      begin
        ExtractedCompiler:=BootstrapArchive+'.out'; //default bzip2 output filename
        OperationSucceeded := True; // Spelling it out can't hurt sometimes
      end;
      // Move compiler to proper directory; note bzip2 will append .out to file
      if OperationSucceeded = True then
      begin
        infoln(localinfotext+'Going to move ' + ExtractedCompiler + ' to ' + FBootstrapCompiler,etDebug);
        OperationSucceeded:=MoveFile(ExtractedCompiler,FBootstrapCompiler);
      end;
      {$ENDIF LINUX}
      {$IFDEF BSD} //*BSD
      {$IFNDEF DARWIN}
      //todo: test parameters
      //Extract bz2, overwriting without prompting
      if ExecuteCommand(FBunzip2+' -d -f -q '+BootstrapArchive,FVerbose) <> 0 then
      begin
        infoln(localinfotext+'Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',etError);
        OperationSucceeded := False;
      end
      else
      begin
        ExtractedCompiler:=BootstrapArchive+'.out'; //default bzip2 output filename
        OperationSucceeded := True; // Spelling it out can't hurt sometimes
      end;
      // Move compiler to proper directory; note bzip2 will append .out to file
      if OperationSucceeded = True then
      begin
        infoln(localinfotext+'Going to move ' + ExtractedCompiler + ' to ' + FBootstrapCompiler,etDebug);
        OperationSucceeded:=MoveFile(ExtractedCompiler,FBootstrapCompiler);
      end;
      {$ELSE DARWIN}
      // Extract .tar.bz2, overwriting without prompting
      // GNU tar: -x -v -j -f
      // BSD tar:
      if ExecuteCommand(FTar+' -xf ' + BootstrapArchive + ' -C ' + ArchiveDir ,FVerbose) <> 0 then
      begin
        infoln(localinfotext+'Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',etError);
        OperationSucceeded := False;
      end
      else
      begin
        OperationSucceeded := True;
      end;
      if OperationSucceeded = True then
      begin
        infoln(localinfotext+'Going to rename/move '+CompilerName+' to '+FBootstrapCompiler,etWarning);
        Sysutils.DeleteFile(FBootstrapCompiler); //ignore errors
        // We might be moving files across partitions so we cannot use renamefile
        OperationSucceeded:=FileUtil.CopyFile(ArchiveDir + CompilerName, FBootstrapCompiler);
        Sysutils.DeleteFile(ArchiveDir + CompilerName);
      end;
      {$ENDIF DARWIN}
      {$ENDIF BSD}

    end
    else
    begin
      // no archive but a normal executable
      infoln(localinfotext+'Going to copy '+BootstrapArchive+' to '+FBootstrapCompiler,etInfo);
      SysUtils.DeleteFile(FBootstrapCompiler); //ignore errors
      OperationSucceeded:=FileUtil.CopyFile(BootstrapArchive, FBootstrapCompiler);
      SysUtils.DeleteFile(BootstrapArchive);
    end;
  end;

  {$IFNDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Make executable
    OperationSucceeded:=(fpChmod(FBootstrapCompiler, &755)=0); //rwxr-xr-x
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootstrapCompiler,eterror);
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded = True then
  begin
    SysUtils.DeleteFile(BootstrapArchive);
  end
  else
  begin
    infoln(localinfotext+'Getting/extracting bootstrap compiler failed. Archive: '+BootstrapArchive, etError);
  end;
  Result := OperationSucceeded;
end;

function TFPCInstaller.GetFPCVersion: string;
var
  testcompiler:string;
begin
  result:='0.0.0';

  testcompiler:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc1'+GetExeExt;

  if not FileExists(testcompiler) then
    testcompiler:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc'+GetExeExt;

  if not FileExists(testcompiler) then
    testcompiler:=GetCompiler;

  if FileExists(testcompiler) then
  begin
    result:=GetCompilerVersion(testcompiler);
  end
  else
  begin
    result:=GetFPCVersionFromSource(FSourceDirectory);
    if result='0.0.0' then result:=GetFPCVersionFromUrl(FURL);
  end;
end;

function TFPCInstaller.InitModule(aBootstrapVersion:string):boolean;
var
  aCompilerList:TStringList;
  i,j:integer;
  aCompilerArchive,aStandardCompilerArchive:string;
  aCompilerFound,aFPCUPCompilerFound, aLookForBetterAlternative:boolean;
  {$IFDEF FREEBSD}
  k,l,FreeBSDVersion:integer;
  {$ENDIF}
  s,s1:string;
  aLocalBootstrapVersion,aLocalFPCUPBootstrapVersion:string;
  aFPCUPBootstrapURL:string;
  aDownLoader: TBasicDownLoader;
begin
  result := true;

  if (InitDone) AND (aBootstrapVersion='') then exit;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (InitModule): ';

  FBinPath:=IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+DirectorySeparator+GetFPCTarget(true);

  result:=CheckAndGetTools;

  if FVerbose then Processor.OnOutputM:=@DumpOutput;

  WritelnLog(localinfotext+'Init:', false);
  WritelnLog(localinfotext+'FPC directory:      ' + FSourceDirectory, false);
  WritelnLog(localinfotext+'FPC URL:            ' + FURL, false);
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

    if FUseWget
       then aDownLoader:=TWGetDownLoader.Create
       else aDownLoader:=TNativeDownLoader.Create;

    try
      if NativeFPCBootstrapCompiler then
      begin
        // first, try official FPC binaries
        infoln(localinfotext+'Looking for a bootstrap compiler from official FPC bootstrap binaries.',etInfo);

        aCompilerList:=TStringList.Create;
        try
          while ((NOT aCompilerFound) AND (GetNumericalVersion(aLocalBootstrapVersion)>(FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION))) do
          begin
            infoln(localinfotext+'Looking for official FPC bootstrapper with version '+aLocalBootstrapVersion,etInfo);

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
            {$ENDIF}
            {$ENDIF}
            {$IFDEF win32}
            if aLocalBootstrapVersion='3.0.2' then aCompilerArchive:='ppc386-i386-win32.zip';
            {$endif}

            s:=FPCFTPURL+'dist/'+aLocalBootstrapVersion+'/bootstrap/';

            infoln(localinfotext+'Looking for (online) bootstrapper '+aCompilerArchive + ' in ' + s,etDebug);

            aCompilerList.Clear;

            result:=aDownLoader.getFTPFileList(s,aCompilerList);

            if (NOT result) then
            begin
              infoln(localinfotext+'Could not get compiler list from ' + s + '. Trying again.',etWarning);
              sleep(100);
              result:=aDownLoader.getFTPFileList(s,aCompilerList);
            end;

            if (NOT result) then
            begin
              infoln(localinfotext+'Could not get compiler list from ' + s + '. Final try.',etWarning);
              sleep(500);
              result:=aDownLoader.getFTPFileList(s,aCompilerList);
            end;

            if result then
            begin

              if FVerbose then
              begin
                if aCompilerList.Count>0 then infoln(localinfotext+'Found FPC v'+aLocalBootstrapVersion+' online bootstrappers: '+aCompilerList.CommaText,etDebug);
              end;

              {$IFDEF FREEBSD}
              // FreeBSD : special because of versions
              FreeBSDVersion:=-1;
              s:=GetTargetCPUOS;
              for i:=0 to Pred(aCompilerList.Count) do
              begin
                infoln(localinfotext+'Found online '+aLocalBootstrapVersion+' bootstrap compiler: '+aCompilerList[i],etDebug);
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
                infoln(localinfotext+'Got a correct bootstrap compiler from official FPC bootstrap sources',etDebug);
                break;
              end;
              {$ELSE}
              for i:=0 to Pred(aCompilerList.Count) do
              begin
                infoln(localinfotext+'Found online '+aLocalBootstrapVersion+' bootstrap compiler: '+aCompilerList[i],etDebug);
                aCompilerFound:=(aCompilerList[i]=aCompilerArchive);
                if aCompilerFound then
                begin
                  infoln(localinfotext+'Found a correct bootstrap compiler from official FPC bootstrap binaries.',etDebug);
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
            infoln(localinfotext+'Got a V'+aLocalBootstrapVersion+' bootstrap compiler from official FPC bootstrap sources.',etInfo);
            FBootstrapCompilerURL := FPCFTPURL+'dist/'+aLocalBootstrapVersion+'/bootstrap/'+aCompilerArchive;
          end;
        end;

      end;

      // go ahead with online compiler found !!
      // get compiler version (if any)
      {
      s:=GetCompilerVersion(FCompiler);
      if (s='0.0.0') AND (aCompilerFound) AND (FBootstrapCompilerURL<>'') then
      begin
        if s<>aLocalBootstrapVersion then
        begin
          result:=DownloadBootstrapCompiler;
          if result then
          begin
            FCompiler:=FBootstrapCompiler;
          end;
        end;
      end;
      }

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
            infoln(localinfotext+'Slight panic: No official FPC bootstrapper found.',etError);
            infoln(localinfotext+'Now looking for last resort bootstrap compiler from Github FPCUP(deluxe) releases.',etError);
          end;
        end
        else
        begin
          infoln(localinfotext+'Now looking for a better [version] bootstrap compiler from Github FPCUP(deluxe) releases.',etInfo);
        end;

        aFPCUPBootstrapURL:='';
        aLocalFPCUPBootstrapVersion:=aBootstrapVersion;
        aFPCUPCompilerFound:=false;

        aCompilerList:=TStringList.Create;
        try
          aCompilerList.Clear;
          try
            GetGitHubFileList(FPCUPGITREPOBOOTSTRAPPERAPI,aCompilerList,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
          except
            on E : Exception do
            begin
              infoln(localinfotext+E.ClassName+' error raised, with message : '+E.Message, etError);
            end;
          end;
          for i:=0 to Pred(aCompilerList.Count) do
          begin
            infoln(localinfotext+'Found online bootstrap compiler: '+aCompilerList[i],etDebug);
          end;

          while ((NOT aFPCUPCompilerFound) AND (GetNumericalVersion(aLocalFPCUPBootstrapVersion)>0)) do
          begin
            infoln(localinfotext+'Looking online for a FPCUP(deluxe) bootstrapper with version '+aLocalFPCUPBootstrapVersion,etInfo);

            s:=GetTargetCPU;
            {$ifdef CPUARMHF}
            s:=s+'hf';
            {$endif CPUARMHF}
            {$IF DEFINED(CPUPOWERPC64) AND DEFINED(LINUX) AND DEFINED(FPC_ABI_ELFV2)}
            s:=s+'le';
            {$ENDIF}
            s:=s+'-';
            {$ifdef LINUX}
            if FMUSL then s:=s+'musl';
            {$endif LINUX}
            {$ifdef Solaris}
            //perhaps needed for special Solaris OpenIndiana bootstrapper
            //if FSolarisOI then s:=s+'OI';
            {$endif Solaris}
            s:=s+GetTargetOS;

            for i:=0 to Pred(aCompilerList.Count) do
            begin
              aFPCUPBootstrapURL:='fpcup-'+StringReplace(aLocalFPCUPBootstrapVersion,'.','_',[rfReplaceAll])+'-'+s+'-'+GetCompilerName(GetTargetCPU);
              aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);

              {$ifdef OPENBSD}
              if (NOT aFPCUPCompilerFound) then
              begin
                // if looking for 3.0.4, also look for 3.0.5, as these have been made available by FPC itself, and are sources on fpcupdeluxe on GitHub.
                // after testing 3.0.5, it seems that these versions do not work for 100%, so disable for now.
                {
                if aLocalFPCUPBootstrapVersion='3.0.4' then
                begin
                  aFPCUPBootstrapURL:='fpcup-'+StringReplace('3.0.5','.','_',[rfReplaceAll])+'-'+s+'-'+GetCompilerName(GetTargetCPU);
                  aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);
                  // as 3.0.5 is not on the offical support list, use the override
                  if aFPCUPCompilerFound then FBootstrapCompilerOverrideVersionCheck:=true;
                end;
                }
              end;
              {$endif}

              {$ifdef FREEBSD}
              if (NOT aFPCUPCompilerFound) then
              begin
                j:=GetFreeBSDVersion;
                if j=0 then j:=DEFAULTFREEBSDVERSION; // default to FreeBSD11 when GetFreeBSDVersion does not give a result

                aFPCUPBootstrapURL:='fpcup-'+StringReplace(aLocalFPCUPBootstrapVersion,'.','_',[rfReplaceAll])+'-'+s+InttoStr(j)+'-'+GetCompilerName(GetTargetCPU);
                aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);

                if (NOT aFPCUPCompilerFound) then
                begin
                  //try other versions if available
                  for j:=13 downto 9 do
                  begin
                    aFPCUPBootstrapURL:='fpcup-'+StringReplace(aLocalFPCUPBootstrapVersion,'.','_',[rfReplaceAll])+'-'+s+InttoStr(j)+'-'+GetCompilerName(GetTargetCPU);
                    aFPCUPCompilerFound:=(Pos(aFPCUPBootstrapURL,aCompilerList[i])>0);
                    if aFPCUPCompilerFound then break;
                  end;
                end;
              end;
              {$endif}

              if aFPCUPCompilerFound then
              begin
                // also ok, but for now, use code below ... we can search on filename or url through github api.
                //aFPCUPBootstrapURL:=aCompilerList[i];
                break;
              end;
            end;

            if aFPCUPCompilerFound then
            begin
              aFPCUPBootstrapURL:=FPCUPGITREPOBOOTSTRAPPER+'/'+aFPCUPBootstrapURL;
              infoln(localinfotext+'Success: found a FPCUP(deluxe) bootstrapper with version '+aLocalFPCUPBootstrapVersion,etInfo);
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
            infoln(localinfotext+'Got a bootstrap compiler from FPCUP(deluxe) bootstrap binaries.',etInfo);
            FBootstrapCompilerURL := aFPCUPBootstrapURL;
            // set standard bootstrap compilername
            FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);
          end
          else
          begin
            if (
              ( GetNumericalVersion(aLocalFPCUPBootstrapVersion)>GetNumericalVersion(aLocalBootstrapVersion) )
              OR
              ( (GetNumericalVersion(aLocalFPCUPBootstrapVersion)=GetNumericalVersion(aLocalBootstrapVersion)) AND aLookForBetterAlternative )
              ) then
            begin
              aCompilerFound:=true;
              infoln(localinfotext+'Got a better [version] bootstrap compiler from FPCUP(deluxe) bootstrap binaries.',etInfo);
              if aLookForBetterAlternative then infoln(localinfotext+'This is important for OSX Cataline, that dislikes FPC multi-arch binaries.',etInfo);
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
      s:=GetCompilerVersion(FCompiler);

      // we did not find any suitable bootstrapper
      // check if we have a manual installed bootstrapper
      if (NOT aCompilerFound) AND (FBootstrapCompilerURL='') then
      begin
        if (s='0.0.0') then
        begin
          infoln(localinfotext+'No bootstrapper local and online. Fatal. Stopping.',etError);
          exit(false);
        end
        else
        begin
          // there is a bootstrapper available: just use it !!
          infoln(localinfotext+'No correct bootstrapper. But going to use the available one with version ' + s,etInfo);
          FBootstrapCompilerOverrideVersionCheck:=true;
          result:=true;
        end;
      end;

      if (aCompilerFound) AND (FBootstrapCompilerURL<>'') then
      begin
        // final check ... do we have the correct (as in version) compiler already ?
        infoln(localinfotext+'Check if we already have a bootstrap compiler with version '+ aLocalBootstrapVersion,etInfo);
        if s<>aLocalBootstrapVersion then
        begin
          infoln(localinfotext+'No correct bootstrapper. Going to download bootstrapper from '+ FBootstrapCompilerURL,etInfo);
          result:=DownloadBootstrapCompiler;
          // always use the newly downloaded bootstrapper !!
          if result then
          begin
            FCompiler:=FBootstrapCompiler;
            s:=GetCompilerVersion(FCompiler);
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
    FCompiler:=FBootstrapCompiler;

  WritelnLog(localinfotext+'Init:',false);
  WritelnLog(localinfotext+'Bootstrap compiler dir: '+ExtractFilePath(FCompiler),false);
  WritelnLog(localinfotext+'FPC URL:                '+FURL,false);
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
      FBinPath+PathSeparator+ {compiler for current architecture}
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
    {$ifdef Darwin}
    s1:=GetSDKVersion('macosx');
    if CompareVersionStrings(s1,'10.14')>=0 then
    begin
      s:=PathSeparator+'/Library/Developer/CommandLineTools/usr/bin';
    end;
    {$endif}
    SetPath(
      FBinPath+PathSeparator+
      FBootstrapCompilerDirectory+PathSeparator+
      ExcludeTrailingPathDelimiter(FInstallDirectory)+PathSeparator+
      IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
      IncludeTrailingPathDelimiter(FInstallDirectory)+'utils'+PathSeparator+
      ExcludeTrailingPathDelimiter(FSourceDirectory)+PathSeparator+
      IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+PathSeparator+
      IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+PathSeparator+
      // pwd is located in /bin ... the makefile needs it !!
      // tools are located in /usr/bin ... the makefile needs it !!
      '/bin'+PathSeparator+
      '/usr/bin'+
      s,
      true,false);
    {$ENDIF UNIX}
  end;
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
    Processor.Parameters.Clear;
    Processor.Parameters.Add('-h');
    try
      Processor.Execute;
      //if Processor.ExitStatus = 0 then
      begin
        if Processor.OutputStrings.Count>0 then
        begin
          aIndex:=StringListStartsWith(Processor.OutputStrings,Trim(aOption));
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
    Processor.Parameters.Add('-d');
    Processor.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FInstallDirectory));
    Processor.Parameters.Add('-o');
    Processor.Parameters.Add('' + aFile + '');
    infoln(infotext+'Creating '+ExtractFileName(aFile)+': '+Processor.Executable+' '+StringReplace(Processor.Parameters.CommaText,',',' ',[rfReplaceAll]));
    try
      Processor.Execute;
      result:=(Processor.ExitStatus=0);
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

  infoln(infotext+'Building module '+ModuleName+'...',etInfo);

  s:=IncludeTrailingPathDelimiter(FSourceDirectory) + MAKEFILENAME;
  if (NOT FileExists(s)) then
  begin
    infoln(infotext+s+' not found. Severe error. Should not happen. Aborting.',etError);
    exit(false);
  end;

  VersionSnippet:='0.0.0';
  s:=GetCompilerInDir(FInstallDirectory);
  if FileExists(s) then VersionSnippet:=GetCompilerVersion(s);
  if VersionSnippet='0.0.0' then VersionSnippet:=GetFPCVersionFromSource(FSourceDirectory);
  if VersionSnippet='0.0.0' then VersionSnippet:=GetFPCVersionFromUrl(FURL);
  if VersionSnippet<>'0.0.0' then
  begin
    FMajorVersion:=0;
    FMinorVersion:=0;
    FReleaseVersion:=0;
    GetVersionFromString(VersionSnippet,FMajorVersion,FMinorVersion,FReleaseVersion);
    if (Self is TFPCCrossInstaller) then
      s:='FPC '+CrossCPU_Target+'-'+CrossOS_Target+' cross-builder: Detected source version FPC (compiler): '
    else
      s:='FPC native builder: Detected source version FPC: ';
    infoln(s+VersionSnippet, etInfo);
  end;

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
       RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromVersion(GetFPCVersionFromSource(FSourceDirectory));
    if RequiredBootstrapVersionLow='0.0.0' then
       RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromVersion(GetFPCVersionFromUrl(FURL));

    if RequiredBootstrapVersionLow='0.0.0' then
    begin
      infoln(infotext+'Could not determine required bootstrap compiler version. Should not happen. Aborting.',etError);
      exit(false);
    end;
    if RequiredBootstrapVersionHigh='0.0.0' then
    begin
      // Only a single bootstrap version found
      infoln(infotext+'To compile this FPC, we use a compiler with version : '+RequiredBootstrapVersionLow,etInfo);
      // we always build with the highest bootstrapper, so, in this case (trick), make high = low !!
      RequiredBootstrapVersionHigh:=RequiredBootstrapVersionLow;
    end
    else
    begin
      infoln(infotext+'To compile this FPC, we need (required) a compiler with version '+RequiredBootstrapVersionLow+' or '+RequiredBootstrapVersionHigh,etInfo);
    end;

    OperationSucceeded:=false;

    if NOT FileExists(FCompiler) then
    begin
      // can we use a FPC bootstrapper that is already on the system (somewhere in the path) ?
      if NativeFPCBootstrapCompiler then
      begin
        s:=GetCompilerName(GetTargetCPU);
        s:=Which(s);
        if NOT FileExists(s) then
        begin
          s:='fpc'+GetExeExt;
          s:=Which(s);
        end;
        if FileExists(s) then
        begin
          FCompiler:=s;
        end;
      end;
    end;

    // do we already have a suitable compiler somewhere ?
    if FileExists(FCompiler) then
    begin
      OperationSucceeded:=(GetCompilerVersion(FCompiler)=RequiredBootstrapVersionLow);
      if OperationSucceeded
        then RequiredBootstrapVersion:=RequiredBootstrapVersionLow
        else
        begin
          // check if higher compiler version is available
          if (RequiredBootstrapVersionLow<>RequiredBootstrapVersionHigh) then
          begin
            OperationSucceeded:=(GetCompilerVersion(FCompiler)=RequiredBootstrapVersionHigh);
            if OperationSucceeded then RequiredBootstrapVersion:=RequiredBootstrapVersionHigh;
          end;
        end;
    end;

    if OperationSucceeded then
    begin
      infoln(infotext+'To compile this FPC, we will use the (already available) compiler with version : '+RequiredBootstrapVersion,etInfo);
    end
    else
    begin
      // get the bootstrapper, among other things (binutils)
      // start with the highest requirement ??!!
      RequiredBootstrapVersion:=RequiredBootstrapVersionHigh;

      result:=InitModule(RequiredBootstrapVersion);

      if (GetCompilerVersion(FCompiler)=RequiredBootstrapVersion)
        then infoln(infotext+'To compile this FPC, we will use a fresh compiler with version : '+RequiredBootstrapVersion,etInfo)
        else
        begin
          // check if we have a lower acceptable requirement for the bootstrapper
          if (GetCompilerVersion(FCompiler)=RequiredBootstrapVersionLow) then
          begin
            // if so, set bootstrapper to lower one !!
            RequiredBootstrapVersion:=RequiredBootstrapVersionLow;
            infoln(infotext+'To compile this FPC, we can also (and will) use (required) a fresh compiler with version : '+RequiredBootstrapVersion,etInfo);
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
    //CreateBinutilsList(GetBootstrapCompilerVersionFromSource(FSourceDirectory));
    //CreateBinutilsList(GetFPCVersionFromSource(FSourceDirectory));
    CreateBinutilsList(RequiredBootstrapVersion);
    result:=CheckAndGetNeededBinUtils;

    {$ifdef Solaris}
    //sometimes, gstrip does not exist on Solaris ... just copy it to a place where it can be found ... tricky
    if (NOT FileExists('/usr/bin/gstrip')) AND (FileExists('/usr/bin/strip')) then
    begin
      if DirectoryExists(FBinPath) then
      begin
        s:=IncludeTrailingPathDelimiter(FBinPath)+'gstrip';
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
      infoln('We have ppc386. We need ppcx64. So make it !',etInfo);
      Processor.Executable := Make;
      Processor.Parameters.Clear;
      {$IFDEF MSWINDOWS}
      if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
      {$ENDIF}
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Parameters.Add('compiler_cycle');
      if (FNoJobs) then
        Processor.Parameters.Add('--jobs=1')
      else
        Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));
      Processor.Parameters.Add('FPC='+FCompiler);
      Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('OS_SOURCE=win32');
      Processor.Parameters.Add('CPU_SOURCE=i386');
      Processor.Parameters.Add('OS_TARGET=win64');
      Processor.Parameters.Add('CPU_TARGET=x86_64');
      Processor.Parameters.Add('OPT='+STANDARDCOMPILERVERBOSITYOPTIONS);
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      infoln(infotext+'Running '+Processor.Executable+' cycle for Windows FPC64:',etInfo);
      Processor.Execute;
      if Processor.ExitStatus <> 0 then
      begin
        result := False;
        WritelnLog(etError, infotext+'Failed to build ppcx64 bootstrap compiler.');
        exit;
      end;
      // Now we can change the compiler from the i386 to the x64 compiler:
      FCompiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64.exe';
      FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\ppcx64.exe',FCompiler,[cffOverwriteFile]);
      FBootstrapCompilerOverrideVersionCheck:=True;
    end;
    {$endif win64}
    {$ifdef darwin}
    if pos('ppcuniversal',FCompiler)>0 then //need to build ppcxxx before
    begin
      infoln(infotext+'We have ppcuniversal. We need '+TargetCompilerName+'. So make it !',etInfo);
      Processor.Executable := Make;
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Parameters.Clear;
      Processor.Parameters.Add('compiler_cycle');
      if (FNoJobs) then
        Processor.Parameters.Add('--jobs=1')
      else
        Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));
      Processor.Parameters.Add('FPC='+FCompiler);
      Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('OS_SOURCE=' + GetTargetOS);
      Processor.Parameters.Add('CPU_SOURCE=' + GetTargetCPU);
      Processor.Parameters.Add('OS_TARGET=' + GetTargetOS);
      Processor.Parameters.Add('CPU_TARGET=' + GetTargetCPU);
      Processor.Parameters.Add('OPT='+STANDARDCOMPILERVERBOSITYOPTIONS);
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      infoln(infotext+'Running '+Processor.Executable+' cycle for FPC '+TargetCompilerName+' bootstrap compiler only',etInfo);
      Processor.Execute;
      if Processor.ExitStatus <> 0 then
      begin
        result := False;
        WritelnLog(etError, infotext+'Failed to build '+s+' bootstrap compiler.');
        exit;
      end;

      // copy over the fresh bootstrapper, if any
      if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName) then
      begin
        // Now we can change the compiler from the ppcuniversal to the target compiler:
        FCompiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+TargetCompilerName;
        infoln(infotext+'Copy fresh compiler ('+TargetCompilerName+') into: '+ExtractFilePath(FCompiler),etDebug);
        FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName,
          FCompiler);
        fpChmod(FCompiler,&755);
        FBootstrapCompilerOverrideVersionCheck:=True;
      end;
    end;
    {$endif darwin}
  end;//(NOT (Self is TFPCCrossInstaller))

  // Now: the real build of FPC !!!
  OperationSucceeded:=BuildModuleCustom(ModuleName);

  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
    // copy the freshly created compiler to the bin/$fpctarget directory so that
    // fpc can find it
    if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName) then
    begin
      infoln(infotext+'Copy compiler ('+TargetCompilerName+') into: '+FBinPath,etDebug);
      FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompilerName,
        IncludeTrailingPathDelimiter(FBinPath)+TargetCompilerName);
      fpChmod(IncludeTrailingPathDelimiter(FBinPath)+TargetCompilerName,&755);
    end;

    // create link 'units' below FInstallDirectory to
    // <somewhere>/lib/fpc/$fpcversion/units
    s:=IncludeTrailingPathDelimiter(FInstallDirectory)+'units';
    DeleteFile(s);
    fpSymlink(pchar(IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc/'+GetFPCVersion+'/units'),
      pchar(s));
  end;
  {$ENDIF UNIX}

  // Let everyone know of our shiny new compiler (or proxy) when NOT crosscompiling !
  if (OperationSucceeded) AND (NOT (Self is TFPCCrossInstaller)) then
  begin
    FCompiler:=GetCompiler;
    // Verify it exists
    if not(FileExists(FCompiler)) then
    begin
      WritelnLog(etError, infotext+'Could not find compiler '+FCompiler+' that should have been created.',true);
      OperationSucceeded:=false;
    end;
  end;

  // only create fpc.cfg and other configs with fpcmkcfg when NOT crosscompiling !
  if (OperationSucceeded) AND (NOT (Self is TFPCCrossInstaller)) then
  begin
    // Find out where fpcmkcfg lives
    if (OperationSucceeded) then
    begin
      FPCMkCfg:=ConcatPaths([FBinPath,FPCMAKECONFIG+GetExeExt]);
      OperationSucceeded:=CheckExecutable(FPCMkCfg,'-h',FPCMAKECONFIG);
      if (NOT OperationSucceeded) then
      begin
        infoln(infotext+'Did not find '+FPCMAKECONFIG+GetExeExt+' in '+ExtractFileDir(FPCMkCfg),etDebug);
        FPCMkCfg:=ConcatPaths([FInstallDirectory,'bin',FPCMAKECONFIG+GetExeExt]);
        OperationSucceeded:=CheckExecutable(FPCMkCfg,'-h',FPCMAKECONFIG);
        if (NOT OperationSucceeded) then
        begin
          infoln(infotext+'Did not find '+FPCMAKECONFIG+GetExeExt+' in '+ExtractFileDir(FPCMkCfg),etDebug);
        end;
      end;
      if OperationSucceeded then
      begin
        infoln(infotext+'Found valid '+FPCMAKECONFIG+GetExeExt+' executable in '+ExtractFileDir(FPCMkCfg),etInfo);
      end
      else
      begin
        infoln(infotext+'Could not find '+FPCMAKECONFIG+GetExeExt+' executable. Aborting.',etError);
        FPCMkCfg:='';
      end;
    end;

    FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + FPCCONFIGFILENAME;

    if (OperationSucceeded) then
    begin
      Processor.Executable:=FPCMkCfg;
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FInstallDirectory);

      s2:= ExtractFilePath(FPCMkCfg)+FPFILENAME+GetExeExt;
      if FileExists(s2) then
      begin
        s := IncludeTrailingPathDelimiter(FBinPath) + FPCONFIGFILENAME;
        if (NOT FileExists(s)) then
        begin
          //create fp.cfg
          //if CheckFPCMkCfgOption('-1') then
          begin
            Processor.Parameters.Clear;
            Processor.Parameters.Add('-1');
            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('fpctargetos='+GetTargetOS);

            {$IFDEF UNIX}
            //s2:=GetStartupObjects;
            //if Length(s2)>0 then
            //begin
            //  Processor.Parameters.Add('-d');
            //  Processor.Parameters.Add('GCCLIBPATH= -Fl'+s2);
            //end;
            {$ENDIF UNIX}

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := IncludeTrailingPathDelimiter(FBinPath) + FPINIFILENAME;
        if (NOT FileExists(s)) then
        begin
          //create fp.ini
          //if CheckFPCMkCfgOption('-2') then
          begin
            Processor.Parameters.Clear;
            Processor.Parameters.Add('-2');

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
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
            Processor.Parameters.Clear;
            Processor.Parameters.Add('-3');

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('LocalRepository='+ConcatPaths([FBaseDirectory,PACKAGESLOCATION])+PathDelim);

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('CompilerConfigDir='+IncludeTrailingPathDelimiter(s2));

            Processor.Parameters.Add('-d');
            {$ifdef MSWINDOWS}
            Processor.Parameters.Add('GlobalPath='+IncludeTrailingPathDelimiter(FInstallDirectory));
            {$ELSE}
            Processor.Parameters.Add('GlobalPath='+ConcatPaths([FInstallDirectory,'lib','fpc','{CompilerVersion}'])+PathDelim);
            {$ENDIF}

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('GlobalPrefix='+ExcludeTrailingPathDelimiter(FInstallDirectory));

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('UserPathSuffix=users');

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;

        s := IncludeTrailingPathDelimiter(s2)+FPCPKGCOMPILERTEMPLATE;
        if (NOT FileExists(s)) then
        begin
          ForceDirectoriesSafe(s2);
          //Create default compiler template
          //if CheckFPCMkCfgOption('-4') then
          begin
            Processor.Parameters.Clear;
            Processor.Parameters.Add('-4');

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('GlobalPrefix='+ExcludeTrailingPathDelimiter(FInstallDirectory));

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('fpcbin='+FCompiler);

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('fpctargetos='+GetTargetOS);

            Processor.Parameters.Add('-d');
            Processor.Parameters.Add('fpctargetcpu='+GetTargetCPU);

            RunFPCMkCfgOption(s);
          end;
        end
        else
        begin
          infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
        end;
      end;


      s := FPCCfg;
      if (NOT FileExists(s)) then
      begin
        //create fpc.cfg
        Processor.Parameters.Clear;
        RunFPCMkCfgOption(s);
      end
      else
      begin
        infoln(infotext+'Found existing '+ExtractFileName(s)+' in '+ExtractFileDir(s)+'. Not touching it !');
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
        y:=ConfigText.Count;

        // cleanup previous fpcup settings
        repeat
          x:=-1;
          if x=-1 then
          begin
            s:='# fpcup:';
            x:=ConfigText.IndexOf(s);
          end;
          if x=-1 then
          begin
            s:='# Fpcup[deluxe]:';
            x:=ConfigText.IndexOf(s);
          end;
          if x=-1 then
          begin
            s:=SnipMagicBegin+FPCUPMAGIC;
            x:=ConfigText.IndexOf(s);
          end;

          if x<>-1 then
          begin
            // save position
            y:=x;

            // delete previous settings by fpcup[deluxe] by looking for some magic ... ;-)
            ConfigText.Delete(x);
            while (x<ConfigText.Count) do
            begin
              if (Length(ConfigText.Strings[x])>0) AND (ConfigText.Strings[x]<>SnipMagicEnd) AND (Pos(SnipMagicBegin,ConfigText.Strings[x])=0) then
                ConfigText.Delete(x)
              else
                break;
            end;
            // remove endmagic if any
            if (ConfigText.Strings[x]=SnipMagicEnd) then ConfigText.Delete(x);
            // remove empty lines if any
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
        PlainBinPath:=SafeExpandFileName(SafeExpandFileName(IncludeTrailingPathDelimiter(FBinPath)+'..'+DirectorySeparator+'..'));
        s:='-FD'+IncludeTrailingPathDelimiter(FBinPath)+';'+IncludeTrailingPathDelimiter(PlainBinPath);
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

        {$ifdef freebsd}
        ConfigText.Append('-dFPC_USE_LIBC');
        {$endif}

        {$IF (defined(NetBSD)) and (not defined(Darwin))}
        {$ifndef FPCONLY}
        ConfigText.Append('-k"-rpath=/usr/X11R6/lib"');
        ConfigText.Append('-k"-rpath=/usr/X11R7/lib"');
        {$endif}
        ConfigText.Append('-k"-rpath=/usr/pkg/lib"');
        {$endif}

        ConfigText.Append('#ENDIF');

        {$ifdef solaris}
        {$IF defined(CPUX64) OR defined(CPUX86)}
        //Intel only. See: https://wiki.lazarus.freepascal.org/Lazarus_on_Solaris#A_note_on_gld_.28Intel_architecture_only.29
        ConfigText.Append('-Xn');
        {$endif}
        {$endif}


        {$ENDIF UNIX}

        {$ifdef Darwin}
        s:=GetSDKVersion('macosx');
        if Length(s)>0 then
        begin
          ConfigText.Append('# Add minimum required OSX version for native compiling');
          ConfigText.Append('# Prevents crti not found linking errors');
          //ConfigText.Append('#IFNDEF FPC_CROSSCOMPILING');
          ConfigText.Append('#IFDEF DARWIN');
          if CompareVersionStrings(s,'10.8')>=0 then
            ConfigText.Append('-WM10.8')
          else
            ConfigText.Append('-WM'+s);
          {
          if CompareVersionStrings(s,'10.14')>=0 then
          begin
            ConfigText.Append('# MacOS 10.14 Mojave and newer have libs and tools in new, yet non-standard directory');
            ConfigText.Append('-FD/Library/Developer/CommandLineTools/usr/bin');
            ConfigText.Append('#ifdef cpui386');
            ConfigText.Append('-Fl/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib');
            ConfigText.Append('#endif');
            ConfigText.Append('#ifndef cpui386');
            ConfigText.Append('#ifndef cpupowerpc');
            ConfigText.Append('#ifndef cpupowerpc64');
            ConfigText.Append('-XR/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk');
            ConfigText.Append('#endif');
            ConfigText.Append('#endif');
            ConfigText.Append('#endif');
            //ConfigText.Append('-FD/Library/Developer/CommandLineTools/usr/bin');
            //ConfigText.Append('-XR/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk');
          end;
          }
          ConfigText.Append('#ENDIF');
        end;
        {$endif Darwin}


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
        {$else Darwin}
        {$ifdef Unix}
        ConfigText.Append('-k"-rpath=./"');
        //ConfigText.Append('-k"-rpath=/usr/local/lib"');
        ConfigText.Append('-k"-rpath=\\$$$$$\\ORIGIN"');
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

    end;

    // do not build pas2js [yet]: separate install ... use the module with rtl
    // if OperationSucceeded then BuildModuleCustom('PAS2JS');
  end;

  RemoveStaleBuildDirectories(FSourceDirectory,GetTargetCPU,GetTargetOS);

  if OperationSucceeded then
  begin
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
  oldlog: TErrorMethod;
  CrossCompiling: boolean;
  FileCounter:word;
  DeleteList: TStringList;
  CPUOS_Signature:string;
  aCleanupCompiler:string;
begin
  result:=inherited;

  // if no sources, then exit;
  if result then exit;

  result:=InitModule;

  if not result then exit;

  oldlog:=Processor.OnErrorM; //current error handler, if any

  CrossCompiling:=(Self is TFPCCrossInstaller);

  if CrossCompiling then
  begin
    CPUOS_Signature:=GetFPCTarget(false);
    // Delete any existing buildstamp file
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'build-stamp.'+CPUOS_Signature);
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'base.build-stamp.'+CPUOS_Signature);
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

  if FileExists(FCompiler)
     then aCleanupCompiler:=FCompiler
     else aCleanupCompiler:=IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);

  if FileExists(aCleanupCompiler) then
  begin

    Processor.OnErrorM:=nil;  //don't want to log errors in distclean
    try
      Processor.Executable := Make;
      Processor.Parameters.Clear;
      {$IFDEF MSWINDOWS}
      if Length(Shell)>0 then Processor.Parameters.Add('SHELL='+Shell);
      {$ENDIF}
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      if (FNoJobs) then
        Processor.Parameters.Add('--jobs=1')
      else
        Processor.Parameters.Add('--jobs='+IntToStr(FCPUCount));
      Processor.Parameters.Add('FPC='+aCleanupCompiler);
      Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('FPCMAKE=' + IncludeTrailingPathDelimiter(FBinPath)+'fpcmake'+GetExeExt);
      Processor.Parameters.Add('PPUMOVE=' + IncludeTrailingPathDelimiter(FBinPath)+'ppumove'+GetExeExt);
      Processor.Parameters.Add('FPCDIR=' + ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
      Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
      {$IFDEF UNIX}
      Processor.Parameters.Add('INSTALL_BINDIR='+FBinPath);
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Processor.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
      Processor.Parameters.Add('OS_SOURCE='+GetTargetOS);
      {$ENDIF}
      if Self is TFPCCrossInstaller then
      begin  // clean out the correct compiler
        Processor.Parameters.Add('OS_TARGET='+CrossOS_Target);
        Processor.Parameters.Add('CPU_TARGET='+CrossCPU_Target);
        //if Length(CrossInstaller.SubArch)>0 then Processor.Parameters.Add('SUBARCH='+CrossInstaller.SubArch);
        if Length(CrossOS_SubArch)>0 then Processor.Parameters.Add('SUBARCH='+CrossOS_SubArch);
      end
      else
      begin
        Processor.Parameters.Add('CPU_TARGET='+GetTargetCPU);
        Processor.Parameters.Add('OS_TARGET='+GetTargetOS);
      end;
      Processor.Parameters.Add('distclean');

      if (NOT CrossCompiling) then
      begin
        infoln(infotext+'Running '+Processor.Executable+' distclean twice',etInfo);
      end
      else
      begin
        infoln(infotext+'Running '+Processor.Executable+' distclean twice (OS_TARGET='+CrossOS_Target+'/CPU_TARGET='+CrossCPU_Target+')',etInfo);
      end;
      try
        writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
        Processor.Execute;
        result:=(Processor.ExitStatus=0);
        if result then
        begin
          Sleep(100); //now do it again
          writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
          Processor.Execute;
          result:=(Processor.ExitStatus=0);
       end;
      except
        on E: Exception do
        begin
          result:=false;
          WritelnLog(etError, infotext+'Running '+Processor.Executable+' distclean failed with an exception!'+LineEnding+'Details: '+E.Message,true);
        end;
      end;
    finally
      Processor.OnErrorM:=oldlog; //restore previous logging
    end;

  end
  else
  begin
    result:=false;
    infoln(infotext+'Running '+Processor.Executable+' distclean failed: could not find cleanup compiler. Will try again later',etInfo);
  end;

  if (NOT CrossCompiling) then
  begin
    infoln(infotext+'Deleting some FPC package config files.', etInfo);
    //DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+PACKAGESCONFIGDIR+DirectorySeparator+FPCPKGFILENAME);
    DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+PACKAGESCONFIGDIR+DirectorySeparator+FPCPKGCOMPILERTEMPLATE);
    {$IFDEF UNIX}
    // Delete any fpc.sh shell scripts
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+DirectorySeparator+CPUOS_Signature+DirectorySeparator+'fpc.sh');
    {$ENDIF UNIX}
  end;

  {$IFDEF UNIX}
  // Delete units
  // Alf: does this work and is it still needed: todo check
  DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'units');
  DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'lib/fpc/'+GetFPCVersion+'/units');
  {$ENDIF UNIX}

  {$IFDEF MSWINDOWS}
  // delete the units directory !!
  // this is needed due to the fact that make distclean will not cleanout this units directory
  // make distclean will only remove the results of a make, not a make install
  DeleteDirectoryEx(IncludeTrailingPathDelimiter(FSourceDirectory)+'units'+DirectorySeparator+CPUOS_Signature);
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

    DeleteList.Clear;

    // delete stray executables, if any !!
    FindAllFiles(DeleteList,IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler', '*'+GetExeExt, False);
    if (NOT CrossCompiling) then
    begin
      FindAllFiles(DeleteList,IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'utils', '*'+GetExeExt, False);
      FindAllFiles(DeleteList,IncludeTrailingPathDelimiter(FSourceDirectory)+'utils', '*'+GetExeExt, True);
    end;
    if DeleteList.Count > 0 then
    begin
      for FileCounter := 0 to (DeleteList.Count-1) do
      begin
        if IsExecutable(DeleteList.Strings[FileCounter]) then
        begin
          if Pos(MAKEFILENAME,DeleteList.Strings[FileCounter])=0 then
          begin
            infoln(infotext+'Deleting [stray] executable: '+DeleteList.Strings[FileCounter],etInfo);
            DeleteFile(DeleteList.Strings[FileCounter]);
          end;
        end;
      end;
    end;

  finally
    DeleteList.Free;
  end;
  // store result !
  FCleanModuleSuccess:=result;
  // do not fail !
  result:=true;
end;

function TFPCInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=true;
end;

function TFPCInstaller.GetModule(ModuleName: string): boolean;
var
  UpdateWarnings: TStringList;
  aRepoClient:TRepoClient;
  VersionSnippet:string;
begin
  result:=inherited;
  result:=InitModule;

  if not result then exit;

  aRepoClient:=GetSuitableRepoClient;

  if aRepoClient=nil then
  begin
    infoln(infotext+'Using FTP for download of ' + ModuleName + ' sources.',etWarning);
    result:=DownloadFromFTP(ModuleName);
    if result then CreateRevision(ModuleName,'unknown');
  end
  else
  begin
    infoln(infotext+'Start checkout/update of ' + ModuleName + ' sources.',etInfo);

    UpdateWarnings:=TStringList.Create;
    try
      if aRepoClient=FGitClient
         then result:=DownloadFromGit(ModuleName,FPreviousRevision, FActualRevision, UpdateWarnings)
         else result:=DownloadFromSVN(ModuleName,FPreviousRevision, FActualRevision, UpdateWarnings);
      if UpdateWarnings.Count>0 then
      begin
        WritelnLog(UpdateWarnings.Text);
      end;
    finally
      UpdateWarnings.Free;
    end;

    if NOT aRepoClient.ExportOnly then
    begin
      if FRepositoryUpdated then
      begin
        infoln(infotext+ModuleName + ' was at revision: '+PreviousRevision,etInfo);
        infoln(infotext+ModuleName + ' is now at revision: '+ActualRevision,etInfo);
      end
      else
      begin
        infoln(infotext+ModuleName + ' is at revision: '+ActualRevision,etInfo);
        infoln(infotext+'No updates for ' + ModuleName + ' found.',etInfo);
      end;
    end;

    if (NOT Result) then
      infoln(infotext+'Checkout/update of ' + ModuleName + ' sources failure.',etError);
  end;

  if result then CreateRevision(ModuleName,ActualRevision);

  if result then
  begin
    VersionSnippet:=GetFPCVersionFromSource(FSourceDirectory);
    if VersionSnippet='0.0.0' then GetFPCVersionFromURL(FURL);
    if VersionSnippet<>'0.0.0' then
    begin
      FMajorVersion:=0;
      FMinorVersion:=0;
      FReleaseVersion:=0;
      GetVersionFromString(VersionSnippet,FMajorVersion,FMinorVersion,FReleaseVersion);
    end
    else
    begin
      infoln(infotext+'Could not get version of ' + ModuleName + ' sources. Expect severe errors.',etError);
    end;
    PatchModule(ModuleName);
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
  inherited create;

  FCompiler := '';
  FSVNDirectory := '';
  FMakeDir :='';

  FTargetCompilerName:=GetCompilerName(GetTargetCPU);

  InitDone:=false;
end;

destructor TFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

