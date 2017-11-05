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

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller, processutils;

Const
  Sequences=
// convention: FPC sequences start with 'FPC'.
//standard fpc build
    'Declare FPC;'+
    'Cleanmodule FPC;'+
    // Create the link early so invalid previous
    // versions are overwritten:
    'Exec CreateFpcupScript;'+
    'Checkmodule FPC;'+
    'Getmodule FPC;'+
    'Buildmodule FPC;'+
    'End;'+

    'Declare FPCscripttest;'+
    'Exec CreateFpcupScript;'+
    'End;'+

//standard uninstall
    'Declare FPCuninstall;'+
    'Uninstallmodule FPC;'+
    'End;'+

    {$ifdef MSWINDOWS}
    // Crosscompile build
    'Declare FPCCrossWin32-64;'+
    // Needs to be run after regular compile because of CPU/OS switch
    'SetCPU x86_64;'+
    'SetOS win64;'+
    // Getmodule has already been done
    'Cleanmodule fpc;'+
    'Buildmodule fpc;'+
    'End;'+
    {$endif}


    //selective actions triggered with --only=SequenceName
    'Declare FPCCheckOnly;'+'Checkmodule FPC;'+'End;'+
    'Declare FPCCleanOnly;'+'Cleanmodule FPC;'+'End;'+
    'Declare FPCGetOnly;'+'Getmodule FPC;'+'End;'+
    'Declare FPCBuildOnly;'+'Buildmodule FPC;'+'End;'+

    //standard clean
    'Declare FPCclean;'+
    'Cleanmodule FPC;'+
    'End;'+

    'Declare FPCCleanAndBuildOnly;'+
    'Cleanmodule FPC;'+
    'Buildmodule FPC;'+
    'End';


type
  { TFPCInstaller }

  TFPCInstaller = class(TInstaller)
  private
    FBinPath: string; // path where generated compiler lives
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
    FBootstrapCompilerOverrideVersionCheck: boolean; //Indicate to make we really want to compile with this version (e.g. trunk compiler), even if it is not the latest stable version
    InitDone: boolean;
    function GetCompilerVersionNumber(aVersion: string; const index:byte=0): integer;
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
    // Retrieves compiler version string
    function GetCompilerVersion(CompilerPath: string): string;
    function GetCompilerVersionFromUrl(aUrl: string): string;
    function GetCompilerVersionFromSource(aSourcePath: string): string;
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
    //Optional; URL from which to download bootstrap FPC compiler if it doesn't exist yet.
    property BootstrapCompilerURL: string write FBootstrapCompilerURL;
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
    // Uninstall module
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
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  fpcuputil,
  repoclient,
  FileUtil, LazFileUtils
  {$IFDEF UNIX}
    ,baseunix
  {$ENDIF UNIX}
  {$IFDEF BSD}
    ,math
  {$ENDIF}
  ;

function InsertFPCCFGSnippet(FPCCFG,Snippet: string): boolean;
// Adds snippet to fpc.cfg file or replaces if if first line of snippet is present
// Returns success (snippet inserted or added) or failure
var
  ConfigText: TStringList;
  i:integer;
  SnipBegin,SnipEnd,SnipEndLastResort: integer;
  SnippetText: TStringList;
begin
  result:=false;
  SnipBegin:=-1;
  SnipEnd:=maxint;
  SnipEndLastResort:=SnipEnd;
  ConfigText:=TStringList.Create;
  SnippetText:=TStringList.Create;
  try
    SnippetText.Text:=Snippet;
    ConfigText.LoadFromFile(FPCCFG);
    // Look for exactly this string:
    SnipBegin:=ConfigText.IndexOf(SnippetText.Strings[0]);
    if SnipBegin>-1 then
    begin
      infoln('FPCCrossInstaller (InsertFPCCFGSnippet: fpc.cfg): Found existing snippet in '+FPCCFG+'. Deleting it and writing new version.',etInfo);
      for i:=(SnipBegin+1) to ConfigText.Count-1 do
      begin
        // Once again, look exactly for this text:
        if ConfigText.Strings[i]=SnipMagicEnd then
        begin
          SnipEnd:=i;
          break;
        end;
        // in case of failure, store beginning of next (magic) config segment
        if Pos(SnipMagicBegin,ConfigText.Strings[i])>0 then
        begin
          SnipEndLastResort:=i;
        end;
      end;
      if SnipEnd=maxint then
      begin
        //apparently snippet was not closed
        infoln('FPCCrossInstaller (InsertFPCCFGSnippet: fpc.cfg): Existing snippet was not closed. Replacing it anyway. Please check your fpc.cfg.',etWarning);
        if SnipEndLastResort<>maxint then
          SnipEnd:=(SnipEndLastResort-1)
        else
          SnipEnd:=i;
      end;
      for i:=SnipEnd downto SnipBegin do
      begin
        ConfigText.Delete(i);
      end;
    end;
    // Add snippet at bottom after blank line
    if ConfigText[ConfigText.Count-1]<>'' then
      ConfigText.Add(LineEnding);
    ConfigText.Add(Snippet);

    //{$ifndef Darwin}
    {$ifdef MSWINDOWS}
    // remove pipeline assembling for Darwin when cross-compiling !!
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

    ConfigText.SaveToFile(FPCCFG);

    result:=true;
  finally
    ConfigText.Free;
    SnippetText.Free;
  end;
end;

// remove stale compiled files
procedure RemoveStaleBuildDirectories(aBaseDir,aCPU,aOS:string);
var
  OldPath:string;
  FileInfo: TSearchRec;
  DeleteList:TStringList;
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
  if FindFirstUTF8(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
    until FindNextUTF8(FileInfo)<>0;
    FindCloseUTF8(FileInfo);
  end;

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'utils'+DirectorySeparator;
  if FindFirstUTF8(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
    until FindNextUTF8(FileInfo)<>0;
    FindCloseUTF8(FileInfo);
  end;

  // for (very) old versions of FPC : fcl and fv directories
  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'fcl'+DirectorySeparator;
  if FindFirstUTF8(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
    until FindNextUTF8(FileInfo)<>0;
    FindCloseUTF8(FileInfo);
  end;
  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'fv'+DirectorySeparator;
  if FindFirstUTF8(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
    until FindNextUTF8(FileInfo)<>0;
    FindCloseUTF8(FileInfo);
  end;

  OldPath:=IncludeTrailingPathDelimiter(aBaseDir)+'compiler'+DirectorySeparator;
  if FindFirstUTF8(OldPath+'*',faDirectory{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
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
    until FindNextUTF8(FileInfo)<>0;
    FindCloseUTF8(FileInfo);
  end;

end;

{ TFPCCrossInstaller }

function TFPCCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
// Runs make/make install for cross compiler.
// Error out on problems; unless module considered optional, i.e. in
// crosswin32-64 and crosswin64-32 steps.
var
  FPCCfg:String; //path+filename of the fpc.cfg configuration file
  CrossInstaller:TCrossInstaller;
  CrossOptions:String;
  ChosenCompiler:String; //Compiler to be used for cross compiling
  IntermediateCompiler:string;
  i:integer;
  OldPath:String;
  Options:String;
  s:string;
  LibsAvailable,BinsAvailable:boolean;

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
  platform; see posting Jonas Maebe http://lists.freepascal.org/lists/fpc-pascal/2011-August/030084.html
  make all install CROSSCOMPILE=1??? find out?
  }

  IntermediateCompiler:='intermediate_'+GetCompilerName(GetTargetCPU);

  result:=false; //fail by default

  CrossInstaller:=GetCrossInstaller;

  if assigned(CrossInstaller) then
  begin

    {$ifdef win32}
    if (CrossInstaller.TargetCPU='x86_64') and ((CrossInstaller.TargetOS='win64') or (CrossInstaller.TargetOS='win32')) then
    begin
      if (GetNumericalVersion(GetFPCVersion)<(2*10000+4*100+2)) then
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
        ChosenCompiler:=IncludeTrailingPathDelimiter(FBinPath)+'fpc'+GetExeExt {todo if this does not work use ppc386.exe etc}
      end
      else //ctBootstrap
      begin
        infoln(infotext+'Using the original bootstrapper to compile and build the cross-compiler',etInfo);
        if FileExists(ExtractFilePath(FCompiler)+IntermediateCompiler)
           then ChosenCompiler:=ExtractFilePath(FCompiler)+IntermediateCompiler
           else ChosenCompiler:=FCompiler;
      end;

      // Add binutils path to path if necessary
      OldPath:=GetPath;
      try
        if CrossInstaller.BinUtilsPathInPath then
           SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),false,true);

        Processor.Executable := Make;
        Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
        Processor.Parameters.Clear;
        if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
        Processor.Parameters.Add('FPC='+ChosenCompiler);
        Processor.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FSourceDirectory));
        Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath<>'' then
           Processor.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
        {$IFDEF MSWINDOWS}
        Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
        Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
        {$ENDIF}

        // will not happen often but if the compiler version is too low, add override
        if (CompareVersionStrings(GetCompilerVersion(ChosenCompiler),GetBootstrapCompilerVersionFromSource(FSourceDirectory,True))<0) then
        begin
          infoln(infotext+'OVERRIDEVERSIONCHECK needed for building of cross-compiler. Very strange.',etError);
          infoln(infotext+'The building process will continue, but results may be unexpected.',etError);
          Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
        end;

        //putting all before target might help!?!?
        if (CrossInstaller.TargetCPU='mipsel') AND (CrossInstaller.TargetOS='embedded') then
        begin
          //This builds only the compiler and the RTL
          Processor.Parameters.Add('crossall');
        end
        else
        begin
          Processor.Parameters.Add('all');
        end;

        // future improvement:
        // 1: Make compiler_cycle CROSSINSTALL=1
        // 2: Make rtl packages CROSSINSTALL=1
        // 3: Make rtl_install packages_install CROSSINSTALL=1
        // if cross-compiler is already present, this could reduce some build-time

        Processor.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
        Processor.Parameters.Add('OS_SOURCE='+GetTargetOS);
        Processor.Parameters.Add('OS_TARGET='+FCrossOS_Target); //cross compile for different OS...
        Processor.Parameters.Add('CPU_TARGET='+FCrossCPU_Target); // and processor.
        //Processor.Parameters.Add('OSTYPE='+CrossInstaller.TargetOS);
        Processor.Parameters.Add('NOGDBMI=1'); // prevent building of IDE to be 100% sure

        if Length(FCrossOS_SubArch)>0 then Processor.Parameters.Add('SUBARCH='+FCrossOS_SubArch);
        Options:=FCompilerOptions;

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
          if (Pos('-dFPC_ARMHF',Options)=0) AND (Pos('-dFPC_ARMEL',Options)=0) then Options:=Options+' -dFPC_ARMHF';
        end;

        {$ifdef solaris}
        Options:=Options+' -Xn';
        {$endif}

        CrossOptions:='';

        if CrossInstaller.BinUtilsPrefix<>'' then
        begin
          // Earlier, we used regular OPT; using CROSSOPT is apparently more precise
          CrossOptions:=CrossOptions+' -XP'+CrossInstaller.BinUtilsPrefix;
          Processor.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
        end;

        if CrossInstaller.LibsPath<>''then
        begin
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
           if (CrossInstaller.TargetOS='macos') OR (CrossInstaller.TargetOS='darwin') OR (CrossInstaller.TargetOS='iphonesim') then
           begin
             s:=ResolveDots(IncludeTrailingPathDelimiter(CrossInstaller.LibsPath)+'../../');
             CrossOptions:=CrossOptions+' -XR'+ExcludeTrailingPathDelimiter(s);
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
          if NOT CrossInstaller.BinUtilsPathInPath then
             SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),true,false);
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

        for i:=0 to CrossInstaller.CrossOpt.Count-1 do
        begin
          CrossOptions:=trimright(CrossOptions+' '+CrossInstaller.CrossOpt[i]);
        end;

        CrossOptions:=Trim(CrossOptions);
        if CrossOptions<>'' then
        begin
          Processor.Parameters.Add('CROSSOPT='+CrossOptions);
        end;

        {$ifdef Darwin}
        Options:=Options+' -ap';
        {$endif}

        {$if not defined(FPC_HAS_TYPE_EXTENDED)}
        (*
        // soft 80 bit float if available
        if (CrossInstaller.TargetCPU='i386') OR ((CrossInstaller.TargetCPU='i8086')) then
        begin
          infoln(infotext+'Adding -dFPC_SOFT_FPUX80 compiler option to enable 80bit (soft)float support.',etInfo);
          Options:=Options+' -dFPC_SOFT_FPUX80';
          // needed at this moment ... the makefile is yet to be adjusted
          Options:=Options+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\systems';
          Options:=Options+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'rtl\inc';
          Options:=Options+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler';
        end;
        *)
        {$endif}

        Options:=StringReplace(Options,'  ',' ',[rfReplaceAll]);
        Options:=Trim(Options);

        s:=STANDARDCOMPILEROPTIONS+' '+Options;
        {$ifdef DEBUG}
        //s:=s+' -g -gl -dEXTDEBUG'; //-va+
        //s:=s+' -dEXTDEBUG'; //-va+
        {$endif}
        Processor.Parameters.Add('OPT='+s);

        try
          if CrossOptions='' then
             infoln(infotext+'Running Make all (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')',etInfo)
          else
            infoln(infotext+'Running Make all (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+') with CROSSOPT: '+CrossOptions,etInfo);
          Processor.Execute;
          result:=(Processor.ExitStatus=0);
        except
          on E: Exception do
          begin
            WritelnLog(infotext+'Running cross compiler fpc make all failed with an exception!'+LineEnding+
              'Details: '+E.Message,true);
            exit(false);
          end;
        end;

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
          infoln(infotext+'Running cross compiler fpc make all for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed with an error code. Optional module; continuing regardless.', etInfo)
        else
          infoln(infotext+'Running cross compiler fpc make all for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed with an error code.',etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(result);
      end
      else
      begin
        {$IFDEF UNIX}
        // fpc bug ?!!
        // fpcupdeluxe bug !!?
        // see infoln for problem description
        s:=GetCompilerName(CrossInstaller.TargetCPU);
        if (FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+s)) AND (s=GetCompilerName(GetTargetCPU)) then
        begin
          infoln(infotext+'Non-native cross-compiler has same same as native compiler ... delete non-native cross-compiler to prevent overwriting of native compiler !!',etInfo);
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+s);
        end;
        {$ENDIF}
        // Install crosscompiler: make crossinstall
        // (apparently equivalent to make install CROSSINSTALL=1)
        Processor.Executable := Make;
        Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
        Processor.Parameters.Clear;
        infoln(infotext+'Running Make crossinstall (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')', etinfo);
        if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
        Processor.Parameters.Add('FPC='+ChosenCompiler);
        Processor.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FSourceDirectory));
        Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
        {$IFDEF UNIX}
        Processor.Parameters.Add('INSTALL_BINDIR='+FBinPath);
        {$ENDIF UNIX}
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath<>'' then
          Processor.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
        {$IFDEF MSWINDOWS}
        Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
        Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
        {$ENDIF}
        //putting crossinstall before target might help!?!?
        Processor.Parameters.Add('crossinstall');
        Processor.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
        Processor.Parameters.Add('OS_SOURCE='+GetTargetOS);
        Processor.Parameters.Add('OS_TARGET='+FCrossOS_Target); //cross compile for different OS...
        Processor.Parameters.Add('CPU_TARGET='+FCrossCPU_Target); // and processor.
        Processor.Parameters.Add('NOGDBMI=1'); // prevent building of IDE to be 100% sure
        // suppress hints
        Processor.Parameters.Add('OPT='+STANDARDCOMPILEROPTIONS);
        if Length(FCrossOS_SubArch)>0 then Processor.Parameters.Add('SUBARCH='+FCrossOS_SubArch);

        CrossOptions:='';

        if CrossInstaller.BinUtilsPrefix<>'' then
        begin
          // Earlier, we used regular OPT; using CROSSOPT is apparently more precise
          CrossOptions:=CrossOptions+' -XP'+CrossInstaller.BinUtilsPrefix;
          Processor.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
        end;

        for i:=0 to CrossInstaller.CrossOpt.Count-1 do
        begin
          CrossOptions:=trimright(CrossOptions+' '+CrossInstaller.CrossOpt[i]);
        end;

        CrossOptions:=Trim(CrossOptions);
        if CrossOptions<>'' then
        begin
          Processor.Parameters.Add('CROSSOPT='+CrossOptions);
        end;

        try
          Processor.Execute;
        except
          on E: Exception do
          begin
            WritelnLog(infotext+'Running cross compiler fpc make crossinstall failed with an exception!'+LineEnding+
              'Details: '+E.Message,true);
            result:=false;
          end;
        end;

        if Processor.ExitStatus<>0 then
        begin
          // If anything else than crosswin32-64 or crosswin64-32, fail:
          result:=false;
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
          if result then
            infoln(infotext+'Problem installing crosscompiler for '+FCrossCPU_Target+'-'+FCrossOS_Target+'. Optional module; continuing regardless.', etInfo)
          else
            infoln(infotext+'Problem installing crosscompiler for '+FCrossCPU_Target+'-'+FCrossOS_Target+'.', etError);
          FCompiler:='////\\\Error trying to compile FPC\|!';
        end
        else
        begin

          // get the name of the cross-compiler we just built.
          IntermediateCompiler:=GetCrossCompilerName(CrossInstaller.TargetCPU);

          {$IFDEF UNIX}

          {$ifdef Darwin}
          // on Darwin, the normal compiler names are used for the final cross-target compiler !!
          // tricky !
          s:=GetCompilerName(CrossInstaller.TargetCPU);
          {$else}
          s:=GetCrossCompilerName(CrossInstaller.TargetCPU);
          {$endif}

          // copy over the cross-compiler towards the FPC bin-directory, with the right compilername.
          if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+IntermediateCompiler) then
          begin
            infoln(infotext+'Copy cross-compiler ('+IntermediateCompiler+') into: '+FBinPath,etInfo);
            FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+IntermediateCompiler,
              IncludeTrailingPathDelimiter(FBinPath)+s);
            fpChmod(IncludeTrailingPathDelimiter(FBinPath)+s,&755);
          end;

          {$ENDIF}

          // delete cross-compiler in source-directory
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+IntermediateCompiler);

          // Modify fpc.cfg
          // always add this, to be able to detect which cross-compilers are installed
          // helpfull for later bulk-update of all cross-compilers
          FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.cfg';
          if CrossInstaller.FPCCFGSnippet<>''
             then s:=CrossInstaller.FPCCFGSnippet+LineEnding
             else s:='# dummy (blank) config for auto-detect cross-compilers'+LineEnding;
          InsertFPCCFGSnippet(FPCCfg,
            SnipMagicBegin+FCrossCPU_target+'-'+FCrossOS_target+LineEnding+
            '#cross compile settings dependent on both target OS and target CPU'+LineEnding+
            '#IFDEF FPC_CROSSCOMPILING'+LineEnding+
            '#IFDEF CPU'+uppercase(FCrossCPU_Target+LineEnding)+
            '#IFDEF '+uppercase(FCrossOS_Target)+LineEnding+
            '# Inserted by fpcup '+DateTimeToStr(Now)+LineEnding+
            s+
            '#ENDIF'+LineEnding+
            '#ENDIF'+LineEnding+
            '#ENDIF'+LineEnding+
            SnipMagicEnd);
          {$IFDEF UNIX}
          result:=CreateFPCScript;
          {$ENDIF UNIX}
          GetCompiler;
        end;
      end;

      finally
        SetPath(OldPath,false,false);
      end;
    end;

    RemoveStaleBuildDirectories(FSourceDirectory,FCrossCPU_Target,FCrossOS_Target);

  end
  else
  begin
    infoln(infotext+'Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target+' !!!',etError);
    result:=false;
  end;

end;


constructor TFPCCrossInstaller.Create;
begin
  inherited create;
end;

destructor TFPCCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TFPCNativeInstaller }

function TFPCNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  OperationSucceeded:boolean;
  {$IFDEF MSWINDOWS}
  FileCounter:integer;
  {$ENDIF}
  s:string;
begin
  result:=inherited;
  OperationSucceeded:=true;

  Processor.Executable := Make;
  FErrorLog.Clear;
  Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
  Processor.Parameters.Clear;
  if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
  Processor.Parameters.Add('FPC='+FCompiler);
  Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
  {$IFDEF DEBUG}
  Processor.Parameters.Add('-d');
  {$ENDIF}
  Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
  {$IFDEF UNIX}
  Processor.Parameters.Add('INSTALL_BINDIR='+FBinPath);
  {$ELSE}
  Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  {$ENDIF}

  Processor.Parameters.Add('REVSTR='+ActualRevision);
  Processor.Parameters.Add('REVINC=force');

  if (GetNumericalVersion(GetFPCVersion)<(2*10000+4*100+4)) then
  begin
    Processor.Parameters.Add('DATA2INC=echo');
  end;

  if FBootstrapCompilerOverrideVersionCheck then
    Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
  s:=STANDARDCOMPILEROPTIONS+' '+FCompilerOptions;
  s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
  s:=Trim(s);
  {$IFDEF UNIX}
  s:='-Sg '+s;
  {$ENDIF}

  s:=s+' -dREVINC';

  {$if not defined(FPC_HAS_TYPE_EXTENDED)}
  // soft 80 bit float if available
  (*
  infoln(infotext+'Adding -dFPC_SOFT_FPUX80 compiler option to enable 80bit (soft)float support.',etInfo);
  s:=s+' -dFPC_SOFT_FPUX80';
  // needed at this moment ... the makefile is yet to be adjusted
  s:=s+' -Fi'+IncludeTrailingPathDelimiter(FSourceDirectory)+'rtl\x86_64';
  s:=s+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'rtl\x86_64';
  s:=s+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\x86_64';
  s:=s+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\x86';
  s:=s+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\systems';
  s:=s+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'rtl\inc';
  s:=s+' -Fu'+IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler';
  *)
  {$endif}

  Processor.Parameters.Add('OPT='+s);

  case UpperCase(ModuleName) of
    'FPC':
    begin
      Processor.Parameters.Add('all');
      Processor.Parameters.Add('install');
      infoln(infotext+'Running make all install',etInfo);
    end;
    else //raise error;
    begin
      Processor.Parameters.Add('--help'); // this should render make harmless
      WritelnLog(etError, infotext+'Invalid module name [' + ModuleName + '] specified! Please fix the code.', true);
      OperationSucceeded := false;
      Result := false;
      exit;
    end;
  end;

  try
    // At least on 2.7.1 we get access violations running fpc make
    // perhaps this try..except isolates that
    Processor.Execute;
    if Processor.ExitStatus <> 0 then
    begin
      OperationSucceeded := False;
      WritelnLog(etError, infotext+'Error running make failed with exit code '+inttostr(Processor.ExitStatus)+LineEnding+'. Details: '+FErrorLog.Text,true);
    end;
  except
    on E: Exception do
    begin
      OperationSucceeded := False;
      WritelnLog(etError, infotext+'Running fpc make failed with an exception!'+LineEnding+'. Details: '+E.Message,true);
    end;
  end;

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
    GetCompiler;
    // Verify it exists
    if not(FileExistsUTF8(FCompiler)) then
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
  result:=OperationSucceeded;
end;

constructor TFPCNativeInstaller.Create;
begin
  inherited create;
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

function TFPCInstaller.GetCompilerVersion(CompilerPath: string): string;
var
  Output: string;
begin
  Result:='0.0.0';
  if CompilerPath='' then exit;
  try
    Output:='';
    // -iW does not work on older compilers : use -iV
    if (ExecuteCommand(CompilerPath+ ' -iV', Output, FVerbose)=0) then
    //-iVSPTPSOTO
    begin
      Output:=TrimRight(Output);
      if Length(Output)>0 then Result:=Output;
    end;
  except
  end;
end;

function TFPCInstaller.GetCompilerVersionNumber(aVersion: string; const index:byte=0): integer;
var
  VersionSnippet:string;
  VersionList : TStringList;
begin
  result:=-1;
  VersionSnippet:=StringReplace(aVersion,'.',',',[rfReplaceAll]);
  if Length(VersionSnippet)>0 then
  begin
    VersionList := TStringList.Create;
    try
      VersionList.CommaText := VersionSnippet;
      if VersionList.Count>index then result := StrToIntDef(VersionList[index], -1);
    finally
      VersionList.Free;
    end;
  end;
end;

function TFPCInstaller.GetCompilerVersionFromUrl(aUrl: string): string;
var
  aVersion: string;
begin
  aVersion:=GetVersionFromUrl(aUrl);
  if aVersion='trunk' then result:=FPCTRUNKVERSION else result:=aVersion;
end;

function TFPCInstaller.GetCompilerVersionFromSource(aSourcePath: string): string;
var
  TxtFile:Text;
  version_nr:string;
  release_nr:string;
  patch_nr:string;
  found_version_nr:boolean;
  found_release_nr:boolean;
  found_patch_nr:boolean;
  s:string;
  x,y:integer;
begin

  //cheap (or expensive) coding ... but effective ... ;-)
  version_nr:='0';
  release_nr:='0';
  patch_nr:='0';

  found_version_nr:=false;
  found_release_nr:=false;
  found_patch_nr:=false;

  s:=IncludeTrailingPathDelimiter(aSourcePath) + 'compiler' + DirectorySeparator + 'version.pas';

  if FileExists(s) then
  begin

    AssignFile(TxtFile,s);
    Reset(TxtFile);
    while NOT EOF (TxtFile) do
    begin
      Readln(TxtFile,s);

      x:=Pos('version_nr',s);
      if x>0 then
      begin
        for y:=x+Length('version_nr') to Length(s) do
        begin
          if Ord(s[y]) in [ord('0')..ord('9')] then
          begin
            version_nr:=s[y];
            found_version_nr:=true;
            break;
          end;
        end;
      end;

      x:=Pos('release_nr',s);
      if x>0 then
      begin
        for y:=x+Length('release_nr') to Length(s) do
        begin
          if Ord(s[y]) in [ord('0')..ord('9')] then
          begin
            release_nr:=s[y];
            found_release_nr:=true;
            break;
          end;
        end;
      end;

      x:=Pos('patch_nr',s);
      if x>0 then
      begin
        for y:=x+Length('patch_nr') to Length(s) do
        begin
          if Ord(s[y]) in [ord('0')..ord('9')] then
          begin
            patch_nr:=s[y];
            found_patch_nr:=true;
            break;
          end;
        end;
      end;

      // check if ready
      if found_version_nr AND found_release_nr AND found_patch_nr then break;

    end;

    CloseFile(TxtFile);

  end else infoln('Tried to get FPC version from version.pas, but no version.pas found',etError);

  result:=version_nr+'.'+release_nr+'.'+patch_nr;
end;

function TFPCInstaller.GetBootstrapCompilerVersionFromVersion(aVersion: string): string;
var
  s:string;
begin
  s:=aVersion;

  {$IFDEF CPUAARCH64}
  if (s=FPCTRUNKVERSION) then result:=FPCTRUNKVERSION
  else result:='0.0.0';
  exit;
  {$ENDIF}

  result:='0.0.0';

  // for trunk i.e. , also 3.0.2 is allowed
  // but online, only official 3.0.0 bootstrapper available

  if s=FPCTRUNKVERSION then result:='3.0.0'
  else if s='3.0.4' then result:='3.0.0'
  else if s='3.0.3' then result:='3.0.0'
  else if (s='3.0.2')  or (s='3.0.1') then result:='3.0.0'
  //else if (s='3.0.2') or (s='3.0.1') then result:='2.6.4'
  else if s='3.0.0' then result:='2.6.4'
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

end;

function TFPCInstaller.GetBootstrapCompilerVersionFromSource(aSourcePath: string; GetLowestRequirement:boolean=false): string;
var
  TxtFile:Text;
  s:string;
  x:integer;
  FinalVersion,RequiredVersion,RequiredVersion2:integer;
begin
  result:='0.0.0';

  {$IFDEF CPUAARCH64}
  result:=FPCTRUNKVERSION;
  exit;
  {$ENDIF}

  s:=IncludeTrailingPathDelimiter(aSourcePath) + 'Makefile.fpc';

  RequiredVersion:=0;
  RequiredVersion2:=0;

  if FileExists(s) then
  begin

    AssignFile(TxtFile,s);
    Reset(TxtFile);
    while NOT EOF (TxtFile) do
    begin
      Readln(TxtFile,s);

      x:=Pos('REQUIREDVERSION=',s);
      if x>0 then
      begin
        x:=x+Length('REQUIREDVERSION=');
        if (x<=Length(s)) then
        begin
          RequiredVersion:=RequiredVersion+(Ord(s[x])-Ord('0'))*10000;
          inc(x,2);
          if (x<=Length(s)) then
          begin
            RequiredVersion:=RequiredVersion+(Ord(s[x])-Ord('0'))*100;
            inc(x,2);
            if (x<=Length(s)) then RequiredVersion:=RequiredVersion+(Ord(s[x])-Ord('0'))*1;
          end;
        end;
      end;

      // REQUIREDVERSION2 is optional; could be empty
      x:=Pos('REQUIREDVERSION2=',s);
      if x>0 then
      begin
        x:=x+Length('REQUIREDVERSION2=');
        if (x<=Length(s)) then
        begin
          RequiredVersion2:=RequiredVersion2+(Ord(s[x])-Ord('0'))*10000;
          inc(x,2);
          if (x<=Length(s)) then
          begin
            RequiredVersion2:=RequiredVersion2+(Ord(s[x])-Ord('0'))*100;
            inc(x,2);
            if (x<=Length(s)) then RequiredVersion2:=RequiredVersion2+(Ord(s[x])-Ord('0'))*1;
          end;
        end;
      end;

      if ((RequiredVersion>0) AND (RequiredVersion2>0)) then break;

    end;

    if GetLowestRequirement then
    begin
      if ( (RequiredVersion2>RequiredVersion) OR (RequiredVersion2=0))
          then FinalVersion:=RequiredVersion
          else FinalVersion:=RequiredVersion2;
    end
    else
    begin
      if (RequiredVersion2>RequiredVersion)
          then FinalVersion:=RequiredVersion2
          else FinalVersion:=RequiredVersion;
    end;

    CloseFile(TxtFile);

    result:=InttoStr(FinalVersion DIV 10000);
    FinalVersion:=FinalVersion MOD 10000;
    result:=result+'.'+InttoStr(FinalVersion DIV 100);
    FinalVersion:=FinalVersion MOD 100;
    result:=result+'.'+InttoStr(FinalVersion);

  end else infoln('Tried to get required bootstrap compiler version from Makefile.fpc, but no Makefile.fpc found',etError);

end;

function TFPCInstaller.CreateFPCScript: boolean;
  {$IFDEF UNIX}
var
  FPCScript:string;
  TxtFile:Text;
  FPCCompiler:String;
  {$ENDIF UNIX}
begin
  {$IFDEF UNIX}
  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (CreateFPCScript): ';
  FPCCompiler:=IncludeTrailingPathDelimiter(FBinPath)+'fpc'+GetExeExt;

  // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
  // If this fails, Lazarus compilation will fail...
  FPCScript := IncludeTrailingPathDelimiter(ExtractFilePath(FPCCompiler)) + 'fpc.sh';
  if FileExists(FPCScript) then
  begin
    infoln(localinfotext+'fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.',etInfo);
    if not(sysutils.DeleteFile(FPCScript)) then
    begin
      infoln(localinfotext+'Error deleting existing launcher script for FPC:'+FPCScript,eterror);
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
    IncludeTrailingPathDelimiter(ExtractFilePath(FPCCompiler)),'fpc.cfg '+
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

  if FBootstrapCompilerURL='' then exit;

  if OperationSucceeded then
  begin
    OperationSucceeded:=ForceDirectoriesUTF8(FBootstrapCompilerDirectory);
    if OperationSucceeded=false then infoln(localinfotext+'Could not create directory '+FBootstrapCompilerDirectory,etError);
  end;

  BootstrapArchive := SysUtils.GetTempFileName;
  if OperationSucceeded then
  begin
    OperationSucceeded:=Download(FUseWget, FBootstrapCompilerURL, BootstrapArchive,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
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
      infoln(localinfotext+'Going to copy '+BootstrapArchive+' to '+FBootstrapCompiler,etWarning);
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
  testcompiler:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc1';
  if not FileExists(testcompiler) then
  begin //darwin
    testcompiler:=IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+DirectorySeparator+'ppc';
  end;
  if FileExists(testcompiler) then
  begin
    result:=GetCompilerVersion(testcompiler);
  end
  else
  begin
    result:=GetCompilerVersionFromSource(FSourceDirectory);
    if result='0.0.0' then result:=GetCompilerVersionFromUrl(FURL);
  end;
end;

function TFPCInstaller.InitModule(aBootstrapVersion:string):boolean;
const
  {$IF (defined(OpenBSD)) and (defined(CPU64))}
  // 2.6.2 and older do not work anymore on newer OpenBSD64 versions
  FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION=(2*10000+6*100+2);
  {$else}
  // 2.2.4 and older have no official FPC bootstrapper available online
  FPC_OFFICIAL_MINIMUM_BOOTSTRAPVERSION=(2*10000+2*100+4);
  {$endif}
var
  aCompilerList:TStringList;
  i,j:integer;
  aCompilerArchive,aStandardCompilerArchive:string;
  aCompilerFound:boolean;
  {$IFDEF FREEBSD}
  FreeBSDVersion:integer;
  {$ENDIF}
  s,s1:string;
  ReturnCode:integer;
  aLocalBootstrapVersion:string;
  aIntermediateBootstrapCompiler:string;
  aGithubBootstrapURL:string;
  aDownLoader: TBasicDownLoader;
begin
  result := true;

  if (InitDone) AND (aBootstrapVersion='') then exit;

  localinfotext:=Copy(Self.ClassName,2,MaxInt)+' (InitModule): ';

  result:=CheckAndGetTools;

  if FVerbose then Processor.OnOutputM:=@DumpOutput;

  WritelnLog(localinfotext+'Init:', false);
  WritelnLog(localinfotext+'FPC directory:      ' + FSourceDirectory, false);
  WritelnLog(localinfotext+'FPC URL:            ' + FURL, false);
  WritelnLog(localinfotext+'FPC options:        ' + FCompilerOptions, false);

  // set standard bootstrap compilername
  FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetTargetCPUOS+'-'+GetCompilerName(GetTargetCPU);
  if NOT FileExists(FBootstrapCompiler) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+GetCompilerName(GetTargetCPU);

  // if we have previously build an intermediate compiler, use that !
  aIntermediateBootstrapCompiler:=ExtractFilePath(FBootstrapCompiler)+'intermediate_'+GetCompilerName(GetTargetCPU);
  if FileExists(aIntermediateBootstrapCompiler) then FBootstrapCompiler:=aIntermediateBootstrapCompiler;

  {$IFDEF Darwin}
    {$IFDEF CPU32}
      if NOT FileExists(FBootstrapCompiler) then FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
    {$ENDIF CPU32}
  {$ENDIF Darwin}

  if (aBootstrapVersion<>'') then
  begin

    infoln(localinfotext+'Looking for a bootstrap compiler from official FPC bootstrap binaries.',etInfo);

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

      // first, try official FPC binaries

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

          s:=FPCFTPURL+'/'+aLocalBootstrapVersion+'/bootstrap/';

          infoln(localinfotext+'Looking for (online) bootstrapper '+aCompilerArchive + ' in ' + s,etInfo);

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
              if aCompilerList.Count>0 then infoln(localinfotext+'Found FPC v'+aLocalBootstrapVersion+' online bootstrappers: '+aCompilerList.CommaText,etInfo);
            end;

            {$IFDEF FREEBSD}
            // FreeBSD : special because of versions
            FreeBSDVersion:=0;
            for i:=0 to Pred(aCompilerList.Count) do
            begin
              infoln(localinfotext+'Found online '+aLocalBootstrapVersion+' bootstrap compiler: '+aCompilerList[i],etDebug);
              if Pos(GetTargetCPUOS,aCompilerList[i])=1 then
              begin
                aCompilerFound:=True;
                // get the latest available version
                FreeBSDVersion:=Max(FreeBSDVersion,StrToIntDef(aCompilerList[i][Length(GetTargetCPUOS)+1],0));
              end;
            end;
            if (aCompilerFound) then
            begin
              if FreeBSDVersion>0
                 then aCompilerArchive:=GetTargetCPUOS+InttoStr(FreeBSDVersion)+'-'+GetCompilerName(GetTargetCPU)
                 else aCompilerArchive:=GetTargetCPUOS+'-'+GetCompilerName(GetTargetCPU);
              // remove file extension
              aCompilerArchive:=ChangeFileExt(aCompilerArchive,'');
              aCompilerArchive:=aCompilerArchive+'.bz2';
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

            // look for a previous compiler if not found, and use overrideversioncheck
            if NOT aCompilerFound then
            begin
              FBootstrapCompilerOverrideVersionCheck:=true;
              s:=GetBootstrapCompilerVersionFromVersion(aLocalBootstrapVersion);
              if aLocalBootstrapVersion<>s
                 then aLocalBootstrapVersion:=s
                 else break;
            end;
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
          infoln(localinfotext+'Got a bootstrap compiler from official FPC bootstrap sources.',etInfo);
          FBootstrapCompilerURL := FPCFTPURL+'/'+aLocalBootstrapVersion+'/bootstrap/'+aCompilerArchive;
        end;
      end;


      // second, try the FPCUP binaries from release
      if (NOT aCompilerFound) then
      begin

        infoln(localinfotext+'Slight panic: No official FPC bootstrapper found.',etError);
        infoln(localinfotext+'Now looking for last resort bootstrap compiler from Github FPCUP(deluxe) releases.',etError);

        aGithubBootstrapURL:='';

        aLocalBootstrapVersion:=aBootstrapVersion;
        FBootstrapCompilerOverrideVersionCheck:=false;

        aCompilerList:=TStringList.Create;
        try
          aCompilerList.Clear;

          if Length(HTTPProxyHost)>0 then aDownLoader.setProxy(HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
          while ((NOT aCompilerFound) AND (GetNumericalVersion(aLocalBootstrapVersion)>0)) do
          begin
            infoln(localinfotext+'Looking online for a FPCUP(deluxe) bootstrapper with version '+aLocalBootstrapVersion,etInfo);
            aGithubBootstrapURL:=FPCUPGITREPO+
              '/releases/download/bootstrappers_v1.0/'+
              'fpcup-'+StringReplace(aLocalBootstrapVersion,'.','_',[rfReplaceAll])+'-'+GetTargetCPUOS+'-'+GetCompilerName(GetTargetCPU);
            infoln(localinfotext+'Checking existence of: '+aGithubBootstrapURL,etDebug);

            aCompilerFound:=aDownLoader.checkURL(aGithubBootstrapURL);

            if aCompilerFound then
            begin
              aCompilerList.Add(aGithubBootstrapURL);
              infoln(localinfotext+'Success: found a FPCUP(deluxe) bootstrapper with version '+aLocalBootstrapVersion,etInfo);
            end
            else
            begin
              // look for a previous (fitting) compiler if not found, and use overrideversioncheck
              FBootstrapCompilerOverrideVersionCheck:=true;
              s:=GetBootstrapCompilerVersionFromVersion(aLocalBootstrapVersion);
              if aLocalBootstrapVersion<>s
                 then aLocalBootstrapVersion:=s
                 else break;
            end;
          end;

          if NOT aCompilerFound then
          begin
            aCompilerList.Sorted:=true;
            for i:=0 to Pred(aCompilerList.Count) do
            begin
              if Pos(GetTargetCPUOS+'-'+GetCompilerName(GetTargetCPU),aCompilerList[i])>0 then
              begin
                aGithubBootstrapURL:=aCompilerList[i];
                FBootstrapCompilerOverrideVersionCheck:=true;
                aCompilerFound:=true;
                j:=Pos('fpcup-',aGithubBootstrapURL);
                aLocalBootstrapVersion := Copy(aGithubBootstrapURL,7,5);
                aLocalBootstrapVersion := StringReplace(aLocalBootstrapVersion,'_','.',[rfReplaceAll]);
                infoln(localinfotext+'Got last resort FPCUP(deluxe) bootstrapper with version: '+aLocalBootstrapVersion,etInfo);
                break;
              end;
            end;
          end;

        finally
          aCompilerList.Free;
        end;

        // found a less official FPCUP bootstrapper !
        if (aCompilerFound) then
        begin
          if FBootstrapCompilerURL='' then
          begin
            infoln(localinfotext+'Got a bootstrap compiler from FPCUP(deluxe) bootstrap sources.',etInfo);
            FBootstrapCompilerURL := aGithubBootstrapURL;
          end;
        end;

      end;

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
  FBinPath:=IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+DirectorySeparator+GetFPCTarget(true);

  {$IFDEF MSWINDOWS}
  s:='';
  // preserve cygwin and msys(2) paths when setting path
  {
  aCompilerList:=TStringList.Create;
  try
    aCompilerList.Delimiter:=PathSeparator;
    aCompilerList.StrictDelimiter:=True;
    aCompilerList.DelimitedText:=GetPath;
    for i:=0 to aCompilerList.Count-1 do
    begin
      s1:=aCompilerList[i];
      if (Pos('cygwin',LowerCase(s1))>0) OR (Pos('msys',LowerCase(s1))>0) OR (Pos('msys2',LowerCase(s1))>0) then
      begin
        s:=PathSeparator+s1+s;
      end;
    end;
  finally
    aCompilerList.Free;
  end;
  }
  if Length(FSVNDirectory)>0
     then s:=PathSeparator+ExcludeTrailingPathDelimiter(FSVNDirectory)+s;
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  // add install/fpc/utils to solve data2inc not found by fpcmkcfg
  // also add src/fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(
    FBinPath+PathSeparator+ {compiler for current architecture}
    FMakeDir+PathSeparator+
    FBootstrapCompilerDirectory+PathSeparator+
    IncludeTrailingPathDelimiter(FInstallDirectory)+PathSeparator+
    IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
    IncludeTrailingPathDelimiter(FInstallDirectory)+'utils'+PathSeparator+
    IncludeTrailingPathDelimiter(FSourceDirectory)+PathSeparator+
    IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+PathSeparator+
    IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+PathSeparator+
    s,
    false,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  // add install/fpc/utils to solve data2inc not found by fpcmkcfg
  // also add src/fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(
    FBinPath+PathSeparator+
    FBootstrapCompilerDirectory+PathSeparator+
    IncludeTrailingPathDelimiter(FInstallDirectory)+PathSeparator+
    IncludeTrailingPathDelimiter(FInstallDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
    IncludeTrailingPathDelimiter(FInstallDirectory)+'utils'+PathSeparator+
    IncludeTrailingPathDelimiter(FSourceDirectory)+PathSeparator+
    IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler'+PathSeparator+
    IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+PathSeparator+
    // pwd is located in /bin ... the makefile needs it !!
    // tools are located in /usr/bin ... the makefile needs it !!
    '/bin'+PathSeparator+'/usr/bin',
    true,false);
  {$ENDIF UNIX}
  InitDone:=result;
end;


function TFPCInstaller.BuildModule(ModuleName: string): boolean;
var
  bIntermediateNeeded:boolean;
  IntermediateCompiler:string;
  TargetCompiler:string;
  ICSVNCommand:string;
  RequiredBootstrapVersion:string;
  RequiredBootstrapVersionLow:string;
  RequiredBootstrapVersionHigh:string;
  RequiredBootstrapBootstrapVersion:string;
  FPCCfg: string;
  FPCMkCfg: string; //path+file of fpcmkcfg
  OperationSucceeded: boolean;
  PlainBinPath: string; //directory above the architecture-dependent FBinDir
  s:string;
  TxtFile:Text;
  BootstrapDirectory :string;
  x:integer;
  Output: string = '';
  ReturnCode: integer;
begin
  result := inherited;
  result := InitModule;
  if not result then exit;

  infoln(infotext+'Building module '+ModuleName+'...',etInfo);

  bIntermediateNeeded:=false;

  TargetCompiler:=GetCompilerName(GetTargetCPU);
  IntermediateCompiler:='intermediate_'+TargetCompiler;

  infoln(infotext+'We have a FPC source (@ '+FSourceDirectory+') with version: '+GetCompilerVersionFromSource(FSourceDirectory),etInfo);

  // if cross-compiling, skip a lot of code
  // trust the previous work done by this code for the native installer!
  if (NOT (Self is TFPCCrossInstaller)) then
  begin
    RequiredBootstrapVersion:='0.0.0';
    RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromSource(FSourceDirectory,True);
    RequiredBootstrapVersionHigh:=GetBootstrapCompilerVersionFromSource(FSourceDirectory,False);

    if RequiredBootstrapVersionLow='0.0.0' then
    begin
      RequiredBootstrapVersionLow:=GetBootstrapCompilerVersionFromVersion(GetCompilerVersionFromSource(FSourceDirectory));
      if RequiredBootstrapVersionLow='0.0.0' then
      begin
        infoln(infotext+'Could not determine required bootstrap compiler version. Should not happen. Aborting.',etError);
        exit(false);
      end else infoln(infotext+'To compile this FPC, we use a compiler with (lowest) version : '+RequiredBootstrapVersionLow,etInfo);
    end else infoln(infotext+'To compile this FPC, we need (required) a compiler with (lowest) version : '+RequiredBootstrapVersionLow,etInfo);

    OperationSucceeded:=false;

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
      // start with the lowest requirement, due to limited availability of online bootstrappers ??!!
      RequiredBootstrapVersion:=RequiredBootstrapVersionLow;
      result:=InitModule(RequiredBootstrapVersion);
      if (GetCompilerVersion(FCompiler)=RequiredBootstrapVersion)
        then infoln(infotext+'To compile this FPC, we will use a fresh compiler with version : '+RequiredBootstrapVersion,etInfo)
        else
        begin
          // check if we have a higher acceptable requirement for the bootstrapper
          if (GetCompilerVersion(FCompiler)=RequiredBootstrapVersionHigh) then
          begin
            // if so, set bootstrapper to lower one !!
            RequiredBootstrapVersion:=RequiredBootstrapVersionHigh;
            infoln(infotext+'To compile this FPC, we can also (and will) use (required) a fresh compiler with version : '+RequiredBootstrapVersion,etInfo);
          end;
        end;
    end;

    // if we still do not have the correct bootstrapper, build an intermediate one with the right version to compile the FPC source
    // but only if required version >= 2.0.0 (no easy source available online for earlier versions)
    if (GetCompilerVersion(FCompiler)<>RequiredBootstrapVersion) AND (GetNumericalVersion(RequiredBootstrapVersion)>=(2*10000+0*100+0)) then
    begin
      // we need an intermediate compiler !!
      if NOT FileExists(ExtractFilePath(FCompiler)+IntermediateCompiler) then
      begin
        bIntermediateNeeded:=true;
      end
      else
      begin
        if (GetCompilerVersion(ExtractFilePath(FCompiler)+IntermediateCompiler)<>RequiredBootstrapVersion) then
        begin
          bIntermediateNeeded:=true;
        end
        else
        begin
          infoln(infotext+'Using available FPC '+GetCompilerVersion(ExtractFilePath(FCompiler)+IntermediateCompiler)+' intermediate compiler.',etInfo);
          FCompiler:=ExtractFilePath(FCompiler)+IntermediateCompiler;
          FBootstrapCompilerOverrideVersionCheck:=False;
        end;
      end;
    end else infoln(infotext+'Available bootstrapper has correct version !',etInfo);

    {$IFDEF CPUAARCH64}
    // we build with >3.0 (trunk), while aarch64 is not available for FPC =< 3.0
    FBootstrapCompilerOverrideVersionCheck:=true;
    {$ENDIF CPUAARCH64}

    if (bIntermediateNeeded) then
    begin
      infoln(infotext+'We need to build an FPC ' + RequiredBootstrapVersion + ' intermediate compiler.',etInfo);

      // get the correct binutils (Windows only)
      CreateBinutilsList(RequiredBootstrapVersion);
      result:=CheckAndGetNeededBinUtils;
      //if not result then exit;

      BootstrapDirectory := IncludeTrailingPathDelimiter(FBaseDirectory)+'fpc'+StringReplace(RequiredBootstrapVersion,'.','',[rfReplaceAll,rfIgnoreCase])+'bootstrap';

      FSVNClient.ModuleName:=ModuleName;

      ReturnCode:=-1;
      if DirectoryExists(BootstrapDirectory) then
      begin
        // first cleanout the intermediat bootstrapper in case of .... as the rules prescibe
        Processor.Executable := Make;
        Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(BootstrapDirectory);
        Processor.Parameters.Clear;
        Processor.Parameters.Add('clean');
        if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
        Processor.Parameters.Add('FPC='+FCompiler);
        Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(BootstrapDirectory));

        Processor.Parameters.Add('OS_TARGET='+GetTargetOS);
        Processor.Parameters.Add('CPU_TARGET='+GetTargetCPU);

        Processor.Execute;
        infoln(infotext+'Cleaned FPC ' + RequiredBootstrapVersion + ' intermediate bootstrap compiler.',etInfo);

        ReturnCode := FSVNClient.Execute('info ' + BootstrapDirectory);
        if (ReturnCode <> 0) then
        begin
          FSVNClient.Execute('cleanup --non-interactive ' + BootstrapDirectory);
          ReturnCode := FSVNClient.Execute('info ' + BootstrapDirectory);
        end;
      end;

      infoln(infotext+'Checking out/updating ' + ModuleName + ' ' + RequiredBootstrapVersion + ' intermediate compiler sources.',etInfo);

      s:=FPCSVNURL+'/fpc/tags/release_'+StringReplace(RequiredBootstrapVersion,'.','_',[rfReplaceAll,rfIgnoreCase]);
      if (ReturnCode = 0)
          then ICSVNCommand:='update --non-interactive --trust-server-cert --quiet'
          else ICSVNCommand:='checkout --non-interactive --trust-server-cert --quiet --depth=files ' + s;

      ReturnCode := FSVNClient.Execute(ICSVNCommand + ' ' + BootstrapDirectory);
      if (ReturnCode <> 0) then
      begin
        // try once again, after a cleanup
        ReturnCode := FSVNClient.Execute('cleanup --non-interactive ' + BootstrapDirectory);
        if (ReturnCode = 0) then ReturnCode := FSVNClient.Execute(ICSVNCommand + ' ' + BootstrapDirectory);
      end;

      // get compiler source
      s:=IncludeTrailingPathDelimiter(BootstrapDirectory)+'compiler';
      if (ReturnCode = 0) then ReturnCode := FSVNClient.Execute('update compiler --non-interactive --trust-server-cert --quiet ' + s);
      // try once again
      if (ReturnCode <> 0) then
      begin
        FSVNClient.Execute('cleanup --non-interactive ' + s);
        ReturnCode := FSVNClient.Execute('update compiler --non-interactive --trust-server-cert --quiet ' + s);
      end;

      // get rtl source
      s:=IncludeTrailingPathDelimiter(BootstrapDirectory)+'rtl';
      if (ReturnCode = 0) then ReturnCode := FSVNClient.Execute('update rtl --non-interactive --trust-server-cert --quiet ' + s);
      // try once again
      if (ReturnCode <> 0) then
      begin
        FSVNClient.Execute('cleanup --non-interactive ' + s);
        ReturnCode := FSVNClient.Execute('update rtl --non-interactive --trust-server-cert --quiet ' + s);
      end;

      if (ReturnCode = 0) then
      begin
        infoln(infotext+'We have a FPC bootstrap source (@ '+BootstrapDirectory+') with version: '+RequiredBootstrapVersion,etInfo);
        RequiredBootstrapBootstrapVersion:=GetBootstrapCompilerVersionFromSource(BootstrapDirectory);
        if RequiredBootstrapBootstrapVersion='0.0.0' then
        begin
          RequiredBootstrapBootstrapVersion:=GetBootstrapCompilerVersionFromVersion(GetCompilerVersionFromSource(BootstrapDirectory));
          infoln(infotext+'To compile this bootstrap FPC, we should use a compiler with version : '+RequiredBootstrapBootstrapVersion,etInfo);
        end else infoln(infotext+'To compile this bootstrap FPC, we need (required) a compiler with version : '+RequiredBootstrapBootstrapVersion,etInfo);

        // check if we have a lower acceptable requirement for the bootstrapbootstrapper
        if (GetCompilerVersion(FCompiler)<>RequiredBootstrapBootstrapVersion) then
        begin
          // get lower requirement for the bootstrapper
          s:=GetBootstrapCompilerVersionFromSource(BootstrapDirectory,True);
          // if so, set bootstrapper to lower one !!
          if (GetCompilerVersion(FCompiler)=s) then
          begin
            RequiredBootstrapBootstrapVersion:=s;
            infoln(infotext+'To compile this bootstrap FPC, we can also (and will) use (required) a compiler with version : '+RequiredBootstrapBootstrapVersion,etInfo);
          end;
        end;

        Processor.Executable := Make;
        Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(BootstrapDirectory);

        // clean and build intermediate
        for ReturnCode:=0 to 1 do
        begin
          Processor.Parameters.Clear;
          if ReturnCode=0
             then Processor.Parameters.Add('clean')
             else Processor.Parameters.Add('compiler_cycle');
          // not sure if this needed here, but better safe than sorry
          if (GetNumericalVersion(RequiredBootstrapBootstrapVersion)<(2*10000+4*100+4)) then
          begin
            Processor.Parameters.Add('DATA2INC=echo');
          end;
          if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
          Processor.Parameters.Add('FPC='+FCompiler);
          Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(BootstrapDirectory));

          // Legacy settings from fpcup ... not sure if correct [Don]: disabled for now
          // Copy over user-specified instruction sets e.g. for trunk compiler...
          // in CROSSOPT though, as the stable compiler likely will not understand them
          // if FCompilerOptions<>'' then Processor.Parameters.Add('CROSSOPT='+FCompilerOptions);

          s:=STANDARDCOMPILEROPTIONS;
          {$ifdef CPUARMHF}
          s:=s+' -dFPC_ARMHF';
          {$endif}
          {$ifdef DEBUG}
          //s:=s+' -g -gl -dEXTDEBUG';
          //s:=s+' -dEXTDEBUG';
          {$endif}
          Processor.Parameters.Add('OPT='+s);

          Processor.Parameters.Add('OS_TARGET='+GetTargetOS);
          Processor.Parameters.Add('CPU_TARGET='+GetTargetCPU);

          if (GetCompilerVersion(FCompiler)<>RequiredBootstrapBootstrapVersion) then
          begin
             if ReturnCode=1 then infoln(infotext+'Apply OVERRIDEVERSIONCHECK=1, because we have a (wrong) bootstrap bootstrapper with version '+GetCompilerVersion(FCompiler),etInfo);
             Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
          end;
          if ReturnCode=1 then infoln(infotext+'Running make cycle for intermediate bootstrap compiler:',etInfo);
          Processor.Execute;
          if Processor.ExitStatus <> 0 then
          begin
            result := False;
            if ReturnCode=0 then infoln(infotext+'Running clean cycle for intermediate bootstrap compiler failed',etError);
            if ReturnCode=1 then infoln(infotext+'Running make cycle for intermediate bootstrap compiler failed',etError);
            exit;
          end;
          if ReturnCode=1 then infoln(infotext+'Successfully build FPC ' + RequiredBootstrapVersion + ' intermediate bootstrap compiler.',etInfo);
        end;

        infoln(infotext+'Going to copy bootstrapper ' + IncludeTrailingPathDelimiter(BootstrapDirectory)+'compiler/'+TargetCompiler + ' towards bootstrapper ' + ExtractFilePath(FCompiler)+IntermediateCompiler,etInfo);
        FileUtil.CopyFile(IncludeTrailingPathDelimiter(BootstrapDirectory)+'compiler/'+TargetCompiler,
          ExtractFilePath(FCompiler)+IntermediateCompiler);

        //Make executable
        {$ifdef unix}
        OperationSucceeded:=(fpChmod(ExtractFilePath(FCompiler)+IntermediateCompiler, &755)=0); //rwxr-xr-x
        if OperationSucceeded=false then infoln('Intermediate bootstrap compiler: chmod failed for '+ExtractFilePath(FCompiler)+IntermediateCompiler,etError);
        {$endif}

        // Now we can change the compiler from the stable one to the one in our FPC repo:
        FCompiler:=ExtractFilePath(FCompiler)+IntermediateCompiler;
        FBootstrapCompilerOverrideVersionCheck:=False;
      end
      else
      begin
        result := False;
        infoln(infotext+'Error (SVN) getting sources for intermediate bootstrap compiler. Error: '+InttoStr(ReturnCode),etError);
        exit;
      end;
    end
    else
    begin
      // get the correct binutils (Windows only)
      //CreateBinutilsList(GetBootstrapCompilerVersionFromSource(FSourceDirectory));
      //CreateBinutilsList(GetCompilerVersionFromSource(FSourceDirectory));
      CreateBinutilsList(RequiredBootstrapVersion);
      result:=CheckAndGetNeededBinUtils;
      //if not result then exit;
    end;

    {$ifdef win64}
    // Deals dynamically with either ppc386.exe or native ppcx64.exe
    if pos('ppc386.exe',FCompiler)>0 then //need to build ppcx64 before
    begin
      infoln('We have ppc386. We need ppcx64. So make it !',etInfo);
      Processor.Executable := Make;
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Parameters.Clear;
      Processor.Parameters.Add('compiler_cycle');
      if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
      Processor.Parameters.Add('FPC='+FCompiler);
      Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('OS_SOURCE=win32');
      Processor.Parameters.Add('CPU_SOURCE=i386');
      Processor.Parameters.Add('OS_TARGET=win64');
      Processor.Parameters.Add('CPU_TARGET=x86_64');
      Processor.Parameters.Add('OPT='+STANDARDCOMPILEROPTIONS);
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      infoln(infotext+'Running make cycle for Windows FPC64:',etInfo);
      Processor.Execute;
      if Processor.ExitStatus <> 0 then
      begin
        result := False;
        WritelnLog(etError, infotext+'Failed to build ppcx64 bootstrap compiler.');
        exit;
      end;
      FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler\ppcx64.exe',
        ExtractFilePath(FCompiler)+'ppcx64.exe');
      // Now we can change the compiler from the i386 to the x64 compiler:
      FCompiler:=ExtractFilePath(FCompiler)+'ppcx64.exe';
    end;
    {$endif win64}
    {$ifdef darwin}
    if pos('ppcuniversal',FCompiler)>0 then //need to build ppcxxx before
    begin
      infoln(infotext+'We have ppcuniversal. We need '+TargetCompiler+'. So make it !',etInfo);
      Processor.Executable := Make;
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Parameters.Clear;
      Processor.Parameters.Add('compiler_cycle');
      if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
      Processor.Parameters.Add('FPC='+FCompiler);
      Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('OS_TARGET=' + GetTargetOS);
      Processor.Parameters.Add('CPU_TARGET=' + GetTargetCPU);
      Processor.Parameters.Add('OPT='+STANDARDCOMPILEROPTIONS);
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        Processor.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      infoln(infotext+'Running make cycle for FPC '+TargetCompiler+' bootstrap compiler only',etInfo);
      Processor.Execute;
      if Processor.ExitStatus <> 0 then
      begin
        result := False;
        WritelnLog(etError, infotext+'Failed to build '+s+' bootstrap compiler.');
        exit;
      end;

      // copy over the fresh bootstrapper, if any
      if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompiler) then
      begin
        // Now we can change the compiler from the ppcuniversal to the target compiler:
        FCompiler:=ExtractFilePath(FCompiler)+TargetCompiler;
        infoln(infotext+'Copy fresh compiler ('+TargetCompiler+') into: '+ExtractFilePath(FCompiler),etDebug);
        FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompiler,
          FCompiler);
        fpChmod(FCompiler,&755);
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
    if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompiler) then
    begin
      infoln(infotext+'Copy compiler ('+TargetCompiler+') into: '+FBinPath,etDebug);
      FileUtil.CopyFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'compiler/'+TargetCompiler,
        IncludeTrailingPathDelimiter(FBinPath)+TargetCompiler);
      fpChmod(IncludeTrailingPathDelimiter(FBinPath)+TargetCompiler,&755);
    end;

    // create link 'units' below FSourceDirectory to
    // <somewhere>/lib/fpc/$fpcversion/units
    DeleteFile(IncludeTrailingPathDelimiter(FInstallDirectory)+'units');
    fpSymlink(pchar(IncludeTrailingPathDelimiter(FInstallDirectory)+'lib/fpc/'+GetFPCVersion+'/units'),
      pchar(IncludeTrailingPathDelimiter(FInstallDirectory)+'units'));
  end;
  {$ENDIF UNIX}

  FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.cfg';

  // Find out where fpcmkcfg lives - only if necessary.
  if OperationSucceeded and
    (FileExists(FPCCfg)=false) then
  begin
    fpcmkcfg:=IncludeTrailingPathDelimiter(FBinPath) + 'fpcmkcfg'+GetExeExt;
    if not(CheckExecutable(fpcmkcfg,'-h','fpcmkcfg')) then
    begin
      // Newer 3.1 trunk versions put fpcmkcfg in bin itself ??!!
      // todo check !!
      // base or install directory
      infoln(infotext+'Did not find '+fpcmkcfg+'. Now looking in '+
        IncludeTrailingPathDelimiter(FInstallDirectory)+'bin.',etDebug);
      fpcmkcfg:=IncludeTrailingPathDelimiter(FInstallDirectory)+
        'bin'+DirectorySeparator+'fpcmkcfg'+GetExeExt;
      if not(CheckExecutable(fpcmkcfg,'-h','fpcmkcfg')) then
      begin
        infoln(infotext+'Could not find fpcmkcfg in '+fpcmkcfg+'. Aborting.',etError);
        fpcmkcfg:='';
        OperationSucceeded:=false;
      end
      else
      begin
        infoln(infotext+'Found valid fpcmkcfg executable: '+fpcmkcfg,etInfo);
      end;
    end;
  end;

  //todo: after fpcmkcfg create a config file for fpkpkg or something
  if OperationSucceeded then
  begin
    // Create fpc.cfg if needed
    if FileExists(FPCCfg) = False then
    begin
      Processor.Executable := fpcmkcfg;
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FInstallDirectory);
      Processor.Parameters.Clear;
      Processor.Parameters.Add('-d');
      Processor.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FInstallDirectory));

      Processor.Parameters.Add('-o');
      Processor.Parameters.Add('' + FPCCfg + '');
      infoln(infotext+'Creating fpc.cfg: '+Processor.Executable+' '+StringReplace(Processor.Parameters.CommaText,',',' ',[rfReplaceAll]),etInfo);
      try
        Processor.Execute;
      except
        on E: Exception do
          begin
          WritelnLog(etError, infotext+'Running fpcmkcfg failed with an exception!'+LineEnding+
            'Details: '+E.Message,true);
          OperationSucceeded := False;
          end;
      end;

      if Processor.ExitStatus <> 0 then
      begin
        OperationSucceeded := False;
        WritelnLog(etError, infotext+'Running fpcmkcfg failed with exit code '+inttostr(Processor.ExitStatus),true);
      end;

      // if, for one reason or another, there is no cfg file, create a minimal one by ourselves
      if FileExists(FPCCfg) = False then
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

      // On *nix FPC 3.1.x, both "architecture bin" and "plain bin" may contain tools like fpcres.
      // Adding this won't hurt on Windows.
      // Adjust for that
      PlainBinPath:=ResolveDots(SafeExpandFileName(IncludeTrailingPathDelimiter(FBinPath)+'..'));
      AssignFile(TxtFile,FPCCfg);
      Append(TxtFile);
      Writeln(TxtFile,'# Fpcup[deluxe]:');
      Writeln(TxtFile,'# Adding binary tools paths to');
      Writeln(TxtFile,'# plain bin dir and architecture bin dir so');
      Writeln(TxtFile,'# fpc 3.1+ fpcres etc can be found.');
      Writeln(TxtFile,'-FD'+IncludeTrailingPathDelimiter(FBinPath)+';'+IncludeTrailingPathDelimiter(PlainBinPath));
      {$IFDEF UNIX}
      // Need to add appropriate library search path
      // where it is e.g /usr/lib/arm-linux-gnueabihf...
      Writeln(TxtFile,'# library search path');
      //Write(TxtFile,'-Fl/lib'+';'+'/usr/lib');
      Write(TxtFile,'-Fl/usr/lib/$FPCTARGET'+';'+'/usr/lib/$FPCTARGET-gnu');
      Write(TxtFile,';'+'/lib/$FPCTARGET'+';'+'/lib/$FPCTARGET-gnu');
      //Write(TxtFile,';'+'/usr/lib/'+TargetCPU+'-'+TargetOS+'-gnu');
      {$IFDEF cpuarm}
      {$IFDEF cpuarmhf}
      Write(TxtFile,';'+'/usr/lib/$FPCTARGET-gnueabihf');
      {$ELSE}
      Write(TxtFile,';'+'/usr/lib/$FPCTARGET-gnueabi');
      {$ENDIF cpuarmhf}
      {$ENDIF cpuarm}
      {$IF (defined(BSD)) and (not defined(Darwin))}
      Write(TxtFile,';'+'/usr/local/lib'+';'+'/usr/X11R6/lib');
      {$endif}
      Write(TxtFile,';'+GetGCCDirectory);
      Writeln(TxtFile);
      {$ENDIF UNIX}

      {$ifndef FPCONLY}
        {$ifdef Darwin}
          {$ifdef LCLQT5}
          Writeln(TxtFile);
          Writeln(TxtFile,'# Fpcup[deluxe]:');
          Writeln(TxtFile,'# Adding some standard paths for QT5 locations ... bit dirty');
          Writeln(TxtFile,'#IFNDEF FPC_CROSSCOMPILING');
          Writeln(TxtFile,'-Fl'+IncludeTrailingPathDelimiter(FBaseDirectory)+'Frameworks');
          Writeln(TxtFile,'-k-F'+IncludeTrailingPathDelimiter(FBaseDirectory)+'Frameworks');
          Writeln(TxtFile,'-k-rpath');
          Writeln(TxtFile,'-k@executable_path/../Frameworks');
          Writeln(TxtFile,'-k-rpath');
          Writeln(TxtFile,'-k'+IncludeTrailingPathDelimiter(FBaseDirectory)+'Frameworks');
          Writeln(TxtFile,'#ENDIF');
          Writeln(TxtFile);
          {$endif}
        {$endif}
      {$endif}

      CloseFile(TxtFile);
    end
    else
    begin
      infoln(infotext+'fpc.cfg already exists; leaving it alone.',etInfo);
    end;
  end;

  RemoveStaleBuildDirectories(FSourceDirectory,GetTargetCPU,GetTargetOS);

  if OperationSucceeded then
  begin
    WritelnLog(infotext+'Update/build succeeded.',false);
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
  CPU_OSSignature:string;
  S : string;
begin
  result := inherited;
  result:=InitModule;
  if not result then exit;

  if not DirectoryExistsUTF8(FSourceDirectory) then exit;

  oldlog:=Processor.OnErrorM; //current error handler, if any
  CrossCompiling:=(FCrossOS_Target<>'') and (FCrossCPU_Target<>'');
  if CrossCompiling then
  begin
    CPU_OSSignature:=FCrossCPU_Target+'-'+FCrossOS_Target;
    // Delete any existing buildstamp file
    Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'build-stamp.'+CPU_OSSignature);
  end else CPU_OSSignature:=GetFPCTarget(true);

  {$IFDEF MSWINDOWS}
  // Remove all fpmakes
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'utils'+DirectorySeparator+'fpmake'+GetExeExt);
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'packages'+DirectorySeparator+'fpmake'+GetExeExt);
  Sysutils.DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'ide'+DirectorySeparator+'fpmake'+GetExeExt);
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('fpmake'+GetExeExt);
    DeleteFilesSubDirs(IncludeTrailingPathDelimiter(FSourceDirectory),DeleteList,CPU_OSSignature);
  finally
    DeleteList.Free;
  end;
  {$ENDIF}

  if FileExists(FCompiler) then
  begin

    Processor.OnErrorM:=nil;  //don't want to log errors in distclean
    try
      Processor.Executable := Make;
      Processor.CurrentDirectory:=ExcludeTrailingPathDelimiter(FSourceDirectory);
      Processor.Parameters.Clear;
      if ((FCPUCount>1) AND (NOT FNoJobs)) then Processor.Parameters.Add('--jobs='+inttostr(FCPUCount));
      Processor.Parameters.Add('FPC='+FCompiler);
      Processor.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FSourceDirectory));
      Processor.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FInstallDirectory));
      {$IFDEF MSWINDOWS}
      Processor.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      Processor.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Processor.Parameters.Add('CPU_SOURCE='+GetTargetCPU);
      Processor.Parameters.Add('OS_SOURCE='+GetTargetOS);
      {$ENDIF}
      if Self is TFPCCrossInstaller then
      begin  // clean out the correct compiler
        Processor.Parameters.Add('OS_TARGET='+FCrossOS_Target);
        Processor.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
        if Length(FCrossOS_SubArch)>0 then Processor.Parameters.Add('SUBARCH='+FCrossOS_SubArch);
      end
      else
      begin
        Processor.Parameters.Add('CPU_TARGET='+GetTargetCPU);
        Processor.Parameters.Add('OS_TARGET='+GetTargetOS);
      end;
      Processor.Parameters.Add('distclean');
      if (FCrossOS_Target='') and (FCrossCPU_Target='') then
      begin
        infoln(infotext+'Running make distclean twice',etInfo);
      end
      else
      begin
        infoln(infotext+'Running make distclean twice (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+')',etInfo);
      end;
      try
        writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
        Processor.Execute;
        Sleep(100); //now do it again
        writelnlog(infotext+'Execute: '+Processor.Executable+'. Params: '+Processor.Parameters.CommaText, true);
        Processor.Execute;
      except
        on E: Exception do
        begin
          result:=false;
          WritelnLog(etError, infotext+'Running make distclean failed with an exception!'+LineEnding+'Details: '+E.Message,true);
        end;
      end;
    finally
      Processor.OnErrorM:=oldlog; //restore previous logging
    end;

  end
  else
  begin
    infoln(infotext+'Running make distclean failed: could not find compiler ('+FCompiler+')',etWarning);
  end;

  // Delete any existing fpc.cfg files
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.cfg');

  {$IFDEF UNIX}
  // Delete any fpc.sh shell scripts
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.sh');
  // Delete units
  DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'units');
  DeleteFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'lib/fpc/'+GetFPCVersion+'/units');
  {$ENDIF UNIX}

  {$IFDEF MSWINDOWS}
  // delete the units directory !!
  // this is needed due to the fact that make distclean will not cleanout this units directory
  // make distclean will only remove the results of a make, not a make install
  DeleteDirectoryEx(IncludeTrailingPathDelimiter(FSourceDirectory)+'units'+DirectorySeparator+CPU_OSSignature);
  {$ENDIF}

  // finally ... if something is still still still floating around ... delete it !!
  DeleteList := FindAllFiles(FSourceDirectory, '*.ppu; *.a; *.o', True);
  try
    if DeleteList.Count > 0 then
    begin
      for FileCounter := 0 to (DeleteList.Count-1) do
      begin
        S:=IncludeTrailingPathDelimiter(FSourceDirectory) + DeleteList.Strings[FileCounter];
        if Pos(CPU_OSSignature,S)>0 then DeleteFile(S);
      end;
    end;
  finally
    DeleteList.Free;
  end;

  result:=true;
end;

function TFPCInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=true;
end;

function TFPCInstaller.GetModule(ModuleName: string): boolean;
var
  BeforeRevision: string;
  PatchFilePath:string;
  Output: string = '';
  LocalPatchCmd : string;
  UpdateWarnings: TStringList;
  ReturnCode,i: integer;
  //MakefileSL:TStringList;
  DiffFileSL:TStringList;
  aRepoClient:TRepoClient;
begin
  result:=inherited;
  result:=InitModule;
  if not result then exit;

  {
  if FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'Makefile') then
  begin
    // try to prevent the building o the FPC IDE
    // reset makefile
    MakefileSL:=TStringList.Create;
    MakefileSL.TextLineBreakStyle:=tlbsLF;
    //DefaultTextLineBreakStyle
    //sLineBreak
    //MakefileSL.SkipLastLineBreak:=;
    try
      MakefileSL.LoadFromFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'Makefile');
      for i:=0 to Pred(MakefileSL.Count) do
      begin
        if MakefileSL.Strings[i]='# FPCUPCHANGE IDE=1' then MakefileSL.Strings[i]:='IDE=1';
      end;
      MakefileSL.SaveToFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'Makefile');
    finally
      MakefileSL.free;
    end;
  end;
  }

  // not so elegant check to see what kind of client we need ...
  if ( {(Pos('GITHUB',UpperCase(FURL))>0) OR} (Pos('.GIT',UpperCase(FURL))>0) )
     then aRepoClient:=FGitClient
     else aRepoClient:=FSVNClient;

  infoln(infotext+'Start checkout/update of ' + ModuleName + ' sources.',etInfo);

  UpdateWarnings:=TStringList.Create;
  try
   aRepoClient.Verbose:=FVerbose;
   aRepoClient.ExportOnly:=FExportOnly;
   aRepoClient.ModuleName:=ModuleName;
   if aRepoClient=FGitClient
      then result:=DownloadFromGit(ModuleName,BeforeRevision, FActualRevision, UpdateWarnings)
      else result:=DownloadFromSVN(ModuleName,BeforeRevision, FActualRevision, UpdateWarnings);
   if UpdateWarnings.Count>0 then
   begin
     WritelnLog(UpdateWarnings.Text);
   end;
  finally
    UpdateWarnings.Free;
  end;

  if NOT aRepoClient.ExportOnly then
  begin
    infoln(infotext+ModuleName + ' was at: '+BeforeRevision,etInfo);
    if FRepositoryUpdated then infoln(infotext+ModuleName + ' is now at revision: '+ActualRevision,etInfo) else
      infoln(infotext+'No updates for ' + ModuleName + ' found.',etInfo);
  end;

  {
  if result AND FileExists(IncludeTrailingPathDelimiter(FSourceDirectory)+'Makefile') then
  begin
    // try to prevent the building o the FPC IDE
    MakefileSL:=TStringList.Create;
    MakefileSL.TextLineBreakStyle:=tlbsLF;
    try
      MakefileSL.LoadFromFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'Makefile');
      for i:=0 to Pred(MakefileSL.Count) do
      begin
        if MakefileSL.Strings[i]='IDE=1' then MakefileSL.Strings[i]:='# FPCUPCHANGE IDE=1';
      end;
      MakefileSL.SaveToFile(IncludeTrailingPathDelimiter(FSourceDirectory)+'Makefile');
    finally
      MakefileSL.free;
    end;
  end;
  }

  if (NOT Result) then
    infoln(infotext+'Checkout/update of ' + ModuleName + ' sources failure.',etError);

  if result then
  begin
    if Length(FSourcePatches)>0 then
    begin
      UpdateWarnings:=TStringList.Create;
      try
        UpdateWarnings.CommaText := FSourcePatches;
        for i:=0 to (UpdateWarnings.Count-1) do
        begin
          infoln(infotext+'Trying to patch ' + ModuleName + ' with '+UpdateWarnings[i],etInfo);
          PatchFilePath:=SafeExpandFileName(UpdateWarnings[i]);
          if NOT FileExists(PatchFilePath) then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+UpdateWarnings[i]);
          if NOT FileExists(PatchFilePath) then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchfpc'+DirectorySeparator+UpdateWarnings[i]);
          if FileExists(PatchFilePath) then
          begin
            // check for default values
            if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
              {$IF defined(BSD) and not defined(DARWIN)}
              then LocalPatchCmd:=FPatchCmd + ' -p0 -N -i '
              {$else}
              then LocalPatchCmd:=FPatchCmd + ' -p0 -N --no-backup-if-mismatch -i '
              {$endif}
               else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
            {$IFDEF MSWINDOWS}
            ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + PatchFilePath, FSourceDirectory, Output, True);
            {$ELSE}
            ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + PatchFilePath, FSourceDirectory, Output, True);
            {$ENDIF}

            if ReturnCode<>0 then
            begin
              // Patching can go wrong when line endings are not compatible
              // Try to circumvent this problem by trick below (replacing line enddings)
              if Pos('different line endings',Output)>0 then
              begin
                DiffFileSL:=TStringList.Create();
                try
                  {$IFDEF MSWINDOWS}
                  DiffFileSL.TextLineBreakStyle:=tlbsLF;
                  {$ELSE}
                  DiffFileSL.TextLineBreakStyle:=tlbsCRLF;
                  {$ENDIF}
                  DiffFileSL.LoadFromFile(PatchFilePath);
                  DiffFileSL.TextLineBreakStyle:=DefaultTextLineBreakStyle;
                  DiffFileSL.SaveToFile(PatchFilePath);
                finally
                  DiffFileSL.Free();
                end;
                {$IFDEF MSWINDOWS}
                ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + PatchFilePath, FSourceDirectory, Output, True);
                {$ELSE}
                ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + PatchFilePath, FSourceDirectory, Output, True);
                {$ENDIF}
              end;
            end;

            if ReturnCode=0
               then infoln(infotext+ModuleName + ' has been patched successfully with '+UpdateWarnings[i],etInfo)
               else
               begin
                 writelnlog(etError, infotext+ModuleName+' Patching ' + ModuleName + ' with ' + UpdateWarnings[i] + ' failed.', true);
                 writelnlog(infotext+ModuleName+' patch output: ' + Output, true);
               end;
          end
          else
          begin
            infoln(infotext+'Strange: could not find patchfile '+PatchFilePath, etWarning);
            writelnlog(etError, infotext+'Patching ' + ModuleName + ' with ' + UpdateWarnings[i] + ' failed due to missing patch file.', true);
          end;
        end;
      finally
        UpdateWarnings.Free;
      end;
    end;
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
  if FileExistsUTF8(IncludeTrailingBackslash(FSourceDirectory)+'Makefile') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FSourceDirectory)+'compiler') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FSourceDirectory)+'rtl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FSourceDirectory)) then
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

  InitDone:=false;
end;

destructor TFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

