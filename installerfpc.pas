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
  FPCTRUNKVERSION='3.1.1';

  Sequences=
// convention: FPC sequences start with 'FPC'.
//standard fpc build
    'Declare FPC;'+
    'Cleanmodule FPC;'+
    // Create the link early so invalid previous
    // versions are overwritten:
    'Exec CreateFpcupScript;'+
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

//standard clean
    'Declare FPCclean;'+
    'Cleanmodule FPC;'+
    'End;'+

    'Declare FPCCleanAndBuildOnly;'+
    'Cleanmodule FPC;'+
    'Buildmodule FPC;'+
    'End;'+

//selective actions triggered with --only=SequenceName
    'Declare FPCCleanOnly;'+
    'Cleanmodule FPC;'+
    'End;'+

    'Declare FPCGetOnly;'+
    'Getmodule FPC;'+
    'End;'+

    'Declare FPCBuildOnly;'+
    'Buildmodule FPC;'+
    'End;'+

// Crosscompile build
    'Declare FPCCrossWin32-64;'+
    // Needs to be run after regular compile because of CPU/OS switch
    'SetCPU x86_64;'+
    'SetOS win64;'+
    // Getmodule has already been done
    'Cleanmodule fpc;'+
    'Buildmodule fpc;'+
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
    function GetCompilerMajorVersion(CompilerPath: string): integer;
    function GetCompilerMinorVersion(CompilerPath: string): integer;
    function GetCompilerReleaseVersion(CompilerPath: string): integer;

    function GetCompilerVersionFromUrl(aUrl: string): string;

    function GetCompilerVersionFromSource(aSourcePath: string): string;
    function GetCompilerMajorVersionFromSource(CompilerSourcePath: string): integer;
    function GetCompilerMinorVersionFromSource(CompilerSourcePath: string): integer;
    function GetCompilerReleaseVersionFromSource(CompilerSourcePath: string): integer;

    // Creates fpc proxy script that masks general fpc.cfg
    function CreateFPCScript:boolean;
    // Downloads bootstrap compiler for relevant platform, reports result.
    function DownloadBootstrapCompiler: boolean;
    // Another way to get the compiler version string
    //todo: choose either GetCompilerVersion or GetFPCVersion
    function GetFPCVersion: string;
    // internal initialisation, called from BuildModule,CleanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
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
  FileUtil, LazFileUtils
  {$IFDEF UNIX}
    ,baseunix
  {$ENDIF UNIX}
  ;
const
  SnipMagicBegin='# begin fpcup do not remove '; //look for this/add this in fpc.cfg cross-compile snippet. Note: normally followed by FPC CPU-os code
  SnipMagicEnd='# end fpcup do not remove'; //denotes end of fpc.cfg cross-compile snippet
  Win64FallBackUsingCrossCompiler=false; //Set to true to download i386 boostrap compiler and cross compile. Leave to use native win x64 compiler

function InsertFPCCFGSnippet(FPCCFG,Snippet: string): boolean;
// Adds snippet to fpc.cfg file or replaces if if first line of snippet is present
// Returns success (snippet inserted or added) or failure
var
  ConfigText: TStringList;
  i:integer;
  SnipBegin,SnipEnd: integer;
  SnippetText: TStringList;
begin
  result:=false;
  SnipBegin:=-1;
  SnipEnd:=maxint;
  ConfigText:=TStringList.Create;
  SnippetText:=TStringList.Create;
  try
    SnippetText.Text:=Snippet;
    ConfigText.LoadFromFile(FPCCFG);
    // Look for exactly this string:
    SnipBegin:=ConfigText.IndexOf(SnippetText.Strings[0]);
    if SnipBegin>-1 then
    begin
      infoln('fpc.cfg: found existing snippet in '+FPCCFG+'. Deleting it and writing new version.',etInfo);
      for i:=SnipBegin to ConfigText.Count-1 do
      begin
        // Once again, look exactly for this text:
        if ConfigText.Strings[i]=SnipMagicEnd then
        begin
          SnipEnd:=i;
          break;
        end;
      end;
      if SnipEnd=maxint then
      begin
        //apparently snippet was not closed
        infoln('fpc.cfg: existing snippet was not closed. Replacing it anyway. Please check your fpc.cfg.',etWarning);
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

    ConfigText.SaveToFile(FPCCFG);
    result:=true;
  finally
    ConfigText.Free;
    SnippetText.Free;
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
begin
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

  IntermediateCompiler:='intermediate_'+GetCompilerName(lowercase({$i %FPCTARGETCPU%}));

  result:=false; //fail by default

  CrossInstaller:=GetCrossInstaller;

  if assigned(CrossInstaller) then
    begin

    if CrossInstaller.TargetCPU='jvm' then DownloadJasmin;

    CrossInstaller.SetCrossOpt(CrossOPT); //pass on user-requested cross compile options

    if Length(CrossToolsDirectory)=0
       then result:=CrossInstaller.GetBinUtils(FBaseDirectory)
       else result:=CrossInstaller.GetBinUtils(CrossToolsDirectory);
    if not result then infoln('Failed to get crossbinutils', etError);

    if result then
    begin
      if Length(CrossLibraryDirectory)=0
         then result:=CrossInstaller.GetLibs(FBaseDirectory)
         else result:=CrossInstaller.GetLibs(CrossLibraryDirectory);
      if not result then infoln('Failed to get crosslibrary', etError)
    end;

    if result then
    begin
      result:=false;
      if CrossInstaller.CompilerUsed=ctInstalled then
        ChosenCompiler:=IncludeTrailingPathDelimiter(FBinPath)+'fpc'+GetExeExt {todo if this does not work use ppc386.exe etc}
      else //ctBootstrap
      begin
        if FileExists(ExtractFilePath(FCompiler)+IntermediateCompiler)
           then ChosenCompiler:=ExtractFilePath(FCompiler)+IntermediateCompiler
           else ChosenCompiler:=FCompiler;
      end;

      // Add binutils path to path if necessary
      OldPath:=GetPath;
      if CrossInstaller.BinUtilsPathInPath then
        begin
        SetPath(IncludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath),false,true);
        end;

      // Make all
      ProcessEx.Executable := Make;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
      ProcessEx.Parameters.Add('FPC='+ChosenCompiler);
      ProcessEx.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FBaseDirectory));
      // this installs everything in the source directory : has consequences for cleaning up !!
      ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      // Tell make where to find the target binutils if cross-compiling:
      if CrossInstaller.BinUtilsPath<>'' then
        ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
      {$IFDEF MSWINDOWS}
      ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      {$ENDIF}
      // Don't really know if this is necessary, but it can't hurt:
      // Override makefile checks that checks for stable compiler in FPC trunk
      if FBootstrapCompilerOverrideVersionCheck then
        ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
      //putting all before target might help!?!?
      ProcessEx.Parameters.Add('all');
      ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
      ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
      if Length(FCrossOS_SubArch)>0 then ProcessEx.Parameters.Add('SUBARCH='+FCrossOS_SubArch);
      Options:=FCompilerOptions;
      // Error checking for some known problems with cross compilers
      //todo: this really should go to the cross compiler unit itself but would require a rewrite
      if (CrossInstaller.TargetCPU='i8086') and
        (CrossInstaller.TargetOS='msdos') then
        begin
        if (pos('-g',Options)>0) then
          begin
          infoln('You specified these FPC options: '+Options+'... however, this cross compiler does not support debug symbols. Aborting.',etError);
          exit(false);
          end;
        end;
      if CrossInstaller.LibsPath<>''then
        Options:=Options+' -Xd -Fl'+CrossInstaller.LibsPath;

      if (CrossInstaller.TargetOS='android') then
      begin
       if (Pos('-dFPC_ARMEL',Options)=0) then Options:=Options+' -dFPC_ARMEL';
      end;

      if CrossInstaller.BinUtilsPrefix<>'' then
        begin
        // Earlier, we used regular OPT; using CROSSOPT is apparently more precise
        CrossOptions:='CROSSOPT=-XP'+CrossInstaller.BinUtilsPrefix;
        ProcessEx.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
        end;
      if (CrossInstaller.CrossOpt.Count>0) and (CrossOptions='') then
        CrossOptions:='CROSSOPT=';
      for i:=0 to CrossInstaller.CrossOpt.Count-1 do
        begin
        CrossOptions:=trimright(CrossOptions+' '+CrossInstaller.CrossOpt[i]);
        end;
      if CrossOptions<>'' then
        ProcessEx.Parameters.Add(CrossOptions);
      // suppress hints
      ProcessEx.Parameters.Add('OPT=-vi-n-h- '+Options);
      try
        if CrossOptions='' then
          infoln('Running Make all (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')',etInfo)
        else
          infoln('Running Make all (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+') with CROSSOPT: '+CrossOptions,etInfo);
        ProcessEx.Execute;
        result:=(ProcessEx.ExitStatus=0);
      except
        on E: Exception do
          begin
          WritelnLog('FPC: Running cross compiler fpc make all failed with an exception!'+LineEnding+
            'Details: '+E.Message,true);
          exit(false);
          end;
    end;

      // Return path to previous state
      if CrossInstaller.BinUtilsPathInPath then
      begin
        SetPath(OldPath,false,false);
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
          infoln('FPC: Running cross compiler fpc make all for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed with an error code. Optional module; continuing regardless.', etInfo)
        else
          infoln('FPC: Running cross compiler fpc make all for '+FCrossCPU_Target+'-'+FCrossOS_Target+' failed with an error code.',etError);
        // No use in going on, but
        // do make sure installation continues if this happened with optional crosscompiler:
        exit(result);
        end
      else
        begin
        // Install crosscompiler: make crossinstall
        // (apparently equivalent to make install CROSSINSTALL=1)
        ProcessEx.Executable := Make;
        ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
        ProcessEx.Parameters.Clear;
        infoln('Running Make crossinstall (FPC crosscompiler: '+CrossInstaller.TargetCPU+'-'+CrossInstaller.TargetOS+')', etinfo);
        if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
        ProcessEx.Parameters.Add('FPC='+ChosenCompiler);
        // this installs everything in the source directory : has consequences for cleaning up !!
        ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
        {$IFDEF UNIX}
        ProcessEx.Parameters.Add('INSTALL_BINDIR='+FBinPath);
        {$ENDIF UNIX}
        // Tell make where to find the target binutils if cross-compiling:
        if CrossInstaller.BinUtilsPath<>'' then
          ProcessEx.Parameters.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(CrossInstaller.BinUtilsPath));
        {$IFDEF MSWINDOWS}
        ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
        ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
        {$ENDIF}
        //putting crossinstall before target might help!?!?
        ProcessEx.Parameters.Add('crossinstall');
        ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target); //cross compile for different OS...
        ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target); // and processor.
        // suppress hints
        ProcessEx.Parameters.Add('OPT=-vi-n-h-');
        if Length(FCrossOS_SubArch)>0 then ProcessEx.Parameters.Add('SUBARCH='+FCrossOS_SubArch);
        if CrossInstaller.BinUtilsPrefix<>'' then
          begin
          // Earlier, we used regular OPT; using CROSSOPT is apparently more precise
          CrossOptions:='CROSSOPT=-XP'+CrossInstaller.BinUtilsPrefix;
          ProcessEx.Parameters.Add('BINUTILSPREFIX='+CrossInstaller.BinUtilsPrefix);
          end;
        if (CrossInstaller.CrossOpt.Count>0) and (CrossOptions='') then
          CrossOptions:='CROSSOPT=';
        for i:=0 to CrossInstaller.CrossOpt.Count-1 do
          begin
          CrossOptions:=trimright(CrossOptions+' '+CrossInstaller.CrossOpt[i]);
          end;
        if CrossOptions<>'' then
          ProcessEx.Parameters.Add(CrossOptions);

        try
          ProcessEx.Execute;
        except
          on E: Exception do
            begin
            WritelnLog('FPC: Running cross compiler fpc make crossinstall failed with an exception!'+LineEnding+
              'Details: '+E.Message,true);
            result:=false;
            end;
        end;

        if ProcessEx.ExitStatus<>0 then
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
            infoln('FPC: Problem installing crosscompiler for '+FCrossCPU_Target+'-'+FCrossOS_Target+'. Optional module; continuing regardless.', etInfo)
          else
            infoln('FPC: Problem installing crosscompiler for '+FCrossCPU_Target+'-'+FCrossOS_Target+'.', etError);
          FCompiler:='////\\\Error trying to compile FPC\|!';
          end
        else
          begin
          {$IFDEF UNIX}
          s:=GetCompilerName(CrossInstaller.TargetCPU);
          if FileExists(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s) then
          begin
            infoln('Copy compiler ('+s+') into: '+FBinPath,etInfo);
            FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s,
              IncludeTrailingPathDelimiter(FBinPath)+s);
            fpChmod(IncludeTrailingPathDelimiter(FBinPath)+s,&755);
          end;
          s:=GetCrossCompilerName(CrossInstaller.TargetCPU);
          if FileExists(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s) then
          begin
            infoln('Copy cross-compiler ('+s+') into: '+FBinPath,etInfo);
            FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s,
              IncludeTrailingPathDelimiter(FBinPath)+s);
            fpChmod(IncludeTrailingPathDelimiter(FBinPath)+s,&755);
          end;
          {$ENDIF}
          if CrossInstaller.FPCCFGSnippet<>'' then
            begin
            // Modify fpc.cfg
            FPCCfg := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.cfg';
            InsertFPCCFGSnippet(FPCCfg,
              SnipMagicBegin+FCrossCPU_target+'-'+FCrossOS_target+LineEnding+
              '#cross compile settings dependent on both target OS and target CPU'+LineEnding+
              '#IFDEF FPC_CROSSCOMPILING'+LineEnding+
              '#IFDEF CPU'+uppercase(FCrossCPU_Target+LineEnding)+
              '#IFDEF '+uppercase(FCrossOS_Target)+LineEnding+
              '# Inserted by fpcup '+DateTimeToStr(Now)+LineEnding+
              CrossInstaller.FPCCFGSnippet+LineEnding+
              '#ENDIF'+LineEnding+
              '#ENDIF'+LineEnding+
              '#ENDIF'+LineEnding+
              SnipMagicEnd);
            end;
        {$IFDEF UNIX}
          result:=CreateFPCScript;
        {$ENDIF UNIX}
          GetCompiler;
          end;
        end;
      end
    end
    else
      begin
      infoln('FPC: Can''t find cross installer for '+FCrossCPU_Target+'-'+FCrossOS_Target,etwarning);
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
  FileCounter:integer;
  s:string;
begin
  OperationSucceeded:=true;

  ProcessEx.Executable := Make;
  FErrorLog.Clear;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
  ProcessEx.Parameters.Add('FPC='+FCompiler);
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  // this installs everything in the source directory : has consequences for cleaning up !!
  ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  {$IFDEF UNIX}
  ProcessEx.Parameters.Add('INSTALL_BINDIR='+FBinPath);
  {$ELSE}
  ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  {$ENDIF}
  if FBootstrapCompilerOverrideVersionCheck then
    ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
  {$IFDEF UNIX}
  s:='-Sg -vi-n-h-';
  {$ELSE}
  s:='-vi-n-h-';
  {$ENDIF}
  ProcessEx.Parameters.Add('OPT='+s+' '+FCompilerOptions);
  ProcessEx.Parameters.Add('all');
  ProcessEx.Parameters.Add('install');
  infoln('Running make all install for FPC:',etInfo);
  try
    // At least on 2.7.1 we get access violations running fpc make
    // perhaps this try..except isolates that
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      begin
      OperationSucceeded := False;
      WritelnLog('FPC: Running fpc make all install failed with exit code '+inttostr(ProcessEx.ExitStatus)+LineEnding+
        'Details: '+FErrorLog.Text,true);
      end;
  except
    on E: Exception do
      begin
      OperationSucceeded := False;
      WritelnLog('FPC: Running fpc make all install failed with an exception!'+LineEnding+
        'Details: '+E.Message,true);
      end;
  end;

  {$IFDEF UNIX}
  if OperationSucceeded then
    begin
    if FVerbose then
      infoln('Creating fpc script:',etInfo)
    else
      infoln('Creating fpc script:',etDebug);
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
      WritelnLog('FPC: error: could not find compiler '+FCompiler+' that should have been created.',true);
      OperationSucceeded:=false;
      end;
    end
  else
    begin
    infoln(ModuleName+': Error trying to compile FPC.',etDebug);
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
        writelnlog('FPC: Error copying binutils: '+E.Message,true);
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
end;

function TFPCInstaller.GetCompilerVersion(CompilerPath: string): string;
var
  Output: string;
  ResultCode: longint;
begin
  Output:='';
  ResultCode:=ExecuteCommand(CompilerPath+ ' -iW', Output, FVerbose);
  Output:=StringReplace(Output,LineEnding,'',[rfReplaceAll,rfIgnoreCase]);
  Result:=Output;
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

function TFPCInstaller.GetCompilerMajorVersion(CompilerPath: string): integer;
begin
  result:=GetCompilerVersionNumber(GetCompilerVersion(CompilerPath),0);
end;
function TFPCInstaller.GetCompilerMinorVersion(CompilerPath: string): integer;
begin
  result:=GetCompilerVersionNumber(GetCompilerVersion(CompilerPath),1);
end;
function TFPCInstaller.GetCompilerReleaseVersion(CompilerPath: string): integer;
begin
  result:=GetCompilerVersionNumber(GetCompilerVersion(CompilerPath),2);
end;

function TFPCInstaller.GetCompilerVersionFromUrl(aUrl: string): string;
var
  Output: string;
begin
  if Pos('trunk',aUrl)>0 then result:=FPCTRUNKVERSION else
  begin
    // todo : process end of furl to get version !!
  end;
end;

function TFPCInstaller.GetCompilerVersionFromSource(aSourcePath: string): string;
var
  TxtFile:Text;
  version_nr:string;
  release_nr:string;
  patch_nr:string;
  s:string;
  x,y:integer;
begin

  //cheap (or expensive) coding ... but effective ... ;-)
  version_nr:='0';
  release_nr:='0';
  patch_nr:='0';

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
            break;
          end;
        end;
      end;

      if (Length(version_nr)>0) AND (Length(release_nr)>0) AND (Length(patch_nr)>0) then break;

    end;
    CloseFile(TxtFile);

  end;
  result:=version_nr+'.'+release_nr+'.'+patch_nr;
end;

function TFPCInstaller.GetCompilerMajorVersionFromSource(CompilerSourcePath: string): integer;
begin
  result:=GetCompilerVersionNumber(GetCompilerVersionFromSource(CompilerSourcePath),0);
end;
function TFPCInstaller.GetCompilerMinorVersionFromSource(CompilerSourcePath: string): integer;
begin
  result:=GetCompilerVersionNumber(GetCompilerVersionFromSource(CompilerSourcePath),1);
end;
function TFPCInstaller.GetCompilerReleaseVersionFromSource(CompilerSourcePath: string): integer;
begin
  result:=GetCompilerVersionNumber(GetCompilerVersionFromSource(CompilerSourcePath),2);
end;


function TFPCInstaller.CreateFPCScript: boolean;
  {$IFDEF UNIX}
var
  FPCScript:string;
  TxtFile:Text;
  {$ENDIF UNIX}
begin
  {$IFDEF UNIX}
  // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
  // If this fails, Lazarus compilation will fail...
  FPCScript := IncludeTrailingPathDelimiter(FBinPath) + 'fpc.sh';
  if FileExists(FPCScript) then
  begin
    infoln('fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.',etInfo);
    if not(sysutils.DeleteFile(FPCScript)) then
    begin
      infoln('Error deleting existing launcher script for FPC:'+FPCScript,eterror);
      Exit(false);
    end;
  end;
  AssignFile(TxtFile,FPCScript);
  Rewrite(TxtFile);
  writeln(TxtFile,'#!/bin/sh');
  writeln(TxtFile,'# This script starts the fpc compiler installed by fpcup');
  writeln(TxtFile,'# and ignores any system-wide fpc.cfg files');
  writeln(TxtFile,'# Note: maintained by fpcup; do not edit directly, your edits will be lost.');
  writeln(TxtFile,IncludeTrailingPathDelimiter(FBinPath),'fpc  -n @',
    IncludeTrailingPathDelimiter(FBinPath),'fpc.cfg '+
    '"$@"');
  CloseFile(TxtFile);
  Result:=(FPChmod(FPCScript,&700)=0); //Make executable; fails if file doesn't exist=>Operationsucceeded update
  if Result then
  begin
    infoln('Created launcher script for FPC:'+FPCScript,etInfo);
  end
  else
  begin
    infoln('Error creating launcher script for FPC:'+FPCScript,eterror);
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

OperationSucceeded:=true;
if OperationSucceeded then
begin
  OperationSucceeded:=ForceDirectoriesUTF8(FBootstrapCompilerDirectory);
  if OperationSucceeded=false then infoln('DownloadBootstrapCompiler error: could not create directory '+FBootstrapCompilerDirectory,eterror);
end;

BootstrapArchive := SysUtils.GetTempFileName;
if OperationSucceeded then
begin
  OperationSucceeded:=Download(FBootstrapCompilerURL, BootstrapArchive);
  if FileExists(BootstrapArchive)=false then OperationSucceeded:=false;
end;

if OperationSucceeded then
begin
  {$IFDEF MSWINDOWS}
  ArchiveDir := ExtractFilePath(BootstrapArchive);
  CompilerName:=ExtractFileName(FBootstrapCompiler);
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
  // Move CompilerName to proper directory
  if OperationSucceeded = True then
  begin
    infoln('Going to rename/move ' + ArchiveDir + CompilerName + ' to ' + FBootstrapCompiler, etinfo);
    renamefile(ArchiveDir + CompilerName, FBootstrapCompiler);
  end;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  // Extract bz2, overwriting without prompting
  if ExecuteCommand(FBunzip2+' -d -f -q '+BootstrapArchive,FVerbose) <> 0 then
    begin
      infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
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
    infoln('Going to move ' + ExtractedCompiler + ' to ' + FBootstrapCompiler,etInfo);
    OperationSucceeded:=MoveFile(ExtractedCompiler,FBootstrapCompiler);
  end;
  if OperationSucceeded then
  begin
    // Make executable
    OperationSucceeded:=(fpChmod(FBootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootstrapCompiler,etwarning);
  end;
  {$ENDIF LINUX}
  {$IFDEF BSD} //*BSD, OSX
  {$IF defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  //todo: test parameters
  //Extract bz2, overwriting without prompting
  if ExecuteCommand(FBunzip2+' -d -f -q '+BootstrapArchive,FVerbose) <> 0 then
    begin
      infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
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
    infoln('Going to move ' + ExtractedCompiler + ' to ' + FBootstrapCompiler,etInfo);
    OperationSucceeded:=MoveFile(ExtractedCompiler,FBootstrapCompiler);
  end;
  if OperationSucceeded then
  begin
    // Make executable
    OperationSucceeded:=(fpChmod(FBootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootstrapCompiler,etwarning);
  end;
  {$ENDIF defined(FREEBSD) or defined(NETBSD) or defined(OPENBSD)}
  {$IFDEF DARWIN}
  // Extract .tar.bz2, overwriting without prompting
  CompilerName:=ExtractFileName(FBootstrapCompiler);
  // GNU tar: -x -v -j -f
  // BSD tar:
  if ExecuteCommand(FTar+' -xf '+BootstrapArchive,FVerbose) <> 0 then
  begin
    infoln('Received non-zero exit code extracting bootstrap compiler. This will abort further processing.',eterror);
    OperationSucceeded := False;
  end
  else
  begin
    OperationSucceeded := True; // Spelling it out can't hurt sometimes
  end;
  // Move compiler to proper directory; note bzip2 will append .out to file
  if OperationSucceeded = True then
  begin
    // todo: currently tar spits out uncompressed file in current dir...
    // which might not have proper permissions to actually create file...!?
    infoln('Going to rename/move '+CompilerName+' to '+FBootStrapCompiler,etwarning);
    sysutils.DeleteFile(FBootStrapCompiler); //ignore errors
    // We might be moving files across partitions so we cannot use renamefile
    OperationSucceeded:=FileUtil.CopyFile(CompilerName, FBootStrapCompiler);
    sysutils.DeleteFile(CompilerName);
  end;
  if OperationSucceeded then
  begin
    // Make executable
    OperationSucceeded:=(fpChmod(FBootStrapCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+FBootStrapCompiler,eterror);
  end;
  {$ENDIF DARWIN}
  {$ENDIF BSD}
end;
if OperationSucceeded = True then
begin
  SysUtils.DeleteFile(BootstrapArchive);
end
else
begin
  infoln('Error getting/extracting bootstrap compiler. Archive: '+BootstrapArchive, eterror);
end;
Result := OperationSucceeded;
end;

function TFPCInstaller.GetFPCVersion: string;
var testcompiler:string;
begin
  testcompiler:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler'+DirectorySeparator+'ppc1';
  if not FileExistsUTF8(testcompiler) then
    begin //darwin
    testcompiler:=IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler'+DirectorySeparator+'ppc';
    end;
  ExecuteCommand(testcompiler+' -iV',result,FVerbose);
  //Remove trailing LF(s) and other control codes:
  while (length(result)>0) and (ord(result[length(result)])<$20) do
    delete(result,length(result),1);
end;

function TFPCInstaller.InitModule:boolean;
const
  // Common path used to get bootstrap compilers.
  //todo: replace when enough compilers are available via 2.6.2
  FTPPath='ftp://ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/';
  FTP262Path='ftp://ftp.freepascal.org/pub/fpc/dist/2.6.2/bootstrap/';
  FTP264Path='ftp://ftp.freepascal.org/pub/fpc/dist/2.6.4/bootstrap/';
  FTP300Path='ftp://ftp.freepascal.org/pub/fpc/dist/3.0.0/bootstrap/';
  FpcupBootsTrappersPath='https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/raw/master/bin/';

var
  BootstrapVersion: string;
  Output: string;
begin
  result:=true;
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  infoln('TFPCInstaller: initialising...',etDebug);
  if FBootstrapCompiler='' then
    begin  // may need to download it
    // We assume we're using the regular latest stable compiler, so don't
    // suggest to make to override version checks.
    // Useful if we forget to update compiler versions when a new stable is released.
    FBootstrapCompilerOverrideVersionCheck:=false;
    {$IFDEF MSWINDOWS}
    {$ifdef win64}
    if Win64FallBackUsingCrossCompiler then
      begin
      // There is no win64 bootstrap compiler, yet
      // Each time we build, we'll make our own starting with the ppc386.exe bootstrap compiler
      // This should eliminate issues with the wrong RTL etc (for trunk, only the exact same svn revision is supported)
      if FBootstrapCompilerURL='' then
         FBootstrapCompilerURL := FTP264Path+'i386-win32-ppc386.zip';
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
      if NOT FileExists(FBootstrapCompiler) then
      begin
        //FBootstrapCompiler := Which('ppc386.exe');
        //if Length(FBootstrapCompiler)=0 then
        FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
      end;
      end
      else
      begin
      // Use regular x64 stable compiler
      if FBootstrapCompilerURL='' then
      begin
        FBootstrapCompilerURL := FTP262Path+'x86_64-win64-ppcx64.zip';
        FBootstrapCompilerOverrideVersionCheck:=true;
      end;
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64.exe';
      if NOT FileExists(FBootstrapCompiler) then
      begin
        //FBootstrapCompiler := Which('ppcx64.exe');
        //if Length(FBootstrapCompiler)=0 then
        FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64.exe';
      end;
      end;
    {$ELSE}
    // Win32
    if FBootstrapCompilerURL='' then
       FBootstrapCompilerURL := FTP300Path+'i386-win32-ppc386.zip';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppc386.exe');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386.exe';
    end;
    {$endif win64}
    {$ENDIF MSWINDOWS}
    {$IFDEF Linux}
    //If compiled for x86 32 bit, install 32 bit
    //If compiled for x64, install x64 only.
    {$IFDEF CPU386}
    if FBootstrapCompilerURL='' then
      FBootstrapCompilerURL := FTP264Path+'i386-linux-ppc386.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-linux-ppc386';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppc386');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386';
    end;
    {$ELSE}
    {$IFDEF cpuarmel} //probably the 2.6.x name for arm
    if FBootstrapCompilerURL='' then
    begin
      FBootstrapCompilerURL := FTP262Path+'arm-linux-ppcarm.bz2';
      // If we're using an old compiler to build >= fpc 3.0, we need this:
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'arm-linux-ppcarm';
    {$ENDIF cpuarmel}
    {$IFDEF cpuarm} //includes armel on FPC 3.1.1
    if FBootstrapCompilerURL='' then
    begin
      FBootstrapCompilerURL := FTP262Path+'arm-linux-ppcarm.bz2';
      // If we're using an old compiler to build >= fpc 3.0, we need this:
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'arm-linux-ppcarm';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppcarm');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcarm';
    end;
    {$ELSE}
    {$IFDEF CPUAARCH64}
    infoln('TFPCInstaller: bootstrap compiler detection for linux aarch64',etWarning);
    if FBootstrapCompilerURL='' then
    begin
      //FBootstrapCompilerURL := FTP300Path+'aarch64-linux-ppca64.bz2';
      FBootstrapCompilerURL := FpcupBootsTrappersPath+'aarch64-linux/aarch64-linux-ppca64';
      // this bootstrap is 3.1.1: disable versioncheck
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'aarch64-linux-ppca64';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppca64');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppca64';
    end;
    {$ELSE}
    // Assume x64
    infoln('TFPCInstaller: bootstrap compiler detection: assuming this is a x64 processor on Linux',etWarning);
    if FBootstrapCompilerURL='' then
    begin
      FBootstrapCompilerURL := FTP262Path+'x86_64-linux-ppcx64.bz2';
      // If we're using an old compiler to build >= fpc 3.0, we need this:
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-linux-ppcx64';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppcx64');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64';
    end;
    {$ENDIF}
    {$ENDIF cpuarm}
    {$ENDIF CPU386}
    {$ENDIF Linux}
    {$IFDEF Darwin}
    //OSX
    //ppcuniversal is not a good bootstrap compiler since it creates a compiler that doesn't handle generics !?!?!?
    //We'll make our own ppc386 starting with the ppcuniversal bootstrap compiler
    //If we made it already pick it up here
    {$IFDEF CPUX86_64}
    if FBootstrapCompilerURL='' then
       FBootstrapCompilerURL := FTP300Path+'x86_64-macosx-10.7-ppcx64.tar.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppcx64');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcx64';
    end;
    {$ELSE}
    if FBootstrapCompilerURL='' then
       FBootstrapCompilerURL := FTP264Path+'universal-macosx-10.5-ppcuniversal.tar.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppc386';
    if NOT FileExists(FBootstrapCompiler) then
    begin
      //FBootstrapCompiler := Which('ppc386');
      //if Length(FBootstrapCompiler)=0 then
      FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
    end;
    {$ENDIF CPUX86_64}
    {$ENDIF Darwin}
    {$IFDEF FREEBSD}
    {$IFDEF CPU386}
    // Assuming user has FreeBSD 9...
    if FBootstrapCompilerURL='' then
       FBootstrapCompilerURL := FTP264Path+'i386-freebsd9-ppc386.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-freebsd9-ppc386';
    {$ENDIF CPU386}
    {$IFDEF CPUX86_64}
    // Assuming user has FreeBSD 9...
    if FBootstrapCompilerURL='' then
       FBootstrapCompilerURL := FTP264Path+'x86_64-freebsd9-ppcx64.bz2';
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-freebsd9.ppcx64';
    {$ENDIF CPUX86_64}
    {$ENDIF FREEBSD}
    {$IFDEF NETBSD}
    {$IFDEF CPU386}
    if FBootstrapCompilerURL='' then
    begin
      FBootstrapCompilerURL := FTP262Path+'i386-netbsd-ppc386.bz2';
      // If we're using an old compiler to build >= fpc 3.0, we need this:
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'i386-netbsd-ppc386';
    {$ENDIF CPU386}
    {$IFDEF CPUX86_64}
    if FBootstrapCompilerURL='' then
    begin
      FBootstrapCompilerURL := FTP262Path+'x86_64-netbsd-ppcx64.bz2';
      // If we're using an old compiler to build >= fpc 3.0, we need this:
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-netbsd-ppcx64';
    {$ENDIF CPUX86_64}
    {$ENDIF NETBSD}
    {$IFDEF OPENBSD}
    {$IFDEF CPU386}
    // No bootstrap compiler available
    raise Exception.Create('No bootstrap compiler available for this operating system.');
    {$ENDIF CPU386}
    {$IFDEF CPUX86_64}
    if FBootstrapCompilerURL='' then
    begin
      FBootstrapCompilerURL := FTP262Path+'x86_64-openbsd-ppcx64.bz2';
      // If we're using an old compiler to build >= fpc 3.0, we need this:
      FBootstrapCompilerOverrideVersionCheck:=true;
    end;
    FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'x86_64-openbsd-ppcx64';
    {$ENDIF CPUX86_64}
    {$ENDIF OPENBSD}
    end;

  // Only download bootstrap compiler if we can't find a valid one
  if CheckExecutable(FBootstrapCompiler, '-i', 'Free Pascal Compiler') then
    begin
      infoln('Found bootstrap compiler version '+GetCompilerVersion(FBootstrapCompiler),etInfo);
      result:=CheckAndGetNeededExecutables;
    end
    else
    begin
      {$ifdef darwin}
      // Force use of universal bootstrap compiler regardless of what user said as fpc ftp
      // doesn't have a ppc386 bootstrap. Will have to build one later in TFPCInstaller.BuildModule
      // FBootstrapCompiler := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'ppcuniversal';
      // Ensure make doesn't care if we build an i386 compiler with an old stable compiler:
      // FBootstrapCompilerOverrideVersionCheck:=true;
      {$endif darwin}
      result:=CheckAndGetNeededExecutables and DownloadBootstrapCompiler;
    end;
  if FCompiler='' then   //!!!Don't use Compiler here. GetCompiler returns installed compiler.
    FCompiler:=FBootstrapCompiler;
  WritelnLog('TFPCInstaller init:',false);
  WritelnLog('Bootstrap compiler dir: '+ExtractFilePath(FCompiler),false);
  WritelnLog('FPC URL:                '+FURL,false);
  WritelnLog('FPC options:            '+FCompilerOptions,false);
  WritelnLog('FPC directory:          '+FBaseDirectory,false);
  {$IFDEF MSWINDOWS}
  WritelnLog('Make/binutils path:     '+FMakeDir,false);
  {$ENDIF MSWINDOWS}
  FBinPath:=IncludeTrailingPathDelimiter(FBaseDirectory)+'bin'+DirectorySeparator+GetFPCTarget(true);
  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  // add fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(
    ExcludeTrailingPathDelimiter(FSVNDirectory)+PathSeparator+
    FBinPath+PathSeparator+ {compiler for current architecture}
    IncludeTrailingPathDelimiter(FBaseDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
    IncludeTrailingPathDelimiter(FBaseDirectory)+'utils'+PathSeparator+
    FMakeDir+PathSeparator+
    FBootstrapCompilerDirectory, {any missing utilities etc; put these last}
    false,false);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  //add fpc/utils to solve data2inc not found by fpcmkcfg
  SetPath(FBinPath+PathSeparator+
  IncludeTrailingPathDelimiter(FBaseDirectory)+'bin'+PathSeparator+ {e.g. fpdoc, fpcres}
  IncludeTrailingPathDelimiter(FBaseDirectory)+'utils',
  true,false);
  {$ENDIF UNIX}
  InitDone:=result;
end;


function TFPCInstaller.BuildModule(ModuleName: string): boolean;
var
  bIntermediateNeeded:boolean;
  IntermediateCompiler:string;
  FPCCfg: string;
  FPCMkCfg: string; //path+file of fpcmkcfg
  OperationSucceeded: boolean;
  PlainBinPath: string; //directory above the architecture-dependent FBinDir
  s:string;
  aOS,aCPU:string;
  TxtFile:Text;
  BootstrapDirectory :string;
  x:integer;
begin
  bIntermediateNeeded:=false;
  aCPU := lowercase({$i %FPCTARGETCPU%});
  aOS  := lowercase({$i %FPCTARGETOS%});
  IntermediateCompiler:='intermediate_'+GetCompilerName(aCPU);
  result:=InitModule;
  if not result then exit;
  infoln('TFPCInstaller: building module '+ModuleName+'...',etInfo);

  // only check trunk and/or arm for now !!
  // arm needs >= 3.0.0 for ARMHF
  // trunk needs >= 3.0.0

  infoln('We have a FPC source (@ '+FBaseDirectory+') with version: '+GetCompilerVersionFromSource(FBaseDirectory),etInfo);

  if GetCompilerMajorVersion(FCompiler)<3 then
  begin
    if (
       {$if defined(cpuarm) and defined(unix)}(True) OR {$endif}
       ((GetCompilerMajorVersionFromSource(FBaseDirectory)>=3) AND (GetCompilerMinorVersionFromSource(FBaseDirectory)>=1))
    )
    then
    begin
      if NOT FileExists(ExtractFilePath(FCompiler)+IntermediateCompiler) then
      begin
        bIntermediateNeeded:=true;
      end
      else
      begin
        if (GetCompilerMajorVersion(ExtractFilePath(FCompiler)+IntermediateCompiler)<3) then
        begin
          bIntermediateNeeded:=true;
        end
        else
        begin
          infoln('Using available intermediate compiler.',etInfo);
          FCompiler:=ExtractFilePath(FCompiler)+IntermediateCompiler;
          FBootstrapCompilerOverrideVersionCheck:=False;
        end;
      end;
    end;
  end;

  if (bIntermediateNeeded) then
  begin
    infoln('Checking out/updating FPC 3.0.0 sources for intermediate bootstrap compiler ...',etInfo);
    BootstrapDirectory := ExpandFileName(IncludeTrailingPathDelimiter(FBaseDirectory) + '..');
    BootstrapDirectory := IncludeTrailingPathDelimiter(BootstrapDirectory)+'fpc300bootstrap';
    //no 3.0 fixes allowed: only 3.0.0 is allowed for current trunk (3.1.1) see: http://lists.freepascal.org/pipermail/fpc-devel/2016-February/036645.html
    {$ifdef darwin}
    //s:='http://svn.freepascal.org/svn/fpc/branches/fixes_3_0_ios';
    {$else}
    //s:='http://svn.freepascal.org/svn/fpc/branches/fixes_3_0';
    {$endif}
    s:='http://svn.freepascal.org/svn/fpc/tags/release_3_0_0';
    ExecuteCommand(FSVNClient.RepoExecutable + ' checkout --depth=files ' + s + ' ' + BootstrapDirectory, FVerbose);
    ExecuteCommand(FSVNClient.RepoExecutable + ' update compiler ' + IncludeTrailingPathDelimiter(BootstrapDirectory)+'compiler', FVerbose);
    ExecuteCommand(FSVNClient.RepoExecutable + ' update rtl ' + IncludeTrailingPathDelimiter(BootstrapDirectory)+'rtl', FVerbose);

    infoln('Checking out/updating FPC 3.0.0 sources for intermediate bootstrap compiler done.',etInfo);

    // Build an intermediate bootstrap compiler in target fpc300bootstrap dir.
    // Version-dependent: please review and modify when new FPC version is released
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(BootstrapDirectory);

    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('distclean');
    ProcessEx.Parameters.Add('compiler_cycle');
    if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(BootstrapDirectory));
    ProcessEx.Parameters.Add('OPT=-vi-n-h-');
    ProcessEx.Parameters.Add('OS_TARGET='+aOS);
    ProcessEx.Parameters.Add('CPU_TARGET='+aCPU);

    // needed if we only have a 2.6.2 or lower bootstrapper
    // we could also build a 2.6.4 intermediate when a 2.6.2 bootstrap is encountered ... but we don't !!
    if (GetCompilerReleaseVersion(FCompiler)<4) then
       ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
    infoln('Running make cycle for intermediate compiler:',etInfo);
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      begin
      result := False;
      WritelnLog('FPC: Failed to build intermediate bootstrap compiler ',true);
      exit;
    end;
    infoln('Successfully build intermediate bootstrap compiler.',etInfo);

    FileUtil.CopyFile(IncludeTrailingPathDelimiter(BootstrapDirectory)+'compiler/'+GetCompilerName(aCPU),
      ExtractFilePath(FCompiler)+IntermediateCompiler);

    //Make executable
    {$ifdef unix}
    OperationSucceeded:=(fpChmod(ExtractFilePath(FCompiler)+IntermediateCompiler, &700)=0); //rwx------
    if OperationSucceeded=false then infoln('Intermediate bootstrap compiler: chmod failed for '+ExtractFilePath(FCompiler)+IntermediateCompiler,etError);
    {$endif}

    // Now we can change the compiler from the stable one to the one in our FPC repo:
    FCompiler:=ExtractFilePath(FCompiler)+IntermediateCompiler;
    FBootstrapCompilerOverrideVersionCheck:=False;
  end;

  {$ifdef win64}
  // Deals dynamically with either ppc386.exe or native ppcx64.exe
  if pos('ppc386.exe',FCompiler)>0 then //need to build ppcx64 before
  begin
    infoln('We have ppc386. We need ppcx64. So make it !',etInfo);
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('compiler_cycle');
    if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
    ProcessEx.Parameters.Add('OS_TARGET=win64');
    ProcessEx.Parameters.Add('CPU_TARGET=x86_64');
    ProcessEx.Parameters.Add('OPT=-vi-n-h-');
    // Override makefile checks that checks for stable compiler in FPC trunk
    if FBootstrapCompilerOverrideVersionCheck then
      ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
    infoln('Running make cycle for Windows FPC64:',etInfo);
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
    begin
      result := False;
      WritelnLog('FPC: Failed to build ppcx64 bootstrap compiler ');
      exit;
    end;
    FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler\ppcx64.exe',
      ExtractFilePath(FCompiler)+'ppcx64.exe');
    // Now we can change the compiler from the i386 to the x64 compiler:
    FCompiler:=ExtractFilePath(FCompiler)+'ppcx64.exe';
  end;
  {$endif win64}
  {$ifdef darwin}
  if pos('ppcuniversal',FCompiler)>0 then //need to build ppc386 before
  begin
    infoln('We have ppcuniversal. We need ppc386. So make it !',etInfo);
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('compiler_cycle');
    if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
    ProcessEx.Parameters.Add('CPU_TARGET=i386');
    ProcessEx.Parameters.Add('OPT=-vi-n-h-');
    // Override makefile checks that checks for stable compiler in FPC trunk
    if FBootstrapCompilerOverrideVersionCheck then
      ProcessEx.Parameters.Add('OVERRIDEVERSIONCHECK=1');
    infoln('Running make cycle for Darwin FPC i386:',etInfo);
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
    begin
      result := False;
      WritelnLog('FPC: Failed to build ppc386 bootstrap compiler ');
      exit;
    end;
    FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/ppc386',
      ExtractFilePath(FCompiler)+'ppc386');
    FCompiler:=ExtractFilePath(FCompiler)+'ppc386';
    fpChmod(FCompiler,&755);
  end;
  {$endif darwin}

  // Now: the real build of FPC !!!
  OperationSucceeded:=BuildModuleCustom(ModuleName);

  if not (OperationSucceeded) then
    infoln('Error running BuildModuleCustom for module '+ModuleName,etError);

  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
    // copy the freshly created compiler to the bin/$fpctarget directory so that
    // fpc can find it
    s:=GetCompilerName(aCPU);
    if FileExists(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s) then
    begin
      infoln('Copy compiler ('+s+') into: '+FBinPath,etInfo);
      FileUtil.CopyFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'compiler/'+s,
        IncludeTrailingPathDelimiter(FBinPath)+s);
      fpChmod(IncludeTrailingPathDelimiter(FBinPath)+s,&755);
    end;

    // create link 'units' below FBaseDirectory to
    // <somewhere>/lib/fpc/$fpcversion/units
    DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'units');
    fpSymlink(pchar(IncludeTrailingPathDelimiter(FBaseDirectory)+'lib/fpc/'+GetFPCVersion+'/units'),
      pchar(IncludeTrailingPathDelimiter(FBaseDirectory)+'units'));
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
      // Newer 3.1 trunk versions put fpcmkcfg in bin itself
      infoln(ModuleName+': did not find '+fpcmkcfg+'. Now looking in '+
        IncludeTrailingPathDelimiter(FBaseDirectory)+'bin.',etDebug);
      fpcmkcfg:=IncludeTrailingPathDelimiter(FBaseDirectory)+
        'bin'+DirectorySeparator+'fpcmkcfg'+GetExeExt;
      if not(CheckExecutable(fpcmkcfg,'-h','fpcmkcfg')) then
      begin
        infoln('Could not find fpcmkcfg in '+fpcmkcfg+'. Aborting.',etError);
        fpcmkcfg:='';
        OperationSucceeded:=false;
      end
      else
      begin
        infoln('Found valid fpcmkcfg executable: '+fpcmkcfg,etInfo);
      end;
    end;
  end;

  //todo: after fpcmkcfg create a config file for fpkpkg or something
  if OperationSucceeded then
  begin
    // Create fpc.cfg if needed
    if FileExists(FPCCfg) = False then
    begin
      ProcessEx.Executable := fpcmkcfg;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('-d');
      ProcessEx.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FBaseDirectory));
      ProcessEx.Parameters.Add('-o');
      ProcessEx.Parameters.Add('' + FPCCfg + '');
      infoln('Creating fpc.cfg:',etInfo);
      try
        ProcessEx.Execute;
      except
        on E: Exception do
          begin
          WritelnLog('FPC: Running fpcmkcfg failed with an exception!'+LineEnding+
            'Details: '+E.Message,true);
          OperationSucceeded := False;
          end;
      end;

      if ProcessEx.ExitStatus <> 0 then
        begin
        OperationSucceeded := False;
        WritelnLog('FPC: Running fpcmkcfg failed with exit code '+inttostr(ProcessEx.ExitStatus),true);
        end;

      // On *nix FPC 3.1.x, both "architecture bin" and "plain bin" may contain tools like fpcres.
      // Adding this won't hurt on Windows.
      // Adjust for that
      PlainBinPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(FBinPath)+'..');
      AssignFile(TxtFile,FPCCfg);
      Append(TxtFile);
      Writeln(TxtFile,'# fpcup:');
      Writeln(TxtFile,'# Adding binary tools paths to');
      Writeln(TxtFile,'# plain bin dir and architecture bin dir so');
      Writeln(TxtFile,'# fpc 3.1+ fpcres etc can be found.');
      Writeln(TxtFile,'-FD'+IncludeTrailingPathDelimiter(FBinPath)+';'+IncludeTrailingPathDelimiter(PlainBinPath));
      {$IFDEF UNIX}
      // Need to add appropriate library search path
      // where it is e.g /usr/lib/arm-linux-gnueabihf...
      Writeln(TxtFile,'# library search path');
      Write(TxtFile,'-Fl/usr/lib/$FPCTARGET'+';'+'/usr/lib/$FPCTARGET-gnu'+';'+GetGCCDirectory);
      {$IFDEF cpuarm}
      {$IFDEF cpuarmhf}
      Write(TxtFile,';'+'/usr/lib/$FPCTARGET-gnueabihf');
      {$ELSE}
      Write(TxtFile,';'+'/usr/lib/$FPCTARGET-gnueabi');
      {$ENDIF cpuarmhf}
      {$ENDIF cpuarm}
      Writeln;
      {$ENDIF UNIX}
      CloseFile(TxtFile);
    end
    else
    begin
      infoln('fpc.cfg already exists; leaving it alone.',etInfo);
    end;
  end;

  if OperationSucceeded then
  begin
    WritelnLog('FPC: update succeeded.',false);
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
  result:=InitModule;
  if not result then exit;
  // Check for valid basedirectory to avoid deleting in random locations or
  // hitting bug 26706: OSX TProcess.Execute fails on next call with invalid
  // current directory
  if not DirectoryExistsUTF8(FBaseDirectory) then
  begin
    infoln('FPC: CleanModule: directory '+FBaseDirectory+' does not exist. Exiting CleanModule.',etWarning);
    exit;
  end;

  oldlog:=ProcessEx.OnErrorM; //current error handler, if any
  CrossCompiling:=(FCrossOS_Target<>'') and (FCrossCPU_Target<>'');
  if CrossCompiling then
    CPU_OSSignature:=FCrossCPU_Target+'-'+FCrossOS_Target
  else
    CPU_OSSignature:=GetFPCTarget(true);

  {$IFDEF MSWINDOWS}
  // Remove all fpmakes
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('fpmake.exe');
    DeleteFilesSubDirs(IncludeTrailingPathDelimiter(FBaseDirectory),DeleteList,CPU_OSSignature);
  finally
    DeleteList.Free;
  end;

  // At least on Windows, compiling dbtestframework yourself may lead to problems compiling fpc later on,
  // so clean compiled files from both packages and test
  DeleteList:=TStringList.Create;
  try
    DeleteList.Add('.a');
    DeleteList.Add('.o');
    DeleteList.Add('.ppu'); //compiled pascal unit
    DeleteList.Add('.rst'); //delp removes .rst files
    //todo: include rsj as well?
    //DeleteList.Add('.rsj'); //javascript format resource file
    //todo: check if all these dirs are required - probably the units one is not needed
    // For some reason base has no cpu subdir - what is this used for!?!?
    // is this only done by the test framework!?!?
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'packages'+DirectorySeparator+
      'fcl-db'+DirectorySeparator+
      'src'+DirectorySeparator+
      'base',DeleteList,'');
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'packages'+DirectorySeparator+
      'fcl-db'+DirectorySeparator+
      'units',DeleteList,CPU_OSSignature);
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'packages'+DirectorySeparator+
      'fcl-db'+DirectorySeparator+
      'tests',DeleteList,'');
    //crazy experiment: also delete the db*.ppu from the units directory in case that's looked for, too
    //C:\Development\fpctrunk\units
    DeleteFilesExtensionsSubdirs(IncludeTrailingPathDelimiter(FBaseDirectory)+
      'units'+DirectorySeparator+
      CPU_OSSignature+DirectorySeparator+
      'fcl-db',DeleteList,'');
  finally
    DeleteList.Free;
  end;
  {$ENDIF}

  ProcessEx.OnErrorM:=nil;  //don't want to log errors in distclean
  try
    ProcessEx.Executable := Make;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
    ProcessEx.Parameters.Clear;
    if ((FCPUCount>1) AND (NOT FNoJobs)) then ProcessEx.Parameters.Add('--jobs='+inttostr(FCPUCount));
    ProcessEx.Parameters.Add('FPC='+FCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
    {$IFDEF MSWINDOWS}
    ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    {$ENDIF}
    if Self is TFPCCrossInstaller then
      begin  // clean out the correct compiler
      ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
      ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
      if Length(FCrossOS_SubArch)>0 then ProcessEx.Parameters.Add('SUBARCH='+FCrossOS_SubArch);
      end;
    ProcessEx.Parameters.Add('distclean');
    if (FCrossOS_Target='') and (FCrossCPU_Target='') then
      begin
      infoln('FPC: running make distclean:',etInfo);
      end
    else
      begin
      infoln('FPC: running make distclean (OS_TARGET='+FCrossOS_Target+'/CPU_TARGET='+FCrossCPU_Target+'):',etInfo);
      end;
    try
      ProcessEx.Execute;
      Sleep(100); //now do it again
      ProcessEx.Execute;
    except
      on E: Exception do
      begin
        result:=false;
        WritelnLog('FPC: running make distclean failed with an exception!'+LineEnding+
          'Details: '+E.Message,true);
      end;
    end;
  finally
    ProcessEx.OnErrorM:=oldlog; //restore previous logging
  end;

  // Delete any existing fpc.cfg files
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.cfg');

  {$IFDEF WIN64}
  // Delete possibly outdated trunk compilers used for compiling the compiler
  if Win64FallBackUsingCrossCompiler then
    begin
    // Only if we're using an i386 stable "bootstrap bootstrap" to create
    // the x64 bootstrap:
    // Delete bootstrap compiler; will be regenerated later with new version:
    infoln('TFPCInstaller: deleting bootstrap x64 compiler (will be rebuilt using x86 compiler)',etInfo);
    Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'ppcx64.exe');
    end;
  {$ENDIF WIN64}

  {$IF DEFINED(CPUARM) AND DEFINED(LINUX)}
  // Delete possibly outdated trunk compilers used for compiling the compiler
  // Delete bootstrap compiler; will be regenerated later with new version:
  infoln('TFPCInstaller: deleting intermediate bootstrap compiler (will be rebuilt using stable bootstrap compiler)',etInfo);
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'ppcarm');
  {$ENDIF} //arm/linux

  {$IFDEF UNIX}
  // Delete any fpc.sh shell scripts
  Sysutils.DeleteFile(ExtractFilePath(FCompiler)+'fpc.sh');
  // Delete units
  DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'units');
  DeleteFile(IncludeTrailingPathDelimiter(FBaseDirectory)+'lib/fpc/'+GetFPCVersion+'/units');
  {$ENDIF UNIX}

  {$IFDEF MSWINDOWS}
  // delete the units directory !!
  // this is needed due to the fact that make distclean will not cleanout this units directory
  // make distclean will only remove the results of a make, not a make install
  DeleteDirectoryEx(IncludeTrailingPathDelimiter(FBaseDirectory)+'units'+DirectorySeparator+CPU_OSSignature);
  {$ENDIF}
  // finally ... if something is still still still floating around ... delete it !!
  DeleteList := FindAllFiles(FBaseDirectory, '*.ppu; *.a; *.o', True);
  try
    if DeleteList.Count > 0 then
    begin
      for FileCounter := 0 to (DeleteList.Count-1) do
      begin
        S:=IncludeTrailingPathDelimiter(FBaseDirectory) + DeleteList.Strings[FileCounter];
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
  result:=true;
end;

function TFPCInstaller.GetModule(ModuleName: string): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  PatchFilePath:string;
  Output: string = '';
  LocalPatchCmd : string;
  UpdateWarnings: TStringList;
  ReturnCode,i: integer;
begin
  result:=InitModule;
  if not result then exit;
  infoln('Checking out/updating FPC sources...',etInfo);
  UpdateWarnings:=TStringList.Create;
  try
   FSVNClient.Verbose:=FVerbose;
   FSVNClient.ExportOnly:=FExportOnly;
   result:=DownloadFromSVN(ModuleName,BeforeRevision, AfterRevision, UpdateWarnings);
   if UpdateWarnings.Count>0 then
   begin
     WritelnLog(UpdateWarnings.Text);
   end;
  finally
    UpdateWarnings.Free;
  end;

  if NOT FSVNClient.ExportOnly then
  begin
    infoln('FPC was at: '+BeforeRevision,etInfo);
    if FRepositoryUpdated then infoln('FPC is now at: '+AfterRevision,etInfo) else
      infoln('No updates for FPC found.',etInfo);
  end;

  if result then
  begin
    if Length(FSourcePatches)>0 then
    begin
      UpdateWarnings:=TStringList.Create;
      try
        UpdateWarnings.CommaText := FSourcePatches;
        for i:=0 to (UpdateWarnings.Count-1) do
        begin
          PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+'patchfpc'+DirectorySeparator+UpdateWarnings[i]);
          if NOT FileExists(PatchFilePath) then PatchFilePath:=SafeExpandFileName(SafeGetApplicationPath+UpdateWarnings[i]);
          if FileExists(PatchFilePath) then
          begin
            // check for default values
            if ((FPatchCmd='patch') OR (FPatchCmd='gpatch'))
               then LocalPatchCmd:=FPatchCmd + ' -p0 -N --no-backup-if-mismatch -i '
               else LocalPatchCmd:=Trim(FPatchCmd) + ' ';
            {$IFDEF MSWINDOWS}
            ReturnCode:=ExecuteCommandInDir(IncludeTrailingPathDelimiter(FMakeDir) + LocalPatchCmd + PatchFilePath, FBaseDirectory, Output, True);
            {$ELSE}
            ReturnCode:=ExecuteCommandInDir(LocalPatchCmd + PatchFilePath, FBaseDirectory, Output, True);
            {$ENDIF}
            if ReturnCode=0
               then infoln('FPC has been patched successfully with '+UpdateWarnings[i],etInfo)
               else
               begin
                 writelnlog(ModuleName+' ERROR: Patching FPC with ' + UpdateWarnings[i] + ' failed.', true);
                 writelnlog(ModuleName+' patch output: ' + Output, true);
               end;
          end;
        end;
      finally
        UpdateWarnings.Free;
      end;
    end;
  end;
end;

function TFPCInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  //sanity check
  if FileExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'Makefile') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'compiler') and
    DirectoryExistsUTF8(IncludeTrailingBackslash(FBaseDirectory)+'rtl') and
    ParentDirectoryIsNotRoot(IncludeTrailingBackslash(FBaseDirectory)) then
    begin
    if DeleteDirectoryEx(FBaseDirectory)=false then
    begin
      WritelnLog('Error deleting FPC directory '+FBaseDirectory);
      result:=false;
    end
    else
    result:=true;
    end
  else
  begin
    WritelnLog('Invalid FPC directory :'+FBaseDirectory);
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

