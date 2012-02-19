{ Installer unit for FPCUp
Copyright (C) 2012 Reinier Olislagers

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
unit installer;

{
Gets/updates/compiles/installs FPC/Lazarus sources
Uses updater unit to get/update the sources.

General remarks:
- For TProcess.Params, don't use (double) quotes even though this would be required in the shell
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, updater, processutils;

type
  { TInstaller }
  TInstaller = class(TObject)
  private
    FAllOptions: string;
    FBinUtils: TStringlist; //binutils such as make.exe, as.exe, needed for compilation
    FBunzip2: string; //Location or name of bunzip2 executable
    FBootstrapCompilerDirectory: string; //Directory where bootstrap compiler is
    FBootstrapCompilerFTP: string;
    FBootstrapCompilerName: string; //OS specific compiler name (e.g. ppcuniversal for OSX)
    FClean: boolean;
    FCrossCPU_Target: string;
    FCrossOS_Target: string;
    FFPCOPT: string;
    FLazarusOPT: string;
    FOnlyModules: string;
    FShortcutName: string; //Name for shortcut/shell script pointing to newly installed Lazarus
    FExecutableExtension: string; //.exe on Windows
    FFPCPlatform: string; //Identification for platform in compiler path (e.g. i386-win32)
    FInstalledCompiler: string; //Complete path to installed FPC compiler; used to compile Lazarus
    FInstalledCompilerName: string; //Name only of installed PPC compiler (e.g. ppcx64 on 64 bit Intel OSX)
    FInstalledLazarus: string; //Path to installed Lazarus; used in creating shortcuts
    FLazarusPrimaryConfigPath: string;
    FLogFile:Text;
    FLogVerboseFile:Text;
    FMake: string;
    FShortCutNameFpcup: string;
    {$IFDEF MSWINDOWS}
    FMakeDir: string;
    {$ENDIF}
    FSkipModules: string;
    //todo: check if we shouldn't rather use FSVNExecutable, extract dir from that.
    FSVNDirectory: string; //Unpack SVN files in this directory. Actual SVN exe may be below this directory.
    FTar: string; //Location or name of tar executable
    FUpdater: TUpdater;
    FUnzip: string; //Location or name of unzip executable
    FVerbose: boolean;
    function CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
    procedure CreateBinutilsList;
    procedure CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string) ;
    procedure CreateHomeStartLink(Target, TargetArguments, ShortcutName: string);
    function DownloadBinUtils: boolean;
    function DownloadBootstrapCompiler: boolean;
    function DownloadFPCHelp(URL, TargetDirectory: string): boolean;
    function DownloadFTP(URL, TargetFile: string): boolean;
    function DownloadHTTP(URL, TargetFile: string): boolean;
    function DownloadSVN: boolean;
    procedure DumpOutput(Sender:TProcessEx; output:string);
    function CheckAndGetNeededExecutables: boolean;
    procedure EnvironmentWithOurPath(var EnvironmentList: TStringList; const NewPath: string);
    // Return complete environment except replace path with our own value
    function FindSVNSubDirs(): boolean;
    function GetBootstrapCompiler: string;
    function GetCompilerName: string;
    function GetFpcDirectory: string;
    function GetFPCRevision: string;
    function GetFPCTarget: string;
    function GetFPCVersion: string;
    function GetFPCUrl: string;
    function GetLazarusRevision: string;
    procedure LogError(Sender:TProcessEx;IsException:boolean);
    function ModuleEnabled(Name:string):boolean;
    function MoveFile(const SrcFilename, DestFilename: string): boolean;
    // Moves file if it exists, overwriting destination file
    procedure SetAllOptions(AValue: string);
    procedure SetCrossCPU_Target(AValue: string);
    procedure SetCrossOS_Target(AValue: string);
    procedure SetFPCDesiredRevision(AValue: string);
    procedure SetLazarusPrimaryConfigPath(AValue: string);
    procedure SetLazarusDesiredRevision(AValue: string);
    procedure SetOnlyModules(AValue: string);
    procedure SetShortCutNameFpcup(AValue: string);
    procedure SetSkipFPC(AValue: boolean);
    procedure SetSkipLazarus(AValue: boolean);
    procedure SetSkipLazarusHelp(AValue: boolean);
    procedure SetSkipModules(AValue: string);
    procedure SetVerbose(AValue: boolean);
    function Which(Executable: string): string; //Runs which command. Returns full path of executable, if it exists
    function GetLazarusDirectory: string;
    function GetLazarusUrl: string;
    function GetMakePath: string;
    procedure SetBootstrapCompilerDirectory(AValue: string);
    procedure SetCompilerToInstalledCompiler;
    procedure SetFPCDirectory(Directory: string);
    procedure SetFPCOPT(AValue: string);
    procedure SetFPCUrl(AValue: string);
    procedure SetLazarusDirectory(Directory: string);
    procedure SetLazarusOPT(AValue: string);
    procedure SetLazarusUrl(AValue: string);
    procedure SetMakePath(AValue: string);
    {$IFDEF UNIX}
    function XdgConfigHome: String;
    {$ENDIF UNIX}
  public
    property ShortCutName: string read FShortcutName write FShortcutName; //Name of the shortcut to Lazarus. If empty, no shortcut is generated.
    property ShortCutNameFpcup:string read FShortCutNameFpcup write SetShortCutNameFpcup;
    // Name for shortcut/shellscript linking to fpcup, useful to update without retyping options
    property CompilerName: string read GetCompilerName;
    //Name only of installed compiler
    property AllOptions:string read FAllOptions write SetAllOptions;
    property BootstrapCompiler: string read GetBootstrapCompiler;
    //Full path to FPC compiler used to compile the downloaded FPC compiler sources
    property BootstrapCompilerDirectory: string
      read FBootstrapCompilerDirectory write SetBootstrapCompilerDirectory;
    //Directory that has compiler needed to compile compiler sources. If compiler doesn't exist, it will be downloaded
    property BootstrapCompilerFTP: string read FBootstrapCompilerFTP
      write FBootstrapCompilerFTP;
    //Optional; URL from which to download bootstrap FPC compiler if it doesn't exist yet.
    property Clean: boolean read FClean write FClean;
    // Switch between cleanup (svn revert etc) and build modes
    function CleanFPC: boolean;
    // Clean up FPC environment
    function CleanLazarus: boolean;
    // Clean up Lazarus environment
    function CleanLazarusHelp: boolean;
    // Clean up help environment
    property CrossCPU_Target:string read FCrossCPU_Target write SetCrossCPU_Target;
    property CrossOS_Target:string read FCrossOS_Target write SetCrossOS_Target;
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property FPCURL: string read GetFPCUrl write SetFPCUrl; //SVN URL for FPC
    property FPCOPT: string read FFPCOPT write SetFPCOPT;
    property FPCDesiredRevision:string read GetFPCRevision write SetFPCDesiredRevision;
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    function GetLazarusHelp: boolean; //Create/get/compile Lazarus help
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    property LazarusPrimaryConfigPath: string
      read FLazarusPrimaryConfigPath write SetLazarusPrimaryConfigPath;
    //The directory where the configuration for this Lazarus instance must be stored.
    property LazarusURL: string read GetLazarusUrl write SetLazarusUrl;
    //SVN URL for Lazarus
    property LazarusOPT:string read FLazarusOPT write SetLazarusOPT;
    property LazarusDesiredRevision:string read GetLazarusRevision write SetLazarusDesiredRevision;
    property MakeDirectory: string read GetMakePath write SetMakePath;
    //Directory of make executable and other binutils. If it doesn't exist, make and binutils will be downloaded
    function Run: boolean;
    // Main entry point to the class: perform all selected actions. Returns success or failure result.
    property SkipModules:string read FSkipModules write SetSkipModules;
    property OnlyModules:string read FOnlyModules write SetOnlyModules;
    property Verbose:boolean read FVerbose write SetVerbose;
    constructor Create;
    destructor Destroy; override;
  end;

procedure infoln(Message: string);
//Uses writeln for now, and waits a bit afterwards so output is hopefully not garbled

implementation

uses
  httpsend {for downloading from http},
  ftpsend {for downloading from ftp},
  strutils, process, FileUtil {Requires LCL}
{$IFDEF MSWINDOWS}
  //Mostly for shortcut code
  ,windows, shlobj {for special folders}, ActiveX, ComObj
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
  ,baseunix
{$ENDIF UNIX}
  ,updatelazconfig
  ;

procedure infoln(Message: string);
begin
  {DEBUG conditional symbol is defined using
  Project Options/Other/Custom Options using -dDEBUG
  }
  //$IFDEF DEBUG //ignore for now
  writeln('Info: ' + Message); //we misuse this for info output
  sleep(200); //hopefully allow output to be written without interfering with other output
  //$ENDIF DEBUG
end;

{$IFDEF UNIX}
//Adapted from sysutils; Unix/Linux only
Function TInstaller.XdgConfigHome: String;
{ Follows base-dir spec,
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
begin
  Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result='') then
    Result:=IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~'))+'.config'+DirectorySeparator
  else
    Result:=IncludeTrailingPathDelimiter(Result);
end;
{$ENDIF UNIX}

{$IFDEF MSWINDOWS}
procedure TInstaller.CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string);
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of Char;
  LinkName: WideString;
begin
  { Creates an instance of IShellLink }
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  ISLink.SetPath(pChar(Target));
  ISLink.SetArguments(pChar(TargetArguments));
  ISLink.SetWorkingDirectory(pChar(ExtractFilePath(Target)));

  { Get the desktop location }
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder);
  LinkName := InFolder + PathDelim + ShortcutName+'.lnk';

  { Get rid of any existing shortcut first }
  SysUtils.DeleteFile(LinkName);

  { Create the link }
  IPFile.Save(PWChar(LinkName), false);
end;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
procedure TInstaller.CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string);
begin
  infoln('todo: implement createdesktopshortcut for '+Target+' with '+TargetArguments+' as '+Shortcutname);
end;
{$ENDIF UNIX}

procedure TInstaller.CreateHomeStartLink(Target, TargetArguments,
  ShortcutName: string);
var
  ScriptText: TStringList;
  ScriptFile: string;
begin
  {$IFDEF MSWINDOWS}
  infoln('todo: write me (CreateHomeStartLink)!');
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  //create dir if it doesn't exist
  ForceDirectoriesUTF8(ExtractFilePath(IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~'))+ShortcutName));
  ScriptText:=TStringList.Create;
  try
    // No quotes here, either, we're not in a shell, apparently...
    ScriptFile:=IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~'))+ShortcutName;
    SysUtils.DeleteFile(ScriptFile); //Get rid of any existing remnants
    ScriptText.Add('#!/bin/sh');
    ScriptText.Add('# shortcut to Lazarus trunk, generated by fcpup');
    ScriptText.Add(Target+' '+TargetArguments);
    ScriptText.SaveToFile(ScriptFile);
    FPChmod(ScriptFile, &700); //rwx------
  finally
    ScriptText.Free;
  end;
  {$ENDIF UNIX}
end;

procedure TInstaller.CreateBinutilsList;
// Windows-centric for now; doubt if it
// can be used in Unixy systems anyway
begin
  // We need FExecutableExtension to be defined first.
  FBinUtils:=TStringList.Create;
  FBinUtils.Add('GoRC'+FExecutableExtension);
  FBinUtils.Add('ar'+FExecutableExtension);
  FBinUtils.Add('as'+FExecutableExtension);
  FBinUtils.Add('bin2obj'+FExecutableExtension);
  FBinUtils.Add('cmp'+FExecutableExtension);
  FBinUtils.Add('cp'+FExecutableExtension);
  FBinUtils.Add('cpp.exe');
  FBinUtils.Add('cygiconv-2.dll');
  FBinUtils.Add('cygncurses-8.dll');
  FBinUtils.Add('cygwin1.dll');
  FBinUtils.Add('diff'+FExecutableExtension);
  FBinUtils.Add('dlltool'+FExecutableExtension);
  FBinUtils.Add('fp32.ico');
  FBinUtils.Add('gcc'+FExecutableExtension);
  FBinUtils.Add('gdate'+FExecutableExtension);
  //GDB.exe apparently can also be found here:
  //http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
  //for Windows x64:
  //http://svn.freepascal.org/svn/lazarus/binaries/x86_64-win64/gdb/bin/
  FBinUtils.Add('gdb'+FExecutableExtension);
  FBinUtils.Add('gecho'+FExecutableExtension);
  FBinUtils.Add('ginstall'+FExecutableExtension);
  FBinUtils.Add('ginstall.exe.manifest');
  FBinUtils.Add('gmkdir'+FExecutableExtension);
  FBinUtils.Add('grep'+FExecutableExtension);
  FBinUtils.Add('ld'+FExecutableExtension);
  FBinUtils.Add('libexpat-1.dll');
  FBinUtils.Add('make'+FExecutableExtension);
  FBinUtils.Add('mv'+FExecutableExtension);
  FBinUtils.Add('objdump'+FExecutableExtension);
  FBinUtils.Add('patch'+FExecutableExtension);
  FBinUtils.Add('patch.exe.manifest');
  FBinUtils.Add('pwd'+FExecutableExtension);
  FBinUtils.Add('rm'+FExecutableExtension);
  FBinUtils.Add('strip'+FExecutableExtension);
  FBinUtils.Add('unzip'+FExecutableExtension);
  //We might just use gecho for that but that would probably confuse people:
  FBinUtils.Add('upx'+FExecutableExtension);
  FBinUtils.Add('windres'+FExecutableExtension);
  FBinUtils.Add('windres'+FExecutableExtension);
  FBinUtils.Add('zip'+FExecutableExtension);
end;

function TInstaller.CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
var
  ResultCode: longint;
  OperationSucceeded: boolean;
  ExeName: string;
  Output: string;
begin
  try
    ExeName:=ExtractFileName(Executable);
    ResultCode:=ExecuteCommandHidden(Executable, Parameters, Output, Verbose);
    if ResultCode=0 then
    begin
      if (ExpectOutput<>'') and (Ansipos(ExpectOutput, Output)=0) then
      begin
        infoln('Error: '+Executable+' is not a valid '+ExeName+' application. '+
          ExeName+' exists but shows no ('+ExpectOutput+')in its output.');
        OperationSucceeded:=false
      end
      else
      begin
        OperationSucceeded:=true;
      end;
    end
    else
    begin
      infoln('Error: '+Executable+' is not a valid '+ExeName+' application ('+
      ExeName+' result code was: '+IntToStr(ResultCode)+')');
      OperationSucceeded:=false;
    end;
  except
    on E: Exception do
    begin
      infoln('Error: '+Executable+' is not a valid '+ExeName+' application ('+
        'Exception: '+E.ClassName+'/'+E.Message+')');
      OperationSucceeded := False;
    end;
  end;
  if OperationSucceeded then infoln('Found valid '+ExeName+' application.');
  Result:=OperationSucceeded;
end;

{ TInstaller }
function TInstaller.DownloadBinUtils: boolean;
// Download binutils. For now, only makes sense on Windows...
const
  {These would be the latest:
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/trunk/install/binw32/';
  These might work but are development, too (might end up in 2.6.2):
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/branches/fixes_2_6/install/binw32/';
  but let's use a stable version:}
  SourceURL = 'http://svn.freepascal.org/svn/fpcbuild/tags/release_2_6_0/install/binw32/';
  //Parent directory of files. Needs trailing backslash.
var
  Counter: integer;
begin
  ForceDirectories(MakeDirectory);
  Result := False;
  for Counter := 0 to FBinUtils.Count - 1 do
  begin
    infoln('Downloading: ' + FBinUtils[Counter] + ' into ' + MakeDirectory);
    try
      DownloadHTTP(SourceUrl + FBinUtils[Counter], MakeDirectory + FBinUtils[Counter]);
    except
      on E: Exception do
      begin
        Result := False;
        infoln('Error downloading binutils: ' + E.Message);
        exit; //out of function.
      end;
    end;
  end;
  Result := True;
end;

function TInstaller.DownloadBootstrapCompiler: boolean;
  // Should be done after we have unzip executable (on Windows: in FMakePath)
var
  ArchiveDir: string;
  BootstrapArchive: string;
  Counter: integer;
  ExtractedCompiler: string;
  Log: string;
  OperationSucceeded: boolean;
begin
  OperationSucceeded:=true;
  if OperationSucceeded then
  begin
    OperationSucceeded:=ForceDirectories(BootstrapCompilerDirectory);
    if OperationSucceeded=false then infoln('DownloadBootstrapCompiler error: could not create directory '+BootstrapCompilerDirectory);
  end;

  BootstrapArchive := SysUtils.GetTempFileName;
  ArchiveDir := ExtractFilePath(BootstrapArchive);
  if OperationSucceeded then
  begin
    OperationSucceeded:=DownloadFTP(FBootstrapCompilerFTP, BootstrapArchive);
    if FileExists(BootstrapArchive)=false then OperationSucceeded:=false;
  end;

  if OperationSucceeded then
  begin
    {$IFDEF MSWINDOWS}
    //Extract zip, overwriting without prompting
    if ExecuteCommandHidden(FUnzip,'-o -d '+ArchiveDir+' '+BootstrapArchive,Verbose) <> 0 then
      begin
        infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
        OperationSucceeded := False;
      end
      else
      begin
        OperationSucceeded := True; // Spelling it out can't hurt sometimes
      end;
    // Move CompilerName to proper directory
    if OperationSucceeded = True then
    begin
      infoln('Going to rename/move ' + ArchiveDir + CompilerName + ' to ' + BootstrapCompiler);
      renamefile(ArchiveDir + CompilerName, BootstrapCompiler);
    end;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    //Extract bz2, overwriting without prompting
    if ExecuteCommandHidden(FBunzip2,'-d -f -q '+BootstrapArchive,Verbose) <> 0 then
      begin
        infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
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
      infoln('Going to move ' + ExtractedCompiler + ' to ' + BootstrapCompiler);
      OperationSucceeded:=MoveFile(ExtractedCompiler, BootstrapCompiler);
    end;
    if OperationSucceeded then
    begin
      //Make executable
      OperationSucceeded:=(fpChmod(BootStrapCompiler, &700)=0); //rwx------
      if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+BootstrapCompiler);
    end;
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    //Extract .tar.bz2, overwriting without prompting
    Params:=TStringList.Create;
    try
      Params.Add('-x');
      Params.Add('-v');
      Params.Add('-j');
      Params.Add('-f');
      Params.Add(BootstrapArchive); // zip/archive file
      if Run(FTar, Params, '') <> 0 then
      begin
        infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
        OperationSucceeded := False;
      end
      else
      begin
        OperationSucceeded := True; // Spelling it out can't hurt sometimes
      end;
    finally
      Params.Free;
    end;
    // Move compiler to proper directory; note bzip2 will append .out to file
    if OperationSucceeded = True then
    begin
      //todo: currently tar spits out uncompressed file in current dir...
      //which might not have proper permissions to actually create file...!?
      infoln('Going to rename/move '+CompilerName+' to '+BootstrapCompiler);
      sysutils.DeleteFile(BootstrapCompiler); //ignore errors
      // We might be moving files across partitions so we cannot use renamefile
      OperationSucceeded:=FileUtil.CopyFile(CompilerName, BootstrapCompiler);
      sysutils.DeleteFile(CompilerName);
    end;
    if OperationSucceeded then
    begin
      //Make executable
      OperationSucceeded:=(fpChmod(BootStrapCompiler, &700)=0); //rwx------
      if OperationSucceeded=false then infoln('Bootstrap compiler: chmod failed for '+BootstrapCompiler);
    end;
    {$ENDIF DARWIN}
  end;
  if OperationSucceeded = True then
  begin
    SysUtils.DeleteFile(BootstrapArchive);
  end
  else
  begin
    infoln('Error getting/extracting bootstrap compiler. Archive: '+BootstrapArchive);
  end;
  Result := OperationSucceeded;
end;

function TInstaller.DownloadFPCHelp(URL, TargetDirectory: string): boolean;
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  DocsZip: string;
begin
  // Download FPC CHM docs zip into TargetDirectory.
  OperationSucceeded:=true;
  ForceDirectories(TargetDirectory);
  DocsZip := SysUtils.GetTempFileName + '.zip';
  try
    OperationSucceeded:=DownloadHTTP(URL,DocsZip);
  except
    on E: Exception do
    begin
      // Deal with timeouts, wrong URLs etc
      OperationSucceeded:=false;
      infoln('DownloadFPCHelp: HTTP download failed. URL: '+URL+LineEnding+
        'Exception: '+E.ClassName+'/'+E.Message);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite, flatten path/junk paths
    // todo: test with spaces in path
    if ExecuteCommandHidden(FUnzip,'-o -j -d '+IncludeTrailingPathDelimiter(TargetDirectory)+' '+DocsZip,Verbose)<> 0 then
    begin
      OperationSucceeded := False;
      infoln('DownloadFPCHelp: unzip failed with resultcode: '+IntToStr(ResultCode));
    end;
  end
  else
  begin
    infoln('DownloadFPCHelp: HTTP download failed. URL: '+URL);
  end;

  if OperationSucceeded then
      SysUtils.deletefile(DocsZip); //Get rid of temp zip if success.
  Result := OperationSucceeded;
end;

function TInstaller.DownloadFTP(URL, TargetFile: string): boolean;
const
  FTPPort=21;
  FTPScheme='ftp://'; //URI scheme name for FTP URLs
var
  Host: string;
  Port: integer;
  Source: string;
  FoundPos: integer;
begin
  if LeftStr(URL, length(FTPScheme))=FTPScheme then URL:=Copy(URL, length(FTPScheme)+1, length(URL));
  FoundPos:=pos('/', URL);
  Host:=LeftStr(URL, FoundPos-1);
  Source:=Copy(URL, FoundPos+1, Length(URL));
  //Check for port numbers:
  FoundPos:=pos(':', Host);
  Port:=FTPPort;
  if FoundPos>0 then
  begin
    Host:=LeftStr(Host, FoundPos-1);
    Port:=StrToIntDef(Copy(Host, FoundPos+1, Length(Host)),21);
  end;
  Result:=FtpGetFile(Host, IntToStr(Port), Source, TargetFile, 'anonymous', 'fpc@example.com');
  if result=false then infoln('DownloadFTP: error downloading '+URL+'. Details: host: '+Host+'; port: '+Inttostr(Port)+'; remote path: '+Source+' to '+TargetFile);
end;

function TInstaller.DownloadHTTP(URL, TargetFile: string): boolean;
  // Download file. If ncessary deal with SourceForge redirection, thanks to
  // Ocye: http://lazarus.freepascal.org/index.php/topic,13425.msg70575.html#msg70575
  // todo: check sourceforge redirection code: does it actually work?
const
  SourceForgeProjectPart = '//sourceforge.net/projects/';
  SourceForgeFilesPart = '/files/';
  MaxRetries = 3;
var
  Buffer: TMemoryStream;
  HTTPGetResult: boolean;
  i, j: integer;
  HTTPSender: THTTPSend;
  RetryAttempt: integer;
  SourceForgeProject: string;
begin
  Result := False;
  // Detect SourceForge download
  i := Pos(SourceForgeProjectPart, URL);
  j := Pos(SourceForgeFilesPart, URL);


  // Rewrite URL if needed for Sourceforge download redirection
  if (i > 0) and (j > 0) then
  begin
    SourceForgeProject := Copy(URL, i + Length(SourceForgeProjectPart), j);
    infoln('project is *' + SourceForgeProject + '*');
    try
      HTTPSender := THTTPSend.Create;
      while not Result do
      begin
        HTTPSender.HTTPMethod('GET', URL);
        case HTTPSender.Resultcode of
          301, 302, 307: for i := 0 to HTTPSender.Headers.Count - 1 do
              if (Pos('Location: ', HTTPSender.Headers.Strings[i]) > 0) or
                (Pos('location: ', HTTPSender.Headers.Strings[i]) > 0) then
              begin
                j := Pos('use_mirror=', HTTPSender.Headers.Strings[i]);
                if j > 0 then
                  URL :=
                    'http://' + RightStr(HTTPSender.Headers.Strings[i],
                    length(HTTPSender.Headers.Strings[i]) - j - 10) +
                    '.dl.sourceforge.net/project/' +
                    SourceForgeProject + '/' + 'DiReCtory' + 'FiLeNAMe'
                else
                  URl :=
                    StringReplace(HTTPSender.Headers.Strings[i], 'Location: ', '', []);
                HTTPSender.Clear;//httpsend
                break;
              end;
          100..200: Result := True; //No changes necessary
          500: raise Exception.Create('No internet connection available');
            //Internal Server Error ('+aURL+')');
          else
            raise Exception.Create('Download failed with error code ' +
              IntToStr(HTTPSender.ResultCode) + ' (' + HTTPSender.ResultString + ')');
        end;//case
      end;//while
      infoln('resulting url after sf redir: *' + URL + '*');
    finally
      HTTPSender.Free;
    end;
  end;

  try
    Buffer := TMemoryStream.Create;
    infoln('Going to call httpgetbinary for url: ' + URL);
    RetryAttempt := 1;
    HTTPGetResult := False;
    while ((HTTPGetResult = False) and (RetryAttempt < MaxRetries)) do
    begin
      HTTPGetResult := HttpGetBinary(URL, Buffer);
      //Application.ProcessMessages;
      Sleep(100 * RetryAttempt);
      RetryAttempt := RetryAttempt + 1;
    end;
    if HTTPGetResult = False then
      raise Exception.Create('Cannot load document from remote server');
    Buffer.Position := 0;
    if Buffer.Size = 0 then
      raise Exception.Create('Downloaded document is empty.');
    Buffer.SaveToFile(TargetFile);
    Result := True;
  finally
    FreeAndNil(Buffer);
  end;
end;

function TInstaller.DownloadSVN: boolean;
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  SVNZip: string;
begin
  // Download SVN in make path. Not required for making FPC/Lazarus, but when downloading FPC/Lazarus from... SVN ;)
  { Alternative 1: sourceforge packaged
  This won't work, we'd get an .msi:
  http://sourceforge.net/projects/win32svn/files/latest/download?source=files
  We don't want msi/Windows installer - this way we can hopefully support Windows 2000, so use:
  http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip
  }

  {Alternative 2: use
  http://www.visualsvn.com/files/Apache-Subversion-1.7.2.zip
  with subdirs bin and licenses. No further subdirs
  However, doesn't work on Windows 2K...}
  OperationSucceeded := True;
  ForceDirectories(FSVNDirectory);
  SVNZip := SysUtils.GetTempFileName + '.zip';
  try
    OperationSucceeded := DownloadHTTP(
      'http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip',
      SVNZip);
  except
    // Deal with timeouts, wrong URLs etc
    OperationSucceeded:=false;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    if ExecuteCommandHidden(FUnzip,'-o -d '+ FSVNDirectory+' '+SVNZip,Verbose)<> 0 then
      begin
        OperationSucceeded := False;
        infoln('resultcode: ' + IntToStr(ResultCode));
      end;
  end;

  if OperationSucceeded then
  begin
    OperationSucceeded := FindSVNSubDirs;
    if OperationSucceeded then
      SysUtils.deletefile(SVNZip); //Get rid of temp zip if success.
  end;
  Result := OperationSucceeded;
end;

procedure TInstaller.DumpOutput(Sender: TProcessEx; output: string);
var
  TempFileName:string;
begin
  if Verbose then
    begin
    if TextRec(FLogVerboseFile).Mode=0 then
      begin
      TempFileName:=SysUtils.GetTempFileName;
      AssignFile(FLogVerboseFile,TempFileName);
      Rewrite(FLogVerboseFile);
      Writeln(FLogFile,'Verbose output saved to ',TempFileName);
      end;
    write(FLogVerboseFile,output);
    end;
  DumpConsole(Sender,output);
end;

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  OperationSucceeded: boolean;
  Output: string;
begin
  OperationSucceeded := True;
  // The extractors used depend on the bootstrap CompilerName URL/file we download
  // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)
  {$IFDEF MSWINDOWS}
  // Need to do it here so we can pick up make path.
  FBunzip2:=EmptyStr;
  FTar:=EmptyStr;
  // By doing this, we expect unzip.exe to be in the binutils dir.
  // This is safe to do because it is included in the FPC binutils.
  FUnzip := IncludeTrailingPathDelimiter(FMakeDir) + 'unzip' + FExecutableExtension;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FBunzip2:='bunzip2';
  FTar:='tar';
  FUnzip:='unzip'; //unzip needed at least for FPC chm help
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  FBunzip2:=''; //not really necessary now
  FTar:='gnutar'; //gnutar can decompress as well; bsd tar can't
  FUnzip:='unzip'; //unzip needed at least for FPC chm help
  {$ENDIF DARIN}

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Check for binutils directory, make and unzip executables.
    // Download if needed; will download unzip - needed for SVN download
    if (DirectoryExists(FMakeDir) = False) or (FileExists(FMake) = False) or
      (FileExists(FUnzip) = False) then
    begin
      infoln('Make path ' + FMakeDir + ' doesn''t have binutils. Going to download');
      OperationSucceeded := DownloadBinUtils;
    end;
  end;
  {$ENDIF MSWINDOWS}


  if OperationSucceeded then
  begin
    // Check for proper make executable
    try
      ExecuteCommandHidden(FMake,'-v',Output,Verbose);
      if Ansipos('GNU Make', Output) = 0 then
      begin
        infoln('Found make executable but it is not GNU Make.');
        OperationSucceeded:=false;
      end;
    except
      // ignore errors, this is only an extra check
    end;
  end;

  if OperationSucceeded then
  begin
    // Try to look for SVN
    if FUpdater.FindSVNExecutable='' then
    begin
      {$IFDEF MSWINDOWS}
      // Make sure we have a sensible default.
      // Set it here so multiple calls to CheckExes will not redownload SVN all the time
      if FSVNDirectory='' then FSVNDirectory := IncludeTrailingPathDelimiter(FBootstrapCompilerDirectory)+'svn'+DirectorySeparator;
      {$ENDIF MSWINDOWS}
      FindSVNSubDirs; //Find svn in or below FSVNDirectory; will also set Updater's SVN executable
      {$IFDEF MSWINDOWS}
      // If it still can't be found, download it
      if FUpdater.SVNExecutable='' then
      begin
        infoln('Going to download SVN');
        OperationSucceeded := DownloadSVN;
      end;
      {$ELSE}
      if FUpdater.SVNExecutable='' then
      begin
        infoln('Error: could not find SVN executable. Please make sure it is installed.');
        OperationSucceeded:=false;
      end;
      {$ENDIF}
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for valid unzip executable, if it is needed
    if FUnzip<>EmptyStr then
    begin
      OperationSucceeded:=CheckExecutable(FUnzip, '-v', '');
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for valid bunzip2 executable, if it is needed
    if FBunzip2 <>EmptyStr then
    begin
      OperationSucceeded:=CheckExecutable(FBunzip2, '--version','');
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for valid tar executable, if it is needed
    if FTar<>EmptyStr then
    begin
      OperationSucceeded:=CheckExecutable(FTar, '--version','');
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for proper FPC bootstrap compiler
    infoln('Checking for FPC bootstrap compiler: '+BootStrapCompiler);
    OperationSucceeded:=CheckExecutable(BootstrapCompiler, '-h', 'Free Pascal Compiler');
    if OperationSucceeded=false then
    begin
      infoln('Bootstrap compiler not found or not a proper FPC compiler; downloading.');
      OperationSucceeded := DownloadBootstrapCompiler;
    end;
  end;
  Result := OperationSucceeded;
end;

procedure TInstaller.EnvironmentWithOurPath(
  var EnvironmentList: TStringList; const NewPath: String);
const
  {$IFDEF MSWINDOWS}
  LookFor='Path=';
  {$ELSE MSWINDOWS}
  LookFor='PATH=';
  {$ENDIF MSWINDOWS}
var
  Counter: integer;
  CustomPath: string;
  SingleLine: string;
begin
  // GetEnvironmentVariableCount is 1 based
  for Counter:=1 to GetEnvironmentVariableCount do
  begin
    // Clean up path we're given
    CustomPath:=NewPath;
    // If missing, add PATH= or path=:
    if ansipos(AnsiUpperCase(LookFor), AnsiUpperCase(CustomPath))=0 then
    begin
      CustomPath:=(LookFor+CustomPath);
    end;
    // Get rid of empty parts; don't know if required but clearer for log output
    CustomPath:=StringReplace(CustomPath, PathSeparator+PathSeparator, PathSeparator, [rfReplaceAll,rfIgnoreCase]);

    SingleLine:=GetEnvironmentString(Counter);
    if AnsiPos(LookFor, SingleLine)>0 then
    begin
      //We found the PATH variable; replace it
      EnvironmentList.Add(CustomPath);
    end
    else
    begin
      EnvironmentList.Add(SingleLine);
    end;
  end;
end;

function TInstaller.FindSVNSubDirs(): boolean;
// Looks through SVN directory and sbudirectories. Sets updater's SVNExecutable
var
  SVNFiles: TStringList;
  OperationSucceeded: boolean;
begin
  //SVNFiles:=TStringList.Create; //No, Findallfiles does that for you!?!?
  SVNFiles := FindAllFiles(FSVNDirectory, 'svn' + FExecutableExtension, True);
  try
    if SVNFiles.Count > 0 then
    begin
      // Just get first result.
      FUpdater.SVNExecutable := SVNFiles.Strings[0];
      OperationSucceeded := True;
    end
    else
    begin
      infoln('Could not find svn executable in or under ' + FSVNDirectory);
      OperationSucceeded := False;
    end;
  finally
    SVNFiles.Free;
  end;
  Result := OperationSucceeded;
end;

function TInstaller.GetBootstrapCompiler: string;
begin
  Result := BootstrapCompilerDirectory + FBootstrapCompilerName;
end;

function TInstaller.GetCompilerName: string;
begin
  // Return installed CompilerName or bootstrap CompilerName as fallback
  // Note: we can't use BootstrapCompiler property otherwise endless loop
  if FInstalledCompilerName<>EmptyStr then
    result:=FInstalledCompilerName
  else
    result:=FBootstrapCompilerName;
end;

function TInstaller.Which(Executable: string): string;
var
  Output: string;
begin
  ExecuteCommandHidden('which',Executable,Output,Verbose);
  //Remove trailing LF(s) and other control codes:
  while (length(output)>0) and (ord(output[length(output)])<$20) do
    delete(output,length(output),1);
  // We could have checked for ExecuteCommandHidden exitcode, but why not
  // do file existence check instead:
  if fileexists(Output) then
  begin
    result:=Output;
  end
  else
  begin
    result:=''; //command failed
  end;
end;

function Tinstaller.GetFpcDirectory: string;
begin
  Result := FUpdater.FPCDirectory;
end;

function TInstaller.GetFPCRevision: string;
begin
  Result := FUpdater.FPCRevision;
end;

function TInstaller.GetFPCTarget: string;
var
  processorname,os:string;
begin
  processorname:='notfound';
  os:=processorname;
  {$ifdef cpui386}
       processorname:='i386';
  {$endif cpui386}
  {$ifdef cpum68k}
       processorname:='m68k';
  {$endif cpum68k}
  {$ifdef cpualpha}
       processorname:='alpha';
  {$endif cpualpha}
  {$ifdef cpupowerpc}
       processorname:='powerpc';
  {$endif cpupowerpc}
  {$ifdef cpupowerpc64}
       processorname:='powerpc64';
  {$endif cpupowerpc64}
  {$ifdef cpuarm}
    {$ifdef fpc_armeb}
       processorname:='armeb';
    {$else}
       processorname:='arm';
    {$endif fpc_armeb}
  {$endif cpuarm}
  {$ifdef cpusparc}
       processorname:='sparc';
  {$endif cpusparc}
  {$ifdef cpux86_64}
       processorname:='x86_64';
  {$endif cpux86_64}
  {$ifdef cpuia64}
       processorname:='ia64';
  {$endif cpuia64}
  {$ifdef darwin}
       os:='darwin';
  {$endif darwin}
  {$ifdef FreeBSD}
       os:='freebsd';
  {$endif FreeBSD}
  {$ifdef linux}
       os:='linux';
  {$endif linux}
  {$ifdef netbsd}
       os:='netbsd';
  {$endif netbsd}
  {$ifdef openbsd}
       os:='openbsd';
  {$endif openbsd}
  {$ifdef os2}
       os:='os2';
  {$endif os2}
  {$ifdef solaris}
       os:='solaris';
  {$endif solaris}
  {$ifdef wince}
       os:='wince';
  {$endif wince}
  {$ifdef win32}
       os:='win32';
  {$endif win32}
  {$ifdef win64}
       os:='win64';
  {$endif win64}
  if FCrossCPU_Target<>'' then
    processorname:= FCrossCPU_Target;
  if FCrossOS_Target<>'' then
    os:=FCrossOS_Target;
  result:=processorname+'-'+os;
end;

function TInstaller.GetFPCVersion: string;
begin
  ExecuteCommandHidden(IncludeTrailingPathDelimiter(FPCDirectory)+'compiler'+DirectorySeparator+'ppc1','-iV',result,FVerbose);
  //Remove trailing LF(s) and other control codes:
  while (length(result)>0) and (ord(result[length(result)])<$20) do
    delete(result,length(result),1);
end;

function TInstaller.GetFPCUrl: string;
begin
  Result := FUpdater.FPCURL;
end;

function TInstaller.GetLazarusHelp(): boolean;
var
  AfterRevision: string;
  BeforeRevision: string;
  BuildLCLDocsDirectory: string;
  CustomPath: string;
  LazarusConfig: TUpdateLazConfig;
  OperationSucceeded: boolean;
  ProcessEx:TProcessEx;
begin
  infoln('Module LHELP: getting/compiling Lazarus help...');

  //Make sure we have the proper tools.
  OperationSucceeded := CheckAndGetNeededExecutables;

  // If we haven't installed FPC, this won't be set:
  if FInstalledCompiler = '' then
  begin
    //Assume we've got a working compiler. This will link through to the
    //platform-specific compiler, e.g. our fpc.sh proxy on Unix
    SetCompilerToInstalledCompiler;
  end;

  ProcessEx:=TProcessEx.Create(nil);
  if Verbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  ProcessEx.OnErrorM:=@LogError;

  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  CustomPath:=BootstrapCompilerDirectory+PathSeparator+
    MakeDirectory+PathSeparator+
    FSVNDirectory+PathSeparator+
    FPCDirectory+PathSeparator+
    LazarusDirectory;
  ProcessEx.Environment.SetVar('Path',CustomPath);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ProcessEx.Environment.SetVar('PATH',ExtractFilePath(FInstalledCompiler)+':'+ProcessEx.Environment.GetVar('PATH'));
  {$ENDIF UNIX}
  if CustomPath<>EmptyStr then
    writeln(FLogFile,'External program path:  '+CustomPath);

  // Location of build_lcl_docs.lpr, and also of the help files to be installed.
  BuildLCLDocsDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
      'docs'+DirectorySeparator+
      'html'+DirectorySeparator;

  if OperationSucceeded then
  begin
    // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
    ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
    ProcessEx.Parameters.Add(BuildLCLDocsDirectory+'build_lcl_docs.lpr');
    infoln('Lazarus: compiling build_lcl_docs help compiler:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    // Download FPC CHM (rtl.chm and fcl.chm) if necessary
    {Possible alternatives
    1. make chm -> requires latex!!!
    2. or
    c:\development\fpc\utils\fpdoc\fpdoc.exe --content=rtl.xct --package=rtl --descr=rtl.xml --output=rtl.chm --auto-toc --auto-index --make-searchable --css-file=C:\Development\fpc\utils\fpdoc\fpdoc.css  --format=chm
    ... but we'd need to include the input files extracted from the Make file.
    }
    infoln('Module LHELP: downloading FPC RTL/CHM help...');
    if FileExistsUTF8(BuildLCLDocsDirectory+'fcl.chm') and
    FileExistsUTF8(BuildLCLDocsDirectory+'rtl.chm') then
    begin
      infoln('Skipping download: FPC rtl.chm and fcl.chm already present in docs directory '+BuildLCLDocsDirectory);
    end
    else
    begin
    // Link to 2.6 documentation: rtl, chm, and reference manuals, including .xct files
    // http://sourceforge.net/projects/freepascal/files/Documentation/2.6.0/doc-chm.zip/download
    // which links to
    // http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip
    //
    // Note: there's also an older file on
    // http://sourceforge.net/projects/freepascal/files/Documentation/
    // that includes the lcl file
    // Download and extract zip contents into build_lcl_docs directory
    // todo: replace with main sourceforge download instead of mirror, but mirror code needs to be fixed
    OperationSucceeded:=DownloadFPCHelp('http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip',
      BuildLCLDocsDirectory);
    end;
  end;

  if OperationSucceeded then
  begin
    // Compile Lazarus CHM help

    // First remove any existing cross reference to LCL.chm; I think that might confuse fpcdoc
    DeleteFileUTF8(BuildLCLDocsDirectory+'lcl.xct');

    ProcessEx.Executable := BuildLCLDocsDirectory+'build_lcl_docs'+FExecutableExtension;
    // Make sure directory switched to that of build_lcl_docs,
    // otherwise paths to source files will not work.
    ProcessEx.CurrentDirectory:=BuildLCLDocsDirectory;
    ProcessEx.Parameters.Clear;
    // Instruct build_lcl_docs to cross-reference FPC documentation by specifying
    // the directory that contains the fcl and rtl .xct files:
    ProcessEx.Parameters.Add('--pfcdocs');
    ProcessEx.Parameters.Add(BuildLCLDocsDirectory);
    ProcessEx.Parameters.Add('--fpdoc');
    // Use the fpdoc in ./utils/fpdoc/, as the compiler directory is now different between
    // Unix+Windows
    ProcessEx.Parameters.Add(FPCDirectory+
    'utils'+DirectorySeparator+
    'fpdoc'+DirectorySeparator+
    'fpdoc'+FExecutableExtension); //fpdoc gets called by build_lcl_docs
    ProcessEx.Parameters.Add('--outfmt');
    ProcessEx.Parameters.Add('chm');
    infoln('Lazarus: compiling chm help docs:');
    { The CHM file gets output into <lazarusdir>/docs/html/lcl/lcl.chm
    Though that may work when adjusting the baseurl option in Lazarus for each
    CHM file, it's easier to move them to <lazarusdir>/docs/html,
    which is also suggested by the wiki.
    The generated .xct file is an index file for fpdoc cross file links,
    used if you want to link to the chm from other chms.}
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    if FileExistsUTF8(BuildLCLDocsDirectory+
      'lcl'+DirectorySeparator+
      'lcl.chm') then
    begin
      infoln('Lazarus: moving lcl.chm to docs directory');
      // Move help file to doc directory
      OperationSucceeded:=MoveFile(BuildLCLDocsDirectory+
        'lcl'+DirectorySeparator+
        'lcl.chm',
        BuildLCLDocsDirectory+
        'lcl.chm');
    end;
  end;

  // Finish up
  ProcessEx.Free;
  result:=OperationSucceeded;
end;

function TInstaller.Run: boolean;
var
  OperationSucceeded:boolean;
begin
  OperationSucceeded:=true;
  if Clean then
  begin
    //Clean
    if ModuleEnabled('FPC') then
    begin
      if OperationSucceeded then OperationSucceeded:=CleanFPC;
    end
    else
    begin
      infoln('FPC cleanup skipped by user.');
      writeln(FLogFile,'FPC clean skipped by user.');
    end;

    if ModuleEnabled('LAZARUS') then
    begin
      if OperationSucceeded then OperationSucceeded:=CleanLazarus;
    end
    else
    begin
      infoln('Module LAZARUS: cleanup skipped by user.');
      writeln(FLogFile,'Module LAZARUS: cleanup skipped by user.');
    end;

    if ModuleEnabled('LHELP') then
    begin
       if OperationSucceeded then OperationSucceeded:=CleanLazarusHelp;
    end
    else
    begin
      infoln('Lazarus cleanup skipped by user.');
      writeln(FLogFile,'Lazarus cleanup skipped by user.');
    end;
  end
  else
  begin
    // Build.
    // Link to fpcup itself, with all options as passed when invoking it:
    if ShortCutNameFpcup<>EmptyStr then
    begin
     {$IFDEF MSWINDOWS}
      CreateDesktopShortCut(paramstr(0),AllOptions,ShortCutNameFpcup);
     {$ELSE}
      FAllOptions:=FAllOptions+' $*';
      CreateHomeStartLink(paramstr(0),FAllOptions,ShortCutNameFpcup);
     {$ENDIF MSWINDOWS}
    end;

    if ModuleEnabled('FPC') then
    begin
      if OperationSucceeded then OperationSucceeded:=GetFPC;
    end
    else
    begin
      infoln('FPC installation/update skipped by user.');
      writeln(FLogFile,'FPC installation/update skipped by user.');
    end;

    if ModuleEnabled('LAZARUS') then
    begin
      if OperationSucceeded then OperationSucceeded:=GetLazarus;
    end
    else
    begin
      infoln('Module LAZARUS: installation/update skipped by user.');
      writeln(FLogFile,'Module LAZARUS: installation/update skipped by user.');
    end;

    if ModuleEnabled('LHELP') then
    begin
       if OperationSucceeded then OperationSucceeded:=GetLazarusHelp;
    end
    else
    begin
      infoln('Lazarus help skipped by user.');
      writeln(FLogFile,'Lazarus help skipped by user.');
    end;
  end;
  result:=OperationSucceeded;
end;

function TInstaller.GetLazarusRevision: string;
begin
  Result := FUpdater.LazarusRevision;
end;

procedure TInstaller.LogError(Sender: TProcessEx; IsException: boolean);
var
  TempFileName:string;
begin
  TempFileName:=SysUtils.GetTempFileName;
  if IsException then
    begin
    infoln('Command raised an exception: ');
    infoln(Sender.ExceptionInfo);
    WriteLn(FLogFile,'Exception raised running ',Sender.Executable + ' ' +Sender.ParametersString);
    WriteLn(FLogFile,Sender.ExceptionInfo);
    end
  else
    begin
    infoln('Command returned non-zero ExitStatus: '+IntToStr(Sender.ExitStatus)+'. Output:');
    infoln(Sender.OutputString);
    WriteLn(FLogFile,'ERROR running ',Sender.Executable + ' ' +Sender.ParametersString);
    Sender.OutputStrings.SaveToFile(TempFileName);
    WriteLn(FLogFile,'  output logged in ',TempFileName);
    end;
end;

function TInstaller.ModuleEnabled(Name: string): boolean;
begin
  result:=(((FOnlyModules='') and (FSkipModules=''))
          or ((FOnlyModules<>'') and (Pos(Name,FOnlyModules)>0)))
          or ((FSkipModules<>'') and (Pos(Name,FSkipModules)<=0))
end;

function TInstaller.MoveFile(const SrcFilename, DestFilename: string): boolean;
// We might (in theory) be moving files across partitions so we cannot use renamefile
begin
  try
    if FileExistsUTF8(SrcFileName) then
    begin
      if FileUtil.CopyFile(SrcFilename, DestFileName) then Sysutils.DeleteFile(SrcFileName);
      result:=true;
    end
    else
    begin
      //Source file does not exist, so cannot move
      result:=false;
    end;
  except
    result:=false;
  end;
end;

procedure TInstaller.SetAllOptions(AValue: string);
begin
  if FAllOptions=AValue then Exit;
  FAllOptions:=AValue;
end;

procedure TInstaller.SetCrossCPU_Target(AValue: string);
begin
  if FCrossCPU_Target=AValue then Exit;
  FCrossCPU_Target:=AValue;
end;

procedure TInstaller.SetCrossOS_Target(AValue: string);
begin
  if FCrossOS_Target=AValue then Exit;
  FCrossOS_Target:=AValue;
end;

procedure TInstaller.SetFPCDesiredRevision(AValue: string);
begin
  FUpdater.FPCRevision:=AValue;
end;

function Tinstaller.GetLazarusDirectory: string;
begin
  Result := FUpdater.LazarusDirectory;
end;

function TInstaller.GetLazarusUrl: string;
begin
  Result := FUpdater.LazarusURL;
end;


function TInstaller.GetMakePath: string;
begin
  {$IFDEF MSWINDOWS}
  Result := FMakeDir;
  {$ELSE}
  Result := ''; //dummy value, done for compatibility
  {$ENDIF MSWINDOWS}
end;

procedure TInstaller.SetCompilerToInstalledCompiler;
begin
  FInstalledCompiler:='//**#$$ Fix your ifdefs in the code.';
  // This differs between Windows and Linux.
  {$IFDEF MSWINDOWS}
  // This will give something like ppc386.exe. We use this in case
  // we need to pass PP=bla when running make.
  // We mangle this later when dealing with Lazarus config, as we require
  // fpc.exe there.
  FInstalledCompiler := FPCDirectory + 'bin' +
    DirectorySeparator + FFPCPlatform + DirectorySeparator + CompilerName;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  // Default FPC compiler installed by make:
  FInstalledCompiler := FPCDirectory + 'bin' +DirectorySeparator+GetFPCTarget+DirectorySeparator+'fpc';
  if FileExistsUTF8(FInstalledCompiler+'.sh') then
  begin
    //Use our proxy if it is installed
    FInstalledCompiler:=FInstalledCompiler+'.sh';
  end;
  {$ENDIF UNIX}
end;


procedure TInstaller.SetBootstrapCompilerDirectory(AValue: string);
begin
  FBootstrapCompilerDirectory:=IncludeTrailingPathDelimiter(ExpandFileName(AValue));
end;

procedure Tinstaller.SetFPCDirectory(Directory: string);
begin
  FUpdater.FPCDirectory := IncludeTrailingPathDelimiter(ExpandFileName(Directory));
end;

procedure TInstaller.SetFPCOPT(AValue: string);
begin
  if FFPCOPT=AValue then Exit;
  FFPCOPT:=AValue;
end;

procedure TInstaller.SetFPCUrl(AValue: string);
begin
  FUpdater.FPCURL := AValue;
end;

procedure Tinstaller.SetLazarusDirectory(Directory: string);
begin
  FUpdater.LazarusDirectory := IncludeTrailingPathDelimiter(ExpandFileName(Directory));
end;

procedure TInstaller.SetLazarusOPT(AValue: string);
begin
  if FLazarusOPT=AValue then Exit;
  FLazarusOPT:=AValue;
  end;
procedure TInstaller.SetLazarusPrimaryConfigPath(AValue: string);
const
  DefaultPCPSubdir='lazarusdevsettings'; //Include the name lazarus for easy searching Caution: shouldn't be the same name as Lazarus dir itself.
var
  AppDataPath: array[0..MaxPathLen] of char; //Allocate memory
begin
  //Directory where Lazarus installation config will end up (primary config path)
  if AValue=EmptyStr then
  begin
    {$IFDEF MSWINDOWS}
    // Somewhere in local appdata special folder
    AppDataPath := '';
    SHGetSpecialFolderPath(0, AppDataPath, CSIDL_LOCAL_APPDATA, False);
    FLazarusPrimaryConfigPath := IncludeTrailingPathDelimiter(AppDataPath)+DefaultPCPSubdir;
    {$ELSE}
    //Note: normsl GetAppConfigDir gets ~/.config/fpcup/.lazarusdev or something
    LazarusPrimaryConfigPath:=IncludeTrailingPathDelimiter(XdgConfigHome)+DefaultPCPSubdir;
    {$ENDIF MSWINDOWS}
  end
  else
  begin
    FLazarusPrimaryConfigPath:=AValue;
  end;
end;

procedure TInstaller.SetLazarusDesiredRevision(AValue: string);
begin
  FUpdater.LazarusRevision:=AValue;
end;

procedure TInstaller.SetOnlyModules(AValue: string);
begin
  if FOnlyModules=AValue then Exit;
  FOnlyModules:=Uppercase(AValue);
end;

procedure TInstaller.SetShortCutNameFpcup(AValue: string);
begin
  if FShortCutNameFpcup=AValue then Exit;
  FShortCutNameFpcup:=AValue;
end;

procedure TInstaller.SetSkipFPC(AValue: boolean);
begin

end;

procedure TInstaller.SetSkipLazarus(AValue: boolean);
begin

end;

procedure TInstaller.SetSkipLazarusHelp(AValue: boolean);
begin

end;

procedure TInstaller.SetSkipModules(AValue: string);
begin
  if FSkipModules=AValue then Exit;
  FSkipModules:=UpperCase(AValue);
end;

procedure TInstaller.SetVerbose(AValue: boolean);
begin
  FUpdater.Verbose:=AValue;
  if FVerbose=AValue then Exit;
  FVerbose:=AValue;
end;

procedure TInstaller.SetLazarusUrl(AValue: string);
begin
  FUpdater.LazarusURL := AValue;
end;


procedure TInstaller.SetMakePath(AValue: string);
begin
  {$IFDEF MSWINDOWS}
  // Make sure there's a trailing delimiter
  FMakeDir:=IncludeTrailingPathDelimiter(AValue);
  FMake:=IncludeTrailingPathDelimiter(FMakeDir)+'make'+FExecutableExtension;
  {$ELSE}
  FMake:='make'; //assume in path
  {$ENDIF MSWINDOWS}
end;

function TInstaller.CleanFPC: boolean;
var
  OperationSucceeded:boolean;
begin
  OperationSucceeded:=true;
  infoln('Module FPC: cleanup...');

  writeln(FLogFile,'Bootstrap compiler dir: '+BootstrapCompilerDirectory);
  writeln(FLogFile,'FPC URL:                '+FPCURL);
  writeln(FLogFile,'FPC options:            '+FPCOPT);
  writeln(FLogFile,'FPC directory:          '+FPCDirectory);
  {$IFDEF MSWINDOWS}
  writeln(FLogFile,'Make/binutils path:     '+MakeDirectory);
  {$ENDIF MSWINDOWS}

  try
    //Make sure we have the proper tools:
    OperationSucceeded:=CheckAndGetNeededExecutables;

    // We need to know compiler path so we can delete fpc.cfg etc
    if FInstalledCompiler = '' then
    begin
      //Assume we've got a working compiler. This will link through to the
      //platform-specific compiler, e.g. our fpc.sh proxy on Unix
      SetCompilerToInstalledCompiler;
    end;

    // SVN revert FPC directory
    FUpdater.RevertFPC;

    // Delete any existing fpc.cfg files
    Sysutils.DeleteFile(ExtractFilePath(FInstalledCompiler)+'fpc.cfg');

    {$IFDEF UNIX}
    // Delete any fpc.sh shell scripts
    Sysutils.DeleteFile(ExtractFilePath(FInstalledCompiler)+'fpc.sh');
    {$ENDIF UNIX}
  except
    on E: Exception do
    begin
      infoln('FPC clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      writeln(FLogFile, 'FPC clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      OperationSucceeded:=false;
    end;
  end;
  result:=OperationSucceeded;
end;

function TInstaller.CleanLazarus: boolean;
var
  OperationSucceeded:boolean;
begin
  OperationSucceeded:=true;
  infoln('Module LAZARUS: cleanup...');

  try
   // SVN revert Lazarus directory
   FUpdater.RevertLazarus;

   infoln('Lazarus: note: NOT cleaning primary config path '+LazarusPrimaryConfigPath+'. If you want to, you can delete it yourself.');
   writeln(FLogFile, 'Lazarus: note: NOT cleaning primary config path '+LazarusPrimaryConfigPath+'. If you want to, you can delete it yourself.');
  except
    on E: Exception do
    begin
      infoln('Lazarus clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      writeln(FLogFile, 'Lazarus clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      OperationSucceeded:=false;
    end;
  end;
  result:=OperationSucceeded;
end;

function TInstaller.CleanLazarusHelp: boolean;
var
  BuildLCLDocsDirectory:string;
  OperationSucceeded:boolean;
begin
  OperationSucceeded:=true;
  infoln('Module LHELP: cleanup...');

  { Delete .chm files and .xct (cross reference) files
    that could have been downloaded in FPC docs or created by fpcup }
  try
    BuildLCLDocsDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
       'docs'+DirectorySeparator+
       'html'+DirectorySeparator;
    sysutils.DeleteFile(BuildLCLDocsDirectory+'fcl.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'fpdoc.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'prog.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'ref.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'rtl.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'lcl.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'toc.chm');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'user.chm');
    // Cross reference (.xct) files:
    sysutils.DeleteFile(BuildLCLDocsDirectory+'fcl.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'fpdoc.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'prog.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'ref.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'rtl.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'lcl.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'toc.xct');
    sysutils.DeleteFile(BuildLCLDocsDirectory+'user.xct');
  except
    on E: Exception do
    begin
      infoln('LHELP clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      writeln(FLogFile, 'LHELP clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')');
      OperationSucceeded:=false;
    end;
  end;
  result:=true;
end;


function Tinstaller.GetFPC: boolean;
{
In this function, we try to deal with existing system wide fpc.cfg (Unix)
and the wrong compilers/binutils being in the path (Windows, mostly).

When running MAKE and other external tools:
- The --FPC= argument passed to MAKE should allow the proper compiler to be picked up.
- We switch the current directory to the makefile's directory, allowing MAKE to find
  the makefile and source files
- On Windows, we tweak the path and place our binutils directory first, allowing
  the system to find make.exe etc.; then the FPC bootstrap directory (though
  redundant: see --FPC=)
- On Unix, we tweak the path to include our FPC bootstrap directory first.

A similar effect could be gained by using --CROSSBINDIR=<makedirectory> or
OPT=-FD<makedirectory>, and use --directory=<makefiledir> but that would be
more verbose / verbose and also works when calling
other tools than make
}

var
  AfterRevision: string;
  BeforeRevision: string;
  BinPath: string; //Path where installed compiler ends up
  CustomPath: string; //Our own version of path we use to pass to commands
  FileCounter:integer;
  FPCCfg: string;
  FPCScript: string; //Used only in Unix code for now.
  UpdateWarnings: TStringList;
  OperationSucceeded: boolean;
  TxtFile:text; //Used only in Unix code for now.
  SearchRec:TSearchRec;
  FPCVersion,FPCTarget:string; //Used only in Unix code for now.
  ProcessEx:TProcessEx;
  i:integer;
  s,s2:string;
const
  COMPILERNAMES='ppc386,ppcm68k,ppcalpha,ppcpowerpc,ppcpowerpc64,ppcarm,ppcsparc,ppcia64,ppcx86_64'+
    'ppcross386,ppcrossm68k,ppcrossalpha,ppcrosspowerpc,ppcrosspowerpc64,ppcrossarm,ppcrosssparc,ppcrossia64,ppcrossx86_64';
begin
  infoln('Module FPC: Getting/compiling FPC...');

  writeln(FLogFile,'Bootstrap compiler dir: '+BootstrapCompilerDirectory);
  writeln(FLogFile,'FPC URL:                '+FPCURL);
  writeln(FLogFile,'FPC options:            '+FPCOPT);
  writeln(FLogFile,'FPC directory:          '+FPCDirectory);
  {$IFDEF MSWINDOWS}
  writeln(FLogFile,'Make/binutils path:     '+MakeDirectory);
  {$ENDIF MSWINDOWS}

  ProcessEx:=TProcessEx.Create(nil);
  if Verbose then
    ProcessEx.OnOutputM:=@DumpOutput;

  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  CustomPath:=BootstrapCompilerDirectory+PathSeparator+
    MakeDirectory+PathSeparator+
    FSVNDirectory+PathSeparator+
    FPCDirectory+PathSeparator+
    LazarusDirectory;
  ProcessEx.Environment.SetVar('Path',CustomPath);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ProcessEx.Environment.SetVar('PATH',ExtractFilePath(FInstalledCompiler)+PathSeparator+ProcessEx.Environment.GetVar('PATH'));
  {$ENDIF UNIX}
  if CustomPath<>EmptyStr then
    writeln(FLogFile,'External program path:  '+CustomPath);

  //Make sure we have the proper tools:
  OperationSucceeded:=CheckAndGetNeededExecutables;

  //Make distclean to clean out any cruft, and speed up svn update
  if OperationSucceeded then
  begin
    // Make distclean; we don't care about failure (e.g. directory might be empty etc)
    ProcessEx.Executable := FMake;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FPCDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+BootstrapCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FPCDirectory));
    ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    ProcessEx.Parameters.Add('distclean');
    infoln('FPC: running make distclean before checkout/update:');
    ProcessEx.Execute;
  end;

  ProcessEx.OnErrorM:=@LogError;  //don't want to log errors in distclean

  infoln('Checking out/updating FPC sources...');
  UpdateWarnings:=TStringList.Create;
  try
   if OperationSucceeded then OperationSucceeded:=FUpdater.UpdateFPC(BeforeRevision, AfterRevision, UpdateWarnings);
   if UpdateWarnings.Count>0 then
   begin
     infoln(UpdateWarnings.Text);
     writeln(FLogFile, UpdateWarnings.Text);
   end;
  finally
    UpdateWarnings.Free;
  end;

  infoln('FPC was at revision: '+BeforeRevision);
  if FUpdater.Updated then infoln('FPC is now at revision: '+AfterRevision) else infoln('No updates for FPC found.');

  if OperationSucceeded then
  begin
    // Make all/install, using bootstrap compiler.
    // Make all should use generated compiler internally for unit compilation
    {$IFDEF UNIX}
    // the long way: make all, see where to install, install
    ProcessEx.Executable := FMake;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FPCDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+BootstrapCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FPCDirectory));
    ProcessEx.Parameters.Add('all');
    infoln('Running make all for FPC:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
    FPCVersion:=GetFPCVersion;
    FPCTarget:=GetFPCTarget;
    BinPath:=IncludeTrailingPathDelimiter(FPCDirectory)+'bin/'+FPCTarget;
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+BootstrapCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FPCDirectory));
    ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
    ProcessEx.Parameters.Add('INSTALL_BINDIR='+BinPath);
    ProcessEx.Parameters.Add('install');
    infoln('Running make install for FPC:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
    // copy the freshly created compiler to the bin/$fpctarget directory so that
    // fpc can find it
    if FindFirst(IncludeTrailingPathDelimiter(FPCDirectory)+'compiler/ppc*',faAnyFile,SearchRec)=0 then
      repeat
        s:=SearchRec.Name;
        if (length(s)>4) and (pos(s,COMPILERNAMES) >0) then  //length(s)>4 skips ppc3
          begin
          OperationSucceeded:=OperationSucceeded and
            FileUtil.CopyFile(IncludeTrailingPathDelimiter(FPCDirectory)+'compiler/'+s,
             IncludeTrailingPathDelimiter(BinPath)+s);
          OperationSucceeded:=OperationSucceeded and
            (0=fpChmod(IncludeTrailingPathDelimiter(BinPath)+s,&755));
          end;
      until FindNext(SearchRec)<>0;
    // create link 'units' below FPCDirectory to <somewhere>/lib/fpc/$fpcversion/units
    DeleteFile(IncludeTrailingPathDelimiter(FPCDirectory)+'units');
    fpSymlink(pchar(IncludeTrailingPathDelimiter(FPCDirectory)+'lib/fpc/'+FPCVersion+'/units'),
    pchar(IncludeTrailingPathDelimiter(FPCDirectory)+'units'));
    {$ELSE UNIX}
    ProcessEx.Executable := FMake;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FPCDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+BootstrapCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FPCDirectory));
    ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
    ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    ProcessEx.Parameters.Add('all');
    ProcessEx.Parameters.Add('install');
    infoln('Running make all install for FPC:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
      OperationSucceeded := False;
    {$ENDIF UNIX}
  end;

  // Let everyone know of our shiny new CompilerName:
  if OperationSucceeded then
  begin
    SetCompilerToInstalledCompiler;
  end
  else
  begin
    FInstalledCompiler:='////\\\Error trying to compile FPC\|!';
  end;

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    //Copy over binutils to new CompilerName bin directory
    try
      for FileCounter:=0 to FBinUtils.Count-1 do
      begin
        FileUtil.CopyFile(FMakeDir+FBinUtils[FileCounter], ExtractFilePath(FInstalledCompiler)+FBinUtils[FileCounter]);
      end;
      // Also, we can change the make/binutils path to our new environment
      // Will modify fmake as well.
      MakeDirectory:=ExtractFilePath(FInstalledCompiler);
    except
      on E: Exception do
      begin
        infoln('Error copying binutils: '+E.Message);
        OperationSucceeded:=false;
      end;
    end;
  end;
  {$ENDIF MSWINDOWS}

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
    if not ModuleEnabled('WINCROSSX64') then
    begin
      OperationSucceeded:=true;  //continue with whatever we do next
      infoln('Module WINCROSSX64: skipped by user.');
      writeln(FLogFile,'Module WINCROSSX64: skipped by user.');
    end
    else
    begin
      // Make crosscompiler using new compiler
      // todo: check out what cross compilers we can install on Linux/OSX (win32?)
      // todo: possibly move this to a separate section that will be called after fpc compilation (reason: we need a valid compiler0.
      // Note: consider this as an optional item, so don't fail the function if this breaks.
      ProcessEx.Executable := FMake;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FPCDirectory);
      ProcessEx.Parameters.Clear;
      infoln('Running Make all (FPC crosscompiler):');
      //Note: make install+make crossinstall work on command line
      //set path=c:\development\fpc\bin\i386-win32;c:\development\fpcbootstrap
      //make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo all OS_TARGET=win64 CPU_TARGET=x86_64
      // => already gives compiler\ppcrossx64.exe, compiler\ppcx64.exe
      //make FPC=c:\development\fpc\bin\i386-win32\fpc.exe --directory=c:\development\fpc INSTALL_PREFIX=c:\development\fpc UPXPROG=echo COPYTREE=echo crossinstall OS_TARGET=win64 CPU_TARGET=x86_64
      // => gives bin\i386-win32\ppcrossx64.exe
      //but not in this program..
      ProcessEx.Parameters.Add('FPC='+FInstalledCompiler+'');
      ProcessEx.Parameters.Add('--directory='+ ExcludeTrailingPathDelimiter(FPCDirectory));
      ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
      ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      //putting all before target might help!?!?
      ProcessEx.Parameters.Add('all');
      ProcessEx.Parameters.Add('OS_TARGET=win64');
      ProcessEx.Parameters.Add('CPU_TARGET=x86_64');
      if FFPCOPT<>'' then
        ProcessEx.Parameters.Add('OPT='+FFPCOPT);
      ProcessEx.Execute;

      if ProcessEx.ExitStatus = 0 then
      begin
        // Install crosscompiler using new CompilerName - todo: only for Windows!?!?
        // make all and make crossinstall perhaps equivalent to
        // make all install CROSSCOMPILE=1??? todo: find out
        ProcessEx.Executable := FMake;
        ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FPCDirectory);
        infoln('Running Make crossinstall for FPC:');
        ProcessEx.Parameters.Clear;
        ProcessEx.Parameters.Add('FPC='+FInstalledCompiler+'');
        ProcessEx.Parameters.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
        ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
        ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
        //putting crossinstall before target might help!?!?
        ProcessEx.Parameters.Add('crossinstall');
        ProcessEx.Parameters.Add('OS_TARGET=win64'); //cross compile for different OS...
        ProcessEx.Parameters.Add('CPU_TARGET=x86_64'); // and processor.

        // Note: consider this as an optional item, so don't fail the function if this breaks.
        ProcessEx.Execute;
        if ProcessEx.ExitStatus<>0 then
        begin
          infoln('Problem compiling/installing crosscompiler. Continuing regardless.');
        end;
      end;
    end;
  {$ENDIF MSWINDOWS}

  //todo: after fpcmkcfg create a config file for fpkpkg or something
  if OperationSucceeded then
  begin
    // Create fpc.cfg if needed
    BinPath := ExtractFilePath(FInstalledCompiler);
    FPCCfg := IncludeTrailingPathDelimiter(BinPath) + 'fpc.cfg';
    if FileExists(FPCCfg) = False then
    begin
      ProcessEx.Executable := BinPath + 'fpcmkcfg';
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FPCDirectory);
      ProcessEx.Parameters.clear;
      ProcessEx.Parameters.Add('-d');
      ProcessEx.Parameters.Add('basepath='+ExcludeTrailingPathDelimiter(FPCDirectory));
      ProcessEx.Parameters.Add('-o');
      ProcessEx.Parameters.Add('' + FPCCfg + '');
      infoln('Creating fpc.cfg:');
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
        OperationSucceeded := False;
    {$IFDEF UNIX}
    {$IFDEF cpuarmel}
      // Need to add multiarch library search path
      AssignFile(TxtFile,FPCCfg);
      Append(TxtFile);
      Writeln(TxtFile,'# multiarch library search path');
      Writeln(TxtFile,'-Fl/usr/lib/$fpctarget-*');
      CloseFile(TxtFile);
    {$ENDIF armelcpu}
    {$ENDIF UNIX}
    end
    else
    begin
      infoln('fpc.cfg already exists; leaving it alone.');
    end;
  end;

  {$IFDEF UNIX}
  if OperationSucceeded then
  begin
    // If needed, create fpc.sh, a launcher to fpc that ignores any existing system-wide fpc.cfgs (e.g. /etc/fpc.cfg)
    // If this fails, Lazarus compilation will fail...
    BinPath := ExtractFilePath(FInstalledCompiler);
    FPCScript := IncludeTrailingPathDelimiter(BinPath) + 'fpc.sh';
    if FileExists(FPCScript) then
    begin
      infoln('fpc.sh launcher script already exists ('+FPCScript+'); trying to overwrite it.');
      sysutils.DeleteFile(FPCScript);
    end;
    AssignFile(TxtFile,FPCScript);
    Rewrite(TxtFile);
    writeln(TxtFile,'#!/bin/sh');
    writeln(TxtFile,'# This script starts the fpc compiler installed by fpcup');
    writeln(TxtFile,'# and ignores any system-wide fpc.cfg files');
    writeln(TxtFile,'# Note: maintained by fpcup; do not edit directly, your edits will be lost.');
    writeln(TxtFile,IncludeTrailingPathDelimiter(BinPath),'fpc  -n @',
         IncludeTrailingPathDelimiter(BinPath),'fpc.cfg -FD'+
         IncludeTrailingPathDelimiter(BinPath)+' $*');
    CloseFile(TxtFile);
    OperationSucceeded:=(FPChmod(FPCScript,&700)=0); //Make executable; fails if file doesn't exist=>Operationsucceeded update
    if OperationSucceeded then
    begin
      infoln('Created launcher script for FPC:'+FPCScript);
    end
    else
    begin
      infoln('Error creating launcher script for FPC:'+FPCScript);
    end;
  end;
  {$ENDIF UNIX}
  if OperationSucceeded then
    writeln(FLogFile,'FPC: update succeeded at revision number ', AfterRevision);
  ProcessEx.Free;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
{
This function does depend on a properly installed FPC but does not
check if there is one.
Assumptions: binutils in FPC directory or in path.

In this function, we try to deal with existing system wide fpc.cfg (Unix)
and the wrong compilers/binutils being in the path (Windows, mostly).

When running MAKE and other external tools:
- The --FPC= argument passed to MAKE should allow the proper compiler to be picked up.
- We switch the current directory to the makefile's (Lazarus) directory, allowing MAKE to find
  the makefile and source files.
  If using lazbuild to build .lprs, we switch the directory to the project directory.
- On Windows, we tweak the path and place our custom FPC compiler directory first.
  The binutils (make.exe etc) should also be found there.
  Then the Lazarus directory, for any utilities that Lazarus may place there.
  Then the make/binutils and subversion client directories.
  Finally the FPC bootstrap directory, as a backup.
- On Unix, we tweak the path to include our custom FPC compiler directory first.

A similar effect could be gained by using --CROSSBINDIR=<makedirectory> or
OPT=-FD<makedirectory>, and use --directory=<makefiledir> but that would be
more verbose / verbose and also works when calling
other tools than make
}
var
  AfterRevision: string;
  BeforeRevision: string;
  CustomPath: string;
  LazarusConfig: TUpdateLazConfig;
  UpdateWarnings:TStringList;
  OperationSucceeded: boolean;
  ProcessEx:TProcessEx;
begin
  infoln('Module LAZARUS: Getting/compiling Lazarus...');

  writeln(FLogFile,'Lazarus directory:      '+LazarusDirectory);
  writeln(FLogFile,'Lazarus primary config path:',LazarusPrimaryConfigPath);
  writeln(FLogFile,'Lazarus URL:            '+LazarusURL);
  writeln(FLogFile,'Lazarus options:        '+LazarusOPT);
  writeln(FLogFile,'Lazarus shortcut name:  '+ShortCutName);
  if ShortCutNameFpcup<>'' then
    writeln(FLogFile,'Shortcut fpcup name:    '+ShortCutNameFpcup);
  //Make sure we have the proper tools.
  OperationSucceeded := CheckAndGetNeededExecutables;

  // If we haven't installed FPC, this won't be set:
  if FInstalledCompiler = '' then
  begin
    //Assume we've got a working compiler. This will link through to the
    //platform-specific compiler, e.g. our fpc.sh proxy on Unix
    SetCompilerToInstalledCompiler;
  end;

  ProcessEx:=TProcessEx.Create(nil);
  if Verbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  ProcessEx.OnErrorM:=@LogError;

  {$IFDEF MSWINDOWS}
  // Try to ignore any existing make.exe, fpc.exe by setting our own path:
  // We include the bootstrap compiler directory, but that fpc.exe will
  // probably give different .ppu files=>more a last resort solution
  CustomPath:=ExtractFilePath(FInstalledCompiler)+PathSeparator+
    LazarusDirectory+PathSeparator+
    MakeDirectory+PathSeparator+
    FSVNDirectory+PathSeparator+
    BootstrapCompilerDirectory;
  ProcessEx.Environment.SetVar('Path',CustomPath);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ProcessEx.Environment.SetVar('PATH',ExtractFilePath(FInstalledCompiler)+PathSeparator+ProcessEx.Environment.GetVar('PATH'));
  {$ENDIF UNIX}
  if CustomPath<>EmptyStr then
    writeln(FLogFile,'External program path:  '+CustomPath);

  // Make distclean to clean out any cruft, and speed up svn update
  if OperationSucceeded then
  begin
    // Make distclean; we don't care about failure (e.g. directory might be empty etc)
    ProcessEx.Executable := FMake;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(LazarusDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+FInstalledCompiler+'');
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(LazarusDirectory));
    ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    ProcessEx.Parameters.Add('distclean');
    infoln('Lazarus: running make distclean before checkout/update:');
    ProcessEx.Execute;
  end;

  // Download Lazarus source:
  if OperationSucceeded = True then
  begin
    infoln('Checking out/updating Lazarus sources...');
    UpdateWarnings:=TStringList.Create;
    try
     if OperationSucceeded then OperationSucceeded:=FUpdater.UpdateLazarus(BeforeRevision, AfterRevision, UpdateWarnings);
     if UpdateWarnings.Count>0 then
     begin
       infoln(UpdateWarnings.Text);
       writeln(FLogFile, UpdateWarnings.Text);
     end;
    finally
      UpdateWarnings.Free;
    end;

    infoln('Lazarus was at revision: '+BeforeRevision);
    if FUpdater.Updated then infoln('Lazarus is now at revision: '+AfterRevision) else infoln('No updates for Lazarus found.');
  end;

  // Make sure primary config path exists
  if DirectoryExists(LazarusPrimaryConfigPath) = False then
  begin
    OperationSucceeded:=ForceDirectories(LazarusPrimaryConfigPath);
    infoln('Created Lazarus primary config directory: '+LazarusPrimaryConfigPath);
  end;

  if OperationSucceeded then
  begin
    // Make all (should include lcl & ide)
    // distclean was already run; otherwise specify make clean all
    ProcessEx.Executable := FMake;
    ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(LazarusDirectory);
    ProcessEx.Parameters.Clear;
    ProcessEx.Parameters.Add('FPC='+FInstalledCompiler);
    ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(LazarusDirectory));
    ProcessEx.Parameters.Add('FPCDIR='+FPCDirectory); //Make sure our FPC units can be found by Lazarus
    ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
    ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
    if LazarusOPT<>'' then
      ProcessEx.Parameters.Add('OPT='+LazarusOPT);
    ProcessEx.Parameters.Add('all');
    infoln('Lazarus: running make all:');
    ProcessEx.Execute;
    if ProcessEx.ExitStatus <> 0 then
    begin
      OperationSucceeded := False;
      FInstalledLazarus:= '//*\\error//\\'; //todo: check if this really is an invalid filename. it should be.
    end
    else
    begin
      FInstalledLazarus:=IncludeTrailingPathDelimiter(LazarusDirectory)+'lazarus'+FExecutableExtension;
    end;
  end;

  if OperationSucceeded then
  begin
    // Set up a minimal config so we can use LazBuild
    LazarusConfig:=TUpdateLazConfig.Create(LazarusPrimaryConfigPath);
    try
      try
        // Configure help path as well.
        // Note that we might be overwriting user's settings here.
        // todo: if overwriting user's help settings, warn him about it
        LazarusConfig.CHMHelpExe:=IncludeTrailingPathDelimiter(LazarusDirectory)+
          'components'+DirectorySeparator+
          'chmhelp'+DirectorySeparator+
          'lhelp'+DirectorySeparator+
          'lhelp'+FExecutableExtension;
        LazarusConfig.CHMHelpFilesPath:=IncludeTrailingPathDelimiter(LazarusDirectory)+
          'docs'+DirectorySeparator+
          'html'+DirectorySeparator;
        LazarusConfig.LazarusDirectory:=LazarusDirectory;
        {$IFDEF MSWINDOWS}
        // FInstalledCompiler could be something like c:\bla\ppc386.exe, e.g.
        // the platform specific compiler. In order to be able to cross compile
        // we'd rather use fpc
        LazarusConfig.CompilerFilename:=ExtractFilePath(FInstalledCompiler)+'fpc'+FExecutableExtension;
        LazarusConfig.DebuggerFilename:=FMakeDir+'gdb'+FExecutableExtension;
        LazarusConfig.MakeFilename:=FMakeDir+'make'+FExecutableExtension;
        {$ENDIF MSWINDOWS}
        {$IFDEF UNIX}
        // On Unix, FInstalledCompiler should be set to our fpc.sh proxy if installed
        LazarusConfig.CompilerFilename:=FInstalledCompiler;
        LazarusConfig.DebuggerFilename:=which('gdb'); //assume in path
        LazarusConfig.MakeFilename:=which('make'); //assume in path
        {$ENDIF UNIX}
        // Source dir in stock Lazarus on windows is something like
        // $(LazarusDir)fpc\$(FPCVer)\source\
        LazarusConfig.FPCSourceDirectory:=FPCDirectory;
      except
        on E: Exception do
        begin
          OperationSucceeded:=false;
          infoln('Error setting Lazarus config: '+E.ClassName+'/'+E.Message);
        end;
      end;
    finally
      LazarusConfig.Free;
    end;
  end;

  if OperationSucceeded then
  begin
    // Right now, we have a minimally working Lazarus directory, enough
    // to justify creating a shortcut to it.
    // For Windows, a desktop shortcut. For Unixy systems, a script in ~
    {$IFDEF MSWINDOWS}
    if ShortCutName<>EmptyStr then
    begin
      infoln('Lazarus: creating desktop shortcut:');
      try
        //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded
        //DO pass quotes here (it's not TProcess.Params)
        // To installed lazarus
        CreateDesktopShortCut(FInstalledLazarus,'--pcp="'+FLazarusPrimaryConfigPath+'"',ShortCutName);
      finally
        //Ignore problems creating shortcut
      end;
    end;
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    if ShortCutName<>EmptyStr then
    begin
      infoln('Lazarus: creating shortcut in your home directory');
      try
        //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded
        //DO pass quotes here (it's not TProcess.Params)
        CreateHomeStartLink(FInstalledLazarus,'--pcp="'+FLazarusPrimaryConfigPath+'"',ShortcutName);
      finally
        //Ignore problems creating shortcut
      end;
    end;
    {$ENDIF UNIX}
  end;

  {$IFDEF MSWINDOWS}
  //todo: find out what crosscompilers we can install on linux/osx
  if OperationSucceeded then
  begin
    if not ModuleEnabled('WINCROSSX64') then
    begin
      infoln('Module WINCROSSX64: skipped by user. Not building 64 bit LCL.');
      writeln(FLogFile,'Module WINCROSSX64: skipped by user. Not building 64 bit LCL.s');
    end
    else
    begin
      // 64 bit crosscompiler. We rely on the fact that the LCL and a working lazbuild
      // is present. NB: don't know if a working 32 bit LCL is actually required, but
      // it might well be.
      // Note: we're cheating as we're rebuilding Lazarus to 64 bit, which drags in the LCL.
      // Afterwards, rebuild as 32 bit. Reason for 32 bit: we can use components not available
      // for x64.
      // Note: a more elegant solution might be this:
      // http://lazarus.freepascal.org/index.php/topic,13195.msg68826.html#msg68826
      // make lcl LCL_PLATFORM=win64 PP=ppcrossx64.exe CPU_TARGET=x86_64 OS_TARGET=win64
      // or probably this would be sufficient as cross compiler should be picked up
      // make lcl LCL_PLATFORM=win64 CPU_TARGET=x86_64 OS_TARGET=win64
      // or, a bit more detail
      // make packager/registration lazutils lcl
      // alternatives:
      // http://lazarus.freepascal.org/index.php/topic,13195.msg68826.html#msg68826
      // question: why platform win64, not win32? Wiki says only change targetos, target cpu, so
      // suppose it should be win32
      ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
      ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      ProcessEx.Parameters.Add('--cpu=x86_64');
      ProcessEx.Parameters.Add('--operating-system==win64');
      ProcessEx.Parameters.Add('--widgetset=win32');
      ProcessEx.Parameters.Add('--build-all'); //build ide/everything
      ProcessEx.Parameters.Add('--build-ide-options='); //Specify build IDE; pass no arguments
      infoln('Lazarus: compiling Win64 ide (for LCL):');
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
      begin
        infoln('Lazarus: error compiling 64 bit IDE and LCL.');
        writeln(FLogFile, 'Lazarus: error compiling 64 bit IDE and LCL.');
        OperationSucceeded := False;
      end;

      if OperationSucceeded then
      begin
        ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
        ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory);
        ProcessEx.Parameters.Clear;
        ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
        ProcessEx.Parameters.Add('--cpu=i386');
        ProcessEx.Parameters.Add('--operating-system==win32');
        ProcessEx.Parameters.Add('--widgetset=win32');
        ProcessEx.Parameters.Add('--build-all'); //build ide/everything
        ProcessEx.Parameters.Add('--build-ide-options='); //Specify build IDE; pass no arguments
        infoln('Lazarus: compiling 32 bit IDE (after 64 bit compile):');
        ProcessEx.Execute;
        if ProcessEx.ExitStatus <> 0 then
        begin
          infoln('Lazarus: error compiling 32 bit IDE and LCL (after 64 bit compile).');
          writeln(FLogFile, 'Lazarus: error compiling 64 bit IDE and LCL (after 64 bit compile).');
          OperationSucceeded := False;
        end;
      end;
    end;
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded then
    if (ModuleEnabled('BIGIDE')=false) and (ModuleEnabled('LHELP')=false) then
    begin
      //todo: find out if lhelp support can be realized by just compiling
      //package chmhelppkg in some way
      OperationSucceeded:=true;  //continue with whatever we do next
      infoln('Module BIGIDE: skipped by user.');
      writeln(FLogFile,'Module BIGIDE: skipped by user.');
    end
    else
    begin
      if ModuleEnabled('BIGIDE')=false then
      begin
        infoln('Module BIGIDE: required by module: LHELP');
        writeln(FLogFile,'Module BIGIDE: required by module: LHELP');
      end;
      // Make bigide: ide with additional packages as specified by user (in primary config path?)
      // this should also make the lhelp package needed for CHM Help.
      ProcessEx.Executable := FMake;
      ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(LazarusDirectory);
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('FPC='+FInstalledCompiler);
      ProcessEx.Parameters.Add('--directory='+LazarusDirectory+'');
      ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
      ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      ProcessEx.Parameters.Add('bigide');
      infoln('Lazarus: running make bigide:');
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
      begin
        OperationSucceeded := False;
      end;
  end;


  if OperationSucceeded then
  begin
    if not ModuleEnabled('LHELP') then
    begin
      OperationSucceeded:=true;  //continue with whatever we do next
      infoln('Module LHELP: skipped by user; not building lhelp help viewer.');
      writeln(FLogFile,'Module LHELP: skipped by user; not building lhelp help viewer.');
    end
    else
    begin
      // Build lhelp chm help viewer
      ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
      // Set directory to item we're compiling:
      ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
        'components'+DirectorySeparator+
        'chmhelp'+DirectorySeparator+
        'lhelp';
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'components'+DirectorySeparator+
        'chmhelp'+DirectorySeparator+
        'lhelp'+DirectorySeparator+
        'lhelp.lpr');
      infoln('Lazarus: compiling lhelp help viewer:');
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
      begin
        infoln('Lazarus: error compiling lhelp help viewer.');
        writeln(FLogFile, 'Lazarus: error compiling lhelp help viewer.');
        OperationSucceeded := False;
      end;
    end;
  end;

  if OperationSucceeded then
    if not ModuleEnabled('LAZDATADESKTOP') then
    begin
      OperationSucceeded:=true;  //continue with whatever we do next
      infoln('Module LAZDATADESKTOP: skipped by user.');
      writeln(FLogFile,'Module LAZDATADESKTOP:  skipped by user.');
    end
    else
    begin
      // Build data desktop, nice example of building with lazbuild
      ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
      // Set directory to item we're compiling:
      ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
        'tools'+DirectorySeparator+
        'lazdatadesktop';
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'tools'+DirectorySeparator+
        'lazdatadesktop'+DirectorySeparator+
        'lazdatadesktop.lpr');
      infoln('Lazarus: compiling data desktop:');
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
        OperationSucceeded := False;
    end;

  if OperationSucceeded then
    if not ModuleEnabled('DOCEDITOR') then
    begin
      OperationSucceeded:=true;  //continue with whatever we do next
      infoln('Module DOCEDITOR: skipped by user.');
      writeln(FLogFile,'Module DOCEDITOR: skipped by user.');
    end
    else
    begin
      // Build Lazarus Doceditor
      ProcessEx.Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
      // Set directory to item we're compiling:
      ProcessEx.CurrentDirectory:=IncludeTrailingPathDelimiter(LazarusDirectory)+
        'doceditor';
      ProcessEx.Parameters.Clear;
      ProcessEx.Parameters.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      ProcessEx.Parameters.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'doceditor'+DirectorySeparator+
        'lazde.lpr');
      infoln('Lazarus: compiling doc editor:');
      ProcessEx.Execute;
      if ProcessEx.ExitStatus <> 0 then
        OperationSucceeded := False;
    end;

  if OperationSucceeded then
    writeln(FLogFile,'Lazarus update succeeded at revision number ', AfterRevision);
  ProcessEx.Free;
  Result := OperationSucceeded;
end;

constructor Tinstaller.Create;
var
  LogFileName: string;
begin
  // We'll set the bootstrap compiler to a file in the temp dir.
  // This won't exist so the CheckAndGetNeededExecutables code will download it for us.
  // User can specify an existing CompilerName later on, if she wants to.
  FBootstrapCompilerDirectory := SysUtils.GetTempDir;
  {$IFDEF MSWINDOWS}
  // On Windows, we can always compile 32 bit with a 64 bit cross CompilerName, regardless
  // of actual architecture (x86 or x64)
  FBootstrapCompilerFTP :=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-win32-ppc386.zip';
  FBootstrapCompilerName := 'ppc386.exe';
  FFPCPlatform:='i386-win32';
  {$ENDIF MSWINDOWS}
  {$IFDEF Linux}
  //If compiled for x86 32 bit, install 32 bit
  //If compiled for x64, install x64 only.//todo: cross compiler!?!
  {$IFDEF CPU386}
  FBootstrapCompilerFTP :=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/i386-linux-ppc386.bz2';
  FBootstrapCompilerName := 'i386-linux-ppc386-1';
  FFPCPlatform:='i386-linux';
  {$ELSE}
  {$IFDEF cpuarmel}
  FBootstrapCompilerFTP :=
  'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/arm-linux-ppcarm.bz2';
  FBootstrapCompilerName := 'arm-linux-ppcarm';
  FFPCPlatform:='arm-linux';
  {$ELSE} // Assume x64 (could also be PowerPC, ARM I suppose)
  FBootstrapCompilerFTP :=
  'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/x86_64-linux-ppcx64.bz2';
  FBootstrapCompilerName := 'x86_64-linux-ppcx64';
  FFPCPlatform:='x86_64-linux';
  {$ENDIF cpuarmel}
  {$ENDIF CPU386}
  {$ENDIF Linux}
  {$IFDEF Darwin}
  //OSX
  FBootstrapCompilerFTP:=
    'ftp.freepascal.org/pub/fpc/dist/2.6.0/bootstrap/universal-darwin-ppcuniversal.tar.bz2';
  FBootstrapCompilerName := 'ppcuniversal';
  FFPCPlatform:='x86_64-darwin';
  {$ENDIF Darwin}
  FClean:=false; //Build, not clean, by default

  {$IFDEF MSWINDOWS}
  FExecutableExtension := '.exe';
  {$ELSE}
  FExecutableExtension := '';
  {$ENDIF MSWINDOWS}
  FShortcutName:='Lazarus_trunk'; //Default shortcut name; if it's not empty, shortcut will be written.
  FShortCutNameFpcup:='fpcup_update'; //Default shortcut name; if it's not empty, shortcut will be written.
  // Binutils needed for compilation
  CreateBinutilsList;

  FInstalledCompiler := '';
  FSVNDirectory := '';
  FUpdater := TUpdater.Create;
  SetLazarusPrimaryConfigPath(''); //Let property set up platform-dependent default
  SetMakePath('');

  {$IFDEF MSWINDOWS}
  LogFileName:='fpcup.log'; //current directory
  {$ELSE}
  LogFileName:=ExpandFileNameUTF8('~')+DirectorySeparator+'fpcup.log'; //In home directory
  {$ENDIF MSWINDOWS}
  AssignFile(FLogFile,LogFileName);
  if FileExistsUTF8(LogFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
  WriteLn(FLogFile,DateTimeToStr(now),': fpcup started.');
  TextRec(FLogVerboseFile).Mode:=0;  //class variables should have been 0
end;

destructor Tinstaller.Destroy;
begin
  WriteLn(FLogFile,DateTimeToStr(now),': fpcup finished.');
  WriteLn(FLogFile,'------------------------------------------------');
  CloseFile(FLogFile);
  if TextRec(FLogVerboseFile).Mode<>0 then
    CloseFile(FLogVerboseFile);
  FUpdater.Free;
  FBinUtils.Free;
  inherited Destroy;
end;

end.
