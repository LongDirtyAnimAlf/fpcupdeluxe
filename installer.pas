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
  Classes, SysUtils, updater;

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
    FFPCOPT: string;
    FLazarusOPT: string;
    FShortcutName: string; //Name for shortcut/shell script pointing to newly installed Lazarus
    FExecutableExtension: string; //.exe on Windows
    FFPCPlatform: string; //Identification for platform in compiler path (e.g. i386-win32)
    FInstalledCompiler: string; //Complete path to installed FPC compiler; used to compile Lazarus
    FInstalledCompilerName: string; //Name only of installed PPC compiler (e.g. ppcx64 on 64 bit Intel OSX)
    FInstalledCrossCompiler: string; //Complete path to an optional cross compiler that we installed (also used for Lazarus)
    FInstalledLazarus: string; //Path to installed Lazarus; used in creating shortcuts
    FLazarusPrimaryConfigPath: string;
    FLogFile:Text;
    FMake: string;
    FShortCutNameFpcup: string;
    {$IFDEF MSWINDOWS}
    FMakeDir: string;
    {$ENDIF}
    FShortCutNameFpcupIsSet:boolean; //indicates if ShortCutNameFpcupSet was set
    FSkipFPC: boolean;
    FSkipLazarus: boolean;
    //todo: check if we shouldn't rather use FSVNExecutable, extract dir from that.
    FSVNDirectory: string; //Unpack SVN files in this directory. Actual SVN exe may be below this directory.
    FTar: string; //Location or name of tar executable
    FUpdater: TUpdater;
    FUnzip: string; //Location or name of unzip executable
    FVerbose: boolean;
    procedure CreateBinutilsList;
    procedure CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string) ;
    procedure CreateHomeStartLink(Target, TargetArguments, ShortcutName: string);
    function DownloadBinUtils: boolean;
    function DownloadBootstrapCompiler: boolean;
    function DownloadFTP(URL, TargetFile: string): boolean;
    function DownloadHTTP(URL, TargetFile: string): boolean;
    function DownloadSVN: boolean;
    function CheckAndGetNeededExecutables: boolean;
    procedure EnvironmentWithOurPath(var EnvironmentList: TStringList; const NewPath: string);
    // Return complete environment except replace path with our own value
    function FindSVNSubDirs(): boolean;
    function GetBootstrapCompiler: string;
    function GetCompilerName: string;
    //Checks for binutils, svn.exe and downloads if needed. Returns true if all prereqs are met.
    function GetFpcDirectory: string;
    function GetFPCRevision: string;
    function GetFPCUrl: string;
    function GetLazarusRevision: string;
    procedure SetAllOptions(AValue: string);
    procedure SetFPCRevision(AValue: string);
    procedure SetLazarusPrimaryConfigPath(AValue: string);
    procedure SetLazarusRevision(AValue: string);
    procedure SetShortCutNameFpcup(AValue: string);
    procedure SetSkipFPC(AValue: boolean);
    procedure SetSkipLazarus(AValue: boolean);
    procedure SetVerbose(AValue: boolean);
    function Which(Executable: string): string; //Runs which command. Returns full path of executable, if it exists
    function GetLazarusDirectory: string;
    function GetLazarusUrl: string;
    function GetMakePath: string;
    function Run(Executable: string; const Params: TStringList; const NewPath: string): longint;
    function RunOutput(Executable: string; const Params: TStringList; const NewPath: string; var Output: TStringList): longint;
    function RunOutput(Executable: string; const Params: TStringList; const NewPath: string; var Output: string): longint;
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
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property FPCURL: string read GetFPCUrl write SetFPCUrl; //SVN URL for FPC
    property FPCOPT: string read FFPCOPT write SetFPCOPT;
    property FPCRevision:string read GetFPCRevision write SetFPCRevision;
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    property LazarusPrimaryConfigPath: string
      read FLazarusPrimaryConfigPath write SetLazarusPrimaryConfigPath;
    //The directory where the configuration for this Lazarus instance must be stored.
    property LazarusURL: string read GetLazarusUrl write SetLazarusUrl;
    //SVN URL for Lazarus
    property LazarusOPT:string read FLazarusOPT write SetLazarusOPT;
    property LazarusRevision:string read GetLazarusRevision write SetLazarusRevision;
    property MakeDirectory: string read GetMakePath write SetMakePath;
    //Directory of make executable and other binutils. If it doesn't exist, make and binutils will be downloaded
    property SkipFPC:boolean read FSkipFPC write SetSkipFPC;
    property SkipLazarus:boolean read FSkipLazarus write SetSkipLazarus;
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
  ScriptText:=TStringList.Create;
  try
    // No quotes here, either, we're not in a shell, apparently...
    ScriptFile:=ExpandFileNameUTF8('~')+DirectorySeparator+ShortcutName;
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
  Params: TStringList;
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
  end;

  if OperationSucceeded then
  begin
    {$IFDEF MSWINDOWS}
    //Extract zip, overwriting without prompting
    Params:=TStringList.Create;
    try
      Params.Add('-o'); //overwrite existing files
      Params.Add('-d'); //Note: apparently we can't call (the FPC supplied) unzip.exe -d with "s
      Params.Add(ArchiveDir);
      Params.Add(BootstrapArchive); // zip/archive file
      if Run(FUnzip, Params, '') <> 0 then
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
    // Move CompilerName to proper directory
    if OperationSucceeded = True then
    begin
      infoln('Going to rename/move ' + ArchiveDir + CompilerName + ' to ' + BootstrapCompiler);
      renamefile(ArchiveDir + CompilerName, BootstrapCompiler);
    end;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    //Extract bz2, overwriting without prompting
    Params:=TStringList.Create;
    try
      Params.Add('-d');
      Params.Add('-f');
      Params.Add('-q');
      Params.Add(BootstrapArchive); // zip/archive file
      if Run(FBunzip2, Params, '') <> 0 then
      begin
        infoln('Error: Received non-zero exit code extracting bootstrap compiler. This will abort further processing.');
        OperationSucceeded := False;
      end
      else
      begin
        ExtractedCompiler:=BootstrapArchive+'.out'; //default bzip2 output filename
        OperationSucceeded := True; // Spelling it out can't hurt sometimes
      end;
    finally
      Params.Free;
    end;
    // Move compiler to proper directory; note bzip2 will append .out to file
    if OperationSucceeded = True then
    begin
      infoln('Going to rename/move ' + ExtractedCompiler + ' to ' + BootstrapCompiler);
      sysutils.DeleteFile(BootstrapCompiler); //ignore errors
      // We might be moving files across partitions so we cannot use renamefile
      OperationSucceeded:=FileUtil.CopyFile(ExtractedCompiler, BootstrapCompiler);
      sysutils.DeleteFile(ExtractedCompiler);
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
  if result=false then infoln('DownloadFTP: error downloading '+URL+'. Details: host:'+Host+'; port: '+Inttostr(Port)+'; remote path:'+Source);
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
  Params: TStringList;
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
  OperationSucceeded := DownloadHTTP(
    'http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip'
    , SVNZip);
  if OperationSucceeded then
  begin
    // Extract, overwrite
    Params:=TStringList.Create;
    try
      Params.Add('-o'); //overwrite existing files
      Params.Add('-d'); //Note: apparently we can't call (the FPC supplied) unzip.exe -d with "s
      Params.Add(FSVNDirectory);
      Params.Add(SVNZip); // zip/archive file
      ResultCode:=Run(FUnzip, Params, '');
      if ResultCode<> 0 then
      begin
        OperationSucceeded := False;
        infoln('resultcode: ' + IntToStr(ResultCode));
      end;
    finally
      Params.Free;
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

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  OperationSucceeded: boolean;
  Output: string;
  Params: TStringList;
  ResultCode: longint;
begin
  OperationSucceeded := True;
  // The extractors used depend on the bootstrap CompilerName URL/file we download
  // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)
  {$IFDEF MSWINDOWS}
  // Need to do it here so we can pick up make path.
  FBunzip2:=EmptyStr;
  FTar:=EmptyStr;
  FUnzip := IncludeTrailingPathDelimiter(FMakeDir) + 'unzip' + FExecutableExtension;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FBunzip2:='bunzip2';
  FTar:='tar';
  FUnzip:=EmptyStr;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  FBunzip2:='bunzip2';
  FTar:='gnutar'; //gnutar can decompress as well; bsd tar can't
  FUnzip:=EmptyStr;
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
      Output := '';
      Params:=TStringList.Create;
      Params.Add('-v');
      try
        ResultCode:=RunOutput(FMake, Params, '', Output);
      finally
        Params.Free;
      end;

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
      if FSVNDirectory='' then FSVNDirectory := IncludeLeadingPathDelimiter(FBootstrapCompilerDirectory)+'svn'+DirectorySeparator;
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
    // Check for valid unzip/gunzip/tar executable
    {$IFDEF MSWINDOWS}
    if FUnzip<>EmptyStr then
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    if FBunzip2<>EmptyStr then
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    if FTar<>EmptyStr then
    {$ENDIF DARWIN}
    begin
      try
        Output := '';
        // See unzip.h for return codes.
        Params:=TStringList.Create;
        try
          // This roundabout way at least avoids messing with ifdefs
          // note that we will test for the last program found
          // but on Unixy systems that probably does not matter
          Output:=EmptyStr;
          if FUnzip<>EmptyStr then
          begin
            Params.Clear;
            Params.Add('-v');
            Output:=FUnzip;
          end;
          if FBunzip2<>EmptyStr then
          begin
            Params.Clear;
            Params.Add('--version');
            Output:=FBUnzip2;
          end;
          if FTar<>EmptyStr then
          begin
            // We put tar after bunzip2; we use it in OSX
            // and want to test it then.
            Params.Clear;
            Params.Add('--version');
            Output:=FTar;
          end;
          if Output=EmptyStr then
          begin
            OperationSucceeded:=false;
            ResultCode:=-1;
            infoln('No valid unzip/bunzip2/tar executable names/locations set up in program code. Please fix program.');
          end
          else
          begin
            ResultCode:=Run(Output, Params, EmptyStr);
          end;
        finally
          Params.Free;
        end;

        if ResultCode=0 then
        begin
          infoln('Found valid extractor: ' + Output);
          OperationSucceeded := true;
        end
        else
        begin
          //invalid unzip/gunzip/whatever
          infoln('Error: could not find valid extractor: ' + Output + ' (result code was: '+IntToStr(ResultCode)+')');
          OperationSucceeded:=false;
        end;
      except
        OperationSucceeded := False;
      end;
    end;
  end;


  if OperationSucceeded then
  begin
    // Check for proper FPC bootstrap compiler
    infoln('Checking for FPC bootstrap compiler: '+BootStrapCompiler);
    try
      Output := '';
      Params:=TStringList.Create;
      try
        // Show help without waiting:
        Params.Add('-h');
        ResultCode:=RunOutput(BootstrapCompiler, Params, EmptyStr, Output);
      finally
        Params.Free;
      end;

      if ResultCode=0 then
      begin
        if Ansipos('Free Pascal Compiler', Output) = 0 then
        begin
          OperationSucceeded := False;
          infoln('Found FPC executable but it is not a Free Pascal compiler. Going to overwrite it.');
        end
        else
        begin
          //valid FPC compiler
          infoln('Found valid FPC bootstrap compiler.');
          OperationSucceeded:=true;
        end;
      end
      else
      begin
        //Error running bootstrapcompiler
        infoln('Error trying to test run bootstrap compiler '+BootstrapCompiler+'. Received output: '+Output+'; resultcode: '+IntToStr(ResultCode));
        OperationSucceeded:=false;
      end;
    except
      on E: Exception do
      begin
        infoln('Exception trying to test run bootstrap compiler '+BootstrapCompiler+'. Received output: '+Output);
        infoln(E.ClassName+'/'+E.Message);
        OperationSucceeded := False;
      end;
    end;
    // Still within bootstrap compilertest...
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
  SingleLine: string;
begin
  // GetEnvironmentVariableCount is 1 based
  for Counter:=1 to GetEnvironmentVariableCount do
  begin
    SingleLine:=GetEnvironmentString(Counter);
    if AnsiPos(LookFor, SingleLine)>0 then
    begin
      //We found the PATH variable; replace it
      //with our own
      if ansipos(AnsiUpperCase(LookFor), AnsiUpperCase(NewPath))=0 then
      begin
        //Add PATH= or path= to the line for our user
        EnvironmentList.Add(LookFor+NewPath);
      end
      else
      begin
        EnvironmentList.Add(NewPath);
      end;
    end
    else
    begin
      EnvironmentList.Add(SingleLine);
    end;
  end;
end;

function TInstaller.FindSVNSubDirs(): boolean;
// Looks through SVN directory and sets updater's SVNExecutable
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
  Params: TStringList;
  Output: string;
begin
  Params:=TStringList.Create;
  try
    Params.Add(Executable);
    RunOutput('which', Params, '', Output);
    //Remove trailing LF(s) and other control codes:
    while (length(output)>0) and (ord(output[length(output)])<$20) do
      delete(output,length(output),1);
    // We could have checked for RunOutput exitcode, but why not
    // do file existence check instead:
    if fileexists(Output) then
    begin
      result:=Output;
    end
    else
    begin
      result:=''; //command failed
    end;
  finally
    Params.Free;
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

function TInstaller.GetFPCUrl: string;
begin
  Result := FUpdater.FPCURL;
end;

function TInstaller.GetLazarusRevision: string;
begin
  Result := FUpdater.LazarusRevision;
end;

procedure TInstaller.SetAllOptions(AValue: string);
begin
  if FAllOptions=AValue then Exit;
  FAllOptions:=AValue;
end;

procedure TInstaller.SetFPCRevision(AValue: string);
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
  FInstalledCompiler := FPCDirectory + 'bin' +DirectorySeparator+'fpc';
  if FileExistsUTF8(FInstalledCompiler+'.sh') then
  begin
    //Use our proxy if it is installed
    FInstalledCompiler:=FInstalledCompiler+'.sh';
  end;
  {$ENDIF UNIX}
end;

function TInstaller.Run(Executable: string; const Params: TStringList; const NewPath: string): longint;
{ Runs executable without showing output, unless something went wrong (result code<>0) }
var
  OutputStringList: TStringList;
  TempFileName:string;
begin
  OutputStringList := TStringList.Create;
  try
    Result:=RunOutput(Executable, Params, NewPath, OutputStringList);
    if result<>0 then
    begin
      //RunOutput call above should already have warned about non-zero exitstatus
      infoln(OutputStringList.Text);
      TempFileName:=SysUtils.GetTempFileName;
      OutputStringList.SaveToFile(TempFileName);
      WriteLn(FLogFile,'  output logged in ',TempFileName);
    end;
  finally
    OutputStringList.Free;
  end;
end;

function TInstaller.RunOutput(Executable: string; const Params: TStringList;
  const NewPath: String; var Output: TStringList): longint;
var
  EnvironmentList: TStringList;
  SpawnedProcess: TProcess;
  OutputStream: TMemoryStream;

  function ReadOutput: boolean;
    // returns true if output was actually read
  const
    BufSize = 4096;
  var
    Buffer: array[0..BufSize - 1] of byte;
    ReadBytes: integer;
  begin
    Result := False;
    while SpawnedProcess.Output.NumBytesAvailable > 0 do
    begin
      ReadBytes := SpawnedProcess.Output.Read(Buffer, BufSize);
      OutputStream.Write(Buffer, ReadBytes);
      if Verbose then
        write(copy(pchar(@buffer[0]),1,ReadBytes));
      Result := True;
    end;
  end;

begin
  Result := 255; //Preset to failure
  infoln('Calling:');
  infoln(Executable + ' ' +AnsiReplaceStr(Params.Text, LineEnding, ' '));

  OutputStream := TMemoryStream.Create;
  SpawnedProcess := TProcess.Create(nil);
  try
    try
      SpawnedProcess.Executable:=Executable;
      SpawnedProcess.Parameters:=Params;

      if NewPath<>EmptyStr then
      begin
        // Replace path of spawned process with the one we want
        EnvironmentList:=TStringList.Create;
        try
          //Procedure will handle strings with and without 'PATH='...
          EnvironmentWithOurPath(EnvironmentList, NewPath);
          WriteLn(FLogFile,'Custom path for calling '+Executable+':');
          WriteLn(FLogFile,NewPath);
          SpawnedProcess.Environment:=EnvironmentList;
        finally
          EnvironmentList.Free;
        end;
      end;
      SpawnedProcess.Options := [poUsePipes, poStderrToOutPut];
      SpawnedProcess.ShowWindow := swoHIDE;
      SpawnedProcess.Execute;
      while SpawnedProcess.Running do
      begin
        if not ReadOutput then
          Sleep(100);
      end;
      ReadOutput;
      OutputStream.Position := 0;
      Output.LoadFromStream(OutputStream);
      result := SpawnedProcess.ExitStatus;
      if result<>0 then
      begin
        infoln('Command returned non-zero ExitStatus: '+IntToStr(result)+'. Output:');
        WriteLn(FLogFile,'ERROR running ',Executable + ' ' +AnsiReplaceStr(Params.Text, LineEnding, ' '));
      end;
    except
      on E: Exception do
      begin
        {We don't want to do an explicit file exists
        as this complicates with paths, current dirs, .exe extensions etc.}

        //Something went wrong. We need to pass on what and mark this as a failure
        infoln('Exception calling '+Executable);
        infoln('Details: '+E.ClassName+'/'+E.Message);
        WriteLn(FLogFile, 'Exception calling '+Executable);
        WriteLn(FLogFile, 'Details: '+E.ClassName+'/'+E.Message);
        Result:=254; //fairly random but still an error, and distinct from earlier code
      end;
    end;
  finally
    OutputStream.Free;
    SpawnedProcess.Free;
  end;
end;

function TInstaller.RunOutput(Executable: string; const Params: TStringList; const NewPath: string; var Output: string): longint;
var
  OutputStringList: TStringList;
begin
  OutputStringList := TStringList.Create;
  try
    Result:=RunOutput(Executable, Params, NewPath, OutputStringList);
    Output := OutputStringList.Text;
  finally
    OutputStringList.Free;
  end;
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

procedure TInstaller.SetLazarusRevision(AValue: string);
begin
  FUpdater.LazarusRevision:=AValue;
end;

procedure TInstaller.SetShortCutNameFpcup(AValue: string);
begin
  FShortCutNameFpcupIsSet:=true;
  if FShortCutNameFpcup=AValue then Exit;
  FShortCutNameFpcup:=AValue;
end;

procedure TInstaller.SetSkipFPC(AValue: boolean);
begin
  if FSkipFPC=AValue then Exit;
  FSkipFPC:=AValue;
end;

procedure TInstaller.SetSkipLazarus(AValue: boolean);
begin
  if FSkipLazarus=AValue then Exit;
  FSkipLazarus:=AValue;
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


function Tinstaller.GetFPC: boolean;
// In this function, we try to deal with existing system wide fpc.cfg (Unix)
// and the wrong compilers/binutils being in the path
var
  AfterRevision: string;
  BeforeRevision: string;
  BinPath: string; //Path where installed CompilerName ends up
  CustomPath: string; //Our own version of path we use to pass to commands
  Executable: string;
  FileCounter:integer;
  FPCCfg: string;
  FPCScript: string;
  OperationSucceeded: boolean;
  Params: TstringList;
  TxtFile:text;
  SearchRec:TSearchRec;
  FPCVersion:string;
begin
  if SkipFPC then
    begin
    result:=true;  //continue with lazarus
    infoln('FPC installation/update skipped by user.');
    writeln(FLogFile,'FPC installation/update skipped by user.');
    exit;
    end;
  writeln(FLogFile,'Bootstrap compiler dir: '+BootstrapCompilerDirectory);
  writeln(FLogFile,'FPC URL:                '+FPCURL);
  writeln(FLogFile,'FPC options:            '+FPCOPT);
  writeln(FLogFile,'FPC directory:          '+FPCDirectory);
  {$IFDEF MSWINDOWS}
  writeln(FLogFile,'Make/binutils path:     '+MakeDirectory);
  {$ENDIF MSWINDOWS}

  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  CustomPath:=BootstrapCompilerDirectory+PathSeparator+
    MakeDirectory+PathSeparator+
    FSVNDirectory+PathSeparator+
    FPCDirectory+PathSeparator+
    LazarusDirectory;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  CustomPath:=Emptystr; // We need make, etc, so we can't really do anything here, can we?
  {$ENDIF UNIX}
  if CustomPath<>EmptyStr then
    writeln(FLogFile,'External program path:  '+CustomPath);

  //Make sure we have the proper tools:
  OperationSucceeded:=CheckAndGetNeededExecutables;

  //Make distclean to clean out any cruft, and speed up svn update
  if OperationSucceeded then
  begin
    // Make distclean; we don't care about failure (e.g. directory might be empty etc)
    Executable := FMake;
    Params:=TStringList.Create;
    try
      Params.Add('FPC='+BootstrapCompiler);
      {$IFDEF MSWINDOWS}
      // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
      // Specify the ones the compiler should use:
      Params.Add('OPT=-FD'+ExcludeTrailingPathDelimiter(MakeDirectory));
      //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
      Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
      {$ENDIF MSWINDOWS}
      Params.Add('--directory='+ExcludeTrailingPathDelimiter(FPCDirectory));
      Params.Add('UPXPROG=echo'); //Don't use UPX
      Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Params.Add('distclean');
      infoln('FPC: running make distclean before checkout/update:');
      Run(Executable, Params, CustomPath);
    finally
      Params.Free;
    end;
  end;

  infoln('Checking out/updating FPC sources...');
  if OperationSucceeded then OperationSucceeded:=FUpdater.UpdateFPC(BeforeRevision, AfterRevision);
  infoln('FPC was at revision: '+BeforeRevision);
  if FUpdater.Updated then infoln('FPC is now at revision: '+AfterRevision) else infoln('No updates for FPC found.');

  if OperationSucceeded then
  begin
    // Make all/install, using bootstrap compiler.
    // Make all should use generated compiler internally for unit compilation
    Executable := FMake;
    Params:=TStringList.Create;
    try
      Params.Add('FPC='+BootstrapCompiler);
      {$IFDEF MSWINDOWS}
      // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
      // Specify the ones the compiler should use:
      Params.Add('OPT=-FD'+ExcludeTrailingPathDelimiter(MakeDirectory));
      //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
      Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
      {$ENDIF MSWINDOWS}
      Params.Add('--directory='+ExcludeTrailingPathDelimiter(FPCDirectory));
      Params.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
      Params.Add('UPXPROG=echo'); //Don't use UPX
      Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Params.Add('all');
      Params.Add('install');
      infoln('Running make all install for FPC:');
      if Run(Executable, Params, CustomPath) <> 0 then
        OperationSucceeded := False;
      {$IFDEF UNIX}
      // create link 'units' below FPCDirectory to <somewhere>/lib/fpc/$fpcversion/units
      // need to find $fpcversion first
      FPCVersion:='';
      if FindFirst(IncludeTrailingPathDelimiter(FPCDirectory)+'lib/fpc/*',faDirectory,SearchRec)=0 then
        repeat
          if (SearchRec.Attr and faDirectory) <>0 then
            begin
            FPCVersion:=SearchRec.Name;
            if (FPCVersion<>'') and (FPCVersion[1]>'1') and (FPCVersion[1]<='9') then
              break;
            end;
        until FindNext(SearchRec)<>0;
      //if not found will point to wrong dir
      DeleteFile(IncludeTrailingPathDelimiter(FPCDirectory)+'units');
      fpSymlink(pchar(IncludeTrailingPathDelimiter(FPCDirectory)+'lib/fpc/'+FPCVersion+'/units'),
      pchar(IncludeTrailingPathDelimiter(FPCDirectory)+'units'));
      {$ENDIF UNIX}
    finally
      Params.Free;
    end;
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
  begin
    // Make crosscompiler using new CompilerName- todo: check out what cross compilers we can install on Linux/OSX
    // Note: consider this as an optional item, so don't fail the function if this breaks.
    Executable := FMake;
    infoln('Running Make all (FPC crosscompiler):');
    Params:=TStringList.Create;
    try
      Params.Add('FPC='+FInstalledCompiler+'');
      // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
      // Specify the ones the compiler should use:
      // We can rely on binutils being copied to compiler bin path here:
      Params.Add('OPT=-FD'+ExtractFilePath(FInstalledCompiler));
      //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
      Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
      Params.Add('--directory='+ ExcludeTrailingPathDelimiter(FPCDirectory));
      Params.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
      Params.Add('UPXPROG=echo'); //Don't use UPX
      Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Params.Add('OS_TARGET=win64');
      Params.Add('CPU_TARGET=x86_64');
      if FFPCOPT<>'' then
        Params.Add('OPT='+FFPCOPT);
      Params.Add('all');
      if Run(Executable, Params, CustomPath) = 0 then
      begin
        // Install crosscompiler using new CompilerName - todo: only for Windows!?!?
        // make all and make crossinstall perhaps equivalent to
        // make all install CROSSCOMPILE=1??? todo: find out
        Executable := FMake;
        infoln('Running Make crossinstall for FPC:');
        // Params already assigned
        Params.Clear;
        Params.Add('FPC='+FInstalledCompiler+'');
        // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
        // Specify the ones the compiler should use:
        // We can rely on binutils being copied to compiler bin path here:
        Params.Add('OPT=-FD'+ExtractFilePath(FInstalledCompiler));
        //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
        Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
        Params.Add('INSTALL_PREFIX='+ExcludeTrailingPathDelimiter(FPCDirectory));
        Params.Add('UPXPROG=echo'); //Don't use UPX
        Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
        Params.Add('OS_TARGET=win64'); //cross compile for different OS...
        Params.Add('CPU_TARGET=x86_64'); // and processor.
        Params.Add('crossinstall');
        // Note: consider this as an optional item, so don't fail the function if this breaks.
        if Run(Executable, Params, CustomPath)=0 then
        begin
          // Let everyone know of our shiny new crosscompiler:
          FInstalledCrossCompiler := IncludeTrailingPathDelimiter(FPCDirectory) + 'bin' +
            DirectorySeparator + IncludeTrailingPathDelimiter(FFPCPlatform) + 'ppcrossx64.exe';
        end
        else
        begin
          infoln('Problem compiling/installing crosscompiler. Continuing regardless.');
        end;
      end;
    finally
      Params.Free;
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
      Executable := BinPath + 'fpcmkcfg';
      Params:=TStringList.Create;
      try
        Params.Add('-d');
        Params.Add('basepath='+ExcludeTrailingPathDelimiter(FPCDirectory));
        Params.Add('-o');
        Params.Add('' + FPCCfg + '');
        infoln('Debug: Running fpcmkcfg: ');
        if Run(Executable, Params, CustomPath) <> 0 then
          OperationSucceeded := False;
      finally
        Params.Free;
      end;
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
         IncludeTrailingPathDelimiter(BinPath),'fpc.cfg -Xp',
         IncludeTrailingPathDelimiter(FPCDirectory),'compiler/ -FD'+
         IncludeTrailingPathDelimiter(BinPath)+' $*');
    CloseFile(TxtFile);
    OperationSucceeded:=(FPChmod(FPCScript,&700)=0); //Make executable; fails if file doesn't exist=>Operationsucceeded update
    if OperationSucceeded then
    begin
      infoln('Created launcher script for fpc:'+FPCScript);
    end
    else
    begin
      infoln('Error creating launcher script for fpc:'+FPCScript);
    end;
  end;
  {$ENDIF UNIX}
  if OperationSucceeded then
    writeln(FLogFile,'FPC update succeeded at revision number ', AfterRevision);
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
// Note: getlazarus depends on properly installed FPC
// Properly installed in this case means: the way
// GetFPC would install it ;)
// Assumed: binutils in fpc dir or in path
var
  AfterRevision: string;
  BeforeRevision: string;
  CustomPath: string;
  Executable: string;
  LazarusConfig: TUpdateLazConfig;
  OperationSucceeded: boolean;
  Params: TStringList;
begin
  if SkipLazarus then
  begin
    result:=true;  //continue with whatever we do next
    infoln('Lazarus installation/update skipped by user.');
    writeln(FLogFile,'Lazarus installation/update skipped by user.');
    exit;
  end;
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

  {$IFDEF MSWINDOWS}
  // Try to ignore existing make.exe, fpc.exe by setting our own path:
  CustomPath:=BootstrapCompilerDirectory+PathSeparator+
    MakeDirectory+PathSeparator+
    FSVNDirectory+PathSeparator+
    FPCDirectory+PathSeparator+
    LazarusDirectory;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  CustomPath:=Emptystr; // We need make, etc, so we can't really do anything here, can we?
  {$ENDIF UNIX}
  if CustomPath<>EmptyStr then
    writeln(FLogFile,'External program path:  '+CustomPath);

  // Make distclean to clean out any cruft, and speed up svn update
  if OperationSucceeded then
  begin
    // Make distclean; we don't care about failure (e.g. directory might be empty etc)
    Executable := FMake;
    Params:=TStringList.Create;
    try
      Params.Add('FPC='+FInstalledCompiler+'');
      {$IFDEF MSWINDOWS}
      // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
      // Specify the ones the compiler should use:
      // We can rely on binutils being copied to compiler bin path here:
      Params.Add('OPT=-FD'+ExtractFilePath(FInstalledCompiler));
      //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
      Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
      {$ENDIF MSWINDOWS}
      Params.Add('--directory='+ExcludeTrailingPathDelimiter(LazarusDirectory));
      Params.Add('UPXPROG=echo'); //Don't use UPX
      Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Params.Add('distclean');
      infoln('Lazarus: running make distclean before checkout/update:');
      Run(Executable, Params, CustomPath);
    finally
      Params.Free;
    end;
  end;


  // Download Lazarus source:
  if OperationSucceeded = True then
  begin
    infoln('Checking out/updating Lazarus sources...');
    OperationSucceeded := FUpdater.UpdateLazarus(BeforeRevision, AfterRevision);
    infoln('Lazarus was at revision: '+BeforeRevision);
    if FUpdater.Updated then infoln('Lazarus is now at revision: '+AfterRevision) else infoln('No updates for Lazarus found.');
  end;

  // Make sure primary config path exists
  if DirectoryExists(LazarusPrimaryConfigPath) = False then
  begin
    OperationSucceeded:=ForceDirectories(LazarusPrimaryConfigPath);
    infoln('Created Lazarus primary config directory: '+LazarusPrimaryConfigPath);
  end;

  {$IFDEF MSWINDOWS}
  //todo: find out what crosscompilers we can install on linux/osx
  if OperationSucceeded then
  begin
    // LCL 64 bit crosscompiler.
    if FInstalledCrossCompiler<>'' then
    begin
      Executable := FMake;
      Params:=TStringList.Create;
      try
        Params.Add('FPC='+FInstalledCrossCompiler+'');
        // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
        // Specify the ones the compiler should use:
        // We can rely on binutils being copied to compiler bin path here:
        Params.Add('OPT=-FD'+ExtractFilePath(FInstalledCompiler));
        //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
        Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
        Params.Add('--directory='+ExcludeTrailingPathDelimiter(LazarusDirectory));
        Params.Add('UPXPROG=echo'); //Don't use UPX
        Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
        Params.Add('LCL_PLATFORM=win32');
        Params.Add('OS_TARGET=win64');
        Params.Add('CPU_TARGET=x86_64');
        Params.Add('clean'); //make clean
        Params.Add('lcl'); //make lcl
        infoln('Lazarus: running make LCL crosscompiler:');
        // Note: consider this optional; don't fail the function if this fails.
        if Run(Executable, Params, CustomPath)<> 0 then infoln('Problem compiling 64 bit LCL; continuing regardless.');
      finally
        Params.Free;
      end;
    end;
  end;
  {$ENDIF MSWINDOWS}

  if OperationSucceeded then
  begin
    // Make clean all (should include lcl & ide)
    // todo: check if we really need to do a make clean after lcl cross...
    Executable := FMake;
    Params:=TStringList.Create;
    try
      Params.Add('FPC='+FInstalledCompiler);
      {$IFDEF MSWINDOWS}
      // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
      // Specify the ones the compiler should use:
      // We can rely on binutils being copied to compiler bin path here:
      Params.Add('OPT=-FD'+ExtractFilePath(FInstalledCompiler));
      //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
      Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
      {$ENDIF MSWINDOWS}
      Params.Add('--directory='+ExcludeTrailingPathDelimiter(LazarusDirectory));
      Params.Add('FPCDIR='+FPCDirectory); //Make sure our FPC units can be found by Lazarus
      Params.Add('UPXPROG=echo'); //Don't use UPX
      Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      if LazarusOPT<>'' then
        Params.Add('OPT='+LazarusOPT);
      Params.Add('clean');
      Params.Add('all');
      infoln('Lazarus: running make clean all:');
      if (Run(Executable, Params, CustomPath)) <> 0 then
      begin
        OperationSucceeded := False;
        FInstalledLazarus:= '//*\\error//\\'; //todo: check if this really is an invalid filename. it should be.
      end
      else
      begin
        FInstalledLazarus:=IncludeTrailingPathDelimiter(LazarusDirectory)+'lazarus'+FExecutableExtension;
      end;
    finally
      Params.Free;
    end;
  end;

  if OperationSucceeded then
  begin
    // Set up a minimal config so we can use LazBuild
    LazarusConfig:=TUpdateLazConfig.Create(LazarusPrimaryConfigPath);
    try
      try
        LazarusConfig.LazarusDirectory:=LazarusDirectory;
        {$IFDEF MSWINDOWS}
        // FInstalledCompiler will be often something like c:\bla\ppc386.exe, e.g.
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
    // Make bigide: ide with additional packages as specified by user (in primary config path?)
    // this should also make the lhelp package needed for CHM Help.
    Executable := FMake;
    Params:=TStringList.Create;
    try
      Params.Add('FPC='+FInstalledCompiler);
      {$IFDEF MSWINDOWS}
      // Some binutils as (assembler) and ld (linker) may not be in path, or the wrong ones may be there.
      // Specify the ones the compiler should use:
      // We can rely on binutils being copied to compiler bin path here:
      Params.Add('OPT=-FD'+ExtractFilePath(FInstalledCompiler));
      //Use CROSSBINDIR to specify binutils directly called by make (not via FPC)
      Params.Add('CROSSBINDIR='+ExcludeTrailingPathDelimiter(MakeDirectory));
      {$ENDIF MSWINDOWS}
      Params.Add('--directory='+LazarusDirectory+'');
      Params.Add('UPXPROG=echo'); //Don't use UPX
      Params.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
      Params.Add('bigide');
      infoln('Lazarus: running make bigide:');
      if (Run(Executable, Params, CustomPath)) <> 0 then
        OperationSucceeded := False;
    finally
      Params.Free;
    end;
  end;

  if OperationSucceeded then
  begin
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
        // To fpcup itself, with all options as passed when invoking it:
        if FShortCutNameFpcupIsSet then
          CreateDesktopShortCut(paramstr(0),AllOptions,ShortCutNameFpcup);
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
        if FShortCutNameFpcupIsSet then
          begin
          FAllOptions:=FAllOptions+' $*';
          CreateHomeStartLink(paramstr(0),FAllOptions,ShortCutNameFpcup);
          end;
      finally
        //Ignore problems creating shortcut
      end;
    end;
    {$ENDIF UNIX}
  end;

  if OperationSucceeded then
  begin
    // Build lhelp chm help viewer
    // todo: while this may compile, to integrate help we need to do more. Taken from Laz wiki:
    // configure paths for lhelp
    // Download/update fpc help (see chmhelp readme)
    // Copy all CHM files to lazarus/docs/html
    // same for fpc help
    // Now context sensitive help using F1 should already be working.
    // reinier: I do suspect we will need to adjust a "help files path" somewhere though; see chmhelp readme
    Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
    Params:=TStringList.Create;
    try
      Params.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      Params.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'components'+DirectorySeparator+
        'chmhelp'+DirectorySeparator+
        'lhelp'+DirectorySeparator+
        'lhelp.lpr');
      infoln('Lazarus: compiling lhelp help viewer:');
      if (Run(Executable, Params, CustomPath)) <> 0 then
        OperationSucceeded := False;
    finally
      Params.Free;
    end;
  end;

  if OperationSucceeded then
  begin
    // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
    Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
    Params:=TStringList.Create;
    try
      Params.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      Params.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'docs'+DirectorySeparator+
        'html'+DirectorySeparator+
        'build_lcl_docs.lpr');
      infoln('Lazarus: compiling build_lcl_docs help compiler:');
      if (Run(Executable, Params, CustomPath)) <> 0 then
        OperationSucceeded := False;
    finally
      Params.Free;
    end;
  end;

  //todo: get/compile FPC docs CHM help
  //get .xct files for FPC help so we can use that in build_lcl_docs below

  if OperationSucceeded then
  begin
    // Compile Lazarus CHM help
    Executable := IncludeTrailingPathDelimiter(LazarusDirectory)+
        'docs'+DirectorySeparator+
        'html'+DirectorySeparator+
        'build_lcl_docs'+FExecutableExtension;
    Params:=TStringList.Create;
    try
      //todo: get .xct files from fpc so LCL CHM can link to it??!
      Params.Add('--fpdoc');
      Params.Add(ExtractFilePath(FInstalledCompiler)+'fpdoc'+FExecutableExtension); //fpdoc gets called by build_lcl_docs
      Params.Add('--outfmt');
      Params.Add('chm');
      infoln('Lazarus: compiling chm help docs:');
      //they get output into <lazarusdir>/docs/html/lcl/lcl.chm
      if (Run(Executable, Params, CustomPath)) <> 0 then
        OperationSucceeded := False;
    finally
      Params.Free;
    end;
  end;

  if OperationSucceeded then
  begin
    // Build data desktop, nice example of building with lazbuild
    Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
    Params:=TStringList.Create;
    try
      Params.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      Params.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'tools'+DirectorySeparator+
        'lazdatadesktop'+DirectorySeparator+
        'lazdatadesktop.lpr');
      infoln('Lazarus: compiling data desktop:');
      if (Run(Executable, Params, CustomPath)) <> 0 then
        OperationSucceeded := False;
    finally
      Params.Free;
    end;
  end;

  if OperationSucceeded then
  begin
    // Build Lazarus Doceditor
    Executable := IncludeTrailingPathDelimiter(LazarusDirectory) + 'lazbuild';
    Params:=TStringList.Create;
    try
      Params.Add('--primary-config-path='+FLazarusPrimaryConfigPath+'');
      Params.Add(IncludeTrailingPathDelimiter(LazarusDirectory)+
        'doceditor'+DirectorySeparator+
        'lazde.lpr');
      infoln('Lazarus: compiling doc editor:');
      if (Run(Executable, Params, CustomPath)) <> 0 then
        OperationSucceeded := False;
    finally
      Params.Free;
    end;
  end;

  if OperationSucceeded then
    writeln(FLogFile,'Lazarus update succeeded at revision number ', AfterRevision);
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
  FShortCutNameFpcupIsSet:=false;
  //Bootstrap CompilerName:
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

  {$IFDEF MSWINDOWS}
  FExecutableExtension := '.exe';
  {$ELSE}
  FExecutableExtension := '';
  {$ENDIF MSWINDOWS}
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
end;

destructor Tinstaller.Destroy;
begin
  WriteLn(FLogFile,DateTimeToStr(now),': fpcup finished.');
  CloseFile(FLogFile);
  FUpdater.Free;
  FBinUtils.Free;
  inherited Destroy;
end;

end.

