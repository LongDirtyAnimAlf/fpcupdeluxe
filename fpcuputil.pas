{ Utility unit for various FPCup versions
Copyright (C) 2012-2014 Reinier Olislagers, Ludo Brands

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

unit fpcuputil;
{ Utility functions that might be needed by fpcup core and plugin units }

//{$mode DELPHI}{$H+}
{$mode objfpc}{$H+}

{Define NOCONSOLE e.g. if using Windows GUI {$APPTYPE GUI} or -WG
this will disable writeln calls
}
{not $DEFINE NOCONSOLE}

{$define ENABLEWGET}

interface

uses
  Classes, SysUtils,
  zipper,
  fphttpclient,
  sslsockets, fpopenssl,
  //fpftpclient,
  eventlog;

Const
  // Maximum retries when downloading a file
  DefMaxRetries = 5;

type
  //callback = class
  //  class procedure Status (Sender: TObject; Reason: THookSocketReason; const Value: String);
  //end;

  {TThreadedUnzipper}

  TOnZipProgress = procedure(Sender: TObject; FPercent: double) of object;
  TOnZipFile = procedure(Sender: TObject; AFileName : string; FileCount,TotalFileCount:cardinal) of object;
  TOnZipCompleted = TNotifyEvent;

  TThreadedUnzipper = class(TThread)
  private
    FStarted: Boolean;
    FErrMsg: String;
    FUnZipper: TUnZipper;
    FPercent: double;
    FFileCount: cardinal;
    FFileList:TStrings;
    FTotalFileCount: cardinal;
    FCurrentFile: string;
    FOnZipProgress: TOnZipProgress;
    FOnZipFile: TOnZipFile;
    FOnZipCompleted: TOnZipCompleted;
    procedure DoOnProgress(Sender : TObject; Const Pct : Double);
    procedure DoOnFile(Sender : TObject; Const AFileName : string);
    procedure DoOnZipProgress;
    procedure DoOnZipFile;
    procedure DoOnZipCompleted;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoUnZip(const ASrcFile, ADstDir: String; Files:array of string);
  published
    property OnZipProgress: TOnZipProgress read FOnZipProgress write FOnZipProgress;
    property OnZipFile: TOnZipFile read FOnZipFile write FOnZipFile;
    property OnZipCompleted: TOnZipCompleted read FOnZipCompleted write FOnZipCompleted;
  end;

  TNormalUnzipper = class(TObject)
  private
    FUnZipper: TThreadedUnzipper;
    procedure DoOnZipFile(Sender: TObject; aFile: string; FileCnt, TotalFileCnt:cardinal);
  public
    function DoUnZip(const ASrcFile, ADstDir: String; Files:array of string):boolean;
  end;


  { TLogger }
  TLogger = class(TObject)
  private
    FLog: TEventLog; //Logging/debug output to file
    function GetLogFile: string;
    procedure SetLogFile(AValue: string);
  public
    // Write to log and optionally console with seriousness etInfo
    procedure WriteLog(Message: string; ToConsole: Boolean=true);overload;
    // Write to log and optionally console with specified seriousness
    procedure WriteLog(EventType: TEventType;Message: string; ToConsole: Boolean);overload;
    property LogFile: string read GetLogFile write SetLogFile ;
    constructor Create;
    destructor Destroy; override;
  end;

  TBasicDownLoader = Class(TComponent)
  private
    FVerbose:boolean;
    FMaxRetries:byte;
    FUsername: string;
    FPassword: string;
    FHTTPProxyHost: string;
    FHTTPProxyPort: integer;
    FHTTPProxyUser: string;
    FHTTPProxyPassword: string;
    procedure parseFTPHTMLListing(F:TStream;filelist:TStringList);
  protected
    procedure SetVerbose(aValue:boolean);virtual;
    property MaxRetries : Byte Read FMaxRetries Write FMaxRetries default DefMaxRetries;
    property Username: string read FUsername;
    property Password: string read FPassword;
    property HTTPProxyHost: string read FHTTPProxyHost;
    property HTTPProxyPort: integer read FHTTPProxyPort;
    property HTTPProxyUser: string read FHTTPProxyUser;
    property HTTPProxyPassword: string read FHTTPProxyPassword;
    property Verbose: boolean write SetVerbose;
  public
    constructor Create;virtual;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure setCredentials(user,pass:string);virtual;
    procedure setProxy(host:string;port:integer;user,pass:string);virtual;
    function getFile(const URL,filename:string):boolean;virtual;abstract;
    function getFTPFileList(const URL:string; filelist:TStringList):boolean;virtual;abstract;
    function checkURL(const URL:string):boolean;virtual;abstract;
  end;


  TUseNativeDownLoader = Class(TBasicDownLoader)
  private
    aFPHTTPClient:TFPHTTPClient;
    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoHeaders(Sender : TObject);
    procedure DoPassword(Sender: TObject; var RepeatRequest: Boolean);
    procedure ShowRedirect(ASender : TObject; Const ASrc : String; Var ADest : String);
    function Download(const URL: String; filename:string):boolean;
  protected
    procedure SetVerbose(aValue:boolean);override;
    function FTPDownload(Const URL : String; filename:string):boolean;
    function HTTPDownload(Const URL : String; filename:string):boolean;
  public
    constructor Create;override;
    destructor Destroy; override;
    procedure setProxy(host:string;port:integer;user,pass:string);override;
    function getFile(const URL,filename:string):boolean;override;
    function getFTPFileList(const URL:string; filelist:TStringList):boolean;override;
    function checkURL(const URL:string):boolean;override;
  end;

  {$IFDEF ENABLEWGET}
  TUseWGetDownloader = Class(TBasicDownLoader)
  private
    function WGetDownload(Const URL : String; Dest : TStream):boolean;
    function LibCurlDownload(Const URL : String; Dest : TStream):boolean;
    function WGetFTPFileList(const URL:string; filelist:TStringList):boolean;
    function LibCurlFTPFileList(const URL:string; filelist:TStringList):boolean;
    function Download(const URL: String; Dest: TStream):boolean;
  protected
    function FTPDownload(Const URL : String; Dest : TStream):boolean;
    function HTTPDownload(Const URL : String; Dest : TStream):boolean;
  public
    function getFile(const URL,filename:string):boolean;override;
    function getFTPFileList(const URL:string; filelist:TStringList):boolean;override;
    function checkURL(const URL:string):boolean;override;
  end;

  {$ENDIF}

  TNativeDownloader = TUseNativeDownLoader;
  {$IFDEF Darwin}
  TWGetDownloader = TUseNativeDownLoader;
  {$else}
  {$IFDEF ENABLEWGET}
  TWGetDownloader = TUseWGetDownloader;
  {$ELSE}
  TWGetDownloader = TUseNativeDownLoader;
  {$ENDIF}
  {$endif}

// Create shortcut on desktop to Target file
procedure CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string) ;
// Create shell script in ~ directory that links to Target
procedure CreateHomeStartLink(Target, TargetArguments, ShortcutName: string);
{$IFDEF MSWINDOWS}
// Delete shortcut on desktop
procedure DeleteDesktopShortcut(ShortcutName: string);
{$ENDIF MSWINDOWS}
// Delete directory and children, even read-only. Equivalent to rm -rf <directory>:
function DeleteDirectoryEx(DirectoryName: string): boolean;
// Recursively delete files with specified name(s), only if path contains specfied directory name somewhere (or no directory name specified):
function DeleteFilesSubDirs(const DirectoryName: string; const Names:TStringList; const OnlyIfPathHas: string): boolean;
// Recursively delete files with specified extension(s),
// only if path contains specfied directory name somewhere (or no directory name specified):
function DeleteFilesExtensionsSubdirs(const DirectoryName: string; const Extensions:TstringList; const OnlyIfPathHas: string): boolean;
// only if filename contains specfied part somewhere
function DeleteFilesNameSubdirs(const DirectoryName: string; const OnlyIfNameHas: string): boolean;
function GetFileNameFromURL(URL:string):string;
function GetVersionFromUrl(URL:string): string;
// Download from HTTP (includes Sourceforge redirection support) or FTP
// HTTP download can work with http proxy
function Download(UseWget:boolean; URL, TargetFile: string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
{$IFDEF MSWINDOWS}
// Get Windows major and minor version number (e.g. 5.0=Windows 2000)
function GetWin32Version(var Major,Minor,Build : Integer): Boolean;
function IsWindows64: boolean;
{$ENDIF}
//check if there is at least one directory between Dir and root
function ParentDirectoryIsNotRoot(Dir:string):boolean;
{$IFDEF MSWINDOWS}
// Get path for Windows per user storage of application data. Useful for storing settings
function GetLocalAppDataPath: string;
{$ENDIF MSWINDOWS}
// Shows non-debug messages on screen (no logging); also shows debug messages if DEBUG defined
procedure infoln(Message: string; Level: TEventType);
// Moves file if it exists, overwriting destination file
function MoveFile(const SrcFilename, DestFilename: string): boolean;
// Like ExpandFilename but does not expand an empty string to current directory
function SafeExpandFileName (Const FileName : String): String;
// Like ExpandFilenameUTF8 but does not expand an empty string to current directory
function SafeExpandFileNameUTF8 (Const FileName : String): String;
// Get application path
function SafeGetApplicationPath: String;
// Copies specified resource (e.g. fpcup.ini, settings.ini)
// to application directory
procedure SaveInisFromResource(filename,resourcename:string);
// Searches for SearchFor in the stringlist and returns the index if found; -1 if not
// Search optionally starts from position SearchFor
function StringListStartsWith(SearchIn:TStringList; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
{$IFDEF UNIX}
function XdgConfigHome: String;
function GetGCCDirectory:string;
{$ENDIF UNIX}
// Emulates/runs which to find executable in path. If not found, returns empty string
function Which(Executable: string): string;
function ExtractFileNameOnly(const AFilename: string): string;
function GetCompilerName(Cpu_Target:string):string;
function GetCrossCompilerName(Cpu_Target:string):string;
function DoubleQuoteIfNeeded(FileName: string): string;
function GetNumericalVersion(aVersion: string): word;
function UppercaseFirstChar(s: String): String;

implementation

uses
  {$ifdef LCL}
  Forms,Controls,
  {$endif}
  IniFiles,
  DOM,DOM_HTML,SAX_HTML,
  ftpsend {for downloading from ftp},
  FileUtil, LazFileUtils, LazUTF8,
  strutils,uriparser
  {$IFDEF MSWINDOWS}
    //Mostly for shortcut code
    ,windows, shlobj {for special folders}, ActiveX, ComObj
  {$ENDIF MSWINDOWS}
  {$IFDEF ENABLEWGET}
  // for wget downloader
  ,process
  {$IFDEF UNIX}
  ,baseunix
  {$ENDIF}
  ,processutils
  // for libc downloader
  ,fpcuplibcurl
  {$ENDIF ENABLEWGET}
  ;

const
  USERAGENT = 'curl/7.50.1 (i686-pc-linux-gnu) libcurl/7.50.1 OpenSSL/1.0.1t zlib/1.2.8 libidn/1.29 libssh2/1.4.3 librtmp/2.3';
  //USERAGENT = 'Mozilla/5.0 (compatible; fpweb)';
  //USERAGENT = 'Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)';
  CURLUSERAGENT='curl/7.51.0';


{$ifdef mswindows}
function GetWin32Version(var Major,Minor,Build : Integer): Boolean;
var
  Info: TOSVersionInfo;
begin
Info.dwOSVersionInfoSize := SizeOf(Info);
if GetVersionEx(Info) then
begin
  with Info do
  begin
    Win32Platform:=dwPlatformId;
    Major:=dwMajorVersion;
    Minor:=dwMinorVersion;
    Build:=dwBuildNumber;
    result:=true
  end;
end
else
  result:=false;
end;

function IsWindows64: boolean;
  {
  Detect if we are running on 64 bit Windows or 32 bit Windows,
  independently of bitness of this program.
  Original source:
  http://www.delphipraxis.net/118485-ermitteln-ob-32-bit-oder-64-bit-betriebssystem.html
  modified for FreePascal in German Lazarus forum:
  http://www.lazarusforum.de/viewtopic.php?f=55&t=5287
  }
{$ifdef WIN32} //Modified KpjComp for 64bit compile mode
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
      Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL; // Result from IsWow64Process
  IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
    Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
      raise SysUtils.Exception.Create('IsWindows64: bad process handle');
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
{$else} //if were running 64bit code, OS must be 64bit :)
begin
  Result := True;
{$endif}
end;

{$endif}

function SafeExpandFileName (Const FileName : String): String;
begin
  if FileName='' then
    result:=''
  else
    result:=ExpandFileName(FileName);
end;

function SafeExpandFileNameUTF8 (Const FileName : String): String;
begin
  if FileName='' then
    result:=''
  else
    result:=ExpandFileNameUTF8(FileName);
end;

function SafeGetApplicationPath: String;
var
  StartPath: String;
  {$ifdef Darwin}
  x:integer;
  {$endif}
begin
 StartPath:=IncludeTrailingPathDelimiter(ProgramDirectory);
 {$ifdef Darwin}
 x:=pos('/Contents/MacOS',StartPath);
 if x>0 then
 begin
   Delete(StartPath,x,MaxInt);
   x:=RPos('/',StartPath);
   if x>0 then
   begin
     Delete(StartPath,x+1,MaxInt);
   end;
 end;
  {$endif}
 if FileIsSymlink(StartPath) then
    StartPath:=GetPhysicalFilename(StartPath,pfeException);
 result:=ExtractFilePath(StartPath);
 if DirectoryExistsUTF8(result) then
    result:=GetPhysicalFilename(result,pfeException);
 result:=AppendPathDelim(result);
end;

procedure SaveInisFromResource(filename,resourcename:string);
var
  fs:Tfilestream;
  ms:TMemoryStream;
  BackupFileName:string;
  Ini:TMemIniFile;
  OldIniVersion,NewIniVersion:string;
begin

  if NOT FileExists(filename) then
  begin
    // create inifile
    with TResourceStream.Create(hInstance, resourcename, RT_RCDATA) do
    try
      try
        fs:=Tfilestream.Create(filename,fmCreate);
        Savetostream(fs);
      finally
        fs.Free;
      end;
    finally
      Free;
    end;
  end
  else
  begin

    // create memory stream of resource
    ms:=TMemoryStream.Create;
    try
      with TResourceStream.Create(hInstance, resourcename, RT_RCDATA) do
      try
        Savetostream(ms);
      finally
        Free;
     end;
     ms.Position:=0;

     Ini:=TMemIniFile.Create(ms);
     {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
     Ini.Options:=[ifoStripQuotes];
     {$ELSE}
     ini.StripQuotes:=true;
     {$ENDIF}
     NewIniVersion:=Ini.ReadString('fpcupinfo','inifileversion','0.0.0.0');
     Ini.Free;

     Ini:=TMemIniFile.Create(filename);
     {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
     Ini.Options:=[ifoStripQuotes];
     {$ELSE}
     ini.StripQuotes:=true;
     {$ENDIF}
     OldIniVersion:=Ini.ReadString('fpcupinfo','inifileversion','0.0.0.0');
     Ini.Free;

     if OldIniVersion<>NewIniVersion then
     begin
       BackupFileName:=ChangeFileExt(filename,'.bak');
       while FileExists(BackupFileName) do BackupFileName := BackupFileName + 'k';
       try
         FileUtil.CopyFile(filename,BackupFileName);
         if SysUtils.DeleteFile(filename) then
         begin
           ms.Position:=0;
           fs := TFileStream.Create(filename,fmCreate);
           try
             fs.CopyFrom(ms, ms.Size);
           finally
             FreeAndNil(fs);
           end;
         end;
       except
         infoln('Could not make a backup copy of old inifile',etError);
       end;
     end;

    finally
      ms.Free;
    end;

  end;

end;

{$IFDEF MSWINDOWS}
procedure CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string);
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
  LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName+'.lnk';

  { Get rid of any existing shortcut first }
  SysUtils.DeleteFile(LinkName);

  { Create the link }
  IPFile.Save(PWChar(LinkName), false);
end;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
procedure CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string);
var
  OperationSucceeded: boolean;
  ResultCode: boolean;
  XdgDesktopContent: TStringList;
  XdgDesktopFile: string;
begin
  // Fail by default:
  OperationSucceeded:=false;
  XdgDesktopFile:=IncludeTrailingPathDelimiter(GetTempDir(false))+'fpcup-'+shortcutname+'.desktop';
  XdgDesktopContent:=TStringList.Create;
  try
    XdgDesktopContent.Add('[Desktop Entry]');
    XdgDesktopContent.Add('Version=1.0');
    XdgDesktopContent.Add('Encoding=UTF-8');
    XdgDesktopContent.Add('Type=Application');
    XdgDesktopContent.Add('Icon='+ExtractFilePath(Target)+'images/icons/lazarus.ico');
    XdgDesktopContent.Add('Exec='+Target+' '+TargetArguments);
    XdgDesktopContent.Add('Name='+ShortcutName);
    XdgDesktopContent.Add('Category=Application;IDE;Development;GUIDesigner;');
    XdgDesktopContent.Add('Keywords=editor;Pascal;IDE;FreePascal;fpc;Design;Designer;');
    //XdgDesktopContent.Add('StartupWMClass=Lazarus');
    //XdgDesktopContent.Add('MimeType=text/x-pascal;');
    //XdgDesktopContent.Add('Patterns=*.pas;*.pp;*.p;*.inc;*.lpi;*.lpk;*.lpr;*.lfm;*.lrs;*.lpl;');
    // We're going to try and call xdg-desktop-icon
    // this may fail if shortcut exists already
    try
      XdgDesktopContent.SaveToFile(XdgDesktopFile);
      OperationSucceeded:=(ExecuteCommand('xdg-desktop-icon install ' + XdgDesktopFile,false)=0);
    except
      OperationSucceeded:=false;
    end;

    if OperationSucceeded=false then
    begin
      infoln('CreateDesktopShortcut: xdg-desktop-icon failed to create shortcut to '+Target,etWarning);
      //infoln('CreateDesktopShortcut: going to create shortcut manually',etWarning);
      //FileUtil.CopyFile(XdgDesktopFile,'/usr/share/applications/'+ExtractFileName(XdgDesktopFile));
    end;
    // Temp file is no longer needed....
    try
      SysUtils.DeleteFile(XdgDesktopFile);
    finally
      // Swallow, let filesystem maintenance clear it up
    end;
  finally
    XdgDesktopContent.Free;
  end;
end;
{$ENDIF UNIX}

procedure CreateHomeStartLink(Target, TargetArguments,
  ShortcutName: string);
var
  ScriptText: TStringList;
  ScriptFile: string;
begin
  {$IFDEF MSWINDOWS}
  infoln('todo: write me (CreateHomeStartLink)!', eterror);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  //create dir if it doesn't exist
  ForceDirectoriesUTF8(ExtractFilePath(IncludeTrailingPathDelimiter(SafeExpandFileNameUTF8('~'))+ShortcutName));
  ScriptText:=TStringList.Create;
  try
    // No quotes here, either, we're not in a shell, apparently...
    ScriptFile:=IncludeTrailingPathDelimiter(SafeExpandFileNameUTF8('~'))+ShortcutName;
    SysUtils.DeleteFile(ScriptFile); //Get rid of any existing remnants
    ScriptText.Add('#!/bin/sh');
    ScriptText.Add('# shortcut generated by fcpup');
    ScriptText.Add(Target+' '+TargetArguments);
    try
      ScriptText.SaveToFile(ScriptFile);
      FPChmod(ScriptFile, &700); //rwx------
    except
      on E: Exception do
        infoln('CreateHomeStartLink: could not create link: '+E.Message,etWarning);
    end;
  finally
    ScriptText.Free;
  end;
  {$ENDIF UNIX}
end;

function GetFileNameFromURL(URL:string):string;
var
  URI:TURI;
  aURL:string;
begin
  if AnsiEndsStr('/download',URL)
     then aURL:=Copy(URL,1,Length(URL)-9)
     else aURL:=URL;
  URI:=ParseURI(aURL);
  result:=URI.Document;
end;

function GetVersionFromUrl(URL:string): string;
var
  VersionSnippet:string;
  i:integer;
  VersionList : TStringList;
  MajorVersion,MinorVersion,ReleaseVersion : string;
begin
  if Pos('trunk',URL)>0 then result:='trunk' else
  begin
    MajorVersion := '0';
    MinorVersion := '0';
    ReleaseVersion := '0';

    VersionSnippet:=UpperCase(URL);

    // find first occurence of _ and delete everything before it
    // if url contains a version, this version always starts with first _

    i := Pos('_',VersionSnippet);
    if i>0 then
    begin
      Delete(VersionSnippet,1,i);
      // ignore release candidate numbering
      i := Pos('_RC',VersionSnippet);
      if i>0 then Delete(VersionSnippet,i,200);
      VersionSnippet:=StringReplace(VersionSnippet,'_',',',[rfReplaceAll]);
    end;

    if Length(VersionSnippet)>0 then
    begin
      VersionList := TStringList.Create;
      try
        VersionList.CommaText := VersionSnippet;
        if VersionList.Count>0 then MajorVersion := VersionList[0];
        if VersionList.Count>1 then MinorVersion := VersionList[1];
        if VersionList.Count>2 then ReleaseVersion := VersionList[2];
      finally
        VersionList.Free;
      end;
    end;
    result:=MajorVersion+'.'+MinorVersion+'.'+ReleaseVersion;

  end;
end;


{$IFDEF MSWINDOWS}
procedure DeleteDesktopShortcut(ShortcutName: string);
var
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of Char;
  LinkName: WideString;
begin
  { Get the desktop location }
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder);
  LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName+'.lnk';
  SysUtils.DeleteFile(LinkName);
end;
{$ENDIF MSWINDOWS}

function DeleteDirectoryEx(DirectoryName: string): boolean;
// Lazarus fileutil.DeleteDirectory on steroids, works like
// deltree <directory>, rmdir /s /q <directory> or rm -rf <directory>
// - removes read-only files/directories (DeleteDirectory doesn't)
// - removes directory itself
// Adapted from fileutil.DeleteDirectory, thanks to Paweł Dmitruk
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        // Remove read-only file attribute so we can delete it:
        if (FileInfo.Attr and faReadOnly)>0 then
          FileSetAttrUTF8(CurFilename, FileInfo.Attr-faReadOnly);
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and faSymLink)=0) {$endif unix} then
        begin
          // Directory; exit with failure on error
          if not DeleteDirectoryEx(CurFilename) then
            begin
            FindCloseUTF8(FileInfo);
            exit;
            end;
        end
        else
        begin
          // File; exit with failure on error
          if not DeleteFileUTF8(CurFilename) then
            begin
            FindCloseUTF8(FileInfo);
            exit;
            end;
        end;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  // Remove root directory; exit with failure on error:
  if (not RemoveDirUTF8(DirectoryName)) then exit;
  Result:=true;
end;

function DeleteFilesSubDirs(const DirectoryName: string;
  const Names: TStringList; const OnlyIfPathHas: string): boolean;
// Deletes all named files starting from DirectoryName and recursing down.
// If the Names are empty, all files will be deleted
// It only deletes files if any directory of the path contains OnlyIfPathHas,
// unless that is empty
// Will try to remove read-only files.
//todo: check how this works with case insensitive file system like Windows
var
  AllFiles: boolean;
  CurSrcDir: String;
  CurFilename: String;
  FileInfo: TSearchRec;
begin
  Result:=false;
  AllFiles:=(Names.Count=0);
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and faSymLink)=0) {$endif unix} then
        begin
          // Directory; call recursively exit with failure on error
          if not DeleteFilesSubDirs(CurFilename,Names,OnlyIfPathHas) then
          begin
            FindCloseUTF8(FileInfo);
            exit;
          end;
        end
        else
        begin
          // If we are in the right path:
          //todo: get utf8 replacement for ExtractFilePath
          if (OnlyIfPathHas='') or
            (pos(DirectorySeparator+OnlyIfPathHas+DirectorySeparator,ExtractFilePath(CurFileName))>0) then
          begin
            // Only delete if file name is right
            //todo: get utf8 extractfilename
            if AllFiles or (Names.IndexOf(ExtractFileName(FileInfo.Name))>=0) then
            begin
              // Remove read-only file attribute so we can delete it:
              if (FileInfo.Attr and faReadOnly)>0 then
                FileSetAttrUTF8(CurFilename, FileInfo.Attr-faReadOnly);
              if not DeleteFileUTF8(CurFilename) then
              begin
                FindCloseUTF8(FileInfo);
                exit;
              end;
            end;
          end;
        end;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  Result:=true;
end;

function DeleteFilesExtensionsSubdirs(const DirectoryName: string; const Extensions:TstringList; const OnlyIfPathHas: string): boolean;
// Deletes all files ending in one of the extensions, starting from
// DirectoryName and recursing down.
// It only deletes files if any directory of the path contains OnlyIfPathHas,
// unless that is empty
// Extensions can contain * to cover everything (other extensions will then be
// ignored), making it delete all files, but leaving the directories.
// Will try to remove read-only files.
//todo: check how this works with case insensitive file system like Windows
var
  AllFiles: boolean;
  CurSrcDir: String;
  CurFilename: String;
  FileInfo: TSearchRec;
  i: integer;
begin
  Result:=false;
  // Make sure we can compare extensions using ExtractFileExt
  for i:=0 to Extensions.Count-1 do
  begin
    if copy(Extensions[i],1,1)<>'.' then Extensions[i]:='.'+Extensions[i];
  end;
  AllFiles:=(Extensions.Count=0) or (Extensions.IndexOf('.*')>=0);
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and faSymLink)=0) {$endif unix} then
        begin
          // Directory; call recursively exit with failure on error
          if not DeleteFilesExtensionsSubdirs(CurFilename, Extensions,OnlyIfPathHas) then
          begin
            FindCloseUTF8(FileInfo);
            exit;
          end;
        end
        else
        begin
          // If we are in the right path:
          //todo: get utf8 replacement for ExtractFilePath
          if (OnlyIfPathHas='') or
            (pos(DirectorySeparator+OnlyIfPathHas+DirectorySeparator,ExtractFilePath(CurFileName))>0) then
          begin
            // Only delete if extension is right
            if AllFiles or (Extensions.IndexOf(ExtractFileExt(FileInfo.Name))>=0) then
            begin
              // Remove read-only file attribute so we can delete it:
              if (FileInfo.Attr and faReadOnly)>0 then
                FileSetAttrUTF8(CurFilename, FileInfo.Attr-faReadOnly);
              if not DeleteFileUTF8(CurFilename) then
              begin
                FindCloseUTF8(FileInfo);
                exit;
              end;
            end;
          end;
        end;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  Result:=true;
end;

function DeleteFilesNameSubdirs(const DirectoryName: string; const OnlyIfNameHas: string): boolean;
// Deletes all files containing OnlyIfNameHas
// DirectoryName and recursing down.
// Will try to remove read-only files.
//todo: check how this works with case insensitive file system like Windows
var
  AllFiles: boolean;
  CurSrcDir: String;
  CurFilename: String;
  FileInfo: TSearchRec;
  i: integer;
begin
  Result:=false;
  AllFiles:=(Length(OnlyIfNameHas)=0);

  // for now, exit when no filename data is given ... use DeleteDirectoryEx
  if AllFiles then exit;

  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or faSymLink {$endif unix},FileInfo)=0 then
  begin
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and faSymLink)=0) {$endif unix} then
        begin
          // Directory; call recursively exit with failure on error
          if not DeleteFilesNameSubdirs(CurFilename, OnlyIfNameHas) then
          begin
            FindCloseUTF8(FileInfo);
            exit;
          end;
        end
        else
        begin
          if AllFiles or (Pos(UpperCase(OnlyIfNameHas),UpperCase(FileInfo.Name))>0) then
          begin
            // Remove read-only file attribute so we can delete it:
            if (FileInfo.Attr and faReadOnly)>0 then
              FileSetAttrUTF8(CurFilename, FileInfo.Attr-faReadOnly);
            if not DeleteFileUTF8(CurFilename) then
            begin
              FindCloseUTF8(FileInfo);
              exit;
            end;
          end;
        end;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  Result:=true;
end;

function DownloadBase(aDownLoader:TBasicDownloader;URL, TargetFile: string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
begin
  result:=false;
  if Length(HTTPProxyHost)>0 then aDownLoader.setProxy(HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
  result:=aDownLoader.getFile(URL,TargetFile);
  if (NOT result) then // try only once again in case of error
  begin
    infoln('Error while trying to download '+URL+'. Trying again.',etDebug);
    SysUtils.DeleteFile(TargetFile); // delete stale targetfile
    result:=aDownLoader.getFile(URL,TargetFile);
  end;
end;


function Download(UseWget:boolean; URL, TargetFile: string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
var
  aDownLoader:TBasicDownLoader;
begin
  result:=false;
  if UseWget
     then aDownLoader:=TWGetDownLoader.Create
     else aDownLoader:=TNativeDownLoader.Create;
  try
    result:=DownloadBase(aDownLoader,URL,TargetFile,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
  finally
    aDownLoader.Destroy;
  end;
end;

// returns file size in bytes or 0 if not found.
function FileSize(FileName: string) : Int64;
//function FileSizeUTF8(FileName: string) : Int64;
var
  sr : TRawByteSearchRec;
  //sr : TSearchRec;
begin
{$ifdef unix}
  result:=filesize(FileName);
{$else}
  if SysUtils.FindFirst(FileName, faAnyFile, sr ) = 0 then
  //if FindFirstUTF8(FileName, faAnyFile, sr ) = 0 then
     result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
  else
     result := 0;
  SysUtils.FindClose(sr);
  //FindCloseUTF8(sr);
{$endif}
end;

function ParentDirectoryIsNotRoot(Dir: string): boolean;
var s:string;
begin
  result:=false;
  Dir:=ExcludeTrailingBackslash(Dir);
  s:=ExtractFileDir(Dir);
  if s<>Dir then //to avoid fe. c:\\\
    begin  // this is one level up
    Dir:=ExcludeTrailingBackslash(s);
    s:=ExtractFileDir(Dir);
    result:=s<>Dir; //to avoid fe. c:\\\
    end;
end;

{$IFDEF MSWINDOWS}

function GetLocalAppDataPath: string;
var
  AppDataPath: array[0..MaxPathLen] of char; //Allocate memory
begin
  AppDataPath := '';
  SHGetSpecialFolderPath(0, AppDataPath, CSIDL_LOCAL_APPDATA, False);
  result:=AppDataPath;
end;
{$ENDIF MSWINDOWS}

procedure infoln(Message: string; Level: TEventType);
const
  {$ifdef LCL}
  BeginSnippet='fpcupdeluxe: '; //helps identify messages as coming from fpcupdeluxe instead of make etc
  {$else}
  {$ifndef FPCONLY}
  BeginSnippet='fpclazup: '; //helps identify messages as coming from fpclazup instead of make etc
  {$else}
  BeginSnippet='fpcup: '; //helps identify messages as coming from fpcup instead of make etc
  {$endif}
  {$endif}
  Seriousness: array [TEventType] of string = ('custom:', 'info:', 'WARNING:', 'ERROR:', 'debug:');
begin
{$IFNDEF NOCONSOLE}
  // Note: these strings should remain as is so any fpcupgui highlighter can pick it up
  if (Level<>etDebug) then
    begin
      if AnsiPos(LineEnding, Message)>0 then writeln(''); //Write an empty line before multiline messagse
      writeln(BeginSnippet+Seriousness[Level]+' '+ Message); //we misuse this for info output
      //sleep(200); //hopefully allow output to be written without interfering with other output
      sleep(1);
    end
  else
    begin
    {$IFDEF DEBUG}
    {DEBUG conditional symbol is defined using
    Project Options/Other/Custom Options using -dDEBUG}
    if AnsiPos(LineEnding, Message)>0 then writeln(''); //Write an empty line before multiline messagse
    writeln(BeginSnippet+Seriousness[Level]+' '+ Message); //we misuse this for info output
    //sleep(200); //hopefully allow output to be written without interfering with other output
    sleep(1);
    {$ENDIF}
    end;
{$ENDIF NOCONSOLE}
end;

function MoveFile(const SrcFilename, DestFilename: string): boolean;
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

function StringListStartsWith(SearchIn:TStringList; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  for i:=StartIndex to SearchIn.Count-1 do
  begin
    if CS then
    begin
      if copy(SearchIn[i],1,length(SearchFor))=SearchFor then
      begin
        Found:=true;
        break;
      end;
    end
    else
    begin
      if UpperCase(copy(SearchIn[i],1,length(SearchFor)))=UpperCase(SearchFor) then
      begin
        Found:=true;
        break;
      end;
    end;
  end;
  if Found then
    result:=i
  else
    result:=-1;
end;

{$IFDEF UNIX}
function GetGCCDirectory:string;
var
  output,s1,s2:string;
  i:integer;
begin

  result:='/usr/lib/gcc/';
  output:='';

  try
    ExecuteCommand('gcc -v', Output, false);
    s1:=' --build=';
    i:=Ansipos(s1, Output);
    if i > 0 then
    begin
      s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
      i:=Ansipos(' ', s2);
      if i > 0 then delete(s2,i,MaxInt);
      result:=result+s2+'/';
    end;
    s1:='gcc version ';
    i:=Ansipos(s1, Output);
    if i > 0 then
    begin
      s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
      i:=Ansipos(' ', s2);
      if i > 0 then delete(s2,i,MaxInt);
      result:=result+s2;
    end;
  except
    // ignore errors
  end;
end;
{$ENDIF UNIX}

function Which(Executable: string): string;
var
  Output: string;
begin
  {$IFDEF UNIX}
  // Note: we're using external which because
  // FindDefaultExecutablePath
  // or
  // ExeSearch(Executable);
  // doesn't check if the user has execute permission
  // on the found file.
  // however
  // ExeSearch(Executable) ... if fpAccess (Executable,X_OK)=0 then ..... see http://www.freepascal.org/docs-html/rtl/baseunix/fpaccess.html
  ExecuteCommand('which '+Executable,Output,false);
  // Remove trailing LF(s) and other control codes:
  while (length(output)>0) and (ord(output[length(output)])<$20) do
    delete(output,length(output),1);
  {$ELSE}
  Output:=FindDefaultExecutablePath(Executable);
  {$ENDIF UNIX}
  // We could have checked for ExecuteCommandHidden exitcode, but why not
  // do file existence check instead:
  if (Output<>'') and fileexists(Output) then
  begin
    result:=Output;
  end
  else
  begin
    result:=''; //command failed
  end;
end;

{$IFDEF UNIX}
//Adapted from sysutils; Unix/Linux only
Function XdgConfigHome: String;
{ Follows base-dir spec,
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
begin
  Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result='') then
    Result:=IncludeTrailingPathDelimiter(SafeExpandFileNameUTF8('~'))+'.config'+DirectorySeparator
  else
    Result:=IncludeTrailingPathDelimiter(Result);
end;
{$ENDIF UNIX}

function ExtractFileNameOnly(const AFilename: string): string;
var
  StartPos: Integer;
  ExtPos: Integer;
begin
  StartPos:=length(AFilename)+1;
  while (StartPos>1)
  and not (AFilename[StartPos-1] in AllowDirectorySeparators)
  {$IFDEF Windows}and (AFilename[StartPos-1]<>':'){$ENDIF}
  do
    dec(StartPos);
  ExtPos:=length(AFilename);
  while (ExtPos>=StartPos) and (AFilename[ExtPos]<>'.') do
    dec(ExtPos);
  if (ExtPos<StartPos) then ExtPos:=length(AFilename)+1;
  Result:=copy(AFilename,StartPos,ExtPos-StartPos);
end;

// from Lazarus: unit DefineTemplates;
function GetDefaultCompilerFilename(const TargetCPU: string;
  Cross: boolean): string;
begin
  if Cross then
    Result:='ppcross'
  else
    Result:='ppc';
  if TargetCPU='' then
    Result:='fpc'
  else if SysUtils.CompareText(TargetCPU,'i386')=0 then
    Result:=Result+'386'
  else if SysUtils.CompareText(TargetCPU,'m68k')=0 then
    Result:=Result+'86k'
  else if SysUtils.CompareText(TargetCPU,'alpha')=0 then
    Result:=Result+'apx'
  else if SysUtils.CompareText(TargetCPU,'powerpc')=0 then
    Result:=Result+'ppc'
  else if SysUtils.CompareText(TargetCPU,'powerpc64')=0 then
    Result:=Result+'ppc64'
  else if SysUtils.CompareText(TargetCPU,'arm')=0 then
    Result:=Result+'arm'
  else if SysUtils.CompareText(TargetCPU,'armeb')=0 then
    Result:=Result+'arm'
  else if SysUtils.CompareText(TargetCPU,'avr')=0 then
    Result:=Result+'avr'
  else if SysUtils.CompareText(TargetCPU,'sparc')=0 then
    Result:=Result+'sparc'
  else if SysUtils.CompareText(TargetCPU,'x86_64')=0 then
    Result:=Result+'x64'
  else if SysUtils.CompareText(TargetCPU,'ia64')=0 then
    Result:=Result+'ia64'
  else if SysUtils.CompareText(TargetCPU,'aarch64')=0 then
    Result:=Result+'a64'
  else if SysUtils.CompareText(TargetCPU,'i8086')=0 then
    Result:=Result+'8086'
  else
    Result:='fpc';
  Result:=Result+GetExeExt;
end;

function GetCompilerName(Cpu_Target:string):string;
begin
  result:=GetDefaultCompilerFilename(Cpu_Target,false);
end;

function GetCrossCompilerName(Cpu_Target:string):string;
begin
  if Cpu_Target<>'jvm'
     then result:=GetDefaultCompilerFilename(Cpu_Target,true)
     else result:=GetDefaultCompilerFilename(Cpu_Target,false);
end;

function DoubleQuoteIfNeeded(FileName: string): string;
begin
  {$IFDEF MSWINDOWS}
  // Unfortunately, we need to double quote in case there's spaces in the path and it's e.g. a .cmd file
  result:=Trim(FileName);
  if Pos(' ',result)>0 then
  //if Copy(FileName, 1, 1) <> '"' then
     Result := '"' + Result + '"';
  {$ELSE}
  Result := filename;
  {$ENDIF}
end;

function GetNumericalVersion(aVersion: string): word;
begin
  result := 0;
  if length(aVersion)=5 then
  begin
    result := ((ord(aVersion[1])-ord('0')) * 10000)+
                ((ord(aVersion[3])-ord('0')) * 100)+
                (ord(aVersion[5])-ord('0'));
  end;
end;

function UppercaseFirstChar(s: String): String;
var
  ch, rest: String;
begin
  ch := Copy(s, 1, 1);
  rest := Copy(s, Length(ch)+1, MaxInt);
  result := UpperCase(ch) + LowerCase(rest);
end;

{TUnzipper}

procedure TThreadedUnzipper.DoOnZipProgress;
begin
  if Assigned(FOnZipProgress) then
    FOnZipProgress(Self, FPercent);
end;

procedure TThreadedUnzipper.DoOnZipFile;
begin
  if Assigned(FOnZipFile) then
    FOnZipFile(Self, FCurrentFile, FFileCount, FTotalFileCount);
end;

procedure TThreadedUnzipper.DoOnZipCompleted;
begin
  if Assigned(FOnZipCompleted) then
    FOnZipCompleted(Self);
end;

procedure TThreadedUnzipper.Execute;
var
  x:cardinal;
  s:string;
begin
  try
    FUnZipper.Examine;

    {$ifdef MSWINDOWS}
    // on windows, .files (hidden files) cannot be created !!??
    // still to check on non-windows
    if FFileList.Count=0 then
    begin
      for x:=0 to FUnZipper.Entries.Count-1 do
      begin
        if FUnZipper.UseUTF8
          then s:=FUnZipper.Entries.Entries[x].UTF8ArchiveFileName
          else s:=FUnZipper.Entries.Entries[x].ArchiveFileName;
        if (Pos('/.',s)>0) OR (Pos('\.',s)>0) then continue;
        if (Length(s)>0) AND (s[1]='.') then continue;
        FFileList.Append(s);
      end;
    end;
    {$endif}

    if FFileList.Count=0
      then FTotalFileCount:=FUnZipper.Entries.Count
      else FTotalFileCount:=FFileList.Count;

    if FFileList.Count=0
      then FUnZipper.UnZipAllFiles
      else FUnZipper.UnZipFiles(FFileList);
  except
    on E: Exception do
    begin
      FErrMsg := E.Message;
      //Synchronize(@DoOnZipError);
    end;
  end;
  Synchronize(@DoOnZipCompleted);
end;

constructor TThreadedUnzipper.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FUnZipper := TUnZipper.Create;
  FFileList := TStringList.Create;
  FStarted := False;
end;

destructor TThreadedUnzipper.Destroy;
begin
  FFileList.Free;
  FUnZipper.Free;
  inherited Destroy;
end;


procedure TThreadedUnzipper.DoOnProgress(Sender : TObject; Const Pct : Double);
begin
  FPercent:=Pct;
  Synchronize(@DoOnZipProgress);
end;

procedure TThreadedUnzipper.DoOnFile(Sender : TObject; Const AFileName : String);
begin
  Inc(FFileCount);
  FCurrentFile:=ExtractFileNAme(AFileName);
  Synchronize(@DoOnZipFile);
end;


procedure TThreadedUnzipper.DoUnZip(const ASrcFile, ADstDir: String; Files:array of string);
var
  i:word;
begin
  if FStarted then exit;
  FUnZipper.Clear;
  FUnZipper.OnPercent:=10;
  FUnZipper.FileName := ASrcFile;
  FUnZipper.OutputPath := ADstDir;
  FUnZipper.OnProgress := @DoOnProgress;
  FUnZipper.OnStartFile:= @DoOnFile;
  FFileList.Clear;
  if Length(Files)>0 then
    for i := 0 to high(Files) do
      FFileList.Append(Files[i]);
  FPercent:=0;
  FFileCount:=0;
  FTotalFileCount:=0;
  FStarted := True;
  Start;
end;

procedure TNormalUnzipper.DoOnZipFile(Sender: TObject; aFile: string; FileCnt, TotalFileCnt:cardinal);
begin
  if TotalFileCnt>50000 then
  begin
    if (FileCnt MOD 5000)=0 then infoln('Extracted #'+InttoStr(FileCnt)+' files out of #'+InttoStr(TotalFileCnt),etInfo);
  end
  else
  if TotalFileCnt>5000 then
  begin
    if (FileCnt MOD 500)=0 then infoln('Extracted #'+InttoStr(FileCnt)+' files out of #'+InttoStr(TotalFileCnt),etInfo);
  end
  else
  if TotalFileCnt>500 then
  begin
    if (FileCnt MOD 50)=0 then infoln('Extracted #'+InttoStr(FileCnt)+' files out of #'+InttoStr(TotalFileCnt),etInfo);
  end
  else
  if TotalFileCnt>50 then
  begin
    if (FileCnt MOD 5)=0 then infoln('Extracted #'+InttoStr(FileCnt)+' files out of #'+InttoStr(TotalFileCnt),etInfo);
  end
  else
    infoln('Extracting '+aFile+'. #'+InttoStr(FileCnt)+' out of #'+InttoStr(TotalFileCnt),etInfo);
end;

function TNormalUnzipper.DoUnZip(const ASrcFile, ADstDir: String; Files:array of string):boolean;
begin
  result:=false;
  FUnzipper := TThreadedUnzipper.Create;
  try
    FUnzipper.FreeOnTerminate:=False;
    FUnzipper.OnZipFile := @DoOnZipFile;
    FUnzipper.DoUnZip(ASrcFile, ADstDir, Files);
    FUnzipper.WaitFor;
    FUnzipper.Free;
    result:=true;
  except
    FUnzipper.Free;
  end;
end;



{ TLogger }

function TLogger.GetLogFile: string;
begin
  result:=FLog.FileName;
end;

procedure TLogger.SetLogFile(AValue: string);
begin
  if AValue<>FLog.FileName then
  begin
    FLog.Active:=false;//save WriteLog output
    FLog.FileName:=AValue;
  end;
end;

procedure TLogger.WriteLog(Message: string; ToConsole: Boolean);
begin
  FLog.Log(etInfo, Message);
  if ToConsole then infoln(Message,etInfo);
end;

procedure TLogger.WriteLog(EventType: TEventType;Message: string; ToConsole: Boolean);
begin
  FLog.Log(EventType, Message);
  if ToConsole then infoln(Message,EventType);
end;

constructor TLogger.Create;
begin
  FLog:=TEventLog.Create(nil);
  FLog.LogType:=ltFile;
  FLog.AppendContent:=true;
  FLog.RaiseExceptionOnError:=false; //Don't throw exceptions on log errors.
end;

destructor TLogger.Destroy;
begin
  FLog.Active:=false;//save WriteLog text
  FLog.Free;
  inherited Destroy;
end;

constructor TBasicDownLoader.Create;
begin
  Inherited Create(nil);
end;

constructor TBasicDownLoader.Create(AOwner: TComponent);
begin
  inherited;
  FVerbose:=False;
  FUsername:='';
  FPassword:='';
  FHTTPProxyHost:='';
  FHTTPProxyPort:=0;
  FHTTPProxyUser:='';
  FHTTPProxyPassword:='';
end;

destructor TBasicDownLoader.Destroy;
begin
  inherited;
end;

procedure TBasicDownLoader.SetVerbose(aValue:boolean);
begin
  FVerbose:=aValue;
end;

procedure TBasicDownLoader.setCredentials(user,pass:string);
begin
  FUsername:=user;
  FPassword:=pass;
end;

procedure TBasicDownLoader.setProxy(host:string;port:integer;user,pass:string);
begin
  FHTTPProxyHost:=host;
  FHTTPProxyPort:=port;
  FHTTPProxyUser:=user;
  FHTTPProxyPassword:=pass;
end;

procedure TBasicDownLoader.parseFTPHTMLListing(F:TStream;filelist:TStringList);
var
  ADoc: THTMLDocument;
  HTMFiles : TDOMNodeList;
  HTMFile : TDOMNode;
  FilenameValid:boolean;
  i,j:integer;
  s:string;
begin
  F.Position:=0;
  try
    ReadHTMLFile(ADoc,F);
    // a bit rough, but it works
    HTMFiles:=ADoc.GetElementsByTagName('a');
    for i:=0 to HTMFiles.Count-1 do
    begin
      HtmFile:=HTMFiles.Item[i];
      s:=HtmFile.TextContent;
      if Length(s)>0 then
      begin
        // validate filename (also rough)
        FilenameValid:=True;
        for j:=1 to Length(s) do
        begin
          FilenameValid := (NOT SysUtils.CharInSet(s[j], [';', '=', '+', '<', '>', '|','"', '[', ']', '\', '/', '''']));
          if (NOT FilenameValid) then break;
        end;
        if FilenameValid then FilenameValid:=(Pos('..',s)=0);
        // restrict ourselves to zip and bz2 ... we only use this to retrieve lists of bootstrapper archives !
        if FilenameValid then FilenameValid:=((LowerCase(ExtractFileExt(s))='.zip') OR (LowerCase(ExtractFileExt(s))='.bz2'));
        // finally, add filename if all is ok !!
        if FilenameValid then filelist.Add(s);
      end;
    end;
  finally
    aDoc.Free;
  end;
end;


procedure TUseNativeDownLoader.DoHeaders(Sender : TObject);
Var
  I : Integer;
begin
  writeln('Response headers received:');
  with (Sender as TFPHTTPClient) do
    for I:=0 to ResponseHeaders.Count-1 do
      writeln(ResponseHeaders[i]);
end;

procedure TUseNativeDownLoader.DoProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  If (ContentLength=0) then
    writeln('Reading headers : ',CurrentPos,' Bytes.')
  else If (ContentLength=-1) then
    writeln('Reading data (no length available) : ',CurrentPos,' Bytes.')
  else
    writeln('Reading data : ',CurrentPos,' Bytes of ',ContentLength);
end;

procedure TUseNativeDownLoader.DoPassword(Sender: TObject; var RepeatRequest: Boolean);
Var
  H,UN,PW : String;
  P : Integer;
begin
  if FUsername <> '' then
  begin
    TFPHTTPClient(Sender).UserName:=FUsername;
    TFPHTTPClient(Sender).Password:=FPassword;
  end
  else
  begin

    with TFPHTTPClient(Sender) do
    begin
      H:=GetHeader(ResponseHeaders,'WWW-Authenticate');
    end;
    P:=Pos('realm',LowerCase(H));
    if (P>0) then
    begin
      P:=Pos('"',H);
      Delete(H,1,P);
      P:=Pos('"',H);
      H:=Copy(H,1,Pos('"',H)-1);
    end;

    writeln('Authorization required. Remote site says: ',H);
    write('Enter username (empty quits): ');
    readLn(UN);
    RepeatRequest:=(UN<>'');
    if RepeatRequest then
    begin
      write('Enter password: ');
      readln(PW);
      TFPHTTPClient(Sender).UserName:=UN;
      TFPHTTPClient(Sender).Password:=PW;
    end;

  end;
end;

procedure TUseNativeDownLoader.ShowRedirect(ASender: TObject; const ASrc: String;
  var ADest: String);
begin
  writeln('Following redirect from ',ASrc,'  ==> ',ADest);
end;

constructor TUseNativeDownLoader.Create;
begin
  aFPHTTPClient:=TFPHTTPClient.Create(Nil);
  with aFPHTTPClient do
  begin
    AllowRedirect:=True;
    FMaxRetries:=DefMaxRetries;
    OnPassword:=@DoPassword;
    if FVerbose then
    begin
      OnRedirect:=@ShowRedirect;
      OnDataReceived:=@DoProgress;
      OnHeaders:=@DoHeaders;
    end;
  end;
end;

procedure TUseNativeDownLoader.SetVerbose(aValue:boolean);
begin
  inherited;
  with aFPHTTPClient do
  begin
    if FVerbose then
    begin
      OnRedirect:=@ShowRedirect;
      OnDataReceived:=@DoProgress;
      OnHeaders:=@DoHeaders;
    end
    else
    begin
      OnRedirect:=nil;
      OnDataReceived:=nil;
      OnHeaders:=nil;
    end;
  end;
end;

procedure TUseNativeDownLoader.setProxy(host:string;port:integer;user,pass:string);
begin
  inherited;
  with aFPHTTPClient do
  begin
    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
    Proxy.Host:=FHTTPProxyHost;
    Proxy.Port:=FHTTPProxyPort;
    Proxy.UserName:=FHTTPProxyUser;
    Proxy.Password:=FHTTPProxyPassword;
    {$ENDIF}
  end;
end;

function TUseNativeDownLoader.getFTPFileList(const URL:string; filelist:TStringList):boolean;
var
  i: Integer;
  s: string;
  URI : TURI;
  P : String;
begin
  result:=false;
  URI:=ParseURI(URL);
  P:=URI.Protocol;
  if CompareText(P,'ftp')=0 then
  begin
    with TFTPSend.Create do
    try
      if FUsername <> '' then
      begin
        Username := FUsername;
        Password := FPassword;
      end
      else
      begin
        if Pos('ftp.freepascal.org',URL)>0 then
        begin
          Username := 'anonymous';
          Password := 'fpc@example.com';
        end;
      end;
      if Length(HTTPProxyHost)>0 then
      begin
        Sock.HTTPTunnelIP:=HTTPProxyHost;
        Sock.HTTPTunnelPort:=InttoStr(HTTPProxyPort);
        Sock.HTTPTunnelUser:=HTTPProxyUser;
        Sock.HTTPTunnelPass:=HTTPProxyPassword;
      end;
      TargetHost := URI.Host;
      if not Login then exit;
      Result := List(URI.Path, False);
      Logout;

      for i := 0 to FtpList.Count -1 do
      begin
        s := FTPList[i].FileName;
        filelist.Add(s);
      end;

      if FTPList.Lines.Count>0 then
      begin
        // do we have a HTML lsiting (due to a proxy) ?
        if Pos('<!DOCTYPE HTML',UpperCase(FTPList.Lines.Strings[0]))=1 then
        begin
          parseFTPHTMLListing(DataStream,filelist);
        end;
      end;

    finally
      Free;
    end;
  end;
end;

function TUseNativeDownLoader.FTPDownload(Const URL : String; filename:string):boolean;
var
  URI : TURI;
  aPort:integer;
begin
  // we will use synapse TFTPSend ... FPHTTPClient does not support FTP (yet)
  result:=false;
  URI:=ParseURI(URL);
  aPort:=URI.Port;
  if aPort=0 then aPort:=21;
  Result := False;
  with TFTPSend.Create do
  try
    TargetHost := URI.Host;
    TargetPort := InttoStr(aPort);
    if FUsername <> '' then
    begin
      Username := FUsername;
      Password := FPassword;
    end
    else
    begin
      if Pos('ftp.freepascal.org',URL)>0 then
      begin
        Username := 'anonymous';
        Password := 'fpc@example.com';
      end;
    end;
    if Length(HTTPProxyHost)>0 then
    begin
      Sock.HTTPTunnelIP:=HTTPProxyHost;
      Sock.HTTPTunnelPort:=InttoStr(HTTPProxyPort);
      Sock.HTTPTunnelUser:=HTTPProxyUser;
      Sock.HTTPTunnelPass:=HTTPProxyPassword;
    end;
    if Login then
    begin
      DirectFileName := filename;
      DirectFile:=True;
      Result := RetrieveFile(URI.Path+URI.Document, False);
      Logout;
    end;
  finally
    Free;
  end;
end;

function TUseNativeDownLoader.HTTPDownload(Const URL : String; filename:string):boolean;
var
  tries:byte;
  response: Integer;
begin
  result:=false;
  tries:=0;
  with aFPHTTPClient do
  begin
    repeat
      //RequestHeaders.Add('Connection: Close');
      // User-Agent needed for sourceforge and GitHub
      AddHeader('User-Agent',USERAGENT);
      try
        Get(URL,filename);
        response:=ResponseStatusCode;
        result:=(response=200);
        //result:=(response>=100) and (response<300);
        if (NOT result) then
        begin
          Inc(tries);
          if FVerbose then
            infoln('TFPHTTPClient retry #' +InttoStr(tries)+ ' of download from '+URL+' into '+filename+'.',etDebug);
        end;
      except
        tries:=(MaxRetries+1);
      end;
      if result then
      begin
        //AddHeader('Connection','Close');
        //HTTPMethod('HEAD', URL, Nil, [200]);
      end;
    until (result or (tries>MaxRetries));
  end;
end;

function TUseNativeDownLoader.getFile(const URL,filename:string):boolean;
begin
  try
    result:=Download(URL,filename);
  except
    SysUtils.DeleteFile(filename);
  end;
end;

function TUseNativeDownLoader.checkURL(const URL:string):boolean;
var
  tries:byte;
  response: Integer;
begin
  result:=false;
  tries:=0;
  with aFPHTTPClient do
  begin
    // User-Agent needed for sourceforge and GitHub
    AddHeader('User-Agent',USERAGENT);
    AddHeader('Connection','Close');
    repeat
      try
        HTTPMethod('HEAD', URL, Nil, []);
        response:=ResponseStatusCode;
        // 404 Not Found
        // The requested resource could not be found but may be available in the future. Subsequent requests by the client are permissible.
        result:=(response<>404);
        if (NOT result) then
        begin
          Inc(tries);
          if FVerbose then
            infoln('TFPHTTPClient retry #' +InttoStr(tries)+ ' check of ' + URL + '.',etInfo);
        end;
      except
        tries:=(MaxRetries+1);
      end;
    until (result or (tries>MaxRetries));
    if result then
    begin
      //AddHeader('Connection','Close');
      //HTTPMethod('HEAD', URL, Nil, [200]);
    end;
  end;
end;

destructor TUseNativeDownLoader.Destroy;
begin
  FreeAndNil(aFPHTTPClient);
  inherited;
end;

function TUseNativeDownLoader.Download(const URL: String; filename:string):boolean;
Var
  URI : TURI;
  P : String;
begin
  result:=false;
  URI:=ParseURI(URL);
  infoln('Native downloader: Getting ' + URI.Document + ' from '+URI.Host+URI.Path,etInfo);
  P:=URI.Protocol;
  If CompareText(P,'ftp')=0 then
    result:=FTPDownload(URL,filename)
  else if CompareText(P,'http')=0 then
    result:=HTTPDownload(URL,filename)
  else if CompareText(P,'https')=0 then
    result:=HTTPDownload(URL,filename);
end;


{$IFDEF ENABLEWGET}

// proxy still to do !!

function TUseWGetDownloader.WGetDownload(Const URL : String; Dest : TStream):boolean;
var
  Buffer : Array[0..4096] of byte;
  Count : Integer;
begin
  result:=false;
  With TProcess.Create(Self) do
  try
    CommandLine:='wget -q --user-agent="'+USERAGENT+'" --tries='+InttoStr(MaxRetries)+' --output-document=- '+URL;
    Options:=[poUsePipes,poNoConsole];
    Execute;
    while Running do
    begin
      Count:=Output.Read(Buffer,SizeOf(Buffer));
      if (Count>0) then Dest.WriteBuffer(Buffer,Count);
    end;
    result:=((ExitStatus=0) AND (Dest.Size>0));
  finally
    Free;
  end;
end;

function DoWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;
begin
  if Data=nil then result:=0 else
  begin
    result:=TStream(Data).Write(Ptr^,Size*nmemb);
  end;
end;

function TUseWGetDownloader.LibCurlDownload(Const URL : String; Dest : TStream):boolean;
var
  hCurl : pCurl;
  res: CURLcode;
  UserPass:string;
begin
  result:=false;

  if LoadCurlLibrary then
  begin

    curl_global_init(CURL_GLOBAL_ALL);

    try
      hCurl:= curl_easy_init();
      if Assigned(hCurl) then
      begin

        res:=CURLE_OK;

        UserPass:='';
        if FUsername <> '' then
        begin
          UserPass:=FUsername+':'+FPassword;
        end
        else
        begin
          if Pos('ftp.freepascal.org',URL)>0 then UserPass:='anonymous:fpc@example.com';
        end;
        if Length(UserPass)>0 then if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_USERPWD, pointer(UserPass));

        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_TCP_KEEPALIVE,1);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_FOLLOWLOCATION, 1);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_MAXREDIRS,50);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_NOPROGRESS,1);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2TLS);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_URL,PChar(URL));
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEFUNCTION,@DoWrite);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEDATA,Pointer(Dest));
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_USERAGENT,PChar(CURLUSERAGENT));

        if res=CURLE_OK then res := curl_easy_perform(hCurl);

        result:=((res=CURLE_OK) AND (Dest.Size>0));

        curl_easy_cleanup(hCurl);
      end;
    except
      // swallow libcurl exceptions
    end;
  end;
end;


function TUseWGetDownloader.FTPDownload(Const URL : String; Dest : TStream):boolean;
begin
  result:=LibCurlDownload(URL,Dest);
  if (result) then infoln('LibCurl FTP file download success !!!', etInfo);
  if (NOT result) then
  begin
    result:=WGetDownload(URL,Dest);
    if (result) then infoln('Wget FTP file download success !', etInfo);
  end;
end;

function TUseWGetDownloader.HTTPDownload(Const URL : String; Dest : TStream):boolean;
begin
  result:=LibCurlDownload(URL,Dest);
  if (result) then infoln('LibCurl HTTP file download success !!!', etInfo);
  if (NOT result) then
  begin
    result:=WGetDownload(URL,Dest);
    if (result) then infoln('Wget HTTP file download success !', etInfo);
  end;
end;

function TUseWGetDownloader.WGetFTPFileList(const URL:string; filelist:TStringList):boolean;
const
  WGETFTPLISTFILE='.listing';
var
  aURL:string;
  aTFTPList:TFTPList;
  s:string;
  i:integer;
  URI : TURI;
  P : String;
begin
  result:=false;
  URI:=ParseURI(URL);
  P:=URI.Protocol;
  if CompareText(P,'ftp')=0 then
  begin
    aURL:=URL;
    if aURL[Length(aURL)]<>'/' then aURL:=aURL+'/';
    result:=(ExecuteCommand('wget -q --no-remove-listing --tries='+InttoStr(MaxRetries)+' --spider '+aURL,false)=0);
    if result then
    begin
      if FileExists(WGETFTPLISTFILE) then
      begin
        aTFTPList:=TFTPList.Create;
        try
          aTFTPList:=TFTPList.Create;
          aTFTPList.Lines.LoadFromFile(WGETFTPLISTFILE);
          aTFTPList.ParseLines;
          for i := 0 to aTFTPList.Count -1 do
          begin
            s := aTFTPList[i].FileName;
            filelist.Add(s);
          end;
          SysUtils.DeleteFile(WGETFTPLISTFILE);
        finally
          aTFTPList.Free;
        end;
      end;
    end;
  end;
end;

function TUseWGetDownloader.LibCurlFTPFileList(const URL:string; filelist:TStringList):boolean;
var
  hCurl : pCurl;
  res: CURLcode;
  URI : TURI;
  s : String;
  aTFTPList:TFTPList;
  F:TMemoryStream;
  i:integer;
  UserPass :string;
begin
  result:=false;

  URI:=ParseURI(URL);
  s:=URI.Protocol;
  if CompareText(s,'ftp')=0 then
  begin
    if LoadCurlLibrary then
    begin

      //curl_global_init(CURL_GLOBAL_ALL);

      try
        hCurl:= curl_easy_init();
        if Assigned(hCurl) then
        begin

          res:=CURLE_OK;

          F:=TMemoryStream.Create;
          try

            UserPass:='';
            if FUsername <> '' then
            begin
              UserPass:=FUsername+':'+FPassword;
            end
            else
            begin
              if Pos('ftp.freepascal.org',URL)>0 then UserPass:='anonymous:fpc@example.com';
            end;
            if Length(UserPass)>0 then if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_USERPWD, pointer(UserPass));

            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_URL,pointer(URL));
            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEFUNCTION,@DoWrite);
            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEDATA,Pointer(F));
            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_USERAGENT,CURLUSERAGENT);
            if res=CURLE_OK then res:=curl_easy_perform(hCurl);

            result:=(res=CURLE_OK);

            curl_easy_cleanup(hCurl);

            // libcurl correct exit ?
            if result then
            begin
              // do we have data ?
              if (F.Size>0) then
              begin
                F.Position:=0;
                aTFTPList:=TFTPList.Create;
                try
                  aTFTPList:=TFTPList.Create;
                  aTFTPList.Lines.LoadFromStream(F);

                  if aTFTPList.Lines.Count>0 then
                  begin
                    // do we have a HTML listing (due to a proxy) ?
                    if Pos('<!DOCTYPE HTML',UpperCase(aTFTPList.Lines.Strings[0]))=1 then
                    begin
                      parseFTPHTMLListing(F,filelist);
                    end
                    else
                    begin
                      // parse the pure FTP response
                      aTFTPList.ParseLines;
                      for i := 0 to aTFTPList.Count -1 do
                      begin
                        s := aTFTPList[i].FileName;
                        filelist.Add(s);
                      end;
                    end;
                  end;
                finally
                  aTFTPList.Free;
                end;
              end;
            end;

          finally
            F.Free;
          end;

        end;
      except
        // swallow libcurl exceptions
      end;
    end;
  end;
end;

function TUseWGetDownloader.getFTPFileList(const URL:string; filelist:TStringList):boolean;
begin
  result:=LibCurlFTPFileList(URL,filelist);
  if (result) then infoln('LibCurl FTP filelist success !!!!', etInfo);
  if (NOT result) then
  begin
    result:=WGetFTPFileList(URL,filelist);
    if (result) then infoln('Wget FTP filelist success !!!!', etInfo);
  end;
end;

function TUseWGetDownloader.checkURL(const URL:string):boolean;
var
  Output:string;
begin
  Output:='';
  result:=(ExecuteCommand('wget --user-agent="'+USERAGENT+'" --tries='+InttoStr(MaxRetries)+' --spider '+URL,Output,false)=0);
  if result then
  begin
    result:=(Pos('Remote file exists',Output)>0);
  end;
  if NOT result then
  begin
    // on github, we get a 403 forbidden for an existing file !!
    result:=(Pos('github',Output)>0) AND (Pos('403 Forbidden',Output)>0);
  end;
end;

function TUseWGetDownloader.Download(const URL: String; Dest: TStream):boolean;
Var
  URI : TURI;
  P : String;
begin
  result:=false;
  URI:=ParseURI(URL);
  P:=URI.Protocol;
  If CompareText(P,'ftp')=0 then
    result:=FTPDownload(URL,Dest)
  else if CompareText(P,'http')=0 then
    result:=HTTPDownload(URL,Dest)
  else if CompareText(P,'https')=0 then
    result:=HTTPDownload(URL,Dest);
end;

function TUseWGetDownloader.getFile(const URL,filename:string):boolean;
var
  F : TFileStream;
begin
  result:=false;
  try
    F:=TFileStream.Create(filename,fmCreate);
    try
      result:=Download(URL,F);
    finally
      F.Free;
    end;
  except
    result:=False;
    SysUtils.DeleteFile(filename);
  end;
end;


{$ENDIF ENABLEWGET}

end.

