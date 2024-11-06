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

{$mode objfpc}{$H+}

{$i fpcupdefines.inc}

{$if not defined(ENABLEWGET) and not defined(ENABLENATIVE)}
{$error No downloader defined !!! }
{$endif}

interface

uses
  Classes, SysUtils, strutils,
  typinfo,
  zipper,
  fphttpclient, // for github api file list and others
  {$ifdef darwin}
  ns_url_request,
  {$endif}
  {$ifndef USEONLYCURL}
  {$IF NOT DEFINED(MORPHOS) AND NOT DEFINED(AROS)}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
  //fpopenssl,
  opensslsockets,
  //gnutls,
  //gnutlssockets,
  {$ENDIF}
  openssl,
  {$ENDIF}
  {$endif USEONLYCURL}
  //fpftpclient,
  FileUtil,
  LazFileUtils,
  eventlog;

Const
  DELUXEKEY='fpcupdeluxeishereforyou';

  MAXCONNECTIONRETRIES=2;
  {$ifdef Windows}
  GetLibExt='.dll';
  {$else}
  GetLibExt='.so';
  {$endif}

type
  {TNormalUnzipper}

  TNormalUnzipper = class(TObject)
  private
    // To get a filetree on Windows: CMD /c "Tree /F /A > Resultant.txt"
    FUnZipper: TUnZipper;
    FFileCnt: cardinal;
    FFileList:TStrings;
    FTotalFileCnt: cardinal;
    FCurrentFile: string;
    FFlat:boolean;
    procedure DoOnFile(Sender : TObject; Const AFileName : string);
    procedure DoOnProgressEx(Sender : TObject; Const ATotPos, ATotSize: Int64);
  public
    function DoUnZip(const ASrcFile, ADstDir: String; Files: array of string):boolean;
    function DoBUnZip2(const SourceFile, TargetFile: string):boolean;
    property Flat:boolean read FFlat write FFlat default False;
  end;

  { TLogger }
  TLogger = class(TObject)
  private
    FLog: TEventLog;
    function GetLogFile: string;
    procedure SetLogFile(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteLog(Message: string);overload;
    procedure WriteLog(EventType: TEventType;Message: string);overload;
    property LogFile: string read GetLogFile write SetLogFile;
  end;

  TBasicDownLoader = Class(TObject)
  private
    FHTTPProxyPort: integer;
    FMaxRetries:byte;
    FVerbose:boolean;
    FUserAgent: string;
    FContentType: string;
    FAccept: string;
    FUsername: string;
    FPassword: string;
    FHTTPProxyHost: string;
    FHTTPProxyUser: string;
    FHTTPProxyPassword: string;
    FFilenameOnly:string;
    procedure parseFTPHTMLListing(F:TStream;filelist:TStringList);
    procedure DoOnWriteStream(Sender: TObject; APos: Int64);
    procedure SetUserAgent(AValue:string);virtual;abstract;
    procedure SetContentType(AValue:string);virtual;abstract;
    procedure SetAccept(AValue:string);virtual;abstract;
  protected
    procedure SetVerbose(aValue:boolean);virtual;
    property  MaxRetries : Byte Read FMaxRetries Write FMaxRetries;
    property  UserAgent: string write SetUserAgent;
    property  ContentType: string write SetContentType;
    property  Accept: string write SetAccept;
    property  Username: string read FUsername;
    property  Password: string read FPassword;
    property  HTTPProxyHost: string read FHTTPProxyHost;
    property  HTTPProxyPort: integer read FHTTPProxyPort;
    property  HTTPProxyUser: string read FHTTPProxyUser;
    property  HTTPProxyPassword: string read FHTTPProxyPassword;
    property  FileNameOnly: string read FFilenameOnly;
    property  Verbose: boolean write SetVerbose;
  public
    constructor Create;virtual;
    destructor Destroy;override;
    procedure setCredentials(user,pass:string);virtual;
    procedure setProxy(host:string;port:integer;user,pass:string);virtual;
    function getFile(const URL,aFilename:string):boolean;virtual;abstract;
    function getStream(const URL:string; aDataStream:TStream):boolean;virtual;abstract;
    function getFTPFileList(const URL:string; filelist:TStringList):boolean;virtual;abstract;
    function checkURL(const URL:string):boolean;virtual;abstract;
  end;

  {$ifdef ENABLENATIVE}

  TUseNativeDownLoader = Class(TBasicDownLoader)
  strict private
    {$ifdef Darwin}
    aFPHTTPClient:TNSHTTPSendAndReceive;
    {$else}
    aFPHTTPClient:TFPHTTPClient;
    {$endif}
    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoHeaders(Sender : TObject);
    procedure DoPassword(Sender: TObject; var {%H-}RepeatRequest: Boolean);
    procedure ShowRedirect({%H-}ASender : TObject; Const ASrc : String; Var ADest : String);
    function Download(const URL: String; aDataStream:TStream):boolean;
    function FTPDownload(Const URL: String; aDataStream:TStream):boolean;
    function HTTPDownload(Const URL : String; aDataStream:TStream):boolean;
  protected
    procedure SetUserAgent(AValue:string);override;
    procedure SetContentType(AValue:string);override;
    procedure SetAccept(AValue:string);override;
    procedure SetVerbose(aValue:boolean);override;
  public
    constructor Create;override;
    destructor Destroy; override;
    procedure setProxy(host:string;port:integer;user,pass:string);override;
    function getFile(const URL,aFilename:string):boolean;override;
    function getStream(const URL:string; aDataStream:TStream):boolean;override;
    function getFTPFileList(const URL:string; filelist:TStringList):boolean;override;
    function checkURL(const URL:string):boolean;override;
  end;
  {$endif}

  {$ifdef ENABLEWGET}
  TUseWGetDownloader = Class(TBasicDownLoader)
  strict private
    FCURLOk:boolean;
    FWGETOk:boolean;
    //WGETBinary:string;
    procedure AddHeader(const aHeader,aValue:String);
    {$ifndef USEONLYCURL}
    function  WGetDownload(Const URL : String; aDataStream : TStream):boolean;
    function  WGetFTPFileList(const URL:string; filelist:TStringList):boolean;
    {$endif USEONLYCURL}
    {$ifdef ENABLECURL}
    function  LibCurlDownload(Const URL : String; aDataStream : TStream):boolean;
    function  LibCurlFTPFileList(const URL:string; filelist:TStringList):boolean;
    {$endif}
    function  Download(const URL: String; aDataStream: TStream):boolean;
    function  FTPDownload(Const URL : String; aDataStream : TStream):boolean;
    function  HTTPDownload(Const URL : String; aDataStream : TStream):boolean;
  protected
    procedure SetContentType(AValue:string);override;
    procedure SetUserAgent(AValue:string);override;
    procedure SetAccept(AValue:string);override;
  public
    class var
        WGETBinary:string;
    constructor Create;override;
    constructor Create(aWGETBinary:string);
    function getFile(const URL,aFilename:string):boolean;override;
    function getStream(const URL:string; aDataStream:TStream):boolean;override;
    function getFTPFileList(const URL:string; filelist:TStringList):boolean;override;
    function checkURL(const URL:string):boolean;override;
  end;
  {$endif}

(*
*)
  {$ifdef USEONLYCURL}
    TNativeDownloader = TUseWGetDownloader;
    TWGetDownloader = TUseWGetDownloader;
  {$else}
    {$ifdef ENABLENATIVE}
    TNativeDownloader = TUseNativeDownLoader;
    {$else}
    TNativeDownloader = TUseWGetDownloader;
    {$endif}
    {$ifdef ENABLEWGET}
    TWGetDownloader = TUseWGetDownloader;
    {$else}
    TWGetDownloader = TUseNativeDownLoader;
    {$endif}
  {$endif USEONLYCURL}


function MulDiv(const a, b, c : Integer ) : Integer;
// Create shortcut on desktop to Target file
procedure CreateDesktopShortCut(const Target, TargetArguments, ShortcutName: string; const AddContext:boolean=false);
// Create shell script in user directory that links to Target
procedure CreateHomeStartLink(const {%H-}Target, {%H-}TargetArguments, {%H-}ShortcutName: string);
{$IFDEF MSWINDOWS}
// Delete shortcut on desktop
procedure DeleteDesktopShortcut(const ShortcutName: string);
{$ENDIF MSWINDOWS}
function FindFileInDirList(const Filename, DirectoryList: String): String;
function FindFileInDir(const Filename, Path: String): String;
function FindFileInDirWildCard(const Filename, Path: String): String;
// Copy a file the safe way
function FileCopy(const Src, Dest: string;const Flags: TCopyFileFlags=[cffOverwriteFile]):boolean;
// Copy a directory recursive
function DirCopy(const SourcePath, DestPath: String): Boolean;
function CheckDirectory(const DirectoryName: string; const CheckRoot: boolean = false):boolean;
// Delete directory and children, even read-only. Equivalent to rm -rf <directory>:
function DeleteDirectoryEx(DirectoryName: string): boolean;
// Recursively delete files with specified name(s), only if path contains specfied directory name somewhere (or no directory name specified):
function DeleteFilesSubDirs(const DirectoryName: string; const Names:TStringList; const OnlyIfPathHas: string): boolean;
// Recursively delete files with specified extension(s),
// only if path contains specfied directory name somewhere (or no directory name specified):
function DeleteFilesExtensionsSubdirs(const DirectoryName: string; const Extensions:TStringList; const OnlyIfPathHas: string): boolean;
// only if filename contains specfied part somewhere
function DeleteFilesNameSubdirs(const DirectoryName: string; const OnlyIfNameHas: string): boolean;
function FileNameFromURL(URL:string):string;
function StripUrl(URL:string): string;
function CompilerVersion(CompilerPath: string): string;
function CompilerRevision(CompilerPath: string): string;
function CompilerABI(CompilerPath: string): string;
function CompilerFPU(CompilerPath: string): string;
function CompilerCPU(CompilerPath: string): string;
function CompilerOS(CompilerPath: string): string;
procedure VersionFromString(const VersionSnippet:string;out Major,Minor,Build:integer; var Patch: Integer);
function CalculateFullVersion(const Major,Minor,Release:integer):dword;overload;
function CalculateFullVersion(const Major,Minor,Release,Patch:integer):qword;overload;
function CalculateNumericalVersion(VersionSnippet: string): dword;
function VersionFromUrl(URL:string): string;
function ReleaseCandidateFromUrl(aURL:string): integer;
// Download from HTTP (includes Sourceforge redirection support) or FTP
// HTTP download can work with http proxy
function Download(UseWget:boolean; URL, TargetFile: string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;overload;
function Download(UseWget:boolean; URL: string; aDataStream:TStream; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;overload;
function GetURLDataFromCache(aURL:string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''):string;
function GetGitHubFileList(aURL:string;fileurllist:TStringList; bWGet:boolean=false; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''):boolean;
{$IFDEF MSWINDOWS}
function CheckFileSignature(aFilePath: string): boolean;
// Get Windows major and minor version number (e.g. 5.0=Windows 2000)
function GetWin32Version(out Major,Minor,Build : Integer): Boolean;
function CheckWin32Version(aMajor,aMinor: Integer): Boolean;
function IsWindows64: boolean;
// Get path for Windows per user storage of application data. Useful for storing settings
function GetWindowsDownloadFolder: string;
function GetWindowsAppDataFolder: string;
{$ENDIF MSWINDOWS}
//check if there is at least one directory between Dir and root
function ParentDirectoryIsNotRoot(Dir:string):boolean;
// Moves file if it exists, overwriting destination file
function MoveFile(const SrcFilename, DestFilename: string): boolean;
// Correct line-endings
function FileCorrectLineEndings(const SrcFilename, DestFilename: string): boolean;
// Correct directory separators
function FixPath(const s:string):string;
function FileIsReadOnly(const s:string):boolean;
function MaybeQuoted(const s:string):string;
function MaybeQuotedSpacesOnly(const s:string):string;
function UnQuote(const s:string):string;
function OccurrencesOfChar(const ContentString: string; const CharToCount: char): integer;
// Like ExpandFilename but does not expand an empty string to current directory
function SafeExpandFileName (Const FileName : String): String;
// Get application name
function SafeGetApplicationName: String;
// Get application path
function SafeGetApplicationPath: String;
// Get config path
function SafeGetApplicationConfigPath(Global:boolean=false): String;
function SaveFileFromResource(filename,resourcename:string):boolean;
// Copies specified resource (e.g. fpcup.ini, settings.ini)
// to application directory
function SaveInisFromResource(filename,resourcename:string):boolean;
// Searches for SearchFor in the stringlist and returns the index if found; -1 if not
// Search optionally starts from position SearchFor
function StringsStartsWith(const SearchIn:array of string; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
function StringsSame(const SearchIn:array of string; SearchFor:string;  StartIndex:integer=0; CS:boolean=false): integer;
function StringListStartsWith(SearchIn:TStringList; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
function StringListEndsWith(SearchIn:TStringList; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
function StringListContains(SearchIn:TStringList; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
function StringListSame(SearchIn:TStringList; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
function XdgConfigHome: String;
{$IFDEF UNIX}
function GetStartupObjects:string;
{$IFDEF LINUX}
function GetFreePhysicalMemory: DWord;
function GetFreeSwapFileSize: DWord;
function IsLinuxMUSL:boolean;
{$ENDIF LINUX}
{$ENDIF UNIX}
function GetLogicalCpuCount: integer;
function GetTotalPhysicalMemory: DWord;
function GetSwapFileSize: DWord;
{$ifdef Darwin}
function GetDarwinSDKVersion(aSDK: string):string;
function GetDarwinSDKLocation:string;
function GetDarwinToolsLocation:string;
function GetXCodeLocation:string;
{$endif}
function GetAndroidSDKDir:string;
function GetAndroidNDKDir:string;
function CompareVersionStrings(s1,s2: string): longint;
function ExistWordInString(const aString:pchar; const aSearchString:string; const aSearchOptions: TStringSearchOptions = []): Boolean;
function UnCamel(const value:string):string;
function GetEnumNameSimple(aTypeInfo:PTypeInfo;const aEnum:integer):string;
function GetEnumValueSimple(aTypeInfo:PTypeInfo;const aEnum:string):integer;
function ContainsDigit(const s: string): Boolean;
//Find a library, if any
function LibWhich(const {%H-}aLibrary: string; out {%H-}location: string): boolean;
function LibWhich(const aLibrary: string): boolean;
// Emulates/runs which to find executable in path. If not found, returns empty string
function Which(const Executable: string): string;
function IsExecutable(Executable: string):boolean;
function ForceDirectoriesSafe(Const Dir: RawByteString): Boolean;
function CheckExecutable(Executable:string;Parameters:array of string;ExpectOutput: string; beSilent:boolean=false): boolean;
function GetJava: string;
function GetJavac: string;
function CheckJava: boolean;
function ExtractFilePathSafe(const AFilename: string): string;
function ExtractFileNameSafe(const AFilename: string): string;
function FileNameWithoutExt(const AFilename: string): string;
function FileNameWithoutAllExt(const AFilename: string): string;
function FileNameAllExt(const AFilename: string): string;
function DoubleQuoteIfNeeded(s: string): string;
function UppercaseFirstChar(s: String): String;
function DirectoryIsEmpty(Directory: string): Boolean;
function GetSourceCPU:string;
function GetSourceOS:string;
function GetSourceCPUOS:string;
function GetFPCBuildVersion:string;
function GetDistro(const aID:string=''):string;
function GetFreeBSDVersion:byte;
function checkGithubRelease(const aURL:string):string;
{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}
Function Pos(Const Substr : string; Const Source : string; Offset : Sizeint = 1) : SizeInt;
{$ENDIF}
{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30000)}
Function CharInSet(Ch:AnsiChar;Const CSet : TSysCharSet) : Boolean; inline;
{$ENDIF}
function SendMail (Host, Subject, pTo, From, login,password: string; Body: TStrings):boolean;

implementation

uses
  {$ifdef LCL}
  Forms,//Controls,
  {$endif}
  {$ifdef Darwin}
  MacTypes,
  Folders,
  Files,
  {$endif}
  IniFiles,
  DOM,DOM_HTML,SAX_HTML,
  {$ifdef ENABLENATIVE}
  ftpsend,
  {$else}
  {$IF NOT DEFINED(MORPHOS) AND NOT DEFINED(AROS)}
  ftplist,
  {$ENDIF}
  {$endif}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30000)}
  fpwebclient,
  fphttpwebclient,
  {$ENDIF}
  fpjson, jsonparser ,uriparser
  {$IFDEF MSWINDOWS}
    //Mostly for shortcut code
    ,windows, shlobj {for special folders}, ActiveX, ComObj, WinDirs, WinINet
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,linux
  {$ENDIF}
  {$IFDEF UNIX}
  ,unix,baseunix
  {$ENDIF}
  {$IFDEF ENABLEWGET}
  {$IF NOT DEFINED(MORPHOS) AND NOT DEFINED(AROS) AND NOT DEFINED(AMIGA)}
  ,fpcuplibcurl
  //,libcurl
  {$ENDIF}
  {$ENDIF ENABLEWGET}
  //,SynCrtSock // SendEmail from the mORMot
  //,LCLIntf // OpenURL
  {$ifndef Haiku}
  {$ifdef ENABLEEMAIL}
  ,mimemess,mimepart,smtpsend
  {$endif}
  {$endif}
  ,process
  ,processutils
  ,bzip2stream
  ,DCPdes
  ,DCPsha256
  ,NumCPULib
  {$IFDEF USEMORMOT}
  ,mormot.net.client
  ,mormot.core.buffers
  {$ENDIF USEMORMOT}
  ;

const
  NORMALUSERAGENT = 'curl/7.50.1 (i686-pc-linux-gnu) libcurl/7.50.1 OpenSSL/1.0.1t zlib/1.2.8 libidn/1.29 libssh2/1.4.3 librtmp/2.3';
  //NORMALUSERAGENT = 'Mozilla/5.0 (compatible; fpweb)';
  FPCUPUSERAGENT = 'fpcupdeluxe';
  {$IFDEF ENABLEWGET}
  CURLUSERAGENT='curl/7.50.1';
  //CURLUSERAGENT='curl/7.51.0';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WININETUSERAGENT = 'Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0; WinInet)';
  {$ENDIF MSWINDOWS}

{$i revision.inc}
{$ifdef ENABLEEMAIL}
{$i secrets.inc}
{$endif}

type
  {$ifdef ENABLENATIVE}
  TMyFTPSend = class(TFTPSend);
  {$endif ENABLENATIVE}

  TOnWriteStream = procedure(Sender: TObject; APos: Int64) of object;

  TDownloadStream = class(TFileStream)
  private
    FStoredTickCount:QWord;
    FOnWriteStream: TOnWriteStream;
    procedure SetOnWriteStream(aValue:TOnWriteStream);
  public
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    class function StreamCreate(const aFileName: string; aMode: cardinal):TStream;
  published
    property OnWriteStream: TOnWriteStream read FOnWriteStream write SetOnWriteStream;
  end;

  //PTGitHubStore = ^TGitHubStore;
  TGitHubStore = record
    URL:string;
    FileList:TStringList;
  end;

  TURLDataCache = record
    URL:string;
    Data:string;
  end;

var
  GitHubFileListCache:array of TGitHubStore;
  URLDataCache:array of TURLDataCache;


function GetStringFromBuffer(const field:PChar):string;
begin
  if ( field <> nil ) then
  begin
    //strpas(field);
    result:=field;
    UniqueString(result);
    SetLength(result,strlen(field));
  end else result:='';
end;

function CleanURL(URL:string):string;
const
  URLMAGIC='/download';
begin
  result:=URL;
  if AnsiEndsText(URLMAGIC,URL) then SetLength(result,Length(URL)-Length(URLMAGIC));
end;

(*

function ResNameProc({%H-}ModuleHandle : TFPResourceHMODULE; {%H-}ResourceType, ResourceName : PChar; {%H-}lParam : PtrInt) : LongBool; stdcall;
var
  aName:string;
begin
  if Assigned(resourcefiles) then
  begin
    if Is_IntResource(ResourceName)
       then aName:=InttoStr({%H-}PtrUInt(ResourceName))
       else aName:=GetStringFromBuffer(ResourceName);
    resourcefiles.Append(aName);
  end;
  Result:=true;
end;

function ResTypeProc(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; lParam : PtrInt) : LongBool; stdcall;
var
  aType:string;
  RT:integer;
begin
  if Is_IntResource(ResourceType) then RT:={%H-}PtrUInt(ResourceType) else
  begin
    aType:=GetStringFromBuffer(ResourceType);
    RT:=StrToIntDef(aType,0);
  end;
  // get only the plain files (resource type 10; RT_RCDATA)
  if RT=10 then EnumResourceNames(ModuleHandle,ResourceType,@ResNameProc,lParam);
  Result:=true;
end;

procedure DoEnumResources;
begin
  EnumResourceTypes(HINSTANCE,@ResTypeProc,0);
end;
*)

procedure FTPHTMLListingParser(F:TStream;filelist:TStringList);
var
  ADoc: THTMLDocument;
  HTMFiles : TDOMNodeList;
  FilenameValid:boolean;
  i,j:integer;
  s:string;
  FPCSVNList,LAZARUSSVNList:boolean;
begin
  FPCSVNList:=False;
  LAZARUSSVNList:=False;
  F.Position:=0;
  try
    ReadHTMLFile(ADoc,F);
    // a bit rough, but it works
    HTMFiles:=ADoc.GetElementsByTagName('title');
    if HTMFiles.Count>0 then
    begin
      s:=HTMFiles[0].TextContent;
      FPCSVNList:=(Pos('[fpc]',s)=1);
      LAZARUSSVNList:=(Pos('[lazarus]',s)=1);
    end;
    HTMFiles:=ADoc.GetElementsByTagName('a');
    for i:=0 to Pred(HTMFiles.Count) do
    begin
      s:=TDOMElement(HTMFiles[i]).GetAttribute('name');
      if Length(s)>0 then
      begin
        // validate filename (also rough)
        FilenameValid:=True;
        for j:=1 to Length(s) do
        begin
          FilenameValid := (NOT CharInSet(s[j], [';', '=', '+', '<', '>', '|','"', '[', ']', '\', '/', '''']));
          if (NOT FilenameValid) then break;
        end;
        if FilenameValid then
        begin
          FilenameValid:=False;
          FilenameValid:=FilenameValid OR ((LowerCase(ExtractFileExt(s))='.zip') OR (LowerCase(ExtractFileExt(s))='.bz2'));
          FilenameValid:=FilenameValid OR ((Pos('lazarus_',s)=1) AND (LAZARUSSVNList));
          FilenameValid:=FilenameValid OR ((Pos('release_',s)=1) AND (FPCSVNList));
          // finally, add filename if all is ok !!
          if FilenameValid then filelist.Add(s);
        end;
      end;
    end;
  finally
    aDoc.Free;
  end;
end;


{$ifdef mswindows}
function GetWin32Version(out Major,Minor,Build : Integer): Boolean;
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
  else result:=false;
end;

function CheckWin32Version(aMajor,aMinor: Integer): Boolean;
var
  Major,Minor,Build : Integer;
begin
  if GetWin32Version(Major,Minor,Build) then
  begin
    result:=(Major>aMajor) or
            ((Major=aMajor) and (Minor>=aMinor));
  end else result:=false;
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

function MulDiv(const a, b, c : Integer ) : Integer;
begin
  result := int64(a)*int64(b) div c;
end;

function SafeExpandFileName (Const FileName : String): String;
begin
  if FileName='' then
    result:=''
  else
    result:=ExpandFileName(FileName);
end;

function SafeGetApplicationName: String;
var
  StartPath: String;
  {$ifdef Darwin}
  x:integer;
  {$endif}
begin
 {$ifdef LCL}
 StartPath:=Application.ExeName;
 {$else}
 StartPath:=Paramstr(0);
 {$endif}
 {$ifdef Darwin}
 // we need the .app itself !!
 x:=pos('/Contents/MacOS',StartPath);
 if x>0 then
 begin
   Delete(StartPath,x,MaxInt);
   (*
   x:=RPos('/',StartPath);
   if x>0 then
   begin
     Delete(StartPath,x+1,MaxInt);
   end;
   *)
 end;
 {$endif}
 if FileIsSymlink(StartPath) then
 begin
   try
     StartPath:=GetPhysicalFilename(StartPath,pfeException);
   except
   end;
 end;
 result:=StartPath;
end;

function SafeGetApplicationPath: String;
var
  StartPath: String;
begin
  {$ifdef Darwin}
  StartPath:=ExtractFileDir(SafeGetApplicationName);
  {$else}
  StartPath:=GetCurrentDir;
  {$endif}
  if DirectoryExists(StartPath) then
  begin
    try
      StartPath:=GetPhysicalFilename(StartPath,pfeException);
    except
    end;
  end;
  result:=IncludeTrailingPathDelimiter(StartPath);
end;

function SafeGetApplicationConfigPath(Global:boolean=false): String;
{$IFDEF DARWIN}
const
  kMaxPath = 1024;
var
  theError: OSErr;
  theRef: FSRef;
  pathBuffer: PChar;
{$ENDIF}
begin
  result:='';
  {$IFDEF DARWIN}
  theRef := Default(FSRef); // init
  try
    pathBuffer := Allocmem(kMaxPath);
  except on exception
    do exit;
  end;
  try
    Fillchar(pathBuffer^, kMaxPath, #0);
    Fillchar(theRef, Sizeof(theRef), #0);
    if Global then // kLocalDomain
      theError := FSFindFolder(kLocalDomain, kPreferencesFolderType, kDontCreateFolder, theRef)
    else // kUserDomain
      theError := FSFindFolder(kUserDomain , kPreferencesFolderType, kDontCreateFolder, theRef);
    if (pathBuffer <> nil) and (theError = noErr) then
    begin
      theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
      if theError = noErr then
        result := UTF8ToAnsi(StrPas(pathBuffer));
    end;
  finally
    Freemem(pathBuffer);
  end;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  if Global then
    result:=GetWindowsSpecialDir(CSIDL_COMMON_APPDATA)
  else
    result:=GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA);
  {$ELSE}
  if Global then
    result:=SysConfigDir
  else
  begin
    result:=SysUtils.GetEnvironmentVariable('HOME');
    if (result='') then
      result:=SafeExpandFileName(IncludeTrailingPathDelimiter(GetUserDir)+'.config')
    else
      result:=IncludeTrailingPathDelimiter(result) + '.config';
  end;
  {$ENDIF}
  {$ENDIF}
  result:=IncludeTrailingPathDelimiter(result);
end;


function SafeGetApplicationTempPath(Global:boolean=false): String;
{$IFDEF DARWIN}
const
  kMaxPath = 1024;
var
  theError: OSErr;
  theRef: FSRef;
  pathBuffer: PChar;
{$ENDIF}
begin
  result:='';
  {$IFDEF DARWIN}
  theRef := Default(FSRef); // init
  try
    pathBuffer := Allocmem(kMaxPath);
  except on exception
    do exit;
  end;
  try
    Fillchar(pathBuffer^, kMaxPath, #0);
    Fillchar(theRef, Sizeof(theRef), #0);
    if Global then // kLocalDomain
      theError := FSFindFolder(kLocalDomain, kTemporaryFolderType, kDontCreateFolder, theRef)
    else // kUserDomain
      theError := FSFindFolder(kUserDomain , kTemporaryFolderType, kDontCreateFolder, theRef);
    if (pathBuffer <> nil) and (theError = noErr) then
    begin
      theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
      if theError = noErr then
        result := UTF8ToAnsi(StrPas(pathBuffer));
    end;
  finally
    Freemem(pathBuffer);
  end;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  if Global then
    result:=GetWindowsSpecialDir(CSIDL_COMMON_APPDATA)
  else
    result:=GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA);
  {$ELSE}
  if Global then
    result:=SysConfigDir
  else
  begin
    result:=SysUtils.GetEnvironmentVariable('HOME');
    if (result='') then
      result:=SafeExpandFileName(IncludeTrailingPathDelimiter(GetUserDir)+'.cache')
    else
      result:=IncludeTrailingPathDelimiter(result) + '.cache';
  end;
  {$ENDIF}
  {$ENDIF}
  result:=IncludeTrailingPathDelimiter(result);
end;

function SaveFileFromResource(filename,resourcename:string):boolean;
var
  fs:Tfilestream;
begin
  result:=false;

  if FileExists(filename) then SysUtils.DeleteFile(filename);
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
  result:=FileExists(filename);
end;

function SaveInisFromResource(filename,resourcename:string):boolean;
var
  fs:Tfilestream;
  ms:TMemoryStream;
  BackupFileName:string;
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  Ini:TMemIniFile;
  {$ELSE}
  Ini:TIniFile;
  {$ENDIF}
  OldIniVersion,NewIniVersion:string;
  Major,Minor,Build,Patch: Integer;
  OldIniVersionNum,NewIniVersionNum:qword;
begin
  result:=false;

 if NOT FileExists(filename) then
 begin
   result:=SaveFileFromResource(filename,resourcename);
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

    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
    Ini:=TMemIniFile.Create(ms);
    {$ELSE}
    Ini:=TIniFile.Create(ms);
    {$ENDIF}

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

    Major:=0;
    Minor:=0;
    Build:=0;
    Patch:=0;
    VersionFromString(OldIniVersion,Major,Minor,Build,Patch);
    OldIniVersionNum:=CalculateFullVersion(Major,Minor,Build,Patch);
    Major:=0;
    Minor:=0;
    Build:=0;
    Patch:=0;
    VersionFromString(NewIniVersion,Major,Minor,Build,Patch);
    NewIniVersionNum:=CalculateFullVersion(Major,Minor,Build,Patch);

    if (NewIniVersionNum>OldIniVersionNum) then
    begin
      BackupFileName:=ChangeFileExt(filename,'.bak');
      while FileExists(BackupFileName) do BackupFileName := BackupFileName + 'k';
      FileCopy(filename,BackupFileName);
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
    end;

   finally
     ms.Free;
   end;

 end;

 result:=FileExists(filename);
end;

{$IFDEF MSWINDOWS}
procedure CreateDesktopShortCut(const Target, TargetArguments, ShortcutName: string; const AddContext:boolean);
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
{$ELSE}
{$IFDEF DARWIN}
procedure CreateDesktopShortCut(const Target, TargetArguments, ShortcutName: string; const AddContext:boolean);
begin
  // Create shortcut on Desktop and in Applications
  fpSystem(
    '/usr/bin/osascript << EOF'+#10+
    'tell application "Finder"'+#10+
      'set myLazApp to POSIX file "'+IncludeLeadingPathDelimiter(Target)+'.app" as alias'+#10+
      'try'+#10+
          'set myLazDeskShort to (path to desktop folder as string) & "'+ShortcutName+'" as alias'+#10+
          'on error'+#10+
             'make new alias to myLazApp at (path to desktop folder as text)'+#10+
             'set name of result to "'+ShortcutName+'"'+#10+
      'end try'+#10+
      'try'+#10+
          'set myLazAppShort to (path to applications folder as string) & "'+ShortcutName+'" as alias'+#10+
          'on error'+#10+
             'make new alias to myLazApp at (path to applications folder as text)'+#10+
             'set name of result to "'+ShortcutName+'"'+#10+
      'end try'+#10+

    'end tell'+#10+
    'EOF');
end;
{$ELSE}
{$IFDEF UNIX}
procedure CreateDesktopShortCut(const Target, TargetArguments, ShortcutName: string; const AddContext:boolean);
var
  OperationSucceeded: boolean;
  XdgDesktopContent: TStringList;
  XdgMimeContent: TStringList;
  Output,XdgDesktopFile,XdgMimeFile: string;
  aDirectory:string;
  aIconFile:string;
begin
  {$ifdef Haiku}
  exit;
  {$endif}

  // Fail by default:
  OperationSucceeded:=false;

  XdgDesktopFile:=IncludeTrailingPathDelimiter(GetTempDir(false))+'fpcup-'+shortcutname+'.desktop';
  XdgDesktopContent:=TStringList.Create;
  try
    XdgDesktopContent.Add('[Desktop Entry]');
    XdgDesktopContent.Add('Version=1.0');
    XdgDesktopContent.Add('Encoding=UTF-8');
    XdgDesktopContent.Add('Type=Application');
    aIconFile:='';
    {$ifdef LINUX}
    aIconFile:=ExtractFilePath(Target)+'images/icons/lazarus128x128.png';
    if (NOT FileExists(aIconFile)) then aIconFile:=ExtractFilePath(Target)+'images/icons/lazarus32x32.png';
    if (NOT FileExists(aIconFile)) then aIconFile:=ExtractFilePath(Target)+'images/icons/lazarus.png';
    {$endif}
    if (Length(aIconFile)=0) OR (NOT FileExists(aIconFile)) then aIconFile:=ExtractFilePath(Target)+'images/icons/lazarus.ico';
    XdgDesktopContent.Add('Icon='+aIconFile);
    XdgDesktopContent.Add('Path='+ExtractFilePath(Target));
    {$ifdef DISABLE_PPC_CONFIG_PATH}
    XdgDesktopContent.Add('Exec=env -u PPC_CONFIG_PATH '+Target+' '+TargetArguments+' %f');
    {$else}
    XdgDesktopContent.Add('Exec='+Target+' '+TargetArguments+' %f');
    {$endif}
    XdgDesktopContent.Add('Name='+ShortcutName);
    XdgDesktopContent.Add('GenericName=Lazarus IDE with Free Pascal Compiler');
    XdgDesktopContent.Add('Category=Application;IDE;Development;GUIDesigner;Programming;');
    XdgDesktopContent.Add('Categories=Application;IDE;Development;GUIDesigner;Programming;');
    XdgDesktopContent.Add('Keywords=editor;Pascal;IDE;FreePascal;fpc;Design;Designer;');

    if AddContext then
    begin
      //XdgDesktopContent.Add('StartupWMClass=Lazarus');
      XdgDesktopContent.Add('MimeType=application/x-lazarus;');
      //XdgDesktopContent.Add('Patterns=*.pas;*.pp;*.p;*.inc;*.lpi;*.lpk;*.lpr;*.lfm;*.lrs;*.lpl;');
    end;

    // We're going to try and call xdg-desktop-icon/menu
    // this may fail if shortcut exists already
    try
      XdgDesktopContent.SaveToFile(XdgDesktopFile);
      FpChmod(XdgDesktopFile, &711); //rwx--x--x
      OperationSucceeded:=RunCommand('xdg-desktop-icon' ,['install','--novendor',XdgDesktopFile],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
      OperationSucceeded:=RunCommand('xdg-desktop-menu' ,['install','--novendor',XdgDesktopFile],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    except
      OperationSucceeded:=false;
    end;

    if (true) then
    begin
      aDirectory:=ConcatPaths(['usr','share','applications']);
      if ( (FpGeteuid=0) AND DirectoryExists(aDirectory) ) then
      begin
        FileCopy(XdgDesktopFile,aDirectory+DirectorySeparator+ExtractFileName(XdgDesktopFile),[]);
      end
      else
      begin
        // Create shortcut directly on User-Desktop
        aDirectory:=ConcatPaths([GetUserDir,'Desktop']);
        if DirectoryExists(aDirectory) then
           FileCopy(XdgDesktopFile,aDirectory+DirectorySeparator+ExtractFileName(XdgDesktopFile),[]);
        // Create user menu item
        if (NOT OperationSucceeded) then
        begin
          aDirectory:=ConcatPaths([GetUserDir,'.local','share','applications']);
          if DirectoryExists(aDirectory) then
            FileCopy(XdgDesktopFile,aDirectory+DirectorySeparator+ExtractFileName(XdgDesktopFile),[]);
        end;
      end;
    end;
    // Temp file is no longer needed....
    try
      SysUtils.DeleteFile(XdgDesktopFile);
    finally
      // Swallow, let filesystem maintenance clear it up
    end;
  finally
    XdgDesktopContent.Free;
    OperationSucceeded:=true;
  end;

  if (OperationSucceeded) then
  begin
    aDirectory:=ConcatPaths([GetUserDir,'.local','share','applications']);
    OperationSucceeded:=RunCommand('update-desktop-database' ,[aDirectory],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
  end;

  if AddContext then
  begin
    ThreadLog('Adding context !');
    {$ifdef LCL}
    Application.ProcessMessages;
    {$endif}

    aDirectory:=ConcatPaths([GetUserDir,'.local','share','mime']);
    ForceDirectoriesSafe(aDirectory);

    //Create mime file associations
    XdgMimeFile:=IncludeTrailingPathDelimiter(GetTempDir(false))+'fpcup-'+shortcutname+'.xml';
    XdgMimeContent:=TStringList.Create;
    try
      XdgMimeContent.Add('<?xml version="1.0" encoding="UTF-8"?>');
      XdgMimeContent.Add('<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">');
      XdgMimeContent.Add('    <mime-type type="application/x-lazarus">');
      XdgMimeContent.Add('        <comment>Lazarus file</comment>');
      XdgMimeContent.Add('        <icon name="application-x-lazarus"/>');
      XdgMimeContent.Add('        <glob-deleteall/>');
      XdgMimeContent.Add('        <glob pattern="*.lpi"/>');
      XdgMimeContent.Add('        <glob pattern="*.lpr"/>');
      XdgMimeContent.Add('        <glob pattern="*.lfm"/>');
      XdgMimeContent.Add('        <glob pattern="*.pas"/>');
      XdgMimeContent.Add('        <glob pattern="*.pp"/>');
      XdgMimeContent.Add('        <glob pattern="*.inc"/>');
      XdgMimeContent.Add('    </mime-type>');
      XdgMimeContent.Add('</mime-info>');
      aDirectory:=ConcatPaths([GetUserDir,'.local','share','mime','packages']);
      ForceDirectoriesSafe(aDirectory);
      XdgMimeContent.SaveToFile(XdgMimeFile);
      OperationSucceeded:=RunCommand('xdg-mime' ,['install','--novendor',XdgMimeFile],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
      SysUtils.DeleteFile(XdgMimeFile);
    finally
      XdgMimeContent.Free;
    end;

    //Process icon
    aDirectory:=ConcatPaths([GetUserDir,'.local','share','icons']);
    ForceDirectoriesSafe(aDirectory);
    //OperationSucceeded:=RunCommand('xdg-icon-resource' ,['install','--novendor','--context','mimetypes','--size','64',ExtractFilePath(Target)+'images/icons/lazarus.ico','application-x-lazarus'],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    OperationSucceeded:=RunCommand('xdg-icon-resource' ,['install','--novendor','--context','mimetypes','--size','64',ExtractFilePath(Target)+'images/icons/lazarus64x64.png','application-x-lazarus'],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});

    //Update mime database
    aDirectory:=ConcatPaths([GetUserDir,'.local','share','mime']);
    OperationSucceeded:=RunCommand('update-mime-database' ,[aDirectory],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
  end;

end;
{$ELSE}
procedure CreateDesktopShortCut(const Target, TargetArguments, ShortcutName: string; const AddContext:boolean);
begin
  ThreadLog('Not creating desktop shortcut: don''t know how to do this.');
end;
{$ENDIF UNIX}
{$ENDIF DARWIN}
{$ENDIF MSWINDOWS}

procedure CreateHomeStartLink(const Target, TargetArguments,ShortcutName: string);
{$IFDEF UNIX}
var
  ScriptText: TStringList;
  ScriptFile: string;
{$ENDIF UNIX}
begin
  {$IFDEF UNIX}
  //create dir if it doesn't exist
  ForceDirectoriesSafe(ExtractFilePath(IncludeTrailingPathDelimiter(GetUserDir)+ShortcutName));
  ScriptText:=TStringList.Create;
  try
    // No quotes here, either, we're not in a shell, apparently...
    ScriptFile:=IncludeTrailingPathDelimiter(GetUserDir)+ShortcutName;
    SysUtils.DeleteFile(ScriptFile); //Get rid of any existing remnants
    ScriptText.Add('#!/bin/sh');
    ScriptText.Add('# '+BeginSnippet+' home startlink script');
    {$ifdef DISABLE_PPC_CONFIG_PATH}
    ScriptText.Add('unset PPC_CONFIG_PATH');
    {$endif}
    ScriptText.Add(Target+' '+TargetArguments+' "$@"');
    try
      ScriptText.SaveToFile(ScriptFile);
      FpChmod(ScriptFile, &755); //rwxr-xr-x
    except
    end;
  finally
    ScriptText.Free;
  end;
  {$ENDIF UNIX}
end;

function FileNameFromURL(URL:string):string;
var
  URI:URIPARSER.TURI;
  aURL:string;
begin
  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  result:=URI.Document;
end;

function StripUrl(URL:string): string;
var
  URI:URIPARSER.TURI;
begin
  URI:=ParseURI(URL);
  result:=URI.Host+URI.Path;
end;

function CompilerCommand(CompilerPath,Command: string): string;
var
  Output: string;
begin
  Result:='';
  if ((CompilerPath='') OR (NOT FileExists(CompilerPath))) then exit;
  try
    Output:='';
    if RunCommand(CompilerPath,[Command], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}{$ENDIF}) then
    begin
      Output:=TrimRight(Output);
      if Length(Output)>0 then Result:=Output;
    end;
  except
  end;
end;

function CompilerVersion(CompilerPath: string): string;
var
  Output: string;
begin
  Result:='0.0.0';
  Output:=CompilerCommand(CompilerPath,'-iV');
  if Length(Output)>0 then Result:=Output;
end;

function CompilerRevision(CompilerPath: string): string;
var
  Output: string;
  i:integer;
begin
  Result:='';
  Output:=CompilerCommand(CompilerPath,'-iW');
  if Length(Output)>0 then
  begin
    i:=0;
    if (i=0) then i:=Pos('-release',Output); // prevent -release from being detected as -r ... tricky
    if (i=0) then
    begin
      i:=Pos('-rrelease',Output); // prevent -rrelease from being detected as -r ... tricky
      if (i>0) then Inc(i);
    end;
    if (i=0) then
    begin
      i:=Pos('-r',Output);
      if (i>0) then Inc(i);
    end;
    if (i=0) then i:=Pos('-',Output);
    if (i>0) then
    begin
      Delete(Output,1,i);
      Result:=Trim(Output);
    end;
  end;
end;

function CompilerABI(CompilerPath: string): string;
begin
  Result:=CompilerCommand(CompilerPath,'-ia');
end;

function CompilerFPU(CompilerPath: string): string;
begin
  Result:=CompilerCommand(CompilerPath,'-if');
end;

function CompilerCPU(CompilerPath: string): string;
begin
  Result:=CompilerCommand(CompilerPath,'-iSP');
end;

function CompilerOS(CompilerPath: string): string;
begin
  Result:=CompilerCommand(CompilerPath,'-iSO');
end;

procedure VersionFromString(const VersionSnippet:string;out Major,Minor,Build:integer; var Patch: Integer);
var
  i,j:integer;
  found:boolean;
begin
  i:=1;

  // move towards first numerical
  while (Length(VersionSnippet)>=i) AND (NOT (VersionSnippet[i] in ['0'..'9'])) do Inc(i);
  // get major version
  j:=0;
  found:=false;
  while (Length(VersionSnippet)>=i) AND (VersionSnippet[i] in ['0'..'9']) do
  begin
    found:=true;
    j:=j*10+Ord(VersionSnippet[i])-$30;
    Inc(i);
  end;
  if found then Major:=j;

  // skip random symbols to move towards next digit
  //while (Length(VersionSnippet)>=i) AND (NOT (VersionSnippet[i] in ['0'..'9'])) do Inc(i);
  // skip a single random symbol [dot] to move towards next digit
  if (Length(VersionSnippet)>=i) then Inc(i);
  // get minor version
  j:=0;
  found:=false;
  while (Length(VersionSnippet)>=i) AND (VersionSnippet[i] in ['0'..'9']) do
  begin
    found:=true;
    j:=j*10+Ord(VersionSnippet[i])-$30;
    Inc(i);
  end;
  if found then Minor:=j;

  // skip random symbols to move towards next digit
  //while (Length(VersionSnippet)>=i) AND (NOT (VersionSnippet[i] in ['0'..'9'])) do Inc(i);
  // skip a single random symbol [dot] to move towards next digit
  if (Length(VersionSnippet)>=i) then Inc(i);
  // get build version
  j:=0;
  found:=false;
  while (Length(VersionSnippet)>=i) AND (VersionSnippet[i] in ['0'..'9']) do
  begin
    found:=true;
    j:=j*10+Ord(VersionSnippet[i])-$30;
    Inc(i);
  end;
  if found then Build:=j;

  // there might be an RC (release candidate)
  // skip random symbols to move towards next digit
  while (Length(VersionSnippet)>=i) AND (NOT (VersionSnippet[i] in ['0'..'9'])) do Inc(i);
  // skip a single random symbol to move towards next digit
  //if (Length(VersionSnippet)>=i) then Inc(i);
  // get patch version
  j:=0;
  found:=false;
  while (Length(VersionSnippet)>=i) AND (VersionSnippet[i] in ['0'..'9']) do
  begin
    found:=true;
    j:=j*10+Ord(VersionSnippet[i])-$30;
    Inc(i);
  end;
  if found then Patch:=j;
end;

function CalculateFullVersion(const Major,Minor,Release:integer):dword;
begin
  if (Major>=0) AND (Major<=6) AND (Minor>=0) then
  begin
    result:=((Major *  100 + Minor) * 100);
    if (Release>=0) then result:=result+Release;
  end
  else
    result:=0;
end;

function CalculateFullVersion(const Major,Minor,Release,Patch:integer):qword;
begin
  result:=0;
  if (Major>=0) then result:=(result+Major)*100;
  if (Minor>=0) then result:=(result+Minor)*100;
  if (Release>=0) then result:=(result+Release)*100;
  if (Patch>=0) then result:=result+Patch;
end;

function CalculateNumericalVersion(VersionSnippet: string): dword;
var
  Major,Minor,Build,Patch: Integer;
begin
  if (Length(VersionSnippet)=0) OR (VersionSnippet='0.0.0') then
  begin
    result:=0;
  end
  else
  begin
    Major:=0;
    Minor:=0;
    Build:=0;
    Patch:=0;
    VersionFromString(VersionSnippet,Major,Minor,Build,Patch);
    result:=CalculateFullVersion(Major,Minor,Build);
  end;
end;

function VersionFromUrl(URL:string): string;
var
  VersionSnippet:string;
  i:integer;
  VersionList : TStringList;
begin
  result:='0.0.0';

  //if (Length(URL)=0) then exit;

  if Pos('trunk',URL)>0 then result:='trunk' else
  if Pos('newpascal',URL)>0 then result:='trunk' else
  if Pos('freepascal.git',URL)>0 then result:='trunk' else
  if Pos('lazarus.git',URL)>0 then result:='trunk' else
  begin

    VersionSnippet := UpperCase(URL);

    i := Length(VersionSnippet);

    // remove trailing delimiter
    if (i>0) and CharInSet(VersionSnippet[i],['\','/']) then
    begin
      Dec(i);
      SetLength(VersionSnippet,i);
    end;

    // remove git trailer
    if (i>0) and (RightStr(VersionSnippet,4)='.GIT') then
    begin
      Dec(i,4);
      SetLength(VersionSnippet,i);
    end;

    // extract last part of URL, the part that should contain the version
    while (i > 0) and (not CharInSet(VersionSnippet[i],['\','/'])) do Dec(i);
    VersionSnippet := Copy(VersionSnippet, i + 1, MaxInt);

    // if url contains a version, this version always starts with first _#
    if Length(VersionSnippet)>0 then
    begin
      i:=1;
      repeat
        if (CharInSet(VersionSnippet[i],['0'..'9'])) then
        begin
          if (i>1) AND (VersionSnippet[i-1]='_') then break;
        end;
        Inc(i);
        if (i>Length(VersionSnippet)) then break;
      until false;

      Delete(VersionSnippet,1,(i-1));
      // ignore release candidate numbering
      i := Pos('_RC',VersionSnippet);
      if i>0 then Delete(VersionSnippet,i,MaxInt);
      VersionSnippet:=StringReplace(VersionSnippet,'_',',',[rfReplaceAll]);
    end;

    if Length(VersionSnippet)>0 then
    begin
      VersionList := TStringList.Create;
      try
        VersionList.CommaText := VersionSnippet;
        if VersionList.Count>0 then
        begin
          result:=VersionList[0];
          if VersionList.Count>1 then result:=result+'.'+VersionList[1];
          if VersionList.Count>2 then result:=result+'.'+VersionList[2];
        end;
      finally
        VersionList.Free;
      end;
    end;
  end;
end;

function ReleaseCandidateFromUrl(aURL:string): integer;
const
  RC_MAGIC='_RC';
var
  VersionSnippet:string;
  i:integer;
begin
  result:=-1;

  VersionSnippet := UpperCase(aURL);
  i := Length(VersionSnippet);

  // remove trailing delimiter
  if (i>0) and CharInSet(VersionSnippet[i],['\','/']) then
  begin
    Dec(i);
    SetLength(VersionSnippet,i);
  end;

  // find last occurence of _RC
  // if url contains a RC, this always starts with _RC
  i := RPos(RC_MAGIC,VersionSnippet);
  if i>0 then
  begin
    Delete(VersionSnippet,1,i+Length(RC_MAGIC)-1);
    result:=StrToIntDef(VersionSnippet,-1);
  end;
end;


{$IFDEF MSWINDOWS}
procedure DeleteDesktopShortcut(const ShortcutName: string);
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

function FindFileInDirList(const Filename, DirectoryList: String): String;
var
  DirList: TStringList;
  ADirectory: String;
  AFile: string;
begin
  Result := '';
  DirList:=TStringList.Create;
  try
    DirList.Delimiter:=PathSeparator;
    DirList.StrictDelimiter:=True;
    DirList.DelimitedText:=SetDirSeparators(DirectoryList);
    for ADirectory in DirList do
    begin
      AFile:=IncludeTrailingPathDelimiter(ADirectory)+Filename;
      if FileExists(AFile) then
      begin
        Result := AFile;
        Break;
      end;
    end;
  finally
    DirList.Free;
  end;
end;

function FindFileInDir(const Filename, Path: String): String;
var
  DirList: TStringList;
  ADirectory: String;
  AFile: string;
begin
  Result := '';
  DirList := FindAllDirectories(Path);
  try
    DirList.Insert(0,ExcludeTrailingPathDelimiter(Path));
    for ADirectory in DirList do
    begin
      AFile:=IncludeTrailingPathDelimiter(ADirectory)+Filename;
      if FileExists(AFile) then
      begin
        Result := AFile;
        Break;
      end;
    end;
  finally
    DirList.Free;
  end;
end;

function FindFileInDirWildCard(const Filename, Path: String): String;
var
  FilesFound: TStringList;
  AFile: string;
begin
  Result := '';
  FilesFound := FindAllFiles(Path, Filename, true);
  try
    if FilesFound.Count>0 then
    begin
      for AFile in FilesFound do
      begin
        if (NOT FileIsSymlink(AFile)) then
        begin
          result:=AFile;
          break;
        end;
      end;
    end;
  finally
    FilesFound.Free;
  end;
end;

function FileCopy(const Src,Dest : string;const Flags: TCopyFileFlags):boolean;
Var
  D : String;
  Fin,FOut : TFileStream;
  Count : Int64;
  A : Integer;
{$ifdef UNIX}
  FileStat: stat;
{$endif UNIX}
begin
  result:=false;
  D:=IncludeTrailingPathDelimiter(Dest);
  if DirectoryExists(D) then
  begin
    D:=D+ExtractFileName(Src);
  end
  else
  begin
    D:=Dest;
  end;
  if (NOT (cffOverwriteFile in Flags)) and FileExists(D) then exit;
  {$ifdef DARWIN}
  { First delete file on Darwin OS to avoid codesign issues }
  if FileExists(D) then SysUtils.DeleteFile(D);
  {$endif DARWIN}
  FIn:=TFileStream.Create(Src,fmopenRead or fmShareDenyNone);
  try
    FOut:=TFileStream.Create(D,fmCreate or fmShareDenyNone);
    try
      Count:=Fout.CopyFrom(FIn,0);
      result:=(Count=Fin.Size);
      if (NOT result) then exit;
    finally
      FreeAndNil(Fout);
    end;
    A:=FileGetDate(FIn.Handle);
    If (A<>-1) then FileSetDate(D,A);
{$ifdef UNIX}
    // Copy the file-access rights on Unix, especially the executable-bit
    filestat:=Default(stat);
    if (FpStat(Src,FileStat)=0) then FpChmod(D,FileStat.st_mode);
{$endif UNIX}
  finally
    FreeAndNil(Fin);
  end;
end;

function DirCopy(const SourcePath, DestPath: String): Boolean;
begin
  result:=FileUtil.CopyDirTree(SourcePath, DestPath,[cffOverwriteFile,cffCreateDestDirectory{,cffPreserveTime}]);
end;

function CheckDirectory(const DirectoryName: string; const CheckRoot: boolean = false):boolean;
{$ifndef Windows}
const
  FORBIDDENFOLDERS:array[0..11] of string = ('/bin','/boot','/dev','/lib','/lib32','/lib64','/proc','/root','/run','/sbin','/sys','/var');
{$endif}
var
  s,aDirectory:string;
  i:integer;
begin
  result:=true;
  aDirectory:=LowerCase(DirectoryName);
  if aDirectory='' then exit;
  if aDirectory=DirectorySeparator then exit;
  {$ifndef Windows}
  s:=LowerCase(IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('HOME')));
  if s=aDirectory then exit;
  s:=IncludeTrailingPathDelimiter(s);
  if s=aDirectory then exit;
  for s in FORBIDDENFOLDERS do
  begin
    if (Pos(s,aDirectory)=1) then exit;
  end;
  {$else}
  if Length(aDirectory)<=3 then exit;
  i:=Pos('\windows',aDirectory);
  if ((i>0) AND (i<4)) then exit;
  {$endif}
  if ((CheckRoot) AND (NOT ParentDirectoryIsNotRoot(aDirectory))) then exit;
  result:=false;
end;

function DeleteDirectoryEx(DirectoryName: string): boolean;
// Lazarus fileutil.DeleteDirectory on steroids, works like
// deltree <directory>, rmdir /s /q <directory> or rm -rf <directory>
// - removes read-only files/directories (DeleteDirectory doesn't)
// - removes directory itself
// Adapted from fileutil.DeleteDirectory, thanks to Paweł Dmitruk
var
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
  CurSrcDir: String;
  CurFilename: String;
begin
  result:=false;

  if CheckDirectory(DirectoryName) then exit;

  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    result:=true;
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        // Remove read-only file attribute so we can delete it:
        if (FileInfo.Attr and faReadOnly)>0 then
          FileSetAttr(CurFilename, FileInfo.Attr-faReadOnly);
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and {%H-}faSymLink)=0) {$endif unix} then
        begin
          // Directory; exit with failure on error
          if not DeleteDirectoryEx(CurFilename) then result:=false;
        end
        else
        begin
          // File; exit with failure on error
          if not SysUtils.DeleteFile(CurFilename) then result:=false;
        end;
      end;
    until (SysUtils.FindNext(FileInfo)<>0) OR (NOT result);
    SysUtils.FindClose(FileInfo);
  end;
  // Remove root directory; exit with failure on error:
  if result then result:=RemoveDir(DirectoryName);
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
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
  AllFiles: boolean;
  CurSrcDir: String;
  CurFilename: String;
begin
  result:=false;

  if CheckDirectory(DirectoryName) then exit;

  AllFiles:=(Names.Count=0);
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    result:=true;
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and {%H-}faSymLink)=0) {$endif unix} then
        begin
          // Directory; call recursively exit with failure on error
          if not DeleteFilesSubDirs(CurFilename,Names,OnlyIfPathHas) then result:=false;
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
                FileSetAttr(CurFilename, FileInfo.Attr-faReadOnly);
              if not SysUtils.DeleteFile(CurFilename) then result:=false;
            end;
          end;
        end;
      end;
    until (SysUtils.FindNext(FileInfo)<>0) OR (NOT result);
    SysUtils.FindClose(FileInfo);
  end;
end;

function DeleteFilesExtensionsSubdirs(const DirectoryName: string; const Extensions:TStringList; const OnlyIfPathHas: string): boolean;
// Deletes all files ending in one of the extensions, starting from
// DirectoryName and recursing down.
// It only deletes files if any directory of the path contains OnlyIfPathHas,
// unless that is empty
// Extensions can contain * to cover everything [other extensions will then be ignored]
// making it delete all files, but leaving the directories.
// Will try to remove read-only files.
//todo: check how this works with case insensitive file system like Windows
var
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
  AllFiles: boolean;
  CurSrcDir: String;
  CurSearchPath: String;
  CurFilename: String;
  i: integer;
  CurSrcDirValid:boolean;
begin
  result:=false;

  if CheckDirectory(DirectoryName) then
  begin
    ThreadLog('Something wrong with directory: ' + DirectoryName + ' .',etError);
    exit;
  end;

  // Make sure we can compare extensions using ExtractFileExt
  for i:=0 to Extensions.Count-1 do
  begin
    if copy(Extensions[i],1,1)<>'.' then Extensions[i]:='.'+Extensions[i];
  end;
  AllFiles:=(Extensions.Count=0) or (Extensions.IndexOf('.*')>=0);
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  CurSrcDirValid:=((OnlyIfPathHas='') OR (Pos(DirectorySeparator+OnlyIfPathHas,CurSrcDir)>0));
  CurSearchPath:=CurSrcDir+GetAllFilesMask;
  if SysUtils.FindFirst(CurSearchPath,faAnyFile{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    result:=true;
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and {%H-}faSymLink)=0) {$endif unix} then
        begin
          // Directory; call recursively exit with failure on error
          if not DeleteFilesExtensionsSubdirs(CurFilename, Extensions,OnlyIfPathHas) then result:=false;
        end
        else
        begin
          // If we are in the right path:
          //todo: get utf8 replacement for ExtractFilePath
          if CurSrcDirValid then
          begin
            // Only delete if extension is right
            if AllFiles or (Extensions.IndexOf(ExtractFileExt(FileInfo.Name))>=0) then
            begin
              // Remove read-only file attribute so we can delete it:
              if (FileInfo.Attr and faReadOnly)>0 then
                FileSetAttr(CurFilename, FileInfo.Attr-faReadOnly);
              if not SysUtils.DeleteFile(CurFilename) then result:=false;
              if (NOT result) then
              begin
                ThreadLog('Delete error of file: ' + CurFilename + ' .',etError);
              end;
            end;
          end;
        end;
      end;
    until (SysUtils.FindNext(FileInfo)<>0) OR (NOT result);
    SysUtils.FindClose(FileInfo);
  end;
  // Remove root directory; exit with failure on error:
  if (result AND CurSrcDirValid) then
  begin
    if DirectoryIsEmpty(CurSrcDir) then result:=RemoveDir(CurSrcDir);
  end;
end;

function DeleteFilesNameSubdirs(const DirectoryName: string; const OnlyIfNameHas: string): boolean;
// Deletes all files containing OnlyIfNameHas
// DirectoryName and recursing down.
// Will try to remove read-only files.
//todo: check how this works with case insensitive file system like Windows
var
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
  AllFiles: boolean;
  CurSrcDir: String;
  CurFilename: String;
begin
  result:=false;

  if CheckDirectory(DirectoryName) then exit;

  AllFiles:=(Length(OnlyIfNameHas)=0);

  // for now, exit when no filename data is given ... use DeleteDirectoryEx
  if AllFiles then exit;

  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    result:=true;
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and {%H-}faSymLink)=0) {$endif unix} then
        begin
          // Directory; call recursively exit with failure on error
          if not DeleteFilesNameSubdirs(CurFilename, OnlyIfNameHas) then result:=false;
        end
        else
        begin
          if AllFiles or (Pos(UpperCase(OnlyIfNameHas),UpperCase(FileInfo.Name))>0) then
          begin
            // Remove read-only file attribute so we can delete it:
            if (FileInfo.Attr and faReadOnly)>0 then
              FileSetAttr(CurFilename, FileInfo.Attr-faReadOnly);
            if not SysUtils.DeleteFile(CurFilename) then result:=false;
          end;
        end;
      end;
    until (SysUtils.FindNext(FileInfo)<>0) OR (NOT result);
    SysUtils.FindClose(FileInfo);
  end;
end;

function DownloadBase(aDownLoader:TBasicDownloader;URL: string; aDataStream:TStream; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
begin
  result:=false;

  //if (Length(aDownloader.FilenameOnly)>0) then ThreadLog('Using native downloader to download '+aDownloader.FilenameOnly);
  //ThreadLog('Using native downloader to download '+URI.Document+' from '+URI.Path);

  {$ifdef mswindows}
  if (Pos('/openssl',URL)>0) AND (Pos('.zip',URL)>0) then
  begin
    // Skip native download of ssl libraries : will not work without these libraries
    if aDownLoader.InheritsFrom(TNativeDownLoader) then exit;
  end;
  {$endif}

  if Length(HTTPProxyHost)>0 then aDownLoader.setProxy(HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);

  {$ifdef mswindows}
  if (NOT aDownLoader.checkURL(URL)) then exit;
  {$endif}

  result:=aDownLoader.getStream(URL,aDataStream);
end;

function DownloadBase(aDownLoader:TBasicDownloader;URL, TargetFile: string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
var
  aFile:TDownloadStream;
  i:integer;
begin
  result:=false;

  aDownLoader.FFileNameOnly:=ExtractFileName(TargetFile);

  aFile:=TDownloadStream.Create(TargetFile,fmCreate);
  try
    if (Pos('api.github.com',URL)>0) AND (Pos('fpcupdeluxe',URL)>0) then
    begin
      aDownLoader.UserAgent:=FPCUPUSERAGENT;
      aDownLoader.ContentType:='application/json';
      //aDownLoader.Accept:='';
    end
    else
    begin
      aDownLoader.UserAgent:=NORMALUSERAGENT;
      //aDownLoader.UserAgent:='Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2725.0 Safari/537.36';
      aDownLoader.ContentType:='';
      //aDownLoader.Accept:='text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
      //aDownLoader.Accept:='*/*';
    end;
    result:=DownloadBase(aDownLoader,URL,aFile,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
  finally
    aFile.Free;
  end;
  if (NOT result) then
  begin
    if FileExists(TargetFile) then SysUtils.DeleteFile(TargetFile); // delete stale targetfile
  end;
end;


{$ifdef MSWindows}
function DownloadByWinINet(URL: string; aDataStream: TSTream): boolean;
var
  URI    : URIPARSER.TURI;
  aURL,P : String;
  NetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[0..1023] of Byte;
  Error,BytesRead: DWord;
  dummy: DWORD;
  s:string;
begin
  result:=false;

  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;

  //do not use WinINet for FTP
  if AnsiStartsText('ftp',P) then exit;

  //do not use WinINet for sourceforge : redirect is not working !
  //if CompareText(URI.Host,'downloads.sourceforge.net')=0 then exit;
  // a bit tricky: we know where sourceforge redirects, so go there ... ;-)
  if CompareText(URI.Host,'downloads.sourceforge.net')=0 then
  begin
    URI.Host:='netix.dl.sourceforge.net';
    aURL:=P+'://'+URI.Host+URI.Path+URI.Document
  end else aURL:=URL;

  if (Pos('api.github.com',URL)>0) AND (Pos('fpcupdeluxe',URL)>0) then
    NetHandle := InternetOpen(FPCUPUSERAGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0)
  else
    NetHandle := InternetOpen(WININETUSERAGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  // NetHandle valid?
  if Assigned(NetHandle) then
  try
    ThreadLog('Using WinINet to download '+URI.Document+' from '+URI.Path);

    UrlHandle := InternetOpenUrl(NetHandle, PChar(aURL), nil, 0, INTERNET_FLAG_NO_UI or INTERNET_FLAG_RELOAD, 0);

    {
    Error:=GetLastError;
    if Error>0 then
    begin
      WinInetErrorMsg(Error);
    end;
    }

    // UrlHandle valid?
    if Assigned(UrlHandle) then
    try
      DeleteUrlCacheEntry(PChar(aURL));

      FillChar({%H-}Buffer, SizeOf(Buffer), #0);
      dummy := 0;
      BytesRead := SizeOf(Buffer);
      if HttpQueryInfo(UrlHandle,HTTP_QUERY_STATUS_CODE,@Buffer[0],BytesRead,dummy) then
      begin
        s:=GetStringFromBuffer(PChar(@Buffer));
        if s='200' then
        begin
          //All ok : get file.
          SetLastError(0);
          FillChar({%H-}Buffer, SizeOf(Buffer), #0);
          while true do
          begin
            if InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer)-1, {%H-}BytesRead) then
            begin
              if (BytesRead = 0) then Break;
              aDataStream.Write(Buffer, BytesRead);
            end
            else
            if InternetQueryDataAvailable(UrlHandle, {%H-}BytesRead, 0, 0) then
            begin
              if (BytesRead = 0) then Break;
            end
            else break;
          end;
          {
          while InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), {%H-}BytesRead) do
          begin
            if (BytesRead = 0) then Break;
            aDataStream.Write(Buffer, BytesRead);
          end;
          }
          //Buffer[0] := 0;
          //aDataStream.Write(Buffer, 1);
          result:=(aDataStream.Size>1);
        end;
      end;
    finally
      InternetCloseHandle(UrlHandle);
    end
  finally
    InternetCloseHandle(NetHandle);
  end;
end;

function WinInetErrorMsg(Err: DWORD): string;
var
  ErrMsg: array of Char;
  ErrLen: DWORD;
begin
  if Err = ERROR_INTERNET_EXTENDED_ERROR then
  begin
    ErrLen := 0;
    InternetGetLastResponseInfo(@Err, nil, ErrLen);
    if GetLastError() = ERROR_INSUFFICIENT_BUFFER then
    begin
      SetLength({%H-}ErrMsg, ErrLen);
      InternetGetLastResponseInfo(@Err, PChar(ErrMsg), ErrLen);
      SetString(Result, PChar(ErrMsg), ErrLen);
    end else begin
      Result := 'Unknown WinInet error';
    end;
  end else
    Result := SysErrorMessage(Err);
end;

function DownloadByWinINet(URL, TargetFile: string): boolean;
var
  aStream:TFileStream;
begin
  result:=false;

  aStream := TFileStream.Create(TargetFile, fmCreate);
  try
    result:=DownloadByWinINet(URL,aStream);
  finally
    aStream.Free;
  end;
  if result then
  begin
    result:=FileExists(TargetFile);
  end;
end;

function DownloadByPowerShell(URL, TargetFile: string): boolean;
var
  Output : String;
  URI    : URIPARSER.TURI;
  aURL,P : String;
begin
  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;
  //result:=(ExecuteCommandCompat('powershell -command "[Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12; (new-object System.Net.WebClient).DownloadFile('''+URL+''','''+TargetFile+''')"', Output, False)=0);
  //result:=(ExecuteCommandCompat('powershell -command "(new-object System.Net.WebClient).DownloadFile('''+URL+''','''+TargetFile+''')"', Output, False)=0);

  if (Pos('api.github.com',URL)>0) AND (Pos('fpcupdeluxe',URL)>0) then
    P:=FPCUPUSERAGENT
  else
    P:=NORMALUSERAGENT;

  ThreadLog('Using PowerShell to download '+URI.Document+' from '+URI.Path);

  result:=RunCommand('powershell' ,['-command','"$cli = New-Object System.Net.WebClient;$cli.Headers[''User-Agent''] = '''+P+''';$cli.DownloadFile('''+URL+''','''+TargetFile+''')"'],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});

  if result then
  begin
    result:=FileExists(TargetFile);
  end;
end;

function DownloadByBitsAdmin(URL, TargetFile: string): boolean;
var
  Output : String;
  URI    : URIPARSER.TURI;
  aURL,P : String;
begin
  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;

  ThreadLog('Using bitsadmin to download '+URI.Document+' from '+URI.Path);

  result:=RunCommand('bitsadmin.exe',['/transfer','"JobName"',URL,TargetFile],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
  if result then
  begin
    result:=FileExists(TargetFile);
  end;
end;
{$endif MSWindows}

{$IFDEF USEMORMOT}
function DownloadBymORMot(URL, TargetFile: string): boolean;
var
  params   : THttpClientSocketWGet;
  URI      : URIPARSER.TURI;
  aURL,P,H : String;
  s        : THttpClientSocket;
  u        : System.UTF8String;
begin
  result:=false;

  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;
  H:=URI.Host;

  if AnsiStartsText('downloads.sourceforge.net',H) then exit;

  if AnsiStartsText('http',P) then
  begin
    params.Clear;
    params.Resume := false;

    params.OnStreamCreate:=@TDownloadStream.StreamCreate;
    try
      s := THttpClientSocket.OpenUri(URL, u, '', 10000, nil);
      try
        s.RedirectMax := 10;
        s.UserAgent:=FPCUPUSERAGENT;
        s.ContentType:='application/zip';
        if (s.WGet(u, TargetFile, params) = TargetFile) then
        begin
          result:=true;
        end;
      finally
        s.Free;
      end;

      //if (params.WGet(aURL,TargetFile,'', nil, 10000, 10) = TargetFile) then
      //begin
      //  result:=true;
      //end;
    except
      // Swallow exceptions
    end;
  end;

end;
{$ENDIF USEMORMOT}

function Download(UseWget:boolean; URL: string; aDataStream:TStream; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
var
  aDownLoader:TBasicDownLoader;
begin
  result:=false;

  if (NOT result) then
  begin
    if UseWget
       then aDownLoader:=TWGetDownLoader.Create
       else aDownLoader:=TNativeDownLoader.Create;
    try
      if (Pos('api.github.com',URL)>0) AND (Pos('fpcupdeluxe',URL)>0) then
      begin
        aDownLoader.UserAgent:=FPCUPUSERAGENT;
        aDownLoader.ContentType:='application/json';
      end;
      result:=DownloadBase(aDownLoader,URL,aDataStream,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
    finally
      aDownLoader.Destroy;
    end;
  end;

  {$ifndef USEONLYCURL}
  {$ifdef Windows}
  if (NOT result) then
  begin
    //Second resort on older Windows: use Windows INet
    // Available from Windows 2000 and better = NT 5.0
    if (CheckWin32Version(5,0)) then
    begin
      result:=DownloadByWinINet(URL,aDataStream);
    end;
  end;
  {$endif}
  {$endif USEONLYCURL}

  //Final resort: use wget by force
  if (NOT result) AND (NOT UseWget) then
  begin
    aDownLoader:=TWGetDownLoader.Create;
    try
      result:=DownloadBase(aDownLoader,URL,aDataStream,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
    finally
      aDownLoader.Destroy;
    end;
  end;

end;


function Download(UseWget:boolean; URL, TargetFile: string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''): boolean;
var
  aDownLoader:TBasicDownLoader;
begin
  result:=false;

  {$IFDEF USEMORMOT}
  if (NOT result) AND (NOT UseWget) then
  begin
    result:=DownloadBymORMot(URL,TargetFile);
  end;
  {$ENDIF USEMORMOT}

  if (NOT result) then
  begin
    SysUtils.Deletefile(TargetFile);
    if UseWget
       then aDownLoader:=TWGetDownLoader.Create
       else aDownLoader:=TNativeDownLoader.Create;
    try
      result:=DownloadBase(aDownLoader,URL,TargetFile,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
    finally
      aDownLoader.Destroy;
    end;
  end;

  {$ifndef USEONLYCURL}
  {$ifdef MSWindows}
  if (NOT result) then
  begin
    //Second resort on older Windows: use Windows INet
    // Available from Windows 2000 and better = NT 5.0
    if (CheckWin32Version(5,0)) then
    begin
      SysUtils.Deletefile(TargetFile);
      result:=DownloadByWinINet(URL,TargetFile);
    end;
  end;

  //Second or third resort: use Windows PowerShell on Windows 7 and better
  if (NOT result) then
  begin
    if (CheckWin32Version(6,1)) then
    begin
      SysUtils.Deletefile(TargetFile);
      result:=DownloadByPowerShell(URL,TargetFile);
    end;
  end;

  if (NOT result) then
  begin
    //Third or fourth resort: use BitsAdmin on older Windows starting from 3.0
    if (CheckWin32Version(3,0) AND (NOT CheckWin32Version(10,0))) then
    begin
      SysUtils.Deletefile(TargetFile);
      result:=DownloadByBitsAdmin(URL,TargetFile);
    end;
  end;

  {$endif}
  {$endif USEONLYCURL}

  //Final resort: use wget by force
  if (NOT result) AND (NOT UseWget) then
  begin
    SysUtils.Deletefile(TargetFile);
    aDownLoader:=TWGetDownLoader.Create;
    try
      result:=DownloadBase(aDownLoader,URL,TargetFile,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
    finally
      aDownLoader.Destroy;
    end;
  end;

  if (NOT result) then SysUtils.Deletefile(TargetFile);
end;

function GetGitLabTags(aURL:string;taglist:TStringList):boolean;
var
  Output:string;
  OutputLines:TStringList;
begin
  result:=false;
  if RunCommand('git',['ls-remote','--tags','--sort=-refname',aURL,'refs/tags/release*'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}{$ENDIF}) then
  //if RunCommand('git',['ls-remote','--tags','--abbrev=0','--sort=-refname',aURL], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}{$ENDIF}) then
  begin
    if (Output<>'') then
    begin
      OutputLines:=TStringList.Create;
      try
        OutputLines.Text:=Output;
      finally
        OutputLines.Destroy;
      end;
    end;
  end;
end;

function GetURLDataFromCache(aURL:string; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''):string;
var
  aStore:TURLDataCache;
  s:string;
  Ss: TStringStream;
  success:boolean;
  i:integer;
begin
  s:='';

  i:=0;
  success:=false;
  if Length(URLDataCache)>0 then
  begin
    for aStore in URLDataCache do
    begin
      if (aStore.URL=aURL) then
      begin
        s:=aStore.Data;
        success:=true;
        break;
      end;
      Inc(i);
    end;
  end;

  //if (Length(s)=0) then
  if ((Length(s)=0) OR ( (Pos('api.github.com',aURL)<>0) AND (Pos('"message": "Not Found"',s)<>0)) ) then
  begin
    // Indicate that we cannot reuse index
    if (NOT success) then i:=-1;
    success:=false;
  end;

  if (NOT success) then
  begin
    s:='';
    Ss := TStringStream.Create('');
    try
      success:=Download(False,aURL,Ss,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
      if (NOT success) then
      begin
        {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
        Ss.Clear;
        {$ENDIF}
        Ss.Position:=0;
        success:=Download(True,aURL,Ss,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
      end;
      if success then s:=Ss.DataString;
    finally
      Ss.Free;
    end;

    if (success AND (Length(s)>0)) then
    begin
      if (i=-1) then
      begin
        SetLength(URLDataCache,Length(URLDataCache)+1);
        i:=High(URLDataCache);
      end;
      with URLDataCache[i] do
      begin
        URL:=aURL;
        Data:=s;
      end;
    end;
  end;

  result:=s;
end;

function GetGitHubFileList(aURL:string;fileurllist:TStringList; bWGet:boolean=false; HTTPProxyHost: string=''; HTTPProxyPort: integer=0; HTTPProxyUser: string=''; HTTPProxyPassword: string=''):boolean;
var
  Ss: TStringStream;
  Content : string;
  Json : TJSONData;
  JsonObject : TJSONObject;
  JsonArray: TJSONArray;
  i:integer;
  aStore:TGitHubStore;
begin
  result:=false;

  json:=nil;

  Content:='';

  if (aURL='') then exit;

  if Length(GitHubFileListCache)>0 then
  begin
    for aStore in GitHubFileListCache do
    begin
      if (aStore.URL=aURL) then
      begin
        if (aStore.FileList.Count>0) then
        begin
          result:=true;
          for Content in aStore.FileList do fileurllist.Add(Content);
        end;
        exit;
      end;
    end;
  end;

  SetLength(GitHubFileListCache,Length(GitHubFileListCache)+1);
  with GitHubFileListCache[High(GitHubFileListCache)] do
  begin
    URL:=aURL;
    FileList:=TStringList.Create;
  end;

  if (NOT result) then
  begin
    Content:=GetURLDataFromCache(aURL,HTTPProxyHost,HTTPProxyPort,HTTPProxyUser,HTTPProxyPassword);
    result:=((Length(Content)>0) AND (Pos('"message": "Not Found"',Content)=0));

    Json:=nil;
    if result then
    begin
      try
        Json:=GetJSON(Content);
      except
        Json:=nil;
      end;
    end;

    result:=(Json<>nil);

    if result then
    begin
        try
          JsonArray:=Json.FindPath('assets') as TJSONArray;
          i:=JsonArray.Count;
          if i=0 then
            result:=false
          else
            begin
              while (i>0) do
              begin
                Dec(i);
                JsonObject := JsonArray.Objects[i];
                Content:=JsonObject.Get('browser_download_url');
                fileurllist.Add(Content);
                with GitHubFileListCache[High(GitHubFileListCache)] do FileList.Add(fileurllist[(fileurllist.Count-1)]);
              end;
            end;
        finally
          Json.Free;
        end;
    end;

  end;

end;

// returns file size in bytes or 0 if not found.
function FileSize(FileName: string) : Int64;
var
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
begin
  if SysUtils.FindFirst(FileName, faAnyFile, FileInfo ) = 0 then
  {$ifdef MSWindows}
     result := Int64(FileInfo.FindData.nFileSizeHigh) shl Int64(32) + Int64(FileInfo.FindData.nFileSizeLow)
  {$else}
    result := FileInfo.Size
  {$endif}
  else
     result := 0;
  SysUtils.FindClose(FileInfo);
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
function CheckFileSignature(aFilePath: string): boolean;
var
  s:TFileStream;
  magic:word;
  offset:integer;
begin
  result:=true;
  if NOT FileExists(aFilePath) then exit;
  try
    s:=TFileStream.Create(aFilePath,fmOpenRead);
    try
      s.Position:=0;
      magic:=s.ReadWord;
      if magic<>$5A4D then exit;
      s.Seek(60,soBeginning);
      offset:=0;
      s.ReadBuffer(offset,4);
      s.Seek(offset,soBeginning);
      magic:=s.ReadWord;
      if magic<>$4550 then exit;
      s.Seek(offset+4,soBeginning);
      magic:=s.ReadWord;
    finally
      s.Free;
    end;

    {$ifdef win32}
    result:=(magic=$014C);
    {$endif}
    {$ifdef win64}
    result:=((magic=$0200) OR (magic=$8664));
    {$endif}
  except
    result:=true;
  end;
end;

{
function GetWindowsFolder(aFolder:TGUID): string;
var
  w : pwidechar;
begin
  SHGetKnownFolderPath(aFolder,0,0,w);
  Result := w;
  CoTaskMemFree(w);
end;
}

function GetWindowsFolderOld(aFolder:longint): string;
var
  w :  Array[0..MaxPathLen] of Char;
begin
  SHGetSpecialFolderPath(0,w,aFolder,false);
  Result := w;
end;

function GetWindowsDownloadFolder: string;
begin
  {
  // if Vista or higher: use modern new functions
  if aMajor>5 then
    result:=GetWindowsFolder(FOLDERID_Downloads)
  else
  }
    result:=GetWindowsFolderOld(CSIDL_MYDOCUMENTS);
end;

function GetWindowsAppDataFolder: string;
begin
  {
  // if Vista or higher: use modern new functions
  if aMajor>5 then
    result:=GetWindowsFolder(FOLDERID_LocalAppData)
  else
  }
    result:=GetWindowsFolderOld(CSIDL_LOCAL_APPDATA);
end;
{$ENDIF MSWINDOWS}

function MoveFile(const SrcFilename, DestFilename: string): boolean;
// We might (in theory) be moving files across partitions so we cannot use renamefile
begin
  try
    if FileExists(SrcFileName) then
    begin
      if FileCopy(SrcFilename, DestFileName) then SysUtils.DeleteFile(SrcFileName);
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

function FileCorrectLineEndings(const SrcFilename, DestFilename: string): boolean;
var
  FileSL:TStringList;
begin
  result:=false;
  try
    if FileExists(SrcFileName) then
    begin
      FileSL:=TStringList.Create;
      try
        FileSL.LoadFromFile(SrcFileName);
        SysUtils.DeleteFile(DestFilename);
        FileSL.SaveToFile(DestFilename);
        result:=true;
      finally
        FileSL.Free;
      end;
    end;
  except
  end;
end;

function FixPath(const s:string):string;
var
  i : longint;
begin
  { Fix separator }
  result:=s;
  {$IFDEF MSWINDOWS}
  for i:=2 to length(s) do
  {$ELSE}
  for i:=1 to length(s) do
  {$ENDIF}
   if (s[i] in ['/','\']){$IFDEF MSWINDOWS} AND (s[i-1]<>' '){$ENDIF} then
    result[i]:=DirectorySeparator;
end;

function FileIsReadOnly(const s:string):boolean;
begin
  result:=((FileGetAttr(s) AND faReadOnly) > 0);
end;

function MaybeQuoted(const s:string):string;
const
  FORBIDDEN_CHARS_DOS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                     '{', '}', '''', '`', '~'];
  FORBIDDEN_CHARS_OTHER = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                     '{', '}', '''', ':', '\', '`', '~'];
var
  forbidden_chars: set of char;
  i  : integer;
  quote_char: ansichar;
  quoted : boolean;
begin
  {$ifdef Windows}
  forbidden_chars:=FORBIDDEN_CHARS_DOS;
  quote_char:='"';
  {$else}
  forbidden_chars:=FORBIDDEN_CHARS_OTHER;
  include(forbidden_chars,'"');
  quote_char:='''';
  {$endif}

  quoted:=false;
  result:=quote_char;
  for i:=1 to length(s) do
   begin
     if s[i]=quote_char then
       begin
         quoted:=true;
         result:=result+'\'+quote_char;
       end
     else case s[i] of
       '\':
         begin
           {$ifdef UNIX}
           result:=result+'\\';
           quoted:=true;
           {$else}
           result:=result+'\';
           {$endif}
         end;
       ' ',
       #128..#255 :
         begin
           quoted:=true;
           result:=result+s[i];
         end;
       else begin
         if s[i] in forbidden_chars then
           quoted:=True;
         result:=result+s[i];
       end;
     end;
   end;
  if quoted then
    result:=result+quote_char
  else
    result:=s;
end;

function MaybeQuotedSpacesOnly(const s:string):string;
begin
  if (Pos(' ',s)>0) then
    result:='"'+s+'"'
  else
    result:=s;
end;

function UnQuote(const s:string):string;
const
  QUOTE_CHARS = ['@', '#', '$', '%', '^', '&', '*', '''', '`', '~', '"'];
var
  sl:word;
begin
  result := s;
  sl:=length(result);
  if (sl>1) then
  begin
     if ((result[1] in QUOTE_CHARS) AND (result[sl] in QUOTE_CHARS)) then
     begin
       result:=Copy(result,2,sl-2);
     end;
  end;
end;

function StringsStartsWith(const SearchIn:array of string; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  for i:=StartIndex to High(SearchIn) do
  begin
    if CS then
      Found:=AnsiStartsStr(SearchFor,TrimLeft(SearchIn[i]))
    else
      Found:=AnsiStartsText(SearchFor,TrimLeft(SearchIn[i]));
    if Found then break;
  end;
  if Found then
    result:=i
  else
    result:=-1;
end;

function StringsSame(const SearchIn:array of string; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  for i:=StartIndex to High(SearchIn) do
  begin
    if CS then
      Found:=AnsiSameStr(SearchFor,TrimLeft(SearchIn[i]))
    else
      Found:=AnsiSameText(SearchFor,TrimLeft(SearchIn[i]));
    if Found then break;
  end;
  if Found then
    result:=i
  else
    result:=-1;
end;


function StringListStartsWith(SearchIn:TStringList; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  result:=-1;
  if (StartIndex>=SearchIn.Count) then exit;
  for i:=StartIndex to Pred(SearchIn.Count) do
  begin
    if CS then
      Found:=AnsiStartsStr(SearchFor,TrimLeft(SearchIn[i]))
    else
      Found:=AnsiStartsText(SearchFor,TrimLeft(SearchIn[i]));
    if Found then break;
  end;
  if Found then
    result:=i;
end;

function StringListEndsWith(SearchIn:TStringList; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  result:=-1;
  if (StartIndex>=SearchIn.Count) then exit;
  for i:=StartIndex to Pred(SearchIn.Count) do
  begin
    if CS then
      Found:=AnsiEndsStr(SearchFor,TrimRight(SearchIn[i]))
    else
      Found:=AnsiEndsText(SearchFor,TrimRight(SearchIn[i]));
    if Found then break;
  end;
  if Found then
    result:=i;
end;

function StringListContains(SearchIn:TStringList; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  result:=-1;
  if (StartIndex>=SearchIn.Count) then exit;
  for i:=StartIndex to Pred(SearchIn.Count) do
  begin
    if CS then
      Found:=AnsiContainsStr(SearchIn[i],SearchFor)
    else
      Found:=AnsiContainsText(SearchIn[i],SearchFor);
    if Found then break;
  end;
  if Found then
    result:=i;
end;

function StringListSame(SearchIn:TStringList; SearchFor:string; StartIndex:integer; CS:boolean): integer;
var
  Found:boolean=false;
  i:integer;
begin
  result:=-1;
  if (StartIndex>=SearchIn.Count) then exit;
  for i:=StartIndex to Pred(SearchIn.Count) do
  begin
    if CS then
      Found:=AnsiSameStr(SearchIn[i],SearchFor)
    else
      Found:=AnsiSameText(SearchIn[i],SearchFor);
    if Found then break;
  end;
  if Found then
    result:=i;
end;

function OccurrencesOfChar(const ContentString: string; const CharToCount: char): integer;
var
  C: Char;
begin
  result := 0;
  for C in ContentString do
    if C = CharToCount then
      Inc(result);
end;

{$IFDEF UNIX}
function GetStartupObjects:string;
const
  LINKFILE='crtbegin.o';
  SEARCHDIRS : array [0..5] of string = (
    '/usr/local/lib/',
    '/usr/lib/',
    '/usr/local/lib/gcc/',
    '/usr/lib/gcc/',
    '/lib/gcc/',
    '/lib/'
    );

var
  LinkFiles     : TStringList;
  Output,s1,s2  : string;
  i,j           : integer;
  ReturnCode    : integer;
  FoundLinkFile : boolean;
  OutputLines   : TStringList;
begin
  FoundLinkFile:=false;
  result:='';

  for i:=Low(SEARCHDIRS) to High(SEARCHDIRS) do
  begin
    s1:=SEARCHDIRS[i];
    if FileExists(s1+LINKFILE) then FoundLinkFile:=true;
    if FoundLinkFile then
    begin
      result:=s1;
      break;
    end;
  end;

  {$ifdef Haiku}
  if (NOT FoundLinkFile) then
  begin
    s1:='/boot/system/develop/tools/x86/lib';
    if NOT DirectoryExists(s1) then s1:='/boot/system/develop/tools/lib';
    if DirectoryExists(s1) then
    begin
      LinkFiles := TStringList.Create;
      try
        FindAllFiles(LinkFiles, s1, '*.o', true);
        if (LinkFiles.Count>0) then
        begin
          for i:=0 to (LinkFiles.Count-1) do
          begin
            if Pos(DirectorySeparator+LINKFILE,LinkFiles[i])>0 then
            begin
              result:=ExtractFileDir(LinkFiles[i]);
              FoundLinkFile:=true;
              break;
            end;
          end;
        end;
      finally
        LinkFiles.Free;
      end;
    end;
    if (NOT FoundLinkFile) then
    begin
      s1:='/boot/system/develop/lib/x86/';
      if NOT DirectoryExists(s1) then s1:='/boot/system/develop/lib/';
      if FileExists(s1+'crti.o') then FoundLinkFile:=true;
      if FoundLinkFile then result:=s1;
    end;
  end;
  {$endif}

  if FoundLinkFile then exit;

  try
    Output:='';
    if RunCommand('gcc',['-print-prog-name=cc1'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
    begin
      s1:=Trim(Output);
      if FileExists(s1) then
      begin
        s2:=ExtractFileDir(s1);
        if FileExists(s2+DirectorySeparator+LINKFILE) then
        begin
          result:=s2;
          FoundLinkFile:=true;
        end;
      end;
    end;

    if (NOT FoundLinkFile) then
    begin
      Output:='';
      if RunCommand('gcc',['-print-search-dirs'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
      begin
        Output:=TrimRight(Output);
        if Length(Output)>0 then
        begin
          OutputLines:=TStringList.Create;
          try
            OutputLines.Text:=Output;
            if OutputLines.Count>0 then
            begin
              for i:=0 to (OutputLines.Count-1) do
              begin
                s1:=OutputLines.Strings[i];
                j:=Pos('libraries:',s1);
                if j=1 then
                begin
                  j:=Pos(DirectorySeparator,s1);
                  if j>0 then
                  begin
                    Delete(s1,1,j-1);
                    LinkFiles := TStringList.Create;
                    try
                      LinkFiles.StrictDelimiter:=true;
                      LinkFiles.Delimiter:=':';
                      LinkFiles.DelimitedText:=s1;
                      if LinkFiles.Count>0 then
                      begin
                        for j:=0 to (LinkFiles.Count-1) do
                        begin
                          s2:=ExcludeTrailingPathDelimiter(LinkFiles.Strings[j]);
                          //s2:=ExtractFileDir(LinkFiles.Strings[j]);
                          if FileExists(s2+DirectorySeparator+LINKFILE) then
                          begin
                            result:=s2;
                            FoundLinkFile:=true;
                            break;
                          end;
                        end;
                      end;
                    finally
                      LinkFiles.Free;
                    end;
                  end;
                  break;
                end;
              end;
            end;
          finally
            OutputLines.Free;
          end;
        end;
      end;
    end;

    if (NOT FoundLinkFile) then
    begin
      Output:='';
      if RunCommand('gcc',['-v'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
      begin

        s1:='COLLECT_LTO_WRAPPER=';
        i:=Ansipos(s1, Output);
        if (i>0) then
        begin
          s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
          // find space as delimiter
          i:=Ansipos(' ', s2);
          // find lf as delimiter
          j:=Ansipos(#10, s2);
          if (j>0) AND (j<i) then i:=j;
          // find cr as delimiter
          j:=Ansipos(#13, s2);
          if (j>0) AND (j<i) then i:=j;
          if (i>0) then delete(s2,i,MaxInt);
          s2:=ExtractFileDir(s2);
          if FileExists(s2+DirectorySeparator+LINKFILE) then
          begin
            result:=s2;
            FoundLinkFile:=true;
          end;
        end;

        if (NOT FoundLinkFile) then
        begin
          s1:=' --libdir=';
          //s1:=' --libexecdir=';
          i:=Ansipos(s1, Output);
          if (i>0) then
          begin
            s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
            // find space as delimiter
            i:=Ansipos(' ', s2);
            // find lf as delimiter
            j:=Ansipos(#10, s2);
            if (j>0) AND (j<i) then i:=j;
            // find cr as delimiter
            j:=Ansipos(#13, s2);
            if (j>0) AND (j<i) then i:=j;
            if (i>0) then delete(s2,i,MaxInt);
            result:=IncludeTrailingPathDelimiter(s2);
          end;

          i:=Ansipos('gcc', result);
          if i=0 then result:=result+'gcc'+DirectorySeparator;

          s1:=' --build=';
          i:=Ansipos(s1, Output);
          if i=0 then
          begin
            s1:=' --target=';
            i:=Ansipos(s1, Output);
          end;
          if (i>0) then
          begin
            s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
            // find space as delimiter
            i:=Ansipos(' ', s2);
            // find lf as delimiter
            j:=Ansipos(#10, s2);
            if (j>0) AND (j<i) then i:=j;
            // find cr as delimiter
            j:=Ansipos(#13, s2);
            if (j>0) AND (j<i) then i:=j;
            if (i>0) then delete(s2,i,MaxInt);
            result:=result+s2+DirectorySeparator;
          end;

          s1:='gcc version ';
          i:=Ansipos(s1, Output);
          if (i>0) then
          begin
            s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
            // find space as delimiter
            i:=Ansipos(' ', s2);
            // find lf as delimiter
            j:=Ansipos(#10, s2);
            if (j>0) AND (j<i) then i:=j;
            // find cr as delimiter
            j:=Ansipos(#13, s2);
            if (j>0) AND (j<i) then i:=j;
            if (i>0) then delete(s2,i,MaxInt);
            result:=result+s2;
            if FileExists(result+DirectorySeparator+LINKFILE) then
            begin
              FoundLinkFile:=true;
            end;
          end;
        end;
      end;
    end;

  except
    // ignore errors
  end;

  //In case of errors or failures, do a brute force search of gcc link file
  if (NOT FoundLinkFile) then
  begin
    {$IF (defined(BSD)) and (not defined(Darwin))}
    result:='/usr/local/lib/gcc';
    {$else}
    result:='/usr/lib/gcc';
    {$endif}

    if DirectoryExists(result) then
    begin
      LinkFiles := TStringList.Create;
      try
        FindAllFiles(LinkFiles, result, '*.o', true);
        if (LinkFiles.Count>0) then
        begin
          for i:=0 to (LinkFiles.Count-1) do
          begin
            if Pos(DirectorySeparator+LINKFILE,LinkFiles[i])>0 then
            begin
              result:=ExtractFileDir(LinkFiles[i]);
              FoundLinkFile:=true;
              break;
            end;
          end;
        end;
      finally
        LinkFiles.Free;
      end;
    end;
  end;

end;

{$IFDEF LINUX}
function GetFreePhysicalMemory: DWord;
var
  SystemInf: TSysInfo;
  mu:        cardinal;
begin
  FillChar({%H-}SystemInf,SizeOf(SystemInf),0);
  SysInfo(@SystemInf);
  mu := SystemInf.mem_unit;
  result := (QWord(SystemInf.freeram*mu) shr 20);
end;


function GetFreeSwapFileSize: DWord;
var
  SystemInf: TSysInfo;
  mu:        cardinal;
begin
  FillChar({%H-}SystemInf,SizeOf(SystemInf),0);
  SysInfo(@SystemInf);
  mu := SystemInf.mem_unit;
  result := (QWord(SystemInf.freeswap*mu) shr 20);
end;

function IsLinuxMUSL:boolean;
var
  Output:string;
begin
  result:=false;
  Output:='';
  RunCommand('getconf',['GNU_LIBC_VERSION'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
  begin
    //exit;
    if AnsiContainsText(Output,'glibc') then exit;
  end;
  Output:='';
  RunCommand('ldd',['--version'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
  begin
    if AnsiContainsText(Output,'musl') then result:=true;
  end;
end;
{$ENDIF LINUX}
{$ENDIF UNIX}

function GetLogicalCpuCount: integer;
var
  TotalMBMemory:DWord;
begin
  result:=1;

  {$if defined(win64) and defined(aarch64)}
  exit;
  {$endif}

  { Uses NumCPULib Library }
  { Copyright (c) 2019 Ugochukwu Mmaduekwe }
  { Github Repository https://github.com/Xor-el }
  result:=TNumCPULib.GetLogicalCPUCount();

  TotalMBMemory:=GetTotalPhysicalMemory+GetSwapFileSize;
  if TotalMBMemory=0 then
  begin
    // no info : be safe
    result:=1;
    exit;
  end;

  // limit the amount of spawn processes in case of limited memory
  if (TotalMBMemory<3000) then
  begin
    result:=(result DIV 2);
  end;
  // limit the amount to 1 process in case of very limited memory
  if (TotalMBMemory<2000) then
  begin
    result:=1;
  end;

  if (result<1) then result:=1;
  if (result>16) then result:=16;
end;

function GetTotalPhysicalMemory: DWord;
begin
  {$if defined(win64) and defined(aarch64)}
  exit(0);
  {$endif}
  result:=TNumCPULib.GetTotalPhysicalMemory();
end;

function GetSwapFileSize: DWord;
begin
  {$if defined(win64) and defined(aarch64)}
  exit(0);
  {$endif}
  result:=TNumCPULib.GetTotalSwapMemory();
end;

{$ifdef Darwin}
function GetDarwinSDKVersion(aSDK: string):string;
const
  SearchTarget='SDKVersion: ';
var
  Output,s:string;
  i,j:integer;
begin
  s:='';
  j:=0;

  if (Length(s)=0) then
  begin
    if (Length(aSDK)=0) OR (aSDK='macosx') then
    begin
      Output:='';
      RunCommand('xcrun',['--show-sdk-version'], Output);
      if (Length(Output)>0) then
      begin
        i:=1;
        while (i<=Length(Output)) AND (Output[i] in ['0'..'9','.']) do
        begin
          s:=s+Output[i];
          Inc(i);
        end;
      end;
    end;
  end;

  if (Length(s)=0) then
  begin
    Output:='';
    //if ExecuteCommandCompat('xcodebuild -version -sdk '+aSDK, Output, False) <> 0 then
    RunCommand('xcodebuild',['-version','-sdk',aSDK], Output);
    begin
      i:=Pos(SearchTarget,Output);
      if i>0 then
      begin
        i:=i+length(SearchTarget);
        while (i<=Length(Output)) AND (Output[i] in ['0'..'9','.']) do
        begin
          s:=s+Output[i];
          Inc(i);
        end;
      end
    end;
  end;

  if (Length(s)=0) then
  begin
    if aSDK='macosx' then
    begin
      Output:='';
      RunCommand('sw_vers',['-productVersion'], Output);
      if (Length(Output)>0) then
      begin
        i:=1;
        while (i<=Length(Output)) AND (Output[i] in ['0'..'9','.']) do
        begin
          s:=s+Output[i];
          Inc(i);
        end;
      end;
    end;
  end;

  result:=s;
end;
function GetDarwinSDKLocation:string;
const
  SDKCOMMAND = '--show-sdk-path';
var
  Output:string;
begin
  result:='';
  Output:=ConcatPaths([GetXCodeLocation,'Platforms','MacOSX.platform','Developer','SDKs','MacOSX.sdk']);
  if DirectoryExists(Output) then
    result:=Output
  else
  begin
    //Output:='';
    //RunCommand('xcrun',['-h'], Output);
    //if (Pos(SDKCOMMAND,Output)>0) then
    begin
      Output:='';
      RunCommand('xcrun',[SDKCOMMAND], Output);
      Output:=Trim(Output);
      if (Length(Output)>0) then
      begin
        if DirectoryExists(Output) then
          result:=Output
      end;
    end;
  end;
end;

function GetDarwinToolsLocation:string;
const
  BINARY = 'clang';
var
  Output:string;
begin
  //Output:=ConcatPaths([GetXCodeLocation,'Toolchains','XcodeDefault.xctoolchain','usr','bin']);
  //if DirectoryExists(Output) then
  //  result:=Output
  //else
  begin
    Output:='';
    RunCommand('xcrun',['-f',BINARY], Output);
    Output:=Trim(Output);
    if (Length(Output)>0) then
    begin
      Delete(Output,Length(Output)-Length(BINARY),MaxInt);
      result:=Output;
    end;
  end;
end;
function GetXCodeLocation:string;
var
  Output:string;
begin
  Output:='';
  RunCommand('xcode-select',['--print-path'], Output);
  Output:=Trim(Output);
  if (Length(Output)>0) then
    result:=Output;
end;
{$endif}

function GetAndroidSDKDir:string;
begin
  result:='';
  {$ifdef MSWindows}
  result:=ConcatPaths([SafeGetApplicationConfigPath,'Android','Sdk']);
  if (NOT DirectoryExists(result)) then
    result:=ConcatPaths([GetUserDir,'AppData','Local','Android','Sdk']);
  {$else}
  {$ifdef Darwin}
  result:=ConcatPaths([GetUserDir,'Library','Android','Sdk']);
  if (NOT DirectoryExists(result)) then
    result:=ConcatPaths([GetUserDir,'Library','Android','sdk']);
  if (NOT DirectoryExists(result)) then
    result:=ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('Library'),'Android','Sdk']);
  if (NOT DirectoryExists(result)) then
    result:=ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('Library'),'Android','sdk']);
  {$else}
  result:=ConcatPaths([{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}UnicodeString{$ENDIF}('usr'),'lib','android-sdk']);
  if (NOT DirectoryExists(result)) then
    result:=ConcatPaths([GetUserDir,'Android','Sdk']);
  if (NOT DirectoryExists(result)) then
    result:=ConcatPaths([GetUserDir,'Android','sdk']);
  {$endif}
  {$endif}
  if (NOT DirectoryExists(result)) then
    result:='';
end;

function GetAndroidNDKDir:string;
const
{$ifdef MSWINOWS}
  SEARCHFILE='ndk-build.cmd';
{$ELSE}
  SEARCHFILE='ndk-build';
{$ENDIF}
var
  aSDKDir,aNDKDir:string;
  FilesList:TStringList;
begin
  result:='';
  aSDKDir:=GetAndroidSDKDir;
  if DirectoryExists(aSDKDir) then
  begin
    aNDKDir:=ConcatPaths([aSDKDir,'ndk-bundle']);
    if (NOT DirectoryExists(aNDKDir)) then
      aNDKDir:=ConcatPaths([aSDKDir,'ndk']);
    FilesList:=TStringList.Create;
    try
      //FindAllFiles(FilesList,aSDKDir, SEARCHFILE, true);
      FindAllDirectories(FilesList,aNDKDir,False);
      if FilesList.Count>0 then
      begin
        FilesList.Sorted:=True;
        //Get the highest version = latest = best I guess ... ;-)
        //result:=ExtractFileDir(FilesList[FilesList.Count-1]);
        result:=FilesList[FilesList.Count-1];
      end;
    finally
      FilesList.Free;
    end;
  end;
end;

// 1on1 shameless copy from unit cutils from the fpc compiler;
function CompareVersionStrings(s1,s2: string): longint;
var
  start1, start2,
  i1, i2,
  num1,num2,
  res,
  err: longint;
begin
  i1:=1;
  i2:=1;
  repeat
    start1:=i1;
    start2:=i2;
    while (i1<=length(s1)) and
          (s1[i1] in ['0'..'9']) do
       inc(i1);
    while (i2<=length(s2)) and
          (s2[i2] in ['0'..'9']) do
       inc(i2);
    { one of the strings misses digits -> other is the largest version }
    if i1=start1 then
      if i2=start2 then
        exit(0)
      else
        exit(-1)
    else if i2=start2 then
      exit(1);
    { get version number part }
    val(copy(s1,start1,i1-start1),num1,err);
    val(copy(s2,start2,i2-start2),num2,err);
    { different -> done }
    res:=num1-num2;
    if res<>0 then
      exit(res);
    { if one of the two is at the end while the other isn't, add a '.0' }
    if (i1>length(s1)) and
       (i2<=length(s2)) then
      s1:=s1+'.0';
    if (i2>length(s2)) and
       (i1<=length(s1)) then
       s2:=s2+'.0';
    { compare non-numerical characters normally }
    while (i1<=length(s1)) and
          not(s1[i1] in ['0'..'9']) and
          (i2<=length(s2)) and
          not(s2[i2] in ['0'..'9']) do
      begin
        res:=ord(s1[i1])-ord(s2[i2]);
        if res<>0 then
          exit(res);
        inc(i1);
        inc(i2);
      end;
    { both should be digits again now, otherwise pick the one with the
      digits as the largest (it more likely means that the input was
      ill-formatted though) }
    if (i1<=length(s1)) and
       not(s1[i1] in ['0'..'9']) then
      exit(-1);
    if (i2<=length(s2)) and
       not(s2[i2] in ['0'..'9']) then
      exit(1);
  until false;
end;

function ExistWordInString(const aString:pchar; const aSearchString:string; const aSearchOptions: TStringSearchOptions): Boolean;
var
  Size : Integer;
  LocalSearchOptions: TStringSearchOptions;
begin
  Size:=StrLen(aString);
  LocalSearchOptions:=aSearchOptions;
  Include(LocalSearchOptions, soDown); // Needed, while we pass 0 as SelStart
  Result:=SearchBuf(aString, Size, 0, 0, aSearchString, LocalSearchOptions)<>nil;
end;

function UnCamel(const value:string):string;
var
  s:string;
  len,i,j:integer;
begin
  result:='';
  len:=Length(value);
  if (len=0) then exit;

  SetLength({%H-}s,256);
  i:=1;

  while (i<=len) do
  begin
    if (value[i] in ['A'..'Z']) then break;
    Inc(i);
  end;

  j:=1;

  while (i<=len) do
  begin

    while (i<=len) do
    begin
      if (value[i] in ['A'..'Z']) then s[j]:=value[i] else break;
      Inc(i);
      Inc(j);
    end;
    if ((j>2) AND (i<=len)) then
    begin
      s[j]:=s[j-1];
      s[j-1]:=' ';
      Inc(j);
    end;
    while (i<=len) do
    begin
      if (NOT (value[i] in ['A'..'Z'])) then s[j]:=value[i] else break;
      Inc(i);
      Inc(j);
    end;

  end;

  SetLength(s,j-1);
  result:=s;
end;

function GetEnumNameSimple(aTypeInfo:PTypeInfo;const aEnum:integer):string;
begin
  begin
    if (aTypeInfo=nil) or (aTypeInfo^.Kind<>tkEnumeration) then
      result := '' else
      result := GetEnumName(aTypeInfo,aEnum);
  end;
end;

function GetEnumValueSimple(aTypeInfo:PTypeInfo;const aEnum:string):integer;
begin
  begin
    if (aTypeInfo=nil) or (aTypeInfo^.Kind<>tkEnumeration) then
      result := -1 else
      result:=GetEnumValue(aTypeInfo,aEnum);
  end;
end;

function ContainsDigit(const s: string): Boolean;
const
  Digits: set of Char = ['0'..'9'];
var
  i: Integer;
begin
  if (Length(s)=0) then exit(false);
  result := true;
  for i := 1 to Length(s) do
    if s[i] in Digits then exit;
  result := false;
end;

function LibWhich(const aLibrary: string; out location: string): boolean;
{$ifdef Unix}
const
  UNIXSEARCHDIRS : array [0..5] of string = (
  '/lib',
  '/lib64',
  '/usr/lib',
  '/usr/lib64',
  '/usr/local/lib',
  '/usr/local/lib64'
  );
  {$ifdef Haiku}
  HAIKUSEARCHDIRS : array [0..3] of string = (
  '/boot/system/lib/x86',
  '/boot/system/non-packaged/lib/x86',
  '/boot/system/lib',
  '/boot/system/non-packaged/lib'
  );
  {$endif}
var
  OutputString: string;
  aFile:string;
  i:integer;
  sd:string;
  OutputLines:TStringList;
{$endif}
begin
  result:=false;

  //SysUtils.GetCurrentDir;

  {$ifdef Unix}
  if (NOT result) then
  begin
    sd:=SysUtils.GetEnvironmentVariable('LIBRARY_PATH');
    if (Length(sd)=0) then sd:=SysUtils.GetEnvironmentVariable('LD_LIBRARY_PATH');
    if ((Length(sd)>0) AND (DirectoryExists(sd))) then
    begin
      OutputString:=FileSearch(aLibrary,sd);
      result:=(Length(OutputString)>0);
      if result then
      begin
        location:=ExtractFileDir(OutputString);
        ThreadLog('Library searcher found '+aLibrary+' in path @ '+location,etDebug);
      end;
    end;
  end;

  if (NOT result) then
  begin
    {$ifdef Haiku}
    for sd in HAIKUSEARCHDIRS do
    {$else}
    for sd in UNIXSEARCHDIRS do
    {$endif}
    begin
      {$ifdef Haiku}
      {$ifndef CPUX86}
      if (RightStr(sd,4)='/x86') then continue;
      {$endif}
      {$endif}
      if DirectoryExists(sd) then
      begin
        if (NOT result) then
        begin
          aFile:=ConcatPaths([sd,aLibrary]);
          result:=FileExists(aFile);
        end;
        if (NOT result) then
        begin
          aFile:=ConcatPaths([sd,GetSourceCPUOS,aLibrary]);
          result:=FileExists(aFile);
        end;
        if (NOT result) then
        begin
          aFile:=ConcatPaths([sd,GetSourceCPUOS+'-gnu',aLibrary]);
          result:=FileExists(aFile);
        end;
        if (NOT result) then
        begin
          aFile:=ConcatPaths([sd,GetSourceCPUOS+'-gnueabi',aLibrary]);
          result:=FileExists(aFile);
        end;
        if (NOT result) then
        begin
          aFile:=ConcatPaths([sd,GetSourceCPUOS+'-gnueabihf',aLibrary]);
          result:=FileExists(aFile);
        end;
        if result then
        begin
          location:=ExtractFileDir(aFile);
          ThreadLog('Library searcher found '+aLibrary+' inside '+location+'.',etDebug);
          break;
        end;
      end;
    end;
  end;

  if (NOT result) then
  begin
    {$ifdef Haiku}
    for sd in HAIKUSEARCHDIRS do
    {$else}
    for sd in UNIXSEARCHDIRS do
    {$endif}
    begin
      OutputString:='';
      {$ifdef Haiku}
      {$ifndef CPUX86}
      if (RightStr(sd,4)='/x86') then continue;
      {$endif}
      {$endif}
      if DirectoryExists(sd) then
      begin
        if (NOT result) then
        begin
          //try to find a file
          //OutputString:=FileSearch(aLibrary,SysUtils.GetEnvironmentVariable('LIBRARY_PATH'));
          //OutputString:=FindFileInDirList(aLibrary,SysUtils.GetEnvironmentVariable('LIBRARY_PATH'));
          RunCommand('find',[sd,'-type','f','-name',aLibrary],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          result:=((Pos(aLibrary,OutputString)>0));
          if result then
          begin
            OutputLines:=TStringList.Create;
            try
              OutputLines.Text:=OutputString;
              i:=StringListContains(OutputLines,aLibrary);
              if (i<>-1) then
              begin
                aFile:=OutputLines[i];
                if FileExists(aFile) then
                  location:=ExtractFileDir(aFile)
                else
                  result:=false;
              end;
            finally
              OutputLines.Destroy;
            end;
          end;

        end;
        if (NOT result) then
        begin
          //try to find a symlink to a file
          RunCommand('find',[sd,'-type','l','-name',aLibrary],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          result:=((Pos(aLibrary,OutputString)>0));
          if result then
          begin
            OutputLines:=TStringList.Create;
            try
              OutputLines.Text:=OutputString;
              i:=StringListContains(OutputLines,aLibrary);
              if (i<>-1) then
              begin
                aFile:=OutputLines[i];
                if FileExists(aFile) then
                  location:=ExtractFileDir(aFile)
                else
                  result:=false;
              end;
            finally
              OutputLines.Destroy;
            end;
          end;
        end;
        if result then
        begin
          ThreadLog('Library searcher found '+aLibrary+' inside '+sd+'.',etDebug);
          break;
        end;
      end;
    end;
  end;

  {$ifndef Haiku}
  if (NOT result) then
  begin
    sd:=Which('ldconfig');
    if (Length(sd)=0) then sd:='/sbin/ldconfig';
    if FileExists(sd) then
    begin
      OutputString:='';
      RunCommand('sh',['-c','"'+sd+' -p | grep '+aLibrary+'"'],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
      result:=( (Pos(aLibrary,OutputString)>0) AND (Pos('not found',OutputString)=0));
      if result then
      begin
        OutputLines:=TStringList.Create;
        try
          OutputLines.Text:=OutputString;
          i:=StringListContains(OutputLines,aLibrary);
          if (i<>-1) then
          begin
            aFile:=Trim(OutputLines[i]);
            i:=Pos(' /',aFile);
            if (i>0) then
            begin
              Delete(aFile,1,i);
              if FileExists(aFile) then
                location:=ExtractFileDir(aFile)
              else
                result:=false;
            end;
          end;
        finally
          OutputLines.Destroy;
        end;
      end;
      if result then
      begin
        ThreadLog('Library '+aLibrary+' found by ldconfig.',etDebug);
      end;
    end;
  end;
  {$endif}
  {$endif}
end;

function LibWhich(const aLibrary: string): boolean;
var
  aDir:string;
begin
  result:=LibWhich(aLibrary,aDir);
end;

function Which(const Executable: string): string;
var
  ExeName,FoundExe:string;
  {$IFDEF UNIX}
  OutputString: string;
  {$IFDEF DARWIN}
  OutputLines: TStringList;
  i: integer;
  {$ENDIF}
  {$ENDIF}
begin
  result:='';

  ExeName:=Executable;

  {$ifdef Windows}
  if ExtractFileExt(ExeName)='' then ExeName:=ExeName+'.exe';
  {$endif}

  if FileExists(ExeName) then result:=ExeName else
  begin
    FoundExe := ExeSearch(ExeName, '');
    if (NOT FileExists(FoundExe)) then
      FoundExe:=ExeSearch(ExeName,SysUtils.GetEnvironmentVariable(PATHVARNAME));
    if FileExists(FoundExe) then
      result:=FoundExe
    else
      result:=FindDefaultExecutablePath(ExeName);
  end;

  {$IFNDEF FREEBSD}
  if (NOT FileIsExecutable(result)) then result:='';
  {$ENDIF}

  {$IFDEF UNIX}
  if (NOT FileExists(result)) then
  begin
    RunCommand('which',[ExeName],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    OutputString:=Trim(OutputString);
    if ((OutputString<>'') and FileExists(OutputString)) then result:=OutputString;
  end;
  {$ENDIF}

  {$IFDEF DARWIN}
  if (NOT FileExists(result)) then
  begin
    RunCommand('type',['-a',ExeName],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    OutputString:=Trim(OutputString);
    if (OutputString=ExeName+' not found') then OutputString:='';
    if (OutputString<>'') then
    begin
      OutputLines:=TStringList.Create;
      try
        OutputLines.Text:=OutputString;
        OutputString:=OutputLines[Pred(OutputLines.Count)];
        i:=Pos(' is ',OutputString);
        if (i>0) then
          Delete(OutputString,1,i+4)
        else
          OutputString:='';
      finally
        OutputLines.Destroy;
      end;
      if ((OutputString<>'') and FileExists(OutputString)) then result:=OutputString;
    end;
  end;
  {$ENDIF}

  (*
  {$IFDEF UNIX}
  // Note: we're using external which because
  // FindDefaultExecutablePath
  // or
  // ExeSearch(Executable);
  // doesn't check if the user has execute permission
  // on the found file.
  // however
  // ExeSearch(Executable) ... if fpAccess (Executable,X_OK)=0 then ..... see http://www.freepascal.org/docs-html/rtl/baseunix/fpaccess.html
  ExecuteCommandCompat('which '+Executable,OutputString,false);
  // Remove trailing LF(s) and other control codes:
  while (length(OutputString)>0) and (ord(OutputString[length(OutputString)])<$20) do
    delete(OutputString,length(OutputString),1);
  {$ELSE}
  OutputString:=FindDefaultExecutablePath(Executable);
  {$ENDIF UNIX}
  // We could have checked for ExecuteCommandHidden exitcode, but why not
  // do file existence check instead:
  if (OutputString<>'') and fileexists(OutputString) then
  begin
    result:=OutputString;
  end
  else
  begin
    result:=''; //command failed
  end;
  *)
end;

function IsExecutable(Executable: string):boolean;
var
  aPath:string;
  {$ifdef UNIX}
  Info : Stat;
  {$endif}
begin
  result:=false;
  aPath:=Executable;
  if NOT FileExists(aPath) then exit;
  {$ifdef Windows}
  //if ExtractFileExt(aPath)='' then aPath:=aPath+'.exe';
  {$endif}
  if ExtractFileExt(aPath)=GetExeExt then
  begin
    {$ifdef UNIX}
    //result:=(fpAccess(aPath,X_OK)=0);
    result:=(FpStat(aPath,info{%H-})<>-1) and FPS_ISREG(info.st_mode) and
            (BaseUnix.FpAccess(aPath,BaseUnix.X_OK)=0);
    {$else}
    result:=true;
    {$endif}
  end;
end;

function ForceDirectoriesSafe(Const Dir: RawByteString): Boolean;
var
  aDir:RawByteString;
begin
  result:=true;
  if (Length(Dir)=0) then exit;
  aDir:=ExcludeTrailingPathDelimiter(Dir);
  if (Length(aDir)=0) then exit;
  if (NOT DirectoryExists(aDir)) then
    result:=ForceDirectories(aDir);
  if result then result:=DirectoryExists(aDir);
end;

//Adapted from SysUtils; Unix/Linux only
Function XdgConfigHome: String;
{ Follows base-dir spec,
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
begin
  {$ifdef UNIX}
  Result:=SysUtils.GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result='') then
    Result:=IncludeTrailingPathDelimiter(GetUserDir)+'.config'+DirectorySeparator
  else
    Result:=IncludeTrailingPathDelimiter(Result);
  {$ELSE}
  Result:=IncludeTrailingPathDelimiter('.');
  {$ENDIF}
end;

function CheckExecutable(const Executable:string; const Parameters:array of String; ExpectOutput: string; Level: TEventType; beSilent:boolean): boolean;
var
  aResultCode: longint;
  ExeName,ExePath: string;
  Output: string;
begin
  Result:=false;

  ExeName := ExtractFileName(Executable);

  if FilenameIsAbsolute(Executable) then
    ExePath := Executable
  else
    ExePath := Which(Executable);

  if FileExists(ExePath) then
  begin
    try
      Output:='';
      RunCommandIndir('',Executable,Parameters, Output, aResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
      if (aResultCode>=0) then //Not all non-0 result codes are errors. There's no way to tell, really
      begin
        if (ExpectOutput <> '') then
        begin
          Result := AnsiContainsText(Output, ExpectOutput);
          if (NOT Result) then
          begin
            // This is not a warning/error message as sometimes we can use multiple different versions of executables
            if ((Level<>etCustom) AND (NOT beSilent)) then
            begin
              if (NOT FileExists(ExePath)) then
                ThreadLog(Executable + ' not found.',Level)
              else
                ThreadLog(Executable + ' is not a valid ' + ExeName + ' application. ' +
                ExeName + ' exists but shows no (' + ExpectOutput + ') in its output.',Level);
            end;
          end;
        end
        else
          Result := true; //not all non-0 result codes are errors. There's no way to tell, really
      end;
    except
      on E: Exception do
      begin
        // This is not a warning/error message as sometimes we can use multiple different versions of executables
        if ((Level<>etCustom) AND (NOT beSilent)) then ThreadLog(Executable + ' is not a valid ' + ExeName + ' application (' + 'Exception: ' + E.ClassName + '/' + E.Message + ')', Level);
      end;
    end;
  end;

  if ((Result) AND (NOT beSilent)) then
    ThreadLog('Found valid ' + ExeName + ' application.',etDebug);
end;

function CheckExecutable(Executable:string;Parameters:array of string;ExpectOutput: string; beSilent:boolean): boolean;
begin
  //result:=IsExecutable(Executable);
  //if result then
    result:=CheckExecutable(Executable, Parameters, ExpectOutput, etInfo, beSilent);
end;

function GetJavaBase(aJava:string): string;
var
  s:string;
  JavaFiles: TStringList;
begin
  {$ifdef Windows}
  result:='';

  s:=SysUtils.GetEnvironmentVariable('JAVA_HOME');
  if s<>'' then
  begin
    s:=IncludeTrailingPathDelimiter(s);
    JavaFiles := FindAllFiles(s, aJava+GetExeExt, true);
    try
      if JavaFiles.Count>0 then
      begin
        result:=JavaFiles[0];
      end;
    finally
      JavaFiles.Free;
    end;
  end;

  if result<>'' then exit;

  // When running a 32bit fpcupdeluxe the command below results in "C:\Program Files (x86)\"
  // When running a 64bit fpcupdeluxe the command below results in "C:\Program Files\"
  s:=GetWindowsSpecialDir(CSIDL_PROGRAM_FILES);

  //On Win32, first try to find the 64bit version of java in the standard 64bit program directory
  {$ifdef win32}
  if (IsWindows64) then
  begin
    s:=StringReplace(s,' (x86)','',[]);
  end;
  {$endif win32}
  s:=IncludeTrailingPathDelimiter(s)+'Java'+DirectorySeparator;
  JavaFiles := FindAllFiles(s, aJava+GetExeExt, true);
  try
    if JavaFiles.Count>0 then
    begin
      // Hack: get the latest java version ... ;-)
      result:=JavaFiles[JavaFiles.Count-1];
    end;
  finally
    JavaFiles.Free;
  end;

  if result<>'' then exit;

  {$ifdef win32}
  //On Win32, try to find the 32bit version of java in the standard 32bit program directory
  s:=GetWindowsSpecialDir(CSIDL_PROGRAM_FILES);
  s:=IncludeTrailingPathDelimiter(s)+'Java'+DirectorySeparator;
  JavaFiles := FindAllFiles(s, aJava+GetExeExt, true);
  try
    if JavaFiles.Count>0 then
    begin
      // Hack: get the latest java version ... ;-)
      result:=JavaFiles[JavaFiles.Count-1];
    end;
  finally
    JavaFiles.Free;
  end;
  {$endif win32}

  if result='' then result:=Which(aJava+GetExeExt);

  {$else Windows}
  result:=Which(aJava+GetExeExt);
  if result<>'' then
  begin
    while FileIsSymlink(result) do
    begin
      try
        result:=GetPhysicalFilename(result,pfeException);
      except
      end;
    end;
  end;

  if result<>'' then exit;

  {$ifdef Linux}
  JavaFiles := FindAllFiles('/usr/lib/jvm', aJava+GetExeExt, true);
  try
    if JavaFiles.Count>0 then
    begin
      result:=JavaFiles[0];
    end;
  finally
    JavaFiles.Free;
  end;
  if result<>'' then exit;
  {$endif}

  {$IF (defined(BSD)) and (not defined(Darwin))}
  JavaFiles := FindAllFiles('/usr/local', aJava+GetExeExt, true);
  try
    if JavaFiles.Count>0 then
    begin
      result:=JavaFiles[0];
    end;
  finally
    JavaFiles.Free;
  end;
  if result<>'' then exit;
  {$endif}

  {$endif Windows}
end;

function GetJava: string;
begin
  result:=GetJavaBase('java');
end;

function GetJavac: string;
begin
  result:=GetJavaBase('javac');
  //if length(result)=0 then result:=GetJavaBase('jexec');
end;

function CheckJava: boolean;
begin
  {$ifdef Windows}
  result:=CheckExecutable(GetJava, ['-version'], '');
  {$else}
  result:=CheckExecutable('java', ['-version'], '');
  {$endif}
end;

function ExtractFilePathSafe(const AFilename: string): string;
var
  i,j : longint;
  EndSep : Set of Char;
begin
  i:=Length(AFilename);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (i > 0) and not CharInSet(AFilename[i],EndSep) do
    Dec(i);
  j:=i+1;
  while (j<=Length(AFilename)) and (AFilename[j]<>' ') do
    Inc(j);
  result:=Copy(AFilename,1,j-1);
end;

function ExtractFileNameSafe(const AFilename: string): string;
begin
  result:=ExtractFileName(ExtractFilePathSafe(AFilename));
end;

function FileNameWithoutExt(const AFilename: string): string;
var
  s1,s2:string;
begin
  result:='';
  s1:=ExtractFileName(AFilename);
  s2:=ExtractFileExt(AFilename);
  result:=copy(s1,1,Length(s1)-Length(s2));
end;

function FileNameWithoutAllExt(const AFilename: string): string;
var
  StartPos: Integer;
  ExtPos,ExPosCounter: Integer;
begin
  result:='';
  StartPos:=length(AFilename);

  // Remove trailing separators
  if (AFilename[StartPos] in (AllowDirectorySeparators+AllowDriveSeparators)) then dec(StartPos);
  ExtPos:=StartPos;

  // Find first separator from the right
  while (StartPos>0)
  and not (AFilename[StartPos] in (AllowDirectorySeparators+AllowDriveSeparators))
  do
    dec(StartPos);

  Inc(StartPos);
  Inc(ExtPos);

  // We now have the filename
  result:=copy(AFilename,StartPos,ExtPos-StartPos);

  StartPos:=length(result);
  ExtPos:=StartPos;
  ExPosCounter:=0;

  //Remove at max 2 extension separators
  //Tricky to say the least
  repeat
    if result[StartPos]=ExtensionSeparator then
    begin
      ExtPos:=StartPos-1;
      Inc(ExPosCounter);
    end;
    Dec(StartPos);
  until (ExPosCounter=2) OR (StartPos=0);

  SetLength(result,ExtPos);
end;

function FileNameAllExt(const AFilename: string): string;
var
  s1,s2:string;
begin
  result:='';
  s1:=ExtractFileName(AFilename);
  s2:=FileNameWithoutAllExt(AFilename);
  result:=copy(s1,Length(s2)+1,MaxInt);
end;

function DoubleQuoteIfNeeded(s: string): string;
begin
  result:=Trim(s);
  if (Pos(' ',result)<>0) AND (Pos('"',result)=0) then result:='"'+result+'"';
end;

function UppercaseFirstChar(s: String): String;
var
  ch, rest: String;
  //first: String;
  i: integer;
begin
  i:=1;
  //while (Length(s)>=i) AND (NOT (s[i] in ['a'..'z'])) do inc(i);
  ch    := Copy(s, i, 1);
  //first := Copy(s, 1, i-1);
  rest  := Copy(s, Length(ch)+i, MaxInt);
  result := {LowerCase(first) + }UpperCase(ch) + LowerCase(rest);
end;

function DirectoryIsEmpty(Directory: string): Boolean;
var
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
  i: Integer;
begin
  Result:=(NOT DirectoryExists(Directory));
  if Result=true then exit;
  SysUtils.FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, FileInfo);
  for i := 1 to 2 do
    if (FileInfo.Name = '.') or (FileInfo.Name = '..') then
      Result := SysUtils.FindNext(FileInfo) <> 0;
  SysUtils.FindClose(FileInfo);
end;

function GetSourceCPU:string;
begin
  result:=lowercase({$i %FPCTARGETCPU%});
end;

function GetSourceOS:string;
begin
  result:=lowercase({$i %FPCTARGETOS%});
end;

function GetSourceCPUOS:string;
begin
  result:=GetSourceCPU+'-'+GetSourceOS;
end;

function GetFPCBuildVersion:string;
begin
  result:=lowercase({$I %FPCVERSION%});// + ' on ' +GetSourceCPUOS;
end;

function GetDistro(const aID:string):string;
var
  {$if defined(Darwin) OR defined(MSWindows)}
  Major,Minor,Build,Patch: Integer;
  {$endif}
  i,j: Integer;
  AllOutput : TStringList;
  s,t:ansistring;
  success:boolean;
begin
  t:='unknown';
  success:=false;
  {$ifdef Unix}
    {$ifndef Darwin}
      s:='';
      if RunCommand('cat',['/etc/os-release'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
      begin
        if Pos('No such file or directory',s)=0 then
        begin
          AllOutput:=TStringList.Create;
          try
            AllOutput.Text:=s;
            s:='';
            if Length(aID)>0 then
            begin
              s:=AllOutput.Values[aID];
            end
            else
            begin
              s:=AllOutput.Values['NAME'];
              if Length(s)=0 then s := AllOutput.Values['ID_LIKE'];
              if Length(s)=0 then s := AllOutput.Values['DISTRIB_ID'];
              if Length(s)=0 then s := AllOutput.Values['ID'];
            end;
            success:=(Length(s)>0);
          finally
            AllOutput.Free;
          end;
        end;
      end;
      if (NOT success) then
      begin
        s:='';
        if RunCommand('cat',['/etc/system-release'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
        begin
          if Pos('No such file or directory',s)=0 then
          begin
            AllOutput:=TStringList.Create;
            try
              AllOutput.Text:=s;
              s:='';
              s:=AllOutput.Values['NAME'];
              if Length(s)=0 then s := AllOutput.Values['ID_LIKE'];
              if Length(s)=0 then s := AllOutput.Values['DISTRIB_ID'];
              if Length(s)=0 then s := AllOutput.Values['ID'];
              success:=(Length(s)>0);
            finally
              AllOutput.Free;
            end;
          end;
        end;
      end;
      if (NOT success) then
      begin
        if FileExists('/bin/lsb_release') then
        begin
          s:='';
          if RunCommand('lsb_release',['-a'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
          begin
            AllOutput:=TStringList.Create;
            AllOutput.NameValueSeparator:=':';
            try
              AllOutput.Text:=s;
              s:='';
              if aID='VERSION' then
                s:=Trim(AllOutput.Values['Release'])
              else
                s:=Trim(AllOutput.Values['Description']);
              success:=(Length(s)>0);
            finally
              AllOutput.Free;
            end;
          end;
        end;
      end;
      if (NOT success) then
      begin
        s:='';
        if RunCommand('hostnamectl',[],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
        begin
          AllOutput:=TStringList.Create;
          try
            AllOutput.NameValueSeparator:=':';
            AllOutput.Delimiter:=#10;
            AllOutput.StrictDelimiter:=true;
            AllOutput.DelimitedText:=s;
            s:='';
            for i:=0 to  AllOutput.Count-1 do
            begin
              j:=Pos('Operating System',AllOutput.Strings[i]);
              if j>0 then s:=s+Trim(AllOutput.Values[AllOutput.Names[i]]);
              j:=Pos('Kernel',AllOutput.Strings[i]);
              if j>0 then s:=s+' '+Trim(AllOutput.Values[AllOutput.Names[i]]);
            end;
            success:=(Length(s)>0);
          finally
            AllOutput.Free;
          end;
        end;
      end;
      if (NOT success) then t:='unknown' else
      begin
        s:=DelChars(s,'"');
        t:=Trim(s);
      end;
      {$ifdef BSD}
      if (t='unknown') then
      begin
        if RunCommand('uname',['-r'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF})
           then t := GetSourceOS+' '+lowercase(Trim(s));
      end;
      {$endif}

      if (t='unknown') then t := GetSourceOS;

      if (NOT success) then if RunCommand('uname',['-r'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF})
         then t := t+' '+lowercase(Trim(s));

    {$else Darwin}
      if RunCommand('sw_vers',['-productName'], s) then
      begin
        if Length(s)>0 then t:=Trim(s);
      end;
      if Length(s)=0 then t:=GetSourceOS;
      if RunCommand('sw_vers',['-productVersion'], s) then
      begin
        if Length(s)>0 then
        begin
          VersionFromString(s,Major,Minor,Build,Patch);
          t:=t+' '+InttoStr(Major)+'.'+InttoStr(Minor)+'.'+InttoStr(Build);
        end;
      end;
    {$endif Darwin}
  {$endif Unix}

  {$ifdef MSWindows}
    t:='Win';
    if IsWindows64
       then t:=t+'64'
       else t:=t+'32';
    if GetWin32Version(Major,Minor,Build)
       then t:=t+'-'+InttoStr(Major)+'.'+InttoStr(Minor)+'.'+InttoStr(Build);
  {$endif MSWindows}
  result:=t;
end;

function GetFreeBSDVersion:byte;
var
  s:string;
  i,j:integer;
begin
  result:=0;
  s:=GetDistro('VERSION');
  if Length(s)>0 then
  begin
    i:=1;
    while (Length(s)>=i) AND (NOT (s[i] in ['0'..'9'])) do Inc(i);
    j:=0;
    while (Length(s)>=i) AND (s[i] in ['0'..'9']) do
    begin
      j:=j*10+Ord(s[i])-$30;
      Inc(i);
    end;
    result:=j;
  end;
end;

function checkGithubRelease(const aURL:string):string;
var
  s,aFile      : string;
  Json         : TJSONData;
  JsonObject   : TJSONObject;
  Releases     : TJSONArray;
  NewVersion   : boolean;
  Fixes        : boolean;
  i            : integer;
  Ss           : TStringStream;
  Content      : string;
  Success      : boolean;
begin
  json:=nil;
  Success:=false;
  NewVersion:=false;
  Fixes:=false;
  result:='';

  // Check for a fixes version
  if DELUXEVERSION[Length(DELUXEVERSION)]='f' then
  begin
    if DELUXEVERSION[Length(DELUXEVERSION)-1] in ['a'..'z'] then
    begin
      Fixes:=true;
    end;
  end;

  if (Length(aURL)>0) then
  begin
    Ss := TStringStream.Create('');
    try
      Success:=Download(False,aURL,Ss);
      if (NOT Success) then
      begin
        {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
        Ss.Clear;
        {$ENDIF}
        Ss.Position:=0;
        Success:=Download(True,aURL,Ss);
      end;
      if Success then Content:=Ss.DataString;
    finally
      Ss.Free;
    end;

    if Success then
    begin
      if (Length(Content)>0) then
      begin
        try
          Json:=GetJSON(Content);
        except
          Json:=nil;
        end;
        if (JSON=nil) then exit;
        try
          JsonObject := TJSONObject(Json);
          // Example ---
          // tag_name: "1.6.2b"
          // name: "Release v1.6.2b of fpcupdeluxe"
          s:=JsonObject.Get('tag_name');
          if CalculateNumericalVersion(s)>CalculateNumericalVersion(DELUXEVERSION) then NewVersion:=True;
          if CalculateNumericalVersion(s)=CalculateNumericalVersion(DELUXEVERSION) then
          begin
            i:=Length(DELUXEVERSION);
            if Fixes then Dec(i);
            if Ord(s[Length(s)])>Ord(DELUXEVERSION[i]) then NewVersion:=True;
          end;
          if NewVersion then
          begin
            s:=JsonObject.Get('prerelease');//Should be False
            NewVersion:=(s='False');
          end;
          //YES !!!
          if NewVersion then
          begin
            //Assets is an array of binaries belonging to a release
            Releases:=JsonObject.Get('assets',TJSONArray(nil));
            for i:=0 to (Releases.Count-1) do
            begin
              JsonObject := TJSONObject(Releases[i]);
              // Example ---
              // browser_download_url: "https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/1.6.2b/fpcupdeluxe-aarch64-linux"
              // name: "fpcupdeluxe-aarch64-linux"
              // created_at: "2018-10-14T06:58:44Z"
              s:=JsonObject.Get('name');
              aFile:='fpcupdeluxe-'+GetSourceCPUOS;
              {$ifdef Darwin}
              {$ifdef LCLCARBON}
              aFile:=aFile+'-carbon';
              {$endif}
              {$ifdef LCLCOCOA}
              aFile:=aFile+'-cocoa';
              {$endif}
              {$endif}
              {$if defined(LCLQT) or defined(LCLQT5)}
              aFile:=aFile+'-qt5';
              {$endif}
              if (Pos(aFile,s)=1) then
              begin
                result:=JsonObject.Get('browser_download_url');
                break;
              end;
            end;
          end;
        finally
          Json.Free;
        end;
      end;
    end;
  end;
end;

{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30200)}
Function Pos(Const Substr : string; Const Source : string; Offset : Sizeint = 1) : SizeInt;
var
  i,MaxLen : SizeInt;
  pc : PAnsiChar;
begin
  Pos:=0;
  if (Length(SubStr)>0) and (Offset>0) and (Offset<=Length(Source)) then
   begin
     MaxLen:=Length(source)-Length(SubStr);
     i:=Offset-1;
     pc:=@source[Offset];
     while (i<=MaxLen) do
      begin
        inc(i);
        if (SubStr[1]=pc^) and
           (CompareByte(Substr[1],pc^,Length(SubStr))=0) then
         begin
           Pos:=i;
           exit;
         end;
        inc(pc);
      end;
   end;
end;
{$ENDIF}

{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION < 30000)}
Function CharInSet(Ch:AnsiChar;Const CSet : TSysCharSet) : Boolean;
begin
  result:=ch in CSet;
end;
{$ENDIF}

Function GetUpTickCount:QWORD;
begin
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  result:=SysUtils.GetTickCount64;
  {$ELSE}
  result:=QWORD(GetTickCount);
  {$ENDIF}
end;

{$ifdef ENABLEEMAIL}
function DoRead(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;
begin
  if Data=nil then result:=0 else
  begin
    result:=TStream(Data).Read(Ptr^,Size*nmemb);
  end;
end;

function SendMail (Host, Subject, pTo, From, login,password: string; Body: TStrings):boolean;
var
  aURI          : URIPARSER.TURI;
  i             : integer;
  clearpassword : string;
  s             : string;
  Cipher        : TDCP_DES;
  {$ifdef Haiku}
  hCurl         : pCurl;
  res           : CURLcode;
  recipients    : pointer;
  aDataStream   : TMemoryStream;
  {$else}
  Msg           : TMimeMess; // message
  MIMEPart      : TMimePart; // parts of the message
 {$endif}
begin
  result:=false;

  clearpassword:=password;
  Cipher := TDCP_DES.Create(nil);
  try
    {$ifdef SECRETDELUXEKEY}
    Cipher.InitStr(VERYSECRETDELUXEKEY,TDCP_sha256);
    {$else}
    Cipher.InitStr(DELUXEKEY,TDCP_sha256);
    {$endif}
    clearpassword:=Cipher.DecryptString(password);
  finally
    Cipher.Burn;
    Cipher.Free;
  end;

  {$ifdef Haiku}
  if LoadCurlLibrary then
  begin
    curl_global_init(CURL_GLOBAL_ALL);
    try
      aDataStream := TMemoryStream.Create;
      Body.SaveToStream(aDataStream);
      try
       hCurl:=curl_easy_init();
       if Assigned(hCurl) then
       begin
        res:=CURLE_OK;
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_URL, 'smtp://smtp.gmail.com:587');
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_FTP_SSL , CURLUSESSL_ALL);

        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_USERNAME, PChar(login));
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_PASSWORD, PChar(clearpassword));
        s:='<'+From+'>';
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_MAIL_FROM, PChar(s));

        recipients := nil;
        if (Length(pTo)>0) then
        begin
          s:='<'+pTo+'>';
          if res=CURLE_OK then recipients := curl_slist_append(nil,PChar(s));
          if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_MAIL_RCPT, recipients);
        end;

        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_READFUNCTION, @DoRead);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_READDATA, Pointer(aDataStream));
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_UPLOAD, 1);

        if res=CURLE_OK then res:=curl_easy_perform(hCurl);

        try
          if Assigned(recipients) then
          begin
            curl_slist_free_all(recipients);
            recipients:=nil;
          end;
        except
        end;
       end;

      finally
        aDataStream.Free;
        if Assigned(hCurl) then curl_easy_cleanup(hCurl);
      end;
    except
      // swallow libcurl exceptions
    end;
  end;
  //s:=Body.Text;
  //result:=SynCrtSock.SendEmail(Host, From, pTo, Subject, s, '', login, clearpassword, '465', '', true);
  {$else}
  {%H-}FillChar({%H-}aUri,SizeOf(TURI),0);
  aURI.Protocol:='mailto';
  aURI.Document:=pTo;
  s:='******************** first 20 lines *************************'+#13#10;
  i:=0;
  while (i<Body.Count) do
  begin
    s:=s+Body.Strings[i]+#13#10;
    if (i>18) then break;
    Inc(i);
  end;
  s:=s+#13#10;
  s:=s+'******************** last 50 lines *************************'+#13#10;
  i:=Body.Count-50;
  if (i<18) then i:=18;
  while (i<Body.Count) do
  begin
    s:=s+Body.Strings[i]+#13#10;
    Inc(i);
  end;
  s:=s+#13#10;
  s:=s+'************************* end ******************************';
  aURI.Params:='subject=Fpcupdeluxe command screen log&body=Please find included part of the command screen output of fpcupdeluxe. You may add more if you want by copy paste of command screen.'+#13#10+#13#10+s;
  //result:=OpenURL(EncodeURI(aURI));
  Msg := TMimeMess.Create;
  try
    Msg.Header.Subject := Subject;
    Msg.Header.From := From;
    Msg.Header.ToList.Add(pTo);
    MIMEPart := Msg.AddPartMultipart('alternative', nil);
    Msg.AddPartText(Body, MIMEPart);
    Msg.EncodeMessage;
    result:=smtpsend.SendToRaw(From,pTo,Host+':465',Msg.Lines,login,clearpassword);
  finally
    Msg.Free;
  end;
  {$endif}
end;
{$else}
function SendMail (Host, Subject, pTo, From, login,password: string; Body: TStrings):boolean;
begin
  result:=true;
end;
{$endif}

{TNormalUnzipper}

procedure TNormalUnzipper.DoOnFile(Sender : TObject; Const AFileName : String);
var
  ProcessInfo:boolean;
begin
  Inc(FFileCnt);
  FCurrentFile:=ExtractFileName(AFileName);
  ProcessInfo:=false;

  if FTotalFileCnt>50000 then
  begin
    ProcessInfo:=((FFileCnt MOD 5000)=0);
  end
  else
  if FTotalFileCnt>5000 then
  begin
    ProcessInfo:=((FFileCnt MOD 500)=0);
  end
  else
  if FTotalFileCnt>500 then
  begin
    ProcessInfo:=((FFileCnt MOD 50)=0);
  end
  else
  if FTotalFileCnt>50 then
  begin
    ProcessInfo:=((FFileCnt MOD 5)=0);
  end
  else
  begin
    ThreadLog('Extracted #'+InttoStr(FFileCnt)+'. File '+FCurrentFile+' out of #'+InttoStr(FTotalFileCnt));
    {$ifdef LCL}
    Application.ProcessMessages;
    {$endif}
  end;

  if ProcessInfo then
  begin
    ThreadLog('Extracted #'+InttoStr(FFileCnt)+' files out of #'+InttoStr(FTotalFileCnt));
    {$ifdef LCL}
    Application.ProcessMessages;
    {$endif}
  end;

end;

procedure TNormalUnzipper.DoOnProgressEx(Sender : TObject; Const ATotPos, ATotSize: Int64);
begin
  if ATotPos=ATotSize then
  begin
    ThreadLog('Extracted #all. Ready extracting.');
    {$ifdef LCL}
    Application.ProcessMessages;
    {$endif}
    // Do this once ...
    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
    if Assigned(FUnZipper) then FUnZipper.OnProgressEx:=nil;
    {$ENDIF}
  end;
end;

function TNormalUnzipper.DoBUnZip2(const SourceFile, TargetFile: string):boolean;
var
  InFile:TFileStream;
  Decompressed:TDecompressBzip2Stream;
  OutFile:TDownloadStream;
  Buffer: Pointer;
  i: integer;
const buffersize=$2000;
begin
  result:=false; //fail by default

  ThreadLog('TNormalUnzipper: Unzipping (bunzip2) '+ExtractFileName(SourceFile));

  InFile:=TFileStream.Create(SourceFile, fmOpenRead);
  try
    try
      Decompressed:=TDecompressBzip2Stream.Create(InFile);
    except
      // So[5mething went wrong, e.g. invalid format
      // Now get out of function with result false
      exit;
    end;
    OutFile:=TDownloadStream.Create(TargetFile,fmCreate);
    try
      //We don't have seek on the TDecompressBzip2stream, so can't use
      //CopyFrom...
      //Decompressed.CopyFrom(InFile, InFile.Size);
      GetMem(Buffer,BufferSize);
      repeat
        i:=Decompressed.Read(buffer^,BufferSize);
        if i>0 then
          OutFile.WriteBuffer(buffer^,i);
      until i<BufferSize;
      result:=true;
    finally
      Decompressed.Free;
      OutFile.Free;
    end;
  finally
    InFile.Free;
  end;

  ThreadLog('TNormalUnzipper: Unzipping (bunzip2) '+ExtractFileName(SourceFile)+' ready.');
end;

function TNormalUnzipper.DoUnZip(const ASrcFile, ADstDir: String; Files:array of string):boolean;
var
  i:word;
  x:cardinal;
  s:string;
begin
  result:=false;
  FUnzipper := TUnzipper.Create;
  try
    FFileList := TStringList.Create;
    try
      try
        FUnZipper.Clear;
        FUnZipper.OnPercent:=10;
        { Flat option only available in FPC >= 3.1 }
        {$IF FPC_FULLVERSION > 30100}
        FUnZipper.Flat:=Flat;
        {$ENDIF}
        FUnZipper.FileName := ASrcFile;
        FUnZipper.OutputPath := ADstDir;
        FUnZipper.OnStartFile:= @DoOnFile;
        {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
        FUnZipper.OnProgressEx:= @DoOnProgressEx;
        {$ENDIF}
        FFileList.Clear;
        if Length(Files)>0 then
          for i := 0 to high(Files) do
            FFileList.Append(Files[i]);
        FFileCnt:=0;
        FTotalFileCnt:=0;

        FUnZipper.Examine;

        {$ifdef MSWINDOWS}
        // on windows, .files (hidden files) cannot be created !!??
        // still to check on non-windows
        if FFileList.Count=0 then
        begin
          for x:=0 to FUnZipper.Entries.Count-1 do
          begin
            { UTF8 features are only available in FPC >= 3.1 }
            {$IF FPC_FULLVERSION > 30100}
            if FUnZipper.UseUTF8
              then s:=FUnZipper.Entries.Entries[x].UTF8ArchiveFileName
              else
            {$endif}
              s:=FUnZipper.Entries.Entries[x].ArchiveFileName;

            if (Pos('/.',s)>0) OR (Pos('\.',s)>0) then continue;
            if (Length(s)>0) AND (s[1]='.') then continue;
            FFileList.Append(s);
          end;
        end;
        {$endif}

        if FFileList.Count=0
          then FTotalFileCnt:=FUnZipper.Entries.Count
          else FTotalFileCnt:=FFileList.Count;

        try
          if FFileList.Count=0
            then FUnZipper.UnZipAllFiles
            else FUnZipper.UnZipFiles(FFileList);
        except
          on E:EFCreateError do
          begin
            ThreadLog('TNormalUnzipper: Could not create file.',etError);
          end
          else
          begin
            ThreadLog('TNormalUnzipper: Unknown exception error.',etError);
          end;
        end;
        { Flat option only available in FPC >= 3.1 }
        {$IF FPC_FULLVERSION < 30100}
        if Flat then
        begin
          if FFileList.Count=0 then
          begin
            for x:=0 to FUnZipper.Entries.Count-1 do
            begin

              if FUnZipper.Entries.Entries[x].IsDirectory then continue;
              if FUnZipper.Entries.Entries[x].IsLink then continue;

              { UTF8 features are only available in FPC >= 3.1 }
              {$IF FPC_FULLVERSION > 30100}
              if FUnZipper.UseUTF8
                 then s:=FUnZipper.Entries.Entries[x].UTF8ArchiveFileName
                 else
              {$endif}
              s:=FUnZipper.Entries.Entries[x].ArchiveFileName;

              if (Pos('/.',s)>0) OR (Pos('\.',s)>0) then continue;
              if (Length(s)>0) AND (s[1]='.') then continue;

              FFileList.Append(s);
            end;
          end;

          for x:=0 to FFileList.Count-1 do
          begin
            s:=FFileList.Strings[x];
            if DirectorySeparator<>'/' then s:=StringReplace(s, '/', DirectorySeparator, [rfReplaceAll]);
            MoveFile(IncludeTrailingPathDelimiter(ADstDir)+s, IncludeTrailingPathDelimiter(ADstDir)+ExtractFileName(s));
          end;

          for x:=0 to FUnZipper.Entries.Count-1 do
          begin
            if FUnZipper.Entries.Entries[x].IsDirectory then
            begin
              { UTF8 features are only available in FPC >= 3.1 }
              {$IF FPC_FULLVERSION > 30100}
              if FUnZipper.UseUTF8
                 then s:=FUnZipper.Entries.Entries[x].UTF8ArchiveFileName
                 else
              {$endif}
              s:=FUnZipper.Entries.Entries[x].ArchiveFileName;
              if DirectorySeparator<>'/' then s:=StringReplace(s, '/', DirectorySeparator, [rfReplaceAll]);
              if (s='.') or (s=DirectorySeparator+'.') or (Pos('..',s)>0) then continue;
              DeleteDirectoryEx(IncludeTrailingPathDelimiter(ADstDir)+s);
            end;
          end;
        end;
        {$ENDIF}

        result:=true;

      except
        on E:EZipError do
        begin
          ThreadLog('TNormalUnzipper: Could not unzip file.',etError);
        end
        else
        begin
          ThreadLog('TNormalUnzipper: Unknown exception error.',etError);
        end;
      end;
    finally
      FFileList.Free;
    end;

  finally
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
    FLog.Active:=false;//save writelog
    FLog.FileName:=AValue;
  end;
end;

procedure TLogger.WriteLog(Message: string);
begin
  FLog.Info(Message);
end;

procedure TLogger.WriteLog(EventType: TEventType;Message: string);
begin
  FLog.Log(EventType, Message);
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
  FLog.Active:=false;//save writelog
  FLog.Free;
  inherited Destroy;
end;

constructor TBasicDownLoader.Create;
begin
  Inherited Create;
  FMaxRetries:=MAXCONNECTIONRETRIES;
  FVerbose:=False;
  FUserAgent:='';
  FContentType:='';
  FAccept:='';
  FUsername:='';
  FPassword:='';
  FFilenameOnly:='';
  FHTTPProxyHost:='';
  FHTTPProxyPort:=0;
  FHTTPProxyUser:='';
  FHTTPProxyPassword:='';
end;

destructor TBasicDownLoader.Destroy;
begin
  inherited Destroy;
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
begin
  FTPHTMLListingParser(F,filelist);
end;

procedure TBasicDownLoader.DoOnWriteStream(Sender: TObject; APos: Int64);
//From the mORMot !!
function KB(bytes: Int64): string;
const
  _B: array[0..5] of string[3] = ('KB','MB','GB','TB','PB','EB');
var
  hi,rem,b: cardinal;
begin
  if bytes<1 shl 10-(1 shl 10) div 10 then begin
    result:=Format('%d Byte',[integer(bytes)]);
    exit;
  end;
  if bytes<1 shl 20-(1 shl 20) div 10 then begin
    b := 0;
    rem := bytes;
    hi := bytes shr 10;
  end else
  if bytes<1 shl 30-(1 shl 30) div 10 then begin
    b := 1;
    rem := bytes shr 10;
    hi := bytes shr 20;
  end else
  if bytes<Int64(1) shl 40-(Int64(1) shl 40) div 10 then begin
    b := 2;
    rem := bytes shr 20;
    hi := bytes shr 30;
  end else
  if bytes<Int64(1) shl 50-(Int64(1) shl 50) div 10 then begin
    b := 3;
    rem := bytes shr 30;
    hi := bytes shr 40;
  end else
  if bytes<Int64(1) shl 60-(Int64(1) shl 60) div 10 then begin
    b := 4;
    rem := bytes shr 40;
    hi := bytes shr 50;
  end else begin
    b := 5;
    rem := bytes shr 50;
    hi := bytes shr 60;
  end;
  rem := rem and 1023;
  if rem<>0 then
    rem := rem div 102;
  if rem=10 then begin
    rem := 0;
    inc(hi); // round up as expected by an human being
  end;
  if rem<>0 then
    result:=Format('%d.%d %s',[hi,rem,_B[b]]) else
    result:=Format('%d %s',[hi,_B[b]]);
end;
begin
  if (APos=0) then
  begin
    ThreadLog('Download progress '+FileNameOnly+': starting.');
  end
  else
  if (APos=-1) then
  begin
    ThreadLog('Download progress '+FileNameOnly+': download ready !');
  end
  else
  //Show progress only every 5 seconds
  {if (Sender is TDownloadStream) then}
  with (Sender as TDownloadStream) do
  begin
    if (GetUpTickCount>(FStoredTickCount+5000)) then
    begin
      if FStoredTickCount=0 then
        ThreadLog('Download progress '+FileNameOnly+': Starting download.')
      else
        ThreadLog('Download progress '+FileNameOnly+': '+KB(APos));
      FStoredTickCount:=GetUpTickCount;
    end;
  end;
  {$ifdef LCL}
  Application.ProcessMessages;
  {$endif}
end;

{$ifdef ENABLENATIVE}
constructor TUseNativeDownLoader.Create;
begin
  Inherited;

  FMaxRetries:=MAXCONNECTIONRETRIES;
  {$ifdef Darwin}
  // GitHub needs TLS 1.2 .... native FPC client does not support this (through OpenSSL)
  // So, use client by Phil, a Lazarus forum member
  // See: https://macpgmr.github.io/
  aFPHTTPClient:=TNSHTTPSendAndReceive.Create;
  with aFPHTTPClient do
  begin
    TimeOut:=10000;
    UserAgent:=NORMALUSERAGENT;
  end;
  {$else}
  aFPHTTPClient:=TFPHTTPClient.Create(Nil);
  with aFPHTTPClient do
  begin
    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
    AllowRedirect:=True;
    //ConnectTimeout:=10000;
    //RequestHeaders.Add('Connection: Close');
    // User-Agent needed for sourceforge and GitHub
    UserAgent:=NORMALUSERAGENT;
    OnPassword:=@DoPassword;
    if FVerbose then
    begin
      OnRedirect:=@ShowRedirect;
      OnDataReceived:=@DoProgress;
      OnHeaders:=@DoHeaders;
    end;
    {$ENDIF}
  end;
  {$endif}
end;

destructor TUseNativeDownLoader.Destroy;
begin
  FreeAndNil(aFPHTTPClient);
  inherited;
end;

procedure TUseNativeDownLoader.DoHeaders(Sender : TObject);
var
  I : Integer;
begin
  ThreadLog('Response headers received:');
  with (Sender as TFPHTTPClient) do
    for I:=0 to ResponseHeaders.Count-1 do
      ThreadLog(ResponseHeaders[i]);
end;

procedure TUseNativeDownLoader.DoProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  If (ContentLength=0) then
    ThreadLog('Reading headers : '+InttoStr(CurrentPos)+' Bytes.')
  else If (ContentLength=-1) then
    ThreadLog('Reading data (no length available) : '+InttoStr(CurrentPos)+' Bytes.')
  else
    ThreadLog('Reading data : '+InttoStr(CurrentPos)+' Bytes of '+InttoStr(ContentLength));

  {$ifdef LCL}
  Application.ProcessMessages;
  {$endif}
end;

procedure TUseNativeDownLoader.DoPassword(Sender: TObject; var RepeatRequest: Boolean);
Var
  H,UN,PW : String;
  P : Integer;
begin
  if FUsername <> '' then
  begin
    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
    TFPHTTPClient(Sender).UserName:=FUsername;
    TFPHTTPClient(Sender).Password:=FPassword;
    {$ENDIF}
  end
  else
  begin

    with TFPHTTPClient(Sender) do
    begin
      {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
      H:=GetHeader(ResponseHeaders,'WWW-Authenticate');
      {$ELSE}
      H:=GetHeader('WWW-Authenticate');
      {$ENDIF}
    end;
    P:=Pos('realm',LowerCase(H));
    if (P>0) then
    begin
      P:=Pos('"',H);
      Delete(H,1,P);
      P:=Pos('"',H);
      H:=Copy(H,1,Pos('"',H)-1);
    end;

    {$ifndef LCL}
    writeln('Authorization required !');
    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
    if Length(H)>1 then
    begin
      writeln('Remote site says: ',H);
      writeln('Enter username (empty quits): ');
      readln(UN);
      RepeatRequest:=(UN<>'');
      if RepeatRequest then
      begin
        writeln('Enter password: ');
        readln(PW);
        TFPHTTPClient(Sender).UserName:=UN;
        TFPHTTPClient(Sender).Password:=PW;
      end;
    end;
    {$ENDIF}
    {$endif LCL}
  end;
end;

procedure TUseNativeDownLoader.ShowRedirect(ASender: TObject; const ASrc: String;
  var ADest: String);
begin
  ThreadLog('Following redirect from '+ASrc+'  ==> '+ADest);
end;

procedure TUseNativeDownLoader.SetContentType(AValue:string);
const
  HEADERMAGIC='Content-Type';
var
  i:integer;
begin
  if AValue<>FContentType then
  begin
    FContentType:=AValue;
    if FContentType='' then
    begin
      i:=aFPHTTPClient.IndexOfHeader(HEADERMAGIC);
      if i<>-1 then aFPHTTPClient.RequestHeaders.Delete(i);
    end
    else aFPHTTPClient.AddHeader(HEADERMAGIC,FContentType);
  end;
end;

procedure TUseNativeDownLoader.SetUserAgent(AValue:string);
const
  HEADERMAGIC='User-Agent';
var
  i:integer;
begin
  if AValue<>FUserAgent then
  begin
    FUserAgent:=AValue;
    if FUserAgent='' then
    begin
      i:=aFPHTTPClient.IndexOfHeader(HEADERMAGIC);
      if i<>-1 then aFPHTTPClient.RequestHeaders.Delete(i);
    end
    else aFPHTTPClient.AddHeader(HEADERMAGIC,FUserAgent);
  end;
end;

procedure TUseNativeDownLoader.SetAccept(AValue:string);
const
  HEADERMAGIC='Accept';
var
  i:integer;
begin
  if AValue<>FAccept then
  begin
    FAccept:=AValue;
    if FAccept='' then
    begin
      i:=aFPHTTPClient.IndexOfHeader(HEADERMAGIC);
      if i<>-1 then aFPHTTPClient.RequestHeaders.Delete(i);
    end
    else aFPHTTPClient.AddHeader(HEADERMAGIC,FAccept);
  end;
end;

procedure TUseNativeDownLoader.SetVerbose(aValue:boolean);
begin
  inherited;
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  {$ifndef Darwin}
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
  {$endif}
  {$ENDIF}
end;

procedure TUseNativeDownLoader.setProxy(host:string;port:integer;user,pass:string);
begin
  Inherited;// setProxy(host,port,user,pass);
  with aFPHTTPClient do
  begin
    {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
    Proxy.Host:=FHTTPProxyHost;
    Proxy.Port:=FHTTPProxyPort;
    Proxy.UserName:=FHTTPProxyUser;
    Proxy.Password:=FHTTPProxyPassword;
    {$endif}
  end;
end;

function TUseNativeDownLoader.getFTPFileList(const URL:string; filelist:TStringList):boolean;
var
  i: Integer;
  s: string;
  URI : URIPARSER.TURI;
  P : String;
begin
  result:=false;
  URI:=ParseURI(URL);
  P:=URI.Protocol;
  if AnsiStartsText('ftp',P) then
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
        if URI.Host='downloads.freepascal.org' then
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

function TUseNativeDownLoader.FTPDownload(Const URL: String; aDataStream:TStream):boolean;
var
  URI : URIPARSER.TURI;
  aPort:integer;
  aFTPClient:TFTPSend;
  aFTPResult:integer;
begin
  result:=false;

  if aDataStream=nil then exit;

  URI:=ParseURI(URL);
  aPort:=URI.Port;
  if aPort=0 then aPort:=21;

  aFTPClient:=TFTPSend.Create;

  try
    with aFTPClient do
    begin
      DirectFile := False; // don't let ftpsend create a file itself
      TargetHost := URI.Host;
      TargetPort := InttoStr(aPort);
      if FUsername <> '' then
      begin
        Username := FUsername;
        Password := FPassword;
      end
      else
      begin
        if URI.Host='downloads.freepascal.org' then
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
    end;
    aDataStream.Position:=0;
    aDataStream.Size:=0;
    if aFTPClient.Login then
    begin
      if TMyFTPSend(aFTPClient).DataSocket then
      begin
        aFTPClient.FTPCommand('TYPE I');
        aFTPResult:=aFTPClient.FTPCommand('RETR ' + URI.Path+URI.Document);
        if ((aFTPResult div 100)=1) then
          result := aFTPClient.DataRead(aDataStream);
      end;
      aFTPClient.Logout;
    end;
  finally
    aFTPClient.Destroy;
  end;
end;

function TUseNativeDownLoader.HTTPDownload(Const URL : String; aDataStream: TStream):boolean;
var
  tries:byte;
  response: Integer;
begin
  result:=false;
  tries:=0;

  if aDataStream=nil then exit;

  with aFPHTTPClient do
  begin
      repeat
        try
          aDataStream.Position:=0;
          aDataStream.Size:=0;
          Get(URL,aDataStream);
          //HTTPMethod('GET',URL,aDataStream,[200,404]);
          response:=ResponseStatusCode;
          result:=(response=200);
          //result:=(response>=100) and (response<300);
          if (NOT result) then
          begin
            // Do no retry on errors that we cannot recover from
            if (response>=400) then break;
            Inc(tries);
          end
          else
          begin
            //For GitHub
            //See: https://developer.github.com/v3/#rate-limiting
            //GetHeader('X-RateLimit-Remaining');
          end;
        except
          tries:=(MaxRetries+1);
        end;
      until (result or (tries>MaxRetries));
  end;
end;

function TUseNativeDownLoader.getFile(const URL,aFilename:string):boolean;
var
  aFile:TDownloadStream;
begin
  result:=false;
  FFileNameOnly:=ExtractFileName(aFilename);
  aFile:=TDownloadStream.Create(aFilename,fmCreate);
  try
    result:=Download(URL,aFile);
  finally
    aFile.Destroy;
  end;
  if (NOT result) then SysUtils.DeleteFile(aFilename);
end;

function TUseNativeDownLoader.getStream(const URL:string; aDataStream:TStream):boolean;
begin
  result:=Download(URL,aDataStream);
end;

function TUseNativeDownLoader.checkURL(const URL:string):boolean;
const
  HTTPHEADER      = 'Connection';
  HTTPHEADERVALUE = 'Close';
var
  aURL,P   : String;
  response : Integer;
  URI      : URIPARSER.TURI;
begin
  result:=false;

  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;

  // Only check http[s]
  if AnsiStartsText('http',P) then
  begin
    with aFPHTTPClient do
    begin
      AddHeader(HTTPHEADER,HTTPHEADERVALUE);
      try
        HTTPMethod('HEAD', URL, Nil, []);
        response:=ResponseStatusCode;
        // 404 Not Found
        // The requested resource could not be found but may be available in the future. Subsequent requests by the client are permissible.
        result:=(response<>404);
      except
      end;
      // remove additional header
      if GetHeader(HTTPHEADER)=HTTPHEADERVALUE then
      begin
        response:=IndexOfHeader(HTTPHEADER);
        if (response<>-1) then RequestHeaders.Delete(response);
      end;
    end;
  end
  else
  begin
    result:=true;
  end;

end;

function TUseNativeDownLoader.Download(const URL: String; aDataStream:TStream):boolean;
Var
  URI    : URIPARSER.TURI;
  aURL,P : String;
begin
  result:=false;

  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;

  if (aDataStream is TDownloadStream) then
  begin
    (aDataStream as TDownloadStream).OnWriteStream:=@DoOnWriteStream;
  end;

  if AnsiStartsText('ftp',P) then result:=FTPDownload(URL,aDataStream);
  if AnsiStartsText('http',P) then result:=HTTPDownload(URL,aDataStream);
end;
{$endif}

{$IFDEF ENABLEWGET}
// proxy still to do !!

constructor TUseWGetDownloader.Create;
begin
  Inherited;

  FCURLOk:=False;
  {$ifdef ENABLECURL}
  FCURLOk:=LoadCurlLibrary;
  {$endif}

  {$ifndef USEONLYCURL}
  if (Length(WGETBinary)=0) OR (NOT FileExists(WGETBinary)) then
  begin
    WGETBinary:='wget';
  end;
  FWGETOk:=CheckExecutable(WGETBinary,['-V'], '', true);

  {$ifdef MSWINDOWS}
  {$ifdef CPU64}
  if (NOT FWGETOk) then
  begin
    WGETBinary:='wget64.exe';
    FWGETOk:=CheckExecutable(WGETBinary,['-V'], '', true);
  end;
  {$endif}
  if (NOT FWGETOk) then
  begin
    WGETBinary:='wget.exe';
    FWGETOk:=CheckExecutable(WGETBinary,['-V'], '', true);
  end;
  {$endif MSWINDOWS}
  {$endif USEONLYCURL}

  if (NOT FCURLOk) AND (NOT FWGETOk) then
  begin
    ThreadLog('Could not initialize either libcurl or wget.',etDebug);
  end;

  UserAgent:=CURLUSERAGENT;
end;

constructor TUseWGetDownloader.Create(aWGETBinary:string);
begin
  WGETBinary:=aWGETBinary;
  inherited Create;
end;

procedure TUseWGetDownloader.AddHeader(const aHeader,aValue:String);
begin
  {
  P := pointer(hdr);
  while P<>nil do begin
    GetNextLine(P,s);
    if s<>'' then // nil would reset the whole list
      fIn.Headers := curl.slist_append(fIn.Headers,pointer(s));
  end;
  }
end;

{$ifndef USEONLYCURL}
function TUseWGetDownloader.WGetDownload(Const URL : String; aDataStream : TStream):boolean;
var
  Buffer : Array[0..4096] of byte;
  Count : Integer;
begin
  result:=false;
  if (NOT FWGETOk) then exit;

  With TProcess.Create(nil) do
  try
    Executable:=WGETBinary;
    // It seems that gitlab and github do'nt like the ipv6+wget combo
    if ((Pos('github.com/',URL)>0) OR (Pos('gitlab.com/',URL)>0)) then Parameters.Add('-4');
    Parameters.Add('-q');
    Parameters.Add('--no-check-certificate');
    Parameters.Add('--user-agent="'+FUserAgent+'"');
    Parameters.Add('--tries='+InttoStr(MaxRetries));
    Parameters.Add('--output-document=-');
    Parameters.Add(URL);
    Options:=[poUsePipes,poNoConsole];
    Execute;
    while Running do
    begin
      Count:=Output.Read(Buffer,SizeOf(Buffer));
      if (Count>0) then aDataStream.WriteBuffer(Buffer,Count);
    end;
    result:=((ExitStatus=0) AND (aDataStream.Size>0));
  finally
    Free;
  end;
end;
function TUseWGetDownloader.WGetFTPFileList(const URL:string; filelist:TStringList):boolean;
const
  WGETFTPLISTFILE='.listing';
var
  aURL:string;
  s:string;
  i:integer;
  URI : URIPARSER.TURI;
  P : String;
  {$IF NOT DEFINED(MORPHOS) AND NOT DEFINED(AROS)}  // this is very bad coding ... ;-)
  aTFTPList:TFTPList;
  {$ENDIF}
begin
  result:=false;
  if (NOT FWGETOk) then exit;

  URI:=ParseURI(URL);
  P:=URI.Protocol;
  if AnsiStartsText('ftp',P) then
  begin
    aURL:=URL;
    if aURL[Length(aURL)]<>'/' then aURL:=aURL+'/';
    result:=RunCommand(WGETBinary,['-q','--no-remove-listing','--tries='+InttoStr(MaxRetries),'--spider',aURL],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    if result then
    begin
      {$IF NOT DEFINED(MORPHOS) AND NOT DEFINED(AROS)}  // this is very bad coding ... ;-)
      if FileExists(WGETFTPLISTFILE) then
      begin
        aTFTPList:=TFTPList.Create;
        try
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
      {$ENDIF}
    end;
  end;
end;
{$endif}

{$ifdef ENABLECURL}
function DoWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;
begin
  if Data=nil then result:=0 else
  begin
    //result:=TStream(Data).Write(PAnsiChar(Ptr)^,Size*nmemb);
    result:=TStream(Data).Write(Ptr^,Size*nmemb);
  end;
end;

function TUseWGetDownloader.LibCurlDownload(Const URL : String; aDataStream : TStream):boolean;
var
  URI : URIPARSER.TURI;
  hCurl : pCurl;
  res: CURLcode;
  UserPass:string;
  aBuffer:PChar;
  location:string;
  response:sizeint;
  curl_headers: pointer;

  //TestURL : Pchar = 'http://www.magsys.co.uk/download/software/openssl-1.0.2o-win32.zip';
  //TestURL : Pchar = 'https://www.google.com';
  //TestURL : Pchar = 'https://github-production-release-asset-2e65be.s3.amazonaws.com/74603442/93575400-14ed-11ea-8b5f-a730911ad37c?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIWNJYAX4CSVEH53A%2F20200430%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20200430T174421Z&X-Amz-Expires=300&X-Amz-Signature=d33b93cfe0a6e0ae22ec2809edee84c67807ef9ce21befe6247a8b46e190e2fd&X-Amz-SignedHeaders=host&actor_id=0&repo_id=74603442&response-content-disposition=attachment%3B%20filename%3DCrossLibsLinuxx64.zip&response-content-type=application%2Foctet-stream';

begin
  result:=false;

  if (NOT FCURLOk) then exit;

  URI:=ParseURI(URL);

  if LoadCurlLibrary then
  begin

    curl_global_init(CURL_GLOBAL_ALL);

    try
      try
       hCurl:=curl_easy_init();
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
          if URI.Host='downloads.freepascal.org' then
          begin
            UserPass:='anonymous:fpc@example.com';
          end;
        end;
        if Length(UserPass)>0 then if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_USERPWD, pointer(UserPass));

        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_TCP_KEEPALIVE,True);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_FOLLOWLOCATION,True);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_MAXREDIRS,10);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_NOPROGRESS,True);
        {$ifdef MSWINDOWS}
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_SSL_VERIFYPEER,False);
        {$else}
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_HTTP_VERSION,CURL_HTTP_VERSION_2TLS);
        {$endif}

        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_URL,PChar(URL));
        //if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_URL,TestURL);

        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEFUNCTION,@DoWrite);
        if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEDATA,Pointer(aDataStream));
        //if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEHEADER,Pointer(aDataStream));

        //if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_SSLVERSION,6);

        if (Length(FUserAgent)>0) then if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_USERAGENT,PChar(FUserAgent));

        curl_headers := nil;
        {
        if (Length(FContentType)>0) then
        begin
          if res=CURLE_OK then curl_headers := curl_slist_append(nil,PChar(FContentType));
          if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_HTTPHEADER,curl_headers);
        end;
        }

        if res=CURLE_OK then res:=curl_easy_perform(hCurl);
        if res=CURLE_OK then res:=curl_easy_getinfo(hCurl,CURLINFO_RESPONSE_CODE, @response);
        result:=((res=CURLE_OK) AND (response<>404) AND (aDataStream.Size>0));

        try
          if Assigned(curl_headers) then
          begin
            curl_slist_free_all(curl_headers);
            curl_headers:=nil;
          end;
        except
        end;
       end;

      finally
        if Assigned(hCurl) then curl_easy_cleanup(hCurl);
      end;
    except
      // swallow libcurl exceptions
    end;
  end;
end;
function TUseWGetDownloader.LibCurlFTPFileList(const URL:string; filelist:TStringList):boolean;
var
  hCurl : pCurl;
  res: CURLcode;
  URI : URIPARSER.TURI;
  s : String;
  aTFTPList:TFTPList;
  F:TMemoryStream;
  i:integer;
  response:sizeint;
  UserPass :string;
begin
  result:=false;
  if (NOT FCURLOk) then exit;

  URI:=ParseURI(URL);
  s:=URI.Protocol;
  if AnsiStartsText('ftp',s) then
  begin
    if LoadCurlLibrary then
    begin

      curl_global_init(CURL_GLOBAL_ALL);

      try
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
              if URI.Host='downloads.freepascal.org' then
              begin
                UserPass:='anonymous:fpc@example.com';
              end;
            end;
            if Length(UserPass)>0 then if res=CURLE_OK then res:=curl_easy_setopt(hCurl, CURLOPT_USERPWD, pointer(UserPass));

            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_URL,pointer(URL));
            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEFUNCTION,@DoWrite);
            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_WRITEDATA,Pointer(F));

            {$ifdef MSWINDOWS}
            if res=CURLE_OK then res:=curl_easy_setopt(hCurl,CURLOPT_SSL_VERIFYPEER,False);
            {$endif}

            if res=CURLE_OK then res:=curl_easy_perform(hCurl);
            if res=CURLE_OK then res:=curl_easy_getinfo(hCurl,CURLINFO_RESPONSE_CODE, @response);
            result:=((res=CURLE_OK) {AND (response=200)});

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
       finally
        if Assigned(hCurl) then curl_easy_cleanup(hCurl);
       end;

      except
        // swallow libcurl exceptions
      end;
    end;
  end;
end;
{$endif ENABLECURL}


function TUseWGetDownloader.FTPDownload(Const URL : String; aDataStream : TStream):boolean;
begin
  result:=false;
  {$ifdef ENABLECURL}
  result:=LibCurlDownload(URL,aDataStream);
  {$endif}
  {$ifndef USEONLYCURL}
  if (NOT result) then
  begin
    result:=WGetDownload(URL,aDataStream);
  end;
  {$endif}
end;

function TUseWGetDownloader.HTTPDownload(Const URL : String; aDataStream : TStream):boolean;
begin
  result:=false;
  {$ifdef ENABLECURL}
  result:=LibCurlDownload(URL,aDataStream);
  {$endif}
  {$ifndef USEONLYCURL}
  if (NOT result) then
  begin
    result:=WGetDownload(URL,aDataStream);
  end;
  {$endif}
end;

procedure TUseWGetDownloader.SetContentType(AValue:string);
begin
  if AValue<>FContentType then
  begin
    FContentType:=AValue;
  end;
end;

procedure TUseWGetDownloader.SetUserAgent(AValue:string);
begin
  if AValue<>FUserAgent then
  begin
    FUserAgent:=AValue;
  end;
end;

procedure TUseWGetDownloader.SetAccept(AValue:string);
begin
  if AValue<>FAccept then
  begin
    FAccept:=AValue;
  end;
end;

function TUseWGetDownloader.getFTPFileList(const URL:string; filelist:TStringList):boolean;
begin
  result:=false;
  {$ifdef ENABLECURL}
  result:=LibCurlFTPFileList(URL,filelist);
  {$endif}
  {$ifndef USEONLYCURL}
  if (NOT result) then
  begin
    result:=WGetFTPFileList(URL,filelist);
  end;
  {$endif}
end;

function TUseWGetDownloader.checkURL(const URL:string):boolean;
var
  Output:string;
  URI:URIPARSER.TURI;
  aURL,P:string;
begin
  result:=false;

  if (NOT FWGETOk) then
    exit;

  aURL:=CleanURL(URL);
  URI:=ParseURI(aURL);
  P:=URI.Protocol;

  // Only check http[s]
  if AnsiStartsText('http',P) then
  begin
    Output:='';
    // It seems that gitlab and github don't like the ipv6+wget combo, so add option "-4"
    result:=RunCommand(WGETBinary,['-4','--no-check-certificate','--user-agent="'+FUserAgent+'"','--tries='+InttoStr(MaxRetries),'--spider',aURL],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    if result then
    begin
      result:=(Pos('Remote file exists',Output)>0);
    end;
    if NOT result then
    begin
      // on github/gitlab?, we get a 403 forbidden for an existing file !!
      result:=((Pos('github',Output)>0) {OR (Pos('gitlab',Output)>0)}) AND (Pos('403 Forbidden',Output)>0);
      if (NOT result) then result:=(Pos('https://',Output)>0) AND (Pos('401 Unauthorized',Output)>0)
    end;
  end
  else
  begin
    result:=true;
  end;

end;

function TUseWGetDownloader.Download(const URL: String; aDataStream: TStream):boolean;
Var
  URI : URIPARSER.TURI;
  P : String;
begin
  result:=false;
  URI:=ParseURI(URL);
  P:=URI.Protocol;

  if (aDataStream is TDownloadStream) then
  begin
    (aDataStream as TDownloadStream).OnWriteStream:=@DoOnWriteStream;
  end;

  if AnsiStartsText('ftp',P) then result:=FTPDownload(URL,aDataStream);
  if AnsiStartsText('http',P) then result:=HTTPDownload(URL,aDataStream);
end;

function TUseWGetDownloader.getFile(const URL,aFilename:string):boolean;
var
  aFile:TDownloadStream;
begin
  result:=false;
  FFileNameOnly:=ExtractFileName(aFilename);
  try
    aFile:=TDownloadStream.Create(aFilename,fmCreate);
    try
      result:=getStream(URL,aFile);
    finally
      aFile.Destroy;
    end;
  except
    result:=False;
  end;
  if (NOT result) then SysUtils.DeleteFile(aFilename);
end;

function TUseWGetDownloader.getStream(const URL:string; aDataStream:TStream):boolean;
begin
  result:=false;
  if aDataStream=nil then exit;
  try
    aDataStream.Position:=0;
    aDataStream.Size:=0;
    result:=Download(URL,aDataStream);
  except
    result:=False;
  end;
end;
{$ENDIF ENABLEWGET}

{ TDownloadStream }

destructor TDownloadStream.Destroy;
begin
  if Assigned(FOnWriteStream) AND (Self.Size>0) then
    FOnWriteStream(Self, -1);
  FileFlush(Handle);
  inherited Destroy;
end;

procedure TDownloadStream.SetOnWriteStream(aValue:TOnWriteStream);
begin
  FOnWriteStream:=aValue;
  FStoredTickCount:=0;
  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, 0);
end;

function TDownloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result:= inherited Write(Buffer, Count);
  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, Self.Position);
end;

class function TDownloadStream.StreamCreate(const aFileName: string; aMode: cardinal):TStream;
begin
  result:=Create(aFileName,aMode);
end;

procedure FinaGitHubStore;
var
  aStore:TGitHubStore;
begin
  if Length(GitHubFileListCache)>0 then
  begin
    for aStore in GitHubFileListCache do
    begin
      if Assigned(aStore.FileList) then aStore.FileList.Free
    end;
  end;
  Finalize(GitHubFileListCache);

  Finalize(URLDataCache);
end;

finalization
  FinaGitHubStore;

end.

