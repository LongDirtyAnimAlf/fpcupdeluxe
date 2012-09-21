{ Utility unit for FPCUp
Copyright (C) 2012 Reinier Olislagers, Ludo Brands

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

interface
uses
  Classes, SysUtils, eventlog;


type

  { TLogger }

  TLogger = class(TObject)
  private
    FLog: TEventLog; //Logging/debug output to file
    function GetLogFile: string;
    procedure SetLogFile(AValue: string);
  public
    // Write to log and optionally console with seriousness etInfo
    procedure WriteLog(Message: string; ToConsole: Boolean=true);
    // Write to log and optionally console with specified seriousness
    procedure WriteLog(EventType: TEventType;Message: string; ToConsole: Boolean);
    property LogFile: string read GetLogFile write SetLogFile ;
    constructor Create;
    destructor Destroy; override;
  end;

// Create shortcut on desktop to Target file
procedure CreateDesktopShortCut(Target, TargetArguments, ShortcutName: string) ;
// Create shell script in ~ directory that links to Target
procedure CreateHomeStartLink(Target, TargetArguments, ShortcutName: string);
{$IFDEF MSWINDOWS}
// Delete shortcut on desktop
procedure DeleteDesktopShortcut(ShortcutName: string);
{$ENDIF MSWINDOWS}
// Delete directory and children, even read-only. Equivalent to rm -rf <directory>
function DeleteDirectoryEx(DirectoryName: string): boolean;
// Download from HTTP (includes Sourceforge redirection support) or FTP
function Download(URL, TargetFile: string): boolean;
// File size; returns 0 if empty, non-existent or error.
function FileSizeUTF8(FileName: string): int64;
//check if there is at least one directory between Dir and root
function ParentDirectoryIsNotRoot(Dir:string):boolean;
{$IFDEF MSWINDOWS}
// Get path for Windows per user storage of application data. Useful for storing settings
function GetLocalAppDataPath: string;
{$ENDIF MSWINDOWS}
//Shows message on screen; if DEBUG defined, also shows debug messages
procedure infoln(Message: string; Level: TEventType);
// Moves file if it exists, overwriting destination file
function MoveFile(const SrcFilename, DestFilename: string): boolean;
{$IFDEF UNIX}
function XdgConfigHome: String;
{$ENDIF UNIX}
// Emulates/runs which to find executable in path. If not found, returns empty string
function Which(Executable: string): string;


implementation
uses
  httpsend {for downloading from http},
  ftpsend {for downloading from ftp},
  FileUtil, strutils
  {$IFDEF MSWINDOWS}
    //Mostly for shortcut code
    ,windows, shlobj {for special folders}, ActiveX, ComObj
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ,baseunix,processutils
  {$ENDIF UNIX}
  ;

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
  LinkName := InFolder + PathDelim + ShortcutName+'.lnk';

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
    XdgDesktopContent.Add('Encoding=UTF-8');
    XdgDesktopContent.Add('Type=Application');
    XdgDesktopContent.Add('Icon='+ExtractFilePath(Target)+'images/icons/lazarus.ico');
    XdgDesktopContent.Add('Exec='+Target+' '+TargetArguments);
    XdgDesktopContent.Add('Name='+ShortcutName);
    XdgDesktopContent.SaveToFile(XdgDesktopFile);
    // We're going to try and call xdg-desktop-icon
    OperationSucceeded:=(ExecuteCommand('xdg-desktop-icon install ' + XdgDesktopFile,false)=0);
    if OperationSucceeded=false then infoln('CreateDesktopShortcut: failed to create shortcut to '+Target,etWarning);
    try
      DeleteFile(XdgDesktopFile);
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
  ForceDirectoriesUTF8(ExtractFilePath(IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~'))+ShortcutName));
  ScriptText:=TStringList.Create;
  try
    // No quotes here, either, we're not in a shell, apparently...
    ScriptFile:=IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~'))+ShortcutName;
    SysUtils.DeleteFile(ScriptFile); //Get rid of any existing remnants
    ScriptText.Add('#!/bin/sh');
    ScriptText.Add('# shortcut generated by fcpup');
    ScriptText.Add(Target+' '+TargetArguments);
    ScriptText.SaveToFile(ScriptFile);
    FPChmod(ScriptFile, &700); //rwx------
  finally
    ScriptText.Free;
  end;
  {$ENDIF UNIX}
end;

function DownloadFTP(URL, TargetFile: string): boolean;
const
  FTPPort=21;
  FTPScheme='ftp://'; //URI scheme name for FTP URLs
var
  Host: string;
  Port: integer;
  Source: string;
  FoundPos: integer;
begin
  // Strip out scheme info:
  if LeftStr(URL, length(FTPScheme))=FTPScheme then URL:=Copy(URL, length(FTPScheme)+1, length(URL));

  // Crude parsing; could have used URI parsing code in FPC packages...
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
  if result=false then infoln('DownloadFTP: error downloading '+URL+'. Details: host: '+Host+'; port: '+Inttostr(Port)+'; remote path: '+Source+' to '+TargetFile, eterror);
end;

function SFDirectLinkURL(URL: string; Document: TMemoryStream): string;
{
Transform this part of the body:
<noscript>
<meta http-equiv="refresh" content="5; url=http://downloads.sourceforge.net/project/base64decoder/base64decoder/version%202.0/b64util.zip?r=&amp;ts=1329648745&amp;use_mirror=kent">
</noscript>
into a valid URL:
http://downloads.sourceforge.net/project/base64decoder/base64decoder/version%202.0/b64util.zip?r=&amp;ts=1329648745&amp;use_mirror=kent
}
const
  Refresh='<meta http-equiv="refresh"';
  URLMarker='url=';
var
  Counter: integer;
  HTMLBody: TStringList;
  RefreshStart: integer;
  URLStart: integer;
begin
  HTMLBody:=TStringList.Create;
  try
    HTMLBody.LoadFromStream(Document);
    for Counter:=0 to HTMLBody.Count-1 do
    begin
      // This line should be between noscript tags and give the direct download locations:
      RefreshStart:=Ansipos(Refresh, HTMLBody[Counter]);
      if RefreshStart>0 then
      begin
        URLStart:=AnsiPos(URLMarker, HTMLBody[Counter])+Length(URLMarker);
        if URLStart>RefreshStart then
        begin
          // Look for closing "
          URL:=Copy(HTMLBody[Counter],
            URLStart,
            PosEx('"',HTMLBody[Counter],URLStart+1)-URLStart);
          break;
        end;
      end;
    end;
  finally
    HTMLBody.Free;
  end;
  result:=URL;
end;

function SourceForgeURL(URL: string): string;
// Detects sourceforge download and tries to deal with
// redirection, and extracting direct download link.
// Thanks to
// Ocye: http://lazarus.freepascal.org/index.php/topic,13425.msg70575.html#msg70575
const
  SFProjectPart = '//sourceforge.net/projects/';
  SFFilesPart = '/files/';
  SFDownloadPart ='/download';
var
  HTTPSender: THTTPSend;
  i, j: integer;
  FoundCorrectURL: boolean;
  SFDirectory: string; //Sourceforge directory
  SFDirectoryBegin: integer;
  SFFileBegin: integer;
  SFFilename: string; //Sourceforge name of file
  SFProject: string;
  SFProjectBegin: integer;
begin
  // Detect SourceForge download; e.g. from URL
  //          1         2         3         4         5         6         7         8         9
  // 1234557890123456789012345578901234567890123455789012345678901234557890123456789012345578901234567890
  // http://sourceforge.net/projects/base64decoder/files/base64decoder/version%202.0/b64util.zip/download
  //                                 ^^^project^^^       ^^^directory............^^^ ^^^file^^^
  FoundCorrectURL:=true; //Assume not a SF download
  i:=Pos(SFProjectPart, URL);
  if i>0 then
  begin
    // Possibly found project; now extract project, directory and filename parts.
    SFProjectBegin:=i+Length(SFProjectPart);
    j := PosEx(SFFilesPart, URL, SFProjectBegin);
    if (j>0) then
    begin
      SFProject:=Copy(URL, SFProjectBegin, j-SFProjectBegin);
      SFDirectoryBegin:=PosEx(SFFilesPart, URL, SFProjectBegin)+Length(SFFilesPart);
      if SFDirectoryBegin>0 then
      begin
        // Find file
        // URL might have trailing arguments... so: search for first
        // /download coming up from the right, but it should be after
        // /files/
        i:=RPos(SFDownloadPart, URL);
        // Now look for previous / so we can make out the file
        // This might perhaps be the trailing / in /files/
        SFFileBegin:=RPosEx('/',URL,i-1)+1;

        if SFFileBegin>0 then
        begin
          SFFilename:=Copy(URL,SFFileBegin, i-SFFileBegin);
          //Include trailing /
          SFDirectory:=Copy(URL, SFDirectoryBegin, SFFileBegin-SFDirectoryBegin);
          FoundCorrectURL:=false;
        end;
      end;
    end;
  end;

  if not FoundCorrectURL then
  begin
    try
      // Rewrite URL if needed for Sourceforge download redirection
      // Detect direct link in HTML body and get URL from that
      HTTPSender := THTTPSend.Create;
      //Who knows, this might help:
      HTTPSender.UserAgent:='curl/7.21.0 (i686-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18';
      while not FoundCorrectURL do
      begin
        HTTPSender.HTTPMethod('GET', URL);
        case HTTPSender.Resultcode of
          301, 302, 307:
            begin
              for i := 0 to HTTPSender.Headers.Count - 1 do
                if (Pos('Location: ', HTTPSender.Headers.Strings[i]) > 0) or
                  (Pos('location: ', HTTPSender.Headers.Strings[i]) > 0) then
                begin
                  j := Pos('use_mirror=', HTTPSender.Headers.Strings[i]);
                  if j > 0 then
                    URL :=
                      'http://' + RightStr(HTTPSender.Headers.Strings[i],
                      length(HTTPSender.Headers.Strings[i]) - j - 10) +
                      '.dl.sourceforge.net/project/' +
                      SFProject + '/' + SFDirectory + SFFilename
                  else
                    URL:=StringReplace(
                      HTTPSender.Headers.Strings[i], 'Location: ', '', []);
                  HTTPSender.Clear;//httpsend
                  FoundCorrectURL:=true;
                  break; //out of rewriting loop
              end;
            end;
          100..200:
            begin
              //Could be a sourceforge timer/direct link page, but...
              if AnsiPos('Content-Type: text/html', HTTPSender.Headers.Text)>0 then
              begin
                // find out... it's at least not a binary
                URL:=SFDirectLinkURL(URL, HTTPSender.Document);
              end;
              FoundCorrectURL:=true; //We're done by now
            end;
          500: raise Exception.Create('No internet connection available');
            //Internal Server Error ('+aURL+')');
          else
            raise Exception.Create('Download failed with error code ' +
              IntToStr(HTTPSender.ResultCode) + ' (' + HTTPSender.ResultString + ')');
        end;//case
      end;//while
    finally
      HTTPSender.Free;
    end;
  end;
  result:=URL;
end;

function DownloadHTTP(URL, TargetFile: string): boolean;
// Download file; retry if necessary.
// Deals with SourceForge download links
// Could use Synapse HttpGetBinary, but that doesn't deal
// with result codes (i.e. it happily downloads a 404 error document)
const
  MaxRetries=3;
var
  HTTPGetResult: boolean;
  HTTPSender: THTTPSend;
  RetryAttempt: integer;
begin
  result:=false;
  RetryAttempt:=1;
  URL:=SourceForgeURL(URL); //Deal with sourceforge URLs
  HTTPSender:=THTTPSend.Create;
  try
    try
      // Try to get the file
      HTTPGetResult:=HTTPSender.HTTPMethod('GET', URL);
      while (HTTPGetResult=false) and (RetryAttempt<MaxRetries) do
      begin
        sleep(500*RetryAttempt);
        HTTPGetResult:=HTTPSender.HTTPMethod('GET', URL);
        RetryAttempt:=RetryAttempt+1;
      end;
      // If we have an answer from the server, check if the file
      // was sent to us.
      case HTTPSender.Resultcode of
        100..299:
          begin
            with TFileStream.Create(TargetFile,fmCreate or fmOpenWrite) do
            try
              Seek(0, soFromBeginning);
              CopyFrom(HTTPSender.Document, 0);
            finally
              Free;
            end;
            result:=true;
          end; //informational, success
        300..399: result:=false; //redirection. Not implemented, but could be.
        400..499: result:=false; //client error; 404 not found etc
        500..599: result:=false; //internal server error
        else result:=false; //unknown code
      end;
    except
      // We don't care for the reason for this error; the download failed.
      result:=false;
    end;
  finally
    HTTPSender.Free;
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
  LinkName := InFolder + PathDelim + ShortcutName+'.lnk';
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
        // Remove all files and directories in this directory:
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

function Download(URL, TargetFile: string): boolean;
begin
  result:=false;
  // Assume http if no ftp detected
  try
    infoln('Going to download '+TargetFile+' from URL: ' + URL, etinfo);
    if (Copy(URL, 1, Length('ftp://'))='ftp://') or
    (Copy(URL,1,Length('ftp.'))='ftp.') then
    begin
      result:=DownloadFTP(URL, TargetFile);
    end
    else
    begin
      result:=DownloadHTTP(URL, TargetFile);
    end;
  except
    on E: Exception do
    begin
      infoln('Download: error occurred downloading file '+TargetFile+' from URL: '+URL+
        '. Exception occurred: '+E.ClassName+'/'+E.Message+')', eterror);
    end;
    //Not writing message here; to be handled by calling code with more context.
    //if result:=false then infoln('Download: download of '+TargetFile+' from URL: '+URL+' failed.');
  end;
end;

// returns file size in bytes or 0 if not found.
function FileSizeUTF8(FileName: string) : Int64;
var
  sr : TSearchRec;
begin
{$ifdef unix}
  result:=filesize(FileName);
{$else}
  if FindFirstUTF8(FileName, faAnyFile, sr ) = 0 then
     result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
  else
     result := 0;
  FindCloseUTF8(sr);
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
var
  Seriousness: string;
begin
  case Level of
    etCustom: Seriousness:='Custom:';
    etDebug: Seriousness:='Debug:';
    etInfo: Seriousness:='Info:';
    etWarning: Seriousness:='WARNING:';
    etError: Seriousness:='ERROR:';
    else Seriousness:='UNKNOWN CATEGORY!!:'
  end;
  if (Level<>etDebug) then
    begin
      if AnsiPos(LineEnding, Message)>0 then writeln(''); //Write an empty line before multiline messagse
      writeln(Seriousness+' '+ Message); //we misuse this for info output
      sleep(200); //hopefully allow output to be written without interfering with other output
    end
  else
    begin
      {$IFDEF DEBUG}
      {DEBUG conditional symbol is defined using
      Project Options/Other/Custom Options using -dDEBUG}
      if AnsiPos(LineEnding, Message)>0 then writeln(''); //Write an empty line before multiline messagse
      writeln(Seriousness+' '+ Message); //we misuse this for info output
      sleep(200); //hopefully allow output to be written without interfering with other output
      {$ENDIF DEBUG}
    end;
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

function Which(Executable: string): string;
var
  Output: string;
begin
  {$IFDEF UNIX}
  // Note: we're using external which because
  // FindDefaultExecutablePath
  // doesn't check if the user has execute permission
  // on the found file.
  ExecuteCommand('which '+Executable,Output,false);
  //Remove trailing LF(s) and other control codes:
  while (length(output)>0) and (ord(output[length(output)])<$20) do
    delete(output,length(output),1);
  {$ELSE}
  Output:=FindDefaultExecutablePath(Executable);
  {$ENDIF UNIX}
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

{ TLogger }

function TLogger.GetLogFile: string;
begin
  result:=FLog.FileName
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
  if ToConsole then infoln(Message,etinfo);
end;

procedure TLogger.WriteLog(EventType: TEventType;Message: string; ToConsole: Boolean);
begin
  FLog.Log(EventType, Message);
  if ToConsole then infoln(Message,etinfo);
end;

constructor TLogger.Create;
begin
  FLog:=TEventLog.Create(nil);
  FLog.LogType:=ltFile;
end;

destructor TLogger.Destroy;
begin
  FLog.Active:=false;//save WriteLog text
  FLog.Free;
  inherited Destroy;
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
    Result:=IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~'))+'.config'+DirectorySeparator
  else
    Result:=IncludeTrailingPathDelimiter(Result);
end;
{$ENDIF UNIX}

end.

