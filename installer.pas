{ Installer unit for FPCUp
  Copyright (C) 2012 Reinier Olislagers

  Based on svncommand unit
  Copyright (C) 2007 Vincent Snijders vincents@freepascal.org,

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
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, updater;

type
  { TInstaller }
  TInstaller = class(TObject)
  private
    FBootstrapCompiler: string;
    FCompiler: string;
    FExecutableExtension: string;
    FLazarusPrimaryConfigPath: string;
    FMake: string;
    FMakePath: string;
    FUpdater: TUpdater;
    FUnzip: string; //Location of unzip executable
    function DownloadBinUtils: boolean;
    function DownloadBootstrapCompiler: boolean;
    function DownloadHTTP(URL, TargetFile: string): boolean;
    function DownloadSVN: boolean;
    function CheckAndGetNeededExecutables: boolean; //Checks for binutils, svn.exe and downloads if needed. Returns true if all prereqs are met.
    function GetFpcDirectory: string;
    function GetFPCUrl: string;
    function GetLazarusDirectory: string;
    function GetLazarusUrl: string;
    function GetMakePath: string;
    procedure SetFPCDirectory(Directory: string);
    procedure SetFPCUrl(AValue: string);
    procedure SetLazarusDirectory(Directory: string);
    procedure SetLazarusUrl(AValue: string);
    procedure SetMakePath(AValue: string);
  public
    property Compiler: string read FCompiler;
    //Full path to FPC compiler that is installed by this program
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler;
    //Compiler used to compile compiler sources
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property FPCURL: string read GetFPCUrl write SetFPCUrl; //SVN URL for FPC
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    property LazarusPrimaryConfigPath: string
      read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
    //The directory where the configuration for this Lazarus instance must be stored.
    property LazarusURL: string read GetLazarusUrl write SetLazarusUrl;
    //SVN URL for Lazarus
    property MakePath: string read GetMakePath write SetMakePath; //Directory of make executable and other binutils. If it doesn't exist, make and binutils will be downloaded
    constructor Create;
    destructor Destroy; override;
  end;

procedure debugln(Message: string);
//Uses writeln for now, and waits a bit afterwards so output is hopefully not garbled

implementation

uses
  httpsend, strutils
{$IFDEF WINDOWS}
  ,shlobj;
{$ENDIF WINDOWS}

procedure debugln(Message: string);
begin
  {$DEFINE DEBUG} //how can I switch this depending on build mode?
  {$IFDEF DEBUG}
  writeln('Debug: ' + Message);
  sleep(200); //allow output to be written
  {$ENDIF DEBUG}
end;

{ TInstaller }

function TInstaller.DownloadBinUtils: boolean;
begin
  ForceDirectories(MakePath);
  //todo
end;

function TInstaller.DownloadBootstrapCompiler: boolean;
begin
 //todo, somewhere in temp
end;

function TInstaller.DownloadHTTP(URL, TargetFile: string): boolean;
// Download file. If ncessary deal with SourceForge redirection, thanks to
// Ocye: http://lazarus.freepascal.org/index.php/topic,13425.msg70575.html#msg70575
const
  SourceForgeProjectPart='//sourceforge.net/projects/';
  SourceForgeFilesPart='/files/';
var
  Buffer: TMemoryStream;
  i,j:integer;
  HTTPSender: THTTPSend;
  SourceForgeProject: string;
begin
  result:=false;
  // Detect SourceForge download
  i:=Pos(SourceForgeProjectPart, URL);
  j:=Pos(SourceForgeFilesPart, URL);

  // Rewrite URL if needed for Sourceforge download redirection
  if (i>0) and (j>0) then
    begin
      SourceForgeProject:=Copy(URL,i+Length(SourceForgeProjectPart),j);
      debugln('project is *'+SourceForgeProject+'*');
      try
        HTTPSender:=THTTPSend.Create;
        while not Result do
         begin
           HTTPSender.HTTPMethod('GET', URL);
           case HTTPSender.Resultcode of
             301,302,307 : for i:=0 to HTTPSender.Headers.Count-1 do
                           if (Pos('Location: ',HTTPSender.Headers.Strings[i])>0) or
                              (Pos('location: ',HTTPSender.Headers.Strings[i])>0) then
                           begin
                             j:=Pos('use_mirror=',HTTPSender.Headers.Strings[i]);
                             if j>0 then
                               URL:='http://'+
                                 RightStr(HTTPSender.Headers.Strings[i],
                                 length(HTTPSender.Headers.Strings[i])-j-10)+
                                 '.dl.sourceforge.net/project/'+SourceForgeProject+'/'+'DiReCtory'+'FiLeNAMe'
                             else
                               URl:=StringReplace(HTTPSender.Headers.Strings[i],'Location: ','',[]);
                             HTTPSender.Clear;//httpsend
                             break;
                           end;
             100..200 : Result:=true; //No changes necessary
             500:raise Exception.Create('No internet connection available');//Internal Server Error ('+aURL+')');
             else raise Exception.Create('Download failed with error code '+inttostr(HTTPSender.ResultCode)+' ('+HTTPSender.ResultString+')');
           end;//case
         end;//while
        debugln('resulting url after sf redir: *'+URL+'*');
      finally
        HTTPSender.Free;
      end;
    end;

  try
    Buffer := TMemoryStream.Create;
    if not HttpGetBinary(URL, Buffer) then
      raise Exception.Create('Cannot load document from remote server');
    //Application.ProcessMessages;
    Buffer.Position := 0;
    if Buffer.Size=0 then raise Exception.Create('Downloaded document is empty.');
    Buffer.SaveToFile(TargetFile);
    result:=true;
  finally
    FreeAndNil(Buffer);
  end;
end;

function TInstaller.DownloadSVN: boolean;
begin
  // Download SVN in make path. Not required for making FPC/Lazarus, but useful when downloading FPC/Lazarus from... SVN ;)
  result:=DownloadHTTP('http://sourceforge.net/projects/win32svn/files/latest/download?source=files', MakePath);
  FUpdater.SVNExecutable:=MakePath+'svn'+FExecutableExtension;
end;

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  OperationSucceeded: boolean;
begin
  OperationSucceeded:=false;
  // Check for binutils directory, make and unzip executables.
  // Download if needed; will download unzip - needed for SVN download
  FUnzip:=FMakePath+'unzip'+FExecutableExtension;
  if (DirectoryExists(FMakePath)=false) or
  (FileExists(FMakePath+'make'+FExecutableExtension)=false) or
  (FileExists(FUnzip)=false) then
  begin
    debugln('Make path ' + FMakePath + ' doesn''t have binutils. Going to download');
    OperationSucceeded:=DownloadBinUtils;
  end;

  //Check for SVN, download if needed
  if (FileExists(FUpdater.SVNExecutable)=false) and (OperationSucceeded) then
  begin
    OperationSucceeded:=DownloadSVN;
  end;

  //Check for bootstrap compiler, download if needed
  if (FileExists(BootstrapCompiler)=false) and (OperationSucceeded) then
  begin
    OperationSucceeded:=DownloadBootstrapCompiler;
  end;

  result:= OperationSucceeded;
end;

function Tinstaller.GetFpcDirectory: string;
begin
  Result := FUpdater.FPCDirectory;
end;

function TInstaller.GetFPCUrl: string;
begin
  Result := FUpdater.FPCURL;
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
  result:=FMakePath;
end;

procedure Tinstaller.SetFPCDirectory(Directory: string);
begin
  FUpdater.FPCDirectory := Directory;
end;

procedure TInstaller.SetFPCUrl(AValue: string);
begin
  FUpdater.FPCURL := AValue;
end;

procedure Tinstaller.SetLazarusDirectory(Directory: string);
begin
  FUpdater.LazarusDirectory := Directory;
end;

procedure TInstaller.SetLazarusUrl(AValue: string);
begin
  FUpdater.LazarusURL := AValue;
end;

procedure TInstaller.SetMakePath(AValue: string);
begin
  // Make sure there's a trailing delimiter
  FMakePath:=IncludeTrailingPathDelimiter(AValue);
  FMake:=FMakePath+'make'+FExecutableExtension;
end;

function Tinstaller.Getfpc: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded:=CheckAndGetNeededExecutables; //MakePath sure we have the proper tools.
  if FUpdater.UpdateFPC = False then
  begin
    OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // MakePath clean using bootstrap compiler
    // Note no error on failure, might be recoverable
    Executable := FMakePath;
    Params := ' FPC=' + FBootstrapCompiler + ' --directory=' +
      FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' clean';
    debugln('Running make clean for fpc:');
    debugln(Executable + ' ' + Params);
    //todo: check for bootstrap fpc compiler
    // MakePath (compile)
    SysUtils.ExecuteProcess(Executable, params, []);
  end;

  if OperationSucceeded then
  begin
    // MakePath (clean & all) using bootstrap compiler
    Executable := FMakePath;
    Params := ' FPC=' + FBootstrapCompiler + ' --directory=' +
      FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' all';
    debugln('debug: running make for fpc:');
    debugln(Executable + ' ' + Params);
    //todo: check for bootstrap fpc compiler
    // MakePath (compile)
    if SysUtils.ExecuteProcess(Executable, params, []) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Install using newly compiled compiler
    // todo: check where to install
    Executable := FMakePath;
    Params := ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' +
      DirectorySeparator + 'ppc386' + ' --directory=' + FPCDirectory +
      ' PREFIX=' + FPCDIRECTORY + ' UPXPROG=echo COPYTREE=echo' + ' install';
    debugln('debug: running make install for fpc:');
    debugln(Executable + ' ' + Params);
    if SysUtils.ExecuteProcess(Executable, Params, []) <> 0 then
      OperationSucceeded := False;
  end;
  { //don't know if this is needed
  if OperationSucceeded then
  begin
    // Make crosscompiler for Windows X64
    Executable:=FMake;
    Params:=' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      '--directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' OS_TARGET=win64 CPU_TARGET=x86_64' + ' all'
    debugln('Running MakePath crosscompile:');
    debugln(Executable + ' ' + Params);
    if
    SysUtils.ExecuteProcess(Executable,Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  }
  if OperationSucceeded then
  begin
    // Install crosscompiler
    Executable := FMakePath;
    debugln('Running Make crossinstall:');
    debugln(Executable + ' ' + Params);
    Params := '--directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' +
      DirectorySeparator + 'ppc386' + ' UPXPROG=echo COPYTREE=echo' +
      ' OS_TARGET=win64 CPU_TARGET=x86_64' + ' crossinstall';
    if SysUtils.ExecuteProcess(Executable, Params, []) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Create fpc.cfg
    //todo: replace -o path with bin path for resulting compiler; we'll need it for compilation/make above, anyway
    //todo: only generate when it doesn't exist yet
    //todo: seems to generate 64 bit config!??!
    Executable := FPCDirectory + DirectorySeparator + 'bin' +
      DirectorySeparator + 'i386-win32' + DirectorySeparator + 'fpcmkcfg';
    Params := ' -d basepath="' + FPCDirectory + '"' + ' -o "' +
      FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'fpc.cfg"';
    debugln('Debug: Running fpcmkcfg: ');
    debugln(Executable + ' ' + Params);
    if SysUtils.ExecuteProcess(Executable, Params, []) <> 0 then
      OperationSucceeded := False;
  end;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded:=CheckAndGetNeededExecutables; //MakePath sure we have the proper tools.
  // Download Lazarus source:
  if OperationSucceeded = True then
    OperationSucceeded := FUpdater.UpdateLazarus;
  if OperationSucceeded then
    debugln('debug: lazarus ok')
  else
    debugln('debug: lazarus not ok');
  // MakePath (compile)
  // todo: remove hardcoded MakePath, ppc compiler
  if OperationSucceeded then
  begin
    // MakePath clean; failure here might be recoverable, so no fiddling with OperationSucceeded
    //todo: fix for linux
    // Note: you apparently can't pass FPC in the FPC= statement.
    Executable := FMakePath;
    Params := '--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386'+FExecutableExtension + ' clean';
    debugln('Lazarus: running make clean:');
    debugln(Executable + ' ' + Params);
    (SysUtils.ExecuteProcess(Executable,
      Params, []));
  end;
  if OperationSucceeded then
  begin
    // MakePath all
    Executable := FMakePath;
    Params := '--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386'+FExecutableExtension + ' all';
    debugln('Lazarus: running make all:');
    debugln(Executable + ' ' + Params);
    if (SysUtils.ExecuteProcess(Executable, Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Build data desktop, nice example of building with lazbuild
    Executable := LazarusDirectory + DirectorySeparator + 'lazbuild';
    Params :=
      '--pcp=' + FLazarusPrimaryConfigPath + ' ' + LazarusDirectory +
      DirectorySeparator + 'tools' + DirectorySeparator + 'lazdatadesktop' +
      DirectorySeparator + 'lazdatadesktop.lpr';
    debugln('Lazarus: compiling data desktop:');
    debugln(Executable + ' ' + Params);
    if (SysUtils.ExecuteProcess(Executable, Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  //todo: setup primary config path, dir etc.
  //todo: MakePath shortcut on desktop, maybe start menu
  Result := OperationSucceeded;
end;

constructor Tinstaller.Create;
var
  AppDataPath: array[0..MaxPathLen] of char; //Allocate memory
begin
  FBootstrapCompiler := '';
  FCompiler := '';
  {$IFDEF WINDOWS}
  FExecutableExtension:='.exe';
  {$ELSE}
  FExecutableExtension:='';
  {$ENDIF WINDOWS}
  FLazarusPrimaryConfigPath := '';
  FUpdater := TUpdater.Create;
  FUnzip:='';
  //Directory where Lazarus installation will end up
  //todo: create if it doesn't exist
  {$IFDEF Windows}
  AppDataPath := '';
  SHGetSpecialFolderPath(0, AppDataPath, CSIDL_LOCAL_APPDATA, False);
  LazarusPrimaryConfigPath := AppDataPath + DirectorySeparator + 'lazarusdev';
  {$ELSE}
  writeln('todo: fix Lazarus primary config path, somewhere in ~ I guess.');
  LazarusPrimaryConfigPath := '/tmp'; //error!???
  {$ENDIF}
  if DirectoryExists(LazarusPrimaryConfigPath) = False then
  begin
    CreateDir(LazarusPrimaryConfigPath);
  end;
  SetMakePath('');
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

