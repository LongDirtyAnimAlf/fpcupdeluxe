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
    FBootstrapCompilerURL: string;
    FCompiler: string;
    FExecutableExtension: string;
    FLazarusPrimaryConfigPath: string;
    FMake: string;
    FMakePath: string;
    FSVNDirectory: string; //Unpack SVN files in this directory
    FUpdater: TUpdater;
    FUnzip: string; //Location of unzip executable
    function DownloadBinUtils: boolean;
    function DownloadBootstrapCompiler: boolean;
    function DownloadHTTP(URL, TargetFile: string): boolean;
    function DownloadSVN: boolean;
    function CheckAndGetNeededExecutables: boolean;
    //Checks for binutils, svn.exe and downloads if needed. Returns true if all prereqs are met.
    function GetFpcDirectory: string;
    function GetFPCUrl: string;
    function GetLazarusDirectory: string;
    function GetLazarusUrl: string;
    function GetMakePath: string;
    function Run(Executable, Params: string): longint;
    function RunOutput(Executable, Params: string; var Output: TStringList): longint;
    function RunOutput(Executable, Params: string; var Output: String): longint;
    procedure SetFPCDirectory(Directory: string);
    procedure SetFPCUrl(AValue: string);
    procedure SetLazarusDirectory(Directory: string);
    procedure SetLazarusUrl(AValue: string);
    procedure SetMakePath(AValue: string);
  public
    property Compiler: string read FCompiler;
    //Full path to FPC compiler that is installed by this program
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler;
    //Compiler used to compile compiler sources. If file doesn't exist, it will be created
    property BootstrapCompilerURL: string read FBootstrapCompilerURL
      write FBootstrapCompilerURL;
    //Optional; URL from which to download bootstrap FPC compiler if it doesn't exist yet.
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
    property MakePath: string read GetMakePath write SetMakePath;
    //Directory of make executable and other binutils. If it doesn't exist, make and binutils will be downloaded
    constructor Create;
    destructor Destroy; override;
  end;

procedure debugln(Message: string);
//Uses writeln for now, and waits a bit afterwards so output is hopefully not garbled

implementation

uses
  httpsend, strutils, process,FileUtil {Requires LCL}
{$IFDEF WINDOWS}
  , shlobj;

{$ENDIF WINDOWS}

procedure debugln(Message: string);
begin
  {DEBUG conditional symbol is defined using
  Project Options/Other/Custom Options using -dDEBUG
  }
  {$IFDEF DEBUG}
  writeln('Debug: ' + Message);
  sleep(200); //hopefully allow output to be written without interfering with other output
  {$ENDIF DEBUG}
end;

{ TInstaller }

function TInstaller.DownloadBinUtils: boolean;
const
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/trunk/install/binw32/';
  //Parent directory of files. Needs trailing backslash.
var
  CopyFiles: TStringList;
  Counter: integer;
begin
  ForceDirectories(MakePath);
  Result := False;
  CopyFiles := TStringList.Create;
  try
    CopyFiles.Add('GoRC.exe');
    CopyFiles.Add('ar.exe');
    CopyFiles.Add('as.exe');
    CopyFiles.Add('bin2obj.exe');
    CopyFiles.Add('cmp.exe');
    CopyFiles.Add('cp.exe');
    CopyFiles.Add('cpp.exe');
    CopyFiles.Add('cygiconv-2.dll');
    CopyFiles.Add('cygncurses-8.dll');
    CopyFiles.Add('cygwin1.dll');
    CopyFiles.Add('diff.exe');
    CopyFiles.Add('dlltool.exe');
    CopyFiles.Add('fp32.ico');
    CopyFiles.Add('gcc.exe');
    CopyFiles.Add('gdate.exe');
    //GDB.exe apparently can also be found here:
    //http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
    //for Windows x64:
    //http://svn.freepascal.org/svn/lazarus/binaries/x86_64-win64/gdb/bin/
    CopyFiles.Add('gdb.exe');
    CopyFiles.Add('gecho.exe');
    CopyFiles.Add('ginstall.exe');
    CopyFiles.Add('ginstall.exe.manifest');
    CopyFiles.Add('gmkdir.exe');
    CopyFiles.Add('grep.exe');
    CopyFiles.Add('ld.exe');
    CopyFiles.Add('libexpat-1.dll');
    CopyFiles.Add('make.exe');
    CopyFiles.Add('mv.exe');
    CopyFiles.Add('objdump.exe');
    CopyFiles.Add('patch.exe');
    CopyFiles.Add('patch.exe.manifest');
    CopyFiles.Add('pwd.exe');
    CopyFiles.Add('rm.exe');
    CopyFiles.Add('strip.exe');
    CopyFiles.Add('unzip.exe');
    CopyFiles.Add('upx.exe');
    CopyFiles.Add('windres.exe');
    CopyFiles.Add('windres.h');
    CopyFiles.Add('zip.exe');
    for Counter := 0 to CopyFiles.Count - 1 do
    begin
      debugln('Downloading: ' + CopyFiles[Counter] + ' into ' + MakePath);
      try
        DownloadHTTP(SourceUrl + CopyFiles[Counter], MakePath + CopyFiles[Counter]);
      except
        on E: Exception do
        begin
          Result := False;
          debugln('Error downloading: ' + E.Message);
          exit; //out of function.
        end;
      end;
    end;
  finally
    CopyFiles.Free;
  end;
end;

function TInstaller.DownloadBootstrapCompiler: boolean;
  // Should be done after we have unzip executable in FMakePath
var
  BootstrapZip: string;
  CompilerName: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  BootstrapZip := BootstrapCompiler + '.zip';
  OperationSucceeded := DownloadHTTP(FBootstrapCompilerURL, BootstrapZip);
  if OperationSucceeded then
  begin
    //Extract zip into makedir, overwriting without prompting
    Params := '-o "' + BootstrapZip + '" -d "' + MakePath + '"';
    if SysUtils.ExecuteProcess(FUnzip, Params, []) <> 0 then
      OperationSucceeded := False;
  end;
  // Rename to bootstrap.exe
    {$IFDEF Windows}
  Compilername := 'ppc386.exe';
    {$ENDIF Windows}
    {$IFDEF Linux}
  //check if this is the right one - 32vs64 bit!?!?
  Compilername := 'ppc386';
    {$ENDIF Linux}
    {$IFDEF Darwin}
  //check if this is the right one - 32vs64 bit!?!?
  Compilername := 'ppc386';
    {$ENDIF Darwin}
  if OperationSucceeded = True then
    renamefile(MakePath + CompilerName, MakePath + 'bootstrap' + FExecutableExtension);
  if OperationSucceeded = True then
    SysUtils.DeleteFile(BootstrapZip);
  //todo chmod ug+x for Linux/OSX?!!
  Result := OperationSucceeded;
end;

function TInstaller.DownloadHTTP(URL, TargetFile: string): boolean;
  // Download file. If ncessary deal with SourceForge redirection, thanks to
  // Ocye: http://lazarus.freepascal.org/index.php/topic,13425.msg70575.html#msg70575
const
  SourceForgeProjectPart = '//sourceforge.net/projects/';
  SourceForgeFilesPart = '/files/';
var
  Buffer: TMemoryStream;
  i, j: integer;
  HTTPSender: THTTPSend;
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
    debugln('project is *' + SourceForgeProject + '*');
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
            raise Exception.Create('Download failed with error code ' + IntToStr(
              HTTPSender.ResultCode) + ' (' + HTTPSender.ResultString + ')');
        end;//case
      end;//while
      debugln('resulting url after sf redir: *' + URL + '*');
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
  SVNZip:string;
  SVNFiles: TStringList;
begin
  // Download SVN in make path. Not required for making FPC/Lazarus, but when downloading FPC/Lazarus from... SVN ;)
  // This won't work, we'd get an .msi:
  // http://sourceforge.net/projects/win32svn/files/latest/download?source=files
  // We don't want msi/Windows installer - this way we can hopefully support Windows 2000
  result:=false;
  {$IFDEF Windows}
  if FSVNDirectory='' then FSVNDirectory:='c:\development\svn';
  {$ELSE}
  raise exception.create('todo: Fix this code'); //probably somewhere in /home??
  {$ENDIF}
  ForceDirectories(FSVNDirectory);
  SVNZip:=FSVNDirectory+'svn.zip';
  Result := DownloadHTTP('http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip'
    , SVNZip);
  Run(FUnzip,'"'+SVNZip+'" -d "'+FSVNDirectory+'"');
  //SVNFiles:=TStringList.Create; //No, Findallfiles does that for you!?!?
  FindAllFiles(FSVNDirectory, 'svn'+FExecutableExtension,true);
  try
    if SVNFiles.Count>0 then
    begin
      // Just get first result.
      FUpdater.SVNExecutable := SVNFiles.Strings[0];
    end
    else
    begin
      debugln('Could not find svn executable in or under ' + FSVNDirectory);
      result:=false;
    end;
  finally
    SVNFiles.Free;
  end;
end;

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  OperationSucceeded: boolean;
  Output: string;
begin
  OperationSucceeded := true;
  FUnzip := FMakePath + 'unzip' + FExecutableExtension;
  if OperationSucceeded then
  begin
    // Check for binutils directory, make and unzip executables.
    // Download if needed; will download unzip - needed for SVN download
    if (DirectoryExists(FMakePath) = False) or
      (FileExists(FMake) = False) or
      (FileExists(FUnzip) = False) then
    begin
      debugln('Make path ' + FMakePath + ' doesn''t have binutils. Going to download');
      OperationSucceeded := DownloadBinUtils;
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for proper make executable
    try
      Output:='';
      RunOutput(FMake, '-v', Output);
      if Ansipos('GNU Make', Output)=0 then raise Exception.Create('Found make executable but it is not GNU Make.');
    except
      // ignore errors, this is only an extra check
    end;
  end;


  //Check for SVN, download if needed
  if OperationSucceeded then
  begin
    if (FileExists(FUpdater.SVNExecutable) = False) and (OperationSucceeded) then
    begin
      debugln('SVN not found in '+FUpdater.SVNExecutable+', downloading');
      OperationSucceeded := DownloadSVN;
    end;
  end;


  if OperationSucceeded then
  begin
    // Check for proper FPC compiler
    try
      Output:='';
      RunOutput(BootstrapCompiler, '-h', Output); // Show help without waiting
      if Ansipos('Free Pascal Compiler', Output)=0 then raise Exception.Create('Found FPC executable but it is not a Free Pascal compiler.');
    except
      // ignore errors, this is only an extra check
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for bootstrap compiler, download if needed
    if (FileExists(BootstrapCompiler) = False) and (OperationSucceeded) then
    begin
      debugln('Bootstrap compiler not found, downloading');
      OperationSucceeded := DownloadBootstrapCompiler;
    end;
  end;

  Result := OperationSucceeded;
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
  Result := FMakePath;
end;

function TInstaller.Run(Executable, Params: string): longint;
begin
  debugln('Calling ' + Executable + ' ' + Params);
  result:=SysUtils.ExecuteProcess(Executable, Params, [])
end;

function TInstaller.RunOutput(Executable, Params: string; var Output: TStringList): longint;
var
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
      Result := True;
    end;
  end;

begin
  result := 255; //Preset to failure
  OutputStream:=TMemoryStream.Create;
  SpawnedProcess := TProcess.Create(nil);
  try
    // We can't use .executable and .parameters as we're passing multiple parameters which
    // would have to be parsed
    SpawnedProcess.CommandLine := Executable + ' ' + Params;
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
    Result := SpawnedProcess.ExitStatus;
  finally
    OutputStream.Free;
    SpawnedProcess.Free;
  end;
end;

function TInstaller.RunOutput(Executable, Params: string; var Output: String
  ): longint;
var
  OutputStringList: TStringList;
begin
  try
    OutputStringList:=TStringList.Create;
    RunOutput(Executable, Params, OutputStringList);
    Output:=OutputStringList.Text;
  finally
    OutputStringList.Free;
  end;
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
  FMakePath := IncludeTrailingPathDelimiter(AValue);
  FMake := FMakePath + 'make' + FExecutableExtension;
end;

function Tinstaller.GetFPC: boolean;
var
  Executable: string;
  FPCCfg: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded := CheckAndGetNeededExecutables;
  //Make sure we have the proper tools.
  if FUpdater.UpdateFPC = False then
  begin
    OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Make clean using bootstrap compiler
    // Note no error on failure, might be recoverable
    Executable := FMake;
    Params := ' FPC=' + FBootstrapCompiler + ' --directory=' +
      FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' clean';
    debugln('Running make clean for fpc:');
    Run(Executable, params);
  end;

  if OperationSucceeded then
  begin
    // Make (clean & all) using bootstrap compiler
    Executable := FMake;
    Params := ' FPC=' + FBootstrapCompiler + ' --directory=' +
      FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' all';
    debugln('Running make for FPC:');
    debugln(Executable + ' ' + Params);
    if Run(Executable, params) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Install using newly compiled compiler
    Executable := FMake;
    Params := ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' +
      DirectorySeparator + 'ppc386' + ' --directory=' + FPCDirectory +
      ' PREFIX=' + FPCDIRECTORY + ' UPXPROG=echo COPYTREE=echo' + ' install';
    debugln('Running make install for FPC:');
    if Run(Executable, Params) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Install crosscompiler
    Executable := FMake;
    debugln('Running Make crossinstall:');
    debugln(Executable + ' ' + Params);
    Params := '--directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' +
      DirectorySeparator + 'ppc386' + ' UPXPROG=echo COPYTREE=echo' +
      ' OS_TARGET=win64 CPU_TARGET=x86_64' + ' crossinstall';
    if Run(Executable, Params) <> 0 then
      OperationSucceeded := False;
  end;

  if OperationSucceeded then
  begin
    // Create fpc.cfg if needed
    //todo: replace fpc.cfg location with correct bin path for resulting compiler (differs per platform); we'll need it for compilation/make above, anyway
    FPCCfg:= FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'fpc.cfg';
    if FileExists(FPCCg) = false then
    begin
      Executable := FPCDirectory + DirectorySeparator + 'bin' +
        DirectorySeparator + 'i386-win32' + DirectorySeparator + 'fpcmkcfg';
      Params := ' -d basepath="' + FPCDirectory + '"' + ' -o "'+FPCCfg +'"';
      debugln('Debug: Running fpcmkcfg: ');
      if Run(Executable, Params) <> 0 then
        OperationSucceeded := False;
    end
    else
    begin
      debugln('fpc.cfg already exists; leaving it alone.');
    end;
  end;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  //Make sure we have the proper tools.
  OperationSucceeded := CheckAndGetNeededExecutables;

  // Download Lazarus source:
  if OperationSucceeded = True then
    OperationSucceeded := FUpdater.UpdateLazarus;
  if OperationSucceeded then
    debugln('Got Lazarus sources ok')
  else
    debugln('Did not get Lazarus sources');

  // Make sure primary config path exists
  if DirectoryExists(LazarusPrimaryConfigPath) = False then
  begin
    ForceDirectories(LazarusPrimaryConfigPath);
  end;

  // Make (compile)
  if OperationSucceeded then
  begin
    // Make clean; failure here might be recoverable, so no fiddling with OperationSucceeded
    // todo: fix FPC argument for linux/other platforms
    // Note: you apparently can't pass FPC in the FPC= statement, you need to pass a PPC executable.
    Executable := FMake;
    Params := '--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386' + FExecutableExtension + ' clean';
    debugln('Lazarus: running make clean:');
    Run(Executable, Params);
  end;
  if OperationSucceeded then
  begin
    // MakePath all
    Executable := FMake;
    Params := '--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386' + FExecutableExtension + ' all';
    debugln('Lazarus: running make all:');
    if (Run(Executable, Params)) <> 0 then
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
    if (Run(Executable, Params)) <> 0 then
      OperationSucceeded := False;
  end;
  debugln('todo: make shortcut on desktop, maybe start menu');
  Result := OperationSucceeded;
end;

constructor Tinstaller.Create;
var
  AppDataPath: array[0..MaxPathLen] of char; //Allocate memory
begin
  // We'll set the bootstrap compiler to a file in the temp dir.
  // This won't exist so the CheckAndGetNeededExecutables code will download it for us.
  // User can specify an existing compiler later on, if she wants to.
  FBootstrapCompiler := SysUtils.GetTempFileName + FExecutableExtension;

  //We don't want to download from FTP, but it's useful to record it here so we can update the URLs below
  //BootstrapURL='ftp://ftp.freepascal.org/pub/fpc/dist/2.4.2/bootstrap/i386-win32-ppc386.zip';
  {$IFDEF Windows}
  FBootstrapCompilerURL :=
    'http://sunet.dl.sourceforge.net/project/freepascal/Bootstrap/2.4.2/i386-win32-ppc386.zip';
  {$ENDIF Windows}
  {$IFDEF Linux}
  //check if this is the right one
  FBootstrapCompilerURL :=
    'http://kent.dl.sourceforge.net/project/freepascal/Bootstrap/2.4.4/i386-linux-ppc386.bz2';
  {$ENDIF Linux}
  {$IFDEF Darwin}
  FBootstrapCompilerURL :=
    'http://freefr.dl.sourceforge.net/project/freepascal/Bootstrap/2.6.0/universal-darwin-ppcuniversal.tar.bz2';
  {$ENDIF Darwin}

  FCompiler := '';
  {$IFDEF WINDOWS}
  FExecutableExtension := '.exe';
  {$ELSE}
  FExecutableExtension := '';
  {$ENDIF WINDOWS}
  FLazarusPrimaryConfigPath := '';
  FSVNDirectory:='';
  FUpdater := TUpdater.Create;
  FUnzip := '';
  //Directory where Lazarus installation config will end up (primary config path)
  {$IFDEF Windows}
  AppDataPath := '';
  SHGetSpecialFolderPath(0, AppDataPath, CSIDL_LOCAL_APPDATA, False);
  LazarusPrimaryConfigPath := AppDataPath + DirectorySeparator + 'lazarusdev';
  {$ELSE}
  writeln('todo: fix Lazarus primary config path, somewhere in ~ I guess.');
  LazarusPrimaryConfigPath := '/tmp'; //error!???
  {$ENDIF}
  SetMakePath('');
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

