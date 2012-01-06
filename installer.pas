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
    FLazarusPrimaryConfigPath: string;
    FMake: string;
    FUpdater: TUpdater;
    function GetFpcDirectory: string;
    function GetFPCUrl: string;
    function GetLazarusDirectory: string;
    function GetLazarusUrl: string;
    procedure SetFPCDirectory(Directory: string);
    procedure SetFPCUrl(AValue: string);
    procedure SetLazarusDirectory(Directory: string);
    procedure SetLazarusUrl(AValue: string);
  public
    property Compiler: string read FCompiler; //Full path to FPC compiler that is installed by this program
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler; //Compiler used to compile compiler sources
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property FPCURL: string read GetFPCUrl write SetFPCUrl; //SVN URL for FPC
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    property LazarusPrimaryConfigPath: string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath; //The directory where the configuration for this Lazarus instance must be stored.
    property LazarusURL: string read GetLazarusUrl write SetLazarusUrl; //SVN URL for Lazarus
    property Make: string read FMake write FMake;
    constructor Create;
    destructor Destroy; override;
  end;
  procedure debugln(Message: string); //Uses writeln for now, and waits a bit afterwards so output is hopefully not garbled

implementation

{$IFDEF WINDOWS}
uses
  shlobj;
{$ENDIF WINDOWS}

procedure debugln(Message: string);
begin
  {$IFDEF DEBUG}
  writeln('Debug: '+Message);
  sleep(200); //allow output to be written
  {$ENDIF DEBUG}
end;

{ TInstaller }


function Tinstaller.GetFpcDirectory: string;
begin
  Result := FUpdater.FPCDirectory;
end;

function TInstaller.GetFPCUrl: string;
begin
  result:=FUpdater.FPCURL;
end;

function Tinstaller.GetLazarusDirectory: string;
begin
  Result := FUpdater.LazarusDirectory;
end;

function TInstaller.GetLazarusUrl: string;
begin
  result:=FUpdater.LazarusURL;
end;

procedure Tinstaller.SetFPCDirectory(Directory: string);
begin
  FUpdater.FPCDirectory := Directory;
end;

procedure TInstaller.SetFPCUrl(AValue: string);
begin
  FUpdater.FPCURL:=AValue;
end;

procedure Tinstaller.SetLazarusDirectory(Directory: string);
begin
  FUpdater.LazarusDirectory := Directory;
end;

procedure TInstaller.SetLazarusUrl(AValue: string);
begin
  FUpdater.LazarusURL:=AValue;
end;

function Tinstaller.Getfpc: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded:=true;
  if FUpdater.UpdateFPC = false then
  begin
    OperationSucceeded := false;
  end;
  if OperationSucceeded then
  begin
    // Make clean using bootstrap compiler
    // Note no error on failure, might be recoverable
    Executable:=FMake;
    Params:= ' FPC='+FBootstrapCompiler+
      ' --directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' clean';
    debugln('Running make clean for fpc:');
    debugln(Executable + ' ' + Params);
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    SysUtils.ExecuteProcess(Executable, params, []);
  end;

  if OperationSucceeded then
  begin
    // Make (clean & all) using bootstrap compiler
    Executable:=FMake;
    Params:= ' FPC='+FBootstrapCompiler+
      ' --directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' + ' all';
    debugln('debug: running make for fpc:');
    debugln(Executable + ' ' + Params);
    //todo: check for bootstrap fpc compiler
    // Make (compile)
    if
    SysUtils.ExecuteProcess(Executable, params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Install using newly compiled compiler
    // todo: check where to install
    Executable:=FMake;
    Params:= ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' --directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' UPXPROG=echo COPYTREE=echo' + ' install';
    debugln('debug: running make install for fpc:');
    debugln(Executable + ' ' + Params);
    if SysUtils.ExecuteProcess(Executable,Params, []) <>0    then     OperationSucceeded:=false;
  end;
  { //don't know if this is needed
  if OperationSucceeded then
  begin
    // Make crosscompiler for Windows X64
    Executable:=FMake;
    Params:=' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      '--directory=' + FPCDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' OS_TARGET=win64 CPU_TARGET=x86_64' + ' all'
    debugln('Running Make crosscompile:');
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
    Executable:=FMake;
    debugln('Running Make crossinstall:');
    debugln(Executable + ' ' + Params);
    Params:='--directory=' + FPCDirectory + ' PREFIX=' + FPCDIRECTORY +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'compiler' + DirectorySeparator + 'ppc386' +
      ' UPXPROG=echo COPYTREE=echo' + ' OS_TARGET=win64 CPU_TARGET=x86_64' +
      ' crossinstall';
    if
    SysUtils.ExecuteProcess(Executable,Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  if OperationSucceeded then
  begin
    // Create fpc.cfg
    //todo: replace -o path with bin path for resulting compiler; we'll need it for compilation/make above, anyway
    //todo: only generate when it doesn't exist yet
    //todo: seems to generate 64 bit config!??!
    Executable:=FPCDirectory + DirectorySeparator + 'bin' +
      DirectorySeparator + 'i386-win32' + DirectorySeparator + 'fpcmkcfg';
    Params:= ' -d basepath="' + FPCDirectory + '"' + ' -o "' + FPCDirectory +
      DirectorySeparator + 'bin' + DirectorySeparator + 'i386-win32' + DirectorySeparator +
      'fpc.cfg"';
    debugln('Debug: Running fpcmkcfg: ');
    debugln(Executable + ' ' + Params);
    if
    SysUtils.ExecuteProcess(Executable,
      Params, [])
    <>0
    then
    OperationSucceeded:=false;
  end;
  Result := OperationSucceeded;
end;

function Tinstaller.GetLazarus: boolean;
var
  Executable: string;
  OperationSucceeded: boolean;
  Params: string;
begin
  OperationSucceeded := True;
  // Download Lazarus source:
  if OperationSucceeded = True then
    OperationSucceeded := FUpdater.UpdateLazarus;
  if OperationSucceeded then debugln('debug: lazarus ok') else debugln('debug: lazarus not ok');
  // Make (compile)
  // todo: remove hardcoded make, ppc compiler
  if OperationSucceeded then
  begin
    // Make clean; failure here might be recoverable, so no fiddling with OperationSucceeded
    //todo: fix for linux
    // Note: you apparently can't pass FPC in the FPC= statement.
    Executable:=FMake;
    Params:='--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386.exe' + ' clean';
    debugln('Lazarus: running make clean:');
    debugln(Executable + ' ' + Params);
    (SysUtils.ExecuteProcess(Executable,
      Params, []));
  end;
  if OperationSucceeded then
  begin
    // Make all
    Executable:=FMake;
    Params:='--directory=' + LazarusDirectory + ' UPXPROG=echo COPYTREE=echo' +
      ' FPC=' + FPCDirectory + DirectorySeparator + 'bin' + DirectorySeparator +
      'i386-win32' + DirectorySeparator + 'ppc386.exe' + ' all';
    debugln('Lazarus: running make all:');
    debugln(Executable + ' ' + Params);
    if (SysUtils.ExecuteProcess(Executable,
      Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  if OperationSucceeded then
  begin
    // Build data desktop, nice example of building with lazbuild
    Executable:=LazarusDirectory+DirectorySeparator+'lazbuild';
    Params:=
    '--pcp='+FLazarusPrimaryConfigPath+
    ' '+LazarusDirectory+DirectorySeparator+'tools'+DirectorySeparator+'lazdatadesktop'+DirectorySeparator+'lazdatadesktop.lpr';
    debugln('Lazarus: compiling data desktop:');
    debugln(Executable + ' ' + Params);
    if (SysUtils.ExecuteProcess(Executable,
      Params, [])) <> 0 then
      OperationSucceeded := False;
  end;
  //todo: setup primary config path, dir etc.
  //todo: make shortcut on desktop, maybe start menu
  Result := OperationSucceeded;
end;

constructor Tinstaller.Create;
var
  AppDataPath: Array[0..MaxPathLen] of Char; //Allocate memory
begin
  FBootstrapCompiler := '';
  FCompiler := '';
  FLazarusPrimaryConfigPath:='';
  FMake:='';
  FUpdater := TUpdater.Create;
  //Directory where Lazarus installation will end up
  //todo: create if it doesn't exist
  {$IFDEF Windows}
  AppDataPath:='';
  SHGetSpecialFolderPath(0,AppDataPath,CSIDL_LOCAL_APPDATA,false);
  LazarusPrimaryConfigPath:=AppDataPath+DirectorySeparator+'lazarusdev';
  {$ELSE}
  writeln('todo: fix Lazarus primary config path, somewhere in ~ I guess.');
  LazarusPrimaryConfigPath:='/tmp'; //error!???
  {$ENDIF}
  if DirectoryExists(LazarusPrimaryConfigPath)=false then
  begin
     CreateDir(LazarusPrimaryConfigPath);
  end;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

