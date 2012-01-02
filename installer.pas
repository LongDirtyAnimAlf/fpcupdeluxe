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
    FUpdater: TUpdater;
    function GetFPCDirectory: string;
    function GetLazarusDirectory: string;
    procedure SetFPCDirectory(Directory: string);
    procedure SetLazarusDirectory(Directory: string);
  public
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property FPCDirectory: string read GetFPCDirectory write SetFPCDirectory;
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses consolelistener;

{ TInstaller }

function Tinstaller.Getfpcdirectory: String;
begin
  result:=FUpdater.FPCDirectory;
end;

function Tinstaller.Getlazarusdirectory: String;
begin
  result:=FUpdater.LazarusDirectory;
end;

procedure Tinstaller.Setfpcdirectory(Directory: String);
begin
  FUpdater.FPCDirectory:=Directory;
end;

procedure Tinstaller.Setlazarusdirectory(Directory: String);
begin
  FUpdater.LazarusDirectory:=Directory;
end;

function Tinstaller.Getfpc: Boolean;
begin
  //todo
end;

function Tinstaller.Getlazarus: Boolean;
begin
  //todo
end;

constructor Tinstaller.Create;
begin
  FUpdater:=TUpdater.Create;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

