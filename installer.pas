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
    fUpdater: TUpdater;
  public
    function GetFPC: boolean; //Get/update FPC
    function GetLazarus: boolean; //Get/update Lazarus
    property FPCDirectory: string read fUpdater.FPCDirectory write FUpdater.FPCDirectory;
    property LazarusDirectory: string read FUpdater.LazarusDirectory write FUpdater.LazarusDirectory;



    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses consolelistener;

{ TInstaller }

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
  FUPdater:=TUpdater.Create;
end;

destructor Tinstaller.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

end.

