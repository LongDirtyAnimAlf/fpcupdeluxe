unit updater;
{ Gets/updats FPC and Lazarus sources }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, svnclient;
type

  { TUpdater }

  TUpdater = class(TObject)
  private
    FSVNClient: TSVNClient;
    //function IsSVNInstalled: boolean;
    FFPCDirectory: string;
    FLazarusDirectory: string;
  public
    { Checks out or updates FPC source }
    function UpdateFPC: boolean;
    { Checks out or updates Lazarus source }
    function UpdateLazarus: boolean;
    property FPCDirectory: string read FFPCDirectory write FFPCDirectory;
    property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
    constructor Create;
    destructor Destroy; override;
  end;
implementation


{ TUpdater }

function Tupdater.Updatefpc: Boolean;
begin
  FSVNClient.LocalRepository:=FPCDirectory;
  //todo: hardcoded FPC repository for now
  FSVNClient.Repository:='http://svn.freepascal.org/svn/fpc/branches/fixes_2_6';
  if FSVNClient.LocalRepositoryExists = false then
  begin
    // Checkout (first download)
    FSVNClient.Checkout;
  end
  else
  begin
    // Update
    FSVNClient.Update;
  end;
  //todo: check for/handle errors
  result:=true;
end;

function Tupdater.Updatelazarus: Boolean;
begin
  FSVNClient.LocalRepository:=LazarusDirectory;
  //todo: hardcoded Lazarus repository for now
  FSVNClient.Repository:='http://svn.freepascal.org/svn/lazarus';
  if FSVNClient.LocalRepositoryExists = false then
  begin
    // Checkout (first download)
    FSVNClient.Checkout;
  end
  else
  begin
    // Update
    FSVNClient.Update;
  end;
  //todo: check for/handle errors
  result:=true;
end;

constructor Tupdater.Create;
begin
  FSVNClient:=TSVNClient.Create;
end;

destructor Tupdater.Destroy;
begin
  FSVNClient.Free;
  inherited Destroy;
end;

end.

