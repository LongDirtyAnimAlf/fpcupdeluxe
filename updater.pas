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
  //use svn2, apparently faster than the svn server ;)
  //todo: rebase later on so users can send patches?
  FSVNClient.Repository:='http://svn2.freepascal.org/svn/fpc/branches/fixes_2_6';
  FSVNClient.CheckOutOrUpdate;
  //todo: check for/handle errors
  result:=true;
end;

function Tupdater.Updatelazarus: Boolean;
begin
  FSVNClient.LocalRepository:=LazarusDirectory;
  //todo: hardcoded Lazarus repository for now
  //use svn2, apparently faster than the svn server ;)
  FSVNClient.Repository:='http://svn2.freepascal.org/svn/lazarus/trunk';
  FSVNClient.CheckOutOrUpdate;
  //todo: check for/handle errors
  result:=true;
  writeln('debug: lazarus checkout/update complete');sleep(100);
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

