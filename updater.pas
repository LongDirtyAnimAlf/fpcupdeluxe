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
    FFPCURL: string;
    FLazarusURL: string;
    FSVNClient: TSVNClient;
    //function IsSVNInstalled: boolean;
    FFPCDirectory: string;
    FLazarusDirectory: string;
  public
    property FPCDirectory: string read FFPCDirectory write FFPCDirectory;
    property FPCURL: string read FFPCURL write FFPCURL; //URL for FPC SVN
    property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
    property LazarusURL: string read FLazarusURL write FLazarusURL; //URL for Lazarus SVN
    function UpdateFPC: boolean; // Checks out or updates FPC source
    function UpdateLazarus: boolean; //Checks out or updates Lazarus source
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
  FSVNClient.Repository:=FPCURL;
  FSVNClient.CheckOutOrUpdate;
  //todo: check for/handle errors
  result:=true;
end;

function Tupdater.Updatelazarus: Boolean;
begin
  FSVNClient.LocalRepository:=LazarusDirectory;
  //todo: hardcoded Lazarus repository for now
  //use svn2, apparently faster than the svn server ;)
  FSVNClient.Repository:=FLazarusURL;
  FSVNClient.CheckOutOrUpdate;
  //todo: check for/handle errors
  result:=true;
  writeln('debug: lazarus checkout/update complete');sleep(100);
end;

constructor Tupdater.Create;
begin
  FSVNClient:=TSVNClient.Create;
  FFPCURL:='http://svn.freepascal.org/svn/fpc/trunk'; //Default: latest (trunk)
  FLazarusURL:='http://svn.freepascal.org/svn/lazarus/trunk'; //Default: latest (trunk)
end;

destructor Tupdater.Destroy;
begin
  FSVNClient.Free;
  inherited Destroy;
end;

end.

