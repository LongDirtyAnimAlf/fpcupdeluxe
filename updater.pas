unit updater;
{ Gets/updats FPC and Lazarus sources }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type

  { TUpdater }

  TUpdater = class(TObject)
  private
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

uses svnclient;


{ TUpdater }

function Tupdater.Updatefpc: Boolean;
begin

end;

function Tupdater.Updatelazarus: Boolean;
begin

end;

constructor Tupdater.Create;
begin

end;

destructor Tupdater.Destroy;
begin
  inherited Destroy;
end;

end.

