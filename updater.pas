{ Lazarus/FPC source downloader unit
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
    function GetSVNExecutable: string;
    procedure SetSVNExecutable(AValue: string);
  public
    property FPCDirectory: string read FFPCDirectory write FFPCDirectory;
    property FPCURL: string read FFPCURL write FFPCURL; //URL for FPC SVN
    property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
    property LazarusURL: string read FLazarusURL write FLazarusURL; //URL for Lazarus SVN
    property SVNExecutable: string read GetSVNExecutable write SetSVNExecutable;
    //Which SVN executable to use
    function UpdateFPC: boolean; // Checks out or updates FPC source
    function UpdateLazarus: boolean; //Checks out or updates Lazarus source
    constructor Create;
    destructor Destroy; override;
  end;

implementation


{ TUpdater }

procedure TUpdater.SetSVNExecutable(AValue: string);
begin
  FSVNClient.SVNExecutable := AValue;
end;

function TUpdater.GetSVNExecutable: string;
begin
  Result := FSVNClient.SVNExecutable;
end;

function Tupdater.Updatefpc: boolean;
begin
  FSVNClient.LocalRepository := FPCDirectory;
  FSVNClient.Repository := FPCURL;
  FSVNClient.CheckOutOrUpdate;
  //todo: check for/handle errors
  Result := True;
end;

function Tupdater.Updatelazarus: boolean;
begin
  FSVNClient.LocalRepository := LazarusDirectory;
  FSVNClient.Repository := FLazarusURL;
  FSVNClient.CheckOutOrUpdate;
  Result := True;
  writeln('debug: lazarus checkout/update complete');
  sleep(100);
end;

constructor Tupdater.Create;
begin
  FSVNClient := TSVNClient.Create;
  FFPCURL := 'http://svn.freepascal.org/svn/fpc/trunk'; //Default: latest (trunk)
  FLazarusURL := 'http://svn.freepascal.org/svn/lazarus/trunk'; //Default: latest (trunk)
end;

destructor Tupdater.Destroy;
begin
  FSVNClient.Free;
  inherited Destroy;
end;

end.

