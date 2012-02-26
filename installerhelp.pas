{ Help installer/uninstaller unit for fpcup
Copyright (C) 2012 Ludo Brands, Reinier Olislagers

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
unit installerHelp;

{This class installs, configures and uninstalls FPC and Lazarus help.
It is called by the state machine in installerManager.

When installing, the class downloads FPC RTL/FCL/reference .CHM files,
because compiling them from source is very complicated, and FPC help is
fairly static.
An LCL help CHM is generated from the Lazarus sources and cross-reference
information in the FPC help.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore;
Const
  Sequences=
// Convention: help modules start with help
//FPC .CHM download
    'Declare helpfpc;'+
    'Cleanmodule helpfpc;'+
    'Getmodule helpfpc;'+
    'End;'+
    //Lazarus help
    {Note: we don't use helpfpc because that will put the
    help files in the FPC base directory, not in the
    Lazarus base directory
    }
    //todo: replace help with lazhelp in main state machine
    'Declare helplazarus;'+
    'Requires BIGIDE;'+
    'Requires lhelp;'+
    'CleanModule helplazarus;'+
    'BuildModule helplazarus;'+
    'ConfigModule helplazarus;'+
    'End;'+
    //selective actions triggered with --only=SequenceName
    'Declare HelpFPCCleanOnly;'+
    'Cleanmodule helpfpc;'+
    'End;'+

    'Declare HelpLazarusCleanOnly;'+
    'Cleanmodule helplazarus;'+
    'End;'+

    'Declare HelpFPCGetOnly;'+
    'Getmodule helpfpc;'+
    'End;'+

    'Declare HelpLazarusBuildOnly;'+
    'Buildmodule helplazarus;'+
    'End;'+

    'Declare HelpLazarusConfigOnly;'+
    'Configmodule helplazarus;'+
    'End';
type

{ THelpInstaller }

THelpInstaller = class(TInstaller)
private
  InitDone:boolean;
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; virtual;
  // internal initialisation, called from BuildModule,CLeanModule,GetModule
  // and UnInstallModule but executed only once
  function InitModule:boolean;
public
  // Build module
  function BuildModule(ModuleName:string): boolean; override;
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  // Uninstall module
  function UnInstallModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

THelpFPCInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
public
  constructor Create;
  destructor Destroy; override;
end;

THelpLazarusInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
public
  constructor Create;
  destructor Destroy; override;
end;

implementation

uses fpcuputil;

{ THelpInstaller }

function THelpInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
end;

function THelpInstaller.InitModule: boolean;
begin

end;

function THelpInstaller.BuildModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  // We pass all responsibility to specialized THelpInstaller descendants.
  result:=BuildModuleCustom(ModuleName);
end;

function THelpInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=false;
  if not InitModule then exit;
  case UpperCase(ModuleName) of
    'HELPFPC':
    begin

    end;
    'HELPLAZARUS':
    begin

    end;
    else
      begin
        writelnlog('Don''t know how to clean module '+ModuleName,true);
      end;
  end;
end;

function THelpInstaller.GetModule(ModuleName: string): boolean;
var
  DocsZip: string;
  OperationSucceeded: boolean;
  ResultCode: longint;
begin
  if not InitModule then exit;
  OperationSucceeded:=false;
  if UpperCase(ModuleName)='HELPLAZARUS' then
  begin
    if (Self is THelpLazarusInstaller)=false then
    begin
      writelnlog('Don''t know how to get module '+ModuleName,true);
      OperationSucceeded:=false;
    end
    else
    begin
      // Download FPC CHM docs zip into TargetDirectory.
      OperationSucceeded:=true;
      ForceDirectories(TargetDirectory);
      DocsZip := SysUtils.GetTempFileName + '.zip';
      try
        OperationSucceeded:=Download(URL,DocsZip);
      except
        on E: Exception do
        begin
          // Deal with timeouts, wrong URLs etc
          OperationSucceeded:=false;
          infoln('DownloadFPCHelp: HTTP download failed. URL: '+URL+LineEnding+
            'Exception: '+E.ClassName+'/'+E.Message);
        end;
      end;

      if OperationSucceeded then
      begin
        // Extract, overwrite, flatten path/junk paths
        // todo: test with spaces in path
        if ExecuteCommandHidden(FUnzip,'-o -j -d '+IncludeTrailingPathDelimiter(TargetDirectory)+' '+DocsZip,Verbose)= 0 then
        begin
          SysUtils.deletefile(DocsZip); //Get rid of temp zip if success.
        end
        else
        begin
          OperationSucceeded := False;
          infoln('DownloadFPCHelp: unzip failed with resultcode: '+IntToStr(ResultCode));
        end;
      end
      else
      begin
        infoln('DownloadFPCHelp: HTTP download failed. URL: '+URL);
      end;
    end;
  end;
  Result := OperationSucceeded;
end;

function THelpInstaller.UnInstallModule(ModuleName: string): boolean;
begin

end;

constructor THelpInstaller.Create;
begin
  inherited Create;
end;

destructor THelpInstaller.Destroy;
begin
  inherited Destroy;
end;

{ THelpFPCInstaller }

function THelpFPCInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin

end;

constructor THelpFPCInstaller.Create;
begin
  inherited Create;
end;

destructor THelpFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

{ THelpLazarusInstaller }

function THelpLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin

end;

constructor THelpLazarusInstaller.Create;
begin
  inherited Create;
end;

destructor THelpLazarusInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

