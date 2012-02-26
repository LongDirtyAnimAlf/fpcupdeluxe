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
  // Directory where help files are placed
  FTargetDirectory: string;
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; virtual;
  // internal initialisation, called from BuildModule,CLeanModule,GetModule
  // and UnInstallModule but executed only once
  function InitModule:boolean; virtual;
  property TargetDirectory: string read FTargetDirectory write FTargetDirectory;
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

{ THelpFPCInstaller }

THelpFPCInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
  function InitModule:boolean; override;
public
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

{ THelpLazarusInstaller }

THelpLazarusInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
  function InitModule:boolean; override;
public
  constructor Create;
  destructor Destroy; override;
end;

implementation

uses fpcuputil, processutils;

{ THelpInstaller }

function THelpInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
end;

function THelpInstaller.InitModule: boolean;
begin
  result:=true;
end;

function THelpInstaller.BuildModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  result:=true;
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
const
  // Location of FPC CHM help zip
  FPC_CHM_URL='http://garr.dl.sourceforge.net/project/freepascal/Documentation/2.6.0/doc-chm.zip';
var
  DocsZip: string;
  OperationSucceeded: boolean;
  ResultCode: longint;
begin
  if not InitModule then exit;
  OperationSucceeded:=false;
  if UpperCase(ModuleName)='HELPLAZARUS' then
  begin
    // Download FPC CHM docs zip into TargetDirectory.
    OperationSucceeded:=true;
    ForceDirectories(TargetDirectory);
    DocsZip := SysUtils.GetTempFileName + '.zip';
    try
      OperationSucceeded:=Download(FPC_CHM_URL,DocsZip);
    except
      on E: Exception do
      begin
        // Deal with timeouts, wrong URLs etc
        OperationSucceeded:=false;
        infoln(ModuleName+': Download failed. URL: '+FPC_CHM_URL+LineEnding+
          'Exception: '+E.ClassName+'/'+E.Message);
      end;
    end;

    if OperationSucceeded then
    begin
      // Extract, overwrite, flatten path/junk paths
      // todo: test with spaces in path
      if ExecuteCommandHidden(FUnzip,'-o -j -d '+IncludeTrailingPathDelimiter(TargetDirectory)+' '+DocsZip,FVerbose)= 0 then
      begin
        SysUtils.deletefile(DocsZip); //Get rid of temp zip if not more needed for troubleshooting.
      end
      else
      begin
        OperationSucceeded := False;
        infoln(ModuleName+': unzip failed with resultcode: '+IntToStr(ResultCode));
      end;
    end
    else
    begin
      infoln(ModuleName+': download failed. FPC_CHM_URL: '+FPC_CHM_URL);
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

function THelpFPCInstaller.InitModule: boolean;
begin
  result:=false;
  if inherited InitModule then
  begin
    //todo: check with FreeVision FPCIDE to see if this is a sensible location.
    //todo: why is the BaseDirectory property write-only? Why use FBaseDirectory?
    TargetDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'doc'+DirectorySeparator+
      'ide'+DirectorySeparator; ;
    result:=true;
  end;
end;

function THelpFPCInstaller.GetModule(ModuleName: string): boolean;
begin
  Result:=inherited GetModule(ModuleName);
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

function THelpLazarusInstaller.InitModule: boolean;
begin
  result:=false;
  if inherited InitModule then
  begin
    TargetDirectory:=IncludeTrailingPathDelimiter(FBaseDirectory)+
      'docs'+DirectorySeparator+
      'html'+DirectorySeparator; ;
    result:=true;
  end;
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

