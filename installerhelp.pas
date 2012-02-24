unit installerHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore;

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

end;

function THelpInstaller.CleanModule(ModuleName: string): boolean;
begin

end;

function THelpInstaller.GetModule(ModuleName: string): boolean;
begin

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

