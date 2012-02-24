unit installerLazarus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore;

type

  { TLazarusInstaller }

  TLazarusInstaller = class(TInstaller)
  private
    FCrossLCL_Platform: string;
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; virtual;
  public
    // LCL widget set to be build
    property CrossLCL_Platform:string write FCrossLCL_Platform;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Create configuration in PrimaryConfigPath
    function ConfigLazarus(PrimaryConfigPath:string):boolean;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    // Install update sources
    function GetModule(ModuleName:string): boolean; override;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TLazarusNativeInstaller }

  TLazarusNativeInstaller = class(TLazarusInstaller)
  protected
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TLazarusCrossInstaller }

  TLazarusCrossInstaller = class(TLazarusInstaller)
  protected
  public
    // Build module descendant customisation
    function BuildModuleCustom(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;



implementation

{ TLazarusCrossInstaller }

function TLazarusCrossInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  Result:=inherited BuildModuleCustom(ModuleName);
end;

constructor TLazarusCrossInstaller.Create;
begin
end;

destructor TLazarusCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusNativeInstaller }

function TLazarusNativeInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  Result:=inherited BuildModuleCustom(ModuleName);
end;

constructor TLazarusNativeInstaller.Create;
begin
  inherited create;
end;

destructor TLazarusNativeInstaller.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusInstaller }

function TLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin

end;

function TLazarusInstaller.BuildModule(ModuleName: string): boolean;
begin

end;

function TLazarusInstaller.ConfigLazarus(PrimaryConfigPath: string): boolean;
begin

end;

function TLazarusInstaller.CleanModule(ModuleName: string): boolean;
begin

end;

function TLazarusInstaller.GetModule(ModuleName: string): boolean;
begin

end;

function TLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
begin

end;

constructor TLazarusInstaller.Create;
begin
  inherited create;
end;

destructor TLazarusInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

