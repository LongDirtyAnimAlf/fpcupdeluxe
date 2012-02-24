unit installerLazarus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller;

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
uses fpcuputil,fileutil,processutils
  {$IFDEF UNIX}
    ,baseunix
  {$ENDIF UNIX}
  ;

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
var
  oldlog:TErrorMethod;
begin
  // Make distclean; we don't care about failure (e.g. directory might be empty etc)
  oldlog:=ProcessEx.OnErrorM;
  ProcessEx.OnErrorM:=nil;  //don't want to log errors in distclean
  ProcessEx.Executable := FMake;
  ProcessEx.CurrentDirectory:=ExcludeTrailingPathDelimiter(FBaseDirectory);
  ProcessEx.Parameters.Clear;
  ProcessEx.Parameters.Add('FPC='+FCompiler+'');
  ProcessEx.Parameters.Add('--directory='+ExcludeTrailingPathDelimiter(FBaseDirectory));
  ProcessEx.Parameters.Add('UPXPROG=echo'); //Don't use UPX
  ProcessEx.Parameters.Add('COPYTREE=echo'); //fix for examples in Win svn, see build FAQ
  if FCrossLCL_Platform <>'' then
    ProcessEx.Parameters.Add('LCL_PLATFORM='+FCrossLCL_Platform );
  if Self is TLazarusCrossInstaller then
  begin  // clean out the correct compiler
    ProcessEx.Parameters.Add('OS_TARGET='+FCrossOS_Target);
    ProcessEx.Parameters.Add('CPU_TARGET='+FCrossCPU_Target);
  end;
  ProcessEx.Parameters.Add('distclean');
  if ModuleName='BIGIDE' then
  begin
    ProcessEx.Parameters.Add('bigideclean');
    infoln('Lazarus: running make distclean bigideclean before checkout/update:');
  end
  else
  begin
    infoln('Lazarus: running make distclean before checkout/update:');
  end;
  ProcessEx.Execute;
  ProcessEx.OnErrorM:=oldlog;
  result:=true;
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

