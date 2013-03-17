unit m_crosswin64;
{ Compiles from Windows 32 to Windows 64 bit }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation
type

{ TWin64 }

TWin64 = class(TCrossInstaller)
private

public
  function GetLibs(Basepath:string):boolean;override;
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin64 }

function TWin64.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='';
  result:=true;
end;

function TWin64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;

function TWin64.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPath:='';
  FBinUtilsPrefix:='';
  result:=true;
end;

constructor TWin64.Create;
begin
  inherited Create;
  FTargetCPU:='x86_64';
  FTargetOS:='win64';
end;

destructor TWin64.Destroy;
begin
  inherited Destroy;
end;

var
  Win64:TWin64;

initialization
  Win64:=TWin64.Create;
  RegisterExtension(Win64.TargetCPU+'-'+Win64.TargetOS,Win64);
finalization
  Win64.Destroy;
end.

