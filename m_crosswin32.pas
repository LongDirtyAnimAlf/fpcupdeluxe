unit m_crosswin32;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation
type

{ TWin32 }

TWin32 = class(TCrossInstaller)
private

public
  function GetLibs(Basepath:string):boolean;override;
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function TWin32.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='';
  result:=true;
end;

function TWin32.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;

function TWin32.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPath:='';
  FBinUtilsPrefix:='';
  result:=true;
end;

constructor TWin32.Create;
begin
  inherited Create;
  FTargetCPU:='i386';
  FTargetOS:='win32';
end;

destructor TWin32.Destroy;
begin
  inherited Destroy;
end;

var
  Win32:TWin32;

initialization
  Win32:=TWin32.Create;
  RegisterExtension(Win32.TargetCPU+'-'+Win32.TargetOS,Win32);
finalization
  Win32.Destroy;
end.

