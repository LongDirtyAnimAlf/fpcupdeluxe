unit m_linux_to_win386;

{ Cross compiles from Linux to Windows i386 code (win32)
Needed files:
- see error message in code
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller, fpcuputil;

implementation
const
  ErrorNotFound='Not all required files are present.'+LineEnding+
    'todo: specify what is missing';

type

{ TLinux_win386 }

TLinux_win386 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function TLinux_win386.GetLibs(Basepath:string): boolean;
begin
  FLibsPath:='';
  result:=true;
  if (result=false) and (FAlreadyWarned=false) then
  begin
    infoln(ErrorNotFound,etError);
    FAlreadyWarned:=true;
  end;
end;

function TLinux_win386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: check if/how we need to implement freebsd=>win386 GetLibsLCL
  result:=true;
end;

function TLinux_win386.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  if (result=false) and (FAlreadyWarned=false) then
  begin
    infoln(ErrorNotFound,etError);
    FAlreadyWarned:=true;
  end;
end;

constructor TLinux_win386.Create;
begin
  inherited Create;
  FTargetCPU:='i386';
  FTargetOS:='win32';
  FAlreadyWarned:=false;
end;

destructor TLinux_win386.Destroy;
begin
  inherited Destroy;
end;

var
  Linux_win386:TLinux_win386;

initialization
  Linux_win386:=TLinux_win386.Create;
  RegisterExtension(Linux_win386.TargetCPU+'-'+Linux_win386.TargetOS,Linux_win386);
finalization
  Linux_win386.Destroy;
end.

