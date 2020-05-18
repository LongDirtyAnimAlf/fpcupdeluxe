unit m_anyinternallinker_to_winarm64;

{ Cross compiles from Linux, FreeBSD,... to Windows x86_64 code (winarm64)
Requirements: FPC should have an internal linker
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  fpcuputil;

type

{ Tanyinternallinker_winarm64 }

Tanyinternallinker_winarm64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs({%H-}Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32 }

function Tanyinternallinker_winarm64.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

function Tanyinternallinker_winarm64.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  ClangBin:string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:='';
  FBinUtilsPath:='';

  ClangBin:=Which('clang'+GetExeExt);

  result:=FileExists(ClangBin);

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    if (Length(FBinUtilsPath)>0) then AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix);
  end
  else
  begin
    ShowInfo('Clang not found. Clang is needed as assembler. Please install clang first.',etError);
  end;
end;

constructor Tanyinternallinker_winarm64.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TAnyinternallinker';
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.win64;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tanyinternallinker_winarm64.Destroy;
begin
  inherited Destroy;
end;

var
  Anyinternallinker_winarm64:Tanyinternallinker_winarm64;

initialization
  Anyinternallinker_winarm64:=Tanyinternallinker_winarm64.Create;
  RegisterCrossCompiler(Anyinternallinker_winarm64.RegisterName,Anyinternallinker_winarm64);

finalization
  Anyinternallinker_winarm64.Destroy;
end.

