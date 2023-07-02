unit m_anyinternallinker_to_winarm64;

{ Cross compiles from Linux, FreeBSD,... to Windows x86_64 code (winarm64)
Requirements: FPC should have an internal linker
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  FileUtil, LazFileUtils, m_crossinstaller, fpcuputil;

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
  result:=inherited;
  if result then exit;
  FLibsPath:='';
  result:=true;
  FLibsFound:=true;
end;

function Tanyinternallinker_winarm64.GetBinUtils(Basepath:string): boolean;
var
  ClangBin:string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:='';
  FBinUtilsPath:='';

  ClangBin:='clang';
  ClangBin:=Which(ClangBin+GetExeExt);

  result:=FileExists(ClangBin);

  if result then
  begin
    if FileIsSymlink(ClangBin) then
    begin
      try
        ClangBin:=GetPhysicalFilename(ClangBin,pfeException);
        FBinUtilsPath:=ExtractFilePath(ClangBin);
      except
      end;
    end;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    if (Length(FBinUtilsPath)>0) then AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix);
    AddFPCCFGSnippet('-O-'); // Diable optimizer for now.
  end
  else
  begin
    ShowInfo('Clang not found. Clang is needed as assembler. Please install clang.',etError);
    ShowInfo('Will continue, but expect errors.',etError);
    result:=True;
    FAlreadyWarned:=True;
  end;
end;

constructor Tanyinternallinker_winarm64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.win64;
  Reset;
  FAlreadyWarned:=False;
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

