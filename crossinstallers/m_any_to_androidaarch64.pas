unit m_any_to_androidaarch64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_android_base;

type
  { TAny_AndroidAarch64 }
  TAny_AndroidAarch64 = class(Tany_android)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TAny_AndroidAarch64 }

function TAny_AndroidAarch64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TAny_AndroidAarch64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TAny_AndroidAarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  Reset;
  FAlreadyWarned:=false;

  SetLength(NDKTOOLCHAINVERSIONS,2);

  ARCH:=TargetCPUName;
  ARCHSHORT:='arm64';
  NDKTOOLCHAINVERSIONS[0]:=ARCH+'-linux-'+OS+'-4.8';
  NDKTOOLCHAINVERSIONS[1]:=ARCH+'-linux-'+OS+'-4.9';
  NDKARCHDIRNAME:='arch-'+ARCHSHORT;

  ShowInfo;
end;

destructor TAny_AndroidAarch64.Destroy;
begin
  inherited Destroy;
end;

var
  Any_AndroidAarch64:TAny_AndroidAarch64;

initialization
  Any_AndroidAarch64:=TAny_AndroidAarch64.Create;
  RegisterCrossCompiler(Any_AndroidAarch64.RegisterName,Any_AndroidAarch64);

finalization
  Any_AndroidAarch64.Destroy;
end.

