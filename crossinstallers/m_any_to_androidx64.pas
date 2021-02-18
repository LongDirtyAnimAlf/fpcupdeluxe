unit m_any_to_androidx64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_android_base;

type
  { TAny_Androidx64 }
  TAny_Androidx64 = class(Tany_android)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TAny_Androidx64 }

function TAny_Androidx64.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TAny_Androidx64.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
end;

constructor TAny_Androidx64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.x86_64;
  Reset;
  FAlreadyWarned:=false;

  SetLength(NDKTOOLCHAINVERSIONS,1);

  ARCH:=TargetCPUName;
  ARCHSHORT:=TargetCPUName;
  NDKTOOLCHAINVERSIONS[0]:=ARCH+'-4.9';
  NDKARCHDIRNAME:='arch-'+ARCHSHORT;

  ShowInfo;
end;

destructor TAny_Androidx64.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Androidx64:TAny_Androidx64;

initialization
  Any_Androidx64:=TAny_Androidx64.Create;
  RegisterCrossCompiler(Any_Androidx64.RegisterName,Any_Androidx64);

finalization
  Any_Androidx64.Destroy;

end.

