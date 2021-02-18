unit m_any_to_androidarm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_android_base;

type
  { TAny_AndroidARM }
  TAny_AndroidARM = class(Tany_android)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TAny_AndroidARM }

function TAny_AndroidARM.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TAny_AndroidARM.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPrefix:=TargetCPUName+'-linux-'+TargetOSName+'eabi-'; //standard eg in Android NDK 9
  result:=inherited;
end;

constructor TAny_AndroidARM.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.arm;
  Reset;
  FAlreadyWarned:=false;

  SetLength(NDKTOOLCHAINVERSIONS,4);

  ARCH:=TargetCPUName;
  ARCHSHORT:=TargetCPUName;
  NDKTOOLCHAINVERSIONS[0]:=ARCH+'-linux-'+OS+'eabi-4.4.7';
  NDKTOOLCHAINVERSIONS[1]:=ARCH+'-linux-'+OS+'eabi-4.6';
  NDKTOOLCHAINVERSIONS[2]:=ARCH+'-linux-'+OS+'eabi-4.8';
  NDKTOOLCHAINVERSIONS[3]:=ARCH+'-linux-'+OS+'eabi-4.9';
  NDKARCHDIRNAME:='arch-'+ARCHSHORT;

  ShowInfo;
end;

destructor TAny_AndroidARM.Destroy;
begin
  inherited Destroy;
end;

var
  Any_AndroidARM:TAny_AndroidARM;

initialization
  Any_AndroidARM:=TAny_AndroidARM.Create;
  RegisterCrossCompiler(Any_AndroidARM.RegisterName,Any_AndroidARM);

finalization
  Any_AndroidARM.Destroy;

end.

