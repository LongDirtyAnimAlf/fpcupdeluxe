unit m_any_to_android386;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  m_crossinstaller, m_any_to_android_base;

type

  { TAny_Android386 }
  TAny_Android386 = class(Tany_android)
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

{ TAny_Android386 }

function TAny_Android386.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
end;

function TAny_Android386.GetBinUtils(Basepath:string): boolean;
begin
  FBinUtilsPrefix:='i686'+'-linux-'+TargetOSName+'-'; //standard eg in Android NDK 9
  result:=inherited;
end;

constructor TAny_Android386.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  Reset;
  FAlreadyWarned:=false;

  SetLength(NDKTOOLCHAINVERSIONS,1);

  ARCH:=TargetCPUName;
  ARCHSHORT:='x86';
  NDKTOOLCHAINVERSIONS[0]:=ARCH+'-4.9';
  NDKARCHDIRNAME:='arch-'+ARCHSHORT;

  ShowInfo;
end;

destructor TAny_Android386.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Android386:TAny_Android386;

initialization
  Any_Android386:=TAny_Android386.Create;
  RegisterCrossCompiler(Any_Android386.RegisterName,Any_Android386);

finalization
  Any_Android386.Destroy;
end.

