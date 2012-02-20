unit m_crossinstaller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  private
  protected
    FBinUtilsPath: string;
    FBinUtilsPrefic: string;
    FLibsPath: string;
    FTargetCPU: string;
    FTargetOS: string;
    procedure SetBinUtilsPath(AValue: string);
    procedure SetBinUtilsPrefic(AValue: string);
    procedure SetLibsPath(AValue: string);
    procedure SetTargetCPU(AValue: string);
    procedure SetTargetOS(AValue: string);
    function GetLibs:boolean;virtual; abstract;
    function GetBinUtils:boolean;virtual; abstract;
    property LibsPath:string read FLibsPath write SetLibsPath;
    property BinUtilsPath:string read FBinUtilsPath write SetBinUtilsPath;
    property BinUtilsPrefic:string read FBinUtilsPrefic write SetBinUtilsPrefic;
    property TargetCPU:string read FTargetCPU write SetTargetCPU;
    property TargetOS:string read FTargetOS write SetTargetOS;
  public
    constructor Create;
    destructor Destroy; override;
  end;

Procedure
RegisterExtension(Platform:string;Extension:TCrossInstaller);
Var
  CrossInstallers:TStringList;

implementation

{ TCrossInstaller }
procedure RegisterExtension(Platform:string;Extension:TCrossInstaller);
begin
  //fill in here
end;

procedure TCrossInstaller.SetBinUtilsPath(AValue: string);
begin

end;

procedure TCrossInstaller.SetBinUtilsPrefic(AValue: string);
begin

end;

procedure TCrossInstaller.SetLibsPath(AValue: string);
begin

end;

procedure TCrossInstaller.SetTargetCPU(AValue: string);
begin
  if FTargetCPU=AValue then Exit;
  FTargetCPU:=AValue;
end;

procedure TCrossInstaller.SetTargetOS(AValue: string);
begin
  if FTargetOS=AValue then Exit;
  FTargetOS:=AValue;
end;

constructor TCrossInstaller.Create;
begin

end;

destructor TCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

end.

