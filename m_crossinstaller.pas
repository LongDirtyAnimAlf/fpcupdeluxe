unit m_crossinstaller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  protected
    FBinUtilsPath: string;
    FBinUtilsPrefix: string;
    FLibsPath: string;
    FTargetCPU: string;
    FTargetOS: string;
  public
    function GetLibs(Basepath:string):boolean;virtual; abstract;
    function GetBinUtils(Basepath:string):boolean;virtual; abstract;
    property LibsPath:string read FLibsPath;
    property BinUtilsPath:string read FBinUtilsPath;
    property BinUtilsPrefix:string read FBinUtilsPrefix;
    property TargetCPU:string read FTargetCPU;
    property TargetOS:string read FTargetOS;
    constructor Create;
    destructor Destroy; override;
  end;

Procedure
RegisterExtension(Platform:string;Extension:TCrossInstaller);
Var
  CrossInstallers:TStringList=nil;

implementation

{ TCrossInstaller }
procedure RegisterExtension(Platform:string;Extension:TCrossInstaller);
begin
  if not assigned(CrossInstallers) then
    CrossInstallers:=TStringList.Create;
  CrossInstallers.AddObject(Platform,TObject(Extension));
end;

constructor TCrossInstaller.Create;
begin

end;

destructor TCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

finalization
if assigned(CrossInstallers) then
  CrossInstallers.Destroy;
end.
