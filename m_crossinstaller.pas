unit m_crossinstaller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  protected
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FLibsPath: string; //path for target environment libraries
    FTargetCPU: string; //cpu for the target environment. Follows FPC names
    FTargetOS: string; //operating system for the target environment. Follows FPC names
  public
    // In your descendent, implement this function: you can download libraries or check for their existence for normal cross compile libs:
    function GetLibs(Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download libraries or check for their existence for Lazarus LCL cross compile libs:
    // Note: the libraries should be presumably under the basepath using the Lazarus naming convention??
    function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download cross compile binutils or check for their existence
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
  // Help ensure our implementers do the right thing with the variables
  // in their extensions
  FBinUtilsPath:='Error: cross compiler extension must set FBinUtilsPath: the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.';
  FBinUtilsPrefix:='Error: cross compiler extension must set FBinUtilsPrefix: can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it';
  FLibsPath:='Error: cross compiler extension must set FLibsPath: path for target environment libraries';
  FTargetCPU:='Error: cross compiler extension must set FTargetCPU: cpu for the target environment. Follows FPC names.';
  FTargetOS:='Error: cross compiler extension must set FTargetOS: operating system for the target environment. Follows FPC names';
end;

destructor TCrossInstaller.Destroy;
begin
  inherited Destroy;
end;

finalization
if assigned(CrossInstallers) then
  CrossInstallers.Destroy;
end.

