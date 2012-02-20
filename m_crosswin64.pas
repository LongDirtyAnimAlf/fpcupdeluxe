unit m_crosswin64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

type
{ TWin32To64 }

  TWin32To64 = class(TCrossInstaller)
  private
    // You must set these variables:
    //FBinUtilsPath: string;
    //FBinUtilsPrefix: string;
    //FLibsPath: string;
    //FTargetCPU: string;
    //FTargetOS: string;
    // You must override/replace these
    // procedures in TCrossInstaller.
    procedure SetBinUtilsPath(AValue: string);
    procedure SetBinUtilsPrefix(AValue: string);
    procedure SetLibsPath(AValue: string);
    procedure SetTargetCPU(AValue: string);
    procedure SetTargetOS(AValue: string);
  public
    // You must override GetLibs and GetBinUtils
    function GetLibs:boolean;override;
    function GetBinUtils:boolean;override;
    property LibsPath:string read FLibsPath write SetLibsPath;
    property BinUtilsPath:string read FBinUtilsPath write SetBinUtilsPath;
    property BinUtilsPrefix:string read FBinUtilsPrefix write SetBinUtilsPrefix;
    property TargetCPU:string read FTargetCPU write SetTargetCPU;
    property TargetOS:string read FTargetOS write SetTargetOS;
    constructor Create;
    destructor Destroy; override;
  end;

  { TWin32ToWin64Def }
  // Used to keep information for TCrossInstaller descendants
  // used in registration
  TWin32ToWin64Def=Class(TCrossInstallerDef)
    Class Function CrossInstallerName: String; override;
    Class Function CrossInstallerClass: TCrossInstallerClass; override;
    Class Function TargetCPU: String; override;
    Class Function TargetName: String; override;
  end;
  TWin32ToWin64DefClass = class of TWin32ToWin64Def;

implementation

{ TWin32ToWin64Def }
// If using this for your own extension, modify these functions:
class function TWin32ToWin64Def.CrossInstallerName: String;
begin
  Result:='Win32 to Win64 cross compiler';
end;

class function TWin32ToWin64Def.CrossInstallerClass: TCrossInstallerClass;
begin
  Result:=TWin32To64;
end;

class function TWin32ToWin64Def.TargetCPU: String;
begin
  // Target processor for cross compiler
  // according to definitions in MAKEFILETARGETS in FPC MakeFile
  Result:='x86_64';
end;

class function TWin32ToWin64Def.TargetName: String;
begin
  // Target operating system for cross compiler
  // according to definitions in MAKEFILETARGETS in FPC MakeFile
  Result:='win64';
end;

procedure TWin32To64.SetBinUtilsPath(AValue: string);
begin
  if FBinUtilsPath=AValue then Exit;
  FBinUtilsPath:=AValue;
end;

procedure TWin32To64.SetBinUtilsPrefix(AValue: string);
begin
  if FBinUtilsPrefix=AValue then Exit;
  FBinUtilsPrefix:=AValue;
end;

procedure TWin32To64.SetLibsPath(AValue: string);
begin
  if FLibsPath=AValue then Exit;
  FLibsPath:=AValue;
end;

procedure TWin32To64.SetTargetCPU(AValue: string);
begin
  if FTargetCPU=AValue then Exit;
  FTargetCPU:=AValue;
end;

procedure TWin32To64.SetTargetOS(AValue: string);
begin
  if FTargetOS=AValue then Exit;
  FTargetOS:=AValue;
end;

function TWin32To64.GetLibs: boolean;
begin
  // Used to get required libraries
  // for cross compiler target and
  // put them in the libspath directory.

  // Not needed for Win64; FPC can use the win32 libs
end;

function TWin32To64.GetBinUtils: boolean;
begin
  // Used to get required binutils (as, ar, etc)
  // for cross compiler target and
  // put them in the binutilspath directory.

  // Not needed for Win64; FPC can use the win32 binutils!?!
end;

{ TWin32To64 }
constructor TWin32To64.Create;
begin
  inherited Create;
  FBinUtilsPath:=''; //Not needed for win64
  FBinUtilsPrefix:=''; //Not needed for win64
  FLibsPath:=''; //Not needed for win64
  FTargetCPU:='x86_64';
  FTargetOS:='win64';
end;

destructor TWin32To64.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterCrossInstaller(TWin32ToWin64Def);

end.

