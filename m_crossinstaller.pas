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
    FBinUtilsPrefix: string;
    FLibsPath: string;
    FTargetCPU: string;
    FTargetOS: string;
    procedure SetBinUtilsPath(AValue: string);
    procedure SetBinUtilsPrefix(AValue: string);
    procedure SetLibsPath(AValue: string);
    procedure SetTargetCPU(AValue: string);
    procedure SetTargetOS(AValue: string);
  public
    function GetLibs:boolean;virtual; abstract; //Used to place required libraries for target system
    function GetBinUtils:boolean;virtual; abstract; //Used to place required binutils for target system
    property LibsPath:string read FLibsPath write SetLibsPath; //Returns library path for target system
    property BinUtilsPath:string read FBinUtilsPath write SetBinUtilsPath; //Returns binutils (make etc) path for target system
    property BinUtilsPrefix:string read FBinUtilsPrefix write SetBinUtilsPrefix; //Returns prefix used if storing binutils in a common directory
    property TargetCPU:string read FTargetCPU write SetTargetCPU; //Target CPU according to FPC Makefile format
    property TargetOS:string read FTargetOS write SetTargetOS; //Target Operating System according to FPC Makefile format
    constructor Create;
    destructor Destroy; override;
  end;
  TCrossInstallerClass= class of TCrossInstaller;

  { TCrossInstallerDef }
  // Used to keep information for TCrossInstaller descendants
  // used in registration
  TCrossInstallerDef=Class(TPersistent)
    Class Function CrossInstallerName: String; virtual;
    Class Function CrossInstallerClass: TCrossInstallerClass; virtual;
    Class Function TargetCPU: String; virtual;
    Class Function TargetName: String; virtual;
  end;
  TCrossInstallerDefClass = class of TCrossInstallerDef;

  // Used for registering and managing list of possible CrossInstallers via CrossInstallerDefs
  Procedure RegisterCrossInstaller(Def:TCrossInstallerDefClass);
  Function GetCrossInstallerDef(CrossInstallerName : String) : TCrossInstallerDef;
  Procedure GetCrossInstallerList(List : TSTrings);

implementation
uses
  m_crosswin64;
  //Implementers: add new cross compiler units here

{ TCrossInstaller }
procedure TCrossInstaller.SetBinUtilsPath(AValue: string);
begin
  raise Exception.Create('Cross compiler extension must implement SetBinUtilsPath');
end;

procedure TCrossInstaller.SetBinUtilsPrefix(AValue: string);
begin
  raise Exception.Create('Cross compiler extension must implement SetBinUtilsPrefix');
end;

procedure TCrossInstaller.SetLibsPath(AValue: string);
begin
  raise Exception.Create('Cross compiler extension must implement SetLibsPath');
end;

procedure TCrossInstaller.SetTargetCPU(AValue: string);
begin
  raise Exception.Create('Cross compiler extension must implement SetTargetCPU');
end;

procedure TCrossInstaller.SetTargetOS(AValue: string);
begin
  raise Exception.Create('Cross compiler extension must implement SetTargetOS');
end;


constructor TCrossInstaller.Create;
begin
  // Help ensure our implementers do the right thing with the variables
  // in their extensions
  FBinUtilsPath:='Error: cross compiler extension must set FBinUtilsPath';
  FBinUtilsPrefix:='Error: cross compiler extension must set FBinUtilsPrefix';
  FLibsPath:='Error: cross compiler extension must set FLibsPath';
  FTargetCPU:='Error: cross compiler extension must set FTargetCPU';
  FTargetOS:='Error: cross compiler extension must set FTargetOS';
end;

destructor TCrossInstaller.Destroy;
begin
  inherited Destroy;
end;


{ TCrossInstallerDef }
class function TCrossInstallerDef.CrossInstallerName: String;
begin
  Result:='Error, implement in extension!'; //Must implement in descendant!
end;

class function TCrossInstallerDef.CrossInstallerClass: TCrossInstallerClass;
begin
  Result:=Nil; //Must implement in descendant; use class of <yourinstaller>def

end;

class function TCrossInstallerDef.TargetCPU: String;
begin
  Result:='Error, implement in extension!'; //Must implement in descendant; see FPC Makelist for options
end;

class function TCrossInstallerDef.TargetName: String;
begin
  Result:='Error, implement in extension!'; //Must implement in descendant; see FPC Makelist for options
end;


{ CrossInstaller extension list management }
Var
  CrossDefs : TStringList;

Procedure CheckDefs;
begin
  If (CrossDefs=Nil) then
    begin
    CrossDefs:=TStringList.Create;
    CrossDefs.Sorted:=True;
    CrossDefs.Duplicates:=dupError;
    end;
end;

Procedure DoneDefs;
var
  I : Integer;
begin
  If Assigned(CrossDefs) then
    begin
    For I:=CrossDefs.Count-1 downto 0 do
      begin
      CrossDefs.Objects[i].Free;
      CrossDefs.Delete(I);
      end;
    FreeAndNil(CrossDefs);
    end;
end;


Function GetCrossInstallerDef(CrossInstallerName : String) : TCrossInstallerDef;
var
  I : Integer;
begin
  CheckDefs;
  I:=CrossDefs.IndexOf(CrossInstallerName);
  If (I<>-1) then
    Result:=TCrossInstallerDef(CrossDefs.Objects[i])
  else
    Result:=Nil;
end;

procedure RegisterCrossInstaller(Def: TCrossInstallerDefClass);
var
  I : Integer;
begin
  CheckDefs;
  I:=CrossDefs.IndexOf(Def.CrossInstallerName);
  If (I=-1) then
    CrossDefs.AddObject(Def.CrossInstallerName,Def.Create)
  else
    begin
    // Replace existing...
    CrossDefs.Objects[I].Free;
    CrossDefs.Objects[I]:=Def.Create;
    end;
end;

procedure GetCrossInstallerList(List: TSTrings);
begin
  CheckDefs;
  List.Text:=CrossDefs.Text;
end;

end.

