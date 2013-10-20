unit m_crossinstaller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpcuputil;

type

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  protected
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FCrossModuleName: string; //used for identifying module to user in messages
    FCrossOpts: TStringList; //Options to be added to CROSSOPT by the calling code. XP= (binutils prefix) is already done, no need to add it
    FFPCCFGSnippet: string; //snippet to be added to fpc.cfg in order to find binutils/libraries etc
    FLibsPath: string; //path for target environment libraries
    FTargetCPU: string; //cpu for the target environment. Follows FPC names
    FTargetOS: string; //operating system for the target environment. Follows FPC names
    // Sets FBinutilspath if AsFile found in Directory. Returns true if found.
    function SearchBinUtil(Directory, AsFile: string): boolean;
  public
    // In your descendent, implement this function: you can download libraries or check for their existence for normal cross compile libs:
    function GetLibs(Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download libraries or check for their existence for Lazarus LCL cross compile libs:
    // Note: the libraries should be presumably under the basepath using the Lazarus naming convention??
    function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download cross compile binutils or check for their existence
    function GetBinUtils(Basepath:string):boolean;virtual; abstract;
    // Represents arguments for CROSSOPT parameter
    // No need to add XP= (binutils prefix): calling code will do this
    // CROSSOPT: Makefile of compiler/ allows to specify compiler options that are only used during the actual crosscompiling phase (i.e. not during the initial bootstrap cycle)
    property CrossOpts: TStringList read FCrossOpts;
    // Conditional define snippet for fpc.cfg used to specify library locations etc
    // Can be empty
    // Does not include the #IFDEF CPU<x> and #ENDIF parts where the target cpu is filled in
    property FPCCFGSnippet: string read FFPCCFGSnippet;
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

function TCrossInstaller.SearchBinUtil(Directory, AsFile: string): boolean;
begin
  FBinUtilsPath:=ExcludeTrailingPathDelimiter(ExpandFileName(Directory));
  result:=FileExists(IncludeTrailingPathDelimiter(FBinUtilsPath)+AsFile);
  if not result then
    infoln(FCrossModuleName + ': failed: searching binutil '+AsFile+
      ' in directory '+FBinUtilsPath, etInfo);
end;

constructor TCrossInstaller.Create;
begin
  // Help ensure our implementers do the right thing with the variables
  // in their extensions
  FBinUtilsPath:='Error: cross compiler extension must set FBinUtilsPath: the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.';
  FBinUtilsPrefix:='Error: cross compiler extension must set FBinUtilsPrefix: can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it';
  FCrossOpts:=TStringList.Create;
  FFPCCFGSnippet:='Error: cross compiler extension must set FFPCCFGSnippet: this can be quite short but must exist';
  FLibsPath:='Error: cross compiler extension must set FLibsPath: path for target environment libraries';
  FTargetCPU:='Error: cross compiler extension must set FTargetCPU: cpu for the target environment. Follows FPC names.';
  FTargetOS:='Error: cross compiler extension must set FTargetOS: operating system for the target environment. Follows FPC names';
end;

destructor TCrossInstaller.Destroy;
begin
  FCrossOpts.Free;
  inherited Destroy;
end;

finalization
if assigned(CrossInstallers) then
  CrossInstallers.Destroy;
end.

