unit m_crossinstaller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpcuputil;

type

  CompilerType=(ctBootstrap,ctInstalled);

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  protected
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinutilsPathInPath: boolean;
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FCompilerUsed: CompilerType;
    FCrossModuleName: string; //used for identifying module to user in messages
    FCrossOpts: TStringList; //Options to be added to CROSSOPT by the calling code. XP= (binutils prefix) is already done, no need to add it
    FFPCCFGSnippet: string; //snippet to be added to fpc.cfg in order to find binutils/libraries etc
    FLibsPath: string; //path for target environment libraries
    FTargetCPU: string; //cpu for the target environment. Follows FPC names
    FTargetOS: string; //operating system for the target environment. Follows FPC names
    // Sets FBinutilspath if file LookFor found in Directory. Returns true if found.
    function SearchBinUtil(Directory, LookFor: string): boolean;
  public
    // In your descendent, implement this function: you can download libraries or check for their existence for normal cross compile libs:
    function GetLibs(Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download libraries or check for their existence for Lazarus LCL cross compile libs:
    // Note: the libraries should be presumably under the basepath using the Lazarus naming convention??
    function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;virtual; abstract;
    // In your descendent, implement this function: you can download cross compile binutils or check for their existence
    function GetBinUtils(Basepath:string):boolean;virtual; abstract;
    // Which compiler should be used for cross compilation.
    // Normally the bootstrap compiler, but cross compilers may need the installed compiler
    // (often a trunk version, though there's no tests yet that check trunk is installed)
    property CompilerUsed: CompilerType read FCompilerUsed;
    // Represents arguments for CROSSOPT parameter
    // No need to add XP= (binutils prefix): calling code will do this
    // CROSSOPT: Makefile of compiler/ allows to specify compiler options that are only used during the actual crosscompiling phase (i.e. not during the initial bootstrap cycle)
    property CrossOpts: TStringList read FCrossOpts;
    // Conditional define snippet for fpc.cfg used to specify library locations etc
    // Can be empty
    // Does not include the #IFDEF CPU<x> and #ENDIF parts where the target cpu is filled in
    property FPCCFGSnippet: string read FFPCCFGSnippet;
    // Path where libraries used for target systems are. May be empty if not needed.
    property LibsPath:string read FLibsPath;
    // Path where binutils used for target systems are. May be empty if not used.
    property BinUtilsPath:string read FBinUtilsPath;
    // Indicates if binutils directory is used as the last entry in PATH when cross compiling.
    // Can be useful if make scripts forget to include the complete path to the binutils path
    // (e.g. some versions of the DOS crosscompiler)
    property BinUtilsPathInPath: boolean read FBinutilsPathInPath;
    // Prefix used before executable names for binutils (e.g. before as.exe). May be empty.
    property BinUtilsPrefix:string read FBinUtilsPrefix;
    // Target processor (in FPC notation). Used to select cross compiler
    property TargetCPU:string read FTargetCPU;
    // Target Operating System (in FPC notation). Used to select cross compiler
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

function TCrossInstaller.SearchBinUtil(Directory, LookFor: string): boolean;
begin
  FBinUtilsPath:=ExcludeTrailingPathDelimiter(ExpandFileName(Directory));
  result:=FileExists(IncludeTrailingPathDelimiter(FBinUtilsPath)+LookFor);
  if not result then
    infoln(FCrossModuleName + ': failed: searching binutil '+LookFor+
      ' in directory '+FBinUtilsPath, etInfo);
end;

constructor TCrossInstaller.Create;
begin
  // Help ensure our implementers do the right thing with the variables
  // in their extensions
  FCompilerUsed:=ctBootstrap; //use bootstrap compiler for cross compiling by default
  FBinUtilsPath:='Error: cross compiler extension must set FBinUtilsPath: the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.';
  FBinutilsPathInPath:=false; //don't add binutils directory to path when cross compiling
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

