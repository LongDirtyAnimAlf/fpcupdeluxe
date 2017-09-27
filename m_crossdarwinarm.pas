unit m_crossdarwinarm;

{ Cross compiles from Darwin to Darwin arm
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  LazFileUtils;

type

{ TDarwinarm }

TDarwinarm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwinarm }

function TDarwinarm.GetLibs(Basepath:string): boolean;
const
  LibName='libc.dylib';
var
  IOS_BASE:string;
  s:string;
begin
  result:=FLibsFound;
  if result then exit;

  IOS_BASE:='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';
  if NOT DirectoryExists(IOS_BASE) then
     IOS_BASE:='/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';

  if not result then
    result:=SearchLibrary(IncludeTrailingPathDelimiter(IOS_BASE)+'usr'+DirectorySeparator+'lib',LibName);
  if not result then
    result:=SearchLibrary(IncludeTrailingPathDelimiter(IOS_BASE)+'usr'+DirectorySeparator+'lib','libc.tbd');

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath);

    s:=IncludeTrailingPathDelimiter(FLibsPath)+'..'+DirectorySeparator+'..'+DirectorySeparator;
    s:=ResolveDots(s);
    s:=ExcludeTrailingBackslash(s);
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+'System'+DirectorySeparator+LineEnding+
    '-k-framework'+LineEnding+
    '-kFoundation'+LineEnding+
    '-k-framework'+LineEnding+
    '-kCoreFoundation'+LineEnding+
    // -XRx is needed for fpc : prepend <x> to all linker search paths
    //'-XR'+ExcludeTrailingPathDelimiter(Basepath);
    '-Xd'+LineEnding+
    '-XR'+s;

    //FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    //'-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
  end;

(*
  if DirectoryExists(IOS_BASE) then
  begin
    FLibsPath:=IncludeTrailingPathDelimiter(IOS_BASE)+'usr/lib/';
    //FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    //'-XR'+ExcludeTrailingPathDelimiter(IOS_BASE);
    //'-Xr'+IncludeTrailingPathDelimiter(FLibsPath); //set linker's rlink path
    //'-Xr'+IncludeTrailingPathDelimiter(IOS_BASE); //set linker's rlink path
    //'-Xr'; //set linker's rlink path
  end;
  *)
end;

function TDarwinarm.GetBinUtils(Basepath:string): boolean;
var
  IOS_BASE:string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;
  (*
  IOS_BASE:='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';
  if NOT DirectoryExists(IOS_BASE) then
     IOS_BASE:='/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';

  if DirectoryExists(IOS_BASE) then
  begin
    //SearchBinUtilsInfo(result);
    //FBinUtilsPath:=IncludeTrailingPathDelimiter(IOS_BASE)+'usr/bin';
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    //'-FD'+FBinUtilsPath+LineEnding+ {search this directory for compiler utilities}
    '-XR'+ExcludeTrailingPathDelimiter(IOS_BASE);
  end;
  *)



end;

constructor TDarwinarm.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwinAny';
  FTargetCPU:='arm';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  FFPCCFGSnippet:=''; //no need to change fpc.cfg
  ShowInfo;
end;

destructor TDarwinarm.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
var
  Darwinarm:TDarwinarm;

initialization
  Darwinarm:=TDarwinarm.Create;
  RegisterExtension(Darwinarm.TargetCPU+'-'+Darwinarm.TargetOS,Darwinarm);
finalization
  Darwinarm.Destroy;
{$ENDIF}
end.

