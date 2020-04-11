unit m_crossdarwinarm;

{ Cross compiles from Darwin to Darwin arm (iphone)
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  fpcuputil;

const
  SDKLOCATIONS:array[0..4] of string = (
    '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk',
    '~/Desktop/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk',
    '~/Downloads/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk'
  );

type

{ TDarwinARM }

TDarwinARM = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwinARM }

function TDarwinARM.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;

  FLibsPath:='';
  result:=false;
  FLibsFound:=false;

  for FLibsPath in SDKLOCATIONS do
  begin
    FLibsPath:=ExpandFileName(FLibsPath);
    if DirectoryExists(FLibsPath) then
    begin
      FLibsFound:=true;
      break;
    end;
  end;

  if FLibsFound then
  begin
    AddFPCCFGSnippet('-ao"-isysroot '+ExcludeTrailingPathDelimiter(FLibsPath)+'"');
    FLibsPath:=IncludeTrailingPathDelimiter(FLibsPath)+'usr/lib/';
  end else FLibsPath:='';

  // Never fail.
  result:=true;
  FLibsFound:=true;
end;

function TDarwinARM.GetBinUtils(Basepath:string): boolean;
var
  aOption:string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=false;
  FBinsFound:=false;

  for FBinUtilsPath in SDKLOCATIONS do
  begin
    FBinUtilsPath:=ExpandFileName(FBinUtilsPath);
    if DirectoryExists(FBinUtilsPath) then
    begin
      FBinsFound:=true;
      break;
    end;
  end;

  if FBinsFound then
  begin
    AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(FBinUtilsPath));
    FBinUtilsPath:=IncludeTrailingPathDelimiter(FBinUtilsPath)+'usr/bin';
    AddFPCCFGSnippet('-FD'+FBinUtilsPath);{search this directory for compiler utilities}
  end else FBinUtilsPath:='';

  aOption:=GetSDKVersion('iphoneos');
  if Length(aOption)>0 then AddFPCCFGSnippet('-WP'+aOption);

  // Never fail
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwinARM.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwinAny';
  FTargetCPU:=GetCPU(TCPU.arm);
  FTargetOS:=GetOS(TOS.darwin);
  FAlreadyWarned:=false;
  FFPCCFGSnippet:='';
  ShowInfo;
end;

destructor TDarwinARM.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
var
  DarwinARM:TDarwinARM;

initialization
  DarwinARM:=TDarwinARM.Create;
  RegisterExtension(DarwinARM.TargetCPU+'-'+DarwinARM.TargetOS,DarwinARM);

finalization
  DarwinARM.Destroy;
{$ENDIF}
end.

