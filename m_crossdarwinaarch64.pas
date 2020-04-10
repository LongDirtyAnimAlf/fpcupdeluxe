unit m_crossdarwinaarch64;

{ Cross compiles from Darwin to Darwin aarch64
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

{ TDarwinaarch64 }

TDarwinaarch64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwinaarch64 }

function TDarwinaarch64.GetLibs(Basepath:string): boolean;
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
    //AddFPCCFGSnippet('-Xr'); //set linker's rlink path
    //AddFPCCFGSnippet('-Xr'+IncludeTrailingPathDelimiter(FLibsPath)); //set linker's rlink path
    FLibsPath:=IncludeTrailingPathDelimiter(FLibsPath)+'usr/lib/';
    //AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(FLibsPath));
    //AddFPCCFGSnippet('-Xr'+IncludeTrailingPathDelimiter(FLibsPath)); //set linker's rlink path
  end else FLibsPath:='';

  // Never fail.
  result:=true;
  FLibsFound:=true;
end;

function TDarwinaarch64.GetBinUtils(Basepath:string): boolean;
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

  // Set some defaults if user hasn't specified otherwise
  i:=StringListStartsWith(FCrossOpts,'-Ca');
  if i=-1 then
  begin
    aOption:='-CaAARCH64IOS';
    FCrossOpts.Add(aOption+' ');
    ShowInfo('Did not find any -Ca architecture parameter; using '+aOption+'.');
  end else aOption:=Trim(FCrossOpts[i]);
  AddFPCCFGSnippet(aOption);

  aOption:=GetSDKVersion('iphoneos');
  if Length(aOption)>0 then AddFPCCFGSnippet('-WP'+aOption);

  // Never fail
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwinaarch64.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwinAny';
  FTargetCPU:=GetCPU(TCPU.aarch64);
  FTargetOS:=GetOS(TOS.darwin);
  FAlreadyWarned:=false;
  FFPCCFGSnippet:='';
  ShowInfo;
end;

destructor TDarwinaarch64.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}

var
  Darwinaarch64:TDarwinaarch64;

initialization
  Darwinaarch64:=TDarwinaarch64.Create;
  RegisterExtension(Darwinaarch64.TargetCPU+'-'+Darwinaarch64.TargetOS,Darwinaarch64);
finalization
  Darwinaarch64.Destroy;
{$ENDIF}
end.

