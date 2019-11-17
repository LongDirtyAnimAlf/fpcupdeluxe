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
var
  IOS_BASE:string;
begin
  result:=FLibsFound;
  if result then exit;

  FLibsPath:='';
  result:=true;
  FLibsFound:=true;

  IOS_BASE:='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';
  if NOT DirectoryExists(IOS_BASE) then
     IOS_BASE:='/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';

  if DirectoryExists(IOS_BASE) then
  begin
    FLibsPath:=IncludeTrailingPathDelimiter(IOS_BASE)+'usr/lib/';
    //FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    //'-XR'+ExcludeTrailingPathDelimiter(IOS_BASE);
    //'-Xr'+IncludeTrailingPathDelimiter(FLibsPath); //set linker's rlink path
    //'-Xr'+IncludeTrailingPathDelimiter(IOS_BASE); //set linker's rlink path
    //'-Xr'; //set linker's rlink path
  end;
end;

function TDarwinaarch64.GetBinUtils(Basepath:string): boolean;
var
  IOS_BASE:string;
  aOption:string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix
  result:=true;
  FBinsFound:=true;

  IOS_BASE:='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';
  if NOT DirectoryExists(IOS_BASE) then
     IOS_BASE:='/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';

  if DirectoryExists(IOS_BASE) then
  begin
    FBinUtilsPath:=IncludeTrailingPathDelimiter(IOS_BASE)+'usr/bin';
    AddFPCCFGSnippet('-FD'+FBinUtilsPath);{search this directory for compiler utilities}
    AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(IOS_BASE));
  end;

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
end;

constructor TDarwinaarch64.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwinAny';
  FTargetCPU:='aarch64';
  FTargetOS:='darwin';
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

