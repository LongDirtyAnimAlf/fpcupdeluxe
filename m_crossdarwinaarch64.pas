unit m_crossdarwinaarch64;

{ Cross compiles from Darwin to Darwin aarch64 (iphone)
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  fpcuputil;

const
  SDKNAME='iPhoneOS';

  SDKLOCATIONS:array[0..4] of string = (
    '/Applications/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/Desktop/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/Downloads/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk'
  );

  TOOLCHAINLOCATIONS:array[0..4] of string = (
    '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/Desktop/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/Downloads/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain'
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
var
  aOption:string;
  i:integer;
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
    {
    i:=StringListContains(FCrossOpts,'-isysroot');
    if i=-1 then
    begin
      aOption:='-ao"-isysroot '+ExcludeTrailingPathDelimiter(FLibsPath)+'"';
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find sysroot parameter; using '+aOption+'.');
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);
    }

    FLibsPath:=IncludeTrailingPathDelimiter(FLibsPath)+'usr/lib/';
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
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

  for FBinUtilsPath in TOOLCHAINLOCATIONS do
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

  aOption:=GetSDKVersion(LowerCase(SDKNAME));
  if Length(aOption)>0 then AddFPCCFGSnippet('-WP'+aOption);

  // Never fail
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwinaarch64.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwinAny';
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.darwin;
  Reset;
  FAlreadyWarned:=false;
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
  RegisterCrossCompiler(Darwinaarch64.RegisterName,Darwinaarch64);

finalization
  Darwinaarch64.Destroy;
{$ENDIF}
end.

