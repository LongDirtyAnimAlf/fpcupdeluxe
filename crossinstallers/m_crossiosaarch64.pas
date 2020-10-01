unit m_crossiosaarch64;

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
  SDKNAME='$SDK';
  iSDKNAME='iOS';
  macSDKNAME='macOS';

  SDKLOCATIONS:array[0..5] of string = (
    '/Applications/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/Desktop/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/Downloads/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '/Applications/Xcode-beta.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk'
  );

  TOOLCHAINLOCATIONS:array[0..5] of string = (
    '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/Desktop/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/Downloads/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '/Applications/Xcode-beta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain'
  );


type

{ TiOSaarch64 }

TiOSaarch64 = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TiOSaarch64 }

function TiOSaarch64.GetLibs(Basepath:string): boolean;
var
  aOption:string;
  i:integer;
begin
  result:=FLibsFound;
  if result then exit;

  FLibsPath:='';
  result:=false;
  FLibsFound:=false;

  if (NOT FLibsFound) then
  begin
    for FLibsPath in SDKLOCATIONS do
    begin
      FLibsPath:=ExpandFileName(FLibsPath);
      FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      if DirectoryExists(FLibsPath) then
      begin
        FLibsFound:=true;
        break;
      end;
    end;
  end;

  if (NOT FLibsFound) then
  begin
    for FLibsPath in SDKLOCATIONS do
    begin
      FLibsPath:=ExpandFileName(FLibsPath);
      FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      if DirectoryExists(FLibsPath) then
      begin
        FLibsFound:=true;
        break;
      end;
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

function TiOSaarch64.GetBinUtils(Basepath:string): boolean;
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
  // Update !!
  // Not valid anymore !!
  // aarch64 can be iOS or macOS
  {
  i:=StringListStartsWith(FCrossOpts,'-Ca');
  if i=-1 then
  begin
    aOption:='-CaAARCH64IOS';
    FCrossOpts.Add(aOption+' ');
    ShowInfo('Did not find any -Ca architecture parameter; using '+aOption+'.');
  end else aOption:=Trim(FCrossOpts[i]);
  AddFPCCFGSnippet(aOption);
  }

  aOption:=GetDarwinSDKVersion(LowerCase(iSDKNAME));
  if Length(aOption)>0 then AddFPCCFGSnippet('-WP'+aOption);

  aOption:=GetDarwinSDKVersion(LowerCase(macSDKNAME));
  if Length(aOption)>0 then AddFPCCFGSnippet('-WM'+aOption);

  // Never fail
  result:=true;
  FBinsFound:=true;
end;

constructor TiOSaarch64.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwinAny';
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.ios;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TiOSaarch64.Destroy;
begin
  inherited Destroy;
end;

{$ifdef Darwin}
var
  iOSaarch64:TiOSaarch64;

initialization
  iOSaarch64:=TiOSaarch64.Create;
  RegisterCrossCompiler(iOSaarch64.RegisterName,iOSaarch64);

finalization
  iOSaarch64.Destroy;
{$endif}

end.

