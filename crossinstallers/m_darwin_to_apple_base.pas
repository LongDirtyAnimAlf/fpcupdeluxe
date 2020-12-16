unit m_darwin_to_apple_base;

{ Cross compiles from Darwin to Darwin
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

type
  Tdarwin_apple = class(TCrossInstaller)
  protected
    FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  fpcuputil;

const
  SDKNAME='$SDK';
  iSDKNAME='iOS';
  macSDKNAME='macOS';
  simSDKNAME='iPhoneSimulator';

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

{ Tdarwin_apple }

function Tdarwin_apple.GetLibs(Basepath:string): boolean;
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
    if (TargetOS=TOS.iphonesim) then
    begin
      for FLibsPath in SDKLOCATIONS do
      begin
        FLibsPath:=ExpandFileName(FLibsPath);
        FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        if DirectoryExists(FLibsPath) then
        begin
          FLibsFound:=true;
          break;
        end;
      end;
    end;
  end;


  if (NOT FLibsFound) then
  begin
    if (TargetOS=TOS.ios) then
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
  end;

  if (NOT FLibsFound) then
  begin
    if (TargetOS=TOS.darwin) then
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
  end;

  if (NOT FLibsFound) then
  begin
    FLibsPath:=GetDarwinSDKLocation;
    if (Length(FLibsPath)>0) AND (DirectoryExists(FLibsPath)) then
      FLibsFound:=true;
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

    AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(FLibsPath));
    FLibsPath:=IncludeTrailingPathDelimiter(FLibsPath)+'usr/lib/';
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
  end else FLibsPath:='';

  if (TargetOS in [TOS.ios,TOS.iphonesim]) then
  begin
    aOption:=GetDarwinSDKVersion(LowerCase(iSDKNAME));
    if Length(aOption)>0 then AddFPCCFGSnippet('-WP'+aOption);
  end;

  if (TargetOS=TOS.darwin) then
  begin
    aOption:=GetDarwinSDKVersion(LowerCase(macSDKNAME));
    if Length(aOption)>0 then AddFPCCFGSnippet('-WM'+aOption);
  end;

  // Never fail.
  result:=true;
  FLibsFound:=true;
end;

function Tdarwin_apple.GetBinUtils(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix

  result:=false;
  FBinsFound:=false;

  if (NOT FBinsFound) then
  begin
    for FBinUtilsPath in TOOLCHAINLOCATIONS do
    begin
      FBinUtilsPath:=ExpandFileName(FBinUtilsPath);
      if DirectoryExists(FBinUtilsPath) then
      begin
        FBinsFound:=true;
        break;
      end;
    end;
  end;

  if (NOT FBinsFound) then
  begin
    FBinUtilsPath:=GetDarwinToolsLocation;
    if (Length(FBinUtilsPath)>0) AND (DirectoryExists(FBinUtilsPath)) then
    begin
      FBinsFound:=true;
    end;
  end;

  if FBinsFound then
  begin
    FBinUtilsPath:=IncludeTrailingPathDelimiter(FBinUtilsPath)+'usr/bin';
    AddFPCCFGSnippet('-FD'+FBinUtilsPath);{search this directory for compiler utilities}
  end else FBinUtilsPath:='';

  // Never fail
  result:=true;
  FBinsFound:=true;
end;

constructor Tdarwin_apple.Create;
begin
  inherited Create;
  FBinutilsPathInPath:=true;
  FAlreadyWarned:=false;
end;

destructor Tdarwin_apple.Destroy;
begin
  inherited Destroy;
end;

end.

