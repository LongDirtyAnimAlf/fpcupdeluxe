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
  iSDKNAME='iPhoneOS';
  macSDKNAME='macOSX';
  simSDKNAME='iPhoneSimulator';

  LIBSLOCATIONSBASEPATH    = SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk';
  TOOLSLOCATIONSBASEPATH   = SDKNAME+'.platform/Developer/usr/bin';

  SDKLOCATIONS:array[0..5] of string = (
    '/Applications/Xcode.app/Contents/Developer/Platforms/',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/',
    '~/Desktop/Xcode.app/Contents/Developer/Platforms/',
    '~/Downloads/Xcode.app/Contents/Developer/Platforms/',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Platforms/',
    '/Applications/Xcode-beta.app/Contents/Developer/Platforms/'
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
begin
  result:=inherited;
  if result then exit;

  FLibsPath:='';
  result:=false;
  FLibsFound:=false;

  if (TargetOS in [TOS.darwin,TOS.ios,TOS.iphonesim]) then
  begin

    if (NOT FLibsFound) then
    begin
      FLibsPath:=SDKNAME+'.sdk';
      case TargetOS of
        TOS.iphonesim : FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.ios       : FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.darwin    : FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      end;
      if RightStr(ExcludeTrailingPathDelimiter(Basepath),Length(FLibsPath))=FLibsPath then
      begin
        if DirectoryExists(Basepath) then
        begin
          FLibsPath:=ExcludeTrailingPathDelimiter(Basepath);
          FLibsFound:=true;
        end;
      end;
    end;

    if (NOT FLibsFound) then
    begin
      FLibsPath:=Basepath;
      FLibsPath:=ConcatPaths([FLibsPath,'Xcode.app','Contents','Developer','Platforms',LIBSLOCATIONSBASEPATH]);
      case TargetOS of
        TOS.iphonesim : FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.ios       : FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.darwin    : FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      end;
      if DirectoryExists(FLibsPath) then
        FLibsFound:=true;
    end;

    if (NOT FLibsFound) then
    begin
      FLibsPath:=Basepath;
      FLibsPath:=ConcatPaths([FLibsPath,'Contents','Developer','Platforms',LIBSLOCATIONSBASEPATH]);
      case TargetOS of
        TOS.iphonesim : FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.ios       : FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.darwin    : FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      end;
      if DirectoryExists(FLibsPath) then
        FLibsFound:=true;
    end;

    if (NOT FLibsFound) then
    begin
      for FLibsPath in SDKLOCATIONS do
      begin
        FLibsPath:=ExpandFileName(FLibsPath)+LIBSLOCATIONSBASEPATH;
        case TargetOS of
          TOS.iphonesim : FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
          TOS.ios       : FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
          TOS.darwin    : FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        end;
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
      FLibsPath:=GetDarwinSDKLocation;
      if (Length(FLibsPath)>0) AND (DirectoryExists(FLibsPath)) then
        FLibsFound:=true;
    end;
  end;

  if (NOT FLibsFound) then
  begin
    FLibsPath:=GetXCodeLocation;
    FLibsPath:=FLibsPath+DirectorySeparator+'Platforms'+DirectorySeparator+LIBSLOCATIONSBASEPATH;
    case TargetOS of
      TOS.iphonesim : FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.ios       : FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.darwin    : FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
    end;
    if DirectoryExists(FLibsPath) then
      FLibsFound:=true;
  end;

  if (NOT FLibsFound) then
  begin
    FLibsPath:=GetXCodeLocation;
    FLibsPath:=FLibsPath+DirectorySeparator+'SDKs'+DirectorySeparator+SDKNAME+'.sdk';
    case TargetOS of
      TOS.iphonesim : FLibsPath:=StringReplace(FLibsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.ios       : FLibsPath:=StringReplace(FLibsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.darwin    : FLibsPath:=StringReplace(FLibsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
    end;
    if DirectoryExists(FLibsPath) then
      FLibsFound:=true;
  end;

  if FLibsFound then
  begin

    SearchLibraryInfo(true);

    {
    // Prevent using default search path
    if (TargetOS in [TOS.ios,TOS.iphonesim]) then
    begin
      aOption:='-Xd';
      AddFPCCFGSnippet(aOption);
    end;
    }

    // Add linker search path
    aOption:='-XR'+LibsPath;
    AddFPCCFGSnippet(aOption);

    // Add library path to be sure ...
    FLibsPath:=IncludeTrailingPathDelimiter(FLibsPath)+'usr'+DirectorySeparator+'lib';
    aOption:='-Fl'+LibsPath;
    AddFPCCFGSnippet(aOption);

    // Add library path when cross-compiling to be sure ...
    aOption:='-Fl'+FLibsPath+DirectorySeparator+'system';
    AddCrossOption(aOption);

  end else FLibsPath:='';

  // Add minimum version to prevent linker failures if needed.

  if (((TargetOS=TOS.ios) OR (TargetOS=TOS.darwin)) AND (TargetCPU=TCPU.arm)) then
  begin
    aOption:=GetDarwinSDKVersion(LowerCase(iSDKNAME));
    if Length(aOption)>0 then
    begin
      if CompareVersionStrings(aOption,'6.0')>=0 then
      begin
        aOption:='6.0';
      end;
      AddFPCCFGSnippet('-WP'+aOption);
    end;
  end;

  if (TargetOS=TOS.iphonesim) then
  begin
    aOption:=GetDarwinSDKVersion(LowerCase(simSDKNAME));
    if Length(aOption)=0 then aOption:=GetDarwinSDKVersion(LowerCase(iSDKNAME));
    if Length(aOption)>0 then
    begin
      if CompareVersionStrings(aOption,'8.1')>=0 then
      begin
        aOption:='8.1';
      end;
      AddFPCCFGSnippet('-WP'+aOption);
    end;
  end;

  if (TargetOS=TOS.darwin) then
  begin
    aOption:=GetDarwinSDKVersion(LowerCase(macSDKNAME));
    if Length(aOption)>0 then
    begin
      if CompareVersionStrings(aOption,'10.8')>=0 then
      begin
        aOption:='10.8';
      end;
      AddFPCCFGSnippet('-WM'+aOption);
    end;
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
    FBinUtilsPath:=Basepath;
    FBinUtilsPath:=ConcatPaths([FBinUtilsPath,'Xcode.app','Contents','Developer','Platforms',TOOLSLOCATIONSBASEPATH]);
    case TargetOS of
      TOS.iphonesim : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.ios       : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.darwin    : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
    end;
    if DirectoryExists(FBinUtilsPath) then
      FBinsFound:=true;
  end;

  if (NOT FBinsFound) then
  begin
    FBinUtilsPath:=Basepath;
    FBinUtilsPath:=ConcatPaths([FBinUtilsPath,'Xcode.app','Contents','Developer','Toolchains','XcodeDefault.xctoolchain','usr','bin']);
    if DirectoryExists(FBinUtilsPath) then
      FBinsFound:=true;
  end;

  if (NOT FBinsFound) then
  begin
    for FBinUtilsPath in SDKLOCATIONS do
    begin
      FBinUtilsPath:=ExpandFileName(FBinUtilsPath)+TOOLSLOCATIONSBASEPATH;
      case TargetOS of
        TOS.iphonesim : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.ios       : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
        TOS.darwin    : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      end;
      if DirectoryExists(FBinUtilsPath) then
      begin
        FBinsFound:=true;
        break;
      end;
    end;
  end;

  if (NOT FBinsFound) then
  begin
    FBinUtilsPath:=GetXCodeLocation;
    FBinUtilsPath:=FBinUtilsPath+DirectorySeparator+'Platforms'+DirectorySeparator+TOOLSLOCATIONSBASEPATH;
    case TargetOS of
      TOS.iphonesim : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,simSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.ios       : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,iSDKNAME,[rfReplaceAll, rfIgnoreCase]);
      TOS.darwin    : FBinUtilsPath:=StringReplace(FBinUtilsPath,SDKNAME,macSDKNAME,[rfReplaceAll, rfIgnoreCase]);
    end;
    if DirectoryExists(FBinUtilsPath) then
      FBinsFound:=true;
  end;

  if (NOT FBinsFound) then
  begin
    FBinUtilsPath:=GetDarwinToolsLocation;
    if (Length(FBinUtilsPath)>0) AND (DirectoryExists(FBinUtilsPath)) then
    begin
      FBinsFound:=true;
    end;
  end;

  if (NOT FBinsFound) then
  begin
    for FBinUtilsPath in TOOLCHAINLOCATIONS do
    begin
      FBinUtilsPath:=ExpandFileName(FBinUtilsPath)+'/usr/bin';
      if DirectoryExists(FBinUtilsPath) then
      begin
        FBinsFound:=true;
        break;
      end;
    end;
  end;

  if FBinsFound then
  begin
    SearchBinUtilsInfo(true);
    // Set this directory for compiler utilities
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
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

