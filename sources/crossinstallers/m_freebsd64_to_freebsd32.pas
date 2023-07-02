unit m_freebsd64_to_freebsd32;
{ Cross compiles from FreeBSD x64 to FreeBSD x86
Needed ports/packages:
- to do: default available with pc bsd; need to find out for freebsd
perhaps something like
cd /usr/src && make build32 install32 && ldconfig -v -m -R /usr/lib32
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

type

{ TFreeBSD64_FreeBSD386 }

TFreeBSD64_FreeBSD386 = class(TCrossInstaller)
private

public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TFreeBSD64_FreeBSD386 }

function TFreeBSD64_FreeBSD386.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  FLibsPath:='';

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // first search local paths based on libraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  //FLibsPath:='/usr/lib32';
  //result:=fileexists(FLibsPath+'/'+LibName);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    AddFPCCFGSnippet('-k-L',false);
    AddFPCCFGSnippet('-k'+IncludeTrailingPathDelimiter(LibsPath),false);
  end
  else ShowInfo('Searched but did not find 32bit libs in '+FLibsPath+'. Please install lib32 first !!');
end;

function TFreeBSD64_FreeBSD386.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:='';

  AsFile:=FBinUtilsPrefix+ASFILENAME;

  result:=SearchBinUtil(BasePath,AsFile);

  // Also allow for (cross)binutils (fpcupdeluxe scripts) with prefix
  if not result then
  begin
    BinPrefixTry:='i386-freebsd-';
    AsFile:=BinPrefixTry+ASFILENAME;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
  end;
end;

constructor TFreeBSD64_FreeBSD386.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.freebsd;
  Reset;
  ShowInfo;
end;

destructor TFreeBSD64_FreeBSD386.Destroy;
begin
  inherited Destroy;
end;

{$if defined(FREEBSD) AND defined(CPU64)}
var
  FreeBSD64_FreeBSD386:TFreeBSD64_FreeBSD386;

initialization
  FreeBSD64_FreeBSD386:=TFreeBSD64_FreeBSD386.Create;
  RegisterCrossCompiler(FreeBSD64_FreeBSD386.RegisterName,FreeBSD64_FreeBSD386);

finalization
  FreeBSD64_FreeBSD386.Destroy;
{$endif}
end.


