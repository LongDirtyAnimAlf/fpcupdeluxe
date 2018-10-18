unit m_crossinstaller;
{
General crossinstaller/updater module

Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands
Copyright (C) 2015-2017 Alfred Glänzer

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpcuputil;

const
  ErrorNotFound='An error occurred getting cross compiling binutils/libraries.'+LineEnding+
    'todo: specify what exactly is missing';
  MAXDARWINVERSION=16;
  MINDARWINVERSION=10;
  MAXIOSVERSION=12;
  MINIOSVERSION=1;
  MAXDELPHIVERSION=22;
  MINDELPHIVERSION=12;
  NDKVERSIONNAMES:array[0..21] of string = ('7','7b','7c','8','8b','8c','8d','8e','9','9b','9c','9d','10','10b','10c','10d','10e','11','11b','11c','12','12b');
  //PLATFORMVERSIONSNUMBERS:array[0..13] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22); //23 does not yet work due to text allocations
  PLATFORMVERSIONSNUMBERS:array[0..17] of byte = (9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26);
  {$ifdef unix}
  UnixBinDirs :array[0..2] of string = ('/usr/local/bin','/usr/bin','/bin');
  UnixLibDirs :array[0..2] of string = ('/usr/local/lib','/usr/lib','/lib');
  {$endif}
  DEFAULTARMCPU = 'ARMV7A';

  NEWPASCALGITREPO='https://github.com/newpascal';
  FPCUPGITREPO=NEWPASCALGITREPO+'/fpcupdeluxe';

  BOOTSTRAPPERVERSION='bootstrappers_v1.0';
  FPCUPGITREPOBOOTSTRAPPER=FPCUPGITREPO+'/releases/download/'+BOOTSTRAPPERVERSION;
  FPCUPGITREPOAPI='https://api.github.com/repos/newpascal/fpcupdeluxe/releases';
  FPCUPGITREPOBOOTSTRAPPERAPI=FPCUPGITREPOAPI+'/tags/'+BOOTSTRAPPERVERSION;

  FPCUPPRIVATEGITREPO='https://www.consulab.nl/git/Alfred/FPCbootstrappers/raw/master';

type
  CompilerType=(ctBootstrap,ctInstalled);
  SearchMode=(smFPCUPOnly,smAuto,smManual);

  { TCrossInstaller }
  TCrossInstaller = class(TObject)
  private
    function GetCrossModuleName:string;
  protected
    FBinUtilsPath: string; //the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.
    FBinutilsPathInPath: boolean;
    FBinUtilsPrefix: string; //can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it
    FCompilerUsed: CompilerType;
    FSearchMode: SearchMode;
    FCrossModuleNamePrefix: string; //used for identifying module to user in messages
    FCrossOpts: TStringList; //Options to be added to CROSSOPT by the calling code. XP= (binutils prefix) is already done, no need to add it
    FFPCCFGSnippet: string; //snippet to be added to fpc.cfg in order to find binutils/libraries etc
    FLibsPath: string; //path for target environment libraries
    FTargetCPU: string; //cpu for the target environment. Follows FPC names
    FTargetOS: string; //operating system for the target environment. Follows FPC names
    FSubArch: string; //optional subarch for embedded targets
    FLibsFound,FBinsFound,FCrossOptsAdded:boolean;
    // Sets FBinutilspath if file LookFor found in Directory. Returns true if found.
    function SearchLibrary(Directory, LookFor: string): boolean;
    function SimpleSearchLibrary(BasePath,DirName: string; const LookFor:string): boolean;
    function SearchBinUtil(Directory, LookFor: string): boolean;
    function SimpleSearchBinUtil(BasePath,DirName: string; const LookFor:string): boolean;
    procedure SearchLibraryInfo(found:boolean; const extrainfo:string='');
    procedure SearchBinUtilsInfo(found:boolean; const extrainfo:string='');
    function SearchUtil(Directory, LookFor: string; LibsOrBins:boolean): boolean;
    function FPCUPToolsSearch(BasePath,DirName: string; LibsOrBins:boolean; const LookFor:string): boolean;
  public
    // In your descendent, implement this function: you can download libraries or check for their existence for normal cross compile libs:
    function GetLibs(Basepath:string):boolean;virtual; abstract;
    {$ifndef FPCONLY}
    // In your descendent, implement this function when needed: you can download libraries or check for their existence for Lazarus LCL cross compile libs:
    // Note: the libraries should be presumably under the basepath using the Lazarus naming convention??
    function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;virtual;
    {$endif}
    procedure AddFPCCFGSnippet(aSnip: string);
    // In your descendent, implement this function: you can download cross compile binutils or check for their existence
    function GetBinUtils(Basepath:string):boolean;virtual;
    // Parses space-delimited crossopt parameters and sets the CrossOpt property
    procedure SetCrossOpt(CrossOpts: string);
    // Pass subarch if any
    procedure SetSubArch(SubArch: string);
    procedure ShowInfo(info: string = ''; Level: TEventType = etInfo);
    // Reset some variables to default values
    procedure Reset; virtual;
    // Which compiler should be used for cross compilation.
    // Normally the bootstrap compiler, but cross compilers may need the installed compiler
    // (often a trunk version, though there's no tests yet that check trunk is installed)
    property CompilerUsed: CompilerType read FCompilerUsed;
    property SearchModeUsed: SearchMode read FSearchMode write FSearchMode;
    property CrossModuleName: string read GetCrossModuleName;
    // Represents arguments for CROSSOPT parameter
    // No need to add XP= (binutils prefix): calling code will do this
    // CROSSOPT: Compiler makefile allows to specify compiler options that are only used during the actual crosscompiling phase (i.e. not during the initial bootstrap cycle)
    // Also used in fpc.cfg snippet to set options when compiling for cross target
    property CrossOpt: TStringList read FCrossOpts;
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
    property SubArch:string read FSubArch;
    constructor Create;
    destructor Destroy; override;
  end;

Procedure RegisterExtension(Platform:string;Extension:TCrossInstaller);

Var
  CrossInstallers:TStringList=nil;

implementation

uses
  StrUtils,
  LazFileUtils;

{ TCrossInstaller }
procedure RegisterExtension(Platform:string;Extension:TCrossInstaller);
begin
  if not assigned(CrossInstallers) then
    CrossInstallers:=TStringList.Create;
  CrossInstallers.AddObject(Platform,TObject(Extension));
end;

function TCrossInstaller.GetCrossModuleName:string;
begin
  result:=FCrossModuleNamePrefix+'_'+TargetOS+'-'+TargetCPU;
end;

procedure TCrossInstaller.AddFPCCFGSnippet(aSnip: string);
var
  aSnippd:string;
  i:integer;
begin
  if Length(Trim(aSnip))=0 then exit;

  aSnippd:=StringReplace(aSnip,' ',LineEnding,[rfReplaceAll]);
  if (Pos(aSnippd,FFPCCFGSnippet)>0) then exit;

  if Length(FPCCFGSnippet)>0 then
  begin
    if RPos(LineEnding,FFPCCFGSnippet)<Length(FFPCCFGSnippet) then FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding;
    FFPCCFGSnippet:=FFPCCFGSnippet+aSnippd;
  end
  else FFPCCFGSnippet:=aSnippd;

end;

procedure TCrossInstaller.SearchLibraryInfo(found:boolean; const extrainfo:string='');
begin
  if found then
    infoln(CrossModuleName + ': Found correct library in directory '+FLibsPath, etInfo)
  else
    infoln(CrossModuleName + ': Searched but did not find any library !!', etError);
  if Length(extrainfo)>0 then infoln(CrossModuleName + ' libs : '+extrainfo, etInfo);
end;

procedure TCrossInstaller.SearchBinUtilsInfo(found:boolean; const extrainfo:string='');
begin
  if found then
    infoln(CrossModuleName + ': Found correct binary utilities in directory '+FBinUtilsPath, etInfo)
  else
    infoln(CrossModuleName + ': Searched but did not find any binary utilities !!', etError);
  if Length(extrainfo)>0 then infoln(CrossModuleName + ' bins : '+extrainfo, etInfo);
end;


function TCrossInstaller.SearchLibrary(Directory, LookFor: string): boolean;
begin
  result:=SearchUtil(Directory, LookFor, true);
end;

function TCrossInstaller.SimpleSearchLibrary(BasePath,DirName: string; const LookFor:string): boolean;
begin
  result:=FPCUPToolsSearch(BasePath,DirName,true,LookFor);
end;

function TCrossInstaller.SearchBinUtil(Directory, LookFor: string): boolean;
begin
  result:=SearchUtil(Directory, LookFor, false);
end;

function TCrossInstaller.SimpleSearchBinUtil(BasePath,DirName: string; const LookFor:string): boolean;
begin
  result:=FPCUPToolsSearch(BasePath,DirName,false,LookFor);
end;

function TCrossInstaller.SearchUtil(Directory, LookFor: string; LibsOrBins:boolean): boolean;
var
  sd:string;
  info:string;
begin
  sd:=ExcludeTrailingPathDelimiter(SafeExpandFileName(Directory));
  if LibsOrBins
     then FLibsPath:=sd
     else FBinUtilsPath:=sd;
  if Length(LookFor)=0
     then result:=DirectoryExists(sd)
     else result:=FileExists(IncludeTrailingPathDelimiter(sd)+LookFor);

  // Report results to user. SearchBinUtil will probably only be called until
  // its result is true; if it's called more times, it still ok to keep the
  // user informed about succesful searches.

  if LibsOrBins
     then info:='library'
     else info:='binutil(s)';

  if result then
    infoln(CrossModuleName + ': found '+info+' '+LookFor+
      ' in directory '+sd, etDebug)
  else
    infoln(CrossModuleName + ': searched but did not find '+info+' '+LookFor+
      ' in directory '+sd, etDebug);
end;


function TCrossInstaller.FPCUPToolsSearch(BasePath,DirName: string; LibsOrBins:boolean; const LookFor:string): boolean;
var
  sd:string;
begin
  result:=false;

  if SearchModeUsed=smManual then exit;

  // first search local paths based on libraries provided for or adviced by fpc itself
  sd:=IncludeTrailingPathDelimiter(BasePath);
  if LibsOrBins
     then sd:=sd+'lib'
     else sd:=sd+'bin';
  if Length(DirName)>0 then sd:=sd+DirectorySeparator+DirName;
  sd:=SafeExpandFileName(sd);
  result:=SearchUtil(sd, LookFor, LibsOrBins);

  if not result then
  begin
    sd:=IncludeTrailingPathDelimiter(BasePath)+'cross'+DirectorySeparator;
    if LibsOrBins
       then sd:=sd+'lib'
       else sd:=sd+'bin';
    if Length(DirName)>0 then sd:=sd+DirectorySeparator+DirName;
    sd:=SafeExpandFileName(sd);
    result:=SearchUtil(sd, LookFor, LibsOrBins);
  end;

  if not result then
  begin
    sd:=SafeGetApplicationPath+'cross'+DirectorySeparator;
    if LibsOrBins
       then sd:=sd+'lib'
       else sd:=sd+'bin';
    if Length(DirName)>0 then sd:=sd+DirectorySeparator+DirName;
    sd:=SafeExpandFileName(sd);
    result:=SearchUtil(sd, LookFor, LibsOrBins);
  end;

  {$IFDEF UNIX}
  if (SearchModeUsed=smAuto) then
  begin
    if LibsOrBins
       then sd:='lib'
       else sd:='bin';

    if not result then
      if Length(DirName)>0 then result:=SearchUtil('/usr/local/'+sd+'/'+DirName,
        LookFor, LibsOrBins);

    // extend search, but not for libraries !!
    if (NOT LibsOrBins) then
    begin
      if not result then
        result:=SearchUtil('/usr/local/'+sd,
          LookFor, LibsOrBins);

      if not result then
        result:=SearchUtil('/usr/'+sd,
          LookFor, LibsOrBins);

      if not result then
        result:=SearchUtil('/'+sd,
          LookFor, LibsOrBins);
    end;

  end;
  {$ENDIF}

end;

{$ifndef FPCONLY}
function TCrossInstaller.GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;
begin
  result:=true;
end;
{$endif}

function TCrossInstaller.GetBinUtils(Basepath: string): boolean;
var
  i:integer;
begin
  result:=FBinsFound;

  // only add options once !
  if FCrossOptsAdded then exit;

  // Add user-selected CROSSOPT to fpc.cfg snippet
  // Descendents can add more fpc.cfg snippets but shouldn't remove what the user chose
  for i:=0 to FCrossOpts.Count-1 do AddFPCCFGSnippet(FCrossOpts[i]);
  FCrossOptsAdded:=true;
end;

procedure TCrossInstaller.SetCrossOpt(CrossOpts: string);
// A bit rough-and-ready but hopefully there won't be too many quoting etc problems
var
  Parser: TStringList;
begin
  Parser:=TStringList.Create;
  try
    Parser.Delimiter:=' ';
    Parser.QuoteChar:=''''; //single '. Assume entire CROSSOPT argument is surround by double quotes; indifividual parameters by single.
    Parser.StrictDelimiter:=false; //ignore quoting characters
    Parser.DelimitedText:=CrossOpts;
    FCrossOpts.Clear;
    FCrossOpts.AddStrings(Parser);
  finally
    Parser.Free;
  end;
end;

procedure TCrossInstaller.SetSubArch(SubArch: string);
begin
  FSubArch:=SubArch;
end;

procedure TCrossInstaller.ShowInfo(info: string = ''; Level: TEventType = etInfo);
begin
  if Length(info)>0 then infoln(CrossModuleName+': '+info,Level)
  {$ifndef LCL}
  else infoln(CrossModuleName+' crosscompiler loading',etDebug);
  {$else}
  ;
  {$endif}
end;

procedure TCrossInstaller.Reset;
begin
  FFPCCFGSnippet:='';
  FLibsFound:=false;
  FBinsFound:=false;
  FCrossOptsAdded:=false;
  FCrossOpts.Clear;
  FSubArch:='';
  FBinutilsPathInPath:=false; //don't add binutils directory to path when cross compiling

  FBinUtilsPath:='Error: cross compiler extension must set FBinUtilsPath: the cross compile binutils (as, ld etc). Could be the same as regular path if a binutils prefix is used.';
  FLibsPath:='Error: cross compiler extension must set FLibsPath: path for target environment libraries';
end;

constructor TCrossInstaller.Create;
begin
  FCrossOpts:=TStringList.Create;

  FTargetCPU:='Error: cross compiler extension must set FTargetCPU: cpu for the target environment. Follows FPC names.';
  FTargetOS:='Error: cross compiler extension must set FTargetOS: operating system for the target environment. Follows FPC names';

  FBinUtilsPrefix:='Error: cross compiler extension must set FBinUtilsPrefix: can be empty, if a prefix is used to separate binutils for different archs in the same directory, use it';

  // use installed source compiler for cross compiling by default
  // bootstrap compiler was only usefull in some cornercases
  // see: http://lists.freepascal.org/pipermail/fpc-devel/2018-August/039494.html
  FCompilerUsed:=ctInstalled;

  FCrossModuleNamePrefix:='TAny';

  Reset;
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

