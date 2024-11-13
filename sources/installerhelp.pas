 { Help installer/uninstaller unit for fpcup
Copyright (C) 2012-2014 Ludo Brands, Reinier Olislagers

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
unit installerHelp;

{This class installs, configures and uninstalls FPC and Lazarus help.
It is called by the state machine in installerManager.

When installing, the class downloads FPC RTL/FCL/reference .CHM files,
because compiling them from source is very complicated, and FPC help is
fairly static.
An LCL help CHM is generated from the Lazarus sources and cross-reference
information in the FPC help.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore;
Const
  Sequences=
// Convention: help modules start with Help
//FPC .CHM download
    _DECLARE+_HELPFPC+_SEP+
    {Not using cleanmodule as we're downloading;
    getmodule will detect existing docs and not
    redownload them}
    //_CLEANMODULE+_HELPFPC+_SEP+
    _GETMODULE+_HELPFPC+_SEP+
    _BUILDMODULE+_HELPFPC+_SEP+
    _END+

    //Remove FPC help:
    _DECLARE+_HELPFPC+_UNINSTALL+_SEP+
    _CLEANMODULE+_HELPFPC+_SEP+
    _UNINSTALLMODULE+_HELPFPC+_SEP+
    _END+

    {$ifndef FPCONLY}
    //Lazarus help
    {Note: we don't use helpfpc because that will put the
    help files in the FPC base directory, not in the
    Lazarus base directory
    }
    _DECLARE+_HELPLAZARUS+_SEP+
    {Recent Lazarus compiles lhelp
    on demand once F1 is pressed. So we could disable it}
    _REQUIRES+_LAZBUILD+_SEP+
    {Not using cleanmodule as we're downloading;
    getmodule will detect existing docs and not
    redownload them}
    //_CLEANMODULE+_HELPLAZARUS+_SEP+
    _GETMODULE+_HELPLAZARUS+_SEP+
    _BUILDMODULE+_HELPLAZARUS+_SEP+
    _CONFIGMODULE+_HELPLAZARUS+_SEP+
    _BUILDMODULE+_LHELP+_SEP+
    _END+

    //Remove Lazarus help:
    _DECLARE+_HELPLAZARUS+_UNINSTALL+_SEP+
    _CLEANMODULE+_HELPLAZARUS+_SEP+
    _UNINSTALLMODULE+_HELPLAZARUS+_SEP+
    _END+
    {$endif}

    //selective actions triggered with --only=SequenceName
    _DECLARE+_HELPFPC+_CLEAN+_ONLY+_SEP+_CLEANMODULE+_HELPFPC+_SEP+_END+
    _DECLARE+_HELPFPC+_GET+_ONLY+_SEP+_GETMODULE+_HELPFPC+_SEP+_END+
    _DECLARE+_HELPFPC+_BUILD+_ONLY+_SEP+_BUILDMODULE+_HELPFPC+_SEP+_END+
    _DECLARE+_HELPFPC+_CONFIG+_ONLY+_SEP+_CONFIGMODULE+_HELPFPC+_SEP+_END+

    {$ifndef FPCONLY}
    _DECLARE+_HELPLAZARUS+_CLEAN+_ONLY+_SEP+_CLEANMODULE+_HELPLAZARUS+_SEP+_END+
    _DECLARE+_HELPLAZARUS+_GET+_ONLY+_SEP+_GETMODULE+_HELPLAZARUS+_SEP+_END+
    _DECLARE+_HELPLAZARUS+_BUILD+_ONLY+_SEP+_BUILDMODULE+_HELPLAZARUS+_SEP+_END+
    _DECLARE+_HELPLAZARUS+_CONFIG+_ONLY+_SEP+_CONFIGMODULE+_HELPLAZARUS+_SEP+_END+
    {$endif}

    _ENDFINAL;

type

{ THelpInstaller }

THelpInstaller = class(TBaseHelpInstaller)
private
  InitDone:boolean;
  // Directory where help files are placed
  FTargetDirectory: string;
  {$ifndef FPCONLY}
  // Directory where build_lcl_docs.exe is placed
  FBuildLCLDocsExeDirectory: string;
  {$endif}
protected
  function GetReleaseCandidateFromSource:integer;override;
  function GetVersionFromSource:string;override;
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; virtual;
  // internal initialisation, called from BuildModule,CleanModule,GetModule
  // and UnInstallModule but executed only once
  function InitModule:boolean; virtual;
  // Directory where docs will be installed.
  property TargetDirectory: string read FTargetDirectory;
public
  // Build module
  function BuildModule(ModuleName:string): boolean; override;
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Configure FPC or Lazarus to use the help
  function ConfigModule(ModuleName:string): boolean; override;
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  // Uninstall module
  function UnInstallModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

{ THelpFPCInstaller }

THelpFPCInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
  function InitModule:boolean; override;
public
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Configure FPC to use the help
  function ConfigModule(ModuleName:string): boolean; override;
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;

{$ifndef FPCONLY}

{ THelpLazarusInstaller }

THelpLazarusInstaller = class(THelpInstaller)
protected
  // Build module descendant customisation
  function BuildModuleCustom(ModuleName:string): boolean; override;
  function InitModule:boolean; override;
public
  // Clean up environment
  function CleanModule(ModuleName:string): boolean; override;
  // Configure Lazarus to use the help
  function ConfigModule(ModuleName:string): boolean; override;
  // Install update sources
  function GetModule(ModuleName:string): boolean; override;
  // Uninstall module
  function UnInstallModule(ModuleName:string): boolean; override;
  constructor Create;
  destructor Destroy; override;
end;
{$endif}

implementation

uses
  FileUtil,
  fpcuputil,
  processutils,
  {$ifndef FPCONLY}
  updatelazconfig,
  {$endif}
  libtar,
  bzip2stream,
  dateutils;

function Decompress(SourceFile, TargetFile: string): boolean;
var
  InFile:TFileStream;
  Decompressed:TDecompressBzip2Stream;
  OutFile:TFileStream;
  Buffer: Pointer;
  i: integer;
const buffersize=$2000;
begin
  result:=false; //fail by default
  InFile:=TFileStream.Create(SourceFile, fmOpenRead);
  try
    try
      Decompressed:=TDecompressBzip2Stream.Create(InFile);
    except
      // So[5mething went wrong, e.g. invalid format
      // Now get out of function with result false
      Decompressed.Free;
      exit;
    end;
    OutFile:=TFileStream.Create(TargetFile, fmCreate);
    try
      //We don't have seek on the TDecompressBzip2stream, so can't use
      //CopyFrom...
      //Decompressed.CopyFrom(InFile, InFile.Size);
      GetMem(Buffer,BufferSize);
      repeat
        i:=Decompressed.Read(buffer^,BufferSize);
        if i>0 then
          OutFile.WriteBuffer(buffer^,i);
      until i<BufferSize;
      result:=true;
    finally
      Freemem(Buffer,BufferSize);
      Decompressed.Free;
      OutFile.Free;
    end;
  finally
    InFile.Free;
  end;
end;


{ THelpInstaller }

function THelpInstaller.GetReleaseCandidateFromSource:integer;
begin
  result:=-1;
end;

function THelpInstaller.GetVersionFromSource:string;
var
  OperationSucceeded:boolean;
  LazbuildApp: string;
begin
  result:='0.0.0';

  OperationSucceeded:=true;

  LazbuildApp:=IncludeTrailingPathDelimiter(InstallDirectory)+LAZBUILDNAME+GetExeExt;
  if CheckExecutable(LazbuildApp, ['--help'],LAZBUILDNAME)=false then
  begin
    //WritelnLog('No valid lazbuild executable found. Aborting.', true);
    OperationSucceeded:=false;
  end;

  if OperationSucceeded then
  begin
    // We have a working lazbuild; let's hope it works with primary config path as well
    // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
    Processor.Executable := LazbuildApp;
    Processor.Process.Parameters.Clear;
    Processor.SetParamData('--version');
    ProcessorResult:=Processor.ExecuteAndWait;
    //if (ProcessorResult=0) then
    begin
      if Processor.WorkerOutput.Count>0 then
      begin
        result:=Processor.WorkerOutput.Strings[Processor.WorkerOutput.Count-1];
      end;
    end;
  end;
end;

function THelpInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=true;
  infotext:=InitInfoText(' (BuildModuleCustom: '+ModuleName+'): ');
  Infoln(infotext+'Entering ...',etDebug);
end;

function THelpInstaller.InitModule: boolean;
var
  PlainBinDir: string; //the directory above e.g. c:\development\fpc\bin\i386-win32
  {$IFDEF MSWINDOWS}
  aPath,s:string;
  {$ENDIF MSWINDOWS}
begin
  localinfotext:=InitInfoText(' (InitModule): ');

  PlainBinDir := SafeExpandFileName(FPCBinDir+DirectorySeparator+'..'+DirectorySeparator+'..');

  Infoln(localinfotext+'Entering ...',etDebug);

  result:=(CheckAndGetTools) AND (CheckAndGetNeededBinUtils);

  if result then
  begin
    {$IFDEF MSWINDOWS}
    // Try to ignore existing make.exe, fpc.exe by setting our own path:
    // Note: apparently on Windows, the FPC, perhaps Lazarus make scripts expect
    // at least one ; to be present in the path. If you only have one entry, you
    // can add PathSeparator without problems.
    // https://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg27351.html
    aPath:='';
    if Assigned(SVNClient) AND SVNClient.ValidClient then
    begin
      s:=SVNClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      aPath:=aPath+PathSeparator+ExtractFileDir(s);
    end;
    if Assigned(GITClient) AND GITClient.ValidClient then
    begin
      s:=GITClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      aPath:=aPath+PathSeparator+ExtractFileDir(s);
    end;
    if Assigned(HGClient) AND HGClient.ValidClient then
    begin
      s:=HGClient.RepoExecutable;
      if (Pos(' ',s)>0) then s:=ExtractShortPathName(s);
      aPath:=aPath+PathSeparator+ExtractFileDir(s);
    end;
    SetPath(
      FPCBinDir+PathSeparator+
      PlainBinDir+PathSeparator+
      FMakeDir+PathSeparator+PathSeparator+
      InstallDirectory+
      aPath,
      false,false);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    SetPath(FPCBinDir+PathSeparator+
    {$IFDEF DARWIN}
    // pwd is located in /bin ... the makefile needs it !!
    // tools are located in /usr/bin ... the makefile needs it !!
    // don't ask, but this is needed when fpcupdeluxe runs out of an .app package ... quirk solved this way .. ;-)
    '/bin'+PathSeparator+'/usr/bin'+PathSeparator+
    {$ENDIF}
    PlainBinDir,true,false);
    {$ENDIF UNIX}
  end;

  GetVersion;
  InitDone:=result;
end;

function THelpInstaller.BuildModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=BuildModuleCustom(ModuleName);
end;

function THelpInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=InitModule;
  if not result then exit;
end;

function THelpInstaller.ConfigModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=true;
end;

function THelpInstaller.GetModule(ModuleName: string): boolean;
const
  HELPSOURCEURL : array [0..26,0..1] of string = (
    ('0.9.28','/Old%20releases/Lazarus%200.9.28/fpc-lazarus-0.9.28-doc-chm.tar.bz2'),
    ('0.9.30','/Old%20releases/Lazarus%200.9.30/fpc-lazarus-doc-chm-0.9.30.tar.bz2'),
    ('0.9.30.4','/Old%20releases/Lazarus%200.9.30.4/fpc-lazarus-doc-chm-0.9.30.4.tar.bz2'),
    ('1.0.0','/Old%20releases/Lazarus%201.0/fpc-lazarus-doc-chm-1.0.zip'),
    ('1.0.12','/Lazarus%201.0.12/fpc-lazarus-doc-chm-1.0.12.zip'),
    ('1.2','/Lazarus%201.2/fpc-lazarus-doc-chm-1.2.zip'),
    ('1.4','/Lazarus%201.4/doc-chm_fpc2014_laz2015.zip'),
    ('1.6','/Lazarus%201.6/doc-chm-fpc3.0.0-laz1.6.zip'),
    ('1.6.4','/Lazarus%201.6.4/doc-chm-fpc3.0.2-laz1.6.zip'),
    ('1.8','/Lazarus%201.8.0/doc-chm-fpc3.0.2-laz1.8.zip'),
    ('1.8.2','/Lazarus%201.8.2/doc-chm-fpc3.0.2-laz1.8.zip'),
    ('1.8.4','/Lazarus%201.8.4/doc-chm-fpc3.0.4-laz1.8.zip'),
    ('2.0.0','/Lazarus%202.0.0/doc-chm-fpc3.0.4-laz2.0.zip'),
    ('2.0.2','/Lazarus%202.0.2/doc-chm-fpc3.0.4-laz2.0.2.zip'),
    ('2.0.4','/Lazarus%202.0.4/doc-chm-fpc3.0.4-laz2.0.4.zip'),
    ('2.0.6','/Lazarus%202.0.6/doc-chm-fpc3.0.4-laz2.0.6.zip'),
    ('2.0.8','/Lazarus%202.0.8/doc-chm-fpc3.0.4-laz2.0.8.zip'),
    ('2.0.10','/Lazarus%202.0.10/doc-chm-fpc3.2.0-laz2.0.10.zip'),
    ('2.0.12','/Lazarus%202.0.12/doc-chm-fpc3.2.0-laz2.0.12.zip'),
    ('2.2.0','/Lazarus%202.2.0/doc-chm-fpc3.2.2-laz2.2.0-0.zip'),
    ('2.2.2','/Lazarus%202.2.2/doc-chm-fpc3.2.2-laz2.2.2-0.zip'),
    ('2.2.4','/Lazarus%202.2.4/doc-chm-fpc3.2.2-laz2.2.4-0.zip'),
    ('2.2.6','/Lazarus%202.2.6/doc-chm-fpc3.2.2-laz2.2.6-0.zip'),
    ('3.0','/Lazarus%203.0/doc-chm-fpc3.2.2-laz3.0-0.zip'),
    ('3.2','/Lazarus%203.2/doc-chm-fpc3.2.2-laz3.2-0.zip'),
    ('3.4','/Lazarus%203.4/doc-chm-fpc3.2.2-laz3.4-0.zip'),
    ('3.6','/Lazarus%203.6/doc-chm-fpc3.2.2-laz3.6-0.zip')
  );
  HELP_URL_BASE='https://sourceforge.net/projects/lazarus/files/Lazarus%20Documentation';

var
  DocsZip,DocsTar: string;
  OperationSucceeded: boolean;
  i: longint;
  HelpUrl:string;
  LazarusVersion:string;
  RunTwice:boolean;
  TA:TTarArchive;
  DirRec:TTarDirRec;
begin
  result:=inherited;
  result:=InitModule;
  if not result then exit;

  OperationSucceeded:=false;

  if FileExists(FTargetDirectory+'fcl.chm') and
    FileExists(FTargetDirectory+'rtl.chm') then
  begin
    OperationSucceeded:=true;
    Infoln(ModuleName+': skipping docs download: FPC rtl.chm and fcl.chm already present in docs directory '+FTargetDirectory,etInfo);
  end
  else
  begin
    HelpUrl:='';

    //Find best help version
    i:=Low(HELPSOURCEURL);
    repeat
      LazarusVersion:=HELPSOURCEURL[i,0];
      if
        (CalculateNumericalVersion(LazarusVersion)>=CalculateFullVersion(FMajorVersion,FMinorVersion,FReleaseVersion)) // check for a stable version of Lazarus
        OR
        ((Odd(FMinorVersion)) AND (CalculateNumericalVersion(LazarusVersion)>=CalculateFullVersion(FMajorVersion,(FMinorVersion-1),FReleaseVersion))) // check for a fixes version of Lazarus
      then
      begin
        HelpUrl:=HELPSOURCEURL[i,1];
        //Continue search for even better version with same version number
        while (i<High(HELPSOURCEURL)) do
        begin
          Inc(i);
          LazarusVersion:=HELPSOURCEURL[i,0];
          if CalculateNumericalVersion(LazarusVersion)=CalculateFullVersion(FMajorVersion,FMinorVersion,FReleaseVersion) then
          begin
            HelpUrl:=HELPSOURCEURL[i,1];
          end;
        end;
        break;
      end;
      Inc(i);
    until (i>High(HELPSOURCEURL));

    if Length(HelpUrl)=0 then
    begin
      //Help version determination failed.
      //Get help from the latest stable !!
      for i:=High(HELPSOURCEURL) downto Low(HELPSOURCEURL) do
      begin
        LazarusVersion:=HELPSOURCEURL[i,0];
        if Pos('RC',LazarusVersion)=0 then
        begin
          HelpUrl:=HELPSOURCEURL[i,1];
          break;
        end;
      end;
    end;

    ForceDirectoriesSafe(ExcludeTrailingPathDelimiter(FTargetDirectory));
    DocsZip := GetTempFileNameExt('FPCUPTMP','zip');
    DocsTar := ChangeFileExt(DocsZip,'.tar');

    for RunTwice in boolean do
    begin
      if OperationSucceeded then break;
      SysUtils.DeleteFile(DocsZip); //Get rid of temp zip
      try
        if Length(HelpUrl)=0 then
          //Help version determination failed totally.
          //Get help from latest !!
          OperationSucceeded:=Download(FUseWget, LAZARUSGITLABBINARIES+'/-/archive/'+LAZARUSTRUNKBRANCH+'/binaries-main.zip?path=docs/chm', DocsZip)
        else
          OperationSucceeded:=Download(FUseWget, HELP_URL_BASE+HelpUrl+'/download', DocsZip);
      except
        on E: Exception do
        begin
          // Deal with timeouts, wrong URLs etc
          OperationSucceeded:=false;
          Infoln(ModuleName+': Download documents failed. URL: '+HELP_URL_BASE+HelpUrl+LineEnding+
            'Exception: '+E.ClassName+'/'+E.Message, etWarning);
        end;
      end;
    end;

    if OperationSucceeded then
    begin

      // Archive might be a bzip2, so protect this zip-unzipper while it might/will fail !
      with TNormalUnzipper.Create do
      begin
        try
          Flat:=True;
          try
            OperationSucceeded:=DoUnZip(DocsZip,FTargetDirectory,[]);
          finally
            Free;
          end;
        except
          Free;
        end;
      end;

      if (NOT OperationSucceeded) then
      begin
        // We might have an bzip2 file
        if Decompress(DocsZip, DocsTar) then
        begin
          if FileExists(DocsTar) then
          begin
            TA:=TTarArchive.Create(DocsTar);
            try
              TA.Reset;
              while TA.FindNext({%H-}DirRec) do
              begin
                TA.ReadFile(FTargetDirectory+DirRec.Name);
              end;
              OperationSucceeded:=True;
            finally
              TA.Free;
            end;
            SysUtils.DeleteFile(DocsTar); //Get rid of temp tar
          end;

        end;
      end;

      if (NOT OperationSucceeded) then WritelnLog(etError, 'Download docs error: unzip failed due to unknown error.');

      {
      ResultCode:=ExecuteCommand(FUnzip+' -o -j -d '+FTargetDirectory+' '+DocsZip,FVerbose);
      if ResultCode <> 0 then
      begin
        OperationSucceeded := False;
        Infoln(ModuleName+': unzip failed with resultcode: '+IntToStr(ResultCode),etwarning);
      end;
      }
    end;
  end;

  SysUtils.DeleteFile(DocsZip); //Get rid of temp zip


  if NOT OperationSucceeded then WritelnLog(ModuleName+': Fatal error. Could not download help docs ! But I will continue !!', true);
  //result:=OperationSucceeded;
  // always continue,  even when docs were not build !!
  result:=True;
end;

function THelpInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=true;
end;

constructor THelpInstaller.Create;
begin
  inherited Create;
end;

destructor THelpInstaller.Destroy;
begin
  inherited Destroy;
end;

{ THelpFPCInstaller }

function THelpFPCInstaller.BuildModuleCustom(ModuleName: string): boolean;
begin
  result:=inherited;
  result:=true;
end;

function THelpFPCInstaller.InitModule: boolean;
begin
  result:=inherited;
  result:=false;
  if inherited InitModule then
  begin
    //todo: check with FreeVision FPCIDE to see if this is a sensible location.
    FTargetDirectory:=IncludeTrailingPathDelimiter(InstallDirectory)+
      'doc'+DirectorySeparator+
      'ide'+DirectorySeparator; ;
    Infoln(infotext+'Documentation directory: '+FTargetDirectory,etInfo);
    result:=true;
  end;
end;

function THelpFPCInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited CleanModule(ModuleName);
  // Check for valid directory
  if not DirectoryExists(FTargetDirectory) then
  begin
    Infoln(infotext+'Directory '+FTargetDirectory+' does not exist. Exiting CleanModule.',etInfo);
    exit;
  end;
  if result then
  try
    { Delete .chm files and .xct (cross reference) files
      that could have been downloaded in FPC docs or created by fpcup }
    SysUtils.DeleteFile(FTargetDirectory+'fcl.chm');
    SysUtils.DeleteFile(FTargetDirectory+'fpdoc.chm');
    SysUtils.DeleteFile(FTargetDirectory+'prog.chm');
    SysUtils.DeleteFile(FTargetDirectory+'ref.chm');
    SysUtils.DeleteFile(FTargetDirectory+'rtl.chm');
    SysUtils.DeleteFile(FTargetDirectory+'toc.chm');
    SysUtils.DeleteFile(FTargetDirectory+'user.chm');
    // Cross reference (.xct) files:
    SysUtils.DeleteFile(FTargetDirectory+'fcl.xct');
    SysUtils.DeleteFile(FTargetDirectory+'fpdoc.xct');
    SysUtils.DeleteFile(FTargetDirectory+'prog.xct');
    SysUtils.DeleteFile(FTargetDirectory+'ref.xct');
    SysUtils.DeleteFile(FTargetDirectory+'rtl.xct');
    SysUtils.DeleteFile(FTargetDirectory+'toc.xct');
    SysUtils.DeleteFile(FTargetDirectory+'user.xct');
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog(ModuleName+' clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')',true);
      result:=false;
    end;
  end;
end;

function THelpFPCInstaller.ConfigModule(ModuleName: string): boolean;
begin
  Result:=inherited ConfigModule(ModuleName);
  //todo: implement config for fpide
end;

function THelpFPCInstaller.GetModule(ModuleName: string): boolean;
begin
  Result:=inherited GetModule(ModuleName);
end;

constructor THelpFPCInstaller.Create;
begin
  inherited Create;
end;

destructor THelpFPCInstaller.Destroy;
begin
  inherited Destroy;
end;

{$ifndef FPCONLY}

{ THelpLazarusInstaller }

function THelpLazarusInstaller.BuildModuleCustom(ModuleName: string): boolean;
var
  BuildLCLDocsExe: string;
  BuildResult: integer;
  ExistingLCLHelp: string;
  FPDocExe: string;
  FPDocExes: TStringList;
  GeneratedLCLHelp: string;
  LazbuildApp: string;
  LCLDate: TDateTime;
  LHelpDirectory: string;
  OperationSucceeded:boolean;
begin
  result:=inherited;
  // lhelp viewer is needed which Lazarus builds that on first run
  // However, it can be prebuilt by enabling it as an external module in fpcup.ini
  OperationSucceeded:=true;
  // The locations of the LCL.chm we generate and the existing one we can overwrite:
  ExistingLCLHelp:=FTargetDirectory+'lcl.chm';
  GeneratedLCLHelp:=FTargetDirectory+'lcl'+DirectorySeparator+'lcl.chm';

  if OperationSucceeded then
  begin
    // A safe, old value
    LCLDate:=EncodeDate(1910,01,01);
    try
      if FileExists(ExistingLCLHelp) then
        LCLDate:=FileDateToDateTime(FileAge(ExistingLCLHelp));
    except
      // Ignore exceptions, leave old date as is
    end;

    // Only consider building if lcl.chm does not exist
    // or is not read-only.
    // Then it should be old (> 7 days) or empty.
    // We assume that readonly means the user doesn't want to
    // overwrite.
    // Note: this still does not seem to go right. On Linux
    // without lcl.chm it detects the file as readonly...
    if FileExists(ExistingLCLHelp) then
      Infoln('Check if '+ExistingLCLHelp+' exists? Yes.',etInfo)
    else
      Infoln('Check if '+ExistingLCLHelp+' exists? No.',etInfo);
    if (FileExists(ExistingLCLHelp)=false) or
      (
      (FileIsReadOnly(ExistingLCLHelp)=false)
      and
      ((DaysBetween(Now,LCLDate)>7)
      or (FileSize(ExistingLCLHelp)=0))
      )
      then
    begin
      BuildLCLDocsExe:=FBuildLCLDocsExeDirectory+'build_lcl_docs'+GetExeExt;
      if OperationSucceeded then
      begin
        // Only recompile build_lcl_docs.exe if needed
        if CheckExecutable(BuildLCLDocsExe, ['--help'], 'build_lcl_docs')=false then
        begin
          // Check for valid lazbuild.
          // Note: we don't check if we have a valid primary config path, but that will come out
          // in the next steps.
          LazbuildApp:=IncludeTrailingPathDelimiter(InstallDirectory)+LAZBUILDNAME+GetExeExt;
          if CheckExecutable(LazbuildApp, ['--help'],LAZBUILDNAME)=false then
          begin
            WritelnLog(ModuleName+': No valid lazbuild executable found. Aborting.', true);
            OperationSucceeded:=false;
          end;

          if OperationSucceeded then
          begin
            // We have a working lazbuild; let's hope it works with primary config path as well
            // Build Lazarus chm help compiler; will be used to compile fpdocs xml format into .chm help
            Processor.Executable := LazbuildApp;
            Processor.Process.Parameters.Clear;
            Processor.SetParamData('--primary-config-path='+FLazarusPrimaryConfigPath+'');
            Processor.SetParamData(FBuildLCLDocsExeDirectory+'build_lcl_docs.lpr');
            Infoln(ModuleName+': compiling build_lcl_docs help compiler:',etInfo);
            WritelnLog('Building help compiler (also time consuming generation of documents) !!!!!!', true);
            ProcessorResult:=Processor.ExecuteAndWait;
            WritelnLog('Execute: '+Processor.Executable+' exit code: '+InttoStr(ProcessorResult), true);
            if ProcessorResult <> 0 then
            begin
              WritelnLog(etError,ModuleName+': error compiling build_lcl_docs docs builder.', true);
              OperationSucceeded := False;
            end;
          end;
        end;
      end;

      // Check for proper fpdoc
      FPDocExe:=FPCBinDir+DirectorySeparator+'fpdoc'+GetExeExt;
      if (CheckExecutable(FPDocExe, ['--help'], 'FPDoc')=false) then
      begin
      FPDocExe:=IncludeTrailingPathDelimiter(FFPCSourceDir)+
        'utils'+DirectorySeparator+
        'fpdoc'+DirectorySeparator+
        'fpdoc'+GetExeExt;
      end;
      if (CheckExecutable(FPDocExe, ['--help'], 'FPDoc')=false) then
      begin
        // Try again, in bin directory; newer FPC releases may have migrated to this
        FPDocExes:=FindAllFiles(IncludeTrailingPathDelimiter(FFPCInstallDir)+'bin'+DirectorySeparator,
          'fpdoc'+GetExeExt,true);
        try
          if FPDocExes.Count>0 then FPDocExe:=FPDocExes[0]; //take only the first
          if (CheckExecutable(FPDocExe, ['--help'], 'FPDoc')=false) then
          begin
            WritelnLog(etError,ModuleName+': no valid fpdoc executable found ('+FPDocExe+'). Please recompile fpc.', true);
            OperationSucceeded := False;
          end
          else
          begin
            Infoln(ModuleName+': found valid fpdoc executable.',etInfo);
          end;
        finally
          FPDocExes.Free;
        end;
      end;

      if OperationSucceeded then
      begin
        // Compile Lazarus LCL CHM help
        Processor.Executable := BuildLCLDocsExe;
        // Make sure directory switched to that of the FPC docs,
        // otherwise paths to source files will not work.
        Processor.Process.CurrentDirectory:=ExcludeTrailingPathDelimiter(FTargetDirectory);
        Processor.Process.Parameters.Clear;
        // Instruct build_lcl_docs to cross-reference FPC documentation by specifying
        // the directory that contains the fcl and rtl .xct files.
        // If those .xct files are not present, FPC 2.7.1 fpdoc will throw an exception
        Processor.SetParamData('--fpcdocs');
        Processor.SetParamData(ExcludeTrailingPathDelimiter(FTargetDirectory));
        // Let build_lcl_docs know which fpdoc application to use:
        Processor.SetParamData('--fpdoc');
        Processor.SetParamData(FPDocExe);
        // Newer versions of fpc mess up the .css file location;
        // Exception at 00441644: Exception:
        // Can't find CSS file "..\fpdoc.css".
        //
        // So specify path explicitly
        // --css-file argument available since r42283
        Processor.SetParamData('--css-file='+IncludeTrailingPathDelimiter(FFPCSourceDir)+
          'utils'+DirectorySeparator+'fpdoc'+DirectorySeparator+'fpdoc.css');

        Processor.SetParamData('--outfmt');
        Processor.SetParamData('chm');
        { this will give a huge amount of warnings which should be fixed by
        fpdoc and/or the .chm files so are rather useless
        Processor.SetParamData('--warnings'); //let tool show warnings as well
        }
        // Show application output if desired:
        Infoln(ModuleName+': compiling chm help docs:',etInfo);
        { The CHM file gets output into <lazarusdir>/docs/chm/lcl/lcl.chm
        Though that may work when adjusting the baseurl option in Lazarus for each
        CHM file, it's easier to move them to <lazarusdir>/docs/chm,
        which is picked up by the default Lazarus settings.
        The generated .xct file is an index file for fpdoc cross file links,
        used if you want to link to the chm from other chms.}
        ProcessorResult:=Processor.ExecuteAndWait;
        BuildResult:=ProcessorResult;
        if BuildResult <> 0 then
        begin
          WritelnLog(etError,ModuleName+': error creating chm help docs. build_lcl_docs exit status: '+IntToStr(BuildResult), true);
          OperationSucceeded := False;
        end;
      end;

      if OperationSucceeded then
      begin
        // Move files if required
        if FileExists(GeneratedLCLHelp) then
        begin
          if FileSize(GeneratedLCLHelp)>0 then
          begin
            Infoln(ModuleName+': moving lcl.chm to docs directory',etInfo);
            OperationSucceeded:=MoveFile(GeneratedLCLHelp,ExistingLCLHelp);
          end
          else
          begin
            // File exists, but is empty. We might have an older file still present
            WritelnLog(etWarning, ModuleName+': WARNING: '+GeneratedLCLHelp+
            ' was created but is empty (perhaps due to FPC bugs). Lcl.chm may be out of date! Try running with --verbose to see build_lcl_docs error messages.', true);
            // Todo: change this once fixes for fpdoc chm generation are in fixes_26:
            OperationSucceeded:=true;
          end;
        end;
      end;
    end
    else
    begin
      // Indicate reason for not creating lcl.chm
      if FileIsReadOnly(ExistingLCLHelp) then
        Infoln(ModuleName+': not building LCL.chm as it is read only.',etInfo)
      else
        Infoln(ModuleName+': not building LCL.chm as it is quite recent: '+FormatDateTime('YYYYMMDD',LCLDate),etInfo);
    end;
  end;

  if NOT OperationSucceeded then WritelnLog(etWarning, ModuleName+': Something went wrong. But I will continue !!', true);
  //result:=OperationSucceeded;
  // always continue,  even when docs were not build !!
  result:=True;
end;

function THelpLazarusInstaller.InitModule: boolean;
begin
  localinfotext:=InitInfoText(' (InitModule): ');

  Infoln(localinfotext+'Entering ...',etDebug);

  result:=false;

  if inherited InitModule then
  begin
    // This must be the directory of the build_lcl_docs project, otherwise
    // build_lcl_docs will fail; at least it won't pick up the FPC help files for cross references
    FTargetDirectory:=IncludeTrailingPathDelimiter(InstallDirectory)+
      'docs'+DirectorySeparator+
      'chm'+DirectorySeparator;
    Infoln('helplazarus: documentation directory: '+FTargetDirectory,etInfo);
    FBuildLCLDocsExeDirectory:=IncludeTrailingPathDelimiter(InstallDirectory)+
      'docs'+DirectorySeparator+
      'html'+DirectorySeparator;
    Infoln(localinfotext+'FBuildLCLDocsExeDirectory: '+FTargetDirectory,etDebug);
    result:=true;
  end;
end;

function THelpLazarusInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=inherited CleanModule(ModuleName);
  // Check for valid directory
  if not DirectoryExists(FTargetDirectory) then
  begin
    Infoln('HelpLazarusInstaller CleanModule: directory '+FTargetDirectory+' does not exist. Exiting CleanModule.',etInfo);
    exit;
  end;
  if result then
  try
    { Delete .chm files and .xct (cross reference) files
      that could have been downloaded in FPC docs or created by fpcup }
    SysUtils.DeleteFile(FTargetDirectory+'fcl.chm');
    SysUtils.DeleteFile(FTargetDirectory+'fpdoc.chm');
    SysUtils.DeleteFile(FTargetDirectory+'prog.chm');
    SysUtils.DeleteFile(FTargetDirectory+'ref.chm');
    SysUtils.DeleteFile(FTargetDirectory+'rtl.chm');
    SysUtils.DeleteFile(FTargetDirectory+'lcl.chm');
    SysUtils.DeleteFile(FTargetDirectory+'toc.chm');
    SysUtils.DeleteFile(FTargetDirectory+'user.chm');
    // Cross reference (.xct) files:
    SysUtils.DeleteFile(FTargetDirectory+'fcl.xct');
    SysUtils.DeleteFile(FTargetDirectory+'fpdoc.xct');
    SysUtils.DeleteFile(FTargetDirectory+'prog.xct');
    SysUtils.DeleteFile(FTargetDirectory+'ref.xct');
    SysUtils.DeleteFile(FTargetDirectory+'rtl.xct');
    SysUtils.DeleteFile(FTargetDirectory+'lcl.xct');
    SysUtils.DeleteFile(FTargetDirectory+'toc.xct');
    SysUtils.DeleteFile(FTargetDirectory+'user.xct');
    result:=true;
  except
    on E: Exception do
    begin
      WritelnLog(ModuleName+' clean: error: exception occurred: '+E.ClassName+'/'+E.Message+')',true);
      result:=false;
    end;
  end;
end;

function THelpLazarusInstaller.ConfigModule(ModuleName: string): boolean;
var
  LazarusConfig: TUpdateLazConfig;
begin
  result:=inherited ConfigModule(ModuleName);
  if result then
  begin
    result:=ForceDirectoriesSafe(FLazarusPrimaryConfigPath);
  end
  else
  begin
    WritelnLog('Lazarus help: error: could not create primary config path '+FLazarusPrimaryConfigPath);
  end;
  if result then
  begin
    LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
    try
      try
        {
        We don't need to set explicit paths as long as we use the defaults, e.g.
        $(LazarusDir)\docs\html and $(LazarusDir)\docs\chm
        http://wiki.lazarus.freepascal.org/Installing_Help_in_the_IDE#Installing_CHM_help_.28Lazarus_1.0RC1_and_later.29
        We could set it explicitly with
        LazarusConfig.SetVariable(HelpConfig,
          'Viewers/TChmHelpViewer/CHMHelp/FilesPath',
          IncludeTrailingPathDelimiter(FInstallDirectory)+'docs'+DirectorySeparator+'chm'+DirectorySeparator
          );
        }
        result:=true;
      except
        on E: Exception do
        begin
          result:=false;
          WritelnLog('Lazarus help: Error setting Lazarus config: '+E.ClassName+'/'+E.Message, true);
        end;
      end;
    finally
      LazarusConfig.Free;
    end;
  end;
end;

function THelpLazarusInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  Result:=inherited UnInstallModule(ModuleName);
  // Removing config not needed anymore since we use the default
end;

function THelpLazarusInstaller.GetModule(ModuleName: string): boolean;
var
  LazarusConfig: TUpdateLazConfig;
  LazarusVersion:string;
begin
  // get Lazarus version for correct version of helpfile
  LazarusConfig:=TUpdateLazConfig.Create(FLazarusPrimaryConfigPath);
  try
    LazarusVersion:=LazarusConfig.GetVariable(EnvironmentConfig,'EnvironmentOptions/Version/Lazarus');
    VersionFromString(LazarusVersion,FMajorVersion,FMinorVersion,FReleaseVersion,FPatchVersion);
  finally
    LazarusConfig.Free;
  end;

  Result:=inherited GetModule(ModuleName);
end;


constructor THelpLazarusInstaller.Create;
begin
  inherited Create;
end;

destructor THelpLazarusInstaller.Destroy;
begin
  inherited Destroy;
end;
{$endif}


end.

