program project1;
{ inspect_pkg.pas
  FPC program to inspect an OpenBSD package archive, extract its
  shared library, detect package dependencies and ELF NEEDED entries.
}

{$mode objfpc}{$H+}

{$define BASE}
{$define PACKAGES}
{$define QTLIBRARIES}

uses
  SysUtils, Classes, Process, StrUtils, URIParser, ftpsend, httpsend, synautil, ssl_openssl;


const
  BASEURL      = 'https://ftp.eu.openbsd.org/pub/OpenBSD/%s'; // VERSION
  //BASEURL      = 'https://cdn.openbsd.org/pub/OpenBSD/%s'; // VERSION
  PACKAGES     = 'packages/%s'; // CPU

  CPU          = 'amd64';

  VERSIONMAJOR = '6';
  VERSIONMINOR = '8';

  VERSION      = VERSIONMAJOR+VERSIONMINOR;
  VERSIONDOT   = VERSIONMAJOR+'.'+VERSIONMINOR;

  BASE         = 'base%s.tgz'; // CPU;VERSION
  COMP         = 'comp%s.tgz'; // CPU;VERSION
  XBASE        = 'xbase%s.tgz'; // CPU;VERSION
  XFONT        = 'xfont%s.tgz'; // CPU;VERSION
  XSERV        = 'xserv%s.tgz'; // CPU;VERSION
  XSHARE       = 'xshare%s.tgz'; // CPU;VERSION

const LAZFILES : array [0..11] of string = (
  'glib2-',
  'gtk+2-',
  'gtk+3-',
  'gdk-pixbuf-',
  'gdk-pixbuf-xlib-',
  'pango-',
  'cairo-',
  'harfbuzz-',
  'libffi-',
  'libiconv-',
  'libcanberra-',
  'libcanberra-gtk3-'
);

const LAZQTFILES : array [0..2] of string = (
  'qtbase',
  'qtx11extras-',
  'qt6-qtbase'
);


type
  //TMyFTPSend = class(TFTPSend);

  TStringListHelper = class helper for TStringList
    function ContainsCI(const S: string): Boolean;
  end;

procedure DownloadPackage(const FileName, LocalPath: string);
var
  HTTP: THTTPSend;
  URL: string;
begin
  //URL := 'https://cdn.openbsd.org/pub/OpenBSD/7.9/packages/amd64/' + FileName;

  //'https://cdn.openbsd.org/pub/OpenBSD/7.9/amd64/base79.tgz'

  writeln('Going do download '+FileName);

  URL := FileName;

  HTTP := THTTPSend.Create;
  try
    // Optional: set timeouts, User-Agent, etc.
    // HTTP.Timeout := 30000;
    // HTTP.UserAgent := 'MyApp/1.0';

    if HTTP.HTTPMethod('GET', URL) then
    begin
      if HTTP.ResultCode = 200 then
      begin
        HTTP.Document.SaveToFile(LocalPath);
        WriteLn('Downloaded ', HTTP.Document.Size, ' bytes');
      end
      else
        WriteLn('HTTP error: ', HTTP.ResultCode, ' ', HTTP.ResultString);
    end
    else
      WriteLn('Connection failed: ', HTTP.Sock.LastErrorDesc);
  finally
    HTTP.Free;
  end;
end;

function TStringListHelper.ContainsCI(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if SameText(Strings[I], S) then
      Exit(True);
end;

var
  FileToDownload,URL:string;
  PackageName, PackageVersion: string;
  Archive, OutDir, WorkDir, SoPath, ContentsFile: string;
  Depends, DependsNew, WantLibs, ElfNeeded, Libraries, LibraryNames: TStringList;
  StartName,FullName:string;
  I: Integer;

{---------------------------------------------------------------}
{ Extract selected members from the package tarball              }
{---------------------------------------------------------------}

function QuoteStr(S: string): string;
begin
  //Result := '''' + S + '''';
  Result := S;
end;

function CopyFile(const Src,Dest : string):boolean;
Var
  D : String;
  Fin,FOut : TFileStream;
  Count : Int64;
  A : Integer;
{$ifdef UNIX}
  FileStat: stat;
{$endif UNIX}
begin
  result:=false;
  D:=IncludeTrailingPathDelimiter(Dest);
  if DirectoryExists(D) then
  begin
    D:=D+ExtractFileName(Src);
  end
  else
  begin
    D:=Dest;
  end;
  //if (NOT (cffOverwriteFile in Flags)) and FileExists(D) then exit;
  {$ifdef DARWIN}
  { First delete file on Darwin OS to avoid codesign issues }
  if FileExists(D) then SysUtils.DeleteFile(D);
  {$endif DARWIN}
  FIn:=TFileStream.Create(Src,fmopenRead or fmShareDenyNone);
  try
    FOut:=TFileStream.Create(D,fmCreate or fmShareDenyNone);
    try
      Count:=Fout.CopyFrom(FIn,0);
      result:=(Count=Fin.Size);
    finally
      FreeAndNil(Fout);
    end;
    if result then
    begin
      A:=FileGetDate(FIn.Handle);
      If (A<>-1) then FileSetDate(D,A);
  {$ifdef UNIX}
      // Copy the file-access rights on Unix, especially the executable-bit
      filestat:=Default(stat);
      if (FpStat(Src,FileStat)=0) then FpChmod(D,FileStat.st_mode);
  {$endif UNIX}
    end;
  finally
    FreeAndNil(Fin);
  end;
end;

function GetAllFilesMask: string;
begin
  {$IFDEF WINDOWS}
  Result:='*.*';
  {$ELSE}
  Result:='*';
  {$ENDIF}
end;

function DeleteDirectoryEx(DirectoryName: string): boolean;
// Lazarus fileutil.DeleteDirectory on steroids, works like
// deltree <directory>, rmdir /s /q <directory> or rm -rf <directory>
// - removes read-only files/directories (DeleteDirectory doesn't)
// - removes directory itself
// Adapted from fileutil.DeleteDirectory, thanks to Paweł Dmitruk
var
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  FileInfo: TRawByteSearchRec;
  {$ELSE}
  FileInfo: TSearchRec;
  {$ENDIF}
  CurSrcDir: String;
  CurFilename: String;
begin
  result:=true;

  //CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  CurSrcDir:=IncludeTrailingPathDelimiter(DirectoryName);

  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,faAnyFile{$ifdef unix} or {%H-}faSymLink {$endif unix},FileInfo)=0 then
  begin
    result:=true;
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Look at all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        // Remove read-only file attribute so we can delete it:
        if (FileInfo.Attr and faReadOnly)>0 then
          FileSetAttr(CurFilename, FileInfo.Attr-faReadOnly);
        if ((FileInfo.Attr and faDirectory)>0) {$ifdef unix} and ((FileInfo.Attr and {%H-}faSymLink)=0) {$endif unix} then
        begin
          // Directory; exit with failure on error
          if not DeleteDirectoryEx(CurFilename) then result:=false;
        end
        else
        begin
          // File; exit with failure on error
          if not SysUtils.DeleteFile(CurFilename) then result:=false;
        end;
      end;
    until (SysUtils.FindNext(FileInfo)<>0) OR (NOT result);
    SysUtils.FindClose(FileInfo);
  end;
  // Remove root directory; exit with failure on error:
  if result then result:=RemoveDir(DirectoryName);
end;



function ExtractMembers(const Arch, Dest: string; const Members: array of string): Boolean;
var
  OutS: string;
  A,M,Mnew: string;
begin
  Result:=True;
  A:=ExtractFileName(ChangeFileExt(Arch,'.tar'));
  A:=ConcatPaths([Dest,A]);
  if (NOT FileExists(A)) then
  begin
    OutS:='';
    Result := RunCommand('.\7za.exe',['x',Arch,'-y','-o'+Dest],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    if not Result then
      WriteLn(StdErr, '7za failed: ', OutS);
  end;
  if Result then
  begin
    for M in Members do
    begin
      if (NOT FileExists(ConcatPaths([Dest,M]))) then
      begin
        OutS:='';
        //Result := RunCommand('.\7za.exe',['x',A,'-o'+Dest,M],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
        Result := RunCommand('.\7za.exe',['x',A,'-y','-o'+Dest,M],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});

        if (Pos('No files to process',OutS)>0) then
        begin
          if M[1] in AllowDirectorySeparators then
            Mnew:=Copy(M,2,MaxInt)
          else
            Mnew:=DirectorySeparator+M;

          Result := RunCommand('.\7za.exe',['x',A,'-y','-o'+Dest,Mnew],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
        end;

        if not Result then
          WriteLn(StdErr, '7za failed: ', OutS);
      end;
    end;
  end;
end;

(*
function ExtractMembers(const Arch, Dest: string; const Members: array of string): Boolean;
var
  OutS: string;
  M: string;
begin
  for M in Members do
  begin
    Result := RunCommand('.\arc.exe',['extract',Arch,M,Dest],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    if not Result then
      WriteLn(StdErr, '7za failed: ', OutS);
  end;
end;
*)

{---------------------------------------------------------------}
{ Parse +CONTENTS                                                }
{---------------------------------------------------------------}
procedure ParseContents(const FileName: string);
var
  SL: TStringList;
  Line, Key, Val, Lib, Sym, Dep: string;
  P: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    for Line in SL do
    begin
      if Line = '' then Continue;
      if Line[1] <> '@' then Continue;

      P := Pos(' ', Line);
      if P = 0 then
      begin
        Key := Copy(Line, 2, MaxInt);
        Val := '';
      end
      else
      begin
        Key := Copy(Line, 2, P - 2);
        Val := Trim(Copy(Line, P + 1, MaxInt));
      end;

      if Libraries.Count>0 then
      begin
        if key = 'symlink' then
        begin
          Lib:=Libraries.Strings[(Libraries.Count-1)];
          Lib:=ExtractFileName(Lib);
          Sym:=val;
          Sym:=ExtractFileName(Sym);
          if Lib=Sym then
            Libraries.Delete(Libraries.Count-1);
        end;
      end;

      case Key of
        'name':    PackageName := Val;
        'version': PackageVersion := Val;
        'depend':
          begin
            P:=RPos(':',Val);
            Dep:=Copy(Val,P+1,MaxInt)+'.tgz';
            P:=Depends.Count;
            Depends.Add(Dep);
            if Depends.Count<>P then DependsNew.Add(Dep);
            //Depends.Add(Val);
          end;
        'wantlib': WantLibs.Add(Val);
        'lib':     Libraries.Add(Val);   { relative path of the shared object }
      end;
    end;
  finally
    SL.Free;
  end;
end;

{---------------------------------------------------------------}
{ Read ELF NEEDED entries via readelf                            }
{---------------------------------------------------------------}
procedure GetElfNeeded(const SoFile: string);
var
  OutS, Line, Lib: string;
  SL: TStringList;
  P1, P2: Integer;
begin
  if not FileExists(SoFile) then Exit;

  if NOT RunCommand('.\readelf.exe',['-d','-W',SoFile],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
  begin
    WriteLn(StdErr, 'readelf failed (binary may be for a different OS)');
    Exit;
  end;

  SL := TStringList.Create;
  try
    SL.Text := OutS;
    for Line in SL do
    begin
      { look for:  (NEEDED) Shared library: [libFoo.so.X.Y] }
      if Pos('(NEEDED)', Line) = 0 then Continue;
      P1 := Pos('[', Line);
      P2 := Pos(']', Line);
      if (P1 > 0) and (P2 > P1) then
      begin
        Lib := Copy(Line, P1 + 1, P2 - P1 - 1);
        if not ElfNeeded.ContainsCI(Lib) then
          ElfNeeded.Add(Lib);
      end;
    end;
  finally
    SL.Free;
  end;
end;

{---------------------------------------------------------------}
{ Copy the library into OutDir/libs/                             }
{---------------------------------------------------------------}
function StoreLibrary(const RelPath, SrcRoot, DestRoot, DestDir: string): string;
var
  Src, Dest: string;
begin
  Src := IncludeTrailingPathDelimiter(SrcRoot) + RelPath;
  Dest := ConcatPaths([DestRoot,DestDir,ExtractFileName(RelPath)]);
  ForceDirectories(ExtractFilePath(Dest));
  if FileExists(Src) then
  begin
    CopyFile(Src, Dest);
    Result := Dest;
  end
  else
    Result := '';
end;

{---------------------------------------------------------------}
{ Main                                                           }
{---------------------------------------------------------------}
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)),
            ' <package.tgz> [output-directory]');
    Halt(1);
  end;

  Archive := ExpandFileName(ParamStr(1));
  if not FileExists(Archive) then
  begin
    //WriteLn(StdErr, 'Archive not found: ', Archive);
    //Halt(1);
  end;

  if ParamCount >= 2 then
    OutDir := ExpandFileName(ParamStr(2))
  else
    OutDir := GetCurrentDir;

  ForceDirectories(OutDir);

  WorkDir := IncludeTrailingPathDelimiter(OutDir) + 'pkgwork';

  //if DirectoryExists(WorkDir) then DeleteDirectoryEx(WorkDir);

  ForceDirectories(WorkDir);

  Depends       := TStringList.Create;
  DependsNew    := TStringList.Create;
  WantLibs      := TStringList.Create;
  ElfNeeded     := TStringList.Create;
  Libraries     := TStringList.Create;
  LibraryNames  := TStringList.Create;

  Depends.Sorted := True;
  Depends.Duplicates := dupIgnore;

  WantLibs.Sorted := True;
  WantLibs.Duplicates := dupIgnore;

  ElfNeeded.Sorted := True;
  ElfNeeded.Duplicates := dupIgnore;

  try

    {$ifdef BASE}

    URL:=Format(BASEURL,[VERSIONDOT])+'/'+CPU+'/';

    FileToDownload:=Format(BASE,[VERSION]);

    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,FileToDownload) ;
    if FileExists(FileToDownload) then
    begin
      ExtractMembers(FileToDownload, WorkDir,['.\usr\lib\*.so.*','.\usr\lib\crtendS.o','.\usr\lib\crtbegin.o','.\usr\lib\crtbeginS.o','.\usr\lib\crtend.o']);
    end;

    (*
    FileToDownload:=Format(COMP,[VERSION]);
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,FileToDownload) ;
    if FileExists(FileToDownload) then
    begin
      ExtractMembers(FileToDownload, WorkDir,['.\usr\lib\*.so.*']);
    end;
    *)

    FileToDownload:=Format(XBASE,[VERSION]);
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,FileToDownload) ;
    if FileExists(FileToDownload) then
    begin
      ExtractMembers(FileToDownload, WorkDir,['.\usr\X11R6\lib\*.so.*']);
    end;

    (*
    FileToDownload:=Format(XFONT,[VERSION]);
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,FileToDownload) ;
    if FileExists(FileToDownload) then
    begin
      ExtractMembers(FileToDownload, WorkDir,['.\usr\lib\*.so.*']);
    end;
    *)

    (*
    FileToDownload:=Format(XSERV,[VERSION]);
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,FileToDownload) ;
    if FileExists(FileToDownload) then
    begin
      ExtractMembers(FileToDownload, WorkDir,['.\usr\lib\*.so.*']);
    end;

    FileToDownload:=Format(XSHARE,[VERSION]);
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,FileToDownload) ;
    if FileExists(FileToDownload) then
    begin
      ExtractMembers(FileToDownload, WorkDir,['.\usr\lib\*.so.*']);
    end;
    *)


    {$endif}


    // Locate our required Lazarus libs in the packages signature file

    {$ifdef PACKAGES}
    URL:=Format(BASEURL,[VERSIONDOT])+'/packages/'+CPU+'/';
    FileToDownload:='SHA256.sig';
    ContentsFile:='packages.'+FileToDownload;
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,ContentsFile) ;
    if FileExists(FileToDownload) then if (NOT FileExists(ContentsFile)) then CopyFile(FileToDownload,ContentsFile);
    if FileExists(ContentsFile) then
    begin
      LibraryNames.LoadFromFile(ContentsFile);
      for FullName in LibraryNames do
      begin
        for StartName in LAZFILES do
        begin
          i:=Pos('('+StartName,FullName);
          if i>0 then
          begin
            FileToDownload:=Copy(FullName,i+1,MaxInt);
            i:=Pos(')',FileToDownload);
            if i=0 then i:=MaxInt;
            FileToDownload:=Copy(FileToDownload,1,i-1);
            Libraries.Add(FileToDownload);
          end;
        end;
      end;

      LibraryNames.Clear;
      LibraryNames.Assign(Libraries);
      Depends.Assign(Libraries);

      //Depends.Clear;
      WantLibs.Clear;
      ElfNeeded.Clear;
      Libraries.Clear;

      // Get our required Lazarus libs

      URL:=Format(BASEURL,[VERSIONDOT])+'/packages/'+CPU+'/';

      repeat

        DependsNew.Clear;

        for FullName in LibraryNames do
        begin

          if (NOT FileExists(FullName)) then
          begin
            DownloadPackage(URL+FullName,FullName) ;
          end;

          if true then
          begin
            if FileExists(FullName) then
            begin
              // Extract the so-libraries

              ExtractMembers(FullName, WorkDir,['\lib\*.so.*']);

              ContentsFile := IncludeTrailingPathDelimiter(WorkDir) + '+CONTENTS';
              if FileExists(ContentsFile) then
              begin
                if FileIsReadOnly(ContentsFile) then FileSetAttr(ContentsFile, FileGetAttr(ContentsFile) and not faReadOnly);
                if (NOT SysUtils.DeleteFile(ContentsFile)) then
                begin
                  WriteLn(StdErr, '+CONTENTS not deleted');
                end;
              end;

              PackageName:='Unknown';
              PackageVersion:='Unknown';

              // Extract the contentsfile

              if ExtractMembers(FullName, WorkDir,['+CONTENTS']) then
              begin
                if not FileExists(ContentsFile) then
                begin
                  WriteLn(StdErr, '+CONTENTS not found inside archive');
                  Halt(1);
                end;

                { 2. Parse package metadata }
                ParseContents(ContentsFile);

                WriteLn;
                WriteLn('=== Package information ===');
                WriteLn('Name     : ', PackageName);
                if Version <> '' then
                  WriteLn('Version  : ', PackageVersion);

              end;
            end;
          end;
        end;

        LibraryNames.Assign(DependsNew);

      until (DependsNew.Count=0);
    end; // packages

    WriteLn;
    WriteLn('=== Package-level dependencies (@depend) ===');
    if Depends.Count = 0 then
      WriteLn('  (none)')
    else
      for I := 0 to Depends.Count - 1 do
        WriteLn('  ', Depends[I]);

    WriteLn;
    WriteLn('=== Required libraries (@wantlib) ===');
    if WantLibs.Count = 0 then
      WriteLn('  (none)')
    else
      for I := 0 to WantLibs.Count - 1 do
        WriteLn('  ', WantLibs[I]);


    { 3. Store the shared object }
    WriteLn;
    WriteLn('=== Shared libraries found in package ===');
    SoPath := '';
    for I := 0 to Libraries.Count - 1 do
    begin
      WriteLn('  ', Libraries[I]);
      //if SoPath = '' then   { take the first real .so }
      begin
        //ExtractMembers(Archive, WorkDir,[Libraries[I]]);
        //SoPath := StoreLibrary(Libraries[I], WorkDir, OutDir,'libs');
      end;
    end;


    (*
    { 4. ELF NEEDED inspection (more precise than @wantlib) }
    if SoPath <> '' then
    begin
      WriteLn;
      WriteLn('=== ELF NEEDED entries (from the .so itself) ===');
      GetElfNeeded(SoPath);
      if ElfNeeded.Count = 0 then
        WriteLn('  (none / not a dynamic object for this host)')
      else
        for I := 0 to ElfNeeded.Count - 1 do
          WriteLn('  ', ElfNeeded[I]);
    end;

    { 5. Combined set that would be walked }
    WriteLn;
    WriteLn('=== All libraries that need to be resolved (union) ===');
    for I := 0 to WantLibs.Count - 1 do
      if not ElfNeeded.ContainsCI(WantLibs[I]) then
        ElfNeeded.Add(WantLibs[I]);   { keep unique }
    for I := 0 to ElfNeeded.Count - 1 do
      WriteLn('  ', ElfNeeded[I]);

    *)
    {$endif}




    {$ifdef QTLIBRARIES}

    // QT files
    Libraries.Clear;
    LibraryNames.Clear;

    URL:=Format(BASEURL,[VERSIONDOT])+'/packages/'+CPU+'/';
    FileToDownload:='SHA256.sig';
    ContentsFile:='packages.'+FileToDownload;
    if (NOT FileExists(FileToDownload)) then DownloadPackage(URL+FileToDownload,ContentsFile) ;
    if FileExists(FileToDownload) then if (NOT FileExists(ContentsFile)) then CopyFile(FileToDownload,ContentsFile);
    if FileExists(ContentsFile) then
    begin
      LibraryNames.LoadFromFile(ContentsFile);
      for FullName in LibraryNames do
      begin
        for StartName in LAZQTFILES do
        begin
          i:=Pos('('+StartName,FullName);
          if i>0 then
          begin
            FileToDownload:=Copy(FullName,i+1,MaxInt);
            i:=Pos(')',FileToDownload);
            if i=0 then i:=MaxInt;
            FileToDownload:=Copy(FileToDownload,1,i-1);
            Libraries.Add(FileToDownload);
          end;
        end;
      end;

      for FullName in Libraries do
      begin

        if (NOT FileExists(FullName)) then
        begin
          DownloadPackage(URL+FullName,FullName) ;
        end;

        if true then
        begin
          if FileExists(FullName) then
          begin
            // Extract the so-libraries
            ExtractMembers(FullName, WorkDir,['\lib\qt5\libQt5Core.so.*','\lib\qt5\libQt5Gui.so.*','\lib\qt5\libQt5Network.so.*','\lib\qt5\libQt5PrintSupport.so.*','\lib\qt5\libQt5Widgets.so.*']);
            ExtractMembers(FullName, WorkDir,['\lib\qt5\libQt5X11Extras.so.3.0']);
            ExtractMembers(FullName, WorkDir,['\lib\libQt6Core.so.*','\lib\libQt6DBus.so.*','\lib\libQt6Gui.so.*','\lib\libQt6PrintSupport.so.*','\lib\libQt6Widgets.so.*']);
          end;
        end;

      end;




    end;
    {$endif}

    WriteLn;
    WriteLn('To continue the dependency walk you would:');
    WriteLn('  1. Locate packages that provide each of the libraries above');
    WriteLn('     (on OpenBSD: pkg_info -P / pkg_info -L, or search other .tgz)');
    WriteLn('  2. Extract those packages the same way');
    WriteLn('  3. Recursively inspect their @wantlib / ELF NEEDED entries');
    WriteLn('  4. Stop when only system libraries (libc, libm, libpthread ...) remain');

    readln;

  finally
    Depends.Free;
    WantLibs.Free;
    ElfNeeded.Free;
    Libraries.Free;
    LibraryNames.Free;
  end;

  WriteLn;
  WriteLn('Done. Working files are under ', WorkDir);
end.
