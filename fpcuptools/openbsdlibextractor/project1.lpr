program project1;
{ inspect_pkg.pas
  FPC program to inspect an OpenBSD package archive, extract its
  shared library, detect package dependencies and ELF NEEDED entries.

  Compile:  fpc -O2 inspect_pkg.pas
  Usage:    ./inspect_pkg /path/to/qtx11extras-5.15.18pl0.tgz [outdir]
}

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process, StrUtils;

type
  TStringListHelper = class helper for TStringList
    function ContainsCI(const S: string): Boolean;
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
  Archive, OutDir, WorkDir, SoPath, ContentsFile: string;
  Name, Version: string;
  Depends, WantLibs, ElfNeeded, Libraries: TStringList;
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
  A,M: string;
begin
  Result:=True;
  A:=ExtractFileName(ChangeFileExt(Arch,'.tar'));
  A:=ConcatPaths([Dest,A]);
  if (NOT FileExists(A)) then
  begin
    Result := RunCommand('.\7za.exe',['x',Arch,'-o'+Dest],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    if not Result then
      WriteLn(StdErr, '7za failed: ', OutS);
  end;
  if Result then
  begin
    for M in Members do
    begin
      if (NOT FileExists(ConcatPaths([Dest,M]))) then
      begin
        Result := RunCommand('.\7za.exe',['x',A,'-o'+Dest,M],OutS,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
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
        'name':    Name := Val;
        'version': Version := Val;
        'depend':
          begin
            P:=RPos(':',Val);
            Dep:=Copy(Val,P+1,MaxInt)+'.tgz';
            Depends.Add(Dep);
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
function StoreLibrary(const RelPath, SrcRoot, DestRoot: string): string;
var
  Src, Dest, DestDir: string;
begin
  Src := IncludeTrailingPathDelimiter(SrcRoot) + RelPath;
  Dest := IncludeTrailingPathDelimiter(DestRoot) + 'libs' +
          DirectorySeparator + ExtractFileName(RelPath);
  DestDir := ExtractFilePath(Dest);
  ForceDirectories(DestDir);
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
    WriteLn(StdErr, 'Archive not found: ', Archive);
    Halt(1);
  end;

  if ParamCount >= 2 then
    OutDir := ExpandFileName(ParamStr(2))
  else
    OutDir := GetCurrentDir;

  ForceDirectories(OutDir);

  WorkDir := IncludeTrailingPathDelimiter(OutDir) + 'pkgwork';
  if DirectoryExists(WorkDir) then
    DeleteDirectoryEx(WorkDir);
  ForceDirectories(WorkDir);

  Depends   := TStringList.Create;
  WantLibs  := TStringList.Create;
  ElfNeeded := TStringList.Create;
  Libraries := TStringList.Create;

  try
    Depends.Sorted := True;   Depends.Duplicates := dupIgnore;
    WantLibs.Sorted := True;  WantLibs.Duplicates := dupIgnore;
    ElfNeeded.Sorted := True; ElfNeeded.Duplicates := dupIgnore;

    { 1. Extract metadata and the library }
    WriteLn('Extracting metadata and library from ', Archive, ' ...');
    if not ExtractMembers(Archive, WorkDir,
         ['+CONTENTS']) then
      Halt(1);

    ContentsFile := IncludeTrailingPathDelimiter(WorkDir) + '+CONTENTS';
    if not FileExists(ContentsFile) then
    begin
      WriteLn(StdErr, '+CONTENTS not found inside archive');
      Halt(1);
    end;

    { 2. Parse package metadata }
    ParseContents(ContentsFile);

    WriteLn;
    WriteLn('=== Package information ===');
    WriteLn('Name     : ', Name);
    if Version <> '' then
      WriteLn('Version  : ', Version);

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
        ExtractMembers(Archive, WorkDir,[Libraries[I]]);
        SoPath := StoreLibrary(Libraries[I], WorkDir, OutDir);
      end;
    end;

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
  end;

  WriteLn;
  WriteLn('Done. Working files are under ', WorkDir);
end.
