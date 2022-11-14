unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, AvgLvlTree;

type
  TMyData = class
    fContent:string;
    procedure SetContent(aContent:string);
  public
    Filename: string;
    property Content: string read fContent write SetContent;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    Memo1: TMemo;
    BaseLibsMemo: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    OpenDialog1: TOpenDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Tree: TIndexedAVLTree;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  process, FileUtil,LazFileUtils, StringHashList, Laz_AVL_Tree, StrUtils;


function CompareMyData(Data1, Data2: Pointer): integer;
begin
  //if (Data1=nil) OR (Data2=nil) then exit(0);
  Result:=CompareFilenamesIgnoreCase(TMyData(Data1).Filename,TMyData(Data2).Filename);
  //if (Result=0) then
  //  Result:=CompareFilenamesIgnoreCase(TMyData(Data1).Content,TMyData(Data2).Content);
end;


function GetDependencies(aTree: TIndexedAVLTree;aNode:TAVLTreeNode):TAVLTreeNode;
const
  LDDSEPARATOR = ' => ';
var
  NewOutputList:TStringList;
  MyData1: TMyData;
  i,j,k:integer;
  Output,Line,aFileName,aLibFile:string;
  LastNode:TAVLTreeNode;
begin
  result:=nil;
  if (aNode=nil) then exit;
  MyData1:=TMyData(aNode.Data);

  if FileExists(MyData1.Content) then
  begin

    NewOutputList:=TStringList.Create;
    try
      Output:='';
      RunCommand('ldd',[MyData1.Content],Output);
      if (Length(Output)=0) then exit;
      NewOutputList.Text:=Output;
      if (NewOutputList.Count=0) then exit;
      for i:=0 to Pred(NewOutputList.Count) do
      begin
        Line:=Trim(NewOutputList.Strings[i]);
        j:=Pos('linux-vdso',Line);
        if (j>0) then continue;
        j:=Pos(LDDSEPARATOR,Line);
        if (j>0) then
        begin
          aFileName:=Trim(Copy(Line,1,j));
          k:=Pos(' (',Line,j+4);
          if (k=0) then k:=MaxInt;
          Inc(j,Length(LDDSEPARATOR));
          aLibFile:=Trim(Copy(Line,j,k-j));

          if ( (Length(aFileName)<>0) AND (Length(aLibFile)<>0) AND (FileExists(aLibFile)) ) then
          begin
            MyData1:=TMyData.Create;
            MyData1.Filename:=aFileName;
            MyData1.Content:=aLibFile;
            if (aTree.Find(MyData1)=nil) then
            begin
              LastNode:=aTree.Add(MyData1);
              result:=LastNode;
              GetDependencies(aTree,LastNode);
            end
            else
            begin
              MyData1.Free;
            end;
          end;
        end;
      end;
    finally
      NewOutputList.Free;
    end;
  end;
end;

function StringListContains(SearchIn:TStringList; SearchFor:string; StartIndex:integer=0; CS:boolean=false): integer;
var
  Found:boolean=false;
  i:integer;
begin
  result:=-1;
  if (StartIndex>=SearchIn.Count) then exit;
  for i:=StartIndex to Pred(SearchIn.Count) do
  begin
    if CS then
      Found:=AnsiContainsStr(SearchIn[i],SearchFor)
    else
      Found:=AnsiContainsText(SearchIn[i],SearchFor);
    if Found then break;
  end;
  if Found then
    result:=i;
end;

function Which(const Executable: string): string;
var
  ExeName,FoundExe:string;
  {$IFDEF UNIX}
  OutputString: string;
  {$IFDEF DARWIN}
  OutputLines: TStringList;
  i: integer;
  {$ENDIF}
  {$ENDIF}
begin
  result:='';

  ExeName:=Executable;

  {$ifdef Windows}
  if ExtractFileExt(ExeName)='' then ExeName:=ExeName+'.exe';
  {$endif}

  if FileExists(ExeName) then result:=ExeName else
  begin
    FoundExe := ExeSearch(ExeName, '');
    if (NOT FileExists(FoundExe)) then
      FoundExe:=ExeSearch(ExeName,SysUtils.GetEnvironmentVariable('PATH'));
    if FileExists(FoundExe) then
      result:=FoundExe
    else
      result:=FindDefaultExecutablePath(ExeName);
  end;

  {$IFNDEF FREEBSD}
  if (NOT FileIsExecutable(result)) then result:='';
  {$ENDIF}

  {$IFDEF UNIX}
  if (NOT FileExists(result)) then
  begin
    RunCommand('which',[ExeName],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    OutputString:=Trim(OutputString);
    if ((OutputString<>'') and FileExists(OutputString)) then result:=OutputString;
  end;
  {$ENDIF}

  {$IFDEF DARWIN}
  if (NOT FileExists(result)) then
  begin
    RunCommand('type',['-a',ExeName],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    OutputString:=Trim(OutputString);
    if (OutputString=ExeName+' not found') then OutputString:='';
    if (OutputString<>'') then
    begin
      OutputLines:=TStringList.Create;
      try
        OutputLines.Text:=OutputString;
        OutputString:=OutputLines[Pred(OutputLines.Count)];
        i:=Pos(' is ',OutputString);
        if (i>0) then
          Delete(OutputString,1,i+4)
        else
          OutputString:='';
      finally
        OutputLines.Destroy;
      end;
      if ((OutputString<>'') and FileExists(OutputString)) then result:=OutputString;
    end;
  end;
  {$ENDIF}

  (*
  {$IFDEF UNIX}
  // Note: we're using external which because
  // FindDefaultExecutablePath
  // or
  // ExeSearch(Executable);
  // doesn't check if the user has execute permission
  // on the found file.
  // however
  // ExeSearch(Executable) ... if fpAccess (Executable,X_OK)=0 then ..... see http://www.freepascal.org/docs-html/rtl/baseunix/fpaccess.html
  ExecuteCommandCompat('which '+Executable,OutputString,false);
  // Remove trailing LF(s) and other control codes:
  while (length(OutputString)>0) and (ord(OutputString[length(OutputString)])<$20) do
    delete(OutputString,length(OutputString),1);
  {$ELSE}
  OutputString:=FindDefaultExecutablePath(Executable);
  {$ENDIF UNIX}
  // We could have checked for ExecuteCommandHidden exitcode, but why not
  // do file existence check instead:
  if (OutputString<>'') and fileexists(OutputString) then
  begin
    result:=OutputString;
  end
  else
  begin
    result:=''; //command failed
  end;
  *)
end;

function LibWhich(const aLibrary: string; out dir: string): boolean;
{$ifdef Unix}
const
  UNIXSEARCHDIRS : array [0..3] of string = (
  '/lib',
  '/lib64',
  '/usr/lib',
  '/usr/local/lib'
  );
  {$ifdef Haiku}
  HAIKUSEARCHDIRS : array [0..3] of string = (
  '/boot/system/lib/x86',
  '/boot/system/non-packaged/lib/x86',
  '/boot/system/lib',
  '/boot/system/non-packaged/lib'
  );
  {$endif}
var
  OutputString: string;
  aFile:string;
  i:integer;
  sd:string;
  OutputLines:TStringList;
{$endif}
begin
  result:=false;

  //SysUtils.GetCurrentDir;

  {$ifdef Unix}
  if (NOT result) then
  begin
    sd:=SysUtils.GetEnvironmentVariable('LIBRARY_PATH');
    if (Length(sd)=0) then sd:=SysUtils.GetEnvironmentVariable('LD_LIBRARY_PATH');
    if ((Length(sd)>0) AND (DirectoryExists(sd))) then
    begin
      OutputString:=FileSearch(aLibrary,sd);
      result:=(Length(OutputString)>0);
      if result then
      begin
        dir:=ExtractFileDir(OutputString);
      end;
    end;
  end;

  if (NOT result) then
  begin
    {$ifdef Haiku}
    for sd in HAIKUSEARCHDIRS do
    {$else}
    for sd in UNIXSEARCHDIRS do
    {$endif}
    begin
      OutputString:='';
      {$ifdef Haiku}
      {$ifndef CPUX86}
      if (RightStr(sd,4)='/x86') then continue;
      {$endif}
      {$endif}
      if DirectoryExists(sd) then
      begin
        if (NOT result) then
        begin
          //try to find a file
          //OutputString:=FileSearch(aLibrary,SysUtils.GetEnvironmentVariable('LIBRARY_PATH'));
          //OutputString:=FindFileInDirList(aLibrary,SysUtils.GetEnvironmentVariable('LIBRARY_PATH'));
          RunCommand('find',[sd,'-type','f','-name',aLibrary],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          result:=((Pos(aLibrary,OutputString)>0));
          if result then
          begin
            OutputLines:=TStringList.Create;
            try
              OutputLines.Text:=OutputString;
              i:=StringListContains(OutputLines,aLibrary);
              if (i<>-1) then
              begin
                aFile:=OutputLines[i];
                if FileExists(aFile) then
                  dir:=aFile
                else
                  result:=false;
              end;
            finally
              OutputLines.Destroy;
            end;
          end;

        end;
        if (NOT result) then
        begin
          //try to find a symlink to a file
          RunCommand('find',[sd,'-type','l','-name',aLibrary],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
          result:=((Pos(aLibrary,OutputString)>0));
          if result then
          begin
            OutputLines:=TStringList.Create;
            try
              OutputLines.Text:=OutputString;
              i:=StringListContains(OutputLines,aLibrary);
              if (i<>-1) then
              begin
                if FileExists(aFile) then
                  dir:=aFile
                else
                  result:=false;
              end;
            finally
              OutputLines.Destroy;
            end;
          end;
        end;
        if result then
        begin
          break;
        end;
      end;
    end;
  end;

  {$ifndef Haiku}
  if (NOT result) then
  begin
    sd:=Which('ldconfig');
    if (Length(sd)=0) then sd:='/sbin/ldconfig';
    if FileExists(sd) then
    begin
      OutputString:='';
      RunCommand('sh',['-c','"'+sd+' -p | grep '+aLibrary+'"'],OutputString,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
      result:=( (Pos(aLibrary,OutputString)>0) AND (Pos('not found',OutputString)=0));
      if result then
      begin
        OutputLines:=TStringList.Create;
        try
          OutputLines.Text:=OutputString;
          i:=StringListContains(OutputLines,aLibrary);
          if (i<>-1) then
          begin
            aFile:=OutputLines[i];
            i:=Pos(' /',aFile);
            if (i>0) then
            begin
              Delete(aFile,1,i);
              if FileExists(aFile) then
                dir:=aFile
              else
                result:=false;
            end;
          end;
        finally
          OutputLines.Destroy;
        end;
      end;
      if result then
      begin
      end;
    end;
  end;
  {$endif}
  {$endif}
end;

function ExtractFileNameWithoutExt(const FileName: string): string;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);

  I := 1;
  EndSep:=[ExtensionSeparator];
  while (I <= Length(Result)) and not CharInSet(Result[I],EndSep) do
    Inc(I);
  Result := Copy(Result, 1, I-1);
end;

function OccurrencesOfChar(const S: string; const C: char): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

procedure TMyData.SetContent(aContent:string);
begin
  if FileIsSymlink(aContent) then fContent:=GetPhysicalFilename(aContent,pfeException) else fContent:=aContent;
end;

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var
  i,j,k:integer;
  LibFiles : TStringList;
  aSearchFile,aRealFile,StartPath:string;
  MyData1:TMyData;
  LastNode:TAVLTreeNode;
begin
  Memo1.Lines.Clear;
  SelectDirectoryDialog1.FileName:='/usr/lib';
  //if SelectDirectoryDialog1.Execute then
  begin
    LibFiles := TStringList.Create;
    try
      FindAllFiles(LibFiles, SelectDirectoryDialog1.FileName, '*.o;*.so;*.so.*', false);
      FindAllFiles(LibFiles, SelectDirectoryDialog1.FileName, '*.o', true);

      for j:=0 to BaseLibsMemo.Lines.Count-1 do
      begin
        aSearchFile:=BaseLibsMemo.Lines.Strings[j];
        if aSearchFile[Length(aSearchFile)]<>'-' then aSearchFile:=aSearchFile+'.';

        i:=StringListContains(LibFiles,aSearchFile);
        while (i<>-1) do
        begin
          aRealFile:=LibFiles[i];
          Inc(i);
          i:=StringListContains(LibFiles,aSearchFile,i);
          k:=Pos('.so.',aRealFile);
          if (k>0) then
          begin
            Inc(k,4);
            aSearchFile:=Copy(aRealFile,1,k);
            k:=Pos('.',aRealFile,k);
            if (k<>0) then continue;
          end
          else
          begin
            aSearchFile:=aRealFile;
          end;
          aSearchFile:=ExtractFileName(aSearchFile);

          MyData1:=TMyData.Create;
          MyData1.Filename:=aSearchFile;
          MyData1.Content:=aRealFile;

          LastNode:=Tree.Find(MyData1);
          if (LastNode=nil) then
          begin
            LastNode:=Tree.Add(MyData1)
          end
          else
            MyData1.Free;
        end;
      end;
    finally
      LibFiles.Free;
    end;

    for LastNode in Tree do
    begin
      GetDependencies(Tree,LastNode);
    end;
    for LastNode in Tree do
    begin
      Memo1.Lines.Append('Found: '+ExtractFileName(TMyData(LastNode.Data).Filename));
      GetDependencies(Tree,LastNode);
    end;



  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i,j:integer;
  aLib,aLibFound,aExtraLibFound:string;
  MyData1:TMyData;
  LastNode:TAVLTreeNode;
begin
  Memo1.Lines.Clear;
  for j:=0 to Pred(Memo3.Lines.Count) do
  begin
    aLib:=Memo3.Lines.Strings[j];
    if LibWhich(aLib,aLibFound) then
    begin
      MyData1:=TMyData.Create;
      MyData1.Filename:=aLib;
      MyData1.Content:=aLibFound;
      LastNode:=Tree.Find(MyData1);
      if (LastNode=nil) then
        LastNode:=Tree.Add(MyData1)
      else
        MyData1.Free;

      i:=Pos('.so.',aLibFound);
      if i>0 then
      begin
        Inc(i,4);
        i:=Pos('.',aLibFound,i);
        aExtraLibFound:=Copy(aLibFound,1,i-1);

        MyData1:=TMyData.Create;
        MyData1.Filename:=aExtraLibFound;
        MyData1.Content:=aLibFound;

        LastNode:=Tree.Find(MyData1);
        if (LastNode=nil) then
          LastNode:=Tree.Add(MyData1)
        else
          MyData1.Free;
      end;
    end;
  end;

  for LastNode in Tree do
  begin
    GetDependencies(Tree,LastNode);
  end;
  for LastNode in Tree do
  begin
    Memo1.Lines.Append('Found: '+ExtractFileName(TMyData(LastNode.Data).Filename));
    GetDependencies(Tree,LastNode);
  end;

end;

procedure TForm1.Button5Click(Sender: TObject);
var
  FileName:string;
  MyData1:TMyData;
  LastNode:TAVLTreeNode;
begin
  Memo1.Lines.Clear;
  FileName:='/home/superdad/fpcupdeluxe/lazarus/lazarus';
  MyData1:=TMyData.Create;
  MyData1.Filename:=ExtractFileName(FileName);
  MyData1.Content:=FileName;
  LastNode:=Tree.NewNode;
  LastNode.Data:=MyData1;
  GetDependencies(Tree,LastNode);
  MyData1.Free;
  for LastNode in Tree do
  begin
    Memo1.Lines.Append('Found: '+ExtractFileName(TMyData(LastNode.Data).Filename));
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
const BASELIBS : array[0..11] of string =
  (
  'libpthread.so',
  'libdl.so',
  'libgdk-x11-2.0.so',
  'libgtk-x11-2.0.so',
  'libX11.so',
  'libgdk_pixbuf-2.0.so',
  'libgobject-2.0.so',
  'libgthread-2.0.so',
  'libgmodule-2.0.so',
  'libpango-1.0.so',
  'libcairo.so',
  'libatk-1.0.so'
  );
var
  MyData1:TMyData;
  LastNode:TAVLTreeNode;
  StartPath:ansistring;
  aTargetFile,aSourceFile,aFile:ansistring;
begin
  Memo2.Lines.Clear;
  Application.ProcessMessages;
  {$ifdef lcl}
  StartPath:=Application.ExeName;
  {$else}
  StartPath:=Paramstr(0);
  {$endif}
  StartPath:=ExtractFileDir(StartPath);
  //StartPath:='/tmp';
  StartPath:=IncludeTrailingPathDelimiter(StartPath)+'libs';
  ForceDirectories(StartPath);
  StartPath:=StartPath+DirectorySeparator;
  //if SelectDirectoryDialog1.Execute then
  begin
    for LastNode in Tree do
    begin
      MyData1:=TMyData(LastNode.Data);
      aFile:=MyData1.Filename;
      aSourceFile:=MyData1.Content;
      //if FileIsSymlink(aTargetFile) then aSourceFile:=GetPhysicalFilename(aTargetFile,pfeException) else aSourceFile:=aTargetFile;
      aTargetFile:=StartPath+aFile;
      Memo2.Lines.Append('Copy: '+aFile);
      Application.ProcessMessages;
      ForceDirectories(ExtractFileDir(aTargetFile));
      FileUtil.CopyFile(aSourceFile,aTargetFile,true);
      //SysCopyFile(aSourceFile,aTargetFile);
      if (Pos('libc.so.6',aFile)=1) then
      begin
        aFile:=aSourceFile;
        aSourceFile:=ExtractFilePath(aFile)+'libc.so';
        aTargetFile:=StartPath+'libc.so';
        if FileExists(aSourceFile) then
          FileUtil.CopyFile(aSourceFile,aTargetFile,true);
          //SysCopyFile(aSourceFile,aTargetFile);

        aSourceFile:=ExtractFilePath(aFile)+'libc_nonshared.a';
        aTargetFile:=StartPath+'libc_nonshared.a';
        if FileExists(aSourceFile) then
          FileUtil.CopyFile(aSourceFile,aTargetFile,true);
          //SysCopyFile(aSourceFile,aTargetFile);

      end;
    end;

    for aFile in BASELIBS do
    begin
      aTargetFile:=StartPath+aFile;
      if (NOT FileExists(aTargetFile)) then
      begin
        for LastNode in Tree do
        begin
          MyData1:=TMyData(LastNode.Data);
          if Pos(aFile,MyData1.Filename)=1 then
          begin
            aSourceFile:=MyData1.Content;
            FileUtil.CopyFile(aSourceFile,aTargetFile,true);
            break;
          end;
        end;
      end;
    end;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Tree:=TIndexedAVLTree.Create(@CompareMyData);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Tree.FreeAndClear;
  Tree.Free;
end;

end.

