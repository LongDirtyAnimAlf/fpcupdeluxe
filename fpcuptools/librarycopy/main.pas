unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    BaseLibsMemo: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  FileUtil,LazFileUtils;

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

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  i:integer;
  LibFiles, LibFilesNoDups : TStringList;
begin
  //if SelectDirectoryDialog1.Execute then
  SelectDirectoryDialog1.FileName:='C:\fpctrunk2\cross\lib\x86_64-linux';
  begin
    LibFiles := TStringList.Create;
    LibFilesNoDups := TStringList.Create;
    LibFilesNoDups.Sorted:=true;
    LibFilesNoDups.Duplicates:=dupIgnore;
    try
      FindAllFiles(LibFiles, SelectDirectoryDialog1.FileName, '*.o;*.so;*.so.?;*.so.??;*.so.???', true);
      for i:=0 to (LibFiles.Count-1) do
      begin
        Memo1.Lines.Append(ExtractFileName(LibFiles[i]));
        LibFilesNoDups.Append(ExtractFileNameWithoutExt(LibFiles[i]));
      end;
      {
      for i:=0 to (LibFilesNoDups.Count-1) do
      begin
        Memo2.Lines.Append(LibFilesNoDups[i]);
      end;
      }
    finally
      LibFiles.Free;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i,j:integer;
  LibFiles, LibFilesNoDups : TStringList;
  aFile,aRealFile,StartPath:string;
begin
  {$ifdef lcl}
  StartPath:=Application.ExeName;
  {$else}
  StartPath:=Paramstr(0);
  {$endif}
  StartPath:=ExtractFileDir(StartPath);
  StartPath:=IncludeTrailingPathDelimiter(StartPath)+'libs';
  ForceDirectories(StartPath);
  StartPath:=StartPath+DirectorySeparator;
  if SelectDirectoryDialog1.Execute then
  begin
    LibFiles := TStringList.Create;
    try
      FindAllFiles(LibFiles, SelectDirectoryDialog1.FileName, '*.o;*.so;*.so.*', false);
      FindAllFiles(LibFiles, SelectDirectoryDialog1.FileName, 'libc_nonshared.a', false);
      for i:=0 to (LibFiles.Count-1) do
      begin
        aFile:=LibFiles[i];
        for j:=0 to BaseLibsMemo.Lines.Count-1 do
        begin
          if Pos(BaseLibsMemo.Lines.Strings[j],ExtractFileName(LibFiles[i]))=1 then
          begin
            if FileIsSymlink(aFile) then aRealFile:=GetPhysicalFilename(aFile,pfeException) else aRealFile:=aFile;
            aFile:=ExtractFileName(aFile);
            if OccurrencesOfChar(aFile,'.')>(OccurrencesOfChar(BaseLibsMemo.Lines.Strings[j],'.')+2) then
            begin
              if Pos('.so',BaseLibsMemo.Lines.Strings[j])=0 then continue;
            end;
            Memo1.Lines.Append(aFile);
            FileUtil.CopyFile(aRealFile,StartPath+aFile);
          end;
        end;
      end;
    finally
      LibFiles.Free;
    end;
  end;
end;

end.

