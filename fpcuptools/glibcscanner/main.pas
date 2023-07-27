unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  FileUtil;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  FUNCTIONMAGIC = '.symver ';
var
  glibcfunctionsfromfile:TStringList;
  glibcfunctions:TStringList;
  PascalFile:TStringList;
  PascalFiles:TStringList;
  sh,fh,pf,sl:string;
  i,j:integer;
begin
  PascalFiles := TStringList.Create;

  PascalFile := TStringList.Create;

  glibcfunctionsfromfile:=TStringList.Create;

  glibcfunctions:=TStringList.Create;

  glibcfunctionsfromfile.LoadFromFile('glibc_2.27_x64.h');

  try
    for sh in glibcfunctionsfromfile do
    begin
      i:=Pos(FUNCTIONMAGIC,sh);
      if (i>0) then
      begin
        j:=Pos(',',sh,i);
        if (j>0) then
        begin
          Inc(i,Length(FUNCTIONMAGIC));
          fh:=Trim(Copy(sh,i,j-i));
          if fh[1]<>'_' then
          begin
            //glibcfunctions.Append('name '''+fh+''';');
            j:=Pos('@',sh,i);
            if (j>0) then
            begin
              Inc(j);
              pf:=Copy(sh,j,MaxInt);
              SetLength(pf,Length(pf)-3);
              glibcfunctions.AddObject('name '''+fh+''';',TObject(StrNew(PChar(pf))));
            end;
          end;
        end;
      end;
    end;
  finally
    glibcfunctionsfromfile.Free;
  end;

  // Check for 2.2.5 or newer from .h file

  //fh:='cmem';
  //glibcfunctions.Append('name '''+fh+''';');

  try
    FindAllFiles(PascalFiles, 'C:\fpcupsystems\trunk\fpcsrc\rtl', '*.pas;*.pp;*.p;*.inc', true); //find e.g. all pascal sourcefiles

    for pf in PascalFiles do
    begin
      Memo2.Lines.Append(pf);
      Application.ProcessMessages;
      PascalFile.LoadFromFile(pf);

      for sl in PascalFile do
      begin
        for sh in glibcfunctions do
        begin
          if (Pos(sh,sl)>0) then
          begin
            i:=glibcfunctions.IndexOf(sh);
            if (i<>-1) then
              fh:=StrPas(PChar(glibcfunctions.Objects[i]))
            else
              fh:='';

            //Memo1.Lines.Append(pf);
            Memo1.Lines.Append(sl);
            Memo1.Lines.Append(fh);
            Memo1.Lines.Append('');

            if fh<>'GLIBC_2.2.5' then
            begin
              Memo3.Lines.Append(pf);
              Memo3.Lines.Append(sl);
              Memo3.Lines.Append(fh);
              Memo3.Lines.Append('');

            end;

            Application.ProcessMessages;
          end;
        end;

      end;

    end;

  finally
    PascalFiles.Free;
    PascalFile.Free;
    glibcfunctions.Free;
  end;




end;

end.

