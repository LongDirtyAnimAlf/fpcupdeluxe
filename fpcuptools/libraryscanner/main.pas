unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartScan: TButton;
    chkQT: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    LibraryMemo: TMemo;
    LibraryNotFoundMemo: TMemo;
    LibraryLocationMemo: TMemo;
    OpenDialog1: TOpenDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    stLocation: TStaticText;
    stFound: TStaticText;
    stNotFound: TStaticText;
    procedure btnStartScanClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  scannercore;

{ TForm1 }

procedure TForm1.btnStartScanClick(Sender: TObject);
begin
  with TScannerCore.Create do
  begin
    try
      {$ifdef Windows}
      ReadelfBinary:=Edit1.Text;
      LibraryLocation:=ExcludeTrailingPathDelimiter(Edit2.Text);
      {$endif}
      GetAndSaveLibs(Application.Location);
      LibraryMemo.Text:=LibraryList.Text;
      LibraryNotFoundMemo.Text:=LibraryNotFoundList.Text;
      LibraryLocationMemo.Text:=LibraryLocationList.Text;
    finally
      Free;
    end;
  end;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Edit1.Text:=OpenDialog1.FileName;

  end;
end;

procedure TForm1.Edit2Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    Edit2.Text:=SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifndef Windows}
  Edit1.Visible:=false;
  Edit2.Visible:=false;
  {$endif}
end;

end.

