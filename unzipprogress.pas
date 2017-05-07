unit unzipprogress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TProgressForm }

  TProgressForm = class(TForm)
    FileCountLabel: TLabel;
    WaitLabel: TLabel;
    ProgressBar: TProgressBar;
    FileLabel: TLabel;
  private
  public
    procedure DoOnZipProgress(Sender: TObject; Pct: double);
    procedure DoOnZipFile(Sender: TObject; aFile: string; FileCnt, TotalFileCnt:cardinal);
    procedure DoOnZipCompleted(Sender: TObject);
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.lfm}

procedure TProgressForm.DoOnZipProgress(Sender: TObject; Pct: double);
begin
  if (NOT ProgressBar.Visible) then ProgressBar.Visible:=True;
  ProgressBar.Position:=Round(Pct*100);
end;

procedure TProgressForm.DoOnZipFile(Sender: TObject; aFile: string; FileCnt, TotalFileCnt:cardinal);
begin
  FileLabel.Caption:='Extracting '+aFile;
  FileCountLabel.Caption:='#'+InttoStr(FileCnt)+' out of #'+InttoStr(TotalFileCnt);
end;

procedure TProgressForm.DoOnZipCompleted(Sender: TObject);
begin
  Application.ProcessMessages;
  Self.ModalResult:=mrOk;
end;

end.

