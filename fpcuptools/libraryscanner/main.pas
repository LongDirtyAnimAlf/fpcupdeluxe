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
    LibraryMemo: TMemo;
    LibraryNotFoundMemo: TMemo;
    LibraryLocationMemo: TMemo;
    stLocation: TStaticText;
    stFound: TStaticText;
    stNotFound: TStaticText;
    procedure btnStartScanClick(Sender: TObject);
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
      GetAndSaveLibs(Application.Location);
      LibraryMemo.Text:=LibraryList.Text;
      LibraryNotFoundMemo.Text:=LibraryNotFoundList.Text;
      LibraryLocationMemo.Text:=LibraryLocationList.Text;
    finally
      Free;
    end;
  end;
end;

end.

