unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Zipper;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  zipper:TZipper;
begin
  if SelectDirectoryDialog1.Execute then
  begin
    zipper:=TZipper.Create;
    try
      zipper.FileName:=SelectDirectoryDialog1.FileName;
      zipper.Entries.AddFileEntry(DirectoryEdit1.Directory+'_copy');
      //Entries[ACount].Attributes:=$0100755 shl 16;
      zipper.ZipAllFiles;
    finally
      zipper.Free;
    end;
  end;
end;

end.

