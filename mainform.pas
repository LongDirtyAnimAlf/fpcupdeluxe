unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    TabControl1: TTabControl;
    UpdateButton: TButton;
    FPCDirectory: TDirectoryEdit;
    Label2: TLabel;
    FPCURL: TEdit;
    FPC: TGroupBox;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

