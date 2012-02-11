unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    OutputTabs: TPageControl;
    OutputTab: TTabSheet;
    CommandTab: TTabSheet;
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
uses installer;

{$R *.lfm}

end.

