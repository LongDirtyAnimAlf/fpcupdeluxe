unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, ValEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label11: TLabel;
    Label12: TLabel;
    LazarusPrimaryConfigPath: TDirectoryEdit;
    LazarusOptionsGroup: TGroupBox;
    FPCBootstrapDir: TDirectoryEdit;
    LazarusDirectory: TDirectoryEdit;
    FPCOptions: TEdit;
    FPCRevision2: TEdit;
    LazarusRevision: TEdit;
    LazarusOptions: TEdit;
    FPCURL: TComboBox;
    LazarusLinkName: TEdit;
    LazarusURL: TComboBox;
    GeneralOptionsGroup: TGroupBox;
    HeadButton: TButton;
    FPCRevision: TEdit;
    HeadButton1: TButton;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    LazarusOptionsChoices: TListBox;
    NoFPCUPScriptButton: TButton;
    OutputTabs: TPageControl;
    OutputTab: TTabSheet;
    CommandTab: TTabSheet;
    UpdateButton: TButton;
    FPCDirectory: TDirectoryEdit;
    Label2: TLabel;
    FPCOptionsGroup: TGroupBox;
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

