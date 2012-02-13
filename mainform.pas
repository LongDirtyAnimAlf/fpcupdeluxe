unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, ValEdit, installer;

type

  { TForm1 }

  TForm1 = class(TForm)
    CommandMemo: TMemo;
    OutputMemo: TMemo;
    SkipFPC: TCheckBox;
    SkipLazarus: TCheckBox;
    Label11: TLabel;
    Label12: TLabel;
    LazarusPrimaryConfigPath: TDirectoryEdit;
    LazarusOptionsGroup: TGroupBox;
    FPCBootstrapDir: TDirectoryEdit;
    LazarusDirectory: TDirectoryEdit;
    FPCOptions: TEdit;
    FPCUpScriptName: TEdit;
    LazarusRevision: TEdit;
    LazarusOptions: TEdit;
    FPCURL: TComboBox;
    LazarusLinkName: TEdit;
    LazarusURL: TComboBox;
    GeneralOptionsGroup: TGroupBox;
    HeadButton: TButton;
    FPCRevision: TEdit;
    LazarusHeadButton: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HeadButtonClick(Sender: TObject);
    procedure LazarusHeadButtonClick(Sender: TObject);
    procedure NoFPCUPScriptButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  private
    { private declarations }
    FFPCUPParams: TStringList;
    FInstaller: TInstaller;
    procedure UpdateCommand;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.NoFPCUPScriptButtonClick(Sender: TObject);
begin
  FPCUpScriptName.Text:=EmptyStr;
end;

procedure TForm1.UpdateButtonClick(Sender: TObject);
begin
  Self.UpdateCommand;
  //todo: perform actual fpcup call
end;

procedure TForm1.UpdateCommand;
begin
  //todo: fix this
end;

procedure TForm1.HeadButtonClick(Sender: TObject);
begin
  FPCRevision.Text:=EmptyStr;;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFPCUPParams:=TStringList.Create;
  FInstaller:=TInstaller.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFPCUpParams.Free;
  FInstaller.Free;
end;

procedure TForm1.LazarusHeadButtonClick(Sender: TObject);
begin
  LazarusRevision.Text:=EmptyStr;
end;

{$R *.lfm}

end.

