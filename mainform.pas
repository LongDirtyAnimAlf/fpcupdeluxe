unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, ValEdit, installermanager;

type

  { TForm1 }

  TForm1 = class(TForm)
    CommandMemo: TMemo;
    FPCBootstrapDir: TDirectoryEdit;
    Label13: TLabel;
    OutputMemo: TMemo;
    SkipFPC: TCheckBox;
    SkipLazarus: TCheckBox;
    Label11: TLabel;
    Label12: TLabel;
    LazarusPrimaryConfigPath: TDirectoryEdit;
    LazarusOptionsGroup: TGroupBox;
    BinutilsDir: TDirectoryEdit;
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
    BinutilsDirLabel: TLabel;
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
    FManager: TFPCupManager;
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
  //todo: perform actual call to installer, analogous to how fpcup would work
end;

procedure TForm1.UpdateCommand;
begin
  //First update installer properties depending on options chosen

  FFPCUpParams.Clear;
  //fill FFPCUPParms out using FManager settings
  CommandMemo.Text:='fpcup '+FFPCUpParams.DelimitedText; //Show how fpcup cli would be invoked
end;

procedure TForm1.HeadButtonClick(Sender: TObject);
begin
  FPCRevision.Text:='HEAD';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFPCUpParams:=TStringList.Create;
  FFPCUpParams.Delimiter:=' ';
  FFPCUpParams.StrictDelimiter:=false; //I believe this works best for output
  FManager:=TFPCupManager.Create;

  LazarusPrimaryConfigPath.Directory:=FManager.LazarusPrimaryConfigPath; //Get a default primary config path
  // Set up defaults depending on platform
  {$IFDEF WINDOWS}
  //Defaults already set up in design mode GUI or in code above
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  BinutilsDir.Visible:=false; //No use on Unix
  FPCBootstrapDir.Directory:='~/fpcbootstrap';
  FPCDirectory.Directory:='~/fpc';
  LazarusDirectory.Directory:='~/lazarus';
  {$ENDIF UNIX}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFPCUpParams.Free;
  FManager.Free;
end;

procedure TForm1.LazarusHeadButtonClick(Sender: TObject);
begin
  LazarusRevision.Text:='HEAD';
end;

{$R *.lfm}

end.

