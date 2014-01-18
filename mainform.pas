unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterIni, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn, ComCtrls, ExtCtrls, ValEdit,
  inifiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    CommandMemo: TMemo;
    FileNameEdit: TFileNameEdit;
    INIFileLabel: TLabel;
    OutputMemo: TMemo;
    OutputTabs: TPageControl;
    OutputTab: TTabSheet;
    CommandTab: TTabSheet;
    EditTabs: TPageControl;
    OptionsTab: TTabSheet;
    IniEditorTab: TTabSheet;
    ProfileLabel: TLabel;
    ProfileSelect: TComboBox;
    SynIniHighlighter: TSynIniSyn;
    SynMemo: TSynMemo;
    UpdateButton: TButton;
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  private
    { private declarations }
    FFPCUPParams: TStringList;
    procedure UpdateCommand;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }


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


procedure TForm1.FormCreate(Sender: TObject);
begin
  FFPCUpParams:=TStringList.Create;
  FFPCUpParams.Delimiter:=' ';
  FFPCUpParams.StrictDelimiter:=false; //I believe this works best for output
end;

procedure TForm1.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
var
  MyIniFile : TIniFile;
  Sections: TStringList;
begin
  // Load selected ini file
  SynMemo.BeginUpdate(false);
  SynMemo.Lines.LoadFromFile(Value);
  MyIniFile:=TIniFile.Create(Value,true);
  Sections:=TStringList.Create;
  try
    Sections.Duplicates:=dupIgnore; //just pick the first one or whatever
    Sections.Sorted:=true;
    MyIniFile.ReadSections(Sections);
    ProfileSelect.Clear;
    ProfileSelect.Items.AddStrings(Sections);
  finally
    SynMemo.EndUpdate;
    Sections.Free;
    MyIniFile.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFPCUpParams.Free;
end;


{$R *.lfm}

end.

