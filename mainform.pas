unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterIni, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn, ComCtrls, ExtCtrls, ValEdit,
  inifiles, processutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    chkVerbose: TCheckBox;
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
    IniMemo: TSynMemo;
    btnRun: TButton;
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure btnRunClick(Sender: TObject);
  private
    { private declarations }
    procedure UpdateCommand(Inifile, IniProfile: string);
  protected
    procedure DumpOutput(Sender: TProcessEx; output: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

// Quote arguments if needed for processutils (e.g. on Windows)
function DoubleQuoteIfNeeded(FileName: string): string;
begin
  {$IFDEF MSWINDOWS}
  // Unfortunately, we need to double quote in case there's spaces in the path and it's e.g. a .cmd file
  if Copy(FileName, 1, 1) <> '"' then
    Result := '"' + FileName + '"';
  {$ELSE}
  Result := filename;
  {$ENDIF}
end;

{ TForm1 }


procedure TForm1.btnRunClick(Sender: TObject);
begin
  UpdateCommand(FileNameEdit.FileName, ProfileSelect.Text);
end;

procedure TForm1.UpdateCommand(Inifile, IniProfile: string);
var
  UpProc: TProcessEx;
begin
  //First update installer properties depending on options chosen
  UpProc:=TProcessEx.Create(nil);
  try
    UpProc.Executable:='fpcup'+GetExeExt;
    if chkVerbose.Checked then
    begin
      UpProc.Parameters.Add('--verbose');
      UpProc.OnOutputM:=@DumpOutput;
    end
    else
    begin
      UpProc.OnOutputM:=nil;
    end;
    if IniFIle<>'' then
      UpProc.Parameters.Add('--inifile='+IniFile);
    if IniProfile<>'' then
      UpProc.Parameters.Add('--inisection='+IniProfile);
    CommandMemo.Text:=UpProc.ResultingCommand;
    UpProc.Execute;
    //todo: handle return code, verbose output
  finally
    UpProc.Free;
  end;
end;

procedure TForm1.DumpOutput(Sender: TProcessEx; output: string);
begin
  OutputMemo.Append(output);
end;


procedure TForm1.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
var
  MyIniFile : TIniFile;
  Sections: TStringList;
begin
  // Load selected ini file
  IniMemo.BeginUpdate(false);
  IniMemo.Lines.LoadFromFile(Value);
  MyIniFile:=TIniFile.Create(Value,true);
  Sections:=TStringList.Create;
  try
    Sections.Duplicates:=dupIgnore; //just pick the first one or whatever
    Sections.Sorted:=true;
    MyIniFile.ReadSections(Sections);
    ProfileSelect.Clear;
    ProfileSelect.Items.Assign(Sections); //bug in .AddStrings LCL combobox handling
  finally
    IniMemo.EndUpdate;
    Sections.Free;
    MyIniFile.Free;
  end;
end;

{$R *.lfm}

end.

