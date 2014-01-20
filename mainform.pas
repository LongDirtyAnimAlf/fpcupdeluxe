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
    btnRun: TButton;
    chkVerbose: TCheckBox;
    FileNameEdit: TFileNameEdit;
    INIFileLabel: TLabel;
    OutputMemo: TMemo;
    EditTabs: TPageControl;
    OutputTab: TTabSheet;
    IniEditorTab: TTabSheet;
    ProfileLabel: TLabel;
    ProfileSelect: TComboBox;
    SynIniHighlighter: TSynIniSyn;
    IniMemo: TSynMemo;
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure btnRunClick(Sender: TObject);
    procedure ProfileSelectGetItems(Sender: TObject);
  private
    procedure LoadProfilesFromFile(INIFile: string);
    { private declarations }
    // Run actual fpcup update
    procedure UpdateCommand(Inifile, IniProfile: string);
  protected
    // Callback that writes output received from TProcessEx to memo
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

procedure TForm1.ProfileSelectGetItems(Sender: TObject);
begin
  // Check for empty combobox but valid filename
  if ProfileSelect.Items.Count=0 then
  begin
    if (FileNameEdit.FileName<>'') and (FileExistsUTF8(FileNameEdit.FileName)) then
    begin
      LoadProfilesFromFile(FileNameEdit.FileName);
    end;
  end;
end;

procedure TForm1.LoadProfilesFromFile(INIFile: string);
var
  Sections: TStringList;
  MyIniFile: TIniFile;
begin
  // Load selected ini file
  IniMemo.BeginUpdate(false);
  IniMemo.Lines.LoadFromFile(INIFile);
  MyIniFile:=TIniFile.Create(INIFile, true);
  Sections:=TStringList.Create;
  try
    MyIniFile.ReadSections(Sections);
    ProfileSelect.Clear;
    ProfileSelect.Items.Assign(Sections); //bug in .AddStrings LCL combobox handling
  finally
    IniMemo.EndUpdate;
    Sections.Free;
    MyIniFile.Free;
  end;
end;

procedure TForm1.UpdateCommand(Inifile, IniProfile: string);
var
  ResultCode: integer;
  UpProc: TProcessEx;
begin
  UpProc:=TProcessEx.Create(nil);
  try
    UpProc.Executable:='fpcup'+GetExeExt;
    UpProc.OnOutputM:=@DumpOutput;
    if chkVerbose.Checked then
    begin
      UpProc.Parameters.Add('--verbose');
    end;
    if IniFile<>'' then
      UpProc.Parameters.Add('--inifile='+IniFile);
    if IniProfile<>'' then
      UpProc.Parameters.Add('--inisection='+IniProfile);
    //CommandMemo.Text:=UpProc.ResultingCommand;
    try
      Screen.Cursor:=crHourGlass;
      OutputMemo.Clear;
      UpProc.Execute;
    finally
      Screen.Cursor:=crDefault;
    end;

    ResultCode:=UpProc.ExitStatus;
    if ResultCode<>0 then
      ShowMessage('Error running fpcup: result code: '+inttostr(ResultCode))
    else
      ShowMessage('Succesfully ran fpcup');
  finally
    UpProc.Free;
  end;
end;

procedure TForm1.DumpOutput(Sender: TProcessEx; output: string);
begin
  OutputMemo.Append(output);
  // Give GUI chance to refresh so user doesn't think it hangs:
  Sleep(5);
  Application.ProcessMessages;
end;


procedure TForm1.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
begin
  LoadProfilesFromFile(Value);
end;

{$R *.lfm}

end.

