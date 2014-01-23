unit mainform;

{$mode objfpc}{$H+}

{$DEFINE NOCONSOLE} //Please define this in project options so fpcuputil etc will not use writeln

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterIni, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn, ComCtrls, ExtCtrls, ValEdit, Menus,
  inifiles, processutils, process, fpcuputil, strutils,
  LCLIntf,LCLType,zipper;

{$IFDEF MSWINDOWS}
// On Windows, we can be certain a valid FPC install has
// windres, so use it.
{$R fpcup.rc}
{$ELSE}
// On other platforms we cannot be certain, so we trust/hope either
// - a previous windows compile
// - manual windres invocation
// has updated fpcup.res
{$R fpcup.res}
{$ENDIF MSWINDOWS}

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRun: TButton;
    btnDeletePPU: TButton;
    btnSaveLog: TButton;
    btnSaveINI: TButton;
    chkVerbose: TCheckBox;
    RepoDirectory: TDirectoryEdit;
    FileNameEdit: TFileNameEdit;
    gpbxDeletePPUs: TGroupBox;
    INIFileLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    InfoMemo: TMemo;
    mnuFPCUPWiki: TMenuItem;
    mnuFPCUPDownload: TMenuItem;
    mnuFile: TMenuItem;
    mnuHelp: TMenuItem;
    mnuShowFPCUPHelp: TMenuItem;
    mnuQuit: TMenuItem;
    OutputMemo: TMemo;
    EditTabs: TPageControl;
    OutputTab: TTabSheet;
    IniEditorTab: TTabSheet;
    ProfileLabel: TLabel;
    ProfileSelect: TComboBox;
    SaveDialog: TSaveDialog;
    SynIniHighlighter: TSynIniSyn;
    IniMemo: TSynMemo;
    TroubleshootingTab: TTabSheet;
    procedure btnDeletePPUClick(Sender: TObject);
    procedure btnSaveINIClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure ProfileSelectSelect(Sender: TObject);
    procedure RepoDirectoryChange(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuFPCUPDownloadClick(Sender: TObject);
    procedure mnuFPCUPWikiClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure mnuShowFPCUPHelpClick(Sender: TObject);
    procedure ProfileSelectGetItems(Sender: TObject);
  private
    FCurrentINIFile: string; //currently loaded ini file
    procedure LoadProfilesFromFile(INIFile: string);
    { private declarations }
    // Run actual fpcup update
    procedure UpdateCommand(Inifile, IniProfile: string);
  protected
    // Callback that writes output received from TProcessEx to memo
    procedure DumpOutput(Sender: TProcessEx; Output: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

const
  FPCUpExe='fpcup'; //fpcup executable filename (without .exe)


{ TForm1 }
procedure TForm1.btnRunClick(Sender: TObject);
begin
  UpdateCommand(FileNameEdit.FileName, ProfileSelect.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FPCUPLocation: string;
  UpProc: TProcessEx;
begin
  SaveDialog.InitialDir:=ExtractFilePath(ParamStr(0)); //application directory
  // Extract settings.ini if necessary
  try
    // Run fpcup --help so it generates relevant ini files.
    // Better than doing fpcuputil.SaveInisFromResource('settings.ini','settings_ini');
    // as we can mix and match fpcup and fpcupgui versions
    if not FileExistsUTF8(ExtractFilePath(ParamStr(0))+'settings.ini') then
    begin
      FPCUPLocation:=ExtractFilePath(ParamStr(0))+FPCUpExe+GetExeExt;
      if FileExistsUTF8(FPCUPLocation) then
      begin
        UpProc:=TProcessEx.Create(nil);
        try
          UpProc.Executable:=FPCUpExe+GetExeExt;
          UpProc.OnOutputM:=nil; //ignore output
          UpProc.Parameters.Add('--help');
          UpProc.Options:=UpProc.Options+[poNoConsole];
          try
            UpProc.Execute;
          except
            // ignore exceptions; user should recreate .ini himself
          end;
        finally
          UpProc.Free;
        end;
      end;
    end;

    //Seems to be a bug in tpagecontrol: last tab is active?!?
    EditTabs.ActivePage:=INiEditorTab;
  except
    //Ignore exceptions - file just won't exist.
  end;
end;

procedure TForm1.mnuFPCUPDownloadClick(Sender: TObject);
begin
  OpenURL('http://bitbucket.org/reiniero/fpcup/downloads');
end;

procedure TForm1.mnuFPCUPWikiClick(Sender: TObject);
begin
  OpenURL('http://wiki.lazarus.freepascal.org/fpcup');
end;

procedure TForm1.mnuQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.mnuShowFPCUPHelpClick(Sender: TObject);
var
  UpProc: TProcessEx;
begin
  UpProc:=TProcessEx.Create(nil);
  try
    UpProc.Executable:=FPCUpExe+GetExeExt;
    UpProc.OnOutputM:=@DumpOutput;
    UpProc.Parameters.Add('--help');
    UpProc.Options:=UpProc.Options+[poNoConsole];
    try
      Screen.Cursor:=crHourGlass;
      OutputMemo.Clear;
      EditTabs.ActivePage:=OutputTab; //switch to output tab
      Application.ProcessMessages;
      UpProc.Execute;
      OutputMemo.SelStart:=0; //move to beginning of output
      OutputMemo.SelLength:=0;
    finally
      Screen.Cursor:=crDefault;
    end;
  finally
    UpProc.Free;
  end;
end;

procedure TForm1.ProfileSelectGetItems(Sender: TObject);
begin
  // Check for empty combobox but valid filename
  if ProfileSelect.Items.Count=0 then
  begin
    if (FileNameEdit.FileName<>'') and (FileExistsUTF8(FileNameEdit.FileName)) then
    begin
      if UpperCase((FileNameEdit.FileName))='FPCUP.INI' then
        ShowMessage('Warning: fpcup.ini does not contain fpcup user profiles but external module definitions. Try settings.ini.');
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
  FCurrentINIFile:=INIFile;
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
  FPCUpLocation: string;
  ResultCode: integer;
  UpProc: TProcessEx;
begin
  FPCUPLocation:=ExtractFilePath(ParamStr(0))+FPCUpExe+GetExeExt;
  if not(FileExistsUTF8(FPCUPLocation)) then
  begin
    ShowMessage('Could not find fpcup executable '+FPCUPLocation+LineEnding+
    'Please make sure fpcup is present and has the proper permissions. Aborting.');
    exit;
  end;

  UpProc:=TProcessEx.Create(nil);
  try
    UpProc.Executable:=FPCUPLocation;
    UpProc.OnOutputM:=@DumpOutput;
    if chkVerbose.Checked then
    begin
      UpProc.Parameters.Add('--verbose');
    end;
    if IniFile<>'' then
      UpProc.Parameters.Add('--inifile='+IniFile);
    if IniProfile<>'' then
      UpProc.Parameters.Add('--inisection='+IniProfile);
    //This will lead to lots of pop ups of make.exe which are actually more annoying
    //UpProc.Options:=UpProc.Options+[poNoConsole];
    try
      Screen.Cursor:=crHourGlass;
      OutputMemo.Clear;
      EditTabs.ActivePage:=OutputTab; //switch to output tab
      Application.ProcessMessages;
      UpProc.Execute;
      OutputMemo.SelStart:=0; //move to beginning of output
      OutputMemo.SelLength:=0;
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

procedure TForm1.DumpOutput(Sender: TProcessEx; Output: string);
var
  LastEnd: integer;
begin
  // Avoid duplicate line endings leading to unintended vertical whitespace
  LastEnd:=RPos(LineEnding,Output);
  if LastEnd=1+Length(Output)-Length(LineEnding) then
    OutputMemo.Append(Copy(Output,1,LastEnd-1))
  else
    OutputMemo.Append(Output);
  // Give GUI chance to refresh so user doesn't think it hangs:
  Sleep(5);
  Application.ProcessMessages;
end;


procedure TForm1.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
begin
  LoadProfilesFromFile(Value);
end;

procedure TForm1.RepoDirectoryChange(Sender: TObject);
begin

end;

procedure TForm1.btnDeletePPUClick(Sender: TObject);
var
  Extensions: TStringList;
  Reply: integer;
begin
  if not(DirectoryExistsUTF8(RepoDirectory.Directory)) then
  begin
    ShowMessage('No directory selected. Please select the directory where fpcup downloads the Lazarus or FPC sources from subversion.');
    exit;
  end;
  Reply:=Application.MessageBox(PChar('Are you sure you want to delete these files from  '+
    RepoDirectory.Directory + '?'),
    'Deleting .ppu, .a, .o files',MB_ICONQUESTION+MB_YESNO);
  if reply=IDYES then
  begin
    Extensions:=TStringList.Create;
    try
      Extensions.Add('.a');
      Extensions.Add('.o');
      Extensions.Add('.ppu');
      if DeleteFilesExtensionsSubdirs(RepoDirectory.Directory,Extensions,'') then
        ShowMessage('Deleted .ppu, .a, .o files. Please run fpcup again to get back all required files (or run svn up).')
      else
        ShowMessage('Error deleting .ppu, .a, .o files. Please run svn up to get back all required files.');
    finally
      Extensions.Free;
    end;
  end;
end;

procedure TForm1.btnSaveINIClick(Sender: TObject);
begin
  SaveDialog.Filter:='INI files (*.ini)|*.ini';
  SaveDialog.FileName:=FCurrentINIFile;
  if SaveDialog.Execute then
  begin
    FCurrentINIFile:=SaveDialog.FileName;
    Inimemo.Lines.SaveToFile(FCurrentINIFile);
  end;
end;

procedure TForm1.btnSaveLogClick(Sender: TObject);
var
  TempStream: TMemoryStream;
  ZipMachine: TZipper;
  ZipEntry: TZipFileEntry;
begin
  SaveDialog.Filter:='Text file (*.txt)|*.txt|Zipped text file (*.zip)|*.zip';
  if SaveDialog.Execute then
  begin
    try
      case UpperCase(sysutils.ExtractFileExt(SaveDialog.FileName)) of
      '.ZIP':
        begin
          TempStream:=TMemoryStream.Create;
          ZipMachine:=TZipper.Create;
          try
            ZipMachine.FileName:=SaveDialog.FileName;
            OutputMemo.Lines.SaveToStream(TempStream);
            TempStream.Position:=0;
            ZipEntry:=ZipMachine.Entries.AddFileEntry(TempStream,'fpcupoutput.txt');
            ZipMachine.ZipAllFiles;
          finally
            TempStream.Free;
            ZipMachine.Free;
          end;
        end
      else {.txt}
        OutputMemo.Lines.SaveToFile(SaveDialog.FileName);
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Error while trying to save file: '+E.Message);
      end;
    end;
  end;
end;

procedure TForm1.ProfileSelectSelect(Sender: TObject);
var
  i:integer;
begin
  // Move to proper y position in synedit to show just selected profile
  IniMemo.BeginUpdate(false);
  i:=IniMemo.Lines.IndexOf('['+ProfileSelect.Text+']');
  if i>-1 then
    IniMemo.CaretY:=i+1; //caret is 1-based
  IniMemo.EndUpdate;
end;

{$R *.lfm}

end.

