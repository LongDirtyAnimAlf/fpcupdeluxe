unit mainform;

{$mode objfpc}{$H+}

{$DEFINE NOCONSOLE} //Please define this in project options so fpcuputil etc will not use writeln

//working on highlighter
{.$DEFINE HLREADY}

//to do: highlighting log
(*
  Highlights:
  - fpcup: debug => gray
  - fpcup: info => green
  - fpcup: warning => orange
  - fpcup: error => red
  - error: => red

  Folds at:
  - fpcup: statements
  => see
  http://forum.lazarus.freepascal.org/index.php/topic,23411.0.html
*)
interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterIni, SynEdit, Forms,
  Controls, Graphics, Dialogs, StdCtrls, EditBtn, ComCtrls, ExtCtrls, ValEdit,
  Menus, inifiles, processutils, process, fpcuputil, strutils, LCLIntf, LCLType,
  XMLPropStorage, zipper, svnclient, SynEditKeyCmds
  {$IFDEF HLREADY}, fpcuploghighlighter {$ENDIF}
  , types;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSwitch: TButton;
    btnRun: TButton;
    btnDeletePPU: TButton;
    btnSaveLog: TButton;
    btnSaveINI: TButton;
    chkVerbose: TCheckBox;
    OutputMemo: TSynEdit;
    SVNRepoSwitchTo: TEdit;
    gpbxSwitch: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    RepoDirectory: TDirectoryEdit;
    INIFileSelectEdit: TFileNameEdit;
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
    EditTabs: TPageControl;
    OutputTab: TTabSheet;
    IniEditorTab: TTabSheet;
    ProfileLabel: TLabel;
    ProfileSelect: TComboBox;
    RepoDirectorySwitch: TDirectoryEdit;
    SaveDialog: TSaveDialog;
    SynIniHighlighter: TSynIniSyn;
    IniMemo: TSynEdit;
    TroubleshootingTab: TTabSheet;
    XMLPropStorage: TXMLPropStorage;
    procedure btnDeletePPUClick(Sender: TObject);
    procedure btnSaveINIClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure btnSwitchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure INIFileSelectEditExit(Sender: TObject);
    procedure ProfileSelectSelect(Sender: TObject);
    procedure INIFileSelectEditAcceptFileName(Sender: TObject; var Value: String);
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuFPCUPDownloadClick(Sender: TObject);
    procedure mnuFPCUPWikiClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure mnuShowFPCUPHelpClick(Sender: TObject);
    procedure ProfileSelectGetItems(Sender: TObject);
  private
    // Currently loaded ini file:
    FCurrentINIFile: string;
    FOneTimeSetup: boolean;
    {$IFDEF HLREADY}
    // Highlighter for fpcup log output
    FSynFPCupLogHL: TSynfpcuplogFold;
    {$ENDIF}
    // Delete .ppu, .a., .o files recursively from RootDirectory without warning
    function DeletePPUs(RootDirectory: string): boolean;
    // Gets fpcup executable full path if possible
    function GetPFCUPLocation: string;
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
  {$IFDEF WIN64}
  FPCUpExe='fpcup64';//fpcup executable filename (without .exe)
  {$ENDIF}
  {$IFDEF WIN32}
  FPCUpExe='fpcup'; //fpcup executable filename (without .exe)
  {$ENDIF}
  {$IFDEF LINUX}
  {$IFDEF CPUARMEL} //2.6.x notation?
  FPCUpExe='fpcup_linux_arm'; //use the 2.7.x fpcup name as that is more likely
  {$ENDIF}
  {$IFDEF CPUARM}
  FPCUpExe='fpcup_linux_arm'; //See below; fpcup_linux_armhf will also be tried by program code
  {$ENDIF}
  {$IFDEF CPU386}
  FPCUpExe='fpcup_linux_x86';
  {$ENDIF}
  {$IFDEF CPUX86_64}
  FPCUpExe='fpcup_linux_x64';
  {$ENDIF} //CPU
  {$ENDIF} //Linux
  {$IFDEF FREEBSD}
  {$IFDEF CPUX86_64}
  FPCUpExe='fpcup_freebsd9_x64';
  {$ENDIF}
  {$IFDEF CPU386}
  FPCUpExe='fpcup_freebsd9_x86';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF OPENBSD}
  {$IFDEF CPUX86_64}
  FPCUpExe='fpcup_openbsd_x64';
  {$ENDIF}
  {$IFDEF CPU386}
  FPCUpExe='fpcup_openbsd_x86';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN} //OSX; at end to let it override
  FPCUpExe='fpcup_osx_x86'; //for now x86 only
  {$ENDIF}

{ TForm1 }
procedure TForm1.btnRunClick(Sender: TObject);
begin
  UpdateCommand(INIFileSelectEdit.FileName, ProfileSelect.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FPCUPLocation: string;
  UpProc: TProcessEx;
begin
  {$IFDEF HLREADY}
  FSynFPCupLogHL:=TSynfpcuplogFold.Create(Self);
  OutputMemo.Highlighter:=FSynFPCupLogHL;
  {$ENDIF}
  FOneTimeSetup:=true;

  SaveDialog.InitialDir:=ExtractFilePath(ParamStr(0)); //application directory
  // Extract settings.ini if necessary
  try
    // Run fpcup --help so it generates relevant ini files.
    // Better than doing fpcuputil.SaveInisFromResource('settings.ini','settings_ini');
    // as we can mix and match fpcup and fpcupgui versions
    if not FileExistsUTF8(ExtractFilePath(ParamStr(0))+'settings.ini') then
    begin
      FPCUPLocation:=GetPFCUPLocation;
      if FileExistsUTF8(FPCUPLocation) then
      begin
        UpProc:=TProcessEx.Create(nil);
        try
          UpProc.Executable:=FPCUPLocation;
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

    // Default location for cleaning up
    {$IFDEF MSWINDOWS}
    RepoDirectory.Directory:='c:\development\';
    {$ENDIF}
    {$IFDEF UNIX}
    RepoDirectory.Directory:='~/development';
    {$ENDIF}

    //Negate design-time tab selection:
    EditTabs.ActivePage:=INIEditorTab;
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
    UpProc.Executable:=GetPFCUPLocation;
    UpProc.OnOutputM:=@DumpOutput;
    UpProc.Parameters.Add('--help');
    UpProc.Options:=UpProc.Options+[poNoConsole];
    try
      Screen.Cursor:=crHourGlass;
      OutputMemo.Lines.Clear;
      EditTabs.ActivePage:=OutputTab; //switch to output tab
      Application.ProcessMessages;
      UpProc.Execute;
      OutputMemo.SelStart:=0; //move to beginning of output
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
    if (INIFileSelectEdit.FileName<>'') and (FileExistsUTF8(INIFileSelectEdit.FileName)) then
    begin
      if UpperCase((INIFileSelectEdit.FileName))='FPCUP.INI' then
        ShowMessage('Warning: fpcup.ini does not contain fpcup user profiles but external module definitions. Try settings.ini.');
      LoadProfilesFromFile(INIFileSelectEdit.FileName);
      ProfileSelect.ItemIndex:=0; //go to first item
    end;
  end;
end;

procedure TForm1.LoadProfilesFromFile(INIFile: string);
var
  Sections: TStringList;
  MyIniFile: TIniFile;
  FileChanged: boolean;
begin
  if FileExistsUTF8(INIFile) then
  begin
    // (re)load selected ini file
    IniMemo.BeginUpdate(false);
    IniMemo.Lines.LoadFromFile(INIFile);
    FileChanged:=not(INIFile=FCurrentIniFile); //todo: proper compare including unicode?
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
    if FileChanged then
      ProfileSelect.ItemIndex:=0;
  end
  else
  begin
    IniMemo.Lines.Clear;
    ShowMessage('File '+INIFile+' does not exist.');
  end;
end;

function TForm1.GetPFCUPLocation: string;
begin
  Result:=ExtractFilePath(ParamStr(0))+FPCUpExe+GetExeExt;
  {$IFDEF CPUARM}
  {$IFDEF LINUX}
  // Allow for hard float variant
  if not(FileExistsUTF8(Result)) then
    Result:=ExtractFilePath(ParamStr(0))+'fpcup_linux_armhf'+GetExeExt;
  {$ENDIF LINUX}
  {$ENDIF CPUARM}
end;

function TForm1.DeletePPUs(RootDirectory: string): boolean;
var
  Extensions: TStringList;
begin
  Extensions:=TStringList.Create;
  try
    Extensions.Add('.a');
    Extensions.Add('.o');
    Extensions.Add('.ppu');
    Screen.Cursor:=crHourGlass;
    try
      try
        Result:=DeleteFilesExtensionsSubdirs(RootDirectory,
          Extensions, '');
      except
        // ignore errors
        Result:=false;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
  finally
    Extensions.Free;
  end;
end;

procedure TForm1.UpdateCommand(Inifile, IniProfile: string);
var
  FPCUpLocation: string;
  ResultCode: integer;
  UpProc: TProcessEx;
begin
  FPCUPLocation:=GetPFCUPLocation;
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
    // Always add --noconfirm so you don't get a prompt we cannot answer
    UpProc.Parameters.Add('--noconfirm');
    //This will lead to lots of pop ups of make.exe which are actually more annoying
    //UpProc.Options:=UpProc.Options+[poNoConsole];
    try
      Screen.Cursor:=crHourGlass;
      OutputMemo.Lines.Clear;
      EditTabs.ActivePage:=OutputTab; //switch to output tab
      Application.ProcessMessages;
      UpProc.Execute;
      { For TMemo/TEdit:
      OutputMemo.SelStart:=0; //move to beginning of output
      }
      // For TSynEdit/TSynMemo which insists on starting at 1
      OutputMemo.SelStart:=1; //move to beginning of output
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
  i:integer;
  OutputList: TStringList;
begin
  // Synedit does not support line breaks...
  OutputList:=TStringList.Create;
  try
    OutputList.Text:=Output;
    for i:=0 to OutputList.Count-1 do
    begin
      OutputMemo.Lines.Append(OutputList[i]);
    end;
  finally
    OutputList.Free;
  end;

  // Give GUI chance to refresh so user doesn't think it hangs:
  Sleep(5);
  Application.ProcessMessages;
end;


procedure TForm1.INIFileSelectEditAcceptFileName(Sender: TObject; var Value: String);
begin
  LoadProfilesFromFile(Value);
end;

procedure TForm1.btnDeletePPUClick(Sender: TObject);
var
  DeleteResult: boolean;
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
    DeleteResult:=DeletePPUs(RepoDirectory.Directory);
    if DeleteResult then
      ShowMessage('Deleted .ppu, .a, .o files. Please run fpcup again to get back all required files (or run svn up).')
    else
      ShowMessage('Error deleting .ppu, .a, .o files. Please run svn up to get back all required files.');
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
            ZipMachine.Entries.AddFileEntry(TempStream,'fpcupoutput.txt');
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

procedure TForm1.btnSwitchClick(Sender: TObject);
var
  Reply: integer;
  ResultCode:integer;
  SVN: TSVNClient;
begin
  if not(DirectoryExistsUTF8(RepoDirectorySwitch.Directory)) then
  begin
    ShowMessage('No directory selected. Please select the directory where fpcup downloads the Lazarus or FPC sources from subversion.');
    exit;
  end;
  Reply:=Application.MessageBox(PChar('Are you really sure you want to delete .ppu etc files and switch '+LineEnding+
    RepoDirectorySwitch.Directory+
    'to SVN repository '+LineEnding+
    SVNRepoSwitchTo.Text +'?'),
    'SVN Switch: are you sure you know what you are doing?',MB_ICONQUESTION+MB_YESNO);
  if reply=IDYES then
  begin
    DeletePPUs(RepoDirectorySwitch.Directory);
    SVN:=TSVNClient.Create;
    try
      ResultCode:=SVN.Execute('switch '+SVNRepoSwitchTo.Text);
    finally
      SVN.Free;
    end;
  end;
  if ResultCode=0 then
    ShowMessage('Switch succeeded. Please run fpcup with the new SVN repository URL.')
  else
    ShowMessage('Switch failed. SVN switch gave result code:'+inttostr(ResultCode));
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Load any stored file specified with propsettings
  if FOneTimeSetup and
    (INIFileSelectEdit.FileName<>'') and
    FileExistsUTF8(INIFileSelectEdit.FileName) then
  begin
    FOneTimeSetup:=false;
    LoadProfilesFromFile(INIFileSelectEdit.FileName);
  end;
end;

procedure TForm1.INIFileSelectEditExit(Sender: TObject);
begin
  LoadProfilesFromFile(INIFileSelectEdit.FileName);
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

