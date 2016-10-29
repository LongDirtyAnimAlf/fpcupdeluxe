unit fpcupdeluxemainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  installerManager;
type
  {
  TMemo = class(StdCtrls.TMemo)
    private
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
      procedure WMEraseBkGnd(var Message: TWMEraseBkGnd);
    end;
  }

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckVerbosity: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    oldoutput: TextFile;
    sInstallDir:string;
    FPCupManager:TFPCupManager;
    procedure FpcupExecute;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

//{$I fpcuplprbase.inc}

uses
  IniFiles,
  installerUniversal,
  fpcuputil,
  synedittext;

Const
  CONFIGFILENAME='fpcup.ini';
  SETTTINGSFILENAME='settings.ini';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  SortedModules: TStringList;
  i,j:integer;
begin

  oldoutput := System.Output;
  AssignSynEdit(System.Output, SynEdit1);
  Reset(System.Input);
  Rewrite(System.Output);

  FPCupManager:=TFPCupManager.Create;

  {$IFDEF MSWINDOWS}
  sInstallDir:='C:\fpcupdeluxe';
  {$ELSE}
  sInstallDir:='~/fpcupdeluxe';
  {$ENDIF}

  with TIniFile.Create('deluxe.ini') do
  try
    sInstallDir:=ReadString('General','InstallDirectory',sInstallDir);
  finally
    Free;
  end;

  sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName(sInstallDir));

  Edit1.Text:=sInstallDir;

  FPCupManager.ConfigFile:=installerUniversal.CONFIGFILENAME;

  FPCupManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update';
  FPCupManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir);

  if not FileExists(SafeGetApplicationPath+SETTTINGSFILENAME) then
      SaveInisFromResource(SafeGetApplicationPath+SETTTINGSFILENAME,'settings_ini');
  if not FileExists(SafeGetApplicationPath+CONFIGFILENAME) then
    SaveInisFromResource(SafeGetApplicationPath+CONFIGFILENAME,'fpcup_ini');

  FPCupManager.LoadFPCUPConfig;

  sInstallDir:=IncludeTrailingPathDelimiter(sInstallDir);

  FPCupManager.MakeDirectory:=sInstallDir+'fpcbootstrap';
  FPCupManager.BootstrapCompilerDirectory:=sInstallDir+'fpcbootstrap';
  FPCupManager.FPCDirectory:=sInstallDir+'fpc';
  FPCupManager.LazarusDirectory:=sInstallDir+'lazarus';

  FPCupManager.LazarusPrimaryConfigPath:=sInstallDir+'config_'+ExtractFileName(FPCupManager.LazarusDirectory);

  {$IF (defined(BSD)) and (not defined(Darwin))}
  FInstaller.LazarusOpt:='-Fl/usr/local/lib -Fl/usr/X11R6/lib';
  {$endif}

  FPCupManager.FPCURL:='default';
  FPCupManager.LazarusURL:='default';
  FPCupManager.Verbose:=true;
  //FPCupManager.SkipModules:='fpc';

  SortedModules:=TStringList.Create;
  try
    for i:=0 to FPCupManager.ModulePublishedList.Count-1 do
    begin
      if RightStr(FPCupManager.ModulePublishedList[i],Length('clean'))='clean' then continue;
      if RightStr(FPCupManager.ModulePublishedList[i],Length('uninstall'))='uninstall' then continue;
      SortedModules.Add(FPCupManager.ModulePublishedList[i]);
    end;
    listbox3.Items.AddStrings(SortedModules);

    for i:=0 to FPCupManager.ModuleEnabledList.Count-1 do
    begin
      j:=listbox3.Items.IndexOf(FPCupManager.ModuleEnabledList[i]);
      if j<>-1 then listbox3.Selected[j]:=true;
    end;

  finally
    SortedModules.Free;
  end;

  listbox1.Items.CommaText:=installerUniversal.GetAlias('fpcURL','list');
  listbox2.Items.CommaText:=installerUniversal.GetAlias('lazURL','list');

  with TIniFile.Create('deluxe.ini') do
  try
    listbox1.ItemIndex:=ReadInteger('URL','fpcURL',0);
    listbox2.ItemIndex:=ReadInteger('URL','lazURL',0);
    CheckVerbosity.Checked:=ReadBool('General','Verbose',True);
    if ReadBool('General','Maximized',False) then
    begin
      Self.WindowState:=wsMaximized;
    end
    else
    begin
      Self.WindowState:=wsNormal;
      Self.Top := ReadInteger('General','Top',Self.Top);
      Self.Left := ReadInteger('General','Left',Self.Left);
      Self.Width := ReadInteger('General','Width',Self.Width);
      Self.Height := ReadInteger('General','Height',Self.Height);
    end;
  finally
    Free;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    FpcupExecute;
  except
    on E: Exception do
    begin
      Sleep(0);
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir:=sInstallDir;
  if SelectDirectoryDialog1.Execute then
  begin
    sInstallDir:=SelectDirectoryDialog1.FileName;
    Edit1.Text:=sInstallDir;
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  sInstallDir:=Edit1.Text;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TIniFile.Create('deluxe.ini') do
  try
    WriteInteger('URL','fpcURL',listbox1.ItemIndex);
    WriteInteger('URL','lazURL',listbox2.ItemIndex);
    WriteBool('General','Verbose',CheckVerbosity.Checked);
    WriteString('General','InstallDirectory',sInstallDir);
    if Self.WindowState=wsNormal then
    begin
      WriteInteger('General','Top',Self.Top);
      WriteInteger('General','Left',Self.Left);
      WriteInteger('General','Width',Self.Width);
      WriteInteger('General','Height',Self.Height);
      WriteBool('General','Maximized',False);
    end
    else
    begin
      WriteBool('General','Maximized',True);
    end;
  finally
    Free;
  end;
  FreeAndNil(FPCupManager);
  (* using CloseFile will ensure that all pending output is flushed *)
  CloseFile(System.Output);
  System.Output := oldoutput;
  CloseAction:=caFree;
end;

procedure TForm1.FpcupExecute;
begin
  try
    FPCupManager.Verbose:=CheckVerbosity.Checked;
    if listbox1.SelCount=1 then  FPCupManager.FPCURL:=listbox1.Items[listbox1.ItemIndex];
    if listbox2.SelCount=1 then  FPCupManager.LazarusURL:=listbox2.Items[listbox2.ItemIndex];
    writeln('FPCUP de luxe.');
    writeln;
    {$IFDEF MSWINDOWS}
    writeln('Binutils/make dir:  '+FPCupManager.MakeDirectory);
    {$ENDIF MSWINDOWS}
    writeln('Bootstrap dir:      '+FPCupManager.BootstrapCompilerDirectory);
    writeln('FPC URL:            '+FPCupManager.FPCURL);
    writeln('FPC options:        '+FPCupManager.FPCOPT);
    writeln('FPC directory:      '+FPCupManager.FPCDirectory);
    {$ifndef FPCONLY}
    writeln('Lazarus URL:        '+FPCupManager.LazarusURL);
    writeln('Lazarus options:    '+FPCupManager.LazarusOPT);
    writeln('Lazarus directory:  '+FPCupManager.LazarusDirectory);
    {$endif}
    writeln;
    writeln('Going to install FPC and Lazarus with given options.');
    writeln('Please stand back and enjoy !');
    writeln;
    Application.ProcessMessages;
    sleep(2000);
    if (FPCupManager.Run=false)
       then writeln('ERROR: Fpclazupdeluxe failed.')
       else writeln('SUCCESS: Fpclazupdeluxe ended without errors.');
  except
    FPCupManager.free;
  end;
  writeln;
  writeln('Please come back when needed !!');
end;

end.

