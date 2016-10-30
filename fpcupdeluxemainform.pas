unit fpcupdeluxemainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  installerManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckVerbosity: TCheckBox;
    Edit1: TEdit;
    RealFPCURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RealLazURL: TEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
  private
    { private declarations }
    oldoutput: TextFile;
    sInstallDir:string;
    FPCupManager:TFPCupManager;
    procedure DisEnable(Sender: TObject;value:boolean);
    procedure PrepareRun;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  IniFiles,
  StrUtils,
  installerUniversal,
  fpcuputil,
  synedittext;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  SortedModules: TStringList;
  i,j:integer;
begin
  Self.Caption:='FPCUpdeluxe based on '+RevisionStr+' version '+versiondate;

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

  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    sInstallDir:=ReadString('General','InstallDirectory',sInstallDir);
  finally
    Free;
  end;

  sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName(sInstallDir));

  Edit1.Text:=sInstallDir;

  FPCupManager.ConfigFile:=SafeGetApplicationPath+installerUniversal.CONFIGFILENAME;

  if not FileExists(SafeGetApplicationPath+installerUniversal.SETTTINGSFILENAME) then
    SaveInisFromResource(SafeGetApplicationPath+installerUniversal.SETTTINGSFILENAME,'settings_ini');
  if not FileExists(SafeGetApplicationPath+installerUniversal.CONFIGFILENAME) then
    SaveInisFromResource(SafeGetApplicationPath+installerUniversal.CONFIGFILENAME,'fpcup_ini');

  FPCupManager.LoadFPCUPConfig;

  {$IF (defined(BSD)) and (not defined(Darwin))}
  FInstaller.LazarusOpt:='-Fl/usr/local/lib -Fl/usr/X11R6/lib';
  {$endif}

  FPCupManager.FPCURL:='default';
  FPCupManager.LazarusURL:='default';
  FPCupManager.Verbose:=true;

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
      //if j<>-1 then listbox3.Selected[j]:=true;
    end;

  finally
    SortedModules.Free;
  end;

  listbox1.Items.CommaText:=installerUniversal.GetAlias('fpcURL','list');
  listbox2.Items.CommaText:=installerUniversal.GetAlias('lazURL','list');

  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    listbox1.ItemIndex:=ReadInteger('URL','fpcURL',-1);
    listbox2.ItemIndex:=ReadInteger('URL','lazURL',-1);
    CheckVerbosity.Checked:=ReadBool('General','Verbose',True);
    SortedModules:=TStringList.Create;
    try
      SortedModules.CommaText:=ReadString('General','Modules','');
      for i:=0 to SortedModules.Count-1 do
      begin
        j:=listbox3.Items.IndexOf(SortedModules[i]);
        if j<>-1 then listbox3.Selected[j]:=true;
      end;
    finally
      SortedModules.Free;
    end;
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

procedure TForm1.FormResize(Sender: TObject);
var
  w:integer;
begin
  w:=(SynEdit1.Width DIV 2);
  RealFPCURL.Width:=(w-4);
  RealLazURL.Width:=RealFPCURL.Width;
  RealLazURL.Left:=RealFPCURL.Left+(w+4);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if (RadioGroup1.ItemIndex<>-1) AND (RadioGroup1.Items[RadioGroup1.ItemIndex]='jvm') then
  begin
    RadioGroup2.ItemIndex:=-1;
    RadioGroup2.Enabled:=false;
  end
  else RadioGroup2.Enabled:=true;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  if (RadioGroup2.ItemIndex<>-1) AND (RadioGroup2.Items[RadioGroup2.ItemIndex]='java') then
  begin
    RadioGroup1.ItemIndex:=-1;
    RadioGroup1.Enabled:=false;
  end
  else RadioGroup1.Enabled:=true;
end;

procedure TForm1.SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
function ExistWordInString(aString:pchar;aSearchString:string;aSearchOptions: TStringSearchOptions): Boolean;
var
  Size : Integer;
begin
  Size:=StrLen(aString);
  Result := SearchBuf(aString, Size, 0, 0, aSearchString, aSearchOptions)<>nil;
end;
var
  s:string;
begin
  s:=SynEdit1.Lines[Line-1];

  if ExistWordInString(PChar(s),'svn: e',[soDown]) then
  begin
    FG      := clFuchsia; //Text Color
    BG      := clBlack;  //BackGround
    Special := True;     //Must be true
  end;

  if ExistWordInString(PChar(s),'info:',[soWholeWord,soDown]) then
  begin
    FG      := clYellow;
    BG      := clBlack;
    Special := True;
  end;

  if (ExistWordInString(PChar(s),'warning:',[soWholeWord,soDown])) OR (ExistWordInString(PChar(s),'hint:',[soWholeWord,soDown])) then
  begin
    FG      := clGreen;
    BG      := clBlack;
    Special := True;
  end;

  if ExistWordInString(PChar(s),'error:',[soWholeWord,soDown]) then
  begin
    FG      := clRed;
    BG      := clBlue;
    Special := True;
  end;

  if ExistWordInString(PChar(s),'Executing :',[soWholeWord,soDown]) then
  begin
    FG      := clAqua;
    BG      := clBlack;
    Special := True;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DisEnable(Sender,False);
  Application.ProcessMessages;
  try
    try
      PrepareRun;
      writeln('Going to install FPC and Lazarus with given options.');
      writeln('Please stand back and enjoy !');
      writeln;
      Application.ProcessMessages;
      sleep(2000);
      if (FPCupManager.Run=false)
         then
         begin
           writeln('ERROR: Fpclazupdeluxe failed.');
           label1.Font.Color:=clRed;
           label2.Font.Color:=clRed;
         end
         else
         begin
           writeln('SUCCESS: Fpclazupdeluxe ended without errors.');
           label1.Font.Color:=clLime;
           label2.Font.Color:=clLime;
         end;
    except
      FPCupManager.free;
    end;
    writeln;
    writeln('Please come back when needed !!');
  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i:integer;
  modules:string;
begin
  DisEnable(Sender,False);
  Application.ProcessMessages;
  try
    try
      PrepareRun;
      modules:='';
      for i:=0 to ListBox3.Count-1 do
      begin
        if ListBox3.Selected[i] then modules:=modules+ListBox3.Items[i]+',';
      end;
      // delete stale trailing comma, if any
      if Length(modules)>0 then Delete(modules,Length(modules),1);
      FPCupManager.OnlyModules:=modules;
      writeln('Limiting installation/update to '+FPCupManager.OnlyModules);
      writeln;
      writeln('Going to install selected modules with given options.');
      writeln('Please stand back and enjoy !');
      writeln;
      Application.ProcessMessages;
      sleep(2000);
      if (FPCupManager.Run=false)
         then
         begin
           writeln('ERROR: Fpclazupdeluxe failed.');
           label1.Font.Color:=clRed;
           label2.Font.Color:=clRed;
         end
         else
         begin
           writeln('SUCCESS: Installing modules ended without errors.');
           label1.Font.Color:=clLime;
           label2.Font.Color:=clLime;
         end;
    except
      FPCupManager.free;
    end;
    writeln;
    writeln('Please come back when needed !!');
  finally
    DisEnable(Sender,True);
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

procedure TForm1.Button4Click(Sender: TObject);
begin
  ListBox3.ClearSelection;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  DisEnable(Sender,False);
  Application.ProcessMessages;
  try
    try
      PrepareRun;

      if RadioGroup1.ItemIndex<>-1 then FPCupManager.CrossCPU_Target:=RadioGroup1.Items[RadioGroup1.ItemIndex];
      if RadioGroup2.ItemIndex<>-1 then FPCupManager.CrossOS_Target:=RadioGroup2.Items[RadioGroup2.ItemIndex];

      if (FPCupManager.CrossCPU_Target='jvm') then FPCupManager.CrossOS_Target:='java';
      if (FPCupManager.CrossOS_Target='java') then FPCupManager.CrossCPU_Target:='jvm';

      if (FPCupManager.CrossCPU_Target='') OR (FPCupManager.CrossOS_Target='') then
      begin
        writeln;
        writeln('Input error.');
        writeln('Please select CPU AND OS !!');
        writeln;
        exit;
      end;

      if FPCupManager.CrossCPU_Target='arm' then
      begin
        // default: armhf
        FPCupManager.FPCOPT:='-dFPC_ARMHF';
        FPCupManager.CrossOPT:='-CpARMV7A -CfVFPV3 -OoFASTMATH -CaEABIHF';
        //'-CfSoft -CpARMV6'
      end;

      FPCupManager.OnlyModules:='FPCCleanOnly,FPCBuildOnly';

      writeln('Going to install a  cross-compiler.');
      writeln('Please stand back and enjoy !');
      writeln;
      Application.ProcessMessages;
      sleep(2000);
      if (FPCupManager.Run=false)
         then
         begin
           writeln('ERROR: Installing cross-compiler failed.');
           label1.Font.Color:=clRed;
           label2.Font.Color:=clRed;
         end
         else
         begin
           writeln('SUCCESS: Installing cross-compiler ended without errors.');
           label1.Font.Color:=clLime;
           label2.Font.Color:=clLime;
         end;
    except
      FPCupManager.free;
    end;
    writeln;
    writeln('Please come back when needed !!');
  finally
    DisEnable(Sender,True);
  end;

end;

procedure TForm1.Button6Click(Sender: TObject);
var
  store:integer;
begin
  DisEnable(Sender,False);
  store:=listbox2.ItemIndex;
  listbox2.ClearSelection;
  Application.ProcessMessages;
  try
    try
      PrepareRun;

      FPCupManager.OnlyModules:='fpc';

      writeln('Going to install FPC with given options.');
      writeln('Please stand back and enjoy !');
      writeln;
      Application.ProcessMessages;
      sleep(2000);
      if (FPCupManager.Run=false)
         then
         begin
           writeln('ERROR: Fpclazupdeluxe failed.');
           label1.Font.Color:=clRed;
           label2.Font.Color:=clRed;
         end
         else
         begin
           writeln('SUCCESS: Fpclazupdeluxe ended without errors.');
           label1.Font.Color:=clLime;
           label2.Font.Color:=clLime;
         end;
    except
      FPCupManager.free;
    end;
    writeln;
    writeln('Please come back when needed !!');
  finally
    DisEnable(Sender,True);
    listbox2.ItemIndex:=store;
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  sInstallDir:=Edit1.Text;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i:integer;
  modules:string;
begin
  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    WriteInteger('URL','fpcURL',listbox1.ItemIndex);
    WriteInteger('URL','lazURL',listbox2.ItemIndex);
    WriteBool('General','Verbose',CheckVerbosity.Checked);
    WriteString('General','InstallDirectory',sInstallDir);

    modules:='';
    for i:=0 to ListBox3.Count-1 do
    begin
      if ListBox3.Selected[i] then modules:=modules+ListBox3.Items[i]+',';
    end;
    // delete stale trailing comma, if any
    if Length(modules)>0 then
    Delete(modules,Length(modules),1);
    WriteString('General','Modules',modules);
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

procedure TForm1.DisEnable(Sender: TObject;value:boolean);
begin
  //if Sender<>Button1 then
  Button1.Enabled:=value;
  //if Sender<>Button2 then
  Button2.Enabled:=value;
  ListBox1.Enabled:=value;
  ListBox2.Enabled:=value;
  ListBox3.Enabled:=value;
  Edit1.Enabled:=value;
  Button3.Enabled:=value;
  Button4.Enabled:=value;
  Button5.Enabled:=value;
  Button6.Enabled:=value;
  RadioGroup1.Enabled:=value;
  RadioGroup2.Enabled:=value;
  CheckVerbosity.Enabled:=value;
end;

procedure TForm1.PrepareRun;
begin
  label1.Font.Color:=clDefault;
  label2.Font.Color:=clDefault;

  FPCupManager.OnlyModules:='';
  FPCupManager.CrossCPU_Target:='';
  FPCupManager.CrossOS_Target:='';
  FPCupManager.CrossOS_SubArch:='';

  FPCupManager.FPCOPT:='';
  FPCupManager.CrossOPT:='';

  FPCupManager.Verbose:=CheckVerbosity.Checked;

  FPCupManager.FPCURL:='default';
  FPCupManager.LazarusURL:='default';

  if (listbox1.ItemIndex<>-1) then FPCupManager.FPCURL:=listbox1.Items[listbox1.ItemIndex];
  if (listbox2.ItemIndex<>-1) then FPCupManager.LazarusURL:=listbox2.Items[listbox2.ItemIndex];

  sInstallDir:=ExcludeTrailingPathDelimiter(sInstallDir);

  FPCupManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update';
  FPCupManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir);

  sInstallDir:=sInstallDir+DirectorySeparator;

  FPCupManager.MakeDirectory:=sInstallDir+'fpcbootstrap';
  FPCupManager.BootstrapCompilerDirectory:=sInstallDir+'fpcbootstrap';
  FPCupManager.FPCDirectory:=sInstallDir+'fpc';
  FPCupManager.LazarusDirectory:=sInstallDir+'lazarus';

  FPCupManager.LazarusPrimaryConfigPath:=sInstallDir+'config_'+ExtractFileName(FPCupManager.LazarusDirectory);

  writeln('FPCUP de luxe.');
  writeln;
  {$IFDEF MSWINDOWS}
  writeln('Binutils/make dir:  '+FPCupManager.MakeDirectory);
  {$ENDIF MSWINDOWS}
  writeln('Bootstrap dir:      '+FPCupManager.BootstrapCompilerDirectory);
  writeln('FPC URL:            '+FPCupManager.FPCURL);
  writeln('FPC options:        '+FPCupManager.FPCOPT);
  writeln('FPC directory:      '+FPCupManager.FPCDirectory);
  RealFPCURL.Text:=FPCupManager.FPCURL;

  if (listbox2.ItemIndex<>-1) then
  begin
    writeln('Lazarus URL:        '+FPCupManager.LazarusURL);
    writeln('Lazarus options:    '+FPCupManager.LazarusOPT);
    writeln('Lazarus directory:  '+FPCupManager.LazarusDirectory);
    RealLazURL.Text:=FPCupManager.LazarusURL;
  end else RealLazURL.Text:='';

  writeln;
end;

end.

