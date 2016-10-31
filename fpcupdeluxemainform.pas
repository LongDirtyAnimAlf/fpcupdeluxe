unit fpcupdeluxemainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel, Buttons,
  installerManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    TrunkBtn: TBitBtn;
    NPBtn: TBitBtn;
    FixesBtn: TBitBtn;
    StableBtn: TBitBtn;
    OldBtn: TBitBtn;
    DinoBtn: TBitBtn;
    FeaturesBtn: TBitBtn;
    mORMotBtn: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckVerbosity: TCheckBox;
    Edit1: TEdit;
    Panel1: TPanel;
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
    procedure QuickBtnClick(Sender: TObject);
  private
    { private declarations }
    oldoutput: TextFile;
    sInstallDir:string;
    FPCupManager:TFPCupManager;
    procedure DisEnable(Sender: TObject;value:boolean);
    procedure PrepareRun;
    function RealRun:boolean;
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
  {$ifdef MSWINDOWS}
  processutils,
  {$endif}
  synedittext;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';
  FPCUPGITREPO='https://github.com/LongDirtyAnimAlf/Reiniero-fpcup';

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

  SetCurrentDir(SafeGetApplicationPath);
  {$IFDEF MSWINDOWS}
  //SetCurrentDir(SafeGetApplicationPath);
  {$ELSE}
  //SetCurrentDir('');
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
  FInstaller.FPCOpt:='-Fl/usr/local/lib';
  FInstaller.LazarusOpt:='-Fl/usr/local/lib -Fl/usr/X11R6/lib';
  {$endif}

  FPCupManager.FPCURL:='default';
  FPCupManager.LazarusURL:='default';
  FPCupManager.Verbose:=true;

  {$IF defined(BSD) and not defined(DARWIN)}
  FPCupManager.PatchCmd:='gpatch';
  {$ELSE}
  FPCupManager.PatchCmd:='patch';
  {$ENDIF MSWINDOWS}

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

procedure TForm1.QuickBtnClick(Sender: TObject);
var
  s,FPCURL,LazarusURL:string;
  i:integer;
begin
  DisEnable(Sender,False);
  Application.ProcessMessages;
  try
    PrepareRun;

    if Sender=TrunkBtn then
    begin
      s:='Going to install both FPC trunk and Lazarus trunk';
      FPCURL:='trunk';
      LazarusURL:='trunk';
    end;

    if Sender=NPBtn then
    begin
      s:='Going to install NewPascal';
      FPCURL:='newpascal';
      LazarusURL:='newpascal';
      FPCupManager.IncludeModules:='mORMotFPC,zeos';
    end;

    if Sender=FixesBtn then
    begin
      s:='Going to install FPC fixes and Lazarus fixes';
      FPCURL:='fixes';
      LazarusURL:='fixes';
    end;

    if Sender=StableBtn then
    begin
      s:='Going to install FPC stable and Lazarus stable';
      FPCURL:='stable';
      LazarusURL:='stable';
    end;

    if Sender=OldBtn then
    begin
      s:='Going to install FPC 2.6.4 and Lazarus 1.4 ';
      FPCURL:='2.6.4';
      LazarusURL:='1.4';
    end;

    if Sender=DinoBtn then
    begin
      s:='Going to install FPC 2.0.2 and Lazarus 0.9.4 ';
      FPCURL:='2.0.2';
      LazarusURL:='0.9.4';
      FPCupManager.SkipModules:='lazbuild';
    end;

    if Sender=FeaturesBtn then
    begin
      s:='Going to install FPC trunk and Lazarus trunk with extras ';
      FPCURL:='trunk';
      LazarusURL:='trunk';
      FPCupManager.IncludeModules:='mORMotFPC,lazgoogleapis,virtualtreeview,lazpaint,bgracontrols,uecontrols,ECControls,zeos,cudatext,indy,lnet,lamw,mupdf,tiopf,abbrevia,uos,wst,anchordocking,simplegraph,cm630commons,turbobird';
    end;

    i:=ListBox1.Items.IndexOf(FPCURL);
    if i<>-1 then ListBox1.Selected[i]:=true;
    i:=ListBox2.Items.IndexOf(LazarusURL);
    if i<>-1 then ListBox2.Selected[i]:=true;

    FPCupManager.FPCURL:=FPCURL;
    FPCupManager.LazarusURL:=LazarusURL;

    writeln(s+'.');

    RealRun;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DisEnable(Sender,False);
  Application.ProcessMessages;
  try
    PrepareRun;
    writeln('Going to install FPC and Lazarus with given options.');
    RealRun;
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

    RealRun;

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
var
  aDownLoader: TDownLoader;
  URL,TargetFile,UnZipper:string;
  success:boolean;
begin
  DisEnable(Sender,False);
  Application.ProcessMessages;
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

    // use the available source to build the cross-compiler ... change nothing !!
    FPCupManager.OnlyModules:='FPCCleanOnly,FPCBuildOnly';
    FPCupManager.FPCURL:='skip';
    FPCupManager.LazarusURL:='skip';

    writeln('Going to install a cross-compiler from current sources.');

    if NOT RealRun then
    begin

      {$ifdef MSWINDOWS}
      // perhaps there were not libraries and/or binutils ... download them (if available) from fpcup on GitHub
      URL:='';

      if FPCupManager.CrossOS_Target='linux' then
      begin
        if FPCupManager.CrossCPU_Target='arm' then URL:='LinuxARM.rar';
        if FPCupManager.CrossCPU_Target='aarch64' then URL:='LinuxAarch64.rar';
        if FPCupManager.CrossCPU_Target='i386' then URL:='Linuxi386.rar';
        if FPCupManager.CrossCPU_Target='x86_64' then URL:='Linuxx64.rar';
      end;
      if FPCupManager.CrossOS_Target='wince' then
      begin
        if FPCupManager.CrossCPU_Target='arm' then URL:='WinceARM.rar';
      end;
      if FPCupManager.CrossOS_Target='android' then
      begin
        if FPCupManager.CrossCPU_Target='arm' then URL:='AndroidARM.rar';
      end;

      if URL<>'' then
      begin

        writeln('Going to download the right cross-tools.');

        URL:=FPCUPGITREPO+'/raw/master/crosstools/WinCross'+URL;

        TargetFile := SysUtils.GetTempFileName;

        aDownLoader:=TDownLoader.Create;
        try
          success:=aDownLoader.getFile(URL,TargetFile);
          if (NOT success) then // try only once again in case of error
          begin
            writeln('Error while trying to download '+URL+'. Trying once again.');
            SysUtils.DeleteFile(TargetFile); // delete stale targetfile
            success:=aDownLoader.getFile(URL,TargetFile);
          end;
        finally
          aDownLoader.Destroy;
        end;

        if success then
        begin
          success:=(ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+TargetFile+' "'+IncludeTrailingPathDelimiter(sInstallDir)+'"',true)=0);

          if (NOT success) then
          begin
            UnZipper := IncludeTrailingPathDelimiter(FPCupManager.MakeDirectory) + '7z1604'{$ifdef win64} + '-x64'{$endif} + '.exe';
            success:=(ExecuteCommand(UnZipper + ' x -o"' + IncludeTrailingPathDelimiter(sInstallDir)+'" '+TargetFile,true)=0);
          end;

          SysUtils.DeleteFile(TargetFile);
          if success then
          begin
            // run again with the correct libs and binutils
            label1.Font.Color:=clDefault;
            label2.Font.Color:=clDefault;
            FPCupManager.Sequencer.ResetAllExecuted;
            RealRun;
          end;
        end;
        if (NOT success) then writeln('No luck in getting then cross-tools ... aborting.');
      end
      else
      begin
        writeln('No suitable cross-tools found ... aborting.');
      end;

      {$endif}
    end;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  store:integer;
begin
  if listbox1.ItemIndex=-1 then exit;
  DisEnable(Sender,False);
  Application.ProcessMessages;
  try
    PrepareRun;

    FPCupManager.OnlyModules:='fpc';
    FPCupManager.LazarusURL:='skip';

    RealRun;
  finally
    DisEnable(Sender,True);
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

  TrunkBtn.Enabled:=value;
  NPBtn.Enabled:=value;
  FixesBtn.Enabled:=value;
  StableBtn.Enabled:=value;
  OldBtn.Enabled:=value;
  DinoBtn.Enabled:=value;
  FeaturesBtn.Enabled:=value;
  mORMotBtn.Enabled:=value;
end;

procedure TForm1.PrepareRun;
begin
  label1.Font.Color:=clDefault;
  label2.Font.Color:=clDefault;

  FPCupManager.Sequencer.ResetAllExecuted;

  FPCupManager.OnlyModules:='';
  FPCupManager.IncludeModules:='';
  FPCupManager.SkipModules:='';
  FPCupManager.CrossCPU_Target:='';
  FPCupManager.CrossOS_Target:='';
  FPCupManager.CrossOS_SubArch:='';

  FPCupManager.FPCOPT:='';
  FPCupManager.CrossOPT:='';

  FPCupManager.Verbose:=CheckVerbosity.Checked;

  // set default values for FPC and Lazarus URL ... can still be changed inside the real run button onclicks
  FPCupManager.FPCURL:='default';
  FPCupManager.LazarusURL:='default';
  if (listbox1.ItemIndex<>-1) then FPCupManager.FPCURL:=listbox1.Items[listbox1.ItemIndex];
  if (listbox2.ItemIndex<>-1) then FPCupManager.LazarusURL:=listbox2.Items[listbox2.ItemIndex];

  sInstallDir:=ExcludeTrailingPathDelimiter(sInstallDir);

  FPCupManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update';
  FPCupManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir);

  sInstallDir:=sInstallDir+DirectorySeparator;

  {$IFDEF MSWINDOWS}
  FPCupManager.MakeDirectory:=sInstallDir+'fpcbootstrap';
  {$ELSE}
  FPCupManager.MakeDirectory:='';
  {$ENDIF MSWINDOWS}
  FPCupManager.BootstrapCompilerDirectory:=sInstallDir+'fpcbootstrap';
  FPCupManager.FPCDirectory:=sInstallDir+'fpc';
  FPCupManager.LazarusDirectory:=sInstallDir+'lazarus';

  FPCupManager.LazarusPrimaryConfigPath:=sInstallDir+'config_'+ExtractFileName(FPCupManager.LazarusDirectory);

  RealFPCURL.Text:='';
  RealLazURL.Text:='';

end;

function TForm1.RealRun:boolean;
begin
  result:=false;

  writeln('FPCUP de luxe is starting up.');
  writeln;
  {$IFDEF MSWINDOWS}
  writeln('Binutils/make dir:  '+FPCupManager.MakeDirectory);
  {$ENDIF MSWINDOWS}
  writeln('Bootstrap dir:      '+FPCupManager.BootstrapCompilerDirectory);

  if FPCupManager.FPCURL<>'SKIP' then
  begin
    writeln('FPC URL:            '+FPCupManager.FPCURL);
    writeln('FPC options:        '+FPCupManager.FPCOPT);
    writeln('FPC directory:      '+FPCupManager.FPCDirectory);
    RealFPCURL.Text:=FPCupManager.FPCURL;
  end else RealFPCURL.Text:='Skipping FPC';

  if FPCupManager.LazarusURL<>'SKIP' then
  begin
    writeln('Lazarus URL:        '+FPCupManager.LazarusURL);
    writeln('Lazarus options:    '+FPCupManager.LazarusOPT);
    writeln('Lazarus directory:  '+FPCupManager.LazarusDirectory);
    RealLazURL.Text:=FPCupManager.LazarusURL;
  end else RealLazURL.Text:='Skipping Lazarus';

  writeln('Please stand back and enjoy !');
  writeln;

  Application.ProcessMessages;
  sleep(2000);

  try
    result:=FPCupManager.Run;
    if (NOT result) then
    begin
      writeln;
      writeln;
      writeln('ERROR: Fpclazupdeluxe failed.');
      label1.Font.Color:=clRed;
      label2.Font.Color:=clRed;
    end
    else
    begin
      writeln;
      writeln;
      writeln('SUCCESS: Fpclazupdeluxe ended without errors.');
      label1.Font.Color:=clLime;
      label2.Font.Color:=clLime;
    end;
  except
    // just swallow exceptions
  end;
end;


end.

