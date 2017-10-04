unit fpcupdeluxemainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Types, Buttons, Menus,
  SynEdit, SynEditPopup, SynEditMiscClasses, SynEditMarkupSpecialLine,
  installerManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnHalt: TBitBtn;
    BitBtnFPCandLazarus: TBitBtn;
    BitBtnFPCOnly: TBitBtn;
    BitBtnLazarusOnly: TBitBtn;
    AutoCrossUpdate: TButton;
    Button7: TButton;
    Button8: TButton;
    CheckAutoClear: TCheckBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    StatusMessage: TEdit;
    TrunkBtn: TBitBtn;
    NPBtn: TBitBtn;
    FixesBtn: TBitBtn;
    StableBtn: TBitBtn;
    OldBtn: TBitBtn;
    DinoBtn: TBitBtn;
    FeaturesBtn: TBitBtn;
    mORMotBtn: TBitBtn;
    Button2: TButton;
    Button3: TButton;
    ButtonInstallCrossCompiler: TButton;
    InstallDirEdit: TEdit;
    Panel1: TPanel;
    RealFPCURL: TEdit;
    FPCVersionLabel: TLabel;
    LazarusVersionLabel: TLabel;
    ListBoxFPCTarget: TListBox;
    ListBoxLazarusTarget: TListBox;
    ListBox3: TListBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RealLazURL: TEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SynEdit1: TSynEdit;
    procedure BitBtnHaltClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LazarusOnlyClick(Sender: TObject);
    procedure BitBtnFPCandLazarusClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonInstallCrossCompilerClick(Sender: TObject);
    procedure FPCOnlyClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ButtonAutoUpdateCrossCompiler(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure TargetSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure SynEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure QuickBtnClick(Sender: TObject);
  private
    { private declarations }
    FPCupManager:TFPCupManager;
    oldoutput: TextFile;
    sInstallDir:string;
    sStatus:string;
    FFPCTarget,FLazarusTarget:string;
    MissingCrossBins:boolean;
    MissingCrossLibs:boolean;
    InternalError:string;
    function InstallCrossCompiler(Sender: TObject):boolean;
    function AutoUpdateCrossCompiler(Sender: TObject):boolean;
    procedure SetFPCTarget(aFPCTarget:string);
    procedure SetLazarusTarget(aLazarusTarget:string);
    procedure DisEnable(Sender: TObject;value:boolean);
    procedure Edit1Change(Sender: TObject);
    procedure PrepareRun;
    function RealRun:boolean;
    function GetFPCUPSettings(IniDirectory:string):boolean;
    function SetFPCUPSettings(IniDirectory:string):boolean;
    procedure AddMessage(aMessage:string; UpdateStatus:boolean=false);
    procedure InitFPCupManager;
    property FPCTarget:string read FFPCTarget write SetFPCTarget;
    property LazarusTarget:string read FLazarusTarget write SetLazarusTarget;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  IniFiles,
  strutils,
  LCLType, // for MessageBox
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF UNIX}
  //LazFileUtils,
  AboutFrm,
  extrasettings,
  //checkoptions,
  installerCore,
  installerUniversal,
  fpcuputil,
  processutils,
  synedittext;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';
  DELUXEVERSION='1.4.0t';

resourcestring
  CrossGCCMsg =
       '{$ifdef FPC_CROSSCOMPILING}'+ sLineBreak +
       '  {$ifdef Linux}'+ sLineBreak +
       '    // for most versions of Linux in case of linking errors'+ sLineBreak +
       '    {$linklib libc_nonshared.a}'+ sLineBreak +
       '    {$IFDEF CPUARM}'+ sLineBreak +
       '      // for GUI on RPi[1,2,3] with Arch Linux in case of linking errors'+ sLineBreak +
       '      // {$linklib GLESv2}'+ sLineBreak +
       '    {$ENDIF}'+ sLineBreak +
       '  {$endif}'+ sLineBreak +
       '{$endif}';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  IniFilesOk:boolean;
  aTarget:string;
begin
  FPCupManager:=nil;

  {$ifdef CPUAARCH64}
  // disable some features
  FixesBtn.Enabled:=False;
  StableBtn.Enabled:=False;
  OldBtn.Enabled:=False;
  DinoBtn.Enabled:=False;
  RadioGroup1.Enabled:=False;
  RadioGroup2.Enabled:=False;
  ButtonInstallCrossCompiler.Enabled:=False;
  {$endif CPUAARCH64}
  {$ifdef CPUARM}
  // disable some features
  FixesBtn.Enabled:=False;
  StableBtn.Enabled:=False;
  RadioGroup1.Enabled:=False;
  RadioGroup2.Enabled:=False;
  ButtonInstallCrossCompiler.Enabled:=False;
  {$endif CPUARM}

  {$ifdef Darwin}
  RadioGroup2.Items.Strings[RadioGroup2.Items.IndexOf('wince')]:='i-sim';
  {$endif Darwin}

  oldoutput := System.Output;
  AssignSynEdit(System.Output, SynEdit1);
  Reset(System.Input);
  Rewrite(System.Output);

  Self.Caption:=
    'FPCUPdeluxe V'+
    DELUXEVERSION+
    ' base fpcup'+
    RevisionStr+
    ' ('+
    VersionDate+
    ') for ' +
    GetTargetCPUOS
    {$ifdef Darwin}
      {$ifdef LCLCARBON}
      +'-carbon'
      {$endif}
      {$ifdef LCLCOCOA}
      +'-cocoa'
      {$endif}
      {$ifdef LCLQT5}
      +'-qt5'
      {$endif}
    {$endif}
    ;

  sStatus:='Sitting and waiting';

  {$IFDEF MSWINDOWS}
  sInstallDir:='C:\fpcupdeluxe';
  {$ELSE}
  sInstallDir:='~/fpcupdeluxe';
  {$ENDIF}

  {$ifdef DARWIN}
  // we could have started from with an .app , so goto the basedir ... not sure if realy needed, but to be sure.
  SetCurrentDir(ExcludeTrailingPathDelimiter(SafeGetApplicationPath));
  {$endif}
  // get last used install directory, proxy and visual settings
  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    sInstallDir:=ReadString('General','InstallDirectory',sInstallDir);
    CheckAutoClear.Checked:=ReadBool('General','AutoClear',True);
    SynEdit1.Font.Size := ReadInteger('General','CommandFontSize',SynEdit1.Font.Size);
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

  IniFilesOk:=
  (SaveInisFromResource(SafeGetApplicationPath+installerUniversal.SETTTINGSFILENAME,'settings_ini'))
  AND
  (SetConfigFile(SafeGetApplicationPath+installerUniversal.CONFIGFILENAME));

  if IniFilesOk then
  begin
    aTarget:='stable';
    if ListBoxFPCTarget.Count=0 then
    begin
      ListBoxFPCTarget.Items.CommaText:=installerUniversal.GetAlias('fpcURL','list');
      {$ifdef CPUAARCH64}
      aTarget:='trunk';
      {$endif}
      FPCTarget:=aTarget;
    end;
    if ListBoxLazarusTarget.Count=0 then
    begin
      ListBoxLazarusTarget.Items.CommaText:=installerUniversal.GetAlias('lazURL','list');
      {$ifdef LCLQT5}
      aTarget:='trunk';
      {$endif}
      {$ifdef LCLCOCOA}
      aTarget:='trunk';
      {$endif}
      LazarusTarget:=aTarget;
    end;

    sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName(sInstallDir));

    InstallDirEdit.Text:=sInstallDir;

    // set InstallDirEdit (installdir) onchange here, to prevent early firing
    InstallDirEdit.OnChange:=nil;
    InstallDirEdit.OnKeyUp:=nil;
    {$ifdef Darwin}
    {$ifdef LCLCOCOA}
    // onchange does not work on cocoa, so use onkeyup
    InstallDirEdit.OnKeyUp:=@Edit1KeyUp;
    {$endif}
    {$endif}
    if InstallDirEdit.OnKeyUp=nil then InstallDirEdit.OnChange:=@Edit1Change;

    // create settings form
    // must be done here, to enable local storage/access of some setttings !!
    Form2:=TForm2.Create(Form1);

    AddMessage('Welcome @ FPCUPdeluxe.');
    AddMessage('');

    InitFPCupManager;
  end
  else
  begin
    AddMessage('');
    AddMessage('FPCUPdeluxe could not create its necessary setting-files.');
    AddMessage('All functions are disabled for now.');
    AddMessage('');
    AddMessage('Please check the folder permissions, and re-start.');
    AddMessage('');
    DisEnable(nil,False);
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPCupManager);
  (* using CloseFile will ensure that all pending output is flushed *)
  CloseFile(System.Output);
  System.Output := oldoutput;
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

procedure TForm1.ButtonAutoUpdateCrossCompiler(Sender: TObject);
begin
  AutoUpdateCrossCompiler(Sender);
end;

function TForm1.AutoUpdateCrossCompiler(Sender: TObject):boolean;
var
  CPUType:TCPU;
  OSType:TOS;
  FPCCfg:string;
  BinPath:string;
  ConfigText: TStringList;
  aCPU, aOS: string;
  // tricky: to be changed; todo
  aRadiogroup_CPU,aRadiogroup_OS: string;
  CheckAutoClearStore:boolean;
  success:boolean;
begin
  aOS := GetTargetOS;
  aCPU := GetTargetCPU;
  BinPath:=IncludeTrailingPathDelimiter(sInstallDir)+'fpc'+DirectorySeparator+'bin'+DirectorySeparator+aCPU + '-' + aOS;
  FPCCfg := IncludeTrailingPathDelimiter(BinPath) + 'fpc.cfg';

  result:=false;

  if NOT FileExists(FPCCfg) then
  begin
    if (Sender<>nil) then AddMessage('FPC configfile [fpc.cfg] not found in ' + BinPath);
    exit;
  end;

  if (Sender<>nil) then
  begin

    CheckAutoClearStore:=CheckAutoClear.Checked;
    if CheckAutoClearStore then Button8.Click;
    CheckAutoClear.Checked:=false;

    Memo1.Lines.Append('Going to auto-build all installed cross-compilers !');
    Memo1.Lines.Append('Checking FPC configfile [fpc.cfg] for cross-compilers in ' + BinPath);
    Memo1.Lines.Append('');

  end
  else
  begin
    Memo1.Clear;
  end;

  ConfigText:=TStringList.Create;
  try
    ConfigText.LoadFromFile(FPCCFG);

    success:=true;

    for OSType := Low(TOS) to High(TOS) do
    begin

      if (NOT success) then break;

      aOS:=GetEnumNameSimple(TypeInfo(TOS),Ord(OSType));

      for CPUType := Low(TCPU) to High(TCPU) do
      begin

        if (NOT success) then break;

        aCPU:=GetEnumNameSimple(TypeInfo(TCPU),Ord(CPUType));

        // tricky; see above; improvement todo
        aRadiogroup_CPU:=aCPU;
        aRadiogroup_OS:=aOS;
        if aRadiogroup_CPU='powerpc' then aRadiogroup_CPU:='ppc';
        if aRadiogroup_CPU='powerpc64' then aRadiogroup_CPU:='ppc64';
        if aRadiogroup_OS='iphonesim' then aRadiogroup_OS:='i-sim';

        if aOS='windows' then
        begin
          if aCPU='i386' then aOS:='win32';
          if aCPU='x86_64' then aOS:='win64';
        end;

        {$ifdef win32}
        // On win32, we always build a win64 cross-compiler.
        // So, if the win32 install is updated, this cross-compiler is also updated already auto-magically.
        // We can skip it here, in that case.
        if aOS='win64' then continue;
        {$endif}

        if (ConfigText.IndexOf(SnipMagicBegin+aCPU+'-'+aOS)<>-1) then
        begin
          AddMessage('Crosscompiler for '+aCPU + '-' + aOS+' found !');
          if (Sender<>nil) then
          begin
            SynEdit1.Clear;
            Memo1.Lines.Append('Crosscompiler for '+aCPU + '-' + aOS+' found !');
            Memo1.Lines.Append('Going to update cross-compiler.');
            RadioGroup1.ItemIndex:=RadioGroup1.Items.IndexOf(aRadiogroup_CPU);
            RadioGroup2.ItemIndex:=RadioGroup2.Items.IndexOf(aRadiogroup_OS);
            success:=InstallCrossCompiler(nil);
            if success
              then Memo1.Lines.Append('Cross-compiler update ok.')
              else Memo1.Lines.Append('Failure during update of cross-compiler !!');
            Memo1.Lines.Append('');
          end;
        end;
      end;
    end;

    if (Sender<>nil) then
    begin
      RadioGroup1.ItemIndex:=-1;
      RadioGroup2.ItemIndex:=-1;
      CheckAutoClear.Checked:=CheckAutoClearStore;
    end;

  finally
    ConfigText.Free;
  end;

  result:=success;

end;

procedure TForm1.InitFPCupManager;
var
  SortedModules: TStringList;
  i:integer;
  s:string;
begin
  FPCupManager:=TFPCupManager.Create;

  FPCupManager.ConfigFile:=SafeGetApplicationPath+installerUniversal.CONFIGFILENAME;

  FPCupManager.LoadFPCUPConfig;

  FPCupManager.FPCURL:='stable';
  FPCupManager.LazarusURL:='stable';
  FPCupManager.Verbose:=true;

  //CheckFPCUPOptions(FPCupManager);

  FPCupManager.PatchCmd:='patch';

  if listbox3.Count=0 then
  begin
    SortedModules:=TStringList.Create;
    try
      for i:=0 to FPCupManager.ModulePublishedList.Count-1 do
      begin
        s:=FPCupManager.ModulePublishedList[i];
        // tricky ... get out the modules that are packages only
        // not nice, but needed to keep list clean of internal commands
        if (FPCupManager.ModulePublishedList.IndexOf(s+'clean')<>-1)
            AND (FPCupManager.ModulePublishedList.IndexOf(s+'uninstall')<>-1)
            AND (s<>'FPC')
            AND (s<>'lazarus')
            AND (s<>'default')
            AND (FPCupManager.ModuleEnabledList.IndexOf(s)=-1)
            then
        begin
          SortedModules.Add(s);
        end;
      end;
      listbox3.Items.AddStrings(SortedModules);

    finally
      SortedModules.Free;
    end;
  end;

  FPCupManager.HTTPProxyPort:=Form2.HTTPProxyPort;
  FPCupManager.HTTPProxyHost:=Form2.HTTPProxyHost;
  FPCupManager.HTTPProxyUser:=Form2.HTTPProxyUser;
  FPCupManager.HTTPProxyPassword:=Form2.HTTPProxyPass;

  // localize FPCUPSettings if possible
  if (NOT GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir)))
     then GetFPCUPSettings(SafeGetApplicationPath);
end;


procedure TForm1.BitBtnHaltClick(Sender: TObject);
begin
  if (MessageDlg('I am going to try to halt.' + sLineBreak +
             'Do not (yet) expect too much of it.' + sLineBreak +
             'Its a non-finished feature !'
             ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
             begin
               exit;
             end;

  if Assigned(FPCupManager.Sequencer.Installer) then
  begin
    FPCupManager.Sequencer.Installer.Processor.Terminate(-1);
  end;
  // brute force ... nothing better at the moment
  // but still does not work when downloading from SVN
  // the process that gets created when downloading if not reachable from here through the fpcupmanager
  FPCupManager.Destroy;
  InitFPCupManager;
  //DisEnable(Sender,True);
end;

procedure TForm1.TargetSelectionChange(Sender: TObject; User: boolean);
begin
  if Sender=ListBoxFPCTarget then FFPCTarget:=ListBoxFPCTarget.Items[ListBoxFPCTarget.ItemIndex];
  if Sender=ListBoxLazarusTarget then FLazarusTarget:=ListBoxLazarusTarget.Items[ListBoxLazarusTarget.ItemIndex];
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if (RadioGroup1.ItemIndex<>-1) then
  begin
  if (RadioGroup1.Items[RadioGroup1.ItemIndex]='i8086') then
    begin
      RadioGroup2.ItemIndex:=-1;
      RadioGroup2.Enabled:=false;
    end
    else RadioGroup2.Enabled:=true;
  end
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  if (RadioGroup2.ItemIndex<>-1) then
  begin
    if (RadioGroup2.Items[RadioGroup2.ItemIndex]='java') OR (RadioGroup2.Items[RadioGroup2.ItemIndex]='msdos') then
    begin
      RadioGroup1.ItemIndex:=-1;
      RadioGroup1.Enabled:=false;
    end else RadioGroup1.Enabled:=true;
  end;
end;

procedure TForm1.SynEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    if (WheelDelta>0) AND (SynEdit1.Font.Size<48) then SynEdit1.Font.Size:=SynEdit1.Font.Size+1;
    if (WheelDelta<0)  AND (SynEdit1.Font.Size>2) then SynEdit1.Font.Size:=SynEdit1.Font.Size-1;
  end;
end;

procedure TForm1.SynEdit1Change(Sender: TObject);
var
  s,searchstring:string;
  x:integer;
begin
  s:=SynEdit1.LineText;
  //if Length(s)=0 then s:=SynEdit1.Lines[SynEdit1.CaretY-2];
  s:=Trim(s);
  if Length(s)=0 then exit;

  searchstring:='checking out/updating';
  if (ExistWordInString(PChar(s),searchstring,[soDown])) then
  begin
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      InternalError:=Copy(s,x+1,MaxInt);
      Memo1.Lines.Append('Getting/updating '+InternalError);
    end;
  end;

  if (ExistWordInString(PChar(s),'checkout',[soWholeWord,soDown])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord,soDown])) then
  begin
    Memo1.Lines.Append('Performing a SVN/GIT checkout ... please wait, could take some time.');
  end;

  if (ExistWordInString(PChar(s),'switch',[soWholeWord,soDown])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord,soDown])) then
  begin
    Memo1.Lines.Append('Performing a SVN repo URL switch ... please wait, could take some time.');
  end;

  // github error
  if (ExistWordInString(PChar(s),'429 too many requests',[soDown])) then
  begin
    Memo1.Lines.Append('GitHub blocked us due to too many download requests.');
    Memo1.Lines.Append('This will last for an hour, so please wait and be patient.');
    Memo1.Lines.Append('After this period, please re-run fpcupdeluxe.');
  end;

  (*
  searchstring:='the makefile doesn''t support target';
  if (ExistWordInString(PChar(s),searchstring,[soDown])) then
  begin
    Memo1.Lines.Append('Sorry, but you have chosen a target that is not supported (yet).');
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      InternalError:=Copy(s,x+1,MaxInt);
      x:=Pos(',',LowerCase(InternalError));
      if x=0 then x:=Pos(' ',LowerCase(InternalError));
      if x>0 then
      begin
        InternalError:=Copy(InternalError,1,x-1);
        Memo1.Lines.Append('Wrong target: '+InternalError);
      end;
    end;
  end;
  *)

  searchstring:='unable to connect to a repository at url';
  if (ExistWordInString(PChar(s),searchstring,[soDown])) then
  begin
    Memo1.Lines.Append('SVN could not connect to the desired repository.');
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      InternalError:=Copy(s,x+1,MaxInt);
      Memo1.Lines.Append('URL: '+InternalError);
      Memo1.Lines.Append('Please check your connection. Or run the SVN command to try yourself:');
      Memo1.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end;
  end;

  if (ExistWordInString(PChar(s),'error:',[soWholeWord,soDown])) OR (ExistWordInString(PChar(s),'fatal:',[soWholeWord,soDown])) then
  begin
    if (ExistWordInString(PChar(s),'fatal: internal error',[soDown])) then
    begin
      x:=RPos(' ',s);
      if x>0 then
      begin
        InternalError:=Copy(s,x+1,MaxInt);
        Memo1.Lines.Append('Compiler error: '+InternalError);
        if (InternalError='2015030501') OR (InternalError='2014051001') OR (InternalError='2014050604') then
        begin
          Memo1.Lines.Append('FPC revision 30351 introduced some changed into the compiler causing this error.');
          Memo1.Lines.Append('Has something todo about how floating points are handled. And that has changed.');
          Memo1.Lines.Append('See: http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=30351');
        end;

        if (InternalError='2013051401') then
        begin
          Memo1.Lines.Append('FPC revision 37182 breaks cross building avr-embedded.');
          Memo1.Lines.Append('However, this has been solved in the meantime !');
          Memo1.Lines.Append('Please update FPC trunk !!');
          //Memo1.Lines.Append('See: https://bugs.freepascal.org/view.php?id=32418');
          //Memo1.Lines.Append('See: https://bugs.freepascal.org/view.php?id=31925');
        end;

      end;
    end
    else if (ExistWordInString(PChar(s),'error: user defined',[soDown])) then
    begin
      x:=Pos('error: user defined',LowerCase(s));
      if x>0 then
      begin
        x:=x+Length('error: user defined');
        InternalError:=Copy(s,x+2,MaxInt);
        Memo1.Lines.Append('Configuration error: '+InternalError);
        x:=Pos('80 bit extended floating point',LowerCase(s));
        if x>0 then
        begin
          //Memo1.Lines.Append('Please use trunk that has 80-bit float type using soft float unit.');
          //Memo1.Lines.Append('FPC revisions 37294 - 37306 add this soft float feature.');
          //Memo1.Lines.Append('So update your FPC trunk to a revision > 37306 !!');
          Memo1.Lines.Append('See: http://bugs.freepascal.org/view.php?id=29892');
          Memo1.Lines.Append('See: http://bugs.freepascal.org/view.php?id=9262');
        end;
      end;
    end
    else if (ExistWordInString(PChar(s),'failed to get crossbinutils',[soDown])) then
    begin
      MissingCrossBins:=true;
      Memo1.Lines.Append('Missing correct cross binary utilities');
    end
    else if (ExistWordInString(PChar(s),'failed to get crosslibrary',[soDown])) then
    begin
      MissingCrossLibs:=true;
      Memo1.Lines.Append('Missing correct cross libraries');
    end
    else if (Pos('error: 256',lowercase(s))>0) AND (Pos('svn',lowercase(s))>0) then
    begin
      Memo1.Lines.Append('We have had a SVN connection failure. Just start again !');
      Memo1.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end
    else if (ExistWordInString(PChar(s),'fatal:',[soDown])) then
    begin
      Memo1.Lines.Append(s);
      Memo1.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end
    else if (ExistWordInString(PChar(s),'error:',[soDown])) then
    begin
      // check if "error:" at the end of the line.
      // if so:
      // the real error will follow on the next line(s).
      // and we have to wait for these lines (done somewhere else in this procedure) !!
      // if not, just print the error message.
      if (Pos('error:',lowercase(s))<>(Length(s)-Length('error:')+1)) then Memo1.Lines.Append(s);
    end;
  end;

  // linker error
  if (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find',[soDown])) then
  begin
    x:=Pos('-l',s);
    if x>0 then
    begin
      // add help into summary memo
      Memo1.Lines.Append('Missing library: lib'+Copy(s,x+2,MaxInt));
    end;
  end;

  // diskspace errors
  if (ExistWordInString(PChar(s),'Stream write error',[soDown])) OR (ExistWordInString(PChar(s),'disk full',[soDown])) then
  begin
    Memo1.Lines.Append('There is not enough diskspace to finish this operation.');
    Memo1.Lines.Append('Please free some space and re-run fpcupdeluxe.');
  end;

  // RAM errors
  if (ExistWordInString(PChar(s),'Can''t call the assembler',[soDown])) then
  begin
    Memo1.Lines.Append('Most likely, there is not enough RAM (swap) to finish this operation.');
    Memo1.Lines.Append('Please add some swap-space (1GB) and re-run fpcupdeluxe.');
  end;

  // warn for time consuming help files
  if (ExistWordInString(PChar(s),'writing',[soDown])) AND (ExistWordInString(PChar(s),'pages...',[soDown])) then
  begin
    Memo1.Lines.Append('Busy with help files. Be patient: can be time consuming !!');
  end;

  // report about correct tools that are found and used
  if (ExistWordInString(PChar(s),'found correct',[soDown])) then
  begin
    Memo1.Lines.Append(s);
  end;

  if ExistWordInString(PChar(s),BeginSnippet,[soWholeWord,soDown]) then
  begin
    if ExistWordInString(PChar(s),Seriousness[etWarning],[soWholeWord,soDown]) then
    begin
      // repeat fpcupdeluxe warning
      Memo1.Lines.Append(s);
    end;
  end;

  // go back a few lines to find a special error case
  x:=(SynEdit1.CaretY-4);
  if (x>0) then
  begin
    s:=SynEdit1.Lines[x];
    s:=Trim(s);
    s:=LowerCase(s);
    if Length(s)=0 then exit;
    // check if "error:" at the end of the line.
    // if so:
    // the real error will follow on the next line(s).
    // and we have to wait for these lines !!
    // if not, just print the error message (done somewhere else in this procedure).
    if (Pos('error:',s)>0) AND (Pos('error:',s)=(Length(s)-Length('error:')+1))
    then
    begin
      // print the error itself and the next 2 lines (good or lucky guess)
      Memo1.Lines.Append(SynEdit1.Lines[x]);
      Memo1.Lines.Append(SynEdit1.Lines[x+1]);
      Memo1.Lines.Append(SynEdit1.Lines[x+2]);
    end;
  end;

end;

procedure TForm1.SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
var
  FG, BG: TColor;
  s:string;
begin
  s:=SynEdit1.Lines[Line-1];
  s:=Trim(s);
  if Length(s)=0 then exit;

  if (NOT Special) AND ExistWordInString(PChar(s),BeginSnippet,[soWholeWord,soDown]) then
  begin
    if ExistWordInString(PChar(s),Seriousness[etInfo],[soWholeWord,soDown]) then
    begin
      FG      := clYellow;
      BG      := clBlack;
      Special := True;
    end;
    if ExistWordInString(PChar(s),Seriousness[etWarning],[soWholeWord,soDown]) then
    begin
      FG      := clFuchsia;
      BG      := clBlack;
      Special := True;
    end;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'executing:',[soWholeWord,soDown]) then
  begin
    FG      := clAqua;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'A',[soWholeWord])) OR (ExistWordInString(PChar(s),'U',[soWholeWord]))) then
  begin
    FG      := clSkyBlue;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'Please wait:',[soWholeWord,soDown]) then
  begin
    FG      := clBlue;
    BG      := clWhite;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'failed:',[soWholeWord,soDown]) then
  begin
    FG      := TColor($FF00AF); //Text Color  BGR
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'hint:',[soWholeWord,soDown])) OR (ExistWordInString(PChar(s),'note:',[soWholeWord,soDown]))) then
  begin
    FG      := clGreen;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'warning:',[soWholeWord,soDown]))) then
  begin
    FG      := clMaroon;
    BG      := clBlack;
    Special := True;
  end;

  // linker error
  if (NOT Special) AND (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find',[soDown])) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
  end;

  // diskspace error
  if (NOT Special) AND ((ExistWordInString(PChar(s),'Stream write error',[soDown])) OR (ExistWordInString(PChar(s),'disk full',[soDown]))) then
  begin
    FG      := clRed;
    BG      := clAqua;
    Special := True;
  end;

  // github error
  if (NOT Special) AND (ExistWordInString(PChar(s),'429 too many requests',[soDown])) then
  begin
    FG      := clRed;   //Text Color
    BG      := clNavy;  //BackGround
    Special := True;    //Must be true
  end;

  // makefile and help warnings
  if (NOT Special)
  AND
  (
    (ExistWordInString(PChar(s),'lines compiled,',[soDown]))
    OR
    (ExistWordInString(PChar(s),'issued',[soWholeWord,soDown]))
    OR
    (ExistWordInString(PChar(s),'Target OS: ',[soDown]))
    OR
    (ExistWordInString(PChar(s),'make.exe: ',[soDown]))
    OR
    (ExistWordInString(PChar(s),'make: ',[soDown]))
    OR
    (ExistWordInString(PChar(s),'echo ',[soDown]))
    OR
    (ExistWordInString(PChar(s),'now ',[soDown]))
    OR
    (ExistWordInString(PChar(s),'this could take some time',[soDown]))
    OR
    (
      (ExistWordInString(PChar(s),'writing',[soDown]))
      AND
      (ExistWordInString(PChar(s),'pages...',[soDown]))
    )
  )
  then
  begin
    FG      := TColor($AF10FF); //Text Color  BGR
    BG      := clBlack;
    Special := True;
  end;

  // svn connection error
  if (NOT Special) AND (ExistWordInString(PChar(s),'unable to connect to a repository at url',[soDown])) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'svn: e',[soDown]) then
  begin
    FG      := clFuchsia;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND (ExistWordInString(PChar(s),'make.exe ',[soDown]) OR ExistWordInString(PChar(s),'make ',[soDown]) OR ExistWordInString(PChar(s),'gmake ',[soDown])) then
  begin
    FG      := TColor($FF8C00);
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'start compiling package',[soDown]) then
  begin
    FG      := TColor($FFA000);
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'success:',[soWholeWord,soDown]) then
  begin
    FG      := TColor($00D7FF);
    BG      := clBlack;
    Special := True;
  end;


  if (NOT Special) AND (ExistWordInString(PChar(s),'compiled package',[soDown]) OR ExistWordInString(PChar(s),'succeeded',[soDown]) OR ExistWordInString(PChar(s),'completed',[soDown])) then
  begin
    FG      := TColor($00A5FF);
    BG      := clBlack;
    Special := True;
  end;

  // special override for debugging statemachine
  if ExistWordInString(PChar(s),'sequencer',[soWholeWord,soDown]) then
  begin
    begin
      FG      := clRed;
      BG      := clBlack;
      Special := True;
    end;
  end;

  if (ExistWordInString(PChar(s),'error:',[soWholeWord,soDown])) OR  (ExistWordInString(PChar(s),'fatal:',[soWholeWord,soDown])) then
  begin
    // skip git fatal messages ... they are not that fatal ... but not sure yet !
    // if (Pos('fatal: not a git repository',lowercase(s))=0) then
    begin
      FG      := clRed;
      BG      := clBlue;
      Special := True;
    end;
  end;

  // special override for me: for easy debugging FPC and Lazarus source with plain writelines in source
  if ExistWordInString(PChar(s),'donalf:',[soWholeWord,soDown]) then
  begin
    begin
      FG      := clBlack;
      BG      := clYellow;
      Special := True;
    end;
  end;

  if Special then
  begin
    Markup.Background:=BG;
    Markup.Foreground:=FG;
  end;

end;

procedure TForm1.QuickBtnClick(Sender: TObject);
var
  s:string;
  FPCRevision,FPCBranch:string;
  LazarusRevision,LazarusBranch:string;
begin

  {$ifdef CPUAARCH64}
  if (Sender<>TrunkBtn) AND (Sender<>NPBtn) then
  begin
    MessageDlg('Aarch64 is only supported by FPC trunk (or NewPascal).',mtError,[],0);
    exit;
  end;
  {$endif CPUAARCH64}

  DisEnable(Sender,False);
  try
    PrepareRun;

    FPCBranch:=FPCupManager.FPCDesiredBranch;
    LazarusBranch:=FPCupManager.LazarusDesiredBranch;
    FPCRevision:=FPCupManager.FPCDesiredRevision;
    LazarusRevision:=FPCupManager.LazarusDesiredRevision;

    if Sender=TrunkBtn then
    begin
      s:='Going to install both FPC trunk and Lazarus trunk';
      FPCTarget:='trunk';
      LazarusTarget:='trunk';
    end;

    if Sender=NPBtn then
    begin
      s:='Going to install NewPascal release';
      FPCTarget:='newpascal';
      FPCBranch:='release';
      LazarusTarget:='newpascal';
      LazarusBranch:='release';
      //FPCupManager.IncludeModules:='mORMotFPC,zeos';
    end;

    if Sender=FixesBtn then
    begin
      s:='Going to install FPC fixes and Lazarus fixes';
      FPCTarget:='fixes';
      LazarusTarget:='fixes';
    end;

    if Sender=StableBtn then
    begin
      s:='Going to install FPC stable and Lazarus stable';
      FPCTarget:='stable';
      LazarusTarget:='stable';
    end;

    if Sender=OldBtn then
    begin
      s:='Going to install FPC 2.6.4 and Lazarus 1.4 ';
      FPCTarget:='2.6.4';
      LazarusTarget:='1.4';
    end;

    if Sender=DinoBtn then
    begin
      s:='Going to install FPC 2.0.2 and Lazarus 0.9.16 ';
      FPCTarget:='2.0.2';
      //LazarusTarget:='0.9.4';
      LazarusTarget:='0.9.16';
      FPCupManager.OnlyModules:='fpc,oldlazarus';
    end;

    if Sender=FeaturesBtn then
    begin
      ShowMessage('Not yet working ... too many included packages will give fatal errors.');
      exit;
      {
      s:='Going to install FPC trunk and Lazarus trunk with extras ';
      FPCTarget:='trunk';
      LazarusTarget:='trunk';
      FPCupManager.IncludeModules:='mORMotFPC,lazgoogleapis,virtualtreeview,lazpaint,bgracontrols,uecontrols,ECControls,zeos,cudatext,indy,lnet,lamw,mupdf,tiopf,abbrevia,uos,wst,anchordocking,simplegraph,cm630commons,turbobird';
      }
    end;

    if Sender=mORMotBtn then
    begin
      s:='Going to install de special version of mORMot for FPC ';
      FPCTarget:='skip';
      LazarusTarget:='skip';
      FPCupManager.OnlyModules:='mORMotFPC';
      //FPCupManager.OnlyModules:='mORMotFPC,zeos';
    end;

    FPCupManager.FPCURL:=FPCTarget;
    FPCupManager.LazarusURL:=LazarusTarget;

    FPCupManager.FPCDesiredBranch:=FPCBranch;
    FPCupManager.LazarusDesiredBranch:=LazarusBranch;
    FPCupManager.FPCDesiredRevision:=FPCRevision;
    FPCupManager.LazarusDesiredRevision:=LazarusRevision;

    if (NOT Form2.IncludeHelp) then
      FPCupManager.SkipModules:=FPCupManager.SkipModules+'helpfpc,helplazarus'
    else
      FPCupManager.IncludeModules:=FPCupManager.IncludeModules+'lhelp';

    AddMessage(s+'.');

    sStatus:=s;

    RealRun;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.BitBtnFPCandLazarusClick(Sender: TObject);
var
  FModuleList: TStringList;
begin
  if (ListBoxFPCTarget.ItemIndex=-1) or (ListBoxLazarusTarget.ItemIndex=-1) then
  begin
    ShowMessage('Please select a FPC and Lazarus version first');
    exit;
  end;

  {$ifdef CPUAARCH64}
  if (MessageDlg('Be forwarned: this will only work with FPC trunk (or NewPascal).' + sLineBreak +
                 'An aarch64 fpcupdeluxe bootstrapper wil be used.' + sLineBreak +
                 'Do you want to continue ?'
                 ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                 begin
                   exit;
                 end;
  {$endif CPUAARCH64}

  DisEnable(Sender,False);
  try
    PrepareRun;

    AddMessage('Going to install/update FPC and Lazarus with given options.');
    sStatus:='Going to install/update FPC and Lazarus.';

    if Form2.UpdateOnly then
    begin
      // still not working 100% for Lazarus ...  todo
      // packages that are installed by the user are not included
      FPCupManager.OnlyModules:='FPCCleanAndBuildOnly,LazCleanAndBuildOnly';
      FModuleList:=TStringList.Create;
      try
        GetModuleEnabledList(FModuleList);
        // also include enabled modules (packages) when rebuilding Lazarus
        if FModuleList.Count>0 then FPCupManager.OnlyModules:=FPCupManager.OnlyModules+','+FModuleList.CommaText;
      finally
        FModuleList.Free;
      end;
    end
    else
    begin
      if (NOT Form2.IncludeHelp) then
        FPCupManager.SkipModules:=FPCupManager.SkipModules+'helpfpc,helplazarus'
      else
        FPCupManager.IncludeModules:=FPCupManager.IncludeModules+'lhelp';
    end;

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
  try

    if ListBox3.SelCount=0 then
    begin
      AddMessage('Please select a module / package.');
      exit;
    end;

    PrepareRun;

    FPCupManager.ExportOnly:=(NOT Form2.CheckPackageRepo.Checked);

    modules:='';
    for i:=0 to ListBox3.Count-1 do
    begin
      if ListBox3.Selected[i] then modules:=modules+ListBox3.Items[i]+',';
    end;

    if Length(modules)>0 then
    begin
      // delete stale trailing comma
      Delete(modules,Length(modules),1);
      FPCupManager.OnlyModules:=modules;
      AddMessage('Limiting installation/update to '+FPCupManager.OnlyModules);
      AddMessage('');
      AddMessage('Going to install selected modules with given options.');
      sStatus:='Going to install/update selected modules.';
      RealRun;
    end;
  finally
    FPCupManager.ExportOnly:=(NOT Form2.CheckRepo.Checked);
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir:=sInstallDir;
  if SelectDirectoryDialog1.Execute then
  begin
    sInstallDir:=SelectDirectoryDialog1.FileName;
    InstallDirEdit.Text:=sInstallDir;
  end;
end;

procedure TForm1.ButtonInstallCrossCompilerClick(Sender: TObject);
begin
  InstallCrossCompiler(Sender);
end;

function TForm1.InstallCrossCompiler(Sender: TObject):boolean;
var
  BinsURL,LibsURL,DownloadURL,TargetFile,TargetPath,BinPath,LibPath,UnZipper,s:string;
  success,verbose:boolean;
  UseNativeUnzip:boolean;
  IncludeLCL:boolean;
  {$ifdef Unix}
  fileList: TStringList;
  i:integer;
  {$endif}
begin

  result:=false;

  if (RadioGroup1.ItemIndex=-1) and (RadioGroup2.ItemIndex=-1) then
  begin
    ShowMessage('Please select a CPU and OS target first');
    exit;
  end;

  // the below three checks are just temporary and rough
  // we should use the array OSCPUSupported : array[TOS,TCpu] of boolean

  success:=true;
  if RadioGroup2.ItemIndex<>-1 then
  begin
    s:=RadioGroup2.Items[RadioGroup2.ItemIndex];
    if s='embedded' then
    begin
      if RadioGroup1.ItemIndex<>-1 then
      begin
        s:=RadioGroup1.Items[RadioGroup1.ItemIndex];
        if (s<>'avr') and (s<>'arm') and (s<>'mipsel') then
        begin
          success:=false;
        end;
      end else success:=false;
    end;
  end;

  if (NOT success) then
  begin
    ShowMessage('No valid CPU target for embedded.');
    exit;
  end;

  success:=true;
  if RadioGroup2.ItemIndex<>-1 then
  begin
    s:=RadioGroup2.Items[RadioGroup2.ItemIndex];
    if s='android' then
    begin
      if RadioGroup1.ItemIndex<>-1 then
      begin
        s:=RadioGroup1.Items[RadioGroup1.ItemIndex];
        if (s<>'i386') and (s<>'arm') and (s<>'mipsel') and (s<>'jvm') then
        begin
          success:=false;
        end;
      end else success:=false;
    end;
  end;

  if (NOT success) then
  begin
    ShowMessage('No valid CPU target for android.');
    exit;
  end;

  success:=true;
  if RadioGroup1.ItemIndex<>-1 then
  begin
    s:=RadioGroup1.Items[RadioGroup1.ItemIndex];
    if s='jvm' then
    begin
      if RadioGroup2.ItemIndex<>-1 then
      begin
        s:=RadioGroup2.Items[RadioGroup2.ItemIndex];
        if (s<>'android') and (s<>'java') then
        begin
          success:=false;
        end;
      end else success:=false;
    end;
  end;

  if (NOT success) then
  begin
    ShowMessage('No valid OS target for jvm.');
    exit;
  end;

  PrepareRun;

  if RadioGroup1.ItemIndex<>-1 then
  begin
    s:=RadioGroup1.Items[RadioGroup1.ItemIndex];
    if s='ppc' then s:='powerpc';
    if s='ppc64' then s:='powerpc64';
    FPCupManager.CrossCPU_Target:=s;
  end;
  if RadioGroup2.ItemIndex<>-1 then
  begin
    s:=RadioGroup2.Items[RadioGroup2.ItemIndex];
    if s='i-sim' then s:='iphonesim';
    FPCupManager.CrossOS_Target:=s;
  end;

  if (FPCupManager.CrossOS_Target='java') then FPCupManager.CrossCPU_Target:='jvm';
  if (FPCupManager.CrossOS_Target='msdos') then FPCupManager.CrossCPU_Target:='i8086';
  if (FPCupManager.CrossCPU_Target='i8086') then FPCupManager.CrossOS_Target:='msdos';

  if FPCupManager.CrossOS_Target='windows' then
  begin
    if FPCupManager.CrossCPU_Target='i386' then FPCupManager.CrossOS_Target:='win32';
    if FPCupManager.CrossCPU_Target='x86_64' then FPCupManager.CrossOS_Target:='win64';
  end;

  if Sender<>nil then
  begin

    if (FPCupManager.CrossCPU_Target='') then
    begin
      Application.MessageBox(PChar('Please select a CPU target first.'), PChar('CPU error'), MB_ICONERROR);
      FPCupManager.CrossOS_Target:=''; // cleanup
      exit;
    end;

    if (FPCupManager.CrossOS_Target='') then
    begin
      Application.MessageBox(PChar('Please select an OS target first.'), PChar('OS error'), MB_ICONERROR);
      FPCupManager.CrossCPU_Target:=''; // cleanup
      exit;
    end;

    {$ifdef Linux}
    if (FPCupManager.CrossOS_Target='darwin') then
    begin
      success:=CheckExecutable('clang', '-v', '');
      if (NOT success) then
      begin
        s:=
        'Clang cannot be found !!'+ sLineBreak +
        'Clang need to be installed to be able to cross-compile towards Darwin !'+ sLineBreak +
        'Install clang and retry !!';
        Application.MessageBox(PChar(s), PChar('Missing clang'), MB_ICONERROR);
        Memo1.Lines.Append('');
        Memo1.Lines.Append('To get clang: sudo apt-get install clang');
        exit;
      end;
    end;
    {$endif}

    if (FPCupManager.CrossOS_Target='java') then
    begin
      success:=CheckExecutable('java', '-version', '');
      if (NOT success) then
      begin
        s:=
        'Java cannot be found !!'+ sLineBreak +
        'Java need to be installed to be able to cross-compile towards java !'+ sLineBreak +
        'Install java and retry !!';
        Application.MessageBox(PChar(s), PChar('Missing java'), MB_ICONERROR);
        {$ifdef Linux}
        Memo1.Lines.Append('');
        Memo1.Lines.Append('To get java: sudo apt-get install default-jre');
        {$endif}
        exit;
      end;
    end;

    {$ifndef BSD}
    if (Pos('bsd',FPCupManager.CrossOS_Target)>0) then
    //if (FPCupManager.CrossOS_Target='freebsd') OR (FPCupManager.CrossOS_Target='netbsd') OR (FPCupManager.CrossOS_Target='openbsd') then
    begin
      if (MessageDlg('Be forwarned: this will only work with FPC>=3.0.2 (trunk, NewPascal, fixes).' + sLineBreak +
                 'See: http://bugs.freepascal.org/view.php?id=30908' + sLineBreak +
                 'Do you want to continue ?'
                 ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                 begin
                   Memo1.Lines.Append('See: http://bugs.freepascal.org/view.php?id=30908');
                   exit;
                 end;
    end;
    {$endif}

    if (FPCupManager.CrossCPU_Target='aarch64')
    {$ifdef MSWINDOWS}OR (FPCupManager.CrossOS_Target='darwin'){$endif}
    OR (FPCupManager.CrossOS_Target='msdos')
    then
    begin
      if (MessageDlg('Be forwarned: this will only work with FPC trunk (or NewPascal).' + sLineBreak +
                     'Do you want to continue ?'
                     ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                     begin
                       exit;
                     end;
    end;

    if (FPCupManager.CrossOS_Target='linux') then
    begin
      ShowMessage('Be forwarned: you may need to add some extra linking when cross-compiling.' + sLineBreak + CrossGCCMsg);
      Memo1.Lines.Append('Be forwarned: you may need to add some extra linking when cross-compiling.' + sLineBreak + CrossGCCMsg);
      Memo1.Lines.Append('');
    end;
  end;

  DisEnable(Sender,False);

  try

    //arm predefined settings
    if (FPCupManager.CrossCPU_Target='arm') AND (FPCupManager.CrossOS_Target<>'embedded') then
    begin
      // default: armhf
      // don't worry: this -dFPC_ARMHF option will still build a normal ppcrossarm for all targets
      // adding this option will allow ppcrossarm compiler to generate ARMHF for Linux
      // but I stand corrected if this assumption is wrong
      FPCupManager.FPCOPT:='-dFPC_ARMHF ';

      if (FPCupManager.CrossOS_Target='wince') then
      begin
        FPCupManager.CrossOPT:='-CpARMV6 ';
      end
      else
      if (FPCupManager.CrossOS_Target='darwin') then
      begin
        FPCupManager.CrossOPT:='-CpARMV7 -CfVFPV3 ';
      end
      else
      begin
        // Use hard floats, using armeabi-v7a Android ABI.
        // Note: do not use -CaEABIHF on Android, to not use
        // armeabi-v7a-hard ABI. Reasons:
        // - armeabi-v7a-hard ABI is not adviced anymore by Google,
        //   see "ARM Hard Float ABI Removal" on
        //   https://android.googlesource.com/platform/ndk/+/353e653824b79c43b948429870d0abeedebde386/docs/HardFloatAbi.md
        // - it prevents calling functions from libraries not using
        //   armeabi-v7a-hard ABI (but only using armeabi-v7a) like
        //   http://repo.or.cz/openal-soft/android.git or
        //   https://github.com/michaliskambi/tremolo-android .
        if (FPCupManager.CrossOS_Target='android')
            then FPCupManager.CrossOPT:='-CpARMV7A -CfVFPV3 ' //-CfVFPV
            else FPCupManager.CrossOPT:='-CpARMV7A -CfVFPV3 -OoFASTMATH -CaEABIHF ';
      end;
    end;

    //darwin predefined settings
    if (FPCupManager.CrossOS_Target='darwin') then
    begin
      if (FPCupManager.CrossCPU_Target='aarch64') OR (FPCupManager.CrossCPU_Target='arm') then
      begin
        if (FPCupManager.CrossCPU_Target='aarch64') then FPCupManager.CrossOPT:='-CaAARCH64IOS ';
        {$ifdef Darwin}
        FPCupManager.CrossOPT:='-WP'+GetSDKVersion('iphoneos')+' '+FPCupManager.CrossOPT;
        {$endif}
      end;
      if (FPCupManager.CrossCPU_Target='i386') OR (FPCupManager.CrossCPU_Target='x86_64') OR (FPCupManager.CrossCPU_Target='powerpc') OR (FPCupManager.CrossCPU_Target='powerpc64') then
      begin
        {$ifdef Darwin}
        FPCupManager.CrossOPT:='-WM'+GetSDKVersion('macosx')+' '+FPCupManager.CrossOPT;
        {$endif}
      end;
    end;

    //iphonesim i386+x86_64 predefined settings
    if (FPCupManager.CrossOS_Target='iphonesim') then
    begin
      if (FPCupManager.CrossCPU_Target='i386') OR (FPCupManager.CrossCPU_Target='x86_64') then
      begin
        {$ifdef Darwin}
        FPCupManager.CrossOPT:='-WP'+GetSDKVersion('iphonesimulator')+' '+FPCupManager.CrossOPT;
        {$endif}
      end;
    end;

    //embedded predefined settings
    if (FPCupManager.CrossOS_Target='embedded') then
    begin
      if (FPCupManager.CrossCPU_Target='avr') then
      begin
        FPCupManager.FPCOPT:='-O2 ';
        // for Uno (ATMega328P) use avr5
        // for Mega (ATMega2560) use avr6
        FPCupManager.CrossOPT:='-Cpavr5 ';
        FPCupManager.CrossOS_SubArch:='avr5';
      end;
      if (FPCupManager.CrossCPU_Target='arm') then
      begin
        // don't worry: this -dFPC_ARMHF option will still build a normal ppcrossarm (embedded) for Embedded
        // adding this option will allow ppcrossarm compiler to generate ARMHF for Linux
        FPCupManager.FPCOPT:='-dFPC_ARMHF ';

        FPCupManager.CrossOPT:='-CpARMV7A -CfVFPV3 -OoFASTMATH -CaEABIHF ';
        FPCupManager.CrossOS_SubArch:='armv7m';
      end;
      if (FPCupManager.CrossCPU_Target='mipsel') then
      begin
        //FPCupManager.CrossOPT:='-Cppic32 ';
        FPCupManager.CrossOPT:='-Cpmips32 -Wppic32mx110f016b';
        FPCupManager.CrossOS_SubArch:='pic32mx';
      end;
    end;

    //msdos predefined settings
    if (FPCupManager.CrossOS_Target='msdos') then
    begin
      if (FPCupManager.CrossCPU_Target='i8086') then
      begin
        {$IFDEF DARWIN}
        FPCupManager.CrossOPT:='-WmLarge ';
        {$ELSE}
        FPCupManager.CrossOPT:='-WmMedium ';
        {$ENDIF DARWIN}
      end;
    end;

    // recheck / override / set custom FPC options by special user input through setup+
    s:=Form2.FPCOptions;
    s:=Trim(s);
    if Length(s)>0 then FPCupManager.FPCOPT:=s+' ';

    // override / set custom FPC crossoptions by special user input through setup+
    s:=Form2.GetCrossBuildOptions(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
    s:=Trim(s);
    if Length(s)>0 then FPCupManager.CrossOPT:=s+' ';

    // override / set custom FPC cross-subarch by special user input through setup+
    s:=Form2.GetCrossSubArch(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
    s:=Trim(s);
    if Length(s)>0 then FPCupManager.CrossOS_SubArch:=s;

    // use the available source to build the cross-compiler ... change nothing about source and url !!
    FPCupManager.OnlyModules:='FPCCleanOnly,FPCBuildOnly';

    IncludeLCL:=Form2.IncludeLCL;
    if (FPCupManager.CrossOS_Target='java') then IncludeLCL:=false;
    if (FPCupManager.CrossOS_Target='android') then IncludeLCL:=false;
    if (FPCupManager.CrossOS_Target='embedded') then IncludeLCL:=false;
    // AFAIK, on Darwin, LCL Carbon and Cocoa are only supported for i386 and x86_64 ... but again I stand corrected if wrong
    if (FPCupManager.CrossOS_Target='darwin') AND (FPCupManager.CrossCPU_Target<>'x86_64') AND (FPCupManager.CrossCPU_Target<>'i386') then IncludeLCL:=false;

    if IncludeLCL then
    begin
      FPCupManager.OnlyModules:=FPCupManager.OnlyModules+',LCL';
      // if Darwin x64, only cocoa (but also qt5) will work.
      if ((FPCupManager.CrossOS_Target='darwin') AND (FPCupManager.CrossCPU_Target='x86_64'))
          then FPCupManager.CrossLCL_Platform:='cocoa';
    end
    else
    begin
      if Form2.IncludeLCL then AddMessage('Skipping build of LCL for this target: not supported (yet).');
    end;

    FPCupManager.FPCURL:='skip';
    FPCupManager.LazarusURL:='skip';

    FPCupManager.CrossLibraryDirectory:=Form2.GetLibraryDirectory(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
    FPCupManager.CrossToolsDirectory:=Form2.GetToolsDirectory(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);

    AddMessage('Going to install a cross-compiler from available sources.');

    sStatus:='Building compiler for '+FPCupManager.CrossOS_Target+'-'+FPCupManager.CrossCPU_Target;
    if FPCupManager.FPCOPT<>'' then sStatus:=sStatus+' (OPT: '+FPCupManager.FPCOPT+')';
    if FPCupManager.CrossOPT<>'' then sStatus:=sStatus+' [CROSSOPT: '+FPCupManager.CrossOPT+']';
    if FPCupManager.CrossOS_SubArch<>'' then sStatus:=sStatus+' {SUBARCH: '+FPCupManager.CrossOS_SubArch+'}';
    sStatus:=sStatus+'.';

    AddMessage(sStatus);
    Memo1.Lines.Append(sStatus);

    success:=RealRun;

    if (Sender<>nil) AND (NOT success) then
    begin

      // perhaps there were no libraries and/or binutils ... download them (if available) from fpcup on GitHub

      if MissingCrossBins OR MissingCrossLibs then
      begin

        if (MessageDlg('The building of a crosscompiler failed due to missing cross-tools.' + sLineBreak +
                   'Fpcupdeluxe can try to download them if available !' + sLineBreak +
                   'Do you want to continue ?'
                   ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                   begin
                     exit;
                   end;

        BinsURL:='';

        AddMessage('Looking for fpcupdeluxe cross-tools on GitHub (if any).');

        if FPCupManager.CrossOS_Target='linux' then
        begin
          {$ifdef Darwin}
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='LinuxARM.zip';
          {$else}
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='LinuxARM.rar';
          {$endif}
          if FPCupManager.CrossCPU_Target='aarch64' then BinsURL:='LinuxAarch64.rar';
          {$ifdef Darwin}
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='Linuxx64.zip';
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='Linuxi386.zip';
          {$else}
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='Linuxx64.rar';
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='Linuxi386.rar';
          {$endif}
          if FPCupManager.CrossCPU_Target='powerpc' then BinsURL:='LinuxPowerPC.rar';
          if FPCupManager.CrossCPU_Target='powerpc64' then BinsURL:='LinuxPowerPC64.rar';
          if FPCupManager.CrossCPU_Target='mips' then BinsURL:='LinuxMips.zip';
          if FPCupManager.CrossCPU_Target='mipsel' then BinsURL:='LinuxMipsel.zip';
        end;
        if FPCupManager.CrossOS_Target='freebsd' then
        begin
          {$ifdef FreeBSD}
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='FreeBSDi386.zip';
          {$else}
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='FreeBSDi386.rar';
          {$endif}
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='FreeBSDx64.rar';
        end;
        if FPCupManager.CrossOS_Target='openbsd' then
        begin
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='OpenBSDi386.rar';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='OpenBSDx64.rar';
        end;
        if FPCupManager.CrossOS_Target='solaris' then
        begin
          if FPCupManager.CrossCPU_Target='sparc' then BinsURL:='SolarisSparc.rar';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='Solarisx64.rar';
        end;
        if FPCupManager.CrossOS_Target='wince' then
        begin
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='WinceARM.rar';
        end;
        if FPCupManager.CrossOS_Target='android' then
        begin
         {$ifdef Darwin}
         if FPCupManager.CrossCPU_Target='arm' then BinsURL:='AndroidARM.zip';
         {$else}
         if FPCupManager.CrossCPU_Target='arm' then BinsURL:='AndroidARM.rar';
         {$endif}
          if FPCupManager.CrossCPU_Target='aarch64' then BinsURL:='AndroidAArch64.rar';
          if FPCupManager.CrossCPU_Target='mipsel' then BinsURL:='AndroidMipsel.zip';
        end;
        if FPCupManager.CrossOS_Target='embedded' then
        begin
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='EmbeddedARM.rar';
          if FPCupManager.CrossCPU_Target='avr' then BinsURL:='EmbeddedAVR.rar';
          if FPCupManager.CrossCPU_Target='mipsel' then BinsURL:='EmbeddedMipsel.rar';
        end;
        if FPCupManager.CrossOS_Target='darwin' then
        begin
          {$ifdef MSWindows}
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='Darwinx86.zip';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='Darwinx86.zip';
          {$else}
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='Darwinx86.rar';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='Darwinx86.rar';
          {$endif}
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='DarwinARM.rar';
          if FPCupManager.CrossCPU_Target='aarch64' then BinsURL:='DarwinAArch64.rar';
        end;

        if FPCupManager.CrossOS_Target='msdos' then
        begin
          if FPCupManager.CrossCPU_Target='i8086' then BinsURL:='MSDosi8086.zip';
        end;

        // normally, we have the same names for libs and bins URL
        LibsURL:=BinsURL;

        // normally, we have the standard names for libs and bins paths
        LibPath:=DirectorySeparator+'lib'+DirectorySeparator+FPCupManager.CrossCPU_Target+'-'+FPCupManager.CrossOS_Target;
        BinPath:=DirectorySeparator+'bin'+DirectorySeparator+FPCupManager.CrossCPU_Target+'-'+FPCupManager.CrossOS_Target;

        if FPCupManager.CrossOS_Target='darwin' then
        begin
          // Darwin is special: combined binaries and libs for i386 and x86_64 with osxcross
          if (FPCupManager.CrossCPU_Target='i386') OR (FPCupManager.CrossCPU_Target='x86_64') then
          begin
            BinPath:=StringReplace(BinPath,FPCupManager.CrossCPU_Target,'x86',[rfIgnoreCase]);
            LibPath:=StringReplace(LibPath,FPCupManager.CrossCPU_Target,'x86',[rfIgnoreCase]);
          end;
          // Darwin is special: combined libs for arm and aarch64 with osxcross
          if (FPCupManager.CrossCPU_Target='arm') OR (FPCupManager.CrossCPU_Target='aarch64') then
          begin
            LibPath:=StringReplace(LibPath,FPCupManager.CrossCPU_Target,'arm',[rfIgnoreCase]);
            LibsURL:=StringReplace(LibsURL,'AArch64','ARM',[rfIgnoreCase]);
          end;
        end;

        // bit tricky ... reset BinsURL in case the binutils and libs are already there ... to exit this retry ... ;-)
        if (
           (NOT DirectoryIsEmpty(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+BinPath))
           AND
           (NOT DirectoryIsEmpty(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath))
           )
          then BinsURL:='';

        if BinsURL<>'' then
        begin

          // many files to unpack for Darwin : do not show progress of unpacking files when unpacking for Darwin.
          verbose:=(FPCupManager.CrossOS_Target<>'darwin');
          UseNativeUnzip:=false;

          if MissingCrossBins then
          begin

            // no cross-bins available
            if (Length(FPCUPBINSURL)=0) then
            begin
              ShowMessage('No tools available online. You could do a feature request ... ;-)');
              exit;
            end;

            success:=false;
            AddMessage('Going to download the right cross-bins. Can (will) take some time !',True);

            {$ifdef MSWINDOWS}
            DownloadURL:=FPCUPBINSURL+'/'+'WinCrossBins'+BinsURL;
            {$else}
            DownloadURL:=FPCUPBINSURL+'/'+'CrossBins'+BinsURL;
            {$endif}
            TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
            SysUtils.DeleteFile(TargetFile);
            UseNativeUnzip:=(ExtractFileExt(TargetFile)='.zip');
            {$ifdef Darwin}
            if (UseNativeUnzip) then
            {$endif}
            begin
              AddMessage('Please wait: Going to download the binary-tools from '+DownloadURL);
              success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            end;

            // if rar then try zip ... if zip then try rar .... very dirty and certainly not elegant ... ;-)
            if (NOT success) then
            begin
              if (ExtractFileExt(DownloadURL)='.zip')
                 then DownloadURL:=ChangeFileExt(DownloadURL,'.rar')
                 else DownloadURL:=ChangeFileExt(DownloadURL,'.zip');
              SysUtils.DeleteFile(TargetFile);
              TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
              SysUtils.DeleteFile(TargetFile);
              UseNativeUnzip:=(ExtractFileExt(TargetFile)='.zip');
              {$ifdef Darwin}
              if (UseNativeUnzip) then
              {$endif}
              begin
                AddMessage('Please wait: Going to download the binary-tools from '+DownloadURL);
                success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
              end;
            end;

            if success then
            begin
              AddMessage('Successfully downloaded binary-tools archive.');
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir);
              {$ifndef MSWINDOWS}
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir)+'cross'+BinPath+DirectorySeparator;
              {$endif}
              if (NOT DirectoryExists(TargetPath)) then ForceDirectories(TargetPath);

              AddMessage('Going to extract archive into '+TargetPath);

              if UseNativeUnzip then
              begin
                with TNormalUnzipper.Create do
                begin
                  try
                    success:=DoUnZip(TargetFile,TargetPath,[]);
                  finally
                    Free;
                  end;
                end;
              end
              else
              begin
                {$ifdef MSWINDOWS}
                if (not verbose) then AddMessage('Please wait: going to unpack binary tools archive.');
                success:=(ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+TargetFile+' "'+TargetPath+'"',verbose)=0);
                if (NOT success) then
                {$endif}
                begin
                  {$ifdef MSWINDOWS}
                  UnZipper := IncludeTrailingPathDelimiter(FPCupManager.MakeDirectory) + 'unrar\bin\unrar.exe';
                  {$else}
                  UnZipper := 'unrar';
                  {$endif}
                  success:=CheckExecutable(UnZipper, '-v', '');
                  if success then
                  begin
                    if (not verbose) then AddMessage('Please wait: going to unpack binary tools archive.');
                    success:=(ExecuteCommand(UnZipper + ' x "' + TargetFile + '" "' + TargetPath + '"',verbose)=0);
                  end else AddMessage('Error: '+UnZipper+' not found on system. Cannot unpack cross-tools !');
                end;
              end;

              if success then
              begin
                {$IFDEF UNIX}
                fileList:=FindAllFiles(TargetPath);
                try
                  if (fileList.Count > 0) then
                  begin
                    for i:=0 to Pred(fileList.Count) do
                    begin
                      fpChmod(fileList.Strings[i],&755);
                    end;
                  end;
                finally
                  fileList.Free;
                end;
                {$ENDIF}
              end;
            end;
            SysUtils.DeleteFile(TargetFile);
          end;

          // force the download of embedded libs if not there ... if this fails, don't worry, building will go on
          if (DirectoryIsEmpty(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath)) AND (FPCupManager.CrossOS_Target='embedded')
            then MissingCrossLibs:=true;

          if MissingCrossLibs then
          begin
            AddMessage('Going to download the right cross-libs. Can (will) take some time !',True);
            DownloadURL:=FPCUPLIBSURL+'/'+'CrossLibs'+LibsURL;
            TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
            SysUtils.DeleteFile(TargetFile);
            UseNativeUnzip:=(ExtractFileExt(TargetFile)='.zip');
            success:=false;
            {$ifdef Darwin}
            if (UseNativeUnzip) then
            {$endif}
            begin
              AddMessage('Please wait: Going to download the libraries from '+DownloadURL);
              success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            end;
            // if rar then try zip ... if zip then try rar .... very dirty and certainly not elegant ... ;-)
            if (NOT success) then
            begin
              if (ExtractFileExt(DownloadURL)='.zip')
                 then DownloadURL:=ChangeFileExt(DownloadURL,'.rar')
                 else DownloadURL:=ChangeFileExt(DownloadURL,'.zip');
              SysUtils.DeleteFile(TargetFile);
              TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
              SysUtils.DeleteFile(TargetFile);
              UseNativeUnzip:=(ExtractFileExt(TargetFile)='.zip');
              {$ifdef Darwin}
              if (UseNativeUnzip) then
              {$endif}
              begin
                AddMessage('Please wait: Going to download the libraries from '+DownloadURL);
                success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
              end;
            end;

            if success then
            begin
              AddMessage('Successfully downloaded the libraries.');
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir);
              //TargetPath:=IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath+DirectorySeparator;
              //if (NOT DirectoryExists(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath)) then ForceDirectories(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath);

              AddMessage('Going to extract them into '+TargetPath);

              if UseNativeUnzip then
              begin
                with TNormalUnzipper.Create do
                begin
                  try
                    success:=DoUnZip(TargetFile,TargetPath,[]);
                  finally
                    Free;
                  end;
                end;
              end
              else
              begin
                {$ifdef MSWINDOWS}
                if (not verbose) then AddMessage('Please wait: going to unpack library files archive.');
                success:=(ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+TargetFile+' "'+TargetPath+'"',verbose)=0);
                if (NOT success) then
                {$endif}
                begin
                  {$ifdef MSWINDOWS}
                  UnZipper := IncludeTrailingPathDelimiter(FPCupManager.MakeDirectory) + 'unrar\bin\unrar.exe';
                  {$else}
                  UnZipper := 'unrar';
                  {$endif}
                  success:=CheckExecutable(UnZipper, '-v', '');
                  if success then
                  begin
                    if (not verbose) then AddMessage('Please wait: going to unpack library files archive.');
                    success:=(ExecuteCommand(UnZipper + ' x "' + TargetFile + '" "' + TargetPath + '"',verbose)=0);
                  end else AddMessage('Error: '+UnZipper+' not found on system. Cannot unpack cross-tools !');
                end;
              end;
            end;
            SysUtils.DeleteFile(TargetFile);
            // as libraries are not needed for embedded, always end with success even if the above has failed
            if FPCupManager.CrossOS_Target='embedded' then success:=true;
          end;

          if success then
          begin
            if CheckAutoClear.Checked then Memo1.Clear;
            AddMessage('Successfully extracted cross-tools.');
            // run again with the correct libs and binutils
            FPCVersionLabel.Font.Color:=clDefault;
            LazarusVersionLabel.Font.Color:=clDefault;
            AddMessage('Got all tools now. New try building a cross-compiler for '+FPCupManager.CrossOS_Target+'-'+FPCupManager.CrossCPU_Target,True);
            if (FPCupManager.CrossOS_Target='linux') then
            begin
              Memo1.Lines.Append('Be forwarned: you may need to add some extra linking when cross-compiling.' + sLineBreak + CrossGCCMsg);
              Memo1.Lines.Append('');
            end;
            FPCupManager.Sequencer.ResetAllExecuted;
            RealRun;
          end;

          if (NOT success) then AddMessage('No luck in getting then cross-tools ... aborting.');
        end;
      end
      else
      begin
        AddMessage('Building cross-tools failed ... ??? ... aborting.');
      end;

    end;

  finally
    DisEnable(Sender,True);
  end;

  result:=success;

end;

procedure TForm1.FPCOnlyClick(Sender: TObject);
begin
  if (ListBoxFPCTarget.ItemIndex=-1) then
  begin
    ShowMessage('Please select a FPC version first');
    exit;
  end;
  DisEnable(Sender,False);
  try
    PrepareRun;

    if Form2.UpdateOnly
       then FPCupManager.OnlyModules:='FPCCleanAndBuildOnly'
       {$ifdef win32}
       else FPCupManager.OnlyModules:='fpc,FPCCrossWin32-64';
       {$else}
       else FPCupManager.OnlyModules:='fpc';
       {$endif}

    FPCupManager.LazarusURL:='skip';

    if NOT Form2.IncludeHelp then
      FPCupManager.SkipModules:=FPCupManager.SkipModules+'helpfpc';

    sStatus:='Going to install/update FPC only.';

    RealRun;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.LazarusOnlyClick(Sender: TObject);
begin
  if (ListBoxLazarusTarget.ItemIndex=-1) then
  begin
    ShowMessage('Please select a Lazarus version first');
    exit;
  end;

  DisEnable(Sender,False);

  try
    PrepareRun;

    if Form2.UpdateOnly then
    begin
      FPCupManager.OnlyModules:='LazCleanAndBuildOnly';
    end
    else
    begin
      {$ifdef win32}
      FPCupManager.OnlyModules:='lazarus,LazarusCrossWin32-64';
      {$else}
      FPCupManager.OnlyModules:='lazarus';
      {$endif}
    end;

    FPCupManager.FPCURL:='skip';

    if (NOT Form2.IncludeHelp) then
      FPCupManager.SkipModules:=FPCupManager.SkipModules+'helplazarus'
    else
      FPCupManager.IncludeModules:=FPCupManager.IncludeModules+'lhelp';

    sStatus:='Going to install/update Lazarus only.';

    RealRun;
  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  Form2.ShowModal;
  if Form2.ModalResult=mrOk then
  begin
    FPCupManager.ExportOnly:=(NOT Form2.Repo);
    FPCupManager.HTTPProxyHost:=Form2.HTTPProxyHost;
    FPCupManager.HTTPProxyPort:=Form2.HTTPProxyPort;
    FPCupManager.HTTPProxyUser:=Form2.HTTPProxyUser;
    FPCupManager.HTTPProxyPassword:=Form2.HTTPProxyPass;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  SynEdit1.Clear;
  Memo1.Clear;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  sInstallDir:=InstallDirEdit.Text;
  GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir));
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  sInstallDir:=InstallDirEdit.Text;
  GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir));
end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // set last used install directory
  if (NOT Assigned(FPCupManager)) then exit;

  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    WriteString('General','InstallDirectory',sInstallDir);

    WriteBool('General','AutoClear',CheckAutoClear.Checked);

    Application.MainForm.Cursor:=crHourGlass;

    WriteString('ProxySettings','HTTPProxyURL',FPCupManager.HTTPProxyHost);
    WriteInteger('ProxySettings','HTTPProxyPort',FPCupManager.HTTPProxyPort);
    WriteString('ProxySettings','HTTPProxyUser',FPCupManager.HTTPProxyUser);
    WriteString('ProxySettings','HTTPProxyPass',FPCupManager.HTTPProxyPassword);

    WriteInteger('General','CommandFontSize',SynEdit1.Font.Size);

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

  // localize FPCUPSettings if possible
  if (NOT SetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir)))
     then SetFPCUPSettings(SafeGetApplicationPath);

  CloseAction:=caFree;
end;

procedure TForm1.DisEnable(Sender: TObject;value:boolean);
begin
  //if Sender<>BitBtnFPCandLazarus then
  BitBtnFPCandLazarus.Enabled:=value;
  //if Sender<>Button2 then
  Button2.Enabled:=value;
  Button3.Enabled:=value;
  ButtonInstallCrossCompiler.Enabled:=value;
  BitBtnFPCOnly.Enabled:=value;
  BitBtnLazarusOnly.Enabled:=value;
  Button7.Enabled:=value;
  Button8.Enabled:=value;
  AutoCrossUpdate.Enabled:=value;

  ListBoxFPCTarget.Enabled:=value;
  ListBoxLazarusTarget.Enabled:=value;
  ListBox3.Enabled:=value;

  InstallDirEdit.Enabled:=value;
  RadioGroup1.Enabled:=value;
  RadioGroup2.Enabled:=value;
  CheckAutoClear.Enabled:=value;

  TrunkBtn.Enabled:=value;
  NPBtn.Enabled:=value;
  FixesBtn.Enabled:=value;
  StableBtn.Enabled:=value;
  OldBtn.Enabled:=value;
  DinoBtn.Enabled:=value;
  FeaturesBtn.Enabled:=value;
  mORMotBtn.Enabled:=value;

  if Sender=nil then BitBtnHalt.Enabled:=value;;
end;

procedure TForm1.PrepareRun;
begin
  FPCVersionLabel.Font.Color:=clDefault;
  LazarusVersionLabel.Font.Color:=clDefault;

  if CheckAutoClear.Checked then Button8.Click;

  FPCupManager.Sequencer.ResetAllExecuted;

  MissingCrossBins:=false;
  MissingCrossLibs:=false;

  {$ifdef win64}
  FPCupManager.NoJobs:=true;
  {$else}
  FPCupManager.NoJobs:=false;
  {$endif}

  FPCupManager.OnlyModules:='';
  FPCupManager.IncludeModules:='';
  FPCupManager.SkipModules:='';
  FPCupManager.CrossCPU_Target:='';
  FPCupManager.CrossOS_Target:='';
  FPCupManager.CrossOS_SubArch:='';
  FPCupManager.CrossLCL_Platform:='';

  FPCupManager.FPCOPT:=Form2.FPCOptions;;
  FPCupManager.CrossOPT:='';

  FPCupManager.CrossLibraryDirectory:='';
  FPCupManager.CrossToolsDirectory:='';

  {$IFDEF DEBUG}
  FPCupManager.Verbose:=True;
  SetVerbosity(True);
  {$ELSE}
  FPCupManager.Verbose:=True;
  SetVerbosity((Form2.ExtraVerbose) AND (FPCupManager.Verbose));
  {$ENDIF}

  FPCupManager.FPCDesiredBranch:=Form2.FPCBranch;
  FPCupManager.FPCDesiredRevision:=Form2.FPCRevision;

  FPCupManager.LazarusOPT:=Form2.LazarusOptions;
  FPCupManager.LazarusDesiredBranch:=Form2.LazarusBranch;
  FPCupManager.LazarusDesiredRevision:=Form2.LazarusRevision;

  FPCupManager.UseWget:=Form2.UseWget;

  FPCupManager.SwitchURL:=Form2.AutoSwitchURL;

  // set default values for FPC and Lazarus URL ... can still be changed inside the real run button onclicks
  FPCupManager.FPCURL:=FPCTarget;
  if (Pos('freepascal.git',lowercase(FPCupManager.FPCURL))>0) then
  begin
    FPCupManager.FPCDesiredBranch:='release';
    // use NewPascal git mirror for trunk sources
    // set branch to get latest freepascal
    if FPCTarget='trunkgit'
       then FPCupManager.FPCDesiredBranch:='freepascal';
  end;

  FPCupManager.LazarusURL:=LazarusTarget;
  if (Pos('lazarus.git',lowercase(FPCupManager.LazarusURL))>0) then
  begin
    FPCupManager.LazarusDesiredBranch:='release';
    // use NewPascal git mirror for trunk sources
    // set branch to get latest lazarus
    if LazarusTarget='trunkgit'
       then FPCupManager.LazarusDesiredBranch:='lazarus';
  end;

  sInstallDir:=ExcludeTrailingPathDelimiter(sInstallDir);
  FPCupManager.BaseDirectory:=sInstallDir;

  // do not create shortcut for fpcupeluxe itself: we have already fpcupdeluxe by itself !!
  //FPCupManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update';
  FPCupManager.ShortCutNameFpcup:=EmptyStr;

  FPCupManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir);

  sInstallDir:=sInstallDir+DirectorySeparator;

  {$IFDEF MSWINDOWS}
  FPCupManager.MakeDirectory:=sInstallDir+'fpcbootstrap';
  {$ELSE}
  FPCupManager.MakeDirectory:='';
  {$ENDIF MSWINDOWS}
  FPCupManager.BootstrapCompilerDirectory:=sInstallDir+'fpcbootstrap';

  FPCupManager.FPCInstallDirectory:=sInstallDir+'fpc';
  if Form2.SplitFPC
     then FPCupManager.FPCSourceDirectory:=FPCupManager.FPCInstallDirectory+'src'
     else FPCupManager.FPCSourceDirectory:=FPCupManager.FPCInstallDirectory;

  FPCupManager.LazarusDirectory:=sInstallDir+'lazarus';

  {
  // not yet
  if Form2.SplitLazarus
     then FPCupManager.LazarusSourceDirectory:=FPCupManager.LazarusInstallDirectory+'src'
     else FPCupManager.LazarusSourceDirectory:=FPCupManager.LazarusInstallDirectory;
  }

  FPCupManager.LazarusPrimaryConfigPath:=sInstallDir+'config_'+ExtractFileName(FPCupManager.LazarusDirectory);

  FPCupManager.ExportOnly:=(NOT Form2.CheckRepo.Checked);

  FPCupManager.FPCPatches:=Form2.FPCPatches;
  FPCupManager.LazarusPatches:=Form2.LazPatches;

  {$ifdef Darwin}
    {$ifdef LCLCOCOA}
      FPCupManager.CrossLCL_Platform:='cocoa';
    {$else}
      {$ifdef CPUX64}
        {$ifdef LCLQT5}
          FPCupManager.CrossLCL_Platform:='qt5';
        {$else}
          FPCupManager.CrossLCL_Platform:='cocoa';
        {$endif}
      {$endif}
    {$endif}
  {$endif}

  RealFPCURL.Text:='';
  RealLazURL.Text:='';

  sStatus:='Sitting and waiting';
  StatusMessage.Text:=sStatus;

  if CheckAutoClear.Checked then Memo1.Lines.Clear;

  AddMessage(Self.Caption);
  AddMessage('');

end;

function TForm1.RealRun:boolean;
begin
  result:=false;

  StatusMessage.Text:=sStatus;

  AddMessage('FPCUP(deluxe) is starting up.');
  AddMessage('');
  {$IFDEF MSWINDOWS}
  AddMessage('Binutils/make dir:  '+FPCupManager.MakeDirectory);
  {$ENDIF MSWINDOWS}
  AddMessage('Bootstrap dir:      '+FPCupManager.BootstrapCompilerDirectory);

  {$IF (defined(BSD)) and (not defined(Darwin))}
  FPCupManager.FPCOpt:=FPCupManager.FPCOpt+' -Fl/usr/local/lib';
  FPCupManager.LazarusOpt:=FPCupManager.LazarusOpt+' -Fl/usr/local/lib -Fl/usr/X11R6/lib';
  {$endif}

  if FPCupManager.FPCURL<>'SKIP' then
  begin
    AddMessage('FPC URL:               '+FPCupManager.FPCURL);
    AddMessage('FPC options:           '+FPCupManager.FPCOPT);
    AddMessage('FPC source directory:  '+FPCupManager.FPCSourceDirectory);
    AddMessage('FPC install directory: '+FPCupManager.FPCInstallDirectory);
    RealFPCURL.Text:=FPCupManager.FPCURL;
  end else RealFPCURL.Text:='Skipping FPC';

  if FPCupManager.LazarusURL<>'SKIP' then
  begin
    AddMessage('Lazarus URL:        '+FPCupManager.LazarusURL);
    AddMessage('Lazarus options:    '+FPCupManager.LazarusOPT);
    AddMessage('Lazarus directory:  '+FPCupManager.LazarusDirectory);
    RealLazURL.Text:=FPCupManager.LazarusURL;
  end else RealLazURL.Text:='Skipping Lazarus';

  AddMessage('Please stand back and enjoy !');
  AddMessage('');

  Application.ProcessMessages;

  sleep(1000);

  try
    result:=FPCupManager.Run;
    if (NOT result) then
    begin
      AddMessage('');
      AddMessage('');
      if MissingCrossBins then AddMessage('Fpcupdeluxe failed due to missing cross binary tools.')
      else if MissingCrossLibs then AddMessage('Fpcupdeluxe failed due to missing cross libraries.')
      else
      begin
        AddMessage('ERROR: Fpcupdeluxe fatal error !');
        StatusMessage.Text:='Hmmm, something went wrong ... have a good look at the command screen !';
      end;
      FPCVersionLabel.Font.Color:=clRed;
      LazarusVersionLabel.Font.Color:=clRed;
    end
    else
    begin
      AddMessage('');
      AddMessage('');
      AddMessage('SUCCESS: Fpcupdeluxe ended without errors.');
      AddMessage('');
      if (FPCupManager.LazarusURL<>'SKIP') then
      begin
        {$ifdef MSWINDOWS}
        AddMessage('Fpcupdeluxe has created a desktop shortcut to start Lazarus.');
        AddMessage('Shortcut-name: '+FPCupManager.ShortCutNameLazarus);
        AddMessage('Lazarus by fpcupdeluxe MUST be started with this shortcut !!');
        {$else}
        AddMessage('Fpcupdeluxe has created a shortcut link in your home-directory to start Lazarus.');
        AddMessage('Shortcut-link: '+FPCupManager.ShortCutNameLazarus);
        AddMessage('Lazarus MUST be started with this link !!');
        AddMessage('Fpcupdeluxe has also (tried to) create a desktop shortcut with the same name.');
        {$endif}
        AddMessage('');
      end;
      FPCVersionLabel.Font.Color:=clLime;
      LazarusVersionLabel.Font.Color:=clLime;
      StatusMessage.Text:='That went well !!!';
    end;
  except
    // just swallow exceptions
    StatusMessage.Text:='Got an unexpected exception ... don''t know what to do unfortunately.';
  end;
end;

function TForm1.GetFPCUPSettings(IniDirectory:string):boolean;
var
  i,j:integer;
  SortedModules:TStringList;
begin
  result:=FileExists(IniDirectory+DELUXEFILENAME);

  SynEdit1.Clear;
  AddMessage('Welcome @ FPCUPdeluxe.');
  AddMessage(Self.Caption);
  AddMessage('');

  if result then with TIniFile.Create(IniDirectory+DELUXEFILENAME) do
  try

    AddMessage('Got settings from install directory');
    AddMessage('');

    // get names of cross-compilers
    AutoUpdateCrossCompiler(nil);

    FPCupManager.ExportOnly:=(NOT ReadBool('General','GetRepo',True));

    FPCTarget:=ReadString('URL','fpcURL','stable');
    if FPCTarget='' then FPCTarget:='stable';
    LazarusTarget:=ReadString('URL','lazURL','stable');
    if LazarusTarget='' then LazarusTarget:='stable';

    Form2.FPCOptions:=ReadString('General','FPCOptions','');
    Form2.LazarusOptions:=ReadString('General','LazarusOptions','');
    Form2.FPCRevision:=ReadString('General','FPCRevision','');
    Form2.LazarusRevision:=ReadString('General','LazarusRevision','');

    Form2.SplitFPC:=ReadBool('General','SplitFPC',True);
    Form2.SplitLazarus:=ReadBool('General','SplitLazarus',False);

    //FPCupManager.UseWget:=ReadBool('General','UseWget',False);
    //Form2.UseWget:=FPCupManager.UseWget;
    {$ifdef Darwin}
    Form2.UseWget:=False;
    {$else}
      {$ifdef MSWINDOWS}
      Form2.UseWget:=False;
      {$else}
        {$ifdef OpenBSD}
        Form2.UseWget:=True;
        {$else}
        Form2.UseWget:=ReadBool('General','UseWget',False);
        {$endif}
      {$endif}
    {$endif}

    Form2.ExtraVerbose:=ReadBool('General','ExtraVerbose',False);

    Form2.FPCPatches:=ReadString('Patches','FPCPatches','');
    Form2.LazPatches:=ReadString('Patches','LazarusPatches','');

    Form2.AutoSwitchURL:=ReadBool('General','AutoSwitchURL',True);

    listbox3.ClearSelection;
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

  finally
    Free;
  end;

end;

function TForm1.SetFPCUPSettings(IniDirectory:string):boolean;
var
  i:integer;
  modules:string;
begin
  result:=DirectoryExists(ExtractFileDir(IniDirectory+DELUXEFILENAME));

  if not result then exit;

  try
    with TIniFile.Create(IniDirectory+DELUXEFILENAME) do
    try
      // mmm, is this correct ?  See extrasettings !!
      WriteBool('General','GetRepo',(NOT FPCupManager.ExportOnly));

      if FPCTarget<>'skip' then WriteString('URL','fpcURL',FPCTarget);
      if LazarusTarget<>'skip' then WriteString('URL','lazURL',LazarusTarget);

      WriteString('General','FPCOptions',Form2.FPCOptions);
      WriteString('General','LazarusOptions',Form2.LazarusOptions);
      WriteString('General','FPCRevision',Form2.FPCRevision);
      WriteString('General','LazarusRevision',Form2.LazarusRevision);

      WriteBool('General','SplitFPC',Form2.SplitFPC);
      WriteBool('General','SplitLazarus',Form2.SplitLazarus);

      WriteBool('General','UseWget',Form2.UseWget);
      WriteBool('General','ExtraVerbose',Form2.ExtraVerbose);

      WriteString('Patches','FPCPatches',Form2.FPCPatches);
      WriteString('Patches','LazarusPatches',Form2.LazPatches);

      WriteBool('General','AutoSwitchURL',Form2.AutoSwitchURL);

      modules:='';
      for i:=0 to ListBox3.Count-1 do
      begin
        if ListBox3.Selected[i] then modules:=modules+ListBox3.Items[i]+',';
      end;
      // delete stale trailing comma, if any
      if Length(modules)>0 then
      Delete(modules,Length(modules),1);
      WriteString('General','Modules',modules);

    finally
      Free;
    end;

    result:=FileExists(IniDirectory+DELUXEFILENAME);

  except
    on E: Exception do
    begin
      //infoln(DELUXEFILENAME+': File creation error: '+E.Message,etError);
    end;
  end;

end;

procedure TForm1.AddMessage(aMessage:string; UpdateStatus:boolean=false);
begin
  SynEdit1.Append(aMessage);
  SynEdit1.CaretX:=0;
  SynEdit1.CaretY:=SynEdit1.Lines.Count;
  if UpdateStatus then StatusMessage.Text:=aMessage;
  Application.ProcessMessages;
end;

procedure TForm1.SetFPCTarget(aFPCTarget:string);
var
  i:integer;
begin
  if aFPCTarget<>FFPCTarget then
  begin
    FFPCTarget:=aFPCTarget;
    i:=ListBoxFPCTarget.Items.IndexOf(FFPCTarget);
    if i<>-1 then
    begin
      ListBoxFPCTarget.Selected[i]:=true;
      ListBoxFPCTarget.Invalidate;
    end;
  end;
end;

procedure TForm1.SetLazarusTarget(aLazarusTarget:string);
var
  i:integer;
begin
  if aLazarusTarget<>FLazarusTarget then
  begin
    FLazarusTarget:=aLazarusTarget;
    i:=ListBoxLazarusTarget.Items.IndexOf(FLazarusTarget);
    if i<>-1 then
    begin
      ListBoxLazarusTarget.Selected[i]:=true;
      ListBoxLazarusTarget.Invalidate;
    end;
  end;
end;

end.

