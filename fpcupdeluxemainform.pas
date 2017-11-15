unit fpcupdeluxemainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Types, Buttons, Menus, SynEdit, SynEditPopup, SynEditMiscClasses,
  SynGutterCodeFolding, installerManager{$ifdef usealternateui},alternateui{$endif}
  {$ifdef RemoteLog}
  ,mormotdatamodelclient
  {$endif}
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnHalt: TBitBtn;
    BitBtnFPCandLazarus: TBitBtn;
    BitBtnFPCOnly: TBitBtn;
    BitBtnLazarusOnly: TBitBtn;
    AutoCrossUpdate: TButton;
    btnSetupPlus: TButton;
    btnClearLog: TButton;
    CheckAutoClear: TCheckBox;
    MainMenu1: TMainMenu;
    memoSummary: TMemo;
    MenuItem1: TMenuItem;
    StatusMessage: TEdit;
    Timer1: TTimer;
    TrunkBtn: TBitBtn;
    NPBtn: TBitBtn;
    FixesBtn: TBitBtn;
    StableBtn: TBitBtn;
    OldBtn: TBitBtn;
    DinoBtn: TBitBtn;
    mORMotBtn: TBitBtn;
    btnInstallModule: TButton;
    btnInstallDirSelect: TButton;
    ButtonInstallCrossCompiler: TButton;
    InstallDirEdit: TEdit;
    Panel1: TPanel;
    RealFPCURL: TEdit;
    FPCVersionLabel: TLabel;
    LazarusVersionLabel: TLabel;
    ListBoxFPCTarget: TListBox;
    ListBoxLazarusTarget: TListBox;
    listModules: TListBox;
    radgrpCPU: TRadioGroup;
    radgrpOS: TRadioGroup;
    RealLazURL: TEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SynEdit1: TSynEdit;
    procedure BitBtnHaltClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LazarusOnlyClick(Sender: TObject);
    procedure BitBtnFPCandLazarusClick(Sender: TObject);
    procedure btnInstallModuleClick(Sender: TObject);
    procedure btnInstallDirSelectClick(Sender: TObject);
    procedure ButtonInstallCrossCompilerClick(Sender: TObject);
    procedure FPCOnlyClick(Sender: TObject);
    procedure btnSetupPlusClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
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
    procedure radgrpCPUClick(Sender: TObject);
    procedure radgrpOSClick(Sender: TObject);
    procedure SynEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure QuickBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    {$ifdef usealternateui}
    procedure alternateuibutClick(Sender: TObject);
    procedure alternateuibutEnter(Sender: TObject);
    procedure alternateuibutLeave(Sender: TObject);
    {$endif}
  private
    { private declarations }
    FPCupManager:TFPCupManager;
    oldoutput: TextFile;
    sInstallDir:string;
    sStatus:string;
    FFPCTarget,FLazarusTarget:string;
    MissingCrossBins:boolean;
    MissingCrossLibs:boolean;
    MissingTools:boolean;
    InternalError:string;
    {$ifdef RemoteLog}
    aDataClient:TDataClient;
    {$endif}
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
    procedure AddMessage(const aMessage:string; const UpdateStatus:boolean=false);
    procedure InitFPCupManager;
    {$ifdef usealternateui}
    {$else}
    property FPCTarget:string read FFPCTarget write SetFPCTarget;
    property LazarusTarget:string read FLazarusTarget write SetLazarusTarget;
    {$endif}

  public
    { public declarations }
    {$ifdef usealternateui}
    property FPCTarget:string read FFPCTarget write SetFPCTarget;
    property LazarusTarget:string read FLazarusTarget write SetLazarusTarget;
    {$endif}
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  IniFiles,
  strutils,
  LCLType, // for MessageBox
  {$ifdef UNIX}
  baseunix,
  {$endif UNIX}
  //LazFileUtils,
  AboutFrm,
  extrasettings,
  //checkoptions,
  installerCore,
  installerUniversal,
  m_crossinstaller, // for checking of availability of fpc[laz]up[deluxe] cross-compilers
  fpcuputil,
  processutils,
  synedittext;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  IniFilesOk:boolean;
  aTarget:string;
begin
  FPCupManager:=nil;

  {$ifdef RemoteLog}
  aDataClient:=TDataClient.Create;
  {$ifdef usealternateui}
  aDataClient.UpInfo.UpVersion:=DELUXEVERSION+'+';
  {$else}
  aDataClient.UpInfo.UpVersion:=DELUXEVERSION;
  {$endif}
  aDataClient.UpInfo.UpOS:=GetTargetCPUOS;
  {$endif}

  {$IF defined(CPUAARCH64) OR defined(CPUARM) OR defined(Haiku)}
  // disable some features
  FixesBtn.Visible:=False;
  StableBtn.Visible:=False;
  OldBtn.Visible:=False;
  DinoBtn.Visible:=False;
  ButtonInstallCrossCompiler.Visible:=False;
  {$endif}

  AutoCrossUpdate.Visible:=ButtonInstallCrossCompiler.Visible;
  radgrpCPU.Visible:=ButtonInstallCrossCompiler.Visible;
  radgrpOS.Visible:=ButtonInstallCrossCompiler.Visible;

  if (NOT AutoCrossUpdate.Visible) then
  begin
    listModules.BorderSpacing.Top:=0;
    listModules.AnchorSideTop.Control:=FPCVersionLabel;
  end;

  {$ifdef Darwin}
  radgrpOS.Items.Strings[radgrpOS.Items.IndexOf('wince')]:='i-sim';
  {$endif Darwin}

  oldoutput := System.Output;
  AssignSynEdit(System.Output, SynEdit1);
  Reset(System.Input);
  Rewrite(System.Output);

  aTarget:=''
  {$ifdef LCLWin32}
  +'win32'
  {$endif}
  {$ifdef LCLWin64}
  +'win64'
  {$endif}
  {$ifdef LCLGtk}
  +'gtk'
  {$endif}
  {$ifdef LCLGtk2}
  +'gtk2'
  {$endif}
  {$ifdef LCLCARBON}
  +'carbon'
  {$endif}
  {$ifdef LCLCOCOA}
  +'cocoa'
  {$endif}
  {$ifdef LCLQT5}
  +'qt5'
  {$endif}
  {$ifdef LCLQT}
  +'qt'
  {$endif}
  ;

  {$ifdef RemoteLog}
  aDataClient.UpInfo.UpWidget:=aTarget;
  {$endif}

  Self.Caption:=
  {$ifdef usealternateui}
  'FPCUPdeluxery V'+
  {$else}
  'FPCUPdeluxe V'+
  {$endif}
    DELUXEVERSION+
    ' for ' +
    GetTargetCPUOS+
    '-'+
    aTarget;

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

  aTarget:='';
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

    // tricky ... due to arm quircks when cross-compiling : GetDistro (ExecuteCommand) gives errors if used in CreateForm
    Timer1.Enabled:=True;
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
  {$ifdef RemoteLog}
  FreeAndNil(aDataClient);
  {$endif}
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
  {$ifdef usealternateui}
  alternateui_resize;
  {$endif}
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
    if CheckAutoClearStore then btnClearLog.Click;
    CheckAutoClear.Checked:=false;

    memoSummary.Lines.Append('Going to auto-build all installed cross-compilers !');
    memoSummary.Lines.Append('Checking FPC configfile [fpc.cfg] for cross-compilers in ' + BinPath);
    memoSummary.Lines.Append('');

  end
  else
  begin
    memoSummary.Clear;
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

        if (aOS='windows') or (aOS='win32') or (aOS='win64') then
        begin
          if aCPU='i386' then aOS:='win32';
          if aCPU='x86_64' then aOS:='win64';
          aRadiogroup_OS:='windows';
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
            memoSummary.Lines.Append('Crosscompiler for '+aCPU + '-' + aOS+' found !');
            memoSummary.Lines.Append('Going to update cross-compiler.');
            radgrpCPU.ItemIndex:=radgrpCPU.Items.IndexOf(aRadiogroup_CPU);
            radgrpOS.ItemIndex:=radgrpOS.Items.IndexOf(aRadiogroup_OS);
            success:=InstallCrossCompiler(nil);
            if success
              then memoSummary.Lines.Append('Cross-compiler update ok.')
              else memoSummary.Lines.Append('Failure during update of cross-compiler !!');
            memoSummary.Lines.Append('');
          end;
        end;
      end;
    end;

    if (Sender<>nil) then
    begin
      radgrpCPU.ItemIndex:=-1;
      radgrpOS.ItemIndex:=-1;
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

  if listModules.Count=0 then
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
      listModules.Items.AddStrings(SortedModules);

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

procedure TForm1.radgrpCPUClick(Sender: TObject);
begin
  if (radgrpCPU.ItemIndex<>-1) then
  begin
  if (radgrpCPU.Items[radgrpCPU.ItemIndex]='i8086') then
    begin
      radgrpOS.ItemIndex:=-1;
      radgrpOS.Enabled:=false;
    end
    else radgrpOS.Enabled:=true;
  end
end;

procedure TForm1.radgrpOSClick(Sender: TObject);
begin
  if (radgrpOS.ItemIndex<>-1) then
  begin
    if (radgrpOS.Items[radgrpOS.ItemIndex]='java') OR (radgrpOS.Items[radgrpOS.ItemIndex]='msdos') then
    begin
      radgrpCPU.ItemIndex:=-1;
      radgrpCPU.Enabled:=false;
    end else radgrpCPU.Enabled:=true;
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
      memoSummary.Lines.Append('Getting/updating '+InternalError);
    end;
  end;

  if (ExistWordInString(PChar(s),'checkout',[soWholeWord,soDown])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord,soDown])) then
  begin
    memoSummary.Lines.Append('Performing a SVN/GIT checkout ... please wait, could take some time.');
  end;

  if (ExistWordInString(PChar(s),'switch',[soWholeWord,soDown])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord,soDown])) then
  begin
    memoSummary.Lines.Append('Performing a SVN repo URL switch ... please wait, could take some time.');
  end;

  // github error
  if (ExistWordInString(PChar(s),'429 too many requests',[soDown])) then
  begin
    memoSummary.Lines.Append('GitHub blocked us due to too many download requests.');
    memoSummary.Lines.Append('This will last for an hour, so please wait and be patient.');
    memoSummary.Lines.Append('After this period, please re-run fpcupdeluxe.');
  end;

  (*
  searchstring:='the makefile doesn''t support target';
  if (ExistWordInString(PChar(s),searchstring,[soDown])) then
  begin
    memoSummary.Lines.Append('Sorry, but you have chosen a target that is not supported (yet).');
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
        memoSummary.Lines.Append('Wrong target: '+InternalError);
      end;
    end;
  end;
  *)

  searchstring:='unable to connect to a repository at url';
  if (ExistWordInString(PChar(s),searchstring,[soDown])) then
  begin
    memoSummary.Lines.Append('SVN could not connect to the desired repository.');
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      InternalError:=Copy(s,x+1,MaxInt);
      memoSummary.Lines.Append('URL: '+InternalError);
      memoSummary.Lines.Append('Please check your connection. Or run the SVN command to try yourself:');
      memoSummary.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end;
  end;

  if (ExistWordInString(PChar(s),'error:',[soWholeWord,soDown])) OR (ExistWordInString(PChar(s),'fatal:',[soWholeWord,soDown])) then
  begin
    memoSummary.Lines.Append(BeginSnippet+' Start of compile error summary.');

    if (ExistWordInString(PChar(s),'fatal: internal error',[soDown])) then
    begin
      x:=RPos(' ',s);
      if x>0 then
      begin
        InternalError:=Copy(s,x+1,MaxInt);
        memoSummary.Lines.Append('Compiler error: '+InternalError);
        if (InternalError='2015030501') OR (InternalError='2014051001') OR (InternalError='2014050604') then
        begin
          memoSummary.Lines.Append('FPC revision 30351 introduced some changed into the compiler causing this error.');
          memoSummary.Lines.Append('Has something todo about how floating points are handled. And that has changed.');
          memoSummary.Lines.Append('See: http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=30351');
        end;

        if (InternalError='2013051401') then
        begin
          memoSummary.Lines.Append('FPC revision 37182 breaks cross building avr-embedded.');
          memoSummary.Lines.Append('However, this has been solved in the meantime !');
          memoSummary.Lines.Append('Please update FPC trunk !!');
          //memoSummary.Lines.Append('See: https://bugs.freepascal.org/view.php?id=32418');
          //memoSummary.Lines.Append('See: https://bugs.freepascal.org/view.php?id=31925');
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
        memoSummary.Lines.Append('Configuration error: '+InternalError);
        x:=Pos('80 bit extended floating point',LowerCase(s));
        if x>0 then
        begin
          memoSummary.Lines.Append('Please use trunk that has 80-bit float type using soft float unit.');
          memoSummary.Lines.Append('FPC revisions 37294 - 37306 add this soft float feature.');
          memoSummary.Lines.Append('So update your FPC trunk to a revision > 37306 !!');
          //memoSummary.Lines.Append('See: http://bugs.freepascal.org/view.php?id=29892');
          //memoSummary.Lines.Append('See: http://bugs.freepascal.org/view.php?id=9262');
        end;
      end;
    end
    else if (ExistWordInString(PChar(s),'failed to get crossbinutils',[soDown])) then
    begin
      MissingCrossBins:=true;
      memoSummary.Lines.Append('Missing correct cross binary utilities');
    end
    else if (ExistWordInString(PChar(s),'failed to get crosslibrary',[soDown])) then
    begin
      MissingCrossLibs:=true;
      memoSummary.Lines.Append('Missing correct cross libraries');
    end
    else if ((ExistWordInString(PChar(s),'CheckAndGetTools',[soDown])) OR (ExistWordInString(PChar(s),'Required package is not installed',[soDown]))) then
    begin
      MissingTools:=true;
      memoSummary.Lines.Append('Missing some tools: please install !');
    end
    else if (Pos('error: 256',lowercase(s))>0) AND (Pos('svn',lowercase(s))>0) then
    begin
      memoSummary.Lines.Append('We have had a SVN connection failure. Just start again !');
      memoSummary.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end
    else if (ExistWordInString(PChar(s),'fatal:',[soDown])) then
    begin
      memoSummary.Lines.Append(s);
      memoSummary.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end
    else if (ExistWordInString(PChar(s),'error:',[soDown])) then
    begin
      // check if "error:" at the end of the line.
      // if so:
      // the real error will follow on the next line(s).
      // and we have to wait for these lines (done somewhere else in this procedure) !!
      // if not, just print the error message.
      if (Pos('error:',lowercase(s))<>(Length(s)-Length('error:')+1)) then memoSummary.Lines.Append(s);
    end;
  end;

  // linker error
  if (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find',[soDown])) then
  begin
    x:=Pos('-l',s);
    if x>0 then
    begin
      // add help into summary memo
      memoSummary.Lines.Append(BeginSnippet+' Missing library: lib'+Copy(s,x+2,MaxInt));
    end;
  end;

  // diskspace errors
  if (ExistWordInString(PChar(s),'Stream write error',[soDown])) OR (ExistWordInString(PChar(s),'disk full',[soDown])) then
  begin
    memoSummary.Lines.Append(BeginSnippet+' There is not enough diskspace to finish this operation.');
    memoSummary.Lines.Append(BeginSnippet+' Please free some space and re-run fpcupdeluxe.');
  end;

  // RAM errors
  if (ExistWordInString(PChar(s),'Can''t call the assembler',[soDown])) then
  begin
    memoSummary.Lines.Append(BeginSnippet+' Most likely, there is not enough RAM (swap) to finish this operation.');
    memoSummary.Lines.Append(BeginSnippet+' Please add some swap-space (1GB) and re-run fpcupdeluxe.');
  end;

  // warn for time consuming help files
  if (ExistWordInString(PChar(s),'writing',[soDown])) AND (ExistWordInString(PChar(s),'pages...',[soDown])) then
  begin
    memoSummary.Lines.Append('Busy with help files. Be patient: can be time consuming !!');
  end;

  // report about correct tools that are found and used
  if (ExistWordInString(PChar(s),'found correct',[soDown])) then
  begin
    memoSummary.Lines.Append(s);
  end;

  if ExistWordInString(PChar(s),BeginSnippet,[soWholeWord,soDown]) then
  begin
    if ExistWordInString(PChar(s),Seriousness[etWarning],[soWholeWord,soDown]) then
    begin
      // repeat fpcupdeluxe warning
      memoSummary.Lines.Append(s);
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
      memoSummary.Lines.Append(BeginSnippet+' Start of special error summary.');
      memoSummary.Lines.Append(SynEdit1.Lines[x]);
      memoSummary.Lines.Append(SynEdit1.Lines[x+1]);
      memoSummary.Lines.Append(SynEdit1.Lines[x+2]);
    end;
  end;
  {$ifdef usealternateui}
  alternateui_AddMessage(s);
  {$endif}

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

    {$ifdef RemoteLog}
    aDataClient.UpInfo.UpFunction:=ufInstallFPCLAZ;
    {$endif}

    RealRun;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  TTimer(Sender).Enabled:=false;
  // only run once !!
  // FPC cross-quirck : GetDistro (ExecuteCommand) gives errors if used in CreateForm
  {$ifdef RemoteLog}
  aDataClient.UpInfo.UpDistro:=GetDistro;
  {$endif}
  InitFPCupManager;
  {$ifdef usealternateui}
  // This must only be called once.
  If Not Alternate_ui_created then alternateui_Create_Controls;
  {$endif}
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

    {$ifdef RemoteLog}
    aDataClient.UpInfo.UpFunction:=ufInstallFPCLAZ;
    {$endif}

    RealRun;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.btnInstallModuleClick(Sender: TObject);
var
  i:integer;
  modules:string;
begin
  DisEnable(Sender,False);
  try

    if listModules.SelCount=0 then
    begin
      AddMessage('Please select a module / package.');
      exit;
    end;

    PrepareRun;

    FPCupManager.ExportOnly:=(NOT Form2.CheckPackageRepo.Checked);

    modules:='';
    for i:=0 to listModules.Count-1 do
    begin
      if listModules.Selected[i] then modules:=modules+listModules.Items[i]+',';
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
      {$ifdef RemoteLog}
      aDataClient.UpInfo.UpFunction:=ufInstallModule;
      aDataClient.AddExtraData('module',modules);
      {$endif}
      RealRun;
    end;
  finally
    FPCupManager.ExportOnly:=(NOT Form2.CheckRepo.Checked);
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.btnInstallDirSelectClick(Sender: TObject);
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
  IncludeLCL,ZipFile:boolean;
  i:integer;
  {$ifdef Unix}
  fileList: TStringList;
  {$endif}
begin

  result:=false;

  if (radgrpCPU.ItemIndex=-1) and (radgrpOS.ItemIndex=-1) then
  begin
    ShowMessage('Please select a CPU and OS target first');
    exit;
  end;

  // the below three checks are just temporary and rough
  // we should use the array OSCPUSupported : array[TOS,TCpu] of boolean

  success:=true;
  if radgrpOS.ItemIndex<>-1 then
  begin
    s:=radgrpOS.Items[radgrpOS.ItemIndex];
    if s='embedded' then
    begin
      if radgrpCPU.ItemIndex<>-1 then
      begin
        s:=radgrpCPU.Items[radgrpCPU.ItemIndex];
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
  if radgrpOS.ItemIndex<>-1 then
  begin
    s:=radgrpOS.Items[radgrpOS.ItemIndex];
    if s='android' then
    begin
      if radgrpCPU.ItemIndex<>-1 then
      begin
        s:=radgrpCPU.Items[radgrpCPU.ItemIndex];
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
  if radgrpCPU.ItemIndex<>-1 then
  begin
    s:=radgrpCPU.Items[radgrpCPU.ItemIndex];
    if s='jvm' then
    begin
      if radgrpOS.ItemIndex<>-1 then
      begin
        s:=radgrpOS.Items[radgrpOS.ItemIndex];
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

  if radgrpCPU.ItemIndex<>-1 then
  begin
    s:=radgrpCPU.Items[radgrpCPU.ItemIndex];
    if s='ppc' then s:='powerpc';
    if s='ppc64' then s:='powerpc64';
    if s='x8664' then s:='x86_64';
    FPCupManager.CrossCPU_Target:=s;
  end;
  if radgrpOS.ItemIndex<>-1 then
  begin
    s:=radgrpOS.Items[radgrpOS.ItemIndex];
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

  if (FPCupManager.CrossCPU_Target='') then
  begin
    if Sender<>nil then Application.MessageBox(PChar('Please select a CPU target first.'), PChar('CPU error'), MB_ICONERROR);
    FPCupManager.CrossOS_Target:=''; // cleanup
    exit;
  end;

  if (FPCupManager.CrossOS_Target='') then
  begin
    if Sender<>nil then Application.MessageBox(PChar('Please select an OS target first.'), PChar('OS error'), MB_ICONERROR);
    FPCupManager.CrossCPU_Target:=''; // cleanup
    exit;
  end;

  if (NOT FPCupManager.CheckValidCPUOS) then
  begin
    if Sender<>nil then
    begin
      Application.MessageBox(PChar('FPC source: No valid CPU / OS target.'), PChar('Configuration error'), MB_ICONERROR);
    end
    else
    begin
      memoSummary.Lines.Append('');
      memoSummary.Lines.Append('FPC source: No valid CPU / OS target. Skipping');
    end;
    FPCupManager.CrossOS_Target:=''; // cleanup
    FPCupManager.CrossCPU_Target:=''; // cleanup
    exit;
  end;

  if assigned(CrossInstallers) then
  begin
    success:=false;
    for i := 0 to CrossInstallers.Count - 1 do
    begin
      success:=(CrossInstallers[i] = GetFPCTargetCPUOS(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,false));
      if success then break;
    end;
    if (NOT success) then
    begin
      if Sender<>nil then
      begin
        Application.MessageBox(PChar('No valid CPU / OS crosscompiler found.'), PChar('FPCUPDELUXE Limitation'), MB_ICONERROR);
      end
      else
      begin
        memoSummary.Lines.Append('');
        memoSummary.Lines.Append('FPCUPDELUXE Limitation: No valid CPU / OS crosscompiler found. Skipping');
        memoSummary.Lines.Append('FPCUPDELUXE Limitation: You could do a feature request !');
      end;
      FPCupManager.CrossOS_Target:=''; // cleanup
      FPCupManager.CrossCPU_Target:=''; // cleanup
      exit;
    end;
  end;

  if Sender<>nil then
  begin
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
        memoSummary.Lines.Append('');
        memoSummary.Lines.Append('To get clang: sudo apt-get install clang');
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
        memoSummary.Lines.Append('');
        memoSummary.Lines.Append('To get java: sudo apt-get install default-jre');
        {$endif}
        exit;
      end;
    end;

    {$ifndef BSD}
    if (Pos('bsd',FPCupManager.CrossOS_Target)>0) then
    //if (FPCupManager.CrossOS_Target='freebsd') OR (FPCupManager.CrossOS_Target='netbsd') OR (FPCupManager.CrossOS_Target='openbsd') then
    begin
      if (MessageDlg('Be forwarned: this will only work with FPC>=3.0.2 (trunk, NewPascal, fixes, stable).' + sLineBreak +
                 'See: http://bugs.freepascal.org/view.php?id=30908' + sLineBreak +
                 'Do you want to continue ?'
                 ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                 begin
                   memoSummary.Lines.Append('See: http://bugs.freepascal.org/view.php?id=30908');
                   exit;
                 end;
    end;
    {$endif}

    if (FPCupManager.CrossCPU_Target='aarch64')
    {$ifdef MSWINDOWS}OR (FPCupManager.CrossOS_Target='darwin'){$endif}
    OR (FPCupManager.CrossOS_Target='msdos')
    OR (FPCupManager.CrossOS_Target='haiku')
    then
    begin
      if (MessageDlg('Be forwarned: this will only work with FPC trunk (or NewPascal).' + sLineBreak +
                     'Do you want to continue ?'
                     ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                     begin
                       exit;
                     end;
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
    // AFAIK, on Darwin, LCL Carbon and Cocoa are only for MACOSX
    if (FPCupManager.CrossOS_Target='darwin') AND ((FPCupManager.CrossCPU_Target='arm') OR (FPCupManager.CrossCPU_Target='aarch64')) then IncludeLCL:=false;

    if IncludeLCL then
    begin
      FPCupManager.OnlyModules:=FPCupManager.OnlyModules+',LCL';
      // if Darwin cpu64, only cocoa (but also qt5) will work.
      if ((FPCupManager.CrossOS_Target='darwin') AND ((FPCupManager.CrossCPU_Target='x86_64') OR (FPCupManager.CrossCPU_Target='powerpc64')))
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
    if FPCupManager.FPCOPT<>'' then
    begin
      sStatus:=sStatus+' (OPT: '+FPCupManager.FPCOPT+')';
      {$ifdef RemoteLog}
      aDataClient.AddExtraData('OPT',FPCupManager.FPCOPT);
      {$endif}
    end;
    if FPCupManager.CrossOPT<>'' then
    begin
      sStatus:=sStatus+' [CROSSOPT: '+FPCupManager.CrossOPT+']';
      {$ifdef RemoteLog}
      aDataClient.AddExtraData('CROSSOPT',FPCupManager.CrossOPT);
      {$endif}
    end;
    if FPCupManager.CrossOS_SubArch<>'' then
    begin
      sStatus:=sStatus+' {SUBARCH: '+FPCupManager.CrossOS_SubArch+'}';
      {$ifdef RemoteLog}
      aDataClient.AddExtraData('SUBARCH',FPCupManager.CrossOS_SubArch);
      {$endif}
    end;
    sStatus:=sStatus+'.';

    AddMessage(sStatus);
    memoSummary.Lines.Append(sStatus);

    {$ifdef RemoteLog}
    aDataClient.UpInfo.UpFunction:=ufInstallCross;
    aDataClient.UpInfo.CrossCPUOS:=FPCupManager.CrossOS_Target+'-'+FPCupManager.CrossCPU_Target;
    if length(FPCupManager.CrossLCL_Platform)>0 then aDataClient.AddExtraData('CrossLCL',FPCupManager.CrossLCL_Platform);
    if length(FPCupManager.OnlyModules)>0 then aDataClient.AddExtraData('Only',FPCupManager.OnlyModules);
    if length(FPCupManager.SkipModules)>0 then aDataClient.AddExtraData('Skip',FPCupManager.SkipModules);
    {$endif}

    success:=RealRun;

    if {(Sender<>nil) AND} (NOT success) then
    begin

      // perhaps there were no libraries and/or binutils ... download them (if available) from fpcup on GitHub

      if MissingCrossBins OR MissingCrossLibs then
      begin

        if (Sender<>nil) then
        begin
          if (MessageDlg('The building of a crosscompiler failed due to missing cross-tools.' + sLineBreak +
                   'Fpcupdeluxe can try to download them if available !' + sLineBreak +
                   'Do you want to continue ?'
                   ,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
                   begin
                     exit;
                   end;
        end;

        BinsURL:='';

        AddMessage('Looking for fpcupdeluxe cross-tools on GitHub (if any).');

        if FPCupManager.CrossCPU_Target='arm' then BinsURL:='ARM';
        if FPCupManager.CrossCPU_Target='aarch64' then BinsURL:='Aarch64';
        if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='x64';
        if FPCupManager.CrossCPU_Target='i386' then BinsURL:='i386';
        if FPCupManager.CrossCPU_Target='powerpc' then BinsURL:='PowerPC';
        if FPCupManager.CrossCPU_Target='powerpc64' then BinsURL:='PowerPC64';
        if FPCupManager.CrossCPU_Target='mips' then BinsURL:='Mips';
        if FPCupManager.CrossCPU_Target='mipsel' then BinsURL:='Mipsel';
        if FPCupManager.CrossCPU_Target='sparc' then BinsURL:='Sparc';
        if FPCupManager.CrossCPU_Target='avr' then BinsURL:='AVR';
        if FPCupManager.CrossCPU_Target='i8086' then BinsURL:='i8086';

        if FPCupManager.CrossOS_Target='darwin' then
        begin
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='x86';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='x86';
          if FPCupManager.CrossCPU_Target='powerpc' then BinsURL:='powerpc';
          if FPCupManager.CrossCPU_Target='powerpc64' then BinsURL:='powerpc';
        end;

        if FPCupManager.CrossOS_Target='freebsd' then BinsURL:='FreeBSD'+BinsURL else
          if FPCupManager.CrossOS_Target='openbsd' then BinsURL:='OpenBSD'+BinsURL else
            if FPCupManager.CrossOS_Target='msdos' then BinsURL:='MSDos'+BinsURL else
              BinsURL:=UppercaseFirstChar(FPCupManager.CrossOS_Target)+BinsURL;

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
          if (FPCupManager.CrossCPU_Target='powerpc') OR (FPCupManager.CrossCPU_Target='powerpc64') then
          begin
            BinPath:=StringReplace(BinPath,FPCupManager.CrossCPU_Target,'powerpc',[rfIgnoreCase]);
            LibPath:=StringReplace(LibPath,FPCupManager.CrossCPU_Target,'powerpc',[rfIgnoreCase]);
          end;

          // Darwin is special: combined libs for arm and aarch64 with osxcross
          if (FPCupManager.CrossCPU_Target='arm') OR (FPCupManager.CrossCPU_Target='aarch64') then
          begin
            LibPath:=StringReplace(LibPath,FPCupManager.CrossCPU_Target,'arm',[rfIgnoreCase]);
            LibsURL:=StringReplace(LibsURL,'Aarch64','ARM',[rfIgnoreCase]);
          end;
        end;

        if FPCupManager.CrossOS_Target='linux' then
        begin
          // PowerPC64 is special: only little endian libs for now
          if (FPCupManager.CrossCPU_Target='powerpc64') then
          begin
            LibsURL:=StringReplace(LibsURL,'PowerPC64','PowerPC64LE',[rfIgnoreCase]);
          end;
        end;


        // bit tricky ... if bins and libs are already there exit this retry ... ;-)
        if (
           (DirectoryIsEmpty(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+BinPath))
           OR
           (DirectoryIsEmpty(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath))
           )
        then
        begin

          // many files to unpack for Darwin : do not show progress of unpacking files when unpacking for Darwin.
          verbose:=(FPCupManager.CrossOS_Target<>'darwin');

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
            {$endif MSWINDOWS}

            //default to zip
            DownloadURL:=DownloadURL+'.zip';
            TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
            SysUtils.DeleteFile(TargetFile);
            AddMessage('Please wait: Going to download the zip binary-tools from '+DownloadURL);
            success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            ZipFile:=success;

            {$ifndef Darwin}
            // try rar .... very dirty and certainly not elegant ... ;-)
            if (NOT success) then
            begin
              DownloadURL:=ChangeFileExt(DownloadURL,'.rar');
              SysUtils.DeleteFile(TargetFile);
              TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
              SysUtils.DeleteFile(TargetFile);
              AddMessage('Please wait: Going to download the rar binary-tools from '+DownloadURL);
              success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            end;
            {$endif}

            if success then
            begin
              AddMessage('Successfully downloaded binary-tools archive.');
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir);
              {$ifndef MSWINDOWS}
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir)+'cross'+BinPath+DirectorySeparator;
              {$endif}
              if (NOT DirectoryExists(TargetPath)) then ForceDirectories(TargetPath);

              AddMessage('Going to extract archive into '+TargetPath);

              if ZipFile then
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

            // default to zip
            DownloadURL:=DownloadURL+'.zip';

            TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
            SysUtils.DeleteFile(TargetFile);
            success:=false;
            AddMessage('Please wait: Going to download the libraries from '+DownloadURL);
            success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            ZipFile:=success;

            {$ifndef Darwin}
            // if rar then try zip ... if zip then try rar .... very dirty and certainly not elegant ... ;-)
            if (NOT success) then
            begin
              DownloadURL:=ChangeFileExt(DownloadURL,'.rar');
              SysUtils.DeleteFile(TargetFile);
              TargetFile := SysUtils.GetTempDir+GetFileNameFromURL(DownloadURL);
              SysUtils.DeleteFile(TargetFile);
              AddMessage('Please wait: Going to download the libraries from '+DownloadURL);
              success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            end;
            {$endif}

            if success then
            begin
              AddMessage('Successfully downloaded the libraries.');
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir);
              //TargetPath:=IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath+DirectorySeparator;
              //if (NOT DirectoryExists(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath)) then ForceDirectories(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath);

              AddMessage('Going to extract them into '+TargetPath);

              if ZipFile then
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
            if CheckAutoClear.Checked then memoSummary.Clear;
            AddMessage('Successfully extracted cross-tools.');
            // run again with the correct libs and binutils
            FPCVersionLabel.Font.Color:=clDefault;
            LazarusVersionLabel.Font.Color:=clDefault;
            AddMessage('Got all tools now. New try building a cross-compiler for '+FPCupManager.CrossOS_Target+'-'+FPCupManager.CrossCPU_Target,True);
            if Assigned(FPCupManager.Sequencer) then FPCupManager.Sequencer.ResetAllExecuted;
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

    {$ifdef RemoteLog}
    aDataClient.UpInfo.UpFunction:=ufInstallFPC;
    {$endif}

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

    {$ifdef RemoteLog}
    aDataClient.UpInfo.UpFunction:=ufInstallLAZ;
    {$endif}

    RealRun;
  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.btnSetupPlusClick(Sender: TObject);
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

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  SynEdit1.Clear;
  memoSummary.Clear;
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

{$ifdef usealternateui}
procedure TForm1.alternateuibutClick(Sender: TObject);
begin
  alteranteui_ClickHandler(Sender);
end;
procedure TForm1.alternateuibutEnter(Sender: TObject);
begin
  alteranteui_EnterHandler(Sender);
end;
procedure TForm1.alternateuibutLeave(Sender: TObject);
begin
  alteranteui_LeaveHandler(Sender);
end;
{$endif}

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

  CloseAction:=caFree;
end;

procedure TForm1.DisEnable(Sender: TObject;value:boolean);
var
  c: TControl;
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if (NOT (Components[i] is TControl)) then continue;
    c := Components[i] AS TControl;
    if c is TLabel then continue;
    if c is TPanel then continue;
    if c is TGroupBox then continue;
    if c = BitBtnHalt then continue;
    if c = SynEdit1 then continue;
    if c = memoSummary then continue;
    c.Enabled := value;
    {$ifdef usealternateui}
    if ((pos('Halt',c.name)>0) or (pos('Halt',c.caption)>0)) then c.Enabled:=true;
    {$endif}
  end;
  {$ifdef usealternateui}
  if value then alternateui_make_sure_images_on_buttons_are_not_enabled;
  {$endif}
end;

procedure TForm1.PrepareRun;
begin
  FPCVersionLabel.Font.Color:=clDefault;
  LazarusVersionLabel.Font.Color:=clDefault;

  if CheckAutoClear.Checked then btnClearLog.Click;

  FPCupManager.Sequencer.ResetAllExecuted;

  MissingCrossBins:=false;
  MissingCrossLibs:=false;
  MissingTools:=false;

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
      {$ifdef CPU64}
        {$ifdef LCLQT5}
          FPCupManager.CrossLCL_Platform:='qt5';
        {$else}
          FPCupManager.CrossLCL_Platform:='cocoa';
        {$endif}
      {$endif}
    {$endif}
  {$endif}

  {$ifdef Haiku}
    {$ifdef LCLQT}
      FPCupManager.CrossLCL_Platform:='qt';
    {$endif}
    {$ifdef LCLQT5}
      FPCupManager.CrossLCL_Platform:='qt5';
    {$endif}
  {$endif}

  RealFPCURL.Text:='';
  RealLazURL.Text:='';

  {$ifdef RemoteLog}
  aDataClient.Enabled:=Form2.SendInfo;
  aDataClient.UpInfo.UpFunction:=ufUnknown;
  aDataClient.ClearExtraData;
  aDataClient.UpInfo.CrossCPUOS:='';
  aDataClient.UpInfo.LogEntry:='';
  {$endif}

  sStatus:='Sitting and waiting';
  StatusMessage.Text:=sStatus;

  if CheckAutoClear.Checked then memoSummary.Lines.Clear;

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

  //create install directory
  if (NOT DirectoryExists(FPCupManager.BaseDirectory)) then ForceDirectories(FPCupManager.BaseDirectory);
  //save install settings in install directory
  SetFPCUPSettings(IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory));

  Application.ProcessMessages;

  {$ifdef RemoteLog}
  aDataClient.UpInfo.FPCVersion:=FPCTarget;
  aDataClient.UpInfo.LazarusVersion:=LazarusTarget;
  aDataClient.UpInfo.UpInstallDir:=FPCupManager.BaseDirectory;
  {$endif}

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
        // skip reporting trivial errors about missing things
        if (NOT MissingTools) then
        begin
          StatusMessage.Text:='Hmmm, something went wrong ... have a good look at the command screen !';
          {$ifdef RemoteLog}
          aDataClient.UpInfo.LogEntry:=memoSummary.Text;
          aDataClient.SendData;
          {$endif}
        end
        else
        begin
          StatusMessage.Text:='Something went wrong due to missing system tools or dev-libs !';
        end;
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

      {$ifdef RemoteLog}
      aDataClient.UpInfo.LogEntry:='Success !';
      aDataClient.SendData;
      {$endif}

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
  AddMessage('Running on '+GetDistro);
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

    Form2.UseWget:=ReadBool('General','UseWget',False);

    Form2.ExtraVerbose:=ReadBool('General','ExtraVerbose',False);

    Form2.FPCPatches:=ReadString('Patches','FPCPatches','');
    Form2.LazPatches:=ReadString('Patches','LazarusPatches','');

    Form2.AutoSwitchURL:=ReadBool('General','AutoSwitchURL',True);

    listModules.ClearSelection;
    SortedModules:=TStringList.Create;
    try
      SortedModules.CommaText:=ReadString('General','Modules','');
      for i:=0 to SortedModules.Count-1 do
      begin
        j:=listModules.Items.IndexOf(SortedModules[i]);
        if j<>-1 then listModules.Selected[j]:=true;
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
      for i:=0 to listModules.Count-1 do
      begin
        if listModules.Selected[i] then modules:=modules+listModules.Items[i]+',';
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

procedure TForm1.AddMessage(const aMessage:string; const UpdateStatus:boolean=false);
begin
  SynEdit1.Append(aMessage);
  SynEdit1.CaretX:=0;
  SynEdit1.CaretY:=SynEdit1.Lines.Count;
  if UpdateStatus then StatusMessage.Text:=aMessage;
  {$ifdef usealternateui}
  alternateui_AddMessage(amessage,updatestatus);
  {$endif}  
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

