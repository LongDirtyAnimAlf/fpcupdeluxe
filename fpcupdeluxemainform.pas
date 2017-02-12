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
    Button4: TButton;
    Button5: TButton;
    CheckVerbosity: TCheckBox;
    Edit1: TEdit;
    Panel1: TPanel;
    RealFPCURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
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
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FPCOnlyClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
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
    procedure SetFPCTarget(aFPCTarget:string);
    procedure SetLazarusTarget(aLazarusTarget:string);
    procedure DisEnable(Sender: TObject;value:boolean);
    procedure Edit1Change(Sender: TObject);
    procedure PrepareRun;
    function RealRun:boolean;
    function GetFPCUPSettings(IniFile:string):boolean;
    function SetFPCUPSettings(IniFile:string):boolean;
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
  StrUtils,
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF UNIX}
  LazFileUtils,
  AboutFrm,
  unzipprogress,
  extrasettings,
  installerUniversal,
  fpcuputil,
  processutils,
  synedittext;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';
  NEWPASCALGITREPO='https://github.com/newpascal';
  FPCUPGITREPO=NEWPASCALGITREPO+'/fpcupdeluxe';
  {$ifdef MSWINDOWS}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/wincrossbins_v1.0';
  {$endif}
  {$ifdef Linux}
  {$ifdef CPUX86}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/linuxi386crossbins_v1.0';
  {$endif CPUX86}
  {$ifdef CPUX64}
  FPCUPBINSURL=FPCUPGITREPO+'/releases/download/linuxx64crossbins_v1.0';
  {$endif CPUX64}
  {$ifdef CPUARM}
  FPCUPBINSURL='';
  {$endif CPUARM}
  {$ifdef CPUAARCH64}
  FPCUPBINSURL='';
  {$endif CPUAARCH64}
  {$endif}
  {$ifdef FreeBSD}
  FPCUPBINSURL='';
  {$endif}
  {$ifdef Darwin}
  FPCUPBINSURL='';
  {$endif}
  FPCUPLIBSURL=FPCUPGITREPO+'/releases/download/crosslibs_v1.0';
  FPCUPDELUXEVERSION='1.2.0l';

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

function ExistWordInString(aString:pchar;aSearchString:string;aSearchOptions: TStringSearchOptions): Boolean;
var
  Size : Integer;
begin
  Size:=StrLen(aString);
  Result := SearchBuf(aString, Size, 0, 0, aSearchString, aSearchOptions)<>nil;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef CPUAARCH64}
  // disable some features
  FixesBtn.Enabled:=False;
  StableBtn.Enabled:=False;
  OldBtn.Enabled:=False;
  DinoBtn.Enabled:=False;
  RadioGroup1.Enabled:=False;
  RadioGroup2.Enabled:=False;
  Button5.Enabled:=False;
  {$endif CPUAARCH64}
  {$ifdef CPUARM}
  // disable some features
  FixesBtn.Enabled:=False;
  StableBtn.Enabled:=False;
  RadioGroup1.Enabled:=False;
  RadioGroup2.Enabled:=False;
  Button5.Enabled:=False;
  {$endif CPUARM}

  {$ifdef Darwin}
  RadioGroup2.Items.Strings[RadioGroup2.Items.IndexOf('wince')]:='i-sim';
  {$endif CPUARM}

  Self.Caption:='Lazarus and FPC installer and updater V'+FPCUPDELUXEVERSION+' based on fpcup'+RevisionStr+' ('+VersionDate+') for '+
                lowercase({$i %FPCTARGETCPU%})+'-'+lowercase({$i %FPCTARGETOS%});

  sStatus:='Sitting and waiting';

  oldoutput := System.Output;
  AssignSynEdit(System.Output, SynEdit1);
  Reset(System.Input);
  Rewrite(System.Output);

  AddMessage('Welcome @ fpclupdeluxe.');
  AddMessage('');

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
    CheckVerbosity.Checked:=ReadBool('General','Verbose',True);
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

  SaveInisFromResource(SafeGetApplicationPath+installerUniversal.SETTTINGSFILENAME,'settings_ini');
  SetConfigFile(SafeGetApplicationPath+installerUniversal.CONFIGFILENAME);

  if ListBoxFPCTarget.Count=0 then
  begin
    ListBoxFPCTarget.Items.CommaText:=installerUniversal.GetAlias('fpcURL','list');
    FPCTarget:='default';
  end;
  if ListBoxLazarusTarget.Count=0 then
  begin
    ListBoxLazarusTarget.Items.CommaText:=installerUniversal.GetAlias('lazURL','list');
    LazarusTarget:='default';
  end;

  sInstallDir:=ExcludeTrailingPathDelimiter(SafeExpandFileName(sInstallDir));

  Edit1.Text:=sInstallDir;

  // set edit1 (installdir) onchange here, to prevent early firing
  Edit1.OnChange:=nil;
  Edit1.OnKeyUp:=nil;
  {$ifdef Darwin}
  {$ifdef LCLCOCOA}
  // onchange does not work on cocoa, so use onkeyup
  Edit1.OnKeyUp:=@Edit1KeyUp;
  {$endif}
  {$endif}
  if Edit1.OnKeyUp=nil then Edit1.OnChange:=@Edit1Change;

  // create settings form
  // must be done here, to enable local storage/access of some setttings !!
  Form2:=TForm2.Create(Form1);

  InitFPCupManager;
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

procedure TForm1.InitFPCupManager;
var
  SortedModules: TStringList;
  i:integer;
  s:string;
begin
  FPCupManager:=TFPCupManager.Create;

  FPCupManager.ConfigFile:=SafeGetApplicationPath+installerUniversal.CONFIGFILENAME;

  FPCupManager.LoadFPCUPConfig;

  FPCupManager.FPCURL:='default';
  FPCupManager.LazarusURL:='default';
  FPCupManager.Verbose:=true;

  {$IF defined(BSD) and not defined(DARWIN)}
  FPCupManager.PatchCmd:='gpatch';
  {$ELSE}
  FPCupManager.PatchCmd:='patch';
  {$ENDIF}

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
  if (NOT GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir)+DELUXEFILENAME))
     then GetFPCUPSettings(SafeGetApplicationPath+DELUXEFILENAME);
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
  s:string;
  x:integer;
begin
  s:=SynEdit1.LineText;
  if Length(s)=0 then s:=SynEdit1.Lines[SynEdit1.CaretY-2];
  s:=Trim(s);
  if Length(s)=0 then exit;

  if (ExistWordInString(PChar(s),'checkout',[soWholeWord,soDown])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord,soDown])) then
  begin
    Memo1.Lines.Append('Performing a checkout ... please wait, could take some time.');
  end;

  // github error
  if (ExistWordInString(PChar(s),'429 too many requests',[soDown])) then
  begin
    Memo1.Lines.Append('GitHub blocked us due to too many download requests.');
    Memo1.Lines.Append('This will last for an hour, so please wait and be patient.');
    Memo1.Lines.Append('After this period, please re-run fpcupdeluxe.');
  end;

  if (ExistWordInString(PChar(s),'unable to connect to a repository at url',[soDown])) then
  begin
    Memo1.Lines.Append('SVN could not connect to the desired repository. URL:');
    Memo1.Lines.Append(FPCupManager.FPCURL);
    Memo1.Lines.Append('Please check your connection. Or run the command to try yourself:');
    Memo1.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
  end;

  if (ExistWordInString(PChar(s),'error:',[soWholeWord,soDown])) OR  (ExistWordInString(PChar(s),'fatal:',[soWholeWord,soDown])) then
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
          Memo1.Lines.Append('See: http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=30351');
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
    else if (Pos('fatal:',lowercase(s))>0) then
    begin
      Memo1.Lines.Append(s);
      Memo1.Lines.Append(SynEdit1.Lines[SynEdit1.CaretY-2]);
    end
    else if (Pos('error:',lowercase(s))>0) then
    begin
      Memo1.Lines.Append(s);
    end;
  end;

  // linker error
  if (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find',[])) then
  begin
    x:=Pos('-l',s);
    if x>0 then
    begin
      // add help into summary memo
      Memo1.Lines.Append('Missing library: lib'+Copy(s,x+2,MaxInt));
    end;
  end;

  // diskspace errors
  if (ExistWordInString(PChar(s),'Stream write error',[])) then
  begin
    Memo1.Lines.Append('There is not enough diskspace to finish this operation.');
    Memo1.Lines.Append('Please free some space and re-run fpcupdeluxe.');
  end;

  // RAM errors
  if (ExistWordInString(PChar(s),'Can''t call the assembler',[])) then
  begin
    Memo1.Lines.Append('Most likely, there is not enough RAM (swap) to finish this operation.');
    Memo1.Lines.Append('Please add some swap-space (1GB) and re-run fpcupdeluxe.');
  end;


end;

procedure TForm1.SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
var
  FG, BG: TColor;
  s:string;
begin
  s:=SynEdit1.Lines[Line-1];

  // github error
  if (ExistWordInString(PChar(s),'429 too many requests',[soDown])) then
  begin
    FG      := clRed;   //Text Color
    BG      := clNavy;  //BackGround
    Special := True;    //Must be true
  end;

  // svn connection error
  if (ExistWordInString(PChar(s),'unable to connect to a repository at url',[soDown])) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
  end;

  if ExistWordInString(PChar(s),'svn: e',[soDown]) then
  begin
    FG      := clFuchsia;
    BG      := clBlack;
    Special := True;
  end;

  if ExistWordInString(PChar(s),'Executing :',[soWholeWord,soDown]) then
  begin
    FG      := clAqua;
    BG      := clBlack;
    Special := True;
  end;

  if (ExistWordInString(PChar(s),'A',[soWholeWord])) OR (ExistWordInString(PChar(s),'U',[soWholeWord])) then
  begin
    FG      := clSkyBlue;
    BG      := clBlack;
    Special := True;
  end;

  if ExistWordInString(PChar(s),'info:',[soWholeWord,soDown]) then
  begin
    FG      := clYellow;
    BG      := clBlack;
    Special := True;
  end;

  if ExistWordInString(PChar(s),'Please wait:',[soWholeWord,soDown]) then
  begin
    FG      := clBlue;
    BG      := clWhite;
    Special := True;
  end;

  if (ExistWordInString(PChar(s),'warning:',[soWholeWord,soDown])) OR (ExistWordInString(PChar(s),'hint:',[soWholeWord,soDown])) then
  begin
    FG      := clGreen;
    BG      := clBlack;
    Special := True;
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

  // linker error
  if (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find',[])) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
  end;

  // diskspace error
  if (ExistWordInString(PChar(s),'Stream write error',[])) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
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

    if NOT Form2.IncludeHelp then
    begin
      FPCupManager.SkipModules:=FPCupManager.SkipModules+'helpfpc,helplazarus';
    end;

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
  i:integer;
begin
  if (ListBoxFPCTarget.ItemIndex=-1) or (ListBoxLazarusTarget.ItemIndex=-1) then
  begin
    ShowMessage('Please select a FPC and Lazarus version first');
    exit;
  end;
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
    Edit1.Text:=sInstallDir;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ListBox3.ClearSelection;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  BinsURL,LibsURL,DownloadURL,TargetFile,TargetPath,BinPath,LibPath,UnZipper,s:string;
  success,verbose:boolean;
  UseNativeUnzip:boolean;
  FileUnzipper: TThreadedUnzipper;
  {$ifdef Unix}
  fileList: TStringList;
  i:integer;
  {$endif}
begin
  if (RadioGroup1.ItemIndex=-1) and (RadioGroup2.ItemIndex=-1) then
  begin
    ShowMessage('Please select a CPU and OS target first');
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

  {$ifndef FreeBSD}
  if (FPCupManager.CrossOS_Target='freebsd') OR (FPCupManager.CrossOS_Target='netbsd') then
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

  if (FPCupManager.CrossOS_Target='linux') then
  begin
    ShowMessage('Be forwarned: you may need to add some extra linking when cross-compiling.' + sLineBreak + CrossGCCMsg);
    Memo1.Lines.Append(CrossGCCMsg);
    Memo1.Lines.Append('');
  end;

  if (FPCupManager.CrossCPU_Target='jvm') then FPCupManager.CrossOS_Target:='java';
  if (FPCupManager.CrossOS_Target='java') then FPCupManager.CrossCPU_Target:='jvm';

  if FPCupManager.CrossOS_Target='windows' then
  begin
    if FPCupManager.CrossCPU_Target='i386' then FPCupManager.CrossOS_Target:='win32';
    if FPCupManager.CrossCPU_Target='x86_64' then FPCupManager.CrossOS_Target:='win64';
  end;

  if (FPCupManager.CrossCPU_Target='') then
  begin
    ShowMessage('Please select a CPU target first');
    FPCupManager.CrossOS_Target:=''; // cleanup
    exit;
  end;

  if (FPCupManager.CrossOS_Target='') then
  begin
    ShowMessage('Please select an OS target first');
    FPCupManager.CrossCPU_Target:=''; // cleanup
    exit;
  end;

  DisEnable(Sender,False);

  try
    if (FPCupManager.CrossCPU_Target='arm') then
    begin
      if (FPCupManager.CrossOS_Target='wince') then
      begin
        //FPCupManager.CrossOPT:='-CpARMV6 -CfSoft ';
        FPCupManager.CrossOPT:='-CpARMV6 ';
      end
      else
      if (FPCupManager.CrossOS_Target='darwin') then
      begin
        //FPCupManager.FPCOPT:='-Sh -WP8.1';
        FPCupManager.FPCOPT:='-dFPC_ARMHF ';
        FPCupManager.CrossOPT:='-CpARMV7 ';
      end
      else
      begin
        // default: armhf
        FPCupManager.FPCOPT:='-dFPC_ARMHF ';
        FPCupManager.CrossOPT:='-CpARMV7A -CfVFPV3 -OoFASTMATH -CaEABIHF ';
      end;
    end;

    if (FPCupManager.CrossCPU_Target='aarch64') then
    begin
      if (FPCupManager.CrossOS_Target='darwin') then
      begin
        //FPCupManager.FPCOPT:='-Sh -WP8.1';
        FPCupManager.CrossOPT:='-CaAARCH64IOS ';
      end;
    end;

    // use the available source to build the cross-compiler ... change nothing about source and url !!
    FPCupManager.OnlyModules:='FPCCleanOnly,FPCBuildOnly';

    if Form2.IncludeLCL then
    begin
      if (FPCupManager.CrossOS_Target<>'java') AND (FPCupManager.CrossOS_Target<>'android') then
      begin
        FPCupManager.OnlyModules:=FPCupManager.OnlyModules+',LCLCross';
        // if Darwin x64, only cocoa will work.
        if ((FPCupManager.CrossOS_Target='darwin') AND (FPCupManager.CrossCPU_Target='x86_64'))
            then FPCupManager.CrossLCL_Platform:='cocoa';
      end;
    end;

    FPCupManager.FPCURL:='skip';
    FPCupManager.LazarusURL:='skip';

    FPCupManager.CrossLibraryDirectory:=Form2.GetLibraryDirectory(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
    FPCupManager.CrossToolsDirectory:=Form2.GetToolsDirectory(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);

    AddMessage('Going to install a cross-compiler from current sources.');

    sStatus:='Going to build a cross-compiler for '+FPCupManager.CrossOS_Target+'-'+FPCupManager.CrossCPU_Target;
    if FPCupManager.FPCOPT<>'' then sStatus:=sStatus+' ('+FPCupManager.FPCOPT+')';
    sStatus:=sStatus+'.';

    MissingCrossBins:=false;
    MissingCrossLibs:=false;

    if NOT RealRun then
    begin
      {$ifndef BSD}

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

        if FPCupManager.CrossOS_Target='linux' then
        begin
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='LinuxARM.rar';
          if FPCupManager.CrossCPU_Target='aarch64' then BinsURL:='LinuxAarch64.rar';
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='Linuxi386.rar';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='Linuxx64.rar';
          if FPCupManager.CrossCPU_Target='powerpc' then BinsURL:='LinuxPowerPC.rar';
          if FPCupManager.CrossCPU_Target='powerpc64' then BinsURL:='LinuxPowerPC64.rar';
        end;
        if FPCupManager.CrossOS_Target='freebsd' then
        begin
          if FPCupManager.CrossCPU_Target='i386' then BinsURL:='FreeBSDi386.rar';
          if FPCupManager.CrossCPU_Target='x86_64' then BinsURL:='FreeBSDx64.rar';
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
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='AndroidARM.rar';
          if FPCupManager.CrossCPU_Target='aarch64' then BinsURL:='AndroidAArch64.rar';
        end;
        if FPCupManager.CrossOS_Target='embedded' then
        begin
          if FPCupManager.CrossCPU_Target='arm' then BinsURL:='EmbeddedARM.rar';
          if FPCupManager.CrossCPU_Target='avr' then BinsURL:='EmbeddedAVR.rar';
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

        // tricky ... reset BinsURL in case the binutils and libs are already there ... to exit this retry ... ;-)
        if (DirectoryExists(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+BinPath))
            AND
            (DirectoryExists(IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath))
          then BinsURL:='';

        if BinsURL<>'' then
        begin

          UseNativeUnzip:=(ExtractFileExt(BinsURL)='.zip');

          if MissingCrossBins then
          begin

            // no cross-bins available
            if (Length(FPCUPBINSURL)=0) then
            begin
              ShowMessage('No tools available online. You could do a feature request ... ;-)');
              exit;
            end;

            AddMessage('Going to download the right cross-bins. Can (will) take some time !',True);
            {$ifdef MSWINDOWS}
            DownloadURL:=FPCUPBINSURL+'/'+'WinCrossBins'+BinsURL;
            {$else}
            DownloadURL:=FPCUPBINSURL+'/'+'CrossBins'+BinsURL;
            {$endif}
            AddMessage('Please wait: Going to download the binary-tools from '+DownloadURL);
            TargetFile := SysUtils.GetTempFileName;
            success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            if success then
            begin
              AddMessage('Successfully downloaded binary-tools archive.');
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir);
              {$ifndef MSWINDOWS}
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir)+'cross'+BinPath+DirectorySeparator;
              {$endif}

              AddMessage('Going to extract archive into '+TargetPath);

              if UseNativeUnzip then
              begin
                ProgressForm := TProgressForm.Create(Self);
                try
                  FileUnzipper := TThreadedUnzipper.Create;
                  try
                    FileUnzipper.OnZipProgress := @ProgressForm.DoOnZipProgress;
                    FileUnzipper.OnZipFile := @ProgressForm.DoOnZipFile;
                    FileUnzipper.OnZipCompleted := @ProgressForm.DoOnZipCompleted;
                    FileUnzipper.DoUnZip(TargetFile, TargetPath,[]);
                    success:=(ProgressForm.ShowModal=mrOk);
                  finally
                    if Assigned(FileUnzipper) then FileUnzipper := nil;
                  end;
               finally
                 ProgressForm.Free;
               end;
              end
              else
              begin
                {$ifdef MSWINDOWS}
                success:=(ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+TargetFile+' "'+TargetPath+'"',true)=0);
                if (NOT success) then
                {$endif}
                begin
                  {$ifdef MSWINDOWS}
                  UnZipper := IncludeTrailingPathDelimiter(FPCupManager.MakeDirectory) + 'unrar\bin\unrar.exe';
                  {$else}
                  UnZipper := 'unrar';
                  {$endif}
                  success:=(ExecuteCommand(UnZipper + ' x "' + TargetFile + '" "' + TargetPath + '"',true)=0);
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

          if MissingCrossLibs then
          begin
            AddMessage('Going to download the right cross-libs. Can (will) take some time !',True);
            DownloadURL:=FPCUPLIBSURL+'/'+'CrossLibs'+LibsURL;
            AddMessage('Please wait: Going to download the libraries from '+DownloadURL);
            TargetFile := SysUtils.GetTempFileName;
            success:=DownLoad(FPCupManager.UseWget,DownloadURL,TargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
            if success then
            begin
              AddMessage('Successfully downloaded the libraries.');
              TargetPath:=IncludeTrailingPathDelimiter(sInstallDir);
              //TargetPath:=IncludeTrailingPathDelimiter(sInstallDir)+'cross'+LibPath+DirectorySeparator;
              AddMessage('Going to extract them into '+TargetPath);

              // many files to unpack for Darwin libs : do not show progress of unpacking files when unpacking for Darwin.
              verbose:=(FPCupManager.CrossOS_Target<>'darwin');

              if UseNativeUnzip then
              begin
                ProgressForm := TProgressForm.Create(Self);
                try
                  FileUnzipper := TThreadedUnzipper.Create;
                  try
                    FileUnzipper.OnZipProgress := @ProgressForm.DoOnZipProgress;
                    FileUnzipper.OnZipFile := @ProgressForm.DoOnZipFile;
                    FileUnzipper.OnZipCompleted := @ProgressForm.DoOnZipCompleted;
                    FileUnzipper.DoUnZip(TargetFile, TargetPath,[]);
                    success:=(ProgressForm.ShowModal=mrOk);
                  finally
                    if Assigned(FileUnzipper) then FileUnzipper := nil;
                  end;
               finally
                 ProgressForm.Free;
               end;
              end
              else
              begin
                {$ifdef MSWINDOWS}
                success:=(ExecuteCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+TargetFile+' "'+TargetPath+'"',true)=0);
                if (NOT success) then
                {$endif}
                begin
                  {$ifdef MSWINDOWS}
                  UnZipper := IncludeTrailingPathDelimiter(FPCupManager.MakeDirectory) + 'unrar\bin\unrar.exe';
                  {$else}
                  UnZipper := 'unrar';
                  {$endif}
                  success:=(ExecuteCommand(UnZipper + ' x "' + TargetFile + '" "' + TargetPath + '"',true)=0);
                end;
              end;
            end;
          end;

          if success then
          begin
            AddMessage('Successfully extracted cross-tools.');
            // run again with the correct libs and binutils
            label1.Font.Color:=clDefault;
            label2.Font.Color:=clDefault;
            AddMessage('Got all tools now. New try building a cross-compiler for '+FPCupManager.CrossOS_Target+'-'+FPCupManager.CrossCPU_Target,True);
            FPCupManager.Sequencer.ResetAllExecuted;
            RealRun;
          end;
          SysUtils.DeleteFile(TargetFile);

          if (NOT success) then AddMessage('No luck in getting then cross-tools ... aborting.');
        end;
      end
      else
      {$endif BSD}
      begin
        AddMessage('Building cross-tools failed ... ??? ... aborting.');
      end;

    end;

  finally
    DisEnable(Sender,True);
  end;
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
       else FPCupManager.OnlyModules:='fpc';

    FPCupManager.LazarusURL:='skip';

    if NOT Form2.IncludeHelp then
    begin
      FPCupManager.SkipModules:=FPCupManager.SkipModules+'helpfpc';
    end;

    sStatus:='Going to install/update FPC only.';

    RealRun;
  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.LazarusOnlyClick(Sender: TObject);
var
  FModuleList:TStringList;
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
      FPCupManager.OnlyModules:='lazarus';
    end;

    FPCupManager.FPCURL:='skip';

    if NOT Form2.IncludeHelp then
    begin
      FPCupManager.SkipModules:=FPCupManager.SkipModules+'helplazarus';
    end;

    sStatus:='Going to install/update Lazarus only.';

    RealRun;
  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  i:integer;
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
  SynEdit1.ClearAll;
  Memo1.Clear;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  sInstallDir:=Edit1.Text;
  if GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir)+DELUXEFILENAME) then
  begin
    AddMessage('Got settings from install directory');
  end;
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  sInstallDir:=Edit1.Text;
  if GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir)+DELUXEFILENAME) then
  begin
    AddMessage('Got settings from install directory');
  end;
end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // set last used install directory
  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    WriteString('General','InstallDirectory',sInstallDir);

    WriteBool('General','Verbose',CheckVerbosity.Checked);
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
  if (NOT SetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir)+DELUXEFILENAME))
     then SetFPCUPSettings(SafeGetApplicationPath+DELUXEFILENAME);

  CloseAction:=caFree;
end;

procedure TForm1.DisEnable(Sender: TObject;value:boolean);
begin
  //if Sender<>BitBtnFPCandLazarus then
  BitBtnFPCandLazarus.Enabled:=value;
  //if Sender<>Button2 then
  Button2.Enabled:=value;
  Button3.Enabled:=value;
  Button4.Enabled:=value;
  Button5.Enabled:=value;
  BitBtnFPCOnly.Enabled:=value;
  BitBtnLazarusOnly.Enabled:=value;
  Button7.Enabled:=value;
  Button8.Enabled:=value;

  ListBoxFPCTarget.Enabled:=value;
  ListBoxLazarusTarget.Enabled:=value;
  ListBox3.Enabled:=value;

  Edit1.Enabled:=value;
  RadioGroup1.Enabled:=value;
  RadioGroup2.Enabled:=value;
  CheckVerbosity.Enabled:=value;
  CheckAutoClear.Enabled:=value;

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
var
  s:string;
begin
  label1.Font.Color:=clDefault;
  label2.Font.Color:=clDefault;

  if CheckAutoClear.Checked then Button8.Click;

  FPCupManager.Sequencer.ResetAllExecuted;

  FPCupManager.OnlyModules:='';
  FPCupManager.IncludeModules:='';
  FPCupManager.SkipModules:='';
  FPCupManager.CrossCPU_Target:='';
  FPCupManager.CrossOS_Target:='';
  FPCupManager.CrossOS_SubArch:='';
  FPCupManager.CrossLCL_Platform:='';

  FPCupManager.CrossOPT:='';

  FPCupManager.CrossLibraryDirectory:='';
  FPCupManager.CrossToolsDirectory:='';

  FPCupManager.Verbose:=CheckVerbosity.Checked;

  FPCupManager.FPCOPT:=Form2.FPCOptions;
  FPCupManager.FPCDesiredBranch:=Form2.FPCBranch;
  FPCupManager.FPCDesiredRevision:=Form2.FPCRevision;

  FPCupManager.LazarusOPT:=Form2.LazarusOptions;
  FPCupManager.LazarusDesiredBranch:=Form2.LazarusBranch;
  FPCupManager.LazarusDesiredRevision:=Form2.LazarusRevision;

  // force some settings for default downloader
  {$ifdef Darwin}
  FPCupManager.UseWget:=false;
  {$else}
    {$ifdef MSWINDOWS}
    FPCupManager.UseWget:=false;
    {$else}
    FPCupManager.UseWget:=Form2.UseWget;
    {$endif}
  {$endif}

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

  FPCupManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update';
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
  if Form2.SplitLazarus
     then FPCupManager.LazarusSourceDirectory:=FPCupManager.LazarusInstallDirectory+'src'
     else FPCupManager.LazarusSourceDirectory:=FPCupManager.LazarusInstallDirectory;
  }

  FPCupManager.LazarusPrimaryConfigPath:=sInstallDir+'config_'+ExtractFileName(FPCupManager.LazarusDirectory);

  FPCupManager.ExportOnly:=(NOT Form2.CheckRepo.Checked);

  FPCupManager.FPCPatches:=Form2.FPCPatches;

  RealFPCURL.Text:='';
  RealLazURL.Text:='';

  sStatus:='Sitting and waiting';
  StatusMessage.Text:=sStatus;

  Memo1.Lines.Clear;
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
      AddMessage('ERROR: Fpcupdeluxe failed.');
      label1.Font.Color:=clRed;
      label2.Font.Color:=clRed;
      StatusMessage.Text:='Hmmm, something went wrong ... have a good look at the command screen !';
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
      label1.Font.Color:=clLime;
      label2.Font.Color:=clLime;
      StatusMessage.Text:='That went well !!!';
    end;
  except
    // just swallow exceptions
    StatusMessage.Text:='Got an unexpected exception ... don''t know what to do unfortunately.';
  end;
end;

function TForm1.GetFPCUPSettings(IniFile:string):boolean;
var
  i,j:integer;
  SortedModules:TStringList;
begin
  result:=FileExists(IniFile);

  if result then with TIniFile.Create(IniFile) do
  try

    FPCupManager.ExportOnly:=(NOT ReadBool('General','GetRepo',True));

    FPCTarget:=ReadString('URL','fpcURL','default');
    if FPCTarget='' then FPCTarget:='default';
    LazarusTarget:=ReadString('URL','lazURL','default');
    if LazarusTarget='' then LazarusTarget:='default';

    Form2.FPCOptions:=ReadString('General','FPCOptions','');
    Form2.LazarusOptions:=ReadString('General','LazarusOptions','');
    Form2.FPCRevision:=ReadString('General','FPCRevision','');
    Form2.LazarusRevision:=ReadString('General','LazarusRevision','');

    Form2.SplitFPC:=ReadBool('General','SplitFPC',True);
    Form2.SplitLazarus:=ReadBool('General','SplitLazarus',False);

    //FPCupManager.UseWget:=ReadBool('General','UseWget',False);
    //Form2.UseWget:=FPCupManager.UseWget;
    Form2.UseWget:=ReadBool('General','UseWget',False);

    Form2.FPCPatches:=ReadString('Patches','FPCPatches','');

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

function TForm1.SetFPCUPSettings(IniFile:string):boolean;
var
  i:integer;
  modules:string;
begin
  result:=DirectoryExists(ExtractFileDir(IniFile));

  if result then with TIniFile.Create(IniFile) do
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

    WriteString('Patches','FPCPatches',Form2.FPCPatches);

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

end;

procedure TForm1.AddMessage(aMessage:string; UpdateStatus:boolean=false);
begin
  //SynEdit1.Append(aMessage);
  SynEdit1.InsertTextAtCaret(aMessage+sLineBreak,scamAdjust);
  SynEdit1.CaretX:=0;
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
    if i<>-1 then ListBoxFPCTarget.Selected[i]:=true;
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
    if i<>-1 then ListBoxLazarusTarget.Selected[i]:=true;
  end;
end;

end.

