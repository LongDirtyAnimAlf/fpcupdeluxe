unit fpcupdeluxemainformreader;

{$mode objfpc}{$H+}

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Types, Buttons, Menus, ComCtrls,
  {$ifndef READER}
  SynEdit, SynEditMiscClasses, SynEditPopup,
  {$endif}
  LMessages, ActnList, StdActns, IniPropStorage, LCLVersion,
  {$ifdef RemoteLog}
  mormotdatamodelclient,
  {$endif}
  installerManager
  {$ifdef usealternateui},alternateui{$endif}
  ;

//{$ifdef lcl_fullversion}
{$if lcl_fullversion>2010000}
{.$define EnableLanguages}
{$endif}
//{$endif}
{$define EnableLanguages}

const
  WM_THREADINFO = LM_USER + 2010;

type
  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    btnCheckToolsLocations: TButton;
    btnBuildNativeCompiler: TButton;
    chkGitlab: TCheckBox;
    imgSVN: TImage;
    imgGitlab: TImage;
    ListBoxFPCHistoryNew: TListView;
    btnCreateLazarusConfig: TButton;
    ButtonSubarchSelect: TButton;
    btnSendLog: TButton;
    btnUpdateLazarusMakefiles: TButton;
    btnInstallModule: TButton;
    btnSetupPlus: TButton;
    btnClearLog: TButton;
    btnUninstallModule: TButton;
    btnGetOpenSSL: TButton;
    ButtonAutoCrossUpdate: TButton;
    ChkMakefileFPC: TButton;
    ButtonInstallCrossCompiler: TButton;
    ButtonRemoveCrossCompiler: TButton;
    CheckAutoClear: TCheckBox;
    CreateStartup: TButton;
    ChkMakefileLaz: TButton;
    actFileExit: TFileExit;
    actFileSave: TFileSaveAs;
    HistorySheet: TTabSheet;
    IniPropStorageApp: TIniPropStorage;
    ListBoxFPCTarget: TListBox;
    ListBoxFPCTargetTag: TListBox;
    ListBoxLazarusHistoryNew: TListView;
    ListBoxLazarusTarget: TListBox;
    ListBoxLazarusTargetTag: TListBox;
    listModules: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MemoAddTag: TMemo;
    memoSummary: TMemo;
    MAbout: TMenuItem;
    MWiki: TMenuItem;
    MHelp: TMenuItem;
    MenuItem2: TMenuItem;
    MEnglishlanguage: TMenuItem;
    MChineseCNlanguage: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuFile: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MDutchlanguage: TMenuItem;
    Separator1: TMenuItem;
    MOnlineDocs: TMenuItem;
    MUkrainianLanguage: TMenuItem;
    MFrenchlanguage: TMenuItem;
    MGermanlanguage: TMenuItem;
    MFPCBugs: TMenuItem;
    MKoreanlanguage: TMenuItem;
    MLazarusBugs: TMenuItem;
    MIssuesGitHub: TMenuItem;
    MIssuesForum: TMenuItem;
    PageControl1: TPageControl;
    radgrpCPU: TRadioGroup;
    radgrpOS: TRadioGroup;
    StatusMessage: TEdit;
    BasicSheet: TTabSheet;
    CrossSheet: TTabSheet;
    ModuleSheet: TTabSheet;
    ExtraSheet: TTabSheet;
    TagSheet: TTabSheet;
    btnInstallDirSelect: TButton;
    InstallDirEdit: TEdit;
    Panel1: TPanel;
    RealFPCURL: TEdit;
    RealLazURL: TEdit;
    MemoHistory: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    {$ifdef READER}
    CommandOutputScreen: TMemo;
    FPCVersionLabel: TStaticText;
    LazarusVersionLabel: TStaticText;
    FPCHistoryLabel: TStaticText;
    LazarusHistoryLabel: TStaticText;
    FPCTagLabel: TStaticText;
    LazarusTagLabel: TStaticText;
    TrunkBtn: TButton;
    FixesBtn: TButton;
    StableBtn: TButton;
    AndroidBtn: TButton;
    WABtn: TButton;
    WioBtn: TButton;
    PicoBtn: TButton;
    UltiboBtn: TButton;
    mORMotBtn: TButton;
    BitBtnHalt: TButton;
    BitBtnFPCandLazarus: TButton;
    BitBtnFPCOnly: TButton;
    BitBtnFPCOnlyTag: TButton;
    BitBtnLazarusOnly: TButton;
    BitBtnLazarusOnlyTag: TButton;
    BitBtnFPCSetRevision: TButton;
    BitBtnLazarusSetRevision: TButton;
    OPMBtn: TButton;
    ESPBtn: TButton;
    {$else}
    CommandOutputScreen: TSynEdit;
    FPCVersionLabel: TLabel;
    LazarusVersionLabel: TLabel;
    FPCHistoryLabel: TLabel;
    LazarusHistoryLabel: TLabel;
    FPCTagLabel: TLabel;
    LazarusTagLabel: TLabel;
    TrunkBtn: TBitBtn;
    FixesBtn: TBitBtn;
    StableBtn: TBitBtn;
    AndroidBtn: TBitBtn;
    WABtn: TBitBtn;
    WioBtn: TBitBtn;
    PicoBtn: TBitBtn;
    UltiboBtn: TBitBtn;
    mORMotBtn: TBitBtn;
    BitBtnHalt: TBitBtn;
    BitBtnFPCandLazarus: TBitBtn;
    BitBtnFPCOnly: TBitBtn;
    BitBtnFPCOnlyTag: TBitBtn;
    BitBtnLazarusOnly: TBitBtn;
    BitBtnLazarusOnlyTag: TBitBtn;
    BitBtnFPCSetRevision: TBitBtn;
    BitBtnLazarusSetRevision: TBitBtn;
    OPMBtn: TBitBtn;
    ESPBtn: TBitBtn;
    {$endif}
    procedure actFileSaveAccept({%H-}Sender: TObject);
    procedure BitBtnSetRevisionClick(Sender: TObject);
    procedure btnCheckToolsLocationsClick({%H-}Sender: TObject);
    procedure btnUpdateLazarusMakefilesClick({%H-}Sender: TObject);
    procedure btnBuildNativeCompilerClick(Sender: TObject);
    procedure ButtonSubarchSelectClick({%H-}Sender: TObject);
    procedure chkGitlabChange(Sender: TObject);
    procedure CommandOutputScreenChange(Sender: TObject);
    procedure IniPropStorageAppRestoringProperties({%H-}Sender: TObject);
    procedure IniPropStorageAppSavingProperties({%H-}Sender: TObject);
    procedure ListBoxTargetDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LanguageClick(Sender: TObject);
    procedure MOnlineDocsClick({%H-}Sender: TObject);
    procedure WikiClick({%H-}Sender: TObject);
    procedure radgrpTargetChanged({%H-}Sender: TObject);
    procedure TagSelectionChange(Sender: TObject;{%H-}User: boolean);
    procedure OnlyTagClick({%H-}Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure BitBtnHaltClick({%H-}Sender: TObject);
    procedure btnGetOpenSSLClick({%H-}Sender: TObject);
    procedure btnCreateLazarusConfigClick({%H-}Sender: TObject);
    procedure ChkMakefileFPCClick(Sender: TObject);
    procedure Edit1KeyUp({%H-}Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure FPCVersionLabelClick({%H-}Sender: TObject);
    procedure btnInstallModuleClick(Sender: TObject);
    procedure btnInstallDirSelectClick({%H-}Sender: TObject);
    procedure btnSetupPlusClick({%H-}Sender: TObject);
    procedure btnLogClick({%H-}Sender: TObject);
    procedure ButtonProcessCrossCompiler(Sender: TObject);
    procedure ButtonAutoUpdateCrossCompiler(Sender: TObject);
    procedure FormClose({%H-}Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
    procedure LazarusVersionLabelClick({%H-}Sender: TObject);
    procedure listModulesSelectionChange(Sender: TObject; User: boolean);
    procedure listModulesShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure MFPCBugsClick({%H-}Sender: TObject);
    procedure MIssuesForumClick({%H-}Sender: TObject);
    procedure MIssuesGitHubClick({%H-}Sender: TObject);
    procedure MLazarusBugsClick({%H-}Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    {$ifndef READER}
    procedure CommandOutputScreenSpecialLineMarkup({%H-}Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    {$endif}
    procedure TargetSelectionChange(Sender: TObject; User: boolean);
    procedure AboutClick({%H-}Sender: TObject);
    procedure CommandOutputScreenMouseWheel({%H-}Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure QuickBtnClick(Sender: TObject);
    {$ifdef usealternateui}
    procedure alternateuibutClick(Sender: TObject);
    procedure alternateuibutEnter(Sender: TObject);
    procedure alternateuibutLeave(Sender: TObject);
    {$endif}
  private
    { private declarations }
    MessageTrigger:boolean;
    FPCupManager:TFPCupManager;
    //oldoutput: TextFile;
    sInstallDir:string;
    sStatus:string;
    {$ifdef EnableLanguages}
    sLanguage:string;
    {$endif}
    FFPCTarget,FLazarusTarget:string;
    MissingCrossBins:boolean;
    MissingCrossLibs:boolean;
    MissingTools:boolean;
    InternalError:string;
    {$ifdef RemoteLog}
    sConsentWarning:boolean;
    aDataClient:TDataClient;
    {$endif}
    procedure HandleInfo(var Msg: TLMessage); message WM_THREADINFO;
    procedure InitFpcupdeluxe({%H-}Data: PtrInt=0);
    procedure ScrollToSelected({%H-}Data: PtrInt=0);
    {$ifdef RemoteLog}
    procedure InitConsent({%H-}Data: PtrInt=0);
    {$endif}
    procedure ProcessInfo(Sender: TObject);
    procedure InitShortCuts;
    procedure CheckForUpdates({%H-}Data: PtrInt=0);
    function  AutoUpdateCrossCompiler(Sender: TObject):boolean;
    function  ProcessCrossCompiler(Sender: TObject):boolean;
    procedure SetFPCTarget(aFPCTarget:string);
    procedure SetLazarusTarget(aLazarusTarget:string);
    procedure DisEnable({%H-}Sender: TObject;value:boolean);
    procedure Edit1Change({%H-}Sender: TObject);
    function  PrepareRun(Sender: TObject):boolean;
    function  RealRun:boolean;
    procedure GetSystemInfo;
    procedure InstallModule(aModule:string; UnInstall:boolean);
    function  GetFPCUPSettings(IniDirectory:string):boolean;
    function  SetFPCUPSettings(IniDirectory:string):boolean;
    procedure FillSourceListboxes;
    procedure AddMessage(const aMessage:string; const UpdateStatus:boolean=false);
    procedure SetTarget(aControl:TControl;const aTarget:string='');
    procedure InitFPCupManager;
    function  GetCmdFontSize:integer;
    procedure SetCmdFontSize(aValue:integer);
    function GetCmdFontName: String;
    procedure SetCmdFontName(aValue: String);
    procedure ParseRevisions(IniDirectory:string);
    procedure AddRevision(TargetFPC,TargetLAZ:boolean;aHash,aName:string;aDate:TDateTime);
    procedure AddTag(Sender: TObject;aTag:string);
    {$ifdef EnableLanguages}
    procedure Translate(const Language: string);
    {$endif}
    {$ifndef usealternateui}
    property  FPCTarget:string read FFPCTarget write SetFPCTarget;
    property  LazarusTarget:string read FLazarusTarget write SetLazarusTarget;
    {$endif}
  public
    { public declarations }
    {$ifdef usealternateui}
    property FPCTarget:string read FFPCTarget write SetFPCTarget;
    property LazarusTarget:string read FLazarusTarget write SetLazarusTarget;
    {$endif}

  published
    property CmdFontSize: Integer read GetCmdFontSize write SetCmdFontSize;
    property CmdFontName: String read GetCmdFontName write SetCmdFontName;
  end;

resourcestring
  upCheckUpdate = 'Please wait. Checking for updates.';
  upUpdateFound = 'New FPCUPdeluxe version available';
  upUpdateNotFound = 'No updates found.';
  upBuildCrossCompiler = 'Going to install a cross-compiler from available sources.';
  upBuildAllCrossCompilers = 'Going to auto-build all installed cross-compilers !';
  upBuildAllCrossCompilersCheck = 'Checking FPC configfile [fpc.cfg] for cross-compilers in ';
  upBuildAllCrossCompilersFound = 'Found crosscompiler for ';
  upBuildAllCrossCompilersUpdate = 'Going to update cross-compiler.';

  upSelectFPCTarget = 'Please select a FPC target first';
  upSelectLazarusTarget = 'Please select a Lazarus target first';
  upSelectFPCLazarusTarget = 'Please select a FPC and Lazarus target first';
  upInstallFPC = 'Going to install/update FPC.';
  upInstallLazarus = 'Going to install/update Lazarus.';
  upInstallFPCLazarus = 'Going to install/update FPC and Lazarus.';

  upInstallModule = 'Going to install/update module';
  upRemoveModule = 'Going to remove module';

  upInstallCrossCompiler = 'Going to install the cross-compiler for';
  upRemoveCrossCompiler = 'Going to remove the cross-compiler for';
  upUpdateCrossAll = 'Going to update all crosscompilers';
  upTryBuildCrossCompiler = 'Trying to build the cross-compiler for';


  upMissingTools = 'The building of a crosscompiler failed due to missing cross-tools.';
  upAdvertiseTools = 'FPCUPdeluxe can try to download them if available !';
  upLookupTools = 'Looking for FPCUPdeluxe cross-tools on GitHub (if any).';
  upLookupBinTools = 'Going to look for the right cross-bins. Can (will) take some time !';
  upLookupLibTools = 'Going to look for the right cross-libraries. Can (will) take some time !';
  upLookup = 'Looking for';

  upInstallDirectory = 'Install directory';
  upQuestionContinue = 'Do you want to continue ?';
  upSpaceWarning = 'Having a space in your install path is very ill-advised !';

  upInstallDirectoryCurrent = 'Current install directory';
  upInstallSettingsCurrent = 'Got settings from install directory';

  upDarwinFolderWarning1 = 'FPCUPdeluxe cannot be run inside the Downloads folder.';
  upDarwinFolderWarning2 = 'This is NOT allowed by OSX security measures.';
  upDarwinFolderWarning3 = 'Copy FPCUPdeluxe into its own directory and run from there.';

  upFolderWarning1 = 'FPCUPdeluxe could not create its necessary setting-files.';
  upFolderWarning2 = 'Please check the folder permissions, and re-start.';

  upQuitRequest = 'Please quit FPCUPdeluxe.';

  upInstallConfimation = 'Going to install %s and %s%s.';
  upInstallConfimationSimple = 'Going to install the %s.';

  upSelectCPUTarget = 'Please select a CPU target first.';
  upSelectOSTarget = 'Please select an OS target first.';

var
  Form1: TForm1;

implementation

{$ifdef READER}
  {$R fpcupdeluxemainformreader.lfm}
{$else}
  {$R fpcupdeluxemainform.lfm}
{$endif}

uses
  InterfaceBase, // for WidgetSet
  LCLType, // for MessageBox
  LCLIntf, // for OpenURL
  IniFiles,
  StrUtils,
  {$ifdef EnableLanguages}
  //Translations,
  LCLTranslator,
  DefaultTranslator,
  //LazUTF8,
  {$endif}
  AboutFrm,
  extrasettings,
  subarch,
  modulesettings,
  {$IF (DEFINED(WINDOWS)) OR (DEFINED(LINUX))}
  DPB.Forms.Sequencial,
  {$ENDIF}
  //checkoptions,
  installerCore,
  installerUniversal,
  m_crossinstaller, // for checking of availability of fpc[laz]up[deluxe] cross-compilers
  fpcuputil,
  process,
  processutils;

//{$I message.inc}

function NaturalCompare(aList: TStringList; aIndex1, aIndex2: Integer): Integer;
begin
  Result := NaturalCompareText(aList[aIndex2], aList[aIndex1]);
end;

// "stolen" from opkman_common
function MessageDlgEx(const AMsg: string; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AParent: TForm): TModalResult;
var
  MsgFrm: TForm;
begin
  MsgFrm := CreateMessageDialog(AMsg, ADlgType, AButtons);
  try
    MsgFrm.FormStyle := fsSystemStayOnTop;
    if AParent <> nil then
    begin
      MsgFrm.Position := poDefaultSizeOnly;
      MsgFrm.Left := AParent.Left + (AParent.Width - MsgFrm.Width) div 2;
      MsgFrm.Top := AParent.Top + (AParent.Height - MsgFrm.Height) div 2;
    end
    else
      MsgFrm.Position := poWorkAreaCenter;
    Result := MsgFrm.ShowModal;
  finally
    MsgFrm.Free
  end;
end;

{ TForm1 }

{$ifdef EnableLanguages}
procedure TForm1.Translate(const Language: string);
var
  PoFileName                    : string;
  aLanguage, FallbackLang       : string;
  //Lang                          : string;
begin
  aLanguage:=Language;

  //Lang:='';
  FallbackLang:='';
  //LazGetLanguageIDs(Lang,FallbackLang); // in unit LazUTF8

  if aLanguage='' then aLanguage:=FallbackLang;
  if aLanguage='' then aLanguage:='en';

  //AddMessage('Trying to set language to '+aLanguage);

  PoFileName:=SafeGetApplicationPath+'fpcupdeluxe.' + aLanguage + '.po';
  //SysUtils.DeleteFile(PoFileName);

  if (NOT FileExists(PoFileName)) then
  begin
    try
      if SaveFileFromResource(PoFileName,'fpcupdeluxe.' + aLanguage) then
      begin
        //AddMessage('Language file for '+aLanguage+' created');
      end;
    except
    end;
  end;

  if FileExists(PoFileName) then
  begin
    SetDefaultLang(aLanguage,SafeGetApplicationPath,'fpcupdeluxe');
    //AddMessage('Language file for '+aLanguage+' used');
  end;
end;
{$endif}

procedure TForm1.FormCreate(Sender: TObject);
var
  IniFilesOk:boolean;
  aSystemTarget:string;
  aFPCTarget,aLazarusTarget:string;
  bGitlab:boolean;
begin
  MessageTrigger:=false;

  IniPropStorageApp.IniFileName:=SafeGetApplicationPath+installerUniversal.DELUXEFILENAME;

  {$ifdef EnableLanguages}
  sLanguage:='en';
  {$endif}

  FPCupManager:=nil;

  {$IF defined(LCLQT) OR defined(LCLQT5)}
  // due to a bugger in QT[5]
  Self.Position:=poDesigned;
  {$endif}

  {$ifdef Darwin}
  CmdFontSize:=-11;
  CmdFontName:='Courier New';
  {$endif}

  {$ifdef RemoteLog}
  aDataClient:=TDataClient.Create;
  {$ifdef usealternateui}
  aDataClient.UpInfo.UpVersion:=DELUXEVERSION+'+';
  {$else}
  aDataClient.UpInfo.UpVersion:=DELUXEVERSION;
  {$endif}
  aDataClient.UpInfo.UpOS:=GetSourceCPUOS;
  {$endif}

  {$ifndef MSWINDOWS}
  btnGetOpenSSL.Visible:=false;
  {$endif}

  {$IF defined(Haiku) OR defined(AROS) OR defined(Morphos) OR (defined(CPUPOWERPC64) AND defined(FPC_ABI_ELFV2)) OR (defined(CPUPOWERPC) AND defined(Darwin)) OR (defined(CPUPOWERPC64) AND defined(Darwin))}
  // disable some features
  AndroidBtn.Visible:=False;
  {DinoBtn.Visible:=False;}
  CrossSheet.TabVisible:=false;
  {$endif}
  {$IF defined(CPUAARCH64) OR (defined(CPUPOWERPC64) AND defined(FPC_ABI_ELFV2))}
  // disable some features
  AndroidBtn.Visible:=False;
  {$endif}

  {$ifdef Darwin}
  radgrpOS.Items.Strings[radgrpOS.Items.IndexOf(GetOS(TOS.wince))]:='i-sim';
  {$ifndef CPUX86}
  //UltiboBtn.Enabled:=False;
  {$endif}
  {$endif Darwin}

  (*
  oldoutput := System.Output;
  AssignSynEdit(System.Output, CommandOutputScreen);
  Reset(System.Input);
  Rewrite(System.Output);
  *)

  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION > 30000)}
  aSystemTarget:=GetLCLWidgetTypeName;
  {$ELSE}
  aTarget:='';
  {$ENDIF}

  {$ifdef RemoteLog}
  aDataClient.UpInfo.UpWidget:=aSystemTarget;
  {$endif}

  Form1.Caption:=
  {$ifdef usealternateui}
  'FPCUPdeluxery V'+
  {$else}
  'FPCUPdeluxe V'+
  {$endif}
    DELUXEVERSION+
    ' for ' +
    GetSourceCPUOS+
    '-'+
    aSystemTarget;

  sStatus:='Sitting and waiting';

  InitShortCuts;

  {$IFDEF MSWINDOWS}
  sInstallDir:='C:\fpcupdeluxe';
  {$ELSE}
  sInstallDir:=ExpandFileName('~/fpcupdeluxe');
  btnGetOpenSSL.Visible:=False;
  {$ENDIF}

  {$ifdef Haiku}
  MenuItem3.Visible:=False;
  {$endif}

  //Prevent overwriting an existing install when starting with a new fpcupdeluxe install
  If DirectoryExists(sInstallDir) then
    sInstallDir:=SafeGetApplicationPath+'fpcupdeluxe';

  {$ifdef DARWIN}
  aSystemTarget:=ExcludeTrailingPathDelimiter(SafeGetApplicationPath);
  if AnsiEndsText('/Downloads',aSystemTarget) then
  begin
    // Many people try to run fpcupdeluxe from the Downloads folder.
    // That is NOT allowed.
    // Fpcupdeluxe must run inside its own folder, due to permission-issues on OSX
    AddMessage('FATAL ERROR !!!');
    AddMessage('');
    AddMessage(upDarwinFolderWarning1);
    AddMessage(upDarwinFolderWarning1);
    AddMessage(upDarwinFolderWarning1);
    AddMessage(upQuitRequest);
    DisEnable(nil,false);
    IniPropStorageApp.Active:=false;
    exit;
  end;
  {$endif}

  {$ifdef DARWIN}
  // we could have started from with an .app , so goto the basedir ... not sure if realy needed, but to be sure.
  AddMessage('Setting base directory to: '+ExcludeTrailingPathDelimiter(SafeGetApplicationPath));
  if (NOT SetCurrentDir(ExcludeTrailingPathDelimiter(SafeGetApplicationPath))) then
    AddMessage('Setting base directory failure !!')
  else
    AddMessage('Current base directory : '+GetCurrentDir);

  {$ifdef CPUAARCH64}
  AddMessage('It might be necessary (quarantine problem) to do: sudo xattr -r -d com.apple.quarantine fpcupdeluxe-aarch64-darwin-cocoa.app');
  {$endif}
  {$endif}

  aFPCTarget:='';
  aLazarusTarget:='';
  bGitlab:=true;

  // get last used install directory, proxy and visual settings
  with TIniFile.Create(SafeGetApplicationPath+installerUniversal.DELUXEFILENAME) do
  try
    sInstallDir:=ReadString('General','InstallDirectory',sInstallDir);

    // Read default FPC and Lazarus target from settings in app directory
    // Will be overwritten by settings in install directory if needed.
    bGitlab:=ReadBool('General','Gitlab',bGitlab);
    aFPCTarget:=ReadString('General','fpcVersion','');
    if (Length(aFPCTarget)=0) then
    begin
      aFPCTarget:='stable'+GITLABEXTENSION;
    end;
    aLazarusTarget:=ReadString('General','lazVersion','');
    if (Length(aLazarusTarget)=0) then
    begin
      aLazarusTarget:='stable'+GITLABEXTENSION;
      {$ifdef Haiku}
      {$ifdef CPUX86_64}
      aLazarusTarget:='trunk'+GITLABEXTENSION;
      {$endif}
      {$endif}
    end;
    {$ifdef EnableLanguages}
    sLanguage:=ReadString('General','Language',sLanguage);
    TransLate(sLanguage);
    {$endif}
    {$ifdef RemoteLog}
    sConsentWarning:=ReadBool('General','ConsentWarning',true);
    {$endif}
    CheckAutoClear.Checked:=ReadBool('General','AutoClear',True);
  finally
    Free;
  end;

  IniFilesOk:=
    (SaveInisFromResource(SafeGetApplicationPath+installerUniversal.SETTTINGSFILENAME,'settings_ini'))
    AND
    (SetConfigFile(SafeGetApplicationPath+installerUniversal.CONFIGFILENAME));

  aSystemTarget:='';
  if IniFilesOk then
  begin
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

    if (chkGitlab.Checked<>bGitlab) then chkGitlab.Checked:=bGitlab;
    if (Length(aFPCTarget)>0) then FPCTarget:=aFPCTarget;
    if (Length(aLazarusTarget)>0) then LazarusTarget:=aLazarusTarget;
    FillSourceListboxes;

    // create settings form
    // must be done here, to enable local storage/access of some setttings !!
    Form2:=TForm2.Create(Form1);
    Form3:=TForm3.Create(Form1);
    SubarchForm:=TSubarchForm.Create(Form1);
    InitFpcupdeluxe;
    Application.QueueAsyncCall(@ScrollToSelected,0);
    {$ifdef RemoteLog}
    Application.QueueAsyncCall(@InitConsent,0);
    {$endif}
  end
  else
  begin
    AddMessage('');
    AddMessage('FATAL ERROR !!!');
    AddMessage('');
    AddMessage(upFolderWarning1);
    AddMessage(upFolderWarning2);
    AddMessage('');
    AddMessage(upQuitRequest);
    AddMessage('');
    DisEnable(nil,False);
    IniPropStorageApp.Active:=false;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i:integer;
begin
  //if Assigned(Form3) then Form3.Destroy;
  //if Assigned(Form2) then Form2.Destroy;

  for i:=(listModules.Count-1) downto 0 do
  begin
    if Assigned(listModules.Items.Objects[i]) then
    begin
      StrDispose(Pchar(listModules.Items.Objects[i]));
    end;
  end;

  {$ifdef RemoteLog}
  if Assigned(aDataClient) then aDataClient.Destroy;
  {$endif}

  (* using CloseFile will ensure that all pending output is flushed *)
  (*
  //if (TTextRec(oldoutput).Handle=UnusedHandle) then
  begin
    CloseFile(System.Output);
    System.Output := oldoutput;
  end;
  *)
end;

{
procedure TForm1.FormResize(Sender: TObject);
var
  w:integer;
begin
  w:=(CommandOutputScreen.Width DIV 2);
  RealFPCURL.Width:=(w-4);
  RealLazURL.Width:=RealFPCURL.Width;
  RealLazURL.Left:=RealFPCURL.Left+(w+4);
end;
}

procedure TForm1.InitShortCuts;
begin
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
  actFileSave.ShortCut := KeyToShortCut(VK_S, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
  actFileSave.ShortCut := KeyToShortCut(VK_S, [ssAlt]);
{$ENDIF}
end;

procedure TForm1.ProcessInfo(Sender: TObject);
var
  s,searchstring  : string;
  x,y             : integer;
  Lines           : TStrings;
  Handled         : boolean;
procedure EchoInfo(info:string);
begin
  memoSummary.Lines.Append(info);
  Handled:=true;
end;
begin
  Handled:=false;

  {$ifdef READER}
  Lines:=TMemo(Sender).Lines;
  {$else}
  Lines:=TSynEdit(Sender).Lines;
  {$endif}

  s:=Lines[Pred(Lines.Count)];
  s:=Trim(s);
  if Length(s)=0 then exit;

  if (ExistWordInString(PChar(s),'error:',[soWholeWord])) OR (ExistWordInString(PChar(s),'fatal:',[soWholeWord])) then
  begin
    if (ExistWordInString(PChar(s),'fatal: Remote branch')) then
    begin
      EchoInfo('We have had a GIT branch failure. Should be non-fatal !');
    end
    else
    begin
      EchoInfo(BeginSnippet+' Start of compile error summary.');
      if (ExistWordInString(PChar(s),'fatal: internal error')) then
      begin
        x:=RPos(' ',s);
        if x>0 then
        begin
          InternalError:=Copy(s,x+1,MaxInt);
          EchoInfo('Compiler error: '+InternalError);
        end;
      end
      else if (ExistWordInString(PChar(s),'error: user defined')) then
      begin
        x:=Pos('error: user defined',LowerCase(s));
        if x>0 then
        begin
          x:=x+Length('error: user defined');
          InternalError:=Copy(s,x+2,MaxInt);
          EchoInfo('Configuration error: '+InternalError);
          x:=Pos('80 bit extended floating point',LowerCase(s));
          if x>0 then
          begin
            EchoInfo('Please use trunk that has 80-bit float type using soft float unit !');
          end;
        end;
      end
      else if (Pos('error: 256',lowercase(s))>0) AND (Pos('svn',lowercase(s))>0) then
      begin
        EchoInfo('We have had a SVN connection failure. Just start again !');
        EchoInfo(Lines[Pred(Lines.Count)-1]);
      end
      else if (ExistWordInString(PChar(s),'fatal:')) then
      begin
        EchoInfo(s);
        EchoInfo(Lines[Pred(Lines.Count)-1]);
      end
      else if (ExistWordInString(PChar(s),'error:')) then
      begin
        // check if "error:" at the end of the line.
        // if so:
        // the real error will follow on the next line(s).
        // and we have to wait for these lines (done somewhere else in this procedure) !!
        // if not, just print the error message.
        if (Pos('error:',lowercase(s))<>(Length(s)-Length('error:')+1)) then EchoInfo(s);
      end;
    end;

  end;

  if Handled then exit;

  if (Pos(BeginSnippet,s)=1) then
  //if ExistWordInString(PChar(s),BeginSnippet,[soWholeWord]) then
  begin
    if ExistWordInString(PChar(s),Seriousness[etWarning],[soWholeWord,soMatchCase]) then
    begin
      // repeat fpcupdeluxe warning
      EchoInfo(s);
    end;
    if (ExistWordInString(PChar(s),Seriousness[etDebug],[soWholeWord,soMatchCase])) then
    begin
      Handled:=true;
    end;
  end;

  if Handled then exit;

  if (ExistWordInString(PChar(s),'Error 217')) then
  begin
    EchoInfo('We have a fatal FPC runtime error 217: Unhandled exception occurred.');
    EchoInfo('See: https://www.freepascal.org/docs-html/user/userap4.html');
    EchoInfo('Most common cause: a stray fpc process still running.');
    EchoInfo('Please check the task manager for FPC or PPC processes that are still active.');
    EchoInfo('Re-running fpcupdeluxe does work in most cases however !. So, just do a restart.');
    s:=
    'We have a fatal FPC runtime error 217: Unhandled exception occurred.' + sLineBreak +
    'Most common cause: a stray fpc process still running.' + sLineBreak +
    'Please check the task manager for FPC or PPC processes that are still active.' + sLineBreak +
    'This sometime happens, due to causes unknown (to me) yet.' + sLineBreak +
    'Just quiting fpcupdeluxe and running it again will result in success.';
    Application.MessageBox(PChar(s), PChar('FPC runtime error 217'), MB_ICONSTOP);
  end;

  if Handled then exit;

  searchstring:='make (e=';
  if (ExistWordInString(PChar(s),searchstring)) then
  begin
    EchoInfo('Make has generated an error.');
    x:=Pos('): ',LowerCase(s));
    if x>0 then
    begin
      x:=x+3;
      InternalError:=Copy(s,x,MaxInt);
      EchoInfo('Make error: '+InternalError);
    end;

    //Get make error code
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      y:=0;
      while s[x] in ['0'..'9'] do
      begin
        y:=y*10+Ord(s[x])-$30;
        Inc(x);
      end;
      // if error=2 then most probable cause: bad checkout of sources.
      if y=2 then
      begin
        EchoInfo('Most probable cause: bad checkout of sources !');
        EchoInfo('Most successfull approach: delete sources and run again.');
      end;
    end;
  end;

  if Handled then exit;

  if (ExistWordInString(PChar(s),'Permission denied')) then
  begin
    EchoInfo(s);
  end;

  if Handled then exit;

  searchstring:='unable to connect to a repository at url';
  if (ExistWordInString(PChar(s),searchstring)) then
  begin
    EchoInfo('SVN could not connect to the desired repository.');
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      InternalError:=Copy(s,x+1,MaxInt);
      EchoInfo('URL: '+InternalError);
      EchoInfo('Please check your connection. Or run the SVN command to try yourself:');
      EchoInfo(Lines[Pred(Lines.Count)-1]);
    end;
  end;

  if Handled then exit;

  //Lazbuild error
  if (ExistWordInString(PChar(s),'Unable to open the package')) then
  begin
    EchoInfo(s);
    EchoInfo('Package source is missing. Please check your Lazarus config files.');
  end;

  if Handled then exit;

  // linker error
  if (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find')) then
  begin
    x:=Pos('-l',s);
    if x>0 then
    begin
      // add help into summary memo
      EchoInfo(BeginSnippet+' Missing library: lib'+Copy(s,x+2,MaxInt));
    end;
  end;

  if Handled then exit;

  // diskspace errors
  if (ExistWordInString(PChar(s),'Stream write error')) OR (ExistWordInString(PChar(s),'disk full')) then
  begin
    EchoInfo(BeginSnippet+' There is not enough diskspace to finish this operation.');
    EchoInfo(BeginSnippet+' Please free some space and re-run fpcupdeluxe.');
  end;

  if Handled then exit;

  // GLIBC hardening error
  if (ExistWordInString(PChar(s),'undefined reference to')) AND (ExistWordInString(PChar(s),'libc_csu')) then
  begin
    EchoInfo(BeginSnippet+' Might be GLIBC>=2.34 hardening error.');
    EchoInfo(BeginSnippet+' See: https://gitlab.com/freepascal.org/fpc/source/-/issues/39295');
  end;

  if Handled then exit;

  // RAM errors
  if (ExistWordInString(PChar(s),'call the assembler')) OR (ExistWordInString(PChar(s),'call the resource compiler')) then
  begin
    EchoInfo(BeginSnippet+' Most (99%) likely, there is not enough RAM (swap) to finish this operation.');
    EchoInfo(BeginSnippet+' Please add some RAM or swap-space (+1GB) and re-run fpcupdeluxe.');
  end;

  if Handled then exit;

  // warn for time consuming help files
  if (ExistWordInString(PChar(s),'writing')) AND (ExistWordInString(PChar(s),'pages...')) then
  begin
    EchoInfo('Busy with help files. Be patient: can be time consuming !!');
  end;

  if Handled then exit;

  // report about correct tools that are found and used
  //if (ExistWordInString(PChar(s),'found correct')) then
  //begin
  //  EchoInfo(s);
  //end;

  // warn about time consuming FPC and Lazarus operations
  if (
    (ExistWordInString(PChar(s),'downloadfromurl',[soWholeWord]))
    OR
    (ExistWordInString(PChar(s),'checkout',[soWholeWord])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord]))
    OR
    (ExistWordInString(PChar(s),'clone',[soWholeWord])) AND (ExistWordInString(PChar(s),'--recurse-submodules',[soWholeWord]))
  ) then
  begin
    EchoInfo(BeginSnippet+' '+Seriousness[etInfo]+' Performing SVN/GIT/HG/FTP/URL checkout/download. Please wait, could take some time.');
  end;

  if Handled then exit;

  searchstring:='checking out/updating';
  if (ExistWordInString(PChar(s),searchstring)) then
  begin
    x:=Pos(searchstring,LowerCase(s));
    if x>0 then
    begin
      x:=x+Length(searchstring);
      InternalError:=Copy(s,x+1,MaxInt);
      EchoInfo('Getting/updating '+InternalError);
    end;
  end;

  if Handled then exit;

  if (ExistWordInString(PChar(s),'switch',[soWholeWord])) AND (ExistWordInString(PChar(s),'--quiet',[soWholeWord])) then
  begin
    EchoInfo(BeginSnippet + ' Performing a SVN repo URL switch ... please wait, could take some time.');
  end;

  if Handled then exit;

  // github error
  if (ExistWordInString(PChar(s),'429 too many requests')) then
  begin
    EchoInfo('GitHub blocked us due to too many download requests.');
    EchoInfo('This will last for an hour, so please wait and be patient.');
    EchoInfo('After this period, please re-run fpcupdeluxe.');
  end;

  if Handled then exit;

  (*
  searchstring:='the makefile doesn''t support target';
  if (ExistWordInString(PChar(s),searchstring)) then
  begin
    EchoInfo('Sorry, but you have chosen a target that is not supported (yet).');
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
        EchoInfo('Wrong target: '+InternalError);
      end;
    end;
  end;

  if Handled then exit;

  *)

  if (ExistWordInString(PChar(s),URL_ERROR)) then
  begin
    s:=
    'Fpcupdeluxe encountered a (fatal) URL error.' + sLineBreak +
    'Most common cause: overwtiting an existing install.' + sLineBreak +
    'Sources with different URL cannot be installed in same directory.' + sLineBreak +
    'Please select an new install directory when changing versions.';
    Application.MessageBox(PChar(s), PChar('URL mismatch error'), MB_ICONSTOP);
  end;

  if Handled then exit;

  if (ExistWordInString(PChar(s),'Native Installer')) OR (ExistWordInString(PChar(s),'Cross Installer')) then
  begin
    EchoInfo(s);
  end;

  if Handled then exit;

  if ExistWordInString(PChar(s),'Installer',[soWholeWord,soMatchCase]) then
  begin
    if ( ExistWordInString(PChar(s),'BuildModule',[soMatchCase]) OR ExistWordInString(PChar(s),'ConfigModule',[soMatchCase]) OR ExistWordInString(PChar(s),'GetModule',[soMatchCase]) ) then
    begin
      EchoInfo(s);
    end;
  end;
end;

procedure TForm1.LazarusVersionLabelClick(Sender: TObject);
begin
  if MessageTrigger then
  begin
    MessageTrigger:=false;
    //Application.MessageBox(PChar(LOVEANDLIES),PChar(LOVEANDLIESHEADER), MB_ICONEXCLAMATION);
  end;
end;

procedure TForm1.listModulesSelectionChange(Sender: TObject; User: boolean);
var
  Index : integer;
  Item : string;
  aList:TListBox;
  aObject:TObject;
begin
  if (NOT User) then exit;
  aList:=TListBox(Sender);
  Memo1.Text:='';
  Index:=aList.ItemIndex;
  aObject:=aList.Items.Objects[Index];
  if Assigned(aObject) then
  begin
    Item:=PChar(aObject);
    Memo1.Text:=Item;
  end;
end;

procedure TForm1.listModulesShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  Index : integer;
  Item : string;
  aList:TListBox;
  aObject:TObject;
begin
  aList:=TListBox(Sender);
  Index:=aList.ItemAtPos(HintInfo^.CursorPos, True);
  if (HintInfo^.HintControl=aList) and (Index > -1) then
  begin
    aObject:=aList.Items.Objects[Index];
    if Assigned(aObject) then
    begin
      Item:=PChar(aObject);
      HintInfo^.HintStr:=Item;
      HintInfo^.CursorRect:=aList.ItemRect(Index);
    end;
  end;
end;

procedure TForm1.MFPCBugsClick(Sender: TObject);
begin
  OpenURL('https://gitlab.com/groups/freepascal.org/fpc/-/issues');
end;

procedure TForm1.MIssuesForumClick(Sender: TObject);
begin
  OpenURL('https://forum.lazarus.freepascal.org/index.php/topic,34645.0.html');
end;

procedure TForm1.MIssuesGitHubClick(Sender: TObject);
begin
  OpenURL('https://github.com/LongDirtyAnimAlf/fpcupdeluxe/issues');
end;

procedure TForm1.MLazarusBugsClick(Sender: TObject);
begin
  OpenURL('https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues');
end;

procedure TForm1.ButtonAutoUpdateCrossCompiler(Sender: TObject);
begin
  AutoUpdateCrossCompiler(Sender);
end;

function TForm1.AutoUpdateCrossCompiler(Sender: TObject):boolean;
var
  FPCCfg:string;
  ConfigPath,BinPath,LibPath:string;
  ConfigText: TStringList;
  aCPU, aOS: string;
  aTCPU:TCPU;
  aTOS:TOS;
  aTSUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  RebuildSubarchs:TSUBARCHS;
  CheckAutoClearStore:boolean;
  success:boolean;
  SnipBegin,SnipEnd: integer;
  i:integer;
  s,dummy,aResultMessage:string;
begin
  aOS := GetSourceOS;
  aCPU := GetSourceCPU;

  aTCPU:=TCPU.cpuNone;
  aTOS:=TOS.osNone;
  aTSUBARCH:=TSUBARCH.saNone;

  //ConfigPath:=ConcatPaths([FPCupManager.FPCInstallDirectory,'bin',aCPU+'-'+aOS]);
  ConfigPath:=ConcatPaths([sInstallDir,'fpc','bin',aCPU+'-'+aOS])+DirectorySeparator;

  FPCCfg:=ConfigPath+FPCCONFIGFILENAME;
  {$ifdef UNIX}
  if (NOT FileExists(FPCCfg)) then FPCCfg:=ExpandFileName(ConfigPath+'../etc/'+FPCCONFIGFILENAME);
  {$endif}

  result:=false;

  if (NOT FileExists(FPCCfg)) then
  begin
    if (Sender<>nil) then AddMessage('FPC configfile ['+FPCCONFIGFILENAME+'] not found in ' + ExtractFilePath(FPCCfg));
    exit;
  end;

  if (Sender<>nil) then
  begin
    s:=upUpdateCrossAll+' !' + sLineBreak + upQuestionContinue;
    if Form2.AskConfirmation then
      if (MessageDlg(s,mtConfirmation,[mbYes, mbNo],0)<>mrYes) then
        exit;
  end;

  if (Sender<>nil) then
  begin

    CheckAutoClearStore:=CheckAutoClear.Checked;
    if CheckAutoClearStore then btnClearLog.Click;
    CheckAutoClear.Checked:=false;

    memoSummary.Lines.Append(upBuildAllCrossCompilers);
    memoSummary.Lines.Append(upBuildAllCrossCompilersCheck+' '+ConfigPath);
    memoSummary.Lines.Append('');

  end
  else
  begin
    memoSummary.Clear;
  end;

  success:=true;

  aResultMessage:='No cross-compilers found. Nothing to do !';

  ConfigText:=TStringList.Create;
  try
    ConfigText.LoadFromFile(FPCCFG);

    SnipBegin:=0;
    SnipEnd:=0;
    while ((SnipBegin<>-1) AND (SnipEnd<>-1) AND (SnipBegin<ConfigText.Count)) do
    begin
      SnipBegin:=StringListStartsWith(ConfigText,SnipMagicBegin,SnipEnd);
      SnipEnd:=-1;
      if (SnipBegin<>-1) then
      begin
        SnipEnd:=SnipBegin+1;
        if (Pos('-',ConfigText.Strings[SnipBegin])>0) then
          SnipEnd:=StringListSame(ConfigText,SnipMagicEnd,SnipEnd)
      end;

      if (SnipBegin<>-1) AND (SnipEnd<>-1) then
      begin
        s:=ConfigText.Strings[SnipBegin];
        Delete(s,1,Length(SnipMagicBegin));
        i:=Pos('-',s);
        if (i>0) then
        begin
          aCPU:=Copy(s,1,i-1);
          aOS:=Trim(Copy(s,i+1,MaxInt));

          aTCPU:=GetTCPU(aCPU);
          aTOS:=GetTOS(aOS);

          // try to distinguish between different Solaris versons
          if (aTOS=TOS.solaris) then
          begin
            GetCrossToolsDir(aTCPU,aTOS,false,true,BinPath,dummy);

            for i:=SnipBegin to SnipEnd do
            begin
              s:=ConfigText.Strings[i];
              if (Pos('-FD',s)>0) AND (Pos(BinPath,s)>0) then
              begin
                aOS:='solaris-oi';
                break;
              end;
            end;
          end;

          // try to distinguish between different Linux versons
          if (aTOS=TOS.linux) then
          begin
            GetCrossToolsDir(aTCPU,aTOS,true,false,BinPath,dummy);
            GetCrossToolsDir(aTCPU,aTOS,false,false,dummy,LibPath);

            for i:=SnipBegin to SnipEnd do
            begin
              s:=ConfigText.Strings[i];
              if (Pos('-FD',s)>0) AND (Pos(BinPath,s)>0) then
              begin
                aOS:='linux-musl';
                break;
              end;
            end;
          end;

          if aTCPU=TCPU.powerpc then aCPU:='ppc';
          if aTCPU=TCPU.powerpc64 then aCPU:='ppc64';
          if aTOS=TOS.iphonesim then aOS:='i-sim';
          if ( (aTOS=TOS.win32) or (aTOS=TOS.win64) ) then aOS:='windows';

          // Get subarchs
          RebuildSubarchs:=[TSUBARCH.saNone];
          Subarchs:=GetSubarchs(aTCPU,aTOS);
          for aTSUBARCH in Subarchs do
          begin
            if (aTSUBARCH=saNone) then continue;
            for i:=(SnipBegin+6) to (SnipEnd-2) do
            begin
              s:=ConfigText.Strings[i];
              if (s='#IFDEF CPU'+UpperCase(GetSubarch(aTSUBARCH))) then
                Include(RebuildSubarchs,aTSUBARCH);
            end;
          end;

          for aTSUBARCH in RebuildSubarchs do
          begin
            //if (Sender=nil) then
            //  Form2.SetCrossAvailable(aTCPU,aTOS,aTSubArch,true);

            // Only build for subarch if we do have subarchs
            if (aTOS in SUBARCH_OS) AND (aTCPU in SUBARCH_CPU) AND (aTSUBARCH=saNone) then continue;

            if aTSUBARCH=saNone then
              AddMessage(upBuildAllCrossCompilersFound+' '+aCPU + '-' + aOS)
            else
              AddMessage(upBuildAllCrossCompilersFound+' '+aCPU + '-' + aOS+ '-' + GetSubarch(aTSUBARCH));

            // build compiler
            if (Sender<>nil) then
            begin
              {$ifdef READER}
              CommandOutputScreen.Clear;
              {$else}
              CommandOutputScreen.ClearAll;
              {$endif}

              aResultMessage:='Finished building of cross-compilers.';
              AddMessage(upBuildAllCrossCompilersUpdate);

              // Set subarch
              SetSelectedSubArch(aTCPU,aTOS,aTSUBARCH);

              // Set CPU and OS
              radgrpCPU.ItemIndex:=radgrpCPU.Items.IndexOf(aCPU);
              radgrpOS.ItemIndex:=radgrpOS.Items.IndexOf(aOS);

              // Perpare !!
              if (NOT PrepareRun(nil)) then exit;

              FPCupManager.CrossCPU_Target:=aTCPU;
              FPCupManager.CrossOS_Target:=aTOS;

              // Build !!
              success:=ProcessCrossCompiler(nil);

              if success
                then memoSummary.Lines.Append('Cross-compiler update ok.')
                else memoSummary.Lines.Append('Failure during update of cross-compiler !!');
              memoSummary.Lines.Append('');
              if (NOT success) then
              begin
                aResultMessage:='Building of cross-compilers ended with some error(s).';
                break;
              end;
            end;
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

  if (Sender<>nil) then
  begin
    AddMessage('');
    AddMessage(aResultMessage,True);
  end;
end;

procedure TForm1.InitFPCupManager;
var
  ModulesList: TStringList;
  SelectedModulesList: TStringList;
  i:integer;
  s,v:string;
  SettingsSuccess:boolean;
begin
  FPCupManager:=TFPCupManager.Create;
  FPCupManager.ConfigFile:=SafeGetApplicationPath+installerUniversal.CONFIGFILENAME;

  FPCupManager.LoadFPCUPConfig;

  FPCupManager.FPCURL:='stable';
  FPCupManager.LazarusURL:='stable';

  FPCupManager.Verbose:=false;

  //CheckFPCUPOptions(FPCupManager);

  if listModules.Count=0 then
  begin
    ModulesList:=TStringList.Create;
    SelectedModulesList:=TStringList.Create;
    try
      ModulesList.Delimiter:=_SEP;
      ModulesList.StrictDelimiter:=true;
      ModulesList.DelimitedText:=GetModuleList;
      // filter modulelist from trivial entries
      for i:=0 to Pred(ModulesList.Count) do
      begin
        s:=ModulesList[i];
        if Pos(_DECLARE,s)=0 then
          continue;
        if (AnsiStartsText(_DECLARE+_SUGGESTED,s)) OR (AnsiStartsText(_DECLARE+_SUGGESTEDADD,s)) then
          continue;
        if (AnsiEndsText(_CLEAN,s)) OR (AnsiEndsText(_UNINSTALL,s)) OR (AnsiEndsText(_BUILD+_ONLY,s)) then
          continue;
        SelectedModulesList.Append(s);
      end;
      SelectedModulesList.Sort;
      for i:=0 to (SelectedModulesList.Count-1) do
      begin
        s:=SelectedModulesList[i];
        Delete(s,1,Length(_DECLARE));
        // get module descriptions
        v:=FPCupManager.ModulePublishedList.Values[s];
        // add to list
        listModules.Items.AddObject(s,TObject(pointer(StrNew(Pchar(v)))));
      end;
    finally
      SelectedModulesList.Free;
      ModulesList.Free;
    end;
  end;

  FPCupManager.HTTPProxyPort:=Form2.HTTPProxyPort;
  FPCupManager.HTTPProxyHost:=Form2.HTTPProxyHost;
  FPCupManager.HTTPProxyUser:=Form2.HTTPProxyUser;
  FPCupManager.HTTPProxyPassword:=Form2.HTTPProxyPass;

  // localize FPCUPSettings if possible

  SettingsSuccess:=GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir));
  if (NOT SettingsSuccess) then SettingsSuccess:=GetFPCUPSettings(SafeGetApplicationPath);

  if (SettingsSuccess and DirectoryExists(sInstallDir)) then
  begin
    s:=ConcatPaths([sInstallDir,'fpcsrc']);
    if DirectoryExists(s) then
      FPCupManager.FPCSourceDirectory:=s;
    s:=ConcatPaths([sInstallDir,'lazarus']);
    if DirectoryExists(s) then
      FPCupManager.LazarusSourceDirectory:=s;
  end;
end;


procedure TForm1.BitBtnHaltClick(Sender: TObject);
begin
  if (MessageDlgEx('I am going to try to halt.' + sLineBreak +
             'Do not (yet) expect too much of it.' + sLineBreak +
             'Its a non-finished feature !'
             ,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
             begin
               exit;
             end;
  if Assigned(FPCupManager.Sequencer) then
  begin
    FPCupManager.Sequencer.Kill;
  end;
end;

procedure TForm1.btnGetOpenSSLClick(Sender: TObject);
{$ifdef MSWindows}
var
  OpenSSLZip,OpenSSLURL:string;
  Success:boolean;
  {$endif MSWindows}
begin
  {$ifdef MSWindows}
  OpenSSLURL:=OpenSSLSourceURL[Low(OpenSSLSourceURL)];
  OpenSSLZip:=IncludeTrailingPathDelimiter(GetWindowsDownloadFolder)+FileNameFromURL(OpenSSLURL);
  SysUtils.Deletefile(OpenSSLZip);
  Success:=OpenURL(OpenSSLURL);
  if (NOT Success) then
  begin
    OpenSSLURL:=OpenSSLSourceURL[High(OpenSSLSourceURL)];
    OpenSSLZip:=IncludeTrailingPathDelimiter(GetWindowsDownloadFolder)+FileNameFromURL(OpenSSLURL);
    SysUtils.Deletefile(OpenSSLZip);
    Success:=OpenURL(OpenSSLURL);
  end;
  if Success then
  begin
    sleep(5000); // give browser some time to finish
    if FileExists(OpenSSLZip) then
    begin
      with TNormalUnzipper.Create do
      begin
        try
          SysUtils.Deletefile(SafeGetApplicationPath+Crypto_DLL_Name+GetLibExt);
          if GetLastOSError<>5 then // no access denied
          begin
            SysUtils.Deletefile(SafeGetApplicationPath+SSL_DLL_Name+GetLibExt);
            if GetLastOSError<>5 then // no access denied
            begin
              if DoUnZip(OpenSSLZip,SafeGetApplicationPath,[Crypto_DLL_Name+GetLibExt,SSL_DLL_Name+GetLibExt]) then
              begin
                AddMessage('Success: got OpenSSL library dll by browser!');
              end;
            end;
          end;
        finally
          Free;
        end;
      end;
      SysUtils.Deletefile(OpenSSLZip);
    end;
  end;
  {$endif MSWindows}
end;

procedure TForm1.btnCreateLazarusConfigClick(Sender: TObject);
begin
  if (NOT PrepareRun(Sender)) then exit;
  FPCupManager.OnlyModules:=_CONFIG+_LAZARUS;
  sStatus:='Going to create basic Lazarus config only.';
  RealRun;
end;

procedure TForm1.ChkMakefileFPCClick(Sender: TObject);
begin
  DisEnable(Sender,False);

  try

    if (Sender=ChkMakefileLaz) OR (Sender=ChkMakefileFPC) then
    begin
      if (NOT PrepareRun(Sender)) then exit;
      if Sender=ChkMakefileLaz then FPCupManager.OnlyModules:=_MAKEFILECHECKLAZARUS;
      if Sender=ChkMakefileFPC then FPCupManager.OnlyModules:=_MAKEFILECHECKFPC;
      sStatus:='Going to check Makefile.';
      {$ifdef RemoteLog}
      aDataClient.UpInfo.UpFunction:=ufCheckMakefile;
      {$endif}
      RealRun;
    end;

    if Sender=CreateStartup then
    begin
      if (NOT PrepareRun(Sender)) then exit;
      FPCupManager.OnlyModules:=_CREATESCRIPT;
      sStatus:='Going to create startup scripts.';
      {$ifdef RemoteLog}
      aDataClient.UpInfo.UpFunction:=ufCreateStartup;
      {$endif}
      RealRun;
    end;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TForm1.CommandOutputScreenMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  {$if defined(Darwin) or defined(macOS) or defined(iphonesim) or defined(ios)}
  if ssMeta in Shift then
  {$else}
  if ssCtrl in Shift then
  {$endif}
  //if ssModifier in Shift then // as defined in controls.pp
  begin
    if (WheelDelta>0) AND (CommandOutputScreen.Font.Size<48) then CommandOutputScreen.Font.Size:=CommandOutputScreen.Font.Size+1;
    if (WheelDelta<0)  AND (CommandOutputScreen.Font.Size>2) then CommandOutputScreen.Font.Size:=CommandOutputScreen.Font.Size-1;
  end;
end;

{$ifndef READER}
procedure TForm1.CommandOutputScreenSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
var
  FG, BG: TColor;
  s:string;
begin
  s:=CommandOutputScreen.Lines[Line-1];
  s:=Trim(s);
  Special:=False;
  if Length(s)=0 then exit;

  // special override for me: for easy debugging FPC and Lazarus source with plain writelines in source
  if ExistWordInString(PChar(s),'donalf:',[soWholeWord]) then
  begin
    begin
      FG      := clBlack;
      BG      := clYellow;
      Special := True;
    end;
  end;

  if ((NOT Special) AND (Pos(BeginSnippet,s)=1)) then
  begin
    if ExistWordInString(PChar(s),Seriousness[etInfo],[soWholeWord,soMatchCase]) then
    begin
      if ExistWordInString(PChar(s),'Client: ',[soMatchCase]) then
        FG      := clFuchsia
      else
      if ExistWordInString(PChar(s),'found correct',[soWholeWord]) then
        FG      := clLime
      else
        FG      := clYellow;
      BG      := clBlack;
      Special := True;
    end;
    if ExistWordInString(PChar(s),Seriousness[etWarning],[soWholeWord,soMatchCase]) then
    begin
      FG      := clFuchsia;
      BG      := clBlack;
      Special := True;
    end;
    if ExistWordInString(PChar(s),Seriousness[etError],[soWholeWord,soMatchCase]) then
    begin
      FG      := clRed;
      BG      := clWhite;
      Special := True;
    end;
  end;

  if (NOT Special)
  AND
  (
    (AnsiStartsStr('FPCupdeluxe basedir:',s))
    OR
    (AnsiStartsStr('FPC URL:',s))
    OR
    (AnsiStartsStr('Lazarus URL:',s))
  )
  then
  begin
    FG      := clRed;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'Found valid',[soWholeWord]) then
  begin
    FG      := clOlive;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'executing:',[soWholeWord]) then
  begin
    FG      := clAqua;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'A',[soWholeWord,soMatchCase])) OR (ExistWordInString(PChar(s),'U',[soWholeWord,soMatchCase]))) then
  begin
    FG      := clSkyBlue;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'HEAD is now at ')) OR (ExistWordInString(PChar(s),'Last Changed '))) then
  begin
    FG      := clMoneyGreen;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'failed:',[soWholeWord]) then
  begin
    FG      := TColor($FF00AF); //Text Color  BGR
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'hint:',[soWholeWord])) OR (ExistWordInString(PChar(s),'note:',[soWholeWord]))) then
  begin
    FG      := clGreen;
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ((ExistWordInString(PChar(s),'warning:',[soWholeWord]))) then
  begin
    FG      := clMaroon;
    BG      := clBlack;
    Special := True;
  end;

  // linker error
  if (NOT Special) AND (ExistWordInString(PChar(s),'/usr/bin/ld: cannot find')) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
  end;

  // override warning
  if (NOT Special) AND (ExistWordInString(PChar(s),'You have overriden the starting compiler versioncheck')) then
  begin
    FG      := clYellow;
    BG      := clGreen;
    Special := True;
  end;

  // diskspace error
  if (NOT Special) AND ((ExistWordInString(PChar(s),'Stream write error')) OR (ExistWordInString(PChar(s),'disk full'))) then
  begin
    FG      := clRed;
    BG      := clAqua;
    Special := True;
  end;

  // github error
  if (NOT Special) AND (ExistWordInString(PChar(s),'429 too many requests')) then
  begin
    FG      := clRed;   //Text Color
    BG      := clNavy;  //BackGround
    Special := True;    //Must be true
  end;

  // makefile and help warnings
  if (NOT Special)
  AND
  (
    (ExistWordInString(PChar(s),'lines compiled,'))
    OR
    (ExistWordInString(PChar(s),'issued',[soWholeWord]))
    OR
    (ExistWordInString(PChar(s),'Target OS: '))
    OR
    (ExistWordInString(PChar(s),'make.exe: '))
    OR
    (ExistWordInString(PChar(s),'make: '))
    OR
    (ExistWordInString(PChar(s),'make['))
    OR
    (ExistWordInString(PChar(s),'dependency dropped'))
    OR
    ( ( ExistWordInString(PChar(s),'echo.exe ') OR ExistWordInString(PChar(s),'echo ') OR ExistWordInString(PChar(s),'gecho.exe ') OR ExistWordInString(PChar(s),'gecho ') ) AND ExistWordInString(PChar(s),REVINCFILENAME) )
    OR
    ( ExistWordInString(PChar(s),'Start ') AND ExistWordInString(PChar(s),'now ') )
    OR
    (ExistWordInString(PChar(s),'this could take some time'))
    OR
    (ExistWordInString(PChar(s),'Skipped package',[soWholeWord]))
    OR
    (ExistWordInString(PChar(s),'Processing Makefile.fpc'))
    OR
    (
      (ExistWordInString(PChar(s),'writing'))
      AND
      (ExistWordInString(PChar(s),'pages...'))
    )
  )
  then
  begin
    FG      := TColor($AF10FF); //Text Color  BGR
    BG      := clBlack;
    Special := True;
  end;

  // svn connection error
  if (NOT Special) AND (ExistWordInString(PChar(s),'unable to connect to a repository at url')) then
  begin
    FG      := clRed;
    BG      := clNavy;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'svn: e') then
  begin
    FG      := clFuchsia;
    BG      := clBlack;
    Special := True;
  end;

  if (ExistWordInString(PChar(s),'make (e=')) then
  begin
    // make fatal messages ...
    begin
      FG      := clRed;
      BG      := clBlue;
      Special := True;
    end;
  end;

  if (ExistWordInString(PChar(s),'Permission denied')) then
  begin
    // Problem when cleaning ... exe still running
    begin
      FG      := clRed;
      BG      := clTeal;
      Special := True;
    end;
  end;

  if (NOT Special) AND (ExistWordInString(PChar(s),'make.exe ') OR ExistWordInString(PChar(s),'make ') OR ExistWordInString(PChar(s),'gmake ')) then
  begin
    if ExistWordInString(PChar(s),'fpmake ') then
      FG      := TColor($FFA000)
    else
      FG      := TColor($FF8C00);
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'start compiling package') then
  begin
    FG      := TColor($FFA000);
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'success:',[soWholeWord]) then
  begin
    FG      := TColor($00D7FF);
    BG      := clBlack;
    Special := True;
  end;


  if (NOT Special) AND (ExistWordInString(PChar(s),'compiled package') OR ExistWordInString(PChar(s),'succeeded') OR ExistWordInString(PChar(s),'completed')) then
  begin
    FG      := TColor($00A5FF);
    BG      := clBlack;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'Extracted #') then
  begin
    BG      := clBlack;
    FG      := clSilver;
    Special := True;
  end;

  if (NOT Special) AND ExistWordInString(PChar(s),'Download progress ') then
  begin
    BG      := clBlack;
    FG      := TColor($0045FF);
    Special := True;
  end;

  // special override for debugging statemachine and tools availability
  if ((ExistWordInString(PChar(s),'sequencer',[soWholeWord])) OR (ExistWordInString(PChar(s),'libs: none')) OR (ExistWordInString(PChar(s),'bins: none'))) then
  begin
    begin
      FG      := clRed;
      BG      := clBlack;
      Special := True;
    end;
  end;

  // lazbuild error
  if (NOT Special) AND (ExistWordInString(PChar(s),'Unable to open the package') OR ExistWordInString(PChar(s),'Unable to load package')) then
  begin
    FG      := clRed;
    BG      := clPurple;
    Special := True;
  end;

  if (ExistWordInString(PChar(s),'error:',[soWholeWord])) OR (ExistWordInString(PChar(s),'fatal:',[soWholeWord])) OR (ExistWordInString(PChar(s),'Memory warning:',[soWholeWord])) then
  begin
    begin
      FG      := TColor($0060FF);
      BG      := TColor($402000);
      Special := True;
    end;
  end;

  if Special then
  begin
    Markup.Background:=BG;
    Markup.Foreground:=FG;
  end;

  {$ifdef usealternateui}
  if Special
     then alternateui_AddMessage(s,false,FG)
     else alternateui_AddMessage(s,false,clLime);
  {$endif}
end;
{$endif}

procedure TForm1.PageControl1Change(Sender: TObject);
var
  aFileList:TStringList;
  aResultCode: longint;
  Output:string;
  GitExe:string;
  GITTagCombo:string;
  GITTag:string;
  i:integer;
begin
  if TPageControl(Sender).ActivePage=ModuleSheet then
  begin
    if (listModules.HandleAllocated) AND (listModules.ItemIndex>8) then listModules.MakeCurrentVisible;
  end;

  if TPageControl(Sender).ActivePage=TagSheet then
  begin
    Application.ProcessMessages;
    aFileList:=TStringList.Create;
    aFileList.Delimiter:=#10;
    aFileList.StrictDelimiter:=true;
    ListBoxFPCTargetTag.Items.BeginUpdate;
    ListBoxLazarusTargetTag.Items.BeginUpdate;
    try
      GitExe:=Which('git'+GetExeExt);
      {$ifdef MSWindows}
      if (NOT FileExists(GitExe)) then
      begin
        GitExe:=ConcatPaths([FPCupManager.MakeDirectory,'git','cmd'])+PathSeparator+'git.exe';
      end;
      {$endif}

      if FileExists(GitExe) then
      begin

        ListBoxFPCTargetTag.Items.Clear;
        ListBoxLazarusTargetTag.Items.Clear;

        aFileList.Clear;
        RunCommandIndir('',GitExe,['ls-remote','--tags','--sort=-v:refname',FPCGITLABREPO+'.git','?.?.?'], Output, aResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
        aFileList.DelimitedText:=Output;
        for GITTagCombo in aFileList do
        begin
          i:=Pos(#9,GITTagCombo);
          GITTag:=Copy(GITTagCombo,i+1,MaxInt);
          Delete(GITTag,1,Length('refs/tags/'));
          ListBoxFPCTargetTag.Items.Append(GITTag);
        end;
        aFileList.Clear;

        RunCommandIndir('',GitExe,['ls-remote','--tags','--sort=-v:refname',LAZARUSGITLABREPO+'.git','*_RC?'], Output, aResultCode,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
        aFileList.DelimitedText:=Output;
        for GITTagCombo in aFileList do
        begin
          i:=Pos(#9,GITTagCombo);
          GITTag:=Copy(GITTagCombo,i+1,MaxInt);
          Delete(GITTag,1,Length('refs/tags/'));
          ListBoxLazarusTargetTag.Items.Append(GITTag);
        end;
        aFileList.Clear;

        //Do this only once !!
        TPageControl(Sender).OnChange:=nil;
      end;

    finally
      aFileList.Free;
      ListBoxLazarusTargetTag.Items.EndUpdate;
      ListBoxFPCTargetTag.Items.EndUpdate;
    end;
  end;
end;

procedure TForm1.OnlyTagClick(Sender: TObject);
var
  aListBox:TListBox;
begin
  if (Sender=BitBtnFPCOnlyTag) then aListBox:=ListBoxFPCTargetTag;
  if (Sender=BitBtnLazarusOnlyTag) then aListBox:=ListBoxLazarusTargetTag;
  if (aListBox.ItemIndex<>-1) then AddTag(aListBox,aListBox.GetSelectedText);
end;

procedure TForm1.AddTag(Sender: TObject;aTag:string);
begin
  if (Sender=ListBoxFPCTargetTag) OR (Sender=ListBoxFPCTarget)  then
  begin
    if SetAlias(FPCTAGLOOKUPMAGIC,aTag+'.gitlab',aTag) then
    begin
      ListBoxFPCTarget.Items.CommaText:=installerUniversal.GetAlias(FPCURLLOOKUPMAGIC,'list');
      MemoAddTag.Lines.Clear;
      MemoAddTag.Lines.Add('The tag with name ['+aTag+'] was added to the FPC sources list.');
      //ListBoxFPCTarget.ItemIndex:=ListBoxFPCTarget.Count-1;
    end;
  end;
  if (Sender=ListBoxLazarusTargetTag) OR (Sender=ListBoxLazarusTarget) then
  begin
    if SetAlias(LAZARUSTAGLOOKUPMAGIC,aTag+'.gitlab',aTag) then
    begin
      ListBoxLazarusTarget.Items.CommaText:=installerUniversal.GetAlias(LAZARUSURLLOOKUPMAGIC,'list');
      MemoAddTag.Lines.Clear;
      MemoAddTag.Lines.Add('The tag with name ['+aTag+'] was added to the Lazarus sources list.');
      //ListBoxLazarusTarget.ItemIndex:=ListBoxLazarusTarget.Count-1;
    end;
  end;

  FillSourceListboxes;
  ScrollToSelected;
end;


procedure TForm1.TagSelectionChange(Sender: TObject;User: boolean);
begin
  MemoAddTag.Lines.Clear;
  MemoAddTag.Lines.Add(TListBox(Sender).GetSelectedText);
end;

procedure TForm1.btnUpdateLazarusMakefilesClick(Sender: TObject);
begin
end;

procedure TForm1.btnBuildNativeCompilerClick(Sender: TObject);
var
  CPUType:TCPU;
  OSType:TOS;
  sOS:string;
  success:boolean;
begin
  CPUType:=TCPU.cpuNone;
  OSType:=TOS.osNone;
  if (radgrpOS.ItemIndex<>-1) then
    OSType:=GetTOS(radgrpOS.Items[radgrpOS.ItemIndex]);
  if (radgrpCPU.ItemIndex<>-1) then
    CPUType:=GetTCPU(radgrpCPU.Items[radgrpCPU.ItemIndex]);

  if ((OSType=win32) AND (CPUType in [x86_64,aarch64])) then OSType:=win64;

  if ((CPUType<>TCPU.cpuNone) AND (OSType<>TOS.osNone)) then
  begin
    DisEnable(Sender,False);
    try
      success:=PrepareRun(Sender);
      if (NOT success) then exit;

      success:=FPCupManager.CheckCurrentFPCInstall;
      if (NOT success) then
      begin
        ShowMessage('No valid FPC install found. Please install FPC first.');
        exit;
      end;

      FPCupManager.CrossCPU_Target:=CPUType;
      FPCupManager.CrossOS_Target:=OSType;
      sOS:=radgrpOS.Items[radgrpOS.ItemIndex];
      if sOS='linux-musl' then FPCupManager.MUSL:=true;
      if sOS='solaris-oi' then FPCupManager.SolarisOI:=true;
      FPCupManager.OnlyModules:=_NATIVECROSSFPC;
      sStatus:='Going to build native compiler for '+FPCupManager.CrossCombo_Target;
      RealRun;
      FPCupManager.CrossCPU_Target:=TCPU.cpuNone;
      FPCupManager.CrossOS_Target:=TOS.osNone;
    finally
      DisEnable(Sender,True);
    end;
  end;
end;

procedure TForm1.IniPropStorageAppRestoringProperties(Sender: TObject);
begin
  {$ifdef Haiku}
  SessionProperties := 'CmdFontSize;CmdFontName;';
  {$else}
  SessionProperties := 'WindowState;Width;Height;Top;Left;CmdFontSize;CmdFontName;';
  {$endif}

  {$ifdef Windows}
  // For now, only Windows
  if Application.Scaled and Scaled and (Screen<>nil) and (PixelsPerInch<>Screen.PixelsPerInch) then
    AutoAdjustLayout(lapAutoAdjustForDPI, PixelsPerInch, Screen.PixelsPerInch, 0, 0);
  {$endif}
end;

procedure TForm1.IniPropStorageAppSavingProperties(Sender: TObject);
begin
  {$ifdef Haiku}
  SessionProperties := 'CmdFontSize;CmdFontName;'
  {$else}
  if Self.WindowState=wsMaximized then
    SessionProperties := 'WindowState;CmdFontSize;CmdFontName;'
  else
    SessionProperties := 'WindowState;Width;Height;Top;Left;CmdFontSize;CmdFontName;';
  {$endif}
end;

procedure TForm1.ListBoxTargetDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  i:integer;
  s:string;
begin
  with Control as TListBox do
  begin
    if Assigned(Font) then
    begin
      Canvas.Font := Font;
      Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
    end;
    if Assigned(Brush) then
      Canvas.Brush := Brush;
    if ((Index<>-1) AND (odSelected in State)) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end
    else
    begin
      Canvas.Brush.Color := GetColorResolvingParent;
      Canvas.Font.Color := clWindowText;
    end;

    if (odFocused in State) and (lboDrawFocusRect in Options) then
      Canvas.DrawFocusRect(ARect);
    if (NOT (odBackgroundPainted in State)) then
      Canvas.FillRect(ARect);
    if (Index<>-1) then
    begin
      s:=Items[Index];
      i:=Pos('.svn',s);
      if (i=0) then
        i:=Pos(GITLABEXTENSION,s)
      else
        Canvas.Font.Color := clRed;
      if (i>0) then Delete(s,i,MaxInt);
      {if ((odDisabled in State) OR (odGrayed in State)) then}
      Canvas.Brush.Style:=bsClear;
      Canvas.TextOut(ARect.Left + 2, ARect.Top, s);
    end;
  end;
end;

procedure TForm1.LanguageClick(Sender: TObject);
begin
  {$ifdef EnableLanguages}
  if ((Sender=nil) OR (Sender=MEnglishlanguage)) then sLanguage:='en';
  if Sender=MChineseCNlanguage then sLanguage:='zh';
  if Sender=MKoreanlanguage then sLanguage:='ko';
  if Sender=MGermanlanguage then sLanguage:='de';
  if Sender=MFrenchlanguage then sLanguage:='fr';
  if Sender=MDutchlanguage then sLanguage:='nl';
  if Sender=MUkrainianLanguage then sLanguage:='uk';

  TransLate(sLanguage);

  // Refresh welcome screen
  GetSystemInfo;

  // This is needed to update the contents of the options list
  Form2.UpdateCheckBoxList;
  {$endif}
end;

procedure TForm1.MOnlineDocsClick(Sender: TObject);
begin
  OpenURL('https://dsiders.gitlab.io/lazdocsnext');
end;

procedure TForm1.WikiClick(Sender: TObject);
begin
  OpenURL('https://wiki.freepascal.org/fpcupdeluxe');
end;

procedure TForm1.radgrpTargetChanged(Sender: TObject);
var
  CPUType:TCPU;
  OSType:TOS;
begin
  CPUType:=TCPU.cpuNone;
  OSType:=TOS.osNone;
  if (radgrpOS.ItemIndex<>-1) then
    OSType:=GetTOS(radgrpOS.Items[radgrpOS.ItemIndex]);
  if (radgrpCPU.ItemIndex<>-1) then
    CPUType:=GetTCPU(radgrpCPU.Items[radgrpCPU.ItemIndex]);
  ButtonSubarchSelect.Enabled:=((OSType in SUBARCH_OS) AND (CPUType in SUBARCH_CPU));
end;

procedure TForm1.actFileSaveAccept(Sender: TObject);
begin
  CommandOutputScreen.Lines.SaveToFile(actFileSave.Dialog.FileName);
end;

procedure TForm1.BitBtnSetRevisionClick(Sender: TObject);
var
  valid:boolean;
begin
  valid:=false;
  if (Sender=BitBtnFPCSetRevision) then
  begin
    if Assigned(ListBoxFPCHistoryNew.Selected) then
    begin
      Form2.ForceFPCRevision:=ListBoxFPCHistoryNew.Selected.Caption;
      valid:=true;
    end;
  end;
  if (Sender=BitBtnLazarusSetRevision) then
  begin
    if Assigned(ListBoxLazarusHistoryNew.Selected) then
    begin
      Form2.ForceLazarusRevision:=ListBoxLazarusHistoryNew.Selected.Caption;
      valid:=true;
    end;
  end;
  if valid then
    btnSetupPlusClick(nil);
end;

procedure TForm1.btnCheckToolsLocationsClick(Sender: TObject);
var
  aCPU:TCPU;
  aOS:TOS;
  success:boolean;
  BinsFileName,LibsFileName,BaseBinsURL,BaseLibsURL,BinPath,LibPath:string;
begin
  AddMessage('Please be patient. Might take some time to scan all libs and bins.');
  AddMessage('');
  for aOS := Low(TOS) to High(TOS) do
  begin
    if aOS=osNone then continue;
    for aCPU := Low(TCPU) to High(TCPU) do
    begin
      if aCPU=cpuNone then continue;
      FPCupManager.CrossCPU_Target:=aCPU;
      FPCupManager.CrossOS_Target:=aOS;
      FPCupManager.GetCrossToolsFileName(BinsFileName,LibsFileName);
      FPCupManager.GetCrossToolsPath(BinPath,LibPath);
      success:=FPCupManager.GetCrossBinsURL(BaseBinsURL,BinsFileName);
      {
      if (NOT success) then
      begin
        BaseBinsURL:='none';
        success:=true;
      end;
      }
      if success then
      begin
        AddMessage(FPCupManager.CrossCombo_Target+' bins: '+BaseBinsURL);
        success:=FPCupManager.GetCrossLibsURL(BaseLibsURL,LibsFileName);
        if (NOT success) then
        begin
          BaseLibsURL:='none';
          success:=true;
        end;
        if success then AddMessage(FPCupManager.CrossCombo_Target+' libs: '+BaseLibsURL);
      end;
    end;
  end;
  FPCupManager.CrossCPU_Target:=TCPU.cpuNone;
  FPCupManager.CrossOS_Target:=TOS.osNone;

  AddMessage('');
  AddMessage('Scanning libs and bins ready.');
end;

procedure TForm1.QuickBtnClick(Sender: TObject);
var
  s:string;
  aFPCTarget:string;
  aLazarusTarget:string;
  aModule:string;
  success:boolean;
  aCPU:TCPU;
  aOS:TOS;
  aSUBARCH:TSUBARCH;
begin
  s:='';

  aFPCTarget:='';
  aLazarusTarget:='';
  aModule:='';

  if Sender=TrunkBtn then
  begin
    s:=Format(upInstallConfimation,['FPC trunk','Lazarus trunk','']);
    aFPCTarget:='trunk'+GITLABEXTENSION;
    aLazarusTarget:='trunk'+GITLABEXTENSION;
  end;

  if Sender=FixesBtn then
  begin
    s:=Format(upInstallConfimation,['FPC fixes','Lazarus fixes','']);
    aFPCTarget:='fixes'+GITLABEXTENSION;
    aLazarusTarget:='fixes'+GITLABEXTENSION;
  end;

  if Sender=StableBtn then
  begin
    s:=Format(upInstallConfimation,['FPC stable','Lazarus stable','']);
    aFPCTarget:='stable'+GITLABEXTENSION;
    aLazarusTarget:='stable'+GITLABEXTENSION;
  end;

  if Sender=WABtn then
  begin
    s:=Format(upInstallConfimation,['FPC trunk','Lazarus trunk',' + cross WebAssembly compiler + tools']);
    aFPCTarget:='trunk'+GITLABEXTENSION;
    aLazarusTarget:='trunk'+GITLABEXTENSION;
    aModule:='pas2js-rtl';
  end;

  {
  if Sender=Win95Btn then
  begin
    s:='Going to install FPC and Lazarus for Win95.';
    aFPCTarget:='2.6.2'+GITLABEXTENSION;
    aLazarusTarget:='1.2'+GITLABEXTENSION;
  end;
  }

  {
  if Sender=OldBtn then
  begin
    //s:='Going to install FPC 2.6.4 and Lazarus 1.4.';
    //aFPCTarget:='2.6.4';
    //aLazarusTarget:='1.4';
    s:='Going to install FPC 3.0.4 and Lazarus 1.8.4.';
    aFPCTarget:='3.0.4';
    aLazarusTarget:='1.8.4';
  end;
  }

  if Sender=AndroidBtn then
  begin
    s:=Format(upInstallConfimation,['FPC stable','Lazarus stable',' + cross armv7/arm64 android compilers + LAMW']);
    aFPCTarget:='stable'+GITLABEXTENSION;
    aLazarusTarget:='stable'+GITLABEXTENSION;
    aModule:='lamw';
  end;

  {
  if Sender=DinoBtn then
  begin
    s:='Going to install FPC 2.0.2 and Lazarus 0.9.16.';
    aFPCTarget:='2.0.2';
    //aLazarusTarget:='0.9.4';
    aLazarusTarget:='0.9.16';
    By @MarkMLl
    lazarus-0.9.24+2.2.4
    lazarus-0.9.26+2.2.4
    lazarus-0.9.28+2.4.0
    lazarus-0.9.30+2.4.4
    lazarus-1.0.0+2.4.4
    lazarus-1.0.0+2.6.0
    lazarus-1.0.14+2.6.4
    lazarus-1.0.8+2.6.2
    lazarus-1.2.6+2.6.4
    lazarus-1.4.4+3.0.0
    lazarus-1.6.0+3.0.0
    lazarus-1.6.2+3.0.0
    lazarus-1.6.4+3.0.2
    lazarus-1.8.0+3.0.4
    lazarus-1.8.2+3.0.4
    lazarus-1.8.4+3.0.4
    lazarus-2.0.10+3.2.0
    lazarus-2.0.6+3.0.4
    lazarus-2.0.8+3.0.4
  end;
  }

  if (Sender=WioBtn) OR (Sender=PicoBtn) then
  begin
    if Sender=PicoBtn then
    begin
      aFPCTarget:='trunk'+GITLABEXTENSION;
      s:=Format(upInstallConfimation,['FPC trunk','Lazarus trunk',' + cross arm Raspberry Pico compiler + tools']);
      aModule:='develtools4fpc';
    end;
    if Sender=WioBtn then
    begin
      aFPCTarget:='embedded'+GITLABEXTENSION;
      s:=Format(upInstallConfimation,['FPC embedded','Lazarus trunk',' + cross arm Wio Terminal compiler + tools']);
      aModule:='develtools4fpc,mbf-freertos-wio';
    end;
    //aFPCTarget:='embedded'+GITLABEXTENSION;
    aLazarusTarget:='trunk'+GITLABEXTENSION;
  end;

  if Sender=ESPBtn then
  begin
    s:=Format(upInstallConfimation,['FPC trunk','Lazarus trunk',' + cross xtensa ESP32 (FreeRTOS) compiler + tools']);
    aModule:='xtensatools4fpc';
    aFPCTarget:='trunk'+GITLABEXTENSION;
    aLazarusTarget:='trunk'+GITLABEXTENSION;
  end;

  if Sender=UltiboBtn then
  begin
    s:=Format(upInstallConfimationSimple,['Ultibo']);
    aFPCTarget:='ultibo.git';
    aLazarusTarget:='ultibo.git';
  end;

  s:=s+sLineBreak+sLineBreak;
  s:=s+upInstallDirectory+': '+Self.sInstallDir;
  if Form2.AskConfirmation then
    if (MessageDlgEx(s+sLineBreak+sLineBreak+upQuestionContinue,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
      exit;

  if ( AnsiEndsText(GITLABEXTENSION,aFPCTarget) AND AnsiEndsText(GITLABEXTENSION,aLazarusTarget)) then
  begin
    if (NOT chkGitlab.Checked) then chkGitlab.Checked:=true;
  end
  else
  begin
    if (chkGitlab.Checked) then chkGitlab.Checked:=false;
  end;

  AddMessage(s+'.');
  //sStatus:=s;

  DisEnable(Sender,False);
  try

    if (Length(aFPCTarget)>0) then
      FPCTarget:=aFPCTarget;
    if (Length(aLazarusTarget)>0) then
      LazarusTarget:=aLazarusTarget;

    if (NOT PrepareRun(Sender)) then exit;

    if (Length(aModule)>0)then
    begin
      if ((Length(aFPCTarget)=0) AND (Length(aLazarusTarget)=0)) then
        FPCupManager.OnlyModules:=aModule
      else
        FPCupManager.IncludeModules:=aModule;
    end;

    if (NOT Form2.IncludeHelp) then
    begin
      if Length(FPCupManager.SkipModules)>0 then FPCupManager.SkipModules:=FPCupManager.SkipModules+',';
      FPCupManager.SkipModules:=FPCupManager.SkipModules+_HELPFPC+','+_HELPLAZARUS;
    end
    else
    begin
      if Length(FPCupManager.IncludeModules)>0 then FPCupManager.IncludeModules:=FPCupManager.IncludeModules+',';
      FPCupManager.IncludeModules:=FPCupManager.IncludeModules+_LHELP;
    end;

    {$ifdef RemoteLog}
    if ((Length(aFPCTarget)>0) OR (Length(aLazarusTarget)>0)) then
    begin
      aDataClient.UpInfo.UpFunction:=ufInstallFPCLAZ;
    end
    else
    begin
      aDataClient.UpInfo.UpFunction:=ufInstallModule;
      aDataClient.AddExtraData('module',aModule);
    end;
    {$endif}

    if Form2.UpdateOnly then
    begin
    end;

    if (Sender=WioBtn) {OR (Sender=PicoBtn)} then
    begin
      // Due to changes in Lazarus, we need a trunk/main version of Lazarus that can be compiled with an embedded (old) FPC trunk
      Form2.ForceLazarusRevision:='5b0ed449f3';
      FPCupManager.LazarusDesiredRevision:='5b0ed449f3';
    end;

    success:=RealRun;
    //success:=true; // for testing only

    if success then
    begin
      aSUBARCH:=TSUBARCH.saNone;

      if Sender=PicoBtn then
      begin
        s:='Going to install FPC cross-compiler for Raspberry Pico.';
        aCPU:=TCPU.arm;
        aOS:=TOS.embedded;
        aSUBARCH:=TSUBARCH.armv6m;
      end;

      if Sender=WioBtn then
      begin
        s:='Going to install FPC cross-compiler for Wio Terminal.';
        aCPU:=TCPU.arm;
        aOS:=TOS.freertos;
        aSUBARCH:=TSUBARCH.armv7em;
      end;

      if Sender=ESPBtn then
      begin
        s:='Going to install FPC cross-compiler for ESP32.';
        aCPU:=TCPU.xtensa;
        aOS:=TOS.freertos;
        aSUBARCH:=TSUBARCH.lx6;
      end;

      if Sender=UltiboBtn then
      begin
        s:='Going to install FPC cross-compiler for Ultibo.';
        aCPU:=TCPU.arm;
        aOS:=TOS.ultibo;
        aSUBARCH:=TSUBARCH.armv7a;
      end;

      if Sender=AndroidBtn then
      begin
        s:='Going to install FPC cross-compiler for Android arm.';
        aCPU:=TCPU.arm;
        aOS:=TOS.android;
      end;

      if Sender=WABtn then
      begin
        s:='Going to install FPC cross-compiler for Web Assembly.';
        aCPU:=TCPU.wasm32;
        aOS:=TOS.wasi;
      end;

      if (Sender=PicoBtn) OR (Sender=WioBtn) OR (Sender=ESPBtn) OR (Sender=UltiboBtn) OR (Sender=AndroidBtn) OR (Sender=WABtn) then
      begin
        radgrpCPU.ItemIndex:=radgrpCPU.Items.IndexOf(GetCPU(aCPU));
        radgrpOS.ItemIndex:=radgrpOS.Items.IndexOf(GetOS(aOS));
        //Form2.SetCrossAvailable(aCPU,aOS,aSUBARCH,true);
        SetSelectedSubArch(aCPU,aOS,aSUBARCH);

        //aSUBARCH:=GetSelectedSubArch(aCPU,aOS);
        if (aSUBARCH<>TSUBARCH.saNone) then
          s:=s+'. Subarch: '+GetSubarch(aSUBARCH);

        AddMessage(s+'.');
        sStatus:=s;

        {$ifdef RemoteLog}
        aDataClient.UpInfo.CrossCPUOS:=GetCPU(aCPU)+'-'+GetOS(aOS);
        aDataClient.UpInfo.UpFunction:=TUpFunction.ufInstallCross;
        {$endif}

        if (NOT PrepareRun(nil)) then exit;

        FPCupManager.CrossCPU_Target:=aCPU;
        FPCupManager.CrossOS_Target:=aOS;

        success:=ProcessCrossCompiler(nil);

        if success
           then memoSummary.Lines.Append('Cross-compiler install/update ok.')
           else memoSummary.Lines.Append('Failure during install update of cross-compiler !!');

        memoSummary.Lines.Append('');
      end;

      aSUBARCH:=TSUBARCH.saNone;

      if (Sender=UltiboBtn) then
      begin
        s:='Going to install FPC cross-compiler for Ultibo armv8.';
        aCPU:=TCPU.aarch64;
        aOS:=TOS.ultibo;
        aSUBARCH:=TSUBARCH.armv8;
      end;

      if (Sender=AndroidBtn) then
      begin
        s:='Going to install FPC cross-compiler for Android arm64.';
        aCPU:=TCPU.aarch64;
        aOS:=TOS.android;
      end;

      {
      if (Sender=WABtn) then
      begin
        s:='Going to install FPC cross-compiler for WebAssembly Embedded.';
        aCPU:=TCPU.wasm32;
        aOS:=TOS.embedded;
      end;
      }

      if ((Sender=AndroidBtn) OR (Sender=UltiboBtn)) then
      begin
        radgrpCPU.ItemIndex:=radgrpCPU.Items.IndexOf(GetCPU(aCPU));
        radgrpOS.ItemIndex:=radgrpOS.Items.IndexOf(GetOS(aOS));
        //Form2.SetCrossAvailable(aCPU,aOS,aSUBARCH,true);
        SetSelectedSubArch(aCPU,aOS,aSUBARCH);

        //aSUBARCH:=GetSelectedSubArch(aCPU,aOS);
        if (aSUBARCH<>TSUBARCH.saNone) then
          s:=s+'. Subarch: '+GetSubarch(aSUBARCH);

        AddMessage(s+'.');
        sStatus:=s;

        {$ifdef RemoteLog}
        aDataClient.UpInfo.CrossCPUOS:=GetCPU(aCPU)+'-'+GetOS(aOS);
        aDataClient.UpInfo.UpFunction:=TUpFunction.ufInstallCross;
        {$endif}

        if (NOT PrepareRun(nil)) then exit;

        FPCupManager.CrossCPU_Target:=aCPU;
        FPCupManager.CrossOS_Target:=aOS;

        success:=ProcessCrossCompiler(nil);

        if success
           then memoSummary.Lines.Append('Cross-compiler install/update ok.')
           else memoSummary.Lines.Append('Failure during install update of cross-compiler !!');

        memoSummary.Lines.Append('');
      end;

    end;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.btnInstallModuleClick(Sender: TObject);
begin
  if (Sender=btnInstallModule) OR (Sender=btnUninstallModule) then
  begin
    if ((listModules.SelCount=0) OR (listModules.ItemIndex=-1)) then
    begin
      AddMessage('Please select a module / package.');
      exit;
    end;
  end;
  DisEnable(Sender,False);
  try
    if (Sender=OPMBtn) then
      InstallModule('opm',false)
    else
    if (Sender=mORMotBtn) then
      InstallModule('mORMot2',false)
    else
      InstallModule(listModules.Items.Strings[listModules.ItemIndex],(Sender=btnUninstallModule));
  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.InstallModule(aModule:string; UnInstall:boolean);
var
  modules:string;
  s:string;
  success:boolean;
begin
  modules:=aModule;

  if UnInstall then modules:=modules+_UNINSTALL else
  begin
    if Form2.UpdateOnly then modules:=modules+_BUILD+_ONLY;
  end;

  if Length(modules)>0 then
  begin
    //Delete stale trailing comma, if any
    if RightStr(modules,1)=',' then SetLength(modules,Length(modules)-1);

    s:=modules;
    s:=StringReplace(s,_UNINSTALL,'',[rfReplaceAll]);
    s:=StringReplace(s,_BUILD+_ONLY,'',[rfReplaceAll]);
    if (NOT UnInstall) then s:=upInstallModule+': '+s;
    if UnInstall then s:=upRemoveModule+': '+s;
    s:=s+'.'+sLineBreak;
    s:=s+upInstallDirectory+': '+Self.sInstallDir;
    s:=s+sLineBreak;
    s:=s+upQuestionContinue;
    if Form2.AskConfirmation then
      if (MessageDlgEx(s,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
        exit;

    if (NOT UnInstall) then
    begin
      AddMessage('Limiting installation/update to '+FPCupManager.OnlyModules);
      AddMessage('');
      AddMessage('Going to install selected modules with given options.');
      sStatus:='Going to install/update selected modules.';
    end
    else
    begin
      AddMessage('Going to remove selected modules.');
      sStatus:='Going to remove selected modules.';
    end;

    success:=PrepareRun(nil);
    if (NOT success) then exit;

    success:=FPCupManager.CheckCurrentFPCInstall;
    if (NOT success) then
    begin
      ShowMessage('No valid FPC install found. Please install FPC first.');
      exit;
    end;

    FPCupManager.ExportOnly:=(NOT Form2.PackageRepo);
    try
      FPCupManager.OnlyModules:=modules;

      {$ifdef RemoteLog}
      s:=modules;
      s:=StringReplace(s,_UNINSTALL,'',[rfReplaceAll]);
      s:=StringReplace(s,_BUILD+_ONLY,'',[rfReplaceAll]);
      aDataClient.AddExtraData('module[s]:',s);
      if (NOT UnInstall) then aDataClient.UpInfo.UpFunction:=ufInstallModule;
      if UnInstall then aDataClient.UpInfo.UpFunction:=ufUninstallModule;
      {$endif}

      success:=RealRun;

    finally
      FPCupManager.ExportOnly:=(NOT Form2.Repo);
    end;

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

procedure TForm1.ButtonProcessCrossCompiler(Sender: TObject);
procedure ShowInfo(info:string);
begin
  if (NOT Assigned(Sender)) then
    memoSummary.Lines.Append(info)
  else
    ShowMessage(info);
end;
var
  s           : string;
  i           : integer;
  success     : boolean;
begin
  success:=false;

  {$if defined(win64) and not defined(aarch64)}
  if (Sender<>nil) then
  begin
    if Form2.AskConfirmation then
      if (MessageDlgEx('It is ill-advised to cross from Windows 64 bit !'+sLineBreak+'(Win64 OS disabled extended support for 64-bit applications)'+sLineBreak+'Better use a Windows 32 bit install.'+sLineBreak+upQuestionContinue,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
        exit;
  end;
  {$endif}

  success:=PrepareRun(nil);
  if (NOT success) then exit;

  success:=FPCupManager.CheckCurrentFPCInstall;
  if (NOT success) then
  begin
    ShowInfo('No valid FPC install found. Please install FPC first.');
    exit;
  end;

  FPCupManager.CrossCPU_Target:=TCPU.cpuNone;
  if (radgrpCPU.ItemIndex<>-1) then
    FPCupManager.CrossCPU_Target:=GetTCPU(radgrpCPU.Items[radgrpCPU.ItemIndex]);

  FPCupManager.CrossOS_Target:=TOS.osNone;
  if (radgrpOS.ItemIndex<>-1) then
    FPCupManager.CrossOS_Target:=GetTOS(radgrpOS.Items[radgrpOS.ItemIndex]);

  if ((FPCupManager.CrossOS_Target=TOS.win32) AND (FPCupManager.CrossCPU_Target in [TCPU.x86_64,TCPU.aarch64])) then FPCupManager.CrossOS_Target:=TOS.win64;
  if ((FPCupManager.CrossOS_Target=TOS.win32) AND (FPCupManager.CrossCPU_Target in [TCPU.cpuNone])) then FPCupManager.CrossCPU_Target:=TCPU.i386;
  if (FPCupManager.CrossOS_Target=TOS.msdos) then FPCupManager.CrossCPU_Target:=TCPU.i8086;
  //For i8086 embedded and win16 are also ok, but not [yet] implemented by fpcupdeluxe
  if (FPCupManager.CrossCPU_Target=TCPU.i8086) then FPCupManager.CrossOS_Target:=TOS.msdos;
  if (FPCupManager.CrossOS_Target=TOS.go32v2) then FPCupManager.CrossCPU_Target:=TCPU.i386;
  //if (FPCupManager.CrossOS_Target=TOS.dragonfly) then FPCupManager.CrossCPU_Target:=TCPU.x86_64;
  //if (FPCupManager.CrossOS_Target=TOS.java) then FPCupManager.CrossCPU_Target:=TCPU.jvm;

  if (FPCupManager.CrossCPU_Target=TCPU.cpuNone) AND (FPCupManager.CrossOS_Target=TOS.osNone) then
  begin
    ShowInfo('Please select a CPU and OS target first.');
    exit;
  end;

  if (FPCupManager.CrossOS_Target=TOS.embedded) then
  begin
    success:=(FPCupManager.CrossCPU_Target in [TCPU.avr,TCPU.arm,TCPU.aarch64,TCPU.mipsel,TCPU.wasm32]);
    if (NOT success) then
    begin
      ShowInfo('No valid CPU target for embedded.');
      exit;
    end;
  end;

  if (FPCupManager.CrossOS_Target=TOS.freertos) then
  begin
    success:=(FPCupManager.CrossCPU_Target in [TCPU.xtensa,TCPU.arm]);
    if (NOT success) then
    begin
      ShowInfo('No valid CPU target for FreeRTOS.');
      exit;
    end;
  end;

  if (FPCupManager.CrossOS_Target=TOS.ultibo) then
  begin
    success:=(FPCupManager.CrossCPU_Target in [TCPU.aarch64,TCPU.arm]);
    if (NOT success) then
    begin
      ShowInfo('No valid CPU target for Ultibo.');
      exit;
    end;
  end;

  if (FPCupManager.CrossOS_Target=TOS.android) then
  begin
    success:=(FPCupManager.CrossCPU_Target in [TCPU.i386,TCPU.arm,TCPU.mipsel,TCPU.jvm,TCPU.aarch64,TCPU.x86_64]);
    if (NOT success) then
    begin
      ShowInfo('No valid CPU target for android.');
      exit;
    end;
  end;

  if (FPCupManager.CrossOS_Target=TOS.dragonfly) then
  begin
    success:=(FPCupManager.CrossCPU_Target in [TCPU.x86_64]);
    if (NOT success) then
    begin
      ShowInfo('No valid CPU target for dragonfly.');
      exit;
    end;
  end;

  if (FPCupManager.CrossOS_Target=TOS.wasi) then
  begin
    success:=(FPCupManager.CrossCPU_Target in [TCPU.wasm32]);
    if (NOT success) then
    begin
      ShowInfo('No valid CPU target for WebAssembly.');
      exit;
    end;
  end;

  if (FPCupManager.CrossCPU_Target=TCPU.jvm) then
  begin
    success:=(FPCupManager.CrossOS_Target in [TOS.android,TOS.java]);
    if (NOT success) then
    begin
      ShowInfo('No valid OS target for jvm.');
      exit;
    end;
  end;

  if (FPCupManager.CrossCPU_Target=TCPU.wasm32) then
  begin
    success:=(FPCupManager.CrossOS_Target in [TOS.wasi,TOS.embedded]);
    if (NOT success) then
    begin
      ShowInfo('No valid OS target for WebAssembly.');
      exit;
    end;
  end;

  if (radgrpOS.ItemIndex<>-1) then
  begin
    s:=radgrpOS.Items[radgrpOS.ItemIndex];
    if s='linux-musl' then FPCupManager.MUSL:=true;
    if s='solaris-oi' then FPCupManager.SolarisOI:=true;
  end;

  //{$ifdef Linux}
  //if FPCupManager.MUSL then
  begin
    if ((FPCupManager.CrossOS_Target=GetTOS(GetSourceOS)) AND (FPCupManager.CrossCPU_Target=GetTCPU(GetSourceCPU))) then
    begin
      if Sender<>nil then
      begin
        s:='On '+GetSourceOS+'-'+GetSourceCPU+', you cannot cross towards another '+GetSourceOS+'-'+GetSourceCPU+'.';
        Application.MessageBox(PChar(s), PChar('FPC limitation'), MB_ICONERROR);
      end;
      FPCupManager.CrossOS_Target:=TOS.osNone; // cleanup
      FPCupManager.CrossCPU_Target:=TCPU.cpuNone; // cleanup
      exit;
    end;
  end;
  //{$endif}

  if (FPCupManager.CrossCPU_Target=TCPU.cpuNone) then
  begin
    if Sender<>nil then Application.MessageBox(PChar(upSelectCPUTarget), PChar('CPU error'), MB_ICONERROR);
    exit;
  end;

  if (FPCupManager.CrossOS_Target=TOS.osNone) then
  begin
    if Sender<>nil then Application.MessageBox(PChar(upSelectOSTarget), PChar('OS error'), MB_ICONERROR);
    exit;
  end;

  if (NOT FPCupManager.CheckValidCPUOS) then
  begin
    if (NOT (FPCupManager.CrossOS_Target in [TOS.freertos,TOS.ultibo])) then
    begin
      memoSummary.Lines.Append('');
      memoSummary.Lines.Append('From current FPC source: No valid CPU/OS target.');
      memoSummary.Lines.Append('Cross-building will continue, but with great changes of errors !!');
    end;
  end;

  if assigned(CrossInstallers) then
  begin
    success:=false;
    for i := 0 to CrossInstallers.Count - 1 do
    begin
      success:=( (TCrossInstaller(CrossInstallers.Objects[i]).TargetCPU=FPCupManager.CrossCPU_Target) AND (TCrossInstaller(CrossInstallers.Objects[i]).TargetOS=FPCupManager.CrossOS_Target) );
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
      FPCupManager.CrossOS_Target:=TOS.osNone; // cleanup
      FPCupManager.CrossCPU_Target:=TCPU.cpuNone; // cleanup
      exit;
    end;
  end;

  success:=ProcessCrossCompiler(Sender);
end;

function TForm1.ProcessCrossCompiler(Sender: TObject):boolean;
var
  BinsFileName,LibsFileName,BaseBinsURL,BaseLibsURL,BinPath,LibPath:string;
  ToolTargetPath,ToolTargetFile,UnZipper,s:string;
  warning,success,verbose:boolean;
  IncludeLCL,ZipFile:boolean;
  aList: TStringList;
  {$IF (DEFINED(WINDOWS)) OR (DEFINED(LINUX))}
  frmSeq: TfrmSequencial;
  {$ENDIF}
begin
  result:=false;

  // Set subarch early
  FPCupManager.CrossOS_SubArch:=GetSelectedSubArch(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);

  {$ifdef RemoteLog}
  aDataClient.UpInfo.CrossCPUOS:=GetCPU(FPCupManager.CrossCPU_Target)+'-'+GetOS(FPCupManager.CrossOS_Target);
  {$endif}

  if Sender=ButtonRemoveCrossCompiler then
  begin
    s:=upRemoveCrossCompiler+' ['+GetCPU(FPCupManager.CrossCPU_Target)+'-'+GetOS(FPCupManager.CrossOS_Target)+'].'+sLineBreak+upQuestionContinue;
    if Form2.AskConfirmation then
      if (MessageDlgEx(s,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
        exit;

    DisEnable(Sender,False);
    try
      if ((FPCupManager.CrossOS_Target=TOS.java) OR
          (FPCupManager.CrossOS_Target=TOS.android) OR
          (FPCupManager.CrossOS_Target=TOS.ios) OR
          (FPCupManager.CrossOS_Target in SUBARCH_OS)) then
      begin
        FPCupManager.OnlyModules:=_FPCREMOVEONLY;
      end
      else
      begin
        FPCupManager.OnlyModules:=_LCLALLREMOVEONLY+','+_FPCREMOVEONLY;
      end;

      {$ifdef RemoteLog}
      aDataClient.UpInfo.UpFunction:=ufUninstallCross;
      {$endif}
      success:=RealRun;
      //if success then
      //  Form2.SetCrossAvailable(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,FPCupManager.CrossOS_SubArch,false);
    finally
      DisEnable(Sender,true);
    end;
  end
  else
  begin
    if Sender<>nil then
    begin
      {$if (defined(UNIX)) and (not defined(Darwin))}
      if (FPCupManager.CrossOS_Target=TOS.darwin) OR ( (FPCupManager.CrossOS_Target=TOS.win64) AND (FPCupManager.CrossCPU_Target=TCPU.aarch64) ) then
      begin
        success:=false;
        s:=Which('clang');
        if FileExists(s) then success:=CheckExecutable(s, ['-v'], '');
        if (NOT success) then
        begin
          s:=
          'Clang cannot be found !!'+ sLineBreak +
          'Clang need to be installed to be able to cross-compile towards '+GetOS(FPCupManager.CrossOS_Target)+' !'+ sLineBreak +
          'Install clang and retry !!';
          Application.MessageBox(PChar(s), PChar('Missing clang'), MB_ICONERROR);
          memoSummary.Lines.Append('');
          memoSummary.Lines.Append('To get clang: sudo apt-get install clang');
          exit;
        end;
      end;
      if (FPCupManager.CrossOS_Target=TOS.wince) then
      begin
        (*
        success:=CheckExecutable('gcc', '-v', '');
        if (NOT success) then
        begin
          s:=
          'Gcc cannot be found !!'+ sLineBreak +
          'Gcc need to be installed to be able to cross-compile towards wince !'+ sLineBreak +
          'Install gcc and retry !!';
          Application.MessageBox(PChar(s), PChar('Missing gcc'), MB_ICONERROR);
          memoSummary.Lines.Append('');
          memoSummary.Lines.Append('To get gcc: sudo apt-get install gcc');
          //memoSummary.Lines.Append('Cross-building will continue, but with great changes of winres errors !!');
          exit;
        end;
        *)
        (*
        {$ifdef CPU64}
        if (NOT FileExists('/lib/ld-linux-x86-64.so.2')) then
        begin
          s:=
          'The current wince binutils need /lib/ld-linux-x86-64.so.2 !' + sLineBreak +
          'If so, add this symlink and point it towards /lib/x86_64-linux-gnu/ld-2.24.so' + sLineBreak +
          'sudo ln -s /lib/x86_64-linux-gnu/ld-2.24.so /lib/ld-linux-x86-64.so.2';
          Application.MessageBox(PChar(s), PChar('Dynamic linker/loader'), MB_ICONWARNING);
          memoSummary.Lines.Append('');
          memoSummary.Lines.Append(s);
        end;
        {$endif}
        *)
      end;
      {$endif}

      if (FPCupManager.CrossOS_Target=TOS.java) then
      begin
        success:=CheckJava;
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

      warning:=false;

      s:=GetMinimumFPCVersion(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
      if (Length(s)>0) AND (s<>'0.0.0') then
      begin
        warning:=true;
        if warning then s:='Be forwarned: this will only work with FPC [(>='+s+')].'+sLineBreak+upQuestionContinue;
      end;

      {$ifdef Linux}
      if (NOT warning) then
      begin
        warning:=((FPCupManager.CrossCPU_Target=TCPU.mips) OR (FPCupManager.CrossCPU_Target=TCPU.mipsel));
        if (warning) then s:='You could get the native cross-utilities first (advised).' + sLineBreak +
             'E.g.: sudo apt-get install libc6-mips-cross binutils-mips-linux-gnu' + sLineBreak +
             upQuestionContinue;
      end;
      {$endif}

      if (NOT warning) then
        s:=upQuestionContinue;
      s:=upInstallCrossCompiler+' ['+FPCupManager.CrossCombo_Target+']'+sLineBreak+s;
      if Form2.AskConfirmation then
        if (MessageDlgEx(s,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
          exit;
    end;

    DisEnable(Sender,False);

    try
      //FPCupManager.CrossOS_SubArch:=GetSelectedSubArch(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);

      //embedded predefined settings
      if (FPCupManager.CrossOS_Target=TOS.embedded) then
      begin
        if FPCupManager.CrossOS_SubArch=TSUBARCH.saNone then
        begin
          if (FPCupManager.CrossCPU_Target=TCPU.avr) then
            FPCupManager.CrossOS_SubArch:=TSubarch.avr5;
          if (FPCupManager.CrossCPU_Target=TCPU.arm) then
            FPCupManager.CrossOS_SubArch:=TSubarch.armv6m;
          if (FPCupManager.CrossCPU_Target=TCPU.mipsel) then
            FPCupManager.CrossOS_SubArch:=TSubarch.pic32mx;
        end;
      end;

      //freertos predefined settings
      if (FPCupManager.CrossOS_Target=TOS.freertos) then
      begin
        if FPCupManager.CrossOS_SubArch=TSUBARCH.saNone then
        begin
          if (FPCupManager.CrossCPU_Target=TCPU.xtensa) then
            FPCupManager.CrossOS_SubArch:=TSubarch.lx6;
          if (FPCupManager.CrossCPU_Target=TCPU.arm) then
            FPCupManager.CrossOS_SubArch:=TSubarch.armv7em;
        end;
      end;

      //ultibo predefined settings
      if (FPCupManager.CrossOS_Target=TOS.ultibo) then
      begin
        if (FPCupManager.CrossCPU_Target=TCPU.arm) then
        begin
          if FPCupManager.CrossOS_SubArch=TSUBARCH.saNone then
          begin
            FPCupManager.CrossOS_SubArch:=TSubarch.armv7a;
          end;
        end;
        if (FPCupManager.CrossCPU_Target=TCPU.aarch64) then
        begin
          if FPCupManager.CrossOS_SubArch=TSUBARCH.saNone then
          begin
            FPCupManager.CrossOS_SubArch:=TSubarch.armv8;
          end;
        end;
      end;

      if (FPCupManager.CrossCPU_Target=TCPU.arm) then
      begin
        if (Pos('-dFPC_ARM',FPCupManager.FPCOPT)=0) then
        begin
          // Set arm abi build option
          FPCupManager.FPCOPT:=FPCupManager.FPCOPT+' '+Form2.GetCrossARMFPCStr(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,FPCupManager.CrossOS_SubArch);
          FPCupManager.FPCOPT:=Trim(FPCupManager.FPCOPT);
        end;
      end;

      // Set FPC cross-compile options
      FPCupManager.CrossOPT:=Form2.GetCrossBuildOptions(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,FPCupManager.CrossOS_SubArch);

      // use the available source to build the cross-compiler ... change nothing about source and url !!
      FPCupManager.OnlyModules:=_FPCCLEANBUILDONLY;

      // handle inclusion of LCL when cross-compiling
      IncludeLCL:=Form2.IncludeLCL;
      if (NOT (FPCupManager.CrossOS_Target in LCL_OS)) then IncludeLCL:=false;

      if IncludeLCL then
      begin
        FPCupManager.OnlyModules:=FPCupManager.OnlyModules+','+_LCL;
        if (FPCupManager.CrossOS_Target in WINDOWS_OS) then
           FPCupManager.LCL_Platform:='win32'
        else
        if (FPCupManager.CrossOS_Target=TOS.wince) then
           FPCupManager.LCL_Platform:='wince'
        else
        if (FPCupManager.CrossOS_Target=TOS.darwin) then
        begin
          FPCupManager.LCL_Platform:='cocoa';
          {$ifdef LCLQT5}
          FPCupManager.LCL_Platform:='qt5';
          {$endif}
          {$ifdef LCLCARBON}
          FPCupManager.LCL_Platform:='carbon';
          {$endif}
        end
        else
        if (FPCupManager.CrossOS_Target in [TOS.amiga,TOS.aros,TOS.morphos]) then
           FPCupManager.LCL_Platform:='mui'
        else
        begin
          if (FPCupManager.CrossOS_Target<>TOS.osNone) AND (FPCupManager.CrossCPU_Target<>TCPU.cpuNone) then
            FPCupManager.LCL_Platform:='gtk2';
        end;
      end
      else
      begin
        if Form2.IncludeLCL then AddMessage('Skipping build of LCL for this target: not supported (yet).');
      end;

      //For testing only !!
      //FPCupManager.OnlyModules:='LCL';

      s:=Form2.GetLibraryDirectory(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,FPCupManager.CrossOS_SubArch);
      s:=Trim(s);
      if Length(s)>0 then
      begin
        if DirectoryExists(s) then
          FPCupManager.CrossLibraryDirectory:=s
        else
        begin
          AddMessage('Cross libraries not found at supplied location.');
          AddMessage('Libs location: '+s);
          AddMessage('Expect failures.');
        end;
      end;
      s:=Form2.GetToolsDirectory(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,FPCupManager.CrossOS_SubArch);
      s:=Trim(s);
      if Length(s)>0 then
      begin
        if DirectoryExists(s) then
          FPCupManager.CrossToolsDirectory:=s
        else
        begin
          AddMessage('Cross tools not found at supplied location.');
          AddMessage('Tools location: '+s);
          AddMessage('Expect failures.');
        end;
      end;

      AddMessage(upBuildCrossCompiler);

      s:='fpcupdeluxe: FPC cross-builder: Building compiler for '+GetOS(FPCupManager.CrossOS_Target);
      if FPCupManager.MUSL then s:=s+'-musl';
      if FPCupManager.SolarisOI then s:=s+'-openindiana';
      s:=s+'-'+GetCPU(FPCupManager.CrossCPU_Target);
      sStatus:=s;

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
      if FPCupManager.CrossOS_SubArch<>TSubarch.saNone then
      begin
        sStatus:=sStatus+' {SUBARCH: '+GetSubarch(FPCupManager.CrossOS_SubArch)+'}';
        {$ifdef RemoteLog}
        aDataClient.AddExtraData('SUBARCH',GetSubarch(FPCupManager.CrossOS_SubArch));
        {$endif}
      end;
      sStatus:=sStatus+'.';

      AddMessage(sStatus);
      memoSummary.Lines.Append(sStatus);

      {$ifdef RemoteLog}
      aDataClient.UpInfo.UpFunction:=ufInstallCross;
      if length(FPCupManager.LCL_Platform)>0 then aDataClient.AddExtraData('LCL',FPCupManager.LCL_Platform);
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
            if (MessageDlgEx(upMissingTools+sLineBreak+upAdvertiseTools+sLineBreak+upQuestionContinue
                     ,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
                     begin
                       exit;
                     end;
          end;

          // Get special tools when needed !

          if (NOT success) then
          begin
            if (FPCupManager.CrossCPU_Target=TCPU.xtensa) AND (FPCupManager.CrossOS_Target=TOS.freertos) then
            begin
              try
                s:=FPCupManager.OnlyModules;
                if Assigned(FPCupManager.Sequencer) then FPCupManager.Sequencer.ResetAllExecuted;
                FPCupManager.OnlyModules:='xtensatools4fpc';
                AddMessage('Getting (if any) xtensa tools from https://github.com/michael-ring/espsdk4fpc.');
                success:=RealRun;
              finally
                FPCupManager.OnlyModules:=s;
              end;
              if success then
              begin
                // Check if we have received some files.
                s:=ConcatPaths([FPCupManager.BaseDirectory,CROSSBINPATH,GetCPU(TCPU.xtensa)+'-'+GetOS(TOS.freertos)]);
                if DirectoryExists(s) then MissingCrossBins:=false;
                s:=ConcatPaths([FPCupManager.BaseDirectory,CROSSLIBPATH,GetCPU(TCPU.xtensa)+'-'+GetOS(TOS.freertos)]);
                if DirectoryExists(s) then MissingCrossLibs:=false;
                if MissingCrossBins OR MissingCrossLibs then success:=false;
              end;
            end;
          end;

          // Get fpcupdeluxe tools when needed !
          if (NOT success) then
          begin
            if ((Sender<>nil) AND (CheckAutoClear.Checked)) then memoSummary.Clear;

            memoSummary.Lines.Append(upTryBuildCrossCompiler+' '+GetOS(FPCupManager.CrossOS_Target)+'-'+GetCPU(FPCupManager.CrossCPU_Target)+'.');

            AddMessage(upLookupTools);

            FPCupManager.GetCrossToolsFileName(BinsFileName,LibsFileName);
            FPCupManager.GetCrossToolsPath(BinPath,LibPath);

            // bit tricky ... if bins and/or libs are already there exit this retry ... ;-)
            if (NOT DirectoryIsEmpty(IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+BinPath)) then MissingCrossBins:=false;

            if (FPCupManager.CrossOS_SubArch=TSUBARCH.saNone) then
            begin
              if (NOT DirectoryIsEmpty(IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+LibPath)) then MissingCrossLibs:=false;
            end
            else
            begin
              s:=ConcatPaths([FPCupManager.BaseDirectory,LibPath,GetSubarch(FPCupManager.CrossOS_SubArch)]);
              if (NOT DirectoryIsEmpty(s)) then
                MissingCrossLibs:=false
              else
                MissingCrossLibs:=true;// force the download of embedded libs if not there ... if this fails, don't worry, building will go on
            end;

            // Get tools !!
            if (MissingCrossBins OR MissingCrossLibs) then
            begin

              // many files to unpack for Darwin : do not show progress of unpacking files when unpacking for Darwin.
              verbose:=(FPCupManager.CrossOS_Target<>TOS.darwin);

              if MissingCrossBins then
              begin
                AddMessage(upLookupBinTools,True);
                AddMessage(upLookup+': '+BinsFileName, True);

                success:=FPCupManager.GetCrossBinsURL(BaseBinsURL,BinsFileName);

                // no cross-bins available
                if (NOT success) then
                begin
                  ShowMessage('No binary tools available online. You could do a feature request ... ;-)');
                  exit;
                end;

                if success then
                begin
                  AddMessage('Found correct online binutils at: '+BaseBinsURL);
                  AddMessage('Going to download the cross-bins. Can (will) take some time !',True);
                  ToolTargetFile := IncludeTrailingPathDelimiter(FPCupManager.TempDirectory)+BinsFileName;
                  SysUtils.DeleteFile(ToolTargetFile);
                  success:=false;
                  {$IF (DEFINED(WINDOWS)) OR (DEFINED(LINUX))}
                  frmSeq:= TfrmSequencial.Create(Self);
                  try
                    frmSeq.AddDownload(BaseBinsURL,ToolTargetFile);
                    frmSeq.ShowModal;
                    success:=frmSeq.Success;
                  finally
                    frmSeq.Free;
                  end;
                  {$ENDIF}
                  if (NOT success) then
                  begin
                    SysUtils.DeleteFile(ToolTargetFile);
                    success:=DownLoad(FPCupManager.UseWget,BaseBinsURL,ToolTargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
                  end;
                  if success then AddMessage('Download successfull !');
                end;

                if success then
                begin
                  ZipFile:=(ExtractFileExt(ToolTargetFile)='.zip');
                  {$ifndef MSWINDOWS}
                  ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+BinPath+DirectorySeparator;
                  {$else}
                  ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory);
                  // Check if we have the new bins
                  if (Pos(NEWBINSTAG,BaseBinsURL)>0) then
                  begin
                    AddMessage('We got new bins !');
                    ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+BinPath+DirectorySeparator;
                  end;
                  {$endif}

                  ForceDirectoriesSafe(ToolTargetPath);

                  AddMessage('Going to extract archive into '+ToolTargetPath);

                  if ZipFile then
                  begin
                    with TNormalUnzipper.Create do
                    begin
                      try
                        success:=DoUnZip(ToolTargetFile,ToolTargetPath,[]);
                      finally
                        Free;
                      end;
                    end;
                  end
                  else
                  begin
                    {$ifdef MSWINDOWS}
                    if (not verbose) then AddMessage('Please wait: going to unpack binary tools archive.');
                    success:={%H-}RunCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+ToolTargetFile+' "'+ToolTargetPath+'"',s);
                    if (NOT success) then
                    {$endif}
                    begin
                      {$ifdef MSWINDOWS}
                      UnZipper:=ConcatPaths([FPCupManager.MakeDirectory,'unrar','bin'])+PathSeparator+'unrar.exe';
                      {$else}
                      UnZipper := 'unrar';
                      {$endif}
                      success:=CheckExecutable(UnZipper, ['-v'], '');
                      if success then
                      begin
                        if (not verbose) then AddMessage('Please wait: going to unpack binary tools archive.');
                        success:={%H-}RunCommand(UnZipper + ' x "' + ToolTargetFile + '" "' + ToolTargetPath + '"',s);
                      end else AddMessage('Error: '+UnZipper+' not found on system. Cannot unpack cross-tools !');
                    end;
                  end;
                end;

                SysUtils.DeleteFile(ToolTargetFile);

                if success then
                begin
                  aList:=TStringList.Create;
                  try
                    aList.Add('These binary utilities were happily provided to you by fpcupdeluxe.');
                    aList.Add('You can find them at:');
                    aList.Add(BaseBinsURL);
                    s:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+BinPath+DirectorySeparator+FPCUP_ACKNOWLEDGE;
                    If DirectoryExists(ExtractFileDir(s)) then
                    begin
                      SysUtils.DeleteFile(s);
                      aList.SaveToFile(s);
                    end;
                  finally
                    aList.Free;
                  end;
                  MissingCrossBins:=False;
                end;
              end;

              // force the download of embedded libs if not there ... if this fails, don't worry, building will go on
              if (DirectoryIsEmpty(IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+LibPath)) AND (FPCupManager.CrossOS_Target=TOS.embedded)
                then MissingCrossLibs:=true;


              if MissingCrossLibs then
              begin
                AddMessage(upLookupLibTools,True);
                AddMessage(upLookup+': '+LibsFileName, True);

                success:=FPCupManager.GetCrossLibsURL(BaseLibsURL,LibsFileName);

                // no cross-libraries available
                if (NOT success) then
                begin
                  // Do not fail !!
                  //ShowMessage('No libraries available online. You could do a feature request ... ;-)');
                  //exit;
                end;

                if success then
                begin
                  AddMessage('Found correct online libraries at: '+BaseLibsURL);
                  AddMessage('Going to download the cross-libs. Can (will) take some time !',True);
                  ToolTargetFile := IncludeTrailingPathDelimiter(FPCupManager.TempDirectory)+LibsFileName;
                  SysUtils.DeleteFile(ToolTargetFile);
                  success:=false;
                  {$IF (DEFINED(WINDOWS)) OR (DEFINED(LINUX))}
                  frmSeq:= TfrmSequencial.Create(Self);
                  try
                    frmSeq.AddDownload(BaseLibsURL,ToolTargetFile);
                    frmSeq.ShowModal;
                    success:=frmSeq.Success;
                  finally
                    frmSeq.Free;
                  end;
                  {$ENDIF}
                  if (NOT success) then
                  begin
                    SysUtils.DeleteFile(ToolTargetFile);
                    success:=DownLoad(FPCupManager.UseWget,BaseLibsURL,ToolTargetFile,FPCupManager.HTTPProxyHost,FPCupManager.HTTPProxyPort,FPCupManager.HTTPProxyUser,FPCupManager.HTTPProxyPassword);
                  end;
                  if success then AddMessage('Download successfull !');
                end;

                if success then
                begin
                  ZipFile:=(ExtractFileExt(ToolTargetFile)='.zip');
                  ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory);
                  // Check if we have the new libs
                  if (Pos(NEWLIBSTAG,BaseLibsURL)>0) then
                  begin
                    AddMessage('We got new libs !');
                    ToolTargetPath:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+LibPath+DirectorySeparator;
                  end;

                  ForceDirectoriesSafe(ToolTargetPath);

                  AddMessage('Going to extract archive into '+ToolTargetPath);

                  if ZipFile then
                  begin
                    with TNormalUnzipper.Create do
                    begin
                      try
                        success:=DoUnZip(ToolTargetFile,ToolTargetPath,[]);
                      finally
                        Free;
                      end;
                    end;
                  end
                  else
                  begin
                    {$ifdef MSWINDOWS}
                    if (not verbose) then AddMessage('Please wait: going to unpack library files archive.');
                    success:={%H-}RunCommand('"C:\Program Files (x86)\WinRAR\WinRAR.exe" x '+ToolTargetFile+' "'+ToolTargetPath+'"',s);
                    if (NOT success) then
                    {$endif}
                    begin
                      {$ifdef MSWINDOWS}
                      UnZipper:=ConcatPaths([FPCupManager.MakeDirectory,'unrar','bin'])+PathSeparator+'unrar.exe';
                      {$else}
                      UnZipper := 'unrar';
                      {$endif}
                      success:=CheckExecutable(UnZipper, ['-v'], '');
                      if success then
                      begin
                        if (not verbose) then AddMessage('Please wait: going to unpack library files archive.');
                        success:={%H-}RunCommand(UnZipper + ' x "' + ToolTargetFile + '" "' + ToolTargetPath + '"',s);
                      end else AddMessage('Error: '+UnZipper+' not found on system. Cannot unpack cross-tools !');
                    end;
                  end;
                end;
                SysUtils.DeleteFile(ToolTargetFile);

                if success then
                begin
                  aList:=TStringList.Create;
                  try
                    aList.Add('These libraries were happily provided to you by fpcupdeluxe.');
                    aList.Add('You can find them at:');
                    aList.Add(BaseLibsURL);
                    s:=IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory)+LibPath+DirectorySeparator+FPCUP_ACKNOWLEDGE;
                    if DirectoryExists(ExtractFileDir(s)) then
                    begin
                      SysUtils.DeleteFile(s);
                      aList.SaveToFile(s);
                    end;
                  finally
                    aList.Free;
                  end;
                  MissingCrossLibs:=False;
                end;

                // as libraries are not always needed for embedded, end with success even if the above has failed
                if FPCupManager.CrossOS_Target=TOS.embedded then
                begin
                  success:=true;
                  MissingCrossLibs:=False;
                end;

              end;
            end;
          end;

          if success then
          begin
            AddMessage('Successfully extracted cross-tools.');
            // run again with the correct libs and binutils
            FPCVersionLabel.Font.Color:=clDefault;
            LazarusVersionLabel.Font.Color:=clDefault;
            AddMessage('Got all tools now. Building a cross-compiler for '+GetOS(FPCupManager.CrossOS_Target)+'-'+GetCPU(FPCupManager.CrossCPU_Target),True);
            memoSummary.Lines.Append('Got all tools. Started to build cross-compiler.');
            if Assigned(FPCupManager.Sequencer) then FPCupManager.Sequencer.ResetAllExecuted;

            AddMessage(sStatus);
            memoSummary.Lines.Append(sStatus);

            MissingCrossBins:=false;
            MissingCrossLibs:=false;

            success:=RealRun;
          end
          else AddMessage('No luck in getting then cross-tools ... aborting.');
        end
        else
        begin
          AddMessage('Building cross-tools failed. Aborting.');
        end;

      end;

      //if success then
      //  Form2.SetCrossAvailable(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target,FPCupManager.CrossOS_SubArch,true);

    finally
      DisEnable(Sender,True);
    end;
  end;

  result:=success;
end;

procedure TForm1.InstallClick(Sender: TObject);
var
  s:string;
  FModuleList: TStringList;
  success:boolean;
begin
  s:='';
  if Sender=BitBtnFPCOnly then
    if (Length(FPCTarget)=0) then s:=upSelectFPCTarget;
  if Sender=BitBtnLazarusOnly then
    if (Length(LazarusTarget)=0) then s:=upSelectLazarusTarget;
  if Sender=BitBtnFPCandLazarus then
    if (Length(FPCTarget)=0) or (Length(LazarusTarget)=0) then s:=upSelectFPCLazarusTarget;

  if Length(s)>0 then
  begin
    ShowMessage(s);
    exit;
  end;

  if Form2.AskConfirmation then
  begin
    s:='';
    if Sender=BitBtnFPCOnly then s:=upInstallFPC;
    if Sender=BitBtnLazarusOnly then s:=upInstallLazarus;
    if Sender=BitBtnFPCandLazarus then s:=upInstallFPCLazarus;
    s:=s+sLineBreak;
    s:=s+upInstallDirectory+': '+Self.sInstallDir;
    s:=s+sLineBreak;
    s:=s+upQuestionContinue;
    if (MessageDlgEx(s,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then exit;
  end;

  DisEnable(Sender,False);
  try

    success:=PrepareRun(nil);
    if (NOT success) then exit;

    if Sender=BitBtnLazarusOnly then
    begin
      if (NOT Form2.SystemFPC) then
      begin
        success:=FPCupManager.CheckCurrentFPCInstall;
        if (NOT success) then
        begin
          ShowMessage('No valid FPC install found. Please install FPC first.');
          exit;
        end;
      end;
    end;

    s:='';

    if Form2.UpdateOnly then
    begin

      if Sender=BitBtnFPCOnly then
      begin
        {$ifdef win32}
        s:=_FPCCLEANBUILDONLY+','+_FPC+_CROSSWIN;
        {$else}
        s:=_FPCCLEANBUILDONLY;
        {$endif}
      end;

      if Sender=BitBtnLazarusOnly then
      begin
        s:=_LAZARUSCLEANBUILDONLY;
      end;

      if Sender=BitBtnFPCandLazarus then
      begin
        // still not working 100% for Lazarus ...  todo
        // packages that are installed by the user are not included
        s:=_FPCCLEANBUILDONLY+','+_LAZARUSCLEANBUILDONLY;
        FModuleList:=TStringList.Create;
        try
          GetModuleEnabledList(FModuleList);
          // also include enabled modules (packages) when rebuilding Lazarus
          if FModuleList.Count>0 then s:=s+','+FModuleList.CommaText;
        finally
          FModuleList.Free;
        end;
      end;

      if Length(s)>0 then
        FPCupManager.OnlyModules:=s
      else
        begin
          ShowMessage('No sequence defined. Should never happen. Please file bugger.');
          exit;
        end;
    end
    else
    begin

      if Sender=BitBtnFPCOnly then
      begin
        s:=_FPC;
      end;

      if Sender=BitBtnLazarusOnly then
      begin
        {$IF defined(CPUARM) or defined(CPUARMHF) or defined(HAIKU) or defined(CPUPOWERPC64)}
        s:=_LAZARUSSIMPLE;
        {$ELSE}
        s:=_LAZARUS;
        {$ENDIF}
      end;

      if Sender=BitBtnFPCandLazarus then
      begin
        //use standard install/default sequence
      end;

      if Length(s)>0 then FPCupManager.OnlyModules:=s;

    end;

    if (NOT Form2.IncludeHelp) then
    begin
      if ((Sender=BitBtnFPCOnly) OR (Sender=BitBtnFPCandLazarus)) then FPCupManager.SkipModules:=FPCupManager.SkipModules+','+_HELPFPC;
      if ((Sender=BitBtnLazarusOnly) OR (Sender=BitBtnFPCandLazarus)) then FPCupManager.SkipModules:=FPCupManager.SkipModules+','+_HELPLAZARUS;
    end
    else
    begin
      if ((Sender=BitBtnLazarusOnly) OR (Sender=BitBtnFPCandLazarus)) then
      begin
        FPCupManager.IncludeModules:=FPCupManager.IncludeModules+','+_LHELP;
      end;
    end;

    if (Form2.DockedLazarus) then
    begin
      if ((Sender=BitBtnLazarusOnly) OR (Sender=BitBtnFPCandLazarus)) then FPCupManager.IncludeModules:=FPCupManager.IncludeModules+',anchordocking';
    end;

    //Delete stray comma
    s:=FPCupManager.IncludeModules;
    if Pos(',',s)=1 then
    begin
      Delete(s,1,1);
      FPCupManager.IncludeModules:=s;
    end;
    //Delete stray comma
    s:=FPCupManager.SkipModules;
    if Pos(',',s)=1 then
    begin
      Delete(s,1,1);
      FPCupManager.SkipModules:=s;
    end;

    if Sender=BitBtnFPCOnly then
      sStatus:='Going to install/update FPC only';
    if Sender=BitBtnLazarusOnly then
      sStatus:='Going to install/update Lazarus only';
    if Sender=BitBtnFPCandLazarus then
      sStatus:='Going to install/update FPC and Lazarus';

    AddMessage(sStatus+' with given options.');

    {$ifdef RemoteLog}
    aDataClient.UpInfo.UpFunction:=ufInstallFPCLAZ;
    if Sender=BitBtnFPCOnly then aDataClient.UpInfo.UpFunction:=ufInstallFPC;
    if Sender=BitBtnLazarusOnly then aDataClient.UpInfo.UpFunction:=ufInstallLAZ;
    {$endif}

    RealRun;

  finally
    DisEnable(Sender,True);
  end;
end;

procedure TForm1.btnSetupPlusClick(Sender: TObject);
var
  s:string;
  aOldSubarch,aNewSubarch:TSUBARCH;
begin
  if radgrpCPU.ItemIndex<>-1 then
  begin
    s:=radgrpCPU.Items[radgrpCPU.ItemIndex];
    FPCupManager.CrossCPU_Target:=GetTCPU(s);
  end;

  if radgrpOS.ItemIndex<>-1 then
  begin
    s:=radgrpOS.Items[radgrpOS.ItemIndex];
    FPCupManager.CrossOS_Target:=GetTOS(s);
  end;

  aOldSubarch:=GetSelectedSubArch(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
  Form2.SetCrossTarget(FPCupManager,FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);

  Form2.ShowModal;
  if (Form2.ModalResult=mrOk) then
  begin
    FPCupManager.ExportOnly:=(NOT Form2.Repo);
    FPCupManager.HTTPProxyHost:=Form2.HTTPProxyHost;
    FPCupManager.HTTPProxyPort:=Form2.HTTPProxyPort;
    FPCupManager.HTTPProxyUser:=Form2.HTTPProxyUser;
    FPCupManager.HTTPProxyPassword:=Form2.HTTPProxyPass;
    aNewSubarch:=GetSelectedSubArch(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);
    if ((aNewSubarch<>aOldSubarch) AND (aNewSubarch<>TSUBARCH.saNone)) then
      AddMessage('Fpcupdeluxe: selected subarch = '+GetSubarch(aNewSubarch));
  end;
end;

procedure TForm1.btnLogClick(Sender: TObject);
begin
  if (Sender=btnClearLog) then
  begin
    {$ifdef READER}
    CommandOutputScreen.Clear;
    {$else}
    CommandOutputScreen.ClearAll;
    {$endif}
    memoSummary.Clear;
  end;
  if (Sender=btnSendLog) then
  begin
    memoSummary.Lines.Append('');
    memoSummary.Lines.Append('Sending email to "fpcupdeluxe@gmail.com" with content of command screen !');
    SendMail('smtp.gmail.com',
             'Fpcupdeluxe log report',
             'fpcupdeluxe@gmail.com',
             'upuser@gmail.com',
             'fpcupdeluxe@gmail.com',
             'W0/0MT6Veeg7CN5R',
             CommandOutputScreen.Lines);
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  sInstallDir:=SetDirSeparators(InstallDirEdit.Text);
  Form2.ResetAll;
  if DirectoryExists(sInstallDir) then GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir));
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  sInstallDir:=SetDirSeparators(InstallDirEdit.Text);
  if DirectoryExists(sInstallDir) then GetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir));
end;

procedure TForm1.FPCVersionLabelClick(Sender: TObject);
begin
  MessageTrigger:=True;
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
  Self.OnResize:=nil;
  if Assigned(FPCupManager) then
  begin
    Application.MainForm.Cursor:=crHourGlass;
    with TMemIniFile.Create(SafeGetApplicationPath+installerUniversal.DELUXEFILENAME) do
    try
      WriteString('General','InstallDirectory',sInstallDir);

      // Termorary store of target in app directory
      WriteBool('General','Gitlab',chkGitlab.Checked);
      WriteString('General','fpcVersion',FPCTarget);
      WriteString('General','lazVersion',LazarusTarget);

      {$ifdef RemoteLog}
      WriteBool('General','ConsentWarning',false);
      {$endif}

      {$ifdef EnableLanguages}
      WriteString('General','Language',sLanguage);
      {$endif}

      if Assigned(FPCupManager) then
      begin
        WriteString('ProxySettings','HTTPProxyURL',FPCupManager.HTTPProxyHost);
        WriteInteger('ProxySettings','HTTPProxyPort',FPCupManager.HTTPProxyPort);
        WriteString('ProxySettings','HTTPProxyUser',FPCupManager.HTTPProxyUser);
        WriteString('ProxySettings','HTTPProxyPass',FPCupManager.HTTPProxyPassword);
      end;

      UpdateFile;
    finally
      Free;
    end;

    SetFPCUPSettings(IncludeTrailingPathDelimiter(sInstallDir));

    FPCupManager.Destroy;
    FPCupManager:=nil;

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
    if (Sender=nil) then
    begin
      if ((Components[i] is TMenuItem)) then
      begin
        TMenuItem(Components[i]).Enabled:=value;
      end;
    end;
    if (NOT (Components[i] is TControl)) then continue;
    c := Components[i] AS TControl;
    if (Sender<>nil) then
    begin
      if c is TLabel then continue;
      if c is TPanel then continue;
      if c is TGroupBox then continue;
      if c = BitBtnHalt then continue;
      if c = CommandOutputScreen then continue;
      if c = memoSummary then continue;
    end;
    c.Enabled := value;
    {$ifdef usealternateui}
    if ((pos('Halt',c.name)>0) or (pos('Halt',c.caption)>0)) then c.Enabled:=true;
    {$endif}
  end;
end;

function TForm1.PrepareRun(Sender: TObject):boolean;
var
  s:string;
begin
  result:=false;

  if Assigned(Sender) then
  begin
    s:='';
    if (Sender<>BitBtnLazarusOnly) AND AnsiEndsText('.svn',FPCTarget) then
    begin
      s:='You have selected a FPC source from SVN';
    end;
    if (Sender<>BitBtnFPCOnly) AND AnsiEndsText('.svn',LazarusTarget) then
    begin
      if (Length(s)>0) then
        s:=s+' and y'
      else
        s:='Y';
      s:=s+'ou have selected a Lazarus source from SVN';
    end;
    if (Length(s)>0) then
    begin
      s:=s+'.'+LineEnding+'Please select another source: SVN is no longer available.';
      Application.MessageBox(PChar(s), PChar('SVN source error'), MB_ICONSTOP);
      exit;
    end;
  end;

  FPCVersionLabel.Font.Color:=clDefault;
  LazarusVersionLabel.Font.Color:=clDefault;

  RealFPCURL.Color:=clDefault;
  RealLazURL.Color:=clDefault;

  if CheckAutoClear.Checked then btnClearLog.Click;

  MissingCrossBins:=false;
  MissingCrossLibs:=false;
  MissingTools:=false;

  FPCupManager.ResetAll;

  FPCupManager.NoJobs:=(NOT Form2.MakeJobs);

  FPCupManager.FPCUnicode:=Form2.FPCUnicode;

  FPCupManager.SoftFloat:=Form2.UseSoftFloat;
  FPCupManager.OnlinePatching:=Form2.OnlinePatching;
  FPCupManager.ReApplyLocalChanges:=Form2.ApplyLocalChanges;

  FPCupManager.LinuxLegacy:=Form2.ForceGLIBCLinking;

  FPCupManager.FPCOPT:=Form2.FPCOptions;
  if Form2.FPCDebug then
  begin
    FPCupManager.FPCOPT:=FPCupManager.FPCOPT+' -g -gl -O1';
    FPCupManager.FPCOPT:=Trim(FPCupManager.FPCOPT);
  end;

  {$IFDEF DEBUG}
  FPCupManager.Verbose:=True;
  {$ELSE}
  FPCupManager.Verbose:=Form2.ExtraVerbose;
  {$ENDIF}

  FPCupManager.LazarusOPT:=Form2.LazarusOptions;
  if Form2.LazarusDebug then
  begin
    FPCupManager.LazarusOPT:=FPCupManager.LazarusOPT+' -g -gl -O1';
    FPCupManager.LazarusOPT:=Trim(FPCupManager.LazarusOPT);
  end;

  FPCupManager.UseSystemFPC:=Form2.SystemFPC;

  FPCupManager.UseWget:=Form2.UseWget;

  // set custom FPC compiler by special user input through setup+
  FPCupManager.CompilerOverride:=Form2.GetCompiler(GetTCPU(GetSourceCPU),GetTOS(GetSourceOS),TSUBARCH.saNone);

  sInstallDir:=ExcludeTrailingPathDelimiter(sInstallDir);
  FPCupManager.BaseDirectory:=sInstallDir;

  // do not create shortcut for fpcupeluxe itself: we have already fpcupdeluxe by itself !!
  //FPCupManager.ShortCutNameFpcup:='fpcup_'+ExtractFileName(sInstallDir)+'_update';
  FPCupManager.ShortCutNameFpcup:=EmptyStr;

  FPCupManager.ShortCutNameLazarus:='Lazarus_'+ExtractFileName(sInstallDir);

  sInstallDir:=IncludeTrailingPathDelimiter(sInstallDir);

  FPCupManager.MakeDirectory:=sInstallDir+DEFAULTBOOTSTRAPDIR;
  FPCupManager.BootstrapCompilerDirectory:=sInstallDir+DEFAULTBOOTSTRAPDIR;

  // Set default logfile location
  FPCupManager.LogFileName:='';

  FPCupManager.FPCInstallDirectory:=sInstallDir+'fpc';
  if Form2.SplitFPC
     then FPCupManager.FPCSourceDirectory:=FPCupManager.FPCInstallDirectory+'src'
     else FPCupManager.FPCSourceDirectory:=FPCupManager.FPCInstallDirectory;

  FPCupManager.LazarusInstallDirectory:=sInstallDir+'lazarus';
  if Form2.SplitLazarus
     then FPCupManager.LazarusSourceDirectory:=FPCupManager.LazarusInstallDirectory+'src'
     else FPCupManager.LazarusSourceDirectory:=FPCupManager.LazarusInstallDirectory;

  FPCupManager.LazarusPrimaryConfigPath:=sInstallDir+'config_'+ExtractFileName(FPCupManager.LazarusInstallDirectory);

  FPCupManager.ExportOnly:=(NOT Form2.Repo);

  FPCupManager.FPCPatches:=Form2.FPCPatches;
  FPCupManager.LazarusPatches:=Form2.LazPatches;
  FPCupManager.NativeFPCBootstrapCompiler:=(NOT Form2.FpcupBootstrappersOnly);
  FPCupManager.ForceLocalRepoClient:=Form2.ForceLocalRepoClient;
  FPCupManager.Context:=Form2.AddContext;

  // Set default Darwin LCL platforms
  {$ifdef Darwin}
    FPCupManager.LCL_Platform:='cocoa';
    {$ifdef LCLCARBON}
      FPCupManager.LCL_Platform:='carbon';
    {$endif}
  {$endif}

  // Override default LCL platforms in case of QT[5]
  {$ifdef LCLQT}
    FPCupManager.LCL_Platform:='qt';
  {$endif}
  {$ifdef LCLQT5}
    FPCupManager.LCL_Platform:='qt5';
  {$endif}

  {$ifdef RemoteLog}
  aDataClient.Enabled:=Form2.SendInfo;
  aDataClient.UpInfo.UpFunction:=ufUnknown;
  aDataClient.ClearExtraData;
  aDataClient.UpInfo.CrossCPUOS:='';
  aDataClient.UpInfo.LogEntry:='';
  {$endif}

  sStatus:='Sitting and waiting';
  StatusMessage.Color:=clDefault;
  StatusMessage.Text:=sStatus;

  if CheckAutoClear.Checked then memoSummary.Lines.Clear;

  result:=true;
end;

function TForm1.RealRun:boolean;
var
  s:string;
  aLazarusVersion:word;
  crossing:boolean;
  {$ifdef RemoteLog}
  aRecordNumber:PtrUInt;
  {$endif}
begin
  result:=false;

  if Pos(' ',FPCupManager.BaseDirectory)>0 then
  begin
    if (MessageDlgEx(upSpaceWarning+sLineBreak+upQuestionContinue,mtConfirmation,[mbYes, mbNo],Self)<>mrYes) then
      exit;
  end;

  crossing:=((FPCupManager.CrossCPU_Target<>TCPU.cpuNone) AND (FPCupManager.CrossOS_Target<>TOS.osNone));

  StatusMessage.Text:=sStatus;

  AddMessage('FPCUP(deluxe) is starting up.');
  AddMessage('');
  AddMessage('FPCupdeluxe basedir:       '+FPCupManager.BaseDirectory);
  {$IFDEF MSWINDOWS}
  AddMessage('Binutils dir:              '+FPCupManager.MakeDirectory);
  {$ENDIF MSWINDOWS}
  AddMessage('Bootstrap dir:             '+FPCupManager.BootstrapCompilerDirectory);

  {$IF (defined(BSD)) and (not defined(Darwin))}
  FPCupManager.FPCOpt:=FPCupManager.FPCOpt+' -Fl/usr/local/lib -Fl/usr/pkg/lib';
  FPCupManager.LazarusOpt:=FPCupManager.LazarusOpt+' -Fl/usr/local/lib -Fl/usr/X11R6/lib -Fl/usr/pkg/lib -Fl/usr/X11R7/lib';
  {$endif}

  if AnsiEndsText(GITLABEXTENSION,FPCTarget) then
  begin
    FPCupManager.FPCTag:=FPCTarget;
  end
  else
  begin
    FPCupManager.FPCURL:=FPCTarget;
    if (Pos('github.com/LongDirtyAnimAlf',FPCupManager.FPCURL)>0) then FPCupManager.FPCBranch:='master';
    if (Pos('github.com/ultibohub/FreePascal',FPCupManager.FPCURL)>0) then FPCupManager.FPCBranch:='ultibo';
  end;

  if AnsiEndsText(GITLABEXTENSION,LazarusTarget) then
  begin
    FPCupManager.LazarusTag:=LazarusTarget;
  end
  else
  begin
    FPCupManager.LazarusURL:=LazarusTarget;
    if (Pos('github.com/LongDirtyAnimAlf',FPCupManager.LazarusURL)>0) then FPCupManager.LazarusBranch:='upstream';
    if (Pos('github.com/LongDirtyAnimAlf/lazarussource',FPCupManager.LazarusURL)>0) then FPCupManager.LazarusBranch:='master';
    if (Pos('github.com/ultibohub/LazarusIDE',FPCupManager.LazarusURL)>0) then FPCupManager.LazarusBranch:='ultibo';
  end;

  if (NOT crossing) then
  begin
    // branch and revision overrides from setup+
    s:=Form2.FPCRevision;
    if Length(s)>0 then FPCupManager.FPCDesiredRevision:=s;
    s:=Form2.FPCBranch;
    if Length(s)>0 then FPCupManager.FPCBranch:=s;

    s:=Form2.LazarusRevision;
    if Length(s)>0 then FPCupManager.LazarusDesiredRevision:=s;
    s:=Form2.LazarusBranch;
    if Length(s)>0 then FPCupManager.LazarusBranch:=s;

    // overrides for old versions of Lazarus
    aLazarusVersion:=CalculateNumericalVersion(LazarusTarget);
    if (aLazarusVersion<>0) AND (aLazarusVersion<CalculateFullVersion(1,0,0)) then
    begin
      s:=FPCupManager.OnlyModules;
      if (Length(s)>0) then
      begin
        if Pos(_LAZARUSSIMPLE,s)=0 then s:=StringReplace(s,_LAZARUS,_LAZARUSSIMPLE,[]);
        {$ifdef mswindows}
        {$ifdef win32}
        s:=StringReplace(s,_FPC+_CROSSWIN,'',[]);
        s:=StringReplace(s,_LAZARUS+_CROSSWIN,'',[]);
        {$endif}
        {$endif}
        FPCupManager.OnlyModules:=s;
      end
      else
      begin
        FPCupManager.OnlyModules:=_FPC+','+_LAZARUSSIMPLE;
      end;
      AddMessage('Detected a very old version of Lazarus !');
      AddMessage('Switching towards old lazarus sequence !!');
    end;
  end;

  AddMessage('');

  AddMessage('FPC URL:                   '+FPCupManager.FPCURL);
  AddMessage('FPC source directory:      '+FPCupManager.FPCSourceDirectory);
  AddMessage('FPC install directory:     '+FPCupManager.FPCInstallDirectory);
  AddMessage('FPC options:               '+FPCupManager.FPCOPT);

  AddMessage('');

  AddMessage('Lazarus URL:               '+FPCupManager.LazarusURL);
  AddMessage('Lazarus source directory:  '+FPCupManager.LazarusSourceDirectory);
  AddMessage('Lazarus install directory: '+FPCupManager.LazarusInstallDirectory);
  AddMessage('Lazarus options:           '+FPCupManager.LazarusOPT);

  AddMessage('');
  AddMessage('Please stand back and enjoy !');
  AddMessage('');

  //save install settings in install directory
  SetFPCUPSettings(IncludeTrailingPathDelimiter(FPCupManager.BaseDirectory));

  Application.ProcessMessages;

  {$ifdef RemoteLog}
  aDataClient.UpInfo.FPCVersion:=FPCTarget;
  aDataClient.UpInfo.LazarusVersion:=LazarusTarget;
  aDataClient.UpInfo.UpInstallDir:=FPCupManager.BaseDirectory;
  {$endif}

  AddMessage(DateTimeToStr(now)+': '+BeginSnippet+' V'+RevisionStr+' ('+VersionDate+') started.');
  AddMessage('FPCUPdeluxe V'+DELUXEVERSION+' for '+GetSourceCPUOS+' running on '+GetDistro);
  AddMessage('Build with: FPC '+GetFPCBuildVersion + ' on Win11 x86_64');
  AddMessage('');

  if Form2.SaveScript then FPCupManager.SaveSettings;

  BitBtnHalt.Enabled:=true;
  try
    {$ifdef READER}
    try
      //Form1.SetFocusedControl(BitBtnHalt);
      CommandOutputScreen.Enabled:=false;
      result:=FPCupManager.Run;
    finally
      //Form1.SetFocusedControl(btnInstallDirSelect);
      CommandOutputScreen.Enabled:=true;
    end;
    {$else}
    result:=FPCupManager.Run;
    {$endif}
    if (NOT result) then
    begin
      AddMessage('');
      AddMessage('');

      MissingCrossBins:=(ieBins in FPCupManager.InstallerErrors);
      MissingCrossLibs:=(ieLibs in FPCupManager.InstallerErrors);

      if (MissingCrossBins OR MissingCrossLibs) then
      begin
        if MissingCrossBins then AddMessage('fpcupdeluxe: ERROR: Failure due to missing cross binary tools.');
        if MissingCrossLibs then AddMessage('fpcupdeluxe: ERROR: Failure due to missing cross libraries.');
        AddMessage('');
      end
      else
      begin
        AddMessage('ERROR: Fpcupdeluxe fatal error !');
        // skip reporting trivial errors about missing things
        if (NOT MissingTools) then
        begin
          StatusMessage.Text:='Hmmm, something went wrong ... have a good look at the command screen !';
          AddMessage(FPCupManager.RunInfo);
          {$ifdef RemoteLog}
          aDataClient.UpInfo.LogEntry:=memoSummary.Text+LineEnding+FPCupManager.RunInfo;
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
      AddMessage('SUCCESS: installation by fpcupdeluxe complete !');
      AddMessage('');

      if FPCupManager.ShortcutCreated then
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
      end;
      AddMessage('');

      FPCVersionLabel.Font.Color:=clLime;
      LazarusVersionLabel.Font.Color:=clLime;
      StatusMessage.Text:='That went well !!!';

      s:=FPCupManager.FPCDesiredRevision;
      if Length(s)>0 then AddRevision(True,False,s,'-',Now);
      s:=FPCupManager.LazarusDesiredRevision;
      if Length(s)>0 then AddRevision(False,True,s,'-',Now);

      {$ifdef RemoteLog}
      aDataClient.UpInfo.LogEntry:='Success !';
      aRecordNumber:=aDataClient.SendData;
      if (aRecordNumber>0) then
      begin
        //AddMessage('SUCCESS ! Info has been send to fpcupdeluxe logger.');
        //AddMessage('');
      end;
      {$endif}
    end;

    memoSummary.Lines.Append(BeginSnippet+' Done !!');

  except
    on E: Exception do
    begin
      StatusMessage.Text:='Exception: '+ E.ClassName + '. ' + E.Message;
      //StatusMessage.Text:=BeginSnippet+' Got an unexpected exception ... don''t know what to do unfortunately.';
      StatusMessage.Color:=clRed;
    end;
  end;

  BitBtnHalt.Enabled:=false;
end;

function TForm1.GetFPCUPSettings(IniDirectory:string):boolean;
var
  aStoredTarget:string;
begin
  result:=FileExists(IniDirectory+installerUniversal.DELUXEFILENAME);

  GetSystemInfo;

  if result then
  begin
    with TIniFile.Create(IniDirectory+installerUniversal.DELUXEFILENAME) do
    try
      AddMessage(upInstallSettingsCurrent+'.');
      AddMessage('');

      chkGitlab.Checked:=ReadBool('General','Gitlab',chkGitlab.Checked);

      FPCupManager.ExportOnly:=(NOT ReadBool('General','GetRepo',True));

      // Read FPC target from settngs
      aStoredTarget:=ReadString('Target','fpcVersion','');
      if (Length(aStoredTarget)=0) then
      begin
        //Fallback to previous settings
        aStoredTarget:=ReadString('URL','fpcURL','');
        if (Length(aStoredTarget)>0) then
          aStoredTarget:=installerUniversal.GetKeyword(FPCURLLOOKUPMAGIC,aStoredTarget);
      end;
      if (Length(aStoredTarget)<>0) then
        FPCTarget:=aStoredTarget;

      // Read Lazarus target from settngs
      aStoredTarget:=ReadString('Target','lazVersion','');
      if (Length(aStoredTarget)=0) then
      begin
        //Fallback to previous settings
        aStoredTarget:=ReadString('URL','lazURL','');
        if (Length(aStoredTarget)>0) then
          aStoredTarget:=installerUniversal.GetKeyword(LAZARUSURLLOOKUPMAGIC,aStoredTarget);
      end;
      if (Length(aStoredTarget)<>0) then
        LazarusTarget:=aStoredTarget;

      radgrpCPU.ItemIndex:=ReadInteger('Cross','CPUTarget',radgrpCPU.ItemIndex);
      radgrpOS.ItemIndex:=ReadInteger('Cross','OSTarget',radgrpOS.ItemIndex);

      if (listModules.Items.Count>0) then listModules.ItemIndex:=ReadInteger('General','Module',listModules.ItemIndex);

      Form2.FPCOptions:=ReadString('General','FPCOptions',Form2.FPCOptions);
      Form2.LazarusOptions:=ReadString('General','LazarusOptions','');

      Form2.FPCDebug:=ReadBool('General','FPCDebug',Form2.FPCDebug);
      Form2.LazarusDebug:=ReadBool('General','LazarusDebug',Form2.LazarusDebug);

      Form2.FPCRevision:=ReadString('General','FPCRevision','');
      Form2.LazarusRevision:=ReadString('General','LazarusRevision','');
      Form2.FPCBranch:=ReadString('General','FPCBranch','');
      Form2.LazarusBranch:=ReadString('General','LazarusBranch','');

      Form2.SplitFPC:=ReadBool('General','SplitFPC',Form2.SplitFPC);
      Form2.SplitLazarus:=ReadBool('General','SplitLazarus',Form2.SplitLazarus);
      Form2.DockedLazarus:=ReadBool('General','DockedLazarus',Form2.DockedLazarus);

      Form2.UseWget:=ReadBool('General','UseWget',Form2.UseWget);
      Form2.MakeJobs:=ReadBool('General','MakeJobs',Form2.MakeJobs);
      Form2.FPCUnicode:=ReadBool('General','BuildFPCUnicode',Form2.FPCUnicode);

      Form2.ExtraVerbose:=ReadBool('General','ExtraVerbose',False);
      Form2.UpdateOnly:=ReadBool('General','UpdateOnly',False);

      Form2.UseSoftFloat:=ReadBool('General','UseSoftFloat',Form2.UseSoftFloat);
      Form2.OnlinePatching:=ReadBool('General','OnlinePatching',Form2.OnlinePatching);
      Form2.ApplyLocalChanges:=ReadBool('General','ApplyLocalChanges',Form2.ApplyLocalChanges);

      Form2.SystemFPC:=ReadBool('General','SystemFPC',False);

      Form2.ForceGLIBCLinking:=ReadBool('General','LinuxLegacy',Form2.ForceGLIBCLinking);

      Form2.FPCPatches:=ReadString('Patches','FPCPatches','');
      Form2.LazPatches:=ReadString('Patches','LazarusPatches','');

      Form2.FpcupBootstrappersOnly:=ReadBool('General','FpcupBootstrappersOnly',Form2.FpcupBootstrappersOnly);

      Form2.ForceLocalRepoClient:=ReadBool('General','ForceLocalRepoClient',Form2.ForceLocalRepoClient);
    finally
      Free;
    end;

    Form2.SetInstallDir(IniDirectory);

    ParseRevisions(IniDirectory);
  end
  else
  begin
    AddMessage(upInstallDirectoryCurrent+': '+sInstallDir);
    {$ifdef Solaris}
    // current trunk does not build with the standard -O2, so use -O1 for all
    Form2.FPCOptions:='-g -gl -O1';
    FPCupManager.FPCOPT:=Form2.FPCOptions;
    {$endif}
  end;
end;

procedure TForm1.GetSystemInfo;
var
  Cores,MemAvailable,SwapAvailable:DWord;
  {$ifdef LCLQT5}
  QT5LibraryLocation:string;
  {$endif}
begin
  {$ifdef READER}
  CommandOutputScreen.Clear;
  {$else}
  CommandOutputScreen.ClearAll;
  {$endif}

  if (CommandOutputScreen.Lines.Count>0) then
    CommandOutputScreen.Lines.Strings[0]:='Welcome @ FPCUPdeluxe.'
  else
    AddMessage('Welcome @ FPCUPdeluxe.');

  AddMessage(Self.Caption);
  {$ifndef NetBSD}
  AddMessage('Running on '+GetDistro);
  AddMessage('Build with: FPC '+GetFPCBuildVersion + ' on Win11 x86_64');
  {$ifdef FreeBSD}
  AddMessage('Detected mayor FreeBSD version '+InttoStr(GetFreeBSDVersion));
  {$endif FreeBSD}
  {$endif NetBSD}

  {$IFDEF LINUX}
  if IsLinuxMUSL then AddMessage('Seems we are running on a MUSL Linux.');
  {$ENDIF LINUX}

  {$ifdef LCLQT5}
  if LibWhich(LIBQT5,QT5LibraryLocation) then
    AddMessage('Found system wide '+LIBQT5+' in '+QT5LibraryLocation+'. And that will be used.')
  else
    AddMessage('No system wide '+LIBQT5+' found. Some QT5 trickery will be used');
  {$endif}
  {$ifdef LCLQT6}
  if LibWhich(LIBQT6,QT6LibraryLocation) then
    AddMessage('Found system wide '+LIBQT6+' in '+QT6LibraryLocation+'. And that will be used.')
  else
    AddMessage('No system wide '+LIBQT6+' found. Some QT6 trickery will be used');
  {$endif}

  Cores:=GetLogicalCpuCount;
  if Cores<>0 then AddMessage('CPU cores used: '+InttoStr(Cores));
  MemAvailable:=GetTotalPhysicalMemory;
  if (MemAvailable<>0) then AddMessage('Available physical memory: '+InttoStr(MemAvailable)+' MB');

  SwapAvailable:=GetSwapFileSize;

  {$IFDEF LINUX}
  AddMessage('Available swap: '+InttoStr(SwapAvailable)+' MB');
  {$ENDIF}

  MemAvailable:=MemAvailable+SwapAvailable;

  if (MemAvailable<>0) AND (MemAvailable<1500) then
  begin
    AddMessage('Please be warned: memory is very limited for building Lazarus');
    {$IFDEF UNIX}
    AddMessage('Memory advice: Please add (more) swap space !!');
    AddMessage('Memory advice: To build Lazarus, you will need (at least) 1GB of (swap-)RAM space.');
    memoSummary.Lines.Append(BeginSnippet+' Most likely, there will not enough RAM (swap) to build Lazarus.');
    memoSummary.Lines.Append(BeginSnippet+' Expected error: Can''t call the assembler');
    memoSummary.Lines.Append(BeginSnippet+' Expected error:  Can''t call the resource compiler');
    memoSummary.Lines.Append(BeginSnippet+' Please add some RAM or swap-space (+1GB) and re-run fpcupdeluxe.');
    {$ENDIF UNIX}
  end;

  AddMessage('');
  AddMessage(upInstallDirectoryCurrent+': '+sInstallDir);
  AddMessage('');

  // get names of cross-compilers
  AutoUpdateCrossCompiler(nil);
  AddMessage('');

 {$ifdef usealternateui}
 alternateui_update_interface_buttons;
 {$endif}
end;


function TForm1.SetFPCUPSettings(IniDirectory:string):boolean;
var
  aDir:string;
begin
  result:=false;

  if NOT Assigned(FPCupManager) then exit;
  if NOT Assigned(Form2) then exit;

  aDir:=ExtractFileDir(IncludeTrailingPathDelimiter(IniDirectory)+installerUniversal.DELUXEFILENAME);
  result:=DirectoryExists(aDir);

  if (NOT result) then exit;

  try
    with TMemIniFile.Create(aDir+DirectorySeparator+installerUniversal.DELUXEFILENAME) do
    try
      WriteString('General','InstallDirectory',sInstallDir);

      WriteBool('General','Gitlab',chkGitlab.Checked);

      // mmm, is this correct ?  See extrasettings !!
      WriteBool('General','GetRepo',(NOT FPCupManager.ExportOnly));

      if FPCTarget<>'skip' then
      begin
        if (ListBoxFPCTarget.ItemIndex<>-1) then
          WriteString('Target','fpcVersion',ListBoxFPCTarget.GetSelectedText);
      end;

      if LazarusTarget<>'skip' then
      begin
        if (ListBoxLazarusTarget.ItemIndex<>-1) then
          WriteString('Target','lazVersion',ListBoxLazarusTarget.GetSelectedText);
      end;

      if (radgrpCPU.ItemIndex<>-1) then WriteInteger('Cross','CPUTarget',radgrpCPU.ItemIndex);
      if (radgrpOS.ItemIndex<>-1) then WriteInteger('Cross','OSTarget',radgrpOS.ItemIndex);

      if ((listModules.Items.Count>0) AND (listModules.ItemIndex<>-1)) then WriteInteger('General','Module',listModules.ItemIndex);

      WriteString('General','FPCOptions',Form2.FPCOptions);
      WriteString('General','LazarusOptions',Form2.LazarusOptions);

      WriteBool('General','FPCDebug',Form2.FPCDebug);
      WriteBool('General','LazarusDebug',Form2.LazarusDebug);

      WriteString('General','FPCRevision',Form2.FPCRevision);
      WriteString('General','LazarusRevision',Form2.LazarusRevision);
      WriteString('General','FPCBranch',Form2.FPCBranch);
      WriteString('General','LazarusBranch',Form2.LazarusBranch);

      WriteBool('General','SplitFPC',Form2.SplitFPC);
      WriteBool('General','SplitLazarus',Form2.SplitLazarus);
      WriteBool('General','DockedLazarus',Form2.DockedLazarus);

      WriteBool('General','SystemFPC',Form2.SystemFPC);

      WriteBool('General','LinuxLegacy',Form2.ForceGLIBCLinking);

      WriteBool('General','UseWget',Form2.UseWget);
      WriteBool('General','MakeJobs',Form2.MakeJobs);
      WriteBool('General','BuildFPCUnicode',Form2.FPCUnicode);
      WriteBool('General','ExtraVerbose',Form2.ExtraVerbose);
      WriteBool('General','UpdateOnly',Form2.UpdateOnly);
      WriteBool('General','UseSoftFloat',Form2.UseSoftFloat);
      WriteBool('General','OnlinePatching',Form2.OnlinePatching);
      WriteBool('General','ApplyLocalChanges',Form2.ApplyLocalChanges);

      WriteString('Patches','FPCPatches',Form2.FPCPatches);
      WriteString('Patches','LazarusPatches',Form2.LazPatches);

      WriteBool('General','FpcupBootstrappersOnly',Form2.FpcupBootstrappersOnly);

      WriteBool('General','ForceLocalRepoClient',Form2.ForceLocalRepoClient);

      UpdateFile;
    finally
      Free;
    end;

    result:=FileExists(IniDirectory+installerUniversal.DELUXEFILENAME);

  except
    on E: Exception do
    begin
      //Infoln(installerUniversal.DELUXEFILENAME+': File creation error: '+E.Message,etError);
    end;
  end;

end;

procedure TForm1.AddMessage(const aMessage:string; const UpdateStatus:boolean=false);
var
  aMessageStrings:TStrings;
begin
  if Length(aMessage)=0 then
    CommandOutputScreen.Lines.Append('')
  else
  begin
    aMessageStrings:=TStringList.Create;
    try
      aMessageStrings.Text:=aMessage;
      CommandOutputScreen.Lines.AddStrings(aMessageStrings);
      if UpdateStatus then StatusMessage.Text:=aMessageStrings[0];
    finally
      aMessageStrings.Free;
    end;
  end;
  {$ifdef READER}
  CommandOutputScreen.CaretPos.SetLocation(0,CommandOutputScreen.Lines.Count);
  {$else}
  CommandOutputScreen.CaretX:=0;
  CommandOutputScreen.CaretY:=CommandOutputScreen.Lines.Count;
  {$endif}

  {$ifdef usealternateui}
  alternateui_AddMessage(amessage,updatestatus);
  {$endif}
  Application.ProcessMessages;
end;

procedure TForm1.TargetSelectionChange(Sender: TObject; User: boolean);
begin
  if (NOT User) then exit;
  SetTarget(TListBox(Sender));
end;

procedure TForm1.SetFPCTarget(aFPCTarget:string);
begin
  SetTarget(RealFPCURL,aFPCTarget);
end;

procedure TForm1.SetLazarusTarget(aLazarusTarget:string);
begin
  SetTarget(RealLazURL,aLazarusTarget);
end;

procedure TForm1.SetTarget(aControl:TControl;const aTarget:string='');
var
  i:integer;
  aEdit:TEdit;
  aListBox:TListBox;
  change:boolean;
  aLocalTarget:string;
  aLocalAlias:string;
begin
  aEdit:=nil;
  aListBox:=nil;
  change:=false;
  aLocalTarget:=aTarget;

  if (aControl is TEdit) then aEdit:=TEdit(aControl);
  if (aControl is TListBox) then aListBox:=TListBox(aControl);

  if (aListBox=nil) then
  begin
    if aEdit=RealFPCURL then
      aListBox:=ListBoxFPCTarget;
    if aEdit=RealLazURL then
      aListBox:=ListBoxLazarusTarget;
  end;

  if (aEdit=nil) then
  begin
    if aListBox=ListBoxFPCTarget then
      aEdit:=RealFPCURL;
    if aListBox=ListBoxLazarusTarget then
      aEdit:=RealLazURL;
  end;

  if (aListBox.HandleAllocated) AND (aListBox.ItemIndex<>-1) AND (Length(aLocalTarget)=0) then aLocalTarget:=aListBox.GetSelectedText;

  if (aEdit=nil) OR (aListBox=nil) OR (Length(aLocalTarget)=0)then
  begin
    ShowMessage('Something serious went wrong when setting target FPC and/or Lazarus target.');
    exit;
  end;

  if (aListBox=ListBoxFPCTarget) then
  begin
    if AnsiEndsText(GITLABEXTENSION,aLocalTarget) then
    begin
      aLocalAlias:=installerUniversal.GetAlias(FPCBRANCHLOOKUPMAGIC,aLocalTarget);
      if (Length(aLocalAlias)=0) then aLocalAlias:=installerUniversal.GetAlias(FPCTAGLOOKUPMAGIC,aLocalTarget);
      if (Length(aLocalAlias)=0) then
      begin
        //  Store the value provided as a new tag
        aLocalAlias:=Copy(aLocalTarget,1,Length(aLocalTarget)-Length(GITLABEXTENSION));
        AddTag(aListBox,aLocalAlias);
        // Default to stable in case of lookup failure
        //aLocalTarget:='stable'+GITLABEXTENSION;
      end;
      if (Pos('://',aLocalAlias)=0) then aLocalAlias:=FPCGITLABREPO;
    end
    else
      aLocalAlias:=installerUniversal.GetAlias(FPCURLLOOKUPMAGIC,aLocalTarget);
  end;
  if (aListBox=ListBoxLazarusTarget) then
  begin
    if AnsiEndsText(GITLABEXTENSION,aLocalTarget) then
    begin
      aLocalAlias:=installerUniversal.GetAlias(LAZARUSBRANCHLOOKUPMAGIC,aLocalTarget);
      if (Length(aLocalAlias)=0) then aLocalAlias:=installerUniversal.GetAlias(LAZARUSTAGLOOKUPMAGIC,aLocalTarget);
      if (Length(aLocalAlias)=0) then
      begin
        //  Store the value provided as a new tag
        aLocalAlias:=Copy(aLocalTarget,1,Length(aLocalTarget)-Length(GITLABEXTENSION));
        AddTag(aListBox,aLocalAlias);
        // Default to stable in case of lookup failure
        //aLocalTarget:='stable'+GITLABEXTENSION;
      end;
      if (Pos('://',aLocalAlias)=0) then aLocalAlias:=LAZARUSGITLABREPO;
    end
    else
      aLocalAlias:=installerUniversal.GetAlias(LAZARUSURLLOOKUPMAGIC,aLocalTarget);
  end;
  //i:=Pos('.git',aLocalAlias);
  //if (i>0) then
  //  Delete(aLocalAlias,i,4);

  if (aListBox=ListBoxFPCTarget) then
  begin
    change:=(aLocalTarget<>FFPCTarget);
    if change then FFPCTarget:=aLocalTarget;
  end;
  if (aListBox=ListBoxLazarusTarget) then
  begin
    change:=(aLocalTarget<>FLazarusTarget);
    if change then FLazarusTarget:=aLocalTarget;
  end;

  //if (change AND (aListBox.HandleAllocated)) then
  begin
    if (NOT (aControl is TListBox)) then
    begin
      if (aListBox.Items.Count>0) then
      begin
        i:=aListBox.Items.IndexOf(aLocalTarget);
        if (i<>-1) then
          aListBox.Selected[i]:=true
        else
          aListBox.ClearSelection;
      end;
    end;
    aEdit.Text:=aLocalAlias;
  end;
end;

procedure TForm1.ButtonSubarchSelectClick(Sender: TObject);
var
  s:string;
begin
  if radgrpCPU.ItemIndex<>-1 then
  begin
    s:=radgrpCPU.Items[radgrpCPU.ItemIndex];
    FPCupManager.CrossCPU_Target:=GetTCPU(s);
  end;

  if radgrpOS.ItemIndex<>-1 then
  begin
    s:=radgrpOS.Items[radgrpOS.ItemIndex];
    FPCupManager.CrossOS_Target:=GetTOS(s);
  end;

  SubarchForm.SetCrossTarget(FPCupManager,FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target);

  SubarchForm.ShowModal;

  if SubarchForm.ModalResult=mrOk then
  begin
    AddMessage('Fpcupdeluxe: selected subarch = '+GetSubarch(GetSelectedSubArch(FPCupManager.CrossCPU_Target,FPCupManager.CrossOS_Target)));
  end;
end;

procedure TForm1.chkGitlabChange(Sender: TObject);
begin
  //RealFPCURL.Visible:=(NOT TCheckBox(Sender).Checked);
  //RealLazURL.Visible:=(NOT TCheckBox(Sender).Checked);

  imgSVN.Visible:=(NOT TCheckBox(Sender).Checked);
  imgGitlab.Visible:=(TCheckBox(Sender).Checked);

  //WioBtn.Enabled:=(NOT TCheckBox(Sender).Checked);
  //PicoBtn.Enabled:=(NOT TCheckBox(Sender).Checked);
  //UltiboBtn.Enabled:=(NOT TCheckBox(Sender).Checked);

  FillSourceListboxes;

  ScrollToSelected;
end;

procedure TForm1.CommandOutputScreenChange(Sender: TObject);
begin

end;

procedure TForm1.HandleInfo(var Msg: TLMessage);
var
  MsgStr: PChar;
  //MsgPStr: PString;
  MsgPasStr: string;
begin
  MsgStr := {%H-}PChar(Msg.lParam);
  MsgPasStr := StrPas(MsgStr);
  //MsgPStr := PString(Msg.lParam);
  //MsgPasStr := MsgPStr^;
  try
    {$ifdef Darwin}
    // suppress all setfocus errors on Darwin, always
    if AnsiContainsText(MsgPasStr,'.setfocus') then exit;
    {$endif}

    {$ifdef READER}
    CommandOutputScreen.Append(MsgPasStr);
    {$else}
    // suppress all SynEdit PaintLock errors, always
    if AnsiContainsText(MsgPasStr,'PaintLock') then exit;
    {$if defined(FPC_FULLVERSION) and (FPC_FULLVERSION > 30000)}
    CommandOutputScreen.Append(MsgPasStr);
    {$else}
    CommandOutputScreen.Lines.Append(MsgPasStr);
    {$endif}
    CommandOutputScreen.CaretX:=0;
    CommandOutputScreen.CaretY:=CommandOutputScreen.Lines.Count;
    {$endif}

    ProcessInfo(CommandOutputScreen);

    if (ExistWordInString(MsgStr,'error:',[soWholeWord])) OR (ExistWordInString(MsgStr,'fatal:',[soWholeWord])) then
    begin
      if (ExistWordInString(MsgStr,'failed to get crossbinutils')) then
      begin
        if (NOT MissingCrossBins) then memoSummary.Lines.Append('Missing correct cross libraries');
      end
      else if (ExistWordInString(MsgStr,'failed to get crosslibrary')) then
      begin
        if (NOT MissingCrossLibs) then memoSummary.Lines.Append('Missing correct cross libraries');
      end
      else if ((ExistWordInString(MsgStr,'CheckAndGetTools')) OR (ExistWordInString(MsgStr,'Required package is not installed'))) then
      begin
        if (NOT MissingTools) then
        begin
          {$ifdef Darwin}
          memoSummary.Lines.Append('Missing some tools: please install Xcode command line tools !');
          memoSummary.Lines.Append('xcode-select --install');
          {$else}
          memoSummary.Lines.Append('Missing some tools: please install missing tools!');
          {$endif}
        end;
        MissingTools:=true;
      {$ifdef Darwin}
      end
      else if (ExistWordInString(MsgStr,'The subversion command line tools are no longer provided by Xcode')) then
      begin
        if (NOT MissingTools) then
        begin
          memoSummary.Lines.Append('SVN is no longer included in Xcode command line tools !');
          memoSummary.Lines.Append('Use a GIT repo (preferred) or install SVN by yourself (brew).');
        end;
        MissingTools:=true;
      {$endif}
      end;
    end;
  finally
    StrDispose(MsgStr);
    //Dispose(MsgPStr);
  end;
end;

procedure TForm1.InitFpcupdeluxe(Data: PtrInt);
begin
  InitFPCupManager;
  {$ifdef usealternateui}
  // This must only be called once.
  If Not Alternate_ui_created then alternateui_Create_Controls;
  {$endif}
  if Form2.GetUpdates then Application.QueueAsyncCall(@CheckForUpdates,0);
end;

{$ifdef RemoteLog}
procedure TForm1.InitConsent(Data: PtrInt);
var
  aModalResult:TModalResult;
begin
  aDataClient.UpInfo.UpDistro:=GetDistro;
  if (sConsentWarning) OR (Form2.SendInfo) then
  begin
    if (sConsentWarning) then
    begin
      aModalResult:=(MessageDlgEx(
                   'Attention !'+sLineBreak+
                   sLineBreak +
                   'Fpcupdeluxe is able to log some install info.' + sLineBreak +
                   'This data is send towards a server,' + sLineBreak +
                   'where it is available to anybody.' + sLineBreak +
                   '(see URL shown in screen and statusbar)' + sLineBreak +
                   sLineBreak +
                   'Do you want logging info to be gathered ?'
                 ,mtConfirmation,[mbYes, mbNo],Self));
      if aModalResult=mrYes
         then Form2.SendInfo:=True
         else Form2.SendInfo:=False;
    end;
  end;

  if (Form2.SendInfo) then
  begin
    AddMessage('Fpcupdeluxe logging info:');
    AddMessage('fpcuplogger.batterybutcher.com/root/getinfohtml',true);
    AddMessage('fpcuplogger.batterybutcher.com/root/getinfohtml?ShowErrors=yes');
  end;

end;
{$endif}

procedure TForm1.ScrollToSelected(Data: PtrInt);
begin
  if (ListBoxFPCTarget.HandleAllocated) AND (ListBoxFPCTarget.ItemIndex>8) then ListBoxFPCTarget.MakeCurrentVisible;
  if (ListBoxLazarusTarget.HandleAllocated) AND (ListBoxLazarusTarget.ItemIndex>8) then ListBoxLazarusTarget.MakeCurrentVisible;
end;

procedure TForm1.ParseRevisions(IniDirectory:string);
type
  TTarget             = (FPC,LAZARUS);
const
  TargetDateMagic     : array[TTarget] of string = (FPCDATEMAGIC,LAZDATEMAGIC);
  TargetHashMagic     : array[TTarget] of string = (FPCHASHMAGIC,LAZHASHMAGIC);
  TargetNameMagic     : array[TTarget] of string = (FPCNAMEMAGIC,LAZNAMEMAGIC);
var
  aTarget             : TTarget;
  RevList             : TStringList;
  RevFile             : string;
  index               : integer;
  hash,revname,date   : string;
  AItem               : TListItem;
  TargetViewArray     : array[TTarget] of TListView;
begin
  TargetViewArray[TTarget.FPC]:=ListBoxFPCHistoryNew;
  TargetViewArray[TTarget.Lazarus]:=ListBoxLazarusHistoryNew;

  RevFile:=IncludeTrailingPathDelimiter(IniDirectory)+REVISIONSLOG;
  if FileExists(RevFile) then
  begin
    RevList:=TStringList.Create;
    try
      RevList.LoadFromFile(RevFile);

      for aTarget in TTarget do
      begin
        TargetViewArray[aTarget].Items.BeginUpdate;
        try
          TargetViewArray[aTarget].Items.Clear;

          index:=0;
          while true do
          begin
            index:=StringListStartsWith(RevList,TargetDateMagic[aTarget],index);
            if index=-1 then
              break
            else
              begin
                date:=RevList[index];
                Delete(date,1,Length(TargetDateMagic[aTarget]));
                Inc(index);
                hash:='';
                revname:='';
                while (index<RevList.Count) do
                begin
                  if (Length(RevList[index])=0) then break;
                  if AnsiStartsText(TargetHashMagic[aTarget],RevList[index]) then
                  begin
                    hash:=RevList[index];
                    Delete(hash,1,Length(TargetHashMagic[aTarget]))
                  end;
                  if AnsiStartsText(TargetNameMagic[aTarget],RevList[index]) then
                  begin
                    revname:=Copy(RevList[index],Succ(Length(TargetNameMagic[aTarget])),MaxInt);
                  end;
                  Inc(index);
                end;

                if ( (Length(hash)>0) AND (hash<>'failure') AND (hash<>'unknown') ) then
                begin
                  AItem:=TargetViewArray[aTarget].FindCaption(0,hash,false,true,false);
                  if (aItem=nil) then
                  begin
                    with TargetViewArray[aTarget].Items.Add do
                    begin
                      Caption:=hash;
                      SubItems.Add(revname);
                      SubItems.Add(date);
                    end;
                  end;
                end;

              end;

          end;

          if aTarget=TTarget.FPC then BitBtnFPCSetRevision.Enabled:=(TargetViewArray[aTarget].Items.Count>0);
          if aTarget=TTarget.LAZARUS then BitBtnLazarusSetRevision.Enabled:=(TargetViewArray[aTarget].Items.Count>0);

        finally
          TargetViewArray[aTarget].Items.EndUpdate;
        end;

      end;

    finally
      RevList.Free;
    end;

  end;

end;

procedure TForm1.AddRevision(TargetFPC,TargetLAZ:boolean;aHash,aName:string;aDate:TDateTime);
type
  TTarget             = (FPC,LAZARUS);
var
  aTarget             : TTarget;
  hash                : string;
  AItem               : TListItem;
  TargetViewArray     : array[TTarget] of TListView;
begin
  TargetViewArray[TTarget.FPC]:=ListBoxFPCHistoryNew;
  TargetViewArray[TTarget.Lazarus]:=ListBoxLazarusHistoryNew;
  if TargetFPC then aTarget:=TTarget.FPC;
  if TargetLAZ then aTarget:=TTarget.LAZARUS;
  TargetViewArray[aTarget].Items.BeginUpdate;
  try
    hash:=aHash;
    if ( (Length(hash)>0) AND (hash<>'failure') AND (hash<>'unknown') ) then
    begin
      AItem:=TargetViewArray[aTarget].FindCaption(0,hash,false,true,false);
      if (aItem=nil) then aItem:=TargetViewArray[aTarget].Items.Add;
      with aItem do
      begin
        Caption:=hash;
        if (SubItems.Count<1) then SubItems.Add(aName) else SubItems[0]:=aName;
        if (SubItems.Count<2) then SubItems.Add(DateTimeToStr(aDate)) else SubItems[1]:=DateTimeToStr(aDate);
      end;
    end;
    if aTarget=TTarget.FPC then BitBtnFPCSetRevision.Enabled:=(TargetViewArray[aTarget].Items.Count>0);
    if aTarget=TTarget.LAZARUS then BitBtnLazarusSetRevision.Enabled:=(TargetViewArray[aTarget].Items.Count>0);
  finally
    TargetViewArray[aTarget].Items.EndUpdate;
  end;
end;

procedure TForm1.CheckForUpdates(Data: PtrInt);
var
  s:string;
begin
  AddMessage(upCheckUpdate);
  s:=checkGithubRelease(FPCUPGITREPOAPIRELEASES+'/latest');
  if Length(s)>0 then
  begin
    AddMessage(upUpdateFound);
    AddMessage(FPCUPGITREPO+'/releases/latest');
    AddMessage(s);
    memoSummary.Lines.Append(upUpdateFound);
  end else AddMessage(upUpdateNotFound);
end;

function TForm1.GetCmdFontSize:integer;
begin
  result:=CommandOutputScreen.Font.Size;
end;

procedure TForm1.SetCmdFontSize(aValue:integer);
begin
  CommandOutputScreen.Font.Size:=aValue;
end;

function TForm1.GetCmdFontName: String;
begin
  Result := CommandOutputScreen.Font.Name;
end;

procedure TForm1.SetCmdFontName(aValue: String);
begin
  CommandOutputScreen.Font.Name:=aValue;
end;

procedure TForm1.FillSourceListboxes;
var
  aFPCKeyword,aLazarusKeyword:string;
  aList:TStringList;
  i:integer;
begin
  if (ListBoxFPCTarget.SelCount=1)
    then aFPCKeyword:=ListBoxFPCTarget.GetSelectedText
  else
    aFPCKeyword:=FPCTarget;

  if (ListBoxLazarusTarget.SelCount=1)
    then aLazarusKeyword:=ListBoxLazarusTarget.GetSelectedText
  else
    aLazarusKeyword:=LazarusTarget;

  aList:=TStringList.Create;
  try
    aList.Clear;

    ListBoxFPCTarget.Items.BeginUpdate;
    ListBoxFPCTarget.Items.Clear;
    if ListBoxFPCTarget.Count=0 then
    begin
      if chkGitlab.Checked then
      begin
        aList.CommaText:=installerUniversal.GetAlias(FPCTAGLOOKUPMAGIC,'list')+','+installerUniversal.GetAlias(FPCBRANCHLOOKUPMAGIC,'list');
      end
      else
        aList.CommaText:=installerUniversal.GetAlias(FPCURLLOOKUPMAGIC,'list');
      if chkGitlab.Checked then aList.CustomSort(@NaturalCompare);
      ListBoxFPCTarget.Items.AddStrings(aList);
    end;
    ListBoxFPCTarget.Items.EndUpdate;

    aList.Clear;

    ListBoxLazarusTarget.Items.BeginUpdate;
    ListBoxLazarusTarget.Items.Clear;
    if ListBoxLazarusTarget.Count=0 then
    begin
      if chkGitlab.Checked then
      begin
        aList.CommaText:=installerUniversal.GetAlias(LAZARUSTAGLOOKUPMAGIC,'list')+','+installerUniversal.GetAlias(LAZARUSBRANCHLOOKUPMAGIC,'list');
      end
      else
        aList.CommaText:=installerUniversal.GetAlias(LAZARUSURLLOOKUPMAGIC,'list');
      if chkGitlab.Checked then aList.CustomSort(@NaturalCompare);
      ListBoxLazarusTarget.Items.AddStrings(aList);
    end;
    ListBoxLazarusTarget.Items.EndUpdate;

  finally
    aList.Free;
  end;

  if ((Length(aFPCKeyword)>0) AND (ListBoxFPCTarget.Items.Count>0)) then
  begin
    i:=ListBoxFPCTarget.Items.IndexOf(aFPCKeyword);
    if (i<>-1) then
      FPCTarget:=aFPCKeyword;
  end;

  if ((Length(aLazarusKeyword)>0) AND (ListBoxLazarusTarget.Items.Count>0)) then
  begin
    i:=ListBoxLazarusTarget.Items.IndexOf(aLazarusKeyword);
    if (i<>-1) then
      LazarusTarget:=aLazarusKeyword;
  end;
end;

end.

