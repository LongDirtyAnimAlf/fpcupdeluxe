unit extrasettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, ExtCtrls,
  Dialogs, CheckLst, IniPropStorage, ComCtrls, m_crossinstaller;

type
  TString = class(TObject)
  private
    fStr: String;
  public
    constructor Create(const AStr: String) ;
    property Str: String read FStr write FStr;
  end;

  { TForm2 }
  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnAddFPCPatch: TButton;
    btnAddLazPatch: TButton;
    btnListCustomOptions: TButton;
    btnRemFPCPatch: TButton;
    btnRemLazPatch: TButton;
    btnSelectLibDir: TButton;
    btnSelectBinDir: TButton;
    btnSelectCompiler: TButton;
    chkFPCDebug: TCheckBox;
    chkLazarusDebug: TCheckBox;
    ComboBoxCPU: TComboBox;
    ComboBoxOS: TComboBox;
    EditFPCPreInstall: TEdit;
    EditFPCPostInstall: TEdit;
    EditLazarusPreInstall: TEdit;
    EditLazarusPostInstall: TEdit;
    grpPatching: TGroupBox;
    GroupBoxFPCLazScripts: TGroupBox;
    IniPropStorageSettings: TIniPropStorage;
    LabelCPU: TLabel;
    LabelFPCPreInstall: TLabel;
    LabelFPCPostInstall: TLabel;
    LabelLazarusPreInstall: TLabel;
    LabelLazarusPostInstall: TLabel;
    LabelOS: TLabel;
    ListBoxFPCPatch: TListBox;
    ListBoxLazPatch: TListBox;
    MiscellaneousCheckListBox: TCheckListBox;
    EditCrossBuildOptions: TEdit;
    EditFPCBranch: TEdit;
    EditFPCOptions: TEdit;
    EditFPCRevision: TEdit;
    EditLazarusBranch: TEdit;
    EditLazarusOptions: TEdit;
    EditLazarusRevision: TEdit;
    EditLibLocation: TEdit;
    EditBinLocation: TEdit;
    EditCompilerOverride: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ScrollBox1: TScrollBox;
    EditHTTPProxyHost: TEdit;
    EditHTTPProxyPort: TEdit;
    EditHTTPProxyUser: TEdit;
    EditHTTPProxyPassword: TEdit;
    GroupBoxCompileOptions: TGroupBox;
    GroupBoxFPCLazBranchRevision: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelCrossBuildOptions: TLabel;
    LabelCompilerOverride: TLabel;
    LabelFPCbranch: TLabel;
    LabelFPCOptions: TLabel;
    LabelFPCRevision: TLabel;
    LabelLazarusbranch: TLabel;
    LabelLazarusOptions: TLabel;
    LabelLazarusRevision: TLabel;
    OpenDialog1: TOpenDialog;
    ButtonPanel: TPanel;
    PageControl1: TPageControl;
    rgrpSearchOptions: TRadioGroup;
    rgrpSubarch: TRadioGroup;
    RadioGroupARMArch: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    tsCPUOS: TTabSheet;
    tsSUBARCH: TTabSheet;
    procedure btnAddPatchClick(Sender: TObject);
    procedure btnRemPatchClick(Sender: TObject);
    procedure btnSelectFile(Sender: TObject);
    procedure btnListCustomOptionsClick({%H-}Sender: TObject);
    procedure ComboBoxCPUOSChange({%H-}Sender: TObject);
    procedure EditCrossBuildOptionsEditingDone(Sender: TObject);
    procedure EditDblClickDelete(Sender: TObject);
    procedure EditScriptClick(Sender: TObject);
    procedure FormClose({%H-}Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
    procedure IniPropStorageSettingsRestoringProperties({%H-}Sender: TObject);
    procedure IniPropStorageSettingsSavingProperties({%H-}Sender: TObject);
    procedure OnDirectorySelect(Sender: TObject);
    procedure rgrpSearchOptionsSelectionChanged(Sender: TObject);
    procedure RadioGroupARMArchSelectionChanged(Sender: TObject);
    procedure rgrpSubarchSelectionChanged(Sender: TObject);
  private
    FInstallPath:string;

    LocalCPU:TCPU;
    LocalOS:TOS;
    LocalSUBARCH:TSUBARCH;

    function GetCheckIndex(aCaption:string):integer;
    function GetCheckState(aCaption:string):boolean;
    procedure SetCheckState(aCaption:string;aState:boolean);
    procedure SetCheckEnabled(aCaption:string;aState:boolean);

    function  GetPatches(const Lazarus:boolean=false):string;
    procedure SetPatches(value:string;const Lazarus:boolean=false);

    function GetRepo:boolean;
    procedure SetRepo(value:boolean);

    function GetPackageRepo:boolean;
    procedure SetPackageRepo(value:boolean);

    function GetUpdateOnly:boolean;
    procedure SetUpdateOnly(value:boolean);

    function GetSystemFPC:boolean;
    procedure SetSystemFPC(value:boolean);

    function GetIncludeLCL:boolean;
    procedure SetIncludeLCL(value:boolean);

    function GetIncludeHelp:boolean;
    procedure SetIncludeHelp(value:boolean);

    function GetSplitFPC:boolean;
    procedure SetSplitFPC(value:boolean);
    function GetSplitLazarus:boolean;
    procedure SetSplitLazarus(value:boolean);

    function GetUseWget:boolean;
    procedure SetUseWget(value:boolean);

    function GetMakeJobs:boolean;
    procedure SetMakeJobs(value:boolean);

    function GetExtraVerbose:boolean;
    procedure SetExtraVerbose(value:boolean);

    function GetAutoSwitchURL:boolean;
    procedure SetAutoSwitchURL(value:boolean);

    function GetSendInfo:boolean;
    procedure SetSendInfo(value:boolean);

    function GetFpcupBootstrappersOnly:boolean;
    procedure SetFpcupBootstrappersOnly(value:boolean);

    function GetForceLocalRepoClient:boolean;
    procedure SetForceLocalRepoClient(value:boolean);

    function GetCheckUpdates:boolean;
    procedure SetCheckUpdates(value:boolean);

    function GetUseSoftFloat:boolean;
    procedure SetUseSoftFloat(value:boolean);

    function GetAllowOnlinePatching:boolean;
    procedure SetAllowOnlinePatching(value:boolean);

    function GetApplyLocalChanges:boolean;
    procedure SetApplyLocalChanges(value:boolean);

    function GetAddContext:boolean;
    procedure SetAddContext(value:boolean);

    function GetAskConfirmation:boolean;
    procedure SetAskConfirmation(value:boolean);

    function GetHTTPProxyHost:string;
    function GetHTTPProxyPort:integer;
    function GetHTTPProxyUser:string;
    function GetHTTPProxyPass:string;
    function GetFPCOptions:string;
    procedure SetFPCOptions(value:string);
    function GetLazarusOptions:string;
    procedure SetLazarusOptions(value:string);

    function GetFPCDebug:boolean;
    procedure SetFPCDebug(value:boolean);
    function GetLazarusDebug:boolean;
    procedure SetLazarusDebug(value:boolean);

    function GetFPCRevision:string;
    procedure SetFPCRevision(value:string);
    procedure ForceSetFPCRevision(value:string);
    function GetLazarusRevision:string;
    procedure SetLazarusRevision(value:string);
    procedure ForceSetLazarusRevision(value:string);
    function GetFPCBranch:string;
    procedure SetFPCBranch(value:string);
    function GetLazarusBranch:string;
    procedure SetLazarusBranch(value:string);

    function GetFPCPreScript:string;
    procedure SetFPCPreScript(value:string);
    function GetFPCPostScript:string;
    procedure SetFPCPostScript(value:string);

    function GetLazarusPreScript:string;
    procedure SetLazarusPreScript(value:string);
    function GetLazarusPostScript:string;
    procedure SetLazarusPostScript(value:string);

    function GetFPCPatches:string;
    procedure SetFPCPatches(value:string);
    function GetLazPatches:string;
    procedure SetLazPatches(value:string);

    {
    function GetCPUFromComboBox:TCPU;
    function GetOSFromComboBox:TOS;

    property CPUFromComboBox:TCPU read GetCPUFromComboBox;
    property OSFromComboBox:TOS read GetOSFromComboBox;
    }
  public
    procedure SetInstallDir(const aInstallDir:string='');

    procedure SetCrossTarget(aSender:TObject;aCPU:TCPU;aOS:TOS);

    function GetLibraryDirectory(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):string;
    function GetToolsDirectory(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):string;
    function GetCrossBuildOptions(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):string;
    function GetCrossARMArch(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):TARMARCH;
    function GetCrossARMFPCStr(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): string;
    function GetCompiler(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): string;

    function  GetCrossAvailable(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): boolean;
    procedure SetCrossAvailable(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH;aValue:boolean);

    procedure ResetAll;

    property Repo:boolean read GetRepo write SetRepo;
    property PackageRepo:boolean read GetPackageRepo write SetPackageRepo;

    property UpdateOnly:boolean read GetUpdateOnly write SetUpdateOnly;
    property SystemFPC:boolean read GetSystemFPC write SetSystemFPC;

    property IncludeLCL:boolean read GetIncludeLCL write SetIncludeLCL;
    property IncludeHelp:boolean read GetIncludeHelp write SetIncludeHelp;

    property SplitFPC:boolean read GetSplitFPC write SetSplitFPC;
    property SplitLazarus:boolean read GetSplitLazarus write SetSplitLazarus;

    property UseWget:boolean read GetUseWget write SetUseWget;
    property MakeJobs:boolean read GetMakeJobs write SetMakeJobs;
    property ExtraVerbose:boolean read GetExtraVerbose write SetExtraVerbose;
    property AutoSwitchURL:boolean read GetAutoSwitchURL write SetAutoSwitchURL;
    property SendInfo:boolean read GetSendInfo write SetSendInfo;
    property FpcupBootstrappersOnly:boolean read GetFpcupBootstrappersOnly write SetFpcupBootstrappersOnly;
    property ForceLocalRepoClient:boolean read GetForceLocalRepoClient write SetForceLocalRepoClient;
    property GetUpdates:boolean read GetCheckUpdates write SetCheckUpdates;
    property UseSoftFloat:boolean read GetUseSoftFloat write SetUseSoftFloat;
    property OnlinePatching:boolean read GetAllowOnlinePatching write SetAllowOnlinePatching;
    property ApplyLocalChanges:boolean read GetApplyLocalChanges write SetApplyLocalChanges;
    property AddContext:boolean read GetAddContext write SetAddContext;
    property AskConfirmation:boolean read GetAskConfirmation write SetAskConfirmation;

    property HTTPProxyHost:string read GetHTTPProxyHost;
    property HTTPProxyPort:integer read GetHTTPProxyPort;
    property HTTPProxyUser:string read GetHTTPProxyUser;
    property HTTPProxyPass:string read GetHTTPProxyPass;

    property FPCOptions:string read GetFPCOptions write SetFPCOptions;
    property LazarusOptions:string read GetLazarusOptions write SetLazarusOptions;

    property FPCDebug:boolean read GetFPCDebug write SetFPCDebug;
    property LazarusDebug:boolean read GetLazarusDebug write SetLazarusDebug;

    property FPCRevision:string read GetFPCRevision write SetFPCRevision;
    property LazarusRevision:string read GetLazarusRevision write SetLazarusRevision;

    property ForceFPCRevision:string write ForceSetFPCRevision;
    property ForceLazarusRevision:string write ForceSetLazarusRevision;

    property FPCBranch:string read GetFPCBranch write SetFPCBranch;
    property LazarusBranch:string read GetLazarusBranch write SetLazarusBranch;

    property FPCPreScript:string read GetFPCPreScript write SetFPCPreScript;
    property FPCPostScript:string read GetFPCPostScript write SetFPCPostScript;

    property LazarusPreScript:string read GetLazarusPreScript write SetLazarusPreScript;
    property LazarusPostScript:string read GetLazarusPostScript write SetLazarusPostScript;

    property FPCPatches:string read GetFPCPatches write SetFPCPatches;
    property LazPatches:string read GetLazPatches write SetLazPatches;
  end;


resourcestring
  HintCheckRepo = 'Download whole repository, or only latest files';
  CaptionCheckRepo = 'Get FPC/Laz repositories (default=yes)';

  HintCheckPackageRepo = '';
  CaptionCheckPackageRepo = 'Get package repositories (default=no)';

  HintCheckIncludeLCL = '';
  CaptionCheckIncludeLCL = 'Include LCL with cross-compiler (default=no)';

  HintCheckUpdateOnly = '';
  CaptionCheckUpdateOnly = 'FPC/Laz rebuild only (default=no)';

  HintCheckSystemFPC = 'Use the system wide install of FPC to build Lazarus.';
  CaptionCheckSystemFPC = 'Use system FPC for Lazarus (default=no)';

  HintCheckIncludeHelp = '';
  CaptionCheckIncludeHelp = 'Include Help (default=no)';

  HintCheckSplitFPC = '';
  CaptionCheckSplitFPC = 'Split FPC source and bins (default=yes)';

  HintCheckSplitLazarus = '';
  CaptionCheckSplitLazarus = 'Split Lazarus source and bins (default=no)';

  HintCheckUseWget = '';
  CaptionCheckUseWget = 'Use wget/libcurl as downloader (default=no)';

  HintCheckUseMakeJobs = '';
  CaptionCheckUseMakeJobs = 'Use jobs for GNU make (default=yes)';

  HintCheckExtraVerbose = '';
  CaptionCheckExtraVerbose = 'Be extra verbose (default=no)';

  HintCheckAutoSwitchURL = '';
  CaptionCheckAutoSwitchURL = 'Auto-switch repo URL (default=no)';

  HintCheckSendInfo = 'Location and install info will be send to public central fpcupdeluxe server.';
  CaptionCheckSendInfo = 'Send location and install info (default=no)';

  HintCheckFpcupBootstrappersOnly = '';
  CaptionCheckFpcupBootstrappersOnly = 'Only use fpcup bootstrappers (default=yes)';

  HintCheckForceLocalRepoClient = 'Use the repo-client by fpcupdeluxe.';
  CaptionCheckForceLocalRepoClient = 'Use local repo-client (default=no)';

  HintCheckGetUpdates = 'Check for updates of fpcupdeluxe binaries.';
  CaptionCheckGetUpdates = 'Check for fpcupdeluxe updates (default=no)';

  HintUseSoftFloat80bit = 'Enable software emulation of 80 bit floats.';
  CaptionUseSoftFloat80bit = 'Enable software emulation of 80 bit floats.';

  HintCheckEnableOnlinePatching = 'Fpcupdeluxe can patch the sources automagically by using online patches.';
  CaptionCheckEnableOnlinePatching = 'Allow patching of sources by online patches.';

  HintCheckApplyLocalChanges = 'Fpcupdeluxe can re-apply the local changes automagically by using local auto-patch.';
  CaptionCheckApplyLocalChanges = 'Re-apply local changes when updating.';

  HintCheckAddContext = 'Double clicking on FPC and Lazarus files will open Lazarus.';
  CaptionCheckAddContext = 'Add context for FPC and Lazarus files.';

  HintCheckAskConfirmation = 'Show a confirmation dialog with yes/no before every build.';
  CaptionCheckAskConfirmation = 'Always ask for confirmation (default=yes).';

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses
  infounit,
  DCPDES,
  //DCPrc4,
  DCPsha256,
  fpcuputil,
  installerUniversal,
  IniFiles;

{ TForm2 }

constructor TString.Create(const AStr: String) ;
begin
  inherited Create;
  FStr := AStr;
end;

procedure TForm2.OnDirectorySelect(Sender: TObject);
begin
  if Sender=btnSelectLibDir then SelectDirectoryDialog1.InitialDir:=EditLibLocation.Text;
  if Sender=btnSelectBinDir then SelectDirectoryDialog1.InitialDir:=EditBinLocation.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    if Sender=btnSelectLibDir then EditLibLocation.Text:=SelectDirectoryDialog1.FileName;
    if Sender=btnSelectBinDir then EditBinLocation.Text:=SelectDirectoryDialog1.FileName;
  end;
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    if Sender=btnSelectLibDir then
       CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].LibDir:=SelectDirectoryDialog1.FileName;
    if Sender=btnSelectBinDir then
       CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].BinDir:=SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  CPU       :TCPU;
  OS        :TOS;
  ARMArch   :TARMARCH;
  s         :string;
  SortedList:TStringList;
  //Cipher    : TDCP_rc4;
  Cipher    :TDCP_DES;
begin
  IniPropStorageSettings.IniFileName:=IncludeTrailingPathDelimiter(SafeGetApplicationPath)+installerUniversal.DELUXEFILENAME;

  SortedList:=TStringList.Create;
  try

    // Fill ComboBoxOS
    SortedList.Clear;
    SortedList.Sorted:=False;
    for OS := Low(TOS) to High(TOS) do
    begin
      if OS=TOS.osNone then continue;
      SortedList.Add(GetOS(OS));
    end;
    SortedList.Sort;
    for s in SortedList do
      ComboBoxOS.Items.Add(s);

    // Fill ComboBoxCPU
    SortedList.Clear;
    SortedList.Sorted:=False;
    for CPU := Low(TCPU) to High(TCPU) do
    begin
      if CPU=TCPU.cpuNone then continue;
      SortedList.Add(GetCPU(CPU));
    end;
    SortedList.Sort;
    for s in SortedList do
      ComboBoxCPU.Items.Add(s);

  finally
    SortedList.Free;
  end;

  // Fill ARM Arch radiogroup
  for ARMArch := Low(TARMARCH) to High(TARMARCH) do
    RadioGroupARMArch.Items.Add(GetEnumNameSimple(TypeInfo(TARMARCH),Ord(ARMArch)));
  RadioGroupARMArch.ItemIndex:=0;

  with MiscellaneousCheckListBox.Items do
  begin
    Append(CaptionCheckRepo);
    Append(CaptionCheckPackageRepo);
    Append(CaptionCheckIncludeLCL);
    Append(CaptionCheckUpdateOnly);
    Append(CaptionCheckSystemFPC);
    Append(CaptionCheckIncludeHelp);
    Append(CaptionCheckSplitFPC);
    Append(CaptionCheckSplitLazarus);
    Append(CaptionCheckUseWget);
    Append(CaptionCheckUseMakeJobs);
    Append(CaptionCheckExtraVerbose);
    Append(CaptionCheckAutoSwitchURL);
    Append(CaptionCheckSendInfo);
    Append(CaptionCheckFpcupBootstrappersOnly);
    Append(CaptionCheckForceLocalRepoClient);
    Append(CaptionCheckGetUpdates);
    Append(CaptionUseSoftFloat80bit);
    Append(CaptionCheckEnableOnlinePatching);
    Append(CaptionCheckApplyLocalChanges);
    Append(CaptionCheckAddContext);
    Append(CaptionCheckAskConfirmation);
  end;

  AskConfirmation        := True;
  FpcupBootstrappersOnly := True;
  Repo                   := True;
  PackageRepo            := False;
  IncludeHelp            := False;
  IncludeLCL             := False;
  {$ifdef RemoteLog}
  SendInfo        := False;
  {$endif}

  with TIniFile.Create(SafeGetApplicationPath+installerUniversal.DELUXEFILENAME) do
  try
    Repo:=ReadBool('General','GetRepo',Repo);
    PackageRepo:=ReadBool('General','GetPackageRepo',PackageRepo);

    IncludeHelp:=ReadBool('General','IncludeHelp',IncludeHelp);
    IncludeLCL:=ReadBool('Cross','IncludeLCL',IncludeLCL);

    {$ifdef RemoteLog}
    SendInfo:=ReadBool('General','SendInfo',SendInfo);
    {$endif}

    GetUpdates:=ReadBool('General','GetUpdates',GetUpdates);

    AskConfirmation:=ReadBool('General','AskConfirmation',AskConfirmation);

    EditHTTPProxyHost.Text:=ReadString('ProxySettings','HTTPProxyURL','');
    EditHTTPProxyPort.Text:=InttoStr(ReadInteger('ProxySettings','HTTPProxyPort',8080));
    EditHTTPProxyUser.Text:=ReadString('ProxySettings','HTTPProxyUser','');

    // add some security into the password storage ... ;-)
    s:=ReadString('ProxySettings','HTTPProxyPass','');
    if Length(s)>0 then
    begin
      //Cipher:= TDCP_rc4.Create(nil);
      Cipher := TDCP_DES.Create(nil);
      try
        {$ifdef SECRETDELUXEKEY}
        Cipher.InitStr(VERYSECRETDELUXEKEY,TDCP_sha256);
        {$else}
        Cipher.InitStr(DELUXEKEY,TDCP_sha256);
        {$endif}
        s:=Cipher.DecryptString(s);
      finally
        Cipher.Burn;
        Cipher.Free;
      end;
    end;
    EditHTTPProxyPassword.Text:=s;

  finally
    Free;
  end;

  //defaults
  LocalCPU:=TCPU.cpuNone;
  LocalOS:=TOS.osNone;
  LocalSUBARCH:=TSUBARCH.saNone;

  SplitFPC:=true;
  MakeJobs:=true;
  LazarusDebug:=true;
  {$ifdef win64}
  MakeJobs:=False;
  {$endif}

  SetInstallDir;// for backwards compatibility

  //{$IF (defined(MSWINDOWS)) OR (defined(Darwin)) OR (defined(OpenBSD))}
  {$IF (defined(BSD)) AND (NOT defined(FreeBSD))}
  // there are default setings for the downloader, so disable user access
  UseWget:=False;
  {$endif}

  {$ifndef RemoteLog}
  SendInfo:=False;
  SetCheckEnabled(CaptionCheckSendInfo,False);
  {$endif}

  {$ifndef Windows}
  ForceLocalRepoClient:=False;
  SetCheckEnabled(CaptionCheckForceLocalRepoClient,False);
  {$endif}

  {$ifdef Windows}
  AddContext:=False;
  SetCheckEnabled(CaptionCheckAddContext,False);
  {$endif}

  UseSoftFloat:=true;

  //Disable split lazarus by default ... still testing
  SplitLazarus:=False;
  SetCheckEnabled(CaptionCheckSplitLazarus,False);

  {$IF defined(CPUAARCH64) OR defined(CPUARM) OR defined(Haiku)}
  // disable some features
  UseSoftFloat:=false;
  SetCheckEnabled(CaptionUseSoftFloat80bit,False);
  {$endif}

  //Disable OnlinePatching by default starting with 1.6.8p
  OnlinePatching:=false;
end;

procedure TForm2.SetInstallDir(const aInstallDir:string='');
var
  CPU:TCPU;
  OS:TOS;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  s1,s2:string;
begin
  if (Length(aInstallDir)>0)
     then FInstallPath:=IncludeTrailingPathDelimiter(aInstallDir)
     else FInstallPath:=SafeGetApplicationPath;


  with TMemIniFile.Create(FInstallPath+installerUniversal.DELUXEFILENAME) do
  try
    for OS := Low(TOS) to High(TOS) do
    begin
      if OS=osNone then continue;
      for CPU := Low(TCPU) to High(TCPU) do
      begin
        if CPU=cpuNone then continue;
        s1:=GetCPU(CPU)+'-'+GetOS(OS);
        Subarchs:=GetSubarchs(CPU,OS);
        for SUBARCH in Subarchs do
        begin
          if (SUBARCH<>saNone) then
            s2:=s1+'-'+GetSubarch(SUBARCH)
          else
            s2:=s1;
          with CrossUtils[CPU,OS,SUBARCH] do
          begin
            Ord(Setting):=ReadInteger(s2,'Setting',Ord(Setting));
            LibDir:=ReadString(s2,'LibPath',LibDir);
            BinDir:=ReadString(s2,'BinPath',BinDir);
            CrossBuildOptions:=ReadString(s2,'CrossBuildOptions',CrossBuildOptions);
            if CPU=arm then
              CrossARMArch:=GetTARMArch(ReadString(s2,'CrossARMArch',GetARMArch(CrossARMArch)));
            Compiler:=ReadString(s2,'Compiler',Compiler);
          end;
        end;
      end;
    end;
  finally
    Free;
  end;

end;

procedure TForm2.SetCrossTarget(aSender:TObject;aCPU:TCPU;aOS:TOS);
var
  Subarch:TSUBARCH;
  Subarchs:TSUBARCHS;
  aIndex:integer;
  e:boolean;
  SystemChange:boolean;
begin

  SystemChange:=( (LocalCPU<>aCPU) OR (LocalOS<>aOS) );

  LocalCPU:=aCPU;
  LocalOS:=aOS;
  LocalSUBARCH:=GetSelectedSubArch(LocalCPU,LocalOS);

  if SystemChange then
  begin
    if LocalCPU=TCPU.cpuNone then
      ComboBoxCPU.ItemIndex:=-1
    else
    begin
      aIndex:=ComboBoxCPU.Items.IndexOf(GetCPU(LocalCPU));
      ComboBoxCPU.ItemIndex:=aIndex;
    end;
    if LocalOS=TOS.osNone then
      ComboBoxOS.ItemIndex:=-1
    else
    begin
      aIndex:=ComboBoxOS.Items.IndexOf(GetOS(LocalOS));
      ComboBoxOS.ItemIndex:=aIndex;
    end;
  end;

  e:=((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone));

  rgrpSearchOptions.Enabled:=e;
  EditCrossBuildOptions.Enabled:=e;
  EditCompilerOverride.Enabled:=e;
  btnSelectCompiler.Enabled:=e;
  RadioGroupARMArch.Enabled:=(e AND (LocalCPU=TCPU.arm));

  EditLibLocation.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].LibDir;
  EditBinLocation.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].BinDir;
  EditCrossBuildOptions.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions;
  rgrpSearchOptions.ItemIndex:=Ord(CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].Setting);
  RadioGroupARMArch.ItemIndex:=Ord(CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossARMArch);
  EditCompilerOverride.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].Compiler;

  if e then e:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].Setting=TSearchSetting.ssCustom;
  EditLibLocation.Enabled:=e;
  EditBinLocation.Enabled:=e;
  btnSelectLibDir.Enabled:=e;
  btnSelectBinDir.Enabled:=e;

  if aSender<>rgrpSubarch then
  begin
    e:=((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone) AND (LocalOS in SUBARCH_OS) AND (LocalCPU in SUBARCH_CPU));

    tsSUBARCH.Enabled:=e;

    if SystemChange then
    begin
      rgrpSubarch.BeginUpdateBounds;
      try
        rgrpSubarch.Items.Clear;
        Subarchs:=[TSUBARCH.saNone];
        if (e) then
          Subarchs:=GetSubarchs(LocalCPU,LocalOS);
        for Subarch in Subarchs do
        begin
          if (Subarch<>TSUBARCH.saNone) then
          begin
            rgrpSubarch.Items.Append(GetSubarch(Subarch));
            if Subarch=LocalSUBARCH then rgrpSubarch.ItemIndex:=Pred(rgrpSubarch.Items.Count);
          end;
        end;
        if rgrpSubarch.Items.Count=1 then rgrpSubarch.ItemIndex:=0;
      finally
        rgrpSubarch.EndUpdateBounds;
      end;
    end
    else
    begin
      aIndex:=rgrpSubarch.Items.IndexOf(GetSubarch(LocalSUBARCH));
      rgrpSubarch.ItemIndex:=aIndex;
    end;
  end;

end;

procedure TForm2.ComboBoxCPUOSChange(Sender: TObject);
var
  aCPU:TCPU;
  aOS:TOS;
begin
  aCPU:=TCPU.cpuNone;
  aOS:=TOS.osNone;
  if (ComboBoxCPU.ItemIndex<>-1) then aCPU:=GetTCPU(ComboBoxCPU.Items[ComboBoxCPU.ItemIndex]);
  if (ComboBoxOS.ItemIndex<>-1) then aOS:=GetTOS(ComboBoxOS.Items[ComboBoxOS.ItemIndex]);
  SetCrossTarget(Sender,aCPU,aOS);
end;

procedure TForm2.EditCrossBuildOptionsEditingDone(Sender: TObject);
begin
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions:=TEdit(Sender).Text;
  end;
end;

procedure TForm2.EditDblClickDelete(Sender: TObject);
begin
  TEdit(Sender).Text:='';
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    if Sender=EditCompilerOverride then CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].Compiler:='';
    if Sender=EditLibLocation then CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].LibDir:='';
    if Sender=EditBinLocation then CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].BinDir:='';
    if Sender=EditCrossBuildOptions then CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions:='';
  end;
end;

procedure TForm2.EditScriptClick(Sender: TObject);
var
  aEdit:TEdit;
begin
  aEdit:=TEdit(Sender);
  {$ifdef MSWindows}
  OpenDialog1.Filter:='Script|*.bat|All|*.*';
  {$else}
  OpenDialog1.Filter:='Script|*.sh|All|*.*';
  {$endif MSWindows}
  OpenDialog1.FilterIndex:=1;
  if OpenDialog1.Execute then
  begin
    aEdit.Text:=ExtractFileName(OpenDialog1.FileName);
    if Sender=EditFPCPreInstall then
    begin
    end;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SetSelectedSubArch(LocalCPU,LocalOS,LocalSUBARCH);
  LocalCPU:=TCPU.cpuNone;
  LocalOS:=TOS.osNone;
  PageControl1.PageIndex:=0;
  EditFPCRevision.Color:=clDefault;
  EditLazarusRevision.Color:=clDefault;
end;

procedure TForm2.btnAddPatchClick(Sender: TObject);
var
  PatchName: string;
  FullPatchPath: string;
  aListBox:TListBox;
begin
  OpenDialog1.Filter:='Diff|*.diff|Patch|*.patch|All|*.*';
  OpenDialog1.FilterIndex:=1;
  if OpenDialog1.Execute then
  begin
    FullPatchPath := OpenDialog1.FileName;
    PatchName := ExtractFileName(FullPatchPath);

    if Sender=btnAddFPCPatch then aListBox:=ListBoxFPCPatch;
    if Sender=btnAddLazPatch then aListBox:=ListBoxLazPatch;

    if aListBox.Items.IndexOf(PatchName)=-1 then
    begin
      aListBox.Items.AddObject(PatchName, TString.Create(FullPatchPath));
    end;
  end;
end;

procedure TForm2.btnRemPatchClick(Sender: TObject);
var
  i:integer;
  aListBox:TListBox;
begin

  if Sender=btnRemFPCPatch then aListBox:=ListBoxFPCPatch;
  if Sender=btnRemLazPatch then aListBox:=ListBoxLazPatch;

  if aListBox.SelCount>0 then
  begin
    for i:=aListBox.Count-1 downto 0 do
    begin
      if aListBox.Selected[i] then
      begin
        TString(aListBox.Items.Objects[i]).Free;
        aListBox.Items.Objects[i]:=nil;
        aListBox.Items.Delete(i);
      end;
    end;
  end;
end;

procedure TForm2.btnSelectFile(Sender: TObject);
begin
  if Sender=btnSelectCompiler then
  begin
    OpenDialog1.InitialDir:=EditCompilerOverride.Text;
    OpenDialog1.FilterIndex:=3;
  end;

  if OpenDialog1.Execute then
  begin
    if Sender=btnSelectCompiler then EditCompilerOverride.Text:=OpenDialog1.FileName;
  end;

  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    if Sender=btnSelectCompiler then
       CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].Compiler:=OpenDialog1.FileName;
  end;
end;

procedure TForm2.btnListCustomOptionsClick(Sender: TObject);
var
  CPU:TCPU;
  OS:TOS;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  s1,s2:string;
  x:integer;
begin
  InfoForm:= TInfoForm.Create(Self);
  try

    for OS := Low(TOS) to High(TOS) do
    begin
      if OS=TOS.osNone then continue;

      for CPU := Low(TCPU) to High(TCPU) do
      begin
        if CPU=TCPU.cpuNone then continue;

        s1:=GetCPU(CPU)+'-'+GetOS(OS);

        Subarchs:=GetSubarchs(CPU,OS);

        for SUBARCH in Subarchs do
        begin
          if (SUBARCH<>TSUBARCH.saNone) then
            s2:=s1+'-'+GetSubarch(SUBARCH)
          else
            s2:=s1;

          x:=InfoForm.Memo1.Lines.Count;

          with CrossUtils[CPU,OS,SUBARCH] do
          begin
            if Setting=TSearchSetting.ssAuto then
            begin
              InfoForm.Memo1.Lines.Append(s2+': full auto search tools and libraries.');
            end
            else
            if Setting=TSearchSetting.ssCustom then
            begin
              InfoForm.Memo1.Lines.Append(s2+' (manual settings):');
              InfoForm.Memo1.Lines.Append('  libs     : '+LibDir);
              InfoForm.Memo1.Lines.Append('  bins      : '+BinDir);
            end;

            if Length(CrossBuildOptions)>0 then
            begin
              if x=InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append(s2);
              InfoForm.Memo1.Lines.Append('  options : '+CrossBuildOptions);
            end;

            if CPU=arm then
            begin
              if (CrossARMArch<>TARMARCH.none) then
              begin
                if x=InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append(s2);
                InfoForm.Memo1.Lines.Append('  ARM Arch : '+GetARMArch(CrossARMArch));
              end;
            end;

            if Length(Compiler)>0 then
            begin
              if x=InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append(s2);
              InfoForm.Memo1.Lines.Append('  compiler : '+Compiler);
            end;

            if x<>InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append('');
          end;
        end;

      end;
    end;
    InfoForm.ShowModal;
  finally
    InfoForm.Free;
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var
  CPU:TCPU;
  OS:TOS;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  s1,s2:string;
  i:integer;
  //Cipher: TDCP_rc4;
  Cipher: TDCP_DES;
begin
  with TMemIniFile.Create(SafeGetApplicationPath+installerUniversal.DELUXEFILENAME) do
  try
    WriteBool('General','GetRepo',Repo);
    WriteBool('General','GetPackageRepo',PackageRepo);

    WriteBool('General','IncludeHelp',IncludeHelp);
    WriteBool('Cross','IncludeLCL',IncludeLCL);

    {$ifdef RemoteLog}
    WriteBool('General','SendInfo',SendInfo);
    {$endif}

    WriteBool('General','GetUpdates',GetUpdates);

    WriteBool('General','AskConfirmation',AskConfirmation);

    WriteString('ProxySettings','HTTPProxyURL',EditHTTPProxyHost.Text);
    if TryStrToInt(EditHTTPProxyPort.Text,i) then WriteInteger('ProxySettings','HTTPProxyPort',i);
    WriteString('ProxySettings','HTTPProxyUser',EditHTTPProxyUser.Text);

    // add some security into the password storage ... ;-)
    s1:=EditHTTPProxyPassword.Text;
    s2:=s1;
    if Length(s1)>0 then
    begin
      //Cipher:= TDCP_rc4.Create(nil);
      Cipher := TDCP_DES.Create(nil);
      try
        {$ifdef SECRETDELUXEKEY}
        Cipher.InitStr(VERYSECRETDELUXEKEY,TDCP_sha256);
        {$else}
        Cipher.InitStr(DELUXEKEY,TDCP_sha256);
        {$endif}
        s2:=Cipher.EncryptString(s1);
      finally
        Cipher.Burn;
        Cipher.Free;
      end;
    end;
    WriteString('ProxySettings','HTTPProxyPass',s2);

    UpdateFile;
  finally
    Free;
  end;

  with TMemIniFile.Create(FInstallPath+installerUniversal.DELUXEFILENAME) do
  try
    for OS := Low(TOS) to High(TOS) do
    begin

      if OS=osNone then continue;

      for CPU := Low(TCPU) to High(TCPU) do
      begin

        if CPU=cpuNone then continue;

        // skip non-combi's to reduce size of ini-file
        if ((OS=morphos) AND (CPU<>powerpc)) then continue;
        if ((OS=java) AND (CPU<>jvm)) OR ((CPU=jvm) AND (OS<>java) AND (OS<>android)) then continue;
        if (OS=ultibo) AND ((CPU<>arm) AND (CPU<>aarch64)) then continue;
        if (OS=android) AND ((CPU<>arm) AND (CPU<>aarch64) AND (CPU<>jvm) AND (CPU<>mipsel)) then continue;
        if (OS=iphonesim) AND ((CPU<>i386) AND (CPU<>x86_64)) then continue;
        if (OS=wince) AND (CPU<>arm) then continue;
        if (OS=win32) AND ((CPU<>i386) AND (CPU<>x86_64)) then continue;
        if (OS=win64) AND ((CPU<>i386) AND (CPU<>x86_64) AND (CPU<>aarch64)) then continue;
        if (OS=haiku) AND ((CPU<>i386) AND (CPU<>x86_64) {AND (CPU<>arm)}) then continue;
        if (OS=solaris) AND ((CPU<>x86_64) AND (CPU<>sparc)) then continue;
        if (OS=ios) AND ((CPU<>arm) AND (CPU<>aarch64)) then continue;
        if ((OS=wasi) AND (CPU<>wasm32)) then continue;


        if (CPU=xtensa) AND ((OS<>linux) AND (OS<>freertos)) then continue;
        if (CPU=m68k) AND ((OS<>linux) AND (OS<>amiga)) then continue;
        if (CPU=powerpc) AND ((OS<>aix) AND (OS<>linux) AND (OS<>darwin)) then continue;
        if (CPU=powerpc64) AND ((OS<>aix) AND (OS<>linux) AND (OS<>darwin)) then continue;
        if (CPU=mips) AND (OS<>linux) then continue;
        if (CPU=mipsel) AND ((OS<>linux) AND (OS<>android) AND (OS<>embedded)) then continue;
        if (CPU=avr) AND (OS<>embedded) then continue;
        if (CPU=sparc64) AND (OS<>linux) then continue;
        if ((CPU=riscv32) OR (CPU=riscv64)) AND ((OS<>linux) AND (OS<>embedded)) then continue;
        if (CPU=wasm32) AND ((OS<>wasi) AND (OS<>embedded)) then continue;

        s1:=GetCPU(CPU)+'-'+GetOS(OS);

        Subarchs:=GetSubarchs(CPU,OS);

        for SUBARCH in Subarchs do
        begin
          if (SUBARCH<>saNone) then
            s2:=s1+'-'+GetSubarch(SUBARCH)
          else
            s2:=s1;

          with CrossUtils[CPU,OS,SUBARCH] do
          begin
            WriteInteger(s2,'Setting',Ord(Setting));
            WriteString(s2,'LibPath',LibDir);
            WriteString(s2,'BinPath',BinDir);
            WriteString(s2,'CrossBuildOptions',CrossBuildOptions);
            if CPU=arm then
              WriteString(s2,'CrossARMArch',GetARMArch(CrossARMArch));
            WriteString(s2,'Compiler',Compiler);
          end;

        end;

      end;
    end;
    UpdateFile;
  finally
    Free;
  end;

  if (ListBoxFPCPatch.Items.Count>0) then
  begin
    for i := 0 to ListBoxFPCPatch.Items.Count - 1 do
    begin
       TString(ListBoxFPCPatch.Items.Objects[i]).Free;
       ListBoxFPCPatch.Items.Objects[i] := nil;
    end;
  end;

  if (ListBoxLazPatch.Items.Count>0) then
  begin
    for i := 0 to ListBoxLazPatch.Items.Count - 1 do
    begin
       TString(ListBoxLazPatch.Items.Objects[i]).Free;
       ListBoxLazPatch.Items.Objects[i] := nil;
    end;
  end;
end;

procedure TForm2.IniPropStorageSettingsRestoringProperties(Sender: TObject);
begin
  {$ifdef Haiku}
  {$else}
  SessionProperties := 'WindowState;Width;Height;Top;Left;';
  {$endif}
  //Width := MulDiv(Width, 96, Screen.PixelsPerInch);
  //Height := MulDiv(Height, 96, Screen.PixelsPerInch);
end;

procedure TForm2.IniPropStorageSettingsSavingProperties(Sender: TObject);
begin
  {$ifdef Haiku}
  {$else}
  if Self.WindowState=wsMaximized then
    SessionProperties := 'WindowState;'
  else
    SessionProperties := 'WindowState;Width;Height;Top;Left;';
  {$endif}
end;

procedure TForm2.rgrpSearchOptionsSelectionChanged(Sender: TObject);
var
  e:boolean;
  i:integer;
begin
  i:=(Sender AS TRadioGroup).ItemIndex;
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].Setting:=TSearchSetting(i);
  e:=(TSearchSetting(i)=TSearchSetting.ssCustom);
  EditLibLocation.Enabled:=e;
  EditBinLocation.Enabled:=e;
  btnSelectLibDir.Enabled:=e;
  btnSelectBinDir.Enabled:=e;
end;

procedure TForm2.RadioGroupARMArchSelectionChanged(Sender: TObject);
var
  i:integer;
  xARMArch:TARMARCH;
begin
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    i:=(Sender AS TRadioGroup).ItemIndex;
    if i=-1 then
      xARMArch:=TARMARCH.none
    else
      xARMArch:=TARMARCH(i);
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossARMArch:=xARMArch;
  end;
end;

procedure TForm2.rgrpSubarchSelectionChanged(Sender: TObject);
var
  i:integer;
begin
  LocalSUBARCH:=TSUBARCH.saNone;
  i:=rgrpSubarch.ItemIndex;
  if (i<>-1) then
    LocalSUBARCH:=GetTSubarch(rgrpSubarch.Items[i]);
  SetSelectedSubArch(LocalCPU,LocalOS,LocalSUBARCH);
  SetCrossTarget(Sender,LocalCPU,LocalOS);
end;

{
function TForm2.GetCPUFromComboBox:TCPU;
begin
  if (ComboBoxCPU.ItemIndex<>-1) then
  begin
    result:=GetCPUOSCombo(ComboBoxCPU.Items[ComboBoxCPU.ItemIndex],'').CPU;
  end;
end;

function TForm2.GetOSFromComboBox:TOS;
begin
  if (ComboBoxOS.ItemIndex<>-1) then
  begin
    result:=GetCPUOSCombo('',ComboBoxOS.Items[ComboBoxOS.ItemIndex]).OS;
  end;
end;
}

function TForm2.GetLibraryDirectory(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):string;
begin
  try
    case CrossUtils[aCPU,aOS,aSubarch].Setting of
      TSearchSetting.ssUp: result:='';
      TSearchSetting.ssAuto: result:=FPCUP_AUTO_MAGIC;
      TSearchSetting.ssCustom: result:=CrossUtils[aCPU,aOS,aSubarch].LibDir;
    else result:='';
    end;
  except
    result:='';
  end;
end;

function TForm2.GetToolsDirectory(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):string;
begin
  try
    case CrossUtils[aCPU,aOS,aSubarch].Setting of
      TSearchSetting.ssUp: result:='';
      TSearchSetting.ssAuto: result:=FPCUP_AUTO_MAGIC;
      TSearchSetting.ssCustom: result:=CrossUtils[aCPU,aOS,aSubarch].BinDir;
    else result:='';
    end;
  except
    result:='';
  end;
end;

function TForm2.GetCrossBuildOptions(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH):string;
begin
  result:=CrossUtils[aCPU,aOS,aSubarch].CrossBuildOptions;
end;

function TForm2.GetCrossARMArch(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): TARMARCH;
begin
  result:=CrossUtils[aCPU,aOS,aSubarch].CrossARMArch;
end;

function TForm2.GetCrossARMFPCStr(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): string;
begin
  result:=GetARMArchFPCDefine(CrossUtils[aCPU,aOS,aSubarch].CrossARMArch);
end;

function TForm2.GetCompiler(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): string;
begin
  result:=CrossUtils[aCPU,aOS,aSubarch].Compiler;
end;

procedure TForm2.SetCrossAvailable(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH; aValue:boolean);
begin
  CrossUtils[aCPU,aOS,aSubarch].Available:=aValue;
end;

function TForm2.GetCrossAvailable(aCPU:TCPU;aOS:TOS;aSubarch:TSUBARCH): boolean;
begin
  result:=CrossUtils[aCPU,aOS,aSubarch].Available;
end;

function TForm2.GetCheckIndex(aCaption:string):integer;
begin
  result:=MiscellaneousCheckListBox.Items.IndexOf(aCaption);
end;

function TForm2.GetCheckState(aCaption:string):boolean;
var
  aIndex:integer;
begin
  result:=false;
  aIndex:=GetCheckIndex(aCaption);
  if aIndex<>-1 then result:=MiscellaneousCheckListBox.Checked[aIndex];
end;

procedure TForm2.SetCheckState(aCaption:string;aState:boolean);
var
  aIndex:integer;
begin
  aIndex:=GetCheckIndex(aCaption);
  MiscellaneousCheckListBox.Checked[aIndex]:=aState;
end;

procedure TForm2.SetCheckEnabled(aCaption:string;aState:boolean);
var
  aIndex:integer;
begin
  aIndex:=GetCheckIndex(aCaption);
  MiscellaneousCheckListBox.ItemEnabled[aIndex]:=aState;
end;

function TForm2.GetRepo:boolean;
begin
  result:=GetCheckState(CaptionCheckRepo);
end;
procedure TForm2.SetRepo(value:boolean);
begin
  SetCheckState(CaptionCheckRepo,value);
end;

function TForm2.GetPackageRepo:boolean;
begin
  result:=GetCheckState(CaptionCheckPackageRepo);
end;
procedure TForm2.SetPackageRepo(value:boolean);
begin
  SetCheckState(CaptionCheckPackageRepo,value);
end;

function TForm2.GetIncludeLCL:boolean;
begin
  result:=GetCheckState(CaptionCheckIncludeLCL);
end;
procedure TForm2.SetIncludeLCL(value:boolean);
begin
  SetCheckState(CaptionCheckIncludeLCL,value);
end;

function TForm2.GetUpdateOnly:boolean;
begin
  result:=GetCheckState(CaptionCheckUpdateOnly);
end;
procedure TForm2.SetUpdateOnly(value:boolean);
begin
  SetCheckState(CaptionCheckUpdateOnly,value);
end;

function TForm2.GetSystemFPC:boolean;
begin
  result:=GetCheckState(CaptionCheckSystemFPC);
end;
procedure TForm2.SetSystemFPC(value:boolean);
begin
  SetCheckState(CaptionCheckSystemFPC,value);
end;


function TForm2.GetIncludeHelp:boolean;
begin
  result:=GetCheckState(CaptionCheckIncludeHelp);
end;
procedure TForm2.SetIncludeHelp(value:boolean);
begin
  SetCheckState(CaptionCheckIncludeHelp,value);
end;

function TForm2.GetSplitFPC:boolean;
begin
  result:=GetCheckState(CaptionCheckSplitFPC);
end;
procedure TForm2.SetSplitFPC(value:boolean);
begin
  SetCheckState(CaptionCheckSplitFPC,value);
end;

function TForm2.GetSplitLazarus:boolean;
begin
  result:=GetCheckState(CaptionCheckSplitLazarus);
end;
procedure TForm2.SetSplitLazarus(value:boolean);
begin
  SetCheckState(CaptionCheckSplitLazarus,value);
end;

function TForm2.GetUseWget:boolean;
begin
  result:=GetCheckState(CaptionCheckUseWget);
end;
procedure TForm2.SetUseWget(value:boolean);
begin
  SetCheckState(CaptionCheckUseWget,value);
end;

function TForm2.GetMakeJobs:boolean;
begin
  result:=GetCheckState(CaptionCheckUseMakeJobs);
end;
procedure TForm2.SetMakeJobs(value:boolean);
begin
  SetCheckState(CaptionCheckUseMakeJobs,value);
end;

function TForm2.GetExtraVerbose:boolean;
begin
  result:=GetCheckState(CaptionCheckExtraVerbose);
end;
procedure TForm2.SetExtraVerbose(value:boolean);
begin
  SetCheckState(CaptionCheckExtraVerbose,value);
end;

function TForm2.GetAutoSwitchURL:boolean;
begin
  result:=GetCheckState(CaptionCheckAutoSwitchURL);
end;
procedure TForm2.SetAutoSwitchURL(value:boolean);
begin
  SetCheckState(CaptionCheckAutoSwitchURL,value);
end;

function TForm2.GetSendInfo:boolean;
begin
  result:=GetCheckState(CaptionCheckSendInfo);
end;
procedure TForm2.SetSendInfo(value:boolean);
begin
  SetCheckState(CaptionCheckSendInfo,value);
end;

function TForm2.GetFpcupBootstrappersOnly:boolean;
begin
  result:=GetCheckState(CaptionCheckFpcupBootstrappersOnly);
end;
procedure TForm2.SetFpcupBootstrappersOnly(value:boolean);
begin
  SetCheckState(CaptionCheckFpcupBootstrappersOnly,value);
end;

function TForm2.GetForceLocalRepoClient:boolean;
begin
  result:=GetCheckState(CaptionCheckForceLocalRepoClient);
end;
procedure TForm2.SetForceLocalRepoClient(value:boolean);
begin
  SetCheckState(CaptionCheckForceLocalRepoClient,value);
end;

function TForm2.GetCheckUpdates:boolean;
begin
  result:=GetCheckState(CaptionCheckGetUpdates);
end;
procedure TForm2.SetCheckUpdates(value:boolean);
begin
  SetCheckState(CaptionCheckGetUpdates,value);
end;

function TForm2.GetUseSoftFloat:boolean;
begin
  result:=GetCheckState(CaptionUseSoftFloat80bit);
end;
procedure TForm2.SetUseSoftFloat(value:boolean);
begin
  SetCheckState(CaptionUseSoftFloat80bit,value);
end;

function TForm2.GetAllowOnlinePatching:boolean;
begin
  result:=GetCheckState(CaptionCheckEnableOnlinePatching);
end;
procedure TForm2.SetAllowOnlinePatching(value:boolean);
begin
  SetCheckState(CaptionCheckEnableOnlinePatching,value);
end;

function TForm2.GetApplyLocalChanges:boolean;
begin
  result:=GetCheckState(CaptionCheckApplyLocalChanges);
end;
procedure TForm2.SetApplyLocalChanges(value:boolean);
begin
  SetCheckState(CaptionCheckApplyLocalChanges,value);
end;

function TForm2.GetAddContext:boolean;
begin
  result:=GetCheckState(CaptionCheckAddContext);
end;
procedure TForm2.SetAddContext(value:boolean);
begin
  SetCheckState(CaptionCheckAddContext,value);
end;

function TForm2.GetAskConfirmation: boolean;
begin
  result := GetCheckState(CaptionCheckAskConfirmation);
end;
procedure TForm2.SetAskConfirmation(value: boolean);
begin
  SetCheckState(CaptionCheckAskConfirmation, value);
end;

function TForm2.GetFPCOptions:string;
begin
  result:=EditFPCOptions.Text;
end;
procedure TForm2.SetFPCOptions(value:string);
begin
  EditFPCOptions.Text:=value;
end;

function TForm2.GetLazarusOptions:string;
begin
  result:=EditLazarusOptions.Text;
end;
procedure TForm2.SetLazarusOptions(value:string);
begin
  EditLazarusOptions.Text:=value;
end;

function TForm2.GetFPCDebug:boolean;
begin
  result:=chkFPCDebug.Checked;
end;
procedure TForm2.SetFPCDebug(value:boolean);
begin
  chkFPCDebug.Checked:=value;
end;
function TForm2.GetLazarusDebug:boolean;
begin
  result:=chkLazarusDebug.Checked;
end;
procedure TForm2.SetLazarusDebug(value:boolean);
begin
  chkLazarusDebug.Checked:=value;
end;



function TForm2.GetFPCRevision:string;
begin
  result:=EditFPCRevision.Text;
end;
procedure TForm2.SetFPCRevision(value:string);
begin
  EditFPCRevision.Text:=value;
end;
procedure TForm2.ForceSetFPCRevision(value:string);
begin
  FPCRevision:=value;
  EditFPCRevision.Color:=clRed;
end;

function TForm2.GetLazarusRevision:string;
begin
  result:=EditLazarusRevision.Text;
end;
procedure TForm2.SetLazarusRevision(value:string);
begin
  EditLazarusRevision.Text:=value;
end;
procedure TForm2.ForceSetLazarusRevision(value:string);
begin
  LazarusRevision:=value;
  EditLazarusRevision.Color:=clRed;
end;

function TForm2.GetFPCBranch:string;
begin
  result:=EditFPCBranch.Text;
end;
procedure TForm2.SetFPCBranch(value:string);
begin
  EditFPCBranch.Text:=value;
end;

function TForm2.GetLazarusBranch:string;
begin
  result:=EditLazarusBranch.Text;
end;
procedure TForm2.SetLazarusBranch(value:string);
begin
  EditLazarusBranch.Text:=value;
end;

function TForm2.GetFPCPreScript:string;
begin
  result:=EditFPCPreInstall.Text;
end;
procedure TForm2.SetFPCPreScript(value:string);
begin
  EditFPCPreInstall.Text:=value;
end;
function TForm2.GetFPCPostScript:string;
begin
  result:=EditFPCPostInstall.Text;
end;
procedure TForm2.SetFPCPostScript(value:string);
begin
  EditFPCPostInstall.Text:=value;
end;

function TForm2.GetLazarusPreScript:string;
begin
  result:=EditLazarusPreInstall.Text;
end;
procedure TForm2.SetLazarusPreScript(value:string);
begin
  EditLazarusPreInstall.Text:=value;
end;
function TForm2.GetLazarusPostScript:string;
begin
  result:=EditLazarusPostInstall.Text;
end;
procedure TForm2.SetLazarusPostScript(value:string);
begin
  EditLazarusPostInstall.Text:=value;
end;

function TForm2.GetHTTPProxyHost:string;
begin
  result:=EditHTTPProxyHost.Text;
end;

function TForm2.GetHTTPProxyPort:integer;
var
  i:integer;
begin
  if TryStrToInt(EditHTTPProxyPort.Text,i) then result:=i;
end;

function TForm2.GetHTTPProxyUser:string;
begin
  result:=EditHTTPProxyUser.Text;
end;

function TForm2.GetHTTPProxyPass:string;
begin
  result:=EditHTTPProxyPassword.Text;
end;

function TForm2.GetPatches(const Lazarus:boolean=false):string;
var
  i:integer;
  FullPatchPath:string;
  aListBox:TListBox;
begin
  if Lazarus
    then aListBox:=ListBoxLazPatch
    else aListBox:=ListBoxFPCPatch;

  result:='';
  if aListBox.Count=0 then exit;
  for i:=0 to aListBox.Count-1 do
  begin
    FullPatchPath := TString(aListBox.Items.Objects[i]).Str;
    result:=result+FullPatchPath+',';
  end;
  // delete last comma
  if Length(result)>0 then
  begin
    Delete(result,Length(result),1);
  end;
end;

procedure TForm2.SetPatches(value:string;const Lazarus:boolean=false);
var
  PatchName: string;
  FullPatchPath: string;
  PatchList:TStringList;
  i:integer;
  aListBox:TListBox;
begin

  if Lazarus
    then aListBox:=ListBoxLazPatch
    else aListBox:=ListBoxFPCPatch;

  // cleanup
  for i := aListBox.Items.Count - 1 downto 0 do
  begin
     TString(aListBox.Items.Objects[i]).Free;
     aListBox.Items.Objects[i] := nil;
     aListBox.Items.Delete(i);
  end;

  PatchList:=TStringList.Create;
  try
    PatchList.CommaText:=value;
    if PatchList.Count=0 then exit;
    for i:=0 to PatchList.Count-1 do
    begin
      FullPatchPath := Trim(PatchList.Strings[i]);
      if Length(FullPatchPath)>0 then
      begin
        PatchName := ExtractFileName(FullPatchPath);
        aListBox.Items.AddObject(PatchName, TString.Create(FullPatchPath));
      end;
    end;
  finally
    PatchList.Free;
  end;
end;


function TForm2.GetFPCPatches:string;
begin
  result:=GetPatches(false);
end;

procedure TForm2.SetFPCPatches(value:string);
begin
  SetPatches(value,false);
end;

function TForm2.GetLazPatches:string;
begin
  result:=GetPatches(true);
end;

procedure TForm2.SetLazPatches(value:string);
begin
  SetPatches(value,true);
end;

procedure TForm2.ResetAll;
begin
  FPCOptions:='';
  LazarusOptions:='';

  FPCRevision:='';
  LazarusRevision:='';

  FPCBranch:='';
  LazarusBranch:='';

  FPCPreScript:='';
  FPCPostScript:='';

  LazarusPreScript:='';
  LazarusPostScript:='';

  FPCPatches:='';
  LazPatches:='';
end;

end.

