unit extrasettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, ExtCtrls,
  Dialogs;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';
  DELUXEKEY='fpcupdeluxeishereforyou';

type
  TCPU = (i386,x86_64,arm,aarch64,powerpc,powerpc64,mipsel,avr,jvm,i8086);
  TOS  = (windows,linux,android,darwin,freebsd,openbsd,wince,iphonesim,embedded,java,msdos);

  TCPUOS = record
    CPU:TCPU;
    OS:TOS;
  end;

  TCrossSetting = (fpcup,auto,custom);

  TCrossUtil = record
    CPU:string;
    OS:string;
    Setting:TCrossSetting;
    LibDir:string;
    BinDir:string;
    CrossBuildOptions:string;
    CrossSubArch:string;
  end;

  TCrossUtils = array[TCPU,TOS] of TCrossUtil;

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
    btnSelectLibDir: TButton;
    btnSelectBinDir: TButton;
    btnAddFPCPatch: TButton;
    btnRemFPCPatch: TButton;
    btnAddLazPatch: TButton;
    btnRemLazPatch: TButton;
    Button1: TButton;
    CheckAutoSwitchURL: TCheckBox;
    CheckIncludeHelp: TCheckBox;
    CheckSplitFPC: TCheckBox;
    CheckIncludeLCL: TCheckBox;
    CheckSplitLazarus: TCheckBox;
    CheckUseWget: TCheckBox;
    CheckUpdateOnly: TCheckBox;
    CheckRepo: TCheckBox;
    CheckPackageRepo: TCheckBox;
    CheckUseLatestGDB: TCheckBox;
    CheckExtraVerbose: TCheckBox;
    ComboBoxOS: TComboBox;
    ComboBoxCPU: TComboBox;
    EditCrossBuildOptions: TEdit;
    EditCrossSubArch: TEdit;
    EditFPCBranch: TEdit;
    EditFPCOptions: TEdit;
    EditFPCRevision: TEdit;
    EditLazarusBranch: TEdit;
    EditLazarusOptions: TEdit;
    EditLazarusRevision: TEdit;
    EditLibLocation: TEdit;
    EditBinLocation: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
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
    LabelCrossSubArch: TLabel;
    LabelFPCbranch: TLabel;
    LabelFPCOptions: TLabel;
    LabelFPCRevision: TLabel;
    LabelLazarusbranch: TLabel;
    LabelLazarusOptions: TLabel;
    LabelLazarusRevision: TLabel;
    ListBoxFPCPatch: TListBox;
    ListBoxLazPatch: TListBox;
    OpenDialog1: TOpenDialog;
    ButtonPanel: TPanel;
    RadioGroup3: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnAddPatchClick(Sender: TObject);
    procedure btnRemPatchClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBoxCPUOSChange(Sender: TObject);
    procedure EditCrossBuildOptionsChange(Sender: TObject);
    procedure EditCrossSubArchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnDirectorySelect(Sender: TObject);
    procedure RadioGroup3SelectionChanged(Sender: TObject);
  private
    FCrossUtils:TCrossUtils;

    function  GetPatches(const Lazarus:boolean=false):string;
    procedure SetPatches(value:string;const Lazarus:boolean=false);

    function GetRepo:boolean;
    function GetPackageRepo:boolean;
    function GetUpdateOnly:boolean;
    function GetIncludeLCL:boolean;
    function GetIncludeHelp:boolean;

    function GetSplitFPC:boolean;
    procedure SetSplitFPC(value:boolean);
    function GetSplitLazarus:boolean;
    procedure SetSplitLazarus(value:boolean);

    function GetUseWget:boolean;
    procedure SetUseWget(value:boolean);

    function GetExtraVerbose:boolean;
    procedure SetExtraVerbose(value:boolean);

    function GetAutoSwitchURL:boolean;
    procedure SetAutoSwitchURL(value:boolean);

    function GetHTTPProxyHost:string;
    function GetHTTPProxyPort:integer;
    function GetHTTPProxyUser:string;
    function GetHTTPProxyPass:string;
    function GetFPCOptions:string;
    procedure SetFPCOptions(value:string);
    function GetLazarusOptions:string;
    procedure SetLazarusOptions(value:string);
    function GetFPCRevision:string;
    procedure SetFPCRevision(value:string);
    function GetLazarusRevision:string;
    procedure SetLazarusRevision(value:string);
    function GetFPCBranch:string;
    procedure SetFPCBranch(value:string);
    function GetLazarusBranch:string;
    procedure SetLazarusBranch(value:string);

    function GetFPCPatches:string;
    procedure SetFPCPatches(value:string);
    function GetLazPatches:string;
    procedure SetLazPatches(value:string);

  public
    function GetCPUOSCombo(aCPU,aOS:string):TCPUOS;

    function GetLibraryDirectory(aCPU,aOS:string):string;
    function GetToolsDirectory(aCPU,aOS:string):string;
    function GetCrossBuildOptions(aCPU,aOS:string):string;
    function GetCrossSubArch(aCPU,aOS:string):string;

    property Repo:boolean read GetRepo;
    property PackageRepo:boolean read GetPackageRepo;

    property UpdateOnly:boolean read GetUpdateOnly;

    property IncludeLCL:boolean read GetIncludeLCL;
    property IncludeHelp:boolean read GetIncludeHelp;

    property SplitFPC:boolean read GetSplitFPC write SetSplitFPC;
    property SplitLazarus:boolean read GetSplitLazarus write SetSplitLazarus;

    property UseWget:boolean read GetUseWget write SetUseWget;
    property ExtraVerbose:boolean read GetExtraVerbose write SetExtraVerbose;
    property AutoSwitchURL:boolean read GetAutoSwitchURL write SetAutoSwitchURL;

    property HTTPProxyHost:string read GetHTTPProxyHost;
    property HTTPProxyPort:integer read GetHTTPProxyPort;
    property HTTPProxyUser:string read GetHTTPProxyUser;
    property HTTPProxyPass:string read GetHTTPProxyPass;

    property FPCOptions:string read GetFPCOptions write SetFPCOptions;
    property LazarusOptions:string read GetLazarusOptions write SetLazarusOptions;
    property FPCRevision:string read GetFPCRevision write SetFPCRevision;
    property LazarusRevision:string read GetLazarusRevision write SetLazarusRevision;
    property FPCBranch:string read GetFPCBranch write SetFPCBranch;
    property LazarusBranch:string read GetLazarusBranch write SetLazarusBranch;

    property FPCPatches:string read GetFPCPatches write SetFPCPatches;
    property LazPatches:string read GetLazPatches write SetLazPatches;
  end;

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
  IniFiles,
  typinfo;

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
  if (ComboBoxOS.ItemIndex<>-1) AND (ComboBoxCPU.ItemIndex<>-1) then
  begin
    if Sender=btnSelectLibDir then
       FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].LibDir:=SelectDirectoryDialog1.FileName;
    if Sender=btnSelectBinDir then
       FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].BinDir:=SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  CPU:TCPU;
  OS:TOS;
  s:string;
  //Cipher: TDCP_rc4;
  Cipher: TDCP_DES;
begin
  for OS := Low(TOS) to High(TOS) do
    ComboBoxOS.Items.Add(GetEnumName(TypeInfo(TOS),Ord(OS)));
  for CPU := Low(TCPU) to High(TCPU) do
    ComboBoxCPU.Items.Add(GetEnumName(TypeInfo(TCPU),Ord(CPU)));

  for OS := Low(TOS) to High(TOS) do
  begin
    for CPU := Low(TCPU) to High(TCPU) do
    begin
      FCrossUtils[CPU,OS].CPU:=GetEnumName(TypeInfo(TCPU),Ord(CPU));
      FCrossUtils[CPU,OS].OS:=GetEnumName(TypeInfo(TOS),Ord(OS));
    end;
  end;

  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    CheckRepo.Checked:=ReadBool('General','GetRepo',True);
    CheckPackageRepo.Checked:=ReadBool('General','GetPackageRepo',False);
    CheckIncludeHelp.Checked:=ReadBool('General','IncludeHelp',False);

    CheckIncludeLCL.Checked:=ReadBool('Cross','IncludeLCL',False);

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
        Cipher.InitStr(DELUXEKEY,TDCP_sha256);
        s:=Cipher.DecryptString(s);
      finally
        Cipher.Burn;
        Cipher.Free;
      end;
    end;
    EditHTTPProxyPassword.Text:=s;

    for OS := Low(TOS) to High(TOS) do
    begin
      for CPU := Low(TCPU) to High(TCPU) do
      begin
        s:=GetEnumName(TypeInfo(TCPU),Ord(CPU))+'-'+GetEnumName(TypeInfo(TOS),Ord(OS));
        FCrossUtils[CPU,OS].Setting:=TCrossSetting(ReadInteger(s,'Setting',Ord(fpcup)));
        FCrossUtils[CPU,OS].LibDir:=ReadString(s,'LibPath','');
        FCrossUtils[CPU,OS].BinDir:=ReadString(s,'BinPath','');
        FCrossUtils[CPU,OS].CrossBuildOptions:=ReadString(s,'CrossBuildOptions','');
        FCrossUtils[CPU,OS].CrossSubArch:=ReadString(s,'CrossSubArch','');
      end;
    end;

  finally
    Free;
  end;

  {$IF (defined(MSWINDOWS)) OR (defined(Darwin)) OR (defined(OpenBSD))}
  // there are default setings for the downloader, so disable user access
  CheckUseWget.Enabled:=False;
  {$endif}

  {$ifdef CPUAARCH64}
  // disable some features
  GroupBox4.Enabled:=False;
  {$endif CPUAARCH64}
  {$ifdef CPUARM}
  // disable some features
  GroupBox4.Enabled:=False;
  {$endif CPUARM}

end;

procedure TForm2.ComboBoxCPUOSChange(Sender: TObject);
var
  e:boolean;
begin
  e:=((ComboBoxOS.ItemIndex<>-1) AND (ComboBoxCPU.ItemIndex<>-1));
  RadioGroup3.Enabled:=e;
  EditCrossBuildOptions.Enabled:=e;
  EditCrossSubArch.Enabled:=e;
  if e then
  begin
    EditLibLocation.Text:=FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].LibDir;
    EditBinLocation.Text:=FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].BinDir;
    EditCrossBuildOptions.Text:=FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].CrossBuildOptions;
    EditCrossSubArch.Text:=FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].CrossSubArch;
    RadioGroup3.ItemIndex:=Ord(FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].Setting);
  end;
end;

procedure TForm2.EditCrossBuildOptionsChange(Sender: TObject);
begin
  FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].CrossBuildOptions:=TEdit(Sender).Text;
end;

procedure TForm2.EditCrossSubArchChange(Sender: TObject);
begin
  FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].CrossSubArch:=TEdit(Sender).Text;
end;

procedure TForm2.btnAddPatchClick(Sender: TObject);
var
  PatchName: string;
  FullPatchPath: string;
  aListBox:TListBox;
begin
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

procedure TForm2.Button1Click(Sender: TObject);
var
  CPU:TCPU;
  OS:TOS;
  s:string;
  x:integer;
begin
  InfoForm:= TInfoForm.Create(Self);
  try
    for OS := Low(TOS) to High(TOS) do
    begin
      for CPU := Low(TCPU) to High(TCPU) do
      begin
        x:=InfoForm.Memo1.Lines.Count;
        s:=GetEnumName(TypeInfo(TCPU),Ord(CPU))+'-'+GetEnumName(TypeInfo(TOS),Ord(OS));
        if FCrossUtils[CPU,OS].Setting=auto then
        begin
          InfoForm.Memo1.Lines.Append(s+': full auto search tools and libraries.');
        end
        else
        if FCrossUtils[CPU,OS].Setting=custom then
        begin
          InfoForm.Memo1.Lines.Append(s+' (manual settings):');
          InfoForm.Memo1.Lines.Append('  libs     : '+FCrossUtils[CPU,OS].LibDir);
          InfoForm.Memo1.Lines.Append('  bins      : '+FCrossUtils[CPU,OS].BinDir);
        end;

        if Length(FCrossUtils[CPU,OS].CrossBuildOptions)>0 then
        begin
          if x=InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append(s);
          InfoForm.Memo1.Lines.Append('  options : '+FCrossUtils[CPU,OS].CrossBuildOptions);
        end;

        if Length(FCrossUtils[CPU,OS].CrossSubArch)>0 then
        begin
          if x=InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append(s);
          InfoForm.Memo1.Lines.Append('  subarch : '+FCrossUtils[CPU,OS].CrossSubArch);
        end;

        if x<>InfoForm.Memo1.Lines.Count then InfoForm.Memo1.Lines.Append('');

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
  s:string;
  i:integer;
  //Cipher: TDCP_rc4;
  Cipher: TDCP_DES;
begin
  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    WriteBool('General','GetRepo',CheckRepo.Checked);
    WriteBool('General','GetPackageRepo',CheckPackageRepo.Checked);
    WriteBool('General','IncludeHelp',CheckIncludeHelp.Checked);

    WriteBool('Cross','IncludeLCL',CheckIncludeLCL.Checked);

    WriteString('ProxySettings','HTTPProxyURL',EditHTTPProxyHost.Text);
    if TryStrToInt(EditHTTPProxyPort.Text,i) then WriteInteger('ProxySettings','HTTPProxyPort',i);
    WriteString('ProxySettings','HTTPProxyUser',EditHTTPProxyUser.Text);

    // add some security into the password storage ... ;-)
    s:=EditHTTPProxyPassword.Text;
    if Length(s)>0 then
    begin
      //Cipher:= TDCP_rc4.Create(nil);
      Cipher := TDCP_DES.Create(nil);
      try
        Cipher.InitStr(DELUXEKEY,TDCP_sha256);
        s:=Cipher.EncryptString(s);
      finally
        Cipher.Burn;
        Cipher.Free;
      end;
    end;
    WriteString('ProxySettings','HTTPProxyPass',s);

    for OS := Low(TOS) to High(TOS) do
    begin
      for CPU := Low(TCPU) to High(TCPU) do
      begin
        // skip non-combi's to reduce size of ini-file
        if ((OS=java) AND (CPU<>jvm)) OR ((CPU=jvm) AND (OS<>java) AND (OS<>android)) then continue;
        if (OS=android) AND ((CPU<>arm) AND (CPU<>aarch64) AND (CPU<>jvm) AND (CPU<>mipsel)) then continue;
        if (OS=iphonesim) AND ((CPU<>i386) AND (CPU<>x86_64)) then continue;
        if (OS=wince) AND (CPU<>arm) then continue;
        if (OS=windows) AND ((CPU=arm) OR (CPU=aarch64)) then continue;
        if (CPU=powerpc) AND ((OS<>linux) AND (OS<>darwin)) then continue;
        if (CPU=powerpc64) AND ((OS<>linux) AND (OS<>darwin)) then continue;
        if (CPU=aarch64) AND ((OS<>linux) AND (OS<>darwin) AND (OS<>android)) then continue;
        if (CPU=mipsel) AND ((OS<>linux) AND (OS<>android)) then continue;
        if (CPU=avr) AND (OS<>embedded) then continue;

        s:=GetEnumName(TypeInfo(TCPU),Ord(CPU))+'-'+GetEnumName(TypeInfo(TOS),Ord(OS));
        WriteInteger(s,'Setting',Ord(FCrossUtils[CPU,OS].Setting));
        WriteString(s,'LibPath',FCrossUtils[CPU,OS].LibDir);
        WriteString(s,'BinPath',FCrossUtils[CPU,OS].BinDir);
        WriteString(s,'CrossBuildOptions',FCrossUtils[CPU,OS].CrossBuildOptions);
        WriteString(s,'CrossSubArch',FCrossUtils[CPU,OS].CrossSubArch);
      end;
    end;

  finally
    Free;
  end;

  for i := 0 to ListBoxFPCPatch.Items.Count - 1 do
  begin
     TString(ListBoxFPCPatch.Items.Objects[i]).Free;
     ListBoxFPCPatch.Items.Objects[i] := nil;
  end;

end;

procedure TForm2.RadioGroup3SelectionChanged(Sender: TObject);
var
  e:boolean;
  i:integer;
begin
  i:=(Sender AS TRadioGroup).ItemIndex;
  if (ComboBoxOS.ItemIndex<>-1) AND (ComboBoxCPU.ItemIndex<>-1) then
  begin
    FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].Setting:=TCrossSetting(i);
  end;
  e:=(i=2);
  EditLibLocation.Enabled:=e;
  EditBinLocation.Enabled:=e;
  btnSelectLibDir.Enabled:=e;
  btnSelectBinDir.Enabled:=e;
end;

function TForm2.GetCPUOSCombo(aCPU,aOS:string):TCPUOS;
var
  xCPU:TCPU;
  xOS:TOS;
  aLocalCPU,aLocalOS:string;
begin
  aLocalCPU:=aCPU;
  //if length(aLocalCPU)=0 then aLocalCPU:=GetTargetCPU;
  if aLocalCPU='ppc' then aLocalCPU:='powerpc';
  if aLocalCPU='ppc64' then aLocalCPU:='powerpc64';

  xCPU:=TCPU(GetEnumValue(TypeInfo(TCPU),aLocalCPU));
  if Ord(xCPU) < 0 then
    raise Exception.CreateFmt('Invalid CPU name "%s" for GetCPUOSCombo.', [aLocalCPU]);

  aLocalOS:=aOS;
  //if length(aLocalOS)=0 then aLocalOS:=GetTargetOS;
  if aLocalOS='win32' then aLocalOS:='windows';
  if aLocalOS='win64' then aLocalOS:='windows';
  if aLocalOS='i-sim' then aLocalOS:='iphonesim';
  if aLocalOS='i-simulator' then aLocalOS:='iphonesim';
  if aLocalOS='iphone-simulator' then aLocalOS:='iphonesim';
  if aLocalOS='iphonesimulator' then aLocalOS:='iphonesim';

  xOS:=TOS(GetEnumValue(TypeInfo(TOS),aLocalOS));
  if Ord(xOS) < 0 then
    raise Exception.CreateFmt('Invalid OS name "%s" for GetCPUOSCombo.', [aLocalOS]);

  result.CPU:=xCPU;
  result.OS:=xOS;
end;

function TForm2.GetLibraryDirectory(aCPU,aOS:string):string;
var
  xCPUOS:TCPUOS;
begin
  try
    xCPUOS:=GetCPUOSCombo(aCPU,aOS);
    case FCrossUtils[xCPUOS.CPU,xCPUOS.OS].Setting of
      fpcup: result:='';
      auto: result:='FPCUP_AUTO';
      custom: result:=FCrossUtils[xCPUOS.CPU,xCPUOS.OS].LibDir;
    else result:='';
    end;
  except
    result:='';
  end;
end;

function TForm2.GetToolsDirectory(aCPU,aOS:string):string;
var
  xCPUOS:TCPUOS;
begin
  try
    xCPUOS:=GetCPUOSCombo(aCPU,aOS);
    case FCrossUtils[xCPUOS.CPU,xCPUOS.OS].Setting of
      fpcup: result:='';
      auto: result:='FPCUP_AUTO';
      custom: result:=FCrossUtils[xCPUOS.CPU,xCPUOS.OS].BinDir;
    else result:='';
    end;
  except
    result:='';
  end;
end;

function TForm2.GetCrossBuildOptions(aCPU,aOS:string):string;
var
  xCPUOS:TCPUOS;
begin
  xCPUOS:=GetCPUOSCombo(aCPU,aOS);
  result:=FCrossUtils[xCPUOS.CPU,xCPUOS.OS].CrossBuildOptions;
end;

function TForm2.GetCrossSubArch(aCPU, aOS: string): string;
var
  xCPUOS:TCPUOS;
begin
  xCPUOS:=GetCPUOSCombo(aCPU,aOS);
  result:=FCrossUtils[xCPUOS.CPU,xCPUOS.OS].CrossSubArch;
end;

function TForm2.GetRepo:boolean;
begin
  result:=CheckRepo.Checked;
end;

function TForm2.GetPackageRepo:boolean;
begin
  result:=CheckPackageRepo.Checked;
end;

function TForm2.GetUpdateOnly:boolean;
begin
  result:=CheckUpdateOnly.Checked;
end;

function TForm2.GetIncludeLCL:boolean;
begin
  result:=CheckIncludeLCL.Checked;
end;

function TForm2.GetIncludeHelp:boolean;
begin
  result:=CheckIncludeHelp.Checked;
end;

function TForm2.GetSplitFPC:boolean;
begin
  result:=CheckSplitFPC.Checked;
end;
procedure TForm2.SetSplitFPC(value:boolean);
begin
  CheckSplitFPC.Checked:=value;
end;

function TForm2.GetSplitLazarus:boolean;
begin
  result:=CheckSplitLazarus.Checked;
end;
procedure TForm2.SetSplitLazarus(value:boolean);
begin
  CheckSplitLazarus.Checked:=value;
end;

function TForm2.GetUseWget:boolean;
begin
  result:=CheckUseWget.Checked;
end;
procedure TForm2.SetUseWget(value:boolean);
begin
  CheckUseWget.Checked:=value;
end;

function TForm2.GetExtraVerbose:boolean;
begin
  result:=CheckExtraVerbose.Checked;
end;
procedure TForm2.SetExtraVerbose(value:boolean);
begin
  CheckExtraVerbose.Checked:=value;
end;

function TForm2.GetAutoSwitchURL:boolean;
begin
  result:=CheckAutoSwitchURL.Checked;
end;
procedure TForm2.SetAutoSwitchURL(value:boolean);
begin
  CheckAutoSwitchURL.Checked:=value;
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

function TForm2.GetFPCRevision:string;
begin
  result:=EditFPCRevision.Text;
end;
procedure TForm2.SetFPCRevision(value:string);
begin
  EditFPCRevision.Text:=value;
end;

function TForm2.GetLazarusRevision:string;
begin
  result:=EditLazarusRevision.Text;
end;
procedure TForm2.SetLazarusRevision(value:string);
begin
  EditLazarusRevision.Text:=value;
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

end.

