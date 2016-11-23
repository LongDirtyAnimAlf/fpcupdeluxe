unit extrasettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, ExtCtrls,
  Dialogs;

Const
  DELUXEFILENAME='fpcupdeluxe.ini';

type
  TCPU = (i386,x86_64,arm,aarch64,jvm);
  TOS  = (windows,linux,android,darwin,freebsd,wince,java);
  TCrossSetting = (fpcup,auto,custom);

  TCrossUtil = record
    CPU:string;
    OS:string;
    Setting:TCrossSetting;
    LibDir:string;
    BinDir:string;
  end;

  TCrossUtils = array[TCPU,TOS] of TCrossUtil;

  { TForm2 }
  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnSelectLibDir: TButton;
    btnSelectBinDir: TButton;
    CheckIncludeFPCIDE: TCheckBox;
    CheckIncludeHelp: TCheckBox;
    CheckIncludeLCL: TCheckBox;
    CheckUpdateOnly: TCheckBox;
    CheckRepo: TCheckBox;
    CheckPackageRepo: TCheckBox;
    ComboBoxOS: TComboBox;
    ComboBoxCPU: TComboBox;
    EditFPCbranch: TEdit;
    EditFPCrevision: TEdit;
    EditLazarusbranch: TEdit;
    EditLazarusrevision: TEdit;
    EditLibLocation: TEdit;
    EditBinLocation: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    EditHTTPProxyHost: TEdit;
    EditHTTPProxyPort: TEdit;
    EditHTTPProxyUser: TEdit;
    EditHTTPProxyPassword: TEdit;
    GroupBoxFPCLazBranchRevision: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelFPCbranch: TLabel;
    LabelFPCrevision: TLabel;
    LabelLazarusbranch: TLabel;
    LabelLazarusrevision: TLabel;
    RadioGroupNPFPCbranch: TRadioGroup;
    RadioGroupNPLazarusbranch: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure ComboBoxCPUOSChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnDirectorySelect(Sender: TObject);
    procedure RadioGroup3SelectionChanged(Sender: TObject);
  private
    FCrossUtils:TCrossUtils;
    function GetRepo:boolean;
    function GetPackageRepo:boolean;
    function GetUpdateOnly:boolean;
    function GetIncludeLCL:boolean;
    function GetIncludeHelp:boolean;
    function GetHTTPProxyHost:string;
    function GetHTTPProxyPort:integer;
    function GetHTTPProxyUser:string;
    function GetHTTPProxyPass:string;
  public
    function GetLibraryDirectory(aCPU,aOS:string):string;
    function GetToolsDirectory(aCPU,aOS:string):string;

    property Repo:boolean read GetRepo;
    property PackageRepo:boolean read GetPackageRepo;

    property UpdateOnly:boolean read GetUpdateOnly;

    property IncludeLCL:boolean read GetIncludeLCL;
    property IncludeHelp:boolean read GetIncludeHelp;

    property HTTPProxyHost:string read GetHTTPProxyHost;
    property HTTPProxyPort:integer read GetHTTPProxyPort;
    property HTTPProxyUser:string read GetHTTPProxyUser;
    property HTTPProxyPass:string read GetHTTPProxyPass;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses
  fpcuputil,
  IniFiles,
  typinfo;

{ TForm2 }

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
    CheckIncludeHelp.Checked:=ReadBool('General','IncludeHelp',True);

    CheckIncludeLCL.Checked:=ReadBool('Cross','IncludeLCL',False);

    EditHTTPProxyHost.Text:=ReadString('ProxySettings','HTTPProxyURL','');
    EditHTTPProxyPort.Text:=InttoStr(ReadInteger('ProxySettings','HTTPProxyPort',8080));
    EditHTTPProxyUser.Text:=ReadString('ProxySettings','HTTPProxyUser','');
    EditHTTPProxyPassword.Text:=ReadString('ProxySettings','HTTPProxyPass','');

    for OS := Low(TOS) to High(TOS) do
    begin
      for CPU := Low(TCPU) to High(TCPU) do
      begin
        s:=GetEnumName(TypeInfo(TCPU),Ord(CPU))+'-'+GetEnumName(TypeInfo(TOS),Ord(OS));
        FCrossUtils[CPU,OS].Setting:=TCrossSetting(ReadInteger(s,'Setting',Ord(fpcup)));
        FCrossUtils[CPU,OS].LibDir:=ReadString(s,'LibPath','');
        FCrossUtils[CPU,OS].BinDir:=ReadString(s,'BinPath','');
      end;
    end;


  finally
    Free;
  end;

end;

procedure TForm2.ComboBoxCPUOSChange(Sender: TObject);
var
  e:boolean;
begin
  e:=((ComboBoxOS.ItemIndex<>-1) AND (ComboBoxCPU.ItemIndex<>-1));
  RadioGroup3.Enabled:=e;
  if e then
  begin
    EditLibLocation.Text:=FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].LibDir;
    EditBinLocation.Text:=FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].BinDir;
    RadioGroup3.ItemIndex:=Ord(FCrossUtils[TCPU(ComboBoxCPU.ItemIndex),TOS(ComboBoxOS.ItemIndex)].Setting);
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var
  CPU:TCPU;
  OS:TOS;
  s:string;
begin
  with TIniFile.Create(SafeGetApplicationPath+DELUXEFILENAME) do
  try
    WriteBool('General','GetRepo',CheckRepo.Checked);
    WriteBool('General','GetPackageRepo',CheckPackageRepo.Checked);
    WriteBool('General','IncludeHelp',CheckIncludeHelp.Checked);

    WriteBool('Cross','IncludeLCL',CheckIncludeLCL.Checked);

    WriteString('ProxySettings','HTTPProxyURL',EditHTTPProxyHost.Text);
    WriteInteger('ProxySettings','HTTPProxyPort',StrToInt(EditHTTPProxyPort.Text));
    WriteString('ProxySettings','HTTPProxyUser',EditHTTPProxyUser.Text);
    WriteString('ProxySettings','HTTPProxyPass',EditHTTPProxyPassword.Text);

    for OS := Low(TOS) to High(TOS) do
    begin
      for CPU := Low(TCPU) to High(TCPU) do
      begin
        // skip non-combi's
        if ((OS=java) AND (CPU<>jvm)) OR ((CPU=jvm) AND (OS<>java)) then continue;
        if (OS=android) AND (CPU<>arm) then continue;
        if (OS=wince) AND (CPU<>arm) then continue;
        if (OS=windows) AND (CPU=arm) then continue;
        if (OS=windows) AND (CPU=aarch64) then continue;
        if (OS=darwin) AND ((CPU=aarch64) OR (CPU=arm)) then continue;

        s:=GetEnumName(TypeInfo(TCPU),Ord(CPU))+'-'+GetEnumName(TypeInfo(TOS),Ord(OS));
        WriteInteger(s,'Setting',Ord(FCrossUtils[CPU,OS].Setting));
        WriteString(s,'LibPath',FCrossUtils[CPU,OS].LibDir);
        WriteString(s,'BinPath',FCrossUtils[CPU,OS].BinDir);
      end;
    end;

  finally
    Free;
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

function TForm2.GetLibraryDirectory(aCPU,aOS:string):string;
var
  xCPU:TCPU;
  xOS:TOS;
begin
  try
    xCPU:=TCPU(GetEnumValue(TypeInfo(TCPU),aCPU));
    xOS:=TOS(GetEnumValue(TypeInfo(TOS),aOS));
    case FCrossUtils[xCPU,xOS].Setting of
      fpcup: result:='FPCUP_AUTO';
      auto: result:='FPCUP_FULLAUTO';
      custom: result:=FCrossUtils[xCPU,xOS].LibDir;
    else result:='';
    end;
  except
    result:='';
  end;
end;

function TForm2.GetToolsDirectory(aCPU,aOS:string):string;
var
  xCPU:TCPU;
  xOS:TOS;
begin
  try
    xCPU:=TCPU(GetEnumValue(TypeInfo(TCPU),aCPU));
    xOS:=TOS(GetEnumValue(TypeInfo(TOS),aOS));
    case FCrossUtils[xCPU,xOS].Setting of
      fpcup: result:='FPCUP_AUTO';
      auto: result:='FPCUP_FULLAUTO';
      custom: result:=FCrossUtils[xCPU,xOS].BinDir;
    else result:='';
    end;
  except
    result:='';
  end;
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

function TForm2.GetHTTPProxyHost:string;
begin
  result:=EditHTTPProxyHost.Text;
end;

function TForm2.GetHTTPProxyPort:integer;
begin
  result:=StrToInt(EditHTTPProxyPort.Text);
end;

function TForm2.GetHTTPProxyUser:string;
begin
  result:=EditHTTPProxyUser.Text;
end;

function TForm2.GetHTTPProxyPass:string;
begin
  result:=EditHTTPProxyPassword.Text;
end;

end.

