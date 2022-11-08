unit subarch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, m_crossinstaller;

type
  { TSubarchForm }

  TSubarchForm = class(TForm)
    btnSelectBinDir: TButton;
    btnSelectLibDir: TButton;
    ButtonPanel1: TButtonPanel;
    EditBinLocation: TEdit;
    EditCrossBuildOptions: TEdit;
    EditLibLocation: TEdit;
    GroupBox4: TGroupBox;
    LabelCrossBuildOptions: TLabel;
    LabelLibraries: TLabel;
    LabelBintools: TLabel;
    RadioGroupABI: TRadioGroup;
    rgrpSelectCPU: TRadioGroup;
    rgrpSelectSubarch: TRadioGroup;
    RadioGroupARMArch: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnSelectDirClick(Sender: TObject);
    procedure EditCrossBuildOptionsEditingDone(Sender: TObject);
    procedure EditDeleteDblClick(Sender: TObject);
    procedure FormClose({%H-}Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormShow({%H-}Sender: TObject);
    procedure RadioGroupABISelectionChanged(Sender: TObject);
    procedure RadioGroupARMArchSelectionChanged(Sender: TObject);
    procedure rgrpSelectCPUSelectionChanged(Sender: TObject);
    procedure rgrpSelectSubarchSelectionChanged({%H-}Sender: TObject);
  private
    LocalCPU:TCPU;
    LocalOS:TOS;
    LocalSUBARCH:TSUBARCH;
    procedure SetGUI;
    procedure SetABI;
  public
    procedure SetCrossTarget({%H-}aSender:TObject;aCPU:TCPU;aOS:TOS);
  end;

var
  SubarchForm: TSubarchForm;

implementation

{$R *.lfm}

uses
  fpcuputil,
  installerCore,
  installerUniversal;

{ TSubarchForm }

procedure TSubarchForm.FormCreate(Sender: TObject);
var
  aCPU:TCPU;
  ARMArch:TARMARCH;
  aABI:TABI;
begin
  // Fill CPU radiogroup
  for aCPU in SUBARCH_CPU do
    rgrpSelectCPU.Items.Append(GetCPU(aCPU));
  rgrpSelectCPU.ItemIndex:=0;

  // Fill ARM Arch radiogroup
  for ARMArch := Low(TARMARCH) to High(TARMARCH) do
    RadioGroupARMArch.Items.Add(GetEnumNameSimple(TypeInfo(TARMARCH),Ord(ARMArch)));
  RadioGroupARMArch.ItemIndex:=0;

  // Fill ABI radiogroup
  for aABI in ABI_ARM do
    RadioGroupABI.Items.Add(GetEnumNameSimple(TypeInfo(TABI),Ord(aABI)));
  RadioGroupABI.ItemIndex:=0;


  LocalCPU:=TCPU.cpuNone;
  LocalOS:=TOS.osNone;
  LocalSUBARCH:=TSUBARCH.saNone;
end;

procedure TSubarchForm.FormShow(Sender: TObject);
begin
  SetGUI;
end;

procedure TSubarchForm.RadioGroupABISelectionChanged(Sender: TObject);
var
  i:integer;
  ABI:TABI;
  s:string;
begin
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    i:=(Sender AS TRadioGroup).ItemIndex;
    if i=-1 then
      ABI:=TABI.default
    else
      ABI:=GetTABI((Sender AS TRadioGroup).Items[i]);

    s:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions;
    i:=Pos('-Ca',s);
    if (i>0) then
    begin
      //while ((i<=Length(s)) AND (s[i] in ['-','0'..'9','a'..'z','A'..'Z'])) do Delete(s,i,1);
      while ((i<=Length(s)) AND (s[i]<>' ')) do Delete(s,i,1);
    end;
    if i=0 then
    begin
      s:=Trim(s);
      i:=Length(s);
      if (i>0) AND (s[i]<>' ') then
      begin
        s:=s+' ';
        Inc(i);
      end;
      Inc(i);
    end;
    if ABI<>TABI.default then
      Insert('-Ca'+UpperCase(GetABI(ABI)),s,i);
    s:=Trim(s);
    s:=s+' ';
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions:=s;
    EditCrossBuildOptions.Text:=s;
  end;
end;

procedure TSubarchForm.RadioGroupARMArchSelectionChanged(Sender: TObject);
var
  i:integer;
  xARMArch:TARMARCH;
begin
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    i:=(Sender AS TRadioGroup).ItemIndex;
    if i=-1 then
      xARMArch:=DEFAULTARMARCH
    else
      xARMArch:=TARMARCH(i);
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossARMArch:=xARMArch;
  end;
end;

procedure TSubarchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SetSelectedSubArch(LocalCPU,LocalOS,LocalSUBARCH);
  LocalCPU:=TCPU.cpuNone;
  LocalOS:=TOS.osNone;
end;

procedure TSubarchForm.btnSelectDirClick(Sender: TObject);
begin
  if Sender=btnSelectLibDir then SelectDirectoryDialog1.InitialDir:=EditLibLocation.Text;
  if Sender=btnSelectBinDir then SelectDirectoryDialog1.InitialDir:=EditBinLocation.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    if Sender=btnSelectLibDir then EditLibLocation.Text:=SelectDirectoryDialog1.FileName;
    if Sender=btnSelectBinDir then EditBinLocation.Text:=SelectDirectoryDialog1.FileName;

    if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
    begin
      with CrossUtils[LocalCPU,LocalOS,LocalSUBARCH] do
      begin
        if Sender=btnSelectLibDir then
          LibDir:=EditLibLocation.Text;
        if Sender=btnSelectBinDir then
          BinDir:=EditBinLocation.Text;
        Setting:=TSearchSetting.ssCustom;
      end;
    end;
  end;
end;

procedure TSubarchForm.EditCrossBuildOptionsEditingDone(Sender: TObject);
begin
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions:=TEdit(Sender).Text;
  end;
end;

procedure TSubarchForm.EditDeleteDblClick(Sender: TObject);
begin
  TEdit(Sender).Text:='';
  if ((LocalCPU<>TCPU.cpuNone) AND (LocalOS<>TOS.osNone)) then
  begin
    with CrossUtils[LocalCPU,LocalOS,LocalSUBARCH] do
    begin
      if Sender=EditLibLocation then
        LibDir:='';
      if Sender=EditBinLocation then
        BinDir:='';
      if (Length(BinDir)=0) AND (Length(LibDir)=0) then
        Setting:=DEFAULTSEARCHSETTING;
      if Sender=EditCrossBuildOptions then
        CrossBuildOptions:='';
    end;
  end;
end;

procedure TSubarchForm.rgrpSelectCPUSelectionChanged(Sender: TObject);
var
  CPU:TCPU;
  SUBARCH:TSUBARCH;
  Subarchs:TSUBARCHS;
  ABI:TABI;
  ABIs:TABIS;
  i:integer;
begin
  if Sender=nil then
  begin
    CPU:=LocalCPU;
    i:=rgrpSelectCPU.Items.IndexOf(GetCPU(CPU));
    if (i<>-1) then rgrpSelectCPU.ItemIndex:=i;
  end
  else
  begin
    i:=rgrpSelectCPU.ItemIndex;
    if (i<0) then exit;
    CPU:=GetTCPU(rgrpSelectCPU.Items[i]);
    //LocalCPU:=CPU;
  end;

  BeginFormUpdate;
  try

    rgrpSelectSubarch.Items.Clear;
    Subarchs:=GetSubarchs(CPU,LocalOS);
    for SUBARCH in Subarchs do
    begin
      if (SUBARCH<>TSUBARCH.saNone) then
      begin
        rgrpSelectSubarch.Items.Append(GetSubarch(SUBARCH));
        if SUBARCH=LocalSUBARCH then rgrpSelectSubarch.ItemIndex:=Pred(rgrpSelectSubarch.Items.Count);
      end;
    end;
    if rgrpSelectSubarch.Items.Count=1 then rgrpSelectSubarch.ItemIndex:=0;

    RadioGroupABI.Items.Clear;
    ABIs:=GetABIs(CPU,LocalOS);
    for ABI in ABIs do
    begin
      if (ABI=TABI.default) then
        RadioGroupABI.Items.Append('default')
      else
        RadioGroupABI.Items.Append(GetABI(ABI));
    end;

  finally
    EndFormUpdate;
  end;
end;

procedure TSubarchForm.rgrpSelectSubarchSelectionChanged(Sender: TObject);
var
  i:integer;
begin
  LocalSUBARCH:=TSUBARCH.saNone;
  i:=rgrpSelectSubarch.ItemIndex;
  if (i<>-1) then
    LocalSUBARCH:=GetTSubarch(rgrpSelectSubarch.Items[i]);
  SetGUI;
  SetABI;
end;

procedure TSubarchForm.SetCrossTarget(aSender:TObject;aCPU:TCPU;aOS:TOS);
begin
  LocalCPU:=aCPU;
  LocalOS:=aOS;
  LocalSUBARCH:=GetSelectedSubArch(LocalCPU,LocalOS);
  rgrpSelectCPUSelectionChanged(nil);
end;

procedure TSubarchForm.SetGUI;
var
  e:boolean;
begin
  EditLibLocation.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].LibDir;
  EditBinLocation.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].BinDir;
  EditCrossBuildOptions.Text:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions;
  RadioGroupARMArch.ItemIndex:=Ord(CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossARMArch);

  e:=(LocalSUBARCH<>TSUBARCH.saNone);
  EditLibLocation.Enabled:=e;
  EditBinLocation.Enabled:=e;
  EditCrossBuildOptions.Enabled:=e;
  RadioGroupARMArch.Enabled:=(e AND (LocalCPU=TCPU.arm));
  RadioGroupABI.Enabled:=(e AND (LocalCPU in [TCPU.arm,TCPU.xtensa]));
  btnSelectLibDir.Enabled:=e;
  btnSelectBinDir.Enabled:=e;
end;

procedure TSubarchForm.SetABI;
var
  s:string;
  aABI:TABI;
  i:integer;
begin
  i:=-1;
  s:=CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossBuildOptions;
  for aABI in ABI_ARM do
  begin
    if (aABI=TABI.default) then continue;
    if (Pos('-Ca'+UpperCase(GetABI(aABI)),s)>0) then
      i:=RadioGroupABI.Items.IndexOf(GetABI(aABI));
  end;
  if (i<>-1) then
    RadioGroupABI.ItemIndex:=i;
end;

end.

