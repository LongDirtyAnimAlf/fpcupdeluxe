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
    rgrpSelectCPU: TRadioGroup;
    rgrpSelectSubarch: TRadioGroup;
    RadioGroupARMArch: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnSelectDirClick(Sender: TObject);
    procedure EditCrossBuildOptionsEditingDone(Sender: TObject);
    procedure EditDeleteDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupARMArchSelectionChanged(Sender: TObject);
    procedure rgrpSelectCPUSelectionChanged(Sender: TObject);
    procedure rgrpSelectSubarchSelectionChanged(Sender: TObject);
  private
    LocalCPU:TCPU;
    LocalOS:TOS;
    LocalSUBARCH:TSUBARCH;
    SUBARCHStore:array[TCPU,TOS] of TSUBARCH;
    procedure SetGUI;
  public
    procedure SetCrossTarget({%H-}aSender:TObject;aCPU:TCPU;aOS:TOS);
    function GetSelectedSubArch(aCPU:TCPU;aOS:TOS):TSUBARCH;
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
  aOS:TOS;
  ARMArch:TARMARCH;
begin
  // Fill CPU radiogroup
  for aCPU in SUBARCH_CPU do
    rgrpSelectCPU.Items.Append(GetCPU(aCPU));
  rgrpSelectCPU.ItemIndex:=0;

  // Fill ARM ABI radiogroup
  for ARMArch := Low(TARMARCH) to High(TARMARCH) do
    RadioGroupARMArch.Items.Add(GetEnumNameSimple(TypeInfo(TARMARCH),Ord(ARMArch)));
  RadioGroupARMArch.ItemIndex:=0;

  LocalCPU:=TCPU.cpuNone;
  LocalOS:=TOS.osNone;
  LocalSUBARCH:=TSUBARCH.saNone;

  for aCPU in TCPU do
  begin
    for aOS in TOS do
    begin
      SUBARCHStore[aCPU,aOS]:=TSUBARCH.saNone;
    end;
  end;

end;

procedure TSubarchForm.FormShow(Sender: TObject);
begin
  SetGUI;
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
      xARMArch:=TARMARCH.none
    else
      xARMArch:=TARMARCH(i);
    CrossUtils[LocalCPU,LocalOS,LocalSUBARCH].CrossARMArch:=xARMArch;
  end;
end;

procedure TSubarchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SUBARCHStore[LocalCPU,LocalOS]:=LocalSUBARCH;
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
        Setting:=TSearchSetting.ssUp;
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
end;

procedure TSubarchForm.SetCrossTarget(aSender:TObject;aCPU:TCPU;aOS:TOS);
begin
  LocalCPU:=aCPU;
  LocalOS:=aOS;
  LocalSUBARCH:=SUBARCHStore[LocalCPU,LocalOS];
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
  btnSelectLibDir.Enabled:=e;
  btnSelectBinDir.Enabled:=e;
end;

function TSubarchForm.GetSelectedSubArch(aCPU:TCPU;aOS:TOS):TSUBARCH;
begin
  result:=SUBARCHStore[aCPU,aOS];
end;

end.

