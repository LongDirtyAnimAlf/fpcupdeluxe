unit subarch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TSubarchForm }

  TSubarchForm = class(TForm)
    btnSelectBinDir: TButton;
    btnSelectLibDir: TButton;
    EditBinLocation: TEdit;
    EditCrossBuildOptions: TEdit;
    EditLibLocation: TEdit;
    GroupBox4: TGroupBox;
    LabelCrossBuildOptions: TLabel;
    rgrpSelectCPU: TRadioGroup;
    rgrpSelectSubarch: TRadioGroup;
    RadioGroupARMArch: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure rgrpSelectCPUSelectionChanged(Sender: TObject);
  private

  public

  end;

var
  SubarchForm: TSubarchForm;

implementation

{$R *.lfm}

uses
  fpcuputil,
  m_crossinstaller,
  installerCore;

{ TSubarchForm }

procedure TSubarchForm.FormCreate(Sender: TObject);
var
  aCPU:TCPU;
  ARMArch:TARMARCH;
begin
  // Fill CPU radiogroup
  for aCPU in SUBARCH_CPU do
    rgrpSelectCPU.Items.Append(GetCPU(aCPU));
  rgrpSelectCPU.ItemIndex:=0;

  // Fill ARM Arch radiogroup
  for ARMArch := Low(TARMARCH) to High(TARMARCH) do
    RadioGroupARMArch.Items.Add(GetEnumNameSimple(TypeInfo(TARMARCH),Ord(ARMArch)));
  RadioGroupARMArch.ItemIndex:=0;
end;

procedure TSubarchForm.rgrpSelectCPUSelectionChanged(Sender: TObject);
var
  aCPU:TCPU;
  aSubarch:TSUBARCH;
  i:integer;
  Subarchs: set of TSUBARCH;
begin
  i:=rgrpSelectCPU.ItemIndex;
  if (i<0) then exit;
  BeginFormUpdate;
  //rgrpSelectSubarch.BeginUpdateBounds;
  try
    rgrpSelectSubarch.Items.Clear;
    aCPU:=GetTCPU(rgrpSelectCPU.Items[i]);
    case aCPU of
      TCPU.arm:Subarchs:=SUBARCH_ARM;
      TCPU.avr:Subarchs:=SUBARCH_AVR;
      TCPU.mipsel:Subarchs:=SUBARCH_MIPSEL;
      TCPU.riscv32:Subarchs:=SUBARCH_RISCV32;
      TCPU.xtensa:Subarchs:=SUBARCH_XTENSA;
    else
      exit;
    end;
    for aSubarch in Subarchs do
      rgrpSelectSubarch.Items.Append(GetSubarch(aSubarch));
    if rgrpSelectSubarch.Items.Count=1 then rgrpSelectSubarch.ItemIndex:=0;
  finally
    //rgrpSelectSubarch.EndUpdateBounds;
    EndFormUpdate;
  end;
end;

end.

