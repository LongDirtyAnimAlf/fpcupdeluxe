unit subarch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TSubarchForm }

  TSubarchForm = class(TForm)
    btnListCustomOptions: TButton;
    btnSelectBinDir: TButton;
    btnSelectCompiler: TButton;
    btnSelectLibDir: TButton;
    ComboBoxCPU: TComboBox;
    ComboBoxOS: TComboBox;
    EditBinLocation: TEdit;
    EditCompilerOverride: TEdit;
    EditCrossBuildOptions: TEdit;
    EditCrossSubArch: TEdit;
    EditLibLocation: TEdit;
    GroupBox4: TGroupBox;
    LabelCompilerOverride: TLabel;
    LabelCPU: TLabel;
    LabelCrossBuildOptions: TLabel;
    LabelCrossSubArch: TLabel;
    LabelOS: TLabel;
    rgrpSelectCPU: TRadioGroup;
    rgrpSelectSubarch: TRadioGroup;
    RadioGroup3: TRadioGroup;
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
  m_crossinstaller;

{ TSubarchForm }

procedure TSubarchForm.FormCreate(Sender: TObject);
var
  aCPU:TCPU;
begin
  for aCPU in SUBARCH_CPU do
    rgrpSelectCPU.Items.Append(GetCPU(aCPU));
  rgrpSelectCPU.ItemIndex:=0;
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
  rgrpSelectSubarch.BeginUpdateBounds;
  try
    rgrpSelectSubarch.Items.Clear;
    aCPU:=TCPU(GetEnumValueSimple(TypeInfo(TCPU),rgrpSelectCPU.Items[i]));
    case aCPU of
      TCPU.arm:Subarchs:=SUBARCH_ARM;
      TCPU.avr:Subarchs:=SUBARCH_AVR;
      TCPU.mipsel:Subarchs:=SUBARCH_MIPSEL;
      TCPU.riscv32:Subarchs:=SUBARCH_RISCV32;
      TCPU.xtensa:Subarchs:=SUBARCH_XTENSA;
    end;
    for aSubarch in Subarchs do
      rgrpSelectSubarch.Items.Append(GetEnumNameSimple(TypeInfo(TSUBARCH),Ord(aSubarch)));
    if rgrpSelectSubarch.Items.Count=1 then rgrpSelectSubarch.ItemIndex:=0;
  finally
    rgrpSelectSubarch.EndUpdateBounds;
  end;
end;

end.

