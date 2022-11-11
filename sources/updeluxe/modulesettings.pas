unit modulesettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TForm3 }

  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListView1: TListView;
    RadioGroup1: TRadioGroup;
    procedure ModuleActionClick(Sender: TObject);
    procedure Button3Click({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
  private
    FModuleName:string;
  public
    property ModuleName:string read FModuleName;
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

uses
  IniFiles,
  installerUniversal,
  fpcuputil;

{ TForm3 }

procedure TForm3.ModuleActionClick(Sender: TObject);
var
  aResult:TModalResult;
begin
  aResult:=mrCancel;
  if Assigned(ListView1.Selected) then
  begin
    FModuleName:=ListView1.Selected.Caption;
    if Sender=Button1 then aResult:=mrYes;
    if Sender=Button2 then aResult:=mrNo;
  end;
  Self.ModalResult:=aResult;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  Self.ModalResult:=mrCancel;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  Item:TListItem;
begin
  FModuleName:='unknown';

  Item := ListView1.Items.Add;
  Item.Caption := 'Name';
  Item.Subitems.Add('Surname');
  Item.Subitems.Add('Birthfghfghdfgdfghdghdhggfhdfhdhgday');

  with TIniFile.Create(SafeGetApplicationPath+installerUniversal.DELUXEFILENAME) do
  try

  finally
    Free;
  end;
end;

end.

