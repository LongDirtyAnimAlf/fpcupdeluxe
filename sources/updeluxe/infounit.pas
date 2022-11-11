unit infounit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Memo1: TMemo;
  private

  public

  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

end.

