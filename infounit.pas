unit infounit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    Memo1: TMemo;
  private

  public

  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

end.

