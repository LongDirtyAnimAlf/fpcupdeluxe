unit extrasettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons;

type

  { TForm2 }

  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    HTTPProxyHost: TEdit;
    HTTPProxyPort: TEdit;
    HTTPProxyUser: TEdit;
    HTTPProxyPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

end.

