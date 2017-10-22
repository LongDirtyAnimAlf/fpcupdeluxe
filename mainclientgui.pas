unit mainclientgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Menus, SynCommons, mORMot, mORMotUI, SynLog;

{$I mORMotDataModel.inc}

type
  { TForm1 }

  TForm1 = class(TForm)
    btnConnect: TButton;
    btnExecuteSQL: TButton;
    DrawGrid1: TDrawGrid;
    Memo1: TMemo;
    miMarkDelete: TMenuItem;
    mmoQuery: TMemo;
    PopupMenu1: TPopupMenu;
    procedure btnConnectClick(Sender: TObject);
    procedure btnExecuteSQLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miMarkDeleteClick(Sender: TObject);
  private
    aTableToGrid:TSQLTableToGrid;
    FConnected:boolean;
    fTableJSON: RawUTF8;
    aClient: TSQLRestClientURI;
    aModel: TSQLModel;
    fCurrentSelectedRow: integer;
    procedure OnDrawCellBackground(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure OnSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
  public
    property Connected:boolean read FConnected;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  mORMotHttpClient;

{ TForm1 }

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  {$ifndef DELPHI5OROLDER}
  TSynLogTestLog := TSQLLog; // share the same log file with whole mORMot
  {$endif}
  aModel:=TSQLModel.Create([TSQLUp]);
  aClient:=TSQLHttpClient.Create(IP_DEFAULT,InttoStr(PORT_DEFAULT),aModel);
  if aClient.ServerTimeStampSynchronize then
  begin
    FConnected:=aClient.SetUser(USER_DEFAULT,PASS_DEFAULT);
  end;
end;

procedure TForm1.btnExecuteSQLClick(Sender: TObject);
var
  SQL: RawUTF8;
begin
  if (NOT Connected) then exit;

  SQL := trim(StringToUTF8(mmoQuery.Text));
  fTableJSON := aClient.ExecuteJson([TSQLUp],SQL);
  FreeAndNil(aTableToGrid);
  if NOT Assigned(aTableToGrid) then aTableToGrid:=TSQLTableToGrid.Create(DrawGrid1,
     TSQLTableJSON.CreateFromTables([TSQLUp],SQL,pointer(fTableJSON),Length(fTableJSON)),aClient);
  aTableToGrid.HeaderCheckboxSelectsInsteadOfSort := true;
  DrawGrid1.DefaultDrawing := false;
  aTableToGrid.OnDrawCellBackground := @OnDrawCellBackground;
  aTableToGrid.OnSelectCell:=@OnSelectCell;
end;

procedure NewDrawCellBackground(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState;
  {$ifdef USETMSPACK} TabAppearance: TTabAppearance;{$endif} Marked: boolean);
var Grid: TDrawGrid absolute Sender;
begin
  if not Sender.InheritsFrom(TDrawGrid) then
    exit;
  with Grid.Canvas do begin
    Font := Grid.Font;
    if gdFixed in State then begin
      Font.Color := clCaptionText;
      Brush.Color := clGradientInactiveCaption;
      Pen.Color := clGrayText;
      inc(Rect.Bottom,1);
      MoveTo(Rect.Right,Rect.Top);
      LineTo(Rect.Right,Rect.Bottom);
    end else
    if (gdSelected in State) then begin
      Font.Color := clHighlightText;
      Brush.Color := clHighlight;
    end else begin
      Font.Color := clWindowText;
      Brush.Color := clWindow;
    end;
    FillRect(Rect);
  end;
end;

procedure TForm1.OnDrawCellBackground(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (self=nil) then exit;
  NewDrawCellBackground(Sender,ACol,ARow,Rect,State,false);
end;

procedure TForm1.OnSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
var
  aValue:PUTF8Char;
  aText:RawUTF8;
begin
  aValue:=aTableToGrid.Table.Get(ARow,'LogEntry');
  Utf8ToRawUTF8(aValue, aText);
  Memo1.Lines.Text:=aText;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConnected:=false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  aClient.Free;
  aModel.Free;
end;

procedure TForm1.miMarkDeleteClick(Sender: TObject);
begin
  aClient.Delete(TSQLUp,aTableToGrid.SelectedID);
  aTableToGrid.Refresh;
end;

end.

