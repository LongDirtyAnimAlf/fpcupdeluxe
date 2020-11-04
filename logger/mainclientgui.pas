unit mainclientgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Menus, SynCommons, mORMot, mORMotUI;

{$I mORMotDataModel.inc}

type
  { TForm1 }

  TForm1 = class(TForm)
    btnConnect: TButton;
    DrawGrid1: TDrawGrid;
    Memo1: TMemo;
    miMarkDelete: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure btnConnectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miMarkDeleteClick(Sender: TObject);
  private
    FConnected:boolean;
    aClient: TSQLRestClientURI;
    aModel: TSQLModel;
    procedure GetData;
    procedure OnGridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
  public
    property Connected:boolean read FConnected;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  mormotdatamodelnativeclient,
  mORMotHttpClient;

{ TForm1 }

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  FreeAndNil(aClient);
  aClient:=TSQLHttpClient.Create(IP_DEFAULT,InttoStr(PORT_DEFAULT_CLIENT),aModel);
  if aClient.ServerTimeStampSynchronize then
  begin
    FConnected:=aClient.SetUser(USER_DEFAULT,PASS_DEFAULT);
  end;
  if FConnected then GetData;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  aUP:TSQLUp;
begin
  aUP:=TSQLUp.Create;
  aUP.UpVersion:='1.10.1a';
  aClient.Add(aUP,True);
  Memo1.Lines.Append('New: '+InttoStr(aUP.ID));
  aUP.Free;
end;

procedure TForm1.GetData;
var
  aTable: TSQLTableJSON;
  aGridTable: TSQLTableToGrid;
  i:integer;
  Means: array of cardinal;
begin
  if (NOT Connected) then exit;

  //get some data, but limit the fields
  aTable := aClient.MultiFieldValues(TSQLUp,'ID,UpVersion,UpOS,Country,FPCVersion,LazarusVersion,UpFunction,LogEntry');
  TSQLTableToGrid.Create(DrawGrid1, aTable, aClient);

  DrawGrid1.DefaultDrawing := true;

  aGridTable:=TSQLTableToGrid.From(DrawGrid1);
  if NOT Assigned(aGridTable) then exit;
  with aGridTable do
  begin
    if Assigned(Table) AND (Table.FieldCount>0) then
    begin
      IDColumnHide;
      SetLength(Means,Table.FieldCount);
      for i:=Low(Means) to High(Means) do
      begin
        Means[i]:=75;
        if Table.FieldNames[i]='LogEntry' then Means[i]:=200;
      end;
      Means[0]:=50;
      Table.SetFieldLengthMean(Means);
      Finalize(Means);

      FieldTitleTruncatedNotShownAsHint := true;
      OnSelectCell:=@OnGridSelectCell;
      i:=Table.FieldIndex('LogDate');
      SortForce(i,true);
      Refresh(true);
    end;
  end;
  DrawGrid1.Show;
end;

procedure TForm1.OnGridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
var
  aValue:PUTF8Char;
  aText:RawUTF8;
  aGridTable: TSQLTableToGrid;
begin
  aGridTable:=TSQLTableToGrid.From(DrawGrid1);
  if NOT Assigned(aGridTable) then exit;
  aValue:=aGridTable.Table.Get(ARow,'LogEntry');
  Utf8ToRawUTF8(aValue, aText);
  Memo1.Lines.Text:=aText;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConnected:=false;
  aModel:=TSQLModel.Create([TSQLUp]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  aClient.Free;
  aModel.Free;
end;

procedure TForm1.miMarkDeleteClick(Sender: TObject);
var
  aGridTable: TSQLTableToGrid;
begin
  aGridTable:=TSQLTableToGrid.From(DrawGrid1);
  if NOT Assigned(aGridTable) then exit;
  aClient.Delete(TSQLUp,aGridTable.SelectedID);
  aGridTable.Refresh;
end;

end.

