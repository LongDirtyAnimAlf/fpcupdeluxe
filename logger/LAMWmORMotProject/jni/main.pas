{Hint: save all files to location: C:\Users\Alfred\Documents\GitHub\fpcupdeluxe\androidclient\LAMWmORMotProject\jni }
unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls,
  SynCommons, mORMot, mORMotHttpClient,
  mormotdatamodelnativeclient;
  
type

  { TAndroidModule1 }

  TAndroidModule1 = class(jForm)
    jButton1: jButton;
    jEditText1: jEditText;
    jListView1: jListView;
    jScrollView1: jScrollView;
    procedure AndroidModule1Create(Sender: TObject);
    procedure AndroidModule1Destroy(Sender: TObject);
    procedure AndroidModule1JNIPrompt(Sender: TObject);
    procedure jButton1Click(Sender: TObject);
    procedure jListView1ClickItem(Sender: TObject; itemIndex: integer;
      itemCaption: string);
  private
    {private declarations}
    aModel:TSQLModel;
    aClient:TSQLHttpClient;
    FConnected:boolean;
    aDataRecord:TSQLUp;
    procedure GetRowData(aRow: integer);
    procedure ShowData;
  public
    {public declarations}
  end;

var
  AndroidModule1: TAndroidModule1;

implementation
  
{$R *.lfm}

{ TAndroidModule1 }

procedure TAndroidModule1.jButton1Click(Sender: TObject);
begin
  FConnected:=false;
  FreeAndNil(aClient);
  //aClient:=TSQLHttpClient.Create(IP_DEFAULT,InttoStr(PORT_DEFAULT),aModel,5000,5000,5000);
  aClient:=TSQLHttpClient.Create(IP_DEFAULT,InttoStr(PORT_DEFAULT),aModel);
  if aClient.ServerTimeStampSynchronize then
  begin
    ShowMessage('Connected with server');
    FConnected:=aClient.SetUser(USER_DEFAULT,PASS_DEFAULT);
    if FConnected
       then ShowMessage('Authenticated')
       else ShowMessage('Authentication failure');
  end else ShowMessage('Connection failure');
  ShowData;
end;

procedure TAndroidModule1.jListView1ClickItem(Sender: TObject;
  itemIndex: integer; itemCaption: string);
begin
  GetRowData(itemIndex+1);
end;

procedure TAndroidModule1.GetRowData(aRow: integer);
begin
  jEditText1.DispatchOnChangedEvent(False);
  jScrollView1.SmoothScrollTo(0,0);
  if (aRow=0) OR (aRow>aDataRecord.FillTable.RowCount) then
  begin
    jEditText1.Text:='';
  end
  else
  begin
    aDataRecord.FillRow(aRow);
    jEditText1.Text:=aDataRecord.LogEntry;
  end;
  jEditText1.DispatchOnChangedEvent(True);
end;


procedure TAndroidModule1.AndroidModule1Create(Sender: TObject);
begin
  FConnected:=false;
  aModel:=TSQLModel.Create([TSQLUp]);
  aDataRecord:=TSQLUp.Create;
end;

procedure TAndroidModule1.AndroidModule1Destroy(Sender: TObject);
begin
  FreeAndNil(aDataRecord);
  FreeAndNil(aClient);
  FreeAndNil(aModel);
end;

procedure TAndroidModule1.AndroidModule1JNIPrompt(Sender: TObject);
begin
  jEditText1.Clear;
end;

procedure TAndroidModule1.ShowData;
var
  aSQL:RawUTF8;
  s:string;
begin
  if FConnected then
  begin
    jListView1.Refresh;
    jListView1.UpdateLayout;
    jListView1.Clear;
    GetRowData(0);

    aSQL:=FormatUTF8('DateOfUse > ?',[],[DateToSQL(Now-5)]); // show only last 5 days

    //get some data, but limit the fields
    aDataRecord.FillPrepare(aClient,aSQL,'UpVersion,UpOS,UpDistro,Country,FPCVersion,LogEntry');

    with aDataRecord do
    begin
      if FillRewind then
      begin
        ShowMessage('Got some data from server');
        GetRowData(1);
        while FillOne do
        begin
          s:=LogEntry;
          if s<>'Success !' then s:='Install failure: click for details.';
          jListView1.Add(
          UpVersion+
          '('+
          UpOS+
          '|'+
          UpDistro+
          '|'+
          Country+
          '|'+
        FPCVersion+
        '|'+
        s
        ,'|');
        end;
      end;
    end;
  end;
end;

end.
