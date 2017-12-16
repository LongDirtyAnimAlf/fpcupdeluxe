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
    jListView1: jListView;
    procedure AndroidModule1Create(Sender: TObject);
    procedure AndroidModule1Destroy(Sender: TObject);
    procedure jButton1Click(Sender: TObject);
  private
    {private declarations}
    aModel:TSQLModel;
    aClient:TSQLHttpClient;
    FConnected:boolean;
    aDataRecord:TSQLUp;
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

    aSQL:=FormatUTF8('DateOfUse > ?',[],[DateToSQL(Now-5)]); // show only last 5 days

    //get some data, but limit the fields
    aDataRecord.FillPrepare(aClient,aSQL,'UpVersion,UpOS,UpDistro,Country,FPCVersion,LogEntry');

    with aDataRecord do
    begin
      if FillRewind then
      begin
        ShowMessage('Got some data from server');
        while FillOne do
        begin
          s:=LogEntry;
          if s<>'Success !' then s:='Install failure';
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
