unit mormotdatamodelclient;

{$mode delphi}

interface

uses
  SysUtils,
  SynCrossPlatformSpecific,
  SynCrossPlatformREST;

{$I mORMotDataModel.inc}

type
  TDataClient = class(TSQLRestClientHTTP)
  private
    FConnected:boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure ClientConnect;
    procedure SendData;
    property Connected:boolean read FConnected;
  end;


implementation

constructor TDataClient.Create;
var
  aModel:TSQLModel;
begin
  FConnected:=false;
  aModel:=TSQLModel.Create([TSQLMachine,TSQLPerson,TSQLLogEntry]);
  inherited Create(IP_DEFAULT,PORT_DEFAULT,aModel,true,false,'','',5000,5000,5000);
end;

destructor TDataClient.Destroy;
begin
  inherited;
end;

procedure TDataClient.ClientConnect;
begin
  Self.Connect;
  if ServerTimeStamp>0 then
  begin
    if SetUser(TSQLRestServerAuthenticationDefault,USER_DEFAULT,PASS_DEFAULT) then
    begin
      FConnected:=true;
    end;
  end;
end;

procedure TDataClient.SendData;
var
  aRecord:TSQLPerson;
begin
  if Connected then
  begin
    aRecord:=TSQLPerson.Create;
    try
      aRecord.LastName:='Yolo';
      Add(aRecord,True);
    finally
      aRecord.Free;
    end;
  end;
end;

end.
