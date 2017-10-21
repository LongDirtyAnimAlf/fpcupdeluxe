unit mormotdatamodelclient;

{$mode delphi}

interface

uses
  SysUtils,
  SynCrossPlatformJSON,
  SynCrossPlatformREST;

{$I mORMotDataModel.inc}

type
  TDataClient = class(TSQLRestClientHTTP)
  private
    FConnected:boolean;
    FUpInfo:TSQLUp;
    FEnabled:boolean;
    procedure ClientConnect;
    procedure ClientGetInfo;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure SendData;
    procedure ClearExtraData;
    procedure AddExtraData(aName,aValue:string);
    property Connected:boolean read FConnected;
    property UpInfo:TSQLUp read FUpInfo;
    property Enabled:boolean write FEnabled;
  end;


implementation

uses
  variants,fphttpclient;

constructor TDataClient.Create;
var
  aModel:TSQLModel;
begin
  FConnected:=false;
  FEnabled:=false;
  FUpInfo:=TSQLUp.Create;
  FUpInfo.IPV4Address:='';
  VarClear(FUpInfo.FExtraData);
  aModel:=TSQLModel.Create([TSQLUp]);
  inherited Create(IP_DEFAULT,PORT_DEFAULT,aModel,true,false,'','',5000,5000,5000);
end;

destructor TDataClient.Destroy;
begin
  FUpInfo.Free;
  inherited;
end;

procedure TDataClient.ClientConnect;
begin
  if NOT Connected then
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
end;

procedure TDataClient.ClientGetInfo;
var
  HTTPClient: TFPHTTPClient;
  RawData: string;
  doc:variant;
begin
  if (Length(FUpInfo.IPV4Address)=0) then
  begin
    HTTPClient:=TFPHTTPClient.Create(nil);
    try
      HTTPClient.IOTimeout:=1000;
      try
        RawData:=HTTPClient.Get('http://ip-api.com/json');
        doc := JSONVariant(RawData);
        RawData:=doc.status;
        if RawData='success' then
        begin
          FUpInfo.IPV4Address:=doc.query;
          FUpInfo.City:=doc.city;
          FUpInfo.Country:=doc.country;
          FUpInfo.Latitude:=doc.lat;
          FUpInfo.Longitude:=doc.lon;
        end;
      except
      end;
    finally
      HTTPClient.Free;
    end;
  end;
end;


procedure TDataClient.AddExtraData(aName,aValue:string);
begin
  if VarIsClear(FUpInfo.ExtraData)
     then FUpInfo.ExtraData := JSONVariant('{'+StringToJSON(aName)+':'+StringToJSON(aValue)+'}')
     else TJSONVariantData(FUpInfo.ExtraData)[aName]:=aValue;
end;

procedure TDataClient.ClearExtraData;
begin
  VarClear(FUpInfo.FExtraData);
end;

procedure TDataClient.SendData;
begin
  if NOT FEnabled then exit;
  ClientConnect;
  if Connected then
  begin
    ClientGetInfo;
    FUpInfo.DateOfUse:=Now;
    Add(FUpInfo,True);
  end;
end;

end.
