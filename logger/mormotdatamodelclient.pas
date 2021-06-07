unit mormotdatamodelclient;

{$define SynCrossPlatform}

{$ifdef SynCrossPlatform}
{$mode delphi}
{$else}
{$I synopse.inc}
{$endif}


interface

uses
  SysUtils,
  {$ifdef SynCrossPlatform}
  SynCrossPlatformJSON,
  SynCrossPlatformREST
  {$else}
  SynCommons,
  mORMot,
  mORMotHttpClient
  {$endif}
  ;

{$I mORMotDataModel.inc}

type
  TSQLUp = class(TSQLUpBase);
  {$ifdef SynCrossPlatform}
  TDataClient = class(TSQLRestClientHTTP)
  {$else}
  TDataClient = class(TSQLHttpClient)
  {$endif}
  private
    FConnected: boolean;
    FLocReady:  boolean;
    FUpInfo:    TSQLUp;
    FEnabled:   boolean;
    procedure   ClientConnect;
    procedure   ClientGetInfo;
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;
    function    SendData:TID;
    procedure   ClearExtraData;
    procedure   AddExtraData(aName,aValue:string);
    property    Connected:boolean read FConnected;
    property    LocationReady:boolean read FLocReady;
    property    UpInfo:TSQLUp read FUpInfo;
    property    Enabled:boolean write FEnabled;
  end;

implementation

uses
  dateutils,variants,fphttpclient, fpjson;

constructor TDataClient.Create;
var
  aModel:TSQLModel;
begin
  FConnected:=false;
  FLocReady:=false;
  FEnabled:=false;

  FUpInfo:=TSQLUp.Create;
  FUpInfo.IPV4Address:='';
  {%H-}VarClear(FUpInfo.FExtraData);

  aModel:=TSQLModel.Create([TSQLUp]);

  {$ifdef SynCrossPlatform}
  inherited Create(IP_DEFAULT,PORT_DEFAULT_CLIENT,aModel,true,false,'','',5000,5000,5000);
  {$else}
  inherited Create(IP_DEFAULT,InttoStr(PORT_DEFAULT_CLIENT),aModel);
  Model.Owner:=Self;
  {$endif}
end;

destructor TDataClient.Destroy;
begin
  FConnected:=false;
  FUpInfo.Free;
  inherited;
end;

procedure TDataClient.ClientConnect;
begin
  if (NOT Connected) then
  begin
    {$ifdef SynCrossPlatform}
    Self.Connect;
    if (ServerTimeStamp>0) then
    {$else}
    if ServerTimeStampSynchronize then
    {$endif}
    begin
      if SetUser({$ifdef SynCrossPlatform}TSQLRestServerAuthenticationDefault,{$endif}USER_DEFAULT,PASS_DEFAULT) then
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
  LocationParams: TJSONData;
  success:boolean;
begin
  if (Length(FUpInfo.IPV4Address)=0) then
  begin
    HTTPClient:=TFPHTTPClient.Create(nil);
    try
      HTTPClient.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
      HTTPClient.IOTimeout:=1000;
      try
        RawData:=HTTPClient.Get('http://ip-api.com/json');
        success:=((Length(RawData)>0) AND (Pos('Not Found',RawData)=0));
        LocationParams:=nil;
        if success then
        begin
          try
            LocationParams:=GetJSON(RawData);
          except
            LocationParams:=nil;
          end;
        end;
        success:=Assigned(LocationParams);
        if success then
        begin
          try
            RawData:=LocationParams.FindPath('status').AsString;
            if RawData='success' then
            begin
              FUpInfo.IPV4Address:=LocationParams.FindPath('query').AsString;
              FUpInfo.City:=LocationParams.FindPath('city').AsString;
              FUpInfo.Country:=LocationParams.FindPath('country').AsString;
              FUpInfo.Latitude:=LocationParams.FindPath('lat').AsString;
              FUpInfo.Longitude:=LocationParams.FindPath('lon').AsString;
              FLocReady:=true;
            end;
          finally
            LocationParams.Free;
          end;
        end;
      except
        //Swallow exceptions
      end;
    finally
      HTTPClient.Free;
    end;
  end;
end;


procedure TDataClient.AddExtraData(aName,aValue:string);
begin
  try
    if VarIsClear(FUpInfo.ExtraData)
       {$ifdef SynCrossPlatform}
       then FUpInfo.ExtraData := JSONVariant('{'+StringToJSON(aName)+':'+StringToJSON(aValue)+'}')
       else TJSONVariantData(FUpInfo.ExtraData)[aName]:=aValue;
       {$else}
       //then FUpInfo.ExtraData := _JSON('{"'+aName+'":"'+aValue+'"}')
       then FUpInfo.ExtraData := _Obj([aName,aValue])
       else TDocVariantData(FUpInfo.ExtraData).U[aName]:=aValue;
       {$endif}
  except
    //Swallow exceptions
  end;
end;

procedure TDataClient.ClearExtraData;
begin
  {%H-}VarClear(FUpInfo.FExtraData);
end;

function TDataClient.SendData:TID;
begin
  result:=0;
  if NOT FEnabled then exit;
  ClientConnect;
  if Connected then
  begin
    if (NOT LocationReady) then ClientGetInfo;
    FUpInfo.DateOfUse:=LocalTimeToUniversal(Now);
    result:=Add(FUpInfo,True);
  end;
end;

end.
