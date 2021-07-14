unit ns_url_request;

{
  TNSHTTPSendAndReceive class for use by itself as an HTTP client or with 
   Web Service Toolkit (http://wiki.freepascal.org/Web_Service_Toolkit).

  Author:    Phil Hess.
  Copyright: Copyright 2011 Phil Hess.
  License:   Modified LGPL (see Free Pascal's rtl/COPYING.FPC). 
             This means you can link your code to this compiled unit (statically 
             in a standalone executable or dynamically in a library) without 
             releasing your code. Only changes to this unit need to be made 
             publicly available.
}

{$modeswitch ObjectiveC1}

interface

uses 
  SysUtils,
  Classes,
  httpdefs,
  CocoaAll,
  NSHelpers;

type
  // to be done
  TProxyData = Class (TPersistent)
  private
    FHost: string;
    FPassword: String;
    FPort: Word;
    FUserName: String;
  Public
    Property Host: string Read FHost Write FHost;
    Property Port: Word Read FPort Write FPort;
    Property UserName : String Read FUserName Write FUserName;
    Property Password : String Read FPassword Write FPassword;
  end;

  TCustomNSHTTPSendAndReceive = class(TObject)
  private
    FAddress : string;
    FMethod : string;
    FRequestHeaders: TStringList;
    FResponseHeaders: TStringList;    // to be done
    FUserAgent : string;
    FContentType : string;
    FUserName : string;               // to be done
    FPassword : string;               // to be done
    FProxy : TProxyData;              // to be done
    FTimeOut : Integer;
    FLastErrMsg : string;
    FResponseStatusCode : NSInteger;
    procedure SetRequestHeaders(const AValue: TStringList);
    function GetProxy: TProxyData;
    procedure SetProxy(AValue: TProxyData);
    function CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean;
    procedure SetUserAgent(const AValue: string);
    procedure SetContentType(const AValue: string);
  protected
    property RequestHeaders : TStringList Read FRequestHeaders Write SetRequestHeaders;
    property ResponseHeaders : TStringList Read FResponseHeaders;
    property UserAgent: String Write SetUserAgent;
    property ContentType: String Write SetContentType;
    property UserName : String Read FUserName Write FUserName;
    property Password : String Read FPassword Write FPassword;
    property Proxy : TProxyData Read GetProxy Write SetProxy;
  public
    property Address : string read FAddress write FAddress;
    property Method : string read FMethod write FMethod;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property LastErrMsg : string read FLastErrMsg;
    property ResponseStatusCode: NSInteger read FResponseStatusCode;
    constructor Create;
    destructor Destroy;override;
    procedure AddHeader(const AHeader, AValue: String);
    function IndexOfHeader(const AHeader: String): Integer;
    function GetHeader(const AHeader: String): String;
    function SendAndReceive(ARequest  : TStream; AResponse : TStream; aMethod:string='GET') : Boolean;
    procedure HTTPMethod(const AMethod, AURL: String; {%H-}Stream: TStream; const {%H-}AllowedResponseCodes: array of Integer);
    function Get(const AURL: String): String;
    procedure Get(const AURL: String; const LocalFileName: String);
    procedure Get(const AURL: String; Stream: TStream);
  end;

  TNSHTTPSendAndReceive = Class(TCustomNSHTTPSendAndReceive)
  published
    property RequestHeaders;
    property ResponseHeaders;
    property UserAgent;
    property ContentType;
    property UserName;
    property Password;
    property Proxy;
  end;

  EHTTPClient = Class(EHTTP);

implementation

uses
  fphttpclient,
  fpcuputil;

constructor TCustomNSHTTPSendAndReceive.Create;
begin
  inherited Create;
  FTimeOut := 30;
  FUserAgent:='';
  FRequestHeaders := TStringList.Create;
  FResponseHeaders := TStringList.Create;
  FResponseStatusCode := 0;
  UserAgent:='Mozilla/5.0 (compatible; fpweb)';
end;

destructor TCustomNSHTTPSendAndReceive.Destroy;
begin
  FreeAndNil(FProxy);
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FResponseHeaders);
  inherited Destroy;
end;

procedure TCustomNSHTTPSendAndReceive.SetRequestHeaders(const AValue: TStringList);
begin
  if FRequestHeaders=AValue then exit;
  FRequestHeaders.Assign(AValue);
end;

procedure TCustomNSHTTPSendAndReceive.SetContentType(const AValue: string);
const
  HEADERMAGIC='Content-Type';
begin
  if AValue<>FContentType then
  begin
    FContentType:=AValue;
    AddHeader(HEADERMAGIC,FContentType);
  end
end;

procedure TCustomNSHTTPSendAndReceive.SetUserAgent(const AValue: string);
const
  HEADERMAGIC='User-Agent';
begin
  if AValue<>FUserAgent then
  begin
    FUserAgent:=AValue;
    AddHeader(HEADERMAGIC,FUserAgent);
  end
end;

function TCustomNSHTTPSendAndReceive.GetProxy: TProxyData;
begin
  If not Assigned(FProxy) then
  begin
    FProxy:=TProxyData.Create;
  end;
  Result:=FProxy;
end;

procedure TCustomNSHTTPSendAndReceive.SetProxy(AValue: TProxyData);
begin
  if (AValue=FProxy) then exit;
  Proxy.Assign(AValue);
end;

function TCustomNSHTTPSendAndReceive.CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean;
var
  I : Integer;
begin
  Result:=(High(AllowedResponseCodes)=-1);
  if not Result then
  begin
    I:=Low(AllowedResponseCodes);
    While (Not Result) and (I<=High(AllowedResponseCodes)) do
    begin
      Result:=(AllowedResponseCodes[i]=ACode);
      Inc(I);
    end
  end;
  {
  If (Not Result) then
  begin
    if AllowRedirect then
      Result:=IsRedirect(ACode);
    If (ACode=401) then
      Result:=Assigned(FOnPassword);
  end;
  }
end;

procedure TCustomNSHTTPSendAndReceive.AddHeader(const AHeader, AValue: String);
var
  J: Integer;
begin
  j:=IndexOfHeader(AHeader);
  if (J<>-1) then
    FRequestHeaders.Delete(j);
  if Length(AValue)>0 then
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
  FRequestHeaders.AddPair(AHeader,AValue);
  {$ELSE}
  FRequestHeaders.Add(AHeader+'='+AValue);
  {$ENDIF}
end;

function TCustomNSHTTPSendAndReceive.IndexOfHeader(const AHeader: String): Integer;
var
  //LH : Integer;
  H : String;
begin
  H:=LowerCase(AHeader);
  //LH:=Length(AHeader);
  Result:=FRequestHeaders.Count-1;
  while (Result>=0) and (LowerCase(FRequestHeaders.Names[Result])<>H) do
    Dec(Result);
end;

function TCustomNSHTTPSendAndReceive.GetHeader(const AHeader: String): String;
var
  I : Integer;
begin
  I:=IndexOfHeader(AHeader);
  if (I=-1) then
    Result:=''
  else
    Result:=FRequestHeaders.ValueFromIndex[I];
end;

procedure TCustomNSHTTPSendAndReceive.HTTPMethod(const AMethod, AURL: String;
  Stream: TStream; const AllowedResponseCodes: array of Integer);
begin
  FMethod := AMethod;
  Address := AURL;
  SendAndReceive(nil,Stream);
  if not CheckResponseCode(ResponseStatusCode,AllowedResponseCodes) then
  begin
    Raise EHTTPClient.CreateFmt('Unexpected response status code: %d',[ResponseStatusCode]);
  end;
end;

function TCustomNSHTTPSendAndReceive.SendAndReceive(ARequest : TStream; AResponse : TStream; aMethod : string = 'GET') : Boolean;
 {Send HTTP request to current Address URL, returning downloaded data 
   in AResponse stream and True as function result. If error occurs, 
   return False and set LastErrMsg.
  Optional ARequest stream can be used to set the HTTP request body.}
var
  FPCClient   : TFPHTTPClient;
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : Integer;
  urlResponse : NSHTTPURLResponse;
  error       : NSError;
  urlData     : NSData;
  s           : string;
begin
  Result := False;

  FMethod := aMethod;

  s:=GetDarwinSDKVersion('macosx');

  //if (true) then
  if (false) then
  //if (Length(s)<>0) AND (CompareVersionStrings('10.12',s)>=0) then
  begin

    FPCClient := TFPHttpClient.Create(nil);
    try
      FPCClient.AllowRedirect := true;
      FPCClient.RequestHeaders:=RequestHeaders;
      FPCClient.HTTPMethod(Method,Address,AResponse,[]);
    finally
      FPCClient.Free;
    end;

  end
  else
  begin

    try
      urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
                     NSURL.URLWithString(StrToNSStr(Address)),
                     NSURLRequestUseProtocolCachePolicy, Timeout);

      if Method <> '' then
        urlRequest.setHTTPMethod(StrToNSStr(Method));

      if Assigned(ARequest) and (ARequest.Size > 0) then
      begin
        try
          requestData := NSMutableData.alloc.initWithLength(ARequest.Size);
          ARequest.Position := 0;
          ARequest.ReadBuffer(requestData.mutableBytes^, ARequest.Size);
          urlRequest.setHTTPBody(requestData);
        finally
          requestData.release;
        end;
      end;

      if Assigned(RequestHeaders) then
      begin
        for HdrNum := 0 to RequestHeaders.Count-1 do
        begin
          urlRequest.addValue_forHTTPHeaderField(StrToNSStr(RequestHeaders.ValueFromIndex[HdrNum]),
                                                 StrToNSStr(RequestHeaders.Names[HdrNum]));
        end;
      end;

      urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
                  urlRequest, @urlResponse, @error);
      if not Assigned(urlData) then
      begin
        FLastErrMsg := NSStrToStr(error.localizedDescription);
        Exit;
      end;

      FResponseStatusCode:=urlResponse.statusCode;

      AResponse.Position := 0;
      AResponse.WriteBuffer(urlData.bytes^, urlData.length);
      AResponse.Position := 0;
      Result := True;

    except
      on E : Exception do
      begin
        FLastErrMsg := E.Message;
      end;
    end;

  end;

end;

function TCustomNSHTTPSendAndReceive.Get(const AURL: String): String;
var
  SS : TStringStream;
begin
  SS:=TStringStream.Create('');
  try
    HTTPMethod('GET', AURL, SS, [200]);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

procedure TCustomNSHTTPSendAndReceive.Get(const AURL: String; const LocalFileName: String);
var
  F : TFileStream;
begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    HTTPMethod('GET', AURL, F, [200]);
  finally
    F.Free;
  end;
end;

procedure TCustomNSHTTPSendAndReceive.Get(const AURL: String; Stream: TStream);
begin
  HTTPMethod('GET', AURL, Stream, [200]);
end;


end.
