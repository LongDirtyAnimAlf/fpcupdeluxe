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
{$IF DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)}  //iOS
 {$IFDEF NoiPhoneAll}
  Foundation,
 {$ELSE}
  iPhoneAll,
 {$ENDIF}
{$ELSE}  //macOS
 {$IFDEF NoCocoaAll}
  Foundation,
 {$ELSE}
  CocoaAll,
 {$ENDIF}
{$ENDIF}
  NSHelpers;

type
  TNSHTTPSendAndReceive = class(TObject)
  private
    FAddress : string;
    FMethod : string;
    FTimeOut : Integer;
    FLastErrMsg : string;
  public
    property Address : string read FAddress write FAddress;
    property Method : string read FMethod write FMethod;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property LastErrMsg : string read FLastErrMsg;
    constructor Create;
    function SendAndReceive(ARequest  : TStream;
                            AResponse : TStream; 
                            Headers   : TStringList) : Boolean; overload;
    function SendAndReceive(out AResponse : string) : Boolean; overload;
    function PostForm(const FormFields : string;
                        out AResponse  : string) : Boolean; overload;
  end;


implementation

constructor TNSHTTPSendAndReceive.Create;
begin
  inherited Create;
  FMethod := 'GET';
  FTimeOut := 30;
end;

function TNSHTTPSendAndReceive.SendAndReceive(ARequest  : TStream;
                                              AResponse : TStream; 
                                              Headers   : TStringList) : Boolean;
 {Send HTTP request to current Address URL, returning downloaded data 
   in AResponse stream and True as function result. If error occurs, 
   return False and set LastErrMsg.
  Optional ARequest stream can be used to set the HTTP request body.
  Optional Headers list of name-value pairs can be used to set 
   HTTP headers.}
var
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : Integer;
  urlResponse : NSURLResponse;
  error       : NSError;
  urlData     : NSData;
begin
  Result := False;
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

    if Assigned(Headers) then
      begin
      for HdrNum := 0 to Headers.Count-1 do
        begin
        urlRequest.addValue_forHTTPHeaderField(StrToNSStr(Headers.ValueFromIndex[HdrNum]),
                                               StrToNSStr(Headers.Names[HdrNum]));
        end;
      end;

    urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
                urlRequest, @urlResponse, @error);
    if not Assigned(urlData) then
      begin
      FLastErrMsg := NSStrToStr(error.localizedDescription);
      Exit;
      end;

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


function TNSHTTPSendAndReceive.SendAndReceive(out AResponse : string) : Boolean;
 {Send HTTP request to current Address URL, returning downloaded data 
   in AResponse string and True as function result. If error occurs, 
   return False and set LastErrMsg.}
var
  Data : TMemoryStream;
begin
  Data := TMemoryStream.Create;
  try
    Result := SendAndReceive(nil, Data, nil);
    if Result then
      begin
      SetLength(AResponse, Data.Size);
      if Data.Size > 0 then
        Data.Read(AResponse[1], Data.Size);
      end;
  finally
    Data.Free;
  end;
end;


function TNSHTTPSendAndReceive.PostForm(const FormFields : string;
                                          out AResponse  : string) : Boolean;
 {Post FormFields to current Address URL, returning downloaded data 
   in AResponse string and True as function result. If error occurs, 
   return False and set LastErrMsg.
  Note FormFields must be in URL query string form (for example,
   'name1=value1&name2=value2') and URL encoded.}
var
  Request : TMemoryStream;
  Headers : TStringList;
  Data    : TMemoryStream;
begin
  Request := TMemoryStream.Create;
  Headers := TStringList.Create;
  Data := TMemoryStream.Create;
  try
    FMethod := 'POST';
    if FormFields <> '' then
      Request.Write(FormFields[1], Length(FormFields));
    Headers.Add('Content-Type=application/x-www-form-urlencoded');
    Headers.Add('Content-Length=' + IntToStr(Request.Size));
    Result := SendAndReceive(Request, Data, Headers);
    if Result then
      begin
      SetLength(AResponse, Data.Size);
      if Data.Size > 0 then
        Data.Read(AResponse[1], Data.Size);
      end;
  finally
    Request.Free;
    Headers.Free;
    Data.Free;
  end;
end;


end.
