{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team

    HTTP client component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fphttpclient;

{ ---------------------------------------------------------------------
  Todo:
  * Proxy support ?
  ---------------------------------------------------------------------}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, httpdefs, uriparser, base64;

Const
  // Socket Read buffer size
  ReadBufLen = 4096;
  // Default for MaxRedirects Request redirection is aborted after this number of redirects.
  DefMaxRedirects = 16;

Type
  TRedirectEvent = Procedure (Sender : TObject; Const ASrc : String; Var ADest: String) of object;
  TPasswordEvent = Procedure (Sender : TObject; Var RepeatRequest : Boolean) of object;
  // During read of headers, ContentLength equals 0.
  // During read of content, of Server did not specify contentlength, -1 is passed.
  // CurrentPos is reset to 0 when the actual content is read, i.e. it is the position in the data, discarding header size.
  TDataEvent   = Procedure (Sender : TObject; Const ContentLength, CurrentPos : Int64) of object;
  // Use this to set up a socket handler. UseSSL is true if protocol was https
  TGetSocketHandlerEvent = Procedure (Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler) of object;
  TSocketHandlerCreatedEvent = Procedure (Sender : TObject; AHandler : TSocketHandler) of object;

  TFPCustomHTTPClient = Class;

  { TProxyData }

  TProxyData = Class (TPersistent)
  private
    FHost: string;
    FPassword: String;
    FPort: Word;
    FUserName: String;
    FHTTPClient : TFPCustomHTTPClient;
  Protected
    Function GetProxyHeaders : String; virtual;
    Function GetOwner: TPersistent; override;
    Property HTTPClient : TFPCustomHTTPClient Read FHTTPClient;
  Public
    Procedure Assign(Source: TPersistent); override;
    Property Host: string Read FHost Write FHost;
    Property Port: Word Read FPort Write FPort;
    Property UserName : String Read FUserName Write FUserName;
    Property Password : String Read FPassword Write FPassword;
  end;

  { TFPCustomHTTPClient }
  TFPCustomHTTPClient = Class(TComponent)
  private
    FDataRead : Int64;
    FContentLength : Int64;
    FAllowRedirect: Boolean;
    FKeepConnection: Boolean;
    FMaxChunkSize: SizeUInt;
    FMaxRedirects: Byte;
    FOnDataReceived: TDataEvent;
    FOnHeaders: TNotifyEvent;
    FOnPassword: TPasswordEvent;
    FOnRedirect: TRedirectEvent;
    FPassword: String;
    FIOTimeout: Integer;
    FConnectTimeout: Integer;
    FSentCookies,
    FCookies: TStrings;
    FHTTPVersion: String;
    FRequestBody: TStream;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
    FResponseStatusCode: Integer;
    FResponseStatusText: String;
    FServerHTTPVersion: String;
    FSocket : TInetSocket;
    FBuffer : Ansistring;
    FTerminated: Boolean;
    FUserName: String;
    FOnGetSocketHandler : TGetSocketHandlerEvent;
    FAfterSocketHandlerCreated : TSocketHandlerCreatedEvent;
    FProxy : TProxyData;
    function CheckContentLength: Int64;
    function CheckTransferEncoding: string;
    function GetCookies: TStrings;
    function GetProxy: TProxyData;
    Procedure ResetResponse;
    procedure SetConnectTimeout(AValue: Integer);
    Procedure SetCookies(const AValue: TStrings);
    procedure SetHTTPVersion(const AValue: String);
    procedure SetKeepConnection(AValue: Boolean);
    procedure SetProxy(AValue: TProxyData);
    Procedure SetRequestHeaders(const AValue: TStrings);
    procedure SetIOTimeout(AValue: Integer);
    Procedure ExtractHostPort(AURI: TURI; Out AHost: String; Out APort: Word);
    Procedure CheckConnectionCloseHeader;
  protected

    Function NoContentAllowed(ACode : Integer) : Boolean;
    // Peform a request, close connection.
    Procedure DoNormalRequest(const AURI: TURI; const AMethod: string;
      AStream: TStream; const AAllowedResponseCodes: array of Integer;
      AHeadersOnly, AIsHttps: Boolean); virtual;
    // Peform a request, try to keep connection.
    Procedure DoKeepConnectionRequest(const AURI: TURI; const AMethod: string;
      AStream: TStream; const AAllowedResponseCodes: array of Integer;
      AHeadersOnly, AIsHttps: Boolean); virtual;
    // Return True if FSocket is assigned
    Function IsConnected: Boolean; virtual;
    // True if we need to use a proxy: ProxyData Assigned and Hostname Set
    Function ProxyActive : Boolean;
    // Override this if you want to create a custom instance of proxy.
    Function CreateProxyData : TProxyData;
    // Called whenever data is read.
    Procedure DoDataRead; virtual;
    // Parse response status line. Saves status text and protocol, returns numerical code. Exception if invalid line.
    Function ParseStatusLine(AStatusLine : String) : Integer;
    // Construct server URL for use in request line.
    function GetServerURL(URI: TURI): String;
    // Read 1 line of response. Fills FBuffer
    function ReadString(out S: String): Boolean;
    // Check if response code is in AllowedResponseCodes. if not, an exception is raised.
    // If AllowRedirect is true, and the result is a Redirect status code, the result is also true
    // If the OnPassword event is set, then a 401 will also result in True.
    function CheckResponseCode(ACode: Integer;  const AllowedResponseCodes: array of Integer): Boolean; virtual;
    // Read response from server, and write any document to Stream.
    Function ReadResponse(Stream: TStream;  const AllowedResponseCodes: array of Integer; HeadersOnly: Boolean = False): Boolean; virtual;
    // Read server response line and headers. Returns status code.
    Function ReadResponseHeaders : integer; virtual;
    // Allow header in request ? (currently checks only if non-empty and contains : token)
    function AllowHeader(var AHeader: String): Boolean; virtual;
    // Return True if the "connection: close" header is present
    Function HasConnectionClose: Boolean; virtual;
    // Connect to the server. Must initialize FSocket.
    Procedure ConnectToServer(const AHost: String; APort: Integer; UseSSL : Boolean=False); virtual;
    // Re-connect to the server. Must reinitialize FSocket.
    Procedure ReconnectToServer(const AHost: String; APort: Integer; UseSSL : Boolean=False); virtual;
    // Disconnect from server. Must free FSocket.
    Procedure DisconnectFromServer; virtual;
    // Run method AMethod, using request URL AURL. Write Response to Stream, and headers in ResponseHeaders.
    // If non-empty, AllowedResponseCodes contains an array of response codes considered valid responses.
    // If HandleRedirect is True, then Redirect status is accepted as a correct status, but request is not repeated.
    // No authorization callback.
    Procedure DoMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual;
    // Send request to server: construct request line and send headers and request body.
    Procedure SendRequest(const AMethod: String; URI: TURI); virtual;
    // Create socket handler for protocol AProtocol. Calls OnGetSocketHandler.
    Function GetSocketHandler(Const UseSSL : Boolean) : TSocketHandler;  virtual;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    // Add header Aheader with value AValue to HTTPHeaders, replacing exiting values
    Class Procedure AddHeader(HTTPHeaders : TStrings; Const AHeader,AValue : String);
    // Index of header AHeader in httpheaders.
    Class Function IndexOfHeader(HTTPHeaders : TStrings; Const AHeader : String) : Integer;
    // Return value of header AHeader from httpheaders. Returns empty if it doesn't exist yet.
    Class Function GetHeader(HTTPHeaders : TStrings; Const AHeader : String) : String;
    { Terminate the current request.
      It will stop the client from trying to send and/or receive data after the current chunk is sent/received. }
    Procedure Terminate;
    // Request Header management
    // Return index of header, -1 if not present.
    Function IndexOfHeader(Const AHeader : String) : Integer;
    // Add header, replacing an existing one if it exists.
    Procedure AddHeader(Const AHeader,AValue : String);
    // Return header value, empty if not present.
    Function  GetHeader(Const AHeader : String) : String;
    // General-purpose call. Handles redirect and authorization retry (OnPassword).
    Procedure HTTPMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual;
    // Execute GET on server, store result in Stream, File, StringList or string
    Procedure Get(Const AURL : String; Stream : TStream);
    Procedure Get(Const AURL : String; const LocalFileName : String);
    Procedure Get(Const AURL : String; Response : TStrings);
    Function Get(Const AURL : String) : RawByteString;
    // Check if responsecode is a redirect code that this class handles (301,302,303,307,308)
    Class Function IsRedirect(ACode : Integer) : Boolean; virtual;
    // If the code is a redirect, then this method  must return TRUE if the next request should happen with a GET (307/308)
    Class Function RedirectForcesGET(ACode : Integer) : Boolean; virtual;
   // Simple class methods
    Class Procedure SimpleGet(Const AURL : String; Stream : TStream);
    Class Procedure SimpleGet(Const AURL : String; const LocalFileName : String);
    Class Procedure SimpleGet(Const AURL : String; Response : TStrings);
    Class Function SimpleGet(Const AURL : String) : RawByteString;
    // Simple post
    // Post URL, and Requestbody. Return response in Stream, File, TstringList or String;
    Procedure Post(const URL: string; const Response: TStream);
    Procedure Post(const URL: string; Response : TStrings);
    Procedure Post(const URL: string; const LocalFileName: String);
    function Post(const URL: string) : RawByteString;
    // Simple class methods.
    Class Procedure SimplePost(const URL: string; const Response: TStream);
    Class Procedure SimplePost(const URL: string; Response : TStrings);
    Class Procedure SimplePost(const URL: string; const LocalFileName: String);
    Class function SimplePost(const URL: string) : RawByteString;
    // Simple Put
    // Put URL, and Requestbody. Return response in Stream, File, TstringList or String;
    Procedure Put(const URL: string; const Response: TStream);
    Procedure Put(const URL: string; Response : TStrings);
    Procedure Put(const URL: string; const LocalFileName: String);
    function Put(const URL: string) : RawByteString;
    // Simple class methods.
    Class Procedure SimplePut(const URL: string; const Response: TStream);
    Class Procedure SimplePut(const URL: string; Response : TStrings);
    Class Procedure SimplePut(const URL: string; const LocalFileName: String);
    Class function SimplePut(const URL: string) : RawByteString;
    // Simple Delete
    // Delete URL, and Requestbody. Return response in Stream, File, TstringList or String;
    Procedure Delete(const URL: string; const Response: TStream);
    Procedure Delete(const URL: string; Response : TStrings);
    Procedure Delete(const URL: string; const LocalFileName: String);
    function Delete(const URL: string) : RawByteString;
    // Simple class methods.
    Class Procedure SimpleDelete(const URL: string; const Response: TStream);
    Class Procedure SimpleDelete(const URL: string; Response : TStrings);
    Class Procedure SimpleDelete(const URL: string; const LocalFileName: String);
    Class function SimpleDelete(const URL: string) : RawByteString;
    // Simple Options
    // Options from URL, and Requestbody. Return response in Stream, File, TstringList or String;
    Procedure Options(const URL: string; const Response: TStream);
    Procedure Options(const URL: string; Response : TStrings);
    Procedure Options(const URL: string; const LocalFileName: String);
    function Options(const URL: string) : RawByteString;
    // Simple class methods.
    Class Procedure SimpleOptions(const URL: string; const Response: TStream);
    Class Procedure SimpleOptions(const URL: string; Response : TStrings);
    Class Procedure SimpleOptions(const URL: string; const LocalFileName: String);
    Class function SimpleOptions(const URL: string) : RawByteString;
    // Get HEAD
    Class Procedure Head(AURL : String; Headers: TStrings);
    // Post Form data (www-urlencoded).
    // Formdata in string (urlencoded) or TStrings (plain text) format.
    // Form data will be inserted in the requestbody.
    // Return response in Stream, File, TStringList or String;
    Procedure FormPost(const URL : String; FormData: RawByteString; const Response: TStream);
    Procedure FormPost(const URL : string; FormData:  TStrings; const Response: TStream);
    Procedure FormPost(const URL, FormData: string; const Response: TStrings);
    Procedure FormPost(const URL : string; FormData:  TStrings; const Response: TStrings);
    function FormPost(const URL : String; Const FormData: RawByteString): RawByteString;
    function FormPost(const URL: string; FormData : TStrings): RawByteString;
    // Simple form 
    Class Procedure SimpleFormPost(const URL : String; Const FormData: RawByteString; const Response: TStream);
    Class Procedure SimpleFormPost(const URL : string; FormData:  TStrings; const Response: TStream);
    Class Procedure SimpleFormPost(const URL : String; Const FormData: RawByteString; const Response: TStrings);
    Class Procedure SimpleFormPost(const URL : string; FormData:  TStrings; const Response: TStrings);
    Class function SimpleFormPost(const URL: String; Const FormData: RawByteString): RawByteString;
    Class function SimpleFormPost(const URL: string; FormData : TStrings): RawByteString;
    // Post a file
    Procedure FileFormPost(const AURL, AFieldName, AFileName: string; const Response: TStream);
    // Post form with a file
    Procedure FileFormPost(const AURL: string; FormData: TStrings; AFieldName, AFileName: string; const Response: TStream);
    // Post a stream
    Procedure StreamFormPost(const AURL, AFieldName, AFileName: string; const AStream: TStream; const Response: TStream);
    // Post form with a stream
    Procedure StreamFormPost(const AURL: string; FormData: TStrings; const AFieldName, AFileName: string; const AStream: TStream; const Response: TStream);
    // Simple form of Posting a file
    Class Procedure SimpleFileFormPost(const AURL, AFieldName, AFileName: string; const Response: TStream);
    // Has Terminate been called ?
    Property Terminated : Boolean Read FTerminated;
  Protected
    // Timeouts
    Property IOTimeout : Integer read FIOTimeout write SetIOTimeout;
    Property ConnectTimeout : Integer read FConnectTimeout write SetConnectTimeout;
    // Before request properties.
    // Additional headers for request. Host; and Authentication are automatically added.
    Property RequestHeaders : TStrings Read FRequestHeaders Write SetRequestHeaders;
    // Cookies. Set before request to send cookies to server.
    // After request the property is filled with the cookies sent by the server.
    Property Cookies : TStrings Read GetCookies Write SetCookies;
    // Optional body to send (mainly in POST request)
    Property RequestBody : TStream read FRequestBody Write FRequestBody;
    // used HTTP version when constructing the request.
    // Setting this to any other value than 1.1 will set KeepConnection to False.
    Property HTTPversion : String Read FHTTPVersion Write SetHTTPVersion;
    // After request properties.
    // After request, this contains the headers sent by server.
    Property ResponseHeaders : TStrings Read FResponseHeaders;
    // After request, HTTP version of server reply.
    Property ServerHTTPVersion : String Read FServerHTTPVersion;
    // After request, HTTP response status of the server.
    Property ResponseStatusCode : Integer Read FResponseStatusCode;
    // After request, HTTP response status text of the server.
    Property ResponseStatusText : String Read FResponseStatusText;
    // Allow redirect in HTTPMethod ?
    Property AllowRedirect : Boolean Read FAllowRedirect Write FAllowRedirect;
    // Maximum number of redirects. When this number is reached, an exception is raised.
    Property MaxRedirects : Byte Read FMaxRedirects Write FMaxRedirects default DefMaxRedirects;
    // Maximum chunk size: If chunk sizes bigger than this are encountered, an error will be raised.
    // Set to zero to disable the check.
    Property MaxChunkSize : SizeUInt Read FMaxChunkSize Write FMaxChunkSize;
    // Called On redirect. Dest URL can be edited.
    // If The DEST url is empty on return, the method is aborted (with redirect status).
    Property OnRedirect : TRedirectEvent Read FOnRedirect Write FOnRedirect;
    // Proxy support
    Property Proxy : TProxyData Read GetProxy Write SetProxy;
    // Authentication.
    // When set, they override the credentials found in the URI.
    // They also override any Authenticate: header in Requestheaders.
    Property UserName : String Read FUserName Write FUserName;
    Property Password : String Read FPassword Write FPassword;
    // Is client connected?
    Property Connected: Boolean read IsConnected;
    // Keep-Alive support. Setting to true will set HTTPVersion to 1.1
    Property KeepConnection: Boolean Read FKeepConnection Write SetKeepConnection;
    // If a request returns a 401, then the OnPassword event is fired.
    // It can modify the username/password and set RepeatRequest to true;
    Property OnPassword : TPasswordEvent Read FOnPassword Write FOnPassword;
    // Called whenever data is read from the connection.
    Property OnDataReceived : TDataEvent Read FOnDataReceived Write FOnDataReceived;
    // Called when headers have been processed.
    Property OnHeaders : TNotifyEvent Read FOnHeaders Write FOnHeaders;
    // Called to create socket handler. If not set, or Nil is returned, a standard socket handler is created.
    Property OnGetSocketHandler : TGetSocketHandlerEvent Read FOnGetSocketHandler Write FOnGetSocketHandler;
    // Called after create socket handler was created, with the created socket handler.
    Property AfterSocketHandlerCreate : TSocketHandlerCreatedEvent Read FAfterSocketHandlerCreated Write FAfterSocketHandlerCreated;
  end;


  TFPHTTPClient = Class(TFPCustomHTTPClient)
  Published
    Property KeepConnection;
    Property Connected;
    Property IOTimeout;
    Property ConnectTimeout;
    Property RequestHeaders;
    Property RequestBody;
    Property ResponseHeaders;
    Property HTTPversion;
    Property ServerHTTPVersion;
    Property ResponseStatusCode;
    Property ResponseStatusText;
    Property Cookies;
    Property AllowRedirect;
    Property MaxRedirects;
    Property OnRedirect;
    Property UserName;
    Property Password;
    Property OnPassword;
    Property OnDataReceived;
    Property OnHeaders;
    Property OnGetSocketHandler;
    Property Proxy;
  end;

  EHTTPClient = Class(EHTTP);

Function EncodeURLElement(S : String) : String;
Function DecodeURLElement(Const S : String) : String;

implementation

uses sslsockets;

resourcestring
  SErrInvalidProtocol = 'Invalid protocol : "%s"';
  SErrReadingSocket = 'Error reading data from socket';
  SErrInvalidProtocolVersion = 'Invalid protocol version in response: "%s"';
  SErrInvalidStatusCode = 'Invalid response status code: %s';
  SErrUnexpectedResponse = 'Unexpected response status code: %d';
  SErrChunkTooBig = 'Chunk too big: Got %d, maximum allowed size: %d';
  SErrChunkLineEndMissing = 'Chunk line end missing';
  SErrMaxRedirectsReached = 'Maximum allowed redirects reached : %d';
  //SErrRedirectAborted = 'Redirect aborted.';

Const
  CRLF = #13#10;

function EncodeURLElement(S: String): String;

Const
  NotAllowed = [ ';', '/', '?', ':', '@', '=', '&', '#', '+', '_', '<', '>',
                 '"', '%', '{', '}', '|', '\', '^', '~', '[', ']', '`' ];

var
  i, o, l : Integer;
  h: string[2];
  P : PChar;
  c: AnsiChar;
begin
  result:='';
  l:=Length(S);
  If (l=0) then Exit;
  SetLength(Result,l*3);
  P:=Pchar(Result);
  for I:=1 to L do
    begin
    C:=S[i];
    O:=Ord(c);
    if (O<=$20) or (O>=$7F) or (c in NotAllowed) then
      begin
      P^ := '%';
      Inc(P);
      h := IntToHex(Ord(c), 2);
      p^ := h[1];
      Inc(P);
      p^ := h[2];
      Inc(P);
      end
    else
      begin
      P^ := c;
      Inc(p);
      end;
    end;
  SetLength(Result,P-PChar(Result));
end;

function DecodeURLElement(Const S: AnsiString): AnsiString;

var
  i,l,o : Integer;
  c: AnsiChar;
  p : pchar;
  h : string;

begin
  l := Length(S);
  if l=0 then exit;
  Result:='';
  SetLength(Result, l);
  P:=PChar(Result);
  i:=1;
  While (I<=L) do
    begin
    c := S[i];
    if (c<>'%') then
      begin
      P^:=c;
      Inc(P);
      end
    else if (I<L-1) then
      begin
      H:='$'+Copy(S,I+1,2);
      o:=StrToIntDef(H,-1);
      If (O>=0) and (O<=255) then
        begin
        P^:=char(O);
        Inc(P);
        Inc(I,2);
        end;
      end;
    Inc(i);
  end;
  SetLength(Result, P-Pchar(Result));
end;

{ TProxyData }

function TProxyData.GetProxyHeaders: String;
begin
  Result:='';
  if (UserName<>'') then
    Result:='Proxy-Authorization: Basic ' + EncodeStringBase64(UserName+':'+Password);
end;

function TProxyData.GetOwner: TPersistent;
begin
  Result:=FHTTPClient;
end;

procedure TProxyData.Assign(Source: TPersistent);

Var
  D : TProxyData;

begin
  if Source is TProxyData then
    begin
    D:=Source as TProxyData;
    Host:=D.Host;
    Port:=D.Port;
    UserName:=D.UserName;
    Password:=D.Password;
    end
  else
    inherited Assign(Source);
end;

{ TFPCustomHTTPClient }

procedure TFPCustomHTTPClient.SetRequestHeaders(const AValue: TStrings);
begin
  if FRequestHeaders=AValue then exit;
  FRequestHeaders.Assign(AValue);
end;

procedure TFPCustomHTTPClient.SetIOTimeout(AValue: Integer);
begin
  if AValue=FIOTimeout then exit;
  FIOTimeout:=AValue;
  if Assigned(FSocket) then
    FSocket.IOTimeout:=AValue;
end;

procedure TFPCustomHTTPClient.SetConnectTimeout(AValue: Integer);
begin
  if FConnectTimeout = AValue then Exit;
  FConnectTimeout := AValue;
end;

function TFPCustomHTTPClient.IsConnected: Boolean;
begin
  Result := Assigned(FSocket);
end;

function TFPCustomHTTPClient.NoContentAllowed(ACode: Integer): Boolean;
begin
  Result:=((ACode div 100)=1) or ((ACode=204) or (ACode=304))
end;

function TFPCustomHTTPClient.ProxyActive: Boolean;
begin
  Result:=Assigned(FProxy) and (FProxy.Host<>'') and (FProxy.Port>0);
end;

function TFPCustomHTTPClient.CreateProxyData: TProxyData;
begin
  Result:=TProxyData.Create;
end;

procedure TFPCustomHTTPClient.DoDataRead;
begin
  If Assigned(FOnDataReceived) Then
    FOnDataReceived(Self,FContentLength,FDataRead);
end;

function TFPCustomHTTPClient.IndexOfHeader(const AHeader: String): Integer;
begin
  Result:=IndexOfHeader(RequestHeaders,AHeader);
end;

procedure TFPCustomHTTPClient.AddHeader(const AHeader, AValue: String);

begin
  AddHeader(RequestHeaders,AHeader,AValue);
end;

function TFPCustomHTTPClient.GetHeader(const AHeader: String): String;


begin
  Result:=GetHeader(RequestHeaders,AHeader);
end;

function TFPCustomHTTPClient.GetServerURL(URI: TURI): String;

Var
  D : String;

begin
  D:=URI.Path;
  If Length(D) = 0 then
    D := '/'
  else  If (D[1]<>'/') then
    D:='/'+D;
  If (D[Length(D)]<>'/') then
    D:=D+'/';
  Result:=D+URI.Document;
  if (URI.Params<>'') then
    Result:=Result+'?'+URI.Params;
  if ProxyActive then
    begin
    if URI.Port>0 then
      Result:=':'+IntToStr(URI.Port)+Result;
    Result:=URI.Protocol+'://'+URI.Host+Result;
    end;
end;

function TFPCustomHTTPClient.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;

begin
  Result:=Nil;
  if Assigned(FonGetSocketHandler) then
    FOnGetSocketHandler(Self,UseSSL,Result);
  if (Result=Nil) then
    If UseSSL then
      Result:=TSSLSocketHandler.GetDefaultHandler
    else
      Result:=TSocketHandler.Create;
  if Assigned(AfterSocketHandlerCreate) then
    AfterSocketHandlerCreate(Self,Result);
end;

procedure TFPCustomHTTPClient.ConnectToServer(const AHost: String;
  APort: Integer; UseSSL : Boolean = False);

Var
  G : TSocketHandler;


begin
  If IsConnected Then
    DisconnectFromServer; // avoid memory leaks
  if (Aport=0) then
    if UseSSL then
      Aport:=443
    else
      Aport:=80;
  G:=GetSocketHandler(UseSSL);    
  FSocket:=TInetSocket.Create(AHost,APort,G);
  try
    if FIOTimeout<>0 then
      FSocket.IOTimeout:=FIOTimeout;
    if FConnectTimeout<>0 then
      FSocket.ConnectTimeout:=FConnectTimeout;
    FSocket.Connect;
  except
    FreeAndNil(FSocket);
    Raise;
  end;
end;

Procedure TFPCustomHTTPClient.ReconnectToServer(const AHost: String;
  APort: Integer; UseSSL: Boolean);
begin
  DisconnectFromServer;
  ConnectToServer(AHost, APort, UseSSL);
end;

procedure TFPCustomHTTPClient.DisconnectFromServer;

begin
  FreeAndNil(FSocket);
end;

function TFPCustomHTTPClient.AllowHeader(var AHeader: String): Boolean;

begin
  Result:=(AHeader<>'') and (Pos(':',AHeader)<>0);
end;

Function TFPCustomHTTPClient.HasConnectionClose: Boolean;
begin
  Result := CompareText(GetHeader('Connection'), 'close') = 0;
end;

procedure TFPCustomHTTPClient.SendRequest(const AMethod: String; URI: TURI);

Var
  PH,UN,PW,S,L : String;
  I : Integer;

begin
  S:=Uppercase(AMethod)+' '+GetServerURL(URI)+' '+'HTTP/'+FHTTPVersion+CRLF;
  UN:=URI.Username;
  PW:=URI.Password;
  if (UserName<>'') then
    begin
    UN:=UserName;
    PW:=Password;
    end;
  If (UN<>'') then
    begin
    S:=S+'Authorization: Basic ' + EncodeStringBase64(UN+':'+PW)+CRLF;
    I:=IndexOfHeader('Authorization');
    If I<>-1 then
      RequestHeaders.Delete(i);
    end;
  if Assigned(FProxy) and (FProxy.Host<>'') then
    begin
    PH:=FProxy.GetProxyHeaders;
    if (PH<>'') then
      S:=S+PH+CRLF;
    end;
  S:=S+'Host: '+URI.Host;
  If (URI.Port<>0) then
    S:=S+':'+IntToStr(URI.Port);
  S:=S+CRLF;
  If Assigned(RequestBody) and (IndexOfHeader('Content-Length')=-1) then
    AddHeader('Content-Length',IntToStr(RequestBody.Size));
  CheckConnectionCloseHeader;
  For I:=0 to FRequestHeaders.Count-1 do
    begin
    l:=FRequestHeaders[i];
    If AllowHeader(L) then
      S:=S+L+CRLF;
    end;
  if Assigned(FCookies) then
    begin
    L:='Cookie: ';
    For I:=0 to FCookies.Count-1 do
      begin
      If (I>0) then
        L:=L+'; ';
      L:=L+FCookies[i];
      end;
    if AllowHeader(L) then
      S:=S+L+CRLF;
    end;
  FreeAndNil(FSentCookies);
  FSentCookies:=FCookies;
  FCookies:=Nil;
  S:=S+CRLF;
  if not Terminated then
    FSocket.WriteBuffer(S[1],Length(S));
  If Assigned(FRequestBody) and not Terminated then
    FSocket.CopyFrom(FRequestBody,FRequestBody.Size);
end;

function TFPCustomHTTPClient.ReadString(out S: String): Boolean;

  Function FillBuffer: Boolean;

  Var
    R : Integer;

  begin
    if Terminated then
      Exit(False);
    SetLength(FBuffer,ReadBufLen);
    r:=FSocket.Read(FBuffer[1],ReadBufLen);
    If (r=0) or Terminated Then
      Exit(False);
    If (r<0) then
      Raise EHTTPClient.Create(SErrReadingSocket);
    if (r<ReadBuflen) then
      SetLength(FBuffer,r);
    FDataRead:=FDataRead+R;
    DoDataRead;
    Result:=r>0;
  end;

Var
  CheckLF: Boolean;
  P,L : integer;

begin
  S:='';
  Result:=False;
  CheckLF:=False;
  Repeat
    if Length(FBuffer)=0 then
      if not FillBuffer then
        Break;
    if Length(FBuffer)=0 then
      Result:=True
    else if CheckLF then
      begin
      If (FBuffer[1]<>#10) then
        S:=S+#13
      else
        begin
        System.Delete(FBuffer,1,1);
        Result:=True;
        end;
      end;
    if not Result then
      begin
      P:=Pos(#13#10,FBuffer);
      If P=0 then
        begin
        L:=Length(FBuffer);
        CheckLF:=FBuffer[L]=#13;
        if CheckLF then
          S:=S+Copy(FBuffer,1,L-1)
        else
          S:=S+FBuffer;
        FBuffer:='';
        end
      else
        begin
        S:=S+Copy(FBuffer,1,P-1);
        System.Delete(FBuffer,1,P+1);
        Result:=True;
        end;
      end;
  until Result or Terminated;
end;

Function GetNextWord(Var S : String) : string;

Const
  WhiteSpace = [' ',#9];

Var
  P : Integer;

begin
  While (Length(S)>0) and (S[1] in WhiteSpace) do
    Delete(S,1,1);
  P:=Pos(' ',S);
  If (P=0) then
   P:=Pos(#9,S);
  If (P=0) then
    P:=Length(S)+1;
  Result:=Copy(S,1,P-1);
  Delete(S,1,P);
end;

function TFPCustomHTTPClient.ParseStatusLine(AStatusLine: String): Integer;

Var
  S : String;

begin
  S:=Uppercase(GetNextWord(AStatusLine));
  If (Copy(S,1,5)<>'HTTP/') then
    Raise EHTTPClient.CreateFmt(SErrInvalidProtocolVersion,[S]);
  System.Delete(S,1,5);
  FServerHTTPVersion:=S;
  S:=GetNextWord(AStatusLine);
  Result:=StrToIntDef(S,-1);
  if Result=-1 then
   Raise EHTTPClient.CreateFmt(SErrInvalidStatusCode,[S]);
  FResponseStatusText:=AStatusLine;
end;

function TFPCustomHTTPClient.ReadResponseHeaders: integer;

  Procedure DoCookies(S : String);

  Var
    P : Integer;
    C : String;

  begin
    P:=Pos(':',S);
    System.Delete(S,1,P);
    Repeat
      P:=Pos(';',S);
      If (P=0) then
        P:=Length(S)+1;
      C:=Trim(Copy(S,1,P-1));
      Cookies.Add(C);
      System.Delete(S,1,P);
    Until (S='') or Terminated;
  end;

Const
  SetCookie = 'set-cookie';

Var
  StatusLine,S : String;

begin
  If Assigned(FCookies) then
    FCookies.Clear;
  if not ReadString(StatusLine) then
    Exit(0);
  Result:=ParseStatusLine(StatusLine);
  Repeat
    if ReadString(S) and (S<>'') then
      begin
      ResponseHeaders.Add(S);
      If (LowerCase(Copy(S,1,Length(SetCookie)))=SetCookie) then
        DoCookies(S);
      end
  Until (S='') or Terminated;
  If Assigned(FOnHeaders) and not Terminated then
    FOnHeaders(Self);
end;

function TFPCustomHTTPClient.CheckResponseCode(ACode: Integer;
  const AllowedResponseCodes: array of Integer): Boolean;

Var
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
  If (Not Result) then
    begin
    if AllowRedirect then
      Result:=IsRedirect(ACode);
    If (ACode=401) then
      Result:=Assigned(FOnPassword);
    end;
end;

function TFPCustomHTTPClient.CheckContentLength: Int64;

Const CL ='content-length:';

Var
  S : String;
  I : integer;

begin
  Result:=-1;
  I:=0;
  While (Result=-1) and (I<FResponseHeaders.Count) do
    begin
    S:=Trim(LowerCase(FResponseHeaders[i]));
    If (Copy(S,1,Length(Cl))=Cl) then
      begin
      System.Delete(S,1,Length(CL));
      Result:=StrToInt64Def(Trim(S),-1);
      end;
    Inc(I);
    end;
  FContentLength:=Result;
end;

function TFPCustomHTTPClient.CheckTransferEncoding: string;

Const CL ='transfer-encoding:';

Var
  S : String;
  I : integer;

begin
  Result:='';
  I:=0;
  While (I<FResponseHeaders.Count) do
    begin
    S:=Trim(LowerCase(FResponseHeaders[i]));
    If (Copy(S,1,Length(Cl))=Cl) then
      begin
      System.Delete(S,1,Length(CL));
      Result:=Trim(S);
      exit;
      end;
    Inc(I);
    end;
end;

function TFPCustomHTTPClient.GetCookies: TStrings;
begin
  If (FCookies=Nil) then
    FCookies:=TStringList.Create;
  Result:=FCookies;
end;

function TFPCustomHTTPClient.GetProxy: TProxyData;
begin
  If not Assigned(FProxy) then
    begin
    FProxy:=CreateProxyData;
    FProxy.FHTTPClient:=Self;
    end;
  Result:=FProxy;
end;

procedure TFPCustomHTTPClient.SetCookies(const AValue: TStrings);
begin
  if GetCookies=AValue then exit;
  GetCookies.Assign(AValue);
end;

procedure TFPCustomHTTPClient.SetHTTPVersion(const AValue: String);
begin
  if FHTTPVersion = AValue then Exit;
  FHTTPVersion := AValue;
  if (AValue<>'1.1') then
    KeepConnection:=False;
end;

procedure TFPCustomHTTPClient.SetKeepConnection(AValue: Boolean);
begin
  if FKeepConnection=AValue then Exit;
  FKeepConnection:=AValue;
  if AValue then
    HTTPVersion:='1.1'
  else if IsConnected then
    DisconnectFromServer;
  CheckConnectionCloseHeader;
end;

procedure TFPCustomHTTPClient.SetProxy(AValue: TProxyData);
begin
  if (AValue=FProxy) then exit;
  Proxy.Assign(AValue);
end;

Function TFPCustomHTTPClient.ReadResponse(Stream: TStream;
  const AllowedResponseCodes: array of Integer; HeadersOnly: Boolean): Boolean;

  Function Transfer(LB : Integer) : Integer;

  begin
    Result:=FSocket.Read(FBuffer[1],LB);
    If Result<0 then
      Raise EHTTPClient.Create(SErrReadingSocket);
    if (Result>0) then
      begin
      FDataRead:=FDataRead+Result;
      DoDataRead;
      Stream.Write(FBuffer[1],Result);
      end;
  end;

  Procedure ReadChunkedResponse;
  { HTTP 1.1 chunked response:
    There is no content-length. The response consists of several chunks of
    data, each
    - beginning with a line
      - starting with a hex number DataSize,
      - an optional parameter,
      - ending with #13#10,
    - followed by the data,
    - ending with #13#10 (not in DataSize),
    It ends when the DataSize is 0.
    After the last chunk there can be a some optional entity header fields.
    This trailer is not yet implemented. }
  var
    BufPos: Integer;

    function FetchData(out Cnt: integer): boolean;

    begin
      Result:=False;
      If Terminated then
        exit;
      SetLength(FBuffer,ReadBuflen);
      Cnt:=FSocket.Read(FBuffer[1],length(FBuffer));
      If Cnt<0 then
        Raise EHTTPClient.Create(SErrReadingSocket);
      SetLength(FBuffer,Cnt);
      BufPos:=1;
      Result:=Cnt>0;
      FDataRead:=FDataRead+Cnt;
      DoDataRead;
  end;

    Function ReadData(Data: PByte; Cnt: integer): integer;

    var
      l: Integer;
    begin
      Result:=0;
      while Cnt>0 do
        begin
        l:=length(FBuffer)-BufPos+1;
        if l=0 then
          if not FetchData(l) then
            exit; // end of stream
        if l>Cnt then
          l:=Cnt;
        System.Move(FBuffer[BufPos],Data^,l);
        inc(BufPos,l);
        inc(Data,l);
        inc(Result,l);
        dec(Cnt,l);
      end;
    end;

  var
    c: char;
    ChunkSize: SizeUInt;
    l: Integer;
  begin
    BufPos:=1;
    repeat
      // read ChunkSize
      ChunkSize:=0;
      repeat
        if ReadData(@c,1)<1 then exit;
        // Protect from overflow
        If ChunkSize>(High(SizeUInt) div 16) then
          Raise EHTTPClient.CreateFmt(SErrChunkTooBig,[ChunkSize,High(SizeUInt) div 16]);
        case c of
        '0'..'9': ChunkSize:=ChunkSize*16+ord(c)-ord('0');
        'a'..'f': ChunkSize:=ChunkSize*16+ord(c)-ord('a')+10;
        'A'..'F': ChunkSize:=ChunkSize*16+ord(c)-ord('A')+10;
        else
          break;
        end;
        If (MaxChunkSize>0) and (ChunkSize>MaxChunkSize) then
          Raise EHTTPClient.CreateFmt(SErrChunkTooBig,[ChunkSize,MaxChunkSize]);
      until Terminated;
      // read till line end
      while (c<>#10) and not Terminated do
        if ReadData(@c,1)<1 then exit;
      if ChunkSize=0 then exit;
      // read data
      repeat
        if Terminated then
          exit;
        l:=length(FBuffer)-BufPos+1;
        if l=0 then
          if not FetchData(l) then
            exit; // end of stream
        if l>ChunkSize then
          l:=ChunkSize;
        if l>0 then
          begin
          // copy chunk data to output
          Stream.Write(FBuffer[BufPos],l);
          inc(BufPos,l);
          dec(ChunkSize,l);
          end;
      until ChunkSize=0;
      // read #13#10
      if ReadData(@c,1)<1 then
        exit;
      if Not Terminated then
        begin
        if c<>#13 then
          Raise EHTTPClient.Create(SErrChunkLineEndMissing);
        if ReadData(@c,1)<1 then exit;
        if c<>#10 then
          Raise EHTTPClient.Create(SErrChunkLineEndMissing);
        // next chunk
        end;
    until Terminated;
  end;

Var
  L : Int64;
  LB,R : Integer;

begin
  FDataRead:=0;
  FContentLength:=0;
  SetLength(FBuffer,0);
  FResponseStatusCode:=ReadResponseHeaders;
  Result := FResponseStatusCode > 0;
  if not Result then
    Exit;
  if not CheckResponseCode(FResponseStatusCode,AllowedResponseCodes) then
    Raise EHTTPClient.CreateFmt(SErrUnexpectedResponse,[ResponseStatusCode]);
  if HeadersOnly Or (AllowRedirect and IsRedirect(FResponseStatusCode)) then
    exit;
  if CompareText(CheckTransferEncoding,'chunked')=0 then
    ReadChunkedResponse
  else
    begin
    // Write remains of buffer to output.
    LB:=Length(FBuffer);
    FDataRead:=LB;
    If (LB>0) then
      Stream.WriteBuffer(FBuffer[1],LB);
    // Now read the rest, if any.
    SetLength(FBuffer,ReadBuflen);
    L:=CheckContentLength;
    If (L>LB) then
      begin
      // We cannot use copyfrom, it uses ReadBuffer, and this is dangerous with sockets
      L:=L-LB;
      Repeat
        LB:=ReadBufLen;
        If (LB>L) then
          LB:=L;
        R:=Transfer(LB);
        L:=L-R;
      until (L=0) or (R=0) or Terminated;
      end
    else if (L<0) and (Not NoContentAllowed(ResponseStatusCode)) then
      begin
      // No content-length, so we read till no more data available.
      Repeat
        R:=Transfer(ReadBufLen);
      until (R=0) or Terminated;
      end;
    end;
end;

Procedure TFPCustomHTTPClient.ExtractHostPort(AURI: TURI; Out AHost: String;
  Out APort: Word);
Begin
  if ProxyActive then
    begin
    AHost:=Proxy.Host;
    APort:=Proxy.Port;
    end
  else
    begin
    AHost:=AURI.Host;
    APort:=AURI.Port;
    end;
End;

procedure TFPCustomHTTPClient.CheckConnectionCloseHeader;

Var
  I : integer;
  N,V : String;

begin
  V:=GetHeader('Connection');
  If FKeepConnection Then
    begin
    I:=IndexOfHeader(FRequestHeaders,'Connection');
    If i>-1 Then
      begin
      // It can be keep-alive, check value
      FRequestHeaders.GetNameValue(I,N,V);
      If CompareText(V,'close')=0  then
        FRequestHeaders.Delete(i);
      end
    end
  Else
    AddHeader('Connection', 'close');
end;

Procedure TFPCustomHTTPClient.DoNormalRequest(const AURI: TURI;
  const AMethod: string; AStream: TStream;
  const AAllowedResponseCodes: array of Integer;
  AHeadersOnly, AIsHttps: Boolean);
Var
  CHost: string;
  CPort: Word;

begin
  ExtractHostPort(AURI, CHost, CPort);
  ConnectToServer(CHost,CPort,AIsHttps);
  Try
    SendRequest(AMethod,AURI);
    if not Terminated then
      ReadResponse(AStream,AAllowedResponseCodes,AHeadersOnly);
  Finally
    DisconnectFromServer;
  End;
end;

Procedure TFPCustomHTTPClient.DoKeepConnectionRequest(const AURI: TURI;
  const AMethod: string; AStream: TStream;
  const AAllowedResponseCodes: array of Integer;
  AHeadersOnly, AIsHttps: Boolean);
Var
  T: Boolean;
  CHost: string;
  CPort: Word;

begin
  ExtractHostPort(AURI, CHost, CPort);
  T := False;
  Repeat
    If Not IsConnected Then
      ConnectToServer(CHost,CPort,AIsHttps);
    Try
      if not Terminated then
        SendRequest(AMethod,AURI);
      if not Terminated then
        begin
        T := ReadResponse(AStream,AAllowedResponseCodes,AHeadersOnly);
        If Not T Then
          ReconnectToServer(CHost,CPort,AIsHttps);
        end;
    Finally
      // On terminate, we close the request
      If HasConnectionClose or Terminated Then
        DisconnectFromServer;
    End;
  Until T or Terminated;
end;

Procedure TFPCustomHTTPClient.DoMethod(Const AMethod, AURL: String;
  Stream: TStream; Const AllowedResponseCodes: Array of Integer);

Var
  URI: TURI;
  P: String;
  IsHttps, HeadersOnly: Boolean;

begin
  ResetResponse;
  URI:=ParseURI(AURL,False);
  p:=LowerCase(URI.Protocol);
  If Not ((P='http') or (P='https')) then
   Raise EHTTPClient.CreateFmt(SErrInvalidProtocol,[URI.Protocol]);
  IsHttps:=P='https';
  HeadersOnly:=CompareText(AMethod,'HEAD')=0;
  if FKeepConnection then
    DoKeepConnectionRequest(URI,AMethod,Stream,AllowedResponseCodes,HeadersOnly,IsHttps)
  else
    DoNormalRequest(URI,AMethod,Stream,AllowedResponseCodes,HeadersOnly,IsHttps);
end;

constructor TFPCustomHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Infinite timeout on most platforms
  FIOTimeout:=0;
  FConnectTimeout:=3000;
  FRequestHeaders:=TStringList.Create;
  FRequestHeaders.NameValueSeparator:=':';
  FResponseHeaders:=TStringList.Create;
  FResponseHeaders.NameValueSeparator:=':';
  HTTPVersion:='1.1';
  FMaxRedirects:=DefMaxRedirects;
end;

destructor TFPCustomHTTPClient.Destroy;
begin
  if IsConnected then
    DisconnectFromServer;
  FreeAndNil(FProxy);
  FreeAndNil(FCookies);
  FreeAndNil(FSentCookies);
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FResponseHeaders);
  inherited Destroy;
end;

class procedure TFPCustomHTTPClient.AddHeader(HTTPHeaders: TStrings;
  const AHeader, AValue: String);

Var
  J: Integer;
  S : String;

begin
  J:=IndexOfHeader(HTTPHeaders,Aheader);
  S:=AHeader+': '+Avalue;
  if (J<>-1) then
    HTTPHeaders[j]:=S
  else
    HTTPHeaders.Add(S);
end;


class function TFPCustomHTTPClient.IndexOfHeader(HTTPHeaders: TStrings;
  const AHeader: String): Integer;

Var
  L : Integer;
  H : String;
begin
  H:=LowerCase(Aheader)+':';
  l:=Length(H);
  Result:=HTTPHeaders.Count-1;
  While (Result>=0) and ((LowerCase(Copy(HTTPHeaders[Result],1,l)))<>h) do
    Dec(Result);
end;

class function TFPCustomHTTPClient.GetHeader(HTTPHeaders: TStrings;
  const AHeader: String): String;
Var
  I : Integer;
begin
  I:=IndexOfHeader(HTTPHeaders,AHeader);
  if (I=-1) then
    Result:=''
  else
    begin
    Result:=HTTPHeaders[i];
    I:=Pos(':',Result);
    if (I=0) then
      I:=Length(Result);
    System.Delete(Result,1,I);
    Result:=TrimLeft(Result);
    end;
end;

procedure TFPCustomHTTPClient.Terminate;
begin
  FTerminated:=True;
end;

procedure TFPCustomHTTPClient.ResetResponse;

begin
  FResponseStatusCode:=0;
  FResponseStatusText:='';
  FResponseHeaders.Clear;
  FServerHTTPVersion:='';
  FBuffer:='';
end;

procedure TFPCustomHTTPClient.HTTPMethod(const AMethod, AURL: String;
  Stream: TStream; const AllowedResponseCodes: array of Integer);

Var
  M,L,NL,RNL : String;
  RC : Integer;
  RR : Boolean; // Repeat request ?

begin
  // Reset Terminated
  FTerminated:=False;
  L:=AURL;
  RC:=0;
  RR:=False;
  M:=AMethod;
  Repeat
    if Not AllowRedirect then
      DoMethod(M,L,Stream,AllowedResponseCodes)
    else
      begin
      DoMethod(M,L,Stream,AllowedResponseCodes);
      if IsRedirect(FResponseStatusCode) and not Terminated then
        begin
        Inc(RC);
        if (RC>MaxRedirects) then
          Raise EHTTPClient.CreateFmt(SErrMaxRedirectsReached,[RC]);
        NL:=GetHeader(FResponseHeaders,'Location');
        if Assigned(FOnRedirect) then
          FOnRedirect(Self,L,NL);
        if (not IsAbsoluteURI(NL)) and ResolveRelativeURI(L,NL,RNL) then
          NL:=RNL;
        if (RedirectForcesGET(FResponseStatusCode)) then
          M:='GET';
        // Request has saved cookies in sentcookies.
        if ParseURI(L).Host=ParseURI(NL).Host then
          FreeAndNil(FSentCookies)
        else
          begin
          FreeAndNil(FCookies);
          FCookies:=FSentCookies;
          FSentCookies:=Nil;
          end;
        L:=NL;
        end;
      end;
    if (FResponseStatusCode=401) then
      begin
      RR:=False;
      if Assigned(FOnPassword) then
        FOnPassword(Self,RR);
      end
    else
      begin
      RR:=AllowRedirect and IsRedirect(FResponseStatusCode) and (L<>'');
      if RR and Assigned(FRequestBody) and (FRequestBody.Size>0) then
        FRequestBody.Position:=0;
      end;
  until Terminated or not RR ;
end;

procedure TFPCustomHTTPClient.Get(const AURL: String; Stream: TStream);
begin
  HTTPMethod('GET',AURL,Stream,[200]);
end;

procedure TFPCustomHTTPClient.Get(const AURL: String;
  const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Get(AURL,F);
  finally
    F.Free;
  end;
end;

procedure TFPCustomHTTPClient.Get(const AURL: String; Response: TStrings);
begin
  Response.Text:=Get(AURL);
end;

function TFPCustomHTTPClient.Get(const AURL: String): RawByteString;

Var
  SS : TRawByteStringStream;

begin
  SS:=TRawByteStringStream.Create;
  try
    Get(AURL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

class function TFPCustomHTTPClient.IsRedirect(ACode: Integer): Boolean;
begin
  Case ACode of
    301,
    302,
    303,
    307,
    308 : Result:=True;
  else
    Result:=False;
  end;
end;

class function TFPCustomHTTPClient.RedirectForcesGET(ACode: Integer): Boolean;
begin
  Result:=(ACode=303)
end;


class procedure TFPCustomHTTPClient.SimpleGet(const AURL: String;
  Stream: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Get(AURL,Stream);
    finally
      Free;
    end;
end;


class procedure TFPCustomHTTPClient.SimpleGet(const AURL: String;
  const LocalFileName: String);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Get(AURL,LocalFileName);
    finally
      Free;
    end;
end;


class procedure TFPCustomHTTPClient.SimpleGet(const AURL: String;
  Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Get(AURL,Response);
    finally
      Free;
    end;
end;


class function TFPCustomHTTPClient.SimpleGet(const AURL: String): RawByteString;
 
begin
  With Self.Create(nil) do
    try
      Result:=Get(AURL);
    finally
      Free;
    end;
end;


procedure TFPCustomHTTPClient.Post(const URL: string; const Response: TStream);
begin
  HTTPMethod('POST',URL,Response,[]);
end;


procedure TFPCustomHTTPClient.Post(const URL: string; Response: TStrings);
begin
  Response.Text:=Post(URL);
end;


procedure TFPCustomHTTPClient.Post(const URL: string;
  const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Post(URL,F);
  finally
    F.Free;
  end;
end;


function TFPCustomHTTPClient.Post(const URL: string): RawByteString;
Var
  SS : TRawByteStringStream;
begin
  SS:=TRawByteStringStream.Create();
  try
    Post(URL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;


class procedure TFPCustomHTTPClient.SimplePost(const URL: string;
  const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Post(URL,Response);
    finally
      Free;
    end;
end;


class procedure TFPCustomHTTPClient.SimplePost(const URL: string;
  Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Post(URL,Response);
    finally
      Free;
    end;
end;


class procedure TFPCustomHTTPClient.SimplePost(const URL: string;
  const LocalFileName: String);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Post(URL,LocalFileName);
    finally
      Free;
    end;
end;


class function TFPCustomHTTPClient.SimplePost(const URL: string): RawByteString;

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Result:=Post(URL);
    finally
      Free;
    end;
end;

procedure TFPCustomHTTPClient.Put(const URL: string; const Response: TStream);
begin
  HTTPMethod('PUT',URL,Response,[]);
end;

procedure TFPCustomHTTPClient.Put(const URL: string; Response: TStrings);
begin
  Response.Text:=Put(URL);
end;

procedure TFPCustomHTTPClient.Put(const URL: string; const LocalFileName: String
  );

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Put(URL,F);
  finally
    F.Free;
  end;
end;

function TFPCustomHTTPClient.Put(const URL: string): RawByteString;
Var
  SS : TRawByteStringStream;
begin
  SS:=TRawByteStringStream.Create();
  try
    Put(URL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

class procedure TFPCustomHTTPClient.SimplePut(const URL: string;
  const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Put(URL,Response);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimplePut(const URL: string;
  Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Put(URL,Response);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimplePut(const URL: string;
  const LocalFileName: String);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Put(URL,LocalFileName);
    finally
      Free;
    end;
end;

class function TFPCustomHTTPClient.SimplePut(const URL: string): RawByteString;

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Result:=Put(URL);
    finally
      Free;
    end;
end;

procedure TFPCustomHTTPClient.Delete(const URL: string; const Response: TStream
  );
begin
  HTTPMethod('DELETE',URL,Response,[]);
end;

procedure TFPCustomHTTPClient.Delete(const URL: string; Response: TStrings);
begin
  Response.Text:=Delete(URL);
end;

procedure TFPCustomHTTPClient.Delete(const URL: string;
  const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Delete(URL,F);
  finally
    F.Free;
  end;
end;

function TFPCustomHTTPClient.Delete(const URL: string): RawByteString;
Var
  SS : TRawByteStringStream;
begin
  SS:=TRawByteStringStream.Create();
  try
    Delete(URL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

class procedure TFPCustomHTTPClient.SimpleDelete(const URL: string;
  const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Delete(URL,Response);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimpleDelete(const URL: string;
  Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Delete(URL,Response);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimpleDelete(const URL: string;
  const LocalFileName: String);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Delete(URL,LocalFileName);
    finally
      Free;
    end;
end;

class function TFPCustomHTTPClient.SimpleDelete(const URL: string): RawByteString;

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Result:=Delete(URL);
    finally
      Free;
    end;
end;

procedure TFPCustomHTTPClient.Options(const URL: string; const Response: TStream
  );
begin
  HTTPMethod('OPTIONS',URL,Response,[]);
end;

procedure TFPCustomHTTPClient.Options(const URL: string; Response: TStrings);
begin
  Response.Text:=Options(URL);
end;

procedure TFPCustomHTTPClient.Options(const URL: string;
  const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Options(URL,F);
  finally
    F.Free;
  end;
end;

function TFPCustomHTTPClient.Options(const URL: string): RawByteString;
Var
  SS : TRawByteStringStream;
begin
  SS:=TRawByteStringStream.Create();
  try
    Options(URL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

class procedure TFPCustomHTTPClient.SimpleOptions(const URL: string;
  const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Options(URL,Response);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimpleOptions(const URL: string;
  Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Options(URL,Response);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimpleOptions(const URL: string;
  const LocalFileName: String);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Options(URL,LocalFileName);
    finally
      Free;
    end;
end;

class function TFPCustomHTTPClient.SimpleOptions(const URL: string): RawByteString;

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Result:=Options(URL);
    finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.Head(AURL: String; Headers: TStrings);
begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      HTTPMethod('HEAD', AURL, Nil, [200]);
      Headers.Assign(ResponseHeaders);
    Finally
      Free;
    end;
end;

procedure TFPCustomHTTPClient.FormPost(const URL : String; FormData: RawBytestring; const Response: TStream);

begin
  RequestBody:=TRawByteStringStream.Create(FormData);
  try
    AddHeader('Content-Type','application/x-www-form-urlencoded');
    Post(URL,Response);
  finally
    RequestBody.Free;
    RequestBody:=Nil;
  end;
end;

procedure TFPCustomHTTPClient.FormPost(const URL: string; FormData: TStrings;
  const Response: TStream);

Var
  I : Integer;
  S,N,V : String;

begin
  S:='';
  For I:=0 to FormData.Count-1 do
    begin
    If (S<>'') then
      S:=S+'&';
    FormData.GetNameValue(i,n,v);
    S:=S+EncodeURLElement(N)+'='+EncodeURLElement(V);
    end;
  FormPost(URL,S,Response);
end;

procedure TFPCustomHTTPClient.FormPost(const URL, FormData: string;
  const Response: TStrings);
begin
  Response.Text:=FormPost(URL,FormData);
end;

procedure TFPCustomHTTPClient.FormPost(const URL: string; FormData: TStrings;
  const Response: TStrings);
begin
  Response.Text:=FormPost(URL,FormData);
end;

function TFPCustomHTTPClient.FormPost(const URL : String;  Const FormData: RawBytestring): RawByteString;
Var
  SS : TRawByteStringStream;
begin
  SS:=TRawByteStringStream.Create();
  try
    FormPost(URL,FormData,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

function TFPCustomHTTPClient.FormPost(const URL: string; FormData: TStrings): RawByteString;
Var
  SS : TRawByteStringStream;
begin
  SS:=TRawByteStringStream.Create();
  try
    FormPost(URL,FormData,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

class procedure TFPCustomHTTPClient.SimpleFormPost(const URL : String; Const FormData: RawByteString; const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      FormPost(URL,FormData,Response);
    Finally
      Free;
    end;
end;


class procedure TFPCustomHTTPClient.SimpleFormPost(const URL: string;
  FormData: TStrings; const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      FormPost(URL,FormData,Response);
    Finally
      Free;
    end;
end;


class procedure TFPCustomHTTPClient.SimpleFormPost(const URL : String; Const FormData: RawBytestring; const Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      FormPost(URL,FormData,Response);
    Finally
      Free;
    end;
end;

class procedure TFPCustomHTTPClient.SimpleFormPost(const URL: string;
  FormData: TStrings; const Response: TStrings);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      FormPost(URL,FormData,Response);
    Finally
      Free;
    end;
end;

class function TFPCustomHTTPClient.SimpleFormPost(const URL: string;Const FormData : RawByteString): RawByteString;

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Result:=FormPost(URL,FormData);
    Finally
      Free;
    end;
end;

class function TFPCustomHTTPClient.SimpleFormPost(const URL: string; FormData: TStrings): RawByteString;

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      Result:=FormPost(URL,FormData);
    Finally
      Free;
    end;
end;


procedure TFPCustomHTTPClient.FileFormPost(const AURL, AFieldName,
  AFileName: string; const Response: TStream);
begin
  FileFormPost(AURL, nil, AFieldName, AFileName, Response);
end;

procedure TFPCustomHTTPClient.FileFormPost(const AURL: string;
  FormData: TStrings; AFieldName, AFileName: string; const Response: TStream);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    StreamFormPost(AURL, FormData, AFieldName, ExtractFileName(AFileName), F, Response);
  finally
    F.Free;
  end;
end;

procedure TFPCustomHTTPClient.StreamFormPost(const AURL, AFieldName,
  AFileName: string; const AStream: TStream; const Response: TStream);
begin
  StreamFormPost(AURL, nil, AFieldName, AFileName, AStream, Response);
end;

procedure TFPCustomHTTPClient.StreamFormPost(const AURL: string;
  FormData: TStrings; const AFieldName, AFileName: string;
  const AStream: TStream; const Response: TStream);
Var
  S, Sep : string;
  SS : TRawByteStringStream;
  I: Integer;
  N,V: String;
begin
  Sep:=Format('%.8x_multipart_boundary',[Random($ffffff)]);
  AddHeader('Content-Type','multipart/form-data; boundary='+Sep);
  SS:=TRawByteStringStream.Create();
  try
    if (FormData<>Nil) then
      for I:=0 to FormData.Count -1 do
        begin
        // not url encoded
        FormData.GetNameValue(I,N,V);
        S :='--'+Sep+CRLF;
        S:=S+Format('Content-Disposition: form-data; name="%s"'+CRLF+CRLF+'%s'+CRLF,[N, V]);
        SS.WriteBuffer(S[1],Length(S));
        end;
    S:='--'+Sep+CRLF;
    s:=s+Format('Content-Disposition: form-data; name="%s"; filename="%s"'+CRLF,[AFieldName,ExtractFileName(AFileName)]);
    s:=s+'Content-Type: application/octet-string'+CRLF+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    AStream.Seek(0, soFromBeginning);
    SS.CopyFrom(AStream,AStream.Size);
    S:=CRLF+'--'+Sep+'--'+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    SS.Position:=0;
    RequestBody:=SS;
    Post(AURL,Response);
  finally
    RequestBody:=Nil;
    SS.Free;
  end;
end;


class procedure TFPCustomHTTPClient.SimpleFileFormPost(const AURL, AFieldName,
  AFileName: string; const Response: TStream);

begin
  With Self.Create(nil) do
    try
      KeepConnection := False;
      FileFormPost(AURL,AFieldName,AFileName,Response);
    Finally
      Free;
    end;
end;

end.

