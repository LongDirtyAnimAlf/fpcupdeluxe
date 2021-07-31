{ Implements Common.Threads

  MIT License

  Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit DPB.Common.Threads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
{$IF FPC_FULLVERSION < 30200}
, ssockets
, sslsockets
{$ENDIF}
;

type
{ TShowStatusEvent }
  TShowStatusEvent = procedure(const ALen, APos: Int64) of object;

{ TDownloadThread }
  TDownloadThread = class(TThread)
  private
    FSize: Int64;
    FPos: Int64;
    FURL: String;
    FFilename: String;
    FOnShowStatus: TShowStatusEvent;

{$IF FPC_FULLVERSION < 30200}
    procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean;
      out AHandler: TSocketHandler);
{$ENDIF}
    procedure DataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;

    property URL: String
      read FURL
      write FURL;

    property Filename: String
      read FFilename
      write FFilename;

    property OnShowStatus: TShowStatusEvent
      read FOnShowStatus
      write FOnShowStatus;
  published
  end;

implementation

uses
  fphttpclient
{$IF FPC_FULLVERSION >= 30200}
, opensslsockets
{$ELSE}
, fpopenssl
, openssl
{$ENDIF}
;

{ TDownloadThread }

constructor TDownloadThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TDownloadThread.Destroy;
begin
  inherited Destroy;
end;

{$IF FPC_FULLVERSION < 30200}
procedure TDownloadThread.GetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  AHandler := TSSLSocketHandler.Create;
  TSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
end;
{$ENDIF}

procedure TDownloadThread.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  FPos:= CurrentPos;
  Synchronize(@ShowStatus);
end;

procedure TDownloadThread.ShowStatus;
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(FSize, FPos);
  end;
end;

procedure TDownloadThread.Execute;
var
  http: TFPHTTPClient;
  index: Integer;
begin
{$IF FPC_FULLVERSION < 30200}
  InitSSLInterface;
{$ENDIF}
  http:= TFPHTTPClient.Create(nil);
{$IF FPC_FULLVERSION < 30200}
  http.OnGetSocketHandler:=@GetSocketHandler;
{$ENDIF}
  http.AllowRedirect:= True;
  try
    try
      http.HTTPMethod('HEAD', FURL, nil, []);
      //TFPHTTPClient.Head(FURL, headers);
      FSize := 0;
      for index := 0 to Pred(http.ResponseHeaders.Count) do
      begin
        if LowerCase(http.ResponseHeaders.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(http.ResponseHeaders.ValueFromIndex[index]);
        end;
      end;
      http.OnDataReceived:= @DataReceived;
      http.Get(FURL);
    except
      on E: Exception do
      begin
        if http.ResponseStatusCode > 399 then
        begin
          //Log(Format('Status: %d', [http.ResponseStatusCode]));
        end;
        //Log('Error: ' + E.Message);
      end;
    end;
  finally
    http.Free;
  end;
end;

end.

