{ Implements Forms.Sequential

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
unit DPB.Forms.Sequencial;

{$mode objfpc}{$H+}

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls
{$IFDEF USEMORMOT}
  , mormot.core.buffers
{$IFDEF UNIX}
  , mormot.lib.openssl11
{$ENDIF UNIX}
{$ELSE}
  {$IF FPC_FULLVERSION < 30200}
  , ssockets
  , sslsockets
  {$ENDIF}
{$ENDIF}
;

type
{ TDownload }
  TDownload = record
    URL: String;
    Filename: String;
  end;

{ TfrmSequencial }
  TfrmSequencial = class(TForm)
    lblTop: TLabel;
    lblDownloads: TLabel;
    pbDownloads: TProgressBar;
    lblBytes: TLabel;
    pbBytes: TProgressBar;
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormActivate({%H-}Sender: TObject);
  private
    FDownloads: Array of TDownload;
    FSize: Int64;
    FSuccess: boolean;
{$IFDEF USEMORMOT}
    procedure DataReceived(Sender: TStreamRedirect);
{$ELSE}
{$IF FPC_FULLVERSION < 30200}
    procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean;
      out AHandler: TSocketHandler);
{$ENDIF}
    procedure DataReceived({%H-}Sender : TObject; const {%H-}ContentLength, CurrentPos : Int64);
{$ENDIF}
    procedure DoDownload(const AIndex: Integer);
  public
    procedure AddDownload(const AURL, AFilename: String);
    property Success:boolean read FSuccess;
  end;

var
  frmSequencial: TfrmSequencial;

implementation

uses
  FileCtrl,
{$IFDEF USEMORMOT}
  mormot.net.client
{$ELSE}
  fphttpclient
{$IF FPC_FULLVERSION >= 30200}
  , opensslsockets
{$ELSE}
  , fpopenssl
  , openssl
{$ENDIF}
{$ENDIF}
, DPB.Common.Utils
;

{$R *.lfm}

{ TfrmSequencial }

procedure TfrmSequencial.FormCreate(Sender: TObject);
begin
  FSize:=0;
  FSuccess:=true;
end;

procedure TfrmSequencial.FormActivate(Sender: TObject);
var
  index: Integer;
begin
  OnActivate:=nil;
  lblDownloads.Visible:=(Length(FDownloads)>1);
  pbDownloads.Visible:=lblDownloads.Visible;
  Application.ProcessMessages;
  if pbDownloads.Visible then pbDownloads.Max:= Length(FDownloads);
  index:=0;
  repeat
    // Adjust text width and text length to be exactly the same width as the progress bar.
    lblTop.Caption:= Format('File: %s',[MiniMizeName(FDownloads[index].Filename, lblTop.Canvas, pbBytes.Width)]);
    //lblTop.Caption:= Format('File: %s',[FDownloads[index].Filename]);
    if lblDownloads.Visible then lblDownloads.Caption:= Format('%d of %d', [index + 1, Length(FDownloads)]);
    Application.ProcessMessages;
    try
      DoDownload(index);
    except
      on E: Exception do
      begin
        FSuccess:=false;
        break;
      end;
    end;
    Inc(index);
    if pbDownloads.Visible then
    begin
      pbDownloads.Position:= index;
      Application.ProcessMessages;
    end;
  until (index=Length(FDownloads));
  if FSuccess then FSuccess:=(index=Length(FDownloads));
  Close;
end;

{$IFNDEF USEMORMOT}
{$IF FPC_FULLVERSION < 30200}
procedure TfrmSequencial.GetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  AHandler := TSSLSocketHandler.Create;
  TSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
end;
{$ENDIF}
{$ENDIF}

procedure TfrmSequencial.DoDownload(const AIndex: Integer);
var
{$IFDEF USEMORMOT}
  params: THttpClientSocketWGet;
{$ELSE}
  http: TFPHTTPClient;
  index: Integer;
{$ENDIF}
begin
{$IFNDEF USEMORMOT}
{$IF FPC_FULLVERSION < 30200}
  InitSSLInterface;
{$ENDIF}
  http:= TFPHTTPClient.Create(nil);
{$IF FPC_FULLVERSION < 30200}
  http.OnGetSocketHandler:=@GetSocketHandler;
{$ENDIF}
  http.AllowRedirect:= True;
{$ENDIF}
  pbBytes.Position:= 0;
  try
    try
      {$IFDEF USEMORMOT}
      params.Clear;
      params.Resume := true;
      params.OnProgress := @DataReceived;
      if params.WGet(FDownloads[AIndex].URL, FDownloads[AIndex].Filename,
           '', nil, 5000, 5) <> FDownloads[AIndex].Filename then
      begin
      end;
      {$ELSE}
      lblBytes.Caption:= 'Determining size...';
      Application.ProcessMessages;
      http.HTTPMethod('HEAD', FDownloads[AIndex].URL, nil, []);
      //TFPHTTPClient.Head(FDownloads[AIndex].URL, headers);
      FSize := 0;
      for index := 0 to Pred(http.ResponseHeaders.Count) do
      begin
        if LowerCase(http.ResponseHeaders.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(http.ResponseHeaders.ValueFromIndex[index]);
        end;
      end;
      http.OnDataReceived:= @DataReceived;
      http.Get(FDownloads[AIndex].URL,FDownloads[AIndex].Filename);
      {$ENDIF}
    except
      on E: Exception do
      begin
        FSuccess:=false;
      end;
    end;
  finally
    {$IFNDEF USEMORMOT}
    http.Free;
    {$ENDIF}
  end;
end;

procedure TfrmSequencial.AddDownload(const AURL, AFilename: String);
var
  len: Integer;
begin
  { #todo 1 -ogcarreno : Maybe test for duplicates? }
  len:= Length(FDownloads);
  SetLength(FDownloads, len + 1);
  FDownloads[len].URL:= AURL;
  FDownloads[len].Filename:= AFilename;
end;

{$IFDEF USEMORMOT}
procedure TfrmSequencial.DataReceived(Sender: TStreamRedirect);
var
  aStream:TStreamRedirect;
begin
  aStream:=TStreamRedirect(Sender);
  pbBytes.Position:= aStream.Percent;
  lblBytes.Caption:= Format('%s of %s', [FormatBytes(aStream.ProcessedSize), FormatBytes(aStream.ExpectedSize)]);
  Application.ProcessMessages;
end;
{$ELSE}
procedure TfrmSequencial.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
var
  currentPercent: Double;
begin
  currentPercent:= (CurrentPos*100)/FSize;
  pbBytes.Position:= round(currentPercent);
  lblBytes.Caption:= Format('%s of %s', [FormatBytes(CurrentPos), FormatBytes(FSize)]);
  Application.ProcessMessages;
end;
{$ENDIF}


end.

