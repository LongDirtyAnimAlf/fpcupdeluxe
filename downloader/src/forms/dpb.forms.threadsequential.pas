{ Implements Forms.ThreadSequential

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
unit DPB.Forms.ThreadSequential;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls
, DPB.Common.Threads
;

type
{ TDownload }
  TDownload = record
    URL: String;
    Filename: String;
  end;

{ TfrmThreadSequential }
  TfrmThreadSequential = class(TForm)
    lblTop: TLabel;
    lblDownloads: TLabel;
    pbDownloads: TProgressBar;
    lblBytes: TLabel;
    pbBytes: TProgressBar;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    FDownloads: Array of TDownload;
    FAllDone: Boolean;
  public
    procedure AddDownload(const AURL, AFilename: String);
    procedure ShowStatus(const ALen, APos: Int64);
  end;

var
  frmThreadSequential: TfrmThreadSequential;

implementation

uses
  DPB.Common.Utils
;

{$R *.lfm}

{ TfrmThreadSequential }

procedure TfrmThreadSequential.FormCreate(Sender: TObject);
begin
  FAllDone:= False;
end;

procedure TfrmThreadSequential.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmThreadSequential.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:= FAllDone;
end;

procedure TfrmThreadSequential.FormActivate(Sender: TObject);
var
  index: Integer;
  dlThread: TDownloadThread;
begin
  Application.ProcessMessages;
  pbDownloads.Max:= Length(FDownloads);
  for index:= 0 to Pred(Length(FDownloads)) do
  begin
    lblTop.Caption:= Format('File: %s',[FDownloads[index].Filename]);
    lblDownloads.Caption:= Format('%d of %d', [index + 1, Length(FDownloads)]);
    Application.ProcessMessages;
    try
      //DoDownload(index);
      dlThread:= TDownloadThread.Create(True);
      dlThread.OnShowStatus:= @ShowStatus;
      dlThread.URL:= FDownloads[index].URL;
      dlThread.Filename:= FDownloads[index].Filename;
      dlThread.Start;
      dlThread.WaitFor;
    except
      on E: Exception do
      begin
        { #todo 1 -ogcarreno : Inform about error }
        break;
      end;
    end;
    pbDownloads.Position:= index + 1;
    Application.ProcessMessages;
  end;
  FAllDone:= True;
  Close;
end;

procedure TfrmThreadSequential.AddDownload(const AURL, AFilename: String);
var
  len: Integer;
begin
  { #todo 1 -ogcarreno : Maybe test for duplicates? }
  len:= Length(FDownloads);
  SetLength(FDownloads, len + 1);
  FDownloads[len].URL:= AURL;
  FDownloads[len].Filename:= AFilename;
end;

procedure TfrmThreadSequential.ShowStatus(const ALen, APos: Int64);
var
  currentPercent: Double;
begin
  currentPercent:= (APos*100)/ALen;
  pbBytes.Position:= round(currentPercent);
  lblBytes.Caption:= Format('%s of %s', [FormatBytes(APos), FormatBytes(Alen)]);
  Application.ProcessMessages;
end;

end.

