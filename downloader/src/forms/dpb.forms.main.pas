{ Implements Forms.Main

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
unit DPB.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actDownloadThreadSequential: TAction;
    alMain: TActionList;
    actDownloadSequencial: TAction;
    btnThreadSequential: TButton;
    panMain: TPanel;
    btnSequencial: TButton;
    memLog: TMemo;
    procedure actDownloadSequencialExecute(Sender: TObject);
    procedure actDownloadThreadSequentialExecute(Sender: TObject);
  private
  public
    procedure Log(const AMessage: String);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  DPB.Forms.Sequencial
, DPB.Forms.ThreadSequential
;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.actDownloadSequencialExecute(Sender: TObject);
var
  frmSeq: TfrmSequencial;
begin
  actDownloadSequencial.Enabled:= False;
  try
    Log('Performing Sequencial download.');
    Log('  Creating form.');
    frmSeq:= TfrmSequencial.Create(nil);


    frmSeq.AddDownload(
      'https://www.visualsvn.com/files/Apache-Subversion-1.12.2.zip',
      'svn.zip');

    {
    frmSeq.AddDownload(
      'https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/wincrossbins_v1.2/WinCrossBinsAppleAll.zip',
      'WinCrossBinsAppleAll.zip');

    frmSeq.AddDownload(
      //'https://ams.edge.kernel.org/opensuse/distribution/leap/15.3/iso/openSUSE-Leap-15.3-NET-x86_64-Current.iso',
      'https://download.opensuse.org/distribution/leap/15.3/iso/openSUSE-Leap-15.3-NET-x86_64-Current.iso',
      'suse.iso');

    Log('  Adding: time_series_covid19_confirmed_global.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
      'time_series_covid19_confirmed_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
      'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
      'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_confirmed_US.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
      'time_series_covid19_confirmed_US.csv');

    Log('  Adding: time_series_covid19_deaths_US.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
      'time_series_covid19_deaths_US.csv');
    }

    Log('  Calling Show Modal.');
    frmSeq.ShowModal;
    Log('Done.');
  finally
    actDownloadSequencial.Enabled:= True;
  end;
end;

procedure TfrmMain.actDownloadThreadSequentialExecute(Sender: TObject);
var
  frmThreadSeq: TfrmThreadSequential;
begin
  actDownloadThreadSequential.Enabled:= False;
  try
    Log('Performing Threaded Sequencial download.');
    Log('  Creating form.');
    frmThreadSeq:= TfrmThreadSequential.Create(nil);

    Log('  Adding: time_series_covid19_confirmed_global.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
      'time_series_covid19_confirmed_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
      'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
      'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_confirmed_US.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
      'time_series_covid19_confirmed_US.csv');

    Log('  Adding: time_series_covid19_deaths_US.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
      'time_series_covid19_deaths_US.csv');

    Log('  Calling Show Modal.');
    frmThreadSeq.ShowModal;
    Log('Done.');
  finally
    actDownloadThreadSequential.Enabled:= True;
  end;
end;

procedure TfrmMain.Log(const AMessage: String);
begin
  memLog.Append(AMessage);
  Application.ProcessMessages;
end;

end.

