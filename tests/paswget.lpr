program paswget;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fpcuputil, httpsend, synsock, blcksock, ftpsend,
  synacode, synafpc, synaip, synautil;

type

  { TWget }

  TWget = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TWget }

procedure TWget.DoRun;
var
  ErrorMsg: String;
  URL: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if (HasOption('h','help')) or
    (LowerCase(ParamStr(1))='help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if ParamCount=0 then
  begin
    // Demo url
    WriteHelp;
    URL:='http://sourceforge.net/projects/base64decoder/files/base64decoder/version%202.0/b64util.zip/download';
  end
  else
  begin
    URL:=ParamStr(1);
  end;

  // Use ExtractFileName for the document name. It isn't perhaps meant to, but it works.
  // Otherwise use the URIParser code in FCL.
  Download(URL, ExtractFileName(URL));

  // stop program loop
  Terminate;
end;

constructor TWget.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TWget.Destroy;
begin
  inherited Destroy;
end;

procedure TWget.WriteHelp;
begin
  writeln('Usage:');
  writeln(ExeName+' <url> download from URL (http only)');
  writeln('If no URL given, a demo URL will be used.');
  writeln(ExeName+' -h');
end;

var
  Application: TWget;
begin
  Application:=TWget.Create(nil);
  Application.Run;
  Application.Free;
end.

