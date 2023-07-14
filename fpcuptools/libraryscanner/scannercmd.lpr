program scannercmd;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp,
  scannercore
  { you can add units after this };

type

  { TLibraryScanner }

  TLibraryScanner = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


{ TLibraryScanner }

procedure TLibraryScanner.DoRun;
var
  ErrorMsg:string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  with TScannerCore.Create do
  begin
    try
      GetAndSaveLibs(Location);
    finally
      Free;
    end;
  end;

  // stop program loop
  Terminate;
end;

constructor TLibraryScanner.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TLibraryScanner.Destroy;
begin
  inherited Destroy;
end;

procedure TLibraryScanner.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TLibraryScanner;
begin
  Application:=TLibraryScanner.Create(nil);
  Application.Title:='My Scanner';
  Application.Run;
  Application.Free;
end.

