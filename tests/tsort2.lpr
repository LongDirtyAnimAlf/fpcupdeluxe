program tsort2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, topologicalsort;

begin
  Application.Title:='tsort2gui';
  Application.Initialize;
  Application.CreateForm(TfrmMainform, frmMainform);
  Application.Run;
end.

