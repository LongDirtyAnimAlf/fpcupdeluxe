program codesign;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, process;
const
  EXECUTABLE = '/usr/bin/codesign';
var
  i: integer;
  AProcess: TProcess;
begin
  if FileExists(EXECUTABLE) then
  begin
    AProcess:=TProcess.Create(nil);
    try
      APRocess.Executable:=EXECUTABLE;
      APRocess.Options := [poWaitOnExit];
      for i:=1 to ParamCount() do APRocess.Parameters.Append(ParamStr(i));
      AProcess.Execute;
    finally
      AProcess.Free;
    end;
  end;
end.

