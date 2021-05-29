program gcc;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, process;

var
  i: integer;
  AProcess: TProcess;
begin
  AProcess:=TProcess.Create(nil);
  APRocess.Executable:=ExtractFilePath(ParamStr(0))+'gcc\gcc.exe';
  APRocess.Options := [poWaitOnExit];
  //APRocess.Executable:=ExtractFilePath(ParamStr(0))+'i386-mingw32-gcc.exe';
  for i:=1 to ParamCount() do APRocess.Parameters.Append(ParamStr(i));
  AProcess.Execute;
  AProcess.Free;
end.

