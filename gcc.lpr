program gcc;
uses
  Classes, SysUtils, Process;
var
  i: integer;
  AProcess: TProcess;
begin
  AProcess:=TProcess.Create(nil);
  APRocess.Executable:=ExtractFilePath(ParamStr(0))+'gcc\gcc.exe';
  //APRocess.Executable:=ExtractFilePath(ParamStr(0))+'i386-mingw32-gcc.exe';
  for i:=1 to ParamCount() do APRocess.Parameters.Append(ParamStr(i));
  AProcess.Execute;
  AProcess.Free;
end.

