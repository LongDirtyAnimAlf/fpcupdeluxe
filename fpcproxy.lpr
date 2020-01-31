program fpcproxy;

{$mode objfpc}{$H+}

uses
  SysUtils;//,process;

procedure error(const s : string);
begin
  writeln('Error: ',s);
  halt(1);
end;

var
  i: integer;
  //AProcess: TProcess;
  aPath:string;
  ppccommandline : array of ansistring;
  errorvalue     : Longint;
  fpcbin: string;
begin
  aPath:=ExtractFilePath(ParamStr(0));
  {$IFDEF WINDOWS}
  fpcbin:=aPath+'fpc.exe';
  {$ELSE}
  fpcbin:=aPath+'fpc';
  {$ENDIF}
  
  {
  AProcess:=TProcess.Create(nil);
  AProcess.Options := [poWaitOnExit];
  APRocess.Executable:=fpcbin;

  APRocess.Parameters.Append('-n');
  APRocess.Parameters.Append('@'+aPath+'fpc.cfg');

  for i:=1 to ParamCount() do APRocess.Parameters.Append(ParamStr(i));
  AProcess.Execute;
  AProcess.Free;
  }

  setlength(ppccommandline,paramcount+2);
  ppccommandline[0]:='-n';
  ppccommandline[1]:='@'+aPath+'fpc.cfg';
  for i:=1 to ParamCount() do ppccommandline[i+1]:=ParamStr(i);
    
  try
    errorvalue:=ExecuteProcess(fpcbin,ppccommandline);
  except
    on e : exception do
      error(fpcbin+' can''t be executed, error message: '+e.message);
  end;
  
  if (errorvalue<>0) and
     (paramcount<>0) then
    error(fpcbin+' returned an error exitcode');
  halt(errorvalue);
end.

