program nm;

{$mode objfpc}{$H+}

uses
 SysUtils, filesearcher;

const
   EXENAME='llvm-nm';

var
  pc              : integer;
  exe,path        : string;
  ppccommandline  : array of ansistring;
  errorvalue      : Longint;
  envpath         : AnsiString;
begin
  envpath:=GetEnvironmentVariable('PATH');

  // Run through clang versions
  for pc:=15 downto 7 do
  begin
    if (pc=15) then
      exe:=EXENAME+exeext
    else
      exe:=EXENAME+'-'+InttoStr(pc)+exeext;
    path:=FileSearch(exe,envpath);
    if ((path<>'') AND (path<>ApplicationName)) then break;
  end;
  if ((path='') OR (path=ApplicationName)) then
  begin
    error(path+' not found');
    halt(-1);
  end
  else
  begin
    if (paramcount>0) then
    begin
      setlength({%H-}ppccommandline,paramcount);
      for pc:=0 to Pred(paramcount) do ppccommandline[pc]:=(ParamStr(pc+1));
    end;
    try
      errorvalue:=ExecuteProcess(path,ppccommandline);
    except
      on e : exception do
        error(path+' can''t be executed, error message: '+e.message);
    end;
    if (errorvalue<>0) and
       (paramcount<>0) then
      error(path+' returned an error exitcode');
    Finalize(ppccommandline);
    halt(errorvalue);
  end;
end.

