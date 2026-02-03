program stub;

{$mode objfpc}{$H+}

uses
 SysUtils, filesearcher;

const
   {$ifdef AR_STUB}
   EXENAME='llvm-ar';
   {$endif}
   {$ifdef AS_STUB}
   EXENAME='llvm-as';
   {$endif}
   {$ifdef NM_STUB}
   EXENAME='llvm-nm';
   {$endif}
   {$ifdef OBJCOPY_STUB}
   EXENAME='llvm-objcopy';
   {$endif}
   {$ifdef OBJDUMP_STUB}
   EXENAME='llvm-objdump';
   {$endif}
   {$ifdef STRIP_STUB}
   EXENAME='llvm-strip';
   {$endif}
var
  pc                 : integer;
  exe,path,aExePath  : string;
  ppccommandline     : array of ansistring;
  errorvalue         : Longint;
  envpath            : AnsiString;
begin
  aExePath:=ExtractFilePath(ParamStr(0));
  exe:=aExePath+EXENAME+exeext;
  //writeln('Lookup: ',exe);
  if FileExists(exe) then
  begin
    path:=exe;
    //writeln('Found file in current directory: ',path);
  end
  else
  begin
    envpath:=GetEnvironmentVariable('PATH');
    path:=FileSearch(exe,envpath);
    if ((path='') OR (path=ApplicationName)) then
    begin
      // Run through clang versions
      for pc:=25 downto 7 do
      begin
        exe:=EXENAME+'-'+InttoStr(pc)+exeext;
        path:=FileSearch(exe,envpath);
        if ((path<>'') AND (path<>ApplicationName)) then
        begin
          //writeln('Found file: ',path);
          break;
        end;
      end;
    end;
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

