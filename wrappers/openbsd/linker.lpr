program linker;

{$mode objfpc}{$H+}

uses
 SysUtils, filesearcher;

const
   EXENAME='ld.lld.exe';
var
  pc              : integer;
  path            : string;
  ppccommandline  : array of ansistring;
  errorvalue      : Longint;
  envpath         : AnsiString;
begin
  path:=ExtractFilePath(Paramstr(0))+EXENAME;

  if (NOT FileExists(path)) then
  begin
    envpath:=GetEnvironmentVariable('PATH');
    path:=FileSearch(EXENAME,envpath);
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

      pc:=0;
      while (pc<(paramcount-1)) do
      begin
        (*
        --allow-multiple-definition
        --allow-multiple-definition
        --relax
        -z
        relro
        --library-path=C:\fpcupsystems\stable324\cross\lib\x86_64-openbsd\
        --library-path=C:\fpcupsystems\stable324\cross\lib\x86_64-openbsd\usr\X11R6\lib\
        -rpath-link=C:\fpcupsystems\stable324\cross\lib\x86_64-openbsd\usr\X11R6\lib\
        -rpath-link=C:\fpcupsystems\stable324\cross\lib\x86_64-openbsd\
        -rpath-link=C:\fpcupsystems\stable324\cross\lib\x86_64-openbsd\usr\X11R6\lib\
        *)

        (*
        if (Pos('-dynamic-linker',(ParamStr(pc+1)))=1) then
        begin
          Inc(pc,1);
          continue;
        end;


        if (Pos('--rpath-link',(ParamStr(pc+1)))=1) then
        begin
          Inc(pc,1);
          continue;
        end;
        *)

        ppccommandline[pc]:=(ParamStr(pc+1));
        Inc(pc);
      end;
      //ppccommandline[pc]:='-T';
      //Inc(pc);
      //ppccommandline[pc]:=(ParamStr(pc));
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

