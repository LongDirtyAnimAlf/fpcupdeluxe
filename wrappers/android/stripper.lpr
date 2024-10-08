program stripper;

{$mode objfpc}{$H+}

uses
 SysUtils, filesearcher;

const
   {$ifdef AARCH64ANDROID}
   EXENAME='aarch64-linux-android-strip.cmd';
   {$endif}
   {$ifdef ARMANDROID}
   EXENAME='arm-linux-androideabi-strip.cmd';
   {$endif}
   {$ifdef I386ANDROID}
   EXENAME='i686-linux-android-strip.cmd';
   {$endif}
   {$ifdef AMD64ANDROID}
   EXENAME='x86_64-linux-android-strip.cmd';
   {$endif}

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
      while (pc<paramcount) do
      begin
        if (Pos('--defsym=',(ParamStr(pc+1)))=1) then
        begin
          Inc(pc,1);
          continue;
        end;
        if (Pos('--defsym',(ParamStr(pc+1)))=1) then
        begin
          Inc(pc,2);
          continue;
        end;
        ppccommandline[pc]:=(ParamStr(pc+1));
        Inc(pc);
      end;
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

