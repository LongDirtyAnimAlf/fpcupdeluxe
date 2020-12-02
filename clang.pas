program clang;

{$mode objfpc}{$H+}

uses
 SysUtils;

const
{$ifdef UNIX}
    exeext='';
{$else UNIX}
  {$ifdef HASAMIGA}
    exeext='';
  {$else}
    {$ifdef NETWARE}
      exeext='.nlm';
    {$else}
      {$ifdef ATARI}
        exeext='.ttp';
      {$else}
        exeext='.exe';
      {$endif ATARI}
    {$endif NETWARE}
  {$endif HASAMIGA}
{$endif UNIX}
   EXENAME='clang'+exeext;

procedure error(const s : string);
  begin
     writeln('Error: ',s);
     halt(1);
  end;

function FileSearch (Const Name, DirList : string) : string;
var
  I : longint;
  Temp : string;
begin
  Result:=Name;
  temp:=SetDirSeparators(DirList);
  while true do
  begin
    If Temp = '' then Break;
    I:=pos(':',Temp);
    if ((I>0) and (Length(Temp)>I) AND (Temp[I+1]='\')) then I:=0;
    if (I=0) then I:=pos(';',Temp);
    if I<>0 then
    begin
      Result:=Copy (Temp,1,I-1);
      system.Delete(Temp,1,I);
    end
    else
    begin
      Result:=Temp;
      Temp:='';
    end;
    if Result<>'' then
    begin
      if (Result<>'') then
        Result:=IncludeTrailingPathDelimiter(Result)+name;
    end;
    if (Result <> '') and FileExists(Result) then exit;
  end;
  Result:='';
end;

var
  pc              : integer;
  path            : string;
  ppccommandline  : array of ansistring;
  errorvalue      : Longint;
  envpath         : AnsiString;
begin
  envpath:=GetEnvironmentVariable('PATH');
  path:=FileSearch(EXENAME,envpath);
  if ((path='') OR (path=EXENAME)) then
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

