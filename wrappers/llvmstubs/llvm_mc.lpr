program llm_mc;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  //targetbin='clang';
  targetbin='wat2wasm';

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


procedure error(const s : string);
begin
  writeln('Error: ',s);
  halt(1);
end;

function SplitPath(Const HStr:String):String;
var
  i : longint;
begin
  i:=Length(Hstr);
  while (i>0) and not(Hstr[i] in ['\','/']) do
    dec(i);
  SplitPath:=Copy(Hstr,1,i);
end;

var
  s              : ansistring;
  path           : string;
  commandline    : array of ansistring;
  commandlinelen : longint;
  i              : longint;
  errorvalue     : Longint;

begin
  path:=splitpath(paramstr(0));
  setlength({%H-}commandline,ParamCount);
  commandlinelen:=0;
  for i:=1 to paramcount do
  begin
     s:=paramstr(i);
     if pos('--arch=wasm32',s)=1 then continue;
     if pos('-mattr=+sign-ext',s)=1 then continue;
     if pos('--filetype=obj',s)=1 then continue;
     if pos('--assemble',s)=1 then continue;
     commandline[commandlinelen]:=s;
     inc(commandlinelen);
  end;
  //commandline[0]:='-c';
  SetLength(commandline,commandlinelen);

  //s:=commandline[commandlinelen];
  //s:=copy(s,1,length(s)-2);
  //commandline[commandlinelen]:=s+'ppu';

  { call targetbin }
  try
    write(path+targetbin+exeext);
    write(' :');
    for i:=0 to Pred(Length(commandline)) do
    begin
       write(' ');
       write(commandline[i]);
    end;
    writeln;
    errorvalue:=ExecuteProcess(path+targetbin+exeext,commandline);
  except
    on e : exception do
      error(targetbin+' can''t be executed, error message: '+e.message);
  end;
  if (errorvalue<>0) and
     (paramcount<>0) then
    error(targetbin+' returned an error exitcode');
  halt(errorvalue);
end.

