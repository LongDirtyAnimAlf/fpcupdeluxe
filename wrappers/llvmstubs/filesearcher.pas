unit filesearcher;

{$mode objfpc}{$H+}

interface

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

procedure error(const s : string);
function FileSearch (Const Name, DirList : string) : string;

implementation

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

  // Start with checking the file in the current directory
  if (Result <> '') and FileExists(Result) then exit;

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
      {$ifndef Unix}
      if (Result[1]='"') and (Result[Length(Result)]='"') then
        Result:=Copy(Result,2,Length(Result)-2);
      {$endif}
      if (Result<>'') then
        Result:=IncludeTrailingPathDelimiter(Result)+name;
    end;
    if (Result <> '') and FileExists(Result) then exit;
  end;
  Result:='';
end;


end.

