program hgversion;

{ 
Creates revision.inc include file with revision id and commit date
from hg/mercurial repository.
Useful for build scripts/Lazarus macro actions to include the version
in your programs.
}

{$mode objfpc}{$H+}
uses
  sysutils,process,processutils {Runcommand support..};
const
  HgCommand = 'hg parents --template "const RevisionStr=''{node|short}'';versiondate=''{date|date}'';"';
  FailedOutput = 'const RevisionStr=''unknown'';versiondate=''unknown'';';
var
  ResultCode:integer;
  s:string;
  F:text;
begin
  // We add an empty parameter to force quoting
  ResultCode:=ExecuteCommand(HgCommand+' ""',s,false);
  if ResultCode=0 then
    begin
    while pos('"',s)>0 do delete(s,pos('"',s),1);
    // Extra test for e.g. OSX where we get here even though there's no hg command:
    if s='' then s:=FailedOutput;
    writeln(s);
    end
    else
    begin
    writeln('Failed: ',s);
    s:=FailedOutput;
    writeln('Command result code was: '+inttostr(ResultCode));
    writeln('Writing this to file:');
    writeln(s);
    end;
  AssignFile(F,'revision.inc');
  Rewrite(F);
  writeln(F,s);
  Closefile(F);
end.
