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
  OutputFile = 'revision.inc';
var
  ResultCode:integer;
  s:string;
  F:text;
begin
  // Quotes:
  // HgCommand : If we don't add anything, this will work on FPC 2.7, but fail on at least Win x86+FPC 2.6.x
  // HgCommand+' ""': will work on Win x86+FPC 2.6.x, fail on Linux
  // HgCommand+' "': will work on Linux+FPC 2.6.x, Win x86+FPC. Thanks, Arny!
  ResultCode:=ExecuteCommand(HgCommand+' "',s,false);
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
  AssignFile(F,OutputFile);
  Rewrite(F);
  writeln(F,s);
  Closefile(F);
end.
