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
var
  s:string;
  F:text;
begin
  if (ExecuteCommand(HgCommand,s,false)=0) then
    begin
    AssignFile(F,'revision.inc');
    Rewrite(F);
    while pos('"',s)>0 do delete(s,pos('"',s),1);
    writeln(F,s);
    Closefile(F);
    writeln(s);
    end
  else
    writeln('Failed : ',s);
end.