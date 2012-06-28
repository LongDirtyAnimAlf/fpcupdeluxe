program hgversion;

{ 
Creates revision.inc include file with revision id and commit date
from hg/mercurial repository.
Useful for build scripts/Lazarus macro actions to include the version
in your programs.
}

{$mode objfpc}{$H+}
uses
  sysutils,process,
  {$IF FPC_FULLVERSION < 20701}
  processutils {Runcommand support..}
  {$ENDIF FPC_FULLVERSION}
  ;
const
  HgCommand = 'hg parents --template "const RevisionStr=''{node|short}'';versiondate=''{date|date}'';" ""';
var
  s:string;
  F:text;
begin
  {$IF FPC_FULLVERSION < 20701}
  if (ExecuteCommand(HgCommand,s,false)=0) then
  {$ELSE}
  if RunCommand(HgCommand,[],s) then
  {$ENDIF FPC_FULLVERSION}  
    begin
    AssignFile(F,'revision.inc');
    Rewrite(F);
    writeln(F,s);
    Closefile(F);
    writeln(s);
    end
  else
    writeln('Failed : ',s);
end.