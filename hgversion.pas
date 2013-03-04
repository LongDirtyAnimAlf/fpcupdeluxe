program hgversion;

{ 
Creates revision.inc include file with revision id and commit date
from hg/mercurial repository.
Useful for build scripts/Lazarus macro actions to include the version
in your programs.
}

{$mode objfpc}{$H+}
uses
  SysUtils,
  Classes,
  process,
  processutils {Runcommand support..};

const
  HgDateCommand = '"hg" parents';
  HgRevisionCommand = '"hg" id -i';
  FailedRevision = 'unknown';
  FailedRevisionDate = 'unknown';
  OutputTemplate = 'const RevisionStr=''%s'';versiondate=''%s'';';
  OutputFile = 'revision.inc';
var
  OutputLines: TStringList;
  ResultCode: integer;
  Revision: string = FailedRevision;
  RevisionDate: string = FailedRevisionDate;
  Output: string;
  F: Text;
begin
  ResultCode := ExecuteCommand(HgRevisionCommand, Output, False);
  if ResultCode = 0 then
  begin
    // Output e.g. 0eae3e0a6aca
    // hg --debug id -i would give something like (+ means modified)
    // 0eae3e0a6aca64a6948e1d7505922f8fb0af389e+
    // Extra test for e.g. OSX where we get here even though there's no hg command:
    if Output <> '' then
    begin
      Revision := trim(Output);
      writeln('Revision: ' + Revision);
    end;
  end
  else
  begin
    writeln('Revision command failed: ', Output);
    writeln('Command result code was: ' + IntToStr(ResultCode));
  end;

  ResultCode := ExecuteCommand(HgDateCommand, Output, False);
  {Example output:
  changeset:   830:0eae3e0a6aca
  tag:         tip
  user:        reiniero
  date:        Sun Mar 03 16:11:19 2013 +0100
  summary:     Fixed invalid option in git pull command
  }
  if ResultCode = 0 then
  begin
    // Check OSX case:
    if Output <> '' then
    begin
      OutputLines := TStringList.Create;
      try
        OutputLines.NameValueSeparator := ':';
        OutputLines.Text := Output;
        if OutputLines.Values['date'] <> '' then
          RevisionDate := trim(OutputLines.Values['date']);
      finally
        OutputLines.Free;
      end;
    end;
    writeln('Revision date: ' + RevisionDate);
  end
  else
  begin
    writeln('Revision date command failed: ', Output);
    writeln('Command result code was: ' + IntToStr(ResultCode));
  end;

  writeln('Writing this to file:');
  writeln(format(OutputTemplate, [Revision, RevisionDate]));

  AssignFile(F, OutputFile);
  Rewrite(F);
  writeln(F, format(OutputTemplate, [Revision, RevisionDate]));
  Closefile(F);
end.
