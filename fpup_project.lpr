program fpup_project;
{ Command line interface to installing/updating FPC/Lazarus instances }
{$mode objfpc}{$H+}
{
First verify: check existing fpc locations, versions, check svn present
Compiler test if suitable found: compile test file

Command: tfplist or something timestamp, sequence description; each has log
Log: timestamp, sequence, description

Choices: set by properties etc.
<id>, choice/value, description
Read out at end to show user what happened

Options startup: unattended: assume update if possible, if not, install
Do not set path, create shortcuts/batch files to start, with --primary-config-path
Add something like fpcup.config in the settings dir so we know for which fpc/laz combo this dir is used
}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  installer, svnclient;

//{$R *.res}
var
  FInstaller: TInstaller;
begin
  writeln('begin');
  // Let's start simple: checkout, update, make for FPC+Lazarus.
  try
    // Adjust these directories to taste/your situation.
    FInstaller:=TInstaller.Create;
    FInstaller.BootstrapCompiler := 'c:\lazarus\fpc\2.5.1\bin\i386-win32\ppc386.exe';
    FInstaller.FPCDirectory:='c:\development\fpc';
    FInstaller.LazarusDirectory:='c:\development\lazarus';
    FInstaller.Make:='C:\Lazarus\fpc\2.5.1\bin\i386-win32\make.exe';
    //todo: check for executables (right version as well), get them if required; should be class called by installer
    writeln('debug: getting and compiling fpc:');
    FInstaller.GetFPC;
    writeln('debug: getting and compiling lazarus:');
    FInstaller.GetLazarus;
  finally
    FInstaller.Free;
  end;
  writeln('end');
end.

