{ FPC/Lazarus installer/updater
Copyright (C) 2012 Reinier Olislagers

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
program fpcup;

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

//todo: check out build-cross.bat in win dir for lazarus for crosscompiling setup instructions

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,Forms, Interfaces {these 2 for application options},
  installer,
  svnclient, updatelazconfig;

//{$R *.res} //Keep it simple, no resources
procedure WriteHelp;
begin
  writeln('fpcup');
  writeln('A FPC/Lazarus downloader/updater/installer');
  writeln('Open source freeware (modified LGPL/BSD), see:');
  writeln('https://bitbucket.org/reiniero/fpcup');
  writeln('');
  writeln('fpcup --<options>)');
  writeln('Options are not required; they include:');
  writeln(' help                  Show this text');
  writeln(' binutilsdir=<dir>     Windows only');
  writeln('                       Directory where make, patch etc');
  writeln('                       (the binutils) are located. If make does not');
  writeln('                       exist, binutils will be downloaded there.');
  writeln('                       Default c:\development\fpcbootstrap\');
  writeln('                       Note: the binutils are copied to the');
  writeln('                       FPC directory for use by FPC. This gives');
  writeln('                       a more standard FPC environment.');
  writeln(' fpcbootstrapdir=<dir> An existing FPC compiler is needed to compile the FPC');
  writeln('                       sources. Specify location with this option; if no');
  writeln('                       compiler found here, FPCUp will download one there.');
  writeln('                       Default: c:\development\fpcbootstrap\');
  writeln(' fpcdir=<dir>          Target FPC dir, default c:\development\fpc\');
  writeln(' fpcURL=<URL>          SVN URL from which to download; default: fixes_2.6:');
  writeln('                       http://svn.freepascal.org/svn/fpc/branches/fixes_2_6');
  writeln(' lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\');
  writeln(' lazlinkname=<name>    Name of the desktop shortcut to the Lazarus install.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: Lazarus (trunk)');
  writeln(' lazURL=<URL>          SVN URL from which to download; default: ');
  writeln('                       trunk (newest version):');
  writeln('                       http://svn.freepascal.org/svn/lazarus/trunk');
  writeln('');
end;

procedure CheckOptions(FInstaller: TInstaller);
const
  BinutilsDir='binutilsdir';
  FPCBootstrapDir='fpcbootstrapdir';
  FPCDir='fpcdir';
  FPCURL='fpcURL';
  Help='help';
  LazDir='lazdir';
  LazLinkName='lazlinkname';
  LazURL='lazURL';
var
  ErrorMessage: string;
begin
// Default values

  FInstaller.DesktopShortCutName:='Lazarus (trunk)';
  FInstaller.FPCURL := 'http://svn.freepascal.org/svn/fpc/branches/fixes_2_6';
  FInstaller.LazarusURL := 'http://svn.freepascal.org/svn/lazarus/trunk';
  //svn2 seems to lag behind a lot.
  {$IFDEF WINDOWS}
  FInstaller.BootstrapCompilerDirectory := 'c:\development\fpcbootstrap\';
  FInstaller.FPCDirectory := 'c:\development\fpc';
  FInstaller.LazarusDirectory := 'c:\development\lazarus';
  FInstaller.MakePath := 'C:\development\fpcbootstrap\';
  {$ENDIF}
  {$IFNDEF WINDOWS}
  FInstaller.BootstrapCompilerDirectory := '~/fpcbootstrap';
  FInstaller.FPCDirectory := '~/fpc';
  FInstaller.LazarusDirectory := '~/lazarus';
  FInstaller.Makepath:='';
  {$ENDIF WINDOWS}

  ErrorMessage := Application.CheckOptions(
    'h', Binutilsdir+': '+FPCBootstrapDir+': '+FPCDir+': '+FPCURL+': '+
    Help+' '+LazDir+': '+
    LazLinkName+': '+LazURL+':');
  if Length(ErrorMessage) > 0 then
  begin
    writeln('Error: wrong command line options given:');
    writeln(ErrorMessage);
    WriteHelp;
    FInstaller.Free;
    halt(13); //Quit with error resultcode
  end;

  if Application.HasOption(BinutilsDir) then
  begin
    FInstaller.MakePath:=Application.GetOptionValue(BinutilsDir)
    {$IFNDEF WINDOWS}
    writeln('The '+BinutilsDir+' parameter is not necessary or supported on this system.');
    writeln('The parameter will be ignored.');
    FInstaller.Makepath:='';
    {$ENDIF WINDOWS}
  end;

  if Application.HasOption(FPCBootstrapDir) then
  begin
    FInstaller.BootstrapCompilerDirectory:=Application.GetOptionValue(FPCBootstrapDir)
  end;

  if Application.HasOption(FPCDir) then
  begin
    FInstaller.FPCDirectory:=Application.GetOptionValue(FPCDir)
  end;

  if Application.HasOption(FPCURL) then
  begin
    FInstaller.FPCURL:=Application.GetOptionValue(FPCURL)
  end;

  if Application.HasOption('h', Help) then
  begin
    writehelp;
    FInstaller.Free;
    halt(0); //quit without error
  end;

  if Application.HasOption(LazDir) then
  begin
    FInstaller.LazarusDirectory:=Application.GetOptionValue(LazDir)
  end;

  if Application.HasOption(LazLinkName) then
  begin
    FInstaller.DesktopShortCutName:=Application.GetOptionValue(LazLinkName)
  end;

  if Application.HasOption(LazURL) then
  begin
    FInstaller.LazarusDirectory:=Application.GetOptionValue(LazURL)
  end;
  writeln('');
  writeln('Options:');
  writeln('Bootstrap compiler dir: '+FInstaller.BootstrapCompilerDirectory);
  writeln('Shortcut name:          '+FInstaller.DesktopShortCutName);
  writeln('FPC URL:                '+FInstaller.FPCURL);
  writeln('FPC directory:          '+FInstaller.FPCDirectory);
  writeln('Lazarus directory:      '+FInstaller.LazarusDirectory);
  writeln('Lazarus URL:            '+FInstaller.LazarusURL);
  writeln('Make/binutils path:     '+FInstaller.MakePath);
  writeln('');
end;

var
  FInstaller: TInstaller;
begin
  writeln('FCPUp FreePascal/Lazarus downloader/installer started.');
  writeln('This program will download the FPC and Lazarus sources');
  writeln('from the source Subversion/SVN repositories,');
  writeln('compile, and install.');
  writeln('Result: you get a fresh, up-to-date Lazarus/FPC installation.');

  try
    // Adjust these directories to taste/your situation.
    FInstaller := TInstaller.Create;
    CheckOptions(FInstaller); //Process command line arguments

    // Get/update/compile (if needed) FPC; only compile Lazarus if succeeded.
    writeln('Getting and compiling fpc:');
    if FInstaller.GetFPC then
    begin
      writeln('Getting and compiling lazarus:');
      if FInstaller.GetLazarus=false then
      begin
        writeln('Lazarus retrieval/compilation failed.');
        writeln('Please check program output for details. Possible troubleshooting steps:');
        writeln('- make sure there''s a valid SVN executable in your path.');
        {$IFNDEF Windows}
        writeln('- make sure the GNU binutils are installed');
        {$ENDIF}
        writeln('You might want to try removing all local changes in your SVN repository with:');
        writeln('- try removing all local changes in your SVN repository with: SVN revert recursive ' + FInstaller.LazarusDirectory);
      end;
    end
    else
    begin
      writeln('FPC retrieval/compilation failed.');
      writeln('Please check program output for details. Possible troubleshooting steps:');
      writeln('- make sure there''s a valid SVN executable in your path.');
      {$IFNDEF Windows}
      writeln('- make sure the GNU binutils are installed');
      {$ENDIF}
      writeln('- try removing all local changes in your SVN repository with: SVN revert recursive ' + FInstaller.FPCDirectory);
    end;
  finally
    FInstaller.Free;
  end;
  writeln('FPCUp finished.');
end.

