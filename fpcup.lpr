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
Possible additional verifications: check existing fpc locations, versions

Command: tfplist or something containing log records with timestamp, sequence description

Add something like fpcup.config in the settings or installed fpc/lazarus dir so we know for which fpc/laz combo this dir is used
}

//todo: check out build-cross.bat in win dir for lazarus for crosscompiling setup instructions

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,Forms,Interfaces {these 2 for application options},
  installer,
  svnclient, updatelazconfig, httpsend, ftpsend;

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
  writeln(' fpcOPT=options        options passed on to the fpc make as OPT=options.');
  writeln(' fpcrevision=<number>  Revert to fpc svn revision <number>');
  writeln(' fpcuplinkname=<name>  Name of the shortcut to the fpcup script.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If not specified, no shortcut will be produced.');
  writeln('                       If empty specified, the shortcut will be ');
  writeln('                       Fpcup_update.');
  writeln(' lazlinkname=<name>    Name of the shortcut to the Lazarus install.');
  writeln('                       On Windows: a desktop shortcut.');
  writeln('                       On other systems: a shell script in your home directory.');
  writeln('                       If empty specified, no shortcut will be produced.');
  writeln('                       Default: Lazarus_trunk');
  writeln(' lazrevision=<number>  Revert to lazarus svn revision <number>');
  writeln(' lazURL=<URL>          SVN URL from which to download; default: ');
  writeln('                       trunk (newest version):');
  writeln('                       http://svn.freepascal.org/svn/lazarus/trunk');
  writeln(' lazOPT=options        options passed on to the lazarus make as OPT=options.');
  writeln(' primary-config-path=<dir>');
  writeln('                       Analogous to Lazarus primary-config-path parameter.');
  writeln('                       Determines where fpcup will create or use as primary');
  writeln('                       configuration path for the Lazarus it installs/updates.');
  writeln('                       Default: empty; then a OS dependent configuration');
  writeln('                       directory is used.');
  writeln(' skipfpc               do not update FPC.');
  writeln(' skiplaz               do not update Lazarus.');
  writeln('');
end;

procedure CheckOptions(FInstaller: TInstaller);
const
  //Parameter names:
  BinutilsDir='binutilsdir';
  FPCBootstrapDir='fpcbootstrapdir';
  FPCDir='fpcdir';
  FPCURL='fpcURL';
  FPCOPT='fpcOPT';
  Help='help';
  LazDir='lazdir';
  LazLinkName='lazlinkname';
  FpcupLinkName='fpcuplinkname';
  LazURL='lazURL';
  LazOPT='lazOPT';
  LazRevision='lazrevision';
  FPCRevision='fpcrevision';
  PrimaryConfigPath='primary-config-path';
  SkipFPC='skipfpc';
  SkipLaz='skiplaz';
var
  ErrorMessage: string;
  alloptions,fpcuplink:string;
begin
  // Default values
  FInstaller.ShortCutName:='Lazarus_trunk';
  //don't initialise ShortCutNameFpcup !!!
  FInstaller.FPCURL := 'http://svn.freepascal.org/svn/fpc/branches/fixes_2_6';
  FInstaller.FPCOPT:='';
  FInstaller.LazarusPrimaryConfigPath:=''; //Let installer figure out default value
  FInstaller.LazarusURL := 'http://svn.freepascal.org/svn/lazarus/trunk';
  //svn2 seems to lag behind a lot, so don't use that.
  FInstaller.LazarusOPT:='';
  {$IFDEF MSWINDOWS}
  FInstaller.BootstrapCompilerDirectory := 'c:\development\fpcbootstrap\';
  FInstaller.FPCDirectory := 'c:\development\fpc';
  FInstaller.LazarusDirectory := 'c:\development\lazarus';
  FInstaller.MakeDirectory := 'C:\development\fpcbootstrap\';
  {$ENDIF MSWINDOWS}
  {$IFNDEF MSWINDOWS}
  FInstaller.BootstrapCompilerDirectory := '~/fpcbootstrap';
  FInstaller.FPCDirectory := '~/fpc';
  FInstaller.LazarusDirectory := '~/lazarus';
  FInstaller.MakeDirectory:='';
  {$ENDIF MSWINDOWS}

  ErrorMessage := Application.CheckOptions(
    'h', Binutilsdir+': '+FPCBootstrapDir+': '+FPCDir+': '+FPCURL+': '+FPCOPT+': '+
    Help+' '+LazDir+': '+LazOPT+': '+ LazRevision+': '+FPCRevision+': '+
    SkipFPC+' '+SkipLaz+' '+
    LazLinkName+': '+FpcupLinkName+': '+LazURL+':'+PrimaryConfigPath+':');
  if Length(ErrorMessage) > 0 then
  begin
    writeln('Error: wrong command line options given:');
    writeln(ErrorMessage);
    WriteHelp;
    FInstaller.Free;
    halt(13); //Quit with error resultcode
  end;

  alloptions:='';

  if Application.HasOption(BinutilsDir) then
  begin
    FInstaller.MakeDirectory:=Application.GetOptionValue(BinutilsDir);
    alloptions:=alloptions+BinutilsDir+'="'+FInstaller.MakeDirectory+'" ';
    {$IFNDEF MSWINDOWS}
    writeln('The '+BinutilsDir+' parameter is not necessary or supported on this system.');
    writeln('The parameter will be ignored.');
    FInstaller.MakeDirectory:='';
    {$ENDIF MSWINDOWS}
  end;

  if Application.HasOption(FPCBootstrapDir) then
  begin
    FInstaller.BootstrapCompilerDirectory:=Application.GetOptionValue(FPCBootstrapDir);
    alloptions:=alloptions+'--'+FPCBootstrapDir+'="'+FInstaller.BootstrapCompilerDirectory+'" ';
  end;

  if Application.HasOption(FPCDir) then
  begin
    FInstaller.FPCDirectory:=Application.GetOptionValue(FPCDir);
    alloptions:=alloptions+'--'+FPCDir+'="'+FInstaller.FPCDirectory+'" ';
  end;

  if Application.HasOption(FPCOPT) then
  begin
    FInstaller.FPCOPT:=Application.GetOptionValue(FPCOPT);
    alloptions:=alloptions+'--'+FPCOPT+'="'+FInstaller.FPCOPT+'" ';
  end;

  if Application.HasOption(FPCRevision) then
  begin
    FInstaller.FPCRevision:=Application.GetOptionValue(FPCRevision);
    //don't store this in alloptions !!!
  end;

  if Application.HasOption(FPCURL) then
  begin
    FInstaller.FPCURL:=Application.GetOptionValue(FPCURL);
    alloptions:=alloptions+'--'+FPCURL+'="'+FInstaller.FPCURL+'" ';
  end;

  if Application.HasOption('h', Help) then
  begin
    writehelp;
    FInstaller.Free;
    halt(0); //quit without error
  end;

  if Application.HasOption(LazDir) then
  begin
    FInstaller.LazarusDirectory:=Application.GetOptionValue(LazDir);
    alloptions:=alloptions+'--'+LazDir+'="'+FInstaller.LazarusDirectory+'" ';
  end;

  if Application.HasOption(LazLinkName) then
  begin
    FInstaller.ShortCutName:=Application.GetOptionValue(LazLinkName);
    alloptions:=alloptions+'--'+LazLinkName+'="'+FInstaller.ShortCutName+'" ';
  end;

  if Application.HasOption(LazOPT) then
  begin
    FInstaller.LazarusOPT:=Application.GetOptionValue(LazOPT);
    alloptions:=alloptions+'--'+LazOPT+'="'+FInstaller.LazarusOPT+'" ';
  end;

  if Application.HasOption(LazRevision) then
  begin
    FInstaller.LazarusRevision:=Application.GetOptionValue(LazRevision);
    //don't store this in alloptions !!!
  end;

  if Application.HasOption(LazURL) then
  begin
    FInstaller.LazarusDirectory:=Application.GetOptionValue(LazURL);
    alloptions:=alloptions+'--'+LazURL+'="'+FInstaller.LazarusDirectory+'" ';
  end;

  if Application.HasOption(PrimaryConfigPath) then
  begin
    // Only change if there's actually a valid value
    if (Application.GetOptionValue(PrimaryConfigPath)<>'') then
    begin
      FInstaller.LazarusPrimaryConfigPath:=Application.GetOptionValue(PrimaryConfigPath);
    end;
    alloptions:=alloptions+'--'+PrimaryConfigPath+'="'+Application.GetOptionValue(PrimaryConfigPath)+'" ';
  end;

  FInstaller.SkipFPC:=Application.HasOption(SkipFPC);
  FInstaller.SkipLazarus:=Application.HasOption(SkipLaz);


  // FpcupLinkName has to be the last since here we store alloptions !!
  // alloptions is rebuild in this clumsy way because we lost the quotes in paramstr()
  // and need them for option sequences, weird paths, etc.

  fpcuplink:='';
  if Application.HasOption(FpcupLinkName) then
  begin
    fpcuplink:=Application.GetOptionValue(FpcupLinkName);
    alloptions:=alloptions+'--'+FpcupLinkName+'="'+fpcuplink+'" ';
    FInstaller.AllOptions:=alloptions;
    if fpcuplink='' then
      if FInstaller.ShortCutName='' then
        fpcuplink:='fpcup_update'
      else
        fpcuplink:=FInstaller.ShortCutName+'_update';
    FInstaller.ShortCutNameFpcup:=fpcuplink;
  end;

  writeln('');
  writeln('Options:');
  writeln('Bootstrap compiler dir: '+FInstaller.BootstrapCompilerDirectory);
  writeln('Lazarus shortcut name:  '+FInstaller.ShortCutName);
  if fpcuplink<>'' then
    writeln('Shortcut fpcup name:    '+FInstaller.ShortCutNameFpcup);
  writeln('FPC URL:                '+FInstaller.FPCURL);
  writeln('FPC options:            '+FInstaller.FPCOPT);
  writeln('FPC directory:          '+FInstaller.FPCDirectory);
  writeln('Lazarus directory:      '+FInstaller.LazarusDirectory);
  writeln('Lazarus primary config path:');
  writeln('(Lazarus settings path) '+FInstaller.LazarusPrimaryConfigPath);
  writeln('Lazarus URL:            '+FInstaller.LazarusURL);
  writeln('Lazarus options:        '+FInstaller.LazarusOPT);
  writeln('Parameter list:         '+FInstaller.AllOptions);
  {$IFDEF MSWINDOWS}
  writeln('Make/binutils path:     '+FInstaller.MakeDirectory);
  {$ENDIF MSWINDOWS}
  writeln('');
  if not FInstaller.SkipFPC and (FInstaller.FPCRevision<>'') then
    writeln('WARNING: Reverting FPC to revision '+FInstaller.FPCRevision);
  if not FInstaller.SkipLazarus and (FInstaller.LazarusRevision<>'') then
    writeln('WARNING: Reverting Lazarus to revision '+FInstaller.LazarusRevision);
  if FInstaller.SkipFPC then
    writeln('WARNING: Skipping installation/update FPC ');
  if FInstaller.SkipLazarus then
    writeln('WARNING: Skipping installation/update Lazarus ');
  writeln('');

end;

procedure ShowErrorHints(SVNSourceDirectory: string);
begin
  writeln('Please check program output for details. Possible troubleshooting steps:');
  writeln('- make sure there''s a valid SVN executable in your path.');
  {$IFNDEF MSWINDOWS}
  writeln('- make sure the GNU binutils (make etc), windres, subversion client  are installed');
  writeln('  e.g. on Debian/Ubuntu: aptitude install build-essential subversion');
  //todo: how to get windres => mingw32-binutils?
  {$ENDIF MSWINDOWS}
  writeln('You might want to try removing all local changes in your SVN repository with:');
  writeln('- try removing all local changes in your SVN repository with: SVN revert recursive ' + SVNSourceDirectory);
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
        ShowErrorHints(FInstaller.LazarusDirectory);
      end;
    end
    else
    begin
      writeln('FPC retrieval/compilation failed.');
      ShowErrorHints(FInstaller.FPCDirectory);
    end;
  finally
    FInstaller.Free;
  end;
  writeln('FPCUp finished.');
end.
