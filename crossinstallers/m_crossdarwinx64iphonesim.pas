unit m_crossdarwinx64iphonesim;

{
Cross compiles from Darwin to Darwin x64 bit iphone simulator

Copyright (C) 2013 Reinier Olislagers
Copyright (C) 2017 DonAlfredo

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation

uses
  fpcuputil;

const
  SDKNAME='iPhoneSimulator';

  SDKLOCATIONS:array[0..4] of string = (
    '/Applications/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/Desktop/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/Downloads/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Platforms/'+SDKNAME+'.platform/Developer/SDKs/'+SDKNAME+'.sdk'
  );

  TOOLCHAINLOCATIONS:array[0..4] of string = (
    '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '/Volumes/Xcode/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/Desktop/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/Downloads/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain',
    '~/fpcupdeluxe/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain'
  );

type

{ TDarwin64iphonesim }

TDarwin64iphonesim = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TDarwin64iphonesim }

function TDarwin64iphonesim.GetLibs(Basepath:string): boolean;
begin
  result:=FLibsFound;
  if result then exit;

  FLibsPath:='';
  result:=false;
  FLibsFound:=false;

  for FLibsPath in SDKLOCATIONS do
  begin
    FLibsPath:=ExpandFileName(FLibsPath);
    if DirectoryExists(FLibsPath) then
    begin
      FLibsFound:=true;
      break;
    end;
  end;

  if FLibsFound then
  begin
    FLibsPath:=IncludeTrailingPathDelimiter(FLibsPath)+'usr/lib/';
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
  end else FLibsPath:='';

  // Never fail.
  result:=true;
  FLibsFound:=true;
end;

function TDarwin64iphonesim.GetBinUtils(Basepath:string): boolean;
var
  aOption:string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPath:='';
  FBinUtilsPrefix:=''; // we have the "native" names, no prefix

  result:=false;
  FBinsFound:=false;

  for FBinUtilsPath in TOOLCHAINLOCATIONS do
  begin
    FBinUtilsPath:=ExpandFileName(FBinUtilsPath);
    if DirectoryExists(FBinUtilsPath) then
    begin
      FBinsFound:=true;
      break;
    end;
  end;

  if FBinsFound then
  begin
    AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(FBinUtilsPath));
    FBinUtilsPath:=IncludeTrailingPathDelimiter(FBinUtilsPath)+'usr/bin';
    AddFPCCFGSnippet('-FD'+FBinUtilsPath);{search this directory for compiler utilities}
  end else FBinUtilsPath:='';

  aOption:=GetDarwinSDKVersion(LowerCase(SDKNAME));
  if Length(aOption)>0 then AddFPCCFGSnippet('-WP'+aOption);

  // Never fail
  result:=true;
  FBinsFound:=true;
end;

constructor TDarwin64iphonesim.Create;
begin
  inherited Create;
  FCrossModuleNamePrefix:='TDarwin64';
  FTargetCPU:=TCPU.x86_64;
  FTargetOS:=TOS.iphonesim;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TDarwin64iphonesim.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF Darwin}
var
  Darwin64iphonesim:TDarwin64iphonesim;

initialization
  Darwin64iphonesim:=TDarwin64iphonesim.Create;
  RegisterCrossCompiler(Darwin64iphonesim.RegisterName,Darwin64iphonesim);

finalization
  Darwin64iphonesim.Destroy;
{$ENDIF}
end.

