{ Command line parsing unit

Copyright (C) 2012-2013 Ludo Brands

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
unit commandline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ECommandLineError=class(Exception);

  { TCommandLineOptions }

  TCommandLineOptions=class(TObject)
  private
    FPersistentOptions: string;
    FCaseSensitive: boolean;
    FIniFile: string;
    FIniFileSection: string;
    // Parameters/options to be used/parsed.
    // The associated object is used as a boolean:
    // if true, option may be saved to persistentoptions. This includes
    // options specified in @file as that is meant to function transparently
    // if false, option is temporary, e.g. display-only options or
    // options specified in an ini file.
    // Items are deleted when the GetOption.. call is made
    FParams:TStringList;
    function GetOption(shortname,name:string;var param:string;bAppendToPersistentOptions,bHasParam:boolean):boolean;
    // Load options specified in file. Should be just the same as specifying
    // the same options directly on command line
    procedure LoadFile(fname:string);
    // Loads ini file and sets parameters found in the file
    // Does not add these params to FPersistentOptions: the point is to reread the ini file each time
    // and load the options specified there
    function LoadIniFile:boolean;
    //read all params in string list, load @filename at start to let command line options override file options
    procedure LoadParams;
    // This also loads ini file
    procedure SetIniFile(AValue: string);
  public
    property CaseSensitive:boolean read FCaseSensitive write FCaseSensitive;
    // Specify inifile to load FParams from inifile
    // these parameters can be overridden by command-line parameters
    property IniFile:string read FIniFile write SetIniFile ;
    // Section name (e.g. [General]) where parameters are if using ini files
    property IniFileSection: string read FIniFileSection write FIniFileSection;
    // Raw parameters. Note that FParams will be deleted when calling GetOption
    property Params: TStringList read FParams;
    // Lists all options retrieved with AppendToPersistentOptions=true in command line arg format.
    property PersistentOptions:string read FPersistentOptions;
    // Arguments left after getting all command line parameters
    property RestArguments:TStringList read FParams;
    function GetOption(shortname,name,defaultVal:string;AppendToPersistentOptions:boolean=true):string;
    function GetOption(shortname,name:string;defaultVal:integer;AppendToPersistentOptions:boolean=true):integer;
    function GetOption(shortname,name:string;defaultVal:boolean;AppendToPersistentOptions:boolean=true):boolean;
    function GetOption(shortname,name:string;defaultVal:double;AppendToPersistentOptions:boolean=true):double;
    // Get option and do not expect any parameter added (e.g. --verbose, not --verbose=yes)
    function GetOptionNoParam(shortname,name:string;AppendToPersistentOptions:boolean=true):boolean;
    // Results non-empty if unknown parameters are left
    function ValidateOptions:string;
    // If IniFileSection specified, the @filename param will attempt to load filename as inifile first,
    // else @filename will always be interpreted as a series of command line arguments
    constructor Create(FileSection:string='');
    destructor Destroy;override;
  end;

implementation

uses inifiles;

{ TCommandLineOptions }

procedure TCommandLineOptions.LoadFile(fname: string);
var
  f:text;
  i,cnt:integer;
  cQuote:char;
  s:string;
begin
  if FileExists(fname) then
    begin
    if (FIniFileSection<>'') then // try to read it as an ini file
      begin
      FIniFile:=fname;
      if LoadIniFile then
        exit; //if errors occurred, we can try to load it as a regular file with arguments...
      end;
    cnt:=0;
    AssignFile(f,fname);
    Reset(f);
    while not eof(f) do
      begin
      readln(f,s);
      // split into parameters
      while length(s)>0 do
        begin
        i:=1;
        while i<=length(s) do
          begin
          if s[i] in ['''','"'] then
            begin
            cQuote:=s[i];
            delete(s,i,1);
            i:=i-1;
            repeat
              i:=i+1;
            until (s[i]=cQuote) or (i=length(s));
            if s[i]=cQuote then
              begin
              delete(s,i,1);
              i:=i-1;
              end;
            end
          else
            if (s[i]<=' ') then
              begin
              if i>1 then
                begin
                // Add param to begin of list
                if cnt>=FParams.Count then
                  FParams.AddObject(copy(s,1,i-1),TObject(true))
                else
                  begin
                  FParams.Insert(cnt,copy(s,1,i-1));
                  FParams.Objects[cnt]:=TObject(true);
                  end;
                cnt:=cnt+1;
                end;
              delete(s,1,i);
              i:=0;
              end;
          i:=i+1;
          end;
        if (i>length(s)) then
          begin
          // Add param at start
          if cnt>=FParams.Count then
            FParams.AddObject(copy(s,1,i-1), TObject(true))
          else
            begin
            FParams.Insert(cnt,copy(s,1,i-1));
            FParams.Objects[cnt]:=TObject(true);
            end;
          cnt:=cnt+1;
          delete(s,1,i);
          end;
        end;
      if length(s)>0 then
        FParams.AddObject(s,TObject(false));
      end;
    CloseFile(f);
    end;
end;

function TCommandLineOptions.LoadIniFile: boolean;
var
  ini:TIniFile;
  i:integer;
  sSection:string;
  SecVals:TStringList;
begin
  result:=false;
  if (FIniFile<>'') and FileExists(FIniFile) then
    begin
    ini:=TIniFile.Create(FIniFile);
    ini.StripQuotes:=true; //let ini handle e.g. lazopt="-g -gl -O1" for us
    SecVals:=TStringList.Create;
    try
      if (FIniFileSection<>'') then
        sSection:=FIniFileSection
      else
        sSection:='General';
      ini.CaseSensitive:=FCaseSensitive;
      ini.ReadSectionValues(sSection,SecVals);
      if SecVals.Count>0 then
        begin
        result:=true;
        for i:=0 to SecVals.Count-1 do
          // Ignore comments; convert rest to parameters
          if (copy(trim(SecVals[i]),1,1)<>';') and
            (copy(trim(SecVals[i]),1,1)<>'#')  then
              // Add param to begin of list
              if i>=FParams.Count then
                FParams.AddObject('--'+SecVals[i],TObject(false))
              else
                begin
                FParams.Insert(i,'--'+SecVals[i]);
                FParams.Objects[i]:=TObject(false);
                end;
        end;
    finally
      SecVals.Free;
      ini.free;
    end;
    end;
end;

procedure TCommandLineOptions.LoadParams;
var
  i:integer;
  sParam:string;
begin
  i:=1;
  while i<=Paramcount do
    begin
    sParam:=ParamStr(i);
    // First load in @file or @ file if specified
    // This lets us override with command line args later
    if sParam[1]='@' then
      begin
      if (length(sParam)=1) and (i<Paramcount) then
        begin
        i:=i+1;
        LoadFile(ParamStr(i));
        end
      else
        LoadFile(copy(ParamStr(i),2,length(ParamStr(i))));
      end;
    i:=i+1;
    end;

  // Load in normal parameters
  i:=1;
  while i<=Paramcount do
    begin
    sParam:=ParamStr(i);
    // First load in @file if specified
    // This lets us override with command line args later
    if sParam[1]<>'@' then
      FParams.AddObject(ParamStr(i),TObject(true));
    i:=i+1;
    end;
end;

procedure TCommandLineOptions.SetIniFile(AValue: string);
begin
  if FIniFile=AValue then Exit;
  FIniFile:=AValue;
  //load FParams from ini file, FParams are overriden by everything else
  // If we have problems loading the file or its contents, we should let the user know.
  // After all, he thinks/hopes the ini file exists & contains valid parameters/sections.
  // Throwing an exception is a bit drastic but short of adding an error property, it's the
  // best we can do.
  if not(LoadIniFile) then
    raise ECommandLineError.CreateFmt('Specified ini file %s could not be read or no values present',[AValue]);
end;

function TCommandLineOptions.GetOption(shortname, name, defaultVal: string;
  AppendToPersistentOptions: boolean): string;
var
  s:string='';
begin
  if GetOption(shortname, name,s,AppendToPersistentOptions,true) then
    result:=s
  else
    result:=defaultVal;
end;

function TCommandLineOptions.GetOption(shortname, name: string;
  defaultVal: integer; AppendToPersistentOptions: boolean): integer;
var
  s:string='';
begin
  if GetOption(shortname, name,s,AppendToPersistentOptions,true) then
    result:=StrToIntDef(s,defaultVal)
  else
    result:=defaultVal;
end;

function TCommandLineOptions.GetOptionNoParam(shortname, name: string;
  AppendToPersistentOptions: boolean): boolean;
var
  s:string='';
begin
  result:=GetOption(shortname, name,s,AppendToPersistentOptions,false);
end;

function TCommandLineOptions.GetOption(shortname, name: string;
  defaultVal: boolean; AppendToPersistentOptions: boolean): boolean;
var
  s:string='';
begin
  if GetOption(shortname, name,s,AppendToPersistentOptions,true) then
    result:=StrToBoolDef(s,defaultVal)
  else
    result:=defaultVal;
end;

function TCommandLineOptions.GetOption(shortname, name: string;
  defaultVal: double; AppendToPersistentOptions: boolean): double;
var
  s:string='';
begin
  if GetOption(shortname, name,s,AppendToPersistentOptions,true) then
    result:=StrToFloatDef(s,defaultVal)
  else
    result:=defaultVal;
end;

function TCommandLineOptions.ValidateOptions: string;
var i:integer;
begin
  result:='';
  for i:=0 to FParams.Count-1 do
    if FParams[i][1]='-' then
      begin
      result:=FParams[i];
      break;
      end;
end;

function TCommandLineOptions.GetOption(shortname, name: string;
  var param: string; bAppendToPersistentOptions, bHasParam: boolean): boolean;
var
  bPersistent:boolean; //add to persistent options or not
  i:integer;
  sParam,sCSParam:string;
  sCSshortname,sCSname:string;
begin
  result:=false;
  if (shortname='') and (name='') then
    exit;
  i:=0;
  if not CaseSensitive then
    begin
    sCSshortname:=UpperCase(shortname);
    sCSname:=UpperCase(name);
    end
  else
    begin
    sCSshortname:=shortname;
    sCSname:=name;
    end;
  while (i<FParams.Count) do
    begin
    sParam:=FParams[i];
    if not CaseSensitive then
      sCSParam:=UpperCase(sParam)
    else
      sCSParam:=sParam;
    if sParam[1]='-' then
      begin
      if (Length(sParam)>1) and (sParam[2]='-') then
        begin //long option
        delete(sParam,1,2);
        delete(sCSParam,1,2);
        // Check approximate match: name occurs in parameter:
        if (name<>'') and (sCSname=copy(sCSParam,1,length(name))) then
          begin
          if bHasParam and (sCSName=SCSParam) then {option names match but no = sign => no parameter}
            raise ECommandLineError.Create('Option -'+shortname+', --'+name+' needs an argument: '+ FParams[i]);
          delete(sParam,1,length(name));
          if (not bHasParam) or (bHasParam and (copy(sParam,1,1)='=')) then
            begin
            // Exact match of parameter name: now name is deleted, the = is left
            bPersistent:=(FParams.Objects[i]=TObject(True));
            FParams.delete(i);
            i:=i-1;
            param:=sParam;
            Result:=true;
            end;
          end;
        end
      else
        begin     //short option
        delete(sParam,1,1);
        delete(sCSParam,1,1);
        if (shortname<>'') and (sCSshortname=copy(sCSParam,1,length(shortname))) then
          begin
          if bHasParam and (sCSName=SCSParam) then {option names match but no = sign => no parameter}
            raise ECommandLineError.Create('Option -'+shortname+', --'+name+' needs an argument: '+ FParams[i]);
          delete(sParam,1,length(shortname));
          if (not bHasParam) or (bHasParam and (copy(sParam,1,1)='=')) then
            begin
            // Exact match of parameter name: now shortname is deleted, the = is left
            bPersistent:=(FParams.Objects[i]=TObject(True));
            FParams.delete(i);
            i:=i-1;
            param:=sParam;
            Result:=true;
            end;
          end;
        end;
      end;
    i:=i+1;
    end;
  if Result then
    begin
    if not bHasParam then
      begin
      if (param<>'') then //error, no argument for this option
        raise ECommandLineError.Create('Option -'+shortname+', --'+name+' does not allow an argument');
      if bPersistent and bAppendToPersistentOptions then
        if name<>'' then
          FPersistentOptions:=trim(FPersistentOptions+' --'+name)
        else
          FPersistentOptions:=trim(FPersistentOptions+' -'+shortname);
      end
    else
      begin //argument needed
      delete(param,1,pos('=',param));
      if bPersistent and bAppendToPersistentOptions then
        if name<>'' then
          FPersistentOptions:=trim(FPersistentOptions+' --'+name+'="'+param+'"')
        else
          FPersistentOptions:=trim(FPersistentOptions+' -'+shortname+'="'+param+'"');
      end;
    end;
end;

constructor TCommandLineOptions.Create(FileSection: string);
begin
  inherited Create;
  FParams:=TStringList.Create;
  FParams.OwnsObjects:=false;
  FIniFileSection:=FileSection;
  LoadParams;
end;

destructor TCommandLineOptions.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

end.

