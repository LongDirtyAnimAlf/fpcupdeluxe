unit processutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { TProcessEnvironment }

  TProcessEnvironment = class(tobject)
    private
      FEnvironmentList:TStringList;
      FCaseSensitive:boolean;
      function GetVarIndex(VarName:string):integer;
    public
      function GetVar(VarName:string):string;
      procedure SetVar(VarName,VarValue:string);
      property EnvironmentList:TStringList read FEnvironmentList;
      constructor Create;
      destructor Destroy; override;
    end;

implementation

{ TProcessEnvironment }

function TProcessEnvironment.GetVarIndex(VarName: string): integer;
var
  idx:integer;

  function ExtractVar(VarVal:string):string;
  begin
    result:='';
    if length(Varval)>0 then
      begin
      if VarVal[1] = '=' then //windows
        delete(VarVal,1,1);
      result:=trim(copy(VarVal,1,pos('=',VarVal)-1));
      if not FCaseSensitive then
        result:=UpperCase(result);
      end
  end;

begin
  if not FCaseSensitive then
    VarName:=UpperCase(VarName);
  idx:=0;
  while idx<FEnvironmentList.Count  do
    begin
    if VarName = ExtractVar(FEnvironmentList[idx]) then
      break;
    idx:=idx+1;
    end;
  if idx<FEnvironmentList.Count then
    result:=idx
  else
    result:=-1;
end;

function TProcessEnvironment.GetVar(VarName: string): string;
var
  idx:integer;

  function ExtractVal(VarVal:string):string;
  begin
    result:='';
    if length(Varval)>0 then
      begin
      if VarVal[1] = '=' then //windows
        delete(VarVal,1,1);
      result:=trim(copy(VarVal,pos('=',VarVal)+1,length(VarVal)));
      end
  end;

begin
  idx:=GetVarIndex(VarName);
  if idx>0 then
    result:=ExtractVal(FEnvironmentList[idx])
  else
    result:='';
end;

procedure TProcessEnvironment.SetVar(VarName, VarValue: string);
var
  idx:integer;
  s:string;
begin
  idx:=GetVarIndex(VarName);
  s:=trim(Varname)+'='+trim(VarValue);
  if idx>0 then
    FEnvironmentList[idx]:=s
  else
    FEnvironmentList.Add(s);
end;

constructor TProcessEnvironment.Create;
var
  i: integer;
begin
  FEnvironmentList:=TStringList.Create;
  {$ifdef WINDOWS}
  FCaseSensitive:=false;
  {$else}
  FCaseSensitive:=true;
  {$endif WINDOWS}
  // GetEnvironmentVariableCount is 1 based
  for i:=1 to GetEnvironmentVariableCount do
    EnvironmentList.Add(trim(GetEnvironmentString(i)));
end;

destructor TProcessEnvironment.Destroy;
begin
  FEnvironmentList.Free;
  inherited Destroy;
end;

end.

