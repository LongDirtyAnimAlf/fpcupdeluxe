unit cpucount;

{ Uses NumCPULib Library }
{ Copyright (c) 2019 Ugochukwu Mmaduekwe }
{ Github Repository https://github.com/Xor-el }

interface

function GetLogicalCpuCount: Integer;

implementation

uses
  NumCPULib  in './numcpulib/NumCPULib.pas';

function GetLogicalCpuCount: integer;
begin
  result:=TNumCPULib.GetLogicalCPUCount();
end;

end.
