unit cpucount;
// Source:
// http://wiki.lazarus.freepascal.org/Example_of_multi-threaded_application:_array_of_threads#1._Detect_number_of_cores_available.

interface
// Returns number of cores: a computer with two hyperthreaded cores will report 4
function GetLogicalCpuCount: Integer;

implementation

{$IFDEF Linux}
uses ctypes;

const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}
{$IF defined(windows)}
uses windows;
{$endif}
{$IF defined(darwin) OR defined(freebsd)}
uses ctypes, sysctl;
{$endif}

function GetLogicalCpuCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
// returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(solaris)}
//untested
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
  end;
{$ELSE}
  // Fallback for other platforms
  begin
    Result:=1;
  end;
{$ENDIF}
end.
