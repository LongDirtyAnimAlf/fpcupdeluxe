{ *********************************************************************************** }
{ *                              NumCPULib Library                                  * }
{ *                Copyright (c) 2019 Ugochukwu Mmaduekwe                           * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit NumCPULib;

{$DEFINE DELPHI}

{$IFDEF FPC}
{$UNDEF DELPHI}
{$MODE DELPHI}

// Disable Hints.
{$HINTS OFF}

{$IFDEF CPU386}
   {$DEFINE NUMCPULIB_X86}
{$ENDIF}

{$IFDEF CPUX64}
   {$DEFINE NUMCPULIB_X86_64}
{$ENDIF}

{$IFDEF CPUARM}
   {$DEFINE NUMCPULIB_ARM}
{$ENDIF}

{$IFDEF CPUAARCH64}
   {$DEFINE NUMCPULIB_AARCH64}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_ARM) OR DEFINED(NUMCPULIB_AARCH64)}
   {$DEFINE NUMCPULIB_ARMCPU}
{$IFEND}

{$IFDEF IPHONESIM}
   {$DEFINE NUMCPULIB_IOSSIM}
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
   {$DEFINE NUMCPULIB_MSWINDOWS}
{$ELSEIF DEFINED(UNIX)}
   {$DEFINE NUMCPULIB_UNIX}
   {$IF DEFINED(BSD)}
      {$IF DEFINED(DARWIN)}
         {$DEFINE NUMCPULIB_APPLE}
         {$IF DEFINED(NUMCPULIB_ARM) OR DEFINED(NUMCPULIB_AARCH64)}
            {$DEFINE NUMCPULIB_IOS}
         {$ELSE}
            {$DEFINE NUMCPULIB_MACOS}
         {$IFEND}
      {$ELSEIF DEFINED(FREEBSD) OR DEFINED(NETBSD) OR DEFINED(OPENBSD) OR DEFINED(DRAGONFLY)}
         {$DEFINE NUMCPULIB_GENERIC_BSD}
      {$IFEND}
  {$ELSEIF DEFINED(ANDROID)}
     {$DEFINE NUMCPULIB_ANDROID}
  {$ELSEIF DEFINED(LINUX)}
     {$DEFINE NUMCPULIB_LINUX}
  {$ELSEIF DEFINED(SOLARIS)}
     {$DEFINE NUMCPULIB_SOLARIS}
  {$ELSE}
     {$DEFINE NUMCPULIB_UNDEFINED_UNIX_VARIANTS}
  {$IFEND}
{$ELSE}
   {$MESSAGE ERROR 'UNSUPPORTED TARGET.'}
{$IFEND}

{$IFDEF NUMCPULIB_ANDROID}
   {$DEFINE NUMCPULIB_LINUX}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_GENERIC_BSD) OR DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCTL}
{$IFEND}

{$IF DEFINED(NUMCPULIB_LINUX) OR DEFINED(NUMCPULIB_GENERIC_BSD) OR DEFINED(NUMCPULIB_SOLARIS) OR DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCONF}
{$IFEND}

{$IF DEFINED(NUMCPULIB_LINUX) OR DEFINED(NUMCPULIB_SOLARIS)}
   {$DEFINE NUMCPULIB_WILL_PARSE_DATA}
{$IFEND}

{$ENDIF FPC}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

{$IFDEF DELPHI}

 // XE3 and Above
{$IF CompilerVersion >= 24.0}
   {$DEFINE DELPHIXE3_UP}
   {$LEGACYIFEND ON}
   {$ZEROBASEDSTRINGS OFF}
{$IFEND}

{$IFDEF CPU386}
   {$DEFINE NUMCPULIB_X86}
{$ENDIF}

{$IFDEF CPUX64}
   {$DEFINE NUMCPULIB_X86_64}
{$ENDIF}

{$IFDEF CPUARM32}
   {$DEFINE NUMCPULIB_ARM}
{$ENDIF}

{$IFDEF CPUARM64}
   {$DEFINE NUMCPULIB_AARCH64}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_ARM) OR DEFINED(NUMCPULIB_AARCH64)}
   {$DEFINE NUMCPULIB_ARMCPU}
{$IFEND}

{$IFDEF IOS}
  {$IFNDEF CPUARM}
     {$DEFINE NUMCPULIB_IOSSIM}
  {$ENDIF}
{$ENDIF}

{$IFDEF IOS}
   {$DEFINE NUMCPULIB_IOS}
{$ENDIF}

{$IFDEF MSWINDOWS}
   {$DEFINE NUMCPULIB_MSWINDOWS}
{$ENDIF}

{$IFDEF MACOS}
   {$IFNDEF IOS}
      {$DEFINE NUMCPULIB_MACOS}
   {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
   {$DEFINE NUMCPULIB_ANDROID}
{$ENDIF}

{$IF DEFINED(NUMCPULIB_IOS) OR DEFINED(NUMCPULIB_MACOS)}
   {$DEFINE NUMCPULIB_APPLE}
{$IFEND}

{$IF DEFINED(LINUX) OR DEFINED(NUMCPULIB_ANDROID)}
   {$DEFINE NUMCPULIB_LINUX}
{$IFEND}

{$IF DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCTL}
{$IFEND}

{$IF DEFINED(NUMCPULIB_LINUX) OR DEFINED(NUMCPULIB_APPLE)}
   {$DEFINE NUMCPULIB_HAS_SYSCONF}
{$IFEND}

{$IF DEFINED(NUMCPULIB_MSWINDOWS)}
// XE2 and Above
   {$IF CompilerVersion >= 23.0}
      {$DEFINE DELPHIXE2_UP}
      {$DEFINE HAS_GET_LOGICAL_PROCESSOR_INFORMATION_INBUILT}
   {$IFEND}
{$IFEND}

{$IFDEF NUMCPULIB_LINUX}
   {$DEFINE NUMCPULIB_WILL_PARSE_DATA}
{$ENDIF}

{$ENDIF DELPHI}

interface

uses
{$IFDEF NUMCPULIB_MSWINDOWS}
  Windows,
{$ENDIF} // ENDIF NUMCPULIB_MSWINDOWS
  // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCONF}
{$IFDEF FPC}
  unixtype,
{$ELSE}
  Posix.Unistd,
{$ENDIF}   // ENDIF FPC
{$ENDIF}  // ENDIF NUMCPULIB_HAS_SYSCONF
  // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCTL}
{$IFDEF FPC}
  sysctl,
{$ELSE}
  Posix.SysTypes,
  Posix.SysSysctl,
{$ENDIF}   // ENDIF FPC
{$ENDIF} // ENDIF NUMCPULIB_HAS_SYSCTL
  // ================================================================//
{$IFDEF NUMCPULIB_APPLE}
{$IFDEF NUMCPULIB_MACOS}
{$IFDEF FPC}
  CocoaAll,
{$ELSE}
  Macapi.AppKit,
{$ENDIF} // ENDIF FPC
{$ENDIF} // ENDIF NUMCPULIB_MACOS
{$ENDIF}   // ENDIF NUMCPULIB_APPLE
  // ================================================================//
{$IFDEF NUMCPULIB_WILL_PARSE_DATA}
{$IFDEF NUMCPULIB_SOLARIS}
  Process,
{$ENDIF} // ENDIF NUMCPULIB_SOLARIS
  Classes,
  StrUtils,
{$ENDIF} // ENDIF NUMCPULIB_WILL_PARSE_DATA

  // ================================================================//
  SysUtils;

type
  /// <summary>
  /// <para>
  /// A class with utilities to determine the number of CPUs available on
  /// the current system.
  /// </para>
  /// <para>
  /// This information can be used as a guide to how many tasks can be
  /// run in parallel.
  /// </para>
  /// <para>
  /// There are many properties of the system architecture that will
  /// affect parallelism, for example memory access speeds (for all the
  /// caches and RAM) and the physical architecture of the processor, so
  /// the number of CPUs should be used as a rough guide only.
  /// </para>
  /// </summary>
  TNumCPULib = class sealed(TObject)

  strict private

    // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCONF}
    class function GetAppropriateSysConfNumber(): Int32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCTL}
{$IFDEF NUMCPULIB_APPLE}
    class function GetCPUCountUsingSysCtlByName(const AName: String)
      : UInt32; static;
{$ENDIF}
    class function GetLogicalCPUCountUsingSysCtl(): UInt32; static;
{$ENDIF}
    // ================================================================//
{$IFDEF NUMCPULIB_MSWINDOWS}

  const
    KERNEL32 = 'kernel32.dll';

{$IFNDEF HAS_GET_LOGICAL_PROCESSOR_INFORMATION_INBUILT}

  type

    TLogicalProcessorRelationship = (RelationProcessorCore = 0,
      RelationNumaNode = 1, RelationCache = 2, RelationProcessorPackage = 3,
      RelationGroup = 4, RelationAll = $FFFF);
    TProcessorCacheType = (CacheUnified, CacheInstruction, CacheData,
      CacheTrace);

    TCacheDescriptor = record
      Level: Byte;
      Associativity: Byte;
      LineSize: Word;
      Size: DWORD;
      pcType: TProcessorCacheType;
    end;

    PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

    TSystemLogicalProcessorInformation = record
      ProcessorMask: ULONG_PTR;
      Relationship: TLogicalProcessorRelationship;
      case Int32 of
        0:
          (Flags: Byte);
        1:
          (NodeNumber: DWORD);
        2:
          (Cache: TCacheDescriptor);
        3:
          (Reserved: array [0 .. 1] of ULONGLONG);
    end;

    KAffinity = NativeUInt;

    TGroupAffinity = record
      Mask: KAffinity;
      Group: Word;
      Reserved: array [0 .. 2] of Word;
    end;

    TProcessorRelationship = record
      Flags: Byte;
      Reserved: array [0 .. 20] of Byte;
      GroupCount: Word;
      GroupMask: array [0 .. 0] of TGroupAffinity;
    end;

    TNumaNodeRelationship = record
      NodeNumber: DWORD;
      Reserved: array [0 .. 19] of Byte;
      GroupMask: TGroupAffinity;
    end;

    TCacheRelationship = record
      Level: Byte;
      Associativity: Byte;
      LineSize: Word;
      CacheSize: DWORD;
      _Type: TProcessorCacheType;
      Reserved: array [0 .. 19] of Byte;
      GroupMask: TGroupAffinity;
    end;

    TProcessorGroupInfo = record
      MaximumProcessorCount: Byte;
      ActiveProcessorCount: Byte;
      Reserved: array [0 .. 37] of Byte;
      ActiveProcessorMask: KAffinity;
    end;

    TGroupRelationship = record
      MaximumGroupCount: Word;
      ActiveGroupCount: Word;
      Reserved: array [0 .. 19] of Byte;
      GroupInfo: array [0 .. 0] of TProcessorGroupInfo;
    end;

    PSystemLogicalProcessorInformationEx = ^
      TSystemLogicalProcessorInformationEx;

    TSystemLogicalProcessorInformationEx = record
      Relationship: TLogicalProcessorRelationship;
      Size: DWORD;
      case Int32 of
        0:
          (Processor: TProcessorRelationship);
        1:
          (NumaNode: TNumaNodeRelationship);
        2:
          (Cache: TCacheRelationship);
        3:
          (Group: TGroupRelationship);
    end;

{$ENDIF}

    // ================================================================//

  type
    TGetLogicalProcessorInformation = function(Buffer:
{$IFNDEF HAS_GET_LOGICAL_PROCESSOR_INFORMATION_INBUILT}TNumCPULib.{$ENDIF}PSystemLogicalProcessorInformation; var ReturnLength: DWORD): BOOL; stdcall;

    TGetLogicalProcessorInformationEx = function(RelationshipType
      : TLogicalProcessorRelationship; Buffer:
{$IFNDEF HAS_GET_LOGICAL_PROCESSOR_INFORMATION_INBUILT}TNumCPULib.{$ENDIF}PSystemLogicalProcessorInformationEx; var ReturnLength: DWORD): BOOL; stdcall;

  class var

    FIsGetLogicalProcessorInformationAvailable,
      FIsGetLogicalProcessorInformationAvailableEx: Boolean;
    FGetLogicalProcessorInformation: TGetLogicalProcessorInformation;
    FGetLogicalProcessorInformationEx: TGetLogicalProcessorInformationEx;


    // ================================================================//

  type
    TProcessorInformation = record
      LogicalProcessorCount: UInt32;
      ProcessorCoreCount: UInt32;
    end;

  type
    TProcessorInformationEx = record
      LogicalProcessorCount: UInt32;
      ProcessorCoreCount: UInt32;
    end;

    // ================================================================//
  class function GetProcedureAddress(ModuleHandle: THandle;
    const AProcedureName: String; var AFunctionFound: Boolean): Pointer; static;
  class function IsGetLogicalProcessorInformationAvailable(): Boolean; static;
  class function IsGetLogicalProcessorInformationExAvailable(): Boolean; static;
  class function CountSetBits(ABitMask: NativeUInt): UInt32; static;
  class function GetProcessorInfo(): TProcessorInformation; static;
  class function GetProcessorInfoEx(): TProcessorInformationEx; static;

  class function GetLogicalCPUCountWindows(): UInt32; static;
  class function GetPhysicalCPUCountWindows(): UInt32; static;
{$ENDIF}
  // ================================================================//
{$IFDEF NUMCPULIB_APPLE}
  class function GetLogicalCPUCountApple(): UInt32; static;
  class function GetPhysicalCPUCountApple(): UInt32; static;
{$ENDIF}
  // ================================================================//
{$IFDEF NUMCPULIB_WILL_PARSE_DATA}

  type
    TNumCPULibStringArray = array of String;

  class function SplitString(const AInputString: String; ADelimiter: Char)
    : TNumCPULibStringArray; static;

  class function ParseLastString(const AInputString: String): String; static;
  class function ParseLastInt32(const AInputString: String; ADefault: Int32)
    : Int32; static;

  class function BeginsWith(const AInputString, ASubString: string;
    AIgnoreCase: Boolean; AOffset: Int32 = 1): Boolean; static;
{$ENDIF}
  // ================================================================//
{$IFDEF NUMCPULIB_LINUX}

  type
    TLogicalProcessor = record
    private
    var
      ProcessorNumber, PhysicalProcessorNumber, PhysicalPackageNumber: UInt32;
    public
      class function Create(AProcessorNumber, APhysicalProcessorNumber,
        APhysicalPackageNumber: UInt32): TLogicalProcessor; static;
    end;

  class procedure ReadFileContents(const AFilePath: String;
    var AOutputParameters: TStringList); static;
  class function GetLogicalCPUCountLinux(): UInt32; static;
  class function GetPhysicalCPUCountLinux(): UInt32; static;
{$ENDIF}
  // ================================================================//
{$IFDEF NUMCPULIB_SOLARIS}
  class procedure ExecuteAndParseProcessOutput(const ACallingProcess: String;
    AInputParameters: TStringList; var AOutputParameters: TStringList);
  class function GetLogicalCPUCountSolaris(): UInt32; static;
  class function GetPhysicalCPUCountSolaris(): UInt32; static;
{$ENDIF}
  // ================================================================//
{$IFDEF NUMCPULIB_GENERIC_BSD}
  class function GetLogicalCPUCountGenericBSD(): UInt32; static;
{$ENDIF}
  // ================================================================//

  class procedure Boot(); static;
  class constructor NumCPULib();

  public

    /// <summary>
    /// This function will get the number of logical cores. Sometimes this is
    /// different from the number of physical cores.
    /// </summary>
    class function GetLogicalCPUCount(): UInt32; static;

    /// <summary>
    /// This function will get the number of physical cores.
    /// </summary>
    class function GetPhysicalCPUCount(): UInt32; static;
  end;

{$IFDEF NUMCPULIB_HAS_SYSCONF}
{$IFDEF FPC}

function sysconf(i: cint): clong; cdecl; external 'c' name 'sysconf';
{$ENDIF}
{$ENDIF}

implementation

{ TNumCPULib }

class procedure TNumCPULib.Boot();
begin
{$IFDEF NUMCPULIB_MSWINDOWS}
  FIsGetLogicalProcessorInformationAvailable :=
    IsGetLogicalProcessorInformationAvailable();
  FIsGetLogicalProcessorInformationAvailableEx :=
    IsGetLogicalProcessorInformationExAvailable();
{$ENDIF}
end;

class constructor TNumCPULib.NumCPULib;
begin
  TNumCPULib.Boot();
end;

// ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCONF}

class function TNumCPULib.GetAppropriateSysConfNumber(): Int32;
begin
  // On ARM targets, processors could be turned off to save power So we
  // use `_SC_NPROCESSORS_CONF` to get the real number.
  // ****************************************************************//
  // NUMCPULIB_LINUX
{$IFDEF NUMCPULIB_LINUX}
{$IFDEF NUMCPULIB_ARMCPU}
{$IFDEF NUMCPULIB_ANDROID}
  Result := 96; // _SC_NPROCESSORS_CONF
{$ELSE}
  // Devices like RPI
  Result := 83; // _SC_NPROCESSORS_CONF
{$ENDIF}
{$ELSE}
  // for non ARM Linux like CPU's
{$IFDEF NUMCPULIB_ANDROID}
  Result := 97; // _SC_NPROCESSORS_ONLN
{$ELSE}
  Result := 84; // _SC_NPROCESSORS_ONLN
{$ENDIF}  // ENDIF NUMCPULIB_ANDROID

{$ENDIF}  // ENDIF NUMCPULIB_ARMCPU
{$ENDIF} // ENDIF NUMCPULIB_LINUX
  // ****************************************************************//
  // NUMCPULIB_GENERIC_BSD
{$IFDEF NUMCPULIB_GENERIC_BSD}
{$IF DEFINED(FREEBSD) OR DEFINED(DRAGONFLY)}
  Result := 58; // _SC_NPROCESSORS_ONLN
{$IFEND}
{$IFDEF OPENBSD}
  Result := 503; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$IFDEF NETBSD}
  Result := 1002; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$ENDIF} // ENDIF NUMCPULIB_GENERIC_BSD
  // ****************************************************************//
  // NUMCPULIB_SOLARIS
{$IFDEF NUMCPULIB_SOLARIS}
{$IFDEF NUMCPULIB_ARMCPU}
  Result := 14; // _SC_NPROCESSORS_CONF
{$ELSE}
  Result := 15; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$ENDIF}  // ENDIF NUMCPULIB_SOLARIS
  // ****************************************************************//
  // NUMCPULIB_APPLE
{$IFDEF NUMCPULIB_APPLE}
{$IFDEF NUMCPULIB_ARMCPU}
  Result := 57; // _SC_NPROCESSORS_CONF
{$ELSE}
  Result := 58; // _SC_NPROCESSORS_ONLN
{$ENDIF}
{$ENDIF}  // ENDIF NUMCPULIB_APPLE
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_HAS_SYSCTL}
{$IFDEF NUMCPULIB_APPLE}

class function TNumCPULib.GetCPUCountUsingSysCtlByName
  (const AName: String): UInt32;
var
  LLen: size_t;
begin
  LLen := System.SizeOf(Result);
{$IFDEF FPC}
  fpsysctlbyname(PChar(AName), @Result, @LLen, nil, 0);
{$ELSE}
  SysCtlByName(PAnsiChar(AName), @Result, @LLen, nil, 0);
{$ENDIF}
end;
{$ENDIF}

class function TNumCPULib.GetLogicalCPUCountUsingSysCtl(): UInt32;
var
  LMib: array [0 .. 1] of Int32;
  LLen: size_t;
begin
  LMib[0] := CTL_HW;
  LMib[1] := HW_NCPU;
  LLen := System.SizeOf(Result);
{$IFDEF FPC}
{$IF DEFINED(VER3_0_0) OR DEFINED(VER3_0_2)}
  fpsysctl(PChar(@LMib), 2, @Result, @LLen, nil, 0);
{$ELSE}
  fpsysctl(@LMib, 2, @Result, @LLen, nil, 0);
{$IFEND}
{$ELSE}
  sysctl(@LMib, 2, @Result, @LLen, nil, 0);
{$ENDIF}
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_MSWINDOWS}

class function TNumCPULib.CountSetBits(ABitMask: NativeUInt): UInt32;
var
  LShift, LIdx: UInt32;
  LBitTest: NativeUInt;
begin
  LShift := (System.SizeOf(NativeUInt) * 8) - 1;
  Result := 0;
  LBitTest := NativeUInt(1) shl LShift;
  LIdx := 0;
  while LIdx <= LShift do
  begin
    if (ABitMask and LBitTest) <> 0 then
    begin
      System.Inc(Result);
    end;
    LBitTest := LBitTest shr 1;
    System.Inc(LIdx);
  end;
end;

class function TNumCPULib.GetProcessorInfo(): TProcessorInformation;
var
  LReturnLength: DWORD;
  LProcInfo, LCurrentInfo: PSystemLogicalProcessorInformation;
begin
  LReturnLength := 0;
  Result := Default (TProcessorInformation);

  FGetLogicalProcessorInformation(nil, LReturnLength);
  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
  begin
    RaiseLastOSError;
  end
  else
  begin
    System.GetMem(LProcInfo, LReturnLength);
    try
      if not FGetLogicalProcessorInformation(LProcInfo, LReturnLength) then
      begin
        RaiseLastOSError;
      end
      else
      begin
        LCurrentInfo := LProcInfo;
        while (NativeUInt(LCurrentInfo) - NativeUInt(LProcInfo)) <
          LReturnLength do
        begin
          case LCurrentInfo.Relationship of
            RelationProcessorCore:
              begin
                System.Inc(Result.ProcessorCoreCount);
                Result.LogicalProcessorCount := Result.LogicalProcessorCount +
                  CountSetBits(LCurrentInfo.ProcessorMask);
              end;
          end;
          LCurrentInfo := PSystemLogicalProcessorInformation
            (NativeUInt(LCurrentInfo) +
            System.SizeOf(TSystemLogicalProcessorInformation));
        end;
      end;
    finally
      System.FreeMem(LProcInfo);
    end;
  end;
end;

class function TNumCPULib.GetProcessorInfoEx: TProcessorInformationEx;
var
  LReturnLength: DWORD;
  LProcInfo, LCurrentInfo: PSystemLogicalProcessorInformationEx;
  LIdx: Int32;
begin
  LReturnLength := 0;
  Result := Default (TProcessorInformationEx);

  FGetLogicalProcessorInformationEx(RelationAll, nil, LReturnLength);
  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
  begin
    RaiseLastOSError;
  end
  else
  begin
    System.GetMem(LProcInfo, LReturnLength);
    try
      if not FGetLogicalProcessorInformationEx(RelationAll, LProcInfo,
        LReturnLength) then
      begin
        RaiseLastOSError;
      end
      else
      begin
        LCurrentInfo := LProcInfo;
        while (NativeUInt(LCurrentInfo) - NativeUInt(LProcInfo)) <
          LReturnLength do
        begin
          case LCurrentInfo.Relationship of
            RelationProcessorCore:
              begin
                System.Inc(Result.ProcessorCoreCount);
                for LIdx := 0 to System.Pred
                  (LCurrentInfo.Processor.GroupCount) do
                begin
                  Result.LogicalProcessorCount := Result.LogicalProcessorCount +
                    CountSetBits(LCurrentInfo.Processor.GroupMask[LIdx].Mask);
                end;
              end;
          end;
          LCurrentInfo := PSystemLogicalProcessorInformationEx
            (NativeUInt(LCurrentInfo) + LCurrentInfo.Size);
        end;
      end;
    finally
      System.FreeMem(LProcInfo);
    end;
  end;
end;

class function TNumCPULib.GetLogicalCPUCountWindows(): UInt32;
var
  LIdx: Int32;
  LProcessAffinityMask, LSystemAffinityMask: DWORD_PTR;
  LMask: DWORD;
  LSystemInfo: SYSTEM_INFO;
  LProcInfo: TProcessorInformation;
  LProcInfoEx: TProcessorInformationEx;
begin
  if IsGetLogicalProcessorInformationExAvailable then
  begin
    LProcInfoEx := GetProcessorInfoEx;
    Result := LProcInfoEx.LogicalProcessorCount;
    Exit;
  end;
  if IsGetLogicalProcessorInformationAvailable then
  begin
    LProcInfo := GetProcessorInfo;
    Result := LProcInfo.LogicalProcessorCount;
    Exit;
  end;
  // fallback if non of the above are available
  // returns total number of processors available to system including logical hyperthreaded processors
  LProcessAffinityMask := 0;
  LSystemAffinityMask := 0;
  if GetProcessAffinityMask(GetCurrentProcess, LProcessAffinityMask,
    LSystemAffinityMask) then
  begin
    Result := 0;
    for LIdx := 0 to 31 do
    begin
      LMask := DWORD(1) shl LIdx;
      if (LProcessAffinityMask and LMask) <> 0 then
      begin
        System.Inc(Result);
      end;
    end;
  end
  else
  begin
    // can't get the affinity mask so we just report the total number of processors
    LSystemInfo := Default (SYSTEM_INFO);
    GetSystemInfo(LSystemInfo);
    Result := LSystemInfo.dwNumberOfProcessors;
  end;
end;

class function TNumCPULib.GetPhysicalCPUCountWindows(): UInt32;
var
  LProcInfo: TProcessorInformation;
  LProcInfoEx: TProcessorInformationEx;
begin
  Result := 0;
  if IsGetLogicalProcessorInformationExAvailable then
  begin
    LProcInfoEx := GetProcessorInfoEx;
    Result := LProcInfoEx.ProcessorCoreCount;
    Exit;
  end;
  if IsGetLogicalProcessorInformationAvailable then
  begin
    LProcInfo := GetProcessorInfo;
    Result := LProcInfo.ProcessorCoreCount;
    Exit;
  end;
end;

class function TNumCPULib.GetProcedureAddress(ModuleHandle: THandle;
  const AProcedureName: String; var AFunctionFound: Boolean): Pointer;
begin
  Result := GetProcAddress(ModuleHandle, PChar(AProcedureName));
  if Result = Nil then
  begin
    AFunctionFound := False;
  end;
end;

class function TNumCPULib.IsGetLogicalProcessorInformationAvailable(): Boolean;
var
  ModuleHandle: THandle;
begin
  Result := False;
  ModuleHandle := SafeLoadLibrary(KERNEL32, SEM_FAILCRITICALERRORS);
  if ModuleHandle <> 0 then
  begin
    Result := True;
    FGetLogicalProcessorInformation := GetProcedureAddress(ModuleHandle,
      'GetLogicalProcessorInformation', Result);
  end;
end;

class function TNumCPULib.IsGetLogicalProcessorInformationExAvailable: Boolean;
var
  ModuleHandle: THandle;
begin
  Result := False;
  ModuleHandle := SafeLoadLibrary(KERNEL32, SEM_FAILCRITICALERRORS);
  if ModuleHandle <> 0 then
  begin
    Result := True;
    FGetLogicalProcessorInformationEx := GetProcedureAddress(ModuleHandle,
      'GetLogicalProcessorInformationEx', Result);
  end;
end;

{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_APPLE}

class function TNumCPULib.GetLogicalCPUCountApple(): UInt32;
var
  LTempRes: Int32;
begin
{$IF DEFINED(NUMCPULIB_MACOS)}
  // >= (Mac OS X 10.4+)
  if NSAppKitVersionNumber >= 824 then // NSAppKitVersionNumber10_4
  begin
    LTempRes := sysconf(GetAppropriateSysConfNumber());
  end
  else
  begin
    // fallback for when sysconf API is not available
    LTempRes := Int32(GetLogicalCPUCountUsingSysCtl());
  end;
{$ELSE}
  LTempRes := sysconf(GetAppropriateSysConfNumber());
{$IFEND}
  // final fallback if all above fails
  if LTempRes < 1 then
  begin
    Result := GetCPUCountUsingSysCtlByName('hw.logicalcpu');
  end
  else
  begin
    Result := UInt32(LTempRes);
  end;
end;

class function TNumCPULib.GetPhysicalCPUCountApple(): UInt32;
begin
  Result := GetCPUCountUsingSysCtlByName('hw.physicalcpu');
end;
{$ENDIF}
// ================================================================//

{$IFDEF NUMCPULIB_WILL_PARSE_DATA}

class function TNumCPULib.SplitString(const AInputString: String;
  ADelimiter: Char): TNumCPULibStringArray;
var
  LPosStart, LPosDel, LSplitPoints, LIdx, LLowIndex, LHighIndex, LLength: Int32;
begin
  Result := Nil;
  if AInputString <> '' then
  begin
    { Determine the length of the resulting array }
    LLowIndex := 1;
    LHighIndex := System.Length(AInputString);
    LSplitPoints := 0;
    for LIdx := LLowIndex to LHighIndex do
    begin
      if (ADelimiter = AInputString[LIdx]) then
      begin
        System.Inc(LSplitPoints);
      end;
    end;

    System.SetLength(Result, LSplitPoints + 1);

    { Split the string and fill the resulting array }

    LIdx := 0;
    LLength := System.Length(ADelimiter);
    LPosStart := 1;
    LPosDel := System.Pos(ADelimiter, AInputString);
    while LPosDel > 0 do
    begin
      Result[LIdx] := System.Copy(AInputString, LPosStart, LPosDel - LPosStart);
      LPosStart := LPosDel + LLength;
      LPosDel := PosEx(ADelimiter, AInputString, LPosStart);
      System.Inc(LIdx);
    end;
    Result[LIdx] := System.Copy(AInputString, LPosStart,
      System.Length(AInputString));
  end;
end;

class function TNumCPULib.ParseLastString(const AInputString: String): String;
var
  LSplitResult: TNumCPULibStringArray;
begin
  LSplitResult := SplitString(AInputString, ' ');
  if (System.Length(LSplitResult) < 1) then
  begin
    Result := Trim(AInputString);
  end
  else
  begin
    Result := Trim(LSplitResult[System.Length(LSplitResult) - 1]);
  end;
end;

class function TNumCPULib.ParseLastInt32(const AInputString: String;
  ADefault: Int32): Int32;
var
  LLocalString: String;
begin
  LLocalString := ParseLastString(AInputString);
  if BeginsWith(LowerCase(LLocalString), '0x', False) then
  begin
    Result := StrToIntDef(StringReplace(LLocalString, '0x', '$',
      [rfReplaceAll, rfIgnoreCase]), ADefault);
  end
  else
  begin
    Result := StrToIntDef(LLocalString, ADefault);
  end;
end;

class function TNumCPULib.BeginsWith(const AInputString, ASubString: String;
  AIgnoreCase: Boolean; AOffset: Int32): Boolean;
var
  LIdx: Int32;
  LPtrInputString, LPtrSubString: PChar;
begin
  LIdx := System.Length(ASubString);
  Result := LIdx > 0;
  LPtrInputString := PChar(AInputString);
  System.Inc(LPtrInputString, AOffset - 1);
  LPtrSubString := PChar(ASubString);
  if Result then
  begin
    if AIgnoreCase then
    begin
      Result := StrLiComp(LPtrSubString, LPtrInputString, LIdx) = 0
    end
    else
    begin
      Result := StrLComp(LPtrSubString, LPtrInputString, LIdx) = 0
    end;
  end;
end;
{$ENDIF}
// ================================================================//

{$IFDEF NUMCPULIB_LINUX}

class function TNumCPULib.TLogicalProcessor.Create(AProcessorNumber,
  APhysicalProcessorNumber, APhysicalPackageNumber: UInt32): TLogicalProcessor;
begin
  Result := Default (TLogicalProcessor);
  Result.ProcessorNumber := AProcessorNumber;
  Result.PhysicalProcessorNumber := APhysicalProcessorNumber;
  Result.PhysicalPackageNumber := APhysicalPackageNumber;
end;

class procedure TNumCPULib.ReadFileContents(const AFilePath: String;
  var AOutputParameters: TStringList);
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  LOutputStream: TStream;
  LFileStream: TFileStream;
  LBytesRead: LongInt;
  LBuffer: array [0 .. BUF_SIZE - 1] of Byte;
begin
  if SysUtils.FileExists(AFilePath) then
  begin
    LFileStream := TFileStream.Create(AFilePath, fmOpenRead);
    try
      LOutputStream := TMemoryStream.Create;
      try
        // All data from file is read in a loop until no more data is available
        repeat
          // Get the new data from the file to a maximum of the LBuffer size that was allocated.
          // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
          LBytesRead := LFileStream.Read(LBuffer, BUF_SIZE);

          // Add the bytes that were read to the stream for later usage
          LOutputStream.Write(LBuffer, LBytesRead)

        until LBytesRead = 0; // Stop if no more data is available

        // Required to make sure all data is copied from the start
        LOutputStream.Position := 0;
        AOutputParameters.LoadFromStream(LOutputStream);
      finally
        LOutputStream.Free;
      end;
    finally
      LFileStream.Free;
    end;
  end;
end;

class function TNumCPULib.GetLogicalCPUCountLinux(): UInt32;
begin
  Result := UInt32(sysconf(GetAppropriateSysConfNumber()));
end;

class function TNumCPULib.GetPhysicalCPUCountLinux(): UInt32;
var
  LProcCpuInfos, LPhysicalProcessorsDetails: TStringList;
  LIdx, LJIdx, LLogicalProcessorsIdx: Int32;
  LCurrentProcessor, LCurrentCore, LCurrentPackage: UInt32;
  LFirst: Boolean;
  LLogicalProcessors: array of TLogicalProcessor;
  LogicalProcessor: TLogicalProcessor;
  LLineProcCpuInfo: String;
begin
  LProcCpuInfos := TStringList.Create();
  LCurrentProcessor := 0;
  LCurrentCore := 0;
  LCurrentPackage := 0;
  LFirst := True;
  try
    ReadFileContents('/proc/cpuinfo', LProcCpuInfos);
    System.SetLength(LLogicalProcessors, LProcCpuInfos.Count);
    // allocate enough space
    LLogicalProcessorsIdx := 0;
    for LIdx := 0 to System.Pred(LProcCpuInfos.Count) do
    begin
      // Count logical processors
      LLineProcCpuInfo := LProcCpuInfos[LIdx];
      if (BeginsWith(LLineProcCpuInfo, 'processor', False)) then
      begin
        if (not LFirst) then
        begin
          LLogicalProcessors[LLogicalProcessorsIdx] :=
            TLogicalProcessor.Create(LCurrentProcessor, LCurrentCore,
            LCurrentPackage);
          System.Inc(LLogicalProcessorsIdx);
        end
        else
        begin
          LFirst := False;
        end;
        LCurrentProcessor := UInt32(ParseLastInt32(LLineProcCpuInfo, 0));
      end
      else if (BeginsWith(LLineProcCpuInfo, 'core id', False) or
        BeginsWith(LLineProcCpuInfo, 'cpu number', False)) then
      begin
        // Count unique combinations of core id and physical id.
        LCurrentCore := UInt32(ParseLastInt32(LLineProcCpuInfo, 0));
      end
      else if (BeginsWith(LLineProcCpuInfo, 'physical id', False)) then
      begin
        LCurrentPackage := UInt32(ParseLastInt32(LLineProcCpuInfo, 0));
      end;
    end;

    LLogicalProcessors[LLogicalProcessorsIdx] :=
      TLogicalProcessor.Create(LCurrentProcessor, LCurrentCore,
      LCurrentPackage);
    System.Inc(LLogicalProcessorsIdx);
    // reduce to used size
    System.SetLength(LLogicalProcessors, LLogicalProcessorsIdx);
    LPhysicalProcessorsDetails := TStringList.Create();
    LPhysicalProcessorsDetails.Sorted := True;
    LPhysicalProcessorsDetails.Duplicates := dupIgnore;
    try
      for LJIdx := 0 to System.Pred(System.Length(LLogicalProcessors)) do
      begin
        LogicalProcessor := LLogicalProcessors[LJIdx];
        LPhysicalProcessorsDetails.Add
          (Format('%u:%u', [LogicalProcessor.PhysicalProcessorNumber,
          LogicalProcessor.PhysicalPackageNumber]));
      end;
      // LogicalProcessorCount := System.Length(LLogicalProcessors);
      Result := UInt32(LPhysicalProcessorsDetails.Count);
    finally
      LPhysicalProcessorsDetails.Free;
    end;
  finally
    LProcCpuInfos.Free;
  end;
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_SOLARIS}

class procedure TNumCPULib.ExecuteAndParseProcessOutput(const ACallingProcess
  : String; AInputParameters: TStringList; var AOutputParameters: TStringList);
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  LProcess: TProcess;
  LOutputStream: TStream;
  LBytesRead: LongInt;
  LBuffer: array [0 .. BUF_SIZE - 1] of Byte;
begin
  LProcess := TProcess.Create(nil);

  try
    LProcess.Executable := ACallingProcess;
    LProcess.Parameters.AddStrings(AInputParameters);

    LProcess.Options := LProcess.Options + [poWaitOnExit, poUsePipes];

    LProcess.Execute;

    // Create a stream object to store the generated output in.
    LOutputStream := TMemoryStream.Create;

    try
      // All generated output from LProcess is read in a loop until no more data is available
      repeat
        // Get the new data from the process to a maximum of the LBuffer size that was allocated.
        // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
        LBytesRead := LProcess.Output.Read(LBuffer, BUF_SIZE);

        // Add the bytes that were read to the stream for later usage
        LOutputStream.Write(LBuffer, LBytesRead)

      until LBytesRead = 0; // Stop if no more data is available

      // Required to make sure all data is copied from the start
      LOutputStream.Position := 0;
      AOutputParameters.LoadFromStream(LOutputStream);
    finally
      LOutputStream.Free;
    end;
  finally
    LProcess.Free;
  end;
end;

class function TNumCPULib.GetLogicalCPUCountSolaris(): UInt32;
begin
  Result := UInt32(sysconf(GetAppropriateSysConfNumber()));
end;

class function TNumCPULib.GetPhysicalCPUCountSolaris(): UInt32;
var
  LInputParameters, LOuputParameters, LCoreChipIDs: TStringList;
  LLineOutputInfo: String;
  LIdx: Int32;
  LChipId, LCoreId: UInt32;
begin
  Result := 0;

  LCoreChipIDs := TStringList.Create();
  LInputParameters := TStringList.Create();
  LOuputParameters := TStringList.Create();
  LCoreChipIDs.Sorted := True;
  LCoreChipIDs.Duplicates := dupIgnore;
  LOuputParameters.Sorted := True;
  LOuputParameters.Duplicates := dupIgnore;
  try
    LInputParameters.Add('-m');
    LInputParameters.Add('cpu_info');

    ExecuteAndParseProcessOutput('/usr/bin/kstat', LInputParameters,
      LOuputParameters);

    for LIdx := 0 to System.Pred(LOuputParameters.Count) do
    begin
      LLineOutputInfo := LOuputParameters[LIdx];
      if BeginsWith(LLineOutputInfo, 'chip_id', False) then
      begin
        LChipId := UInt32(ParseLastInt32(LLineOutputInfo, 0));
      end
      else if (BeginsWith(LLineOutputInfo, 'core_id', False)) then
      begin
        LCoreId := UInt32(ParseLastInt32(LLineOutputInfo, 0));
      end;

      LCoreChipIDs.Add(Format('%u:%u', [LCoreId, LChipId]));
    end;

    Result := UInt32(LCoreChipIDs.Count);

    // fallback if above method fails, note: the method below only works only for Solaris 10 and above
    if Result < 1 then
    begin
      LInputParameters.Clear;
      LOuputParameters.Clear;

      LInputParameters.Add('-p');
      ExecuteAndParseProcessOutput('psrinfo', LInputParameters,
        LOuputParameters);

      Result := UInt32(ParseLastInt32(LOuputParameters.Text, 0));
    end;

  finally
    LCoreChipIDs.Free;
    LInputParameters.Free;
    LOuputParameters.Free;
  end;
end;
{$ENDIF}
// ================================================================//
{$IFDEF NUMCPULIB_GENERIC_BSD}

class function TNumCPULib.GetLogicalCPUCountGenericBSD(): UInt32;
var
  LTempRes: Int32;
begin
  LTempRes := sysconf(GetAppropriateSysConfNumber());
  if LTempRes < 1 then
  begin
    Result := GetLogicalCPUCountUsingSysCtl();
  end
  else
  begin
    Result := UInt32(LTempRes);
  end;
end;
{$ENDIF}

class function TNumCPULib.GetLogicalCPUCount(): UInt32;
begin
{$IF DEFINED(NUMCPULIB_MSWINDOWS)}
  Result := GetLogicalCPUCountWindows();
{$ELSEIF DEFINED(NUMCPULIB_APPLE)}
  Result := GetLogicalCPUCountApple();
{$ELSEIF DEFINED(NUMCPULIB_LINUX)}
  Result := GetLogicalCPUCountLinux();
{$ELSEIF DEFINED(NUMCPULIB_SOLARIS)}
  Result := GetLogicalCPUCountSolaris();
{$ELSEIF DEFINED(NUMCPULIB_GENERIC_BSD)}
  Result := GetLogicalCPUCountGenericBSD();
{$ELSE}
  // fallback for other Unsupported Oses
  Result := 1;
{$IFEND}
end;

class function TNumCPULib.GetPhysicalCPUCount(): UInt32;
begin
{$IF DEFINED(NUMCPULIB_MSWINDOWS)}
  Result := GetPhysicalCPUCountWindows();
{$ELSEIF DEFINED(NUMCPULIB_APPLE)}
  Result := GetPhysicalCPUCountApple();
{$ELSEIF DEFINED(NUMCPULIB_LINUX)}
  Result := GetPhysicalCPUCountLinux();
{$ELSEIF DEFINED(NUMCPULIB_SOLARIS)}
  Result := GetPhysicalCPUCountSolaris();
{$ELSE}
  // fallback for other Unsupported Oses
  Result := 1;
{$IFEND}
end;

end.
