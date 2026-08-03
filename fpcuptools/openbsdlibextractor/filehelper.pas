unit FileHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, SysUtils;

function MoveData(const Src, Dst: WideString; FileOverwrite: Boolean = True): Boolean;

implementation

  const
    FO_MOVE = $0001;
    FOF_NOCONFIRMMKDIR = $0200;

  type
    TSHFileOpStructW = record
      Wnd: HWND;
      wFunc: UINT;
      pFrom: LPCWSTR;
      pTo: LPCWSTR;
      fFlags: FILEOP_FLAGS;
      fAnyOperationsAborted: BOOL;
      hNameMappings: Pointer;
      lpszProgressTitle: LPCWSTR;
    end;

  function MoveFileW(lpExistingFileName, lpNewFileName: PWideChar): BOOL; stdcall; external 'Kernel32.dll' name 'MoveFileW';

  function SHFileOperationW(const lpFileOp: TSHFileOpStructW): Integer; stdcall; external 'shell32.dll' name 'SHFileOperationW';

  function MoveDir(const Src, Dst: WideString): Boolean;
  var
    FOS: TSHFileOpStructW;
  begin
    ZeroMemory(@FOS, SizeOf(FOS));
    FOS.wFunc := FO_MOVE;
    FOS.fFlags := FOF_NOCONFIRMMKDIR;
    FOS.pFrom := PWideChar(IncludeTrailingPathDelimiter(Src) + '*.*'#0);
    FOS.pTo := PWideChar(Dst + #0);
    Result := (SHFileOperationW(FOS) = 0);
  end;

  // this method wants either a full source and destination path
  // or a full source and destination path with "filename.ext"
  // it return true on success or false on failure
  function MoveData(const Src, Dst: WideString; FileOverwrite: Boolean = True): Boolean;
  begin
    Result := False;
    if FileExists(Src) then
      begin
        Result := ForceDirectories(ExtractFilePath(Dst));
        if Result then
          begin
            if (FileOverwrite and FileExists(Dst)) then
              Result := ((SetFileAttributesW(PWideChar('\\?\' + Dst), faNormal)) and DeleteFileW(PWideChar('\\?\' + Dst)));
            if Result then
              Result := MoveFileW(PWideChar('\\?\' + Src), PWideChar('\\?\' + Dst));
          end;
      end
    else
    if DirectoryExists(Src) then
      begin
        Result := MoveDir(Src, Dst);
        if Result then
          Result := RemoveDir(Src);
      end;
  end;

end.

