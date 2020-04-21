{==============================================================================|
| Project : Ararat Synapse                                       | 004.000.000 |
|==============================================================================|
| Content: FTP client                                                          |
|==============================================================================|
| Copyright (c)1999-2011, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c) 1999-2010.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Petr Esner <petr.esner@atlas.cz>                                           |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{: @abstract(FTP client protocol)

Used RFC: RFC-959, RFC-2228, RFC-2428
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$TYPEINFO ON}// Borland changed defualt Visibility from Public to Published
                // and it requires RTTI to be generated $M+
{$M+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ftplist;

interface

uses
  SysUtils, Classes;

const
  {:Terminating value for TLogonActions}
  FTP_OK = 255;
  {:Terminating value for TLogonActions}
  FTP_ERR = 254;

type
  {:Array for holding definition of logon sequence.}
  TLogonActions = array [0..17] of byte;

  {:Procedural type for OnStatus event. Sender is calling @link(TFTPSend) object.
   Value is FTP command or reply to this comand. (if it is reply, Response
   is @True).}
  TFTPStatus = procedure(Sender: TObject; Response: Boolean;
    const Value: string) of object;

  {: @abstract(Object for holding file information) parsed from directory
   listing of FTP server.}
  TFTPListRec = class(TObject)
  private
    FFileName: String;
    FDirectory: Boolean;
    FReadable: Boolean;
    FFileSize: int64;
    FFileTime: TDateTime;
    FOriginalLine: string;
    FMask: string;
    FPermission: String;
  public
    {: You can assign another TFTPListRec to this object.}
    procedure Assign(Value: TFTPListRec); virtual;
    {:name of file}
    property FileName: string read FFileName write FFileName;
    {:if name is subdirectory not file.}
    property Directory: Boolean read FDirectory write FDirectory;
    {:if you have rights to read}
    property Readable: Boolean read FReadable write FReadable;
    {:size of file in bytes}
    property FileSize: int64 read FFileSize write FFileSize;
    {:date and time of file. Local server timezone is used. Any timezone
     conversions was not done!}
    property FileTime: TDateTime read FFileTime write FFileTime;
    {:original unparsed line}
    property OriginalLine: string read FOriginalLine write FOriginalLine;
    {:mask what was used for parsing}
    property Mask: string read FMask write FMask;
    {:permission string (depending on used mask!)}
    property Permission: string read FPermission write FPermission;
  end;

  {:@abstract(This is TList of TFTPListRec objects.)
   This object is used for holding lististing of all files information in listed
   directory on FTP server.}
  TFTPList = class(TObject)
  protected
    FList: TList;
    FLines: TStringList;
    FMasks: TStringList;
    FUnparsedLines: TStringList;
    Monthnames: string;
    BlockSize: string;
    DirFlagValue: string;
    FileName: string;
    VMSFileName: string;
    Day: string;
    Month: string;
    ThreeMonth: string;
    YearTime: string;
    Year: string;
    Hours: string;
    HoursModif: Ansistring;
    Minutes: string;
    Seconds: string;
    Size: Ansistring;
    Permissions: Ansistring;
    DirFlag: string;
    function GetListItem(Index: integer): TFTPListRec; virtual;
    function ParseEPLF(Value: string): Boolean; virtual;
    procedure ClearStore; virtual;
    function ParseByMask(Value, NextValue, Mask: ansistring): Integer; virtual;
    function CheckValues: Boolean; virtual;
    procedure FillRecord(const Value: TFTPListRec); virtual;
  public
    {:Constructor. You not need create this object, it is created by TFTPSend
     class as their property.}
    constructor Create;
    destructor Destroy; override;

    {:Clear list.}
    procedure Clear; virtual;

    {:count of holded @link(TFTPListRec) objects}
    function Count: integer; virtual;

    {:Assigns one list to another}
    procedure Assign(Value: TFTPList); virtual;

    {:try to parse raw directory listing in @link(lines) to list of
     @link(TFTPListRec).}
    procedure ParseLines; virtual;

    {:By this property you have access to list of @link(TFTPListRec).
     This is for compatibility only. Please, use @link(Items) instead.}
    property List: TList read FList;

    {:By this property you have access to list of @link(TFTPListRec).}
    property Items[Index: Integer]: TFTPListRec read GetListItem; default;

    {:Set of lines with RAW directory listing for @link(parseLines)}
    property Lines: TStringList read FLines;

    {:Set of masks for directory listing parser. It is predefined by default,
    however you can modify it as you need. (for example, you can add your own
    definition mask.) Mask is same as mask used in TotalCommander.}
    property Masks: TStringList read FMasks;

    {:After @link(ParseLines) it holding lines what was not sucessfully parsed.}
    property UnparsedLines: TStringList read FUnparsedLines;
  end;

implementation


uses synautil;
{==============================================================================}

procedure TFTPListRec.Assign(Value: TFTPListRec);
begin
  FFileName := Value.FileName;
  FDirectory := Value.Directory;
  FReadable := Value.Readable;
  FFileSize := Value.FileSize;
  FFileTime := Value.FileTime;
  FOriginalLine := Value.OriginalLine;
  FMask := Value.Mask;
end;

constructor TFTPList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FLines := TStringList.Create;
  FMasks := TStringList.Create;
  FUnparsedLines := TStringList.Create;
  //various UNIX
  FMasks.add('pppppppppp $!!!S*$TTT$DD$hh mm ss$YYYY$n*');
  FMasks.add('pppppppppp $!!!S*$DD$TTT$hh mm ss$YYYY$n*');
  FMasks.add('pppppppppp $!!!S*$TTT$DD$UUUUU$n*');  //mostly used UNIX format
  FMasks.add('pppppppppp $!!!S*$DD$TTT$UUUUU$n*');
  //MacOS
  FMasks.add('pppppppppp $!!S*$TTT$DD$UUUUU$n*');
  FMasks.add('pppppppppp $!S*$TTT$DD$UUUUU$n*');
  //Novell
  FMasks.add('d            $!S*$TTT$DD$UUUUU$n*');
  //Windows
  FMasks.add('MM DD YY  hh mmH !S* n*');
  FMasks.add('MM DD YY  hh mmH $ d!n*');
  FMasks.add('MM DD YYYY  hh mmH !S* n*');
  FMasks.add('MM DD YYYY  hh mmH $ d!n*');
  FMasks.add('DD MM YYYY  hh mmH !S* n*');
  FMasks.add('DD MM YYYY  hh mmH $ d!n*');
  //VMS
  FMasks.add('v*$  DD TTT YYYY hh mm');
  FMasks.add('v*$!DD TTT YYYY hh mm');
  FMasks.add('n*$                 YYYY MM DD hh mm$S*');
  //AS400
  FMasks.add('!S*$MM DD YY hh mm ss !n*');
  FMasks.add('!S*$DD MM YY hh mm ss !n*');
  FMasks.add('n*!S*$MM DD YY hh mm ss d');
  FMasks.add('n*!S*$DD MM YY hh mm ss d');
  //VxWorks
  FMasks.add('$S*    TTT DD YYYY  hh mm ss $n* $ d');
  FMasks.add('$S*    TTT DD YYYY  hh mm ss $n*');
  //Distinct
  FMasks.add('d    $S*$TTT DD YYYY  hh mm$n*');
  FMasks.add('d    $S*$TTT DD$hh mm$n*');
  //PC-NFSD
  FMasks.add('nnnnnnnn.nnn  dSSSSSSSSSSS MM DD YY  hh mmH');
  //VOS
  FMasks.add('-   SSSSS            YY MM DD hh mm ss  n*');
  FMasks.add('- d=  SSSSS  YY MM DD hh mm ss  n*');
  //Unissys ClearPath
  FMasks.add('nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn               SSSSSSSSS MM DD YYYY hh mm');
  FMasks.add('n*\x                                               SSSSSSSSS MM DD YYYY hh mm');
  //IBM
  FMasks.add('-     SSSSSSSSSSSS           d   MM DD YYYY   hh mm  n*');
  //OS9
  FMasks.add('-         YY MM DD hhmm d                        SSSSSSSSS n*');
  //tandem
  FMasks.add('nnnnnnnn                   SSSSSSS DD TTT YY hh mm ss');
  //MVS
  FMasks.add('-             YYYY MM DD                     SSSSS   d=O n*');
  //BullGCOS8
  FMasks.add('             $S* MM DD YY hh mm ss  !n*');
  FMasks.add('d            $S* MM DD YY           !n*');
  //BullGCOS7
  FMasks.add('                                         TTT DD  YYYY n*');
  FMasks.add('  d                                                   n*');
end;

destructor TFTPList.Destroy;
begin
  Clear;
  FList.Free;
  FLines.Free;
  FMasks.Free;
  FUnparsedLines.Free;
  inherited Destroy;
end;

procedure TFTPList.Clear;
var
  n:integer;
begin
  for n := 0 to FList.Count - 1 do
    if Assigned(FList[n]) then
      TFTPListRec(FList[n]).Free;
  FList.Clear;
  FLines.Clear;
  FUnparsedLines.Clear;
end;

function TFTPList.Count: integer;
begin
  Result := FList.Count;
end;

function TFTPList.GetListItem(Index: integer): TFTPListRec;
begin
  Result := nil;
  if Index < Count then
    Result := TFTPListRec(FList[Index]);
end;

procedure TFTPList.Assign(Value: TFTPList);
var
  flr: TFTPListRec;
  n: integer;
begin
  Clear;
  for n := 0 to Value.Count - 1 do
  begin
    flr := TFTPListRec.Create;
    flr.Assign(Value[n]);
    Flist.Add(flr);
  end;
  Lines.Assign(Value.Lines);
  Masks.Assign(Value.Masks);
  UnparsedLines.Assign(Value.UnparsedLines);
end;

procedure TFTPList.ClearStore;
begin
  Monthnames := '';
  BlockSize := '';
  DirFlagValue := '';
  FileName := '';
  VMSFileName := '';
  Day := '';
  Month := '';
  ThreeMonth := '';
  YearTime := '';
  Year := '';
  Hours := '';
  HoursModif := '';
  Minutes := '';
  Seconds := '';
  Size := '';
  Permissions := '';
  DirFlag := '';
end;

function TFTPList.ParseByMask(Value, NextValue, Mask: AnsiString): Integer;
var
  Ivalue, IMask: integer;
  MaskC, LastMaskC: AnsiChar;
  c: AnsiChar;
  s: string;
begin
  ClearStore;
  Result := 0;
  if Value = '' then
    Exit;
  if Mask = '' then
    Exit;
  Ivalue := 1;
  IMask := 1;
  Result := 1;
  LastMaskC := ' ';
  while Imask <= Length(mask) do
  begin
    if (Mask[Imask] <> '*') and (Ivalue > Length(Value)) then
    begin
      Result := 0;
      Exit;
    end;
    MaskC := Mask[Imask];
    if Ivalue > Length(Value) then
      Exit;
    c := Value[Ivalue];
    case MaskC of
      'n':
        FileName := FileName + c;
      'v':
        VMSFileName := VMSFileName + c;
      '.':
        begin
          if c in ['.', ' '] then
            FileName := TrimSP(FileName) + '.'
          else
          begin
            Result := 0;
            Exit;
          end;
        end;
      'D':
        Day := Day + c;
      'M':
        Month := Month + c;
      'T':
        ThreeMonth := ThreeMonth + c;
      'U':
        YearTime := YearTime + c;
      'Y':
        Year := Year + c;
      'h':
        Hours := Hours + c;
      'H':
        HoursModif := HoursModif + c;
      'm':
        Minutes := Minutes + c;
      's':
        Seconds := Seconds + c;
      'S':
        Size := Size + c;
      'p':
        Permissions := Permissions + c;
      'd':
        DirFlag := DirFlag + c;
      'x':
        if c <> ' ' then
          begin
            Result := 0;
            Exit;
          end;
      '*':
        begin
          s := '';
          if LastMaskC in ['n', 'v'] then
          begin
            if Imask = Length(Mask) then
              s := Copy(Value, IValue, Maxint)
            else
              while IValue <= Length(Value) do
              begin
                if Value[Ivalue] = ' ' then
                  break;
                s := s + Value[Ivalue];
                Inc(Ivalue);
              end;
            if LastMaskC = 'n' then
              FileName := FileName + s
            else
              VMSFileName := VMSFileName + s;
          end
          else
          begin
            while IValue <= Length(Value) do
            begin
              if not(Value[Ivalue] in ['0'..'9']) then
                break;
              s := s + Value[Ivalue];
              Inc(Ivalue);
            end;
            case LastMaskC of
              'S':
                Size := Size + s;
            end;
          end;
          Dec(IValue);
        end;
      '!':
        begin
          while IValue <= Length(Value) do
          begin
            if Value[Ivalue] = ' ' then
              break;
            Inc(Ivalue);
          end;
          while IValue <= Length(Value) do
          begin
            if Value[Ivalue] <> ' ' then
              break;
            Inc(Ivalue);
          end;
          Dec(IValue);
        end;
      '$':
        begin
          while IValue <= Length(Value) do
          begin
            if not(Value[Ivalue] in [' ', #9]) then
              break;
            Inc(Ivalue);
          end;
          Dec(IValue);
        end;
      '=':
        begin
          s := '';
          case LastmaskC of
            'S':
              begin
                while Imask <= Length(Mask) do
                begin
                  if not(Mask[Imask] in ['0'..'9']) then
                    break;
                  s := s + Mask[Imask];
                  Inc(Imask);
                end;
                Dec(Imask);
                BlockSize := s;
              end;
            'T':
              begin
                Monthnames := Copy(Mask, IMask, 12 * 3);
                Inc(IMask, 12 * 3);
              end;
            'd':
              begin
                Inc(Imask);
                DirFlagValue := Mask[Imask];
              end;
          end;
        end;
      '\':
        begin
          Value := NextValue;
          IValue := 0;
          Result := 2;
        end;
    end;
    Inc(Ivalue);
    Inc(Imask);
    LastMaskC := MaskC;
  end;
end;

function TFTPList.CheckValues: Boolean;
var
  x, n: integer;
begin
  Result := false;
  if FileName <> '' then
  begin
    if Pos('?', VMSFilename) > 0 then
      Exit;
    if Pos('*', VMSFilename) > 0 then
      Exit;
  end;
  if VMSFileName <> '' then
    if Pos(';', VMSFilename) <= 0 then
      Exit;
  if (FileName = '') and (VMSFileName = '') then
    Exit;
  if Permissions <> '' then
  begin
    if length(Permissions) <> 10 then
      Exit;
    for n := 1 to 10 do
      if not(Permissions[n] in
        ['a', 'b', 'c', 'd', 'h', 'l', 'p', 'r', 's', 't', 'w', 'x', 'y', '-']) then
        Exit;
  end;
  if Day <> '' then
  begin
    Day := TrimSP(Day);
    x := StrToIntDef(day, -1);
    if (x < 1) or (x > 31) then
      Exit;
  end;
  if Month <> '' then
  begin
    Month := TrimSP(Month);
    x := StrToIntDef(Month, -1);
    if (x < 1) or (x > 12) then
      Exit;
  end;
  if Hours <> '' then
  begin
    Hours := TrimSP(Hours);
    x := StrToIntDef(Hours, -1);
    if (x < 0) or (x > 24) then
      Exit;
  end;
  if HoursModif <> '' then
  begin
    if not (HoursModif[1] in ['a', 'A', 'p', 'P']) then
      Exit;
  end;
  if Minutes <> '' then
  begin
    Minutes := TrimSP(Minutes);
    x := StrToIntDef(Minutes, -1);
    if (x < 0) or (x > 59) then
      Exit;
  end;
  if Seconds <> '' then
  begin
    Seconds := TrimSP(Seconds);
    x := StrToIntDef(Seconds, -1);
    if (x < 0) or (x > 59) then
      Exit;
  end;
  if Size <> '' then
  begin
    Size := TrimSP(Size);
    for n := 1 to Length(Size) do
      if not (Size[n] in ['0'..'9']) then
        Exit;
  end;

  if length(Monthnames) = (12 * 3) then
    for n := 1 to 12 do
      CustomMonthNames[n] := Copy(Monthnames, ((n - 1) * 3) + 1, 3);
  if ThreeMonth <> '' then
  begin
    x := GetMonthNumber(ThreeMonth);
    if (x = 0) then
      Exit;
  end;
  if YearTime <> '' then
  begin
    YearTime := ReplaceString(YearTime, '-', ':');
    if Pos(':', YearTime) > 0 then
    begin
      if (GetTimeFromstr(YearTime) = -1) then
        Exit;
    end
    else
    begin
      YearTime := TrimSP(YearTime);
      x := StrToIntDef(YearTime, -1);
      if (x = -1) then
        Exit;
      if (x < 1900) or (x > 2100) then
        Exit;
    end;
  end;
  if Year <> '' then
  begin
    Year := TrimSP(Year);
    x := StrToIntDef(Year, -1);
    if (x = -1) then
      Exit;
    if Length(Year) = 4 then
    begin
      if not((x > 1900) and (x < 2100)) then
        Exit;
    end
    else
      if Length(Year) = 2 then
      begin
        if not((x >= 0) and (x <= 99)) then
          Exit;
      end
      else
        if Length(Year) = 3 then
        begin
          if not((x >= 100) and (x <= 110)) then
            Exit;
        end
        else
          Exit;
  end;
  Result := True;
end;

procedure TFTPList.FillRecord(const Value: TFTPListRec);
var
  s: string;
  x: integer;
  myear: Word;
  mmonth: Word;
  mday: Word;
  mhours, mminutes, mseconds: word;
  n: integer;
begin
  s := DirFlagValue;
  if s = '' then
    s := 'D';
  s := Uppercase(s);
  Value.Directory :=  s = Uppercase(DirFlag);
  if FileName <> '' then
    Value.FileName := SeparateLeft(Filename, ' -> ');
  if VMSFileName <> '' then
  begin
    Value.FileName := VMSFilename;
    Value.Directory := Pos('.DIR;',VMSFilename) > 0;
  end;
  Value.FileName := TrimSPRight(Value.FileName);
  Value.Readable := not Value.Directory;
  if BlockSize <> '' then
    x := StrToIntDef(BlockSize, 1)
  else
    x := 1;
  {$IFDEF VER100}
  Value.FileSize := x * StrToIntDef(Size, 0);
  {$ELSE}
  Value.FileSize := x * StrToInt64Def(Size, 0);
  {$ENDIF}

  DecodeDate(Date,myear,mmonth,mday);
  mhours := 0;
  mminutes := 0;
  mseconds := 0;

  if Day <> '' then
    mday := StrToIntDef(day, 1);
  if Month <> '' then
    mmonth := StrToIntDef(Month, 1);
  if length(Monthnames) = (12 * 3) then
    for n := 1 to 12 do
      CustomMonthNames[n] := Copy(Monthnames, ((n - 1) * 3) + 1, 3);
  if ThreeMonth <> '' then
    mmonth := GetMonthNumber(ThreeMonth);
  if Year <> '' then
  begin
    myear := StrToIntDef(Year, 0);
    if (myear <= 99) and (myear > 50) then
      myear := myear + 1900;
    if myear <= 50 then
      myear := myear + 2000;
  end;
  if YearTime <> '' then
  begin
    if Pos(':', YearTime) > 0 then
    begin
      YearTime := TrimSP(YearTime);
      mhours := StrToIntDef(Separateleft(YearTime, ':'), 0);
      mminutes := StrToIntDef(SeparateRight(YearTime, ':'), 0);
      if (Encodedate(myear, mmonth, mday)
        + EncodeTime(mHours, mminutes, 0, 0)) > now then
        Dec(mYear);
    end
    else
      myear := StrToIntDef(YearTime, 0);
  end;
  if Minutes <> '' then
    mminutes := StrToIntDef(Minutes, 0);
  if Seconds <> '' then
    mseconds := StrToIntDef(Seconds, 0);
  if Hours <> '' then
  begin
    mHours := StrToIntDef(Hours, 0);
    if HoursModif <> '' then
      if Uppercase(HoursModif[1]) = 'P' then
        if mHours <> 12 then
          mHours := MHours + 12;
  end;
  Value.FileTime := Encodedate(myear, mmonth, mday)
    + EncodeTime(mHours, mminutes, mseconds, 0);
  if Permissions <> '' then
  begin
    Value.Permission := Permissions;
    Value.Readable := Uppercase(permissions)[2] = 'R';
    if Uppercase(permissions)[1] = 'D' then
    begin
      Value.Directory := True;
      Value.Readable := false;
    end
    else
      if Uppercase(permissions)[1] = 'L' then
        Value.Directory := True;
  end;
end;

function TFTPList.ParseEPLF(Value: string): Boolean;
var
  s, os: string;
  flr: TFTPListRec;
begin
  Result := False;
  if Value <> '' then
    if Value[1] = '+' then
    begin
      os := Value;
      Delete(Value, 1, 1);
      flr := TFTPListRec.create;
      flr.FileName := SeparateRight(Value, #9);
      s := Fetch(Value, ',');
      while s <> '' do
      begin
        if s[1] = #9 then
          Break;
        case s[1] of
          '/':
            flr.Directory := true;
          'r':
            flr.Readable := true;
          's':
            {$IFDEF VER100}
            flr.FileSize := StrToIntDef(Copy(s, 2, Length(s) - 1), 0);
            {$ELSE}
            flr.FileSize := StrToInt64Def(Copy(s, 2, Length(s) - 1), 0);
            {$ENDIF}
          'm':
            flr.FileTime := (StrToIntDef(Copy(s, 2, Length(s) - 1), 0) / 86400)
              + 25569;
        end;
        s := Fetch(Value, ',');
      end;
      if flr.FileName <> '' then
      if (flr.Directory and ((flr.FileName = '.') or (flr.FileName = '..')))
        or (flr.FileName = '') then
        flr.free
      else
      begin
        flr.OriginalLine := os;
        flr.Mask := 'EPLF';
        Flist.Add(flr);
        Result := True;
      end;
    end;
end;

procedure TFTPList.ParseLines;
var
  flr: TFTPListRec;
  n, m: Integer;
  S: string;
  x: integer;
  b: Boolean;
begin
  n := 0;
  while n < Lines.Count do
  begin
    if n = Lines.Count - 1 then
      s := ''
    else
      s := Lines[n + 1];
    b := False;
    x := 0;
    if ParseEPLF(Lines[n]) then
    begin
      b := True;
      x := 1;
    end
    else
      for m := 0 to Masks.Count - 1 do
      begin
        x := ParseByMask(Lines[n], s, Masks[m]);
        if x > 0 then
          if CheckValues then
          begin
            flr := TFTPListRec.create;
            FillRecord(flr);
            flr.OriginalLine := Lines[n];
            flr.Mask := Masks[m];
            if flr.Directory and ((flr.FileName = '.') or (flr.FileName = '..')) then
              flr.free
            else
              Flist.Add(flr);
            b := True;
            Break;
          end;
      end;
    if not b then
      FUnparsedLines.Add(Lines[n]);
    Inc(n);
    if x > 1 then
      Inc(n, x - 1);
  end;
end;

end.
