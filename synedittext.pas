unit synedittext;
{
      Original by Kiriakos Vlahos (kvlahos.@lbs.lon.ac.uk)
      This version by DonAlfredo)
}

interface

{$mode objfpc}{$H+}

uses
  Forms,
  SynEdit;

  procedure AssignSynEdit(var T: Text; NewSynEditComponent: TCustomSynEdit);

implementation

uses
  LCLIntf,
  LMessages,
  StrUtils,
  SysUtils;

type

  SynEditData = record
    SynEdit: TCustomSynEdit;
    {$ifdef CPU64}
    Filler: array [1..8] of char;
    {$else}
    Filler: array [1..12] of char;
    {$endif}
  end;
  PSynEditData = ^SynEditData;

  TSynEditHelper = class helper for TCustomSynEdit
    procedure SetSelTextBuf(aBuf: PChar); inline;
  end;

const
  WM_THREADINFO = LM_USER + 2010;

var
  linestore:string;

procedure ThreadLog(Msg: string);
var
  PInfo: PChar;
begin
  PInfo := StrAlloc(Length(Msg)+1);
  StrCopy(PInfo, PChar(Msg));
  Application.MainForm.Handle;
  PostMessage(Application.MainForm.Handle, WM_THREADINFO, NativeUInt(PInfo), 0);
end;

procedure TSynEditHelper.SetSelTextBuf(aBuf: PChar);
var
  i:cardinal;
  subline,line:string;
begin
  subline:=StrPas(aBuf);
  linestore:=linestore+subline;

  {$ifdef Windows}
  for i:=2 to Length(linestore) do
  begin
    if ((linestore[i]=#10) AND (linestore[i-1]<>#13)) then
    begin
      Insert(#13,linestore,i);
    end;
  end;
  {$endif}

  i:=Pos(LineEnding,linestore);
  while (i>0) do
  begin

    if i=1 then
    begin
      line:='';
    end
    else
    begin
      line:=Copy(linestore,1,i-1);
      line:=TrimRight(line);
    end;

    i:=i+Length(LineEnding);
    Delete(linestore,1,i-1);

    // get new line-ending to be used by next loop
    i:=Pos(LineEnding,linestore);

    // skip stray empty lines
    //if (Length(line)=0) then continue;

    ThreadLog(line);
  end;
end;

function EditWrite(var F: TTextRec): Integer;
begin
  InOutRes:=0;
  try
    with F do
    begin
      if BufPos=0 then exit;
      InOutRes:=101;
      BufPtr^[BufPos] := #0;
      with TSynEdit(PSynEditData(@F.UserData)^.SynEdit) do
      begin
        Lines.BeginUpdate;
        try
          SetSelTextBuf(PChar(BufPtr));
        finally
          Lines.EndUpdate;
        end;
      end;
    end;
    InOutRes:=0;
  finally
    F.BufPos := 0;
    EditWrite := 0;
  end;
  Application.ProcessMessages;
end;

function EditFlush(var F: TTextRec): Integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  EditFlush := 0;
end;


function EditOpen(var F: TTextRec): Integer;
begin
  with F do
  begin
    BufPos:=0;
    BufEnd:=0;
    if Mode <> fmInput then
    begin
      Mode := fmOutput;
      InOutFunc := @EditWrite;
      FlushFunc := @EditWrite;
    end;
    EditOpen := 0;
  end;
end;

function EditIgnore(var {%H-}F: TTextRec): Integer;
begin
  EditIgnore := 0;
end;

procedure AssignSynEdit(var T: Text; NewSynEditComponent: TCustomSynEdit);
begin
  FillChar(T,SizeOf(TextRec),0);
  {$ifdef FPC_HAS_CPSTRING}
  {$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
  SetTextCodePage(T,TTextRec(T).CodePage);
  {$else FPC_HAS_FEATURE_ANSISTRINGS}
  TextRec(t).CodePage:=0;
  {$endif FPC_HAS_FEATURE_ANSISTRINGS}
  {$endif}
  with TTextRec(T) do
  begin
    Handle := UnusedHandle;
    Mode := fmClosed;
    BufPtr := @Buffer;
    BufSize := SizeOf(Buffer)-1; //this -1 is very important: line-edings are missed without it !!
    //BufSize := TextRecBufSize-1;
    OpenFunc := @EditOpen;
    CloseFunc := @EditIgnore;
    case DefaultTextLineBreakStyle of
      tlbsLF: LineEnd := #10;
      tlbsCRLF: LineEnd := #13#10;
      tlbsCR: LineEnd := #13;
    end;
    Name[0] := #0;
    PSynEditData(@UserData)^.SynEdit:= NewSynEditComponent;
  end;
end;

end.
