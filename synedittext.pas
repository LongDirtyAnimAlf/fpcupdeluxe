unit synedittext;
{
      Original by Kiriakos Vlahos (kvlahos.@lbs.lon.ac.uk)
}

interface

{$mode objfpc}

uses
  SynEdit;

  procedure AssignSynEdit(var T: Text; NewSynEditComponent: TCustomSynEdit);

implementation

uses
  Forms,
  SysUtils;

type
  SynEditData = record
    SynEdit: TCustomSynEdit;
    Filler: Array [1..12] of Char;
  end;
  PSynEditData = ^SynEditData;

  TSynEditHelper = class helper for TCustomSynEdit
    procedure SetSelTextBuf(aBuf: PChar); inline;
  end;

procedure TSynEditHelper.SetSelTextBuf(aBuf: PChar);
begin
  //Self.Append(StrPas(aBuf));
  Self.InsertTextAtCaret(StrPas(aBuf),scamIgnore);
  Self.CaretX:=0;
end;

function EditWrite(var F: TTextRec): Integer; far;
begin
  with F do
  begin
    BufPtr^[BufPos] := #0;
    with TSynEdit(PSynEditData(@F.UserData)^.SynEdit) do
    begin
      Lines.BeginUpdate;
      try
        SetSelTextBuf(PChar(BufPtr));
        SelStart:=Length(Text);
      finally
        Lines.EndUpdate;
      end;
    BufPos := 0;
    end;
  end;
  EditWrite := 0;
  Application.ProcessMessages;
end;

function EditFlush(var F: TTextRec): Integer; far;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  EditFlush := 0;
end;

function EditOpen(var F: TTextRec): Integer; far;
begin
  with F do
  begin
    if Mode <> fmInput then
    begin
      Mode := fmOutput;
      InOutFunc := @EditWrite;
      FlushFunc := @EditWrite;
    end;
    EditOpen := 0;
  end;
end;

function EditIgnore(var F: TTextRec): Integer; far;
begin
  EditIgnore := 0;
end;

procedure AssignSynEdit(var T: Text; NewSynEditComponent: TCustomSynEdit);
begin
  with TTextRec(T) do
  begin
    Handle := $FFFF;
    Mode := fmClosed;
    BufSize := SizeOf(Buffer)-1;
    BufPtr := @Buffer;
    OpenFunc := @EditOpen;
    CloseFunc := @EditIgnore;
    Name[0] := #0;
    PSynEditData(@UserData)^.SynEdit:= NewSynEditComponent;
  end;
end;

end.



