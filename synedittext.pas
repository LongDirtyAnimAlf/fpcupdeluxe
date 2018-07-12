unit synedittext;
{
      Original by Kiriakos Vlahos (kvlahos.@lbs.lon.ac.uk)
      This version by DonAlfredo)
}

interface

{$mode objfpc}{$H+}

uses
  SynEdit;

  procedure AssignSynEdit(var T: Text; NewSynEditComponent: TCustomSynEdit);
  procedure SetVerbosity(verbose:boolean);

implementation

uses
  Forms,
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

var
  linestore:string;
  filteroutput:boolean;

procedure TSynEditHelper.SetSelTextBuf(aBuf: PChar);
var
  i:cardinal;
  subline,line,s:string;
  outputline:boolean;
begin
  outputline:=false;
  subline:=StrPas(aBuf);
  linestore:=linestore+subline;

  i:=Pos(LineEnding,linestore);
  while (i>0) do
  begin
    outputline:=(NOT filteroutput);

    if i=1 then
    begin
      line:='';
      outputline:=true;
    end else line:=Copy(linestore,1,i-1);

    i:=i+Length(LineEnding);
    Delete(linestore,1,i-1);

    // get new line-ending to be used by next loop
    i:=Pos(LineEnding,linestore);

    {$ifdef Darwin}
    // suppress all setfocus errors on Darwin, always
    if AnsiContainsText(line,'.setfocus') then continue;
    {$endif}
    // suppress all SynEdit PaintLock errors, always
    if AnsiContainsText(line,'PaintLock') then continue;

    // suppress some GIT errors, always
    if AnsiContainsText(line,'fatal: not a git repository') then continue;

    // suppress some lazbuild errors, always
    if AnsiContainsText(line,'lazbuild') then
    begin
      if AnsiContainsText(line,'only for runtime') then continue;
      if AnsiContainsText(line,'lpk file expected') then continue;
    end;

    if (NOT outputline) then
    begin
      // to be absolutely sure not to miss errors and fatals and fpcupdeluxe messages !!
      // will be a bit redundant , but just to be sure !
      if (AnsiContainsText(line,'error:'))
         OR (AnsiContainsText(line,'donalf:'))
         OR (AnsiContainsText(line,'fatal:'))
         OR (AnsiContainsText(line,'fpcupdeluxe:'))
         OR (AnsiContainsText(line,'execute:'))
         OR (AnsiContainsText(line,'executing:'))
         OR ((AnsiContainsText(line,'compiling ')) AND (NOT AnsiContainsText(line,'when compiling target')))
         OR (AnsiContainsText(line,'linking '))
      then outputline:=true;

      if (NOT outputline) then
      begin
        // remove hints and other "trivial"* warnings from output
        // these line are not that interesting for the average user of fpcupdeluxe !
        if AnsiContainsText(line,'hint: ') then continue;
        if AnsiContainsText(line,'verbose: ') then continue;
        if AnsiContainsText(line,'note: ') then continue;
        if AnsiContainsText(line,'assembling ') then continue;
        if AnsiContainsText(line,': entering directory ') then continue;
        if AnsiContainsText(line,': leaving directory ') then continue;
        // when generating help
        if AnsiContainsText(line,'illegal XML element: ') then continue;
        if AnsiContainsText(line,'parsing used unit ') then continue;
        if AnsiContainsText(line,'extracting ') then continue;

        // during building of lazarus components, default compiler switches cause version and copyright info to be shown
        // do not know if this is allowed, but this version / copyright info is very redundant as it is shown everytime the compiler is called ...
        // I stand corrected if this has to be changed !
        if AnsiContainsText(line,'Copyright (c) 1993-') then continue;
        if AnsiContainsText(line,'Free Pascal Compiler version ') then continue;

        // harmless make error
        if AnsiContainsText(line,'make') then
        begin
          if AnsiContainsText(line,'error 1') then continue;
          if AnsiContainsText(line,'(e=1)') then continue;
          if AnsiContainsText(line,'error 87') then continue;
          if AnsiContainsText(line,'(e=87)') then continue;
        end;

        {$ifdef Darwin}
        if AnsiContainsText(line,'~~~~~~~~') then continue;
        if AnsiContainsText(line,'coalesced') then continue;
        {$endif}

        // filter warnings
        if AnsiContainsText(line,'warning: ') then
        begin
          if AnsiContainsText(line,'is not portable') then continue;
          if AnsiContainsText(line,'is deprecated') then continue;
          if AnsiContainsText(line,'implicit string type conversion') then continue;
          if AnsiContainsText(line,'function result does not seem to be set') then continue;
          if AnsiContainsText(line,'comparison might be always') then continue;
          //if AnsiContainsText(line,'unreachable code') then continue;
          if AnsiContainsText(line,'converting pointers to signed integers') then continue;
          if AnsiContainsText(line,'does not seem to be initialized') then continue;
          if AnsiContainsText(line,'an inherited method is hidden') then continue;
          if AnsiContainsText(line,'with abstract method') then continue;
          if AnsiContainsText(line,'comment level 2 found') then continue;
          if AnsiContainsText(line,'did you forget -T') then continue;
          if AnsiContainsText(line,'is not recommended') then continue;
          if AnsiContainsText(line,'were not initialized') then continue;
          if AnsiContainsText(line,'which is not available for the') then continue;
          if AnsiContainsText(line,'argument unused during compilation') then continue;
          if AnsiContainsText(line,'invalid unitname') then continue;
          if AnsiContainsText(line,'procedure type "FAR" ignored') then continue;
          if AnsiContainsText(line,'duplicate unit') then continue;
          if AnsiContainsText(line,'is ignored for the current target platform') then continue;
          if AnsiContainsText(line,'Inlining disabled') then continue;
          if AnsiContainsText(line,'not yet supported inside inline procedure/function') then continue;
          if AnsiContainsText(line,'Check size of memory operand') then continue;
          if AnsiContainsText(line,'User defined: TODO') then continue;
          if AnsiContainsText(line,'Circular dependency detected when compiling target') then continue;
          if AnsiContainsText(line,'overriding recipe for target') then continue;
          if AnsiContainsText(line,'ignoring old recipe for target') then continue;
          // when generating help
          if AnsiContainsText(line,'is unknown') then continue;
          {$ifdef MSWINDOWS}
          if AnsiContainsText(line,'unable to determine the libgcc path') then continue;
          {$endif}
        end;
        // suppress "trivial"* build commands
        {$ifdef MSWINDOWS}
        if AnsiContainsText(line,'rm.exe ') then continue;
        if AnsiContainsText(line,'mkdir.exe ') then continue;
        if AnsiContainsText(line,'mv.exe ') then continue;
        if AnsiContainsText(line,'cmp.exe ') then continue;
        if (AnsiContainsText(line,'cp.exe ')) AND (AnsiContainsText(line,'.compiled')) then continue;
        {$endif}
        {$ifdef UNIX}
        s:='rm -f ';
        if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then continue;
        s:='rm -rf ';
        if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then continue;
        s:='mkdir ';
        if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then continue;
        s:='mv ';
        if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then continue;
        s:='cp ';
        if ( (AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line)) AND AnsiContainsText(line,'.compiled') ) then continue;
        {$endif}
        if AnsiContainsText(line,'is up to date.') then continue;
        if AnsiContainsText(line,'searching ') then continue;
        // found modified files
        outputline:=true;
      end;
    end;

    // output line !!
    if (outputline) then
    begin
      Self.Append(line);
      Self.CaretX:=0;
      Self.CaretY:=Self.Lines.Count;
      // the below is needed:
      // onchange is no longer called, when appending a line ... bug or feature ?!!
      Self.OnChange(Self);
      Application.ProcessMessages;
    end;

  end;
end;

function EditWrite(var F: TTextRec): Integer;
begin
  try
    with F do
    begin
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
  filteroutput:=false;
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

procedure SetVerbosity(verbose:boolean);
begin
  filteroutput:=(NOT verbose);
end;

end.
