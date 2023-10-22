unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  TCPU      = (cpuNone,i386,x86_64,arm,aarch64);

const
  CPUStr : array[TCPU] of string = (
    '','i386','x86_64','arm','aarch64'
  );

  // 1on1 shameless copy from unit cutils from the fpc compiler;
function CompareVersionStrings(s1,s2: string): longint;
var
  start1, start2,
  i1, i2,
  num1,num2,
  res,
  err: longint;
begin
  i1:=1;
  i2:=1;
  repeat
    start1:=i1;
    start2:=i2;
    while (i1<=length(s1)) and
          (s1[i1] in ['0'..'9']) do
       inc(i1);
    while (i2<=length(s2)) and
          (s2[i2] in ['0'..'9']) do
       inc(i2);
    { one of the strings misses digits -> other is the largest version }
    if i1=start1 then
      if i2=start2 then
        exit(0)
      else
        exit(-1)
    else if i2=start2 then
      exit(1);
    { get version number part }
    val(copy(s1,start1,i1-start1),num1,err);
    val(copy(s2,start2,i2-start2),num2,err);
    { different -> done }
    res:=num1-num2;
    if res<>0 then
      exit(res);
    { if one of the two is at the end while the other isn't, add a '.0' }
    if (i1>length(s1)) and
       (i2<=length(s2)) then
      s1:=s1+'.0';
    if (i2>length(s2)) and
       (i1<=length(s1)) then
       s2:=s2+'.0';
    { compare non-numerical characters normally }
    while (i1<=length(s1)) and
          not(s1[i1] in ['0'..'9']) and
          (i2<=length(s2)) and
          not(s2[i2] in ['0'..'9']) do
      begin
        res:=ord(s1[i1])-ord(s2[i2]);
        if res<>0 then
          exit(res);
        inc(i1);
        inc(i2);
      end;
    { both should be digits again now, otherwise pick the one with the
      digits as the largest (it more likely means that the input was
      ill-formatted though) }
    if (i1<=length(s1)) and
       not(s1[i1] in ['0'..'9']) then
      exit(-1);
    if (i2<=length(s2)) and
       not(s2[i2] in ['0'..'9']) then
      exit(1);
  until false;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[1,0]:=CPUStr[TCPU.i386];
  StringGrid1.Cells[2,0]:=CPUStr[TCPU.x86_64];
  StringGrid1.Cells[3,0]:=CPUStr[TCPU.arm];
  StringGrid1.Cells[4,0]:=CPUStr[TCPU.aarch64];
  StringGrid1.ColWidths[0]:=150;
end;

procedure TForm1.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
const
  //ABIS : array [0..2] of string = ('libc','libm','libresolve');
  ABIS : array [0..1] of string = ('libc','libm');
var
  aCPU:TCPU;
  datafromfile:TStringList;
  glibcfunctions:TStringList;
  sh,abi:string;
  v,f,vd:string;
  i,j,k:integer;
  vi:integer;
  fl:TStrings;
begin
  aCPU:=TCPU.cpuNone;
  if IsColumn then
  begin
    case Index of
      1:aCPU:=TCPU.i386;
      2:aCPU:=TCPU.x86_64;
      3:aCPU:=TCPU.arm;
      4:aCPU:=TCPU.aarch64;
    end;

    glibcfunctions:=TStringList.Create;
    try

      datafromfile:=TStringList.Create;
      try
        for abi in ABIS do
        begin
          datafromfile.Clear;
          if (aCPU=TCPU.x86_64) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_amd64.txt');
          if (aCPU=TCPU.i386) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_i386.txt');
          if (aCPU=TCPU.arm) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_armle.txt');
          if (aCPU=TCPU.aarch64) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_aarch64.txt');
          if datafromfile.Count>0 then
          begin
            for sh in datafromfile do
            begin
              i:=Pos(' ',sh);
              j:=Pos('_',sh);
              Inc(j);
              v:=Copy(sh,j,i-j);
              Inc(i);
              j:=Pos(' ',sh,i);
              f:=Copy(sh,i,j-i);
              // Now we have a version string [v] and the function itself [f]
              //while f[1]='_' do Delete(f,1,1);
              if (f[1]='_') AND (Pos('__libc',f)<>1) then continue;
              glibcfunctions.AddObject(f,TObject(StrNew(PChar(v))));
            end;
          end;
        end;
      finally
        datafromfile.Free;
      end;

      glibcfunctions.Sort;

      // Process duplicates in the function list
      // Use the oldest version
      for i:=Pred(glibcfunctions.Count) downto 0 do
      begin
        f:=glibcfunctions[i];
        v:=StrPas(PChar(glibcfunctions.Objects[i]));
        k:=glibcfunctions.IndexOf(f);
        if (k<>i) then
        begin
          // we have a duplicate
          // delete the newer glibc version
          vd:=StrPas(PChar(glibcfunctions.Objects[k]));
          if CompareVersionStrings(v,vd)<0 then
          begin
            StrDispose(PChar(glibcfunctions.Objects[k]));
            glibcfunctions.Delete(k);
          end;
          vd:=StrPas(PChar(glibcfunctions.Objects[k]));
          if CompareVersionStrings(v,vd)>0 then
          begin
            StrDispose(PChar(glibcfunctions.Objects[i]));
            glibcfunctions.Delete(i);
          end;
        end;
      end;

      // Just a simple speedup of first use.
      // Could be left out
      if StringGrid1.RowCount=1 then
      begin
        StringGrid1.RowCount:=Succ(glibcfunctions.Count);
        i:=1;
        for sh in glibcfunctions do
        begin
          StringGrid1.Cells[0,i]:=sh;
          Inc(i);
        end;
      end;

      fl:=TStringList.Create;
      try
        fl.SetStrings(StringGrid1.Cols[0]);
        for i:=0 to Pred(glibcfunctions.Count) do
        begin
          f:=glibcfunctions[i];
          v:=StrPas(PChar(glibcfunctions.Objects[i]));
          j:=fl.IndexOf(f);
          if j=-1 then
          begin
            k:=0;
            if (i>0) then k:=fl.IndexOf(glibcfunctions[i-1]{,i});
            Inc(k,1);
            StringGrid1.InsertColRow(false,k);
            StringGrid1.Cells[0,k]:=f;
            StringGrid1.Cells[Index,k]:=v;
            fl.Clear;
            fl.AddStrings(StringGrid1.Cols[0]);
          end
          else
          begin
            StringGrid1.Cells[Index,j]:=v;
          end;
        end;
      finally
        fl.Free;
      end;

    finally
      for i:=0 to Pred(glibcfunctions.Count) do StrDispose(PChar(glibcfunctions.Objects[i]));
      glibcfunctions.Free;
    end;
  end;
end;

end.

