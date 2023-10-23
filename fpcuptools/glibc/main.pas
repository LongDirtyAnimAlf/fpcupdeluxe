unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnExport: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StringGrid1: TStringGrid;
    procedure btnExportClick(Sender: TObject);
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

uses
  typinfo,
  StrUtils,
  FileUtil;

type
  TGLIBCCPU  = (
    cpu_no,                       { 0 }
    cpu_i386,                     { 1 }
    cpu_m68k,                     { 2 }
    obsolete_cpu_alpha,           { 3 }
    cpu_powerpc,                  { 4 }
    cpu_sparc,                    { 5 }
    obsolete_cpu_vm,              { 6 }
    obsolete_cpu_ia64,            { 7 }
    cpu_x86_64,                   { 8 }
    cpu_mipseb,                   { 9 }
    cpu_arm,                      { 10 }
    cpu_powerpc64,                { 11 }
    cpu_avr,                      { 12 }
    cpu_mipsel,                   { 13 }
    cpu_jvm,                      { 14 }
    cpu_i8086,                    { 15 }
    cpu_aarch64,                  { 16 }
    cpu_wasm,                     { 17 }
    cpu_sparc64                   { 18 }
  );


const
  GLIBCCPU : set of TGLIBCCPU = [cpu_i386,cpu_powerpc,cpu_x86_64,cpu_arm,cpu_powerpc64,cpu_aarch64];

function GetEnumNameSimple(aTypeInfo:PTypeInfo;const aEnum:integer):string;
begin
  begin
    if (aTypeInfo=nil) or (aTypeInfo^.Kind<>tkEnumeration) then
      result := '' else
      result := GetEnumName(aTypeInfo,aEnum);
  end;
end;

function CPUStr(aCPU:TGLIBCCPU):string;
var
  s:string;
begin
  s:=GetEnumNameSimple(TypeInfo(TGLIBCCPU),Ord(aCPU));
  Delete(s,1,4);
  result:=s;
end;

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
var
  aCPU:TGLIBCCPU;
  i:integer;
begin
  i:=1;
  for aCPU in GLIBCCPU do
  begin
    StringGrid1.Cells[i,0]:=CPUStr(aCPU);
    Inc(i);
  end;
  StringGrid1.ColWidths[0]:=150;
end;

procedure TForm1.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
const
  //ABIS : array [0..2] of string = ('libc','libm','libresolve');
  ABIS : array [0..1] of string = ('libc','libm');
var
  aCPU:TGLIBCCPU;
  datafromfile:TStringList;
  glibcfunctions:TStringList;
  sh,abi:string;
  v,f,vd:string;
  i,j,k:integer;
  fl:TStrings;
  fa:boolean;
begin
  if IsColumn then
  begin

    StaticText1.Visible:=True;
    Application.ProcessMessages;

    case Index of
      1:aCPU:=TGLIBCCPU.cpu_i386;
      2:aCPU:=TGLIBCCPU.cpu_powerpc;
      3:aCPU:=TGLIBCCPU.cpu_x86_64;
      4:aCPU:=TGLIBCCPU.cpu_arm;
      5:aCPU:=TGLIBCCPU.cpu_powerpc64;
      6:aCPU:=TGLIBCCPU.cpu_aarch64;
    end;

    glibcfunctions:=TStringList.Create;
    try

      datafromfile:=TStringList.Create;
      try
        for abi in ABIS do
        begin
          datafromfile.Clear;
          if (aCPU=TGLIBCCPU.cpu_x86_64) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_amd64.txt');
          if (aCPU=TGLIBCCPU.cpu_i386) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_i386.txt');
          if (aCPU=TGLIBCCPU.cpu_arm) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_armle.txt');
          if (aCPU=TGLIBCCPU.cpu_aarch64) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_aarch64.txt');
          if (aCPU=TGLIBCCPU.cpu_powerpc) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_ppc.txt');
          if (aCPU=TGLIBCCPU.cpu_powerpc64) then datafromfile.LoadFromFile('.\abilists\linux_'+abi+'_ppc64le.txt');
          if datafromfile.Count>0 then
          begin
            for sh in datafromfile do
            begin
              fa:=true;
              i:=Pos(' ',sh);
              j:=Pos('_',sh);
              Inc(j);
              v:=Copy(sh,j,i-j);
              Inc(i);
              j:=Pos(' ',sh,i);
              f:=Copy(sh,i,j-i);
              // Now we have a version string [v] and the function itself [f]
              //while f[1]='_' do Delete(f,1,1);
              if (f[1]='_') then
              begin
                fa:=false;
                if (f='__setfpucw') then fa:=true;
                if (Pos('__libc_',f)=1) then fa:=true;
                if (Pos('__cxa_',f)=1) then fa:=true;
                if (Pos('__errno_',f)=1) then fa:=true;
              end;
              if (NOT fa) then continue;
              glibcfunctions.AddObject(f,TObject(StrNew(PChar(v))));
            end;
          end;
        end;
      finally
        datafromfile.Free;
      end;

      // Only for i386
      if (aCPU=TGLIBCCPU.cpu_i386) then
      begin
        glibcfunctions.AddObject('__setfpucw',TObject(StrNew('2.0.6')));
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
            if (i>0) then k:=fl.IndexOf(glibcfunctions[i-1]);
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

    StaticText1.Visible:=False;
  end;
end;

procedure TForm1.btnExportClick(Sender: TObject);
var
  glibcfunctionsfromfile:TStringList;
  i,j:integer;
  setsize:integer;
  sl,fh,fa:string;
  aCPU:TGLIBCCPU;
begin
  glibcfunctionsfromfile:=TStringList.Create;

  setsize:=0;
  fa:='';
  // Add CPUs included
  for aCPU in GLIBCCPU do
  begin
    fa:=fa+GetEnumNameSimple(TypeInfo(TGLIBCCPU),Ord(aCPU))+',';
    Inc(setsize);
  end;
  // Remove last comma
  SetLength(fa,Pred(Length(fa)));

  glibcfunctionsfromfile.Add('const');
  glibcfunctionsfromfile.Add('  GLIBCCPU : set of tsystemcpu = ['+fa+'];');
  glibcfunctionsfromfile.Add('type');
  glibcfunctionsfromfile.Add('  TGLIBCFUNCTION = record');
  glibcfunctionsfromfile.Add('    Name:string;');
  glibcfunctionsfromfile.Add('    Version: array[0..'+InttoStr(Pred(setsize))+'] of string;');
  glibcfunctionsfromfile.Add('  end;');

  glibcfunctionsfromfile.Add('const');
  glibcfunctionsfromfile.Add('  GLIBCFUNCTIONS : array [0..'+InttoStr(Pred(StringGrid1.RowCount))+'] of TGLIBCFUNCTION = (');

  try
    for i:=0 to Pred(StringGrid1.RowCount) do
    begin
      if i=0 then
        sl:='CPU_included'
      else
        sl:=StringGrid1.Cells[0,i];

      if (Length(sl)>30) then
        sl:=PadRight(sl,45)
      else
        sl:=PadRight(sl,35);

      sl:=StringReplace(sl,' ','''',[]);
      sl:=''''+sl;

      fa:='';

      j:=1;
      for aCPU in GLIBCCPU do
      begin
        if i=0 then
          fh:=CPUStr(aCPU)
        else
          fh:=StringGrid1.Cells[j,i];
        fh:=PadRight(fh,12);
        fh:=StringReplace(fh,' ','''',[]);
        if (j<setsize) then fh:=StringReplace(fh,' ',',',[]);
        fh:=''''+fh;
        fa:=fa+fh;
        Inc(j);
      end;
      if i<Pred(StringGrid1.RowCount) then
        fa:='('+fa+')),'
      else
        fa:='('+fa+'))';
      glibcfunctionsfromfile.Add('    (Name : '+sl+' ; Version : '+fa);
    end;

    glibcfunctionsfromfile.Add(' );');
    glibcfunctionsfromfile.SaveToFile('glibc.inc');
  finally
    glibcfunctionsfromfile.Free;
  end;

end;

end.

