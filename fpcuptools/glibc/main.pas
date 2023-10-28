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
    btnGetABI: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StringGrid1: TStringGrid;
    procedure btnExportClick(Sender: TObject);
    procedure btnGetABIClick(Sender: TObject);
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
  fphttpclient, openssl, opensslsockets,
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

  TONLINEABI  = (ld,libanl,libc,libc_malloc_debug,libcrypt,libdl,libm,libmvec,libnsl,libpthread,libresolv,librt,libthread_db,libutil);

  TGLIBCVERSION = record
    Date:string;
    Version: string;
  end;

const
  GLIBCCPU : set of TGLIBCCPU = [cpu_i386,cpu_m68k,cpu_powerpc,cpu_sparc,cpu_x86_64,cpu_arm,cpu_powerpc64,cpu_aarch64,cpu_sparc64];
  FILEFORMAT = 'abi_%s_%s.dat';

  GLIBCVERSION : array [0..119] of TGLIBCVERSION = (
    (Date : '01-08-2024 '; Version : '2.40'),
    (Date : '01-02-2024 '; Version : '2.39'),
    (Date : '31-07-2023 '; Version : '2.38'),
    (Date : '01-02-2023 '; Version : '2.37'),
    (Date : '01-08-2022 '; Version : '2.36'),
    (Date : '03-02-2022 '; Version : '2.35'),
    (Date : '02-08-2021 '; Version : '2.34'),
    (Date : '01-02-2021 '; Version : '2.33'),
    (Date : '05-08-2020 '; Version : '2.32'),
    (Date : '01-02-2020 '; Version : '2.31'),
    (Date : '01-08-2019 '; Version : '2.30'),
    (Date : '01-02-2019 '; Version : '2.29'),
    (Date : '01-08-2018 '; Version : '2.28'),
    (Date : '01-02-2018 '; Version : '2.27'),
    (Date : '02-08-2017 '; Version : '2.26'),
    (Date : '01-02-2017 '; Version : '2.25'),
    (Date : '04-08-2016 '; Version : '2.24'),
    (Date : '19-02-2016 '; Version : '2.23'),
    (Date : '14-08-2015 '; Version : '2.22'),
    (Date : '06-02-2015 '; Version : '2.21'),
    (Date : '08-09-2014 '; Version : '2.20'),
    (Date : '07-02-2014 '; Version : '2.19'),
    (Date : '12-08-2013 '; Version : '2.18'),
    (Date : '25-12-2012 '; Version : '2.17'),
    (Date : '30-06-2012 '; Version : '2.16'),
    (Date : '21-03-2012 '; Version : '2.15'),
    (Date : '07-10-2011 '; Version : '2.14.1'),
    (Date : '01-06-2011 '; Version : '2.14'),
    (Date : '01-02-2011 '; Version : '2.13'),
    (Date : '13-12-2010 '; Version : '2.12.2'),
    (Date : '30-11-2010 '; Version : '2.11.3'),
    (Date : '03-08-2010 '; Version : '2.12.1'),
    (Date : '19-05-2010 '; Version : '2.11.2'),
    (Date : '03-05-2010 '; Version : '2.12'),
    (Date : '29-12-2009 '; Version : '2.11.1'),
    (Date : '16-11-2009 '; Version : '2.10.2'),
    (Date : '03-11-2009 '; Version : '2.11'),
    (Date : '18-05-2009 '; Version : '2.10.1'),
    (Date : '09-05-2009 '; Version : '2.10'),
    (Date : '13-11-2008 '; Version : '2.9'),
    (Date : '12-04-2008 '; Version : '2.8'),
    (Date : '19-10-2007 '; Version : '2.7'),
    (Date : '31-07-2007 '; Version : '2.6.1'),
    (Date : '31-07-2007 '; Version : '2.5.1'),
    (Date : '17-05-2007 '; Version : '2.6'),
    (Date : '29-09-2006 '; Version : '2.5'),
    (Date : '06-03-2006 '; Version : '2.4'),
    (Date : '04-11-2005 '; Version : '2.3.6'),
    (Date : '07-04-2005 '; Version : '2.3.5'),
    (Date : '29-12-2004 '; Version : '2.3.4'),
    (Date : '01-12-2003 '; Version : '2.3.3'),
    (Date : '28-02-2003 '; Version : '2.3.2'),
    (Date : '10-10-2002 '; Version : '2.3.1'),
    (Date : '02-10-2002 '; Version : '2.3'),
    (Date : '20-01-2002 '; Version : '2.2.5'),
    (Date : '27-07-2001 '; Version : '2.2.4'),
    (Date : '26-04-2001 '; Version : '2.2.3'),
    (Date : '15-02-2001 '; Version : '2.2.2'),
    (Date : '13-01-2001 '; Version : '2.2.1'),
    (Date : '09-11-2000 '; Version : '2.2'),
    (Date : '24-02-2000 '; Version : '2.1.3'),
    (Date : '06-09-1999 '; Version : '2.1.2'),
    (Date : '24-05-1999 '; Version : '2.1.1'),
    (Date : '03-02-1999 '; Version : '2.1'),
    (Date : '29-12-1997 '; Version : '2.0.6'),
    (Date : '25-08-1997 '; Version : '2.0.5'),
    (Date : '27-05-1997 '; Version : '2.0.4'),
    (Date : '22-04-1997 '; Version : '2.0.3'),
    (Date : '22-03-1997 '; Version : '2.0.2'),
    (Date : '04-02-1997 '; Version : '2.0.1'),
    (Date : '26-01-1997 '; Version : '2.0'),
    (Date : '16-01-1995 '; Version : '1.09.5'),
    (Date : '12-12-1994 '; Version : '1.09.3'),
    (Date : '05-12-1994 '; Version : '1.09.2'),
    (Date : '06-11-1994 '; Version : '1.09'),
    (Date : '04-11-1994 '; Version : '1.08.14'),
    (Date : '01-11-1994 '; Version : '1.08.13'),
    (Date : '24-10-1994 '; Version : '1.08.12'),
    (Date : '19-10-1994 '; Version : '1.08.11'),
    (Date : '11-10-1994 '; Version : '1.08.10'),
    (Date : '26-09-1994 '; Version : '1.08.9'),
    (Date : '04-09-1994 '; Version : '1.08.8'),
    (Date : '01-09-1994 '; Version : '1.08.7'),
    (Date : '08-08-1994 '; Version : '1.08.6'),
    (Date : '03-08-1994 '; Version : '1.08.5'),
    (Date : '29-07-1994 '; Version : '1.08.4'),
    (Date : '04-07-1994 '; Version : '1.08.3'),
    (Date : '05-06-1994 '; Version : '1.08.1'),
    (Date : '21-05-1994 '; Version : '1.08'),
    (Date : '18-05-1994 '; Version : '1.07.6'),
    (Date : '03-05-1994 '; Version : '1.07.5'),
    (Date : '17-02-1994 '; Version : '1.07.4'),
    (Date : '08-02-1994 '; Version : '1.07.3'),
    (Date : '05-02-1994 '; Version : '1.07.2'),
    (Date : '25-01-1994 '; Version : '1.07.1'),
    (Date : '17-01-1994 '; Version : '1.07'),
    (Date : '16-01-1994 '; Version : '1.06.13'),
    (Date : '13-01-1994 '; Version : '1.06.12'),
    (Date : '11-01-1994 '; Version : '1.06.11'),
    (Date : '08-01-1994 '; Version : '1.06.10'),
    (Date : '25-12-1993 '; Version : '1.06.9'),
    (Date : '21-12-1993 '; Version : '1.06.8'),
    (Date : '09-11-1993 '; Version : '1.06.7'),
    (Date : '16-08-1993 '; Version : '1.06.6'),
    (Date : '29-06-1993 '; Version : '1.06.4'),
    (Date : '16-06-1993 '; Version : '1.06.3'),
    (Date : '31-05-1993 '; Version : '1.06.2'),
    (Date : '27-05-1993 '; Version : '1.06.1'),
    (Date : '23-05-1993 '; Version : '1.06'),
    (Date : '24-09-1992 '; Version : '1.05'),
    (Date : '03-09-1992 '; Version : '1.04'),
    (Date : '09-04-1992 '; Version : '1.03'),
    (Date : '20-03-1992 '; Version : '1.02'),
    (Date : '11-03-1992 '; Version : '1.01'),
    (Date : '18-02-1992 '; Version : '1.00'),
    (Date : '17-02-1992 '; Version : '0.6'),
    (Date : '16-02-1992 '; Version : '0.5'),
    (Date : '14-02-1992 '; Version : '0.4.1'),
    (Date : '14-02-1992 '; Version : '0.4'),
    (Date : '08-10-1991 '; Version : '0.1')
);

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

function ABIStr(aABI:TONLINEABI):string;
begin
  result:=GetEnumNameSimple(TypeInfo(TONLINEABI),Ord(aABI));
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

function GetUrlAs(Url: String; AsName: String): Boolean;
begin
  Result := False;
  with TFPHttpClient.Create(nil) do
  try
    AllowRedirect := True;
    if (ExtractFilePath(AsName) <> '') then
      if not DirectoryExists(ExtractFilePath(AsName)) then
        if not ForceDirectories(ExtractFilePath(AsName)) then Exit;
    try
      AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:18.0) Gecko/20100101 Firefox/18.0');
      Get(Url, AsName);
      Result := True;
    finally
      Free;
    end;
  except
  end;
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
  ABIS : set of TONLINEABI = [libc,libm,libdl,libpthread,libresolv];
var
  aCPU:TGLIBCCPU;
  datafromfile:TStringList;
  glibcfunctions:TStringList;
  fl:TStringList;
  tabi:TONLINEABI;
  sh,abi:string;
  v,f,vd:string;
  i,j,k:integer;
  fa:boolean;
begin
  if IsColumn then
  begin
    StaticText1.Visible:=True;
    Application.ProcessMessages;

    case Index of
      1:aCPU:=TGLIBCCPU.cpu_i386;
      2:aCPU:=TGLIBCCPU.cpu_m68k;
      3:aCPU:=TGLIBCCPU.cpu_powerpc;
      4:aCPU:=TGLIBCCPU.cpu_sparc;
      5:aCPU:=TGLIBCCPU.cpu_x86_64;
      6:aCPU:=TGLIBCCPU.cpu_arm;
      7:aCPU:=TGLIBCCPU.cpu_powerpc64;
      8:aCPU:=TGLIBCCPU.cpu_aarch64;
      9:aCPU:=TGLIBCCPU.cpu_sparc64;
    end;

    glibcfunctions:=TStringList.Create;
    try
      datafromfile:=TStringList.Create;
      try
        for tabi in ABIS do
        begin
          datafromfile.Clear;
          sh:=CPUStr(aCPU);
          abi:=ABIStr(tabi);
          v:=Format(FILEFORMAT,[abi,sh]);
          datafromfile.LoadFromFile('.'+DirectorySeparator+'abilists'+DirectorySeparator+v);
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
              if (f[1]='.') then fa:=false;
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
      // Only for arm
      if (aCPU=TGLIBCCPU.cpu_arm) then
      begin
        glibcfunctions.AddObject('__setfpucw',TObject(StrNew('2.3.6')));
      end;

      glibcfunctions.Sort;

      fl:=TStringList.Create;


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
          // save the older glibc version
          vd:=StrPas(PChar(glibcfunctions.Objects[k]));
          if CompareVersionStrings(v,vd)<0 then
          begin
            (*
            j:=fl.IndexOf(glibcfunctions[i]);
            if j=-1 then
              fl.AddObject(glibcfunctions[i],TObject(StrNew(PChar(v))))
            else
            begin
              StrDispose(PChar(fl.Objects[j]));
              fl.Objects[j]:=TObject(StrNew(PChar(v)));
            end;
            *)
            StrDispose(PChar(glibcfunctions.Objects[k]));
            glibcfunctions.Delete(k);
          end;
          if CompareVersionStrings(v,vd)>0 then
          begin
            (*
            j:=fl.IndexOf(glibcfunctions[k]);
            if j=-1 then
              fl.AddObject(glibcfunctions[k],TObject(StrNew(PChar(vd))))
            else
            begin
              StrDispose(PChar(fl.Objects[j]));
              fl.Objects[j]:=TObject(StrNew(PChar(vd)));
            end;
            *)
            StrDispose(PChar(glibcfunctions.Objects[i]));
            glibcfunctions.Delete(i);
          end;
        end;
      end;

      (*
      for i:=0 to Pred(glibcfunctions.Count) do StrDispose(PChar(glibcfunctions.Objects[i]));
      glibcfunctions.Clear;
      for i:=0 to Pred(fl.Count) do glibcfunctions.AddObject(fl[i],fl.Objects[i]);
      fl.Clear;
      fl.Free;
      *)

      glibcfunctions.Sort;

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

      if (Length(sl)>32) then
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

procedure TForm1.btnGetABIClick(Sender: TObject);
const
  ONLINEABIS : set of TONLINEABI = [Low(TONLINEABI) .. High(TONLINEABI)];
var
  aCPU:TGLIBCCPU;
  tabi:TONLINEABI;
  sh,abi:string;
  v,f:string;
begin
  for aCPU in GLIBCCPU do
  begin
    sh:=CPUStr(aCPU);
    f:=sh;
    if aCPU=TGLIBCCPU.cpu_x86_64 then f:=f+'/64';
    if aCPU=TGLIBCCPU.cpu_arm then f:=f+'/le';
    if aCPU=TGLIBCCPU.cpu_m68k then f:=f+'/coldfire';
    if aCPU=TGLIBCCPU.cpu_powerpc then f:='powerpc/powerpc32';
    if aCPU=TGLIBCCPU.cpu_powerpc64 then f:='powerpc/powerpc64/le';
    if aCPU=TGLIBCCPU.cpu_sparc then f:='sparc/sparc32';
    if aCPU=TGLIBCCPU.cpu_sparc64 then f:='sparc/sparc64';
    for tabi in ONLINEABIS do
    begin
      abi:=ABIStr(tabi);
      v:='https://sourceware.org/git/?p=glibc.git;a=blob_plain;f=sysdeps/unix/sysv/linux/'+f+'/'+abi+'.abilist';
      Memo1.Lines.Append(v);
      GetUrlAs(v,'.'+DirectorySeparator+'abilists'+DirectorySeparator+Format(FILEFORMAT,[abi,sh]));
    end;
    if aCPU=TGLIBCCPU.cpu_powerpc then
    begin
      f:='powerpc/powerpc32/fpu';
      for tabi in [libc,libm] do
      begin
        abi:=ABIStr(tabi);
        v:='https://sourceware.org/git/?p=glibc.git;a=blob_plain;f=sysdeps/unix/sysv/linux/'+f+'/'+abi+'.abilist';
        Memo1.Lines.Append(v);
        GetUrlAs(v,'.'+DirectorySeparator+'abilists'+DirectorySeparator+Format(FILEFORMAT,[abi,sh]));
      end;
    end;
  end;
end;

end.

