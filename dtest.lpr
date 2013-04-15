program dtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, db, dbf,dbf_fields,dbf_common,
  SysUtils,dateutils;

procedure CreateDBF(FileName: string; TableLevel: integer);
const
  UseID=true;
  UseName=true;
  UseFloater=false;
  UseMyMemo=false;
  UseBCDF=false;
  UseVarBytes=true;
var
  NewDBF: TDBF;
  i: integer;
begin
  NewDBF:=TDBF.Create(nil);
  try
    NewDBF.TableLevel:=TableLevel;
    NewDBF.TableName:=FileName;
    writeln('Creating ',FileName,' with table level ',NewDBF.TableLevel);
    if UseID then
      if TableLevel>=30 then
        begin
          NewDBF.FieldDefs.Add('ID', ftAutoInc)
        end
        else
          NewDBF.FieldDefs.Add('ID', ftInteger);
    if UseName then
      NewDBF.FieldDefs.Add('NAME', ftString, 5);
    if UseFloater then
      NewDBF.FieldDefs.Add('FLOATER', ftFloat);
    if UseMyMemo then
      NewDBF.FieldDefs.Add('MYMEMO', ftMemo);
    // Only in visual foxpro ('Y' field)
    if (TableLevel>=30) and (UseBCDF) then
      NewDBF.FieldDefs.Add('BCDF',ftBCD);
    if (TableLevel>=30) and (UseVarBytes) then
      NewDBF.FieldDefs.Add('VBYTE5',ftVarBytes,5);
    if (TableLevel>=30) and (UseVarBytes) then
      NewDBF.FieldDefs.Add('VBYTE5N',ftVarBytes,5);
    NewDBF.CreateTable;
    NewDBF.Open;
    writeln('Codepage: ',NewDBF.CodePage);
    for i := 1 to 5 do //keep size manageable until we have working files
    begin
      NewDBF.Append;
      if (UseID) and (NewDBF.FieldDefs.Find('ID').DataType<>ftAutoInc) then
        NewDBF.FieldByName('ID').AsInteger := i;
      if UseName then
        if i=1 then
          begin
            writeln('Setting Record ',i,' NAME to NULL');
            NewDBF.FieldByName('NAME').Clear; // set to null
          end
        else
          NewDBF.FieldByName('NAME').AsString := StringOfChar('A',i);
      if UseFloater then
        NewDBF.FieldByName('FLOATER').AsFloat := 2/i;
      if UseMyMemo then
        NewDBF.FieldByName('MYMEMO').AsString := 'Test' + IntToStr(i);
      // Only in visual foxpro ('Y' field)
      if (TableLevel>=30) and (UseBCDF) then
        NewDBF.FieldByName('BCDF').AsCurrency:= 2/i;
      if (TableLevel>=30) and (UseVarBytes) then
        NewDBF.FieldByName('VBYTE5').AsString:=StringOfChar('A',i);
      if (TableLevel>=30) and (UseVarBytes) then
        NewDBF.FieldByName('VBYTE5N').AsString:=StringOfChar('A',i);
      NewDBF.Post;
    end;
    writeln('After assigning fields & data: tablelevel: ',NewDBF.TableLevel);
    NewDBF.Close;
  finally
    NewDBF.Free;
  end;
end;

procedure ListDBFs(Results: TstringList);
var r:TSearchRec;
begin
  results.Clear;
  if FindFirst('*.dbf', faAnyFile-{$WARNINGS OFF}faVolumeID-faSymLink{$WARNINGS ON}, r)=0 then
  begin
    repeat
      if (r.Attr and faDirectory) <> faDirectory then
      begin
        results.add(expandfilename(r.Name))
      end;
    until (FindNext(r)<>0);
    findclose(r);
  end;
end;

function BinFieldToHex(BinarySource: TField): string;
var
  HexValue:PChar;
begin
  result:='';
  HexValue:=StrAlloc(Length(BinarySource.AsBytes));
  try
    try
      BinToHex(PChar(BinarySource.AsBytes),HexValue,Length(BinarySource.AsBytes));
      result:='size: '+inttostr(Length(BinarySource.AsBytes))+'; hex: '+HexValue;
    except
      on E: Exception do
      begin
        result:='exception: '+E.ClassName+'/'+E.Message;
      end;
    end;
  finally
    StrDispose(HexValue);
  end;
end;

procedure PrintRecord(DBf: TDBf; RecordNumber: integer);
var
  i:integer;
begin
  writeln('Record ' + inttostr(RecordNumber));
  for i:=0 to DBf.Fields.Count-1 do
  begin
    if DBF.fields[i].IsNull then
      writeln('Field ',DBf.Fields[i].FieldName,' is          ***NULL***')
    else
      if DBF.Fields[i].DataType in [ftVarBytes,ftBytes] then
        writeln('Field ',DBF.Fields[i].FieldName,' has value: binary '+BinFieldToHex(DBF.Fields[i]))
      else
        writeln('Field ',DBf.Fields[i].FieldName,' has value: '+DBf.fields[i].asstring);
  end;
end;

var
  DBFs: TStringList;
  FileNo: integer;
  MyDbf: TDbf;
  RecCount: integer;
  SomeDate: TDateTime;

procedure w32_functionstest;
begin
  if lowercase(mydbf.tablename)='w32_functions.dbf' then
  begin
    SomeDate:=ScanDateTime('yyyymmdd hhnnss','20051216 154425');
    if SomeDate=mydbf.FieldByName('MODIFIED').AsDateTime then
      writeln('modified field is equal to somedate!');
    {
    if mydbf.fieldbyname('EXAMPLEID').Asinteger=503 then
      begin
        somedate:=mydbf.FieldByName('MODIFIED').AsDateTime;
        writeln('Copied modified date value which is: ');
        writeln(FormatDateTime('yyyymmdd hhnnss zzzz',SomeDate));
      end;
    }
  end;
end;

procedure w32_examplestest;
begin
  if lowercase(mydbf.tablename)='w32_examples.dbf' then
  begin
    writeln('Testing locate:');
    try
      writeln('Recno is now: '+inttostr(MyDBF.RecNo));
      SomeDate:=ScanDateTime('yyyymmdd hhnnss zzz','20051216 154425 999');
      writeln('trying to find modified at date '+FormatDateTime('yyyymmdd hhnnss zzz',SomeDate));
      if MyDBF.Locate('MODIFIED',SomeDate,[]) then
      begin
        writeln('Recno is now: '+inttostr(MyDBF.RecNo));
        PrintRecord(MyDBF,RecCount)
      end
      else
      begin
        writeln('Locate did not work/find anything.');
      end;

      writeln('Testing locate partial key with inexact date which should fail:');
      writeln('Recno is now: '+inttostr(MyDBF.RecNo));
      SomeDate:=ScanDateTime('yyyymmdd hhnnss zzz','20051216 154425 000');
      writeln('trying to find modified at/after '+FormatDateTime('yyyymmdd hhnnss zzz',SomeDate));
      if MyDBF.Locate('MODIFIED',SomeDate,[loPartialKey]) then
      begin
        writeln('Recno is now: '+inttostr(MyDBF.RecNo));
        PrintRecord(MyDBF,RecCount)
      end
      else
      begin
        writeln('Locate partial key did not work/find anything.');
      end;
      writeln('End locate partial test');

      writeln('Testing locate partial key with inexact string which should work:');
      writeln('Recno is now: '+inttostr(MyDBF.RecNo));
      writeln('trying to find EXAMPLENAM GDI+');
      if MyDBF.Locate('EXAMPLENAM','GDI+',[loPartialKey]) then
      begin
        writeln('Recno is now: '+inttostr(MyDBF.RecNo));
        PrintRecord(MyDBF,RecCount)
      end
      else
      begin
        writeln('Locate partial key did not work/find anything.');
      end;
      writeln('End locate partial test');
    except
      on E: EDbfError do
      begin
        writeln('Locate test: dbf error '+E.Message);
      end;
      on F: EDatabaseError do
      begin
        writeln('Locate test: database error '+F.Message);
      end;
    end;
  end;
end;

procedure w32_orderstest;
begin
  if lowercase(mydbf.tablename)='orders.dbf' then
  begin
    //todo: set descending index
    writeln('Testing locate for orders:');
    try
      mydbf.AddIndex('orderiddesc','ORDER_ID',[ixDescending]);
      writeln('Recno is now: '+inttostr(MyDBF.RecNo));

      writeln('Testing locate partial key with inexact orderid which should fail:');
      writeln('Recno is now: '+inttostr(MyDBF.RecNo));
      writeln('trying to find ORDER_ID at/after '+inttostr(10006));
      if MyDBF.Locate('ORDER_ID',10006,[loPartialKey]) then
      begin
        writeln('Recno is now: '+inttostr(MyDBF.RecNo));
        PrintRecord(MyDBF,RecCount)
      end
      else
      begin
        writeln('Locate partial key did not work/find anything.');
      end;
      writeln('End locate partial test');

    except
      on E: EDbfError do
      begin
        writeln('Locate test: dbf error '+E.Message);
      end;
      on F: EDatabaseError do
      begin
        writeln('Locate test: database error '+F.Message);
      end;
    end;
  end;
end;

var
  Create: boolean=false;
  Delete: boolean=false;
  i: integer;
begin
  DBFs:=TStringList.Create;
  try
    // --create create & run everything
    // --create FOXPRO.DBF only create & run foxpro db
    // Don't create if we specify one dbf to read as paramstr(1)
    for i:=1 To Paramcount do
    begin
      if UpperCase(paramstr(i))=uppercase('--create') then Create:=true
      else if UpperCase(paramstr(i))=uppercase('--delete') then Delete:=true
    end;
    for i:=1 To Paramcount do
    begin
      if (UpperCase(paramstr(i))<>uppercase('--create')) and
      (UpperCase(paramstr(i))<>uppercase('--delete')) then
      begin
        if fileexists(ExpandFileName(paramstr(i))) then
          DBFs.Add(ExpandFileName(paramstr(i)))
        else if fileexists(ExpandFileName(paramstr(i)+'.dbf')) then
          DBFs.Add(ExpandFileName(paramstr(i)+'.dbf'))
        else
          if not(Create) then
            writeln('Invalid dbf file: ',paramstr(i),'. Ignoring it.')
          else
            DBFs.Add(ExpandFileName(ChangeFileExt(paramstr(i),'.dbf')));
      end;
    end;

    if Delete then
    begin
      writeln('** Going to delete existing databases: ');
      for i:=0 to DBFs.Count-1 do
      begin
        if fileexists(DBFs[i]) then sysutils.DeleteFile(DBFs[i]);
      end;
    end;

    if Create then
    begin
      writeln('** Going to create databases: ');
      if (DBFs.IndexOf(ExpandFileName('vfoxpro.dbf'))>-1) then
        CreateDBF('vfoxpro.dbf',30);
      if (DBFs.IndexOf(ExpandFileName('foxpro.dbf'))>-1) then
        CreateDBF('foxpro.dbf',25);
      if (DBFs.IndexOf(ExpandFileName('dbase7.dbf'))>-1) then
        CreateDBF('dbase7.dbf',7);
      if (DBFs.IndexOf(ExpandFileName('dbase4.dbf'))>-1) then
        CreateDBF('dbase4.dbf',4);
      if (DBFs.IndexOf(ExpandFileName('dbase3.dbf'))>-1) then
        CreateDBF('dbase3.dbf',4);
    end;

    // Process all dbfs if no files specified
    if DBFs.Count=0 then
      ListDBFs(DBFs);

    for FileNo:=0 to DBFs.Count-1 do
    begin
      if not(fileexists(DBFs[FileNo])) then
      begin
        // for some reason, fpc trunk suddenly returns the directory as well...
        //writeln('Sorry, file ',DBFs[FileNo],' does not exist.');
        break;
      end;
      MyDbf := TDbf.Create(nil);
      try
        try
          MyDbf.FilePath := ExtractFilePath(DBFs[FileNo]);
          MyDbf.TableName := ExtractFileName(DBFs[FileNo]);
          MyDbf.ReadOnly:=true;
          writeln('*** Opening: '+DBFs[FileNo]);
          MyDbf.Open;
          writeln('Database tablelevel: '+inttostr(MyDbf.TableLevel));

          RecCount:=1;
          while not(MyDbf.EOF) do
          begin
            PrintRecord(MyDBF, RecCount);
            {
            //Some extra tests for specific dbfs
            w32_functionstest;
            }
            MyDBF.Next;
            RecCount:=RecCount+1;
            writeln('');
          end;
          {
          //another set of tests for a specific file
          w32_examplestest;

          //and another one
          w32_orderstest;
          }
          MyDbf.Close;
        except
          on E: Exception do
          begin
            writeln('File ',FileNo,': error ',E.Message);
          end;
        end;
      finally
        MyDbf.Free;
      end;
    end;
  finally
    DBFs.Free;
  end;
  {
  writeln('Press enter to continue.');
  readln;
  }
end.

