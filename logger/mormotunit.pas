unit mormotunit;

{$mode delphi}

interface

uses
  SysUtils, SynCommons, mORMot, mORMotSQLite3, recorddata, machinelogdata, projectdata, persondata;

const
  // Settings SOHIT LogEntry server
  //IP_DEFAULT   = '127.0.0.1';
  //IP_DEFAULT   = '192.168.1.175';
  //IP_DEFAULT   = '192.168.2.14';
  //IP_DEFAULT   = '62.207.77.22';
  //PORT_DEFAULT = '8880';
  PORT_DEFAULT = '80';
  //USER_DEFAULT = 'User';
  //PASS_DEFAULT = 'synopse';
  CONNECTIONTIMEOUT = 5000;

type
  TSharedmORMotData = class(TObject)
  private
    FClient           : TSQLRestClient;
    FLogModel         : TSQLModel;
    {$ifdef LocalLog}
    FProjectModel     : TSQLModel;
    FPersonModel      : TSQLModel;
    FDocModel         : TSQLModel;
    {$endif}
    FConnected        : boolean;
    FSQLPersonRecord  : TSQLPerson;
    FSQLProjectRecord : TSQLProject;
    FSQLMachineRecord : TSQLMachine;
    FSQLLogEntryRecord: TSQLLogEntry;
    FLogEntryFields   : string;
    FOldMachineID     : string;
    FPastDate         : TDate;
    {$ifndef LocalLog}
    FHost,FPort       : string;
    {$endif}
    FUser,FPassword   : string;
    FLogDB            : TSQLRestServerDB;
    {$ifdef LocalLog}
    FLogDBFile        : string;
    FProjectDBFile    : string;
    FPersonDBFile     : string;
    FDocDBFile        : string;
    FProjectDB        : TSQLRestServerDB;
    FPersonDB         : TSQLRestServerDB;
    FDocDB            : TSQLRestServerDB;
    {$endif}
    FFilterEntries    : boolean;
    function GetName(aSQLRecord:TSQLRecord;aID:TID):string;
    function GetSimpleTable(aRecord:TSQLRecord):boolean;
    {property setters}
    procedure SetWidthFields(aSQLTable:TSQLTable);
    procedure SetHost(value:string);
    procedure SetPort(value:string);
    function GetClient:TSQLRestClient;
    procedure SetPassword(value:string);
  protected
    property LogModel: TSQLModel read FLogModel;
    {$ifdef LocalLog}
    property ProjectModel: TSQLModel read FProjectModel;
    property PersonModel: TSQLModel read FPersonModel;
    property DocModel: TSQLModel read FDocModel;
    {$endif}
  public
    constructor Create;
    destructor Destroy;override;
    function SetSelectedMachine(aRow:integer):boolean;
    function GetPersonTable:boolean;
    function GetProjectTable:boolean;
    function GetMachineTable:boolean;
    function GetLogEntryTable(const allentries:boolean=false):boolean;
    function GetPersonName(aID:TID):string;
    function GetProjectName(aID:TID):string;
    function Connect:boolean;
    function Authenticate:boolean;
    function AddRecord(aRecord:TSQLRecord):TID;overload;
    function AddRecord(aRecord:TSQLRecord;const CustomCSVFields: RawUTF8):TID;overload;
    function EditRecord(aRecord:TSQLRecord;aFieldName:string;aValue:RawUTF8):TID;
    function DeleteRecord(aRecord:TSQLRecord):boolean;
    function SearchRecordRow(aRecord:TSQLRecord;aValue:string;aField:string=''):integer;
    function SearchRecordID(aRecord:TSQLRecord;aValue:string;aField:string=''):TID;
    function AddLogEntry:TID;

    function LocalUpdateDocument(aRecord:TSQLRecord;const aFileName:string;const ForDeletion:boolean=false):boolean;
    function LocalRetrieveDocument(aRecord:TSQLRecord;aFilename:string;out aFile:string):boolean;overload;
    function LocalRetrieveDocument(aID:TID;out aFile:string):boolean;overload;
  published
    property Client:TSQLRestClient read FClient;
    property Server:TSQLRestServerDB read FLogDB;
    property SQLPerson : TSQLPerson read FSQLPersonRecord;
    property SQLProject : TSQLProject read FSQLProjectRecord;
    property SQLMachine : TSQLMachine read FSQLMachineRecord;
    property SQLLogEntry : TSQLLogEntry read FSQLLogEntryRecord;
    property PastDate : TDate read FPastDate write FPastDate;
    {$ifndef LocalLog}
    property Host:string read FHost write SetHost;
    property Port:string read FPort write SetPort;
    {$endif}
    property User:string read FUser write FUser;
    property Password:string read FPassword write FPassword;
    {$ifdef LocalLog}
    property LogDBFile:string read FLogDBFile write FLogDBFile;
    property ProjectDBFile:string read FProjectDBFile write FProjectDBFile;
    property PersonDBFile:string read FPersonDBFile write FPersonDBFile;
    property DocDBFile:string read FDocDBFile write FDocDBFile;
    {$endif}
    property ClearPassword:string write SetPassword;
    property Connected:boolean read FConnected;
  end;

implementation

uses
  dateutils,
  SynSQLite3,
  SynSQLite3Static,
  mORMotHttpClient;

constructor TSharedmORMotData.Create;
begin
  inherited;

  FClient:=nil;
  FLogDB:=nil;

  FLogModel          := CreateLogModel; // from MachineData unit
  {$ifdef LocalLog}
  FProjectModel      := CreateProjectModel; // from MachineData unit
  FPersonModel       := CreatePersonModel; // from MachineData unit
  FDocModel          := CreateDocModel; // from MachineData unit
  {$endif}

  FSQLPersonRecord   := TSQLPerson.Create;
  FSQLProjectRecord  := TSQLProject.Create;
  FSQLMachineRecord  := TSQLMachine.Create;
  FSQLLogEntryRecord := TSQLLogEntry.Create;

  FOldMachineID      := '';
  FPastDate          := IncDay(Now,-60);

  //defaults !

  with FSQLLogEntryRecord.RecordProps do FLogEntryFields:='ID,'+CSVFromFieldBits(FieldBitsFromExcludingCSV('MachineID,Picture'));

  FConnected        := False;
  FFilterEntries    := False;

  {$ifndef LocalLog}
  FHost             := IP_DEFAULT;
  FPort             := PORT_DEFAULT;
  {$endif}

  FUser             := USER_DEFAULT;
  ClearPassword     := 'synopse';

  {$ifdef LocalLog}
  FLogDBFile     := 'logdata.db3';
  FProjectDBFile := 'projectdata.db3';
  FPersonDBFile  := 'persondata.db3';
  FDocDBFile     := 'logdocdata.db3';
  {$endif}
end;

destructor TSharedmORMotData.Destroy;
begin
  FConnected:=false;

  FreeAndNil(FSQLPersonRecord);
  FreeAndNil(FSQLProjectRecord);
  FreeAndNil(FSQLMachineRecord);
  FreeAndNil(FSQLLogEntryRecord);

  FreeAndNil(FClient);

  {$ifdef LocalLog}
  FreeAndNil(FDocDB);
  FreeAndNil(FProjectDB);
  FreeAndNil(FPersonDB);
  FreeAndNil(FLogDB);
  {$endif}

  LogModel.Free;
  {$ifdef LocalLog}
  PersonModel.Free;
  ProjectModel.Free;
  DocModel.Free;
  {$endif}

  inherited Destroy;
end;

function TSharedmORMotData.GetClient:TSQLRestClient;
begin
  {$ifdef LocalLog}
  FLogDB:=TSQLRestServerDB.Create(LogModel,LogDBFile,True);

  FProjectDB:= TSQLRestServerDB.Create(ProjectModel,ProjectDBFile);
  with FProjectDB do
  begin
    DB.Synchronous := smOff;
    DB.LockingMode := lmExclusive;
    CreateMissingTables;
  end;

  FPersonDB:= TSQLRestServerDB.Create(PersonModel,PersonDBFile);
  with FPersonDB do
  begin
    DB.Synchronous := smOff;
    DB.LockingMode := lmExclusive;
    CreateMissingTables;
  end;

  FDocDB:= TSQLRestServerDB.Create(DocModel,DocDBFile);
  with FDocDB do
  begin
    DB.Synchronous := smOff;
    DB.LockingMode := lmExclusive;
    CreateMissingTables;
  end;

  with FLogDB do
  begin
    RemoteDataCreate(TSQLProject,FProjectDB);
    RemoteDataCreate(TSQLPerson,FPersonDB);
    RemoteDataCreate(TSQLDocument,FDocDB);
    DB.Synchronous := smOff;
    DB.LockingMode := lmExclusive;
    CreateMissingTables;
  end;
  result := TSQLRestClientDB.Create(FLogDB);
 {$else}
  result := TSQLHttpClient.Create(Host,Port,LogModel,CONNECTIONTIMEOUT,CONNECTIONTIMEOUT,CONNECTIONTIMEOUT*2);
 {$endif}
end;

procedure TSharedmORMotData.SetHost(value:string);
begin
  {$ifndef LocalLog}
  if value<>FHost then
  begin
    FConnected:=false;
    FHost:=value;
    if Assigned(Client) then
    begin
      Client.Free;
      FClient:=GetClient;
    end;
  end;
  {$endif}
end;

procedure TSharedmORMotData.SetPort(value:string);
begin
  {$ifndef LocalLog}
  if value<>FPort then
  begin
    FConnected:=false;
    FPort:=value;
    if Assigned(Client) then
    begin
      Client.Free;
      FClient:=GetClient;
    end;
  end;
  {$endif}
end;

function TSharedmORMotData.Connect:boolean;
begin
  FConnected:=false;
  FreeAndNil(FClient);
  {$ifdef LocalLog}
  FreeAndNil(FDocDB);
  FreeAndNil(FProjectDB);
  FreeAndNil(FPersonDB);
  FreeAndNil(FLogDB);
  {$endif}
  FClient:=GetClient;
  if Client.InheritsFrom(TSQLRestClientURI)
     then result:=TSQLRestClientURI(Client).ServerTimeStampSynchronize
     else result:=true;
end;

function TSharedmORMotData.Authenticate:boolean;
begin
  result:=false;
  if Assigned(Client) then
  begin
    result:=true;
    if Client.InheritsFrom(TSQLRestClientURI) then
    begin
      // first try Windows Authentication
      result:=TSQLRestClientURI(Client).SetUser('','');
      if not result then
      begin
        // normal Authentication
        result:=TSQLRestClientURI(Client).SetUser(FUser,FPassword,True);
      end;
    end;
  end;
  FConnected:=result;
end;

procedure TSharedmORMotData.SetPassword(value:string);
begin
  if Length(value)=0
     then FPassword:=''
     else FPassword:=TSQLAuthUser.ComputeHashedPassword(value);
end;



function TSharedmORMotData.SetSelectedMachine(aRow:integer):boolean;
begin
  result:=false;
  if NOT Assigned(SQLMachine.FillTable) then exit;
  if (aRow<1) OR (aRow>SQLMachine.FillTable.RowCount) then exit;
  result:=SQLMachine.FillRow(aRow);
  if (NOT result) then exit;
  if (FOldMachineID<>SQLMachine.MachineID) then
  begin
    FOldMachineID:=SQLMachine.MachineID;
    // new logentry table needed
    // bit tricky : recreate record
    if Assigned(FSQLLogEntryRecord) then FSQLLogEntryRecord.Destroy;
    FSQLLogEntryRecord := TSQLLogEntry.Create;
    //FSQLLogEntryRecord := TSQLLogEntry.CreateJoined();
  end;
end;

procedure TSharedmORMotData.SetWidthFields(aSQLTable:TSQLTable);
var
  aFieldType : TSQLFieldType;
  i:integer;
begin
  if NOT Assigned(aSQLTable) then exit;

  for i:=0 to aSQLTable.FieldCount-1 do
  begin
    aFieldType:=aSQLTable.FieldType(i,nil);
    if aFieldType in [sftAnsiText,sftUTF8Text,sftUTF8Custom] then aSQLTable.SetFieldType(i,aFieldType,nil,100);
    if aSQLTable.FieldNames[i]='LogGeneral' then aSQLTable.SetFieldType(i,aFieldType,nil,500);
  end;
end;

function TSharedmORMotData.GetSimpleTable(aRecord:TSQLRecord):boolean;
var
  aSQLRecord:TSQLRecord;
  NewTable:boolean;
  refreshed:boolean;
  aID:TID;
begin

  result:=Connected;
  if not result then exit;

  aSQLRecord:=aRecord;

  NewTable:=(Not Assigned(aSQLRecord.FillTable));

  try
    if NOT NewTable then
    begin
      //aCurrentRow:=aSQLRecord.FillCurrentRow;
      aID:=aSQLRecord.ID;
      if Client.InheritsFrom(TSQLRestClientURI) then result:=TSQLRestClientURI(Client).UpdateFromServer([aSQLRecord.FillTable],Refreshed);
      //aSQLRecord.FillContext.ReMap(aSQLRecord,ctnNoCheck);
      if result then
      begin
        //aSQLRecord.FillRow(aSQLRecord.FillTable.RowFromID(aID));
      end;
    end
    else
    begin
      if aSQLRecord.InheritsFrom(TSQLMachine) then
      begin
        TSQLMachine(aSQLRecord).FillPrepare(Client,'ORDER BY length(MachineID),MachineID');
        result:=TSQLMachine(aSQLRecord).FillOne;
      end;

      if aSQLRecord.InheritsFrom(TSQLLogEntry) then
      begin
         if Length(SQLMachine.MachineID)>0 then
         begin
           if FFilterEntries
              then result:=TSQLLogEntry(aSQLRecord).FillPrepare(Client,'MachineID=? and LogDate>? order by LogDate',[SQLMachine.ID,DateTimeToSQL(FPastDate)],FLogEntryFields)
              else result:=TSQLLogEntry(aSQLRecord).FillPrepare(Client,'MachineID=? order by LogDate',[SQLMachine.ID],FLogEntryFields);
         end;
      end;

      if aSQLRecord.InheritsFrom(TSQLPerson)
         then TSQLPerson(aSQLRecord).FillPrepare(Client,'order by FirstName');

      if aSQLRecord.InheritsFrom(TSQLProject)
         then TSQLProject(aSQLRecord).FillPrepare(Client,'ProjectNumber NOT LIKE '+QuotedStr('201%')+' ORDER BY length(ProjectNumber),ProjectNumber');

      // force set fieldwidths
      SetWidthFields(aSQLRecord.FillTable);
    end;
  except
    on E: Exception do
    begin
      //ShowException(E);
    end;
  end;

  result:=(result AND Assigned(aSQLRecord.FillTable));
end;

function TSharedmORMotData.GetPersonTable:boolean;
begin
  result:=GetSimpleTable(SQLPerson);
end;

function TSharedmORMotData.GetProjectTable:boolean;
begin
  result:=GetSimpleTable(SQLProject);
end;

function TSharedmORMotData.GetMachineTable:boolean;
begin
  result:=GetSimpleTable(SQLMachine);
end;

function TSharedmORMotData.GetName(aSQLRecord:TSQLRecord;aID:TID):string;
var
  aRow:integer;
begin
  if (aSQLRecord.InheritsFrom(TSQLPerson)) AND (NOT Assigned(aSQLRecord.FillTable)) then GetPersonTable;
  if (aSQLRecord.InheritsFrom(TSQLProject)) AND (NOT Assigned(aSQLRecord.FillTable)) then GetProjectTable;

  if aID<1 then
  begin
    result:='Unknown';
    if aID=-1 then //get comma seperated list of all names
    begin
      if aSQLRecord.FillRewind then
      begin
        while aSQLRecord.FillOne do
        begin
          if aSQLRecord.InheritsFrom(TSQLPerson) then result:=result+','+TSQLPerson(aSQLRecord).FirstName;
          if aSQLRecord.InheritsFrom(TSQLProject) then result:=result+','+TSQLProject(aSQLRecord).ProjectNumber;
        end;
      end;
    end;
  end
  else
  begin
    aRow:=aSQLRecord.FillTable.RowFromID(aID,true);
    if aRow=-1 then result:='Unknown' else
    begin
      aSQLRecord.FillRow(aRow);
      if aSQLRecord.InheritsFrom(TSQLPerson) then result:=TSQLPerson(aSQLRecord).FirstName;
      if aSQLRecord.InheritsFrom(TSQLProject) then result:=TSQLProject(aSQLRecord).ProjectNumber;
    end;
  end;
end;

function TSharedmORMotData.GetPersonName(aID:TID):string;
begin
  result:=GetName(SQLPerson,aID);
end;

function TSharedmORMotData.GetProjectName(aID:TID):string;
begin
  result:=GetName(SQLProject,aID);
end;


function TSharedmORMotData.GetLogEntryTable(const allentries:boolean=false):boolean;
var
  PreviousFFilterEntries:boolean;
begin
  PreviousFFilterEntries:=FFilterEntries;
  FFilterEntries:=(NOT allentries);
  if PreviousFFilterEntries<>FFilterEntries then
  begin
    // new logentry table needed ... tricky
    if Assigned(FSQLLogEntryRecord) then FSQLLogEntryRecord.Destroy;
    FSQLLogEntryRecord := TSQLLogEntry.Create;
  end;
  result:=GetSimpleTable(SQLLogEntry);
end;

function TSharedmORMotData.AddRecord(aRecord:TSQLRecord):TID;
begin
  result:=0;
  if (NOT Connected) then exit;
  result:=Client.AddOrUpdate(aRecord);
  GetSimpleTable(aRecord);
end;

function TSharedmORMotData.AddRecord(aRecord:TSQLRecord;const CustomCSVFields: RawUTF8):TID;
begin
  result:=0;
  if (NOT Connected) then exit;
  if Client.Update(aRecord,CustomCSVFields) then result:=aRecord.ID;
  GetSimpleTable(aRecord);
end;

function TSharedmORMotData.EditRecord(aRecord:TSQLRecord;aFieldName:string;aValue:RawUTF8):TID;
var
  aID:TID;
begin
  result:=0;
  if (NOT Connected) then exit;

  if aRecord.InheritsFrom(TSQLLogEntry) then
  begin
    // handle update of special fields
    if aFieldName='Person' then
    begin
      aID:=SearchRecordID(SQLPerson,aValue,'FirstName');
      TSQLLogEntry(aRecord).Person:=Pointer(aID);
    end
    else
    if aFieldName='Project' then
    begin
      aID:=SearchRecordID(SQLProject,aValue,'ProjectNumber');
      TSQLLogEntry(aRecord).Project:=Pointer(aID);
    end
    else
    if aFieldName='LocationNumber' then
    begin
      if (UpperCase(aValue)='UNKNOWN')
         then TSQLLogEntry(aRecord).LocationNumber:=0
         else TSQLLogEntry(aRecord).LocationNumber:=StrtoIntDef(aValue,0);
    end

    // all other field updates of TSQLLogEntry
    else aRecord.SetFieldValue(aFieldName,PUTF8Char(aValue));
  end
  // all other field updates of tables other than TSQLLogEntry
  else aRecord.SetFieldValue(aFieldName,PUTF8Char(aValue));

  result:=AddRecord(aRecord,aFieldName);
end;


function TSharedmORMotData.DeleteRecord(aRecord:TSQLRecord):boolean;
begin
  result:=Connected;
  if not result then exit;
  result:=Client.Delete(aRecord.RecordClass,aRecord.ID);
  GetSimpleTable(aRecord);
end;

function TSharedmORMotData.SearchRecordRow(aRecord:TSQLRecord;aValue:string;aField:string=''):integer;
var
  aSearchField:string;
begin
  if Assigned(aRecord.FillTable) then
  begin
    with aRecord.FillTable do
    begin
      if Length(aField)=0
         then aSearchField:=aRecord.RecordProps.MainFieldName(True)
         else aSearchField:=aField;
      result:=SearchFieldEquals(aValue,FieldIndex(aSearchField));
    end;
  end else result:=0;
end;

function TSharedmORMotData.SearchRecordID(aRecord:TSQLRecord;aValue:string;aField:string=''):TID;
var
  aRow:integer;
begin
  result:=0;
  aRow:=SearchRecordRow(aRecord,aValue,aField);
  if Assigned(aRecord.FillTable) then result:=aRecord.FillTable.IDColumnHiddenValue(aRow);
end;

function TSharedmORMotData.AddLogEntry:TID;
var
  aRowCount:integer;
begin
  aRowCount:=SQLLogEntry.FillTable.RowCount;
  if aRowCount>0 then
  begin
    SQLLogEntry.FillRow(aRowCount);
    SQLLogEntry.LogGeneral:='';
    SQLLogEntry.LogSalt:='';
    SQLLogEntry.LogShine:='';
  end;
  SQLLogEntry.SetMachineID(SQLMachine);
  SQLLogEntry.IDValue:=0;
  SQLLogEntry.LogDate:=Now;
  result:=AddRecord(SQLLogEntry);
  GetSimpleTable(SQLLogEntry);
end;

function TSharedmORMotData.LocalUpdateDocument(aRecord:TSQLRecord;const aFileName:string;const ForDeletion:boolean=false):boolean;
var
  aDoc:TSQLDocument;
  aDocEntry:TDocument;
  aID:TID;
  aArray:TObjectDynArray;
  aIndex:integer;
begin
  result:=false;
  if NOT Connected then exit;
  aID:=0;

  if NOT ForDeletion then
  begin
    aDoc:=TSQLDocument.CreateWithFile(aFileName,aRecord.ID);
    try
      aID:=Client.AddOrUpdate(aDoc);
      if (aID>0) then Client.UpdateBlobFields(aDoc);
    finally
      aDoc.Free;
    end;
    if (aID>0) then
    begin
      aRecord.DynArray('Document').FindAndAddIfNotExisting(TDocument.Create(aID,aFileName));
      aID:=AddRecord(aRecord,'Document');
      result:=(aID>0);
    end;
  end
  else
  begin
    aDoc:=TSQLDocument.Create(Client,'OwnerID = ? AND FullPath LIKE ?',[aRecord.ID,'%'+aFileName]);
    try
      aID:=aDoc.ID;
      if aID>0 then
      begin
        if Client.Delete(TSQLDocument,aID) then
        begin
          aDocEntry:=TDocument.Create(aID,aFileName);
          try
            aIndex:=aRecord.DynArray('Document').FindAndDelete(aDocEntry,nil,@DocumentSort);
            aID:=AddRecord(aRecord,'Document');
            result:=(aID>0);
          finally
            aDocEntry.Free;
          end;
        end;
      end;
    finally
      aDoc.Free;
    end;
  end;
end;

function TSharedmORMotData.LocalRetrieveDocument(aRecord:TSQLRecord;aFilename:string;out aFile:string):boolean;
var
  aSQLDoc:TSQLDocument;
  aID:TID;
  m:integer;
begin
  result:=false;
  if NOT Connected then exit;
  aID:=aRecord.ID;

  if (aID>0) then
  begin
    for m:=0 to High(TSQLLogEntry(aRecord).Documents) do
    begin
      aID:=0;
      if TSQLLogEntry(aRecord).Documents[m].FileName=aFileName then
      begin
        aID:=TSQLLogEntry(aRecord).Documents[m].DocumentID;
        aSQLDoc:=TSQLDocument.Create(Client,aID);
        if aSQLDoc.ID>0 then
        begin
          Client.RetrieveBlobFields(aSQLDoc);
          aFile:=aSQLDoc.FileContents;
          result:=True;
        end;
        aSQLDoc.Free;
      end;
    end;
  end;
end;

function TSharedmORMotData.LocalRetrieveDocument(aID:TID;out aFile:string):boolean;
var
  aSQLDoc:TSQLDocument;
begin
  result:=false;
  if NOT Connected then exit;
  aSQLDoc:=TSQLDocument.Create(Client,aID);
  try
    if aSQLDoc.ID>0 then
    begin
      Client.RetrieveBlobFields(aSQLDoc);
      aFile:=aSQLDoc.FileContents;
      result:=True;
    end;
  finally
    aSQLDoc.Free;
  end;
end;


end.

