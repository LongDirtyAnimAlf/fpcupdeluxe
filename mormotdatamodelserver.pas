unit mormotdatamodelserver;

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 SQLITE3_FASTCALL

interface

uses
  SysUtils,
  SynCommons,
  mORMot,
  mORMotSQLite3,
  SynSQLite3Static
  {$ifdef WITHLOG}
  ,SynLog
  {$endif}
  ;

{$I mORMotDataModel.inc}

type
  TSQLUp = class(TSQLUpBase)
  public
    procedure ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent);override;
  end;

  TDataServer = class(TSQLRestServerDB)
  private
    function GetTable(const aCountry,aFPCVersion,aOS,aDistro:string;const witherrors:boolean=false):TSQLTableJSON;
  protected
    fRootFolder: TFileName;
    fAppFolder: TFileName;
    aBaseSQL: RawUTF8;
  public
    constructor Create(const aRootFolder: TFileName); reintroduce;
    destructor Destroy; override;
    property RootFolder: TFileName read fRootFolder;
  published
    procedure GetInfoJSON(Ctxt: TSQLRestServerURIContext);
    procedure GetInfoHTML(Ctxt: TSQLRestServerURIContext);
  end;

implementation

uses
  SynSQLite3;

{ TSQLUp }

procedure TSQLUp.ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent);
begin
  inherited;
  // limit length of logentry
  if aOccasion=seAdd
     then if Length(Self.fLogEntry)>500
             then Delete(Self.fLogEntry,500,MaxInt);
end;

{ TDataServer }

constructor TDataServer.Create(const aRootFolder: TFileName);
var
  aModel:TSQLModel;
  U: TSQLAuthUser;
  aFunction:TUpFunction;
  i:integer;
  s:string;
begin
  fRootFolder := EnsureDirectoryExists(ExpandFileName(aRootFolder),true);
  fAppFolder := EnsureDirectoryExists(ExpandFileName(''),true);

  // define the log level
  {$ifdef WITHLOG}
  with TSQLLog.Family do begin
    PerThreadLog := ptIdentifiedInOnFile;
    Level := [sllError,sllCustom1];
    LevelStackTrace:=[sllNone];
    DestinationPath := fRootFolder+'..'+PathDelim+'log'+PathDelim;
    if not FileExists(DestinationPath) then
       CreateDir(DestinationPath);
  end;
  {$endif}

  aModel:=TSQLModel.Create([TSQLUp]);

  //TSQLMachine.AddFilterOrValidate('UpVersion',TSynValidateText.Create('{MinLength:3}'));
  //TSQLLogEntry.AddFilterOrValidate('UpOS',TSynValidateNonVoidText.Create);

  inherited Create(aModel,fRootFolder+'data.db3',True);

  // use this to limit length of logentry !!
  // procedure TSQLUp.ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent);
  include(Self.fOptions,rsoComputeFieldsBeforeWriteOnServerSide);

  DB.Synchronous := smNormal;
  DB.LockingMode := lmExclusive;

  CreateMissingTables;

  // change default passwords into some dark secrets ... ;-)
  U := Self.SQLAuthUserClass.Create;
  U.ClearProperties;
  Self.Retrieve('LogonName=?',[],['Admin'],U);
  U.PasswordPlain := ADMINPASS;
  Self.Update(U);
  U.ClearProperties;
  Self.Retrieve('LogonName=?',[],['Supervisor'],U);
  U.PasswordPlain := SUPERVISORPASS;
  Self.Update(U);
  U.ClearProperties;
  Self.Retrieve('LogonName=?',[],['User'],U);
  U.PasswordPlain := USERPASS;
  Self.Update(U);
  U.Free;

  // allow anybody to see some filtered results
  Self.ServiceMethodByPassAuthentication('GetInfoJSON');
  Self.ServiceMethodByPassAuthentication('GetInfoHTML');

  // make base SQL and translate enum into SQL
  aBaseSQL:='SELECT UpVersion,UpOS,UpDistro,UpWidget,Country,DateOfUse,(CASE UpFunction';
  for aFunction := Low(TUpFunction) to High(TUpFunction) do
  begin
    i:=ord(aFunction);
    s:=GetCaptionFromEnum(TypeInfo(TUpFunction),ord(aFunction));
    aBaseSQL:=aBaseSQL+' WHEN '+InttoStr(i)+' THEN '+QuotedStr(s);
  end;
  aBaseSQL:=aBaseSQL+' ELSE ''ErrorUnknown'' END) AS ''Action'',FPCVersion,LazarusVersion,CrossCPUOS,ExtraData,LogEntry FROM Up';

  //AddToServerWrapperMethod(Self,
  //        [fRootFolder+'..'+PathDelim+'templates'+PathDelim,'..\mORMotCurrent\CrossPlatform\templates','..\..\mORMotCurrent\CrossPlatform\templates']);
  {$ifdef WITHLOG}
  TSQLLog.Add.Log(sllCustom1,'Fpcupdeluxe server started !!');
  {$endif}
end;

destructor TDataServer.Destroy;
begin
  {$ifdef WITHLOG}
  TSQLLog.Add.Log(sllCustom1,'Closing fpcupdeluxe server.');
  {$endif}
  fModel.Free;
  Inherited;
end;

function TDataServer.GetTable(const aCountry,aFPCVersion,aOS,aDistro:string;const witherrors:boolean=false):TSQLTableJSON;
var
  aSQL:string;
begin
  aSQL:=aBaseSQL;
  if Length(aCountry)>0 then
  begin
    if Pos('WHERE',aSQL)>0 then aSQL:=aSQL+' AND ' else aSQL:=aSQL+' WHERE ' ;
    aSQL:=aSQL+'Country = ' + QuotedStr(aCountry);
  end;
  if Length(aFPCVersion)>0 then
  begin
    if Pos('WHERE',aSQL)>0 then aSQL:=aSQL+' AND ' else aSQL:=aSQL+' WHERE ' ;
    aSQL:=aSQL+'FPCVersion = ' + QuotedStr(aFPCVersion);
  end;
  if Length(aOS)>0 then
  begin
    if Pos('WHERE',aSQL)>0 then aSQL:=aSQL+' AND ' else aSQL:=aSQL+' WHERE ' ;
    aSQL:=aSQL+'UpOS LIKE ' + QuotedStr('%'+aOS+'%');
  end;
  if Length(aDistro)>0 then
  begin
    if Pos('WHERE',aSQL)>0 then aSQL:=aSQL+' AND ' else aSQL:=aSQL+' WHERE ' ;
    aSQL:=aSQL+'UpDistro LIKE ' + QuotedStr('%'+aDistro+'%');
  end;
  if (NOT witherrors) then
  begin
    if Pos('WHERE',aSQL)>0 then aSQL:=aSQL+' AND ' else aSQL:=aSQL+' WHERE ' ;
    aSQL:=aSQL+'LogEntry = ' + QuotedStr('Success !');
  end;
  aSQL:=aSQL+';';
  result:=ExecuteList([TSQLUp],aSQL);
end;

procedure TDataServer.GetInfoHTML(Ctxt: TSQLRestServerURIContext);
var
  T:TSQLTableJSON;
  aCountry,aFPCVersion,aOS,aDistro:string;

begin
  case Ctxt.Method of
    mGET:
    begin
      aCountry:=Ctxt.InputStringOrVoid['Country'];
      aFPCVersion:=Ctxt.InputStringOrVoid['FPCVersion'];
      aFPCVersion:=Ctxt.InputStringOrVoid['FPCVersion'];
      aOS:=Ctxt.InputStringOrVoid['OS'];
      aDistro:=Ctxt.InputStringOrVoid['Distro'];
      if Length(Ctxt.InputStringOrVoid['ShowErrors'])>0
         then T:=GetTable(aCountry,aFPCVersion,aOS,aDistro,true)
         else T:=GetTable(aCountry,aFPCVersion,aOS,aDistro);
      Ctxt.Returns(T.GetHtmlTable,HTTP_SUCCESS,HTML_CONTENT_TYPE_HEADER);
      T.Free;
    end;
  end;
end;

procedure TDataServer.GetInfoJSON(Ctxt: TSQLRestServerURIContext);
var
  T:TSQLTableJSON;
  aCountry,aFPCVersion,aOS,aDistro:string;
begin
  case Ctxt.Method of
    mGET:
    begin
      aCountry:=Ctxt.InputStringOrVoid['Country'];
      aFPCVersion:=Ctxt.InputStringOrVoid['FPCVersion'];
      aOS:=Ctxt.InputStringOrVoid['OS'];
      aDistro:=Ctxt.InputStringOrVoid['Distro'];
      if Length(Ctxt.InputStringOrVoid['ShowErrors'])>0
         then T:=GetTable(aCountry,aFPCVersion,aOS,aDistro,true)
         else T:=GetTable(aCountry,aFPCVersion,aOS,aDistro);
      // this will return an escaped json with result as title
      //Ctxt.Results([T.GetJSONValues(True)]);
      // this will return raw json without title
      Ctxt.Returns(T.GetJSONValues(True));
      T.Free;
    end;
  end;
end;

initialization
{$ifndef ISDELPHI2010}
{$ifndef HASINTERFACERTTI} // circumvent a old FPC bug
TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TUpFunction));
{$endif}
{$endif}

end.
