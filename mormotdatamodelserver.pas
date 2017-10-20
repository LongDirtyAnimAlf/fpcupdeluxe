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
  TDataServer = class(TSQLRestServerDB)
  protected
    fRootFolder: TFileName;
    fAppFolder: TFileName;
  public
    constructor Create(const aRootFolder: TFileName); reintroduce;
    destructor Destroy; override;
    property RootFolder: TFileName read fRootFolder;
  end;

implementation

uses
  SynSQLite3;

{ TDataServer }

constructor TDataServer.Create(const aRootFolder: TFileName);
var
  aModel:TSQLModel;
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
    //DestinationPath := fRootFolder+'..'+PathDelim+'log';
    if not FileExists(DestinationPath) then
       CreateDir(DestinationPath);
  end;
  {$endif}

  // prepare the server SQLite3 storage
  //inherited Create(DataModel(aRootURI),ChangeFileExt(paramstr(0),'.db3'));
  aModel:=TSQLModel.Create([TSQLMachine,TSQLPerson,TSQLLogEntry]);

  //TSQLMachine.AddFilterOrValidate('MachineID',TSynValidateText.Create('{MinLength:3}'));
  //TSQLLogEntry.AddFilterOrValidate('LogEntry',TSynValidateNonVoidText.Create);


  inherited Create(aModel,fRootFolder+'data.db3',True);

  DB.Synchronous := smNormal;
  DB.LockingMode := lmExclusive;

  CreateMissingTables;

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

end.
