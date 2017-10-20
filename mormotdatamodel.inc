type
  TSQLMachine = class(TSQLRecord)
  private
    fMachineID: RawUTF8;
    fMachineBrand: RawUTF8;
    fMachineType: RawUTF8;
    fPicture: TSQLRawBlob;
  published
    property MachineID: RawUTF8 index 40 read fMachineID write fMachineID stored AS_UNIQUE;
    property MachineBrand: RawUTF8 index 30 read fMachineBrand write fMachineBrand;
    property MachineType: RawUTF8 index 40 read fMachineType write fMachineType;
    property Picture: TSQLRawBlob read fPicture write fPicture;
  end;

  TSQLPerson = class(TSQLRecord)
  private
    fLastName: RawUTF8;
    fFirstName: RawUTF8;
    fMiddleName: RawUTF8;
    fPicture: TSQLRawBlob;
  published
    property LastName: RawUTF8 index 40 read fLastName write fLastName;
    property FirstName: RawUTF8 index 20 read fFirstName write fFirstName;
    property MiddleName: RawUTF8 index 20 read fMiddleName write fMiddleName;
    property Picture: TSQLRawBlob read fPicture write fPicture;
  end;

  TSQLLogEntry = class(TSQLRecord)
  private
    fLogEntry: RawUTF8;
    fMachineID: TSQLMachine;
    fPerson: TSQLPerson;
    fLogDate: TDateTime;
    fPicture: TSQLRawBlob;
  published
    property LogEntry: RawUTF8 index 400 read fLogEntry write fLogEntry;
    property MachineID: TSQLMachine read fMachineID write fMachineID;
    property Person: TSQLPerson read fPerson write fPerson;
    property LogDate: TDateTime read fLogDate write fLogDate;
    property Picture: TSQLRawBlob read fPicture write fPicture;
  end;

const
  // Settings SOHIT LogEntry server
  IP_DEFAULT   = '192.168.1.175';
  PORT_DEFAULT = 8880;
  USER_DEFAULT = 'User';
  PASS_DEFAULT = 'synopse';

