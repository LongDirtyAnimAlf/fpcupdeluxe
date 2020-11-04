{$I secrets.inc}

type
  TUpFunction = (
        ufUnknown,
        ufInstallFPC,
        ufInstallLAZ,
        ufInstallFPCLAZ,
        ufInstallCross,
        ufUninstallCross,
        ufInstallModule,
        ufUninstallModule,
        ufCheckMakefile,
        ufCreateStartup
      );

  // keep it very simple: all in one record !!
  TSQLUpBase = class(TSQLRecord)
  private
    fUpVersion      : RawUTF8;
    fUpOS           : RawUTF8;
    fUpDistro       : RawUTF8;
    fUpWidget       : RawUTF8;
    fUpInstallDir   : RawUTF8;
    fIPV4Address    : RawUTF8;
    fIPV6Address    : RawUTF8;
    fCity           : RawUTF8;
    fCountry        : RawUTF8;
    fLatitude       : RawUTF8;
    fLongitude      : RawUTF8;
    fUpFunction     : TUpFunction;
    fFPCVersion     : RawUTF8;
    fLazarusVersion : RawUTF8;
    fCrossCPUOS     : RawUTF8;
    fExtraData      : variant;
    fLogEntry       : RawUTF8;
    fDateOfUse      : TDateTime;
  published
    property UpVersion: RawUTF8 index 20 read fUpVersion write fUpVersion;
    property UpOS: RawUTF8 index 30 read fUpOS write fUpOS;
    property UpDistro: RawUTF8 index 30 read fUpDistro write fUpDistro;
    property UpWidget: RawUTF8 index 30 read fUpWidget write fUpWidget;
    property UpInstallDir: RawUTF8 index 255 read fUpInstallDir write fUpInstallDir;
    property IPV4Address: RawUTF8 index 15 read fIPV4Address write fIPV4Address;
    property IPV6Address: RawUTF8 index 40 read fIPV6Address write fIPV6Address;
    property City: RawUTF8 read fCity write fCity;
    property Country: RawUTF8 read fCountry write fCountry;
    property Latitude: RawUTF8 read fLatitude write fLatitude;
    property Longitude: RawUTF8 read fLongitude write fLongitude;
    property UpFunction: TUpFunction read fUpFunction write fUpFunction;
    property FPCVersion: RawUTF8 index 15 read fFPCVersion write fFPCVersion;
    property LazarusVersion: RawUTF8 index 15 read fLazarusVersion write fLazarusVersion;
    property CrossCPUOS: RawUTF8 index 40 read fCrossCPUOS write fCrossCPUOS;
    property ExtraData: variant read fExtraData write fExtraData;
    property LogEntry: RawUTF8 index 500 read fLogEntry write fLogEntry;
    property DateOfUse: TDateTime read fDateOfUse write fDateOfUse;
  end;

const
  //AWS_HOST_IP  = '54.194.211.233';
  //AWS_HOST_IP  = '52.50.101.131';
  AWS_HOST_IP  = 'fpcuplogger.batterybutcher.com';
  IP_DEFAULT   = AWS_HOST_IP;
  //IP_DEFAULT   = '127.0.0.1';
  PORT_DEFAULT_SERVER = 8890;
  PORT_DEFAULT_CLIENT = 8880;
  USER_DEFAULT = 'User';
  //PASS_DEFAULT = 'synopse';
  PASS_DEFAULT = USERPASS;

