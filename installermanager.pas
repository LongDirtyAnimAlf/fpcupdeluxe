unit installerManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TFPCupManager }

  TFPCupManager=class(Tobject)
  private
    FAllOptions: string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerFTP: string;
    FClean: boolean;
    FCompilerName: string;
    FCrossCPU_Target: string;
    FCrossLCL_Platform: string;
    FCrossOS_Target: string;
    FFPCDesiredRevision: string;
    FFPCDirectory: string;
    FFPCOPT: string;
    FFPCURL: string;
    FLazarusDesiredRevision: string;
    FLazarusDirectory: string;
    FLazarusOPT: string;
    FLazarusPrimaryConfigPath: string;
    FLazarusURL: string;
    FMakeDirectory: string;
    FOnlyModules: string;
    FShortCutName: string;
    FShortCutNameFpcup: string;
    FSkipModules: string;
    FVerbose: boolean;
  public
    property ShortCutName: string read FShortCutName write FShortCutName;
    property ShortCutNameFpcup:string read FShortCutNameFpcup write FShortCutNameFpcup;
    property CompilerName: string read FCompilerName write FCompilerName;
    property AllOptions:string read FAllOptions write FAllOptions;
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler;
    property BootstrapCompilerDirectory: string read FBootstrapCompilerDirectory write FBootstrapCompilerDirectory;
    property BootstrapCompilerFTP: string read FBootstrapCompilerFTP write FBootstrapCompilerFTP;
    property Clean: boolean read FClean write FClean;
    property CrossCPU_Target:string read FCrossCPU_Target write FCrossCPU_Target;
    property CrossLCL_Platform:string read FCrossLCL_Platform write FCrossLCL_Platform;
    property CrossOS_Target:string read FCrossOS_Target write FCrossOS_Target;
    property FPCDirectory: string read FFPCDirectory write FFPCDirectory;
    property FPCURL: string read FFPCURL write FFPCURL;
    property FPCOPT: string read FFPCOPT write FFPCOPT;
    property FPCDesiredRevision:string read FFPCDesiredRevision write FFPCDesiredRevision;
    property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
    property LazarusPrimaryConfigPath: string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath ;
    property LazarusURL: string read FLazarusURL write FLazarusURL;
    property LazarusOPT:string read FLazarusOPT write FLazarusOPT;
    property LazarusDesiredRevision:string read FLazarusDesiredRevision write FLazarusDesiredRevision;
    property MakeDirectory: string read FMakeDirectory write FMakeDirectory;
    property SkipModules:string read FSkipModules write FSkipModules;
    property OnlyModules:string read FOnlyModules write FOnlyModules;
    property Verbose:boolean read FVerbose write FVerbose;
    function Run: boolean;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TFPCupManager }

function TFPCupManager.Run: boolean;
begin
  result:=true; //be optimistic !!!
end;

constructor TFPCupManager.Create;
begin

end;

destructor TFPCupManager.Destroy;
begin
  inherited Destroy;
end;

end.

