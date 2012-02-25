unit installerManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,installerCore,installerFpc,installerLazarus,installerUniversal;

type

  { TFPCupManager }

  TFPCupManager=class(Tobject)
  private
    FAllOptions: string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
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
  protected
    LogFile:Text;
    ModuleList:TStringList;
    function CreateOnly(OnlyModules:string):boolean;
    function LoadModuleList:boolean;
    function DeleteOnly:boolean;
  public
    property ShortCutName: string read FShortCutName write FShortCutName;
    property ShortCutNameFpcup:string read FShortCutNameFpcup write FShortCutNameFpcup;
    property CompilerName: string read FCompilerName write FCompilerName;
    property AllOptions:string read FAllOptions write FAllOptions;
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler;
    property BootstrapCompilerDirectory: string read FBootstrapCompilerDirectory write FBootstrapCompilerDirectory;
    property BootstrapCompilerURL: string read FBootstrapCompilerURL write FBootstrapCompilerURL;
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


  TExecState=(ESNever,ESFailed,ESSucceeded);

  TSequenceAttributes=record
    EntryPoint:integer;  //instead of rescanning the sequence table everytime, we can as well store the index in the table
    Executed:TExecState; //  Reset to ESNever at sequencer start up
  end;
  PSequenceAttributes=^TSequenceAttributes;

  TKeyword=(SMdeclare, SMdo, SMrequire, SMexec, SMend, SMcleanmodule, SMgetmodule, SMbuildmodule,
    SMuninstallmodule, SMconfigmodule, SMSetLCL, SMSetOS, SMSetCPU);

  TState=record
    instr:TKeyword;
    param:string;
    end;

  { TSequencer }

  TSequencer=class(TObject)
    protected
      FParent:TFPCupManager;
      CurrentModule:String;
      Installer:TInstaller;  //current installer
      SkipList:TStringList;
      StateMachine:array of TState;
      function DoBuildModule(ModuleName:string):boolean;
      function DoCleanModule(ModuleName:string):boolean;
      function DoConfigModule(ModuleName:string):boolean;
      function DoExec(FunctionName:string):boolean;
      function DoGetModule(ModuleName:string):boolean;
      function DoSetCPU(CPU:string):boolean;
      function DoSetOS(OS:string):boolean;
      function DoSetLCL(LCL:string):boolean;
      function DoUnInstallModule(ModuleName:string):boolean;
      function GetInstaller(ModuleName:string):boolean;
      function IsSkipped(ModuleName:string):boolean;
    public
      property Parent:TFPCupManager write Fparent;
      function Run(SequenceName:string):boolean;
    end;

implementation

{ TFPCupManager }

function TFPCupManager.CreateOnly(OnlyModules: string): boolean;
begin

end;

function TFPCupManager.LoadModuleList: boolean;
begin

end;

function TFPCupManager.DeleteOnly: boolean;
begin

end;

function TFPCupManager.Run: boolean;
var Sequencer:TSequencer;
begin
  result:=false;
  Sequencer:=TSequencer.create;
  Sequencer.Parent:=Self;
  try
    if LoadModuleList then
      begin
      if FOnlyModules<>'' then
        begin
        CreateOnly(FOnlyModules);
        result:=Sequencer.Run('Only');
        DeleteOnly;
        end
      else
        {$ifdef win32}
        result:=Sequencer.Run('DefaultWin32');
        {$else}
        result:=Sequencer.Run('Default');
        {$endif win32}
      end;
  finally
    Sequencer.free;
  end;
end;

constructor TFPCupManager.Create;
begin

end;

destructor TFPCupManager.Destroy;
begin
  inherited Destroy;
end;

{ TSequencer }

function TSequencer.DoBuildModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.BuildModule(ModuleName);
end;

function TSequencer.DoCleanModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.CleanModule(ModuleName);
end;

function TSequencer.DoConfigModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and (Installer as TLazarusInstaller).ConfigLazarus(FParent.LazarusPrimaryConfigPath);
end;

function TSequencer.DoExec(FunctionName: string): boolean;
begin

end;

function TSequencer.DoGetModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.GetModule(ModuleName);
end;

function TSequencer.DoSetCPU(CPU: string): boolean;
begin
  FParent.CrossCPU_Target:=CPU;
end;

function TSequencer.DoSetOS(OS: string): boolean;
begin
  FParent.CrossOS_Target:=OS;
end;

function TSequencer.DoSetLCL(LCL: string): boolean;
begin
  FParent.CrossLCL_Platform:=LCL;
end;

function TSequencer.DoUnInstallModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.UnInstallModule(ModuleName);
end;

{GetInstaller gets a new installer for ModuleName and initialises parameters unless one exist already.}

function TSequencer.GetInstaller(ModuleName: string): boolean;
var
  CrossCompiling:boolean;
begin
  CrossCompiling:= (FParent.CrossCPU_Target<>'') or (FParent.CrossOS_Target<>'');
  //check if this is a known module
  if uppercase(ModuleName)='FPC' then
    begin
    if assigned(Installer) then
      begin
      if (not crosscompiling and (Installer is TFPCNativeInstaller)) or
        ( crosscompiling and (Installer is TFPCCrossInstaller)) then
        begin
        result:=true; //all fine, continue with current installer
        exit;
        end
      else
        Installer.free; // get rid of old installer
      end;
    if CrossCompiling then
      begin
      Installer:=TFPCCrossInstaller.Create;
      Installer.CrossOS_Target:=FParent.CrossOS_Target;
      Installer.CrossCPU_Target:=FParent.CrossCPU_Target;
      end
    else
      Installer:=TFPCNativeInstaller.Create;
    Installer.BaseDirectory:=FParent.FPCDirectory;
    (Installer as TFPCInstaller).BootstrapCompilerDirectory:=FParent.BootstrapCompilerDirectory;
    (Installer as TFPCInstaller).BootstrapCompilerURL:=FParent.BootstrapCompilerURL;
    Installer.Compiler:='';  //bootstrap used
    Installer.CompilerOptions:=FParent.FPCOPT;
    Installer.DesiredRevision:=FParent.FPCDesiredRevision;
    Installer.LogFile:=FParent.LogFile;
    {$IFDEF MSWINDOWS}
    Installer.MakeDirectory:=FParent.MakeDirectory;
    {$ENDIF}
    Installer.URL:=FParent.FPCURL;
    Installer.Verbose:=FParent.Verbose;
    end
  else if (uppercase(ModuleName)='LAZARUS') or (uppercase(ModuleName)='BIGIDE') then
    begin
    if assigned(Installer) then
      begin
      if (not crosscompiling and (Installer is TLazarusNativeInstaller)) or
        ( crosscompiling and (Installer is TLazarusCrossInstaller)) then
        begin
        result:=true; //all fine, continue with current installer
        exit;
        end
      else
        Installer.free; // get rid of old installer
      end;
    if CrossCompiling then
      begin
      Installer:=TLazarusCrossInstaller.Create;
      Installer.CrossOS_Target:=FParent.CrossOS_Target;
      Installer.CrossCPU_Target:=FParent.CrossCPU_Target;
      end
    else
      Installer:=TLazarusNativeInstaller.Create;
    Installer.BaseDirectory:=FParent.LazarusDirectory ;
    if FParent.CompilerName='' then
      Installer.Compiler:=Installer.GetCompilerInDir(FParent.FPCDirectory)
    else
      Installer.Compiler:=FParent.CompilerName;
    Installer.CompilerOptions:=FParent.LazarusOPT;
    Installer.DesiredRevision:=FParent.LazarusDesiredRevision;
    Installer.LogFile:=FParent.LogFile;
    {$IFDEF MSWINDOWS}
    Installer.MakeDirectory:=FParent.MakeDirectory;
    {$ENDIF}
    Installer.URL:=FParent.FPCURL;
    Installer.Verbose:=FParent.Verbose;
    end
  else if (uppercase(ModuleName)='HELP') then
    begin
    end
  else       // this is a universal module
    begin
      if assigned(Installer) then
        begin
        if (Installer is TUniversalInstaller) and
          (CurrentModule= ModuleName) then
          begin
          result:=true; //all fine, continue with current installer
          exit;
          end
        else
          Installer.free; // get rid of old installer
        end;
      Installer:=TUniversalInstaller.Create;
      CurrentModule:=ModuleName;
      //assign properties
    end;
end;

function TSequencer.IsSkipped(ModuleName: string): boolean;
begin
  result:=SkipList.IndexOf(ModuleName)>=0;
end;

function TSequencer.Run(SequenceName: string): boolean;
var
  InstructionPointer:integer;
  idx:integer;
  SeqAttr:^TSequenceAttributes;
begin
  if not assigned(FParent.ModuleList) then
    begin
    result:=false;
    exit;
    end;
  idx:=FParent.ModuleList.IndexOf(SequenceName);
  if (idx >0) then
    begin
    result:=true;
    SeqAttr:=PSequenceAttributes(pointer(FParent.ModuleList.Objects[idx]));
    case SeqAttr^.Executed of
      ESFailed   : begin
                     result:=false;
                     exit;
                   end;
      ESSucceeded : exit;
      end;
    InstructionPointer:=SeqAttr^.EntryPoint;
    while true do
      begin
      case StateMachine[InstructionPointer].instr of
        SMdeclare     :;
        SMdo          : if not IsSkipped(StateMachine[InstructionPointer].param) then
                          result:=Run(StateMachine[InstructionPointer].param);
        SMrequire     : result:=Run(StateMachine[InstructionPointer].param);
        SMexec        : result:=DoExec(StateMachine[InstructionPointer].param);
        SMend         : exit; //success
        SMcleanmodule : result:=DoCleanModule(StateMachine[InstructionPointer].param);
        SMgetmodule   : result:=DoGetModule(StateMachine[InstructionPointer].param);
        SMbuildmodule : result:=DoBuildModule(StateMachine[InstructionPointer].param);
        SMuninstallmodule: result:=DoUnInstallModule(StateMachine[InstructionPointer].param);
        SMconfigmodule: result:=DoConfigModule(StateMachine[InstructionPointer].param);
        SMSetLCL      : DoSetLCL(StateMachine[InstructionPointer].param);
        SMSetOS       : DoSetOS(StateMachine[InstructionPointer].param);
        SMSetCPU      : DoSetCPU(StateMachine[InstructionPointer].param);
        end;
      if not result then exit; //failure, bail out
      InstructionPointer:=InstructionPointer+1;
      end;
    end
  else
    result:=false;  // sequence not found
end;

end.

