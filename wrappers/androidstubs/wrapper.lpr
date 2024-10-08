program wrapper;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, process;

const
  {%H-}AS_NAME='clang';
  {%H-}LD_NAME='ld';
  {%H-}STRIP_NAME='strip';

  {$I targets.inc}

function ResolveDots(const AFilename: string): string;
//trim double path delims and expand special dirs like .. and .
//on Windows change also '/' to '\' except for filenames starting with '\\?\'

  {$ifdef windows}
  function IsDriveDelim(const Path: string; p: integer): boolean; inline;
  begin
    Result:=(p=2) and (Path[2]=DriveDelim) and (Path[1] in ['a'..'z','A'..'Z']);
  end;
  {$endif}

  function IsPathDelim(const Path: string; p: integer): boolean;
  begin
    if (p<=0) or (Path[p]=PathDelim) then exit(true);
    {$ifdef windows}
    if IsDriveDelim(Path,p) then
      exit(true);
    {$endif}
    Result:=false;
  end;

var SrcPos, DestPos, Len, DirStart: integer;
  c: char;
  MacroPos: LongInt;
  ResultMustEndWithPathDelim: Boolean;
begin
  Len:=length(AFilename);
  if Len=0 then exit('');

  Result:=AFilename;

  {$ifdef windows}
  //Special case: everything is literal after this, even dots (this does not apply to '//?/')
  if (length(AFilename)>=4) and (AFilename[1]='\') and (AFilename[2]='\')
  and (AFilename[3]='?') and (AFilename[4]='\') then
    exit;
  {$endif}

  //To add some consistency to the outcomes
  //Depending on the path the algorithm takes it may remove the trailing PathDelim, so we restore it later if needed
  //Issue #37188
  //It's a workaround, fee free to implement a better fix
  ResultMustEndWithPathDelim := ((Len>2) and (AFilename[Len]='.') and (AFilename[Len-1]='.') and (AFilename[Len-2] in AllowDirectorySeparators)) or
                                ((Len>1) and (AFilename[Len]='.') and (AFilename[Len-1] in AllowDirectorySeparators));

  SrcPos:=1;
  DestPos:=1;

  // trim double path delimiters and special dirs . and ..
  while (SrcPos<=Len) do begin
    c:=AFilename[SrcPos];
    {$ifdef windows}
    //change / to \. The WinApi accepts both, but it leads to strange effects in other places
    if (c in AllowDirectorySeparators) then c := PathDelim;
    {$endif}
    // check for duplicate path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF Windows}
      if (DestPos>2)
      {$ELSE}
      if (DestPos>1)
      {$ENDIF}
      and (Result[DestPos-1]=PathDelim) then begin
        // skip duplicate PathDelim
        continue;
      end;
      Result[DestPos]:=c;
      inc(DestPos);
      continue;
    end;
    // check for special dirs . and ..
    if (c='.') then begin
      if (SrcPos<Len) then begin
        if (AFilename[SrcPos+1] in AllowDirectorySeparators)
        and IsPathDelim(Result,DestPos-1) then begin
          // special dir ./ or */./
          // -> skip
          inc(SrcPos,2);
          while (SrcPos<=Len) and (AFilename[SrcPos] in AllowDirectorySeparators) do
            inc(SrcPos);
          continue;
        end else if (AFilename[SrcPos+1]='.')
        and ((SrcPos+1=Len) or (AFilename[SrcPos+2] in AllowDirectorySeparators)) then
        begin
          // special dir ..
          //  1. ..      -> copy
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> copy
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. ../..   -> copy because if the first '..' was not resolved, the next can't neither
          //  7. dir/..  -> trim dir and ..
          //  8. dir$macro/..  -> copy
          if DestPos=1 then begin
            //  1. .. or ../  -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF Windows}
          end else if (DestPos=3) and IsDriveDelim(Result,2) then begin
            //  3. C:..    -> copy
          end else if (DestPos=4) and (Result[3]=PathDelim)
          and IsDriveDelim(Result,2) then begin
            //  4. C:\..   -> skip .., keep C:\
            inc(SrcPos,2);
            continue;
          end else if (DestPos=3) and (Result[1]=PathDelim)
          and (Result[2]=PathDelim) then begin
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
            // */.
            if (DestPos>3)
            and (Result[DestPos-2]='.') and (Result[DestPos-3]='.')
            and IsPathDelim(Result,DestPos-4) then begin
              //  6. ../..   -> copy because if the first '..' was not resolved, the next can't neither
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DirStart:=DestPos-2;
              while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
                dec(DirStart);
              {$ifdef windows}
              if (DirStart=1) and IsDriveDelim(Result,2) then
                inc(DirStart,2);
              {$endif}
              MacroPos:=DirStart;
              while MacroPos<DestPos do begin
                if (Result[MacroPos]='$')
                and (Result[MacroPos+1] in ['(','a'..'z','A'..'Z']) then begin
                  // 8. directory contains a macro -> keep
                  break;
                end;
                inc(MacroPos);
              end;
              if MacroPos=DestPos then begin
                // previous directory does not contain a macro -> remove dir/..
                DestPos:=DirStart;
                inc(SrcPos,2);
                //writeln('ResolveDots ',DestPos,' SrcPos=',SrcPos,' File="',AFilename,'" Result="',copy(Result,1,DestPos-1),'"');
                if SrcPos>Len then begin
                  // '..' at end of filename
                  if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
                    // foo/dir/.. -> foo
                    dec(DestPos);
                  end else if (DestPos=1) then begin
                    // foo/.. -> .
                    Result[1]:='.';
                    DestPos:=2;
                  end;
                end else if DestPos=1 then begin
                  // e.g. 'foo/../'
                  while (SrcPos<=Len) and (AFilename[SrcPos] in AllowDirectorySeparators) do
                    inc(SrcPos);
                end;
                continue;
              end;
            end;
          end;
        end;
      end else begin
        // special dir . at end of filename
        if DestPos=1 then begin
          Result:='.';
          exit;
        end;
        if (DestPos>2) and (Result[DestPos-1]=PathDelim)
        {$ifdef windows}
        and not IsDriveDelim(Result,DestPos-2)
        {$endif}
        then begin
          // foo/. -> foo
          // C:foo\. -> C:foo
          // C:\. -> C:\
          {dec(DestPos); } //Part of issue #37188
        end;
        break;
      end;
    end;
    // copy directory
    repeat
      Result[DestPos]:=c;
      inc(DestPos);
      inc(SrcPos);
      if (SrcPos>Len) then break;
      c:=AFilename[SrcPos];
      {$ifdef windows}
      //change / to \. The WinApi accepts both, but it leads to strange effects in other places
      if (c in AllowDirectorySeparators) then c := PathDelim;
      {$endif}
      if c=PathDelim then break;
    until false;
  end;
  // trim result
  if DestPos<=length(AFilename) then
    if (DestPos=1) then
      Result:='.'
    else
      SetLength(Result,DestPos-1);
  if ResultMustEndWithPathDelim and (Result<>'.') and (Result[Length(Result)]<>PathDelim) then
    Result := Result + PathDelim;
end;

function CheckParameter(aList:TStrings;aSetting:string):boolean;
var
  index:integer;
begin
  result:=false;
  if (aList.Count=0) then exit;;
  for index:=0 to Pred(aList.Count) do
  begin
    if aList[index]=aSetting then
    begin
      result:=true;
      break
    end;
  end;
end;

var
  i: integer;
  AProcess: TProcess;
  aTarget,aExe,aExePath,aSDKPathBase,aSDKPath:string;
  aParamName,aParamValue:string;

begin
  aExePath:=ExtractFilePath(ParamStr(0));
  aTarget:='';

  {$if DECLARED(EXEWRAPPER)}
  aExe:=aExePath+EXEWRAPPER;

  (*
  aTarget:=ExtractFileName(ParamStr(0));

  i:=Pos(EXEWRAPPER,aTarget);
  if (i>1) then
  begin
    Delete(aTarget,i,MaxInt);
    i:=Pos('-',aTarget);
    if (i>0) then
    begin
      aCPU:=Copy(aTarget,1,Pred(i));
    end;
  end
  else
    aTarget:='';
  *)

  {$ifdef MSWINDOWS}
  aExe:=aExe+'.exe';
  {$endif}
  {$ifdef UNIX}
  if (EXEWRAPPER<>LD_NAME) then
  begin
    if NOT FileExists(aExe) then aExe:='/bin/'+EXEWRAPPER;
    if NOT FileExists(aExe) then aExe:='/usr/bin/'+EXEWRAPPER;
    if NOT FileExists(aExe) then aExe:='/usr/local/bin/'+EXEWRAPPER;
  end;
  {$endif}
  if FileExists(aExe) then
  begin
    AProcess:=TProcess.Create(nil);

    AProcess.Executable:=aExe;
    APRocess.Options := [poWaitOnExit];

    if ((EXEWRAPPER=CLANG_NAME) OR (EXEWRAPPER=CLANGPP_NAME)) then
    begin
      {$if DECLARED(TARGETTRIPLE)}
      APRocess.Parameters.Append('-target');
      APRocess.Parameters.Append(TARGETTRIPLE);
      {$else}
      if (Length(aTarget>0) then
      begin
        APRocess.Parameters.Append('-target');
        APRocess.Parameters.Append(aTarget);
      end;
      {$endif}
    end;

    aSDKPath:='';
    aSDKPathBase:=ResolveDots(aExePath+'..'+PathDelim+'..'+PathDelim+'..'+PathDelim+'lib'+PathDelim);
    if DirectoryExists(aSDKPathBase) then
    begin
      {$if defined(amd64_darwin) OR defined(arm64_darwin) OR defined(i386_darwin)}
      {$if DECLARED(SDKDIR_12)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin'+PathDelim+SDKDIR_12;
      {$endif}
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin';
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-darwin'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-darwin'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-darwin';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:='';
      {$endif}
      {$if defined(arm64_ios) OR defined(arm_ios)}
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-ios'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-ios'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-ios';
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-ios'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-ios'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-ios';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:='';
      {$endif}
    end;

    aSDKPathBase:=ResolveDots(aExePath+'..'+PathDelim+'..'+PathDelim+'lib'+PathDelim);
    if DirectoryExists(aSDKPathBase) then
    begin
      {$if defined(amd64_darwin) OR defined(arm64_darwin) OR defined(i386_darwin)}
      {$if DECLARED(SDKDIR_12)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin'+PathDelim+SDKDIR_12;
      {$endif}
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-darwin';
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-darwin'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-darwin'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-darwin';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:='';
      {$endif}
      {$if defined(arm64_ios) OR defined(arm_ios)}
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-ios'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-ios'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+'all-ios';
      {$if DECLARED(SDKDIR)}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-ios'+PathDelim+SDKDIR;
      {$endif}
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-ios'+PathDelim+'SDK';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:=aSDKPathBase+TARGETCPU+'-ios';
      if NOT DirectoryExists(aSDKPath) then aSDKPath:='';
      {$endif}
    end;

    for i:=1 to ParamCount() do APRocess.Parameters.Append(ParamStr(i));

    aParamName:='-arch';
    aParamValue:='';
    if CheckParameter(APRocess.Parameters,aParamName) then aParamValue:='We got a CPU target';
    if (Length(aParamValue)=0) then
    begin
      {$if DECLARED(TARGETCPU)}
      APRocess.Parameters.Append(aParamName);
      {$if defined(arm64_darwin) OR defined(arm64_ios)}
      APRocess.Parameters.Append('arm64');
      {$else}
      APRocess.Parameters.Append(TARGETCPU);
      {$endif}
      {$endif}
    end;

    if (EXEWRAPPER=LD_NAME) then
    begin
      aParamName:='-syslibroot';
      aParamValue:='';
      if CheckParameter(APRocess.Parameters,aParamName) then aParamValue:='We got a syslibroot';
      if (Length(aParamValue)=0) then
      begin
        if DirectoryExists(aSDKPath) then
        begin
          APRocess.Parameters.Append(aParamName);
          APRocess.Parameters.Append(aSDKPath);
          //writeln('A syslibroot was found and added: '+aSDKPath);
        end;
      end;
    end;

    if ((EXEWRAPPER=CLANG_NAME) OR (EXEWRAPPER=CLANGPP_NAME)) then
    begin
      aParamName:='-isysroot';
      aParamValue:='';
      if CheckParameter(APRocess.Parameters,aParamName) then aParamValue:='We got a sysroot';
      if (Length(aParamValue)=0) then
      begin
        if DirectoryExists(aSDKPath) then
        begin
          APRocess.Parameters.Append(aParamName);
          APRocess.Parameters.Append(aSDKPath);
          //writeln('A sysroot was found and added: '+aSDKPath);
        end;
      end;

      if (BASEOS=OSX_NAME) then
      begin
        aParamName:='-mmacosx-version-min';
        aParamValue:='';
        if CheckParameter(APRocess.Parameters,aParamName) then aParamValue:='We got a OSX version minimum';
        if (Length(aParamValue)=0) then
        begin
          APRocess.Parameters.Append(aParamName+'='+MIN_SDK_VERSION);
          //writeln('A OSX version minimum was found and added: '+MIN_SDK_VERSION);
        end;
      end;

      if (BASEOS=IOS_NAME) then
      begin
        aParamName:='-miphoneos-version-min';
        aParamValue:='';
        if CheckParameter(APRocess.Parameters,aParamName) then aParamValue:='We got a iOS version minimum';
        if (Length(aParamValue)=0) then
        begin
          APRocess.Parameters.Append(aParamName+'='+MIN_SDK_VERSION);
          //writeln('A iOS version minimum was found and added: '+MIN_SDK_VERSION);
        end;
      end;

      APRocess.Parameters.Append('-mlinker-version=609');
      //APRocess.Parameters.Append('-Wl,-adhoc_codesign');
      APRocess.Parameters.Append('-Wno-unused-command-line-argument');
      APRocess.Parameters.Append('-Wno-overriding-t-option');
    end;

    AProcess.Execute;

    AProcess.Free;
  end
  else
  begin
    writeln('Error: could not find '+EXEWRAPPER);
  end;
  {$endif}
end.

