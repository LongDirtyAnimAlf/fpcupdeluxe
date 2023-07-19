unit scannercore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TScannerCore = class
  private
    FReadelfBinary: string;
    {$ifdef Windows}
    FLibraryLocation: string;
    {$endif}
    FLibraryList: TStringList;
    FLibraryNotFoundList: TStringList;
    FLibraryLocationList: TStringList;
    chkQT:boolean;
    function StoreLibrary(const aLib:string):boolean;
    procedure CheckAndAddLibrary(const aLib:string);
  public
    procedure GetAndSaveLibs(const Location:string);
    constructor Create;
    destructor Destroy; override;
  published
    property LibraryList: TStringList read FLibraryList;
    property LibraryNotFoundList: TStringList read FLibraryNotFoundList;
    property LibraryLocationList: TStringList read FLibraryLocationList;
    property ReadelfBinary: string write FReadelfBinary;
    {$ifdef Windows}
    property LibraryLocation: string write FLibraryLocation;
    {$endif}
  end;

implementation

uses
  StrUtils,process,FileUtil,LazFileUtils,LookupStringList;

const
  {$ifdef CPUX86}
  UNIXSEARCHDIRS : array [0..10] of string = (
  {$else}
  UNIXSEARCHDIRS : array [0..7] of string = (
  {$endif CPUX86}
  {$ifdef CPUX86}
  '/usr/lib/i386-linux-gnu',
  '/usr/local/lib/i386-linux-gnu',
  '/lib/i386-linux-gnu',
  '/usr/lib/i686-linux-gnu',
  '/usr/local/lib/i686-linux-gnu',
  '/lib/i686-linux-gnu',
  {$endif CPUX86}
  {$ifdef CPUX86_64}
  '/usr/lib/x86_64-linux-gnu',
  '/usr/local/lib/x86_64-linux-gnu',
  '/lib/x86_64-linux-gnu',
  {$endif CPUX86_64}
  {$ifdef CPUARM}
  {$ifdef CPUARMHF}
  '/usr/lib/arm-linux-gnueabihf',
  '/usr/local/lib/arm-linux-gnueabihf',
  '/lib/arm-linux-gnueabihf',
  {$else}
  '/usr/lib/arm-linux-gnueabi',
  '/usr/local/lib/arm-linux-gnueabi',
  '/lib/arm-linux-gnueabi',
  {$endif CPUARMHF}
  {$endif CPUARM}
  {$ifdef CPUAARCH64}
  '/usr/lib/aarch64-linux-gnu',
  '/usr/local/lib/aarch64-linux-gnu',
  '/lib/aarch64-linux-gnu',
  {$endif CPUAARCH64}
  {$ifdef CPULOONGARCH}
  '/usr/lib/loongarch-linux-gnu',
  '/usr/local/lib/loongarch-linux-gnu',
  '/lib/loongarch-linux-gnu',
  {$endif CPULOONGARCH}
  '/usr/lib',
  '/usr/local/lib',
  '/lib',
  {$ifdef CPU32}
  '/usr/lib32',
  '/lib32'
  {$endif CPU32}
  {$ifdef CPU64}
  '/usr/lib64',
  '/lib64'
  {$endif CPU64}
  );

  {$ifdef Haiku}
  HAIKUSEARCHDIRS : array [0..3] of string = (
  '/boot/system/lib/x86',
  '/boot/system/non-packaged/lib/x86',
  '/boot/system/lib',
  '/boot/system/non-packaged/lib'
  );
  {$endif}

  {$if defined(FreeBSD) OR defined(NetBSD) OR defined(OenBSD) OR defined(DragonFly)}
  BSDSEARCHDIRS : array [0..9] of string = (
  '/lib',
  '/libexec',
  '/usr/lib',
  '/usr/pkg/lib',
  '/usr/X11R7/lib',
  '/usr/local/lib',
  '/usr/local/lib/qt5',
  '/usr/local/lib/qt6',
  '/usr/pkg/qt5/lib',
  '/usr/pkg/qt6/lib'
  );
  {$endif}

  {$ifdef Windows}
  WINDOWSSEARCHDIRS : array [0..0] of string = (
  'dummy'
  );
  {$endif}

  {$ifdef Windows}
  DYNLINKV1='ld-*.so.1';
  DYNLINKV2='ld-*.so.2';
  DYNLINKV3='ld-*.so.3';
  {$else}
  {$if defined(FreeBSD) OR defined(NetBSD) OR defined(OenBSD) OR defined(DragonFly)}
  DYNLINKV1='ld-elf.so.1';
  DYNLINKV2='ld-elf.so.2';
  DYNLINKV3='ld-elf.so.3';
  {$else}
  {$ifdef CPUX86}
  DYNLINKV1='ld-linux.so.1';
  DYNLINKV2='ld-linux.so.2';
  DYNLINKV3='ld-linux.so.3';
  {$endif CPUX86}
  {$ifdef CPUX86_64}
  DYNLINKV1='ld-linux-x86-64.so.1';
  DYNLINKV2='ld-linux-x86-64.so.2';
  DYNLINKV3='ld-linux-x86-64.so.3';
  {$endif CPUX86_64}
  {$ifdef CPUARM}
  {$ifdef CPUARMHF}
  DYNLINKV1='ld-linux-armhf.so.1';
  DYNLINKV2='ld-linux-armhf.so.2';
  DYNLINKV3='ld-linux-armhf.so.3';
  {$else}
  DYNLINKV1='ld-linux.so.1';
  DYNLINKV2='ld-linux.so.2';
  DYNLINKV3='ld-linux.so.3';
  {$endif CPUARMHF}
  {$endif CPUARM}
  {$ifdef CPUAARCH64}
  DYNLINKV1='ld-linux-aarch64.so.1';
  DYNLINKV2='ld-linux-aarch64.so.2';
  DYNLINKV3='ld-linux-aarch64.so.3';
  {$endif CPUAARCH64}
  {$ifdef CPULOONGARCH}
  DYNLINKV1='ld-linux-loongarch-lp64d.so.1';
  DYNLINKV2='ld-linux-loongarch-lp64d.so.2';
  DYNLINKV3='ld-linux-loongarch-lp64d.so.3';
  {$endif CPULOONGARCH}
  {$endif}
  {$endif}

const FPCLIBS : array [0..43] of string = (
  'crtbegin.o',
  'crtbeginS.o',
  'crtend.o',
  'crtendS.o',
  'crt1.o',
  'crti.o',
  'crtn.o',
  'Mcrt1.o',
  'Scrt1.o',
  'grcrt1.o',
  'ld.so.1',
  'ld.so.2',
  'ld.so.3',
  DYNLINKV1,
  DYNLINKV2,
  DYNLINKV3,
  'libanl.so.1',
  'libcrypt.so.1',
  'libc.so.*',
  'libdb1.so.2',
  'libdb2.so.3',
  'libdl.so.1',
  'libdl.so.2',
  'libglib-2.0.so.0',
  'libgobject-2.0.so.0',
  'libgthread-2.0.so.0',
  'libgmodule-2.0.so.0',
  'libm.so.6',
  'libmvec.so.1',
  'libnsl.so.1',
  'libnss_compat.so.2',
  'libnss_dns6.so.2',
  'libnss_dns.so.2',
  'libnss_files.so.2',
  'libnss_hesiod.so.2',
  'libnss_ldap.so.2',
  'libnss_nisplus.so.2',
  'libnss_nis.so.2',
  'libpthread.so.0',
  'libresolv.so.2',
  'librt.so.1',
  'libthread_db.so.1',
  'libutil.so.1',
  'libz.so.1'
);

const FPCALIBS : array [0..11] of string = (
  'libc_nonshared.a',
  'libssp_nonshared.a',
  'libgcc.a',
  'libmvec_nonshared.a',
  'libpthread_nonshared.a',
  'libcrypt.a',    // might be a placeholder only [musl]
  'libdl.a',       // might be a placeholder only [musl]
  'libm.a',        // might be a placeholder only [musl]
  'libpthread.a',  // might be a placeholder only [musl]
  'libresolv.a',   // might be a placeholder only [musl]
  'librt.a',       // might be a placeholder only [musl]
  'libutil.a'      // might be a placeholder only [musl]
);

const FPCLINKLIBS : array [0..10] of string = (
  'ld.so',
  'libc.so',
  'libm.so',
  'libpthread.so',
  'libdl.so',
  'libgobject-2.0.so',
  'libglib-2.0.so',
  'libgthread-2.0.so',
  'libgmodule-2.0.so',
  'librt.so',
  'libz.so'
);

const FPCEXTRALIBS : array [0..36] of string = (
  'liba52.so',
  'libaspell.so',
  'libdts.so',
  'libfreetype.so',
  'libgmp.so',
  'libgtkhtml-2.so',
  'libglade-2.0.so',
  'libfontconfig.so',
  'libnettle.so',
  'libhogweed.so',
  'librsvg-2.so',
  'libsee.so',
  'libusb-1.0.so',
  'libmad.so',
  'libmatroska.so',
  'libmodplug.so',
  'libogg.so',
  'libvorbis.so',
  'libvorbisfile.so',
  'libvorbisenc.so',
  'libopenal.so',
  'libOpenCL.so',
  'libSDL2.so',
  'libSDL2_image.so',
  'libSDL2_mixer.so',
  'libSDL2_net.so',
  'libSDL2_ttf.so',
  'libsmpeg.so',
  'libwasmtime.so',
  'libmysqlclient.so',
  'libmysqlclient.so.21',
  'libmysqlclient.so.20',
  'libmysqlclient.so.18',
  'libmysqlclient.so.16',
  'libmysqlclient.so.15',
  'libmysqlclient.so.14',
  'libmysqlclient.so.12'
);

const LAZLIBS : array [0..19] of string = (
  'libgdk-x11-2.0.so.0',
  'libgtk-x11-2.0.so.0',
  'libX11.so.6',
  'libXtst.so.6',
  'libgdk_pixbuf-2.0.so.0',
  'libgobject-2.0.so.0',
  'libglib-2.0.so.0',
  'libgthread-2.0.so.0',
  'libgmodule-2.0.so.0',
  'libpango-1.0.so.0',
  'libcairo.so.2',
  'libatk-1.0.so.0',
  'libiconv.so.2',
  'libicui18n.so',
  'libgtk-3.so.0',
  'libsqlite3.so.0',
  'libGL.so',
  'libGLU.so',
  'libEGL.so',
  'libvulkan.so.1'
);

const LAZLINKLIBS : array [0..7] of string = (
  'libgdk-x11-2.0.so',
  'libgtk-x11-2.0.so',
  'libX11.so',
  'libgdk_pixbuf-2.0.so',
  'libpango-1.0.so',
  'libiconv.so',
  'libcairo.so',
  'libatk-1.0.so'
);

const QTLIBS : array [0..14] of string = (
  'libQt5Pas.so.1',
  'libQt5Core.so.5',
  'libQt5GUI.so.5',
  'libQt5Network.so.5',
  'libQt5Pas.so.1',
  'libQt5PrintSupport.so.5',
  'libQt5Widgets.so.5',
  'libQt5X11Extras.so.5',
  'libQt6Pas.so.6',
  'libQt6Core.so.6',
  'libQt6DBus.so.6',
  'libQt6GUI.so.6',
  'libQt6Pas.so.6',
  'libQt6PrintSupport.so.6',
  'libQt6Widgets.so.6'
);

const QTLINKLIBS : array [0..1] of string = (
  'libQt5Pas.so',
  'libQt6Pas.so'
);

var
  ErrorMsg: String;
  {$ifndef Windows}
  GccDirectory:string;
  {$endif}
  SearchLib,SearchDir,SearchLibPath:string;
  FinalSearchResultList:TLookupStringList;
  sl:string;
  Index:integer;
  FileName,TargetFile,LinkFile:string;
  aList:TStringList;

function GetSourceCPU:string;
begin
  result:=lowercase({$i %FPCTARGETCPU%});
end;

function GetSourceOS:string;
begin
  result:=lowercase({$i %FPCTARGETOS%});
end;

function GetSourceCPUOS:string;
begin
  result:=GetSourceCPU+'-'+GetSourceOS;
end;

{$ifndef Windows}
function GetStartupObjects:string;
const
  LINKFILE='crtbegin.o';
  SEARCHDIRS : array [0..5] of string = (
    '/usr/local/lib/',
    '/usr/lib/',
    '/usr/local/lib/gcc/',
    '/usr/lib/gcc/',
    '/lib/gcc/',
    '/lib/'
    );

var
  LinkFiles     : TStringList;
  Output,s1,s2  : string;
  i,j           : integer;
  FoundLinkFile : boolean;
  OutputLines   : TStringList;
begin
  FoundLinkFile:=false;
  result:='';

  for i:=Low(SEARCHDIRS) to High(SEARCHDIRS) do
  begin
    s1:=SEARCHDIRS[i];
    if FileExists(s1+LINKFILE) then FoundLinkFile:=true;
    if FoundLinkFile then
    begin
      result:=s1;
      break;
    end;
  end;

  {$ifdef Haiku}
  if (NOT FoundLinkFile) then
  begin
    s1:='/boot/system/develop/tools/x86/lib';
    if NOT DirectoryExists(s1) then s1:='/boot/system/develop/tools/lib';
    if DirectoryExists(s1) then
    begin
      LinkFiles := TStringList.Create;
      try
        FindAllFiles(LinkFiles, s1, '*.o', true);
        if (LinkFiles.Count>0) then
        begin
          for i:=0 to (LinkFiles.Count-1) do
          begin
            if Pos(DirectorySeparator+LINKFILE,LinkFiles[i])>0 then
            begin
              result:=ExtractFileDir(LinkFiles[i]);
              FoundLinkFile:=true;
              break;
            end;
          end;
        end;
      finally
        LinkFiles.Free;
      end;
    end;
    if (NOT FoundLinkFile) then
    begin
      s1:='/boot/system/develop/lib/x86/';
      if NOT DirectoryExists(s1) then s1:='/boot/system/develop/lib/';
      if FileExists(s1+'crti.o') then FoundLinkFile:=true;
      if FoundLinkFile then result:=s1;
    end;
  end;
  {$endif}

  if FoundLinkFile then exit;

  try
    Output:='';
    if RunCommand('gcc',['-print-prog-name=cc1'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
    begin
      s1:=Trim(Output);
      if FileExists(s1) then
      begin
        s2:=ExtractFileDir(s1);
        if FileExists(s2+DirectorySeparator+LINKFILE) then
        begin
          result:=s2;
          FoundLinkFile:=true;
        end;
      end;
    end;

    if (NOT FoundLinkFile) then
    begin
      Output:='';
      if RunCommand('gcc',['-print-search-dirs'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
      begin
        Output:=TrimRight(Output);
        if Length(Output)>0 then
        begin
          OutputLines:=TStringList.Create;
          try
            OutputLines.Text:=Output;
            if OutputLines.Count>0 then
            begin
              for i:=0 to (OutputLines.Count-1) do
              begin
                s1:=OutputLines.Strings[i];
                j:=Pos('libraries:',s1);
                if j=1 then
                begin
                  j:=Pos(DirectorySeparator,s1);
                  if j>0 then
                  begin
                    Delete(s1,1,j-1);
                    LinkFiles := TStringList.Create;
                    try
                      LinkFiles.StrictDelimiter:=true;
                      LinkFiles.Delimiter:=':';
                      LinkFiles.DelimitedText:=s1;
                      if LinkFiles.Count>0 then
                      begin
                        for j:=0 to (LinkFiles.Count-1) do
                        begin
                          s2:=ExcludeTrailingPathDelimiter(LinkFiles.Strings[j]);
                          //s2:=ExtractFileDir(LinkFiles.Strings[j]);
                          if FileExists(s2+DirectorySeparator+LINKFILE) then
                          begin
                            result:=s2;
                            FoundLinkFile:=true;
                            break;
                          end;
                        end;
                      end;
                    finally
                      LinkFiles.Free;
                    end;
                  end;
                  break;
                end;
              end;
            end;
          finally
            OutputLines.Free;
          end;
        end;
      end;
    end;

    if (NOT FoundLinkFile) then
    begin
      Output:='';
      if RunCommand('gcc',['-v'], Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
      begin

        s1:='COLLECT_LTO_WRAPPER=';
        i:=Ansipos(s1, Output);
        if (i>0) then
        begin
          s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
          // find space as delimiter
          i:=Ansipos(' ', s2);
          // find lf as delimiter
          j:=Ansipos(#10, s2);
          if (j>0) AND (j<i) then i:=j;
          // find cr as delimiter
          j:=Ansipos(#13, s2);
          if (j>0) AND (j<i) then i:=j;
          if (i>0) then delete(s2,i,MaxInt);
          s2:=ExtractFileDir(s2);
          if FileExists(s2+DirectorySeparator+LINKFILE) then
          begin
            result:=s2;
            FoundLinkFile:=true;
          end;
        end;

        if (NOT FoundLinkFile) then
        begin
          s1:=' --libdir=';
          //s1:=' --libexecdir=';
          i:=Ansipos(s1, Output);
          if (i>0) then
          begin
            s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
            // find space as delimiter
            i:=Ansipos(' ', s2);
            // find lf as delimiter
            j:=Ansipos(#10, s2);
            if (j>0) AND (j<i) then i:=j;
            // find cr as delimiter
            j:=Ansipos(#13, s2);
            if (j>0) AND (j<i) then i:=j;
            if (i>0) then delete(s2,i,MaxInt);
            result:=IncludeTrailingPathDelimiter(s2);
          end;

          i:=Ansipos('gcc', result);
          if i=0 then result:=result+'gcc'+DirectorySeparator;

          s1:=' --build=';
          i:=Ansipos(s1, Output);
          if i=0 then
          begin
            s1:=' --target=';
            i:=Ansipos(s1, Output);
          end;
          if (i>0) then
          begin
            s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
            // find space as delimiter
            i:=Ansipos(' ', s2);
            // find lf as delimiter
            j:=Ansipos(#10, s2);
            if (j>0) AND (j<i) then i:=j;
            // find cr as delimiter
            j:=Ansipos(#13, s2);
            if (j>0) AND (j<i) then i:=j;
            if (i>0) then delete(s2,i,MaxInt);
            result:=result+s2+DirectorySeparator;
          end;

          s1:='gcc version ';
          i:=Ansipos(s1, Output);
          if (i>0) then
          begin
            s2:=RightStr(Output,Length(Output)-(i+Length(s1)-1));
            // find space as delimiter
            i:=Ansipos(' ', s2);
            // find lf as delimiter
            j:=Ansipos(#10, s2);
            if (j>0) AND (j<i) then i:=j;
            // find cr as delimiter
            j:=Ansipos(#13, s2);
            if (j>0) AND (j<i) then i:=j;
            if (i>0) then delete(s2,i,MaxInt);
            result:=result+s2;
            if FileExists(result+DirectorySeparator+LINKFILE) then
            begin
              FoundLinkFile:=true;
            end;
          end;
        end;
      end;
    end;

  except
    // ignore errors
  end;

  //In case of errors or failures, do a brute force search of gcc link file
  if (NOT FoundLinkFile) then
  begin
    {$IF (defined(BSD)) and (not defined(Darwin))}
    result:='/usr/local/lib/gcc';
    {$else}
    result:='/usr/lib/gcc';
    {$endif}

    if DirectoryExists(result) then
    begin
      LinkFiles := TStringList.Create;
      try
        FindAllFiles(LinkFiles, result, '*.o', true);
        if (LinkFiles.Count>0) then
        begin
          for i:=0 to (LinkFiles.Count-1) do
          begin
            if Pos(DirectorySeparator+LINKFILE,LinkFiles[i])>0 then
            begin
              result:=ExtractFileDir(LinkFiles[i]);
              FoundLinkFile:=true;
              break;
            end;
          end;
        end;
      finally
        LinkFiles.Free;
      end;
    end;
  end;
end;
{$endif Windows}

function GetDistro(const aID:string=''):string;
var
  {$if defined(Darwin) OR defined(MSWindows)}
  Major,Minor,Build,Patch: Integer;
  {$endif}
  i,j: Integer;
  AllOutput : TStringList;
  s,t:ansistring;
  success:boolean;
begin
  t:='unknown';
  success:=false;
  {$ifdef Unix}
    {$ifndef Darwin}
      s:='';
      if RunCommand('cat',['/etc/os-release'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
      begin
        if Pos('No such file or directory',s)=0 then
        begin
          AllOutput:=TStringList.Create;
          try
            AllOutput.Text:=s;
            s:='';
            if Length(aID)>0 then
            begin
              s:=AllOutput.Values[aID];
            end
            else
            begin
              s:=AllOutput.Values['NAME'];
              if Length(s)=0 then s := AllOutput.Values['ID_LIKE'];
              if Length(s)=0 then s := AllOutput.Values['DISTRIB_ID'];
              if Length(s)=0 then s := AllOutput.Values['ID'];
            end;
            success:=(Length(s)>0);
          finally
            AllOutput.Free;
          end;
        end;
      end;
      if (NOT success) then
      begin
        s:='';
        if RunCommand('cat',['/etc/system-release'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
        begin
          if Pos('No such file or directory',s)=0 then
          begin
            AllOutput:=TStringList.Create;
            try
              AllOutput.Text:=s;
              s:='';
              s:=AllOutput.Values['NAME'];
              if Length(s)=0 then s := AllOutput.Values['ID_LIKE'];
              if Length(s)=0 then s := AllOutput.Values['DISTRIB_ID'];
              if Length(s)=0 then s := AllOutput.Values['ID'];
              success:=(Length(s)>0);
            finally
              AllOutput.Free;
            end;
          end;
        end;
      end;
      if (NOT success) then
      begin
        if FileExists('/usr/bin/hostnamectl') then
        begin
          s:='';
          if RunCommand('hostnamectl',[],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF}) then
          begin
            AllOutput:=TStringList.Create;
            try
              AllOutput.NameValueSeparator:=':';
              AllOutput.Delimiter:=#10;
              AllOutput.StrictDelimiter:=true;
              AllOutput.DelimitedText:=s;
              s:='';
              for i:=0 to  AllOutput.Count-1 do
              begin
                j:=Pos('Operating System',AllOutput.Strings[i]);
                if j>0 then s:=s+Trim(AllOutput.Values[AllOutput.Names[i]]);
                j:=Pos('Kernel',AllOutput.Strings[i]);
                if j>0 then s:=s+' '+Trim(AllOutput.Values[AllOutput.Names[i]]);
              end;
              success:=(Length(s)>0);
            finally
              AllOutput.Free;
            end;
          end;
        end;
      end;

      if (NOT success) then t:='unknown' else
      begin
        s:=DelChars(s,'"');
        t:=Trim(s);
      end;
      {$ifdef BSD}
      if (t='unknown') then
      begin
        if RunCommand('uname',['-r'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF})
           then t := GetSourceOS+' '+lowercase(Trim(s));
      end;
      {$endif}

      if (t='unknown') then t := GetSourceOS;

      if (NOT success) then if RunCommand('uname',['-r'],s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF})
         then t := t+' '+lowercase(Trim(s));

    {$else Darwin}
      if RunCommand('sw_vers',['-productName'], s) then
      begin
        if Length(s)>0 then t:=Trim(s);
      end;
      if Length(s)=0 then t:=GetSourceOS;
      if RunCommand('sw_vers',['-productVersion'], s) then
      begin
        if Length(s)>0 then
        begin
          VersionFromString(s,Major,Minor,Build,Patch);
          t:=t+' '+InttoStr(Major)+'.'+InttoStr(Minor)+'.'+InttoStr(Build);
        end;
      end;
    {$endif Darwin}
  {$endif Unix}

  result:=t;
end;

function TScannerCore.StoreLibrary(const aLib:string):boolean;
begin
  result:=(FinalSearchResultList.Add(aLib)<>-1);
end;

procedure TScannerCore.CheckAndAddLibrary(const aLib:string);
const
  MAGICNEEDED = 'NEEDED';
  MAGICSHARED = 'Shared library:';
var
  SearchResultList:TStringList;
  SearchResult:string;
  FileName:string;
  i: integer;
  sd,sr,s:string;
begin
  SearchResultList:=TStringList.Create;
  try
    {$ifdef Windows}
    for sd in WINDOWSSEARCHDIRS do
    {$else}
    {$ifdef Haiku}
    for sd in HAIKUSEARCHDIRS do
    {$else}
    {$if defined(FreeBSD) OR defined(NetBSD) OR defined(OenBSD) OR defined(DragonFly)}
    for sd in BSDSEARCHDIRS do
    {$else}
    for sd in UNIXSEARCHDIRS do
    {$endif}
    {$endif}
    {$endif}
    begin
      {$ifdef Haiku}
      {$ifndef CPUX86}
      if (RightStr(sd,4)='/x86') then continue;
      {$endif}
      {$endif}
      FileName:=sd+DirectorySeparator+aLib;
      {$ifdef Windows}
      FileName:=StringReplace(FileName,'dummy',FLibraryLocation,[]);
      {$endif}
      // Do we have a wildcard ?
      if (Pos('*',aLib)>0) then
      begin
        SearchResultList.Clear;
        FindAllFiles(SearchResultList,ExtractFileDir(FileName),aLib,false);
        if (SearchResultList.Count>0) then
        begin
          for sr in SearchResultList do
          begin
            StoreLibrary('['+ExtractFileName(sr)+']');
          end;
        end;
        FileName:='';
        break;
      end;
      if FileExists(FileName) then
      begin
        StoreLibrary('['+ExtractFileName(FileName)+']');
        // These files might be a text-file, so skip analysis
        if ((ExtractFileName(FileName)='libc.so') OR (ExtractFileName(FileName)='libm.so') OR (ExtractFileName(FileName)='libpthread.so')) then
        begin
          FileName:='';
          break;
        end;
        // Skip static files from analysis
        if ExtractFileExt(FileName)='.a' then
        begin
          FileName:='';
          break;
        end;
        while FileIsSymlink(FileName) do FileName:=GetPhysicalFilename(FileName,pfeException);
        SearchResult:='';
        RunCommand(FReadelfBinary,['-d','-W',FileName],SearchResult,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
        SearchResultList.Text:=SearchResult;
        if (SearchResultList.Count=0) then continue;
        for sr in SearchResultList do
        begin
          s:=sr;
          if (Pos(MAGICNEEDED,s)>0) then
          begin
            i:=Pos(MAGICSHARED,s);
            if (i<>-1) then
            begin
              s:=Trim(Copy(s,i+Length(MAGICSHARED),MaxInt));
              if StoreLibrary(s) then
              begin
                s:=Copy(s,2,Length(s)-2);
                CheckAndAddLibrary(s);
              end;
            end;
          end;
        end;
        FileName:='';
        break;
      end;
    end;
    {$ifndef Windows}
    if (Length(FileName)>0) then
    begin
      FileName:=GccDirectory+DirectorySeparator+aLib;
      if FileExists(FileName) then
      begin
        StoreLibrary('['+aLib+']');
        FileName:='';
      end;
      if (Length(FileName)>0) then
      begin
        FLibraryNotFoundList.Append('Not found: '+aLib);
      end;
    end;
    {$endif}
  finally
    SearchResultList.Free;
  end;
end;

procedure TScannerCore.GetAndSaveLibs(const Location:string);
begin
  FLibraryList.Clear;
  FLibraryNotFoundList.Clear;
  FLibraryLocationList.Clear;

  //sd:=SysUtils.GetEnvironmentVariable('LIBRARY_PATH');
  //if (Length(sd)=0) then sd:=SysUtils.GetEnvironmentVariable('LD_LIBRARY_PATH');
  //if ((Length(sd)>0) AND (DirectoryExists(sd))) then
  //begin
  //end;
  {$ifndef Windows}
  GccDirectory:=GetStartupObjects;
  {$endif}

  FinalSearchResultList:=TLookupStringList.Create;
  try
    for SearchLib in FPCLINKLIBS do
    begin
      CheckAndAddLibrary(SearchLib);
    end;
    for SearchLib in FPCLIBS do
    begin
      CheckAndAddLibrary(SearchLib);
    end;
    for SearchLib in FPCALIBS do
    begin
      CheckAndAddLibrary(SearchLib);
    end;
    for SearchLib in FPCEXTRALIBS do
    begin
      CheckAndAddLibrary(SearchLib);
    end;
    for SearchLib in LAZLIBS do
    begin
      CheckAndAddLibrary(SearchLib);
    end;
    for SearchLib in LAZLINKLIBS do
    begin
      CheckAndAddLibrary(SearchLib);
    end;
    if chkQT then
    begin
      for SearchLib in QTLIBS do
      begin
        CheckAndAddLibrary(SearchLib);
      end;
      for SearchLib in QTLINKLIBS do
      begin
        CheckAndAddLibrary(SearchLib);
      end;
    end;

    FLibraryList.Text:=FinalSearchResultList.Text;

    for sl in FinalSearchResultList do
    begin
      SearchLib:=Copy(sl,2,Length(sl)-2);
      {$ifdef Windows}
      for SearchDir in WINDOWSSEARCHDIRS do
      {$else}
      {$ifdef Haiku}
      for SearchDir in HAIKUSEARCHDIRS do
      {$else}
      {$if defined(FreeBSD) OR defined(NetBSD) OR defined(OenBSD) OR defined(DragonFly)}
      for SearchDir in BSDSEARCHDIRS do
      {$else}
      for SearchDir in UNIXSEARCHDIRS do
      {$endif}
      {$endif}
      {$endif}
      begin
        {$ifdef Haiku}
        {$ifndef CPUX86}
        if (RightStr(SearchDir,4)='/x86') then continue;
        {$endif}
        {$endif}
        SearchLibPath:=SearchDir+DirectorySeparator+SearchLib;
        {$ifdef Windows}
        SearchLibPath:=StringReplace(SearchLibPath,'dummy',FLibraryLocation,[]);
        {$endif}
        if FileExists(SearchLibPath) then
        begin
          FLibraryLocationList.Append(SearchLibPath);
          SearchLib:='';
          break;
        end;
      end;
      {$ifndef Windows}
      if (Length(SearchLib)>0) then
      begin
        SearchLibPath:=GccDirectory+DirectorySeparator+SearchLib;
        if FileExists(SearchLibPath) then
        begin
          FLibraryLocationList.Append(SearchLibPath);
          SearchLib:='';
        end;
      end;
      {$endif}
      if (Length(SearchLib)>0) then
      begin
        FLibraryNotFoundList.Append('Copy not found: '+SearchLib);
      end;
    end;
  finally
    FinalSearchResultList.Free;
  end;


  // Process the results from the scan by getting the real files

  ForceDirectories(Location+'libs');

  aList:=TStringList.Create;
  try
    aList.Add('These libraries were sourced from: '+GetDistro+' version '+GetDistro('VERSION'));
    aList.SaveToFile(Location+'libs'+DirectorySeparator+'actual_library_version_fpcup.txt');
  finally
    aList.Free;
  end;

  // copy the libraries found
  for Index:=0 to Pred(FLibraryLocationList.Count) do
  begin
    FileName:=FLibraryLocationList.Strings[Index];
    TargetFile:=ExtractFileName(FileName);
    CopyFile(FileName,Location+'libs'+DirectorySeparator+TargetFile,[]);
  end;

  // if there are any linklibs not found, create them now
  for Index:=0 to Pred(FLibraryLocationList.Count) do
  begin
    FileName:=FLibraryLocationList.Strings[Index];
    TargetFile:=ExtractFileName(FileName);

    for LinkFile in FPCLINKLIBS do
    begin
      if (Pos(LinkFile,TargetFile)=1) then
      begin
        CopyFile(FileName,Location+'libs'+DirectorySeparator+LinkFile,[]);
        break;
      end;
    end;
    for LinkFile in FPCEXTRALIBS do
    begin
      if (Pos(LinkFile,TargetFile)=1) then
      begin
        CopyFile(FileName,Location+'libs'+DirectorySeparator+LinkFile,[]);
        break;
      end;
    end;
    for LinkFile in LAZLINKLIBS do
    begin
      if (Pos(LinkFile,TargetFile)=1) then
      begin
        CopyFile(FileName,Location+'libs'+DirectorySeparator+LinkFile,[]);
        break;
      end;
    end;

    if chkQT then
    begin
      for LinkFile in QTLINKLIBS do
      begin
        if (Pos(LinkFile,TargetFile)=1) then
        begin
          CopyFile(FileName,Location+'libs'+DirectorySeparator+LinkFile,[]);
          break;
        end;
      end;
    end;

  end;

end;

constructor TScannerCore.Create;
begin
  inherited Create;
  FLibraryList:=TStringList.Create;
  FLibraryNotFoundList:=TStringList.Create;
  FLibraryLocationList:=TStringList.Create;
  chkQT:=True;
  FReadelfBinary:='readelf';
  {$ifdef Windows}
  FLibraryLocation:='c:\fpcupdeluxe';
  {$endif}
end;

destructor TScannerCore.Destroy;
begin
  FLibraryList.Free;
  FLibraryNotFoundList.Free;
  FLibraryLocationList.Free;
  inherited Destroy;
end;

end.

