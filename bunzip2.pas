unit bunzip2;

{$mode objfpc}{$H+}

interface

function Decompress(SourceFile, TargetFile: string; out ErrorLog: string): boolean;

implementation

uses
  SysUtils, objects, bzip2;

function Decompress(SourceFile, TargetFile: string; out ErrorLog: string): boolean;
  // Adapted from pasbzip.pas
const
  BufferSize = 4096;
var
  infile, outfile: Tbufstream;
  decoder: Tbzip2_decode_stream;
  a: array[1..BufferSize] of byte;
  readsize: cardinal;
  Status: boolean;
begin
  Status := True;
  if (FileExists(TargetFile)) then
  begin
    //Just get rid of it
    if SysUtils.DeleteFile(TargetFile) then
    begin
      ErrorLog := ErrorLog + LineEnding + 'Target file ' +
        TargetFile + ' already existed. Deleted it.';
    end
    else
    begin
      status := False;
      //If we can't delete it, bunzip2 probably won't be able to overwrite it.
    end;
  end;

  if (fileexists(SourceFile)=false) then
  begin
    ErrorLog := 'bunzip2: could not find input file: ' + SourceFile;
    status := False;
  end;

  if status = True then
  begin
    try
      infile.init(SourceFile, stopenread, BufferSize);
      //size was hardcoded 4096 in original code
      outfile.init(TargetFile, stcreate, BufferSize);
      //size was hardcoded 4096 in original code
      decoder.init(@infile);
      if decoder.status <> stok then
      begin
        ErrorLog := ErrorLog + LineEnding +
          'Error initializing bunzip: decoder status: ' + IntToStr(decoder.status) +
          '; decoder error info:' + IntToStr(decoder.errorinfo);
        status := False;
      end
      else
      begin
        repeat
          // Try to read entire buffer...
          readsize := BufferSize;
          decoder.Read(a, readsize);
          // ... if only part could be read, subtract how much we're short...
          Dec(readsize, decoder.short);
          outfile.Write(a, readsize);
        until decoder.status <> 0;
      end;
      // got rid of status check in original code as that sometimes failed while
      // decompression went ok
      decoder.done;
      infile.done;
      outfile.done;
      status := True;
    except
      on E: Exception do
      begin
        ErrorLog := 'bunzip2: error decompressing ' + SourceFile + '. Details:' +
          E.ClassName + '/' + E.Message;
        status := False;
      end;
    end;
  end;
  Result := status;
end;
end.
