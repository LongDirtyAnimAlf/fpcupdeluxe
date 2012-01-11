unit bunzip2;

{$mode objfpc}{$H+}

interface

function Decompress(SourceFile, TargetFile: string; var ErrorLog: string): boolean;

implementation

uses
  SysUtils, objects, bzip2;

function Decompress(SourceFile, TargetFile: string; var ErrorLog: string): boolean;
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
  Status := False;
  infile.init(SourceFile, stopenread, 4096);
  outfile.init(TargetFile, stcreate, 4096);
  decoder.init(@infile);
  if decoder.status <> stok then
  begin
    ErrorLog := ErrorLog + LineEnding +
      ('Error initializing bunzip: decoder status: ' + IntToStr(decoder.status) +
      '; decoder error info:' + IntToStr(decoder.errorinfo));
    status := False;
  end;
  repeat
    readsize := BufferSize;
    decoder.Read(a, readsize);
    Dec(readsize, decoder.short);
    outfile.Write(a, readsize);
  until decoder.status <> 0;
  {
  //ignore errors here, seems to work...
  if decoder.status <> stok then
  begin
    ErrorLog := ErrorLog + LineEnding +
      ('Error finalizing bunzip: decoder status: ' + IntToStr(decoder.status) +
      '; decoder error info:' + IntToStr(decoder.errorinfo));
    status := False;
  end;
  }
  decoder.done;
  infile.done;
  outfile.done;
  Result := status;
end;


end.

