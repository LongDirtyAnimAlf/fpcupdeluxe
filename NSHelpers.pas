unit NSHelpers;

{
  Unit of handy routines for use with Foundation and Core Foundation.

  Author:    Phil Hess.
  Copyright: Copyright 2011 Phil Hess.
  License:   Modified LGPL. This means you can link your code to this
             compiled unit (statically in a standalone executable or 
             dynamically in a library) without releasing your code. Only
             changes to this unit need to be made publicly available.

  CFStrToAnsiStr was adapted from the Lazarus CarbonProc unit's
   CFStringToStr function.

  Note that objects returned by functions with "Create" or "Copy"
   in the function name need to be released by the calling code.
   For example, CFStringCreateWithCString is called in AnsiStrToCFStr,
   meaning this applies to code that calls AnsiStrToCFStr as well.
  FreeCFRef and FreeAndNilCFRef are convenience routines provided 
   for that purpose.
  See Apple docs for more information on the so-called Create Rule 
   and Get Rule: 
  https://developer.apple.com/library/mac/#documentation/CoreFoundation/
          Conceptual/CFMemoryMgmt/Concepts/Ownership.html
}

{$MODE Delphi}
{$modeswitch ObjectiveC1}

interface

uses
  CFBase,
  CFString,
{$IF DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)}  //iOS
 {$IFDEF NoiPhoneAll}
  Foundation;
 {$ELSE}
  iPhoneAll;
 {$ENDIF}
{$ELSE}  //macOS
 {$IFDEF NoCocoaAll}
  Foundation;
 {$ELSE}
  CocoaAll;
 {$ENDIF}
{$ENDIF}
  
 {Routines to convert CFString to and from:
   - AnsiString with specified encoding
   - string with specified encoding
   - UTF8-encoded AnsiString
   - WideString
   - UnicodeString}
function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;

procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
                               
function CFStrToStr(cfStr    : CFStringRef;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;

procedure StrToCFStr(const aStr     : string;
                       out cfStr    : CFStringRef;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
                               
function CFStrToUtf8Str(cfStr : CFStringRef): AnsiString;

procedure Utf8StrToCFStr(const aStr  : AnsiString;
                           out cfStr : CFStringRef);
                               
function CFStrToWideStr(cfStr : CFStringRef): WideString;

procedure WideStrToCFStr(const aStr  : WideString;
                           out cfStr : CFStringRef);

function CFStrToUniStr(cfStr : CFStringRef): UnicodeString;

procedure UniStrToCFStr(const aStr  : UnicodeString;
                          out cfStr : CFStringRef);

procedure FreeCFRef(var cfRef: CFTypeRef);

procedure FreeAndNilCFRef(var cfRef : CFTypeRef);


 {Routines to convert NSString to and from: 
   - AnsiString with specified encoding
   - string with specified encoding
   - UTF8-encoded AnsiString
   - WideString
   - UnicodeString}
function NSStrToAnsiStr(aNSStr   : NSString;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;

procedure AnsiStrToNSStr(const aStr     : AnsiString;
                           out aNSStr   : NSString;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1); overload;

function AnsiStrToNSStr(const aStr     : AnsiString;
                              encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString; overload;                             
                               

function NSStrToStr(aNSStr   : NSString;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;

procedure StrToNSStr(const aStr     : string;
                       out aNSStr   : NSString;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1); overload;
                               
function StrToNSStr(const aStr     : string;
                          encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString; overload;
                               

function NSStrToUtf8Str(aNSStr : NSString): AnsiString;

procedure Utf8StrToNSStr(const aStr   : AnsiString;
                           out aNSStr : NSString); overload;

function Utf8StrToNSStr(const aStr : AnsiString) : NSString; overload;


function NSStrToWideStr(aNSStr : NSString): WideString;

procedure WideStrToNSStr(const aStr   : WideString;
                           out aNSStr : NSString); overload;

function WideStrToNSStr(const aStr : WideString) : NSString; overload;


function NSStrToUniStr(aNSStr : NSString): UnicodeString;

procedure UniStrToNSStr(const aStr   : UnicodeString;
                          out aNSStr : NSString); overload;

function UniStrToNSStr(const aStr : UnicodeString) : NSString; overload;


implementation

function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;
 {Convert CFString to AnsiString.
  If encoding is not specified, encode using CP1252.}
var
  StrPtr   : Pointer;
  StrRange : CFRange;
  StrSize  : CFIndex;
begin
  if cfStr = nil then
    begin
    Result := '';
    Exit;
    end;

   {First try the optimized function}
  StrPtr := CFStringGetCStringPtr(cfStr, encoding);
  if StrPtr <> nil then  {Succeeded?}
    Result := PChar(StrPtr)
  else  {Use slower approach - see comments in CFString.pas}
    begin
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(cfStr);

     {Determine how long resulting string will be}
    CFStringGetBytes(cfStr, StrRange, encoding, Ord('?'),
                     False, nil, 0, StrSize);
    SetLength(Result, StrSize);  {Expand string to needed length}

    if StrSize > 0 then  {Convert string?}
      CFStringGetBytes(cfStr, StrRange, encoding, Ord('?'),
                       False, @Result[1], StrSize, StrSize);
    end;
end;  {CFStrToAnsiStr}

procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
 {Create CFString from AnsiString.
  If encoding is not specified, assume encoded as CP1252.
  Note: Calling code is responsible for calling CFRelease on 
   returned CFString. Presumably that's the reason why CarbonProc 
   unit's CreateCFString is a procedure, so you don't use it in 
   an expression and leave the CFString dangling.}
begin
  cfStr := CFStringCreateWithCString(nil, Pointer(PChar(aStr)), encoding);
end;


function CFStrToStr(cfStr    : CFStringRef;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;
 {Convert CFString to string.
  If encoding is not specified, encode using CP1252.
  This assumes string = AnsiString.}
begin
  Result := CFStrToAnsiStr(cfStr, encoding);
end;

procedure StrToCFStr(const aStr     : string;
                       out cfStr    : CFStringRef;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
 {Create CFString from string.
  If encoding is not specified, assume encoded as CP1252.
  This assumes string = AnsiString.
  Note: Calling code is responsible for calling CFRelease on 
   returned CFString.}
begin
  AnsiStrToCFStr(aStr, cfStr, encoding);
end;


function CFStrToUtf8Str(cfStr : CFStringRef): AnsiString;
 {Convert CFString to UTF8-encoded AnsiString.}
begin
  Result := CFStrToAnsiStr(cfStr, kCFStringEncodingUTF8);
end;

procedure Utf8StrToCFStr(const aStr  : AnsiString;
                           out cfStr : CFStringRef);
 {Create CFString from UTF8-encoded AnsiString.
  Note: Calling code is responsible for calling CFRelease on 
   returned CFString.}
begin
  AnsiStrToCFStr(aStr, cfStr, kCFStringEncodingUTF8);
end;


function CFStrToWideStr(cfStr : CFStringRef): WideString;
 {Convert CFString to WideString.}
begin
  Result := UTF8Decode(CFStrToAnsiStr(cfStr, kCFStringEncodingUTF8));
end;

procedure WideStrToCFStr(const aStr  : WideString;
                           out cfStr : CFStringRef);
 {Create CFString from WideString.
  Note: Calling code is responsible for calling CFRelease on 
   returned CFString.}
begin
  AnsiStrToCFStr(UTF8Encode(aStr), cfStr, kCFStringEncodingUTF8);
end;


function CFStrToUniStr(cfStr : CFStringRef): UnicodeString;
 {Convert CFString to UnicodeString.}
begin
  Result := UTF8Decode(CFStrToAnsiStr(cfStr, kCFStringEncodingUTF8));
end;

procedure UniStrToCFStr(const aStr  : UnicodeString;
                          out cfStr : CFStringRef);
 {Create CFString from UnicodeString.
  Note: Calling code is responsible for calling CFRelease on 
   returned CFString.}
begin
  AnsiStrToCFStr(UTF8Encode(aStr), cfStr, kCFStringEncodingUTF8);
end;


procedure FreeCFRef(var cfRef : CFTypeRef);
 {Convenience routine to free a CF reference so you don't have
   to check if it's nil.}
begin
  if Assigned(cfRef) then
    CFRelease(cfRef);
end;

procedure FreeAndNilCFRef(var cfRef : CFTypeRef);
 {Convenience routine to free a CF reference and set it to nil.}
begin
  FreeCFRef(cfRef);
  cfRef := nil;
end;


  {NSString routines}

function NSStrToAnsiStr(aNSStr   : NSString;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;
 {Convert NSString to AnsiString.
  If encoding is not specified, encode using CP1252.}
begin
   {Note NSString and CFStringRef are interchangable}
  Result := CFStrToAnsiStr(CFStringRef(aNSStr), encoding);
end;

procedure AnsiStrToNSStr(const aStr     : AnsiString;
                           out aNSStr   : NSString;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
 {Create NSString from AnsiString.
  If encoding is not specified, assume encoded as CP1252.
  Note: Calling code is responsible for calling returned
   NSString's release method.}
begin
   {Note NSString and CFStringRef are interchangable}
  AnsiStrToCFStr(aStr, CFStringRef(aNSStr), encoding);
end;

function AnsiStrToNSStr(const aStr     : AnsiString;
                              encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString;
 {Function version of previous procedure that can be called
   without releasing NSString result.
  Tip: You can use the procedure version and call release
   explictly to reduce use of autoreleased objects.}
begin
  AnsiStrToNSStr(aStr, Result, encoding);
  Result.autorelease;
end;


function NSStrToStr(aNSStr   : NSString;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;
 {Convert NSString to string.
  If encoding is not specified, encode using CP1252.
  This assumes string = AnsiString.}
begin
   {Note NSString and CFStringRef are interchangable}
  Result := CFStrToAnsiStr(CFStringRef(aNSStr), encoding);
end;

procedure StrToNSStr(const aStr     : string;
                       out aNSStr   : NSString;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
 {Create NSString from string.
  If encoding is not specified, assume encoded as CP1252.
  This assumes string = AnsiString.
  Note: Calling code is responsible for calling returned
   NSString's release method.}
begin
   {Note NSString and CFStringRef are interchangable}
  AnsiStrToCFStr(aStr, CFStringRef(aNSStr), encoding);
end;

function StrToNSStr(const aStr     : string;
                          encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString;
 {Function version of previous procedure that can be called
   without releasing NSString result.
  Tip: You can use the procedure version and call release
   explictly to reduce use of autoreleased objects.}
begin
  StrToNSStr(aStr, Result, encoding);
  Result.autorelease;
end;


function NSStrToUtf8Str(aNSStr : NSString): AnsiString;
 {Convert NSString to UTF8-encoded AnsiString.}
begin
   {Note NSString and CFStringRef are interchangable}
  Result := CFStrToAnsiStr(CFStringRef(aNSStr), kCFStringEncodingUTF8);
end;

procedure Utf8StrToNSStr(const aStr   : AnsiString;
                           out aNSStr : NSString);
 {Create NSString from UTF8-encoded AnsiString.
  Note: Calling code is responsible for calling returned
   NSString's release method.}
begin
   {Note NSString and CFStringRef are interchangable}
  AnsiStrToCFStr(aStr, CFStringRef(aNSStr), kCFStringEncodingUTF8);
end;

function Utf8StrToNSStr(const aStr : AnsiString) : NSString;
 {Function version of previous procedure that can be called
   without releasing NSString result.
  Tip: You can use the procedure version and call release
   explictly to reduce use of autoreleased objects.}
begin
  Utf8StrToNSStr(aStr, Result);
  Result.autorelease;
end;


function NSStrToWideStr(aNSStr : NSString): WideString;
 {Convert NSString to WideString.}
begin
   {Note NSString and CFStringRef are interchangable}
  Result := CFStrToWideStr(CFStringRef(aNSStr));
end;

procedure WideStrToNSStr(const aStr   : WideString;
                           out aNSStr : NSString);
 {Create NSString from WideString.
  Note: Calling code is responsible for calling returned
   NSString's release method.}
begin
   {Note NSString and CFStringRef are interchangable}
  WideStrToCFStr(aStr, CFStringRef(aNSStr));
end;

function WideStrToNSStr(const aStr : WideString) : NSString;
 {Function version of previous procedure that can be called
   without releasing NSString result.
  Tip: You can use the procedure version and call release
   explictly to reduce use of autoreleased objects.}
begin
  WideStrToNSStr(aStr, Result);
  Result.autorelease;
end;


function NSStrToUniStr(aNSStr : NSString): UnicodeString;
 {Convert NSString to UnicodeString.}
begin
   {Note NSString and CFStringRef are interchangable}
  Result := CFStrToUniStr(CFStringRef(aNSStr));
end;

procedure UniStrToNSStr(const aStr   : UnicodeString;
                          out aNSStr : NSString);
 {Create NSString from UnicodeString.
  Note: Calling code is responsible for calling returned
   NSString's release method.}
begin
   {Note NSString and CFStringRef are interchangable}
  UniStrToCFStr(aStr, CFStringRef(aNSStr));
end;

function UniStrToNSStr(const aStr : UnicodeString) : NSString;
 {Function version of previous procedure that can be called
   without releasing NSString result.
  Tip: You can use the procedure version and call release
   explictly to reduce use of autoreleased objects.}
begin
  UniStrToNSStr(aStr, Result);
  Result.autorelease;
end;


end.
