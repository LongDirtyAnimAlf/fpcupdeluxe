{ Process utility unit. Extends TProcess.
Not unicode-aware (change this when FPC becomes so).

Copyright (C) 2012-2014 Ludo Brands, Reinier Olislagers

This unit is licensed as modified LGPL or MIT, at your choice. Licenses below
}
{
This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.
}
unit processutils;

{$mode objfpc}{$H+}
{not $DEFINE DEBUGCONSOLE} //define debug to get writeln output of commands called

interface

uses
  Classes, SysUtils,
  process;

const
  // Internal error code/result codes:
  PROC_INTERNALERROR=-1; // error while running process code in this unit
  PROC_INTERNALEXCEPTION=-2; //exception while running process code in this unit
  {$IFDEF MSWINDOWS}
  PATHVARNAME = 'Path'; //Name for path environment variable
  {$ELSE}
  //Unix/Linux
  PATHVARNAME = 'PATH';
  {$ENDIF MSWINDOWS}

// Convenience functions
// Runs command, returns result code. Negative codes are processutils internal error codes
function ExecuteCommand(Commandline: string; Verbose:boolean): integer; overload;
// Runs command, returns result code. Negative codes are processutils internal error codes
function ExecuteCommand(Commandline: string; out Output:string; Verbose:boolean): integer; overload;
// Runs command, returns result code. Negative codes are processutils internal error codes
function ExecuteCommandInDir(Commandline, Directory: string; Verbose:boolean): integer; overload;
// Runs command, returns result code. Negative codes are processutils internal error codes
function ExecuteCommandInDir(Commandline, Directory: string; out Output:string; Verbose:boolean): integer; overload;
// Runs command, returns result code. Negative codes are processutils internal error codes
// PrependPath is prepended to existing path. If empty, keep current path
function ExecuteCommandInDir(Commandline, Directory: string; out Output:string; PrependPath: string; Verbose:boolean): integer; overload;

implementation

uses
  RunTools;

function ExecuteCommand(Commandline: string; Verbose: boolean): integer;
var
  s:string='';
begin
  Result:=ExecuteCommandInDir(Commandline,'',s,Verbose);
end;

function ExecuteCommand(Commandline: string; out Output: string;
  Verbose: boolean): integer;
begin
  Result:=ExecuteCommandInDir(Commandline,'',Output,Verbose);
end;

function ExecuteCommandInDir(Commandline, Directory: string; Verbose: boolean
  ): integer;
var
  s:string='';
begin
  Result:=ExecuteCommandInDir(Commandline,Directory,s,Verbose);
end;

function ExecuteCommandInDir(Commandline, Directory: string;
  out Output: string; Verbose: boolean): integer;
begin
  Result:=ExecuteCommandInDir(CommandLine,Directory,Output,'',Verbose);
end;

function ExecuteCommandInDir(Commandline, Directory: string;
  out Output: string; PrependPath: string; Verbose: boolean): integer;
var
  OldPath: string;
  i:integer;
  aTool:TExternalTool;
begin
  aTool:=TExternalTool.Create(nil);

  try
    if Directory<>'' then
      aTool.Process.CurrentDirectory:=Directory;

    // Prepend specified PrependPath if needed:
    if PrependPath<>'' then
    begin
      OldPath:=aTool.Environment.GetVar(PATHVARNAME);
      if OldPath<>'' then
         aTool.Environment.SetVar(PATHVARNAME, PrependPath+PathSeparator+OldPath)
      else
        aTool.Environment.SetVar(PATHVARNAME, PrependPath);
    end;

    CommandToList(Commandline,aTool.Process.Parameters);

    If aTool.Process.Parameters.Count>0 then
    begin
      aTool.Process.Executable:=aTool.Process.Parameters[0];
      aTool.Process.Parameters.Delete(0);
      i:=aTool.Process.Parameters.IndexOf('emptystring');
      if (i<>-1) then aTool.Process.Parameters.Strings[i]:='""';
    end;

    aTool.Execute;

    aTool.WaitForExit;

    Output:=aTool.WorkerOutput.Text;

    Result:=aTool.ExitCode;

  finally
    aTool.Free;
  end;

end;

end.

