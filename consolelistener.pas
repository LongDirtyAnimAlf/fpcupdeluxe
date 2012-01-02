unit consolelistener;
{
Starts a command-line/console program and records its output (stdout and stderr)
Single threaded so your program waits for the other to finish.

Ideas:
- turn into consolewhisperer that can also send input to your console program (think fdisk ;) )
- make it multi-threaded?
}
{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils; 

implementation

end.

