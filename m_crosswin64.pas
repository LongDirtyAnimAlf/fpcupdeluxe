unit m_crosswin64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller;

implementation
type

{ TWin32To64 }

TWin32To64 = class(TCrossInstaller)
private

public

  constructor Create;
  destructor Destroy; override;
end;

{ TWin32To64 }

constructor TWin32To64.Create;
begin
  inherited Create;
end;

destructor TWin32To64.Destroy;
begin
  inherited Destroy;
end;

end.

