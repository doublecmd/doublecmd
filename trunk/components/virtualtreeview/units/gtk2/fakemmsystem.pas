unit fakemmsystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCLIntf;
  
function timeBeginPeriod(x1: DWord): DWord;

function timeEndPeriod(x1: DWord): DWord;

function timeGetTime: DWORD;

implementation

function timeBeginPeriod(x1: DWord): DWord;
begin
  //
end;

function timeEndPeriod(x1: DWord): DWord;
begin
  //
end;

function timeGetTime: DWORD;
begin
  Result := GetTickCount;
end;

end.

