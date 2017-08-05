unit uRandom;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure Random(ABlock: PByte; ACount: Integer);

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows;

var
  RtlGenRandom: function(RandomBuffer: PByte; RandomBufferLength: ULONG): LongBool; stdcall;
{$ENDIF}

procedure Random(ABlock: PByte; ACount: Integer);
var
  I: Integer;
  Result: Boolean = False;
begin
{$IF DEFINED(MSWINDOWS)}
  Result:= Assigned(RtlGenRandom);
  if Result then Result:= RtlGenRandom(ABlock, ACount);
{$ENDIF}
  if not Result then for I:= 0 to ACount - 1 do ABlock[I]:= Byte(System.Random(256));
end;

initialization
{$IF DEFINED(MSWINDOWS)}
  @RtlGenRandom:= GetProcAddress(GetModuleHandle('advapi32.dll'), 'SystemFunction036');
{$ENDIF}

end.

