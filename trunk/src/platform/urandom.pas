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
{$ELSEIF DEFINED(UNIX)}
uses
  DCOSUtils;

const
  random_dev = '/dev/urandom';

var
  HasRandom: Boolean = False;
{$ENDIF}

procedure Random(ABlock: PByte; ACount: Integer);
var
  I: THandle;
  Result: Boolean = False;
begin
{$IF DEFINED(MSWINDOWS)}
  Result:= Assigned(RtlGenRandom);
  if Result then Result:= RtlGenRandom(ABlock, ACount);
{$ELSEIF DEFINED(UNIX)}
  if HasRandom then
  begin
    I:= mbFileOpen(random_dev, fmOpenRead or fmShareDenyNone);
    Result:= (I <> feInvalidHandle);
    if Result then
    begin
      Result:= (FileRead(I, ABlock^, ACount) = ACount);
      FileClose(I);
    end;
  end;
{$ENDIF}
  if not Result then for I:= 0 to ACount - 1 do ABlock[I]:= Byte(System.Random(256));
end;

initialization
{$IF DEFINED(MSWINDOWS)}
  @RtlGenRandom:= GetProcAddress(GetModuleHandle('advapi32.dll'), 'SystemFunction036');
{$ELSEIF DEFINED(UNIX)}
  HasRandom:= mbFileAccess(random_dev, fmOpenRead);
{$ENDIF}

end.

