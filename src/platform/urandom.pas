unit uRandom;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure Random(ABlock: PByte; ACount: Integer);

implementation

uses
  ISAAC
{$IF DEFINED(MSWINDOWS)}
  , Windows
{$ELSEIF DEFINED(UNIX)}
  , DCOSUtils
{$ENDIF}
  ;

threadvar
  Context: isaac_ctx;

{$IF DEFINED(MSWINDOWS)}
var
  RtlGenRandom: function(RandomBuffer: PByte; RandomBufferLength: ULONG): LongBool; stdcall;
{$ELSEIF DEFINED(UNIX)}
const
  random_dev = '/dev/urandom';

var
  HasRandom: Boolean = False;
{$ENDIF}

procedure Random(ABlock: PByte; ACount: Integer);
var
{$IF DEFINED(UNIX)}
  Handle: THandle;
{$ENDIF}
  Result: Boolean = False;
begin
{$IF DEFINED(MSWINDOWS)}
  Result:= Assigned(RtlGenRandom);
  if Result then Result:= RtlGenRandom(ABlock, ACount);
{$ELSEIF DEFINED(UNIX)}
  if HasRandom then
  begin
    Handle:= mbFileOpen(random_dev, fmOpenRead or fmShareDenyNone);
    Result:= (Handle <> feInvalidHandle);
    if Result then
    begin
      Result:= (FileRead(Handle, ABlock^, ACount) = ACount);
      FileClose(Handle);
    end;
  end;
{$ENDIF}
  if not Result then
  begin
    if (CompareDWord(Context.randrsl[0], Context.randrsl[128], 128) = 0) then
    begin
      isaac_inita({%H-}Context, [GetTickCount,
                                 Integer(GetThreadID),
                                 Integer(GetProcessID),
                                 GetHeapStatus.TotalFree], 4);
    end;
    isaac_read(Context, ABlock, ACount);
  end;
end;

initialization
{$IF DEFINED(MSWINDOWS)}
  @RtlGenRandom:= GetProcAddress(GetModuleHandle('advapi32.dll'), 'SystemFunction036');
{$ELSEIF DEFINED(UNIX)}
  HasRandom:= mbFileAccess(random_dev, fmOpenRead);
{$ENDIF}

end.

