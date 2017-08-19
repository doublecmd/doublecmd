{
   Double Commander
   -------------------------------------------------------------------------
   Cryptographically secure pseudo-random number generator

   Copyright (C) 2017 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

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
  {$IF DEFINED(LINUX)}
  , dl, BaseUnix, InitC
  {$ENDIF}
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
  {$IF DEFINED(LINUX)}
    getrandom: function(buf: PByte; buflen: csize_t; flags: cuint): cint; cdecl;
  {$ENDIF}
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
{$ELSEIF DEFINED(UNIX)} {$IF DEFINED(LINUX)}
  if Assigned(getrandom) then
  begin
    repeat
      Result:= (getrandom(ABlock, ACount, 0) = ACount);
    until (Result = True) or (fpgetCerrno <> ESysEINTR);
  end;
  if not Result then
  {$ENDIF}
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
    if (Context.randidx = 0) then
    begin
      isaac_inita({%H-}Context, [Int32(GetTickCount64),
                                 Integer(GetThreadID),
                                 Integer(GetProcessID),
                                 GetHeapStatus.TotalFree,
                                 Int32(Trunc(Now * MSecsPerDay))], 5);
    end;
    isaac_read(Context, ABlock, ACount);
  end;
end;

initialization
{$IF DEFINED(MSWINDOWS)}
  @RtlGenRandom:= GetProcAddress(GetModuleHandle('advapi32.dll'), 'SystemFunction036');
{$ELSEIF DEFINED(UNIX)}
  HasRandom:= mbFileAccess(random_dev, fmOpenRead);
  {$IF DEFINED(LINUX)}
    @getrandom:= dlsym(dlopen('libc.so.6', RTLD_NOW), 'getrandom');
  {$ENDIF}
{$ENDIF}

end.

