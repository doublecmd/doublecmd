{
    Double Commander
    -------------------------------------------------------------------------
    Fast CRC32 calculation algorithm
    http://create.stephan-brumme.com/crc32

    Copyright (C) 2011-2015 Stephan Brumme. All rights reserved.
    Slicing-by-16 contributed by Bulat Ziganshin
    See http://create.stephan-brumme.com/disclaimer.html

    Pascal tranlastion
    Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

    This software is provided 'as-is', without any express or implied warranty.
    In no event will the author be held liable for any damages arising from the
    use of this software. Permission is granted to anyone to use this software
    for any purpose, including commercial applications, and to alter it and
    redistribute it freely, subject to the following restrictions:

      1. The origin of this software must not be misrepresented; you must not
         claim that you wrote the original software.
      2. If you use this software in a product, an acknowledgment in the product
         documentation would be appreciated but is not required.
      3. Altered source versions must be plainly marked as such, and must not be
         misrepresented as being the original software.
}

unit DCcrc32;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure crc32_init;
function crc32_16bytes(const data: PByte; length: Integer; previousCrc32: Cardinal = 0): Cardinal;

implementation
{$R-}{$Q-}

const
  /// zlib's CRC32 polynomial
  Polynomial: Cardinal = $EDB88320;

var
  HaveTable: Boolean = False;
  Crc32Lookup: array[0..15, 0..255] of Cardinal;

procedure crc32_init;
var
  i, j: Integer;
  crc: Cardinal;
  slice: Integer;
begin
  if HaveTable then Exit;

  //// same algorithm as crc32_bitwise
  for i := 0 to $FF do
  begin
    crc := Cardinal(i);

    for j := 0 to 7 do
    begin
      if (crc and 1) <> 0 then
        crc := Polynomial xor (crc shr 1)
      else
        crc := (crc shr 1);
    end;

    Crc32Lookup[0][i] := crc;
  end;
  // ... and the following slicing-by-8 algorithm (from Intel):
  // http://www.intel.com/technology/comms/perfnet/download/CRC_generators.pdf
  // http://sourceforge.net/projects/slicing-by-8/
  for i := 0 to $FF do
  begin
    for slice := 1 to 15 do
      Crc32Lookup[slice][i] := (Crc32Lookup[slice - 1][i] shr 8) xor Crc32Lookup[0][Crc32Lookup[slice - 1][i] and $FF];
  end;
  HaveTable:= True;
end;

/// compute CRC32 (Slicing-by-16 algorithm)
function crc32_16bytes(const data: PByte; length: Integer; previousCrc32: Cardinal = 0): Cardinal;
const
  Unroll = 4;
  BytesAtOnce = 16 * Unroll;
var
  crc: cardinal;
  unrolling: integer;
  current: PLongWord;
  currentChar: PByte;
  one, two, three, four: cardinal;
begin
  crc :=  previousCrc32 xor $FFFFFFFF;
  current := PLongWord(data);

  // enabling optimization (at least -O2) automatically unrolls the inner for-loop
  while (length >= BytesAtOnce) do
  begin
    for unrolling := 0 to Unroll - 1 do
    begin
      one   := current^ xor crc; Inc(current);
      two   := current^; Inc(current);
      three := current^; Inc(current);
      four  := current^; Inc(current);
      crc   := Crc32Lookup[ 0][(four  shr 24) and $FF] xor
               Crc32Lookup[ 1][(four  shr 16) and $FF] xor
               Crc32Lookup[ 2][(four  shr  8) and $FF] xor
               Crc32Lookup[ 3][ four          and $FF] xor
               Crc32Lookup[ 4][(three shr 24) and $FF] xor
               Crc32Lookup[ 5][(three shr 16) and $FF] xor
               Crc32Lookup[ 6][(three shr  8) and $FF] xor
               Crc32Lookup[ 7][ three         and $FF] xor
               Crc32Lookup[ 8][(two   shr 24) and $FF] xor
               Crc32Lookup[ 9][(two   shr 16) and $FF] xor
               Crc32Lookup[10][(two   shr  8) and $FF] xor
               Crc32Lookup[11][ two           and $FF] xor
               Crc32Lookup[12][(one   shr 24) and $FF] xor
               Crc32Lookup[13][(one   shr 16) and $FF] xor
               Crc32Lookup[14][(one   shr  8) and $FF] xor
               Crc32Lookup[15][ one           and $FF];
    end;

    length -= BytesAtOnce;
  end;

  currentChar := PByte(current);
  // remaining 1 to 63 bytes (standard algorithm)
  while (length <> 0) do
  begin
    crc := (crc shr 8) xor Crc32Lookup[0][(crc and $FF) xor currentChar^];
    Inc(currentChar);
    Dec(length);
  end;

  Result:=  crc xor $FFFFFFFF;
end;

end.

