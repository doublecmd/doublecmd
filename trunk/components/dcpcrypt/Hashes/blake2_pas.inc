{
  Implementations of BLAKE2b, BLAKE2s, aimed at portability and simplicity

  Copyright (C) 2018 Alexander Koblov (alexx2000@mail.ru)

  Based on blake2*.pas, (C) Copyright 2017 Wolfgang Ehrhardt

  This software is provided 'as-is', without any express or implied warranty.
  In no event will the authors be held liable for any damages arising from
  the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
}

const blake2s_sigma: array[0..9] of array[0..15] of cuint8 =
(
  (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ) ,
  ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ) ,
  ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ) ,
  (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ) ,
  (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ) ,
  (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ) ,
  ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ) ,
  ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ) ,
  (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ) ,
  ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 )
);

const blake2b_sigma: array[0..11] of array[0..15] of cuint8 =
(
  (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ) ,
  ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ) ,
  ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ) ,
  (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ) ,
  (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ) ,
  (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ) ,
  ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ) ,
  ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ) ,
  (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ) ,
  ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 ) ,
  (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ) ,
  ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 )
);

function blake2s_compress( S: Pblake2s_state; const block: pcuint8 ): cint;
var
  i: csize_t;
  tem, round: cint32;
  m: array[0..15] of cuint32;
  v: array[0..15] of cuint32;
begin
  for i := 0 to 15 do
    m[i] := load32( @block[i * sizeof( m[i] )] );

  for i := 0 to 7 do
    v[i] := S^.h[i];

  v[ 8] := blake2s_IV[0];
  v[ 9] := blake2s_IV[1];
  v[10] := blake2s_IV[2];
  v[11] := blake2s_IV[3];
  v[12] := S^.t[0] xor blake2s_IV[4];
  v[13] := S^.t[1] xor blake2s_IV[5];
  v[14] := S^.f[0] xor blake2s_IV[6];
  v[15] := S^.f[1] xor blake2s_IV[7];

  {Code contributed by EddyHawk}
  for round:=0 to 9 do begin
    {separates BLAKE2s round into quarter-rounds}
    {replaces the rotr with direct code}
    {uses tem var for xor-ed words}
    {moves message-additions to the front}
    {regroups the rest}

    v[ 0] := (v[ 0] + v[ 4]) + m[blake2s_sigma[round][ 0]];
    v[ 1] := (v[ 1] + v[ 5]) + m[blake2s_sigma[round][ 2]];
    v[ 2] := (v[ 2] + v[ 6]) + m[blake2s_sigma[round][ 4]];
    v[ 3] := (v[ 3] + v[ 7]) + m[blake2s_sigma[round][ 6]];

    tem   := v[12] xor v[ 0];
    v[12] := (tem shr 16) or (tem shl (32-16));
    tem   := v[13] xor v[ 1];
    v[13] := (tem shr 16) or (tem shl (32-16));
    tem   := v[14] xor v[ 2];
    v[14] := (tem shr 16) or (tem shl (32-16));
    tem   := v[15] xor v[ 3];
    v[15] := (tem shr 16) or (tem shl (32-16));

    v[ 8] := v[ 8] + v[12];
    v[ 9] := v[ 9] + v[13];
    v[10] := v[10] + v[14];
    v[11] := v[11] + v[15];

    tem   := v[ 4] xor v[ 8];
    v[ 4] := (tem shr 12) or (tem shl (32-12));
    tem   := v[ 5] xor v[ 9];
    v[ 5] := (tem shr 12) or (tem shl (32-12));
    tem   := v[ 6] xor v[10];
    v[ 6] := (tem shr 12) or (tem shl (32-12));
    tem   := v[ 7] xor v[11];
    v[ 7] := (tem shr 12) or (tem shl (32-12));

    {2nd quarter-round}
    v[ 0]  := (v[ 0] + v[ 4]) + m[blake2s_sigma[round][ 1]];
    v[ 1]  := (v[ 1] + v[ 5]) + m[blake2s_sigma[round][ 3]];
    v[ 2]  := (v[ 2] + v[ 6]) + m[blake2s_sigma[round][ 5]];
    v[ 3]  := (v[ 3] + v[ 7]) + m[blake2s_sigma[round][ 7]];

    tem   := v[12] xor v[ 0];
    v[12] := (tem shr  8) or (tem shl (32- 8));
    tem   := v[13] xor v[ 1];
    v[13] := (tem shr  8) or (tem shl (32- 8));
    tem   := v[14] xor v[ 2];
    v[14] := (tem shr  8) or (tem shl (32- 8));
    tem   := v[15] xor v[ 3];
    v[15] := (tem shr  8) or (tem shl (32- 8));

    v[ 8] := v[ 8] + v[12];
    v[ 9] := v[ 9] + v[13];
    v[10] := v[10] + v[14];
    v[11] := v[11] + v[15];

    tem   := v[ 4] xor v[ 8];
    v[ 4] := (tem shr  7) or (tem shl (32- 7));
    tem   := v[ 5] xor v[ 9];
    v[ 5] := (tem shr  7) or (tem shl (32- 7));
    tem   := v[ 6] xor v[10];
    v[ 6] := (tem shr  7) or (tem shl (32- 7));
    tem   := v[ 7] xor v[11];
    v[ 7] := (tem shr  7) or (tem shl (32- 7));

    {3rd quarter-round}
    v[ 0] := (v[ 0] + v[ 5]) + m[blake2s_sigma[round][ 8]];
    v[ 1] := (v[ 1] + v[ 6]) + m[blake2s_sigma[round][10]];
    v[ 2] := (v[ 2] + v[ 7]) + m[blake2s_sigma[round][12]];
    v[ 3] := (v[ 3] + v[ 4]) + m[blake2s_sigma[round][14]];

    tem   := v[15] xor v[ 0];
    v[15] := (tem shr 16) or (tem shl (32-16));
    tem   := v[12] xor v[ 1];
    v[12] := (tem shr 16) or (tem shl (32-16));
    tem   := v[13] xor v[ 2];
    v[13] := (tem shr 16) or (tem shl (32-16));
    tem   := v[14] xor v[ 3];
    v[14] := (tem shr 16) or (tem shl (32-16));

    v[10] := v[10] + v[15];
    v[11] := v[11] + v[12];
    v[ 8] := v[ 8] + v[13];
    v[ 9] := v[ 9] + v[14];

    tem   := v[ 5] xor v[10];
    v[ 5] := (tem shr 12) or (tem shl (32-12));
    tem   := v[ 6] xor v[11];
    v[ 6] := (tem shr 12) or (tem shl (32-12));
    tem   := v[ 7] xor v[ 8];
    v[ 7] := (tem shr 12) or (tem shl (32-12));
    tem   := v[ 4] xor v[ 9];
    v[ 4] := (tem shr 12) or (tem shl (32-12));

    {4th quarter-round}
    v[ 0] := (v[ 0] + v[ 5]) + m[blake2s_sigma[round][ 9]];
    v[ 1] := (v[ 1] + v[ 6]) + m[blake2s_sigma[round][11]];
    v[ 2] := (v[ 2] + v[ 7]) + m[blake2s_sigma[round][13]];
    v[ 3] := (v[ 3] + v[ 4]) + m[blake2s_sigma[round][15]];

    tem   := v[15] xor v[ 0];
    v[15] := (tem shr  8) or (tem shl (32- 8));
    tem   := v[12] xor v[ 1];
    v[12] := (tem shr  8) or (tem shl (32- 8));
    tem   := v[13] xor v[ 2];
    v[13] := (tem shr  8) or (tem shl (32- 8));
    tem   := v[14] xor v[ 3];
    v[14] := (tem shr  8) or (tem shl (32- 8));

    v[10] := v[10] + v[15];
    v[11] := v[11] + v[12];
    v[ 8] := v[ 8] + v[13];
    v[ 9] := v[ 9] + v[14];

    tem   := v[ 5] xor v[10];
    v[ 5] := (tem shr  7) or (tem shl (32- 7));
    tem   := v[ 6] xor v[11];
    v[ 6] := (tem shr  7) or (tem shl (32- 7));
    tem   := v[ 7] xor v[ 8];
    v[ 7] := (tem shr  7) or (tem shl (32- 7));
    tem   := v[ 4] xor v[ 9];
    v[ 4] := (tem shr  7) or (tem shl (32- 7));
  end;

  for i := 0 to 7 do
    S^.h[i] := S^.h[i] xor v[i] xor v[i + 8];

  Result := 0;
end;

procedure blake2b_compress( S: Pblake2b_state; const block: pcuint8 );
var
  i: csize_t;
  tem, round: cint64;
  m: array[0..15] of cuint64;
  v: array[0..15] of cuint64;
begin
  for i := 0 to 15 do
    m[i] := load64( block + i * sizeof( m[i] ) );

  for i := 0 to 7 do
    v[i] := S^.h[i];

  v[ 8] := cuint64(blake2b_IV[0]);
  v[ 9] := cuint64(blake2b_IV[1]);
  v[10] := cuint64(blake2b_IV[2]);
  v[11] := cuint64(blake2b_IV[3]);
  v[12] := cuint64(blake2b_IV[4] xor S^.t[0]);
  v[13] := cuint64(blake2b_IV[5] xor S^.t[1]);
  v[14] := cuint64(blake2b_IV[6] xor S^.f[0]);
  v[15] := cuint64(blake2b_IV[7] xor S^.f[1]);

  {do 12 rounds}
  for round:=0 to 11 do begin
    {** EddyHawk speed-ups **}
    {use same rearrangements as blake2s' 32/64 bit code}
    v[ 0] := (v[ 0] + v[ 4]) + m[blake2b_sigma[round][ 0]];
    v[ 1] := (v[ 1] + v[ 5]) + m[blake2b_sigma[round][ 2]];
    v[ 2] := (v[ 2] + v[ 6]) + m[blake2b_sigma[round][ 4]];
    v[ 3] := (v[ 3] + v[ 7]) + m[blake2b_sigma[round][ 6]];

    tem   := v[12] xor v[ 0];
    v[12] := (tem shr 32) or (tem shl (64-32));
    tem   := v[13] xor v[ 1];
    v[13] := (tem shr 32) or (tem shl (64-32));
    tem   := v[14] xor v[ 2];
    v[14] := (tem shr 32) or (tem shl (64-32));
    tem   := v[15] xor v[ 3];
    v[15] := (tem shr 32) or (tem shl (64-32));

    v[ 8] := v[ 8] + v[12];
    v[ 9] := v[ 9] + v[13];
    v[10] := v[10] + v[14];
    v[11] := v[11] + v[15];

    tem   := v[ 4] xor v[ 8];
    v[ 4] := (tem shr 24) or (tem shl (64-24));
    tem   := v[ 5] xor v[ 9];
    v[ 5] := (tem shr 24) or (tem shl (64-24));
    tem   := v[ 6] xor v[10];
    v[ 6] := (tem shr 24) or (tem shl (64-24));
    tem   := v[ 7] xor v[11];
    v[ 7] := (tem shr 24) or (tem shl (64-24));

    {---}

    v[ 0] := (v[ 0] + v[ 4]) + m[blake2b_sigma[round][ 1]];
    v[ 1] := (v[ 1] + v[ 5]) + m[blake2b_sigma[round][ 3]];
    v[ 2] := (v[ 2] + v[ 6]) + m[blake2b_sigma[round][ 5]];
    v[ 3] := (v[ 3] + v[ 7]) + m[blake2b_sigma[round][ 7]];

    tem   := v[12] xor v[ 0];
    v[12] := (tem shr 16) or (tem shl (64-16));
    tem   := v[13] xor v[ 1];
    v[13] := (tem shr 16) or (tem shl (64-16));
    tem   := v[14] xor v[ 2];
    v[14] := (tem shr 16) or (tem shl (64-16));
    tem   := v[15] xor v[ 3];
    v[15] := (tem shr 16) or (tem shl (64-16));

    v[ 8] := v[ 8] + v[12];
    v[ 9] := v[ 9] + v[13];
    v[10] := v[10] + v[14];
    v[11] := v[11] + v[15];

    tem   := v[ 4] xor v[ 8];
    v[ 4] := (tem shr 63) or (tem shl (64-63));
    tem   := v[ 5] xor v[ 9];
    v[ 5] := (tem shr 63) or (tem shl (64-63));
    tem   := v[ 6] xor v[10];
    v[ 6] := (tem shr 63) or (tem shl (64-63));
    tem   := v[ 7] xor v[11];
    v[ 7] := (tem shr 63) or (tem shl (64-63));

    {---}

    v[ 0] := (v[ 0] + v[ 5]) + m[blake2b_sigma[round][ 8]];
    v[ 1] := (v[ 1] + v[ 6]) + m[blake2b_sigma[round][10]];
    v[ 2] := (v[ 2] + v[ 7]) + m[blake2b_sigma[round][12]];
    v[ 3] := (v[ 3] + v[ 4]) + m[blake2b_sigma[round][14]];

    tem   := v[15] xor v[ 0];
    v[15] := (tem shr 32) or (tem shl (64-32));
    tem   := v[12] xor v[ 1];
    v[12] := (tem shr 32) or (tem shl (64-32));
    tem   := v[13] xor v[ 2];
    v[13] := (tem shr 32) or (tem shl (64-32));
    tem   := v[14] xor v[ 3];
    v[14] := (tem shr 32) or (tem shl (64-32));

    v[10] := v[10] + v[15];
    v[11] := v[11] + v[12];
    v[ 8] := v[ 8] + v[13];
    v[ 9] := v[ 9] + v[14];

    tem   := v[ 5] xor v[10];
    v[ 5] := (tem shr 24) or (tem shl (64-24));
    tem   := v[ 6] xor v[11];
    v[ 6] := (tem shr 24) or (tem shl (64-24));
    tem   := v[ 7] xor v[ 8];
    v[ 7] := (tem shr 24) or (tem shl (64-24));
    tem   := v[ 4] xor v[ 9];
    v[ 4] := (tem shr 24) or (tem shl (64-24));

    {---}

    v[ 0] := (v[ 0] + v[ 5]) + m[blake2b_sigma[round][ 9]];
    v[ 1] := (v[ 1] + v[ 6]) + m[blake2b_sigma[round][11]];
    v[ 2] := (v[ 2] + v[ 7]) + m[blake2b_sigma[round][13]];
    v[ 3] := (v[ 3] + v[ 4]) + m[blake2b_sigma[round][15]];

    tem   := v[15] xor v[ 0];
    v[15] := (tem shr 16) or (tem shl (64-16));
    tem   := v[12] xor v[ 1];
    v[12] := (tem shr 16) or (tem shl (64-16));
    tem   := v[13] xor v[ 2];
    v[13] := (tem shr 16) or (tem shl (64-16));
    tem   := v[14] xor v[ 3];
    v[14] := (tem shr 16) or (tem shl (64-16));

    v[10] := v[10] + v[15];
    v[11] := v[11] + v[12];
    v[ 8] := v[ 8] + v[13];
    v[ 9] := v[ 9] + v[14];

    tem   := v[ 5] xor v[10];
    v[ 5] := (tem shr 63) or (tem shl (64-63));
    tem   := v[ 6] xor v[11];
    v[ 6] := (tem shr 63) or (tem shl (64-63));
    tem   := v[ 7] xor v[ 8];
    v[ 7] := (tem shr 63) or (tem shl (64-63));
    tem   := v[ 4] xor v[ 9];
    v[ 4] := (tem shr 63) or (tem shl (64-63));
  end;

  for i := 0 to 7 do
    S^.h[i] := S^.h[i] xor v[i] xor v[i + 8];
end;

