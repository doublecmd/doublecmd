{
  Implementations of BLAKE2b, BLAKE2s,
  aimed at portability and simplicity
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

function blake2s_compress( S: Pblake2s_state; const block: pcuint8 ): cint;
var
  i: csize_t;
  m: array[0..15] of cuint32;
  v: array[0..15] of cuint32;

  procedure G(r,i: cuint32; var a,b,c,d: cuint32); inline;
  begin
    a := a + b + m[blake2s_sigma[r][2*i+0]];
    d := RorDWord(d xor a, 16);
    c := c + d;
    b := RorDWord(b xor c, 12);
    a := a + b + m[blake2s_sigma[r][2*i+1]];
    d := RorDWord(d xor a, 8);
    c := c + d;
    b := RorDWord(b xor c, 7);
  end;

  {$define ROUND_MACRO:=
    G(r_,0,v[ 0],v[ 4],v[ 8],v[12]);
    G(r_,1,v[ 1],v[ 5],v[ 9],v[13]);
    G(r_,2,v[ 2],v[ 6],v[10],v[14]);
    G(r_,3,v[ 3],v[ 7],v[11],v[15]);
    G(r_,4,v[ 0],v[ 5],v[10],v[15]);
    G(r_,5,v[ 1],v[ 6],v[11],v[12]);
    G(r_,6,v[ 2],v[ 7],v[ 8],v[13]);
    G(r_,7,v[ 3],v[ 4],v[ 9],v[14]);
  }

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

  {$define r_:= 0} ROUND_MACRO;
  {$define r_:= 1} ROUND_MACRO;
  {$define r_:= 2} ROUND_MACRO;
  {$define r_:= 3} ROUND_MACRO;
  {$define r_:= 4} ROUND_MACRO;
  {$define r_:= 5} ROUND_MACRO;
  {$define r_:= 6} ROUND_MACRO;
  {$define r_:= 7} ROUND_MACRO;
  {$define r_:= 8} ROUND_MACRO;
  {$define r_:= 9} ROUND_MACRO;

  for i := 0 to 7 do
    S^.h[i] := S^.h[i] xor v[i] xor v[i + 8];

  Result := 0;
end;

