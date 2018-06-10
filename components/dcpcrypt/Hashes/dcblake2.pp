{
   BLAKE2 reference source code package - reference C implementations

   Written in 2012 by Samuel Neves <sneves@dei.uc.pt>

   Pascal tranlastion in 2014-2018 by Alexander Koblov (alexx2000@mail.ru)

   To the extent possible under law, the author(s) have dedicated all copyright
   and related and neighboring rights to this software to the public domain
   worldwide. This software is distributed without any warranty.

   You should have received a copy of the CC0 Public Domain Dedication along with
   this software. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
}

unit DCblake2;

{$mode objfpc}{$H+}
{$macro on}{$R-}{$Q-}
{$define USE_MTPROCS}

interface

uses
  SysUtils, CTypes;

const
  BLAKE2S_BLOCKBYTES = 64;
  BLAKE2S_OUTBYTES   = 32;
  BLAKE2S_KEYBYTES   = 32;
  BLAKE2S_SALTBYTES  = 8;
  BLAKE2S_PERSONALBYTES = 8;
  BLAKE2S_PARALLELISM_DEGREE = 8;

const
  BLAKE2B_BLOCKBYTES = 128;
  BLAKE2B_OUTBYTES   = 64;
  BLAKE2B_KEYBYTES   = 64;
  BLAKE2B_SALTBYTES  = 16;
  BLAKE2B_PERSONALBYTES = 16;
  BLAKE2B_PARALLELISM_DEGREE = 4;

type
  {$packrecords 1}
  Pblake2s_param = ^blake2s_param;
  blake2s_param = record
    digest_length: cuint8; // 1
    key_length:    cuint8; // 2
    fanout: cuint8;        // 3
    depth: cuint8;         // 4
    leaf_length: cuint32;  // 8
    node_offset: array[0..5] of cuint8;// 14
    node_depth: cuint8;    // 15
    inner_length: cuint8;  // 16
    // uint8_t  reserved[0];
    salt: array[0..Pred(BLAKE2S_SALTBYTES)] of cuint8; // 24
    personal: array[0..Pred(BLAKE2S_PERSONALBYTES)] of cuint8; // 32
  end;
  {$packrecords 8}
  Pblake2s_state = ^blake2s_state;
  blake2s_state = record
    h: array[0..7] of cuint32;
    t: array[0..1] of cuint32;
    f: array[0..1] of cuint32;
    buf: array[0..Pred(2 * BLAKE2S_BLOCKBYTES)] of cuint8;
    buflen: csize_t;
    last_node: cuint8;
  end;
  {$packrecords 1}
  Pblake2sp_state = ^blake2sp_state;
  blake2sp_state = record
    S: array[0..7] of blake2s_state;
    R: blake2s_state;
    buf: array[0..Pred(8 * BLAKE2S_BLOCKBYTES)] of cuint8;
    buflen: csize_t;
    inlen: csize_t;
    inp: PByte;
  end;
  {$packrecords c}
  Pblake2b_state = ^blake2b_state;
  blake2b_state = record
    h: array[0..7] of cuint64;
    t: array[0..1] of cuint64;
    f: array[0..1] of cuint64;
    buf: array [0..Pred(BLAKE2B_BLOCKBYTES)] of cuint8;
    buflen: csize_t;
    outlen: csize_t;
    last_node: cuint8;
  end;
  {$packrecords 1}
  Pblake2b_param = ^blake2b_param;
  blake2b_param = record
    digest_length: uint8; // 1
    key_length: cuint8;   // 2
    fanout: cuint8;       // 3
    depth: cuint8;        // 4
    leaf_length: cuint32; // 8
    node_offset: cuint32; // 12
    xof_length: cuint32;  // 16
    node_depth: cuint8;   // 17
    inner_length: cuint8; // 18
    reserved: array[0..13] of cuint8; // 32
    salt: array [0..Pred(BLAKE2B_SALTBYTES)] of cuint8; // 48
    personal: array[0..Pred(BLAKE2B_PERSONALBYTES)] of cuint8; // 64
  end;
  {$packrecords default}
  Pblake2bp_state = ^blake2bp_state;
  blake2bp_state = record
    S: array[0..3] of blake2b_state;
    R: blake2b_state;
    buf: array[0..Pred(4 * BLAKE2B_BLOCKBYTES)] of cuint8;
    buflen: csize_t;
    outlen: csize_t;
    inlen: csize_t;
    inp: PByte;
  end;

function blake2s_init( S: Pblake2s_state; const outlen: cuint8 ): cint;
function blake2s_update( S: Pblake2s_state; inp: pcuint8; inlen: cuint64 ): cint;
function blake2s_final( S: Pblake2s_state; outp: pcuint8; outlen: cuint8 ): cint;

function blake2sp_init( S: Pblake2sp_state; const outlen: cuint8 ): cint;
function blake2sp_update( S: Pblake2sp_state; inp: pcuint8; inlen: cuint64 ): cint;
function blake2sp_final( S: Pblake2sp_state; outp: pcuint8; const outlen: cuint8 ): cint;

function blake2b_init( S: Pblake2b_state; outlen: csize_t ): cint;
function blake2b_update( S: Pblake2b_state; pin: pcuint8; inlen: csize_t ): cint;
function blake2b_final( S: Pblake2b_state; pout: pcuint8; outlen: csize_t ): cint;

function blake2bp_init( S: Pblake2bp_state; outlen: csize_t ): cint;
function blake2bp_update( S: Pblake2bp_state; inp: pcuint8; inlen: csize_t ): cint;
function blake2bp_final( S: Pblake2bp_state; out_: PByte; outlen: csize_t ): cint;

implementation

{$IF DEFINED(USE_MTPROCS)}
uses
  MTProcs;
{$ELSE}
type
  TMultiThreadProcItem = Pointer;
{$ENDIF}

const blake2s_IV: array[0..7] of cuint32 =
(
  $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
  $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
);

const blake2b_IV: array[0..7] of cint64 =
(
    $6a09e667f3bcc908, $bb67ae8584caa73b,
    $3c6ef372fe94f82b, $a54ff53a5f1d36f1,
    $510e527fade682d1, $9b05688c2b3e6c1f,
    $1f83d9abfb41bd6b, $5be0cd19137e2179
);

function load32( const src: Pointer ): cuint32; inline;
begin
  Result := NtoLE(pcuint32(src)^);
end;

function load64( const src: pointer ): cuint64; inline;
begin
  Result := NtoLE(pcuint64(src)^);
end;

procedure store32( dst: pointer; w: cuint32 ); inline;
begin
  pcuint32(dst)^ := LEtoN(w);
end;

procedure store64( dst: pointer; w: cuint64 ); inline;
begin
  pcuint64(dst)^ := LEtoN(w);
end;

function load48( const src: pointer ): cuint64; inline;
var
  w: cuint64;
  p: pcuint8;
begin
  p := pcuint8(src);
  w := p^; Inc(p);
  w := w or cuint64( p^ ) shl  8; inc(p);
  w := w or cuint64( p^ ) shl 16; inc(p);
  w := w or cuint64( p^ ) shl 24; inc(p);
  w := w or cuint64( p^ ) shl 32; inc(p);
  w := w or cuint64( p^ ) shl 40; inc(p);
  Result := w;
end;

procedure store48( dst: pointer; w: cuint64 ); inline;
var
  p: pcuint8;
begin
  p := pcuint8(dst);
  p^ := cuint8(w); w := w shr 8; inc(p);
  p^ := cuint8(w); w := w shr 8; inc(p);
  p^ := cuint8(w); w := w shr 8; inc(p);
  p^ := cuint8(w); w := w shr 8; inc(p);
  p^ := cuint8(w); w := w shr 8; inc(p);
  p^ := cuint8(w); inc(p);
end;

function blake2s_set_lastnode( S: Pblake2s_state ): cint; inline;
begin
  S^.f[1] := $FFFFFFFF;
  Result := 0;
end;

function blake2s_clear_lastnode( S: Pblake2s_state ): cint; inline;
begin
  S^.f[1] := 0;
  Result := 0;
end;

//* Some helper functions, not necessarily useful */
function blake2s_set_lastblock( S: Pblake2s_state ): cint; inline;
begin
  if( S^.last_node <> 0 ) then blake2s_set_lastnode( S );

  S^.f[0] := $FFFFFFFF;
  Result := 0;
end;

function blake2s_clear_lastblock( S: Pblake2s_state ): cint; inline;
begin
  if( S^.last_node <> 0 ) then blake2s_clear_lastnode( S );

  S^.f[0] := 0;
  Result := 0;
end;

function blake2s_increment_counter( S: Pblake2s_state; const inc: cuint32 ): cint; inline;
begin
  S^.t[0] += inc;
  S^.t[1] += cuint32( S^.t[0] < inc );
  Result := 0;
end;

function blake2s_init0( S: Pblake2s_state ): cint; inline;
var
  i: cint;
begin
  FillChar( S^, sizeof( blake2s_state ), 0 );

  for i := 0 to 8 - 1 do S^.h[i] := blake2s_IV[i];

  Result := 0;
end;

//* init2 xors IV with input parameter block */
function blake2s_init_param( S: Pblake2s_state; const P: Pblake2s_param ): cint;
var
  i: csize_t;
  pp: pcuint32;
begin
  blake2s_init0( S );
  pp := pcuint32( P );

  //* IV XOR ParamBlock */
  // for i := 0; i < 8; ++i )
  for i := 0 to 8 - 1 do
    S^.h[i] := S^.h[i] xor load32( @pp[i] );

  Result := 0;
end;

// Sequential blake2s initialization
function blake2s_init( S: Pblake2s_state; const outlen: cuint8 ): cint;
var
  P: blake2s_param;
begin
  //* Move interval verification here? */
  if ( ( outlen = 0 ) or ( outlen > BLAKE2S_OUTBYTES ) ) then Exit(-1);

  P.digest_length := outlen;
  P.key_length    := 0;
  P.fanout        := 1;
  P.depth         := 1;
  store32( @P.leaf_length, 0 );
  store48( @P.node_offset, 0 );
  P.node_depth    := 0;
  P.inner_length  := 0;
  // memset(P^.reserved, 0, sizeof(P^.reserved) );
  FillChar( P.salt, sizeof( P.salt ), 0 );
  FillChar( P.personal, sizeof( P.personal ), 0 );
  Result := blake2s_init_param( S, @P );
end;

{$IF DEFINED(CPUX86_64)}
  {$include blake2_sse.inc}
{$ELSE}
  {$include blake2_pas.inc}
{$ENDIF}

function blake2s_update( S: Pblake2s_state; inp: pcuint8; inlen: cuint64 ): cint;
var
  left, fill: csize_t;
begin
  while( inlen > 0 ) do
  begin
    left := S^.buflen;
    fill := 2 * BLAKE2S_BLOCKBYTES - left;

    if( inlen > fill ) then
    begin
      Move( inp^, S^.buf[left], fill ); // Fill buffer
      S^.buflen += fill;
      blake2s_increment_counter( S, BLAKE2S_BLOCKBYTES );
      blake2s_compress( S, S^.buf ); // Compress
      Move( S^.buf[BLAKE2S_BLOCKBYTES], S^.buf, BLAKE2S_BLOCKBYTES ); // Shift buffer left
      S^.buflen -= BLAKE2S_BLOCKBYTES;
      inp += fill;
      inlen -= fill;
    end
    else // inlen <= fill
    begin
      Move( inp^, S^.buf [left], inlen );
      S^.buflen += inlen; // Be lazy, do not compress
      inp += inlen;
      inlen -= inlen;
    end;
  end;

  Result := 0;
end;

function blake2s_final( S: Pblake2s_state; outp: pcuint8; outlen: cuint8 ): cint;
var
  i: cint;
 buffer: array[0..Pred(BLAKE2S_OUTBYTES)] of cuint8;
begin
  if( S^.buflen > BLAKE2S_BLOCKBYTES ) then
  begin
    blake2s_increment_counter( S, BLAKE2S_BLOCKBYTES );
    blake2s_compress( S, S^.buf);
    S^.buflen -= BLAKE2S_BLOCKBYTES;
    Move( S^.buf[BLAKE2S_BLOCKBYTES], S^.buf, S^.buflen );
  end;

  blake2s_increment_counter( S, cuint32(S^.buflen) );
  blake2s_set_lastblock( S );
  FillChar( S^.buf[S^.buflen], 2 * BLAKE2S_BLOCKBYTES - S^.buflen, 0 ); //* Padding */
  blake2s_compress( S, S^.buf );

  for i := 0 to 7 do //* Output full hash to temp buffer */
    store32( @buffer[sizeof( S^.h[i] ) * i], S^.h[i] );

  Move( buffer, outp^, outlen );
  Result := 0;
end;

function blake2sp_init_leaf(S: Pblake2s_state; outlen: cuint8; keylen: cuint8; offset: cuint64):cint; inline;
var
  P: blake2s_param;
begin
  P.digest_length := outlen;
  P.key_length := keylen;
  P.fanout := BLAKE2S_PARALLELISM_DEGREE;
  P.depth := 2;
  store32( @P.leaf_length, 0 );
  store48( @P.node_offset[0], offset );
  P.node_depth := 0;
  P.inner_length := BLAKE2S_OUTBYTES;
  FillChar( P.salt, sizeof( P.salt ), 0 );
  FillChar( P.personal, sizeof( P.personal ), 0 );
  Result:= blake2s_init_param( S, @P );
end;

function blake2sp_init_root( S: Pblake2s_state; outlen: cuint8; keylen: cuint8 ): cint; inline;
var
  P: blake2s_param;
begin
  P.digest_length := outlen;
  P.key_length := keylen;
  P.fanout := BLAKE2S_PARALLELISM_DEGREE;
  P.depth := 2;
  store32( @P.leaf_length, 0 );
  store48( @P.node_offset[0], 0 );
  P.node_depth := 1;
  P.inner_length := BLAKE2S_OUTBYTES;
  FillChar( P.salt, sizeof( P.salt ), 0 );
  FillChar( P.personal, sizeof( P.personal ), 0 );
  Result:= blake2s_init_param( S, @P );
end;

function blake2sp_init( S: Pblake2sp_state; const outlen: cuint8 ): cint;
var
  i: csize_t;
begin
  if (outlen = 0) or (outlen > BLAKE2S_OUTBYTES) then Exit(-1);

  FillChar( S^.buf, sizeof( S^.buf ), 0 );
  S^.buflen := 0;

  if( blake2sp_init_root( @S^.R, outlen, 0 ) < 0 ) then Exit(-1);

  for i := 0 to BLAKE2S_PARALLELISM_DEGREE - 1 do
    if ( blake2sp_init_leaf( @S^.S[i], outlen, 0, i ) < 0 ) then Exit(-1);

  S^.R.last_node := 1;
  S^.S[BLAKE2S_PARALLELISM_DEGREE - 1].last_node := 1;
  Result := 0;
end;

procedure MTProcedure(id__: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
 in__: pcuint8;
 inlen__: cuint64;
 S: Pblake2sp_state absolute Data;
begin
  in__ := S^.inp;
  inlen__ := S^.inlen;
  in__ += id__ * BLAKE2S_BLOCKBYTES;

  while ( inlen__ >= BLAKE2S_PARALLELISM_DEGREE * BLAKE2S_BLOCKBYTES ) do
  begin
    blake2s_update( @S^.S[id__], in__, BLAKE2S_BLOCKBYTES );
    in__ += BLAKE2S_PARALLELISM_DEGREE * BLAKE2S_BLOCKBYTES;
    inlen__ -= BLAKE2S_PARALLELISM_DEGREE * BLAKE2S_BLOCKBYTES;
  end;
end;

function blake2sp_update( S: Pblake2sp_state; inp: pcuint8; inlen: cuint64 ): cint;
var
  i, left, fill: csize_t;
begin
  left := S^.buflen;
  fill := sizeof( S^.buf ) - left;

  if ( left <> 0) and (inlen >= fill ) then
  begin
    Move(inp^, S^.buf[left], fill);

    for i := 0 to BLAKE2S_PARALLELISM_DEGREE - 1 do
      blake2s_update( @S^.S[i], @S^.buf[ i * BLAKE2S_BLOCKBYTES], BLAKE2S_BLOCKBYTES );

    inp += fill;
    inlen -= fill;
    left := 0;
  end;

  S^.inp := inp;
  S^.inlen := inlen;

{$IF DEFINED(USE_MTPROCS)}
  ProcThreadPool.DoParallel(@MTProcedure, 0, BLAKE2S_PARALLELISM_DEGREE - 1, S);
{$ELSE}
  for i := 0 to PARALLELISM_DEGREE - 1 do MTProcedure(i, S, nil);
{$ENDIF}

  inp += inlen - inlen mod ( BLAKE2S_PARALLELISM_DEGREE * BLAKE2S_BLOCKBYTES );
  inlen := inlen mod (BLAKE2S_PARALLELISM_DEGREE * BLAKE2S_BLOCKBYTES);

  if ( inlen > 0 ) then
    Move(inp^, S^.buf[left], inlen );

  S^.buflen := left + inlen;
  Result := 0;
end;

function blake2sp_final( S: Pblake2sp_state; outp: pcuint8; const outlen: cuint8 ): cint;
var
  i, left: csize_t;
  hash: array[0..Pred(BLAKE2S_PARALLELISM_DEGREE), 0..Pred(BLAKE2S_OUTBYTES)] of cuint8;
begin

  for i := 0 to BLAKE2S_PARALLELISM_DEGREE - 1 do
  begin
    if ( S^.buflen > i * BLAKE2S_BLOCKBYTES ) then
    begin
      left := S^.buflen - i * BLAKE2S_BLOCKBYTES;

      if ( left > BLAKE2S_BLOCKBYTES ) then left := BLAKE2S_BLOCKBYTES;

      blake2s_update( @S^.S[i], @S^.buf[i * BLAKE2S_BLOCKBYTES], left );
    end;

    blake2s_final( @S^.S[i], hash[i], BLAKE2S_OUTBYTES );
  end;

  for i := 0 to BLAKE2S_PARALLELISM_DEGREE - 1 do
    blake2s_update( @S^.R, hash[i], BLAKE2S_OUTBYTES );

  blake2s_final( @S^.R, outp, outlen );
  Result := 0;
end;

procedure blake2b_set_lastnode( S: Pblake2b_state ); inline;
begin
  S^.f[1] := cuint64(-1);
end;

//* Some helper functions, not necessarily useful */
function blake2b_is_lastblock( S: Pblake2b_state ): cint; inline;
begin
  Result := cint(S^.f[0] <> 0);
end;

procedure blake2b_set_lastblock( S: Pblake2b_state );
begin
  if( S^.last_node <> 0 ) then blake2b_set_lastnode( S );

  S^.f[0] := cuint64(-1);
end;

procedure blake2b_increment_counter( S: Pblake2b_state; const inc: cuint64 );
begin
  S^.t[0] += inc;
  S^.t[1] += cuint64( S^.t[0] < inc );
end;

procedure blake2b_init0( S: Pblake2b_state );
var
  i: csize_t;
begin
  fillchar( S^, sizeof( blake2b_state ), 0 );

  for i := 0 to 7 do S^.h[i] := cuint64(blake2b_IV[i]);
end;

//* init xors IV with input parameter block */
function blake2b_init_param( S: Pblake2b_state; const P: Pblake2b_param ): cint;
var
  i: csize_t;
  pp: pcuint8;
begin
  pp := pcuint8( P );

  blake2b_init0( S );

  //* IV XOR ParamBlock */
  for i := 0 to 7 do
    S^.h[i] := S^.h[i] xor load64( pp + sizeof( S^.h[i] ) * i );

  S^.outlen := P^.digest_length;
  Result := 0;
end;

function blake2b_init( S: Pblake2b_state; outlen: csize_t ): cint;
var
  P: blake2b_param;
begin
  if ( ( outlen = 0 ) or ( outlen > BLAKE2B_OUTBYTES ) ) then Exit(-1);

  P.digest_length := cuint8(outlen);
  P.key_length    := 0;
  P.fanout        := 1;
  P.depth         := 1;
  store32( @P.leaf_length, 0 );
  store32( @P.node_offset, 0 );
  store32( @P.xof_length, 0 );
  P.node_depth    := 0;
  P.inner_length  := 0;
  fillchar( P.reserved, sizeof( P.reserved ), 0 );
  fillchar( P.salt,     sizeof( P.salt ), 0 );
  fillchar( P.personal, sizeof( P.personal ), 0 );
  Result := blake2b_init_param( S, @P );
end;

function blake2b_update( S: Pblake2b_state; pin: pcuint8; inlen: csize_t ): cint;
var
  left, fill: csize_t;
begin
  if ( inlen > 0 ) then
  begin
    left := S^.buflen;
    fill := BLAKE2B_BLOCKBYTES - left;
    if ( inlen > fill ) then
    begin
      S^.buflen := 0;
      Move( pin^, S^.buf[left], fill ); //* Fill buffer */
      blake2b_increment_counter( S, BLAKE2B_BLOCKBYTES );
      blake2b_compress( S, S^.buf ); //* Compress */
      pin += fill; inlen -= fill;
      while (inlen > BLAKE2B_BLOCKBYTES) do
      begin
        blake2b_increment_counter(S, BLAKE2B_BLOCKBYTES);
        blake2b_compress( S, pin );
        pin += BLAKE2B_BLOCKBYTES;
        inlen -= BLAKE2B_BLOCKBYTES;
      end
    end;
    Move( pin^, S^.buf[S^.buflen], inlen );
    S^.buflen += inlen;
  end;
  Result := 0;
end;

function blake2b_final( S: Pblake2b_state; pout: pcuint8; outlen: csize_t ): cint;
var
  i: csize_t;
  buffer: array[0..Pred(BLAKE2B_OUTBYTES)] of cuint8;
begin
  if( pout = nil) or (outlen < S^.outlen ) then
    Exit(-1);

  if ( blake2b_is_lastblock( S ) <> 0 ) then
    Exit(-1);

  fillchar(buffer[0], BLAKE2B_OUTBYTES, 0);
  blake2b_increment_counter( S, S^.buflen );
  blake2b_set_lastblock( S );
  fillchar( S^.buf[S^.buflen], BLAKE2B_BLOCKBYTES - S^.buflen, 0 ); //* Padding */
  blake2b_compress( S, S^.buf );

  for i := 0 to 7 do //* Output full hash to temp buffer */
    store64( @buffer[sizeof( S^.h[i] ) * i], S^.h[i] );

  move( buffer[0], pout^, S^.outlen );
  fillchar(buffer[0], sizeof(buffer), 0);
  Result := 0;
end;

function blake2bp_init_leaf_param( S: Pblake2b_state; const P: Pblake2b_param ): cint;
begin
  Result:= blake2b_init_param(S, P);
  S^.outlen := P^.inner_length;
end;

function blake2bp_init_leaf( S: Pblake2b_state; outlen, keylen: csize_t; offset: cuint64 ): cint;
var
  P: blake2b_param;
begin
  P.digest_length := cuint8(outlen);
  P.key_length := cuint8(keylen);
  P.fanout := BLAKE2B_PARALLELISM_DEGREE;
  P.depth := 2;
  store32( @P.leaf_length, 0 );
  store32( @P.node_offset, offset );
  store32( @P.xof_length, 0 );
  P.node_depth := 0;
  P.inner_length := BLAKE2B_OUTBYTES;
  FillChar( P.reserved[0], sizeof( P.reserved ), 0 );
  FillChar( P.salt[0], sizeof( P.salt ), 0 );
  FillChar( P.personal[0], sizeof( P.personal ), 0 );
  Result:= blake2bp_init_leaf_param( S, @P );
end;

function blake2bp_init_root( S: Pblake2b_state; outlen, keylen: csize_t ): cint;
var
  P: blake2b_param;
begin
  P.digest_length := cuint8(outlen);
  P.key_length := cuint8(keylen);
  P.fanout := BLAKE2B_PARALLELISM_DEGREE;
  P.depth := 2;
  store32( @P.leaf_length, 0 );
  store32( @P.node_offset, 0 );
  store32( @P.xof_length, 0 );
  P.node_depth := 1;
  P.inner_length := BLAKE2B_OUTBYTES;
  FillChar( P.reserved[0], sizeof( P.reserved ), 0 );
  FillChar( P.salt[0], sizeof( P.salt ), 0 );
  FillChar( P.personal[0], sizeof( P.personal ), 0 );
  Result:= blake2b_init_param( S, @P );
end;

function blake2bp_init( S: Pblake2bp_state; outlen: csize_t ): cint;
var
  i: csize_t;
begin
  if (outlen = 0) or (outlen > BLAKE2B_OUTBYTES) then Exit(-1);

  FillChar( S^.buf[0], sizeof( S^.buf ), 0 );
  S^.buflen := 0;
  S^.outlen := outlen;

  if( blake2bp_init_root( @S^.R, outlen, 0 ) < 0 ) then
    Exit(-1);

  for i := 0 to BLAKE2B_PARALLELISM_DEGREE - 1 do
    if ( blake2bp_init_leaf( @S^.S[i], outlen, 0, i ) < 0 ) then Exit(-1);

  S^.R.last_node := 1;
  S^.S[BLAKE2B_PARALLELISM_DEGREE - 1].last_node := 1;
  Result:= 0;
end;

procedure blake2bp_MTProcedure(i: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
 in__: pcuint8;
 inlen__: cuint64;
 S: Pblake2bp_state absolute Data;
begin
  in__ := S^.inp;
  inlen__ := S^.inlen;
  in__ += i * BLAKE2B_BLOCKBYTES;

  while ( inlen__ >= BLAKE2B_PARALLELISM_DEGREE * BLAKE2B_BLOCKBYTES ) do
  begin
    blake2b_update( @S^.S[i], in__, BLAKE2B_BLOCKBYTES );
    in__ += BLAKE2B_PARALLELISM_DEGREE * BLAKE2B_BLOCKBYTES;
    inlen__ -= BLAKE2B_PARALLELISM_DEGREE * BLAKE2B_BLOCKBYTES;
  end;
end;

function blake2bp_update( S: Pblake2bp_state; inp: pcuint8; inlen: csize_t ): cint;
var
  left, fill, i: csize_t;
begin
  left := S^.buflen;
  fill := sizeof( S^.buf ) - left;

  if( left > 0) and (inlen >= fill ) then
  begin
    Move( inp^, S^.buf[left], fill );

    for i := 0 to BLAKE2B_PARALLELISM_DEGREE - 1 do
      blake2b_update( @S^.S[i], @S^.buf[i * BLAKE2B_BLOCKBYTES], BLAKE2B_BLOCKBYTES );

    inp += fill;
    inlen -= fill;
    left := 0;
  end;

  S^.inp := inp;
  S^.inlen := inlen;

{$IF DEFINED(USE_MTPROCS)}
  ProcThreadPool.DoParallel(@blake2bp_MTProcedure, 0, BLAKE2B_PARALLELISM_DEGREE - 1, S);
{$ELSE}
  for i := 0 to BLAKE2B_PARALLELISM_DEGREE - 1 do blake2bp_MTProcedure(i, S, nil);
{$ENDIF}

  inp += inlen - inlen mod ( BLAKE2B_PARALLELISM_DEGREE * BLAKE2B_BLOCKBYTES );
  inlen := inlen mod (BLAKE2B_PARALLELISM_DEGREE * BLAKE2B_BLOCKBYTES);

  if ( inlen > 0 ) then
    Move( inp^, S^.buf[left], inlen );

  S^.buflen := left + inlen;
  Result:= 0;
end;

function blake2bp_final( S: Pblake2bp_state; out_: PByte; outlen: csize_t ): cint;
var
  i, left: csize_t;
  hash: array[0..Pred(BLAKE2B_PARALLELISM_DEGREE), 0..Pred(BLAKE2B_OUTBYTES)] of cuint8;
begin
  if (out_ = nil) or (outlen < S^.outlen) then Exit(-1);

  for i := 0 to BLAKE2B_PARALLELISM_DEGREE - 1 do
  begin
    if ( S^.buflen > i * BLAKE2B_BLOCKBYTES ) then
    begin
      left := S^.buflen - i * BLAKE2B_BLOCKBYTES;

      if ( left > BLAKE2B_BLOCKBYTES ) then left := BLAKE2B_BLOCKBYTES;

      blake2b_update( @S^.S[i], @S^.buf[i * BLAKE2B_BLOCKBYTES], left );
    end;

    blake2b_final( @S^.S[i], hash[i], BLAKE2B_OUTBYTES );
  end;

  for i := 0 to BLAKE2B_PARALLELISM_DEGREE -1 do
    blake2b_update( @S^.R, hash[i], BLAKE2B_OUTBYTES );

  Result:= blake2b_final( @S^.R, out_, S^.outlen );
end;

end.
