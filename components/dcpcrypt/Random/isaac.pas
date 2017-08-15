unit ISAAC;

{Bob Jenkins' public domain random number generator ISAAC}

interface

{$i std.inc}

(*************************************************************************

 DESCRIPTION     :  Bob Jenkins' public domain random number generator ISAAC
                    (Indirection, Shift, Accumulate, Add, and Count)
                    Period at least 2^40, average 2^8295

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://burtleburtle.net/bob/rand/isaacafa.html
                    (ISAAC: a fast cryptographic random number generator)

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     23.07.05  W.Ehrhardt  Initial BP7 port of rand.c with RANDSIZ=256
 0.11     23.07.05  we          Some tweaking in isaac_generate
 0.12     23.07.05  we          non crypt option (RANDSIZ=16), much slower
 0.13     23.07.05  we          use RANDSIZ=256 only, procedure Mix
 0.14     23.07.05  we          use mix array m, use inc where possible
 0.15     23.07.05  we          BASM16 in isaac_generate
 0.16     24.07.05  we          routines for word, long, double etc.
 0.17     01.09.05  we          byte typecast in init0
 0.18     05.11.08  we          isaac_dword function
 0.19     02.12.08  we          BTypes/Ptr2Inc
 0.20     06.01.09  we          Uses BTypes moved to implementation
 0.21     14.06.12  we          Fix bug in _read for trailing max 3 bytes
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2005-2012 Wolfgang Ehrhardt

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
----------------------------------------------------------------------------*)



{$ifdef BIT16}
  {$N+}
{$endif}

type
  isaac_ctx = record                               {context of random number generator}
                randmem: array[0..255] of longint; {the internal state}
                randrsl: array[0..255] of longint; {the results given to the user}
                randa  : longint;                  {accumulator}
                randb  : longint;                  {the last result}
                randc  : longint;                  {counter, guarantees cycle >= 2^40 }
                nextres: longint;                  {the next result }
                randidx: word;                     {the index in randrsl[] }
              end;


procedure isaac_init (var ctx: isaac_ctx; seed: longint);
  {-Init context from randrsl[0]=seed, randrsl[i]=0 otherwise}

procedure isaac_init0(var ctx: isaac_ctx);
  {-Init context from randseed}

{$ifdef CONST}
procedure isaac_inita(var ctx: isaac_ctx; const key: array of longint; klen: integer);
  {-Init all context variables with separate seeds, klen: number of seeds}
{$else}
procedure isaac_inita(var ctx: isaac_ctx; var KArr; klen: integer);
  {-Init all context variables with separate seeds, klen: number of seeds}
{$endif}

procedure isaac_next(var ctx: isaac_ctx);
  {-Next step of PRNG}

procedure isaac_read(var ctx: isaac_ctx; dest: pointer; len: longint);
  {-Read len bytes from the PRNG to dest}

function  isaac_long(var ctx: isaac_ctx): longint;
  {-Next random positive longint}

function  isaac_dword(var ctx: isaac_ctx): {$ifdef HAS_CARD32}cardinal{$else}longint{$endif};
  {-Next 32 bit random dword (cardinal or longint)}

function  isaac_word(var ctx: isaac_ctx): word;
  {-Next random word}

function  isaac_double(var ctx: isaac_ctx): double;
  {-Next random double [0..1) with 32 bit precision}

function  isaac_double53(var ctx: isaac_ctx): double;
  {-Next random double in [0..1) with full double 53 bit precision}

function  isaac_selftest: boolean;
  {-Simple self-test of ISAAC PRNG}


{$ifdef testing}
{interfaced for cycle testing without overhead, do not use for normal use}
procedure isaac_generate(var ctx: isaac_ctx);
  {-generate next 256 result values, ie refill randrsl}
{$endif}


implementation

uses
  BTypes;

{---------------------------------------------------------------------------}
procedure isaac_generate(var ctx: isaac_ctx);
  {-generate next 256 result values, ie refill randrsl}
var
  x,y: longint;
  xi : integer absolute x;  {better performance for BIT16}
  i  : integer;
 {$ifdef BASM16}
   pra: pointer;            {pointer to cxt.randa for faster BASM16 access}
 {$endif}
begin
  {$ifdef BASM16}
    pra := @ctx.randa;
  {$endif}
  with ctx do begin
    inc(randc);
    inc(randb, randc);
    for i:=0 to 255 do begin
      {$ifdef BASM16}
        case i and 3 of
          0: asm
               les di,[pra]
               db $66; mov ax,es:[di]
               db $66; shl ax,13
               db $66; xor es:[di],ax
             end;
          1: asm
               les di,[pra]
               db $66; mov ax,es:[di]
               db $66; shr ax,6
               db $66; xor es:[di],ax
             end;
          2: asm
               les di,[pra]
               db $66; mov ax,es:[di]
               db $66; shl ax,2
               db $66; xor es:[di],ax
             end;
          3: asm
               {shr 16 is special, use word [pra+2]}
               les di,[pra]
               mov ax, es:[di+2]
               xor es:[di],ax
             end;
        end;
      {$else}
        case i and 3 of
          0: randa := randa xor (randa shl 13);
          1: randa := randa xor (randa shr  6);
          2: randa := randa xor (randa shl  2);
          3: randa := randa xor (randa shr 16);
        end;
      {$endif}
       x := randmem[i];
       inc(randa,randmem[(i+128) and 255]);
       y := randmem[(xi shr 2) and 255] + randa + randb;
       randmem[i] := y;
       randb := randmem[(y shr 10) and 255] + x;
       randrsl[i] := randb;
    end;
    {reset result index}
    randidx:=0;
  end;
end;


{---------------------------------------------------------------------------}
procedure internal_init(var ctx: isaac_ctx; flag: boolean);
  {-Init state, use randrsl if flag=true}
var
  i,j: integer;
  m: array[0..7] of longint;

  procedure Mix;
    {-mix the array}
  begin
    m[0] := m[0] xor (m[1] shl 11); inc(m[3], m[0]); inc(m[1], m[2]);
    m[1] := m[1] xor (m[2] shr  2); inc(m[4], m[1]); inc(m[2], m[3]);
    m[2] := m[2] xor (m[3] shl  8); inc(m[5], m[2]); inc(m[3], m[4]);
    m[3] := m[3] xor (m[4] shr 16); inc(m[6], m[3]); inc(m[4], m[5]);
    m[4] := m[4] xor (m[5] shl 10); inc(m[7], m[4]); inc(m[5], m[6]);
    m[5] := m[5] xor (m[6] shr  4); inc(m[0], m[5]); inc(m[6], m[7]);
    m[6] := m[6] xor (m[7] shl  8); inc(m[1], m[6]); inc(m[7], m[0]);
    m[7] := m[7] xor (m[0] shr  9); inc(m[2], m[7]); inc(m[0], m[1]);
  end;

begin
  with ctx do begin
    randa := 0;
    randb := 0;
    randc := 0;

    for i:=0 to 7 do m[i] := longint($9e3779b9); {the golden ratio}
    for i:=0 to 3 do Mix;

    i := 0;
    while i<256 do begin
      {fill in randmem[] with messy stuff}
      if flag then begin
        {use all the information in the seed}
        for j:=0 to 7 do inc(m[j], randrsl[i+j]);
      end;
      Mix;
      move(m, randmem[i], sizeof(m));
      inc(i,8);
    end;

    if flag then begin
      {do a second pass to make all of the seed affect all of randmem}
      i := 0;
      while i<256 do begin
       for j:=0 to 7 do inc(m[j], randmem[i+j]);
       Mix;
       move(m, randmem[i], sizeof(m));
       inc(i,8);
      end;
    end;

    {generate first set of results}
    isaac_generate(ctx);
    {prepare to use the first set of results }
    randidx := 0;
  end;
end;


{---------------------------------------------------------------------------}
procedure isaac_init(var ctx: isaac_ctx; seed: longint);
  {-Init context from randrsl[0]=seed, randrsl[i]=0 otherwise}
begin
  with ctx do begin
    fillchar(randrsl, sizeof(randrsl),0);
    randrsl[0] := seed;
  end;
  internal_init(ctx, true);
end;


{---------------------------------------------------------------------------}
procedure isaac_init0(var ctx: isaac_ctx);
  {-Init context from randseed and randrsl[i]:=random}
var
  i,j: integer;
  tl: longint;
  ta: packed array[0..3] of byte absolute tl;
begin
  with ctx do begin
    for i:=0 to 255 do begin
      for j:=0 to 3 do ta[j] := byte(random(256));
      randrsl[i] := tl;
    end;
  end;
  internal_init(ctx, true);
end;


{---------------------------------------------------------------------------}
{$ifdef CONST}
procedure isaac_inita(var ctx: isaac_ctx; const key: array of longint; klen: integer);
  {-Init all context variables with separate seeds, klen: number of seeds}
{$else}
procedure isaac_inita(var ctx: isaac_ctx; var KArr; klen: integer);
  {-Init all context variables with separate seeds, klen: number of seeds}
var
  key: packed array[0..255] of longint absolute KArr; {T5-6 do not have open arrrays}
{$endif}
var
  i: integer;
begin
  {$ifdef CONST}
    if klen>high(key)+1 then klen := high(key)+1;
  {$endif}
  with ctx do begin
    for i:=0 to 255 do begin
      if i<klen then randrsl[i]:=key[i] else randrsl[i]:=0;
    end;
  end;
  internal_init(ctx, true);
end;


{---------------------------------------------------------------------------}
procedure isaac_next(var ctx: isaac_ctx);
  {-Next step of PRNG}
begin
  with ctx do begin
    if randidx>255 then isaac_generate(ctx);
    nextres := randrsl[randidx];
    inc(randidx);
  end;
end;


{---------------------------------------------------------------------------}
procedure isaac_read(var ctx: isaac_ctx; dest: pointer; len: longint);
  {-Read len bytes from the PRNG to dest}
type
  plong = ^longint;
begin
  {not optimized}
  while len>3 do begin
    isaac_next(ctx);
    plong(dest)^ := ctx.nextres;
    inc(Ptr2Inc(dest),4);
    dec(len, 4);
  end;
  if len>0 then begin
    isaac_next(ctx);
    move(ctx.nextres, dest^, len and 3);
  end;
end;


{---------------------------------------------------------------------------}
function isaac_long(var ctx: isaac_ctx): longint;
  {-Next random positive longint}
begin
  isaac_next(ctx);
  isaac_long := ctx.nextres shr 1;
end;


{---------------------------------------------------------------------------}
function isaac_dword(var ctx: isaac_ctx): {$ifdef HAS_CARD32}cardinal{$else}longint{$endif};
  {-Next 32 bit random dword (cardinal or longint)}
begin
  isaac_next(ctx);
  {$ifdef HAS_CARD32}
    isaac_dword := cardinal(ctx.nextres);
  {$else}
    isaac_dword := ctx.nextres;
  {$endif}
end;


{---------------------------------------------------------------------------}
function isaac_word(var ctx: isaac_ctx): word;
  {-Next random word}
type
  TwoWords = packed record
               L,H: word
             end;
begin
  isaac_next(ctx);
  isaac_word := TwoWords(ctx.nextres).H;
end;


{---------------------------------------------------------------------------}
function isaac_double(var ctx: isaac_ctx): double;
  {-Next random double [0..1) with 32 bit precision}
begin
  isaac_next(ctx);
  isaac_double := (ctx.nextres + 2147483648.0) / 4294967296.0;
end;


{---------------------------------------------------------------------------}
function isaac_double53(var ctx: isaac_ctx): double;
  {-Next random double in [0..1) with full double 53 bit precision}
var
  hb,lb: longint;
begin
  isaac_next(ctx);
  hb := ctx.nextres shr 5;
  isaac_next(ctx);
  lb := ctx.nextres shr 6;
  isaac_double53 := (hb*67108864.0+lb)/9007199254740992.0;
end;


{---------------------------------------------------------------------------}
function isaac_selftest: boolean;
  {-Simple self-test of ISAAC PRNG}
var
  ctx: isaac_ctx;
begin
  fillchar(ctx, sizeof(ctx),0);
  internal_init(ctx, true);
  isaac_generate(ctx);
  {check first and last longint of randvec.txt}
  if ctx.randrsl[0]<>longint($f650e4c8) then begin
    isaac_selftest := false;
    exit;
  end;
  isaac_generate(ctx);
  isaac_selftest := ctx.randrsl[255] = longint($4bb5af29);
end;

end.
