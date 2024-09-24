{
 * xxHash - Extremely Fast Hash algorithm
 * Copyright (C) 2012-2023 Yann Collet
 *
 * The Pascal translation by Alexander Koblov, 2024
 *
 * BSD 2-Clause License (https://www.opensource.org/licenses/bsd-license.php)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following disclaimer
 *      in the documentation and/or other materials provided with the
 *      distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * You can contact the author at:
 *   - xxHash homepage: https://www.xxhash.com
 *   - xxHash source repository: https://github.com/Cyan4973/xxHash
}

unit DCxxhash;

{$mode objfpc}{$H+}
{$inline on}{$Q-}
{$macro on}{$R-}

interface

uses
  SysUtils;

const
  XXH3_SECRET_DEFAULT_SIZE = 192;
  XXH3_INTERNALBUFFER_SIZE = 256;

type
  XXH64_hash_t = UInt64;
  XXH32_hash_t = UInt32;

  XXH128_hash_t = record
    low64: XXH64_hash_t;
    high64: XXH64_hash_t;
  end;

  {$CODEALIGN RECORDMIN=64}
  PXXH3_state_t = ^XXH3_state_t;
  XXH3_state_t = record
    acc: array[0..7] of XXH64_hash_t;
    customSecret: array[0..Pred(XXH3_SECRET_DEFAULT_SIZE)] of Byte;
    buffer: array[0..Pred(XXH3_INTERNALBUFFER_SIZE)] of Byte;
    bufferedSize: XXH32_hash_t;
    useSeed: XXH32_hash_t;
    nbStripesSoFar: UIntPtr;
    totalLen: XXH64_hash_t;
    nbStripesPerBlock: UIntPtr;
    secretLimit: UIntPtr;
    seed: XXH64_hash_t;
    reserved64: XXH64_hash_t;
    extSecret: PByte;
  end;

function XXH3_createState: PXXH3_state_t;
procedure XXH3_freeState(statePtr: PXXH3_state_t);
procedure XXH3_128bits_reset(statePtr: PXXH3_state_t);
procedure XXH3_128bits_update(state: PXXH3_state_t; const input: PByte; len: UIntPtr);
function XXH3_128bits_digest (const state: PXXH3_state_t): XXH128_hash_t;

implementation

{$IF DEFINED(CPUX86_64)}
uses
  CPU;
{$ENDIF}

{$CODEALIGN CONSTMIN=64}

const
  XXH_PRIME32_1 = $9E3779B1;
  XXH_PRIME32_2 = $85EBCA77;
  XXH_PRIME32_3 = $C2B2AE3D;

  XXH_PRIME64_1 = UInt64($9E3779B185EBCA87);
  XXH_PRIME64_2 = UInt64($C2B2AE3D27D4EB4F);
  XXH_PRIME64_3 = UInt64($165667B19E3779F9);
  XXH_PRIME64_4 = UInt64($85EBCA77C2B2AE63);
  XXH_PRIME64_5 = UInt64($27D4EB2F165667C5);

  XXH3_MIDSIZE_MAX = 240;
  XXH_SECRET_LASTACC_START = 7;
  XXH_SECRET_MERGEACCS_START = 11;

  XXH3_MIDSIZE_STARTOFFSET = 3;
  XXH3_MIDSIZE_LASTOFFSET  = 17;

  XXH_SECRET_CONSUME_RATE = 8;
  XXH_STRIPE_LEN = 64;
  XXH_ACC_SIZE = 64;

  XXH3_SECRET_SIZE_MIN = 136;
  XXH_SECRET_DEFAULT_SIZE = 192;

  PRIME_MX1 = UInt64($165667919E3779F9);
  PRIME_MX2 = UInt64($9FB21C651E98DF25);

  XXH_ACC_ALIGN = 64; //* for compatibility with avx512 */

  XXH3_INTERNALBUFFER_STRIPES = (XXH3_INTERNALBUFFER_SIZE div XXH_STRIPE_LEN);

  //*! Pseudorandom secret taken directly from FARSH. */
  const XXH3_kSecret: array[0..Pred(XXH_SECRET_DEFAULT_SIZE)] of Byte = (
      $b8, $fe, $6c, $39, $23, $a4, $4b, $be, $7c, $01, $81, $2c, $f7, $21, $ad, $1c,
      $de, $d4, $6d, $e9, $83, $90, $97, $db, $72, $40, $a4, $a4, $b7, $b3, $67, $1f,
      $cb, $79, $e6, $4e, $cc, $c0, $e5, $78, $82, $5a, $d0, $7d, $cc, $ff, $72, $21,
      $b8, $08, $46, $74, $f7, $43, $24, $8e, $e0, $35, $90, $e6, $81, $3a, $26, $4c,
      $3c, $28, $52, $bb, $91, $c3, $00, $cb, $88, $d0, $65, $8b, $1b, $53, $2e, $a3,
      $71, $64, $48, $97, $a2, $0d, $f9, $4e, $38, $19, $ef, $46, $a9, $de, $ac, $d8,
      $a8, $fa, $76, $3f, $e3, $9c, $34, $3f, $f9, $dc, $bb, $c7, $c7, $0b, $4f, $1d,
      $8a, $51, $e0, $4b, $cd, $b4, $59, $31, $c8, $9f, $7e, $c9, $d9, $78, $73, $64,
      $ea, $c5, $ac, $83, $34, $d3, $eb, $c3, $c5, $81, $a0, $ff, $fa, $13, $63, $eb,
      $17, $0d, $dd, $51, $b7, $f0, $da, $49, $d3, $16, $55, $26, $29, $d4, $68, $9e,
      $2b, $16, $be, $58, $7d, $47, $a1, $fc, $8f, $f8, $b8, $d1, $7a, $d0, $31, $ce,
      $45, $cb, $3a, $8f, $95, $16, $04, $28, $af, $d7, $fb, $ca, $bb, $4b, $40, $7e
  );

type
  TXXH3_scrambleAcc_f = procedure(acc: PByte; const secret: PByte);
  TXXH3_accumulate_512_f = procedure(acc: PByte; const input: PByte; const secret: PByte);
  TXXH3_accumulate_f = procedure(acc: PByte; const input: PByte; const secret: PByte; nbStripes: UIntPtr);

var
  XXH3_accumulate: TXXH3_accumulate_f;
  XXH3_scrambleAcc: TXXH3_scrambleAcc_f;
  XXH3_accumulate_512: TXXH3_accumulate_512_f;

function XXH_readLE32(const ptr: Pointer): UInt32; inline;
begin
  Result:= PUInt32(ptr)^;
end;

function XXH_readLE64(const ptr: Pointer): UInt64; inline;
begin
  Result:= PUInt64(ptr)^;
end;

function XXH_mult32to64(x, y: UInt64): UInt64; inline;
begin
  Result:= (x and $FFFFFFFF) * (y and $FFFFFFFF);
end;

function XXH_xorshift64(v64: UInt64; shift: Integer): UInt64; inline;
begin
  // XXH_ASSERT(0 <= shift && shift < 64);
  Result:=  v64 xor (v64 shr shift);
end;

function XXH64_avalanche(hash: UInt64): UInt64;
begin
  hash := hash xor hash shr 33;
  hash *= XXH_PRIME64_2;
  hash := hash xor hash shr 29;
  hash *= XXH_PRIME64_3;
  hash := hash xor hash shr 32;
  Result := hash;
end;

function XXH_alignedMalloc(s: UIntPtr; align: UIntPtr): Pointer;
var
  offset: UIntPtr;
  base, ptr: PByte;
begin
  Assert((align <= 128) and (align >= 8)); //* range check */
  Assert((align and (align-1)) = 0);       //* power of 2 */
  Assert((s <> 0) and (s < (s + align)));  //* empty/overflow */
  //* Overallocate to make room for manual realignment and an offset byte */
  base := GetMem(s + align);
  if (base <> nil) then
  begin
    {*
     * Get the offset needed to align this pointer.
     *
     * Even if the returned pointer is aligned, there will always be
     * at least one byte to store the offset to the original pointer.
    *}
    offset := align - (UIntPtr(base) and (align - 1)); //* base % align */
    //* Add the offset for the now-aligned pointer */
    ptr := base + offset;

    Assert(UIntPtr(ptr) mod align = 0);

    //* Store the offset immediately before the returned pointer. */
    ptr[-1] := Byte(offset);
    Exit(ptr);
  end;
  Result:= nil;
end;

procedure XXH_alignedFree(p: Pointer);
var
  offset: Byte;
  base, ptr: PByte;
begin
  if (p <> nil) then
  begin
    ptr:= PByte(p);
    //* Get the offset byte we added in XXH_malloc. */
    offset:= ptr[-1];
    //* Free the original malloc'd pointer */
    base:= ptr - offset;
    FreeMem(base);
  end;
end;

function XXH3_createState: PXXH3_state_t;
begin
  Result:= XXH_alignedMalloc(SizeOf(XXH3_state_t), XXH_ACC_ALIGN);
  if (Result = nil) then Exit(nil);
  Result^.seed:= 0;
  Result^.extSecret:= nil;
end;

procedure XXH3_freeState(statePtr: PXXH3_state_t);
begin
  XXH_alignedFree(statePtr);
end;

procedure XXH3_reset_internal(statePtr: PXXH3_state_t; seed: XXH64_hash_t;
                              const secret: PByte; secretSize: UIntPtr);
var
  initStart: PByte;
  initLength: UIntPtr;
begin
  Assert(statePtr <> nil);
  initStart:= @statePtr^.bufferedSize;
  initLength:= @statePtr^.nbStripesPerBlock - initStart;
  //* set members from bufferedSize to nbStripesPerBlock (excluded) to 0 */
  FillChar(initStart^, initLength, 0);
  statePtr^.acc[0]:= XXH_PRIME32_3;
  statePtr^.acc[1]:= XXH_PRIME64_1;
  statePtr^.acc[2]:= XXH_PRIME64_2;
  statePtr^.acc[3]:= XXH_PRIME64_3;
  statePtr^.acc[4]:= XXH_PRIME64_4;
  statePtr^.acc[5]:= XXH_PRIME32_2;
  statePtr^.acc[6]:= XXH_PRIME64_5;
  statePtr^.acc[7]:= XXH_PRIME32_1;
  statePtr^.seed:= seed;
  statePtr^.useSeed:= XXH32_hash_t(seed <> 0);
  statePtr^.extSecret:= secret;
  Assert(secretSize >= XXH3_SECRET_SIZE_MIN);
  statePtr^.secretLimit:= secretSize - XXH_STRIPE_LEN;
  statePtr^.nbStripesPerBlock:= statePtr^.secretLimit div XXH_SECRET_CONSUME_RATE;
end;

procedure XXH3_64bits_reset(statePtr: PXXH3_state_t);
begin
  XXH3_reset_internal(statePtr, 0, XXH3_kSecret, XXH_SECRET_DEFAULT_SIZE);
end;

procedure XXH3_128bits_reset(statePtr: PXXH3_state_t);
begin
  XXH3_64bits_reset(statePtr);
end;

{$IF DEFINED(CPUX86_64)}

const
  SSE_PRIME32_1: array[0..3] of UInt32 = (XXH_PRIME32_1, XXH_PRIME32_1, XXH_PRIME32_1, XXH_PRIME32_1);

procedure XXH3_accumulate_512_sse2(acc: PByte; const input: PByte; const secret: PByte); assembler; nostackframe;
// UNIX    RDI, RSI, RDX
// WIN64:  RCX, RDX, R8
asm
{$IF DEFINED(UNIX)}
  movq     %rdx, %r8
  movq     %rdi, %rcx
  movq     %rsi, %rdx
{$ENDIF}
  movdqu	(%rdx), %xmm3
  movdqu	(%r8), %xmm0
  movdqu	(%rdx), %xmm4
  movdqu	16(%rdx), %xmm5
  pxor	%xmm3, %xmm0
  movdqu	16(%rdx), %xmm2
  movdqu	32(%rdx), %xmm3
  pshufd	$49, %xmm0, %xmm1
  pmuludq	%xmm1, %xmm0
  pshufd	$78, %xmm4, %xmm1
  movdqu	32(%rdx), %xmm4
  paddq	%xmm1, %xmm0
  paddq	(%rcx), %xmm0
  movups	%xmm0, (%rcx)
  movdqu	16(%r8), %xmm0
  pxor	%xmm5, %xmm0
  pshufd	$49, %xmm0, %xmm1
  pmuludq	%xmm1, %xmm0
  pshufd	$78, %xmm2, %xmm1
  paddq	%xmm1, %xmm0
  paddq	16(%rcx), %xmm0
  movups	%xmm0, 16(%rcx)
  movdqu	32(%r8), %xmm0
  pxor	%xmm3, %xmm0
  pshufd	$49, %xmm0, %xmm1
  pmuludq	%xmm1, %xmm0
  pshufd	$78, %xmm4, %xmm1
  paddq	%xmm1, %xmm0
  paddq	32(%rcx), %xmm0
  movdqu	48(%rdx), %xmm1
  movups	%xmm0, 32(%rcx)
  movdqu	48(%r8), %xmm0
  pxor	%xmm1, %xmm0
  pshufd	$78, %xmm1, %xmm1
  pshufd	$49, %xmm0, %xmm2
  pmuludq	%xmm2, %xmm0
  paddq	%xmm1, %xmm0
  paddq	48(%rcx), %xmm0
  movups	%xmm0, 48(%rcx)
end;

procedure XXH3_accumulate_sse2(acc: PByte; const input: PByte; const secret: PByte; nbStripes: UIntPtr); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX
// WIN64:  RCX, RDX, R8,  R9
asm
{$IF DEFINED(UNIX)}
  movq     %rdx, %r8
  movq     %rcx, %r9
  movq     %rdi, %rcx
  movq     %rsi, %rdx
{$ENDIF}
  testq	%r9, %r9
  je	.L271
  leaq	448(%rdx), %rax
  prefetcht0	384(%rdx)
  movdqu	(%rcx), %xmm4
  movdqu	16(%rcx), %xmm3
  movdqu	32(%rcx), %xmm2
  movdqu	48(%rcx), %xmm1
  xorl	%edx, %edx
  jmp	.L276
.L274:
  prefetcht0	(%rax)
  addq	$64, %rax
.L276:
  movdqu	(%r8,%rdx,8), %xmm0
  movdqu	-448(%rax), %xmm5
  pxor	%xmm5, %xmm0
  pshufd	$49, %xmm0, %xmm5
  pmuludq	%xmm5, %xmm0
  movdqu	-448(%rax), %xmm5
  pshufd	$78, %xmm5, %xmm5
  paddq	%xmm5, %xmm0
  movdqu	-432(%rax), %xmm5
  paddq	%xmm0, %xmm4
  movdqu	16(%r8,%rdx,8), %xmm0
  pxor	%xmm5, %xmm0
  pshufd	$49, %xmm0, %xmm5
  pmuludq	%xmm5, %xmm0
  movdqu	-432(%rax), %xmm5
  pshufd	$78, %xmm5, %xmm5
  paddq	%xmm5, %xmm0
  movdqu	-416(%rax), %xmm5
  paddq	%xmm0, %xmm3
  movdqu	32(%r8,%rdx,8), %xmm0
  pxor	%xmm5, %xmm0
  pshufd	$49, %xmm0, %xmm5
  pmuludq	%xmm5, %xmm0
  movdqu	-416(%rax), %xmm5
  pshufd	$78, %xmm5, %xmm5
  paddq	%xmm5, %xmm0
  movdqu	-400(%rax), %xmm5
  paddq	%xmm0, %xmm2
  movdqu	48(%r8,%rdx,8), %xmm0
  addq	$1, %rdx
  pxor	%xmm5, %xmm0
  pshufd	$49, %xmm0, %xmm5
  pmuludq	%xmm5, %xmm0
  movdqu	-400(%rax), %xmm5
  pshufd	$78, %xmm5, %xmm5
  paddq	%xmm5, %xmm0
  paddq	%xmm0, %xmm1
  cmpq	%rdx, %r9
  jne	.L274
  movups	%xmm4, (%rcx)
  movups	%xmm3, 16(%rcx)
  movups	%xmm2, 32(%rcx)
  movups	%xmm1, 48(%rcx)
.L271:
  ret
end;

procedure XXH3_accumulate_512_avx2(acc: PByte; const input: PByte; const secret: PByte); assembler; nostackframe;
// UNIX    RDI, RSI, RDX
// WIN64:  RCX, RDX, R8
asm
{$IF DEFINED(UNIX)}
  movq     %rdx, %r8
  movq     %rdi, %rcx
  movq     %rsi, %rdx
{$ENDIF}
  vmovdqu	(%r8), %ymm3
  vpxor	(%rdx), %ymm3, %ymm0
  vpsrlq	$32, %ymm0, %ymm1
  vpmuludq	%ymm1, %ymm0, %ymm0
  vpshufd	$78, (%rdx), %ymm1
  vpaddq	%ymm1, %ymm0, %ymm0
  vpaddq	(%rcx), %ymm0, %ymm0
  vmovdqu	32(%rdx), %ymm1
  vmovdqu	%ymm0, (%rcx)
  vpxor	32(%r8), %ymm1, %ymm0
  vpshufd	$78, %ymm1, %ymm1
  vpsrlq	$32, %ymm0, %ymm2
  vpmuludq	%ymm2, %ymm0, %ymm0
  vpaddq	%ymm1, %ymm0, %ymm0
  vpaddq	32(%rcx), %ymm0, %ymm0
  vmovdqu	%ymm0, 32(%rcx)
  vzeroupper
end;

procedure XXH3_accumulate_avx2(acc: PByte; const input: PByte; const secret: PByte; nbStripes: UIntPtr); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX
// WIN64:  RCX, RDX, R8,  R9
asm
{$IF DEFINED(UNIX)}
  movq     %rdx, %r8
  movq     %rcx, %r9
  movq     %rdi, %rcx
  movq     %rsi, %rdx
{$ENDIF}
  testq	%r9, %r9
  je	.L290
  leaq	448(%rdx), %rax
  prefetcht0	384(%rdx)
  vmovdqu	(%rcx), %ymm3
  xorl	%edx, %edx
  vmovdqu	32(%rcx), %ymm2
  jmp	.L288
.L286:
  prefetcht0	(%rax)
  addq	$64, %rax
.L288:
  vmovdqu	(%r8,%rdx,8), %ymm4
  vpxor	-448(%rax), %ymm4, %ymm0
  vmovdqu	32(%r8,%rdx,8), %ymm5
  addq	$1, %rdx
  vpsrlq	$32, %ymm0, %ymm1
  vpmuludq	%ymm1, %ymm0, %ymm0
  vpshufd	$78, -448(%rax), %ymm1
  vpaddq	%ymm1, %ymm0, %ymm0
  vpaddq	%ymm3, %ymm0, %ymm3
  vpxor	-416(%rax), %ymm5, %ymm0
  vpsrlq	$32, %ymm0, %ymm1
  vpmuludq	%ymm1, %ymm0, %ymm0
  vpshufd	$78, -416(%rax), %ymm1
  vpaddq	%ymm1, %ymm0, %ymm0
  vpaddq	%ymm2, %ymm0, %ymm2
  cmpq	%rdx, %r9
  jne	.L286
  vmovdqu	%ymm3, (%rcx)
  vmovdqu	%ymm2, 32(%rcx)
  vzeroupper
.L290:
  ret
end;

procedure XXH3_scrambleAcc_sse2(acc: PByte; const secret: PByte); assembler; nostackframe;
// UNIX    RDI, RSI
// WIN64:  RCX, RDX
asm
{$IF DEFINED(UNIX)}
  movq     %rdi, %rcx
  movq     %rsi, %rdx
{$ENDIF}
  movdqu	(%rcx), %xmm1
  movdqu	(%rdx), %xmm0
  pxor	(%rcx), %xmm0
  psrlq	$47, %xmm1
  pxor	%xmm1, %xmm0
  movdqu	SSE_PRIME32_1(%rip), %xmm1
  pshufd	$49, %xmm0, %xmm2
  pmuludq	%xmm1, %xmm2
  pmuludq	%xmm1, %xmm0
  psllq	$32, %xmm2
  paddq	%xmm2, %xmm0
  movdqu	16(%rcx), %xmm2
  movups	%xmm0, (%rcx)
  movdqu	16(%rdx), %xmm0
  pxor	16(%rcx), %xmm0
  psrlq	$47, %xmm2
  pxor	%xmm2, %xmm0
  pshufd	$49, %xmm0, %xmm2
  pmuludq	%xmm1, %xmm0
  pmuludq	%xmm1, %xmm2
  psllq	$32, %xmm2
  paddq	%xmm2, %xmm0
  movdqu	32(%rcx), %xmm2
  movups	%xmm0, 16(%rcx)
  movdqu	32(%rdx), %xmm0
  pxor	32(%rcx), %xmm0
  psrlq	$47, %xmm2
  pxor	%xmm2, %xmm0
  pshufd	$49, %xmm0, %xmm2
  pmuludq	%xmm1, %xmm0
  pmuludq	%xmm1, %xmm2
  psllq	$32, %xmm2
  paddq	%xmm2, %xmm0
  movdqu	48(%rcx), %xmm2
  movups	%xmm0, 32(%rcx)
  movdqu	48(%rdx), %xmm0
  pxor	48(%rcx), %xmm0
  psrlq	$47, %xmm2
  pxor	%xmm2, %xmm0
  pshufd	$49, %xmm0, %xmm2
  pmuludq	%xmm1, %xmm0
  pmuludq	%xmm2, %xmm1
  psllq	$32, %xmm1
  paddq	%xmm1, %xmm0
  movups	%xmm0, 48(%rcx)
end;

procedure XXH3_scrambleAcc_avx2(acc: PByte; const secret: PByte); assembler; nostackframe;
// UNIX    RDI, RSI
// WIN64:  RCX, RDX
asm
{$IF DEFINED(UNIX)}
  movq     %rdi, %rcx
  movq     %rsi, %rdx
{$ENDIF}
  movl	$-1640531535, %eax
  vmovdqu	(%rcx), %ymm3
  vmovdqu	(%rdx), %ymm4
  vmovdqu	32(%rcx), %ymm5
  vpxor	%ymm3, %ymm4, %ymm0
  vpsrlq	$47, %ymm3, %ymm1
  vmovdqu	32(%rdx), %ymm3
  vpxor	%ymm1, %ymm0, %ymm0
  vmovd	%eax, %xmm1
  vpbroadcastd	%xmm1, %ymm1
  vpsrlq	$32, %ymm0, %ymm2
  vpmuludq	%ymm1, %ymm2, %ymm2
  vpmuludq	%ymm1, %ymm0, %ymm0
  vpsllq	$32, %ymm2, %ymm2
  vpaddq	%ymm2, %ymm0, %ymm0
  vpsrlq	$47, %ymm5, %ymm2
  vmovdqu	%ymm0, (%rcx)
  vpxor	%ymm5, %ymm3, %ymm0
  vpxor	%ymm2, %ymm0, %ymm0
  vpsrlq	$32, %ymm0, %ymm2
  vpmuludq	%ymm1, %ymm0, %ymm0
  vpmuludq	%ymm1, %ymm2, %ymm1
  vpsllq	$32, %ymm1, %ymm1
  vpaddq	%ymm1, %ymm0, %ymm0
  vmovdqu	%ymm0, 32(%rcx)
  vzeroupper
end;

{$ELSE}

const
  XXH_ACC_NB = 8;

function XXH_mult32to64_add64(lhs, rhs, acc: UInt64): UInt64; inline;
begin
  Result:= XXH_mult32to64(UInt32(lhs), UInt32(rhs)) + acc;
end;

procedure XXH3_scalarRound(acc: PByte; const input: PByte; const secret: PByte; lane: UIntPtr); inline;
var
  xinput, xsecret: PByte;
  data_val, data_key: UInt64;
  xacc: PUInt64 absolute acc;
begin
  xinput:= input;
  xsecret:= secret;
  Assert(lane < XXH_ACC_NB);
  // XXH_ASSERT(((size_t)acc & (XXH_ACC_ALIGN-1)) == 0);
  data_val:= XXH_readLE64(xinput + lane * 8);
  data_key:= data_val xor XXH_readLE64(xsecret + lane * 8);
  xacc[lane xor 1] += data_val; //* swap adjacent lanes */
  xacc[lane]:= XXH_mult32to64_add64(data_key, data_key shr 32, xacc[lane]);
end;

procedure XXH3_accumulate_512_scalar(acc: PByte; const input: PByte; const secret: PByte);
begin
  XXH3_scalarRound(acc, input, secret, 0);
  XXH3_scalarRound(acc, input, secret, 1);
  XXH3_scalarRound(acc, input, secret, 2);
  XXH3_scalarRound(acc, input, secret, 3);
  XXH3_scalarRound(acc, input, secret, 4);
  XXH3_scalarRound(acc, input, secret, 5);
  XXH3_scalarRound(acc, input, secret, 6);
  XXH3_scalarRound(acc, input, secret, 7);
end;

procedure XXH3_accumulate_scalar(acc: PByte; const input: PByte; const secret: PByte; nbStripes: UIntPtr);
var
  n: UIntPtr;
  in_: PByte;
begin
  for n:= 0 to nbStripes - 1 do
  begin
    in_:= input + n * XXH_STRIPE_LEN;
    XXH3_accumulate_512_scalar(acc, in_, secret + n * XXH_SECRET_CONSUME_RATE);
  end;
end;

procedure XXH3_scalarScrambleRound(acc: PByte; const secret: PByte; lane: UIntPtr); inline;
var
  acc64: UInt64;
  key64: UInt64;
  xacc: PUInt64;
  xsecret: PByte;
begin
  xacc:= PUInt64(acc); //* presumed aligned */
  xsecret:= secret;    //* no alignment restriction */

  // XXH_ASSERT((((size_t)acc) & (XXH_ACC_ALIGN-1)) == 0);
  Assert(lane < XXH_ACC_NB);

  key64:= XXH_readLE64(xsecret + lane * 8);
  acc64:= xacc[lane];
  acc64:= XXH_xorshift64(acc64, 47);
  acc64:= acc64 xor key64;
  acc64 *= XXH_PRIME32_1;
  xacc[lane]:= acc64;
end;

procedure XXH3_scrambleAcc_scalar(acc: PByte; const secret: PByte); inline;
begin
  XXH3_scalarScrambleRound(acc, secret, 0);
  XXH3_scalarScrambleRound(acc, secret, 1);
  XXH3_scalarScrambleRound(acc, secret, 2);
  XXH3_scalarScrambleRound(acc, secret, 3);
  XXH3_scalarScrambleRound(acc, secret, 4);
  XXH3_scalarScrambleRound(acc, secret, 5);
  XXH3_scalarScrambleRound(acc, secret, 6);
  XXH3_scalarScrambleRound(acc, secret, 7);
end;

{$ENDIF}

function XXH3_consumeStripes(acc: PByte; nbStripesSoFarPtr: PUIntPtr; nbStripesPerBlock: UIntPtr;
                             input: PByte; nbStripes: UIntPtr;
                             const secret: PByte; secretLimit: UIntPtr;
                             f_acc: TXXH3_accumulate_f;
                             f_scramble: TXXH3_scrambleAcc_f): PByte; inline;
var
  initialSecret: PByte;
  nbStripesThisIter: UIntPtr;
begin
  initialSecret:= secret + nbStripesSoFarPtr^ * XXH_SECRET_CONSUME_RATE;
  //* Process full blocks */
  if (nbStripes >= (nbStripesPerBlock - nbStripesSoFarPtr^)) then
  begin
    //* Process the initial partial block... */
    nbStripesThisIter:= nbStripesPerBlock - nbStripesSoFarPtr^;
    repeat
      //* Accumulate and scramble */
      f_acc(acc, input, initialSecret, nbStripesThisIter);
      f_scramble(acc, secret + secretLimit);
      input += nbStripesThisIter * XXH_STRIPE_LEN;
      nbStripes -= nbStripesThisIter;
      //* Then continue the loop with the full block size */
      nbStripesThisIter:= nbStripesPerBlock;
      initialSecret:= secret;
    until not (nbStripes >= nbStripesPerBlock);
    nbStripesSoFarPtr^:= 0;
  end;
  //* Process a partial block */
  if (nbStripes > 0) then
  begin
    f_acc(acc, input, initialSecret, nbStripes);
    input += nbStripes * XXH_STRIPE_LEN;
    nbStripesSoFarPtr^ += nbStripes;
  end;
  //* Return end pointer */
  Result:= input;
end;

procedure XXH3_update(const state: PXXH3_state_t; input: PByte; len: UIntPtr;
                      f_acc: TXXH3_accumulate_f; f_scramble: TXXH3_scrambleAcc_f); // inline;
var
  bEnd: PByte;
  acc: PUInt64;
  secret: PByte;
  loadSize: UIntPtr;
  nbStripes: UIntPtr;
begin
  bEnd:= input + len;
  if (state^.extSecret = nil) then
    secret:= state^.customSecret
  else begin
    secret:= state^.extSecret;
  end;
  acc:= state^.acc;
  state^.totalLen += len;
  Assert(state^.bufferedSize <= XXH3_INTERNALBUFFER_SIZE);

  //* small input : just fill in tmp buffer */
  if (len <= XXH3_INTERNALBUFFER_SIZE - state^.bufferedSize) then
  begin
    Move(input^, state^.buffer[state^.bufferedSize], len);
    state^.bufferedSize += XXH32_hash_t(len);
    Exit;
  end;

  //* total input is now > XXH3_INTERNALBUFFER_SIZE */
  Assert(XXH3_INTERNALBUFFER_SIZE mod XXH_STRIPE_LEN = 0); //* clean multiple */

  (*
   * Internal buffer is partially filled (always, except at beginning)
   * Complete it, then consume it.
   *)
  if (state^.bufferedSize > 0) then
  begin
    loadSize:= XXH3_INTERNALBUFFER_SIZE - state^.bufferedSize;
    Move(input^, state^.buffer[state^.bufferedSize], loadSize);
    input += loadSize;
    XXH3_consumeStripes(PByte(acc),
                        @state^.nbStripesSoFar, state^.nbStripesPerBlock,
                        state^.buffer, XXH3_INTERNALBUFFER_STRIPES,
                        secret, state^.secretLimit,
                        f_acc, f_scramble);
    state^.bufferedSize:= 0;
  end;
  Assert(input < bEnd);
  if (bEnd - input > XXH3_INTERNALBUFFER_SIZE) then
  begin
    nbStripes:= UIntPtr(bEnd - 1 - input) div XXH_STRIPE_LEN;
    input:= XXH3_consumeStripes(PByte(acc),
                                @state^.nbStripesSoFar, state^.nbStripesPerBlock,
                                input, nbStripes,
                                secret, state^.secretLimit,
                                f_acc, f_scramble);
    Move((input - XXH_STRIPE_LEN)^, state^.buffer[ + sizeof(state^.buffer) - XXH_STRIPE_LEN], XXH_STRIPE_LEN);

  end;
  //* Some remaining input (always) : buffer it */
  Assert(input < bEnd);
  Assert(bEnd - input <= XXH3_INTERNALBUFFER_SIZE);
  Assert(state^.bufferedSize = 0);
  Move(input^, state^.buffer[0], UIntPtr(bEnd - input));
  state^.bufferedSize:= XXH32_hash_t(bEnd - input);
end;

procedure XXH3_64bits_update(state: PXXH3_state_t; const input: Pointer; len: UIntPtr); inline;
begin
  XXH3_update(state, input, len, XXH3_accumulate, XXH3_scrambleAcc);
end;

procedure XXH3_128bits_update(state: PXXH3_state_t; const input: PByte; len: UIntPtr);
begin
  XXH3_64bits_update(state, input, len);
end;

procedure XXH3_digest_long(acc: PUInt64; const state: PXXH3_state_t;
                           const secret: PByte); inline;
var
  lastStripePtr: PByte;
  nbStripes, nbStripesSoFar, catchupSize: UIntPtr;
  lastStripe: array[0..Pred(XXH_STRIPE_LEN)] of Byte;
begin
  (*
   * Digest on a local copy. This way, the state remains unaltered, and it can
   * continue ingesting more input afterwards.
   *)
  Move(state^.acc[0], acc^, sizeof(state^.acc));
  if (state^.bufferedSize >= XXH_STRIPE_LEN) then
  begin
    //* Consume remaining stripes then point to remaining data in buffer */
    nbStripes:= (state^.bufferedSize - 1) div XXH_STRIPE_LEN;
    nbStripesSoFar:= state^.nbStripesSoFar;
    XXH3_consumeStripes(PByte(acc),
                       @nbStripesSoFar, state^.nbStripesPerBlock,
                        state^.buffer, nbStripes,
                        secret, state^.secretLimit,
                        XXH3_accumulate, XXH3_scrambleAcc);
    lastStripePtr:= @state^.buffer[state^.bufferedSize - XXH_STRIPE_LEN];
  end else begin  //* bufferedSize < XXH_STRIPE_LEN */
    //* Copy to temp buffer */
    catchupSize:= XXH_STRIPE_LEN - state^.bufferedSize;
    Assert(state^.bufferedSize > 0);  //* there is always some input buffered */
    Move(state^.buffer[sizeof(state^.buffer) - catchupSize], lastStripe[0], catchupSize);
    Move(state^.buffer[0], lastStripe[catchupSize], state^.bufferedSize);
    lastStripePtr:= lastStripe;
  end;
  //* Last stripe */
  XXH3_accumulate_512(PByte(acc),
                      lastStripePtr,
                      secret + state^.secretLimit - XXH_SECRET_LASTACC_START);
end;

function XXH_mult64to128(lhs, rhs: UInt64): XXH128_hash_t;
var
  cross, upper, lower: UInt64;
  lo_lo, hi_lo, lo_hi, hi_hi: UInt64;
begin
  //* First calculate all of the cross products. */
  lo_lo:= XXH_mult32to64(lhs and $FFFFFFFF, rhs and $FFFFFFFF);
  hi_lo:= XXH_mult32to64(lhs shr 32,        rhs and $FFFFFFFF);
  lo_hi:= XXH_mult32to64(lhs and $FFFFFFFF, rhs shr 32);
  hi_hi:= XXH_mult32to64(lhs shr 32,        rhs shr 32);

  //* Now add the products together. These will never overflow. */
  cross:= (lo_lo shr 32) + (hi_lo and $FFFFFFFF) + lo_hi;
  upper:= (hi_lo shr 32) + (cross shr 32)        + hi_hi;
  lower:= (cross shl 32) or (lo_lo and $FFFFFFFF);

  Result.low64  := lower;
  Result.high64 := upper;
end;

function XXH3_mul128_fold64(lhs, rhs: UInt64): UInt64;
var
  product: XXH128_hash_t;
begin
  product:= XXH_mult64to128(lhs, rhs);
  Result:= product.low64 xor product.high64;
end;

function XXH3_mix2Accs(const acc: PUInt64; const secret: PByte): Uint64; inline;
begin
  Result:= XXH3_mul128_fold64(
                              acc[0] xor XXH_readLE64(secret),
                              acc[1] xor XXH_readLE64(secret + 8) );
end;

function XXH3_avalanche(h64: UInt64): XXH64_hash_t;
begin
  h64:= XXH_xorshift64(h64, 37);
  h64 *= PRIME_MX1;
  h64:= XXH_xorshift64(h64, 32);
  Result:= h64;
end;

function XXH3_mergeAccs(const acc: PUInt64; const secret: PByte; start: UInt64): XXH64_hash_t;
var
  i: UIntPtr;
begin
  Result:= start;
  for i:= 0 to 3 do
  begin
    result += XXH3_mix2Accs(acc + 2 * i, secret + 16 * i);
  end;
  Result:= XXH3_avalanche(Result);
end;

function XXH3_len_9to16_128b(const input: PByte; len: UIntPtr; const secret: PByte; seed: XXH64_hash_t): XXH128_hash_t; inline;
var
  m128: XXH128_hash_t;
  bitflipl, bitfliph, input_lo, input_hi: UInt64;
begin
  Assert(input <> nil);
  Assert(secret <> nil);
  Assert((9 <= len) and (len <= 16));
  bitflipl := (XXH_readLE64(secret+32) xor XXH_readLE64(secret+40)) - seed;
  bitfliph := (XXH_readLE64(secret+48) xor XXH_readLE64(secret+56)) + seed;
  input_lo := XXH_readLE64(input);
  input_hi := XXH_readLE64(input + len - 8);
  m128:= XXH_mult64to128(input_lo xor input_hi xor bitflipl, XXH_PRIME64_1);
  {*
   * Put len in the middle of m128 to ensure that the length gets mixed to
   * both the low and high bits in the 128x64 multiply below.
   *}
  m128.low64 += UInt64(len - 1) << 54;
  input_hi   := input_hi xor bitfliph;
  {*
   * Add the high 32 bits of input_hi to the high 32 bits of m128, then
   * add the long product of the low 32 bits of input_hi and XXH_PRIME32_2 to
   * the high 64 bits of m128.
   *
   * The best approach to this operation is different on 32-bit and 64-bit.
   *}
{$IF DEFINED(CPU32)}
  {*
   * 32-bit optimized version, which is more readable.
   *
   * On 32-bit, it removes an ADC and delays a dependency between the two
   * halves of m128.high64, but it generates an extra mask on 64-bit.
   *}
  m128.high64 += (input_hi and UInt64($FFFFFFFF00000000)) + XXH_mult32to64(UInt32(input_hi), XXH_PRIME32_2);
{$ELSE}
  {*
   * 64-bit optimized (albeit more confusing) version.
   *
   * Uses some properties of addition and multiplication to remove the mask:
   *
   * Let:
   *    a = input_hi.lo = (input_hi & 0x00000000FFFFFFFF)
   *    b = input_hi.hi = (input_hi & 0xFFFFFFFF00000000)
   *    c = XXH_PRIME32_2
   *
   *    a + (b * c)
   * Inverse Property: x + y - x == y
   *    a + (b * (1 + c - 1))
   * Distributive Property: x * (y + z) == (x * y) + (x * z)
   *    a + (b * 1) + (b * (c - 1))
   * Identity Property: x * 1 == x
   *    a + b + (b * (c - 1))
   *
   * Substitute a, b, and c:
   *    input_hi.hi + input_hi.lo + ((xxh_u64)input_hi.lo * (XXH_PRIME32_2 - 1))
   *
   * Since input_hi.hi + input_hi.lo == input_hi, we get this:
   *    input_hi + ((xxh_u64)input_hi.lo * (XXH_PRIME32_2 - 1))
   *}
  m128.high64 += input_hi + XXH_mult32to64(UInt32(input_hi), XXH_PRIME32_2 - 1);
{$ENDIF}
  //* m128 ^= XXH_swap64(m128 >> 64); */
  m128.low64  := m128.low64 xor SwapEndian(m128.high64);

  //* 128x64 multiply: h128 = m128 * XXH_PRIME64_2; */
  Result:= XXH_mult64to128(m128.low64, XXH_PRIME64_2);
  Result.high64 += m128.high64 * XXH_PRIME64_2;

  Result.low64   := XXH3_avalanche(Result.low64);
  Result.high64  := XXH3_avalanche(Result.high64);
end;

function XXH3_len_4to8_128b(const input: PByte; len: UIntPtr; const secret: PByte; seed: XXH64_hash_t): XXH128_hash_t; inline;
var
  input_lo, input_hi: UInt32;
  input_64, bitflip, keyed: UInt64;
begin
  Assert(input <> nil);
  Assert(secret <> nil);
  Assert((4 <= len) and (len <= 8));
  seed := seed xor (UInt64(SwapEndian(UInt32(seed))) shl 32);
  input_lo := XXH_readLE32(input);
  input_hi := XXH_readLE32(input + len - 4);
  input_64 := input_lo + (UInt64(input_hi) shl 32);
  bitflip := (XXH_readLE64(secret+16) xor XXH_readLE64(secret+24)) + seed;
  keyed := input_64 xor bitflip;

  ///* Shift len to the left to ensure it is even, this avoids even multiplies. */
  Result:= XXH_mult64to128(keyed, XXH_PRIME64_1 + (len shl 2));

  Result.high64 += (Result.low64 shl 1);
  Result.low64  := Result.low64 xor (Result.high64 shr 3);

  Result.low64   := XXH_xorshift64(Result.low64, 35);
  Result.low64  *= PRIME_MX2;
  Result.low64   := XXH_xorshift64(Result.low64, 28);
  Result.high64  := XXH3_avalanche(Result.high64);
end;

function XXH3_len_1to3_128b(const input: PByte; len: UIntPtr; const secret: PByte; seed: XXH64_hash_t): XXH128_hash_t; inline;
var
  c1, c2, c3: Byte;
  combinedl, combinedh: UInt32;
  bitflipl, bitfliph, keyed_lo, keyed_hi: UInt64;
begin
  //* A doubled version of 1to3_64b with different constants. */
  Assert(input <> nil);
  Assert((1 <= len) and (len <= 3));
  Assert(secret <> nil);
  (*
   * len = 1: combinedl = { input[0], 0x01, input[0], input[0] }
   * len = 2: combinedl = { input[1], 0x02, input[0], input[1] }
   * len = 3: combinedl = { input[2], 0x03, input[0], input[1] }
   *)
  c1 := input[0];
  c2 := input[len >> 1];
  c3 := input[len - 1];
  combinedl := (UInt32(c1) shl 16) or (UInt32(c2) shl 24) or
               (UInt32(c3) shl 0) or (UInt32(len) shl 8);
  combinedh := RolDWord(SwapEndian(combinedl), 13);
  bitflipl := (XXH_readLE32(secret) xor XXH_readLE32(secret + 4)) + seed;
  bitfliph := (XXH_readLE32(secret+8) xor XXH_readLE32(secret + 12)) - seed;
  keyed_lo := UInt64(combinedl) xor bitflipl;
  keyed_hi := UInt64(combinedh) xor bitfliph;

  Result.low64  := XXH64_avalanche(keyed_lo);
  Result.high64 := XXH64_avalanche(keyed_hi);
end;

function XXH3_len_0to16_128b(const input: PByte; len: UIntPtr; const secret: PByte; seed: XXH64_hash_t): XXH128_hash_t; inline;
var
  bitflipl, bitfliph: UInt64;
begin
  Assert(len <= 16);
  if (len > 8) then
    Result:= XXH3_len_9to16_128b(input, len, secret, seed)
  else if (len >= 4) then
    Result:= XXH3_len_4to8_128b(input, len, secret, seed)
  else if (len > 0) then
    Result:= XXH3_len_1to3_128b(input, len, secret, seed)
  else begin
    bitflipl:= XXH_readLE64(secret+64) xor XXH_readLE64(secret+72);
    bitfliph:= XXH_readLE64(secret+80) xor XXH_readLE64(secret+88);
    Result.low64:= XXH64_avalanche(seed xor bitflipl);
    Result.high64:= XXH64_avalanche( seed xor bitfliph);
  end;
end;

function XXH3_mix16B(const input: PByte;
                     const secret: PByte; seed64: UInt64): UInt64; inline;
var
  input_lo, input_hi: UInt64;
begin
  input_lo := XXH_readLE64(input);
  input_hi := XXH_readLE64(input+8);
  Result:= XXH3_mul128_fold64(
            input_lo xor (XXH_readLE64(secret)   + seed64),
            input_hi xor (XXH_readLE64(secret+8) - seed64)
        );
end;

function XXH128_mix32B(var acc: XXH128_hash_t; const input_1: PByte; const input_2: PByte;
                       const secret: PByte; seed: XXH64_hash_t): XXH128_hash_t; inline;
begin
  acc.low64  += XXH3_mix16B (input_1, secret+0, seed);
  acc.low64  := acc.low64 xor (XXH_readLE64(input_2) + XXH_readLE64(input_2 + 8));
  acc.high64 += XXH3_mix16B (input_2, secret+16, seed);
  acc.high64 := acc.high64 xor (XXH_readLE64(input_1) + XXH_readLE64(input_1 + 8));
  Result:= acc;
end;

function XXH3_len_17to128_128b(const input: PByte; len: UIntPtr;
                               const secret: PByte; secretSize: UIntPtr;
                               seed: XXH64_hash_t): XXH128_hash_t; inline;
var
  acc: XXH128_hash_t;
begin
  Assert(secretSize >= XXH3_SECRET_SIZE_MIN);
  Assert((16 < len) and (len <= 128));

  acc.low64 := len * XXH_PRIME64_1;
  acc.high64 := 0;

  if (len > 32) then
  begin
    if (len > 64) then
    begin
      if (len > 96) then
      begin
        acc := XXH128_mix32B(acc, input+48, input+len-64, secret+96, seed);
      end;
      acc := XXH128_mix32B(acc, input+32, input+len-48, secret+64, seed);
    end;
    acc := XXH128_mix32B(acc, input+16, input+len-32, secret+32, seed);
  end;
  acc := XXH128_mix32B(acc, input, input+len-16, secret, seed);

  Result.low64  := acc.low64 + acc.high64;
  Result.high64 := (acc.low64    * XXH_PRIME64_1)
                 + (acc.high64   * XXH_PRIME64_4)
                 + ((len - seed) * XXH_PRIME64_2);
  Result.low64  := XXH3_avalanche(Result.low64);
  Result.high64 := XXH64_hash_t(0) - XXH3_avalanche(Result.high64);
end;

function XXH3_len_129to240_128b(const input: PBYte; len: UIntPtr;
                                const secret: PByte; secretSize: UIntPtr;
                                seed: XXH64_hash_t): XXH128_hash_t; inline;
var
  i: UInt32;
  acc: XXH128_hash_t;
begin
  Assert(secretSize >= XXH3_SECRET_SIZE_MIN);
  Assert((128 < len) and (len <= XXH3_MIDSIZE_MAX));

  acc.low64 := len * XXH_PRIME64_1;
  acc.high64 := 0;
  {*
   *  We set as `i` as offset + 32. We do this so that unchanged
   * `len` can be used as upper bound. This reaches a sweet spot
   * where both x86 and aarch64 get simple agen and good codegen
   * for the loop.
   *}
  i:= 32;
  while (i < 160) do
  begin
    acc := XXH128_mix32B(acc,
                         input  + i - 32,
                         input  + i - 16,
                         secret + i - 32,
                         seed);
    Inc(i, 32);
  end;

  acc.low64 := XXH3_avalanche(acc.low64);
  acc.high64 := XXH3_avalanche(acc.high64);
  {*
   * NB: `i <= len` will duplicate the last 32-bytes if
   * len % 32 was zero. This is an unfortunate necessity to keep
   * the hash result stable.
   *}
  i:= 160;
  while i <= len do
  begin
      acc := XXH128_mix32B(acc,
                           input + i - 32,
                           input + i - 16,
                           secret + XXH3_MIDSIZE_STARTOFFSET + i - 160,
                           seed);
    Inc(i, 32);
  end;
  //* last bytes */
  acc := XXH128_mix32B(acc,
                       input + len - 16,
                       input + len - 32,
                       secret + XXH3_SECRET_SIZE_MIN - XXH3_MIDSIZE_LASTOFFSET - 16,
                       XXH64_hash_t(0) - seed);

  Result.low64  := acc.low64 + acc.high64;
  Result.high64 := (acc.low64    * XXH_PRIME64_1)
                 + (acc.high64   * XXH_PRIME64_4)
                 + ((len - seed) * XXH_PRIME64_2);
  Result.low64  := XXH3_avalanche(Result.low64);
  Result.high64 := XXH64_hash_t(0) - XXH3_avalanche(Result.high64);
end;

function XXH3_128bits_internal(const input: PByte; len: UIntPtr; seed64: XXH64_hash_t;
                               const secret: PByte; secretLen: UIntPtr): XXH128_hash_t; inline;
begin
  Assert(len <= XXH3_MIDSIZE_MAX);
  Assert(secretLen >= XXH3_SECRET_SIZE_MIN);
  (*
   * If an action is to be taken if `secret` conditions are not respected,
   * it should be done here.
   * For now, it's a contract pre-condition.
   * Adding a check and a branch here would cost performance at every hash.
   *)
  if (len <= 16) then
    Result:= XXH3_len_0to16_128b(input, len, secret, seed64)
  else if (len <= 128) then
    Result:= XXH3_len_17to128_128b(input, len, secret, secretLen, seed64)
  else begin
    Result:= XXH3_len_129to240_128b(input, len, secret, secretLen, seed64);
  end;
end;

function XXH3_128bits_digest(const state: PXXH3_state_t): XXH128_hash_t;
var
  acc: PUInt64;
  secret: PByte;
  buffer: array[0..Pred(XXH_ACC_SIZE + XXH_ACC_ALIGN)] of Byte;
begin
  if (state^.extSecret = nil) then
    secret:= state^.customSecret
  else begin
    secret:= state^.extSecret;
  end;
  if (state^.totalLen > XXH3_MIDSIZE_MAX) then
  begin
    acc:= System.Align(@buffer[0], XXH_ACC_ALIGN);
    Assert(UIntPtr(acc) mod XXH_ACC_ALIGN = 0);

    XXH3_digest_long(acc, state, secret);
    Assert(state^.secretLimit + XXH_STRIPE_LEN >= sizeof(acc) + XXH_SECRET_MERGEACCS_START);

    Result.low64  := XXH3_mergeAccs(acc,
                                    secret + XXH_SECRET_MERGEACCS_START,
                                    UInt64(state^.totalLen) * XXH_PRIME64_1);
    Result.high64 := XXH3_mergeAccs(acc,
                                    secret + state^.secretLimit + XXH_STRIPE_LEN -
                                    XXH_ACC_SIZE - XXH_SECRET_MERGEACCS_START,
                                    not (UInt64(state^.totalLen) * XXH_PRIME64_2));
  end
  else begin
    //* len <= XXH3_MIDSIZE_MAX : short code */
    {
    if (state^.useSeed)
      Result:= XXH3_128bits_withSeed(state^.buffer, UIntPtr(state^.totalLen), state^.seed);
    else
    }
    Result:= XXH3_128bits_internal(state^.buffer, UIntPtr(state^.totalLen), 0,
                                   secret, state^.secretLimit + XXH_STRIPE_LEN);
  end;
end;

initialization
{$IF DEFINED(CPUX86_64)}
  if AVX2Support then
  begin
    XXH3_accumulate:= @XXH3_accumulate_avx2;
    XXH3_scrambleAcc:= @XXH3_scrambleAcc_avx2;
    XXH3_accumulate_512:= @XXH3_accumulate_512_avx2;
  end
  else begin
    XXH3_accumulate:= @XXH3_accumulate_sse2;
    XXH3_scrambleAcc:= @XXH3_scrambleAcc_sse2;
    XXH3_accumulate_512:= @XXH3_accumulate_512_sse2;
  end;
{$ELSE}
  XXH3_accumulate:= @XXH3_accumulate_scalar;
  XXH3_scrambleAcc:= @XXH3_scrambleAcc_scalar;
  XXH3_accumulate_512:= @XXH3_accumulate_512_scalar;
{$ENDIF}
end.
