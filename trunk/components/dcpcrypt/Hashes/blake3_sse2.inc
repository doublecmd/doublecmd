const
  PBLENDW_0x33_MASK: array[0..3] of cuint32 = ($FFFFFFFF, $00000000, $FFFFFFFF, $00000000);
  PBLENDW_0xCC_MASK: array[0..3] of cuint32 = ($00000000, $FFFFFFFF, $00000000, $FFFFFFFF);
  PBLENDW_0x3F_MASK: array[0..3] of cuint32 = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00000000);
  PBLENDW_0xC0_MASK: array[0..3] of cuint32 = ($00000000, $00000000, $00000000, $FFFFFFFF);

procedure blake3_compress_in_place_sse2(cv: pcuint32; const block: pcuint8;
                                        block_len: cuint8; counter: cuint64;
                                        flags: cuint8); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX, R8
// WIN64:  RCX, RDX, R8,  R9,  STACK
asm
  .p2align 6
{$IF DEFINED(WIN64)}
  push   %rsi
  push   %rdi
  movq   %rdx, %rsi
  movq   %rcx, %rdi
  movq   %r8, %rdx
  movq   %r9, %rcx
  sub    $0x78,%rsp
  movdqa %xmm6,(%rsp)
  movdqa %xmm7,0x10(%rsp)
  movdqa %xmm8,0x20(%rsp)
  movdqa %xmm9,0x30(%rsp)
  movdqa %xmm11,0x40(%rsp)
  movdqa %xmm14,0x50(%rsp)
  movdqa %xmm15,0x60(%rsp)
  movzbl 0xb0(%rsp),%r8
{$ENDIF}
  movups (%rdi),%xmm0
  movups 0x10(%rdi),%xmm1
  movaps IV(%rip),%xmm2
  shl    $0x20,%r8
  add    %r8,%rdx
  movq   %rcx,%xmm3
  movq   %rdx,%xmm4
  punpcklqdq %xmm4,%xmm3
  movups (%rsi),%xmm4
  movups 0x10(%rsi),%xmm5
  movaps %xmm4,%xmm8
  shufps $0x88,%xmm5,%xmm4
  shufps $0xdd,%xmm5,%xmm8
  movaps %xmm8,%xmm5
  movups 0x20(%rsi),%xmm6
  movups 0x30(%rsi),%xmm7
  movaps %xmm6,%xmm8
  shufps $0x88,%xmm7,%xmm6
  pshufd $0x93,%xmm6,%xmm6
  shufps $0xdd,%xmm7,%xmm8
  pshufd $0x93,%xmm8,%xmm7
  mov    $0x7,%al

.L2bdb:
  paddd  %xmm4,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  pshuflw $0xb1,%xmm3,%xmm3
  pshufhw $0xb1,%xmm3,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x14,%xmm1
  psrld  $0xc,%xmm11
  por    %xmm11,%xmm1
  paddd  %xmm5,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  movdqa %xmm3,%xmm14
  psrld  $0x8,%xmm3
  pslld  $0x18,%xmm14
  pxor   %xmm14,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x19,%xmm1
  psrld  $0x7,%xmm11
  por    %xmm11,%xmm1
  pshufd $0x93,%xmm0,%xmm0
  pshufd $0x4e,%xmm3,%xmm3
  pshufd $0x39,%xmm2,%xmm2
  paddd  %xmm6,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  pshuflw $0xb1,%xmm3,%xmm3
  pshufhw $0xb1,%xmm3,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x14,%xmm1
  psrld  $0xc,%xmm11
  por    %xmm11,%xmm1
  paddd  %xmm7,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  movdqa %xmm3,%xmm14
  psrld  $0x8,%xmm3
  pslld  $0x18,%xmm14
  pxor   %xmm14,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x19,%xmm1
  psrld  $0x7,%xmm11
  por    %xmm11,%xmm1
  pshufd $0x39,%xmm0,%xmm0
  pshufd $0x4e,%xmm3,%xmm3
  pshufd $0x93,%xmm2,%xmm2
  dec    %al
  je     .L2d5e
  movdqa %xmm4,%xmm8
  shufps $0xd6,%xmm5,%xmm8
  pshufd $0xf,%xmm4,%xmm9
  pshufd $0x39,%xmm8,%xmm4
  movdqa %xmm6,%xmm8
  shufps $0xfa,%xmm7,%xmm8
  pand   PBLENDW_0x33_MASK(%rip),%xmm9
  pand   PBLENDW_0xCC_MASK(%rip),%xmm8
  por    %xmm8,%xmm9
  movdqa %xmm7,%xmm8
  punpcklqdq %xmm5,%xmm8
  movdqa %xmm6,%xmm10
  pand   PBLENDW_0x3F_MASK(%rip),%xmm8
  pand   PBLENDW_0xC0_MASK(%rip),%xmm10
  por    %xmm10,%xmm8
  pshufd $0x78,%xmm8,%xmm8
  punpckhdq %xmm7,%xmm5
  punpckldq %xmm5,%xmm6
  pshufd $0x1e,%xmm6,%xmm7
  movdqa %xmm9,%xmm5
  movdqa %xmm8,%xmm6
  jmpq   .L2bdb

.L2d5e:
  pxor   %xmm2,%xmm0
  pxor   %xmm3,%xmm1
  movups %xmm0,(%rdi)
  movups %xmm1,0x10(%rdi)
{$IF DEFINED(WIN64)}
  movdqa (%rsp),%xmm6
  movdqa 0x10(%rsp),%xmm7
  movdqa 0x20(%rsp),%xmm8
  movdqa 0x30(%rsp),%xmm9
  movdqa 0x40(%rsp),%xmm11
  movdqa 0x50(%rsp),%xmm14
  movdqa 0x60(%rsp),%xmm15
  add    $0x78,%rsp
  pop    %rdi
  pop    %rsi
{$ENDIF}
  retq
end;

procedure blake3_compress_xof_sse2(const cv: pcuint32;
                                   const block: pcuint8;
                                   block_len: cuint8; counter: cuint64;
                                   flags: cuint8; out_: pcuint8); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX, R8,    R9
// WIN64:  RCX, RDX, R8,  R9,  STACK, STACK
asm
{$IF DEFINED(WIN64)}
  push   %rsi
  push   %rdi
  movq   %rdx, %rsi
  movq   %rcx, %rdi
  movq   %r8, %rdx
  movq   %r9, %rcx
  sub    $0x78,%rsp
  movdqa %xmm6,(%rsp)
  movdqa %xmm7,0x10(%rsp)
  movdqa %xmm8,0x20(%rsp)
  movdqa %xmm9,0x30(%rsp)
  movdqa %xmm11,0x40(%rsp)
  movdqa %xmm14,0x50(%rsp)
  movdqa %xmm15,0x60(%rsp)
  movzbl 0xb0(%rsp),%r8
  mov    0xb8(%rsp),%r9
{$ENDIF}
  movups (%rdi),%xmm0
  movups 0x10(%rdi),%xmm1
  movaps IV(%rip),%xmm2
  movzbl %r8b,%eax
  movzbl %dl,%edx
  shl    $0x20,%rax
  add    %rax,%rdx
  movq   %rcx,%xmm3
  movq   %rdx,%xmm4
  punpcklqdq %xmm4,%xmm3
  movups (%rsi),%xmm4
  movups 0x10(%rsi),%xmm5
  movaps %xmm4,%xmm8
  shufps $0x88,%xmm5,%xmm4
  shufps $0xdd,%xmm5,%xmm8
  movaps %xmm8,%xmm5
  movups 0x20(%rsi),%xmm6
  movups 0x30(%rsi),%xmm7
  movaps %xmm6,%xmm8
  shufps $0x88,%xmm7,%xmm6
  pshufd $0x93,%xmm6,%xmm6
  shufps $0xdd,%xmm7,%xmm8
  pshufd $0x93,%xmm8,%xmm7
  mov    $0x7,%al

.LB2ca4:
  paddd  %xmm4,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  pshuflw $0xb1,%xmm3,%xmm3
  pshufhw $0xb1,%xmm3,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x14,%xmm1
  psrld  $0xc,%xmm11
  por    %xmm11,%xmm1
  paddd  %xmm5,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  movdqa %xmm3,%xmm14
  psrld  $0x8,%xmm3
  pslld  $0x18,%xmm14
  pxor   %xmm14,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x19,%xmm1
  psrld  $0x7,%xmm11
  por    %xmm11,%xmm1
  pshufd $0x93,%xmm0,%xmm0
  pshufd $0x4e,%xmm3,%xmm3
  pshufd $0x39,%xmm2,%xmm2
  paddd  %xmm6,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  pshuflw $0xb1,%xmm3,%xmm3
  pshufhw $0xb1,%xmm3,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x14,%xmm1
  psrld  $0xc,%xmm11
  por    %xmm11,%xmm1
  paddd  %xmm7,%xmm0
  paddd  %xmm1,%xmm0
  pxor   %xmm0,%xmm3
  movdqa %xmm3,%xmm14
  psrld  $0x8,%xmm3
  pslld  $0x18,%xmm14
  pxor   %xmm14,%xmm3
  paddd  %xmm3,%xmm2
  pxor   %xmm2,%xmm1
  movdqa %xmm1,%xmm11
  pslld  $0x19,%xmm1
  psrld  $0x7,%xmm11
  por    %xmm11,%xmm1
  pshufd $0x39,%xmm0,%xmm0
  pshufd $0x4e,%xmm3,%xmm3
  pshufd $0x93,%xmm2,%xmm2
  dec    %al
  je .LB2e27
  movdqa %xmm4,%xmm8
  shufps $0xd6,%xmm5,%xmm8
  pshufd $0xf,%xmm4,%xmm9
  pshufd $0x39,%xmm8,%xmm4
  movdqa %xmm6,%xmm8
  shufps $0xfa,%xmm7,%xmm8
  pand   PBLENDW_0x33_MASK(%rip),%xmm9
  pand   PBLENDW_0xCC_MASK(%rip),%xmm8
  por    %xmm8,%xmm9
  movdqa %xmm7,%xmm8
  punpcklqdq %xmm5,%xmm8
  movdqa %xmm6,%xmm10
  pand   PBLENDW_0x3F_MASK(%rip),%xmm8
  pand   PBLENDW_0xC0_MASK(%rip),%xmm10
  por    %xmm10,%xmm8
  pshufd $0x78,%xmm8,%xmm8
  punpckhdq %xmm7,%xmm5
  punpckldq %xmm5,%xmm6
  pshufd $0x1e,%xmm6,%xmm7
  movdqa %xmm9,%xmm5
  movdqa %xmm8,%xmm6
  jmpq .LB2ca4

.LB2e27:
  movdqu (%rdi),%xmm4
  movdqu 0x10(%rdi),%xmm5
  pxor   %xmm2,%xmm0
  pxor   %xmm3,%xmm1
  pxor   %xmm4,%xmm2
  pxor   %xmm5,%xmm3
  movups %xmm0,(%r9)
  movups %xmm1,0x10(%r9)
  movups %xmm2,0x20(%r9)
  movups %xmm3,0x30(%r9)
{$IF DEFINED(WIN64)}
  movdqa (%rsp),%xmm6
  movdqa 0x10(%rsp),%xmm7
  movdqa 0x20(%rsp),%xmm8
  movdqa 0x30(%rsp),%xmm9
  movdqa 0x40(%rsp),%xmm11
  movdqa 0x50(%rsp),%xmm14
  movdqa 0x60(%rsp),%xmm15
  add    $0x78,%rsp
  pop    %rdi
  pop    %rsi
{$ENDIF}
  retq
end;

procedure blake3_hash4_sse2(inputs: ppcuint8; blocks: csize_t;
                            const key: pcuint32; counter: cuint64;
                            increment_counter: cbool; flags: cuint8;
                            flags_start: cuint8; flags_end: cuint8;
                            out_: pcuint8); assembler; nostackframe;
asm

end;

procedure hash_one_sse2(input: pcuint8; blocks: csize_t;
                        const key: pcuint32; counter: cuint64;
                        flags: uint8; flags_start: cuint8;
                        flags_end: cuint8; out_: pcuint8); inline;
var
  block_flags: cuint8;
  cv: array[0..7] of cuint32;
begin
  Move(key^, cv[0], BLAKE3_KEY_LEN);
  block_flags := flags or flags_start;
  while (blocks > 0) do
  begin
    if (blocks = 1) then begin
      block_flags := block_flags or flags_end;
    end;
    blake3_compress_in_place_sse2(cv, input, BLAKE3_BLOCK_LEN, counter,
                                      block_flags);
    input := @input[BLAKE3_BLOCK_LEN];
    blocks -= 1;
    block_flags := flags;
  end;
  Move(cv[0], out_^, BLAKE3_OUT_LEN);
end;

procedure blake3_hash_many_sse2(inputs: ppcuint8; num_inputs: csize_t;
                                blocks: csize_t; const key: pcuint32;
                                counter: cuint64; increment_counter: cbool;
                                flags: cuint8; flags_start: cuint8;
                                flags_end: cuint8; out_: pcuint8);
const
  DEGREE = 4;
begin
  {
  while (num_inputs >= DEGREE) do
  begin
    blake3_hash4_sse2(inputs, blocks, key, counter, increment_counter, flags,
                      flags_start, flags_end, out_);
    if (increment_counter) then
    begin
      counter += DEGREE;
    end;
    inputs += DEGREE;
    num_inputs -= DEGREE;
    out_ := @out_[DEGREE * BLAKE3_OUT_LEN];
  end;
  }
  while (num_inputs > 0) do
  begin
    hash_one_sse2(inputs[0], blocks, key, counter, flags, flags_start,
                      flags_end, out_);
    if (increment_counter) then begin
      counter += 1;
    end;
    inputs += 1;
    num_inputs -= 1;
    out_ := @out_[BLAKE3_OUT_LEN];
  end;
end;


