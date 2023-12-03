{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 2012, Intel Corporation
;
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
; * Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;
; * Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in the
;   documentation and/or other materials provided with the
;   distribution.
;
; * Neither the name of the Intel Corporation nor the names of its
;   contributors may be used to endorse or promote products derived from
;   this software without specific prior written permission.
;
;
; THIS SOFTWARE IS PROVIDED BY INTEL CORPORATION "AS IS" AND ANY
; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INTEL CORPORATION OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/
/*
 * Conversion to GAS assembly and integration to libgcrypt
 *  by Jussi Kivilinna <jussi.kivilinna@iki.fi>
 *
 * Note: original implementation was named as SHA512-SSE4. However, only SSSE3
 *       is required.
 */
}

procedure sha512_compress_sse(HashBuffer: PByte; CurrentHash: PInt64; BufferCount: UIntPtr); assembler; nostackframe;
asm
{$IF DEFINED(WIN64)}
  pushq  %rsi
  pushq  %rdi
  movq   %rcx, %rdi
  movq   %rdx, %rsi
  movq   %r8,  %rdx
{$ENDIF}

  xor    %eax,%eax

  cmp    $0x0,%rdx
  je     .Lnowork

  //* Allocate Stack Space */
  sub    $0x2b8,%rsp

  //* Save GPRs */
  mov    %rbx,0x290(%rsp)
  mov    %r12,0x298(%rsp)
  mov    %r13,0x2a0(%rsp)
  mov    %r14,0x2a8(%rsp)
  mov    %r15,0x2b0(%rsp)

.Lupdateblock:

  mov    (%rsi),%r9
  mov    0x8(%rsi),%r10
  mov    0x10(%rsi),%r11
  mov    0x18(%rsi),%r12
  mov    0x20(%rsi),%r13
  mov    0x28(%rsi),%r14
  mov    0x30(%rsi),%r15
  mov    0x38(%rsi),%rbx

  //* (80 rounds) / (2 rounds/iteration) + (1 iteration) */

  //.if t < 2 (t == 0)

  //* BSWAP 2 QWORDS */
  movdqa .LXMM_QWORD_BSWAP(%rip),%xmm1
  movdqu (%rdi),%xmm0
  //* BSWAP */
  pshufb %xmm1,%xmm0
  //* Store Scheduled Pair */
  movdqa %xmm0,(%rsp)
  //* Compute W[t]+K[t] */
  paddq  .LK512_0(%rip),%xmm0
  //* Store into WK for rounds */
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 2)

  //* BSWAP 2 QWORDS; Compute 2 Rounds */
  movdqu 0x10(%rdi),%xmm0
  //* BSWAP */
  pshufb %xmm1,%xmm0
  //* Round t-2 */
  mov    %r14,%rcx
  mov    %r13,%rax
  xor    %r15,%rcx
  ror    $0x17,%rax
  and    %r13,%rcx
  xor    %r13,%rax
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r13,%rax
  mov    %r9,%r8
  add    %rbx,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r9,%rax
  xor    %r11,%r8
  and    %r11,%rax
  and    %r10,%r8
  xor    %rax,%r8
  mov    %r9,%rax
  ror    $0x5,%rax
  xor    %r9,%rax
  add    %rcx,%r12
  ror    $0x6,%rax
  xor    %r9,%rax
  lea    (%rcx,%r8,1),%rbx
  ror    $0x1c,%rax
  add    %rax,%rbx
  //* Store Scheduled Pair */
  movdqa %xmm0,0x10(%rsp)
  //* Compute W[t]+K[t] */
  paddq  .LK512_1(%rip),%xmm0

  //* Round t-1 */
  mov    %r13,%rcx
  mov    %r12,%rax
  xor    %r14,%rcx
  ror    $0x17,%rax
  and    %r12,%rcx
  xor    %r12,%rax
  xor    %r14,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r12,%rax
  mov    %rbx,%r8
  add    %r15,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %rbx,%rax
  xor    %r10,%r8
  and    %r10,%rax
  and    %r9,%r8
  xor    %rax,%r8
  mov    %rbx,%rax
  ror    $0x5,%rax
  xor    %rbx,%rax
  add    %rcx,%r11
  ror    $0x6,%rax
  xor    %rbx,%rax
  lea    (%rcx,%r8,1),%r15
  ror    $0x1c,%rax
  add    %rax,%r15
  //* Store W[t]+K[t] into WK */
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 4)
  movdqu 0x20(%rdi),%xmm0
  pshufb %xmm1,%xmm0
  mov    %r12,%rcx
  mov    %r11,%rax
  xor    %r13,%rcx
  ror    $0x17,%rax
  and    %r11,%rcx
  xor    %r11,%rax
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r11,%rax
  mov    %r15,%r8
  add    %r14,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r15,%rax
  xor    %r9,%r8
  and    %r9,%rax
  and    %rbx,%r8
  xor    %rax,%r8
  mov    %r15,%rax
  ror    $0x5,%rax
  xor    %r15,%rax
  add    %rcx,%r10
  ror    $0x6,%rax
  xor    %r15,%rax
  lea    (%rcx,%r8,1),%r14
  ror    $0x1c,%rax
  add    %rax,%r14
  movdqa %xmm0,0x20(%rsp)
  paddq  .LK512_2(%rip),%xmm0

  mov    %r11,%rcx
  mov    %r10,%rax
  xor    %r12,%rcx
  ror    $0x17,%rax
  and    %r10,%rcx
  xor    %r10,%rax
  xor    %r12,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r10,%rax
  mov    %r14,%r8
  add    %r13,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r14,%rax
  xor    %rbx,%r8
  and    %rbx,%rax
  and    %r15,%r8
  xor    %rax,%r8
  mov    %r14,%rax
  ror    $0x5,%rax
  xor    %r14,%rax
  add    %rcx,%r9
  ror    $0x6,%rax
  xor    %r14,%rax
  lea    (%rcx,%r8,1),%r13
  ror    $0x1c,%rax
  add    %rax,%r13
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 6)
  movdqu 0x30(%rdi),%xmm0
  pshufb %xmm1,%xmm0
  mov    %r10,%rcx
  mov    %r9,%rax
  xor    %r11,%rcx
  ror    $0x17,%rax
  and    %r9,%rcx
  xor    %r9,%rax
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r9,%rax
  mov    %r13,%r8
  add    %r12,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r13,%rax
  xor    %r15,%r8
  and    %r15,%rax
  and    %r14,%r8
  xor    %rax,%r8
  mov    %r13,%rax
  ror    $0x5,%rax
  xor    %r13,%rax
  add    %rcx,%rbx
  ror    $0x6,%rax
  xor    %r13,%rax
  lea    (%rcx,%r8,1),%r12
  ror    $0x1c,%rax
  add    %rax,%r12
  movdqa %xmm0,0x30(%rsp)
  paddq  .LK512_3(%rip),%xmm0

  mov    %r9,%rcx
  mov    %rbx,%rax
  xor    %r10,%rcx
  ror    $0x17,%rax
  and    %rbx,%rcx
  xor    %rbx,%rax
  xor    %r10,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %rbx,%rax
  mov    %r12,%r8
  add    %r11,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r12,%rax
  xor    %r14,%r8
  and    %r14,%rax
  and    %r13,%r8
  xor    %rax,%r8
  mov    %r12,%rax
  ror    $0x5,%rax
  xor    %r12,%rax
  add    %rcx,%r15
  ror    $0x6,%rax
  xor    %r12,%rax
  lea    (%rcx,%r8,1),%r11
  ror    $0x1c,%rax
  add    %rax,%r11
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 8)
  movdqu 0x40(%rdi),%xmm0
  pshufb %xmm1,%xmm0
  mov    %rbx,%rcx
  mov    %r15,%rax
  xor    %r9,%rcx
  ror    $0x17,%rax
  and    %r15,%rcx
  xor    %r15,%rax
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r15,%rax
  mov    %r11,%r8
  add    %r10,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r11,%rax
  xor    %r13,%r8
  and    %r13,%rax
  and    %r12,%r8
  xor    %rax,%r8
  mov    %r11,%rax
  ror    $0x5,%rax
  xor    %r11,%rax
  add    %rcx,%r14
  ror    $0x6,%rax
  xor    %r11,%rax
  lea    (%rcx,%r8,1),%r10
  ror    $0x1c,%rax
  add    %rax,%r10
  movdqa %xmm0,0x40(%rsp)
  paddq  .LK512_4(%rip),%xmm0

  mov    %r15,%rcx
  mov    %r14,%rax
  xor    %rbx,%rcx
  ror    $0x17,%rax
  and    %r14,%rcx
  xor    %r14,%rax
  xor    %rbx,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r14,%rax
  mov    %r10,%r8
  add    %r9,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r10,%rax
  xor    %r12,%r8
  and    %r12,%rax
  and    %r11,%r8
  xor    %rax,%r8
  mov    %r10,%rax
  ror    $0x5,%rax
  xor    %r10,%rax
  add    %rcx,%r13
  ror    $0x6,%rax
  xor    %r10,%rax
  lea    (%rcx,%r8,1),%r9
  ror    $0x1c,%rax
  add    %rax,%r9
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 10)
  movdqu 0x50(%rdi),%xmm0
  pshufb %xmm1,%xmm0
  mov    %r14,%rcx
  mov    %r13,%rax
  xor    %r15,%rcx
  ror    $0x17,%rax
  and    %r13,%rcx
  xor    %r13,%rax
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r13,%rax
  mov    %r9,%r8
  add    %rbx,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r9,%rax
  xor    %r11,%r8
  and    %r11,%rax
  and    %r10,%r8
  xor    %rax,%r8
  mov    %r9,%rax
  ror    $0x5,%rax
  xor    %r9,%rax
  add    %rcx,%r12
  ror    $0x6,%rax
  xor    %r9,%rax
  lea    (%rcx,%r8,1),%rbx
  ror    $0x1c,%rax
  add    %rax,%rbx
  movdqa %xmm0,0x50(%rsp)
  paddq  .LK512_5(%rip),%xmm0

  mov    %r13,%rcx
  mov    %r12,%rax
  xor    %r14,%rcx
  ror    $0x17,%rax
  and    %r12,%rcx
  xor    %r12,%rax
  xor    %r14,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r12,%rax
  mov    %rbx,%r8
  add    %r15,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %rbx,%rax
  xor    %r10,%r8
  and    %r10,%rax
  and    %r9,%r8
  xor    %rax,%r8
  mov    %rbx,%rax
  ror    $0x5,%rax
  xor    %rbx,%rax
  add    %rcx,%r11
  ror    $0x6,%rax
  xor    %rbx,%rax
  lea    (%rcx,%r8,1),%r15
  ror    $0x1c,%rax
  add    %rax,%r15
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 12)
  movdqu 0x60(%rdi),%xmm0
  pshufb %xmm1,%xmm0
  mov    %r12,%rcx
  mov    %r11,%rax
  xor    %r13,%rcx
  ror    $0x17,%rax
  and    %r11,%rcx
  xor    %r11,%rax
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r11,%rax
  mov    %r15,%r8
  add    %r14,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r15,%rax
  xor    %r9,%r8
  and    %r9,%rax
  and    %rbx,%r8
  xor    %rax,%r8
  mov    %r15,%rax
  ror    $0x5,%rax
  xor    %r15,%rax
  add    %rcx,%r10
  ror    $0x6,%rax
  xor    %r15,%rax
  lea    (%rcx,%r8,1),%r14
  ror    $0x1c,%rax
  add    %rax,%r14
  movdqa %xmm0,0x60(%rsp)
  paddq  .LK512_6(%rip),%xmm0

  mov    %r11,%rcx
  mov    %r10,%rax
  xor    %r12,%rcx
  ror    $0x17,%rax
  and    %r10,%rcx
  xor    %r10,%rax
  xor    %r12,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r10,%rax
  mov    %r14,%r8
  add    %r13,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r14,%rax
  xor    %rbx,%r8
  and    %rbx,%rax
  and    %r15,%r8
  xor    %rax,%r8
  mov    %r14,%rax
  ror    $0x5,%rax
  xor    %r14,%rax
  add    %rcx,%r9
  ror    $0x6,%rax
  xor    %r14,%rax
  lea    (%rcx,%r8,1),%r13
  ror    $0x1c,%rax
  add    %rax,%r13
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 16 (t == 14)
  movdqu 0x70(%rdi),%xmm0
  pshufb %xmm1,%xmm0
  mov    %r10,%rcx
  mov    %r9,%rax
  xor    %r11,%rcx
  ror    $0x17,%rax
  and    %r9,%rcx
  xor    %r9,%rax
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r9,%rax
  mov    %r13,%r8
  add    %r12,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r13,%rax
  xor    %r15,%r8
  and    %r15,%rax
  and    %r14,%r8
  xor    %rax,%r8
  mov    %r13,%rax
  ror    $0x5,%rax
  xor    %r13,%rax
  add    %rcx,%rbx
  ror    $0x6,%rax
  xor    %r13,%rax
  lea    (%rcx,%r8,1),%r12
  ror    $0x1c,%rax
  add    %rax,%r12
  movdqa %xmm0,0x70(%rsp)
  paddq  .LK512_7(%rip),%xmm0

  mov    %r9,%rcx
  mov    %rbx,%rax
  xor    %r10,%rcx
  ror    $0x17,%rax
  and    %rbx,%rcx
  xor    %rbx,%rax
  xor    %r10,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %rbx,%rax
  mov    %r12,%r8
  add    %r11,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r12,%rax
  xor    %r14,%r8
  and    %r14,%rax
  and    %r13,%r8
  xor    %rax,%r8
  mov    %r12,%rax
  ror    $0x5,%rax
  xor    %r12,%rax
  add    %rcx,%r15
  ror    $0x6,%rax
  xor    %r12,%rax
  lea    (%rcx,%r8,1),%r11
  ror    $0x1c,%rax
  add    %rax,%r11
  movdqa %xmm0,0x280(%rsp)

  //.elseif t < 79 (t == 16)

  //* Schedule 2 QWORDS; Compute 2 Rounds */
  mov    %rbx,%rcx
  movdqa 0x70(%rsp),%xmm2
  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x8(%rsp),%xmm5
  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x48(%rsp),%xmm1
  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  (%rsp),%xmm0
  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x80(%rsp)

  xor    %r10,%rax
  paddq  .LK512_8(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 18)
  mov    %r14,%rcx
  movdqa 0x80(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x18(%rsp),%xmm5
  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x58(%rsp),%xmm1
  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x10(%rsp),%xmm0
  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x90(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_9(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 20)
  mov    %r12,%rcx
  movdqa 0x90(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x28(%rsp),%xmm5
  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0x68(%rsp),%xmm1
  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x20(%rsp),%xmm0
  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0xa0(%rsp)

  xor    %r14,%rax
  paddq  .LK512_10(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 22)
  mov    %r10,%rcx
  movdqa 0xa0(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x38(%rsp),%xmm5
  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0x78(%rsp),%xmm1
  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x30(%rsp),%xmm0
  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0xb0(%rsp)

  xor    %r12,%rax
  paddq  .LK512_11(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 24)
  mov    %rbx,%rcx
  movdqa 0xb0(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x48(%rsp),%xmm5
  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x88(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x40(%rsp),%xmm0
  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0xc0(%rsp)

  xor    %r10,%rax
  paddq  .LK512_12(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 26)
  mov    %r14,%rcx
  movdqa 0xc0(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x58(%rsp),%xmm5
  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x98(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x50(%rsp),%xmm0
  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0xd0(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_13(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 28)
  mov    %r12,%rcx
  movdqa 0xd0(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x68(%rsp),%xmm5
  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0xa8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x60(%rsp),%xmm0
  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0xe0(%rsp)

  xor    %r14,%rax
  paddq  .LK512_14(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 30)
  mov    %r10,%rcx
  movdqa 0xe0(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x78(%rsp),%xmm5
  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0xb8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x70(%rsp),%xmm0
  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0xf0(%rsp)

  xor    %r12,%rax
  paddq  .LK512_15(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 32)
  mov    %rbx,%rcx
  movdqa 0xf0(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x88(%rsp),%xmm5

  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0xc8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x80(%rsp),%xmm0

  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x100(%rsp)

  xor    %r10,%rax
  paddq  .LK512_16(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 34)
  mov    %r14,%rcx
  movdqa 0x100(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x98(%rsp),%xmm5

  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0xd8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x90(%rsp),%xmm0

  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x110(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_17(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 36)
  mov    %r12,%rcx
  movdqa 0x110(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0xa8(%rsp),%xmm5

  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0xe8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0xa0(%rsp),%xmm0

  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x120(%rsp)

  xor    %r14,%rax
  paddq  .LK512_18(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 38)
  mov    %r10,%rcx
  movdqa 0x120(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0xb8(%rsp),%xmm5

  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0xf8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0xb0(%rsp),%xmm0

  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x130(%rsp)

  xor    %r12,%rax
  paddq  .LK512_19(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 40)
  mov    %rbx,%rcx
  movdqa 0x130(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0xc8(%rsp),%xmm5

  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x108(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0xc0(%rsp),%xmm0

  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x140(%rsp)

  xor    %r10,%rax
  paddq  .LK512_20(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 42)
  mov    %r14,%rcx
  movdqa 0x140(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0xd8(%rsp),%xmm5

  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x118(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0xd0(%rsp),%xmm0

  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x150(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_21(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 44)
  mov    %r12,%rcx
  movdqa 0x150(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0xe8(%rsp),%xmm5

  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0x128(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0xe0(%rsp),%xmm0

  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x160(%rsp)

  xor    %r14,%rax
  paddq  .LK512_22(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 46)
  mov    %r10,%rcx
  movdqa 0x160(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0xf8(%rsp),%xmm5

  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0x138(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0xf0(%rsp),%xmm0

  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x170(%rsp)

  xor    %r12,%rax
  paddq  .LK512_23(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 48)
  mov    %rbx,%rcx
  movdqa 0x170(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x108(%rsp),%xmm5

  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x148(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x100(%rsp),%xmm0

  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x180(%rsp)

  xor    %r10,%rax
  paddq  .LK512_24(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 50)
  mov    %r14,%rcx
  movdqa 0x180(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x118(%rsp),%xmm5

  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x158(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x110(%rsp),%xmm0

  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x190(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_25(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 52)
  mov    %r12,%rcx
  movdqa 0x190(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x128(%rsp),%xmm5

  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0x168(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x120(%rsp),%xmm0

  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x1a0(%rsp)

  xor    %r14,%rax
  paddq  .LK512_26(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 54)
  mov    %r10,%rcx
  movdqa 0x1a0(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x138(%rsp),%xmm5

  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0x178(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x130(%rsp),%xmm0

  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x1b0(%rsp)

  xor    %r12,%rax
  paddq  .LK512_27(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 56)
  mov    %rbx,%rcx
  movdqa 0x1b0(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x148(%rsp),%xmm5

  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x188(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x140(%rsp),%xmm0

  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x1c0(%rsp)

  xor    %r10,%rax
  paddq  .LK512_28(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 58)
  mov    %r14,%rcx
  movdqa 0x1c0(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x158(%rsp),%xmm5

  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x198(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x150(%rsp),%xmm0

  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x1d0(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_29(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 60)
  mov    %r12,%rcx
  movdqa 0x1d0(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x168(%rsp),%xmm5

  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0x1a8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x160(%rsp),%xmm0

  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x1e0(%rsp)

  xor    %r14,%rax
  paddq  .LK512_30(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 62)
  mov    %r10,%rcx
  movdqa 0x1e0(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x178(%rsp),%xmm5

  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0x1b8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x170(%rsp),%xmm0

  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x1f0(%rsp)

  xor    %r12,%rax
  paddq  .LK512_31(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 64)
  mov    %rbx,%rcx
  movdqa 0x1f0(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x188(%rsp),%xmm5

  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x1c8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x180(%rsp),%xmm0

  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x200(%rsp)

  xor    %r10,%rax
  paddq  .LK512_32(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 66)
  mov    %r14,%rcx
  movdqa 0x200(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x198(%rsp),%xmm5

  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x1d8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x190(%rsp),%xmm0

  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x210(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_33(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 68)
  mov    %r12,%rcx
  movdqa 0x210(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x1a8(%rsp),%xmm5

  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0x1e8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x1a0(%rsp),%xmm0

  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x220(%rsp)

  xor    %r14,%rax
  paddq  .LK512_34(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 70)
  mov    %r10,%rcx
  movdqa 0x220(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x1b8(%rsp),%xmm5

  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0x1f8(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x1b0(%rsp),%xmm0

  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x230(%rsp)

  xor    %r12,%rax
  paddq  .LK512_35(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //.elseif t < 79 (t == 72)
  mov    %rbx,%rcx
  movdqa 0x230(%rsp),%xmm2

  xor    %r9,%rcx
  and    %r15,%rcx
  movdqa %xmm2,%xmm0
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x1c8(%rsp),%xmm5

  mov    %r15,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r15,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r10,%rcx
  pxor   %xmm2,%xmm0
  mov    %r11,%r8
  xor    %r13,%r8
  pxor   %xmm5,%xmm3
  and    %r12,%r8
  mov    %r11,%rax
  psrlq  $0xd,%xmm0
  and    %r13,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r11,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r11,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r14
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r10
  movdqa %xmm2,%xmm1
  mov    %r15,%rcx
  xor    %rbx,%rcx
  movdqa %xmm5,%xmm4
  and    %r14,%rcx
  xor    %rbx,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r14,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r14,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r14,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r9,%rcx
  mov    %r10,%r8
  psllq  $0x38,%xmm4
  xor    %r12,%r8
  and    %r11,%r8
  pxor   %xmm1,%xmm0
  mov    %r10,%rax
  and    %r12,%rax
  movdqu 0x208(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r10,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x1c0(%rsp),%xmm0

  xor    %r10,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x240(%rsp)

  xor    %r10,%rax
  paddq  .LK512_36(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r13
  lea    (%rcx,%r8,1),%r9

  //.elseif t < 79 (t == 74)
  mov    %r14,%rcx
  movdqa 0x240(%rsp),%xmm2

  xor    %r15,%rcx
  and    %r13,%rcx
  movdqa %xmm2,%xmm0
  xor    %r15,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x1d8(%rsp),%xmm5

  mov    %r13,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r13,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %rbx,%rcx
  pxor   %xmm2,%xmm0
  mov    %r9,%r8
  xor    %r11,%r8
  pxor   %xmm5,%xmm3
  and    %r10,%r8
  mov    %r9,%rax
  psrlq  $0xd,%xmm0
  and    %r11,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r9,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r9,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r12
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%rbx
  movdqa %xmm2,%xmm1
  mov    %r13,%rcx
  xor    %r14,%rcx
  movdqa %xmm5,%xmm4
  and    %r12,%rcx
  xor    %r14,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r12,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r12,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r12,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r15,%rcx
  mov    %rbx,%r8
  psllq  $0x38,%xmm4
  xor    %r10,%r8
  and    %r9,%r8
  pxor   %xmm1,%xmm0
  mov    %rbx,%rax
  and    %r10,%rax
  movdqu 0x218(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %rbx,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x1d0(%rsp),%xmm0

  xor    %rbx,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x250(%rsp)

  xor    %rbx,%rax
  paddq  .LK512_37(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r11
  lea    (%rcx,%r8,1),%r15

  //.elseif t < 79 (t == 76)
  mov    %r12,%rcx
  movdqa 0x250(%rsp),%xmm2

  xor    %r13,%rcx
  and    %r11,%rcx
  movdqa %xmm2,%xmm0
  xor    %r13,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x1e8(%rsp),%xmm5

  mov    %r11,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r11,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r11,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r14,%rcx
  pxor   %xmm2,%xmm0
  mov    %r15,%r8
  xor    %r9,%r8
  pxor   %xmm5,%xmm3
  and    %rbx,%r8
  mov    %r15,%rax
  psrlq  $0xd,%xmm0
  and    %r9,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r15,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r15,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r15,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%r10
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r14
  movdqa %xmm2,%xmm1
  mov    %r11,%rcx
  xor    %r12,%rcx
  movdqa %xmm5,%xmm4
  and    %r10,%rcx
  xor    %r12,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %r10,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %r10,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %r10,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r13,%rcx
  mov    %r14,%r8
  psllq  $0x38,%xmm4
  xor    %rbx,%r8
  and    %r15,%r8
  pxor   %xmm1,%xmm0
  mov    %r14,%rax
  and    %rbx,%rax
  movdqu 0x228(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r14,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x1e0(%rsp),%xmm0

  xor    %r14,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x260(%rsp)

  xor    %r14,%rax
  paddq  .LK512_38(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r9
  lea    (%rcx,%r8,1),%r13

  //.elseif t < 79 (t == 78)
  mov    %r10,%rcx
  movdqa 0x260(%rsp),%xmm2

  xor    %r11,%rcx
  and    %r9,%rcx
  movdqa %xmm2,%xmm0
  xor    %r11,%rcx
  add    0x280(%rsp),%rcx

  movdqu 0x1f8(%rsp),%xmm5

  mov    %r9,%rax
  ror    $0x17,%rax
  movdqa %xmm5,%xmm3
  xor    %r9,%rax
  ror    $0x4,%rax
  psrlq  $0x2a,%xmm0
  xor    %r9,%rax
  ror    $0xe,%rax
  psrlq  $0x1,%xmm3
  add    %rax,%rcx
  add    %r12,%rcx
  pxor   %xmm2,%xmm0
  mov    %r13,%r8
  xor    %r15,%r8
  pxor   %xmm5,%xmm3
  and    %r14,%r8
  mov    %r13,%rax
  psrlq  $0xd,%xmm0
  and    %r15,%rax
  xor    %rax,%r8
  psrlq  $0x6,%xmm3
  mov    %r13,%rax
  ror    $0x5,%rax
  pxor   %xmm2,%xmm0
  xor    %r13,%rax
  ror    $0x6,%rax
  pxor   %xmm5,%xmm3
  xor    %r13,%rax
  ror    $0x1c,%rax
  psrlq  $0x6,%xmm0
  add    %rax,%r8
  add    %rcx,%rbx
  psrlq  $0x1,%xmm3
  lea    (%rcx,%r8,1),%r12
  movdqa %xmm2,%xmm1
  mov    %r9,%rcx
  xor    %r10,%rcx
  movdqa %xmm5,%xmm4
  and    %rbx,%rcx
  xor    %r10,%rcx
  psllq  $0x2a,%xmm1
  add    0x288(%rsp),%rcx

  mov    %rbx,%rax
  psllq  $0x7,%xmm4
  ror    $0x17,%rax
  xor    %rbx,%rax
  pxor   %xmm2,%xmm1
  ror    $0x4,%rax
  xor    %rbx,%rax
  pxor   %xmm5,%xmm4
  ror    $0xe,%rax
  add    %rax,%rcx
  psllq  $0x3,%xmm1
  add    %r11,%rcx
  mov    %r12,%r8
  psllq  $0x38,%xmm4
  xor    %r14,%r8
  and    %r13,%r8
  pxor   %xmm1,%xmm0
  mov    %r12,%rax
  and    %r14,%rax
  movdqu 0x238(%rsp),%xmm1

  xor    %rax,%r8
  pxor   %xmm4,%xmm3
  mov    %r12,%rax
  paddq  %xmm3,%xmm0
  ror    $0x5,%rax
  paddq  0x1f0(%rsp),%xmm0

  xor    %r12,%rax
  paddq  %xmm1,%xmm0
  ror    $0x6,%rax
  movdqa %xmm0,0x270(%rsp)

  xor    %r12,%rax
  paddq  .LK512_39(%rip),%xmm0

  ror    $0x1c,%rax
  movdqa %xmm0,0x280(%rsp)

  add    %rax,%r8
  add    %rcx,%r15
  lea    (%rcx,%r8,1),%r11

  //* Compute 2 Rounds */

  // SHA512_Round (t - 2)
  mov    %rbx,%rcx
  mov    %r15,%rax
  xor    %r9,%rcx
  ror    $0x17,%rax
  and    %r15,%rcx
  xor    %r15,%rax
  xor    %r9,%rcx
  add    0x280(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r15,%rax
  mov    %r11,%r8
  add    %r10,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r11,%rax
  xor    %r13,%r8
  and    %r13,%rax
  and    %r12,%r8
  xor    %rax,%r8
  mov    %r11,%rax
  ror    $0x5,%rax
  xor    %r11,%rax
  add    %rcx,%r14
  ror    $0x6,%rax
  xor    %r11,%rax
  lea    (%rcx,%r8,1),%r10
  ror    $0x1c,%rax
  add    %rax,%r10

  // SHA512_Round (t - 1)
  mov    %r15,%rcx
  mov    %r14,%rax
  xor    %rbx,%rcx
  ror    $0x17,%rax
  and    %r14,%rcx
  xor    %r14,%rax
  xor    %rbx,%rcx
  add    0x288(%rsp),%rcx

  ror    $0x4,%rax
  xor    %r14,%rax
  mov    %r10,%r8
  add    %r9,%rcx
  ror    $0xe,%rax
  add    %rax,%rcx
  mov    %r10,%rax
  xor    %r12,%r8
  and    %r12,%rax
  and    %r11,%r8
  xor    %rax,%r8
  mov    %r10,%rax
  ror    $0x5,%rax
  xor    %r10,%rax
  add    %rcx,%r13
  ror    $0x6,%rax
  xor    %r10,%rax
  lea    (%rcx,%r8,1),%r9
  ror    $0x1c,%rax
  add    %rax,%r9

  //* Update digest */
  add    %r9,(%rsi)
  add    %r10,0x8(%rsi)
  add    %r11,0x10(%rsi)
  add    %r12,0x18(%rsi)
  add    %r13,0x20(%rsi)
  add    %r14,0x28(%rsi)
  add    %r15,0x30(%rsi)
  add    %rbx,0x38(%rsi)

  //* Advance to next message block */
  add    $0x80,%rdi
  dec    %rdx
  jne    .Lupdateblock

  //* Restore GPRs */
  mov    0x290(%rsp),%rbx
  mov    0x298(%rsp),%r12
  mov    0x2a0(%rsp),%r13
  mov    0x2a8(%rsp),%r14
  mov    0x2b0(%rsp),%r15

  //* Restore Stack Pointer */
  add    $0x2b8,%rsp

  pxor   %xmm0,%xmm0
  pxor   %xmm1,%xmm1
  pxor   %xmm2,%xmm2
  pxor   %xmm3,%xmm3
  pxor   %xmm4,%xmm4
  pxor   %xmm5,%xmm5

  //* Return stack burn depth */
  mov    $0x2b8,%rax

.Lnowork:
{$IF DEFINED(WIN64)}
  popq   %rdi
  popq   %rsi
{$ENDIF}
  retq

//*
// Binary Data
//*

  .balign 16

//* Mask for byte-swapping a couple of qwords in an XMM register using (v)pshufb. */
.LXMM_QWORD_BSWAP:
  .quad 0x0001020304050607, 0x08090a0b0c0d0e0f

//* K[t] used in SHA512 hashing */
.LK512_0:
  .quad 0x428a2f98d728ae22,0x7137449123ef65cd
.LK512_1:
  .quad 0xb5c0fbcfec4d3b2f,0xe9b5dba58189dbbc
.LK512_2:
  .quad 0x3956c25bf348b538,0x59f111f1b605d019
.LK512_3:
  .quad 0x923f82a4af194f9b,0xab1c5ed5da6d8118
.LK512_4:
  .quad 0xd807aa98a3030242,0x12835b0145706fbe
.LK512_5:
  .quad 0x243185be4ee4b28c,0x550c7dc3d5ffb4e2
.LK512_6:
  .quad 0x72be5d74f27b896f,0x80deb1fe3b1696b1
.LK512_7:
  .quad 0x9bdc06a725c71235,0xc19bf174cf692694
.LK512_8:
  .quad 0xe49b69c19ef14ad2,0xefbe4786384f25e3
.LK512_9:
  .quad 0x0fc19dc68b8cd5b5,0x240ca1cc77ac9c65
.LK512_10:
  .quad 0x2de92c6f592b0275,0x4a7484aa6ea6e483
.LK512_11:
  .quad 0x5cb0a9dcbd41fbd4,0x76f988da831153b5
.LK512_12:
  .quad 0x983e5152ee66dfab,0xa831c66d2db43210
.LK512_13:
  .quad 0xb00327c898fb213f,0xbf597fc7beef0ee4
.LK512_14:
  .quad 0xc6e00bf33da88fc2,0xd5a79147930aa725
.LK512_15:
  .quad 0x06ca6351e003826f,0x142929670a0e6e70
.LK512_16:
  .quad 0x27b70a8546d22ffc,0x2e1b21385c26c926
.LK512_17:
  .quad 0x4d2c6dfc5ac42aed,0x53380d139d95b3df
.LK512_18:
  .quad 0x650a73548baf63de,0x766a0abb3c77b2a8
.LK512_19:
  .quad 0x81c2c92e47edaee6,0x92722c851482353b
.LK512_20:
  .quad 0xa2bfe8a14cf10364,0xa81a664bbc423001
.LK512_21:
  .quad 0xc24b8b70d0f89791,0xc76c51a30654be30
.LK512_22:
  .quad 0xd192e819d6ef5218,0xd69906245565a910
.LK512_23:
  .quad 0xf40e35855771202a,0x106aa07032bbd1b8
.LK512_24:
  .quad 0x19a4c116b8d2d0c8,0x1e376c085141ab53
.LK512_25:
  .quad 0x2748774cdf8eeb99,0x34b0bcb5e19b48a8
.LK512_26:
  .quad 0x391c0cb3c5c95a63,0x4ed8aa4ae3418acb
.LK512_27:
  .quad 0x5b9cca4f7763e373,0x682e6ff3d6b2b8a3
.LK512_28:
  .quad 0x748f82ee5defb2fc,0x78a5636f43172f60
.LK512_29:
  .quad 0x84c87814a1f0ab72,0x8cc702081a6439ec
.LK512_30:
  .quad 0x90befffa23631e28,0xa4506cebde82bde9
.LK512_31:
  .quad 0xbef9a3f7b2c67915,0xc67178f2e372532b
.LK512_32:
  .quad 0xca273eceea26619c,0xd186b8c721c0c207
.LK512_33:
  .quad 0xeada7dd6cde0eb1e,0xf57d4f7fee6ed178
.LK512_34:
  .quad 0x06f067aa72176fba,0x0a637dc5a2c898a6
.LK512_35:
  .quad 0x113f9804bef90dae,0x1b710b35131c471b
.LK512_36:
  .quad 0x28db77f523047d84,0x32caab7b40c72493
.LK512_37:
  .quad 0x3c9ebe0a15c9bebc,0x431d67c49c100d4c
.LK512_38:
  .quad 0x4cc5d4becb3e42b6,0x597f299cfc657e2a
.LK512_39:
  .quad 0x5fcb6fab3ad6faec,0x6c44198c4a475817
end;

