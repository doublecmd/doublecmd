{
   The eXtended Keccak Code Package (XKCP)
   https://github.com/XKCP/XKCP

   Copyright (c) 2006-2017, CRYPTOGAMS by <appro@openssl.org>
   Copyright (c) 2017 Ronny Van Keer
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

    * Redistributions of source code must retain copyright notices,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials
      provided with the distribution.

    * Neither the name of the CRYPTOGAMS nor the names of its
      copyright holder and contributors may be used to endorse or
      promote products derived from this software without specific
      prior written permission.

   ALTERNATIVELY, provided that this notice is retained in full, this
   product may be distributed under the terms of the GNU General Public
   License (GPL), in which case the provisions of the GPL apply INSTEAD OF
   those given above.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   Notes:
   The code for the permutation was generated with
   Andy Polyakov's keccak1600-avx2.pl from the CRYPTOGAMS project
   (https://github.com/dot-asm/cryptogams/blob/master/x86_64/keccak1600-avx2.pl).
   The rest of the code was written by Ronny Van Keer.
   Adaptations for macOS by Stéphane Léon.
   Pascal translation by Alexander Koblov.
}

{$CODEALIGN CONSTMIN=64}

const
  mapState: array[0..24] of UInt64 = (
    0*8,  1*8,  2*8,  3*8,  4*8,
    7*8, 21*8, 10*8, 15*8, 20*8,
    5*8, 13*8, 22*8, 19*8, 12*8,
    8*8,  9*8, 18*8, 23*8, 16*8,
    6*8, 17*8, 14*8, 11*8, 24*8
  );

procedure KeccakPermutationAVX(var state: TState_L); assembler; nostackframe;
// UNIX    RDI
// WIN64:  RCX
asm
.balign 32
{$IF DEFINED(WIN64)}
  xchg   %rcx, %rdi
  subq   $168, %rsp
  movdqa %xmm6, (%rsp)
  movdqa %xmm7, 16(%rsp)
  movdqa %xmm8, 32(%rsp)
  movdqa %xmm9, 48(%rsp)
  movdqa %xmm10, 64(%rsp)
  movdqa %xmm11, 80(%rsp)
  movdqa %xmm12, 96(%rsp)
  movdqa %xmm13, 112(%rsp)
  movdqa %xmm14, 128(%rsp)
  movdqa %xmm15, 144(%rsp)
{$ENDIF}
  lea             .LKrhotates_left+96(%rip),%r8
  lea             .LKrhotates_right+96(%rip),%r9
  lea             .LKiotas(%rip),%r10
  mov             $24,%eax
  lea             96(%rdi),%rdi
  vzeroupper
  vpbroadcastq    -96(%rdi),%ymm0         // load A[5][5]
  vmovdqu         8+32*0-96(%rdi),%ymm1
  vmovdqu         8+32*1-96(%rdi),%ymm2
  vmovdqu         8+32*2-96(%rdi),%ymm3
  vmovdqu         8+32*3-96(%rdi),%ymm4
  vmovdqu         8+32*4-96(%rdi),%ymm5
  vmovdqu         8+32*5-96(%rdi),%ymm6
  // -----------------------------------------
.Loop_avx2:
  // ######################################### Theta
  vpshufd     $0b01001110,%ymm2,%ymm13
  vpxor       %ymm3,%ymm5,%ymm12
  vpxor       %ymm6,%ymm4,%ymm9
  vpxor       %ymm1,%ymm12,%ymm12
  vpxor       %ymm9,%ymm12,%ymm12         // C[1..4]

  vpermq      $0b10010011,%ymm12,%ymm11
  vpxor       %ymm2,%ymm13,%ymm13
  vpermq      $0b01001110,%ymm13,%ymm7

  vpsrlq      $63,%ymm12,%ymm8
  vpaddq      %ymm12,%ymm12,%ymm9
  vpor        %ymm9,%ymm8,%ymm8           // ROL64(C[1..4],1)

  vpermq      $0b00111001,%ymm8,%ymm15
  vpxor       %ymm11,%ymm8,%ymm14
  vpermq      $0b00000000,%ymm14,%ymm14   // D[0..0] = ROL64(C[1],1) ^ C[4]

  vpxor       %ymm0,%ymm13,%ymm13
  vpxor       %ymm7,%ymm13,%ymm13         // C[0..0]

  vpsrlq      $63,%ymm13,%ymm7
  vpaddq      %ymm13,%ymm13,%ymm8
  vpor        %ymm7,%ymm8,%ymm8           // ROL64(C[0..0],1)

  vpxor       %ymm14,%ymm2,%ymm2          // ^= D[0..0]
  vpxor       %ymm14,%ymm0,%ymm0          // ^= D[0..0]

  vpblendd    $0b11000000,%ymm8,%ymm15,%ymm15
  vpblendd    $0b00000011,%ymm13,%ymm11,%ymm11
  vpxor       %ymm11,%ymm15,%ymm15        // D[1..4] = ROL64(C[2..4,0),1) ^ C[0..3]

  // ######################################### Rho + Pi + pre-Chi shuffle
  vpsllvq     0*32-96(%r8),%ymm2,%ymm10
  vpsrlvq     0*32-96(%r9),%ymm2,%ymm2
  vpor        %ymm10,%ymm2,%ymm2

  vpxor       %ymm15,%ymm3,%ymm3          // ^= D[1..4] from Theta
  vpsllvq     2*32-96(%r8),%ymm3,%ymm11
  vpsrlvq     2*32-96(%r9),%ymm3,%ymm3
  vpor        %ymm11,%ymm3,%ymm3

  vpxor       %ymm15,%ymm4,%ymm4          // ^= D[1..4] from Theta
  vpsllvq     3*32-96(%r8),%ymm4,%ymm12
  vpsrlvq     3*32-96(%r9),%ymm4,%ymm4
  vpor        %ymm12,%ymm4,%ymm4

  vpxor       %ymm15,%ymm5,%ymm5          // ^= D[1..4] from Theta
  vpsllvq     4*32-96(%r8),%ymm5,%ymm13
  vpsrlvq     4*32-96(%r9),%ymm5,%ymm5
  vpor        %ymm13,%ymm5,%ymm5

  vpxor       %ymm15,%ymm6,%ymm6          // ^= D[1..4] from Theta
  vpermq      $0b10001101,%ymm2,%ymm10    // %ymm2 -> future %ymm3
  vpermq      $0b10001101,%ymm3,%ymm11    // %ymm3 -> future %ymm4
  vpsllvq     5*32-96(%r8),%ymm6,%ymm14
  vpsrlvq     5*32-96(%r9),%ymm6,%ymm8
  vpor        %ymm14,%ymm8,%ymm8          // %ymm6 -> future %ymm1

  vpxor       %ymm15,%ymm1,%ymm1          // ^= D[1..4] from Theta
  vpermq      $0b00011011,%ymm4,%ymm12    // %ymm4 -> future %ymm5
  vpermq      $0b01110010,%ymm5,%ymm13    // %ymm5 -> future %ymm6
  vpsllvq     1*32-96(%r8),%ymm1,%ymm15
  vpsrlvq     1*32-96(%r9),%ymm1,%ymm9
  vpor        %ymm15,%ymm9,%ymm9          // %ymm1 -> future %ymm2

  // ######################################### Chi
  vpsrldq     $8,%ymm8,%ymm14
  vpandn      %ymm14,%ymm8,%ymm7                  // tgting  [0][0] [0][0] [0][0] [0][0]

  vpblendd    $0b00001100,%ymm13,%ymm9,%ymm3      //               [4][4] [2][0]
  vpblendd    $0b00001100,%ymm9,%ymm11,%ymm15     //               [4][0] [2][1]
  vpblendd    $0b00001100,%ymm11,%ymm10,%ymm5     //               [4][2] [2][4]
  vpblendd    $0b00001100,%ymm10,%ymm9,%ymm14     //               [4][3] [2][0]
  vpblendd    $0b00110000,%ymm11,%ymm3,%ymm3      //        [1][3] [4][4] [2][0]
  vpblendd    $0b00110000,%ymm12,%ymm15,%ymm15    //        [1][4] [4][0] [2][1]
  vpblendd    $0b00110000,%ymm9,%ymm5,%ymm5       //        [1][0] [4][2] [2][4]
  vpblendd    $0b00110000,%ymm13,%ymm14,%ymm14    //        [1][1] [4][3] [2][0]
  vpblendd    $0b11000000,%ymm12,%ymm3,%ymm3      // [3][2] [1][3] [4][4] [2][0]
  vpblendd    $0b11000000,%ymm13,%ymm15,%ymm15    // [3][3] [1][4] [4][0] [2][1]
  vpblendd    $0b11000000,%ymm13,%ymm5,%ymm5      // [3][3] [1][0] [4][2] [2][4]
  vpblendd    $0b11000000,%ymm11,%ymm14,%ymm14    // [3][4] [1][1] [4][3] [2][0]
  vpandn      %ymm15,%ymm3,%ymm3                  // tgting  [3][1] [1][2] [4][3] [2][4]
  vpandn      %ymm14,%ymm5,%ymm5                  // tgting  [3][2] [1][4] [4][1] [2][3]

  vpblendd    $0b00001100,%ymm9,%ymm12,%ymm6      //               [4][0] [2][3]
  vpblendd    $0b00001100,%ymm12,%ymm10,%ymm15    //               [4][1] [2][4]
  vpxor       %ymm10,%ymm3,%ymm3
  vpblendd    $0b00110000,%ymm10,%ymm6,%ymm6      //        [1][2] [4][0] [2][3]
  vpblendd    $0b00110000,%ymm11,%ymm15,%ymm15    //        [1][3] [4][1] [2][4]
  vpxor       %ymm12,%ymm5,%ymm5
  vpblendd    $0b11000000,%ymm11,%ymm6,%ymm6      // [3][4] [1][2] [4][0] [2][3]
  vpblendd    $0b11000000,%ymm9,%ymm15,%ymm15     // [3][0] [1][3] [4][1] [2][4]
  vpandn      %ymm15,%ymm6,%ymm6                  // tgting  [3][3] [1][1] [4][4] [2][2]
  vpxor       %ymm13,%ymm6,%ymm6

  vpermq      $0b00011110,%ymm8,%ymm4             // [0][1] [0][2] [0][4] [0][3]
  vpblendd    $0b00110000,%ymm0,%ymm4,%ymm15      // [0][1] [0][0] [0][4] [0][3]
  vpermq      $0b00111001,%ymm8,%ymm1             // [0][1] [0][4] [0][3] [0][2]
  vpblendd    $0b11000000,%ymm0,%ymm1,%ymm1       // [0][0] [0][4] [0][3] [0][2]
  vpandn      %ymm15,%ymm1,%ymm1                  // tgting  [0][4] [0][3] [0][2] [0][1]

  vpblendd    $0b00001100,%ymm12,%ymm11,%ymm2     //               [4][1] [2][1]
  vpblendd    $0b00001100,%ymm11,%ymm13,%ymm14    //               [4][2] [2][2]
  vpblendd    $0b00110000,%ymm13,%ymm2,%ymm2      //        [1][1] [4][1] [2][1]
  vpblendd    $0b00110000,%ymm10,%ymm14,%ymm14    //        [1][2] [4][2] [2][2]
  vpblendd    $0b11000000,%ymm10,%ymm2,%ymm2      // [3][1] [1][1] [4][1] [2][1]
  vpblendd    $0b11000000,%ymm12,%ymm14,%ymm14    // [3][2] [1][2] [4][2] [2][2]
  vpandn      %ymm14,%ymm2,%ymm2                  // tgting  [3][0] [1][0] [4][0] [2][0]
  vpxor       %ymm9,%ymm2,%ymm2

  vpermq      $0b00000000,%ymm7,%ymm7             // [0][0] [0][0] [0][0] [0][0]
  vpermq      $0b00011011,%ymm3,%ymm3             // post-Chi shuffle
  vpermq      $0b10001101,%ymm5,%ymm5
  vpermq      $0b01110010,%ymm6,%ymm6

  vpblendd    $0b00001100,%ymm10,%ymm13,%ymm4     //               [4][3] [2][2]
  vpblendd    $0b00001100,%ymm13,%ymm12,%ymm14    //               [4][4] [2][3]
  vpblendd    $0b00110000,%ymm12,%ymm4,%ymm4      //        [1][4] [4][3] [2][2]
  vpblendd    $0b00110000,%ymm9,%ymm14,%ymm14     //        [1][0] [4][4] [2][3]
  vpblendd    $0b11000000,%ymm9,%ymm4,%ymm4       // [3][0] [1][4] [4][3] [2][2]
  vpblendd    $0b11000000,%ymm10,%ymm14,%ymm14    // [3][1] [1][0] [4][4] [2][3]
  vpandn      %ymm14,%ymm4,%ymm4                  // tgting  [3][4] [1][3] [4][2] [2][1]

  vpxor       %ymm7,%ymm0,%ymm0
  vpxor       %ymm8,%ymm1,%ymm1
  vpxor       %ymm11,%ymm4,%ymm4

  // ######################################### Iota
  vpxor       (%r10),%ymm0,%ymm0
  lea         32(%r10),%r10

  dec         %eax
  jnz         .Loop_avx2
  // -----------------------------------------
  vmovq           %xmm0,-96(%rdi)
  vmovdqu         %ymm1,8+32*0-96(%rdi)
  vmovdqu         %ymm2,8+32*1-96(%rdi)
  vmovdqu         %ymm3,8+32*2-96(%rdi)
  vmovdqu         %ymm4,8+32*3-96(%rdi)
  vmovdqu         %ymm5,8+32*4-96(%rdi)
  vmovdqu         %ymm6,8+32*5-96(%rdi)
  vzeroupper
{$IF DEFINED(WIN64)}
  movdqa  (%rsp),%xmm6
  movdqa  0x10(%rsp),%xmm7
  movdqa  0x20(%rsp),%xmm8
  movdqa  0x30(%rsp),%xmm9
  movdqa  0x40(%rsp),%xmm10
  movdqa  0x50(%rsp),%xmm11
  movdqa  0x60(%rsp),%xmm12
  movdqa  0x70(%rsp),%xmm13
  movdqa  0x80(%rsp),%xmm14
  movdqa  0x90(%rsp),%xmm15
  addq    $168, %rsp
  movq    %rcx, %rdi
{$ENDIF}
  retq
.balign 64
.LKrhotates_left:
  .quad     3,   18,    36,    41         // [2][0] [4][0] [1][0] [3][0]
  .quad     1,   62,    28,    27         // [0][1] [0][2] [0][3] [0][4]
  .quad    45,    6,    56,    39         // [3][1] [1][2] [4][3] [2][4]
  .quad    10,   61,    55,     8         // [2][1] [4][2] [1][3] [3][4]
  .quad     2,   15,    25,    20         // [4][1] [3][2] [2][3] [1][4]
  .quad    44,   43,    21,    14         // [1][1] [2][2] [3][3] [4][4]
.LKrhotates_right:
  .quad    64-3,  64-18,  64-36,  64-41
  .quad    64-1,  64-62,  64-28,  64-27
  .quad    64-45, 64-6,   64-56,  64-39
  .quad    64-10, 64-61,  64-55,  64-8
  .quad    64-2,  64-15,  64-25,  64-20
  .quad    64-44, 64-43,  64-21,  64-14
.LKiotas:
  .quad    0x0000000000000001, 0x0000000000000001, 0x0000000000000001, 0x0000000000000001
  .quad    0x0000000000008082, 0x0000000000008082, 0x0000000000008082, 0x0000000000008082
  .quad    0x800000000000808a, 0x800000000000808a, 0x800000000000808a, 0x800000000000808a
  .quad    0x8000000080008000, 0x8000000080008000, 0x8000000080008000, 0x8000000080008000
  .quad    0x000000000000808b, 0x000000000000808b, 0x000000000000808b, 0x000000000000808b
  .quad    0x0000000080000001, 0x0000000080000001, 0x0000000080000001, 0x0000000080000001
  .quad    0x8000000080008081, 0x8000000080008081, 0x8000000080008081, 0x8000000080008081
  .quad    0x8000000000008009, 0x8000000000008009, 0x8000000000008009, 0x8000000000008009
  .quad    0x000000000000008a, 0x000000000000008a, 0x000000000000008a, 0x000000000000008a
  .quad    0x0000000000000088, 0x0000000000000088, 0x0000000000000088, 0x0000000000000088
  .quad    0x0000000080008009, 0x0000000080008009, 0x0000000080008009, 0x0000000080008009
  .quad    0x000000008000000a, 0x000000008000000a, 0x000000008000000a, 0x000000008000000a
  .quad    0x000000008000808b, 0x000000008000808b, 0x000000008000808b, 0x000000008000808b
  .quad    0x800000000000008b, 0x800000000000008b, 0x800000000000008b, 0x800000000000008b
  .quad    0x8000000000008089, 0x8000000000008089, 0x8000000000008089, 0x8000000000008089
  .quad    0x8000000000008003, 0x8000000000008003, 0x8000000000008003, 0x8000000000008003
  .quad    0x8000000000008002, 0x8000000000008002, 0x8000000000008002, 0x8000000000008002
  .quad    0x8000000000000080, 0x8000000000000080, 0x8000000000000080, 0x8000000000000080
  .quad    0x000000000000800a, 0x000000000000800a, 0x000000000000800a, 0x000000000000800a
  .quad    0x800000008000000a, 0x800000008000000a, 0x800000008000000a, 0x800000008000000a
  .quad    0x8000000080008081, 0x8000000080008081, 0x8000000080008081, 0x8000000080008081
  .quad    0x8000000000008080, 0x8000000000008080, 0x8000000000008080, 0x8000000000008080
  .quad    0x0000000080000001, 0x0000000080000001, 0x0000000080000001, 0x0000000080000001
  .quad    0x8000000080008008, 0x8000000080008008, 0x8000000080008008, 0x8000000080008008
end;

procedure KeccakP1600_ExtractBytes(const state: Pointer; data: PByte; offset, length: Cardinal); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX
// WIN64:  RCX, RDX, R8,  R9
asm
.balign 32
{$IF DEFINED(WIN64)}
  mov         %rsi, 16(%rsp)
  mov         %rdi, 24(%rsp)
  mov         %rcx, %rdi
  mov         %rdx, %rsi
  mov         %r8, %rdx
  mov         %r9, %rcx
{$ENDIF}
  push        %rbx
  cmp         $0, %rcx
  jz          .LKeccakP1600_ExtractBytes_Exit
  mov         %rdx, %rax                              // rax offset in lane
  and         $0xFFFFFFF8, %edx                       // rdx pointer into state index mapper
  lea         mapState(%rip), %r9
  add         %r9, %rdx
  and         $7, %rax
  jz          .LKeccakP1600_ExtractBytes_LaneAlignedCheck
  mov         $8, %rbx                                // rbx is (max) length of incomplete lane
  sub         %rax, %rbx
  cmp         %rcx, %rbx
  cmovae      %rcx, %rbx
  sub         %rbx, %rcx                              // length -= length of incomplete lane
  mov         (%rdx), %r9
  add         $8, %rdx
  add         %rdi, %r9
  add         %rax, %r9
.LKeccakP1600_ExtractBytes_NotAlignedLoop:
  mov         (%r9), %r8b
  inc         %r9
  mov         %r8b, (%rsi)
  inc         %rsi
  dec         %rbx
  jnz         .LKeccakP1600_ExtractBytes_NotAlignedLoop
  jmp         .LKeccakP1600_ExtractBytes_LaneAlignedCheck
.LKeccakP1600_ExtractBytes_LaneAlignedLoop:
  mov         (%rdx), %rax
  add         $8, %rdx
  add         %rdi, %rax
  mov         (%rax), %r8
  mov         %r8, (%rsi)
  add         $8, %rsi
.LKeccakP1600_ExtractBytes_LaneAlignedCheck:
  sub         $8, %rcx
  jnc         .LKeccakP1600_ExtractBytes_LaneAlignedLoop
.LKeccakP1600_ExtractBytes_LastIncompleteLane:
  add         $8, %rcx
  jz          .LKeccakP1600_ExtractBytes_Exit
  mov         (%rdx), %rax
  add         %rdi, %rax
  mov         (%rax), %r8
.LKeccakP1600_ExtractBytes_LastIncompleteLaneLoop:
  mov         %r8b, (%rsi)
  shr         $8, %r8
  inc         %rsi
  dec         %rcx
  jnz         .LKeccakP1600_ExtractBytes_LastIncompleteLaneLoop
.LKeccakP1600_ExtractBytes_Exit:
  pop         %rbx
{$IF DEFINED(WIN64)}
  mov         16(%rsp), %rsi
  mov         24(%rsp), %rdi
{$ENDIF}
end;

procedure KeccakP1600_AddBytes(state: Pointer; const data: PByte; offset, length: Cardinal); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX
// WIN64:  RCX, RDX, R8,  R9
asm
.balign 32
{$IF DEFINED(WIN64)}
  mov         %rsi, 16(%rsp)
  mov         %rdi, 24(%rsp)
  mov         %rcx, %rdi
  mov         %rdx, %rsi
  mov         %r8, %rdx
  mov         %r9, %rcx
{$ENDIF}
  cmp         $0, %rcx
  jz          .LKeccakP1600_AddBytes_Exit
  mov         %rdx, %rax                              // rax offset in lane
  and         $0xFFFFFFF8, %edx                       // rdx pointer into state index mapper
  lea         mapState(%rip), %r9
  add         %r9, %rdx
  and         $7, %rax
  jz          .LKeccakP1600_AddBytes_LaneAlignedCheck
  mov         $8, %r9                                 // r9 is (max) length of incomplete lane
  sub         %rax, %r9
  cmp         %rcx, %r9
  cmovae      %rcx, %r9
  sub         %r9, %rcx                               // length -= length of incomplete lane
  add         (%rdx), %rax                            // rax = pointer to state lane
  add         $8, %rdx
  add         %rdi, %rax
.LKeccakP1600_AddBytes_NotAlignedLoop:
  mov         (%rsi), %r8b
  inc         %rsi
  xorb        %r8b, (%rax)
  inc         %rax
  dec         %r9
  jnz         .LKeccakP1600_AddBytes_NotAlignedLoop
  jmp         .LKeccakP1600_AddBytes_LaneAlignedCheck
.LKeccakP1600_AddBytes_LaneAlignedLoop:
  mov         (%rsi), %r8
  add         $8, %rsi
  mov         (%rdx), %rax
  add         $8, %rdx
  add         %rdi, %rax
  xor         %r8, (%rax)
.LKeccakP1600_AddBytes_LaneAlignedCheck:
  sub         $8, %rcx
  jnc         .LKeccakP1600_AddBytes_LaneAlignedLoop
.LKeccakP1600_AddBytes_LastIncompleteLane:
  add         $8, %rcx
  jz          .LKeccakP1600_AddBytes_Exit
  mov         (%rdx), %rax
  add         %rdi, %rax
.LKeccakP1600_AddBytes_LastIncompleteLaneLoop:
  mov         (%rsi), %r8b
  inc         %rsi
  xor         %r8b, (%rax)
  inc         %rax
  dec         %rcx
  jnz         .LKeccakP1600_AddBytes_LastIncompleteLaneLoop
.LKeccakP1600_AddBytes_Exit:
{$IF DEFINED(WIN64)}
  movq        16(%rsp), %rsi
  movq        24(%rsp), %rdi
{$ENDIF}
end;

{---------------------------------------------------------------------------}
procedure extractFromStateAVX(outp: pointer; const state: TState_L; laneCount: integer);
begin
  KeccakP1600_ExtractBytes(@state[0], outp, 0, laneCount * 8);
end;

{---------------------------------------------------------------------------}
procedure xorIntoStateAVX(var state: TState_L; inp: PLongint; laneCount: integer);
  {-Include input message data bits into the sponge state}
begin
  KeccakP1600_AddBytes(@state[0], PByte(inp), 0, laneCount * 8);
end;
