{$asmmode intel}

const
  ROT8: array[0..15] of cuint8  = (1, 2, 3, 0, 5, 6, 7, 4, 9, 10, 11, 8, 13, 14, 15, 12);
  ROT16: array[0..15] of cuint8 = (2, 3, 0, 1, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13);

procedure blake3_hash_many_sse41(inputs: ppcuint8; num_inputs: csize_t;
                                 blocks: csize_t; const key: pcuint32;
                                 counter: cuint64; increment_counter: boolean32;
                                 flags: cuint8; flags_start: cuint8;
                                 flags_end: cuint8; out_: pcuint8); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX, R8,    R9,    STACK, STACK, STACK, STACK
// WIN64:  RCX, RDX, R8,  R9,  STACK, STACK, STACK, STACK, STACK, STACK
asm
        push    rbp
        mov     rbp, rsp
        push    r15
        push    r14
        push    r13
        push    r12
        push    rbx
        sub     rsp, 360
        and     rsp, $FFFFFFFFFFFFFFC0
{$IF DEFINED(WIN64)}
        sub     rsp, 168
        and     rsp, $FFFFFFFFFFFFFFC0
        movdqa  xmmword ptr [rsp+$170], xmm6
        movdqa  xmmword ptr [rsp+$180], xmm7
        movdqa  xmmword ptr [rsp+$190], xmm8
        movdqa  xmmword ptr [rsp+$1A0], xmm9
        movdqa  xmmword ptr [rsp+$1B0], xmm10
        movdqa  xmmword ptr [rsp+$1C0], xmm11
        movdqa  xmmword ptr [rsp+$1D0], xmm12
        movdqa  xmmword ptr [rsp+$1E0], xmm13
        movdqa  xmmword ptr [rsp+$1F0], xmm14
        movdqa  xmmword ptr [rsp+$200], xmm15
        mov     qword ptr [rbp+16], rsi
        mov     qword ptr [rbp+24], rdi
        mov     rdi, rcx
        mov     rsi, rdx
        mov     rdx, r8
        mov     rcx, r9
        mov     r8, qword ptr [counter]
        movzx   r9, dword ptr [increment_counter]
{$ENDIF}
        neg     r9d
        movd    xmm0, r9d
        pshufd  xmm0, xmm0, $00
        movdqa  xmmword ptr [rsp+$130], xmm0
        movdqa  xmm1, xmm0
        pand    xmm1, xmmword ptr [ADD0+rip]
        pand    xmm0, xmmword ptr [ADD1+rip]
        movdqa  xmmword ptr [rsp+$150], xmm0
        movd    xmm0, r8d
        pshufd  xmm0, xmm0, $00
        paddd   xmm0, xmm1
        movdqa  xmmword ptr [rsp+$110], xmm0
        pxor    xmm0, xmmword ptr [CMP_MSB_MASK+rip]
        pxor    xmm1, xmmword ptr [CMP_MSB_MASK+rip]
        pcmpgtd xmm1, xmm0
        shr     r8, 32
        movd    xmm2, r8d
        pshufd  xmm2, xmm2, $00
        psubd   xmm2, xmm1
        movdqa  xmmword ptr [rsp+$120], xmm2
        mov     rbx, qword ptr [out_]
        mov     r15, rdx
        shl     r15, 6
        movzx   r13d, byte ptr [flags]
        movzx   r12d, byte ptr [flags_end]
        cmp     rsi, 4
        jc      @L03L03
@L00L02:
        movdqu  xmm3, xmmword ptr [rcx]
        pshufd  xmm0, xmm3, $00
        pshufd  xmm1, xmm3, $55
        pshufd  xmm2, xmm3, $AA
        pshufd  xmm3, xmm3, $FF
        movdqu  xmm7, xmmword ptr [rcx+$10]
        pshufd  xmm4, xmm7, $00
        pshufd  xmm5, xmm7, $55
        pshufd  xmm6, xmm7, $AA
        pshufd  xmm7, xmm7, $FF
        mov     r8, qword ptr [rdi]
        mov     r9, qword ptr [rdi+$8]
        mov     r10, qword ptr [rdi+$10]
        mov     r11, qword ptr [rdi+$18]
        movzx   eax, byte ptr [flags_start]
        or      eax, r13d
        xor     edx, edx
@L01L09:
        mov     r14d, eax
        or      eax, r12d
        add     rdx, 64
        cmp     rdx, r15
        cmovne  eax, r14d
        movdqu  xmm8, xmmword ptr [r8+rdx-$40]
        movdqu  xmm9, xmmword ptr [r9+rdx-$40]
        movdqu  xmm10, xmmword ptr [r10+rdx-$40]
        movdqu  xmm11, xmmword ptr [r11+rdx-$40]
        movdqa  xmm12, xmm8
        punpckldq xmm8, xmm9
        punpckhdq xmm12, xmm9
        movdqa  xmm14, xmm10
        punpckldq xmm10, xmm11
        punpckhdq xmm14, xmm11
        movdqa  xmm9, xmm8
        punpcklqdq xmm8, xmm10
        punpckhqdq xmm9, xmm10
        movdqa  xmm13, xmm12
        punpcklqdq xmm12, xmm14
        punpckhqdq xmm13, xmm14
        movdqa  xmmword ptr [rsp], xmm8
        movdqa  xmmword ptr [rsp+$10], xmm9
        movdqa  xmmword ptr [rsp+$20], xmm12
        movdqa  xmmword ptr [rsp+$30], xmm13
        movdqu  xmm8, xmmword ptr [r8+rdx-$30]
        movdqu  xmm9, xmmword ptr [r9+rdx-$30]
        movdqu  xmm10, xmmword ptr [r10+rdx-$30]
        movdqu  xmm11, xmmword ptr [r11+rdx-$30]
        movdqa  xmm12, xmm8
        punpckldq xmm8, xmm9
        punpckhdq xmm12, xmm9
        movdqa  xmm14, xmm10
        punpckldq xmm10, xmm11
        punpckhdq xmm14, xmm11
        movdqa  xmm9, xmm8
        punpcklqdq xmm8, xmm10
        punpckhqdq xmm9, xmm10
        movdqa  xmm13, xmm12
        punpcklqdq xmm12, xmm14
        punpckhqdq xmm13, xmm14
        movdqa  xmmword ptr [rsp+$40], xmm8
        movdqa  xmmword ptr [rsp+$50], xmm9
        movdqa  xmmword ptr [rsp+$60], xmm12
        movdqa  xmmword ptr [rsp+$70], xmm13
        movdqu  xmm8, xmmword ptr [r8+rdx-$20]
        movdqu  xmm9, xmmword ptr [r9+rdx-$20]
        movdqu  xmm10, xmmword ptr [r10+rdx-$20]
        movdqu  xmm11, xmmword ptr [r11+rdx-$20]
        movdqa  xmm12, xmm8
        punpckldq xmm8, xmm9
        punpckhdq xmm12, xmm9
        movdqa  xmm14, xmm10
        punpckldq xmm10, xmm11
        punpckhdq xmm14, xmm11
        movdqa  xmm9, xmm8
        punpcklqdq xmm8, xmm10
        punpckhqdq xmm9, xmm10
        movdqa  xmm13, xmm12
        punpcklqdq xmm12, xmm14
        punpckhqdq xmm13, xmm14
        movdqa  xmmword ptr [rsp+$80], xmm8
        movdqa  xmmword ptr [rsp+$90], xmm9
        movdqa  xmmword ptr [rsp+$A0], xmm12
        movdqa  xmmword ptr [rsp+$B0], xmm13
        movdqu  xmm8, xmmword ptr [r8+rdx-$10]
        movdqu  xmm9, xmmword ptr [r9+rdx-$10]
        movdqu  xmm10, xmmword ptr [r10+rdx-$10]
        movdqu  xmm11, xmmword ptr [r11+rdx-$10]
        movdqa  xmm12, xmm8
        punpckldq xmm8, xmm9
        punpckhdq xmm12, xmm9
        movdqa  xmm14, xmm10
        punpckldq xmm10, xmm11
        punpckhdq xmm14, xmm11
        movdqa  xmm9, xmm8
        punpcklqdq xmm8, xmm10
        punpckhqdq xmm9, xmm10
        movdqa  xmm13, xmm12
        punpcklqdq xmm12, xmm14
        punpckhqdq xmm13, xmm14
        movdqa  xmmword ptr [rsp+$C0], xmm8
        movdqa  xmmword ptr [rsp+$D0], xmm9
        movdqa  xmmword ptr [rsp+$E0], xmm12
        movdqa  xmmword ptr [rsp+$F0], xmm13
        movdqa  xmm9, xmmword ptr [BLAKE3_IV_1+rip]
        movdqa  xmm10, xmmword ptr [BLAKE3_IV_2+rip]
        movdqa  xmm11, xmmword ptr [BLAKE3_IV_3+rip]
        movdqa  xmm12, xmmword ptr [rsp+$110]
        movdqa  xmm13, xmmword ptr [rsp+$120]
        movdqa  xmm14, xmmword ptr [BLAKE3_BLOCK_LEN8+rip]
        movd    xmm15, eax
        pshufd  xmm15, xmm15, $00
        prefetcht0 [r8+rdx+$80]
        prefetcht0 [r9+rdx+$80]
        prefetcht0 [r10+rdx+$80]
        prefetcht0 [r11+rdx+$80]
        paddd   xmm0, xmmword ptr [rsp]
        paddd   xmm1, xmmword ptr [rsp+$20]
        paddd   xmm2, xmmword ptr [rsp+$40]
        paddd   xmm3, xmmword ptr [rsp+$60]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [BLAKE3_IV_0+rip]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$10]
        paddd   xmm1, xmmword ptr [rsp+$30]
        paddd   xmm2, xmmword ptr [rsp+$50]
        paddd   xmm3, xmmword ptr [rsp+$70]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$80]
        paddd   xmm1, xmmword ptr [rsp+$A0]
        paddd   xmm2, xmmword ptr [rsp+$C0]
        paddd   xmm3, xmmword ptr [rsp+$E0]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$90]
        paddd   xmm1, xmmword ptr [rsp+$B0]
        paddd   xmm2, xmmword ptr [rsp+$D0]
        paddd   xmm3, xmmword ptr [rsp+$F0]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$20]
        paddd   xmm1, xmmword ptr [rsp+$30]
        paddd   xmm2, xmmword ptr [rsp+$70]
        paddd   xmm3, xmmword ptr [rsp+$40]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$60]
        paddd   xmm1, xmmword ptr [rsp+$A0]
        paddd   xmm2, xmmword ptr [rsp]
        paddd   xmm3, xmmword ptr [rsp+$D0]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$10]
        paddd   xmm1, xmmword ptr [rsp+$C0]
        paddd   xmm2, xmmword ptr [rsp+$90]
        paddd   xmm3, xmmword ptr [rsp+$F0]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$B0]
        paddd   xmm1, xmmword ptr [rsp+$50]
        paddd   xmm2, xmmword ptr [rsp+$E0]
        paddd   xmm3, xmmword ptr [rsp+$80]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$30]
        paddd   xmm1, xmmword ptr [rsp+$A0]
        paddd   xmm2, xmmword ptr [rsp+$D0]
        paddd   xmm3, xmmword ptr [rsp+$70]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$40]
        paddd   xmm1, xmmword ptr [rsp+$C0]
        paddd   xmm2, xmmword ptr [rsp+$20]
        paddd   xmm3, xmmword ptr [rsp+$E0]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$60]
        paddd   xmm1, xmmword ptr [rsp+$90]
        paddd   xmm2, xmmword ptr [rsp+$B0]
        paddd   xmm3, xmmword ptr [rsp+$80]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$50]
        paddd   xmm1, xmmword ptr [rsp]
        paddd   xmm2, xmmword ptr [rsp+$F0]
        paddd   xmm3, xmmword ptr [rsp+$10]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$A0]
        paddd   xmm1, xmmword ptr [rsp+$C0]
        paddd   xmm2, xmmword ptr [rsp+$E0]
        paddd   xmm3, xmmword ptr [rsp+$D0]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$70]
        paddd   xmm1, xmmword ptr [rsp+$90]
        paddd   xmm2, xmmword ptr [rsp+$30]
        paddd   xmm3, xmmword ptr [rsp+$F0]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$40]
        paddd   xmm1, xmmword ptr [rsp+$B0]
        paddd   xmm2, xmmword ptr [rsp+$50]
        paddd   xmm3, xmmword ptr [rsp+$10]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp]
        paddd   xmm1, xmmword ptr [rsp+$20]
        paddd   xmm2, xmmword ptr [rsp+$80]
        paddd   xmm3, xmmword ptr [rsp+$60]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$C0]
        paddd   xmm1, xmmword ptr [rsp+$90]
        paddd   xmm2, xmmword ptr [rsp+$F0]
        paddd   xmm3, xmmword ptr [rsp+$E0]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$D0]
        paddd   xmm1, xmmword ptr [rsp+$B0]
        paddd   xmm2, xmmword ptr [rsp+$A0]
        paddd   xmm3, xmmword ptr [rsp+$80]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$70]
        paddd   xmm1, xmmword ptr [rsp+$50]
        paddd   xmm2, xmmword ptr [rsp]
        paddd   xmm3, xmmword ptr [rsp+$60]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$20]
        paddd   xmm1, xmmword ptr [rsp+$30]
        paddd   xmm2, xmmword ptr [rsp+$10]
        paddd   xmm3, xmmword ptr [rsp+$40]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$90]
        paddd   xmm1, xmmword ptr [rsp+$B0]
        paddd   xmm2, xmmword ptr [rsp+$80]
        paddd   xmm3, xmmword ptr [rsp+$F0]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$E0]
        paddd   xmm1, xmmword ptr [rsp+$50]
        paddd   xmm2, xmmword ptr [rsp+$C0]
        paddd   xmm3, xmmword ptr [rsp+$10]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$D0]
        paddd   xmm1, xmmword ptr [rsp]
        paddd   xmm2, xmmword ptr [rsp+$20]
        paddd   xmm3, xmmword ptr [rsp+$40]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$30]
        paddd   xmm1, xmmword ptr [rsp+$A0]
        paddd   xmm2, xmmword ptr [rsp+$60]
        paddd   xmm3, xmmword ptr [rsp+$70]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$B0]
        paddd   xmm1, xmmword ptr [rsp+$50]
        paddd   xmm2, xmmword ptr [rsp+$10]
        paddd   xmm3, xmmword ptr [rsp+$80]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$F0]
        paddd   xmm1, xmmword ptr [rsp]
        paddd   xmm2, xmmword ptr [rsp+$90]
        paddd   xmm3, xmmword ptr [rsp+$60]
        paddd   xmm0, xmm4
        paddd   xmm1, xmm5
        paddd   xmm2, xmm6
        paddd   xmm3, xmm7
        pxor    xmm12, xmm0
        pxor    xmm13, xmm1
        pxor    xmm14, xmm2
        pxor    xmm15, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        pshufb  xmm15, xmm8
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm12
        paddd   xmm9, xmm13
        paddd   xmm10, xmm14
        paddd   xmm11, xmm15
        pxor    xmm4, xmm8
        pxor    xmm5, xmm9
        pxor    xmm6, xmm10
        pxor    xmm7, xmm11
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        paddd   xmm0, xmmword ptr [rsp+$E0]
        paddd   xmm1, xmmword ptr [rsp+$20]
        paddd   xmm2, xmmword ptr [rsp+$30]
        paddd   xmm3, xmmword ptr [rsp+$70]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT16+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        movdqa  xmmword ptr [rsp+$100], xmm8
        movdqa  xmm8, xmm5
        psrld   xmm8, 12
        pslld   xmm5, 20
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 12
        pslld   xmm6, 20
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 12
        pslld   xmm7, 20
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 12
        pslld   xmm4, 20
        por     xmm4, xmm8
        paddd   xmm0, xmmword ptr [rsp+$A0]
        paddd   xmm1, xmmword ptr [rsp+$C0]
        paddd   xmm2, xmmword ptr [rsp+$40]
        paddd   xmm3, xmmword ptr [rsp+$D0]
        paddd   xmm0, xmm5
        paddd   xmm1, xmm6
        paddd   xmm2, xmm7
        paddd   xmm3, xmm4
        pxor    xmm15, xmm0
        pxor    xmm12, xmm1
        pxor    xmm13, xmm2
        pxor    xmm14, xmm3
        movdqa  xmm8, xmmword ptr [ROT8+rip]
        pshufb  xmm15, xmm8
        pshufb  xmm12, xmm8
        pshufb  xmm13, xmm8
        pshufb  xmm14, xmm8
        paddd   xmm10, xmm15
        paddd   xmm11, xmm12
        movdqa  xmm8, xmmword ptr [rsp+$100]
        paddd   xmm8, xmm13
        paddd   xmm9, xmm14
        pxor    xmm5, xmm10
        pxor    xmm6, xmm11
        pxor    xmm7, xmm8
        pxor    xmm4, xmm9
        pxor    xmm0, xmm8
        pxor    xmm1, xmm9
        pxor    xmm2, xmm10
        pxor    xmm3, xmm11
        movdqa  xmm8, xmm5
        psrld   xmm8, 7
        pslld   xmm5, 25
        por     xmm5, xmm8
        movdqa  xmm8, xmm6
        psrld   xmm8, 7
        pslld   xmm6, 25
        por     xmm6, xmm8
        movdqa  xmm8, xmm7
        psrld   xmm8, 7
        pslld   xmm7, 25
        por     xmm7, xmm8
        movdqa  xmm8, xmm4
        psrld   xmm8, 7
        pslld   xmm4, 25
        por     xmm4, xmm8
        pxor    xmm4, xmm12
        pxor    xmm5, xmm13
        pxor    xmm6, xmm14
        pxor    xmm7, xmm15
        mov     eax, r13d
        jne     @L01L09
        movdqa  xmm9, xmm0
        punpckldq xmm0, xmm1
        punpckhdq xmm9, xmm1
        movdqa  xmm11, xmm2
        punpckldq xmm2, xmm3
        punpckhdq xmm11, xmm3
        movdqa  xmm1, xmm0
        punpcklqdq xmm0, xmm2
        punpckhqdq xmm1, xmm2
        movdqa  xmm3, xmm9
        punpcklqdq xmm9, xmm11
        punpckhqdq xmm3, xmm11
        movdqu  xmmword ptr [rbx], xmm0
        movdqu  xmmword ptr [rbx+$20], xmm1
        movdqu  xmmword ptr [rbx+$40], xmm9
        movdqu  xmmword ptr [rbx+$60], xmm3
        movdqa  xmm9, xmm4
        punpckldq xmm4, xmm5
        punpckhdq xmm9, xmm5
        movdqa  xmm11, xmm6
        punpckldq xmm6, xmm7
        punpckhdq xmm11, xmm7
        movdqa  xmm5, xmm4
        punpcklqdq xmm4, xmm6
        punpckhqdq xmm5, xmm6
        movdqa  xmm7, xmm9
        punpcklqdq xmm9, xmm11
        punpckhqdq xmm7, xmm11
        movdqu  xmmword ptr [rbx+$10], xmm4
        movdqu  xmmword ptr [rbx+$30], xmm5
        movdqu  xmmword ptr [rbx+$50], xmm9
        movdqu  xmmword ptr [rbx+$70], xmm7
        movdqa  xmm1, xmmword ptr [rsp+$110]
        movdqa  xmm0, xmm1
        paddd   xmm1, xmmword ptr [rsp+$150]
        movdqa  xmmword ptr [rsp+$110], xmm1
        pxor    xmm0, xmmword ptr [CMP_MSB_MASK+rip]
        pxor    xmm1, xmmword ptr [CMP_MSB_MASK+rip]
        pcmpgtd xmm0, xmm1
        movdqa  xmm1, xmmword ptr [rsp+$120]
        psubd   xmm1, xmm0
        movdqa  xmmword ptr [rsp+$120], xmm1
        add     rbx, 128
        add     rdi, 32
        sub     rsi, 4
        cmp     rsi, 4
        jnc     @L00L02
        test    rsi, rsi
        jne     @L03L03
@L02L04:
{$IF DEFINED(WIN64)}
        movdqa  xmm6, xmmword ptr [rsp+$170]
        movdqa  xmm7, xmmword ptr [rsp+$180]
        movdqa  xmm8, xmmword ptr [rsp+$190]
        movdqa  xmm9, xmmword ptr [rsp+$1A0]
        movdqa  xmm10, xmmword ptr [rsp+$1B0]
        movdqa  xmm11, xmmword ptr [rsp+$1C0]
        movdqa  xmm12, xmmword ptr [rsp+$1D0]
        movdqa  xmm13, xmmword ptr [rsp+$1E0]
        movdqa  xmm14, xmmword ptr [rsp+$1F0]
        movdqa  xmm15, xmmword ptr [rsp+$200]
        mov     rdi, qword ptr [rbp+24]
        mov     rsi, qword ptr [rbp+16]
{$ENDIF}
        mov     rsp, rbp
        sub     rsp, 40
        pop     rbx
        pop     r12
        pop     r13
        pop     r14
        pop     r15
        mov     rsp, rbp
        pop     rbp
        ret
@L03L03:
        test    esi, $2
        je      @L07L03
        movups  xmm0, xmmword ptr [rcx]
        movups  xmm1, xmmword ptr [rcx+$10]
        movaps  xmm8, xmm0
        movaps  xmm9, xmm1
        movd    xmm13, dword ptr [rsp+$110]
        pinsrd  xmm13, dword ptr [rsp+$120], 1
        pinsrd  xmm13, dword ptr [BLAKE3_BLOCK_LEN8+rip], 2
        movaps  xmmword ptr [rsp], xmm13
        movd    xmm14, dword ptr [rsp+$114]
        pinsrd  xmm14, dword ptr [rsp+$124], 1
        pinsrd  xmm14, dword ptr [BLAKE3_BLOCK_LEN8+rip], 2
        movaps  xmmword ptr [rsp+$10], xmm14
        mov     r8, qword ptr [rdi]
        mov     r9, qword ptr [rdi+$8]
        movzx   eax, byte ptr [flags_start]
        or      eax, r13d
        xor     edx, edx
@L04L02:
        mov     r14d, eax
        or      eax, r12d
        add     rdx, 64
        cmp     rdx, r15
        cmovne  eax, r14d
        movaps  xmm2, xmmword ptr [BLAKE3_IV+rip]
        movaps  xmm10, xmm2
        movups  xmm4, xmmword ptr [r8+rdx-$40]
        movups  xmm5, xmmword ptr [r8+rdx-$30]
        movaps  xmm3, xmm4
        shufps  xmm4, xmm5, 136
        shufps  xmm3, xmm5, 221
        movaps  xmm5, xmm3
        movups  xmm6, xmmword ptr [r8+rdx-$20]
        movups  xmm7, xmmword ptr [r8+rdx-$10]
        movaps  xmm3, xmm6
        shufps  xmm6, xmm7, 136
        pshufd  xmm6, xmm6, $93
        shufps  xmm3, xmm7, 221
        pshufd  xmm7, xmm3, $93
        movups  xmm12, xmmword ptr [r9+rdx-$40]
        movups  xmm13, xmmword ptr [r9+rdx-$30]
        movaps  xmm11, xmm12
        shufps  xmm12, xmm13, 136
        shufps  xmm11, xmm13, 221
        movaps  xmm13, xmm11
        movups  xmm14, xmmword ptr [r9+rdx-$20]
        movups  xmm15, xmmword ptr [r9+rdx-$10]
        movaps  xmm11, xmm14
        shufps  xmm14, xmm15, 136
        pshufd  xmm14, xmm14, $93
        shufps  xmm11, xmm15, 221
        pshufd  xmm15, xmm11, $93
        movaps  xmm3, xmmword ptr [rsp]
        movaps  xmm11, xmmword ptr [rsp+$10]
        pinsrd  xmm3, eax, 3
        pinsrd  xmm11, eax, 3
        mov     al, 7
@L05L09:
        paddd   xmm0, xmm4
        paddd   xmm8, xmm12
        movaps  xmmword ptr [rsp+$20], xmm4
        movaps  xmmword ptr [rsp+$30], xmm12
        paddd   xmm0, xmm1
        paddd   xmm8, xmm9
        pxor    xmm3, xmm0
        pxor    xmm11, xmm8
        movaps  xmm12, xmmword ptr [ROT16+rip]
        pshufb  xmm3, xmm12
        pshufb  xmm11, xmm12
        paddd   xmm2, xmm3
        paddd   xmm10, xmm11
        pxor    xmm1, xmm2
        pxor    xmm9, xmm10
        movdqa  xmm4, xmm1
        pslld   xmm1, 20
        psrld   xmm4, 12
        por     xmm1, xmm4
        movdqa  xmm4, xmm9
        pslld   xmm9, 20
        psrld   xmm4, 12
        por     xmm9, xmm4
        paddd   xmm0, xmm5
        paddd   xmm8, xmm13
        movaps  xmmword ptr [rsp+$40], xmm5
        movaps  xmmword ptr [rsp+$50], xmm13
        paddd   xmm0, xmm1
        paddd   xmm8, xmm9
        pxor    xmm3, xmm0
        pxor    xmm11, xmm8
        movaps  xmm13, xmmword ptr [ROT8+rip]
        pshufb  xmm3, xmm13
        pshufb  xmm11, xmm13
        paddd   xmm2, xmm3
        paddd   xmm10, xmm11
        pxor    xmm1, xmm2
        pxor    xmm9, xmm10
        movdqa  xmm4, xmm1
        pslld   xmm1, 25
        psrld   xmm4, 7
        por     xmm1, xmm4
        movdqa  xmm4, xmm9
        pslld   xmm9, 25
        psrld   xmm4, 7
        por     xmm9, xmm4
        pshufd  xmm0, xmm0, $93
        pshufd  xmm8, xmm8, $93
        pshufd  xmm3, xmm3, $4E
        pshufd  xmm11, xmm11, $4E
        pshufd  xmm2, xmm2, $39
        pshufd  xmm10, xmm10, $39
        paddd   xmm0, xmm6
        paddd   xmm8, xmm14
        paddd   xmm0, xmm1
        paddd   xmm8, xmm9
        pxor    xmm3, xmm0
        pxor    xmm11, xmm8
        pshufb  xmm3, xmm12
        pshufb  xmm11, xmm12
        paddd   xmm2, xmm3
        paddd   xmm10, xmm11
        pxor    xmm1, xmm2
        pxor    xmm9, xmm10
        movdqa  xmm4, xmm1
        pslld   xmm1, 20
        psrld   xmm4, 12
        por     xmm1, xmm4
        movdqa  xmm4, xmm9
        pslld   xmm9, 20
        psrld   xmm4, 12
        por     xmm9, xmm4
        paddd   xmm0, xmm7
        paddd   xmm8, xmm15
        paddd   xmm0, xmm1
        paddd   xmm8, xmm9
        pxor    xmm3, xmm0
        pxor    xmm11, xmm8
        pshufb  xmm3, xmm13
        pshufb  xmm11, xmm13
        paddd   xmm2, xmm3
        paddd   xmm10, xmm11
        pxor    xmm1, xmm2
        pxor    xmm9, xmm10
        movdqa  xmm4, xmm1
        pslld   xmm1, 25
        psrld   xmm4, 7
        por     xmm1, xmm4
        movdqa  xmm4, xmm9
        pslld   xmm9, 25
        psrld   xmm4, 7
        por     xmm9, xmm4
        pshufd  xmm0, xmm0, $39
        pshufd  xmm8, xmm8, $39
        pshufd  xmm3, xmm3, $4E
        pshufd  xmm11, xmm11, $4E
        pshufd  xmm2, xmm2, $93
        pshufd  xmm10, xmm10, $93
        dec     al
        je      @L06L09
        movdqa  xmm12, xmmword ptr [rsp+$20]
        movdqa  xmm5, xmmword ptr [rsp+$40]
        pshufd  xmm13, xmm12, $0F
        shufps  xmm12, xmm5, 214
        pshufd  xmm4, xmm12, $39
        movdqa  xmm12, xmm6
        shufps  xmm12, xmm7, 250
        pblendw xmm13, xmm12, $CC
        movdqa  xmm12, xmm7
        punpcklqdq xmm12, xmm5
        pblendw xmm12, xmm6, $C0
        pshufd  xmm12, xmm12, $78
        punpckhdq xmm5, xmm7
        punpckldq xmm6, xmm5
        pshufd  xmm7, xmm6, $1E
        movdqa  xmmword ptr [rsp+$20], xmm13
        movdqa  xmmword ptr [rsp+$40], xmm12
        movdqa  xmm5, xmmword ptr [rsp+$30]
        movdqa  xmm13, xmmword ptr [rsp+$50]
        pshufd  xmm6, xmm5, $0F
        shufps  xmm5, xmm13, 214
        pshufd  xmm12, xmm5, $39
        movdqa  xmm5, xmm14
        shufps  xmm5, xmm15, 250
        pblendw xmm6, xmm5, $CC
        movdqa  xmm5, xmm15
        punpcklqdq xmm5, xmm13
        pblendw xmm5, xmm14, $C0
        pshufd  xmm5, xmm5, $78
        punpckhdq xmm13, xmm15
        punpckldq xmm14, xmm13
        pshufd  xmm15, xmm14, $1E
        movdqa  xmm13, xmm6
        movdqa  xmm14, xmm5
        movdqa  xmm5, xmmword ptr [rsp+$20]
        movdqa  xmm6, xmmword ptr [rsp+$40]
        jmp     @L05L09
@L06L09:
        pxor    xmm0, xmm2
        pxor    xmm1, xmm3
        pxor    xmm8, xmm10
        pxor    xmm9, xmm11
        mov     eax, r13d
        cmp     rdx, r15
        jne     @L04L02
        movups  xmmword ptr [rbx], xmm0
        movups  xmmword ptr [rbx+$10], xmm1
        movups  xmmword ptr [rbx+$20], xmm8
        movups  xmmword ptr [rbx+$30], xmm9
        movdqa  xmm0, xmmword ptr [rsp+$130]
        movdqa  xmm1, xmmword ptr [rsp+$110]
        movdqa  xmm2, xmmword ptr [rsp+$120]
        movdqu  xmm3, xmmword ptr [rsp+$118]
        movdqu  xmm4, xmmword ptr [rsp+$128]
        // blendvps xmm1, xmm3, xmm0
        DB $66, $0f, $38, $14, $cb
        // blendvps xmm2, xmm4, xmm0
        DB $66, $0f, $38, $14, $d4
        movdqa  xmmword ptr [rsp+$110], xmm1
        movdqa  xmmword ptr [rsp+$120], xmm2
        add     rdi, 16
        add     rbx, 64
        sub     rsi, 2
@L07L03:
        test    esi, $1
        je      @L02L04
        movups  xmm0, xmmword ptr [rcx]
        movups  xmm1, xmmword ptr [rcx+$10]
        movd    xmm13, dword ptr [rsp+$110]
        pinsrd  xmm13, dword ptr [rsp+$120], 1
        pinsrd  xmm13, dword ptr [BLAKE3_BLOCK_LEN8+rip], 2
        movaps  xmm14, xmmword ptr [ROT8+rip]
        movaps  xmm15, xmmword ptr [ROT16+rip]
        mov     r8, qword ptr [rdi]
        movzx   eax, byte ptr [flags_start]
        or      eax, r13d
        xor     edx, edx
@L08L02:
        mov     r14d, eax
        or      eax, r12d
        add     rdx, 64
        cmp     rdx, r15
        cmovne  eax, r14d
        movaps  xmm2, xmmword ptr [BLAKE3_IV+rip]
        movaps  xmm3, xmm13
        pinsrd  xmm3, eax, 3
        movups  xmm4, xmmword ptr [r8+rdx-$40]
        movups  xmm5, xmmword ptr [r8+rdx-$30]
        movaps  xmm8, xmm4
        shufps  xmm4, xmm5, 136
        shufps  xmm8, xmm5, 221
        movaps  xmm5, xmm8
        movups  xmm6, xmmword ptr [r8+rdx-$20]
        movups  xmm7, xmmword ptr [r8+rdx-$10]
        movaps  xmm8, xmm6
        shufps  xmm6, xmm7, 136
        pshufd  xmm6, xmm6, $93
        shufps  xmm8, xmm7, 221
        pshufd  xmm7, xmm8, $93
        mov     al, 7
@L09L09:
        paddd   xmm0, xmm4
        paddd   xmm0, xmm1
        pxor    xmm3, xmm0
        pshufb  xmm3, xmm15
        paddd   xmm2, xmm3
        pxor    xmm1, xmm2
        movdqa  xmm11, xmm1
        pslld   xmm1, 20
        psrld   xmm11, 12
        por     xmm1, xmm11
        paddd   xmm0, xmm5
        paddd   xmm0, xmm1
        pxor    xmm3, xmm0
        pshufb  xmm3, xmm14
        paddd   xmm2, xmm3
        pxor    xmm1, xmm2
        movdqa  xmm11, xmm1
        pslld   xmm1, 25
        psrld   xmm11, 7
        por     xmm1, xmm11
        pshufd  xmm0, xmm0, $93
        pshufd  xmm3, xmm3, $4E
        pshufd  xmm2, xmm2, $39
        paddd   xmm0, xmm6
        paddd   xmm0, xmm1
        pxor    xmm3, xmm0
        pshufb  xmm3, xmm15
        paddd   xmm2, xmm3
        pxor    xmm1, xmm2
        movdqa  xmm11, xmm1
        pslld   xmm1, 20
        psrld   xmm11, 12
        por     xmm1, xmm11
        paddd   xmm0, xmm7
        paddd   xmm0, xmm1
        pxor    xmm3, xmm0
        pshufb  xmm3, xmm14
        paddd   xmm2, xmm3
        pxor    xmm1, xmm2
        movdqa  xmm11, xmm1
        pslld   xmm1, 25
        psrld   xmm11, 7
        por     xmm1, xmm11
        pshufd  xmm0, xmm0, $39
        pshufd  xmm3, xmm3, $4E
        pshufd  xmm2, xmm2, $93
        dec     al
        jz      @L0AL09
        movdqa  xmm8, xmm4
        shufps  xmm8, xmm5, 214
        pshufd  xmm9, xmm4, $0F
        pshufd  xmm4, xmm8, $39
        movdqa  xmm8, xmm6
        shufps  xmm8, xmm7, 250
        pblendw xmm9, xmm8, $CC
        movdqa  xmm8, xmm7
        punpcklqdq xmm8, xmm5
        pblendw xmm8, xmm6, $C0
        pshufd  xmm8, xmm8, $78
        punpckhdq xmm5, xmm7
        punpckldq xmm6, xmm5
        pshufd  xmm7, xmm6, $1E
        movdqa  xmm5, xmm9
        movdqa  xmm6, xmm8
        jmp     @L09L09
@L0AL09:
        pxor    xmm0, xmm2
        pxor    xmm1, xmm3
        mov     eax, r13d
        cmp     rdx, r15
        jne     @L08L02
        movups  xmmword ptr [rbx], xmm0
        movups  xmmword ptr [rbx+$10], xmm1
        jmp     @L02L04
end;


procedure blake3_compress_in_place_sse41(cv: pcuint32; const block: pcuint8;
                                         block_len: cuint8; counter: cuint64;
                                         flags: cuint8); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX, R8
// WIN64:  RCX, RDX, R8,  R9,  STACK
asm
{$IF DEFINED(WIN64)}
    push    rsi
    push    rdi
    mov     rsi, rdx
    mov     rdi, rcx
    mov     rdx, r8
    mov     rcx, r9
    sub     rsp, 120
    movdqa  xmmword ptr [rsp], xmm6
    movdqa  xmmword ptr [rsp+$10], xmm7
    movdqa  xmmword ptr [rsp+$20], xmm8
    movdqa  xmmword ptr [rsp+$30], xmm9
    movdqa  xmmword ptr [rsp+$40], xmm11
    movdqa  xmmword ptr [rsp+$50], xmm14
    movdqa  xmmword ptr [rsp+$60], xmm15
    movzx   r8, byte ptr [rsp+$B0]
{$ENDIF}
    movups  xmm0, xmmword ptr [rdi]
    movups  xmm1, xmmword ptr [rdi+$10]
    movaps  xmm2, xmmword ptr [BLAKE3_IV+rip]
    shl     r8, 32
    add     rdx, r8
    movq    xmm3, rcx
    movq    xmm4, rdx
    punpcklqdq xmm3, xmm4
    movups  xmm4, xmmword ptr [rsi]
    movups  xmm5, xmmword ptr [rsi+$10]
    movaps  xmm8, xmm4
    shufps  xmm4, xmm5, 136
    shufps  xmm8, xmm5, 221
    movaps  xmm5, xmm8
    movups  xmm6, xmmword ptr [rsi+$20]
    movups  xmm7, xmmword ptr [rsi+$30]
    movaps  xmm8, xmm6
    shufps  xmm6, xmm7, 136
    pshufd  xmm6, xmm6, $93
    shufps  xmm8, xmm7, 221
    pshufd  xmm7, xmm8, $93
    movaps  xmm14, xmmword ptr [ROT8+rip]
    movaps  xmm15, xmmword ptr [ROT16+rip]
    mov     al, 7
@Lab:
    paddd   xmm0, xmm4
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm15
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 20
    psrld   xmm11, 12
    por     xmm1, xmm11
    paddd   xmm0, xmm5
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm14
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 25
    psrld   xmm11, 7
    por     xmm1, xmm11
    pshufd  xmm0, xmm0, $93
    pshufd  xmm3, xmm3, $4E
    pshufd  xmm2, xmm2, $39
    paddd   xmm0, xmm6
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm15
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 20
    psrld   xmm11, 12
    por     xmm1, xmm11
    paddd   xmm0, xmm7
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm14
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 25
    psrld   xmm11, 7
    por     xmm1, xmm11
    pshufd  xmm0, xmm0, $39
    pshufd  xmm3, xmm3, $4E
    pshufd  xmm2, xmm2, $93
    dec     al
    jz      @Laf
    movdqa  xmm8, xmm4
    shufps  xmm8, xmm5, 214
    pshufd  xmm9, xmm4, $0F
    pshufd  xmm4, xmm8, $39
    movdqa  xmm8, xmm6
    shufps  xmm8, xmm7, 250
    pblendw xmm9, xmm8, $CC
    movdqa  xmm8, xmm7
    punpcklqdq xmm8, xmm5
    pblendw xmm8, xmm6, $C0
    pshufd  xmm8, xmm8, $78
    punpckhdq xmm5, xmm7
    punpckldq xmm6, xmm5
    pshufd  xmm7, xmm6, $1E
    movdqa  xmm5, xmm9
    movdqa  xmm6, xmm8
    jmp     @Lab
@Laf:
    pxor    xmm0, xmm2
    pxor    xmm1, xmm3
    movups  xmmword ptr [rdi], xmm0
    movups  xmmword ptr [rdi+$10], xmm1
{$IF DEFINED(WIN64)}
    movdqa  xmm6, xmmword ptr [rsp]
    movdqa  xmm7, xmmword ptr [rsp+$10]
    movdqa  xmm8, xmmword ptr [rsp+$20]
    movdqa  xmm9, xmmword ptr [rsp+$30]
    movdqa  xmm11, xmmword ptr [rsp+$40]
    movdqa  xmm14, xmmword ptr [rsp+$50]
    movdqa  xmm15, xmmword ptr [rsp+$60]
    add     rsp, 120
    pop     rdi
    pop     rsi
{$ENDIF}
    ret
end;

procedure blake3_compress_xof_sse41(const cv: pcuint32;
                                    const block: pcuint8;
                                    block_len: cuint8; counter: cuint64;
                                    flags: cuint8; out_: pcuint8); assembler; nostackframe;
// UNIX    RDI, RSI, RDX, RCX, R8,    R9
// WIN64:  RCX, RDX, R8,  R9,  STACK, STACK
asm
{$IF DEFINED(WIN64)}
    push    rsi
    push    rdi
    mov     rsi, rdx
    mov     rdi, rcx
    mov     rdx, r8
    mov     rcx, r9
    sub     rsp, 120
    movdqa  xmmword ptr [rsp], xmm6
    movdqa  xmmword ptr [rsp+$10], xmm7
    movdqa  xmmword ptr [rsp+$20], xmm8
    movdqa  xmmword ptr [rsp+$30], xmm9
    movdqa  xmmword ptr [rsp+$40], xmm11
    movdqa  xmmword ptr [rsp+$50], xmm14
    movdqa  xmmword ptr [rsp+$60], xmm15
    movzx   r8, byte ptr [rsp+$B0]
    mov     r9, qword ptr [rsp+$B8]
{$ENDIF}
    movups  xmm0, xmmword ptr [rdi]
    movups  xmm1, xmmword ptr [rdi+$10]
    movaps  xmm2, xmmword ptr [BLAKE3_IV+rip]
    movzx   eax, r8b
    movzx   edx, dl
    shl     rax, 32
    add     rdx, rax
    movq    xmm3, rcx
    movq    xmm4, rdx
    punpcklqdq xmm3, xmm4
    movups  xmm4, xmmword ptr [rsi]
    movups  xmm5, xmmword ptr [rsi+$10]
    movaps  xmm8, xmm4
    shufps  xmm4, xmm5, 136
    shufps  xmm8, xmm5, 221
    movaps  xmm5, xmm8
    movups  xmm6, xmmword ptr [rsi+$20]
    movups  xmm7, xmmword ptr [rsi+$30]
    movaps  xmm8, xmm6
    shufps  xmm6, xmm7, 136
    pshufd  xmm6, xmm6, $93
    shufps  xmm8, xmm7, 221
    pshufd  xmm7, xmm8, $93
    movaps  xmm14, xmmword ptr [ROT8+rip]
    movaps  xmm15, xmmword ptr [ROT16+rip]
    mov     al, 7
@Lab:
    paddd   xmm0, xmm4
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm15
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 20
    psrld   xmm11, 12
    por     xmm1, xmm11
    paddd   xmm0, xmm5
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm14
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 25
    psrld   xmm11, 7
    por     xmm1, xmm11
    pshufd  xmm0, xmm0, $93
    pshufd  xmm3, xmm3, $4E
    pshufd  xmm2, xmm2, $39
    paddd   xmm0, xmm6
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm15
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 20
    psrld   xmm11, 12
    por     xmm1, xmm11
    paddd   xmm0, xmm7
    paddd   xmm0, xmm1
    pxor    xmm3, xmm0
    pshufb  xmm3, xmm14
    paddd   xmm2, xmm3
    pxor    xmm1, xmm2
    movdqa  xmm11, xmm1
    pslld   xmm1, 25
    psrld   xmm11, 7
    por     xmm1, xmm11
    pshufd  xmm0, xmm0, $39
    pshufd  xmm3, xmm3, $4E
    pshufd  xmm2, xmm2, $93
    dec     al
    jz      @Laf
    movdqa  xmm8, xmm4
    shufps  xmm8, xmm5, 214
    pshufd  xmm9, xmm4, $0F
    pshufd  xmm4, xmm8, $39
    movdqa  xmm8, xmm6
    shufps  xmm8, xmm7, 250
    pblendw xmm9, xmm8, $CC
    movdqa  xmm8, xmm7
    punpcklqdq xmm8, xmm5
    pblendw xmm8, xmm6, $C0
    pshufd  xmm8, xmm8, $78
    punpckhdq xmm5, xmm7
    punpckldq xmm6, xmm5
    pshufd  xmm7, xmm6, $1E
    movdqa  xmm5, xmm9
    movdqa  xmm6, xmm8
    jmp     @Lab
@Laf:
    movdqu  xmm4, xmmword ptr [rdi]
    movdqu  xmm5, xmmword ptr [rdi+$10]
    pxor    xmm0, xmm2
    pxor    xmm1, xmm3
    pxor    xmm2, xmm4
    pxor    xmm3, xmm5
    movups  xmmword ptr [r9], xmm0
    movups  xmmword ptr [r9+$10], xmm1
    movups  xmmword ptr [r9+$20], xmm2
    movups  xmmword ptr [r9+$30], xmm3
{$IF DEFINED(WIN64)}
    movdqa  xmm6, xmmword ptr [rsp]
    movdqa  xmm7, xmmword ptr [rsp+$10]
    movdqa  xmm8, xmmword ptr [rsp+$20]
    movdqa  xmm9, xmmword ptr [rsp+$30]
    movdqa  xmm11, xmmword ptr [rsp+$40]
    movdqa  xmm14, xmmword ptr [rsp+$50]
    movdqa  xmm15, xmmword ptr [rsp+$60]
    add     rsp, 120
    pop     rdi
    pop     rsi
{$ENDIF}
    ret
end;

function SSE41Support: LongBool; assembler;
asm
  push  rbx
  mov   eax, 1
  cpuid
  and   ecx, $80000
  mov   eax, ecx
  pop   rbx
end;

