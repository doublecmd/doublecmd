{$asmmode intel}

const
  ADD08: array[0..7] of cuint32 = (0, 1, 2, 3, 4, 5, 6, 7);
  ADD18: array[0..7] of cuint32 = (8, 8, 8, 8, 8, 8, 8, 8);

procedure blake3_hash_many_avx2(inputs: ppcuint8; num_inputs: csize_t;
                                blocks: csize_t; const key: pcuint32;
                                counter: cuint64; increment_counter: boolean32;
                                flags: cuint8; flags_start: cuint8;
                                flags_end: cuint8; out_: pcuint8); assembler; nostackframe;
asm
        push    rbp
        mov     rbp, rsp
        push    r15
        push    r14
        push    r13
        push    r12
        push    rbx
        sub     rsp, 880
        and     rsp, $FFFFFFFFFFFFFFC0
{$IF DEFINED(WIN64)}
        vmovdqa xmmword ptr [rsp+$2D0], xmm6
        vmovdqa xmmword ptr [rsp+$2E0], xmm7
        vmovdqa xmmword ptr [rsp+$2F0], xmm8
        vmovdqa xmmword ptr [rsp+$300], xmm9
        vmovdqa xmmword ptr [rsp+$310], xmm10
        vmovdqa xmmword ptr [rsp+$320], xmm11
        vmovdqa xmmword ptr [rsp+$330], xmm12
        vmovdqa xmmword ptr [rsp+$340], xmm13
        vmovdqa xmmword ptr [rsp+$350], xmm14
        vmovdqa xmmword ptr [rsp+$360], xmm15
        mov     qword ptr [rbp+16], rsi
        mov     qword ptr [rbp+24], rdi
        mov     rdi, rcx
        mov     rsi, rdx
        mov     rdx, r8
        mov     rcx, r9
        mov     r8, qword ptr [counter]
        mov     r9, dword ptr [increment_counter]
{$ENDIF}
        neg     r9d
        vmovd   xmm0, r9d
        vpbroadcastd ymm0, xmm0
        vmovdqa ymmword ptr [rsp+$260], ymm0
        vpand   ymm1, ymm0, ymmword ptr [ADD08+rip]
        vpand   ymm2, ymm0, ymmword ptr [ADD18+rip]
        vmovdqa ymmword ptr [rsp+$2A0], ymm2
        vmovd   xmm2, r8d
        vpbroadcastd ymm2, xmm2
        vpaddd  ymm2, ymm2, ymm1
        vmovdqa ymmword ptr [rsp+$220], ymm2
        vpxor   ymm1, ymm1, ymmword ptr [CMP_MSB_MASK+rip]
        vpxor   ymm2, ymm2, ymmword ptr [CMP_MSB_MASK+rip]
        vpcmpgtd ymm2, ymm1, ymm2
        shr     r8, 32
        vmovd   xmm3, r8d
        vpbroadcastd ymm3, xmm3
        vpsubd  ymm3, ymm3, ymm2
        vmovdqa ymmword ptr [rsp+$240], ymm3
        shl     rdx, 6
        mov     qword ptr [rsp+$2C0], rdx
        cmp     rsi, 8
        jc      @L03L03
@L00L02:
        vpbroadcastd ymm0, dword ptr [rcx]
        vpbroadcastd ymm1, dword ptr [rcx+$4]
        vpbroadcastd ymm2, dword ptr [rcx+$8]
        vpbroadcastd ymm3, dword ptr [rcx+$C]
        vpbroadcastd ymm4, dword ptr [rcx+$10]
        vpbroadcastd ymm5, dword ptr [rcx+$14]
        vpbroadcastd ymm6, dword ptr [rcx+$18]
        vpbroadcastd ymm7, dword ptr [rcx+$1C]
        mov     r8, qword ptr [rdi]
        mov     r9, qword ptr [rdi+$8]
        mov     r10, qword ptr [rdi+$10]
        mov     r11, qword ptr [rdi+$18]
        mov     r12, qword ptr [rdi+$20]
        mov     r13, qword ptr [rdi+$28]
        mov     r14, qword ptr [rdi+$30]
        mov     r15, qword ptr [rdi+$38]
        movzx   eax, byte ptr [flags]
        movzx   ebx, byte ptr [flags_start]
        or      eax, ebx
        xor     edx, edx
@L01L09:
        movzx   ebx, byte ptr [flags_end]
        or      ebx, eax
        add     rdx, 64
        cmp     rdx, qword ptr [rsp+$2C0]
        cmove   eax, ebx
        mov     dword ptr [rsp+$200], eax
        vmovups xmm8, xmmword ptr [r8+rdx-$40]
        vinsertf128 ymm8, ymm8, xmmword ptr [r12+rdx-$40], $01
        vmovups xmm9, xmmword ptr [r9+rdx-$40]
        vinsertf128 ymm9, ymm9, xmmword ptr [r13+rdx-$40], $01
        vunpcklpd ymm12, ymm8, ymm9
        vunpckhpd ymm13, ymm8, ymm9
        vmovups xmm10, xmmword ptr [r10+rdx-$40]
        vinsertf128 ymm10, ymm10, xmmword ptr [r14+rdx-$40], $01
        vmovups xmm11, xmmword ptr [r11+rdx-$40]
        vinsertf128 ymm11, ymm11, xmmword ptr [r15+rdx-$40], $01
        vunpcklpd ymm14, ymm10, ymm11
        vunpckhpd ymm15, ymm10, ymm11
        vshufps ymm8, ymm12, ymm14, 136
        vmovaps ymmword ptr [rsp], ymm8
        vshufps ymm9, ymm12, ymm14, 221
        vmovaps ymmword ptr [rsp+$20], ymm9
        vshufps ymm10, ymm13, ymm15, 136
        vmovaps ymmword ptr [rsp+$40], ymm10
        vshufps ymm11, ymm13, ymm15, 221
        vmovaps ymmword ptr [rsp+$60], ymm11
        vmovups xmm8, xmmword ptr [r8+rdx-$30]
        vinsertf128 ymm8, ymm8, xmmword ptr [r12+rdx-$30], $01
        vmovups xmm9, xmmword ptr [r9+rdx-$30]
        vinsertf128 ymm9, ymm9, xmmword ptr [r13+rdx-$30], $01
        vunpcklpd ymm12, ymm8, ymm9
        vunpckhpd ymm13, ymm8, ymm9
        vmovups xmm10, xmmword ptr [r10+rdx-$30]
        vinsertf128 ymm10, ymm10, xmmword ptr [r14+rdx-$30], $01
        vmovups xmm11, xmmword ptr [r11+rdx-$30]
        vinsertf128 ymm11, ymm11, xmmword ptr [r15+rdx-$30], $01
        vunpcklpd ymm14, ymm10, ymm11
        vunpckhpd ymm15, ymm10, ymm11
        vshufps ymm8, ymm12, ymm14, 136
        vmovaps ymmword ptr [rsp+$80], ymm8
        vshufps ymm9, ymm12, ymm14, 221
        vmovaps ymmword ptr [rsp+$A0], ymm9
        vshufps ymm10, ymm13, ymm15, 136
        vmovaps ymmword ptr [rsp+$C0], ymm10
        vshufps ymm11, ymm13, ymm15, 221
        vmovaps ymmword ptr [rsp+$E0], ymm11
        vmovups xmm8, xmmword ptr [r8+rdx-$20]
        vinsertf128 ymm8, ymm8, xmmword ptr [r12+rdx-$20], $01
        vmovups xmm9, xmmword ptr [r9+rdx-$20]
        vinsertf128 ymm9, ymm9, xmmword ptr [r13+rdx-$20], $01
        vunpcklpd ymm12, ymm8, ymm9
        vunpckhpd ymm13, ymm8, ymm9
        vmovups xmm10, xmmword ptr [r10+rdx-$20]
        vinsertf128 ymm10, ymm10, xmmword ptr [r14+rdx-$20], $01
        vmovups xmm11, xmmword ptr [r11+rdx-$20]
        vinsertf128 ymm11, ymm11, xmmword ptr [r15+rdx-$20], $01
        vunpcklpd ymm14, ymm10, ymm11
        vunpckhpd ymm15, ymm10, ymm11
        vshufps ymm8, ymm12, ymm14, 136
        vmovaps ymmword ptr [rsp+$100], ymm8
        vshufps ymm9, ymm12, ymm14, 221
        vmovaps ymmword ptr [rsp+$120], ymm9
        vshufps ymm10, ymm13, ymm15, 136
        vmovaps ymmword ptr [rsp+$140], ymm10
        vshufps ymm11, ymm13, ymm15, 221
        vmovaps ymmword ptr [rsp+$160], ymm11
        vmovups xmm8, xmmword ptr [r8+rdx-$10]
        vinsertf128 ymm8, ymm8, xmmword ptr [r12+rdx-$10], $01
        vmovups xmm9, xmmword ptr [r9+rdx-$10]
        vinsertf128 ymm9, ymm9, xmmword ptr [r13+rdx-$10], $01
        vunpcklpd ymm12, ymm8, ymm9
        vunpckhpd ymm13, ymm8, ymm9
        vmovups xmm10, xmmword ptr [r10+rdx-$10]
        vinsertf128 ymm10, ymm10, xmmword ptr [r14+rdx-$10], $01
        vmovups xmm11, xmmword ptr [r11+rdx-$10]
        vinsertf128 ymm11, ymm11, xmmword ptr [r15+rdx-$10], $01
        vunpcklpd ymm14, ymm10, ymm11
        vunpckhpd ymm15, ymm10, ymm11
        vshufps ymm8, ymm12, ymm14, 136
        vmovaps ymmword ptr [rsp+$180], ymm8
        vshufps ymm9, ymm12, ymm14, 221
        vmovaps ymmword ptr [rsp+$1A0], ymm9
        vshufps ymm10, ymm13, ymm15, 136
        vmovaps ymmword ptr [rsp+$1C0], ymm10
        vshufps ymm11, ymm13, ymm15, 221
        vmovaps ymmword ptr [rsp+$1E0], ymm11
        vpbroadcastd ymm15, dword ptr [rsp+$200]
        prefetcht0 [r8+rdx+$80]
        prefetcht0 [r12+rdx+$80]
        prefetcht0 [r9+rdx+$80]
        prefetcht0 [r13+rdx+$80]
        prefetcht0 [r10+rdx+$80]
        prefetcht0 [r14+rdx+$80]
        prefetcht0 [r11+rdx+$80]
        prefetcht0 [r15+rdx+$80]
        vpaddd  ymm0, ymm0, ymmword ptr [rsp]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$40]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$80]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$C0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm0, ymmword ptr [rsp+$220]
        vpxor   ymm13, ymm1, ymmword ptr [rsp+$240]
        vpxor   ymm14, ymm2, ymmword ptr [BLAKE3_BLOCK_LEN8+rip]
        vpxor   ymm15, ymm3, ymm15
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [BLAKE3_IV_0+rip]
        vpaddd  ymm9, ymm13, ymmword ptr [BLAKE3_IV_1+rip]
        vpaddd  ymm10, ymm14, ymmword ptr [BLAKE3_IV_2+rip]
        vpaddd  ymm11, ymm15, ymmword ptr [BLAKE3_IV_3+rip]
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$20]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$60]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$A0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$E0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$100]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$140]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$180]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1C0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$120]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$160]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$1A0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1E0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$40]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$60]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$E0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$80]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$C0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$140]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1A0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$20]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$180]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$120]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1E0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$160]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$A0]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$1C0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$100]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$60]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$140]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$1A0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$E0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$80]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$180]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$40]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1C0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$C0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$120]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$160]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$100]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$A0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$1E0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$20]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$140]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$180]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$1C0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1A0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$E0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$120]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$60]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1E0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$80]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$160]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$A0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$20]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$40]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$100]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$C0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$180]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$120]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$1E0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1C0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$1A0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$160]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$140]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$100]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$E0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$A0]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$C0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$40]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$60]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$20]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$80]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$120]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$160]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$100]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1E0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$1C0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$A0]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$180]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$20]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$1A0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$40]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$80]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$60]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$140]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$C0]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$E0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$160]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$A0]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$20]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$100]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$1E0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$120]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$C0]
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm1, ymm1, ymm5
        vpaddd  ymm2, ymm2, ymm6
        vpaddd  ymm3, ymm3, ymm7
        vpxor   ymm12, ymm12, ymm0
        vpxor   ymm13, ymm13, ymm1
        vpxor   ymm14, ymm14, ymm2
        vpxor   ymm15, ymm15, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpshufb ymm15, ymm15, ymm8
        vpaddd  ymm8, ymm12, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm13
        vpaddd  ymm10, ymm10, ymm14
        vpaddd  ymm11, ymm11, ymm15
        vpxor   ymm4, ymm4, ymm8
        vpxor   ymm5, ymm5, ymm9
        vpxor   ymm6, ymm6, ymm10
        vpxor   ymm7, ymm7, ymm11
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$1C0]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$40]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$60]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$E0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT16+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vmovdqa ymmword ptr [rsp+$200], ymm8
        vpsrld  ymm8, ymm5, 12
        vpslld  ymm5, ymm5, 20
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 12
        vpslld  ymm6, ymm6, 20
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 12
        vpslld  ymm7, ymm7, 20
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 12
        vpslld  ymm4, ymm4, 20
        vpor    ymm4, ymm4, ymm8
        vpaddd  ymm0, ymm0, ymmword ptr [rsp+$140]
        vpaddd  ymm1, ymm1, ymmword ptr [rsp+$180]
        vpaddd  ymm2, ymm2, ymmword ptr [rsp+$80]
        vpaddd  ymm3, ymm3, ymmword ptr [rsp+$1A0]
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm1, ymm1, ymm6
        vpaddd  ymm2, ymm2, ymm7
        vpaddd  ymm3, ymm3, ymm4
        vpxor   ymm15, ymm15, ymm0
        vpxor   ymm12, ymm12, ymm1
        vpxor   ymm13, ymm13, ymm2
        vpxor   ymm14, ymm14, ymm3
        vbroadcasti128 ymm8, xmmword ptr [ROT8+rip]
        vpshufb ymm15, ymm15, ymm8
        vpshufb ymm12, ymm12, ymm8
        vpshufb ymm13, ymm13, ymm8
        vpshufb ymm14, ymm14, ymm8
        vpaddd  ymm10, ymm10, ymm15
        vpaddd  ymm11, ymm11, ymm12
        vpaddd  ymm8, ymm13, ymmword ptr [rsp+$200]
        vpaddd  ymm9, ymm9, ymm14
        vpxor   ymm5, ymm5, ymm10
        vpxor   ymm6, ymm6, ymm11
        vpxor   ymm7, ymm7, ymm8
        vpxor   ymm4, ymm4, ymm9
        vpxor   ymm0, ymm0, ymm8
        vpxor   ymm1, ymm1, ymm9
        vpxor   ymm2, ymm2, ymm10
        vpxor   ymm3, ymm3, ymm11
        vpsrld  ymm8, ymm5, 7
        vpslld  ymm5, ymm5, 25
        vpor    ymm5, ymm5, ymm8
        vpsrld  ymm8, ymm6, 7
        vpslld  ymm6, ymm6, 25
        vpor    ymm6, ymm6, ymm8
        vpsrld  ymm8, ymm7, 7
        vpslld  ymm7, ymm7, 25
        vpor    ymm7, ymm7, ymm8
        vpsrld  ymm8, ymm4, 7
        vpslld  ymm4, ymm4, 25
        vpor    ymm4, ymm4, ymm8
        vpxor   ymm4, ymm4, ymm12
        vpxor   ymm5, ymm5, ymm13
        vpxor   ymm6, ymm6, ymm14
        vpxor   ymm7, ymm7, ymm15
        movzx   eax, byte ptr [flags]
        jne     @L01L09
        mov     rbx, qword ptr [out_]
        vunpcklps ymm8, ymm0, ymm1
        vunpcklps ymm9, ymm2, ymm3
        vunpckhps ymm10, ymm0, ymm1
        vunpcklps ymm11, ymm4, ymm5
        vunpcklps ymm0, ymm6, ymm7
        vshufps ymm12, ymm8, ymm9, 78
        vblendps ymm1, ymm8, ymm12, $CC
        vshufps ymm8, ymm11, ymm0, 78
        vunpckhps ymm13, ymm2, ymm3
        vblendps ymm2, ymm11, ymm8, $CC
        vblendps ymm3, ymm12, ymm9, $CC
        vperm2f128 ymm12, ymm1, ymm2, $20
        vmovups ymmword ptr [rbx], ymm12
        vunpckhps ymm14, ymm4, ymm5
        vblendps ymm4, ymm8, ymm0, $CC
        vunpckhps ymm15, ymm6, ymm7
        vperm2f128 ymm7, ymm3, ymm4, $20
        vmovups ymmword ptr [rbx+$20], ymm7
        vshufps ymm5, ymm10, ymm13, 78
        vblendps ymm6, ymm5, ymm13, $CC
        vshufps ymm13, ymm14, ymm15, 78
        vblendps ymm10, ymm10, ymm5, $CC
        vblendps ymm14, ymm14, ymm13, $CC
        vperm2f128 ymm8, ymm10, ymm14, $20
        vmovups ymmword ptr [rbx+$40], ymm8
        vblendps ymm15, ymm13, ymm15, $CC
        vperm2f128 ymm13, ymm6, ymm15, $20
        vmovups ymmword ptr [rbx+$60], ymm13
        vperm2f128 ymm9, ymm1, ymm2, $31
        vperm2f128 ymm11, ymm3, ymm4, $31
        vmovups ymmword ptr [rbx+$80], ymm9
        vperm2f128 ymm14, ymm10, ymm14, $31
        vperm2f128 ymm15, ymm6, ymm15, $31
        vmovups ymmword ptr [rbx+$A0], ymm11
        vmovups ymmword ptr [rbx+$C0], ymm14
        vmovups ymmword ptr [rbx+$E0], ymm15
        vmovdqa ymm0, ymmword ptr [rsp+$2A0]
        vpaddd  ymm1, ymm0, ymmword ptr [rsp+$220]
        vmovdqa ymmword ptr [rsp+$220], ymm1
        vpxor   ymm0, ymm0, ymmword ptr [CMP_MSB_MASK+rip]
        vpxor   ymm2, ymm1, ymmword ptr [CMP_MSB_MASK+rip]
        vpcmpgtd ymm2, ymm0, ymm2
        vmovdqa ymm0, ymmword ptr [rsp+$240]
        vpsubd  ymm2, ymm0, ymm2
        vmovdqa ymmword ptr [rsp+$240], ymm2
        add     rdi, 64
        add     rbx, 256
        mov     qword ptr [out_], rbx
        sub     rsi, 8
        cmp     rsi, 8
        jnc     @L00L02
        test    rsi, rsi
        jnz     @L03L03
@L02L04:
        vzeroupper
{$IF DEFINED(WIN64)}
        vmovdqa xmm6, xmmword ptr [rsp+$2D0]
        vmovdqa xmm7, xmmword ptr [rsp+$2E0]
        vmovdqa xmm8, xmmword ptr [rsp+$2F0]
        vmovdqa xmm9, xmmword ptr [rsp+$300]
        vmovdqa xmm10, xmmword ptr [rsp+$310]
        vmovdqa xmm11, xmmword ptr [rsp+$320]
        vmovdqa xmm12, xmmword ptr [rsp+$330]
        vmovdqa xmm13, xmmword ptr [rsp+$340]
        vmovdqa xmm14, xmmword ptr [rsp+$350]
        vmovdqa xmm15, xmmword ptr [rsp+$360]
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
        mov     rbx, qword ptr [out_]
        mov     r15, qword ptr [rsp+$2C0]
        movzx   r13d, byte ptr [flags]
        movzx   r12d, byte ptr [flags_end]
        test    rsi, $4
        je      @L07L03
        vbroadcasti128 ymm0, xmmword ptr [rcx]
        vbroadcasti128 ymm1, xmmword ptr [rcx+$10]
        vmovdqa ymm8, ymm0
        vmovdqa ymm9, ymm1
        vbroadcasti128 ymm12, xmmword ptr [rsp+$220]
        vbroadcasti128 ymm13, xmmword ptr [rsp+$240]
        vpunpckldq ymm14, ymm12, ymm13
        vpunpckhdq ymm15, ymm12, ymm13
        vpermq  ymm14, ymm14, $50
        vpermq  ymm15, ymm15, $50
        vbroadcasti128 ymm12, xmmword ptr [BLAKE3_BLOCK_LEN8+rip]
        vpblendd ymm14, ymm14, ymm12, $44
        vpblendd ymm15, ymm15, ymm12, $44
        vmovdqa ymmword ptr [rsp], ymm14
        vmovdqa ymmword ptr [rsp+$20], ymm15
        mov     r8, qword ptr [rdi]
        mov     r9, qword ptr [rdi+$8]
        mov     r10, qword ptr [rdi+$10]
        mov     r11, qword ptr [rdi+$18]
        movzx   eax, byte ptr [flags_start]
        or      eax, r13d
        xor     edx, edx
@L04L02:
        mov     r14d, eax
        or      eax, r12d
        add     rdx, 64
        cmp     rdx, r15
        cmovne  eax, r14d
        mov     dword ptr [rsp+$200], eax
        vmovups ymm2, ymmword ptr [r8+rdx-$40]
        vinsertf128 ymm2, ymm2, xmmword ptr [r9+rdx-$40], $01
        vmovups ymm3, ymmword ptr [r8+rdx-$30]
        vinsertf128 ymm3, ymm3, xmmword ptr [r9+rdx-$30], $01
        vshufps ymm4, ymm2, ymm3, 136
        vshufps ymm5, ymm2, ymm3, 221
        vmovups ymm2, ymmword ptr [r8+rdx-$20]
        vinsertf128 ymm2, ymm2, xmmword ptr [r9+rdx-$20], $01
        vmovups ymm3, ymmword ptr [r8+rdx-$10]
        vinsertf128 ymm3, ymm3, xmmword ptr [r9+rdx-$10], $01
        vshufps ymm6, ymm2, ymm3, 136
        vshufps ymm7, ymm2, ymm3, 221
        vpshufd ymm6, ymm6, $93
        vpshufd ymm7, ymm7, $93
        vmovups ymm10, ymmword ptr [r10+rdx-$40]
        vinsertf128 ymm10, ymm10, xmmword ptr [r11+rdx-$40], $01
        vmovups ymm11, ymmword ptr [r10+rdx-$30]
        vinsertf128 ymm11, ymm11, xmmword ptr [r11+rdx-$30], $01
        vshufps ymm12, ymm10, ymm11, 136
        vshufps ymm13, ymm10, ymm11, 221
        vmovups ymm10, ymmword ptr [r10+rdx-$20]
        vinsertf128 ymm10, ymm10, xmmword ptr [r11+rdx-$20], $01
        vmovups ymm11, ymmword ptr [r10+rdx-$10]
        vinsertf128 ymm11, ymm11, xmmword ptr [r11+rdx-$10], $01
        vshufps ymm14, ymm10, ymm11, 136
        vshufps ymm15, ymm10, ymm11, 221
        vpshufd ymm14, ymm14, $93
        vpshufd ymm15, ymm15, $93
        prefetcht0 [r8+rdx+$80]
        prefetcht0 [r9+rdx+$80]
        prefetcht0 [r10+rdx+$80]
        prefetcht0 [r11+rdx+$80]
        vpbroadcastd ymm2, dword ptr [rsp+$200]
        vmovdqa ymm3, ymmword ptr [rsp]
        vmovdqa ymm11, ymmword ptr [rsp+$20]
        vpblendd ymm3, ymm3, ymm2, $88
        vpblendd ymm11, ymm11, ymm2, $88
        vbroadcasti128 ymm2, xmmword ptr [BLAKE3_IV+rip]
        vmovdqa ymm10, ymm2
        mov     al, 7
@L05L09:
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm8, ymm8, ymm12
        vmovdqa ymmword ptr [rsp+$40], ymm4
        nop
        vmovdqa ymmword ptr [rsp+$60], ymm12
        nop
        vpaddd  ymm0, ymm0, ymm1
        vpaddd  ymm8, ymm8, ymm9
        vpxor   ymm3, ymm3, ymm0
        vpxor   ymm11, ymm11, ymm8
        vbroadcasti128 ymm4, xmmword ptr [ROT16+rip]
        vpshufb ymm3, ymm3, ymm4
        vpshufb ymm11, ymm11, ymm4
        vpaddd  ymm2, ymm2, ymm3
        vpaddd  ymm10, ymm10, ymm11
        vpxor   ymm1, ymm1, ymm2
        vpxor   ymm9, ymm9, ymm10
        vpsrld  ymm4, ymm1, 12
        vpslld  ymm1, ymm1, 20
        vpor    ymm1, ymm1, ymm4
        vpsrld  ymm4, ymm9, 12
        vpslld  ymm9, ymm9, 20
        vpor    ymm9, ymm9, ymm4
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm8, ymm8, ymm13
        vpaddd  ymm0, ymm0, ymm1
        vpaddd  ymm8, ymm8, ymm9
        vmovdqa ymmword ptr [rsp+$80], ymm5
        vmovdqa ymmword ptr [rsp+$A0], ymm13
        vpxor   ymm3, ymm3, ymm0
        vpxor   ymm11, ymm11, ymm8
        vbroadcasti128 ymm4, xmmword ptr [ROT8+rip]
        vpshufb ymm3, ymm3, ymm4
        vpshufb ymm11, ymm11, ymm4
        vpaddd  ymm2, ymm2, ymm3
        vpaddd  ymm10, ymm10, ymm11
        vpxor   ymm1, ymm1, ymm2
        vpxor   ymm9, ymm9, ymm10
        vpsrld  ymm4, ymm1, 7
        vpslld  ymm1, ymm1, 25
        vpor    ymm1, ymm1, ymm4
        vpsrld  ymm4, ymm9, 7
        vpslld  ymm9, ymm9, 25
        vpor    ymm9, ymm9, ymm4
        vpshufd ymm0, ymm0, $93
        vpshufd ymm8, ymm8, $93
        vpshufd ymm3, ymm3, $4E
        vpshufd ymm11, ymm11, $4E
        vpshufd ymm2, ymm2, $39
        vpshufd ymm10, ymm10, $39
        vpaddd  ymm0, ymm0, ymm6
        vpaddd  ymm8, ymm8, ymm14
        vpaddd  ymm0, ymm0, ymm1
        vpaddd  ymm8, ymm8, ymm9
        vpxor   ymm3, ymm3, ymm0
        vpxor   ymm11, ymm11, ymm8
        vbroadcasti128 ymm4, xmmword ptr [ROT16+rip]
        vpshufb ymm3, ymm3, ymm4
        vpshufb ymm11, ymm11, ymm4
        vpaddd  ymm2, ymm2, ymm3
        vpaddd  ymm10, ymm10, ymm11
        vpxor   ymm1, ymm1, ymm2
        vpxor   ymm9, ymm9, ymm10
        vpsrld  ymm4, ymm1, 12
        vpslld  ymm1, ymm1, 20
        vpor    ymm1, ymm1, ymm4
        vpsrld  ymm4, ymm9, 12
        vpslld  ymm9, ymm9, 20
        vpor    ymm9, ymm9, ymm4
        vpaddd  ymm0, ymm0, ymm7
        vpaddd  ymm8, ymm8, ymm15
        vpaddd  ymm0, ymm0, ymm1
        vpaddd  ymm8, ymm8, ymm9
        vpxor   ymm3, ymm3, ymm0
        vpxor   ymm11, ymm11, ymm8
        vbroadcasti128 ymm4, xmmword ptr [ROT8+rip]
        vpshufb ymm3, ymm3, ymm4
        vpshufb ymm11, ymm11, ymm4
        vpaddd  ymm2, ymm2, ymm3
        vpaddd  ymm10, ymm10, ymm11
        vpxor   ymm1, ymm1, ymm2
        vpxor   ymm9, ymm9, ymm10
        vpsrld  ymm4, ymm1, 7
        vpslld  ymm1, ymm1, 25
        vpor    ymm1, ymm1, ymm4
        vpsrld  ymm4, ymm9, 7
        vpslld  ymm9, ymm9, 25
        vpor    ymm9, ymm9, ymm4
        vpshufd ymm0, ymm0, $39
        vpshufd ymm8, ymm8, $39
        vpshufd ymm3, ymm3, $4E
        vpshufd ymm11, ymm11, $4E
        vpshufd ymm2, ymm2, $93
        vpshufd ymm10, ymm10, $93
        dec     al
        je      @L06L09
        vmovdqa ymm4, ymmword ptr [rsp+$40]
        vmovdqa ymm5, ymmword ptr [rsp+$80]
        vshufps ymm12, ymm4, ymm5, 214
        vpshufd ymm13, ymm4, $0F
        vpshufd ymm4, ymm12, $39
        vshufps ymm12, ymm6, ymm7, 250
        vpblendd ymm13, ymm13, ymm12, $AA
        vpunpcklqdq ymm12, ymm7, ymm5
        vpblendd ymm12, ymm12, ymm6, $88
        vpshufd ymm12, ymm12, $78
        vpunpckhdq ymm5, ymm5, ymm7
        vpunpckldq ymm6, ymm6, ymm5
        vpshufd ymm7, ymm6, $1E
        vmovdqa ymmword ptr [rsp+$40], ymm13
        vmovdqa ymmword ptr [rsp+$80], ymm12
        vmovdqa ymm12, ymmword ptr [rsp+$60]
        vmovdqa ymm13, ymmword ptr [rsp+$A0]
        vshufps ymm5, ymm12, ymm13, 214
        vpshufd ymm6, ymm12, $0F
        vpshufd ymm12, ymm5, $39
        vshufps ymm5, ymm14, ymm15, 250
        vpblendd ymm6, ymm6, ymm5, $AA
        vpunpcklqdq ymm5, ymm15, ymm13
        vpblendd ymm5, ymm5, ymm14, $88
        vpshufd ymm5, ymm5, $78
        vpunpckhdq ymm13, ymm13, ymm15
        vpunpckldq ymm14, ymm14, ymm13
        vpshufd ymm15, ymm14, $1E
        vmovdqa ymm13, ymm6
        vmovdqa ymm14, ymm5
        vmovdqa ymm5, ymmword ptr [rsp+$40]
        vmovdqa ymm6, ymmword ptr [rsp+$80]
        jmp     @L05L09
@L06L09:
        vpxor   ymm0, ymm0, ymm2
        vpxor   ymm1, ymm1, ymm3
        vpxor   ymm8, ymm8, ymm10
        vpxor   ymm9, ymm9, ymm11
        mov     eax, r13d
        cmp     rdx, r15
        jne     @L04L02
        vmovdqu xmmword ptr [rbx], xmm0
        vmovdqu xmmword ptr [rbx+$10], xmm1
        vextracti128 xmmword ptr [rbx+$20], ymm0, $01
        vextracti128 xmmword ptr [rbx+$30], ymm1, $01
        vmovdqu xmmword ptr [rbx+$40], xmm8
        vmovdqu xmmword ptr [rbx+$50], xmm9
        vextracti128 xmmword ptr [rbx+$60], ymm8, $01
        vextracti128 xmmword ptr [rbx+$70], ymm9, $01
        vmovaps xmm8, xmmword ptr [rsp+$260]
        vmovaps xmm0, xmmword ptr [rsp+$220]
        vmovaps xmm1, xmmword ptr [rsp+$230]
        vmovaps xmm2, xmmword ptr [rsp+$240]
        vmovaps xmm3, xmmword ptr [rsp+$250]
        vblendvps xmm0, xmm0, xmm1, xmm8
        vblendvps xmm2, xmm2, xmm3, xmm8
        vmovaps xmmword ptr [rsp+$220], xmm0
        vmovaps xmmword ptr [rsp+$240], xmm2
        add     rbx, 128
        add     rdi, 32
        sub     rsi, 4
@L07L03:
        test    rsi, $2
        je      @L0BL03
        vbroadcasti128 ymm0, xmmword ptr [rcx]
        vbroadcasti128 ymm1, xmmword ptr [rcx+$10]
        vmovd   xmm13, dword ptr [rsp+$220]
        vpinsrd xmm13, xmm13, dword ptr [rsp+$240], 1
        vpinsrd xmm13, xmm13, dword ptr [BLAKE3_BLOCK_LEN8+rip], 2
        vmovd   xmm14, dword ptr [rsp+$224]
        vpinsrd xmm14, xmm14, dword ptr [rsp+$244], 1
        vpinsrd xmm14, xmm14, dword ptr [BLAKE3_BLOCK_LEN8+rip], 2
        vinserti128 ymm13, ymm13, xmm14, $01
        vbroadcasti128 ymm14, xmmword ptr [ROT16+rip]
        vbroadcasti128 ymm15, xmmword ptr [ROT8+rip]
        mov     r8, qword ptr [rdi]
        mov     r9, qword ptr [rdi+$8]
        movzx   eax, byte ptr [flags_start]
        or      eax, r13d
        xor     edx, edx
@L08L02:
        mov     r14d, eax
        or      eax, r12d
        add     rdx, 64
        cmp     rdx, r15
        cmovne  eax, r14d
        mov     dword ptr [rsp+$200], eax
        vbroadcasti128 ymm2, xmmword ptr [BLAKE3_IV+rip]
        vpbroadcastd ymm8, dword ptr [rsp+$200]
        vpblendd ymm3, ymm13, ymm8, $88
        vmovups ymm8, ymmword ptr [r8+rdx-$40]
        vinsertf128 ymm8, ymm8, xmmword ptr [r9+rdx-$40], $01
        vmovups ymm9, ymmword ptr [r8+rdx-$30]
        vinsertf128 ymm9, ymm9, xmmword ptr [r9+rdx-$30], $01
        vshufps ymm4, ymm8, ymm9, 136
        vshufps ymm5, ymm8, ymm9, 221
        vmovups ymm8, ymmword ptr [r8+rdx-$20]
        vinsertf128 ymm8, ymm8, xmmword ptr [r9+rdx-$20], $01
        vmovups ymm9, ymmword ptr [r8+rdx-$10]
        vinsertf128 ymm9, ymm9, xmmword ptr [r9+rdx-$10], $01
        vshufps ymm6, ymm8, ymm9, 136
        vshufps ymm7, ymm8, ymm9, 221
        vpshufd ymm6, ymm6, $93
        vpshufd ymm7, ymm7, $93
        mov     al, 7
@L09L09:
        vpaddd  ymm0, ymm0, ymm4
        vpaddd  ymm0, ymm0, ymm1
        vpxor   ymm3, ymm3, ymm0
        vpshufb ymm3, ymm3, ymm14
        vpaddd  ymm2, ymm2, ymm3
        vpxor   ymm1, ymm1, ymm2
        vpsrld  ymm8, ymm1, 12
        vpslld  ymm1, ymm1, 20
        vpor    ymm1, ymm1, ymm8
        vpaddd  ymm0, ymm0, ymm5
        vpaddd  ymm0, ymm0, ymm1
        vpxor   ymm3, ymm3, ymm0
        vpshufb ymm3, ymm3, ymm15
        vpaddd  ymm2, ymm2, ymm3
        vpxor   ymm1, ymm1, ymm2
        vpsrld  ymm8, ymm1, 7
        vpslld  ymm1, ymm1, 25
        vpor    ymm1, ymm1, ymm8
        vpshufd ymm0, ymm0, $93
        vpshufd ymm3, ymm3, $4E
        vpshufd ymm2, ymm2, $39
        vpaddd  ymm0, ymm0, ymm6
        vpaddd  ymm0, ymm0, ymm1
        vpxor   ymm3, ymm3, ymm0
        vpshufb ymm3, ymm3, ymm14
        vpaddd  ymm2, ymm2, ymm3
        vpxor   ymm1, ymm1, ymm2
        vpsrld  ymm8, ymm1, 12
        vpslld  ymm1, ymm1, 20
        vpor    ymm1, ymm1, ymm8
        vpaddd  ymm0, ymm0, ymm7
        vpaddd  ymm0, ymm0, ymm1
        vpxor   ymm3, ymm3, ymm0
        vpshufb ymm3, ymm3, ymm15
        vpaddd  ymm2, ymm2, ymm3
        vpxor   ymm1, ymm1, ymm2
        vpsrld  ymm8, ymm1, 7
        vpslld  ymm1, ymm1, 25
        vpor    ymm1, ymm1, ymm8
        vpshufd ymm0, ymm0, $39
        vpshufd ymm3, ymm3, $4E
        vpshufd ymm2, ymm2, $93
        dec     al
        jz      @L0AL09
        vshufps ymm8, ymm4, ymm5, 214
        vpshufd ymm9, ymm4, $0F
        vpshufd ymm4, ymm8, $39
        vshufps ymm8, ymm6, ymm7, 250
        vpblendd ymm9, ymm9, ymm8, $AA
        vpunpcklqdq ymm8, ymm7, ymm5
        vpblendd ymm8, ymm8, ymm6, $88
        vpshufd ymm8, ymm8, $78
        vpunpckhdq ymm5, ymm5, ymm7
        vpunpckldq ymm6, ymm6, ymm5
        vpshufd ymm7, ymm6, $1E
        vmovdqa ymm5, ymm9
        vmovdqa ymm6, ymm8
        jmp     @L09L09
@L0AL09:
        vpxor   ymm0, ymm0, ymm2
        vpxor   ymm1, ymm1, ymm3
        mov     eax, r13d
        cmp     rdx, r15
        jne     @L08L02
        vmovdqu xmmword ptr [rbx], xmm0
        vmovdqu xmmword ptr [rbx+$10], xmm1
        vextracti128 xmmword ptr [rbx+$20], ymm0, $01
        vextracti128 xmmword ptr [rbx+$30], ymm1, $01
        vmovaps ymm8, ymmword ptr [rsp+$260]
        vmovaps ymm0, ymmword ptr [rsp+$220]
        vmovups ymm1, ymmword ptr [rsp+$228]
        vmovaps ymm2, ymmword ptr [rsp+$240]
        vmovups ymm3, ymmword ptr [rsp+$248]
        vblendvps ymm0, ymm0, ymm1, ymm8
        vblendvps ymm2, ymm2, ymm3, ymm8
        vmovaps ymmword ptr [rsp+$220], ymm0
        vmovaps ymmword ptr [rsp+$240], ymm2
        add     rbx, 64
        add     rdi, 16
        sub     rsi, 2
@L0BL03:
        test    rsi, $1
        je      @L02L04
        vmovdqu xmm0, xmmword ptr [rcx]
        vmovdqu xmm1, xmmword ptr [rcx+$10]
        vmovd   xmm3, dword ptr [rsp+$220]
        vpinsrd xmm3, xmm3, dword ptr [rsp+$240], 1
        vpinsrd xmm13, xmm3, dword ptr [BLAKE3_BLOCK_LEN8+rip], 2
        vmovdqa xmm14, xmmword ptr [ROT16+rip]
        vmovdqa xmm15, xmmword ptr [ROT8+rip]
        mov     r8, qword ptr [rdi]
        movzx   eax, byte ptr [flags_start]
        or      eax, r13d
        xor     edx, edx
@L0CL02:
        mov     r14d, eax
        or      eax, r12d
        add     rdx, 64
        cmp     rdx, r15
        cmovne  eax, r14d
        vmovdqa xmm2, xmmword ptr [BLAKE3_IV+rip]
        vmovdqa xmm3, xmm13
        vpinsrd xmm3, xmm3, eax, 3
        vmovups xmm8, xmmword ptr [r8+rdx-$40]
        vmovups xmm9, xmmword ptr [r8+rdx-$30]
        vshufps xmm4, xmm8, xmm9, 136
        vshufps xmm5, xmm8, xmm9, 221
        vmovups xmm8, xmmword ptr [r8+rdx-$20]
        vmovups xmm9, xmmword ptr [r8+rdx-$10]
        vshufps xmm6, xmm8, xmm9, 136
        vshufps xmm7, xmm8, xmm9, 221
        vpshufd xmm6, xmm6, $93
        vpshufd xmm7, xmm7, $93
        mov     al, 7
@L0DL09:
        vpaddd  xmm0, xmm0, xmm4
        vpaddd  xmm0, xmm0, xmm1
        vpxor   xmm3, xmm3, xmm0
        vpshufb xmm3, xmm3, xmm14
        vpaddd  xmm2, xmm2, xmm3
        vpxor   xmm1, xmm1, xmm2
        vpsrld  xmm8, xmm1, 12
        vpslld  xmm1, xmm1, 20
        vpor    xmm1, xmm1, xmm8
        vpaddd  xmm0, xmm0, xmm5
        vpaddd  xmm0, xmm0, xmm1
        vpxor   xmm3, xmm3, xmm0
        vpshufb xmm3, xmm3, xmm15
        vpaddd  xmm2, xmm2, xmm3
        vpxor   xmm1, xmm1, xmm2
        vpsrld  xmm8, xmm1, 7
        vpslld  xmm1, xmm1, 25
        vpor    xmm1, xmm1, xmm8
        vpshufd xmm0, xmm0, $93
        vpshufd xmm3, xmm3, $4E
        vpshufd xmm2, xmm2, $39
        vpaddd  xmm0, xmm0, xmm6
        vpaddd  xmm0, xmm0, xmm1
        vpxor   xmm3, xmm3, xmm0
        vpshufb xmm3, xmm3, xmm14
        vpaddd  xmm2, xmm2, xmm3
        vpxor   xmm1, xmm1, xmm2
        vpsrld  xmm8, xmm1, 12
        vpslld  xmm1, xmm1, 20
        vpor    xmm1, xmm1, xmm8
        vpaddd  xmm0, xmm0, xmm7
        vpaddd  xmm0, xmm0, xmm1
        vpxor   xmm3, xmm3, xmm0
        vpshufb xmm3, xmm3, xmm15
        vpaddd  xmm2, xmm2, xmm3
        vpxor   xmm1, xmm1, xmm2
        vpsrld  xmm8, xmm1, 7
        vpslld  xmm1, xmm1, 25
        vpor    xmm1, xmm1, xmm8
        vpshufd xmm0, xmm0, $39
        vpshufd xmm3, xmm3, $4E
        vpshufd xmm2, xmm2, $93
        dec     al
        jz      @L0EL09
        vshufps xmm8, xmm4, xmm5, 214
        vpshufd xmm9, xmm4, $0F
        vpshufd xmm4, xmm8, $39
        vshufps xmm8, xmm6, xmm7, 250
        vpblendd xmm9, xmm9, xmm8, $AA
        vpunpcklqdq xmm8, xmm7, xmm5
        vpblendd xmm8, xmm8, xmm6, $88
        vpshufd xmm8, xmm8, $78
        vpunpckhdq xmm5, xmm5, xmm7
        vpunpckldq xmm6, xmm6, xmm5
        vpshufd xmm7, xmm6, $1E
        vmovdqa xmm5, xmm9
        vmovdqa xmm6, xmm8
        jmp     @L0DL09
@L0EL09:
        vpxor   xmm0, xmm0, xmm2
        vpxor   xmm1, xmm1, xmm3
        mov     eax, r13d
        cmp     rdx, r15
        jne     @L0CL02
        vmovdqu xmmword ptr [rbx], xmm0
        vmovdqu xmmword ptr [rbx+$10], xmm1
        jmp     @L02L04
end;
