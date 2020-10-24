const
  BLAKE2S_LC0: array[0..15] of cuint8 = (2,  3,  0, 1, 6,  7,  4,  5,
                                         10, 11, 8, 9, 14, 15, 12, 13);
  BLAKE2S_LC1: array[0..15] of cuint8 = (1, 2,  3,  0, 5,  6,  7,  4,
                                         9, 10, 11, 8, 13, 14, 15, 12);

  BLAKE2B_LC0: array[0..15] of cuint8 = (3,  4,  5,  6,  7,  0, 1, 2,
                                         11, 12, 13, 14, 15, 8, 9, 10);
  BLAKE2B_LC1: array[0..15] of cuint8 = (2,  3,  4,  5,  6,  7,  0, 1,
                                         10, 11, 12, 13, 14, 15, 8, 9);

function blake2s_compress_avx( S: Pblake2s_state; const block: pcuint8 ): cint; assembler; nostackframe;
asm
{$IF DEFINED(UNIX)}
  pushq	%rcx
  movq	%rdi,	%rcx
  pushq	%rdx
  movq	%rsi,	%rdx
{$ENDIF}
  subq	$168, %rsp
  vmovaps	%xmm6, (%rsp)
  vmovaps	%xmm7, 16(%rsp)
  vmovaps	%xmm8, 32(%rsp)
  vmovaps	%xmm9, 48(%rsp)
  vmovaps	%xmm10, 64(%rsp)
  vmovaps	%xmm11, 80(%rsp)
  vmovaps	%xmm12, 96(%rsp)
  vmovaps	%xmm13, 112(%rsp)
  vmovaps	%xmm14, 128(%rsp)
  vmovaps	%xmm15, 144(%rsp)
  vmovdqu	(%rdx), %xmm4
  vmovdqu	16(%rdx), %xmm3
  vmovdqu	16(%rcx), %xmm9
  vpaddd	(%rcx), %xmm9, %xmm1
  vshufps	$136, %xmm3, %xmm4, %xmm0
  vmovdqu	32(%rcx), %xmm7
  vshufps	$221, %xmm3, %xmm4, %xmm10
  vpaddd	%xmm0, %xmm1, %xmm1
  vpxor	blake2s_IV_2(%rip), %xmm7, %xmm0
  vmovdqu	32(%rdx), %xmm2
  vmovdqa	BLAKE2S_LC0(%rip), %xmm7
  vpxor	%xmm1, %xmm0, %xmm0
  vpaddd	%xmm10, %xmm1, %xmm1
  vmovdqu	48(%rdx), %xmm5
  vpsrldq	$4, %xmm2, %xmm15
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	blake2s_IV(%rip), %xmm0, %xmm8
  vpshufd	$30, %xmm5, %xmm13
  vpblendw	$12, %xmm3, %xmm5, %xmm14
  vpxor	%xmm8, %xmm9, %xmm6
  vpslld	$20, %xmm6, %xmm11
  vpsrld	$12, %xmm6, %xmm6
  vpxor	%xmm11, %xmm6, %xmm11
  vmovdqa	BLAKE2S_LC1(%rip), %xmm6
  vpaddd	%xmm11, %xmm1, %xmm1
  vpxor	%xmm1, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpshufd	$78, %xmm0, %xmm0
  vpxor	%xmm8, %xmm11, %xmm11
  vpslld	$25, %xmm11, %xmm10
  vpshufd	$57, %xmm8, %xmm8
  vpsrld	$7, %xmm11, %xmm11
  vpxor	%xmm10, %xmm11, %xmm11
  vpshufd	$147, %xmm1, %xmm10
  vpshufd	$225, %xmm2, %xmm1
  vpblendw	$195, %xmm13, %xmm1, %xmm12
  vpaddd	%xmm12, %xmm10, %xmm10
  vpblendw	$60, %xmm13, %xmm1, %xmm1
  vpblendw	$192, %xmm5, %xmm3, %xmm13
  vpaddd	%xmm11, %xmm10, %xmm10
  vpshufd	$177, %xmm1, %xmm1
  vpxor	%xmm10, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpxor	%xmm8, %xmm11, %xmm11
  vpslld	$20, %xmm11, %xmm12
  vpsrld	$12, %xmm11, %xmm11
  vpxor	%xmm12, %xmm11, %xmm11
  vpaddd	%xmm11, %xmm1, %xmm1
  vpaddd	%xmm10, %xmm1, %xmm1
  vpxor	%xmm1, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpshufd	$57, %xmm1, %xmm10
  vpxor	%xmm8, %xmm11, %xmm11
  vpslld	$25, %xmm11, %xmm12
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$147, %xmm8, %xmm8
  vpsrld	$7, %xmm11, %xmm11
  vpxor	%xmm12, %xmm11, %xmm11
  vpslldq	$4, %xmm5, %xmm1
  vpblendw	$12, %xmm2, %xmm3, %xmm12
  vpblendw	$240, %xmm1, %xmm12, %xmm12
  vpshufd	$147, %xmm12, %xmm12
  vpaddd	%xmm12, %xmm10, %xmm10
  vpaddd	%xmm11, %xmm10, %xmm10
  vpxor	%xmm10, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpxor	%xmm8, %xmm11, %xmm12
  vpslld	$20, %xmm12, %xmm11
  vpsrld	$12, %xmm12, %xmm12
  vpxor	%xmm11, %xmm12, %xmm12
  vpshufd	$8, %xmm2, %xmm11
  vpblendw	$240, %xmm13, %xmm11, %xmm11
  vpshufd	$177, %xmm11, %xmm11
  vpaddd	%xmm12, %xmm11, %xmm11
  vpaddd	%xmm10, %xmm11, %xmm10
  vpxor	%xmm10, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpshufd	$147, %xmm10, %xmm10
  vpxor	%xmm8, %xmm12, %xmm12
  vpslld	$25, %xmm12, %xmm11
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$57, %xmm8, %xmm8
  vpsrld	$7, %xmm12, %xmm12
  vpxor	%xmm11, %xmm12, %xmm12
  vpslldq	$4, %xmm3, %xmm11
  vpblendw	$48, %xmm11, %xmm2, %xmm11
  vpblendw	$240, %xmm11, %xmm4, %xmm11
  vpshufd	$198, %xmm11, %xmm11
  vpaddd	%xmm11, %xmm10, %xmm11
  vpaddd	%xmm12, %xmm11, %xmm11
  vpxor	%xmm11, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpxor	%xmm8, %xmm12, %xmm10
  vpslld	$20, %xmm10, %xmm12
  vpsrld	$12, %xmm10, %xmm10
  vpxor	%xmm12, %xmm10, %xmm10
  vpunpckhdq	%xmm3, %xmm4, %xmm12
  vpblendw	$12, %xmm1, %xmm12, %xmm13
  vpshufd	$198, %xmm13, %xmm13
  vpaddd	%xmm10, %xmm13, %xmm13
  vpaddd	%xmm11, %xmm13, %xmm11
  vpxor	%xmm11, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpshufd	$57, %xmm11, %xmm11
  vpxor	%xmm8, %xmm10, %xmm10
  vpslld	$25, %xmm10, %xmm13
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$147, %xmm8, %xmm8
  vpsrld	$7, %xmm10, %xmm10
  vpxor	%xmm13, %xmm10, %xmm10
  vpunpckhdq	%xmm5, %xmm2, %xmm13
  vpblendw	$15, %xmm14, %xmm13, %xmm13
  vpslldq	$8, %xmm5, %xmm14
  vpshufd	$210, %xmm13, %xmm13
  vpaddd	%xmm13, %xmm11, %xmm11
  vpaddd	%xmm10, %xmm11, %xmm13
  vpxor	%xmm13, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpxor	%xmm8, %xmm10, %xmm10
  vpslld	$20, %xmm10, %xmm11
  vpsrld	$12, %xmm10, %xmm10
  vpxor	%xmm11, %xmm10, %xmm10
  vpunpckldq	%xmm4, %xmm2, %xmm11
  vpblendw	$240, %xmm4, %xmm11, %xmm11
  vpblendw	$192, %xmm14, %xmm11, %xmm11
  vpsrldq	$12, %xmm3, %xmm14
  vpaddd	%xmm11, %xmm10, %xmm11
  vpaddd	%xmm13, %xmm11, %xmm11
  vpxor	%xmm11, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpshufd	$147, %xmm11, %xmm11
  vpxor	%xmm8, %xmm10, %xmm10
  vpslld	$25, %xmm10, %xmm13
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$57, %xmm8, %xmm8
  vpsrld	$7, %xmm10, %xmm10
  vpxor	%xmm13, %xmm10, %xmm10
  vpblendw	$60, %xmm2, %xmm4, %xmm13
  vpblendw	$3, %xmm14, %xmm13, %xmm13
  vpunpckhdq	%xmm2, %xmm12, %xmm14
  vpblendw	$12, %xmm5, %xmm14, %xmm14
  vpshufd	$57, %xmm13, %xmm13
  vpaddd	%xmm13, %xmm11, %xmm11
  vpshufd	$210, %xmm14, %xmm14
  vpaddd	%xmm10, %xmm11, %xmm11
  vpxor	%xmm11, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm8, %xmm8
  vpxor	%xmm8, %xmm10, %xmm10
  vpslld	$20, %xmm10, %xmm13
  vpsrld	$12, %xmm10, %xmm10
  vpxor	%xmm13, %xmm10, %xmm10
  vpblendw	$51, %xmm3, %xmm4, %xmm13
  vpblendw	$192, %xmm1, %xmm13, %xmm1
  vpshufd	$108, %xmm1, %xmm1
  vpaddd	%xmm10, %xmm1, %xmm1
  vpaddd	%xmm11, %xmm1, %xmm11
  vpxor	%xmm11, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm13
  vpaddd	%xmm13, %xmm8, %xmm8
  vpshufd	$57, %xmm11, %xmm11
  vpxor	%xmm8, %xmm10, %xmm10
  vpaddd	%xmm14, %xmm11, %xmm11
  vpslld	$25, %xmm10, %xmm0
  vpshufd	$78, %xmm13, %xmm13
  vpsrld	$7, %xmm10, %xmm10
  vpshufd	$147, %xmm8, %xmm8
  vpxor	%xmm0, %xmm10, %xmm10
  vpaddd	%xmm10, %xmm11, %xmm11
  vpxor	%xmm11, %xmm13, %xmm13
  vpshufb	%xmm7, %xmm13, %xmm1
  vpaddd	%xmm1, %xmm8, %xmm8
  vpxor	%xmm8, %xmm10, %xmm0
  vpslld	$20, %xmm0, %xmm13
  vpblendw	$12, %xmm4, %xmm5, %xmm10
  vpsrld	$12, %xmm0, %xmm0
  vpxor	%xmm13, %xmm0, %xmm0
  vpslldq	$8, %xmm2, %xmm13
  vpblendw	$192, %xmm13, %xmm10, %xmm10
  vpshufd	$135, %xmm10, %xmm10
  vpaddd	%xmm0, %xmm10, %xmm10
  vpaddd	%xmm11, %xmm10, %xmm10
  vpxor	%xmm10, %xmm1, %xmm1
  vpshufb	%xmm6, %xmm1, %xmm14
  vpaddd	%xmm14, %xmm8, %xmm8
  vpshufd	$147, %xmm10, %xmm10
  vpxor	%xmm8, %xmm0, %xmm0
  vpslld	$25, %xmm0, %xmm13
  vpshufd	$57, %xmm8, %xmm8
  vpshufd	$78, %xmm14, %xmm14
  vpsrld	$7, %xmm0, %xmm0
  vpxor	%xmm13, %xmm0, %xmm0
  vpblendw	$15, %xmm3, %xmm4, %xmm13
  vpblendw	$192, %xmm5, %xmm13, %xmm13
  vpshufd	$27, %xmm13, %xmm13
  vpaddd	%xmm13, %xmm10, %xmm10
  vpaddd	%xmm0, %xmm10, %xmm11
  vpalignr	$4, %xmm3, %xmm4, %xmm10
  vpxor	%xmm11, %xmm14, %xmm14
  vpshufb	%xmm7, %xmm14, %xmm14
  vpaddd	%xmm14, %xmm8, %xmm1
  vpblendw	$51, %xmm2, %xmm10, %xmm10
  vpxor	%xmm1, %xmm0, %xmm13
  vpslld	$20, %xmm13, %xmm0
  vpsrld	$12, %xmm13, %xmm13
  vpxor	%xmm0, %xmm13, %xmm13
  vpaddd	%xmm10, %xmm13, %xmm10
  vpaddd	%xmm11, %xmm10, %xmm10
  vpunpcklqdq	%xmm2, %xmm3, %xmm11
  vpxor	%xmm10, %xmm14, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpunpcklqdq	%xmm3, %xmm4, %xmm14
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$25, %xmm13, %xmm8
  vpshufd	$57, %xmm10, %xmm10
  vpshufd	$78, %xmm0, %xmm0
  vpsrld	$7, %xmm13, %xmm13
  vpshufd	$147, %xmm1, %xmm1
  vpxor	%xmm8, %xmm13, %xmm13
  vpunpckhqdq	%xmm2, %xmm4, %xmm8
  vpblendw	$51, %xmm8, %xmm11, %xmm8
  vpshufd	$135, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm10, %xmm10
  vpaddd	%xmm13, %xmm10, %xmm10
  vpxor	%xmm10, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$20, %xmm13, %xmm8
  vpsrld	$12, %xmm13, %xmm13
  vpxor	%xmm8, %xmm13, %xmm13
  vpunpckhqdq	%xmm5, %xmm3, %xmm8
  vpblendw	$51, %xmm14, %xmm8, %xmm8
  vpunpckhqdq	%xmm4, %xmm2, %xmm14
  vpaddd	%xmm8, %xmm13, %xmm8
  vpaddd	%xmm10, %xmm8, %xmm8
  vpxor	%xmm8, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpshufd	$147, %xmm8, %xmm8
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$25, %xmm13, %xmm10
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$57, %xmm1, %xmm1
  vpsrld	$7, %xmm13, %xmm13
  vpxor	%xmm10, %xmm13, %xmm13
  vpunpckhqdq	%xmm3, %xmm5, %xmm10
  vpblendw	$51, %xmm10, %xmm14, %xmm14
  vpblendw	$3, %xmm2, %xmm4, %xmm10
  vpshufd	$147, %xmm14, %xmm14
  vpaddd	%xmm14, %xmm8, %xmm8
  vpaddd	%xmm13, %xmm8, %xmm14
  vpxor	%xmm14, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$20, %xmm13, %xmm8
  vpsrld	$12, %xmm13, %xmm13
  vpxor	%xmm8, %xmm13, %xmm13
  vpslldq	$8, %xmm10, %xmm8
  vpblendw	$15, %xmm5, %xmm8, %xmm8
  vpshufd	$141, %xmm8, %xmm8
  vpaddd	%xmm13, %xmm8, %xmm8
  vpaddd	%xmm14, %xmm8, %xmm8
  vpxor	%xmm8, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpshufd	$57, %xmm8, %xmm8
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$25, %xmm13, %xmm14
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$147, %xmm1, %xmm1
  vpsrld	$7, %xmm13, %xmm13
  vpxor	%xmm14, %xmm13, %xmm13
  vpunpckldq	%xmm2, %xmm4, %xmm14
  vpunpcklqdq	%xmm14, %xmm12, %xmm14
  vpaddd	%xmm14, %xmm8, %xmm8
  vpaddd	%xmm13, %xmm8, %xmm8
  vpblendw	$15, %xmm5, %xmm12, %xmm12
  vpxor	%xmm8, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpshufd	$141, %xmm12, %xmm12
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$20, %xmm13, %xmm14
  vpsrld	$12, %xmm13, %xmm13
  vpxor	%xmm14, %xmm13, %xmm13
  vpblendw	$3, %xmm5, %xmm4, %xmm14
  vpblendw	$60, %xmm15, %xmm14, %xmm14
  vpaddd	%xmm14, %xmm13, %xmm14
  vpaddd	%xmm8, %xmm14, %xmm8
  vpxor	%xmm8, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpshufd	$147, %xmm8, %xmm8
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$25, %xmm13, %xmm15
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$57, %xmm1, %xmm1
  vpsrld	$7, %xmm13, %xmm13
  vpxor	%xmm15, %xmm13, %xmm14
  vpblendw	$12, %xmm4, %xmm3, %xmm15
  vpsrldq	$4, %xmm5, %xmm13
  vpblendw	$48, %xmm13, %xmm15, %xmm15
  vpshufd	$177, %xmm15, %xmm15
  vpaddd	%xmm15, %xmm8, %xmm8
  vpaddd	%xmm14, %xmm8, %xmm15
  vpxor	%xmm15, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpxor	%xmm1, %xmm14, %xmm14
  vpslld	$20, %xmm14, %xmm13
  vpsrld	$12, %xmm14, %xmm14
  vpxor	%xmm13, %xmm14, %xmm8
  vpunpcklqdq	%xmm3, %xmm2, %xmm13
  vpsrldq	$4, %xmm13, %xmm13
  vpshufd	$132, %xmm5, %xmm14
  vpblendw	$51, %xmm13, %xmm14, %xmm13
  vpaddd	%xmm13, %xmm8, %xmm13
  vpaddd	%xmm15, %xmm13, %xmm13
  vpblendw	$51, %xmm5, %xmm4, %xmm15
  vpxor	%xmm13, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpshufd	$57, %xmm13, %xmm13
  vpxor	%xmm1, %xmm8, %xmm8
  vpslld	$25, %xmm8, %xmm14
  vpshufd	$78, %xmm0, %xmm0
  vpshufd	$147, %xmm1, %xmm1
  vpsrld	$7, %xmm8, %xmm8
  vpxor	%xmm14, %xmm8, %xmm8
  vpslldq	$12, %xmm3, %xmm14
  vpblendw	$192, %xmm14, %xmm15, %xmm15
  vpsrldq	$4, %xmm3, %xmm14
  vpaddd	%xmm15, %xmm13, %xmm13
  vpaddd	%xmm8, %xmm13, %xmm15
  vpxor	%xmm15, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpxor	%xmm1, %xmm8, %xmm8
  vpslld	$20, %xmm8, %xmm13
  vpsrld	$12, %xmm8, %xmm8
  vpxor	%xmm13, %xmm8, %xmm8
  vpblendw	$48, %xmm2, %xmm5, %xmm13
  vpblendw	$3, %xmm14, %xmm13, %xmm13
  vpshufd	$156, %xmm13, %xmm13
  vpaddd	%xmm8, %xmm13, %xmm13
  vpaddd	%xmm15, %xmm13, %xmm13
  vpxor	%xmm13, %xmm0, %xmm0
  vpshufb	%xmm6, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm1, %xmm1
  vpshufd	$147, %xmm13, %xmm13
  vpxor	%xmm1, %xmm8, %xmm8
  vpslld	$25, %xmm8, %xmm15
  vpshufd	$78, %xmm0, %xmm0
  vpsrld	$7, %xmm8, %xmm8
  vpxor	%xmm15, %xmm8, %xmm8
  vpshufd	$57, %xmm1, %xmm15
  vpunpcklqdq	%xmm2, %xmm4, %xmm1
  vpblendw	$12, %xmm14, %xmm1, %xmm1
  vpshufd	$210, %xmm1, %xmm1
  vpaddd	%xmm1, %xmm13, %xmm13
  vpunpckhdq	%xmm2, %xmm3, %xmm14
  vpaddd	%xmm8, %xmm13, %xmm13
  vpxor	%xmm13, %xmm0, %xmm0
  vpshufb	%xmm7, %xmm0, %xmm1
  vpaddd	%xmm1, %xmm15, %xmm15
  vpxor	%xmm15, %xmm8, %xmm8
  vpslld	$20, %xmm8, %xmm0
  vpsrld	$12, %xmm8, %xmm8
  vpxor	%xmm0, %xmm8, %xmm8
  vpunpckhqdq	%xmm14, %xmm4, %xmm0
  vpshufd	$27, %xmm0, %xmm0
  vpaddd	%xmm8, %xmm0, %xmm0
  vpaddd	%xmm13, %xmm0, %xmm13
  vpxor	%xmm13, %xmm1, %xmm1
  vpshufb	%xmm6, %xmm1, %xmm1
  vpaddd	%xmm1, %xmm15, %xmm15
  vpshufd	$57, %xmm13, %xmm13
  vpxor	%xmm15, %xmm8, %xmm8
  vpaddd	%xmm12, %xmm13, %xmm13
  vpslld	$25, %xmm8, %xmm0
  vpshufd	$147, %xmm15, %xmm15
  vpsrld	$7, %xmm8, %xmm8
  vpshufd	$78, %xmm1, %xmm1
  vpxor	%xmm0, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm13, %xmm13
  vpxor	%xmm13, %xmm1, %xmm1
  vpshufb	%xmm7, %xmm1, %xmm12
  vpaddd	%xmm12, %xmm15, %xmm0
  vpsrldq	$4, %xmm4, %xmm15
  vpxor	%xmm0, %xmm8, %xmm8
  vpslld	$20, %xmm8, %xmm1
  vpsrld	$12, %xmm8, %xmm8
  vpxor	%xmm8, %xmm1, %xmm8
  vpblendw	$48, %xmm5, %xmm2, %xmm1
  vpblendw	$3, %xmm15, %xmm1, %xmm1
  vmovaps	144(%rsp), %xmm15
  vpshufd	$75, %xmm1, %xmm1
  vpaddd	%xmm8, %xmm1, %xmm1
  vpaddd	%xmm13, %xmm1, %xmm13
  vpxor	%xmm12, %xmm13, %xmm12
  vpshufb	%xmm6, %xmm12, %xmm12
  vpaddd	%xmm12, %xmm0, %xmm0
  vpshufd	$147, %xmm13, %xmm13
  vpxor	%xmm0, %xmm8, %xmm8
  vpslld	$25, %xmm8, %xmm1
  vpshufd	$78, %xmm12, %xmm12
  vpshufd	$57, %xmm0, %xmm0
  vpsrld	$7, %xmm8, %xmm8
  vpxor	%xmm8, %xmm1, %xmm8
  vpunpckhqdq	%xmm5, %xmm4, %xmm1
  vpblendw	$60, %xmm11, %xmm1, %xmm11
  vpshufd	$180, %xmm11, %xmm11
  vpaddd	%xmm13, %xmm11, %xmm13
  vpaddd	%xmm8, %xmm13, %xmm13
  vpxor	%xmm12, %xmm13, %xmm12
  vpshufb	%xmm7, %xmm12, %xmm1
  vpaddd	%xmm1, %xmm0, %xmm0
  vpunpckldq	%xmm3, %xmm4, %xmm12
  vpxor	%xmm0, %xmm8, %xmm8
  vpslld	$20, %xmm8, %xmm11
  vpunpcklqdq	%xmm14, %xmm12, %xmm12
  vpshufd	$147, %xmm12, %xmm12
  vpsrld	$12, %xmm8, %xmm8
  vpxor	%xmm8, %xmm11, %xmm11
  vpaddd	%xmm11, %xmm12, %xmm12
  vpsrldq	$8, %xmm2, %xmm14
  vpaddd	%xmm13, %xmm12, %xmm12
  vpxor	%xmm1, %xmm12, %xmm8
  vpshufb	%xmm6, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm0, %xmm1
  vpshufd	$57, %xmm12, %xmm12
  vpxor	%xmm1, %xmm11, %xmm0
  vpslld	$25, %xmm0, %xmm13
  vpshufd	$78, %xmm8, %xmm8
  vpshufd	$147, %xmm1, %xmm1
  vpsrld	$7, %xmm0, %xmm11
  vpunpckhdq	%xmm5, %xmm3, %xmm0
  vpunpcklqdq	%xmm4, %xmm0, %xmm0
  vpblendw	$192, %xmm2, %xmm0, %xmm0
  vpshufhw	$78, %xmm0, %xmm0
  vpaddd	%xmm0, %xmm12, %xmm12
  vpxor	%xmm11, %xmm13, %xmm11
  vpaddd	%xmm11, %xmm12, %xmm12
  vpxor	%xmm8, %xmm12, %xmm8
  vpshufb	%xmm7, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm1, %xmm1
  vpxor	%xmm1, %xmm11, %xmm11
  vpslld	$20, %xmm11, %xmm13
  vpsrld	$12, %xmm11, %xmm11
  vpxor	%xmm11, %xmm13, %xmm13
  vpunpckhdq	%xmm5, %xmm4, %xmm11
  vpblendw	$240, %xmm11, %xmm2, %xmm0
  vpshufd	$39, %xmm0, %xmm0
  vpaddd	%xmm13, %xmm0, %xmm0
  vpaddd	%xmm12, %xmm0, %xmm12
  vpxor	%xmm8, %xmm12, %xmm8
  vpshufb	%xmm6, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm1, %xmm1
  vpshufd	$147, %xmm12, %xmm12
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$25, %xmm13, %xmm0
  vpshufd	$78, %xmm8, %xmm8
  vpshufd	$57, %xmm1, %xmm1
  vpsrld	$7, %xmm13, %xmm13
  vpxor	%xmm13, %xmm0, %xmm13
  vpunpcklqdq	%xmm5, %xmm4, %xmm0
  vpblendw	$3, %xmm14, %xmm0, %xmm0
  vpshufd	$120, %xmm0, %xmm0
  vpaddd	%xmm12, %xmm0, %xmm12
  vpaddd	%xmm13, %xmm12, %xmm12
  vpxor	%xmm8, %xmm12, %xmm8
  vpshufb	%xmm7, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm1, %xmm1
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$20, %xmm13, %xmm0
  vpsrld	$12, %xmm13, %xmm13
  vpxor	%xmm13, %xmm0, %xmm13
  vpblendw	$48, %xmm4, %xmm3, %xmm0
  vpshufd	$57, %xmm0, %xmm0
  vpaddd	%xmm13, %xmm0, %xmm0
  vpaddd	%xmm12, %xmm0, %xmm0
  vpxor	%xmm8, %xmm0, %xmm8
  vpshufb	%xmm6, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm1, %xmm1
  vpshufd	$57, %xmm0, %xmm0
  vpxor	%xmm1, %xmm13, %xmm14
  vpslld	$25, %xmm14, %xmm12
  vpshufd	$78, %xmm8, %xmm8
  vpshufd	$147, %xmm1, %xmm1
  vpsrld	$7, %xmm14, %xmm13
  vmovaps	128(%rsp), %xmm14
  vpxor	%xmm13, %xmm12, %xmm13
  vpblendw	$48, %xmm2, %xmm3, %xmm12
  vpblendw	$15, %xmm10, %xmm12, %xmm10
  vpshufd	$114, %xmm10, %xmm10
  vpaddd	%xmm0, %xmm10, %xmm0
  vpaddd	%xmm13, %xmm0, %xmm10
  vpslldq	$4, %xmm4, %xmm0
  vpunpckldq	%xmm5, %xmm4, %xmm4
  vpxor	%xmm8, %xmm10, %xmm8
  vpshufb	%xmm7, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm1, %xmm1
  vpblendw	$192, %xmm0, %xmm3, %xmm0
  vpxor	%xmm1, %xmm13, %xmm13
  vpslld	$20, %xmm13, %xmm12
  vpshufd	$99, %xmm0, %xmm0
  vpsrld	$12, %xmm13, %xmm13
  vpxor	%xmm13, %xmm12, %xmm12
  vpaddd	%xmm12, %xmm0, %xmm0
  vmovaps	112(%rsp), %xmm13
  vpaddd	%xmm10, %xmm0, %xmm0
  vmovaps	64(%rsp), %xmm10
  vpxor	%xmm8, %xmm0, %xmm8
  vpshufb	%xmm6, %xmm8, %xmm8
  vpaddd	%xmm8, %xmm1, %xmm1
  vpshufd	$147, %xmm0, %xmm0
  vpxor	%xmm1, %xmm12, %xmm12
  vpslld	$25, %xmm12, %xmm3
  vpshufd	$78, %xmm8, %xmm8
  vpshufd	$57, %xmm1, %xmm1
  vpsrld	$7, %xmm12, %xmm12
  vpxor	%xmm12, %xmm3, %xmm12
  vpunpckldq	%xmm5, %xmm2, %xmm3
  vpunpckhqdq	%xmm3, %xmm11, %xmm11
  vpshufd	$39, %xmm11, %xmm11
  vpaddd	%xmm0, %xmm11, %xmm0
  vpaddd	%xmm12, %xmm0, %xmm11
  vpblendw	$192, %xmm2, %xmm5, %xmm0
  vpxor	%xmm8, %xmm11, %xmm8
  vpshufb	%xmm7, %xmm8, %xmm7
  vpaddd	%xmm7, %xmm1, %xmm1
  vpblendw	$15, %xmm4, %xmm0, %xmm0
  vpxor	%xmm1, %xmm12, %xmm12
  vpslld	$20, %xmm12, %xmm3
  vpshufd	$108, %xmm0, %xmm0
  vmovaps	32(%rsp), %xmm8
  vpsrld	$12, %xmm12, %xmm12
  vpxor	%xmm12, %xmm3, %xmm3
  vpaddd	%xmm3, %xmm0, %xmm0
  vmovaps	96(%rsp), %xmm12
  vpaddd	%xmm11, %xmm0, %xmm0
  vmovaps	80(%rsp), %xmm11
  vpxor	%xmm7, %xmm0, %xmm7
  vpshufb	%xmm6, %xmm7, %xmm6
  vpaddd	%xmm6, %xmm1, %xmm1
  vmovaps	16(%rsp), %xmm7
  vpxor	%xmm1, %xmm3, %xmm3
  vpslld	$25, %xmm3, %xmm2
  vpshufd	$78, %xmm6, %xmm6
  vpxor	%xmm6, %xmm9, %xmm6
  vpsrld	$7, %xmm3, %xmm3
  vmovaps	48(%rsp), %xmm9
  vpshufd	$147, %xmm1, %xmm1
  vpshufd	$57, %xmm0, %xmm0
  vpxor	%xmm3, %xmm2, %xmm3
  vpxor	%xmm3, %xmm6, %xmm6
  vmovups	%xmm6, 16(%rcx)
  vpxor	%xmm0, %xmm1, %xmm0
  vmovaps	(%rsp), %xmm6
  vpxor	(%rcx), %xmm0, %xmm0
  vmovups	%xmm0, (%rcx)
  addq	$168, %rsp
{$IF DEFINED(UNIX)}
  popq	%rdx
  popq	%rcx
{$ENDIF}
end;

procedure blake2b_compress_avx( S: Pblake2b_state; const block: pcuint8 ); assembler; nostackframe;
asm
{$IF DEFINED(UNIX)}
  pushq	%rcx
  movq	%rdi,	%rcx
  pushq	%rdx
  movq	%rsi,	%rdx
{$ENDIF}
  subq	$568, %rsp
  vmovaps	%xmm6, 400(%rsp)
  vmovaps	%xmm7, 416(%rsp)
  vmovaps	%xmm8, 432(%rsp)
  vmovaps	%xmm9, 448(%rsp)
  vmovaps	%xmm10, 464(%rsp)
  vmovaps	%xmm11, 480(%rsp)
  vmovaps	%xmm12, 496(%rsp)
  vmovaps	%xmm13, 512(%rsp)
  vmovaps	%xmm14, 528(%rsp)
  vmovaps	%xmm15, 544(%rsp)
  vmovdqu	112(%rdx), %xmm5
  vmovdqu	16(%rdx), %xmm1
  vmovdqu	(%rdx), %xmm4
  vmovdqu	32(%rcx), %xmm3
  vpaddq	(%rcx), %xmm3, %xmm9
  vmovdqu	32(%rdx), %xmm13
  vmovaps	%xmm5, 80(%rsp)
  vpunpcklqdq	%xmm1, %xmm4, %xmm2
  vmovdqu	48(%rdx), %xmm12
  vpaddq	%xmm2, %xmm9, %xmm9
  vmovaps	%xmm4, 48(%rsp)
  vpunpckhqdq	%xmm1, %xmm4, %xmm4
  vmovdqa	BLAKE2B_LC0(%rip), %xmm5
  vmovaps	%xmm1, (%rsp)
  vmovdqu	48(%rcx), %xmm11
  vpaddq	16(%rcx), %xmm11, %xmm11
  vpunpcklqdq	%xmm12, %xmm13, %xmm0
  vmovaps	%xmm2, 272(%rsp)
  vmovdqu	64(%rcx), %xmm14
  vpxor	blake2b_IV_3(%rip), %xmm14, %xmm2
  vpxor	%xmm9, %xmm2, %xmm2
  vmovaps	%xmm0, 288(%rsp)
  vpaddq	%xmm0, %xmm11, %xmm11
  vpshufd	$177, %xmm2, %xmm2
  vmovdqu	80(%rdx), %xmm6
  vpaddq	blake2b_IV(%rip), %xmm2, %xmm3
  vpxor	32(%rcx), %xmm3, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vpaddq	%xmm4, %xmm0, %xmm1
  vmovaps	%xmm4, 304(%rsp)
  vmovdqu	64(%rdx), %xmm7
  vmovaps	%xmm12, 16(%rsp)
  vpaddq	%xmm9, %xmm1, %xmm9
  vpunpckhqdq	%xmm12, %xmm13, %xmm12
  vmovdqu	80(%rcx), %xmm14
  vpxor	blake2b_IV_4(%rip), %xmm14, %xmm10
  vpxor	%xmm11, %xmm10, %xmm10
  vpshufd	$177, %xmm10, %xmm10
  vmovdqu	96(%rdx), %xmm15
  vpxor	%xmm2, %xmm9, %xmm2
  vpaddq	blake2b_IV_2(%rip), %xmm10, %xmm14
  vpxor	48(%rcx), %xmm14, %xmm8
  vmovdqa	BLAKE2B_LC1(%rip), %xmm4
  vpshufb	%xmm5, %xmm8, %xmm8
  vpaddq	%xmm12, %xmm8, %xmm1
  vmovaps	%xmm12, 320(%rsp)
  vpaddq	%xmm11, %xmm1, %xmm11
  vmovaps	%xmm13, 64(%rsp)
  vpshufb	%xmm4, %xmm2, %xmm2
  vmovdqa	%xmm2, %xmm12
  vpaddq	%xmm2, %xmm3, %xmm2
  vpxor	%xmm10, %xmm11, %xmm10
  vpxor	%xmm0, %xmm2, %xmm0
  vpshufb	%xmm4, %xmm10, %xmm10
  vpsrlq	$63, %xmm0, %xmm3
  vmovdqa	%xmm10, %xmm13
  vpaddq	%xmm0, %xmm0, %xmm0
  vpaddq	%xmm10, %xmm14, %xmm10
  vpunpcklqdq	%xmm6, %xmm7, %xmm14
  vmovaps	%xmm14, 112(%rsp)
  vmovdqa	%xmm15, %xmm14
  vmovdqa	80(%rsp), %xmm15
  vpxor	%xmm3, %xmm0, %xmm1
  vpxor	%xmm8, %xmm10, %xmm8
  vpsrlq	$63, %xmm8, %xmm0
  vpaddq	%xmm8, %xmm8, %xmm8
  vmovaps	%xmm14, 32(%rsp)
  vpunpcklqdq	%xmm15, %xmm14, %xmm3
  vpxor	%xmm0, %xmm8, %xmm8
  vpalignr	$8, %xmm1, %xmm8, %xmm0
  vpalignr	$8, %xmm8, %xmm1, %xmm1
  vpalignr	$8, %xmm12, %xmm13, %xmm8
  vpalignr	$8, %xmm13, %xmm12, %xmm12
  vmovdqa	%xmm3, %xmm13
  vpaddq	112(%rsp), %xmm0, %xmm3
  vmovaps	%xmm13, 128(%rsp)
  vpaddq	%xmm9, %xmm3, %xmm9
  vpaddq	%xmm13, %xmm1, %xmm3
  vpunpckhqdq	%xmm15, %xmm14, %xmm13
  vmovaps	%xmm13, 144(%rsp)
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm12, %xmm9, %xmm12
  vpshufd	$177, %xmm12, %xmm12
  vpaddq	%xmm12, %xmm10, %xmm10
  vpxor	%xmm8, %xmm11, %xmm8
  vpshufd	$177, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm2, %xmm3
  vpxor	%xmm0, %xmm10, %xmm0
  vpunpckhqdq	%xmm6, %xmm7, %xmm2
  vpshufb	%xmm5, %xmm0, %xmm0
  vpxor	%xmm1, %xmm3, %xmm1
  vpaddq	%xmm2, %xmm0, %xmm13
  vpshufb	%xmm5, %xmm1, %xmm1
  vpaddq	%xmm9, %xmm13, %xmm9
  vpunpcklqdq	64(%rsp), %xmm15, %xmm15
  vmovaps	%xmm2, 336(%rsp)
  vpaddq	144(%rsp), %xmm1, %xmm2
  vpxor	%xmm12, %xmm9, %xmm12
  vpshufb	%xmm4, %xmm12, %xmm12
  vpaddq	%xmm12, %xmm10, %xmm10
  vpxor	%xmm0, %xmm10, %xmm0
  vmovdqa	%xmm12, %xmm13
  vmovaps	%xmm15, 160(%rsp)
  vpaddq	%xmm11, %xmm2, %xmm11
  vpxor	%xmm8, %xmm11, %xmm2
  vpshufb	%xmm4, %xmm2, %xmm2
  vmovdqa	%xmm2, %xmm8
  vpaddq	%xmm2, %xmm3, %xmm2
  vpsrlq	$63, %xmm0, %xmm3
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm1, %xmm2, %xmm1
  vpxor	%xmm3, %xmm0, %xmm12
  vpsrlq	$63, %xmm1, %xmm3
  vpaddq	%xmm1, %xmm1, %xmm1
  vpxor	%xmm3, %xmm1, %xmm0
  vpalignr	$8, %xmm0, %xmm12, %xmm3
  vpalignr	$8, %xmm8, %xmm13, %xmm1
  vpalignr	$8, %xmm12, %xmm0, %xmm0
  vpalignr	$8, %xmm13, %xmm8, %xmm8
  vpunpckhqdq	%xmm14, %xmm7, %xmm13
  vmovaps	%xmm13, 176(%rsp)
  vpaddq	%xmm15, %xmm3, %xmm13
  vpaddq	176(%rsp), %xmm0, %xmm15
  vpaddq	%xmm9, %xmm13, %xmm9
  vpunpcklqdq	%xmm7, %xmm6, %xmm13
  vmovaps	%xmm13, 192(%rsp)
  vpxor	%xmm8, %xmm9, %xmm8
  vpshufd	$177, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm2, %xmm2
  vpaddq	%xmm11, %xmm15, %xmm11
  vmovdqa	16(%rsp), %xmm15
  vpxor	%xmm3, %xmm2, %xmm3
  vpalignr	$8, 80(%rsp), %xmm15, %xmm12
  vpxor	%xmm1, %xmm11, %xmm1
  vpshufd	$177, %xmm1, %xmm1
  vpaddq	%xmm1, %xmm10, %xmm10
  vpshufb	%xmm5, %xmm3, %xmm3
  vmovdqa	%xmm12, %xmm15
  vpaddq	%xmm13, %xmm3, %xmm13
  vpxor	%xmm0, %xmm10, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vmovaps	%xmm15, 208(%rsp)
  vpaddq	%xmm9, %xmm13, %xmm9
  vpaddq	208(%rsp), %xmm0, %xmm15
  vpxor	%xmm8, %xmm9, %xmm13
  vpshufb	%xmm4, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm2, %xmm2
  vmovdqa	%xmm13, %xmm8
  vpaddq	%xmm11, %xmm15, %xmm11
  vpxor	%xmm3, %xmm2, %xmm3
  vpsrlq	$63, %xmm3, %xmm13
  vpaddq	%xmm3, %xmm3, %xmm3
  vpxor	%xmm1, %xmm11, %xmm1
  vpshufb	%xmm4, %xmm1, %xmm1
  vpaddq	%xmm1, %xmm10, %xmm10
  vmovdqa	%xmm1, %xmm12
  vpxor	%xmm0, %xmm10, %xmm0
  vpxor	%xmm13, %xmm3, %xmm1
  vpsrlq	$63, %xmm0, %xmm3
  vpaddq	%xmm0, %xmm0, %xmm0
  vpunpckhqdq	64(%rsp), %xmm6, %xmm13
  vpxor	%xmm3, %xmm0, %xmm0
  vmovdqa	%xmm13, %xmm15
  vpalignr	$8, %xmm1, %xmm0, %xmm3
  vmovaps	%xmm15, 352(%rsp)
  vpalignr	$8, %xmm0, %xmm1, %xmm1
  vpalignr	$8, %xmm8, %xmm12, %xmm0
  vpaddq	%xmm15, %xmm1, %xmm15
  vpalignr	$8, %xmm12, %xmm8, %xmm8
  vpshufd	$78, 48(%rsp), %xmm12
  vpaddq	%xmm11, %xmm15, %xmm11
  vmovaps	%xmm12, 224(%rsp)
  vpaddq	224(%rsp), %xmm3, %xmm13
  vpxor	%xmm0, %xmm11, %xmm0
  vpshufd	$177, %xmm0, %xmm0
  vpaddq	%xmm0, %xmm2, %xmm2
  vmovdqa	16(%rsp), %xmm12
  vpxor	%xmm1, %xmm2, %xmm1
  vpshufb	%xmm5, %xmm1, %xmm1
  vpaddq	%xmm9, %xmm13, %xmm9
  vmovdqa	(%rsp), %xmm13
  vpxor	%xmm8, %xmm9, %xmm8
  vpshufd	$177, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm10, %xmm10
  vpunpcklqdq	%xmm13, %xmm14, %xmm14
  vmovdqa	%xmm14, %xmm15
  vpxor	%xmm3, %xmm10, %xmm3
  vpunpckhqdq	%xmm13, %xmm12, %xmm14
  vpshufb	%xmm5, %xmm3, %xmm3
  vpaddq	%xmm15, %xmm3, %xmm13
  vmovaps	%xmm14, 96(%rsp)
  vmovaps	%xmm15, 368(%rsp)
  vpaddq	%xmm9, %xmm13, %xmm13
  vpaddq	%xmm14, %xmm1, %xmm15
  vpaddq	%xmm11, %xmm15, %xmm11
  vpxor	%xmm8, %xmm13, %xmm8
  vpshufb	%xmm4, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm10, %xmm10
  vpxor	%xmm3, %xmm10, %xmm3
  vpxor	%xmm0, %xmm11, %xmm0
  vmovdqa	%xmm8, %xmm9
  vpshufb	%xmm4, %xmm0, %xmm0
  vpsrlq	$63, %xmm3, %xmm12
  vpaddq	%xmm0, %xmm2, %xmm2
  vpaddq	%xmm3, %xmm3, %xmm3
  vmovdqa	64(%rsp), %xmm15
  vpxor	%xmm12, %xmm3, %xmm8
  vpxor	%xmm1, %xmm2, %xmm1
  vpsrlq	$63, %xmm1, %xmm3
  vpaddq	%xmm1, %xmm1, %xmm1
  vmovdqa	%xmm0, %xmm14
  vpxor	%xmm3, %xmm1, %xmm0
  vmovdqa	32(%rsp), %xmm1
  vpalignr	$8, %xmm0, %xmm8, %xmm3
  vpalignr	$8, %xmm8, %xmm0, %xmm0
  vpalignr	$8, %xmm6, %xmm1, %xmm12
  vpunpckhqdq	80(%rsp), %xmm15, %xmm1
  vpaddq	%xmm1, %xmm0, %xmm15
  vmovaps	%xmm1, 240(%rsp)
  vpalignr	$8, %xmm14, %xmm9, %xmm8
  vpaddq	%xmm12, %xmm3, %xmm12
  vpaddq	%xmm11, %xmm15, %xmm15
  vpalignr	$8, %xmm9, %xmm14, %xmm14
  vpxor	%xmm8, %xmm15, %xmm8
  vpaddq	%xmm13, %xmm12, %xmm9
  vpshufd	$177, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm10, %xmm1
  vmovdqa	(%rsp), %xmm10
  vpxor	%xmm14, %xmm9, %xmm14
  vpshufd	$177, %xmm14, %xmm14
  vpaddq	%xmm14, %xmm2, %xmm2
  vpunpcklqdq	48(%rsp), %xmm7, %xmm12
  vpxor	%xmm0, %xmm1, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vpblendw	$240, 32(%rsp), %xmm10, %xmm13
  vpxor	%xmm3, %xmm2, %xmm3
  vpshufb	%xmm5, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm12, %xmm12
  vpaddq	%xmm9, %xmm12, %xmm12
  vpaddq	%xmm13, %xmm0, %xmm13
  vpblendw	$240, (%rsp), %xmm6, %xmm9
  vpaddq	%xmm15, %xmm13, %xmm11
  vpxor	%xmm14, %xmm12, %xmm13
  vpshufb	%xmm4, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm2, %xmm2
  vpxor	%xmm3, %xmm2, %xmm3
  vpxor	%xmm8, %xmm11, %xmm8
  vpshufb	%xmm4, %xmm8, %xmm8
  vmovdqa	%xmm8, %xmm10
  vpaddq	%xmm8, %xmm1, %xmm1
  vpsrlq	$63, %xmm3, %xmm8
  vpaddq	%xmm3, %xmm3, %xmm3
  vpxor	%xmm0, %xmm1, %xmm0
  vpxor	%xmm8, %xmm3, %xmm3
  vpsrlq	$63, %xmm0, %xmm8
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm8, %xmm0, %xmm0
  vpalignr	$8, %xmm3, %xmm0, %xmm14
  vpalignr	$8, %xmm0, %xmm3, %xmm3
  vmovdqa	16(%rsp), %xmm0
  vpaddq	%xmm9, %xmm14, %xmm9
  vpaddq	%xmm12, %xmm9, %xmm12
  vpalignr	$8, %xmm13, %xmm10, %xmm8
  vmovdqa	80(%rsp), %xmm9
  vpunpcklqdq	16(%rsp), %xmm9, %xmm9
  vpunpckhqdq	%xmm7, %xmm0, %xmm15
  vpalignr	$8, %xmm10, %xmm13, %xmm10
  vpaddq	%xmm3, %xmm15, %xmm15
  vpaddq	%xmm11, %xmm15, %xmm13
  vpxor	%xmm10, %xmm12, %xmm10
  vmovdqa	64(%rsp), %xmm11
  vpshufd	$177, %xmm10, %xmm10
  vpalignr	$8, 48(%rsp), %xmm11, %xmm11
  vpaddq	%xmm10, %xmm1, %xmm1
  vpxor	%xmm8, %xmm13, %xmm8
  vpshufd	$177, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm2, %xmm0
  vpxor	%xmm14, %xmm1, %xmm2
  vpshufb	%xmm5, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm9, %xmm9
  vpaddq	%xmm12, %xmm9, %xmm9
  vpxor	%xmm3, %xmm0, %xmm3
  vpshufb	%xmm5, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm11, %xmm11
  vpaddq	%xmm13, %xmm11, %xmm15
  vpxor	%xmm10, %xmm9, %xmm10
  vpshufb	%xmm4, %xmm10, %xmm10
  vpaddq	%xmm10, %xmm1, %xmm1
  vmovdqa	%xmm10, %xmm12
  vpxor	%xmm2, %xmm1, %xmm2
  vpxor	%xmm8, %xmm15, %xmm8
  vpsrlq	$63, %xmm2, %xmm10
  vpshufb	%xmm4, %xmm8, %xmm8
  vpaddq	%xmm2, %xmm2, %xmm2
  vpaddq	%xmm8, %xmm0, %xmm0
  vmovdqa	%xmm8, %xmm13
  vpxor	%xmm3, %xmm0, %xmm3
  vpxor	%xmm10, %xmm2, %xmm8
  vpsrlq	$63, %xmm3, %xmm10
  vpaddq	%xmm3, %xmm3, %xmm3
  vpxor	%xmm10, %xmm3, %xmm2
  vpalignr	$8, %xmm2, %xmm8, %xmm3
  vpaddq	96(%rsp), %xmm3, %xmm14
  vpalignr	$8, %xmm8, %xmm2, %xmm2
  vmovdqa	32(%rsp), %xmm8
  vpalignr	$8, %xmm13, %xmm12, %xmm10
  vpaddq	%xmm9, %xmm14, %xmm14
  vpunpckhqdq	%xmm6, %xmm8, %xmm11
  vpalignr	$8, %xmm12, %xmm13, %xmm13
  vpaddq	%xmm2, %xmm11, %xmm11
  vpunpckhqdq	48(%rsp), %xmm7, %xmm8
  vpaddq	%xmm15, %xmm11, %xmm11
  vpxor	%xmm13, %xmm14, %xmm13
  vpshufd	$177, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm0, %xmm0
  vpxor	%xmm10, %xmm11, %xmm10
  vpxor	%xmm3, %xmm0, %xmm3
  vpshufd	$177, %xmm10, %xmm10
  vpshufb	%xmm5, %xmm3, %xmm3
  vpaddq	%xmm10, %xmm1, %xmm1
  vpaddq	%xmm3, %xmm8, %xmm8
  vpaddq	%xmm14, %xmm8, %xmm9
  vpxor	%xmm2, %xmm1, %xmm2
  vpshufb	%xmm5, %xmm2, %xmm2
  vpaddq	128(%rsp), %xmm2, %xmm8
  vpxor	%xmm13, %xmm9, %xmm12
  vpshufb	%xmm4, %xmm12, %xmm12
  vpaddq	%xmm12, %xmm0, %xmm0
  vmovdqa	48(%rsp), %xmm13
  vpaddq	%xmm11, %xmm8, %xmm11
  vpxor	%xmm3, %xmm0, %xmm3
  vpxor	%xmm10, %xmm11, %xmm10
  vpshufb	%xmm4, %xmm10, %xmm10
  vmovdqa	%xmm10, %xmm8
  vpaddq	%xmm10, %xmm1, %xmm1
  vpsrlq	$63, %xmm3, %xmm10
  vpaddq	%xmm3, %xmm3, %xmm3
  vpxor	%xmm2, %xmm1, %xmm2
  vpxor	%xmm10, %xmm3, %xmm3
  vpsrlq	$63, %xmm2, %xmm10
  vpaddq	%xmm2, %xmm2, %xmm2
  vpxor	%xmm10, %xmm2, %xmm10
  vpalignr	$8, %xmm12, %xmm8, %xmm14
  vpalignr	$8, %xmm3, %xmm10, %xmm2
  vpalignr	$8, %xmm8, %xmm12, %xmm15
  vmovdqa	(%rsp), %xmm8
  vpalignr	$8, %xmm10, %xmm3, %xmm3
  vmovdqa	64(%rsp), %xmm10
  vpblendw	$240, %xmm10, %xmm8, %xmm12
  vpblendw	$240, 80(%rsp), %xmm10, %xmm10
  vpaddq	%xmm12, %xmm2, %xmm12
  vpaddq	%xmm9, %xmm12, %xmm8
  vmovdqa	16(%rsp), %xmm12
  vpaddq	%xmm10, %xmm3, %xmm9
  vpaddq	%xmm11, %xmm9, %xmm11
  vpxor	%xmm15, %xmm8, %xmm15
  vpshufd	$177, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm1, %xmm1
  vpunpcklqdq	%xmm6, %xmm12, %xmm9
  vmovdqa	%xmm9, %xmm12
  vpxor	%xmm14, %xmm11, %xmm14
  vpxor	%xmm2, %xmm1, %xmm2
  vpshufd	$177, %xmm14, %xmm14
  vpshufb	%xmm5, %xmm2, %xmm2
  vpaddq	%xmm14, %xmm0, %xmm0
  vpunpcklqdq	%xmm7, %xmm13, %xmm9
  vmovaps	%xmm12, 384(%rsp)
  vpaddq	%xmm12, %xmm2, %xmm12
  vpxor	%xmm3, %xmm0, %xmm3
  vpshufb	%xmm5, %xmm3, %xmm3
  vpaddq	%xmm8, %xmm12, %xmm12
  vpaddq	%xmm3, %xmm9, %xmm8
  vpaddq	%xmm11, %xmm8, %xmm11
  vpxor	%xmm15, %xmm12, %xmm15
  vpshufb	%xmm4, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm1, %xmm1
  vpxor	%xmm2, %xmm1, %xmm2
  vpxor	%xmm14, %xmm11, %xmm14
  vpshufb	%xmm4, %xmm14, %xmm14
  vmovdqa	%xmm14, %xmm8
  vpaddq	%xmm14, %xmm0, %xmm14
  vpsrlq	$63, %xmm2, %xmm0
  vpaddq	%xmm2, %xmm2, %xmm2
  vmovdqa	%xmm15, %xmm13
  vpxor	%xmm3, %xmm14, %xmm3
  vpxor	%xmm0, %xmm2, %xmm15
  vpsrlq	$63, %xmm3, %xmm0
  vpaddq	%xmm3, %xmm3, %xmm3
  vpxor	%xmm0, %xmm3, %xmm2
  vpalignr	$8, %xmm8, %xmm13, %xmm3
  vpalignr	$8, %xmm2, %xmm15, %xmm0
  vpalignr	$8, %xmm13, %xmm8, %xmm8
  vmovdqa	(%rsp), %xmm13
  vpalignr	$8, %xmm15, %xmm2, %xmm2
  vpunpckhqdq	64(%rsp), %xmm7, %xmm15
  vpaddq	%xmm0, %xmm15, %xmm15
  vpaddq	%xmm12, %xmm15, %xmm12
  vpunpcklqdq	%xmm6, %xmm13, %xmm13
  vpaddq	%xmm2, %xmm13, %xmm13
  vpaddq	%xmm11, %xmm13, %xmm11
  vpxor	%xmm8, %xmm12, %xmm8
  vpshufd	$177, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm14, %xmm14
  vpxor	%xmm3, %xmm11, %xmm3
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufd	$177, %xmm3, %xmm13
  vpshufb	%xmm5, %xmm0, %xmm3
  vmovdqa	48(%rsp), %xmm0
  vpaddq	%xmm13, %xmm1, %xmm1
  vpxor	%xmm2, %xmm1, %xmm2
  vpshufb	%xmm5, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm10, %xmm10
  vpblendw	$240, 16(%rsp), %xmm0, %xmm15
  vpaddq	%xmm11, %xmm10, %xmm11
  vpaddq	%xmm15, %xmm3, %xmm15
  vpxor	%xmm13, %xmm11, %xmm13
  vpshufb	%xmm4, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm1, %xmm1
  vpaddq	%xmm12, %xmm15, %xmm12
  vmovdqa	%xmm13, %xmm15
  vpxor	%xmm8, %xmm12, %xmm8
  vpshufb	%xmm4, %xmm8, %xmm8
  vmovdqa	%xmm8, %xmm10
  vpaddq	%xmm8, %xmm14, %xmm14
  vpxor	%xmm2, %xmm1, %xmm8
  vpsrlq	$63, %xmm8, %xmm0
  vpaddq	%xmm8, %xmm8, %xmm2
  vpxor	%xmm3, %xmm14, %xmm3
  vmovdqa	80(%rsp), %xmm8
  vpsrlq	$63, %xmm3, %xmm13
  vpaddq	%xmm3, %xmm3, %xmm3
  vpxor	%xmm0, %xmm2, %xmm2
  vpxor	%xmm13, %xmm3, %xmm3
  vmovdqa	(%rsp), %xmm13
  vpalignr	$8, %xmm3, %xmm2, %xmm0
  vpalignr	$8, %xmm2, %xmm3, %xmm2
  vpalignr	$8, %xmm10, %xmm15, %xmm3
  vpalignr	$8, %xmm15, %xmm10, %xmm10
  vpblendw	$240, %xmm6, %xmm8, %xmm15
  vmovdqa	16(%rsp), %xmm8
  vpaddq	%xmm15, %xmm0, %xmm15
  vpblendw	$240, %xmm13, %xmm8, %xmm8
  vpaddq	%xmm12, %xmm15, %xmm12
  vpaddq	%xmm8, %xmm2, %xmm8
  vpxor	%xmm10, %xmm12, %xmm10
  vpshufd	$177, %xmm10, %xmm10
  vpaddq	%xmm10, %xmm1, %xmm1
  vpaddq	%xmm11, %xmm8, %xmm11
  vmovdqa	32(%rsp), %xmm8
  vpxor	%xmm0, %xmm1, %xmm0
  vpalignr	$8, 48(%rsp), %xmm8, %xmm15
  vpshufb	%xmm5, %xmm0, %xmm0
  vpblendw	$240, 32(%rsp), %xmm7, %xmm8
  vpxor	%xmm3, %xmm11, %xmm3
  vpshufd	$177, %xmm3, %xmm3
  vpaddq	%xmm0, %xmm15, %xmm15
  vpaddq	%xmm3, %xmm14, %xmm14
  vpaddq	%xmm12, %xmm15, %xmm12
  vpxor	%xmm2, %xmm14, %xmm2
  vpshufb	%xmm5, %xmm2, %xmm2
  vpaddq	%xmm8, %xmm2, %xmm8
  vpaddq	%xmm11, %xmm8, %xmm11
  vpxor	%xmm10, %xmm12, %xmm10
  vpshufb	%xmm4, %xmm10, %xmm10
  vpaddq	%xmm10, %xmm1, %xmm1
  vmovdqa	%xmm10, %xmm15
  vpxor	%xmm3, %xmm11, %xmm3
  vpxor	%xmm0, %xmm1, %xmm0
  vpshufb	%xmm4, %xmm3, %xmm8
  vpsrlq	$63, %xmm0, %xmm10
  vpaddq	%xmm8, %xmm14, %xmm14
  vpaddq	%xmm0, %xmm0, %xmm0
  vmovdqa	%xmm8, %xmm3
  vpxor	%xmm2, %xmm14, %xmm2
  vpxor	%xmm10, %xmm0, %xmm8
  vpsrlq	$63, %xmm2, %xmm0
  vpaddq	%xmm2, %xmm2, %xmm2
  vmovdqa	16(%rsp), %xmm10
  vpxor	%xmm0, %xmm2, %xmm2
  vpalignr	$8, %xmm2, %xmm8, %xmm0
  vpalignr	$8, %xmm8, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm9, %xmm9
  vpalignr	$8, %xmm3, %xmm15, %xmm8
  vpaddq	%xmm11, %xmm9, %xmm11
  vpalignr	$8, %xmm15, %xmm3, %xmm15
  vpunpcklqdq	%xmm10, %xmm13, %xmm3
  vpaddq	%xmm0, %xmm3, %xmm3
  vpaddq	%xmm12, %xmm3, %xmm12
  vpxor	%xmm8, %xmm11, %xmm8
  vpshufd	$177, %xmm8, %xmm9
  vpaddq	%xmm9, %xmm1, %xmm1
  vmovdqa	32(%rsp), %xmm8
  vpxor	%xmm15, %xmm12, %xmm15
  vpunpckhqdq	%xmm13, %xmm6, %xmm3
  vpxor	%xmm2, %xmm1, %xmm2
  vpshufd	$177, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm14, %xmm14
  vpshufb	%xmm5, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm3, %xmm3
  vpxor	%xmm0, %xmm14, %xmm0
  vpunpcklqdq	%xmm6, %xmm8, %xmm8
  vpshufb	%xmm5, %xmm0, %xmm0
  vpaddq	%xmm0, %xmm8, %xmm8
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm15, %xmm12, %xmm15
  vpshufb	%xmm4, %xmm15, %xmm13
  vpaddq	%xmm13, %xmm14, %xmm14
  vpxor	%xmm9, %xmm11, %xmm9
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufb	%xmm4, %xmm9, %xmm9
  vpsrlq	$63, %xmm0, %xmm8
  vpaddq	%xmm9, %xmm1, %xmm1
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm2, %xmm1, %xmm2
  vmovdqa	64(%rsp), %xmm15
  vpalignr	$8, %xmm9, %xmm13, %xmm3
  vpxor	%xmm8, %xmm0, %xmm0
  vpsrlq	$63, %xmm2, %xmm8
  vpaddq	%xmm2, %xmm2, %xmm2
  vpxor	%xmm8, %xmm2, %xmm2
  vpalignr	$8, %xmm0, %xmm2, %xmm8
  vpblendw	$240, 16(%rsp), %xmm15, %xmm10
  vpalignr	$8, %xmm2, %xmm0, %xmm0
  vpaddq	%xmm10, %xmm8, %xmm10
  vpalignr	$8, %xmm13, %xmm9, %xmm2
  vmovdqa	80(%rsp), %xmm9
  vpaddq	%xmm12, %xmm10, %xmm12
  vpunpckhqdq	48(%rsp), %xmm9, %xmm9
  vpaddq	%xmm0, %xmm9, %xmm9
  vpxor	%xmm3, %xmm12, %xmm3
  vpshufd	$177, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm1, %xmm1
  vpaddq	%xmm11, %xmm9, %xmm11
  vmovdqa	32(%rsp), %xmm9
  vpxor	%xmm8, %xmm1, %xmm8
  vpshufb	%xmm5, %xmm8, %xmm8
  vpxor	%xmm2, %xmm11, %xmm2
  vpshufd	$177, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm14, %xmm14
  vpunpckhqdq	%xmm15, %xmm9, %xmm10
  vmovdqa	80(%rsp), %xmm9
  vpaddq	%xmm8, %xmm10, %xmm10
  vpxor	%xmm0, %xmm14, %xmm0
  vpaddq	%xmm12, %xmm10, %xmm12
  vpshufb	%xmm5, %xmm0, %xmm0
  vpblendw	$240, %xmm7, %xmm9, %xmm9
  vpxor	%xmm3, %xmm12, %xmm3
  vpshufb	%xmm4, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm1, %xmm1
  vpaddq	%xmm9, %xmm0, %xmm9
  vpxor	%xmm8, %xmm1, %xmm8
  vmovdqa	%xmm3, %xmm10
  vpaddq	%xmm11, %xmm9, %xmm11
  vpsrlq	$63, %xmm8, %xmm9
  vpaddq	%xmm8, %xmm8, %xmm8
  vpxor	%xmm2, %xmm11, %xmm2
  vpshufb	%xmm4, %xmm2, %xmm13
  vpaddq	%xmm13, %xmm14, %xmm14
  vpxor	%xmm9, %xmm8, %xmm3
  vpxor	%xmm0, %xmm14, %xmm0
  vpsrlq	$63, %xmm0, %xmm9
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm9, %xmm0, %xmm8
  vmovdqa	32(%rsp), %xmm9
  vpalignr	$8, %xmm8, %xmm3, %xmm0
  vpalignr	$8, %xmm13, %xmm10, %xmm15
  vpblendw	$240, 48(%rsp), %xmm9, %xmm2
  vpalignr	$8, %xmm3, %xmm8, %xmm8
  vpaddq	%xmm2, %xmm0, %xmm2
  vpalignr	$8, %xmm10, %xmm13, %xmm10
  vpaddq	%xmm12, %xmm2, %xmm12
  vpaddq	160(%rsp), %xmm8, %xmm2
  vpxor	%xmm10, %xmm12, %xmm10
  vpshufd	$177, %xmm10, %xmm10
  vpaddq	%xmm10, %xmm14, %xmm14
  vpaddq	%xmm11, %xmm2, %xmm11
  vpxor	%xmm0, %xmm14, %xmm0
  vpalignr	$8, 32(%rsp), %xmm6, %xmm2
  vpshufb	%xmm5, %xmm0, %xmm0
  vpaddq	240(%rsp), %xmm0, %xmm3
  vpxor	%xmm15, %xmm11, %xmm15
  vpshufd	$177, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm1, %xmm1
  vpxor	%xmm8, %xmm1, %xmm8
  vpshufb	%xmm5, %xmm8, %xmm8
  vpaddq	%xmm8, %xmm2, %xmm2
  vpaddq	%xmm12, %xmm3, %xmm12
  vpaddq	%xmm11, %xmm2, %xmm11
  vpxor	%xmm10, %xmm12, %xmm10
  vpshufb	%xmm4, %xmm10, %xmm13
  vpaddq	%xmm13, %xmm14, %xmm14
  vpxor	%xmm15, %xmm11, %xmm15
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufb	%xmm4, %xmm15, %xmm15
  vpsrlq	$63, %xmm0, %xmm3
  vpaddq	%xmm15, %xmm1, %xmm1
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm8, %xmm1, %xmm8
  vpalignr	$8, %xmm15, %xmm13, %xmm2
  vpxor	%xmm3, %xmm0, %xmm0
  vpsrlq	$63, %xmm8, %xmm3
  vpaddq	%xmm8, %xmm8, %xmm8
  vpxor	%xmm3, %xmm8, %xmm8
  vpalignr	$8, %xmm0, %xmm8, %xmm9
  vpshufd	$78, %xmm7, %xmm3
  vpalignr	$8, %xmm8, %xmm0, %xmm0
  vpalignr	$8, %xmm13, %xmm15, %xmm8
  vmovdqa	48(%rsp), %xmm13
  vpaddq	%xmm3, %xmm0, %xmm3
  vpunpcklqdq	16(%rsp), %xmm13, %xmm10
  vpaddq	%xmm9, %xmm10, %xmm10
  vpaddq	%xmm11, %xmm3, %xmm11
  vpaddq	%xmm12, %xmm10, %xmm12
  vmovdqa	(%rsp), %xmm10
  vpxor	%xmm8, %xmm11, %xmm8
  vpshufd	$177, %xmm8, %xmm8
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufd	$177, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm1, %xmm1
  vpaddq	%xmm8, %xmm14, %xmm14
  vpblendw	$240, %xmm6, %xmm10, %xmm3
  vpxor	%xmm9, %xmm1, %xmm9
  vpshufb	%xmm5, %xmm9, %xmm9
  vpaddq	96(%rsp), %xmm9, %xmm10
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vpaddq	%xmm3, %xmm0, %xmm3
  vpaddq	%xmm12, %xmm10, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufb	%xmm4, %xmm2, %xmm15
  vpaddq	%xmm15, %xmm1, %xmm1
  vpxor	%xmm8, %xmm11, %xmm8
  vpxor	%xmm9, %xmm1, %xmm9
  vpshufb	%xmm4, %xmm8, %xmm8
  vpsrlq	$63, %xmm9, %xmm10
  vpaddq	%xmm8, %xmm14, %xmm14
  vpaddq	%xmm9, %xmm9, %xmm9
  vmovdqa	%xmm8, %xmm2
  vpxor	%xmm0, %xmm14, %xmm0
  vpxor	%xmm10, %xmm9, %xmm8
  vpsrlq	$63, %xmm0, %xmm10
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm10, %xmm0, %xmm9
  vpalignr	$8, %xmm9, %xmm8, %xmm0
  vpalignr	$8, %xmm2, %xmm15, %xmm10
  vpalignr	$8, %xmm8, %xmm9, %xmm9
  vpalignr	$8, %xmm15, %xmm2, %xmm3
  vmovdqa	32(%rsp), %xmm2
  vmovdqa	%xmm13, %xmm15
  vpunpckhqdq	16(%rsp), %xmm2, %xmm8
  vpaddq	%xmm0, %xmm8, %xmm8
  vpaddq	%xmm12, %xmm8, %xmm12
  vmovdqa	80(%rsp), %xmm8
  vpblendw	$240, (%rsp), %xmm2, %xmm2
  vpaddq	%xmm2, %xmm9, %xmm2
  vpxor	%xmm3, %xmm12, %xmm3
  vpshufd	$177, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm14, %xmm14
  vpalignr	$8, %xmm6, %xmm8, %xmm8
  vpaddq	%xmm11, %xmm2, %xmm11
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vmovaps	%xmm8, 256(%rsp)
  vpxor	%xmm10, %xmm11, %xmm10
  vpshufd	$177, %xmm10, %xmm10
  vpaddq	%xmm10, %xmm1, %xmm1
  vpaddq	256(%rsp), %xmm0, %xmm8
  vpunpckhqdq	%xmm7, %xmm13, %xmm2
  vpxor	%xmm9, %xmm1, %xmm9
  vpshufb	%xmm5, %xmm9, %xmm9
  vpaddq	%xmm9, %xmm2, %xmm2
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm2, %xmm11
  vpxor	%xmm3, %xmm12, %xmm3
  vpshufb	%xmm4, %xmm3, %xmm8
  vpaddq	%xmm8, %xmm14, %xmm14
  vmovdqa	%xmm8, %xmm2
  vpxor	%xmm0, %xmm14, %xmm0
  vpxor	%xmm10, %xmm11, %xmm10
  vpsrlq	$63, %xmm0, %xmm8
  vpshufb	%xmm4, %xmm10, %xmm10
  vpaddq	%xmm0, %xmm0, %xmm0
  vpaddq	%xmm10, %xmm1, %xmm1
  vpunpcklqdq	(%rsp), %xmm7, %xmm3
  vpxor	%xmm9, %xmm1, %xmm9
  vpxor	%xmm8, %xmm0, %xmm0
  vpsrlq	$63, %xmm9, %xmm8
  vpaddq	%xmm9, %xmm9, %xmm9
  vpxor	%xmm8, %xmm9, %xmm9
  vpalignr	$8, %xmm0, %xmm9, %xmm13
  vpaddq	240(%rsp), %xmm13, %xmm8
  vpalignr	$8, %xmm9, %xmm0, %xmm0
  vpaddq	%xmm12, %xmm8, %xmm12
  vpalignr	$8, %xmm2, %xmm10, %xmm9
  vpaddq	%xmm0, %xmm3, %xmm3
  vpalignr	$8, %xmm10, %xmm2, %xmm2
  vpaddq	%xmm11, %xmm3, %xmm11
  vpunpcklqdq	64(%rsp), %xmm15, %xmm3
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufd	$177, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm1, %xmm1
  vpxor	%xmm9, %xmm11, %xmm9
  vpxor	%xmm13, %xmm1, %xmm13
  vpshufd	$177, %xmm9, %xmm9
  vpshufb	%xmm5, %xmm13, %xmm13
  vpaddq	%xmm9, %xmm14, %xmm14
  vpaddq	%xmm13, %xmm3, %xmm3
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vpaddq	%xmm12, %xmm3, %xmm12
  vpaddq	384(%rsp), %xmm0, %xmm3
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufb	%xmm4, %xmm2, %xmm10
  vpaddq	%xmm10, %xmm1, %xmm1
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm13, %xmm1, %xmm13
  vpxor	%xmm9, %xmm11, %xmm9
  vpshufb	%xmm4, %xmm9, %xmm9
  vmovdqa	%xmm9, %xmm2
  vpaddq	%xmm9, %xmm14, %xmm14
  vpsrlq	$63, %xmm13, %xmm9
  vpaddq	%xmm13, %xmm13, %xmm13
  vpxor	%xmm0, %xmm14, %xmm0
  vpxor	%xmm9, %xmm13, %xmm8
  vpsrlq	$63, %xmm0, %xmm9
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm9, %xmm0, %xmm13
  vmovdqa	48(%rsp), %xmm9
  vpalignr	$8, %xmm13, %xmm8, %xmm0
  vpalignr	$8, %xmm2, %xmm10, %xmm15
  vpalignr	$8, %xmm8, %xmm13, %xmm13
  vpalignr	$8, %xmm10, %xmm2, %xmm3
  vmovdqa	16(%rsp), %xmm10
  vpunpcklqdq	80(%rsp), %xmm10, %xmm8
  vpaddq	%xmm0, %xmm8, %xmm8
  vpalignr	$8, %xmm6, %xmm9, %xmm2
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm2, %xmm13, %xmm2
  vpxor	%xmm3, %xmm12, %xmm3
  vpshufd	$177, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm14, %xmm14
  vpaddq	%xmm11, %xmm2, %xmm11
  vpxor	%xmm0, %xmm14, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm0
  vpalignr	$8, %xmm9, %xmm6, %xmm6
  vpxor	%xmm15, %xmm11, %xmm15
  vpshufd	$177, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm1, %xmm10
  vmovdqa	80(%rsp), %xmm1
  vpxor	%xmm13, %xmm10, %xmm13
  vpshufb	%xmm5, %xmm13, %xmm13
  vpunpckhqdq	%xmm7, %xmm1, %xmm8
  vpalignr	$8, (%rsp), %xmm7, %xmm7
  vpaddq	%xmm0, %xmm8, %xmm1
  vpaddq	%xmm12, %xmm1, %xmm12
  vpaddq	%xmm13, %xmm7, %xmm7
  vpxor	%xmm3, %xmm12, %xmm3
  vpshufb	%xmm4, %xmm3, %xmm3
  vpaddq	%xmm3, %xmm14, %xmm14
  vpaddq	%xmm11, %xmm7, %xmm11
  vpxor	%xmm0, %xmm14, %xmm0
  vpsrlq	$63, %xmm0, %xmm7
  vpaddq	%xmm0, %xmm0, %xmm0
  vpxor	%xmm15, %xmm11, %xmm15
  vpshufb	%xmm4, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm10, %xmm10
  vpxor	%xmm7, %xmm0, %xmm0
  vmovdqa	%xmm3, %xmm2
  vpxor	%xmm13, %xmm10, %xmm3
  vpsrlq	$63, %xmm3, %xmm7
  vpaddq	%xmm3, %xmm3, %xmm13
  vpxor	%xmm7, %xmm13, %xmm13
  vpalignr	$8, %xmm0, %xmm13, %xmm7
  vpaddq	32(%rsp), %xmm7, %xmm1
  vmovdqa	%xmm9, %xmm3
  vpalignr	$8, %xmm13, %xmm0, %xmm13
  vmovdqa	(%rsp), %xmm9
  vpaddq	%xmm13, %xmm6, %xmm6
  vpalignr	$8, %xmm2, %xmm15, %xmm0
  vpaddq	%xmm12, %xmm1, %xmm12
  vpaddq	%xmm11, %xmm6, %xmm11
  vpalignr	$8, %xmm15, %xmm2, %xmm2
  vpblendw	$240, 16(%rsp), %xmm9, %xmm1
  vpxor	%xmm2, %xmm12, %xmm2
  vpxor	%xmm0, %xmm11, %xmm0
  vpshufd	$177, %xmm2, %xmm2
  vpshufd	$177, %xmm0, %xmm0
  vpaddq	%xmm2, %xmm10, %xmm10
  vpaddq	%xmm0, %xmm14, %xmm14
  vpxor	%xmm7, %xmm10, %xmm7
  vpxor	%xmm13, %xmm14, %xmm13
  vpshufb	%xmm5, %xmm7, %xmm7
  vpshufb	%xmm5, %xmm13, %xmm13
  vpaddq	%xmm1, %xmm7, %xmm1
  vpaddq	64(%rsp), %xmm13, %xmm6
  vpaddq	%xmm12, %xmm1, %xmm12
  vpaddq	%xmm11, %xmm6, %xmm11
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufb	%xmm4, %xmm2, %xmm15
  vpaddq	%xmm15, %xmm10, %xmm10
  vpxor	%xmm7, %xmm10, %xmm7
  vpxor	%xmm0, %xmm11, %xmm0
  vpsrlq	$63, %xmm7, %xmm1
  vpshufb	%xmm4, %xmm0, %xmm0
  vpaddq	%xmm7, %xmm7, %xmm7
  vpaddq	%xmm0, %xmm14, %xmm14
  vmovdqa	%xmm0, %xmm2
  vpxor	%xmm1, %xmm7, %xmm7
  vpxor	%xmm13, %xmm14, %xmm13
  vpsrlq	$63, %xmm13, %xmm1
  vpaddq	%xmm13, %xmm13, %xmm13
  vpxor	%xmm1, %xmm13, %xmm0
  vmovdqa	16(%rsp), %xmm13
  vpalignr	$8, %xmm0, %xmm7, %xmm9
  vpaddq	192(%rsp), %xmm9, %xmm6
  vpalignr	$8, %xmm7, %xmm0, %xmm0
  vpalignr	$8, %xmm2, %xmm15, %xmm1
  vpaddq	%xmm12, %xmm6, %xmm12
  vmovdqa	(%rsp), %xmm6
  vpalignr	$8, %xmm15, %xmm2, %xmm2
  vmovdqa	%xmm3, %xmm15
  vpunpckhqdq	%xmm3, %xmm13, %xmm3
  vpaddq	%xmm0, %xmm3, %xmm3
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufd	$177, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm14, %xmm14
  vpxor	%xmm1, %xmm11, %xmm1
  vpshufd	$177, %xmm1, %xmm1
  vpaddq	%xmm1, %xmm10, %xmm10
  vpxor	%xmm9, %xmm14, %xmm9
  vpxor	%xmm0, %xmm10, %xmm0
  vpshufb	%xmm5, %xmm0, %xmm7
  vmovdqa	64(%rsp), %xmm0
  vpshufb	%xmm5, %xmm9, %xmm9
  vpblendw	$240, %xmm0, %xmm13, %xmm3
  vpunpcklqdq	%xmm0, %xmm6, %xmm6
  vpaddq	%xmm9, %xmm6, %xmm6
  vpaddq	%xmm3, %xmm7, %xmm3
  vpaddq	%xmm12, %xmm6, %xmm12
  vmovdqa	(%rsp), %xmm6
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm2, %xmm12, %xmm2
  vpshufb	%xmm4, %xmm2, %xmm2
  vpaddq	%xmm2, %xmm14, %xmm14
  vpxor	%xmm1, %xmm11, %xmm1
  vpshufb	%xmm4, %xmm1, %xmm1
  vpaddq	%xmm1, %xmm10, %xmm10
  vmovdqa	%xmm2, %xmm3
  vpxor	%xmm9, %xmm14, %xmm9
  vpxor	%xmm7, %xmm10, %xmm7
  vpsrlq	$63, %xmm9, %xmm0
  vpaddq	%xmm9, %xmm9, %xmm9
  vpsrlq	$63, %xmm7, %xmm2
  vpaddq	%xmm7, %xmm7, %xmm7
  vmovdqa	%xmm1, %xmm13
  vpxor	%xmm0, %xmm9, %xmm1
  vpxor	%xmm2, %xmm7, %xmm7
  vpalignr	$8, %xmm1, %xmm7, %xmm9
  vpaddq	%xmm9, %xmm8, %xmm8
  vpalignr	$8, %xmm7, %xmm1, %xmm7
  vmovdqa	32(%rsp), %xmm1
  vpalignr	$8, %xmm3, %xmm13, %xmm0
  vpaddq	%xmm12, %xmm8, %xmm12
  vpalignr	$8, %xmm13, %xmm3, %xmm13
  vpunpckhqdq	%xmm1, %xmm6, %xmm3
  vpaddq	%xmm7, %xmm3, %xmm3
  vpxor	%xmm13, %xmm12, %xmm13
  vpshufd	$177, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm10, %xmm10
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm9, %xmm10, %xmm9
  vpshufb	%xmm5, %xmm9, %xmm9
  vpxor	%xmm0, %xmm11, %xmm0
  vpunpcklqdq	%xmm15, %xmm1, %xmm3
  vpaddq	256(%rsp), %xmm9, %xmm8
  vpshufd	$177, %xmm0, %xmm0
  vpaddq	%xmm0, %xmm14, %xmm14
  vpxor	%xmm7, %xmm14, %xmm7
  vpshufb	%xmm5, %xmm7, %xmm7
  vpaddq	%xmm7, %xmm3, %xmm3
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm13, %xmm12, %xmm13
  vpshufb	%xmm4, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm10, %xmm10
  vpxor	%xmm0, %xmm11, %xmm0
  vpxor	%xmm9, %xmm10, %xmm9
  vpshufb	%xmm4, %xmm0, %xmm0
  vmovdqa	%xmm0, %xmm15
  vpaddq	%xmm0, %xmm14, %xmm14
  vpsrlq	$63, %xmm9, %xmm0
  vpaddq	%xmm9, %xmm9, %xmm9
  vpxor	%xmm7, %xmm14, %xmm7
  vpxor	%xmm0, %xmm9, %xmm9
  vpsrlq	$63, %xmm7, %xmm0
  vpaddq	%xmm7, %xmm7, %xmm7
  vpxor	%xmm0, %xmm7, %xmm7
  vpalignr	$8, %xmm7, %xmm9, %xmm6
  vpaddq	272(%rsp), %xmm6, %xmm8
  vpalignr	$8, %xmm9, %xmm7, %xmm7
  vpaddq	288(%rsp), %xmm7, %xmm3
  vpalignr	$8, %xmm15, %xmm13, %xmm0
  vpaddq	%xmm12, %xmm8, %xmm12
  vpalignr	$8, %xmm13, %xmm15, %xmm15
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm15, %xmm12, %xmm15
  vpshufd	$177, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm14, %xmm14
  vpxor	%xmm0, %xmm11, %xmm0
  vpxor	%xmm6, %xmm14, %xmm6
  vpshufd	$177, %xmm0, %xmm9
  vpshufb	%xmm5, %xmm6, %xmm6
  vpaddq	%xmm9, %xmm10, %xmm10
  vpaddq	304(%rsp), %xmm6, %xmm8
  vpxor	%xmm7, %xmm10, %xmm7
  vpshufb	%xmm5, %xmm7, %xmm7
  vpaddq	320(%rsp), %xmm7, %xmm3
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm15, %xmm12, %xmm15
  vpshufb	%xmm4, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm14, %xmm14
  vpxor	%xmm9, %xmm11, %xmm9
  vpxor	%xmm6, %xmm14, %xmm6
  vpshufb	%xmm4, %xmm9, %xmm9
  vpsrlq	$63, %xmm6, %xmm3
  vpaddq	%xmm9, %xmm10, %xmm10
  vpaddq	%xmm6, %xmm6, %xmm6
  vpxor	%xmm7, %xmm10, %xmm7
  vpalignr	$8, %xmm9, %xmm15, %xmm1
  vpxor	%xmm3, %xmm6, %xmm0
  vpsrlq	$63, %xmm7, %xmm3
  vpaddq	%xmm7, %xmm7, %xmm7
  vpxor	%xmm3, %xmm7, %xmm7
  vpalignr	$8, %xmm0, %xmm7, %xmm6
  vpaddq	112(%rsp), %xmm6, %xmm8
  vpalignr	$8, %xmm7, %xmm0, %xmm7
  vpaddq	128(%rsp), %xmm7, %xmm3
  vpaddq	%xmm12, %xmm8, %xmm12
  vpalignr	$8, %xmm15, %xmm9, %xmm0
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm1, %xmm12, %xmm1
  vpshufd	$177, %xmm1, %xmm1
  vpaddq	%xmm1, %xmm10, %xmm10
  vpxor	%xmm0, %xmm11, %xmm0
  vpxor	%xmm6, %xmm10, %xmm6
  vpshufd	$177, %xmm0, %xmm0
  vpshufb	%xmm5, %xmm6, %xmm6
  vpaddq	336(%rsp), %xmm6, %xmm8
  vpaddq	%xmm0, %xmm14, %xmm14
  vpxor	%xmm7, %xmm14, %xmm7
  vpshufb	%xmm5, %xmm7, %xmm7
  vpaddq	144(%rsp), %xmm7, %xmm3
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm1, %xmm12, %xmm1
  vpshufb	%xmm4, %xmm1, %xmm1
  vpaddq	%xmm1, %xmm10, %xmm10
  vmovdqa	%xmm1, %xmm2
  vpxor	%xmm6, %xmm10, %xmm6
  vpxor	%xmm0, %xmm11, %xmm0
  vpsrlq	$63, %xmm6, %xmm1
  vpshufb	%xmm4, %xmm0, %xmm0
  vpaddq	%xmm6, %xmm6, %xmm6
  vpaddq	%xmm0, %xmm14, %xmm14
  vmovdqa	%xmm0, %xmm13
  vpxor	%xmm7, %xmm14, %xmm7
  vpxor	%xmm1, %xmm6, %xmm0
  vpsrlq	$63, %xmm7, %xmm1
  vpaddq	%xmm7, %xmm7, %xmm7
  vpxor	%xmm1, %xmm7, %xmm7
  vpalignr	$8, %xmm7, %xmm0, %xmm6
  vpaddq	160(%rsp), %xmm6, %xmm8
  vpalignr	$8, %xmm0, %xmm7, %xmm7
  vpaddq	176(%rsp), %xmm7, %xmm3
  vpalignr	$8, %xmm13, %xmm2, %xmm0
  vpaddq	%xmm12, %xmm8, %xmm12
  vpalignr	$8, %xmm2, %xmm13, %xmm13
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm13, %xmm12, %xmm13
  vpshufd	$177, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm14, %xmm14
  vpxor	%xmm0, %xmm11, %xmm0
  vpxor	%xmm6, %xmm14, %xmm6
  vpshufd	$177, %xmm0, %xmm9
  vpshufb	%xmm5, %xmm6, %xmm6
  vpaddq	%xmm9, %xmm10, %xmm10
  vpaddq	192(%rsp), %xmm6, %xmm8
  vpxor	%xmm7, %xmm10, %xmm7
  vpshufb	%xmm5, %xmm7, %xmm7
  vpaddq	208(%rsp), %xmm7, %xmm3
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm13, %xmm12, %xmm13
  vpshufb	%xmm4, %xmm13, %xmm13
  vpaddq	%xmm13, %xmm14, %xmm14
  vpxor	%xmm9, %xmm11, %xmm9
  vpxor	%xmm6, %xmm14, %xmm6
  vpshufb	%xmm4, %xmm9, %xmm9
  vpsrlq	$63, %xmm6, %xmm1
  vpaddq	%xmm9, %xmm10, %xmm10
  vpaddq	%xmm6, %xmm6, %xmm6
  vpxor	%xmm7, %xmm10, %xmm7
  vpalignr	$8, %xmm9, %xmm13, %xmm15
  vpxor	%xmm1, %xmm6, %xmm0
  vpsrlq	$63, %xmm7, %xmm1
  vpaddq	%xmm7, %xmm7, %xmm7
  vpxor	%xmm1, %xmm7, %xmm7
  vpalignr	$8, %xmm0, %xmm7, %xmm6
  vpaddq	224(%rsp), %xmm6, %xmm8
  vpalignr	$8, %xmm7, %xmm0, %xmm7
  vpaddq	352(%rsp), %xmm7, %xmm3
  vpaddq	%xmm12, %xmm8, %xmm12
  vpalignr	$8, %xmm13, %xmm9, %xmm0
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm15, %xmm12, %xmm15
  vpshufd	$177, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm10, %xmm10
  vpxor	%xmm0, %xmm11, %xmm0
  vpxor	%xmm6, %xmm10, %xmm6
  vpshufd	$177, %xmm0, %xmm0
  vpshufb	%xmm5, %xmm6, %xmm6
  vpaddq	368(%rsp), %xmm6, %xmm8
  vpaddq	%xmm0, %xmm14, %xmm14
  vpxor	%xmm7, %xmm14, %xmm7
  vpshufb	%xmm5, %xmm7, %xmm5
  vpaddq	96(%rsp), %xmm5, %xmm3
  vmovaps	416(%rsp), %xmm7
  vpaddq	%xmm12, %xmm8, %xmm12
  vpaddq	%xmm11, %xmm3, %xmm11
  vpxor	%xmm15, %xmm12, %xmm15
  vpshufb	%xmm4, %xmm15, %xmm15
  vpaddq	%xmm15, %xmm10, %xmm10
  vpxor	%xmm0, %xmm11, %xmm0
  vpxor	%xmm6, %xmm10, %xmm6
  vpshufb	%xmm4, %xmm0, %xmm4
  vpsrlq	$63, %xmm6, %xmm0
  vpaddq	%xmm4, %xmm14, %xmm14
  vpaddq	%xmm6, %xmm6, %xmm6
  vpalignr	$8, %xmm4, %xmm15, %xmm2
  vpxor	%xmm10, %xmm11, %xmm11
  vpxor	%xmm0, %xmm6, %xmm6
  vpxor	%xmm5, %xmm14, %xmm5
  vpsrlq	$63, %xmm5, %xmm0
  vpaddq	%xmm5, %xmm5, %xmm5
  vpalignr	$8, %xmm15, %xmm4, %xmm1
  vpxor	%xmm0, %xmm5, %xmm5
  vpxor	16(%rcx), %xmm11, %xmm11
  vpxor	%xmm14, %xmm12, %xmm12
  vpalignr	$8, %xmm5, %xmm6, %xmm0
  vpxor	(%rcx), %xmm12, %xmm12
  vmovups	%xmm11, 16(%rcx)
  vmovaps	432(%rsp), %xmm8
  vpalignr	$8, %xmm6, %xmm5, %xmm5
  vmovups	%xmm12, (%rcx)
  vpxor	%xmm0, %xmm1, %xmm1
  vmovaps	400(%rsp), %xmm6
  vpxor	32(%rcx), %xmm1, %xmm1
  vpxor	%xmm5, %xmm2, %xmm5
  vmovups	%xmm1, 32(%rcx)
  vpxor	48(%rcx), %xmm5, %xmm5
  vmovups	%xmm5, 48(%rcx)
  vmovaps	448(%rsp), %xmm9
  vmovaps	464(%rsp), %xmm10
  vmovaps	480(%rsp), %xmm11
  vmovaps	496(%rsp), %xmm12
  vmovaps	512(%rsp), %xmm13
  vmovaps	528(%rsp), %xmm14
  vmovaps	544(%rsp), %xmm15
  addq	$568, %rsp
{$IF DEFINED(UNIX)}
  popq	%rdx
  popq	%rcx
{$ENDIF}
end;

