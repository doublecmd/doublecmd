{
  Implementations of BLAKE2b, BLAKE2s,
  optimized for speed on CPUs supporting SSE2
}

const blake2s_IV_2: array[0..3] of cuint32 =
(
  $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
);

function blake2s_compress( S: Pblake2s_state; const block: pcuint8 ): cint; assembler; nostackframe;
asm
{$IF DEFINED(WINDOWS)}
  pushq	%rdi
  pushq	%rsi
{$ELSE IF DEFINED(UNIX)}
  pushq	%rcx
  movq	%rdi,	%rcx
  pushq	%rdx
  movq	%rsi,	%rdx
{$ENDIF}
  pushq	%r15
  pushq	%r14
  pushq	%r12
  pushq	%rbp
  pushq	%rbx
  subq	$208, %rsp
  movaps	%xmm6, 48(%rsp)
  movaps	%xmm7, 64(%rsp)
  movaps	%xmm8, 80(%rsp)
  movaps	%xmm9, 96(%rsp)
  movaps	%xmm10, 112(%rsp)
  movaps	%xmm11, 128(%rsp)
  movaps	%xmm12, 144(%rsp)
  movaps	%xmm13, 160(%rsp)
  movaps	%xmm14, 176(%rsp)
  movaps	%xmm15, 192(%rsp)
  movdqu	blake2s_IV(%rip), %xmm5
  movdqu	(%rcx), %xmm0
  movl	16(%rdx), %ebx
  movdqa	%xmm0, %xmm14
  movdqu	16(%rcx), %xmm0
  movd	(%rdx), %xmm4
  movdqa	%xmm0, %xmm13
  movd	8(%rdx), %xmm1
  movaps	%xmm14, (%rsp)
  movd	24(%rdx), %xmm7
  movdqa	%xmm13, %xmm15
  paddd	(%rsp), %xmm15
  movd	%ebx, %xmm12
  movdqu	32(%rcx), %xmm2
  movdqa	%xmm4, %xmm14
  movaps	%xmm13, 16(%rsp)
  punpckldq	%xmm7, %xmm12
  punpckldq	%xmm1, %xmm14
  punpcklqdq	%xmm12, %xmm14
  movdqu	blake2s_IV_2(%rip), %xmm0
  paddd	%xmm15, %xmm14
  movl	12(%rdx), %eax
  pxor	%xmm2, %xmm0
  pxor	%xmm14, %xmm0
  movdqa	%xmm0, %xmm12
  movl	20(%rdx), %r9d
  pslld	$16, %xmm12
  movd	4(%rdx), %xmm11
  movdqa	%xmm13, %xmm2
  movdqa	%xmm14, %xmm15
  psrld	$16, %xmm0
  movd	28(%rdx), %xmm10
  pxor	%xmm12, %xmm0
  paddd	%xmm0, %xmm5
  movd	%eax, %xmm13
  movdqa	%xmm11, %xmm14
  pxor	%xmm5, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  movl	56(%rdx), %edi
  pslld	$20, %xmm12
  punpckldq	%xmm13, %xmm14
  movdqa	%xmm14, %xmm13
  movl	32(%rdx), %r11d
  pxor	%xmm12, %xmm2
  movd	%r9d, %xmm12
  paddd	%xmm2, %xmm15
  movl	40(%rdx), %r10d
  punpckldq	%xmm10, %xmm12
  punpcklqdq	%xmm12, %xmm13
  paddd	%xmm15, %xmm13
  movd	48(%rdx), %xmm6
  pxor	%xmm13, %xmm0
  movdqa	%xmm0, %xmm12
  movl	60(%rdx), %ebp
  psrld	$8, %xmm0
  pslld	$24, %xmm12
  movdqa	%xmm6, %xmm15
  movd	36(%rdx), %xmm3
  pxor	%xmm12, %xmm0
  paddd	%xmm0, %xmm5
  movd	52(%rdx), %xmm9
  pshufd	$147, %xmm0, %xmm0
  pxor	%xmm5, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  movd	44(%rdx), %xmm8
  pslld	$25, %xmm12
  pshufd	$78, %xmm5, %xmm5
  pxor	%xmm12, %xmm2
  movd	%edi, %xmm12
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm12, %xmm15
  movd	%r11d, %xmm12
  movq	%xmm15, %rdx
  movd	%r10d, %xmm15
  punpckldq	%xmm15, %xmm12
  movq	%rdx, %xmm15
  punpcklqdq	%xmm15, %xmm12
  paddd	%xmm2, %xmm12
  movdqa	%xmm9, %xmm15
  paddd	%xmm13, %xmm12
  movd	%ebx, %xmm13
  movdqa	%xmm12, %xmm14
  pxor	%xmm12, %xmm0
  movdqa	%xmm0, %xmm12
  pslld	$16, %xmm12
  psrld	$16, %xmm0
  pxor	%xmm12, %xmm0
  paddd	%xmm0, %xmm5
  pxor	%xmm5, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movd	%ebp, %xmm12
  punpckldq	%xmm12, %xmm15
  movdqa	%xmm3, %xmm12
  punpckldq	%xmm8, %xmm12
  punpcklqdq	%xmm15, %xmm12
  paddd	%xmm2, %xmm12
  paddd	%xmm14, %xmm12
  pxor	%xmm12, %xmm0
  movdqa	%xmm0, %xmm15
  psrld	$8, %xmm0
  pslld	$24, %xmm15
  pxor	%xmm15, %xmm0
  paddd	%xmm0, %xmm5
  pshufd	$57, %xmm0, %xmm14
  movdqa	%xmm3, %xmm0
  pxor	%xmm5, %xmm2
  movdqa	%xmm2, %xmm15
  psrld	$7, %xmm2
  punpckldq	%xmm9, %xmm0
  pslld	$25, %xmm15
  pshufd	$78, %xmm5, %xmm5
  pxor	%xmm15, %xmm2
  movd	%edi, %xmm15
  pshufd	$147, %xmm2, %xmm2
  punpckldq	%xmm13, %xmm15
  movdqa	%xmm15, %xmm13
  movq	%xmm15, %r14
  punpcklqdq	%xmm0, %xmm13
  movdqa	%xmm13, %xmm0
  paddd	%xmm2, %xmm0
  paddd	%xmm0, %xmm12
  movdqa	%xmm14, %xmm0
  pxor	%xmm12, %xmm0
  movdqa	%xmm0, %xmm15
  pslld	$16, %xmm15
  movdqa	%xmm15, %xmm13
  movdqa	%xmm0, %xmm15
  psrld	$16, %xmm15
  movdqa	%xmm15, %xmm14
  movd	%r10d, %xmm15
  pxor	%xmm13, %xmm14
  paddd	%xmm14, %xmm5
  movd	%r11d, %xmm13
  pxor	%xmm5, %xmm2
  movdqa	%xmm2, %xmm0
  psrld	$12, %xmm2
  punpckldq	%xmm13, %xmm15
  pslld	$20, %xmm0
  movq	%xmm15, %rsi
  pxor	%xmm0, %xmm2
  movd	%ebp, %xmm0
  punpckldq	%xmm7, %xmm0
  punpcklqdq	%xmm0, %xmm15
  movdqa	%xmm15, %xmm0
  paddd	%xmm2, %xmm0
  paddd	%xmm0, %xmm12
  pxor	%xmm12, %xmm14
  movdqa	%xmm14, %xmm15
  psrld	$8, %xmm14
  pslld	$24, %xmm15
  pxor	%xmm15, %xmm14
  paddd	%xmm14, %xmm5
  pshufd	$147, %xmm14, %xmm14
  pxor	%xmm5, %xmm2
  movdqa	%xmm2, %xmm0
  psrld	$7, %xmm2
  pshufd	$78, %xmm5, %xmm15
  pslld	$25, %xmm0
  movdqa	%xmm8, %xmm5
  pxor	%xmm0, %xmm2
  movd	%r9d, %xmm0
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm0, %xmm5
  movdqa	%xmm11, %xmm0
  punpckldq	%xmm4, %xmm0
  punpcklqdq	%xmm5, %xmm0
  paddd	%xmm2, %xmm0
  paddd	%xmm12, %xmm0
  pxor	%xmm0, %xmm14
  movdqa	%xmm14, %xmm12
  psrld	$16, %xmm14
  pslld	$16, %xmm12
  movdqa	%xmm14, %xmm5
  movdqa	%xmm10, %xmm14
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movd	%eax, %xmm12
  punpckldq	%xmm12, %xmm14
  movq	%xmm14, %r8
  movdqa	%xmm14, %xmm13
  movdqa	%xmm6, %xmm14
  punpckldq	%xmm1, %xmm14
  punpcklqdq	%xmm13, %xmm14
  paddd	%xmm2, %xmm14
  movdqa	%xmm8, %xmm13
  paddd	%xmm0, %xmm14
  punpckldq	%xmm6, %xmm13
  pxor	%xmm14, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$57, %xmm5, %xmm5
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  pshufd	$78, %xmm15, %xmm0
  pslld	$25, %xmm12
  movd	%r9d, %xmm15
  pxor	%xmm12, %xmm2
  movd	%ebp, %xmm12
  pshufd	$147, %xmm2, %xmm2
  punpckldq	%xmm12, %xmm15
  punpcklqdq	%xmm15, %xmm13
  paddd	%xmm2, %xmm13
  movq	%xmm15, %r15
  paddd	%xmm14, %xmm13
  movdqa	%xmm1, %xmm14
  movd	%eax, %xmm15
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  punpckldq	%xmm9, %xmm14
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movd	%r11d, %xmm12
  punpckldq	%xmm4, %xmm12
  punpcklqdq	%xmm14, %xmm12
  paddd	%xmm2, %xmm12
  movdqa	%xmm10, %xmm14
  paddd	%xmm12, %xmm13
  punpckldq	%xmm3, %xmm14
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$8, %xmm5
  pslld	$24, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$147, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm0
  pslld	$25, %xmm12
  pxor	%xmm12, %xmm2
  movd	%r10d, %xmm12
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm15, %xmm12
  punpcklqdq	%xmm14, %xmm12
  paddd	%xmm2, %xmm12
  movdqa	%xmm11, %xmm14
  paddd	%xmm13, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm15
  psrld	$16, %xmm5
  pslld	$16, %xmm15
  pxor	%xmm15, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm15
  psrld	$12, %xmm2
  pslld	$20, %xmm15
  pxor	%xmm15, %xmm2
  movd	%ebx, %xmm15
  punpckldq	%xmm15, %xmm14
  movd	%edi, %xmm15
  punpckldq	%xmm7, %xmm15
  movdqa	%xmm15, %xmm13
  punpcklqdq	%xmm14, %xmm13
  paddd	%xmm2, %xmm13
  movq	%r8, %xmm14
  paddd	%xmm13, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm15
  psrld	$8, %xmm5
  pslld	$24, %xmm15
  pxor	%xmm15, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$57, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm15
  psrld	$7, %xmm2
  pslld	$25, %xmm15
  movdqa	%xmm15, %xmm13
  pshufd	$78, %xmm0, %xmm15
  movdqa	%xmm9, %xmm0
  punpckldq	%xmm8, %xmm0
  punpcklqdq	%xmm0, %xmm14
  movdqa	%xmm14, %xmm0
  pxor	%xmm13, %xmm2
  pshufd	$147, %xmm2, %xmm2
  paddd	%xmm2, %xmm0
  movq	%rdx, %xmm14
  paddd	%xmm0, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$16, %xmm5
  pslld	$16, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm0
  psrld	$12, %xmm2
  pslld	$20, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm3, %xmm0
  punpckldq	%xmm11, %xmm0
  punpcklqdq	%xmm14, %xmm0
  paddd	%xmm2, %xmm0
  movd	%ebx, %xmm14
  paddd	%xmm0, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$147, %xmm5, %xmm5
  movdqa	%xmm15, %xmm0
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm15
  pslld	$25, %xmm15
  movdqa	%xmm15, %xmm13
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm15
  movd	%ebp, %xmm0
  punpckldq	%xmm0, %xmm14
  pxor	%xmm13, %xmm2
  movdqa	%xmm1, %xmm0
  movd	%r9d, %xmm13
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm13, %xmm0
  punpcklqdq	%xmm14, %xmm0
  paddd	%xmm2, %xmm0
  paddd	%xmm12, %xmm0
  movd	%r11d, %xmm13
  pxor	%xmm0, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm4, %xmm12
  punpckldq	%xmm13, %xmm12
  movdqa	%xmm7, %xmm13
  movq	%xmm12, %rdx
  movd	%r10d, %xmm12
  punpckldq	%xmm12, %xmm13
  movq	%xmm13, %r12
  movdqa	%xmm13, %xmm12
  movq	%rdx, %xmm13
  punpcklqdq	%xmm13, %xmm12
  movdqa	%xmm12, %xmm13
  paddd	%xmm2, %xmm13
  movdqa	%xmm13, %xmm12
  movd	%r10d, %xmm13
  paddd	%xmm0, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$57, %xmm5, %xmm5
  movaps	%xmm5, 32(%rsp)
  movdqa	%xmm15, %xmm0
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm15
  movd	%r9d, %xmm5
  pslld	$25, %xmm15
  pshufd	$78, %xmm0, %xmm0
  psrld	$7, %xmm2
  pxor	%xmm15, %xmm2
  movdqa	%xmm1, %xmm15
  pshufd	$147, %xmm2, %xmm2
  punpckldq	%xmm13, %xmm15
  movdqa	%xmm3, %xmm13
  punpckldq	%xmm5, %xmm13
  movdqa	32(%rsp), %xmm5
  punpcklqdq	%xmm15, %xmm13
  paddd	%xmm2, %xmm13
  paddd	%xmm12, %xmm13
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm4, %xmm12
  punpckldq	%xmm10, %xmm12
  punpcklqdq	%xmm14, %xmm12
  paddd	%xmm2, %xmm12
  movdqa	%xmm7, %xmm14
  paddd	%xmm12, %xmm13
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$8, %xmm5
  pslld	$24, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$147, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm15
  pslld	$25, %xmm12
  pxor	%xmm12, %xmm2
  movd	%eax, %xmm12
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm12, %xmm14
  movd	%edi, %xmm12
  punpckldq	%xmm8, %xmm12
  punpcklqdq	%xmm14, %xmm12
  paddd	%xmm2, %xmm12
  movd	%r11d, %xmm14
  paddd	%xmm13, %xmm12
  punpckldq	%xmm9, %xmm14
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$16, %xmm5
  pslld	$16, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm0
  psrld	$12, %xmm2
  pslld	$20, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm11, %xmm0
  punpckldq	%xmm6, %xmm0
  punpcklqdq	%xmm14, %xmm0
  paddd	%xmm2, %xmm0
  movq	%rdx, %xmm14
  paddd	%xmm0, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$57, %xmm5, %xmm5
  movdqa	%xmm15, %xmm0
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm15
  pslld	$25, %xmm15
  movdqa	%xmm15, %xmm13
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm15
  movdqa	%xmm1, %xmm0
  pxor	%xmm13, %xmm2
  punpckldq	%xmm7, %xmm0
  pshufd	$147, %xmm2, %xmm2
  punpcklqdq	%xmm14, %xmm0
  paddd	%xmm2, %xmm0
  movd	%eax, %xmm14
  movd	%r10d, %xmm13
  paddd	%xmm12, %xmm0
  pxor	%xmm0, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm8, %xmm12
  punpckldq	%xmm14, %xmm12
  movdqa	%xmm6, %xmm14
  punpckldq	%xmm13, %xmm14
  punpcklqdq	%xmm12, %xmm14
  paddd	%xmm2, %xmm14
  movd	%ebx, %xmm13
  paddd	%xmm0, %xmm14
  punpckldq	%xmm10, %xmm13
  pxor	%xmm14, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$147, %xmm5, %xmm5
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  pshufd	$78, %xmm15, %xmm0
  psrld	$7, %xmm2
  pslld	$25, %xmm12
  movd	%ebp, %xmm15
  punpckldq	%xmm11, %xmm15
  pxor	%xmm12, %xmm2
  punpcklqdq	%xmm15, %xmm13
  pshufd	$57, %xmm2, %xmm2
  paddd	%xmm2, %xmm13
  movd	%r9d, %xmm15
  paddd	%xmm14, %xmm13
  movd	%edi, %xmm14
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  punpckldq	%xmm3, %xmm14
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm9, %xmm12
  punpckldq	%xmm15, %xmm12
  punpcklqdq	%xmm14, %xmm12
  paddd	%xmm2, %xmm12
  movd	%r10d, %xmm14
  paddd	%xmm12, %xmm13
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$8, %xmm5
  pslld	$24, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$57, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm15
  pslld	$25, %xmm12
  movq	%r14, %xmm0
  pxor	%xmm12, %xmm2
  movdqa	%xmm6, %xmm12
  pshufd	$147, %xmm2, %xmm2
  punpckldq	%xmm11, %xmm12
  punpcklqdq	%xmm0, %xmm12
  paddd	%xmm2, %xmm12
  paddd	%xmm13, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$16, %xmm5
  pslld	$16, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm0
  psrld	$12, %xmm2
  pslld	$20, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm9, %xmm0
  punpckldq	%xmm14, %xmm0
  movq	%r15, %xmm14
  punpcklqdq	%xmm0, %xmm14
  movdqa	%xmm14, %xmm0
  movdqa	%xmm3, %xmm14
  paddd	%xmm2, %xmm0
  paddd	%xmm0, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$147, %xmm5, %xmm5
  movdqa	%xmm15, %xmm0
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm15
  pslld	$25, %xmm15
  movdqa	%xmm15, %xmm13
  pshufd	$78, %xmm0, %xmm15
  movd	%r11d, %xmm0
  punpckldq	%xmm0, %xmm14
  psrld	$7, %xmm2
  movdqa	%xmm4, %xmm0
  pxor	%xmm13, %xmm2
  punpckldq	%xmm7, %xmm0
  pshufd	$57, %xmm2, %xmm2
  punpcklqdq	%xmm14, %xmm0
  paddd	%xmm2, %xmm0
  movq	%r8, %xmm13
  paddd	%xmm12, %xmm0
  pxor	%xmm0, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm15
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm1, %xmm12
  punpckldq	%xmm8, %xmm12
  punpcklqdq	%xmm12, %xmm13
  paddd	%xmm2, %xmm13
  movdqa	%xmm13, %xmm14
  movdqa	%xmm9, %xmm13
  paddd	%xmm0, %xmm14
  punpckldq	%xmm10, %xmm13
  pxor	%xmm14, %xmm5
  movdqa	%xmm5, %xmm0
  psrld	$8, %xmm5
  pslld	$24, %xmm0
  pxor	%xmm0, %xmm5
  paddd	%xmm5, %xmm15
  pshufd	$57, %xmm5, %xmm5
  pxor	%xmm15, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  pshufd	$78, %xmm15, %xmm0
  pslld	$25, %xmm12
  movdqa	%xmm6, %xmm15
  pxor	%xmm12, %xmm2
  movd	%eax, %xmm12
  pshufd	$147, %xmm2, %xmm2
  punpckldq	%xmm12, %xmm15
  punpcklqdq	%xmm15, %xmm13
  paddd	%xmm2, %xmm13
  movd	%edi, %xmm15
  paddd	%xmm14, %xmm13
  movdqa	%xmm8, %xmm14
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  punpckldq	%xmm15, %xmm14
  pslld	$16, %xmm12
  movdqa	%xmm14, %xmm15
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm11, %xmm12
  punpckldq	%xmm3, %xmm12
  punpcklqdq	%xmm12, %xmm15
  movdqa	%xmm15, %xmm12
  movq	%r15, %xmm15
  paddd	%xmm2, %xmm12
  paddd	%xmm12, %xmm13
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$8, %xmm5
  pslld	$24, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$147, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm0
  pslld	$25, %xmm12
  pxor	%xmm12, %xmm2
  movd	%r11d, %xmm12
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm1, %xmm12
  punpcklqdq	%xmm12, %xmm15
  movdqa	%xmm15, %xmm12
  movd	%ebx, %xmm15
  paddd	%xmm2, %xmm12
  paddd	%xmm12, %xmm13
  pxor	%xmm13, %xmm5
  movdqa	%xmm5, %xmm12
  psrld	$16, %xmm5
  pslld	$16, %xmm12
  pxor	%xmm12, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm12
  psrld	$12, %xmm2
  pslld	$20, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm4, %xmm12
  punpckldq	%xmm15, %xmm12
  movq	%r12, %xmm15
  punpcklqdq	%xmm15, %xmm12
  paddd	%xmm2, %xmm12
  paddd	%xmm13, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm15
  psrld	$8, %xmm5
  pslld	$24, %xmm15
  pxor	%xmm15, %xmm5
  paddd	%xmm5, %xmm0
  movdqa	%xmm8, %xmm15
  movdqa	%xmm7, %xmm8
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm13
  psrld	$7, %xmm2
  punpckldq	%xmm4, %xmm15
  pslld	$25, %xmm13
  pshufd	$57, %xmm5, %xmm5
  pshufd	$78, %xmm0, %xmm0
  pxor	%xmm13, %xmm2
  movdqa	%xmm15, %xmm13
  movd	%edi, %xmm15
  pshufd	$147, %xmm2, %xmm2
  punpckldq	%xmm15, %xmm8
  punpcklqdq	%xmm13, %xmm8
  paddd	%xmm2, %xmm8
  movd	%r11d, %xmm15
  paddd	%xmm8, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm8
  psrld	$16, %xmm5
  pslld	$16, %xmm8
  pxor	%xmm8, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm8
  psrld	$12, %xmm2
  pslld	$20, %xmm8
  pxor	%xmm8, %xmm2
  movd	%eax, %xmm8
  punpckldq	%xmm15, %xmm8
  movd	%ebp, %xmm15
  punpckldq	%xmm3, %xmm15
  movdqa	%xmm15, %xmm3
  punpcklqdq	%xmm8, %xmm3
  movdqa	%xmm3, %xmm8
  movdqa	%xmm11, %xmm3
  paddd	%xmm2, %xmm8
  paddd	%xmm8, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm8
  psrld	$8, %xmm5
  pslld	$24, %xmm8
  pxor	%xmm8, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$147, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm8
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm0
  pslld	$25, %xmm8
  pxor	%xmm8, %xmm2
  movd	%r10d, %xmm8
  pshufd	$57, %xmm2, %xmm2
  punpckldq	%xmm8, %xmm3
  movdqa	%xmm6, %xmm8
  punpckldq	%xmm4, %xmm6
  punpcklqdq	%xmm6, %xmm14
  punpckldq	%xmm9, %xmm8
  punpcklqdq	%xmm3, %xmm8
  paddd	%xmm2, %xmm8
  movdqa	(%rsp), %xmm4
  paddd	%xmm8, %xmm12
  movaps	48(%rsp), %xmm6
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm8
  psrld	$16, %xmm5
  pslld	$16, %xmm8
  pxor	%xmm8, %xmm5
  paddd	%xmm5, %xmm0
  movd	%r9d, %xmm8
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm3
  psrld	$12, %xmm2
  pslld	$20, %xmm3
  pxor	%xmm3, %xmm2
  movd	%ebx, %xmm3
  punpckldq	%xmm8, %xmm3
  movdqa	%xmm1, %xmm8
  punpckldq	%xmm10, %xmm8
  punpcklqdq	%xmm3, %xmm8
  paddd	%xmm2, %xmm8
  punpckldq	%xmm11, %xmm10
  paddd	%xmm8, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm8
  psrld	$8, %xmm5
  pslld	$24, %xmm8
  pxor	%xmm8, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$57, %xmm5, %xmm5
  pxor	%xmm0, %xmm2
  movdqa	%xmm2, %xmm3
  psrld	$7, %xmm2
  pshufd	$78, %xmm0, %xmm0
  pslld	$25, %xmm3
  movdqa	%xmm3, %xmm13
  movd	%r9d, %xmm3
  pxor	%xmm2, %xmm13
  movq	%rsi, %xmm2
  pshufd	$147, %xmm13, %xmm13
  punpckldq	%xmm3, %xmm7
  punpcklqdq	%xmm10, %xmm2
  paddd	%xmm13, %xmm2
  movd	%ebx, %xmm3
  paddd	%xmm2, %xmm12
  punpckldq	%xmm3, %xmm1
  punpcklqdq	%xmm7, %xmm1
  movdqa	16(%rsp), %xmm3
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm2
  psrld	$16, %xmm5
  movaps	64(%rsp), %xmm7
  pslld	$16, %xmm2
  pxor	%xmm2, %xmm5
  paddd	%xmm5, %xmm0
  pxor	%xmm0, %xmm13
  movdqa	%xmm13, %xmm8
  psrld	$12, %xmm13
  pslld	$20, %xmm8
  pxor	%xmm8, %xmm13
  paddd	%xmm13, %xmm1
  paddd	%xmm1, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm1
  psrld	$8, %xmm5
  pslld	$24, %xmm1
  pxor	%xmm1, %xmm5
  paddd	%xmm5, %xmm0
  pshufd	$147, %xmm5, %xmm5
  pxor	%xmm0, %xmm13
  movdqa	%xmm13, %xmm1
  psrld	$7, %xmm13
  pshufd	$78, %xmm0, %xmm0
  pslld	$25, %xmm1
  pxor	%xmm1, %xmm13
  movd	%eax, %xmm1
  pshufd	$57, %xmm13, %xmm13
  punpckldq	%xmm9, %xmm1
  punpcklqdq	%xmm1, %xmm15
  paddd	%xmm13, %xmm15
  paddd	%xmm15, %xmm12
  pxor	%xmm12, %xmm5
  movdqa	%xmm5, %xmm2
  psrld	$16, %xmm5
  pslld	$16, %xmm2
  pxor	%xmm5, %xmm2
  paddd	%xmm2, %xmm0
  pxor	%xmm0, %xmm13
  movdqa	%xmm13, %xmm1
  psrld	$12, %xmm13
  pslld	$20, %xmm1
  pxor	%xmm1, %xmm13
  paddd	%xmm13, %xmm14
  movdqa	%xmm13, %xmm1
  paddd	%xmm14, %xmm12
  pxor	%xmm12, %xmm2
  movdqa	%xmm2, %xmm5
  psrld	$8, %xmm2
  pslld	$24, %xmm5
  pxor	%xmm5, %xmm2
  paddd	%xmm2, %xmm0
  pshufd	$57, %xmm2, %xmm2
  pxor	%xmm0, %xmm1
  movdqa	%xmm1, %xmm5
  psrld	$7, %xmm1
  pshufd	$78, %xmm0, %xmm0
  pslld	$25, %xmm5
  pxor	%xmm0, %xmm4
  pxor	%xmm12, %xmm4
  movups	%xmm4, (%rcx)
  pxor	%xmm5, %xmm1
  pshufd	$147, %xmm1, %xmm1
  pxor	%xmm1, %xmm3
  pxor	%xmm2, %xmm3
  movups	%xmm3, 16(%rcx)
  movaps	80(%rsp), %xmm8
  movaps	96(%rsp), %xmm9
  movaps	112(%rsp), %xmm10
  movaps	128(%rsp), %xmm11
  movaps	144(%rsp), %xmm12
  movaps	160(%rsp), %xmm13
  movaps	176(%rsp), %xmm14
  movaps	192(%rsp), %xmm15
  addq	$208, %rsp
  popq	%rbx
  popq	%rbp
  popq	%r12
  popq	%r14
  popq	%r15
{$IF DEFINED(WINDOWS)}
  popq	%rsi
  popq	%rdi
{$ELSE IF DEFINED(UNIX)}
  popq	%rdx
  popq	%rcx
{$ENDIF}
end;

