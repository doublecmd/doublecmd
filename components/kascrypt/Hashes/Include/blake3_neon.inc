procedure blake3_hash4_neon(inputs: ppcuint8;
                            blocks: csize_t; const key: pcuint32;
                            counter: cuint64; increment_counter: boolean32;
                            flags: cuint8; flags_start: cuint8;
                            flags_end: cuint8; out_: pcuint8); assembler; nostackframe;
asm
  .long 0x6db827e8  // stp	d8, d9, [sp, #-128]!
  .long 0x6d033fee  // stp	d14, d15, [sp, #48]
  .long 0xf94043ea  // ldr	x10, [sp, #128]
  .long 0x4d40c85f  // ld1r	{v31.4s}, [x2]
  .long 0x2d40f85c  // ldp	s28, s30, [x2, #4]
  .long 0x4e04079c  // dup	v28.4s, v28.s[0]
  .long 0x4e0407de  // dup	v30.4s, v30.s[0]
  .long 0x2d41845d  // ldp	s29, s1, [x2, #12]
  .long 0x4e0407bd  // dup	v29.4s, v29.s[0]
  .long 0x4e040421  // dup	v1.4s, v1.s[0]
  .long 0x2d42804f  // ldp	s15, s0, [x2, #20]
  .long 0x4e0405ef  // dup	v15.4s, v15.s[0]
  .long 0x4e040400  // dup	v0.4s, v0.s[0]
  .long 0xbd401c5b  // ldr	s27, [x2, #28]
  .long 0x4e040769  // dup	v9.4s, v27.s[0]
  .long 0x92401c84  // and	x4, x4, #0xff
  .long 0x8b04006b  // add	x11, x3, x4
  .long 0x8b040462  // add	x2, x3, x4, lsl #1
  .long 0x8b040044  // add	x4, x2, x4
  .long 0xd2800008  // mov	x8, #0x0                   	// #0
  .long 0xd2800009  // mov	x9, #0x0                   	// #0
  .long 0xb3407c68  // bfxil	x8, x3, #0, #32
  .long 0xb3607d68  // bfi	x8, x11, #32, #32
  .long 0xb3407c49  // bfxil	x9, x2, #0, #32
  .long 0xb3607c89  // bfi	x9, x4, #32, #32
  .long 0x9e67011b  // fmov	d27, x8
  .long 0x4e181d3b  // mov	v27.d[1], x9
  .long 0x3d801ffb  // str	q27, [sp, #112]
  .long 0xd360fc63  // lsr	x3, x3, #32
  .long 0xd2800008  // mov	x8, #0x0                   	// #0
  .long 0xd2800009  // mov	x9, #0x0                   	// #0
  .long 0xb3407c68  // bfxil	x8, x3, #0, #32
  .long 0xd360fd6b  // lsr	x11, x11, #32
  .long 0xb3607d68  // bfi	x8, x11, #32, #32
  .long 0xd360fc42  // lsr	x2, x2, #32
  .long 0xb3407c49  // bfxil	x9, x2, #0, #32
  .long 0xd360fc84  // lsr	x4, x4, #32
  .long 0xb3607c89  // bfi	x9, x4, #32, #32
  .long 0x9e67011b  // fmov	d27, x8
  .long 0x4e181d3b  // mov	v27.d[1], x9
  .long 0x3d801bfb  // str	q27, [sp, #96]
  .long 0xb40081e1  // cbz	x1, 10e0 <blake3_hash4_neon+0x10e0>
  .long 0x6d012fea  // stp	d10, d11, [sp, #16]
  .long 0x6d0237ec  // stp	d12, d13, [sp, #32]
  .long 0x12001ca5  // and	w5, w5, #0xff
  .long 0x12001cc6  // and	w6, w6, #0xff
  .long 0x12001ce7  // and	w7, w7, #0xff
  .long 0x2a0600a6  // orr	w6, w5, w6
  .long 0xa9401003  // ldp	x3, x4, [x0]
  .long 0xa9410800  // ldp	x0, x2, [x0, #16]
  .long 0xd2804028  // mov	x8, #0x201                 	// #513
  .long 0xf2a00068  // movk	x8, #0x3, lsl #16
  .long 0xf2c0c0a8  // movk	x8, #0x605, lsl #32
  .long 0xf2e080e8  // movk	x8, #0x407, lsl #48
  .long 0xd2814129  // mov	x9, #0xa09                 	// #2569
  .long 0xf2a10169  // movk	x9, #0x80b, lsl #16
  .long 0xf2c1c1a9  // movk	x9, #0xe0d, lsl #32
  .long 0xf2e181e9  // movk	x9, #0xc0f, lsl #48
  .long 0x4e081d07  // mov	v7.d[0], x8
  .long 0x4e181d27  // mov	v7.d[1], x9
  .long 0xd2800008  // mov	x8, #0x0                   	// #0
  .long 0x4eaf1de2  // mov	v2.16b, v15.16b
  .long 0x4ea11c26  // mov	v6.16b, v1.16b
  .long 0x14000002  // b	104 <blake3_hash4_neon+0x104>
  .long 0x2a0503e6  // mov	w6, w5
  .long 0x91000508  // add	x8, x8, #0x1
  .long 0x2a0600e9  // orr	w9, w7, w6
  .long 0x12001d29  // and	w9, w9, #0xff
  .long 0xeb01011f  // cmp	x8, x1
  .long 0x1a860126  // csel	w6, w9, w6, eq	// eq = none
  .long 0xad405c76  // ldp	q22, q23, [x3]
  .long 0xad402c90  // ldp	q16, q11, [x4]
  .long 0xad406413  // ldp	q19, q25, [x0]
  .long 0xad40344c  // ldp	q12, q13, [x2]
  .long 0xad415474  // ldp	q20, q21, [x3, #32]
  .long 0xad413c8e  // ldp	q14, q15, [x4, #32]
  .long 0xad416c1a  // ldp	q26, q27, [x0, #32]
  .long 0xad416051  // ldp	q17, q24, [x2, #32]
  .long 0x4e902ad2  // trn1	v18.4s, v22.4s, v16.4s
  .long 0x4e906ad6  // trn2	v22.4s, v22.4s, v16.4s
  .long 0x4e8c2a70  // trn1	v16.4s, v19.4s, v12.4s
  .long 0x4e8c6a73  // trn2	v19.4s, v19.4s, v12.4s
  .long 0x4ed01a4c  // uzp1	v12.2d, v18.2d, v16.2d
  .long 0x4ed31aca  // uzp1	v10.2d, v22.2d, v19.2d
  .long 0x5e180652  // mov	d18, v18.d[1]
  .long 0x5e180610  // mov	d16, v16.d[1]
  .long 0x4ed01a52  // uzp1	v18.2d, v18.2d, v16.2d
  .long 0x5e1806d6  // mov	d22, v22.d[1]
  .long 0x5e180673  // mov	d19, v19.d[1]
  .long 0x4ed31ad6  // uzp1	v22.2d, v22.2d, v19.2d
  .long 0x4e8b2af0  // trn1	v16.4s, v23.4s, v11.4s
  .long 0x4e8b6af7  // trn2	v23.4s, v23.4s, v11.4s
  .long 0x4e8d2b33  // trn1	v19.4s, v25.4s, v13.4s
  .long 0x4e8d6b39  // trn2	v25.4s, v25.4s, v13.4s
  .long 0x4ed31a05  // uzp1	v5.2d, v16.2d, v19.2d
  .long 0x4ed91ae8  // uzp1	v8.2d, v23.2d, v25.2d
  .long 0x5e180610  // mov	d16, v16.d[1]
  .long 0x5e180673  // mov	d19, v19.d[1]
  .long 0x4ed31a10  // uzp1	v16.2d, v16.2d, v19.2d
  .long 0x5e1806f7  // mov	d23, v23.d[1]
  .long 0x5e180739  // mov	d25, v25.d[1]
  .long 0x4ed91af7  // uzp1	v23.2d, v23.2d, v25.2d
  .long 0x4e8e2a93  // trn1	v19.4s, v20.4s, v14.4s
  .long 0x4e8e6a94  // trn2	v20.4s, v20.4s, v14.4s
  .long 0x4e912b59  // trn1	v25.4s, v26.4s, v17.4s
  .long 0x4e916b5a  // trn2	v26.4s, v26.4s, v17.4s
  .long 0x4ed91a6b  // uzp1	v11.2d, v19.2d, v25.2d
  .long 0x4eda1a8d  // uzp1	v13.2d, v20.2d, v26.2d
  .long 0x5e180673  // mov	d19, v19.d[1]
  .long 0x5e180739  // mov	d25, v25.d[1]
  .long 0x4ed91a73  // uzp1	v19.2d, v19.2d, v25.2d
  .long 0x5e180694  // mov	d20, v20.d[1]
  .long 0x5e18075a  // mov	d26, v26.d[1]
  .long 0x4eda1a94  // uzp1	v20.2d, v20.2d, v26.2d
  .long 0x4e8f2ab1  // trn1	v17.4s, v21.4s, v15.4s
  .long 0x4e8f6ab5  // trn2	v21.4s, v21.4s, v15.4s
  .long 0x4e982b7a  // trn1	v26.4s, v27.4s, v24.4s
  .long 0x4e986b7b  // trn2	v27.4s, v27.4s, v24.4s
  .long 0x6d056bf1  // stp	d17, d26, [sp, #80]
  .long 0x4edb1aae  // uzp1	v14.2d, v21.2d, v27.2d
  .long 0x5e180631  // mov	d17, v17.d[1]
  .long 0x5e18075a  // mov	d26, v26.d[1]
  .long 0x4eda1a31  // uzp1	v17.2d, v17.2d, v26.2d
  .long 0x5e1806b5  // mov	d21, v21.d[1]
  .long 0x5e18077b  // mov	d27, v27.d[1]
  .long 0x4edb1ab5  // uzp1	v21.2d, v21.2d, v27.2d
  .long 0x4ea68581  // add	v1.4s, v12.4s, v6.4s
  .long 0x4ebf8421  // add	v1.4s, v1.4s, v31.4s
  .long 0x4ea2864f  // add	v15.4s, v18.4s, v2.4s
  .long 0x4ebc85ef  // add	v15.4s, v15.4s, v28.4s
  .long 0x3d8013e5  // str	q5, [sp, #64]
  .long 0x4ea084a5  // add	v5.4s, v5.4s, v0.4s
  .long 0x4ebe84a5  // add	v5.4s, v5.4s, v30.4s
  .long 0x4ea98603  // add	v3.4s, v16.4s, v9.4s
  .long 0x4ebd8463  // add	v3.4s, v3.4s, v29.4s
  .long 0x3dc01fff  // ldr	q31, [sp, #112]
  .long 0x6e211ffc  // eor	v28.16b, v31.16b, v1.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x3dc01bff  // ldr	q31, [sp, #96]
  .long 0x6e2f1ffd  // eor	v29.16b, v31.16b, v15.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x4f02041f  // movi	v31.4s, #0x40
  .long 0x6e3f1cbe  // eor	v30.16b, v5.16b, v31.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4e040cdf  // dup	v31.4s, w6
  .long 0x6e231fff  // eor	v31.16b, v31.16b, v3.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0xd29ccce6  // mov	x6, #0xe667                	// #58983
  .long 0xf2ad4126  // movk	x6, #0x6a09, lsl #16
  .long 0xf2dccce6  // movk	x6, #0xe667, lsl #32
  .long 0xf2ed4126  // movk	x6, #0x6a09, lsl #48
  .long 0x4e081cdb  // mov	v27.d[0], x6
  .long 0x4e181cdb  // mov	v27.d[1], x6
  .long 0x4ebc8778  // add	v24.4s, v27.4s, v28.4s
  .long 0xd295d0a6  // mov	x6, #0xae85                	// #44677
  .long 0xf2b76ce6  // movk	x6, #0xbb67, lsl #16
  .long 0xf2d5d0a6  // movk	x6, #0xae85, lsl #32
  .long 0xf2f76ce6  // movk	x6, #0xbb67, lsl #48
  .long 0x4e081cdb  // mov	v27.d[0], x6
  .long 0x4e181cdb  // mov	v27.d[1], x6
  .long 0x4ebd8779  // add	v25.4s, v27.4s, v29.4s
  .long 0xd29e6e46  // mov	x6, #0xf372                	// #62322
  .long 0xf2a78dc6  // movk	x6, #0x3c6e, lsl #16
  .long 0xf2de6e46  // movk	x6, #0xf372, lsl #32
  .long 0xf2e78dc6  // movk	x6, #0x3c6e, lsl #48
  .long 0x4e081cdb  // mov	v27.d[0], x6
  .long 0x4e181cdb  // mov	v27.d[1], x6
  .long 0x4ebe877a  // add	v26.4s, v27.4s, v30.4s
  .long 0xd29ea746  // mov	x6, #0xf53a                	// #62778
  .long 0xf2b4a9e6  // movk	x6, #0xa54f, lsl #16
  .long 0xf2dea746  // movk	x6, #0xf53a, lsl #32
  .long 0xf2f4a9e6  // movk	x6, #0xa54f, lsl #48
  .long 0x4e081cdb  // mov	v27.d[0], x6
  .long 0x4e181cdb  // mov	v27.d[1], x6
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e221f22  // eor	v2.16b, v25.16b, v2.16b
  .long 0x6e201f40  // eor	v0.16b, v26.16b, v0.16b
  .long 0x6e291f69  // eor	v9.16b, v27.16b, v9.16b
  .long 0x4f3454c4  // shl	v4.4s, v6.4s, #20
  .long 0x6f3444c4  // sri	v4.4s, v6.4s, #12
  .long 0x4f345446  // shl	v6.4s, v2.4s, #20
  .long 0x6f344446  // sri	v6.4s, v2.4s, #12
  .long 0x4f345402  // shl	v2.4s, v0.4s, #20
  .long 0x6f344402  // sri	v2.4s, v0.4s, #12
  .long 0x4f345520  // shl	v0.4s, v9.4s, #20
  .long 0x6f344520  // sri	v0.4s, v9.4s, #12
  .long 0x4eaa8429  // add	v9.4s, v1.4s, v10.4s
  .long 0x4ea48529  // add	v9.4s, v9.4s, v4.4s
  .long 0x4eb685ef  // add	v15.4s, v15.4s, v22.4s
  .long 0x4ea685ef  // add	v15.4s, v15.4s, v6.4s
  .long 0x4ea884a5  // add	v5.4s, v5.4s, v8.4s
  .long 0x4ea284a5  // add	v5.4s, v5.4s, v2.4s
  .long 0x4eb78461  // add	v1.4s, v3.4s, v23.4s
  .long 0x4ea08421  // add	v1.4s, v1.4s, v0.4s
  .long 0x6e3c1d3c  // eor	v28.16b, v9.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1dfd  // eor	v29.16b, v15.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1cbe  // eor	v30.16b, v5.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1c3f  // eor	v31.16b, v1.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381c84  // eor	v4.16b, v4.16b, v24.16b
  .long 0x6e391cc6  // eor	v6.16b, v6.16b, v25.16b
  .long 0x6e3a1c42  // eor	v2.16b, v2.16b, v26.16b
  .long 0x6e3b1c00  // eor	v0.16b, v0.16b, v27.16b
  .long 0x4f395483  // shl	v3.4s, v4.4s, #25
  .long 0x6f394483  // sri	v3.4s, v4.4s, #7
  .long 0x4f3954c4  // shl	v4.4s, v6.4s, #25
  .long 0x6f3944c4  // sri	v4.4s, v6.4s, #7
  .long 0x4f395446  // shl	v6.4s, v2.4s, #25
  .long 0x6f394446  // sri	v6.4s, v2.4s, #7
  .long 0x4f395402  // shl	v2.4s, v0.4s, #25
  .long 0x6f394402  // sri	v2.4s, v0.4s, #7
  .long 0x4eab8480  // add	v0.4s, v4.4s, v11.4s
  .long 0x4ea98400  // add	v0.4s, v0.4s, v9.4s
  .long 0x4eb384c9  // add	v9.4s, v6.4s, v19.4s
  .long 0x4eaf8529  // add	v9.4s, v9.4s, v15.4s
  .long 0x3dc017ef  // ldr	q15, [sp, #80]
  .long 0x4eaf844f  // add	v15.4s, v2.4s, v15.4s
  .long 0x4ea585ef  // add	v15.4s, v15.4s, v5.4s
  .long 0x4eb18465  // add	v5.4s, v3.4s, v17.4s
  .long 0x4ea184a5  // add	v5.4s, v5.4s, v1.4s
  .long 0x6e3f1c1f  // eor	v31.16b, v0.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x6e3c1d3c  // eor	v28.16b, v9.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1dfd  // eor	v29.16b, v15.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1cbe  // eor	v30.16b, v5.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c84  // eor	v4.16b, v4.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381c42  // eor	v2.16b, v2.16b, v24.16b
  .long 0x6e391c63  // eor	v3.16b, v3.16b, v25.16b
  .long 0x4f345481  // shl	v1.4s, v4.4s, #20
  .long 0x6f344481  // sri	v1.4s, v4.4s, #12
  .long 0x4f3454c4  // shl	v4.4s, v6.4s, #20
  .long 0x6f3444c4  // sri	v4.4s, v6.4s, #12
  .long 0x4f345446  // shl	v6.4s, v2.4s, #20
  .long 0x6f344446  // sri	v6.4s, v2.4s, #12
  .long 0x4f345462  // shl	v2.4s, v3.4s, #20
  .long 0x6f344462  // sri	v2.4s, v3.4s, #12
  .long 0x4ead8423  // add	v3.4s, v1.4s, v13.4s
  .long 0x4ea08463  // add	v3.4s, v3.4s, v0.4s
  .long 0x4eb48480  // add	v0.4s, v4.4s, v20.4s
  .long 0x4ea98400  // add	v0.4s, v0.4s, v9.4s
  .long 0x4eae84c9  // add	v9.4s, v6.4s, v14.4s
  .long 0x4eaf8529  // add	v9.4s, v9.4s, v15.4s
  .long 0x4eb5844f  // add	v15.4s, v2.4s, v21.4s
  .long 0x4ea585ef  // add	v15.4s, v15.4s, v5.4s
  .long 0x6e3f1c7f  // eor	v31.16b, v3.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x6e3c1c1c  // eor	v28.16b, v0.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1d3d  // eor	v29.16b, v9.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1dfe  // eor	v30.16b, v15.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c21  // eor	v1.16b, v1.16b, v26.16b
  .long 0x6e3b1c84  // eor	v4.16b, v4.16b, v27.16b
  .long 0x6e381cc6  // eor	v6.16b, v6.16b, v24.16b
  .long 0x6e391c42  // eor	v2.16b, v2.16b, v25.16b
  .long 0x4f395425  // shl	v5.4s, v1.4s, #25
  .long 0x6f394425  // sri	v5.4s, v1.4s, #7
  .long 0x4f395481  // shl	v1.4s, v4.4s, #25
  .long 0x6f394481  // sri	v1.4s, v4.4s, #7
  .long 0x4f3954c4  // shl	v4.4s, v6.4s, #25
  .long 0x6f3944c4  // sri	v4.4s, v6.4s, #7
  .long 0x4f395446  // shl	v6.4s, v2.4s, #25
  .long 0x6f394446  // sri	v6.4s, v2.4s, #7
  .long 0x4eb284c2  // add	v2.4s, v6.4s, v18.4s
  .long 0x4ea38442  // add	v2.4s, v2.4s, v3.4s
  .long 0x4eb684a3  // add	v3.4s, v5.4s, v22.4s
  .long 0x4ea08463  // add	v3.4s, v3.4s, v0.4s
  .long 0x4eb78420  // add	v0.4s, v1.4s, v23.4s
  .long 0x4ea98400  // add	v0.4s, v0.4s, v9.4s
  .long 0x3dc013e9  // ldr	q9, [sp, #64]
  .long 0x4ea98489  // add	v9.4s, v4.4s, v9.4s
  .long 0x4eaf8529  // add	v9.4s, v9.4s, v15.4s
  .long 0x6e3c1c5c  // eor	v28.16b, v2.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1c7d  // eor	v29.16b, v3.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c1e  // eor	v30.16b, v0.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x6e3f1d3f  // eor	v31.16b, v9.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e251f25  // eor	v5.16b, v25.16b, v5.16b
  .long 0x6e211f41  // eor	v1.16b, v26.16b, v1.16b
  .long 0x6e241f64  // eor	v4.16b, v27.16b, v4.16b
  .long 0x4f3454cf  // shl	v15.4s, v6.4s, #20
  .long 0x6f3444cf  // sri	v15.4s, v6.4s, #12
  .long 0x4f3454a6  // shl	v6.4s, v5.4s, #20
  .long 0x6f3444a6  // sri	v6.4s, v5.4s, #12
  .long 0x4f345425  // shl	v5.4s, v1.4s, #20
  .long 0x6f344425  // sri	v5.4s, v1.4s, #12
  .long 0x4f345481  // shl	v1.4s, v4.4s, #20
  .long 0x6f344481  // sri	v1.4s, v4.4s, #12
  .long 0x4eb085e4  // add	v4.4s, v15.4s, v16.4s
  .long 0x4ea28484  // add	v4.4s, v4.4s, v2.4s
  .long 0x4eb384c2  // add	v2.4s, v6.4s, v19.4s
  .long 0x4ea38442  // add	v2.4s, v2.4s, v3.4s
  .long 0x4eac84a3  // add	v3.4s, v5.4s, v12.4s
  .long 0x4ea08463  // add	v3.4s, v3.4s, v0.4s
  .long 0x4eae8420  // add	v0.4s, v1.4s, v14.4s
  .long 0x4ea98400  // add	v0.4s, v0.4s, v9.4s
  .long 0x6e3c1c9c  // eor	v28.16b, v4.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1c5d  // eor	v29.16b, v2.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1c7e  // eor	v30.16b, v3.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1c1f  // eor	v31.16b, v0.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381def  // eor	v15.16b, v15.16b, v24.16b
  .long 0x6e391cc6  // eor	v6.16b, v6.16b, v25.16b
  .long 0x6e3a1ca5  // eor	v5.16b, v5.16b, v26.16b
  .long 0x6e3b1c21  // eor	v1.16b, v1.16b, v27.16b
  .long 0x4f3955e9  // shl	v9.4s, v15.4s, #25
  .long 0x6f3945e9  // sri	v9.4s, v15.4s, #7
  .long 0x4f3954cf  // shl	v15.4s, v6.4s, #25
  .long 0x6f3944cf  // sri	v15.4s, v6.4s, #7
  .long 0x4f3954a6  // shl	v6.4s, v5.4s, #25
  .long 0x6f3944a6  // sri	v6.4s, v5.4s, #7
  .long 0x4f395425  // shl	v5.4s, v1.4s, #25
  .long 0x6f394425  // sri	v5.4s, v1.4s, #7
  .long 0x4eaa85e1  // add	v1.4s, v15.4s, v10.4s
  .long 0x4ea48421  // add	v1.4s, v1.4s, v4.4s
  .long 0x3dc017e4  // ldr	q4, [sp, #80]
  .long 0x4ea484c4  // add	v4.4s, v6.4s, v4.4s
  .long 0x4ea28484  // add	v4.4s, v4.4s, v2.4s
  .long 0x4ead84a2  // add	v2.4s, v5.4s, v13.4s
  .long 0x4ea38442  // add	v2.4s, v2.4s, v3.4s
  .long 0x4eb58523  // add	v3.4s, v9.4s, v21.4s
  .long 0x4ea08463  // add	v3.4s, v3.4s, v0.4s
  .long 0x6e3f1c3f  // eor	v31.16b, v1.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x6e3c1c9c  // eor	v28.16b, v4.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1c5d  // eor	v29.16b, v2.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c7e  // eor	v30.16b, v3.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1def  // eor	v15.16b, v15.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381ca5  // eor	v5.16b, v5.16b, v24.16b
  .long 0x6e391d29  // eor	v9.16b, v9.16b, v25.16b
  .long 0x4f3455e0  // shl	v0.4s, v15.4s, #20
  .long 0x6f3445e0  // sri	v0.4s, v15.4s, #12
  .long 0x4f3454cf  // shl	v15.4s, v6.4s, #20
  .long 0x6f3444cf  // sri	v15.4s, v6.4s, #12
  .long 0x4f3454a6  // shl	v6.4s, v5.4s, #20
  .long 0x6f3444a6  // sri	v6.4s, v5.4s, #12
  .long 0x4f345525  // shl	v5.4s, v9.4s, #20
  .long 0x6f344525  // sri	v5.4s, v9.4s, #12
  .long 0x4eb48409  // add	v9.4s, v0.4s, v20.4s
  .long 0x4ea18529  // add	v9.4s, v9.4s, v1.4s
  .long 0x4ea885e1  // add	v1.4s, v15.4s, v8.4s
  .long 0x4ea48421  // add	v1.4s, v1.4s, v4.4s
  .long 0x4eb184c4  // add	v4.4s, v6.4s, v17.4s
  .long 0x4ea28484  // add	v4.4s, v4.4s, v2.4s
  .long 0x4eab84a2  // add	v2.4s, v5.4s, v11.4s
  .long 0x4ea38442  // add	v2.4s, v2.4s, v3.4s
  .long 0x6e3f1d3f  // eor	v31.16b, v9.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x6e3c1c3c  // eor	v28.16b, v1.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1c9d  // eor	v29.16b, v4.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1c5e  // eor	v30.16b, v2.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c00  // eor	v0.16b, v0.16b, v26.16b
  .long 0x6e3b1def  // eor	v15.16b, v15.16b, v27.16b
  .long 0x6e381cc6  // eor	v6.16b, v6.16b, v24.16b
  .long 0x6e391ca5  // eor	v5.16b, v5.16b, v25.16b
  .long 0x4f395403  // shl	v3.4s, v0.4s, #25
  .long 0x6f394403  // sri	v3.4s, v0.4s, #7
  .long 0x4f3955e0  // shl	v0.4s, v15.4s, #25
  .long 0x6f3945e0  // sri	v0.4s, v15.4s, #7
  .long 0x4f3954cf  // shl	v15.4s, v6.4s, #25
  .long 0x6f3944cf  // sri	v15.4s, v6.4s, #7
  .long 0x4f3954a6  // shl	v6.4s, v5.4s, #25
  .long 0x6f3944a6  // sri	v6.4s, v5.4s, #7
  .long 0x4eb684c5  // add	v5.4s, v6.4s, v22.4s
  .long 0x4ea984a5  // add	v5.4s, v5.4s, v9.4s
  .long 0x4eb38469  // add	v9.4s, v3.4s, v19.4s
  .long 0x4ea18529  // add	v9.4s, v9.4s, v1.4s
  .long 0x4eae8401  // add	v1.4s, v0.4s, v14.4s
  .long 0x4ea48421  // add	v1.4s, v1.4s, v4.4s
  .long 0x4eb785e4  // add	v4.4s, v15.4s, v23.4s
  .long 0x4ea28484  // add	v4.4s, v4.4s, v2.4s
  .long 0x6e3c1cbc  // eor	v28.16b, v5.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1d3d  // eor	v29.16b, v9.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c3e  // eor	v30.16b, v1.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x6e3f1c9f  // eor	v31.16b, v4.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e231f23  // eor	v3.16b, v25.16b, v3.16b
  .long 0x6e201f40  // eor	v0.16b, v26.16b, v0.16b
  .long 0x6e2f1f6f  // eor	v15.16b, v27.16b, v15.16b
  .long 0x4f3454c2  // shl	v2.4s, v6.4s, #20
  .long 0x6f3444c2  // sri	v2.4s, v6.4s, #12
  .long 0x4f345466  // shl	v6.4s, v3.4s, #20
  .long 0x6f344466  // sri	v6.4s, v3.4s, #12
  .long 0x4f345403  // shl	v3.4s, v0.4s, #20
  .long 0x6f344403  // sri	v3.4s, v0.4s, #12
  .long 0x4f3455e0  // shl	v0.4s, v15.4s, #20
  .long 0x6f3445e0  // sri	v0.4s, v15.4s, #12
  .long 0x3dc013ef  // ldr	q15, [sp, #64]
  .long 0x4eaf844f  // add	v15.4s, v2.4s, v15.4s
  .long 0x4ea585ef  // add	v15.4s, v15.4s, v5.4s
  .long 0x3dc017e5  // ldr	q5, [sp, #80]
  .long 0x4ea584c5  // add	v5.4s, v6.4s, v5.4s
  .long 0x4ea984a5  // add	v5.4s, v5.4s, v9.4s
  .long 0x4eb28469  // add	v9.4s, v3.4s, v18.4s
  .long 0x4ea18529  // add	v9.4s, v9.4s, v1.4s
  .long 0x4eb18401  // add	v1.4s, v0.4s, v17.4s
  .long 0x4ea48421  // add	v1.4s, v1.4s, v4.4s
  .long 0x6e3c1dfc  // eor	v28.16b, v15.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1cbd  // eor	v29.16b, v5.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1d3e  // eor	v30.16b, v9.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1c3f  // eor	v31.16b, v1.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381c42  // eor	v2.16b, v2.16b, v24.16b
  .long 0x6e391cc6  // eor	v6.16b, v6.16b, v25.16b
  .long 0x6e3a1c63  // eor	v3.16b, v3.16b, v26.16b
  .long 0x6e3b1c00  // eor	v0.16b, v0.16b, v27.16b
  .long 0x4f395444  // shl	v4.4s, v2.4s, #25
  .long 0x6f394444  // sri	v4.4s, v2.4s, #7
  .long 0x4f3954c2  // shl	v2.4s, v6.4s, #25
  .long 0x6f3944c2  // sri	v2.4s, v6.4s, #7
  .long 0x4f395466  // shl	v6.4s, v3.4s, #25
  .long 0x6f394466  // sri	v6.4s, v3.4s, #7
  .long 0x4f395403  // shl	v3.4s, v0.4s, #25
  .long 0x6f394403  // sri	v3.4s, v0.4s, #7
  .long 0x4eb08440  // add	v0.4s, v2.4s, v16.4s
  .long 0x4eaf8400  // add	v0.4s, v0.4s, v15.4s
  .long 0x4ead84cf  // add	v15.4s, v6.4s, v13.4s
  .long 0x4ea585ef  // add	v15.4s, v15.4s, v5.4s
  .long 0x4eb48465  // add	v5.4s, v3.4s, v20.4s
  .long 0x4ea984a5  // add	v5.4s, v5.4s, v9.4s
  .long 0x4eab8489  // add	v9.4s, v4.4s, v11.4s
  .long 0x4ea18529  // add	v9.4s, v9.4s, v1.4s
  .long 0x6e3f1c1f  // eor	v31.16b, v0.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x6e3c1dfc  // eor	v28.16b, v15.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1cbd  // eor	v29.16b, v5.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1d3e  // eor	v30.16b, v9.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c42  // eor	v2.16b, v2.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381c63  // eor	v3.16b, v3.16b, v24.16b
  .long 0x6e391c84  // eor	v4.16b, v4.16b, v25.16b
  .long 0x4f345441  // shl	v1.4s, v2.4s, #20
  .long 0x6f344441  // sri	v1.4s, v2.4s, #12
  .long 0x4f3454c2  // shl	v2.4s, v6.4s, #20
  .long 0x6f3444c2  // sri	v2.4s, v6.4s, #12
  .long 0x4f345466  // shl	v6.4s, v3.4s, #20
  .long 0x6f344466  // sri	v6.4s, v3.4s, #12
  .long 0x4f345483  // shl	v3.4s, v4.4s, #20
  .long 0x6f344483  // sri	v3.4s, v4.4s, #12
  .long 0x4ea88424  // add	v4.4s, v1.4s, v8.4s
  .long 0x4ea08484  // add	v4.4s, v4.4s, v0.4s
  .long 0x4eac8440  // add	v0.4s, v2.4s, v12.4s
  .long 0x4eaf8400  // add	v0.4s, v0.4s, v15.4s
  .long 0x4eb584cf  // add	v15.4s, v6.4s, v21.4s
  .long 0x4ea585ef  // add	v15.4s, v15.4s, v5.4s
  .long 0x4eaa8465  // add	v5.4s, v3.4s, v10.4s
  .long 0x4ea984a5  // add	v5.4s, v5.4s, v9.4s
  .long 0x6e3f1c9f  // eor	v31.16b, v4.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x6e3c1c1c  // eor	v28.16b, v0.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1dfd  // eor	v29.16b, v15.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1cbe  // eor	v30.16b, v5.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c21  // eor	v1.16b, v1.16b, v26.16b
  .long 0x6e3b1c42  // eor	v2.16b, v2.16b, v27.16b
  .long 0x6e381cc6  // eor	v6.16b, v6.16b, v24.16b
  .long 0x6e391c63  // eor	v3.16b, v3.16b, v25.16b
  .long 0x4f395429  // shl	v9.4s, v1.4s, #25
  .long 0x6f394429  // sri	v9.4s, v1.4s, #7
  .long 0x4f395441  // shl	v1.4s, v2.4s, #25
  .long 0x6f394441  // sri	v1.4s, v2.4s, #7
  .long 0x4f3954c2  // shl	v2.4s, v6.4s, #25
  .long 0x6f3944c2  // sri	v2.4s, v6.4s, #7
  .long 0x4f395466  // shl	v6.4s, v3.4s, #25
  .long 0x6f394466  // sri	v6.4s, v3.4s, #7
  .long 0x4eb384c3  // add	v3.4s, v6.4s, v19.4s
  .long 0x4ea48463  // add	v3.4s, v3.4s, v4.4s
  .long 0x3dc017e4  // ldr	q4, [sp, #80]
  .long 0x4ea48524  // add	v4.4s, v9.4s, v4.4s
  .long 0x4ea08484  // add	v4.4s, v4.4s, v0.4s
  .long 0x4eb18420  // add	v0.4s, v1.4s, v17.4s
  .long 0x4eaf8400  // add	v0.4s, v0.4s, v15.4s
  .long 0x4eae844f  // add	v15.4s, v2.4s, v14.4s
  .long 0x4ea585ef  // add	v15.4s, v15.4s, v5.4s
  .long 0x6e3c1c7c  // eor	v28.16b, v3.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1c9d  // eor	v29.16b, v4.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c1e  // eor	v30.16b, v0.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x6e3f1dff  // eor	v31.16b, v15.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e291f29  // eor	v9.16b, v25.16b, v9.16b
  .long 0x6e211f41  // eor	v1.16b, v26.16b, v1.16b
  .long 0x6e221f62  // eor	v2.16b, v27.16b, v2.16b
  .long 0x4f3454c5  // shl	v5.4s, v6.4s, #20
  .long 0x6f3444c5  // sri	v5.4s, v6.4s, #12
  .long 0x4f345526  // shl	v6.4s, v9.4s, #20
  .long 0x6f344526  // sri	v6.4s, v9.4s, #12
  .long 0x4f345429  // shl	v9.4s, v1.4s, #20
  .long 0x6f344429  // sri	v9.4s, v1.4s, #12
  .long 0x4f345441  // shl	v1.4s, v2.4s, #20
  .long 0x6f344441  // sri	v1.4s, v2.4s, #12
  .long 0x4eb784a2  // add	v2.4s, v5.4s, v23.4s
  .long 0x4ea38442  // add	v2.4s, v2.4s, v3.4s
  .long 0x4ead84c3  // add	v3.4s, v6.4s, v13.4s
  .long 0x4ea48463  // add	v3.4s, v3.4s, v4.4s
  .long 0x4eb68524  // add	v4.4s, v9.4s, v22.4s
  .long 0x4ea08484  // add	v4.4s, v4.4s, v0.4s
  .long 0x4eb58420  // add	v0.4s, v1.4s, v21.4s
  .long 0x4eaf8400  // add	v0.4s, v0.4s, v15.4s
  .long 0x6e3c1c5c  // eor	v28.16b, v2.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1c7d  // eor	v29.16b, v3.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1c1f  // eor	v31.16b, v0.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381ca5  // eor	v5.16b, v5.16b, v24.16b
  .long 0x6e391cc6  // eor	v6.16b, v6.16b, v25.16b
  .long 0x6e3a1d29  // eor	v9.16b, v9.16b, v26.16b
  .long 0x6e3b1c21  // eor	v1.16b, v1.16b, v27.16b
  .long 0x4f3954af  // shl	v15.4s, v5.4s, #25
  .long 0x6f3944af  // sri	v15.4s, v5.4s, #7
  .long 0x4f3954c5  // shl	v5.4s, v6.4s, #25
  .long 0x6f3944c5  // sri	v5.4s, v6.4s, #7
  .long 0x4f395526  // shl	v6.4s, v9.4s, #25
  .long 0x6f394526  // sri	v6.4s, v9.4s, #7
  .long 0x4f395429  // shl	v9.4s, v1.4s, #25
  .long 0x6f394429  // sri	v9.4s, v1.4s, #7
  .long 0x3dc013e1  // ldr	q1, [sp, #64]
  .long 0x4ea184a1  // add	v1.4s, v5.4s, v1.4s
  .long 0x4ea28421  // add	v1.4s, v1.4s, v2.4s
  .long 0x4eb484c2  // add	v2.4s, v6.4s, v20.4s
  .long 0x4ea38442  // add	v2.4s, v2.4s, v3.4s
  .long 0x4ea88523  // add	v3.4s, v9.4s, v8.4s
  .long 0x4ea48463  // add	v3.4s, v3.4s, v4.4s
  .long 0x4eaa85e4  // add	v4.4s, v15.4s, v10.4s
  .long 0x4ea08484  // add	v4.4s, v4.4s, v0.4s
  .long 0x6e3f1c3f  // eor	v31.16b, v1.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x6e3c1c5c  // eor	v28.16b, v2.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1c7d  // eor	v29.16b, v3.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1ca5  // eor	v5.16b, v5.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381d29  // eor	v9.16b, v9.16b, v24.16b
  .long 0x6e391def  // eor	v15.16b, v15.16b, v25.16b
  .long 0x4f3454a0  // shl	v0.4s, v5.4s, #20
  .long 0x6f3444a0  // sri	v0.4s, v5.4s, #12
  .long 0x4f3454c5  // shl	v5.4s, v6.4s, #20
  .long 0x6f3444c5  // sri	v5.4s, v6.4s, #12
  .long 0x4f345526  // shl	v6.4s, v9.4s, #20
  .long 0x6f344526  // sri	v6.4s, v9.4s, #12
  .long 0x4f3455e9  // shl	v9.4s, v15.4s, #20
  .long 0x6f3445e9  // sri	v9.4s, v15.4s, #12
  .long 0x4eac840f  // add	v15.4s, v0.4s, v12.4s
  .long 0x4ea185ef  // add	v15.4s, v15.4s, v1.4s
  .long 0x4eb284a1  // add	v1.4s, v5.4s, v18.4s
  .long 0x4ea28422  // add	v2.4s, v1.4s, v2.4s
  .long 0x4eab84c1  // add	v1.4s, v6.4s, v11.4s
  .long 0x4ea38423  // add	v3.4s, v1.4s, v3.4s
  .long 0x4eb08521  // add	v1.4s, v9.4s, v16.4s
  .long 0x4ea48424  // add	v4.4s, v1.4s, v4.4s
  .long 0x6e3f1dff  // eor	v31.16b, v15.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x6e3c1c5c  // eor	v28.16b, v2.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1c7d  // eor	v29.16b, v3.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c00  // eor	v0.16b, v0.16b, v26.16b
  .long 0x6e3b1ca5  // eor	v5.16b, v5.16b, v27.16b
  .long 0x6e381cc6  // eor	v6.16b, v6.16b, v24.16b
  .long 0x6e391d29  // eor	v9.16b, v9.16b, v25.16b
  .long 0x4f395401  // shl	v1.4s, v0.4s, #25
  .long 0x6f394401  // sri	v1.4s, v0.4s, #7
  .long 0x4f3954a0  // shl	v0.4s, v5.4s, #25
  .long 0x6f3944a0  // sri	v0.4s, v5.4s, #7
  .long 0x4f3954c5  // shl	v5.4s, v6.4s, #25
  .long 0x6f3944c5  // sri	v5.4s, v6.4s, #7
  .long 0x4f395526  // shl	v6.4s, v9.4s, #25
  .long 0x6f394526  // sri	v6.4s, v9.4s, #7
  .long 0x3dc017e9  // ldr	q9, [sp, #80]
  .long 0x4ea984c9  // add	v9.4s, v6.4s, v9.4s
  .long 0x4eaf852f  // add	v15.4s, v9.4s, v15.4s
  .long 0x4ead8429  // add	v9.4s, v1.4s, v13.4s
  .long 0x4ea28529  // add	v9.4s, v9.4s, v2.4s
  .long 0x4eb58402  // add	v2.4s, v0.4s, v21.4s
  .long 0x4ea38443  // add	v3.4s, v2.4s, v3.4s
  .long 0x4eb184a2  // add	v2.4s, v5.4s, v17.4s
  .long 0x4ea48444  // add	v4.4s, v2.4s, v4.4s
  .long 0x6e3c1dfc  // eor	v28.16b, v15.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1d3d  // eor	v29.16b, v9.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c7e  // eor	v30.16b, v3.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x6e3f1c9f  // eor	v31.16b, v4.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e211f21  // eor	v1.16b, v25.16b, v1.16b
  .long 0x6e201f40  // eor	v0.16b, v26.16b, v0.16b
  .long 0x6e251f65  // eor	v5.16b, v27.16b, v5.16b
  .long 0x4f3454c2  // shl	v2.4s, v6.4s, #20
  .long 0x6f3444c2  // sri	v2.4s, v6.4s, #12
  .long 0x4f345426  // shl	v6.4s, v1.4s, #20
  .long 0x6f344426  // sri	v6.4s, v1.4s, #12
  .long 0x4f345401  // shl	v1.4s, v0.4s, #20
  .long 0x6f344401  // sri	v1.4s, v0.4s, #12
  .long 0x4f3454a0  // shl	v0.4s, v5.4s, #20
  .long 0x6f3444a0  // sri	v0.4s, v5.4s, #12
  .long 0x4eae8445  // add	v5.4s, v2.4s, v14.4s
  .long 0x4eaf84af  // add	v15.4s, v5.4s, v15.4s
  .long 0x4eb484c5  // add	v5.4s, v6.4s, v20.4s
  .long 0x4ea984a9  // add	v9.4s, v5.4s, v9.4s
  .long 0x4eb38425  // add	v5.4s, v1.4s, v19.4s
  .long 0x4ea384a5  // add	v5.4s, v5.4s, v3.4s
  .long 0x4eab8403  // add	v3.4s, v0.4s, v11.4s
  .long 0x4ea48464  // add	v4.4s, v3.4s, v4.4s
  .long 0x6e3c1dfc  // eor	v28.16b, v15.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1d3d  // eor	v29.16b, v9.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1cbe  // eor	v30.16b, v5.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1c9f  // eor	v31.16b, v4.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381c42  // eor	v2.16b, v2.16b, v24.16b
  .long 0x6e391cc6  // eor	v6.16b, v6.16b, v25.16b
  .long 0x6e3a1c21  // eor	v1.16b, v1.16b, v26.16b
  .long 0x6e3b1c00  // eor	v0.16b, v0.16b, v27.16b
  .long 0x4f395443  // shl	v3.4s, v2.4s, #25
  .long 0x6f394443  // sri	v3.4s, v2.4s, #7
  .long 0x4f3954c2  // shl	v2.4s, v6.4s, #25
  .long 0x6f3944c2  // sri	v2.4s, v6.4s, #7
  .long 0x4f395426  // shl	v6.4s, v1.4s, #25
  .long 0x6f394426  // sri	v6.4s, v1.4s, #7
  .long 0x4f395401  // shl	v1.4s, v0.4s, #25
  .long 0x6f394401  // sri	v1.4s, v0.4s, #7
  .long 0x4eb78440  // add	v0.4s, v2.4s, v23.4s
  .long 0x4eaf840f  // add	v15.4s, v0.4s, v15.4s
  .long 0x4ea884c0  // add	v0.4s, v6.4s, v8.4s
  .long 0x4ea98409  // add	v9.4s, v0.4s, v9.4s
  .long 0x4eac8420  // add	v0.4s, v1.4s, v12.4s
  .long 0x4ea58405  // add	v5.4s, v0.4s, v5.4s
  .long 0x4eb08460  // add	v0.4s, v3.4s, v16.4s
  .long 0x4ea48404  // add	v4.4s, v0.4s, v4.4s
  .long 0x6e3f1dff  // eor	v31.16b, v15.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x6e3c1d3c  // eor	v28.16b, v9.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1cbd  // eor	v29.16b, v5.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c42  // eor	v2.16b, v2.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381c21  // eor	v1.16b, v1.16b, v24.16b
  .long 0x6e391c63  // eor	v3.16b, v3.16b, v25.16b
  .long 0x4f345440  // shl	v0.4s, v2.4s, #20
  .long 0x6f344440  // sri	v0.4s, v2.4s, #12
  .long 0x4f3454c2  // shl	v2.4s, v6.4s, #20
  .long 0x6f3444c2  // sri	v2.4s, v6.4s, #12
  .long 0x4f345426  // shl	v6.4s, v1.4s, #20
  .long 0x6f344426  // sri	v6.4s, v1.4s, #12
  .long 0x4f345461  // shl	v1.4s, v3.4s, #20
  .long 0x6f344461  // sri	v1.4s, v3.4s, #12
  .long 0x4eb28403  // add	v3.4s, v0.4s, v18.4s
  .long 0x4eaf846f  // add	v15.4s, v3.4s, v15.4s
  .long 0x4eb68443  // add	v3.4s, v2.4s, v22.4s
  .long 0x4ea98469  // add	v9.4s, v3.4s, v9.4s
  .long 0x4eaa84c3  // add	v3.4s, v6.4s, v10.4s
  .long 0x4ea58465  // add	v5.4s, v3.4s, v5.4s
  .long 0x3dc013e3  // ldr	q3, [sp, #64]
  .long 0x4ea38423  // add	v3.4s, v1.4s, v3.4s
  .long 0x4ea48464  // add	v4.4s, v3.4s, v4.4s
  .long 0x6e3f1dff  // eor	v31.16b, v15.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x6e3c1d3c  // eor	v28.16b, v9.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1cbd  // eor	v29.16b, v5.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c00  // eor	v0.16b, v0.16b, v26.16b
  .long 0x6e3b1c42  // eor	v2.16b, v2.16b, v27.16b
  .long 0x6e381cc6  // eor	v6.16b, v6.16b, v24.16b
  .long 0x6e391c21  // eor	v1.16b, v1.16b, v25.16b
  .long 0x4f395403  // shl	v3.4s, v0.4s, #25
  .long 0x6f394403  // sri	v3.4s, v0.4s, #7
  .long 0x4f395440  // shl	v0.4s, v2.4s, #25
  .long 0x6f394440  // sri	v0.4s, v2.4s, #7
  .long 0x4f3954c2  // shl	v2.4s, v6.4s, #25
  .long 0x6f3944c2  // sri	v2.4s, v6.4s, #7
  .long 0x4f395426  // shl	v6.4s, v1.4s, #25
  .long 0x6f394426  // sri	v6.4s, v1.4s, #7
  .long 0x4ead84c1  // add	v1.4s, v6.4s, v13.4s
  .long 0x4eaf842f  // add	v15.4s, v1.4s, v15.4s
  .long 0x4eb48461  // add	v1.4s, v3.4s, v20.4s
  .long 0x4ea98429  // add	v9.4s, v1.4s, v9.4s
  .long 0x4eab8401  // add	v1.4s, v0.4s, v11.4s
  .long 0x4ea58425  // add	v5.4s, v1.4s, v5.4s
  .long 0x4eb58441  // add	v1.4s, v2.4s, v21.4s
  .long 0x4ea48424  // add	v4.4s, v1.4s, v4.4s
  .long 0x6e3c1dfc  // eor	v28.16b, v15.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1d3d  // eor	v29.16b, v9.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1cbe  // eor	v30.16b, v5.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x6e3f1c9f  // eor	v31.16b, v4.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e231f23  // eor	v3.16b, v25.16b, v3.16b
  .long 0x6e201f40  // eor	v0.16b, v26.16b, v0.16b
  .long 0x6e221f62  // eor	v2.16b, v27.16b, v2.16b
  .long 0x4f3454c1  // shl	v1.4s, v6.4s, #20
  .long 0x6f3444c1  // sri	v1.4s, v6.4s, #12
  .long 0x4f345466  // shl	v6.4s, v3.4s, #20
  .long 0x6f344466  // sri	v6.4s, v3.4s, #12
  .long 0x4f345403  // shl	v3.4s, v0.4s, #20
  .long 0x6f344403  // sri	v3.4s, v0.4s, #12
  .long 0x4f345440  // shl	v0.4s, v2.4s, #20
  .long 0x6f344440  // sri	v0.4s, v2.4s, #12
  .long 0x4eb18422  // add	v2.4s, v1.4s, v17.4s
  .long 0x4eaf844f  // add	v15.4s, v2.4s, v15.4s
  .long 0x4ea884c2  // add	v2.4s, v6.4s, v8.4s
  .long 0x4ea98449  // add	v9.4s, v2.4s, v9.4s
  .long 0x3dc017e2  // ldr	q2, [sp, #80]
  .long 0x4ea28462  // add	v2.4s, v3.4s, v2.4s
  .long 0x4ea58445  // add	v5.4s, v2.4s, v5.4s
  .long 0x4eaa8402  // add	v2.4s, v0.4s, v10.4s
  .long 0x4ea48444  // add	v4.4s, v2.4s, v4.4s
  .long 0x6e3c1dfc  // eor	v28.16b, v15.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1d3d  // eor	v29.16b, v9.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1cbe  // eor	v30.16b, v5.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1c9f  // eor	v31.16b, v4.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381c21  // eor	v1.16b, v1.16b, v24.16b
  .long 0x6e391cc6  // eor	v6.16b, v6.16b, v25.16b
  .long 0x6e3a1c63  // eor	v3.16b, v3.16b, v26.16b
  .long 0x6e3b1c00  // eor	v0.16b, v0.16b, v27.16b
  .long 0x4f395422  // shl	v2.4s, v1.4s, #25
  .long 0x6f394422  // sri	v2.4s, v1.4s, #7
  .long 0x4f3954c1  // shl	v1.4s, v6.4s, #25
  .long 0x6f3944c1  // sri	v1.4s, v6.4s, #7
  .long 0x4f395466  // shl	v6.4s, v3.4s, #25
  .long 0x6f394466  // sri	v6.4s, v3.4s, #7
  .long 0x4f395403  // shl	v3.4s, v0.4s, #25
  .long 0x6f394403  // sri	v3.4s, v0.4s, #7
  .long 0x4eae8420  // add	v0.4s, v1.4s, v14.4s
  .long 0x4eaf840f  // add	v15.4s, v0.4s, v15.4s
  .long 0x4eac84c0  // add	v0.4s, v6.4s, v12.4s
  .long 0x4ea98409  // add	v9.4s, v0.4s, v9.4s
  .long 0x4eb28460  // add	v0.4s, v3.4s, v18.4s
  .long 0x4ea58405  // add	v5.4s, v0.4s, v5.4s
  .long 0x3dc013e0  // ldr	q0, [sp, #64]
  .long 0x4ea08440  // add	v0.4s, v2.4s, v0.4s
  .long 0x4ea48404  // add	v4.4s, v0.4s, v4.4s
  .long 0x6e3f1dff  // eor	v31.16b, v15.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x6e3c1d3c  // eor	v28.16b, v9.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1cbd  // eor	v29.16b, v5.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c21  // eor	v1.16b, v1.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381c63  // eor	v3.16b, v3.16b, v24.16b
  .long 0x6e391c42  // eor	v2.16b, v2.16b, v25.16b
  .long 0x4f345420  // shl	v0.4s, v1.4s, #20
  .long 0x6f344420  // sri	v0.4s, v1.4s, #12
  .long 0x4f3454c1  // shl	v1.4s, v6.4s, #20
  .long 0x6f3444c1  // sri	v1.4s, v6.4s, #12
  .long 0x4f345466  // shl	v6.4s, v3.4s, #20
  .long 0x6f344466  // sri	v6.4s, v3.4s, #12
  .long 0x4f345443  // shl	v3.4s, v2.4s, #20
  .long 0x6f344443  // sri	v3.4s, v2.4s, #12
  .long 0x4eb68402  // add	v2.4s, v0.4s, v22.4s
  .long 0x4eaf844f  // add	v15.4s, v2.4s, v15.4s
  .long 0x4eb38422  // add	v2.4s, v1.4s, v19.4s
  .long 0x4ea98449  // add	v9.4s, v2.4s, v9.4s
  .long 0x4eb084c2  // add	v2.4s, v6.4s, v16.4s
  .long 0x4ea58445  // add	v5.4s, v2.4s, v5.4s
  .long 0x4eb78462  // add	v2.4s, v3.4s, v23.4s
  .long 0x4ea48444  // add	v4.4s, v2.4s, v4.4s
  .long 0x6e3f1dff  // eor	v31.16b, v15.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x6e3c1d3c  // eor	v28.16b, v9.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1cbd  // eor	v29.16b, v5.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1c9e  // eor	v30.16b, v4.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x4ebf875a  // add	v26.4s, v26.4s, v31.4s
  .long 0x4ebc877b  // add	v27.4s, v27.4s, v28.4s
  .long 0x4ebd8718  // add	v24.4s, v24.4s, v29.4s
  .long 0x4ebe8739  // add	v25.4s, v25.4s, v30.4s
  .long 0x6e3a1c00  // eor	v0.16b, v0.16b, v26.16b
  .long 0x6e3b1c21  // eor	v1.16b, v1.16b, v27.16b
  .long 0x6e381cc6  // eor	v6.16b, v6.16b, v24.16b
  .long 0x6e391c63  // eor	v3.16b, v3.16b, v25.16b
  .long 0x4f395402  // shl	v2.4s, v0.4s, #25
  .long 0x6f394402  // sri	v2.4s, v0.4s, #7
  .long 0x4f395420  // shl	v0.4s, v1.4s, #25
  .long 0x6f394420  // sri	v0.4s, v1.4s, #7
  .long 0x4f3954c1  // shl	v1.4s, v6.4s, #25
  .long 0x6f3944c1  // sri	v1.4s, v6.4s, #7
  .long 0x4f395466  // shl	v6.4s, v3.4s, #25
  .long 0x6f394466  // sri	v6.4s, v3.4s, #7
  .long 0x4eb484d4  // add	v20.4s, v6.4s, v20.4s
  .long 0x4eaf8694  // add	v20.4s, v20.4s, v15.4s
  .long 0x4ea88448  // add	v8.4s, v2.4s, v8.4s
  .long 0x4ea98508  // add	v8.4s, v8.4s, v9.4s
  .long 0x4eaa840a  // add	v10.4s, v0.4s, v10.4s
  .long 0x4ea5854a  // add	v10.4s, v10.4s, v5.4s
  .long 0x4eab842b  // add	v11.4s, v1.4s, v11.4s
  .long 0x4ea4856b  // add	v11.4s, v11.4s, v4.4s
  .long 0x6e3c1e9c  // eor	v28.16b, v20.16b, v28.16b
  .long 0x6e600b9c  // rev32	v28.8h, v28.8h
  .long 0x6e3d1d1d  // eor	v29.16b, v8.16b, v29.16b
  .long 0x6e600bbd  // rev32	v29.8h, v29.8h
  .long 0x6e3e1d5e  // eor	v30.16b, v10.16b, v30.16b
  .long 0x6e600bde  // rev32	v30.8h, v30.8h
  .long 0x6e3f1d7f  // eor	v31.16b, v11.16b, v31.16b
  .long 0x6e600bff  // rev32	v31.8h, v31.8h
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e261f06  // eor	v6.16b, v24.16b, v6.16b
  .long 0x6e221f22  // eor	v2.16b, v25.16b, v2.16b
  .long 0x6e201f40  // eor	v0.16b, v26.16b, v0.16b
  .long 0x6e211f61  // eor	v1.16b, v27.16b, v1.16b
  .long 0x4f3454cf  // shl	v15.4s, v6.4s, #20
  .long 0x6f3444cf  // sri	v15.4s, v6.4s, #12
  .long 0x4f345449  // shl	v9.4s, v2.4s, #20
  .long 0x6f344449  // sri	v9.4s, v2.4s, #12
  .long 0x4f345406  // shl	v6.4s, v0.4s, #20
  .long 0x6f344406  // sri	v6.4s, v0.4s, #12
  .long 0x4f345425  // shl	v5.4s, v1.4s, #20
  .long 0x6f344425  // sri	v5.4s, v1.4s, #12
  .long 0x4eb585f5  // add	v21.4s, v15.4s, v21.4s
  .long 0x4eb486a4  // add	v4.4s, v21.4s, v20.4s
  .long 0x4eac852c  // add	v12.4s, v9.4s, v12.4s
  .long 0x4ea8858c  // add	v12.4s, v12.4s, v8.4s
  .long 0x4ead84cd  // add	v13.4s, v6.4s, v13.4s
  .long 0x4eaa85ad  // add	v13.4s, v13.4s, v10.4s
  .long 0x4eb084b0  // add	v16.4s, v5.4s, v16.4s
  .long 0x4eab860b  // add	v11.4s, v16.4s, v11.4s
  .long 0x6e3c1c9c  // eor	v28.16b, v4.16b, v28.16b
  .long 0x4e07039c  // tbl	v28.16b, {v28.16b}, v7.16b
  .long 0x6e3d1d9d  // eor	v29.16b, v12.16b, v29.16b
  .long 0x4e0703bd  // tbl	v29.16b, {v29.16b}, v7.16b
  .long 0x6e3e1dbe  // eor	v30.16b, v13.16b, v30.16b
  .long 0x4e0703de  // tbl	v30.16b, {v30.16b}, v7.16b
  .long 0x6e3f1d7f  // eor	v31.16b, v11.16b, v31.16b
  .long 0x4e0703ff  // tbl	v31.16b, {v31.16b}, v7.16b
  .long 0x4ebc8718  // add	v24.4s, v24.4s, v28.4s
  .long 0x4ebd8739  // add	v25.4s, v25.4s, v29.4s
  .long 0x4ebe875a  // add	v26.4s, v26.4s, v30.4s
  .long 0x4ebf877b  // add	v27.4s, v27.4s, v31.4s
  .long 0x6e381def  // eor	v15.16b, v15.16b, v24.16b
  .long 0x6e391d29  // eor	v9.16b, v9.16b, v25.16b
  .long 0x6e3a1cc6  // eor	v6.16b, v6.16b, v26.16b
  .long 0x6e3b1ca5  // eor	v5.16b, v5.16b, v27.16b
  .long 0x4f3955ea  // shl	v10.4s, v15.4s, #25
  .long 0x6f3945ea  // sri	v10.4s, v15.4s, #7
  .long 0x4f395523  // shl	v3.4s, v9.4s, #25
  .long 0x6f394523  // sri	v3.4s, v9.4s, #7
  .long 0x4f3954c8  // shl	v8.4s, v6.4s, #25
  .long 0x6f3944c8  // sri	v8.4s, v6.4s, #7
  .long 0x4f3954a9  // shl	v9.4s, v5.4s, #25
  .long 0x6f3944a9  // sri	v9.4s, v5.4s, #7
  .long 0x4eb18471  // add	v17.4s, v3.4s, v17.4s
  .long 0x4ea48631  // add	v17.4s, v17.4s, v4.4s
  .long 0x4eb28512  // add	v18.4s, v8.4s, v18.4s
  .long 0x4eac864c  // add	v12.4s, v18.4s, v12.4s
  .long 0x4eb68536  // add	v22.4s, v9.4s, v22.4s
  .long 0x4ead86cd  // add	v13.4s, v22.4s, v13.4s
  .long 0x4eb78557  // add	v23.4s, v10.4s, v23.4s
  .long 0x4eab86f7  // add	v23.4s, v23.4s, v11.4s
  .long 0x6e3f1e3f  // eor	v31.16b, v17.16b, v31.16b
  .long 0x6e600bf5  // rev32	v21.8h, v31.8h
  .long 0x6e3c1d9c  // eor	v28.16b, v12.16b, v28.16b
  .long 0x6e600b8f  // rev32	v15.8h, v28.8h
  .long 0x6e3d1dbd  // eor	v29.16b, v13.16b, v29.16b
  .long 0x6e600bb0  // rev32	v16.8h, v29.8h
  .long 0x6e3e1efe  // eor	v30.16b, v23.16b, v30.16b
  .long 0x6e600bd4  // rev32	v20.8h, v30.8h
  .long 0x4eb5875a  // add	v26.4s, v26.4s, v21.4s
  .long 0x4eaf877b  // add	v27.4s, v27.4s, v15.4s
  .long 0x4eb08718  // add	v24.4s, v24.4s, v16.4s
  .long 0x4eb48739  // add	v25.4s, v25.4s, v20.4s
  .long 0x6e3a1c63  // eor	v3.16b, v3.16b, v26.16b
  .long 0x6e3b1d08  // eor	v8.16b, v8.16b, v27.16b
  .long 0x6e381d29  // eor	v9.16b, v9.16b, v24.16b
  .long 0x6e391d4a  // eor	v10.16b, v10.16b, v25.16b
  .long 0x4f345465  // shl	v5.4s, v3.4s, #20
  .long 0x6f344465  // sri	v5.4s, v3.4s, #12
  .long 0x4f345506  // shl	v6.4s, v8.4s, #20
  .long 0x6f344506  // sri	v6.4s, v8.4s, #12
  .long 0x4f345528  // shl	v8.4s, v9.4s, #20
  .long 0x6f344528  // sri	v8.4s, v9.4s, #12
  .long 0x4f34554b  // shl	v11.4s, v10.4s, #20
  .long 0x6f34454b  // sri	v11.4s, v10.4s, #12
  .long 0x4eb384b3  // add	v19.4s, v5.4s, v19.4s
  .long 0x4eb18673  // add	v19.4s, v19.4s, v17.4s
  .long 0xad420be0  // ldp	q0, q2, [sp, #64]
  .long 0x4ea284d2  // add	v18.4s, v6.4s, v2.4s
  .long 0x4eac8652  // add	v18.4s, v18.4s, v12.4s
  .long 0x4ea08516  // add	v22.4s, v8.4s, v0.4s
  .long 0x4ead86d6  // add	v22.4s, v22.4s, v13.4s
  .long 0x4eae856e  // add	v14.4s, v11.4s, v14.4s
  .long 0x4eb785ce  // add	v14.4s, v14.4s, v23.4s
  .long 0x6e351e75  // eor	v21.16b, v19.16b, v21.16b
  .long 0x4e0702b5  // tbl	v21.16b, {v21.16b}, v7.16b
  .long 0x6e2f1e4f  // eor	v15.16b, v18.16b, v15.16b
  .long 0x4e0701ef  // tbl	v15.16b, {v15.16b}, v7.16b
  .long 0x6e301ed0  // eor	v16.16b, v22.16b, v16.16b
  .long 0x4e070210  // tbl	v16.16b, {v16.16b}, v7.16b
  .long 0x6e341dd4  // eor	v20.16b, v14.16b, v20.16b
  .long 0x4e070294  // tbl	v20.16b, {v20.16b}, v7.16b
  .long 0x4eb5875a  // add	v26.4s, v26.4s, v21.4s
  .long 0x4eaf877b  // add	v27.4s, v27.4s, v15.4s
  .long 0x4eb08718  // add	v24.4s, v24.4s, v16.4s
  .long 0x4eb48739  // add	v25.4s, v25.4s, v20.4s
  .long 0x6e3a1ca5  // eor	v5.16b, v5.16b, v26.16b
  .long 0x6e3b1cc6  // eor	v6.16b, v6.16b, v27.16b
  .long 0x6e381d08  // eor	v8.16b, v8.16b, v24.16b
  .long 0x6e391d6b  // eor	v11.16b, v11.16b, v25.16b
  .long 0x4f3954a2  // shl	v2.4s, v5.4s, #25
  .long 0x6f3944a2  // sri	v2.4s, v5.4s, #7
  .long 0x4f3954c0  // shl	v0.4s, v6.4s, #25
  .long 0x6f3944c0  // sri	v0.4s, v6.4s, #7
  .long 0x4f395509  // shl	v9.4s, v8.4s, #25
  .long 0x6f394509  // sri	v9.4s, v8.4s, #7
  .long 0x4f395566  // shl	v6.4s, v11.4s, #25
  .long 0x6f394566  // sri	v6.4s, v11.4s, #7
  .long 0x6e381e7f  // eor	v31.16b, v19.16b, v24.16b
  .long 0x6e391e5c  // eor	v28.16b, v18.16b, v25.16b
  .long 0x6e3a1ede  // eor	v30.16b, v22.16b, v26.16b
  .long 0x6e3b1ddd  // eor	v29.16b, v14.16b, v27.16b
  .long 0x6e2f1cc6  // eor	v6.16b, v6.16b, v15.16b
  .long 0x6e301c42  // eor	v2.16b, v2.16b, v16.16b
  .long 0x6e341c00  // eor	v0.16b, v0.16b, v20.16b
  .long 0x6e351d29  // eor	v9.16b, v9.16b, v21.16b
  .long 0x91010063  // add	x3, x3, #0x40
  .long 0x91010084  // add	x4, x4, #0x40
  .long 0x91010000  // add	x0, x0, #0x40
  .long 0x91010042  // add	x2, x2, #0x40
  .long 0x54ff81a1  // b.ne	100 <blake3_hash4_neon+0x100>  // b.any
  .long 0x4ea21c4f  // mov	v15.16b, v2.16b
  .long 0x4ea61cc1  // mov	v1.16b, v6.16b
  .long 0x6d412fea  // ldp	d10, d11, [sp, #16]
  .long 0x6d4237ec  // ldp	d12, d13, [sp, #32]
  .long 0x4e9c2bfb  // trn1	v27.4s, v31.4s, v28.4s
  .long 0x4e9c6bff  // trn2	v31.4s, v31.4s, v28.4s
  .long 0x4e9d2bda  // trn1	v26.4s, v30.4s, v29.4s
  .long 0x4e9d6bde  // trn2	v30.4s, v30.4s, v29.4s
  .long 0x4e8f283d  // trn1	v29.4s, v1.4s, v15.4s
  .long 0x4e8f6821  // trn2	v1.4s, v1.4s, v15.4s
  .long 0x4e89281c  // trn1	v28.4s, v0.4s, v9.4s
  .long 0x4e896800  // trn2	v0.4s, v0.4s, v9.4s
  .long 0x4eda1b79  // uzp1	v25.2d, v27.2d, v26.2d
  .long 0x4edc1bb8  // uzp1	v24.2d, v29.2d, v28.2d
  .long 0xad006159  // stp	q25, q24, [x10]
  .long 0x4ede1bf9  // uzp1	v25.2d, v31.2d, v30.2d
  .long 0x4ec01838  // uzp1	v24.2d, v1.2d, v0.2d
  .long 0xad016159  // stp	q25, q24, [x10, #32]
  .long 0x5e18077b  // mov	d27, v27.d[1]
  .long 0x5e18075a  // mov	d26, v26.d[1]
  .long 0x4eda1b7b  // uzp1	v27.2d, v27.2d, v26.2d
  .long 0x5e1807bd  // mov	d29, v29.d[1]
  .long 0x5e18079c  // mov	d28, v28.d[1]
  .long 0x4edc1bbd  // uzp1	v29.2d, v29.2d, v28.2d
  .long 0xad02755b  // stp	q27, q29, [x10, #64]
  .long 0x5e1807ff  // mov	d31, v31.d[1]
  .long 0x5e1807de  // mov	d30, v30.d[1]
  .long 0x4ede1bff  // uzp1	v31.2d, v31.2d, v30.2d
  .long 0x5e180421  // mov	d1, v1.d[1]
  .long 0x5e180400  // mov	d0, v0.d[1]
  .long 0x4ec01821  // uzp1	v1.2d, v1.2d, v0.2d
  .long 0xad03055f  // stp	q31, q1, [x10, #96]
  .long 0x6d433fee  // ldp	d14, d15, [sp, #48]
  .long 0x6cc827e8  // ldp	d8, d9, [sp], #128
  .long 0xd65f03c0  // ret
end;

procedure blake3_hash_many_neon(inputs: ppcuint8; num_inputs: csize_t;
                                blocks: csize_t; const key: pcuint32;
                                counter: cuint64; increment_counter: boolean32;
                                flags: cuint8; flags_start: cuint8;
                                flags_end: cuint8; out_: pcuint8);
begin
  while (num_inputs >= 4) do
  begin
    blake3_hash4_neon(inputs, blocks, key, counter, increment_counter, flags,
                      flags_start, flags_end, out_);
    if (increment_counter) then begin
      counter += 4;
    end;
    inputs += 4;
    num_inputs -= 4;
    out_ := @out_[4 * BLAKE3_OUT_LEN];
  end;

  while (num_inputs > 0) do
  begin
    hash_one_portable(inputs[0], blocks, key, counter, flags, flags_start,
                      flags_end, out_);
    if (increment_counter) then begin
      counter += 1;
    end;
    inputs += 1;
    num_inputs -= 1;
    out_ := @out_[BLAKE3_OUT_LEN];
  end;
end;
