{
   Fast SHA256 using ARMv8-A + Cryptographic Extension

   Copyright (C) 2025 Alexander Koblov (alexx2000@mail.ru)

   Based on sha256_arm_shani.cpp
   https://github.com/bitcoin/bitcoin
   Copyright (C) 2022 The Bitcoin Core developers

   Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php.
}

procedure sha256_compress_arm(CurrentHash: PLongWord; HashBuffer: PByte; BufferCount: UIntPtr); assembler; nostackframe;
asm
  .long 0xad401003  // ldp	q3, q4, [x0]
  .long 0xd1000443  // sub	x3, x2, #0x1
  .long 0x4ea31c61  // mov	v1.16b, v3.16b
  .long 0x4ea41c80  // mov	v0.16b, v4.16b
  .long 0xb4000e62  // cbz	x2, 1dc <sha256_compress_arm+0x1dc>
  .long 0x10000ee2  // adr	x2, 1f0 <sha256_compress_arm+0x1f0>
  .long 0xad401426  // ldp	q6, q5, [x1]
  .long 0xd1000463  // sub	x3, x3, #0x1
  .long 0xad404041  // ldp	q1, q16, [x2]
  .long 0xb100047f  // cmn	x3, #0x1
  .long 0x6e2008c6  // rev32	v6.16b, v6.16b
  .long 0x91010021  // add	x1, x1, #0x40
  .long 0x4ea31c72  // mov	v18.16b, v3.16b
  .long 0x4ea41c80  // mov	v0.16b, v4.16b
  .long 0x6e2008a5  // rev32	v5.16b, v5.16b
  .long 0x4ea68421  // add	v1.4s, v1.4s, v6.4s
  .long 0x3cde0022  // ldur	q2, [x1, #-32]
  .long 0x4ea58610  // add	v16.4s, v16.4s, v5.4s
  .long 0x5e014092  // sha256h	q18, q4, v1.4s
  .long 0x5e015060  // sha256h2	q0, q3, v1.4s
  .long 0x6e200842  // rev32	v2.16b, v2.16b
  .long 0x3dc00847  // ldr	q7, [x2, #32]
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x3cdf0021  // ldur	q1, [x1, #-16]
  .long 0x4ea284e7  // add	v7.4s, v7.4s, v2.4s
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x6e200821  // rev32	v1.16b, v1.16b
  .long 0x5e2828a6  // sha256su0	v6.4s, v5.4s
  .long 0x5e282845  // sha256su0	v5.4s, v2.4s
  .long 0x4eb11e32  // mov	v18.16b, v17.16b
  .long 0x5e016046  // sha256su1	v6.4s, v2.4s, v1.4s
  .long 0x5e282822  // sha256su0	v2.4s, v1.4s
  .long 0x5e074012  // sha256h	q18, q0, v7.4s
  .long 0x5e075220  // sha256h2	q0, q17, v7.4s
  .long 0xad419c50  // ldp	q16, q7, [x2, #48]
  .long 0x5e066025  // sha256su1	v5.4s, v1.4s, v6.4s
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x4ea18610  // add	v16.4s, v16.4s, v1.4s
  .long 0x4ea684e7  // add	v7.4s, v7.4s, v6.4s
  .long 0x5e2828c1  // sha256su0	v1.4s, v6.4s
  .long 0x5e0560c2  // sha256su1	v2.4s, v6.4s, v5.4s
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x5e2828a6  // sha256su0	v6.4s, v5.4s
  .long 0x5e0260a1  // sha256su1	v1.4s, v5.4s, v2.4s
  .long 0x4eb11e32  // mov	v18.16b, v17.16b
  .long 0x5e074012  // sha256h	q18, q0, v7.4s
  .long 0x5e075220  // sha256h2	q0, q17, v7.4s
  .long 0xad429c50  // ldp	q16, q7, [x2, #80]
  .long 0x5e016046  // sha256su1	v6.4s, v2.4s, v1.4s
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x4ea58610  // add	v16.4s, v16.4s, v5.4s
  .long 0x4ea284e7  // add	v7.4s, v7.4s, v2.4s
  .long 0x5e282845  // sha256su0	v5.4s, v2.4s
  .long 0x5e282822  // sha256su0	v2.4s, v1.4s
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x5e066025  // sha256su1	v5.4s, v1.4s, v6.4s
  .long 0x4eb11e32  // mov	v18.16b, v17.16b
  .long 0x5e0560c2  // sha256su1	v2.4s, v6.4s, v5.4s
  .long 0x5e074012  // sha256h	q18, q0, v7.4s
  .long 0x5e075220  // sha256h2	q0, q17, v7.4s
  .long 0xad439c50  // ldp	q16, q7, [x2, #112]
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x4ea18610  // add	v16.4s, v16.4s, v1.4s
  .long 0x4ea684e7  // add	v7.4s, v7.4s, v6.4s
  .long 0x5e2828c1  // sha256su0	v1.4s, v6.4s
  .long 0x5e2828a6  // sha256su0	v6.4s, v5.4s
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x5e0260a1  // sha256su1	v1.4s, v5.4s, v2.4s
  .long 0x4eb11e32  // mov	v18.16b, v17.16b
  .long 0x5e016046  // sha256su1	v6.4s, v2.4s, v1.4s
  .long 0x5e074012  // sha256h	q18, q0, v7.4s
  .long 0x5e075220  // sha256h2	q0, q17, v7.4s
  .long 0xad449c50  // ldp	q16, q7, [x2, #144]
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x4ea58610  // add	v16.4s, v16.4s, v5.4s
  .long 0x4ea284e7  // add	v7.4s, v7.4s, v2.4s
  .long 0x5e282845  // sha256su0	v5.4s, v2.4s
  .long 0x5e282822  // sha256su0	v2.4s, v1.4s
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x5e066025  // sha256su1	v5.4s, v1.4s, v6.4s
  .long 0x4eb11e32  // mov	v18.16b, v17.16b
  .long 0x5e0560c2  // sha256su1	v2.4s, v6.4s, v5.4s
  .long 0x5e074012  // sha256h	q18, q0, v7.4s
  .long 0x5e075220  // sha256h2	q0, q17, v7.4s
  .long 0xad459c50  // ldp	q16, q7, [x2, #176]
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x4ea18610  // add	v16.4s, v16.4s, v1.4s
  .long 0x4ea684e7  // add	v7.4s, v7.4s, v6.4s
  .long 0x5e2828c1  // sha256su0	v1.4s, v6.4s
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x5e0260a1  // sha256su1	v1.4s, v5.4s, v2.4s
  .long 0x4eb11e32  // mov	v18.16b, v17.16b
  .long 0x5e074012  // sha256h	q18, q0, v7.4s
  .long 0x5e075220  // sha256h2	q0, q17, v7.4s
  .long 0xad469c50  // ldp	q16, q7, [x2, #208]
  .long 0x4eb21e51  // mov	v17.16b, v18.16b
  .long 0x4ea58610  // add	v16.4s, v16.4s, v5.4s
  .long 0x4ea284e2  // add	v2.4s, v7.4s, v2.4s
  .long 0x3dc03c45  // ldr	q5, [x2, #240]
  .long 0x5e104011  // sha256h	q17, q0, v16.4s
  .long 0x5e105240  // sha256h2	q0, q18, v16.4s
  .long 0x4ea184a1  // add	v1.4s, v5.4s, v1.4s
  .long 0x4eb11e26  // mov	v6.16b, v17.16b
  .long 0x5e024006  // sha256h	q6, q0, v2.4s
  .long 0x5e025220  // sha256h2	q0, q17, v2.4s
  .long 0x4ea61cc2  // mov	v2.16b, v6.16b
  .long 0x5e014002  // sha256h	q2, q0, v1.4s
  .long 0x5e0150c0  // sha256h2	q0, q6, v1.4s
  .long 0x4ea28463  // add	v3.4s, v3.4s, v2.4s
  .long 0x4ea08484  // add	v4.4s, v4.4s, v0.4s
  .long 0x54fff241  // b.ne	18 <sha256_compress_arm+0x18>  // b.any
  .long 0x4ea41c80  // mov	v0.16b, v4.16b
  .long 0x4ea31c61  // mov	v1.16b, v3.16b
  .long 0xad000001  // stp	q1, q0, [x0]
  .long 0xd65f03c0  // ret
  .long 0xd503201f  // nop
  .long 0xd503201f  // nop
  .long 0xd503201f  // nop
  .long 0x428a2f98  // .word	0x428a2f98
  .long 0x71374491  // .word	0x71374491
  .long 0xb5c0fbcf  // .word	0xb5c0fbcf
  .long 0xe9b5dba5  // .word	0xe9b5dba5
  .long 0x3956c25b  // .word	0x3956c25b
  .long 0x59f111f1  // .word	0x59f111f1
  .long 0x923f82a4  // .word	0x923f82a4
  .long 0xab1c5ed5  // .word	0xab1c5ed5
  .long 0xd807aa98  // .word	0xd807aa98
  .long 0x12835b01  // .word	0x12835b01
  .long 0x243185be  // .word	0x243185be
  .long 0x550c7dc3  // .word	0x550c7dc3
  .long 0x72be5d74  // .word	0x72be5d74
  .long 0x80deb1fe  // .word	0x80deb1fe
  .long 0x9bdc06a7  // .word	0x9bdc06a7
  .long 0xc19bf174  // .word	0xc19bf174
  .long 0xe49b69c1  // .word	0xe49b69c1
  .long 0xefbe4786  // .word	0xefbe4786
  .long 0x0fc19dc6  // .word	0x0fc19dc6
  .long 0x240ca1cc  // .word	0x240ca1cc
  .long 0x2de92c6f  // .word	0x2de92c6f
  .long 0x4a7484aa  // .word	0x4a7484aa
  .long 0x5cb0a9dc  // .word	0x5cb0a9dc
  .long 0x76f988da  // .word	0x76f988da
  .long 0x983e5152  // .word	0x983e5152
  .long 0xa831c66d  // .word	0xa831c66d
  .long 0xb00327c8  // .word	0xb00327c8
  .long 0xbf597fc7  // .word	0xbf597fc7
  .long 0xc6e00bf3  // .word	0xc6e00bf3
  .long 0xd5a79147  // .word	0xd5a79147
  .long 0x06ca6351  // .word	0x06ca6351
  .long 0x14292967  // .word	0x14292967
  .long 0x27b70a85  // .word	0x27b70a85
  .long 0x2e1b2138  // .word	0x2e1b2138
  .long 0x4d2c6dfc  // .word	0x4d2c6dfc
  .long 0x53380d13  // .word	0x53380d13
  .long 0x650a7354  // .word	0x650a7354
  .long 0x766a0abb  // .word	0x766a0abb
  .long 0x81c2c92e  // .word	0x81c2c92e
  .long 0x92722c85  // .word	0x92722c85
  .long 0xa2bfe8a1  // .word	0xa2bfe8a1
  .long 0xa81a664b  // .word	0xa81a664b
  .long 0xc24b8b70  // .word	0xc24b8b70
  .long 0xc76c51a3  // .word	0xc76c51a3
  .long 0xd192e819  // .word	0xd192e819
  .long 0xd6990624  // .word	0xd6990624
  .long 0xf40e3585  // .word	0xf40e3585
  .long 0x106aa070  // .word	0x106aa070
  .long 0x19a4c116  // .word	0x19a4c116
  .long 0x1e376c08  // .word	0x1e376c08
  .long 0x2748774c  // .word	0x2748774c
  .long 0x34b0bcb5  // .word	0x34b0bcb5
  .long 0x391c0cb3  // .word	0x391c0cb3
  .long 0x4ed8aa4a  // .word	0x4ed8aa4a
  .long 0x5b9cca4f  // .word	0x5b9cca4f
  .long 0x682e6ff3  // .word	0x682e6ff3
  .long 0x748f82ee  // .word	0x748f82ee
  .long 0x78a5636f  // .word	0x78a5636f
  .long 0x84c87814  // .word	0x84c87814
  .long 0x8cc70208  // .word	0x8cc70208
  .long 0x90befffa  // .word	0x90befffa
  .long 0xa4506ceb  // .word	0xa4506ceb
  .long 0xbef9a3f7  // .word	0xbef9a3f7
  .long 0xc67178f2  // .word	0xc67178f2
end;
