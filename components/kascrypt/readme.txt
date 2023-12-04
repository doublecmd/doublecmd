KAScrypt Cryptographic Component Library

Copyright (C) 2011-2023 Alexander Koblov

KAScrypt library implements a modern cryptographic hash functions
with hardware acceleration using SIMD instructions under x86_64 platform:

  | Function | Acceleration       |
  | ---------| -------------------|
  | SHA224   | SSSE3, AVX2        |
  | SHA256   | SSSE3, AVX2        |
  | SHA384   | SSSE3              |
  | SHA512   | SSSE3              |
  | SHA3-224 |                    |
  | SHA3-256 |                    |
  | SHA3-384 |                    |
  | SHA3-512 |                    |
  | BLAKE2s  | SSE2, AVX          |
  | BLAKE2sp | SSE2, AVX          |
  | BLAKE2b  | SSE2, AVX          |
  | BLAKE2bp | SSE2, AVX          |
  | BLAKE3   | SSE2, SSE4.1, AVX2 |

Based on:
  DCPcrypt Cryptographic Component Library
  https://wiki.lazarus.freepascal.org/DCPcrypt

Original author:
  Copyright (C) 1999-2003 David Barton
  https://cityinthesky.co.uk

Contributors:
  Port to Lazarus by Barko - 2006
  Graeme Geldenhuys - 2009-2014
