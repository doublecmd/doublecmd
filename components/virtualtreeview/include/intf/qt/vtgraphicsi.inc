uses
  qt4, qtobjects;

{$ASMMODE INTEL}

procedure AlphaBlendLineConstant(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using a constant alpha value.
// The layout of a pixel must be BGRA where A is ignored (but is calculated as the other components).
// ConstantAlpha must be in the range 0..255 where 0 means totally transparent (destination pixel only)
// and 255 totally opaque (source pixel only).
// Bias is an additional value which gets added to every component and must be in the range -128..127

asm

{$ifdef CPU64}
//windows
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains ConstantAlpha
// Bias is on the stack

//non windows
// RDI contains Source
// RSI contains Destination
// EDX contains Count
// ECX contains ConstantAlpha
// R8D contains Bias

        //.NOFRAME

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        {$ifdef windows}
        MOVD        XMM3, R9D  // ConstantAlpha
        {$else}
        MOVD        XMM3, ECX  // ConstantAlpha
        {$endif}
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Load XMM5 with the bias value.
        {$ifdef windows}
        MOVD        XMM5, [Bias]
        {$else}
        MOVD        XMM5, R8D  //Bias
        {$endif}
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        {$ifdef windows}
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        {$else}
        MOVD        XMM1, DWORD PTR [RDI]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RSI]   // data is unaligned
        {$endif}
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}    // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RDX]}    // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM3    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW     XMM0, XMM4
        PADDSW    XMM0, XMM5
        PADDW     XMM0, XMM4
        PACKUSWB  XMM0, XMM0      // convert words to bytes with saturation
        {$ifdef windows}
        MOVD      DWORD PTR [RDX], XMM0     // store the result
        {$else}
        MOVD      DWORD PTR [RSI], XMM0     // store the result
        {$endif}
@3:
        {$ifdef windows}
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        {$else}
        ADD       RDI, 4
        ADD       RSI, 4
        DEC       EDX
        {$endif}
        JNZ       @1


{$else}
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// ConstantAlpha and Bias are on the stack


        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM6 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOV     EAX, [ConstantAlpha]
        DB      $0F, $6E, $F0          /// MOVD      MM6, EAX
        DB      $0F, $61, $F6          /// PUNPCKLWD MM6, MM6
        DB      $0F, $62, $F6          /// PUNPCKLDQ MM6, MM6

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C6          /// PMULLW    MM0, MM6,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
{$endif}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLinePerPixel(Source, Destination: Pointer; Count, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using the alpha value of the source pixels.
// The layout of a pixel must be BGRA.
// Bias is an additional value which gets added to every component and must be in the range -128..127

asm

{$ifdef CPU64}
//windows
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains Bias

//non windows
// RDI contains Source
// RSI contains Destination
// EDX contains Count
// ECX contains Bias

        //.NOFRAME

        // Load XMM5 with the bias value.
        {$ifdef windows}
        MOVD        XMM5, R9D   // Bias
        {$else}
        MOVD        XMM5, ECX   // Bias
        {$endif}
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        {$ifdef windows}
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        {$else}
        MOVD        XMM1, DWORD PTR [RDI]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RSI]   // data is unaligned
        {$endif}
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}    // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RDX]}    // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // Load XMM3 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        MOVQ        XMM3, XMM0
        PUNPCKHWD   XMM3, XMM3
        PUNPCKHDQ   XMM3, XMM3

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM3    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW       XMM0, XMM4
        PADDSW      XMM0, XMM5
        PADDW       XMM0, XMM4
        PACKUSWB    XMM0, XMM0    // convert words to bytes with saturation
        {$ifdef windows}
        MOVD        DWORD PTR [RDX], XMM0   // store the result
        {$else}
        MOVD        DWORD PTR [RSI], XMM0   // store the result
        {$endif}
@3:
        {$ifdef windows}
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        {$else}
        ADD         RDI, 4
        ADD         RSI, 4
        DEC         EDX
        {$endif}
        JNZ         @1


{$else}

// EAX contains Source
// EDX contains Destination
// ECX contains Count
// Bias is on the stack

        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // Load MM6 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        DB      $0F, $6F, $F0          /// MOVQ MM6, MM0
        DB      $0F, $69, $F6          /// PUNPCKHWD MM6, MM6
        DB      $0F, $6A, $F6          /// PUNPCKHDQ MM6, MM6

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C6          /// PMULLW    MM0, MM6,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
{$endif}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineMaster(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using the source pixel and a constant alpha value.
// The layout of a pixel must be BGRA.
// ConstantAlpha must be in the range 0..255.
// Bias is an additional value which gets added to every component and must be in the range -128..127

asm

{$ifdef CPU64}
//windows
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains ConstantAlpha
// Bias is on the stack

//non windows
// RDI contains Source
// RSI contains Destination
// EDX contains Count
// ECX contains ConstantAlpha
// R8D contains Bias

        //.SAVENV XMM6  //todo see how implement in fpc

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        {$ifdef windows}
        MOVD        XMM3, R9D    // ConstantAlpha
        {$else}
        MOVD        XMM3, ECX    // ConstantAlpha
        {$endif}
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Load XMM5 with the bias value.
        {$ifdef windows}
        MOV         R10D, [Bias]
        MOVD        XMM5, R10D
        {$else}
        MOVD        XMM5, R8D
        {$endif}
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        {$ifdef windows}
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        {$else}
        MOVD        XMM1, DWORD PTR [RDI]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RSI]   // data is unaligned
        {$endif}
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}     // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RCX]}     // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // Load XMM6 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        MOVQ        XMM6, XMM0
        PUNPCKHWD   XMM6, XMM6
        PUNPCKHDQ   XMM6, XMM6
        PMULLW      XMM6, XMM3    // source alpha * master alpha
        PSRLW       XMM6, 8       // divide by 256

        // calculation is: target = (alpha * master alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM6    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW       XMM0, XMM4
        PADDSW      XMM0, XMM5
        PADDW       XMM0, XMM4
        PACKUSWB    XMM0, XMM0    // convert words to bytes with saturation
        {$ifdef windows}
        MOVD        DWORD PTR [RDX], XMM0   // store the result
        {$else}
        MOVD        DWORD PTR [RSI], XMM0   // store the result
        {$endif}
@3:
        {$ifdef windows}
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        {$else}
        ADD         RDI, 4
        ADD         RSI, 4
        DEC         EDX
        {$endif}
        JNZ         @1

{$else}

// EAX contains Source
// EDX contains Destination
// ECX contains Count
// ConstantAlpha and Bias are on the stack


        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM6 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOV     EAX, [ConstantAlpha]
        DB      $0F, $6E, $F0          /// MOVD      MM6, EAX
        DB      $0F, $61, $F6          /// PUNPCKLWD MM6, MM6
        DB      $0F, $62, $F6          /// PUNPCKLDQ MM6, MM6

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // Load MM7 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        DB      $0F, $6F, $F8          /// MOVQ      MM7, MM0
        DB      $0F, $69, $FF          /// PUNPCKHWD MM7, MM7
        DB      $0F, $6A, $FF          /// PUNPCKHDQ MM7, MM7
        DB      $0F, $D5, $FE          /// PMULLW    MM7, MM6,   source alpha * master alpha
        DB      $0F, $71, $D7, $08     /// PSRLW     MM7, 8,     divide by 256

        // calculation is: target = (alpha * master alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C7          /// PMULLW    MM0, MM7,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
{$endif}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineMasterAndColor(Destination: Pointer; Count: Integer; ConstantAlpha, Color: Integer);

// Blends a line of Count pixels in Destination against the given color using a constant alpha value.
// The layout of a pixel must be BGRA and Color must be rrggbb00 (as stored by a COLORREF).
// ConstantAlpha must be in the range 0..255.

asm

{$ifdef CPU64}
//windows
// RCX contains Destination
// EDX contains Count
// R8D contains ConstantAlpha
// R9D contains Color

//non windows
// RDI contains Destination
// ESI contains Count
// EDX contains ConstantAlpha
// ECX contains Color

        //.NOFRAME

        // The used formula is: target = (alpha * color + (256 - alpha) * target) / 256.
        // alpha * color (factor 1) and 256 - alpha (factor 2) are constant values which can be calculated in advance.
        // The remaining calculation is therefore: target = (F1 + F2 * target) / 256

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size. (Every calculation here works on word sized operands.)
        {$ifdef windows}
        MOVD        XMM3, R8D   // ConstantAlpha
        {$else}
        MOVD        XMM3, EDX   // ConstantAlpha
        {$endif}
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Calculate factor 2.
        MOV         R10D, $100
        MOVD        XMM2, R10D
        PUNPCKLWD   XMM2, XMM2
        PUNPCKLDQ   XMM2, XMM2
        PSUBW       XMM2, XMM3             // XMM2 contains now: 255 - alpha = F2

        // Now calculate factor 1. Alpha is still in XMM3, but the r and b components of Color must be swapped.
        {$ifdef windows}
        BSWAP       R9D  // Color
        ROR         R9D, 8
        MOVD        XMM1, R9D              // Load the color and convert to word sized values.
        {$else}
        BSWAP       ECX  // Color
        ROR         ECX, 8
        MOVD        XMM1, ECX              // Load the color and convert to word sized values.
        {$endif}
        PXOR        XMM4, XMM4
        PUNPCKLBW   XMM1, XMM4
        PMULLW      XMM1, XMM3             // XMM1 contains now: color * alpha = F1

@1:     // The pixel loop calculates an entire pixel in one run.
        {$ifdef windows}
        MOVD        XMM0, DWORD PTR [RCX]
        {$else}
        MOVD        XMM0, DWORD PTR [RDI]
        {$endif}
        PUNPCKLBW   XMM0, XMM4

        PMULLW      XMM0, XMM2             // calculate F1 + F2 * target
        PADDW       XMM0, XMM1
        PSRLW       XMM0, 8                // divide by 256

        PACKUSWB    XMM0, XMM0             // convert words to bytes with saturation
        {$ifdef windows}
        MOVD        DWORD PTR [RCX], XMM0            // store the result

        ADD         RCX, 4
        DEC         EDX
        {$else}
        MOVD        DWORD PTR [RDI], XMM0            // store the result

        ADD         RDI, 4
        DEC         ESI
        {$endif}
        JNZ         @1


{$else}

// EAX contains Destination
// EDX contains Count
// ECX contains ConstantAlpha
// Color is passed on the stack


        // The used formula is: target = (alpha * color + (256 - alpha) * target) / 256.
        // alpha * color (factor 1) and 256 - alpha (factor 2) are constant values which can be calculated in advance.
        // The remaining calculation is therefore: target = (F1 + F2 * target) / 256

        // Load MM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size. (Every calculation here works on word sized operands.)
        DB      $0F, $6E, $D9          /// MOVD      MM3, ECX
        DB      $0F, $61, $DB          /// PUNPCKLWD MM3, MM3
        DB      $0F, $62, $DB          /// PUNPCKLDQ MM3, MM3

        // Calculate factor 2.
        MOV     ECX, $100
        DB      $0F, $6E, $D1          /// MOVD      MM2, ECX
        DB      $0F, $61, $D2          /// PUNPCKLWD MM2, MM2
        DB      $0F, $62, $D2          /// PUNPCKLDQ MM2, MM2
        DB      $0F, $F9, $D3          /// PSUBW     MM2, MM3             // MM2 contains now: 255 - alpha = F2

        // Now calculate factor 1. Alpha is still in MM3, but the r and b components of Color must be swapped.
        MOV     ECX, [Color]
        BSWAP   ECX
        ROR     ECX, 8
        DB      $0F, $6E, $C9          /// MOVD      MM1, ECX             // Load the color and convert to word sized values.
        DB      $0F, $EF, $E4          /// PXOR      MM4, MM4
        DB      $0F, $60, $CC          /// PUNPCKLBW MM1, MM4
        DB      $0F, $D5, $CB          /// PMULLW    MM1, MM3             // MM1 contains now: color * alpha = F1

@1:     // The pixel loop calculates an entire pixel in one run.
        DB      $0F, $6E, $00          /// MOVD      MM0, [EAX]
        DB      $0F, $60, $C4          /// PUNPCKLBW MM0, MM4

        DB      $0F, $D5, $C2          /// PMULLW    MM0, MM2             // calculate F1 + F2 * target
        DB      $0F, $FD, $C1          /// PADDW     MM0, MM1
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8               // divide by 256

        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0             // convert words to bytes with saturation
        DB      $0F, $7E, $00          /// MOVD      [EAX], MM0           // store the result

        ADD     EAX, 4
        DEC     EDX
        JNZ     @1
{$endif}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure EMMS;

// Reset MMX state to use the FPU for other tasks again.

{$ifdef CPU64}
  inline;
  begin
  end;

{$else}

asm
        DB      $0F, $77               /// EMMS
end;
{$endif}

//----------------------------------------------------------------------------------------------------------------------

function GetBitmapBitsFromDeviceContext(DC: HDC; out Width, Height: Integer): Pointer;

// Helper function used to retrieve the bitmap selected into the given device context. If there is a bitmap then
// the function will return a pointer to its bits otherwise nil is returned.
// Additionally the dimensions of the bitmap are returned.

var
  Bitmap: HBITMAP;
  DIB: TDIBSection;

begin
  Result := nil;
  Width := 0;
  Height := 0;
  Bitmap := GetCurrentObject(DC, OBJ_BITMAP);
  if Bitmap <> 0 then
  begin
    if GetObject(Bitmap, SizeOf(DIB), @DIB) = SizeOf(DIB) then
    begin
      Assert(DIB.dsBm.bmPlanes * DIB.dsBm.bmBitsPixel = 32, 'Alpha blending error: bitmap must use 32 bpp.');
      Result := DIB.dsBm.bmBits;
      Width := DIB.dsBmih.biWidth;
      Height := DIB.dsBmih.biHeight;
    end;
  end;
  Assert(Result <> nil, 'Alpha blending DC error: no bitmap available.');
end;

//----------------------------------------------------------------------------------------------------------------------

function GetBitmapBitsFromBitmap(Bitmap: HBITMAP): Pointer;
var
  DIB: TDIBSection;
begin
  Result := nil;
  if Bitmap <> 0 then
  begin
    if GetObject(Bitmap, SizeOf(DIB), @DIB) = SizeOf(DIB) then
    begin
      Assert(DIB.dsBm.bmPlanes * DIB.dsBm.bmBitsPixel = 32, 'Alpha blending error: bitmap must use 32 bpp.');
      Result := DIB.dsBm.bmBits;
    end;
  end;
end;

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;

// Helper function to calculate the start address for the given row.

begin
  //todo: Height is always > 0 in LCL
  {
  if Height > 0 then  // bottom-up DIB
    Row := Height - Row - 1;
  }
  // Return DWORD aligned address of the requested scanline.
  Result := Bits + Row * ((Width * 32 + 31) and not 31) div 8;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);

// Optimized alpha blend procedure using MMX instructions to perform as quick as possible.
// For this procedure to work properly it is important that both source and target bitmap use the 32 bit color format.
// R describes the source rectangle to work on.
// Target is the place (upper left corner) in the target bitmap where to blend to. Note that source width + X offset
// must be less or equal to the target width. Similar for the height.
// If Mode is bmConstantAlpha then the blend operation uses the given ConstantAlpha value for all pixels.
// If Mode is bmPerPixelAlpha then each pixel is blended using its individual alpha value (the alpha value of the source).
// If Mode is bmMasterAlpha then each pixel is blended using its individual alpha value multiplied by ConstantAlpha.
// If Mode is bmConstantAlphaAndColor then each destination pixel is blended using ConstantAlpha but also a constant
// color which will be obtained from Bias. In this case no offset value is added, otherwise Bias is used as offset.
// Blending of a color into target only (bmConstantAlphaAndColor) ignores Source (the DC) and Target (the position).
// CAUTION: This procedure does not check whether MMX instructions are actually available! Call it only if MMX is really
//          usable.

var
  Y: Integer;
  SourceRun,
  TargetRun: PByte;

  SourceBits,
  DestBits: Pointer;
  SourceWidth,
  SourceHeight,
  DestWidth,
  DestHeight: Integer;

  //BlendColor: TQColor;
begin
  if not IsRectEmpty(R) then
  begin
    {$ifdef CPU64}
    //avoid MasterAlpha due to incomplete AlphaBlendLineMaster. See comment in procedure
    if Mode = bmMasterAlpha then
      Mode := bmConstantAlpha;
    {$endif}
    // Note: it is tempting to optimize the special cases for constant alpha 0 and 255 by just ignoring soure
    //       (alpha = 0) or simply do a blit (alpha = 255). But this does not take the bias into account.
    case Mode of
      bmConstantAlpha:
        begin
          // Get a pointer to the bitmap bits for the source and target device contexts.
          // Note: this supposes that both contexts do actually have bitmaps assigned!
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * R.Left);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              Inc(TargetRun, 4 * Target.X);
              AlphaBlendLineConstant(SourceRun, TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
      bmPerPixelAlpha:
        begin
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * R.Left);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              Inc(TargetRun, 4 * Target.X);
              AlphaBlendLinePerPixel(SourceRun, TargetRun, R.Right - R.Left, Bias);
            end;
          end;
          EMMS;
        end;
      bmMasterAlpha:
        begin
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * Target.X);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              AlphaBlendLineMaster(SourceRun, TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
      bmConstantAlphaAndColor:
        begin
          //todo: see why is not working
          {
          QColor_fromRgb(@BlendColor,
            Bias and $000000FF,
            (Bias shr 8) and $000000FF,
            (Bias shr 16) and $000000FF,
            ConstantAlpha);
          QPainter_fillRect(TQTDeviceContext(Destination).Widget,
            R.Left + Target.x, R.Top + Target.y,
            R.Right - R.Left, R.Bottom - R.Top, @BlendColor);
          }
          // Source is ignored since there is a constant color value.

          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + R.Top);
              Inc(TargetRun, 4 * R.Left);
              AlphaBlendLineMasterAndColor(TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
    end;
  end;
end;



