const
	EUCKRCharLenTable: array [0..3] of byte = (0, 1, 2, 0);

  //PCK4BITS(0,1,1,1,1,1,1,1,  // 00 - 07

  EUCKR_cls: array [0..255] of byte = (
  1,1,1,1,1,1,1,1,  // 00 - 07
  1,1,1,1,1,1,0,0,  // 08 - 0f
  1,1,1,1,1,1,1,1,  // 10 - 17
  1,1,1,0,1,1,1,1,  // 18 - 1f
  1,1,1,1,1,1,1,1,  // 20 - 27
  1,1,1,1,1,1,1,1,  // 28 - 2f
  1,1,1,1,1,1,1,1,  // 30 - 37
  1,1,1,1,1,1,1,1,  // 38 - 3f
  1,1,1,1,1,1,1,1,  // 40 - 47
  1,1,1,1,1,1,1,1,  // 48 - 4f
  1,1,1,1,1,1,1,1,  // 50 - 57 
  1,1,1,1,1,1,1,1,  // 58 - 5f
  1,1,1,1,1,1,1,1,  // 60 - 67 
  1,1,1,1,1,1,1,1,  // 68 - 6f
  1,1,1,1,1,1,1,1,  // 70 - 77
  1,1,1,1,1,1,1,1,  // 78 - 7f
  0,0,0,0,0,0,0,0,  // 80 - 87
  0,0,0,0,0,0,0,0,  // 88 - 8f 
  0,0,0,0,0,0,0,0,  // 90 - 97 
  0,0,0,0,0,0,0,0,  // 98 - 9f 
  0,2,2,2,2,2,2,2,  // a0 - a7
  2,2,2,2,2,3,3,3,  // a8 - af 
  2,2,2,2,2,2,2,2,  // b0 - b7
  2,2,2,2,2,2,2,2,  // b8 - bf
  2,2,2,2,2,2,2,2,  // c0 - c7
  2,3,2,2,2,2,2,2,  // c8 - cf 
  2,2,2,2,2,2,2,2,  // d0 - d7
  2,2,2,2,2,2,2,2,  // d8 - df
  2,2,2,2,2,2,2,2,  // e0 - e7
  2,2,2,2,2,2,2,2,  // e8 - ef
  2,2,2,2,2,2,2,2,  // f0 - f7
  2,2,2,2,2,2,2,0   // f8 - ff
  );

  EUCKR_st: array [0..15] of byte = (
  byte(eError),byte(eStart),     3,byte(eError),byte(eError),byte(eError),byte(eError),byte(eError), //00-07
  byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eError),byte(eError),byte(eStart),byte(eStart) //08-0f
  );

  EUCKRLangModel: SMModel =(
    classTable:   @EUCKR_cls;
    classFactor:  4;
    stateTable:   @EUCKR_st;
    charLenTable: @EUCKRCharLenTable;
    CharsetID:    EUC_KR_CHARSET;
  );
//  EUCKRLangModel: SMModel =(
//   classTable: (
//   idxsft:  eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk:  eUnitMsk4bits;
//    data:  @EUCKR_cls;
//   );
//  classFactor:  4;
//  stateTable: (
//   idxsft:  eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk:  eUnitMsk4bits;
//    data:  @EUCKR_st;
//  );
//  charLenTable:  @EUCKRCharLenTable;
//  CharsetID:  EUC_KR_CHARSET;
//  );
