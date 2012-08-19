const
	EUCTWCharLenTable: array [0..6] of Byte = (0, 0, 1, 2, 2, 2, 3);

  //PCK4BITS(0,2,2,2,2,2,2,2,  // 00 - 07
  EUCTW_cls: array [0..255] of Byte = (
  2,2,2,2,2,2,2,2,  // 00 - 07
  2,2,2,2,2,2,0,0,  // 08 - 0f
  2,2,2,2,2,2,2,2,  // 10 - 17
  2,2,2,0,2,2,2,2,  // 18 - 1f
  2,2,2,2,2,2,2,2,  // 20 - 27
  2,2,2,2,2,2,2,2,  // 28 - 2f
  2,2,2,2,2,2,2,2,  // 30 - 37
  2,2,2,2,2,2,2,2,  // 38 - 3f
  2,2,2,2,2,2,2,2,  // 40 - 47
  2,2,2,2,2,2,2,2,  // 48 - 4f
  2,2,2,2,2,2,2,2,  // 50 - 57
  2,2,2,2,2,2,2,2,  // 58 - 5f
  2,2,2,2,2,2,2,2,  // 60 - 67
  2,2,2,2,2,2,2,2,  // 68 - 6f
  2,2,2,2,2,2,2,2,  // 70 - 77
  2,2,2,2,2,2,2,2,  // 78 - 7f
  0,0,0,0,0,0,0,0,  // 80 - 87
  0,0,0,0,0,0,6,0,  // 88 - 8f
  0,0,0,0,0,0,0,0,  // 90 - 97
  0,0,0,0,0,0,0,0,  // 98 - 9f
  0,3,4,4,4,4,4,4,  // a0 - a7
  5,5,1,1,1,1,1,1,  // a8 - af
  1,1,1,1,1,1,1,1,  // b0 - b7
  1,1,1,1,1,1,1,1,  // b8 - bf
  1,1,3,1,3,3,3,3,  // c0 - c7
  3,3,3,3,3,3,3,3,  // c8 - cf
  3,3,3,3,3,3,3,3,  // d0 - d7
  3,3,3,3,3,3,3,3,  // d8 - df
  3,3,3,3,3,3,3,3,  // e0 - e7
  3,3,3,3,3,3,3,3,  // e8 - ef
  3,3,3,3,3,3,3,3,  // f0 - f7
  3,3,3,3,3,3,3,0    // f8 - ff
  );

  EUCTW_st: array [0..47] of Byte = (
  Byte(eError),Byte(eError),Byte(eStart),     3,     3,     3,     4,Byte(eError),//00-07
  Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eItsMe),//08-0f
  Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eError),Byte(eStart),Byte(eError),//10-17
  Byte(eStart),Byte(eStart),Byte(eStart),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),//18-1f
       5,Byte(eError),Byte(eError),Byte(eError),Byte(eStart),Byte(eError),Byte(eStart),Byte(eStart),//20-27
  Byte(eStart),Byte(eError),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart) //28-2f
  );

  EUCTWLangModel: SMModel = (
    classTable:   @EUCTW_cls;
    classFactor:  7;
    stateTable:   @EUCTW_st;
    charLenTable: @EUCTWCharLenTable;
    CharsetID:    X_EUC_TW_CHARSET;
  );

//  EUCTWLangModel: SMModel = (
//   classTable: (
//    idxsft:   eIdxSft4bits;
//    sftmsk:   eSftMsk4bits;
//    bitsft:   eBitSft4bits;
//    unitmsk:   eUnitMsk4bits;
//    data:   @EUCTW_cls;
//   );
//  classFactor:   7;
//  stateTable: (
//    idxsft:   eIdxSft4bits;
//    sftmsk:   eSftMsk4bits;
//    bitsft:   eBitSft4bits;
//    unitmsk:   eUnitMsk4bits;
//    data:   @EUCTW_st;
//  );
//  charLenTable:   @EUCTWCharLenTable;
//  CharsetID:   X_EUC_TW_CHARSET;
//  );
