const
	EUCJPCharLenTable: array [0..5] of byte = (2, 2, 2, 3, 1, 0);

  //PCK4BITS(5,4,4,4,4,4,4,4),  // 00 - 07
  EUCJP_cls: array [0..255] of byte = (
  4,4,4,4,4,4,4,4,  // 00 - 07
  4,4,4,4,4,4,5,5,  // 08 - 0f
  4,4,4,4,4,4,4,4,  // 10 - 17
  4,4,4,5,4,4,4,4,  // 18 - 1f
  4,4,4,4,4,4,4,4,  // 20 - 27
  4,4,4,4,4,4,4,4,  // 28 - 2f
  4,4,4,4,4,4,4,4,  // 30 - 37
  4,4,4,4,4,4,4,4,  // 38 - 3f
  4,4,4,4,4,4,4,4,  // 40 - 47
  4,4,4,4,4,4,4,4,  // 48 - 4f
  4,4,4,4,4,4,4,4,  // 50 - 57
  4,4,4,4,4,4,4,4,  // 58 - 5f
  4,4,4,4,4,4,4,4,  // 60 - 67
  4,4,4,4,4,4,4,4,  // 68 - 6f
  4,4,4,4,4,4,4,4,  // 70 - 77
  4,4,4,4,4,4,4,4,  // 78 - 7f 
  5,5,5,5,5,5,5,5,  // 80 - 87
  5,5,5,5,5,5,1,3,  // 88 - 8f
  5,5,5,5,5,5,5,5,  // 90 - 97 
  5,5,5,5,5,5,5,5,  // 98 - 9f
  5,2,2,2,2,2,2,2,  // a0 - a7
  2,2,2,2,2,2,2,2,  // a8 - af 
  2,2,2,2,2,2,2,2,  // b0 - b7
  2,2,2,2,2,2,2,2,  // b8 - bf 
  2,2,2,2,2,2,2,2,  // c0 - c7 
  2,2,2,2,2,2,2,2,  // c8 - cf
  2,2,2,2,2,2,2,2,  // d0 - d7
  2,2,2,2,2,2,2,2,  // d8 - df
  0,0,0,0,0,0,0,0,  // e0 - e7
  0,0,0,0,0,0,0,0,  // e8 - ef
  0,0,0,0,0,0,0,0,  // f0 - f7
  0,0,0,0,0,0,0,5   // f8 - ff
  );

  EUCJP_st: array [0..39] of byte = (
       3,     4,     3,     5,byte(eStart),byte(eError),byte(eError),byte(eError),//00-07
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eItsMe),//08-0f
  byte(eItsMe),byte(eItsMe),byte(eStart),byte(eError),byte(eStart),byte(eError),byte(eError),byte(eError),//10-17
  byte(eError),byte(eError),byte(eStart),byte(eError),byte(eError),byte(eError),     3,byte(eError),//18-1f
       3,byte(eError),byte(eError),byte(eError),byte(eStart),byte(eStart),byte(eStart),byte(eStart) //20-27
  );

  EUCJPLangModel: SMModel = (
    classTable:   @EUCJP_cls;
    classFactor:  6;
    stateTable:   @EUCJP_st;
    charLenTable: @EUCJPCharLenTable;
    CharsetID:    EUC_JP_CHARSET;
  );
//  EUCJPLangModel: SMModel = (
//   classTable: (
//    idxsft:   eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk:  eUnitMsk4bits;
//    data:  @EUCJP_cls;
//   );
//   classFactor:  6;
//   stateTable: (
//    idxsft:  eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk:  eUnitMsk4bits;
//    data:  @EUCJP_st;
//   );
//   charLenTable:  @EUCJPCharLenTable;
//   CharsetID:  EUC_JP_CHARSET;
//  );
