const


  SJIS_cls: array [0..255] of byte = (
    1,1,1,1,1,1,1,1,  // 00 - 07
    1,1,1,1,1,1,0,0,  // 08 - 0f
    1,1,1,1,1,1,1,1,  // 10 - 17
    1,1,1,0,1,1,1,1,  // 18 - 1f
    1,1,1,1,1,1,1,1,  // 20 - 27
    1,1,1,1,1,1,1,1,  // 28 - 2f
    1,1,1,1,1,1,1,1,  // 30 - 37
    1,1,1,1,1,1,1,1,  // 38 - 3f
    2,2,2,2,2,2,2,2,  // 40 - 47
    2,2,2,2,2,2,2,2,  // 48 - 4f
    2,2,2,2,2,2,2,2,  // 50 - 57
    2,2,2,2,2,2,2,2,  // 58 - 5f
    2,2,2,2,2,2,2,2,  // 60 - 67
    2,2,2,2,2,2,2,2,  // 68 - 6f
    2,2,2,2,2,2,2,2,  // 70 - 77
    2,2,2,2,2,2,2,1,  // 78 - 7f
    3,3,3,3,3,3,3,3,  // 80 - 87
    3,3,3,3,3,3,3,3,  // 88 - 8f
    3,3,3,3,3,3,3,3,  // 90 - 97
    3,3,3,3,3,3,3,3,  // 98 - 9f
     //0xa0 is illegal in sjis encoding, but some pages does
     //contain such byte. We need to be more error forgiven.
    2,2,2,2,2,2,2,2,  // a0 - a7
    2,2,2,2,2,2,2,2,  // a8 - af
    2,2,2,2,2,2,2,2,  // b0 - b7
    2,2,2,2,2,2,2,2,  // b8 - bf
    2,2,2,2,2,2,2,2,  // c0 - c7
    2,2,2,2,2,2,2,2,  // c8 - cf
    2,2,2,2,2,2,2,2,  // d0 - d7
    2,2,2,2,2,2,2,2,  // d8 - df
    3,3,3,3,3,3,3,3,  // e0 - e7
    3,3,3,3,3,4,4,4,  // e8 - ef
    4,4,4,4,4,4,4,4,  // f0 - f7
    4,4,4,4,4,0,0,0   // f8 - ff
  );

	SJIS_st: array [0..23] of byte = (
    byte(eError),byte(eStart),byte(eStart),     3,byte(eError),byte(eError),byte(eError),byte(eError),//00-07
	  byte(eError),byte(eError),byte(eError),byte(eError),byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eItsMe),//08-0f
	  byte(eItsMe),byte(eItsMe),byte(eError),byte(eError),byte(eStart),byte(eStart),byte(eStart),byte(eStart) //10-17
   );

  SJISCharLenTable: array [0..5] of byte = (0, 1, 1, 2, 0, 0);

	SJISLangModel: SMModel = (
   classTable:   @SJIS_cls;
   classFactor:  6;
   stateTable:   @SJIS_st;
   charLenTable: @SJISCharLenTable;
   CharsetID:    SHIFT_JIS_CHARSET;
  );
//	SJISLangModel: SMModel = (
//   classTable: (
//    idxsft:  eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk: eUnitMsk4bits;
//    data: @SJIS_cls;
//   );
//  classFactor: 6;
//    stateTable: (
//      idxsft:  eIdxSft4bits;
//      sftmsk:  eSftMsk4bits;
//      bitsft:  eBitSft4bits;
//      unitmsk: eUnitMsk4bits;
//      data: @SJIS_st;
//    );
//  charLenTable: @SJISCharLenTable;
//  CharsetID: SHIFT_JIS_CHARSET;
//  );

