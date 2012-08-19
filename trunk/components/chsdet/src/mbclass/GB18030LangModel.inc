const
	GB18030CharLenTable: array [0..6] of byte = (0, 1, 1, 1, 1, 1, 2);


  GB18030_cls: array [0..255] of Byte = (
  1,1,1,1,1,1,1,1,  // 00 - 07
  1,1,1,1,1,1,0,0,  // 08 - 0f
  1,1,1,1,1,1,1,1,  // 10 - 17
  1,1,1,0,1,1,1,1,  // 18 - 1f
  1,1,1,1,1,1,1,1,  // 20 - 27
  1,1,1,1,1,1,1,1,  // 28 - 2f
  3,3,3,3,3,3,3,3,  // 30 - 37
  3,3,1,1,1,1,1,1,  // 38 - 3f
  2,2,2,2,2,2,2,2,  // 40 - 47
  2,2,2,2,2,2,2,2,  // 48 - 4f
  2,2,2,2,2,2,2,2,  // 50 - 57
  2,2,2,2,2,2,2,2,  // 58 - 5f
  2,2,2,2,2,2,2,2,  // 60 - 67
  2,2,2,2,2,2,2,2,  // 68 - 6f
  2,2,2,2,2,2,2,2,  // 70 - 77 
  2,2,2,2,2,2,2,4,  // 78 - 7f
  5,6,6,6,6,6,6,6,  // 80 - 87
  6,6,6,6,6,6,6,6,  // 88 - 8f
  6,6,6,6,6,6,6,6,  // 90 - 97
  6,6,6,6,6,6,6,6,  // 98 - 9f
  6,6,6,6,6,6,6,6,  // a0 - a7
  6,6,6,6,6,6,6,6,  // a8 - af
  6,6,6,6,6,6,6,6,  // b0 - b7
  6,6,6,6,6,6,6,6,  // b8 - bf
  6,6,6,6,6,6,6,6,  // c0 - c7
  6,6,6,6,6,6,6,6,  // c8 - cf
  6,6,6,6,6,6,6,6,  // d0 - d7
  6,6,6,6,6,6,6,6,  // d8 - df
  6,6,6,6,6,6,6,6,  // e0 - e7
  6,6,6,6,6,6,6,6,  // e8 - ef
  6,6,6,6,6,6,6,6,  // f0 - f7
  6,6,6,6,6,6,6,0   // f8 - ff
  );

  GB18030_st: array [0..47] of byte = (
  byte(eError),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),     3,byte(eError),//00-07
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eItsMe),byte(eItsMe),//08-0f
  byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eItsMe),byte(eError),byte(eError),byte(eStart),//10-17
       4,byte(eError),byte(eStart),byte(eStart),byte(eError),byte(eError),byte(eError),byte(eError),//18-1f
  byte(eError),byte(eError),     5,byte(eError),byte(eError),byte(eError),byte(eItsMe),byte(eError),//20-27
  byte(eError),byte(eError),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart) //28-2f
  );

  GB18030LangModel: SMModel = (
    classTable:   @GB18030_cls;
    classFactor:  7;
    stateTable:   @GB18030_st;
    charLenTable: @GB18030CharLenTable;
    CharsetID:    GB18030_CHARSET;
  );

//  GB18030LangModel: SMModel = (
//   classTable: (
//    idxsft:  eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk:  eUnitMsk4bits;
//    data:  @GB18030_cls;
//   );
//  classFactor:  7;
//  stateTable: (
//    idxsft:  eIdxSft4bits;
//    sftmsk:  eSftMsk4bits;
//    bitsft:  eBitSft4bits;
//    unitmsk:  eUnitMsk4bits;
//    data:  @GB18030_st;
//  );
//  charLenTable:  @GB18030CharLenTable;
//  CharsetID:  GB18030_CHARSET;
//  );

