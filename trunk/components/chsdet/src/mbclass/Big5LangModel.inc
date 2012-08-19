const
	Big5CharLenTable: array [0..4] of Byte = (0, 1, 1, 2, 0);

               
  BIG5_cls: array [0..255] of Byte = (
  1,1,1,1,1,1,1,1,  // 00 - 07    //allow 0x00 as legal value
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
  4,4,4,4,4,4,4,4,  // 80 - 87 
  4,4,4,4,4,4,4,4,  // 88 - 8f
  4,4,4,4,4,4,4,4,  // 90 - 97 
  4,4,4,4,4,4,4,4,  // 98 - 9f 
  4,3,3,3,3,3,3,3,  // a0 - a7
  3,3,3,3,3,3,3,3,  // a8 - af 
  3,3,3,3,3,3,3,3,  // b0 - b7
  3,3,3,3,3,3,3,3,  // b8 - bf
  3,3,3,3,3,3,3,3,  // c0 - c7
  3,3,3,3,3,3,3,3,  // c8 - cf 
  3,3,3,3,3,3,3,3,  // d0 - d7
  3,3,3,3,3,3,3,3,  // d8 - df
  3,3,3,3,3,3,3,3,  // e0 - e7
  3,3,3,3,3,3,3,3,  // e8 - ef
  3,3,3,3,3,3,3,3,  // f0 - f7
  3,3,3,3,3,3,3,0   // f8 - ff
  );

  BIG5_st: array [0..23] of Byte = (
  Byte(eError),Byte(eStart),Byte(eStart),     3,Byte(eError),Byte(eError),Byte(eError),Byte(eError),//00-07
  Byte(eError),Byte(eError),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eError),//08-0f
  Byte(eError),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart) //10-17
  );

  Big5LangModel: SMModel = (
    classTable:   @BIG5_cls;
    classFactor:  5;
    stateTable:   @BIG5_st;
    charLenTable: @Big5CharLenTable;
    CharsetID:    BIG5_CHARSET;
  );
//  Big5LangModel: SMModel = (
//   classTable: (
//    idxsft:   eIdxSft4bits;
//    sftmsk:   eSftMsk4bits;
//    bitsft:   eBitSft4bits;
//    unitmsk:   eUnitMsk4bits;
//    data:   @BIG5_cls;
//   );
//  classFactor:   5;
//  stateTable: (
//    idxsft:   eIdxSft4bits;
//    sftmsk:   eSftMsk4bits;
//    bitsft:   eBitSft4bits;
//    unitmsk:   eUnitMsk4bits;
//    data:   @BIG5_st;
//  );
//  charLenTable:   @Big5CharLenTable;
//  CharsetID:   BIG5_CHARSET;
//  );
