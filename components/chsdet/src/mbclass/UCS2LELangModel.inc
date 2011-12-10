const
  UCS2LECharLenTable: array [0..5] of PRUint32 = (2, 2, 2, 2, 2, 2);

  UCS2LE_cls: array [Char] of Byte = (
  0,0,0,0,0,0,0,0,  // 00 - 07  //allow 0x00 as a legal value
  0,0,1,0,0,2,0,0,  // 08 - 0f
  0,0,0,0,0,0,0,0,  // 10 - 17
  0,0,0,3,0,0,0,0,  // 18 - 1f
  0,0,0,0,0,0,0,0,  // 20 - 27
  0,3,3,3,3,3,0,0,  // 28 - 2f
  0,0,0,0,0,0,0,0,  // 30 - 37
  0,0,0,0,0,0,0,0,  // 38 - 3f
  0,0,0,0,0,0,0,0,  // 40 - 47
  0,0,0,0,0,0,0,0,  // 48 - 4f
  0,0,0,0,0,0,0,0,  // 50 - 57
  0,0,0,0,0,0,0,0,  // 58 - 5f
  0,0,0,0,0,0,0,0,  // 60 - 67
  0,0,0,0,0,0,0,0,  // 68 - 6f
  0,0,0,0,0,0,0,0,  // 70 - 77
  0,0,0,0,0,0,0,0,  // 78 - 7f
  0,0,0,0,0,0,0,0,  // 80 - 87
  0,0,0,0,0,0,0,0,  // 88 - 8f
  0,0,0,0,0,0,0,0,  // 90 - 97
  0,0,0,0,0,0,0,0,  // 98 - 9f
  0,0,0,0,0,0,0,0,  // a0 - a7
  0,0,0,0,0,0,0,0,  // a8 - af
  0,0,0,0,0,0,0,0,  // b0 - b7
  0,0,0,0,0,0,0,0,  // b8 - bf
  0,0,0,0,0,0,0,0,  // c0 - c7
  0,0,0,0,0,0,0,0,  // c8 - cf
  0,0,0,0,0,0,0,0,  // d0 - d7
  0,0,0,0,0,0,0,0,  // d8 - df
  0,0,0,0,0,0,0,0,  // e0 - e7
  0,0,0,0,0,0,0,0,  // e8 - ef
  0,0,0,0,0,0,0,0,  // f0 - f7
  0,0,0,0,0,0,4,5   // f8 - ff
);

  UCS2LE_st: array [0..55] of Byte = (
       6,     6,     7,     6,     4,     3,integer(eError),integer(eError),//00-07
  integer(eError),integer(eError),integer(eError),integer(eError),integer(eItsMe),integer(eItsMe),integer(eItsMe),integer(eItsMe),//08-0f
  integer(eItsMe),integer(eItsMe),     5,     5,     5,integer(eError),integer(eItsMe),integer(eError),//10-17
       5,     5,     5,integer(eError),     5,integer(eError),     6,     6,//18-1f
       7,     6,     8,     8,     5,     5,     5,integer(eError),//20-27
       5,     5,     5,integer(eError),integer(eError),integer(eError),     5,     5,//28-2f
       5,     5,     5,integer(eError),     5,integer(eError),integer(eStart),integer(eStart) //30-37
);

  UCS2LELangModel: SMModel = (
    classTable:   @UCS2LE_cls;
    classFactor:  6;
    stateTable:   @UCS2LE_st;
    charLenTable: @UCS2LECharLenTable;
    CharsetID:    UTF16_LE_CHARSET;
  );



