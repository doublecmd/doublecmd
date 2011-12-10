const
	HZCharLenTable: array [0..5] of PRUint32 = (0, 0, 0, 0, 0, 0);

  HZ_cls: array [0..255] of Byte = (
    1,0,0,0,0,0,0,0,  // 00 - 07
    0,0,0,0,0,0,0,0,  // 08 - 0f
    0,0,0,0,0,0,0,0,  // 10 - 17
    0,0,0,1,0,0,0,0,  // 18 - 1f
    0,0,0,0,0,0,0,0,  // 20 - 27
    0,0,0,0,0,0,0,0,  // 28 - 2f
    0,0,0,0,0,0,0,0,  // 30 - 37
    0,0,0,0,0,0,0,0,  // 38 - 3f
    0,0,0,0,0,0,0,0,  // 40 - 47
    0,0,0,0,0,0,0,0,  // 48 - 4f
    0,0,0,0,0,0,0,0,  // 50 - 57
    0,0,0,0,0,0,0,0,  // 58 - 5f
    0,0,0,0,0,0,0,0,  // 60 - 67
    0,0,0,0,0,0,0,0,  // 68 - 6f
    0,0,0,0,0,0,0,0,  // 70 - 77
    0,0,0,4,0,5,2,0,  // 78 - 7f
    1,1,1,1,1,1,1,1,  // 80 - 87
    1,1,1,1,1,1,1,1,  // 88 - 8f
    1,1,1,1,1,1,1,1,  // 90 - 97
    1,1,1,1,1,1,1,1,  // 98 - 9f
    1,1,1,1,1,1,1,1,  // a0 - a7
    1,1,1,1,1,1,1,1,  // a8 - af
    1,1,1,1,1,1,1,1,  // b0 - b7
    1,1,1,1,1,1,1,1,  // b8 - bf
    1,1,1,1,1,1,1,1,  // c0 - c7
    1,1,1,1,1,1,1,1,  // c8 - cf
    1,1,1,1,1,1,1,1,  // d0 - d7
    1,1,1,1,1,1,1,1,  // d8 - df
    1,1,1,1,1,1,1,1,  // e0 - e7
    1,1,1,1,1,1,1,1,  // e8 - ef
    1,1,1,1,1,1,1,1,  // f0 - f7
    1,1,1,1,1,1,1,1   // f8 - ff
  );

HZ_st: array [0..47] of Byte = (
  Byte(eStart),Byte(eError),     3,Byte(eStart),Byte(eStart),Byte(eStart),Byte(eError),Byte(eError),//00-07
  Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),//08-0f
  Byte(eItsMe),Byte(eItsMe),Byte(eError),Byte(eError),Byte(eStart),Byte(eStart),     4,Byte(eError),//10-17
       5,Byte(eError),     6,Byte(eError),     5,     5,     4,Byte(eError),//18-1f
       4,Byte(eError),     4,     4,     4,Byte(eError),     4,Byte(eError),//20-27
       4,Byte(eItsMe),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart)//28-2f
);

HZSMModel: SMModel = (
  classTable:   @HZ_cls;
  classFactor:   6;
  stateTable:   @HZ_st;
  charLenTable:   @HZCharLenTable;
  CharsetID:   HZ_GB_2312_CHARSET;
);