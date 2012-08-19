const
  UTF8_cls: array [0..255] of byte = (
  1,1,1,1,1,1,1,1,  // 00 - 07  //allow 0x00 as a legal value
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
  2,2,2,2,3,3,3,3,  // 80 - 87
  4,4,4,4,4,4,4,4,  // 88 - 8f
  4,4,4,4,4,4,4,4,  // 90 - 97
  4,4,4,4,4,4,4,4,  // 98 - 9f
  5,5,5,5,5,5,5,5,  // a0 - a7
  5,5,5,5,5,5,5,5,  // a8 - af
  5,5,5,5,5,5,5,5,  // b0 - b7
  5,5,5,5,5,5,5,5,  // b8 - bf
  0,0,6,6,6,6,6,6,  // c0 - c7
  6,6,6,6,6,6,6,6,  // c8 - cf
  6,6,6,6,6,6,6,6,  // d0 - d7
  6,6,6,6,6,6,6,6,  // d8 - df
  7,8,8,8,8,8,8,8,  // e0 - e7
  8,8,8,8,8,9,8,8,  // e8 - ef
  10,11,11,11,11,11,11,11,  // f0 - f7
  12,13,13,13,14,15,0,0     // f8 - ff
  );

 	UTF8CharLenTable: array [0..15] of byte = (0, 1, 0, 0, 0, 0, 2, 3,
                            				 				 3, 3, 4, 4, 5, 5, 6, 6 );

  UTF8_st: array [0..207] of byte = (
  byte(eError),byte(eStart),byte(eError),byte(eError),byte(eError),byte(eError),     12,     10,//00-07
       9,     11,     8,     7,     6,     5,     4,     3,//08-0f
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//10-17
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//18-1f
  byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),//20-27
  byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eStart),//28-2f
  byte(eError),byte(eError),     5,     5,     5,     5,byte(eError),byte(eError),//30-37
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//38-3f
  byte(eError),byte(eError),byte(eError),     5,     5,     5,byte(eError),byte(eError),//40-47
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//48-4f
  byte(eError),byte(eError),     7,     7,     7,     7,byte(eError),byte(eError),//50-57
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//58-5f
  byte(eError),byte(eError),byte(eError),byte(eError),     7,     7,byte(eError),byte(eError),//60-67
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//68-6f
  byte(eError),byte(eError),     9,     9,     9,     9,byte(eError),byte(eError),//70-77
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//78-7f
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),     9,byte(eError),byte(eError),//80-87
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//88-8f
  byte(eError),byte(eError),     12,     12,     12,     12,byte(eError),byte(eError),//90-97
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//98-9f
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),     12,byte(eError),byte(eError),//a0-a7
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//a8-af
  byte(eError),byte(eError),     12,     12,     12,byte(eError),byte(eError),byte(eError),//b0-b7
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),//b8-bf
  byte(eError),byte(eError),byte(eStart),byte(eStart),byte(eStart),byte(eStart),byte(eError),byte(eError),//c0-c7
  byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError),byte(eError) //c8-cf
  );

  UTF8LangModel: SMModel = (
    classTable:  @UTF8_cls;
    classFactor: 16;
    stateTable:  @UTF8_st;
    charLenTable:  @UTF8CharLenTable;
    CharsetID: UTF8_CHARSET;
  );
