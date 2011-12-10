const
	ISO2022JPCharLenTable: array [0..7] of PRUint32 = (0, 0, 0, 0, 0, 0, 0, 0);


  ISO2022JP_cls: array [Char] of Byte = (
    2,0,0,0,0,0,0,0,  // 00 - 07
    0,0,0,0,0,0,2,2,  // 08 - 0f
    0,0,0,0,0,0,0,0,  // 10 - 17
    0,0,0,1,0,0,0,0,  // 18 - 1f
    0,0,0,0,7,0,0,0,  // 20 - 27
    3,0,0,0,0,0,0,0,  // 28 - 2f
    0,0,0,0,0,0,0,0,  // 30 - 37
    0,0,0,0,0,0,0,0,  // 38 - 3f 
    6,0,4,0,8,0,0,0,  // 40 - 47
    0,9,5,0,0,0,0,0,  // 48 - 4f
    0,0,0,0,0,0,0,0,  // 50 - 57
    0,0,0,0,0,0,0,0,  // 58 - 5f
    0,0,0,0,0,0,0,0,  // 60 - 67 
    0,0,0,0,0,0,0,0,  // 68 - 6f
    0,0,0,0,0,0,0,0,  // 70 - 77
    0,0,0,0,0,0,0,0,  // 78 - 7f
    2,2,2,2,2,2,2,2,  // 80 - 87
    2,2,2,2,2,2,2,2,  // 88 - 8f
    2,2,2,2,2,2,2,2,  // 90 - 97
    2,2,2,2,2,2,2,2,  // 98 - 9f
    2,2,2,2,2,2,2,2,  // a0 - a7
    2,2,2,2,2,2,2,2,  // a8 - af
    2,2,2,2,2,2,2,2,  // b0 - b7
    2,2,2,2,2,2,2,2,  // b8 - bf
    2,2,2,2,2,2,2,2,  // c0 - c7
    2,2,2,2,2,2,2,2,  // c8 - cf
    2,2,2,2,2,2,2,2,  // d0 - d7
    2,2,2,2,2,2,2,2,  // d8 - df
    2,2,2,2,2,2,2,2,  // e0 - e7
    2,2,2,2,2,2,2,2,  // e8 - ef
    2,2,2,2,2,2,2,2,  // f0 - f7
    2,2,2,2,2,2,2,2   // f8 - ff
  );

  ISO2022JP_st: array [0..71] of Byte = (
    Byte(eStart),     3,Byte(eError),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart),//00-07
    Byte(eStart),Byte(eStart),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),//08-0f
    Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),//10-17
    Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eError),Byte(eError),//18-1f
    Byte(eError),     5,Byte(eError),Byte(eError),Byte(eError),     4,Byte(eError),Byte(eError),//20-27
    Byte(eError),Byte(eError),Byte(eError),     6,Byte(eItsMe),Byte(eError),Byte(eItsMe),Byte(eError),//28-2f
    Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eItsMe),//30-37
    Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eError),Byte(eError),Byte(eError),Byte(eError),//38-3f
    Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eError),Byte(eStart),Byte(eStart)//40-47
  );

  ISO2022JPSMModel: SMModel = (
    classTable:   @ISO2022JP_cls;
    classFactor:   10;
    stateTable:   @ISO2022JP_st;
    charLenTable:   @ISO2022JPCharLenTable;
    CharsetID:   ISO_2022_JP_CHARSET;
  );
