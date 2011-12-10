const
	ISO2022KRCharLenTable: array [0..5] of PRUint32 = (0, 0, 0, 0, 0, 0);

  ISO2022KR_cls: array [Char] of Byte = (
    2,0,0,0,0,0,0,0,  // 00 - 07
    0,0,0,0,0,0,0,0,  // 08 - 0f
    0,0,0,0,0,0,0,0,  // 10 - 17
    0,0,0,1,0,0,0,0,  // 18 - 1f
    0,0,0,0,3,0,0,0,  // 20 - 27
    0,4,0,0,0,0,0,0,  // 28 - 2f
    0,0,0,0,0,0,0,0,  // 30 - 37 
    0,0,0,0,0,0,0,0,  // 38 - 3f
    0,0,0,5,0,0,0,0,  // 40 - 47
    0,0,0,0,0,0,0,0,  // 48 - 4f
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

  ISO2022KR_st: array [0..39] of Byte = (
    Byte(eStart),     3,Byte(eError),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eError),Byte(eError),//00-07
    Byte(eError),Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),Byte(eItsMe),//08-0f
    Byte(eItsMe),Byte(eItsMe),Byte(eError),Byte(eError),Byte(eError),     4,Byte(eError),Byte(eError),//10-17
    Byte(eError),Byte(eError),Byte(eError),Byte(eError),     5,Byte(eError),Byte(eError),Byte(eError),//18-1f
    Byte(eError),Byte(eError),Byte(eError),Byte(eItsMe),Byte(eStart),Byte(eStart),Byte(eStart),Byte(eStart) //20-27
  );

  ISO2022KRSMModel: SMModel = (
    classTable:   @ISO2022KR_cls;
    classFactor:   6;
    stateTable:   @ISO2022KR_st;
    charLenTable:   @ISO2022KRCharLenTable;
    CharsetID:   ISO_2022_KR_CHARSET;
  );
