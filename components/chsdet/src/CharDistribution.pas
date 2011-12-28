// +----------------------------------------------------------------------+
// |    chsdet - Charset Detector Library                                 |
// +----------------------------------------------------------------------+
// | Copyright (C) 2006, Nick Yakowlew     http://chsdet.sourceforge.net  |
// +----------------------------------------------------------------------+
// | Based on Mozilla sources     http://www.mozilla.org/projects/intl/   |
// +----------------------------------------------------------------------+
// | This library is free software; you can redistribute it and/or modify |
// | it under the terms of the GNU General Public License as published by |
// | the Free Software Foundation; either version 2 of the License, or    |
// | (at your option) any later version.                                  |
// | This library is distributed in the hope that it will be useful       |
// | but WITHOUT ANY WARRANTY; without even the implied warranty of       |
// | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 |
// | See the GNU Lesser General Public License for more details.          |
// | http://www.opensource.org/licenses/lgpl-license.php                  |
// +----------------------------------------------------------------------+
//
// $Id: CharDistribution.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit CharDistribution;

{$R-}

interface

uses
	nsCore;

type
	TCharDistributionAnalysis = class (TObject)
    protected
      //mDone: PRBool; (*If this flag is set to PR_TRUE, detection is done and conclusion has been made*)
      // YaN: nice idea. Unfortunately is not implemented :((
      mFreqChars: PRUint32; (*The number of characters whose frequency order is less than 512*)
      mTotalChars: PRUint32; (*Total character encounted.*)

      mCharToFreqOrder: pPRInt16; (*Mapping table to get frequency order from char order
      																			(get from GetOrder())*)

      mTableSize: PRUint32; (*Size of above table*)
      mTypicalDistributionRatio: double;(*This is a constant value varies from language to language,
      																	  it is used in calculating confidence.
                                          See my paper for further detail.*)



      //we do not handle character base on its original encoding string, but
      //convert this encoding string to a number, here called order.
      //This allow multiple encoding of a language to share one frequency table
			function GetOrder(str: PChar): PRInt32; virtual; abstract;
      (*feed a block of data and do distribution analysis*)
//      function HandleData(const aBuf: PChar; aLen: PRUint32): eProbingState; virtual; abstract;
		public
      destructor Destroy; override;
      (*This function is for future extension.
      	Caller can use this function to control analyser's behavior*)
      // procedure SetOpion(); virtual; abstract;
    	(*return confidence base on existing data*)
			function GetConfidence: float; virtual;
      (*Reset analyser, clear any state *)
	    procedure Reset; virtual;

      (*It is not necessary to receive all data to draw conclusion.
        For charset detection, certain amount of data is enough*)
      function GotEnoughData: Boolean;

      (*Feed a character with known length*)
      procedure HandleOneChar(aStr: PChar;  aCharLen: PRUint32); virtual;

  end;

	TEUCTWDistributionAnalysis = class (TCharDistributionAnalysis)
  (*for euc-TW encoding, we are interested *)
  (*  first  byte range: 0xc4 -- 0xfe*)
  (*  second byte range: 0xa1 -- 0xfe*)
  (*no validation needed here. State machine has done that*)
  	protected
			function GetOrder(str: PChar): PRInt32; override;
    public
    	constructor Create; reintroduce;
  end;

  TEUCKRDistributionAnalysis = class (TCharDistributionAnalysis)
  (*for euc-KR encoding, we are interested *)
  (*  first  byte range: 0xb0 -- 0xfe*)
  (*  second byte range: 0xa1 -- 0xfe*)
  (*no validation needed here. State machine has done that*)
  	protected
			function GetOrder(str: PChar): PRInt32; override;
    public
    	constructor Create; reintroduce;
  end;

  TGB2312DistributionAnalysis = class (TCharDistributionAnalysis)
  (*for GB2312 encoding, we are interested *)
  (*  first  byte range: 0xb0 -- 0xfe*)
  (*  second byte range: 0xa1 -- 0xfe*)
  (*no validation needed here. State machine has done that*)
  	protected
			function GetOrder(str: PChar): PRInt32; override;
    public
    	constructor Create; reintroduce;
  end;

  TBig5DistributionAnalysis = class (TCharDistributionAnalysis)
  (*for big5 encoding, we are interested *)
  (*  first  byte range: 0xa4 -- 0xfe*)
  (*  second byte range: 0x40 -- 0x7e , 0xa1 -- 0xfe*)
  (*no validation needed here. State machine has done that*)
  	protected
			function GetOrder(str: PChar): PRInt32; override;
    public
    	constructor Create; reintroduce;
  end;

  TSJISDistributionAnalysis = class (TCharDistributionAnalysis)
  (*for sjis encoding, we are interested *)
  (*  first  byte range: 0x81 -- 0x9f , 0xe0 -- 0xfe*)
  (*  second byte range: 0x40 -- 0x7e,  0x81 -- oxfe*)
  (*no validation needed here. State machine has done that*)
  	protected
			function GetOrder(str: PChar): PRInt32; override;
    public
    	constructor Create; reintroduce;
  end;

  TEUCJPDistributionAnalysis = class (TCharDistributionAnalysis)
  (*for euc-JP encoding, we are interested *)
  (*  first  byte range: 0xa0 -- 0xfe*)
  (*  second byte range: 0xa1 -- 0xfe*)
  (*no validation needed here. State machine has done that*)
  	protected
			function GetOrder(str: PChar): PRInt32; override;
    public
    	constructor Create; reintroduce;
  end;

implementation
uses
	JISFreq,
	Big5Freq,
  EUCKRFreq,
  EUCTWFreq,
  GB2312Freq;

destructor TCharDistributionAnalysis.Destroy;
begin
  mCharToFreqOrder := nil;

  inherited;
end;

procedure TCharDistributionAnalysis.HandleOneChar(aStr: PChar;  aCharLen: PRUint32);
var
  order: integer;
begin
	(*we only care about 2-bytes character in our distribution analysis*)
  if (aCharLen=2) then
  	order := GetOrder(aStr)
  else
  	order := -1;
  if order >= 0 then
    begin
      inc(mTotalChars); (*order is valid*)
      if order < integer(mTableSize) then
        begin
          if 512 > aPRint16(mCharToFreqOrder)[order] then
            inc(mFreqChars);
        end;
    end;
end;

function TCharDistributionAnalysis.GetConfidence: float;
var
  r: float;
begin
  (*if we didn't receive any character in our consideration range, return negative answer*)
  if mTotalChars <= 0 then
    begin
      Result := SURE_NO;
      exit;
    end;
  if mTotalChars <> mFreqChars then
  begin
    r := mFreqChars / ((mTotalChars - mFreqChars) * mTypicalDistributionRatio);
    if r < SURE_YES then
      begin
        Result := r;
        exit;
      end;
  end;
  (*normalize confidence, (we don't want to be 100% sure)*)
  Result := SURE_YES;
end;

procedure TCharDistributionAnalysis.Reset;
begin
  mTotalChars := 0;
  mFreqChars := 0;
end;

function TCharDistributionAnalysis.GotEnoughData: Boolean;
begin
  Result := (mTotalChars > ENOUGH_DATA_THRESHOLD);
end;

constructor TEUCTWDistributionAnalysis.Create;
begin
  inherited Create;
  mCharToFreqOrder := @EUCTWCharToFreqOrder;
  mTableSize := EUCTW_TABLE_SIZE;
  mTypicalDistributionRatio := EUCTW_TYPICAL_DISTRIBUTION_RATIO;
end;

function TEUCTWDistributionAnalysis.GetOrder(str: PChar): PRInt32;
begin
  if byte(str^) >= $c4 then
    Result := 94 * (byte(str[0]) - $c4) + byte(str[1]) - byte($a1)
  else
    Result := -1;
end;

constructor TEUCKRDistributionAnalysis.Create;
begin
	inherited Create;
  mCharToFreqOrder := @EUCKRCharToFreqOrder;
  mTableSize := EUCKR_TABLE_SIZE;
  mTypicalDistributionRatio := EUCKR_TYPICAL_DISTRIBUTION_RATIO;
end;

function TEUCKRDistributionAnalysis.GetOrder(str: PChar): PRInt32;
begin
  if byte(str^) >= $b0 then
    Result := 94 * (byte(str[0]) - $b0) + byte(str[1]) - $a1
  else
    Result := -1;
end;

constructor TGB2312DistributionAnalysis.Create;
begin
  inherited;
  mCharToFreqOrder := @GB2312CharToFreqOrder;
  mTableSize := GB2312_TABLE_SIZE;
  mTypicalDistributionRatio := GB2312_TYPICAL_DISTRIBUTION_RATIO;
end;

function TGB2312DistributionAnalysis.GetOrder(str: PChar): PRInt32;
begin
  if (byte(str[0]) >= $b0) and
  	 (byte(str[1]) >= $a1) then
    Result := 94 * (byte(str[0]) - $b0) + byte(str[1]) - $a1
  else
    Result := -1;
end;

constructor TBig5DistributionAnalysis.Create;
begin
  inherited;
  mCharToFreqOrder := @Big5CharToFreqOrder;
  mTableSize := BIG5_TABLE_SIZE;
  mTypicalDistributionRatio := BIG5_TYPICAL_DISTRIBUTION_RATIO;
end;

function TBig5DistributionAnalysis.GetOrder(str: PChar): PRInt32;
begin
  if byte(str[0]) >= $a4 then
  	begin
      if byte(str[1]) >= $a1 then
        Result := 157 * (byte(str[0]) - $a4) + byte(str[1]) - $a1 + 63
      else
        Result := 157 * (byte(str[0]) - $a4) + byte(str[1]) - $40;
    end
  else
    Result:= -1;
end;

constructor TSJISDistributionAnalysis.Create;
begin
	inherited Create;
  mCharToFreqOrder := @JISCharToFreqOrder;
  mTableSize := JIS_TABLE_SIZE;
  mTypicalDistributionRatio := JIS_TYPICAL_DISTRIBUTION_RATIO;
end;

function TSJISDistributionAnalysis.GetOrder(str: PChar): PRInt32;
var
  order: PRInt32;
begin
  if (byte(str[0]) >= $81) and
  	 (byte(str[0]) <= $9f) then
    order := 188 * (byte(str[0]) - $81)
  else
    if (byte(str[0]) >= $e0) and
    	 (byte(str[0]) <= $ef) then
	    order := 188 * (byte(str[0]) - $e0 + 31)
    else
	    begin
  	    Result:= -1;
    	  exit;
    	end;
    order := order + (byte((str+1)^) - $40);
    if byte(str[1]) > $7f then
	    dec(order);
    Result := order;
end;

constructor TEUCJPDistributionAnalysis.Create;
begin
	inherited Create;
  mCharToFreqOrder := @JISCharToFreqOrder;
  mTableSize := JIS_TABLE_SIZE;
  mTypicalDistributionRatio := JIS_TYPICAL_DISTRIBUTION_RATIO;
end;

function TEUCJPDistributionAnalysis.GetOrder(str: PChar): PRInt32;
begin
  if byte(str[0]) >= $a0 then
    Result := 94 * (byte(str[0]) - $a1) + byte(str[1]) - $a1
  else
    Result:= -1;
end;

end.



