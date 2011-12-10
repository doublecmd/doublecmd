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
// $Id: nsLatin1Prober.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit nsLatin1Prober;

interface

uses
	nsCore,
	CustomDetector;

type
	TnsLatin1Prober = class(TCustomDetector)
		private
      mLastCharClass: Char;
      mFreqCounter: array of PRUint32;
		public
    	constructor Create; override;
      destructor Destroy; override;

      function HandleData(aBuf: PChar;  aLen: integer): eProbingState; override;
      function GetDetectedCharset: eInternalCharsetID; override;
      procedure Reset; override;
      function GetConfidence: float; override;
      {$ifdef DEBUG_chardet}
      procedure DumpStatus; override;
      {$endif}
  end;

implementation

uses
	SysUtils;

const
	FREQ_CAT_NUM = 4;

  UDF = 0; (* undefined*)
  OTH = 1; (*other*)
  ASC = 2; (* ascii capital letter*)
  ASS = 3; (* ascii small letter*)
  ACV = 4; (* accent capital vowel*)
  ACO = 5; (* accent capital other*)
  ASV = 6; (* accent small vowel*)
  ASO = 7; (* accent small other*)
	CLASS_NUM = 8; (* total classes*)

Latin1_CharToClass: array [0..255] of byte =
(
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 00 - 07
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 08 - 0F
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 10 - 17
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 18 - 1F
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 20 - 27
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 28 - 2F
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 30 - 37
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 38 - 3F
  OTH, ASC, ASC, ASC, ASC, ASC, ASC, ASC,   // 40 - 47
  ASC, ASC, ASC, ASC, ASC, ASC, ASC, ASC,   // 48 - 4F
  ASC, ASC, ASC, ASC, ASC, ASC, ASC, ASC,   // 50 - 57
  ASC, ASC, ASC, OTH, OTH, OTH, OTH, OTH,   // 58 - 5F
  OTH, ASS, ASS, ASS, ASS, ASS, ASS, ASS,   // 60 - 67
  ASS, ASS, ASS, ASS, ASS, ASS, ASS, ASS,   // 68 - 6F
  ASS, ASS, ASS, ASS, ASS, ASS, ASS, ASS,   // 70 - 77
  ASS, ASS, ASS, OTH, OTH, OTH, OTH, OTH,   // 78 - 7F
  OTH, UDF, OTH, ASO, OTH, OTH, OTH, OTH,   // 80 - 87
  OTH, OTH, ACO, OTH, ACO, UDF, ACO, UDF,   // 88 - 8F
  UDF, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // 90 - 97
  OTH, OTH, ASO, OTH, ASO, UDF, ASO, ACO,   // 98 - 9F
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // A0 - A7
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // A8 - AF
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // B0 - B7
  OTH, OTH, OTH, OTH, OTH, OTH, OTH, OTH,   // B8 - BF
  ACV, ACV, ACV, ACV, ACV, ACV, ACO, ACO,   // C0 - C7
  ACV, ACV, ACV, ACV, ACV, ACV, ACV, ACV,   // C8 - CF
  ACO, ACO, ACV, ACV, ACV, ACV, ACV, OTH,   // D0 - D7
  ACV, ACV, ACV, ACV, ACV, ACO, ACO, ACO,   // D8 - DF
  ASV, ASV, ASV, ASV, ASV, ASV, ASO, ASO,   // E0 - E7
  ASV, ASV, ASV, ASV, ASV, ASV, ASV, ASV,   // E8 - EF
  ASO, ASO, ASV, ASV, ASV, ASV, ASV, OTH,   // F0 - F7
  ASV, ASV, ASV, ASV, ASV, ASO, ASO, ASO    // F8 - FF
);


(*
	 0 : illegal
   1 : very unlikely
   2 : normal
   3 : very likely
*)
Latin1ClassModel: array [0..63] of byte =
(
(*      UDF OTH ASC ASS ACV ACO ASV ASO  *)
(*UDF*)  0,  0,  0,  0,  0,  0,  0,  0,
(*OTH*)  0,  3,  3,  3,  3,  3,  3,  3,
(*ASC*)  0,  3,  3,  3,  3,  3,  3,  3,
(*ASS*)  0,  3,  3,  3,  1,  1,  3,  3,
(*ACV*)  0,  3,  3,  3,  1,  2,  1,  2,
(*ACO*)  0,  3,  3,  3,  3,  3,  3,  3,
(*ASV*)  0,  3,  1,  3,  1,  1,  1,  3,
(*ASO*)  0,  3,  1,  3,  1,  1,  3,  3
);

  { TnsLatin1Prober }

constructor TnsLatin1Prober.Create;
begin
	inherited Create;
  SetLength(mFreqCounter, FREQ_CAT_NUM);
  Reset;
end;

destructor TnsLatin1Prober.Destroy;
begin
  SetLength(mFreqCounter, 0);

  inherited;
end;

{$ifdef DEBUG_chardet}
procedure TnsLatin1Prober.DumpStatus;
begin
  printf(' Latin1Prober: %1.3f [%s]r'#13#10'',GetConfidence,GetCharSetName);
end;
{$endif}

function TnsLatin1Prober.GetDetectedCharset: eInternalCharsetID;
begin
	Result := WINDOWS_1252_CHARSET;
end;

function TnsLatin1Prober.GetConfidence: float;
var
  confidence: float;
  total: cardinal;
  i: integer;
begin
  if mState = psNotMe then
    begin
      Result := SURE_NO;
      exit;
    end;

  total := 0;
  for i := 0 to Pred(FREQ_CAT_NUM) do
	  total := total + mFreqCounter[i];

  if total = 0 then
	  confidence := 0.0
  else
    begin
      confidence := mFreqCounter[3] * 1.0 / total;
      confidence := confidence - (mFreqCounter[1] * 20.0 /total);
    end;
  if confidence < 0.0 then
	  confidence := 0.0;

  confidence := confidence * (0.50);
  (* lower the confidence of latin1 so that other more accurate detector *)
  (* can take priority.*)
  Result := confidence;
end;

function TnsLatin1Prober.HandleData(aBuf: PChar; aLen: integer): eProbingState;
var
  newBuf1: PChar;
  newLen1: integer;
  charClass: char;
  freq: byte;
  i: integer;
begin
  Result := inherited HandleData(aBuf, aLen);
  if Result = psNotMe then
    exit;

  newBuf1 := nil;
  newLen1 := 0;
  newBuf1 := AllocMem(aLen);
  try
    if not FilterWithEnglishLetters(aBuf,aLen,newBuf1,newLen1) then
      begin
        newBuf1 := aBuf;
        newLen1 := aLen;
      end;
    for i := 0 to Pred(newLen1) do
      begin
        charClass := char(Latin1_CharToClass[integer(newBuf1[i])]);
        freq := Latin1ClassModel[byte(mLastCharClass) * CLASS_NUM + byte(charClass)];
        if freq = 0 then
          begin
            mState:= psNotMe;
            break;
          end;
        inc(mFreqCounter[freq]);
        mLastCharClass := charClass;
      end;
	finally
	  FreeMem(newBuf1, aLen);
  end;
  Result := mState;
end;

procedure TnsLatin1Prober.Reset;
var
	i: integer;
begin
  mState := psDetecting;
  mLastCharClass := Char(OTH);
  for i := 0 to Pred(FREQ_CAT_NUM) do
  	mFreqCounter[i] := 0;
end;

end.

