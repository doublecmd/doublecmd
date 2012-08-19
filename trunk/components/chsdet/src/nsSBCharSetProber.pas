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
// $Id: nsSBCharSetProber.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit nsSBCharSetProber;

interface

uses
  {$I dbg.inc}
	nsCore,
	CustomDetector;


const
  NUMBER_OF_SEQ_CAT = 4;

type
  SequenceModel = record
    charToOrderMap: PChar; (* [256] table use to find a char's order*)
    precedenceMatrix: PChar; (* [SAMPLE_SIZE][SAMPLE_SIZE]; table to find a 2-char sequence's frequency*)
    mTypicalPositiveRatio: float; (* = freqSeqs / totalSeqs *)
    keepEnglishLetter: Boolean; (* says if this script contains English characters (not implemented)*)
    CharsetID: eInternalCharsetID;
  end;

	TnsSingleByteCharSetProber = class(TCustomDetector)
		protected
      mModel: SequenceModel;
      mReversed: Boolean; (* TRUE if we need to reverse every pair in the model lookup*)
      mLastOrder: byte;   (*char order of last character*)
      mTotalSeqs: PRUint32;
      mSeqCounters: array [0..Pred(NUMBER_OF_SEQ_CAT)] of PRUint32;
      mTotalChar: PRUint32; (*characters that fall in our sampling range*)
      mFreqChar: PRUint32; (* Optional auxiliary prober for name decision. created and destroyed by the GroupProber*)
      mNameProber: TCustomDetector;

		public
    	constructor Create(model: SequenceModel; reversed: Boolean = FALSE; nameProber: TCustomDetector = nil); reintroduce;
      destructor Destroy; override;
		  function GetDetectedCharset: eInternalCharsetID; override;
		  function HandleData(aBuf: PChar;  aLen: integer): eProbingState; override;
		  procedure Reset; override;

		  function GetConfidence: float; override;
      (* This feature is not implemented yet. any current language model*)
      (* contain this parameter as PR_FALSE. No one is looking at this*)
      (* parameter or calling this method.*)
      (* Moreover, the nsSBCSGroupProber which calls the HandleData of this*)
      (* prober has a hard-coded call to FilterWithoutEnglishLetters which gets rid*)
      (* of the English letters.*)

		  function KeepEnglishLetters: Boolean; virtual;
		  (* (not implemented)*)
   end;

implementation

{$ifdef DEBUG_chardet}
uses
  TypInfo,
  SysUtils;
{$endif}

const
  SAMPLE_SIZE: byte = 64;
  SB_ENOUGH_REL_THRESHOLD = 1024;
  POSITIVE_SHORTCUT_THRESHOLD = SHORTCUT_THRESHOLD;
  NEGATIVE_SHORTCUT_THRESHOLD = 1 - POSITIVE_SHORTCUT_THRESHOLD;
  SYMBOL_CAT_ORDER: byte = 250;
  POSITIVE_CAT = (NUMBER_OF_SEQ_CAT-1);
(*#define NEGATIVE_APPROACH 1*)
{$ifdef NEGATIVE_APPROACH}
  NEGATIVE_CAT = 0;
{$endif}

{ TnsSingleByteCharSetProber }

constructor TnsSingleByteCharSetProber.Create(model: SequenceModel;  reversed: Boolean = FALSE; nameProber: TCustomDetector = nil);
begin
	inherited Create;
  mModel := model;
  mReversed := reversed;
  mNameProber := nameProber;
  Reset;
end;

destructor TnsSingleByteCharSetProber.Destroy;
begin

  inherited;
end;

function TnsSingleByteCharSetProber.GetDetectedCharset: eInternalCharsetID;
begin
  if mNameProber = nil then
    begin
      Result := mModel.CharsetID;
      exit;
    end;
  Result := mNameProber.GetDetectedCharset;
end;

(*#define NEGATIVE_APPROACH 1*)
function TnsSingleByteCharSetProber.GetConfidence: float;
var
	r: float;
begin
{$ifdef NEGATIVE_APPROACH}
  if mTotalSeqs > 0 then
	  if mTotalSeqs > mSeqCounters[NEGATIVE_CAT] * 10 then
      begin
        Result := (mTotalSeqs-mSeqCounters[NEGATIVE_CAT] * 10) / mTotalSeqs * mFreqChar / mTotalChar;
        exit;
      end;
  Result := SURE_NO;
{$else}
(*POSITIVE_APPROACH*)
  if mTotalSeqs > 0 then
    begin
      r := (1.0) * mSeqCounters[POSITIVE_CAT] / mTotalSeqs / mModel.mTypicalPositiveRatio;
      r := r * mFreqChar / mTotalChar;
      if r >= 1.0 then
	      r := SURE_YES;

      Result := r;
      exit;
    end;

  Result := SURE_NO;
{$endif}
end;

function TnsSingleByteCharSetProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
var
  order: byte;
  i: integer;
  cf: float;
begin
  Result := inherited HandleData(aBuf, aLen);
  if Result = psNotMe then
    exit;
// TODO - move here call to FilterWithoutEnglishLetters from nsSBCSGroupProber.pas
// if not mModel.keepEnglishLetter then ...

  for i := 0 to Pred(aLen) do
    begin
      order := byte(mModel.charToOrderMap[byte(aBuf[i])]);
      if order < SYMBOL_CAT_ORDER then
	      inc(mTotalChar);
      if order < SAMPLE_SIZE then
        begin
          inc(mFreqChar);
          if mLastOrder < SAMPLE_SIZE then
            begin
              inc(mTotalSeqs);
      {$ifdef DEBUG_chardet}
      //if ((mLastOrder * SAMPLE_SIZE + order) >= 4096) then
      AddDump(Format('oredr %4d for byte %4d last order %4d'+#10#13+
                  'array index %4d array high %8d'+#10#13+
                  'LangModel %s',
                  [order,byte(aBuf[i]),mLastOrder,
                     (mLastOrder * SAMPLE_SIZE + order),4096,
                     getEnumName(TypeInfo(eInternalCharsetID), integer(mModel.CharsetID))]));
      {$endif}
              if not mReversed then
	              inc(mSeqCounters[cardinal(mModel.precedenceMatrix[mLastOrder * SAMPLE_SIZE + order])])
              else
	              inc(mSeqCounters[cardinal(mModel.precedenceMatrix[order * SAMPLE_SIZE + mLastOrder])]);
              (* reverse the order of the letters in the lookup*)
            end;
        end;
      mLastOrder:= order;
    end;
  if mState = psDetecting then
	  if mTotalSeqs > SB_ENOUGH_REL_THRESHOLD then
      begin
        cf := GetConfidence;
        if cf > POSITIVE_SHORTCUT_THRESHOLD then
	        mState:= psFoundIt
        else
	        if cf < NEGATIVE_SHORTCUT_THRESHOLD then
		        mState:= psNotMe;
      end;

  Result := mState;
end;

function TnsSingleByteCharSetProber.KeepEnglishLetters: Boolean;
begin
  Result := mModel.keepEnglishLetter;
end;

procedure TnsSingleByteCharSetProber.Reset;
var
	i: integer;
begin
  if mEnabled then
    mState := psDetecting
  else
    mState := psNotMe;
  mLastOrder := 255;
  for i := 0 to Pred(NUMBER_OF_SEQ_CAT) do
	  mSeqCounters[i] := 0;
  mTotalSeqs := 0;
  mTotalChar := 0;
  mFreqChar := 0;
end;

end.

