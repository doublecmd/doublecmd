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
// $Id: nsHebrewProber.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit nsHebrewProber;

interface

uses
	nsCore,
	CustomDetector;
(* This prober doesn't actually recognize a language or a charset.*)
(* It is a helper prober for the use of the Hebrew model probers*)
type
	TnsHebrewProber = class(TCustomDetector)
		protected
      mFinalCharLogicalScore: PRInt32;
      mFinalCharVisualScore: PRInt32;
      (* The two last characters seen in the previous buffer.*)
      mPrev: char;
      mBeforePrev: char;
      (* These probers are owned by the group prober.*)
      mLogicalProb: TCustomDetector;
      mVisualProb: TCustomDetector;

		public
    	constructor Create; override;
      destructor Destroy; override;
      function HandleData(aBuf: PChar; aLen: integer): eProbingState; override;
      function GetDetectedCharset: eInternalCharsetID; override;
      procedure Reset; override;
      function GetState: eProbingState; override;
      procedure SetModelProbers(logicalPrb: TCustomDetector;  visualPrb: TCustomDetector);
      {$ifdef DEBUG_chardet}
      procedure DumpStatus; override;
      {$endif}
      function isFinal(c: char): Boolean;
      function isNonFinal(c: char): Boolean;
      function GetConfidence: float; override;

  end;

implementation
(* windows-1255 / ISO-8859-8 code points of interest*)
const
  FINAL_KAF = (#$ea);
  NORMAL_KAF = (#$eb);
  FINAL_MEM = (#$ed);
  NORMAL_MEM = (#$ee);
  FINAL_NUN = (#$ef);
  NORMAL_NUN = (#$f0);
  FINAL_PE = (#$f3);
  NORMAL_PE = (#$f4);
  FINAL_TSADI = (#$f5);
//  YaN - Not used
//  NORMAL_TSADI = (#$f6);
  (* Minimum Visual vs Logical final letter score difference.*)
  (* If the difference is below this, don't rely solely on the final letter score distance.*)
  MIN_FINAL_CHAR_DISTANCE = (5);
  (* Minimum Visual vs Logical model score difference.*)
  (* If the difference is below this, don't rely at all on the model score distance.*)
  MIN_MODEL_DISTANCE = (0.01);
var
  VISUAL_HEBREW_CHARSET,
  LOGICAL_HEBREW_CHARSET: eInternalCharsetID;

{ TnsHebrewProber }

constructor TnsHebrewProber.Create;
begin
  inherited Create;
	mLogicalProb := nil;
  mVisualProb := nil;

  VISUAL_HEBREW_CHARSET := ISO_8859_8_CHARSET;
  LOGICAL_HEBREW_CHARSET := WINDOWS_1255_CHARSET;

  Reset;
end;

destructor TnsHebrewProber.Destroy;
begin

  inherited;
end;

{$ifdef DEBUG_chardet}
procedure TnsHebrewProber.DumpStatus;
begin
  printf('  HEB: %d - %d [Logical-Visual score]r'#13#10'',mFinalCharLogicalScore,mFinalCharVisualScore); 
end;
{$endif}

(* Make the decision: is it Logical or Visual?*)
function TnsHebrewProber.GetDetectedCharset: eInternalCharsetID;
var
	finalsub: PRInt32;
  modelsub: float;
begin
  (* If the final letter score distance is dominant enough, rely on it.*)
  finalsub := mFinalCharLogicalScore-mFinalCharVisualScore;
  if finalsub >= MIN_FINAL_CHAR_DISTANCE then
    begin
      Result := LOGICAL_HEBREW_CHARSET;
      exit;
    end;
  if finalsub <= -(MIN_FINAL_CHAR_DISTANCE) then
    begin
      Result:= VISUAL_HEBREW_CHARSET;
      exit;
    end;
 (* It's not dominant enough, try to rely on the model scores instead.*)
  modelsub := mLogicalProb.GetConfidence - mVisualProb.GetConfidence;
  if modelsub > MIN_MODEL_DISTANCE then
    begin
      Result := LOGICAL_HEBREW_CHARSET;
      exit;
    end;
  if modelsub < -(MIN_MODEL_DISTANCE) then
    begin
      Result := VISUAL_HEBREW_CHARSET;
      exit;
    end;
  (* Still no good, back to final letter distance, maybe it'll save the day.*)
  if finalsub < 0 then
    begin
      Result := VISUAL_HEBREW_CHARSET;
      exit;
    end;
 (* (finalsub > 0 - Logical) or (don't know what to do) default to Logical.*)
  Result:= LOGICAL_HEBREW_CHARSET;
end;

function TnsHebrewProber.GetConfidence: float;
begin
	Result := 0.0;
end;

function TnsHebrewProber.GetState: eProbingState;
begin
  (* Remain active as long as any of the model probers are active.*)
  if (mLogicalProb.GetState = psNotMe) and
  	 (mVisualProb.GetState = psNotMe)  then
    begin
      Result := psNotMe;
      exit;
    end;
  Result := psDetecting;
end;

function TnsHebrewProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
(*
 * Final letter analysis for logical-visual decision.
 * Look for evidence that the received buffer is either logical Hebrew or
 * visual Hebrew.
 * The following cases are checked:
 * 1) A word longer than 1 letter, ending with a final letter. This is an
 *    indication that the text is laid out "naturally" since the final letter
 *    really appears at the end. +1 for logical score.
 * 2) A word longer than 1 letter, ending with a Non-Final letter. In normal
 *    Hebrew, words ending with Kaf, Mem, Nun, Pe or Tsadi, should not end with
 *    the Non-Final form of that letter. Exceptions to this rule are mentioned
 *    above in isNonFinal(). This is an indication that the text is laid out
 *    backwards. +1 for visual score
 * 3) A word longer than 1 letter, starting with a final letter. Final letters 
 *    should not appear at the beginning of a word. This is an indication that 
 *    the text is laid out backwards. +1 for visual score.
 *
 * The visual score and logical score are accumulated throughout the text and 
 * are finally checked against each other in GetCharSetName().
 * No checking for final letters in the middle of words is done since that case
 * is not an indication for either Logical or Visual text.
 *
 * The input buffer should not contain any white spaces that are not (' ')
 * or any low-ascii punctuation marks.
 *)
var
  curPtr: PChar;
	endPtr: PChar;
	cur: char;
begin
  (* check prober enabled*)
  inherited HandleData(aBuf, aLen);

  (* Both model probers say it's not them. No reason to continue.*)
  if GetState = psNotMe then
    begin
      Result:= psNotMe;
      exit;
    end;

	endPtr := aBuf + aLen;
  curPtr := aBuf;
  while curPtr < endPtr do
    begin
      cur := curPtr^;
      if cur = ' ' then
        begin
          (* We stand on a space - a word just ended*)
          if mBeforePrev <> ' ' then
            begin
              (* *(curPtr-2) was not a space so prev is not a 1 letter word*)
              if isFinal(mPrev) then
	              inc(mFinalCharLogicalScore)
              else
              (* case (1) [-2:not space][-1:final letter][cur:space]*)
	              if isNonFinal(mPrev) then
		              inc(mFinalCharVisualScore);
              (* case (2) [-2:not space][-1:Non-Final letter][cur:space]*)
            end;
        end
      else
        begin
          (* Not standing on a space*)
          if (mBeforePrev = ' ') and
          	  isFinal(mPrev) and
              (cur <> ' ')     then
	          inc(mFinalCharVisualScore);
          (* case (3) [-2:space][-1:final letter][cur:not space]*)
        end;
      mBeforePrev := mPrev;
      mPrev := cur;

      inc(curPtr);
    end;
	(* Forever detecting, till the end or until both model probers return psNotMe (handled above).*)
  Result := psDetecting;
end;

function TnsHebrewProber.isFinal(c: char): Boolean;
begin
  Result := ((c=FINAL_KAF))or((c=FINAL_MEM))or((c=FINAL_NUN))or((c=FINAL_PE))or((c=FINAL_TSADI)); 
end;

function TnsHebrewProber.isNonFinal(c: char): Boolean;
begin
  Result:= ((c=NORMAL_KAF))or((c=NORMAL_MEM))or((c=NORMAL_NUN))or((c=NORMAL_PE));
  (* The normal Tsadi is not a good Non-Final letter due to words like *)
  (* 'lechotet' (to chat) containing an apostrophe after the tsadi. This *)
  (* apostrophe is converted to a space in FilterWithoutEnglishLetters causing *)
  (* the Non-Final tsadi to appear at an end of a word even though this is not *)
  (* the case in the original text.*)
  (* The letters Pe and Kaf rarely display a related behavior of not being a *)
  (* good Non-Final letter. Words like 'Pop', 'Winamp' and 'Mubarak' for *)
  (* example legally end with a Non-Final Pe or Kaf. However, the benefit of *)
  (* these letters as Non-Final letters outweighs the damage since these words *)
  (* are quite rare.*)
end;

procedure TnsHebrewProber.Reset;
begin
  mFinalCharLogicalScore := 0;
  mFinalCharVisualScore := 0;
  mPrev := ' ';
  mBeforePrev := ' ';
  (* mPrev and mBeforePrev are initialized to space in order to simulate a word *)
  (* delimiter at the beginning of the data*)
end;

procedure TnsHebrewProber.SetModelProbers(logicalPrb, visualPrb: TCustomDetector);
begin
  mLogicalProb := logicalPrb;
  mVisualProb := visualPrb;
end;

(**
 * ** General ideas of the Hebrew charset recognition **
 *
 * Four main charsets exist in Hebrew:
 * "ISO-8859-8" - Visual Hebrew
 * "windows-1255" - Logical Hebrew 
 * "ISO-8859-8-I" - Logical Hebrew
 * "x-mac-hebrew" - ?? Logical Hebrew ??
 *
 * Both "ISO" charsets use a completely identical set of code points, whereas
 * "windows-1255" and "x-mac-hebrew" are two different proper supersets of 
 * these code points. windows-1255 defines additional characters in the range
 * 0x80-0x9F as some misc punctuation marks as well as some Hebrew-specific
 * diacritics and additional 'Yiddish' ligature letters in the range 0xc0-0xd6.
 * x-mac-hebrew defines similar additional code points but with a different 
 * mapping.
 *
 * As  as an average Hebrew text with no diacritics is concerned, all four 
 * charsets are identical with respect to code points. Meaning that for the 
 * main Hebrew alphabet, all four map the same values to all 27 Hebrew letters 
 * (including final letters).
 *
 * The dominant difference between these charsets is their directionality.
 * "Visual" directionality means that the text is ordered as if the renderer is
 * not aware of a BIDI rendering algorithm. The renderer sees the text and 
 * draws it from left to right. The text itself when ordered naturally is read 
 * backwards. A buffer of Visual Hebrew generally looks like so:
 * "[last word of first line spelled backwards] [whole line ordered backwards
 * and spelled backwards] [first word of first line spelled backwards] 
 * [end of line] [last word of second line] ... etc' "
 * adding punctuation marks, numbers and English text to visual text is
 * naturally also "visual" and from left to right.
 * 
 * "Logical" directionality means the text is ordered "naturally" according to
 * the order it is read. It is the responsibility of the renderer to display
 * the text from right to left. A BIDI algorithm is used to place general 
 * punctuation marks, numbers and English text in the text.
 *
 * Texts in x-mac-hebrew are almost impossible to find on the Internet. From 
 * what little evidence I could find, it seems that its general directionality
 * is Logical.
 *
 * To sum up all of the above, the Hebrew probing mechanism knows about two
 * charsets:
 * Visual Hebrew - "ISO-8859-8" - backwards text - Words and sentences are
 *    backwards while line order is natural. For charset recognition purposes
 *    the line order is unimportant (In fact, for this implementation, even 
 *    word order is unimportant).
 * Logical Hebrew - "windows-1255" - normal, naturally ordered text.
 *
 * "ISO-8859-8-I" is a subset of windows-1255 and doesn't need to be
 *    specifically identified.
 * "x-mac-hebrew" is also identified as windows-1255. A text in x-mac-hebrew
 *    that contain special punctuation marks or diacritics is displayed with
 *    some unconverted characters showing as question marks. This problem might
 *    be corrected using another model prober for x-mac-hebrew. Due to the fact
 *    that x-mac-hebrew texts are so rare, writing another model prober isn't
 *    worth the effort and performance hit.
 *
 * *** The Prober ***
 *
 * The prober is divided between two nsSBCharSetProbers and an nsHebrewProber,
 * all of which are managed, created, fed data, inquired and deleted by the
 * nsSBCSGroupProber. The two nsSBCharSetProbers identify that the text is in
 * fact some kind of Hebrew, Logical or Visual. The final decision about which
 * one is it is made by the nsHebrewProber by combining final-letter scores
 * with the scores of the two nsSBCharSetProbers to produce a final answer.
 *
 * The nsSBCSGroupProber is responsible for stripping the original text of HTML
 * tags, English characters, numbers, low-ASCII punctuation characters, spaces
 * and new lines. It reduces any sequence of such characters to a single space.
 * The buffer fed to each prober in the SBCS group prober is pure text in
 * high-ASCII.
 * The two nsSBCharSetProbers (model probers) share the same language model:
 * Win1255Model.
 * The first nsSBCharSetProber uses the model normally as any other
 * nsSBCharSetProber does, to recognize windows-1255, upon which this model was
 * built. The second nsSBCharSetProber is told to make the pair-of-letter
 * lookup in the language model backwards. This in practice exactly simulates
 * a visual Hebrew model using the windows-1255 logical Hebrew model.
 *
 * The nsHebrewProber is not using any language model. All it does is look for
 * final-letter evidence suggesting the text is either logical Hebrew or visual
 * Hebrew. Disjointed from the model probers, the results of the nsHebrewProber
 * alone are meaningless. nsHebrewProber always returns 0.00 as confidence
 * since it never identifies a charset by itself. Instead, the pointer to the
 * nsHebrewProber is passed to the model probers as a helper "Name Prober".
 * When the Group prober receives a positive identification from any prober,
 * it asks for the name of the charset identified. If the prober queried is a
 * Hebrew model prober, the model prober forwards the call to the
 * nsHebrewProber to make the final decision. In the nsHebrewProber, the
 * decision is made according to the final-letters scores maintained and Both
 * model probers scores. The answer is returned in the form of the name of the
 * charset identified, either "windows-1255" or "ISO-8859-8".
 *
 *)
end.
