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
// $Id: nsUniversalDetector.pas,v 1.5 2008/06/22 09:04:20 ya_nick Exp $

unit nsUniversalDetector;

interface
uses
  {$I dbg.inc}
	nsCore,
  CustomDetector;


const
	NUM_OF_CHARSET_PROBERS = 4;

type nsInputState = (
  ePureAscii = 0,
  eEscAscii  = 1,
  eHighbyte  = 2
	) ;

	TnsUniversalDetector  = class (TObject)
    protected
      mInputState: nsInputState;
      mDone: Boolean;
      mStart: Boolean;
      mGotData: Boolean;
      mLastChar: Char;
      mDetectedCharset: eInternalCharsetID;
      mCharSetProbers: array [0..Pred(NUM_OF_CHARSET_PROBERS)] of TCustomDetector;
      mEscCharSetProber: TCustomDetector;
      mDetectedBOM: eBOMKind;

		  procedure Report(aCharsetID: eInternalCharsetID);
      function CheckBOM(aBuf: pChar; aLen: integer): integer;
      function GetCharsetID(CodePage: integer): eInternalCharsetID;
      procedure DoEnableCharset(Charset: eInternalCharsetID; SetEnabledTo: Boolean);
		public
    	constructor Create;
      destructor Destroy; override;

		  procedure Reset;
		  function HandleData(aBuf: PChar; aLen: integer): nsResult;
		  procedure DataEnd;

      function GetDetectedCharsetInfo: nsCore.rCharsetInfo;

      function GetKnownCharset(out KnownCharsets: pChar): integer;
      procedure GetAbout(out About: rAboutHolder);
      procedure DisableCharset(CodePage: integer);

      property Done: Boolean read mDone;
      property BOMDetected: eBOMKind read mDetectedBOM;
end;

implementation
uses
  SysUtils,
  nsGroupProber,
	nsMBCSMultiProber,
	nsSBCSGroupProber,
	nsEscCharsetProber,
	nsLatin1Prober,
  MBUnicodeMultiProber;


const
	MINIMUM_THRESHOLD: float  = 0.20;

  AboutInfo: rAboutHolder = (
    MajorVersionNr: 0;
    MinorVersionNr: 2;
    BuildVersionNr: 6;
    About: 'Charset Detector Library. Copyright (C) 2006 - 2008, Nick Yakowlew. http://chsdet.sourceforge.net';
  );
{ TnsUniversalDetector }

constructor TnsUniversalDetector.Create;
begin
	inherited Create;

  mCharSetProbers[0] := TnsMBCSMultiProber.Create;
  mCharSetProbers[1] := TnsSBCSGroupProber.Create;
  mCharSetProbers[2] := TnsLatin1Prober.Create;
  mCharSetProbers[3] := TMBUnicodeMultiProber.Create;
  mEscCharSetProber  := TnsEscCharSetProber.Create;
  Reset;
end;

destructor TnsUniversalDetector.Destroy;
var
  i: integer;
begin
	for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
    mCharSetProbers[i].Free;

  mEscCharSetProber.Free;

  inherited;
end;

procedure TnsUniversalDetector.DataEnd;
var
	proberConfidence: float;
  maxProberConfidence: float;
  maxProber: PRInt32;
  i: integer;
begin
  if not mGotData then
    (* we haven't got any data yet, return immediately *)
    (* caller program sometimes call DataEnd before anything has been sent to detector*)
    exit;

  if mDetectedCharset <> UNKNOWN_CHARSET then
    begin
      mDone := TRUE;
      Report(mDetectedCharset);
      exit;
    end;
  case mInputState of
    eHighbyte:
      begin
        maxProberConfidence := 0.0;
        maxProber := 0;
        for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
          begin
            proberConfidence := mCharSetProbers[i].GetConfidence;
            if proberConfidence > maxProberConfidence then
            begin
              maxProberConfidence := proberConfidence;
              maxProber := i;
            end;
          end;
        (*do not report anything because we are not confident of it, that's in fact a negative answer*)
        if maxProberConfidence > MINIMUM_THRESHOLD then
	        Report(mCharSetProbers[maxProber].GetDetectedCharset);
      end;
    eEscAscii:
	    begin
        mDetectedCharset := mEscCharSetProber.GetDetectedCharset;
      end;
    else
      begin
      	mDetectedCharset := PURE_ASCII_CHARSET;
      end;
  end;{case}
  {$ifdef DEBUG_chardet}
  AddDump('Universal detector - DataEnd');
  {$endif}
end;

function TnsUniversalDetector.HandleData(aBuf: PChar; aLen: integer): nsResult;
var
  i: integer;
  st: eProbingState;
//  startAt: integer;
//newBuf: pChar;
//BufPtr: pChar;
//b: integer;
//tmpBOM: eBOMKind;
begin
//  startAt := 0;
  if mDone then
    begin
      Result := NS_OK;
      exit;
    end;
  if aLen > 0 then
	  mGotData := TRUE;

  (*If the data starts with BOM, we know it is Unicode*)
  if mStart then
    begin
      mStart := FALSE;
//      startAt := CheckBOM(aBuf, aLen);
      CheckBOM(aBuf, aLen);
//     case mDetectedBOM of
//        BOM_UCS4_BE: mDetectedCharset   := UCS4_BE_CHARSET;
//        BOM_UCS4_LE: mDetectedCharset   := UCS4_LE_CHARSET;
//        BOM_UTF16_BE: mDetectedCharset  := UTF16_BE_CHARSET;
//        BOM_UTF16_LE: mDetectedCharset  := UTF16_LE_CHARSET;
//        BOM_UTF8: mDetectedCharset      := UTF8_CHARSET;
//
//        BOM_UCS4_2143: mDetectedCharset := UCS4_BE_CHARSET;
//        BOM_UCS4_3412: mDetectedCharset := UCS4_LE_CHARSET;
//      end;
// TODO - some stuppid ASCII text can starts with BOM. What to do?
      if mDetectedCharset <> UNKNOWN_CHARSET then
        begin
//          mDone := TRUE;
//         Result := NS_OK;
//          exit;
        end;
    end; {if mStart}

  for i := 0 to Pred(aLen) do
    (*other than 0xa0, if every othe character is ascii, the page is ascii*)
    if (aBuf[i] > #$80) and (aBuf[i] <> #$A0) then
      begin
        (*Since many Ascii only page contains NBSP *)
        (*we got a non-ascii byte (high-byte)*)
        if mInputState <> eHighbyte then
          begin
            (*adjust state*)
            mInputState := eHighbyte;
          end;
      end
    else
      begin
        (*ok, just pure ascii so *)
        if (mInputState = ePureAscii) and
        	 ((aBuf[i] = #$1B) or
           	(aBuf[i] = '{') and
            (mLastChar = '~')) then
          (*found escape character or HZ "~{"*)
          mInputState := eEscAscii;

        mLastChar := aBuf[i];
      end;

  case mInputState of
    eEscAscii:
      begin
        {$ifdef DEBUG_chardet}
        AddDump('Universal detector - Escape Detector started');
        {$endif}
        st := mEscCharSetProber.HandleData(aBuf,aLen);
        if st = psFoundIt then
          begin
            mDone := TRUE;
            mDetectedCharset := mEscCharSetProber.GetDetectedCharset;
          end;
      end;
    eHighbyte:
      begin
        {$ifdef DEBUG_chardet}
        AddDump('Universal detector - HighByte Detector started');
        {$endif}
        for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
          begin
//newBuf := AllocMem(aLen+StartAt);
//BufPtr := newBuf;
//try
//tmpBOM := BOM_Not_Found;
//if mDetectedBOM = BOM_Not_Found then
//begin
////case mCharSetProbers[i].GetDetectedCharset of
//// UTF16_BE_CHARSET: tmpBOM := BOM_UCS4_BE;
//// UTF16_LE_CHARSET: tmpBOM := BOM_UCS4_LE;
//// else
////  tmpBOM := BOM_Not_Found;
////end;
//tmpBOM := BOM_UTF16_BE;
//end;
//for b:=0 to integer(KnownBOM[tmpBOM][0])-1 do
//begin
//BufPtr^ := KnownBOM[tmpBOM][b+1];
//inc(BufPtr);
//end;
//
//for b:=0 to aLen do
//begin
//BufPtr^ := aBuf[b];
//inc(BufPtr);
//end;
          st := mCharSetProbers[i].HandleData(aBuf,aLen);
//          st := mCharSetProbers[i].HandleData(newBuf,aLen+startAt);
          if st = psFoundIt then
            begin
              mDone:= TRUE;
              mDetectedCharset := mCharSetProbers[i].GetDetectedCharset;
//              Result := NS_OK;
              break;
            end;
//finally
//FreeMem(newBuf, aLen);
//end;
        end;
      end;
    else
    (*pure ascii*)
    begin
      (*do nothing here*)
    end;
  end;{case}
  Result := NS_OK;
end;

procedure TnsUniversalDetector.Report(aCharsetID: eInternalCharsetID);
begin

	if (aCharsetID <> UNKNOWN_CHARSET) and
  	 (mDetectedCharset = UNKNOWN_CHARSET) then

  mDetectedCharset := aCharsetID;  
end;

procedure TnsUniversalDetector.Reset;
var
  i: integer;
begin
  mDone := FALSE;
  mStart := TRUE;
  mDetectedCharset := UNKNOWN_CHARSET;
  mGotData := FALSE;
  mInputState := ePureAscii;
  mLastChar := #0; (*illegal value as signal*)
  mEscCharSetProber.Reset;
  for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
	  mCharSetProbers[i].Reset;
  mDetectedBOM := BOM_Not_Found;
end;

function TnsUniversalDetector.GetDetectedCharsetInfo: nsCore.rCharsetInfo;
begin
  Result := KNOWN_CHARSETS[mDetectedCharset];
end;

function TnsUniversalDetector.GetKnownCharset(out KnownCharsets: pChar): integer;
var
  s: ANSIstring;
  i: integer;
begin
  s := '';
  for i := integer(low(KNOWN_CHARSETS)) to integer(High(KNOWN_CHARSETS)) do
    s := s + #10 + KNOWN_CHARSETS[eInternalCharsetID(i)].Name +
             ' - ' + inttostr(KNOWN_CHARSETS[eInternalCharsetID(i)].CodePage);

  KnownCharsets := pChar(s);
  Result := Length(s);
end;

procedure TnsUniversalDetector.GetAbout(out About: rAboutHolder);
begin
  About := AboutInfo;
end;

function TnsUniversalDetector.CheckBOM(aBuf: pChar; aLen: integer): integer;
  function BOMLength(BOM: eBOMKind): integer;
  begin
    Result := integer(KnownBOM[BOM, 0]);
  end;
var
  i, b: integer;
  Same: Boolean;
begin
  Result := 0;
  for i := integer(low(KnownBOM))+1 to integer(high(KnownBOM)) do
    if aLen > BOMLength(eBOMKind(i)) then
      begin
        Same := true;
        for b := 0 to BOMLength(eBOMKind(i)) - 1 do
          if (aBuf[b] <> KnownBOM[eBOMKind(i), b+1]) then
            begin
              Same := false;
              break;
            end;
        if Same then
          begin
            mDetectedBOM := eBOMKind(i);
            Result := BOMLength(mDetectedBOM);
            exit;
          end;
      end;
end;

procedure TnsUniversalDetector.DisableCharset(CodePage: integer);
begin
  DoEnableCharset(GetCharsetID(CodePage), false);
end;

function TnsUniversalDetector.GetCharsetID(CodePage: integer): eInternalCharsetID;
var
  i: integer;
begin
  for i := integer(low(KNOWN_CHARSETS))+1 to integer(high(KNOWN_CHARSETS)) do
    if (KNOWN_CHARSETS[eInternalCharsetID(i)].CodePage = CodePage) then
      begin
        Result := eInternalCharsetID(i);
        exit;
      end;
  Result := UNKNOWN_CHARSET;
end;

procedure TnsUniversalDetector.DoEnableCharset(Charset: eInternalCharsetID; SetEnabledTo: Boolean);
var
  i: integer;
begin
  if Charset = UNKNOWN_CHARSET then
    exit;
  for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
    begin
      if (mCharSetProbers[i] is TnsGroupProber) then
        begin
          if TnsGroupProber(mCharSetProbers[i]).EnableCharset(Charset, SetEnabledTo) then
            exit;
        end;
      if (mCharSetProbers[i] is TnsEscCharSetProber) then
        begin
          TnsEscCharSetProber(mCharSetProbers[i]).Enabled := SetEnabledTo;
        end;
      if (mCharSetProbers[i] is TCustomDetector) then
        begin
          if TCustomDetector(mCharSetProbers[i]).GetDetectedCharset = Charset then
            TCustomDetector(mCharSetProbers[i]).Enabled := SetEnabledTo;
        end;
    end;

end;                                                                    

end.





