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
// $Id: nsUniversalDetector.pas,v 1.7 2013/05/16 15:41:14 ya_nick Exp $

unit nsUniversalDetector;

interface
uses
{$I dbg.inc}
  nsCore,
  CustomDetector;

const
  NUM_OF_CHARSET_PROBERS = 4;

type
  eInputState = (
    isPureAscii = 0,
    isEscAscii = 1,
    isHighbyte = 2
    );

  TnsUniversalDetector = class(TObject)
  protected
    mInputState: eInputState;
    mDone: Boolean;
    mStart: Boolean;
    mGotData: Boolean;
    mLastChar: AnsiChar;
    mDetectedCharset: eInternalCharsetID;
    mCharSetProbers: array[0..Pred(NUM_OF_CHARSET_PROBERS)] of TCustomDetector;
    mEscCharSetProber: TCustomDetector;
    mDetectedBOM: eBOMKind;

    procedure Report(aCharsetID: eInternalCharsetID);
    function CheckBOM(aBuf: pAnsiChar; aLen: integer): integer;
    function GetCharsetID(CodePage: integer): eInternalCharsetID;
    procedure DoEnableCharset(Charset: eInternalCharsetID; SetEnabledTo: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    function HandleData(aBuf: pAnsiChar; aLen: integer): nsResult;
    procedure DataEnd;

    function GetDetectedCharsetInfo: nsCore.rCharsetInfo;

    function GetKnownCharset(out KnownCharsets: String): integer;
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
  MINIMUM_THRESHOLD: float = 0.20;

  AboutInfo: rAboutHolder = (
    MajorVersionNr: 0;
    MinorVersionNr: 2;
    BuildVersionNr: 8;
    About: 'Charset Detector Library. Copyright (C) 2006 - 2013, Nick Yakowlew. http://chsdet.sourceforge.net';
  );
  { TnsUniversalDetector }

constructor TnsUniversalDetector.Create;
begin
  inherited Create;

  mCharSetProbers[0] := TnsMBCSMultiProber.Create;
  mCharSetProbers[1] := TnsSBCSGroupProber.Create;
  mCharSetProbers[2] := TnsLatin1Prober.Create;
  mCharSetProbers[3] := TMBUnicodeMultiProber.Create;
  mEscCharSetProber := TnsEscCharSetProber.Create;
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
  maxProber: int32;
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
    isHighbyte:
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
    isEscAscii:
      begin
        mDetectedCharset := mEscCharSetProber.GetDetectedCharset;
      end;
  else
    begin
      mDetectedCharset := PURE_ASCII_CHARSET;
    end;
  end;                                  {case}
{$IFDEF DEBUG_chardet}
  AddDump('Universal detector - DataEnd');
{$ENDIF}
end;

function TnsUniversalDetector.HandleData(aBuf: pAnsiChar; aLen: integer): nsResult;
var
  i: integer;
  st: eProbingState;
begin
  if mDone then
    begin
      Result := NS_OK;
      exit;
    end;
  if aLen > 0 then
    mGotData := TRUE;

  (*If the data starts with BOM, we know it is UTF*)
  if mStart then
    begin
      mStart := FALSE;
      if CheckBOM(aBuf, aLen) > 0 then
      begin
        case mDetectedBOM of
          BOM_UTF8:      mDetectedCharset := UTF8_CHARSET;
          BOM_UTF16_LE:  mDetectedCharset := UTF16_LE_CHARSET;
          BOM_UTF16_BE:  mDetectedCharset := UTF16_BE_CHARSET;
          BOM_UCS4_LE:   mDetectedCharset := UTF32_LE_CHARSET;
          BOM_UCS4_BE:   mDetectedCharset := UTF32_BE_CHARSET;
          BOM_UCS4_2143: mDetectedCharset := UCS4_LE_CHARSET;
          BOM_UCS4_3412: mDetectedCharset := UCS4_BE_CHARSET
        end;
        mDone := TRUE;
        Result := NS_OK;
        Exit;
      end;
    end;                                {if mStart}

  for i := 0 to Pred(aLen) do
    (*other than 0xa0, if every othe character is ascii, the page is ascii*)
    if (aBuf[i] > #$80) and (aBuf[i] <> #$A0) then
      begin
        (*Since many Ascii only page contains NBSP *)
        (*we got a non-ascii byte (high-byte)*)
        if mInputState <> isHighbyte then
          begin
            (*adjust state*)
            mInputState := isHighbyte;
          end;
      end
    else
      begin
        (*ok, just pure ascii so *)
        if (mInputState = isPureAscii) and
          ((aBuf[i] = #$1B) or
          (aBuf[i] = '{') and
          (mLastChar = '~')) then
          (*found escape character or HZ "~{"*)
          mInputState := isEscAscii;

        mLastChar := aBuf[i];
      end;

  case mInputState of
    isEscAscii:
      begin
{$IFDEF DEBUG_chardet}
        AddDump('Universal detector - Escape Detector started');
{$ENDIF}
        st := mEscCharSetProber.HandleData(aBuf, aLen);
        if st = psFoundIt then
          begin
            mDone := TRUE;
            mDetectedCharset := mEscCharSetProber.GetDetectedCharset;
          end;
      end;
    isHighbyte:
      begin
{$IFDEF DEBUG_chardet}
        AddDump('Universal detector - HighByte Detector started');
{$ENDIF}
        for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
          begin
            st := mCharSetProbers[i].HandleData(aBuf, aLen);
            if st = psFoundIt then
              begin
                mDone := TRUE;
                mDetectedCharset := mCharSetProbers[i].GetDetectedCharset;
                break;
              end;
          end;
      end;
  else
    (*pure ascii*)
    begin
      (*do nothing here*)
    end;
  end;                                  {case}
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
  mInputState := isPureAscii;
  mLastChar := #0;                      (*illegal value as signal*)
  mEscCharSetProber.Reset;
  for i := 0 to Pred(NUM_OF_CHARSET_PROBERS) do
    mCharSetProbers[i].Reset;
  mDetectedBOM := BOM_Not_Found;
end;

function TnsUniversalDetector.GetDetectedCharsetInfo: nsCore.rCharsetInfo;
begin
  Result := KNOWN_CHARSETS[mDetectedCharset];
end;

function TnsUniversalDetector.GetKnownCharset(out KnownCharsets: String): integer;
var
  i: eInternalCharsetID;
begin
  KnownCharsets := '';
  for i := low(KNOWN_CHARSETS) to high(KNOWN_CHARSETS) do
    KnownCharsets := KnownCharsets + #10 + KNOWN_CHARSETS[i].Name +
      ' - ' + IntToStr(KNOWN_CHARSETS[i].CodePage);

  Result := Length(KnownCharsets);
end;

procedure TnsUniversalDetector.GetAbout(out About: rAboutHolder);
begin
  About := AboutInfo;
end;

function TnsUniversalDetector.CheckBOM(aBuf: pAnsiChar; aLen: integer): integer;
var
  bom: eBOMKind;
  i: integer;
  same: Boolean;
begin
  Result := 0;
  mDetectedBOM := BOM_Not_Found;
  for bom := Succ(low(eBOMKind)) to high(eBomKind) do
    if aLen > KNOWN_BOM[bom].Length then
      begin
        same := true;
        for i := 0 to KNOWN_BOM[bom].Length - 1 do
          if (aBuf[i] <> KNOWN_BOM[bom].BOM[i]) then
            begin
              same := false;
              break;
            end;
        if same then
          begin
            mDetectedBOM := bom;
            Result := KNOWN_BOM[bom].Length;
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
  for i := integer(low(KNOWN_CHARSETS)) + 1 to integer(high(KNOWN_CHARSETS)) do
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

