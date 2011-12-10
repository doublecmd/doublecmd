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
// $Id: nsCore.pas,v 1.4 2008/06/22 09:04:20 ya_nick Exp $

unit nsCore;

interface

type
  PRInt16 = smallint;
  PRUint16 = word;
  PRInt32 = integer;
  PRUint32 = cardinal;

  pByteArray = array of Byte;
  pPRUint32 = ^PRUint32;
  aPRUint32 = array of PRUint32;

  pPRint16 = ^PRint16;
  aPRint16 = array of PRint16;

const
  SURE_YES: double = 0.99;
  SURE_NO: double  = 0.01;
const
	ENOUGH_DATA_THRESHOLD: cardinal = 1024;
	SHORTCUT_THRESHOLD = 0.95;

type
  eProbingState = (
    psDetecting = 0,   //We are still detecting, no sure answer yet, but caller can ask for confidence.
    psFoundIt   = 1,   //That's a positive answer
    psNotMe     = 2    //Negative answer
	);

type
	nsResult = PRUint32;
const
  NS_OK = 0;
  NS_ERROR_OUT_OF_MEMORY = $8007000e;

type
	float = double;

  rAboutHolder = record
    MajorVersionNr: Cardinal;
    MinorVersionNr: Cardinal;
    BuildVersionNr: Cardinal;
    About: pChar;
  end;

  eBOMKind = (
    BOM_Not_Found,
    BOM_UCS4_BE,    // 00 00 FE FF           UCS-4,    big-endian machine    (1234 order)
    BOM_UCS4_LE,    // FF FE 00 00           UCS-4,    little-endian machine (4321 order)
    BOM_UCS4_2143,  // 00 00 FF FE           UCS-4,    unusual octet order   (2143)
    BOM_UCS4_3412,  // FE FF 00 00           UCS-4,    unusual octet order   (3412)
    BOM_UTF16_BE,   // FE FF ## ##           UTF-16,   big-endian
    BOM_UTF16_LE,   // FF FE ## ##           UTF-16,   little-endian
    BOM_UTF8        // EF BB BF              UTF-8
  );

const
  KnownBOM: array [eBOMKind] of array [0..4] of Char = (
  // first element = byte count
    (#$00, #$00, #$00, #$00, #$00),
    (#$04, #$00, #$00, #$FE, #$FF),
    (#$04, #$FF, #$FE, #$00, #$00),
    (#$04, #$00, #$00, #$FF, #$FE),
    (#$04, #$FE, #$FF, #$00, #$00),
    (#$02, #$FE, #$FF, #$00, #$00),
    (#$02, #$FF, #$FE, #$00, #$00),
    (#$03, #$EF, #$BB, #$BF, #$00)
  );

// "extended" charset info
type
	rCharsetInfo = record
  	Name: pChar;
    CodePage: integer;
    Language: pChar;
  end;

  eInternalCharsetID = (
    UNKNOWN_CHARSET           = 000,
    PURE_ASCII_CHARSET        = 001,
    UTF8_CHARSET              = 002,
    UCS4_BE_CHARSET           = 003,
    UTF16_BE_CHARSET          = 004,
    UTF32_BE_CHARSET          = 005,
    UCS4_LE_CHARSET           = 006,
    UTF32_LE_CHARSET          = 007,
    UTF16_LE_CHARSET          = 008,
    LATIN5_BULGARIAN_CHARSET  = 009,
    WINDOWS_BULGARIAN_CHARSET = 010,
    KOI8_R_CHARSET            = 011,
    WINDOWS_1251_CHARSET      = 012,
    ISO_8859_5_CHARSET        = 013,
    X_MAC_CYRILLIC_CHARSET    = 014,
    IBM866_CHARSET            = 015,
    IBM855_CHARSET            = 016,
    ISO_8859_7_CHARSET        = 017,
    WINDOWS_1253_CHARSET      = 018,
    ISO_8859_8_CHARSET        = 019,
    WINDOWS_1255_CHARSET      = 020,
    BIG5_CHARSET              = 021,
    ISO_2022_CN_CHARSET       = 022,
    ISO_2022_JP_CHARSET       = 023,
    ISO_2022_KR_CHARSET       = 024,
    EUC_JP_CHARSET            = 025,
    EUC_KR_CHARSET            = 026,
    X_EUC_TW_CHARSET          = 027,
    SHIFT_JIS_CHARSET         = 028,
    GB18030_CHARSET           = 029,
    HZ_GB_2312_CHARSET        = 030,
    WINDOWS_1252_CHARSET      = 031
  );

const
  KNOWN_CHARSETS: array [eInternalCharsetID] of rCharsetInfo = (
	// UNKNOWN_CHARSET
    (
      Name: 'Unknown';
      CodePage: -1;
      Language: 'Unknown'
    ),
  // PURE_ASCII_CHARSET
    (
      Name: 'ASCII';
      CodePage: 0;
      Language: 'ASCII'
    ),
  // UTF8_CHARSET
    (
      Name: 'UTF-8';
      CodePage: 65001;
      Language: 'Unicode'
    ),
  // UCS4_BE_CHARSET
    (
      Name: 'X-ISO-10646-UCS-4-3412';
      CodePage: 12001;
      Language: 'Unicode'
    ),
  // UTF16_BE_CHARSET
    (
      Name: 'UTF-16BE';
      CodePage: 1201;
      Language: 'Unicode'
    ),
  // UTF32_BE_CHARSET
    (
      Name: 'UTF-32BE';
      CodePage: 12001;
      Language: 'Unicode'
    ),
  // UCS4_LE_CHARSET
    (
      Name: 'X-ISO-10646-UCS-4-2143';
      CodePage: 12000;
      Language: 'Unicode'
    ),
  // UTF32_LE_CHARSET
    (
      Name: 'UTF-32LE';
      CodePage: 12000;
      Language: 'Unicode'
    ),
  // UTF16_LE_CHARSET
    (
      Name: 'UTF-16LE';
      CodePage: 1200;
      Language: 'Unicode'
    ),
	// LATIN5_BULGARIAN_CHARSET
    (
      Name: 'ISO-8859-5';
      CodePage: 28595;
      Language: 'Bulgarian'
    ),
  // WINDOWS_BULGARIAN_CHARSET
    (
      Name: 'windows-1251';
      CodePage: 1251;
      Language: 'Bulgarian'
    ),
  // KOI8_R_CHARSET
    (
      Name: 'KOI8-R';
      CodePage: 20866;
      Language: 'russian'
    ),
  // WINDOWS_1251_CHARSET
    (
      Name: 'windows-1251';
      CodePage: 1251;
      Language: 'russian'
    ),
  // ISO_8859_5_CHARSET
    (
      Name: 'ISO-8859-5';
      CodePage: 28595;
      Language: 'russian'
    ),
  // X_MAC_CYRILLIC_CHARSET
    (
      Name: 'x-mac-cyrillic';
      CodePage: 10007;
      Language: 'russian'
    ),
  // IBM866_CHARSET
    (
      Name: 'IBM866';
      CodePage: 866;
      Language: 'russian'
    ),
  // IBM855_CHARSET
    (
      Name: 'IBM855';
      CodePage: 855;
      Language: 'russian'
    ),
  //  ISO_8859_7_CHARSET
    (
      Name: 'ISO-8859-7';
      CodePage: 28597;
      Language: 'greek'
    ),
  // WINDOWS_1253_CHARSET
    (
      Name: 'windows-1253';
      CodePage: 1253;
      Language: 'greek'
    ),
  // ISO_8859_8_CHARSET
    (
      Name: 'ISO-8859-8';
      CodePage: 28598;
      Language: 'hebrew'
    ),
  // WINDOWS_1255_CHARSET
    (
      Name: 'windows-1255';
      CodePage: 1255;
      Language: 'hebrew'
    ),
  // BIG5_CHARSET
    (
      Name: 'Big5';
      CodePage: 950;
      Language: 'ch'
       ),
  // ISO_2022_CN_CHARSET
    (
      Name:  'ISO-2022-CN';
      CodePage:  50227;
      Language:  'ch';
      ),
  // ISO_2022_JP_CHARSET
    (
      Name:  'ISO-2022-JP';
      CodePage:  50222;
      Language:  'japanese';
    ),
  // ISO_2022_KR_CHARSET
    (
      Name:  'ISO-2022-KR';
      CodePage:  50225;
      Language:  'kr';
    ),
  // EUC_JP_CHARSET
    (
      Name:  'EUC-JP';
      CodePage:  51932;
      Language:  'japanese';
    ),
  // EUC_KR_CHARSET
    (
      Name:  'EUC-KR';
      CodePage:  51949;
      Language:  'kr';
    ),
  // X_EUC_TW_CHARSET
    (
      Name:  'x-euc-tw';
      CodePage:  51936;
      Language:  'ch';
    ),
  // SHIFT_JIS_CHARSET
    (
      Name:  'Shift_JIS';
      CodePage:  932;
      Language:  'japanese';
    ),
  // GB18030_CHARSET
    (
      Name:  'GB18030';
      CodePage:  54936;
      Language:  'ch';
    ),
  // HZ_GB_2312_CHARSET
    (
      Name:  'HZ-GB-2312';
      CodePage:  52936;
      Language:  'ch';
    ),
  // WINDOWS_1252_CHARSET
    (
      Name:  'windows-1252';
      CodePage:  1252;
      Language:  'eu';
    )

  );

  (* Helper functions used in the Latin1 and Group probers.*)
  (* both functions Allocate a new buffer for newBuf. This buffer should be *)
  (* freed by the caller using PR_FREEIF.*)
  (* Both functions return PR_FALSE in case of memory allocation failure.*)
  function FilterWithoutEnglishLetters(aBuf: PChar;  aLen: integer; var newBuf: PChar; var newLen: integer): Boolean;
  function FilterWithEnglishLetters(aBuf: PChar;  aLen: integer; var newBuf: PChar; var newLen: integer): Boolean;
implementation

function FilterWithEnglishLetters(aBuf: PChar;
  aLen: integer; var newBuf: PChar; var newLen: integer): Boolean;
var
  newptr: pChar;
  prevPtr: pChar;
  curPtr: pChar;
  isInTag: Boolean;
begin
  //do filtering to reduce load to probers
  isInTag := FALSE;
  newLen := 0;

  newptr := newBuf;
  if (newptr = nil) then
  	begin
    	Result := FALSE;
      exit;
    end;

  prevPtr := aBuf;
  curPtr := prevPtr;
  while (curPtr < aBuf+aLen) do
  begin
    if (curPtr^ = '>') then
      isInTag := FALSE
    else
    	if (curPtr^ = '<') then
      	isInTag := TRUE;

    if ((curPtr^ < #$80) and
        ((curPtr^ < 'A') or ((curPtr^ > 'Z') and (curPtr^ < 'a')) or (curPtr^ > 'z')) ) then
      begin
        if ((curPtr > prevPtr) and (not isInTag)) then 	// Current segment contains more than just a symbol
                               		           		 				// and it is not inside a tag, keep it.
          begin
            while (prevPtr < curPtr) do
            	begin
              	newptr^ := prevPtr^;
              	inc(newptr);
              	inc(prevPtr);
              end;
            inc(prevPtr);
            newptr^ := ' ';
            inc(newptr);
          end
        else
          prevPtr := curPtr+1;
      end;
  	inc(curPtr);
  end;

  // If the current segment contains more than just a symbol
  // and it is not inside a tag then keep it.
  if ( not isInTag) then
    while (prevPtr < curPtr) do
    	begin
        newptr^ := prevPtr^;
        inc(newptr);
        inc(prevPtr);
      end;

  newLen := newptr - newBuf;

  Result := TRUE;
end;

function FilterWithoutEnglishLetters(aBuf: PChar;
  aLen: integer; var newBuf: PChar; var newLen: integer): Boolean;
var
	newPtr: pChar;
  prevPtr: pChar;
  curPtr: pChar;
  meetMSB: Boolean;
begin
(*This filter applies to all scripts which do not use English characters*)
	Result := FALSE;
  newLen := 0;
  meetMSB:= FALSE;

  if newBuf = nil then
    exit;

  newPtr := newBuf;
  curPtr := aBuf;
  prevPtr := curPtr;

  while curPtr < aBuf+aLen do
    begin
      if curPtr^ > #$80 then
        meetMSB := TRUE
      else
        if ((curPtr^ < 'A') or ((curPtr^ > 'Z') and (curPtr^ < 'a')) or (curPtr^ > 'z')) then
          begin
            //current char is a symbol, most likely a punctuation. we treat it as segment delimiter
            if (meetMSB and (curPtr > prevPtr)) then
            //this segment contains more than single symbol, and it has upper ASCII, we need to keep it
              begin
                while (prevPtr < curPtr) do
                  begin
                    newptr^ := prevPtr^;
                    inc(newptr);
                    inc(prevPtr);
                  end;
                inc(prevPtr);
                newptr^ := ' ';
                inc(newptr);
                meetMSB := FALSE;
            	end
          	else //ignore current segment. (either because it is just a symbol or just an English word)
            	prevPtr := curPtr+1;
         end;
      inc(curPtr);
    end;
  if (meetMSB and (curPtr > prevPtr)) then
    while (prevPtr < curPtr) do
    	begin
        newptr^ := prevPtr^;
        inc(newptr);
        inc(prevPtr);
      end;

  newLen := newptr - newBuf;

  Result := TRUE;
end;

end.





