{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclUnicode.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Mike Lischke (public att lischke-online dott de).  }
{ Portions created by Mike Lischke are Copyright (C) 1999-2000 Mike Lischke. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Andreas Hausladen (ahuser)                                                                     }
{   Mike Lischke                                                                                   }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Peter Schraut (http://www.console-dev.de)                                                      }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ String conversion routines                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStringConversions;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase;

type
  EJclStringConversionError = class(EJclError);
  EJclUnexpectedEOSequenceError = class (EJclStringConversionError)
  public
    constructor Create;
  end;

// conversion routines between Ansi, UTF-16, UCS-4 and UTF8 strings

// one shot conversion between PAnsiChar and PWideChar
procedure ExpandASCIIString(const Source: PAnsiChar; Target: PWideChar; Count: SizeInt);

// tpye of stream related functions
type
  TJclStreamGetNextCharFunc = function(S: TStream; out Ch: UCS4): Boolean;
  TJclStreamSkipCharsFunc = function(S: TStream; var NbSeq: SizeInt): Boolean;
  TJclStreamSetNextCharFunc = function(S: TStream; Ch: UCS4): Boolean;

// iterative conversions

// UTF8GetNextChar = read next UTF8 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: TUTF8String; var StrPos: SizeInt): UCS4;
function UTF8GetNextBuffer(const S: TUTF8String; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
function UTF8GetNextCharFromStream(S: TStream; out Ch: UCS4): Boolean;
function UTF8GetNextBufferFromStream(S: TStream; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;

// UTF8SkipChars = skip NbSeq UTF8 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: TUTF8String; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
function UTF8SkipCharsFromStream(S: TStream; var NbSeq: SizeInt): Boolean;

// UTF8SetNextChar = append an UTF8 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: SizeInt; Ch: UCS4): Boolean;
function UTF8SetNextBuffer(var S: TUTF8String; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
function UTF8SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
function UTF8SetNextBufferToStream(S: TStream; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;

// UTF16GetNextChar = read next UTF16 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: TUTF16String; var StrPos: SizeInt): UCS4; overload;
function UTF16GetNextBuffer(const S: TUTF16String; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetNextChar(const S: WideString; var StrPos: SizeInt): UCS4; overload;
function UTF16GetNextBuffer(const S: WideString; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function UTF16GetNextCharFromStream(S: TStream; out Ch: UCS4): Boolean;
function UTF16GetNextBufferFromStream(S: TStream; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;

// UTF16GetPreviousChar = read previous UTF16 sequence starting at StrPos-1
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: TUTF16String; var StrPos: SizeInt): UCS4; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetPreviousChar(const S: WideString; var StrPos: SizeInt): UCS4; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}

// UTF16SkipChars = skip NbSeq UTF16 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: TUTF16String; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SkipChars(const S: WideString; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function UTF16SkipCharsFromStream(S: TStream; var NbSeq: SizeInt): Boolean;

// UTF16SetNextChar = append an UTF16 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-16 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: TUTF16String; var StrPos: SizeInt; Ch: UCS4): Boolean; overload;
function UTF16SetNextBuffer(var S: TUTF16String; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SetNextChar(var S: WideString; var StrPos: SizeInt; Ch: UCS4): Boolean; overload;
function UTF16SetNextBuffer(var S: WideString; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function UTF16SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
function UTF16SetNextBufferToStream(S: TStream; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;

// AnsiGetNextChar = read next character at StrPos
// StrPos will be incremented by the number of chars that were read (1)
function AnsiGetNextChar(const S: AnsiString; var StrPos: SizeInt): UCS4; overload;
function AnsiGetNextBuffer(const S: AnsiString; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
function AnsiGetNextCharFromStream(S: TStream; out Ch: UCS4): Boolean; overload;
function AnsiGetNextBufferFromStream(S: TStream; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;

// same as AnsiGetNextChar* with custom codepage
function AnsiGetNextChar(const S: AnsiString; CodePage: Word; var StrPos: SizeInt): UCS4; overload;
function AnsiGetNextBuffer(const S: AnsiString; CodePage: Word; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
function AnsiGetNextCharFromStream(S: TStream; CodePage: Word; out Ch: UCS4): Boolean; overload;
function AnsiGetNextBufferFromStream(S: TStream; CodePage: Word; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;

// AnsiSkipChars = skip NbSeq characters starting from StrPos
// returns False if String is too small
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function AnsiSkipChars(const S: AnsiString; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
function AnsiSkipCharsFromStream(S: TStream; var NbSeq: SizeInt): Boolean;

// AnsiSetNextChar = append a character at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an ansi string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written (1)
function AnsiSetNextChar(var S: AnsiString; var StrPos: SizeInt; Ch: UCS4): Boolean; overload;
function AnsiSetNextBuffer(var S: AnsiString; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
function AnsiSetNextCharToStream(S: TStream; Ch: UCS4): Boolean; overload;
function AnsiSetNextBufferToStream(S: TStream; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;

// same as AnsiSetNextChar* with custom codepage
function AnsiSetNextChar(var S: AnsiString; CodePage: Word; var StrPos: SizeInt; Ch: UCS4): Boolean; overload;
function AnsiSetNextBuffer(var S: AnsiString; CodePage: Word; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
function AnsiSetNextCharToStream(S: TStream; CodePage: Word; Ch: UCS4): Boolean; overload;
function AnsiSetNextBufferToStream(S: TStream; CodePage: Word; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;

// StringGetNextChar = read next character/sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were read
function StringGetNextChar(const S: string; var StrPos: SizeInt): UCS4; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function StringGetNextBuffer(const S: string; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// StringSkipChars = skip NbSeq characters/sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function StringSkipChars(const S: string; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// StringSetNextChar = append a character/sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to a string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function StringSetNextChar(var S: string; var StrPos: SizeInt; Ch: UCS4): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function StringSetNextBuffer(var S: string; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// one shot conversions between WideString and others
function WideStringToUTF8(const S: WideString): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToWideString(const S: TUTF8String): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideStringToUCS4(const S: WideString): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToWideString(const S: TUCS4Array): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

// one shot conversions between AnsiString and others
function AnsiStringToUTF8(const S: AnsiString): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToAnsiString(const S: TUTF8String): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function AnsiStringToUTF16(const S: AnsiString): TUTF16String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF16ToAnsiString(const S: TUTF16String): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function AnsiStringToUCS4(const S: AnsiString): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToAnsiString(const S: TUCS4Array): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

// one shot conversions between string and others
function StringToUTF8(const S: string): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToString(const S: TUTF8String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function StringToUTF16(const S: string): TUTF16String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF16ToString(const S: TUTF16String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function StringToUCS4(const S: string): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToString(const S: TUCS4Array): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function TryStringToUTF8(const S: string; out D: TUTF8String): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TryUTF8ToString(const S: TUTF8String; out D: string): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TryStringToUTF16(const S: string; out D: TUTF16String): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TryUTF16ToString(const S: TUTF16String; out D: string): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TryStringToUCS4(const S: string; out D: TUCS4Array): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TryUCS4ToString(const S: TUCS4Array; out D: string): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function UTF8ToUTF16(const S: TUTF8String): TUTF16String;
function UTF16ToUTF8(const S: TUTF16String): TUTF8String;
function UTF8ToUCS4(const S: TUTF8String): TUCS4Array;
function UCS4ToUTF8(const S: TUCS4Array): TUTF8String;
function UTF16ToUCS4(const S: TUTF16String): TUCS4Array;
function UCS4ToUTF16(const S: TUCS4Array): TUTF16String;

function TryUTF8ToUTF16(const S: TUTF8String; out D: TUTF16String): Boolean;
function TryUTF16ToUTF8(const S: TUTF16String; out D: TUTF8String): Boolean;
function TryUTF8ToUCS4(const S: TUTF8String; out D: TUCS4Array): Boolean;
function TryUCS4ToUTF8(const S: TUCS4Array; out D: TUTF8String): Boolean;
function TryUTF16ToUCS4(const S: TUTF16String; out D: TUCS4Array): Boolean;
function TryUCS4ToUTF16(const S: TUCS4Array; out D: TUTF16String): Boolean;

// indexed conversions
function UTF8CharCount(const S: TUTF8String): SizeInt;
function UTF16CharCount(const S: TUTF16String): SizeInt;
function UCS2CharCount(const S: TUCS2String): SizeInt;
function UCS4CharCount(const S: TUCS4Array): SizeInt;
// returns False if string is too small
// if UNICODE_SILENT_FAILURE is not defined and an invalid UTFX sequence is detected, an exception is raised
// returns True on success and Value contains UCS4 character that was read
function GetUCS4CharAt(const UTF8Str: TUTF8String; Index: SizeInt; out Value: UCS4): Boolean; overload;
function GetUCS4CharAt(const WideStr: TUTF16String; Index: SizeInt; out Value: UCS4; IsUTF16: Boolean = True): Boolean; overload;
function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: SizeInt; out Value: UCS4): Boolean; overload;

function UCS4ToAnsiChar(Value: UCS4): AnsiChar;
function UCS4ToWideChar(Value: UCS4): WideChar;
function UCS4ToChar(Value: UCS4): Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function AnsiCharToUCS4(Value: AnsiChar): UCS4;
function WideCharToUCS4(Value: WideChar): UCS4;
function CharToUCS4(Value: Char): UCS4; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF ~HAS_UNITSCOPE}
  JclResources;

const MB_ERR_INVALID_CHARS = 8;

constructor EJclUnexpectedEOSequenceError.Create;
begin
  inherited CreateRes(@RsEUnexpectedEOSeq);
end;

function StreamReadByte(S: TStream; out B: Byte): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  B := 0;
  Result := S.Read(B, SizeOf(B)) = SizeOf(B);
end;

function StreamWriteByte(S: TStream; B: Byte): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := S.Write(B, SizeOf(B)) = SizeOf(B);
end;

function StreamReadWord(S: TStream; out W: Word): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  W := 0;
  Result := S.Read(W, SizeOf(W)) = SizeOf(W);
end;

function StreamWriteWord(S: TStream; W: Word): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := S.Write(W, SizeOf(W)) = SizeOf(W);
end;

//----------------- conversion routines ------------------------------------------------------------

// Converts the given source ANSI string into a Unicode string by expanding each character
// from one byte to two bytes.
// EAX contains Source, EDX contains Target, ECX contains Count

procedure ExpandASCIIString(const Source: PAnsiChar; Target: PWideChar; Count: SizeInt);
asm
       {$IFDEF CPU32}
       // --> EAX Source
       //     EDX Target
       //     ECX Count
       JECXZ   @@Finish           // go out if there is nothing to do (ECX = 0)
       PUSH    ESI
       MOV     ESI, EAX
       XOR     EAX, EAX
@@1:
       MOV     AL, [ESI]
       INC     ESI
       MOV     [EDX], AX
       ADD     EDX, 2
       DEC     ECX
       JNZ     @@1
       POP     ESI
       {$ENDIF CPU32}
       {$IFDEF CPU64}
       // --> RCX Source
       //     RDX Target
       //     R8  Count

       DEC     R8    // go out if there is nothing to do (R8 = 0)
       JS      @@Finish
@@1:
       MOVZX   AX, BYTE PTR [RCX]
       INC     RCX
       MOV     WORD PTR [RDX], AX
       ADD     RDX, 2
@@2:
       DEC     R8
       JNS     @@1
       {$ENDIF CPU64}
@@Finish:
end;

const
  HalfShift: Integer = 10;

  HalfBase: UCS4 = $0010000;
  HalfMask: UCS4 = $3FF;

procedure FlagInvalidSequence(var StrPos: SizeInt; Increment: SizeInt; out Ch: UCS4); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := UCS4ReplacementCharacter;
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence(var StrPos: SizeInt; Increment: SizeInt); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence(out Ch: UCS4); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := UCS4ReplacementCharacter;
  {$ELSE ~UNICODE_SILENT_FAILURE}
  raise EJclUnexpectedEOSequenceError.Create;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence; overload;
begin
  {$IFNDEF UNICODE_SILENT_FAILURE}
  raise EJclUnexpectedEOSequenceError.Create;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: TUTF8String; var StrPos: SizeInt): UCS4;
var
  StrLength: SizeInt;
  Ch: UCS4;
  ReadSuccess: Boolean;
begin
  StrLength := Length(S);
  ReadSuccess := True;

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      $00..$7F:
        // 1 byte to read
        Inc(StrPos);
      $C0..$DF:
        begin
          // 2 bytes to read
          if StrPos < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch and $C0) = $80 then
            begin
              Result := ((Result and $1F) shl 6) or (Ch and $3F);
              Inc(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            ReadSuccess := False;
        end;
      $E0..$EF:
        begin
          // 3 bytes to read
          if (StrPos + 1) < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch and $C0) = $80 then
            begin
              Result := ((Result and $0F) shl 12) or ((Ch and $3F) shl 6);
              Ch := UCS4(S[StrPos + 2]);
              if (Ch and $C0) = $80 then
              begin
                Result := Result or (Ch and $3F);
                Inc(StrPos, 3);
              end
              else
                FlagInvalidSequence(StrPos, 2, Result);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            ReadSuccess := False;
        end;
      $F0..$F7:
        begin
          // 4 bytes to read
          if (StrPos + 2) < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch and $C0) = $80 then
            begin
              Result := ((Result and $07) shl 18) or ((Ch and $3F) shl 12);
              Ch := UCS4(S[StrPos + 2]);
              if (Ch and $C0) = $80 then
              begin
                Result := Result or ((Ch and $3F) shl 6);
                Ch := UCS4(S[StrPos + 3]);
                if (Ch and $C0) = $80 then
                begin
                  Result := Result or (Ch and $3F);
                  Inc(StrPos, 4);
                end
                else
                  FlagInvalidSequence(StrPos, 3, Result);
              end
              else
                FlagInvalidSequence(StrPos, 2, Result);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            ReadSuccess := False;
        end;
      $F8..$FB:
        begin
          // 5 bytes to read
          if (StrPos + 3) < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch and $C0) = $80 then
            begin
              Result := ((Result and $03) shl 24) or ((Ch and $3F) shl 18);
              Ch := UCS4(S[StrPos + 2]);
              if (Ch and $C0) = $80 then
              begin
                Result := Result or ((Ch and $3F) shl 12);
                Ch := UCS4(S[StrPos + 3]);
                if (Ch and $C0) = $80 then
                begin
                  Result := Result or ((Ch and $3F) shl 6);
                  Ch := UCS4(S[StrPos + 4]);
                  if (Ch and $C0) = $80 then
                  begin
                    Result := Result or (Ch and $3F);
                    Inc(StrPos, 5);
                  end
                  else
                    FlagInvalidSequence(StrPos, 4, Result);
                end
                else
                  FlagInvalidSequence(StrPos, 3, Result);
              end
              else
                FlagInvalidSequence(StrPos, 2, Result);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            ReadSuccess := False;
        end;
      $FC..$FD:
        begin
          // 6 bytes to read
          if (StrPos + 4) < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch and $C0) = $80 then
            begin
              Result := ((Result and $01) shl 30) or ((Ch and $3F) shl 24);
              Ch := UCS4(S[StrPos + 2]);
              if (Ch and $C0) = $80 then
              begin
                Result := Result or ((Ch and $3F) shl 18);
                Ch := UCS4(S[StrPos + 3]);
                if (Ch and $C0) = $80 then
                begin
                  Result := Result or ((Ch and $3F) shl 12);
                  Ch := UCS4(S[StrPos + 4]);
                  if (Ch and $C0) = $80 then
                  begin
                    Result := Result or ((Ch and $3F) shl 6);
                    Ch := UCS4(S[StrPos + 5]);
                    if (Ch and $C0) = $80 then
                    begin
                      Result := Result or (Ch and $3F);
                      Inc(StrPos, 6);
                    end
                    else
                      FlagInvalidSequence(StrPos, 5, Result);
                  end
                  else
                    FlagInvalidSequence(StrPos, 4, Result);
                end
                else
                  FlagInvalidSequence(StrPos, 3, Result);
              end
              else
                FlagInvalidSequence(StrPos, 2, Result);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            ReadSuccess := False;
        end;
    else
      FlagInvalidSequence(StrPos, 1, Result);
    end;
    if not ReadSuccess then
      FlagInvalidSequence(StrPos, 1, Result);
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function UTF8GetNextBuffer(const S: TUTF8String; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
var
  B: Byte;
  Ch: UCS4;
  ReadSuccess: Boolean;
  StrLength: SizeInt;
begin
  StrLength := Length(S);
  Result := 0;
  ReadSuccess := True;
  while (StrPos <= StrLength) and (StrPos > 0) and (Count > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      $00..$7F:
        // 1 byte to read
        Inc(StrPos);
      $C0..$DF:
        begin
          // 2 bytes to read
          if StrPos < StrLength then
          begin
            B := Ord(S[StrPos + 1]);
            if (B and $C0) = $80 then
              Ch := ((Ch and $1F) shl 6) or (B and $3F)
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end;
      $E0..$EF:
        begin
          // 3 bytes to read
          if (StrPos + 1) < StrLength then
          begin
            B := Ord(S[StrPos + 1]);
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $0F) shl 12) or ((B and $3F) shl 6);
              B := Ord(S[StrPos + 2]);
              if (B and $C0) = $80 then
                Ch := Ch or (B and $3F)
              else
                FlagInvalidSequence(StrPos, 2, Ch);
            end
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end;
      $F0..$F7:
        begin
          // 4 bytes to read
          if (StrPos + 2) < StrLength then
          begin
            B := Ord(S[StrPos + 1]);
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $07) shl 18) or ((B and $3F) shl 12);
              B := Ord(S[StrPos + 2]);
              if (B and $C0) = $80 then
              begin
                Ch := Ch or ((B and $3F) shl 6);
                B := Ord(S[StrPos + 3]);
                if (B and $C0) = $80 then
                  Ch := Ch or (B and $3F)
                else
                  FlagInvalidSequence(StrPos, 3, Ch);
              end
              else
                FlagInvalidSequence(StrPos, 2, Ch);
            end
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end;
      $F8..$FB:
        begin
          // 5 bytes to read
          if (StrPos + 3) < StrLength then
          begin
            B := Ord(S[StrPos + 1]);
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $03) shl 24) or ((B and $3F) shl 18);
              B := Ord(S[StrPos + 2]);
              if (B and $C0) = $80 then
              begin
                Ch := Ch or ((B and $3F) shl 12);
                B := Ord(S[StrPos + 3]);
                if (B and $C0) = $80 then
                begin
                  Ch := Ch or ((B and $3F) shl 6);
                  B := Ord(S[StrPos + 4]);
                  if (B and $C0) = $80 then
                    Ch := Ch or (B and $3F)
                  else
                    FlagInvalidSequence(StrPos, 4, Ch);
                end
                else
                  FlagInvalidSequence(StrPos, 3, Ch);
              end
              else
                FlagInvalidSequence(StrPos, 2, Ch);
            end
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end;
      $FC..$FD:
        begin
          // 6 bytes to read
          if (StrPos + 4) < StrLength then
          begin
            B := Ord(S[StrPos + 1]);
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $01) shl 30) or ((B and $3F) shl 24);
              B := Ord(S[StrPos + 2]);
              if (B and $C0) = $80 then
              begin
                Ch := Ch or ((B and $3F) shl 18);
                B := Ord(S[StrPos + 3]);
                if (B and $C0) = $80 then
                begin
                  Ch := Ch or ((B and $3F) shl 12);
                  B := Ord(S[StrPos + 4]);
                  if (B and $C0) = $80 then
                  begin
                    Ch := Ch or ((B and $3F) shl 6);
                    B := Ord(S[StrPos + 5]);
                    if (B and $C0) = $80 then
                      Ch := Ch or (B and $3F)
                    else
                      FlagInvalidSequence(StrPos, 5, Ch);
                  end
                  else
                    FlagInvalidSequence(StrPos, 4, Ch);
                end
                else
                  FlagInvalidSequence(StrPos, 3, Ch);
              end
              else
                FlagInvalidSequence(StrPos, 2, Ch);
            end
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end
    else
      FlagInvalidSequence(StrPos, 1, Ch);
    end;
    if not ReadSuccess then
      FlagInvalidSequence(StrPos, 1, Ch);

    Buffer[Start] := Ch;
    Inc(Start);
    Inc(Result);
    Dec(Count);
  end;
end;

function UTF8GetNextCharFromStream(S: TStream; out Ch: UCS4): Boolean;
var
  B: Byte;
begin
  Result := StreamReadByte(S,B);
  if Result then
  begin
    Ch := UCS4(B);

    case Ch of
      $00..$7F: ;
        // 1 byte to read
        // nothing to do
      $C0..$DF:
        begin
          // 2 bytes to read
          Result := StreamReadByte(S,B);
          if Result then
          begin
            if (B and $C0) = $80 then
              Ch := ((Ch and $1F) shl 6) or (B and $3F)
            else
              FlagInvalidSequence(Ch);
          end;
        end;
      $E0..$EF:
        begin
          // 3 bytes to read
          Result := StreamReadByte(S,B);
          if Result then
          begin
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $0F) shl 12) or ((B and $3F) shl 6);
              Result := StreamReadByte(S,B);
              if Result then
              begin
                if (B and $C0) = $80 then
                  Ch := Ch or (B and $3F)
                else
                  FlagInvalidSequence(Ch);
              end;
            end
            else
              FlagInvalidSequence(Ch);
          end;
        end;
      $F0..$F7:
        begin
          // 4 bytes to read
          Result := StreamReadByte(S,B);
          if Result then
          begin
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $07) shl 18) or ((B and $3F) shl 12);
              Result := StreamReadByte(S,B);
              if Result then
              begin
                if (B and $C0) = $80 then
                begin
                  Ch := Ch or ((B and $3F) shl 6);
                  Result := StreamReadByte(S,B);
                  if Result then
                  begin
                    if (B and $C0) = $80 then
                      Ch := Ch or (B and $3F)
                    else
                      FlagInvalidSequence(Ch);
                  end;
                end
                else
                  FlagInvalidSequence(Ch);
              end;
            end
            else
              FlagInvalidSequence(Ch);
          end;
        end;
      $F8..$FB:
        begin
          // 5 bytes to read
          Result := StreamReadByte(S,B);
          if Result then
          begin
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $03) shl 24) or ((B and $3F) shl 18);
              Result := StreamReadByte(S,B);
              if Result then
              begin
                if (B and $C0) = $80 then
                begin
                  Ch := Ch or ((B and $3F) shl 12);
                  Result := StreamReadByte(S,B);
                  if Result then
                  begin
                    if (B and $C0) = $80 then
                    begin
                      Ch := Ch or ((B and $3F) shl 6);
                      Result := StreamReadByte(S,B);
                      if Result then
                      begin
                        if (B and $C0) = $80 then
                          Ch := Ch or (B and $3F)
                        else
                          FlagInvalidSequence(Ch);
                      end;
                    end
                    else
                      FlagInvalidSequence(Ch);
                  end;
                end
                else
                  FlagInvalidSequence(Ch);
              end;
            end
            else
              FlagInvalidSequence(Ch);
          end;
        end;
      $FC..$FD:
        begin
          // 6 bytes to read
          Result := StreamReadByte(S,B);
          if Result then
          begin
            if (B and $C0) = $80 then
            begin
              Ch := ((Ch and $01) shl 30) or ((B and $3F) shl 24);
              Result := StreamReadByte(S,B);
              if Result then
              begin
                if (B and $C0) = $80 then
                begin
                  Ch := Ch or ((B and $3F) shl 18);
                  Result := StreamReadByte(S,B);
                  if Result then
                  begin
                    if (B and $C0) = $80 then
                    begin
                      Ch := Ch or ((B and $3F) shl 12);
                      Result := StreamReadByte(S,B);
                      if Result then
                      begin
                        if (B and $C0) = $80 then
                        begin
                          Ch := Ch or ((B and $3F) shl 6);
                          Result := StreamReadByte(S,B);
                          if Result then
                          begin
                            if (B and $C0) = $80 then
                              Ch := Ch or (B and $3F)
                            else
                              FlagInvalidSequence(Ch);
                          end;
                        end
                        else
                          FlagInvalidSequence(Ch);
                      end;
                    end
                    else
                      FlagInvalidSequence(Ch);
                  end;
                end
                else
                  FlagInvalidSequence(Ch);
              end;
            end
            else
              FlagInvalidSequence(Ch);
          end;
        end;
    else
      FlagInvalidSequence(Ch);
    end;
  end;
end;

function UTF8GetNextBufferFromStream(S: TStream; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
var
  B: Byte;
  Ch: UCS4;
  ReadSuccess: Boolean;
begin
  Result := 0;
  ReadSuccess := True;
  while ReadSuccess and (Count > 0) do
  begin
    if StreamReadByte(S,B) then
    begin
      Ch := UCS4(B);

      case Ch of
        $00..$7F: ;
          // 1 byte to read
          // nothing to do
        $C0..$DF:
          begin
            // 2 bytes to read
            if StreamReadByte(S,B) then
            begin
              if (B and $C0) = $80 then
                Ch := ((Ch and $1F) shl 6) or (B and $3F)
              else
                FlagInvalidSequence(Ch);
            end
            else
              ReadSuccess := False;
          end;
        $E0..$EF:
          begin
            // 3 bytes to read
            if StreamReadByte(S,B) then
            begin
              if (B and $C0) = $80 then
              begin
                Ch := ((Ch and $0F) shl 12) or ((B and $3F) shl 6);
                if StreamReadByte(S,B) then
                begin
                  if (B and $C0) = $80 then
                    Ch := Ch or (B and $3F)
                  else
                    FlagInvalidSequence(Ch);
                end
                else
                  ReadSuccess := False;
              end
              else
                FlagInvalidSequence(Ch);
            end
            else
              ReadSuccess := False;
          end;
        $F0..$F7:
          begin
            // 4 bytes to read
            if StreamReadByte(S,B) then
            begin
              if (B and $C0) = $80 then
              begin
                Ch := ((Ch and $07) shl 18) or ((B and $3F) shl 12);
                if StreamReadByte(S,B) then
                begin
                  if (B and $C0) = $80 then
                  begin
                    Ch := Ch or ((B and $3F) shl 6);
                    if StreamReadByte(S,B) then
                    begin
                      if (B and $C0) = $80 then
                        Ch := Ch or (B and $3F)
                      else
                        FlagInvalidSequence(Ch);
                    end
                    else
                      ReadSuccess := False;
                  end
                  else
                    FlagInvalidSequence(Ch);
                end
                else
                  ReadSuccess := False;
              end
              else
                FlagInvalidSequence(Ch);
            end
            else
              ReadSuccess := False;
          end;
        $F8..$FB:
          begin
            // 5 bytes to read
            if StreamReadByte(S,B) then
            begin
              if (B and $C0) = $80 then
              begin
                Ch := ((Ch and $03) shl 24) or ((B and $3F) shl 18);
                if StreamReadByte(S,B) then
                begin
                  if (B and $C0) = $80 then
                  begin
                    Ch := Ch or ((B and $3F) shl 12);
                    if StreamReadByte(S,B) then
                    begin
                      if (B and $C0) = $80 then
                      begin
                        Ch := Ch or ((B and $3F) shl 6);
                        if StreamReadByte(S,B) then
                        begin
                          if (B and $C0) = $80 then
                            Ch := Ch or (B and $3F)
                          else
                            FlagInvalidSequence(Ch);
                        end
                        else
                          ReadSuccess := False;
                      end
                      else
                        FlagInvalidSequence(Ch);
                    end
                    else
                      ReadSuccess := False;
                  end
                  else
                    FlagInvalidSequence(Ch);
                end
                else
                  ReadSuccess := False;
              end
              else
                FlagInvalidSequence(Ch);
            end
            else
              ReadSuccess := False;
          end;
        $FC..$FD:
          begin
            // 6 bytes to read
            if StreamReadByte(S,B) then
            begin
              if (B and $C0) = $80 then
              begin
                Ch := ((Ch and $01) shl 30) or ((B and $3F) shl 24);
                if StreamReadByte(S,B) then
                begin
                  if (B and $C0) = $80 then
                  begin
                    Ch := Ch or ((B and $3F) shl 18);
                    if StreamReadByte(S,B) then
                    begin
                      if (B and $C0) = $80 then
                      begin
                        Ch := Ch or ((B and $3F) shl 12);
                        if StreamReadByte(S,B) then
                        begin
                          if (B and $C0) = $80 then
                          begin
                            Ch := Ch or ((B and $3F) shl 6);
                            if StreamReadByte(S,B) then
                            begin
                              if (B and $C0) = $80 then
                                Ch := Ch or (B and $3F)
                              else
                                FlagInvalidSequence(Ch);
                            end
                            else
                              ReadSuccess := False;
                          end
                          else
                            FlagInvalidSequence(Ch);
                        end
                        else
                          ReadSuccess := False;
                      end
                      else
                        FlagInvalidSequence(Ch);
                    end
                    else
                      ReadSuccess := False;
                  end
                  else
                    FlagInvalidSequence(Ch);
                end
                else
                  ReadSuccess := False;
              end
              else
                FlagInvalidSequence(Ch);
            end
            else
              ReadSuccess := False;
          end
      else
        FlagInvalidSequence(Ch);
      end;
      if ReadSuccess then
      begin
        Buffer[Start] := Ch;
        Inc(Start);
        Inc(Result);
      end;
    end
    else
      ReadSuccess := False;
    Dec(Count);
  end;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of ansi chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: TUTF8String; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
var
  StrLength: SizeInt;
  Ch: UCS4;
  Index: SizeInt;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  while (Index < NbSeq) and (StrPos > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      $00..$7F:
        // 1 byte to skip
        Inc(StrPos);
      $C0..$DF:
        // 2 bytes to skip
        if (StrPos >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
          Inc(StrPos, 2);
      $E0..$EF:
        // 3 bytes to skip
        if ((StrPos + 1) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
          Inc(StrPos, 3);
      $F0..$F7:
        // 4 bytes to skip
        if ((StrPos + 2) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
          Inc(StrPos, 4);
      $F8..$FB:
        // 5 bytes to skip
        if ((StrPos + 3) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
          Inc(StrPos, 5);
      $FC..$FD:
        // 6 bytes to skip
        if ((StrPos + 4) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
        if (UCS4(S[StrPos + 5]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 5)
        else
          Inc(StrPos, 6);
    else
      FlagInvalidSequence(StrPos, 1);
    end;

    if StrPos <> -1 then
      Inc(Index);
    if (StrPos > StrLength) and (Index < NbSeq) then
    begin
      Result := False;
      Break;
    end;
  end;
  NbSeq := Index;
end;

function UTF8SkipCharsFromStream(S: TStream; var NbSeq: SizeInt): Boolean;
var
  B: Byte;
  Index: SizeInt;
begin
  Index := 0;
  while (Index < NbSeq) do
  begin
    Result := StreamReadByte(S, B);
    if not Result then
      Break;
    case B of
      $00..$7F: ;
        // 1 byte to skip
        // nothing to do
      $C0..$DF:
        // 2 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $E0..$EF:
        // 3 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $F0..$F7:
        // 4 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $F8..$FB:
        // 5 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $FC..$FD:
        // 6 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
    else
      FlagInvalidSequence;
    end;
    Inc(Index);
  end;
  Result := Index = NbSeq;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: SizeInt; Ch: UCS4): Boolean;
var
  StrLength: SizeInt;
begin
  StrLength := Length(S);

  if Ch <= $7F then
  begin
    // 7 bits to store
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := AnsiChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= $7FF then
  begin
    // 11 bits to store
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then
    begin
      S[StrPos] := AnsiChar($C0 or (Ch shr 6));  // 5 bits
      S[StrPos + 1] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 2);
    end;
  end
  else
  if Ch <= $FFFF then
  begin
    // 16 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 1));
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (Ch shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
  end
  else
  if Ch <= $1FFFFF then
  begin
    // 21 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 2));
    if Result then
    begin
      S[StrPos] := AnsiChar($F0 or (Ch shr 18)); // 3 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 4);
    end;
  end
  else
  if Ch <= $3FFFFFF then
  begin
    // 26 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 2));
    if Result then
    begin
      S[StrPos] := AnsiChar($F8 or (Ch shr 24)); // 2 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 5);
    end;
  end
  else
  if Ch <= MaximumUCS4 then
  begin
    // 31 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 3));
    if Result then
    begin
      S[StrPos] := AnsiChar($FC or (Ch shr 30)); // 1 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 24) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 5] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 6);
    end;
  end
  else
  begin
    {$IFDEF UNICODE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos < (StrLength - 1));
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (UCS4ReplacementCharacter shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((UCS4ReplacementCharacter shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((UCS4ReplacementCharacter and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function UTF8SetNextBuffer(var S: TUTF8String; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
var
  StrLength: SizeInt;
  Ch: UCS4;
  Success: Boolean;
begin
  StrLength := Length(S);
  Success := True;
  Result := 0;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];
    if Ch <= $7F then
    begin
      // 7 bits to store
      if (StrPos > 0) and (StrPos <= StrLength) then
      begin
        S[StrPos] := AnsiChar(Ch);
        Inc(StrPos);
      end
      else
        Success := False;
    end
    else
    if Ch <= $7FF then
    begin
      // 11 bits to store
      if (StrPos > 0) and (StrPos < StrLength) then
      begin
        S[StrPos] := AnsiChar($C0 or (Ch shr 6));  // 5 bits
        S[StrPos + 1] := AnsiChar((Ch and $3F) or $80); // 6 bits
        Inc(StrPos, 2);
      end
      else
        Success := False;
    end
    else
    if Ch <= $FFFF then
    begin
      // 16 bits to store
      if (StrPos > 0) and (StrPos < (StrLength - 1)) then
      begin
        S[StrPos] := AnsiChar($E0 or (Ch shr 12)); // 4 bits
        S[StrPos + 1] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
        S[StrPos + 2] := AnsiChar((Ch and $3F) or $80); // 6 bits
        Inc(StrPos, 3);
      end
      else
        Success := False;
    end
    else
    if Ch <= $1FFFFF then
    begin
      // 21 bits to store
      if (StrPos > 0) and (StrPos < (StrLength - 2)) then
      begin
        S[StrPos] := AnsiChar($F0 or (Ch shr 18)); // 3 bits
        S[StrPos + 1] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
        S[StrPos + 2] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
        S[StrPos + 3] := AnsiChar((Ch and $3F) or $80); // 6 bits
        Inc(StrPos, 4);
      end
      else
        Success := False;
    end
    else
    if Ch <= $3FFFFFF then
    begin
      // 26 bits to store
      if (StrPos > 0) and (StrPos < (StrLength - 2)) then
      begin
        S[StrPos] := AnsiChar($F8 or (Ch shr 24)); // 2 bits
        S[StrPos + 1] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
        S[StrPos + 2] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
        S[StrPos + 3] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
        S[StrPos + 4] := AnsiChar((Ch and $3F) or $80); // 6 bits
        Inc(StrPos, 5);
      end
      else
        Success := False;
    end
    else
    if Ch <= MaximumUCS4 then
    begin
      // 31 bits to store
      if (StrPos > 0) and (StrPos < (StrLength - 3)) then
      begin
        S[StrPos] := AnsiChar($FC or (Ch shr 30)); // 1 bits
        S[StrPos + 1] := AnsiChar(((Ch shr 24) and $3F) or $80); // 6 bits
        S[StrPos + 2] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
        S[StrPos + 3] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
        S[StrPos + 4] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
        S[StrPos + 5] := AnsiChar((Ch and $3F) or $80); // 6 bits
        Inc(StrPos, 6);
      end
      else
        Success := False;
    end
    else
    begin
      {$IFDEF UNICODE_SILENT_FAILURE}
      // add ReplacementCharacter
      if (StrPos > 0) and (StrPos < (StrLength - 1)) then
      begin
        S[StrPos] := AnsiChar($E0 or (UCS4ReplacementCharacter shr 12)); // 4 bits
        S[StrPos + 1] := AnsiChar(((UCS4ReplacementCharacter shr 6) and $3F) or $80); // 6 bits
        S[StrPos + 2] := AnsiChar((UCS4ReplacementCharacter and $3F) or $80); // 6 bits
        Inc(StrPos, 3);
      end
      else
        Success := False;
      {$ELSE ~UNICODE_SILENT_FAILURE}
      StrPos := -1;
      Success := False;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

function UTF8SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
begin
  if Ch <= $7F then
    // 7 bits to store
    Result := StreamWriteByte(S,Ch)
  else
  if Ch <= $7FF then
    // 11 bits to store
    Result := StreamWriteByte(S, $C0 or (Ch shr 6)) and  // 5 bits
              StreamWriteByte(S, (Ch and $3F) or $80)    // 6 bits
  else
  if Ch <= $FFFF then
    // 16 bits to store
    Result := StreamWriteByte(S, $E0 or (Ch shr 12))          and // 4 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)             // 6 bits
  else
  if Ch <= $1FFFFF then
    // 21 bits to store
    Result := StreamWriteByte(S, $F0 or (Ch shr 18))           and // 3 bits
              StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
  else
  if Ch <= $3FFFFFF then
    // 26 bits to store
    Result := StreamWriteByte(S, $F8 or (Ch shr 24))           and // 2 bits
              StreamWriteByte(S, ((Ch shr 18) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
  else
  if Ch <= MaximumUCS4 then
    // 31 bits to store
    Result := StreamWriteByte(S, $FC or (Ch shr 30))           and // 1 bits
              StreamWriteByte(S, ((Ch shr 24) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 18) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
  else
    {$IFDEF UNICODE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StreamWriteByte(S, $E0 or (UCS4ReplacementCharacter shr 12))          and // 4 bits
              StreamWriteByte(S, ((UCS4ReplacementCharacter shr 6) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, (UCS4ReplacementCharacter and $3F) or $80); // 6 bits
    {$ELSE ~UNICODE_SILENT_FAILURE}
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

function UTF8SetNextBufferToStream(S: TStream; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
var
  Ch: UCS4;
  Success: Boolean;
begin
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];
    if Ch <= $7F then
      // 7 bits to store
      Success := StreamWriteByte(S,Ch)
    else
    if Ch <= $7FF then
      // 11 bits to store
      Success := StreamWriteByte(S, $C0 or (Ch shr 6)) and  // 5 bits
                 StreamWriteByte(S, (Ch and $3F) or $80)    // 6 bits
    else
    if Ch <= $FFFF then
      // 16 bits to store
      Success := StreamWriteByte(S, $E0 or (Ch shr 12))          and // 4 bits
                 StreamWriteByte(S, ((Ch shr 6) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, (Ch and $3F) or $80)             // 6 bits
    else
    if Ch <= $1FFFFF then
      // 21 bits to store
      Success := StreamWriteByte(S, $F0 or (Ch shr 18))           and // 3 bits
                 StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
                 StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
    else
    if Ch <= $3FFFFFF then
      // 26 bits to store
      Success := StreamWriteByte(S, $F8 or (Ch shr 24))           and // 2 bits
                 StreamWriteByte(S, ((Ch shr 18) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
                 StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
    else
    if Ch <= MaximumUCS4 then
      // 31 bits to store
      Success := StreamWriteByte(S, $FC or (Ch shr 30))           and // 1 bits
                 StreamWriteByte(S, ((Ch shr 24) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, ((Ch shr 18) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
                 StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
    else
      {$IFDEF UNICODE_SILENT_FAILURE}
      // add ReplacementCharacter
      Success := StreamWriteByte(S, $E0 or (UCS4ReplacementCharacter shr 12))          and // 4 bits
                 StreamWriteByte(S, ((UCS4ReplacementCharacter shr 6) and $3F) or $80) and // 6 bits
                 StreamWriteByte(S, (UCS4ReplacementCharacter and $3F) or $80); // 6 bits
      {$ELSE ~UNICODE_SILENT_FAILURE}
      Success := False;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: TUTF16String; var StrPos: SizeInt): UCS4;
var
  StrLength: SizeInt;
  Ch: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch >= SurrogateLowStart) and (Ch <= SurrogateLowEnd) then
            begin
              Result := ((Result - SurrogateHighStart) shl HalfShift) +  (Ch - SurrogateLowStart) + HalfBase;
              Inc(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            FlagInvalidSequence(StrPos, 1, Result);
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function UTF16GetNextBuffer(const S: TUTF16String; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  StrLength: SizeInt;
  Ch, ChNext: UCS4;
  ReadSuccess: Boolean;
begin
  StrLength := Length(S);
  Result := 0;
  ReadSuccess := True;
  while (StrPos <= StrLength) and (StrPos > 0) and (Count > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos < StrLength then
          begin
            ChNext := UCS4(S[StrPos + 1]);
            if (ChNext >= SurrogateLowStart) and (ChNext <= SurrogateLowEnd) then
            begin
              Ch := ((Ch - SurrogateHighStart) shl HalfShift) +  (ChNext - SurrogateLowStart) + HalfBase;
              Inc(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Ch);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
    if not ReadSuccess then
      FlagInvalidSequence(StrPos, 1, Ch);

    Buffer[Start] := Ch;
    Inc(Start);
    Inc(Result);
    Dec(Count);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetNextChar(const S: WideString; var StrPos: SizeInt): UCS4;
var
  StrLength: SizeInt;
  Ch: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos < StrLength then
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch >= SurrogateLowStart) and (Ch <= SurrogateLowEnd) then
            begin
              Result := ((Result - SurrogateHighStart) shl HalfShift) +  (Ch - SurrogateLowStart) + HalfBase;
              Inc(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, 1, Result);
          end
          else
            FlagInvalidSequence(StrPos, 1, Result);
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function UTF16GetNextBuffer(const S: WideString; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  StrLength: SizeInt;
  Ch, ChNext: UCS4;
  ReadSuccess: Boolean;
begin
  StrLength := Length(S);
  Result := 0;
  ReadSuccess := True;
  while (StrPos <= StrLength) and (StrPos > 0) and (Count > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos < StrLength then
          begin
            ChNext := UCS4(S[StrPos + 1]);
            if (ChNext >= SurrogateLowStart) and (ChNext <= SurrogateLowEnd) then
            begin
              Ch := ((Ch - SurrogateHighStart) shl HalfShift) +  (ChNext - SurrogateLowStart) + HalfBase;
              Inc(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, 1, Ch);
          end
          else
            ReadSuccess := False;
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Ch);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
    if not ReadSuccess then
      FlagInvalidSequence(StrPos, 1, Ch);

    Buffer[Start] := Ch;
    Inc(Start);
    Inc(Result);
    Dec(Count);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function UTF16GetNextCharFromStream(S: TStream; out Ch: UCS4): Boolean;
var
  W: Word;
begin
  Result := StreamReadWord(S, W);
  if Result then
  begin
    Ch := UCS4(W);

    case W of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          Result := StreamReadWord(S, W);
          if Result then
          begin
            if (W >= SurrogateLowStart) and (W <= SurrogateLowEnd) then
              Ch := ((Ch - SurrogateHighStart) shl HalfShift) +  (W - SurrogateLowStart) + HalfBase
            else
              FlagInvalidSequence(Ch);
          end;
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(Ch);
    else
      // 1 byte to read
      // nothing to do
    end;
  end;
end;

function UTF16GetNextBufferFromStream(S: TStream; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
var
  W: Word;
  Ch: UCS4;
  ReadSuccess: Boolean;
begin
  Result := 0;
  ReadSuccess := True;
  while ReadSuccess and (Count > 0) do
  begin
    if StreamReadWord(S, W) then
    begin
      Ch := UCS4(W);

      case W of
        SurrogateHighStart..SurrogateHighEnd:
          begin
            // 2 bytes to read
            if StreamReadWord(S, W) then
            begin
              if (W >= SurrogateLowStart) and (W <= SurrogateLowEnd) then
                Ch := ((Ch - SurrogateHighStart) shl HalfShift) +  (W - SurrogateLowStart) + HalfBase
              else
                FlagInvalidSequence(Ch);
            end
            else
              ReadSuccess := False;
          end;
        SurrogateLowStart..SurrogateLowEnd:
          FlagInvalidSequence(Ch);
      else
        // 1 byte to read
        // nothing to do
      end;
      if ReadSuccess then
      begin
        Buffer[Start] := Ch;
        Inc(Result);
        Inc(Start);
      end;
    end;
    Dec(Count);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: TUTF16String; var StrPos: SizeInt): UCS4;
var
  StrLength: SizeInt;
  ChPrev: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= (StrLength + 1)) and (StrPos > 1) then
  begin
    Result := UCS4(S[StrPos - 1]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        FlagInvalidSequence(StrPos, -1, Result);
      SurrogateLowStart..SurrogateLowEnd:
        begin
          // 2 bytes to read
          if StrPos > 2 then
          begin
            ChPrev := UCS4(S[StrPos - 2]);
            if (ChPrev >= SurrogateHighStart) and (ChPrev <= SurrogateHighEnd) then
            begin
              Result := ((ChPrev - SurrogateHighStart) shl HalfShift) +  (Result - SurrogateLowStart) + HalfBase;
              Dec(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, -1, Result);
          end
          else
            FlagInvalidSequence(StrPos, -1, Result);
        end;
    else
      // 1 byte to read
      Dec(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetPreviousChar(const S: WideString; var StrPos: SizeInt): UCS4;
var
  StrLength: SizeInt;
  ChPrev: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= (StrLength + 1)) and (StrPos > 1) then
  begin
    Result := UCS4(S[StrPos - 1]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        FlagInvalidSequence(StrPos, -1, Result);
      SurrogateLowStart..SurrogateLowEnd:
        begin
          // 2 bytes to read
          if StrPos > 2 then
          begin
            ChPrev := UCS4(S[StrPos - 2]);
            if (ChPrev >= SurrogateHighStart) and (ChPrev <= SurrogateHighEnd) then
            begin
              Result := ((ChPrev - SurrogateHighStart) shl HalfShift) +  (Result - SurrogateLowStart) + HalfBase;
              Dec(StrPos, 2);
            end
            else
              FlagInvalidSequence(StrPos, -1, Result);
          end
          else
            FlagInvalidSequence(StrPos, -1, Result);
        end;
    else
      // 1 byte to read
      Dec(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: TUTF16String; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
var
  StrLength, Index: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  if NbSeq >= 0 then
    while (Index < NbSeq) and (StrPos > 0) do
    begin
      Ch := UCS4(S[StrPos]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // 2 bytes to skip
          if StrPos >= StrLength then
            FlagInvalidSequence(StrPos, 1)
          else
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch < SurrogateLowStart) or (Ch > SurrogateLowEnd) then
              FlagInvalidSequence(StrPos, 1)
            else
              Inc(StrPos, 2);
          end;
        SurrogateLowStart..SurrogateLowEnd:
          // error
          FlagInvalidSequence(StrPos, 1);
      else
        // 1 byte to skip
        Inc(StrPos);
      end;

      if StrPos <> -1 then
        Inc(Index);

      if (StrPos > StrLength) and (Index < NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end
  else
    while (Index > NbSeq) and (StrPos > 1) do
    begin
      Ch := UCS4(S[StrPos - 1]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // error
          FlagInvalidSequence(StrPos, -1);
        SurrogateLowStart..SurrogateLowEnd:
          // 2 bytes to skip
          if StrPos <= 2 then
            FlagInvalidSequence(StrPos, -1)
          else
          begin
            Ch := UCS4(S[StrPos - 2]);
            if (Ch < SurrogateHighStart) or (Ch > SurrogateHighEnd) then
              FlagInvalidSequence(StrPos, -1)
            else
              Dec(StrPos, 2);
          end;
      else
        // 1 byte to skip
        Dec(StrPos);
      end;

      if StrPos <> -1 then
        Dec(Index);

      if (StrPos = 1) and (Index > NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end;
  NbSeq := Index;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF16 sequences that were skipped
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SkipChars(const S: WideString; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
var
  StrLength, Index: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  if NbSeq >= 0 then
    while (Index < NbSeq) and (StrPos > 0) do
    begin
      Ch := UCS4(S[StrPos]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // 2 bytes to skip
          if StrPos >= StrLength then
            FlagInvalidSequence(StrPos, 1)
          else
          begin
            Ch := UCS4(S[StrPos + 1]);
            if (Ch < SurrogateLowStart) or (Ch > SurrogateLowEnd) then
              FlagInvalidSequence(StrPos, 1)
            else
              Inc(StrPos, 2);
          end;
        SurrogateLowStart..SurrogateLowEnd:
          // error
          FlagInvalidSequence(StrPos, 1);
      else
        // 1 byte to skip
        Inc(StrPos);
      end;

      if StrPos <> -1 then
        Inc(Index);

      if (StrPos > StrLength) and (Index < NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end
  else
    while (Index > NbSeq) and (StrPos > 1) do
    begin
      Ch := UCS4(S[StrPos - 1]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // error
          FlagInvalidSequence(StrPos, -1);
        SurrogateLowStart..SurrogateLowEnd:
          // 2 bytes to skip
          if StrPos <= 2 then
            FlagInvalidSequence(StrPos, -1)
          else
          begin
            Ch := UCS4(S[StrPos - 2]);
            if (Ch < SurrogateHighStart) or (Ch > SurrogateHighEnd) then
              FlagInvalidSequence(StrPos, -1)
            else
              Dec(StrPos, 2);
          end;
      else
        // 1 byte to skip
        Dec(StrPos);
      end;

      if StrPos <> -1 then
        Dec(Index);

      if (StrPos = 1) and (Index > NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end;
  NbSeq := Index;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function UTF16SkipCharsFromStream(S: TStream; var NbSeq: SizeInt): Boolean;
var
  Index: SizeInt;
  W: Word;
begin
  Index := 0;
  while Index < NbSeq do
  begin
    Result := StreamReadWord(S, W);
    if not Result then
      Break;
    case W of
      SurrogateHighStart..SurrogateHighEnd:
        // 2 bytes to skip
        begin
          Result := StreamReadWord(S, W);
          if not Result then
            Break;
          if (W < SurrogateLowStart) or (W > SurrogateLowEnd) then
            FlagInvalidSequence;
        end;
      SurrogateLowStart..SurrogateLowEnd:
        // error
        FlagInvalidSequence;
    else
      // 1 byte to skip
      // nothing to do
    end;
    Inc(Index);
  end;
  Result := Index = NbSeq;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: TUTF16String; var StrPos: SizeInt; Ch: UCS4): Boolean;
var
  StrLength: SizeInt;
begin
  StrLength := Length(S);

  if Ch <= MaximumUCS2 then
  begin
    // 16 bits to store in place
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= MaximumUTF16 then
  begin
    // stores a surrogate pair
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then
    begin
      Ch := Ch - HalfBase;
      S[StrPos] := WideChar((Ch shr HalfShift) or SurrogateHighStart);
      S[StrPos + 1] := WideChar((Ch and HalfMask) or SurrogateLowStart);
      Inc(StrPos, 2);
    end;
  end
  else
  begin
    {$IFDEF UNICODE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(UCS4ReplacementCharacter);
      Inc(StrPos, 1);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function UTF16SetNextBuffer(var S: TUTF16String; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  StrLength: SizeInt;
  Ch: UCS4;
  Success: Boolean;
begin
  StrLength := Length(S);
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];

    if Ch <= MaximumUCS2 then
    begin
      // 16 bits to store in place
      if (StrPos > 0) and (StrPos <= StrLength) then
      begin
        S[StrPos] := WideChar(Ch);
        Inc(StrPos);
      end
      else
        Success := False;
    end
    else
    if Ch <= MaximumUTF16 then
    begin
      // stores a surrogate pair
      if (StrPos > 0) and (StrPos < StrLength) then
      begin
        Ch := Ch - HalfBase;
        S[StrPos] := WideChar((Ch shr HalfShift) or SurrogateHighStart);
        S[StrPos + 1] := WideChar((Ch and HalfMask) or SurrogateLowStart);
        Inc(StrPos, 2);
      end
      else
        Success := False;
    end
    else
    begin
      {$IFDEF UNICODE_SILENT_FAILURE}
      // add ReplacementCharacter
      if (StrPos > 0) and (StrPos <= StrLength) then
      begin
        S[StrPos] := WideChar(UCS4ReplacementCharacter);
        Inc(StrPos, 1);
      end
      else
        Success := False;
      {$ELSE ~UNICODE_SILENT_FAILURE}
      StrPos := -1;
      Success := False;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SetNextChar(var S: WideString; var StrPos: SizeInt; Ch: UCS4): Boolean;
var
  StrLength: SizeInt;
begin
  StrLength := Length(S);

  if Ch <= MaximumUCS2 then
  begin
    // 16 bits to store in place
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= MaximumUTF16 then
  begin
    // stores a surrogate pair
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then
    begin
      Ch := Ch - HalfBase;
      S[StrPos] := WideChar((Ch shr HalfShift) + SurrogateHighStart);
      S[StrPos + 1] := WideChar((Ch and HalfMask) + SurrogateLowStart);
      Inc(StrPos, 2);
    end;
  end
  else
  begin
    {$IFDEF UNICODE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(UCS4ReplacementCharacter);
      Inc(StrPos, 1);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function UTF16SetNextBuffer(var S: WideString; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  StrLength: SizeInt;
  Ch: UCS4;
  Success: Boolean;
begin
  StrLength := Length(S);
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];

    if Ch <= MaximumUCS2 then
    begin
      // 16 bits to store in place
      if (StrPos > 0) and (StrPos <= StrLength) then
      begin
        S[StrPos] := WideChar(Ch);
        Inc(StrPos);
      end
      else
        Success := False;
    end
    else
    if Ch <= MaximumUTF16 then
    begin
      // stores a surrogate pair
      if (StrPos > 0) and (StrPos < StrLength) then
      begin
        Ch := Ch - HalfBase;
        S[StrPos] := WideChar((Ch shr HalfShift) or SurrogateHighStart);
        S[StrPos + 1] := WideChar((Ch and HalfMask) or SurrogateLowStart);
        Inc(StrPos, 2);
      end
      else
        Success := False;
    end
    else
    begin
      {$IFDEF UNICODE_SILENT_FAILURE}
      // add ReplacementCharacter
      if (StrPos > 0) and (StrPos <= StrLength) then
      begin
        S[StrPos] := WideChar(UCS4ReplacementCharacter);
        Inc(StrPos, 1);
      end
      else
        Success := False;
      {$ELSE ~UNICODE_SILENT_FAILURE}
      StrPos := -1;
      Success := False;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function UTF16SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
begin
  if Ch <= MaximumUCS2 then
    // 16 bits to store in place
    Result := StreamWriteWord(S, Ch)
  else
  if Ch <= MaximumUTF16 then
    // stores a surrogate pair
    Result := StreamWriteWord(S, (Ch shr HalfShift) or SurrogateHighStart) and
              StreamWriteWord(S, (Ch and HalfMask) or SurrogateLowStart)
  else
  begin
    {$IFDEF UNICODE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StreamWriteWord(S, UCS4ReplacementCharacter);
    {$ELSE ~UNICODE_SILENT_FAILURE}
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function UTF16SetNextBufferToStream(S: TStream; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
var
  Ch: UCS4;
  Success: Boolean;
begin
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];
    if Ch <= MaximumUCS2 then
      // 16 bits to store in place
      Success := StreamWriteWord(S, Ch)
    else
    if Ch <= MaximumUTF16 then
      // stores a surrogate pair
      Success := StreamWriteWord(S, (Ch shr HalfShift) or SurrogateHighStart) and
                 StreamWriteWord(S, (Ch and HalfMask) or SurrogateLowStart)
    else
    begin
      {$IFDEF UNICODE_SILENT_FAILURE}
      // add ReplacementCharacter
      Success := StreamWriteWord(S, UCS4ReplacementCharacter);
      {$ELSE ~UNICODE_SILENT_FAILURE}
      Success := False;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

// AnsiGetNextChar = read next character at StrPos
// StrPos will be incremented by the number of chars that were read (1)
function AnsiGetNextChar(const S: AnsiString; var StrPos: SizeInt): UCS4;
var
  StrLen, TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  StrLen := Length(S);

  if (StrPos <= StrLen) and (StrPos > 0) then
  begin
    UTF16Buffer := WideString(S[StrPos]);
    TmpPos := 1;
    Result := UTF16GetNextChar(UTF16Buffer, TmpPos);
    if TmpPos = -1 then
      StrPos := -1
    else
      Inc(StrPos);
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function AnsiGetNextBuffer(const S: AnsiString; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  StrLength, TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  StrLength := Length(S);
  if (StrPos > 0) and (StrPos <= StrLength) then
  begin
    UTF16Buffer := WideString(Copy(S, StrPos, Count));
    TmpPos := 1;
    Result := UTF16GetNextBuffer(UTF16Buffer, TmpPos, Buffer, Start, Count);
    if TmpPos > 0 then
      Inc(StrPos, Result)
    else
      StrPos := -1;
  end
  else
    Result := 0;
end;

function AnsiGetNextCharFromStream(S: TStream; out Ch: UCS4): Boolean;
var
  B: Byte;
  TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  Result := StreamReadByte(S, B);
  if Result then
  begin
    UTF16Buffer := WideString(AnsiString(Chr(B)));
    TmpPos := 1;
    Ch := UTF16GetNextChar(UTF16Buffer, TmpPos);
    Result := TmpPos <> -1;
  end;
end;

function AnsiGetNextBufferFromStream(S: TStream; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  B: TDynByteArray;
  ReadSuccess: Boolean;
  ReadCount, TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  Result := 0;
  ReadSuccess := True;
  SetLength(B, Count);
  SetLength(UTF16Buffer, 2 * Count);
  while ReadSuccess and (Count > 0) do
  begin
    ReadCount := S.Read(B[0], Count);
    if ReadCount > 0 then
    begin
      ReadCount := MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED or MB_ERR_INVALID_CHARS, @B[0], ReadCount, PWideChar(UTF16Buffer), 2 * Count);
      if ReadCount > 0 then
      begin
        TmpPos := 1;
        ReadCount := UTF16GetNextBuffer(UTF16Buffer, TmpPos, Buffer, Start, ReadCount);
        if TmpPos <> -1 then
          Inc(Result, ReadCount)
        else
          ReadSuccess := False;
      end
      else
      begin
        Result := 0;
        FlagInvalidSequence;
      end;
    end
    else
      ReadSuccess := False;
    Dec(Count, ReadCount);
  end;
end;

// AnsiGetNextChar = read next character at StrPos
// StrPos will be incremented by the number of chars that were read (1)
function AnsiGetNextChar(const S: AnsiString; CodePage: Word; var StrPos: SizeInt): UCS4;
var
  StrLen, TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  StrLen := Length(S);

  if (StrPos <= StrLen) and (StrPos > 0) then
  begin
    SetLength(UTF16Buffer, 2);
    if MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_ERR_INVALID_CHARS, @S[StrPos], 1, PWideChar(UTF16Buffer), 2) > 0 then
    begin
      TmpPos := 1;
      Result := UTF16GetNextChar(UTF16Buffer, TmpPos);
      if TmpPos > 0 then
        Inc(StrPos)
      else
        StrPos := -1;
    end
    else
    begin
      Result := UCS4ReplacementCharacter;
      FlagInvalidSequence(StrPos, 1, Result);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function AnsiGetNextBuffer(const S: AnsiString; CodePage: Word; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  ReadCount, StrLength, TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  StrLength := Length(S);
  if (StrPos > 0) and (StrPos <= StrLength) then
  begin
    SetLength(Buffer, 2 * Count);
    ReadCount :=MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_ERR_INVALID_CHARS, @S[StrPos], Count, PWideChar(UTF16Buffer), 2 * Count);
    if ReadCount > 0 then
    begin
      TmpPos := 1;
      Result := UTF16GetNextBuffer(UTF16Buffer, TmpPos, Buffer, Start, ReadCount);
      if TmpPos > 0 then
        Inc(StrPos, Result)
      else
        StrPos := -1;
    end
    else
    begin
      Result := 0;
      FlagInvalidSequence(StrPos, 1);
    end;
  end
  else
    Result := 0;
end;

function AnsiGetNextCharFromStream(S: TStream; CodePage: Word; out Ch: UCS4): Boolean;
var
  B: Byte;
  TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  Result := StreamReadByte(S, B);
  if Result then
  begin
    SetLength(UTF16Buffer, 2);
    if MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_ERR_INVALID_CHARS, @B, 1, PWideChar(UTF16Buffer), 2) <> 0 then
    begin
      TmpPos := 1;
      Ch := UTF16GetNextChar(UTF16Buffer, TmpPos);
      Result := TmpPos <> -1;
    end
    else
    begin
      Result := False;
      Ch := UCS4ReplacementCharacter;
    end;
  end;
end;

function AnsiGetNextBufferFromStream(S: TStream; CodePage: Word; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  B: TDynByteArray;
  ReadSuccess: Boolean;
  ReadCount, TmpPos: SizeInt;
  UTF16Buffer: TUTF16String;
begin
  Result := 0;
  ReadSuccess := True;
  SetLength(B, Count);
  SetLength(UTF16Buffer, 2 * Count);
  while ReadSuccess and (Count > 0) do
  begin
    ReadCount := S.Read(B[0], Count);
    if ReadCount > 0 then
    begin
      ReadCount := MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_ERR_INVALID_CHARS, @B[0], ReadCount, PWideChar(UTF16Buffer), 2 * Count);
      if ReadCount > 0 then
      begin
        TmpPos := 1;
        ReadCount := UTF16GetNextBuffer(UTF16Buffer, TmpPos, Buffer, Start, ReadCount);
        if TmpPos <> -1 then
          Inc(Result, ReadCount)
        else
          ReadSuccess := False;
      end
      else
      begin
        Result := 0;
        FlagInvalidSequence;
      end;
    end
    else
      ReadSuccess := False;
    Dec(Count, ReadCount);
  end;
end;

// AnsiSkipChars = skip NbSeq characters starting from StrPos
// returns False if String is too small
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of chars that were skipped
function AnsiSkipChars(const S: AnsiString; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
var
  StrLen: SizeInt;
begin
  StrLen := Length(S);

  if StrPos > 0 then
  begin
    if StrPos + NbSeq > StrLen then
    begin
      NbSeq := StrLen + 1 - StrPos;
      StrPos := StrLen + 1;
      Result := False;
    end
    else
    begin
      // NbSeq := NbSeq;
      StrPos := StrLen + NbSeq;
      Result := True;
    end;
  end
  else
  begin
    // previous error
    NbSeq := 0;
    // StrPos := -1;
    Result := False;
  end;
end;

function AnsiSkipCharsFromStream(S: TStream; var NbSeq: SizeInt): Boolean;
var
  Index: SizeInt;
  B: Byte;
begin
  Index := 0;
  while Index < NbSeq do
  begin
    Result := StreamReadByte(S, B);
    if not Result then
      Break;
    Inc(Index);
  end;
  Result := Index = NbSeq;
  NbSeq := Index;
end;

// AnsiSetNextChar = append a character at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an ansi string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written (1)
function AnsiSetNextChar(var S: AnsiString; var StrPos: SizeInt; Ch: UCS4): Boolean;
var
  StrLen, TmpPos, AnsiStrLen: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
begin
  StrLen := Length(S);
  Result := (StrPos > 0) and (StrPos <= StrLen);
  if Result then
  begin
    SetLength(UTF16Buffer, 2);
    TmpPos := 1;
    Result := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
    if Result and (TmpPos = 2) then
      // one wide character
      AnsiBuffer := AnsiString(WideString(UTF16Buffer[1]))
    else
    if Result and (TmpPos = 3) then
      // one surrogate pair
      AnsiBuffer := AnsiString(UTF16Buffer)
    else
    begin
      // add ReplacementCharacter
      AnsiBuffer := AnsiReplacementCharacter;
      {$IFDEF UNICODE_SILENT_FAILURE}
      Result := True;
      {$ELSE}
      StrPos := -1;
      {$ENDIF UNICODE_SILENT_FAILURE}
    end;
    AnsiStrLen := Length(AnsiBuffer);
    Result := Result and ((StrPos + AnsiStrLen) <= (StrLen + 1));
    if Result then
    begin
      for TmpPos := 1 to AnsiStrLen do
      begin
        S[StrPos] := AnsiBuffer[TmpPos];
        Inc(StrPos);
      end;
    end;
  end;
end;

function AnsiSetNextBuffer(var S: AnsiString; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  AnsiBuffer: AnsiString;
  UTF16Buffer: WideString;
  StrLen, TmpPos, AnsiStrLen: SizeInt;
  Ch: UCS4;
  Success: Boolean;
begin
  StrLen:= Length(S);
  SetLength(UTF16Buffer, 2);
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];

    TmpPos := 1;
    Success := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
    if Success and (TmpPos = 2) then
      // one wide character
      AnsiBuffer := AnsiString(WideString(UTF16Buffer[1]))
    else
    if Success and (TmpPos = 3) then
      // one surrogate pair
      AnsiBuffer := AnsiString(UTF16Buffer)
    else
    begin
      // add ReplacementCharacter
      AnsiBuffer := AnsiReplacementCharacter;
      {$IFDEF UNICODE_SILENT_FAILURE}
      Success := True;
      {$ELSE}
      StrPos := -1;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
    AnsiStrLen := Length(AnsiBuffer);
    Success := Success and ((StrPos + AnsiStrLen) <= (StrLen + 1));
    if Success then
    begin
      for TmpPos := 1 to AnsiStrLen do
      begin
        S[StrPos] := AnsiBuffer[TmpPos];
        Inc(StrPos);
      end;
    end;
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

function AnsiSetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
var
  TmpPos, I: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
begin
  SetLength(UTF16Buffer, 2);
  TmpPos := 1;
  Result := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);

  if Result and (TmpPos = 2) then
    // one wide character
    AnsiBuffer := AnsiString(WideString(UTF16Buffer[1]))
  else
  if Result and (TmpPos = 3) then
    // one surrogate pair
    AnsiBuffer := AnsiString(UTF16Buffer)
  else
  begin
    // add ReplacementCharacter
    AnsiBuffer := AnsiReplacementCharacter;
    {$IFDEF UNICODE_SILENT_FAILURE}
    Result := True;
    {$ENDIF UNICODE_SILENT_FAILURE}
  end;
  if Result then
    for I := 1 to Length(AnsiBuffer) do
      Result := Result and StreamWriteByte(S, Ord(AnsiBuffer[I]));
end;

function AnsiSetNextBufferToStream(S: TStream; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  TmpPos, I: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
  Success: Boolean;
  Ch: UCS4;
begin
  SetLength(UTF16Buffer, 2);
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];
    TmpPos := 1;
    Success := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);

    if Success and (TmpPos = 2) then
      // one wide character
      AnsiBuffer := AnsiString(WideString(UTF16Buffer[1]))
    else
    if Success and (TmpPos = 3) then
      // one surrogate pair
      AnsiBuffer := AnsiString(UTF16Buffer)
    else
    begin
      // add ReplacementCharacter
      AnsiBuffer := AnsiReplacementCharacter;
      {$IFDEF UNICODE_SILENT_FAILURE}
      Success := True;
      {$ENDIF UNICODE_SILENT_FAILURE}
    end;
    if Success then
      for I := 1 to Length(AnsiBuffer) do
        Success := Success and StreamWriteByte(S, Ord(AnsiBuffer[I]));
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

function AnsiSetNextChar(var S: AnsiString; CodePage: Word; var StrPos: SizeInt; Ch: UCS4): Boolean;
var
  StrLen, TmpPos, AnsiStrLen: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
begin
  StrLen := Length(S);
  Result := (StrPos > 0) and (StrPos <= StrLen);
  if Result then
  begin
    SetLength(UTF16Buffer, 2);
    TmpPos := 1;
    Result := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
    AnsiStrLen := WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, nil, 0, nil, nil);
    SetLength(AnsiBuffer, AnsiStrLen);
    Result := Result and (WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, @AnsiBuffer[1], AnsiStrLen, nil, nil) > 0);
    if not Result then
    begin
      // add ReplacementCharacter
      AnsiBuffer := AnsiReplacementCharacter;
      AnsiStrLen := 1;
      {$IFDEF UNICODE_SILENT_FAILURE}
      Result := True;
      {$ELSE}
      StrPos := -1;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
    Result := Result and ((StrPos + AnsiStrLen) <= (StrLen + 1));
    if Result then
      for TmpPos := 1 to AnsiStrLen do
      begin
        S[StrPos] := AnsiBuffer[TmpPos];
        Inc(StrPos);
      end;
  end;
end;

function AnsiSetNextBuffer(var S: AnsiString; CodePage: Word; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  StrLen, TmpPos, AnsiStrLen: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
  Ch: UCS4;
  Success: Boolean;
begin
  StrLen:= Length(S);
  SetLength(UTF16Buffer, 2);
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];

    TmpPos := 1;
    Success := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
    AnsiStrLen := WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, nil, 0, nil, nil);
    SetLength(AnsiBuffer, AnsiStrLen);
    Success := Success and (WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, @AnsiBuffer[1], AnsiStrLen, nil, nil) > 0);
    if not Success then
    begin
      // add ReplacementCharacter
      AnsiBuffer := AnsiReplacementCharacter;
      AnsiStrLen := 1;
      {$IFDEF UNICODE_SILENT_FAILURE}
      Success := True;
      {$ELSE}
      StrPos := -1;
      {$ENDIF UNICODE_SILENT_FAILURE}
    end;
    Success := Success and ((StrPos + AnsiStrLen) <= (StrLen + 1));
    if Success then
      for TmpPos := 1 to AnsiStrLen do
      begin
        S[StrPos] := AnsiBuffer[TmpPos];
        Inc(StrPos);
      end;
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

function AnsiSetNextCharToStream(S: TStream; CodePage: Word; Ch: UCS4): Boolean;
var
  TmpPos, AnsiStrLen: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
begin
  SetLength(UTF16Buffer, 2);
  TmpPos := 1;
  Result := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
  AnsiStrLen := WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, nil, 0, nil, nil);
  SetLength(AnsiBuffer, AnsiStrLen);
  Result := Result and (WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, @AnsiBuffer[1], AnsiStrLen, nil, nil) > 0);
  if not Result then
  begin
    // add ReplacementCharacter
    AnsiBuffer := AnsiReplacementCharacter;
    AnsiStrLen := 1;
    {$IFDEF UNICODE_SILENT_FAILURE}
    Result := True;
    {$ENDIF UNICODE_SILENT_FAILURE}
  end;
  if Result then
    for TmpPos := 1 to AnsiStrLen do
      Result := Result and StreamWriteByte(S, Ord(AnsiBuffer[TmpPos]));
end;

function AnsiSetNextBufferToStream(S: TStream; CodePage: Word; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt; overload;
var
  TmpPos, AnsiStrLen: SizeInt;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
  Success: Boolean;
  Ch: UCS4;
begin
  SetLength(UTF16Buffer, 2);
  Result := 0;
  Success := True;
  while Success and (Count > 0) do
  begin
    Ch := Buffer[Start];
    TmpPos := 1;
    Success := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
    AnsiStrLen := WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, nil, 0, nil, nil);
    SetLength(AnsiBuffer, AnsiStrLen);
    Success := Success and (WideCharToMultiByte(CodePage, 0, PWideChar(UTF16Buffer), TmpPos-1, @AnsiBuffer[1], AnsiStrLen, nil, nil) > 0);
    if not Success then
    begin
      // add ReplacementCharacter
      AnsiBuffer := AnsiReplacementCharacter;
      AnsiStrLen := 1;
      {$IFDEF UNICODE_SILENT_FAILURE}
      Success := True;
      {$ENDIF UNICODE_SILENT_FAILURE}
    end;
    if Success then
      for TmpPos := 1 to AnsiStrLen do
        Success := Success and StreamWriteByte(S, Ord(AnsiBuffer[TmpPos]));
    if Success then
    begin
      Inc(Start);
      Inc(Result);
    end;
    Dec(Count);
  end;
end;

// StringGetNextChar = read next character/sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were read
function StringGetNextChar(const S: string; var StrPos: SizeInt): UCS4;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16GetNextChar(S, StrPos);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiGetNextChar(S, StrPos);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function StringGetNextBuffer(const S: string; var StrPos: SizeInt; var Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16GetNextBuffer(S, StrPos, Buffer, Start, Count);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiGetNextBuffer(S, StrPos, Buffer, Start, Count);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

// StringSkipChars = skip NbSeq characters/sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function StringSkipChars(const S: string; var StrPos: SizeInt; var NbSeq: SizeInt): Boolean;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16SkipChars(S, StrPos, NbSeq);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiSkipChars(S, StrPos, NbSeq);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

// StringSetNextChar = append a character/sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to a string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function StringSetNextChar(var S: string; var StrPos: SizeInt; Ch: UCS4): Boolean;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16SetNextChar(S, StrPos, Ch);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiSetNextChar(S, StrPos, Ch);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function StringSetNextBuffer(var S: string; var StrPos: SizeInt; const Buffer: TUCS4Array; var Start: SizeInt; Count: SizeInt): SizeInt;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16SetNextBuffer(S, StrPos, Buffer, Start, Count);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiSetNextBuffer(S, StrPos, Buffer, Start, Count);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function WideStringToUTF8(const S: WideString): TUTF8String;
begin
  Result := UTF16ToUTF8(S);
end;

function UTF8ToWideString(const S: TUTF8String): WideString;
begin
  Result := UTF8ToUTF16(S);
end;

function WideStringToUCS4(const S: WideString): TUCS4Array;
begin
  Result := UTF16ToUCS4(S);
end;

function UCS4ToWideString(const S: TUCS4Array): WideString;
begin
  Result := UCS4ToUTF16(S);
end;

function AnsiStringToUTF8(const S: AnsiString): TUTF8String;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUTF8(WS);
end;

function UTF8ToAnsiString(const S: TUTF8String): AnsiString;
var
  WS: TUTF16String;
begin
  WS := UTF8ToUTF16(S);
  Result := AnsiString(WS);
end;

function AnsiStringToUTF16(const S: AnsiString): TUTF16String;
begin
  Result := TUTF16String(S);
end;

function UTF16ToAnsiString(const S: TUTF16String): AnsiString;
begin
  Result := AnsiString(S);
end;

function AnsiStringToUCS4(const S: AnsiString): TUCS4Array;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUCS4(WS);
end;

function UCS4ToAnsiString(const S: TUCS4Array): AnsiString;
var
  WS: TUTF16String;
begin
  WS := UCS4ToUTF16(S);
  Result := AnsiString(WS);
end;

function StringToUTF8(const S: string): TUTF8String;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUTF8(WS);
end;

function TryStringToUTF8(const S: string; out D: TUTF8String): Boolean;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := TryUTF16ToUTF8(WS, D);
end;

function UTF8ToString(const S: TUTF8String): string;
var
  WS: TUTF16String;
begin
  WS := UTF8ToUTF16(S);
  Result := string(WS);
end;

function TryUTF8ToString(const S: TUTF8String; out D: string): Boolean;
var
  WS: TUTF16String;
begin
  Result := TryUTF8ToUTF16(S, WS);
  D := string(WS);
end;

function StringToUTF16(const S: string): TUTF16String;
begin
  Result := TUTF16String(S);
end;

function TryStringToUTF16(const S: string; out D: TUTF16String): Boolean;
begin
  D := TUTF16String(S);
  Result := True;
end;

function UTF16ToString(const S: TUTF16String): string;
begin
  Result := string(S);
end;

function TryUTF16ToString(const S: TUTF16String; out D: string): Boolean;
begin
  D := string(S);
  Result := True;
end;

function StringToUCS4(const S: string): TUCS4Array;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUCS4(WS);
end;

function TryStringToUCS4(const S: string; out D: TUCS4Array): Boolean;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := TryUTF16ToUCS4(WS, D);
end;

function UCS4ToString(const S: TUCS4Array): string;
var
  WS: WideString;
begin
  WS := UCS4ToUTF16(S);
  Result := string(WS);
end;

function TryUCS4ToString(const S: TUCS4Array; out D: string): Boolean;
var
  WS: TUTF16String;
begin
  Result := TryUCS4ToUTF16(S, WS);
  D := string(WS);
end;

function UTF8ToUTF16(const S: TUTF8String): TUTF16String;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      UTF16SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function TryUTF8ToUTF16(const S: TUTF8String; out D: TUTF16String): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  if S = '' then
    D := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(D, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while (SrcIndex > 0) and (SrcIndex <= SrcLength) do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex > 0 then
        UTF16SetNextChar(D, DestIndex, Ch)
      else
        Result := False;
    end;
    if Result then
      SetLength(D, DestIndex - 1) // now fix up length
    else
      D := '';
  end;
end;

function UTF16ToUTF8(const S: TUTF16String): TUTF8String;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength * 3); // worste case

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      UTF8SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function TryUTF16ToUTF8(const S: TUTF16String; out D: TUTF8String): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  if S = '' then
    D := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(D, SrcLength * 3); // worste case

    SrcIndex := 1;
    DestIndex := 1;
    while (SrcIndex > 0) and (SrcIndex <= SrcLength) do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex > 0 then
        UTF8SetNextChar(D, DestIndex, Ch)
      else
        Result := False;
    end;
    if Result then
      SetLength(D, DestIndex - 1) // now fix up length
    else
      D := '';
  end;
end;

function UTF8ToUCS4(const S: TUTF8String): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function TryUTF8ToUCS4(const S: TUTF8String; out D: TUCS4Array): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(D, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while (SrcIndex > 0) and (SrcIndex <= SrcLength) do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex > 0 then
      begin
        D[DestIndex] := Ch;
        Inc(DestIndex);
      end
      else
        Result := False;
    end;
    if Result then
      SetLength(D, DestIndex) // now fix up length
    else
      SetLength(D, 0);
  end;
end;

function UCS4ToUTF8(const S: TUCS4Array): TUTF8String;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
begin
  SrcLength := Length(S);
  if Length(S) = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF8SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function TryUCS4ToUTF8(const S: TUCS4Array; out D: TUTF8String): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
begin
  SrcLength := Length(S);
  Result := True;
  if Length(S) = 0 then
    D := ''
  else
  begin
    SetLength(D, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF8SetNextChar(D, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
      begin
        Result := False;
        Break;
      end;
    end;
    if Result then
      SetLength(D, DestIndex - 1) // set to actual length
    else
      D := '';
  end;
end;

function UTF16ToUCS4(const S: TUTF16String): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function TryUTF16ToUCS4(const S: TUTF16String; out D: TUCS4Array): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(D, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while (SrcIndex > 0) and (SrcIndex <= SrcLength) do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex > 0 then
      begin
        D[DestIndex] := Ch;
        Inc(DestIndex);
      end
      else
        Result := False;
    end;
    if Result then
      SetLength(D, DestIndex) // now fix up length
    else
      SetLength(D, 0);
  end;
end;

function UCS4ToUTF16(const S: TUCS4Array): TUTF16String;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
begin
  SrcLength := Length(S);
  if SrcLength = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF16SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function TryUCS4ToUTF16(const S: TUCS4Array; out D:TUTF16String): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
begin
  SrcLength := Length(S);
  Result := True;
  if SrcLength = 0 then
    D := ''
  else
  begin
    SetLength(D, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF16SetNextChar(D, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
      begin
        Result := False;
        Break;
      end;
    end;

    if Result then
      SetLength(D, DestIndex - 1) // set to actual length
    else
      D := '';
  end;
end;

function UTF8CharCount(const S: TUTF8String): SizeInt;
var
  StrPos: SizeInt;
begin
  StrPos := 1;
  Result := Length(S);
  UTF8SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
end;

function UTF16CharCount(const S: TUTF16String): SizeInt;
var
  StrPos: SizeInt;
begin
  StrPos := 1;
  Result := Length(S);
  UTF16SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
end;

function UCS2CharCount(const S: TUCS2String): SizeInt;
begin
  Result := Length(S);
end;

function UCS4CharCount(const S: TUCS4Array): SizeInt;
begin
  Result := Length(S);
end;

function GetUCS4CharAt(const UTF8Str: TUTF8String; Index: SizeInt; out Value: UCS4): Boolean; overload;
var
  StrPos: SizeInt;
begin
  StrPos := 1;
  Result := Index >= 0;
  if Result then
    Result := UTF8SkipChars(UTF8Str, StrPos, Index);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
  Result := Result and (StrPos <= Length(UTF8Str));
  if Result then
  begin
    Value := UTF8GetNextChar(UTF8Str, StrPos);
    if StrPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
  end;
end;

function GetUCS4CharAt(const WideStr: TUTF16String; Index: SizeInt; out Value: UCS4; IsUTF16: Boolean): Boolean; overload;
var
  StrPos: SizeInt;
begin
  if IsUTF16 then
  begin
    StrPos := 1;
    Result := Index >= 0;
    if Result then
      Result := UTF16SkipChars(WideStr, StrPos, Index);
    if StrPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
    Result := Result and (StrPos <= Length(WideStr));
    if Result then
    begin
      Value := UTF16GetNextChar(WideStr, StrPos);
      if StrPos = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;
  end
  else
  begin
    Result := (Index >= 1) and (Index <= Length(WideStr));
    Value := UCS4(WideStr[Index]);
  end;
end;

function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: SizeInt; out Value: UCS4): Boolean; overload;
begin
  Result := (Index >= 0) and (Index < Length(UCS4Str));
  if Result then
    Value := UCS4Str[Index];
end;

function UCS4ToAnsiChar(Value: UCS4): AnsiChar;
var
  Buf: WideString;
  StrPos: SizeInt;
begin
  StrPos := 1;
  Buf := #0#0;
  if UTF16SetNextChar(Buf, StrPos, Value) then
    Result := AnsiString(Buf)[1]
  else
    Result := AnsiReplacementCharacter;
end;

function UCS4ToWideChar(Value: UCS4): WideChar;
begin
  if Value <= MaximumUCS2 then
    Result := WideChar(Value)
  else
    Result := WideChar(UCS4ReplacementCharacter);
end;

function UCS4ToChar(Value: UCS4): Char;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UCS4ToWideChar(Value);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := UCS4ToAnsiChar(Value);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function AnsiCharToUCS4(Value: AnsiChar): UCS4;
var
  Buf: WideString;
  StrPos: SizeInt;
begin
  StrPos := 1;
  Buf := WideString(AnsiString(Value));
  Result := UTF16GetNextChar(Buf, StrPos);
end;

function WideCharToUCS4(Value: WideChar): UCS4;
begin
  Result := UCS4(Value);
end;

function CharToUCS4(Value: Char): UCS4;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := WideCharToUCS4(Value);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiCharToUCS4(Value);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
