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
{ The Original Code is JclStrings.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Alexander Radchenko                                                                            }
{   Andreas Hausladen (ahuser)                                                                     }
{   Anthony Steele                                                                                 }
{   Azret Botash                                                                                   }
{   Barry Kelly                                                                                    }
{   Huanlin Tsai                                                                                   }
{   Jack N.A. Bakker                                                                               }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   John C Molyneux                                                                                }
{   Kiriakos Vlahos                                                                                }
{   Leonard Wennekers                                                                              }
{   Marcel Bestebroer                                                                              }
{   Martin Kimmings                                                                                }
{   Martin Kubecka                                                                                 }
{   Massimo Maria Ghisalberti                                                                      }
{   Matthias Thoma (mthoma)                                                                        }
{   Michael Winter                                                                                 }
{   Nick Hodges                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Pelle F. S. Liljendal                                                                          }
{   Petr Vones (pvones)                                                                            }
{   Rik Barker (rikbarker)                                                                         }
{   Robert Lee                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Andreas Schmidt                                                                                }
{   Sean Farrow (sfarrow)                                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various character and string routines (searching, testing and transforming)                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStrings;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNICODE_RTL_DATABASE}
  System.Character,
  {$ENDIF UNICODE_RTL_DATABASE}
  System.Classes, System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNICODE_RTL_DATABASE}
  Character,
  {$ENDIF UNICODE_RTL_DATABASE}
  Classes, SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclAnsiStrings,
  JclWideStrings,
  JclBase;

// Exceptions
type
  EJclStringError = class(EJclError);

// Character constants and sets

const
  // Misc. often used character definitions
  NativeNull = Char(#0);
  NativeSoh = Char(#1);
  NativeStx = Char(#2);
  NativeEtx = Char(#3);
  NativeEot = Char(#4);
  NativeEnq = Char(#5);
  NativeAck = Char(#6);
  NativeBell = Char(#7);
  NativeBackspace = Char(#8);
  NativeTab = Char(#9);
  NativeLineFeed = JclBase.NativeLineFeed;
  NativeVerticalTab = Char(#11);
  NativeFormFeed = Char(#12);
  NativeCarriageReturn = JclBase.NativeCarriageReturn;
  NativeCrLf = JclBase.NativeCrLf;
  NativeSo = Char(#14);
  NativeSi = Char(#15);
  NativeDle = Char(#16);
  NativeDc1 = Char(#17);
  NativeDc2 = Char(#18);
  NativeDc3 = Char(#19);
  NativeDc4 = Char(#20);
  NativeNak = Char(#21);
  NativeSyn = Char(#22);
  NativeEtb = Char(#23);
  NativeCan = Char(#24);
  NativeEm = Char(#25);
  NativeEndOfFile = Char(#26);
  NativeEscape = Char(#27);
  NativeFs = Char(#28);
  NativeGs = Char(#29);
  NativeRs = Char(#30);
  NativeUs = Char(#31);
  NativeSpace = Char(' ');
  NativeComma = Char(',');
  NativeBackslash = Char('\');
  NativeForwardSlash = Char('/');

  NativeDoubleQuote = Char('"');
  NativeSingleQuote = Char('''');

  NativeLineBreak = JclBase.NativeLineBreak;

const
  // CharType return values
  C1_UPPER = $0001; // Uppercase
  C1_LOWER = $0002; // Lowercase
  C1_DIGIT = $0004; // Decimal digits
  C1_SPACE = $0008; // Space characters
  C1_PUNCT = $0010; // Punctuation
  C1_CNTRL = $0020; // Control characters
  C1_BLANK = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

  {$IFDEF MSWINDOWS}
  {$IFDEF SUPPORTS_EXTSYM}
  {$EXTERNALSYM C1_UPPER}
  {$EXTERNALSYM C1_LOWER}
  {$EXTERNALSYM C1_DIGIT}
  {$EXTERNALSYM C1_SPACE}
  {$EXTERNALSYM C1_PUNCT}
  {$EXTERNALSYM C1_CNTRL}
  {$EXTERNALSYM C1_BLANK}
  {$EXTERNALSYM C1_XDIGIT}
  {$EXTERNALSYM C1_ALPHA}
  {$ENDIF SUPPORTS_EXTSYM}
  {$ENDIF MSWINDOWS}

type
  TCharValidator = function(const C: Char): Boolean;

function ArrayContainsChar(const Chars: array of Char; const C: Char): Boolean; overload;
function ArrayContainsChar(const Chars: array of Char; const C: Char; out Index: SizeInt): Boolean; overload;

// String Test Routines
function StrIsAlpha(const S: string): Boolean;
function StrIsAlphaNum(const S: string): Boolean;
function StrIsAlphaNumUnderscore(const S: string): Boolean;
function StrContainsChars(const S: string; const Chars: TCharValidator; CheckAll: Boolean): Boolean; overload;
function StrContainsChars(const S: string; const Chars: array of Char; CheckAll: Boolean): Boolean; overload;
function StrConsistsOfNumberChars(const S: string): Boolean;
function StrIsDigit(const S: string): Boolean;
function StrIsSubset(const S: string; const ValidChars: TCharValidator): Boolean; overload;
function StrIsSubset(const S: string; const ValidChars: array of Char): Boolean; overload;
function StrSame(const S1, S2: string; CaseSensitive: Boolean = False): Boolean;

// String Transformation Routines
function StrCenter(const S: string; L: SizeInt; C: Char = ' '): string;
function StrCharPosLower(const S: string; CharPos: SizeInt): string;
function StrCharPosUpper(const S: string; CharPos: SizeInt): string;
function StrDoubleQuote(const S: string): string;
function StrEnsureNoPrefix(const Prefix, Text: string): string;
function StrEnsureNoSuffix(const Suffix, Text: string): string;
function StrEnsurePrefix(const Prefix, Text: string): string;
function StrEnsureSuffix(const Suffix, Text: string): string;
function StrEscapedToString(const S: string): string;
function StrLower(const S: string): string;
procedure StrLowerInPlace(var S: string);
procedure StrLowerBuff(S: PChar);
procedure StrMove(var Dest: string; const Source: string; const ToIndex,
  FromIndex, Count: SizeInt);
function StrPadLeft(const S: string; Len: SizeInt; C: Char = NativeSpace): string;
function StrPadRight(const S: string; Len: SizeInt; C: Char = NativeSpace): string;
function StrProper(const S: string): string;
procedure StrProperBuff(S: PChar);
function StrQuote(const S: string; C: Char): string;
function StrRemoveChars(const S: string; const Chars: TCharValidator): string; overload;
function StrRemoveChars(const S: string; const Chars: array of Char): string; overload;
function StrRemoveLeadingChars(const S: string; const Chars: TCharValidator): string; overload;
function StrRemoveLeadingChars(const S: string; const Chars: array of Char): string; overload;
function StrRemoveEndChars(const S: string; const Chars: TCharValidator): string; overload;
function StrRemoveEndChars(const S: string; const Chars: array of Char): string; overload;
function StrKeepChars(const S: string; const Chars: TCharValidator): string; overload;
function StrKeepChars(const S: string; const Chars: array of Char): string; overload;
procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
function StrReplaceChar(const S: string; const Source, Replace: Char): string;
function StrReplaceChars(const S: string; const Chars: TCharValidator; Replace: Char): string; overload;
function StrReplaceChars(const S: string; const Chars: array of Char; Replace: Char): string; overload;
function StrReplaceButChars(const S: string; const Chars: TCharValidator; Replace: Char): string; overload;
function StrReplaceButChars(const S: string; const Chars: array of Char; Replace: Char): string; overload;
function StrRepeat(const S: string; Count: SizeInt): string;
function StrRepeatLength(const S: string; L: SizeInt): string;
function StrReverse(const S: string): string;
procedure StrReverseInPlace(var S: string);
function StrSingleQuote(const S: string): string;
procedure StrSkipChars(var S: PChar; const Chars: TCharValidator); overload;
procedure StrSkipChars(var S: PChar; const Chars: array of Char); overload;
procedure StrSkipChars(const S: string; var Index: SizeInt; const Chars: TCharValidator); overload;
procedure StrSkipChars(const S: string; var Index: SizeInt; const Chars: array of Char); overload;
function StrSmartCase(const S: string; const Delimiters: TCharValidator): string; overload;
function StrSmartCase(const S: string; const Delimiters: array of Char): string; overload;
function StrStringToEscaped(const S: string): string;
function StrStripNonNumberChars(const S: string): string;
function StrToHex(const Source: string): string;
function StrTrimCharLeft(const S: string; C: Char): string;
function StrTrimCharsLeft(const S: string; const Chars: TCharValidator): string; overload;
function StrTrimCharsLeft(const S: string; const Chars: array of Char): string; overload;
function StrTrimCharRight(const S: string; C: Char): string;
function StrTrimCharsRight(const S: string; const Chars: TCharValidator): string; overload;
function StrTrimCharsRight(const S: string; const Chars: array of Char): string; overload;
function StrTrimQuotes(const S: string): string;
function StrUpper(const S: string): string;
procedure StrUpperInPlace(var S: string);
procedure StrUpperBuff(S: PChar);

// String Management
procedure StrAddRef(var S: string);
procedure StrDecRef(var S: string);
function StrLength(const S: string): SizeInt;
function StrRefCount(const S: string): SizeInt;

// String Search and Replace Routines
function StrCharCount(const S: string; C: Char): SizeInt; overload;
function StrCharsCount(const S: string; const Chars: TCharValidator): SizeInt; overload;
function StrCharsCount(const S: string; const Chars: array of Char): SizeInt; overload;
function StrStrCount(const S, SubS: string): SizeInt;
function StrCompare(const S1, S2: string; CaseSensitive: Boolean = False): SizeInt;
function StrCompareRange(const S1, S2: string; Index, Count: SizeInt; CaseSensitive: Boolean = True): SizeInt;
function StrCompareRangeEx(const S1, S2: string; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
procedure StrFillChar(var S; Count: SizeInt; C: Char);
function StrRepeatChar(C: Char; Count: SizeInt): string;
function StrFind(const Substr, S: string; const Index: SizeInt = 1): SizeInt;
function StrHasPrefix(const S: string; const Prefixes: array of string): Boolean;
function StrHasSuffix(const S: string; const Suffixes: array of string): Boolean;
function StrIndex(const S: string; const List: array of string; CaseSensitive: Boolean = False): SizeInt;
function StrIHasPrefix(const S: string; const Prefixes: array of string): Boolean;
function StrIHasSuffix(const S: string; const Suffixes: array of string): Boolean;
function StrILastPos(const SubStr, S: string): SizeInt;
function StrIPos(const SubStr, S: string): SizeInt;
function StrIPrefixIndex(const S: string; const Prefixes: array of string): SizeInt;
function StrIsOneOf(const S: string; const List: array of string): Boolean;
function StrISuffixIndex(const S: string; const Suffixes: array of string): SizeInt;
function StrLastPos(const SubStr, S: string): SizeInt;
function StrMatch(const Substr, S: string; Index: SizeInt = 1): SizeInt;
function StrMatches(const Substr, S: string; const Index: SizeInt = 1): Boolean;
function StrNIPos(const S, SubStr: string; N: SizeInt): SizeInt;
function StrNPos(const S, SubStr: string; N: SizeInt): SizeInt;
function StrPrefixIndex(const S: string; const Prefixes: array of string): SizeInt;
function StrSearch(const Substr, S: string; const Index: SizeInt = 1): SizeInt;
function StrSuffixIndex(const S: string; const Suffixes: array of string): SizeInt;

// String Extraction
/// Returns the string after SubStr
function StrAfter(const SubStr, S: string): string;
/// Returns the String before SubStr
function StrBefore(const SubStr, S: string): string;
/// Splits a string at SubStr, returns true when SubStr is found, Left contains the
/// string before the SubStr and Right the string behind SubStr
function StrSplit(const SubStr, S: string;var Left, Right : string): boolean;
/// Returns the string between Start and Stop
function StrBetween(const S: string; const Start, Stop: Char): string;
/// Returns all but rightmost N characters of the string
function StrChopRight(const S: string; N: SizeInt): string;{$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}
/// Returns the left Count characters of the string
function StrLeft(const S: string; Count: SizeInt): string; {$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}
/// Returns the string starting from position Start for the Count Characters
function StrMid(const S: string; Start, Count: SizeInt): string; {$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}
/// Returns the string starting from position N to the end
function StrRestOf(const S: string; N: SizeInt): string;{$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}
/// Returns the right Count characters of the string
function StrRight(const S: string; Count: SizeInt): string;{$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}

// Character Test Routines
function CharEqualNoCase(const C1, C2: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsAlpha(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsAlphaNum(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsBlank(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsControl(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsDelete(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsDigit(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsFracDigit(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsHexDigit(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsLower(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsNumberChar(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}
function CharIsNumber(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} {$IFDEF COMPILER16_UP} inline; {$ENDIF} {$ENDIF}
function CharIsPrintable(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsPunctuation(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsReturn(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsSpace(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsUpper(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsValidIdentifierLetter(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsWhiteSpace(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsWildcard(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharType(const C: Char): Word;

// Character Transformation Routines
function CharHex(const C: Char): Byte;
function CharLower(const C: Char): Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharUpper(const C: Char): Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharToggleCase(const C: Char): Char;

// Character Search and Replace
function CharPos(const S: string; const C: Char; const Index: SizeInt = 1): SizeInt;
function CharLastPos(const S: string; const C: Char; const Index: SizeInt = 1): SizeInt;
function CharIPos(const S: string; C: Char; const Index: SizeInt = 1): SizeInt;
function CharReplace(var S: string; const Search, Replace: Char): SizeInt;

// PCharVector
type
  PCharVector = ^PChar;

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
function PCharVectorCount(Source: PCharVector): SizeInt;
procedure PCharVectorToStrings(const Dest: TStrings; Source: PCharVector);
procedure FreePCharVector(var Dest: PCharVector);

// MultiSz Routines
type
  PMultiSz = PChar;
  PAnsiMultiSz = JclAnsiStrings.PAnsiMultiSz;
  PWideMultiSz = JclWideStrings.PWideMultiSz;

  TAnsiStrings = JclAnsiStrings.TJclAnsiStrings;
  TWideStrings = JclWideStrings.TJclWideStrings;
  TAnsiStringList = JclAnsiStrings.TJclAnsiStringList;
  TWideStringList = JclWideStrings.TJclWideStringList;

function StringsToMultiSz(var Dest: PMultiSz; const Source: TStrings): PMultiSz;
procedure MultiSzToStrings(const Dest: TStrings; const Source: PMultiSz);
function MultiSzLength(const Source: PMultiSz): SizeInt;
procedure AllocateMultiSz(var Dest: PMultiSz; Len: SizeInt);
procedure FreeMultiSz(var Dest: PMultiSz);
function MultiSzDup(const Source: PMultiSz): PMultiSz;

function AnsiStringsToAnsiMultiSz(var Dest: PAnsiMultiSz; const Source: TAnsiStrings): PAnsiMultiSz;
 {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure AnsiMultiSzToAnsiStrings(const Dest: TAnsiStrings; const Source: PAnsiMultiSz); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function AnsiMultiSzLength(const Source: PAnsiMultiSz): SizeInt; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure AllocateAnsiMultiSz(var Dest: PAnsiMultiSz; Len: SizeInt); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure FreeAnsiMultiSz(var Dest: PAnsiMultiSz); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function AnsiMultiSzDup(const Source: PAnsiMultiSz): PAnsiMultiSz; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function WideStringsToWideMultiSz(var Dest: PWideMultiSz; const Source: TWideStrings): PWideMultiSz;
 {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure WideMultiSzToWideStrings(const Dest: TWideStrings; const Source: PWideMultiSz); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function WideMultiSzLength(const Source: PWideMultiSz): SizeInt; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure AllocateWideMultiSz(var Dest: PWideMultiSz; Len: SizeInt); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure FreeWideMultiSz(var Dest: PWideMultiSz); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function WideMultiSzDup(const Source: PWideMultiSz): PWideMultiSz; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// TStrings Manipulation
procedure StrIToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean = True): string; overload;
function StringsToStr(const List: TStrings; const Sep: string; const NumberOfItems: SizeInt; const AllowEmptyString:
    Boolean = True): string; overload;
procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True);
procedure TrimStringsRight(const List: TStrings; DeleteIfEmpty: Boolean = True);
procedure TrimStringsLeft(const List: TStrings; DeleteIfEmpty: Boolean = True);
function AddStringToStrings(const S: string; Strings: TStrings; const Unique: Boolean): Boolean;

// Miscellaneous
// (OF) moved to JclSysUtils
// function BooleanToStr(B: Boolean): string;
 // AnsiString here because it is binary data
function FileToString(const FileName: string): {$IFDEF COMPILER12_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
procedure StringToFile(const FileName: string; const Contents: {$IFDEF COMPILER12_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
  Append: Boolean = False);

function StrToken(var S: string; Separator: Char): string;
procedure StrTokens(const S: string; const List: TStrings);
procedure StrTokenToStrings(S: string; Separator: Char; const List: TStrings);
function StrWord(const S: string; var Index: SizeInt; out Word: string): Boolean; overload;
function StrWord(var S: PChar; out Word: string): Boolean; overload;
function StrIdent(const S: string; var Index: SizeInt; out Ident: string): Boolean; overload;
function StrIdent(var S: PChar; out Ident: string): Boolean; overload;
function StrToFloatSafe(const S: string): Float;
function StrToIntSafe(const S: string): Integer;
procedure StrNormIndex(const StrLen: SizeInt; var Index: SizeInt; var Count: SizeInt); overload;

function ArrayOf(List: TStrings): TDynStringArray; overload;

type
  FormatException = class(EJclError);
  ArgumentException = class(EJclError);
  ArgumentNullException = class(EJclError);
  ArgumentOutOfRangeException = class(EJclError);

  IToString = interface
    ['{C4ABABB4-1029-46E7-B5FA-99800F130C05}']
    function ToString: string;
  end;

  TCharDynArray = array of Char;

  // The TStringBuilder class is a Delphi implementation of the .NET
  // System.Text.StringBuilder.
  // It is zero based and the method that allow an TObject (Append, Insert,
  // AppendFormat) are limited to IToString implementors or Delphi 2009+ RTL.
  // This class is not threadsafe. Any instance of TStringBuilder should not
  // be used in different threads at the same time.
  TJclStringBuilder = class(TInterfacedObject, IToString)
  private
    FChars: TCharDynArray;
    FLength: SizeInt;
    FMaxCapacity: SizeInt;

    function GetCapacity: SizeInt;
    procedure SetCapacity(const Value: SizeInt);
    function GetChars(Index: SizeInt): Char;
    procedure SetChars(Index: SizeInt; const Value: Char);
    procedure Set_Length(const Value: SizeInt);
  protected
    function AppendPChar(Value: PChar; Count: SizeInt; RepeatCount: SizeInt = 1): TJclStringBuilder;
    function InsertPChar(Index: SizeInt; Value: PChar; Count: SizeInt; RepeatCount: SizeInt = 1): TJclStringBuilder;
  public
    constructor Create(const Value: string; Capacity: SizeInt = 16); overload;
    constructor Create(Capacity: SizeInt = 16; MaxCapacity: SizeInt = MaxInt); overload;
    constructor Create(const Value: string; StartIndex, Length, Capacity: SizeInt); overload;

    function Append(const Value: string): TJclStringBuilder; overload;
    function Append(const Value: string; StartIndex, Length: SizeInt): TJclStringBuilder; overload;
    function Append(Value: Boolean): TJclStringBuilder; overload;
    function Append(Value: Char; RepeatCount: SizeInt = 1): TJclStringBuilder; overload;
    function Append(const Value: array of Char): TJclStringBuilder; overload;
    function Append(const Value: array of Char; StartIndex, Length: SizeInt): TJclStringBuilder; overload;
    function Append(Value: Cardinal): TJclStringBuilder; overload;
    function Append(Value: Integer): TJclStringBuilder; overload;
    function Append(Value: Double): TJclStringBuilder; overload;
    function Append(Value: Int64): TJclStringBuilder; overload;
    function Append(Obj: TObject): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; const Args: array of const): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; Arg0: Variant): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; Arg0, Arg1: Variant): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; Arg0, Arg1, Arg2: Variant): TJclStringBuilder; overload;

    function Insert(Index: SizeInt; const Value: string; Count: SizeInt = 1): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; Value: Boolean): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; const Value: array of Char): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; const Value: array of Char; StartIndex, Length: SizeInt): TJclStringBuilder;
      overload;
    function Insert(Index: SizeInt; Value: Cardinal): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; Value: Integer): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; Value: Double): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; Value: Int64): TJclStringBuilder; overload;
    function Insert(Index: SizeInt; Obj: TObject): TJclStringBuilder; overload;

    function Replace(OldChar, NewChar: Char; StartIndex: SizeInt = 0; Count: SizeInt = -1): TJclStringBuilder;
      overload;
    function Replace(OldValue, NewValue: string; StartIndex: SizeInt = 0; Count: SizeInt = -1): TJclStringBuilder;
      overload;

    function Remove(StartIndex, Length: SizeInt): TJclStringBuilder;
    function EnsureCapacity(Capacity: SizeInt): SizeInt;
    procedure Clear;

    { IToString }
    function ToString: string; {$IFDEF RTL200_UP} override; {$ENDIF RTL200_UP}

    property __Chars__[Index: SizeInt]: Char read GetChars write SetChars; default;
    property Chars: TCharDynArray read FChars;
    property Length: SizeInt read FLength write Set_Length;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    property MaxCapacity: SizeInt read FMaxCapacity;
  end;

  {$IFDEF RTL200_UP}
  TStringBuilder = {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.TStringBuilder;
  {$ELSE ~RTL200_UP}
  TStringBuilder = TJclStringBuilder;
  {$ENDIF ~RTL200_UP}

// DotNetFormat() uses the .NET format style: "{argX}"
function DotNetFormat(const Fmt: string; const Args: array of const): string; overload;
function DotNetFormat(const Fmt: string; const Arg0: Variant): string; overload;
function DotNetFormat(const Fmt: string; const Arg0, Arg1: Variant): string; overload;
function DotNetFormat(const Fmt: string; const Arg0, Arg1, Arg2: Variant): string; overload;

// TJclTabSet
type
  TJclTabSet = class (TInterfacedObject, IToString)
  private
    FData: TObject;
    function GetCount: SizeInt;
    function GetStops(Index: SizeInt): SizeInt;
    function GetTabWidth: SizeInt;
    function GetZeroBased: Boolean;
    procedure SetStops(Index, Value: SizeInt);
    procedure SetTabWidth(Value: SizeInt);
    procedure SetZeroBased(Value: Boolean);
  protected
    function FindStop(Column: SizeInt): SizeInt;
    function InternalTabStops: TDynSizeIntArray;
    function InternalTabWidth: SizeInt;
    procedure RemoveAt(Index: SizeInt);
  public
    constructor Create; overload;
    constructor Create(Data: TObject); overload;
    constructor Create(TabWidth: SizeInt); overload;
    constructor Create(const Tabstops: array of SizeInt; ZeroBased: Boolean); overload;
    constructor Create(const Tabstops: array of SizeInt; ZeroBased: Boolean; TabWidth: SizeInt); overload;
    destructor Destroy; override;

    // cloning and referencing
    function Clone: TJclTabSet;
    function NewReference: TJclTabSet;

    // Tab stops manipulation
    function Add(Column: SizeInt): SizeInt;
    function Delete(Column: SizeInt): SizeInt;

    // Usage
    function Expand(const S: string): string; overload;
    function Expand(const S: string; Column: SizeInt): string; overload;
    procedure OptimalFillInfo(StartColumn, TargetColumn: SizeInt; out TabsNeeded, SpacesNeeded: SizeInt);
    function Optimize(const S: string): string; overload;
    function Optimize(const S: string; Column: SizeInt): string; overload;
    function StartColumn: SizeInt;
    function TabFrom(Column: SizeInt): SizeInt;
    function UpdatePosition(const S: string): SizeInt; overload;
    function UpdatePosition(const S: string; Column: SizeInt): SizeInt; overload;
    function UpdatePosition(const S: string; var Column, Line: SizeInt): SizeInt; overload;

    { IToString }
    function ToString: string; overload; {$IFDEF RTL200_UP} override; {$ENDIF RTL200_UP}
    // Conversions
    function ToString(FormattingOptions: SizeInt): string; {$IFDEF RTL200_UP} reintroduce; {$ENDIF RTL200_UP} overload;
    class function FromString(const S: string): TJclTabSet; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF SUPPORTS_STATIC}

    // Properties
    property ActualTabWidth: SizeInt read InternalTabWidth;
    property Count: SizeInt read GetCount;
    property TabStops[Index: SizeInt]: SizeInt read GetStops write SetStops; default;
    property TabWidth: SizeInt read GetTabWidth write SetTabWidth;
    property ZeroBased: Boolean read GetZeroBased write SetZeroBased;
  end;

// Formatting constants
const
  TabSetFormatting_SurroundStopsWithBrackets = 1;
  TabSetFormatting_EmptyBracketsIfNoStops = 2;
  TabSetFormatting_NoTabStops = 4;
  TabSetFormatting_NoTabWidth = 8;
  TabSetFormatting_AutoTabWidth = 16;
  // common combinations
  TabSetFormatting_Default = 0;
  TabSetFormatting_AlwaysUseBrackets = TabSetFormatting_SurroundStopsWithBrackets or
    TabSetFormatting_EmptyBracketsIfNoStops;
  TabSetFormatting_Full = TabSetFormatting_AlwaysUseBrackets or TabSetFormatting_AutoTabWidth;
  // aliases
  TabSetFormatting_StopsOnly = TabSetFormatting_NoTabWidth;
  TabSetFormatting_TabWidthOnly = TabSetFormatting_NoTabStops;
  TabSetFormatting_StopsWithoutBracketsAndTabWidth = TabSetFormatting_Default;

// Tab expansion routines
function StrExpandTabs(S: string): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF} overload;
function StrExpandTabs(S: string; TabWidth: SizeInt): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF} overload;
function StrExpandTabs(S: string; TabSet: TJclTabSet): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF} overload;
// Tab optimization routines
function StrOptimizeTabs(S: string): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF} overload;
function StrOptimizeTabs(S: string; TabWidth: SizeInt): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF} overload;
function StrOptimizeTabs(S: string; TabSet: TJclTabSet): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF} overload;

// move to JclBase?
type
  NullReferenceException = class(EJclError)
  public
    constructor Create; overload;
  end;

procedure StrResetLength(var S: WideString); overload;
procedure StrResetLength(var S: AnsiString); overload;
procedure StrResetLength(S: TJclStringBuilder); overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
procedure StrResetLength(var S: UnicodeString); overload;
{$ENDIF SUPPORTS_UNICODE_STRING}

// natural comparison functions
function CompareNaturalStr(const S1, S2: string): SizeInt; overload;
function CompareNaturalText(const S1, S2: string): SizeInt; overload;

{$IFNDEF UNICODE_RTL_DATABASE}
// internal structures published to make function inlining working
const
  MaxStrCharCount = Ord(High(Char)) + 1;       // # of chars in one set
  StrLoOffset = MaxStrCharCount * 0;       // offset to lower case chars
  StrUpOffset = MaxStrCharCount * 1;       // offset to upper case chars
  StrReOffset = MaxStrCharCount * 2;       // offset to reverse case chars
  StrCaseMapSize = MaxStrCharCount * 3;       // # of chars is a table

var
  StrCaseMap: array [0..StrCaseMapSize - 1] of Char; // case mappings
  StrCaseMapReady: Boolean = False;         // true if case map exists
  StrCharTypes: array [Char] of Word;
{$ENDIF ~UNICODE_RTL_DATABASE}

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
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF SUPPORTS_UNICODE}
  {$IFDEF HAS_UNITSCOPE}
  System.StrUtils,
  {$ELSE ~HAS_UNITSCOPE}
  StrUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  {$ENDIF SUPPORTS_UNICODE}
  JclLogic, JclResources, JclStreams, JclSynch, JclSysUtils;

//=== Internal ===============================================================

type
  TStrRec = packed record
    RefCount: Longint;
    Length: Longint;
  end;
  PStrRec = ^TStrRec;

{$IFNDEF UNICODE_RTL_DATABASE}
procedure LoadCharTypes;
var
  CurrChar: Char;
  CurrType: Word;
begin
  for CurrChar := Low(CurrChar) to High(CurrChar) do
  begin
    {$IFDEF MSWINDOWS}
    CurrType := 0;
    GetStringTypeEx(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, 1, CurrType);
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    CurrType := 0;
    if isupper(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_UPPER;
    if islower(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_LOWER;
    if isdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_DIGIT;
    if isspace(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_SPACE;
    if ispunct(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_PUNCT;
    if iscntrl(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_CNTRL;
    if isblank(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_BLANK;
    if isxdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_XDIGIT;
    if isalpha(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_ALPHA;
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF LINUX}
    StrCharTypes[CurrChar] := CurrType;
    {$IFNDEF CHAR_TYPES_INITIALIZED}
    Implement case map initialization here
    {$ENDIF ~CHAR_TYPES_INITIALIZED}
  end;
end;

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: Char;
begin
  if not StrCaseMapReady then
  begin
    for CurrChar := Low(Char) to High(Char) do
    begin
      {$IFDEF MSWINDOWS}
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CharLowerBuff(@LoCaseChar, 1);
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CharUpperBuff(@UpCaseChar, 1);
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      LoCaseChar := Char(tolower(Byte(CurrChar)));
      UpCaseChar := Char(toupper(Byte(CurrChar)));
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF LINUX}
      {$IFNDEF CASE_MAP_INITIALIZED}
      Implement case map initialization here
      {$ENDIF ~CASE_MAP_INITIALIZED}
      if CharIsUpper(CurrChar) then
        ReCaseChar := LoCaseChar
      else
      if CharIsLower(CurrChar) then
        ReCaseChar := UpCaseChar
      else
        ReCaseChar := CurrChar;
      StrCaseMap[Ord(CurrChar) + StrLoOffset] := LoCaseChar;
      StrCaseMap[Ord(CurrChar) + StrUpOffset] := UpCaseChar;
      StrCaseMap[Ord(CurrChar) + StrReOffset] := ReCaseChar;
    end;
    StrCaseMapReady := True;
  end;
end;

// Uppercases or Lowercases a give string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCase(var Str: string; const Offset: SizeInt);
var
  P: PChar;
  I, L: SizeInt;
begin
  L := Length(Str);
  if L > 0 then
  begin
    UniqueString(Str);
    P := PChar(Str);
    for I := 1 to L do
    begin
      P^ := StrCaseMap[Offset + Ord(P^)];
      Inc(P);
    end;
  end;
end;

// Internal utility function
// Uppercases or Lowercases a give null terminated string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCaseBuff(S: PChar; const Offset: SizeInt);
var
  C: Char;
begin
  if S <> nil then
  begin
    repeat
      C := S^;
      S^ := StrCaseMap[Offset + Ord(C)];
      Inc(S);
    until C = #0;
  end;
end;
{$ENDIF ~UNICODE_RTL_DATABASE}

function StrEndW(Str: PWideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> #0 do
    Inc(Result);
end;

function ArrayContainsChar(const Chars: array of Char; const C: Char): Boolean;
var
  idx: SizeInt;
begin
  Result := ArrayContainsChar(Chars, C, idx);
end;

function ArrayContainsChar(const Chars: array of Char; const C: Char; out Index: SizeInt): Boolean;
{ optimized version for sorted arrays
var
  I, L, H: SizeInt;
begin
  L := Low(Chars);
  H := High(Chars);
  while L <= H do
  begin
    I := (L + H) div 2;
    if C = Chars[I] then
    begin
      Result := True;
      Exit;
    end
    else
    if C < Chars[I] then
      H := I - 1
    else
      // C > Chars[I]
      L := I + 1;
  end;
  Result := False;
end;}
begin
  Index := High(Chars);
  while (Index >= Low(Chars)) and (Chars[Index] <> C) do
    Dec(Index);
  Result := Index >= Low(Chars);
end;

// String Test Routines
function StrIsAlpha(const S: string): Boolean;
var
  I: SizeInt;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlpha(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsAlphaNum(const S: string): Boolean;
var
  I: SizeInt;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrConsistsofNumberChars(const S: string): Boolean;
var
  I: SizeInt;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsNumberChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrContainsChars(const S: string; const Chars: TCharValidator; CheckAll: Boolean): Boolean;
var
  I: SizeInt;
begin
  Result := False;
  if CheckAll then
  begin
    // this will not work with the current definition of the validator. The validator would need to check each character
    // it requires against the string (which is currently not provided to the Validator). The current implementation of
    // CheckAll will check if all characters in S will be accepted by the provided Validator, which is wrong and incon-
    // sistent with the documentation and the array-based overload.
    for I := 1 to Length(S) do
    begin
      Result := Chars(S[I]);
      if not Result then
        Break;
    end;
  end
  else
  begin
    for I := 1 to Length(S) do
    begin
      Result := Chars(S[I]);
      if Result then
        Break;
    end;
  end;
end;

function StrContainsChars(const S: string; const Chars: array of Char; CheckAll: Boolean): Boolean;
var
  I: SizeInt;
begin
  if CheckAll then
  begin
    Result := True;
    I := High(Chars);
    while (I >= 0) and Result do
    begin
      Result := CharPos(S, Chars[I]) > 0;
      Dec(I);
    end;
  end
  else
  begin
    Result := False;
    for I := 1 to Length(S) do
    begin
      Result := ArrayContainsChar(Chars, S[I]);
      if Result then
        Break;
    end;
  end;
end;

function StrIsAlphaNumUnderscore(const S: string): Boolean;
var
  I: SizeInt;
  C: Char;
begin
  for I := 1 to Length(S) do
  begin
    C := S[I];

    if not (CharIsAlphaNum(C) or (C = '_')) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := Length(S) > 0;
end;

function StrIsDigit(const S: string): Boolean;
var
  I: SizeInt;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsDigit(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsSubset(const S: string; const ValidChars: TCharValidator): Boolean;
var
  I: SizeInt;
begin
  for I := 1 to Length(S) do
  begin
    Result := ValidChars(S[I]);
    if not Result then
      Exit;
  end;

  Result := Length(S) > 0;
end;

function StrIsSubset(const S: string; const ValidChars: array of Char): Boolean;
var
  I: SizeInt;
begin
  for I := 1 to Length(S) do
  begin
    Result := ArrayContainsChar(ValidChars, S[I]);
    if not Result then
      Exit;
  end;

  Result := Length(S) > 0;
end;

function StrSame(const S1, S2: string; CaseSensitive: Boolean): Boolean;
begin
  Result := StrCompare(S1, S2, CaseSensitive) = 0;
end;

//=== String Transformation Routines =========================================

function StrCenter(const S: string; L: SizeInt; C: Char = ' '): string;
begin
  if Length(S) < L then
  begin
    Result := StringOfChar(C, (L - Length(S)) div 2) + S;
    Result := Result + StringOfChar(C, L - Length(Result));
  end
  else
    Result := S;
end;

function StrCharPosLower(const S: string; CharPos: SizeInt): string;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharLower(Result[CharPos]);
end;

function StrCharPosUpper(const S: string; CharPos: SizeInt): string;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharUpper(Result[CharPos]);
end;

function StrDoubleQuote(const S: string): string;
begin
  Result := NativeDoubleQuote + S + NativeDoubleQuote;
end;

function StrEnsureNoPrefix(const Prefix, Text: string): string;
var
  PrefixLen: SizeInt;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Copy(Text, PrefixLen + 1, Length(Text))
  else
    Result := Text;
end;

function StrEnsureNoSuffix(const Suffix, Text: string): string;
var
  SuffixLen: SizeInt;
  StrLength: SizeInt;
begin
  SuffixLen := Length(Suffix);
  StrLength := Length(Text);
  if Copy(Text, StrLength - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Copy(Text, 1, StrLength - SuffixLen)
  else
    Result := Text;
end;

function StrEnsurePrefix(const Prefix, Text: string): string;
var
  PrefixLen: SizeInt;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Text
  else
    Result := Prefix + Text;
end;

function StrEnsureSuffix(const Suffix, Text: string): string;
var
  SuffixLen: SizeInt;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

function StrEscapedToString(const S: string): string;
  procedure HandleHexEscapeSeq(const S: string; var I: SizeInt; Len: SizeInt; var Dest: string);
  const
    HexDigits = string('0123456789abcdefABCDEF');
  var
    StartI, Val, N: SizeInt;
  begin
    StartI := I;
    N := Pos(S[I + 1], HexDigits) - 1;
    if N < 0 then
      // '\x' without hex digit following is not escape sequence
      Dest := Dest + '\x'
    else
    begin
      Inc(I); // Jump over x
      if N >= 16 then
        N := N - 6;
      Val := N;
      // Same for second digit
      if I < Len then
      begin
        N := Pos(S[I + 1], HexDigits) - 1;
        if N >= 0 then
        begin
          Inc(I); // Jump over first digit
          if N >= 16 then
            N := N - 6;
          Val := Val * 16 + N;
        end;
      end;

      if Val > Ord(High(Char)) then
        raise EJclStringError.CreateResFmt(@RsNumericConstantTooLarge, [Val, StartI]);

      Dest := Dest + Char(Val);
    end;
  end;

  procedure HandleOctEscapeSeq(const S: string; var I: SizeInt; Len: SizeInt; var Dest: string);
  const
    OctDigits = string('01234567');
  var
    StartI, Val, N: SizeInt;
  begin
    StartI := I;
    // first digit
    Val := Pos(S[I], OctDigits) - 1;
    if I < Len then
    begin
      N := Pos(S[I + 1], OctDigits) - 1;
      if N >= 0 then
      begin
        Inc(I);
        Val := Val * 8 + N;
      end;
      if I < Len then
      begin
        N := Pos(S[I + 1], OctDigits) - 1;
        if N >= 0 then
        begin
          Inc(I);
          Val := Val * 8 + N;
        end;
      end;
    end;

    if Val > Ord(High(Char)) then
      raise EJclStringError.CreateResFmt(@RsNumericConstantTooLarge, [Val, StartI]);

    Dest := Dest + Char(Val);
  end;

var
  I, Len: SizeInt;
begin
  Result := '';
  I := 1;
  Len := Length(S);
  while I <= Len do
  begin
    if not ((S[I] = '\') and (I < Len)) then
      Result := Result + S[I]
    else
    begin
      Inc(I); // Jump over escape character
      case S[I] of
        'a':
          Result := Result + NativeBell;
        'b':
          Result := Result + NativeBackspace;
        'f':
          Result := Result + NativeFormFeed;
        'n':
          Result := Result + NativeLineFeed;
        'r':
          Result := Result + NativeCarriageReturn;
        't':
          Result := Result + NativeTab;
        'v':
          Result := Result + NativeVerticalTab;
        '\':
          Result := Result + '\';
        '"':
          Result := Result + '"';
        '''':
          Result := Result + ''''; // Optionally escaped
        '?':
          Result := Result + '?';  // Optionally escaped
        'x':
          if I < Len then
            // Start of hex escape sequence
            HandleHexEscapeSeq(S, I, Len, Result)
          else
            // '\x' at end of string is not escape sequence
            Result := Result + '\x';
        '0'..'7':
          // start of octal escape sequence
          HandleOctEscapeSeq(S, I, Len, Result);
      else
        // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
end;

function StrLower(const S: string): string;
begin
  Result := S;
  StrLowerInPlace(Result);
end;

procedure StrLowerInPlace(var S: string);
{$IFDEF UNICODE_RTL_DATABASE}
var
  P: PChar;
  I, L: SizeInt;
begin
  L := Length(S);
  if L > 0 then
  begin
    UniqueString(S);
    P := PChar(S);
    for I := 1 to L do
    begin
      P^ := TCharacter.ToLower(P^);
      Inc(P);
    end;
  end;
end;
{$ELSE ~UNICODE_RTL_DATABASE}
begin
  StrCase(S, StrLoOffset);
end;
{$ENDIF ~UNICODE_RTL_DATABASE}

procedure StrLowerBuff(S: PChar);
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  if S <> nil then
  begin
    repeat
      S^ := TCharacter.ToLower(S^);
      Inc(S);
    until S^ = #0;
  end;
  {$ELSE ~UNICODE_RTL_DATABASE}
  StrCaseBuff(S, StrLoOffset);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

procedure StrMove(var Dest: string; const Source: string;
  const ToIndex, FromIndex, Count: SizeInt);
begin
  // Check strings
  if (Source = '') or (Length(Dest) = 0) then
    Exit;

  // Check FromIndex
  if (FromIndex <= 0) or (FromIndex > Length(Source)) or
    (ToIndex <= 0) or (ToIndex > Length(Dest)) or
    ((FromIndex + Count - 1) > Length(Source)) or ((ToIndex + Count - 1) > Length(Dest)) then
     { TODO : Is failure without notice the proper thing to do here? }
    Exit;

  // Move
  Move(Source[FromIndex], Dest[ToIndex], Count * SizeOf(Char));
end;

function StrPadLeft(const S: string; Len: SizeInt; C: Char): string;
var
  L: SizeInt;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else
    Result := S;
end;

function StrPadRight(const S: string; Len: SizeInt; C: Char): string;
var
  L: SizeInt;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else
    Result := S;
end;

function StrProper(const S: string): string;
begin
  Result := StrLower(S);
  if Result <> '' then
    Result[1] := UpCase(Result[1]);
end;

procedure StrProperBuff(S: PChar);
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    StrLowerBuff(S);
    S^ := CharUpper(S^);
  end;
end;

function StrQuote(const S: string; C: Char): string;
var
  L: SizeInt;
begin
  L := Length(S);
  Result := S;
  if L > 0 then
  begin
    if Result[1] <> C then
    begin
      Result := C + Result;
      Inc(L);
    end;
    if Result[L] <> C then
      Result := Result + C;
  end;
end;

function StrRemoveChars(const S: string; const Chars: TCharValidator): string;
var
  Source, Dest: PChar;
  Len, Index:   SizeInt;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if not Chars(Source^) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
end;

function StrRemoveChars(const S: string; const Chars: array of Char): string;
var
  Source, Dest: PChar;
  Len, Index:   SizeInt;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if not ArrayContainsChar(Chars, Source^) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
end;

function StrRemoveLeadingChars(const S: string; const Chars: TCharValidator): string;
var
  Len : SizeInt;
  I: SizeInt;
begin
  Len := Length(S);
  I := 1;
  while (I <= Len) and Chars(s[I]) do
    Inc(I);
  Result := Copy (s, I, Len-I+1);
end;

function StrRemoveLeadingChars(const S: string; const Chars: array of Char): string;
var
  Len : SizeInt;
  I: SizeInt;
begin
  Len := Length(S);
  I := 1;
  while (I <= Len) and ArrayContainsChar(Chars, s[I]) do
    Inc(I);
  Result := Copy (s, I, Len-I+1);
end;

function StrRemoveEndChars(const S: string; const Chars: TCharValidator): string;
var
  Len :   SizeInt;
begin
  Len := Length(S);
  while (Len > 0) and Chars(s[Len]) do
    Dec(Len);
  Result := Copy (s, 1, Len);
end;

function StrRemoveEndChars(const S: string; const Chars: array of Char): string;
var
  Len :   SizeInt;
begin
  Len := Length(S);
  while (Len > 0) and ArrayContainsChar(Chars, s[Len]) do
    Dec(Len);
  Result := Copy (s, 1, Len);
end;

function StrKeepChars(const S: string; const Chars: TCharValidator): string;
var
  Source, Dest: PChar;
  Len, Index:   SizeInt;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if Chars(Source^) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
end;

function StrKeepChars(const S: string; const Chars: array of Char): string;
var
  Source, Dest: PChar;
  Len, Index:   SizeInt;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if ArrayContainsChar(Chars, Source^) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
end;

function StrRepeat(const S: string; Count: SizeInt): string;
var
  Len, Index: SizeInt;
  Dest, Source: PChar;
begin
  Len := Length(S);
  SetLength(Result, Count * Len);
  Dest := PChar(Result);
  Source := PChar(S);
  if Dest <> nil then
    for Index := 0 to Count - 1 do
    begin
      Move(Source^, Dest^, Len * SizeOf(Char));
      Inc(Dest, Len);
    end;
end;

function StrRepeatLength(const S: string; L: SizeInt): string;
var
  Len: SizeInt;
  Dest: PChar;
begin
  Result := '';
  Len := Length(S);

  if (Len > 0) and (S <> '') then
  begin
    SetLength(Result, L);
    Dest := PChar(Result);
    while (L > 0) do
    begin
      Move(S[1], Dest^, Min(L, Len) * SizeOf(Char));
      Inc(Dest, Len);
      Dec(L, Len);
    end;
  end;
end;

procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags);
var
  SearchStr: string;
  ResultStr: string; { result string }
  SourcePtr: PChar;      { pointer into S of character under examination }
  SourceMatchPtr: PChar; { pointers into S and Search when first character has }
  SearchMatchPtr: PChar; { been matched and we're probing for a complete match }
  ResultPtr: PChar;      { pointer into Result of character being written }
  ResultIndex,
  SearchLength,          { length of search string }
  ReplaceLength,         { length of replace string }
  BufferLength,          { length of temporary result buffer }
  ResultLength: SizeInt; { length of result string }
  C: Char;               { first character of search string }
  IgnoreCase: Boolean;
begin
  if Search = '' then
  begin
    if S = '' then
    begin
      S := Replace;
      Exit;
    end
    else
      raise EJclStringError.CreateRes(@RsBlankSearchString);
  end;

  if S <> '' then
  begin
    IgnoreCase := rfIgnoreCase in Flags;
    if IgnoreCase then
      SearchStr := StrUpper(Search)
    else
      SearchStr := Search;
    { avoid having to call Length() within the loop }
    SearchLength := Length(Search);
    ReplaceLength := Length(Replace);
    ResultLength := Length(S);
    BufferLength := ResultLength;
    SetLength(ResultStr, BufferLength);
    { get pointers to begin of source and result }
    ResultPtr := PChar(ResultStr);
    SourcePtr := PChar(S);
    C := SearchStr[1];
    { while we haven't reached the end of the string }
    while True do
    begin
      { copy characters until we find the first character of the search string }
      if IgnoreCase then
        while (CharUpper(SourcePtr^) <> C) and (SourcePtr^ <> #0) do
        begin
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end
      else
        while (SourcePtr^ <> C) and (SourcePtr^ <> #0) do
        begin
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end;
      { did we find that first character or did we hit the end of the string? }
      if SourcePtr^ = #0 then
        Break
      else
      begin
        { continue comparing, +1 because first character was matched already }
        SourceMatchPtr := SourcePtr + 1;
        SearchMatchPtr := PChar(SearchStr) + 1;
        if IgnoreCase then
          while (CharUpper(SourceMatchPtr^) = SearchMatchPtr^) and (SearchMatchPtr^ <> #0) do
          begin
            Inc(SourceMatchPtr);
            Inc(SearchMatchPtr);
          end
        else
          while (SourceMatchPtr^ = SearchMatchPtr^) and (SearchMatchPtr^ <> #0) do
          begin
            Inc(SourceMatchPtr);
            Inc(SearchMatchPtr);
          end;
        { did we find a complete match? }
        if SearchMatchPtr^ = #0 then
        begin
          // keep track of result length
          Inc(ResultLength, ReplaceLength - SearchLength);
          if ReplaceLength > 0 then
          begin
            // increase buffer size if required
            if ResultLength > BufferLength then
            begin
              BufferLength := ResultLength * 2;
              ResultIndex := ResultPtr - PChar(ResultStr) + 1;
              SetLength(ResultStr, BufferLength);
              ResultPtr := @ResultStr[ResultIndex];
            end;
            { append replace to result and move past the search string in source }
            Move((@Replace[1])^, ResultPtr^, ReplaceLength * SizeOf(Char));
          end;
          Inc(SourcePtr, SearchLength);
          Inc(ResultPtr, ReplaceLength);
          { replace all instances or just one? }
          if not (rfReplaceAll in Flags) then
          begin
            { just one, copy until end of source and break out of loop }
            while SourcePtr^ <> #0 do
            begin
              ResultPtr^ := SourcePtr^;
              Inc(ResultPtr);
              Inc(SourcePtr);
            end;
            Break;
          end;
        end
        else
        begin
          { copy current character and start over with the next }
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end;
      end;
    end;
    { set result length and copy result into S }
    SetLength(ResultStr, ResultLength);
    S := ResultStr;
  end;
end;

function StrReplaceChar(const S: string; const Source, Replace: Char): string;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] = Source then
      Result[I] := Replace;
end;

function StrReplaceChars(const S: string; const Chars: TCharValidator; Replace: Char): string;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Chars(Result[I]) then
      Result[I] := Replace;
end;

function StrReplaceChars(const S: string; const Chars: array of Char; Replace: Char): string;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if ArrayContainsChar(Chars, Result[I]) then
      Result[I] := Replace;
end;

function StrReplaceButChars(const S: string; const Chars: TCharValidator;
  Replace: Char): string;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if not Chars(Result[I]) then
      Result[I] := Replace;
end;

function StrReplaceButChars(const S: string; const Chars: array of Char; Replace: Char): string;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if not ArrayContainsChar(Chars, Result[I]) then
      Result[I] := Replace;
end;

function StrReverse(const S: string): string;
begin
  Result := S;
  StrReverseInplace(Result);
end;

procedure StrReverseInPlace(var S: string);
{ TODO -oahuser : Warning: This is dangerous for unicode surrogates }
var
  P1, P2: PChar;
  C: Char;
begin
  UniqueString(S);
  P1 := PChar(S);
  P2 := P1 + (Length(S) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;

function StrSingleQuote(const S: string): string;
begin
  Result := NativeSingleQuote + S + NativeSingleQuote;
end;

procedure StrSkipChars(var S: PChar; const Chars: TCharValidator);
begin
  while Chars(S^) do
    Inc(S);
end;

procedure StrSkipChars(var S: PChar; const Chars: array of Char);
begin
  while ArrayContainsChar(Chars, S^) do
    Inc(S);
end;

procedure StrSkipChars(const S: string; var Index: SizeInt; const Chars: TCharValidator);
begin
  while Chars(S[Index]) do
    Inc(Index);
end;

procedure StrSkipChars(const S: string; var Index: SizeInt; const Chars: array of Char);
begin
  while ArrayContainsChar(Chars, S[Index]) do
    Inc(Index);
end;

function StrSmartCase(const S: string; const Delimiters: TCharValidator): string;
var
  Source, Dest: PChar;
  Index, Len:   SizeInt;
  InternalDelimiters: TCharValidator;
begin
  Result := '';
  if Assigned(Delimiters) then
    InternalDelimiters := Delimiters
  else
    InternalDelimiters := CharIsSpace;

  if S <> '' then
  begin
    Result := S;
    UniqueString(Result);

    Len := Length(S);
    Source := PChar(S);
    Dest := PChar(Result);
    Inc(Dest);

    for Index := 2 to Len do
    begin
      if InternalDelimiters(Source^) and not InternalDelimiters(Dest^) then
        Dest^ := CharUpper(Dest^);
      Inc(Dest);
      Inc(Source);
    end;
    Result[1] := CharUpper(Result[1]);
  end;
end;

function StrSmartCase(const S: string; const Delimiters: array of Char): string;
var
  Source, Dest: PChar;
  Index, Len:   SizeInt;
begin
  Result := '';

  if S <> '' then
  begin
    Result := S;
    UniqueString(Result);

    Len := Length(S);
    Source := PChar(S);
    Dest := PChar(Result);
    Inc(Dest);

    for Index := 2 to Len do
    begin
      if ArrayContainsChar(Delimiters, Source^) and not ArrayContainsChar(Delimiters, Dest^) then
        Dest^ := CharUpper(Dest^);
      Inc(Dest);
      Inc(Source);
    end;
    Result[1] := CharUpper(Result[1]);
  end;
end;

function StrStringToEscaped(const S: string): string;
var
  I: SizeInt;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      NativeBackspace:
        Result := Result + '\b';
      NativeBell:
        Result := Result + '\a';
      NativeCarriageReturn:
        Result := Result + '\r';
      NAtiveFormFeed:
        Result := Result + '\f';
      NativeLineFeed:
        Result := Result + '\n';
      NativeTab:
        Result := Result + '\t';
      NativeVerticalTab:
        Result := Result + '\v';
      NativeBackSlash:
        Result := Result + '\\';
      NativeDoubleQuote:
        Result := Result + '\"';
    else
      // Characters < ' ' are escaped with hex sequence
      if S[I] < #32 then
        Result := Result + Format('\x%.2x', [SizeInt(S[I])])
      else
        Result := Result + S[I];
    end;
  end;
end;

function StrStripNonNumberChars(const S: string): string;
var
  I: SizeInt;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if CharIsNumberChar(C) then
      Result := Result + C;
  end;
end;

function StrToHex(const Source: string): string;
var
  Index: SizeInt;
  C, L, N: SizeInt;
  BL, BH: Byte;
  S:     string;
begin
  Result := '';
  if Source <> '' then
  begin
    S := Source;
    L := Length(S);
    if Odd(L) then
    begin
      S := '0' + S;
      Inc(L);
    end;
    Index := 1;
    SetLength(Result, L div 2);
    C := 1;
    N := 1;
    while C <= L do
    begin
      BH := CharHex(S[Index]);
      Inc(Index);
      BL := CharHex(S[Index]);
      Inc(Index);
      Inc(C, 2);
      if (BH = $FF) or (BL = $FF) then
      begin
        Result := '';
        Exit;
      end;
      Result[N] := Char((BH shl 4) or BL);
      Inc(N);
    end;
  end;
end;

function StrTrimCharLeft(const S: string; C: Char): string;
var
  I, L: SizeInt;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] = C) do
    Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsLeft(const S: string; const Chars: TCharValidator): string;
var
  I, L: SizeInt;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and Chars(S[I]) do
    Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsLeft(const S: string; const Chars: array of Char): string;
var
  I, L: SizeInt;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and ArrayContainsChar(Chars, S[I]) do
    Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharRight(const S: string; C: Char): string;
var
  I: SizeInt;
begin
  I := Length(S);
  while (I >= 1) and (S[I] = C) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimCharsRight(const S: string; const Chars: TCharValidator): string;
var
  I: SizeInt;
begin
  I := Length(S);
  while (I >= 1) and Chars(S[I]) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimCharsRight(const S: string; const Chars: array of Char): string;
var
  I: SizeInt;
begin
  I := Length(S);
  while (I >= 1) and ArrayContainsChar(Chars, S[I]) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimQuotes(const S: string): string;
var
  First, Last: Char;
  L: SizeInt;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and ((First = NativeSingleQuote) or (First = NativeDoubleQuote)) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := S;
end;

function StrUpper(const S: string): string;
begin
  Result := S;
  StrUpperInPlace(Result);
end;

procedure StrUpperInPlace(var S: string);
{$IFDEF UNICODE_RTL_DATABASE}
var
  P: PChar;
  I, L: SizeInt;
begin
  L := Length(S);
  if L > 0 then
  begin
    UniqueString(S);
    P := PChar(S);
    for I := 1 to L do
    begin
      P^ := TCharacter.ToUpper(P^);
      Inc(P);
    end;
  end;
end;
{$ELSE ~UNICODE_RTL_DATABASE}
begin
  StrCase(S, StrUpOffset);
end;
{$ENDIF ~UNICODE_RTL_DATABASE}

procedure StrUpperBuff(S: PChar);
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  if S <> nil then
  begin
    repeat
      S^ := TCharacter.ToUpper(S^);
      Inc(S);
    until S^ = #0;
  end;
  {$ELSE ~UNICODE_RTL_DATABASE}
  StrCaseBuff(S, StrUpOffset);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

//=== String Management ======================================================

procedure StrAddRef(var S: string);
var
  P: PStrRec;
begin
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    if P^.RefCount = -1 then
      UniqueString(S)
    else
      LockedInc(P^.RefCount);
  end;
end;

procedure StrDecRef(var S: string);
var
  P: PStrRec;
begin
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    case P^.RefCount of
      -1, 0: { nothing } ;
      1:
        begin
          Finalize(S);
          Pointer(S) := nil;
        end;
    else
      LockedDec(P^.RefCount);
    end;
  end;
end;

function StrLength(const S: string): SizeInt;
var
  P: PStrRec;
begin
  Result := 0;
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    Result := P^.Length and (not $80000000 shr 1);
  end;
end;

function StrRefCount(const S: string): SizeInt;
var
  P: PStrRec;
begin
  Result := 0;
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    Result := P^.RefCount;
  end;
end;

procedure StrResetLength(var S: WideString);
var
  I: SizeInt;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

procedure StrResetLength(var S: AnsiString);
var
  I: SizeInt;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

procedure StrResetLength(S: TJclStringBuilder);
var
  I: SizeInt;
begin
  if S <> nil then
    for I := 0 to S.Length - 1 do
      if S[I] = #0 then
      begin
        S.Length := I;
        Exit;
      end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure StrResetLength(var S: UnicodeString);
var
  I: SizeInt;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== String Search and Replace Routines =====================================

function StrCharCount(const S: string; C: Char): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

function StrCharsCount(const S: string; const Chars: TCharValidator): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if Chars(S[I]) then
      Inc(Result);
end;

function StrCharsCount(const S: string; const Chars: array of Char): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if ArrayContainsChar(Chars, S[I]) then
      Inc(Result);
end;

function StrStrCount(const S, SubS: string): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  if (Length(SubS) > Length(S)) or (Length(SubS) = 0) or (Length(S) = 0) then
    Exit;
  if Length(SubS) = 1 then
  begin
    Result := StrCharCount(S, SubS[1]);
    Exit;
  end;
  I := StrSearch(SubS, S, 1);

  if I > 0 then
    Inc(Result);

  while (I > 0) and (Length(S) > I + Length(SubS)) do
  begin
    I := StrSearch(SubS, S, I + 1);

    if I > 0 then
      Inc(Result);
  end;
end;

(*
{ 1}  Test(StrCompareRange('', '', 1, 5), 0);
{ 2}  Test(StrCompareRange('A', '', 1, 5), -1);
{ 3}  Test(StrCompareRange('AB', '', 1, 5), -1);
{ 4}  Test(StrCompareRange('ABC', '', 1, 5), -1);
{ 5}  Test(StrCompareRange('', 'A', 1, 5), -1);
{ 6}  Test(StrCompareRange('', 'AB',  1, 5), -1);
{ 7}  Test(StrCompareRange('', 'ABC', 1, 5), -1);
{ 8}  Test(StrCompareRange('A', 'a', 1, 5), -2);
{ 9}  Test(StrCompareRange('A', 'a', 1, 1), -32);
{10}  Test(StrCompareRange('aA', 'aB', 1, 1), 0);
{11}  Test(StrCompareRange('aA', 'aB', 1, 2), -1);
{12}  Test(StrCompareRange('aB', 'aA', 1, 2), 1);
{13}  Test(StrCompareRange('aA', 'aa', 1, 2), -32);
{14}  Test(StrCompareRange('aa', 'aA', 1, 2), 32);
{15}  Test(StrCompareRange('', '', 1, 0), 0);
{16}  Test(StrCompareRange('A', 'A', 1, 0), -2);
{17}  Test(StrCompareRange('Aa', 'A', 1, 0), -2);
{18}  Test(StrCompareRange('Aa', 'Aa', 1, 2), 0);
{19}  Test(StrCompareRange('Aa', 'A', 1, 2), 0);
{20}  Test(StrCompareRange('Ba', 'A', 1, 2), 1);
*)
function StrCompareRangeEx(const S1, S2: string; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
var
  Len1, Len2: SizeInt;
  I: SizeInt;
  C1, C2: Char;
begin
  if Pointer(S1) = Pointer(S2) then
  begin
    if (Count <= 0) and (S1 <> '') then
      Result := -2 // no work
    else
      Result := 0;
  end
  else
  if (S1 = '') or (S2 = '') then
    Result := -1 // null string
  else
  if Count <= 0 then
    Result := -2 // no work
  else
  begin
    Len1 := Length(S1);
    Len2 := Length(S2);

    if (Index - 1) + Count > Len1 then
      Result := -2
    else
    begin
      if (Index - 1) + Count > Len2 then // strange behaviour, but the assembler code does it
        Count := Len2 - (Index - 1);

      if CaseSensitive then
      begin
        for I := 0 to Count - 1 do
        begin
          C1 := S1[Index + I];
          C2 := S2[Index + I];
          if C1 <> C2 then
          begin
            Result := Ord(C1) - Ord(C2);
            Exit;
          end;
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          C1 := S1[Index + I];
          C2 := S2[Index + I];
          if C1 <> C2 then
          begin
            C1 := CharLower(C1);
            C2 := CharLower(C2);
            if C1 <> C2 then
            begin
              Result := Ord(C1) - Ord(C2);
              Exit;
            end;
          end;
        end;
      end;
      Result := 0;
    end;
  end;
end;

function StrCompare(const S1, S2: string; CaseSensitive: Boolean): SizeInt;
var
  Len1, Len2: SizeInt;
begin
  if Pointer(S1) = Pointer(S2) then
    Result := 0
  else
  begin
    Len1 := Length(S1);
    Len2 := Length(S2);
    Result := Len1 - Len2;
    if Result = 0 then
      Result := StrCompareRangeEx(S1, S2, 1, Len1, CaseSensitive);
  end;
end;

function StrCompareRange(const S1, S2: string; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
begin
  Result := StrCompareRangeEx(S1, S2, Index, Count, CaseSensitive);
end;

procedure StrFillChar(var S; Count: SizeInt; C: Char);
{$IFDEF SUPPORTS_UNICODE}
asm
        // 32 --> EAX S
        //        EDX Count
        //        ECX C
        // 64 --> RCX S
        //        RDX Count
        //        R8W C
        {$IFDEF CPU32}
        DEC     EDX
        JS      @@Leave
@@Loop:
        MOV     [EAX], CX
        ADD     EAX, 2
        DEC     EDX
        JNS     @@Loop
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        DEC     RDX
        JS      @@Leave
@@Loop:
        MOV     WORD PTR [RCX], R8W
        ADD     RCX, 2
        DEC     RDX
        JNS     @@Loop
        {$ENDIF CPU64}
@@Leave:
end;
{$ELSE ~SUPPORTS_UNICODE}
begin
  if Count > 0 then
    FillChar(S, Count, C);
end;
{$ENDIF ~SUPPORTS_UNICODE}

function StrRepeatChar(C: Char; Count: SizeInt): string;
begin
  SetLength(Result, Count);
  if Count > 0 then
    StrFillChar(Result[1], Count, C);
end;

function StrFind(const Substr, S: string; const Index: SizeInt): SizeInt;
var
  pos: SizeInt;
begin
  if (SubStr <> '') and (S <> '') then
  begin
    pos := StrIPos(Substr, Copy(S, Index, Length(S) - Index + 1));
    if pos = 0 then
      Result := 0
    else
      Result := Index + Pos - 1;
  end
  else
    Result := 0;
end;

function StrHasPrefix(const S: string; const Prefixes: array of string): Boolean;
begin
  Result := StrPrefixIndex(S, Prefixes) > -1;
end;

function StrHasSuffix(const S: string; const Suffixes: array of string): Boolean;
begin
  Result := StrSuffixIndex(S, Suffixes) > -1;
end;

function StrIndex(const S: string; const List: array of string; CaseSensitive: Boolean): SizeInt;
var
  I: SizeInt;
begin
  Result := -1;
  for I := Low(List) to High(List) do
  begin
    if StrCompare(S, List[I], CaseSensitive) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrIHasPrefix(const S: string; const Prefixes: array of string): Boolean;
begin
  Result := StrIPrefixIndex(S, Prefixes) > -1;
end;

function StrIHasSuffix(const S: string; const Suffixes: array of string): Boolean;
begin
  Result := StrISuffixIndex(S, Suffixes) > -1;
end;

function StrILastPos(const SubStr, S: string): SizeInt;
begin
  Result := StrLastPos(StrUpper(SubStr), StrUpper(S));
end;

function StrIPos(const SubStr, S: string): SizeInt;
begin
  Result := Pos(StrUpper(SubStr), StrUpper(S));
end;

function StrIPrefixIndex(const S: string; const Prefixes: array of string): SizeInt;
var
  I: SizeInt;
  Test: string;
begin
  Result := -1;
  for I := Low(Prefixes) to High(Prefixes) do
  begin
    Test := StrLeft(S, Length(Prefixes[I]));
    if CompareText(Test, Prefixes[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrIsOneOf(const S: string; const List: array of string): Boolean;
begin
  Result := StrIndex(S, List) > -1;
end;

function StrISuffixIndex(const S: string; const Suffixes: array of string): SizeInt;
var
  I: SizeInt;
  Test: string;
begin
  Result := -1;
  for I := Low(Suffixes) to High(Suffixes) do
  begin
    Test := StrRight(S, Length(Suffixes[I]));
    if CompareText(Test, Suffixes[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrLastPos(const SubStr, S: string): SizeInt;
var
  Last, Current: PChar;
begin
  Result := 0;
  Last := nil;
  Current := PChar(S);

  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := StrPos(PChar(Current), PChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current);
    end;
  end;
  if Last <> nil then
    Result := Abs(PChar(S) - Last) + 1;
end;

// IMPORTANT NOTE: The StrMatch function does currently not work with the Asterix (*)
// (*) acts like (?)

function StrMatch(const Substr, S: string; Index: SizeInt): SizeInt;
var
  SI, SubI, SLen, SubLen: SizeInt;
  SubC: Char;
begin
  SLen := Length(S);
  SubLen := Length(Substr);
  Result := 0;
  if (Index > SLen) or (SubLen = 0) then
    Exit;
  while Index <= SLen do
  begin
    SubI := 1;
    SI := Index;
    while (SI <= SLen) and (SubI <= SubLen) do
    begin
      SubC := Substr[SubI];
      if (SubC = '*') or (SubC = '?') or (SubC = S[SI]) then
      begin
        Inc(SI);
        Inc(SubI);
      end
      else
        Break;
    end;
    if SubI > SubLen then
    begin
      Result := Index;
      Break;
    end;
    Inc(Index);
  end;
end;

// Derived from "Like" by Michael Winter
function StrMatches(const Substr, S: string; const Index: SizeInt): Boolean;
var
  StringPtr: PChar;
  PatternPtr: PChar;
  StringRes: PChar;
  PatternRes: PChar;
begin
  if SubStr = '' then
    raise EJclStringError.CreateRes(@RsBlankSearchString);

  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  if (Index <= 0) or (Index > Length(S)) then
    raise EJclStringError.CreateRes(@RsArgumentOutOfRange);

  StringPtr := PChar(@S[Index]);
  PatternPtr := PChar(SubStr);
  StringRes := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
        begin
          Result := StringPtr^ = #0;
          if Result or (StringRes = nil) or (PatternRes = nil) then
            Exit;

          StringPtr := StringRes;
          PatternPtr := PatternRes;
          Break;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
          Break;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
      else
      begin
        if StringPtr^ = #0 then
          Exit;
        if StringPtr^ <> PatternPtr^ then
        begin
          if (StringRes = nil) or (PatternRes = nil) then
            Exit;
          StringPtr := StringRes;
          PatternPtr := PatternRes;
          Break;
        end
        else
        begin
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
      end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
        begin
          Result := True;
          Exit;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
      else
      begin
        repeat
          if StringPtr^ = #0 then
            Exit;
          if StringPtr^ = PatternPtr^ then
            Break;
          Inc(StringPtr);
        until False;
        Inc(StringPtr);
        StringRes := StringPtr;
        Inc(PatternPtr);
        Break;
      end;
      end;
    until False;
  until False;
end;

function StrNPos(const S, SubStr: string; N: SizeInt): SizeInt;
var
  I, P: SizeInt;
begin
  if N < 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := StrSearch(SubStr, S, 1);
  I := 1;
  while I < N do
  begin
    P := StrSearch(SubStr, S, Result + 1);
    if P = 0 then
    begin
      Result := 0;
      Break;
    end
    else
    begin
      Result := P;
      Inc(I);
    end;
  end;
end;

function StrNIPos(const S, SubStr: string; N: SizeInt): SizeInt;
var
  I, P: SizeInt;
begin
  if N < 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := StrFind(SubStr, S, 1);
  I := 1;
  while I < N do
  begin
    P := StrFind(SubStr, S, Result + 1);
    if P = 0 then
    begin
      Result := 0;
      Break;
    end
    else
    begin
      Result := P;
      Inc(I);
    end;
  end;
end;

function StrPrefixIndex(const S: string; const Prefixes: array of string): SizeInt;
var
  I: SizeInt;
  Test: string;
begin
  Result := -1;
  for I := Low(Prefixes) to High(Prefixes) do
  begin
    Test := StrLeft(S, Length(Prefixes[I]));
    if CompareStr(Test, Prefixes[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrSearch(const Substr, S: string; const Index: SizeInt): SizeInt;
var
  SP, SPI, SubP: PChar;
  SLen: SizeInt;
begin
  SLen := Length(S);
  if Index <= SLen then
  begin
    SP := PChar(S);
    SubP := PChar(Substr);
    SPI := SP;
    Inc(SPI, Index);
    Dec(SPI);
    SPI := StrPos(SPI, SubP);
    if SPI <> nil then
      Result := SPI - SP + 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function StrSuffixIndex(const S: string; const Suffixes: array of string): SizeInt;
var
  I: SizeInt;
  Test: string;
begin
  Result := -1;
  for I := Low(Suffixes) to High(Suffixes) do
  begin
    Test := StrRight(S, Length(Suffixes[I]));
    if CompareStr(Test, Suffixes[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//=== String Extraction ======================================================

function StrAfter(const SubStr, S: string): string;
var
  P: SizeInt;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: string): string;
var
  P: SizeInt;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;

function StrSplit(const SubStr, S: string;var Left, Right : string): boolean;
var
  P: SizeInt;
begin
  P := StrFind(SubStr, S, 1);
  Result:= p > 0;
  if Result then
  begin
    Left := StrLeft(S, P - 1);
    Right := StrRestOf(S, P + Length(SubStr));
  end
  else
  begin
    Left := '';
    Right := '';
  end;
end;

function StrBetween(const S: string; const Start, Stop: Char): string;
var
  PosStart, PosEnd: SizeInt;
  L: SizeInt;
begin
  PosStart := Pos(Start, S);
  PosEnd := StrSearch(Stop, S, PosStart + 1);  // PosEnd has to be after PosStart.

  if (PosStart > 0) and (PosEnd > PosStart) then
  begin
    L := PosEnd - PosStart;
    Result := Copy(S, PosStart + 1, L - 1);
  end
  else
    Result := '';
end;

function StrChopRight(const S: string; N: SizeInt): string;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLeft(const S: string; Count: SizeInt): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrMid(const S: string; Start, Count: SizeInt): string;
begin
  Result := Copy(S, Start, Count);
end;

function StrRestOf(const S: string; N: SizeInt): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: string; Count: SizeInt): string;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

//=== Character (do we have it ;) ============================================

function CharEqualNoCase(const C1, C2: Char): Boolean;
begin
  //if they are not equal chars, may be same letter different case
  Result := (C1 = C2) or
    (CharIsAlpha(C1) and CharIsAlpha(C2) and (CharLower(C1) = CharLower(C2)));
end;


function CharIsAlpha(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsLetter(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := (StrCharTypes[C] and C1_ALPHA) <> 0;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsLetterOrDigit(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := ((StrCharTypes[C] and C1_ALPHA) <> 0) or ((StrCharTypes[C] and C1_DIGIT) <> 0);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsBlank(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  //http://blogs.msdn.com/b/michkap/archive/2007/06/11/3230072.aspx
  Result := (C = ' ') or (C = #$0009) or (C = #$00A0) or (C = #$3000);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := ((StrCharTypes[C] and C1_BLANK) <> 0);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsControl(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsControl(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := (StrCharTypes[C] and C1_CNTRL) <> 0;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsDelete(const C: Char): Boolean;
begin
  Result := (C = #8);
end;

function CharIsDigit(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsDigit(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := (StrCharTypes[C] and C1_DIGIT) <> 0;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsFracDigit(const C: Char): Boolean;
begin
  Result := (C = '.') or CharIsDigit(C);
end;

function CharIsHexDigit(const C: Char): Boolean;
begin
  case C of
    'A'..'F',
    'a'..'f':
      Result := True;
  else
    Result := CharIsDigit(C);
  end;
end;

function CharIsLower(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsLower(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := (StrCharTypes[C] and C1_LOWER) <> 0;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsNumberChar(const C: Char): Boolean;
begin
  Result := CharIsDigit(C) or (C = '+') or (C = '-') or (C = JclFormatSettings.DecimalSeparator);
end;

function CharIsNumber(const C: Char): Boolean;
begin
  Result := CharIsDigit(C) or (C = JclFormatSettings.DecimalSeparator);
end;

function CharIsPrintable(const C: Char): Boolean;
begin
  Result := not CharIsControl(C);
end;

function CharIsPunctuation(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsPunctuation(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := ((StrCharTypes[C] and C1_PUNCT) <> 0);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsReturn(const C: Char): Boolean;
begin
  Result := (C = NativeLineFeed) or (C = NativeCarriageReturn);
end;

function CharIsSpace(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsWhiteSpace(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := (StrCharTypes[C] and C1_SPACE) <> 0;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsUpper(const C: Char): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsUpper(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := (StrCharTypes[C] and C1_UPPER) <> 0;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharIsValidIdentifierLetter(const C: Char): Boolean;
begin
  case C of
    {$IFDEF SUPPORTS_UNICODE}
    // from XML specifications
    #$00C0..#$00D6, #$00D8..#$00F6, #$00F8..#$02FF, #$0370..#$037D,
    #$037F..#$1FFF, #$200C..#$200D, #$2070..#$218F, #$2C00..#$2FEF,
    #$3001..#$D7FF, #$F900..#$FDCF, #$FDF0..#$FFFD, // #$10000..#$EFFFF, howto match surrogate pairs?
    #$00B7, #$0300..#$036F, #$203F..#$2040,
    {$ENDIF SUPPORTS_UNICODE}
    '0'..'9', 'A'..'Z', 'a'..'z', '_':
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
  case C of
    NativeTab,
    NativeLineFeed,
    NativeVerticalTab,
    NativeFormFeed,
    NativeCarriageReturn,
    NativeSpace:
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsWildcard(const C: Char): Boolean;
begin
  case C of
    '*', '?':
      Result := True;
  else
    Result := False;
  end;
end;

function CharType(const C: Char): Word;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  GetStringTypeEx(LOCALE_USER_DEFAULT, CT_CTYPE1, @C, 1, Result);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := StrCharTypes[C];
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

//=== PCharVector ============================================================

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
var
  I: SizeInt;
  S: string;
  List: array of PChar;
begin
  Assert(Source <> nil);
  Dest := AllocMem((Source.Count + SizeOf(Char)) * SizeOf(PChar));
  SetLength(List, Source.Count + SizeOf(Char));
  for I := 0 to Source.Count - 1 do
  begin
    S := Source[I];
    List[I] := StrAlloc(Length(S) + SizeOf(Char));
    StrPCopy(List[I], S);
  end;
  List[Source.Count] := nil;
  Move(List[0], Dest^, (Source.Count + 1) * SizeOf(PChar));
  Result := Dest;
end;

function PCharVectorCount(Source: PCharVector): SizeInt;
begin
  Result := 0;
  if Source <> nil then
  begin
    while Source^ <> nil do
    begin
      Inc(Source);
      Inc(Result);
    end;
  end;
end;

procedure PCharVectorToStrings(const Dest: TStrings; Source: PCharVector);
var
  I, Count: SizeInt;
  List:     array of PChar;
begin
  Assert(Dest <> nil);
  if Source <> nil then
  begin
    Count := PCharVectorCount(Source);
    SetLength(List, Count);
    Move(Source^, List[0], Count * SizeOf(PChar));
    Dest.BeginUpdate;
    try
      Dest.Clear;
      for I := 0 to Count - 1 do
        Dest.Add(List[I]);
    finally
      Dest.EndUpdate;
    end;
  end;
end;

procedure FreePCharVector(var Dest: PCharVector);
var
  I, Count: SizeInt;
  List:     array of PChar;
begin
  if Dest <> nil then
  begin
    Count := PCharVectorCount(Dest);
    SetLength(List, Count);
    Move(Dest^, List[0], Count * SizeOf(PChar));
    for I := 0 to Count - 1 do
      StrDispose(List[I]);
    FreeMem(Dest, (Count + 1) * SizeOf(PChar));
    Dest := nil;
  end;
end;

//=== Character Transformation Routines ======================================

function CharHex(const C: Char): Byte;
begin
  case C of
    '0'..'9':
      Result := Ord(C) - Ord('0');
    'a'..'f':
      Result := Ord(C) - Ord('a') + 10;
    'A'..'F':
      Result := Ord(C) - Ord('A') + 10;
  else
    Result := $FF;
  end;
end;

function CharLower(const C: Char): Char;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.ToLower(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := StrCaseMap[Ord(C) + StrLoOffset];
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharToggleCase(const C: Char): Char;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  if CharIsLower(C) then
    Result := CharUpper(C)
  else if CharIsUpper(C) then
    Result := CharLower(C)
  else
    Result := C;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := StrCaseMap[Ord(C) + StrReOffset];
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function CharUpper(const C: Char): Char;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.ToUpper(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := StrCaseMap[Ord(C) + StrUpOffset];
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

//=== Character Search and Replace ===========================================

function CharLastPos(const S: string; const C: Char; const Index: SizeInt): SizeInt;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    for Result := Length(S) downto Index do
      if S[Result] = C then
        Exit;
  end;
  Result := 0;
end;

function CharPos(const S: string; const C: Char; const Index: SizeInt): SizeInt;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    for Result := Index to Length(S) do
      if S[Result] = C then
        Exit;
  end;
  Result := 0;
end;

function CharIPos(const S: string; C: Char; const Index: SizeInt): SizeInt;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    C := CharUpper(C);
    for Result := Index to Length(S) do
      if CharUpper(S[Result]) = C then
        Exit;
  end;
  Result := 0;
end;

function CharReplace(var S: string; const Search, Replace: Char): SizeInt;
var
  P: PChar;
  Index, Len: SizeInt;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    P := PChar(S);
    Len := Length(S);
    for Index := 0 to Len - 1 do
    begin
      if P^ = Search then
      begin
        P^ := Replace;
        Inc(Result);
      end;
      Inc(P);
    end;
  end;
end;

//=== MultiSz ================================================================

function StringsToMultiSz(var Dest: PMultiSz; const Source: TStrings): PMultiSz;
var
  I, TotalLength: SizeInt;
  P: PMultiSz;
begin
  Assert(Source <> nil);
  TotalLength := 1;
  for I := 0 to Source.Count - 1 do
    if Source[I] = '' then
      raise EJclStringError.CreateRes(@RsInvalidEmptyStringItem)
    else
      Inc(TotalLength, StrLen(PChar(Source[I])) + 1);
  AllocateMultiSz(Dest, TotalLength);
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopy(P, PChar(Source[I]));
    Inc(P);
  end;
  P^ := #0;
  Result := Dest;
end;

procedure MultiSzToStrings(const Dest: TStrings; const Source: PMultiSz);
var
  P: PMultiSz;
begin
  Assert(Dest <> nil);
  Dest.BeginUpdate;
  try
    Dest.Clear;
    if Source <> nil then
    begin
      P := Source;
      while P^ <> #0 do
      begin
        Dest.Add(P);
        P := StrEnd(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

function MultiSzLength(const Source: PMultiSz): SizeInt;
var
  P: PMultiSz;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source;
    repeat
      Inc(Result, StrLen(P) + 1);
      P := StrEnd(P);
      Inc(P);
    until P^ = #0;
    Inc(Result);
  end;
end;

procedure AllocateMultiSz(var Dest: PMultiSz; Len: SizeInt);
begin
  if Len > 0 then
    GetMem(Dest, Len * SizeOf(Char))
  else
    Dest := nil;
end;

procedure FreeMultiSz(var Dest: PMultiSz);
begin
  if Dest <> nil then
    FreeMem(Dest);
  Dest := nil;
end;

function MultiSzDup(const Source: PMultiSz): PMultiSz;
var
  Len: SizeInt;
begin
  if Source <> nil then
  begin
    Len := MultiSzLength(Source);
    Result := nil;
    AllocateMultiSz(Result, Len);
    Move(Source^, Result^, Len * SizeOf(Char));
  end
  else
    Result := nil;
end;

function AnsiStringsToAnsiMultiSz(var Dest: PAnsiMultiSz; const Source: TAnsiStrings): PAnsiMultiSz;
begin
  Result := JclAnsiStrings.StringsToMultiSz(Dest, Source);
end;

procedure AnsiMultiSzToAnsiStrings(const Dest: TAnsiStrings; const Source: PAnsiMultiSz);
begin
  JclAnsiStrings.MultiSzToStrings(Dest, Source);
end;

function AnsiMultiSzLength(const Source: PAnsiMultiSz): SizeInt;
begin
  Result := JclAnsiStrings.MultiSzLength(Source);
end;

procedure AllocateAnsiMultiSz(var Dest: PAnsiMultiSz; Len: SizeInt);
begin
  JclAnsiStrings.AllocateMultiSz(Dest, Len);
end;

procedure FreeAnsiMultiSz(var Dest: PAnsiMultiSz);
begin
  JclAnsiStrings.FreeMultiSz(Dest);
end;

function AnsiMultiSzDup(const Source: PAnsiMultiSz): PAnsiMultiSz;
begin
  Result := JclAnsiStrings.MultiSzDup(Source);
end;

function WideStringsToWideMultiSz(var Dest: PWideMultiSz; const Source: TWideStrings): PWideMultiSz;
begin
  Result := JclWideStrings.StringsToMultiSz(Dest, Source);
end;

procedure WideMultiSzToWideStrings(const Dest: TWideStrings; const Source: PWideMultiSz);
begin
  JclWideStrings.MultiSzToStrings(Dest, Source);
end;

function WideMultiSzLength(const Source: PWideMultiSz): SizeInt;
begin
  Result := JclWideStrings.MultiSzLength(Source);
end;

procedure AllocateWideMultiSz(var Dest: PWideMultiSz; Len: SizeInt);
begin
  JclWideStrings.AllocateMultiSz(Dest, Len);
end;

procedure FreeWideMultiSz(var Dest: PWideMultiSz);
begin
  JclWideStrings.FreeMultiSz(Dest);
end;

function WideMultiSzDup(const Source: PWideMultiSz): PWideMultiSz;
begin
  Result := JclWideStrings.MultiSzDup(Source);
end;

//=== TStrings Manipulation ==================================================

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: SizeInt;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if (S <> '') or AllowEmptyString then
      List.Add(S);  // Ignore empty strings at the end (only if AllowEmptyString = False).
  finally
    List.EndUpdate;
  end;
end;

procedure StrIToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: SizeInt;
  LowerCaseStr: string;
  Left: string;
begin
  Assert(List <> nil);
  LowerCaseStr := StrLower(S);
  Sep := StrLower(Sep);
  L := Length(Sep);
  I := Pos(Sep, LowerCaseStr);
  List.BeginUpdate;
  try
    List.Clear;
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      Delete(LowerCaseStr, 1, I + L - 1);
      I := Pos(Sep, LowerCaseStr);
    end;
    if (S <> '') or AllowEmptyString then
      List.Add(S);  // Ignore empty strings at the end (only if AllowEmptyString = False).
  finally
    List.EndUpdate;
  end;
end;

function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean = True): string;
var
  I, L: SizeInt;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count > 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

function StringsToStr(const List: TStrings; const Sep: string; const NumberOfItems: SizeInt; const AllowEmptyString:
    Boolean = True): string;
var
  I, L, N: SizeInt;
begin
  Result := '';
  if List.Count > NumberOfItems then
    N := NumberOfItems
  else
    N := List.Count;
  for I := 0 to N - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if N > 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: SizeInt;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := Trim(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TrimStringsRight(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: SizeInt;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := TrimRight(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TrimStringsLeft(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: SizeInt;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := TrimLeft(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

function AddStringToStrings(const S: string; Strings: TStrings; const Unique: Boolean): Boolean;
begin
  Assert(Strings <> nil);
  Result := Unique and (Strings.IndexOf(S) <> -1);
  if not Result then
    Result := Strings.Add(S) > -1;
end;

//=== Miscellaneous ==========================================================

function FileToString(const FileName: string): {$IFDEF COMPILER12_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
var
  fs: TFileStream;
  Len: SizeInt;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Len := fs.Size;
    SetLength(Result, Len);
    if Len > 0 then
      fs.ReadBuffer(Result[1], Len);
  finally
    fs.Free;
  end;
end;

procedure StringToFile(const FileName: string; const Contents: {$IFDEF COMPILER12_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
  Append: Boolean);
var
  FS: TFileStream;
  Len: SizeInt;
begin
  if Append and FileExists(filename) then
    FS := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
  else
    FS := TFileStream.Create(FileName, fmCreate);
  try
    if Append then
      FS.Seek(0, soEnd);  // faster than .Position := .Size
    Len := Length(Contents);
    if Len > 0 then
      FS.WriteBuffer(Contents[1], Len);
  finally
    FS.Free;
  end;
end;

function StrToken(var S: string; Separator: Char): string;
var
  I: SizeInt;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure StrTokens(const S: string; const List: TStrings);
var
  Start: PChar;
  Token: string;
  Done:  Boolean;
begin
  Assert(List <> nil);
  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    Start := Pointer(S);
    repeat
      Done := JclStrings.StrWord(Start, Token);
      if Token <> '' then
        List.Add(Token);
    until Done;
  finally
    List.EndUpdate;
  end;
end;

function StrWord(const S: string; var Index: SizeInt; out Word: string): Boolean;
var
  Start: SizeInt;
  C: Char;
begin
  Word := '';
  if (S = '') then
  begin
    Result := True;
    Exit;
  end;
  Start := Index;
  Result := False;
  while True do
  begin
    C := S[Index];
    case C of
      #0:
        begin
          if Start <> 0 then
            Word := Copy(S, Start, Index - Start);
          Result := True;
          Exit;
        end;
      NativeSpace, NativeLineFeed, NativeCarriageReturn:
        begin
          if Start <> 0 then
          begin
            Word := Copy(S, Start, Index - Start);
            Exit;
          end
          else
          begin
            while CharIsWhiteSpace(C) do
            begin
              Inc(Index);
              C := S[Index];
            end;
          end;
        end;
    else
      if Start = 0 then
        Start := Index;
      Inc(Index);
    end;
  end;
end;

function StrWord(var S: PChar; out Word: string): Boolean;
var
  Start: PChar;
begin
  Word := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := False;
  while True do
  begin
    case S^ of
      #0:
      begin
        if Start <> nil then
          SetString(Word, Start, S - Start);
        Result := True;
        Exit;
      end;
      NativeSpace, NativeLineFeed, NativeCarriageReturn:
      begin
        if Start <> nil then
        begin
          SetString(Word, Start, S - Start);
          Exit;
        end
        else
          while CharIsWhiteSpace(S^) do
            Inc(S);
      end;
    else
      if Start = nil then
        Start := S;
      Inc(S);
    end;
  end;
end;

function StrIdent(const S: string; var Index: SizeInt; out Ident: string): Boolean;
var
  Start: SizeInt;
  C: Char;
begin
  Ident := '';
  if (S = '') then
  begin
    Result := True;
    Exit;
  end;
  Start := Index;
  Result := False;
  while True do
  begin
    C := S[Index];
    if CharIsValidIdentifierLetter(C) then
    begin
      if Start = 0 then
        Start := Index;
    end
    else
    if C = #0 then
    begin
      if Start <> 0 then
        Ident := Copy(S, Start, Index - Start);
      Result := True;
      Exit;
    end
    else
    begin
      if Start <> 0 then
      begin
        Ident := Copy(S, Start, Index - Start);
        Exit;
      end;
    end;
    Inc(Index);
  end;
end;

function StrIdent(var S: PChar; out Ident: string): Boolean;
var
  Start: PChar;
  C: Char;
begin
  Ident := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := False;
  while True do
  begin
    C := S^;
    if CharIsValidIdentifierLetter(C) then
    begin
      if Start = nil then
        Start := S;
    end
    else
    if C = #0 then
    begin
      if Start <> nil then
        SetString(Ident, Start, S - Start);
      Result := True;
      Exit;
    end
    else
    begin
      if Start <> nil then
      begin
        SetString(Ident, Start, S - Start);
        Exit;
      end
    end;
    Inc(S);
  end;
end;

procedure StrTokenToStrings(S: string; Separator: Char; const List: TStrings);
var
  Token: string;
begin
  Assert(List <> nil);

  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    while S <> '' do
    begin
      Token := StrToken(S, Separator);
      List.Add(Token);
    end;
  finally
    List.EndUpdate;
  end;
end;

function StrToFloatSafe(const S: string): Float;
var
  Temp: string;
  I, J, K: SizeInt;
  SwapSeparators, IsNegative: Boolean;
  DecSep, ThouSep, C: Char;
begin
  DecSep := {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}DecimalSeparator;
  ThouSep := {$IFDEF RTL220_UP}FormatSettings.{$ENDIF}ThousandSeparator;
  Temp := S;
  SwapSeparators := False;

  IsNegative := False;
  J := 0;
  for I := 1 to Length(Temp) do
  begin
    C := Temp[I];
    if C = '-' then
      IsNegative := not IsNegative
    else
    if (C <> ' ') and (C <> '(') and (C <> '+') then
    begin
        // if it appears prior to any digit, it has to be a decimal separator
      SwapSeparators := Temp[I] = ThouSep;
      J := I;
      Break;
    end;
  end;

  if not SwapSeparators then
  begin
    K := CharPos(Temp, DecSep);
    SwapSeparators :=
      // if it appears prior to any digit, it has to be a decimal separator
      (K > J) and
      // if it appears multiple times, it has to be a thousand separator
      ((StrCharCount(Temp, DecSep) > 1) or
      // we assume (consistent with Windows Platform SDK documentation),
      // that thousand separators appear only to the left of the decimal
      (K < CharPos(Temp, ThouSep)));
  end;

  if SwapSeparators then
  begin
    // assume a numerical string from a different locale,
    // where DecimalSeparator and ThousandSeparator are exchanged
    for I := 1 to Length(Temp) do
      if Temp[I] = DecSep then
        Temp[I] := ThouSep
      else
      if Temp[I] = ThouSep then
        Temp[I] := DecSep;
  end;

  Temp := StrKeepChars(Temp, CharIsNumber);

  if Length(Temp) > 0 then
  begin
    if Temp[1] = DecSep then
      Temp := '0' + Temp;
    if Temp[Length(Temp)] = DecSep then
      Temp := Temp + '0';
    Result := StrToFloat(Temp);
    if IsNegative then
      Result := -Result;
  end
  else
    Result := 0.0;
end;

function StrToIntSafe(const S: string): Integer;
begin
  Result := Trunc(StrToFloatSafe(S));
end;

procedure StrNormIndex(const StrLen: SizeInt; var Index: SizeInt; var Count: SizeInt); overload;
begin
  Index := Max(1, Min(Index, StrLen + 1));
  Count := Max(0, Min(Count, StrLen + 1 - Index));
end;

function ArrayOf(List: TStrings): TDynStringArray;
var
  I: SizeInt;
begin
  if List <> nil then
  begin
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I];
  end
  else
    Result := nil;
end;

const
  BoolToStr: array [Boolean] of string = ('false', 'true');

type
  TInterfacedObjectAccess = class(TInterfacedObject);

procedure MoveChar(const Source; var Dest; Count: SizeInt);
begin
  if Count > 0 then
    Move(Source, Dest, Count * SizeOf(Char));
end;

function DotNetFormat(const Fmt: string; const Arg0: Variant): string;
begin
  Result := DotNetFormat(Fmt, [Arg0]);
end;

function DotNetFormat(const Fmt: string; const Arg0, Arg1: Variant): string;
begin
  Result := DotNetFormat(Fmt, [Arg0, Arg1]);
end;

function DotNetFormat(const Fmt: string; const Arg0, Arg1, Arg2: Variant): string;
begin
  Result := DotNetFormat(Fmt, [Arg0, Arg1, Arg2]);
end;

function DotNetFormat(const Fmt: string; const Args: array of const): string;
var
  F, P: PChar;
  Len, Capacity, Count: SizeInt;
  Index: SizeInt;
  ErrorCode: Integer;
  S: string;

  procedure Grow(Count: SizeInt);
  begin
    if Len + Count > Capacity then
    begin
      Capacity := Capacity * 5 div 3 + Count;
      SetLength(Result, Capacity);
    end;
  end;

  function InheritsFrom(AClass: TClass; const ClassName: string): Boolean;
  begin
    Result := True;
    while AClass <> nil do
    begin
      if CompareText(AClass.ClassName, ClassName) = 0 then
        Exit;
      AClass := AClass.ClassParent;
    end;
    Result := False;
  end;

  function GetStringOf(const V: TVarData; Index: SizeInt): string; overload;
  begin
    case V.VType of
      varEmpty, varNull:
        raise ArgumentNullException.CreateRes(@RsArgumentIsNull);
      varSmallInt:
        Result := IntToStr(V.VSmallInt);
      varInteger:
        Result := IntToStr(V.VInteger);
      varSingle:
        Result := FloatToStr(V.VSingle);
      varDouble:
        Result := FloatToStr(V.VDouble);
      varCurrency:
        Result := CurrToStr(V.VCurrency);
      varDate:
        Result := DateTimeToStr(V.VDate);
      varOleStr:
        Result := V.VOleStr;
      varBoolean:
        Result := BoolToStr[V.VBoolean <> False];
      varByte:
        Result := IntToStr(V.VByte);
      varWord:
        Result := IntToStr(V.VWord);
      varShortInt:
        Result := IntToStr(V.VShortInt);
      varLongWord:
        Result := IntToStr(V.VLongWord);
      varInt64:
        Result := IntToStr(V.VInt64);
      varString:
        Result := string(V.VString);
      {$IFDEF SUPPORTS_UNICODE_STRING}
      varUString:
        Result := string(V.VUString);
      {$ENDIF SUPPORTS_UNICODE_STRING}
      {varArray,
      varDispatch,
      varError,
      varUnknown,
      varAny,
      varByRef:}
    else
      raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
    end;
  end;

  function GetStringOf(Index: SizeInt): string; overload;
  var
    V: TVarRec;
    Intf: IToString;
  begin
    V := Args[Index];
    if (V.VInteger = 0) and
      (V.VType in [vtExtended, vtString, vtObject, vtClass, vtCurrency,
      vtInterface, vtInt64]) then
      raise ArgumentNullException.CreateResFmt(@RsArgumentIsNull, [Index]);

    case V.VType of
      vtInteger:
        Result := IntToStr(V.VInteger);
      vtBoolean:
        Result := BoolToStr[V.VBoolean];
      vtChar:
        Result := string(AnsiString(V.VChar));
      vtExtended:
        Result := FloatToStr(V.VExtended^);
      vtString:
        Result := string(V.VString^);
      vtPointer:
        Result := IntToHex(TJclAddr(V.VPointer), 8);
      vtPChar:
        Result := string(AnsiString(V.VPChar));
      vtObject:
        if (V.VObject is TInterfacedObject) and V.VObject.GetInterface(IToString, Intf) then
        begin
          Result := Intf.ToString;
          Pointer(Intf) := nil; // do not release the object
          // undo the RefCount change
          Dec(TInterfacedObjectAccess(V.VObject).FRefCount);
        end
        else
        if ((V.VObject is TComponent) or (V.VObject is TInterfacedPersistent)) and V.VObject.GetInterface(IToString, Intf) then
          Result := Intf.ToString
        {$IFDEF RTL200_UP}
        else
          Result := V.VObject.ToString;
        {$ELSE}
        else
          raise ArgumentNullException.CreateResFmt(@RsDotNetFormatObjectArgumentNotSupported, [V.VObject.ClassName, Index]);
        {$ENDIF RTL200_UP}
      vtClass:
        Result := V.VClass.ClassName;
      vtWideChar:
        Result := V.VWideChar;
      vtPWideChar:
        Result := V.VPWideChar;
      vtAnsiString:
        Result := string(V.VAnsiString);
      vtCurrency:
        Result := CurrToStr(V.VCurrency^);
      vtVariant:
        Result := GetStringOf(TVarData(V.VVariant^), Index);
      vtInterface:
        if IInterface(V.VInterface).QueryInterface(IToString, Intf) = 0 then
          Result := IToString(Intf).ToString
        else
          raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
      vtWideString:
        Result := WideString(V.VWideString);
      vtInt64:
        Result := IntToStr(V.VInt64^);
      {$IFDEF SUPPORTS_UNICODE_STRING}
      vtUnicodeString:
        Result := UnicodeString(V.VUnicodeString);
      {$ENDIF SUPPORTS_UNICODE_STRING}
    else
      raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
    end;
  end;

begin
  if Length(Args) = 0 then
  begin
    Result := Fmt;
    Exit;
  end;
  Len := 0;
  Capacity := Length(Fmt);
  SetLength(Result, Capacity);
  if Capacity = 0 then
    raise ArgumentNullException.CreateRes(@RsDotNetFormatNullFormat);

  P := Pointer(Fmt);
  F := P;
  while True do
  begin
    if (P[0] = #0) or (P[0] = '{') then
    begin
      Count := P - F;
      Inc(P);
      if (P[-1] <> #0) and (P[0] = '{') then
        Inc(Count); // include '{'

      if Count > 0 then
      begin
        Grow(Count);
        MoveChar(F[0], Result[Len + 1], Count);
        Inc(Len, Count);
      end;

      if P[-1] = #0 then
        Break;

      if P[0] <> '{' then
      begin
        F := P;
        Inc(P);
        while (P[0] <> #0) and (P[0] <> '}') do
          Inc(P);
        SetString(S, F, P - F);
        Val(S, Index, ErrorCode);
        if ErrorCode <> 0 then
          raise FormatException.CreateRes(@RsFormatException);
        if (Index < 0) or (Index > High(Args)) then
          raise FormatException.CreateRes(@RsFormatException);
        S := GetStringOf(Index);
        if S <> '' then
        begin
          Grow(Length(S));
          MoveChar(S[1], Result[Len + 1], Length(S));
          Inc(Len, Length(S));
        end;

        if P[0] = #0 then
          Break;
      end;
      F := P + 1;
    end
    else
    if (P[0] = '}') and (P[1] = '}') then
    begin
      Count := P - F + 1;
      Inc(P); // skip next '}'

      Grow(Count);
      MoveChar(F[0], Result[Len + 1], Count);
      Inc(Len, Count);
      F := P + 1;
    end;

    Inc(P);
  end;

  SetLength(Result, Len);
end;

//=== { TJclStringBuilder } =====================================================

constructor TJclStringBuilder.Create(Capacity: SizeInt; MaxCapacity: SizeInt);
begin
  inherited Create;
  SetLength(FChars, Capacity);
  FMaxCapacity := MaxCapacity;
end;

constructor TJclStringBuilder.Create(const Value: string; Capacity: SizeInt);
begin
  Create(Capacity);
  Append(Value);
end;

constructor TJclStringBuilder.Create(const Value: string; StartIndex, Length, Capacity: SizeInt);
begin
  Create(Capacity);
  Append(Value, StartIndex + 1, Length);
end;

function TJclStringBuilder.ToString: string;
begin
  if FLength > 0 then
    SetString(Result, PChar(@FChars[0]), FLength)
  else
    Result := '';
end;

function TJclStringBuilder.EnsureCapacity(Capacity: SizeInt): SizeInt;
begin
  if System.Length(FChars) < Capacity then
    SetCapacity(Capacity);
  Result := System.Length(FChars);
end;

procedure TJclStringBuilder.Clear;
begin
  Length := 0;
end;

procedure TJclStringBuilder.SetCapacity(const Value: SizeInt);
begin
  if Value <> System.Length(FChars) then
  begin
    SetLength(FChars, Value);
    if Value < FLength then
      FLength := Value;
  end;
end;

function TJclStringBuilder.GetChars(Index: SizeInt): Char;
begin
  Result := FChars[Index];
end;

procedure TJclStringBuilder.SetChars(Index: SizeInt; const Value: Char);
begin
  FChars[Index] := Value;
end;

procedure TJclStringBuilder.Set_Length(const Value: SizeInt);
begin
  FLength := Value;
end;

function TJclStringBuilder.GetCapacity: SizeInt;
begin
  Result := System.Length(FChars);
end;

function TJclStringBuilder.AppendPChar(Value: PChar; Count: SizeInt; RepeatCount: SizeInt): TJclStringBuilder;
var
  Capacity: SizeInt;
begin
  if (Count > 0) and (RepeatCount > 0) then
  begin
    repeat
      Capacity := System.Length(FChars);
      if Capacity + Count > MaxCapacity then
        raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
      if Capacity < FLength + Count then
        SetLength(FChars, Capacity * 5 div 3 + Count);
      if Count = 1 then
        FChars[FLength] := Value[0]
      else
        MoveChar(Value[0], FChars[FLength], Count);
      Inc(FLength, Count);
      Dec(RepeatCount);
    until RepeatCount <= 0;
  end;
  Result := Self;
end;

function TJclStringBuilder.InsertPChar(Index: SizeInt; Value: PChar; Count,
  RepeatCount: SizeInt): TJclStringBuilder;
var
  Capacity: SizeInt;
begin
  if (Index < 0) or (Index > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);

  if Index = FLength then
    AppendPChar(Value, Count, RepeatCount)
  else
  if (Count > 0) and (RepeatCount > 0) then
  begin
    repeat
      Capacity := System.Length(FChars);
      if Capacity + Count > MaxCapacity then
        raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
      if Capacity < FLength + Count then
        SetLength(FChars, Capacity * 5 div 3 + Count);
      MoveChar(FChars[Index], FChars[Index + Count], FLength - Index);
      if Count = 1 then
        FChars[Index] := Value[0]
      else
        MoveChar(Value[0], FChars[Index], Count);
      Inc(FLength, Count);

      Dec(RepeatCount);

      Inc(Index, Count); // little optimization
    until RepeatCount <= 0;
  end;
  Result := Self;
end;

function TJclStringBuilder.Append(const Value: array of Char): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if Len > 0 then
    AppendPChar(@Value[0], Len);
  Result := Self;
end;

function TJclStringBuilder.Append(const Value: array of Char; StartIndex, Length: SizeInt): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if (Length > 0) and (StartIndex < Len) then
  begin
    if StartIndex + Length > Len then
      Length := Len - StartIndex;
    AppendPChar(PChar(@Value[0]) + StartIndex, Length);
  end;
  Result := Self;
end;

function TJclStringBuilder.Append(Value: Char; RepeatCount: SizeInt = 1): TJclStringBuilder;
begin
  Result := AppendPChar(@Value, 1, RepeatCount);
end;

function TJclStringBuilder.Append(const Value: string): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if Len > 0 then
    AppendPChar(Pointer(Value), Len);
  Result := Self;
end;

function TJclStringBuilder.Append(const Value: string; StartIndex, Length: SizeInt): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if (Length > 0) and (StartIndex < Len) then
  begin
    if StartIndex + Length > Len then
      Length := Len - StartIndex;
    AppendPChar(PChar(Pointer(Value)) + StartIndex, Length);
  end;
  Result := Self;
end;

function TJclStringBuilder.Append(Value: Boolean): TJclStringBuilder;
begin
  Result := Append(BoolToStr[Value]);
end;

function TJclStringBuilder.Append(Value: Cardinal): TJclStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TJclStringBuilder.Append(Value: Integer): TJclStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TJclStringBuilder.Append(Value: Double): TJclStringBuilder;
begin
  Result := Append(FloatToStr(Value));
end;

function TJclStringBuilder.Append(Value: Int64): TJclStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TJclStringBuilder.Append(Obj: TObject): TJclStringBuilder;
begin
  Result := Append(DotNetFormat('{0}', [Obj]));
end;

function TJclStringBuilder.AppendFormat(const Fmt: string; Arg0: Variant): TJclStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, [Arg0]));
end;

function TJclStringBuilder.AppendFormat(const Fmt: string; Arg0, Arg1: Variant): TJclStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, [Arg0, Arg1]));
end;

function TJclStringBuilder.AppendFormat(const Fmt: string; Arg0, Arg1, Arg2: Variant): TJclStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, [Arg0, Arg1, Arg2]));
end;

function TJclStringBuilder.AppendFormat(const Fmt: string; const Args: array of const): TJclStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, Args));
end;

function TJclStringBuilder.Insert(Index: SizeInt; const Value: array of Char): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if Len > 0 then
    InsertPChar(Index, @Value[0], Len);
  Result := Self;
end;

function TJclStringBuilder.Insert(Index: SizeInt; const Value: string; Count: SizeInt): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if Len > 0 then
    InsertPChar(Index, Pointer(Value), Len, Count);
  Result := Self;
end;

function TJclStringBuilder.Insert(Index: SizeInt; Value: Boolean): TJclStringBuilder;
begin
  Result := Insert(Index, BoolToStr[Value]);
end;

function TJclStringBuilder.Insert(Index: SizeInt; const Value: array of Char;
  StartIndex, Length: SizeInt): TJclStringBuilder;
var
  Len: SizeInt;
begin
  Len := System.Length(Value);
  if (Length > 0) and (StartIndex < Len) then
  begin
    if StartIndex + Length > Len then
      Length := Len - StartIndex;
    InsertPChar(Index, PChar(@Value[0]) + StartIndex, Length);
  end;
  Result := Self;
end;

function TJclStringBuilder.Insert(Index: SizeInt; Value: Double): TJclStringBuilder;
begin
  Result := Insert(Index, FloatToStr(Value));
end;

function TJclStringBuilder.Insert(Index: SizeInt; Value: Int64): TJclStringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function TJclStringBuilder.Insert(Index: SizeInt; Value: Cardinal): TJclStringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function TJclStringBuilder.Insert(Index: SizeInt; Value: Integer): TJclStringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function TJclStringBuilder.Insert(Index: SizeInt; Obj: TObject): TJclStringBuilder;
begin
  Result := Insert(Index, DotNetFormat('{0}', [Obj]));
end;

function TJclStringBuilder.Remove(StartIndex, Length: SizeInt): TJclStringBuilder;
begin
  if (StartIndex < 0) or (Length < 0) or (StartIndex + Length >= FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  if Length > 0 then
  begin
    MoveChar(FChars[StartIndex + Length], FChars[StartIndex], FLength - (StartIndex + Length));
    Dec(FLength, Length);
  end;
  Result := Self;
end;

function TJclStringBuilder.Replace(OldChar, NewChar: Char; StartIndex,
  Count: SizeInt): TJclStringBuilder;
var
  I: SizeInt;
begin
  if Count = -1 then
    Count := FLength;
  if (StartIndex < 0) or (Count < 0) or (StartIndex + Count > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  if (Count > 0) and (OldChar <> NewChar) then
  begin
    for I := StartIndex to StartIndex + Length - 1 do
      if FChars[I] = OldChar then
        FChars[I] := NewChar;
  end;
  Result := Self;
end;

function TJclStringBuilder.Replace(OldValue, NewValue: string; StartIndex, Count: SizeInt): TJclStringBuilder;
var
  I: SizeInt;
  Offset: SizeInt;
  NewLen, OldLen, Capacity: SizeInt;
begin
  if Count = -1 then
    Count := FLength;
  if (StartIndex < 0) or (Count < 0) or (StartIndex + Count > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  if OldValue = '' then
    raise ArgumentException.CreateResFmt(@RsArgumentIsNull, [0]);

  if (Count > 0) and (OldValue <> NewValue) then
  begin
    OldLen := System.Length(OldValue);
    NewLen := System.Length(NewValue);
    Offset := NewLen - OldLen;
    Capacity := System.Length(FChars);
    for I := StartIndex to StartIndex + Length - 1 do
      if FChars[I] = OldValue[1] then
      begin
        if OldLen > 1 then
          if StrLComp(@FChars[I + 1], PChar(OldValue) + 1, OldLen - 1) <> 0 then
            Continue;
        if Offset <> 0 then
        begin
          if FLength - OldLen + NewLen > MaxCurrency then
            raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
          if Capacity < FLength + Offset then
          begin
            Capacity := Capacity * 5 div 3 + Offset;
            SetLength(FChars, Capacity);
          end;
          if Offset < 0 then
            MoveChar(FChars[I - Offset], FChars[I], FLength - I)
          else
            MoveChar(FChars[I + OldLen], FChars[I + OldLen + Offset], FLength - OldLen - I);
          Inc(FLength, Offset);
        end;
        if NewLen > 0 then
        begin
          if (OldLen = 1) and (NewLen = 1) then
            FChars[I] := NewValue[1]
          else
            MoveChar(NewValue[1], FChars[I], NewLen);
        end;
      end;
  end;
  Result := Self;
end;

function StrExpandTabs(S: string): string;
begin
  // use an empty tab set, which will default to a tab width of 2
  Result := TJclTabSet(nil).Expand(s);
end;

function StrExpandTabs(S: string; TabWidth: SizeInt): string;
var
  TabSet: TJclTabSet;
begin
  // create a tab set with no tab stops and the given tab width
  TabSet := TJclTabSet.Create(TabWidth);
  try
    Result := TabSet.Expand(S);
  finally
    TabSet.Free;
  end;
end;

function StrExpandTabs(S: string; TabSet: TJclTabSet): string;
begin
  // use the provided tab set to perform the expansion
  Result := TabSet.Expand(S);
end;

function StrOptimizeTabs(S: string): string;
begin
  // use an empty tab set, which will default to a tab width of 2
  Result := TJclTabSet(nil).Optimize(s);
end;

function StrOptimizeTabs(S: string; TabWidth: SizeInt): string;
var
  TabSet: TJclTabSet;
begin
  // create a tab set with no tab stops and the given tab width
  TabSet := TJclTabSet.Create(TabWidth);
  try
    Result := TabSet.Optimize(S);
  finally
    TabSet.Free;
  end;
end;

function StrOptimizeTabs(S: string; TabSet: TJclTabSet): string;
begin
  // use the provided tab set to perform the optimization
  Result := TabSet.Optimize(S);
end;

// === { TTabSetData } ===================================================

type
  TTabSetData = class
  public
    FStops: TDynSizeIntArray;
    FRealWidth: SizeInt;
    FRefCount: SizeInt;
    FWidth: SizeInt;
    FZeroBased: Boolean;
    constructor Create(TabStops: array of SizeInt; ZeroBased: Boolean; TabWidth: SizeInt);

    function Add(Column: SizeInt): SizeInt;
    function AddRef: SizeInt;
    procedure CalcRealWidth;
    function FindStop(Column: SizeInt): SizeInt;
    function ReleaseRef: SizeInt;
    procedure RemoveAt(Index: SizeInt);
    procedure SetStops(Index, Value: SizeInt);
  end;

constructor TTabSetData.Create(TabStops: array of SizeInt; ZeroBased: Boolean; TabWidth: SizeInt);
var
  idx: SizeInt;
begin
  inherited Create;
  FRefCount := 1;
  for idx := 0 to High(Tabstops) do
    Add(Tabstops[idx]);
  FWidth := TabWidth;
  FZeroBased := ZeroBased;
  CalcRealWidth;
end;

function TTabSetData.Add(Column: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  if Column < Ord(FZeroBased) then
    raise ArgumentOutOfRangeException.Create('Column');
  Result := FindStop(Column);
  if Result < 0 then
  begin
    // the column doesn't exist; invert the result of FindStop to get the correct index position
    Result := not Result;
    // increase the tab stop array
    SetLength(FStops, Length(FStops) + 1);
    // shift rooms after the insert position
    for I := High(FStops) - 1 downto Result do
      FStops[I + 1] := FStops[I];
    // add the tab stop at the correct location
    FStops[Result] := Column;
    CalcRealWidth;
  end
  else
  begin
    raise EJclStringError.CreateRes(@RsTabs_DuplicatesNotAllowed);
  end;
end;

function TTabSetData.AddRef: SizeInt;
begin
  Result := LockedInc(FRefCount);
end;

procedure TTabSetData.CalcRealWidth;
begin
  if FWidth < 1 then
  begin
    if Length(FStops) > 1 then
      FRealWidth := FStops[High(FStops)] - FStops[Pred(High(FStops))]
    else
    if Length(FStops) = 1 then
      FRealWidth := FStops[0]
    else
      FRealWidth := 2;
  end
  else
    FRealWidth := FWidth;
end;

function TTabSetData.FindStop(Column: SizeInt): SizeInt;
begin
  Result := High(FStops);
  while (Result >= 0) and (FStops[Result] > Column) do
    Dec(Result);
  if (Result >= 0) and (FStops[Result] <> Column) then
    Result := not Succ(Result);
end;

function TTabSetData.ReleaseRef: SizeInt;
begin
  Result := LockedDec(FRefCount);
  if Result <= 0 then
    Destroy;
end;

procedure TTabSetData.RemoveAt(Index: SizeInt);
var
  I: SizeInt;
begin
  for I := Index to High(FStops) - 1 do
    FStops[I] := FStops[I + 1];
  SetLength(FStops, High(FStops));
  CalcRealWidth;
end;

procedure TTabSetData.SetStops(Index, Value: SizeInt);
var
  temp: SizeInt;
begin
  if (Index < 0) or (Index >= Length(FStops)) then
  begin
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  end
  else
  begin
    temp := FindStop(Value);
    if temp < 0 then
    begin
      // remove existing tab stop...
      RemoveAt(Index);
      // now add the new tab stop
      Add(Value);
    end
    else
    if temp <> Index then
    begin
      // new tab stop already present at another index
      raise EJclStringError.CreateRes(@RsTabs_DuplicatesNotAllowed);
    end;
  end;
end;

//=== { TJclTabSet } =====================================================

constructor TJclTabSet.Create;
begin
  // no tab stops, tab width set to auto
  Create([], True, 0);
end;

constructor TJclTabSet.Create(TabWidth: SizeInt);
begin
  // no tab stops, specified tab width
  Create([], True, TabWidth);
end;

constructor TJclTabSet.Create(const Tabstops: array of SizeInt; ZeroBased: Boolean);
begin
  // specified tab stops, tab width equal to distance between last two tab stops
  Create(Tabstops, ZeroBased, 0);
end;

constructor TJclTabSet.Create(const Tabstops: array of SizeInt; ZeroBased: Boolean; TabWidth: SizeInt);
begin
  inherited Create;
  FData := TTabSetData.Create(Tabstops, ZeroBased, TabWidth);
end;

constructor TJclTabSet.Create(Data: TObject);
begin
  inherited Create;
  // add a reference to the data
  TTabSetData(Data).AddRef;
  // assign the data to this instance
  FData := TTabSetData(Data);
end;

destructor TJclTabSet.Destroy;
begin
  // release the reference to the tab set data
  TTabSetData(FData).ReleaseRef;
  // make sure we won't accidentally refer to it later, just in case something goes wrong during destruction
  FData := nil;
  // really destroy the instance
  inherited Destroy;
end;

function TJclTabSet.Add(Column: SizeInt): SizeInt;
begin
  if Self = nil then
    raise NullReferenceException.Create;
  Result := TTabSetData(FData).Add(Column);
end;

function TJclTabSet.Clone: TJclTabSet;
begin
  if Self <> nil then
    Result := TJclTabSet.Create(TTabSetData(FData).FStops, TTabSetData(FData).FZeroBased, TTabSetData(FData).FWidth)
  else
    Result := nil;
end;

function TJclTabSet.Delete(Column: SizeInt): SizeInt;
begin
  Result := TTabSetData(FData).FindStop(Column);
  if Result >= 0 then
    TTabSetData(FData).RemoveAt(Result);
end;

function TJclTabSet.Expand(const S: string): string;
begin
  Result := Expand(s, StartColumn);
end;

function TJclTabSet.Expand(const S: string; Column: SizeInt): string;
var
  sb: TJclStringBuilder;
  head: PChar;
  cur: PChar;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  sb := TJclStringBuilder.Create(Length(S));
  try
    cur := PChar(S);
    while cur^ <> #0 do
    begin
      head := cur;
      while (cur^ <> #0) and (cur^ <> #9) do
      begin
        if CharIsReturn(cur^) then
          Column := StartColumn
        else
          Inc(Column);
        Inc(cur);
      end;
      if cur > head then
        sb.Append(head, 0, cur - head);
      if cur^ = #9 then
      begin
        sb.Append(' ', TabFrom(Column) - Column);
        Column := TabFrom(Column);
        Inc(cur);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TJclTabSet.FindStop(Column: SizeInt): SizeInt;
begin
  if Self <> nil then
    Result := TTabSetData(FData).FindStop(Column)
  else
    Result := -1;
end;

class function TJclTabSet.FromString(const S: string): TJclTabSet;
var
  cur: PChar;

  function ParseNumber: Integer;
  var
    head: PChar;
  begin
    StrSkipChars(cur, CharIsWhiteSpace);
    head := cur;
    while CharIsDigit(cur^) do
      Inc(cur);
    Result := -1;
    if (cur <= head) or not TryStrToInt(Copy(head, 1, cur - head), Result) then
      Result := -1;
  end;

  procedure ParseStops;
  var
    openBracket, hadComma: Boolean;
    num: SizeInt;
  begin
    StrSkipChars(cur, CharIsWhiteSpace);
    openBracket := cur^ = '[';
    hadComma := False;
    if openBracket then
      Inc(cur);
    repeat
      num := ParseNumber;
      if (num < 0) and hadComma then
        raise EJclStringError.CreateRes(@RsTabs_StopExpected)
      else
      if num >= 0 then
        Result.Add(num);
      StrSkipChars(cur, CharIsWhiteSpace);
      hadComma := cur^ = ',';
      if hadComma then
        Inc(cur);
    until (cur^ = #0) or (cur^ = '+') or (cur^ = ']');
    if hadComma then
      raise EJclStringError.CreateRes(@RsTabs_StopExpected)
    else
    if openBracket and (cur^ <> ']') then
      raise EJclStringError.CreateRes(@RsTabs_CloseBracketExpected);
  end;

  procedure ParseTabWidth;
  var
    num: SizeInt;
  begin
    StrSkipChars(cur, CharIsWhiteSpace);
    if cur^ = '+' then
    begin
      Inc(cur);
      StrSkipChars(cur, CharIsWhiteSpace);
      num := ParseNumber;
      if (num < 0) then
        raise EJclStringError.CreateRes(@RsTabs_TabWidthExpected)
      else
        Result.TabWidth := num;
    end;
  end;

  procedure ParseZeroBasedFlag;
  begin
    StrSkipChars(cur, CharIsWhiteSpace);
    if cur^ = '0' then
    begin
      Inc(cur);
      if CharIsWhiteSpace(cur^) or (cur^ = #0) or (cur^ = '[') then
      begin
        Result.ZeroBased := True;
        StrSkipChars(cur, CharIsWhiteSpace);
      end
      else
        Dec(cur);
    end;
  end;

begin
  Result := TJclTabSet.Create;
  try
    Result.ZeroBased := False;
    cur := PChar(S);
    ParseZeroBasedFlag;
    ParseStops;
    ParseTabWidth;
  except
    // clean up the partially complete instance (to avoid memory leaks)...
    Result.Free;
    // ... and re-raise the exception
    raise;
  end;
end;

function TJclTabSet.GetCount: SizeInt;
begin
  if Self <> nil then
    Result := Length(TTabSetData(FData).FStops)
  else
    Result := 0;
end;

function TJclTabSet.GetStops(Index: SizeInt): SizeInt;
begin
  if Self <> nil then
  begin
    if (Index < 0) or (Index >= Length(TTabSetData(FData).FStops)) then
    begin
      raise EJclStringError.CreateRes(@RsArgumentOutOfRange);
    end
    else
      Result := TTabSetData(FData).FStops[Index];
  end
  else
  begin
    raise EJclStringError.CreateRes(@RsArgumentOutOfRange);
  end;
end;

function TJclTabSet.GetTabWidth: SizeInt;
begin
  if Self <> nil then
    Result := TTabSetData(FData).FWidth
  else
    Result := 0;
end;

function TJclTabSet.GetZeroBased: Boolean;
begin
  Result := (Self = nil) or TTabSetData(FData).FZeroBased;
end;

procedure TJclTabSet.OptimalFillInfo(StartColumn, TargetColumn: SizeInt; out TabsNeeded, SpacesNeeded: SizeInt);
var
  nextTab: SizeInt;
begin
  if StartColumn < Self.StartColumn then  // starting column less than 1 or 0 (depending on ZeroBased state)
    raise ArgumentOutOfRangeException.Create('StartColumn');
  if (TargetColumn < StartColumn) then    // target lies before the starting column
    raise ArgumentOutOfRangeException.Create('TargetColumn');
  TabsNeeded := 0;
  repeat
    nextTab := TabFrom(StartColumn);
    if nextTab <= TargetColumn then
    begin
      Inc(TabsNeeded);
      StartColumn := nextTab;
    end;
  until nextTab > TargetColumn;
  SpacesNeeded := TargetColumn - StartColumn;
end;

function TJclTabSet.Optimize(const S: string): string;
begin
  Result := Optimize(S, StartColumn);
end;

function TJclTabSet.Optimize(const S: string; Column: SizeInt): string;
var
  sb: TJclStringBuilder;
  head: PChar;
  cur: PChar;
  tgt: SizeInt;

  procedure AppendOptimalWhiteSpace(Target: SizeInt);
  var
    tabCount: SizeInt;
    spaceCount: SizeInt;
  begin
    if cur > head then
    begin
      OptimalFillInfo(Column, Target, tabCount, spaceCount);
      if tabCount > 0 then
        sb.Append(#9, tabCount);
      if spaceCount > 0 then
        sb.Append(' ', spaceCount);
    end;
  end;

begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  sb := TJclStringBuilder.Create(Length(S));
  try
    cur := PChar(s);
    while cur^ <> #0 do
    begin
      // locate first whitespace character
      head := cur;
      while (cur^ <> #0) and not CharIsWhiteSpace(cur^) do
        Inc(cur);
      // output non whitespace characters
      if cur > head then
        sb.Append(head, 0, cur - head);
      // advance column
      Inc(Column, cur - head);
      // initialize target column indexer
      tgt := Column;
      // locate end of whitespace sequence
      while CharIsWhiteSpace(cur^) do
      begin
        if CharIsReturn(cur^) then
        begin
          // append optimized whitespace sequence...
          AppendOptimalWhiteSpace(tgt);
          // ...set the column back to the start of the line...
          Column := StartColumn;
          // ...reset target column indexer...
          tgt := Column;
          // ...add the line break character...
          sb.Append(cur^);
        end
        else
        if cur^ = #9 then
          tgt := TabFrom(tgt)       // expand the tab
        else
          Inc(tgt);                 // a normal whitespace; taking up 1 column
        Inc(cur);
      end;
      AppendOptimalWhiteSpace(tgt); // append optimized whitespace sequence...
      Column := tgt;                // ...and memorize the column for the next iteration
    end;
    Result := sb.ToString;          // convert result to a string
  finally
    sb.Free;
  end;
end;

procedure TJclTabSet.RemoveAt(Index: SizeInt);
begin
  if Self <> nil then
    TTabSetData(FData).RemoveAt(Index)
  else
    raise NullReferenceException.Create;
end;

procedure TJclTabSet.SetStops(Index, Value: SizeInt);
begin
  if Self <> nil then
    TTabSetData(FData).SetStops(Index, Value)
  else
    raise NullReferenceException.Create;
end;

procedure TJclTabSet.SetTabWidth(Value: SizeInt);
begin
  if Self <> nil then
  begin
    TTabSetData(FData).FWidth := Value;
    TTabSetData(FData).CalcRealWidth;
  end
  else
    raise NullReferenceException.Create;
end;

procedure TJclTabSet.SetZeroBased(Value: Boolean);
var
  shift: SizeInt;
  idx:   SizeInt;
begin
  if Self <> nil then
  begin
    if Value <> TTabSetData(FData).FZeroBased then
    begin
      TTabSetData(FData).FZeroBased := Value;
      if Value then
        shift := -1
      else
        shift := 1;
      for idx := 0 to High(TTabSetData(FData).FStops) do
        TTabSetData(FData).FStops[idx] := TTabSetData(FData).FStops[idx] + shift;
    end;
  end
  else
    raise NullReferenceException.Create;
end;

function TJclTabSet.InternalTabStops: TDynSizeIntArray;
begin
  if Self <> nil then
    Result := TTabSetData(FData).FStops
  else
    Result := nil;
end;

function TJclTabSet.InternalTabWidth: SizeInt;
begin
  if Self <> nil then
    Result := TTabSetData(FData).FRealWidth
  else
    Result := 2;
end;

function TJclTabSet.NewReference: TJclTabSet;
begin
  if Self <> nil then
    Result := TJclTabSet.Create(FData)
  else
    Result := nil;
end;

function TJclTabSet.StartColumn: SizeInt;
begin
  if GetZeroBased then
    Result := 0
  else
    Result := 1;
end;

function TJclTabSet.TabFrom(Column: SizeInt): SizeInt;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  Result := FindStop(Column);
  if Result < 0 then
    Result := not Result
  else
    Inc(Result);
  if Result >= GetCount then
  begin
    if GetCount > 0 then
      Result := TTabSetData(FData).FStops[High(TTabSetData(FData).FStops)]
    else
      Result := StartColumn;
    while Result <= Column do
      Inc(Result, ActualTabWidth);
  end
  else
    Result := TTabSetData(FData).FStops[Result];
end;

function TJclTabSet.ToString: string;
begin
  Result := ToString(TabSetFormatting_Full);
end;

function TJclTabSet.ToString(FormattingOptions: SizeInt): string;
var
  sb: TJclStringBuilder;
  idx: SizeInt;

  function WantBrackets: Boolean;
  begin
    Result := (TabSetFormatting_SurroundStopsWithBrackets and FormattingOptions) <> 0;
  end;

  function EmptyBrackets: Boolean;
  begin
    Result := (TabSetFormatting_EmptyBracketsIfNoStops and FormattingOptions) <> 0;
  end;

  function IncludeAutoWidth: Boolean;
  begin
    Result := (TabSetFormatting_AutoTabWidth and FormattingOptions) <> 0;
  end;

  function IncludeTabWidth: Boolean;
  begin
    Result := (TabSetFormatting_NoTabWidth and FormattingOptions) = 0;
  end;

  function IncludeStops: Boolean;
  begin
    Result := (TabSetFormatting_NoTabStops and FormattingOptions) = 0;
  end;

begin
  sb := TJclStringBuilder.Create;
  try
    // output the fixed tabulation positions if requested...
    if IncludeStops then
    begin
      // output each individual tabulation position
      for idx := 0 to GetCount - 1 do
      begin
        sb.Append(TabStops[idx]);
        sb.Append(',');
      end;
      // remove the final comma if any tabulation positions where outputted
      if sb.Length <> 0 then
        sb.Remove(sb.Length - 1, 1);
      // bracket the tabulation positions if requested
      if WantBrackets and (EmptyBrackets or (sb.Length > 0)) then
      begin
        sb.Insert(0, '[');
        sb.Append(']');
      end;
    end;
    // output the tab width if requested....
    if IncludeTabWidth and (IncludeAutoWidth or (TabWidth > 0)) then
    begin
      // separate the tab width from any outputted tabulation positions with a whitespace
      if sb.Length > 0 then
        sb.Append(' ');
      // flag tab width
      sb.Append('+');
      // finally, output the tab width
      sb.Append(ActualTabWidth);
    end;
    // flag zero-based tabset by outputting a 0 (zero) as the first character.
    if ZeroBased then
      sb.Insert(0, string('0 '));
    Result := StrTrimCharRight(sb.ToString, ' ');
  finally
    sb.Free;
  end;
end;

function TJclTabSet.UpdatePosition(const S: string): SizeInt;
var
  Line: SizeInt;
begin
  Result := StartColumn;
  Line := -1;
  UpdatePosition(S, Result, Line);
end;

function TJclTabSet.UpdatePosition(const S: string; Column: SizeInt): SizeInt;
var
  Line: SizeInt;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  Result := Column;
  Line := -1;
  UpdatePosition(S, Result, Line);
end;

function TJclTabSet.UpdatePosition(const S: string; var Column, Line: SizeInt): SizeInt;
var
  prevChar: Char;
  cur:      PChar;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  // initialize loop
  cur := PChar(S);
  // iterate until end of string (the Null-character)
  while cur^ <> #0 do
  begin
    // check for line-breaking characters
    if CharIsReturn(cur^) then
    begin
      // Column moves back all the way to the left
      Column := StartColumn;
      // If this is the first line-break character or the same line-break character, increment the Line parameter
      Inc(Line);
      // check if it's the first of a two-character line-break
      prevChar := cur^;
      Inc(cur);
      // if it isn't a two-character line-break, undo the previous advancement
      if (cur^ = prevChar) or not CharIsReturn(cur^) then
        Dec(cur);
    end
    else // check for tab character and expand it
    if cur^ = #9 then
      Column := TabFrom(Column)
    else // a normal character; increment column
      Inc(Column);
    // advance pointer
    Inc(cur);
  end;
  // set the result to the newly calculated column
  Result := Column;
end;

//=== { NullReferenceException } =============================================

constructor NullReferenceException.Create;
begin
  CreateRes(@RsArg_NullReferenceException);
end;

function CompareNatural(const S1, S2: string; CaseInsensitive: Boolean): SizeInt;
var
  Cur1, Len1,
  Cur2, Len2: SizeInt;

  function IsRealNumberChar(ch: Char): Boolean;
  begin
    Result := ((ch >= '0') and (ch <= '9')) or (ch = '-') or (ch = '+');
  end;

  procedure NumberCompare;
  var
    IsReallyNumber: Boolean;
    FirstDiffBreaks: Boolean;
    Val1, Val2:     SizeInt;
  begin
    Result := 0;
    IsReallyNumber := False;
    // count leading spaces in S1
    while (Cur1 <= Len1) and CharIsWhiteSpace(S1[Cur1]) do
    begin
      Dec(Result);
      Inc(Cur1);
    end;
    // count leading spaces in S2 (canceling them out against the ones in S1)
    while (Cur2 <= Len2) and CharIsWhiteSpace(S2[Cur2]) do
    begin
      Inc(Result);
      Inc(Cur2);
    end;

    // if spaces match, or both strings are actually followed by a numeric character, continue the checks
    if (Result = 0) or ((Cur1 <= Len1) and CharIsNumberChar(S1[Cur1]) and (Cur2 <= Len2) and CharIsNumberChar(S2[Cur2])) then
    begin
      // Check signed number
      if (Cur1 <= Len1) and (S1[Cur1] = '-') and ((Cur2 > Len2) or (S2[Cur2] <> '-')) then
        Result := 1
      else
      if (Cur2 <= Len2) and (S2[Cur2] = '-') and ((Cur1 > Len1) or (S1[Cur1] <> '-')) then
        Result := -1
      else
        Result := 0;

      if (Cur1 <= Len1) and ((S1[Cur1] = '-') or (S1[Cur1] = '+')) then
        Inc(Cur1);
      if (Cur2 <= Len2) and ((S2[Cur2] = '-') or (S2[Cur2] = '+')) then
        Inc(Cur2);

      FirstDiffBreaks := (Cur1 <= Len1) and (S1[Cur1] = '0') or (Cur2 <= Len2) and (S2[Cur2] = '0');
      while (Cur1 <= Len1) and CharIsDigit(S1[Cur1]) and (Cur2 <= Len2) and CharIsDigit(S2[Cur2]) do
      begin
        IsReallyNumber := True;
        Val1 := StrToInt(S1[Cur1]);
        Val2 := StrToInt(S2[Cur2]);

        if (Result = 0) and (Val1 < Val2) then
          Result := -1
        else
        if (Result = 0) and (Val1 > Val2) then
          Result := 1;
        if FirstDiffBreaks and (Result <> 0) then
          Break;
        Inc(Cur1);
        Inc(Cur2);
      end;

      if IsReallyNumber then
      begin
        if not FirstDiffBreaks then
        begin
          if (Cur1 <= Len1) and CharIsDigit(S1[Cur1]) then
            Result := 1
          else
          if (Cur2 <= Len2) and CharIsDigit(S2[Cur2]) then
            Result := -1;
        end;
      end;
    end;
  end;

  procedure SetByCompareLength;
  var
    Remain1: SizeInt;
    Remain2: SizeInt;
  begin
    // base result on relative compare length (spaces could be ignored, so even if S1 is longer than S2, they could be
    // completely equal, or S2 could be longer)
    Remain1 := Len1 - Cur1 + 1;
    Remain2 := Len2 - Cur2 + 1;
    if Remain1 < 0 then
      Remain1 := 0;
    if Remain2 < 0 then
      Remain2 := 0;

    if Remain1 < Remain2 then
      Result := -1
    else
    if Remain1 > Remain2 then
      Result := 1;
  end;

begin
  Cur1 := 1;
  Len1 := Length(S1);
  Cur2 := 1;
  Len2 := Length(S2);
  Result := 0;

  while (Result = 0) do
  begin
    if (Cur1 > Len1) or (Cur2 > Len2) then
    begin
      SetByCompareLength;
      Break;
    end
    else
    if (Cur1 <= Len1) and (Cur2 > Len2) then
      Result := 1
    else
    if (S1[Cur1] = '-') and IsRealNumberChar(S2[Cur2]) and (S2[Cur2] <> '-') then
      Result := -1
    else
    if (S2[Cur2] = '-') and IsRealNumberChar(S1[Cur1]) and (S1[Cur1] <> '-') then
      Result := 1
    else
    if (IsRealNumberChar(S1[Cur1]) or CharIsWhiteSpace(S1[Cur1])) and (IsRealNumberChar(S2[Cur2]) or CharIsWhiteSpace(S2[Cur2])) then
      NumberCompare
    else
    begin
      if CaseInsensitive then
        Result := StrLIComp(PChar(@S1[Cur1]), PChar(@S2[Cur2]), 1)
      else
        Result := StrLComp(PChar(@S1[Cur1]), PChar(@S2[Cur2]), 1);
      Inc(Cur1);
      Inc(Cur2);
    end;
  end;
end;

function CompareNaturalStr(const S1, S2: string): SizeInt; overload;
begin
  Result := CompareNatural(S1, S2, False);
end;

function CompareNaturalText(const S1, S2: string): SizeInt; overload;
begin
  Result := CompareNatural(S1, S2, True);
end;

initialization
  {$IFNDEF UNICODE_RTL_DATABASE}
  LoadCharTypes;  // this table first
  LoadCaseMap;    // or this function does not work
  {$ENDIF ~UNICODE_RTL_DATABASE}
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

