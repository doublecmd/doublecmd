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
{   Leonard Wennekers                                                                              }
{   Martin Kimmings                                                                                }
{   Martin Kubecka                                                                                 }
{   Massimo Maria Ghisalberti                                                                      }
{   Matthias Thoma (mthoma)                                                                        }
{   Michael Winter                                                                                 }
{   Nick Hodges                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Patrick Kolla                                                                                  }
{   Pelle F. S. Liljendal                                                                          }
{   Petr Vones (pvones)                                                                            }
{   Robert Lee                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Andreas Schmidt                                                                                }
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

unit JclAnsiStrings; // former JclStrings

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
  System.Classes, System.SysUtils,
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  System.AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

// Ansi types

type
  {$IFDEF SUPPORTS_UNICODE}
  TJclAnsiStringList = class;

  // Codegear should be the one providing this class, in the AnsiStrings unit.
  // It has been requested in QC 65630 but this was closed as "won't do".
  // So we are providing here a very light implementation that is designed
  // to provide the basics, and in no way be a "copy/paste" of what is in the RTL.
  TJclAnsiStrings = class(TPersistent)
  private
    FDelimiter: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FStrictDelimiter: Boolean;
    FQuoteChar: AnsiChar;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
    function GetCommaText: AnsiString; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure SetCommaText(const Value: AnsiString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function GetDelimitedText: AnsiString; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function GetDelimitedText(const ADelimiter: AnsiString; AQuoteChar: AnsiChar): AnsiString; overload;
    procedure SetDelimitedText(const Value: AnsiString); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure SetDelimitedText(const Value, ADelimiter: AnsiString; AQuoteChar: AnsiChar); overload;
    function ExtractName(const S: AnsiString): AnsiString;
    function GetName(Index: Integer): AnsiString;
    function GetValue(const Name: AnsiString): AnsiString;
    procedure SetValue(const Name, Value: AnsiString);
    function GetValueFromIndex(Index: Integer): AnsiString;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;

    function GetString(Index: Integer): AnsiString; virtual; abstract;
    procedure SetString(Index: Integer; const Value: AnsiString); virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual; abstract;
    procedure SetObject(Index: Integer; AObject: TObject); virtual; abstract;

    function GetCapacity: Integer; virtual;
    procedure SetCapacity(const Value: Integer); virtual;
    function GetCount: Integer; virtual; abstract;
    function CompareStrings(const S1, S2: AnsiString): Integer; virtual;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;

    function Add(const S: AnsiString): Integer; virtual;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; virtual; abstract;
    procedure AddStrings(Strings: TJclAnsiStrings); virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: TFileName); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IndexOf(const S: AnsiString): Integer; virtual;
    function IndexOfName(const Name: AnsiString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;

    property Delimiter: AnsiChar read FDelimiter write FDelimiter;
    property DelimitedText: AnsiString read GetDelimitedText write SetDelimitedText;
    property CommaText: AnsiString read GetCommaText write SetCommaText;
    property StrictDelimiter: Boolean read FStrictDelimiter write FStrictDelimiter;
    property QuoteChar: AnsiChar read FQuoteChar write FQuoteChar;

    property Strings[Index: Integer]: AnsiString read GetString write SetString; default;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Text: AnsiString read GetText write SetText;
    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Names[Index: Integer]: AnsiString read GetName;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: AnsiChar read FNameValueSeparator write FNameValueSeparator;
  end;

  TJclAnsiStringListSortCompare = function(List: TJclAnsiStringList; Index1, Index2: Integer): Integer;

  TJclAnsiStringObjectHolder = record
    Str: AnsiString;
    Obj: TObject;
  end;

  TJclAnsiStringList = class(TJclAnsiStrings)
  private
    FStrings: array of TJclAnsiStringObjectHolder;
    FCount: Integer;
    FDuplicates: TDuplicates;
    FSorted: Boolean;
    FCaseSensitive: Boolean;
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TJclAnsiStringListSortCompare);
    procedure SetSorted(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetString(Index: Integer): AnsiString; override;
    procedure SetString(Index: Integer; const Value: AnsiString); override;
    function GetObject(Index: Integer): TObject; override;
    procedure SetObject(Index: Integer; AObject: TObject); override;
    function GetCapacity: Integer; override;
    procedure SetCapacity(const Value: Integer); override;
    function GetCount: Integer; override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
  public
    constructor Create;

    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure InsertObject(Index: Integer; const S: AnsiString; AObject: TObject); override;
    procedure Delete(Index: Integer); override;
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
    procedure CustomSort(Compare: TJclAnsiStringListSortCompare); virtual;
    procedure Sort; virtual;
    procedure Clear; override;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
  end;
  {$ELSE ~SUPPORTS_UNICODE}
  TJclAnsiStrings = Classes.TStrings;
  TJclAnsiStringList = Classes.TStringList;
  {$ENDIF ~SUPPORTS_UNICODE}

  TAnsiStrings = TJclAnsiStrings;
  TAnsiStringList = TJclAnsiStringList;

// Exceptions
type
  EJclAnsiStringError = class(EJclError);
  EJclAnsiStringListError = class(EJclAnsiStringError);

// Character constants and sets

const
  // Misc. often used character definitions
  AnsiNull           = AnsiChar(#0);
  AnsiSoh            = AnsiChar(#1);
  AnsiStx            = AnsiChar(#2);
  AnsiEtx            = AnsiChar(#3);
  AnsiEot            = AnsiChar(#4);
  AnsiEnq            = AnsiChar(#5);
  AnsiAck            = AnsiChar(#6);
  AnsiBell           = AnsiChar(#7);
  AnsiBackspace      = AnsiChar(#8);
  AnsiTab            = AnsiChar(#9);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiVerticalTab    = AnsiChar(#11);
  AnsiFormFeed       = AnsiChar(#12);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  AnsiSo             = AnsiChar(#14);
  AnsiSi             = AnsiChar(#15);
  AnsiDle            = AnsiChar(#16);
  AnsiDc1            = AnsiChar(#17);
  AnsiDc2            = AnsiChar(#18);
  AnsiDc3            = AnsiChar(#19);
  AnsiDc4            = AnsiChar(#20);
  AnsiNak            = AnsiChar(#21);
  AnsiSyn            = AnsiChar(#22);
  AnsiEtb            = AnsiChar(#23);
  AnsiCan            = AnsiChar(#24);
  AnsiEm             = AnsiChar(#25);
  AnsiEndOfFile      = AnsiChar(#26);
  AnsiEscape         = AnsiChar(#27);
  AnsiFs             = AnsiChar(#28);
  AnsiGs             = AnsiChar(#29);
  AnsiRs             = AnsiChar(#30);
  AnsiUs             = AnsiChar(#31);
  AnsiSpace          = AnsiChar(' ');
  AnsiComma          = AnsiChar(',');
  AnsiBackslash      = AnsiChar('\');
  AnsiForwardSlash   = AnsiChar('/');

  AnsiDoubleQuote = AnsiChar('"');
  AnsiSingleQuote = AnsiChar('''');

  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  AnsiLineBreak = AnsiLineFeed;
  {$ENDIF UNIX}

  AnsiSignMinus = AnsiChar('-');
  AnsiSignPlus  = AnsiChar('+');

  // Misc. character sets

  AnsiWhiteSpace             = [AnsiTab, AnsiLineFeed, AnsiVerticalTab,
    AnsiFormFeed, AnsiCarriageReturn, AnsiSpace];
  AnsiSigns                  = [AnsiSignMinus, AnsiSignPlus];
  AnsiUppercaseLetters       = ['A'..'Z'];
  AnsiLowercaseLetters       = ['a'..'z'];
  AnsiLetters                = ['A'..'Z', 'a'..'z'];
  AnsiDecDigits              = ['0'..'9'];
  AnsiOctDigits              = ['0'..'7'];
  AnsiHexDigits              = ['0'..'9', 'A'..'F', 'a'..'f'];
  AnsiValidIdentifierLetters = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];

const
  // CharType return values
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

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

// String Test Routines
function StrIsAlpha(const S: AnsiString): Boolean;
function StrIsAlphaNum(const S: AnsiString): Boolean;
function StrIsAlphaNumUnderscore(const S: AnsiString): Boolean;
function StrContainsChars(const S: AnsiString; Chars: TSysCharSet; CheckAll: Boolean): Boolean;
function StrConsistsOfNumberChars(const S: AnsiString): Boolean;
function StrIsDigit(const S: AnsiString): Boolean;
function StrIsSubset(const S: AnsiString; const ValidChars: TSysCharSet): Boolean;
function StrSame(const S1, S2: AnsiString): Boolean;

// String Transformation Routines
function StrCenter(const S: AnsiString; L: SizeInt; C: AnsiChar = ' '): AnsiString;
function StrCharPosLower(const S: AnsiString; CharPos: SizeInt): AnsiString;
function StrCharPosUpper(const S: AnsiString; CharPos: SizeInt): AnsiString;
function StrDoubleQuote(const S: AnsiString): AnsiString;
function StrEnsureNoPrefix(const Prefix, Text: AnsiString): AnsiString;
function StrEnsureNoSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrEscapedToString(const S: AnsiString): AnsiString;
function StrLower(const S: AnsiString): AnsiString;
procedure StrLowerInPlace(var S: AnsiString);
procedure StrLowerBuff(S: PAnsiChar);
procedure StrMove(var Dest: AnsiString; const Source: AnsiString; const ToIndex,
  FromIndex, Count: SizeInt);
function StrPadLeft(const S: AnsiString; Len: SizeInt; C: AnsiChar = AnsiSpace): AnsiString;
function StrPadRight(const S: AnsiString; Len: SizeInt; C: AnsiChar = AnsiSpace): AnsiString;
function StrProper(const S: AnsiString): AnsiString;
procedure StrProperBuff(S: PAnsiChar);
function StrQuote(const S: AnsiString; C: AnsiChar): AnsiString;
function StrRemoveChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrKeepChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString; Flags: TReplaceFlags = []);
function StrReplaceChar(const S: AnsiString; const Source, Replace: AnsiChar): AnsiString;
function StrReplaceChars(const S: AnsiString; const Chars: TSysCharSet; Replace: AnsiChar): AnsiString;
function StrReplaceButChars(const S: AnsiString; const Chars: TSysCharSet; Replace: AnsiChar): AnsiString;
function StrRepeat(const S: AnsiString; Count: SizeInt): AnsiString;
function StrRepeatLength(const S: AnsiString; const L: SizeInt): AnsiString;
function StrReverse(const S: AnsiString): AnsiString;
procedure StrReverseInPlace(var S: AnsiString);
function StrSingleQuote(const S: AnsiString): AnsiString;
procedure StrSkipChars(var S: PAnsiChar; const Chars: TSysCharSet); overload;
procedure StrSkipChars(const S: AnsiString; var Index: SizeInt; const Chars: TSysCharSet); overload;
function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
function StrStringToEscaped(const S: AnsiString): AnsiString;
function StrStripNonNumberChars(const S: AnsiString): AnsiString;
function StrToHex(const Source: AnsiString): AnsiString;
function StrTrimCharLeft(const S: AnsiString; C: AnsiChar): AnsiString;
function StrTrimCharsLeft(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrTrimCharRight(const S: AnsiString; C: AnsiChar): AnsiString;
function StrTrimCharsRight(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrTrimQuotes(const S: AnsiString): AnsiString; overload;
function StrTrimQuotes(const S: AnsiString; QuoteChar: AnsiChar): AnsiString; overload;
function StrUpper(const S: AnsiString): AnsiString;
procedure StrUpperInPlace(var S: AnsiString);
procedure StrUpperBuff(S: PAnsiChar);

{$IFDEF MSWINDOWS}
function StrOemToAnsi(const S: AnsiString): AnsiString;
function StrAnsiToOem(const S: AnsiString): AnsiString;
{$ENDIF MSWINDOWS}

// String Management
procedure StrAddRef(var S: AnsiString);
procedure StrDecRef(var S: AnsiString);
function StrLength(const S: AnsiString): Longint;
function StrRefCount(const S: AnsiString): Longint;
procedure StrResetLength(var S: AnsiString);

// String Search and Replace Routines
function StrCharCount(const S: AnsiString; C: AnsiChar): SizeInt;
function StrCharsCount(const S: AnsiString; Chars: TSysCharSet): SizeInt;
function StrStrCount(const S, SubS: AnsiString): SizeInt;
function StrCompare(const S1, S2: AnsiString; CaseSensitive: Boolean = False): SizeInt;
function StrCompareRangeEx(const S1, S2: AnsiString; Index, Count: SizeInt; CaseSensitive: Boolean = False): SizeInt;
function StrCompareRange(const S1, S2: AnsiString; Index, Count: SizeInt; CaseSensitive: Boolean = True): SizeInt;
function StrRepeatChar(C: AnsiChar; Count: SizeInt): AnsiString;
function StrFind(const Substr, S: AnsiString; const Index: SizeInt = 1): SizeInt;
function StrHasPrefix(const S: AnsiString; const Prefixes: array of AnsiString): Boolean;
function StrHasSuffix(const S: AnsiString; const Suffixes: array of AnsiString): Boolean;
function StrIHasPrefix(const S: AnsiString; const Prefixes: array of AnsiString): Boolean;
function StrIHasSuffix(const S: AnsiString; const Suffixes: array of AnsiString): Boolean;
function StrIndex(const S: AnsiString; const List: array of AnsiString; CaseSensitive: Boolean = False): SizeInt;
function StrILastPos(const SubStr, S: AnsiString): SizeInt;
function StrIPos(const SubStr, S: AnsiString): SizeInt;
function StrIPrefixIndex(const S: AnsiString; const Prefixes: array of AnsiString): SizeInt;
function StrIsOneOf(const S: AnsiString; const List: array of AnsiString): Boolean;
function StrISuffixIndex(const S: AnsiString; const Suffixes: array of AnsiString): SizeInt;
function StrLastPos(const SubStr, S: AnsiString): SizeInt;
function StrMatch(const Substr, S: AnsiString; Index: SizeInt = 1): SizeInt;
function StrMatches(const Substr, S: AnsiString; const Index: SizeInt = 1): Boolean;
function StrNIPos(const S, SubStr: AnsiString; N: SizeInt): SizeInt;
function StrNPos(const S, SubStr: AnsiString; N: SizeInt): SizeInt;
function StrPrefixIndex(const S: AnsiString; const Prefixes: array of AnsiString): SizeInt;
function StrSearch(const Substr, S: AnsiString; const Index: SizeInt = 1): SizeInt;
function StrSuffixIndex(const S: AnsiString; const Suffixes: array of AnsiString): SizeInt;

// String Extraction
// String Extraction
// Returns the String before SubStr
function StrAfter(const SubStr, S: AnsiString): AnsiString;
/// Returns the AnsiString after SubStr
function StrBefore(const SubStr, S: AnsiString): AnsiString;
/// Splits a AnsiString at SubStr, returns true when SubStr is found, Left contains the
/// AnsiString before the SubStr and Rigth the AnsiString behind SubStr
function StrSplit(const SubStr, S: AnsiString;var Left, Right : AnsiString): boolean;
/// Returns the AnsiString between Start and Stop
function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
/// Returns the left N characters of the AnsiString
function StrChopRight(const S: AnsiString; N: SizeInt): AnsiString;
/// Returns the left Count characters of the AnsiString
function StrLeft(const S: AnsiString; Count: SizeInt): AnsiString;
/// Returns the AnsiString starting from position Start for the Count Characters
function StrMid(const S: AnsiString; Start, Count: SizeInt): AnsiString;
/// Returns the AnsiString starting from position N to the end
function StrRestOf(const S: AnsiString; N: SizeInt): AnsiString;
/// Returns the right Count characters of the AnsiString
function StrRight(const S: AnsiString; Count: SizeInt): AnsiString;

// Character Test Routines
function CharEqualNoCase(const C1, C2: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsAlpha(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsAlphaNum(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsBlank(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsControl(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsDelete(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsDigit(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsFracDigit(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsHexDigit(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsLower(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsNumberChar(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsNumber(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsPrintable(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsPunctuation(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsReturn(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsSpace(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsUpper(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsValidIdentifierLetter(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsWhiteSpace(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsWildcard(const C: AnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharType(const C: AnsiChar): Word;

// Character Transformation Routines
function CharHex(const C: AnsiChar): Byte;
function CharLower(const C: AnsiChar): AnsiChar;
function CharUpper(const C: AnsiChar): AnsiChar;
function CharToggleCase(const C: AnsiChar): AnsiChar;

// Character Search and Replace
function CharPos(const S: AnsiString; const C: AnsiChar; const Index: SizeInt = 1): SizeInt;
function CharLastPos(const S: AnsiString; const C: AnsiChar; const Index: SizeInt = 1): SizeInt;
function CharIPos(const S: AnsiString; C: AnsiChar; const Index: SizeInt = 1): SizeInt;
function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): SizeInt;

// PCharVector
type
  PAnsiCharVector = ^PAnsiChar;

function StringsToPCharVector(var Dest: PAnsiCharVector; const Source: TJclAnsiStrings): PAnsiCharVector;
function PCharVectorCount(Source: PAnsiCharVector): SizeInt;
procedure PCharVectorToStrings(const Dest: TJclAnsiStrings; Source: PAnsiCharVector);
procedure FreePCharVector(var Dest: PAnsiCharVector);

// MultiSz Routines
type
  PAnsiMultiSz = PAnsiChar;

function StringsToMultiSz(var Dest: PAnsiMultiSz; const Source: TJclAnsiStrings): PAnsiMultiSz;
procedure MultiSzToStrings(const Dest: TJclAnsiStrings; const Source: PAnsiMultiSz);
function MultiSzLength(const Source: PAnsiMultiSz): SizeInt;
procedure AllocateMultiSz(var Dest: PAnsiMultiSz; Len: SizeInt);
procedure FreeMultiSz(var Dest: PAnsiMultiSz);
function MultiSzDup(const Source: PAnsiMultiSz): PAnsiMultiSz;

// TJclAnsiStrings Manipulation
procedure StrIToStrings(S, Sep: AnsiString; const List: TJclAnsiStrings; const AllowEmptyString: Boolean = True);
procedure StrToStrings(S, Sep: AnsiString; const List: TJclAnsiStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TJclAnsiStrings; const Sep: AnsiString; const AllowEmptyString: Boolean = True): AnsiString;
procedure TrimStrings(const List: TJclAnsiStrings; DeleteIfEmpty: Boolean = True);
procedure TrimStringsRight(const List: TJclAnsiStrings; DeleteIfEmpty: Boolean = True);
procedure TrimStringsLeft(const List: TJclAnsiStrings; DeleteIfEmpty: Boolean = True);
function AddStringToStrings(const S: AnsiString; Strings: TJclAnsiStrings; const Unique: Boolean): Boolean;

// Miscellaneous
// (OF) moved to JclSysUtils
//function BooleanToStr(B: Boolean): AnsiString;
function FileToString(const FileName: TFileName): AnsiString;
procedure StringToFile(const FileName: TFileName; const Contents: AnsiString; Append: Boolean = False);
function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
procedure StrTokens(const S: AnsiString; const List: TJclAnsiStrings);
procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; const List: TJclAnsiStrings);
function StrWord(const S: AnsiString; var Index: SizeInt; out Word: AnsiString): Boolean; overload;
function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean; overload;
function StrIdent(const S: AnsiString; var Index: SizeInt; out Ident: AnsiString): Boolean; overload;
function StrIdent(var S: PAnsiChar; out Ident: AnsiString): Boolean; overload;
function StrToFloatSafe(const S: AnsiString): Float;
function StrToIntSafe(const S: AnsiString): Integer;
procedure StrNormIndex(const StrLen: SizeInt; var Index: SizeInt; var Count: SizeInt); overload;

function ArrayOf(List: TJclAnsiStrings): TDynStringArray; overload;

function AnsiCompareNaturalStr(const S1, S2: AnsiString): SizeInt; overload;
function AnsiCompareNaturalText(const S1, S2: AnsiString): SizeInt; overload;

// Explicit ANSI version of former/deprecated SysUtils PAnsiChar functions
{$IFNDEF DEPRECATED_SYSUTILS_ANSISTRINGS}
  {$IFDEF SUPPORTS_INLINE}
    {$DEFINE ANSI_INLINE} // inline if the functions are in SysUtils but don't force the user to include System.AnsiStrings
  {$ENDIF SUPPORTS_INLINE}
{$ENDIF ~DEPRECATED_SYSUTILS_ANSISTRINGS}
function StrNewA(const Str: PAnsiChar): PAnsiChar;                                           {$IFDEF ANSI_INLINE}inline;{$ENDIF}
procedure StrDisposeA(Str: PAnsiChar);                                                       {$IFDEF ANSI_INLINE}inline;{$ENDIF}

function StrLenA(S: PAnsiChar): Integer;                                                     {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrEndA(const Str: PAnsiChar): PAnsiChar;                                           {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrPosA(const Str1, Str2: PAnsiChar): PAnsiChar;                                    {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrPasA(const Str: PAnsiChar): AnsiString;                                          {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrCopyA(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;                      {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrLCopyA(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;   {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrPCopyA(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar;                    {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrPLCopyA(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar; {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrECopyA(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;                     {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrCatA(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;                       {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrLCatA(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;    {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrCompA(const Str1, Str2: PAnsiChar): Integer;                                     {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrLCompA(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;                  {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrICompA(const Str1, Str2: PAnsiChar): Integer;                                    {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function StrLICompA(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;                 {$IFDEF ANSI_INLINE}inline;{$ENDIF}

function StrFmtA(Buffer, Format: PAnsiChar; const Args: array of const): PAnsiChar;

function AnsiStrPosA(const Str1, Str2: PAnsiChar): PAnsiChar;                                {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function AnsiStrLICompA(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;                       {$IFDEF ANSI_INLINE}inline;{$ENDIF}
function AnsiStrLCompA(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;                        {$IFDEF ANSI_INLINE}inline;{$ENDIF}


// internal structures published to make function inlining working
const
  AnsiCharCount   = Ord(High(AnsiChar)) + 1; // # of chars in one set
  AnsiLoOffset    = AnsiCharCount * 0;       // offset to lower case chars
  AnsiUpOffset    = AnsiCharCount * 1;       // offset to upper case chars
  AnsiReOffset    = AnsiCharCount * 2;       // offset to reverse case chars
  AnsiCaseMapSize = AnsiCharCount * 3;       // # of chars is a table

var
  AnsiCaseMap: array [0..AnsiCaseMapSize - 1] of AnsiChar; // case mappings
  AnsiCaseMapReady: Boolean = False;         // true if case map exists
  AnsiCharTypes: array [AnsiChar] of Word;

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
  RtlConsts,
  {$ENDIF SUPPORTS_UNICODE}
  JclLogic, JclResources, JclStreams, JclSynch, JclSysUtils;

//=== Internal ===============================================================

type
  TAnsiStrRec = packed record
    RefCount: SizeInt;
    Length: SizeInt;
  end;
  PAnsiStrRec = ^TAnsiStrRec;

const
  AnsiStrRecSize  = SizeOf(TAnsiStrRec);     // size of the AnsiString header rec

procedure LoadCharTypes;
var
  CurrChar: AnsiChar;
  CurrType: Word;
begin
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
    {$IFDEF MSWINDOWS}
    CurrType := 0;
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
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
    AnsiCharTypes[CurrChar] := CurrType;
    {$IFNDEF CHAR_TYPES_INITIALIZED}
    Implement case map initialization here
    {$ENDIF ~CHAR_TYPES_INITIALIZED}
  end;
end;

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: AnsiChar;
begin
  if not AnsiCaseMapReady then
  begin
    for CurrChar := Low(AnsiChar) to High(AnsiChar) do
    begin
      {$IFDEF MSWINDOWS}
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CharLowerBuffA(@LoCaseChar, 1);
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CharUpperBuffA(@UpCaseChar, 1);
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      LoCaseChar := AnsiChar(tolower(Byte(CurrChar)));
      UpCaseChar := AnsiChar(toupper(Byte(CurrChar)));
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
      AnsiCaseMap[Ord(CurrChar) + AnsiLoOffset] := LoCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiUpOffset] := UpCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiReOffset] := ReCaseChar;
    end;
    AnsiCaseMapReady := True;
  end;
end;

// Uppercases or Lowercases a give AnsiString depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCase(var Str: AnsiString; const Offset: SizeInt);
var
  P: PAnsiChar;
  I, L: SizeInt;
begin
  if Str <> '' then
  begin
    UniqueString(Str);
    P := PAnsiChar(Str);
    L := Length(Str);
    for I := 1 to L do
    begin
      P^ := AnsiCaseMap[Offset + Ord(P^)];
      Inc(P);
    end;
  end;
end;

// Internal utility function
// Uppercases or Lowercases a give null terminated string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCaseBuff(S: PAnsiChar; const Offset: SizeInt);
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    repeat
      S^ := AnsiCaseMap[Offset + Ord(S^)];
      Inc(S);
    until S^ = #0;
  end;
end;

{$IFDEF SUPPORTS_UNICODE}

//=== { TJclAnsiStrings } ====================================================

constructor TJclAnsiStrings.Create;
begin
  inherited Create;

  FDelimiter := ',';
  FNameValueSeparator := '=';
  FQuoteChar := '"';
  FStrictDelimiter := False;
end;

procedure TJclAnsiStrings.Assign(Source: TPersistent);
var
  StringsSource: TStrings;
  I: Integer;
begin
  if Source is TStrings then
  begin
    StringsSource := TStrings(Source);
    BeginUpdate;
    try
      Clear;
      FDelimiter := AnsiChar(StringsSource.Delimiter);
      FNameValueSeparator := AnsiChar(StringsSource.NameValueSeparator);
      for I := 0 to StringsSource.Count - 1 do
        AddObject(AnsiString(StringsSource.Strings[I]), StringsSource.Objects[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJclAnsiStrings.AssignTo(Dest: TPersistent);
var
  StringsDest: TStrings;
  AnsiStringsDest: TJclAnsiStrings;
  I: Integer;
begin
  if Dest is TStrings then
  begin
    StringsDest := TStrings(Dest);
    StringsDest.BeginUpdate;
    try
      StringsDest.Clear;
      StringsDest.Delimiter := Char(Delimiter);
      StringsDest.NameValueSeparator := Char(NameValueSeparator);
      for I := 0 to Count - 1 do
        StringsDest.AddObject(string(Strings[I]), Objects[I]);
    finally
      StringsDest.EndUpdate;
    end;
  end
  else
  if Dest is TJclAnsiStrings then
  begin
    AnsiStringsDest := TJclAnsiStrings(Dest);
    BeginUpdate;
    try
      AnsiStringsDest.Clear;
      AnsiStringsDest.FNameValueSeparator := FNameValueSeparator;
      AnsiStringsDest.FDelimiter := FDelimiter;
      for I := 0 to Count - 1 do
        AnsiStringsDest.AddObject(Strings[I], Objects[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

function TJclAnsiStrings.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

procedure TJclAnsiStrings.AddStrings(Strings: TJclAnsiStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings.Strings[I]);
end;

procedure TJclAnsiStrings.Error(const Msg: string; Data: Integer);
begin
  raise EJclAnsiStringListError.CreateFmt(Msg, [Data]);
end;

procedure TJclAnsiStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

function TJclAnsiStrings.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := CompareStr(S1, S2);
end;

function TJclAnsiStrings.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareStrings(Strings[Result], S) = 0 then
      Exit;
  Result := -1;
end;

function TJclAnsiStrings.IndexOfName(const Name: AnsiString): Integer;
var
  P: Integer;
  S: AnsiString;
begin
  for Result := 0 to Count - 1 do
  begin
    S := Strings[Result];
    P := AnsiPos(NameValueSeparator, S);
    if (P > 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then
      Exit;
  end;
  Result := -1;
end;

function TJclAnsiStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if Objects[Result] = AObject then
      Exit;
  Result := -1;
end;

procedure TJclAnsiStrings.Exchange(Index1, Index2: Integer);
var
  TempString: AnsiString;
  TempObject: TObject;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TJclAnsiStrings.GetCommaText: AnsiString;
begin
  Result := GetDelimitedText(AnsiComma, AnsiDoubleQuote);
end;

function TJclAnsiStrings.GetDelimitedText: AnsiString;
begin
  Result := GetDelimitedText(Delimiter, QuoteChar);
end;

function TJclAnsiStrings.GetDelimitedText(const ADelimiter: AnsiString; AQuoteChar: AnsiChar): AnsiString;

  function Quoted(Item: AnsiString): AnsiString;
  begin
    if (not StrictDelimiter) and ((Pos(AnsiSpace, Item) > 0) or (Pos(FQuoteChar, Item) > 0)) then
    begin
      Result := AnsiQuotedStr(Item, AQuoteChar);
    end
    else
      Result := Item;
  end;

var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 2 do
    Result := Result + Quoted(Strings[I]) + ADelimiter;
  if Count > 0 then
    Result := Result + Quoted(Strings[Count - 1]);
end;

procedure TJclAnsiStrings.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

procedure TJclAnsiStrings.SetCommaText(const Value: AnsiString);
begin
  SetDelimitedText(Value, AnsiComma, AnsiDoubleQuote);
end;

procedure TJclAnsiStrings.SetDelimitedText(const Value: AnsiString);
begin
  SetDelimitedText(Value, Delimiter, QuoteChar);
end;

procedure TJclAnsiStrings.SetDelimitedText(const Value, ADelimiter: AnsiString; AQuoteChar: AnsiChar);

  procedure InternalAdd(Item: AnsiString);
  begin
    Item := StrTrimQuotes(Item, AQuoteChar);
    StrReplace(Item, AQuoteChar + AQuoteChar, AQuoteChar, [rfReplaceAll]);
    Add(Item);
  end;

var
  ValueLength, LastStart, Index, QuoteCharCount: Integer;
  ValueChar: AnsiChar;
begin
  Clear;
  LastStart := 1;
  QuoteCharCount := 0;
  ValueLength := Length(Value);
  for Index := 1 to ValueLength do
  begin
    ValueChar := Value[Index];
    if ValueChar = AQuoteChar then
      Inc(QuoteCharCount);
    if ((ValueChar = ADelimiter) or ((ValueChar = ' ') and (not StrictDelimiter)))
    and ((not Odd(QuoteCharCount) or (QuoteCharCount = 0))) then
    begin
      if StrictDelimiter then
        Add(Copy(Value, LastStart, Index - LastStart))
      else
        InternalAdd(Copy(Value, LastStart, Index - LastStart));
      QuoteCharCount := 0;
      LastStart := Index + 1;
    end;
    if (Index = ValueLength) and (LastStart < ValueLength) then
    begin
      if StrictDelimiter then
        Add(Copy(Value, LastStart, ValueLength - LastStart + 1))
      else
        InternalAdd(Copy(Value, LastStart, Index - LastStart + 1));
    end;
  end;
end;

function TJclAnsiStrings.GetText: AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 2 do
    Result := Result + Strings[I] + AnsiLineBreak;
  if Count > 0 then
    Result := Result + Strings[Count - 1] + AnsiLineBreak;
end;

procedure TJclAnsiStrings.SetText(const Value: AnsiString);
var
  Index, Start, Len: Integer;
  S: AnsiString;
begin
  Clear;
  Len := Length(Value);
  Index := 1;
  while Index <= Len do
  begin
    Start := Index;
    while (Index <= Len) and not CharIsReturn(Value[Index]) do
      Inc(Index);

    S := Copy(Value, Start, Index - Start);
    Add(S);

    if (Index <= Len) and (Value[Index] = AnsiCarriageReturn) then
      Inc(Index);
    if (Index <= Len) and (Value[Index] = AnsiLineFeed) then
      Inc(Index);
  end;
end;

function TJclAnsiStrings.GetCapacity: Integer;
begin
  Result := Count; // Might be overridden in derived classes
end;

procedure TJclAnsiStrings.SetCapacity(const Value: Integer);
begin
  // Nothing at this level
end;

procedure TJclAnsiStrings.BeginUpdate;
begin
end;

procedure TJclAnsiStrings.EndUpdate;
begin
end;

procedure TJclAnsiStrings.LoadFromFile(const FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclAnsiStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    System.SetString(S, nil, Size);
    Stream.Read(PAnsiChar(S)^, Size);
    SetText(S);
  finally
    EndUpdate;
  end;
end;

procedure TJclAnsiStrings.SaveToFile(const FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclAnsiStrings.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := GetText;
  Stream.WriteBuffer(PAnsiChar(S)^, Length(S));
end;

function TJclAnsiStrings.ExtractName(const S: AnsiString): AnsiString;
var
  P: Integer;
begin
  Result := S;
  P := AnsiPos(NameValueSeparator, Result);
  if P > 0 then
    SetLength(Result, P - 1)
  else
    SetLength(Result, 0);
end;

function TJclAnsiStrings.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Strings[Index]);
end;

function TJclAnsiStrings.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(GetString(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

procedure TJclAnsiStrings.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      I := Add('');
    SetString(I, Name + NameValueSeparator + Value);
  end
  else
  begin
    if I >= 0 then
      Delete(I);
  end;
end;

function TJclAnsiStrings.GetValueFromIndex(Index: Integer): AnsiString;
var
  S: AnsiString;
  P: Integer;
begin
  if Index >= 0 then
  begin
    S := Strings[Index];
    P := AnsiPos(NameValueSeparator, S);
    if P > 0 then
      Result := Copy(S, P + 1, Length(S) - P)
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TJclAnsiStrings.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then
      Index := Add('');
    SetString(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
  begin
    if Index >= 0 then
      Delete(Index);
  end;
end;

//=== { TJclAnsiStringList } =================================================

constructor TJclAnsiStringList.Create;
begin
  inherited Create;
  FCaseSensitive := True;
end;

procedure TJclAnsiStringList.Assign(Source: TPersistent);
var
  StringListSource: TStringList;
begin
  if Source is TStringList then
  begin
    StringListSource := TStringList(Source);
    FDuplicates := StringListSource.Duplicates;
    FSorted := StringListSource.Sorted;
    FCaseSensitive := StringListSource.CaseSensitive;
  end;
  inherited Assign(Source);
end;

procedure TJclAnsiStringList.AssignTo(Dest: TPersistent);
var
  StringListDest: TStringList;
  AnsiStringListDest: TJclAnsiStringList;
begin
  if Dest is TStringList then
  begin
    StringListDest := TStringList(Dest);
    StringListDest.Clear; // make following assignments a lot faster
    StringListDest.Duplicates := FDuplicates;
    StringListDest.Sorted := FSorted;
    StringListDest.CaseSensitive := FCaseSensitive;
  end
  else
  if Dest is TJclAnsiStringList then
  begin
    AnsiStringListDest := TJclAnsiStringList(Dest);
    AnsiStringListDest.Clear;
    AnsiStringListDest.FDuplicates := FDuplicates;
    AnsiStringListDest.FSorted := FSorted;
    AnsiStringListDest.FCaseSensitive := FCaseSensitive;
  end;
  inherited AssignTo(Dest);
end;

function TJclAnsiStringList.CompareStrings(const S1: AnsiString; const S2: AnsiString): Integer;
begin
  if FCaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

procedure TJclAnsiStringList.Grow;
var
  Delta: Integer;
begin
  if Capacity > 64 then
    Delta := Capacity div 4
  else if Capacity > 8 then
    Delta := 16
  else
    Delta := 4;

  SetCapacity(Capacity + Delta);
end;

function TJclAnsiStringList.GetString(Index: Integer): AnsiString;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  Result := FStrings[Index].Str;
end;

procedure TJclAnsiStringList.SetString(Index: Integer; const Value: AnsiString);
begin
  if Sorted then
    Error(@SSortedListError, 0);

  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  FStrings[Index].Str := Value;
end;

function TJclAnsiStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  Result := FStrings[Index].Obj;
end;

procedure TJclAnsiStringList.SetObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  FStrings[Index].Obj := AObject;
end;

function TJclAnsiStringList.GetCapacity: Integer;
begin
  Result := Length(FStrings);
end;

procedure TJclAnsiStringList.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) then
    Error(@SListCapacityError, Value);

  if Value <> Capacity then
    SetLength(FStrings, Value);
end;

function TJclAnsiStringList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TJclAnsiStringList.InsertObject(Index: Integer; const S: AnsiString; AObject: TObject);
var
  I: Integer;
begin
  if Count = Capacity then
    Grow;

  for I := Count - 1 downto Index do
    FStrings[I + 1] := FStrings[I];

  FStrings[Index].Str := S;
  FStrings[Index].Obj := AObject;
  Inc(FCount);
end;

function TJclAnsiStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then
  begin
    Result := Count;
  end
  else
  begin
    case Duplicates of
      dupAccept: ;
      dupIgnore:
        if Find(S, Result) then
          Exit;
      dupError:
        if Find(S, Result) then
          Error(@SDuplicateString, 0);
    end;
  end;

  InsertObject(Result, S, AObject);
end;

procedure TJclAnsiStringList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  for I := Index to Count - 2 do
    FStrings[I] := FStrings[I + 1];
    
  SetLength(FStrings[FCount - 1].Str, 0);  // the last string is no longer useful
    
  Dec(FCount);
end;

procedure TJclAnsiStringList.Clear;
var
  I: Integer;
begin
  FCount := 0;
  for I := 0 to Length(FStrings) - 1 do
  begin
    FStrings[I].Str := '';
    FStrings[I].Obj := nil;
  end;
end;

function TJclAnsiStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FStrings[I].Str, S);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function AnsiStringListCompareStrings(List: TJclAnsiStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FStrings[Index1].Str,
                                List.FStrings[Index2].Str);
end;

procedure TJclAnsiStringList.Sort;
begin
  CustomSort(AnsiStringListCompareStrings);
end;

procedure TJclAnsiStringList.CustomSort(Compare: TJclAnsiStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
    QuickSort(0, FCount - 1, Compare);
end;

procedure TJclAnsiStringList.QuickSort(L, R: Integer; SCompare: TJclAnsiStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do
        Inc(I);
      while SCompare(Self, J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
          Exchange(I, J);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TJclAnsiStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

{$ENDIF SUPPORTS_UNICODE}

// String Test Routines
function StrIsAlpha(const S: AnsiString): Boolean;
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

function StrIsAlphaNum(const S: AnsiString): Boolean;
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

function StrConsistsofNumberChars(const S: AnsiString): Boolean;
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

function StrContainsChars(const S: AnsiString; Chars: TSysCharSet; CheckAll: Boolean): Boolean;
var
  I: SizeInt;
  C: AnsiChar;
begin
  Result := Chars = [];
  if not Result then
  begin
    if CheckAll then
    begin
      for I := 1 to Length(S) do
      begin
        C := S[I];
        if C in Chars then
        begin
          Chars := Chars - [C];
          if Chars = [] then
            Break;
        end;
      end;
      Result := (Chars = []);
    end
    else
    begin
      for I := 1 to Length(S) do
        if S[I] in Chars then
        begin
          Result := True;
          Break;
        end;
    end;
  end;
end;

function StrIsAlphaNumUnderscore(const S: AnsiString): Boolean;
var
  I: SizeInt;
  C: AnsiChar;
begin
  for i := 1 to Length(s) do
  begin
    C := S[I];

    if not (CharIsAlphaNum(C) or (C = '_')) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True and (Length(S) > 0);
end;

function StrIsDigit(const S: AnsiString): Boolean;
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

function StrIsSubset(const S: AnsiString; const ValidChars: TSysCharSet): Boolean;
var
  I: SizeInt;
begin
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ValidChars) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True and (Length(S) > 0);
end;

function StrSame(const S1, S2: AnsiString): Boolean;
begin
  Result := StrCompare(S1, S2) = 0;
end;

//=== String Transformation Routines =========================================

function StrCenter(const S: AnsiString; L: SizeInt; C: AnsiChar = ' '): AnsiString;
begin
  if Length(S) < L then
  begin
    Result := StringOfChar(C, (L - Length(S)) div 2) + S;
    Result := Result + StringOfChar(C, L - Length(Result));
  end
  else
    Result := S;
end;

function StrCharPosLower(const S: AnsiString; CharPos: SizeInt): AnsiString;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharLower(Result[CharPos]);
end;

function StrCharPosUpper(const S: AnsiString; CharPos: SizeInt): AnsiString;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharUpper(Result[CharPos]);
end;

function StrDoubleQuote(const S: AnsiString): AnsiString;
begin
  Result := AnsiDoubleQuote + S + AnsiDoubleQuote;
end;

function StrEnsureNoPrefix(const Prefix, Text: AnsiString): AnsiString;
var
  PrefixLen: SizeInt;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Copy(Text, PrefixLen + 1, Length(Text))
  else
    Result := Text;
end;

function StrEnsureNoSuffix(const Suffix, Text: AnsiString): AnsiString;
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

function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
var
  PrefixLen: SizeInt;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Text
  else
    Result := Prefix + Text;
end;

function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
var
  SuffixLen: SizeInt;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

function StrEscapedToString(const S: AnsiString): AnsiString;
  procedure HandleHexEscapeSeq(const S: AnsiString; var I: SizeInt; Len: SizeInt; var Dest: AnsiString);
  const
    HexDigits = AnsiString('0123456789abcdefABCDEF');
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

      if Val > Ord(High(AnsiChar)) then
        raise EJclAnsiStringError.CreateResFmt(@RsNumericConstantTooLarge, [Val, StartI]);

      Dest := Dest + AnsiChar(Val);
    end;
  end;

  procedure HandleOctEscapeSeq(const S: AnsiString; var I: SizeInt; Len: SizeInt; var Dest: AnsiString);
  const
    OctDigits = AnsiString('01234567');
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

    if Val > Ord(High(AnsiChar)) then
      raise EJclAnsiStringError.CreateResFmt(@RsNumericConstantTooLarge, [Val, StartI]);

    Dest := Dest + AnsiChar(Val);
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
          Result := Result + AnsiBell;
        'b':
          Result := Result + AnsiBackspace;
        'f':
          Result := Result + AnsiFormFeed;
        'n':
          Result := Result + AnsiLineFeed;
        'r':
          Result := Result + AnsiCarriageReturn;
        't':
          Result := Result + AnsiTab;
        'v':
          Result := Result + AnsiVerticalTab;
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
            // '\x' at end of AnsiString is not escape sequence
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

function StrLower(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrLowerInPlace(Result);
end;

procedure StrLowerInPlace(var S: AnsiString);
begin
  StrCase(S, AnsiLoOffset);
end;

procedure StrLowerBuff(S: PAnsiChar);
begin
  StrCaseBuff(S, AnsiLoOffset);
end;

procedure StrMove(var Dest: AnsiString; const Source: AnsiString;
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
  Move(Source[FromIndex], Dest[ToIndex], Count);
end;

function StrPadLeft(const S: AnsiString; Len: SizeInt; C: AnsiChar): AnsiString;
var
  L: SizeInt;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else
    Result := S;
end;

function StrPadRight(const S: AnsiString; Len: SizeInt; C: AnsiChar): AnsiString;
var
  L: SizeInt;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else
    Result := S;
end;

function StrProper(const S: AnsiString): AnsiString;
begin
  Result := StrLower(S);
  if Result <> '' then
    Result[1] := UpCase(Result[1]);
end;

procedure StrProperBuff(S: PAnsiChar);
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    StrLowerBuff(S);
    S^ := CharUpper(S^);
  end;
end;

function StrQuote(const S: AnsiString; C: AnsiChar): AnsiString;
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

function StrRemoveChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  Source, Dest: PAnsiChar;
  Index, Len: SizeInt;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PAnsiChar(S);
  Dest := PAnsiChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if not (Source^ in Chars) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PAnsiChar(Result));
end;

function StrKeepChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  Source, Dest: PAnsiChar;
  Index, Len: SizeInt;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PAnsiChar(S);
  Dest := PAnsiChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if Source^ in Chars then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PAnsiChar(Result));
end;

function StrRepeat(const S: AnsiString; Count: SizeInt): AnsiString;
var
  L: SizeInt;
  P: PAnsiChar;
begin
  L := Length(S);
  SetLength(Result, Count * L);
  P := Pointer(Result);
  if P <> nil then
  begin
    while Count > 0 do
    begin
      Move(Pointer(S)^, P^, L);
      P := P + L;
      Dec(Count);
    end;
  end;
end;

function StrRepeatLength(const S: AnsiString; const L: SizeInt): AnsiString;
var
  Count: SizeInt;
  LenS: SizeInt;
  P: PAnsiChar;
begin
  Result := '';
  LenS := Length(S);

  if (LenS > 0) and (S <> '') then
  begin
    Count := L div LenS;
    if Count * LenS < L then
      Inc(Count);
    SetLength(Result, Count * LenS);
    P := Pointer(Result);
    while Count > 0 do
    begin
      Move(Pointer(S)^, P^, LenS);
      P := P + LenS;
      Dec(Count);
    end;
    if Length(S) > L then
      SetLength(Result, L);
  end;
end;

procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString; Flags: TReplaceFlags);
var
  SearchStr: AnsiString;
  ResultStr: AnsiString;     { result string }
  SourcePtr: PAnsiChar;      { pointer into S of character under examination }
  SourceMatchPtr: PAnsiChar; { pointers into S and Search when first character has }
  SearchMatchPtr: PAnsiChar; { been matched and we're probing for a complete match }
  ResultPtr: PAnsiChar;      { pointer into Result of character being written }
  ResultIndex: SizeInt;
  SearchLength: SizeInt;     { length of search string }
  ReplaceLength: SizeInt;    { length of replace string }
  BufferLength: SizeInt;     { length of temporary result buffer }
  ResultLength: SizeInt;     { length of result string }
  C: AnsiChar;               { first character of search string }
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
      raise EJclAnsiStringError.CreateRes(@RsBlankSearchString);
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
    ResultPtr := PAnsiChar(ResultStr);
    SourcePtr := PAnsiChar(S);
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
        SearchMatchPtr := PAnsiChar(SearchStr) + 1;
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
              ResultIndex := ResultPtr - PAnsiChar(ResultStr) + 1;
              SetLength(ResultStr, BufferLength);
              ResultPtr := @ResultStr[ResultIndex];
            end;
            { append replace to result and move past the search string in source }
            Move((@Replace[1])^, ResultPtr^, ReplaceLength);
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

function StrReplaceChar(const S: AnsiString; const Source, Replace: AnsiChar): AnsiString;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] = Source then
      Result[I] := Replace;
end;

function StrReplaceChars(const S: AnsiString; const Chars: TSysCharSet; Replace: AnsiChar): AnsiString;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] in Chars then
      Result[I] := Replace;
end;

function StrReplaceButChars(const S: AnsiString; const Chars: TSysCharSet;
  Replace: AnsiChar): AnsiString;
var
  I: SizeInt;
begin
  Result := S;
  for I := 1 to Length(S) do
    if not (Result[I] in Chars) then
      Result[I] := Replace;
end;

function StrReverse(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrReverseInplace(Result);
end;

procedure StrReverseInPlace(var S: AnsiString);
var
  P1, P2: PAnsiChar;
  C: AnsiChar;
begin
  UniqueString(S);
  P1 := PAnsiChar(S);
  P2 := P1 + SizeOf(AnsiChar) * (Length(S) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;

function StrSingleQuote(const S: AnsiString): AnsiString;
begin
  Result := AnsiSingleQuote + S + AnsiSingleQuote;
end;

procedure StrSkipChars(var S: PAnsiChar; const Chars: TSysCharSet);
begin
  while S^ in Chars do
    Inc(S);
end;

procedure StrSkipChars(const S: AnsiString; var Index: SizeInt; const Chars: TSysCharSet);
begin
  while S[Index] in Chars do
    Inc(Index);
end;

function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
var
  Source, Dest: PAnsiChar;
  Index, Len: SizeInt;
begin
  Result := '';
  if Delimiters = [] then
    Include(Delimiters, AnsiSpace);

  if S <> '' then
  begin
    Result := S;
    UniqueString(Result);

    Len := Length(S);
    Source := PAnsiChar(S);
    Dest := PAnsiChar(Result);
    Inc(Dest);

    for Index := 2 to Len do
    begin
      if (Source^ in Delimiters) then
        Dest^ := CharUpper(Dest^);

      Inc(Dest);
      Inc(Source);
    end;

    Result[1] := CharUpper(Result[1]);
  end;
end;

function StrStringToEscaped(const S: AnsiString): AnsiString;
var
  I: SizeInt;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      AnsiBackspace:
        Result := Result + '\b';
      AnsiBell:
        Result := Result + '\a';
      AnsiCarriageReturn:
        Result := Result + '\r';
      AnsiFormFeed:
        Result := Result + '\f';
      AnsiLineFeed:
        Result := Result + '\n';
      AnsiTab:
        Result := Result + '\t';
      AnsiVerticalTab:
        Result := Result + '\v';
      '\':
        Result := Result + '\\';
      '"':
        Result := Result + '\"';
    else
      // Characters < ' ' are escaped with hex sequence
      if S[I] < #32 then
        Result := Result + AnsiString(Format('\x%.2x', [SizeInt(S[I])]))
      else
        Result := Result + S[I];
    end;
  end;
end;

function StrStripNonNumberChars(const S: AnsiString): AnsiString;
var
  I: SizeInt;
  C: AnsiChar;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if CharIsNumberChar(C) then
      Result := Result + C;
  end;
end;

function StrToHex(const Source: AnsiString): AnsiString;
var
  Index: SizeInt;
  C, L, N: SizeInt;
  BL, BH: Byte;
  S: AnsiString;
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
      Result[N] := AnsiChar((Cardinal(BH) shl 4) or Cardinal(BL));
      Inc(N);
    end;
  end;
end;

function StrTrimCharLeft(const S: AnsiString; C: AnsiChar): AnsiString;
var
  I, L: SizeInt;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] = C) do
    Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsLeft(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  I, L: SizeInt;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] in Chars) do
    Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsRight(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  I: SizeInt;
begin
  I := Length(S);
  while (I >= 1) and (S[I] in Chars) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimCharRight(const S: AnsiString; C: AnsiChar): AnsiString;
var
  I: SizeInt;
begin
  I := Length(S);
  while (I >= 1) and (S[I] = C) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimQuotes(const S: AnsiString): AnsiString;
var
  First, Last: AnsiChar;
  L: SizeInt;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and ((First = AnsiSingleQuote) or (First = AnsiDoubleQuote)) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := S;
end;

function StrTrimQuotes(const S: AnsiString; QuoteChar: AnsiChar): AnsiString;
var
  First, Last: AnsiChar;
  L: SizeInt;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and (First = QuoteChar) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := S;
end;

function StrUpper(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrUpperInPlace(Result);
end;

procedure StrUpperInPlace(var S: AnsiString);
begin
  StrCase(S, AnsiUpOffset);
end;

procedure StrUpperBuff(S: PAnsiChar);
begin
  StrCaseBuff(S, AnsiUpOffset);
end;

{$IFDEF MSWINDOWS}
function StrOemToAnsi(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  OemToAnsiBuff(PAnsiChar(S), PAnsiChar(Result), Length(S));
end;

function StrAnsiToOem(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  AnsiToOemBuff(PAnsiChar(S), PAnsiChar(Result), Length(S));
end;
{$ENDIF MSWINDOWS}

//=== String Management ======================================================

procedure StrAddRef(var S: AnsiString);
var
  P: PAnsiStrRec;
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

procedure StrDecRef(var S: AnsiString);
var
  P: PAnsiStrRec;
begin
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    case P^.RefCount of
      -1, 0:
        { nothing } ;
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

function StrLength(const S: AnsiString): Longint;
var
  P: PAnsiStrRec;
begin
  Result := 0;
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    Result := P^.Length and (not $80000000 shr 1);
  end;
end;

function StrRefCount(const S: AnsiString): Longint;
var
  P: PAnsiStrRec;
begin
  Result := 0;
  P := Pointer(S);
  if P <> nil then
  begin
    Dec(P);
    Result := P^.RefCount;
  end;
end;

procedure StrResetLength(var S: AnsiString);
var
  I: SizeInt;
begin
  for I := 1 to Length(S) do
    if S[I] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

//=== String Search and Replace Routines =====================================

function StrCharCount(const S: AnsiString; C: AnsiChar): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

function StrCharsCount(const S: AnsiString; Chars: TSysCharSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] in Chars then
      Inc(Result);
end;

function StrStrCount(const S, SubS: AnsiString): SizeInt;
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
function StrCompareRangeEx(const S1, S2: AnsiString; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
var
  Len1, Len2: SizeInt;
  I: SizeInt;
  C1, C2: AnsiChar;
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

function StrCompare(const S1, S2: AnsiString; CaseSensitive: Boolean): SizeInt;
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

function StrCompareRange(const S1, S2: AnsiString; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
begin
  Result := StrCompareRangeEx(S1, S2, Index, Count, CaseSensitive);
end;

function StrRepeatChar(C: AnsiChar; Count: SizeInt): AnsiString;
begin
  SetLength(Result, Count);
  if Count > 0 then
    FillChar(Result[1], Count, C);
end;

function StrFind(const Substr, S: AnsiString; const Index: SizeInt): SizeInt;
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

function StrHasPrefix(const S: AnsiString; const Prefixes: array of AnsiString): Boolean;
begin
  Result := StrPrefixIndex(S, Prefixes) > -1;
end;

function StrHasSuffix(const S: AnsiString; const Suffixes: array of AnsiString): Boolean;
begin
  Result := StrSuffixIndex(S, Suffixes) > -1;
end;

function StrIHasPrefix(const S: AnsiString; const Prefixes: array of AnsiString): Boolean;
begin
  Result := StrIPrefixIndex(S, Prefixes) > -1;
end;

function StrIHasSuffix(const S: AnsiString; const Suffixes: array of AnsiString): Boolean;
begin
  Result := StrISuffixIndex(S, Suffixes) > -1;
end;

function StrIndex(const S: AnsiString; const List: array of AnsiString; CaseSensitive: Boolean): SizeInt;
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

function StrILastPos(const SubStr, S: AnsiString): SizeInt;
begin
  Result := StrLastPos(StrUpper(SubStr), StrUpper(S));
end;

function StrIPos(const SubStr, S: AnsiString): SizeInt;
begin
  Result := Pos(StrUpper(SubStr), StrUpper(S));
end;

function StrIPrefixIndex(const S: AnsiString; const Prefixes: array of AnsiString): SizeInt;
var
  I: SizeInt;
  Test: AnsiString;
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

function StrIsOneOf(const S: AnsiString; const List: array of AnsiString): Boolean;
begin
  Result := StrIndex(S, List) > -1;
end;

function StrISuffixIndex(const S: AnsiString; const Suffixes: array of AnsiString): SizeInt;
var
  I: SizeInt;
  Test: AnsiString;
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

function StrLastPos(const SubStr, S: AnsiString): SizeInt;
var
  Last, Current: PAnsiChar;
begin
  Result := 0;
  Last := nil;
  Current := PAnsiChar(S);

  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := AnsiStrPosA(PAnsiChar(Current), PAnsiChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current);
    end;
  end;
  if Last <> nil then
    Result := Abs(PAnsiChar(S) - Last) + 1;
end;

// IMPORTANT NOTE: The StrMatch function does currently not work with the Asterix (*)
// (*) acts like (?)

function StrMatch(const Substr, S: AnsiString; Index: SizeInt): SizeInt;
var
  SI, SubI, SLen, SubLen: SizeInt;
  SubC: AnsiChar;
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

function StrMatches(const Substr, S: AnsiString; const Index: SizeInt): Boolean;
var
  StringPtr: PAnsiChar;
  PatternPtr: PAnsiChar;
  StringRes: PAnsiChar;
  PatternRes: PAnsiChar;
begin
  if SubStr = '' then
    raise EJclAnsiStringError.CreateRes(@RsBlankSearchString);

  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  if (Index <= 0) or (Index > Length(S)) then
    raise EJclAnsiStringError.CreateRes(@RsArgumentOutOfRange);

  StringPtr := PAnsiChar(@S[Index]);
  PatternPtr := PAnsiChar(SubStr);
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

function StrNPos(const S, SubStr: AnsiString; N: SizeInt): SizeInt;
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

function StrNIPos(const S, SubStr: AnsiString; N: SizeInt): SizeInt;
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

function StrPrefixIndex(const S: AnsiString; const Prefixes: array of AnsiString): SizeInt;
var
  I: SizeInt;
  Test: AnsiString;
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

function StrSearch(const Substr, S: AnsiString; const Index: SizeInt): SizeInt;
var
  SP, SPI, SubP: PAnsiChar;
  SLen: SizeInt;
begin
  SLen := Length(S);
  if Index <= SLen then
  begin
    SP := PAnsiChar(S);
    SubP := PAnsiChar(Substr);
    SPI := SP;
    Inc(SPI, Index);
    Dec(SPI);
    SPI := StrPosA(SPI, SubP);
    if SPI <> nil then
      Result := SPI - SP + 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function StrSuffixIndex(const S: AnsiString; const Suffixes: array of AnsiString): SizeInt;
var
  I: SizeInt;
  Test: AnsiString;
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

function StrAfter(const SubStr, S: AnsiString): AnsiString;
var
  P: SizeInt;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: AnsiString): AnsiString;
var
  P: SizeInt;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;

function StrSplit(const SubStr, S: AnsiString;var Left, Right : AnsiString): boolean;
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

function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
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

function StrChopRight(const S: AnsiString; N: SizeInt): AnsiString;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLeft(const S: AnsiString; Count: SizeInt): AnsiString;
begin
  Result := Copy(S, 1, Count);
end;

function StrMid(const S: AnsiString; Start, Count: SizeInt): AnsiString;
begin
  Result := Copy(S, Start, Count);
end;

function StrRestOf(const S: AnsiString; N: SizeInt): AnsiString;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: AnsiString; Count: SizeInt): AnsiString;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

//=== Character (do we have it ;) ============================================

function CharEqualNoCase(const C1, C2: AnsiChar): Boolean;
begin
  // if they are not equal chars, may be same letter different case
  Result := (C1 = C2) or
    (CharIsAlpha(C1) and CharIsAlpha(C2) and (CharLower(C1) = CharLower(C2)));
end;


function CharIsAlpha(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

function CharIsAlphaNum(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or
    ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

function CharIsBlank(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_BLANK) <> 0);
end;

function CharIsControl(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_CNTRL) <> 0;
end;

function CharIsDelete(const C: AnsiChar): Boolean;
begin
  Result := (C = #8);
end;

function CharIsDigit(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
end;

function CharIsFracDigit(const C: AnsiChar): Boolean;
begin
  Result := (C = '.') or ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

function CharIsHexDigit(const C: AnsiChar): Boolean;
begin
  case C of
    'A'..'F',
    'a'..'f':
      Result := True;
  else
    Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
  end;
end;

function CharIsLower(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_LOWER) <> 0;
end;

// JclSysUtils.TJclFormatSettings.GetDecimalSeparator is manually inlined in the 2 following functions
// this fixes compiler warnings about functions not being inlined
 
function CharIsNumberChar(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or (C = AnsiSignMinus) or (C = AnsiSignPlus) or
    (Char(C) = {$IFDEF RTL220_UP}FormatSettings.DecimalSeparator{$ELSE}SysUtils.DecimalSeparator{$ENDIF});
end;

function CharIsNumber(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or
    (Char(C) = {$IFDEF RTL220_UP}FormatSettings.DecimalSeparator{$ELSE}SysUtils.DecimalSeparator{$ENDIF});
end;

function CharIsPrintable(const C: AnsiChar): Boolean;
begin
  Result := not CharIsControl(C);
end;

function CharIsPunctuation(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_PUNCT) <> 0);
end;

function CharIsReturn(const C: AnsiChar): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;

function CharIsSpace(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_SPACE) <> 0;
end;

function CharIsUpper(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_UPPER) <> 0;
end;

function CharIsValidIdentifierLetter(const C: AnsiChar): Boolean;
begin
  case C of
    '0'..'9', 'A'..'Z', 'a'..'z', '_':
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsWhiteSpace(const C: AnsiChar): Boolean;
begin
  Result := (C = AnsiTab) or (C = AnsiLineFeed) or (C = AnsiVerticalTab) or
            (C = AnsiFormFeed) or (C = AnsiCarriageReturn) or (C =AnsiSpace) or
            ((AnsiCharTypes[C] and C1_SPACE) <> 0);
end;

function CharIsWildcard(const C: AnsiChar): Boolean;
begin
  case C of
    '*', '?':
      Result := True;
  else
    Result := False;
  end;
end;

function CharType(const C: AnsiChar): Word;
begin
  Result := AnsiCharTypes[C];
end;

//=== PCharVector ============================================================

function StringsToPCharVector(var Dest: PAnsiCharVector; const Source: TJclAnsiStrings): PAnsiCharVector;
var
  I: SizeInt;
  S: AnsiString;
  List: array of PAnsiChar;
begin
  Assert(Source <> nil);
  Dest := AllocMem((Source.Count + SizeOf(AnsiChar)) * SizeOf(PAnsiChar));
  SetLength(List, Source.Count + SizeOf(AnsiChar));
  for I := 0 to Source.Count - 1 do
  begin
    S := Source[I];
    {$IFDEF SUPPORTS_UNICODE}
    List[I] := AnsiStrAlloc(Length(S) + SizeOf(AnsiChar));
    {$ELSE ~SUPPORTS_UNICODE}
    List[I] := StrAlloc(Length(S) + SizeOf(AnsiChar));
    {$ENDIF ~SUPPORTS_UNICODE}
    StrPCopyA(List[I], S);
  end;
  List[Source.Count] := nil;
  Move(List[0], Dest^, (Source.Count + 1) * SizeOf(PAnsiChar));
  Result := Dest;
end;

function PCharVectorCount(Source: PAnsiCharVector): SizeInt;
begin
  Result := 0;
  if Source <> nil then
    while Source^ <> nil do
  begin
    Inc(Source);
    Inc(Result);
  end;
end;

procedure PCharVectorToStrings(const Dest: TJclAnsiStrings; Source: PAnsiCharVector);
var
  I, Count: SizeInt;
  List: array of PAnsiChar;
begin
  Assert(Dest <> nil);
  if Source <> nil then
  begin
    Count := PCharVectorCount(Source);
    SetLength(List, Count);
    Move(Source^, List[0], Count * SizeOf(PAnsiChar));
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

procedure FreePCharVector(var Dest: PAnsiCharVector);
var
  I, Count: SizeInt;
  List: array of PAnsiChar;
begin
  if Dest <> nil then
  begin
    Count := PCharVectorCount(Dest);
    SetLength(List, Count);
    Move(Dest^, List[0], Count * SizeOf(PAnsiChar));
    for I := 0 to Count - 1 do
      StrDisposeA(List[I]);
    FreeMem(Dest, (Count + 1) * SizeOf(PAnsiChar));
    Dest := nil;
  end;
end;

//=== Character Transformation Routines ======================================

function CharHex(const C: AnsiChar): Byte;
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

function CharLower(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiLoOffset];
end;

function CharToggleCase(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiReOffset];
end;

function CharUpper(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiUpOffset];
end;

//=== Character Search and Replace ===========================================

function CharLastPos(const S: AnsiString; const C: AnsiChar; const Index: SizeInt): SizeInt;
begin
  if (Index > 0) and (Index <= Length(S)) then
    for Result := Length(S) downto Index do
      if S[Result] = C then
        Exit;
  Result := 0;
end;

function CharPos(const S: AnsiString; const C: AnsiChar; const Index: SizeInt): SizeInt;
begin
  if (Index > 0) and (Index <= Length(S)) then
    for Result := Index to Length(S) do
      if S[Result] = C then
        Exit;
  Result := 0;
end;

function CharIPos(const S: AnsiString; C: AnsiChar; const Index: SizeInt): SizeInt;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    C := CharUpper(C);
    for Result := Index to Length(S) do
      if AnsiCaseMap[Ord(S[Result]) + AnsiUpOffset] = C then
        Exit;
  end;
  Result := 0;
end;

function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): SizeInt;
var
  P: PAnsiChar;
  Index, Len: SizeInt;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    Len := Length(S);
    P := PAnsiChar(S);
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

function StringsToMultiSz(var Dest: PAnsiMultiSz; const Source: TJclAnsiStrings): PAnsiMultiSz;
var
  I, TotalLength: SizeInt;
  P: PAnsiMultiSz;
begin
  Assert(Source <> nil);
  TotalLength := 1;
  for I := 0 to Source.Count - 1 do
    if Source[I] = '' then
      raise EJclAnsiStringError.CreateRes(@RsInvalidEmptyStringItem)
    else
      Inc(TotalLength, StrLenA(PAnsiChar(AnsiString(Source[I]))) + 1);
  AllocateMultiSz(Dest, TotalLength);
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopyA(P, PAnsiChar(AnsiString(Source[I])));
    Inc(P);
  end;
  P^ := #0;
  Result := Dest;
end;

procedure MultiSzToStrings(const Dest: TJclAnsiStrings; const Source: PAnsiMultiSz);
var
  P: PAnsiMultiSz;
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
        P := StrEndA(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

function MultiSzLength(const Source: PAnsiMultiSz): SizeInt;
var
  P: PAnsiMultiSz;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source;
    repeat
      Inc(Result, StrLenA(P) + 1);
      P := StrEndA(P);
      Inc(P);
    until P^ = #0;
    Inc(Result);
  end;
end;

procedure AllocateMultiSz(var Dest: PAnsiMultiSz; Len: SizeInt);
begin
  if Len > 0 then
    GetMem(Dest, Len * SizeOf(AnsiChar))
  else
    Dest := nil;
end;

procedure FreeMultiSz(var Dest: PAnsiMultiSz);
begin
  if Dest <> nil then
    FreeMem(Dest);
  Dest := nil;
end;

function MultiSzDup(const Source: PAnsiMultiSz): PAnsiMultiSz;
var
  Len: SizeInt;
begin
  if Source <> nil then
  begin
    Len := MultiSzLength(Source);
    Result := nil;
    AllocateMultiSz(Result, Len);
    Move(Source^, Result^, Len * SizeOf(AnsiChar));
  end
  else
    Result := nil;
end;

//=== TJclAnsiStrings Manipulation ===============================================

procedure StrToStrings(S, Sep: AnsiString; const List: TJclAnsiStrings; const AllowEmptyString: Boolean = True);
var
  I, L: SizeInt;
  Left: AnsiString;
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

procedure StrIToStrings(S, Sep: AnsiString; const List: TJclAnsiStrings; const AllowEmptyString: Boolean = True);
var
  I, L: SizeInt;
  LowerCaseStr: AnsiString;
  Left: AnsiString;
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

function StringsToStr(const List: TJclAnsiStrings; const Sep: AnsiString;
  const AllowEmptyString: Boolean): AnsiString;
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
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

procedure TrimStrings(const List: TJclAnsiStrings; DeleteIfEmpty: Boolean);
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

procedure TrimStringsRight(const List: TJclAnsiStrings; DeleteIfEmpty: Boolean);
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

procedure TrimStringsLeft(const List: TJclAnsiStrings; DeleteIfEmpty: Boolean);
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

function AddStringToStrings(const S: AnsiString; Strings: TJclAnsiStrings; const Unique: Boolean): Boolean;
begin
  Assert(Strings <> nil);
  Result := Unique and (Strings.IndexOf(S) <> -1);
  if not Result then
    Result := Strings.Add(S) > -1;
end;

//=== Miscellaneous ==========================================================

function FileToString(const FileName: TFileName): AnsiString;
var
  FS: TFileStream;
  Len: SizeInt;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Len := FS.Size;
    SetLength(Result, Len);
    if Len > 0 then
    FS.ReadBuffer(Result[1], Len);
  finally
    FS.Free;
  end;
end;

procedure StringToFile(const FileName: TFileName; const Contents: AnsiString; Append: Boolean);
var
  FS: TFileStream;
  Len: SizeInt;
begin
  if Append and FileExists(FileName) then
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

function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
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

procedure StrTokens(const S: AnsiString; const List: TJclAnsiStrings);
var
  Start: PAnsiChar;
  Token: AnsiString;
  Done: Boolean;
begin
  Assert(List <> nil);
  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    Start := Pointer(S);
    repeat
      Done := StrWord(Start, Token);
      if Token <> '' then
        List.Add(Token);
    until Done;
  finally
    List.EndUpdate;
  end;
end;

procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; const List: TJclAnsiStrings);
var
  Token: AnsiString;
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

function StrWord(const S: AnsiString; var Index: SizeInt; out Word: AnsiString): Boolean;
var
  Start: SizeInt;
  C: AnsiChar;
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
      AnsiSpace, AnsiLineFeed, AnsiCarriageReturn:
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

function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean;
var
  Start: PAnsiChar;
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
      AnsiSpace, AnsiLineFeed, AnsiCarriageReturn:
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

function StrIdent(const S: AnsiString; var Index: SizeInt; out Ident: AnsiString): Boolean;
var
  Start: SizeInt;
  C: AnsiChar;
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

function StrIdent(var S: PAnsiChar; out Ident: AnsiString): Boolean;
var
  Start: PAnsiChar;
  C: AnsiChar;
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

function StrToFloatSafe(const S: AnsiString): Float;
var
  Temp: AnsiString;
  I, J, K: SizeInt;
  SwapSeparators, IsNegative: Boolean;
  DecSep: AnsiChar;
  ThouSep: AnsiChar;
begin
  DecSep := AnsiChar(JclFormatSettings.DecimalSeparator);
  ThouSep := AnsiChar(JclFormatSettings.ThousandSeparator);
  Temp := S;
  SwapSeparators := False;

  IsNegative := False;
  J := 0;
  for I := 1 to Length(Temp) do
  begin
    if Temp[I] = '-' then
      IsNegative := not IsNegative
    else
    if not (Temp[I] in [' ', '(', '+']) then
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

  Temp := StrKeepChars(Temp, AnsiDecDigits + [DecSep]);

  if Length(Temp) > 0 then
  begin
    if Temp[1] = DecSep then
      Temp := '0' + Temp;
    if Temp[Length(Temp)] = DecSep then
      Temp := Temp + '0';
    Result := StrToFloat(string(Temp));
    if IsNegative then
      Result := -Result;
  end
  else
    Result := 0.0;
end;

function StrToIntSafe(const S: AnsiString): Integer;
begin
  Result := Trunc(StrToFloatSafe(S));
end;

procedure StrNormIndex(const StrLen: SizeInt; var Index: SizeInt; var Count: SizeInt); overload;
begin
  Index := Max(1, Min(Index, StrLen + 1));
  Count := Max(0, Min(Count, StrLen + 1 - Index));
end;

function ArrayOf(List: TJclAnsiStrings): TDynStringArray;
var
  I: SizeInt;
begin
  if List <> nil then
  begin
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := string(List[I]);
  end
  else
    Result := nil;
end;

function AnsiCompareNatural(const S1, S2: AnsiString; CaseInsensitive: Boolean): SizeInt;
var
  Cur1, Len1,
  Cur2, Len2: SizeInt;

  procedure NumberCompare;
  var
    IsReallyNumber: Boolean;
    FirstDiffBreaks: Boolean;
    Val1, Val2: SizeInt;
  begin
    Result := 0;
    IsReallyNumber := False;
    // count leading spaces in S1
    while CharIsWhiteSpace(S1[Cur1]) do
    begin
      Dec(Result);
      Inc(Cur1);
    end;
    // count leading spaces in S2 (canceling them out against the ones in S1)
    while CharIsWhiteSpace(S2[Cur2]) do
    begin
      Inc(Result);
      Inc(Cur2);
    end;

    // if spaces match, or both strings are actually followed by a numeric character, continue the checks
    if (Result = 0) or (CharIsNumberChar(S1[Cur1])) and (CharIsNumberChar(S2[Cur2])) then
    begin
      // Check signed number
      if (S1[Cur1] = '-') and (S2[Cur2] <> '-') then
        Result := 1
      else
      if (S2[Cur2] = '-') and (S1[Cur1] <> '-') then
        Result := -1
      else
        Result := 0;

      if (S1[Cur1] = '-') or (S1[Cur1] = '+') then
        Inc(Cur1);
      if (S2[Cur2] = '-') or (S2[Cur2] = '+') then
        Inc(Cur2);

      FirstDiffBreaks := (S1[Cur1] = '0') or (S2[Cur2] = '0');
      while CharIsDigit(S1[Cur1]) and CharIsDigit(S2[Cur2]) do
      begin
        IsReallyNumber := True;
        Val1 := StrToInt(string(S1[Cur1]));
        Val2 := StrToInt(string(S2[Cur2]));

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
          if CharIsDigit(S1[Cur1]) then
            Result := 1
          else
          if CharIsDigit(S2[Cur2]) then
            Result := -1;
        end;
      end;
    end;
  end;

begin
  Cur1 := 1;
  Len1 := Length(S1);
  Cur2 := 1;
  Len2 := Length(S2);
  Result := 0;

  while (Result = 0) do
  begin
    if (Cur1 = Len1) and (Cur2 = Len2) then
      Break
    else
    if (S1[Cur1] = '-') and CharIsNumberChar(S2[Cur2]) and (S2[Cur2] <> '-') then
      Result := -1
    else
    if (S2[Cur2] = '-') and CharIsNumberChar(S1[Cur1]) and (S1[Cur1] <> '-') then
      Result := 1
    else
    if CharIsNumberChar(S1[Cur1]) and CharIsNumberChar(S2[Cur2]) then
      NumberCompare
    else
    if (Cur1 = Len1) and (Cur2 < Len2) then
      Result := -1
    else
    if (Cur1 < Len1) and (Cur2 = Len2) then
      Result := 1
    else
    begin
      Result := StrCompare(S1,S2);
      if CaseInsensitive then
        Result := AnsiStrLICompA(PAnsiChar(@S1[Cur1]), PAnsiChar(@S2[Cur2]), 1)
      else
        Result := AnsiStrLCompA(PAnsiChar(@S1[Cur1]), PAnsiChar(@S2[Cur2]), 1);
      Inc(Cur1);
      Inc(Cur2);
    end;
  end;
end;

function AnsiCompareNaturalStr(const S1, S2: AnsiString): SizeInt; overload;
begin
  Result := AnsiCompareNatural(S1, S2, False);
end;

function AnsiCompareNaturalText(const S1, S2: AnsiString): SizeInt; overload;
begin
  Result := AnsiCompareNatural(S1, S2, True);
end;


function StrNewA(const Str: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrNew(Str);
end;

procedure StrDisposeA(Str: PAnsiChar);
begin
  {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrDispose(Str);
end;

function StrLenA(S: PAnsiChar): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrLen(S);
end;

function StrEndA(const Str: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrEnd(Str);
end;

function StrPosA(const Str1, Str2: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrPos(Str1, Str2);
end;

function StrPasA(const Str: PAnsiChar): AnsiString;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrPas(Str);
end;

function StrCopyA(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrCopy(Dest, Source);
end;

function StrLCopyA(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrLCopy(Dest, Source, MaxLen);
end;

function StrPCopyA(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrPCopy(Dest, Source);
end;

function StrPLCopyA(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrPLCopy(Dest, Source, MaxLen);
end;

function StrECopyA(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrECopy(Dest, Source);
end;

function StrCatA(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrCat(Dest, Source);
end;

function StrLCatA(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrLCat(Dest, Source, MaxLen);
end;

function StrCompA(const Str1, Str2: PAnsiChar): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrComp(Str1, Str2);
end;

function StrLCompA(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrLComp(Str1, Str2, MaxLen);
end;

function StrICompA(const Str1, Str2: PAnsiChar): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrIComp(Str1, Str2);
end;

function StrLICompA(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrIComp(Str1, Str2);
end;

function StrFmtA(Buffer, Format: PAnsiChar; const Args: array of const): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrFmt(Buffer, Format, Args);
end;

function AnsiStrPosA(const Str1, Str2: PAnsiChar): PAnsiChar;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}AnsiStrPos(Str1, Str2);
end;

function AnsiStrLICompA(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}AnsiStrLIComp(S1, S2, MaxLen);
end;

function AnsiStrLCompA(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result := {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}AnsiStrLComp(S1, S2, MaxLen);
end;


initialization
  LoadCharTypes;  // this table first
  LoadCaseMap;    // or this function does not work
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
