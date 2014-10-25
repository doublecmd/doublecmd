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
{ The Original Code is JclBase.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright Marcel van Brakel. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel,                                                                             }
{   Peter Friese,                                                                                  }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL base classes and routines to support earlier                      }
{ versions of Delphi as well as FPC.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclBase;

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
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

// Version
const
  JclVersionMajor   = 2;    // 0=pre-release|beta/1, 2, ...=final
  JclVersionMinor   = 6;    // Fifth minor release since JCL 1.90
  JclVersionRelease = 0;    // 0: pre-release|beta/ 1: release
  JclVersionBuild   = 5178; // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
    (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

// EJclError
type
  EJclError = class(Exception);

// EJclInternalError
type
  EJclInternalError = class(EJclError);

// Types
type
  {$IFDEF MATH_EXTENDED_PRECISION}
  Float = Extended;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  Float = Double;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  Float = Single;
  {$ENDIF MATH_SINGLE_PRECISION}

  PFloat = ^Float;

type
  {$IFDEF FPC}
  Largeint = Int64;
  {$ELSE ~FPC}
  {$IFDEF CPU32}
  SizeInt = Integer;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  SizeInt = NativeInt;
  {$ENDIF CPU64}
  PSizeInt = ^SizeInt;
  PPointer = ^Pointer;
  PByte = System.PByte;
  Int8 = ShortInt;
  Int16 = Smallint;
  Int32 = Integer;
  UInt8 = Byte;
  UInt16 = Word;
  UInt32 = LongWord;
  PCardinal = ^Cardinal;
  {$IFNDEF COMPILER7_UP}
  UInt64 = Int64;
  {$ENDIF ~COMPILER7_UP}
  PWideChar = System.PWideChar;
  PPWideChar = ^JclBase.PWideChar;
  PPAnsiChar = ^PAnsiChar;
  PInt64 = type System.PInt64;
  {$ENDIF ~FPC}
  PPInt64 = ^PInt64;
  PPPAnsiChar = ^PPAnsiChar;

// Int64 support
procedure I64ToCardinals(I: Int64; out LowPart, HighPart: Cardinal);
procedure CardinalsToI64(out I: Int64; const LowPart, HighPart: Cardinal);

// Redefinition of TLargeInteger to relieve dependency on Windows.pas

{$IFNDEF FPC}
type
  PLargeInteger = ^TLargeInteger;
  TLargeInteger = Int64;
{$ENDIF ~FPC}

{$IFNDEF COMPILER11_UP}
type
  TBytes = array of Byte;
{$ENDIF ~COMPILER11_UP}

// Redefinition of PByteArray to avoid range check exceptions.
type
  TJclByteArray = array [0..MaxInt div SizeOf(Byte) - 1] of Byte;
  PJclByteArray = ^TJclByteArray;
  TJclBytes = Pointer; // under .NET System.pas: TBytes = array of Byte;

// Redefinition of ULARGE_INTEGER to relieve dependency on Windows.pas
type
  {$IFNDEF FPC}
  PULARGE_INTEGER = ^ULARGE_INTEGER;
  {$EXTERNALSYM PULARGE_INTEGER}
  ULARGE_INTEGER = record
    case Integer of
    0:
     (LowPart: LongWord;
      HighPart: LongWord);
    1:
     (QuadPart: Int64);
  end;
  {$EXTERNALSYM ULARGE_INTEGER}
  {$ENDIF ~FPC}
  TJclULargeInteger = ULARGE_INTEGER;
  PJclULargeInteger = PULARGE_INTEGER;

// Dynamic Array support
type
  TDynByteArray          = array of Byte;
  TDynShortIntArray      = array of Shortint;
  TDynWordArray          = array of Word;
  TDynSmallIntArray      = array of Smallint;
  TDynLongIntArray       = array of Longint;
  TDynInt64Array         = array of Int64;
  TDynCardinalArray      = array of Cardinal;
  TDynIntegerArray       = array of Integer;
  TDynSizeIntArray       = array of SizeInt;
  TDynExtendedArray      = array of Extended;
  TDynDoubleArray        = array of Double;
  TDynSingleArray        = array of Single;
  TDynFloatArray         = array of Float;
  TDynPointerArray       = array of Pointer;
  TDynStringArray        = array of string;
  TDynAnsiStringArray    = array of AnsiString;
  TDynWideStringArray    = array of WideString;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TDynUnicodeStringArray = array of UnicodeString;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  TDynIInterfaceArray    = array of IInterface;
  TDynObjectArray        = array of TObject;
  TDynCharArray       = array of Char;
  TDynAnsiCharArray   = array of AnsiChar;
  TDynWideCharArray   = array of WideChar;

// Cross-Platform Compatibility
const
  // line delimiters for a version of Delphi/C++Builder
  NativeLineFeed       = Char(#10);
  NativeCarriageReturn = Char(#13);
  NativeCrLf           = string(#13#10);
  // default line break for a version of Delphi on a platform
  {$IFDEF MSWINDOWS}
  NativeLineBreak      = NativeCrLf;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  NativeLineBreak      = NativeLineFeed;
  {$ENDIF UNIX}

  HexPrefixPascal = string('$');
  HexPrefixC      = string('0x');
  HexDigitFmt32   = string('%.8x');
  HexDigitFmt64   = string('%.16x');

  {$IFDEF BCB}
  HexPrefix = HexPrefixC;
  {$ELSE ~BCB}
  HexPrefix = HexPrefixPascal;
  {$ENDIF ~BCB}

  {$IFDEF CPU32}
  HexDigitFmt = HexDigitFmt32;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  HexDigitFmt = HexDigitFmt64;
  {$ENDIF CPU64}

  HexFmt = HexPrefix + HexDigitFmt;

const
  BOM_UTF16_LSB: array [0..1] of Byte = ($FF,$FE);
  BOM_UTF16_MSB: array [0..1] of Byte = ($FE,$FF);
  BOM_UTF8: array [0..2] of Byte = ($EF,$BB,$BF);
  BOM_UTF32_LSB: array [0..3] of Byte = ($FF,$FE,$00,$00);
  BOM_UTF32_MSB: array [0..3] of Byte = ($00,$00,$FE,$FF);
//  BOM_UTF7_1: array [0..3] of Byte = ($2B,$2F,$76,$38);
//  BOM_UTF7_2: array [0..3] of Byte = ($2B,$2F,$76,$39);
//  BOM_UTF7_3: array [0..3] of Byte = ($2B,$2F,$76,$2B);
//  BOM_UTF7_4: array [0..3] of Byte = ($2B,$2F,$76,$2F);
//  BOM_UTF7_5: array [0..3] of Byte = ($2B,$2F,$76,$38,$2D);

type
  // Unicode transformation formats (UTF) data types
  PUTF7 = ^UTF7;
  UTF7 = AnsiChar;
  PUTF8 = ^UTF8;
  UTF8 = AnsiChar;
  PUTF16 = ^UTF16;
  UTF16 = WideChar;
  PUTF32 = ^UTF32;
  UTF32 = Cardinal;

  // UTF conversion schemes (UCS) data types
  PUCS4 = ^UCS4;
  UCS4 = Cardinal;
  PUCS2 = PWideChar;
  UCS2 = WideChar;

  TUCS2Array = array of UCS2;
  TUCS4Array = array of UCS4;

  // string types
  TUTF8String = AnsiString;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  TUTF16String = UnicodeString;
  TUCS2String = UnicodeString;
  {$ELSE}
  TUTF16String = WideString;
  TUCS2String = WideString;
  {$ENDIF SUPPORTS_UNICODE_STRING}

var
  AnsiReplacementCharacter: AnsiChar;

const
  UCS4ReplacementCharacter: UCS4 = $0000FFFD;
  MaximumUCS2: UCS4 = $0000FFFF;
  MaximumUTF16: UCS4 = $0010FFFF;
  MaximumUCS4: UCS4 = $7FFFFFFF;

  SurrogateHighStart = UCS4($D800);
  SurrogateHighEnd = UCS4($DBFF);
  SurrogateLowStart = UCS4($DC00);
  SurrogateLowEnd = UCS4($DFFF);

// basic set types
type
  TSetOfAnsiChar = set of AnsiChar;

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
{$ENDIF ~XPLATFORM_RTL}

{$IFNDEF RTL230_UP}
procedure CheckOSError(ErrorCode: Cardinal);
{$ENDIF RTL230_UP}

procedure MoveChar(const Source: string; FromIndex: SizeInt;
  var Dest: string; ToIndex, Count: SizeInt); overload; // Index: 0..n-1

function AnsiByteArrayStringLen(Data: TBytes): SizeInt;
function StringToAnsiByteArray(const S: string): TBytes;
function AnsiByteArrayToString(const Data: TBytes; Count: SizeInt): string;

function BytesOf(const Value: AnsiString): TBytes; overload;
function BytesOf(const Value: WideString): TBytes; overload;
function BytesOf(const Value: WideChar): TBytes; overload;
function BytesOf(const Value: AnsiChar): TBytes; overload;
function StringOf(const Bytes: array of Byte): AnsiString; overload;
function StringOf(const Bytes: Pointer; Size: Cardinal): AnsiString; overload;

{$IFNDEF FPC}
{$IFNDEF COMPILER11_UP}
type // Definitions for 32 Bit Compilers
  // From BaseTsd.h
  INT_PTR = Integer;
  {$EXTERNALSYM INT_PTR}
  LONG_PTR = Longint;
  {$EXTERNALSYM LONG_PTR}
  UINT_PTR = Cardinal;
  {$EXTERNALSYM UINT_PTR}
  ULONG_PTR = LongWord;
  {$EXTERNALSYM ULONG_PTR}
  DWORD_PTR = ULONG_PTR;
  {$EXTERNALSYM DWORD_PTR}
{$ENDIF ~COMPILER11_UP}

type
  PDWORD_PTR = ^DWORD_PTR;
  {$EXTERNALSYM PDWORD_PTR}
{$ENDIF ~FPC}

type
  TJclAddr32 = Cardinal;
  {$IFDEF FPC}
  TJclAddr64 = QWord;
  {$IFDEF CPU64}
  TJclAddr = QWord;
  {$ENDIF CPU64}
  {$IFDEF CPU32}
  TJclAddr = Cardinal;
  {$ENDIF CPU32}
  {$ENDIF FPC}
  {$IFDEF BORLAND}
  TJclAddr64 = Int64;
  {$IFDEF CPU64}
  TJclAddr = TJclAddr64;
  {$ENDIF CPU64}
  {$IFDEF CPU32}
  TJclAddr = TJclAddr32;
  {$ENDIF CPU32}
  {$ENDIF BORLAND}
  PJclAddr = ^TJclAddr;

  EJclAddr64Exception = class(EJclError);

function Addr64ToAddr32(const Value: TJclAddr64): TJclAddr32;
function Addr32ToAddr64(const Value: TJclAddr32): TJclAddr64;

{$IFDEF FPC}
type
  HWND = type Windows.HWND;
{$ENDIF FPC}

 {$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

type
  TCompare<T> = function(const Obj1, Obj2: T): Integer;
  TEqualityCompare<T> = function(const Obj1, Obj2: T): Boolean;
  THashConvert<T> = function(const AItem: T): Integer;

  IEqualityComparer<T> = interface
    function Equals(A, B: T): Boolean;
    function GetHashCode(Obj: T): Integer;
  end;

  TEquatable<T: class> = class(TInterfacedObject, IEquatable<T>, IEqualityComparer<T>)
  public
    { IEquatable<T> }
    function TestEquals(Other: T): Boolean; overload;
    function IEquatable<T>.Equals = TestEquals;
    { IEqualityComparer<T> }
    function TestEquals(A, B: T): Boolean; overload;
    function IEqualityComparer<T>.Equals = TestEquals;
    function GetHashCode2(Obj: T): Integer;
    function IEqualityComparer<T>.GetHashCode = GetHashCode2;
  end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

const
  {$IFDEF SUPPORTS_UNICODE}
  AWSuffix = 'W';
  {$ELSE ~SUPPORTS_UNICODE}
  AWSuffix = 'A';
  {$ENDIF ~SUPPORTS_UNICODE}

{$IFDEF FPC}
// FPC emits a lot of warning because the first parameter of its internal
// GetMem is a var parameter, which is not initialized before the call to GetMem
procedure GetMem(out P; Size: Longint);
{$ENDIF FPC}

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
  JclResources;

procedure MoveChar(const Source: string; FromIndex: SizeInt;
  var Dest: string; ToIndex, Count: SizeInt);
begin
  Move(Source[FromIndex + 1], Dest[ToIndex + 1], Count * SizeOf(Char));
end;

function AnsiByteArrayStringLen(Data: TBytes): SizeInt;
var
  I: SizeInt;
begin
  Result := Length(Data);
  for I := 0 to Result - 1 do
    if Data[I] = 0 then
    begin
      Result := I + 1;
      Break;
    end;
end;

function StringToAnsiByteArray(const S: string): TBytes;
var
  I: SizeInt;
  AnsiS: AnsiString;
begin
  AnsiS := AnsiString(S); // convert to AnsiString
  SetLength(Result, Length(AnsiS));
  for I := 0 to High(Result) do
    Result[I] := Byte(AnsiS[I + 1]);
end;

function AnsiByteArrayToString(const Data: TBytes; Count: SizeInt): string;
var
  I: SizeInt;
  AnsiS: AnsiString;
begin
  if Length(Data) < Count then
    Count := Length(Data);
  SetLength(AnsiS, Count);
  for I := 0 to Length(AnsiS) - 1 do
    AnsiS[I + 1] := AnsiChar(Data[I]);
  Result := string(AnsiS); // convert to System.String
end;

function BytesOf(const Value: AnsiString): TBytes;
begin
  SetLength(Result, Length(Value));
  if Value <> '' then
    Move(Pointer(Value)^, Result[0], Length(Value));
end;

function BytesOf(const Value: WideString): TBytes;
begin
  if Value <> '' then
    Result := JclBase.BytesOf(AnsiString(Value))
  else
    SetLength(Result, 0);
end;

function BytesOf(const Value: WideChar): TBytes;
begin
  Result := JclBase.BytesOf(WideString(Value));
end;

function BytesOf(const Value: AnsiChar): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := Byte(Value);
end;

function StringOf(const Bytes: array of Byte): AnsiString;
begin
  if Length(Bytes) > 0 then
  begin
    SetLength(Result, Length(Bytes));
    Move(Bytes[0], Pointer(Result)^, Length(Bytes));
  end
  else
    Result := '';
end;

function StringOf(const Bytes: Pointer; Size: Cardinal): AnsiString;
begin
  if (Bytes <> nil) and (Size > 0) then
  begin
    SetLength(Result, Size);
    Move(Bytes^, Pointer(Result)^, Size);
  end
  else
    Result := '';
end;

// Int64 support

procedure I64ToCardinals(I: Int64; out LowPart, HighPart: Cardinal);
begin
  LowPart := TJclULargeInteger(I).LowPart;
  HighPart := TJclULargeInteger(I).HighPart;
end;

procedure CardinalsToI64(out I: Int64; const LowPart, HighPart: Cardinal);
begin
  TJclULargeInteger(I).LowPart := LowPart;
  TJclULargeInteger(I).HighPart := HighPart;
end;

// Cross Platform Compatibility

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF ~XPLATFORM_RTL}

{$IFNDEF RTL230_UP}
procedure CheckOSError(ErrorCode: Cardinal);
begin
  if ErrorCode <> ERROR_SUCCESS then
    {$IFDEF RTL170_UP}
    RaiseLastOSError(ErrorCode);
    {$ELSE ~RTL170_UP}
    RaiseLastOSError;
    {$ENDIF ~RTL170_UP}
end;
{$ENDIF RTL230_UP}

{$OVERFLOWCHECKS OFF}

function Addr64ToAddr32(const Value: TJclAddr64): TJclAddr32;
begin
  if (Value shr 32) = 0 then
    Result := Value
  else
    raise EJclAddr64Exception.CreateResFmt(@RsCantConvertAddr64, [HexPrefix, Value]);
end;

function Addr32ToAddr64(const Value: TJclAddr32): TJclAddr64;
begin
  Result := Value;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TEquatable<T> } ======================================================

function TEquatable<T>.TestEquals(Other: T): Boolean;
begin
  if Other = nil then
    Result := False
  else
    Result := GetHashCode = Other.GetHashCode;
end;

function TEquatable<T>.TestEquals(A, B: T): Boolean;
begin
  if A = nil then
    Result := B = nil
  else
  if B = nil then
    Result := False
  else
    Result := A.GetHashCode = B.GetHashCode;
end;

function TEquatable<T>.GetHashCode2(Obj: T): Integer;
begin
  if Obj = nil then
    Result := 0
  else
    Result := Obj.GetHashCode;
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

procedure LoadAnsiReplacementCharacter;
{$IFDEF MSWINDOWS}
var
  CpInfo: TCpInfo;
begin
  CpInfo.MaxCharSize := 0;
  if GetCPInfo(CP_ACP, CpInfo) then
    AnsiReplacementCharacter := AnsiChar(Chr(CpInfo.DefaultChar[0]))
  else
    raise EJclInternalError.CreateRes(@RsEReplacementChar);
end;
{$ELSE ~MSWINDOWS}
begin
  AnsiReplacementCharacter := '?';
end;
{$ENDIF ~MSWINDOWS}

{$IFDEF FPC}
// FPC emits a lot of warning because the first parameter of its internal
// GetMem is a var parameter, which is not initialized before the call to GetMem
procedure GetMem(out P; Size: Longint);
begin
  Pointer(P) := nil;
  GetMem(Pointer(P), Size);
end;
{$ENDIF FPC}

initialization

  LoadAnsiReplacementCharacter;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
