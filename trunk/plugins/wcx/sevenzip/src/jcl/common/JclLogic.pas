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
{ The Original Code is JclLogic.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel Bestebroer (marcelb)                                                                    }
{   Marcel van Brakel                                                                              }
{   ESB Consultancy                                                                                }
{   Martin Kimmings                                                                                }
{   Robert Marquardt (marquardt)                                                                   }
{   Chris Morris                                                                                   }
{   Andreas Schmidt shmia at bizerba.de                                                            }
{   Michael Schnell                                                                                }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various routines to perform various arithmetic and logical operations on one or more ordinal     }
{ values (integer numbers). This includes various bit manipulation routines, min/max testing and   }
{ conversion to string.                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

{.$DEFINE PUREPASCAL}

unit JclLogic;

{$I jcl.inc}
{$RANGECHECKS OFF}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

// Conversion
function OrdToBinary(Value: Byte): string; overload;
function OrdToBinary(Value: ShortInt): string; overload;
function OrdToBinary(Value: SmallInt): string; overload;
function OrdToBinary(Value: Word): string; overload;
function OrdToBinary(Value: Integer): string; overload;
function OrdToBinary(Value: Cardinal): string; overload;
function OrdToBinary(Value: Int64): string; overload;

// Bit manipulation
type
  TBitRange = Byte;
  TBooleanArray = array of Boolean;

function BitsHighest(X: Byte): Integer; overload;
function BitsHighest(X: ShortInt): Integer; overload;
function BitsHighest(X: SmallInt): Integer; overload;
function BitsHighest(X: Word): Integer; overload;
function BitsHighest(X: Integer): Integer; overload;
function BitsHighest(X: Cardinal): Integer; overload;
function BitsHighest(X: Int64): Integer; overload;

function BitsLowest(X: Byte): Integer; overload;
function BitsLowest(X: Shortint): Integer; overload;
function BitsLowest(X: Smallint): Integer; overload;
function BitsLowest(X: Word): Integer; overload;
function BitsLowest(X: Cardinal): Integer; overload;
function BitsLowest(X: Integer): Integer; overload;
function BitsLowest(X: Int64): Integer; overload;

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ClearBit(const Value: Shortint; const Bit: TBitRange): Shortint; overload;
function ClearBit(const Value: Smallint; const Bit: TBitRange): Smallint; overload;
function ClearBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ClearBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
function ClearBit(const Value: Cardinal; const Bit: TBitRange): Cardinal; overload;
function ClearBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
procedure ClearBitBuffer(var Value; const Bit: Cardinal); overload;
{$IFDEF CPU64}
procedure ClearBitBuffer(var Value; const Bit: Int64); overload;
{$ENDIF CPU64}

function CountBitsSet(X: Byte): Integer; overload;
function CountBitsSet(X: Word): Integer; overload;
function CountBitsSet(X: Smallint): Integer; overload;
function CountBitsSet(X: ShortInt): Integer; overload;
function CountBitsSet(X: Integer): Integer; overload;
function CountBitsSet(X: Cardinal): Integer; overload;
function CountBitsSet(X: Int64): Integer; overload;
function CountBitsSet(P: Pointer; Count: Cardinal): Cardinal; overload;

function CountBitsCleared(X: Byte): Integer; overload;
function CountBitsCleared(X: Shortint): Integer; overload;
function CountBitsCleared(X: Smallint): Integer; overload;
function CountBitsCleared(X: Word): Integer; overload;
function CountBitsCleared(X: Integer): Integer; overload;
function CountBitsCleared(X: Cardinal): Integer; overload;
function CountBitsCleared(X: Int64): Integer; overload;
function CountBitsCleared(P: Pointer; Count: Cardinal): Cardinal; overload;

function LRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function LRot(const Value: Word; const Count: TBitRange): Word; overload;
function LRot(const Value: Integer; const Count: TBitRange): Integer; overload;
function LRot(const Value: Int64; const Count: TBitRange): Int64; overload;
function ReverseBits(Value: Byte): Byte; overload;
function ReverseBits(Value: Shortint): Shortint; overload;
function ReverseBits(Value: Smallint): Smallint; overload;
function ReverseBits(Value: Word): Word; overload;
function ReverseBits(Value: Integer): Integer; overload;
function ReverseBits(Value: Cardinal): Cardinal; overload;
function ReverseBits(Value: Int64): Int64; overload;
function ReverseBits(P: Pointer; Count: Integer): Pointer; overload;

function RRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function RRot(const Value: Word; const Count: TBitRange): Word; overload;
function RRot(const Value: Integer; const Count: TBitRange): Integer; overload;
function RRot(const Value: Int64; const Count: TBitRange): Int64; overload;

function Sar(const Value: Shortint; const Count: TBitRange): Shortint; overload;
function Sar(const Value: Smallint; const Count: TBitRange): Smallint; overload;
function Sar(const Value: Integer; const Count: TBitRange): Integer; overload;

function SetBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function SetBit(const Value: Shortint; const Bit: TBitRange): Shortint; overload;
function SetBit(const Value: Smallint; const Bit: TBitRange): Smallint; overload;
function SetBit(const Value: Word; const Bit: TBitRange): Word; overload;
function SetBit(const Value: Cardinal; const Bit: TBitRange): Cardinal; overload;
function SetBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
function SetBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
procedure SetBitBuffer(var Value; const Bit: Cardinal); overload;
{$IFDEF CPU64}
procedure SetBitBuffer(var Value; const Bit: Int64); overload;
{$ENDIF CPU64}

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Shortint; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Smallint; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Word; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Cardinal; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Integer; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Int64; const Bit: TBitRange): Boolean; overload;
function TestBitBuffer(const Value; const Bit: Cardinal): Boolean; overload;
{$IFDEF CPU64}
function TestBitBuffer(const Value; const Bit: Int64): Boolean; overload;
{$ENDIF CPU64}

function TestBits(const Value, Mask: Byte): Boolean; overload;
function TestBits(const Value, Mask: Shortint): Boolean; overload;
function TestBits(const Value, Mask: Smallint): Boolean; overload;
function TestBits(const Value, Mask: Word): Boolean; overload;
function TestBits(const Value, Mask: Cardinal): Boolean; overload;
function TestBits(const Value, Mask: Integer): Boolean; overload;
function TestBits(const Value, Mask: Int64): Boolean; overload;

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ToggleBit(const Value: Shortint; const Bit: TBitRange): Shortint; overload;
function ToggleBit(const Value: Smallint; const Bit: TBitRange): Smallint; overload;
function ToggleBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ToggleBit(const Value: Cardinal; const Bit: TBitRange): Cardinal; overload;
function ToggleBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
procedure ToggleBitBuffer(var Value; const Bit: Cardinal); overload;
{$IFDEF CPU64}
procedure ToggleBitBuffer(var Value; const Bit: Int64); overload;
{$ENDIF CPU64}

procedure BooleansToBits(var Dest: Byte; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Word; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Integer; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Int64; const B: TBooleanArray); overload;

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Integer; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean = False); overload;

function BitsNeeded(const X: Byte): Integer; overload;
function BitsNeeded(const X: Word): Integer; overload;
function BitsNeeded(const X: Integer): Integer; overload;
function BitsNeeded(const X: Int64): Integer; overload;

function Digits(const X: Cardinal): Integer;

function ReverseBytes(Value: Word): Word; overload;
function ReverseBytes(Value: Smallint): Smallint; overload;
function ReverseBytes(Value: Integer): Integer; overload;
function ReverseBytes(Value: Cardinal): Cardinal; overload;
function ReverseBytes(Value: Int64): Int64; overload;
function ReverseBytes(P: Pointer; Count: Integer): Pointer; overload;

// Arithmetic
procedure SwapOrd(var I, J: Byte); overload;
procedure SwapOrd(var I, J: Shortint); overload;
procedure SwapOrd(var I, J: Smallint); overload;
procedure SwapOrd(var I, J: Word); overload;
procedure SwapOrd(var I, J: Integer); overload;
procedure SwapOrd(var I, J: Cardinal); overload;
procedure SwapOrd(var I, J: Int64); overload;

function IncLimit(var B: Byte; const Limit: Byte; const Incr: Byte = 1): Byte; overload;
function IncLimit(var B: Shortint; const Limit: Shortint; const Incr: Shortint = 1): Shortint; overload;
function IncLimit(var B: Smallint; const Limit: Smallint; const Incr: Smallint = 1): Smallint; overload;
function IncLimit(var B: Word; const Limit: Word; const Incr: Word = 1): Word; overload;
function IncLimit(var B: Integer; const Limit: Integer; const Incr: Integer = 1): Integer; overload;
function IncLimit(var B: Cardinal; const Limit: Cardinal; const Incr: Cardinal = 1): Cardinal; overload;
function IncLimit(var B: Int64; const Limit: Int64; const Incr: Int64 = 1): Int64; overload;

function DecLimit(var B: Byte; const Limit: Byte; const Decr: Byte = 1): Byte; overload;
function DecLimit(var B: Shortint; const Limit: Shortint; const Decr: Shortint = 1): Shortint; overload;
function DecLimit(var B: Smallint; const Limit: Smallint; const Decr: Smallint = 1): Smallint; overload;
function DecLimit(var B: Word; const Limit: Word; const Decr: Word = 1): Word; overload;
function DecLimit(var B: Integer; const Limit: Integer; const Decr: Integer = 1): Integer; overload;
function DecLimit(var B: Cardinal; const Limit: Cardinal; const Decr: Cardinal = 1): Cardinal; overload;
function DecLimit(var B: Int64; const Limit: Int64; const Decr: Int64 = 1): Int64; overload;

function IncLimitClamp(var B: Byte; const Limit: Byte; const Incr: Byte = 1): Byte; overload;
function IncLimitClamp(var B: Shortint; const Limit: Shortint; const Incr: Shortint = 1): Shortint; overload;
function IncLimitClamp(var B: Smallint; const Limit: Smallint; const Incr: Smallint = 1): Smallint; overload;
function IncLimitClamp(var B: Word; const Limit: Word; const Incr: Word = 1): Word; overload;
function IncLimitClamp(var B: Integer; const Limit: Integer; const Incr: Integer = 1): Integer; overload;
function IncLimitClamp(var B: Cardinal; const Limit: Cardinal; const Incr: Cardinal = 1): Cardinal; overload;
function IncLimitClamp(var B: Int64; const Limit: Int64; const Incr: Int64 = 1): Int64; overload;

function DecLimitClamp(var B: Byte; const Limit: Byte; const Decr: Byte = 1): Byte; overload;
function DecLimitClamp(var B: Shortint; const Limit: Shortint; const Decr: Shortint = 1): Shortint; overload;
function DecLimitClamp(var B: Smallint; const Limit: Smallint; const Decr: Smallint = 1): Smallint; overload;
function DecLimitClamp(var B: Word; const Limit: Word; const Decr: Word = 1): Word; overload;
function DecLimitClamp(var B: Integer; const Limit: Integer; const Decr: Integer = 1): Integer; overload;
function DecLimitClamp(var B: Cardinal; const Limit: Cardinal; const Decr: Cardinal = 1): Cardinal; overload;
function DecLimitClamp(var B: Int64; const Limit: Int64; const Decr: Int64 = 1): Int64; overload;

function Max(const B1, B2: Byte): Byte; overload;
function Max(const B1, B2: Shortint): Shortint; overload;
function Max(const B1, B2: Smallint): Smallint; overload;
function Max(const B1, B2: Word): Word; overload;
function Max(const B1, B2: Integer): Integer; overload;
function Max(const B1, B2: Cardinal): Cardinal; overload;
function Max(const B1, B2: Int64): Int64; overload;

function Min(const B1, B2: Byte): Byte; overload;
function Min(const B1, B2: Shortint): Shortint; overload;
function Min(const B1, B2: Smallint): Smallint; overload;
function Min(const B1, B2: Word): Word; overload;
function Min(const B1, B2: Integer): Integer; overload;
function Min(const B1, B2: Cardinal): Cardinal; overload;
function Min(const B1, B2: Int64): Int64; overload;

const
  // Constants defining the number of bits in each Integer type

  BitsPerNibble   = 4;
  BitsPerByte     = 8;
  BitsPerShortint = SizeOf(Shortint) * BitsPerByte;
  BitsPerSmallint = SizeOf(Smallint) * BitsPerByte;
  BitsPerWord     = SizeOf(Word) * BitsPerByte;
  BitsPerInteger  = SizeOf(Integer) * BitsPerByte;
  BitsPerCardinal = SizeOf(Cardinal) * BitsPerByte;
  BitsPerInt64    = SizeOf(Int64) * BitsPerByte;

  // Constants defining the number of nibbles in each Integer type

  NibblesPerByte     = BitsPerByte div BitsPerNibble;
  NibblesPerShortint = SizeOf(Shortint) * NibblesPerByte;
  NibblesPerSmallint = SizeOf(Smallint) * NibblesPerByte;
  NibblesPerWord     = SizeOf(Word) * NibblesPerByte;
  NibblesPerInteger  = SizeOf(Integer) * NibblesPerByte;
  NibblesPerCardinal = SizeOf(Cardinal) * NibblesPerByte;
  NibblesPerInt64    = SizeOf(Int64) * NibblesPerByte;

  // Constants defining a mask with all bits set for each Integer type

  NibbleMask      = $F;
  ByteMask        = Byte($FF);
  ShortintMask    = Shortint($FF);
  SmallintMask    = Smallint($FFFF);
  WordMask        = Word($FFFF);
  IntegerMask     = Integer($FFFFFFFF);
  CardinalMask    = Cardinal($FFFFFFFF);
  Int64Mask       = Int64($FFFFFFFFFFFFFFFF);

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
  JclBase;

// Conversion
function OrdToBinary(Value: Byte): string;
var
  I: Integer;
begin
  SetLength(Result, BitsPerByte);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (Value and $00000001));
    Value := Value shr 1;
  end;
end;

function OrdToBinary(Value: Shortint): string;
var
  I: Integer;
begin
  SetLength(Result, BitsPerShortint);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (Value and $00000001));
    Value := Value shr 1;
  end;
end;

function OrdToBinary(Value: Smallint): string;
var
  I: Integer;
  S: Smallint;
begin
  SetLength(Result, BitsPerSmallint);
  S := Value;
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (S and $00000001));
    S := S shr 1;
  end;
end;

function OrdToBinary(Value: Word): string;
var
  I: Integer;
begin
  SetLength(Result, BitsPerWord);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (Value and $00000001));
    Value := Value shr 1;
  end;
end;

function OrdToBinary(Value: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, BitsPerInteger);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (Value and $00000001));
    Value := Value shr 1;
  end;
end;

function OrdToBinary(Value: Cardinal): string;
var
  I: Integer;
begin
  SetLength(Result, BitsPerCardinal);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (Value and $00000001));
    Value := Value shr 1;
  end;
end;

function OrdToBinary(Value: Int64): string;
var
  I: Integer;
begin
  SetLength(Result, BitsPerInt64);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I + 1] := Chr(48 + (Value and Int64(1)));
    Value := Value shr Int64(1);
  end;
end;


// Bit manipulation
function BitsHighest(X: Cardinal): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX X
  // <-- EAX
  MOV     ECX, EAX
  MOV     EAX, -1
  BSR     EAX, ECX
  JNZ     @@End
  MOV     EAX, -1
@@End:
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX X
  // <-- RAX
  MOV     EAX, -1
  MOV     R10D, EAX
  BSR     EAX, ECX
  CMOVZ   EAX, R10D
  {$ENDIF CPU64}
end;

function BitsHighest(X: Integer): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX X
  // <-- EAX
  MOV     ECX, EAX
  MOV     EAX, -1
  BSR     EAX, ECX
  JNZ     @@End
  MOV     EAX, -1
@@End:
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX X
  // <-- RAX
  MOV     EAX, -1
  MOV     R10D, EAX
  BSR     EAX, ECX
  CMOVZ   EAX, R10D
  {$ENDIF CPU64}
end;

function BitsHighest(X: Byte): Integer;
begin
  Result := BitsHighest(Cardinal(X));
end;

function BitsHighest(X: Word): Integer;
begin
  Result := BitsHighest(Cardinal(X));
end;

function BitsHighest(X: SmallInt): Integer;
begin
  Result := BitsHighest(Integer(X));
end;

function BitsHighest(X: ShortInt): Integer;
begin
  Result := BitsHighest(Integer(X));
end;

function BitsHighest(X: Int64): Integer;
{$IFDEF CPU32}
begin
  if TJclULargeInteger(X).HighPart = 0 then
    Result := BitsHighest(TJclULargeInteger(X).LowPart)
  else
    Result := BitsHighest(TJclULargeInteger(X).HighPart) + 32;
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX X
  // <-- RAX

  // this is much shorter than "MOV RAX, -1"
  XOR     RAX, RAX
  DEC     EAX

  MOV     R10, RAX
  BSR     RAX, RCX
  CMOVZ   RAX, R10
end;
{$ENDIF CPU64}

function BitsLowest(X: Cardinal): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX X
  // <-- EAX
  MOV     ECX, EAX
  MOV     EAX, -1
  BSF     EAX, ECX
  JNZ     @@End
  MOV     EAX, -1
@@End:
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> RCX X
  // <-- EAX
  MOV     EAX, -1
  MOV     R10D, EAX
  BSF     EAX, ECX
  CMOVZ   EAX, R10D
  {$ENDIF CPU64}
end;

function BitsLowest(X: Integer): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX X
  // <-- EAX
  MOV     ECX, EAX
  MOV     EAX, -1
  BSF     EAX, ECX
  JNZ     @@End
  MOV     EAX, -1
@@End:
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> RCX X
  // <-- EAX
  MOV     EAX, -1
  MOV     R10D, EAX
  BSF     EAX, ECX
  CMOVZ   EAX, R10D
  {$ENDIF CPU64}
end;

function BitsLowest(X: Byte): Integer;
begin
  Result := BitsLowest(Cardinal(X));
end;

function BitsLowest(X: Shortint): Integer;
begin
  Result := BitsLowest(Integer(X));
end;

function BitsLowest(X: Smallint): Integer;
begin
  Result := BitsLowest(Integer(X));
end;

function BitsLowest(X: Word): Integer;
begin
  Result := BitsLowest(Cardinal(X));
end;

function BitsLowest(X: Int64): Integer;
{$IFDEF CPU32}
begin
  if TJclULargeInteger(X).LowPart = 0 then
    Result := BitsLowest(TJclULargeInteger(X).HighPart) + 32
  else
    Result := BitsLowest(TJclULargeInteger(X).LowPart);
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX X
  // <-- RAX

  // this is much shorter than "MOV RAX, -1"
  XOR     RAX, RAX
  DEC     EAX

  MOV     R10, RAX
  BSF     RAX, RCX
  CMOVZ   RAX, R10
end;
{$ENDIF CPU64}

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte;
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerByte - 1   // modulo BitsPerByte
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BTR    EAX, EDX
end;

function ClearBit(const Value: Shortint; const Bit: TBitRange): Shortint;
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerShortint - 1   // modulo BitsPerShortint
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BTR    EAX, EDX
end;

function ClearBit(const Value: Smallint; const Bit: TBitRange): Smallint;
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerSmallint - 1   // modulo BitsPerSmallint
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BTR    EAX, EDX
end;

function ClearBit(const Value: Word; const Bit: TBitRange): Word;
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerWord - 1   // modulo BitsPerWord
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BTR    EAX, EDX
end;

function ClearBit(const Value: Cardinal; const Bit: TBitRange): Cardinal;
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BTR    EAX, EDX
end;

function ClearBit(const Value: Integer; const Bit: TBitRange): Integer;
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BTR    EAX, EDX
end;

function ClearBit(const Value: Int64; const Bit: TBitRange): Int64;
{$IFDEF CPU32}
begin
  Result := Value and not (Int64(1) shl (Bit and (BitsPerInt64 - 1)));
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  //     DL  Bit
  // <-- RAX Result
  MOV    RAX, RCX
  BTR    RAX, RDX
end;
{$ENDIF CPU64}

procedure ClearBitBuffer(var Value; const Bit: Cardinal);
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  P^ := ClearBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> EAX Value
  //        EDX Bit
  // 64 --> RCX Value
  //        EDX Bit
  BTR    [Value], Bit
end;
{$ENDIF ~PUREPASCAL}

{$IFDEF CPU64}
procedure ClearBitBuffer(var Value; const Bit: Int64);
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  P^ := ClearBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  // 64 --> RCX Value
  //        RDX Bit
  BTR    [Value], Bit
end;
{$ENDIF ~PUREPASCAL}
{$ENDIF CPU64}

const
  BitSetPerNibble: array[0..15] of Integer = (0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4);

function CountBitsSet(X: Cardinal): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to NibblesPerCardinal - 1 do
  begin
    Inc(Result, BitSetPerNibble[X and $F]);
    X := X shr BitsPerNibble;
  end;
end;

function CountBitsSet(X: Byte): Integer;
begin
  Result := BitSetPerNibble[X shr BitsPerNibble] + BitSetPerNibble[X and $F];
end;

function CountBitsSet(X: Word): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to NibblesPerWord - 1 do
  begin
    Inc(Result, BitSetPerNibble[X and $F]);
    X := X shr BitsPerNibble;
  end;
end;

function CountBitsSet(X: Smallint): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to NibblesPerSmallint - 1 do
  begin
    Inc(Result, BitSetPerNibble[X and $F]);
    X := X shr BitsPerNibble;
  end;
end;

function CountBitsSet(X: ShortInt): Integer;
begin
  Result := BitSetPerNibble[X shr BitsPerNibble] + BitSetPerNibble[X and $F];
end;

function CountBitsSet(X: Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to NibblesPerInteger - 1 do
  begin
    Inc(Result, BitSetPerNibble[X and $F]);
    X := X shr BitsPerNibble;
  end;
end;

function CountBitsSet(P: Pointer; Count: Cardinal): Cardinal;
var
  b: Byte;
begin
  Result := 0;
  while Count > 0 do
  begin
    b := PByte(P)^;

    // lower Nibble
    Inc(Result, BitSetPerNibble[b and $0F]);
    // upper Nibble
    Inc(Result, BitSetPerNibble[b shr BitsPerNibble]);

    Dec(Count);
    Inc(PByte(P));
  end;
end;

function CountBitsSet(X: Int64): Integer;
begin
  Result := CountBitsSet(TJclULargeInteger(X).LowPart) + CountBitsSet(TJclULargeInteger(X).HighPart);
end;

function CountBitsCleared(X: Byte): Integer;
begin
  Result := BitsPerByte - CountBitsSet(Byte(X));
end;

function CountBitsCleared(X: Shortint): Integer;
begin
  Result := BitsPerShortint - CountBitsSet(Byte(X));
end;

function CountBitsCleared(X: Smallint): Integer;
begin
  Result := BitsPerSmallint - CountBitsSet(Word(X));
end;

function CountBitsCleared(X: Word): Integer;
begin
  Result := BitsPerWord - CountBitsSet(Word(X));
end;

function CountBitsCleared(X: Integer): Integer;
begin
  Result := BitsPerInteger - CountBitsSet(Integer(X));
end;

function CountBitsCleared(X: Cardinal): Integer;
begin
  Result := BitsPerCardinal - CountBitsSet(Cardinal(X));
end;

function CountBitsCleared(X: Int64): Integer;
begin
  Result := BitsPerInt64 - CountBitsSet(Int64(X));
end;

function CountBitsCleared(P: Pointer; Count: Cardinal): Cardinal;
begin
  Result := Count * BitsPerByte - CountBitsSet(P, Count);
end;

function LRot(const Value: Byte; const Count: TBitRange): Byte;
asm
  {$IFDEF CPU32}
  // --> AL Value
  //     DL Count
  // <-- AL Result
  MOV    CL, DL
  ROL    AL, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CL Value
  //     DL Count
  // <-- AL Result
  MOV    AL, CL
  MOV    CL, DL
  ROL    AL, CL
  {$ENDIF CPU64}
end;

function LRot(const Value: Word; const Count: TBitRange): Word;
asm
  {$IFDEF CPU32}
  // --> AX Value
  //     DL Count
  // <-- AX Result
  MOV    CL, DL
  ROL    AX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CX Value
  //     DL Count
  // <-- AX Result
  MOV    AX, CX
  MOV    CL, DL
  ROL    AX, CL
  {$ENDIF CPU64}
end;

function LRot(const Value: Integer; const Count: TBitRange): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX Value
  //     DL  Count
  // <-- EAX Result
  MOV    CL,  DL
  ROL    EAX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX Value
  //     DL  Count
  // <-- EAX Result
  MOV    EAX, ECX
  MOV    CL,  DL
  ROL    EAX, CL
  {$ENDIF CPU64}
end;

function LRot(const Value: Int64; const Count: TBitRange): Int64;
{$IFDEF CPU32}
asm
  // --> Value on stack
  //     AL  Count
  // <-- EDX:EAX Result

  PUSH   ESI
  PUSH   EDI

  MOV    CL, Count
  MOV    EDX, DWORD PTR [Value + 4]
  MOV    EAX, DWORD PTR [Value]

  // Count := Count mod 64
  AND    CL, $3F
  JZ     @@End

  CMP    CL, 32
  JL     @@RolBits

  // Count >= 32: "rol Count" = "rol 32" + "rol (32 - Count)"
@@Swap:
  // "rol 32"
  XCHG   EAX, EDX
  // Count := 32 - Count
  SUB    CL, 32

@@RolBits:
  MOV    EDI, EDX
  MOV    ESI, EAX

  // shift the bits
  SHL    EDX, CL
  SHL    EAX, CL

  // CounterShiftCount := 32 - Count
  NEG    CL
  ADD    CL, 32
  // bitwise-or the counter shifted bits
  SHR    EDI, CL
  SHR    ESI, CL
  OR     EAX, EDI
  OR     EDX, ESI

@@End:
  POP    EDI
  POP    ESI
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  //     DL  Count
  // <-- RAX Result
  MOV    RAX, RCX
  MOV    CL,  DL
  ROL    RAX, CL
end;
{$ENDIF CPU64}

function RRot(const Value: Int64; const Count: TBitRange): Int64;
{$IFDEF CPU32}
begin
  Result := LRot(Value, 64 - (Count and $3F));
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  //     DL  Count
  // <-- RAX Result
  MOV    RAX, RCX
  MOV    CL,  DL
  ROR    RAX, CL
end;
{$ENDIF CPU64}

const
  // Lookup table of bit reversed nibbles, used by simple overloads of ReverseBits
  RevNibbles: array [0..NibbleMask] of Byte =
    ($0, $8, $4, $C, $2, $A, $6, $E, $1, $9, $5, $D, $3, $B, $7, $F);

function ReverseBits(Value: Byte): Byte;
begin
  Result := RevNibbles[Value shr BitsPerNibble] or
    (RevNibbles[Value and NibbleMask] shl BitsPerNibble);
end;

function ReverseBits(Value: Shortint): Shortint;
begin
  Result := RevNibbles[Byte(Value) shr BitsPerNibble] or
    (RevNibbles[Value and NibbleMask] shl BitsPerNibble);
end;

function ReverseBits(Value: Smallint): Smallint;
begin
  Result := ReverseBits(Word(Value));
end;

function ReverseBits(Value: Word): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NibblesPerWord - 1 do
  begin
    Result := (Result shl BitsPerNibble) or RevNibbles[Value and NibbleMask];
    Value := Value shr BitsPerNibble;
  end;
end;

function ReverseBits(Value: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NibblesPerInteger - 1 do
  begin
    Result := (Result shl BitsPerNibble) or RevNibbles[Value and NibbleMask];
    Value := Value shr BitsPerNibble;
  end;
end;

function ReverseBits(Value: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NibblesPerCardinal - 1 do
  begin
    Result := (Result shl BitsPerNibble) or RevNibbles[Value and NibbleMask];
    Value := Value shr BitsPerNibble;
  end;
end;

function ReverseBits(Value: Int64): Int64;
begin
  TJclULargeInteger(Result).LowPart := ReverseBits(TJclULargeInteger(Value).HighPart);
  TJclULargeInteger(Result).HighPart := ReverseBits(TJclULargeInteger(Value).LowPart);
end;

const
  // Lookup table of reversed bytes, used by pointer overload of ReverseBits
  ReverseTable: array [0..ByteMask] of Byte = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0,
    $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8,
    $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4,
    $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
    $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2,
    $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
    $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6,
    $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
    $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1,
    $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9,
    $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5,
    $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
    $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3,
    $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
    $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7,
    $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
    $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);

function ReverseBits(P: Pointer; Count: Integer): Pointer;
var
  P1, P2: PByte;
  T: Byte;
begin
  if (P <> nil) and (Count > 0) then
  begin
    P1 := P;
    P2 := P;
    Inc(P2, Count - 1);
    while TJclAddr(P1) < TJclAddr(P2) do
    begin
      T := ReverseTable[P1^];
      P1^ := ReverseTable[P2^];
      P2^ := T;
      Inc(P1);
      Dec(P2);
    end;
    if P1 = P2 then
      P1^ := ReverseTable[P1^];
  end;
  Result := P;
end;

function RRot(const Value: Byte; const Count: TBitRange): Byte;
asm
  {$IFDEF CPU32}
  // --> AL Value
  //     DL Count
  // <-- AL Result
  MOV    CL, DL
  ROR    AL, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CL Value
  //     DL Count
  // <-- AL Result
  MOV    AL, CL
  MOV    CL, DL
  ROR    AL, CL
  {$ENDIF CPU64}
end;

function RRot(const Value: Word; const Count: TBitRange): Word;
asm
  {$IFDEF CPU32}
  // --> AX Value
  //     DL Count
  // <-- AX Result
  MOV    CL, DL
  ROR    AX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CX Value
  //     DL Count
  // <-- AX Result
  MOV    AX, CX
  MOV    CL, DL
  ROR    AX, CL
  {$ENDIF CPU64}
end;

function RRot(const Value: Integer; const Count: TBitRange): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX Value
  //     DL  Count
  // <-- EAX Result
  MOV    CL,  DL
  ROR    EAX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX Value
  //     DL  Count
  // <-- EAX Result
  MOV    EAX, ECX
  MOV    CL,  DL
  ROR    EAX, CL
  {$ENDIF CPU64}
end;

function Sar(const Value: Shortint; const Count: TBitRange): Shortint;
asm
  {$IFDEF CPU32}
  // --> AL Value
  //     DL Count
  // <-- AL Result
  MOV    CL, DL
  SAR    AL, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CL Value
  //     DL Count
  // <-- AL Result
  MOV    AL, CL
  MOV    CL, DL
  SAR    AL, CL
  {$ENDIF CPU64}
end;

function Sar(const Value: Smallint; const Count: TBitRange): Smallint;
asm
  {$IFDEF CPU32}
  // --> AX Value
  //     DL Count
  // <-- AX Result
  MOV    CL, DL
  SAR    AX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CX Value
  //     DL Count
  // <-- AX Result
  MOV    AX, CX
  MOV    CL, DL
  SAR    AX, CL
  {$ENDIF CPU64}
end;

function Sar(const Value: Integer; const Count: TBitRange): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX Value
  //     DL  Count
  // <-- EAX Result
  MOV    CL, DL
  SAR    EAX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX Value
  //     DL  Count
  // <-- EAX Result
  MOV    EAX, ECX
  MOV    CL,  DL
  SAR    EAX, CL
  {$ENDIF CPU64}
end;

function SetBit(const Value: Byte; const Bit: TBitRange): Byte;
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerByte - 1   // modulo BitsPerByte
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BTS    EAX, EDX
end;

function SetBit(const Value: Shortint; const Bit: TBitRange): Shortint;
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerShortInt - 1   // modulo BitsPerShortInt
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BTS    EAX, EDX
end;

function SetBit(const Value: Smallint; const Bit: TBitRange): Smallint;
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerSmallInt - 1   // modulo BitsPerSmallInt
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BTS    EAX, EDX
end;

function SetBit(const Value: Word; const Bit: TBitRange): Word;
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerWord - 1   // modulo BitsPerWord
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BTS    EAX, EDX
end;

function SetBit(const Value: Cardinal; const Bit: TBitRange): Cardinal;
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BTS    EAX, EDX
end;

function SetBit(const Value: Integer; const Bit: TBitRange): Integer;
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BTS    EAX, EDX
end;

function SetBit(const Value: Int64; const Bit: TBitRange): Int64;
{$IFDEF CPU32}
begin
  Result := Value or (Int64(1) shl (Bit and (BitsPerInt64 - 1)));
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  //     DL  Bit
  // <-- RAX Result
  MOV    RAX, RCX
  BTS    RAX, RDX
end;
{$ENDIF CPU64}

procedure SetBitBuffer(var Value; const Bit: Cardinal);
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  P^ := SetBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  BTS    [Value], Bit
end;
{$ENDIF ~PUREPASCAL}

{$IFDEF CPU64}
procedure SetBitBuffer(var Value; const Bit: Int64);
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  P^ := SetBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  BTS    [Value], Bit
end;
{$ENDIF ~PUREPASCAL}
{$ENDIF CPU64}

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr (Bit and (BitsPerByte - 1))) and 1 <> 0;
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerByte - 1   // modulo BitsPerByte
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BT     EAX, EDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBit(const Value: Shortint; const Bit: TBitRange): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr (Bit and (BitsPerShortint - 1))) and 1 <> 0;
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerShortInt - 1   // modulo BitsPerShortInt
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BT     EAX, EDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBit(const Value: Smallint; const Bit: TBitRange): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr (Bit and (BitsPerSmallint - 1))) and 1 <> 0;
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerSmallInt - 1   // modulo BitsPerSmallInt
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BT     EAX, EDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBit(const Value: Word; const Bit: TBitRange): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr (Bit and (BitsPerWord - 1))) and 1 <> 0;
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerWord - 1   // modulo BitsPerWord
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BT     EAX, EDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBit(const Value: Cardinal; const Bit: TBitRange): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr (Bit and (BitsPerCardinal - 1))) and 1 <> 0;
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BT     EAX, EDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBit(const Value: Integer; const Bit: TBitRange): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr (Bit and (BitsPerInteger - 1))) and 1 <> 0;
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BT     EAX, EDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBit(const Value: Int64; const Bit: TBitRange): Boolean;
{$IFDEF CPU32}
begin
  Result := (Value shr (Bit and (BitsPerInt64 - 1))) and 1 <> 0;
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  //     DL  Bit
  // <-- RAX Result
  MOV    RAX, RCX
  BT     RAX, RDX
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

function TestBitBuffer(const Value; const Bit: Cardinal): Boolean;
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  Result := TestBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  BT     [Value], Bit
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}

{$IFDEF CPU64}
function TestBitBuffer(const Value; const Bit: Int64): Boolean;
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  Result := TestBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  BT     [Value], Bit
  SETC   AL
end;
{$ENDIF ~PUREPASCAL}
{$ENDIF CPU64}

function TestBits(const Value, Mask: Byte): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function TestBits(const Value, Mask: Shortint): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function TestBits(const Value, Mask: Smallint): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function TestBits(const Value, Mask: Word): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function TestBits(const Value, Mask: Cardinal): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function TestBits(const Value, Mask: Integer): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function TestBits(const Value, Mask: Int64): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte;
{$IFDEF PUREPASCAL}
begin
  Result := Value xor (1 shl (Bit and (BitsPerByte - 1)));
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerByte - 1   // modulo BitsPerByte
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BTC    EAX, EDX
end;
{$ENDIF ~PUREPASCAL}

function ToggleBit(const Value: Shortint; const Bit: TBitRange): Shortint;
{$IFDEF PUREPASCAL}
begin
  Result := Value xor (1 shl (Bit and (BitsPerShortint - 1)));
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AL Value
  //        DL Bit
  //    <-- AL Result
  // 64 --> CL Value
  //        DL Bit
  //    <-- AL Result
  AND    EDX, BitsPerShortInt - 1   // modulo BitsPerShortInt
  {$IFDEF CPU64}
  MOVZX  EAX, CL
  {$ENDIF CPU64}
  BTC    EAX, EDX
end;
{$ENDIF ~PUREPASCAL}

function ToggleBit(const Value: Smallint; const Bit: TBitRange): Smallint;
{$IFDEF PUREPASCAL}
begin
  Result := Value xor (1 shl (Bit and (BitsPerSmallint - 1)));
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerSmallInt - 1   // modulo BitsPerSmallInt
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BTC    EAX, EDX
end;
{$ENDIF ~PUREPASCAL}

function ToggleBit(const Value: Word; const Bit: TBitRange): Word;
{$IFDEF PUREPASCAL}
begin
  Result := Value xor (1 shl (Bit and (BitsPerWord - 1)));
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> AX Value
  //        DL Bit
  //    <-- AX Result
  // 64 --> CX Value
  //        DL Bit
  //    <-- AX Result
  AND    EDX, BitsPerWord - 1   // modulo BitsPerWord
  {$IFDEF CPU64}
  MOVZX  EAX, CX
  {$ENDIF CPU64}
  BTC    EAX, EDX
end;
{$ENDIF ~PUREPASCAL}

function ToggleBit(const Value: Cardinal; const Bit: TBitRange): Cardinal;
{$IFDEF PUREPASCAL}
begin
  Result := Value xor (1 shl (Bit and (BitsPerCardinal - 1)));
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BTC    EAX, EDX
end;
{$ENDIF ~PUREPASCAL}

function ToggleBit(const Value: Integer; const Bit: TBitRange): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value xor (1 shl (Bit and (BitsPerInteger - 1)));
end;
{$ELSE ~PUREPASCAL}
asm
  // 32 --> EAX Value
  //        DL  Bit
  //    <-- EAX Result
  // 64 --> ECX Value
  //        DL  Bit
  //    <-- EAX Result
  {$IFDEF CPU64}
  MOV    EAX, ECX
  {$ENDIF CPU64}
  BTC    EAX, EDX
end;
{$ENDIF ~PUREPASCAL}

function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64;
{$IFDEF CPU32}
begin
  Result := Value xor (Int64(1) shl (Bit and (BitsPerInt64 - 1)));
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  //     DL  Bit
  // <-- RAX Result
  MOV    RAX, RCX
  BTC    RAX, RDX
end;
{$ENDIF CPU64}

procedure ToggleBitBuffer(var Value; const Bit: Cardinal);
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  P^ := ToggleBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  BTC    [Value], Bit
end;
{$ENDIF ~PUREPASCAL}

{$IFDEF CPU64}
procedure ToggleBitBuffer(var Value; const Bit: Int64);
{$IFDEF PUREPASCAL}
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div 8);
  BitOfs := Bit mod 8;
  P^ := ToggleBit(P^, BitOfs);
end;
{$ELSE ~PUREPASCAL}
asm
  BTC    [Value], Bit
end;
{$ENDIF ~PUREPASCAL}
{$ENDIF CPU64}

procedure BooleansToBits(var Dest: Byte; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Byte(BitsPerByte - 1), High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

procedure BooleansToBits(var Dest: Word; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Word(BitsPerWord - 1), High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

procedure BooleansToBits(var Dest: Integer; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Integer(BitsPerInteger - 1), High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

procedure BooleansToBits(var Dest: Int64; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Int64(BitsPerInt64 - 1), High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, BitsPerByte)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, BitsPerWord)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

procedure BitsToBooleans(const Bits: Integer; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, BitsPerInteger)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, BitsPerInt64)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

function Digits(const X: Cardinal): Integer;
var
  Val: Cardinal;
begin
  Result := 0;
  Val := X;
  repeat
    Inc(Result);
    Val := Val div 10;
  until Val = 0;
end;

function BitsNeeded(const X: Byte): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

function BitsNeeded(const X: Word): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

function BitsNeeded(const X: Integer): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

function BitsNeeded(const X: Int64): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

function ReverseBytes(Value: Word): Word;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr 8) or (Value shl 8);
end;
{$ELSE ~PUREPASCAL}
asm
  {$IFDEF CPU32}
  // --> AX Value
  // <-- AX Value
  XCHG   AL, AH
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CX Value
  // <-- AX Value
  MOV    CL, AH
  MOV    CH, AL
  {$ENDIF CPU64}
end;
{$ENDIF ~PUREPASCAL}

function ReverseBytes(Value: Smallint): Smallint;
{$IFDEF PUREPASCAL}
asm
  XCHG    AL, AH
end;
{$ELSE ~PUREPASCAL}
asm
  {$IFDEF CPU32}
  // --> AX Value
  // <-- AX Value
  XCHG   AL, AH
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> CX Value
  // <-- AX Value
  MOV    CL, AH
  MOV    CH, AL
  {$ENDIF CPU64}
end;
{$ENDIF ~PUREPASCAL}

function ReverseBytes(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr 24) or (Value shl 24) or ((Value and $00FF0000) shr 8) or ((Value and $0000FF00) shl 8);
end;
{$ELSE ~PUREPASCAL}
asm
  {$IFDEF CPU32}
  // --> EAX Value
  // <-- EAX Value
  BSWAP  EAX
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX Value
  // <-- EAX Value
  MOV    EAX, ECX
  BSWAP  EAX
  {$ENDIF CPU64}
end;
{$ENDIF ~PUREPASCAL}

function ReverseBytes(Value: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr 24) or (Value shl 24) or ((Value and $00FF0000) shr 8) or ((Value and $0000FF00) shl 8);
end;
{$ELSE ~PUREPASCAL}
asm
  {$IFDEF CPU32}
  // --> EAX Value
  // <-- EAX Value
  BSWAP  EAX
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX Value
  // <-- EAX Value
  MOV    EAX, ECX
  BSWAP  EAX
  {$ENDIF CPU64}
end;
{$ENDIF ~PUREPASCAL}

function ReverseBytes(Value: Int64): Int64;
{$IFDEF CPU32}
var
  Lo, Hi: Cardinal;
begin
  // low and hi DWORD swap
  Lo := TJclULargeInteger(Value).HighPart;
  Hi := TJclULargeInteger(Value).LowPart;
  TJclULargeInteger(Result).HighPart := (Hi shr 24) or (Hi shl 24) or ((Hi and $00FF0000) shr 8) or ((Hi and $0000FF00) shl 8);
  TJclULargeInteger(Result).LowPart := (Lo shr 24) or (Lo shl 24) or ((Lo and $00FF0000) shr 8) or ((Lo and $0000FF00) shl 8);
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
asm
  // --> RCX Value
  // <-- RAX Result
  MOV    RAX, RCX
  BSWAP  RAX
end;
{$ENDIF CPU64}

function ReverseBytes(P: Pointer; Count: Integer): Pointer;
var
  P1, P2: PByte;
  T: Byte;
begin
  if (P <> nil) and (Count > 0) then
  begin
    P1 := P;
    P2 := P;
    Inc(P2, Count - 1);
    while TJclAddr(P1) < TJclAddr(P2) do
    begin
      T := P1^;
      P1^ := P2^;
      P2^ := T;
      Inc(P1);
      Dec(P2);
    end;
  end;
  Result := P;
end;

// Arithmetic
procedure SwapOrd(var I, J: Byte);
var
  T: Byte;
begin
  T := I;
  I := J;
  J := T;
end;

procedure SwapOrd(var I, J: Cardinal);
var
  T: Cardinal;
begin
  T := I;
  I := J;
  J := T;
end;

procedure SwapOrd(var I, J: Integer);
var
  T: Integer;
begin
  T := I;
  I := J;
  J := T;
end;

procedure SwapOrd(var I, J: Int64);
var
  T: Int64;
begin
  T := I;
  I := J;
  J := T;
end;

procedure SwapOrd(var I, J: Shortint);
var
  T: Shortint;
begin
  T := I;
  I := J;
  J := T;
end;

procedure SwapOrd(var I, J: Smallint);
var
  T: Smallint;
begin
  T := I;
  I := J;
  J := T;
end;

procedure SwapOrd(var I, J: Word);
var
  T: Word;
begin
  T := I;
  I := J;
  J := T;
end;

function IncLimit(var B: Byte; const Limit, Incr: Byte): Byte;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function IncLimit(var B: Shortint; const Limit, Incr: Shortint): Shortint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function IncLimit(var B: Smallint; const Limit, Incr: Smallint): Smallint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function IncLimit(var B: Word; const Limit, Incr: Word): Word;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function IncLimit(var B: Integer; const Limit, Incr: Integer): Integer;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function IncLimit(var B: Cardinal; const Limit, Incr: Cardinal): Cardinal;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function IncLimit(var B: Int64; const Limit, Incr: Int64): Int64;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

function DecLimit(var B: Byte; const Limit, Decr: Byte): Byte;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function DecLimit(var B: Shortint; const Limit, Decr: Shortint): shortint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function DecLimit(var B: Smallint; const Limit, Decr: Smallint): Smallint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function DecLimit(var B: Word; const Limit, Decr: Word): Word;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function DecLimit(var B: Integer; const Limit, Decr: Integer): Integer;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function DecLimit(var B: Cardinal; const Limit, Decr: Cardinal): Cardinal;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function DecLimit(var B: Int64; const Limit, Decr: Int64): Int64;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

function IncLimitClamp(var B: Byte; const Limit, Incr: Byte): Byte;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function IncLimitClamp(var B: Shortint; const Limit, Incr: Shortint): Shortint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function IncLimitClamp(var B: Smallint; const Limit, Incr: Smallint): Smallint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function IncLimitClamp(var B: Word; const Limit, Incr: Word): Word;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function IncLimitClamp(var B: Integer; const Limit, Incr: Integer): Integer;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function IncLimitClamp(var B: Cardinal; const Limit, Incr: Cardinal): Cardinal;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function IncLimitClamp(var B: Int64; const Limit, Incr: Int64): Int64;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Byte; const Limit, Decr: Byte): Byte;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Shortint; const Limit, Decr: Shortint): Shortint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Smallint; const Limit, Decr: Smallint): Smallint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Word; const Limit, Decr: Word): Word;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Integer; const Limit, Decr: Integer): Integer;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Cardinal; const Limit, Decr: Cardinal): Cardinal;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function DecLimitClamp(var B: Int64; const Limit, Decr: Int64): Int64;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

function Max(const B1, B2: Byte): Byte;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Byte): Byte;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function Max(const B1, B2: Shortint): Shortint;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Max(const B1, B2: Smallint): Smallint;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Shortint): Shortint;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Smallint): Smallint;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function Max(const B1, B2: Word): Word;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Max(const B1, B2: Int64): Int64;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Word): Word;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function Max(const B1, B2: Integer): Integer;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Integer): Integer;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function Max(const B1, B2: Cardinal): Cardinal;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Cardinal): Cardinal;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function Min(const B1, B2: Int64): Int64;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
