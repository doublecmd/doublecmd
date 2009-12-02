{
  Most of this code is based on similar functions from Lazarus LCLProc.
}

unit UnicodeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

{en
   Retrieves length in bytes of the next UTF-8 character.

   @param(P
          Pointer to the UTF-8 characters.)
   @param(aMaxBytes
          States how many bytes from P can be read.)
   @param(InvalidCharLen
          If an invalid UTF-8 character was found then InvalidCharLen has
          the number of bytes this character spans. If the character was valid
          InvalidCharLen is zero.)
}
function SafeUTF8NextCharLen(P: PByte; aMaxBytes: Integer; out InvalidCharLen: Integer): Integer;

{en
   Retrieves length in bytes of the previous UTF-8 character.
   It does not read from P, but rather from memory locations before P.

   @param(P
          Pointer to the UTF-8 characters.)
   @param(aMaxBytes
          States how many bytes from P *backwards* can be read.
          So, to safely read 3 bytes backwards ([p-1], [p-2], [p-3])
          this parameter should be at least 3.)
   @param(InvalidCharLen
          If an invalid UTF-8 character was found then InvalidCharLen has
          the number of bytes this character spans. If the character was valid
          InvalidCharLen is zero.)
}
function SafeUTF8PrevCharLen(P: PByte; aMaxBytes: Integer; out InvalidCharLen: Integer): Integer;
function SafeUTF8NextCharStart(UTF8Str: PByte; Len: PtrInt): PByte;
function SafeUTF8PrevCharEnd(UTF8Str: PByte; Len: PtrInt): PByte;
{en
   Returns UTF-16 character length, which is either 1 or 2.
   @param(utf16char
          Any UTF-16 char or one of the surrogate pairs.)
}
function UTF16CharLen(utf16char: Word): Integer;
{en
   Converts an UTF-16 surrogate pair into a unicode character.
}
function utf16PairToUnicode(u1, u2: Word): Cardinal;

function Utf16LEToUtf8(const s: string): string; // UTF16-LE 2 or 4 byte little endian
function Utf16BEToUtf8(const s: string): string; // UTF16-BE 2 or 4 byte little endian
function Utf32LEToUtf8(const s: string): string; // UTF32-LE 4 byte little endian
function Utf32BEToUtf8(const s: string): string; // UTF32-BE 4 byte big endian

{en
   Replaces invalid UTF-8 characters with '?'.
}
function Utf8ReplaceBroken(const s: UTF8String): UTF8String;

implementation

uses
  LCLProc;

const
  maxUTF8Len = 7;  // really is 4, but this includes any invalid characters up to length 7

function SafeUTF8NextCharLen(P: PByte; aMaxBytes: Integer; out InvalidCharLen: Integer): Integer;
var
  BytesLen: Integer;
  i: Integer;
begin
  if (p=nil) or (aMaxBytes = 0) then
  begin
    InvalidCharLen := 0;
    Result := 0;
  end
  else if p^<%10000000 then begin
    // regular single byte character
    InvalidCharLen := 0;
    Result := 1;
  end
  else if p^<%11000000 then begin
    // invalid single byte character
    InvalidCharLen := 1;
    Result := 1;
  end
  else
  begin
    // Read length of UTF-8 character in bytes.
         if ((p^ and %11100000) = %11000000) then BytesLen := 2
    else if ((p^ and %11110000) = %11100000) then BytesLen := 3
    else if ((p^ and %11111000) = %11110000) then BytesLen := 4
    else if ((p^ and %11111100) = %11111000) then BytesLen := 5
    else if ((p^ and %11111110) = %11111100) then BytesLen := 6
    else if ((p^ and %11111111) = %11111110) then BytesLen := 7
    else
      begin
        InvalidCharLen := 1;
        exit(1);
      end;

    // Check if the next bytes are from the middle of a character.
    for i := 1 to BytesLen - 1 do
    begin
      if (aMaxBytes < i) or ((p[i] and %11000000) <> %10000000) then
      begin
        InvalidCharLen := i;
        exit(1);
      end;
    end;

    InvalidCharLen := 0;
    Result := BytesLen;
  end;
end;

function SafeUTF8PrevCharLen(P: PByte; aMaxBytes: Integer; out InvalidCharLen: Integer): Integer;
var
  BytesLen: Integer;
  signature: Byte;
begin
  if (p=nil) or (aMaxBytes = 0) then
  begin
    InvalidCharLen := 0;
    Result := 0;
  end
  else if p[-1]<%10000000 then begin
    // regular single byte character
    InvalidCharLen := 0;
    Result := 1;
  end
  else
  begin
    for BytesLen := 1 to maxUTF8Len do
    begin
      if (aMaxBytes < BytesLen) then
      begin
        InvalidCharLen := aMaxBytes;
        exit(1);
      end;

      // Move past all the bytes in the middle of a character.
      if (p[-BytesLen] and %11000000) <> %10000000 then
        break;

      if BytesLen = maxUTF8Len then
      begin
        InvalidCharLen := BytesLen;
        exit(1);
      end;
    end;

    if p[-BytesLen]<%11000000 then
    begin
      // invalid first byte of a character
      InvalidCharLen := BytesLen;
      Result := 1;
    end
    else
    begin
      signature := Byte($FF shl (7 - BytesLen));
      if (p[-BytesLen] and signature) = Byte(signature shl 1) then
      begin
        // Correct first byte of a character.
        InvalidCharLen := 0;
        Result := BytesLen;
      end
      else
      begin
        // Invalid first byte of a character, or p is in the middle of a character.
        InvalidCharLen := BytesLen;
        Result := 1;
      end;
    end;
  end;
end;

function SafeUTF8NextCharStart(UTF8Str: PByte; Len: PtrInt): PByte;
var
  CharLen: LongInt;
  InvalidCharLen: Integer;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (Len>0) do begin
      CharLen := SafeUTF8NextCharLen(Result, Len, InvalidCharLen);
      if InvalidCharLen > 0 then
      begin
        dec(Len,InvalidCharLen);
        inc(Result,InvalidCharLen);
      end
      else if CharLen = 0 then
        exit(nil)
      else
        exit(Result);
    end;
    Result:=nil;
  end;
end;

function SafeUTF8PrevCharEnd(UTF8Str: PByte; Len: PtrInt): PByte;
var
  CharLen: LongInt;
  InvalidCharLen: Integer;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (Len>0) do begin
      CharLen := SafeUTF8PrevCharLen(Result, Len, InvalidCharLen);
      if InvalidCharLen > 0 then
      begin
        dec(Len,InvalidCharLen);
        dec(Result,InvalidCharLen);
      end
      else if CharLen = 0 then
        exit(nil)
      else
        exit(Result);  // Result is the character beginning
    end;
    Result:=nil;
  end;
end;

function UTF16CharLen(utf16char: Word): Integer; inline;
begin
  if (utf16char < $D800) or (utf16char > $DFFF) then
    Result := 1
  else
    Result := 2;
end;

function utf16PairToUnicode(u1, u2: Word): Cardinal;
begin
  if (u1 >= $D800) and (u1 <= $DBFF) then
  begin
    if (u2 >= $DC00) and (u2 <= $DFFF) then
      Result := (Cardinal(u1 - $D800) shl 10) + Cardinal(u2 - $DC00) + $10000
    else
      Result := 0;
  end
  else
    Result := u1;
end;

function Utf16LEToUtf8(const s: string): string;
var
  len: Integer;
  Src, Limit: PWord;
  Dest: PAnsiChar;
  u: Cardinal;
begin
  if Length(s) < 2 then begin
    Result:='';
    exit;
  end;
  Src:=PWord(Pointer(s));
  Limit := PWord(Pointer(Src) + Length(s));
  SetLength(Result, length(s) * 2);
  Dest:=PAnsiChar(Result);
  while Src + 1 <= Limit do begin
    len := UTF16CharLen(Src^);
    if len = 1 then
      u := LEtoN(Src^)
    else
    begin
      if Src + 2 <= Limit then
        u := utf16PairToUnicode(LEtoN(Src[0]), LEtoN(Src[1]))
      else
        break;
    end;
    inc(Src, len);
    if u<128 then begin
      Dest^:=chr(u);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(u,Dest));
    end;
  end;
  len:=PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    RaiseGDBException('');
  SetLength(Result,len);
end;

function Utf16BEToUtf8(const s: string): string;
var
  len: Integer;
  Src, Limit: PWord;
  Dest: PAnsiChar;
  u: Cardinal;
begin
  if Length(s) < 2 then begin
    Result:='';
    exit;
  end;
  Src:=PWord(Pointer(s));
  Limit := PWord(Pointer(Src) + Length(s));
  SetLength(Result, length(s) * 2);
  Dest:=PAnsiChar(Result);
  while Src + 1 <= Limit do begin
    len := UTF16CharLen(Src^);
    if len = 1 then
      u := BEtoN(Src^)
    else
    begin
      if Src + 2 <= Limit then
        u := utf16PairToUnicode(BEtoN(Src[0]), BEtoN(Src[1]))
      else
        break;
    end;
    inc(Src, len);
    if u<128 then begin
      Dest^:=chr(u);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(u,Dest));
    end;
  end;
  len:=PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    RaiseGDBException('');
  SetLength(Result,len);
end;

function Utf32LEToUtf8(const s: string): string;
var
  len: Integer;
  Src: PLongWord;
  Dest: PAnsiChar;
  i: Integer;
  c: LongWord;
begin
  if Length(s) < 4 then begin
    Result:='';
    exit;
  end;
  len:=length(s) div 4;
  SetLength(Result,len*4);
  Src:=PLongWord(Pointer(s));
  Dest:=PAnsiChar(Result);
  for i:=1 to len do begin
    c:=LEtoN(Src^);
    inc(Src);
    if c<128 then begin
      Dest^:=chr(c);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(c,Dest));
    end;
  end;
  len:=PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    RaiseGDBException('');
  SetLength(Result,len);
end;

function Utf32BEToUtf8(const s: string): string;
var
  len: Integer;
  Src: PLongWord;
  Dest: PAnsiChar;
  i: Integer;
  c: LongWord;
begin
  if Length(s) < 4 then begin
    Result:='';
    exit;
  end;
  len:=length(s) div 4;
  SetLength(Result,len*4);
  Src:=PLongWord(Pointer(s));
  Dest:=PAnsiChar(Result);
  for i:=1 to len do begin
    c:=BEtoN(Src^);
    inc(Src);
    if c<128 then begin
      Dest^:=chr(c);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(c,Dest));
    end;
  end;
  len:=PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    RaiseGDBException('');
  SetLength(Result,len);
end;

function Utf8ReplaceBroken(const s: UTF8String): UTF8String;
var
  i: Integer;
  Src, Dst, LastGoodPos: PByte;
  BytesLeft: Integer;
  InvalidCharLen: Integer;
  CharLen: Integer;
  GoodLen: Integer = 0;
begin
  if Length(s) = 0 then
    Exit(s);

  BytesLeft := Length(s);
  SetLength(Result, BytesLeft); // at most the same length

  Src := PByte(s);
  Dst := PByte(Result);
  LastGoodPos := Src;

  while BytesLeft > 0 do
  begin
    CharLen := SafeUTF8NextCharLen(Src, BytesLeft, InvalidCharLen);
    if InvalidCharLen > 0 then
    begin
      if LastGoodPos < Src then
      begin
        System.Move(LastGoodPos^, Dst^, Src - LastGoodPos);
        Inc(Dst, Src - LastGoodPos);
      end;

      Inc(Src, InvalidCharLen);
      Dec(BytesLeft, InvalidCharLen);
      LastGoodPos := Src;
      Dst^ := ord('?');
      Inc(Dst);
    end
    else
    begin
      Inc(Src, CharLen);
      Dec(BytesLeft, CharLen);
    end;
  end;

  if LastGoodPos = PByte(s) then
    Result := s    // All characters are good.
  else
  begin
    if LastGoodPos < Src then
    begin
      System.Move(LastGoodPos^, Dst^, Src - LastGoodPos);
      Inc(Dst, Src - LastGoodPos);
    end;

    SetLength(Result, Dst - PByte(Result));
  end;
end;

end.

