{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.011 |
|==============================================================================|
| Content: Inline MIME support procedures and functions                        |
|==============================================================================|
| Copyright (c)1999-2017, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2000-2006.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s): Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)      |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Utilities for inline MIME)
Support for Inline MIME encoding and decoding.

Used RFC: RFC-2047, RFC-2231
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit MimeInLn;

interface

uses
  SysUtils, Classes;

type
  TSpecials = set of AnsiChar;

const
  SpecialChar: TSpecials =
  ['=', '(', ')', '[', ']', '<', '>', ':', ';', ',', '@', '/', '?', '\',
    '"', '_'];
  NonAsciiChar: TSpecials =
  [#0..#31, #127..#255];

{:Returns a portion of the "Value" string located to the left of the "Delimiter"
 string. If a delimiter is not found, results is original string.}
function SeparateLeft(const Value, Delimiter: string): string;

{:Returns the portion of the "Value" string located to the right of the
 "Delimiter" string. If a delimiter is not found, results is original string.}
function SeparateRight(const Value, Delimiter: string): string;

{:Returns parameter value from string in format:
 parameter1="value1"; parameter2=value2}
function GetParameter(const Value, Parameter: string): string;

{:Fetch string from left of Value string. This function ignore delimitesr inside
 quotations.}
function FetchEx(var Value: string; const Delimiter, Quotation: string): string;

{:Decodes mime inline encoding (i.e. in headers).}
function InlineDecode(const Value: string): string;

{:Encodes string to MIME inline encoding.}
function InlineEncode(const Value: string): string;

{:Returns @true, if "Value" contains characters which require inline coding.}
function NeedInline(const Value: AnsiString): boolean;

{:Decodes mime inline encoding similar to @link(InlineDecode), but it checks
 first that "Value" encoded by inline coding.}
function InlineDecodeEx(const Value: string): string;

{:Inline MIME encoding similar to @link(InlineEncode), but it checks first that
 "Value" contains characters which require inline coding.}
function InlineEncodeEx(const Value: string): string;

implementation

uses
  Base64, LConvEncoding, DCConvertEncoding;

{==============================================================================}

function UnquoteStr(const Value: string; Quote: Char): string;
var
  n: integer;
  inq, dq: Boolean;
  c, cn: char;
begin
  Result := '';
  if Value = '' then
    Exit;
  if Value = Quote + Quote then
    Exit;
  inq := False;
  dq := False;
  for n := 1 to Length(Value) do
  begin
    c := Value[n];
    if n <> Length(Value) then
      cn := Value[n + 1]
    else
      cn := #0;
    if c = quote then
      if dq then
        dq := False
      else
        if not inq then
          inq := True
        else
          if cn = quote then
          begin
            Result := Result + Quote;
            dq := True;
          end
          else
            inq := False
    else
      Result := Result + c;
  end;
end;

{==============================================================================}

function FetchEx(var Value: string; const Delimiter, Quotation: string): string;
var
  b: Boolean;
begin
  Result := '';
  b := False;
  while Length(Value) > 0 do
  begin
    if b then
    begin
      if Pos(Quotation, Value) = 1 then
        b := False;
      Result := Result + Value[1];
      Delete(Value, 1, 1);
    end
    else
    begin
      if Pos(Delimiter, Value) = 1 then
      begin
        Delete(Value, 1, Length(delimiter));
        break;
      end;
      b := Pos(Quotation, Value) = 1;
      Result := Result + Value[1];
      Delete(Value, 1, 1);
    end;
  end;
end;

{==============================================================================}

function SeparateLeft(const Value, Delimiter: string): string;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x < 1 then
    Result := Value
  else
    Result := Copy(Value, 1, x - 1);
end;

{==============================================================================}

function SeparateRight(const Value, Delimiter: string): string;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x > 0 then
    x := x + Length(Delimiter) - 1;
  Result := Copy(Value, x + 1, Length(Value) - x);
end;

{==============================================================================}

function GetParameter(const Value, Parameter: string): string;
var
  s: string;
  v: string;
begin
  Result := '';
  v := Value;
  while v <> '' do
  begin
    s := Trim(FetchEx(v, ';', '"'));
    if Pos(Uppercase(parameter), Uppercase(s)) = 1 then
    begin
      Delete(s, 1, Length(Parameter));
      s := Trim(s);
      if s = '' then
        Break;
      if s[1] = '=' then
      begin
        Result := Trim(SeparateRight(s, '='));
        Result := UnquoteStr(Result, '"');
        break;
      end;
    end;
  end;
end;

{==============================================================================}

function DecodeTriplet(const Value: AnsiString; Delimiter: AnsiChar): AnsiString;
var
  x, l, lv: Integer;
  c: AnsiChar;
  b: Byte;
  bad: Boolean;
begin
  lv := Length(Value);
  SetLength(Result, lv);
  x := 1;
  l := 1;
  while x <= lv do
  begin
    c := Value[x];
    Inc(x);
    if c <> Delimiter then
    begin
      Result[l] := c;
      Inc(l);
    end
    else
      if x < lv then
      begin
        Case Value[x] Of
          #13:
            if (Value[x + 1] = #10) then
              Inc(x, 2)
            else
              Inc(x);
          #10:
            if (Value[x + 1] = #13) then
              Inc(x, 2)
            else
              Inc(x);
        else
          begin
            bad := False;
            Case Value[x] Of
              '0'..'9': b := (Byte(Value[x]) - 48) Shl 4;
              'a'..'f', 'A'..'F': b := ((Byte(Value[x]) And 7) + 9) shl 4;
            else
              begin
                b := 0;
                bad := True;
              end;
            end;
            Case Value[x + 1] Of
              '0'..'9': b := b Or (Byte(Value[x + 1]) - 48);
              'a'..'f', 'A'..'F': b := b Or ((Byte(Value[x + 1]) And 7) + 9);
            else
              bad := True;
            end;
            if bad then
            begin
              Result[l] := c;
              Inc(l);
            end
            else
            begin
              Inc(x, 2);
              Result[l] := AnsiChar(b);
              Inc(l);
            end;
          end;
        end;
      end
      else
        break;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function DecodeQuotedPrintable(const Value: AnsiString): AnsiString;
begin
  Result := DecodeTriplet(Value, '=');
end;

{==============================================================================}

function EncodeTriplet(const Value: AnsiString; Delimiter: AnsiChar;
  Specials: TSpecials): AnsiString;
var
  n, l: Integer;
  s: AnsiString;
  c: AnsiChar;
begin
  SetLength(Result, Length(Value) * 3);
  l := 1;
  for n := 1 to Length(Value) do
  begin
    c := Value[n];
    if c in Specials then
    begin
      Result[l] := Delimiter;
      Inc(l);
      s := IntToHex(Ord(c), 2);
      Result[l] := s[1];
      Inc(l);
      Result[l] := s[2];
      Inc(l);
    end
    else
    begin
      Result[l] := c;
      Inc(l);
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function EncodeQuotedPrintable(const Value: AnsiString): AnsiString;
begin
  Result := EncodeTriplet(Value, '=',  ['='] + NonAsciiChar);
end;

{==============================================================================}

function EncodeSafeQuotedPrintable(const Value: AnsiString): AnsiString;
begin
  Result := EncodeTriplet(Value, '=', SpecialChar + NonAsciiChar);
end;

{==============================================================================}

function InlineDecode(const Value: string): string;
var
  s, su, e, v: string;
  x, y, z, n: Integer;
  b: Boolean;
  c: Char;

  function SearchEndInline(const Value: string; be: Integer): Integer;
  var
    n, q: Integer;
  begin
    q := 0;
    Result := 0;
    for n := be + 2 to Length(Value) - 1 do
      if Value[n] = '?' then
      begin
        Inc(q);
        if (q > 2) and (Value[n + 1] = '=') then
        begin
          Result := n;
          Break;
        end;
      end;
  end;

begin
  Result := '';
  v := Value;
  x := Pos('=?', v);
  y := SearchEndInline(v, x);
  // fix for broken coding
  // with begin, but not with end.
  if (x > 0) and (y <= 0) then
    y := Length(Result);
  while (y > x) and (x > 0) do
  begin
    s := Copy(v, 1, x - 1);
    if Trim(s) <> '' then
      Result := Result + s;
    s := Copy(v, x, y - x + 2);
    Delete(v, 1, y + 1);
    su := Copy(s, 3, Length(s) - 4);
    z := Pos('?', su);
    if (Length(su) >= (z + 2)) and (su[z + 2] = '?') then
    begin
      e := SeparateLeft(Copy(su, 1, z - 1), '*');
      c := UpperCase(su)[z + 1];
      su := Copy(su, z + 3, Length(su) - z - 2);
      if c = 'B' then
      begin
        s := DecodeStringBase64(su);
        s := ConvertEncodingToUTF8(s, e, b);
        if not b then Exit(EmptyStr);
      end;
      if c = 'Q' then
      begin
        s := '';
        for n := 1 to Length(su) do
          if su[n] = '_' then
            s := s + ' '
          else
            s := s + su[n];
        s := DecodeQuotedPrintable(s);
        s := ConvertEncodingToUTF8(s, e, b);
        if not b then Exit(EmptyStr);
      end;
    end;
    Result := Result + s;
    x := Pos('=?', v);
    y := SearchEndInline(v, x);
  end;
  Result := Result + v;
end;

{==============================================================================}

function InlineEncode(const Value: string): string;
var
  s, s1, e: string;
  n: Integer;
begin
  s := Value;
  e := 'UTF-8';
  s := EncodeSafeQuotedPrintable(s);
  s1 := '';
  Result := '';
  for n := 1 to Length(s) do
    if s[n] = ' ' then
    begin
      s1 := s1 + '_';
      if Length(s1) > 32 then
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + '=?' + e + '?Q?' + s1 + '?=';
        s1 := '';
      end;
    end
    else
      s1 := s1 + s[n];
  if s1 <> '' then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + '=?' + e + '?Q?' + s1 + '?=';
  end;
end;

{==============================================================================}

function NeedInline(const Value: AnsiString): boolean;
var
  n: Integer;
begin
  Result := False;
  for n := 1 to Length(Value) do
    if Value[n] in (SpecialChar + NonAsciiChar - ['_']) then
    begin
      Result := True;
      Break;
    end;
end;

{==============================================================================}

function InlineDecodeEx(const Value: string): string;
begin
  if Pos('=?', Value) > 0 then
    Result := InlineDecode(Value)
  else
    Result := CeSysToUtf8(Value);
end;

{==============================================================================}

function InlineEncodeEx(const Value: string): string;
begin
  if NeedInline(Value) then
    Result := InlineEncode(Value)
  else
    Result := Value;
end;

end.
