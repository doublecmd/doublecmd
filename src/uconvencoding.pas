{
   Double Commander
   -------------------------------------------------------------------------
   Encoding conversion and related stuff

   Copyright (C) 2011-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uConvEncoding;

{$mode delphi}

interface

uses
  Classes;

const
  EncodingOem = 'oem';
  EncodingDefault = 'default';

type
  TMacroEncoding = (meOEM, meANSI, meUTF8, meUTF8BOM, meUTF16LE, meUTF16BE);

function HexToBin(HexString: String): String;
function TextIsASCII(const S: String): Boolean;
procedure GetSupportedEncodings(List: TStrings);
function DetectEncoding(const S: String): String; overload;
function SingleByteEncoding(TextEncoding: String): Boolean;
function DetectEncoding(const S: String; ADefault: TMacroEncoding; AStrict: Boolean): TMacroEncoding; overload;
function ConvertEncoding(const S, FromEncoding, ToEncoding: String{$ifdef FPC_HAS_CPSTRING}; SetTargetCodePage: Boolean = False{$endif}): String;

implementation

uses
  SysUtils, LazUTF8, LConvEncoding, GetText, DCConvertEncoding,
  nsCore, nsUniversalDetector, uLng;

var
  Lang, FallbackLang: AnsiString;
  SupportedEncodings: TStringList = nil;

type
  TMyCodePages = (cp1251, cpKOI8R, cp866);

const
  scCodePage  : array[TMyCodePages] of AnsiString =
    (
     // CP1251 (WINDOWS)
     #$C0#$E0 + // Аа
     #$C1#$E1 + // Бб
     #$C2#$E2 + // Вв
     #$C3#$E3 + // Гг
     #$C4#$E4 + // Дд
     #$C5#$E5 + // Ее
     #$A8#$B8 + // Ёё
     #$C6#$E6 + // Жж
     #$C7#$E7 + // Зз
     #$C8#$E8 + // Ии
     #$C9#$E9 + // Йй
     #$CA#$EA + // Кк
     #$CB#$EB + // Лл
     #$CC#$EC + // Мм
     #$CD#$ED + // Нн
     #$CE#$EE + // Оо
     #$CF#$EF + // Пп
     #$D0#$F0 + // Рр
     #$D1#$F1 + // Сс
     #$D2#$F2 + // Тт
     #$D3#$F3 + // Уу
     #$D4#$F4 + // Фф
     #$D5#$F5 + // Хх
     #$D6#$F6 + // Цц
     #$D7#$F7 + // Чч
     #$D8#$F8 + // Шш
     #$D9#$F9 + // Щщ
     #$DA#$FA + // Ъъ
     #$DB#$FB + // Ыы
     #$DC#$FC + // Ьь
     #$DD#$FD + // Ээ
     #$DE#$FE + // Юю
     #$DF#$FF , // Яя
     // KOI8-R (UNIX)
     #$E1#$C1 + // Аа
     #$E2#$C2 + // Бб
     #$F7#$D7 + // Вв
     #$E7#$C7 + // Гг
     #$E4#$C4 + // Дд
     #$E5#$C5 + // Ее
     #$B3#$A3 + // Ёё
     #$F6#$D6 + // Жж
     #$FA#$DA + // Зз
     #$E9#$C9 + // Ии
     #$EA#$CA + // Йй
     #$EB#$CB + // Кк
     #$EC#$CC + // Лл
     #$ED#$CD + // Мм
     #$EE#$CE + // Нн
     #$EF#$CF + // Оо
     #$F0#$D0 + // Пп
     #$F2#$D2 + // Рр
     #$F3#$D3 + // Сс
     #$F4#$D4 + // Тт
     #$F5#$D5 + // Уу
     #$E6#$C6 + // Фф
     #$E8#$C8 + // Хх
     #$E3#$C3 + // Цц
     #$FE#$DE + // Чч
     #$FB#$DB + // Шш
     #$FD#$DD + // Щщ
     #$FF#$DF + // Ъъ
     #$F9#$D9 + // Ыы
     #$F8#$D8 + // Ьь
     #$FC#$DC + // Ээ
     #$E0#$C0 + // Юю
     #$F1#$D1 , // Яя
     // CP866 (DOS)
     #$80#$A0 + // Аа
     #$81#$A1 + // Бб
     #$82#$A2 + // Вв
     #$83#$A3 + // Гг
     #$84#$A4 + // Дд
     #$85#$A5 + // Ее
     #$F0#$F1 + // Ёё
     #$86#$A6 + // Жж
     #$87#$A7 + // Зз
     #$88#$A8 + // Ии
     #$89#$A9 + // Йй
     #$8A#$AA + // Кк
     #$8B#$AB + // Лл
     #$8C#$AC + // Мм
     #$8D#$AD + // Нн
     #$8E#$AE + // Оо
     #$8F#$AF + // Пп
     #$90#$E0 + // Рр
     #$91#$E1 + // Сс
     #$92#$E2 + // Тт
     #$93#$E3 + // Уу
     #$94#$E4 + // Фф
     #$95#$E5 + // Хх
     #$96#$E6 + // Цц
     #$97#$E7 + // Чч
     #$98#$E8 + // Шш
     #$99#$E9 + // Щщ
     #$9A#$EA + // Ъъ
     #$9B#$EB + // Ыы
     #$9C#$EC + // Ьь
     #$9D#$ED + // Ээ
     #$9E#$EE + // Юю
     #$9F#$EF   // Яя
    );

var
  svStatistic : array[AnsiChar] of Single;

procedure InitStatistic;
begin
  FillChar(svStatistic, SizeOf(svStatistic), 0);
  // CP1251 (WINDOWS)
  svStatistic[#$C0] := 0.001; // 'А'
  svStatistic[#$C1] := 0;     // 'Б'
  svStatistic[#$C2] := 0.002; // 'В'
  svStatistic[#$C3] := 0;     // 'Г'
  svStatistic[#$C4] := 0.001; // 'Д'
  svStatistic[#$C5] := 0.001; // 'Е'
  svStatistic[#$C6] := 0;     // 'Ж'
  svStatistic[#$C7] := 0;     // 'З'
  svStatistic[#$C8] := 0.001; // 'И'
  svStatistic[#$C9] := 0;     // 'Й'
  svStatistic[#$CA] := 0.001; // 'К'
  svStatistic[#$CB] := 0;     // 'Л'
  svStatistic[#$CC] := 0.001; // 'М'
  svStatistic[#$CD] := 0.001; // 'Н'
  svStatistic[#$CE] := 0.001; // 'О'
  svStatistic[#$CF] := 0.002; // 'П'
  svStatistic[#$D0] := 0.002; // 'Р'
  svStatistic[#$D1] := 0.001; // 'С'
  svStatistic[#$D2] := 0.001; // 'Т'
  svStatistic[#$D3] := 0;     // 'У'
  svStatistic[#$D4] := 0;     // 'Ф'
  svStatistic[#$D5] := 0;     // 'Х'
  svStatistic[#$D6] := 0;     // 'Ц'
  svStatistic[#$D7] := 0.001; // 'Ч'
  svStatistic[#$D8] := 0.001; // 'Ш'
  svStatistic[#$D9] := 0;     // 'Щ'
  svStatistic[#$DA] := 0;     // 'Ъ'
  svStatistic[#$DB] := 0;     // 'Ы'
  svStatistic[#$DC] := 0;     // 'Ь'
  svStatistic[#$DD] := 0.001; // 'Э'
  svStatistic[#$DE] := 0;     // 'Ю'
  svStatistic[#$DF] := 0;     // 'Я'
  svStatistic[#$E0] := 0.057; // 'а'
  svStatistic[#$E1] := 0.01;  // 'б'
  svStatistic[#$E2] := 0.031; // 'в'
  svStatistic[#$E3] := 0.011; // 'г'
  svStatistic[#$E4] := 0.021; // 'д'
  svStatistic[#$E5] := 0.067; // 'е'
  svStatistic[#$E6] := 0.007; // 'ж'
  svStatistic[#$E7] := 0.013; // 'з'
  svStatistic[#$E8] := 0.052; // 'и'
  svStatistic[#$E9] := 0.011; // 'й'
  svStatistic[#$EA] := 0.023; // 'к'
  svStatistic[#$EB] := 0.03;  // 'л'
  svStatistic[#$EC] := 0.024; // 'м'
  svStatistic[#$ED] := 0.043; // 'н'
  svStatistic[#$EE] := 0.075; // 'о'
  svStatistic[#$EF] := 0.026; // 'п'
  svStatistic[#$F0] := 0.038; // 'р'
  svStatistic[#$F1] := 0.034; // 'с'
  svStatistic[#$F2] := 0.046; // 'т'
  svStatistic[#$F3] := 0.016; // 'у'
  svStatistic[#$F4] := 0.001; // 'ф'
  svStatistic[#$F5] := 0.006; // 'х'
  svStatistic[#$F6] := 0.002; // 'ц'
  svStatistic[#$F7] := 0.011; // 'ч'
  svStatistic[#$F8] := 0.004; // 'ш'
  svStatistic[#$F9] := 0.004; // 'щ'
  svStatistic[#$FA] := 0;     // 'ъ'
  svStatistic[#$FB] := 0.012; // 'ы'
  svStatistic[#$FC] := 0.012; // 'ь'
  svStatistic[#$FD] := 0.003; // 'э'
  svStatistic[#$FE] := 0.005; // 'ю'
  svStatistic[#$FF] := 0.015; // 'я'
end;

function MyConvertString(const S: AnsiString; const FromCP, ToCP: TMyCodePages): AnsiString;
var
  I: Integer;
  C: AnsiChar;
  Chars: array [AnsiChar] of AnsiChar;
begin
  Result:= S;

  if FromCP = ToCP then Exit;

  for C := #0 to #255 do
    Chars[C] := C;

  for I := 1 to Length(scCodePage[cp1251]) do
    Chars[scCodePage[FromCP][I]] := scCodePage[ToCP][I];

  for I := 1 to Length(s) do
    Result[I] := Chars[Result[I]];
end;

function DetectCharsetCyrillic(const S: AnsiString): AnsiString;
var
  I: Integer;
  J: LongWord;
  C: AnsiChar;
  D, M: Single;
  T: AnsiString;
  CodePage: TMyCodePages;
  CharCount: array [AnsiChar] of Integer;
begin
  J := 0;
  M := 0;
  T := S;
  FillChar(CharCount, SizeOf(CharCount), 0);
  for I := 1 to Length(S) do
    Inc(CharCount[S[I]]);
  // Check for CP866 encoding
  for C := #$80 {'А'} to #$AF {'п'} do
    Inc(J, CharCount[C]);
  if J > (Length(S) div 3) then
  begin
    Result := 'CP866';
    Exit;
  end;
  for C := #$C0 {'А'} to #$FF {'я'} do
    M := M + sqr(CharCount[C] / Length(S) - svStatistic[C]);
  for CodePage := Low(TMyCodePages) to High(TMyCodePages) do
  begin
    // Convert to cp1251, because statistic in this encoding
    T:= MyConvertString(S, CodePage, cp1251);

    FillChar(CharCount, SizeOf(CharCount), 0);
    for I := 1 to Length(T) do
      Inc(CharCount[T[I]]);
    D := 0;
    for C := #$C0 {'А'} to #$FF {'я'} do
      D := D + sqr(CharCount[C] / Length(S) - svStatistic[C]);
    if D <= M then
    begin
      M := D;
      case CodePage of
        cp1251 : Result:= 'CP1251';
        cpKOI8R: Result:= 'KOI-8';
        cp866  : Result:= 'CP866';
      end;
    end;
  end;
end;

function MyDetectCodePageType(const S: AnsiString): AnsiString;
var
  Detector: TnsUniversalDetector = nil;
  CharsetInfo: rCharsetInfo;
begin
  Detector:= TnsUniversalDetector.Create;
  try
    Detector.Reset;
    Detector.HandleData(PChar(S), Length(S));
    if not Detector.Done then Detector.DataEnd;
    CharsetInfo:= Detector.GetDetectedCharsetInfo;
    case CharsetInfo.CodePage of
        866: Result:= 'CP866';
        932: Result:= 'CP932';
        950: Result:= 'CP950';
       1251: Result:= 'CP1251';
       1252: Result:= 'CP1252';
       1253: Result:= 'CP1253';
       1255: Result:= 'CP1255';
      20866: Result:= 'KOI-8';
      else
        begin
          Result:= CharsetInfo.Name;
          // When unknown encoding then use system encoding
          if SupportedEncodings.IndexOf(Result) < 0 then
          begin
            if (FallbackLang = 'be') or (FallbackLang = 'bg') or
               (FallbackLang = 'ky') or (FallbackLang = 'mk') or
               (FallbackLang = 'mn') or (FallbackLang = 'ru') or
               (FallbackLang = 'tt') then
              Result:= DetectCharsetCyrillic(S)
            else
              begin
                Result:= GetDefaultTextEncoding;
                if NormalizeEncoding(Result) = EncodingUTF8 then begin
                  // the system encoding is UTF-8, but it is not UTF-8
                  // use ISO-8859-1 instead. This encoding has a full 1:1 mapping to unicode,
                  // so no character is lost during conversions.
                  Result:= 'ISO-8859-1';
                end;
              end;
          end;
        end;
    end;
  finally
    FreeAndNil(Detector);
  end;
end;

procedure GetSupportedEncodings(List: TStrings);
var
  Index: Integer;
begin
  if SupportedEncodings.Count > 0 then
    List.Assign(SupportedEncodings)
  else begin
    TStringList(List).CaseSensitive:= False;
    LConvEncoding.GetSupportedEncodings(List);
    Index:= List.IndexOf(EncodingAnsi);
    List[Index] := UpperCase(EncodingAnsi);
    List.Insert(Index + 1, UpperCase(EncodingOem));
  end;
end;

function DetectEncoding(const S: String): String;

  function CompareI(p1, p2: PChar; Count: integer): boolean;
  var
    i: Integer;
    Chr1: Byte;
    Chr2: Byte;
  begin
    for i:=1 to Count do begin
      Chr1 := byte(p1^);
      Chr2 := byte(p2^);
      if Chr1<>Chr2 then begin
        if Chr1 in [97..122] then
          dec(Chr1,32);
        if Chr2 in [97..122] then
          dec(Chr2,32);
        if Chr1<>Chr2 then exit(false);
      end;
      inc(p1);
      inc(p2);
    end;
    Result:=true;
  end;

var
  L, P: Integer;
  EndPos: Integer;
begin
  L:= Length(S);
  if L = 0 then begin
    Result:= GetDefaultTextEncoding;
    Exit;
  end;

  // Try detect Unicode
  case DetectEncoding(S, meOEM, False) of
    meUTF8:    Exit(EncodingUTF8);
    meUTF8BOM: Exit(EncodingUTF8BOM);
    meUTF16LE: Exit(EncodingUCS2LE);
    meUTF16BE: Exit(EncodingUCS2BE);
  end;

  // Try {%encoding eee}
  if (L >= 11) and CompareI(@S[1], '{%encoding ', 11) then
  begin
    P:= 12;
    while (P <= L) and (S[P] in [' ', #9]) do Inc(P);
    EndPos:= P;
    while (EndPos <= L) and (not (S[EndPos] in ['}', ' ', #9])) do Inc(EndPos);
    Result:= NormalizeEncoding(Copy(S, P, EndPos - P));
    Exit;
  end;

  // Try to detect encoding
  Result:= MyDetectCodePageType(S);
end;

function SingleByteEncoding(TextEncoding: String): Boolean;
begin
  TextEncoding := NormalizeEncoding(TextEncoding);
  if TextEncoding = EncodingDefault then TextEncoding := GetDefaultTextEncoding;
  Result := (TextEncoding <> EncodingUTF8) and (TextEncoding <> EncodingUTF8BOM) and
            (TextEncoding <> EncodingUCS2LE) and (TextEncoding <> EncodingUCS2BE);
end;

function DetectEncoding(const S: String; ADefault: TMacroEncoding;
  AStrict: Boolean): TMacroEncoding;
var
  L, P, I: Integer;
begin
  L:= Length(S);
  if L = 0 then Exit(ADefault);

  // Try UTF-8 BOM (Byte Order Mark)
  if (L >= 3) and (S[1] = #$EF) and (S[2] = #$BB ) and (S[3] = #$BF) then
  begin
    Result:= meUTF8BOM;
    Exit;
  end;

  // Try ucs-2le BOM FF FE
  if (L >= 2) and (S[1] = #$FF) and (S[2] = #$FE) then
  begin
    Result:= meUTF16LE;
    Exit;
  end;

  // Try ucs-2be BOM FE FF
  if (L >= 2) and (S[1] = #$FE) and (S[2] = #$FF) then
  begin
    Result:= meUTF16BE;
    Exit;
  end;

  // Try UTF-8 (this includes ASCII)
  P:= 1;
  I:= Ord(not AStrict);
  while (P <= L) do
  begin
    if Ord(S[P]) < 128 then
    begin
      // ASCII
      Inc(P);
    end
    else begin
      I:= UTF8CharacterStrictLength(@S[P]);
      if I = 0 then Exit(ADefault);
      Inc(P, I);
    end;
  end;
  if I <> 0 then
    Result:= meUTF8
  else begin
    Result:= ADefault;
  end;
end;

function ConvertEncoding(const S, FromEncoding, ToEncoding: String{$ifdef FPC_HAS_CPSTRING};
  SetTargetCodePage: Boolean{$endif}): String;
var
  Encoded : Boolean;
  AFrom, ATo : String;
begin
  AFrom:= NormalizeEncoding(FromEncoding);
  ATo:= NormalizeEncoding(ToEncoding);
  if AFrom = ATo then Exit(S);
  if S = EmptyStr then
  begin
    if ATo = EncodingUTF8BOM then
      Result:= UTF8BOM
    else begin
      Result := S;
    end;
    Exit;
  end;
  Encoded:= True;
  if AFrom = EncodingUTF8 then
  begin
    if ATo = EncodingAnsi then Result:= CeUtf8ToAnsi(S)
    else if ATo = EncodingOem then Result:= CeUtf8ToOem(S)
    else if ATo = EncodingDefault then Result:= CeUtf8ToSys(S)
    else Result:= ConvertEncodingFromUTF8(S, ATo, Encoded{$ifdef FPC_HAS_CPSTRING}, SetTargetCodePage{$endif});
    if Encoded then Exit;
  end
  else if ATo = EncodingUTF8 then
  begin
    if AFrom = EncodingAnsi then Result:= CeAnsiToUtf8(S)
    else if AFrom = EncodingOem then Result:= CeOemToUtf8(S)
    else if AFrom = EncodingDefault then Result:= CeSysToUtf8(S)
    else Result:= ConvertEncodingToUTF8(S, AFrom, Encoded);
    if Encoded then Exit;
  end
  else begin
    Result:= ConvertEncodingToUTF8(S, AFrom, Encoded);
    if Encoded then
      Result:= ConvertEncodingFromUTF8(Result, ATo, Encoded{$ifdef FPC_HAS_CPSTRING}, SetTargetCodePage{$endif});
    if Encoded then Exit;
  end;
  // Cannot encode: return original string
  Result:= S;
end;

function TextIsASCII(const S: String): Boolean; inline;
var
  I: Integer;
begin
  for I:= 1 to Length(S) do
  begin
    if Ord(S[I]) > 127 then
      Exit(False);
  end;
  Result:= True;
end;

function HexToBin(HexString: String): String;
var
  Byte: LongRec;
  L, J, C: Integer;
  HexValue: PAnsiChar;
  BinValue: PAnsiChar;
begin
  C:= 0;
  L:= Length(HexString);
  SetLength(Result, L);
  BinValue:= PAnsiChar(Result);
  HexValue:= PAnsiChar(HexString);
  while (L > 0) do
  begin
    // Skip space
    if HexValue^ = #32 then
    begin
      Dec(L);
      Inc(HexValue);
      Continue;
    end;
    // Read high and low 4 bits
    for J:= 1 downto 0 do
    begin
      if HexValue^ in ['A'..'F', 'a'..'f'] then
        Byte.Bytes[J]:= ((Ord(HexValue^) + 9) and 15)
      else if HexValue^ in ['0'..'9'] then
        Byte.Bytes[J]:= ((Ord(HexValue^)) and 15)
      else
        raise EConvertError.CreateFmt(rsMsgInvalidHexNumber, [HexValue^]);
      Dec(L);
      Inc(HexValue);
    end;
    // Result 8 bit
    BinValue^:= Chr(Byte.Bytes[0] + (Byte.Bytes[1] shl 4));
    Inc(BinValue);
    Inc(C);
  end;
  SetLength(Result, C);
end;

initialization
  InitStatistic;
  GetLanguageIDs(Lang, FallbackLang);
  SupportedEncodings:= TStringList.Create;
  GetSupportedEncodings(SupportedEncodings);

finalization
  FreeAndNil(SupportedEncodings);

end.
