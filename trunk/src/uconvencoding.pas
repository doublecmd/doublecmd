{
   Double Commander
   -------------------------------------------------------------------------
   Encoding conversion and related stuff

   Copyright (C) 2011 Alexander Koblov (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uConvEncoding;

{$mode delphi}

interface

function DetectEncoding(const s: string): string;

implementation

uses
  SysUtils, Classes, LCLProc, LConvEncoding, GetText,
  nsCore, nsUniversalDetector;

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

function DetectEncoding(const s: string): string;

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
  l: Integer;
  p: Integer;
  EndPos: LongInt;
  i: LongInt;
begin
  l:= Length(s);
  if l = 0 then begin
    Result:= GetDefaultTextEncoding;
    Exit;
  end;

  // try UTF-8 BOM (Byte Order Mark)
  if CompareI(@s[1],#$EF#$BB#$BF,3) then begin
    Result:=EncodingUTF8BOM;
    exit;
  end;

  // try ucs-2le BOM FF FE
  if (length(s)>=2) and (s[1]=#$FF) and (s[2]=#$FE) then begin
    Result:=EncodingUCS2LE;
    exit;
  end;

  // try ucs-2be BOM FE FF
  if (length(s)>=2) and (s[1]=#$FE) and (s[2]=#$FF) then begin
    Result:=EncodingUCS2BE;
    exit;
  end;

  // try {%encoding eee}
  if CompareI(@s[1],'{%encoding ',11) then begin
    p:=12;
    while (p<=l) and (s[p] in [' ',#9]) do inc(p);
    EndPos:=p;
    while (EndPos<=l) and (not (s[EndPos] in ['}',' ',#9])) do inc(EndPos);
    Result:=NormalizeEncoding(copy(s,p,EndPos-p));
    exit;
  end;

  // try UTF-8 (this includes ASCII)
  p:=1;
  while (p<=l) do begin
    if ord(s[p])<128 then begin
      // ASCII
      inc(p);
    end else begin
      i:=UTF8CharacterStrictLength(@s[p]);
      //DebugLn(['GuessEncoding ',i,' ',DbgStr(s[p])]);
      if i=0 then begin
        break;
      end;
      inc(p,i);
    end;
  end;
  if p>l then begin
    Result:=EncodingUTF8;
    exit;
  end;

  // Try to detect encoding
  Result:= MyDetectCodePageType(s);
end;

initialization
  InitStatistic;
  GetLanguageIDs(Lang, FallbackLang);
  SupportedEncodings:= TStringList.Create;
  GetSupportedEncodings(SupportedEncodings);

finalization
  FreeAndNil(SupportedEncodings);

end.
