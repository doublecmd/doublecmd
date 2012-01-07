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
  SysUtils, Classes, LCLProc, LConvEncoding, nsCore, nsUniversalDetector;

var
  SupportedEncodings: TStringList = nil;

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
  l:=length(s);
  if l=0 then begin
    Result:='';
    exit;
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
  SupportedEncodings:= TStringList.Create;
  GetSupportedEncodings(SupportedEncodings);

finalization
  FreeAndNil(SupportedEncodings);

end.
