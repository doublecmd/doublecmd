
{
GetUTF8ByteCount returns the number of bytes necessary to hold the requested number
of characters (count). Not necessarily the number of characters is equal to the
widestring length but here we assume it to skip the extra overhead
}
//todo do a function that convert the str and the count at one pass
function GetUTF8ByteCount(const UTF8Str: UTF8String; WideCount: Integer): Integer;
var
  CharCount, CharLen, StrLen: Integer;
  P: PChar;
begin
  Result := 0;
  CharCount := 0;
  P := PChar(UTF8Str);
  StrLen := Length(UTF8Str);
  WideCount := Min(WideCount, StrLen);
  while (CharCount < WideCount) do
  begin
    CharLen := UTF8CharacterLength(P);
    Inc(P, CharLen);
    Inc(Result, CharLen);
    Inc(CharCount);
  end;
  Result := Min(Result, StrLen);
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := GetTextExtentPoint(DC, PChar(TempStr),
    GetUTF8ByteCount(TempStr, Count), Size);
end;
