
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

{$ifndef HAS_DRAWTEXTW}
function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(lpString));
  Result := DrawText(hDC, PChar(TempStr), GetUTF8ByteCount(TempStr, nCount),
    lpRect, uFormat);
end;
{$endif}

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: ObjPas.PInteger): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := ExtTextOut(DC, X, Y, Options, Rect, PChar(TempStr),
    GetUTF8ByteCount(TempStr, Count), Dx);
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := GetTextExtentPoint(DC, PChar(TempStr),
    GetUTF8ByteCount(TempStr, Count), Size);
end;

function GetTextExtentExPointW(DC: HDC; Str: PWideChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: ObjPas.PInteger;
  var Size: TSize): BOOL;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := DelphiCompat.GetTextExtentExPoint(DC, PChar(TempStr),
    Count, MaxWidth, MaxCount, PartialWidths, Size);
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := GetTextExtentPoint(DC, PChar(TempStr),
    GetUTF8ByteCount(TempStr, Count), Size);
end;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;
var
  TempStr: UTF8String;
begin
  TempStr := UTF8Encode(WideString(Str));
  Result := TextOut(DC, X, Y, PChar(TempStr), GetUTF8ByteCount(TempStr, Count));
end;
