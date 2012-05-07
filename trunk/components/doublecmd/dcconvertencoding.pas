unit DCConvertEncoding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

var

  {en
    Convert from OEM to System encoding, if needed
  }
  CeOemToSys: function (const Source: String): String;
  CeSysToOem: function (const Source: String): String;

  {en
    Convert from OEM to UTF-8 encoding, if needed
  }
  CeOemToUtf8: function (const Source: String): String;
  CeUtf8ToOem: function (const Source: String): String;

  {en
    Convert from Ansi to System encoding, if needed
  }
  CeAnsiToSys: function (const Source: String): String;
  CeSysToAnsi: function (const Source: String): String;

  {en
    Convert from ANSI to UTF-8 encoding, if needed
  }
  CeAnsiToUtf8: function (const Source: String): String;
  CeUtf8ToAnsi: function (const Source: String): String;

  {en
    Convert from Utf8 to System encoding, if needed
  }
  CeUtf8ToSys: function (const Source: String): String;
  CeSysToUtf8: function (const Source: String): String;

function CeRawToUtf8(const Source: String): String;

{$IF DEFINED(MSWINDOWS)}
function CeTryEncode(const aValue: UnicodeString; aCodePage: Cardinal;
                     aAllowBestFit: Boolean; out aResult: AnsiString): Boolean;
function CeTryDecode(const aValue: AnsiString; aCodePage: Cardinal;
                     out aResult: UnicodeString): Boolean;
{$ELSEIF DEFINED(UNIX)}
var
  SystemEncodingUtf8: Boolean = False;
  SystemLanguage, SystemEncoding: String;
{$ENDIF}

implementation

uses
  {$IF DEFINED(UNIX)}
  iconvenc_dyn
  {$ELSEIF DEFINED(MSWINDOWS)}
  Windows
  {$ENDIF}
  ;

function UTF8CharacterStrictLength(P: PAnsiChar): integer;
begin
  if p=nil then exit(0);
  if ord(p^)<%10000000 then begin
    // regular single byte character
    exit(1);
  end
  else if ord(p^)<%11000000 then begin
    // invalid single byte character
    exit(0);
  end
  else if ((ord(p^) and %11100000) = %11000000) then begin
    // should be 2 byte character
    if (ord(p[1]) and %11000000) = %10000000 then
      exit(2)
    else
      exit(0);
  end
  else if ((ord(p^) and %11110000) = %11100000) then begin
    // should be 3 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then
      exit(3)
    else
      exit(0);
  end
  else if ((ord(p^) and %11111000) = %11110000) then begin
    // should be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then
      exit(4)
    else
      exit(0);
  end else
    exit(0);
end;

function CeRawToUtf8(const Source: String): String;
var
  P: PAnsiChar;
  I, L: LongInt;
begin
  L:= Length(Source);
  // Try UTF-8 (this includes ASCII)
  P:= PAnsiChar(Source);
  repeat
    if Ord(P^) < 128 then begin
      // ASCII
      if (P^ = #0) and (P - PAnsiChar(Source) >= L) then begin
        Result:= Source;
        Exit;
      end;
      Inc(P);
    end else begin
      I:= UTF8CharacterStrictLength(P);
      if I = 0 then Break;
      Inc(P, I);
    end;
  until False;
  Result:= CeSysToUtf8(Source);
end;

function Dummy(const Source: String): String;
begin
  Result:= Source;
end;

function Sys2UTF8(const Source: String): String;
begin
  Result:= UTF8Encode(Source);
end;

function UTF82Sys(const Source: String): String;
begin
  Result:= UTF8Decode(Source);
end;

{$IF DEFINED(MSWINDOWS)}

function CeTryEncode(const aValue: UnicodeString; aCodePage: Cardinal;
  aAllowBestFit: Boolean; out aResult: AnsiString): Boolean;
// Try to encode the given Unicode string as the requested codepage
const
  WC_NO_BEST_FIT_CHARS = $00000400;
  Flags: array[Boolean] of DWORD = (WC_NO_BEST_FIT_CHARS, 0);
var
  UsedDefault: BOOL;
begin
  if not aAllowBestFit and not CheckWin32Version(4, 1) then
    Result := False
  else begin
    SetLength(aResult, WideCharToMultiByte(aCodePage, Flags[aAllowBestFit],
      PWideChar(aValue), Length(aValue), nil, 0, nil, @UsedDefault));
    SetLength(aResult, WideCharToMultiByte(aCodePage, Flags[aAllowBestFit],
      PWideChar(aValue), Length(aValue), PAnsiChar(aResult),
      Length(aResult), nil, @UsedDefault));
    Result := not UsedDefault;
  end;
end;

function CeTryDecode(const aValue: AnsiString; aCodePage: Cardinal;
  out aResult: UnicodeString): Boolean;
begin
  SetLength(aResult, MultiByteToWideChar(aCodePage, MB_ERR_INVALID_CHARS,
    LPCSTR(aValue), Length(aValue), nil, 0) * SizeOf(UnicodeChar));
  SetLength(aResult, MultiByteToWideChar(aCodePage, MB_ERR_INVALID_CHARS,
    LPCSTR(aValue), Length(aValue), PWideChar(aResult), Length(aResult)));
  Result := Length(aResult) > 0;
end;

function Oem2Utf8(const Source: String): String;
var
  UnicodeResult: UnicodeString;
begin
  if CeTryDecode(Source, CP_OEMCP, UnicodeResult) then
    Result:= UTF8Encode(UnicodeResult)
  else
    Result:= Source;
end;

function Utf82Oem(const Source: String): String;
var
  AnsiResult: AnsiString;
begin
  if CeTryEncode(UTF8Decode(Source), CP_OEMCP, False, AnsiResult) then
    Result:= AnsiResult
  else
    Result:= Source;
end;

function OEM2Ansi(const Source: String): String;
var
  Dst: PAnsiChar;
begin
  Result:= Source;
  Dst:= AllocMem((Length(Result) + 1) * SizeOf(AnsiChar));
  if OEMToChar(PAnsiChar(Result), Dst) then
    Result:= StrPas(Dst);
  FreeMem(Dst);
end;

function Ansi2OEM(const Source: String): String;
var
  Dst: PAnsiChar;
begin
  Result := Source;
  Dst := AllocMem((Length(Result) + 1) * SizeOf(AnsiChar));
  if CharToOEM(PAnsiChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
end;

procedure Initialize;
begin
  CeOemToSys:=   @OEM2Ansi;
  CeSysToOem:=   @Ansi2OEM;
  CeOemToUtf8:=  @Oem2Utf8;
  CeUtf8ToOem:=  @Utf82Oem;
  CeAnsiToSys:=  @Dummy;
  CeSysToAnsi:=  @Dummy;
  CeAnsiToUtf8:= @Sys2UTF8;
  CeUtf8ToAnsi:= @UTF82Sys;
  CeSysToUtf8:=  @Sys2UTF8;
  CeUtf8ToSys:=  @UTF82Sys;
end;

{$ELSEIF DEFINED(UNIX)}

function GetSystemEncoding(out Language, Encoding: String): Boolean;
var
  I: Integer;
  Lang: String;
begin
  Result:= True;
  Lang:= SysUtils.GetEnvironmentVariable('LC_ALL');
  if Length(Lang) = 0 then
    begin
      Lang:= SysUtils.GetEnvironmentVariable('LC_MESSAGES');
      if Length(Lang) = 0 then
      begin
        Lang:= SysUtils.GetEnvironmentVariable('LANG');
        if Length(Lang) = 0 then
          Exit(False);
      end;
    end;
  Language:= Copy(Lang, 1, 2);
  I:= System.Pos('.', Lang);
  if (I > 0) then
    Encoding:= Copy(Lang, I + 1, Length(Lang) - I);
  if Length(Encoding) = 0 then
    Encoding:= 'UTF-8';
end;

const
  EncodingUTF8 = 'UTF-8'; // UTF-8 Encoding

var
  EncodingOEM,           // OEM Encoding
  EncodingANSI: String;  // ANSI Encoding

function Oem2Utf8(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, EncodingOEM, EncodingUTF8);
end;

function Utf82Oem(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, EncodingUTF8, EncodingOEM);
end;

function OEM2Sys(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, EncodingOEM, SystemEncoding);
end;

function Sys2OEM(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, SystemEncoding, EncodingOEM);
end;

function Ansi2Sys(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, EncodingANSI, SystemEncoding);
end;

function Sys2Ansi(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, SystemEncoding, EncodingANSI);
end;

function Ansi2Utf8(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, EncodingANSI, EncodingUTF8);
end;

function Utf82Ansi(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, EncodingUTF8, EncodingANSI);
end;

procedure Initialize;
var
  Error: String;
begin
  CeOemToSys:=   @Dummy;
  CeSysToOem:=   @Dummy;
  CeOemToUtf8:=  @Dummy;
  CeUtf8ToOem:=  @Dummy;
  CeAnsiToSys:=  @Dummy;
  CeSysToAnsi:=  @Dummy;
  CeUtf8ToSys:=  @Dummy;
  CeSysToUtf8:=  @Dummy;
  CeAnsiToUtf8:= @Dummy;
  CeUtf8ToAnsi:= @Dummy;

  // Try to get system encoding and initialize Iconv library
  if not (GetSystemEncoding(SystemLanguage, SystemEncoding) and InitIconv(Error)) then
    WriteLn(Error)
  else
    begin
      SystemEncodingUtf8:= (CompareText(SystemEncoding, 'UTF-8') = 0) or
                           (CompareText(SystemEncoding, 'UTF8') = 0);
      if (SystemLanguage = 'be') or (SystemLanguage = 'ru') or
         (SystemLanguage = 'uk') then
      begin
        EncodingOEM:= 'CP866';
        CeOemToSys:=  @OEM2Sys;
        CeSysToOem:=  @Sys2OEM;
        CeOemToUtf8:= @Oem2Utf8;
        CeUtf8ToOem:= @Utf82Oem;
      end;
      if (SystemLanguage = 'be') or (SystemLanguage = 'bg') or
         (SystemLanguage = 'ru') or (SystemLanguage = 'uk') then
      begin
        EncodingANSI:= 'CP1251';
        CeAnsiToSys:=  @Ansi2Sys;
        CeSysToAnsi:=  @Sys2Ansi;
        CeAnsiToUtf8:= @Ansi2Utf8;
        CeUtf8ToAnsi:= @Utf82Ansi;
      end;
      if not SystemEncodingUtf8 then
      begin
        CeUtf8ToSys:= @UTF82Sys;
        CeSysToUtf8:= @Sys2UTF8;
      end;
    end;
end;

{$ELSE}

procedure Initialize;
begin
  CeOemToSys:=   @Dummy;
  CeSysToOem:=   @Dummy;
  CeOemToUtf8:=  @Dummy;
  CeUtf8ToOem:=  @Dummy;
  CeAnsiToSys:=  @Dummy;
  CeSysToAnsi:=  @Dummy;
  CeUtf8ToSys:=  @Dummy;
  CeSysToUtf8:=  @Dummy;
  CeAnsiToUtf8:= @Dummy;
  CeUtf8ToAnsi:= @Dummy;
end;

{$ENDIF}

initialization
  Initialize;

end.
