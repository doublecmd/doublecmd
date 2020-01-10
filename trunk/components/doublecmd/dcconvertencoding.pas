unit DCConvertEncoding;

{$mode objfpc}{$H+}

{$IF DEFINED(DARWIN)}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils; 

{$IF NOT DECLARED(RawByteString)}
type
  RawByteString = AnsiString;
{$IFEND}

var

  {en
    Convert from OEM to System encoding, if needed
  }
  CeOemToSys: function (const Source: String): RawByteString;
  CeSysToOem: function (const Source: String): RawByteString;

  {en
    Convert from OEM to UTF-8 encoding, if needed
  }
  CeOemToUtf8: function (const Source: String): RawByteString;
  CeUtf8ToOem: function (const Source: String): RawByteString;

  {en
    Convert from Ansi to System encoding, if needed
  }
  CeAnsiToSys: function (const Source: String): RawByteString;
  CeSysToAnsi: function (const Source: String): RawByteString;

  {en
    Convert from ANSI to UTF-8 encoding, if needed
  }
  CeAnsiToUtf8: function (const Source: String): RawByteString;
  CeUtf8ToAnsi: function (const Source: String): RawByteString;

  {en
    Convert from Utf8 to System encoding, if needed
  }
  CeUtf8ToSys: function (const Source: String): RawByteString;
  CeSysToUtf8: function (const Source: String): RawByteString;

function CeRawToUtf8(const Source: String): RawByteString;

function CeUtf8ToUtf16(const Source: String): UnicodeString;
function CeUtf16ToUtf8(const Source: UnicodeString): RawByteString;

{$IF DEFINED(MSWINDOWS)}
function CeTryEncode(const aValue: UnicodeString; aCodePage: Cardinal;
                     aAllowBestFit: Boolean; out aResult: AnsiString): Boolean;
function CeTryDecode(const aValue: AnsiString; aCodePage: Cardinal;
                     out aResult: UnicodeString): Boolean;
{$ELSEIF DEFINED(UNIX)}
var
  SystemEncodingUtf8: Boolean = False;
  SystemLanguage, SystemEncoding, SystemLocale: String;
{$ENDIF}

implementation

uses
  {$IF DEFINED(UNIX)}
  iconvenc_dyn
    {$IF DEFINED(DARWIN)}
    , MacOSAll, CocoaAll
    {$ENDIF}
  {$ELSEIF DEFINED(MSWINDOWS)}
  Windows
  {$ENDIF}
  ;

{$IF DEFINED(FPC_HAS_CPSTRING)}
var
  FileSystemCodePage: TSystemCodePage;
{$ENDIF}

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

function CeRawToUtf8(const Source: String): RawByteString;
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

function CeUtf8ToUtf16(const Source: String): UnicodeString;
{$IF DEFINED(MSWINDOWS)}
var
  L: SizeUInt;
begin
  L:= Length(Source);
  if L = 0 then Exit('');
  SetLength(Result, L + 1);
  // wide chars of UTF-16 <= bytes of UTF-8 string
  SetLength(Result, MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(Source), L, PWideChar(Result), L + 1));
end;
{$ELSE}
var
  L: SizeUInt;
begin
  L:= Length(Source);
  if L = 0 then Exit('');
  SetLength(Result, L + 1);
  L:= Utf8ToUnicode(PUnicodeChar(Result), L + 1, PAnsiChar(Source), L);
  if L > 0 then
    SetLength(Result, L - 1)
  else begin
    SetLength(Result, 0);
  end;
end;
{$ENDIF}

function CeUtf16ToUtf8(const Source: UnicodeString): RawByteString;
{$IF DEFINED(MSWINDOWS)}
var
  L: SizeUInt;
begin
  L:= Length(Source);
  if (L = 0) then Exit('');
  SetLength(Result, L * 3);
  // bytes of UTF-8 <= 3 * wide chars of UTF-16 string
  // e.g. %11100000 10100000 10000000 (UTF-8) is $0800 (UTF-16)
  SetLength(Result, WideCharToMultiByte(CP_UTF8, 0,
    PWideChar(Source), L, PAnsiChar(Result), Length(Result), nil, nil));
end;
{$ELSE}
var
  L: SizeUInt;
begin
  L:= Length(Source);
  if (L = 0) then Exit('');
  SetLength(Result, L * 3);
  L:= UnicodeToUtf8(PAnsiChar(Result), Length(Result) + 1, PUnicodeChar(Source), L);
  if L > 0 then
    SetLength(Result, L - 1)
  else begin
    SetLength(Result, 0);
  end;
end;
{$ENDIF}

function Dummy(const Source: String): RawByteString;
begin
  Result:= Source;
end;

{$IF DEFINED(FPC_HAS_CPSTRING)}

function Sys2UTF8(const Source: String): RawByteString;
begin
  Result:= Source;
  SetCodePage(Result, FileSystemCodePage, False);
  SetCodePage(Result, CP_UTF8, True);
  // Prevent another codepage appear in the strings
  // we don't need codepage conversion magic in our code
  SetCodePage(Result, DefaultSystemCodePage, False);
end;

function UTF82Sys(const Source: String): RawByteString;
begin
  Result:= Source;
  SetCodePage(Result, CP_UTF8, False);
  SetCodePage(Result, FileSystemCodePage, True);
  // Prevent another codepage appear in the strings
  // we don't need codepage conversion magic in our code
  SetCodePage(Result, DefaultSystemCodePage, False);
end;

{$ELSE}

function Sys2UTF8(const Source: String): RawByteString;
begin
  Result:= UTF8Encode(Source);
end;

function UTF82Sys(const Source: String): RawByteString;
begin
  Result:= UTF8Decode(Source);
end;

{$ENDIF}

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

function Oem2Utf8(const Source: String): RawByteString;
var
  UnicodeResult: UnicodeString;
begin
  if CeTryDecode(Source, CP_OEMCP, UnicodeResult) then
    Result:= UTF8Encode(UnicodeResult)
  else
    Result:= Source;
end;

function Utf82Oem(const Source: String): RawByteString;
var
  AnsiResult: AnsiString;
begin
  if CeTryEncode(UTF8Decode(Source), CP_OEMCP, False, AnsiResult) then
    Result:= AnsiResult
  else
    Result:= Source;
end;

function OEM2Ansi(const Source: String): RawByteString;
var
  Dst: PAnsiChar;
begin
  Result:= Source;
  Dst:= AllocMem((Length(Result) + 1) * SizeOf(AnsiChar));
  if OEMToChar(PAnsiChar(Result), Dst) then
    Result:= StrPas(Dst);
  FreeMem(Dst);
end;

function Ansi2OEM(const Source: String): RawByteString;
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

{$I dcconvertencoding.inc}

const
  EncodingUTF8 = 'UTF-8'; // UTF-8 Encoding

var
  EncodingOEM,           // OEM Encoding
  EncodingANSI: String;  // ANSI Encoding

function GetSystemEncoding: Boolean;
{$IF DEFINED(DARWIN)}
var
  Country: String;
  CurrentLocale: NSLocale;
  LanguageCFRef: CFStringRef = nil;
  LanguageCFArray: CFArrayRef = nil;
begin
  // System encoding
  SystemEncoding:= EncodingUTF8;
  // Get system language
  LanguageCFArray:= CFLocaleCopyPreferredLanguages;
  try
    Result:= CFArrayGetCount(LanguageCFArray) > 0;
    if Result then
    begin
      LanguageCFRef:= CFArrayGetValueAtIndex(LanguageCFArray, 0);
      SetLength(SystemLanguage, MAX_PATH);
      Result:= CFStringGetCString(LanguageCFRef,
                                  PAnsiChar(SystemLanguage),
                                  MAX_PATH,
                                  kCFStringEncodingUTF8
                                  );
    end;
  finally
    CFRelease(LanguageCFArray);
  end;
  if Result then
  begin
    // Crop to terminating zero
    SystemLanguage:= PAnsiChar(SystemLanguage);
    // Get system country
    CurrentLocale:= NSLocale.currentLocale();
    Country:= NSString(CurrentLocale.objectForKey(NSLocaleCountryCode)).UTF8String;
    // Combine system locale
    if (Length(SystemLanguage) > 0) and (Length(Country) > 0) then
    begin
      SystemLocale:= SystemLanguage + '_' + Country;
    end;
  end;
end;
{$ELSE}
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
  I:= Pos('_', Lang);
  if (I = 0) then
    SystemLanguage:= Lang
  else begin
    SystemLanguage:= Copy(Lang, 1, I - 1);
  end;
  I:= System.Pos('.', Lang);
  if (I > 0) then
  begin
    SystemLocale:= Copy(Lang, 1, I - 1);
    SystemEncoding:= Copy(Lang, I + 1, Length(Lang) - I);
  end
  else begin
    SystemLocale:= Lang;
    SystemEncoding:= EncodingUTF8;
  end;
end;
{$ENDIF}

{$IF DEFINED(DARWIN)}
function InitIconv(var Error: String): Boolean;
begin
  Error:= EmptyStr;
  Result:= TryLoadLib('libiconv.dylib', Error);
  IconvLibFound:= IconvLibFound or Result;
end;
{$ENDIF}

function FindEncoding: Boolean;
var
  Index: Integer;
begin
  // Try to find by language and country
  for Index:= Low(charset_relation) to High(charset_relation) do
  begin
    if CompareStr(charset_relation[Index, 1], SystemLocale) = 0 then
    begin
      EncodingANSI:= charset_relation[Index, 2];
      EncodingOEM:= charset_relation[Index, 3];
      Exit(True);
    end;
  end;
  // Try to find by language only
  for Index:= Low(charset_relation) to High(charset_relation) do
  begin
    if CompareStr(charset_relation[Index, 0], SystemLanguage) = 0 then
    begin
      EncodingANSI:= charset_relation[Index, 2];
      EncodingOEM:= charset_relation[Index, 3];
      Exit(True);
    end;
  end;
  Result:= False;
end;

function Oem2Utf8(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), EncodingOEM, EncodingUTF8);
end;

function Utf82Oem(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), EncodingUTF8, EncodingOEM);
end;

function OEM2Sys(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), EncodingOEM, SystemEncoding);
end;

function Sys2OEM(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), SystemEncoding, EncodingOEM);
end;

function Ansi2Sys(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), EncodingANSI, SystemEncoding);
end;

function Sys2Ansi(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), SystemEncoding, EncodingANSI);
end;

function Ansi2Utf8(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), EncodingANSI, EncodingUTF8);
end;

function Utf82Ansi(const Source: String): RawByteString;
begin
  Result:= Source;
  Iconvert(Source, String(Result), EncodingUTF8, EncodingANSI);
end;

procedure Initialize;
var
  Error: String = '';
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
  if not (GetSystemEncoding and InitIconv(Error)) then
    WriteLn(Error)
  else
    begin
      SystemEncodingUtf8:= (SysUtils.CompareText(SystemEncoding, 'UTF-8') = 0) or
                           (SysUtils.CompareText(SystemEncoding, 'UTF8') = 0);
      if FindEncoding then
      begin
        if (Length(EncodingOEM) > 0) then
        begin
          CeOemToSys:=  @OEM2Sys;
          CeSysToOem:=  @Sys2OEM;
          CeOemToUtf8:= @Oem2Utf8;
          CeUtf8ToOem:= @Utf82Oem;
        end;
        if (Length(EncodingANSI) > 0) then
        begin
          CeAnsiToSys:=  @Ansi2Sys;
          CeSysToAnsi:=  @Sys2Ansi;
          CeAnsiToUtf8:= @Ansi2Utf8;
          CeUtf8ToAnsi:= @Utf82Ansi;
        end;
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
  {$IF DEFINED(FPC_HAS_CPSTRING)}
  FileSystemCodePage:= WideStringManager.GetStandardCodePageProc(scpFileSystemSingleByte);
  {$ENDIF}
  Initialize;

end.
