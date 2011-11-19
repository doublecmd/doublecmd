unit osConvEncoding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

var

  {en
    Convert from OEM to System encoding, if needed
  }
  OEMToSys: function (const Source: String): String;
  SysToOEM: function (const Source: String): String;

  {en
    Convert from Ansi to System encoding, if needed
  }
  AnsiToSys: function (const Source: String): String;
  SysToAnsi: function (const Source: String): String;

  {en
    Convert from Utf8 to System encoding, if needed
  }
  Utf8ToSys: function (const Source: String): String;
  SysToUtf8: function (const Source: String): String;

{$IFDEF UNIX}
function GetSystemEncoding(out Language, Encoding: String): Boolean;
{$ENDIF}

implementation

uses
  {$IF DEFINED(UNIX)}
  iconvenc_dyn
  {$ELSEIF DEFINED(MSWINDOWS)}
  Windows
  {$ENDIF}
  ;

function Dummy(const Source: String): String;
begin
  Result:= Source;
end;

function Ansi2UTF8(const Source: String): String;
begin
  Result:= UTF8Encode(Source);
end;

function UTF82Ansi(const Source: String): String;
begin
  Result:= UTF8Decode(Source);
end;

{$IF DEFINED(MSWINDOWS)}

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

var
  OEM,          // OEM Encoding
  ANSI: String; // ANSI Encoding
  Language, Encoding: String;

function OEM2Sys(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, OEM, Encoding);
end;

function Sys2OEM(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, Encoding, OEM);
end;

function Ansi2Sys(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, ANSI, Encoding);
end;

function Sys2Ansi(const Source: String): String;
begin
  Result:= Source;
  Iconvert(Source, Result, Encoding, ANSI);
end;

{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
initialization
  OEMToSys:=  @OEM2Ansi;
  SysToOEM:=  @Ansi2OEM;
  AnsiToSys:= @Dummy;
  SysToAnsi:= @Dummy;
  Utf8ToSys:= @UTF82Ansi;
  SysToUtf8:= @Ansi2UTF8;
{$ELSEIF DEFINED(UNIX)}
var
  Error: String;
initialization
  OEMToSys:=  @Dummy;
  SysToOEM:=  @Dummy;
  AnsiToSys:= @Dummy;
  SysToAnsi:= @Dummy;
  Utf8ToSys:= @Dummy;
  SysToUtf8:= @Dummy;

  // Try to get system encoding and initialize Iconv library
  if not (GetSystemEncoding(Language, Encoding) and InitIconv(Error)) then
    WriteLn(Error)
  else
    begin
      if (Language = 'be') or (Language = 'ru') or (Language = 'uk') then
      begin
        OEM:= 'CP866';
        OEMToSys:= @OEM2Sys;
        SysToOEM:= @Sys2OEM;
      end;
      if (Language = 'be') or (Language = 'bg') or (Language = 'ru') or (Language = 'uk') then
      begin
        ANSI:= 'CP1251';
        AnsiToSys:= @Ansi2Sys;
        SysToAnsi:= @Sys2Ansi;
      end;
      if not ((Encoding = 'UTF8') or (Encoding = 'UTF-8')) then
      begin
        Utf8ToSys:= @UTF82Ansi;
        SysToUtf8:= @Ansi2UTF8;
      end;
    end;
{$ELSE}
initialization
  OEMToSys:=  @Dummy;
  SysToOEM:=  @Dummy;
  AnsiToSys:= @Dummy;
  SysToAnsi:= @Dummy;
  Utf8ToSys:= @Dummy;
  SysToUtf8:= @Dummy;
{$ENDIF}

end.

