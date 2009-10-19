unit uCryptProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uClassesEx;

type

  { TPasswordStore }

  TPasswordStore = class(TIniFileEx)
  private
    FMasterKey: AnsiString;
  public
    function WritePassword(Prefix, Name: UTF8String; const Password: AnsiString): Boolean;
    function ReadPassword(Prefix, Name: UTF8String; out Password: AnsiString): Boolean;
  end;

function Encode(MasterKey, Data: AnsiString): AnsiString;
function Decode(MasterKey, Data: AnsiString): AnsiString;

var
  PasswordStore: TPasswordStore = nil;

implementation

uses
  LCLProc, Base64, BlowFish, uGlobsPaths;

type
  TBlowFishKeyRec = record
    dwSize: LongWord;
    case Boolean of
      True: (bBlowFishKey: TBlowFishKey);
      False: (cBlowFishKey: array [0..SizeOf(TBlowFishKey)] of AnsiChar);
  end;

function Encode(MasterKey, Data: AnsiString): AnsiString;
var
  BlowFishKeyRec: TBlowFishKeyRec;
  StringStream: TStringStream = nil;
  Base64EncodingStream: TBase64EncodingStream = nil;
  BlowFishEncryptStream: TBlowFishEncryptStream = nil;
begin
  Result:= EmptyStr;
  BlowFishKeyRec.cBlowFishKey:= MasterKey;
  BlowFishKeyRec.dwSize:= Length(MasterKey);

  try
    StringStream:= TStringStream.Create(EmptyStr);
    Base64EncodingStream:= TBase64EncodingStream.Create(StringStream);

    BlowFishEncryptStream:= TBlowFishEncryptStream.Create(BlowFishKeyRec.bBlowFishKey, BlowFishKeyRec.dwSize, Base64EncodingStream);
    BlowFishEncryptStream.Write(PAnsiChar(Data)^, Length(Data));
    BlowFishEncryptStream.Flush;
  finally
    FreeThenNil(BlowFishEncryptStream);
    FreeThenNil(Base64EncodingStream);
    Result:= StringStream.DataString;
    FreeThenNil(StringStream);
  end;
end;

function Decode(MasterKey, Data: AnsiString): AnsiString;
var
  BlowFishKeyRec: TBlowFishKeyRec;
  StringStream: TStringStream = nil;
  Base64DecodingStream: TBase64DecodingStream = nil;
  BlowFishDeCryptStream: TBlowFishDeCryptStream = nil;
begin
  Result:= EmptyStr;
  BlowFishKeyRec.cBlowFishKey:= MasterKey;
  BlowFishKeyRec.dwSize:= Length(MasterKey);

  try
    StringStream:= TStringStream.Create(Data);
    Base64DecodingStream:= TBase64DecodingStream.Create(StringStream);

    SetLength(Result, Base64DecodingStream.Size);
    BlowFishDeCryptStream:= TBlowFishDeCryptStream.Create(BlowFishKeyRec.bBlowFishKey, BlowFishKeyRec.dwSize, Base64DecodingStream);
    BlowFishDeCryptStream.Read(PAnsiChar(Result)^, Base64DecodingStream.Size);
  finally
    FreeThenNil(BlowFishDeCryptStream);
    FreeThenNil(Base64DecodingStream);
    FreeThenNil(StringStream);
  end;
end;

{ TPasswordStore }

function TPasswordStore.WritePassword(Prefix, Name: UTF8String;
  const Password: AnsiString): Boolean;
begin

end;

function TPasswordStore.ReadPassword(Prefix, Name: UTF8String; out
  Password: AnsiString): Boolean;
begin

end;

initialization
  try
    PasswordStore:= TPasswordStore.Create(gpIniDir + 'pwd.ini', fmOpenReadWrite);
  except
    DebugLn('Can not create secure password store!');
  end;

finalization
  FreeThenNil(PasswordStore);

end.

