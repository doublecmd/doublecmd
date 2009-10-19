{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains Encrypt/Decrypt classes and functions.

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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
    function HasMasterKey: Boolean;
    function WritePassword(Prefix, Name, Connection: UTF8String; const Password: AnsiString): Boolean;
    function ReadPassword(Prefix, Name, Connection: UTF8String; out Password: AnsiString): Boolean;
    function DeletePassword(Prefix, Name, Connection: UTF8String): Boolean;
  end;

  { EEncryptDecryptFailed }

  EEncryptDecryptFailed = class(Exception)
  public
    constructor Create; reintroduce;
  end;


function Encode(MasterKey, Data: AnsiString): AnsiString;
function Decode(MasterKey, Data: AnsiString): AnsiString;

var
  PasswordStore: TPasswordStore = nil;

implementation

uses
  LCLProc, Dialogs, Base64, BlowFish, uGlobsPaths, uLng;

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

function TPasswordStore.HasMasterKey: Boolean;
begin
  Result:= (Length(FMasterKey) <> 0);
end;

function TPasswordStore.WritePassword(Prefix, Name, Connection: UTF8String;
                                      const Password: AnsiString): Boolean;
var
  Data: AnsiString;
begin
  Result:= False;
  if Length(FMasterKey) = 0 then
    begin
      if not InputQuery(rsMsgMasterPassword, rsMsgMasterPasswordEnter, True, FMasterKey) then
        Exit;
    end;
  Data:= Encode(FMasterKey, Password);
  if Data = EmptyStr then
    raise EEncryptDecryptFailed.Create;
  WriteString(Prefix + '_' + Name, Connection, Data);
  Result:= True;
end;

function TPasswordStore.ReadPassword(Prefix, Name, Connection: UTF8String;
                                     out Password: AnsiString): Boolean;
var
  Data: AnsiString;
begin
  Result:= False;
  if Length(FMasterKey) = 0 then
    begin
      if not InputQuery(rsMsgMasterPassword, rsMsgMasterPasswordEnter, True, FMasterKey) then
        Exit;
    end;
  Data:= ReadString(Prefix + '_' + Name, Connection, Data);
  if Data = EmptyStr then
    raise EEncryptDecryptFailed.Create;
  Password:= Decode(FMasterKey, Data);
  Result:= True;
end;

function TPasswordStore.DeletePassword(Prefix, Name, Connection: UTF8String): Boolean;
begin
  DeleteKey(Prefix + '_' + Name, Connection);
end;

procedure InitPasswordStore;
begin
  try
    PasswordStore:= TPasswordStore.Create(gpIniDir + 'pwd.ini', fmOpenReadWrite);
  except
    DebugLn('Can not create secure password store!');
  end;
end;

{ EEncryptDecryptFailed }

constructor EEncryptDecryptFailed.Create;
begin
  inherited Create('Encrypt/Decrypt failed');
end;

initialization
  InitPasswordStore;

finalization
  FreeThenNil(PasswordStore);

end.

