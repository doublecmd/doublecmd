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
  Classes, SysUtils, DCClassesUtf8;

type

  { TPasswordStore }

  TPasswordStore = class(TIniFileEx)
  private
    FMasterKey,
    FMasterKeyHash: AnsiString;
  public
    constructor Create(const AFileName: String); reintroduce;
  public
    function HasMasterKey: Boolean;
    function CheckMasterKey: Boolean;
    function WritePassword(Prefix, Name, Connection: String; const Password: AnsiString): Boolean;
    function ReadPassword(Prefix, Name, Connection: String; out Password: AnsiString): Boolean;
    function DeletePassword(Prefix, Name, Connection: String): Boolean;
  end;

  { EEncryptDecryptFailed }

  EEncryptDecryptFailed = class(Exception)
  public
    constructor Create; reintroduce;
  end;


function Encode(MasterKey, Data: AnsiString): AnsiString;
function Decode(MasterKey, Data: AnsiString): AnsiString;

procedure InitPasswordStore;

var
  PasswordStore: TPasswordStore = nil;

implementation

uses
  LCLProc, LCLType, Base64, BlowFish, md5, uShowMsg, uGlobsPaths, uLng, uDebug;

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
    Base64EncodingStream.Flush;
    Result:= StringStream.DataString;
  finally
    FreeThenNil(BlowFishEncryptStream);
    FreeThenNil(Base64EncodingStream);
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

constructor TPasswordStore.Create(const AFileName: String);
begin
  inherited Create(AFileName);

  CacheUpdates:= False;
  if ReadOnly then DCDebug('Read only password store!');
  FMasterKeyHash:= ReadString('General', 'MasterKey', EmptyStr);
end;

function TPasswordStore.HasMasterKey: Boolean;
begin
  Result:= (Length(FMasterKey) <> 0);
end;

function TPasswordStore.CheckMasterKey: Boolean;
var
  MasterKey,
  MasterKeyHash: AnsiString;
begin
  Result:= False;
  if Length(FMasterKey) <> 0 then Exit(True);
  if not ShowInputQuery(rsMsgMasterPassword, rsMsgMasterPasswordEnter, True, MasterKey) then
    Exit;
  if Length(MasterKey) = 0 then Exit;
  MasterKeyHash:= MD5Print(MD5String(MasterKey));
  MasterKeyHash:= Encode(MasterKey, MasterKeyHash);
  if FMasterKeyHash = EmptyStr then
    begin
      FMasterKeyHash:= MasterKeyHash;
      FMasterKey:= MasterKey;
      WriteString('General', 'MasterKey', FMasterKeyHash);
      Result:= True;
    end
  else if SameText(FMasterKeyHash, MasterKeyHash) then
    begin
      FMasterKey:= MasterKey;
      Result:= True;
    end
  else
    begin
      ShowMessageBox('Wrong password!'#13'Please try again!', 'Error!', MB_OK or MB_ICONERROR);
    end;
end;

function TPasswordStore.WritePassword(Prefix, Name, Connection: String;
                                      const Password: AnsiString): Boolean;
var
  Data: AnsiString;
begin
  Result:= False;
  if ReadOnly then Exit;
  if CheckMasterKey = False then Exit;
  Data:= Encode(FMasterKey, Password);
  if Length(Data) = 0 then raise EEncryptDecryptFailed.Create;
  try
    WriteString(Prefix + '_' + Name, Connection, Data);
  except
    Exit;
  end;
  Result:= True;
end;

function TPasswordStore.ReadPassword(Prefix, Name, Connection: String;
                                     out Password: AnsiString): Boolean;
var
  Data: AnsiString = '';
begin
  Result:= False;
  if CheckMasterKey = False then Exit;
  Data:= ReadString(Prefix + '_' + Name, Connection, Data);
  if Length(Data) = 0 then Exit;
  Password:= Decode(FMasterKey, Data);
  if Length(Password) = 0 then raise EEncryptDecryptFailed.Create;
  Result:= True;
end;

function TPasswordStore.DeletePassword(Prefix, Name, Connection: String): Boolean;
begin
  Result:= not ReadOnly;
  if Result then
  try
    DeleteKey(Prefix + '_' + Name, Connection);
  except
    Result:= False;
  end;
end;

procedure InitPasswordStore;
var
  AFileName: String;
begin
  AFileName := gpCfgDir + 'pwd.ini';
  try
    PasswordStore:= TPasswordStore.Create(AFileName);
  except
    DCDebug('Can not create secure password store!');
  end;
end;

{ EEncryptDecryptFailed }

constructor EEncryptDecryptFailed.Create;
begin
  inherited Create('Encrypt/Decrypt failed');
end;

finalization
  FreeThenNil(PasswordStore);

end.

