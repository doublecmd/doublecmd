{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains Encrypt/Decrypt classes and functions.

    Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uCryptProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCClassesUtf8;

type

  { TCryptStoreResult }

  TCryptStoreResult = (
     csrSuccess,    // Success
     csrFailed,     // Encrypt/Decrypt failed
     csrWriteError, // Could not write password to password store
     csrNotFound,   // Password not found in password store
     csrNoMasterKey // No master password entered yet
  );

  { TPasswordStore }

  TPasswordStore = class(TIniFileEx)
  private
    FMode: Byte;
    FMasterStrong: Boolean;
    FMasterKey: AnsiString;
    FMasterKeyHash: AnsiString;
  private
    procedure ConvertStore;
    procedure UpdateMasterKey(var MasterKey: AnsiString; var MasterKeyHash: AnsiString);
  public
    constructor Create(const AFileName: String); reintroduce;
  public
    function HasMasterKey: Boolean;
    function CheckMasterKey: Boolean;
    function WritePassword(Prefix, Name, Connection: String; const Password: AnsiString): TCryptStoreResult;
    function ReadPassword(Prefix, Name, Connection: String; out Password: AnsiString): TCryptStoreResult;
    function DeletePassword(Prefix, Name, Connection: String): Boolean;
  end;

procedure InitPasswordStore;

var
  PasswordStore: TPasswordStore = nil;

implementation

uses
  LCLType, LCLStrConsts, Base64, BlowFish, MD5, HMAC, SCRYPT, SHA3_512,
  Hash, DCPrijndael, Argon2, uShowMsg, uGlobsPaths, uLng, uDebug, uRandom;

const
  SCRYPT_N = (1 shl 14);
  SCRYPT_R = 8;
  SCRYPT_P = 1;

const
  ARGON2_M = (1 shl 16);
  ARGON2_T = 2;
  ARGON2_P = 4;

const
  AES_OFFS = 12; // (56 - 32) / 2
  KEY_SIZE = SizeOf(TBlowFishKey);
  MAC_SIZE = SizeOf(TSHA3_256Digest);
  BUF_SIZE = KEY_SIZE + MAC_SIZE;

type
  TBlowFishKeyRec = record
    dwSize: LongWord;
    case Boolean of
      True:  (bBlowFishKey: TBlowFishKey);
      False: (cBlowFishKey: array [0..KEY_SIZE - 1] of AnsiChar);
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
    FreeAndNil(BlowFishEncryptStream);
    FreeAndNil(Base64EncodingStream);
    FreeAndNil(StringStream);
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
    FreeAndNil(BlowFishDeCryptStream);
    FreeAndNil(Base64DecodingStream);
    FreeAndNil(StringStream);
  end;
end;

function hmac_sha3_512(AKey: PByte; AKeyLength: Integer; AMessage: AnsiString): AnsiString;
var
  HashDesc: PHashDesc;
  Buffer: THashDigest;
  Context: THMAC_Context;
begin
  HashDesc:= FindHash_by_ID(_SHA3_512);
  hmac_init({%H-}Context, HashDesc, AKey, AKeyLength);
  hmac_update(Context, Pointer(AMessage), Length(AMessage));
  hmac_final(Context, {%H-}Buffer);
  SetLength(Result, HashDesc^.HDigestlen);
  Move(Buffer[0], Result[1], HashDesc^.HDigestlen);
end;

procedure DeriveBytes(Mode: Byte; MasterKey, Salt: AnsiString; var Key; KeyLen: Int32);
begin
  if (Mode > 1) then
  begin
    argon2id_kdf(ARGON2_T, ARGON2_M, ARGON2_P,
                 Pointer(MasterKey), Length(MasterKey),
                 Pointer(Salt), Length(Salt), @Key, KeyLen);

  end
  else begin
    scrypt_kdf(Pointer(MasterKey), Length(MasterKey),
               Pointer(Salt), Length(Salt),
               SCRYPT_N, SCRYPT_R, SCRYPT_P, Key, KeyLen);
  end;
end;

function EncodeStrong(Mode: Byte; MasterKey, Data: AnsiString): AnsiString;
var
  Salt, Hash: AnsiString;
  StringStream: TStringStream = nil;
  Buffer: array[0..BUF_SIZE - 1] of Byte;
  BlowFishKey: TBlowFishKey absolute Buffer;
  BlowFishEncryptStream: TBlowFishEncryptStream = nil;
begin
  // Generate random salt
  SetLength(Salt, SizeOf(TSHA3_256Digest));
  Random(PByte(Salt), SizeOf(TSHA3_256Digest));
  // Generate encryption key
  DeriveBytes(Mode, MasterKey, Salt, {%H-}Buffer[0], SizeOf(Buffer));
  // Encrypt password using encryption key
  StringStream:= TStringStream.Create(EmptyStr);
  try
    BlowFishEncryptStream:= TBlowFishEncryptStream.Create(BlowFishKey, SizeOf(TBlowFishKey), StringStream);
    try
      BlowFishEncryptStream.Write(PAnsiChar(Data)^, Length(Data));
    finally
      BlowFishEncryptStream.Free;
    end;
    Result:= StringStream.DataString;
  finally
    StringStream.Free;
  end;
  if (Mode > 0) then
  begin
    with TDCP_rijndael.Create(nil) do
    begin
      Data:= Copy(Result, 1, Length(Result));
      Init(Buffer[AES_OFFS], GetMaxKeySize, nil);
      Encrypt(PAnsiChar(Data)^, Pointer(Result)^, Length(Data));
      Free;
    end;
  end;
  // Calculate password hash message authentication code
  Hash := hmac_sha3_512(@Buffer[KEY_SIZE], MAC_SIZE, Result);
  // Calcuate result base64 encoded string
  Result := EncodeStringBase64(Salt + Result + Copy(Hash, 1, 8));
end;

function DecodeStrong(Mode: Byte; MasterKey, Data: AnsiString): AnsiString;
var
  Salt, Hash: AnsiString;
  StringStream: TStringStream = nil;
  Buffer: array[0..BUF_SIZE - 1] of Byte;
  BlowFishKey: TBlowFishKey absolute Buffer;
  BlowFishDeCryptStream: TBlowFishDeCryptStream = nil;
begin
  Data:= DecodeStringBase64(Data);
  Hash:= Copy(Data, Length(Data) - 7, 8);
  Data:= Copy(Data, 1, Length(Data) - 8);
  Salt:= Copy(Data, 1, SizeOf(TSHA3_256Digest));
  Data:= Copy(Data, SizeOf(TSHA3_256Digest) + 1, MaxInt);
  // Generate encryption key
  DeriveBytes(Mode, MasterKey, Salt, {%H-}Buffer[0], SizeOf(Buffer));
  // Verify password using hash message authentication code
  Salt:= hmac_sha3_512(@Buffer[KEY_SIZE], MAC_SIZE, Data);
  if StrLComp(Pointer(Hash), Pointer(Salt), 8) <> 0 then
    Exit(EmptyStr);
  // Decrypt password using encryption key
  SetLength(Result, Length(Data));
  if (Mode > 0) then
  begin
    with TDCP_rijndael.Create(nil) do
    begin
      Init(Buffer[AES_OFFS], GetMaxKeySize, nil);
      Decrypt(PAnsiChar(Data)^, Pointer(Result)^, Length(Data));
      Data:= Copy(Result, 1, Length(Result));
      Free;
    end;
  end;
  StringStream:= TStringStream.Create(Data);
  try
    BlowFishDeCryptStream:= TBlowFishDeCryptStream.Create(BlowFishKey, SizeOf(TBlowFishKey), StringStream);
    try
      BlowFishDeCryptStream.Read(PAnsiChar(Result)^, Length(Result));
    finally
      BlowFishDeCryptStream.Free;
    end;
  finally
    StringStream.Free;
  end;
end;

{ TPasswordStore }

procedure TPasswordStore.ConvertStore;
var
  I, J: Integer;
  Password: String;
  Sections, Strings: TStringList;
begin
  if ReadOnly then Exit;
  Strings:= TStringList.Create;
  Sections:= TStringList.Create;
  try
    CacheUpdates:= True;
    ReadSections(Sections);
    for I:= 0 to Sections.Count - 1 do
    begin
      if not SameText(Sections[I], 'General') then
      begin
        ReadSectionValues(Sections[I], Strings);
        for J:= 0 to Strings.Count - 1 do
        begin
          Password:= Decode(FMasterKey, Strings.ValueFromIndex[J]);
          Password:= EncodeStrong(FMode, FMasterKey, Password);
          WriteString(Sections[I], Strings.Names[J], Password);
        end;
      end;
    end;
    FMasterStrong:= True;
    FMasterKeyHash:= EmptyStr;
    UpdateMasterKey(FMasterKey, FMasterKeyHash);
    WriteString('General', 'MasterKey', FMasterKeyHash);
    try
      CacheUpdates:= False;
    except
      on E: Exception do msgError(E.Message);
    end;
  finally
    Strings.Free;
    Sections.Free;
  end;
end;

procedure TPasswordStore.UpdateMasterKey(var MasterKey: AnsiString; var
  MasterKeyHash: AnsiString);
const
  RAND_SIZE = 16;
var
  Randata: AnsiString;
begin
  if not FMasterStrong then
  begin
    MasterKeyHash:= MD5Print(MD5String(MasterKey));
    MasterKeyHash:= Encode(MasterKey, MasterKeyHash);
  end
  else begin
    if Length(FMasterKeyHash) = 0 then
    begin
      SetLength(Randata, RAND_SIZE);
      Random(PByte(Randata), RAND_SIZE);
      MasterKeyHash:= '!' + IntToStr(FMode) + EncodeStrong(FMode, MasterKey, Randata);
    end
    else begin
      FMode:= StrToIntDef(Copy(FMasterKeyHash, 2, 1), FMode);
      Randata:= DecodeStrong(FMode, MasterKey, Copy(FMasterKeyHash, 3, MaxInt));
      if Length(Randata) < RAND_SIZE then
        MasterKeyHash:= EmptyStr
      else begin
        MasterKeyHash:= FMasterKeyHash;
      end;
    end;
  end;
end;

constructor TPasswordStore.Create(const AFileName: String);
begin
  inherited Create(AFileName);

  FMode:= 1;
  CacheUpdates:= False;
  if ReadOnly then DCDebug('Read only password store!');
  FMasterKeyHash:= ReadString('General', 'MasterKey', EmptyStr);
  FMasterStrong:= (Length(FMasterKeyHash) = 0) or (FMasterKeyHash[1] = '!');
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
  while (Result = False) do
  begin
    if not ShowInputQuery(rsMsgMasterPassword, rsMsgMasterPasswordEnter, True, MasterKey) then
      Exit;
    if Length(MasterKey) = 0 then Exit;
    UpdateMasterKey(MasterKey, MasterKeyHash);
    if FMasterKeyHash = EmptyStr then
      begin
        FMasterKey:= MasterKey;
        FMasterKeyHash:= MasterKeyHash;
        WriteString('General', 'MasterKey', FMasterKeyHash);
        Result:= True;
      end
    else if SameText(FMasterKeyHash, MasterKeyHash) then
      begin
        FMasterKey:= MasterKey;
        if not FMasterStrong then ConvertStore;
        Result:= True;
      end
    else
      begin
        ShowMessageBox(rsMsgWrongPasswordTryAgain, rsMtError, MB_OK or MB_ICONERROR);
      end;
  end;
end;

function TPasswordStore.WritePassword(Prefix, Name, Connection: String;
                                      const Password: AnsiString): TCryptStoreResult;
var
  Data: AnsiString;
begin
  if ReadOnly then Exit(csrWriteError);
  if CheckMasterKey = False then Exit(csrNoMasterKey);
  if not FMasterStrong then
    Data:= Encode(FMasterKey, Password)
  else begin
    Data:= EncodeStrong(FMode, FMasterKey, Password)
  end;
  if Length(Data) = 0 then Exit(csrFailed);
  try
    WriteString(Prefix + '_' + Name, Connection, Data);
  except
    Exit(csrWriteError);
  end;
  Result:= csrSuccess;
end;

function TPasswordStore.ReadPassword(Prefix, Name, Connection: String;
                                     out Password: AnsiString): TCryptStoreResult;
var
  Data: AnsiString = '';
begin
  if CheckMasterKey = False then Exit(csrNoMasterKey);
  Data:= ReadString(Prefix + '_' + Name, Connection, Data);
  if Length(Data) = 0 then Exit(csrNotFound);
  if not FMasterStrong then
    Password:= Decode(FMasterKey, Data)
  else begin
    Password:= DecodeStrong(FMode, FMasterKey, Data)
  end;
  if Length(Password) = 0 then
    Result:= csrFailed
  else begin
    Result:= csrSuccess;
  end;
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

finalization
  FreeAndNil(PasswordStore);

end.

