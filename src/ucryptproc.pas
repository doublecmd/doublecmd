{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains Encrypt/Decrypt classes and functions.

    Copyright (C) 2009-2025 Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, DCClassesUtf8, Argon2;

type

  { TCryptStoreResult }

  TCryptStoreResult = (
     csrSuccess,    // Success
     csrFailed,     // Encrypt/Decrypt failed
     csrWriteError, // Could not write password to password store
     csrNotFound,   // Password not found in password store
     csrNoMasterKey // No master password entered yet
  );

  { TArgon2Params }

  TArgon2Params = packed record
    A: Targon2_type;
    case Integer of
      0 : ( Value: Int64 );
      1 : ( M: UInt32; T, P: UInt16; );
  end;

  { TPasswordStore }

  TPasswordStore = class(TIniFileEx)
  private
    FMode: Byte;
    FArgon2: TArgon2Params;
    FMasterKey: AnsiString;
    FMasterKeyHash: AnsiString;
  private
    procedure ConvertStore;
    procedure LoadParameters;
    procedure SaveParameters;
    function EncodeStrong(Mode: Byte; MasterKey, Data: AnsiString): AnsiString;
    function DecodeStrong(Mode: Byte; MasterKey, Data: AnsiString): AnsiString;
    procedure UpdateMasterKey(var MasterKey: AnsiString; var MasterKeyHash: AnsiString);
    procedure DeriveBytes(Mode: Byte; MasterKey, Salt: AnsiString; var Key; KeyLen: Int32);
  public
    constructor Create(const AFileName: String); reintroduce;
  public
    function MasterKeySet: Boolean;
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
  Math, LCLType, LCLStrConsts, Base64, BlowFish, HMAC, SCRYPT, SHA3_512,
  Hash, DCPrijndael, uShowMsg, uGlobsPaths, uLng, uDebug, uRandom, fMasterKey;

const
  KDF_MODE = 2;

const
  SCRYPT_N = (1 shl 14);
  SCRYPT_R = 8;
  SCRYPT_P = 1;

const
  ARGON2_M = (1 shl 16);
  ARGON2_T = 2;
  ARGON2_P = 2;

const
  AES_OFFS = 12; // (56 - 32) / 2
  KEY_SIZE = SizeOf(TBlowFishKey);
  MAC_SIZE = SizeOf(TSHA3_256Digest);
  BUF_SIZE = KEY_SIZE + MAC_SIZE;

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

procedure TPasswordStore.DeriveBytes(Mode: Byte; MasterKey, Salt: AnsiString; var Key; KeyLen: Int32);
var
  Res: Integer;
begin
  if (Mode > 1) then
  begin
    Res:= argon2_kdf(FArgon2.T, FArgon2.M, FArgon2.P,
                     Pointer(MasterKey), Length(MasterKey),
                     Pointer(Salt), Length(Salt), @Key, KeyLen, FArgon2.A);

  end
  else begin
    Res:= scrypt_kdf(Pointer(MasterKey), Length(MasterKey),
                     Pointer(Salt), Length(Salt),
                     SCRYPT_N, SCRYPT_R, SCRYPT_P, Key, KeyLen);
  end;
  if (Res < 0) then begin
    raise Exception.CreateFmt(rsMsgKeyTransformError, [Res]);
  end;
end;

function TPasswordStore.EncodeStrong(Mode: Byte; MasterKey, Data: AnsiString): AnsiString;
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

function TPasswordStore.DecodeStrong(Mode: Byte; MasterKey, Data: AnsiString): AnsiString;
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
  if (FMode < 2) then
  begin
    with FArgon2 do begin
      if not CreateMasterKey(True, Password, A, M, T, P) then
        Exit;
    end;
  end;
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
          Password:= DecodeStrong(FMode, FMasterKey, Strings.ValueFromIndex[J]);
          Password:= EncodeStrong(KDF_MODE, FMasterKey, Password);
          WriteString(Sections[I], Strings.Names[J], Password);
        end;
      end;
    end;
    FMode:= KDF_MODE;
    FMasterKeyHash:= EmptyStr;
    UpdateMasterKey(FMasterKey, FMasterKeyHash);
    SaveParameters;
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

procedure TPasswordStore.LoadParameters;
const
  ARGON2_TYP: array[0..1] of Targon2_type = (Argon2_d, Argon2_id);
var
  ATemp: TArgon2Params;
begin
  FMasterKeyHash:= ReadString('General', 'MasterKey', EmptyStr);
  if (Length(FMasterKeyHash) > 0) then
  begin
    ATemp.Value:= ReadInt64('General', 'Parameters', 0);
    // Validate parameters
    FArgon2.A:= ARGON2_TYP[ATemp.M >> 31];
    FArgon2.T:= Min(64, Max(ATemp.T, ARGON2_T));
    FArgon2.P:= Min(64, Max(ATemp.P, ARGON2_P));
    FArgon2.M:= Min(2048, Max(ATemp.M and $7FFFFFFF, ARGON2_M));
  end;
end;

procedure TPasswordStore.SaveParameters;
const
  ARGON2_TYP: array[Targon2_type] of UInt32 = (0, 0, 1);
var
  ATemp: TArgon2Params;
begin
  ATemp:= FArgon2;
  ATemp.M:= ATemp.M or (ARGON2_TYP[FArgon2.A] << 31);
  WriteString('General', 'MasterKey', FMasterKeyHash);
  WriteString('General', 'Parameters', '$' + HexStr(ATemp.Value, 16));
end;

constructor TPasswordStore.Create(const AFileName: String);
begin
  FMode:= KDF_MODE;
  inherited Create(AFileName);

  LoadParameters;
  CacheUpdates:= False;
  if ReadOnly then DCDebug('Read only password store!');
end;

function TPasswordStore.MasterKeySet: Boolean;
begin
  Result:= (Length(FMasterKeyHash) <> 0);
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
    if Length(FMasterKeyHash) > 0 then
    begin
      if not ShowInputQuery(rsMsgMasterPassword, rsMsgMasterPasswordEnter, True, MasterKey) then
        Exit;
    end
    else begin
      if not CreateMasterKey(False, MasterKey, FArgon2.A, FArgon2.M, FArgon2.T, FArgon2.P) then
        Exit;
    end;
    if Length(MasterKey) = 0 then Exit;
    UpdateMasterKey(MasterKey, MasterKeyHash);
    if FMasterKeyHash = EmptyStr then
      begin
        FMasterKey:= MasterKey;
        FMasterKeyHash:= MasterKeyHash;
        SaveParameters;
        Result:= True;
      end
    else if SameText(FMasterKeyHash, MasterKeyHash) then
      begin
        FMasterKey:= MasterKey;
        if (FMode < KDF_MODE) then ConvertStore;
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
  if CheckMasterKey = False then Exit(csrFailed);
  Data:= EncodeStrong(FMode, FMasterKey, Password);
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
  if CheckMasterKey = False then Exit(csrFailed);
  Data:= ReadString(Prefix + '_' + Name, Connection, Data);
  if Length(Data) = 0 then Exit(csrNotFound);
  Password:= DecodeStrong(FMode, FMasterKey, Data);
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

