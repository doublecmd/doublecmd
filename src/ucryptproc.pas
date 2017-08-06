{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains Encrypt/Decrypt classes and functions.

    Copyright (C) 2009-2017 Alexander Koblov (alexx2000@mail.ru)

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
    FMasterStrong: Boolean;
    FMasterKey: AnsiString;
    FMasterKeyHash: AnsiString;
  private
    procedure UpdateMasterKey(var MasterKey: AnsiString; var MasterKeyHash: AnsiString);
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

procedure InitPasswordStore;

var
  PasswordStore: TPasswordStore = nil;

implementation

uses
  LCLProc, LCLType, Base64, BlowFish, Math, MD5, DCPcrypt2, DCPsha3, HMAC,
  SHA3_256, Hash, uShowMsg, uGlobsPaths, uLng, uDebug, uHash, uRandom;

const
  HMAC_COUNT = 8192;

type
  TBlowFishKeyRec = record
    dwSize: LongWord;
    case Boolean of
      True: (bBlowFishKey: TBlowFishKey);
      False: (cBlowFishKey: array [0..SizeOf(TBlowFishKey) - 1] of AnsiChar);
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

procedure pbkdf2_hmac_sha3_256(const Password, Salt: AnsiString; Key: PByte; KeyLength, IterationCount: Integer);
var
  I, J, K: Integer;
  BlockCnt: Integer;
  HashDesc: PHashDesc;
  Buffer: THashDigest;
  Xuffer: TSHA3_256Digest;
  Ctx1, Ctx2, Ctx3: THMAC_Context;
begin
  HashDesc:= FindHash_by_Name('SHA3-256');
  // Init HMAC context (with password)
  hmac_init(Ctx1, HashDesc, PByte(Password), Length(Password));
  // Prepare HMAC context (with password and salt)
  Move(Ctx1, Ctx2, SizeOf(THMAC_Context));
  hmac_update(Ctx2, Pointer(Salt), Length(Salt));
  // Calculate the number of SHA3-256 blocks in the key
  BlockCnt := ceil(KeyLength / HashDesc^.HDigestlen);
  // Process each key block
  for I := 1 to BlockCnt do
  begin
    // Prepare HMAC context (with password and salt)
    Move(Ctx2, Ctx3, SizeOf(THMAC_Context));
    ZeroMemory(@Xuffer[0], HashDesc^.HDigestlen);
    // Init first block data
    K:= 4;
    PLongWord(@Buffer[0])^:= NtoBE(I);
    // Start iteration
    for J := 1 to IterationCount do
    begin
      hmac_update(Ctx3, @Buffer[0], K);
      hmac_final(Ctx3, Buffer);
      XorBlockEx(Xuffer[0], Buffer[0], HashDesc^.HDigestlen);
      // Prepare HMAC context (with password)
      Move(Ctx1, Ctx3, SizeOf(THMAC_Context));
      // Update buffer length
      K:= HashDesc^.HDigestlen;
    end;
    // Merge key block into the result key
    K:= (I - 1) * HashDesc^.HDigestlen;
    if (I = BlockCnt) then
      Move(Xuffer[0], Key[K], KeyLength - K)
    else begin
      Move(Xuffer[0], Key[K], HashDesc^.HDigestlen);
    end;
  end;
end;

function EncodeStrong(MasterKey, Salt, Data: AnsiString): AnsiString;
var
  Checksum: LongWord;
  BlowFishKey: TBlowFishKey;
  StringStream: TStringStream = nil;
  Hash: array[0..3] of AnsiChar absolute Checksum;
  BlowFishEncryptStream: TBlowFishEncryptStream = nil;
begin
  pbkdf2_hmac_sha3_256(MasterKey, Salt, BlowFishKey, SizeOf(TBlowFishKey), HMAC_COUNT);
  StringStream:= TStringStream.Create(EmptyStr);
  try
    BlowFishEncryptStream:= TBlowFishEncryptStream.Create(BlowFishKey, SizeOf(TBlowFishKey), StringStream);
    try
      BlowFishEncryptStream.Write(PAnsiChar(Data)^, Length(Data));
    finally
      BlowFishEncryptStream.Free;
    end;
    Result:= Salt + StringStream.DataString;
  finally
    StringStream.Free;
  end;
  Checksum:= HashString(Result, False, False);
  Result := EncodeStringBase64(Result + Hash);
end;

function EncodeStrong(MasterKey, Data: AnsiString): AnsiString;
var
  Salt: AnsiString;
begin
  SetLength(Salt, SizeOf(TSHA3_256Digest));
  Random(PByte(Salt), SizeOf(TSHA3_256Digest));
  Result:= EncodeStrong(MasterKey, Salt, Data);
end;

function DecodeStrong(MasterKey, Data: AnsiString): AnsiString;
var
  Salt: AnsiString;
  Checksum: LongWord;
  BlowFishKey: TBlowFishKey;
  StringStream: TStringStream = nil;
  Hash: array[0..3] of AnsiChar absolute Checksum;
  BlowFishDeCryptStream: TBlowFishDeCryptStream = nil;
begin
  Data:= DecodeStringBase64(Data);
  Hash:= Copy(Data, Length(Data) - 3, 4);
  Data:= Copy(Data, 1, Length(Data) - 4);
  // Verify password data checksum
  if (Checksum <> HashString(Data, False, False)) then
    Exit(EmptyStr);
  Salt:= Copy(Data, 1, SizeOf(TSHA3_256Digest));
  Data:= Copy(Data, SizeOf(TSHA3_256Digest) + 1, MaxInt);
  pbkdf2_hmac_sha3_256(MasterKey, Salt, BlowFishKey, SizeOf(TBlowFishKey), HMAC_COUNT);
  SetLength(Result, Length(Data));
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

procedure TPasswordStore.UpdateMasterKey(var MasterKey: AnsiString; var
  MasterKeyHash: AnsiString);
var
  I: Integer;
  HashTemp: AnsiString;
  MasterSalt: AnsiString;
begin
  if not FMasterStrong then
  begin
    MasterKeyHash:= MD5Print(MD5String(MasterKey));
    MasterKeyHash:= Encode(MasterKey, MasterKeyHash);
  end
  else begin
    // Get master salt
    if Length(FMasterKeyHash) = 0 then
    begin
      SetLength(MasterSalt, SizeOf(TSHA3_256Digest));
      Random(PByte(MasterSalt), SizeOf(TSHA3_256Digest));
    end
    else begin
      MasterSalt:= DecodeStringBase64(Copy(FMasterKeyHash, 2, MaxInt));
      MasterSalt:= Copy(MasterSalt, 1, SizeOf(TSHA3_256Digest));
    end;
    // Calculate master key hash #1
    HashTemp:= MasterKey + MasterSalt;
    for I:= 1 to 4 do
    begin
      with TDCP_sha3_512.Create(nil) do
      begin
        Init;
        UpdateStr(HashTemp);
        SetLength(HashTemp, HashSize div 8);
        Final(HashTemp[1]);
        Free;
      end;
      SetLength(HashTemp, Length(HashTemp) div 2);
    end;
    // Calculate master key hash #2
    SetLength(MasterKeyHash, SizeOf(TSHA3_256Digest));
    pbkdf2_hmac_sha3_256(HashTemp, MasterSalt, PByte(MasterKeyHash), SizeOf(TSHA3_256Digest), HMAC_COUNT);
    // Calculate final master key hash
    MasterKeyHash:= '!' + EncodeStrong(MasterKey, MasterSalt, MasterKeyHash);
  end;
end;

constructor TPasswordStore.Create(const AFileName: String);
begin
  inherited Create(AFileName);

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
  if not FMasterStrong then
    Data:= Encode(FMasterKey, Password)
  else begin
    Data:= EncodeStrong(FMasterKey, Password)
  end;
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
  if not FMasterStrong then
    Password:= Decode(FMasterKey, Data)
  else begin
    Password:= DecodeStrong(FMasterKey, Data)
  end;
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

