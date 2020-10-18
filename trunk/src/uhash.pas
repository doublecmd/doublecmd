{
    Double Commander
    -------------------------------------------------------------------------
    General Hash Unit: This unit defines the common types, functions,
    and procedures

    Copyright (C) 2009-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uHash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPcrypt2, crc;

type
  THashContext   = TDCP_hash;
  THashAlgorithm = (HASH_BLAKE2S, HASH_BLAKE2SP, HASH_BLAKE2B, HASH_BLAKE2BP, HASH_BLAKE3,
                    HASH_CRC32, HASH_HAVAL, HASH_MD4, HASH_MD5, HASH_RIPEMD128, HASH_RIPEMD160,
                    HASH_SFV, HASH_SHA1, HASH_SHA224, HASH_SHA256, HASH_SHA384, HASH_SHA512,
                    HASH_SHA3_224, HASH_SHA3_256, HASH_SHA3_384, HASH_SHA3_512, HASH_TIGER,
                    HASH_BEST
                    );

var
  HashFileExt: array[Low(THashAlgorithm)..Pred(High(THashAlgorithm))] of String = (
                 'blake2s', 'blake2sp', 'blake2b', 'blake2bp', 'blake3', 'crc32', 'haval',
                 'md4', 'md5', 'ripemd128', 'ripemd160', 'sfv', 'sha', 'sha224', 'sha256',
                 'sha384', 'sha512', 'sha3', 'sha3', 'sha3', 'sha3', 'tiger'
               );

var
  HashName: array[Low(THashAlgorithm)..Pred(High(THashAlgorithm))] of String = (
                 'blake2s', 'blake2sp', 'blake2b', 'blake2bp', 'blake3', 'crc32', 'haval',
                 'md4', 'md5', 'ripemd128', 'ripemd160', 'sfv', 'sha1_160', 'sha2_224',
                 'sha2_256', 'sha2_384', 'sha2_512', 'sha3_224', 'sha3_256',
                 'sha3_384', 'sha3_512', 'tiger'
               );

procedure HashInit(out Context: THashContext; Algorithm: THashAlgorithm);
procedure HashUpdate(var Context: THashContext; const Buffer; BufLen: LongWord);
procedure HashFinal(var Context: THashContext; out Hash: String);

function HashString(const Line: String; IgnoreCase, IgnoreWhiteSpace: Boolean): LongWord;

{ Helper functions }
function FileExtIsHash(const FileExt: String): Boolean;
function FileExtToHashAlg(const FileExt: String): THashAlgorithm;

implementation

uses
  LazUTF8, DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160, DCPcrc32,
  DCPsha1, DCPsha256, DCPsha512, DCPtiger, DCPblake2, DCPblake3, DCPsha3;

procedure HashInit(out Context: THashContext; Algorithm: THashAlgorithm);
begin
{$PUSH}{$WARNINGS OFF}
  if (Algorithm = HASH_BEST) then
  begin
    if SizeOf(UIntPtr) = Sizeof(UInt64) then
      Algorithm:= HASH_BLAKE2B
    else
      Algorithm:= HASH_BLAKE2S;
  end;
{$POP}
  case Algorithm of
    HASH_BLAKE2S:    Context:= TDCP_blake2s.Create(nil);
    HASH_BLAKE2SP:   Context:= TDCP_blake2sp.Create(nil);
    HASH_BLAKE2B:    Context:= TDCP_blake2b.Create(nil);
    HASH_BLAKE2BP:   Context:= TDCP_blake2bp.Create(nil);
    HASH_BLAKE3:     Context:= TDCP_blake3.Create(nil);
    HASH_CRC32:      Context:= TDCP_crc32.Create(nil);
    HASH_HAVAL:      Context:= TDCP_haval.Create(nil);
    HASH_MD4:        Context:= TDCP_md4.Create(nil);
    HASH_MD5:        Context:= TDCP_md5.Create(nil);
    HASH_RIPEMD128:  Context:= TDCP_ripemd128.Create(nil);
    HASH_RIPEMD160:  Context:= TDCP_ripemd160.Create(nil);
    HASH_SFV:        Context:= TDCP_crc32.Create(nil);
    HASH_SHA1:       Context:= TDCP_sha1.Create(nil);
    HASH_SHA224:     Context:= TDCP_sha224.Create(nil);
    HASH_SHA256:     Context:= TDCP_sha256.Create(nil);
    HASH_SHA384:     Context:= TDCP_sha384.Create(nil);
    HASH_SHA512:     Context:= TDCP_sha512.Create(nil);
    HASH_SHA3_224:   Context:= TDCP_sha3_224.Create(nil);
    HASH_SHA3_256:   Context:= TDCP_sha3_256.Create(nil);
    HASH_SHA3_384:   Context:= TDCP_sha3_384.Create(nil);
    HASH_SHA3_512:   Context:= TDCP_sha3_512.Create(nil);
    HASH_TIGER:      Context:= TDCP_tiger.Create(nil);
  end;
  Context.Init;
end;

procedure HashUpdate(var Context: THashContext; const Buffer; BufLen: LongWord);
begin
  Context.Update(Buffer, BufLen);
end;

procedure HashFinal(var Context: THashContext; out Hash: String);
var
  I, HashSize: LongWord;
  Digest: array of Byte;
begin
  Hash:= EmptyStr;
  HashSize:= Context.HashSize div 8;
  SetLength(Digest, HashSize);
  Context.Final(Pointer(Digest)^);
  for I := 0 to HashSize - 1 do
    Hash := Hash + HexStr(Digest[I], 2);
  Hash := LowerCase(Hash);
  FreeAndNil(Context);
end;

function HashString(const Line: String; IgnoreCase, IgnoreWhiteSpace: Boolean): LongWord;
var
  S: String;
  I, J, L: Integer;
begin
  S := Line;
  if IgnoreWhiteSpace then
  begin
    J := 1;
    L := Length(Line);
    for I:= 1 to L do
    begin
      // Skip white spaces
      if not (Line[I] in [#9, #32]) then
      begin
        S[J] := Line[I];
        Inc(J);
      end;
    end;
    SetLength(S, J - 1);
  end;
  if IgnoreCase then S := UTF8LowerCase(S);

  Result := crc32(0, nil, 0);
  Result := crc32(Result, PByte(S), Length(S));
end;

function FileExtIsHash(const FileExt: String): Boolean;
var
  I: THashAlgorithm;
begin
  Result:= False;
  for I:= Low(HashFileExt) to High(HashFileExt) do
  begin
    if SameText(FileExt, HashFileExt[I]) then Exit(True);
  end;
end;

function FileExtToHashAlg(const FileExt: String): THashAlgorithm;
var
  I: THashAlgorithm;
begin
  for I:= Low(HashFileExt) to High(HashFileExt) do
  begin
    if SameText(FileExt, HashFileExt[I]) then Exit(I);
  end;
end;

end.

