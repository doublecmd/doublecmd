{
    Double Commander
    -------------------------------------------------------------------------
    General Hash Unit: This unit defines the common types, functions,
    and procedures

    Copyright (C) 2009-2011  Koblov Alexander (Alexx2000@mail.ru)

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
  THashAlgorithm = (HASH_HAVAL, HASH_MD4, HASH_MD5, HASH_RIPEMD128, HASH_RIPEMD160,
                    HASH_SHA1, HASH_SHA256, HASH_SHA384, HASH_SHA512, HASH_TIGER);

var
  HashFileExt: array[THashAlgorithm] of String = (
                 'haval', 'md4', 'md5', 'ripemd128', 'ripemd160',
                 'sha', 'sha256', 'sha384', 'sha512', 'tiger'
               );

procedure HashInit(out Context: THashContext; const Algorithm: THashAlgorithm);
procedure HashUpdate(var Context: THashContext; const Buffer; BufLen: LongWord);
procedure HashFinal(var Context: THashContext; out Hash: String);

function HashString(const Line: String; IgnoreCase, IgnoreWhiteSpace: Boolean): Pointer;

{ Helper functions }
function FileExtIsHash(const FileExt: String): Boolean;
function FileExtToHashAlg(const FileExt: String): THashAlgorithm;

implementation

uses
  DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160, DCPsha1, DCPsha256,
  DCPsha512, DCPtiger;

procedure HashInit(out Context: THashContext; const Algorithm: THashAlgorithm);
begin
  case Algorithm of
    HASH_HAVAL:      Context:= TDCP_haval.Create(nil);
    HASH_MD4:        Context:= TDCP_md4.Create(nil);
    HASH_MD5:        Context:= TDCP_md5.Create(nil);
    HASH_RIPEMD128:  Context:= TDCP_ripemd128.Create(nil);
    HASH_RIPEMD160:  Context:= TDCP_ripemd160.Create(nil);
    HASH_SHA1:       Context:= TDCP_sha1.Create(nil);
    HASH_SHA256:     Context:= TDCP_sha256.Create(nil);
    HASH_SHA384:     Context:= TDCP_sha384.Create(nil);
    HASH_SHA512:     Context:= TDCP_sha512.Create(nil);
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

function HashString(const Line: String; IgnoreCase, IgnoreWhiteSpace: Boolean): Pointer;
var
  CRC: LongWord;
  I, J, L: Integer;
  S: String;
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
  if IgnoreCase then S := AnsiLowerCase(S);

  CRC := crc32(0, nil, 0);
  // Return result as a pointer to save typecasting later...
  Result := Pointer(PtrUInt(crc32(CRC, PByte(S), Length(S))));
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

