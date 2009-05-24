{
    Double Commander
    -------------------------------------------------------------------------
    General Hash Unit: This unit defines the common types, functions,
    and procedures

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

unit uHash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5, sha1;

type
  THashAlgorithm = (HASH_MD5, HASH_SHA1);
  THashDigest = array of Byte;
  THashContext = record
    HashContext: Pointer;
    HashAlgorithm: THashAlgorithm;
  end;

var
  HashFileExt: array[THashAlgorithm] of String = ('md5', 'sha');

procedure HashInit(var Context: THashContext; const Algorithm: THashAlgorithm);
procedure HashUpdate(var Context: THashContext; var Buf; const BufLen: PtrUInt);
procedure HashFinal(var Context: THashContext; var Digest: THashDigest);
function HashPrint(const Digest: THashDigest): String;

implementation

procedure HashInit(var Context: THashContext; const Algorithm: THashAlgorithm);
var
  MD5Context: PMDContext;
  SHA1Context: PSHA1Context;
begin
  case Algorithm of
    HASH_MD5:
      begin
        New(MD5Context);
        MD5Init(MD5Context^);
        Context.HashContext:= MD5Context;
        Context.HashAlgorithm:= Algorithm;
      end;
    HASH_SHA1:
      begin
        New(SHA1Context);
        SHA1Init(SHA1Context^);
        Context.HashContext:= SHA1Context;
        Context.HashAlgorithm:= Algorithm;
      end;
  end;
end;

procedure HashUpdate(var Context: THashContext; var Buf; const BufLen: PtrUInt);
begin
  case Context.HashAlgorithm of
    HASH_MD5:
      MD5Update(PMD5Context(Context.HashContext)^, Buf, BufLen);
    HASH_SHA1:
      SHA1Update(PSHA1Context(Context.HashContext)^, Buf, BufLen);
  end;
end;

procedure HashFinal(var Context: THashContext; var Digest: THashDigest);
var
  MD5Digest: TMD5Digest;
  SHA1Digest: TSHA1Digest;
  I: Integer;
begin
  case Context.HashAlgorithm of
    HASH_MD5:
      begin
        MD5Final(PMD5Context(Context.HashContext)^, MD5Digest);
        SetLength(Digest, SizeOf(MD5Digest));
        for I:= Low(MD5Digest) to High(MD5Digest) do
          Digest[I]:= MD5Digest[I];
        Dispose(PMD5Context(Context.HashContext));
      end;
    HASH_SHA1:
      begin
        SHA1Final(PSHA1Context(Context.HashContext)^, SHA1Digest);
        SetLength(Digest, SizeOf(SHA1Digest));
        for I:= Low(SHA1Digest) to High(SHA1Digest) do
          Digest[I]:= SHA1Digest[I];
        Dispose(PSHA1Context(Context.HashContext));
      end;
  end;
end;

function HashPrint(const Digest: THashDigest): String;
var
  I: Byte;
begin
  Result:= '';
  for I:= Low(Digest) to High(Digest) do
    Result:= Result + HexStr(Digest[I], 2);
  Result:= LowerCase(Result);
end;

end.

