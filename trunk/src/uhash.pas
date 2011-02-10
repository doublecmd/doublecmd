{
    Double Commander
    -------------------------------------------------------------------------
    General Hash Unit: This unit defines the common types, functions,
    and procedures

    Copyright (C) 2009-2010  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, md5, sha, crc;

type
  THashAlgorithm = (HASH_MD5, HASH_SHA1, HASH_SHA256, HASH_SHA384, HASH_SHA512);
  THashContext = record
    HashContext: Pointer;
    HashAlgorithm: THashAlgorithm;
  end;

var
  HashFileExt: array[THashAlgorithm] of String = ('md5', 'sha', 'sha256', 'sha384', 'sha512');

procedure HashInit(out Context: THashContext; const Algorithm: THashAlgorithm);
procedure HashUpdate(var Context: THashContext; var Buf; const BufLen: PtrUInt);
procedure HashFinal(var Context: THashContext; out Hash: String);

function HashString(const Line: String; IgnoreCase, IgnoreWhiteSpace: Boolean): Pointer;

{ Helper functions }
function FileExtIsHash(const FileExt: String): Boolean;
function FileExtToHashAlg(const FileExt: String): THashAlgorithm;

implementation

procedure HashInit(out Context: THashContext; const Algorithm: THashAlgorithm);
begin
  case Algorithm of
    HASH_MD5:
      begin
        GetMem(Context.HashContext, SizeOf(TMD5Context));
        MD5Init(PMD5Context(Context.HashContext)^);
        Context.HashAlgorithm:= Algorithm;
      end;
    HASH_SHA1:
      begin
        GetMem(Context.HashContext, SizeOf(TSHA1Context));
        SHA1Init(PSHA1Context(Context.HashContext)^);
        Context.HashAlgorithm:= Algorithm;
      end;
    HASH_SHA256:
      begin
        GetMem(Context.HashContext, SizeOf(TSHA256Context));
        SHA256Init(PSHA256Context(Context.HashContext)^);
        Context.HashAlgorithm:= Algorithm;
      end;
    HASH_SHA384:
      begin
        GetMem(Context.HashContext, SizeOf(TSHA384Context));
        SHA384Init(PSHA384Context(Context.HashContext)^);
        Context.HashAlgorithm:= Algorithm;
      end;
    HASH_SHA512:
      begin
        GetMem(Context.HashContext, SizeOf(TSHA512Context));
        SHA512Init(PSHA512Context(Context.HashContext)^);
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
    HASH_SHA256:
      SHA256Update(PSHA256Context(Context.HashContext)^, Buf, BufLen);
    HASH_SHA384:
      SHA384Update(PSHA384Context(Context.HashContext)^, Buf, BufLen);
    HASH_SHA512:
      SHA512Update(PSHA512Context(Context.HashContext)^, Buf, BufLen);
  end;
end;

procedure HashFinal(var Context: THashContext; out Hash: String);
begin
  case Context.HashAlgorithm of
    HASH_MD5:
      begin
        MD5Final(PMD5Context(Context.HashContext)^, Hash);
        FreeMem(Context.HashContext, SizeOf(TMD5Context));
      end;
    HASH_SHA1:
      begin
        SHA1Final(PSHA1Context(Context.HashContext)^, Hash);
        FreeMem(Context.HashContext, SizeOf(TSHA1Context));
      end;
    HASH_SHA256:
      begin
        SHA256Final(PSHA256Context(Context.HashContext)^, Hash);
        FreeMem(Context.HashContext, SizeOf(TSHA256Context));
      end;
    HASH_SHA384:
      begin
        SHA384Final(PSHA384Context(Context.HashContext)^, Hash);
        FreeMem(Context.HashContext, SizeOf(TSHA384Context));
      end;
    HASH_SHA512:
      begin
        SHA512Final(PSHA512Context(Context.HashContext)^, Hash);
        FreeMem(Context.HashContext, SizeOf(TSHA512Context));
      end;
  end;
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

