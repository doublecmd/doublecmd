{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible (with Total Commander) implementation of CRC32         *}
{******************************************************************************}
{* Copyright (C) 2011-2015 Alexander Koblov (alexx2000@mail.ru)               *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit DCPcrc32;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCcrc32;

type

  { TDCP_crc32 }

  TDCP_crc32 = class(TDCP_hash)
  protected
    CurrentHash: LongWord;
  public
    class function GetId: integer; override;
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    constructor Create(AOwner: TComponent); override;
    procedure Init; override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longword); override;
    procedure Final(var Digest); override;
  end;

implementation
{$R-}{$Q-}

{ TDCP_crc32 }

class function TDCP_crc32.GetHashSize: integer;
begin
  Result:= 32;
end;

class function TDCP_crc32.GetId: integer;
begin
  Result:= DCP_crc32;
end;

class function TDCP_crc32.GetAlgorithm: string;
begin
  Result:= 'CRC32';
end;

class function TDCP_crc32.SelfTest: boolean;
const
  Test1Out: array[0..3] of byte=($35,$24,$41,$C2);
  Test2Out: array[0..3] of byte=($4C,$27,$50,$BD);
var
  TestHash: TDCP_crc32;
  TestOut: array[0..3] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_crc32.Create(nil);
  TestHash.Init;
  TestHash.UpdateStr('abc');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  TestHash.Init;
  TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
  TestHash.Free;
end;

constructor TDCP_crc32.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDCP_crc32.Init;
begin
  Burn;
  crc32_init;
  CurrentHash:= 0;
  fInitialized:= true;
end;

procedure TDCP_crc32.Burn;
begin
  CurrentHash:= 0;
  fInitialized:= false;
end;

procedure TDCP_crc32.Update(const Buffer; Size: longword);
var
  Bytes: PByte;
begin
  Bytes:= @Buffer;
  CurrentHash:= crc32_16bytes(Bytes, Size, CurrentHash);
end;

procedure TDCP_crc32.Final(var Digest);
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  CurrentHash:= SwapEndian(CurrentHash);
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

end.
