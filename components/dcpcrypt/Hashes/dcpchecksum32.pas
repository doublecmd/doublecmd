{******************************************************************************}
{* Simple 32-bits checksum class integrated in existing              **********}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of simple 32-bits checksum              *}
{******************************************************************************}
{* Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)                    *}
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
unit dcpchecksum32;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, DCPcrypt2, DCPconst;

type

  { TDCP_checksum32 }

  TDCP_checksum32 = class(TDCP_hash)
  protected
    CurrentHash: DWORD;
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

{ TDCP_checksum32 }

{ TDCP_checksum32.GetHashSize }
class function TDCP_checksum32.GetHashSize: integer;
begin
  Result:= 32;
end;

{ TDCP_checksum32.GetId }
class function TDCP_checksum32.GetId: integer;
begin
  Result:= DCP_checksum32;
end;

{ TDCP_checksum32.GetAlgorithm }
class function TDCP_checksum32.GetAlgorithm: string;
begin
  Result:= 'CHECKSUM32';
end;

{ TDCP_checksum32.SelfTest }
class function TDCP_checksum32.SelfTest: boolean;
const
  Test1Out: array[0..3] of byte=($00, $00, $01, $26); //Verified on 2021-08-24
  Test2Out: array[0..3] of byte=($00, $00, $0B, $1F);
var
  TestHash: TDCP_checksum32;
  TestOut: array[0..3] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_checksum32.Create(nil);
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

{ TDCP_checksum32.Create }
constructor TDCP_checksum32.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{ TDCP_checksum32.Init }
procedure TDCP_checksum32.Init;
begin
  Burn;
  CurrentHash:= 0;
  fInitialized:= true;
end;

{ TDCP_checksum32 }
procedure TDCP_checksum32.Burn;
begin
  CurrentHash:= 0;
  fInitialized:= false;
end;

{ TDCP_checksum32.Update }
{$PUSH}{$R-}{$Q-}{$OPTIMIZATION LEVEL3} // no range, no overflow checks, optimize for speed (not size)
procedure TDCP_checksum32.Update(const Buffer; Size: longword);
var
  data: PByte;
  iIndex: longword;
  iChecksumLocal: DWORD;
begin
  iChecksumLocal := CurrentHash; //Manipulating the copy "iChecksumLocal" in the loop is overall faster then working directly with property "CurrentHash".
  data := @Buffer;
  for iIndex := 1 to Size do
  begin
    iChecksumLocal := iChecksumLocal + data^;
    inc(data);
  end;
  CurrentHash := iChecksumLocal;
end;
{$POP}

{ TDCP_checksum32.Final }
procedure TDCP_checksum32.Final(var Digest);
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  CurrentHash:= SwapEndian(CurrentHash);
  Move(CurrentHash, Digest, Sizeof(CurrentHash));
  Burn;
end;

end.

