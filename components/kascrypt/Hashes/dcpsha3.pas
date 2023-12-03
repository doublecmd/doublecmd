{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of SHA3 (224, 256, 384, 512) ************}
{******************************************************************************}
{* Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)                    *}
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
unit DCPsha3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPcrypt2, DCPconst, SHA3;

type

  { TDCP_sha3base }

  TDCP_sha3base = class(TDCP_hash)
  protected
    FState: TSHA3State;
  public
    procedure Final(var Digest); override;
    procedure Update(const Buffer; Size: longword); override;
  end;

  { TDCP_sha3_224 }

  TDCP_sha3_224 = class(TDCP_sha3base)
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
  end;

  { TDCP_sha3_256 }

  TDCP_sha3_256 = class(TDCP_sha3base)
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
  end;

  { TDCP_sha3_384 }

  TDCP_sha3_384 = class(TDCP_sha3base)
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
  end;

  { TDCP_sha3_512 }

  TDCP_sha3_512 = class(TDCP_sha3base)
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
  end;

implementation

{ TDCP_sha3base }

procedure TDCP_sha3base.Final(var Digest);
begin
  SHA3_FinalBit_LSB(FState, 0, 0, @Digest, FState.fixedOutputLength);
end;

procedure TDCP_sha3base.Update(const Buffer; Size: longword);
begin
  SHA3_UpdateXL(FState, @Buffer, Size);
end;

{ TDCP_sha3_224 }

class function TDCP_sha3_224.GetAlgorithm: string;
begin
  Result:= 'SHA3_224';
end;

class function TDCP_sha3_224.GetHashSize: integer;
begin
  Result:= 224;
end;

class function TDCP_sha3_224.SelfTest: boolean;
begin
  Result:= False; // TODO: SelfTest SHA3_224
end;

procedure TDCP_sha3_224.Init;
begin
  SHA3_Init(FState, __SHA3_224);
end;

{ TDCP_sha3_256 }

class function TDCP_sha3_256.GetAlgorithm: string;
begin
  Result:= 'SHA3_256';
end;

class function TDCP_sha3_256.GetHashSize: integer;
begin
  Result:= 256;
end;

class function TDCP_sha3_256.SelfTest: boolean;
begin
  Result:= False; // TODO: SelfTest SHA3_256
end;

procedure TDCP_sha3_256.Init;
begin
  SHA3_Init(FState, __SHA3_256);
end;

{ TDCP_sha3_384 }

class function TDCP_sha3_384.GetAlgorithm: string;
begin
  Result:= 'SHA3_384';
end;

class function TDCP_sha3_384.GetHashSize: integer;
begin
  Result:= 384;
end;

class function TDCP_sha3_384.SelfTest: boolean;
begin
  Result:= False; // TODO: SelfTest SHA3_384
end;

procedure TDCP_sha3_384.Init;
begin
  SHA3_Init(FState, __SHA3_384);
end;

{ TDCP_sha3_512 }

class function TDCP_sha3_512.GetAlgorithm: string;
begin
  Result:= 'SHA3_512';
end;

class function TDCP_sha3_512.GetHashSize: integer;
begin
  Result:= 512;
end;

class function TDCP_sha3_512.SelfTest: boolean;
begin
  Result:= False; // TODO: SelfTest SHA3_512
end;

procedure TDCP_sha3_512.Init;
begin
  SHA3_Init(FState, __SHA3_512);
end;

end.

