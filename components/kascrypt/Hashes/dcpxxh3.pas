{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of XXH3-128                             *}
{******************************************************************************}
{* Copyright (C) 2024 Alexander Koblov (alexx2000@mail.ru)                    *}
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
unit DCPxxh3;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, DCPcrypt2, DCxxhash;

type

  { TDCP_xxh3_128 }

  TDCP_xxh3_128 = class(TDCP_hash)
  protected
    S: PXXH3_state_t;
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longword); override;
    procedure Final(var Digest); override;
  end;

implementation
{$R-}{$Q-}

{ TDCP_xxh3_128 }

class function TDCP_xxh3_128.GetHashSize: integer;
begin
  Result:= 128;
end;

class function TDCP_xxh3_128.GetAlgorithm: string;
begin
  Result:= 'XXH3-128';
end;

class function TDCP_xxh3_128.SelfTest: boolean;
begin
  Result:= False; // TODO: SelfTest XXH3_128
end;

constructor TDCP_xxh3_128.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  S:= XXH3_createState();
end;

destructor TDCP_xxh3_128.Destroy;
begin
  XXH3_freeState(S);
  inherited Destroy;
end;

procedure TDCP_xxh3_128.Init;
begin
  Burn;
  fInitialized:= true;
end;

procedure TDCP_xxh3_128.Burn;
begin
  XXH3_128bits_reset(S);
  fInitialized:= false;
end;

procedure TDCP_xxh3_128.Update(const Buffer; Size: longword);
begin
  XXH3_128bits_update(S, @Buffer, Size);
end;

procedure TDCP_xxh3_128.Final(var Digest);
var
  Temp: UInt64;
  Hash: XXH128_hash_t;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  Hash:= XXH3_128bits_digest(S);
  Temp:= SwapEndian(Hash.low64);
  Hash.low64:= SwapEndian(Hash.high64);
  Hash.high64:= Temp;
  Move(Hash, Digest, Sizeof(XXH128_hash_t));
  Burn;
end;

end.
