{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of SHA224 *******************************}
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
unit DCPsha224;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPsha256;

type

  { TDCP_sha224 }

  TDCP_sha224 = class(TDCP_sha256)
  public
    class function GetId: integer; override;
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
  end;

implementation

{ TDCP_sha224 }

class function TDCP_sha224.GetId: integer;
begin
  Result:= 0;
end;

class function TDCP_sha224.GetAlgorithm: string;
begin
  Result:= 'SHA224';
end;

class function TDCP_sha224.GetHashSize: integer;
begin
  Result:= 224;
end;

class function TDCP_sha224.SelfTest: boolean;
begin
  Result:= False; // TODO: SelfTest SHA2_224
end;

procedure TDCP_sha224.Init;
begin
  Burn;
  CurrentHash[0]:= $C1059ED8;
  CurrentHash[1]:= $367CD507;
  CurrentHash[2]:= $3070DD17;
  CurrentHash[3]:= $F70E5939;
  CurrentHash[4]:= $FFC00B31;
  CurrentHash[5]:= $68581511;
  CurrentHash[6]:= $64F98FA7;
  CurrentHash[7]:= $BEFA4FA4;
  fInitialized:= True;
end;

end.

