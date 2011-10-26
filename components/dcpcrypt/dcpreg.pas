{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* Component registration for DCPcrypt ****************************************}
{******************************************************************************}
{* Copyright (c) 1999-2002 David Barton                                       *}
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
unit DCPreg;

{$MODE Delphi}

interface
uses
  LResources,Classes;

procedure Register;

implementation

uses
  DCPcrypt2, DCPblockciphers, DCPconst, DCPblowfish, DCPcast128, DCPcast256,
  DCPdes, DCPgost, DCPice, DCPidea, DCPmars, DCPmisty1, DCPrc2, DCPrc4, DCPrc5,
  DCPrc6, DCPrijndael, DCPserpent, DCPtea, DCPtwofish,
  DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160, DCPsha1, DCPsha256,
  DCPsha512, DCPtiger;

procedure Register;
begin
  RegisterComponents(DCPcipherpage,[TDCP_blowfish,TDCP_cast128,TDCP_cast256,
    TDCP_des,TDCP_3des,{TDCP_gost,}TDCP_ice,TDCP_thinice,TDCP_ice2,TDCP_idea,
    TDCP_mars,TDCP_misty1,TDCP_rc2,TDCP_rc4,TDCP_rc5,TDCP_rc6,TDCP_rijndael,
    TDCP_serpent,TDCP_tea,TDCP_twofish]);
  RegisterComponents(DCPhashpage,[TDCP_haval,TDCP_md4,TDCP_md5,TDCP_ripemd128,
    TDCP_ripemd160,TDCP_sha1,TDCP_sha256,TDCP_sha384,TDCP_sha512,TDCP_tiger]);
end;

initialization
{$I DCPciphers.lrs}
{$I DCPhashes.lrs}
end.
