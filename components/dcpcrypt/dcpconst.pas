{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* Constants for use with DCPcrypt ********************************************}
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
unit DCPconst;

interface

{******************************************************************************}
const
  { Component registration }
  DCPcipherpage     = 'DCPciphers';
  DCPhashpage       = 'DCPhashes';

  { ID values }
  DCP_rc2           =  1;
  DCP_sha1          =  2;
  DCP_rc5           =  3;
  DCP_rc6           =  4;
  DCP_blowfish      =  5;
  DCP_twofish       =  6;
  DCP_cast128       =  7;
  DCP_gost          =  8;
  DCP_rijndael      =  9;
  DCP_ripemd160     = 10;
  DCP_misty1        = 11;
  DCP_idea          = 12;
  DCP_mars          = 13;
  DCP_haval         = 14;
  DCP_cast256       = 15;
  DCP_md5           = 16;
  DCP_md4           = 17;
  DCP_tiger         = 18;
  DCP_rc4           = 19;
  DCP_ice           = 20;
  DCP_thinice       = 21;
  DCP_ice2          = 22;
  DCP_des           = 23;
  DCP_3des          = 24;
  DCP_tea           = 25;
  DCP_serpent       = 26;
  DCP_ripemd128     = 27;
  DCP_sha256        = 28;
  DCP_sha384        = 29;
  DCP_sha512        = 30;
  DCP_sfv           = 99;


{******************************************************************************}
{******************************************************************************}
implementation

end.
