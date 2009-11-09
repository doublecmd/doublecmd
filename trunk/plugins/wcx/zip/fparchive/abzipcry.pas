(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbZipCry.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: PKZip crypto units                          *}
{* Based on information from Appnote.txt, shipped with   *}
{* PKWare's PKZip for Windows 2.5                        *}
{*********************************************************}

{$I AbDefine.inc}

unit AbZipCry;

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  AbUtils,
  AbZipTyp;

type
  TAbZipKeys = array [0..2] of DWORD;                                  

  TAbZDecoder = class( TObject )
  protected
    FZipKeys : TAbZipKeys;
    FBuffer : array [0..11] of Byte;
    FCRC : LongInt;                                                      
    FCheckByte : Boolean;
    FOnNeedPassword : TAbNeedPasswordEvent;
    FPassword : string;
    FRetries : Byte;
    FStream : TStream;

    function DecryptByte : Byte;
      {get the decryption byte}
    procedure DoOnNeedPassword( var NewPassword : string );
              virtual;
    procedure InitKeys;
      {-Initialize Keys}
  public
    constructor Create(const aPassword : string;
                         var aStream : TStream;
                             aCRC : LongInt; aCheckByte : Boolean );          
    destructor Destroy; override;

    function Decode( c : Byte ) : Byte;
      {-returns a decoded byte}
    procedure DecodeBuffer( var Buffer; Count : Integer );             
      {-decodes the next Count bytes of Buffer}                        
    function Encode( c : Byte ) : Byte;
      {-returns an encoded byte}
    procedure EncodeBuffer( var Buffer; Count : Integer );             
      {-encodes the next Count bytes of Buffer}                        
    function ReadEncryptionHeader : Boolean;
      {-read and validate the encryption header}
    procedure WriteEncryptionHeader;
      {-initialize and create the encryption header}
    property OnNeedPassword : TAbNeedPasswordEvent
             read FOnNeedPassword
             write FOnNeedPassword;
    property Password : string
             write FPassword;
    property Retries : Byte
             read FRetries
             write FRetries;
  end;

  procedure AbUpdateKeys( c : Byte; var Keys : TAbZipKeys );
    {-Updates the keys with c}


implementation

uses
  AbConst,
  AbExcept;

const
  AbZipKeyInit1 = 305419896;
  AbZipKeyInit2 = 591751049;
  AbZipKeyInit3 = 878082192;
  AbZipMagicNumber = 134775813;

{ -------------------------------------------------------------------------- }
constructor TAbZDecoder.Create(const aPassword : string;
                                var aStream : TStream;
                                    aCRC : LongInt; aCheckByte : Boolean );
begin
  inherited Create;
  FPassword := aPassword;
  FStream := aStream;
  FCRC := aCRC;
  FCheckByte := aCheckByte;
  FRetries := 3;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZDecoder.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZDecoder.Decode( c : Byte ) : Byte;
var
  Temp : Word;
begin
  Temp := Word( FZipKeys[2] ) or 2;

  Result := c xor ( ( Temp * ( Temp xor 1 ) ) shr 8 );
  AbUpdateKeys( Result, FZipKeys );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZDecoder.DecodeBuffer( var Buffer; Count : Integer );     
  {-decodes the next Count bytes of Buffer}                            
var
  Buf : PByte;
  i : Integer;
  Temp : Word;
begin
  Buf := @Buffer;

  for i := 0 to pred( Count ) do begin
    Temp := Word( FZipKeys[2] ) or 2;
    Buf^ := Buf^ xor ( ( Temp * ( Temp xor 1 ) ) shr 8 );

    {begin of AbUpdateKeys( Buf^, FZipKeys );}
    FZipKeys[0] := AbUpdateCrc32( Buf^, FZipKeys[0] );
    FZipKeys[1] := FZipKeys[1] + ( FZipKeys[0] and $FF );
    FZipKeys[1] := ( FZipKeys[1] * AbZipMagicNumber ) + 1;
    {FZipKeys[2] := AbUpdateCrc32( FZipKeys[1] shr 24, FZipKeys[2] );}
    FZipKeys[2] := AbCrc32Table[ Byte( FZipKeys[1] shr 24 xor
                                       DWORD( FZipKeys[2] ) ) ] xor    
                                 ( (FZipKeys[2] shr 8) and $00FFFFFF );
    {end of AbUpdateKeys( Buf^, FZipKeys );}
    Inc(Buf, 1);
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbZDecoder.DecryptByte : Byte;
  {function has also been included as inline to Decode/Encode}
var
  Temp : Word;
begin
  Temp := Word( FZipKeys[2] ) or 2;
  Result := ( Temp * ( Temp xor 1 ) ) shr 8;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZDecoder.DoOnNeedPassword( var NewPassword : string );
begin
  if Assigned( FOnNeedPassword ) then
    FOnNeedPassword( Self, NewPassword )
  else
    raise EAbZipInvalidPassword.Create;
end;
{ -------------------------------------------------------------------------- }
function TAbZDecoder.Encode( c : Byte ) : Byte;
  {-returns an encoded byte}
var
  t : Word;
begin
  t := Word( FZipKeys[2] ) or 2;
  t := ( t * ( t xor 1 ) ) shr 8;
  AbUpdateKeys( c, FZipKeys );
  Result := t xor c;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZDecoder.EncodeBuffer( var Buffer; Count : Integer );
  {-encodes the next Count bytes of Buffer}
var
  Buf : PByte;
  i : Integer;
  t : Word;
begin
  Buf := @Buffer;

  for i := 0 to pred( Count ) do begin
    t := Word( FZipKeys[2] ) or 2;
    t := ( t * ( t xor 1 ) ) shr 8;
    {begin of AbUpdateKeys( Buf^, FZipKeys );}
    FZipKeys[0] := AbUpdateCrc32( Buf^, FZipKeys[0] );
    FZipKeys[1] := FZipKeys[1] + ( FZipKeys[0] and $FF );
    FZipKeys[1] := ( FZipKeys[1] * AbZipMagicNumber ) + 1;
    {FZipKeys[2] := AbUpdateCrc32( FZipKeys[1] shr 24, FZipKeys[2] );}
    FZipKeys[2] := AbCrc32Table[ Byte( FZipKeys[1] shr 24 xor
                                       DWORD( FZipKeys[2] ) ) ] xor
                                 ( (FZipKeys[2] shr 8) and $00FFFFFF );
    {end of AbUpdateKeys( Buf^, FZipKeys );}
    Buf^ := t xor Buf^;
    Inc(Buf, 1);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZDecoder.InitKeys;
var
  i : Integer;
begin
  FZipKeys[0] := AbZipKeyInit1;
  FZipKeys[1] := AbZipKeyInit2;
  FZipKeys[2] := AbZipKeyInit3;

  for i := 1 to Length( FPassword ) do
    AbUpdateKeys( Ord( FPassword[i] ), FZipKeys );
end;
{ -------------------------------------------------------------------------- }
function TAbZDecoder.ReadEncryptionHeader : Boolean;
type
  Bytes = packed record
    L1, L2, L3, L4 : Byte
  end;
var
  i : Integer;
  Valid : Boolean;
  Attempts : Byte;
  Pos : LongInt;
begin
  {save the current position}
  Pos := FStream.Position;
  Valid := False;
  Attempts := 0;
  while ( not Valid ) and (Attempts < Retries ) do begin
    InitKeys;
    {read the header}
    FStream.Seek( Pos, soFromBeginning );
    FStream.Read( FBuffer[0], 12 );
    for i := 0 to 11 do begin
      FBuffer[i] := FBuffer[i] xor DecryptByte;
      AbUpdateKeys( FBuffer[i], FZipKeys );
    end;
    if FCheckByte then
      {version 2.0 or better}
      Valid := ( FBuffer[11] = Bytes( FCRC ).L4 )
    else
      {prior to version 2.0}
      Valid := ( FBuffer[11] = Bytes( FCRC ).L4 ) and
               ( FBuffer[10] = Bytes( FCRC ).L3 );
    if not Valid then
      DoOnNeedPassword( FPassword );
    inc( Attempts );
  end;
  if not Valid then
    raise EAbZipInvalidPassword.Create;
  Result := Valid;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZDecoder.WriteEncryptionHeader;
type
  Bytes = packed record
    L1, L2, L3, L4 : Byte
  end;
var
  n : Integer;
  c : Byte;
  t : Word;
begin
  InitKeys;
  for n := 0 to 9 do begin
    c := Random( 256 );
    t := DecryptByte;
    AbUpdateKeys( c, FZipKeys );
    FBuffer[n] := t xor c;
  end;
  InitKeys;
  for n := 0 to 9 do begin
    t := DecryptByte;
    AbUpdateKeys( FBuffer[n], FZipKeys );
    FBuffer[n] := t xor FBuffer[n];
  end;
  {now do FBuffer[10]}
  t := DecryptByte;
  AbUpdateKeys( Bytes( FCRC ).L3, FZipKeys );
  FBuffer[10] := t xor Bytes( FCRC ).L3;
  {now do FBuffer[11]}
  t := DecryptByte;
  AbUpdateKeys( Bytes( FCRC ).L4, FZipKeys );
  FBuffer[11] := t xor Bytes( FCRC ).L4;
  {now write it to the buffer}
  FStream.Write( FBuffer[0], 12 );
end;
{ -------------------------------------------------------------------------- }
procedure AbUpdateKeys( c : Byte; var Keys : TAbZipKeys );
begin
  Keys[0] := AbUpdateCrc32( c, Keys[0] );
  Keys[1] := Keys[1] + ( Keys[0] and $FF );
  Keys[1] := ( Keys[1] * AbZipMagicNumber ) + 1;
  Keys[2] := AbUpdateCrc32( Keys[1] shr 24, Keys[2] );
end;

end.
