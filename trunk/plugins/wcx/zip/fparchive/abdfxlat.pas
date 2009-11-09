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
{* ABBREVIA: AbDfXlat.pas 3.05                           *}
{*********************************************************}
{* Deflate length/dist to symbol translator              *}
{*********************************************************}

unit AbDfXlat;

{$I AbDefine.inc}

interface

uses
  SysUtils,
  Classes;

type
  TAbDfTranslator = class
    private
      FBuffer : PAnsiChar;
      FLenSymbols        : PByteArray;
                           {for lengths 3..258}
      FLongDistSymbols   : PByteArray;
                           {for distances 32769..65536 (deflate64)}
      FMediumDistSymbols : PByteArray;
                           {for distances 257..32768}
      FShortDistSymbols  : PByteArray;
                           {for distances 1..256}
    protected
      procedure trBuild;
    public
      constructor Create;
      destructor Destroy; override;

      function TranslateLength(aLen  : integer): integer;
      function TranslateDistance(aDist : integer) : integer;

      property LenSymbols : PByteArray read FLenSymbols;
      property LongDistSymbols : PByteArray read FLongDistSymbols;
      property MediumDistSymbols : PByteArray read FMediumDistSymbols;
      property ShortDistSymbols  : PByteArray read FShortDistSymbols;
  end;

var
  AbSymbolTranslator : TAbDfTranslator;

implementation

uses
  AbDfBase;

{====================================================================}
constructor TAbDfTranslator.Create;
begin
  {create the ancestor}
  inherited Create;

  {allocate the translation arrays (the buffer *must* be zeroed)}
  FBuffer := AllocMem(256 + 2 + 256 + 256);                    {!!.02}
  FLenSymbols := PByteArray(FBuffer);
  FLongDistSymbols := PByteArray(FBuffer + 256);
  FMediumDistSymbols := PByteArray(FBuffer + 256 + 2);
  FShortDistSymbols := PByteArray(FBuffer + 256 + 2 + 256);

  {build the translation arrays}
  trBuild;
end;
{--------}
destructor TAbDfTranslator.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMem(FBuffer);
  inherited Destroy;
end;
{--------}
function TAbDfTranslator.TranslateDistance(aDist : integer) : integer;
begin
  {save against dumb programming mistakes}
  Assert((1 <= aDist) and (aDist <= 65536),
         'TAbDfTranslator.Translate: distance should be 1..65536');

  {translate the distance}
  if (aDist <= 256) then
    Result := FShortDistSymbols[aDist - 1]
  else if (aDist <= 32768) then
    Result := FMediumDistSymbols[((aDist - 1) div 128) - 2]
  else
    Result := FLongDistSymbols[((aDist - 1) div 16384) - 2];
end;
{--------}
function TAbDfTranslator.TranslateLength(aLen  : integer): integer;
begin
  {save against dumb programming mistakes}
  Assert((3 <= aLen) and (aLen <= 65536),
         'TAbDfTranslator.Translate: length should be 3..65536');

  {translate the length}
  dec(aLen, 3);
  if (0 <= aLen) and (aLen <= 255) then
    Result := FLenSymbols[aLen] + 257
  else
    Result := 285;
end;
{--------}
procedure TAbDfTranslator.trBuild;
var
  i     : integer;
  Len   : integer;
  Dist  : integer;
  Value : integer;
begin
  {initialize the length translation array; elements will contain
   (Symbol - 257) for a given (length - 3)}
  for i := low(dfc_LengthBase) to pred(high(dfc_LengthBase)) do begin
    Len := dfc_LengthBase[i] - 3;
    FLenSymbols[Len] := i;
  end;
  FLenSymbols[255] := 285 - 257;
  Value := -1;
  for i := 0 to 255 do begin
    if (Value < FLenSymbols[i]) then
      Value := FLenSymbols[i]
    else
      FLenSymbols[i] := Value;
  end;

  {initialize the short distance translation array: it will contain
   the Symbol for a given (distance - 1) where distance <= 256}
  for i := 0 to 15 do begin
    Dist := dfc_DistanceBase[i] - 1;
    FShortDistSymbols[Dist] := i;
  end;
  Value := -1;
  for i := 0 to 255 do begin
    if (Value < FShortDistSymbols[i]) then
      Value := FShortDistSymbols[i]
    else
      FShortDistSymbols[i] := Value;
  end;

  {initialize the medium distance translation array: it will contain
   the Symbol for a given (((distance - 1) div 128) - 2) where
   distance is in the range 256..32768}
  for i := 16 to 29 do begin
    Dist := ((dfc_DistanceBase[i] - 1) div 128) - 2;
    FMediumDistSymbols[Dist] := i;
  end;
  Value := -1;
  for i := 0 to 255 do begin
    if (Value < FMediumDistSymbols[i]) then
      Value := FMediumDistSymbols[i]
    else
      FMediumDistSymbols[i] := Value;
  end;

  {initialize the long distance translation array: it will contain the
   Symbol for a given ((distance - 1) div 16384) - 2) for distances
   over 32768 in deflate64}
  FLongDistSymbols[0] := 30;
  FLongDistSymbols[1] := 31;
end;
{====================================================================}

initialization
  AbSymbolTranslator := TAbDfTranslator.Create;

finalization
  AbSymbolTranslator.Free;

end.
