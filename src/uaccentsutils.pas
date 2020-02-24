{
   Double Commander
   -------------------------------------------------------------------------
   Routine related with characters with accents/ligatures and their equivalents without

   Copyright (C) 2016  Alexander Koblov (alexx2000@mail.ru)

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

unit uAccentsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LoadInMemoryOurAccentLookupTableList;
procedure FreeMemoryFromOurAccentLookupTableList;
function NormalizeAccentedChar(sInput: string): string;
function PosOfSubstrWithVersatileOptions(sSubString, sWholeString: string; bCaseSensitive, bIgnoreAccent: boolean; var ActualCharFittedInInput: integer): integer;

var
  gslAccents, gslAccentsStripped: TStringList;

resourcestring
  rsStrAccents = 'á;â;à;å;ã;ä;ç;é;ê;è;ë;í;î;ì;ï;ñ;ó;ô;ò;ø;õ;ö;ú;û;ù;ü;ÿ;Á;Â;À;Å;Ã;Ä;Ç;É;Ê;È;Ë;Í;Í;Ì;Ï;Ñ;Ó;Ô;Ø;Õ;Ö;ß;Ú;Û;Ù;Ü;Ÿ;¿;¡;œ;æ;Æ;Œ';
  rsStrAccentsStripped = 'a;a;a;a;a;a;c;e;e;e;e;i;i;i;i;n;o;o;o;o;o;o;u;u;u;u;y;A;A;A;A;A;A;C;E;E;E;E;I;I;I;I;N;O;O;O;O;O;B;U;U;U;U;Y;?;!;oe;ae;AE;OE';

implementation

uses
  //Lazarus, Free-Pascal, etc.
  LazUTF8,

  //DC
  DCStrUtils;

{ LoadInMemoryOurAccentLookupTableList }
procedure LoadInMemoryOurAccentLookupTableList;
var
  slTempoAccents, slTempoAccentsStripped: TStringList;
  iChar, iPos: integer;
begin
  slTempoAccents := TStringList.Create;
  slTempoAccentsStripped := TStringList.Create;
  try
    ParseLineToList(rsStrAccents, slTempoAccents);
    ParseLineToList(rsStrAccentsStripped, slTempoAccentsStripped);

    if slTempoAccents.Count <> slTempoAccentsStripped.Count then
      raise Exception.Create('Unexpected situation in LoadInMemoryOurAccentLookupTableList!' + #$0A + 'Most probably problem in language file regarding conversion string with accents...');

    gslAccents := TStringList.Create;
    gslAccents.Assign(slTempoAccents);
    gslAccents.Sort;
    gslAccentsStripped := TStringList.Create;

    for iChar := 0 to pred(gslAccents.Count) do
    begin
      iPos := slTempoAccents.IndexOf(gslAccents.Strings[iChar]);
      if iPos <> -1 then
        gslAccentsStripped.add(slTempoAccentsStripped.Strings[iPos])
      else
        raise Exception.Create('Unexpected situation in LoadInMemoryOurAccentLookupTableList! (Error in Rejavik)');
    end;

    if gslAccents.Count <> gslAccentsStripped.Count then
      raise Exception.Create('Unexpected situation in LoadInMemoryOurAccentLookupTableList! (Error in Mexico)');

  finally
    FreeAndNil(slTempoAccents);
    FreeAndNil(slTempoAccentsStripped);
  end;
end;

{ FreeMemoryFromOurAccentLookupTableList }
procedure FreeMemoryFromOurAccentLookupTableList;
begin
  if gslAccents <> nil then FreeAndNil(gslAccents);
  if gslAccentsStripped <> nil then FreeAndNil(gslAccentsStripped);
end;

{ NormalizeAccentedChar }
function NormalizeAccentedChar(sInput: string): string;
var
  iIndexChar, iPosChar: integer;
  cWorkingChar: string;
begin
  Result := '';
  for iIndexChar := 1 to UTF8length(sInput) do
  begin
    cWorkingChar := UTF8Copy(sInput, iIndexChar, 1);
    iPosChar := gslAccents.IndexOf(cWorkingChar);
    if iPosChar = -1 then
      Result := Result + cWorkingChar
    else
      Result := Result + gslAccentsStripped.Strings[iPosChar];
  end;
end;

{ PosOfSubstrWithVersatileOptions }
// NOTE: Function will search "sSubString" inside the "sWholeString" and return the position where it found it.
// WARNING! Function is assuming the "sSubString" is already preformated adequately and won't do it.
//          For example, if we do a search case insensitive, it is assumed you arrived here with "sSubString" already in UTF8UpperCase. So with "ABC" and not "abc".
//          For example, if you search ignoring accent, it is assumed you arrived here with "sSubString" already without accents. So with "aei" and not "àéî".
//          For example, if you search ignoring ligature, it is assumed you arrived here with "sSubString" already without ligature. So with "oe" and not with "œ".
//          No need to preformat, obviously, the "sWholeString".
//          ALL this is to speed up a little things since often we'll search the SAME string over and over in a whole string.
//          We'll gain something preparing our "sWholeString" once for all AND THEN keep re-searhcing in many strings using the following routine.
//          ALSO, because of the ligature possibility, the parameter "ActualCharFittedInInput" will be set according to the number of chars from the "sWholeString" that was used for finding the "sSubString".
//          For example, if we search "oeu" inside "soeur", the return value will be 2 and the "ActualCharFittedInInput" will be set to 3...
//          But if we search "oeu" inside "sœur", the return value will still be 2 BUT the "ActualCharFittedInInput" will be set to 2 since only two chars were required!
//          The author of this doesn't know for other language, but for French, this is a nice routine! :-)
function PosOfSubstrWithVersatileOptions(sSubString, sWholeString: string; bCaseSensitive, bIgnoreAccent: boolean; var ActualCharFittedInInput: integer): integer;
var
  sLocal: string;
  iActualPos: integer;
  iInnerResult: integer = 0;
begin
  ActualCharFittedInInput := 0;
  if bIgnoreAccent then sLocal := NormalizeAccentedChar(sWholeString) else sLocal := sWholeString;
  if not bCaseSensitive then sLocal := UTF8UpperCase(sLocal);
  iInnerResult := UTF8Pos(sSubString, sLocal);

  if iInnerResult > 0 then
  begin
    iActualPos := 0;
    sLocal := '';
    while (UTF8Length(sLocal) < iInnerResult) and (iActualPos < length(sWholeString)) do
    begin
      Inc(iActualPos);
      if bIgnoreAccent then
        sLocal := sLocal + NormalizeAccentedChar(UTF8copy(sWholeString, iActualPos, 1))
      else
        sLocal := sLocal + UTF8copy(sWholeString, iActualPos, 1);
    end;
    Result := iActualPos;

    //Once here, "iActualPos" holds the actual position of our substring in the string.
    //Then, we now add the char one by one until it match what were where searching AND IT SHOULD MATCH because "iInnerResult" has a value.
    sLocal := '';
    while (UTF8Pos(sSubString, sLocal) = 0) and ((Result + ActualCharFittedInInput) <= UTF8Length(sWholeString)) do
    begin
      Inc(ActualCharFittedInInput);
      sLocal := UTF8copy(sWholeString, Result, ActualCharFittedInInput);
      if bIgnoreAccent then sLocal := NormalizeAccentedChar(sLocal) else sLocal := sLocal;
      if not bCaseSensitive then sLocal := UTF8UpperCase(sLocal);
    end;
  end
  else
  begin
    Result := 0;
  end;
end;

end.

