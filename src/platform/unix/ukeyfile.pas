{
   Double Commander
   -------------------------------------------------------------------------
   Simple key file parser

   Copyright (C) 2012 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uKeyFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetText, DCBasicTypes, DCClassesUtf8;

const

  { Constants for handling freedesktop.org Desktop files }

  DESKTOP_GROUP = 'Desktop Entry';

  DESKTOP_KEY_CATEGORIES = 'Categories';
  DESKTOP_KEY_COMMENT = 'Comment';
  DESKTOP_KEY_EXEC = 'Exec';
  DESKTOP_KEY_ICON = 'Icon';
  DESKTOP_KEY_NAME = 'Name';
  DESKTOP_KEY_NO_DISPLAY = 'NoDisplay';
  DESKTOP_KEY_TERMINAL = 'Terminal';

type

  { TKeyFile }

  TKeyFile = class(TIniFileEx)
  public
    constructor Create(const AFileName: String; Mode: Word); override;
    function ReadBool(const Section, Ident: String; Default: Boolean): Boolean; override;
    procedure WriteBool(const Section, Ident: String; Value: Boolean); override;
    function ReadString(const Section, Ident, Default: String): String; override;
    function ReadLocaleString(const Section, Ident, Default: String): String; virtual;
  end;

implementation

uses
  DCStrUtils;

var
  LocaleKeyList: TDynamicStringArray;

procedure InitializeLocaleKeyList;
var
  FLang,
  FFallbackLang: String;
  EncodingIndex,
  ModifierIndex: Integer;
begin
  GetLanguageIDs(FLang, FFallbackLang);
  EncodingIndex:= Pos('.', FLang);
  ModifierIndex:= Pos('@', FLang);
  // Strip encoding part
  if EncodingIndex > 0 then
  begin
    if ModifierIndex = 0 then
      FLang:= Copy(FLang, 1, EncodingIndex - 1)
    else
      Delete(FLang, EncodingIndex, ModifierIndex - EncodingIndex);
  end;
  // Fill possible keys in order of matching
  AddString(LocaleKeyList, FLang);
  if FLang <> FFallbackLang then
  begin
    if (ModifierIndex > 0) and (Pos('_', FLang) > 0) then
    begin
      AddString(LocaleKeyList, Copy(FLang, 1, ModifierIndex - 1));
      AddString(LocaleKeyList, FFallbackLang + Copy(FLang, ModifierIndex, MaxInt));
    end;
    AddString(LocaleKeyList, FFallbackLang);
  end;
end;

function EscapeSequences(const S: UTF8String): UTF8String;
var
  C: AnsiChar;
  R, P: PAnsiChar;
begin
  P:= PAnsiChar(S); C:= P^;
  SetLength(Result, Length(S));
  R:= PAnsiChar(Result);
  repeat
    Inc(P);
    if C = '\' then
      begin
        case P^ of
          #00: Exit;
          't': C:= #09;
          'n': C:= #10;
          'r': C:= #13;
          's': C:= #32;
          '\':
            begin
              R^:= C;
              Inc(P);
              C:= P^;
            end;
        end;
      end
    else
      begin
        R^:= C;
        C:= P^;
        Inc(R);
      end;
  until P^ = #00;
end;

{ TKeyFile }

constructor TKeyFile.Create(const AFileName: String; Mode: Word);
begin
  inherited Create(AFileName, Mode);
  CaseSensitive:= True;
end;

function TKeyFile.ReadBool(const Section, Ident: String;
  Default: Boolean): Boolean;
var
  S: String;
begin
  Result := Default;
  S := inherited ReadString(Section, Ident, EmptyStr);
  if Length(S) > 0 then Result := (S[1] in ['1', 't']);
end;

procedure TKeyFile.WriteBool(const Section, Ident: String;
  Value: Boolean);
var
  S: String;
begin
  if Value then S := 'true' else S := 'false';
  inherited WriteString(Section, Ident, S);
end;

function TKeyFile.ReadString(const Section, Ident, Default: String): String;
begin
  Result:= inherited ReadString(Section, Ident, Default);
  if Length(Result) > 0 then Result := EscapeSequences(Result);
end;

function TKeyFile.ReadLocaleString(const Section, Ident, Default: String): String;
const
  LocaleString: String = '%s[%s]';
var
  I: Integer;
begin
  for I:= Low(LocaleKeyList) to High(LocaleKeyList) do
  begin
    Result:=  ReadString(Section, Format(LocaleString, [Ident, LocaleKeyList[I]]), EmptyStr);
    if Length(Result) > 0 then Exit;
  end;
  Result:= ReadString(Section, Ident, Default);
end;

initialization
  InitializeLocaleKeyList;

end.

