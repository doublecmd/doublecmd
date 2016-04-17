{
   Double commander
   -------------------------------------------------------------------------
   Wdx plugin is intended to show one line of a text file

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

library TextLine;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  SysUtils, Classes, StreamEx, WdxPlugin, LazUTF8,
  DCClassesUtf8, DCConvertEncoding, DCStrUtils, DCBasicTypes;

var
  FReplace: Boolean = False;
  FSkipEmpty: Boolean = False;
  FExtensions: TDynamicStringArray;
  FReplaces: array[1..10, 1..2] of String;

function ContentGetSupportedField(FieldIndex: Integer;
  FieldName, Units: PAnsiChar; MaxLen: Integer): Integer; dcpcall;
begin
  if (FieldIndex < 0) or (FieldIndex > 9) then
  begin
    Result := FT_NOMOREFIELDS;
    Exit;
  end;

  StrLCopy(FieldName, PAnsiChar(IntToStr(FieldIndex + 1)), MaxLen - 1);
  StrLCopy(Units, 'ANSI|OEM|UTF-8', MaxLen - 1);
  Result := FT_STRINGW;
end;

function ContentGetValueW(FileName: PWideChar; FieldIndex, UnitIndex: Integer;
  FieldValue: PWideChar; MaxLen, Flags: Integer): Integer; dcpcall;
var
  Value: String;
  Index: Integer;
  FileAttr: Integer;
  FileNameU: String;
  Stream: TFileStreamEx;
  Reader: TStreamReader;
begin
  if (FieldIndex < 0) or (FieldIndex > 9) then
  begin
    Result:= ft_nosuchfield;
    Exit;
  end;
  FileNameU:= UTF16ToUTF8(UnicodeString(FileName));
  FileAttr:= FileGetAttr(FileNameU);
  if (FileAttr < 0) or (FileAttr and faSysFile <> 0) then
  begin
    Result:= ft_fileerror;
    Exit;
  end;
  if Length(FExtensions) > 0 then
  begin
    Value:= ExtractOnlyFileExt(FileNameU);
    if not Contains(FExtensions, Value) then Exit(ft_fileerror);
  end;
  Result:= ft_fieldempty;
  try
    Stream:= TFileStreamEx.Create(FileNameU, fmOpenRead or fmShareDenyNone);
    try
      Index:= -1;
      Reader:= TStreamReader.Create(Stream, BUFFER_SIZE, True);
      repeat
        Value:= EmptyStr;
        if Reader.IsEof then Break;
        Value:= Trim(Reader.ReadLine);
        if (Length(Value) = 0) and FSkipEmpty then
          Continue;
        Inc(Index);
      until Index = FieldIndex;
    finally
      Reader.Free;
    end;
  except
    Exit(ft_fileerror);
  end;

  if Value = EmptyStr then Exit;

  case UnitIndex of
    0: Value:= CeAnsiToUtf8(Value);
    1: Value:= CeOemToUtf8(Value);
  end;

  if FReplace and (Length(Value) > 0) then
  begin
    for Flags:= Low(FReplaces) to High(FReplaces) do
    begin
      if Length(FReplaces[Flags, 1]) > 0 then
        Value:= StringReplace(Value, FReplaces[Flags, 1], FReplaces[Flags, 2], [rfReplaceAll]);
    end;
  end;

  if Length(Value) > 0 then
  begin
    StrPLCopy(FieldValue, UTF8ToUTF16(Value), MaxLen - 2);
    Result:= ft_stringw;
  end;
end;

procedure ContentSetDefaultParams(dps: PContentDefaultParamStruct); dcpcall;
var
  S: String;
  Index: Integer;
  Ini: TIniFileEx;
  FileName: String;
begin
  FileName:= CeSysToUtf8(dps^.DefaultIniName);
  FileName:= ExtractFilePath(FileName) + 'textline.ini';
  try
    Ini:= TIniFileEx.Create(FileName, fmOpenRead);
    try
      FExtensions:= SplitString(Ini.ReadString('Options', 'Extensions', EmptyStr), ' ');
      FSkipEmpty:= Ini.ReadBool('Options', 'SkipEmpty', FSkipEmpty);
      for Index:= Low(FReplaces) to High(FReplaces) do
      begin
        S:= Ini.ReadString('Replaces', 'S' + IntToStr(Index), '=');
        FReplaces[Index, 1]:= Copy(S, 1, Pos('=', S) - 1);
        FReplaces[Index, 2]:= Copy(S, Pos('=', S) + 1, MaxInt);
        if (FReplace = False) then FReplace:= (S <> '=');
      end;
    finally
      Ini.Free;
    end;
  except
    // Ignore
  end;
end;

exports
  ContentGetSupportedField,
  ContentGetValueW,
  ContentSetDefaultParams;

begin

end.

