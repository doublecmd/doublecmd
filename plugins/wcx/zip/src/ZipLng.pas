{
  Double Commander
  -------------------------------------------------------------------------
  Zip archiver plugin, language support

  Copyright (C) 2023 Alexander Koblov (alexx2000@mail.ru)

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

unit ZipLng;

{$mode delphi}

interface

uses
  Classes, SysUtils;

resourcestring
  rsCompressionMethodStore = 'Store';
  rsCompressionMethodOptimal = 'Optimal (2x slower)';

  rsCompressionLevelFastest = 'Fastest';
  rsCompressionLevelFast = 'Fast';
  rsCompressionLevelNormal = 'Normal';
  rsCompressionLevelMaximum = 'Maximum';
  rsCompressionLevelUltra = 'Ultra';

  procedure TranslateResourceStrings;

implementation

uses
  ZipFunc;

function Translate(Name, Value: AnsiString; Hash: LongInt; Arg: Pointer): AnsiString;
var
  ALen: Integer;
begin
  with gStartupInfo do
  begin
    SetLength(Result, MaxSmallint);
    ALen:= TranslateString(Translation, PAnsiChar(Name), PAnsiChar(Value), PAnsiChar(Result), MaxSmallint);
    SetLength(Result, ALen);
  end;
end;

procedure TranslateResourceStrings;
begin
  if Assigned(gStartupInfo.Translation) then
  begin
    SetResourceStrings(@Translate, nil);
  end;
end;

end.

