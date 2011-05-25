{
   Double commander
   -------------------------------------------------------------------------
   svn_wdx is a content plugin that displays some information from subversion

   Copyright (C) 2011 Koblov Alexander (Alexx2000@mail.ru)

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as 
   published by the Free Software Foundation, either version 3 of the 
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit svn_io;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, svn_def;

function ParseEntries(const FilePath: AnsiString): Boolean;

function FileNameIndex(const FileName: AnsiString): Integer;

function GetSvnInfo(FileIndex, Offset: Integer): AnsiString;

implementation

threadvar
  EntriesFile: array of AnsiString; // Current svn entries file

function ParseEntries(const FilePath: AnsiString): Boolean;
var
  I: LongWord;
  FileName: AnsiString;
  StringList: TStringList = nil;
begin
  Result:= False;
  FileName:= FilePath + PathDelim + '.svn' + PathDelim + 'entries';
  if not FileExists(FileName) then Exit;
  StringList:= TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    SetLength(EntriesFile, StringList.Count);
    for I:= 0 to StringList.Count - 1 do
      EntriesFile[I]:= StringList[I];
    Result:= True;
  finally
    FreeAndNil(StringList);
  end;
end;

function FileNameIndex(const FileName: AnsiString): Integer;
var
  I: LongWord;
begin
  Result:= -1;
  for I:= Low(EntriesFile) to High(EntriesFile) do
  begin
    if SameText(EntriesFile[I], FileName) then
      Exit(I);
  end;
end;

function GetSvnInfo(FileIndex, Offset: Integer): AnsiString;
var
  Index: Integer;
begin
  Result:= EmptyStr;
  if FileIndex < 0 then Exit;
  Index:= FileIndex + Offset;
  if (Index >= Low(EntriesFile)) and (Index <= High(EntriesFile)) then
    Result:= EntriesFile[Index];
end;

end.

