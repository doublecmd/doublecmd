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

unit svn_wdx_intf;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, WdxPlugin;

function ContentGetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar;
                                  Units: PAnsiChar; MaxLen: Integer): Integer; dcpcall;
function ContentGetValue(FileName: PAnsiChar; FieldIndex, UnitIndex: Integer;
                         FieldValue: PByte; MaxLen, Flags: Integer): Integer; dcpcall;

implementation

uses
  md5, svn_def, svn_io;

const
  IDX_SVN_AUTHOR      = 0;
  IDX_SVN_REVISION    = 1;
  IDX_SVN_STATUS      = 2;

  FIELD_COUNT         = 3;

threadvar
  CurrentFilePath:  AnsiString; // Current file path
  CurrentFileName:  AnsiString; // Current file name
  CurrentFileIndex: Integer;    // Current file index

function EnsureLength(S: AnsiString; MaxLen: Integer): AnsiString;
begin
  Result := S;
  if Length(Result) >= MaxLen then
    begin
      Result := Copy(Result, 1, MaxLen - 4);
      Result := Result + '...';
    end;
end;

function ContentGetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar;
  Units: PAnsiChar; MaxLen: Integer): Integer; dcpcall;
var
  Field: AnsiString;
begin
  StrPCopy(Units, EmptyStr);

  if FieldIndex >= FIELD_COUNT then
    begin
      Result := FT_NOMOREFIELDS;
      Exit;
    end;

  Result := FT_STRING;

  case FieldIndex of
      IDX_SVN_AUTHOR:       Field := 'SVN Author';
      IDX_SVN_REVISION:     Field := 'SVN Revision';
      IDX_SVN_STATUS:       Field := 'SVN Status';
  end;

  StrPLCopy(FieldName, Field, MaxLen);
end;

function ContentGetValue(FileName: PAnsiChar; FieldIndex, UnitIndex: Integer;
  FieldValue: PByte; MaxLen, Flags: Integer): Integer; dcpcall;
var
  Value,
  FilePath,
  CheckSum: AnsiString;
  FileAttr: LongInt;
begin
  Result := FT_FILEERROR;

  FileAttr:= FileGetAttr(FileName);
  if (FileAttr < 0) or Boolean(FileAttr and faDirectory) then Exit;

  FilePath:= ExtractFileDir(FileName);

  if CurrentFilePath <> FilePath then
  begin
     if not ParseEntries(FilePath) then Exit;
     CurrentFilePath := FilePath;
  end;

  if CurrentFileName <> FileName then
  begin
    CurrentFileIndex:= FileNameIndex(ExtractFileName(FileName));
    CurrentFileName := FileName;
  end;

  if (FieldIndex >= FIELD_COUNT) then
   begin
      Result := FT_NOSUCHFIELD;
      Exit;
   end;

  Result := FT_STRING;

  case FieldIndex of
      IDX_SVN_AUTHOR:       Value := GetSvnInfo(CurrentFileIndex, SVN_AUTHOR);
      IDX_SVN_REVISION:     Value := GetSvnInfo(CurrentFileIndex, SVN_REVISION);
      IDX_SVN_STATUS:
        begin
          if CurrentFileIndex < 0 then
            Value:= 'Unversioned'
          else
            begin
              // Get file checksum from subversion
              Value := GetSvnInfo(CurrentFileIndex, SVN_CHECKSUM);
              // Calculate file checksum
              CheckSum:= MD5Print(MD5File(FileName));
              // Compare checksums
              if not SameText(Value, CheckSum) then
                Value:= 'Modified'
              else
                Value:= 'Normal';
            end;
        end
  else
      Result := FT_FIELDEMPTY;
      Exit;
  end;

  StrPLCopy(PAnsiChar(FieldValue), EnsureLength(Value, MaxLen), MaxLen);
end;

end.

