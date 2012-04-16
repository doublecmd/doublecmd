{
   Double Commander
   -------------------------------------------------------------------------
   Structures and functions for searching files.   

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2010 Koblov Alexander (Alexx2000@mail.ru)
   Copyright (C) 2010 Przemys³aw Nagay (cobines@gmail.com)

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

unit uFindFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

type
  TTimeUnit = (tuSecond, tuMinute, tuHour, tuDay, tuWeek, tuMonth, tuYear);
  TFileSizeUnit = (suBytes, suKilo, suMega, suGiga, suTera);

  TSearchTemplateRec = record
    StartPath: String;
    ExcludeDirectories: String;
    FilesMasks: String;
    ExcludeFiles: String;
    SearchDepth: Integer; // -1 = unlimited
    RegExp: Boolean;
    IsPartialNameSearch: Boolean;
    FollowSymLinks: Boolean;
    AttributesPattern: String;
    { Date/time }
    IsDateFrom,
    IsDateTo,
    IsTimeFrom,
    IsTimeTo : Boolean;
    DateTimeFrom,
    DateTimeTo : TDateTime;
    IsNotOlderThan: Boolean;
    NotOlderThan: Integer;
    NotOlderThanUnit: TTimeUnit;
    { File size }
    IsFileSizeFrom,
    IsFileSizeTo : Boolean;
    FileSizeFrom,
    FileSizeTo : Int64;
    FileSizeUnit: TFileSizeUnit;
    { Find/replace text }
    IsFindText: Boolean;
    FindText: String;
    IsReplaceText : Boolean;
    ReplaceText: String;
    CaseSensitive,
    NotContainingText: Boolean;
    TextEncoding: String;
    SearchPlugin: String;
  end;

  TFindFileAttrsCheck = record
    HaveAttrs: TFileAttrs;     //en> what attributes files must have
    DontHaveAttrs: TFileAttrs; //en> what attributes files must not have
    Negated: Boolean;
  end;

  TFindFileChecks = record
    FilesMasks: String;
    RegExp: Boolean;
    DateTimeFrom,
    DateTimeTo : TDateTime;
    FileSizeFrom,
    FileSizeTo : Int64;
    Attributes: array of TFindFileAttrsCheck;  //en> Each entry is OR'ed.
  end;

  procedure SearchTemplateToFindFileChecks(const SearchTemplate: TSearchTemplateRec;
                                           out FileChecks: TFindFileChecks);

  function CheckFileTime(const FileChecks: TFindFileChecks; FT : TFileTime) : Boolean; inline;
  function CheckFileDateTime(const FileChecks: TFindFileChecks; DT : TDateTime) : Boolean;
  function CheckFileSize(const FileChecks: TFindFileChecks; FileSize : Int64) : Boolean;
  function CheckFileAttributes(const FileChecks: TFindFileChecks; Attrs : TFileAttrs) : Boolean;

implementation

uses
  strutils, DateUtils, DCDateTimeUtils, DCFileAttributes;

const
  cKilo = 1024;
  cMega = 1024 * cKilo;
  cGiga = 1024 * cMega;
  cTera = 1024 * cGiga;

procedure DateTimeOptionsToChecks(const SearchTemplate: TSearchTemplateRec;
                                  var FileChecks: TFindFileChecks);
begin
  with FileChecks do
  begin
    if SearchTemplate.IsNotOlderThan then
    begin
      DateTimeFrom := SysUtils.Now;
      DateTimeTo   := DateTimeFrom;

      case SearchTemplate.NotOlderThanUnit of
        tuSecond:
          DateTimeFrom := IncSecond(DateTimeFrom, -SearchTemplate.NotOlderThan);
        tuMinute:
          DateTimeFrom := IncMinute(DateTimeFrom, -SearchTemplate.NotOlderThan);
        tuHour:
          DateTimeFrom := IncHour(DateTimeFrom, -SearchTemplate.NotOlderThan);
        tuDay:
          DateTimeFrom := IncDay(DateTimeFrom, -SearchTemplate.NotOlderThan);
        tuWeek:
          DateTimeFrom := IncWeek(DateTimeFrom, -SearchTemplate.NotOlderThan);
        tuMonth:
          DateTimeFrom := IncMonth(DateTimeFrom, -SearchTemplate.NotOlderThan);
        tuYear:
          DateTimeFrom := IncYear(DateTimeFrom, -SearchTemplate.NotOlderThan);
      end;
    end
    else
    begin
      if SearchTemplate.IsDateFrom then
      begin
        if SearchTemplate.IsTimeFrom then
          DateTimeFrom := SearchTemplate.DateTimeFrom
        else
          DateTimeFrom := Trunc(SearchTemplate.DateTimeFrom);
      end
      else if SearchTemplate.IsTimeFrom then
        DateTimeFrom := Frac(SearchTemplate.DateTimeFrom)
      else
        DateTimeFrom := 0;

      if SearchTemplate.IsDateTo then
      begin
        if SearchTemplate.IsTimeTo then
          DateTimeTo := SearchTemplate.DateTimeTo
        else
          DateTimeTo := Trunc(SearchTemplate.DateTimeTo) + Frac(MaxDateTime);
      end
      else if SearchTemplate.IsTimeTo then
        DateTimeFrom := Frac(SearchTemplate.DateTimeTo)
      else
        DateTimeFrom := 0;
    end;
  end;
end;

procedure FileSizeOptionsToChecks(const SearchTemplate: TSearchTemplateRec;
                                  var FileChecks: TFindFileChecks);

  function GetFileSizeWithUnit(Size: Int64): Int64;
  begin
    case SearchTemplate.FileSizeUnit of
      suBytes:
        Result := Size;
      suKilo:
        Result := Size * cKilo;
      suMega:
        Result := Size * cMega;
      suGiga:
        Result := Size * cGiga;
      suTera:
        Result := Size * cTera;
    end;
  end;

begin
  if SearchTemplate.IsFileSizeFrom then
    FileChecks.FileSizeFrom := GetFileSizeWithUnit(SearchTemplate.FileSizeFrom)
  else
    FileChecks.FileSizeFrom := 0;

  if SearchTemplate.IsFileSizeTo then
    FileChecks.FileSizeTo := GetFileSizeWithUnit(SearchTemplate.FileSizeTo)
  else
    FileChecks.FileSizeTo := High(FileChecks.FileSizeTo);
end;

function AttrPatternToCheck(const AttrPattern: String): TFindFileAttrsCheck;
var
  StartIndex, CurIndex: Integer;
begin
  Result.HaveAttrs := 0;
  Result.DontHaveAttrs := 0;
  Result.Negated := False;

  StartIndex := 1;
  CurIndex := StartIndex;
  while CurIndex <= Length(AttrPattern) do
  begin
    case AttrPattern[CurIndex] of
      '+':
        begin
          Result.HaveAttrs := Result.HaveAttrs or
            SingleStrToFileAttr(Copy(AttrPattern, StartIndex, CurIndex - StartIndex));
          StartIndex := CurIndex + 1;
        end;
      '-':
        begin
          Result.DontHaveAttrs := Result.DontHaveAttrs or
          SingleStrToFileAttr(Copy(AttrPattern, StartIndex, CurIndex - StartIndex));
          StartIndex := CurIndex + 1;
        end;
      '!':
        begin
          if CurIndex = 1 then
            Result.Negated := True;
          StartIndex := CurIndex + 1;
        end;
      ' ': // omit spaces
        begin
          StartIndex := CurIndex + 1;
        end;
    end;

    Inc(CurIndex);
  end;
end;

procedure AttrsPatternOptionsToChecks(const SearchTemplate: TSearchTemplateRec;
                                      var FileChecks: TFindFileChecks);
var
  AttrsPattern, CurPattern: String;
begin
  FileChecks.Attributes := nil;

  AttrsPattern := SearchTemplate.AttributesPattern;
  while AttrsPattern <> '' do
  begin
    // For each pattern separated by '|' create a new TFindFileAttrsCheck.
    CurPattern := Copy2SymbDel(AttrsPattern, '|');
    if CurPattern <> '' then
      with FileChecks do
      begin
        SetLength(Attributes, Length(Attributes) + 1);
        Attributes[Length(Attributes) - 1] := AttrPatternToCheck(CurPattern);
      end;
  end;
end;

procedure SearchTemplateToFindFileChecks(const SearchTemplate: TSearchTemplateRec;
                                         out FileChecks: TFindFileChecks);
begin
  if SearchTemplate.IsPartialNameSearch then
    FileChecks.FilesMasks := '*' + SearchTemplate.FilesMasks + '*'
  else
    FileChecks.FilesMasks := SearchTemplate.FilesMasks;
  FileChecks.RegExp := SearchTemplate.RegExp;
  DateTimeOptionsToChecks(SearchTemplate, FileChecks);
  FileSizeOptionsToChecks(SearchTemplate, FileChecks);
  AttrsPatternOptionsToChecks(SearchTemplate, FileChecks);
end;

function CheckFileTime(const FileChecks: TFindFileChecks; FT : TFileTime) : Boolean;
begin
  Result := CheckFileDateTime(FileChecks, FileTimeToDateTime(FT));
end;

function CheckFileDateTime(const FileChecks: TFindFileChecks; DT : TDateTime) : Boolean;
begin
  with FileChecks do
    Result := (DateTimeFrom <= DT) and (DT <= DateTimeTo);
end;

function CheckFileSize(const FileChecks: TFindFileChecks; FileSize: Int64): Boolean;
begin
   with FileChecks do
     Result := (FileSizeFrom <= FileSize) and (FileSize <= FileSizeTo);
end;

function CheckFileAttributes(const FileChecks: TFindFileChecks; Attrs : TFileAttrs) : Boolean;
var
  i: Integer;
begin
  if Length(FileChecks.Attributes) = 0 then
    Result := True
  else
  begin
    for i := Low(FileChecks.Attributes) to High(FileChecks.Attributes) do
    begin
      with FileChecks.Attributes[i] do
      begin
        Result := ((Attrs and HaveAttrs) = HaveAttrs) and
                  ((Attrs and DontHaveAttrs) = 0);

        if Negated then
          Result := not Result;

        if Result then
          Exit;
      end;
    end;
    Result := False;
  end;
end;

end.
