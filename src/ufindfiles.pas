{
   Double Commander
   -------------------------------------------------------------------------
   Structures and functions for searching files.   

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2010 Przemys³aw Nagay (cobines@gmail.com)
   Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFindFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes, uFile;

type
  TTextSearchOption = (tsoMatchCase, tsoRegExpr, tsoHex);
  TTextSearchOptions = set of TTextSearchOption;
  TTextSearch = (tsAnsi, tsUtf8, tsUtf16le, tsUtf16be, tsOther);
  TTimeUnit = (tuSecond, tuMinute, tuHour, tuDay, tuWeek, tuMonth, tuYear);
  TFileSizeUnit = (suBytes, suKilo, suMega, suGiga, suTera);
  TPluginOperator = (poEqualCaseSensitive, poNotEqualCaseSensitive, poMore, poLess, poMoreEqual, poLessEqual,
                     poEqualCaseInsensitive, poNotEqualCaseInsensitive, poContainsCaseSensitive, poNotContainsCaseSensitive,
                     poContainsCaseInsensitive, poNotContainsCaseInsensitive, poRegExpr, poNotRegExpr);

  TPluginSearchRec = record
    Plugin: String;
    Field: String;
    UnitName: String;
    FieldType: Integer;
    Compare: TPluginOperator;
    Value: Variant;
  end;

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
    FindInArchives: Boolean;
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
    HexValue,
    CaseSensitive,
    NotContainingText: Boolean;
    TextRegExp: Boolean;
    TextEncoding: String;
    OfficeXML: Boolean;
    { Duplicates }
    Duplicates: Boolean;
    DuplicateName: Boolean;
    DuplicateSize: Boolean;
    DuplicateHash: Boolean;
    DuplicateContent: Boolean;
    { Plugins }
    SearchPlugin: String;
    ContentPlugin: Boolean;
    ContentPluginCombine: Boolean;
    ContentPlugins: array of TPluginSearchRec;
  end;

  TFindFileAttrsCheck = record
    HaveAttrs: TFileAttrs;     //en> what attributes files must have
    DontHaveAttrs: TFileAttrs; //en> what attributes files must not have
    Negated: Boolean;
  end;

  TFindFileChecks = record
    FilesMasks: String;
    ExcludeFiles: String;
    ExcludeDirectories: String;
    RegExp: Boolean;
    DateTimeFrom,
    DateTimeTo : TDateTime;
    FileSizeFrom,
    FileSizeTo : Int64;
    Attributes: array of TFindFileAttrsCheck;  //en> Each entry is OR'ed.
  end;
  TPFindFileChecks = ^TFindFileChecks;

  procedure SearchTemplateToFindFileChecks(const SearchTemplate: TSearchTemplateRec;
                                           out FileChecks: TFindFileChecks);
  procedure DateTimeOptionsToChecks(const SearchTemplate: TSearchTemplateRec;
                                    var FileChecks: TFindFileChecks);

  function CheckPlugin(const SearchTemplate: TSearchTemplateRec; const FileName: String) : Boolean;
  function CheckDirectoryName(const FileChecks: TFindFileChecks; const DirectoryName: String) : Boolean;
  function CheckDirectoryNameRelative(const FileChecks: TFindFileChecks; const FullPath, BasePath: String) : Boolean;
  function CheckFileName(const FileChecks: TFindFileChecks; const FileName: String) : Boolean;
  function CheckFileTime(const FileChecks: TFindFileChecks; FT : TFileTime) : Boolean; inline;
  function CheckFileDateTime(const FileChecks: TFindFileChecks; DT : TDateTime) : Boolean;
  function CheckFileSize(const FileChecks: TFindFileChecks; FileSize : Int64) : Boolean;
  function CheckFileAttributes(const FileChecks: TFindFileChecks; Attrs : TFileAttrs) : Boolean;
  function CheckFile(const SearchTemplate: TSearchTemplateRec; const FileChecks: TFindFileChecks; const AFile: TFile) : Boolean;
  procedure AttrsPatternOptionsToChecks(const SearchTemplate: TSearchTemplateRec; var FileChecks: TFindFileChecks);

implementation

uses
  strutils, DateUtils, DCDateTimeUtils, DCFileAttributes, RegExpr, uMasks,
  DCStrUtils, DCUnicodeUtils, uFileProperty, uGlobs, uWDXModule, LazUTF8,
  WdxPlugin, uRegExprW;

const
  cKilo = 1024;
  cMega = 1024 * cKilo;
  cGiga = 1024 * cMega;
  cTera = 1024 * cGiga;

procedure FileMaskOptionsToChecks(const SearchTemplate: TSearchTemplateRec;
                                  var FileChecks: TFindFileChecks);
var
  sMask, sTemp: String;
begin
  FileChecks.FilesMasks := SearchTemplate.FilesMasks;
  if SearchTemplate.IsPartialNameSearch then
  begin
    sTemp:= EmptyStr;
    while (Length(FileChecks.FilesMasks) > 0) do
    begin
      sMask:= Copy2SymbDel(FileChecks.FilesMasks, ';');
      if not ContainsOneOf(sMask, '*?') then
      begin
        if Length(sMask) = 0 then
          sMask:= AllFilesMask
        else begin
          sMask:= '*' + sMask + '*';
        end;
      end;
      sTemp:= sTemp + sMask + ';';
    end;
    if (Length(sTemp) = 0) then
      FileChecks.FilesMasks := AllFilesMask
    else
      FileChecks.FilesMasks := Copy(sTemp, 1, Length(sTemp) - 1);
  end;
end;

procedure DateTimeOptionsToChecks(const SearchTemplate: TSearchTemplateRec;
                                  var FileChecks: TFindFileChecks);
begin
  with FileChecks do
  begin
    if SearchTemplate.IsNotOlderThan then
    begin
      DateTimeFrom := SysUtils.Now;
      DateTimeTo   := MaxDateTime;

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
        DateTimeFrom := MinDateTime;

      if SearchTemplate.IsDateTo then
      begin
        if SearchTemplate.IsTimeTo then
          DateTimeTo := SearchTemplate.DateTimeTo
        else
          DateTimeTo := Trunc(SearchTemplate.DateTimeTo) + Frac(MaxDateTime);
      end
      else if SearchTemplate.IsTimeTo then
        DateTimeTo := Frac(SearchTemplate.DateTimeTo)
      else
        DateTimeTo := MaxDateTime;
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
  FileChecks.ExcludeFiles := SearchTemplate.ExcludeFiles;
  FileChecks.ExcludeDirectories := SearchTemplate.ExcludeDirectories;
  FileChecks.RegExp := SearchTemplate.RegExp;
  FileMaskOptionsToChecks(SearchTemplate, FileChecks);
  DateTimeOptionsToChecks(SearchTemplate, FileChecks);
  FileSizeOptionsToChecks(SearchTemplate, FileChecks);
  AttrsPatternOptionsToChecks(SearchTemplate, FileChecks);
end;

function CheckPluginFullText(Module: TWdxModule; constref ContentPlugin: TPluginSearchRec;
  const FileName: String): Boolean;
var
  Value: String;
  Old: String = '';
  FindText: String;
  FieldIndex: Integer;
  UnitIndex: Integer = 0;
begin
  // Prepare find text
  case ContentPlugin.Compare of
    poContainsCaseInsensitive,
    poNotContainsCaseInsensitive: FindText := UTF8LowerCase(ContentPlugin.Value);
    else FindText:= ContentPlugin.Value;
  end;
  // Find field index
  FieldIndex:= Module.GetFieldIndex(ContentPlugin.Field);
  Value:= Module.CallContentGetValue(FileName, FieldIndex, UnitIndex);
  while Length(Value) > 0 do
  begin
    Old+= Value;
    DCUnicodeUtils.Utf8FixBroken(Old);
    case ContentPlugin.Compare of
      poContainsCaseSensitive: Result := Pos(FindText, Old) > 0;
      poNotContainsCaseSensitive: Result := Pos(FindText, Old) = 0;
      poContainsCaseInsensitive: Result := Pos(FindText, UTF8LowerCase(Old)) > 0;
      poNotContainsCaseInsensitive: Result := Pos(FindText, UTF8LowerCase(Old)) = 0;
    end;
    if Result then begin
       Module.CallContentGetValue(FileName, FieldIndex, -1, 0);
       Exit;
    end;
    Old:= RightStr(Value, Length(FindText));
    Value:= Module.CallContentGetValue(FileName, FieldIndex, UnitIndex);
  end;
  Result:= False;
end;

function CheckPlugin(const SearchTemplate: TSearchTemplateRec;
  const FileName: String): Boolean;
var
  I: Integer;
  Work: Boolean;
  Value: Variant;
  Module: TWdxModule;
begin
  Result := SearchTemplate.ContentPluginCombine;
  for I:= Low(SearchTemplate.ContentPlugins) to High(SearchTemplate.ContentPlugins) do
  with SearchTemplate do
  begin
    Module := gWDXPlugins.GetWdxModule(ContentPlugins[I].Plugin);
    if (Module = nil) or (not Module.IsLoaded) then Continue;
    if ContentPlugins[I].FieldType in [ft_fulltext, ft_fulltextw] then
      Work:= CheckPluginFullText(Module, ContentPlugins[I], FileName)
    else begin
      Value:= Module.CallContentGetValueV(FileName, ContentPlugins[I].Field, ContentPlugins[I].UnitName, 0);
      case ContentPlugins[I].Compare of
        poEqualCaseSensitive: Work:= (ContentPlugins[I].Value = Value);
        poNotEqualCaseSensitive: Work:= (ContentPlugins[I].Value <> Value);
        poMore: Work := (Value > ContentPlugins[I].Value);
        poLess: Work := (Value < ContentPlugins[I].Value);
        poMoreEqual: Work := (Value >= ContentPlugins[I].Value);
        poLessEqual: Work := (Value <= ContentPlugins[I].Value);
        poEqualCaseInsensitive: Work:= UTF8CompareText(Value, ContentPlugins[I].Value) = 0;
        poNotEqualCaseInsensitive: Work:= UTF8CompareText(Value, ContentPlugins[I].Value) <> 0;
        poContainsCaseSensitive: Work := UTF8Pos(ContentPlugins[I].Value, Value) > 0;
        poNotContainsCaseSensitive: Work := UTF8Pos(ContentPlugins[I].Value, Value) = 0;
        poContainsCaseInsensitive: Work := UTF8Pos(UTF8LowerCase(ContentPlugins[I].Value), UTF8LowerCase(Value)) > 0;
        poNotContainsCaseInsensitive: Work := UTF8Pos(UTF8LowerCase(ContentPlugins[I].Value), UTF8LowerCase(Value)) = 0;
        poRegExpr: Work := ExecRegExpr(UTF8ToUTF16(ContentPlugins[I].Value), UTF8ToUTF16(Value));
        poNotRegExpr: Work := not ExecRegExpr(UTF8ToUTF16(ContentPlugins[I].Value), UTF8ToUTF16(Value));
      end;
    end;
    if ContentPluginCombine then
      Result := Result and Work
    else
      Result := Result or Work;
  end;
end;

function CheckDirectoryName(const FileChecks: TFindFileChecks; const DirectoryName: String): Boolean;
begin
  with FileChecks do
  begin
    Result := not MatchesMaskList(DirectoryName, ExcludeDirectories);
  end;
end;

function CheckDirectoryNameRelative(const FileChecks: TFindFileChecks; const FullPath, BasePath: String): Boolean;
begin
  Result := True;
  with FileChecks do
  begin
    // Check if FullPath is a path relative to BasePath.
    if GetPathType(ExcludeDirectories) = ptRelative then
    begin
      Result := ExcludeDirectories <> ExtractDirLevel(BasePath, FullPath);
    end;
  end;
end;

function CheckFileName(const FileChecks: TFindFileChecks; const FileName: String): Boolean;
begin
  with FileChecks do
  begin
    if RegExp then
    begin
      Result := ((FilesMasks = '') or ExecRegExpr(FilesMasks, FileName)) and
                ((ExcludeFiles = '') or not ExecRegExpr(ExcludeFiles, FileName));
    end
    else
    begin
      Result := MatchesMaskList(FileName, FilesMasks) and
                not MatchesMaskList(FileName, ExcludeFiles);
    end;
  end;
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

function CheckFile(const SearchTemplate: TSearchTemplateRec; const FileChecks: TFindFileChecks; const AFile: TFile): Boolean;
var
  IsDir: Boolean;
  DirectoryName: String;
begin
  Result := True;
  with SearchTemplate do
  begin
    IsDir := AFile.IsDirectory or AFile.IsLinkToDirectory;

    if (fpName in AFile.SupportedProperties) then
    begin
      DirectoryName:= ExtractFileName(ExcludeTrailingBackslash(AFile.Path));
      Result := CheckDirectoryName(FileChecks, DirectoryName);
      if Result then Result := CheckFileName(FileChecks, AFile.Name);
    end;

    if Result and (fpModificationTime in AFile.SupportedProperties) then
      if (IsDateFrom or IsDateTo or IsTimeFrom or IsTimeTo or IsNotOlderThan) then
        Result:= CheckFileDateTime(FileChecks, AFile.ModificationTime);

    if Result and not IsDir and (fpSize in AFile.SupportedProperties) then
      if (IsFileSizeFrom or IsFileSizeTo) then
        Result:= CheckFileSize(FileChecks, AFile.Size);

    if Result and (fpAttributes in AFile.SupportedProperties) then
    begin
      if AFile.AttributesProperty.IsNativeAttributes then
        Result:= CheckFileAttributes(FileChecks, AFile.Attributes)
      else if (Length(FileChecks.Attributes) > 0) then
        Result:= False;
    end;

    if Result and ContentPlugin then
    begin
      Result:= CheckPlugin(SearchTemplate, AFile.FullPath);
    end;
  end;
end;

end.
