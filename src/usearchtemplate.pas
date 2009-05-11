{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save search templates

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit uSearchTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDSXPlugin, uClassesEx, uTypes;

type

  { TSearchTemplate }

  TSearchTemplate = class
  private
    FTemplateName,
    FStartPath: UTF8String;
    FIsNotOlderThan: Boolean;
    FNotOlderThan: Double;
  public
    constructor Create;
    SearchRecord: TSearchAttrRecord;
    function CheckFile(const FileRecItem: TFileRecItem): Boolean;
    property TemplateName: UTF8String read FTemplateName write FTemplateName;
    property StartPath: UTF8String read FStartPath write FStartPath;
    property IsNotOlderThan: Boolean read FIsNotOlderThan write FIsNotOlderThan;
    property NotOlderThan: Double read FNotOlderThan write FNotOlderThan;
  end;

  { TSearchTemplateList }

  TSearchTemplateList = class(TList)
  private
    function GetTemplate(Index: Integer): TSearchTemplate;
  public
    function Add(SearchTemplate: TSearchTemplate): Integer;
    procedure DeleteTemplate(Index: Integer);
    procedure LoadToStringList(StringList: TStrings);
    procedure LoadFromIni(IniFile: TIniFileEx);
    procedure SaveToIni(IniFile: TIniFileEx);
    property Templates[Index: Integer]: TSearchTemplate read GetTemplate;
  end;

implementation

uses
  DateUtils;

function CheckFileDate(const SearchRecord: TSearchAttrRecord; DateTime: TDateTime): Boolean;
begin
  Result:= True;
  with SearchRecord do
  begin
    (* Check date from *)
    if rIsDateFrom then
      Result:= (Int(DateTime) >= Int(rDateTimeFrom));

    (* Check time to *)
    if (rIsDateTo and Result) then
      Result:= (Int(DateTime) <= Int(rDateTimeTo));

    (* Check time from *)         //TODO seconds
    if (rIsTimeFrom and Result) then
      Result:= ((Trunc(Frac(DateTime) * 10000000) / 10000000) >= (Trunc(Frac(rDateTimeFrom) * 1000) / 1000));

    //DebugLn('Time From = ', FloatToStr(rDateTimeFrom), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));

    (* Check time to *)
    if (rIsTimeTo and Result) then
      Result:= ((Trunc(Frac(DateTime) * 10000000) / 10000000) <= (Trunc(Frac(rDateTimeTo) * 1000) / 1000));

    //DebugLn('Time To = ', FloatToStr(rDateTimeTo), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));
  end;
end;

function CheckFileSize(const SearchRecord: TSearchAttrRecord; FileSize: Int64): Boolean;
begin
   Result:= True;
   with SearchRecord do
   begin
     if rIsFileSizeFrom then
       Result:= (FileSize >= rFileSizeFrom);
     //DebugLn('After From', FileSize, '-',  rFileSizeFrom, BoolToStr(Result));

     if (rIsFileSizeTo and Result) then
       Result:= (FileSize <= rFileSizeTo);
     //DebugLn('After To',  FileSize, '-',  rFileSizeTo, BoolToStr(Result));
  end;
end;

{ TSearchTemplate }

constructor TSearchTemplate.Create;
begin
  inherited Create;
  FillByte(SearchRecord, SizeOf(SearchRecord), 0);
end;

function TSearchTemplate.CheckFile(const FileRecItem: TFileRecItem): Boolean;
var
  dtNow: TDateTime;
  iCount: Integer;
begin
  Result:= True;
  with SearchRecord do
  begin
    if FIsNotOlderThan then
      begin
        dtNow:= Now;
        iCount:= Trunc(FNotOlderThan);
        case Trunc(Frac(FNotOlderThan)*10) of
        0:  //Minute(s)
          begin
            rIsTimeFrom:= True;
            rDateTimeFrom:= dtNow - OneMinute * iCount;
          end;
        1:  //Hour(s)
          begin
            rIsTimeFrom:= True;
            rDateTimeFrom:= dtNow - OneHour * iCount;
          end;
        2:  //Day(s)
          begin
            rIsDateFrom:= True;
            rDateTimeFrom:= dtNow - iCount;
          end;
        3:  //Week(s)
          begin
            rIsDateFrom:= True;
            rDateTimeFrom:= dtNow - DaysPerWeek * iCount;
          end;
        4:  //Month(s)
          begin
            rIsDateFrom:= True;
            rDateTimeFrom:= dtNow - DaysInMonth(dtNow) * iCount;
          end;
        5:  //Year(s)
          begin
            rIsDateFrom:= True;
            rDateTimeFrom:= dtNow - DaysInYear(dtNow) * iCount;
          end;
        end;
      end;

    if (rIsDateFrom or rIsDateTo or rIsTimeFrom or rIsTimeTo) then
      Result:= CheckFileDate(SearchRecord, FileRecItem.fTimeI);

    if (rIsFileSizeFrom or rIsFileSizeTo) and Result then
      Result := CheckFileSize(SearchRecord, FileRecItem.iSize);
  end;
end;

{ TSearchTemplateList }

function TSearchTemplateList.GetTemplate(Index: Integer): TSearchTemplate;
begin
  Result:= TSearchTemplate(Items[Index]);
end;

function TSearchTemplateList.Add(SearchTemplate: TSearchTemplate): Integer;
begin
  Result:= inherited Add(SearchTemplate);
end;

procedure TSearchTemplateList.DeleteTemplate(Index: Integer);
begin
  Templates[Index].Free;
  Delete(Index);
end;

procedure TSearchTemplateList.LoadToStringList(StringList: TStrings);
var
  I: Integer;
begin
  StringList.Clear;
  for I:= 0 to Count - 1 do
    StringList.Add(Templates[I].TemplateName);
end;

const
  cSection = 'SearchTemplates';

procedure TSearchTemplateList.LoadFromIni(IniFile: TIniFileEx);
var
  I, iCount: Integer;
  sTemplate: String;
  SearchTemplate: TSearchTemplate;
begin
  iCount:= IniFile.ReadInteger(cSection, 'TemplateCount', 0);
  for I:= 0 to iCount - 1 do
    begin
      SearchTemplate:= TSearchTemplate.Create;
      with SearchTemplate.SearchRecord do
      begin
        sTemplate:= 'Template' + IntToStr(I+1);
        SearchTemplate.TemplateName:= IniFile.ReadString(cSection, sTemplate+'Name', '');
        SearchTemplate.StartPath:= IniFile.ReadString(cSection, sTemplate+'StartPath', '');
        rFileMask:= StrNew(PChar(IniFile.ReadString(cSection, sTemplate+'FileMask', '*')));
        rAttributes:= IniFile.ReadInteger(cSection, sTemplate+'Attributes', faAnyFile);
        rAttribStr:= StrNew(PChar(IniFile.ReadString(cSection, sTemplate+'AttribStr', '*')));
        // date/time
        rCaseSens:= IniFile.ReadBool(cSection, sTemplate+'CaseSens', False);
        rIsDateFrom:= IniFile.ReadBool(cSection, sTemplate+'IsDateFrom', False);
        rIsDateTo:= IniFile.ReadBool(cSection, sTemplate+'IsDateTo', False);
        rIsTimeFrom:= IniFile.ReadBool(cSection, sTemplate+'IsTimeFrom', False);
        rIsTimeTo:= IniFile.ReadBool(cSection, sTemplate+'IsTimeTo', False);
        if rIsDateFrom or rIsTimeFrom then
          rDateTimeFrom:= IniFile.ReadDateTime(cSection, sTemplate+'DateTimeFrom', 0);
        if rIsDateTo or rIsTimeTo then
          rDateTimeTo:= IniFile.ReadDateTime(cSection, sTemplate+'DateTimeTo', Now);
        // not older then
        SearchTemplate.IsNotOlderThan:= IniFile.ReadBool(cSection, sTemplate+'IsNotOlderThan', False);
        if SearchTemplate.IsNotOlderThan then
          SearchTemplate.NotOlderThan:= IniFile.ReadFloat(cSection, sTemplate+'NotOlderThan', 0);
        // file size
        rIsFileSizeFrom:= IniFile.ReadBool(cSection, sTemplate+'IsFileSizeFrom', False);
        rIsFileSizeTo:= IniFile.ReadBool(cSection, sTemplate+'IsFileSizeTo', False);
        if rIsFileSizeFrom then
          rFileSizeFrom:= IniFile.ReadInteger(cSection, sTemplate+'FileSizeFrom', 0);
        if rIsFileSizeTo then
          rFileSizeTo:= IniFile.ReadInteger(cSection, sTemplate+'FileSizeTo', MaxInt);
        // find text
        rIsNoThisText:= IniFile.ReadBool(cSection, sTemplate+'IsNoThisText', False);
        rFindInFiles:= IniFile.ReadBool(cSection, sTemplate+'FindInFiles', False);
        if rFindInFiles then
          rFindData:= StrNew(PChar(IniFile.ReadString(cSection, sTemplate+'FindData', '')));
        // replace text
        rReplaceInFiles:= IniFile.ReadBool(cSection, sTemplate+'ReplaceInFiles', False);
        if rReplaceInFiles then
          rReplaceData:= StrNew(PChar(IniFile.ReadString(cSection, sTemplate+'ReplaceData', '')));
      end;
      Add(SearchTemplate)
    end;
end;

procedure TSearchTemplateList.SaveToIni(IniFile: TIniFileEx);
var
  I: Integer;
  sTemplate: String;
begin
  IniFile.EraseSection(cSection);
  IniFile.WriteInteger(cSection, 'TemplateCount', Count);
  for I:= 0 to Count - 1 do
    with Templates[I].SearchRecord do
    begin
      sTemplate:= 'Template' + IntToStr(I+1);
      IniFile.WriteString(cSection, sTemplate+'Name', Templates[I].TemplateName);
      IniFile.WriteString(cSection, sTemplate+'StartPath', Templates[I].StartPath);
      IniFile.WriteString(cSection, sTemplate+'FileMask', StrPas(rFileMask));
      IniFile.WriteInteger(cSection, sTemplate+'Attributes', rAttributes);
      IniFile.WriteString(cSection, sTemplate+'AttribStr', StrPas(rAttribStr));
      // date/time
      IniFile.WriteBool(cSection, sTemplate+'CaseSens', rCaseSens);
      IniFile.WriteBool(cSection, sTemplate+'IsDateFrom', rIsDateFrom);
      IniFile.WriteBool(cSection, sTemplate+'IsDateTo', rIsDateTo);
      IniFile.WriteBool(cSection, sTemplate+'IsTimeFrom', rIsTimeFrom);
      IniFile.WriteBool(cSection, sTemplate+'IsTimeTo', rIsTimeTo);
      if rIsDateFrom or rIsTimeFrom then
        IniFile.WriteDateTime(cSection, sTemplate+'DateTimeFrom', rDateTimeFrom);
      if rIsDateTo or rIsTimeTo then
        IniFile.WriteDateTime(cSection, sTemplate+'DateTimeTo', rDateTimeTo);
      // not older then
      IniFile.WriteBool(cSection, sTemplate+'IsNotOlderThan', Templates[I].IsNotOlderThan);
      if Templates[I].IsNotOlderThan then
        IniFile.WriteFloat(cSection, sTemplate+'NotOlderThan', Templates[I].NotOlderThan);
      // file size
      IniFile.WriteBool(cSection, sTemplate+'IsFileSizeFrom', rIsFileSizeFrom);
      IniFile.WriteBool(cSection, sTemplate+'IsFileSizeTo', rIsFileSizeTo);
      if rIsFileSizeFrom then
        IniFile.WriteInteger(cSection, sTemplate+'FileSizeFrom', rFileSizeFrom);
      if rIsFileSizeTo then
        IniFile.WriteInteger(cSection, sTemplate+'FileSizeTo', rFileSizeTo);
      // find text
      IniFile.WriteBool(cSection, sTemplate+'IsNoThisText', rIsNoThisText);
      IniFile.WriteBool(cSection, sTemplate+'FindInFiles', rFindInFiles);
      if rFindInFiles then
        IniFile.WriteString(cSection, sTemplate+'FindData', StrPas(rFindData));
      // replace text
      IniFile.WriteBool(cSection, sTemplate+'ReplaceInFiles', rReplaceInFiles);
      if rReplaceInFiles then
        IniFile.WriteString(cSection, sTemplate+'ReplaceData', StrPas(rReplaceData));
    end;
end;

end.

