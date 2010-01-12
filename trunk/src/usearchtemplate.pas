{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save search templates

   Copyright (C) 2009-2010  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, DsxPlugin, uClassesEx, uFile;

type

  { TSearchTemplate }

  TSearchTemplate = class
  private
    FTemplateName,
    FStartPath: UTF8String;
    FIsNotOlderThan: Boolean;
    FNotOlderThan: Double;
    function CheckFileDate(DateTime: TDateTime): Boolean;
    function CheckFileSize(FileSize: Int64): Boolean;
  public
    SearchRecord: TSearchAttrRecord;

    constructor Create;
    function CheckFile(const AFile: TFile): Boolean;
    property TemplateName: UTF8String read FTemplateName write FTemplateName;
    property StartPath: UTF8String read FStartPath write FStartPath;
    property IsNotOlderThan: Boolean read FIsNotOlderThan write FIsNotOlderThan;
    property NotOlderThan: Double read FNotOlderThan write FNotOlderThan;
  end;

  { TSearchTemplateList }

  TSearchTemplateList = class(TList)
  private
    function GetTemplate(Index: Integer): TSearchTemplate;
    function GetTemplate(const AName: UTF8String): TSearchTemplate;
  public
    function Add(SearchTemplate: TSearchTemplate): Integer;
    procedure DeleteTemplate(Index: Integer);
    procedure LoadToStringList(StringList: TStrings);
    procedure LoadFromIni(IniFile: TIniFileEx);
    procedure SaveToIni(IniFile: TIniFileEx);
    property TemplateByName[const AName: UTF8String]: TSearchTemplate read GetTemplate;
    property Templates[Index: Integer]: TSearchTemplate read GetTemplate;
  end;

const
  cTemplateSign = '>';

function IsMaskSearchTemplate(const sMask: UTF8String): Boolean; inline;

implementation

uses
  DateUtils, Masks, uFileProperty;

function IsMaskSearchTemplate(const sMask: UTF8String): Boolean; inline;
begin
  Result:= (Length(sMask) > 0) and (sMask[1] = cTemplateSign);
end;

{ TSearchTemplate }

function TSearchTemplate.CheckFileDate(DateTime: TDateTime): Boolean;
var
  dtNow: TDateTime;
  iCount: Integer;
  bIsDateFrom,
  bIsTimeFrom: Boolean;
begin
  Result:= True;
  with SearchRecord do
  begin
    if FIsNotOlderThan then
      begin
        dtNow:= Now;
        iCount:= -Trunc(FNotOlderThan);
        case Round(Frac(FNotOlderThan)*10) of
        0:  //Minute(s)
          begin
            bIsDateFrom:= True;
            bIsTimeFrom:= True;
            rDateTimeFrom:= IncMinute(dtNow, iCount);
          end;
        1:  //Hour(s)
          begin
            bIsDateFrom:= True;
            bIsTimeFrom:= True;
            rDateTimeFrom:= IncHour(dtNow, iCount);
          end;
        2:  //Day(s)
          begin
            bIsDateFrom:= True;
            rDateTimeFrom:= IncDay(dtNow, iCount);
          end;
        3:  //Week(s)
          begin
            bIsDateFrom:= True;
            rDateTimeFrom:= IncWeek(dtNow, iCount);
          end;
        4:  //Month(s)
          begin
            bIsDateFrom:= True;
            rDateTimeFrom:= IncMonth(dtNow, iCount);
          end;
        5:  //Year(s)
          begin
            bIsDateFrom:= True;
            rDateTimeFrom:= IncYear(dtNow, iCount);
          end;
        end;
      end;

    (* Check date from *)
    if rIsDateFrom or bIsDateFrom then
      Result:= (Int(DateTime) >= Int(rDateTimeFrom));

    (* Check time to *)
    if (rIsDateTo and Result) then
      Result:= (Int(DateTime) <= Int(rDateTimeTo));

    (* Check time from *)
    if ((rIsTimeFrom or bIsTimeFrom) and Result) then
      Result:= (CompareTime(DateTime, rDateTimeFrom) >= 0);

    //DebugLn('Time From = ', FloatToStr(rDateTimeFrom), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));

    (* Check time to *)
    if (rIsTimeTo and Result) then
      Result:= (CompareTime(DateTime, rDateTimeTo) <= 0);

    //DebugLn('Time To = ', FloatToStr(rDateTimeTo), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));
  end;
end;

function TSearchTemplate.CheckFileSize(FileSize: Int64): Boolean;
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

constructor TSearchTemplate.Create;
begin
  inherited Create;
  FillByte(SearchRecord, SizeOf(SearchRecord), 0);
end;

function TSearchTemplate.CheckFile(const AFile: TFile): Boolean;
begin
  Result:= True;
  with SearchRecord do
  begin
    if (fpName in AFile.GetSupportedProperties) then
      Result:= MatchesMaskList(AFile.Name, SearchRecord.rFileMask);

    if (fpModificationTime in AFile.GetSupportedProperties) then
      if (rIsDateFrom or rIsDateTo or rIsTimeFrom or rIsTimeTo or FIsNotOlderThan) then
        Result:= CheckFileDate((AFile.Properties[fpModificationTime] as TFileDateTimeProperty).Value);

    if (fpSize in AFile.GetSupportedProperties) then
      if (rIsFileSizeFrom or rIsFileSizeTo) and Result then
        Result:= CheckFileSize((AFile.Properties[fpSize] as TFileSizeProperty).Value);
  end;
end;

{ TSearchTemplateList }

function TSearchTemplateList.GetTemplate(Index: Integer): TSearchTemplate;
begin
  Result:= TSearchTemplate(Items[Index]);
end;

function TSearchTemplateList.GetTemplate(const AName: UTF8String): TSearchTemplate;
var
  I: Integer;
  sName: UTF8String;
begin
  Result:= nil;

  if IsMaskSearchTemplate(AName) then
    sName:= PChar(AName) + 1 // skip template sign
  else
    sName:= AName;

  for I:= 0 to Count - 1 do
    if SameText(TSearchTemplate(Items[I]).TemplateName, sName) then
      begin
        Result:= TSearchTemplate(Items[I]);
        Exit;
      end;
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
        // not older than
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
      // not older than
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

