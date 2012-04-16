{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save search templates

   Copyright (C) 2009-2011  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, DCClassesUtf8, uFile, DCXmlConfig, uFindFiles;

type

  { TSearchTemplate }

  TSearchTemplate = class
  private
    FTemplateName: UTF8String;
  public
    SearchRecord: TSearchTemplateRec;

    constructor Create;
    destructor Destroy; override;
    function CheckFile(const AFile: TFile): Boolean;
    property TemplateName: UTF8String read FTemplateName write FTemplateName;
  end;

  { TSearchTemplateList }

  TSearchTemplateList = class(TList)
  private
    function GetTemplate(Index: Integer): TSearchTemplate;
    function GetTemplate(const AName: UTF8String): TSearchTemplate;
  public
    procedure Clear; override;
    function Add(SearchTemplate: TSearchTemplate): Integer;
    procedure DeleteTemplate(Index: Integer);
    procedure LoadToStringList(StringList: TStrings);
    procedure LoadFromIni(IniFile: TIniFileEx);
    procedure LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToIni(IniFile: TIniFileEx);
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
    property TemplateByName[const AName: UTF8String]: TSearchTemplate read GetTemplate;
    property Templates[Index: Integer]: TSearchTemplate read GetTemplate;
  end;

const
  cTemplateSign = '>';

function IsMaskSearchTemplate(const sMask: UTF8String): Boolean; inline;

implementation

uses
  uMasks, uFileProperty;

function IsMaskSearchTemplate(const sMask: UTF8String): Boolean; inline;
begin
  Result:= (Length(sMask) > 0) and (sMask[1] = cTemplateSign);
end;

{ TSearchTemplate }

constructor TSearchTemplate.Create;
begin
  inherited Create;
  FillByte(SearchRecord, SizeOf(SearchRecord), 0);
end;

destructor TSearchTemplate.Destroy;
begin
  inherited Destroy;
end;

function TSearchTemplate.CheckFile(const AFile: TFile): Boolean;
var
  FileChecks: TFindFileChecks;
begin
  Result:= True;
  SearchTemplateToFindFileChecks(SearchRecord, FileChecks);
  with SearchRecord do
  begin
    if (fpName in AFile.SupportedProperties) then
    begin
      Result:= MatchesMaskList(AFile.Name, FilesMasks) and
               not MatchesMaskList(AFile.Name, ExcludeFiles);
    end;

    if Result and (fpModificationTime in AFile.SupportedProperties) then
      if (IsDateFrom or IsDateTo or IsTimeFrom or IsTimeTo or IsNotOlderThan) then
        Result:= CheckFileDateTime(FileChecks, AFile.ModificationTime);

    if Result and (fpSize in AFile.SupportedProperties) then
      if (IsFileSizeFrom or IsFileSizeTo) then
        Result:= CheckFileSize(FileChecks, AFile.Size);

    if Result and (fpAttributes in AFile.SupportedProperties) then
      Result:= CheckFileAttributes(FileChecks, AFile.Attributes);
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

procedure TSearchTemplateList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Templates[i].Free;
  inherited Clear;
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
  FloatNotOlderThan: Double;
begin
  Clear;

  iCount:= IniFile.ReadInteger(cSection, 'TemplateCount', 0);
  for I:= 0 to iCount - 1 do
    begin
      SearchTemplate:= TSearchTemplate.Create;
      with SearchTemplate.SearchRecord do
      begin
        sTemplate:= 'Template' + IntToStr(I+1);
        SearchTemplate.TemplateName:= IniFile.ReadString(cSection, sTemplate+'Name', '');
        StartPath:= IniFile.ReadString(cSection, sTemplate+'StartPath', '');
        FilesMasks:= IniFile.ReadString(cSection, sTemplate+'FileMask', '*');
        //Attributes:= IniFile.ReadInteger(cSection, sTemplate+'Attributes', faAnyFile);
        //AttributesPattern:= IniFile.ReadString(cSection, sTemplate+'AttribStr', '');
        // date/time
        CaseSensitive:= IniFile.ReadBool(cSection, sTemplate+'CaseSens', False);
        IsDateFrom:= IniFile.ReadBool(cSection, sTemplate+'IsDateFrom', False);
        IsDateTo:= IniFile.ReadBool(cSection, sTemplate+'IsDateTo', False);
        IsTimeFrom:= IniFile.ReadBool(cSection, sTemplate+'IsTimeFrom', False);
        IsTimeTo:= IniFile.ReadBool(cSection, sTemplate+'IsTimeTo', False);
        if IsDateFrom or IsTimeFrom then
          DateTimeFrom:= IniFile.ReadDateTime(cSection, sTemplate+'DateTimeFrom', 0);
        if IsDateTo or IsTimeTo then
          DateTimeTo:= IniFile.ReadDateTime(cSection, sTemplate+'DateTimeTo', Now);
        // not older than
        IsNotOlderThan:= IniFile.ReadBool(cSection, sTemplate+'IsNotOlderThan', False);
        if IsNotOlderThan then
        begin
          FloatNotOlderThan:= IniFile.ReadFloat(cSection, sTemplate+'NotOlderThan', 0);
          NotOlderThan:= Trunc(FloatNotOlderThan);
          NotOlderThanUnit:= TTimeUnit(Round(Frac(FloatNotOlderThan)*10) + 1);
        end;
        // file size
        IsFileSizeFrom:= IniFile.ReadBool(cSection, sTemplate+'IsFileSizeFrom', False);
        IsFileSizeTo:= IniFile.ReadBool(cSection, sTemplate+'IsFileSizeTo', False);
        if IsFileSizeFrom then
          FileSizeFrom:= IniFile.ReadInteger(cSection, sTemplate+'FileSizeFrom', 0);
        if IsFileSizeTo then
          FileSizeTo:= IniFile.ReadInteger(cSection, sTemplate+'FileSizeTo', MaxInt);
        // find text
        NotContainingText:= IniFile.ReadBool(cSection, sTemplate+'IsNoThisText', False);
        IsFindText:= IniFile.ReadBool(cSection, sTemplate+'FindInFiles', False);
        if IsFindText then
          FindText:= IniFile.ReadString(cSection, sTemplate+'FindData', '');
        // replace text
        IsReplaceText:= IniFile.ReadBool(cSection, sTemplate+'ReplaceInFiles', False);
        if IsReplaceText then
          ReplaceText:= IniFile.ReadString(cSection, sTemplate+'ReplaceData', '');
      end;
      Add(SearchTemplate);
    end;
end;

procedure TSearchTemplateList.LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  SearchTemplate: TSearchTemplate;
  FloatNotOlderThan: Double;
begin
  Clear;

  ANode := ANode.FindNode(cSection);
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('Template') = 0 then
      begin
        SearchTemplate:= TSearchTemplate.Create;
        with SearchTemplate.SearchRecord do
        begin
          SearchTemplate.TemplateName:= AConfig.GetValue(ANode, 'Name', '');
          StartPath:= AConfig.GetValue(ANode, 'StartPath', '');
          ExcludeDirectories:= AConfig.GetValue(ANode, 'ExcludeDirectories', '');
          FilesMasks:= AConfig.GetValue(ANode, 'FilesMasks', '*');
          ExcludeFiles:= AConfig.GetValue(ANode, 'ExcludeFiles', '');
          SearchDepth:= AConfig.GetValue(ANode, 'SearchDepth', -1);
          IsPartialNameSearch:= AConfig.GetValue(ANode, 'IsPartialNameSearch', True);
          RegExp:= AConfig.GetValue(ANode, 'RegExp', False);
          FollowSymLinks:= AConfig.GetValue(ANode, 'FollowSymLinks', False);
          AttributesPattern:= AConfig.GetValue(ANode, 'AttributesPattern', '');
          // date/time
          IsDateFrom:= AConfig.GetValue(ANode, 'IsDateFrom', False);
          IsDateTo:= AConfig.GetValue(ANode, 'IsDateTo', False);
          IsTimeFrom:= AConfig.GetValue(ANode, 'IsTimeFrom', False);
          IsTimeTo:= AConfig.GetValue(ANode, 'IsTimeTo', False);
          if IsDateFrom or IsTimeFrom then
            DateTimeFrom:= AConfig.GetValue(ANode, 'DateTimeFrom', TDateTime(0));
          if IsDateTo or IsTimeTo then
            DateTimeTo:= AConfig.GetValue(ANode, 'DateTimeTo', Now);
          // not older than
          IsNotOlderThan:= AConfig.GetValue(ANode, 'IsNotOlderThan', False);
          if IsNotOlderThan then
          begin
            // Workaround because old value was floating point.
            FloatNotOlderThan:= AConfig.GetValue(ANode, 'NotOlderThan', Double(0));
            NotOlderThan:= Trunc(FloatNotOlderThan);
            NotOlderThanUnit:= TTimeUnit(AConfig.GetValue(ANode, 'NotOlderThanUnit', 0));
          end;
          // file size
          IsFileSizeFrom:= AConfig.GetValue(ANode, 'IsFileSizeFrom', False);
          IsFileSizeTo:= AConfig.GetValue(ANode, 'IsFileSizeTo', False);
          if IsFileSizeFrom then
            FileSizeFrom:= AConfig.GetValue(ANode, 'FileSizeFrom', Int64(0));
          if IsFileSizeTo then
            FileSizeTo:= AConfig.GetValue(ANode, 'FileSizeTo', High(Int64));
          FileSizeUnit:= TFileSizeUnit(AConfig.GetValue(ANode, 'FileSizeUnit', 0));
          // find text
          IsFindText:= AConfig.GetValue(ANode, 'IsFindText', False);
          if IsFindText then
            FindText:= AConfig.GetValue(ANode, 'FindText', '');
          // replace text
          IsReplaceText:= AConfig.GetValue(ANode, 'IsReplaceText', False);
          if IsReplaceText then
            ReplaceText:= AConfig.GetValue(ANode, 'ReplaceText', '');
          CaseSensitive:= AConfig.GetValue(ANode, 'CaseSensitive', False);
          NotContainingText:= AConfig.GetValue(ANode, 'NotContainingText', False);
          TextEncoding:= AConfig.GetValue(ANode, 'TextEncoding', '');
          SearchPlugin:= AConfig.GetValue(ANode, 'SearchPlugin', '');
        end;
        Add(SearchTemplate);
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TSearchTemplateList.SaveToIni(IniFile: TIniFileEx);
var
  I: Integer;
  sTemplate: String;
  FloatNotOlderThan: Double;
begin
  IniFile.EraseSection(cSection);
  IniFile.WriteInteger(cSection, 'TemplateCount', Count);
  for I:= 0 to Count - 1 do
    with Templates[I].SearchRecord do
    begin
      sTemplate:= 'Template' + IntToStr(I+1);
      IniFile.WriteString(cSection, sTemplate+'Name', Templates[I].TemplateName);
      IniFile.WriteString(cSection, sTemplate+'StartPath', StartPath);
      IniFile.WriteString(cSection, sTemplate+'FileMask', FilesMasks);
      //IniFile.WriteInteger(cSection, sTemplate+'Attributes', Attributes);
      //IniFile.WriteString(cSection, sTemplate+'AttribStr', AttributesPattern);
      // date/time
      IniFile.WriteBool(cSection, sTemplate+'CaseSens', CaseSensitive);
      IniFile.WriteBool(cSection, sTemplate+'IsDateFrom', IsDateFrom);
      IniFile.WriteBool(cSection, sTemplate+'IsDateTo', IsDateTo);
      IniFile.WriteBool(cSection, sTemplate+'IsTimeFrom', IsTimeFrom);
      IniFile.WriteBool(cSection, sTemplate+'IsTimeTo', IsTimeTo);
      if IsDateFrom or IsTimeFrom then
        IniFile.WriteDateTime(cSection, sTemplate+'DateTimeFrom', DateTimeFrom);
      if IsDateTo or IsTimeTo then
        IniFile.WriteDateTime(cSection, sTemplate+'DateTimeTo', DateTimeTo);
      // not older than
      IniFile.WriteBool(cSection, sTemplate+'IsNotOlderThan', IsNotOlderThan);
      if IsNotOlderThan then
      begin
        FloatNotOlderThan := Double(NotOlderThan) + Double(Integer(NotOlderThanUnit) - 1) / 10;
        IniFile.WriteFloat(cSection, sTemplate+'NotOlderThan', FloatNotOlderThan);
      end;
      // file size
      IniFile.WriteBool(cSection, sTemplate+'IsFileSizeFrom', IsFileSizeFrom);
      IniFile.WriteBool(cSection, sTemplate+'IsFileSizeTo', IsFileSizeTo);
      if IsFileSizeFrom then
        IniFile.WriteInteger(cSection, sTemplate+'FileSizeFrom', FileSizeFrom);
      if IsFileSizeTo then
        IniFile.WriteInteger(cSection, sTemplate+'FileSizeTo', FileSizeTo);
      // find text
      IniFile.WriteBool(cSection, sTemplate+'IsNoThisText', NotContainingText);
      IniFile.WriteBool(cSection, sTemplate+'FindInFiles', IsFindText);
      if IsFindText then
        IniFile.WriteString(cSection, sTemplate+'FindData', FindText);
      // replace text
      IniFile.WriteBool(cSection, sTemplate+'ReplaceInFiles', IsReplaceText);
      if IsReplaceText then
        IniFile.WriteString(cSection, sTemplate+'ReplaceData', ReplaceText);
    end;
end;

procedure TSearchTemplateList.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, cSection, True);
  AConfig.ClearNode(ANode);
  for I:= 0 to Count - 1 do
    with Templates[I].SearchRecord do
    begin
      SubNode := AConfig.AddNode(ANode, 'Template');
      AConfig.AddValue(SubNode, 'Name', Templates[I].TemplateName);
      AConfig.AddValue(SubNode, 'StartPath', StartPath);
      AConfig.AddValue(SubNode, 'ExcludeDirectories', ExcludeDirectories);
      AConfig.AddValue(SubNode, 'FilesMasks', FilesMasks);
      AConfig.AddValue(SubNode, 'ExcludeFiles', ExcludeFiles);
      AConfig.AddValue(SubNode, 'SearchDepth', SearchDepth);
      AConfig.AddValue(SubNode, 'IsPartialNameSearch', IsPartialNameSearch);
      AConfig.AddValue(SubNode, 'RegExp', RegExp);
      AConfig.AddValue(SubNode, 'FollowSymLinks', FollowSymLinks);
      AConfig.AddValue(SubNode, 'AttributesPattern', AttributesPattern);
      // date/time
      AConfig.AddValue(SubNode, 'IsDateFrom', IsDateFrom);
      AConfig.AddValue(SubNode, 'IsDateTo', IsDateTo);
      AConfig.AddValue(SubNode, 'IsTimeFrom', IsTimeFrom);
      AConfig.AddValue(SubNode, 'IsTimeTo', IsTimeTo);
      if IsDateFrom or IsTimeFrom then
        AConfig.AddValue(SubNode, 'DateTimeFrom', DateTimeFrom);
      if IsDateTo or IsTimeTo then
        AConfig.AddValue(SubNode, 'DateTimeTo', DateTimeTo);
      // not older than
      AConfig.AddValue(SubNode, 'IsNotOlderThan', IsNotOlderThan);
      if IsNotOlderThan then
      begin
        AConfig.AddValue(SubNode, 'NotOlderThan', NotOlderThan);
        AConfig.AddValue(SubNode, 'NotOlderThanUnit', Integer(NotOlderThanUnit));
      end;
      // file size
      AConfig.AddValue(SubNode, 'IsFileSizeFrom', IsFileSizeFrom);
      AConfig.AddValue(SubNode, 'IsFileSizeTo', IsFileSizeTo);
      if IsFileSizeFrom then
        AConfig.AddValue(SubNode, 'FileSizeFrom', FileSizeFrom);
      if IsFileSizeTo then
        AConfig.AddValue(SubNode, 'FileSizeTo', FileSizeTo);
      AConfig.AddValue(SubNode, 'FileSizeUnit', Integer(FileSizeUnit));
      // find text
      AConfig.AddValue(SubNode, 'IsFindText', IsFindText);
      if IsFindText then
        AConfig.AddValue(SubNode, 'FindText', FindText);
      // replace text
      AConfig.AddValue(SubNode, 'IsReplaceText', IsReplaceText);
      if IsReplaceText then
        AConfig.AddValue(SubNode, 'ReplaceText', ReplaceText);
      AConfig.AddValue(SubNode, 'CaseSensitive', CaseSensitive);
      AConfig.AddValue(SubNode, 'NotContainingText', NotContainingText);
      AConfig.AddValue(SubNode, 'TextEncoding', TextEncoding);
      AConfig.AddValue(SubNode, 'SearchPlugin', SearchPlugin);
    end;
end;

end.

