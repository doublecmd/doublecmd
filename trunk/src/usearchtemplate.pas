{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save search templates

   Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

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
    FTemplateName: String;
    FSearchRecord: TSearchTemplateRec;
    FFileChecks: TFindFileChecks;
    procedure MakeFileChecks;
    procedure SetSearchRecord(const AValue: TSearchTemplateRec);
  public
    constructor Create;
    function CheckFile(const AFile: TFile): Boolean;
    property SearchRecord: TSearchTemplateRec read FSearchRecord write SetSearchRecord;
    property TemplateName: String read FTemplateName write FTemplateName;
  end;

  { TSearchTemplateList }

  TSearchTemplateList = class(TList)
  private
    function GetTemplate(Index: Integer): TSearchTemplate;
    function GetTemplate(const AName: String): TSearchTemplate;
  public
    procedure Clear; override;
    function Add(SearchTemplate: TSearchTemplate): Integer;
    procedure DeleteTemplate(Index: Integer);
    procedure LoadToStringList(StringList: TStrings);
    procedure LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
    property TemplateByName[const AName: String]: TSearchTemplate read GetTemplate;
    property Templates[Index: Integer]: TSearchTemplate read GetTemplate;
  end;

const
  cTemplateSign = '>';

function IsMaskSearchTemplate(const sMask: String): Boolean; inline;

implementation

uses
 Variants, DCFileAttributes, DCBasicTypes, WdxPlugin, uWdxModule;

function IsMaskSearchTemplate(const sMask: String): Boolean; inline;
begin
  Result:= (Length(sMask) > 0) and (sMask[1] = cTemplateSign);
end;

{ TSearchTemplate }

constructor TSearchTemplate.Create;
begin
  inherited Create;
  FillByte(FSearchRecord, SizeOf(FSearchRecord), 0);
end;

procedure TSearchTemplate.MakeFileChecks;
begin
  SearchTemplateToFindFileChecks(FSearchRecord, FFileChecks);
end;

procedure TSearchTemplate.SetSearchRecord(const AValue: TSearchTemplateRec);
begin
  FSearchRecord := AValue;
  MakeFileChecks;
end;

function TSearchTemplate.CheckFile(const AFile: TFile): Boolean;
begin
  // If template has IsNotOlderThan option then DateTime checks must be recalculated
  // everytime because they depend on current time.
  if FSearchRecord.IsNotOlderThan then
    DateTimeOptionsToChecks(FSearchRecord, FFileChecks);
  Result := uFindFiles.CheckFile(FSearchRecord, FFileChecks, AFile);
end;

{ TSearchTemplateList }

function TSearchTemplateList.GetTemplate(Index: Integer): TSearchTemplate;
begin
  Result:= TSearchTemplate(Items[Index]);
end;

function TSearchTemplateList.GetTemplate(const AName: String): TSearchTemplate;
var
  I: Integer;
  sName: String;
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

procedure TSearchTemplateList.LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  Index: Integer;
  SearchTemplate: TSearchTemplate;
  FloatNotOlderThan: Double;
  Node: TXmlNode;
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
        with SearchTemplate.FSearchRecord do
        begin
          SearchTemplate.TemplateName:= AConfig.GetValue(ANode, 'Name', '');
          StartPath:= AConfig.GetValue(ANode, 'StartPath', '');
          ExcludeDirectories:= AConfig.GetValue(ANode, 'ExcludeDirectories', '');
          FilesMasks:= AConfig.GetValue(ANode, 'FilesMasks', '*');
          ExcludeFiles:= AConfig.GetValue(ANode, 'ExcludeFiles', '');
          SearchDepth:= AConfig.GetValue(ANode, 'SearchDepth', -1);
          IsPartialNameSearch:= AConfig.GetValue(ANode, 'IsPartialNameSearch', False);
          RegExp:= AConfig.GetValue(ANode, 'RegExp', False);
          FollowSymLinks:= AConfig.GetValue(ANode, 'FollowSymLinks', False);
          FindInArchives:= AConfig.GetValue(ANode, 'FindInArchives', False);
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
          HexValue:= AConfig.GetValue(ANode, 'HexValue', False);
          CaseSensitive:= AConfig.GetValue(ANode, 'CaseSensitive', False);
          NotContainingText:= AConfig.GetValue(ANode, 'NotContainingText', False);
          TextRegExp:= AConfig.GetValue(ANode, 'TextRegExp', False);
          TextEncoding:= AConfig.GetValue(ANode, 'TextEncoding', '');
          if TextEncoding = 'UTF-8BOM' then TextEncoding:= 'UTF-8';
          if TextEncoding = 'UCS-2LE' then TextEncoding:= 'UTF-16LE';
          if TextEncoding = 'UCS-2BE' then TextEncoding:= 'UTF-16BE';
          // duplicates
          Node := AConfig.FindNode(ANode, 'Duplicates', True);
          Duplicates:=  AConfig.GetAttr(Node, 'Enabled', False);
          if Duplicates then
          begin
            DuplicateName:= AConfig.GetValue(Node, 'Name', False);
            DuplicateSize:= AConfig.GetValue(Node, 'Size', False);
            DuplicateHash:= AConfig.GetValue(Node, 'Hash', False);
            DuplicateContent:= AConfig.GetValue(Node, 'Content', False);
          end;
          // plugins
          SearchPlugin:= AConfig.GetValue(ANode, 'SearchPlugin', '');
          Node := AConfig.FindNode(ANode, 'ContentPlugins', True);
          ContentPlugin:=  AConfig.GetAttr(Node, 'Enabled', False);
          if ContentPlugin then
          begin
            ContentPluginCombine:= AConfig.GetAttr(Node, 'Combine', True);
            Node := Node.FirstChild;
            while Assigned(Node) do
            begin
              if Node.CompareName('Plugin') = 0 then
              begin
                Index:= Length(ContentPlugins);
                SetLength(ContentPlugins, Index + 1);
                ContentPlugins[Index].Plugin:= AConfig.GetValue(Node, 'Name', EmptyStr);
                ContentPlugins[Index].Field:= AConfig.GetValue(Node, 'Field', EmptyStr);
                ContentPlugins[Index].UnitName:= AConfig.GetValue(Node, 'Unit', EmptyStr);
                ContentPlugins[Index].FieldType:= AConfig.GetValue(Node, 'FieldType', ft_string);
                ContentPlugins[Index].Compare:= TPluginOperator(AConfig.GetValue(Node, 'Compare', 0));
                ContentPlugins[Index].Value:= StrToVar(AConfig.GetValue(Node, 'Value', EmptyStr), ContentPlugins[Index].FieldType);
              end;
              Node := Node.NextSibling;
            end;
          end;
        end;
        SearchTemplate.MakeFileChecks;
        Add(SearchTemplate);
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TSearchTemplateList.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I, J: Integer;
  Node, SubNode: TXmlNode;
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
      AConfig.AddValue(SubNode, 'FindInArchives', FindInArchives);
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
      AConfig.AddValue(SubNode, 'HexValue', HexValue);
      AConfig.AddValue(SubNode, 'CaseSensitive', CaseSensitive);
      AConfig.AddValue(SubNode, 'NotContainingText', NotContainingText);
      AConfig.AddValue(SubNode, 'TextRegExp', TextRegExp);
      AConfig.AddValue(SubNode, 'TextEncoding', TextEncoding);
      // duplicates
      Node := AConfig.AddNode(SubNode, 'Duplicates');
      AConfig.SetAttr(Node, 'Enabled', Duplicates);
      if Duplicates then
      begin
        AConfig.AddValue(Node, 'Name', DuplicateName);
        AConfig.AddValue(Node, 'Size', DuplicateSize);
        AConfig.AddValue(Node, 'Hash', DuplicateHash);
        AConfig.AddValue(Node, 'Content', DuplicateContent);
      end;
      // plugins
      AConfig.AddValue(SubNode, 'SearchPlugin', SearchPlugin);
      Node := AConfig.FindNode(SubNode, 'ContentPlugins', True);
      AConfig.SetAttr(Node, 'Enabled', ContentPlugin);
      if ContentPlugin then
      begin
        AConfig.SetAttr(Node, 'Combine', ContentPluginCombine);
        for J:= Low(ContentPlugins) to High(ContentPlugins) do
        begin
          SubNode := AConfig.AddNode(Node, 'Plugin');
          AConfig.SetValue(SubNode, 'Name', ContentPlugins[J].Plugin);
          AConfig.SetValue(SubNode, 'Field', ContentPlugins[J].Field);
          AConfig.SetValue(SubNode, 'Unit', ContentPlugins[J].UnitName);
          AConfig.SetValue(SubNode, 'FieldType', ContentPlugins[J].FieldType);
          AConfig.SetValue(SubNode, 'Compare', Integer(ContentPlugins[J].Compare));
          AConfig.SetValue(SubNode, 'Value', VarToStr(ContentPlugins[J].Value));
        end;
      end;
    end;
end;

end.

