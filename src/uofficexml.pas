{
   Double commander
   -------------------------------------------------------------------------
   Load text from office xml (*.docx, *.odt)

   Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uOfficeXML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMasks;

var
  OfficeMask: TMaskList;

const
  OFFICE_FILTER = '(*.docx, *.xlsx, *.odt, *.ods)';

function LoadFromOffice(const FileName: String; out AText: String): Boolean;

implementation

uses
  Math, Unzip, ZipUtils, Laz2_DOM, laz2_XMLRead, fpsNumFormat, fpsCommon, fgl;

type
  TIntegerMap = class(specialize TFPGMap<Integer, TsNumFormatParams>);

function ExtractFile(ZipFile: unzFile; MemoryStream: TMemoryStream): Boolean;
var
  ASize: LongInt;
  FileInfo: unz_file_info;
begin
  Result:= unzGetCurrentFileInfo(ZipFile, @FileInfo, nil, 0, nil, 0, nil, 0) = UNZ_OK;
  if Result then
  begin
    MemoryStream.SetSize(FileInfo.uncompressed_size);
    if unzOpenCurrentFile(ZipFile) = UNZ_OK then
    begin
      ASize:= unzReadCurrentFile(ZipFile, MemoryStream.Memory, FileInfo.uncompressed_size);
      Result:= (ASize = FileInfo.uncompressed_size);
      unzCloseCurrentFile(ZipFile);
    end;
  end;
end;

{ Office Open XML }

procedure ProcessOfficeOpenNodes(var S: String; ANode: TDOMNode);
var
  I: Integer;
  ASubNode: TDOMNode;
  ANodeName: DOMString;
begin
  for I:= 0 to ANode.ChildNodes.Count - 1 do
  begin
    ASubNode := ANode.ChildNodes.Item[I];
    ANodeName := ASubNode.NodeName;

    if (ANodeName = 'w:t') then
    begin
      if Assigned(ASubNode.FirstChild) then
        S += ASubNode.FirstChild.NodeValue;
    end
    else if (ANodeName = 'w:p') then
      S += LineEnding + LineEnding
    else if (ANodeName = 'w:br') or (ANodeName = 'w:cr') then
      S += LineEnding
    else if (ANodeName = 'w:tab') then
      S += #9;

    if ASubNode.ChildNodes.Count > 0 then
      ProcessOfficeOpenNodes(S, ASubNode);
  end;
end;

procedure ProcessFile(ZipFile: unzFile; const FileName: String; var AText: String);
var
  ADoc: TXMLDocument;
  AStream: TMemoryStream;
begin
  if unzLocateFile(ZipFile, PAnsiChar(FileName), 0) = UNZ_OK then
  begin
    AStream:= TMemoryStream.Create;
    try
      if ExtractFile(ZipFile, AStream) then
      begin
        ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
        if Assigned (ADoc) then
        begin
          ProcessOfficeOpenNodes(AText, ADoc.DocumentElement);
          ADoc.Free;
        end;
      end;
    finally
      AStream.Free;
    end;
  end;
end;

function LoadFromOfficeOpen(const FileName: String; out AText: String): Boolean;
const
  HEADER_XML = 'word/header%d.xml';
  FOOTER_XML = 'word/footer%d.xml';
var
  Index: Integer;
  ZipFile: unzFile;
begin
  AText:= EmptyStr;
  ZipFile:= unzOpen(PAnsiChar(FileName));
  Result:= Assigned(ZipFile);
  if Result then
  try
    // Read headers
    for Index:= 0 to 9 do
    begin
      ProcessFile(ZipFile, Format(HEADER_XML, [Index]), AText);
    end;
    // Read body
    ProcessFile(ZipFile, 'word/document.xml', AText);
    // Read footers
    for Index:= 0 to 9 do
    begin
      ProcessFile(ZipFile, Format(FOOTER_XML, [Index]), AText);
    end;
    Result:= Length(AText) > 0;
  finally
    unzClose(ZipFile);
  end;
end;

{ Open Document Format }

procedure ProcessOpenOfficeNodes(var S: String; ANode: TDOMNode);
var
  I: Integer;
  ASubNode: TDOMNode;
  ANodeName: DOMString;

  procedure ParseSubNode(ANode: TDOMNode);
  var
    J: Integer;
    ASubNode: TDOMNode;
  begin
    for J:= 0 to ANode.ChildNodes.Count - 1 do
    begin
      ASubNode := ANode.ChildNodes.Item[J];
      ANodeName := ASubNode.NodeName;

      if ANodeName = 'text:s' then
        S += ' '
      else if ANodeName = 'text:tab' then
        S += #9
      else if ANodeName = 'text:line-break' then
        S += LineEnding
      else if (ASubNode.NodeType = TEXT_NODE) then
        S += ASubNode.NodeValue
      else begin
        ParseSubNode(ASubNode);
      end;
    end;
  end;

begin
  for I:= 0 to ANode.ChildNodes.Count - 1 do
  begin
    ASubNode := ANode.ChildNodes.Item[I];
    ANodeName := ASubNode.NodeName;

    if (ANodeName = 'text:p') or (ANodeName = 'text:h')then
    begin
      if ASubNode.ChildNodes.Count > 0 then
      begin
        ParseSubNode(ASubNode);
        S += LineEnding;
      end;
    end
    else if ASubNode.ChildNodes.Count > 0 then
      ProcessOpenOfficeNodes(S, ASubNode);
  end;
end;

function LoadFromOpenOffice(const FileName: String; out AText: String): Boolean;
const
  CONTENT_XML = 'content.xml';
var
  ZipFile: unzFile;
  ADoc: TXMLDocument;
  AStream: TMemoryStream;
begin
  Result:= False;
  AText:= EmptyStr;
  ZipFile:= unzOpen(PAnsiChar(FileName));
  if Assigned(ZipFile) then
  try
    if unzLocateFile(ZipFile, CONTENT_XML, 0) = UNZ_OK then
    begin
      AStream:= TMemoryStream.Create;
      try
        if ExtractFile(ZipFile, AStream) then
        begin
          ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
          if Assigned (ADoc) then
          begin
            ProcessOpenOfficeNodes(AText, ADoc.DocumentElement);
            ADoc.Free;
          end;
        end;
      finally
        AStream.Free;
      end;
    end;
    Result:= Length(AText) > 0;
  finally
    unzClose(ZipFile);
  end;
end;

{ Office Open XML Excel }

function FindNode(ANode: TDOMNode; const ANodeName: String): TDOMNode;
begin
  Result:= ANode.FindNode(ANodeName);
  if Result = nil then
    Result:= ANode.FindNode('x:' + ANodeName);
end;

function ParseSubNode(ANode: TDOMNode): String;
var
  ASubNode: TDOMNode;
begin
  Result:= EmptyStr;
  ASubNode:= ANode.FirstChild;
  while Assigned(ASubNode) do
  begin
    if (ASubNode.NodeType = TEXT_NODE) then
      Result+= ASubNode.NodeValue
    else begin
      Result+= ParseSubNode(ASubNode);
    end;
    ASubNode:= ASubNode.NextSibling;
  end;
end;

function GetAttrValue(ANode: TDOMNode; AName: String): String;
begin
  Result:= EmptyStr;
  if (ANode = nil) or (ANode.Attributes = nil) then Exit;
  ANode:= ANode.Attributes.GetNamedItem(AName);
  if Assigned(ANode) then Result:= ANode.NodeValue;
end;

procedure ParseStyles(ZipFile: unzFile; Styles: TIntegerMap; Storage: TsNumFormatList);
const
  STYLES_XML = 'xl/styles.xml';
var
  AName: String;
  Index: Integer;
  Style: Integer;
  ADoc: TXMLDocument;
  Formats: TStringList;
  AStream: TMemoryStream;
  ANode, ASubNode, AFormat: TDOMNode;
begin
  Formats:= TStringList.Create;
  try
    if unzLocateFile(ZipFile, STYLES_XML, 0) = UNZ_OK then
    begin
      AStream:= TMemoryStream.Create;
      try
        if ExtractFile(ZipFile, AStream) then
        begin
          ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
          if Assigned (ADoc) then
          begin
            AddBuiltInBiffFormats(Formats, FormatSettings, 163);

            ANode:= ADoc.DocumentElement;
            if Assigned(ANode) then
            begin
              ASubNode:= FindNode(ANode, 'numFmts');
              if Assigned(ASubNode) then
              begin
                  AFormat:= ASubNode.FirstChild;
                  while Assigned(AFormat) do
                  begin
                    AName:= AFormat.NodeName;
                    if (AName = 'numFmt') or (AName = 'x:numFmt') then
                    begin
                      AName:= GetAttrValue(AFormat, 'numFmtId');
                      if TryStrToInt(AName, Index) then
                      begin
                        while Formats.Count <= Index do
                          Formats.Add(EmptyStr);
                        Formats[Index]:= GetAttrValue(AFormat, 'formatCode');
                      end;
                    end;
                    AFormat:= AFormat.NextSibling;
                  end;
              end;
              ASubNode:= FindNode(ANode, 'cellXfs');
              if Assigned(ASubNode) then
              begin
                Style:= 0;
                AFormat:= ASubNode.FirstChild;
                while Assigned(AFormat) do
                begin
                  AName:= AFormat.NodeName;
                  if (AName = 'xf') or (AName = 'x:xf') then
                  begin
                    AName:= GetAttrValue(AFormat, 'numFmtId');
                    if TryStrToInt(AName, Index) then
                    begin
                      AName:= GetAttrValue(AFormat, 'applyNumberFormat');
                      if StrToBoolDef(AName, True) then
                      begin
                        if InRange(Index, 0, Formats.Count - 1) then
                        begin
                          AName:= Formats[Index];
                          if not SameText(AName, 'General') then
                          begin
                            Index:= Storage.AddFormat(AName);
                            Styles.Add(Style, Storage.Items[Index]);
                          end;
                        end;
                      end;
                    end;
                    Inc(Style);
                  end;
                  AFormat:= AFormat.NextSibling;
                end;
              end;
            end;
            ADoc.Free;
          end;
        end;
      finally
        AStream.Free;
      end;
    end;
  finally
    Formats.Free;
  end;
end;

function ParseWorkbook(ZipFile: unzFile; Sheets: TStringList): Boolean;
const
  CONTENT_XML = 'xl/workbook.xml';
var
  AName: String;
  ADoc: TXMLDocument;
  AStream: TMemoryStream;
  ANode, ASubNode: TDOMNode;
begin
  if unzLocateFile(ZipFile, CONTENT_XML, 0) = UNZ_OK then
  begin
    AStream:= TMemoryStream.Create;
    try
      if ExtractFile(ZipFile, AStream) then
      begin
        ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
        if Assigned(ADoc) then
        begin
          ANode:= FindNode(ADoc.DocumentElement, 'sheets');
          if Assigned(ANode) then
          begin
            ASubNode:= ANode.FirstChild;
            while Assigned(ASubNode) do
            begin
              AName:= ASubNode.NodeName;
              if (AName = 'sheet') or (AName = 'x:sheet') then
              begin
                AName:= GetAttrValue(ASubNode, 'name');
                Sheets.Add(AName);
              end;
              ASubNode:= ASubNode.NextSibling;
            end;
          end;
          ADoc.Free;
        end;
      end;
    finally
      AStream.Free;
    end;
  end;
  Result:= (Sheets.Count > 0);
end;

procedure ParseSharedStrings(ZipFile: unzFile; Strings: TStringList);
const
  STRINGS_XML = 'xl/sharedStrings.xml';
var
  AName: String;
  ADoc: TXMLDocument;
  AStream: TMemoryStream;
  ANode, ASubNode: TDOMNode;
begin
  if unzLocateFile(ZipFile, STRINGS_XML, 0) = UNZ_OK then
  begin
    AStream:= TMemoryStream.Create;
    try
      if ExtractFile(ZipFile, AStream) then
      begin
        ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
        if Assigned (ADoc) then
        begin
          ANode:= ADoc.DocumentElement;
          if Assigned(ANode) then
          begin
            ASubNode:= ANode.FirstChild;
            while Assigned(ASubNode) do
            begin
              AName:= ASubNode.NodeName;
              if (AName = 'si') or (AName = 'x:si') then
              begin
                Strings.Add(ParseSubNode(ASubNode));
              end;
              ASubNode:= ASubNode.NextSibling;
            end;
          end;
          ADoc.Free;
        end;
      end;
    finally
      AStream.Free;
    end;
  end;
end;

procedure ParseCell(ACell: TDOMNode; Strings: TStringList; Styles: TIntegerMap; var Text: String);
var
  D: Double;
  K: Integer;
  ATemp: String;
  AType: String;
  Index: Integer;
  AStyle: String;
  AValue: TDOMNode;
  F: TsNumFormatParams;
  Format: TFormatSettings;
begin
  AType:= GetAttrValue(ACell, 't');

  if (AType = 'inlineStr') then
    AValue:= FindNode(ACell, 'is')
  else begin
    AValue:= FindNode(ACell, 'v');
  end;

  if Assigned(AValue) then
  begin
    ATemp:= ParseSubNode(AValue);
    // Shared string
    if AType = 's' then
    begin
      K:= StrToIntDef(ATemp, -1);
      if InRange(K, 0, Strings.Count - 1) then
        Text+= Strings[K];
    end
    // Inline string or formula
    else if (AType = 'inlineStr') or (AType = 'str') then
    begin
     Text+= ATemp;
    end
    // Number or general
    else if (AType = 'n') or (AType = '') then
    begin
      AStyle:= GetAttrValue(ACell, 's');
      if not TryStrToInt(AStyle, K) then
        Text+= ATemp
      else begin
        Index:= Styles.IndexOf(K);
        if (Index < 0) then
          Text+= ATemp
        else begin
          F:= Styles.Data[Index];
          Format:= FormatSettings;
          Format.DecimalSeparator:= '.';

          if not TryStrToFloat(ATemp, D, Format) then
            Text+= ATemp
          else
            Text+= ConvertFloatToStr(D, F, FormatSettings);
        end;
      end;
    end;
  end;
end;

procedure ParseSheet(ZipFile: unzFile; Sheet: Integer; Strings: TStringList; Styles: TIntegerMap; var Text: String);
const
  SHEET_XML = 'xl/worksheets/sheet%d.xml';
var
  AName: String;
  ADoc: TXMLDocument;
  AStream: TMemoryStream;
  ANode, ARow, ACell: TDOMNode;
begin
  AName:= Format(SHEET_XML, [Sheet]);
  if unzLocateFile(ZipFile, PAnsiChar(AName), 0) = UNZ_OK then
  begin
    AStream:= TMemoryStream.Create;
    try
      if ExtractFile(ZipFile, AStream) then
      begin
        ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
        if Assigned(ADoc) then
        begin
          ANode:= FindNode(ADoc.DocumentElement, 'sheetData');
          if Assigned(ANode) then
          begin
            ARow:= ANode.FirstChild;
            while Assigned(ARow) do
            begin
              AName:= ARow.NodeName;
              if (AName = 'row') or (AName = 'x:row') then
              begin
                ACell:= ARow.FirstChild;
                while Assigned(ACell) do
                begin
                  AName:= ACell.NodeName;
                  if (AName = 'c') or (AName = 'x:c') then
                  begin
                    Text+= #26;
                    ParseCell(ACell, Strings, Styles, Text);
                  end;
                  ACell:= ACell.NextSibling;
                end;
                Text+= LineEnding;
              end;
              ARow:= ARow.NextSibling;
            end;
          end;
          ADoc.Free;
        end;
      end;
    finally
      AStream.Free;
    end;
  end;
end;

function LoadFromExcel(const FileName: String; out AText: String): Boolean;
var
  Index: Integer;
  ZipFile: unzFile;
  Styles: TIntegerMap;
  Storage: TsNumFormatList;
  Sheets, Strings: TStringList;
begin
  Result:= False;
  Sheets:= TStringList.Create;
  Styles:= TIntegerMap.Create;
  Strings:= TStringList.Create;
  Storage:= TsNumFormatList.Create(FormatSettings, True);
  try
    ZipFile:= unzOpen(PAnsiChar(FileName));
    if Assigned(ZipFile) then
    try
      if ParseWorkbook(ZipFile, Sheets) then
      begin
        AText:= EmptyStr;
        ParseSharedStrings(ZipFile, Strings);
        ParseStyles(ZipFile, Styles, Storage);
        for Index:= 0 to Sheets.Count - 1 do
        begin
          AText+= Sheets[Index] + LineEnding;
          ParseSheet(ZipFile, Index + 1, Strings, Styles, AText);
        end;
        Result:= Length(AText) > 0;
      end;
    finally
      unzClose(ZipFile);
    end;
  finally
    Sheets.Free;
    Styles.Free;
    Strings.Free;
    Storage.Free;
  end;
end;

function LoadFromOffice(const FileName: String; out AText: String): Boolean;
begin
  if SameText(ExtractFileExt(FileName), '.docx') then
    Result:= LoadFromOfficeOpen(FileName, AText)
  else if SameText(ExtractFileExt(FileName), '.xlsx') then
    Result:= LoadFromExcel(FileName, AText)
  else
    Result:= LoadFromOpenOffice(FileName, AText);
end;

initialization
  OfficeMask:= TMaskList.Create('*.docx;*.xlsx;*.odt;*.ods');

finalization
  OfficeMask.Free;

end.

