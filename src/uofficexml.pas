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
  Classes, SysUtils;

function LoadFromOffice(const FileName: String; out AText: String): Boolean;

implementation

uses
  Unzip, ZipUtils, Laz2_DOM, laz2_XMLRead;

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

function LoadFromOffice(const FileName: String; out AText: String): Boolean;
begin
  if SameText(ExtractFileExt(FileName), '.docx') then
    Result:= LoadFromOfficeOpen(FileName, AText)
  else
    Result:= LoadFromOpenOffice(FileName, AText);
end;

end.

