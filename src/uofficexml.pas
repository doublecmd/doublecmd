{
   Double commander
   -------------------------------------------------------------------------
   Load text from office xml (*.docx)

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

procedure ProcessNodes(var S: String; ANode: TDOMNode);
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
      ProcessNodes(S, ASubNode);
  end;
end;

function ExtractFile(ZipFile: unzFile; FileName: PAnsiChar; MemoryStream: TMemoryStream): Boolean;
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

procedure ProcessFile(ZipFile: unzFile; const FileName: String; var AText: String);
var
  ADoc: TXMLDocument;
  AStream: TMemoryStream;
begin
  if unzLocateFile(ZipFile, PAnsiChar(FileName), 0) = UNZ_OK then
  begin
    AStream:= TMemoryStream.Create;
    try
      if ExtractFile(ZipFile, PAnsiChar(FileName), AStream) then
      begin
        ReadXMLFile(ADoc, AStream, [xrfPreserveWhiteSpace]);
        if Assigned (ADoc) then
        begin
          ProcessNodes(AText, ADoc.DocumentElement);
          ADoc.Free;
        end;
      end;
    finally
      AStream.Free;
    end;
  end;
end;

function LoadFromOffice(const FileName: String; out AText: String): Boolean;
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

end.

