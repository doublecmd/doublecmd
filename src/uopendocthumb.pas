{
    Double Commander
    -------------------------------------------------------------------------
    Open Document Format thumbnail provider

    Copyright (C) 2017 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uOpenDocThumb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  Unzip, ZipUtils, Graphics, Types, uThumbnails, uMasks, uGraphics;

function ExtractThumbnail(FileName: PAnsiChar; MemoryStream: TMemoryStream): Boolean;
var
  ASize: LongInt;
  ZipFile: unzFile;
  FileInfo: unz_file_info;
begin
  Result:= False;
  ZipFile:= unzOpen(FileName);
  if Assigned(ZipFile) then
  try
    if unzLocateFile(ZipFile, 'Thumbnails/thumbnail.png', 0) = UNZ_OK then
    begin
      if unzGetCurrentFileInfo(ZipFile, @FileInfo, nil, 0, nil, 0, nil, 0) = UNZ_OK then
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
  finally
    unzClose(ZipFile);
  end;
end;

var
  MaskList: TMaskList = nil;

function GetThumbnail(const aFileName: String; {%H-}aSize: TSize): Graphics.TBitmap;
var
  MemoryStream: TMemoryStream;
  ABitmap: TPortableNetworkGraphic = nil;
begin
  Result:= nil;

  if MaskList.Matches(aFileName) then
  begin
    MemoryStream:= TMemoryStream.Create;
    try
      if ExtractThumbnail(PAnsiChar(aFileName), MemoryStream) then
      begin
        ABitmap:= TPortableNetworkGraphic.Create;
        try
          ABitmap.LoadFromStream(MemoryStream);
          Result:= TBitmap.Create;
          BitmapAssign(Result, ABitmap);
        finally
          ABitmap.Free;
        end;
      end;
    except
      // Skip
    end;
    MemoryStream.Free;
  end;
end;

initialization
  TThumbnailManager.RegisterProvider(@GetThumbnail);
  MaskList:= TMaskList.Create('*.odt;*.ods;*.odp;*.odg');

finalization
  MaskList.Free;

end.

