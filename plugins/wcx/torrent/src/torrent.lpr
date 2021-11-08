{
  Double Commander
  -------------------------------------------------------------------------
  BitTorrent archiver plugin

  Copyright (C) 2017 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <https://www.gnu.org/licenses/>.
}

library torrent;

{$mode delphi}
{$include calling.inc}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  FPCAdds,
  Classes, SysUtils, TorrentFile, WcxPlugin, DCDateTimeUtils, DCClassesUtf8,
  DCConvertEncoding;

type
  PTorrentHandle = ^TTorrentHandle;
  TTorrentHandle = record
   Index: Integer;
   Torrent: TTorrentFile;
  end;

function OpenArchive(var ArchiveData : tOpenArchiveData) : TArcHandle; dcpcall;
begin
  Result := 0;
  ArchiveData.OpenResult := E_NOT_SUPPORTED;
end;

function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle; dcpcall;
var
  AFileName: String;
  AStream: TFileStreamEx;
  AHandle: PTorrentHandle = nil;
begin
  Result:= 0;
  if ArchiveData.OpenMode = PK_OM_EXTRACT then
  begin
    ArchiveData.OpenResult:= E_NOT_SUPPORTED;
    Exit;
  end;
  try
    AFileName := CeUtf16ToUtf8(UnicodeString(ArchiveData.ArcName));
    AStream:= TFileStreamEx.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      New(AHandle);
      AHandle.Index:= 0;
      AHandle.Torrent:= TTorrentFile.Create;
      if not AHandle.Torrent.Load(AStream) then
        raise Exception.Create(EmptyStr);
      Result:= TArcHandle(AHandle);
    finally
      AStream.Free;
    end;
  except
    ArchiveData.OpenResult:= E_EOPEN;
    if Assigned(AHandle) then
    begin
      AHandle.Torrent.Free;
      Dispose(AHandle);
    end;
  end;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer; dcpcall;
begin
  Result := E_NOT_SUPPORTED;
end;

function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer; dcpcall;
var
  AFile: TTorrentSubFile;
  AHandle: PTorrentHandle absolute hArcData;
begin
  if AHandle.Index >= AHandle.Torrent.Files.Count then Exit(E_END_ARCHIVE);

  AFile:= TTorrentSubFile(AHandle.Torrent.Files[AHandle.Index]);
  HeaderData.FileTime:= UnixFileTimeToWcxTime(AHandle.Torrent.CreationTime);
  HeaderData.FileName:= CeUtf8ToUtf16(AFile.Path + AFile.Name);
  HeaderData.UnpSize:= Int64Rec(AFile.Length).Lo;
  HeaderData.UnpSizeHigh:= Int64Rec(AFile.Length).Hi;
  HeaderData.PackSize:= HeaderData.UnpSize;
  HeaderData.PackSizeHigh:= HeaderData.UnpSizeHigh;
  Result:= E_SUCCESS;
end;

function ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer; dcpcall;
begin
  Result := E_NOT_SUPPORTED;
end;

function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer; dcpcall;
var
  AHandle: PTorrentHandle absolute hArcData;
begin
  Inc(AHandle.Index);
  Result:= E_SUCCESS;
end;

function CloseArchive (hArcData : TArcHandle) : Integer; dcpcall;
var
  AHandle: PTorrentHandle absolute hArcData;
begin
  if hArcData <> wcxInvalidHandle then
  begin
    AHandle.Torrent.Free;
    Dispose(AHandle);
  end;
  Result:= E_SUCCESS;
end;

procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : TChangeVolProc); dcpcall;
begin

end;

procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc); dcpcall;
begin

end;

function GetPackerCaps : Integer;dcpcall;
begin
  Result := PK_CAPS_MULTIPLE or PK_CAPS_HIDE;
end;

exports
  OpenArchive,
  OpenArchiveW,
  ReadHeader,
  ReadHeaderExW,
  ProcessFile,
  ProcessFileW,
  CloseArchive,
  SetChangeVolProc,
  SetProcessDataProc,
  GetPackerCaps;

end.

