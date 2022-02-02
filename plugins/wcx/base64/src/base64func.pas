{
  Double Commander
  -------------------------------------------------------------------------
  Base64 archiver plugin

  Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit Base64Func;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses 
  Classes, WcxPlugin;

{ Mandatory functions }
function OpenArchiveW(var ArchiveData: TOpenArchiveDataW): TArcHandle; dcpcall;
function ReadHeaderExW(hArcData: TArcHandle; var HeaderData: THeaderDataExW): Integer; dcpcall;
function ProcessFileW(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PWideChar): Integer; dcpcall;
function CloseArchive (hArcData: TArcHandle): Integer; dcpcall;
procedure SetChangeVolProcW(hArcData: TArcHandle; pChangeVolProc: TChangeVolProcW); dcpcall;
procedure SetProcessDataProcW(hArcData: TArcHandle; pProcessDataProc: TProcessDataProcW); dcpcall;

{ Optional functions }
function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar; SrcPath: PWideChar; AddList: PWideChar; Flags: Integer): Integer; dcpcall;
function GetPackerCaps: Integer; dcpcall;

implementation

uses
  SysUtils, Base64, NullStream, LazFileUtils, DCConvertEncoding, DCOSUtils,
  DCStrUtils, DCClassesUtf8;

const
  MIME_VERSION = 'MIME-VERSION:';
  CONTENT_TYPE = 'CONTENT-TYPE:';
  CONTENT_DISPOSITION = 'CONTENT-DISPOSITION:';
  CONTENT_TRANSFER_ENCODING = 'CONTENT-TRANSFER-ENCODING:';

  MIME_HEADER = 'MIME-Version: 1.0' + #10 +
                'Content-Transfer-Encoding: base64' + #10 +
                'Content-Disposition: attachment; filename="%s"' + #10 +
                'Content-Type: application/octet-stream; name="%s"' + #10#10;

threadvar
  gProcessDataProcW : TProcessDataProcW;

type
  TRecord = class
    Count: Integer;
    FileName: String;
    Stream: TFileStreamEx;
    ProcessDataProcW: TProcessDataProcW;
  end;

function ParseHeader(var AHandle: TRecord): Boolean;
var
  N, P, X, ALength: Integer;
  ABuffer, AText, S: String;
begin
  SetLength(ABuffer, 4096);
  ALength:= AHandle.Stream.Read(ABuffer[1], Length(ABuffer));
  if ALength > 0 then
  begin
    SetLength(ABuffer, ALength);
    AText:= Copy(ABuffer, 1, Length(MIME_VERSION));
    // No MIME-header, assume raw Base64 data
    if CompareStr(MIME_VERSION, UpperCase(AText)) <> 0 then
      AHandle.Stream.Seek(0, soBeginning)
    else begin
      P:= 1;
      while GetNextLine(ABuffer, AText, P) do
      begin
        // Base64 data starts after empty line
        if (Length(AText) = 0) then
        begin
          AHandle.Stream.Seek(P - 1, soBeginning);
          Break;
        end;
        S:= UpperCase(AText);
        if StrBegins(S, CONTENT_TYPE) then
        begin
          if (Pos('MESSAGE/PARTIAL', S) > 0) then
            Exit(False);
          N:= Pos('name="', AText);
          if (N > 0) then
          begin
            N:= N + 6;
            X:= Pos('"', AText, N);
            AHandle.FileName:= Copy(AText, N, X - N);
          end;
        end
        else if StrBegins(S, CONTENT_TRANSFER_ENCODING) then
        begin
          if (Pos('BASE64', S) = 0) then Exit(False);
        end
        else if StrBegins(S, CONTENT_DISPOSITION) then
        begin
          N:= Pos('filename="', AText);
          if (N > 0) then
          begin
            N:= N + 10;
            X:= Pos('"', AText, N);
            AHandle.FileName:= Copy(AText, N, X - N);
          end;
        end;
      end;
    end;
  end;
  Result:= True;
end;

{ Mandatory functions }

function OpenArchiveW(var ArchiveData: TOpenArchiveDataW): TArcHandle; dcpcall;
var
  AHandle: TRecord absolute Result;
begin
  AHandle:= TRecord.Create;
  try
    AHandle.Stream:= TFileStreamEx.Create(CeUtf16ToUtf8(ArchiveData.ArcName),
                                          fmOpenRead or fmShareDenyNone);

    if not ParseHeader(AHandle) then
    begin
      AHandle.Stream.Free;
      FreeAndNil(AHandle);
      ArchiveData.OpenResult:= E_UNKNOWN_FORMAT;
    end
    else if Length(AHandle.FileName) = 0 then
    begin
      AHandle.FileName:= ExtractFileNameOnly(AHandle.Stream.FileName);
    end;
  except
    AHandle.Stream.Free;
    FreeAndNil(AHandle);
    ArchiveData.OpenResult:= E_EOPEN;
  end;
end;

function ReadHeaderExW(hArcData: TArcHandle; var HeaderData: THeaderDataExW): Integer; dcpcall;
var
  FileName: UnicodeString;
  AHandle: TRecord absolute hArcData;
begin
  if AHandle.Count > 0 then
    Result:= E_END_ARCHIVE
  else begin
    Result := E_SUCCESS;
    FileName:= CeUtf8ToUtf16(AHandle.FileName);
    FillChar(HeaderData, SizeOf(AHandle.Count), 0);
    StrPLCopy(HeaderData.FileName, FileName, SizeOf(HeaderData.FileName) - 1);
  end;
end;

function ProcessFileW(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PWideChar) : Integer; dcpcall;
var
  ARead: Integer;
  ABuffer: TBytes;
  fsOutput: TStream;
  AStream: TBase64DecodingStream;
  AHandle: TRecord absolute hArcData;
begin
  case Operation of
  PK_TEST,
  PK_EXTRACT:
    begin
      try
        if Operation = PK_TEST then
          fsOutput:= TNullStream.Create
        else begin
          fsOutput:= TFileStreamEx.Create(CeUtf16ToUtf8(DestPath) + CeUtf16ToUtf8(DestName), fmCreate);
        end;
        try
          AStream:= TBase64DecodingStream.Create(AHandle.Stream);
          try
            SetLength(ABuffer, MaxSmallint);
            repeat
              ARead:= AStream.Read(ABuffer[0], MaxSmallint);
              if ARead > 0 then
              begin
                fsOutput.WriteBuffer(ABuffer[0], ARead);
                AHandle.ProcessDataProcW(DestName, ARead);
              end;
            until ARead < MaxSmallint;
          finally
            AStream.Free;
          end;
        finally
          fsOutput.Free;
        end;
      except
        Exit(E_ECREATE);
      end;
    end;

  PK_SKIP:
    begin

    end;
  end;
  Inc(AHandle.Count);
  Result:= E_SUCCESS;
end;

function CloseArchive (hArcData: TArcHandle): Integer; dcpcall;
var
  AHandle: TRecord absolute hArcData;
begin
  Result := E_SUCCESS;
  AHandle.Stream.Free;
  AHandle.Free;
end;

procedure SetChangeVolProcW(hArcData: TArcHandle; pChangeVolProc: TChangeVolProcW); dcpcall;
begin

end;

procedure SetProcessDataProcW(hArcData: TArcHandle; pProcessDataProc: TProcessDataProcW); dcpcall;
var
  AHandle: TRecord absolute hArcData;
begin
  if (hArcData <> wcxInvalidHandle) then
    AHandle.ProcessDataProcW := pProcessDataProc
  else begin
    gProcessDataProcW := pProcessDataProc;
  end;
end;

{ Optional functions }

function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar; SrcPath: PWideChar; AddList: PWideChar; Flags: Integer): Integer; dcpcall;
const
  LINE_FEED = 10;
var
  ARead: Integer;
  ABuffer: TBytes;
  AHeader: String;
  AFileName: String;
  AStream: TBase64EncodingStream;
  fsInput, fsOutput: TFileStreamEx;
begin
  if (Flags and PK_PACK_MOVE_FILES) <> 0 then begin
    Exit(E_NOT_SUPPORTED);
  end;

  try
    AFileName:= CeUtf16ToUtf8(AddList);
    fsInput:= TFileStreamEx.Create(CeUtf16ToUtf8(SrcPath) + AFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        fsOutput:= TFileStreamEx.Create(CeUtf16ToUtf8(PackedFile), fmCreate);
        try
          AHeader:= Format(MIME_HEADER, [AFileName, AFileName]);
          try
            fsOutput.WriteBuffer(AHeader[1], Length(AHeader));
            AStream:= TBase64EncodingStream.Create(fsOutput);
            try
              SetLength(ABuffer, MaxSmallint);
              repeat
                ARead:= fsInput.Read(ABuffer[0], MaxSmallint);
                if ARead > 0 then
                begin
                  AStream.WriteBuffer(ABuffer[0], ARead);
                  gProcessDataProcW(PackedFile, ARead);
                  fsOutput.WriteByte(LINE_FEED);
                end;
              until ARead < MaxSmallint;
            finally
              AStream.Free;
            end;
          except
            Exit(E_EWRITE);
          end;
        finally
          fsOutput.Free;
        end;
      except
        Exit(E_ECREATE);
      end;
    finally
      fsInput.Free;
    end;
  except
    Exit(E_EOPEN);
  end;

  Result:= E_SUCCESS;
end;

function GetPackerCaps: Integer; dcpcall;
begin
  Result := PK_CAPS_NEW;
end;

end.

