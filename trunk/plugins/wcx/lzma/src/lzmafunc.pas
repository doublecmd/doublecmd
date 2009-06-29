{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for extract *.lzma archives

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

   based on:
     LZMAAlone from Pascal LZMA SDK

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit lzmafunc;

{$mode delphi}{$H+}

interface

uses
  uWCXhead;

{ Mandatory functions }
function OpenArchive (var ArchiveData: TOpenArchiveData): TArcHandle; stdcall;
function ReadHeader (hArcData: TArcHandle; var HeaderData: THeaderData): Integer; stdcall;
function ProcessFile (hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar): Integer; stdcall;
function CloseArchive (hArcData: TArcHandle): Integer; stdcall;
procedure SetChangeVolProc (hArcData: TArcHandle; pChangeVolProc: TChangeVolProc); stdcall;
procedure SetProcessDataProc (hArcData: TArcHandle; pProcessDataProc: TProcessDataProc); stdcall;
{ Optional functions }
function CanYouHandleThisFile(FileName: PChar): Boolean; stdcall;

implementation

uses
  Classes, SysUtils, ULZMACommon, ULZMADecoder, UBufferedFS;

var
  sArcName: String;
  Count: Integer = 0;
  ProcessDataProc: TProcessDataProc;


function ExtractOnlyFileName(const FileName: String): String;
var
 iDotIndex,
 I: LongInt;
begin
  // Find a dot index
  I:= Length(FileName);
  while (I > 0) and not (FileName[I] in ['.', '/', '\', ':']) do Dec(I);
  if (I > 0) and (FileName[I] = '.') then
     iDotIndex:= I
  else
     iDotIndex:= MaxInt;
  // Find file name index
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
  Result:= Copy(FileName, I + 1, iDotIndex - I - 1);
end;

  
function OpenArchive (var ArchiveData: TOpenArchiveData): TArcHandle;
begin
  if FileExists(ArchiveData.ArcName) then
    begin
      sArcName:= ArchiveData.ArcName;
      Count:= 0;
      Result:= 1985;
    end
  else
    Result:= E_EOPEN;
end;

function ReadHeader (hArcData: TArcHandle; var HeaderData: THeaderData): Integer;
var
  sr: TSearchRec;
begin
  if Count > 0 then
    begin
      Result:= E_END_ARCHIVE;
      Exit;
    end;

  with HeaderData do
    begin
      FindFirst(sArcName, faAnyFile, sr);
      FileName := ExtractOnlyFileName(sArcName);
      PackSize := sr.Size;
      UnpSize  := sr.Size; // we don't know real file size
      FileTime := sr.Time;
      FileAttr := sr.Attr;
      FindClose(sr);
    end;
  Result:= 0;
end;

function ProcessFile (hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar): Integer;
const
  PropertiesSize = 5;
var
  inStream: TBufferedFS;
  outStream: TBufferedFS;
  decoder: TLZMADecoder;
  outSize: Int64;
  sOutputFileName: String;
  Properties: array[0..4] of Byte;
  I: Integer;
  v: Byte;
begin
  case Operation of
  PK_TEST:
    begin

    end;

  PK_EXTRACT:
    begin
      sOutputFileName:= DestPath;
      if sOutputFileName <> '' then
        sOutputFileName:= sOutputFileName + ExtractOnlyFileName(sArcName)
      else
        sOutputFileName:= DestName;
      inStream:= TBufferedFS.Create(sArcName, fmOpenRead or fmShareDenyNone);
      outStream:= TBufferedFS.Create(sOutputFileName, fmCreate);
      try
        if inStream.read(Properties, PropertiesSize) <> PropertiesSize then
          raise Exception.Create('input .lzma file is too short');
        decoder:= TLZMADecoder.Create;
        if not decoder.SetDecoderProperties(properties) then
          raise Exception.Create('Incorrect stream properties');
        outSize:= 0;
        for I:= 0 to 7 do begin
          v:= {shortint}(ReadByte(inStream));
          if v < 0 then
            raise Exception.Create('Can''t read stream size');
          outSize := outSize or v shl (8 * I);
        end;
        if not decoder.Code(inStream, outStream, outSize) then
          raise Exception.Create('Error in data stream');
        decoder.Free;
      finally
        outStream.Free;
        inStream.Free;
      end;
    end;

  PK_SKIP:
    begin

    end;
  end; // case

  Count:= Count + 1;
  Result:= 0;
end;

function CloseArchive (hArcData: TArcHandle): Integer;
begin
  Result:= 0;
end;

procedure SetChangeVolProc (hArcData: TArcHandle; pChangeVolProc: TChangeVolProc);
begin
end;

procedure SetProcessDataProc (hArcData: TArcHandle; pProcessDataProc: TProcessDataProc);
begin
  if Assigned(pProcessDataProc) then
    ProcessDataProc:= pProcessDataProc
  else
    ProcessDataProc:= nil;
end;

function CanYouHandleThisFile(FileName: PChar): Boolean;stdcall;
begin
  Result:= True;
end;

end.
