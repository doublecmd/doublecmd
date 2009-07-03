{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for extract *.lzma archives

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

   Based on:
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
  uWCXhead, ULZMACommon;

type
  TEncoderOptions = record
    DictionarySize: Integer;
    Lc: Integer;
    Lp: Integer;
    Pb: Integer;
    Fb: Integer;
    Eos: Boolean;
    Algorithm: Integer;
    MatchFinder: Integer;
  end;

  { TProgressProc }

  TProgressProc = class
    procedure LZMAProgress(const Action: TLZMAProgressAction; const Value: Int64);
  end;

{ Mandatory functions }
function OpenArchive (var ArchiveData: TOpenArchiveData): TArcHandle; stdcall;
function ReadHeader (hArcData: TArcHandle; var HeaderData: THeaderData): Integer; stdcall;
function ProcessFile (hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar): Integer; stdcall;
function CloseArchive (hArcData: TArcHandle): Integer; stdcall;
procedure SetChangeVolProc (hArcData: TArcHandle; pChangeVolProc: TChangeVolProc); stdcall;
procedure SetProcessDataProc (hArcData: TArcHandle; pProcessDataProc: TProcessDataProc); stdcall;
{ Optional functions }
function PackFiles(PackedFile: PChar;  SubPath: PChar;  SrcPath: PChar;  AddList: PChar;  Flags: Integer): Integer; stdcall;
function GetPackerCaps: Integer; stdcall;

implementation

uses
  Classes, SysUtils, ULZMADecoder, ULZMAEncoder, UBufferedFS;

var
  sArcName: String;
  Count: Integer = 0;
  ProgressPos: Int64 = 0;
  EncoderOptions: TEncoderOptions;
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

procedure ApplyDefaultEncoderOptions;
begin
  with EncoderOptions do
  begin
    DictionarySize:= 1 shl 23;
    Lc:= 3;
    Lp:= 0;
    Pb:= 2;
    Fb:= 128;
    Eos:= False;
    Algorithm:= 2;
    MatchFinder:= 1;
  end;
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
  Result:= E_SUCCESS;
end;

function ProcessFile (hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar): Integer;
const
  PropertiesSize = 5;
var
  inStream: TBufferedFS = nil;
  outStream: TBufferedFS = nil;
  decoder: TLZMADecoder = nil;
  outSize: Int64;
  sOutputFileName: String;
  Properties: array[0..4] of Byte;
  I: Integer;
  v: Byte;
begin
  Result:= E_SUCCESS;
  ProgressPos:= 0;
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
      try
        try
          inStream:= TBufferedFS.Create(sArcName, fmOpenRead or fmShareDenyNone);
          outStream:= TBufferedFS.Create(sOutputFileName, fmCreate);
        except
          on EFOpenError do
            Result:= E_EOPEN;
          on EFCreateError do
            Result:= E_ECREATE;
        end;
        if Result <> E_SUCCESS then Exit;

        if inStream.Read(Properties, PropertiesSize) <> PropertiesSize then
          Exit(E_BAD_DATA);
        decoder:= TLZMADecoder.Create;
        decoder.OnProgress:= TProgressProc.LZMAProgress;
        if not decoder.SetDecoderProperties(properties) then
          Exit(E_BAD_DATA);
        outSize:= 0;
        for I:= 0 to 7 do begin
          v:= {shortint}(ReadByte(inStream));
          if v < 0 then
            Exit(E_EREAD);
          outSize := outSize or v shl (8 * I);
        end;
        if not decoder.Code(inStream, outStream, outSize) then
          Exit(E_BAD_DATA);
      finally
        if Assigned(decoder) then
          FreeAndNil(decoder);
        if Assigned(outStream) then
          FreeAndNil(outStream);
        if Assigned(inStream) then
          FreeAndNil(inStream);
      end;
    end;

  PK_SKIP:
    begin

    end;
  end; // case

  Count:= Count + 1;
end;

function CloseArchive (hArcData: TArcHandle): Integer;
begin
  Result:= E_SUCCESS;
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

function PackFiles(PackedFile: PChar; SubPath: PChar; SrcPath: PChar; AddList: PChar; Flags: Integer): Integer;
var
  inStream: TBufferedFS = nil;
  outStream: TBufferedFS = nil;
  encoder: TLZMAEncoder = nil;
  filesize: Int64;
  I: Integer;
  sInputFileName: String;
begin
  Result:= E_SUCCESS;
  ProgressPos:= 0;
  sInputFileName:= StrPas(SrcPath) + StrPas(AddList);
  sArcName:= PackedFile;
  try
    try
      inStream:= TBufferedFS.Create(sInputFileName, fmOpenRead or fmShareDenyNone);
      outStream:= TBufferedFS.Create(sArcName, fmCreate);
    except
      on EFOpenError do
        Result:= E_EOPEN;
      on EFCreateError do
        Result:= E_ECREATE;
    end;
    if Result <> E_SUCCESS then Exit;

    ApplyDefaultEncoderOptions;
    encoder:= TLZMAEncoder.Create;
    encoder.OnProgress:= TProgressProc.LZMAProgress;
    with EncoderOptions do
    begin
      if not encoder.SetAlgorithm(Algorithm) then
        Exit(E_BAD_DATA);
      if not encoder.SetDictionarySize(DictionarySize) then
        Exit(E_BAD_DATA);
      if not encoder.SeNumFastBytes(Fb) then
        Exit(E_BAD_DATA);
      if not encoder.SetMatchFinder(MatchFinder) then
        Exit(E_BAD_DATA);
      if not encoder.SetLcLpPb(Lc, Lp, Pb) then
        Exit(E_BAD_DATA);
      encoder.SetEndMarkerMode(Eos);
      encoder.WriteCoderProperties(outStream);
      if Eos then
        fileSize:= -1
      else
        fileSize:= inStream.Size;
    end;
    for I:= 0 to 7 do
      WriteByte(outStream, (fileSize shr (8 * I)) and $FF);
    encoder.Code(inStream, outStream, -1, -1);
  finally
    if Assigned(encoder) then
      FreeAndNil(encoder);
    if Assigned(outStream) then
      FreeAndNil(outStream);
    if Assigned(inStream) then
      FreeAndNil(inStream);
  end;
end;

function GetPackerCaps: Integer;
begin
  Result:= PK_CAPS_NEW;
end;

{ TProgressProc }

procedure TProgressProc.LZMAProgress(const Action: TLZMAProgressAction; const Value: Int64);
begin
  if (Action = LPAPos) and Assigned(ProcessDataProc) then
    begin
      ProcessDataProc(PChar(sArcName), (Value - ProgressPos));
      ProgressPos:= Value;
    end;
end;

end.
