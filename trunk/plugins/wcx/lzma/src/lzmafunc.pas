{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for extract *.lzma archives

   Copyright (C) 2009-2011  Koblov Alexander (Alexx2000@mail.ru)

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
{$include calling.inc}

interface

uses
  WcxPlugin, ULZMACommon;

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
function OpenArchive (var ArchiveData: TOpenArchiveData): TArcHandle; dcpcall;
function ReadHeader (hArcData: TArcHandle; var HeaderData: THeaderData): Integer; dcpcall;
function ProcessFile (hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar): Integer; dcpcall;
function CloseArchive (hArcData: TArcHandle): Integer; dcpcall;
procedure SetChangeVolProc (hArcData: TArcHandle; pChangeVolProc: TChangeVolProc); dcpcall;
procedure SetProcessDataProc (hArcData: TArcHandle; pProcessDataProc: TProcessDataProc); dcpcall;
{ Optional functions }
function PackFiles(PackedFile: PChar;  SubPath: PChar;  SrcPath: PChar;  AddList: PChar;  Flags: Integer): Integer; dcpcall;
function GetPackerCaps: Integer; dcpcall;

implementation

uses
  Classes, SysUtils, ULZMADecoder, ULZMAEncoder, UBufferedFS;

const
  PropertiesSize = 5;

type

  { TLzmaHandle }

  TLzmaHandle = class
    inStream: TBufferedFS;
    outSize: Int64;
    Properties: array[0..Pred(PropertiesSize)] of Byte;
    function Open(const FileName: String): LongInt;
    destructor Destroy; override;
  end;

{ TLzmaHandle }

function TLzmaHandle.Open(const FileName: String): LongInt;
begin
  Result:= E_SUCCESS;
  try
    inStream:= nil;
    inStream:= TBufferedFS.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    on EFOpenError do
      Result:= E_EOPEN;
  end;
  if Result <> E_SUCCESS then Exit;
  if inStream.Read(Properties, PropertiesSize) <> PropertiesSize then
    Exit(E_BAD_DATA);
  outSize := LEtoN(inStream.ReadQWord);
  if (outSize = DWORD(-1)) then
     outSize := 0;
end;

destructor TLzmaHandle.Destroy;
begin
  if Assigned(inStream) then
    FreeAndNil(inStream);
  inherited Destroy;
end;

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
var
  lzmaHandle: TLzmaHandle;
begin
  Result:= 0;
  if FileExists(ArchiveData.ArcName) then
    begin
      sArcName:= ArchiveData.ArcName;
      lzmaHandle:= TLzmaHandle.Create;
      ArchiveData.OpenResult:= lzmaHandle.Open(sArcName);
      if ArchiveData.OpenResult <> E_SUCCESS then Exit;
      Count:= 0;
      Result:= TArcHandle(lzmaHandle);
    end;
end;

function ReadHeader (hArcData: TArcHandle; var HeaderData: THeaderData): Integer;
var
  lzmaHandle: TLzmaHandle absolute hArcData;
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
      UnpSize  := Lo(lzmaHandle.outSize);
      FileTime := sr.Time;
      FileAttr := sr.Attr;
      FindClose(sr);
    end;
  Result:= E_SUCCESS;
end;

function ProcessFile (hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar): Integer;
var
  lzmaHandle: TLzmaHandle absolute hArcData;
  outStream: TBufferedFS = nil;
  decoder: TLZMADecoder = nil;
  sOutputFileName: String;
begin
  Result:= E_SUCCESS;
  ProgressPos:= 0;

  case Operation of
  PK_TEST:
    begin
      Result := E_NOT_SUPPORTED;
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
          outStream:= TBufferedFS.Create(sOutputFileName, fmCreate);
        except
          on EFCreateError do
            Result:= E_ECREATE;
        end;
        if Result <> E_SUCCESS then Exit;
        decoder:= TLZMADecoder.Create;
        decoder.OnProgress:= TProgressProc.LZMAProgress;
        if not decoder.SetDecoderProperties(lzmaHandle.Properties) then
          Exit(E_BAD_DATA);
        if not decoder.Code(lzmaHandle.inStream, outStream, lzmaHandle.outSize) then
          Exit(E_BAD_DATA);
      finally
        if Assigned(decoder) then
          FreeAndNil(decoder);
        if Assigned(outStream) then
          FreeAndNil(outStream);
      end;
    end;

  PK_SKIP:
    begin

    end;
  end; // case

  Count:= Count + 1;
end;

function CloseArchive (hArcData: TArcHandle): Integer;
var
  lzmaHandle: TLzmaHandle absolute hArcData;
begin
  if Assigned(lzmaHandle) then
    FreeAndNil(lzmaHandle);
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
    outStream.WriteQWord(NtoLE(fileSize));
    encoder.Code(inStream, outStream, -1, -1);
  finally
    FreeAndNil(encoder);
    {$IFDEF CPU64}
    outStream.Flush;
    FileClose(outStream.Handle);
    {$ELSE}
    FreeAndNil(outStream);
    {$ENDIF}
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
