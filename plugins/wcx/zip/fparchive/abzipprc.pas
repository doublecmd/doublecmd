(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbZipPrc.pas                                *}
{*********************************************************}
{* ABBREVIA: TABZipHelper class                          *}
{*********************************************************}

unit AbZipPrc;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbZipTyp;

  procedure AbZip( Sender : TAbZipArchive; Item : TAbZipItem;
    OutStream : TStream );

  procedure AbZipFromStream(Sender : TAbZipArchive; Item : TAbZipItem;
    OutStream, InStream : TStream);

  procedure DeflateStream( UncompressedStream, CompressedStream : TStream );
    {-Deflates everything in UncompressedStream to CompressedStream
      no encryption is tried, no check on CRC is done, uses the whole
      compressedstream - no Progress events - no Frills! }

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LibcAPI}
  Libc,
{$ENDIF}
  SysUtils,
  AbArcTyp,
  AbExcept,
  AbUtils,
  AbDfCryS,
  AbVMStrm,
  AbDfBase,
  AbDfEnc,
  AbSpanSt,
  DCClassesUtf8;


{ ========================================================================== }
procedure DoDeflate(Archive : TAbZipArchive; Item : TAbZipItem; OutStream, InStream : TStream);
const
  DEFLATE_NORMAL_MASK    = $00;
  DEFLATE_MAXIMUM_MASK   = $02;
  DEFLATE_FAST_MASK      = $04;
  DEFLATE_SUPERFAST_MASK = $06;
var
  Hlpr : TAbDeflateHelper;
begin
  Item.CompressionMethod := cmDeflated;

  Hlpr := TAbDeflateHelper.Create;

  {anything dealing with store options, etc. should already be done.}

  try {Hlpr}
    Hlpr.StreamSize := InStream.Size;

    { set deflation level desired }
    Hlpr.PKZipOption := '0';

    case Archive.DeflationOption of
      doNormal    : begin
        Hlpr.PKZipOption := 'n';
        Item.GeneralPurposeBitFlag :=
          Item.GeneralPurposeBitFlag or DEFLATE_NORMAL_MASK;
      end;

      doMaximum   : begin
        Hlpr.PKZipOption := 'x';
        Item.GeneralPurposeBitFlag :=
          Item.GeneralPurposeBitFlag or DEFLATE_MAXIMUM_MASK;
      end;

      doFast      : begin
        Hlpr.PKZipOption := 'f';
        Item.GeneralPurposeBitFlag :=
          Item.GeneralPurposeBitFlag or DEFLATE_FAST_MASK;
      end;

      doSuperFast : begin
        Hlpr.PKZipOption := 's';
        Item.GeneralPurposeBitFlag :=
          Item.GeneralPurposeBitFlag or DEFLATE_SUPERFAST_MASK;
      end;
    end;

    { attach progress notification method }
    Hlpr.OnProgressStep := Archive.DoInflateProgress;

    { provide encryption check value }
    Item.CRC32 := Deflate(InStream, OutStream, Hlpr);

  finally {Hlpr}
    Hlpr.Free;
  end;    {Hlpr}
end;
{ ========================================================================== }
procedure DoStore(Archive : TAbZipArchive; Item : TAbZipItem; OutStream, InStream : TStream);
var
  CRC32       : LongInt;
  Percent     : LongInt;
  LastPercent : LongInt;
  InSize      : Int64;
  DataRead    : Int64;
  Total       : Int64;
  Abort       : Boolean;
  Buffer      : array [0..8191] of byte;
begin
  { setup }
  Item.CompressionMethod := cmStored;
  Abort := False;
  CRC32 := -1;
  Total := 0;
  Percent := 0;
  LastPercent := 0;
  InSize := InStream.Size;

  { get first bufferful }
  DataRead := InStream.Read(Buffer, SizeOf(Buffer));
  { while more data has been read and we're not told to bail }
  while (DataRead <> 0) and not Abort do begin
    {report the progress}
    if Assigned(Archive.OnProgress) then begin
      Total := Total + DataRead;
      Percent := Round((100.0 * Total) / InSize);
      if (LastPercent <> Percent) then
        Archive.OnProgress(Percent, Abort);
      LastPercent := Percent;
    end;

    { update CRC}
    AbUpdateCRCBuffer(CRC32, Buffer, DataRead);

    { write data (encrypting if needed) }
    OutStream.WriteBuffer(Buffer, DataRead);

    { get next bufferful }
    DataRead := InStream.Read(Buffer, SizeOf(Buffer));
  end;

  { finish CRC calculation }
  Item.CRC32 := not CRC32;

  { show final progress increment }
  if (Percent < 100) and Assigned(Archive.OnProgress) then
    Archive.OnProgress(100, Abort);

  { User wants to bail }
  if Abort then begin
    raise EAbUserAbort.Create;
  end;

end;
{ ========================================================================== }
procedure DoZipFromStream(Sender : TAbZipArchive; Item : TAbZipItem;
  OutStream, InStream : TStream);
var
  ZipArchive : TAbZipArchive;
  InStartPos : LongInt;
  TempOut : TAbVirtualMemoryStream;
  DestStrm : TStream;
begin
  ZipArchive := TAbZipArchive(Sender);

  { configure Item }
  Item.UncompressedSize := InStream.Size;
  Item.GeneralPurposeBitFlag := Item.GeneralPurposeBitFlag and AbLanguageEncodingFlag;

  if ZipArchive.Password <> '' then  { encrypt the stream }
    DestStrm := TAbDfEncryptStream.Create(OutStream,
                                          LongInt(Item.LastModFileTime shl $10),
                                          ZipArchive.Password)
  else
    DestStrm := OutStream;
  try
    if InStream.Size > 0 then begin

      { determine how to store Item based on specified CompressionMethodToUse }
      case ZipArchive.CompressionMethodToUse of
        smDeflated : begin
        { Item is to be deflated regarless }
          { deflate item }
          DoDeflate(ZipArchive, Item, DestStrm, InStream);
        end;

        smStored : begin
        { Item is to be stored regardless }
          { store item }
          DoStore(ZipArchive, Item, DestStrm, InStream);
        end;

        smBestMethod : begin
        { Item is to be archived using method producing best compression }
          TempOut := TAbVirtualMemoryStream.Create;
          try
            TempOut.SwapFileDirectory := Sender.TempDirectory;

            { save starting points }
            InStartPos  := InStream.Position;

            { try deflating item }
            DoDeflate(ZipArchive, Item, TempOut, InStream);
            { if deflated size > input size then got negative compression }
            { so storing the item is more efficient }

            if TempOut.Size > InStream.Size then begin { store item instead }
              { reset streams to original positions }
              InStream.Position  := InStartPos;
              TempOut.Free;
              TempOut := TAbVirtualMemoryStream.Create;
              TempOut.SwapFileDirectory := Sender.TempDirectory;

              { store item }
              DoStore(ZipArchive, Item, TempOut, InStream);
            end {if};

            TempOut.Seek(0, soBeginning);
            DestStrm.CopyFrom(TempOut, TempOut.Size);
          finally
            TempOut.Free;
          end;
        end;
      end; { case }

    end
    else begin
      { InStream is zero length}
      Item.CRC32 := 0;
      { ignore any storage indicator and treat as stored }
      DoStore(ZipArchive, Item, DestStrm, InStream);
    end;
  finally
    if DestStrm <> OutStream then
      DestStrm.Free;
  end;

  { update item }
  Item.CompressedSize := OutStream.Size;
  Item.InternalFileAttributes := 0; { don't care }
  if (ZipArchive.Password <> '') then
    Item.GeneralPurposeBitFlag := Item.GeneralPurposeBitFlag
      or AbFileIsEncryptedFlag or AbHasDataDescriptorFlag;
end;
{ -------------------------------------------------------------------------- }
procedure AbZipFromStream(Sender : TAbZipArchive; Item : TAbZipItem;
  OutStream, InStream : TStream);
var
  FileTimeStamp : LongInt;
begin
  // Set item properties for non-file streams
  Item.ExternalFileAttributes := 0;
  FileTimeStamp := DateTimeToFileDate(SysUtils.Now);
  Item.LastModFileTime := LongRec(FileTimeStamp).Lo;
  Item.LastModFileDate := LongRec(FileTimeStamp).Hi;

  DoZipFromStream(Sender, Item, OutStream, InStream);
end;
{ -------------------------------------------------------------------------- }
procedure AbZip( Sender : TAbZipArchive; Item : TAbZipItem;
                 OutStream : TStream );
var
  UncompressedStream : TStream;
  AttrEx : TAbAttrExRec;
begin
  if not AbFileGetAttrEx(Item.DiskFileName, AttrEx) then
    Raise EAbFileNotFound.Create;
  if ((AttrEx.Attr and faDirectory) <> 0) then
    UncompressedStream := TMemoryStream.Create
  else
    UncompressedStream := TFileStreamEx.Create(Item.DiskFileName, fmOpenRead or fmShareDenyWrite);
  try {UncompressedStream}
    {$IFDEF UNIX}
    Item.ExternalFileAttributes := LongWord(AttrEx.Mode) shl 16 + LongWord(AttrEx.Attr);
    {$ELSE}
    Item.ExternalFileAttributes := AttrEx.Attr;
    {$ENDIF}
    Item.LastModTimeAsDateTime := AttrEx.Time;
    DoZipFromStream(Sender, Item, OutStream, UncompressedStream);
  finally {UncompressedStream}
    UncompressedStream.Free;
  end; {UncompressedStream}
end;
{ -------------------------------------------------------------------------- }
procedure DeflateStream( UncompressedStream, CompressedStream : TStream );
  {-Deflates everything in CompressedStream to UncompressedStream
    no encryption is tried, no check on CRC is done, uses the whole
    Uncompressedstream - no Progress events - no Frills!
  }
begin
  Deflate(UncompressedStream, CompressedStream, nil);
end;
{ -------------------------------------------------------------------------- }

end.

