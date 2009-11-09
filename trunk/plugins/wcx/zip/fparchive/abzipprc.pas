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
{* ABBREVIA: AbZipPrc.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: TABZipHelper class                          *}
{*********************************************************}

{$I AbDefine.inc}

unit AbZipPrc;

interface

uses
  Classes,
  AbZipTyp;

  procedure AbZip( ZipArchive : TAbZipArchive; Item : TAbZipItem;
    OutStream : TStream );

  procedure AbZipFromStream(ZipArchive : TAbZipArchive; Item : TAbZipItem;
    OutStream, InStream : TStream);

  procedure DeflateStream( UncompressedStream, CompressedStream : TStream );
    {-Deflates everything in UncompressedStream to CompressedStream
      no encryption is tried, no check on CRC is done, uses the whole
      compressedstream - no Progress events - no Frills! }

implementation

uses
  AbConst,
  AbExcept,
  AbUtils,
  AbDfBase,
  AbDfCryS,
  AbDfEnc,
  AbSpanSt,
  AbVMStrm,                                                              {!!.01}
  SysUtils;


{ ========================================================================== }
procedure DoDeflate(Archive : TAbZipArchive; Item : TAbZipItem; OutStream, InStream : TStream);
{!!.02 Added }
const
  DEFLATE_NORMAL_MASK    = $00;
  DEFLATE_MAXIMUM_MASK   = $02;
  DEFLATE_FAST_MASK      = $04;
  DEFLATE_SUPERFAST_MASK = $06;
{!!.02 End Added }
var
  Hlpr : TAbDeflateHelper;
begin
  Item.CompressionMethod := cmDeflated;

  Hlpr := TAbDeflateHelper.Create;

  if (Archive.Password <> '') then begin
    Hlpr.Passphrase := Archive.Password;
  end;

  {anything dealing with store options, etc. should already be done.}

  try {Hlpr}
    Hlpr.StreamSize := InStream.Size;

    { set deflation level desired }
    Hlpr.PKZipOption := '0';

{!!.02 Rewritten}
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
{!!.02 End Rewritten}

    { attach progress notification method }
    Hlpr.OnProgressStep := Archive.DoInflateProgress;

    { provide encryption check value }
    Hlpr.CheckValue := Item.LastModFileTime shl $10;
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
  DestStrm    : TStream;  { place holder for Output, either direct or encrypted }
begin
  { setup }
  Item.CompressionMethod := cmStored;
  Abort := False;
  CRC32 := -1;
  Total := 0;
  Percent := 0;
  LastPercent := 0;
  InSize := InStream.Size;

  if Archive.Password <> '' then  { encrypt the stream }
    DestStrm := TAbDfEncryptStream.Create(OutStream,
                                          LongInt(Item.LastModFileTime shl $10),
                                          Archive.Password)
  else { just use data stream as-is }
    DestStrm := OutStream;
  try
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
      DestStrm.WriteBuffer(Buffer, DataRead);

      { get next bufferful }
      DataRead := InStream.Read(Buffer, SizeOf(Buffer));
    end;
  finally
    if Archive.Password <> '' then
      DestStrm.Free;
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
procedure AbZipFromStream(ZipArchive : TAbZipArchive; Item : TAbZipItem;
  OutStream, InStream : TStream);
var
  FileTimeStamp   : LongInt;
  InStartPos : LongInt;                                                  {!!.01}
  TempOut : TAbVirtualMemoryStream;                                      {!!.01}

procedure ConfigureItem;
begin
  Item.UncompressedSize := InStream.Size;
  Item.GeneralPurposeBitFlag := 0;
  Item.CompressedSize := 0;
  Item.VersionMadeBy := MakeVersionMadeBy;

  { if not reading from a file, then set defaults for storing the item }
  if not (InStream is TFileStream) or (InStream is TAbSpanStream) then begin
    FileTimeStamp := DateTimeToFileDate(SysUtils.Now);
    Item.ExternalFileAttributes := 0;
    Item.UncompressedSize := InStream.Size;
    Item.LastModFileTime := LongRec(FileTimeStamp).Lo;
    Item.LastModFileDate := LongRec(FileTimeStamp).Hi;
  end;
end;

procedure Validate;
begin
end;

procedure UpdateItem;
begin
  if (Item.CompressionMethod = cmDeflated) then
    Item.VersionNeededToExtract := 20
  else
    Item.VersionNeededToExtract := 10;

  if (ZipArchive.Password <> '') then begin
    Item.GeneralPurposeBitFlag := Item.GeneralPurposeBitFlag
      or AbFileIsEncryptedFlag or AbHasDataDescriptorFlag;
  end;

end;

begin
  TempOut := TAbVirtualMemoryStream.Create;                              {!!.01}
  TempOut.SwapFileDirectory := ZipArchive.TempDirectory;                 {!!.01}

  try
    { configure Item }
    ConfigureItem;

    { validate }
    Validate;

    if InStream.Size > 0 then begin                                      {!!.01}

      { determine how to store Item based on specified CompressionMethodToUse }
      case ZipArchive.CompressionMethodToUse of
        smDeflated : begin
        { Item is to be deflated regarless }
          { deflate item }
          DoDeflate(ZipArchive, Item, OutStream, InStream);                {!!.01}
        end;

        smStored : begin
        { Item is to be stored regardless }
          { store item }
          DoStore(ZipArchive, Item, OutStream, InStream);                  {!!.01}
        end;

        smBestMethod : begin
        { Item is to be archived using method producing best compression }
          { save starting points }
          InStartPos  := InStream.Position;

          { try deflating item }
          DoDeflate(ZipArchive, Item, TempOut, InStream);                {!!.01}
          { if deflated size > input size then got negative compression }
          { so storing the item is more efficient }

          if TempOut.Size > InStream.Size then begin { store item instead }
            { reset streams to original positions }
            InStream.Position  := InStartPos;
            TempOut.Free;                                                {!!.01}
            TempOut := TAbVirtualMemoryStream.Create;                    {!!.01}
            TempOut.SwapFileDirectory := ZipArchive.TempDirectory;       {!!.01}

            { store item }
            DoStore(ZipArchive, Item, TempOut, InStream);          {!!.01}
          end {if};
          
    	 TempOut.Seek(0, soBeginning);                                    {!!.01}
    	 if TempOut.Size > 0 then
           OutStream.CopyFrom(TempOut, TempOut.Size);        
        end;
      end; { case }

    end                                                                  {!!.01}
    else begin                                                           {!!.01}
      { InStream is zero length}                                         {!!.01}
      Item.CRC32 := 0;                                                   {!!.01}
      { ignore any storage indicator and treat as stored }               {!!.01}
      DoStore(ZipArchive, Item, TempOut, InStream);                      {!!.01}
    end;                                                                 {!!.01}

    { update item }
    UpdateItem;


  finally                                                                {!!.01}
    TempOut.Free;                                                        {!!.01}
  end;                                                                   {!!.01}

  Item.CompressedSize := OutStream.Size;
  Item.InternalFileAttributes := 0; { don't care }
end;
{ -------------------------------------------------------------------------- }
procedure AbZip( ZipArchive : TAbZipArchive; Item : TAbZipItem;
                 OutStream : TStream );
var
  UncompressedStream : TStream = nil;
  Name : string;
  Attrs: LongInt;
  FileTime : LongInt;
begin
  if TAbZipHostOS((Item.VersionMadeBy shr 8) and $FF) = hosMSDOS then
    Name := AbStrOemToAnsi(Item.DiskFileName)
  else
    Name := Item.DiskFileName;

  {Now get the file's attributes}
  Attrs := AbFileGetAttr(Name);

  if Attrs = -1 then
    Raise EAbFileNotFound.Create;

  Item.SystemSpecificAttributes := Attrs;

  // Date and time
  FileTime := AbGetFileTime(Name);

  if FileTime < 0 then
    FileTime := DateTimeToFileDate(SysUtils.Now);

  Item.SystemSpecificLastModFileTime := FileTime;

  if AbAttrIsDir(Attrs) then begin

    {Directory. Only set fields.}
    Item.UncompressedSize := 0;
    Item.CompressedSize := 0;
    Item.GeneralPurposeBitFlag := 0;
    Item.VersionMadeBy := MakeVersionMadeBy;
    Item.VersionNeededToExtract := 20;
    Item.CRC32 := 0;
    Item.InternalFileAttributes := 0;
    Item.CompressionMethod:=cmStored;

  end else begin
    { File. Open stream for compression. }
    UncompressedStream := TFileStream.Create(Name,
      fmOpenRead or fmShareDenyWrite );

    Item.UncompressedSize := UncompressedStream.Size;

    try {UncompressedStream}
      AbZipFromStream(ZipArchive, Item, OutStream, UncompressedStream);
    finally {UncompressedStream}
      UncompressedStream.Free;
    end; {UncompressedStream}
  end;
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

