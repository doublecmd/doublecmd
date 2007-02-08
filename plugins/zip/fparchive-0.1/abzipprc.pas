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
{* ABBREVIA: AbZipPrc.pas 3.04                           *}
{*********************************************************}
{* ABBREVIA: TABZipHelper class                          *}
{*********************************************************}

{$I AbDefine.inc}

unit AbZipPrc;

interface

uses
  Types, //!! MVC for max_path.
  Classes,
  AbArcTyp,
  AbZipTyp,
  AbDfBase,
  AbDfEnc,
  AbSpanSt;

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
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  AbConst,
  AbExcept,
  AbUtils,
  AbDfCryS,
  AbZipCry,                                                              {!!.01}
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
{
    case Archive.DeflationOption of
      doNormal    : Hlpr.PKZipOption := 'n';
      doMaximum   : Hlpr.PKZipOption := 'x';
      doFast      : Hlpr.PKZipOption := 'f';
      doSuperFast : Hlpr.PKZipOption := 's';
    end;
}
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
  InSize      : LongInt;
  DataRead    : LongInt;
  Total       : LongInt;
  Abort       : Boolean;
  Buffer      : array [0..1023] of byte;
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
procedure AbZipFromStream(Sender : TAbZipArchive; Item : TAbZipItem;
  OutStream, InStream : TStream);
var
  ZipArchive : TAbZipArchive;
  FileTimeStamp   : LongInt;
  InStartPos{, OutStartPos} : LongInt;                                   {!!.01}
  TempOut : TAbVirtualMemoryStream;                                      {!!.01}

procedure ConfigureItem;
begin
  Item.UncompressedSize := InStream.Size;
  Item.GeneralPurposeBitFlag := 0;
  Item.CompressedSize := 0;

  { if not reading from a file, then set defaults for storing the item }
  if not (InStream is TFileStream) or (InStream is TAbSpanStream) then begin
    FileTimeStamp := DateTimeToFileDate(SysUtils.Now);
    Item.ExternalFileAttributes := 0;
    Item.UncompressedSize := InStream.Size;
    Item.LastModFileTime := LongRec(FileTimeStamp).Lo;
    Item.LastModFileDate := LongRec(FileTimeStamp).Hi;
  end;

{$IFDEF MSWINDOWS}
  if (GetVersion shr 16) = 0 then {Windows NT}
    Item.VersionMadeBy := $0A00 + 20
  else
    Item.VersionMadeBy := 20;
{$ENDIF}
{$IFDEF LINUX}
  Item.VersionMadeBy := 20;
{$ENDIF}
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
  ZipArchive := TAbZipArchive(Sender);

  TempOut := TAbVirtualMemoryStream.Create;                              {!!.01}
  TempOut.SwapFileDirectory := Sender.TempDirectory;                     {!!.01}

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
          DoDeflate(ZipArchive, Item, TempOut, InStream);                {!!.01}
        end;

        smStored : begin
        { Item is to be stored regardless }
          { store item }
          DoStore(ZipArchive, Item, TempOut, InStream);                  {!!.01}
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
            TempOut.SwapFileDirectory := Sender.TempDirectory;           {!!.01}

            { store item }
            DoStore(ZipArchive, Item, TempOut, InStream);                {!!.01}
          end {if};
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

    TempOut.Seek(0, soFromBeginning);                                    {!!.01}
    OutStream.CopyFrom(TempOut, TempOut.Size);                           {!!.01}

  finally                                                                {!!.01}
    TempOut.Free;                                                        {!!.01}
  end;                                                                   {!!.01}

  Item.CompressedSize := OutStream.Size;
  Item.InternalFileAttributes := 0; { don't care }
end;
{ -------------------------------------------------------------------------- }
procedure AbZip( Sender : TAbZipArchive; Item : TAbZipItem;
                 OutStream : TStream );
var
  UncompressedStream : TStream;
  DateTime : LongInt;
  SaveDir : string;
  Buff : array [0..MAX_PATH] of AnsiChar;
  ZipArchive : TAbZipArchive;
begin
  ZipArchive := TAbZipArchive(Sender);
  GetDir(0, SaveDir);
  try {SaveDir}
    if (ZipArchive.BaseDirectory <> '') then
      ChDir(ZipArchive.BaseDirectory);
    StrPCopy(Buff, Item.DiskFileName);
{!!OEM - Added }
{$IFDEF Linux}
 { do nothing to Buff }
{$ELSE}
    if AreFileApisANSI then begin
      OEMToAnsi(Buff, Buff);
    end;
{$ENDIF}
{!!OEM - End Added }
    { Converting file names causing problems on some systems, take hands off approach }
    {  OEMToAnsi(Buff, Buff);  }
    UncompressedStream := TFileStream.Create(StrPas(Buff),
      fmOpenRead or fmShareDenyWrite );
    {Now get the file's attributes}
    Item.ExternalFileAttributes := AbFileGetAttr(StrPas(Buff));
    Item.UncompressedSize := UncompressedStream.Size;
  finally {SaveDir}
    ChDir( SaveDir );
  end; {SaveDir}
  try {UncompressedStream}
    DateTime := FileGetDate(TFileStream(UncompressedStream).Handle);
    Item.LastModFileTime := LongRec(DateTime).Lo;
    Item.LastModFileDate := LongRec(DateTime).Hi;
    AbZipFromStream(Sender, Item, OutStream, UncompressedStream);
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

