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
 * Joel Haynie
 * Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Alexander Koblov <alexx2000@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbBrotliTyp.pas                             *}
{*********************************************************}
{* ABBREVIA: TAbBrotliArchive, TAbBrotliItem classes     *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with Brotli files                                     *}
{*********************************************************}

unit AbBrotliTyp;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp, AbTarTyp, AbUtils;

{ The Purpose for this Item is the placeholder for aaAdd and aaDelete Support. }
{ For all intents and purposes we could just use a TAbArchiveItem }
type
  TAbBrotliItem = class(TabArchiveItem);

  TAbBrotliArchiveState = (gsBrotli, gsTar);

  TAbBrotliArchive = class(TAbTarArchive)
  private
    FBrotliStream : TStream;        { stream for Brotli file}
    FBrotliItem   : TAbArchiveList; { item in Brotli (only one, but need polymorphism of class)}
    FTarStream    : TStream;        { stream for possible contained Tar }
    FTarList      : TAbArchiveList; { items in possible contained Tar }
    FState        : TAbBrotliArchiveState;
    FIsBrotliTar  : Boolean;

    procedure DecompressToStream(aStream: TStream);
    procedure SetTarAutoHandle(const Value: Boolean);
    procedure SwapToBrotli;
    procedure SwapToTar;

  protected
    { Inherited Abstract functions }
    function CreateItem(const SourceFileName   : string;
                        const ArchiveDirectory : string): TAbArchiveItem; override;
    procedure ExtractItemAt(Index : Integer; const NewName : string); override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream); override;
    procedure LoadArchive; override;
    procedure SaveArchive; override;
    procedure TestItemAt(Index : Integer); override;
    function GetStreamMode : Boolean; override;
    function GetSupportsEmptyFolders : Boolean; override;

  public {methods}
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string); override;
    destructor  Destroy; override;

    procedure DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean); override;

    { Properties }
    property TarAutoHandle : Boolean
      read FTarAutoHandle write SetTarAutoHandle;

    property IsBrotliTar : Boolean
      read FIsBrotliTar write FIsBrotliTar;
  end;

function VerifyBrotli(Strm : TStream) : TAbArchiveType;

implementation

uses
  StrUtils, SysUtils,
  AbBrotli, AbExcept, AbVMStrm, AbProgress, DCOSUtils, DCClasses, DCClassesUtf8;

{ ****************** Helper functions Not from Classes Above ***************** }
function VerifyBrotli(Strm : TStream) : TAbArchiveType;
var
  CurPos, DecompSize : Int64;
  DecompStream, TarStream: TStream;
  Buffer: array[0..Pred(AB_TAR_RECORDSIZE * 4)] of Byte;
begin
  Result := atUnknown;

  CurPos := Strm.Position;
  Strm.Seek(0, soBeginning);

  try
    DecompStream := TBrotliDecompressionStream.Create(Strm);
    try
      TarStream := TMemoryStream.Create;
      try
        DecompSize := DecompStream.Read(Buffer, SizeOf(Buffer));
        Result := atBrotli;
        TarStream.Write(Buffer, DecompSize);
        TarStream.Seek(0, soBeginning);
        { Check for embedded TAR }
        if VerifyTar(TarStream) = atTar then
          Result := atBrotliTar;
      finally
        TarStream.Free;
      end;
    finally
      DecompStream.Free;
    end;
  except
    Result := atUnknown;
  end;
  Strm.Position := CurPos; { Return to original position. }
end;


{ ****************************** TAbBrotliArchive ***************************** }
constructor TAbBrotliArchive.CreateFromStream(aStream: TStream;
  const aArchiveName: string);
begin
  inherited CreateFromStream(aStream, aArchiveName);
  FState        := gsBrotli;
  FBrotliStream := FStream;
  FBrotliItem   := FItemList;
  FTarList      := TAbArchiveList.Create(True);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.SwapToTar;
begin
  FStream   := FTarStream;
  FItemList := FTarList;
  FState    := gsTar;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.SwapToBrotli;
begin
  FStream   := FBrotliStream;
  FItemList := FBrotliItem;
  FState    := gsBrotli;
end;
{ -------------------------------------------------------------------------- }
function TAbBrotliArchive.CreateItem(const SourceFileName   : string;
                                    const ArchiveDirectory : string): TAbArchiveItem;
var
  BrotliItem : TAbBrotliItem;
  FullSourceFileName, FullArchiveFileName: String;
begin
  if IsBrotliTar and TarAutoHandle then begin
    SwapToTar;
    Result := inherited CreateItem(SourceFileName, ArchiveDirectory);
  end
  else begin
    SwapToBrotli;
    BrotliItem := TAbBrotliItem.Create;
    try
      MakeFullNames(SourceFileName, ArchiveDirectory,
                    FullSourceFileName, FullArchiveFileName);

      BrotliItem.FileName := FullArchiveFileName;
      BrotliItem.DiskFileName := FullSourceFileName;

      Result := BrotliItem;
    except
      Result := nil;
      raise;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
destructor TAbBrotliArchive.Destroy;
begin
  SwapToBrotli;
  FTarList.Free;
  FTarStream.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TStream;
begin
  if IsBrotliTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, NewName);
  end
  else begin
    SwapToBrotli;
    OutStream := TFileStreamEx.Create(NewName, fmCreate or fmShareDenyNone);
    try
      try
        ExtractItemToStreamAt(Index, OutStream);
      finally
        OutStream.Free;
      end;
      { Brotli doesn't store the last modified time or attributes, so don't set them }
    except
      on E : EAbUserAbort do begin
        FStatus := asInvalid;
        if mbFileExists(NewName) then
          mbDeleteFile(NewName);
        raise;
      end else begin
        if mbFileExists(NewName) then
          mbDeleteFile(NewName);
        raise;
      end;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
begin
  if IsBrotliTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemToStreamAt(Index, aStream);
  end
  else begin
    SwapToBrotli;
    { Index ignored as there's only one item in a Brotli }
    DecompressToStream(aStream);
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbBrotliArchive.GetSupportsEmptyFolders : Boolean;
begin
  Result := IsBrotliTar and TarAutoHandle;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.LoadArchive;
var
  ItemName: String;
  Item: TAbBrotliItem;
  Abort: Boolean = False;
begin
  if FBrotliStream.Size = 0 then
    Exit;

  if IsBrotliTar and TarAutoHandle then
  begin
    { Decompress and load archive on the fly }
    if OpenMode <> opModify  then
    begin
      FTarStream := TBrotliDecompressionStream.Create(FBrotliStream);
      SwapToTar;
    end
    else begin
      FTarStream := TAbVirtualMemoryStream.Create;
      { Decompress and send to tar LoadArchive }
      DecompressToStream(FTarStream);
      SwapToTar;
      inherited LoadArchive;
    end;
  end
  else begin
    SwapToBrotli;
    Item := TAbBrotliItem.Create;
    Item.Action := aaNone;
    Item.UncompressedSize := -1;
    Item.CompressedSize := FBrotliStream.Size;
    { Filename isn't stored, so constuct one based on the archive name }
    ItemName := ExtractFileName(ArchiveName);
    if ItemName = '' then
      Item.FileName := 'unknown'
    else if AnsiEndsText('.tbr', ItemName) then
      Item.FileName := ChangeFileExt(ItemName, '.tar')
    else
      Item.FileName := ChangeFileExt(ItemName, '');
    Item.DiskFileName := Item.FileName;
    FItemList.Add(Item);
  end;
  DoArchiveProgress(100, Abort);
  FIsDirty := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.SaveArchive;
begin
  raise ENotSupportedException.Create(EmptyStr);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.SetTarAutoHandle(const Value: Boolean);
begin
  if Value then
    SwapToTar
  else begin
    SwapToBrotli;
  end;
  FTarAutoHandle := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.DecompressToStream(aStream: TStream);
var
  ProxyStream: TAbProgressReadStream;
  DecompStream: TBrotliDecompressionStream;
begin
  ProxyStream := TAbProgressReadStream.Create(FBrotliStream, OnProgress);
  try
    DecompStream := TBrotliDecompressionStream.Create(ProxyStream);
    try
      aStream.CopyFrom(DecompStream, 0)
    finally
      DecompStream.Free;
    end;
  finally
    ProxyStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.TestItemAt(Index: Integer);
var
  BitBucket: TNullStreamEx;
begin
  if IsBrotliTar and TarAutoHandle then begin
    SwapToTar;
    inherited TestItemAt(Index);
  end
  else begin
    BitBucket := TNullStreamEx.Create;
    try
      DecompressToStream(BitBucket);
    finally
      BitBucket.Free;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbBrotliArchive.GetStreamMode: Boolean;
begin
  Result := FIsBrotliTar and (inherited GetStreamMode);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBrotliArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  Abort := False;
end;

end.

