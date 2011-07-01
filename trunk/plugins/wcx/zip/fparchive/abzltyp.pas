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
{* ABBREVIA: AbZLTyp.pas                                 *}
{*********************************************************}
{* ABBREVIA: TAbZlItem class                             *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with ZLib compressed data                             *}
{* See: RFC 1950                                         *}
{* "ZLIB Compressed Data Format Specification            *}
{*  version 3.3" for more information on ZLib            *}
{*********************************************************}

unit AbZLTyp;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, AbUtils, AbArcTyp, AbZipPrc, AbDfBase, AbDfDec, AbDfEnc;

const
  AB_ZL_PRESET_DICT = $20;

  AB_ZL_DEF_COMPRESSIONMETHOD = $8;  { Deflate }
  AB_ZL_DEF_COMPRESSIONINFO   = $7;  { 32k window for Deflate }

  AB_ZL_FASTEST_COMPRESSION = $0;
  AB_ZL_FAST_COMPRESSION    = $1;
  AB_ZL_DEFAULT_COMPRESSION = $2;
  AB_ZL_MAXIMUM_COMPRESSION = $3;

  AB_ZL_FCHECK_MASK         = $1F;
  AB_ZL_CINFO_MASK          = $F0; { mask out leftmost 4 bits }
  AB_ZL_FLEVEL_MASK         = $C0; { mask out leftmost 2 bits }
  AB_ZL_CM_MASK             = $0F; { mask out rightmost 4 bits }


type
  TAbZLHeader = packed record
    CMF : Byte;
    FLG : Byte;
  end;

  TAbZLItem = class(TAbArchiveItem)
  private
    function GetCompressionInfo: Byte;
    function GetCompressionLevel: Byte;
    function GetIsPresetDictionaryPresent: Boolean;
    procedure SetCompressionInfo(Value: Byte);
    procedure SetCompressionLevel(Value: Byte);
    function GetCompressionMethod: Byte;
    procedure SetCompressionMethod(Value: Byte);
    function GetFCheck: Byte;
    procedure MakeFCheck;
  protected { private }
    FZLHeader : TAbZlHeader;
    FAdler32  : LongInt;
  public
    constructor Create;

    property IsPresetDictionaryPresent : Boolean
      read GetIsPresetDictionaryPresent;
    property CompressionLevel : Byte
      read GetCompressionLevel write SetCompressionLevel;
    property CompressionInfo : Byte
      read GetCompressionInfo write SetCompressionInfo;

    property CompressionMethod : Byte
      read GetCompressionMethod write SetCompressionMethod;
    property Adler32 : LongInt
      read FAdler32 write FAdler32;

    property FCheck : Byte
      read GetFCheck;

    procedure SaveZLHeaderToStream(AStream : TStream);
    procedure ReadZLHeaderFromStream(AStream : TStream);
  end;

  TAbZLStreamHelper = class(TAbArchiveStreamHelper)
  protected { private }
    FItem : TAbZLItem;
  public
    constructor Create(AStream : TStream);
    destructor Destroy; override;

    property Item : TAbZLItem
      read FItem;

    procedure ExtractItemData(AStream : TStream); override;
    function FindFirstItem : Boolean; override;
    function FindNextItem : Boolean; override;
    procedure ReadHeader; override;
    procedure ReadTail; override;
    function SeekItem(Index : Integer): Boolean; override;
    procedure WriteArchiveHeader; override;
    procedure WriteArchiveItem(AStream : TStream); override;
    procedure WriteArchiveTail; override;
    function GetItemCount : Integer; override;
  end;


implementation

{ TAbZLStreamHelper }

constructor TAbZLStreamHelper.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FItem := TAbZLItem.Create;
end;

destructor TAbZLStreamHelper.Destroy;
begin
  FItem.Free;
  inherited Destroy;
end;

procedure TAbZLStreamHelper.ExtractItemData(AStream: TStream);
{ assumes already positioned appropriately }
var
  Hlpr : TAbDeflateHelper;
begin
  Hlpr := TAbDeflateHelper.Create;
  Hlpr.Options := Hlpr.Options or dfc_UseAdler32;
  if not FItem.IsPresetDictionaryPresent then
    Inflate(FStream, AStream, Hlpr)
  else
    raise Exception.Create('preset dictionaries unsupported');
  Hlpr.Free;
end;

function TAbZLStreamHelper.FindFirstItem: Boolean;
var
  ZLH : TAbZLHeader;
begin
  FStream.Seek(0, soFromBeginning);
  Result := FStream.Read(ZLH, SizeOf(TAbZLHeader)) = SizeOf(TAbZLHeader);
  FItem.FZLHeader := ZLH;
  FStream.Seek(0, soFromBeginning);
end;

function TAbZLStreamHelper.FindNextItem: Boolean;
begin
  { only one item in a ZLib Stream }
  Result := FindFirstItem;
end;

function TAbZLStreamHelper.GetItemCount: Integer;
begin
  { only one item in a ZLib Stream }
  Result := 1;
end;

procedure TAbZLStreamHelper.ReadHeader;
{ assumes already positioned appropriately }
var
  ZLH : TAbZLHeader;
begin
  FStream.Read(ZLH, SizeOf(TAbZlHeader));
  FItem.FZLHeader := ZLH;
end;

procedure TAbZLStreamHelper.ReadTail;
{ assumes already positioned appropriately }
var
  Adler: LongInt;
begin
  FStream.Read(Adler, SizeOf(LongInt));
  FItem.Adler32 := Adler;
end;

function TAbZLStreamHelper.SeekItem(Index: Integer): Boolean;
begin
  { only one item in a ZLib Stream }
  if Index <> 1 then
    Result := False
  else
    Result := FindFirstItem;
end;

procedure TAbZLStreamHelper.WriteArchiveHeader;
begin
  Item.SaveZLHeaderToStream(FStream);
end;

procedure TAbZLStreamHelper.WriteArchiveItem(AStream: TStream);
var
  Hlpr : TAbDeflateHelper;
begin
  { Compress file }
  Hlpr := TAbDeflateHelper.Create;
  Hlpr.Options := Hlpr.Options or dfc_UseAdler32;
  Item.Adler32 := AbDfEnc.Deflate(AStream, FStream, Hlpr);
  Hlpr.Free;
end;

procedure TAbZLStreamHelper.WriteArchiveTail;
var
  Ad32 : LongInt;
begin
  Ad32 := AbSwapLongEndianness(Item.Adler32);
  FStream.Write(Ad32, SizeOf(LongInt));
end;

{ TAbZLItem }

constructor TAbZLItem.Create;
begin
  { Set default Values for fields }
  FillChar(FZLHeader, SizeOf(TAbZlHeader), #0);
  FZLHeader.CMF := (AB_ZL_DEF_COMPRESSIONINFO shl 4); { 32k Window size }
  FZLHeader.CMF := FZLHeader.CMF or AB_ZL_DEF_COMPRESSIONMETHOD; { Deflate }
  FZLHeader.FLG := FZLHeader.FLG and not AB_ZL_PRESET_DICT; { no preset dictionary}
  FZLHeader.FLG := FZLHeader.FLG or (AB_ZL_DEFAULT_COMPRESSION shl 6); { assume default compression }
  MakeFCheck;
end;

function TAbZLItem.GetCompressionInfo: Byte;
begin
  Result := FZLHeader.CMF shr 4;
end;

function TAbZLItem.GetCompressionLevel: Byte;
begin
  Result := FZLHeader.FLG shr 6;
end;

function TAbZLItem.GetCompressionMethod: Byte;
begin
  Result := FZLHeader.CMF and AB_ZL_CM_MASK;
end;

function TAbZLItem.GetFCheck: Byte;
begin
  Result := FZLHeader.FLG and AB_ZL_FCHECK_MASK;
end;

function TAbZLItem.GetIsPresetDictionaryPresent: Boolean;
begin
  Result := (FZLHeader.FLG and AB_ZL_PRESET_DICT) = AB_ZL_PRESET_DICT;
end;

procedure TAbZLItem.MakeFCheck;
{ create the FCheck value for the current Header }
var
  zlh : Word;
begin
  FZLHeader.FLG := FZLHeader.FLG and not AB_ZL_FCHECK_MASK;
  zlh := (FZLHeader.CMF * 256) + FZLHeader.FLG;
  Inc(FZLHeader.FLG, 31 - (zlh mod 31));
end;

procedure TAbZLItem.ReadZLHeaderFromStream(AStream: TStream);
begin
  AStream.Read(FZLHeader, SizeOf(TAbZLHeader));
end;

procedure TAbZLItem.SaveZLHeaderToStream(AStream: TStream);
begin
  MakeFCheck;
  AStream.Write(FZLHeader, SizeOf(TAbZlHeader));
end;

procedure TAbZLItem.SetCompressionInfo(Value: Byte);
begin
  FZLHeader.CMF := FZLHeader.CMF and not AB_ZL_CINFO_MASK;
  FZLHeader.CMF := FZLHeader.CMF or (Value shl 4); { shift value and add to bit field }
end;

procedure TAbZLItem.SetCompressionLevel(Value: Byte);
var
  Temp : Byte;
begin
  Temp := Value;
  if not Temp in [AB_ZL_FASTEST_COMPRESSION..AB_ZL_MAXIMUM_COMPRESSION] then
    Temp := AB_ZL_DEFAULT_COMPRESSION;
  FZLHeader.FLG := FZLHeader.FLG and not AB_ZL_FLEVEL_MASK;
  FZLHeader.FLG := FZLHeader.FLG or (Temp shl 6); { shift value and add to bit field }
end;

procedure TAbZLItem.SetCompressionMethod(Value: Byte);
begin
  if Value > AB_ZL_CM_MASK then Value := (Value shl 4) shr 4;
  FZLHeader.CMF := FZLHeader.CMF and not AB_ZL_CM_MASK;
  FZLHeader.CMF := FZLHeader.CMF or Value;
end;

end.
