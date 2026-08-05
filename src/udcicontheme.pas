{
    Double Commander
    -------------------------------------------------------------------------
    Icon Theme with ZIP archive support

    Copyright (C) 2026 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uDCIconTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DCStringHashListUtf8, DCStringHashTable,
  uIconTheme;

type

  { TZipItem }

  TZipItem = class
  private
    FPos: PByte;
    FSize: Int64;
  end;

  { TZipArchive }

  TZipArchive = class
  private
    FStream: TMemoryStream;
    FList: TDCStringHashTable;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
    function GetIcons(const Directory: String): TStringHashListUtf8;
  end;

  { TDCIconTheme }

  TDCIconTheme = class(TIconTheme)
  private
    FBasePath: String;
    FArchive: TZipArchive;
    function LoadIcon(AStream: TStream): TBitmap;
    function LoadIconFromFile(const FileName: String; ASize: Integer): TBitmap;
    function LoadIconFromArchive(const FileName: String; ASize: Integer): TBitmap;
  protected
    function CreateParentTheme(const sThemeName: String): TIconTheme; override;
    function LoadThemeWithInherited(AInherits: TStringList): Boolean; override;
  public
    function LoadThemeIcon(const AIconName: String; AIconSize: Integer): TBitmap;
  end;

implementation

uses
  IntegerList, DCOSUtils, DCStrUtils, uDCUtils, uPixMapManager, uClassesEx,
  uVectorImage, uDebug, uGraphics;

const
  ZIP_LOCAL_SIGN       = $04034b50;
  ZIP_CENTRAL_SIGN     = $02014b50;
  ZIP_END_CENTRAL_SIGN = $06054b50;

type
  TLocalFileHeader = packed record
    Signature: UInt32;
    VersionNeeded: UInt16;
    GeneralPurposeFlag: UInt16;
    CompressionMethod: UInt16;
    LastModFileTime: UInt16;
    LastModFileDate: UInt16;
    CRC32: UInt32;
    CompressedSize: UInt32;
    UncompressedSize: UInt32;
    FileNameLength: UInt16;
    ExtraFieldLength: UInt16;
    // FileName (variable size)
    // ExtraField (variable size)
  end;

  TCentralDirectoryHeader = packed record
    Signature: UInt32;
    VersionMadeBy: UInt16;
    VersionNeeded: UInt16;
    GeneralPurposeFlag: UInt16;
    CompressionMethod: UInt16;
    LastModFileTime: UInt16;
    LastModFileDate: UInt16;
    CRC32: UInt32;
    CompressedSize: UInt32;
    UncompressedSize: UInt32;
    FileNameLength: UInt16;
    ExtraFieldLength: UInt16;
    FileCommentLength: UInt16;
    DiskNumberStart: UInt16;
    InternalFileAttributes: UInt16;
    ExternalFileAttributes: UInt32;
    RelativeOffsetLocalHeader: UInt32;
    // FileName (variable size)
    // ExtraField (variable size)
    // FileComment (variable size)
  end;

  TEndCentralDirectory = packed record
    Signature: UInt32;
    DiskNumber: UInt16;
    CentralDirectoryStartDisk: UInt16;
    EntriesThisDisk: UInt16;
    EntriesTotalNumber: UInt16;
    CentralDirectorySize: UInt32;
    StartDiskOffset: UInt32;
    CommentLength: UInt16;
    // Comment (variable size)
  end;

{ TZipArchive }

constructor TZipArchive.Create(const FileName: String);
var
  APos: Int64;
  ASize: Int64;
  AName: String;
  Index: Integer;
  AItem: TZipItem;
  AList: TCardinalList;
  ALocal: TLocalFileHeader;
  AEndDir: TEndCentralDirectory;
  AHeader: TCentralDirectoryHeader;
begin
  FStream:= TMemoryStream.Create;
  try
    FStream.LoadFromFile(FileName);
  except
    FreeAndNil(FStream);
    raise;
  end;

  APos:= FStream.Seek(-SizeOf(TEndCentralDirectory), soEnd);

  if (APos < 0) then
  begin
    raise EInvalidContainer.Create(EmptyStr);
  end;

  FStream.ReadBuffer(AEndDir, SizeOf(AEndDir));

  if (AEndDir.Signature <> ZIP_END_CENTRAL_SIGN) then
  begin
    raise EInvalidContainer.Create(EmptyStr);
  end;

  ASize:= FStream.Size;
  AList:= TCardinalList.Create;
  FList:= TDCStringHashTable.Create;
  try
    FStream.Seek(AEndDir.StartDiskOffset, soBeginning);

    for Index:= 0 to Int32(AEndDir.EntriesThisDisk) - 1 do
    begin
      FStream.ReadBuffer(AHeader, SizeOf(AHeader));

      if (AHeader.Signature <> ZIP_CENTRAL_SIGN) or
         (AHeader.CompressionMethod <> 0) then
      begin
        raise EInvalidContainer.Create(EmptyStr);
      end;

      FStream.Seek(AHeader.FileNameLength, soCurrent);
      FStream.Seek(AHeader.ExtraFieldLength, soCurrent);
      FStream.Seek(AHeader.FileCommentLength, soCurrent);

      if (AHeader.ExternalFileAttributes and faDirectory = 0) then
      begin
        AList.Add(AHeader.RelativeOffsetLocalHeader);
      end;
    end;

    for Index:= 0 to AList.Count - 1 do
    begin
      FStream.Seek(AList[Index], soBeginning);
      FStream.ReadBuffer(ALocal, SizeOf(ALocal));

      if (ALocal.Signature <> ZIP_LOCAL_SIGN) or
         (ALocal.CompressionMethod <> 0) then
      begin
        raise EInvalidContainer.Create(EmptyStr);
      end;

      SetLength(AName, ALocal.FileNameLength);
      FStream.ReadBuffer(AName[1], ALocal.FileNameLength);
      FStream.Seek(ALocal.ExtraFieldLength, soCurrent);

      APos:= FStream.Position;

      if (APos + ALocal.UncompressedSize > ASize) then
      begin
        raise EInvalidContainer.Create(EmptyStr);
      end;

      AItem:= TZipItem.Create;
      AItem.FSize:= ALocal.UncompressedSize;
      AItem.FPos:= PByte(FStream.Memory) + APos;

      AName:= NormalizePathDelimiters(AName);

      FList.Add(AName, AItem);
    end;
  except
    FreeAndNil(FStream);
    FreeAndNil(FList);
    AList.Free;
    raise;
  end;
end;

destructor TZipArchive.Destroy;
begin
  FList.Free;
  FStream.Free;
  inherited Destroy;
end;

function TZipArchive.GetIcons(const Directory: String): TStringHashListUtf8;
var
  I: Integer;
  ExtIdx: IntPtr;
  D, S, E: String;
begin
  D:= NormalizePathDelimiters(Directory);
  Result:= TStringHashListUtf8.Create(True);
  for I:= 0 to FList.Count - 1 do
  begin
    S:= FList.Items[I]^.Key;
    if StrBegins(S, D) then
    begin
      E:= LowerCase(ExtractOnlyFileExt(S));
      if (E = 'svg') then
        ExtIdx:= EXT_IDX_SVG
      else if (E = 'png') then
        ExtIdx:= EXT_IDX_PNG
      else if (E = 'xpm') then
        ExtIdx:= EXT_IDX_XPM
      else begin
        Break;
      end;
      Result.Add(ExtractOnlyFileName(S), Pointer(ExtIdx));
    end;
  end;
end;

{ TDCIconTheme }

function TDCIconTheme.LoadIcon(AStream: TStream): TBitmap;
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Result := Graphics.TBitmap.Create;
    try
      Picture.LoadFromStream(AStream);
      Result.Assign(Picture.Graphic);

      // if unsupported BitsPerPixel
      if Result.RawImage.Description.BitsPerPixel > 32 then
        BitmapConvert(Result);
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        DCDebug(Format('Error: Cannot load pixmap : %s', [e.Message]));
      end;
    end;
  finally
    FreeAndNil(Picture);
  end;
end;

function TDCIconTheme.LoadIconFromArchive(const FileName: String; ASize: Integer): TBitmap;
var
  ALen: Integer;
  AItem: TZipItem;
  AStream: TStream;
  AFileName: String;
  ANode: PDCHashItem;
begin
  ALen:= Length(FBasePath);
  AFileName:= Copy(FileName, ALen + 1, MaxInt);
  AFileName:= NormalizePathDelimiters(AFileName);

  ANode:= FArchive.FList.Find(AFileName);

  if (ANode = nil) then Exit(nil);
  AItem:= TZipItem(ANode^.Value);

  AStream:= TBlobStream.Create(AItem.FPos, AItem.FSize);
  try
    if TScalableVectorGraphics.IsFileExtensionSupported(ExtractFileExt(AFileName)) then
      Result := TScalableVectorGraphics.CreateBitmap(AStream, ASize, ASize)
    else begin
      Result := LoadIcon(AStream);
      if Assigned(Result) then begin
        Result := StretchBitmap(Result, ASize, clNone, True);
      end;
    end;
  finally
    AStream.Free;
  end;
end;

function TDCIconTheme.CreateParentTheme(const sThemeName: String): TIconTheme;
begin
  Result:= TDCIconTheme.Create(sThemeName, FBaseDirListAtCreate);
end;

function TDCIconTheme.LoadIconFromFile(const FileName: String; ASize: Integer): TBitmap;
begin
  if TScalableVectorGraphics.IsFileExtensionSupported(ExtractFileExt(FileName)) then
    Result:= TScalableVectorGraphics.CreateBitmap(FileName, ASize, ASize)
  else begin
    PixMapManager.LoadBitmapFromFile(FileName, Result);
    if Assigned(Result) then begin
      Result:= StretchBitmap(Result, ASize, clNone, True);
    end;
  end;
end;

function TDCIconTheme.LoadThemeWithInherited(AInherits: TStringList): Boolean;
var
  I: Integer;
  FileName: String;
begin
  Result:= inherited LoadThemeWithInherited(AInherits);
  if Result then
  begin
    FBasePath:= FBaseDirList[FCacheIndex] + PathDelim + FTheme + PathDelim;
    FileName:= FBasePath + 'icon-theme.zip';
    if mbFileExists(FileName) then
    try
      FArchive:= TZipArchive.Create(FileName);
      DCDebug('Loading theme icons from zip');
      for I:= 0 to FDirectories.Count - 1 do
      begin
        FDirectories.Items[I]^.FileListCache[FCacheIndex]:= FArchive.GetIcons(FDirectories[I]);
      end;
    except
      DCDebug('ERROR: Invalid archive - ', FileName);
    end;
  end;
end;

function TDCIconTheme.LoadThemeIcon(const AIconName: String; AIconSize: Integer): TBitmap;
var
  FileName: String;
  bitmapSize: Integer;
  AIconTheme: TDCIconTheme;
begin
  bitmapSize:= Round(AIconSize * findScaleFactorByFirstForm());
  FileName:= FindIcon(AIconName, bitmapSize, 1);
  if FileName = EmptyStr then Exit(nil);

  if FParentIndex < 0 then
    AIconTheme:= Self
  else begin
    AIconTheme:= TDCIconTheme(FInherits.Objects[FParentIndex]);
  end;

  with AIconTheme do
  begin
    if Assigned(FArchive) and (FBaseDirIndex = FCacheIndex) then
      Result:= LoadIconFromArchive(FileName, bitmapSize)
    else begin
      Result:= LoadIconFromFile(FileName, bitmapSize);
    end;
  end;
end;

end.

