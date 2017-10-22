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
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbSpanSt.pas                                *}
{*********************************************************}
{* ABBREVIA: TAbSpan*Stream Classes                      *}
{*********************************************************}
{* Streams to handle splitting ZIP files or spanning     *}
{* them to diskettes                                     *}
{*********************************************************}

unit AbSpanSt;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp;

type
{ TAbSpanBaseStream interface ============================================== }
  TAbSpanBaseStream = class(TStream)
  protected {private}
    FArchiveName: string;

    FOnRequestImage: TAbRequestImageEvent;

  protected {methods}
    function GetImageName( ImageNumber: Integer ): string;

  public {methods}
    constructor Create( const ArchiveName: string );

  public {events}
    property OnRequestImage : TAbRequestImageEvent
      read FOnRequestImage
      write FOnRequestImage;
  end;

{ TAbSpanReadStream interface ============================================== }
  TAbSpanReadStream = class(TAbSpanBaseStream)
  protected {private}
    FCurrentImage: LongWord;
    FIsSplit: Boolean;
    FLastImage: LongWord;
    FStream: TStream;

    FOnRequestNthDisk : TAbRequestNthDiskEvent;

  protected {methods}
    procedure GotoImage( ImageNumber: Integer );
    procedure SetOnRequestImage(Value: TAbRequestImageEvent);

  public {methods}
    constructor Create( const ArchiveName: string; CurrentImage: LongWord;
      Stream: TStream );
    destructor Destroy;
      override;
    function Read(var Buffer; Count: Longint): Longint;
      override;
    function Write(const Buffer; Count: Longint): Longint;
      override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      override;
    procedure SeekImage( Image: LongWord; const Offset: Int64);

  public {events}
    property OnRequestImage
      write SetOnRequestImage;
    property OnRequestNthDisk : TAbRequestNthDiskEvent
      read FOnRequestNthDisk
      write FOnRequestNthDisk;
  end;

{ TAbSpanWriteStream interface ============================================= }
  TAbSpanWriteStream = class(TAbSpanBaseStream)
  protected {private}
    FCurrentImage: LongWord;
    FImageSize: Int64;
    FStream: TStream;
    FThreshold: Int64;

    FOnRequestBlankDisk : TAbRequestDiskEvent;

  protected {methods}
    procedure NewImage;

  public {methods}
    constructor Create( const ArchiveName: string; Stream: TStream;
      Threshold: Int64 );
    destructor Destroy;
      override;
    function Read(var Buffer; Count: Longint): Longint;
      override;
    function Write(const Buffer; Count: Longint): Longint;
      override;
    function WriteUnspanned(const Buffer; Count: Longint;
      FailOnSpan: Boolean = False): Boolean;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      override;
    function ReleaseStream: TStream;

  public {properties}
    property CurrentImage : LongWord
      read FCurrentImage;

  public {events}
    property OnRequestBlankDisk : TAbRequestDiskEvent
      read FOnRequestBlankDisk
      write FOnRequestBlankDisk;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Math, RTLConsts, SysUtils, AbUtils, AbExcept, DCOSUtils, DCClassesUtf8;


{============================================================================}
{ TAbSpanBaseStream implementation ========================================= }
constructor TAbSpanBaseStream.Create( const ArchiveName: string );
begin
  inherited Create;
  FArchiveName := ArchiveName;
end;
{------------------------------------------------------------------------------}
function TAbSpanBaseStream.GetImageName( ImageNumber: Integer ): string;
var
  Abort : Boolean;
  Ext : string;
begin
  {generate default name}
  Ext := ExtractFileExt(FArchiveName);
  if (Length(Ext) < 2) then
    Ext := '.' + Format('%.2d', [ImageNumber])
  else
    Ext := Ext[1] + Ext[2] + Format('%.2d', [ImageNumber]);
  Result := ChangeFileExt(FArchiveName, Ext);
  {call event}
  if Assigned(FOnRequestImage) then begin
    Abort := False;
    FOnRequestImage(Self, ImageNumber, Result, Abort);
    if Abort then
      raise EAbUserAbort.Create;
  end;
end;

{============================================================================}
{ TAbSpanReadStream implementation ========================================= }
constructor TAbSpanReadStream.Create( const ArchiveName: string;
  CurrentImage: LongWord; Stream: TStream );
begin
  inherited Create(ArchiveName);
  FCurrentImage := CurrentImage;
  FIsSplit := mbFileExists(GetImageName(1)) or not AbDriveIsRemovable(ArchiveName);
  FLastImage := CurrentImage;
  FStream := Stream;
end;
{------------------------------------------------------------------------------}
destructor TAbSpanReadStream.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TAbSpanReadStream.GotoImage( ImageNumber: Integer );
var
  Abort: Boolean;
  ImageName: string;
begin
  { switch to the requested image.  ImageNumber is passed in as 0-based to
    match the zip spec, but all of the callbacks receive 1-based values. }
  FreeAndNil(FStream);
  FCurrentImage := ImageNumber;
  Inc(ImageNumber);
  ImageName := FArchiveName;
  if FIsSplit then begin
    { the last image uses the original filename }
    if FCurrentImage <> FLastImage then
      ImageName := GetImageName(ImageNumber)
  end
  else if Assigned(FOnRequestNthDisk) then begin
    Abort := False;
    repeat
      FOnRequestNthDisk(Self, ImageNumber, Abort);
      if Abort then
        raise EAbUserAbort.Create;
    until AbGetDriveFreeSpace(ImageName) <> -1;
  end
  else
    raise EAbUserAbort.Create;
  FStream := TFileStreamEx.Create(ImageName, fmOpenRead or fmShareDenyWrite);
end;
{------------------------------------------------------------------------------}
function TAbSpanReadStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesRead, BytesLeft: LongInt;
  PBuf: PByte;
begin
  { read until the buffer's full, switching images if necessary }
  Result := 0;
  if FStream = nil then
    Exit;
  PBuf := @Buffer;
  BytesLeft := Count;
  while Result < Count do begin
    BytesRead := FStream.Read(PBuf^, BytesLeft);
    Inc(Result, BytesRead);
    Inc(PBuf, BytesRead);
    Dec(BytesLeft, BytesRead);
    if BytesRead < BytesLeft then begin
      if FCurrentImage <> FLastImage then
        GotoImage(FCurrentImage + 1)
      else
        Break;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TAbSpanReadStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EAbException.Create('TAbSpanReadStream.Write unsupported');
end;
{------------------------------------------------------------------------------}
function TAbSpanReadStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if FStream = nil then
    Result := 0
  else if (Offset = 0) and (Origin = soCurrent) then
    Result := FStream.Position
  else
    raise EAbException.Create('TAbSpanReadStream.Seek unsupported');
end;
{------------------------------------------------------------------------------}
procedure TAbSpanReadStream.SeekImage( Image: LongWord; const Offset: Int64);
begin
  if FStream = nil then
    Exit;
  if FCurrentImage <> Image then
    GotoImage(Image);
  FStream.Position := Offset;
end;
{------------------------------------------------------------------------------}
procedure TAbSpanReadStream.SetOnRequestImage(Value: TAbRequestImageEvent);
begin
  FOnRequestImage := Value;
  FIsSplit := mbFileExists(GetImageName(1)) or not AbDriveIsRemovable(FArchiveName);
end;

{============================================================================}
{ TAbSpanWriteStream implementation ======================================== }
constructor TAbSpanWriteStream.Create( const ArchiveName: string;
  Stream: TStream; Threshold: Int64 );
begin
  inherited Create(ArchiveName);
  FCurrentImage := 0;
  FStream := Stream;
  FThreshold := Threshold;
end;
{------------------------------------------------------------------------------}
destructor TAbSpanWriteStream.Destroy;
begin
  FStream.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TAbSpanWriteStream.NewImage;
var
  Abort: Boolean;
begin
  { start a new span or blank disk.  FCurrentImage is 0-based to match the zip
    spec, but all of the callbacks receive 1-based values. }
  FreeAndNil(FStream);
  Inc(FCurrentImage);
  if FThreshold > 0 then
    mbRenameFile(FArchiveName, GetImageName(FCurrentImage))
  else begin
    if Assigned(FOnRequestBlankDisk) then begin
      Abort := False;
      repeat
        FOnRequestBlankDisk(Self, Abort);
        if Abort then
          raise EAbUserAbort.Create;
      until AbGetDriveFreeSpace(FArchiveName) <> -1;
    end
    else
      raise EAbUserAbort.Create;
    AbSetSpanVolumeLabel(AbDrive(FArchiveName), FCurrentImage);
  end;
  FStream := TFileStreamEx.Create(FArchiveName, fmCreate or fmShareDenyWrite);
  FImageSize := 0;
end;
{------------------------------------------------------------------------------}
function TAbSpanWriteStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EAbException.Create('TAbSpanWriteStream.Read unsupported');
end;
{------------------------------------------------------------------------------}
function TAbSpanWriteStream.Write(const Buffer; Count: Longint): Longint;
var
  BytesWritten, BytesLeft: LongInt;
  PBuf: PByte;
begin
  { write until the buffer is done, starting new spans if necessary }
  Result := 0;
  if FStream = nil then
    Exit;
  PBuf := @Buffer;
  BytesLeft := Count;
  while Result < Count do begin
    if FThreshold > 0 then
      BytesWritten := FStream.Write(PBuf^, Min(BytesLeft, FThreshold - FImageSize))
    else
      BytesWritten := FStream.Write(PBuf^, BytesLeft);
    Inc(FImageSize, BytesWritten);
    Inc(Result, BytesWritten);
    Inc(PBuf, BytesWritten);
    Dec(BytesLeft, BytesWritten);
    if BytesWritten < BytesLeft then
      NewImage;
  end;
end;
{------------------------------------------------------------------------------}
function TAbSpanWriteStream.WriteUnspanned(const Buffer; Count: Longint;
  FailOnSpan: Boolean = False): Boolean;
var
  BytesWritten: LongInt;
begin
  { write as a contiguous block, starting a new span if there isn't room.
    FailOnSpan (and result = false) can be used to update data before it's
    written again }
  if FStream = nil then
    raise EWriteError.Create(SWriteError);
  if (FThreshold > 0) and (FThreshold - FImageSize < Count) then
    BytesWritten := 0
  else
    BytesWritten := FStream.Write(Buffer, Count);
  if BytesWritten < Count then begin
    if BytesWritten > 0 then
      FStream.Size := FStream.Size - BytesWritten;
    NewImage;
    if FailOnSpan then
      BytesWritten := 0
    else begin
      BytesWritten := Count;
      FStream.WriteBuffer(Buffer, Count);
    end;
  end;
  Inc(FImageSize, BytesWritten);
  Result := (BytesWritten = Count);
end;
{------------------------------------------------------------------------------}
function TAbSpanWriteStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if FStream = nil then
    Result := 0
  else if (Offset = 0) and (Origin = soCurrent) then
    Result := FStream.Position
  else
    raise EAbException.Create('TAbSpanWriteStream.Seek unsupported');
end;
{------------------------------------------------------------------------------}
function TAbSpanWriteStream.ReleaseStream: TStream;
begin
  Result := FStream;
  FStream := nil;
end;
{------------------------------------------------------------------------------}
end.
