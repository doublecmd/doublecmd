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
 * The Initial Developer of the Original Code is Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbUnzOutStm.pas                             *}
{*********************************************************}
{* ABBREVIA: UnZip output stream;  progress and CRC32    *}
{*********************************************************}

unit AbUnzOutStm;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, AbArcTyp;

type
  // Fixed-length read-only stream, limits reads to the range between
  // the input stream's starting position and a specified size.  Seek/Position
  // are adjusted to be 0 based.
  TAbUnzipSubsetStream = class( TStream )
  private
    FStream : TStream;
    FStartPos: Int64;
    FCurPos: Int64;
    FEndPos: Int64;

  public
    constructor Create(aStream: TStream; aStreamSize: Int64);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;


  // Write-only output stream, computes CRC32 and calls progress event
  TAbUnzipOutputStream = class( TStream )
  private
    FBytesWritten : Int64;
    FCRC32 : LongInt;
    FCurrentProgress : Byte;
    FStream : TStream;
    FUncompressedSize : Int64;
    FOnProgress : TAbProgressEvent;

    function GetCRC32 : LongInt;

  public
    constructor Create(aStream : TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property CRC32 : LongInt
      read GetCRC32;
    property Stream : TStream
      read FStream
      write FStream;
    property UncompressedSize : Int64
      read FUncompressedSize
      write FUncompressedSize;
    property OnProgress : TAbProgressEvent
      read FOnProgress
      write FOnProgress;
  end;


implementation

uses
  Math, AbExcept, AbUtils;

{ TAbUnzipSubsetStream implementation ====================================== }

{ -------------------------------------------------------------------------- }
constructor TAbUnzipSubsetStream.Create(aStream: TStream; aStreamSize: Int64);
begin
  inherited Create;
  FStream := aStream;
  FStartPos := FStream.Position;
  FCurPos := FStartPos;
  FEndPos := FStartPos + aStreamSize;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipSubsetStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count > FEndPos - FCurPos then
    Count := FEndPos - FCurPos;
  if Count > 0 then begin
    Result := FStream.Read(Buffer, Count);
    Inc(FCurPos, Result);
  end
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipSubsetStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EAbException.Create('TAbUnzipSubsetStream.Write not supported');
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipSubsetStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  OldPos: Int64;
begin
  OldPos := FCurPos;
  case Origin of
    soBeginning: FCurPos := FStartPos + Offset;
    soCurrent: FCurPos := FCurPos + Offset;
    soEnd: FCurPos := FEndPos + Offset;
  end;
  if FCurPos < FStartPos then
    FCurPos := FStartPos;
  if FCurPos > FEndPos then
    FCurPos := FEndPos;
  if OldPos <> FCurPos then
    FStream.Position := FCurPos;
  Result := FCurPos - FStartPos;
end;
{ -------------------------------------------------------------------------- }


{ TAbUnzipOutputStream implementation ====================================== }

{ -------------------------------------------------------------------------- }
constructor TAbUnzipOutputStream.Create(aStream: TStream);
begin
  inherited Create;
  FStream := aStream;
  FCRC32 := -1;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.Read(var Buffer; Count: Integer): Longint;
begin
  raise EAbException.Create('TAbUnzipOutputStream.Read not supported');
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.Write(const Buffer; Count: Longint): Longint;
var
  Abort : Boolean;
  NewProgress : Byte;
begin
  Result := FStream.Write(Buffer, Count);

  AbUpdateCRC( FCRC32, Buffer, Count );

  Inc( FBytesWritten, Result );
  if Assigned( FOnProgress ) then begin
    Abort := False;
    NewProgress := AbPercentage(FBytesWritten, FUncompressedSize);
    if (NewProgress <> FCurrentProgress) then begin
      FOnProgress( NewProgress, Abort );
      FCurrentProgress := NewProgress;
    end;
    if Abort then
      raise EAbUserAbort.Create;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.GetCRC32: LongInt;
begin
  Result := not FCRC32;
end;

end.

