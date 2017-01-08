{
  Double Commander
  -------------------------------------------------------------------------
  Simple exchangeable image file format reader

  Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit uExifReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamEx;

type

  { TTag }

  TTag = packed record
    ID      : UInt16;   // Tag number
    Typ     : UInt16;   // Tag type
    Count   : UInt32;   // Tag length
    Offset  : UInt32;   // Offset / Value
  end;

  { TExifReader }

  TExifReader = class(TMemoryStream)
  private
    FOffset: Int64;
    FSwap: Boolean;
  protected
    FMake: String;
    FModel: String;
    FImageWidth: UInt16;
    FImageHeight: UInt16;
    FOrientation: UInt16;
    FDateTimeOriginal: String;
  private
    procedure Reset;
    function ReadString(Offset, Count: Int32): String;
    procedure ReadTag(var ATag: TTag);
    function DoImageFileDirectory: Boolean;
  public
    function LoadFromFile(const FileName: String): Boolean;
    property Make: String read FMake;
    property Model: String read FModel;
    property ImageWidth: UInt16 read FImageWidth;
    property ImageHeight: UInt16 read FImageHeight;
    property Orientation: UInt16 read FOrientation;
    property DateTimeOriginal: String read FDateTimeOriginal;
  end;

resourcestring
  rsMake = 'Manufacturer';
  rsModel = 'Camera model';
  rsImageWidth = 'Width';
  rsImageHeight = 'Height';
  rsOrientation = 'Orientation';
  rsDateTimeOriginal = 'Date taken';

implementation

uses
  Math, DCClassesUtf8;

{ TExifReader }

procedure TExifReader.Reset;
begin
  Clear;
  FImageWidth:= 0;
  FImageHeight:= 0;
  FOrientation:= 0;
  FMake:= EmptyStr;
  FModel:= EmptyStr;
  FDateTimeOriginal:= EmptyStr;
end;

function TExifReader.ReadString(Offset, Count: Int32): String;
var
  AOffset: Int64;
begin
  AOffset:= Self.Seek(0, soCurrent);
  Self.Seek(Offset, soBeginning);
  SetLength(Result, Count);
  Self.ReadBuffer(Result[1], Count);
  Result:= PAnsiChar(Result);
  Self.Seek(AOffset, soBeginning);
end;

procedure TExifReader.ReadTag(var ATag: TTag);
begin
  Self.ReadBuffer(ATag, SizeOf(TTag));
  if FSwap = False then
  begin
    case ATag.Typ of
      1, 6: ATag.Offset:= UInt8(ATag.Offset);
      3, 8: ATag.Offset:= UInt16(ATag.Offset);
    end;
  end
  else begin
    ATag.ID:= SwapEndian(ATag.ID);
    ATag.Typ:= SwapEndian(ATag.Typ);
    ATag.Count:= SwapEndian(ATag.Count);
    case ATag.Typ of
      1, 6: ATag.Offset:= UInt8(ATag.Offset);
      3, 8: ATag.Offset:= SwapEndian(UInt16(ATag.Offset));
      else  ATag.Offset:= SwapEndian(ATag.Offset);
    end;
  end;
end;

function TExifReader.DoImageFileDirectory: Boolean;
var
  I: Int32;
  ATag: TTag;
  ACount: UInt16;
  AOffset: Int32 = 0;
begin
  ACount:= Self.ReadWord;
  if FSwap then ACount:= SwapEndian(ACount);
  for I:= 1 to ACount do
  begin
    ReadTag(ATag);
    case ATag.ID of
     $100: // Image width
       begin
         FImageWidth := ATag.Offset;
       end;
     $101: // Image height
       begin
         FImageHeight := ATag.Offset;
       end;
     $010f: // Shows manufacturer of digicam
       begin
         FMake:= ReadString(ATag.Offset + FOffset, ATag.Count);
       end;
     $0110: // Shows model number of digicam
       begin
         FModel:= ReadString(ATag.Offset + FOffset, ATag.Count);
       end;
     $0112: // The orientation of the camera relative to the scene
       begin
         FOrientation:= ATag.Offset;
       end;
     $8769: // Exif IFD Pointer
       begin
         AOffset:= ATag.Offset;
       end;
    end;
  end;
  Result:= ACount > 0;
  if AOffset > 0 then
  begin
    Self.Seek(FOffset + AOffset, soBeginning);
    ACount:= Self.ReadWord;
    if FSwap then ACount:= SwapEndian(ACount);
    for I:= 1 to ACount do
    begin
      ReadTag(ATag);
      case ATag.ID of
        $9003: // Date/Time of original image taken
          begin
            FDateTimeOriginal:= ReadString(ATag.Offset + FOffset, ATag.Count);
          end;
        // Image pixel width
        $A002: if FImageWidth = 0 then FImageWidth := ATag.Offset;
        // Image pixel height
        $A003: if FImageHeight = 0 then FImageHeight := ATag.Offset;
      end;
    end;
  end;
end;

function TExifReader.LoadFromFile(const FileName: String): Boolean;
const
  BUFFER_SIZE = 196608;
var
  P: UInt16;
  ASize: UInt16;
  Offset: UInt32;
  AFile: TFileStreamEx;
  Magic: array [0..5] of AnsiChar;
begin
  Reset;
  try
    AFile:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Self.SetSize(Min(AFile.Size, BUFFER_SIZE));
      AFile.ReadBuffer(Self.Memory^, Self.Size);
    finally
      AFile.Free;
    end;
  except
    Exit(False);
  end;
  try
    if (Self.ReadByte <> $FF) then Exit(False);
    if (Self.ReadByte <> $D8) then Exit(False);

    repeat
      if Self.ReadByte = $FF then
      begin
        case Self.ReadByte of
          $E1: // Exif Marker
            begin
              Break;
            end;
          $D9: // End Of Image (EOI)
            begin
              Exit(False);
            end;
          else begin // Unknown section, skip
            P:= Self.ReadWordBE;
            Self.Seek(Int64(P) - 2, soCurrent);
          end;
        end;
      end;
    until False;

    // Exif data size
    ASize:= Self.ReadWordBE;
    // Exif magic string
    Self.Read(Magic, SizeOf(Magic));
    if (CompareByte(Magic, 'Exif'#0#0, SizeOf(Magic)) <> 0) then
      Exit(False);
    FOffset:= Self.Seek(0, soCurrent);
    // Byte order
    case Self.ReadWord of
      $4949: FSwap:= {$IF DEFINED(ENDIAN_BIG)} True {$ELSE} False {$ENDIF}; // little-endian
      $4D4D: FSwap:= {$IF DEFINED(ENDIAN_LITTLE)} True {$ELSE} False {$ENDIF}; // big-endian
      else   Exit(False);
    end;
    // Magic word
    P:= Self.ReadWord;
    if (P <> $002A) and (P <> $2A00) then Exit(False);
    // Offset to first IFD
    Offset:= Self.ReadDWord;
    if FSwap then Offset:= SwapEndian(Offset);
    // Go to Image file directory
    Self.Seek(Offset - 8, soCurrent);
    Result:= DoImageFileDirectory;
  except
    Reset;
    Result:= False;
  end;
end;

end.

