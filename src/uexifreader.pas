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

  TExifReader = class
  private
    FOffset: Int64;
    FSwap: Boolean;
    FFile: TFileStream;
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

implementation

{ TExifReader }

procedure TExifReader.Reset;
begin
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
  AOffset:= FFile.Seek(0, soCurrent);
  FFile.Seek(Offset, soBeginning);
  SetLength(Result, Count);
  FFile.ReadBuffer(Result[1], Count);
  Result:= PAnsiChar(Result);
  FFile.Seek(AOffset, soBeginning);
end;

procedure TExifReader.ReadTag(var ATag: TTag);
begin
  FFile.ReadBuffer(ATag, SizeOf(TTag));
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
  ACount:= FFile.ReadWord;
  if FSwap then ACount:= SwapEndian(ACount);
  for I:= 1 to ACount do
  begin
    ReadTag(ATag);
    case ATag.ID of
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
    FFile.Seek(FOffset + AOffset, soBeginning);
    ACount:= FFile.ReadWord;
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
        $A002: FImageWidth := ATag.Offset;
        // Image pixel height
        $A003: FImageHeight := ATag.Offset;
      end;
    end;
  end;
end;

function TExifReader.LoadFromFile(const FileName: String): Boolean;
var
  P: UInt16;
  ASize: UInt16;
  Offset: UInt32;
  Magic: array [0..5] of AnsiChar;
begin
  try
    Reset;
    FFile:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      if (FFile.ReadByte <> $FF) then Exit(False);
      if (FFile.ReadByte <> $D8) then Exit(False);

      repeat
        if FFile.ReadByte = $FF then
        begin
          case FFile.ReadByte of
            $E1: // Exif Marker
              begin
                Break;
              end;
            $D9: // End Of Image (EOI)
              begin
                Exit(False);
              end;
            else begin // Unknown section, skip
              P:= FFile.ReadWordBE;
              FFile.Seek(Int64(P) - 2, soCurrent);
            end;
          end;
        end;
      until False;

      // Exif data size
      ASize:= FFile.ReadWordBE;
      // Exif magic string
      FFile.Read(Magic, SizeOf(Magic));
      if (CompareByte(Magic, 'Exif'#0#0, SizeOf(Magic)) <> 0) then
        Exit(False);
      FOffset:= FFile.Seek(0, soCurrent);
      // Byte order
      case FFile.ReadWord of
        $4949: FSwap:= {$IF DEFINED(ENDIAN_BIG)} True {$ELSE} False {$ENDIF}; // little-endian
        $4D4D: FSwap:= {$IF DEFINED(ENDIAN_LITTLE)} True {$ELSE} False {$ENDIF}; // big-endian
        else   Exit(False);
      end;
      // Magic word
      P:= FFile.ReadWord;
      if (P <> $002A) and (P <> $2A00) then Exit(False);
      // Offset to first IFD
      Offset:= FFile.ReadDWord;
      if FSwap then Offset:= SwapEndian(Offset);
      // Go to Image file directory
      FFile.Seek(Offset - 8, soCurrent);
      Result:= DoImageFileDirectory;
    finally
      FFile.Free;
    end;
  except
    Result:= False;
  end;
end;

end.

