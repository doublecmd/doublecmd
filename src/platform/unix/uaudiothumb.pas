unit uAudioThumb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  Graphics, Types, DCClassesUtf8, uThumbnails, uMasks, uGraphics;

var
  MaskList: TMaskList = nil;

type
  TFrameHeader = packed record
    ID: array [1..4] of AnsiChar;
    Size: Integer;
    Flags: UInt16;
  end;

  TTagHeader = packed record
    ID: array [1..3] of AnsiChar;
    Version: Byte;
    Revision: Byte;
    Flags: Byte;
    Size: array [1..4] of Byte;
  end;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
var
  AInc: Byte;
  FSize: Int64;
  AData: UInt16;
  Index: Integer;
  AChar: AnsiChar;
  ALength: Integer;
  ATag: TTagHeader;
  AFile: TFileStreamEx;
  AFrame: TFrameHeader;
  ABitmap: TRasterImage;
  AStream: TMemoryStream;
  AMimeType: array[Byte] of AnsiChar;
begin
  Result:= nil;

  if MaskList.Matches(aFileName) then
  begin
    try
      AFile:= TFileStreamEx.Create(aFileName, fmOpenRead or fmShareDenyNone);
      try
        AFile.ReadBuffer(ATag, SizeOf(TTagHeader));
        if (ATag.ID = 'ID3') and (ATag.Version >= 3) then
        begin
          FSize := (ATag.Size[1] shl 21) or (ATag.Size[2] shl 14) +
                   (ATag.Size[3] shl 7 ) or (ATag.Size[4]) + 10;
          if ATag.Flags and $10 = $10 then Inc(FSize, 10);

          while (AFile.Position < FSize) do
          begin
            AFile.ReadBuffer(AFrame, SizeOf(TFrameHeader));
            if not (AFrame.ID[1] in ['A'..'Z']) then Break;

            ALength:= BEtoN(AFrame.Size);
            if (AFrame.ID = 'APIC') then
            begin
              AStream:= TMemoryStream.Create;
              try
                AStream.SetSize(ALength);
                AFile.ReadBuffer(AStream.Memory^, ALength);
                // Text encoding
                case AStream.ReadByte of
                  $01, $02: AInc:= 2;
                  else      AInc:= 1;
                end;
                // MIME type
                Index:= 0;
                repeat
                  AChar:= Chr(AStream.ReadByte);
                  AMimeType[Index]:= AChar;
                  Inc(Index);
                until not ((AChar > #0) and (Index < High(Byte)));
                // Picture type
                AStream.ReadByte;
                // Description
                repeat
                  AStream.ReadBuffer(AData, AInc);
                until (AData = 0);
                // Picture data
                if (StrPos(AMimeType, 'image/') = nil) then
                begin
                  AMimeType := 'image/' + AMimeType;
                end;
                if AMimeType = 'image/png' then
                begin
                  ABitmap:= TPortableNetworkGraphic.Create;
                end
                else if AMimeType = 'image/jpeg' then
                begin
                  ABitmap:= TJPEGImage.Create;
                end;
                if Assigned(ABitmap) then
                try
                  ABitmap.LoadFromStream(AStream, ALength - AStream.Position);
                  Result:= TBitmap.Create;
                  BitmapAssign(Result, ABitmap);
                finally
                  ABitmap.Free;
                end;
              finally
                AStream.Free;
              end;
              Break;
            end;
            AFile.Seek(ALength, soCurrent);
          end;
        end;
      finally
        AFile.Free;
      end;
    except
      // Ignore
    end;
  end;
end;

procedure Initialize;
begin
  MaskList:= TMaskList.Create('*.mp3');
  // Register thumbnail provider
  TThumbnailManager.RegisterProvider(@GetThumbnail);
end;

initialization
  Initialize;

finalization
  MaskList.Free;

end.

