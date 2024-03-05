unit AbGz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbZlibPrc;

type

  { TGzDecompressionStream }

  TGzDecompressionStream = class(TInflateStream)
  protected
    function ReadNextHeader(ASource: TStream): Boolean;
  public
    constructor Create(ASource: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  EGzDecompressionError = class(EStreamError);

implementation

uses
  RtlConsts, ZInflate;

const
  { Header signature }
  GZ_HDR_ID1 = $1F;
  GZ_HDR_ID2 = $8B;

const
  GZ_HEAD_CRC    = $02; { bit 1 set: header CRC present }
  GZ_EXTRA_FIELD = $04; { bit 2 set: extra field present }
  GZ_ORIG_NAME   = $08; { bit 3 set: original file name present }
  GZ_COMMENT     = $10; { bit 4 set: file comment present }
  GZ_RESERVE     = $E0; { bit 5-7  : reserved }

type
  TGzHeader = packed record
    ID1        : Byte;
    ID2        : Byte;
    Method     : Byte;
    Flags      : Byte;
    ModTime    : Int32;
    XtraFlags  : Byte;
    OS         : Byte;
  end;

  TGzFooter = packed record
    case Integer of
    0: (Bytes : array[0..7] of Byte);
    1: (CRC32: LongInt; Size: LongWord);
  end;

{ TGzDecompressionStream }

function TGzDecompressionStream.ReadNextHeader(ASource: TStream): Boolean;
var
  ALength: Integer;
  AHeader: TGzHeader;
begin
  Result:= ASource.Read(AHeader, SizeOf(TGzHeader)) = SizeOf(TGzHeader);
  if Result then
  begin
    if (AHeader.ID1 <> GZ_HDR_ID1) or (AHeader.ID2 <> GZ_HDR_ID2) or (AHeader.Method <> 8) then
    begin
      raise EGzDecompressionError.Create(EmptyStr);
    end;
    if (AHeader.Flags and GZ_RESERVE <> 0) then
    begin
      raise EGzDecompressionError.Create(EmptyStr);
    end;
    // Skip the extra field
    if (AHeader.Flags and GZ_EXTRA_FIELD <> 0) then
    begin
      ALength:= ASource.ReadWord;
      while ALength > 0 do
      begin
        ASource.ReadByte;
        Dec(ALength);
      end;
    end;
    // Skip the original file name
    if (AHeader.Flags and GZ_ORIG_NAME <> 0) then
    begin
      while (ASource.ReadByte > 0) do;
    end;
    // Skip the .gz file comment
    if (AHeader.Flags and GZ_COMMENT <> 0) then
    begin
      while (ASource.ReadByte > 0) do;
    end;
    // Skip the header crc
    if (AHeader.Flags and GZ_HEAD_CRC <> 0) then
    begin
      ASource.ReadWord;
    end;
  end;
end;

constructor TGzDecompressionStream.Create(ASource: TStream);
begin
  ReadNextHeader(ASource);
  inherited Create(ASource, True);
end;

function TGzDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  AFooter: TGzFooter;
  total_in, total_out: QWord;
begin
  Result:= inherited Read(Buffer, Count);

  while Result < Count do
  begin
    FSource.ReadBuffer(AFooter, SizeOf(TGzFooter));

    // Try concatenated streams
    if not ReadNextHeader(FSource) then Break;

    total_in := Fstream.total_in;
    total_out := Fstream.total_out;

    inflateReset(Fstream);

    Fstream.total_in:= total_in;
    Fstream.total_out:= total_out;

    Result+= inherited Read(PByte(@Buffer)[Result], Count - Result);
  end;
end;

function TGzDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset >= 0) and (Origin = soCurrent) then
  begin
    if (Offset > 0) then Discard(Offset);
    Result:= Fstream.total_out;
  end
  else if (Origin = soBeginning) and (Fstream.total_out = Offset) then
    Result:= Offset
  else begin
    raise EGzDecompressionError.CreateFmt(SStreamInvalidSeek, [ClassName]);
  end;
end;

end.

