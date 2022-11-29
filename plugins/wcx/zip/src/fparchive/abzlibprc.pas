unit AbZlibPrc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZStream, AbDfBase;

type

  { TDeflateStream }

  TDeflateStream = class(TCompressionStream)
  private
    FSize: Int64;
    FHash: UInt32;
    FOnProgressStep: TAbProgressStep;
  public
    constructor Create(ALevel: Integer; ADest: TStream);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TInflateStream }

  TInflateStream = class(TDecompressionStream)
  private
    FHash: UInt32;
  public
    function CopyInto(ATarget: TStream; ACount: Int64): Int64;
    function Read(var Buffer; Count: LongInt): LongInt; override;
  end;

function Deflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;

function Inflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;

implementation

uses
  Math, ZDeflate, ZBase, DCcrc32;

function Deflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper): longint;
var
  ALevel: Integer;
  ADeflateStream: TDeflateStream;
begin
  case aHelper.PKZipOption of
    'n': ALevel:= 6;
    'x': ALevel:= 9;
    'f': ALevel:= 3;
    's': ALevel:= 1;
    else ALevel:= Z_DEFAULT_COMPRESSION;
  end;

  { if the helper's stream size <= 0, calculate
    the stream size from the stream itself }
  if (aHelper.StreamSize <= 0) then
    aHelper.StreamSize := aSource.Size;

  ADeflateStream:= TDeflateStream.Create(ALevel, aDest);
  try
    ADeflateStream.FSize:= aHelper.StreamSize;
    { attach progress notification method }
    ADeflateStream.FOnProgressStep:= aHelper.OnProgressStep;
    ADeflateStream.CopyFrom(aSource, aHelper.StreamSize);
    { save the uncompressed and compressed sizes }
    aHelper.NormalSize:= ADeflateStream.raw_written;
    aHelper.CompressedSize:= ADeflateStream.compressed_written;
    { provide encryption check value }
    Result := LongInt(ADeflateStream.FHash);
  finally
    ADeflateStream.Free;
  end;
end;

function Inflate(aSource: TStream; aDest: TStream;
                 aHelper: TAbDeflateHelper): longint;
var
  ACount: Int64;
  AInflateStream: TInflateStream;
begin
  AInflateStream:= TInflateStream.Create(aSource, True);
  try
    if aHelper.PartialSize > 0 then
    begin
      ACount:= aHelper.PartialSize;
      aHelper.NormalSize:= AInflateStream.CopyInto(aDest, ACount);
    end
    else begin
      ACount:= aHelper.NormalSize;
      aHelper.NormalSize:= aDest.CopyFrom(AInflateStream, ACount);
    end;
    aHelper.CompressedSize:= AInflateStream.compressed_read;
    Result:= LongInt(AInflateStream.FHash);
  finally
    AInflateStream.Free;
  end;
end;

{ TInflateStream }

function TInflateStream.CopyInto(ATarget: TStream; ACount: Int64): Int64;
var
  ARead, ASize: Integer;
  ABuffer: array of Byte;
begin
  Result:= 0;
  ASize:= Min(ACount, $8000);
  SetLength(ABuffer, ASize);
  repeat
    if ACount < ASize then
    begin
      ASize:= ACount;
    end;
    ARead:= Read(ABuffer[0], ASize);
    if ARead > 0 then
    begin
      Dec(ACount, ARead);
      Inc(Result, ARead);
      ATarget.WriteBuffer(ABuffer[0], ARead);
    end;
  until (ARead < ASize) or (ACount = 0);
end;

function TInflateStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result:= inherited Read(Buffer, Count);
  FHash:= crc32_16bytes(@Buffer, Result, FHash);
  if (Result < Count) and (Fstream.avail_in > 0) then
  begin
    FSource.Seek(-Fstream.avail_in, soCurrent);
    Fstream.avail_in:= 0;
  end;
end;

{ TDeflateStream }

constructor TDeflateStream.Create(ALevel: Integer;
  ADest: TStream);
const
  BUF_SIZE = 16384;
var
  AError: Integer;
begin
  TOwnerStream(Self).Create(ADest);

  Fbuffer:= GetMem(BUF_SIZE);
  Fstream.next_out:= Fbuffer;
  Fstream.avail_out:= BUF_SIZE;

  AError:= deflateInit2(Fstream, ALevel, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);

  if AError <> Z_OK then
    raise Ecompressionerror.Create(zerror(AError));
end;

function TDeflateStream.Write(const Buffer; Count: Longint): Longint;
begin
  FHash:= crc32_16bytes(@Buffer, Count, FHash);
  Result:= inherited Write(Buffer, Count);
  if Assigned(FOnProgressStep) then
  begin
    FOnProgressStep(raw_written * 100 div FSize);
  end;
end;

end.

