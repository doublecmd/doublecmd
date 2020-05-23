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
    FHash: LongInt;
    FOnProgressStep: TAbProgressStep;
  public
    constructor Create(ALevel: Integer; ADest: TStream);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

function Deflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;

implementation

uses
  ZDeflate, ZBase;

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
    Result := not ADeflateStream.FHash;
  finally
    ADeflateStream.Free;
  end;
end;

{ TDeflateStream }

constructor TDeflateStream.Create(ALevel: Integer;
  ADest: TStream);
var
  AError: Integer;
begin
  FHash := -1;
  TCustomZlibStream(Self).Create(ADest);

  Fstream.next_out:= Fbuffer;
  Fstream.avail_out:= MemSize(Fbuffer);

  AError:= deflateInit2(Fstream, ALevel, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);

  if AError <> Z_OK then
    raise Ecompressionerror.Create(zerror(AError));
end;

function TDeflateStream.Write(const Buffer; Count: Longint): Longint;
begin
  AbUpdateCRCBuffer(FHash, (@Buffer)^, Count);
  Result:= inherited Write(Buffer, Count);
  if Assigned(FOnProgressStep) then
  begin
    FOnProgressStep(raw_written * 100 div FSize);
  end;
end;

end.

