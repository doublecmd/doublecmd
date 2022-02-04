unit Base64Buf;

{$mode delphi}

interface

uses
  Classes, SysUtils, Base64, BufStream;

type

  { TWriteBufStreamEx }

  TWriteBufStreamEx = class(TWriteBufStream)
  private
    FPosition: Int64;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TBase64EncodingStreamEx }

  TBase64EncodingStreamEx = class(TBase64EncodingStream)
  private
    FStream: TStream;
  public
    constructor Create(ASource : TStream); reintroduce;
    destructor Destroy; override;
  end;

  { TBase64DecodingStreamEx }

  TBase64DecodingStreamEx = class(TBase64DecodingStream)
  private
    FStream: TStream;
  public
    constructor Create(ASource : TStream); reintroduce; overload;
    constructor Create(ASource: TStream; AMode: TBase64DecodingMode); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

const
  BUFFER_SIZE = 32768;

{ TWriteBufStreamEx }

function TWriteBufStreamEx.Write(const Buffer; Count: Longint): Longint;
const
  LINE_LENGTH = 80;
  EOL = String(LineEnding);
begin
  if (FPosition + Count) > LINE_LENGTH then
  begin
    FPosition:= 0;
    inherited Write(EOL[1], Length(EOL));
  end;
  Inc(FPosition, Count);
  Result:= inherited Write(Buffer, Count);
end;

{ TBase64DecodingStreamEx }

constructor TBase64DecodingStreamEx.Create(ASource: TStream);
begin
  FStream:= TReadBufStream.Create(ASource, BUFFER_SIZE);
  inherited Create(FStream);
end;

constructor TBase64DecodingStreamEx.Create(ASource: TStream;
  AMode: TBase64DecodingMode);
begin
  Create(ASource);
  Mode:= AMode;
end;

destructor TBase64DecodingStreamEx.Destroy;
begin
  inherited Destroy;
  FStream.Free;
end;

{ TBase64EncodingStreamEx }

constructor TBase64EncodingStreamEx.Create(ASource: TStream);
begin
  FStream:= TWriteBufStreamEx.Create(ASource, BUFFER_SIZE);
  inherited Create(FStream);
end;

destructor TBase64EncodingStreamEx.Destroy;
begin
  inherited Destroy;
  FStream.Free;
end;

end.

