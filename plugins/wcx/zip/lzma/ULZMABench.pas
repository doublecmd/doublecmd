unit ULZMABench;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes,UCRC,ULZMACommon,windows,ULZMAEncoder,ULZMADecoder;

type TLZMABench=class
       public
         function GetLogSize(const size:integer):integer;
         function MyMultDiv64(const value, elapsedTime:int64):int64;
         function GetCompressRating(const dictionarySize:integer;const elapsedTime,size:int64):int64;
         function GetDecompressRating(const elapsedTime:int64;const outSize,inSize:int64):int64;
         function GetTotalRating(const dictionarySize:int64;const elapsedTimeEn, sizeEn, elapsedTimeDe, inSizeDe, outSizeDe:int64):int64;
         procedure PrintValue(const v:int64);
         procedure PrintRating(const rating:int64);
         procedure PrintResults(const dictionarySize:integer;const elapsedTime, size:int64;const decompressMode:boolean;const secondSize:int64);
         function LzmaBenchmark(const numIterations, dictionarySize:integer):integer;
       end;

     TLZMARandomGenerator=class
       public
         A1:integer;
         A2:integer;
         constructor Create;
         procedure Init;
         function GetRnd:integer;
       end;

     TLZMABenchBitRandomGenerator=class
       public
         RG:TLZMARandomGenerator;
         Value:integer;
         NumBits:integer;
         constructor Create;
         destructor Destroy;override;
         procedure Init;
         function GetRnd(numBits:integer):integer;
       end;

     TLZMABenchRandomGenerator=class
       public
         RG:TLZMABenchBitRandomGenerator;
         Pos:integer;
         Rep0:integer;
         Buffer:array of byte;
         BufferSize:integer;
         constructor Create;
         destructor Destroy;override;
         procedure _Set(const bufferSize:integer);
         function GetRndBit:integer;
         function GetLogRandBits(const numBits:integer):integer;
         function GetOffset:integer;
         function GetLen1:integer;
         function GetLen2:integer;
         procedure Generate;
       end;

     TCRCStream=class(TStream)
       public
         CRC:TCRC;
         constructor Create;
         destructor Destroy;override;
         procedure Init;
         function GetDigest:integer;
         function Write(const Buffer; Count: Longint): Longint;override;
       end;

     TByteArray=array of byte;
     PByteArray=^TByteArray;

     TMyOutputStream=class(TStream)
       public
         _buffer:PByteArray;
         _size:integer;
         _pos:integer;
         constructor Create(const buffer:PByteArray);
         procedure Reset;
         function Write(const Buffer; Count: Longint): Longint;override;
         function Size:integer;
       end;

     TMyInputStream=class(TStream)
       public
         _buffer:PByteArray;
         _size:integer;
         _pos:integer;
         constructor Create(const buffer:PByteArray;const size:integer);
         procedure Reset;
         function Read(var Buffer; Count: Longint): Longint;override;
       end;

     TLZMAProgressInfo=class
       public
         ApprovedStart:int64;
         InSize:int64;
         Time:cardinal;
         procedure Init;
         procedure OnProgress(const Action:TLZMAProgressAction;const Value:int64);
       end;


implementation

uses SysUtils;

const kAdditionalSize = (1 shl 21);
      kCompressedAdditionalSize = (1 shl 10);
      kSubBits = 8;

constructor TLZMARandomGenerator.Create;
begin
Init;
end;

procedure TLZMARandomGenerator.Init;
begin
A1 := 362436069;
A2 := 521288629;
end;

function TLZMARandomGenerator.GetRnd:integer;
begin
A1 := 36969 * (A1 and $ffff) + (A1 shr 16);
A2 := 18000 * (A2 and $ffff) + (A2 shr 16);
result:=(A1 shl 16) xor (A2);
end;

constructor TLZMABenchBitRandomGenerator.Create;
begin
RG:=TLZMARandomGenerator.Create;
end;

destructor TLZMABenchBitRandomGenerator.Destroy;
begin
RG.Free;
end;

procedure TLZMABenchBitRandomGenerator.Init;
begin
Value := 0;
NumBits := 0;
end;

function TLZMABenchBitRandomGenerator.GetRnd(numBits:integer):integer;
begin
if self.NumBits > numBits then begin
   result := Value and ((1 shl numBits) - 1);
   Value := Value shr numBits;
   self.NumBits := self.NumBits - numBits;
   exit;
   end;
numBits := numBits - self.NumBits;
result := (Value shl numBits);
Value := RG.GetRnd;
result := result or (Value and ((1 shl numBits) - 1));
Value := value shr numBits;
self.NumBits := 32 - numBits;
end;

constructor TLZMABenchRandomGenerator.Create;
begin
RG:=TLZMABenchBitRandomGenerator.Create;
end;

destructor TLZMABenchRandomGenerator.Destroy;
begin
RG.free;
end;

procedure TLZMABenchRandomGenerator._Set(const bufferSize:integer);
begin
setlength(Buffer,bufferSize);
Pos := 0;
self.BufferSize := bufferSize;
end;

function TLZMABenchRandomGenerator.GetLogRandBits(const numBits:integer):integer;
var len:integer;
begin
len := RG.GetRnd(numBits);
result:=RG.GetRnd(len);
end;

function TLZMABenchRandomGenerator.GetRndBit:integer;
begin
result:=RG.GetRnd(1);
end;

function TLZMABenchRandomGenerator.GetOffset:integer;
begin
if GetRndBit = 0 then
   result:=GetLogRandBits(4)
   else result:=(GetLogRandBits(4) shl 10) or RG.GetRnd(10);
end;

function TLZMABenchRandomGenerator.GetLen1:integer;
begin
result:=RG.GetRnd(1 + RG.GetRnd(2));
end;

function TLZMABenchRandomGenerator.GetLen2:integer;
begin
result:=RG.GetRnd(2 + RG.GetRnd(2));
end;

procedure TLZMABenchRandomGenerator.Generate;
var len,i:integer;
begin
RG.Init;
Rep0 := 1;
while Pos < BufferSize do begin
      if (GetRndBit = 0) or (Pos < 1) then begin
         Buffer[Pos] := RG.GetRnd(8);
         inc(pos);
         end else begin
             if RG.GetRnd(3) = 0 then
                len := 1 + GetLen1
                else begin
                     repeat
                       Rep0 := GetOffset;
                       until not (Rep0 >= Pos);
                     inc(Rep0);
                     len := 2 + GetLen2;
                     end;
             i:=0;
             while (i < len) and (Pos < BufferSize) do begin
                   Buffer[Pos] := Buffer[Pos - Rep0];
                   inc(i);
                   inc(pos);
                   end;
             end;
      end;
end;

constructor TCRCStream.Create;
begin
CRC:=TCRC.Create;
end;

destructor TCRCStream.Destroy;
begin
CRC.Free;
end;

procedure TCRCStream.Init;
begin
CRC.Init;
end;

function TCRCStream.GetDigest:integer;
begin
result:=CRC.GetDigest;
end;

function TCRCStream.Write(const Buffer; Count: Longint): Longint;
var p:^byte;
    i:integer;
begin
p:=@buffer;
for i:=0 to count -1 do begin
    CRC.UpdateByte(p^);
    inc(p);
    end;
result:=count;
end;

constructor TMyOutputStream.Create(const buffer:PByteArray);
begin
_buffer:=buffer;
_size:=length(buffer^);
end;

procedure TMyOutputStream.Reset;
begin
_pos:=0;
end;

function TMyOutputStream.Write(const Buffer; Count: Longint): Longint;
begin
if _pos+count>=_size then
   raise Exception.Create('Error');
move(buffer,_buffer^[_pos],count);
_pos:=_pos+count;
result:=count;
end;

function TMyOutputStream.Size:integer;
begin
result:=_pos;
end;

constructor TMyInputStream.Create(const buffer:PByteArray;const size:integer);
begin
_buffer:=buffer;
_size:=size;
end;

procedure TMyInputStream.Reset;
begin
_pos:=0;
end;

function TMyInputStream.Read(var Buffer; Count: Longint): Longint;
var b:int64;
begin
try
b:=_size-_pos;
if b>count then b:=count;
result:=b;
move(_buffer^[_pos],buffer,b);
_pos:=_pos+b;
except writeln('inread error');
end;
end;

procedure TLZMAProgressInfo.Init;
begin
InSize:=0;
end;

procedure TLZMAProgressInfo.OnProgress(const Action:TLZMAProgressAction;const Value:int64);
begin
if Action=LPAMax then exit;
if (value >= ApprovedStart) and (InSize = 0) then begin
   Time := GetTickCount;
   InSize := value;
   end;
end;

function TLZMABench.GetLogSize(const size:integer):integer;
var i,j:integer;
begin
for i := kSubBits to 31 do
    for j := 0 to 1 shl kSubBits -1 do
        if (size <= (1 shl i) + (j shl (i - kSubBits))) then begin
           result:=(i shl kSubBits) + j;
           exit;
           end;
result:=32 shl kSubBits;
end;

function TLZMABench.MyMultDiv64(const value, elapsedTime:int64):int64;
var freq,elTime:int64;
begin
freq := 1000; // ms
elTime := elapsedTime;
while freq > 1000000 do begin
      freq := freq shr 1;
      elTime :=elTime shr 1;
      end;
if elTime = 0 then
   elTime := 1;
result:=value * freq div elTime;
end;

function TLZMABench.GetCompressRating(const dictionarySize:integer;const elapsedTime,size:int64):int64;
var t,numCommandsForOne,numCommands:int64;
begin
t := GetLogSize(dictionarySize) - (18 shl kSubBits);
numCommandsForOne := 1060 + ((t * t * 10) shr (2 * kSubBits));
numCommands := size * numCommandsForOne;
result:=MyMultDiv64(numCommands, elapsedTime);
end;

function TLZMABench.GetDecompressRating(const elapsedTime:int64;const outSize,inSize:int64):int64;
var numCommands:int64;
begin
numCommands := inSize * 220 + outSize * 20;
result:=MyMultDiv64(numCommands, elapsedTime);
end;

function TLZMABench.GetTotalRating(const dictionarySize:int64;const elapsedTimeEn, sizeEn, elapsedTimeDe, inSizeDe, outSizeDe:int64):int64;
begin
result:=(GetCompressRating(dictionarySize, elapsedTimeEn, sizeEn) +
        GetDecompressRating(elapsedTimeDe, inSizeDe, outSizeDe)) div 2;
end;

procedure TLZMABench.PrintValue(const v:int64);
var s:string;
    i:integer;
begin
s:=inttostr(v);
i:=0;
while i+length(s)<6 do begin
      write(' ');
      inc(i);
      end;
write(s);
end;

procedure TLZMABench.PrintRating(const rating:int64);
begin
PrintValue(rating div 1000000);
write(' MIPS');
end;

procedure TLZMABench.PrintResults(const dictionarySize:integer;const elapsedTime, size:int64;const decompressMode:boolean;const secondSize:int64);
var speed:int64;
    rating:int64;
begin
speed := MyMultDiv64(size, elapsedTime);
PrintValue(speed div 1024);
write(' KB/s  ');
if decompressMode then
   rating := GetDecompressRating(elapsedTime, size, secondSize)
   else rating := GetCompressRating(dictionarySize, elapsedTime, size);
PrintRating(rating);
end;

function TLZMABench.LzmaBenchmark(const numIterations, dictionarySize:integer):integer;
var encoder:TLZMAEncoder;
    decoder:TLZMADecoder;
    kBufferSize,kCompressedBufferSize:integer;
    propStream:TMemoryStream;
    proparray:array of byte;
    rg:TLZMABenchRandomGenerator;
    crc:TCRC;
    progressInfo:TLZMAProgressInfo;
    totalBenchSize,totalEncodeTime,totalDecodeTime,totalCompressedSize:int64;
    inStream:TMyInputStream;
    compressedBuffer:array of byte;
    compressedStream:TMyOutputStream;
    CrcOutStream:TCRCStream;
    inputCompressedStream:TMyInputStream;
    compressedSize,i,j:integer;
    encodeTime,decodeTime:cardinal;
    outSize,startTime,benchSize:int64;
begin
if numIterations <= 0 then begin
   result:=0;
   exit;
   end;
if dictionarySize < (1 shl 18) then begin
   writeln(#10'Error: dictionary size for benchmark must be >= 18 (256 KB)');
   result:=1;
   exit;
   end;
write(#10'       Compressing                Decompressing'#10#10);

encoder := TLZMAEncoder.Create;
decoder := TLZMADecoder.Create;

if not encoder.SetDictionarySize(dictionarySize) then
   raise Exception.Create('Incorrect dictionary size');

kBufferSize := dictionarySize + kAdditionalSize;
kCompressedBufferSize := (kBufferSize div 2) + kCompressedAdditionalSize;

propstream:=TMemoryStream.Create;
encoder.WriteCoderProperties(propStream);
setlength(proparray,propstream.size);
propstream.Position:=0;
propstream.Read(propArray[0],propstream.Size);
decoder.SetDecoderProperties(propArray);

rg := TLZMABenchRandomGenerator.Create;

rg._Set(kBufferSize);
rg.Generate;
crc := TCRC.Create;
crc.Init;
crc.Update(rg.Buffer[0], 0, rg.BufferSize);

progressInfo := TLZMAProgressInfo.Create;
progressInfo.ApprovedStart := dictionarySize;

totalBenchSize := 0;
totalEncodeTime := 0;
totalDecodeTime := 0;
totalCompressedSize := 0;

inStream := TMyInputStream.Create(@(rg.Buffer), rg.BufferSize);

setlength(compressedBuffer,kCompressedBufferSize);
compressedStream := TMyOutputStream.Create(@compressedBuffer);
crcOutStream :=TCRCStream.Create;
inputCompressedStream := nil;
compressedSize := 0;
for i := 0 to numIterations -1 do begin
    progressInfo.Init;
    inStream.reset;
    compressedStream.reset;
    encoder.OnProgress:=progressInfo.OnProgress;
    encoder.Code(inStream, compressedStream, rg.BufferSize, -1);
    encodeTime := GetTickCount - progressInfo.Time;

    if i = 0 then begin
       compressedSize := compressedStream.size;
       inputCompressedStream := TMyInputStream.Create(@compressedBuffer, compressedSize);
       end else if compressedSize <> compressedStream.size then
           raise Exception.Create('Encoding error');

    if progressInfo.InSize = 0 then
       raise Exception.Create('Internal ERROR 1282');

    decodeTime := 0;
    for j := 0 to 1 do begin
        inputCompressedStream.reset;
        crcOutStream.Init;

        outSize := kBufferSize;
        startTime := GetTickCount;
        if not decoder.Code(inputCompressedStream, crcOutStream, outSize) then
           raise Exception.Create('Decoding Error');
        decodeTime := GetTickCount - startTime;
        if crcOutStream.GetDigest <> crc.GetDigest then
           raise Exception.Create('CRC Error');
        end;
    benchSize := kBufferSize - progressInfo.InSize;
    PrintResults(dictionarySize, encodeTime, benchSize, false, 0);
    write('     ');
    PrintResults(dictionarySize, decodeTime, kBufferSize, true, compressedSize);
    writeln('');

    totalBenchSize := totalBenchSize + benchSize;
    totalEncodeTime := totalEncodeTime + encodeTime;
    totalDecodeTime := totalDecodeTime + decodeTime;
    totalCompressedSize := totalCompressedSize + compressedSize;
    end;
writeln('---------------------------------------------------');
PrintResults(dictionarySize, totalEncodeTime, totalBenchSize, false, 0);
write('     ');
PrintResults(dictionarySize, totalDecodeTime, kBufferSize * numIterations, true, totalCompressedSize);
writeln('    Average');
result:=0;
end;

end.
