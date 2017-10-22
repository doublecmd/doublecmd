unit ULZInWindow;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes;

type TLZInWindow=class
       public
         bufferBase: array of byte;// pointer to buffer with data
         stream:TStream;
         posLimit:integer; // offset (from _buffer) of first byte when new block reading must be done
         streamEndWasReached:boolean; // if (true) then _streamPos shows real end of stream

         pointerToLastSafePosition:integer;

         bufferOffset:integer;

         blockSize:integer;  // Size of Allocated memory block
         pos:integer;             // offset (from _buffer) of curent byte
         keepSizeBefore:integer;  // how many BYTEs must be kept in buffer before _pos
         keepSizeAfter:integer;   // how many BYTEs must be kept buffer after _pos
         streamPos:integer;   // offset (from _buffer) of first not read byte from Stream

         procedure MoveBlock;
         procedure ReadBlock;
         procedure _Free;
         procedure _Create(const keepSizeBefore, keepSizeAfter, keepSizeReserv:integer);virtual;
         procedure SetStream(const stream:TStream);
         procedure ReleaseStream;
         procedure Init;virtual;
         procedure MovePos;virtual;
         function GetIndexByte(const index:integer):byte;
         // index + limit have not to exceed _keepSizeAfter;
         function GetMatchLen(const index:integer;distance,limit:integer):integer;
         function GetNumAvailableBytes:integer;
         procedure ReduceOffsets(const subValue:integer);
       end;

implementation

procedure TLZInWindow.MoveBlock;
var offset,numbytes,i:integer;
begin
offset := bufferOffset + pos - keepSizeBefore;
// we need one additional byte, since MovePos moves on 1 byte.
if (offset > 0) then
   dec(offset);

numBytes := bufferOffset + streamPos - offset;

// check negative offset ????
for i := 0 to numBytes -1 do
    bufferBase[i] := bufferBase[offset + i];
bufferOffset := bufferOffset - offset;
end;

procedure TLZInWindow.ReadBlock;
var size,numreadbytes,pointerToPostion:integer;
begin
if streamEndWasReached then
   exit;
while (true) do begin
      size := (0 - bufferOffset) + blockSize - streamPos;
      if size = 0 then
         exit;
      numReadBytes := stream.Read(bufferBase[bufferOffset + streamPos], size);
      if (numReadBytes = 0) then begin
         posLimit := streamPos;
         pointerToPostion := bufferOffset + posLimit;
         if (pointerToPostion > pointerToLastSafePosition) then
            posLimit := pointerToLastSafePosition - bufferOffset;
         streamEndWasReached := true;
         exit;
         end;
      streamPos := streamPos + numReadBytes;
      if (streamPos >= pos + keepSizeAfter) then
         posLimit := streamPos - keepSizeAfter;
    end;
end;

procedure TLZInWindow._Free;
begin
setlength(bufferBase,0);
end;

procedure TLZInWindow._Create(const keepSizeBefore, keepSizeAfter, keepSizeReserv:integer);
var blocksize:integer;
begin
self.keepSizeBefore := keepSizeBefore;
self.keepSizeAfter := keepSizeAfter;
blockSize := keepSizeBefore + keepSizeAfter + keepSizeReserv;
if (length(bufferBase) = 0) or (self.blockSize <> blockSize) then begin
   _Free;
   self.blockSize := blockSize;
   setlength(bufferBase,self.blockSize);
   end;
pointerToLastSafePosition := self.blockSize - keepSizeAfter;
end;

procedure TLZInWindow.SetStream(const stream:TStream);
begin
self.stream:=stream;
end;

procedure TLZInWindow.ReleaseStream;
begin
stream:=nil;
end;

procedure TLZInWindow.Init;
begin
bufferOffset := 0;
pos := 0;
streamPos := 0;
streamEndWasReached := false;
ReadBlock;
end;

procedure TLZInWindow.MovePos;
var pointerToPostion:integer;
begin
inc(pos);
if pos > posLimit then begin
   pointerToPostion := bufferOffset + pos;
   if pointerToPostion > pointerToLastSafePosition then
      MoveBlock;
   ReadBlock;
   end;
end;

function TLZInWindow.GetIndexByte(const index:integer):byte;
begin
result:=bufferBase[bufferOffset + pos + index];
end;

function TLZInWindow.GetMatchLen(const index:integer;distance,limit:integer):integer;
var pby,i:integer;
begin
if streamEndWasReached then
   if (pos + index) + limit > streamPos then
      limit := streamPos - (pos + index);
inc(distance);
// Byte *pby = _buffer + (size_t)_pos + index;
pby := bufferOffset + pos + index;

i:=0;
while (i<limit)and(bufferBase[pby + i] = bufferBase[pby + i - distance]) do begin
      inc(i);
      end;
result:=i;
end;

function TLZInWindow.GetNumAvailableBytes:integer;
begin
result:=streamPos - pos;
end;

procedure TLZInWindow.ReduceOffsets(const subvalue:integer);
begin
bufferOffset := bufferOffset + subValue;
posLimit := posLimit - subValue;
pos := pos - subValue;
streamPos := streamPos - subValue;
end;

end.
