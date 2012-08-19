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
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbSWStm.pas                                 *}
{*********************************************************}
{* ABBREVIA: TabSlidingWindowStream class                *}
{*********************************************************}

unit AbSWStm;    
                 
{$I AbDefine.inc}
                 
{Notes: The TabSlidingWindowStream class provides a simple buffered
        stream for sliding window compression/decompression routines.
        The sliding window stream is limited when compared with a true
        buffered stream:
         - it is assumed that the underlying stream is just going to
           be written to and is initially empty
         - the buffer is fixed in size to 40KB
         - write operations can only occur at the end of the stream
         - the stream can only be positioned with a certain limited
           range 
         - we can only read up to 32KB
         - we can only write up to 32KB
        The stream is written as a wrapper around another stream
        (presumably a file stream) which is used for actual reads to
        the buffer and writes from the buffer.
        The stream buffer is organized as five 8KB chunks in an
        array. The last chunk is the only one used for writing, the
        other four are a 32KB buffer for reading. As the final chunk
        gets filled, the class will drop off the first chunk (writing
        it to the underlying stream, and shift the other chunks in the
        array.}  
                 
                 
{Define this if you wish to see a trace of the stream usage in a file
 called C:\SlideWin.LOG}
{.$DEFINE DebugTrace}
                 
interface        
                 
uses             
  SysUtils,      
  Classes;       
                 
const            
  abSWChunkCount = 5;
                 
type             
  TabSlidingWindowStream = class(TStream)
    protected {private}
      bsChunks      : array [0..pred(abSWChunkCount)] of PByteArray;
      bsBufferStart : longint;
      bsLastPos     : integer;
      bsCurChunk    : integer;
      bsPosInChunk  : integer;
      bsPosInBuffer : longint;
      bsSize      : Longint;    {count of bytes in stream}
      bsDirty     : boolean;    {whether the buffer is dirty or not}
      bsStream    : TStream;    {actual stream containing data}
      {$IFDEF DebugTrace}
      bsF : System.Text;
      {$ENDIF}   
    protected    
      procedure bsWriteChunk(aIndex : integer);
      procedure bsSlide;
                 
    public       
      constructor Create(aStream : TStream);
        {-create the buffered stream}
      destructor Destroy; override;
        {-destroy the buffered stream}
                 
      procedure Flush;
        {-ensures that all dirty buffered data is flushed}
      function Read(var Buffer; Count : Longint) : Longint; override;
        {-read from the stream into a buffer}
      function Seek(Offset : Longint; Origin : Word) : Longint; override;
        {-seek to a particular point in the stream}
      function Write(const Buffer; Count : Longint) : Longint; override;
        {-write to the stream from a buffer}
  end;           
                 
implementation   
                 
const
  ChunkSize = 8192; {cannot be greater than MaxInt}


{===Helper routines==================================================}
procedure RaiseException(const S : string);
begin            
  raise Exception.Create(S);
end;             
{====================================================================}
                 
                 
{===TabSlidingWindowStream===========================================}
constructor TabSlidingWindowStream.Create(aStream  : TStream);
var              
  i : integer;   
begin            
  inherited Create;
  {save the actual stream}
  bsStream := aStream;
  {allocate the chunks-they must be set to binary zeros}
  for i := 0 to pred(abSWChunkCount) do
    bsChunks[i] := AllocMem(ChunkSize);
  {set the page/buffer variables to the start of the stream; remember
   we only write to the last chunk--the previous chunks are set to
   binary zeros} 
  aStream.Position := 0;
  bsSize := 0;   
  bsBufferStart := -ChunkSize * pred(abSWChunkCount);
  bsPosInBuffer := ChunkSize * pred(abSWChunkCount);
  bsCurChunk := pred(abSWChunkCount);
  bsPosInChunk := 0;
  bsDirty := false;
  {$IFDEF DebugTrace}
  System.Assign(bsF, 'c:\SlideWin.LOG');
  if FileExists('c:\SlideWin.LOG') then
    System.Append(bsF)
  else           
    System.Rewrite(bsF);
  writeln(bsF, '---NEW LOG---');
  {$ENDIF}       
end;             
{--------}       
destructor TabSlidingWindowStream.Destroy;
var              
  i : integer;   
begin            
  {destroy the buffer, after writing it to the actual stream}
  if bsDirty then
    Flush;       
  for i := 0 to pred(abSWChunkCount) do
    if (bsChunks[i] <> nil) then
      FreeMem(bsChunks[i], ChunkSize);
  {$IFDEF DebugTrace}
  System.Close(bsF);
  {$ENDIF}       
  {let our ancestor clean up}
  inherited Destroy;
end;             
{--------}       
procedure TabSlidingWindowStream.bsSlide;
var              
  SavePtr : PByteArray;
  i       : integer;
begin            
  {write out the first chunk}
  bsWriteChunk(0);
  {slide the chunks around}
  SavePtr := bsChunks[0];
  for i := 0 to abSWChunkCount-2 do
    bsChunks[i] := bsChunks[i+1];
  bsChunks[pred(abSWChunkCount)] := SavePtr;
  {advance the buffer start position}
  inc(bsBufferStart, ChunkSize);
  {reset the write position}
  bsPosInChunk := 0;
  bsPosInBuffer := ChunkSize * pred(abSWChunkCount);
  bsLastPos := 0;
end;             
{--------}       
procedure TabSlidingWindowStream.bsWriteChunk(aIndex : integer);
var              
  SeekResult : longint;
  BytesWrit  : longint;
  Offset     : longint;
  BytesToWrite : integer;
begin            
  Offset := bsBufferStart + (longint(aIndex) * ChunkSize);
  if (Offset >= 0) then begin
    SeekResult := bsStream.Seek(Offset, 0);
    if (SeekResult = -1) then
      RaiseException('TabSlidingWindowStream.bsWriteChunk: seek failed');
    if (aIndex <> pred(abSWChunkCount)) then
      BytesToWrite := ChunkSize
    else         
      BytesToWrite := bsLastPos;
    BytesWrit := bsStream.Write(bsChunks[aIndex]^, BytesToWrite);
    if (BytesWrit <> BytesToWrite) then
      RaiseException('TabSlidingWindowStream.bsWriteChunk: write failed');
  end;           
end;             
{--------}       
procedure TabSlidingWindowStream.Flush;
var              
  i : integer;   
begin            
  if bsDirty then begin
    for i := 0 to pred(abSWChunkCount) do
      bsWriteChunk(i);
    bsDirty := false;
  end;           
end;             
{--------}       
function TabSlidingWindowStream.Read(var Buffer; Count : Longint) : Longint;
var              
  BufPtr      : PByte;
  BytesToGo   : Longint;
  BytesToRead : integer;
begin
  BufPtr := @Buffer;

  {$IFDEF DebugTrace}
  System.Writeln(bsF, 'Read:  ', Count, ' bytes');
  {$ENDIF}       
  {we do not support reads greater than 32KB bytes}
  if (Count > 32*1024) then
    Count := 32*1024;
                 
  {reading is complicated by the fact we can only read in chunks of
   ChunkSize: we need to partition out the overall read into a
   read from part of the chunk, zero or more reads from complete
   chunks and then a possible read from part of a chunk}
                 
  {calculate the actual number of bytes we can read - this depends on
   the current position and size of the stream as well as the number
   of bytes requested}
  BytesToGo := Count;
  if (bsSize < (bsBufferStart + bsPosInBuffer + Count)) then
    BytesToGo := bsSize - (bsBufferStart + bsPosInBuffer);
  if (BytesToGo <= 0) then begin
    Result := 0; 
    Exit;        
  end;           
  {remember to return the result of our calculation}
  Result := BytesToGo;
                 
  {calculate the number of bytes we can read prior to the loop}
  BytesToRead := ChunkSize - bsPosInChunk;
  if (BytesToRead > BytesToGo) then
    BytesToRead := BytesToGo;
  {copy from the stream buffer to the caller's buffer}
  if (BytesToRead = 1) then
    BufPtr^ := bsChunks[bsCurChunk]^[bsPosInChunk]
  else           
    Move(bsChunks[bsCurChunk]^[bsPosInChunk], BufPtr^, BytesToRead);
  {calculate the number of bytes still to read}
  dec(BytesToGo, BytesToRead);
                 
  {while we have bytes to read, read them}
  while (BytesToGo > 0) do begin
    {advance the pointer for the caller's buffer}
    inc(BufPtr, BytesToRead);
    {as we've exhausted this chunk, advance to the next}
    inc(bsCurChunk);
    bsPosInChunk := 0;
    {calculate the number of bytes we can read in this cycle}
    BytesToRead := ChunkSize;
    if (BytesToRead > BytesToGo) then
      BytesToRead := BytesToGo;
    {copy from the stream buffer to the caller's buffer}
    Move(bsChunks[bsCurChunk]^, BufPtr^, BytesToRead);
    {calculate the number of bytes still to read}
    dec(BytesToGo, BytesToRead);
  end;           
  {remember our new position}
  inc(bsPosInChunk, BytesToRead);
end;             
{--------}       
function TabSlidingWindowStream.Seek(Offset : Longint;
                                     Origin : Word) : Longint;
{$IFDEF DebugTrace}
const            
  OriginStr : array [0..2] of string[7] = ('start', 'current', 'end');
{$ENDIF}         
var              
  NewPos       : Longint;
begin            
  {$IFDEF DebugTrace}
  System.Writeln(bsF, 'Seek:  ', Offset, ' bytes from ', OriginStr[Origin]);
  {$ENDIF}       
  {calculate the new position}
  case Origin of 
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := bsBufferStart + bsPosInBuffer + Offset;
    soFromEnd       : NewPos := bsSize + Offset;
  else           
    NewPos := 0; 
    RaiseException('TabSlidingWindowStream.Seek: invalid origin');
  end;           
  {if the new position is invalid, say so}
  if (NewPos < bsBufferStart) or (NewPos > bsSize) then
    RaiseException('TabSlidingWindowStream.Seek: invalid new position');
  {calculate the chunk number and the position in buffer & chunk}
  bsPosInBuffer := NewPos - bsBufferStart;
  bsCurChunk := bsPosInBuffer div ChunkSize;
  bsPosInChunk := bsPosInBuffer mod ChunkSize;
  {return the new position}
  Result := NewPos;
end;             
{--------}       
function TabSlidingWindowStream.Write(const Buffer; Count : Longint) : Longint;
var              
  BufPtr      : PByte;
  BytesToGo   : Longint;
  BytesToWrite: integer;
begin
  BufPtr := @Buffer;

  {$IFDEF DebugTrace}
  System.Writeln(bsF, 'Write: ', Count, ' bytes');
  {$ENDIF}       
  {we ONLY write at the end of the stream}
  if ((bsBufferStart + bsPosInBuffer) <> bsSize) then
    RaiseException('TabSlidingWindowStream.Write: Not at end of stream');
                 
  {we do not support writes greater than 32KB bytes}
  if (Count > 32*1024) then
    Count := 32*1024;
                 
  {writing is complicated by the fact we write in chunks of Chunksize
   bytes: we need to partition out the overall write into a write
   to part of the chunk, zero or more writes to complete chunks and
   then a possible write to part of a chunk; every time we fill a
   chunk we have toi slide the buffer}
                 
  {when we write to this stream we always assume that we can write the
   requested number of bytes: if we can't (eg, the disk is full) we'll
   get an exception somewhere eventually}
  BytesToGo := Count;
  {remember to return the result of our calculation}
  Result := BytesToGo;
                 
  {calculate the number of bytes we can write prior to the loop}
  BytesToWrite := ChunkSize - bsPosInChunk;
  if (BytesToWrite > BytesToGo) then
    BytesToWrite := BytesToGo;
  {copy from the caller's buffer to the stream buffer}
  if (BytesToWrite = 1) then
    bsChunks[pred(abSWChunkCount)]^[bsPosInChunk] := BufPtr^
  else           
    Move(BufPtr^,
         bsChunks[pred(abSWChunkCount)]^[bsPosInChunk],
         BytesToWrite);
  {mark our buffer as requiring a save to the actual stream}
  bsDirty := true;
  {calculate the number of bytes still to write}
  dec(BytesToGo, BytesToWrite);
                 
  {while we have bytes to write, write them}
  while (BytesToGo > 0) do begin
    {slide the buffer}
    bsSlide;     
    {advance the pointer for the caller's buffer}
    inc(BufPtr, BytesToWrite);
    {calculate the number of bytes we can write in this cycle}
    BytesToWrite := ChunkSize;
    if (BytesToWrite > BytesToGo) then
      BytesToWrite := BytesToGo;
    {copy from the caller's buffer to our buffer}
    Move(BufPtr^,
         bsChunks[pred(abSWChunkCount)]^,
         BytesToWrite);
    {calculate the number of bytes still to write}
    dec(BytesToGo, BytesToWrite);
  end;           
  {remember our new position}
  inc(bsPosInChunk, BytesToWrite);
  bsPosInBuffer := (longint(ChunkSize) * pred(abSWChunkCount)) + bsPosInChunk;
  bsLastPos := bsPosInChunk;
  {make sure the stream size is correct}
  inc(bsSize, Result);
  {if we're at the end of the chunk, slide the buffer ready for next
   time we write}
  if (bsPosInChunk = ChunkSize) then
    bsSlide;     
end;             
{====================================================================}
                 
end.             
