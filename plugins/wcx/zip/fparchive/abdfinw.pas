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
{* ABBREVIA: AbDfInW.pas                                 *}
{*********************************************************}
{* Deflate input sliding window unit                     *}
{*********************************************************}

unit AbDfInW;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbDfBase;

{Notes: TdfInputWindow implements a sliding window on data for the
        LZ77 dictionary encoding.

        The stream passed to the class is automatically read when
        required to keep the internal buffer fully loaded.
        }

type
  TAbDfMatch = record
    maLen  : integer;
    maDist : integer;
    maLit  : AnsiChar;
  end;

type
  PAbPointerList = ^TAbPointerList;
  TAbPointerList = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  TAbDfInputWindow = class
    private
      FAdvanceStart : boolean;
      FBuffer       : PAnsiChar;
      FBufferEnd    : PAnsiChar;
      FBytesUsed    : longint;
      FChainLen     : integer;
      FHashChains   : PAbPointerList;
      FHashHeads    : PAbPointerList;
      FHashIndex    : integer;
      FChecksum     : longint;
      FCurrent      : PAnsiChar;
      FLookAheadEnd : PAnsiChar;
      FMaxMatchLen  : integer;
      FMustSlide    : boolean;
      FOnProgress   : TAbProgressStep;
      FSlidePoint   : PAnsiChar;
      FStart        : PAnsiChar;
      FStartOffset  : longint;
      FStream       : TStream;
      FStreamSize   : longint;
      FUseCRC32     : boolean;
      FUseDeflate64 : boolean;
      FWinMask      : integer;
      FWinSize      : integer;
    protected
      function iwGetChecksum : longint;
      procedure iwReadFromStream;
      procedure iwSetCapacity(aValue : longint);
      procedure iwSlide;
    public
      constructor Create(aStream       : TStream;
                         aStreamSize   : longint;
                         aWinSize      : integer;
                         aChainLength  : integer;
                         aUseDeflate64 : boolean;
                         aUseCRC32     : boolean);
      destructor Destroy; override;

      procedure Advance(aCount     : integer;
                        aHashCount : integer);
      procedure AdvanceByOne;
      function FindLongestMatch(aAmpleLength : integer;
                            var aMatch       : TAbDfMatch;
                          const aPrevMatch   : TAbDfMatch) : boolean;
      function GetNextChar : AnsiChar;
      function GetNextKeyLength : integer;
      function Position : longint;
      procedure ReadBuffer(var aBuffer; aCount  : longint;
                                        aOffset : Int64);

      property ChainLen : integer read FChainLen write FChainLen;
      property Checksum : longint read iwGetChecksum;
      property OnProgress : TAbProgressStep
                  read FOnProgress write FOnProgress;
  end;

implementation

uses
  SysUtils;

{Notes:
        Meaning of the internal pointers:

        |----------+===================+==+--------------------------|
        |          |                   |  |                          |
        FBuffer    FStart       FCurrent  FLookAheadEnd     FBufferEnd

        FCurrent is the current match position. The valid data that
        can be matched is between FStart and FLookAheadEnd, The data
        between FStart and FCurrent has already been seen; the data
        between FCurrent and FLookAheadEnd can be used for matching.

        The buffer size depends on the requested window size (a
        multiple of 1KB, up to 32KB for deflate, up to 64KB for
        deflate64) and the lookahead size (up to 258 bytes for deflate
        and 64KB for deflate64.)

        The window of data continuously slides to the right, and is
        slid back to FBuffer whenever FStart reaches a point 16KB
        away, this point being given by FSlidePoint.


        The hash table:
        This is a chained hash table with some peculiarities. First
        the table itself, FHashHeads. It contains pointers to strings
        in the window buffer, not to chains. The chains are held is a
        separate structure, FHashChains. The hash function on the
        three-character keys is a Rabin-Karp function:
          ((((Ch1 shl 5) xor Ch2) shl 5) xor Ch3) and $3FFF
        designed so that a running hash value can be kept and
        calculated per character. The hash table is $4000 elements
        long (obviously, given the hash function).
        On insertion, the previous pointer in the hash table at the
        calculated index is saved and replaced by the new pointer. The
        old pointer is saved in the chains array. This has the same
        number of elements as the sliding window has characters. The
        pointer is placed at (Ptr and (WindowsSize-1)) overwriting the
        value that's already there. In this fashion the individual
        chains in the standard hash table are interwoven with each
        other in this hash table, like a skein of threads.
        }

const
  c_HashCount = $4000;             {the number of hash entries}
  c_HashMask  = c_HashCount - 1;   {a mask for the hash function}
  c_HashShift = 5;                 {shift value for the hash function}

{===TAbDfInputWindow=================================================}
constructor TAbDfInputWindow.Create(aStream       : TStream;
                                    aStreamSize   : longint;
                                    aWinSize      : integer;
                                    aChainLength  : integer;
                                    aUseDeflate64 : boolean;
                                    aUseCRC32     : boolean);
begin
  {create the ancestor}
  inherited Create;

  {save parameters}
  FStreamSize := aStreamSize;
  FWinSize := aWinSize;
  FWinMask := aWinSize - 1;
  FStream := aStream;
  FChainLen := aChainLength;
  FUseDeflate64 := aUseDeflate64;
  FUseCRC32 := aUseCRC32;
  if aUseCRC32 then
    FChecksum := -1  { CRC32 starts off with all bits set }
  else
    FCheckSum := 1;  { Adler32 starts off with a value of 1 }

  {set capacity of sliding window}
  iwSetCapacity(aWinSize);

  {create the hash table, first the hash table itself (and set all
   entries to nil)}
  FHashHeads := AllocMem(c_HashCount * sizeof(pointer));

  {..now the chains (there's no need to set the entries to nil, since
   the chain entries get fed from the head entries before searching)}
  GetMem(FHashChains, aWinSize * sizeof(pointer));

  {read the first chunk of data from the stream}
  FMustSlide := true;
  iwReadFromStream;

  {if there are at least two bytes, prime the hash index}
  if ((FLookAheadEnd - FBuffer) >= 2) then 
    FHashIndex := ((longint(FBuffer[0]) shl c_HashShift) xor
                   longint(FBuffer[1])) and
                  c_HashMask;
end;
{--------}
destructor TAbDfInputWindow.Destroy;
begin
  {free the hash table}
  FreeMem(FHashHeads);
  FreeMem(FHashChains);

  {free the buffer}
  FreeMem(FBuffer);

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TAbDfInputWindow.Advance(aCount     : integer;
                                   aHashCount : integer);
var
  i : integer;
  ByteCount : integer;
  Percent   : integer;
  HashChains: PAbPointerList;
  HashHeads : PAbPointerList;
  HashInx   : integer;
  CurPos    : PAnsiChar;
begin
  Assert((FLookAheadEnd - FCurrent) >= aCount,
         'TAbDfInputWindow.Advance: seem to be advancing into the unknown');
  Assert((aHashCount = aCount) or (aHashCount = pred(aCount)),
         'TAbDfInputWindow.Advance: the parameters are plain wrong');
  {use local var for speed}
  CurPos := FCurrent;

  {advance the current pointer if needed}
  if (aCount > aHashCount) then
    inc(CurPos);

  {make sure we update the hash table; remember that the string[3] at
   the current position has already been added to the hash table (for
   notes on updating the hash table, see FindLongestMatch}

  {use local vars for speed}
  HashChains := FHashChains;
  HashHeads := FHashHeads;
  HashInx := FHashIndex;

  {update the hash table}
  for i := 0 to pred(aHashCount) do begin
    HashInx :=
       ((HashInx shl c_HashShift) xor longint(CurPos[2])) and
       c_HashMask;
    HashChains^[PtrUInt(CurPos) and FWinMask] := HashHeads^[HashInx];
    HashHeads^[HashInx] := CurPos;
    inc(CurPos);
  end;

  {replace old values}
  FHashChains := HashChains;
  FHashHeads := HashHeads;
  FHashIndex := HashInx;
  FCurrent := CurPos;

  {if we've seen at least FWinSize bytes...}
  if FAdvanceStart then begin

    {advance the start of the sliding window}
    inc(FStart, aCount);
    inc(FStartOffset, aCount);

    {check to see if we have advanced into the slide zone}
    if FMustSlide and (FStart >= FSlidePoint) then
      iwSlide;
  end

  {otherwise check to see if we've seen at least FWinSize bytes}
  else if ((CurPos - FStart) >= FWinSize) then begin
    FAdvanceStart := true;
    {note: we can't advance automatically aCount bytes here, we need
           to calculate the actual count}
    ByteCount := (CurPos - FWinSize) - FStart;
    inc(FStart, ByteCount);
    inc(FStartOffset, ByteCount);
  end;

  {show progress}
  if Assigned(FOnProgress) then begin
    inc(FBytesUsed, aCount);
    if ((FBytesUsed and $FFF) = 0) then begin
      Percent := Round((100.0 * FBytesUsed) / FStreamSize);
      FOnProgress(Percent);
    end;
  end;

  {check to see if we have advanced into the slide zone}
  if (FStart >= FSlidePoint) then
    iwSlide;
end;
{--------}
procedure TAbDfInputWindow.AdvanceByOne;
var
  Percent   : integer;
begin
  {advance the current pointer}
  inc(FCurrent);

  {if we've seen at least FWinSize bytes...}
  if FAdvanceStart then begin

    {advance the start of the sliding window}
    inc(FStart, 1);
    inc(FStartOffset, 1);

    {check to see if we have advanced into the slide zone}
    if FMustSlide and (FStart >= FSlidePoint) then
      iwSlide;
  end

  {otherwise check to see if we've seen FWinSize bytes}
  else if ((FCurrent - FStart) = FWinSize) then
    FAdvanceStart := true;

  {show progress}
  if Assigned(FOnProgress) then begin
    inc(FBytesUsed, 1);
    if ((FBytesUsed and $FFF) = 0) then begin
      Percent := Round((100.0 * FBytesUsed) / FStreamSize);
      FOnProgress(Percent);
    end;
  end;
end;
{--------}
function TAbDfInputWindow.FindLongestMatch(aAmpleLength : integer;
                                       var aMatch       : TAbDfMatch;
                                     const aPrevMatch   : TAbDfMatch)
                                                        : boolean;
{Note: this routine implements a greedy algorithm and is by far the
       time sink for compression. There are two versions, one written
       in Pascal for understanding, one in assembler for speed.
       Activate one and only one of the following compiler defines.}
{$IFDEF CPU386}
  {$DEFINE UseGreedyAsm}
{$ELSE}
  {$DEFINE UseGreedyPascal}
{$ENDIF}

{Check to see that all is correct}
{$IFDEF UseGreedyAsm}
  {$IFDEF UseGreedyPascal}
    !! Compile Error: only one of the greedy compiler defines can be used
  {$ENDIF}
{$ELSE}
  {$IFNDEF UseGreedyPascal}
    !! Compile Error: one of the greedy compiler defines must be used
  {$ENDIF}
{$ENDIF}
type
  PWord    = ^word;
var
  MaxLen     : longint;
  MaxDist    : longint;
  MaxMatch   : integer;
  ChainLen   : integer;
  PrevStrPos : PAnsiChar;
  CurPos     : PAnsiChar;
  {$IFDEF UseGreedyAsm}
  CurWord    : word;
  MaxWord    : word;
  {$ENDIF}
  {$IFDEF UseGreedyPascal}
  Len        : longint;
  MatchStr   : PAnsiChar;
  CurrentCh  : PAnsiChar;
  CurCh      : AnsiChar;
  MaxCh      : AnsiChar;
  {$ENDIF}
begin
  {calculate the hash index for the current position; using the
   Rabin-Karp algorithm this is equal to the previous index less the
   effect of the character just lost plus the effect of the character
   just gained}
  CurPos := FCurrent;
  FHashIndex :=
     ((FHashIndex shl c_HashShift) xor longint(CurPos[2])) and
     c_HashMask;

  {get the head of the hash chain: this is the position in the sliding
   window of the previous 3-character string with this hash value}
  PrevStrPos := FHashHeads^[FHashIndex];

  {set the head of the hash chain equal to our current position}
  FHashHeads^[FHashIndex] := CurPos;

  {update the chain itself: set the entry for this position equal to
   the previous string position}
  FHashChains^[PtrUInt(CurPos) and FWinMask] := PrevStrPos;

  {calculate the maximum match we could do at this position}
  MaxMatch := (FLookAheadEnd - CurPos);
  if (MaxMatch > FMaxMatchLen) then
    MaxMatch := FMaxMatchLen;
  if (aAmpleLength > MaxMatch) then
    aAmpleLength := MaxMatch;

  {calculate the current match length}
  if (aPrevMatch.maLen = 0) then
    MaxLen := 2
  else begin
    if (MaxMatch < aPrevMatch.maLen) then begin
      Result := false;
      aMatch.maLen := 0;
      aMatch.maLit := CurPos^;
      Exit;
    end;
    MaxLen := aPrevMatch.maLen;
  end;

  {get the bytes at the current position and at the end of the maximum
   match we have to better}
  {$IFDEF UseGreedyAsm}
  CurWord := PWord(CurPos)^;
  MaxWord := PWord(CurPos + pred(MaxLen))^;
  {$ENDIF}
  {$IFDEF UseGreedyPascal}
  CurCh := CurPos^;
  MaxCh := (CurPos + pred(MaxLen))^;
  {$ENDIF}

  {set the chain length to search based on the current maximum match
   (basically: if we've already satisfied the ample length
   requirement, don't search as far)}
  if (MaxLen >= aAmpleLength) then
    ChainLen := FChainLen div 4
  else
    ChainLen := FChainLen;

  {get ready for the loop}
  {$IFDEF DefeatWarnings}
  MaxDist := 0;
  {$ENDIF}

  {$IFDEF UseGreedyAsm} { slip into assembler for speed...}
  asm
    push ebx                 { save those registers we should}
    push esi
    push edi

    mov ebx, Self            { ebx will store the Self pointer}
    mov edi, PrevStrPos      { edi => previous string}
    mov esi, CurPos          { esi => current string}

  @@TestThisPosition:
                             { check previous string is in range}
    or edi, edi
    je @@Exit
    cmp edi, [ebx].TAbDfInputWindow.FStart
    jb @@Exit
    cmp edi, CurPos
    jae @@Exit

    mov ax, [edi]            { check previous string starts with same}
    cmp CurWord, ax          {   two bytes as current}
    jne @@GetNextPosition    { ..nope, they don't match}

    mov edx, edi             { check previous string ends with same}
    add edi, MaxLen          {   two bytes as current (by "ends" we}
    dec edi                  {   mean the last two bytes at the}
    mov ax, [edi]            {   current match length)}
    cmp MaxWord, ax
    mov edi, edx
    jne @@GetNextPosition    { ..nope, they don't match}

    push edi                 { compare the previous string with the}
    push esi                 {   current string}
    mov eax, MaxMatch
    add edi, 2               { (we've already checked that the first}
    sub eax, 2               { two characters are the same)}
    add esi, 2
    mov ecx, eax

  @@CmpQuads:
    cmp ecx, 4
    jb @@CmpSingles

    mov edx, [esi]
    cmp edx, [edi]
    jne @@CmpSingles

    add esi, 4
    add edi, 4
    sub ecx, 4
    jnz @@CmpQuads

    jmp @@MatchCheck

  @@CmpSingles:
    or ecx, ecx
    jb @@MatchCheck

    mov dl, [esi]
    cmp dl, [edi]
    jne @@MatchCheck

    inc esi
    inc edi
    dec ecx
    jnz @@CmpSingles

  @@MatchCheck:
    sub eax, ecx
    add eax, 2
    pop esi
    pop edi

    cmp eax, MaxLen          { have we found a longer match?}
    jbe @@GetNextPosition    { ..no}
    mov MaxLen, eax          { ..yes, so save it}

    mov eax, esi             { calculate the dist for this new match}
    sub eax, edi
    mov MaxDist, eax

    cmp eax, aAmpleLength    { if this match is ample enough, exit}
    jae @@Exit

    mov eax, esi             { calculate the two bytes at the end of}
    add eax, MaxLen          {   this new match}
    dec eax
    mov ax, [eax]
    mov MaxWord, ax

  @@GetNextPosition:
    mov eax, ChainLen        { we've visited one more link on the}
    dec eax                  {   chain, if that's the last one we}
    je @@Exit                {   should visit, exit}
    mov ChainLen, eax

                             { advance along the chain}
    mov edx, [ebx].TAbDfInputWindow.FHashChains
    mov eax, [ebx].TAbDfInputWindow.FWinMask
    and edi, eax
    shl edi, 2
    mov edi, [edx+edi]
    jmp @@TestThisPosition

  @@Exit:
    pop edi
    pop esi
    pop ebx
  end;
  {$ENDIF}

  {$IFDEF UseGreedyPascal}
  {for all possible hash nodes in the chain...}
  while (FStart <= PrevStrPos) and (PrevStrPos < CurPos) do begin

    {if the initial and maximal characters match...}
    if (PrevStrPos[0] = CurCh) and
       (PrevStrPos[pred(MaxLen)] = MaxCh) then begin

      {compare more characters}
      Len := 1;
      CurrentCh := CurPos + 1;
      MatchStr := PrevStrPos + 1;

      {compare away, but don't go above the maximum length}
      while (Len < MaxMatch) and (MatchStr^ = CurrentCh^) do begin
        inc(CurrentCh);
        inc(MatchStr);
        inc(Len);
      end;

      {have we reached another maximum for the length?}
      if (Len > MaxLen) then begin
        MaxLen := Len;
        {calculate the distance}
        MaxDist := CurPos - PrevStrPos;
        MaxCh := CurPos[pred(MaxLen)];

        {is the new best length ample enough?}
        if MaxLen >= aAmpleLength then
          Break;
      end;
    end;

    {have we reached the end of this chain?}
    dec(ChainLen);
    if (ChainLen = 0) then
      Break;

    {otherwise move onto the next position}
    PrevStrPos := FHashChains^[PtrUInt(PrevStrPos) and FWinMask];
  end;
  {$ENDIF}

  {based on the results of our investigation, return the match values}
  if (MaxLen < 3) or (MaxLen <= aPrevMatch.maLen) then begin
    Result := false;
    aMatch.maLen := 0;
    aMatch.maLit := CurPos^;
  end
  else begin
    Result := true;
    aMatch.maLen := MaxLen;
    aMatch.maDist := MaxDist;
    aMatch.maLit := CurPos^; { just in case...}
  end;
end;
{--------}
function TAbDfInputWindow.GetNextChar : AnsiChar;
begin
  Result := FCurrent^;
  inc(FCurrent);
end;
{--------}
function TAbDfInputWindow.GetNextKeyLength : integer;
begin
  Result := FLookAheadEnd - FCurrent;
  if (Result > 3) then
    Result := 3;
end;
{--------}
function TAbDfInputWindow.iwGetChecksum : longint;
begin
  {the CRC32 checksum algorithm requires a post-conditioning step
   after being calculated (the result is NOTted), whereas Adler32 does
   not}
  if FUseCRC32 then
    Result := not FChecksum
  else
    Result := FChecksum;
end;
{--------}
procedure TAbDfInputWindow.iwReadFromStream;
var
  BytesRead   : longint;
  BytesToRead : longint;
begin
  {read some more data into the look ahead zone}
  BytesToRead := FBufferEnd - FLookAheadEnd;
  BytesRead := FStream.Read(FLookAheadEnd^, BytesToRead);

  {if nothing was read, we reached the end of the stream; hence
   there's no more need to slide the window since we have all the
   data}
  if (BytesRead = 0) then
    FMustSlide := false

  {otherwise something was actually read...}
  else begin
    {update the checksum}
    if FUseCRC32 then
      AbUpdateCRCBuffer(FChecksum, FLookAheadEnd^, BytesRead)
    else
      AbUpdateAdlerBuffer(FChecksum, FLookAheadEnd^, BytesRead);

    {reposition the pointer for the end of the lookahead area}
    inc(FLookAheadEnd, BytesRead);
  end;
end;
{--------}
procedure TAbDfInputWindow.iwSetCapacity(aValue : longint);
var
  ActualSize : integer;
begin
  {calculate the actual size; this will be the value passed in, plus
   the correct look ahead size, plus 16KB}
  ActualSize := aValue + (16 * 1024);
  if FUseDeflate64 then begin
    inc(ActualSize, dfc_MaxMatchLen64);
    FMaxMatchLen := dfc_MaxMatchLen64;
  end
  else begin
    inc(ActualSize, dfc_MaxMatchLen);
    FMaxMatchLen := dfc_MaxMatchLen;
  end;

  {get the new buffer}
  GetMem(FBuffer, ActualSize);

  {set the other buffer pointers}
  FStart := FBuffer;
  FCurrent := FBuffer;
  FLookAheadEnd := FBuffer;
  FBufferEnd := FBuffer + ActualSize;
  FSlidePoint := FBuffer + (16 * 1024);
end;
{--------}
procedure TAbDfInputWindow.iwSlide;
var
  i : integer;
  ByteCount : PtrInt;
  Buffer    : PByte;
  ListItem  : PPointer;
begin
  {move current valid data back to the start of the buffer}
  ByteCount := FLookAheadEnd - FStart;
  Move(FStart^, FBuffer^, ByteCount);

  {reset the various pointers}
  ByteCount := FStart - FBuffer;
  FStart := FBuffer;
  dec(FCurrent, ByteCount);
  dec(FLookAheadEnd, ByteCount);

  {patch up the hash table: the head pointers}
  Buffer := FBuffer;
  ListItem := @FHashHeads^[0];
  for i := 0 to pred(c_HashCount) do begin
    dec(ListItem^, ByteCount);
    if (ListItem^ < Buffer) then
      ListItem^ := nil;
    inc(ListItem);
  end;

  {..the chain pointers}
  ListItem  := @FHashChains^[0];
  for i := 0 to pred(FWinSize) do begin
    dec(ListItem^, ByteCount);
    if (ListItem^ < Buffer) then
      ListItem^ := nil;
    inc(ListItem);
  end;

  {now read some more data from the stream}
  iwReadFromStream;
end;
{--------}
function TAbDfInputWindow.Position : longint;
begin
  Result := (FCurrent - FStart) + FStartOffset;
end;
{--------}
procedure TAbDfInputWindow.ReadBuffer(var aBuffer; aCount  : longint;
                                                   aOffset : Int64);
var
  CurPos : Int64;              
begin
  CurPos := FStream.Seek(0, soCurrent);
  FStream.Seek(aOffSet, soBeginning);
  FStream.ReadBuffer(aBuffer, aCount);
  FStream.Seek(CurPos, soBeginning);
end;
{====================================================================}

end.



