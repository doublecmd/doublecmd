unit ULZBinTree;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses ULZInWindow,Math;

type TLZBinTree=class(TLZInWindow)
       public
         cyclicBufferPos:integer;
         cyclicBufferSize:integer;
         matchMaxLen:integer;

         son: array of integer;
         hash: array of integer;

         cutValue:integer;
         hashMask:integer;
         hashSizeSum:integer;

         HASH_ARRAY:boolean;


         kNumHashDirectBytes:integer;
         kMinMatchCheck:integer;
         kFixHashSize:integer;
         constructor Create;
         procedure SetType(const numHashBytes:integer);
         procedure Init;override;
         procedure MovePos;override;
         function _Create(const historySize,keepAddBufferBefore,matchMaxLen,keepAddBufferAfter:integer):boolean;reintroduce;
         function GetMatches(var distances:array of integer):integer;
         procedure Skip(num:integer);
         procedure NormalizeLinks(var items:array of integer;const numItems,subValue:integer);
         procedure Normalize;
         procedure SetCutValue(const cutValue:integer);
       end;

implementation

const kHash2Size = 1 shl 10;
      kHash3Size = 1 shl 16;
      kBT2HashSize = 1 shl 16;
      kStartMaxLen = 1;
      kHash3Offset = kHash2Size;
      kEmptyHashValue = 0;
      kMaxValForNormalize = (1 shl 30) - 1;

var CRCTable: array [0..255] of integer;

constructor TLZBinTree.Create;
begin
inherited Create;
cyclicBufferSize:=0;
cutValue:=$FF;
hashSizeSum:=0;
HASH_ARRAY:=true;
kNumHashDirectBytes:=0;
kMinMatchCheck:=4;
kFixHashsize:=kHash2Size + kHash3Size;
end;

procedure TLZBinTree.SetType(const numHashBytes:integer);
begin
HASH_ARRAY := (numHashBytes > 2);
if HASH_ARRAY then begin
   kNumHashDirectBytes := 0;
   kMinMatchCheck := 4;
   kFixHashSize := kHash2Size + kHash3Size;
   end
   else begin
        kNumHashDirectBytes := 2;
        kMinMatchCheck := 2 + 1;
        kFixHashSize := 0;
        end;
end;

procedure TLZBinTree.Init;
var i:integer;
begin
inherited init;
for i := 0 to hashSizeSum - 1  do
    hash[i] := kEmptyHashValue;
cyclicBufferPos := 0;
ReduceOffsets(-1);
end;

procedure TLZBinTree.MovePos;
begin
inc(cyclicBufferPos);
if cyclicBufferPos >= cyclicBufferSize then
   cyclicBufferPos := 0;
inherited MovePos;
if pos = kMaxValForNormalize then
   Normalize;
end;

function TLZBinTree._Create(const historySize,keepAddBufferBefore,matchMaxLen,keepAddBufferAfter:integer):boolean;
var windowReservSize:integer;
    cyclicBufferSize:integer;
    hs:integer;
begin
if (historySize > kMaxValForNormalize - 256) then begin
   result:=false;
   exit;
   end;
cutValue := 16 + (matchMaxLen shr 1);

windowReservSize := (historySize + keepAddBufferBefore + matchMaxLen + keepAddBufferAfter) div 2 + 256;

inherited _Create(historySize + keepAddBufferBefore, matchMaxLen + keepAddBufferAfter, windowReservSize);

self.matchMaxLen := matchMaxLen;

cyclicBufferSize := historySize + 1;
if self.cyclicBufferSize <> cyclicBufferSize then begin
   self.cyclicBufferSize:=cyclicBufferSize;
   setlength(son,cyclicBufferSize * 2);
   end;

hs := kBT2HashSize;

if HASH_ARRAY then begin
   hs := historySize - 1;
   hs := hs or (hs shr 1);
   hs := hs or (hs shr 2);
   hs := hs or (hs shr 4);
   hs := hs or (hs shr 8);
   hs := hs shr 1;
   hs := hs or $FFFF;
   if (hs > (1 shl 24)) then
      hs := hs shr 1;
   hashMask := hs;
   inc(hs);
   hs := hs + kFixHashSize;
   end;
if (hs <> hashSizeSum) then begin
   hashSizeSum := hs;
   setlength(hash,hashSizeSum);
   end;
result:=true;
end;

function TLZBinTree.GetMatches(var distances:array of integer):integer;
var lenLimit:integer;
    offset,matchMinPos,cur,maxlen,hashvalue,hash2value,hash3value:integer;
    temp,curmatch,curmatch2,curmatch3,ptr0,ptr1,len0,len1,count:integer;
    delta,cyclicpos,pby1,len:integer;
begin
if pos + matchMaxLen <= streamPos then
   lenLimit := matchMaxLen
   else begin
        lenLimit := streamPos - pos;
        if lenLimit < kMinMatchCheck then begin
           MovePos();
           result:=0;
           exit;
           end;
        end;

offset := 0;
if (pos > cyclicBufferSize) then
   matchMinPos:=(pos - cyclicBufferSize)
   else matchMinPos:=0;
cur := bufferOffset + pos;
maxLen := kStartMaxLen; // to avoid items for len < hashSize;
hash2Value := 0;
hash3Value := 0;

if HASH_ARRAY then begin
   temp := CrcTable[bufferBase[cur] and $FF] xor (bufferBase[cur + 1] and $FF);
   hash2Value := temp and (kHash2Size - 1);
   temp := temp xor ((bufferBase[cur + 2] and $FF) shl 8);
   hash3Value := temp and (kHash3Size - 1);
   hashValue := (temp xor (CrcTable[bufferBase[cur + 3] and $FF] shl 5)) and hashMask;
   end else
       hashValue := ((bufferBase[cur] and $FF) xor ((bufferBase[cur + 1] and $FF) shl 8));

curMatch := hash[kFixHashSize + hashValue];
if HASH_ARRAY then begin
   curMatch2 := hash[hash2Value];
   curMatch3 := hash[kHash3Offset + hash3Value];
   hash[hash2Value] := pos;
   hash[kHash3Offset + hash3Value] := pos;
   if curMatch2 > matchMinPos then
      if bufferBase[bufferOffset + curMatch2] = bufferBase[cur] then begin
         maxLen := 2;
         distances[offset] := maxLen;
         inc(offset);
         distances[offset] := pos - curMatch2 - 1;
         inc(offset);
         end;
   if curMatch3 > matchMinPos then
      if bufferBase[bufferOffset + curMatch3] = bufferBase[cur] then begin
         if curMatch3 = curMatch2 then
            offset := offset - 2;
         maxLen := 3;
         distances[offset] := maxlen;
         inc(offset);
         distances[offset] := pos - curMatch3 - 1;
         inc(offset);
         curMatch2 := curMatch3;
         end;
   if (offset <> 0) and (curMatch2 = curMatch) then begin
      offset := offset - 2;
      maxLen := kStartMaxLen;
      end;
   end;

hash[kFixHashSize + hashValue] := pos;

ptr0 := (cyclicBufferPos shl 1) + 1;
ptr1 := (cyclicBufferPos shl 1);

len0 := kNumHashDirectBytes;
len1 := len0;

if kNumHashDirectBytes <> 0 then begin
   if (curMatch > matchMinPos) then begin
      if (bufferBase[bufferOffset + curMatch + kNumHashDirectBytes] <> bufferBase[cur + kNumHashDirectBytes]) then begin
         maxLen := kNumHashDirectBytes;
         distances[offset] := maxLen;
         inc(offset);
         distances[offset] := pos - curMatch - 1;
         inc(offset);
         end;
      end;
   end;

count := cutValue;

while (true) do begin
      if (curMatch <= matchMinPos) or (count = 0) then begin
         son[ptr1] := kEmptyHashValue;
         son[ptr0] := son[ptr1];
         break;
         end;
      dec(count);
      delta := pos - curMatch;
      if delta<=cyclicBufferPos then
         cyclicpos:=(cyclicBufferPos - delta) shl 1
         else cyclicpos:=(cyclicBufferPos - delta + cyclicBufferSize) shl 1;

      pby1 := bufferOffset + curMatch;
      len := min(len0, len1);
      if bufferBase[pby1 + len] = bufferBase[cur + len] then begin
         inc(len);
         while (len <> lenLimit) do begin
               if (bufferBase[pby1 + len] <> bufferBase[cur + len]) then
                   break;
               inc(len);
               end;
        if maxLen < len then begin
           maxLen := len;
           distances[offset] := maxlen;
           inc(offset);
           distances[offset] := delta - 1;
           inc(offset);
           if (len = lenLimit) then begin
              son[ptr1] := son[cyclicPos];
              son[ptr0] := son[cyclicPos + 1];
              break;
              end;
           end;
        end;
      if (bufferBase[pby1 + len] and $FF) < (bufferBase[cur + len] and $FF) then begin
         son[ptr1] := curMatch;
         ptr1 := cyclicPos + 1;
         curMatch := son[ptr1];
         len1 := len;
         end else begin
             son[ptr0] := curMatch;
             ptr0 := cyclicPos;
             curMatch := son[ptr0];
             len0 := len;
             end;
      end;
MovePos;
result:=offset;
end;

procedure TLZBinTree.Skip(num:integer);
var lenLimit,matchminpos,cur,hashvalue,temp,hash2value,hash3value,curMatch:integer;
    ptr0,ptr1,len,len0,len1,count,delta,cyclicpos,pby1:integer;
begin
repeat
  if pos + matchMaxLen <= streamPos then
     lenLimit := matchMaxLen
     else begin
          lenLimit := streamPos - pos;
          if lenLimit < kMinMatchCheck then begin
             MovePos();
             dec(num);
             continue;
             end;
          end;

  if pos>cyclicBufferSize then
     matchminpos:=(pos - cyclicBufferSize)
     else matchminpos:=0;
  cur := bufferOffset + pos;

  if HASH_ARRAY then begin
     temp := CrcTable[bufferBase[cur] and $FF] xor (bufferBase[cur + 1] and $FF);
     hash2Value := temp and (kHash2Size - 1);
     hash[hash2Value] := pos;
     temp := temp xor ((bufferBase[cur + 2] and $FF) shl 8);
     hash3Value := temp and (kHash3Size - 1);
     hash[kHash3Offset + hash3Value] := pos;
     hashValue := (temp xor (CrcTable[bufferBase[cur + 3] and $FF] shl 5)) and hashMask;
     end else
         hashValue := ((bufferBase[cur] and $FF) xor ((bufferBase[cur + 1] and $FF) shl 8));

  curMatch := hash[kFixHashSize + hashValue];
  hash[kFixHashSize + hashValue] := pos;

  ptr0 := (cyclicBufferPos shl 1) + 1;
  ptr1 := (cyclicBufferPos shl 1);

  len0 := kNumHashDirectBytes;
  len1 := kNumHashDirectBytes;

  count := cutValue;
  while true do begin
        if (curMatch <= matchMinPos) or (count = 0) then begin
           son[ptr1] := kEmptyHashValue;
           son[ptr0] := son[ptr1];
           break;
           end else dec(count);

        delta := pos - curMatch;
        if (delta <= cyclicBufferPos) then
           cyclicpos:=(cyclicBufferPos - delta) shl 1
           else cyclicpos:=(cyclicBufferPos - delta + cyclicBufferSize) shl 1;

        pby1 := bufferOffset + curMatch;
        len := min(len0, len1);
        if bufferBase[pby1 + len] = bufferBase[cur + len] then begin
           inc(len);
           while (len <> lenLimit) do begin
                 if bufferBase[pby1 + len] <> bufferBase[cur + len] then
                    break;
                 inc(len);
                 end;
           if len = lenLimit then begin
              son[ptr1] := son[cyclicPos];
              son[ptr0] := son[cyclicPos + 1];
              break;
              end;
           end;
        if ((bufferBase[pby1 + len] and $FF) < (bufferBase[cur + len] and $FF)) then begin
           son[ptr1] := curMatch;
           ptr1 := cyclicPos + 1;
           curMatch := son[ptr1];
           len1 := len;
           end else begin
               son[ptr0] := curMatch;
               ptr0 := cyclicPos;
               curMatch := son[ptr0];
               len0 := len;
               end;
        end;
  MovePos;
  dec(num);
  until num=0;
end;

procedure TLZBinTree.NormalizeLinks(var items:array of integer;const numItems,subValue:integer);
var i,value:integer;
begin
for i:=0 to NumItems-1 do begin
    value := items[i];
    if value <= subValue then
       value := kEmptyHashValue
       else value := value - subValue;
    items[i] := value;
    end;
end;

procedure TLZBinTree.Normalize;
var subvalue:integer;
begin
subValue := pos - cyclicBufferSize;
NormalizeLinks(son, cyclicBufferSize * 2, subValue);
NormalizeLinks(hash, hashSizeSum, subValue);
ReduceOffsets(subValue);
end;

procedure TLZBinTree.SetCutValue(const cutvalue:integer);
begin
self.cutValue:=cutValue;
end;

procedure InitCRC;
var i,r,j:integer;
begin
for i := 0 to 255 do begin
    r := i;
    for j := 0 to 7 do
        if ((r and 1) <> 0) then
          r := (r shr 1) xor integer($EDB88320)
        else
          r := r shr 1;
    CrcTable[i] := r;
    end;
end;

initialization
InitCRC;
end.
