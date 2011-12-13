unit ULZMAEncoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses UBitTreeEncoder,ULZMABase,ULZBinTree,URangeEncoder,Classes,Math,ULZMACommon;

const EMatchFinderTypeBT2 = 0;
      EMatchFinderTypeBT4 = 1;
      kIfinityPrice:integer = $FFFFFFF;
      kDefaultDictionaryLogSize = 22;
      kNumFastBytesDefault = $20;
      kNumLenSpecSymbols = ULZMABase.kNumLowLenSymbols + ULZMABase.kNumMidLenSymbols;
      kNumOpts = 1 shl 12;
      kPropSize = 5;

type TLZMAEncoder2=class;
     TLZMALiteralEncoder=class;
     TLZMAOptimal=class;
     TLZMALenPriceTableEncoder=class;

     TLZMAEncoder=class
       private
         FOnProgress:TLZMAProgress;
         procedure DoProgress(const Action:TLZMAProgressAction;const Value:integer);
       public
         g_FastPos:array [0..1 shl 11-1] of byte;
         _state:integer;
         _previousByte:byte;
         _repDistances:array [0..ULZMABase.kNumRepDistances-1] of integer;

         _optimum: array [0..kNumOpts-1] of TLZMAOptimal;
         _matchFinder:TLZBinTree;
         _rangeEncoder:TRangeEncoder;

         _isMatch:array [0..ULZMABase.kNumStates shl ULZMABase.kNumPosStatesBitsMax-1]of smallint;
         _isRep:array [0..ULZMABase.kNumStates-1] of smallint;
         _isRepG0:array [0..ULZMABase.kNumStates-1] of smallint;
         _isRepG1:array [0..ULZMABase.kNumStates-1] of smallint;
         _isRepG2:array [0..ULZMABase.kNumStates-1] of smallint;
         _isRep0Long:array [0..ULZMABase.kNumStates shl ULZMABase.kNumPosStatesBitsMax-1]of smallint;

         _posSlotEncoder:array [0..ULZMABase.kNumLenToPosStates-1] of TBitTreeEncoder; // kNumPosSlotBits

         _posEncoders:array [0..ULZMABase.kNumFullDistances-ULZMABase.kEndPosModelIndex-1]of smallint;
         _posAlignEncoder:TBitTreeEncoder;

         _lenEncoder:TLZMALenPriceTableEncoder;
         _repMatchLenEncoder:TLZMALenPriceTableEncoder;

         _literalEncoder:TLZMALiteralEncoder;

         _matchDistances:array [0..ULZMABase.kMatchMaxLen*2+1] of integer;

         _numFastBytes:integer;
         _longestMatchLength:integer;
         _numDistancePairs:integer;

         _additionalOffset:integer;

         _optimumEndIndex:integer;
         _optimumCurrentIndex:integer;

         _longestMatchWasFound:boolean;

         _posSlotPrices:array [0..1 shl (ULZMABase.kNumPosSlotBits+ULZMABase.kNumLenToPosStatesBits)-1] of integer;
         _distancesPrices:array [0..ULZMABase.kNumFullDistances shl ULZMABase.kNumLenToPosStatesBits-1] of integer;
         _alignPrices:array [0..ULZMABase.kAlignTableSize-1] of integer;
         _alignPriceCount:integer;

         _distTableSize:integer;

         _posStateBits:integer;
         _posStateMask:integer;
         _numLiteralPosStateBits:integer;
         _numLiteralContextBits:integer;

         _dictionarySize:integer;
         _dictionarySizePrev:integer;
         _numFastBytesPrev:integer;

         nowPos64:int64;
         _finished:boolean;
         _inStream:TStream;

         _matchFinderType:integer;
         _writeEndMark:boolean;

         _needReleaseMFStream:boolean;

         reps:array [0..ULZMABase.kNumRepDistances-1]of integer;
         repLens:array [0..ULZMABase.kNumRepDistances-1] of integer;
         backRes:integer;
         processedInSize:int64;
         processedOutSize:int64;
         finished:boolean;
         properties:array [0..kPropSize] of byte;
         tempPrices:array [0..ULZMABase.kNumFullDistances-1]of integer;
         _matchPriceCount:integer;
         constructor Create;
         destructor Destroy;override;
         function GetPosSlot(const pos:integer):integer;
         function GetPosSlot2(const pos:integer):integer;
         procedure BaseInit;
         procedure _Create;
         procedure SetWriteEndMarkerMode(const writeEndMarker:boolean);
         procedure Init;
         function ReadMatchDistances:integer;
         procedure MovePos(const num:integer);
         function GetRepLen1Price(const state,posState:integer):integer;
         function GetPureRepPrice(const repIndex, state, posState:integer):integer;
         function GetRepPrice(const repIndex, len, state, posState:integer):integer;
         function GetPosLenPrice(const pos, len, posState:integer):integer;
         function Backward(cur:integer):integer;
         function GetOptimum(position:integer):integer;
         function ChangePair(const smallDist, bigDist:integer):boolean;
         procedure WriteEndMarker(const posState:integer);
         procedure Flush(const nowPos:integer);
         procedure ReleaseMFStream;
         procedure CodeOneBlock(var inSize,outSize:int64;var finished:boolean);
         procedure FillDistancesPrices;
         procedure FillAlignPrices;
         procedure SetOutStream(const outStream:TStream);
         procedure ReleaseOutStream;
         procedure ReleaseStreams;
         procedure SetStreams(const inStream, outStream:TStream;const inSize, outSize:int64);
         procedure Code(const inStream, outStream:TStream;const inSize, outSize:int64);
         procedure WriteCoderProperties(const outStream:TStream);
         function SetAlgorithm(const algorithm:integer):boolean;
         function SetDictionarySize(dictionarySize:integer):boolean;
         function SeNumFastBytes(const numFastBytes:integer):boolean;
         function SetMatchFinder(const matchFinderIndex:integer):boolean;
         function SetLcLpPb(const lc,lp,pb:integer):boolean;
         procedure SetEndMarkerMode(const endMarkerMode:boolean);
         property OnProgress:TLZMAProgress read FOnProgress write FOnProgress;
       end;

     TLZMALiteralEncoder=class
       public
         m_Coders: array of TLZMAEncoder2;
   m_NumPrevBits:integer;
   m_NumPosBits:integer;
   m_PosMask:integer;
         procedure _Create(const numPosBits,numPrevBits:integer);
         destructor Destroy;override;
         procedure Init;
         function GetSubCoder(const pos:integer;const prevByte:byte):TLZMAEncoder2;
       end;

     TLZMAEncoder2=class
       public
         m_Encoders: array[0..$300-1] of smallint;
         procedure Init;
         procedure Encode(const rangeEncoder:TRangeEncoder;const symbol:byte);
         procedure EncodeMatched(const rangeEncoder:TRangeEncoder;const matchByte,symbol:byte);
         function GetPrice(const matchMode:boolean;const matchByte,symbol:byte):integer;
       end;

     TLZMALenEncoder=class
       public
         _choice:array[0..1] of smallint;
         _lowCoder: array [0..ULZMABase.kNumPosStatesEncodingMax-1] of TBitTreeEncoder;
         _midCoder: array [0..ULZMABase.kNumPosStatesEncodingMax-1] of TBitTreeEncoder;
         _highCoder:TBitTreeEncoder;
         constructor Create;
         destructor Destroy;override;
         procedure Init(const numPosStates:integer);
         procedure Encode(const rangeEncoder:TRangeEncoder;symbol:integer;const posState:integer);virtual;
         procedure SetPrices(const posState,numSymbols:integer;var prices:array of integer;const st:integer);
       end;

     TLZMALenPriceTableEncoder=class(TLZMALenEncoder)
       public
         _prices: array [0..ULZMABase.kNumLenSymbols shl ULZMABase.kNumPosStatesBitsEncodingMax-1] of integer;
         _tableSize:integer;
         _counters: array [0..ULZMABase.kNumPosStatesEncodingMax-1] of integer;
         procedure SetTableSize(const tableSize:integer);
         function GetPrice(const symbol,posState:integer):integer;
         procedure UpdateTable(const posState:integer);
         procedure UpdateTables(const numPosStates:integer);
         procedure Encode(const rangeEncoder:TRangeEncoder;symbol:integer;const posState:integer);override;
       end;

     TLZMAOptimal=class
       public
         State:integer;

         Prev1IsChar:boolean;
         Prev2:boolean;

         PosPrev2:integer;
         BackPrev2:integer;

         Price:integer;
         PosPrev:integer;
         BackPrev:integer;

         Backs0:integer;
         Backs1:integer;
         Backs2:integer;
         Backs3:integer;

         procedure MakeAsChar;
         procedure MakeAsShortRep;
         function IsShortRep:boolean;
       end;

implementation

constructor TLZMAEncoder.Create;
var kFastSlots,c,slotFast,j,k:integer;
begin
kFastSlots := 22;
c := 2;
g_FastPos[0] := 0;
g_FastPos[1] := 1;
for slotFast := 2 to kFastSlots -1 do begin
    k := (1 shl ((slotFast shr 1) - 1));
    for j := 0 to k -1 do begin
        g_FastPos[c] := slotFast;
        inc(c);
        end;
    end;
_state := ULZMABase.StateInit();
_matchFinder:=nil;
_rangeEncoder:=TRangeEncoder.Create;
_posAlignEncoder:=TBitTreeEncoder.Create(ULZMABase.kNumAlignBits);
_lenEncoder:=TLZMALenPriceTableEncoder.Create;
_repMatchLenEncoder:=TLZMALenPriceTableEncoder.Create;
_literalEncoder:=TLZMALiteralEncoder.Create;
_numFastBytes:= kNumFastBytesDefault;
_distTableSize:= (kDefaultDictionaryLogSize * 2);
_posStateBits:= 2;
_posStateMask:= (4 - 1);
_numLiteralPosStateBits:= 0;
_numLiteralContextBits:= 3;

_dictionarySize:= (1 shl kDefaultDictionaryLogSize);
_dictionarySizePrev:= -1;
_numFastBytesPrev:= -1;
_matchFinderType:= EMatchFinderTypeBT4;
_writeEndMark:= false;

_needReleaseMFStream:= false;
end;

destructor TLZMAEncoder.Destroy;
var i:integer;
begin
_rangeEncoder.Free;
_posAlignEncoder.Free;
_lenEncoder.Free;
_repMatchLenEncoder.Free;
_literalEncoder.Free;
if _matchFinder<>nil then _matchFinder.Free;
for i := 0 to kNumOpts -1 do
    _optimum[i].Free;
for i := 0 to ULZMABase.kNumLenToPosStates -1 do
    _posSlotEncoder[i].Free;
end;

procedure TLZMAEncoder._Create;
var bt:TLZBinTree;
    numHashBytes,i:integer;
begin
if _matchFinder = nil then begin
   bt := TLZBinTree.Create;
   numHashBytes:= 4;
   if _matchFinderType = EMatchFinderTypeBT2 then
      numHashBytes := 2;
   bt.SetType(numHashBytes);
   _matchFinder := bt;
   end;
_literalEncoder._Create(_numLiteralPosStateBits, _numLiteralContextBits);

if (_dictionarySize = _dictionarySizePrev) and (_numFastBytesPrev = _numFastBytes) then
   exit;
_matchFinder._Create(_dictionarySize, kNumOpts, _numFastBytes, ULZMABase.kMatchMaxLen + 1);
_dictionarySizePrev := _dictionarySize;
_numFastBytesPrev := _numFastBytes;

for i := 0 to kNumOpts -1 do
    _optimum[i]:=TLZMAOptimal.Create;
for i := 0 to ULZMABase.kNumLenToPosStates -1 do
    _posSlotEncoder[i] :=TBitTreeEncoder.Create(ULZMABase.kNumPosSlotBits);
end;

function TLZMAEncoder.GetPosSlot(const pos:integer):integer;
begin
if (pos < (1 shl 11)) then
   result:=g_FastPos[pos]
else if (pos < (1 shl 21)) then
     result:=(g_FastPos[pos shr 10] + 20)
else result:=(g_FastPos[pos shr 20] + 40);
end;

function TLZMAEncoder.GetPosSlot2(const pos:integer):integer;
begin
if (pos < (1 shl 17)) then
   result:=(g_FastPos[pos shr 6] + 12)
else if (pos < (1 shl 27)) then
     result:=(g_FastPos[pos shr 16] + 32)
else result:=(g_FastPos[pos shr 26] + 52);
end;

procedure TLZMAEncoder.BaseInit;
var i:integer;
begin
_state := ulzmaBase.StateInit;
_previousByte := 0;
for i := 0 to ULZMABase.kNumRepDistances -1 do
    _repDistances[i] := 0;
end;

procedure TLZMAEncoder.SetWriteEndMarkerMode(const writeEndMarker:boolean);
begin
_writeEndMark := writeEndMarker;
end;

procedure TLZMAEncoder.Init;
var i:integer;
begin
BaseInit;
_rangeEncoder.Init;

URangeEncoder.InitBitModels(_isMatch);
URangeEncoder.InitBitModels(_isRep0Long);
URangeEncoder.InitBitModels(_isRep);
URangeEncoder.InitBitModels(_isRepG0);
URangeEncoder.InitBitModels(_isRepG1);
URangeEncoder.InitBitModels(_isRepG2);
URangeEncoder.InitBitModels(_posEncoders);


_literalEncoder.Init();
for i := 0 to ULZMABase.kNumLenToPosStates -1 do
    _posSlotEncoder[i].Init;

_lenEncoder.Init(1 shl _posStateBits);
_repMatchLenEncoder.Init(1 shl _posStateBits);

_posAlignEncoder.Init;

_longestMatchWasFound := false;
_optimumEndIndex := 0;
_optimumCurrentIndex := 0;
_additionalOffset := 0;
end;

function TLZMAEncoder.ReadMatchDistances:integer;
var lenRes:integer;
begin
lenRes := 0;
_numDistancePairs := _matchFinder.GetMatches(_matchDistances);

if _numDistancePairs > 0 then begin
   lenRes := _matchDistances[_numDistancePairs - 2];
   if lenRes = _numFastBytes then
      lenRes := lenRes + _matchFinder.GetMatchLen(lenRes - 1, _matchDistances[_numDistancePairs - 1], ULZMABase.kMatchMaxLen - lenRes);
   end;
inc(_additionalOffset);
result:=lenRes;
end;

procedure TLZMAEncoder.MovePos(const num:integer);
begin
if num > 0 then begin
   _matchFinder.Skip(num);
   _additionalOffset := _additionalOffset + num;
   end;
end;

function TLZMAEncoder.GetRepLen1Price(const state,posState:integer):integer;
begin
result:=RangeEncoder.GetPrice0(_isRepG0[state]) +
        RangeEncoder.GetPrice0(_isRep0Long[(state shl ULZMABase.kNumPosStatesBitsMax) + posState]);
end;

function TLZMAEncoder.GetPureRepPrice(const repIndex, state, posState:integer):integer;
var price:integer;
begin
if repIndex = 0 then begin
   price := RangeEncoder.GetPrice0(_isRepG0[state]);
   price := price + RangeEncoder.GetPrice1(_isRep0Long[(state shl ULZMABase.kNumPosStatesBitsMax) + posState]);
   end else begin
       price := RangeEncoder.GetPrice1(_isRepG0[state]);
       if repIndex = 1 then
          price := price + RangeEncoder.GetPrice0(_isRepG1[state])
          else begin
               price := price + RangeEncoder.GetPrice1(_isRepG1[state]);
               price := price + RangeEncoder.GetPrice(_isRepG2[state], repIndex - 2);
               end;
       end;
result:=price;
end;

function TLZMAEncoder.GetRepPrice(const repIndex, len, state, posState:integer):integer;
var price:integer;
begin
price := _repMatchLenEncoder.GetPrice(len - ULZMABase.kMatchMinLen, posState);
result := price + GetPureRepPrice(repIndex, state, posState);
end;

function TLZMAEncoder.GetPosLenPrice(const pos, len, posState:integer):integer;
var price,lenToPosState:integer;
begin
lenToPosState := ULZMABase.GetLenToPosState(len);
if pos < ULZMABase.kNumFullDistances then
   price := _distancesPrices[(lenToPosState * ULZMABase.kNumFullDistances) + pos]
   else price := _posSlotPrices[(lenToPosState shl ULZMABase.kNumPosSlotBits) + GetPosSlot2(pos)] +
           _alignPrices[pos and ULZMABase.kAlignMask];
result := price + _lenEncoder.GetPrice(len - ULZMABase.kMatchMinLen, posState);
end;

function TLZMAEncoder.Backward(cur:integer):integer;
var posMem,backMem,posPrev,backCur:integer;
begin
_optimumEndIndex := cur;
posMem := _optimum[cur].PosPrev;
backMem := _optimum[cur].BackPrev;
repeat
  if _optimum[cur].Prev1IsChar then begin
     _optimum[posMem].MakeAsChar;
     _optimum[posMem].PosPrev := posMem - 1;
     if _optimum[cur].Prev2 then begin
        _optimum[posMem - 1].Prev1IsChar := false;
        _optimum[posMem - 1].PosPrev := _optimum[cur].PosPrev2;
        _optimum[posMem - 1].BackPrev := _optimum[cur].BackPrev2;
        end;
     end;
  posPrev := posMem;
  backCur := backMem;

  backMem := _optimum[posPrev].BackPrev;
  posMem := _optimum[posPrev].PosPrev;

  _optimum[posPrev].BackPrev := backCur;
  _optimum[posPrev].PosPrev := cur;
  cur := posPrev;
  until not (cur > 0);
backRes := _optimum[0].BackPrev;
_optimumCurrentIndex := _optimum[0].PosPrev;
result:=_optimumCurrentIndex;
end;

function TLZMAEncoder.GetOptimum(position:integer):integer;
var lenRes,lenMain,numDistancePairs,numAvailableBytes,repMaxIndex,i:integer;
    matchPrice,repMatchPrice,shortRepPrice,lenEnd,len,repLen,price:integer;
    curAndLenPrice,normalMatchPrice,Offs,distance,cur,newLen:integer;
    posPrev,state,pos,curPrice,curAnd1Price,numAvailableBytesFull:integer;
    lenTest2,t,state2,posStateNext,nextRepMatchPrice,offset:integer;
    startLen,repIndex,lenTest,lenTestTemp,curAndLenCharPrice:integer;
    nextMatchPrice,curBack:integer;
    optimum,opt,nextOptimum:TLZMAOptimal;
    currentByte,matchByte,posState:byte;
    nextIsChar:boolean;
begin
if (_optimumEndIndex <> _optimumCurrentIndex) then begin
   lenRes := _optimum[_optimumCurrentIndex].PosPrev - _optimumCurrentIndex;
   backRes := _optimum[_optimumCurrentIndex].BackPrev;
   _optimumCurrentIndex := _optimum[_optimumCurrentIndex].PosPrev;
   result:=lenRes;
   exit;
   end;//if optimumendindex
_optimumCurrentIndex := 0;
_optimumEndIndex := 0;

if not _longestMatchWasFound then begin
   lenMain := ReadMatchDistances();
   end else begin //if not longest
       lenMain := _longestMatchLength;
       _longestMatchWasFound := false;
       end;//if not longest else
numDistancePairs := _numDistancePairs;

numAvailableBytes := _matchFinder.GetNumAvailableBytes + 1;
if numAvailableBytes < 2 then begin
   backRes := -1;
   result:=1;
   exit;
   end;//if numavailable
{if numAvailableBytes > ULZMABase.kMatchMaxLen then
   numAvailableBytes := ULZMABase.kMatchMaxLen;}

repMaxIndex := 0;
for i := 0 to ULZMABase.kNumRepDistances-1 do begin
    reps[i] := _repDistances[i];
    repLens[i] := _matchFinder.GetMatchLen(0 - 1, reps[i], ULZMABase.kMatchMaxLen);
    if repLens[i] > repLens[repMaxIndex] then
       repMaxIndex := i;
    end;//for i
if repLens[repMaxIndex] >= _numFastBytes then begin
   backRes := repMaxIndex;
   lenRes := repLens[repMaxIndex];
   MovePos(lenRes - 1);
   result:=lenRes;
   exit;
   end;//if replens[]

if lenMain >= _numFastBytes then begin
   backRes := _matchDistances[numDistancePairs - 1] + ULZMABase.kNumRepDistances;
   MovePos(lenMain - 1);
   result:=lenMain;
   exit;
   end;//if lenMain

currentByte := _matchFinder.GetIndexByte(0 - 1);
matchByte := _matchFinder.GetIndexByte(0 - _repDistances[0] - 1 - 1);

if (lenMain < 2) and (currentByte <> matchByte) and (repLens[repMaxIndex] < 2) then begin
   backRes := -1;
   result:=1;
   exit;
   end;//if lenmain<2

_optimum[0].State := _state;

posState := (position and _posStateMask);

_optimum[1].Price := RangeEncoder.GetPrice0(_isMatch[(_state shl ULZMABase.kNumPosStatesBitsMax) + posState]) +
        _literalEncoder.GetSubCoder(position, _previousByte).GetPrice(not ULZMABase.StateIsCharState(_state), matchByte, currentByte);
_optimum[1].MakeAsChar();

matchPrice := RangeEncoder.GetPrice1(_isMatch[(_state shl ULZMABase.kNumPosStatesBitsMax) + posState]);
repMatchPrice := matchPrice + RangeEncoder.GetPrice1(_isRep[_state]);

if matchByte = currentByte then begin
   shortRepPrice := repMatchPrice + GetRepLen1Price(_state, posState);
   if shortRepPrice < _optimum[1].Price then begin
      _optimum[1].Price := shortRepPrice;
      _optimum[1].MakeAsShortRep;
      end;//if shortrepprice
   end;//if matchbyte

if lenMain >= repLens[repMaxIndex] then lenEnd:=lenMain
   else lenEnd:=repLens[repMaxIndex];

if lenEnd < 2 then begin
   backRes := _optimum[1].BackPrev;
   result:=1;
   exit;
   end;//if lenend<2

_optimum[1].PosPrev := 0;

_optimum[0].Backs0 := reps[0];
_optimum[0].Backs1 := reps[1];
_optimum[0].Backs2 := reps[2];
_optimum[0].Backs3 := reps[3];

len := lenEnd;
repeat
  _optimum[len].Price := kIfinityPrice;
  dec(len);
  until not (len >= 2);

for i := 0 to ULZMABase.kNumRepDistances -1 do begin
    repLen := repLens[i];
    if repLen < 2 then
       continue;
    price := repMatchPrice + GetPureRepPrice(i, _state, posState);
    repeat
      curAndLenPrice := price + _repMatchLenEncoder.GetPrice(repLen - 2, posState);
      optimum := _optimum[repLen];
      if curAndLenPrice < optimum.Price then begin
         optimum.Price := curAndLenPrice;
         optimum.PosPrev := 0;
         optimum.BackPrev := i;
         optimum.Prev1IsChar := false;
         end;//if curandlenprice
      dec(replen);
      until not (repLen >= 2);
    end;//for i

normalMatchPrice := matchPrice + RangeEncoder.GetPrice0(_isRep[_state]);

if repLens[0] >= 2 then len:=repLens[0] + 1
   else len:=2;

if len <= lenMain then begin
   offs := 0;
   while len > _matchDistances[offs] do
         offs := offs + 2;
   while (true) do begin
         distance := _matchDistances[offs + 1];
         curAndLenPrice := normalMatchPrice + GetPosLenPrice(distance, len, posState);
         optimum := _optimum[len];
         if curAndLenPrice < optimum.Price then begin
            optimum.Price := curAndLenPrice;
            optimum.PosPrev := 0;
            optimum.BackPrev := distance + ULZMABase.kNumRepDistances;
            optimum.Prev1IsChar := false;
            end;//if curlenandprice
         if len = _matchDistances[offs] then begin
            offs := offs + 2;
            if offs = numDistancePairs then
               break;
            end;//if len=_match
         inc(len);
         end;//while (true)
   end;//if len<=lenmain

cur := 0;

while (true) do begin
      inc(cur);
      if cur = lenEnd then begin
         result:=Backward(cur);
         exit;
         end;//if cur=lenEnd
      newLen := ReadMatchDistances;
      numDistancePairs := _numDistancePairs;
      if newLen >= _numFastBytes then begin
         _longestMatchLength := newLen;
         _longestMatchWasFound := true;
         result:=Backward(cur);
         exit;
         end;//if newlen=_numfast
      inc(position);
      posPrev := _optimum[cur].PosPrev;
      if _optimum[cur].Prev1IsChar then begin
         dec(posPrev);
         if _optimum[cur].Prev2 then begin
            state := _optimum[_optimum[cur].PosPrev2].State;
            if _optimum[cur].BackPrev2 < ULZMABase.kNumRepDistances then
               state := ULZMABase.StateUpdateRep(state)
               else state := ULZMABase.StateUpdateMatch(state);
            end//if _optimum[cur].Prev2
            else state := _optimum[posPrev].State;
         state := ULZMABase.StateUpdateChar(state);
         end//if _optimum[cur].Prev1IsChar
         else state := _optimum[posPrev].State;
      if posPrev = cur - 1 then begin
         if _optimum[cur].IsShortRep then
            state := ULZMABase.StateUpdateShortRep(state)
            else state := ULZMABase.StateUpdateChar(state);
         end //if posPrev = cur - 1
         else begin
              if _optimum[cur].Prev1IsChar and _optimum[cur].Prev2 then begin
                 posPrev := _optimum[cur].PosPrev2;
                 pos := _optimum[cur].BackPrev2;
                 state := ULZMABase.StateUpdateRep(state);
                 end//if _optimum[cur].Prev1IsChar
                 else begin
                      pos := _optimum[cur].BackPrev;
                      if pos < ULZMABase.kNumRepDistances then
                         state := ULZMABase.StateUpdateRep(state)
                         else state := ULZMABase.StateUpdateMatch(state);
                      end;//if else  _optimum[cur].Prev1IsChar
              opt := _optimum[posPrev];
              if pos < ULZMABase.kNumRepDistances then begin
                 if pos = 0 then begin
                    reps[0] := opt.Backs0;
                    reps[1] := opt.Backs1;
                    reps[2] := opt.Backs2;
                    reps[3] := opt.Backs3;
                    end//if pos=0
                    else if pos = 1 then begin
                         reps[0] := opt.Backs1;
                         reps[1] := opt.Backs0;
                         reps[2] := opt.Backs2;
                         reps[3] := opt.Backs3;
                         end //if pos=1
                    else if pos = 2 then begin
                         reps[0] := opt.Backs2;
                         reps[1] := opt.Backs0;
                         reps[2] := opt.Backs1;
                         reps[3] := opt.Backs3;
                         end//if pos=2
                    else begin
                         reps[0] := opt.Backs3;
                         reps[1] := opt.Backs0;
                         reps[2] := opt.Backs1;
                         reps[3] := opt.Backs2;
                         end;//else if pos=
                 end// if pos < ULZMABase.kNumRepDistances
                 else begin
                      reps[0] := (pos - ULZMABase.kNumRepDistances);
                      reps[1] := opt.Backs0;
                      reps[2] := opt.Backs1;
                      reps[3] := opt.Backs2;
                      end;//if else pos < ULZMABase.kNumRepDistances
              end;//if else posPrev = cur - 1
      _optimum[cur].State := state;
      _optimum[cur].Backs0 := reps[0];
      _optimum[cur].Backs1 := reps[1];
      _optimum[cur].Backs2 := reps[2];
      _optimum[cur].Backs3 := reps[3];
      curPrice := _optimum[cur].Price;

      currentByte := _matchFinder.GetIndexByte(0 - 1);
      matchByte := _matchFinder.GetIndexByte(0 - reps[0] - 1 - 1);

      posState := (position and _posStateMask);

      curAnd1Price := curPrice +
        RangeEncoder.GetPrice0(_isMatch[(state shl ULZMABase.kNumPosStatesBitsMax) + posState]) +
        _literalEncoder.GetSubCoder(position, _matchFinder.GetIndexByte(0 - 2)).
        GetPrice(not ULZMABase.StateIsCharState(state), matchByte, currentByte);

      nextOptimum := _optimum[cur + 1];

      nextIsChar := false;
      if curAnd1Price < nextOptimum.Price then begin
         nextOptimum.Price := curAnd1Price;
         nextOptimum.PosPrev := cur;
         nextOptimum.MakeAsChar;
         nextIsChar := true;
         end;//if curand1price

      matchPrice := curPrice + RangeEncoder.GetPrice1(_isMatch[(state shl ULZMABase.kNumPosStatesBitsMax) + posState]);
      repMatchPrice := matchPrice + RangeEncoder.GetPrice1(_isRep[state]);

      if (matchByte = currentByte) and
           (not ((nextOptimum.PosPrev < cur) and (nextOptimum.BackPrev = 0))) then begin
         shortRepPrice := repMatchPrice + GetRepLen1Price(state, posState);
         if shortRepPrice <= nextOptimum.Price then begin
            nextOptimum.Price := shortRepPrice;
            nextOptimum.PosPrev := cur;
            nextOptimum.MakeAsShortRep;
            nextIsChar := true;
            end;//if shortRepPrice <= nextOptimum.Price
         end;//if (matchByte = currentByte) and

      numAvailableBytesFull := _matchFinder.GetNumAvailableBytes + 1;
      numAvailableBytesFull := min(kNumOpts - 1 - cur, numAvailableBytesFull);
      numAvailableBytes := numAvailableBytesFull;

      if numAvailableBytes < 2 then
         continue;
      if numAvailableBytes > _numFastBytes then
         numAvailableBytes := _numFastBytes;
      if (not nextIsChar) and (matchByte <> currentByte) then begin
         // try Literal + rep0
         t := min(numAvailableBytesFull - 1, _numFastBytes);
         lenTest2 := _matchFinder.GetMatchLen(0, reps[0], t);
         if lenTest2 >= 2 then begin
            state2 := ULZMABase.StateUpdateChar(state);

            posStateNext := (position + 1) and _posStateMask;
            nextRepMatchPrice := curAnd1Price +
               RangeEncoder.GetPrice1(_isMatch[(state2 shl ULZMABase.kNumPosStatesBitsMax) + posStateNext]) +
               RangeEncoder.GetPrice1(_isRep[state2]);
            begin
              offset := cur + 1 + lenTest2;
              while lenEnd < offset do begin
                    inc(lenEnd);
                    _optimum[lenEnd].Price := kIfinityPrice;
                    end;//while lenend
              curAndLenPrice := nextRepMatchPrice + GetRepPrice(
                   0, lenTest2, state2, posStateNext);
              optimum := _optimum[offset];
              if curAndLenPrice < optimum.Price then begin
                 optimum.Price := curAndLenPrice;
                 optimum.PosPrev := cur + 1;
                 optimum.BackPrev := 0;
                 optimum.Prev1IsChar := true;
                 optimum.Prev2 := false;
                 end;//if curandlenprice
              end;//none
            end;//if lentest
         end;//if not nextischar and ...

      startLen := 2; // speed optimization

      for repIndex := 0 to ULZMABase.kNumRepDistances -1 do begin
          lenTest := _matchFinder.GetMatchLen(0 - 1, reps[repIndex], numAvailableBytes);
          if lenTest < 2 then
             continue;
          lenTestTemp := lenTest;
          repeat
            while lenEnd < cur + lenTest do begin
                  inc(lenEnd);
                  _optimum[lenEnd].Price := kIfinityPrice;
                  end;//while lenEnd
            curAndLenPrice := repMatchPrice + GetRepPrice(repIndex, lenTest, state, posState);
            optimum := _optimum[cur + lenTest];
            if curAndLenPrice < optimum.Price then begin
               optimum.Price := curAndLenPrice;
               optimum.PosPrev := cur;
               optimum.BackPrev := repIndex;
               optimum.Prev1IsChar := false;
               end;//if curandlen
            dec(lenTest);
            until not (lenTest >= 2);
          lenTest := lenTestTemp;

          if repIndex = 0 then
             startLen := lenTest + 1;

          // if (_maxMode)
          if lenTest < numAvailableBytesFull then begin
             t := min(numAvailableBytesFull - 1 - lenTest, _numFastBytes);
             lenTest2 := _matchFinder.GetMatchLen(lenTest, reps[repIndex], t);
             if lenTest2 >= 2 then begin
                state2 := ULZMABase.StateUpdateRep(state);

                posStateNext := (position + lenTest) and _posStateMask;
                curAndLenCharPrice :=
                   repMatchPrice + GetRepPrice(repIndex, lenTest, state, posState) +
                   RangeEncoder.GetPrice0(_isMatch[(state2 shl ULZMABase.kNumPosStatesBitsMax) + posStateNext]) +
                   _literalEncoder.GetSubCoder(position + lenTest,
                   _matchFinder.GetIndexByte(lenTest - 1 - 1)).GetPrice(true,
                   _matchFinder.GetIndexByte(lenTest - 1 - (reps[repIndex] + 1)),
                   _matchFinder.GetIndexByte(lenTest - 1));
                state2 := ULZMABase.StateUpdateChar(state2);
                posStateNext := (position + lenTest + 1) and _posStateMask;
                nextMatchPrice := curAndLenCharPrice + RangeEncoder.GetPrice1(_isMatch[(state2 shl ULZMABase.kNumPosStatesBitsMax) + posStateNext]);
                nextRepMatchPrice := nextMatchPrice + RangeEncoder.GetPrice1(_isRep[state2]);

                // for(; lenTest2 >= 2; lenTest2--)
                begin
                  offset := lenTest + 1 + lenTest2;
                  while lenEnd < cur + offset do begin
                        inc(lenEnd);
                        _optimum[lenEnd].Price := kIfinityPrice;
                        end;//while lenEnd
                  curAndLenPrice := nextRepMatchPrice + GetRepPrice(0, lenTest2, state2, posStateNext);
                  optimum := _optimum[cur + offset];
                  if curAndLenPrice < optimum.Price then begin
                     optimum.Price := curAndLenPrice;
                     optimum.PosPrev := cur + lenTest + 1;
                     optimum.BackPrev := 0;
                     optimum.Prev1IsChar := true;
                     optimum.Prev2 := true;
                     optimum.PosPrev2 := cur;
                     optimum.BackPrev2 := repIndex;
                     end;//if curAndLenPrice < optimum.Price
                  end;//none
                end;//if lenTest2 >= 2
             end;//if lenTest < numAvailableBytesFull
          end;//for repIndex

      if newLen > numAvailableBytes then begin
         newLen := numAvailableBytes;
         numDistancePairs := 0;
         while newLen > _matchDistances[numDistancePairs] do
               numDistancePairs := numDistancePairs + 2;
         _matchDistances[numDistancePairs] := newLen;
         numDistancePairs := numDistancePairs + 2;
         end;//if newLen > numAvailableBytes
      if newLen >= startLen then begin
         normalMatchPrice := matchPrice + RangeEncoder.GetPrice0(_isRep[state]);
         while lenEnd < cur + newLen do begin
               inc(lenEnd);
               _optimum[lenEnd].Price := kIfinityPrice;
               end;//while lenEnd

         offs := 0;
         while startLen > _matchDistances[offs] do
               offs := offs + 2;

         lenTest := startLen;
         while (true) do begin
               curBack := _matchDistances[offs + 1];
               curAndLenPrice := normalMatchPrice + GetPosLenPrice(curBack, lenTest, posState);
               optimum := _optimum[cur + lenTest];
               if curAndLenPrice < optimum.Price then begin
                  optimum.Price := curAndLenPrice;
                  optimum.PosPrev := cur;
                  optimum.BackPrev := curBack + ULZMABase.kNumRepDistances;
                  optimum.Prev1IsChar := false;
                  end;//if curAndLenPrice < optimum.Price

               if lenTest = _matchDistances[offs] then begin
                  if lenTest < numAvailableBytesFull then begin
                     t := min(numAvailableBytesFull - 1 - lenTest, _numFastBytes);
                     lenTest2 := _matchFinder.GetMatchLen(lenTest, curBack, t);
                     if lenTest2 >= 2 then begin
                        state2 := ULZMABase.StateUpdateMatch(state);

                        posStateNext := (position + lenTest) and _posStateMask;
                        curAndLenCharPrice := curAndLenPrice +
                           RangeEncoder.GetPrice0(_isMatch[(state2 shl ULZMABase.kNumPosStatesBitsMax) + posStateNext]) +
                           _literalEncoder.GetSubCoder(position + lenTest,
                           _matchFinder.GetIndexByte(lenTest - 1 - 1)).
                           GetPrice(true,
                           _matchFinder.GetIndexByte(lenTest - (curBack + 1) - 1),
                           _matchFinder.GetIndexByte(lenTest - 1));
                        state2 := ULZMABase.StateUpdateChar(state2);
                        posStateNext := (position + lenTest + 1) and _posStateMask;
                        nextMatchPrice := curAndLenCharPrice + RangeEncoder.GetPrice1(_isMatch[(state2 shl ULZMABase.kNumPosStatesBitsMax) + posStateNext]);
                        nextRepMatchPrice := nextMatchPrice + RangeEncoder.GetPrice1(_isRep[state2]);

                        offset := lenTest + 1 + lenTest2;
                        while lenEnd < cur + offset do begin
                              inc(lenEnd);
                              _optimum[lenEnd].Price := kIfinityPrice;
                              end;//while lenEnd
                        curAndLenPrice := nextRepMatchPrice + GetRepPrice(0, lenTest2, state2, posStateNext);
                        optimum := _optimum[cur + offset];
                        if curAndLenPrice < optimum.Price then begin
                           optimum.Price := curAndLenPrice;
                           optimum.PosPrev := cur + lenTest + 1;
                           optimum.BackPrev := 0;
                           optimum.Prev1IsChar := true;
                           optimum.Prev2 := true;
                           optimum.PosPrev2 := cur;
                           optimum.BackPrev2 := curBack + ULZMABase.kNumRepDistances;
                           end;//if curAndLenPrice < optimum.Price
                        end;//if lenTest2 >= 2
                     end;//lenTest < numAvailableBytesFull
                  offs :=offs + 2;
                  if offs = numDistancePairs then
                     break;
                  end;//if lenTest = _matchDistances[offs]
               inc(lenTest);
               end;//while(true)
         end;//if newLen >= startLen
      end;//while (true)
end;

function TLZMAEncoder.ChangePair(const smallDist, bigDist:integer):boolean;
var kDif:integer;
begin
kDif := 7;
result:= (smallDist < (1 shl (32 - kDif))) and (bigDist >= (smallDist shl kDif));
end;

procedure TLZMAEncoder.WriteEndMarker(const posState:integer);
var len,posSlot,lenToPosState,footerBits,posReduced:integer;
begin
if not _writeEndMark then
   exit;

_rangeEncoder.Encode(_isMatch, (_state shl ULZMABase.kNumPosStatesBitsMax) + posState, 1);
_rangeEncoder.Encode(_isRep, _state, 0);
_state := ULZMABase.StateUpdateMatch(_state);
len := ULZMABase.kMatchMinLen;
_lenEncoder.Encode(_rangeEncoder, len - ULZMABase.kMatchMinLen, posState);
posSlot := (1 shl ULZMABase.kNumPosSlotBits) - 1;
lenToPosState := ULZMABase.GetLenToPosState(len);
_posSlotEncoder[lenToPosState].Encode(_rangeEncoder, posSlot);
footerBits := 30;
posReduced := (1 shl footerBits) - 1;
_rangeEncoder.EncodeDirectBits(posReduced shr ULZMABase.kNumAlignBits, footerBits - ULZMABase.kNumAlignBits);
_posAlignEncoder.ReverseEncode(_rangeEncoder, posReduced and ULZMABase.kAlignMask);
end;

procedure TLZMAEncoder.Flush(const nowPos:integer);
begin
ReleaseMFStream;
WriteEndMarker(nowPos and _posStateMask);
_rangeEncoder.FlushData();
_rangeEncoder.FlushStream();
end;

procedure TLZMAEncoder.CodeOneBlock(var inSize,outSize:int64;var finished:boolean);
var progressPosValuePrev:int64;
    posState,len,pos,complexState,distance,i,posSlot,lenToPosState:integer;
    footerBits,baseVal,posReduced:integer;
    curByte,matchByte:byte;
    subcoder:TLZMAEncoder2;
begin
inSize := 0;
outSize := 0;
finished := true;

if _inStream <>nil then begin
   _matchFinder.SetStream(_inStream);
   _matchFinder.Init;
   _needReleaseMFStream := true;
   _inStream := nil;
   end;

if _finished then
   exit;
_finished := true;

progressPosValuePrev := nowPos64;
if nowPos64 = 0 then begin
   if _matchFinder.GetNumAvailableBytes = 0 then begin
      Flush(nowPos64);
      exit;
      end;

   ReadMatchDistances;
   posState := integer(nowPos64) and _posStateMask;
   _rangeEncoder.Encode(_isMatch, (_state shl ULZMABase.kNumPosStatesBitsMax) + posState, 0);
   _state := ULZMABase.StateUpdateChar(_state);
   curByte := _matchFinder.GetIndexByte(0 - _additionalOffset);
   _literalEncoder.GetSubCoder(integer(nowPos64), _previousByte).Encode(_rangeEncoder, curByte);
   _previousByte := curByte;
   dec(_additionalOffset);
   inc(nowPos64);
   end;
if _matchFinder.GetNumAvailableBytes = 0 then begin
   Flush(integer(nowPos64));
   exit;
   end;
while true do begin
      len := GetOptimum(integer(nowPos64));
      pos := backRes;
      posState := integer(nowPos64) and _posStateMask;
      complexState := (_state shl ULZMABase.kNumPosStatesBitsMax) + posState;
      if (len = 1) and (pos = -1) then begin
         _rangeEncoder.Encode(_isMatch, complexState, 0);
         curByte := _matchFinder.GetIndexByte(0 - _additionalOffset);
         subCoder := _literalEncoder.GetSubCoder(integer(nowPos64), _previousByte);
         if not ULZMABase.StateIsCharState(_state) then begin
            matchByte := _matchFinder.GetIndexByte(0 - _repDistances[0] - 1 - _additionalOffset);
            subCoder.EncodeMatched(_rangeEncoder, matchByte, curByte);
            end else subCoder.Encode(_rangeEncoder, curByte);
         _previousByte := curByte;
         _state := ULZMABase.StateUpdateChar(_state);
         end else begin
             _rangeEncoder.Encode(_isMatch, complexState, 1);
             if pos < ULZMABase.kNumRepDistances then begin
                _rangeEncoder.Encode(_isRep, _state, 1);
                if pos = 0 then begin
                   _rangeEncoder.Encode(_isRepG0, _state, 0);
                   if len = 1 then
                      _rangeEncoder.Encode(_isRep0Long, complexState, 0)
                      else _rangeEncoder.Encode(_isRep0Long, complexState, 1);
                   end else begin
                       _rangeEncoder.Encode(_isRepG0, _state, 1);
                       if pos = 1 then
                          _rangeEncoder.Encode(_isRepG1, _state, 0)
                          else begin
                               _rangeEncoder.Encode(_isRepG1, _state, 1);
                               _rangeEncoder.Encode(_isRepG2, _state, pos - 2);
                               end;
                       end;
                if len = 1 then
                   _state := ULZMABase.StateUpdateShortRep(_state)
                   else begin
                        _repMatchLenEncoder.Encode(_rangeEncoder, len - ULZMABase.kMatchMinLen, posState);
                        _state := ULZMABase.StateUpdateRep(_state);
                        end;
                distance := _repDistances[pos];
                if pos <> 0 then begin
                   for i := pos downto 1 do
                       _repDistances[i] := _repDistances[i - 1];
                   _repDistances[0] := distance;
                   end;
                end else begin
                    _rangeEncoder.Encode(_isRep, _state, 0);
                    _state := ULZMABase.StateUpdateMatch(_state);
                    _lenEncoder.Encode(_rangeEncoder, len - ULZMABase.kMatchMinLen, posState);
                    pos := pos - ULZMABase.kNumRepDistances;
                    posSlot := GetPosSlot(pos);
                    lenToPosState := ULZMABase.GetLenToPosState(len);
                    _posSlotEncoder[lenToPosState].Encode(_rangeEncoder, posSlot);

                    if posSlot >= ULZMABase.kStartPosModelIndex then begin
                       footerBits := integer((posSlot shr 1) - 1);
                       baseVal := ((2 or (posSlot and 1)) shl footerBits);
                       posReduced := pos - baseVal;

                       if posSlot < ULZMABase.kEndPosModelIndex then
                          UBitTreeEncoder.ReverseEncode(_posEncoders,
                              baseVal - posSlot - 1, _rangeEncoder, footerBits, posReduced)
                          else begin
                               _rangeEncoder.EncodeDirectBits(posReduced shr ULZMABase.kNumAlignBits, footerBits - ULZMABase.kNumAlignBits);
                               _posAlignEncoder.ReverseEncode(_rangeEncoder, posReduced and ULZMABase.kAlignMask);
                               inc(_alignPriceCount);
                               end;
                       end;
                    distance := pos;
                    for i := ULZMABase.kNumRepDistances - 1 downto 1 do
                        _repDistances[i] := _repDistances[i - 1];
                    _repDistances[0] := distance;
                    inc(_matchPriceCount);
                    end;
             _previousByte := _matchFinder.GetIndexByte(len - 1 - _additionalOffset);
      end;
_additionalOffset := _additionalOffset - len;
nowPos64 := nowPos64 + len;
if _additionalOffset = 0 then begin
   // if (!_fastMode)
   if _matchPriceCount >= (1 shl 7) then
      FillDistancesPrices;
   if _alignPriceCount >= ULZMABase.kAlignTableSize then
      FillAlignPrices;
   inSize := nowPos64;
   outSize := _rangeEncoder.GetProcessedSizeAdd;
   if _matchFinder.GetNumAvailableBytes = 0 then begin
      Flush(integer(nowPos64));
      exit;
      end;

if (nowPos64 - progressPosValuePrev >= (1 shl 12)) then begin
   _finished := false;
   finished := false;
   exit;
   end;
end;
end;
end;

procedure TLZMAEncoder.ReleaseMFStream;
begin
if (_matchFinder <>nil) and _needReleaseMFStream then begin
   _matchFinder.ReleaseStream;
   _needReleaseMFStream := false;
   end;
end;

procedure TLZMAEncoder.SetOutStream(const outStream:TStream);
begin
_rangeEncoder.SetStream(outStream);
end;

procedure TLZMAEncoder.ReleaseOutStream;
begin
_rangeEncoder.ReleaseStream;
end;

procedure TLZMAEncoder.ReleaseStreams;
begin
ReleaseMFStream;
ReleaseOutStream;
end;

procedure TLZMAEncoder.SetStreams(const inStream, outStream:TStream;const inSize, outSize:int64);
begin
_inStream := inStream;
_finished := false;
_Create();
SetOutStream(outStream);
Init();

// if (!_fastMode)
FillDistancesPrices;
FillAlignPrices;

_lenEncoder.SetTableSize(_numFastBytes + 1 - ULZMABase.kMatchMinLen);
_lenEncoder.UpdateTables(1 shl _posStateBits);
_repMatchLenEncoder.SetTableSize(_numFastBytes + 1 - ULZMABase.kMatchMinLen);
_repMatchLenEncoder.UpdateTables(1 shl _posStateBits);

nowPos64 := 0;
end;

procedure TLZMAEncoder.Code(const inStream, outStream:TStream;const inSize, outSize:int64);
var lpos:int64;
    progint:int64;
    inputsize:int64;
begin
if insize=-1 then
   inputsize:=instream.Size-instream.Position
   else inputsize:=insize;
progint:=inputsize div CodeProgressInterval;
lpos:=progint;

_needReleaseMFStream := false;
DoProgress(LPAMax,inputsize);
try
   SetStreams(inStream, outStream, inSize, outSize);
   while true do begin
         CodeOneBlock(processedInSize, processedOutSize, finished);
         if finished then begin
            DoProgress(LPAPos,inputsize);
            exit;
            end;
         if (processedInSize>=lpos) then begin
            DoProgress(LPAPos,processedInSize);
            lpos:=lpos+progint;
            end;
         end;
   finally
     ReleaseStreams();
   end;
end;

procedure TLZMAEncoder.WriteCoderProperties(const outStream:TStream);
var i:integer;
begin
properties[0] := (_posStateBits * 5 + _numLiteralPosStateBits) * 9 + _numLiteralContextBits;
for i := 0 to 3 do
    properties[1 + i] := byte(_dictionarySize shr (8 * i));
outStream.write(properties, kPropSize);
end;

procedure TLZMAEncoder.FillDistancesPrices;
var i,posSlot,footerBits,baseVal,lenToPosState,st,st2:integer;
    encoder:TBitTreeEncoder;
begin
for i := ULZMABase.kStartPosModelIndex to ULZMABase.kNumFullDistances -1 do begin
    posSlot := GetPosSlot(i);
    footerBits := integer((posSlot shr 1) - 1);
    baseVal := (2 or (posSlot and 1)) shl footerBits;
    tempPrices[i] := ReverseGetPrice(_posEncoders,
        baseVal - posSlot - 1, footerBits, i - baseVal);
    end;

for lenToPosState := 0 to ULZMABase.kNumLenToPosStates -1  do begin
    encoder := _posSlotEncoder[lenToPosState];

    st := (lenToPosState shl ULZMABase.kNumPosSlotBits);
    for posSlot := 0 to _distTableSize -1 do
        _posSlotPrices[st + posSlot] := encoder.GetPrice(posSlot);
    for posSlot := ULZMABase.kEndPosModelIndex to _distTableSize -1 do
        _posSlotPrices[st + posSlot] := _posSlotPrices[st + posSlot] + ((((posSlot shr 1) - 1) - ULZMABase.kNumAlignBits) shl kNumBitPriceShiftBits);

    st2 := lenToPosState * ULZMABase.kNumFullDistances;
    for i := 0 to ULZMABase.kStartPosModelIndex -1 do
        _distancesPrices[st2 + i] := _posSlotPrices[st + i];
    for i := ULZMABase.kStartPosModelIndex to ULZMABase.kNumFullDistances-1 do
        _distancesPrices[st2 + i] := _posSlotPrices[st + GetPosSlot(i)] + tempPrices[i];
    end;
_matchPriceCount := 0;
end;

procedure TLZMAEncoder.FillAlignPrices;
var i:integer;
begin
for i := 0 to ULZMABase.kAlignTableSize -1 do
    _alignPrices[i] := _posAlignEncoder.ReverseGetPrice(i);
_alignPriceCount := 0;
end;

function TLZMAEncoder.SetAlgorithm(const algorithm:integer):boolean;
begin
{
    _fastMode = (algorithm == 0);
    _maxMode = (algorithm >= 2);
}
result:=true;
end;

function TLZMAEncoder.SetDictionarySize(dictionarySize:integer):boolean;
var kDicLogSizeMaxCompress,dicLogSize:integer;
begin
kDicLogSizeMaxCompress := 29;
if (dictionarySize < (1 shl ULZMABase.kDicLogSizeMin)) or (dictionarySize > (1 shl kDicLogSizeMaxCompress)) then begin
   result:=false;
   exit;
   end;
_dictionarySize := dictionarySize;
dicLogSize := 0;
while dictionarySize > (1 shl dicLogSize) do
      inc(dicLogSize);
_distTableSize := dicLogSize * 2;
result:=true;
end;

function TLZMAEncoder.SeNumFastBytes(const numFastBytes:integer):boolean;
begin
if (numFastBytes < 5) or (numFastBytes > ULZMABase.kMatchMaxLen) then begin
   result:=false;
   exit;
   end;
_numFastBytes := numFastBytes;
result:=true;
end;

function TLZMAEncoder.SetMatchFinder(const matchFinderIndex:integer):boolean;
var matchFinderIndexPrev:integer;
begin
if (matchFinderIndex < 0) or (matchFinderIndex > 2) then begin
   result:=false;
   exit;
   end;
matchFinderIndexPrev := _matchFinderType;
_matchFinderType := matchFinderIndex;
if (_matchFinder <> nil) and (matchFinderIndexPrev <> _matchFinderType) then begin
   _dictionarySizePrev := -1;
   _matchFinder := nil;
   end;
result:=true;
end;

function TLZMAEncoder.SetLcLpPb(const lc,lp,pb:integer):boolean;
begin
if (lp < 0) or (lp > ULZMABase.kNumLitPosStatesBitsEncodingMax) or
   (lc < 0) or (lc > ULZMABase.kNumLitContextBitsMax) or
   (pb < 0) or (pb > ULZMABase.kNumPosStatesBitsEncodingMax) then begin
   result:=false;
   exit;
   end;
_numLiteralPosStateBits := lp;
_numLiteralContextBits := lc;
_posStateBits := pb;
_posStateMask := ((1) shl _posStateBits) - 1;
result:=true;
end;

procedure TLZMAEncoder.SetEndMarkerMode(const endMarkerMode:boolean);
begin
_writeEndMark := endMarkerMode;
end;

procedure TLZMAEncoder2.Init;
begin
URangeEncoder.InitBitModels(m_Encoders);
end;

procedure TLZMAEncoder2.Encode(const rangeEncoder:TRangeEncoder;const symbol:byte);
var context:integer;
    bit,i:integer;
begin
context := 1;
for i := 7 downto 0 do begin
    bit := ((symbol shr i) and 1);
    rangeEncoder.Encode(m_Encoders, context, bit);
    context := (context shl 1) or bit;
    end;
end;

procedure TLZMAEncoder2.EncodeMatched(const rangeEncoder:TRangeEncoder;const matchByte,symbol:byte);
var context,i,bit,state,matchbit:integer;
    same:boolean;
begin
context := 1;
same := true;
for i := 7 downto 0 do begin
    bit := ((symbol shr i) and 1);
    state := context;
    if same then begin
       matchBit := ((matchByte shr i) and 1);
       state :=state + ((1 + matchBit) shl 8);
       same := (matchBit = bit);
       end;
    rangeEncoder.Encode(m_Encoders, state, bit);
    context := (context shl 1) or bit;
    end;
end;

function TLZMAEncoder2.GetPrice(const matchMode:boolean;const matchByte,symbol:byte):integer;
var price,context,i,matchbit,bit:integer;
begin
price := 0;
context := 1;
i := 7;
if matchMode then
   while i>=0 do begin
         matchBit := (matchByte shr i) and 1;
         bit := (symbol shr i) and 1;
         price := price + RangeEncoder.GetPrice(m_Encoders[((1 + matchBit) shl 8) + context], bit);
         context := (context shl 1) or bit;
         if (matchBit <> bit) then begin
            dec(i);
            break;
            end;
         dec(i);
         end;
while i>=0 do begin
      bit := (symbol shr i) and 1;
      price := price + RangeEncoder.GetPrice(m_Encoders[context], bit);
      context := (context shl 1) or bit;
      dec(i);
      end;
result:=price;
end;

procedure TLZMALiteralEncoder._Create(const numPosBits,numPrevBits:integer);
var numstates:integer;
    i:integer;
begin
if (length(m_Coders)<>0) and (m_NumPrevBits = numPrevBits) and (m_NumPosBits = numPosBits) then
   exit;
m_NumPosBits := numPosBits;
m_PosMask := (1 shl numPosBits) - 1;
m_NumPrevBits := numPrevBits;
numStates := 1 shl (m_NumPrevBits + m_NumPosBits);
setlength(m_coders,numStates);
for i := 0 to numStates-1 do
    m_Coders[i] := TLZMAEncoder2.Create;
end;

destructor TLZMALiteralEncoder.Destroy;
var i:integer;
begin
for i:=low(m_Coders) to high(m_Coders) do
    if m_Coders[i]<>nil then m_Coders[i].Free;
inherited;
end;

procedure TLZMALiteralEncoder.Init;
var numstates,i:integer;
begin
numStates := 1 shl (m_NumPrevBits + m_NumPosBits);
for i := 0 to numStates-1 do
    m_Coders[i].Init;
end;

function TLZMALiteralEncoder.GetSubCoder(const pos:integer;const prevByte:byte):TLZMAEncoder2;
begin
result:=m_Coders[((pos and m_PosMask) shl m_NumPrevBits) + ((prevByte and $FF) shr (8 - m_NumPrevBits))];
end;

constructor TLZMALenEncoder.Create;
var posState:integer;
begin
_highCoder := TBitTreeEncoder.Create(ULZMABase.kNumHighLenBits);
for posState := 0 to ULZMABase.kNumPosStatesEncodingMax-1 do begin
    _lowCoder[posState] := TBitTreeEncoder.Create(ULZMABase.kNumLowLenBits);
    _midCoder[posState] := TBitTreeEncoder.Create(ULZMABase.kNumMidLenBits);
    end;
end;

destructor TLZMALenEncoder.Destroy;
var posState:integer;
begin
_highCoder.Free;
for posState := 0 to ULZMABase.kNumPosStatesEncodingMax-1 do begin
    _lowCoder[posState].Free;
    _midCoder[posState].Free;
    end;
inherited;
end;

procedure TLZMALenEncoder.Init(const numPosStates:integer);
var posState:integer;
begin
URangeEncoder.InitBitModels(_choice);

for posState := 0 to numPosStates -1 do begin
    _lowCoder[posState].Init;
    _midCoder[posState].Init;
    end;
_highCoder.Init;
end;

procedure TLZMALenEncoder.Encode(const rangeEncoder:TRangeEncoder;symbol:integer;const posState:integer);
begin
if (symbol < ULZMABase.kNumLowLenSymbols) then begin
   rangeEncoder.Encode(_choice, 0, 0);
   _lowCoder[posState].Encode(rangeEncoder, symbol);
   end else begin
       symbol := symbol - ULZMABase.kNumLowLenSymbols;
       rangeEncoder.Encode(_choice, 0, 1);
       if symbol < ULZMABase.kNumMidLenSymbols then begin
          rangeEncoder.Encode(_choice, 1, 0);
          _midCoder[posState].Encode(rangeEncoder, symbol);
          end else begin
              rangeEncoder.Encode(_choice, 1, 1);
              _highCoder.Encode(rangeEncoder, symbol - ULZMABase.kNumMidLenSymbols);
              end;
       end;
end;

procedure TLZMALenEncoder.SetPrices(const posState,numSymbols:integer;var prices:array of integer;const st:integer);
var a0,a1,b0,b1,i:integer;
begin
a0 := RangeEncoder.GetPrice0(_choice[0]);
a1 := RangeEncoder.GetPrice1(_choice[0]);
b0 := a1 + RangeEncoder.GetPrice0(_choice[1]);
b1 := a1 + RangeEncoder.GetPrice1(_choice[1]);
i:=0;
while i<ULZMABase.kNumLowLenSymbols do begin
    if i >= numSymbols then
       exit;
    prices[st + i] := a0 + _lowCoder[posState].GetPrice(i);
    inc(i);
    end;
while i < ULZMABase.kNumLowLenSymbols + ULZMABase.kNumMidLenSymbols do begin
      if i >= numSymbols then
         exit;
      prices[st + i] := b0 + _midCoder[posState].GetPrice(i - ULZMABase.kNumLowLenSymbols);
      inc(i);
      end;
while i < numSymbols do begin
      prices[st + i] := b1 + _highCoder.GetPrice(i - ULZMABase.kNumLowLenSymbols - ULZMABase.kNumMidLenSymbols);
      inc(i);
      end;
end;

procedure TLZMALenPriceTableEncoder.SetTableSize(const tableSize:integer);
begin
_tableSize := tableSize;
end;

function TLZMALenPriceTableEncoder.GetPrice(const symbol,posState:integer):integer;
begin
result:=_prices[posState * ULZMABase.kNumLenSymbols + symbol]
end;

procedure TLZMALenPriceTableEncoder.UpdateTable(const posState:integer);
begin
SetPrices(posState, _tableSize, _prices, posState * ULZMABase.kNumLenSymbols);
_counters[posState] := _tableSize;
end;

procedure TLZMALenPriceTableEncoder.UpdateTables(const numPosStates:integer);
var posState:integer;
begin
for posState := 0 to numPosStates -1 do
    UpdateTable(posState);
end;

procedure TLZMALenPriceTableEncoder.Encode(const rangeEncoder:TRangeEncoder;symbol:integer;const posState:integer);
begin
inherited Encode(rangeEncoder, symbol, posState);
dec(_counters[posState]);
if (_counters[posState] = 0) then
   UpdateTable(posState);
end;

procedure TLZMAOptimal.MakeAsChar;
begin
BackPrev := -1;
Prev1IsChar := false;
end;

procedure TLZMAOptimal.MakeAsShortRep;
begin
BackPrev := 0;
Prev1IsChar := false;
end;

function TLZMAOptimal.IsShortRep:boolean;
begin
result:=BackPrev = 0;
end;

procedure TLZMAEncoder.DoProgress(const Action:TLZMAProgressAction;const Value:integer);
begin
if assigned(fonprogress) then
   fonprogress(action,value);
end;

end.
