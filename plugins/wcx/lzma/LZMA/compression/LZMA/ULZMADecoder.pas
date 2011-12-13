unit ULZMADecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses ULZMABase,UBitTreeDecoder,ULZOutWindow,URangeDecoder,Math,Classes,ULZMACommon;

type TLZMALenDecoder = class;
     TLZMALiteralDecoder = class;

     TLZMADecoder = class
       private
         FOnProgress:TLZMAProgress;
         procedure DoProgress(const Action:TLZMAProgressAction;const Value:integer);
       public
         m_OutWindow:TLZOutWindow;
         m_RangeDecoder:TRangeDecoder;

         m_IsMatchDecoders: array [0..ULZMABase.kNumStates shl ULZMABase.kNumPosStatesBitsMax-1] of smallint;
         m_IsRepDecoders: array [0..ULZMABase.kNumStates-1] of smallint;
         m_IsRepG0Decoders: array [0..ULZMABase.kNumStates-1] of smallint;
         m_IsRepG1Decoders: array [0..ULZMABase.kNumStates-1] of smallint;
         m_IsRepG2Decoders: array [0..ULZMABase.kNumStates-1] of smallint;
         m_IsRep0LongDecoders: array [0..ULZMABase.kNumStates shl ULZMABase.kNumPosStatesBitsMax-1] of smallint;

         m_PosSlotDecoder: array [0..ULZMABase.kNumLenToPosStates-1] of TBitTreeDecoder;
         m_PosDecoders: array [0..ULZMABase.kNumFullDistances - ULZMABase.kEndPosModelIndex-1] of smallint;

         m_PosAlignDecoder:TBitTreeDecoder;

         m_LenDecoder:TLZMALenDecoder;
         m_RepLenDecoder:TLZMALenDecoder;

         m_LiteralDecoder:TLZMALiteralDecoder;

         m_DictionarySize:integer;
         m_DictionarySizeCheck:integer;

         m_PosStateMask:integer;

         constructor Create;
         destructor Destroy;override;
         function SetDictionarySize(const dictionarySize:integer):boolean;
         function SetLcLpPb(const lc,lp,pb:integer):boolean;
         procedure Init;
         function Code(const inStream,outStream:TStream;outSize:int64):boolean;
         function SetDecoderProperties(const properties:array of byte):boolean;
         property OnProgress:TLZMAProgress read FOnProgress write FOnProgress;
       end;

     TLZMALenDecoder = class
       public
         m_Choice:array [0..1] of smallint;
         m_LowCoder: array[0..ULZMABase.kNumPosStatesMax-1] of TBitTreeDecoder;
         m_MidCoder: array[0..ULZMABase.kNumPosStatesMax-1] of TBitTreeDecoder;
         m_HighCoder: TBitTreeDecoder;
         m_NumPosStates:integer;
         constructor Create;
         destructor Destroy;override; 
         procedure _Create(const numPosStates:integer);
         procedure Init;
         function Decode(const rangeDecoder:TRangeDecoder;const posState:integer):integer;
       end;

     TLZMADecoder2 = class
       public
         m_Decoders: array [0..$300-1] of smallint;
         procedure Init;
         function DecodeNormal(const rangeDecoder:TRangeDecoder):byte;
         function DecodeWithMatchByte(const rangeDecoder:TRangeDecoder;matchByte:byte):byte;
       end;

     TLZMALiteralDecoder = class
       public
         m_Coders: array of TLZMADecoder2;
         m_NumPrevBits:integer;
         m_NumPosBits:integer;
         m_PosMask:integer;
         procedure _Create(const numPosBits, numPrevBits:integer);
         procedure Init;
         function GetDecoder(const pos:integer;const prevByte:byte):TLZMADecoder2;
         destructor Destroy;override;
       end;

implementation

constructor TLZMALenDecoder.Create;
begin
m_HighCoder:=TBitTreeDecoder.Create(ULZMABase.kNumHighLenBits);
m_NumPosStates:=0;
end;

destructor TLZMALenDecoder.Destroy;
var i:integer;
begin
m_HighCoder.free;
for i:=low(m_LowCoder) to high(m_LowCoder) do begin
    if m_LowCoder[i]<>nil then m_LowCoder[i].free;
    if m_MidCoder[i]<>nil then m_MidCoder[i].free;
    end;
inherited;
end;

procedure TLZMALenDecoder._Create(const numPosStates:integer);
begin
while m_NumPosStates < numPosStates do begin
      m_LowCoder[m_NumPosStates] := TBitTreeDecoder.Create(ULZMABase.kNumLowLenBits);
      m_MidCoder[m_NumPosStates] := TBitTreeDecoder.Create(ULZMABase.kNumMidLenBits);
      inc(m_NumPosStates);
      end;
end;

procedure TLZMALenDecoder.Init;
var posState:integer;
begin
URangeDecoder.InitBitModels(m_Choice);
for posState := 0 to m_NumPosStates-1 do begin
    m_LowCoder[posState].Init;
    m_MidCoder[posState].Init;
    end;
m_HighCoder.Init;
end;

function TLZMALenDecoder.Decode(const rangeDecoder:TRangeDecoder;const posState:integer):integer;
var symbol:integer;
begin
if (rangeDecoder.DecodeBit(m_Choice, 0) = 0) then begin
   result:=m_LowCoder[posState].Decode(rangeDecoder);
   exit;
   end;
symbol := ULZMABase.kNumLowLenSymbols;
if (rangeDecoder.DecodeBit(m_Choice, 1) = 0) then
   symbol := symbol + m_MidCoder[posState].Decode(rangeDecoder)
   else symbol := symbol + ULZMABase.kNumMidLenSymbols + m_HighCoder.Decode(rangeDecoder);
result:=symbol;
end;

procedure TLZMADecoder2.Init;
begin
URangeDecoder.InitBitModels(m_Decoders);
end;

function TLZMADecoder2.DecodeNormal(const rangeDecoder:TRangeDecoder):byte;
var symbol:integer;
begin
symbol := 1;
repeat
  symbol := (symbol shl 1) or rangeDecoder.DecodeBit(m_Decoders, symbol);
  until not (symbol < $100);
result:= byte(symbol);
end;

function TLZMADecoder2.DecodeWithMatchByte(const rangeDecoder:TRangeDecoder;matchByte:byte):byte;
var symbol:integer;
    matchbit:integer;
    bit:integer;
begin
symbol := 1;
repeat
  matchBit := (matchByte shr 7) and 1;
  matchByte := byte(matchByte shl 1);
  bit := rangeDecoder.DecodeBit(m_Decoders, ((1 + matchBit) shl 8) + symbol);
  symbol := (symbol shl 1) or bit;
  if (matchBit <> bit) then begin
     while (symbol < $100) do begin
           symbol := (symbol shl 1) or rangeDecoder.DecodeBit(m_Decoders, symbol);
           end;
     break;
     end;
  until not (symbol < $100);
result:= byte(symbol);
end;

procedure TLZMALiteralDecoder._Create(const numPosBits, numPrevBits:integer);
var numStates,i:integer;
begin
if (length(m_Coders) <> 0) and (m_NumPrevBits = numPrevBits) and (m_NumPosBits = numPosBits) then
   exit;
m_NumPosBits := numPosBits;
m_PosMask := (1 shl numPosBits) - 1;
m_NumPrevBits := numPrevBits;
numStates := 1 shl (m_NumPrevBits + m_NumPosBits);
setlength(m_Coders,numStates);
for i :=0 to numStates-1 do
    m_Coders[i] := TLZMADecoder2.Create;
end;

destructor TLZMALiteralDecoder.Destroy;
var i:integer;
begin
for i :=low(m_Coders) to high(m_Coders) do
    if m_Coders[i]<>nil then m_Coders[i].Free;
inherited;
end;

procedure TLZMALiteralDecoder.Init;
var numStates,i:integer;
begin
numStates := 1 shl (m_NumPrevBits + m_NumPosBits);
for i := 0 to numStates -1 do
    m_Coders[i].Init;
end;

function TLZMALiteralDecoder.GetDecoder(const pos:integer;const prevByte:byte):TLZMADecoder2;
begin
result:=m_Coders[((pos and m_PosMask) shl m_NumPrevBits) + ((prevByte and $FF) shr (8 - m_NumPrevBits))];
end;

constructor TLZMADecoder.Create;
var i:integer;
begin
FOnProgress:=nil;
m_OutWindow:=TLZOutWindow.Create;
m_RangeDecoder:=TRangeDecoder.Create;
m_PosAlignDecoder:=TBitTreeDecoder.Create(ULZMABase.kNumAlignBits);
m_LenDecoder:=TLZMALenDecoder.Create;
m_RepLenDecoder:=TLZMALenDecoder.Create;
m_LiteralDecoder:=TLZMALiteralDecoder.Create;
m_DictionarySize:= -1;
m_DictionarySizeCheck:=  -1;
for i := 0 to ULZMABase.kNumLenToPosStates -1 do
    m_PosSlotDecoder[i] :=TBitTreeDecoder.Create(ULZMABase.kNumPosSlotBits);
end;

destructor TLZMADecoder.Destroy;
var i:integer;
begin
m_OutWindow.Free;
m_RangeDecoder.Free;
m_PosAlignDecoder.Free;
m_LenDecoder.Free;
m_RepLenDecoder.Free;
m_LiteralDecoder.Free;
for i := 0 to ULZMABase.kNumLenToPosStates -1 do
    m_PosSlotDecoder[i].Free;
end;

function TLZMADecoder.SetDictionarySize(const dictionarySize:integer):boolean;
begin
if dictionarySize < 0 then
   result:=false
   else begin
        if m_DictionarySize <> dictionarySize then begin
           m_DictionarySize := dictionarySize;
           m_DictionarySizeCheck := max(m_DictionarySize, 1);
           m_OutWindow._Create(max(m_DictionarySizeCheck, (1 shl 12)));
           end;
        result:=true;
        end;
end;

function TLZMADecoder.SetLcLpPb(const lc,lp,pb:integer):boolean;
var numPosStates:integer;
begin
if (lc > ULZMABase.kNumLitContextBitsMax) or (lp > 4) or (pb > ULZMABase.kNumPosStatesBitsMax) then begin
   result:=false;
   exit;
   end;
m_LiteralDecoder._Create(lp, lc);
numPosStates := 1 shl pb;
m_LenDecoder._Create(numPosStates);
m_RepLenDecoder._Create(numPosStates);
m_PosStateMask := numPosStates - 1;
result:=true;
end;

procedure TLZMADecoder.Init;
var i:integer;
begin
m_OutWindow.Init(false);

URangeDecoder.InitBitModels(m_IsMatchDecoders);
URangeDecoder.InitBitModels(m_IsRep0LongDecoders);
URangeDecoder.InitBitModels(m_IsRepDecoders);
URangeDecoder.InitBitModels(m_IsRepG0Decoders);
URangeDecoder.InitBitModels(m_IsRepG1Decoders);
URangeDecoder.InitBitModels(m_IsRepG2Decoders);
URangeDecoder.InitBitModels(m_PosDecoders);

m_LiteralDecoder.Init();
for i := 0 to ULZMABase.kNumLenToPosStates -1 do
    m_PosSlotDecoder[i].Init;
m_LenDecoder.Init;
m_RepLenDecoder.Init;
m_PosAlignDecoder.Init;
m_RangeDecoder.Init;
end;

function TLZMADecoder.Code(const inStream,outStream:TStream;outSize:int64):boolean;
var state,rep0,rep1,rep2,rep3:integer;
    nowPos64:int64;
    prevByte:byte;
    posState:integer;
    decoder2:TLZMADecoder2;
    len,distance,posSlot,numDirectBits:integer;
    lpos:int64;
    progint:int64;
begin
DoProgress(LPAMax,outSize);
m_RangeDecoder.SetStream(inStream);
m_OutWindow.SetStream(outStream);
Init;

state := ULZMABase.StateInit;
rep0 := 0; rep1 := 0; rep2 := 0; rep3 := 0;

nowPos64 := 0;
prevByte := 0;
progint:=outsize div CodeProgressInterval;
lpos:=progint;
while (outSize < 0) or (nowPos64 < outSize) do begin
      if (nowPos64 >=lpos) then begin
         DoProgress(LPAPos,nowPos64);
         lpos:=lpos+progint;
         end;
      posState := nowPos64 and m_PosStateMask;
      if (m_RangeDecoder.DecodeBit(m_IsMatchDecoders, (state shl ULZMABase.kNumPosStatesBitsMax) + posState) = 0) then begin
         decoder2 := m_LiteralDecoder.GetDecoder(nowPos64, prevByte);
         if not ULZMABase.StateIsCharState(state) then
            prevByte := decoder2.DecodeWithMatchByte(m_RangeDecoder, m_OutWindow.GetByte(rep0))
            else prevByte := decoder2.DecodeNormal(m_RangeDecoder);
         m_OutWindow.PutByte(prevByte);
         state := ULZMABase.StateUpdateChar(state);
         inc(nowPos64);
         end else begin
             if (m_RangeDecoder.DecodeBit(m_IsRepDecoders, state) = 1) then begin
                len := 0;
                if (m_RangeDecoder.DecodeBit(m_IsRepG0Decoders, state) = 0) then begin
                   if (m_RangeDecoder.DecodeBit(m_IsRep0LongDecoders, (state shl ULZMABase.kNumPosStatesBitsMax) + posState) = 0) then begin
                      state := ULZMABase.StateUpdateShortRep(state);
                      len := 1;
                      end;
                   end else begin
                       if m_RangeDecoder.DecodeBit(m_IsRepG1Decoders, state) = 0 then
                          distance := rep1
                       else begin
                            if (m_RangeDecoder.DecodeBit(m_IsRepG2Decoders, state) = 0) then
                               distance := rep2
                            else begin
                                 distance := rep3;
                                 rep3 := rep2;
                                 end;
                            rep2 := rep1;
                            end;
                       rep1 := rep0;
                       rep0 := distance;
                       end;
                if len = 0 then begin
                   len := m_RepLenDecoder.Decode(m_RangeDecoder, posState) + ULZMABase.kMatchMinLen;
                   state := ULZMABase.StateUpdateRep(state);
                   end;
                end else begin
                    rep3 := rep2;
                    rep2 := rep1;
                    rep1 := rep0;
                    len := ULZMABase.kMatchMinLen + m_LenDecoder.Decode(m_RangeDecoder, posState);
                    state := ULZMABase.StateUpdateMatch(state);
                    posSlot := m_PosSlotDecoder[ULZMABase.GetLenToPosState(len)].Decode(m_RangeDecoder);
                    if posSlot >= ULZMABase.kStartPosModelIndex then begin
                       numDirectBits := (posSlot shr 1) - 1;
                       rep0 := ((2 or (posSlot and 1)) shl numDirectBits);
                       if posSlot < ULZMABase.kEndPosModelIndex then
                          rep0 := rep0 + UBitTreeDecoder.ReverseDecode(m_PosDecoders,
                                   rep0 - posSlot - 1, m_RangeDecoder, numDirectBits)
                          else begin
                               rep0 := rep0 + (m_RangeDecoder.DecodeDirectBits(
                                        numDirectBits - ULZMABase.kNumAlignBits) shl ULZMABase.kNumAlignBits);
                               rep0 := rep0 + m_PosAlignDecoder.ReverseDecode(m_RangeDecoder);
                               if rep0 < 0 then begin
                                  if rep0 = -1 then
                                     break;
                                  result:=false;
                                  exit;
                                  end;
                               end;
                       end else rep0 := posSlot;
                    end;
      if (rep0 >= nowPos64) or (rep0 >= m_DictionarySizeCheck) then begin
         m_OutWindow.Flush();
         result:=false;
         exit;
         end;
      m_OutWindow.CopyBlock(rep0, len);
      nowPos64 := nowPos64 + len;
      prevByte := m_OutWindow.GetByte(0);
      end;
end;
m_OutWindow.Flush();
m_OutWindow.ReleaseStream();
m_RangeDecoder.ReleaseStream();
DoProgress(LPAPos,nowPos64);
result:=true;
end;

function TLZMADecoder.SetDecoderProperties(const properties:array of byte):boolean;
var val,lc,remainder,lp,pb,dictionarysize,i:integer;
begin
if length(properties) < 5 then begin
   result:=false;
   exit;
   end;
val := properties[0] and $FF;
lc := val mod 9;
remainder := val div 9;
lp := remainder mod 5;
pb := remainder div 5;
dictionarySize := 0;
for i := 0 to 3 do
    dictionarySize := dictionarysize + ((properties[1 + i]) and $FF) shl (i * 8);
    if (not SetLcLpPb(lc, lp, pb)) then begin
       result:=false;
       exit;
       end;
result:=SetDictionarySize(dictionarySize);
end;

procedure TLZMADecoder.DoProgress(const Action:TLZMAProgressAction;const Value:integer);
begin
if assigned(fonprogress) then
   fonprogress(action,value);
end;

end.
