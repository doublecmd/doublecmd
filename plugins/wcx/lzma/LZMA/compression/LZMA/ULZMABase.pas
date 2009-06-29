unit ULZMABase;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

function StateInit:integer;
function StateUpdateChar(const index:integer):integer;
function StateUpdateMatch(const index:integer):integer;
function StateUpdateRep(const index:integer):integer;
function StateUpdateShortRep(const index:integer):integer;
function StateIsCharState(const index:integer):boolean;
function GetLenToPosState(len:integer):integer;

const kNumRepDistances = 4;
      kNumStates = 12;
      kNumPosSlotBits = 6;
      kDicLogSizeMin = 0;
//    kDicLogSizeMax = 28;
//    kDistTableSizeMax = kDicLogSizeMax * 2;

      kNumLenToPosStatesBits = 2; // it's for speed optimization
      kNumLenToPosStates = 1 shl kNumLenToPosStatesBits;

      kMatchMinLen = 2;

      kNumAlignBits = 4;
      kAlignTableSize = 1 shl kNumAlignBits;
      kAlignMask = (kAlignTableSize - 1);

      kStartPosModelIndex = 4;
      kEndPosModelIndex = 14;
      kNumPosModels = kEndPosModelIndex - kStartPosModelIndex;

      kNumFullDistances = 1 shl (kEndPosModelIndex div 2);

      kNumLitPosStatesBitsEncodingMax = 4;
      kNumLitContextBitsMax = 8;

      kNumPosStatesBitsMax = 4;
      kNumPosStatesMax = (1 shl kNumPosStatesBitsMax);
      kNumPosStatesBitsEncodingMax = 4;
      kNumPosStatesEncodingMax = (1 shl kNumPosStatesBitsEncodingMax);

      kNumLowLenBits = 3;
      kNumMidLenBits = 3;
      kNumHighLenBits = 8;
      kNumLowLenSymbols = 1 shl kNumLowLenBits;
      kNumMidLenSymbols = 1 shl kNumMidLenBits;
      kNumLenSymbols = kNumLowLenSymbols + kNumMidLenSymbols + (1 shl kNumHighLenBits);
      kMatchMaxLen = kMatchMinLen + kNumLenSymbols - 1;

implementation

function StateInit:integer;
begin
result:=0;
end;

function StateUpdateChar(const index:integer):integer;
begin
if (index < 4) then
   result:=0
   else
if (index < 10) then
   result:=index - 3
   else
   result:=index - 6;
end;

function StateUpdateMatch(const index:integer):integer;
begin
if index<7 then result:=7
   else result:=10;
end;

function StateUpdateRep(const index:integer):integer;
begin
if index<7 then result:=8
   else result:=11;
end;

function StateUpdateShortRep(const index:integer):integer;
begin
if index<7 then result:=9
   else result:=11;
end;

function StateIsCharState(const index:integer):boolean;
begin
result:=index<7;
end;

function GetLenToPosState(len:integer):integer;
begin
len := len - kMatchMinLen;
if (len < kNumLenToPosStates) then
   result:=len
   else result:=(kNumLenToPosStates - 1);
end;

end.
