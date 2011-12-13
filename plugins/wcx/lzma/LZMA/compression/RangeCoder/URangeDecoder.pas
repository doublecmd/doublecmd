unit URangeDecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes,ULZMACommon;

type TRangeDecoder=class
       public
         Range,Code:integer;
         Stream:TStream;
         procedure SetStream(const Stream:TStream);
         procedure ReleaseStream;
         procedure Init;
         function DecodeDirectBits(const numTotalBits:integer):integer;
         function DecodeBit(var probs: array of smallint;const index:integer):integer;
       end;

procedure InitBitModels(var probs: array of smallint);

implementation

const kTopMask = not ((1 shl 24) - 1);
      kNumBitModelTotalBits = 11;
      kBitModelTotal = (1 shl kNumBitModelTotalBits);
      kNumMoveBits = 5;

procedure TRangeDecoder.SetStream(const Stream:TStream);
begin
self.Stream:=Stream;
end;

procedure TRangeDecoder.ReleaseStream;
begin
stream:=nil;
end;

procedure TRangeDecoder.Init;
var i:integer;
begin
code:=0;
Range:=-1;
for i:=0 to 4 do begin
    code:=(code shl 8) or byte(ReadByte(stream));
    end;
end;

function TRangeDecoder.DecodeDirectBits(const numTotalBits:integer):integer;
var i,t:integer;
begin
result:=0;
for i := numTotalBits downto 1 do begin
    range:=range shr 1;
    t := (cardinal(Code - Range) shr 31);
    Code := integer(Code - Range and (t - 1));
    result := (result shl 1) or (1 - t);
    if ((Range and kTopMask) = 0) then begin
       Code := (Code shl 8) or ReadByte(stream);
       Range := Range shl 8;
       end;
    end;
end;

function TRangeDecoder.DecodeBit(var probs: array of smallint;const index:integer):integer;
var prob,newbound:integer;
begin
prob:=probs[index];
newbound:= integer((Range shr kNumBitModelTotalBits) * prob);
if (integer((integer(Code) xor integer($80000000))) < integer((integer(newBound) xor integer($80000000)))) then begin
   Range := newBound;
   probs[index] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
   if ((Range and kTopMask) = 0) then begin
      Code := (Code shl 8) or ReadByte(stream);
      Range := Range shl 8;
      end;
   result:=0;
   end else begin
       Range := integer(Range - newBound);
       Code := integer(Code - newBound);
       probs[index] := (prob - ((prob) shr kNumMoveBits));
       if ((Range and kTopMask) = 0) then begin
          Code := (Code shl 8) or ReadByte(stream);
          Range := Range shl  8;
          end;
       result:=1;
       end;
end;

procedure InitBitModels(var probs: array of smallint);
var i:integer;
begin
for i:=0 to length(probs)-1 do
    probs[i] := kBitModelTotal shr 1;
end;

end.
