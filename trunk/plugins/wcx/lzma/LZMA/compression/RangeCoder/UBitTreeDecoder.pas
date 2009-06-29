unit UBitTreeDecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses URangeDecoder;

type TBitTreeDecoder=class
       public
         Models: array of smallint;
         NumBitLevels:integer;
         constructor Create(const numBitLevels:integer);
         procedure Init;
         function Decode(const rangeDecoder:TRangeDecoder):integer;
         function ReverseDecode(const rangeDecoder:TRangeDecoder):integer;overload;
       end;

function ReverseDecode(var Models: array of smallint; const startIndex:integer;const rangeDecoder:TRangeDecoder; const NumBitLevels:integer):integer;overload;

implementation

constructor TBitTreeDecoder.Create(const numBitLevels:integer);
begin
self.NumBitLevels := numBitLevels;
setlength(Models,1 shl numBitLevels);
end;

procedure TBitTreeDecoder.Init;
begin
urangedecoder.InitBitModels(Models);
end;

function TBitTreeDecoder.Decode(const rangeDecoder:TRangeDecoder):integer;
var m,bitIndex:integer;
begin
m:=1;
for bitIndex := NumBitLevels downto 1 do begin
    m:=m shl 1 + rangeDecoder.DecodeBit(Models, m);
    end;
result:=m - (1 shl NumBitLevels);
end;

function TBitTreeDecoder.ReverseDecode(const rangeDecoder:TRangeDecoder):integer;
var m,symbol,bitindex,bit:integer;
begin
m:=1;
symbol:=0;
for bitindex:=0 to numbitlevels-1 do begin
    bit:=rangeDecoder.DecodeBit(Models, m);
    m:=(m shl 1) + bit;
    symbol:=symbol or (bit shl bitIndex);
    end;
result:=symbol;
end;

function ReverseDecode(var Models: array of smallint;const startIndex:integer;
            const rangeDecoder:TRangeDecoder;const NumBitLevels:integer):integer;
var m,symbol,bitindex,bit:integer;
begin
m:=1;
symbol:=0;
for bitindex:=0 to numbitlevels -1 do begin
    bit := rangeDecoder.DecodeBit(Models, startIndex + m);
    m := (m shl 1) + bit;
    symbol := symbol or bit shl bitindex;
    end;
result:=symbol;
end;

end.
