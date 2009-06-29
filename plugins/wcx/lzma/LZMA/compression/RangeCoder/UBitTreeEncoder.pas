unit UBitTreeEncoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses URangeDecoder,URangeEncoder;

type TBitTreeEncoder=class
       public
         Models: array of smallint;
         NumBitLevels:integer;
         constructor Create(const numBitLevels:integer);
         procedure Init;
         procedure Encode(const rangeEncoder:TRangeEncoder;const symbol:integer);
         procedure ReverseEncode(const rangeEncoder:TRangeEncoder;symbol:integer);
         function GetPrice(const symbol:integer):integer;
         function ReverseGetPrice(symbol:integer):integer;overload;
       end;

procedure ReverseEncode(var Models:array of smallint;const startIndex:integer;const rangeEncoder:TRangeEncoder;const NumBitLevels:integer; symbol:integer);
function ReverseGetPrice(var Models:array of smallint;const startIndex,NumBitLevels:integer; symbol:integer):integer;

implementation

constructor TBitTreeEncoder.Create(const numBitLevels:integer);
begin
self.NumBitLevels:=numBitLevels;
setlength(Models,1 shl numBitLevels);
end;

procedure TBitTreeEncoder.Init;
begin
URangeDecoder.InitBitModels(Models);
end;

procedure TBitTreeEncoder.Encode(const rangeEncoder:TRangeEncoder;const symbol:integer);
var m,bitindex,bit:integer;
begin
m := 1;
for bitIndex := NumBitLevels -1 downto 0 do begin
    bit := (symbol shr bitIndex) and 1;
    rangeEncoder.Encode(Models, m, bit);
    m := (m shl 1) or bit;
    end;
end;

procedure TBitTreeEncoder.ReverseEncode(const rangeEncoder:TRangeEncoder;symbol:integer);
var m,i,bit:integer;
begin
m:=1;
for i:= 0 to NumBitLevels -1 do begin
    bit := symbol and 1;
    rangeEncoder.Encode(Models, m, bit);
    m := (m shl 1) or bit;
    symbol := symbol shr 1;
    end;
end;

function TBitTreeEncoder.GetPrice(const symbol:integer):integer;
var price,m,bitindex,bit:integer;
begin
price := 0;
m := 1;
for bitIndex := NumBitLevels - 1 downto 0 do begin
    bit := (symbol shr bitIndex) and 1;
    price := price + RangeEncoder.GetPrice(Models[m], bit);
    m := (m shl 1) + bit;
    end;
result:=price;
end;

function TBitTreeEncoder.ReverseGetPrice(symbol:integer):integer;
var price,m,i,bit:integer;
begin
price := 0;
m := 1;
for i:= NumBitLevels downto 1 do begin
    bit := symbol and 1;
    symbol := symbol shr 1;
    price :=price + RangeEncoder.GetPrice(Models[m], bit);
    m := (m shl 1) or bit;
    end;
result:=price;
end;

function ReverseGetPrice(var Models:array of smallint;const startIndex,NumBitLevels:integer;symbol:integer):integer;
var price,m,i,bit:integer;
begin
price := 0;
m := 1;
for i := NumBitLevels downto 1 do begin
    bit := symbol and 1;
    symbol := symbol shr 1;
    price := price + RangeEncoder.GetPrice(Models[startIndex + m], bit);
    m := (m shl 1) or bit;
    end;
result:=price;
end;

procedure ReverseEncode(var Models:array of smallint;const startIndex:integer;const rangeEncoder:TRangeEncoder;const NumBitLevels:integer;symbol:integer);
var m,i,bit:integer;
begin
m:=1;
for i := 0 to NumBitLevels -1 do begin
    bit := symbol and 1;
    rangeEncoder.Encode(Models, startIndex + m, bit);
    m := (m shl 1) or bit;
    symbol := symbol shr 1;
    end;
end;

end.
