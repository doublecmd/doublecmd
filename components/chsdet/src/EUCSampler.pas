unit EUCSampler;

interface

const
  FreqSize = 94;
type
  rEUCStatistics = record
    mFirstByteFreq: array[0..Pred(FreqSize)] of Double;
    mFirstByteStdDev,
    mFirstByteMean,
    mFirstByteWeight: Double;
    mSecoundByteFreq: array [0..Pred(FreqSize)] of Double;
    mSecoundByteStdDev,
    mSecoundByteMean,
    mSecoundByteWeight: Double;
  end;
  prEUCStatistics = ^rEUCStatistics;

type
  TEUCSampler = class (TObject)
    private
      mTotal,
      mThreshold,
      mState: integer;
      mFirstByteCnt: array [0..Pred(FreqSize)] of integer;
      mSecondByteCnt: array [0..Pred(FreqSize)] of integer;
      mFirstByteFreq: array [0..Pred(FreqSize)] of double;
      mSecondByteFreq: array [0..Pred(FreqSize)] of double;
    public
      constructor Create;
      destructor Destroy; override;

      function Sample(aIn: pChar; aLen: integer): Boolean;
      function GetSomeData: Boolean;
      function EnoughData: Boolean;
      procedure CalFreq;
      function GetScore(const aFirstByteFreq: array of Double; aFirstByteWeight: Double;
                     const aSecondByteFreq: array of Double; aSecondByteWeight: Double): double; overload; virtual;
      function GetScore(const array1, array2: array of Double): double; overload; virtual;
      procedure Reset;
end;

implementation

{ TEUCSampler }

procedure TEUCSampler.CalFreq;
var
  i: integer;
begin
   for i := 0 to Pred(FreqSize) do
   begin
      mFirstByteFreq[i] := mFirstByteCnt[i] / mTotal;
      mSecondByteFreq[i] := mSecondByteCnt[i] / mTotal;
   end;
end;

constructor TEUCSampler.Create;
begin
  inherited;
  Reset;
end;

destructor TEUCSampler.Destroy;
begin

  inherited;
end;

function TEUCSampler.EnoughData: Boolean;
begin
  Result := mTotal > mThreshold; 
end;

function TEUCSampler.GetScore(const aFirstByteFreq: array of Double; aFirstByteWeight: Double;
     const aSecondByteFreq: array of Double; aSecondByteWeight: Double): double;
begin
   Result := aFirstByteWeight * GetScore(aFirstByteFreq, mFirstByteFreq) +
          aSecondByteWeight * GetScore(aSecondByteFreq, mSecondByteFreq);
end;

function TEUCSampler.GetScore(const array1, array2: array of Double): double;
var
   s,
   sum: Double;
   i: integer;
begin
   sum := 0.0;
   for i := 0 to Pred(FreqSize) do
   begin
     s := array1[i] - array2[i];
     sum := sum + s * s;
   end;
   Result := sqrt(sum) / (FreqSize*1.0);
end;

function TEUCSampler.GetSomeData: Boolean;
begin
  Result := mTotal > 1;
end;

procedure TEUCSampler.Reset;
var
  i: integer;
begin
  mTotal := 0;
  mThreshold := 200;
  mState := 0;
  for i := 0 to Pred(FreqSize) do
    begin
      mFirstByteCnt[i] := 0;
      mSecondByteCnt[i] := 0;
    end;
end;

function TEUCSampler.Sample(aIn: pChar; aLen: integer): Boolean;
const
  MAX_LENGTH: integer = MaxInt;// $80000000;  
var
  i: integer;
  p: pChar;
begin
    if (mState = 1) then
      begin
        Result := FALSE;
        exit;
      end;

    p := aIn;
    if (aLen + mTotal > MAX_LENGTH) then
       aLen := MAX_LENGTH - mTotal;

     i := 0;
     while (i < aLen) and (mState <> 1) do
     begin
        case mState of
           0:
             if (byte(p^) and $0080) > 0 then
             begin
                if (byte(p^) = $00ff) or ( byte(p^) < $00a1) then
                   mState := 1
                else
                  begin
                   Inc(mTotal);
                   Inc(mFirstByteCnt[byte(p^) - $00a1]);
                   mState := 2;
                  end;
             end;
           1:
             begin
             end;
           2:
             if ( byte(p^) and $0080) > 0 then
             begin
                if(byte(p^) = $00ff) or (byte(p^) < $00a1) then
                   mState := 1
                else
                begin
                   Inc(mTotal);
                   Inc(mSecondByteCnt[byte(p^) - $00a1]);
                   mState := 0;
                end;
             end
             else
                mState := 1;

           else
             mState := 1;
        end;
      Inc(i);
      Inc(p);

     end;
   Result := ( mState <> 1 );
end;

end.
