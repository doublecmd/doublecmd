unit MultiModelProber;

interface
uses
  {$I dbg.inc}
	nsCore,
  CustomDetector,
	nsCodingStateMachine;

type
	TMultiModelProber = class (TCustomDetector)
    private
    protected
      mDetectedCharset: eInternalCharsetID;
      mActiveSM: integer;
      mCharsetsCount: integer;
      mSMState: array of eProbingState;
      mCodingSM: array of TnsCodingStateMachine;
      procedure AddCharsetModel(Model: SMModel);
		public
      constructor Create; override;
      destructor Destroy; override;

	 	  function HandleData(aBuf: PChar;  aLen: integer): eProbingState;  override;
		  function GetDetectedCharset: eInternalCharsetID; override;
      procedure Reset; override;
      function EnableCharset(Charset: eInternalCharsetID; NewValue: Boolean): Boolean;
		  {$ifdef DEBUG_chardet}
  		procedure DumpStatus(Dump: string); override;
		  {$endif}
      function GetConfidence: float; override;
  end;


implementation
{$ifdef DEBUG_chardet}
uses
  SysUtils,
  TypInfo;
{$endif}

{ TMultiModelProber }

constructor TMultiModelProber.Create;
begin
  inherited;
  mCharsetsCount := 0;
end;

destructor TMultiModelProber.Destroy;
var
	i: integer;
begin
	for i := 0 to Pred(mCharsetsCount) do
    begin
      mCodingSM[i].Free;
      mCodingSM[i] := nil;
    end;
  SetLength(mCodingSM, 0);
  inherited;
end;

procedure TMultiModelProber.AddCharsetModel(Model: SMModel);
begin
  inc(mCharsetsCount);
  SetLength(mCodingSM, mCharsetsCount);
  SetLength(mSMState, mCharsetsCount);
  mCodingSM[Pred(mCharsetsCount)] := TnsCodingStateMachine.Create(Model);
  mSMState[Pred(mCharsetsCount)] := psDetecting;
  Reset;

end;

function TMultiModelProber.GetDetectedCharset: eInternalCharsetID;
begin
	Result := mDetectedCharset;
end;

function TMultiModelProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
var
  codingState: nsSMState;
  j: integer;
  i: integer;
begin
  {$IFDEF DEBUG_chardet}
   AddDump('Multi Model - HandleData - start');
  {$endif}
  Result := mState;
  for i := 0 to Pred(aLen) do
    begin
//      if mState = psDetecting then
//        break;

      for j := mCharsetsCount - 1 downto 0 do
        begin
          // skip disabled
          if (not mCodingSM[j].Enabled) or
             (mSMState[j] = psNotMe) then
            continue;
          (*byte is feed to all active state machine *)
          codingState := mCodingSM[j].NextState(aBuf[i]);
          if codingState = eError then
            begin
              (*got negative answer for this state machine, make it inactive*)
              mSMState[j] := psNotMe;
              dec(mActiveSM);
              if mActiveSM = 0 then
                begin
                  mState:= psNotMe;
                  Result:= mState;
                  {$IFDEF DEBUG_chardet}
                    DumpStatus('Multi Model - HandleData - NOT ME!');
                  {$endif}
                  exit;
                end;
          	end
          else
            if codingState = eItsMe then
              begin
                {$IFDEF DEBUG_chardet}
                  DumpStatus('Multi Model - HandleData - FOUND IT!');
                {$endif}
                mSMState[j] := psFoundIt;
                mState := psFoundIt;
                mDetectedCharset := mCodingSM[j].GetCharsetID;
                Result:= mState;
                exit;
              end;
        end; // if codingState = eError
    end;

  if mActiveSM = 1 then
    begin
      for i := 0 to Pred(mCharsetsCount) do
        if (mSMState[i] <> psNotMe) then
          begin
            // TODO - set confidience ? or....
            //  signalised that it's not sure
            if GetConfidence > SURE_NO then
              begin
                mSMState[i] := psFoundIt;
                mState := psFoundIt;
                mDetectedCharset := mCodingSM[i].GetCharsetID;
                break;
              end;
          end;
    end
  else
    Result := mState;
  {$IFDEF DEBUG_chardet}
   DumpStatus('Multi Model - HandleData - EXIT.');
  {$endif}
end;

procedure TMultiModelProber.Reset;
var
	i: integer;
begin
  mState:= psDetecting;
  for i := 0 to Pred(mCharsetsCount) do
    begin
  	  mCodingSM[i].Reset;
      if mCodingSM[i].Enabled then
        mSMState[i] := psDetecting
      else
        mSMState[i]  := psNotMe;
    end;
  mActiveSM := mCharsetsCount;
  mDetectedCharset := UNKNOWN_CHARSET;
end;

function TMultiModelProber.EnableCharset(Charset: eInternalCharsetID; NewValue: Boolean): Boolean;
var
  i: integer;
begin
  for i := 0 to Pred(mCharsetsCount) do
    if mCodingSM[i].GetCharsetID = Charset then
      begin
        Result := true;
        mCodingSM[i].Enabled := NewValue;
        exit;
      end;
  Result := false;
end;


function TMultiModelProber.GetConfidence: float;
var
  i: integer;
begin
 	Result := SURE_NO;
  case mState of
    psNotMe:
    	begin
      	Result := SURE_NO;  
        mDetectedCharset := UNKNOWN_CHARSET;
      end;
    else
      for i := 0 to Pred(mCharsetsCount) do
        if (mSMState[i] = psFoundIt) or
           ((mSMState[i] = psDetecting) and (mActiveSM = 1)) then
          begin
            Result := SURE_YES;
            mDetectedCharset := mCodingSM[i].GetCharsetID;
            break;
          end;
  end;{case}
end;

{$ifdef DEBUG_chardet}
procedure TMultiModelProber.DumpStatus(Dump: string);
var
  i: integer;
begin
  inherited DumpStatus(Dump + ' Current state ' + GetEnumName(TypeInfo(eProbingState), integer(mState)));
  inherited DumpStatus(Format('%30s - %5s',
          ['Prober',
           'State'
           ]));
  for i := 0 to Pred(mCharsetsCount) do
    inherited DumpStatus(Format('%30s - %5s',
          [GetEnumName(TypeInfo(eInternalCharsetID), integer(mCodingSM[i].GetCharsetID)),
           GetEnumName(TypeInfo(eProbingState), integer(mSMState[i]))
           ]));

end;
{$endif}

end.

