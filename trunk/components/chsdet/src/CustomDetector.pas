unit CustomDetector;

interface
uses
  {$I dbg.inc}
	nscore;


type
	TCustomDetector = class(TObject)
  	protected
      mState: eProbingState;
      mEnabled: Boolean;

//      procedure Init; virtual; abstract;
      procedure SetEnabled(NewValue: Boolean); virtual;
      function  GetEnabled: Boolean; virtual;
		public
      constructor Create; virtual;
		  function GetDetectedCharset: eInternalCharsetID; virtual; abstract;
		  function HandleData(aBuf: PChar;  aLen: integer): eProbingState; virtual;
		  function GetState: eProbingState; virtual;
      function GetConfidence: float; virtual; abstract;
			procedure Reset; virtual;
		  {$ifdef DEBUG_chardet}
  		procedure DumpStatus(Dump: string); virtual;
		  {$endif}

      property Enabled: Boolean read GetEnabled write SetEnabled;
end;

implementation
{ TCustomDetector }

constructor TCustomDetector.Create;
begin
  mEnabled := true;
end;

function TCustomDetector.GetEnabled: Boolean;
begin
  Result := mEnabled;
end;

procedure TCustomDetector.SetEnabled(NewValue: Boolean);
begin
  mEnabled := NewValue;
end;

function TCustomDetector.GetState: eProbingState;
begin
	Result := mState;
end;

function TCustomDetector.HandleData(aBuf: PChar; aLen: integer): eProbingState;
begin
  if not mEnabled then
    begin
      mState := psNotMe;
    end;
  Result := mState;
end;

procedure TCustomDetector.Reset;
begin
  mState := psDetecting;
end;

{$ifdef DEBUG_chardet}
procedure TCustomDetector.DumpStatus(Dump: string);
begin
  addDump(Dump);
end;
{$endif}

end.




