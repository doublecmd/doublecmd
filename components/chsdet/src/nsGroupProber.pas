// +----------------------------------------------------------------------+
// |    chsdet - Charset Detector Library                                 |
// +----------------------------------------------------------------------+
// | Copyright (C) 2006, Nick Yakowlew     http://chsdet.sourceforge.net  |
// +----------------------------------------------------------------------+
// | Based on Mozilla sources     http://www.mozilla.org/projects/intl/   |
// +----------------------------------------------------------------------+
// | This library is free software; you can redistribute it and/or modify |
// | it under the terms of the GNU General Public License as published by |
// | the Free Software Foundation; either version 2 of the License, or    |
// | (at your option) any later version.                                  |
// | This library is distributed in the hope that it will be useful       |
// | but WITHOUT ANY WARRANTY; without even the implied warranty of       |
// | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 |
// | See the GNU Lesser General Public License for more details.          |
// | http://www.opensource.org/licenses/lgpl-license.php                  |
// +----------------------------------------------------------------------+
//
// $Id: nsGroupProber.pas,v 1.2 2007/05/26 13:09:38 ya_nick Exp $

unit nsGroupProber;

interface
uses
  {$I dbg.inc}
	nsCore,
  CustomDetector;


type
	TnsGroupProber = class(TCustomDetector)
		protected
    	mNumOfProbers: integer;
      mProbers: array of TCustomDetector;
      mIsActive: array of Boolean;
      mBestGuess: integer;
      mActiveNum: integer;
      mProberStates: array of eProbingState;

		public
      constructor Create; override;
      destructor Destroy; override;
      procedure Reset; override;
      function HandleData(aBuf: PChar;  aLen: integer): eProbingState; override;
      function GetDetectedCharset: eInternalCharsetID; override;
      function GetConfidence: float; override;
      function EnableCharset(Charset: eInternalCharsetID; NewValue: Boolean): Boolean;
		  {$ifdef DEBUG_chardet}
  		procedure DumpStatus(Dump: string); override;
		  {$endif}
  end;

implementation
uses
	SysUtils
  {$ifdef DEBUG_chardet}
  ,TypInfo
  {$endif}
  ;

{ TnsGroupProber }

constructor TnsGroupProber.Create;
begin
  inherited Create;

  Self.Reset;
end;

destructor TnsGroupProber.Destroy;
var
	i: integer;
begin
	for i := 0 to Pred(mNumOfProbers) do
    if mProbers[i] <> nil then
      mProbers[i].Free;

  SetLength(mProbers, 0);
  SetLength(mProberStates, 0);
  SetLength(mIsActive, 0);
  inherited;
end;

function TnsGroupProber.GetDetectedCharset: eInternalCharsetID;
begin
  (*if we have no answer yet*)
  if mBestGuess = -1 then
    begin
      GetConfidence;
      (*no charset seems positive*)
      if mBestGuess = -1 then
	      mBestGuess:= 0;
      (*we will use default.*)
    end;

	Result := mProbers[mBestGuess].GetDetectedCharset;
end;

function TnsGroupProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
var
  i: integer;
begin
  {$IFDEF DEBUG_chardet}
    AddDump('Group Prober - HandleData - start');
  {$endif}
  for i:=0 to Pred(mNumOfProbers) do
    begin
      if (mProberStates[i] = psNotMe) or
         (mProberStates[i] = psFoundIt) then
        continue;

      mProberStates[i] := mProbers[i].HandleData(aBuf, aLen);

      if mProberStates[i] = psFoundIt then
        begin
          mBestGuess := i;
          mState := psFoundIt;
          break;
        end
      else
        if mProberStates[i] = psNotMe then
          begin
            mIsActive[i] := FALSE;
            dec(mActiveNum);
            if mActiveNum <= 0 then
              begin
                mState := psNotMe;
                break;
              end;
          end;
    end;
  Result := mState;
  {$IFDEF DEBUG_chardet}
    DumpStatus('Group Prober - HandleData - exit');
  {$endif}
end;

procedure TnsGroupProber.Reset;
var
	i: integer;
begin
  mActiveNum := 0;
  for i := 0 to Pred(mNumOfProbers) do
    begin
      if mProbers[i] <> nil then
        begin
          mProbers[i].Reset;
          mIsActive[i] := mProbers[i].Enabled;
          if mProbers[i].Enabled then
            begin
              mProberStates[i] := psDetecting;
              inc(mActiveNum);
            end
          else
            mProberStates[i] := psNotMe;
        end
      else
      	mIsActive[i] := FALSE;
    end;
  mBestGuess := -1;
  mEnabled := (mActiveNum > 0);
  if mEnabled then
    mState := psDetecting
  else
    mState := psNotMe;
end;

function TnsGroupProber.GetConfidence: float;
var
  i: integer;
  bestConf: float;
  cf: float;
begin
  bestConf := 0.0;
  case mState of
    psFoundIt:
      begin
      	Result := SURE_YES;  (*sure yes*)
        exit;
      end;
    psNotMe:
    	begin
      	Result := SURE_NO;  (*sure no*)
        exit;
      end;
    else
      for i := 0 to Pred(mNumOfProbers) do
        begin
          if not mIsActive[i] then
            continue;

          cf := mProbers[i].GetConfidence;
          if bestConf < cf then
            begin
              bestConf := cf;
              mBestGuess := i;
            end;
        end;
  end;{case}
  {$IFDEF DEBUG_chardet}
    DumpStatus('Group Prober - GetConfidience');
  {$endif}

  Result := bestConf;
end;

function TnsGroupProber.EnableCharset(Charset: eInternalCharsetID; NewValue: Boolean): Boolean;
var
  i: integer;
begin
  for i := 0 to Pred(mNumOfProbers) do
    if mProbers[i].GetDetectedCharset = Charset then
      begin
        Result := true;
        mProbers[i].Enabled := NewValue;
        exit;
      end;
  Result := false;
end;

{$ifdef DEBUG_chardet}
procedure TnsGroupProber.DumpStatus(Dump: string);
var
  i: integer;
begin
  inherited DumpStatus(Dump);
  inherited DumpStatus(Format('%30s - %5s - %5s',
          ['Prober',
           'Active',
           'Conf']));
  for i := 0 to Pred(mNumOfProbers) do
    inherited DumpStatus(Format('%30s - %5s - %1.5f',
          [GetEnumName(TypeInfo(eInternalCharsetID), integer(mProbers[i].GetDetectedCharset)),
           BoolToStr(mIsActive[i], True),
           mProbers[i].GetConfidence]));

end;
{$endif}

end.




