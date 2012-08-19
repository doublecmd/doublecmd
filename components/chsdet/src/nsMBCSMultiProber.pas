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
// $Id: nsMBCSMultiProber.pas,v 1.2 2007/05/26 13:09:38 ya_nick Exp $

unit nsMBCSMultiProber;

interface

uses
  {$I dbg.inc}
	nsCore,
  MultiModelProber,
  JpCntx,
	CharDistribution;

type
	TnsMBCSMultiProber = class (TMultiModelProber)
    private
      mDistributionAnalysis: array of TCharDistributionAnalysis;
      mContextAnalysis: array of TJapaneseContextAnalysis;
      mBestGuess: integer;

      function RunStatAnalyse(aBuf: PChar; aLen: integer): eProbingState;
      function GetConfidenceFor(index: integer): double; reintroduce;
		public
			constructor Create; reintroduce;
      destructor Destroy; override;
		  function HandleData(aBuf: PChar; aLen: integer): eProbingState; override;
      function GetConfidence: double; override;
      procedure Reset; override;
      {$ifdef DEBUG_chardet}
      procedure DumpStatus(Dump: string); override;
      {$endif}
end;

implementation
uses
	SysUtils,
  nsCodingStateMachine
  {$ifdef DEBUG_chardet}
  ,TypInfo
  {$endif}
  ;

{$I '.\mbclass\SJISLangModel.inc'}
{$I '.\mbclass\EUCJPLangModel.inc'}
{$I '.\mbclass\GB18030LangModel.inc'}
{$I '.\mbclass\EUCKRLangModel.inc'}
{$I '.\mbclass\Big5LangModel.inc'}
{$I '.\mbclass\EUCTWLangModel.inc'}



{ TnsMBCSMultiProber }
const
	NUM_OF_PROBERS = 6;


constructor TnsMBCSMultiProber.Create;
begin
  inherited Create;
  SetLength(mDistributionAnalysis, NUM_OF_PROBERS);
  SetLength(mContextAnalysis, NUM_OF_PROBERS);

  AddCharsetModel(SJISLangModel);
  mDistributionAnalysis[0] := TSJISDistributionAnalysis.Create;
  mContextAnalysis[0] := TSJISContextAnalysis.Create;

  AddCharsetModel(EUCJPLangModel);
  mDistributionAnalysis[1] := TEUCKRDistributionAnalysis.Create;
  mContextAnalysis[1] := nil;

  AddCharsetModel(GB18030LangModel);
  mDistributionAnalysis[2] := TGB2312DistributionAnalysis.Create;
  mContextAnalysis[2] := nil;

  AddCharsetModel(EUCKRLangModel);
  mDistributionAnalysis[3] := TEUCKRDistributionAnalysis.Create;
  mContextAnalysis[3] := nil;

  AddCharsetModel(Big5LangModel);
  mDistributionAnalysis[4] := TBig5DistributionAnalysis.Create;
  mContextAnalysis[4] := nil;

  AddCharsetModel(EUCTWLangModel);
  mDistributionAnalysis[5] := TEUCTWDistributionAnalysis.Create;
  mContextAnalysis[5] := nil;

end;

destructor TnsMBCSMultiProber.Destroy;
var
  i: integer;
begin
  inherited;
  for i := 0 to Pred(mCharsetsCount) do
    begin
      if mDistributionAnalysis[i] <> nil then
        mDistributionAnalysis[i].Free;
      if mContextAnalysis[i] <> nil then
        mContextAnalysis[i].Free;
    end;

  SetLength(mDistributionAnalysis, 0);
  SetLength(mContextAnalysis, 0);

end;

{$ifdef DEBUG_chardet}
procedure TnsMBCSMultiProber.DumpStatus(Dump: string);
var
  i: integer;
begin
  AddDump(Dump + ' Current state ' + GetEnumName(TypeInfo(eProbingState), integer(mState)));
  AddDump(Format('%30s - %10s - %5s',
          ['Prober',
           'State',
           'Conf']));
  for i := 0 to Pred(mCharsetsCount) do
    AddDump(Format('%30s - %10s - %1.5f',
          [GetEnumName(TypeInfo(eInternalCharsetID), integer(mCodingSM[i].GetCharsetID)),
           GetEnumName(TypeInfo(eProbingState), integer(mSMState[i])),
           GetConfidenceFor(i)
           ]));
end;
{$endif}

function TnsMBCSMultiProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
var
  i: integer; (*do filtering to reduce load to probers*)
  highbyteBuf: PChar;
  hptr: PChar;
  keepNext: Boolean;
begin
	keepNext := TRUE;
  (*assume previous is not ascii, it will do no harm except add some noise*)
  highbyteBuf := AllocMem(aLen);
  try
    hptr:= highbyteBuf;
    if hptr = nil  then
      begin
        Result := mState;
        exit;
      end;
    for i:=0 to Pred(aLen) do
      begin
        if (Byte(aBuf[i]) > $80) then
          begin
            inc(hptr);
            hptr^ := aBuf[i];
            keepNext:= TRUE;
          end
        else
          begin
            (*if previous is highbyte, keep this even it is a ASCII*)
            if keepNext = TRUE then
              begin
                inc(hptr);
                hptr^ := aBuf[i];
                keepNext:= FALSE;
              end;
          end;
      end;
    {$IFDEF DEBUG_chardet}
     AddDump('MultiByte - HandleData - start');
    {$endif}

    inherited HandleData(highbyteBuf, hptr - highbyteBuf);
    {$IFDEF DEBUG_chardet}
     AddDump('MultiByte - HandleData - end');
    {$endif}
  finally
	  FreeMem(highbyteBuf, aLen);
  end;

  // if we have more when one candidat then
  // try statisic analyse
  if (mState <> psFoundIt) or
     (mState <> psNotMe) then
    RunStatAnalyse(aBuf, aLen);
	Result := mState;
end;

function TnsMBCSMultiProber.RunStatAnalyse(aBuf: PChar; aLen: integer): eProbingState;
var
  i, c: integer;
  codingState: nsSMState;
  charLen: byte;
  mLastChar: array [0..1] of Char;
begin
  {$IFDEF DEBUG_chardet}
   AddDump('MultiByte - Stat Analyse - start');
  {$endif}

  for i := 0 to Pred(mCharsetsCount) do
    begin
      if (mSMState[i] = psFoundIt) or
         (mSMState[i] = psNotMe) then
        continue;
      if mDistributionAnalysis[i] = nil then
        continue;
      for c := 0 to Pred(aLen) do
        begin
          codingState := mCodingSM[i].NextState(aBuf[c]);
          if codingState = eStart then
            begin
              charLen := mCodingSM[i].GetCurrentCharLen;
              if c = 0 then
                begin
                  mLastChar[1] := aBuf[0];
                  if mContextAnalysis[i] <> nil then
                      mContextAnalysis[i].HandleOneChar(mLastChar,charLen);
                  mDistributionAnalysis[i].HandleOneChar(mLastChar,charLen);
                end
              else
                begin
                  if mContextAnalysis[i] <> nil then
                    mContextAnalysis[i].HandleOneChar(aBuf+c-1,charLen);
                  mDistributionAnalysis[i].HandleOneChar(aBuf+c-1,charLen);
                end;
            end;
          if (mContextAnalysis[i] <> nil) then
             if mContextAnalysis[i].GotEnoughData and
    	         (GetConfidenceFor(i) > SHORTCUT_THRESHOLD) then
            begin
  		        mState := psFoundIt;
              mDetectedCharset := mCodingSM[i].GetCharsetID;
              break;
            end;
        end;
    end;

  {$IFDEF DEBUG_chardet}
   AddDump('MultiByte - Stat Analyse - EXIT');
  {$endif}
  Result := mState;
end;

function TnsMBCSMultiProber.GetConfidenceFor(index: integer): double;
var
  contxtCf: double;
  distribCf: double;
begin
  if mContextAnalysis[index] <> nil then
    contxtCf := mContextAnalysis[index].GetConfidence
  else
    contxtCf := -1;

  distribCf := mDistributionAnalysis[index].GetConfidence;

  if contxtCf > distribCf then
    Result := contxtCf
  else
    Result := distribCf;
end;

function TnsMBCSMultiProber.GetConfidence: double;
var
  i: integer;
  conf,
  bestConf: double;
begin
  mBestGuess := -1;
  bestConf := SURE_NO;
  for i := 0 to Pred(mCharsetsCount) do
    begin
      if (mSMState[i] = psFoundIt) or
         (mSMState[i] = psNotMe) then
        continue;
      if mDistributionAnalysis[i] = nil then
        continue;
      conf := GetConfidenceFor(i);
      if conf > bestConf then
        begin
          mBestGuess := i;
          bestConf := conf;
        end;
    end;
  Result := bestConf;
  if mBestGuess > -1 then
    mDetectedCharset := mCodingSM[mBestGuess].GetCharsetID
  else
    mDetectedCharset := UNKNOWN_CHARSET;
end;

procedure TnsMBCSMultiProber.Reset;
var
  i: integer;
begin
  inherited Reset;
  for i := 0 to Pred(mCharsetsCount) do
    begin
      if mDistributionAnalysis[i] <> nil then
        mDistributionAnalysis[i].Reset;
      if mContextAnalysis[i] <> nil then
        mContextAnalysis[i].Reset;
    end;
end;

end.
