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
// $Id: MBUnicodeMultiProber.pas,v 1.2 2007/05/26 13:09:38 ya_nick Exp $

unit MBUnicodeMultiProber;

interface

uses
  {$I dbg.inc}
	nsCore,
  MultiModelProber;

type
	TMBUnicodeMultiProber = class (TMultiModelProber)
		public
			constructor Create; reintroduce;
      destructor Destroy; override;
		  function HandleData(aBuf: PChar; aLen: integer): eProbingState; override;
//      function GetConfidence: double; override;
end;

implementation
uses
	SysUtils,
  nsCodingStateMachine;

{$I '.\mbclass\UTF8LangModel.inc'}
{$I '.\mbclass\UCS2BELangModel.inc'}
{$I '.\mbclass\UCS2LELangModel.inc'}



{ TMBUnicodeMultiProber }
const
	NUM_OF_PROBERS = 3;
	ONE_CHAR_PROB: float = 0.50;

{$ifdef DEBUG_chardet}
{$endif}

constructor TMBUnicodeMultiProber.Create;
begin
  inherited Create;

  AddCharsetModel(UTF8LangModel);
  AddCharsetModel(UCS2BELangModel);
  AddCharsetModel(UCS2LELangModel);
end;

destructor TMBUnicodeMultiProber.Destroy;
begin
  inherited;
end;

function TMBUnicodeMultiProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
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

//    inherited HandleData(highbyteBuf, hptr - highbyteBuf);
    inherited HandleData(aBuf, aLen);
  finally
	  FreeMem(highbyteBuf, aLen);
  end;

	Result := mState;
end;

//function TMBUnicodeMultiProber.GetConfidence: double;
//var
//  i: integer;
//  conf,
//  bestConf: double;
//begin
//  mBestGuess := -1;
//  bestConf := SURE_NO;
//  for i := 0 to Pred(mCharsetsCount) do
//    begin
//      if (mSMState[i] = psFoundIt) or
//         (mSMState[i] = psNotMe) then
//        continue;
//
//      if conf > bestConf then
//        begin
//          mBestGuess := i;
//          bestConf := conf;
//        end;
//    end;
//  Result := bestConf;
//  if mBestGuess > 0 then
//    mDetectedCharset := mCodingSM[mBestGuess].GetCharsetID
//  else
//    mDetectedCharset := UNKNOWN_CHARSET;
//end;

end.
