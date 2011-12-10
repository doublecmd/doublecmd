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
// $Id: nsCodingStateMachine.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit nsCodingStateMachine;

{$R-}

interface

uses
	nsCore;

type
	nsSMState = (
   eStart = 0,
   eError = 1,
   eItsMe = 2
	);

(*state machine model*)
type
  SMModel = record
    classTable: Pointer; //nsPkgInt;
    classFactor: PRUint32;
    stateTable: Pointer; //nsPkgInt;
    charLenTable: Pointer; // pByteArray; // array of byte; // pPRUint32;
		CharsetID: eInternalCharsetID;
  end;
  pSMModel = ^SMModel;

type
	TnsCodingStateMachine = class (TObject)
		protected
      mCurrentState: nsSMState;
      mCurrentCharLen: PRUint32;
      mCurrentBytePos: PRUint32;
      mModel: SMModel;

  	public
      Enabled: Boolean;
		  constructor Create(sm: SMModel);
      destructor Destroy; override;
			function NextState(c: char): nsSMState;
			function GetCurrentCharLen: PRUint32;
		  procedure Reset;
			function GetCharsetID: eInternalCharsetID;

property LangModel: SMModel read mModel;
  end;

implementation

{ TnsCodingStateMachine }

constructor TnsCodingStateMachine.Create(sm: SMModel);
begin
  mCurrentState:= eStart;

  mModel := sm;
  Enabled := true;
end;

destructor TnsCodingStateMachine.Destroy;
begin
	mModel.classTable := nil;
  mModel.stateTable := nil;
  mModel.charLenTable := nil;
  inherited;
end;

function TnsCodingStateMachine.NextState(c: char): nsSMState;
var
  byteCls: PRUint32;
begin
  if not Enabled then
    begin
      Result := eError;
      exit;
    end;
	(*for each byte we get its class , if it is first byte, we also get byte length*)
  byteCls  := pByteArray(mModel.classTable)[integer(c)];
  if mCurrentState = eStart then
    begin
      mCurrentBytePos := 0;
      mCurrentCharLen := pByteArray(mModel.charLenTable)[byteCls];
    end;
  (*from byte's class and stateTable, we get its next state*)
  mCurrentState := nsSMState(pByteArray(mModel.stateTable)[cardinal(mCurrentState) * mModel.classFactor + byteCls]);
  inc(mCurrentBytePos);

//if mCurrentBytePos > mCurrentCharLen then
//  mCurrentState := eError;

  Result:= mCurrentState;
end;

function TnsCodingStateMachine.GetCurrentCharLen: PRUint32;
begin
  Result:= mCurrentCharLen;
end;

procedure TnsCodingStateMachine.Reset;
begin
  mCurrentState:= eStart;
end;

function TnsCodingStateMachine.GetCharsetID: eInternalCharsetID;
begin
  Result:= mModel.CharsetID;
end;

end.
