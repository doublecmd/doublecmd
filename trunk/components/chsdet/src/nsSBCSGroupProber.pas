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
// $Id: nsSBCSGroupProber.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit nsSBCSGroupProber;

interface
uses
	nsCore,
  nsGroupProber;

type
	TnsSBCSGroupProber = class(TnsGroupProber)
		public
      constructor Create; reintroduce;
      function HandleData(aBuf: PChar;  aLen: integer): eProbingState; override;
//      {$ifdef DEBUG_chardet}
//      procedure DumpStatus; override;
//      {$endif}
  end;

implementation
uses
	SysUtils,
	nsHebrewProber,
  nsSBCharSetProber,
  LangCyrillicModel,
  LangGreekModel,
  LangBulgarianModel,
  LangHebrewModel;

{ TnsSBCSGroupProber }
const
	NUM_OF_PROBERS = 13;

constructor TnsSBCSGroupProber.Create;
var
  hebprober: TnsHebrewProber;
  i: integer;
begin
  mNumOfProbers := NUM_OF_PROBERS;
  SetLength(mProbers, NUM_OF_PROBERS);
  SetLength(mIsActive, NUM_OF_PROBERS);
  SetLength(mProberStates, NUM_OF_PROBERS);
  mProbers[0] := TnsSingleByteCharSetProber.Create(Win1251Model);
  mProbers[1] := TnsSingleByteCharSetProber.Create(Koi8rModel);
  mProbers[2] := TnsSingleByteCharSetProber.Create(Latin5Model);
  mProbers[3] := TnsSingleByteCharSetProber.Create(MacCyrillicModel);
  mProbers[4] := TnsSingleByteCharSetProber.Create(Ibm866Model);
  mProbers[5] := TnsSingleByteCharSetProber.Create(Ibm855Model);
  mProbers[6] := TnsSingleByteCharSetProber.Create(Latin7Model);
  mProbers[7] := TnsSingleByteCharSetProber.Create(Win1253Model);
  mProbers[8] := TnsSingleByteCharSetProber.Create(Latin5BulgarianModel);
  mProbers[9] := TnsSingleByteCharSetProber.Create(Win1251BulgarianModel);

  hebprober := TnsHebrewProber.Create;
  // Notice: Any change in these indexes - 10,11,12 must be reflected
  // in the code below as well.
  mProbers[10]:= hebprober;
  mProbers[11]:= TnsSingleByteCharSetProber.Create(Win1255Model,FALSE,hebprober);(* Logical Hebrew*)
  mProbers[12]:= TnsSingleByteCharSetProber.Create(Win1255Model,TRUE,hebprober); (* Visual Hebrew*)
  (* Tell the Hebrew prober about the logical and visual probers*)
  if (mProbers[10]<>nil)and(mProbers[11]<>nil)and(mProbers[12]<>nil) then
    (* all are not null*)
    hebprober.SetModelProbers(mProbers[11],mProbers[12])
  else
    (* One or more is null. avoid any Hebrew probing, null them all*)
    for i := 10 to 12 do
      begin
        mProbers[i].Free;
        mProbers[i] := nil;
      end;

  inherited Create;
  (* disable latin2 before latin1 is available, otherwise all latin1 *)
  (* will be detected as latin2 because of their similarity.*)
  // mProbers[10] = new nsSingleByteCharSetProber(&Latin2HungarianModel);
  // mProbers[11] = new nsSingleByteCharSetProber(&Win1250HungarianModel);
end;

function TnsSBCSGroupProber.HandleData(aBuf: PChar; aLen: integer): eProbingState;
var
  newBuf1: PChar;
  newLen1: integer;
begin
  newBuf1 := AllocMem(aLen);
  newLen1 := 0;
  (*apply filter to original buffer, and we got new buffer back*)
  (*depend on what script it is, we will feed them the new buffer *)
  (*we got after applying proper filter*)
  (*this is done without any consideration to KeepEnglishLetters*)
  (*of each prober since as of now, there are no probers here which*)
  (*recognize languages with English characters.*)
  Result := mState;
  try
    if (not FilterWithoutEnglishLetters(aBuf,aLen,newBuf1,newLen1)) or
        (newLen1 = 0) then
      exit; (* Nothing to see here, move on.*)
    inherited HandleData(newBuf1, newLen1);
  finally
    FreeMem(newBuf1, aLen);
  end;
  Result:= mState;
end;

{$ifdef DEBUG_chardet}
procedure TnsSBCSGroupProber.DumpStatus;
var
  i: integer;
  cf: float;
  i: integer;
begin
  cf := GetConfidence;
  printf(' SBCS Group Prober --------begin status r'#13#10'');
  for i := 0 to Pred(NUM_OF_SBCS_PROBERS) do
    begin
      if 0 = mIsActive[i] then
	      printf('  inactive: [%s] (i.e. confidence is too low).r'#13#10'',mProbers[i].GetCharSetName)
      else
  	    mProbers[i].DumpStatus;
    end;
  printf(' SBCS Group found best match [%s] confidence %f.r'#13#10'',mProbers[mBestGuess].GetCharSetName,cf);
end;
{$endif}

end.
