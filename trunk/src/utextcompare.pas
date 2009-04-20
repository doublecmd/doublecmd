{
 /***************************************************************************
                          uTextCompare.pas
                          -----------------------
              This unit contains differ class for two TStrings


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 This unit is part of Seksi Commander

 Initial developer: Radek Cervinka

 Last modification: 12.4.03

}

unit uTextCompare;
{ $mode objfpc}{ $H+}
interface
uses
  Classes;
function TextCompare(lsLeft, lsRight:TStrings):Integer;

type
  TTextCompare=Class
    FlsLeft, FlsRight: TStrings;  // what compare
    FiPosLeft, FiPosRight:Integer;
    FiPosLeftOrig, FiPosRightOrig:Integer;   // position in original file
  private
    procedure ScanToDiff;
    procedure AppendLines;
    function NumberLine(const sLine:String; var iCount:Integer):String;
    procedure NumberActualLines;
//    function ResyncRight2Left:Boolean;
    procedure InsertLines(lst:TStrings; iPos:Integer; iCount:Integer; lstNotify:TStrings; var iOrigPos:Integer);
//    function ResyncLeft2Right:Boolean;
    function CountResyncLeft2Right(iResyncSecond:Integer):Integer;
    function CountResyncRight2Left:Integer;

    function CmpLines(sLine1, sLine2:String):Integer;
    function CheckString(const s1, s2:String; iPos1, iPos2:Integer):Integer;

  public
    function CompareMain:Integer;
    procedure SetTStrings(ALeft, ARight:TStrings);
  end;

implementation
uses
  SysUtils;

procedure TTextCompare.AppendLines;
var
  xIndex:Integer;
  iCount:Integer;
begin
  iCount:=Abs(FlsRight.Count-FlsLeft.Count);
  if FlsRight.Count>FlsLeft.Count then
  begin
  
    for xIndex:= 0 to iCount-1 do
    begin
      FlsLeft.AddObject('+++', TObject(2));
      FlsRight.Objects[FiPosRight+xIndex]:=TObject(2);
      FlsRight[FiPosRight+xIndex]:=NumberLine(FlsRight[FiPosRight+xIndex], FiPosRightOrig);
    end;
  end
  else
  begin
    for xIndex:= 0 to iCount-1 do
    begin
      FlsRight.AddObject('+++', TObject(2));
      FlsLeft.Objects[FiPosLeft+xIndex]:=TObject(2);
      FlsLeft[FiPosLeft+xIndex]:=NumberLine(FlsLeft[FiPosLeft+xIndex], FiPosLeftOrig);

    end;
  end;
end;

function TTextCompare.NumberLine(const sLine: String; var iCount: Integer): String;
begin
  Result:=Format('%5d: %s',[iCount,sLine]);
  inc(iCount);
end;

procedure TTextCompare.NumberActualLines;
begin
  FlsLeft[FiPosLeft]:=NumberLine(FlsLeft[FiPosLeft], FiPosLeftOrig);
  FlsRight[FiPosRight]:=NumberLine(FlsRight[FiPosRight], FiPosRightOrig);
end;

function TTextCompare.CmpLines(sLine1, sLine2:String):Integer;
var
  xIndex, xIndex2:Integer;
  iSame:Integer;
  iLine1Len:Integer; // for optimalization only
begin
  Result:=0;
  xIndex:=0;
  sLine1:=Trim(sLine1);
  sLine2:=Trim(sLine2);
  iLine1Len:=Length(sLine1);
  if (sLine1='') or (sLine2 ='') then Exit;
  while (xIndex <iLine1Len)  do
  begin
    inc(xIndex);
    xIndex2:=0;
    while xIndex2 <length(sLine2) do
    begin
      inc(xIndex2);
      if sLine1[xIndex]<>sLine2[xIndex2] then
        Continue;

      iSame:=CheckString(sLine1, sLine2, xIndex, xIndex2); // how much chars is same?
      if iSame>Result then
        Result:=iSame;
      if iSame + xIndex >iLine1Len then // we found max of possible
        Exit;
      if iLine1Len - xIndex < Result then // there is no chance to find big then Result
        Exit;
    end;
  end;
end;


function TextCompare(lsLeft, lsRight:TStrings):Integer;
begin
  with TTextCompare.Create do
  begin
    try
      SetTStrings(lsLeft, lsRight);
      Result:=CompareMain;
    finally
      Free;
    end;
  end;

end;

function TTextCompare.CompareMain:Integer;
var
  iResyncToLeft, iResyncToRight: Integer;
//  i:Integer;
begin
  FiPosLeft :=0;
  FiPosRight :=0;
  FiPosLeftOrig:=1;
  FiPosRightOrig:=1;
  Result:=0; // how much changes
  FlsLeft.Add('');
  FlsRight.Add(''); // add one line for security (at the end is removed - in finally)
  try
    repeat
      ScanToDiff;
      // determine how much lines is different in both direction (left2right and right2left)
      iResyncToLeft:=CountResyncRight2Left;
      iResyncToRight:=CountResyncLeft2Right(iResyncToLeft);


      // no possible resync
      if (iResyncToLeft = 0) and (iResyncToRight = 0) then
      begin
        FlsLeft.Objects[FiPosLeft]:=TObject(2); // notify different lines
        FlsRight.Objects[FiPosRight]:=TObject(2);
        NumberActualLines;
        inc(FiPosLeft);
        inc(FiPosRight);
        Continue;
      end;

      if ((iResyncToLeft< iResyncToRight) or (iResyncToRight = 0)) and (iResyncToLeft<>0) then
      begin
        InsertLines(FlsLeft,FiPosLeft, iResyncToLeft, FlsRight, FiPosRightOrig );
        FiPosLeft:=FiPosLeft + iResyncToLeft;
        FiPosRight:=FiPosLeft ;
        NumberActualLines;
        inc(FiPosRight);
        FiPosLeft:=FiPosRight ;

        inc(Result);
      end
      else
      begin
        if iResyncToRight>0 then
          InsertLines(FlsRight,FiPosRight, iResyncToRight, FlsLeft, FiPosLeftOrig )
        else
          inc(iResyncToRight);
          
        FiPosRight:=FiPosRight + iResyncToRight ;
        FiPosLeft:=FiPosRight ;
        NumberActualLines;
        inc(FiPosRight);
        FiPosLeft:=FiPosRight ;

        inc(Result);
      end;

    until (FiPosLeft= FlsLeft.Count) or (FiPosRight = FlsRight.Count);

    AppendLines; // append lines at the end (both list have to same line count)
  finally
//    FlsLeft.Delete(FlsLeft.Count-1); // delete added security lines
//    FlsRight.Delete(FlsRight.Count-1);
   // hack: lazarus Synedit sometimes not show last line (i think taht is scrollbar related problem)
   // so apppended line is only for safe (last line is appended line)
  end;
end;


procedure TTextCompare.InsertLines(lst: TStrings; iPos, iCount: Integer; lstNotify:TStrings; var iOrigPos:Integer);
var
  xIndex:Integer;
begin
  for xIndex:= 0 to iCount-1 do
  begin
    lst.InsertObject(iPos,'+++', TObject(2));
    lstNotify.Objects[iPos+xIndex]:=TObject(2);
    lstNotify[iPos+xIndex]:=NumberLine(lstNotify[iPos+xIndex], iOrigPos);
  end;
end;

function TTextCompare.CountResyncLeft2Right(iResyncSecond:Integer): Integer;
var
  xIndex:Integer;
//  iMinSuccess:Integer;
  sComparedLine:String;
begin

  Result:=0;
  sComparedLine:=Trim(FlsRight[FiPosRight]);
  if sComparedLine='' then Exit;
  for xIndex:=FiPosLeft to FlsLeft.Count -1 do
  begin

    if (iResyncSecond>0) and ((xIndex - FiPosRight)> iResyncSecond) then
      Break; //befored resync is better

    if (sComparedLine = Trim(FlsLeft[xIndex])) then // if line is exactly (without white chars) same
    begin
      Result:=xIndex - FiPosRight;
      Break;
    end;
    if Pos(sComparedLine, Trim(FlsLeft[xIndex]))>0 then // line look like same
    begin
      FlsLeft.Objects[xIndex]:=TObject(1); // notify change
      FlsRight.Objects[FiPosRight]:=TObject(1);
      Result:=xIndex - FiPosRight;
      Break;
    end;
  end;

end;

function TTextCompare.CountResyncRight2Left: Integer;
var
  xIndex:Integer;
//  iMinSuccess:Integer;
  sComparedLine:String;
begin
  Result:=0;
  // see second Resync :)
  sComparedLine:=Trim(FlsLeft[FiPosLeft]);
  if sComparedLine='' then Exit;
  for xIndex:=FiPosRight to FlsRight.Count -1 do
  begin

    if (sComparedLine = Trim(FlsRight[xIndex])) then
    begin
      Result:=xIndex - FiPosLeft;
      Break;
    end;
    if Pos(sComparedLine, Trim(FlsRight[xIndex]))>0 then
    begin
      FlsRight.Objects[xIndex]:=TObject(1);
      FlsLeft.Objects[FiPosLeft]:=TObject(1);
      Result:=xIndex - FiPosLeft;
      Break;
    end;
  end;
end;


procedure TTextCompare.ScanToDiff;
var
  s:String;
  iMinSame:Integer;
begin
  while (FiPosLeft<FlsLeft.Count-1) and (FiPosRight<FlsRight.Count-1) do
  begin
    s:=Trim(FlsLeft[FiPosLeft]);
    if ( s<> Trim(FlsRight[FiPosRight])) then
    begin
      iMinSame:=(Length(s)+Length(FlsRight[FiPosRight])) div 6; //33 % must be same
      if (CmpLines(s, FlsRight[FiPosRight])<iMinSame ) then
        Break // lines not look like that is same
      else
      begin  // notify change but scan next line
        FlsLeft.Objects[FiPosLeft]:=TObject(1);
        FlsRight.Objects[FiPosRight]:=TObject(1);
      end;
    end;
    NumberActualLines;
    inc(FiPosLeft);
    inc(FiPosRight);
  end;
end;

procedure TTextCompare.SetTStrings(ALeft, ARight: TStrings);
begin
  FlsLeft:=ALeft;
  FlsRight:=ARight;
end;

// return how much chars is same
function TTextCompare.CheckString(const s1, s2: String; iPos1,
  iPos2: Integer): Integer;
begin
  Result:=0;
  while (iPos1<length(s1)) and (iPos2<length(s2)) do
  begin
    if s1[iPos1] = s2[iPos2] then
      Inc(Result)
    else
      Break;
    inc(iPos1);
    inc(iPos2);     
  end;
end;

end.
