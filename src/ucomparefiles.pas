{$mode objfpc}{$H+}
unit uCompareFiles;

interface
uses
  Classes;
Type
  TCompareMethod=(cmInternalText, cmInternalBin);
const
  cColumnSize=10;
  cLineDiverg=50;

function CompareFiles(const sLeftFileName, sRightFileName:UTF8String; {Var}
  lsLeft, lsRight:TStrings; CompareMethod:TCompareMethod):Integer;

implementation
uses
  SysUtils, uClassesEx, uTextCompare;

function ConvertByte(b:Byte):Char;
begin
  if b in [32..255] then
    Result:=Chr(b)
  else
    Result:='.';
{  case b of
    0..31: Result:='.';
    32..127: Result:=Chr(b);
  else
    Result:='.';
  end;}
end;

function Byte2Hex(b:Byte):String;
begin
  Result:=IntToHex(b,2)+' ';
end;

function CompareFilesText(const sLeftFileName, sRightFileName:UTF8String; {Var}
  lsLeft, lsRight:TStrings):Integer;

  procedure ReadFromFile(const FileName: UTF8String; Strings: TStrings);
  var
    FileStream : TFileStreamEx;
  begin
    FileStream := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Strings.LoadFromStream(FileStream);
    finally
      FreeAndNil(FileStream);
    end;
  end;

begin
  ReadFromFile(sLeftFileName, lsLeft);
  ReadFromFile(sRightFileName, lsRight);

  Result := TextCompare(lsLeft, lsRight);
end;

function CompareFilesBin(const sLeftFileName, sRightFileName:UTF8String; {Var}
  lsLeft, lsRight:TStrings):Integer;

var
  fLeft, fRight:TFileStreamEx;
  bLeft:Byte;
  bRight:Byte;
  iColumnCount:Integer;
  sLeftLine, sRightLine:String;
  sLeftHexLine, sRightHexLine:String;
  iDif:Integer;
  iLineCount:Integer;
  iDiffed:PtrInt;

procedure ClearLines;
begin
  sLeftLine:='';
  sRightLine:='';
  sLeftHexLine:='';
  sRightHexLine:='';
end;

Function LineFormat(const sHex, sAscii:String; iLine:Integer):String;
var
  sDummy:String;
begin
  sDummy:='';
  if length(sHex)<(cColumnSize*3) then
    sDummy:=StringOfChar('*',cColumnSize*3-length(sHex));
//  Result:=Format('%4d: %s%s  %s',[iLine,sHex,sDummy,sAscii]);;
  Result:=Format('%6d: %s%s  %s',[iLine,sHex,sDummy,sAscii]);;
end;

begin
  fLeft:=TFileStreamEx.Create(sLeftFileName,fmOpenRead);
  fRight:=TFileStreamEx.Create(sRightFileName,fmOpenRead);
  ClearLines;
  try
    iColumnCount:=0;
    iLineCount:=1;
    iDif:=0; // diferences count
    iDiffed:=0;
    while (fRight.Position<fRight.Size) and (fLeft.Position<fLeft.Size) do
    begin
      fLeft.Read(bLeft,1);
      fRight.Read(bRight,1);
      if bLeft<>bRight then
      begin
        inc(iDif);
        inc(iDiffed);
      end;
      sLeftLine:=sLeftLine+ConvertByte(bLeft);
      sLeftHexLine:=sLeftHexLine+Byte2Hex(bLeft);
      sRightLine:=sRightLine+ConvertByte(bRight);
      sRightHexLine:=sRightHexLine+Byte2Hex(bRight);
      inc(iColumnCount);
      if iColumnCount>=cColumnSize then
      begin
        lsLeft.AddObject(LineFormat(sLeftHexLine,sLeftLine, iLineCount), TObject(iDiffed));
        lsRight.AddObject(LineFormat(sRightHexLine,sRightLine, iLineCount),TObject(iDiffed));
        iDiffed:=0;
        ClearLines;
        iColumnCount:=0;
        inc(iLineCount);
      end;
    end;
// load other data
    if fLeft.Size>fRight.Size then
    begin
      while fLeft.Position<fLeft.Size do
      begin
        fLeft.Read(bLeft,1);
        inc(iDiffed);
        inc(iDif);
        sLeftLine:=sLeftLine+ConvertByte(bLeft);
        sLeftHexLine:=sLeftHexLine+Byte2Hex(bRight);
        inc(iColumnCount);
        if iColumnCount>=cColumnSize then
        begin
          lsLeft.AddObject(LineFormat(sLeftHexLine,sLeftLine, iLineCount),TObject(iDiffed));
          iDiffed:=0;
          sLeftLine:='';
          sLeftHexLine:='';
          iColumnCount:=0;
          inc(iLineCount);
        end;
      end;
    end
    else
    begin
// load other data
      while fRight.Position<fRight.Size do
      begin
        fRight.Read(bRight,1);
        inc(iDiffed);
        inc(iDif);
        sRightLine:=sRightLine+ConvertByte(bRight);
        sRightHexLine:=sRightHexLine+Byte2Hex(bRight);
        inc(iColumnCount);
        if iColumnCount>=cColumnSize then
        begin
          lsRight.AddObject(LineFormat(sRightHexLine,sRightLine, iLineCount),TObject(iDiffed));
          iDiffed:=0;
          sRightLine:='';
          sRightHexLine:='';
          iColumnCount:=0;
          inc(iLineCount);
        end;
      end;
    end;

    if sRightLine<>'' then
      lsRight.AddObject(LineFormat(sRightHexLine,sRightLine, iLineCount),TObject(iDiffed));
    if sLeftLine<>'' then
      lsLeft.AddObject(LineFormat(sLeftHexLine,sLeftLine, iLineCount),TObject(iDiffed));
  Result:=iDif;
  finally
    FreeAndNil(fLeft);
    FreeAndNil(fRight);
  end;
end;

function CompareFiles(const sLeftFileName, sRightFileName:UTF8String; {Var}
  lsLeft, lsRight:TStrings; CompareMethod:TCompareMethod):Integer;
begin
  assert(lsLeft<>nil,'CompareFiles: lsLeft=nil');
  assert(lsRight<>nil,'CompareFiles: lsRight=nil');
  try
    lsLeft.BeginUpdate;
    lsRight.BeginUpdate;

    lsLeft.Clear;
    lsRight.Clear;

    case CompareMethod of
      cmInternalText:Result:=CompareFilesText(sLeftFileName,sRightFileName, lsLeft, lsRight);
      cmInternalBin:Result:=CompareFilesBin(sLeftFileName,sRightFileName, lsLeft, lsRight);
    end;
  finally
    lsLeft.EndUpdate;
    lsRight.EndUpdate;
  end;
end;

end.
