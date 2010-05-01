unit uBinaryCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes;

const
  cColumnSize = 8;
  cLineDiverg = 50;

function BinaryCompare(const sLeftFileName, sRightFileName: UTF8String; lsLeft, lsRight: TStrings): Integer;

implementation

uses
  SysUtils, uClassesEx;

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

function Byte2Hex(b: Byte): String;
begin
  Result:= IntToHex(b, 2) + ' ';
end;

function BinaryCompare(const sLeftFileName, sRightFileName: UTF8String; lsLeft, lsRight: TStrings): Integer;
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

function LineFormat(const sHex, sAscii: String): String;
var
  sDummy: String;
begin
  sDummy:= EmptyStr;
  if Length(sHex) < (cColumnSize * 3) then
    sDummy:= StringOfChar('*', cColumnSize*3 - Length(sHex));
  Result:= Format('%s%s  %s',[sHex, sDummy, sAscii]);
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
        lsLeft.AddObject(LineFormat(sLeftHexLine,sLeftLine), TObject(iDiffed));
        lsRight.AddObject(LineFormat(sRightHexLine,sRightLine),TObject(iDiffed));
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
          lsLeft.AddObject(LineFormat(sLeftHexLine,sLeftLine),TObject(iDiffed));
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
          lsRight.AddObject(LineFormat(sRightHexLine,sRightLine),TObject(iDiffed));
          iDiffed:=0;
          sRightLine:='';
          sRightHexLine:='';
          iColumnCount:=0;
          inc(iLineCount);
        end;
      end;
    end;

    if sRightLine<>'' then
      lsRight.AddObject(LineFormat(sRightHexLine,sRightLine),TObject(iDiffed));
    if sLeftLine<>'' then
      lsLeft.AddObject(LineFormat(sLeftHexLine,sLeftLine),TObject(iDiffed));
  Result:=iDif;
  finally
    FreeAndNil(fLeft);
    FreeAndNil(fRight);
  end;
end;

end.
