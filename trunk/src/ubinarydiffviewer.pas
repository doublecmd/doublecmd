{
   Double Commander
   -------------------------------------------------------------------------
   Binary difference viewer and comparator

   Copyright (C) 2014 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uBinaryDiffViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ViewerControl;

type

  { TBinaryDiffViewer }

  TBinaryDiffViewer = class(TViewerControl)
  private
    FScrollLock: Integer;
    FKeepScrolling: Boolean;
    FSecondViewer: TBinaryDiffViewer;
  protected
    procedure WriteCustom; override;
    procedure SetPosition(Value: PtrInt); override;
  public
    constructor Create(AOwner: TComponent); override;
    property KeepScrolling: Boolean read FKeepScrolling write FKeepScrolling;
    property SecondViewer: TBinaryDiffViewer read FSecondViewer write FSecondViewer;
  end;

  { TBinaryCompare }

  TBinaryCompare = class(TThread)
  private
    FFirst,
    FSecond: PByte;
    FFinish: PtrInt;
    FEqual: Boolean;
    FResult: TFPList;
    FOnFinish: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(First, Second: PByte; FirstSize, SecondSize: PtrInt; Result: TFPList);
    property OnFinish: TThreadMethod read FOnFinish write FOnFinish;
  end;

implementation

uses
  Math;

const
  cHexWidth       = 16;
  cHexOffsetWidth = 8;
  cHexStartHex    = cHexOffsetWidth + 2;  // ': '
  cHexStartAscii  = cHexStartHex + (cHexWidth * 3) + 2;  // '  '

{ TBinaryDiffViewer }

procedure TBinaryDiffViewer.WriteCustom;
const
  cWordSize = 3;
var
  I: Integer;
  X, Y: Integer;
  yIndex: Integer;
  P1, P2: PAnsiChar;
  CurrentPos, LineStart: PtrInt;
  Mine, Foreign, WordHex: String;
  WordWidth, SymbolWidth: Integer;
  MineLength, ForeignLength: Integer;
  SymbolColor: array[0..15] of TColor;
begin
  CurrentPos := Position;
  SymbolWidth := Canvas.TextWidth('W');
  WordWidth := SymbolWidth * cWordSize;
  // Draw visible lines
  for yIndex := 0 to GetClientHeightInLines - 1 do
  begin
    if CurrentPos >= FHighLimit then
      Break;
    // Draw if second viewer exists
    if Assigned(SecondViewer) then
    begin
      X := 0;
      Y := yIndex * FTextHeight;
      LineStart := CurrentPos;
      AddLineOffset(CurrentPos);
      // Mine text
      Mine := TransformHex(CurrentPos, FHighLimit);
      MineLength:= Min(cHexWidth, (Length(Mine) - cHexStartHex) div cWordSize);
      // Foreign text
      Foreign := SecondViewer.TransformHex(LineStart, SecondViewer.FHighLimit);
      ForeignLength:= (Length(Foreign) - cHexStartHex) div cWordSize;
      // Pointers to text
      P1 := PAnsiChar(Mine) + cHexStartHex;
      P2 := PAnsiChar(Foreign) + cHexStartHex;
      // Write line number
      Canvas.TextOut(X, Y, Copy(Mine, 1, cHexStartHex));
      X := X + SymbolWidth * cHexStartHex;
      // Write hex part
      for I := 0 to MineLength - 1 do
      begin
        if (I > ForeignLength) or (PWord(P1)^ <> PWord(P2)^) then
          Canvas.Font.Color := clRed
        else
          Canvas.Font.Color := clBlack;
        SymbolColor[I]:= Canvas.Font.Color;
        WordHex:= Copy(P1, 1, cWordSize);
        Canvas.TextOut(X, Y, WordHex);
        Inc(X, WordWidth);
        Inc(P1, cWordSize);
        Inc(P2, cWordSize)
      end;
      Inc(X, SymbolWidth);
      // Write ASCII part
      WordHex:= Copy(Mine, cHexStartAscii + 1, cHexWidth);
      for I:= 1 to Length(WordHex) do
      begin
        Canvas.Font.Color := SymbolColor[I - 1];
        Canvas.TextOut(X, Y, WordHex[I]);
        Inc(X, SymbolWidth);
      end;
      Canvas.Font.Color := clBlack
    end;
  end;
end;

procedure TBinaryDiffViewer.SetPosition(Value: PtrInt);
begin
  if not (csDestroying in ComponentState) then
  begin
    if FScrollLock = 0 then
    begin
      Inc(FScrollLock);
      try
        inherited SetPosition(Value);
        if FKeepScrolling and Assigned(SecondViewer) then
          SecondViewer.SetPosition(Value);
      finally
        Dec(FScrollLock);
      end;
    end;
  end;
end;

constructor TBinaryDiffViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Mode:= vcmHex;
end;

{ TBinaryCompare }

procedure TBinaryCompare.Execute;
var
  Finish: PtrInt;
  Remain: PtrInt;
  Position: PtrInt = 0;
  Equal: Boolean = True;
begin
  FResult.Clear;
  Remain:= (FFinish mod cHexWidth);
  Finish:= (FFinish - Remain);
  // Compare integer block size
  while (Terminated = False) and (Position < Finish) do
  begin
    if CompareMem(FFirst + Position, FSecond + Position, cHexWidth) then
      Equal:= True
    else if Equal then
    begin
      Equal:= False;
      FResult.Add(Pointer(Position));
    end;
    Position:= Position + cHexWidth;
  end;
  // Compare remain bytes
  if (Remain > 0) then
  begin
    if not CompareMem(FFirst + Position, FSecond + Position, Remain) then
    begin
      if Equal then
      begin
        Equal:= False;
        FResult.Add(Pointer(Position));
      end;
    end;
  end;
  // Different file size
  if (FEqual = False) and (Equal = True) then
  begin
    FResult.Add(Pointer(Position + Remain))
  end;
  if Assigned(FOnFinish) then Synchronize(FOnFinish);
end;

constructor TBinaryCompare.Create(First, Second: PByte; FirstSize,
  SecondSize: PtrInt; Result: TFPList);
begin
  FFirst:= First;
  FSecond:= Second;
  FResult:= Result;
  inherited Create(True);
  FreeOnTerminate:= True;
  FEqual:= (FirstSize = SecondSize);
  FFinish:= Min(FirstSize, SecondSize);
end;

end.

