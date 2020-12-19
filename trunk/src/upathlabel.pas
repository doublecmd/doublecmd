{
    Double Commander
    -------------------------------------------------------------------------
    Label displaying a path, highlighting directories with mouse.

    Copyright (C) 2010-2011  Przemysław Nagay (cobines@gmail.com)
    Copyright (C) 2014-2020  Alexander Koblov (alexx2000@mail.ru)

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

unit uPathLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics;

type

  { TPathLabel }

  TPathLabel = class(TLabel)
  private
    FActive: Boolean;
    FAllowHighlight: Boolean;
    FHighlightStartPos: Integer;
    FHighlightText: String;
    FMousePos: Integer;
    FColors: array[0..3] of TColor;
    {en
       How much space to leave between the text and left border.
    }
    FLeftSpacing: Integer;

    {en
       If a user clicks on a parent directory of the path,
       this stores the full path of that parent directory.
    }
    FSelectedDir: String;

    {en
       If a mouse if over some parent directory of the currently displayed path,
       it is highlighted, so that user can click on it.
    }
    procedure Highlight;

    function GetColor(const AIndex: Integer): TColor;
    procedure SetColor(const AIndex: Integer; const AValue: TColor); overload;

  protected

    procedure TextChanged; override;

    procedure MouseEnter; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseLeave; override;

  public

    constructor Create(AOwner: TComponent; bAllowHighlight: Boolean = False); reintroduce;

    procedure Paint; override;

    {en
       Changes drawing colors depending active/inactive state.
    }
    procedure SetActive(Active: Boolean);

    property AllowHighlight: Boolean read FAllowHighlight write FAllowHighlight;
    property LeftSpacing: Integer read FLeftSpacing write FLeftSpacing;
    property SelectedDir: String read FSelectedDir;

    property ActiveColor: TColor index 0 read GetColor write SetColor;
    property ActiveFontColor: TColor index 1 read GetColor write SetColor;
    property InactiveColor: TColor index 2 read GetColor write SetColor;
    property InactiveFontColor: TColor index 3 read GetColor write SetColor;
  end;

implementation

uses
  Controls, Math;

{ TPathLabel }

constructor TPathLabel.Create(AOwner: TComponent; bAllowHighlight: Boolean);
begin
  FLeftSpacing := 3; // set before painting

  FColors[0] := clHighlight;
  FColors[1] := clHighlightText;
  FColors[2] := clBtnFace;
  FColors[3] := clBtnText;

  inherited Create(AOwner);

  FAllowHighlight := bAllowHighlight;

  FSelectedDir := '';

  FHighlightStartPos := -1;
  FHighlightText := '';

  SetActive(False);
end;

procedure TPathLabel.Paint;
var
  TextTop: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := Font.Color;

  // Center vertically.
  TextTop := (Height - Canvas.TextHeight(Text)) div 2;

  Canvas.FillRect(0, 0, Width, Height); // background
  Canvas.TextOut(LeftSpacing, TextTop, Text); // path

  // Highlight part of the path if mouse is over it.
  if FHighlightStartPos <> -1 then
  begin
    Canvas.Brush.Color := Font.Color;  // reverse colors
    Canvas.Font.Color  := Color;
    Canvas.TextOut(FHighlightStartPos, TextTop, FHighlightText);
  end;
end;

procedure TPathLabel.SetActive(Active: Boolean);
begin
  case Active of
    False:
      begin
        Color      := InactiveColor;
        Font.Color := InactiveFontColor;
      end;
    True:
      begin
        Color      := ActiveColor;
        Font.Color := ActiveFontColor;
      end;
  end;
  FActive := Active;
end;

procedure TPathLabel.Highlight;
var
  PartText: String;
  StartPos, CurPos: Integer;
  PartWidth: Integer;
  CurrentHighlightPos, NewHighlightPos: Integer;
  TextLen: Integer;
  PathDelimWidth: Integer;
begin
  CurrentHighlightPos := LeftSpacing; // start at the beginning of the path
  NewHighlightPos := -1;

  Canvas.Font := Self.Font;
  PathDelimWidth := Canvas.TextWidth(PathDelim);
  TextLen := Length(Text);

  // Start from the first character, but omit any path delimiters at the beginning.
  StartPos := 1;
  while (StartPos <= TextLen) and (Text[StartPos] = PathDelim) do
    Inc(StartPos);

  // Move the canvas position after the skipped text (if any).
  CurrentHighlightPos := CurrentHighlightPos + (StartPos - 1) * PathDelimWidth;

  for CurPos := StartPos + 1 to TextLen - 1 do
  begin
    if Text[CurPos] = PathDelim then
    begin
      PartText := Copy(Text, StartPos, CurPos - StartPos);
      PartWidth := Canvas.TextWidth(PartText);

      // If mouse is over this part of the path - highlight it.
      if InRange(FMousePos, CurrentHighlightPos, CurrentHighlightPos + PartWidth) then
      begin
        NewHighlightPos := CurrentHighlightPos;
        Break;
      end;

      CurrentHighlightPos := CurrentHighlightPos + PartWidth + PathDelimWidth;
      StartPos := CurPos + 1;
    end;
  end;

  // Repaint if highlighted part has changed.
  if NewHighlightPos <> FHighlightStartPos then
  begin
    // Omit minimized part of the displayed path.
    if PartText = '..' then
      FHighlightStartPos := -1
    else
      FHighlightStartPos := NewHighlightPos;

    if FHighlightStartPos <> -1 then
    begin
      Cursor := crHandPoint;

      FHighlightText := PartText;
      // If clicked, this will be the new directory.
      FSelectedDir := Copy(Text, 1, CurPos - 1);
    end
    else
    begin
      Cursor := crDefault;

      FSelectedDir := '';
      FHighlightText := '';
    end;

    Self.Invalidate;
  end;
end;

function TPathLabel.GetColor(const AIndex: Integer): TColor;
begin
  Result:= FColors[AIndex];
end;

procedure TPathLabel.SetColor(const AIndex: Integer; const AValue: TColor);
begin
  FColors[AIndex] := AValue;
  SetActive(FActive);
end;

procedure TPathLabel.TextChanged;
begin
  inherited TextChanged;
  if FAllowHighlight and MouseEntered then Highlight;
end;

procedure TPathLabel.MouseEnter;
begin
  inherited MouseEnter;
  if FAllowHighlight then
  begin
    Cursor := crDefault;
    Invalidate;
  end;
end;

procedure TPathLabel.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FMousePos := X;
  if FAllowHighlight then Highlight;
end;

procedure TPathLabel.MouseLeave;
begin
  inherited MouseLeave;
  if FAllowHighlight then
  begin
    FSelectedDir := '';
    FHighlightStartPos := -1;
    FHighlightText := '';
    Cursor := crDefault;
    Invalidate;
  end;
end;

end.

