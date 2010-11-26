{
   Double Commander
   -------------------------------------------------------------------------
   Control that shows drives list and allows selecting a drive.

   Copyright (C) 2009  Przemyslaw Nagay (cobines@gmail.com)

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

unit uDrivesList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, Controls, LCLType,
  uFilePanelSelect, uDrive;

type
  TDriveSelected = procedure (Sender: TObject; ADriveIndex: Integer;
    APanel: TFilePanelSelect) of object;

  TDrivesListPopup = class(TStringGrid)
  private
    FDrivesList: TDrivesList;
    FPanel: TFilePanelSelect;
    FShortCuts: array of TUTF8Char;
    FAllowSelectDummyRow: Boolean;
    FOnDriveSelected: TDriveSelected;
    FOnClose: TNotifyEvent;

    {en
       @param(ARow
              Row nr in the grid (LowestRow..HighestRow).)
    }
    function GetDriveIndexByRow(ARow: Integer): Integer;
    function GetDrivesCount: Integer;
    function GetLowestRow: Integer;
    function GetHighestRow: Integer;

    procedure PrepareCanvasEvent(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure SelectCellEvent(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure EnterEvent(Sender: TObject);
    procedure ExitEvent(Sender: TObject);
    procedure KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure SelectDrive(ADriveIndex: Integer);
    procedure DoDriveSelected(ADriveIndex: Integer);

    {en
       Checks if the given shortcut is assigned to a drive.
       If it is then that drive is selected.
       @returns(@true if shortcut found, @false otherwise.)
    }
    function CheckShortcut(AShortcut: TUTF8Char): Boolean;

    procedure Close;

    property LowestRow: Integer read GetLowestRow;
    property HighestRow: Integer read GetHighestRow;

  protected
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
       aState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce;

    procedure UpdateDrivesList(ADrivesList: TDrivesList);

    {en
       Shows the drive list.
       @param(AtPoint
              Position where to show the list.)
       @param(APanel
              For which panel the list is to be shown.)
       @param(ASelectedDriveIndex
              Which drive to pre-select (0..DrivesCount-1).)
    }
    procedure Show(AtPoint: TPoint; APanel: TFilePanelSelect;
                   ASelectedDriveIndex: Integer = -1);

    procedure SetFocus; override;

    property Panel: TFilePanelSelect read FPanel;
    property DrivesCount: Integer read GetDrivesCount;

    property OnDriveSelected: TDriveSelected read FOnDriveSelected write FOnDriveSelected;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  StdCtrls, Graphics, LCLProc,
  uPixMapManager, uOSUtils, uDCUtils
  {$IFDEF LCLGTK2}
  , Gtk2Proc  // for ReleaseMouseCapture
  {$ENDIF}
  ;

const
  DriveIconSize = 16;
  // One dummy row is added, which is not displayed and cannot be selected.
  // It is used to simulate having no selection in the grid, because the
  // TCustomGrid forces at least one row/cell to be selected or focused.
  DummyRows = 1;

constructor TDrivesListPopup.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csNoFocus];
  Parent := AParent;

  FDrivesList := nil;
  FShortCuts := nil;
  FAllowSelectDummyRow := False;
  FOnDriveSelected := nil;
  FOnClose := nil;

  AllowOutboundEvents := False;
  AutoFillColumns := False;
  BorderStyle := bsNone;
  BorderWidth := 0;
  ExtendedSelect := False;
  Flat := False;
  FocusRectVisible := False;
  MouseWheelOption := mwGrid;
  Options := [goRowSelect, goThumbTracking];
  ScrollBars := ssAutoVertical;
  Visible := False;

  ColCount := 4;
  RowCount := 0 + DummyRows;
  FixedCols := 0;
  FixedRows := 0;
  if DummyRows > 0 then
    RowHeights[FixedRows] := 1; // Every row must have Height > 0

  Color := clBtnFace;
  Font.Color := clWindowText;

  OnPrepareCanvas := @PrepareCanvasEvent;
  OnSelectCell    := @SelectCellEvent;
  OnEnter         := @EnterEvent;
  OnExit          := @ExitEvent;
  OnKeyDown       := @KeyDownEvent;
  OnKeyPress      := @KeyPressEvent;
  OnUTF8KeyPress  := @UTF8KeyPressEvent;
end;

procedure TDrivesListPopup.UpdateDrivesList(ADrivesList: TDrivesList);
var
  I, RowNr : Integer;
  FreeSize, TotalSize: Int64;
begin
  FDrivesList := ADrivesList;

  ColCount := 4;
  RowCount := LowestRow + ADrivesList.Count;
  Clean;
  SetLength(FShortCuts, ADrivesList.Count);

  for I := 0 to ADrivesList.Count - 1 do
    begin
      with ADrivesList[I]^ do
      begin
        RowNr := LowestRow + I;

        if Length(DisplayName) > 0 then
        begin
          Cells[1, RowNr] := DisplayName;
          FShortCuts[I] := UTF8Copy(DisplayName, 1, 1);
        end
        else
        begin
          Cells[1, RowNr] := Path;
          FShortCuts[I] := '';
        end;

        Cells[2, RowNr] := DriveLabel;

        // Display free space only for some drives
        // (removable, network, etc. may be slow).
        if (DriveType in [dtHardDisk, dtOptical, dtRamDisk]) and
           IsAvailable(Path, False) and
           GetDiskFreeSpace(Path, FreeSize, TotalSize) then
        begin
          Cells[3, RowNr] :=
            Format('%s/%s', [cnvFormatFileSize(FreeSize, True),
                             cnvFormatFileSize(TotalSize, True)])
        end;
      end;  // with
    end; // for

  AutoSizeColumns;

  // Add some space to the icon column.
  ColWidths[0] := DriveIconSize + 8;

  // Add some space to other columns.
  for I := 1 to ColCount - 1 do
    ColWidths[I] := ColWidths[I] + 4;
end;

procedure TDrivesListPopup.Show(AtPoint: TPoint; APanel: TFilePanelSelect;
                                ASelectedDriveIndex: Integer = -1);
var
  w, h: Integer;
begin
  FPanel := APanel;

  Left := AtPoint.X;
  Top := AtPoint.Y;
  Visible := True;

  w := GridWidth;
  h := GridHeight;

  if DummyRows > 0 then
    Inc(h, RowHeights[FixedRows] + GridLineWidth);

  Width := w;
  Height := h;

  ASelectedDriveIndex := LowestRow + ASelectedDriveIndex;
  if (ASelectedDriveIndex >= LowestRow) and (ASelectedDriveIndex <= HighestRow) then
    Row := ASelectedDriveIndex
  else
  begin
    FAllowSelectDummyRow := True;
    Row := FixedRows; // Select dummy row to clear selection
    FAllowSelectDummyRow := False;
  end;

  // Set focus using parent procedure.
  inherited SetFocus;
end;

procedure TDrivesListPopup.SetFocus;
begin
  // Empty - don't allow setting focus.
end;

procedure TDrivesListPopup.PrepareCanvasEvent(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if aCol = 3 then
  begin
    // Right-align free space text in third column.
    ts := Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    Canvas.TextStyle := ts;
  end
  else if aCol > 0 then
  begin
    // Left-align other columns (except column 0 which shows the icon).
    ts := Canvas.TextStyle;
    ts.Alignment := taLeftJustify;
    Canvas.TextStyle := ts;
  end;
end;

function TDrivesListPopup.GetDriveIndexByRow(ARow: Integer): Integer;
begin
  if (ARow >= LowestRow) and (ARow <= HighestRow) then
    Result := ARow - LowestRow
  else
    Result := -1;
end;

function TDrivesListPopup.GetDrivesCount: Integer;
begin
  Result := HighestRow - LowestRow + 1;
end;

function TDrivesListPopup.GetLowestRow: Integer;
begin
  Result := FixedRows + DummyRows;
end;

function TDrivesListPopup.GetHighestRow: Integer;
begin
  Result := RowCount - 1;
end;

procedure TDrivesListPopup.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  Drive: PDrive;
  BitmapTmp: TBitmap;
begin
  if (aRow = FixedRows) and (DummyRows > 0) then
    // Don't draw the dummy row.
    Exit
  else if (aCol = 0) and (aRow >= LowestRow) then
  begin
    inherited;

    // Draw drive icon in the first column.

    Drive := FDrivesList.Items[GetDriveIndexByRow(aRow)];

    // get disk icon
    BitmapTmp := PixMapManager.GetDriveIcon(Drive, DriveIconSize, Self.Color);

    if Assigned(BitmapTmp) then
    begin
      // Center icon in the cell.
      aRect.Left := aRect.Left + (ColWidths[aCol] - DriveIconSize) div 2;
      aRect.Top := aRect.Top + (RowHeights[aRow] - DriveIconSize) div 2;

      Canvas.Draw(aRect.Left, aRect.Top, BitmapTmp);

      FreeAndNil(BitmapTmp);
    end;
  end
  else
  begin
    inherited;

    // Draw vertical lines separating cells, but only in columns other than first.
    with Canvas, aRect do
    begin
      MoveTo(Right - 1, Top);
      LineTo(Right - 1, Bottom);
    end;
  end;
end;

procedure TDrivesListPopup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  // Totally override MouseDown (don't call inherited).

  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
    Close
  else
  begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol < 0) or (ARow < 0) then
      Close
    else
      SelectDrive(GetDriveIndexByRow(ARow));
  end;
end;

procedure TDrivesListPopup.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  // Totally override MouseMove (don't call inherited).

  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
    Exit;

  MouseToCell(X, Y, ACol, ARow);
  if (ACol >= 0) and (ARow >= 0) then
    Row := ARow;
end;

procedure TDrivesListPopup.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  // Totally override MouseUp (don't call inherited).

  MouseToCell(X, Y, ACol, ARow);
  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) or (ACol < 0) or (ARow < 0) then
    Close;
end;

procedure TDrivesListPopup.Paint;
var
  ARect: TRect;
begin
{$IFDEF LCLQT}
  // In QT Frame3d draws filled rectangle, so it must be drawn before
  // or it would overwrite all the painting done below.
  ARect := Classes.Rect(0, 0, Width, Height);
  Canvas.Frame3d(ARect, 1, bvRaised);
{$ENDIF}

  inherited Paint;

{$IFNDEF LCLQT}
  // This draws empty frame rectangle.
  ARect := Classes.Rect(0, 0, Width, Height);
  Canvas.Frame3d(ARect, 1, bvRaised);
{$ENDIF}
end;

procedure TDrivesListPopup.SelectCellEvent(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  // Don't allow selecting dummy row.
  if (not FAllowSelectDummyRow) and (DummyRows > 0) then
    CanSelect := aRow > FixedRows
  else
    CanSelect := True;
end;

procedure TDrivesListPopup.EnterEvent(Sender: TObject);
begin
  // Mouse capture is needed for detecting when mouse is clicked outside the control.
  // This also recaptures mouse if user switched to another application and back.
  MouseCapture := True;
end;

procedure TDrivesListPopup.ExitEvent(Sender: TObject);
begin
  Close;
end;

procedure TDrivesListPopup.KeyDownEvent(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_HOME:
      begin
        Row := LowestRow;
        Key := 0;
      end;
    VK_END:
      begin
        Row := HighestRow;
        Key := 0;
      end;
    VK_PRIOR, VK_NEXT:
      // Disable page-up, page-down.
      Key := 0;
    VK_UP, VK_LEFT:
      begin
        if Row > LowestRow then
          Row := Row - 1
        // If dummy row selected then select the last row.
        else if Row = FixedRows then
          Row := HighestRow;
        Key := 0;
      end;
    VK_DOWN, VK_RIGHT:
      begin
        if Row < HighestRow then
          Row := Row + 1;
        Key := 0;
      end;
    VK_RETURN, VK_SELECT, VK_SPACE:
      begin
        SelectDrive(GetDriveIndexByRow(Row));
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        Close;
        Key := 0;
      end;
  end;
end;

procedure TDrivesListPopup.KeyPressEvent(Sender: TObject; var Key: Char);
begin
  if CheckShortcut(TUTF8Char(Key)) then
    Key := #0;
end;

procedure TDrivesListPopup.UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if CheckShortcut(UTF8Key) then
    UTF8Key := '';
end;

procedure TDrivesListPopup.SelectDrive(ADriveIndex: Integer);
begin
  if (ADriveIndex >= 0) and (ADriveIndex < DrivesCount) then
  begin
    MouseCapture := False;
{$IFDEF LCLGTK2}
    // Workaround a bug in GTK2 where mouse capture is not released.
    ReleaseMouseCapture;

    // On GTK2 first the control must be hidden (to lose focus), otherwise
    // any modal form shown in the OnDriveSelected handler cannot be closed
    // due to a bug in LCLGTK2.
    Visible := False;
{$ENDIF}

    DoDriveSelected(ADriveIndex);
    Close;
  end;
end;

procedure TDrivesListPopup.DoDriveSelected(ADriveIndex: Integer);
begin
  if Assigned(FOnDriveSelected) then
    FOnDriveSelected(Self, ADriveIndex, FPanel);
end;

function TDrivesListPopup.CheckShortcut(AShortcut: TUTF8Char): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(FShortCuts) - 1 do
  begin
    if FShortCuts[i] = AShortcut then
    begin
      SelectDrive(i);
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TDrivesListPopup.Close;
begin
  MouseCapture := False;
{$IFDEF LCLGTK2}
  // Workaround a bug in GTK2 where mouse capture is not released.
  ReleaseMouseCapture;
{$ENDIF}
  Visible := False;

  if Assigned(FOnClose) then
    FOnClose(Self);
end;

end.
