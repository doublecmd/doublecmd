{
   Double Commander
   -------------------------------------------------------------------------
   Base class for file views which have a main control with a list of files.

   Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)

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

unit uFileViewWithMainCtrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, LCLType,
  uFileViewWorker,
  uOrderedFileView,
  uFileView,
  uDragDropEx;

type

  { TFileViewWithMainCtrl }

  TFileViewWithMainCtrl = class(TOrderedFileView)
  private
{$IFDEF LCLGTK2}
    FLastDoubleClickTime : TDateTime;
{$ENDIF}
    FMainControl: TWinControl;

    { Events for drag&drop from external applications }
    function OnExDragBegin: Boolean;
    function OnExDragEnd: Boolean;
    function OnExDragEnter(var DropEffect: TDropEffect; ScreenPoint: TPoint): Boolean;
    function OnExDragOver(var DropEffect: TDropEffect; ScreenPoint: TPoint): Boolean;
    function OnExDrop(const FileNamesList: TStringList; DropEffect: TDropEffect; ScreenPoint: TPoint): Boolean;
    function OnExDragLeave: Boolean;

    procedure SetMainControl(AValue: TWinControl);
    procedure tmContextMenuTimer(Sender: TObject);
    // If internal dragging is currently in effect, this function
    // stops internal dragging and starts external.
    procedure TransformDraggingToExternal(ScreenPoint: TPoint);

  protected
    // Used to register as a drag and drop source and target.
    FDragDropSource: uDragDropEx.TDragDropSource;
    FDragDropTarget: uDragDropEx.TDragDropTarget;
    FHintFileIndex: PtrInt;
    FMainControlLastMouseButton: TMouseButton; // Mouse button that initiated dragging
    {en
       Used to check if button-up was received after button-down
       or after dropping something after dragging with right mouse button.
    }
    FMainControlMouseDown: Boolean;
    FMouseSelectionStartIndex: Integer;
    FMouseSelectionLastState: Boolean;
    FDragStartPoint: TPoint;
    FDragFileIndex: PtrInt;
    FDropFileIndex: PtrInt;
    FStartDrag: Boolean;
    tmContextMenu: TTimer;
    // Simulates releasing mouse button that started a dragging operation,
    // but was released in another window or another application.
    procedure ClearAfterDragDrop; virtual;
    procedure CreateDefault(AOwner: TWinControl); override;
    procedure DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer); virtual; abstract;
    procedure DoUpdateView; override;
    procedure FinalizeDragDropEx(AControl: TWinControl);
    {en
       Retrieves file index under mouse cursor.
       @param(X, Y
              Should be client coordinates of MainControl.)
       @param(AtFileList
              Whether X, Y point to the filelist, not at specific file but at empty space.
              If AtFileList is @false then X, Y point somewhere outside the file list.)
    }
    function GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt; virtual; abstract;
    procedure InitializeDragDropEx(AControl: TWinControl);
    {en
       Returns @true if currently selecting with right mouse button.
    }
    function IsMouseSelecting: Boolean; inline;
    procedure MainControlDblClick(Sender: TObject);
    procedure MainControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MainControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure MainControlEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure MainControlEnter(Sender: TObject);
    procedure MainControlExit(Sender: TObject);
    procedure MainControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MainControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MainControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainControlMouseLeave(Sender: TObject);
    procedure MainControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MainControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainControlShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure MainControlUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    {en
       Changes drawing colors depending on if this panel is active.
    }
    procedure SetActive(bActive: Boolean); override;
    {en
       Updates the drop row index, which is used to draw a rectangle
       on directories during drag&drop operations.
    }
    procedure SetDropFileIndex(NewFileIndex: PtrInt);
    procedure WorkerStarting(const Worker: TFileViewWorker); override;
    procedure WorkerFinished(const Worker: TFileViewWorker); override;
    property MainControl: TWinControl read FMainControl write SetMainControl;

{$IFDEF LCLGTK2}
    function TooManyDoubleClicks: Boolean;
{$ENDIF}

  public
    destructor Destroy; override;
    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  var DropParams: TDropParams); override;
    function Focused: Boolean; override;
    procedure SetFocus; override;

  published
    procedure cm_ContextMenu(const Params: array of string);
  end;

implementation

uses
{$IF DEFINED(LCLGTK2)}
  Gtk2Proc,  // for ReleaseMouseCapture
  GTK2Globals,  // for DblClickTime
{$ENDIF}
  LCLIntf, Forms,
  fMain,
  uGlobs, uInfoToolTip, uDisplayFile, uFile, uFileSystemFileSource;

type
  TControlHandlersHack = class(TWinControl)
  end;

{ TFileViewWithMainCtrl }

procedure TFileViewWithMainCtrl.ClearAfterDragDrop;
begin
  // Clear some control specific flags.
  MainControl.ControlState := MainControl.ControlState - [csClicked, csLButtonDown];
end;

procedure TFileViewWithMainCtrl.cm_ContextMenu(const Params: array of string);
var
  Rect: TRect;
  Point: TPoint;
begin
  Rect := GetFileRect(GetActiveFileIndex);
  Point.X := Rect.Left + ((Rect.Right - Rect.Left) div 2);
  Point.Y := Rect.Top + ((Rect.Bottom - Rect.Top) div 2);
  Point := MainControl.ClientToScreen(Point);
  frmMain.Commands.DoContextMenu(Self, Point.X, Point.Y, False);
end;

procedure TFileViewWithMainCtrl.CreateDefault(AOwner: TWinControl);
begin
  FDropFileIndex := -1;
  FHintFileIndex := -1;
{$IFDEF LCLGTK2}
  FLastDoubleClickTime := Now;
{$ENDIF}
  FStartDrag := False;

  inherited CreateDefault(AOwner);

  tmContextMenu := TTimer.Create(Self);
  tmContextMenu.Enabled  := False;
  tmContextMenu.Interval := 500;
  tmContextMenu.OnTimer  := @tmContextMenuTimer;
end;

destructor TFileViewWithMainCtrl.Destroy;
begin
  if Assigned(HotMan) then
    HotMan.UnRegister(MainControl);
  inherited Destroy;
end;

procedure TFileViewWithMainCtrl.DoDragDropOperation(Operation: TDragDropOperation; var DropParams: TDropParams);
var
  AFile: TDisplayFile;
  ClientDropPoint: TPoint;
  FileIndex: PtrInt;
  AtFileList: Boolean;
  FileSourceIndex, PathIndex: Integer;
begin
  try
    with DropParams do
    begin
      if Files.Count > 0 then
      begin
        ClientDropPoint := MainControl.ScreenToClient(ScreenDropPoint);
        FileIndex := GetFileIndexFromCursor(ClientDropPoint.X, ClientDropPoint.Y, AtFileList);

        // default to current active directory in the destination panel
        TargetPath := Self.CurrentPath;

        if (DropIntoDirectories = True) and IsFileIndexInRange(FileIndex) then
        begin
          AFile := FFiles[FileIndex];

          // If dropped into a directory modify destination path and file source accordingly.
          if Assigned(AFile) and
             (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) then
          begin
            if AFile.FSFile.Name = '..' then
            begin
              if TargetFileSource.IsPathAtRoot(CurrentPath) then
              begin
                // Change to previous file source and last path.
                FileSourceIndex := History.CurrentFileSourceIndex - 1;
                if FileSourceIndex < 0 then
                  TargetFileSource := nil // No parent file sources.
                else
                begin
                  PathIndex := History.PathsCount[FileSourceIndex] - 1;
                  if PathIndex < 0 then
                    TargetFileSource := nil // No paths.
                  else
                  begin
                    TargetFileSource := FileSources[FileSourceIndex];
                    TargetPath := History.Path[FileSourceIndex, PathIndex];
                  end;
                end;
              end
              else
              begin
                // Remove the last subdirectory in the path.
                TargetPath := TargetFileSource.GetParentDir(TargetPath);
              end;
            end
            else
              TargetPath := TargetPath + AFile.FSFile.Name + DirectorySeparator;
          end;
        end;
      end;
    end;

    // Execute the operation.
    frmMain.DoDragDropOperation(Operation, DropParams);

  finally
    FreeAndNil(DropParams);
  end;
end;

procedure TFileViewWithMainCtrl.DoUpdateView;
begin
  inherited DoUpdateView;
  MainControl.Color := DimColor(gBackColor);
  MainControl.ShowHint := (gShowToolTipMode <> []);
end;

procedure TFileViewWithMainCtrl.FinalizeDragDropEx(AControl: TWinControl);
begin
  FreeAndNil(FDragDropSource);
  FreeAndNil(FDragDropTarget);
end;

function TFileViewWithMainCtrl.Focused: Boolean;
begin
  Result := Assigned(MainControl) and MainControl.Focused;
end;

procedure TFileViewWithMainCtrl.InitializeDragDropEx(AControl: TWinControl);
begin
  // Register as drag&drop source and target.
  FDragDropSource := uDragDropEx.CreateDragDropSource(AControl);
  if Assigned(FDragDropSource) then
    FDragDropSource.RegisterEvents(nil, nil, @OnExDragEnd);

  FDragDropTarget := uDragDropEx.CreateDragDropTarget(AControl);
  if Assigned(FDragDropTarget) then
    FDragDropTarget.RegisterEvents(@OnExDragEnter, @OnExDragOver,
                                   @OnExDrop, @OnExDragLeave);
end;

function TFileViewWithMainCtrl.IsMouseSelecting: Boolean;
begin
  Result := FMainControlMouseDown and (FMainControlLastMouseButton = mbRight);
end;

procedure TFileViewWithMainCtrl.MainControlDblClick(Sender: TObject);
var
  Point : TPoint;
  FileIndex : PtrInt;
  AtFileList: Boolean;
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  FStartDrag := False; // don't start drag on double click
  Point := MainControl.ScreenToClient(Mouse.CursorPos);

  // If on a file/directory then choose it.
  FileIndex := GetFileIndexFromCursor(Point.x, Point.y, AtFileList);
  if IsFileIndexInRange(FileIndex) then
    ChooseFile(FFiles[FileIndex]);

{$IFDEF LCLGTK2}
  FLastDoubleClickTime := Now;
{$ENDIF}
end;

procedure TFileViewWithMainCtrl.MainControlDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourcePanel: TFileViewWithMainCtrl;
  SourceFiles: TFiles;
  DropParams: TDropParams;
begin
  if not (Source is TWinControl) or
     not (TWinControl(Source).Parent is TFileViewWithMainCtrl) then
    Exit;

  SourcePanel := ((Source as TWinControl).Parent) as TFileViewWithMainCtrl;

  // Get file names from source panel.
  SourceFiles := SourcePanel.CloneSelectedFiles;
  try
    // Drop onto target panel.
    DropParams := TDropParams.Create(
      SourceFiles, // Will be freed automatically.
      GetDropEffectByKeyAndMouse(GetKeyShiftState,
                                 SourcePanel.FMainControlLastMouseButton),
      MainControl.ClientToScreen(Classes.Point(X, Y)),
      True,
      SourcePanel, Self,
      Self.FileSource, Self.CurrentPath);

    frmMain.DropFiles(DropParams);
    SetDropFileIndex(-1);
  except
    FreeAndNil(SourceFiles);
    raise;
  end;
end;

procedure TFileViewWithMainCtrl.MainControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  AFile: TDisplayFile;
  SourcePanel: TFileView;
  TargetPanel: TFileView;
  SourceDir, TargetDir: String;
  FileIndex: PtrInt;
  AtFileList: Boolean;
begin
  Accept := False;

  if not (Source is TWinControl) or
     not (TWinControl(Source).Parent is TFileView) then
    Exit;

  SourcePanel := ((Source as TWinControl).Parent) as TFileView;
  TargetPanel := Self;

  FileIndex := GetFileIndexFromCursor(X, Y, AtFileList);

  // Always allow dropping into an empty panel.
  // And it is also allowed to drop onto header in case all visible items
  // are directories and the user wants to drop into panel's current directory.
  if FileIndex = InvalidFileIndex then
  begin
    SetDropFileIndex(-1);
    Accept := True;
    Exit;
  end;

  SourceDir := SourcePanel.CurrentPath;
  TargetDir := TargetPanel.CurrentPath;

  AFile := FFiles[FileIndex];

  if (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) then
    begin
      if State = dsDragLeave then
        // Mouse is leaving the control or drop will occur immediately.
        // Don't draw DropRow rectangle.
        SetDropFileIndex(-1)
      else
        SetDropFileIndex(FileIndex);

      if Sender = Source then
      begin
        if not ((FileIndex = FDragFileIndex) or (AFile.Selected = True)) then
          Accept := True;
      end
      else
      begin
        if Assigned(SourcePanel) and Assigned(TargetPanel) then
        begin
          if AFile.FSFile.Name = '..' then
            TargetDir := TargetPanel.FileSource.GetParentDir(TargetDir)
          else
            TargetDir := TargetDir + AFile.FSFile.Name + DirectorySeparator;

          if SourceDir <> TargetDir then Accept := True;
        end
        else
          Accept := True;
      end;
    end
  else if (Sender <> Source) then
    begin
      SetDropFileIndex(-1);

      if Assigned(SourcePanel) then
      begin
        if SourcePanel.CurrentPath <> TargetPanel.CurrentPath then
          Accept := True;
      end
      else
        Accept := True;
    end
  else
    begin
      SetDropFileIndex(-1);
    end;
end;

procedure TFileViewWithMainCtrl.MainControlEndDrag(Sender, Target: TObject; X, Y: Integer);
  procedure ClearDropNode(aFileView: TFileView);
  begin
    if aFileView is TFileViewWithMainCtrl then
      TFileViewWithMainCtrl(aFileView).SetDropFileIndex(-1);
  end;
begin
  // If cancelled by the user, DragManager does not send drag-leave event
  // to the target, so we must clear the DropRow in both panels.

  ClearDropNode(frmMain.FrameLeft);
  ClearDropNode(frmMain.FrameRight);

  if uDragDropEx.TransformDragging = False then
    ClearAfterDragDrop;
end;

procedure TFileViewWithMainCtrl.MainControlEnter(Sender: TObject);
begin
  SetActive(True);
  if Assigned(OnActivate) then
    OnActivate(Self);
end;

procedure TFileViewWithMainCtrl.MainControlExit(Sender: TObject);
begin
  SetActive(False);
  FRangeSelecting := False;
end;

procedure TFileViewWithMainCtrl.MainControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ScreenPoint: TPoint;
begin
  case Key of

    VK_APPS:
      begin
        cm_ContextMenu([]);
        Key := 0;
      end;

    VK_MENU:  // Alt key
      if MainControl.Dragging then
      begin
        // Force transform to external dragging in anticipation of user
        // pressing Alt+Tab to change active application window.

        // Disable flag, so that dragging isn't immediately transformed
        // back to internal before the other application window is shown.
        uDragDropEx.AllowTransformToInternal := False;

        GetCursorPos(ScreenPoint);
        TransformDraggingToExternal(ScreenPoint);
      end;
  end;

  DoHandleKeyDown(Key, Shift);
end;

procedure TFileViewWithMainCtrl.MainControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FRangeSelecting := False;
end;

procedure TFileViewWithMainCtrl.MainControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  FileIndex: PtrInt;
  AtFileList: Boolean;
  AFile, APreviousFile: TDisplayFile;
begin
  FileIndex := GetFileIndexFromCursor(X, Y, AtFileList);
  if not AtFileList then
    Exit;

  SetFocus;

  // history navigation for mice with extra buttons
  case Button of
    mbExtra1:
      begin
        GoToPrevHistory;
        Exit;
      end;
    mbExtra2:
      begin
        GoToNextHistory;
        Exit;
      end;
  end;

  if IsFileIndexInRange(FileIndex) then
  begin
    AFile := FFiles[FileIndex];
    FMainControlLastMouseButton := Button;

    case Button of
      mbRight:
      begin
        SetActiveFile(FileIndex);

        if gMouseSelectionEnabled and (gMouseSelectionButton = 1) then
        begin
          FMouseSelectionStartIndex := FileIndex;
          FMouseSelectionLastState := not AFile.Selected;
          tmContextMenu.Enabled:= True; // start context menu timer
          MarkFile(AFile, FMouseSelectionLastState, False);
          DoSelectionChanged(FileIndex);
        end;
      end;

      mbLeft:
      begin
        if gMouseSelectionEnabled then
        begin
          if ssCtrl in Shift then
            begin
              // if there is no selected files then select also previous file
              if not HasSelectedFiles then
              begin
                APreviousFile := GetActiveDisplayFile;
                if Assigned(APreviousFile) and (APreviousFile <> AFile) then
                  MarkFile(APreviousFile, True, False);
              end;
              InvertFileSelection(AFile, False);
              DoSelectionChanged(FileIndex);
            end
          else if ssShift in Shift then
            begin
              FRangeSelecting := True;
              SelectRange(FileIndex);
            end
          else if (gMouseSelectionButton = 0) then
            begin
              if not AFile.Selected then
                MarkFiles(False);
            end;
        end;//of mouse selection handler
      end;
    else
      SetActiveFile(FileIndex);
    end;

    { Dragging }

    // Check if not already dragging (started by a different button)
    // and if the mouse button is not used for selection.
    if not MainControl.Dragging and
       not (gMouseSelectionEnabled and (Button = mbRight) and (gMouseSelectionButton = Integer(Button))) then
    begin
      // indicate that drag start at next mouse move event
      FStartDrag := True;
      FDragStartPoint.X := X;
      FDragStartPoint.Y := Y;
      FDragFileIndex := FileIndex;
      uDragDropEx.TransformDragging := False;
      uDragDropEx.AllowTransformToInternal := True;
    end;
  end
  else // if mouse on empty space
    begin
      if (Button = mbRight) and (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
        tmContextMenu.Enabled:= True; // start context menu timer
    end;
end;

procedure TFileViewWithMainCtrl.MainControlMouseLeave(Sender: TObject);
begin
  if (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
    FMainControlMouseDown:= False;
end;

procedure TFileViewWithMainCtrl.MainControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  AFile: TDisplayFile;
  ExpectedButton: TShiftStateEnum;
  FileIndex: PtrInt;
  AtFileList: Boolean;
  SelStartIndex, SelEndIndex: Integer;
begin
  if FMainControlMouseDown and MainControl.Dragging then
  begin
    // If dragging has started then clear MouseDown flag.
    if (Abs(FDragStartPoint.X - X) > DragManager.DragThreshold) or
       (Abs(FDragStartPoint.Y - Y) > DragManager.DragThreshold) then
    begin
      FMainControlMouseDown := False;
    end;
  end;

  // If dragging is currently in effect, the window has mouse capture and
  // we can retrieve the window over which the mouse cursor currently is.
  if MainControl.Dragging and uDragDropEx.IsExternalDraggingSupported then
  begin
    Point := MainControl.ClientToScreen(Classes.Point(X, Y));

    // use specifically LCLIntf.WindowFromPoint to avoid confusion with Windows.WindowFromPoint
    if LCLIntf.WindowFromPoint(Point) = 0 then
    begin
      // If result is 0 then the window belongs to another process
      // and we transform intra-process dragging into inter-process dragging.

      TransformDraggingToExternal(Point);
    end;
  end

  else

  // if we are about to start dragging
  if FStartDrag then
    begin
      FStartDrag := False;

      case FMainControlLastMouseButton of
        mbLeft   : ExpectedButton := ssLeft;
        mbMiddle : ExpectedButton := ssMiddle;
        mbRight  : ExpectedButton := ssRight;
        else       Exit;
      end;

      // Make sure the same mouse button is still pressed.
      if not (ExpectedButton in Shift) then
      begin
        ClearAfterDragDrop;
      end
      else if IsFileIndexInRange(FDragFileIndex) then
      begin
        AFile := FFiles[FDragFileIndex];
        // Check if valid item is being dragged.
        if IsItemValid(AFile) then
        begin
          MainControl.BeginDrag(False);
        end;
      end;
    end;

  // Show file info tooltip.
  if ShowHint and
     not MainControl.Dragging and ([ssLeft, ssMiddle, ssRight] * Shift = []) then
    begin
      FileIndex := GetFileIndexFromCursor(X, Y, AtFileList);
      if FileIndex <> FHintFileIndex then
        begin
          FHintFileIndex := FileIndex;
          Application.CancelHint;
          MainControl.Hint:= EmptyStr; // don't show by default

          if IsFileIndexInRange(FileIndex) then
            DoMainControlShowHint(FHintFileIndex, X, Y);
        end;
    end;

  // Selection with right mouse button, if enabled.
  if FMainControlMouseDown and (FMainControlLastMouseButton = mbRight) and
     gMouseSelectionEnabled and (gMouseSelectionButton = 1) then
  begin
    FileIndex := GetFileIndexFromCursor(X, Y, AtFileList);
    if IsFileIndexInRange(FileIndex) and
       (GetActiveFileIndex <> FileIndex) then
    begin
      tmContextMenu.Enabled:= False; // stop context menu timer
      if FMouseSelectionStartIndex < FileIndex then
      begin
        SelStartIndex := FMouseSelectionStartIndex;
        SelEndIndex := FileIndex;
      end
      else
      begin
        SelStartIndex := FileIndex;
        SelEndIndex := FMouseSelectionStartIndex;
      end;

      SetActiveFile(FileIndex);
      MarkFiles(SelStartIndex, SelEndIndex, FMouseSelectionLastState);
    end;
  end;
end;

procedure TFileViewWithMainCtrl.MainControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FStartDrag := False;
  FRangeSelecting := False;

  // Handle only if button-up was not lifted to finish drag&drop operation.
  if not FMainControlMouseDown then
    Exit;

  FMainControlMouseDown := False;
end;

procedure TFileViewWithMainCtrl.MainControlShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  AFile: TDisplayFile;
  sHint: UTF8String;
begin
  if (HintInfo^.HintStr = EmptyStr) or not IsFileIndexInRange(FHintFileIndex) then
    Exit; // don't show

  AFile := FFiles[FHintFileIndex];
  if not AFile.FSFile.IsDirectory then
    begin
      sHint:= GetFileInfoToolTip(FileSource, AFile.FSFile);
      with HintInfo^ do
      begin
        if (sHint = EmptyStr) and (HintStr = #32) then  // no tooltip
          HintStr:= EmptyStr
        else if (sHint <> EmptyStr) then // has tooltip
          begin
            if HintStr = #32 then // without name
              HintStr:= sHint
            else
              HintStr:= HintStr + LineEnding + sHint;
          end;
      end;
    end;
end;

procedure TFileViewWithMainCtrl.MainControlUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  // check if ShiftState is equal to quick search / filter modes
  if quickSearch.CheckSearchOrFilter(UTF8Key) then
    Exit;
end;

function TFileViewWithMainCtrl.OnExDragBegin: Boolean;
begin
  Result := True;
end;

function TFileViewWithMainCtrl.OnExDragEnd: Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  startPoint: TPoint;
  currentPoint: TPoint;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  // On windows dragging can be transformed back into internal.
  // Check if drag was aborted due to mouse moving back into
  // the application window or the user just cancelled it.
  if TransformDragging and (FDragDropSource.GetLastStatus = DragDropAborted) then
  begin
    // Transform to internal dragging again.

    // Save current mouse position.
    GetCursorPos(currentPoint);

    // Temporarily set cursor position to the point where the drag was started
    // so that DragManager can properly read the control being dragged.
    startPoint := MainControl.ClientToScreen(FDragStartPoint);
    SetCursorPos(startPoint.X, startPoint.Y);

    // Begin internal dragging.
    MainControl.BeginDrag(True);

    // Move cursor back.
    SetCursorPos(currentPoint.X, currentPoint.Y);

    // Clear flag.
    TransformDragging := False;

    Exit(True);
  end;
{$ENDIF}

  ClearAfterDragDrop;

  Result := True;
end;

function TFileViewWithMainCtrl.OnExDragEnter(var DropEffect: TDropEffect; ScreenPoint: TPoint): Boolean;
begin
  Result := True;
end;

function TFileViewWithMainCtrl.OnExDragLeave: Boolean;
begin
  SetDropFileIndex(-1);
  Result := True;
end;

function TFileViewWithMainCtrl.OnExDragOver(var DropEffect: TDropEffect; ScreenPoint: TPoint): Boolean;
var
  ClientPoint: TPoint;
  AFile: TDisplayFile;
  FileIndex: PtrInt;
  AtFileList: Boolean;
begin
  // Dropping into empty panel allowed.
  Result := True;

  ClientPoint := MainControl.ScreenToClient(ScreenPoint);
  FileIndex := GetFileIndexFromCursor(ClientPoint.x, ClientPoint.y, AtFileList);

  if IsFileIndexInRange(FileIndex) then
  begin
    // Get the file over which there is something dragged.
    AFile := FFiles[FileIndex];

    // If it is a directory or link mark possibility of drop.
    if (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) then
      SetDropFileIndex(FileIndex)
    else
      SetDropFileIndex(-1);
  end
  else
    SetDropFileIndex(-1);
end;

function TFileViewWithMainCtrl.OnExDrop(const FileNamesList: TStringList; DropEffect: TDropEffect; ScreenPoint: TPoint): Boolean;
var
  AFiles: TFiles;
  DropParams: TDropParams;
begin
  if FileNamesList.Count > 0 then
  begin
    AFiles := TFileSystemFileSource.CreateFilesFromFileList(
        ExtractFilePath(FileNamesList[0]), FileNamesList);
    try
      DropParams := TDropParams.Create(
        AFiles, DropEffect, ScreenPoint, True,
        nil, Self, Self.FileSource, Self.CurrentPath);

      frmMain.DropFiles(DropParams);
    except
      FreeAndNil(AFiles);
      raise;
    end;
  end;

  SetDropFileIndex(-1);
  Result := True;
end;

procedure TFileViewWithMainCtrl.SetActive(bActive: Boolean);
begin
  inherited SetActive(bActive);
  MainControl.Color := DimColor(gBackColor);
end;

procedure TFileViewWithMainCtrl.SetDropFileIndex(NewFileIndex: PtrInt);
var
  OldDropIndex: PtrInt;
begin
  if FDropFileIndex <> NewFileIndex then
  begin
    OldDropIndex := FDropFileIndex;

    // Set new index before redrawing.
    FDropFileIndex := NewFileIndex;

    if IsFileIndexInRange(OldDropIndex) then
      RedrawFile(OldDropIndex);
    if IsFileIndexInRange(NewFileIndex) then
      RedrawFile(NewFileIndex);
  end;
end;

procedure TFileViewWithMainCtrl.SetFocus;
begin
  // CanFocus checks parent controls, but not parent form.
  if GetParentForm(Self).CanFocus and MainControl.CanFocus then
    MainControl.SetFocus;
end;

procedure TFileViewWithMainCtrl.SetMainControl(AValue: TWinControl);
begin
  if FMainControl = AValue then Exit;
  FMainControl := AValue;

  FMainControl.OnEnter        := @MainControlEnter;
  FMainControl.OnExit         := @MainControlExit;
  FMainControl.OnKeyDown      := @MainControlKeyDown;
  FMainControl.OnKeyUp        := @MainControlKeyUp;
  FMainControl.OnShowHint     := @MainControlShowHint;
  FMainControl.OnUTF8KeyPress := @MainControlUTF8KeyPress;

  TControlHandlersHack(FMainControl).OnDblClick   := @MainControlDblClick;
  TControlHandlersHack(FMainControl).OnDragDrop   := @MainControlDragDrop;
  TControlHandlersHack(FMainControl).OnDragOver   := @MainControlDragOver;
  TControlHandlersHack(FMainControl).OnEndDrag    := @MainControlEndDrag;
  TControlHandlersHack(FMainControl).OnMouseDown  := @MainControlMouseDown;
  TControlHandlersHack(FMainControl).OnMouseLeave := @MainControlMouseLeave;
  TControlHandlersHack(FMainControl).OnMouseMove  := @MainControlMouseMove;
  TControlHandlersHack(FMainControl).OnMouseUp    := @MainControlMouseUp;

  HotMan.Register(MainControl, 'Files Panel');
end;

procedure TFileViewWithMainCtrl.tmContextMenuTimer(Sender: TObject);
var
  AFile: TDisplayFile;
  ClientPoint, MousePoint: TPoint;
  Background: Boolean;
  FileIndex: PtrInt;
  AtFileList: Boolean;
begin
  FMainControlMouseDown:= False;
  tmContextMenu.Enabled:= False; // stop context menu timer

  MousePoint  := Mouse.CursorPos;
  ClientPoint := MainControl.ScreenToClient(MousePoint);
  FileIndex   := GetFileIndexFromCursor(ClientPoint.x, ClientPoint.y, AtFileList);
  Background  := not IsFileIndexInRange(FileIndex);

  if not Background then
  begin
    AFile := FFiles[FileIndex];
    MarkFile(AFile, not FMouseSelectionLastState, False);
    DoSelectionChanged(FileIndex);
  end;

  frmMain.Commands.DoContextMenu(Self, MousePoint.x, MousePoint.y, Background);
end;

procedure TFileViewWithMainCtrl.TransformDraggingToExternal(ScreenPoint: TPoint);
begin
  // Set flag temporarily before stopping internal dragging,
  // so that triggered events will know that dragging is transforming.
  TransformDragging := True;

  // Stop internal dragging
  DragManager.DragStop(False);

{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  // Under GTK, DragManager does not release it's mouse capture on
  // DragStop(). We must release it here manually or LCL will get confused
  // with who "owns" the capture after the GTK drag&drop finishes.
  ReleaseMouseCapture;
{$ENDIF}

  // Clear flag before starting external dragging.
  TransformDragging := False;

  // Start external dragging.
  // On Windows it does not return until dragging is finished.

  if IsFileIndexInRange(FDragFileIndex) then
  begin
    BeginDragExternal(FFiles[FDragFileIndex],
                      FDragDropSource,
                      FMainControlLastMouseButton,
                      ScreenPoint);
  end;
end;

{$IFDEF LCLGTK2}
function TFileViewWithMainCtrl.TooManyDoubleClicks: Boolean;
begin
  Result := ((Now - FLastDoubleClickTime) <= ((1/86400)*(DblClickTime/1000)));
end;
{$ENDIF}

procedure TFileViewWithMainCtrl.WorkerFinished(const Worker: TFileViewWorker);
begin
  inherited WorkerFinished(Worker);
  MainControl.Cursor := crDefault;
  UpdateInfoPanel; // Update status line only
end;

procedure TFileViewWithMainCtrl.WorkerStarting(const Worker: TFileViewWorker);
begin
  inherited WorkerStarting(Worker);
  MainControl.Cursor := crHourGlass;
  UpdateInfoPanel; // Update status line only
end;

end.

