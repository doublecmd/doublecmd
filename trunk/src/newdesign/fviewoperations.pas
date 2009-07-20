unit fViewOperations; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  uFileSourceOperation;

type

  { TfrmViewOperations }

  TfrmViewOperations = class(TForm)
    lblCount: TLabel;
    lblOperationsCount: TLabel;
    sboxOperations: TScrollBox;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure OnUpdateTimer(Sender: TObject);
    procedure sboxOperationsDblClick(Sender: TObject);
    procedure sboxOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sboxOperationsPaint(Sender: TObject);

  private
    procedure UpdateView(Operation: TFileSourceOperation);

  public

  end;

var
  frmViewOperations: TfrmViewOperations = nil;

implementation

uses
  uOperationsManager,
  uFileSourceOperationTypes,
  uLng, LCLProc, fFileOpDlg;

const
  aRowHeight = 50;

{ TfrmViewOperations }

procedure TfrmViewOperations.FormCreate(Sender: TObject);
begin
  lblCount.Caption := '0';

  sboxOperations.AutoScroll := True;
  sboxOperations.VertScrollBar.Visible := True;

  OperationsManager.OnOperationAdded := @UpdateView;
  OperationsManager.OnOperationRemoved := @UpdateView;
  OperationsManager.OnOperationStarted := @UpdateView;
  OperationsManager.OnOperationFinished := @UpdateView;

  lblCount.Caption := IntToStr(OperationsManager.OperationsCount);
  sboxOperations.Invalidate;     // force redraw
end;

procedure TfrmViewOperations.OnUpdateTimer(Sender: TObject);
var
  Operation: TFileSourceOperation;
  i: Integer;
begin
  for i := 0 to OperationsManager.OperationsCount - 1 do
  begin
    // Timer is called from main thread, so it is safe
    // to use reference to Operation from OperationsManager.
    Operation := OperationsManager.GetOperationByIndex(i);
    if Assigned(Operation) then
    begin
      sboxOperations.Invalidate;     // force redraw
    end;
  end;
end;

procedure TfrmViewOperations.sboxOperationsDblClick(Sender: TObject);
var
  OperationNumber: Integer;
  CursorPos: TPoint;
  Operation: TFileSourceOperation;
  OperationDialog: TfrmFileOp;
begin
  CursorPos := Mouse.CursorPos;
  CursorPos := sboxOperations.ScreenToClient(CursorPos);

  OperationNumber := CursorPos.Y div aRowHeight;

  OperationDialog := TfrmFileOp.Create(OperationsManager.GetHandleById(OperationNumber));
  OperationDialog.Show;
end;

procedure TfrmViewOperations.sboxOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OperationNumber: Integer;
  CursorPos: TPoint;
  Operation: TFileSourceOperation;
  OperationDialog: TfrmFileOp;
begin
  CursorPos := Mouse.CursorPos;
  CursorPos := sboxOperations.ScreenToClient(CursorPos);

  OperationNumber := CursorPos.Y div aRowHeight;

  case Button of
    mbMiddle:
      OperationsManager.MoveOperation(OperationNumber, OperationNumber - 1);
    mbRight:
      OperationsManager.MoveOperation(OperationNumber, OperationNumber + 1);
  end;
end;

procedure TfrmViewOperations.sboxOperationsPaint(Sender: TObject);
var
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  StartingState: TOperationStartingState;
  i: Integer;
  OutString: String;
begin
  for i := 0 to OperationsManager.OperationsCount - 1 do
  begin
    // Timer is called from main thread, so it is safe
    // to use reference to Operation from OperationsManager.
    Operation := OperationsManager.GetOperationByIndex(i);
    if Assigned(Operation) then
    begin
      OperationHandle := OperationsManager.GetHandleById(i);

      case Operation.ID of
        fsoCopyIn, fsoCopyOut:
          OutString := rsDlgCp;
        else
          OutString := 'Unknown operation';
      end;

      OutString := IntToStr(OperationHandle) + ': '
                 + OutString + ' - '
                 + IntToStr(Operation.Progress) + ' %'
                 + ' (' + FileSourceOperationStateText[Operation.State] + ')';

      StartingState := OperationsManager.GetStartingState(OperationHandle);
      if StartingState <> ossInvalid then
        if StartingState <> ossDontStart then
          OutString := OutString + ' [' + OperationStartingStateText[StartingState] + ']';

      sboxOperations.Canvas.Brush.Color := Canvas.Brush.Color;
      sboxOperations.Canvas.Rectangle(0, 0 + (aRowHeight * i), sboxOperations.Width, aRowHeight + (aRowHeight * i));
      sboxOperations.Canvas.TextOut(5, 5 + (aRowHeight * i), OutString);
      sboxOperations.Caption := OutString;

      sboxOperations.Canvas.Brush.Color := clHighlight;
      sboxOperations.Canvas.FillRect(
        5,
        5 + (aRowHeight * i) + sboxOperations.Canvas.TextHeight('Pg'),
        5 + (sboxOperations.Width - 10) * Operation.Progress div 100,
        aRowHeight * (i + 1) - 5);
    end;
  end;
end;

procedure TfrmViewOperations.UpdateView(Operation: TFileSourceOperation);
begin
  lblCount.Caption := IntToStr(OperationsManager.OperationsCount);
  sboxOperations.Invalidate;     // force redraw
end;

initialization
  {$I fviewoperations.lrs}

end.

