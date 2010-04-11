unit fViewOperations; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, uFileSourceOperation, uOperationsManager;

type

  { TfrmViewOperations }

  TfrmViewOperations = class(TForm)
    AllProgressLabel: TLabel;
    CheckQueueBox: TCheckBox;
    CurrentOperationPanel: TPanel;
    CurrentOperationLabel: TLabel;
    Queue: TLabel;
    AllButtonsLable: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lblCount: TLabel;
    lblOperationsCount: TLabel;
    AllProgressBar: TProgressBar;
    AllOperationPanel: TPanel;
    Cntr_running: TPanel;
    AllPause: TSpeedButton;
    AllStart: TSpeedButton;
    CurOpQueueInOut: TSpeedButton;
    AllInQueue: TSpeedButton;
    RunAllStart: TSpeedButton;
    StartQueue: TSpeedButton;
    UpCurOp: TSpeedButton;
    DnCurOp: TSpeedButton;
    StartPauseCurOp: TSpeedButton;
    CancelCurOp: TSpeedButton;
    VOMainPanel: TPanel;
    sboxOperations: TScrollBox;
    RunAllPause: TSpeedButton;
    AllCancel: TSpeedButton;
    UpdateTimer: TTimer;

    procedure AllInQueueClick(Sender: TObject);
    procedure AllPauseClick(Sender: TObject);
    procedure AllStartClick(Sender: TObject);
    procedure CancelCurOpClick(Sender: TObject);
    procedure DnCurOpClick(Sender: TObject);
    procedure RunAllPauseClick(Sender: TObject);
    procedure AllCancelClick(Sender: TObject);
    procedure RunAllStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnUpdateTimer(Sender: TObject);
    procedure sboxOperationsDblClick(Sender: TObject);
    procedure sboxOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sboxOperationsPaint(Sender: TObject);
    procedure CurOpQueueInOutClick(Sender: TObject);
    procedure StartPauseCurOpClick(Sender: TObject);
    procedure StartQueueClick(Sender: TObject);
    procedure UpCurOpClick(Sender: TObject);

  private
    procedure UpdateView(Operation: TFileSourceOperation; Event: TOperationManagerEvent);

  public

  end;

var
  frmViewOperations: TfrmViewOperations = nil;
  indexFocus: integer;

implementation

uses
  uFileSourceOperationTypes,
  uLng, fFileOpDlg;

const
  aRowHeight = 50;

{ TfrmViewOperations }

procedure TfrmViewOperations.FormCreate(Sender: TObject);
begin

  lblCount.Caption := '0';
  indexFocus := 1;
  sboxOperations.AutoScroll := True;
  sboxOperations.VertScrollBar.Visible := True;

  OperationsManager.AddEventsListener([omevOperationAdded, omevOperationRemoved], @UpdateView);

  lblCount.Caption := IntToStr(OperationsManager.OperationsCount);
  sboxOperations.Invalidate;     // force redraw
end;

procedure TfrmViewOperations.RunAllPauseClick(Sender: TObject);
begin
  OperationsManager.PauseRunning;
end;

procedure TfrmViewOperations.AllPauseClick(Sender: TObject);
begin
  OperationsManager.PauseAll;
end;

procedure TfrmViewOperations.AllInQueueClick(Sender: TObject);
var
i: integer;
begin
  for i:=0 to OperationsManager.OperationsCount-1 do
   OperationsManager.InQueue (OperationsManager.GetHandleById(i), true);
end;

procedure TfrmViewOperations.AllStartClick(Sender: TObject);
begin
  OperationsManager.StartAll;
end;

procedure TfrmViewOperations.CancelCurOpClick(Sender: TObject);
begin
  OperationsManager.GetOperationByIndex(indexFocus).Stop;
end;

procedure TfrmViewOperations.DnCurOpClick(Sender: TObject);
begin
  if indexFocus < OperationsManager.OperationsCount-1 then
  begin
 OperationsManager.MoveOperation(indexFocus, indexFocus + 1);
  indexFocus:= indexFocus+1;
  end;
end;

procedure TfrmViewOperations.AllCancelClick(Sender: TObject);
begin
  OperationsManager.CancelAll;
end;

procedure TfrmViewOperations.RunAllStartClick(Sender: TObject);
begin
  OperationsManager.StartRunning;
end;

procedure TfrmViewOperations.FormDestroy(Sender: TObject);
begin
  OperationsManager.RemoveEventsListener([omevOperationAdded, omevOperationRemoved], @UpdateView);
end;

procedure TfrmViewOperations.OnUpdateTimer(Sender: TObject);
var
  Operation: TFileSourceOperation;
  i: Integer;
begin
  if indexFocus > OperationsManager.OperationsCount-1 then
    indexFocus := OperationsManager.OperationsCount-1;           //Если фокус ушел за пределы, то делаем его последним

  if OperationsManager.OperationsCount=0 then
    begin
      CurrentOperationPanel.Enabled:=false;
      AllOperationPanel.Enabled:=false;
    end
  else
    begin
      CurrentOperationPanel.Enabled:=true;
      AllOperationPanel.Enabled:=true;
    end;

  AllProgressBar.Position:= OperationsManager.AllProgressPoint;

  if AllProgressBar.Position <> 0 then
    AllProgressLabel.Caption:= 'Progress all operation '+  IntToStr(AllProgressBar.Position)+' %'
  else
    AllProgressLabel.Caption:= 'All operation complete';

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
  OperationDialog: TfrmFileOp;
begin
  CursorPos := Mouse.CursorPos;
  CursorPos := sboxOperations.ScreenToClient(CursorPos);

  OperationNumber := CursorPos.Y div aRowHeight;
  if OperationsManager.GetFormCreate(OperationsManager.GetHandleById(OperationNumber))= false then
  begin
  OperationDialog := TfrmFileOp.Create(OperationsManager.GetHandleById(OperationNumber));
  OperationDialog.Show;
  end;
end;

procedure TfrmViewOperations.sboxOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OperationNumber: Integer;
  CursorPos: TPoint;
begin
  CursorPos := Mouse.CursorPos;
  CursorPos := sboxOperations.ScreenToClient(CursorPos);

  OperationNumber := CursorPos.Y div aRowHeight;

  if OperationsManager.OperationsCount > 0 then
  begin
  case Button of
    mbMiddle:
      begin
        if OperationNumber-1>=0 then //проверка, если операция самая первая, то наверх уже не сдвинешь(((
        begin
          if  (OperationNumber = indexFocus) or (OperationNumber-1 = indexFocus) then     // изменяем фокус, если на одной из функций был фокус
          begin
            if  OperationNumber=indexFocus then indexFocus:=indexFocus-1 else indexFocus:=indexFocus+1;
          end;
          OperationsManager.MoveOperation(OperationNumber, OperationNumber - 1);
        end;
      end;
    mbRight:
      begin
        if OperationNumber<OperationsManager.OperationsCount then    //если операция последняя, то ничего уже не сделаешь(((
        begin
          if  (OperationNumber = indexFocus) or (OperationNumber+1 = indexFocus) then     // изменяем фокус, если на одной из функций был фокус
          begin
            if  OperationNumber=indexFocus then indexFocus:=indexFocus+1 else indexFocus:=indexFocus-1;
          end;
          OperationsManager.MoveOperation(OperationNumber, OperationNumber + 1);
        end;
      end;
    mbLeft:
      begin
        if OperationNumber< OperationsManager.OperationsCount then indexFocus:=OperationNumber;
      end;
  end;
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
        fsoCopy, fsoCopyIn, fsoCopyOut:
          OutString := rsDlgCp;
        fsoMove:
          OutString := rsDlgMv;
        fsoDelete:
          OutString := rsDlgDel;
        fsoWipe:
          OutString := rsDlgWipe;
        fsoCalcChecksum:
          OutString := rsDlgCheckSumCalc;
        else
          OutString := 'Unknown operation';
      end;

      OutString := IntToStr(OperationHandle) + ': '
                 + OutString + ' - '
                 + IntToStr(Operation.Progress) + ' %'
                 + ' (' + FileSourceOperationStateText[Operation.State] + ')';

      StartingState := OperationsManager.GetStartingState(OperationHandle);
      if not (StartingState in [ossInvalid, ossManualStart]) then
        OutString := OutString + ' [' + OperationStartingStateText[StartingState] + ']';

      sboxOperations.Canvas.Brush.Color := Canvas.Brush.Color;
      sboxOperations.Canvas.Rectangle(0, 0 + (aRowHeight * i), sboxOperations.Width, aRowHeight + (aRowHeight * i));
      sboxOperations.Canvas.TextOut(5, 5 + (aRowHeight * i), OutString);
      sboxOperations.Caption := OutString;

      if i<> indexFocus then sboxOperations.Canvas.Brush.Color := clMenu else
      begin
      sboxOperations.Canvas.Brush.Color := clHighlight;                    // изменение цвета полоски если на ней фокус
      CurrentOperationLabel.Caption:=OutString;                              // загаловок для текущей операции в CurrentOperation panel определяется индексом

      if (StartingState in [ossQueueFirst, ossQueueLast, ossQueueIn]) then
      CurOpQueueInOut.Caption:='Out' else CurOpQueueInOut.Caption:='In';
      if Operation.State=fsosRunning then
      StartPauseCurOp.Caption:='||' else StartPauseCurOp.Caption:='Start';

      end;

      sboxOperations.Canvas.FillRect(
        5,
        5 + (aRowHeight * i) + sboxOperations.Canvas.TextHeight('Pg'),
        5 + (sboxOperations.Width - 10) * Operation.Progress div 100,
        aRowHeight * (i + 1) - 5);

     end;

  end;

end;

procedure TfrmViewOperations.CurOpQueueInOutClick(Sender: TObject);
begin
  if (OperationsManager.GetStartingState(OperationsManager.GetHandleById(indexFocus)) in [ossQueueFirst, ossQueueLast, ossQueueIn])  then
    begin
      OperationsManager.InQueue(OperationsManager.GetHandleById(indexFocus), false);
      CurOpQueueInOut.Caption:= 'In';
    end
  else
    begin
      OperationsManager.InQueue(OperationsManager.GetHandleById(indexFocus), true);
      CurOpQueueInOut.Caption:= 'Out';
    end;
end;

procedure TfrmViewOperations.StartPauseCurOpClick(Sender: TObject);
begin
  if OperationsManager.GetOperationByIndex(indexFocus).State=fsosRunning then
    begin
      OperationsManager.GetOperationByIndex(indexFocus).pause;
      StartPauseCurOp.Caption:= 'Start';
      OperationsManager.CheckQueuedOperations;
    end
  else
    begin
      OperationsManager.GetOperationByIndex(indexFocus).Start;
      OperationsManager.SetPauseRunning(OperationsManager.GetHandleById(indexFocus), False);
      StartPauseCurOp.Caption:= '||';
    end;
end;

procedure TfrmViewOperations.StartQueueClick(Sender: TObject);
begin
  OperationsManager.CheckQueuedOperations;
end;

procedure TfrmViewOperations.UpCurOpClick(Sender: TObject);
begin
  if indexFocus>0 then
  begin
   OperationsManager.MoveOperation(indexFocus, indexFocus - 1);
   indexFocus:= indexFocus-1;
  end;
end;


procedure TfrmViewOperations.UpdateView(Operation: TFileSourceOperation;
                                        Event: TOperationManagerEvent);
begin

  if OperationsManager.OperationsCount=0 then
  begin
  indexFocus:=-1;                                                //сброс индекса и
  CurrentOperationLabel.Caption:='Caption of operation';         // заголовка, если нет операций в фоне
  end;
  lblCount.Caption := IntToStr(OperationsManager.OperationsCount);
  sboxOperations.Invalidate;     // force redraw
end;

initialization
  {$I fviewoperations.lrs}

end.

