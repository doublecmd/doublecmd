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
    lblProgress: TLabel;
    lblCount: TLabel;
    lblOperationsCount: TLabel;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure OnUpdateTimer(Sender: TObject);

  private
    procedure UpdateView(Operation: TFileSourceOperation);

  public

  end;

var
  frmViewOperations: TfrmViewOperations = nil;

implementation

uses
  uOperationsManager, lclproc;

{ TfrmViewOperations }

procedure TfrmViewOperations.FormCreate(Sender: TObject);
begin
  lblCount.Caption := '0';
  lblProgress.Caption := '0 %';

  OperationsManager.OnOperationAdded := @UpdateView;
  OperationsManager.OnOperationRemoved := @UpdateView;
  OperationsManager.OnOperationStarted := @UpdateView;
  OperationsManager.OnOperationFinished := @UpdateView;
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
    Operation := OperationsManager.OperationByIndex[i];
    if Assigned(Operation) then
    begin
      lblProgress.Caption := IntToStr(Operation.Progress) + ' %';
    end;
  end;
end;

procedure TfrmViewOperations.UpdateView(Operation: TFileSourceOperation);
begin
  lblCount.Caption := IntToStr(OperationsManager.OperationsCount);
  lblProgress.Caption := IntToStr(Operation.Progress) + ' %';
end;

initialization
  {$I fviewoperations.lrs}

end.

