unit fDeleteDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, fButtonForm, uOperationsManager;

type

  { TfrmDeleteDlg }

  TfrmDeleteDlg = class(TfrmButtonForm)
    lblMessage: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

function ShowDeleteDialog(const Message: String; out QueueId: TOperationsManagerQueueIdentifier): Boolean;

implementation

function ShowDeleteDialog(const Message: String; out
  QueueId: TOperationsManagerQueueIdentifier): Boolean;
begin
  with TfrmDeleteDlg.Create(Application) do
  begin
    Caption:= Application.Title;
    lblMessage.Caption:= Message;
    Constraints.MinWidth:= 400;
    Constraints.MaxWidth:= 800;
    Result:= ShowModal = mrOK;
    QueueId:= QueueIdentifier;
    Free;
  end;
end;

{$R *.lfm}

end.

