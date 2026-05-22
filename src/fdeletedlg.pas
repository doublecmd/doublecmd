unit fDeleteDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, fButtonForm, uOperationsManager, uFileSource;

type

  { TfrmDeleteDlg }

  TfrmDeleteDlg = class(TfrmButtonForm)
    chkUseTrash: TCheckBox;
    lblMessage: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chkUseTrashChange(Sender: TObject);
  private
    FMsgTrash: String;
    FMsgNoTrash: String;
  public
    { public declarations }
  end;

function ShowDeleteDialog(TheOwner: TComponent; const Message: String; FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier): Boolean; overload;
function ShowDeleteDialog(TheOwner: TComponent; const TrashMessage, NoTrashMessage: String; FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier; var UseTrash: Boolean): Boolean; overload;

implementation

uses
  LCLType;

function ShowDeleteDialog(TheOwner: TComponent; const Message: String; FileSource: IFileSource;
  out QueueId: TOperationsManagerQueueIdentifier): Boolean;
begin
  with TfrmDeleteDlg.Create(TheOwner, FileSource) do
  begin
    Caption:= Application.Title;
    lblMessage.Caption:= Message;
    Result:= ShowModal = mrOK;
    QueueId:= QueueIdentifier;
    Free;
  end;
end;

function ShowDeleteDialog(TheOwner: TComponent; const TrashMessage, NoTrashMessage: String;
  FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier;
  var UseTrash: Boolean): Boolean;
begin
  with TfrmDeleteDlg.Create(TheOwner, FileSource) do
  begin
    Caption:= Application.Title;
    FMsgTrash:= TrashMessage;
    FMsgNoTrash:= NoTrashMessage;
    chkUseTrash.Visible:= True;
    chkUseTrash.Checked:= UseTrash;
    if UseTrash then
      lblMessage.Caption:= TrashMessage
    else
      lblMessage.Caption:= NoTrashMessage;
    Result:= ShowModal = mrOK;
    if Result then UseTrash:= chkUseTrash.Checked;
    QueueId:= QueueIdentifier;
    Free;
  end;
end;

{$R *.lfm}

{ TfrmDeleteDlg }

procedure TfrmDeleteDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssShift in Shift) then
  begin
    btnOK.Click;
    Key:= 0;
  end;
end;

procedure TfrmDeleteDlg.chkUseTrashChange(Sender: TObject);
begin
  if chkUseTrash.Checked then
    lblMessage.Caption:= FMsgTrash
  else
    lblMessage.Caption:= FMsgNoTrash;
end;

end.

