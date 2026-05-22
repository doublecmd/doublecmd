unit fDeleteDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, fButtonForm, uOperationsManager, uFileSource;

type
  TDeleteMode = (dmTrash, dmDelete, dmWipe);

  { TfrmDeleteDlg }

  TfrmDeleteDlg = class(TfrmButtonForm)
    rbTrash: TRadioButton;
    rbDelete: TRadioButton;
    rbWipe: TRadioButton;
    lblMessage: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rbModeChange(Sender: TObject);
  private
    FMessages: array[TDeleteMode] of String;
  public
    { public declarations }
  end;

function ShowDeleteDialog(TheOwner: TComponent; const Message: String; FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier): Boolean; overload;
function ShowDeleteDialog(TheOwner: TComponent; const TrashMessage, DeleteMessage, WipeMessage: String;
  FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier;
  ShowTrash, ShowWipe: Boolean; var DeleteMode: TDeleteMode): Boolean; overload;

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

function ShowDeleteDialog(TheOwner: TComponent; const TrashMessage, DeleteMessage, WipeMessage: String;
  FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier;
  ShowTrash, ShowWipe: Boolean; var DeleteMode: TDeleteMode): Boolean;
begin
  with TfrmDeleteDlg.Create(TheOwner, FileSource) do
  begin
    Caption:= Application.Title;
    FMessages[dmTrash]  := TrashMessage;
    FMessages[dmDelete] := DeleteMessage;
    FMessages[dmWipe]   := WipeMessage;
    rbTrash.Visible  := ShowTrash;
    rbDelete.Visible := True;
    rbWipe.Visible   := ShowWipe;
    // Set initial selection; fall back to dmDelete if the preferred mode is unavailable
    case DeleteMode of
      dmTrash:  if ShowTrash then rbTrash.Checked  := True else rbDelete.Checked := True;
      dmWipe:   if ShowWipe  then rbWipe.Checked   := True else rbDelete.Checked := True;
      else           rbDelete.Checked := True;
    end;
    lblMessage.Caption:= FMessages[DeleteMode];
    Result:= ShowModal = mrOK;
    if Result then
    begin
      if rbTrash.Checked then DeleteMode := dmTrash
      else if rbWipe.Checked then DeleteMode := dmWipe
      else DeleteMode := dmDelete;
    end;
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

procedure TfrmDeleteDlg.rbModeChange(Sender: TObject);
var
  Mode: TDeleteMode;
begin
  if rbTrash.Checked then Mode := dmTrash
  else if rbWipe.Checked then Mode := dmWipe
  else Mode := dmDelete;
  lblMessage.Caption := FMessages[Mode];
end;

end.

