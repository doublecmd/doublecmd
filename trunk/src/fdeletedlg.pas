unit fDeleteDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, fButtonForm, uOperationsManager, uFileSource;

type

  { TfrmDeleteDlg }

  TfrmDeleteDlg = class(TfrmButtonForm)
    lblMessage: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

function ShowDeleteDialog(const Message: String; FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier): Boolean;

implementation

uses
  LCLType;

function ShowDeleteDialog(const Message: String; FileSource: IFileSource;
  out QueueId: TOperationsManagerQueueIdentifier): Boolean;
begin
  with TfrmDeleteDlg.Create(Application, FileSource) do
  begin
    Caption:= Application.Title;
    lblMessage.Caption:= Message;
    Result:= ShowModal = mrOK;
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

end.

