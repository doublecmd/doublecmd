unit fMultiRenameWait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, uOSForms;

type

  { TfrmMultiRenameWait }

  TfrmMultiRenameWait = class(TModalDialog)
    ButtonPanel: TButtonPanel;
    lblMessage: TLabel;
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

function ShowMultiRenameWaitForm(const AFileName: String; TheOwner: TCustomForm): Boolean;

implementation

uses
  uShowForm;

function ShowMultiRenameWaitForm(const AFileName: String; TheOwner: TCustomForm): Boolean;
begin
  with TfrmMultiRenameWait.Create(TheOwner) do
  try
    Hint := AFileName;
    Result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmMultiRenameWait }

procedure TfrmMultiRenameWait.FormPaint(Sender: TObject);
begin
  OnPaint := nil;
  ShowEditorByGlob(Hint);
end;

end.

