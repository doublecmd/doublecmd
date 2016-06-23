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
  private
    { private declarations }
  public
    { public declarations }
  end;

function ShowMultiRenameWaitForm(TheOwner: TCustomForm): Boolean;

implementation

function ShowMultiRenameWaitForm(TheOwner: TCustomForm): Boolean;
begin
  with TfrmMultiRenameWait.Create(TheOwner) do
  try
    Result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

end.

