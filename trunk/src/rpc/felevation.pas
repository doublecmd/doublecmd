unit fElevation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  uShowMsg;

type

  { TfrmElevation }

  TfrmElevation = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    chkElevateAll: TCheckBox;
    ImageList: TImageList;
    lblText: TLabel;
  private
    procedure ShowModalSync;
  public

  end;

function ShowElevation(const ATitle, AText: String): TMyMsgResult;

implementation

{$R *.lfm}

function ShowElevation(const ATitle, AText: String): TMyMsgResult;
begin
  with TfrmElevation.Create(Application) do
  try
    Caption:= ATitle;
    lblText.Caption:= AText;

    TThread.Synchronize(nil, @ShowModalSync);

    if (ModalResult <> mrOK) then
      Result:= mmrCancel
    else begin
      if chkElevateAll.Checked then
        Result:= mmrAll
      else
        Result:= mmrOK;
    end;

  finally
    Free;
  end;
end;

{ TfrmElevation }

procedure TfrmElevation.ShowModalSync;
begin
  ModalResult:= ShowModal;
end;

end.

