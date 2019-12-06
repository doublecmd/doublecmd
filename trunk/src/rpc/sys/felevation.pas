unit fElevation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, uShowMsg;

type

  { TfrmElevation }

  TfrmElevation = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    chkElevateAll: TCheckBox;
    imgShield: TImage;
    lblText: TLabel;
  private
    procedure ShowModalSync;
  public

  end;

function ShowElevation(const ATitle, AText: String): TMyMsgResult;

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows, uIcoFiles;
{$ENDIF}

{$R *.lfm}

function ShowElevation(const ATitle, AText: String): TMyMsgResult;
begin
  with TfrmElevation.Create(Application) do
  try
    Caption:= ATitle;
    lblText.Caption:= AText;

    TThread.Synchronize(nil, @ShowModalSync);

    if (ModalResult <> mrOK) then
    begin
      if chkElevateAll.Checked then
        Result:= mmrSkipAll
      else
        Result:= mmrSkip
    end
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
{$IF DEFINED(MSWINDOWS)}
const
  IDI_SHIELD = PAnsiChar(32518);
var
  hIcon: THandle;
  AIcon: Graphics.TIcon;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  hIcon:= LoadImage(0, IDI_SHIELD, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
  if (hIcon <> 0) then
  begin
    AIcon:= CreateIconFromHandle(hIcon);
    imgShield.Picture.Assign(AIcon);
    AIcon.Free;
  end;
{$ENDIF}
  ModalResult:= ShowModal;
end;

end.

