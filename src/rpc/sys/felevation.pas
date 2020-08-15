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
  public
    function ShowModal: Integer; override;
  end;

function ShowElevation(const ATitle, AText: String): TMyMsgResult;

implementation

{$R *.lfm}

{$IF DEFINED(MSWINDOWS)}
uses
  Windows, uBitmap;
{$ENDIF}

type
  TElevationData = class
    FResult: TMyMsgResult;
    FTitle, FText: String;
    procedure ShowElevation;
  public
    constructor Create(const ATitle, AText: String);
  end;

function ShowElevation(const ATitle, AText: String): TMyMsgResult;
begin
  with TElevationData.Create(ATitle, AText) do
  try
    TThread.Synchronize(nil, @ShowElevation);
    Result:= FResult;
  finally
    Free
  end;
end;

{ TElevationData }

procedure TElevationData.ShowElevation;
begin
  with TfrmElevation.Create(Application) do
  try
    Caption:= FTitle;
    lblText.Caption:= FText;

    ShowModal;

    if (ModalResult <> mrOK) then
    begin
      if chkElevateAll.Checked then
        FResult:= mmrSkipAll
      else
        FResult:= mmrSkip
    end
    else begin
      if chkElevateAll.Checked then
        FResult:= mmrAll
      else
        FResult:= mmrOK;
    end;

  finally
    Free;
  end;
end;

constructor TElevationData.Create(const ATitle, AText: String);
begin
  FText:= AText;
  FTitle:= ATitle;
end;

{ TfrmElevation }

function TfrmElevation.ShowModal: Integer;
{$IF DEFINED(MSWINDOWS)}
const
  IDI_SHIELD = PAnsiChar(32518);
var
  hIcon: THandle;
  AIcon: Graphics.TBitmap;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  hIcon:= LoadImage(0, IDI_SHIELD, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
  if (hIcon <> 0) then
  begin
    AIcon:= BitmapCreateFromHICON(hIcon);
    imgShield.Picture.Assign(AIcon);
    AIcon.Free;
  end;
{$ENDIF}
  Result:= inherited ShowModal;
end;

end.

