unit fPrintSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  SpinEx, uOSForms;

type

  { TfrmPrintSetup }

  TfrmPrintSetup = class(TModalForm)
    ButtonPanel: TButtonPanel;
    gbMargins: TGroupBox;
    lblLeft: TLabel;
    lblRight: TLabel;
    lblTop: TLabel;
    lblBottom: TLabel;
    seeLeft: TFloatSpinEditEx;
    seeRight: TFloatSpinEditEx;
    seeTop: TFloatSpinEditEx;
    seeBottom: TFloatSpinEditEx;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

uses
  uGlobs;

{ TfrmPrintSetup }

procedure TfrmPrintSetup.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOK then
  begin
    gPrintMargins.Left:= Round(seeLeft.Value * 10);
    gPrintMargins.Top:= Round(seeTop.Value * 10);
    gPrintMargins.Right:= Round(seeRight.Value * 10);
    gPrintMargins.Bottom:= Round(seeBottom.Value * 10);
  end;
end;

procedure TfrmPrintSetup.FormCreate(Sender: TObject);
begin
  seeLeft.Value:= gPrintMargins.Left / 10;
  seeTop.Value:= gPrintMargins.Top / 10;
  seeRight.Value:= gPrintMargins.Right / 10;
  seeBottom.Value:= gPrintMargins.Bottom / 10;
end;

end.

