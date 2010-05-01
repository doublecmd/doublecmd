unit fModView;

{$mode objfpc}{$H+}

interface

uses
  LResources, LClType,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, FileUtil, ExtCtrls, ButtonPanel, ComCtrls;

type

  { TfrmModView }

  TfrmModView = class(TForm)
    btnOk: TSpeedButton;
    btnCancel: TSpeedButton;
    teQuality: TEdit;
    Label1: TLabel;
    pnlQuality: TPanel;
    teWidth: TEdit;
    teHeight: TEdit;
    lblWidth: TLabel;
    lblHeight: TLabel;
    pnlSize: TPanel;
    btnProportion: TSpeedButton;
    tbQuality: TTrackBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnProportionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbQualityMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure teHeightKeyPress(Sender: TObject; var Key: char);
    procedure teHeightKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure teQualityKeyPress(Sender: TObject; var Key: char);
    procedure teQualityKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure teWidthKeyPress(Sender: TObject; var Key: char);
    procedure teWidthKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    prX, prY: integer;
    { private declarations }
  public
    { public declarations }
  end; 

//var
  //frmModView: TfrmModView;

implementation

procedure TfrmModView.btnProportionClick(Sender: TObject);
begin
  btnProportion.ShowCaption:= not (btnProportion.ShowCaption);
end;

procedure TfrmModView.FormShow(Sender: TObject);
begin
  if pnlSize.Visible then
    begin
      prX:=StrToInt(teWidth.Text);
      prY:=StrToInt(teHeight.Text);
    end;
end;

procedure TfrmModView.tbQualityMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  teQuality.Text:= intToStr(tbQuality.Position);
end;

procedure TfrmModView.btnOkClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TfrmModView.btnCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TfrmModView.teHeightKeyPress(Sender: TObject; var Key: char);
begin
  if  not (key in ['0'..'9', #8]) then key:=#0;
end;

procedure TfrmModView.teHeightKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if btnProportion.ShowCaption then
   begin
     teWidth.Text := IntToStr(round(StrToInt(teHeight.Text)*prX/prY));
   end;
end;

procedure TfrmModView.teQualityKeyPress(Sender: TObject; var Key: char);
begin
  if  not (key in ['0'..'9', #8]) then key:=#0;
end;

procedure TfrmModView.teQualityKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if StrToInt(teQuality.Text)<=100 then
  tbQuality.Position:=StrToInt(teQuality.Text);
end;

procedure TfrmModView.teWidthKeyPress(Sender: TObject; var Key: char);
begin
  if  not (key in ['0'..'9', #8]) then key:=#0;
end;

procedure TfrmModView.teWidthKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if btnProportion.ShowCaption then
   begin
     teHeight.Text := IntToStr(round(StrToInt(teWidth.Text)*prY/prX));
   end;
end;

initialization
  {$I fmodview.lrs}

end.

