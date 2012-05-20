unit fModView;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, uOSForms;

type

  { TfrmModView }

  TfrmModView = class(TModalForm)
    btnOk: TSpeedButton;
    btnCancel: TSpeedButton;
    btnPath1: TSpeedButton;
    lblPath1: TLabel;
    lblPath2: TLabel;
    lblPath3: TLabel;
    lblPath4: TLabel;
    lblPath5: TLabel;
    pnlButtons: TPanel;
    rbPnm: TRadioButton;
    rbIco: TRadioButton;
    rbPng: TRadioButton;
    rbBmp: TRadioButton;
    rbJpg: TRadioButton;
    rbPath5: TRadioButton;
    rbPath4: TRadioButton;
    rbPath3: TRadioButton;
    rbPath2: TRadioButton;
    rbPath1: TRadioButton;
    btnPath2: TSpeedButton;
    btnPath3: TSpeedButton;
    btnPath4: TSpeedButton;
    btnPath5: TSpeedButton;
    tePath5: TEdit;
    tePath4: TEdit;
    tePath3: TEdit;
    sddCopyMoveFile: TSelectDirectoryDialog;
    tePath2: TEdit;
    tePath1: TEdit;
    pnlCopyMoveFile: TPanel;
    teQuality: TEdit;
    lblQuality: TLabel;
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
    procedure btnPathClick(Sender: TObject);
    procedure btnProportionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure rbBmpChange(Sender: TObject);
    procedure rbIcoChange(Sender: TObject);
    procedure rbJpgChange(Sender: TObject);
    procedure rbPngChange(Sender: TObject);
    procedure rbPnmChange(Sender: TObject);
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
    { private declarations }
    prX, prY: integer;
  public
    Path, sExt : string;
    { public declarations }
  end; 

implementation

{$R *.lfm}

uses
  uGlobs;

procedure TfrmModView.btnProportionClick(Sender: TObject);
begin
  btnProportion.ShowCaption:= not (btnProportion.ShowCaption);
end;

procedure TfrmModView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
      gCopyMovePath1 := tePath1.Text;
      gCopyMovePath2 := tePath2.Text;
      gCopyMovePath3 := tePath3.Text;
      gCopyMovePath4 := tePath4.Text;
      gCopyMovePath5 := tePath5.Text;
end;

procedure TfrmModView.FormKeyPress(Sender: TObject; var Key: char);
begin
  if pnlCopyMoveFile.Visible then
    begin
      rbPath1.Checked:= false;
      rbPath2.Checked:= false;
      rbPath3.Checked:= false;
      rbPath4.Checked:= false;
      rbPath5.Checked:= false;
      case Key of
      '1':
        begin
          rbPath1.Checked:= true;
          Key := #0;
          btnOkClick(Sender);
        end;
      '2':
        begin
          rbPath2.Checked:= true;
          Key := #0;
          btnOkClick(Sender);
        end;
      '3':
        begin
          rbPath3.Checked:= true;
          Key := #0;
          btnOkClick(Sender);
        end;
      '4':
        begin
          rbPath4.Checked:= true;
          Key := #0;
          btnOkClick(Sender);
        end;
      '5':
        begin
          rbPath5.Checked:= true;
          Key := #0;
          btnOkClick(Sender);
        end;
      end;
    end;
end;

procedure TfrmModView.FormShow(Sender: TObject);
begin
  if pnlSize.Visible then
    begin
      prX:=StrToInt(teWidth.Text);
      prY:=StrToInt(teHeight.Text);
    end;
  if pnlCopyMoveFile.Visible then
    begin
      rbPath1.SetFocus;
      tePath1.Text := gCopyMovePath1;
      tePath2.Text := gCopyMovePath2;
      tePath3.Text := gCopyMovePath3;
      tePath4.Text := gCopyMovePath4;
      tePath5.Text := gCopyMovePath5;
    end;
  if pnlQuality.Visible then
    begin
      sExt:= '.jpg';
      tbQuality.Enabled:=true;
      lblQuality.Enabled:=True;
    end;
end;

procedure TfrmModView.rbBmpChange(Sender: TObject);
begin
  sExt:='.bmp';
  tbQuality.Enabled:=False;
  teQuality.Enabled:=False;
  lblQuality.Enabled:=False;
end;

procedure TfrmModView.rbIcoChange(Sender: TObject);
begin
  sExt:='.ico';
  tbQuality.Enabled:=False;
  teQuality.Enabled:=False;
  lblQuality.Enabled:=False;
end;

procedure TfrmModView.rbJpgChange(Sender: TObject);
begin
  sExt:='.jpg';
  tbQuality.Enabled:=True;
  teQuality.Enabled:=True;
  lblQuality.Enabled:=True;
end;

procedure TfrmModView.rbPngChange(Sender: TObject);
begin
  sExt:='.png';
  tbQuality.Enabled:=False;
  teQuality.Enabled:=False;
  lblQuality.Enabled:=False;
end;

procedure TfrmModView.rbPnmChange(Sender: TObject);
begin
  sExt:='.pnm';
  tbQuality.Enabled:=False;
  teQuality.Enabled:=False;
  lblQuality.Enabled:=False;
end;

procedure TfrmModView.tbQualityMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  teQuality.Text:= intToStr(tbQuality.Position);
end;

procedure TfrmModView.btnOkClick(Sender: TObject);
begin
  if pnlCopyMoveFile.Visible then
    begin
      if rbPath1.Checked then Path:=tePath1.Text;
      if rbPath2.Checked then Path:=tePath2.Text;
      if rbPath3.Checked then Path:=tePath3.Text;
      if rbPath4.Checked then Path:=tePath4.Text;
      if rbPath5.Checked then Path:=tePath5.Text;
    end;
  ModalResult:= mrOk;
end;

procedure TfrmModView.btnPathClick(Sender: TObject);
begin
  if sddCopyMoveFile.Execute then
    begin
    if sender=btnPath1 then
      begin
      tePath1.Text:= sddCopyMoveFile.Filename;
      rbPath1.Checked:=true;
      end;
    if sender=btnPath2 then
      begin
      tePath2.Text:= sddCopyMoveFile.Filename;
      rbPath2.Checked:=true;
      end;
    if sender=btnPath3 then
      begin
      tePath3.Text:= sddCopyMoveFile.Filename;
      rbPath3.Checked:=true;
      end;
    if sender=btnPath4 then
      begin
      tePath4.Text:= sddCopyMoveFile.Filename;
      rbPath4.Checked:=true;
      end;
    if sender=btnPath5 then
      begin
      tePath5.Text:= sddCopyMoveFile.Filename;
      rbPath5.Checked:=true;
      end;
    end;
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

end.

