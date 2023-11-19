unit fModView;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ButtonPanel, Spin, uOSForms;

type

  { TfrmModView }

  TfrmModView = class(TModalForm)
    btnPath1: TSpeedButton;
    btnPath2: TSpeedButton;
    btnPath3: TSpeedButton;
    btnPath4: TSpeedButton;
    btnPath5: TSpeedButton;
    btnProportion: TSpeedButton;
    bplButtons: TButtonPanel;
    ImageList: TImageList;
    lblHeight: TLabel;
    lblPath1: TLabel;
    lblPath2: TLabel;
    lblPath3: TLabel;
    lblPath4: TLabel;
    lblPath5: TLabel;
    lblQuality: TLabel;
    lblWidth: TLabel;
    pnlMain: TPanel;
    pnlCopyMoveFile: TPanel;
    pnlQuality: TPanel;
    pnlSize: TPanel;
    rbPath1: TRadioButton;
    rbPath2: TRadioButton;
    rbPath3: TRadioButton;
    rbPath4: TRadioButton;
    rbPath5: TRadioButton;
    sddCopyMoveFile: TSelectDirectoryDialog;
    tbQuality: TTrackBar;
    teHeight: TEdit;
    tePath1: TEdit;
    tePath2: TEdit;
    tePath3: TEdit;
    tePath4: TEdit;
    tePath5: TEdit;
    teQuality: TSpinEdit;
    teWidth: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnPathClick(Sender: TObject);
    procedure btnProportionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure tbQualityChange(Sender: TObject);
    procedure teHeightKeyPress(Sender: TObject; var Key: char);
    procedure teHeightKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure teQualityChange(Sender: TObject);
    procedure teWidthKeyPress(Sender: TObject; var Key: char);
    procedure teWidthKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    prX, prY: integer;
  public
    Path : string;
    { public declarations }
  end; 

implementation

{$R *.lfm}

uses
  uGlobs;

procedure TfrmModView.btnProportionClick(Sender: TObject);
begin
  if btnProportion.Down then
    ImageList.GetBitmap(0, btnProportion.Glyph)
  else begin
    ImageList.GetBitmap(1, btnProportion.Glyph);
  end;
end;

procedure TfrmModView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  gCopyMovePath1 := tePath1.Text;
  gCopyMovePath2 := tePath2.Text;
  gCopyMovePath3 := tePath3.Text;
  gCopyMovePath4 := tePath4.Text;
  gCopyMovePath5 := tePath5.Text;
end;

procedure TfrmModView.FormCreate(Sender: TObject);
begin
  ImageList.GetBitmap(0, btnProportion.Glyph);
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
      tbQuality.Enabled:=true;
      lblQuality.Enabled:=True;
      tbQuality.Position:=gViewerJpegQuality;
      teQuality.Value:= gViewerJpegQuality;
    end;
end;

procedure TfrmModView.tbQualityChange(Sender: TObject);
begin
  teQuality.Value:= tbQuality.Position;
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
  if btnProportion.Down then
  begin
    teWidth.Text := IntToStr(Round(StrToInt(teHeight.Text) * prX / prY));
  end;
end;

procedure TfrmModView.teQualityChange(Sender: TObject);
begin
  tbQuality.Position:= teQuality.Value;
end;

procedure TfrmModView.teWidthKeyPress(Sender: TObject; var Key: char);
begin
  if  not (key in ['0'..'9', #8]) then key:=#0;
end;

procedure TfrmModView.teWidthKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if btnProportion.Down then
  begin
    teHeight.Text := IntToStr(Round(StrToInt(teWidth.Text) * prY / prX));
  end;
end;

end.

