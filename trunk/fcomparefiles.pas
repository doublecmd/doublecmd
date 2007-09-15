unit fCompareFiles;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, fLngForm,
  ComCtrls, Buttons, SynEdit, EditBtn;

type

  { TfrmCompareFiles }

  TfrmCompareFiles = class(TFrmLng)
    edtFileNameLeft: TFileNameEdit;
    edtFileNameRight: TFileNameEdit;
    pnlLeft: TPanel;
    Splitter1: TSplitter;
    pnlRight: TPanel;
    pnlLeftBox: TPanel;
    pnlRightBox: TPanel;
    lstRight: TSynEdit;
    lstLeft: TSynEdit;
    pnlStatusBar: TStatusBar;
    pnlButtons: TPanel;
    btnCompare: TButton;
    btnNextDiff: TButton;
    btnPrevDiff: TButton;
    chbBinMode: TCheckBox;
    btnClose: TButton;
    chbKeepScrolling: TCheckBox;
    procedure btnCompareClick(Sender: TObject);
    procedure lstLeftSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure lstLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure lstRightSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure lstRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LoadLng; override;
  end;

procedure ShowCmpFiles(const sFile1, sFile2:String);

implementation


uses
  uCompareFiles, uLng, uGlobs;

procedure ShowCmpFiles(const sFile1, sFile2:String);
begin
  with TfrmCompareFiles.Create(Application)do
  begin
    try
      edtFileNameLeft.Text:=sFile1;
      edtFileNameRight.Text:=sFile2;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmCompareFiles.LoadLng;
begin
  lstLeft.Font.Name:=gEditorFontName;
  lstLeft.Font.Style:=[];
  lstRight.Font.Name:=gEditorFontName;
  lstLeft.Font.Size:=gEditorSize;
  lstRight.Font.Size:=gEditorSize;
  lstRight.Font.Style:=[];
end;

procedure TfrmCompareFiles.btnCompareClick(Sender: TObject);
var
  iChanges : integer;
begin
  if chbBinMode.Checked then
    iChanges := CompareFiles(edtFileNameLeft.Text, edtFileNameRight.Text,
       lstLeft.Lines, lstRight.Lines, cmInternalBin)
  else
    iChanges := CompareFiles(edtFileNameLeft.Text, edtFileNameRight.Text,
       lstLeft.Lines, lstRight.Lines, cmInternalText);
{  CompareFiles(edtFileNameLeft.Text, edtFileNameRight.Text,
    lstLeft.Items, lstRight.Items, cmInternalText);}
  pnlStatusBar.Panels[0].Text := lngGetString(clngCompareDiffs) + ' ' + IntToStr(iChanges);
end;

procedure TfrmCompareFiles.lstLeftSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  i:Integer;
begin
  i:=Integer(lstLeft.Lines.Objects[Line-1]);
  if i = 0 then Exit;
  Special:=True;
  if chbBinMode.Checked then
  begin
    FG:=clRed;
    Exit;
  end;
  if i=1 then
    FG:=clRed
  else
    FG:=clGreen;
end;

procedure TfrmCompareFiles.lstLeftStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (chbKeepScrolling.Checked) then
    begin
      lstRight.TopLine := lstLeft.TopLine;
      lstLeft.Invalidate;
    end;
end;

procedure TfrmCompareFiles.lstRightSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  i:Integer;
begin
  i:=Integer(lstRight.Lines.Objects[Line-1]);
  if i = 0 then Exit;
  Special:=True;
  if chbBinMode.Checked then
  begin
    FG:=clRed;
    Exit;
  end;
  if i=1 then
    FG:=clRed
  else
    FG:=clGreen;
end;

procedure TfrmCompareFiles.lstRightStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (chbKeepScrolling.Checked) then
    begin
      lstLeft.TopLine := lstRight.TopLine;
      lstRight.Invalidate;
    end;
end;

procedure TfrmCompareFiles.btnCloseClick(Sender: TObject);
begin
  inherited;
  Close();
end;

initialization
 {$I fcomparefiles.lrs}
end.

