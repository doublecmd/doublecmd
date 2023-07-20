{
   Double Commander
   -------------------------------------------------------------------------
   File panels colors options page

   Copyright (C) 2006-2023  Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit fOptionsFilePanelsColors;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, Classes, SysUtils, ComCtrls, StdCtrls, ColorBox, ExtCtrls, Dialogs,
  DividerBevel, LMessages, KASComboBox,

  //DC
  uColumns, fOptionsFrame, uColumnsFileView, Controls;

type
  { TfrmOptionsFilePanelsColors }
  TfrmOptionsFilePanelsColors = class(TOptionsEditor)
    btnResetToDCDefault: TButton;
    cbAllowOverColor: TCheckBox;
    cbPathActiveText: TKASColorBoxButton;
    cbPathInactiveText: TKASColorBoxButton;
    cbUseCursorBorder: TCheckBox;
    cbCursorBorderColor: TKASColorBoxButton;
    dbOptionsVertical: TDividerBevel;
    lblPathInactiveText: TLabel;
    lblPathActiveText: TLabel;
    lblTextColor: TLabel;
    cbTextColor: TKASColorBoxButton;
    lblBackgroundColor: TLabel;
    cbBackColor: TKASColorBoxButton;
    lblBackgroundColor2: TLabel;
    cbBackColor2: TKASColorBoxButton;
    lblMarkColor: TLabel;
    cbMarkColor: TKASColorBoxButton;
    lblCursorColor: TLabel;
    cbCursorColor: TKASColorBoxButton;
    lblCursorText: TLabel;
    cbCursorText: TKASColorBoxButton;
    lblInactiveCursorColor: TLabel;
    cbInactiveCursorColor: TKASColorBoxButton;
    lblInactiveMarkColor: TLabel;
    cbInactiveMarkColor: TKASColorBoxButton;
    cbbUseInvertedSelection: TCheckBox;
    cbbUseInactiveSelColor: TCheckBox;
    cbbUseFrameCursor: TCheckBox;
    lblInactivePanelBrightness: TLabel;
    spPanelSplitter: TSplitter;
    tbInactivePanelBrightness: TTrackBar;
    dbCurrentPath: TDividerBevel;
    lblPathActiveBack: TLabel;
    cbPathActiveBack: TKASColorBoxButton;
    lblPathInactiveBack: TLabel;
    cbPathInactiveBack: TKASColorBoxButton;
    pnlPreviewCont: TPanel;
    lblPreview: TLabel;
    pnlLeftPreview: TPanel;
    pnlRightPreview: TPanel;
    optColorDialog: TColorDialog;
    procedure btnResetToDCDefaultClick(Sender: TObject);
    procedure cbbUseFrameCursorChange(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure cbbUseInactiveSelColorChange(Sender: TObject);
    procedure cbPathActiveTextChange(Sender: TObject);
    procedure cbPathInactiveBackChange(Sender: TObject);
    procedure cbPathInactiveTextChange(Sender: TObject);
    procedure cbUseCursorBorderChange(Sender: TObject);
    procedure tbInactivePanelBrightnessChange(Sender: TObject);
    procedure cbPathActiveBackChange(Sender: TObject);
    procedure RefreshPreviewPanel;
    procedure pnlLeftPreviewEnter(Sender: TObject);
    procedure pnlRightPreviewEnter(Sender: TObject);
    function JustForConfigDim(AColor: TColor): TColor;
    function JustForConfigNoDim(AColor: TColor): TColor;
  private
    bLoadCompleted: boolean;
    PreviewLeftPanel: TColumnsFileView;
    PreviewRightPanel: TColumnsFileView;
    ColumnClass: TPanelColumnsClass;
    ColPrm: TColPrm;
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure CMThemeChanged(var Message: TLMessage); message CM_THEMECHANGED;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Forms,

  //DC
  uSampleForConfigFileSource, uFileFunctions, fMain, uLng, uGlobs,
  uDCUtils;

{ TfrmOptionsFilePanelsColors }

{ TfrmOptionsFilePanelsColors.GetIconIndex }
class function TfrmOptionsFilePanelsColors.GetIconIndex: integer;
begin
  Result := 20;
end;

{ TfrmOptionsFilePanelsColors.GetTitle }
class function TfrmOptionsFilePanelsColors.GetTitle: string;
begin
  Result := rsOptionsEditorFilePanels;
end;

{ TfrmOptionsFilePanelsColors.Load }
procedure TfrmOptionsFilePanelsColors.Load;
begin
  bLoadCompleted := False;

  //1. Let's create the element we'll need.
  ColPrm := TColPrm.Create;
  ColumnClass := TPanelColumnsClass.Create;

  //2. Let's load the current settings to be shown on screen
  with gColors.FilePanel^ do
  begin
    cbTextColor.Selected := ForeColor;
    cbBackColor.Selected := BackColor;
    cbBackColor2.Selected := BackColor2;
    cbMarkColor.Selected := MarkColor;
    cbCursorColor.Selected := CursorColor;
    cbCursorText.Selected := CursorText;
    cbInactiveCursorColor.Selected := InactiveCursorColor;
    cbInactiveMarkColor.Selected := InactiveMarkColor;
    cbCursorBorderColor.Selected := CursorBorderColor;
  end;
  cbAllowOverColor.Checked := gAllowOverColor;
  cbbUseInvertedSelection.Checked := gUseInvertedSelection;
  cbbUseInactiveSelColor.Checked := gUseInactiveSelColor;
  cbbUseFrameCursor.Checked := gUseFrameCursor;
  cbUseCursorBorder.Checked := gUseCursorBorder;
  tbInactivePanelBrightness.Position := gInactivePanelBrightness;
  cbbUseFrameCursorChange(cbbUseFrameCursor);
  cbbUseInactiveSelColorChange(cbbUseInactiveSelColor);

  //3. Let's create our preview panels
  PreviewLeftPanel := TColumnsFileView.Create(pnlLeftPreview, TSampleForConfigFileSource.Create, SAMPLE_PATH);
  PreviewLeftPanel.JustForColorPreviewSetActiveState(True);
  PreviewLeftPanel.SetGridFunctionDim(@JustForConfigNoDim);
  PreviewRightPanel := TColumnsFileView.Create(pnlRightPreview, TSampleForConfigFileSource.Create, SAMPLE_PATH);
  PreviewRightPanel.JustForColorPreviewSetActiveState(False);
  PreviewRightPanel.SetGridFunctionDim(@JustForConfigDim);

  //4. Let's define which ColumnClass it's gonna follow
  PreviewLeftPanel.ActiveColmSlave := ColumnClass;
  PreviewLeftPanel.isSlave := True;
  PreviewLeftPanel.Demo := True;
  PreviewRightPanel.ActiveColmSlave := ColumnClass;
  PreviewRightPanel.isSlave := True;
  PreviewRightPanel.Demo := True;

  with gColors.Path^ do
  begin
    cbPathActiveText.Selected := ActiveFontColor;
    cbPathActiveBack.Selected := ActiveColor;
    cbPathInactiveText.Selected := InactiveFontColor;
    cbPathInactiveBack.Selected := InactiveColor;
  end;

  //5. Let's refresh the panel so we will show something
  RefreshPreviewPanel;

  //6. Good. Loading is completed.
  bLoadCompleted := True;
end;

{ TfrmOptionsFilePanelsColors.Save }
function TfrmOptionsFilePanelsColors.Save: TOptionsEditorSaveFlags;
begin
  with gColors.FilePanel^ do
  begin
    ForeColor := cbTextColor.Selected;
    BackColor := cbBackColor.Selected;
    BackColor2 := cbBackColor2.Selected;
    MarkColor := cbMarkColor.Selected;
    CursorColor := cbCursorColor.Selected;
    CursorText := cbCursorText.Selected;
    InactiveCursorColor := cbInactiveCursorColor.Selected;
    InactiveMarkColor := cbInactiveMarkColor.Selected;
    CursorBorderColor := cbCursorBorderColor.Selected;
  end;
  gUseInvertedSelection := cbbUseInvertedSelection.Checked;
  gAllowOverColor := cbAllowOverColor.Checked;
  gUseInactiveSelColor := cbbUseInactiveSelColor.Checked;
  gUseFrameCursor := cbbUseFrameCursor.Checked;
  gUseCursorBorder := cbUseCursorBorder.Checked;
  gInactivePanelBrightness := tbInactivePanelBrightness.Position;
  with gColors.Path^ do
  begin
    ActiveFontColor:= cbPathActiveText.Selected;
    ActiveColor:= cbPathActiveBack.Selected;
    InactiveFontColor:= cbPathInactiveText.Selected;
    InactiveColor:= cbPathInactiveBack.Selected;
  end;
  Result := [];
end;

procedure TfrmOptionsFilePanelsColors.CMThemeChanged(var Message: TLMessage);
begin
  LoadSettings;
end;

{ TfrmOptionsFilePanelsColors.cbColorBoxChange }
procedure TfrmOptionsFilePanelsColors.cbColorBoxChange(Sender: TObject);
begin
  if bLoadCompleted then
    RefreshPreviewPanel;
end;

procedure TfrmOptionsFilePanelsColors.btnResetToDCDefaultClick(Sender: TObject);
begin
  cbTextColor.Selected := clWindowText;
  cbBackColor.Selected := clWindow;
  cbBackColor2.Selected := clWindow;
  cbMarkColor.Selected := clRed;
  cbCursorColor.Selected := clHighlight;
  cbCursorText.Selected := clHighlightText;
  cbInactiveCursorColor.Selected := clInactiveCaption;
  cbInactiveMarkColor.Selected := clMaroon;
  cbAllowOverColor.Checked := True;
  cbbUseInvertedSelection.Checked := False;
  cbbUseInactiveSelColor.Checked := False;
  cbbUseFrameCursor.Checked := False;
  cbUseCursorBorder.Checked := False;
  cbCursorBorderColor.Selected := clHighlight;
  tbInactivePanelBrightness.Position := 100;
  cbPathActiveText.Selected := clHighlightText;
  cbPathActiveBack.Selected := clHighlight;
  cbPathInactiveText.Selected := clBtnText;
  cbPathInactiveBack.Selected := clBtnFace;
  cbbUseFrameCursorChange(cbbUseFrameCursor);
end;

procedure TfrmOptionsFilePanelsColors.cbbUseFrameCursorChange(Sender: TObject);
begin
  cbUseCursorBorder.Enabled := not cbbUseFrameCursor.Checked;
  lblCursorText.Enabled := not cbbUseFrameCursor.Checked;
  cbCursorText.Enabled := not cbbUseFrameCursor.Checked;
  cbUseCursorBorderChange(cbUseCursorBorder);
end;

{ TfrmOptionsFilePanelsColors.cbbUseInactiveSelColorChange }
procedure TfrmOptionsFilePanelsColors.cbbUseInactiveSelColorChange(Sender: TObject);
begin
  lblInactiveCursorColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  cbInactiveCursorColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  lblInactiveMarkColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  cbInactiveMarkColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;

  if bLoadCompleted then
  begin
    RefreshPreviewPanel;
  end;
end;

procedure TfrmOptionsFilePanelsColors.cbPathActiveTextChange(Sender: TObject);
begin
  PreviewLeftPanel.Header.PathLabel.ActiveFontColor:= cbPathActiveText.Selected;
  PreviewRightPanel.Header.PathLabel.ActiveFontColor:= cbPathActiveText.Selected;
end;

{ TfrmOptionsFilePanelsColors.cbIndColorChange }
procedure TfrmOptionsFilePanelsColors.cbPathActiveBackChange(Sender: TObject);
begin
  PreviewLeftPanel.Header.PathLabel.ActiveColor:= cbPathActiveBack.Selected;
  PreviewRightPanel.Header.PathLabel.ActiveColor:= cbPathActiveBack.Selected;
end;

procedure TfrmOptionsFilePanelsColors.cbPathInactiveBackChange(Sender: TObject);
begin
  PreviewLeftPanel.Header.PathLabel.InactiveColor:= cbPathInactiveBack.Selected;
  PreviewRightPanel.Header.PathLabel.InactiveColor:= cbPathInactiveBack.Selected;
end;

procedure TfrmOptionsFilePanelsColors.cbPathInactiveTextChange(Sender: TObject);
begin
  PreviewLeftPanel.Header.PathLabel.InactiveFontColor:= cbPathInactiveText.Selected;
  PreviewRightPanel.Header.PathLabel.InactiveFontColor:= cbPathInactiveText.Selected;
end;

procedure TfrmOptionsFilePanelsColors.cbUseCursorBorderChange(Sender: TObject);
begin
  cbCursorBorderColor.Enabled := cbUseCursorBorder.Checked and cbUseCursorBorder.Enabled;
  if bLoadCompleted then
    RefreshPreviewPanel;
end;

{ TfrmOptionsFilePanelsColors.tbInactivePanelBrightnessChange }
procedure TfrmOptionsFilePanelsColors.tbInactivePanelBrightnessChange(Sender: TObject);
begin
  if bLoadCompleted then
  begin
    PreviewLeftPanel.UpdateColumnsView;
    PreviewLeftPanel.Reload;
    PreviewRightPanel.UpdateColumnsView;
    PreviewRightPanel.Reload;
  end;
end;

{ TfrmOptionsFilePanelsColors.RefreshPreviewPanel }
procedure TfrmOptionsFilePanelsColors.RefreshPreviewPanel;
const
  DCFunc = '[DC().%s{}]';
var
  indx: integer;
begin
  //Set color
  ColPrm.FontName := gFonts[dcfMain].Name;
  ColPrm.FontSize := gFonts[dcfMain].Size;
  ColPrm.FontStyle := gFonts[dcfMain].Style;
  ColPrm.Overcolor := cbAllowOverColor.Checked;
  ColPrm.UseInvertedSelection := cbbUseInvertedSelection.Checked;
  ColPrm.UseInactiveSelColor := cbbUseInactiveSelColor.Checked;
  ColPrm.TextColor := cbTextColor.Selected;
  ColPrm.Background := cbBackColor.Selected;
  ColPrm.Background2 := cbBackColor2.Selected;
  ColPrm.MarkColor := cbMarkColor.Selected;
  ColPrm.CursorColor := cbCursorColor.Selected;
  ColPrm.CursorText := cbCursorText.Selected;
  ColPrm.InactiveCursorColor := cbInactiveCursorColor.Selected;
  ColPrm.InactiveMarkColor := cbInactiveMarkColor.Selected;

  ColumnClass.Clear;
  ColumnClass.Add(rsColName, Format(DCFunc, [TFileFunctionStrings[fsfNameNoExtension]]), 200, taLeftJustify);
  ColumnClass.Add(rsColExt, Format(DCFunc, [TFileFunctionStrings[fsfExtension]]), 70, taLeftJustify);
  ColumnClass.Add(rsColSize, Format(DCFunc, [TFileFunctionStrings[fsfSize]]), 90, taRightJustify);

  for indx := 0 to pred(ColumnClass.Count) do
    ColumnClass.SetColumnPrm(Indx, ColPrm);

  ColumnClass.CustomView := True;
  ColumnClass.UseFrameCursor := cbbUseFrameCursor.Checked;
  ColumnClass.CursorBorderColor := clRed;
  ColumnClass.UseFrameCursor := cbbUseFrameCursor.Checked;
  ColumnClass.UseCursorBorder := cbUseCursorBorder.Checked;
  ColumnClass.CursorBorderColor := cbCursorBorderColor.Selected;

  ColumnClass.Name := 'JustForSetup';

  PreviewLeftPanel.UpdateColumnsView;
  PreviewRightPanel.UpdateColumnsView;
end;

{ TfrmOptionsFilePanelsColors.pnlLeftPreviewEnter }
procedure TfrmOptionsFilePanelsColors.pnlLeftPreviewEnter(Sender: TObject);
begin
  PreviewRightPanel.SetGridFunctionDim(@JustForConfigDim);
  PreviewRightPanel.JustForColorPreviewSetActiveState(False);
  PreviewLeftPanel.SetGridFunctionDim(@JustForConfigNoDim);
  PreviewLeftPanel.JustForColorPreviewSetActiveState(True);
end;

{ TfrmOptionsFilePanelsColors.pnlRightPreviewEnter }
procedure TfrmOptionsFilePanelsColors.pnlRightPreviewEnter(Sender: TObject);
begin
  PreviewLeftPanel.SetGridFunctionDim(@JustForConfigDim);
  PreviewLeftPanel.JustForColorPreviewSetActiveState(False);
  PreviewRightPanel.SetGridFunctionDim(@JustForConfigNoDim);
  PreviewRightPanel.JustForColorPreviewSetActiveState(True);
end;

{ TfrmOptionsFilePanelsColors.JustForConfigDim }
function TfrmOptionsFilePanelsColors.JustForConfigDim(AColor: TColor): TColor;
begin
  if (tbInactivePanelBrightness.Position < 100) then
    Result := ModColor(AColor, tbInactivePanelBrightness.Position)
  else
    Result := AColor;
end;

{ TfrmOptionsFilePanelsColors.JustForConfigNoDim }
function TfrmOptionsFilePanelsColors.JustForConfigNoDim(AColor: TColor): TColor;
begin
  Result := AColor;
end;

end.

