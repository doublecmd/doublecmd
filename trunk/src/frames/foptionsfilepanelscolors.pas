{
   Double Commander
   -------------------------------------------------------------------------
   File panels colors options page

   Copyright (C) 2006-2016  Alexander Koblov (alexx2000@mail.ru)

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
  DividerBevel,

  //DC
  uColumns, fOptionsFrame
  {$IFDEF COLUMNSFILEVIEW_VTV}
  , uColumnsFileViewVtv
  {$ELSE}
  , uColumnsFileView
  {$ENDIF}
  ;

type
  { TfrmOptionsFilePanelsColors }
  TfrmOptionsFilePanelsColors = class(TOptionsEditor)
    btnCursorBorderColor: TButton;
    btnResetToDCDefault: TButton;
    cbAllowOverColor: TCheckBox;
    cbUseCursorBorder: TCheckBox;
    cbCursorBorderColor: TColorBox;
    lblTextColor: TLabel;
    cbTextColor: TColorBox;
    btnForeColor: TButton;
    lblBackgroundColor: TLabel;
    cbBackColor: TColorBox;
    btnBackColor: TButton;
    lblBackgroundColor2: TLabel;
    cbBackColor2: TColorBox;
    btnBackColor2: TButton;
    lblMarkColor: TLabel;
    cbMarkColor: TColorBox;
    btnMarkColor: TButton;
    lblCursorColor: TLabel;
    cbCursorColor: TColorBox;
    btnCursorColor: TButton;
    lblCursorText: TLabel;
    cbCursorText: TColorBox;
    btnCursorText: TButton;
    lblInactiveCursorColor: TLabel;
    cbInactiveCursorColor: TColorBox;
    btnInactiveCursorColor: TButton;
    lblInactiveMarkColor: TLabel;
    cbInactiveMarkColor: TColorBox;
    btnInactiveMarkColor: TButton;
    dbOptionsVertical: TDividerBevel;
    cbbUseInvertedSelection: TCheckBox;
    cbbUseInactiveSelColor: TCheckBox;
    cbbUseFrameCursor: TCheckBox;
    lblInactivePanelBrightness: TLabel;
    spPanelSplitter: TSplitter;
    tbInactivePanelBrightness: TTrackBar;
    dbFreeSpaceIndicator: TDividerBevel;
    cbbUseGradientInd: TCheckBox;
    pbxFakeDrive: TPaintBox;
    lblIndColor: TLabel;
    cbIndColor: TColorBox;
    btnIndColor: TButton;
    lblIndBackColor: TLabel;
    cbIndBackColor: TColorBox;
    btnIndBackColor: TButton;
    pnlPreviewCont: TPanel;
    lblPreview: TLabel;
    pnlLeftPreview: TPanel;
    pnlRightPreview: TPanel;
    optColorDialog: TColorDialog;
    procedure btnCursorBorderColorClick(Sender: TObject);
    procedure btnResetToDCDefaultClick(Sender: TObject);
    procedure cbbUseFrameCursorChange(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure btnBackColor2Click(Sender: TObject);
    procedure btnMarkColorClick(Sender: TObject);
    procedure btnCursorColorClick(Sender: TObject);
    procedure btnCursorTextClick(Sender: TObject);
    procedure btnInactiveCursorColorClick(Sender: TObject);
    procedure btnInactiveMarkColorClick(Sender: TObject);
    procedure cbbUseInactiveSelColorChange(Sender: TObject);
    procedure cbUseCursorBorderChange(Sender: TObject);
    procedure tbInactivePanelBrightnessChange(Sender: TObject);
    procedure cbbUseGradientIndChange(Sender: TObject);
    procedure cbIndColorChange(Sender: TObject);
    procedure btnIndColorClick(Sender: TObject);
    procedure btnIndBackColorClick(Sender: TObject);
    procedure RefreshPreviewPanel;
    procedure pbxFakeDrivePaint(Sender: TObject);
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
  uSampleForConfigFileSource, uFileFunctions, DCOSUtils, fMain, uLng, uGlobs,
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
  SetColorInColorBox(cbTextColor, gForeColor);
  SetColorInColorBox(cbBackColor, gBackColor);
  SetColorInColorBox(cbBackColor2, gBackColor2);
  SetColorInColorBox(cbMarkColor, gMarkColor);
  SetColorInColorBox(cbCursorColor, gCursorColor);
  SetColorInColorBox(cbCursorText, gCursorText);
  SetColorInColorBox(cbInactiveCursorColor, gInactiveCursorColor);
  SetColorInColorBox(cbInactiveMarkColor, gInactiveMarkColor);
  cbAllowOverColor.Checked := gAllowOverColor;
  cbbUseInvertedSelection.Checked := gUseInvertedSelection;
  cbbUseInactiveSelColor.Checked := gUseInactiveSelColor;
  cbbUseFrameCursor.Checked := gUseFrameCursor;
  cbUseCursorBorder.Checked := gUseCursorBorder;
  SetColorInColorBox(cbCursorBorderColor, gCursorBorderColor);
  tbInactivePanelBrightness.Position := gInactivePanelBrightness;
  SetColorInColorBox(cbIndColor, gIndForeColor);
  SetColorInColorBox(cbIndBackColor, gIndBackColor);
  cbbUseGradientInd.Checked := gIndUseGradient;
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
  PreviewRightPanel.ActiveColmSlave := ColumnClass;
  PreviewRightPanel.isSlave := True;

  //5. Let's refresh the panel so we will show something
  RefreshPreviewPanel;

  //6. Good. Loading is completed.
  bLoadCompleted := True;
end;

{ TfrmOptionsFilePanelsColors.Save }
function TfrmOptionsFilePanelsColors.Save: TOptionsEditorSaveFlags;
begin
  gForeColor := cbTextColor.Selected;
  gBackColor := cbBackColor.Selected;
  gBackColor2 := cbBackColor2.Selected;
  gMarkColor := cbMarkColor.Selected;
  gCursorColor := cbCursorColor.Selected;
  gCursorText := cbCursorText.Selected;
  gInactiveCursorColor := cbInactiveCursorColor.Selected;
  gInactiveMarkColor := cbInactiveMarkColor.Selected;
  gUseInvertedSelection := cbbUseInvertedSelection.Checked;
  gAllowOverColor := cbAllowOverColor.Checked;
  gUseInactiveSelColor := cbbUseInactiveSelColor.Checked;
  gUseFrameCursor := cbbUseFrameCursor.Checked;
  gUseCursorBorder := cbUseCursorBorder.Checked;
  gCursorBorderColor := cbCursorBorderColor.Selected;
  gInactivePanelBrightness := tbInactivePanelBrightness.Position;
  gIndUseGradient := cbbUseGradientInd.Checked;
  gIndForeColor := cbIndColor.Selected;
  gIndBackColor := cbIndBackColor.Selected;
  Result := [];
end;

{ TfrmOptionsFilePanelsColors.cbColorBoxChange }
procedure TfrmOptionsFilePanelsColors.cbColorBoxChange(Sender: TObject);
begin
  if bLoadCompleted then
    RefreshPreviewPanel;
end;

procedure TfrmOptionsFilePanelsColors.btnCursorBorderColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbCursorBorderColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorBorderColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnResetToDCDefaultClick(Sender: TObject);
begin
  SetColorInColorBox(cbTextColor, clWindowText);
  SetColorInColorBox(cbBackColor, clWindow);
  SetColorInColorBox(cbBackColor2, clWindow);
  SetColorInColorBox(cbMarkColor, clRed);
  SetColorInColorBox(cbCursorColor, clHighlight);
  SetColorInColorBox(cbCursorText, clHighlightText);
  SetColorInColorBox(cbInactiveCursorColor, clInactiveCaption);
  SetColorInColorBox(cbInactiveMarkColor, clMaroon);
  cbAllowOverColor.Checked := True;
  cbbUseInvertedSelection.Checked := False;
  cbbUseInactiveSelColor.Checked := False;
  cbbUseFrameCursor.Checked := False;
  cbUseCursorBorder.Checked := False;
  SetColorInColorBox(cbCursorBorderColor, clHighlight);
  tbInactivePanelBrightness.Position := 100;
  SetColorInColorBox(cbIndColor, clBlack);
  SetColorInColorBox(cbIndBackColor, clWhite);
  cbbUseGradientInd.Checked := True;
  cbbUseFrameCursorChange(cbbUseFrameCursor);
end;

procedure TfrmOptionsFilePanelsColors.cbbUseFrameCursorChange(Sender: TObject);
begin
  cbUseCursorBorder.Enabled := not cbbUseFrameCursor.Checked;
  lblCursorText.Enabled := not cbbUseFrameCursor.Checked;
  cbCursorText.Enabled := not cbbUseFrameCursor.Checked;
  btnCursorText.Enabled := not cbbUseFrameCursor.Checked;
  if not cbbUseFrameCursor.Checked then
    cbCursorText.Font.Color := clDefault
  else
    cbCursorText.Font.Color := clInactiveCaption;
  cbUseCursorBorderChange(cbUseCursorBorder);
end;

{ TfrmOptionsFilePanelsColors.btnForeColorClick }
procedure TfrmOptionsFilePanelsColors.btnForeColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbTextColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbTextColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnBackColorClick }
procedure TfrmOptionsFilePanelsColors.btnBackColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbBackColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnBackColor2Click }
procedure TfrmOptionsFilePanelsColors.btnBackColor2Click(Sender: TObject);
begin
  optColorDialog.Color := cbBackColor2.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackColor2, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnMarkColorClick }
procedure TfrmOptionsFilePanelsColors.btnMarkColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbMarkColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbMarkColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnCursorColorClick }
procedure TfrmOptionsFilePanelsColors.btnCursorColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbCursorColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnCursorTextClick }
procedure TfrmOptionsFilePanelsColors.btnCursorTextClick(Sender: TObject);
begin
  optColorDialog.Color := cbCursorText.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorText, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnInactiveCursorColorClick }
procedure TfrmOptionsFilePanelsColors.btnInactiveCursorColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbInactiveCursorColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbInactiveCursorColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnInactiveMarkColorClick }
procedure TfrmOptionsFilePanelsColors.btnInactiveMarkColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbInactiveMarkColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbInactiveMarkColor, optColorDialog.Color);
    RefreshPreviewPanel;
  end;
end;

{ TfrmOptionsFilePanelsColors.cbbUseInactiveSelColorChange }
procedure TfrmOptionsFilePanelsColors.cbbUseInactiveSelColorChange(Sender: TObject);
begin
  lblInactiveCursorColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  cbInactiveCursorColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  btnInactiveCursorColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  lblInactiveMarkColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  cbInactiveMarkColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;
  btnInactiveMarkColor.Enabled := cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled;

  if bLoadCompleted then
  begin
    if cbbUseInactiveSelColor.Checked and cbbUseInactiveSelColor.Enabled then
    begin
      cbInactiveCursorColor.Font.Color := clDefault;
      cbInactiveMarkColor.Font.Color := clDefault;
    end
    else
    begin
      cbInactiveCursorColor.Font.Color := clInactiveCaption;
      cbInactiveMarkColor.Font.Color := clInactiveCaption;
    end;
    RefreshPreviewPanel;
  end;
end;

procedure TfrmOptionsFilePanelsColors.cbUseCursorBorderChange(Sender: TObject);
begin
  cbCursorBorderColor.Enabled := cbUseCursorBorder.Checked and cbUseCursorBorder.Enabled;
  btnCursorBorderColor.Enabled := cbUseCursorBorder.Checked and cbUseCursorBorder.Enabled;
  if cbUseCursorBorder.Checked and cbUseCursorBorder.Enabled then
    cbCursorBorderColor.Font.Color := clDefault
  else
    cbCursorBorderColor.Font.Color := clInactiveCaption;
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

{ TfrmOptionsFilePanelsColors.cbbUseGradientIndChange }
procedure TfrmOptionsFilePanelsColors.cbbUseGradientIndChange(Sender: TObject);
begin
  lblIndColor.Enabled := not (cbbUseGradientInd.Checked);
  lblIndBackColor.Enabled := not (cbbUseGradientInd.Checked);
  cbIndColor.Enabled := not (cbbUseGradientInd.Checked);
  cbIndBackColor.Enabled := not (cbbUseGradientInd.Checked);
  btnIndColor.Enabled := not (cbbUseGradientInd.Checked);
  btnIndBackColor.Enabled := not (cbbUseGradientInd.Checked);
  pbxFakeDrive.Repaint;
end;

{ TfrmOptionsFilePanelsColors.cbIndColorChange }
procedure TfrmOptionsFilePanelsColors.cbIndColorChange(Sender: TObject);
begin
  pbxFakeDrive.Repaint;
end;

{ TfrmOptionsFilePanelsColors.btnIndColorClick }
procedure TfrmOptionsFilePanelsColors.btnIndColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbIndColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbIndColor, optColorDialog.Color);
    pbxFakeDrive.Repaint;
  end;
end;

{ TfrmOptionsFilePanelsColors.btnIndBackColorClick }
procedure TfrmOptionsFilePanelsColors.btnIndBackColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbIndBackColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbIndBackColor, optColorDialog.Color);
    pbxFakeDrive.Repaint;
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

{ TfrmOptionsFilePanelsColors.pbxFakeDrivePaint }
procedure TfrmOptionsFilePanelsColors.pbxFakeDrivePaint(Sender: TObject);
begin
  frmMain.PaintDriveFreeBar(pbxFakeDrive, cbbUseGradientInd.Checked, cbIndColor.Selected, cbIndBackColor.Selected);
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

