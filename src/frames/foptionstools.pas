{
   Double Commander
   -------------------------------------------------------------------------
   Tools options page

   Copyright (C) 2006-2014  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fOptionsTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Spin, ExtCtrls, ColorBox, Dialogs, Types,
  fOptionsFrame, fOptionsToolBase;

type

  { TfrmOptionsViewer }

  TfrmOptionsViewer = class(TfrmOptionsToolBase)
    btnBackViewerColor: TButton;
    btnFontViewerColor: TButton;
    cbBackgroundColorViewerBook: TColorBox;
    cbFontColorViewerBook: TColorBox;
    cmbEncoding: TComboBox;
    gbViewerBookMode: TGroupBox;
    gbViewerExample: TGroupBox;
    lblBackgroundColorViewerBook: TLabel;
    lblEncoding: TLabel;
    lblFontColorViewerBook: TLabel;
    lblNumberColumnsViewer: TLabel;
    optColorDialog: TColorDialog;
    pbViewerBook: TPaintBox;
    seNumberColumnsViewer: TSpinEdit;
    procedure btnBackViewerColorClick(Sender: TObject);
    procedure btnFontViewerColorClick(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure seNumberColumnsViewerChange(Sender: TObject);
    procedure pbViewerBookPaint(Sender: TObject);
  private
    FPreviewTextSize: TSize;
    procedure UseExternalProgramChanged(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uDCUtils, uGlobs, uLng, ViewerControl;

const
  ViewerBookPreviewText = 'Text';

{ TfrmOptionsViewer }

procedure TfrmOptionsViewer.btnBackViewerColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbBackgroundColorViewerBook.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackgroundColorViewerBook, optColorDialog.Color);
  end;
end;

procedure TfrmOptionsViewer.btnFontViewerColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbFontColorViewerBook.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbFontColorViewerBook, optColorDialog.Color);
  end;
end;

procedure TfrmOptionsViewer.cbColorBoxChange(Sender: TObject);
begin
  pbViewerBook.Repaint;
end;

procedure TfrmOptionsViewer.seNumberColumnsViewerChange(Sender: TObject);
begin
  pbViewerBook.Repaint;
end;

procedure TfrmOptionsViewer.pbViewerBookPaint(Sender: TObject);
var
  i, numb: integer;
begin
  with pbViewerBook.Canvas do
  begin
    Brush.Color := cbBackgroundColorViewerBook.Selected;
    Font.Color := cbFontColorViewerBook.Selected;
    FillRect(0, 0, pbViewerBook.Width, pbViewerBook.Height);
    for i:= 0 to seNumberColumnsViewer.Value - 1 do
    begin
      for numb:= 0 to 1 do
        TextOut(i * (FPreviewTextSize.cx + 5) + 5,
                FPreviewTextSize.cy * numb + 4, ViewerBookPreviewText);
    end;
  end;
end;

procedure TfrmOptionsViewer.UseExternalProgramChanged(Sender: TObject);
begin
  gbViewerBookMode.Enabled        := not (cbToolsUseExternalProgram.Checked);
  lblBackgroundColorViewerBook.Enabled := not (cbToolsUseExternalProgram.Checked);
  lblNumberColumnsViewer.Enabled  := not (cbToolsUseExternalProgram.Checked);
  lblFontColorViewerBook.Enabled  := not (cbToolsUseExternalProgram.Checked);
end;

class function TfrmOptionsViewer.GetIconIndex: Integer;
begin
  Result := 22;
end;

class function TfrmOptionsViewer.GetTitle: String;
begin
  Result := rsToolViewer;
end;

procedure TfrmOptionsViewer.Init;
var
  ViewerControl: TViewerControl = Nil;
begin
  ExternalTool := etViewer;
  OnUseExternalProgramChange := @UseExternalProgramChanged;
  gbViewerBookMode.Enabled := not (cbToolsUseExternalProgram.Checked);
  pbViewerBook.Font.Name := gFonts[dcfViewerBook].Name;
  pbViewerBook.Font.Size := gFonts[dcfViewerBook].Size;
  pbViewerBook.Font.Style := gFonts[dcfViewerBook].Style;
  // encoding combobox
  cmbEncoding.Items.Clear;
  ViewerControl.GetSupportedEncodings(cmbEncoding.Items);

  inherited Init;

  FPreviewTextSize := pbViewerBook.Canvas.TextExtent(ViewerBookPreviewText);
end;

procedure TfrmOptionsViewer.Load;
var
  I: Integer;
begin
  inherited;
  seNumberColumnsViewer.Value := gColCount;
  SetColorInColorBox(cbBackgroundColorViewerBook,gBookBackgroundColor);
  SetColorInColorBox(cbFontColorViewerBook,gBookFontColor);
  // encoding combobox
  I := cmbEncoding.Items.IndexOf(gViewerEncoding);
  if I >= 0 then
    cmbEncoding.ItemIndex := I
  else
    cmbEncoding.ItemIndex := cmbEncoding.Items.IndexOf(
      ViewerControl.ViewerEncodingsNames[ViewerControl.veAutoDetect]);
end;

function TfrmOptionsViewer.Save: TOptionsEditorSaveFlags;
begin
  Result := inherited;
  gColCount := seNumberColumnsViewer.Value;
  gBookBackgroundColor := cbBackgroundColorViewerBook.Selected;
  gBookFontColor := cbFontColorViewerBook.Selected;
  gViewerEncoding := cmbEncoding.Text;
end;

end.

