{
   Double Commander
   -------------------------------------------------------------------------
   Tools options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, StdCtrls, Spin, ExtCtrls, ColorBox, Dialogs,
  fOptionsFrame, fOptionsToolBase;

type

  { TfrmOptionsViewer }

  TfrmOptionsViewer = class(TfrmOptionsToolBase)
    btnBackViewerColor: TButton;
    btnFontViewerColor: TButton;
    cbBackgroundColorViewerBook: TColorBox;
    cbFontColorViewerBook: TColorBox;
    gbViewerBookMode: TGroupBox;
    gbViewerExample: TGroupBox;
    lblBackgroundColorViewerBook: TLabel;
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
    procedure UseExternalProgramChanged(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

  { TfrmOptionsDiffer }

  TfrmOptionsDiffer = class(TfrmOptionsToolBase)
  protected
    procedure Init; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

  { TfrmOptionsEditor }

  TfrmOptionsEditor = class(TfrmOptionsToolBase)
  protected
    procedure Init; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uDCUtils, uGlobs, uLng;

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
  i, numb, x, y: integer;
  sStr: String;
begin
  sStr:= 'Text';
  with pbViewerBook.Canvas do
  begin
    Font.Name := gFonts[dcfViewerBook].Name;
    Font.Size := gFonts[dcfViewerBook].Size;
    x:= TextWidth(sStr);
    y:= TextHeight(sStr);
    pbViewerBook.Width := (x + 10) * seNumberColumnsViewer.Value;
    Brush.Color := cbBackgroundColorViewerBook.Selected;
    Font.Color := cbFontColorViewerBook.Selected;
    FillRect(0, 0, pbViewerBook.Width, pbViewerBook.Height);
    for i:= 0 to seNumberColumnsViewer.Value - 1 do
    begin
      for numb:= 0 to 1 do
        TextOut(i * (x + 5) + 5, y * numb + 4, sStr);
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
begin
  ExternalTool := etViewer;
  OnUseExternalProgramChange := @UseExternalProgramChanged;
  gbViewerBookMode.Enabled := not (cbToolsUseExternalProgram.Checked);
  inherited Init;
end;

procedure TfrmOptionsViewer.Load;
begin
  inherited;
  seNumberColumnsViewer.Value := gColCount;
  SetColorInColorBox(cbBackgroundColorViewerBook,gBookBackgroundColor);
  SetColorInColorBox(cbFontColorViewerBook,gBookFontColor);
end;

function TfrmOptionsViewer.Save: TOptionsEditorSaveFlags;
begin
  Result := inherited;
  gColCount := seNumberColumnsViewer.Value;
  gBookBackgroundColor := cbBackgroundColorViewerBook.Selected;
  gBookFontColor := cbFontColorViewerBook.Selected;
end;

{ TfrmOptionsEditor }

procedure TfrmOptionsEditor.Init;
begin
  ExternalTool := etEditor;
  inherited Init;
end;

constructor TfrmOptionsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name := 'frmOptionsEditor';
end;

class function TfrmOptionsEditor.GetIconIndex: Integer;
begin
  Result := 10;
end;

class function TfrmOptionsEditor.GetTitle: String;
begin
  Result := rsToolEditor;
end;

{ TfrmOptionsDiffer }

procedure TfrmOptionsDiffer.Init;
begin
  ExternalTool := etDiffer;
  inherited Init;
end;

constructor TfrmOptionsDiffer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name := 'frmOptionsDiffer';
end;

class function TfrmOptionsDiffer.GetIconIndex: Integer;
begin
  Result := 25;
end;

class function TfrmOptionsDiffer.GetTitle: String;
begin
  Result := rsToolDiffer;
end;

end.

