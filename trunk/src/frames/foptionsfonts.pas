{
   Double Commander
   -------------------------------------------------------------------------
   Fonts options page

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

unit fOptionsFonts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls, Spin, Dialogs;

type

  { TfrmOptionsFonts }

  TfrmOptionsFonts = class(TOptionsEditor)
    btnSelEditFnt: TButton;
    btnSelLogFnt: TButton;
    btnSelMainFnt: TButton;
    btnSelViewerBookFnt: TButton;
    btnSelViewFnt: TButton;
    dlgFnt: TFontDialog;
    edtEditorFont: TEdit;
    edtEditorFontSize: TSpinEdit;
    edtLogFont: TEdit;
    edtLogFontSize: TSpinEdit;
    edtMainFont: TEdit;
    edtMainFontSize: TSpinEdit;
    edtViewerBookFont: TEdit;
    edtViewerBookFontSize: TSpinEdit;
    edtViewerFont: TEdit;
    edtViewerFontSize: TSpinEdit;
    lblEditorFont: TLabel;
    lblLogFont: TLabel;
    lblMainFont: TLabel;
    lblViewerBookFont: TLabel;
    lblViewerFont: TLabel;
    procedure btnSelEditFntClick(Sender: TObject);
    procedure btnSelMainFntClick(Sender: TObject);
    procedure btnSelViewFntClick(Sender: TObject);
    procedure btnSelLogFntClick(Sender: TObject);
    procedure btnSelViewerBookFntClick(Sender: TObject);
    procedure edtEditorFontSizeChange(Sender: TObject);
    procedure edtMainFontSizeChange(Sender: TObject);
    procedure edtViewerFontSizeChange(Sender: TObject);
    procedure edtLogFontSizeChange(Sender: TObject);
    procedure edtViewerBookFontSizeChange(Sender: TObject);
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

{ TfrmOptionsFonts }

procedure TfrmOptionsFonts.btnSelEditFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtEditorFont.Text;
  dlgFnt.Font.Size  := edtEditorFontSize.Value;
  dlgFnt.Font.Style := edtEditorFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtEditorFont.Text       := dlgFnt.Font.Name;
      edtEditorFontSize.Value  := dlgFnt.Font.Size;
      edtEditorFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptionsFonts.btnSelMainFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtMainFont.Text;
  dlgFnt.Font.Size  := edtMainFontSize.Value;
  dlgFnt.Font.Style := edtMainFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtMainFont.Text       := dlgFnt.Font.Name;
      edtMainFontSize.Value  := dlgFnt.Font.Size;
      edtMainFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptionsFonts.btnSelViewFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtViewerFont.Text;
  dlgFnt.Font.Size  := edtViewerFontSize.Value;
  dlgFnt.Font.Style := edtViewerFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtViewerFont.Text       := dlgFnt.Font.Name;
      edtViewerFontSize.Value  := dlgFnt.Font.Size;
      edtViewerFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptionsFonts.btnSelLogFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtLogFont.Text;
  dlgFnt.Font.Size  := edtLogFontSize.Value;
  dlgFnt.Font.Style := edtLogFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtLogFont.Text       := dlgFnt.Font.Name;
      edtLogFontSize.Value  := dlgFnt.Font.Size;
      edtLogFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptionsFonts.btnSelViewerBookFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtViewerBookFont.Text;
  dlgFnt.Font.Size  := edtViewerBookFontSize.Value;
  dlgFnt.Font.Style := edtViewerBookFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtViewerBookFont.Text       := dlgFnt.Font.Name;
      edtViewerBookFontSize.Value  := dlgFnt.Font.Size;
      edtViewerBookFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptionsFonts.edtEditorFontSizeChange(Sender: TObject);
begin
  edtEditorFont.Font.Size := edtEditorFontSize.Value;
end;

procedure TfrmOptionsFonts.edtMainFontSizeChange(Sender: TObject);
begin
  edtMainFont.Font.Size := edtMainFontSize.Value;
end;

procedure TfrmOptionsFonts.edtViewerFontSizeChange(Sender: TObject);
begin
  edtViewerFont.Font.Size := edtViewerFontSize.Value;
end;

procedure TfrmOptionsFonts.edtLogFontSizeChange(Sender: TObject);
begin
  edtLogFont.Font.Size := edtLogFontSize.Value;
end;

procedure TfrmOptionsFonts.edtViewerBookFontSizeChange(Sender: TObject);
begin
  edtViewerBookFont.Font.Size := edtViewerBookFontSize.Value;
end;

class function TfrmOptionsFonts.GetIconIndex: Integer;
begin
  Result := 3;
end;

class function TfrmOptionsFonts.GetTitle: String;
begin
  Result := rsOptionsEditorFonts;
end;

procedure TfrmOptionsFonts.Load;
begin
  edtMainFont.Text   := gFonts[dcfMain].Name;
  edtEditorFont.Text := gFonts[dcfEditor].Name;
  edtViewerFont.Text := gFonts[dcfViewer].Name;
  edtLogFont.Text    := gFonts[dcfLog].Name;
  edtViewerBookFont.Text := gFonts[dcfViewerBook].Name;

  edtMainFontSize.Value   := gFonts[dcfMain].Size;
  edtEditorFontSize.Value := gFonts[dcfEditor].Size;
  edtViewerFontSize.Value := gFonts[dcfViewer].Size;
  edtLogFontSize.Value    := gFonts[dcfLog].Size;
  edtViewerBookFontSize.Value := gFonts[dcfViewerBook].Size;

  FontOptionsToFont(gFonts[dcfMain], edtMainFont.Font);
  FontOptionsToFont(gFonts[dcfEditor], edtEditorFont.Font);
  FontOptionsToFont(gFonts[dcfViewer], edtViewerFont.Font);
  FontOptionsToFont(gFonts[dcfLog], edtLogFont.Font);
  FontOptionsToFont(gFonts[dcfViewerBook], edtViewerBookFont.Font);
end;

function TfrmOptionsFonts.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  with gFonts[dcfMain] do
  begin
    Name  := edtMainFont.Text;
    Size  := edtMainFontSize.Value;
    Style := edtMainFont.Font.Style;
  end;

  with gFonts[dcfEditor] do
  begin
    Name  := edtEditorFont.Text;
    Size  := edtEditorFontSize.Value;
    Style := edtEditorFont.Font.Style;
  end;

  with gFonts[dcfViewer] do
  begin
    Name  := edtViewerFont.Text;
    Size  := edtViewerFontSize.Value;
    Style := edtViewerFont.Font.Style;
  end;

  with gFonts[dcfLog] do
  begin
    Name  := edtLogFont.Text;
    Size  := edtLogFontSize.Value;
    Style := edtLogFont.Font.Style;
  end;

  with gFonts[dcfViewerBook] do
  begin
    Name  := edtViewerBookFont.Text;
    Size  := edtViewerBookFontSize.Value;
    Style := edtViewerBookFont.Font.Style;
  end;
end;

end.

