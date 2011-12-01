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
  StdCtrls, Spin, Dialogs, Graphics, fOptionsFrame, uGlobs;

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
    procedure edtEditorFontExit(Sender: TObject);
    procedure edtEditorFontSizeChange(Sender: TObject);
    procedure edtLogFontExit(Sender: TObject);
    procedure edtMainFontExit(Sender: TObject);
    procedure edtMainFontSizeChange(Sender: TObject);
    procedure edtViewerBookFontExit(Sender: TObject);
    procedure edtViewerFontExit(Sender: TObject);
    procedure edtViewerFontSizeChange(Sender: TObject);
    procedure edtLogFontSizeChange(Sender: TObject);
    procedure edtViewerBookFontSizeChange(Sender: TObject);
  private
    procedure LoadFont(aDCFont:TDCFont);
    procedure SaveFont(aDCFont:TDCFont);
    //
    function GetFont(aDCFont:TDCFont):TFont;
    procedure SetFont(aDCFont:TDCFont;aFont:TFont);
    procedure SetFontSize(aDCFont:TDCFont;aFontSize:Integer);
    procedure SetFontName(aDCFont:TDCFont;aFontName:String);
    //
    procedure RunDialogFont(aDCFont:TDCFont);
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
  uLng;

{ TfrmOptionsFonts }

procedure TfrmOptionsFonts.btnSelMainFntClick(Sender: TObject);
begin
  RunDialogFont(dcfMain);
end;

procedure TfrmOptionsFonts.btnSelEditFntClick(Sender: TObject);
begin
  RunDialogFont(dcfEditor);
end;

procedure TfrmOptionsFonts.btnSelViewFntClick(Sender: TObject);
begin
  RunDialogFont(dcfViewer);
end;

procedure TfrmOptionsFonts.btnSelViewerBookFntClick(Sender: TObject);
begin
  RunDialogFont(dcfViewerBook);
end;

procedure TfrmOptionsFonts.btnSelLogFntClick(Sender: TObject);
begin
  RunDialogFont(dcfLog);
end;

procedure TfrmOptionsFonts.edtMainFontExit(Sender: TObject);
begin
  SetFontName(dcfMain, TEdit(Sender).Text);
end;

procedure TfrmOptionsFonts.edtEditorFontExit(Sender: TObject);
begin
  SetFontName(dcfEditor, TEdit(Sender).Text);
end;

procedure TfrmOptionsFonts.edtViewerFontExit(Sender: TObject);
begin
  SetFontName(dcfViewer, TEdit(Sender).Text);
end;

procedure TfrmOptionsFonts.edtViewerBookFontExit(Sender: TObject);
begin
  SetFontName(dcfViewerBook, TEdit(Sender).Text);
end;

procedure TfrmOptionsFonts.edtLogFontExit(Sender: TObject);
begin
  SetFontName(dcfLog, TEdit(Sender).Text);
end;

procedure TfrmOptionsFonts.edtMainFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfMain, TSpinEdit(Sender).Value);
end;

procedure TfrmOptionsFonts.edtEditorFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfEditor, TSpinEdit(Sender).Value);
end;

procedure TfrmOptionsFonts.edtViewerFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfViewer, TSpinEdit(Sender).Value);
end;

procedure TfrmOptionsFonts.edtViewerBookFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfViewerBook, TSpinEdit(Sender).Value);
end;

procedure TfrmOptionsFonts.edtLogFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfLog, TSpinEdit(Sender).Value);
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
  LoadFont(dcfMain);
  LoadFont(dcfEditor);
  LoadFont(dcfViewer);
  LoadFont(dcfLog);
  LoadFont(dcfViewerBook);
end;

function TfrmOptionsFonts.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  SaveFont(dcfMain);
  SaveFont(dcfEditor);
  SaveFont(dcfViewer);
  SaveFont(dcfLog);
  SaveFont(dcfViewerBook);
end;

procedure TfrmOptionsFonts.LoadFont(aDCFont: TDCFont);
 var xFont:TFont;
begin
  xFont:=TFont.Create;
  try
    FontOptionsToFont(gFonts[aDCFont], xFont);
    SetFont(aDCFont, xFont);
  finally
    FreeAndNil(xFont);
  end;
end;

procedure TfrmOptionsFonts.SaveFont(aDCFont: TDCFont);
begin
  FontToFontOptions(GetFont(aDCFont), gFonts[aDCFont]);
end;

procedure TfrmOptionsFonts.SetFont(aDCFont: TDCFont; aFont: TFont);
begin
  case aDCFont of
    dcfMain: begin
      edtMainFont.Font := aFont;
      edtMainFont.Text := aFont.Name;
      edtMainFontSize.Value := aFont.Size;
    end;
    dcfEditor: begin
      edtEditorFont.Font := aFont;
      edtEditorFont.Text := aFont.Name;
      edtEditorFontSize.Value := aFont.Size;
    end;
    dcfViewer: begin
      edtViewerFont.Font := aFont;
      edtViewerFont.Text := aFont.Name;
      edtViewerFontSize.Value := aFont.Size;
    end;
    dcfLog: begin
      edtLogFont.Font := aFont;
      edtLogFont.Text := aFont.Name;
      edtLogFontSize.Value := aFont.Size;
    end;
    dcfViewerBook: begin
      edtViewerBookFont.Font := aFont;
      edtViewerBookFont.Text := aFont.Name;
      edtViewerBookFontSize.Value := aFont.Size;
    end;
  end;
end;

procedure TfrmOptionsFonts.SetFontSize(aDCFont: TDCFont; aFontSize: Integer);
begin
  GetFont(aDCFont).Size:=aFontSize;
end;

procedure TfrmOptionsFonts.SetFontName(aDCFont: TDCFont; aFontName: String);
begin
  GetFont(aDCFont).Name:=aFontName;
end;

function TfrmOptionsFonts.GetFont(aDCFont: TDCFont): TFont;
begin
  case aDCFont of
    dcfMain      : result := edtMainFont.Font;
    dcfEditor    : result := edtEditorFont.Font;
    dcfViewer    : result := edtViewerFont.Font;
    dcfLog       : result := edtLogFont.Font;
    dcfViewerBook: result := edtViewerBookFont.Font;
    else result:=nil; //TODO: show error for programmer
  end;
end;

procedure TfrmOptionsFonts.RunDialogFont(aDCFont: TDCFont);
begin
  dlgFnt.Font := GetFont(aDCFont);
  if dlgFnt.Execute then SetFont(aDCFont, dlgFnt.Font);
end;


end.

