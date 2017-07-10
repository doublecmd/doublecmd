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
    btnSelConsoleFnt: TButton;
    btnSelMainFnt: TButton;
    btnSelViewerBookFnt: TButton;
    btnSelViewFnt: TButton;
    btnPathEditFnt: TButton;
    btnSearchResultsFnt: TButton;
    dlgFnt: TFontDialog;
    edtSearchResultsFont: TEdit;
    edtPathEditFont: TEdit;
    edtConsoleFontSize1: TSpinEdit;
    edtEditorFont: TEdit;
    edtEditorFontSize: TSpinEdit;
    edtLogFont: TEdit;
    edtConsoleFont: TEdit;
    edtLogFontSize: TSpinEdit;
    edtConsoleFontSize: TSpinEdit;
    edtMainFont: TEdit;
    edtMainFontSize: TSpinEdit;
    edtViewerBookFont: TEdit;
    edtViewerBookFontSize: TSpinEdit;
    edtViewerFont: TEdit;
    edtViewerFontSize: TSpinEdit;
    lblSearchResultsFont: TLabel;
    lblPathEditFont: TLabel;
    lblEditorFont: TLabel;
    lblLogFont: TLabel;
    lblConsoleFont: TLabel;
    lblMainFont: TLabel;
    lblViewerBookFont: TLabel;
    lblViewerFont: TLabel;
    edtPathEditFontSize: TSpinEdit;
    edtSearchresultsFontSize: TSpinEdit;
    procedure btnPathEditFntClick(Sender: TObject);
    procedure btnSelConsoleFntClick(Sender: TObject);
    procedure btnSelEditFntClick(Sender: TObject);
    procedure btnSelMainFntClick(Sender: TObject);
    procedure btnSelViewFntClick(Sender: TObject);
    procedure btnSelLogFntClick(Sender: TObject);
    procedure btnSelViewerBookFntClick(Sender: TObject);
    procedure edtConsoleFontExit(Sender: TObject);
    procedure edtConsoleFontSizeChange(Sender: TObject);
    procedure edtEditorFontExit(Sender: TObject);
    procedure edtEditorFontSizeChange(Sender: TObject);
    procedure edtLogFontExit(Sender: TObject);
    procedure edtMainFontExit(Sender: TObject);
    procedure edtMainFontSizeChange(Sender: TObject);
    procedure edtPathEditFontSizeChange(Sender: TObject);
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

procedure TfrmOptionsFonts.btnSelConsoleFntClick(Sender: TObject);
begin
  RunDialogFont(dcfConsole);
end;

procedure TfrmOptionsFonts.btnPathEditFntClick(Sender: TObject);
begin
  RunDialogFont(dcfPathEdit);
end;

procedure TfrmOptionsFonts.btnSelViewFntClick(Sender: TObject);
begin
  RunDialogFont(dcfViewer);
end;

procedure TfrmOptionsFonts.btnSelViewerBookFntClick(Sender: TObject);
begin
  RunDialogFont(dcfViewerBook);
end;

procedure TfrmOptionsFonts.edtConsoleFontExit(Sender: TObject);
begin
  SetFontName(dcfConsole, TEdit(Sender).Text);
end;

procedure TfrmOptionsFonts.edtConsoleFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfConsole, TSpinEdit(Sender).Value);
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

procedure TfrmOptionsFonts.edtPathEditFontSizeChange(Sender: TObject);
begin
  SetFontSize(dcfPathEdit, TSpinEdit(Sender).Value);
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
  LoadFont(dcfConsole);
  LoadFont(dcfPathEdit);
  LoadFont(dcfSearchResults);
end;

function TfrmOptionsFonts.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  SaveFont(dcfMain);
  SaveFont(dcfEditor);
  SaveFont(dcfViewer);
  SaveFont(dcfLog);
  SaveFont(dcfViewerBook);
  SaveFont(dcfConsole);
  SaveFont(dcfPathEdit);
  SaveFont(dcfSearchResults);
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
    dcfConsole: begin
      edtConsoleFont.Font := aFont;
      edtConsoleFont.Text := aFont.Name;
      edtConsoleFontSize.Value := aFont.Size;
    end;

    dcfSearchResults: begin
      edtSearchResultsFont.Font := aFont;
      edtSearchResultsFont.Text := aFont.Name;
      edtSearchResultsFontSize.Value := aFont.Size;
    end;
    dcfPathEdit: begin
      edtPathEditFont.Font := aFont;
      edtPathEditFont.Text := aFont.Name;
      edtPathEditFontSize.Value := aFont.Size;
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
    dcfMain      : Result := edtMainFont.Font;
    dcfEditor    : Result := edtEditorFont.Font;
    dcfViewer    : Result := edtViewerFont.Font;
    dcfLog       : Result := edtLogFont.Font;
    dcfViewerBook: Result := edtViewerBookFont.Font;
    dcfConsole   : Result := edtConsoleFont.Font;
    dcfSearchResults: Result := edtSearchResultsFont.Font;
    dcfPathEdit     : Result := edtPathEditFont.Font;

    else Result  := nil; // TODO: show error for programmer
  end;
end;

procedure TfrmOptionsFonts.RunDialogFont(aDCFont: TDCFont);
begin
  dlgFnt.Font := GetFont(aDCFont);
  if not (aDCFont in [dcfMain, dcfViewerBook]) then begin
    dlgFnt.Options:= dlgFnt.Options + [fdFixedPitchOnly];
  end;
  if dlgFnt.Execute then SetFont(aDCFont, dlgFnt.Font);
  dlgFnt.Options:= dlgFnt.Options - [fdFixedPitchOnly];
end;


end.

