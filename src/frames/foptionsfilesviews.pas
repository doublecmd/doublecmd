{
   Double Commander
   -------------------------------------------------------------------------
   Files views options page

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

unit fOptionsFilesViews;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, ExtCtrls,
  fOptionsFrame;

type

  { TfrmOptionsFilesViews }

  TfrmOptionsFilesViews = class(TOptionsEditor)
    cbDateTimeFormat: TComboBox;
    cbHighlightUpdatedFiles: TCheckBox;
    cbDirBrackets: TCheckBox;
    cbListFilesInThread: TCheckBox;
    cbLoadIconsSeparately: TCheckBox;
    cbDelayLoadingTabs: TCheckBox;
    cbUpdatedFilesPosition: TComboBox;
    cbShowSystemFiles: TCheckBox;
    cbNewFilesPosition: TComboBox;
    cbSortMethod: TComboBox;
    cbSpaceMovesDown: TCheckBox;
    cbCaseSensitivity: TComboBox;
    cbSortFolderMode: TComboBox;
    cbFileSizeFormat: TComboBox;
    gbFormatting: TGroupBox;
    gbSorting: TGroupBox;
    gbMisc: TGroupBox;
    lblDateTimeExample: TLabel;
    lblUpdatedFilesPosition: TLabel;
    lblSortFolderMode: TLabel;
    lblCaseSensitivity: TLabel;
    lblDateTimeFormat: TLabel;
    lblNewFilesPosition: TLabel;
    lblSortMethod: TLabel;
    lblFileSizeFormat: TLabel;
    pnlDateTime: TPanel;
    procedure cbDateTimeFormatChange(Sender: TObject);
  private
    FIncorrectFormatMessage: string;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    procedure AfterConstruction; override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng, uTypes;

{ TfrmOptionsFilesViews }

procedure TfrmOptionsFilesViews.cbDateTimeFormatChange(Sender: TObject);
begin
  try
    lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
    lblDateTimeExample.Font.Color := clDefault;
  except
    on E: EConvertError do
    begin
      lblDateTimeExample.Caption:= FIncorrectFormatMessage;
      lblDateTimeExample.Font.Color := clRed;
    end;
  end;
end;

procedure TfrmOptionsFilesViews.Init;
begin
  ParseLineToList(rsOptSortMethod, cbSortMethod.Items);
  ParseLineToList(rsOptSortCaseSens, cbCaseSensitivity.Items);
  ParseLineToList(rsOptSortFolderMode, cbSortFolderMode.Items);
  ParseLineToList(rsOptNewFilesPosition, cbNewFilesPosition.Items);
  ParseLineToList(rsOptUpdatedFilesPosition, cbUpdatedFilesPosition.Items);
  ParseLineToList(rsOptFileSizeFormat, cbFileSizeFormat.Items);
end;

procedure TfrmOptionsFilesViews.Load;
begin
  case gSortCaseSensitivity of
    cstNotSensitive: cbCaseSensitivity.ItemIndex := 0;
    cstLocale:       cbCaseSensitivity.ItemIndex := 1;
    cstCharValue:    cbCaseSensitivity.ItemIndex := 2;
  end;
  if not gSortNatural then
    cbSortMethod.ItemIndex:= 0
  else
    cbSortMethod.ItemIndex:= 1;
  case gSortFolderMode of
    sfmSortNameShowFirst:      cbSortFolderMode.ItemIndex := 0;
    sfmSortLikeFileShowFirst:  cbSortFolderMode.ItemIndex := 1;
    sfmSortLikeFile:           cbSortFolderMode.ItemIndex := 2;
  end;
  case gNewFilesPosition of
    nfpTop:                 cbNewFilesPosition.ItemIndex := 0;
    nfpTopAfterDirectories: cbNewFilesPosition.ItemIndex := 1;
    nfpSortedPosition:      cbNewFilesPosition.ItemIndex := 2;
    nfpBottom:              cbNewFilesPosition.ItemIndex := 3;
  end;
  case gUpdatedFilesPosition of
    ufpNoChange:       cbUpdatedFilesPosition.ItemIndex := 0;
    ufpSameAsNewFiles: cbUpdatedFilesPosition.ItemIndex := 1;
    ufpSortedPosition: cbUpdatedFilesPosition.ItemIndex := 2;
  end;
  cbFileSizeFormat.ItemIndex := Ord(gFileSizeFormat);
  cbDateTimeFormat.Text := gDateTimeFormat;
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
  cbSpaceMovesDown.Checked := gSpaceMovesDown;
  cbDirBrackets.Checked := gDirBrackets;
  cbShowSystemFiles.Checked:= gShowSystemFiles;
  {$IFDEF LCLCARBON}
  // Under Mac OS X loading file list in separate thread are very very slow
  // so disable and hide this option under Mac OS X Carbon
  cbListFilesInThread.Visible:= False;
  {$ELSE}
  cbListFilesInThread.Checked:= gListFilesInThread;
  {$ENDIF}
  cbLoadIconsSeparately.Checked:= gLoadIconsSeparately;
  cbDelayLoadingTabs.Checked:= gDelayLoadingTabs;
  cbHighlightUpdatedFiles.Checked:= gHighlightUpdatedFiles;
end;

function TfrmOptionsFilesViews.Save: TOptionsEditorSaveFlags;
begin
  case cbCaseSensitivity.ItemIndex of
    0: gSortCaseSensitivity := cstNotSensitive;
    1: gSortCaseSensitivity := cstLocale;
    2: gSortCaseSensitivity := cstCharValue;
  end;
  gSortNatural := (cbSortMethod.ItemIndex = 1);
  case cbSortFolderMode.ItemIndex of
    0: gSortFolderMode := sfmSortNameShowFirst;
    1: gSortFolderMode := sfmSortLikeFileShowFirst;
    2: gSortFolderMode := sfmSortLikeFile;
  end;
  case cbNewFilesPosition.ItemIndex of
    0: gNewFilesPosition := nfpTop;
    1: gNewFilesPosition := nfpTopAfterDirectories;
    2: gNewFilesPosition := nfpSortedPosition;
    3: gNewFilesPosition := nfpBottom;
  end;
  case cbUpdatedFilesPosition.ItemIndex of
    0: gUpdatedFilesPosition := ufpNoChange;
    1: gUpdatedFilesPosition := ufpSameAsNewFiles;
    2: gUpdatedFilesPosition := ufpSortedPosition;
  end;
  gFileSizeFormat := TFileSizeFormat(cbFileSizeFormat.ItemIndex);

  gDateTimeFormat := GetValidDateTimeFormat(cbDateTimeFormat.Text, gDateTimeFormat);

  gSpaceMovesDown := cbSpaceMovesDown.Checked;
  gDirBrackets := cbDirBrackets.Checked;
  gShowSystemFiles:= cbShowSystemFiles.Checked;
  gListFilesInThread:= cbListFilesInThread.Checked;
  gLoadIconsSeparately:= cbLoadIconsSeparately.Checked;
  gDelayLoadingTabs := cbDelayLoadingTabs.Checked;
  gHighlightUpdatedFiles := cbHighlightUpdatedFiles.Checked;

  Result := [];
end;

procedure TfrmOptionsFilesViews.AfterConstruction;
begin
  inherited AfterConstruction;
  //save localized "Incorrect format" string
  FIncorrectFormatMessage := lblDateTimeExample.Caption;
end;

class function TfrmOptionsFilesViews.GetIconIndex: Integer;
begin
  Result := 29;
end;

class function TfrmOptionsFilesViews.GetTitle: String;
begin
  Result := rsOptionsEditorFilesViews;
end;

end.

