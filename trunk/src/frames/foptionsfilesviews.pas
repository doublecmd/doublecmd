{
   Double Commander
   -------------------------------------------------------------------------
   Files views options page

   Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fOptionsFilesViews;

{$mode objfpc}{$H+}

interface

uses
  uTypes, Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, Spin,
  fOptionsFrame;

type

  { TfrmOptionsFilesViews }

  TfrmOptionsFilesViews = class(TOptionsEditor)
    btnDefault: TButton;
    cbDateTimeFormat: TComboBox;
    cbHeaderFooterSizeFormat: TComboBox;
    cbOperationSizeFormat: TComboBox;
    cbUpdatedFilesPosition: TComboBox;
    cbNewFilesPosition: TComboBox;
    cbSortMethod: TComboBox;
    cbCaseSensitivity: TComboBox;
    cbSortFolderMode: TComboBox;
    cbFileSizeFormat: TComboBox;
    edByte: TEdit;
    edKilo: TEdit;
    edMega: TEdit;
    edGiga: TEdit;
    edTera: TEdit;
    gbFormatting: TGroupBox;
    gbSorting: TGroupBox;
    gbPersonalizedAbbreviationToUse: TGroupBox;
    lblByte: TLabel;
    lblKilobyte: TLabel;
    lblMegabyte: TLabel;
    lblGigabyte: TLabel;
    lblTerabyte: TLabel;
    lblHeaderFooterSizeExample: TLabel;
    lblHeaderFooterSizeFormat: TLabel;
    lblOperationSizeExample: TLabel;
    lblOperationSizeFormat: TLabel;
    lblFileSizeExample: TLabel;
    lblDateTimeExample: TLabel;
    lblUpdatedFilesPosition: TLabel;
    lblSortFolderMode: TLabel;
    lblCaseSensitivity: TLabel;
    lblDateTimeFormat: TLabel;
    lblNewFilesPosition: TLabel;
    lblSortMethod: TLabel;
    lblFileSizeFormat: TLabel;
    pnlDateTime: TPanel;
    speNumberOfDigitsFile: TSpinEdit;
    speNumberOfDigitsHeaderFooter: TSpinEdit;
    speNumberOfDigitsOperation: TSpinEdit;
    procedure btnDefaultClick(Sender: TObject);
    procedure cbDateTimeFormatChange(Sender: TObject);
    procedure RefreshOurExamples(Sender: TObject);
    procedure TransferUnitsToOfficialUnits;
  private
    FIncorrectFormatMessage: string;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    procedure AfterConstruction; override;
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng, uDCUtils;

const
  cFileSizeExample = 1335875825;

{ TfrmOptionsFilesViews }

procedure TfrmOptionsFilesViews.cbDateTimeFormatChange(Sender: TObject);
begin
  try
    lblDateTimeExample.Caption := FormatDateTime(cbDateTimeFormat.Text, Now);
    lblDateTimeExample.Font.Color := clDefault;
  except
    on E: EConvertError do
    begin
      lblDateTimeExample.Caption := FIncorrectFormatMessage;
      lblDateTimeExample.Font.Color := clRed;
    end;
  end;
end;

procedure TfrmOptionsFilesViews.RefreshOurExamples(Sender: TObject);
var
  PreserveUnits: array[fsfPersonalizedByte .. fsfPersonalizedTera] of string;
  iFileSizeFormat: TFileSizeFormat;
begin
  //We will temporary switch our units with official ones the time to show the preview.
  for iFileSizeFormat := fsfPersonalizedByte to fsfPersonalizedTera do PreserveUnits[iFileSizeFormat] := gSizeDisplayUnits[iFileSizeFormat];
  try
    TransferUnitsToOfficialUnits;
    lblFileSizeExample.Caption := CnvFormatFileSize(cFileSizeExample, TFileSizeFormat(cbFileSizeFormat.ItemIndex), speNumberOfDigitsFile.Value);
    lblHeaderFooterSizeExample.Caption := CnvFormatFileSize(cFileSizeExample, TFileSizeFormat(cbHeaderFooterSizeFormat.ItemIndex), speNumberOfDigitsHeaderFooter.Value);
    lblOperationSizeExample.Caption := CnvFormatFileSize(cFileSizeExample, TFileSizeFormat(cbOperationSizeFormat.ItemIndex), speNumberOfDigitsOperation.Value);
  finally
    //We restore the previous units.
    for iFileSizeFormat := fsfPersonalizedByte to fsfPersonalizedTera do gSizeDisplayUnits[iFileSizeFormat] := PreserveUnits[iFileSizeFormat];
  end;
end;

procedure TfrmOptionsFilesViews.Init;
begin
  ParseLineToList(rsOptSortMethod, cbSortMethod.Items);
  ParseLineToList(rsOptSortCaseSens, cbCaseSensitivity.Items);
  ParseLineToList(rsOptSortFolderMode, cbSortFolderMode.Items);
  ParseLineToList(rsOptNewFilesPosition, cbNewFilesPosition.Items);
  ParseLineToList(rsOptUpdatedFilesPosition, cbUpdatedFilesPosition.Items);
  ParseLineToList(rsOptFileSizeFloat + ';' + rsLegacyOperationByteSuffixLetter + ';' + rsLegacyDisplaySizeSingleLetterKilo + ';' + rsLegacyDisplaySizeSingleLetterMega + ';' + rsLegacyDisplaySizeSingleLetterGiga + ';' + rsLegacyDisplaySizeSingleLetterTera + ';' + rsOptPersonalizedFileSizeFormat, cbFileSizeFormat.Items);
  cbHeaderFooterSizeFormat.Items.Assign(cbFileSizeFormat.Items);
  cbOperationSizeFormat.Items.Assign(cbFileSizeFormat.Items);
end;

procedure TfrmOptionsFilesViews.Load;
begin
  case gSortCaseSensitivity of
    cstNotSensitive: cbCaseSensitivity.ItemIndex := 0;
    cstLocale: cbCaseSensitivity.ItemIndex := 1;
    cstCharValue: cbCaseSensitivity.ItemIndex := 2;
  end;
  if not gSortNatural then
    cbSortMethod.ItemIndex := 0
  else
    cbSortMethod.ItemIndex := 1;
  case gSortFolderMode of
    sfmSortNameShowFirst: cbSortFolderMode.ItemIndex := 0;
    sfmSortLikeFileShowFirst: cbSortFolderMode.ItemIndex := 1;
    sfmSortLikeFile: cbSortFolderMode.ItemIndex := 2;
  end;
  case gNewFilesPosition of
    nfpTop: cbNewFilesPosition.ItemIndex := 0;
    nfpTopAfterDirectories: cbNewFilesPosition.ItemIndex := 1;
    nfpSortedPosition: cbNewFilesPosition.ItemIndex := 2;
    nfpBottom: cbNewFilesPosition.ItemIndex := 3;
  end;
  case gUpdatedFilesPosition of
    ufpNoChange: cbUpdatedFilesPosition.ItemIndex := 0;
    ufpSameAsNewFiles: cbUpdatedFilesPosition.ItemIndex := 1;
    ufpSortedPosition: cbUpdatedFilesPosition.ItemIndex := 2;
  end;
  cbFileSizeFormat.ItemIndex := Ord(gFileSizeFormat);
  cbHeaderFooterSizeFormat.ItemIndex := Ord(gHeaderFooterSizeFormat);
  cbOperationSizeFormat.ItemIndex := Ord(gOperationSizeFormat);
  speNumberOfDigitsFile.Value := gFileSizeDigits;
  speNumberOfDigitsHeaderFooter.Value := gHeaderFooterDigits;
  speNumberOfDigitsOperation.Value := gOperationSizeDigits;
  edByte.Text := Trim(gSizeDisplayUnits[fsfPersonalizedByte]);
  edKilo.Text := Trim(gSizeDisplayUnits[fsfPersonalizedKilo]);
  edMega.Text := Trim(gSizeDisplayUnits[fsfPersonalizedMega]);
  edGiga.Text := Trim(gSizeDisplayUnits[fsfPersonalizedGiga]);
  edTera.Text := Trim(gSizeDisplayUnits[fsfPersonalizedTera]);
  cbDateTimeFormat.Text := gDateTimeFormat;
  lblDateTimeExample.Caption := FormatDateTime(cbDateTimeFormat.Text, Now);

  lblFileSizeExample.Constraints.MinWidth := lblFileSizeExample.Canvas.TextWidth(CnvFormatFileSize(cFileSizeExample, fsfKilo, speNumberOfDigitsFile.MaxValue) + 'WWW');
  lblHeaderFooterSizeExample.Constraints.MinWidth := lblHeaderFooterSizeExample.Canvas.TextWidth(CnvFormatFileSize(cFileSizeExample, fsfKilo, speNumberOfDigitsHeaderFooter.MaxValue) + 'WWW');
  lblOperationSizeExample.Constraints.MinWidth := lblOperationSizeExample.Canvas.TextWidth(CnvFormatFileSize(cFileSizeExample, fsfKilo, speNumberOfDigitsOperation.MaxValue) + 'WWW');

  Self.RefreshOurExamples(nil);
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
  gHeaderFooterSizeFormat := TFileSizeFormat(cbHeaderFooterSizeFormat.ItemIndex);
  gOperationSizeFormat := TFileSizeFormat(cbOperationSizeFormat.ItemIndex);
  gFileSizeDigits := speNumberOfDigitsFile.Value;
  gHeaderFooterDigits := speNumberOfDigitsHeaderFooter.Value;
  gOperationSizeDigits := speNumberOfDigitsOperation.Value;
  TransferUnitsToOfficialUnits;
  gDateTimeFormat := GetValidDateTimeFormat(cbDateTimeFormat.Text, gDateTimeFormat);

  Result := [];
end;

procedure TfrmOptionsFilesViews.AfterConstruction;
begin
  inherited AfterConstruction;
  //save localized "Incorrect format" string
  FIncorrectFormatMessage := lblDateTimeExample.Caption;
end;

class function TfrmOptionsFilesViews.GetIconIndex: integer;
begin
  Result := 29;
end;

class function TfrmOptionsFilesViews.GetTitle: string;
begin
  Result := rsOptionsEditorFilesViews;
end;


procedure TfrmOptionsFilesViews.btnDefaultClick(Sender: TObject);
begin
  Self.edByte.Text := Trim(rsDefaultPersonalizedAbbrevByte);
  Self.edKilo.Text := Trim(rsDefaultPersonalizedAbbrevKilo);
  Self.edMega.Text := Trim(rsDefaultPersonalizedAbbrevMega);
  Self.edGiga.Text := Trim(rsDefaultPersonalizedAbbrevGiga);
  Self.edTera.Text := Trim(rsDefaultPersonalizedAbbrevTera);
end;

procedure TfrmOptionsFilesViews.TransferUnitsToOfficialUnits;
begin
  gSizeDisplayUnits[fsfPersonalizedByte] := Trim(edByte.Text);
  if gSizeDisplayUnits[fsfPersonalizedByte] <> '' then gSizeDisplayUnits[fsfPersonalizedByte] := ' ' + gSizeDisplayUnits[fsfPersonalizedByte];
  gSizeDisplayUnits[fsfPersonalizedKilo] := ' ' + Trim(edKilo.Text);
  gSizeDisplayUnits[fsfPersonalizedMega] := ' ' + Trim(edMega.Text);
  gSizeDisplayUnits[fsfPersonalizedGiga] := ' ' + Trim(edGiga.Text);
  gSizeDisplayUnits[fsfPersonalizedTera] := ' ' + Trim(edTera.Text);
end;

end.

