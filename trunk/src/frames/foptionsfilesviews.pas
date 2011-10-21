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
  Classes, SysUtils, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsFilesViews }

  TfrmOptionsFilesViews = class(TOptionsEditor)
    cbDateTimeFormat: TComboBox;
    cbDirBrackets: TCheckBox;
    cbListFilesInThread: TCheckBox;
    cbLoadIconsSeparately: TCheckBox;
    cbShortFileSizeFormat: TCheckBox;
    cbShowSystemFiles: TCheckBox;
    cbSortCaseSensitive: TCheckBox;
    cbSortMethod: TComboBox;
    cbSpaceMovesDown: TCheckBox;
    gbFormatting: TGroupBox;
    gbSorting: TGroupBox;
    gbMisc: TGroupBox;
    lblDateTimeExample: TLabel;
    lblDateTimeFormat: TLabel;
    lblSortMethod: TLabel;
    procedure cbDateTimeFormatChange(Sender: TObject);
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
  uDCUtils, uGlobs, uLng;

{ TfrmOptionsFilesViews }

procedure TfrmOptionsFilesViews.cbDateTimeFormatChange(Sender: TObject);
begin
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
end;

procedure TfrmOptionsFilesViews.Init;
begin
  ParseLineToList(rsOptSortMethod, cbSortMethod.Items);
end;

procedure TfrmOptionsFilesViews.Load;
begin
  cbSortCaseSensitive.Checked := gSortCaseSensitive;
  if gSortNatural then cbSortMethod.ItemIndex:= 1;
  cbShortFileSizeFormat.Checked :=gShortFileSizeFormat;
  cbDateTimeFormat.Text := gDateTimeFormat;
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
  cbSpaceMovesDown.Checked := gSpaceMovesDown;
  cbDirBrackets.Checked := gDirBrackets;
  cbShowSystemFiles.Checked:= gShowSystemFiles;
  cbListFilesInThread.Checked:= gListFilesInThread;
  cbLoadIconsSeparately.Checked:= gLoadIconsSeparately;
end;

function TfrmOptionsFilesViews.Save: TOptionsEditorSaveFlags;
begin
  gSortCaseSensitive := cbSortCaseSensitive.Checked;
  gSortNatural := (cbSortMethod.ItemIndex = 1);
  gShortFileSizeFormat := cbShortFileSizeFormat.Checked;
  gDateTimeFormat := cbDateTimeFormat.Text;
  gSpaceMovesDown := cbSpaceMovesDown.Checked;
  gDirBrackets := cbDirBrackets.Checked;
  gShowSystemFiles:= cbShowSystemFiles.Checked;
  gListFilesInThread:= cbListFilesInThread.Checked;
  gLoadIconsSeparately:= cbLoadIconsSeparately.Checked;

  Result := [];
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

