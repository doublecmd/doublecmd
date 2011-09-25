{
   Double Commander
   -------------------------------------------------------------------------
   Behavior options page

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

unit fOptionsBehavior;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Spin,
  fOptionsFrame;

type

  { TfrmOptionsBehavior }

  TfrmOptionsBehavior = class(TOptionsEditor)
    cbAlwaysShowTrayIcon: TCheckBox;
    cbCutTextToColWidth: TCheckBox;
    cbDateTimeFormat: TComboBox;
    cbListFilesInThread: TCheckBox;
    cbLoadIconsSeparately: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbOnlyOnce: TCheckBox;
    cbShortFileSizeFormat: TCheckBox;
    cbShowSystemFiles: TCheckBox;
    chkAutoFillColumns: TCheckBox;
    cmbAutoSizeColumn: TComboBox;
    gbDateTimeFormat: TGroupBox;
    gbMisc1: TGroupBox;
    gbMisc2: TGroupBox;
    gbMisc3: TGroupBox;
    lblAutoSizeColumn: TLabel;
    lblDateTimeExample: TLabel;
    lblDateTimeFormat: TLabel;
    ledDriveBlackList: TLabeledEdit;
    procedure cbAlwaysShowTrayIconChange(Sender: TObject);
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
  uGlobs, uDCUtils, uLng;

{ TfrmOptionsBehavior }

procedure TfrmOptionsBehavior.cbAlwaysShowTrayIconChange(Sender: TObject);
begin
  // Force minimizing to tray when tray icon is always shown.
  cbMinimizeToTray.Enabled:= not cbAlwaysShowTrayIcon.Checked;
end;

procedure TfrmOptionsBehavior.cbDateTimeFormatChange(Sender: TObject);
begin
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
end;

class function TfrmOptionsBehavior.GetIconIndex: Integer;
begin
  Result := 1;
end;

class function TfrmOptionsBehavior.GetTitle: String;
begin
  Result := rsOptionsEditorBehavior;
end;

procedure TfrmOptionsBehavior.Init;
begin
  ParseLineToList(rsOptAutoSizeColumn, cmbAutoSizeColumn.Items);
end;

procedure TfrmOptionsBehavior.Load;
begin
  cbOnlyOnce.Checked:= gOnlyOneAppInstance;
  cbShortFileSizeFormat.Checked:=gShortFileSizeFormat;

  chkAutoFillColumns.Checked:= gAutoFillColumns;
  cmbAutoSizeColumn.ItemIndex:= gAutoSizeColumn;

  cbMinimizeToTray.Checked:= gMinimizeToTray;
  cbMinimizeToTray.Enabled:= not gAlwaysShowTrayIcon;
  cbAlwaysShowTrayIcon.Checked:= gAlwaysShowTrayIcon;
  cbDateTimeFormat.Text:= gDateTimeFormat;
  lblDateTimeExample.Caption:= FormatDateTime(gDateTimeFormat, Now);
  cbCutTextToColWidth.Checked:= gCutTextToColWidth;
  ledDriveBlackList.Text:= gDriveBlackList;
  cbShowSystemFiles.Checked:= gShowSystemFiles;
  cbListFilesInThread.Checked:= gListFilesInThread;
  cbLoadIconsSeparately.Checked:= gLoadIconsSeparately;
end;

function TfrmOptionsBehavior.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gOnlyOneAppInstance:=cbOnlyOnce.Checked;
  gShortFileSizeFormat:=cbShortFileSizeFormat.Checked;

  gMinimizeToTray:= cbMinimizeToTray.Checked;
  gAlwaysShowTrayIcon:= cbAlwaysShowTrayIcon.Checked;
  gDateTimeFormat := cbDateTimeFormat.Text;
  gCutTextToColWidth:= cbCutTextToColWidth.Checked;
  gDriveBlackList:= ledDriveBlackList.Text;
  gShowSystemFiles:= cbShowSystemFiles.Checked;
  gListFilesInThread:= cbListFilesInThread.Checked;
  gLoadIconsSeparately:= cbLoadIconsSeparately.Checked;

  gAutoFillColumns:= chkAutoFillColumns.Checked;
  gAutoSizeColumn:= cmbAutoSizeColumn.ItemIndex;
end;

end.

