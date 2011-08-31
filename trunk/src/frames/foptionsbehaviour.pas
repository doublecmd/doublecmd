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

unit fOptionsBehaviour;

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
    cbLynxLike: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbMouseMode: TComboBox;
    cbOnlyOnce: TCheckBox;
    cbSelectionByMouse: TCheckBox;
    cbShortFileSizeFormat: TCheckBox;
    cbShowSystemFiles: TCheckBox;
    chkAutoFillColumns: TCheckBox;
    cmbAutoSizeColumn: TComboBox;
    edtRunTerm: TEdit;
    edtRunInTerm: TEdit;
    gbDateTimeFormat: TGroupBox;
    gbMisc1: TGroupBox;
    gbMisc2: TGroupBox;
    gbMisc3: TGroupBox;
    gbMisc4: TGroupBox;
    gbScrolling: TGroupBox;
    lblAutoSizeColumn: TLabel;
    lblDateTimeExample: TLabel;
    lblDateTimeFormat: TLabel;
    lblMouseMode: TLabel;
    lblRunTerm: TLabel;
    lblRunInTerm: TLabel;
    ledDriveBlackList: TLabeledEdit;
    rbScrollPageByPage: TRadioButton;
    rbScrollLineByLine: TRadioButton;
    rbScrollLineByLineCursor: TRadioButton;
    seWheelScrollLines: TSpinEdit;
    procedure FrameResize(Sender: TObject);
    procedure cbAlwaysShowTrayIconChange(Sender: TObject);
    procedure cbDateTimeFormatChange(Sender: TObject);
  protected
    procedure Init; override;
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uDCUtils, uLng;

{ TfrmOptionsBehavior }

procedure TfrmOptionsBehavior.FrameResize(Sender: TObject);
var
  iWidth: Integer;
begin
  iWidth:= Width div 2 - 26;
  gbMisc2.Width:= iWidth;
end;

procedure TfrmOptionsBehavior.cbAlwaysShowTrayIconChange(Sender: TObject);
begin
  // Force minimizing to tray when tray icon is always shown.
  cbMinimizeToTray.Enabled:= not cbAlwaysShowTrayIcon.Checked;
end;

procedure TfrmOptionsBehavior.cbDateTimeFormatChange(Sender: TObject);
begin
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
end;

procedure TfrmOptionsBehavior.Init;
begin
  ParseLineToList(rsOptMouseSelectionButton, cbMouseMode.Items);
  ParseLineToList(rsOptAutoSizeColumn, cmbAutoSizeColumn.Items);
end;

procedure TfrmOptionsBehavior.Load;
begin
  edtRunInTerm.Text:= gRunInTerm;
  edtRunTerm.Text:=gRunTerm;
  cbOnlyOnce.Checked:= gOnlyOneAppInstance;
  cbLynxLike.Checked:=gLynxLike;
  cbShortFileSizeFormat.Checked:=gShortFileSizeFormat;

  cbSelectionByMouse.Checked:=gMouseSelectionEnabled;
  cbMouseMode.ItemIndex := gMouseSelectionButton;

  chkAutoFillColumns.Checked:= gAutoFillColumns;
  cmbAutoSizeColumn.ItemIndex:= gAutoSizeColumn;

  case gScrollMode of
    smLineByLineCursor:
      rbScrollLineByLineCursor.Checked:= True;
    smLineByLine:
      rbScrollLineByLine.Checked:= True;
    smPageByPage:
      rbScrollPageByPage.Checked:= True;
    else
      rbScrollLineByLine.Checked:= True;
  end;
  seWheelScrollLines.Value:= gWheelScrollLines;

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

  gRunInTerm:=edtRunInTerm.Text;
  gRunTerm:= edtRunTerm.Text;
  gOnlyOneAppInstance:=cbOnlyOnce.Checked;
  gLynxLike:=cbLynxLike.Checked;
  gShortFileSizeFormat:=cbShortFileSizeFormat.Checked;

  if rbScrollLineByLineCursor.Checked then
    gScrollMode:= smLineByLineCursor
  else if rbScrollLineByLine.Checked then
    gScrollMode:= smLineByLine
  else if rbScrollPageByPage.Checked then
    gScrollMode:= smPageByPage;
  gWheelScrollLines:= seWheelScrollLines.Value;

  gMinimizeToTray:= cbMinimizeToTray.Checked;
  gAlwaysShowTrayIcon:= cbAlwaysShowTrayIcon.Checked;
  gDateTimeFormat := cbDateTimeFormat.Text;
  gCutTextToColWidth:= cbCutTextToColWidth.Checked;
  gDriveBlackList:= ledDriveBlackList.Text;
  gShowSystemFiles:= cbShowSystemFiles.Checked;
  gListFilesInThread:= cbListFilesInThread.Checked;
  gLoadIconsSeparately:= cbLoadIconsSeparately.Checked;

  gMouseSelectionEnabled := cbSelectionByMouse.Checked;
  gMouseSelectionButton := cbMouseMode.ItemIndex;

  gAutoFillColumns:= chkAutoFillColumns.Checked;
  gAutoSizeColumn:= cmbAutoSizeColumn.ItemIndex;
end;

initialization
  RegisterOptionsEditor(optedBehaviours, TfrmOptionsBehavior);

end.

