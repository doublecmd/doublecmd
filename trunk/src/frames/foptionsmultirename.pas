{
   Double Commander
   -------------------------------------------------------------------------
   Multi-Rename options page

   Copyright (C) 2020 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsMultiRename;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, StdCtrls, ExtCtrls, EditBtn, Buttons, Menus,

  //DC
  fOptionsFrame;

type
  { TfrmOptionsMultiRename }
  TfrmOptionsMultiRename = class(TOptionsEditor)
    ckbShowMenuBarOnTop: TCheckBox;
    edInvalidCharReplacement: TEdit;
    lbInvalidCharReplacement: TLabel;
    rgLaunchBehavior: TRadioGroup;
    rgExitModifiedPreset: TRadioGroup;
    gbSaveRenamingLog: TGroupBox;
    rbRenamingLogPerPreset: TRadioButton;
    rbRenamingLogAppendSameFile: TRadioButton;
    fneMulRenLogFilename: TFileNameEdit;
    btnMulRenLogFilenameRelative: TSpeedButton;
    btnMulRenLogFilenameView: TSpeedButton;
    ckbDailyIndividualDirMultRenLog: TCheckBox;
    ckbFilenameWithFullPathInLog: TCheckBox;
    pmPathToBeRelativeToHelper: TPopupMenu;
    procedure rbRenamingLogAppendSameFileChange(Sender: TObject);
    procedure btnMulRenLogFilenameRelativeClick(Sender: TObject);
    procedure btnMulRenLogFilenameViewClick(Sender: TObject);
  private
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Controls,

  //DC
  uShowMsg, uShowForm, uDCUtils, uSpecialDir, DCStrUtils, uGlobs, uLng;

{ TfrmOptionsMultiRename }

{ TfrmOptionsMultiRename.GetIconIndex }
class function TfrmOptionsMultiRename.GetIconIndex: integer;
begin
  Result := 42;
end;

{ TfrmOptionsMultiRename.GetTitle }
class function TfrmOptionsMultiRename.GetTitle: string;
begin
  Result := rsOptionsEditorMultiRename;
end;

{ TfrmOptionsMultiRename.Init }
procedure TfrmOptionsMultiRename.Init;
begin
  ParseLineToList(rsMulRenExitModifiedPresetOptions, rgExitModifiedPreset.Items);
  ParseLineToList(rsMulRenLaunchBehaviorOptions, rgLaunchBehavior.Items);
end;

{ TfrmOptionsMultiRename.Load }
procedure TfrmOptionsMultiRename.Load;
begin
  ckbShowMenuBarOnTop.Checked := gMulRenShowMenuBarOnTop;
  edInvalidCharReplacement.Text := gMulRenInvalidCharReplacement;
  rgLaunchBehavior.ItemIndex := integer(gMulRenLaunchBehavior);
  rgExitModifiedPreset.ItemIndex := integer(gMulRenExitModifiedPreset);
  case gMulRenSaveRenamingLog of
    mrsrlPerPreset:
    begin
      rbRenamingLogPerPreset.Checked := True;
      rbRenamingLogAppendSameFileChange(rbRenamingLogAppendSameFile);
    end;
    mrsrlAppendSameLog: rbRenamingLogAppendSameFile.Checked := True;
  end;
  fneMulRenLogFilename.FileName := gMulRenLogFilename;
  ckbDailyIndividualDirMultRenLog.Checked := gMultRenDailyIndividualDirLog;
  ckbFilenameWithFullPathInLog.Checked := gMulRenFilenameWithFullPathInLog;

  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathToBeRelativeToHelper, mp_PATHHELPER, nil);
end;

{ TfrmOptionsMultiRename.Save }
function TfrmOptionsMultiRename.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  gMulRenShowMenuBarOnTop := ckbShowMenuBarOnTop.Checked;
  gMulRenInvalidCharReplacement := edInvalidCharReplacement.Text;
  gMulRenLaunchBehavior := TMulRenLaunchBehavior(rgLaunchBehavior.ItemIndex);
  gMulRenExitModifiedPreset := TMulRenExitModifiedPreset(rgExitModifiedPreset.ItemIndex);
  if rbRenamingLogPerPreset.Checked then
    gMulRenSaveRenamingLog := mrsrlPerPreset
  else
    gMulRenSaveRenamingLog := mrsrlAppendSameLog;
  gMulRenLogFilename := fneMulRenLogFilename.FileName;
  gMultRenDailyIndividualDirLog := ckbDailyIndividualDirMultRenLog.Checked;
  gMulRenFilenameWithFullPathInLog := ckbFilenameWithFullPathInLog.Checked;
end;

{ TfrmOptionsMultiRename.rbRenamingLogAppendSameFileChange }
procedure TfrmOptionsMultiRename.rbRenamingLogAppendSameFileChange(Sender: TObject);
begin
  fneMulRenLogFilename.Enabled := rbRenamingLogAppendSameFile.Checked;
  btnMulRenLogFilenameRelative.Enabled := rbRenamingLogAppendSameFile.Checked;
  btnMulRenLogFilenameView.Enabled := rbRenamingLogAppendSameFile.Checked;
  ckbDailyIndividualDirMultRenLog.Enabled := rbRenamingLogAppendSameFile.Checked;
end;

{ TfrmOptionsMultiRename.btnMulRenLogFilenameRelativeClick }
procedure TfrmOptionsMultiRename.btnMulRenLogFilenameRelativeClick(Sender: TObject);
begin
  fneMulRenLogFilename.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneMulRenLogFilename, pfFILE);
  pmPathToBeRelativeToHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsMultiRename.btnMulRenLogFilenameViewClick }
procedure TfrmOptionsMultiRename.btnMulRenLogFilenameViewClick(Sender: TObject);
var
  sRenameLogFilename: string;
begin
  if ckbDailyIndividualDirMultRenLog.Checked then
    sRenameLogFilename := mbExpandFileName(ExtractFilePath(fneMulRenLogFilename.FileName) + IncludeTrailingPathDelimiter(EnvVarTodaysDate) + ExtractFilename(fneMulRenLogFilename.FileName))
  else
    sRenameLogFilename := mbExpandFileName(fneMulRenLogFilename.FileName);

  if FileExists(sRenameLogFilename) then
    ShowViewerByGlob(sRenameLogFilename)
  else
    MsgError(Format(rsMsgFileNotFound, [sRenameLogFilename]));
end;

end.


