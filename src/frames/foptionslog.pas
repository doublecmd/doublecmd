{
   Double Commander
   -------------------------------------------------------------------------
   Log options page

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

unit fOptionsLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, EditBtn, Buttons, Menus, fOptionsFrame;

type

  { TfrmOptionsLog }

  TfrmOptionsLog = class(TOptionsEditor)
    cbLogArcOp: TCheckBox;
    cbLogCpMvLn: TCheckBox;
    cbLogDelete: TCheckBox;
    cbLogDirOp: TCheckBox;
    cbLogErrors: TCheckBox;
    cbLogFile: TCheckBox;
    cbIncludeDateInLogFilename: TCheckBox;
    cbLogInfo: TCheckBox;
    cbLogSuccess: TCheckBox;
    cbLogVFS: TCheckBox;
    cbLogStartShutdown: TCheckBox;
    fneLogFileName: TFileNameEdit;
    gbLogFile: TGroupBox;
    gbLogFileOp: TGroupBox;
    gbLogFileStatus: TGroupBox;
    btnRelativeLogFile: TSpeedButton;
    pmPathHelper: TPopupMenu;
    btnViewLogFile: TSpeedButton;
    procedure btnRelativeLogFileClick(Sender: TObject);
    procedure cbLogFileChange(Sender: TObject);
    procedure btnViewLogFileClick(Sender: TObject);
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
  uGlobs, uLng, uSpecialDir, uShowForm, uDCUtils;

{ TfrmOptionsLog }

class function TfrmOptionsLog.GetIconIndex: Integer;
begin
  Result := 23;
end;

class function TfrmOptionsLog.GetTitle: String;
begin
  Result := rsOptionsEditorLog;
end;

procedure TfrmOptionsLog.cbLogFileChange(Sender: TObject);
begin
  cbIncludeDateInLogFilename.Enabled:=cbLogFile.Checked;
end;

procedure TfrmOptionsLog.btnViewLogFileClick(Sender: TObject);
var
  MyYear,MyMonth,MyDay:word;
  TempoFilename:string;
begin
  TempoFilename:=ReplaceEnvVars(fneLogFileName.FileName);

  if cbIncludeDateInLogFilename.Checked then
  begin
    DecodeDate(now,MyYear,MyMonth,MyDay);
    TempoFilename:=copy(TempoFilename,1,length(TempoFilename)-length(ExtractFileExt(TempoFilename)))+
                   Format('_%d-%2.2d-%2.2d',[MyYear,MyMonth,MyDay])+
                   ExtractFileExt(TempoFilename);

  end;

  ShowViewerByGlob(TempoFilename);
end;

procedure TfrmOptionsLog.btnRelativeLogFileClick(Sender: TObject);
begin
  fneLogFileName.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneLogFileName,pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmOptionsLog.Load;
begin
  cbIncludeDateInLogFilename.Checked := gLogFileWithDateInName;
  cbLogFile.Checked := gLogFile;
  cbLogFileChange(cbLogFile);
  fneLogFileName.FileName := gLogFileName;
  cbLogStartShutdown.Checked := (log_start_shutdown in gLogOptions);
  cbLogCpMvLn.Checked := (log_cp_mv_ln in gLogOptions);
  cbLogDelete.Checked := (log_delete in gLogOptions);
  cbLogDirOp.Checked := (log_dir_op in gLogOptions);
  cbLogArcOp.Checked := (log_arc_op in gLogOptions);
  cbLogVFS.Checked := (log_vfs_op in gLogOptions);
  cbLogSuccess.Checked := (log_success in gLogOptions);
  cbLogErrors.Checked := (log_errors in gLogOptions);
  cbLogInfo.Checked := (log_info in gLogOptions);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
end;

function TfrmOptionsLog.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gLogFile := cbLogFile.Checked;
  gLogFileWithDateInName := cbIncludeDateInLogFilename.Checked;
  gLogFileName := fneLogFileName.FileName;
  gLogOptions := []; // Reset log options
  if cbLogStartShutdown.Checked then
    Include(gLogOptions, log_start_shutdown);
  if cbLogCpMvLn.Checked then
    Include(gLogOptions, log_cp_mv_ln);
  if cbLogDelete.Checked then
    Include(gLogOptions, log_delete);
  if cbLogDirOp.Checked then
    Include(gLogOptions, log_dir_op);
  if cbLogArcOp.Checked then
    Include(gLogOptions, log_arc_op);
  if cbLogVFS.Checked then
    Include(gLogOptions, log_vfs_op);
  if cbLogSuccess.Checked then
    Include(gLogOptions, log_success);
  if cbLogErrors.Checked then
    Include(gLogOptions, log_errors);
  if cbLogInfo.Checked then
    Include(gLogOptions, log_info);
end;

end.

