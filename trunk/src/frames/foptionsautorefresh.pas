{
   Double Commander
   -------------------------------------------------------------------------
   Auto-refresh options page

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

unit fOptionsAutoRefresh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsAutoRefresh }

  TfrmOptionsAutoRefresh = class(TOptionsEditor)
    cbWatchAttributesChange: TCheckBox;
    cbWatchExcludeDirs: TCheckBox;
    cbWatchFileNameChange: TCheckBox;
    cbWatchOnlyForeground: TCheckBox;
    edtWatchExcludeDirs: TEdit;
    gbAutoRefreshDisable: TGroupBox;
    gbAutoRefreshEnable: TGroupBox;
    procedure cbWatchExcludeDirsChange(Sender: TObject);
    procedure OnAutoRefreshOptionChanged(Sender: TObject);
  private
    { private declarations }
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  uGlobs;

{ TfrmOptionsAutoRefresh }

procedure TfrmOptionsAutoRefresh.cbWatchExcludeDirsChange(Sender: TObject);
begin
  edtWatchExcludeDirs.Enabled := cbWatchExcludeDirs.Checked;
end;

procedure TfrmOptionsAutoRefresh.OnAutoRefreshOptionChanged(Sender: TObject);
begin
  gbAutoRefreshDisable.Enabled := cbWatchFileNameChange.Checked or
                                  cbWatchAttributesChange.Checked;
end;

procedure TfrmOptionsAutoRefresh.Load;
begin
  cbWatchFileNameChange.Checked := (watch_file_name_change in gWatchDirs);
  cbWatchAttributesChange.Checked := (watch_attributes_change in gWatchDirs);
  cbWatchOnlyForeground.Checked := (watch_only_foreground in gWatchDirs);
  cbWatchExcludeDirs.Checked := (watch_exclude_dirs in gWatchDirs);
  edtWatchExcludeDirs.Text := gWatchDirsExclude;
  OnAutoRefreshOptionChanged(nil);
  cbWatchExcludeDirsChange(nil);
end;

function TfrmOptionsAutoRefresh.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gWatchDirs := []; // Reset watch options
  if cbWatchFileNameChange.Checked then
    Include(gWatchDirs, watch_file_name_change);
  if cbWatchAttributesChange.Checked then
    Include(gWatchDirs, watch_attributes_change);
  if cbWatchOnlyForeground.Checked then
    Include(gWatchDirs, watch_only_foreground);
  if cbWatchExcludeDirs.Checked then
    Include(gWatchDirs, watch_exclude_dirs);
  gWatchDirsExclude:= edtWatchExcludeDirs.Text;
end;

initialization
  RegisterOptionsEditor(optedAutoRefresh, TfrmOptionsAutoRefresh);

end.

