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
  Classes, SysUtils, StdCtrls, ExtCtrls,
  fOptionsFrame;

type

  { TfrmOptionsBehavior }

  TfrmOptionsBehavior = class(TOptionsEditor)
    cbAlwaysShowTrayIcon: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbOnlyOnce: TCheckBox;
    cbBlacklistUnmountedDevices: TCheckBox;
    edtDrivesBlackList: TEdit;
    gbMisc1: TGroupBox;
    gbMisc2: TGroupBox;
    lblDrivesBlackList: TLabel;
    procedure cbAlwaysShowTrayIconChange(Sender: TObject);
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
  uGlobs, uLng;

{ TfrmOptionsBehavior }

procedure TfrmOptionsBehavior.cbAlwaysShowTrayIconChange(Sender: TObject);
begin
  // Force minimizing to tray when tray icon is always shown.
  cbMinimizeToTray.Enabled:= not cbAlwaysShowTrayIcon.Checked;
end;

class function TfrmOptionsBehavior.GetIconIndex: Integer;
begin
  Result := 1;
end;

class function TfrmOptionsBehavior.GetTitle: String;
begin
  Result := rsOptionsEditorBehavior;
end;

procedure TfrmOptionsBehavior.Load;
begin
  cbOnlyOnce.Checked:= gOnlyOneAppInstance;
  cbMinimizeToTray.Checked:= gMinimizeToTray;
  cbMinimizeToTray.Enabled:= not gAlwaysShowTrayIcon;
  cbAlwaysShowTrayIcon.Checked:= gAlwaysShowTrayIcon;
  edtDrivesBlackList.Text:= gDriveBlackList;
  cbBlacklistUnmountedDevices.Checked:= gDriveBlackListUnmounted;
end;

function TfrmOptionsBehavior.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gOnlyOneAppInstance:=cbOnlyOnce.Checked;
  gMinimizeToTray:= cbMinimizeToTray.Checked;
  gAlwaysShowTrayIcon:= cbAlwaysShowTrayIcon.Checked;
  gDriveBlackList:= edtDrivesBlackList.Text;
  gDriveBlackListUnmounted:= cbBlacklistUnmountedDevices.Checked;
end;

end.
