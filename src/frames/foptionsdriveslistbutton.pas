{
   Double Commander
   -------------------------------------------------------------------------
   Drives list button options page

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

unit fOptionsDrivesListButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsDrivesListButton }

  TfrmOptionsDrivesListButton = class(TOptionsEditor)
    cbShowLabel: TCheckBox;
    cbShowFileSystem: TCheckBox;
    cbShowFreeSpace: TCheckBox;
    gbDrivesList: TGroupBox;
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

{ TfrmOptionsDrivesListButton }

procedure TfrmOptionsDrivesListButton.Load;
begin
  cbShowLabel.Checked      := dlbShowLabel in gDrivesListButtonOptions;
  cbShowFileSystem.Checked := dlbShowFileSystem in gDrivesListButtonOptions;
  cbShowFreeSpace.Checked  := dlbShowFreeSpace in gDrivesListButtonOptions;
end;

function TfrmOptionsDrivesListButton.Save: TOptionsEditorSaveFlags;
begin
  gDrivesListButtonOptions := [];
  if cbShowLabel.Checked then
    Include(gDrivesListButtonOptions, dlbShowLabel);
  if cbShowFileSystem.Checked then
    Include(gDrivesListButtonOptions, dlbShowFileSystem);
  if cbShowFreeSpace.Checked then
    Include(gDrivesListButtonOptions, dlbShowFreeSpace);

  Result := [];
end;

class function TfrmOptionsDrivesListButton.GetIconIndex: Integer;
begin
  Result := 31;
end;

class function TfrmOptionsDrivesListButton.GetTitle: String;
begin
  Result := rsOptionsEditorDrivesListButton;
end;

end.

