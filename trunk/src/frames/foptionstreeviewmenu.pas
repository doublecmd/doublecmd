{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of TreeView Menu behavior options.

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit fOptionsTreeViewMenu;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus,
  Dialogs,

  //DC
  fOptionsFrame;

type
  { TfrmOptionsTreeViewMenu }
  TfrmOptionsTreeViewMenu = class(TOptionsEditor)
    gbTreeViewMenuSettings: TGroupBox;
    gbWhereToUseTreeViewMenu: TGroupBox;
    lblUseInDirectoryHotlist: TLabel;
    ckbDirectoryHotlistFromMenuCommand: TCheckBox;
    ckbDirectoryHotlistFromDoubleClick: TCheckBox;
    lblUseWithFavoriteTabs: TLabel;
    ckbFavoritaTabsFromMenuCommand: TCheckBox;
    ckbFavoriteTabsFromDoubleClick: TCheckBox;
    lblUseWithHistory: TLabel;
    ckbUseForDirHistory: TCheckBox;
    ckbUseForViewHistory: TCheckBox;
    ckbUseForCommandLineHistory: TCheckBox;
    gbBehavior: TGroupBox;
    ckbShortcutSelectAndClose: TCheckBox;
    ckbSingleClickSelect: TCheckBox;
    ckbDoubleClickSelect: TCheckBox;
    lblNote: TLabel;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    { Public declarations }
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LCLProc, LCLIntf,

  //DC
  uGlobs, uLng, fmain, DCOSUtils;

{ TfrmOptionsTreeViewMenu.Init }
procedure TfrmOptionsTreeViewMenu.Init;
begin
  //Nothing here for the moment, but let's take the good habit to reserve the place to load eventual TComboBox and stuff like that with a ressource form the language file.
end;

{ TfrmOptionsTreeViewMenu.Load }
procedure TfrmOptionsTreeViewMenu.Load;
begin
  ckbDirectoryHotlistFromMenuCommand.Checked := gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand;
  ckbDirectoryHotlistFromDoubleClick.Checked := gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick;
  ckbFavoritaTabsFromMenuCommand.Checked := gUseTreeViewMenuWithFavoriteTabsFromMenuCommand;
  ckbFavoriteTabsFromDoubleClick.Checked := gUseTreeViewMenuWithFavoriteTabsFromDoubleClick;
  ckbUseForDirHistory.Checked := gUseTreeViewMenuWithDirHistory;
  ckbUseForViewHistory.Checked := gUseTreeViewMenuWithViewHistory;
  ckbUseForCommandLineHistory.Checked := gUseTreeViewMenuWithCommandLineHistory;
  ckbShortcutSelectAndClose.Checked := gTreeViewMenuShortcutExit;
  ckbSingleClickSelect.Checked := gTreeViewMenuSingleClickExit;
  ckbDoubleClickSelect.Checked := gTreeViewMenuDoubleClickExit;
end;

{ TfrmOptionsTreeViewMenu.Save }
function TfrmOptionsTreeViewMenu.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand := ckbDirectoryHotlistFromMenuCommand.Checked;
  gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick := ckbDirectoryHotlistFromDoubleClick.Checked;
  gUseTreeViewMenuWithFavoriteTabsFromMenuCommand := ckbFavoritaTabsFromMenuCommand.Checked;
  gUseTreeViewMenuWithFavoriteTabsFromDoubleClick := ckbFavoriteTabsFromDoubleClick.Checked;
  gUseTreeViewMenuWithDirHistory := ckbUseForDirHistory.Checked;
  gUseTreeViewMenuWithViewHistory := ckbUseForViewHistory.Checked;
  gUseTreeViewMenuWithCommandLineHistory := ckbUseForCommandLineHistory.Checked;
  gTreeViewMenuShortcutExit := ckbShortcutSelectAndClose.Checked;
  gTreeViewMenuSingleClickExit := ckbSingleClickSelect.Checked;
  gTreeViewMenuDoubleClickExit := ckbDoubleClickSelect.Checked;
end;

{ TfrmOptionsTreeViewMenu.GetIconIndex }
class function TfrmOptionsTreeViewMenu.GetIconIndex: integer;
begin
  Result := 39;
end;

{ TfrmOptionsTreeViewMenu.GetTitle }
class function TfrmOptionsTreeViewMenu.GetTitle: string;
begin
  Result := rsOptionsEditorTreeViewMenu;
end;

{ TfrmOptionsTreeViewMenu.Destroy }
destructor TfrmOptionsTreeViewMenu.Destroy;
begin
  inherited Destroy;
end;

end.
