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

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   -This unit has been added in 2016.
}

unit fOptionsTreeViewMenu;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus,
  Dialogs, types,

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
  private
    { Private declarations }
    FLastSignature: dword;
  public
    { Public declarations }
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    destructor Destroy; override;
    function CanWeClose(var {%H-}WillNeedUpdateWindowView: boolean): boolean; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LCLProc, LCLIntf,

  //DC
  uGlobs, uLng, fmain, uShowMsg, DCOSUtils, fOptions, uComponentsSignature;

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

  FLastSignature := ComputeSignatureBasedOnComponent(self, $00000000);
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

  FLastSignature := ComputeSignatureBasedOnComponent(self, $00000000);
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

{ TfrmOptionsTreeViewMenu.CanWeClose }
function TfrmOptionsTreeViewMenu.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := (FLastSignature = ComputeSignatureBasedOnComponent(self, $00000000));
  if not Result then
  begin
    ShowOptions(TfrmOptionsTreeViewMenu);
    Answer := MsgBox(rsMsgTreeViewMenuModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
    case Answer of
      mmrYes:
      begin
        Save;
        Result := True;
      end;

      mmrNo: Result := True;
      else
        Result := False;
    end;
  end;
end;

end.
