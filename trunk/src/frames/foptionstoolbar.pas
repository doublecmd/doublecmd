{
   Double Commander
   -------------------------------------------------------------------------
   Toolbar configuration options page

   Copyright (C) 2012      Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, fOptionsFrame,
  fOptionsToolbarBase;

type

  { TfrmOptionsToolbar }

  TfrmOptionsToolbar = class(TfrmOptionsToolbarBase)
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  KASToolBar, DCXmlConfig, uGlobs, uGlobsPaths, uSpecialDir, uLng;

{ TfrmOptionsToolbar }

class function TfrmOptionsToolbar.GetTitle: String;
begin
  Result := rsOptionsEditorToolbar;
end;

procedure TfrmOptionsToolbar.Load;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  trbBarSize.Position   := gToolBarButtonSize div 2;
  trbIconSize.Position  := gToolBarIconSize div 2;
  cbFlatButtons.Checked := gToolBarFlat;
  cbShowCaptions.Checked := gToolBarShowCaptions;
  cbReportErrorWithCommands.Checked := gToolbarReportErrorWithCommands;

  lblBarSizeValue.Caption  := IntToStr(trbBarSize.Position*2);
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);

  FCurrentButton := nil;
  CloseToolbarsBelowCurrentButton;

  ToolBar := GetTopToolbar;
  ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
  LoadToolbar(ToolBar, gConfig, ToolBarNode, tocl_FlushCurrentToolbarContent);
  if ToolBar.ButtonCount > 0 then
    PressButtonDown(ToolBar.Buttons[0]);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);

  FUpdateHotKey := False;
end;

function TfrmOptionsToolbar.Save: TOptionsEditorSaveFlags;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  ApplyEditControls;

  gToolBarFlat         := cbFlatButtons.Checked;
  gToolBarShowCaptions := cbShowCaptions.Checked;
  gToolbarReportErrorWithCommands := cbReportErrorWithCommands.Checked;
  gToolBarButtonSize   := trbBarSize.Position * 2;
  gToolBarIconSize     := trbIconSize.Position * 2;

  ToolBar := GetTopToolbar;
  if Assigned(ToolBar) then
  begin
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', True);
    gConfig.ClearNode(ToolBarNode);
    Toolbar.SaveConfiguration(gConfig, ToolBarNode);
  end;

  if FUpdateHotKey then
  begin
    FUpdateHotKey := False;
    HotMan.Save(gpCfgDir + gNameSCFile);
  end;

  Result := [];
end;

constructor TfrmOptionsToolbar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name := 'frmOptionsToolbar';
end;

end.
