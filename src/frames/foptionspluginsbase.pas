{
   Double Commander
   -------------------------------------------------------------------------
   Plugins options page

   Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsPluginsBase;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, ComCtrls, StdCtrls, Grids, Buttons, Controls, ExtCtrls,

  //DC
  fOptionsFrame, uGlobs;

type
  { TfrmOptionsPluginsBase }
  TfrmOptionsPluginsBase = class(TOptionsEditor)
    pnlPlugIn: TPanel;
    lblPlugInDescription: TLabel;
    stgPlugins: TStringGrid;
    pnlButton: TPanel;
    btnToggleOptionPlugins: TBitBtn;
    btnAddPlugin: TBitBtn;
    btnEnablePlugin: TBitBtn;
    btnRemovePlugin: TBitBtn;
    btnTweakPlugin: TBitBtn;
    btnConfigPlugin: TBitBtn;
    ImgSwitchEnable: TImage;
    ImgSwitchDisable: TImage;
    ImgByPlugin: TImage;
    ImgByExtension: TImage;
    procedure btnPluginsNotImplementedClick(Sender: TObject);
    procedure btnRemovePluginClick(Sender: TObject);
    procedure btnTweakPluginClick(Sender: TObject);
    procedure stgPluginsDblClick(Sender: TObject);
    procedure stgPluginsDragOver(Sender, {%H-}Source: TObject; X, Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure stgPluginsDragDrop(Sender, {%H-}Source: TObject; X, Y: integer);
    procedure stgPluginsGetCellHint(Sender: TObject; ACol, ARow: integer; var HintText: string);
    procedure stgPluginsShowHint(Sender: TObject; HintInfo: PHintInfo);
    function GetPluginFilenameToSave(const Filename: string): string;
  private
    FPluginType: TPluginType;
  protected
    property PluginType: TPluginType read FPluginType write FPluginType;
    procedure Init; override;
    procedure ShowPluginsTable; virtual;
    procedure stgPluginsOnSelection(Sender: TObject; {%H-}aCol, {%H-}aRow: integer); virtual;
    procedure ActualDeletePlugin({%H-}iIndex: integer); virtual;
    procedure ActualPluginsMove({%H-}iSource, {%H-}iDestination: integer); virtual;
  public
    class function GetIconIndex: integer; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  StrUtils, LCLProc, Forms, Dialogs,

  //DC
  udcutils, uLng, uShowMsg, fTweakPlugin, uDefaultPlugins;

{ TfrmOptionsPluginsBase }

{ TfrmOptionsPluginsBase.Init }
procedure TfrmOptionsPluginsBase.Init;
begin
  // Localize plugins.
  stgPlugins.Columns.Items[0].Title.Caption := rsOptPluginsActive;
  stgPlugins.Columns.Items[1].Title.Caption := rsOptPluginsName;
  stgPlugins.Columns.Items[2].Title.Caption := rsOptPluginsRegisteredFor;
  stgPlugins.Columns.Items[3].Title.Caption := rsOptPluginsFileName;
  stgPlugins.OnSelection := @stgPluginsOnSelection;
end;

{ TfrmOptionsPluginsBase }
procedure TfrmOptionsPluginsBase.ShowPluginsTable;
begin
  //empty
end;

{ TfrmOptionsPluginsBase.stgPluginsOnSelection}
procedure TfrmOptionsPluginsBase.stgPluginsOnSelection(Sender: TObject; aCol, aRow: integer);
begin
  //empty
end;

{ TfrmOptionsPluginsBase.ActualDeletePlugin }
procedure TfrmOptionsPluginsBase.ActualDeletePlugin(iIndex: integer);
begin
  //empty
end;

{ TfrmOptionsPluginsBase.ActualPluginsMove }
procedure TfrmOptionsPluginsBase.ActualPluginsMove(iSource, iDestination: integer);
begin
  //empty
end;

{ TfrmOptionsPluginsBase.GetIconIndex }
class function TfrmOptionsPluginsBase.GetIconIndex: integer;
begin
  Result := 6;
end;

{ TfrmOptionsPluginsBase.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsPluginsBase.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  Result := False;
end;

{ TfrmOptionsPluginsBase.btnPluginsNotImplementedClick }
procedure TfrmOptionsPluginsBase.btnPluginsNotImplementedClick(Sender: TObject);
begin
  msgError(rsMsgNotImplemented);
end;

{ TfrmOptionsPluginsBase.btnRemovePluginClick }
procedure TfrmOptionsPluginsBase.btnRemovePluginClick(Sender: TObject);
var
  iCurrentSelection: integer;
begin
  iCurrentSelection := stgPlugins.Row;
  if iCurrentSelection < stgPlugins.FixedRows then
    Exit;
  self.ActualDeletePlugin(pred(iCurrentSelection));
  stgPlugins.DeleteColRow(False, iCurrentSelection);
  if iCurrentSelection < stgPlugins.RowCount then
    stgPlugins.Row := iCurrentSelection
  else if stgPlugins.RowCount > 1 then
    stgPlugins.Row := pred(stgPlugins.RowCount)
  else
    stgPlugins.Row := -1;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsBase. }
procedure TfrmOptionsPluginsBase.btnTweakPluginClick(Sender: TObject);
var
  iPluginIndex: integer;
begin
  iPluginIndex := stgPlugins.Row - stgPlugins.FixedRows;
  if iPluginIndex < 0 then
    Exit;
  if ShowTweakPluginDlg(PluginType, iPluginIndex) then
    ShowPluginsTable;
end;

{ TfrmOptionsPluginsBase.stgPluginsDblClick }
procedure TfrmOptionsPluginsBase.stgPluginsDblClick(Sender: TObject);
begin
  if btnTweakPlugin.Enabled then
    btnTweakPlugin.Click;
end;

{ TfrmOptionsPluginsBase.stgPluginsDragOver }
procedure TfrmOptionsPluginsBase.stgPluginsDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
var
  iDestCol: integer = 0;
  iDestRow: integer = 0;
begin
  stgPlugins.MouseToCell(X, Y, iDestCol, iDestRow);
  Accept := (iDestRow > 0);
end;

{ TfrmOptionsPluginsBase.stgPluginsDragDrop }
procedure TfrmOptionsPluginsBase.stgPluginsDragDrop(Sender, Source: TObject; X, Y: integer);
var
  iDestCol, iDestRow, iSourceRow: integer;
begin
  stgPlugins.MouseToCell(X, Y, {%H-}iDestCol, {%H-}iDestRow);
  if iDestRow > 0 then
  begin
    iSourceRow := stgPlugins.Row; //We need to that because after having done the following "MoveColRow", the "stgPlugins.Row" changed! So we need to remember original index.
    stgPlugins.MoveColRow(False, iSourceRow, iDestRow);
    ActualPluginsMove(pred(iSourceRow), pred(iDestRow));
  end;
end;

{ TfrmOptionsPluginsBase.stgPluginsGetCellHint }
procedure TfrmOptionsPluginsBase.stgPluginsGetCellHint(Sender: TObject; ACol, ARow: integer; var HintText: string);
var
  sMaybeHint: string;
begin
  //The actual "pipe" symbol interfere when showing the hint. Let's replace it with a similar look-alike symbol.
  sMaybeHint := Stringreplace(stgPlugins.Cells[ACol, ARow], '|', '¦', [rfReplaceAll]);
  HintText := IfThen(((stgPlugins.Canvas.TextWidth(sMaybeHint) + 10) > stgPlugins.ColWidths[ACol]), sMaybeHint, '');
end;

{ TfrmOptionsPluginsWLX.stgPluginsShowHint }
procedure TfrmOptionsPluginsBase.stgPluginsShowHint(Sender: TObject; HintInfo: PHintInfo);
begin
  if gFileInfoToolTipValue[Ord(gToolTipHideTimeOut)] <> -1 then
    HintInfo^.HideTimeout := gFileInfoToolTipValue[Ord(gToolTipHideTimeOut)];
end;

{ GetPluginFilenameToSave }
function TfrmOptionsPluginsBase.GetPluginFilenameToSave(const Filename: string): string;
var
  sMaybeBasePath, SubWorkingPath, MaybeSubstitionPossible: string;
begin
  Result := Filename;

  sMaybeBasePath := IfThen((gPluginFilenameStyle = pfsRelativeToDC), EnvVarCommanderPath, gPluginPathToBeRelativeTo);

  case gPluginFilenameStyle of
    pfsAbsolutePath: ;
    pfsRelativeToDC, pfsRelativeToFollowingPath:
    begin
      SubWorkingPath := IncludeTrailingPathDelimiter(mbExpandFileName(sMaybeBasePath));
      MaybeSubstitionPossible := ExtractRelativePath(IncludeTrailingPathDelimiter(SubWorkingPath), Filename);
      if MaybeSubstitionPossible <> Filename then
        Result := IncludeTrailingPathDelimiter(sMaybeBasePath) + MaybeSubstitionPossible;
    end;
  end;
end;

end.
