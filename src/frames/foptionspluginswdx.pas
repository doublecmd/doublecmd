{
   Double Commander
   -------------------------------------------------------------------------
   Plugins WDX options page

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

unit fOptionsPluginsWDX;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, ComCtrls, StdCtrls, Grids, Buttons, Controls, ExtCtrls,

  //DC
  fOptionsFrame, uWDXModule, foptionspluginsbase;

type
  { TfrmOptionsPluginsWDX }
  TfrmOptionsPluginsWDX = class(TfrmOptionsPluginsBase)
    procedure btnAddPluginClick(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure Done; override;
    procedure stgPluginsOnSelection(Sender: TObject; {%H-}aCol, aRow: integer); override;
    procedure ActualAddPlugin(sPluginFilename: string); override;
    procedure ActualDeletePlugin(iIndex: integer); override;
    procedure ActualPluginsMove(iSource, iDestination: integer); override;
  public
    class function GetTitle: string; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
    procedure ShowPluginsTable; override;
  end;

var
  tmpWDXPlugins: TWDXModuleList;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LCLProc, Forms, Dialogs,

  //DC
  uLng, uGlobs, dmCommonData, DCStrUtils, uDefaultPlugins;

const
  COLNO_NAME = 0;
  COLNO_EXT = 1;
  COLNO_FILENAME = 2;

{ TfrmOptionsPluginsWDX }

{ TfrmOptionsPluginsWDX.Init }
procedure TfrmOptionsPluginsWDX.Init;
begin
  PluginType := ptWDX;
  inherited Init;
  stgPlugins.Columns.Items[COLNO_NAME].Title.Caption := rsOptPluginsName;
  stgPlugins.Columns.Items[COLNO_NAME].Alignment := taLeftJustify; // Because from the "Base", it was centered.
  stgPlugins.Columns.Items[COLNO_NAME].Width := stgPlugins.Columns.Items[succ(COLNO_NAME)].Width;
  stgPlugins.Columns.Items[COLNO_EXT].Title.Caption := rsOptPluginsRegisteredFor;
  stgPlugins.Columns.Items[COLNO_EXT].Width := 183;
  stgPlugins.Columns.Items[COLNO_EXT].Width := stgPlugins.Columns.Items[succ(COLNO_EXT)].Width;
  stgPlugins.Columns.Items[COLNO_FILENAME].Title.Caption := rsOptPluginsFileName;
  stgPlugins.Columns.Delete(succ(COLNO_FILENAME));
  btnEnablePlugin.Visible := False; //Because with WDX there is no enable/disable.
  btnConfigPlugin.Visible := False;
  tmpWDXPlugins := TWDXModuleList.Create;
end;

{ TfrmOptionsPluginsWDX.Load }
procedure TfrmOptionsPluginsWDX.Load;
begin
  tmpWDXPlugins.Assign(gWDXPlugins);
  ShowPluginsTable;
end;

{ TfrmOptionsPluginsWDX.Save }
function TfrmOptionsPluginsWDX.Save: TOptionsEditorSaveFlags;
begin
  gWDXPlugins.Assign(tmpWDXPlugins);
  Result := [];
end;

{ TfrmOptionsPluginsWDX.Done }
procedure TfrmOptionsPluginsWDX.Done;
begin
  FreeThenNil(tmpWDXPlugins);
end;

{ TfrmOptionsPluginsWDX.GetTitle }
class function TfrmOptionsPluginsWDX.GetTitle: string;
begin
  Result := rsOptionsEditorPlugins + ' WDX';
end;

{ TfrmOptionsPluginsWDX.ExtraOptionsSignature }
function TfrmOptionsPluginsWDX.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  Result := tmpWDXPlugins.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsPluginsWDX.ShowPluginsTable }
procedure TfrmOptionsPluginsWDX.ShowPluginsTable;
var
  I: integer;
begin
  stgPlugins.RowCount := tmpWDXPlugins.Count + stgPlugins.FixedRows;
  for i := 0 to pred(tmpWDXPlugins.Count) do
  begin
    stgPlugins.Cells[COLNO_NAME, I + stgPlugins.FixedRows] := tmpWDXPlugins.GetWdxModule(i).Name;
    stgPlugins.Cells[COLNO_EXT, I + stgPlugins.FixedRows] := tmpWDXPlugins.GetWdxModule(i).DetectStr;
    stgPlugins.Cells[COLNO_FILENAME, I + stgPlugins.FixedRows] := tmpWDXPlugins.GetWdxModule(i).FileName;
  end;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsWDX.stgPluginsOnSelection }
procedure TfrmOptionsPluginsWDX.stgPluginsOnSelection(Sender: TObject; aCol, aRow: integer);
var
  bEnable: boolean = False;
begin
  if (aRow > 0) and (aRow < stgPlugins.RowCount) then
    bEnable := not (tmpWDXPlugins.GetWdxModule(aRow - stgPlugins.FixedRows) is TEmbeddedWDX);
  btnRemovePlugin.Enabled := bEnable;
  btnTweakPlugin.Enabled := bEnable;
  btnConfigPlugin.Enabled := bEnable;
end;

{ TfrmOptionsPluginsWDX.btnAddPluginClick }
procedure TfrmOptionsPluginsWDX.btnAddPluginClick(Sender: TObject);
begin
  dmComData.OpenDialog.Filter := Format('Content plugins (%s;*.lua)|%s;*.lua', [WdxMask, WdxMask]);
  if dmComData.OpenDialog.Execute then
    ActualAddPlugin(dmComData.OpenDialog.FileName);
end;

{ TfrmOptionsPluginsWDX.ActualAddPlugin }
procedure TfrmOptionsPluginsWDX.ActualAddPlugin(sPluginFilename: string);
var
  I, J: integer;
  sPluginName: string;
begin
  if not (StrEnds(sPluginFilename, '.lua') or CheckPlugin(sPluginFilename)) then
    Exit;

  sPluginName := ExtractOnlyFileName(sPluginFilename);
  I := tmpWDXPlugins.Add(sPluginName, GetPluginFilenameToSave(sPluginFilename), EmptyStr);

  if not tmpWDXPlugins.LoadModule(pred(tmpWDXPlugins.Count)) then
  begin
    MessageDlg(Application.Title, rsMsgInvalidPlugin, mtError, [mbOK], 0, mbOK);
    tmpWDXPlugins.DeleteItem(I);
    Exit;
  end;
  tmpWDXPlugins.GetWdxModule(pred(tmpWDXPlugins.Count)).DetectStr := tmpWDXPlugins.GetWdxModule(pred(tmpWDXPlugins.Count)).CallContentGetDetectString;

  stgPlugins.RowCount := stgPlugins.RowCount + 1;
  J := stgPlugins.RowCount - 1;
  stgPlugins.Cells[COLNO_NAME, J] := tmpWDXPlugins.GetWdxModule(I).Name;
  stgPlugins.Cells[COLNO_EXT, J] := tmpWDXPlugins.GetWdxModule(I).DetectStr;
  stgPlugins.Cells[COLNO_FILENAME, J] := tmpWDXPlugins.GetWdxModule(I).FileName;
  stgPlugins.Row := J; //This will trig automatically the "OnSelection" event.
  if gPluginInAutoTweak then
    btnTweakPlugin.Click;
end;

{ TfrmOptionsPluginsWDX.ActualDeletePlugin }
procedure TfrmOptionsPluginsWDX.ActualDeletePlugin(iIndex: integer);
begin
  tmpWDXPlugins.DeleteItem(iIndex);
end;

{ TfrmOptionsPluginsWDX.ActualPluginsMove }
procedure TfrmOptionsPluginsWDX.ActualPluginsMove(iSource, iDestination: integer);
begin
  tmpWDXPlugins.Move(iSource, iDestination);
end;

end.

