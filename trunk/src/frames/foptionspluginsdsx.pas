{
   Double Commander
   -------------------------------------------------------------------------
   Plugins DSX options page

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

unit fOptionsPluginsDSX;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, ComCtrls, StdCtrls, Grids, Buttons, Controls, ExtCtrls,

  //DC
  fOptionsFrame, uDSXModule, foptionspluginsbase;

type
  { TfrmOptionsPluginsDSX }
  TfrmOptionsPluginsDSX = class(TfrmOptionsPluginsBase)
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
  tmpDSXPlugins: TDSXModuleList;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LCLProc, Forms, Dialogs,

  //DC
  uLng, uGlobs, dmCommonData, DCStrUtils, uDefaultPlugins;

const
  COLNO_NAME = 0;
  COLNO_DESCRIPTION = 1;
  COLNO_FILENAME = 2;

{ TfrmOptionsPluginsDSX }

{ TfrmOptionsPluginsDSX.Init }
procedure TfrmOptionsPluginsDSX.Init;
begin
  PluginType := ptDSX;
  inherited Init;
  stgPlugins.Columns.Items[COLNO_NAME].Title.Caption := rsOptPluginsName;
  stgPlugins.Columns.Items[COLNO_NAME].Alignment := taLeftJustify; //Because from the "Base", it was centered.
  stgPlugins.Columns.Items[COLNO_DESCRIPTION].Title.Caption := rsOptPluginsDescription;
  stgPlugins.Columns.Items[COLNO_FILENAME].Title.Caption := rsOptPluginsFileName;
  stgPlugins.Columns.Delete(succ(COLNO_FILENAME)); //Because from the "Base" it has one column more than required.
  btnEnablePlugin.Visible := False; //Because with DSX there is no enable/disable.
  btnConfigPlugin.Visible := False;
  tmpDSXPlugins := TDSXModuleList.Create;
end;

{ TfrmOptionsPluginsDSX.Load }
procedure TfrmOptionsPluginsDSX.Load;
begin
  tmpDSXPlugins.Assign(gDSXPlugins);
  ShowPluginsTable;
end;

{ TfrmOptionsPluginsDSX.Save }
function TfrmOptionsPluginsDSX.Save: TOptionsEditorSaveFlags;
begin
  gDSXPlugins.Assign(tmpDSXPlugins);
  Result := [];
end;

{ TfrmOptionsPluginsDSX.Done }
procedure TfrmOptionsPluginsDSX.Done;
begin
  FreeThenNil(tmpDSXPlugins);
end;

{ TfrmOptionsPluginsDSX.GetTitle }
class function TfrmOptionsPluginsDSX.GetTitle: string;
begin
  Result := rsOptionsEditorPlugins + ' DSX';
end;

{ TfrmOptionsPluginsDSX.ExtraOptionsSignature }
function TfrmOptionsPluginsDSX.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  Result := tmpDSXPlugins.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsPluginsDSX.ShowPluginsTable }
procedure TfrmOptionsPluginsDSX.ShowPluginsTable;
var
  I: integer;
begin
  stgPlugins.RowCount := tmpDSXPlugins.Count + stgPlugins.FixedRows;
  for i := 0 to pred(tmpDSXPlugins.Count) do
  begin
    stgPlugins.Cells[COLNO_NAME, I + stgPlugins.FixedRows] := tmpDSXPlugins.GetDsxModule(i).Name;
    stgPlugins.Cells[COLNO_DESCRIPTION, I + stgPlugins.FixedRows] := tmpDSXPlugins.GetDsxModule(i).Descr;
    stgPlugins.Cells[COLNO_FILENAME, I + stgPlugins.FixedRows] := tmpDSXPlugins.GetDsxModule(i).FileName;
  end;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsDSX.stgPluginsOnSelection }
procedure TfrmOptionsPluginsDSX.stgPluginsOnSelection(Sender: TObject; aCol, aRow: integer);
var
  bEnable: boolean = False;
begin
  if (aRow > 0) and (aRow < stgPlugins.RowCount) then
    bEnable := True;
  btnRemovePlugin.Enabled := bEnable;
  btnTweakPlugin.Enabled := bEnable;
  btnConfigPlugin.Enabled := bEnable;
end;

{ TfrmOptionsPluginsDSX.ActualDeletePlugin }
procedure TfrmOptionsPluginsDSX.ActualDeletePlugin(iIndex: integer);
begin
  tmpDSXPlugins.DeleteItem(iIndex);
end;

{ TfrmOptionsPluginsDSX.ActualPluginsMove }
procedure TfrmOptionsPluginsDSX.ActualPluginsMove(iSource, iDestination: integer);
begin
  tmpDSXPlugins.Move(iSource, iDestination);
end;

{ TfrmOptionsPluginsDSX.btnAddPluginClick }
procedure TfrmOptionsPluginsDSX.btnAddPluginClick(Sender: TObject);
begin
  dmComData.OpenDialog.Filter := 'Search plugins (*.dsx)|*.dsx';
  if dmComData.OpenDialog.Execute then
    ActualAddPlugin(dmComData.OpenDialog.FileName);
end;

{ TfrmOptionsPluginsDSX.ActualAddPlugin }
procedure TfrmOptionsPluginsDSX.ActualAddPlugin(sPluginFilename: string);
var
  I, J: integer;
  sPluginName: string;
begin
  if not CheckPlugin(sPluginFilename) then
    Exit;

  sPluginName := ExtractOnlyFileName(sPluginFilename);
  I := tmpDSXPlugins.Add(sPluginName, GetPluginFilenameToSave(sPluginFilename), EmptyStr);

  if not tmpDSXPlugins.LoadModule(sPluginName) then
  begin
    MessageDlg(Application.Title, rsMsgInvalidPlugin, mtError, [mbOK], 0, mbOK);
    tmpDSXPlugins.DeleteItem(I);
    Exit;
  end;

  stgPlugins.RowCount := stgPlugins.RowCount + 1;
  J := stgPlugins.RowCount - stgPlugins.FixedRows;
  stgPlugins.Cells[COLNO_NAME, J] := tmpDSXPlugins.GetDsxModule(I).Name;
  stgPlugins.Cells[COLNO_DESCRIPTION, J] := tmpDSXPlugins.GetDsxModule(I).Descr;
  stgPlugins.Cells[COLNO_FILENAME, J] := tmpDSXPlugins.GetDsxModule(I).FileName;
  stgPlugins.Row := J; //This will trig automatically the "OnSelection" event.
  if gPluginInAutoTweak then
    btnTweakPlugin.click;
end;

end.

