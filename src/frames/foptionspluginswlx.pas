{
   Double Commander
   -------------------------------------------------------------------------
   Plugins WLX options page

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

unit fOptionsPluginsWLX;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, ComCtrls, StdCtrls, Grids, Buttons, Controls, ExtCtrls,

  //DC
  fOptionsFrame, uWLXModule, foptionspluginsbase;

type
  { TfrmOptionsPluginsWLX }
  TfrmOptionsPluginsWLX = class(TfrmOptionsPluginsBase)
    procedure btnAddPluginClick(Sender: TObject);
    procedure btnEnablePluginClick(Sender: TObject);
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
  tmpWLXPlugins: TWLXModuleList;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  StrUtils, LCLProc, Forms, Dialogs, DynLibs,

  //DC
  uLng, uGlobs, dmCommonData, DCStrUtils, DCConvertEncoding, uDefaultPlugins;

const
  COLNO_ACTIVE = 0;
  COLNO_NAME = 1;
  COLNO_EXT = 2;
  COLNO_FILENAME = 3;

{ TfrmOptionsPluginsWLX }

{ TfrmOptionsPluginsWLX.Init }
procedure TfrmOptionsPluginsWLX.Init;
begin
  PluginType := ptWLX;
  inherited Init;
  btnConfigPlugin.Visible := False;
  tmpWLXPlugins := TWLXModuleList.Create;
end;

{ TfrmOptionsPluginsWLX.Load }
procedure TfrmOptionsPluginsWLX.Load;
begin
  tmpWLXPlugins.Assign(gWLXPlugins);
  ShowPluginsTable;
end;

{ TfrmOptionsPluginsWLX.Save }
function TfrmOptionsPluginsWLX.Save: TOptionsEditorSaveFlags;
begin
  gWLXPlugins.Assign(tmpWLXPlugins);
  Result := [];
end;

{ TfrmOptionsPluginsWLX.Done }
procedure TfrmOptionsPluginsWLX.Done;
begin
  FreeThenNil(tmpWLXPlugins);
end;

{ TfrmOptionsPluginsWLX.GetTitle }
class function TfrmOptionsPluginsWLX.GetTitle: string;
begin
  Result := rsOptionsEditorPlugins + ' WLX';
end;

{ TfrmOptionsPluginsWLX.ExtraOptionsSignature }
function TfrmOptionsPluginsWLX.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  Result := tmpWLXPlugins.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsPluginsWLX.ShowPluginsTable }
procedure TfrmOptionsPluginsWLX.ShowPluginsTable;
var
  I: integer;
begin
  stgPlugins.RowCount := tmpWLXPlugins.Count + stgPlugins.FixedRows;
  for i := 0 to pred(tmpWLXPlugins.Count) do
  begin
    stgPlugins.Cells[COLNO_ACTIVE, I + stgPlugins.FixedRows] := IfThen(tmpWLXPlugins.GetWlxModule(i).Enabled, '+', '-');
    stgPlugins.Cells[COLNO_NAME, I + stgPlugins.FixedRows] := tmpWLXPlugins.GetWlxModule(i).Name;
    stgPlugins.Cells[COLNO_EXT, I + stgPlugins.FixedRows] := tmpWLXPlugins.GetWlxModule(i).DetectStr;
    stgPlugins.Cells[COLNO_FILENAME, I + stgPlugins.FixedRows] := tmpWLXPlugins.GetWlxModule(i).FileName;
  end;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsWLX.stgPluginsOnSelection }
procedure TfrmOptionsPluginsWLX.stgPluginsOnSelection(Sender: TObject; aCol, aRow: integer);
var
  bEnable: boolean = False;
  bEnabled: boolean;
begin
  if (aRow > 0) and (aRow < stgPlugins.RowCount) then
  begin
    bEnabled := (stgPlugins.Cells[COLNO_ACTIVE, aRow] = '-');
    btnEnablePlugin.Caption := IfThen(bEnabled, rsOptPluginEnable, rsOptPluginDisable);
    if bEnabled then
      btnEnablePlugin.Glyph.Assign(ImgSwitchDisable.Picture.Bitmap)
    else
      btnEnablePlugin.Glyph.Assign(ImgSwitchEnable.Picture.Bitmap);
    bEnable := True;
  end;

  btnEnablePlugin.Enabled := bEnable;
  btnRemovePlugin.Enabled := bEnable;
  btnTweakPlugin.Enabled := bEnable;
  btnConfigPlugin.Enabled := bEnable;
end;

{ TfrmOptionsPluginsWFX.btnAddPluginClick }
procedure TfrmOptionsPluginsWLX.btnAddPluginClick(Sender: TObject);
begin
  dmComData.OpenDialog.Filter := Format('Viewer plugins (%s)|%s', [WlxMask, WlxMask]);
  if dmComData.OpenDialog.Execute then
    ActualAddPlugin(dmComData.OpenDialog.FileName);
end;

{ TfrmOptionsPluginsWLX.ActualAddPlugin }
procedure TfrmOptionsPluginsWLX.ActualAddPlugin(sPluginFilename: string);
const
  cNextLine = LineEnding + LineEnding;
var
  I, J: integer;
  sPluginName: string;
begin
  if not CheckPlugin(sPluginFilename) then
    Exit;

  sPluginName := ExtractOnlyFileName(sPluginFilename);
  I := tmpWLXPlugins.Add(sPluginName, GetPluginFilenameToSave(sPluginFilename), EmptyStr);

  if not tmpWLXPlugins.LoadModule(pred(tmpWLXPlugins.Count)) then
  begin
    MessageDlg(Application.Title, rsMsgInvalidPlugin + cNextLine + CeSysToUtf8(GetLoadErrorStr), mtError, [mbOK], 0, mbOK);
    tmpWLXPlugins.DeleteItem(I);
    Exit;
  end;
  tmpWLXPlugins.GetWlxModule(pred(tmpWLXPlugins.Count)).DetectStr := tmpWLXPlugins.GetWlxModule(pred(tmpWLXPlugins.Count)).CallListGetDetectString;

  stgPlugins.RowCount := stgPlugins.RowCount + 1;
  J := pred(stgPlugins.RowCount);
  stgPlugins.Cells[COLNO_ACTIVE, J] := '+';
  stgPlugins.Cells[COLNO_NAME, J] := tmpWLXPlugins.GetWlxModule(I).Name;
  stgPlugins.Cells[COLNO_EXT, J] := tmpWLXPlugins.GetWlxModule(I).DetectStr;
  stgPlugins.Cells[COLNO_FILENAME, J] := tmpWLXPlugins.GetWlxModule(I).FileName;
  stgPlugins.Row := J; //This will trig automatically the "OnSelection" event.
  if gPluginInAutoTweak then
    btnTweakPlugin.click;
end;

{ TfrmOptionsPluginsWLX.ActualDeletePlugin }
procedure TfrmOptionsPluginsWLX.ActualDeletePlugin(iIndex: integer);
begin
  tmpWLXPlugins.DeleteItem(iIndex);
end;

{ TfrmOptionsPluginsWLX.ActualPluginsMove }
procedure TfrmOptionsPluginsWLX.ActualPluginsMove(iSource, iDestination: integer);
begin
  tmpWLXPlugins.Move(iSource, iDestination);
end;

{ TfrmOptionsPluginsWLX.btnEnablePluginClick }
procedure TfrmOptionsPluginsWLX.btnEnablePluginClick(Sender: TObject);
begin
  if stgPlugins.Row < stgPlugins.FixedRows then
    Exit;
  with tmpWLXPlugins.GetWlxModule(stgPlugins.Row - stgPlugins.FixedRows) do
  begin
    Enabled := not Enabled;
    stgPlugins.Cells[COLNO_ACTIVE, stgPlugins.Row] := IfThen(Enabled, '+', '-');
    btnEnablePlugin.Caption := IfThen(Enabled, rsOptPluginDisable, rsOptPluginEnable);
  end;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

end.

