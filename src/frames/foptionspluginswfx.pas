{
   Double Commander
   -------------------------------------------------------------------------
   Plugins WFX options page

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

unit fOptionsPluginsWFX;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, ComCtrls, StdCtrls, Grids, Buttons, Controls, ExtCtrls,

  //DC
  uDCUtils, fOptionsFrame, uWFXModule, foptionspluginsbase;

type
  { TfrmOptionsPluginsWFX }
  TfrmOptionsPluginsWFX = class(TfrmOptionsPluginsBase)
    procedure btnAddPluginClick(Sender: TObject);
    procedure btnEnablePluginClick(Sender: TObject);
    procedure btnConfigPluginClick(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure Done; override;
    procedure stgPluginsOnSelection(Sender: TObject; {%H-}aCol, aRow: integer); override;
    procedure ActualDeletePlugin(iIndex: integer); override;
    procedure ActualPluginsMove(iSource, iDestination: integer); override;
  public
    class function GetTitle: string; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
    procedure ShowPluginsTable; override;
  end;

var
  tmpWFXPlugins: TWFXModuleList;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  StrUtils, LCLProc, Forms, Dialogs,

  //DC
  uLng, uGlobs, uShowMsg, dmCommonData, DCStrUtils, uDefaultPlugins;

const
  COLNO_ACTIVE = 0;
  COLNO_NAME = 1;
  COLNO_FILENAME = 2;

{ TfrmOptionsPluginsWFX }

{ TfrmOptionsPluginsWFX.Init }
procedure TfrmOptionsPluginsWFX.Init;
begin
  PluginType := ptWFX;
  inherited Init;
  stgPlugins.Columns.Items[COLNO_FILENAME].Title.Caption := rsOptPluginsFileName;
  stgPlugins.Columns.Delete(succ(COLNO_FILENAME));
  tmpWFXPlugins := TWFXModuleList.Create;
end;

{ TfrmOptionsPluginsWFX.Load }
procedure TfrmOptionsPluginsWFX.Load;
begin
  tmpWFXPlugins.Assign(gWFXPlugins);
  ShowPluginsTable;
end;

{ TfrmOptionsPluginsWFX.Save }
function TfrmOptionsPluginsWFX.Save: TOptionsEditorSaveFlags;
begin
  gWFXPlugins.Assign(tmpWFXPlugins);
  Result := [];
end;

{ TfrmOptionsPluginsWFX.Done }
procedure TfrmOptionsPluginsWFX.Done;
begin
  FreeThenNil(tmpWFXPlugins);
end;

{ TfrmOptionsPluginsWFX.GetTitle }
class function TfrmOptionsPluginsWFX.GetTitle: string;
begin
  Result := rsOptionsEditorPlugins + ' WFX';
end;

{ TfrmOptionsPluginsWFX.ExtraOptionsSignature }
function TfrmOptionsPluginsWFX.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  Result := tmpWFXPlugins.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsPluginsWFX.ShowPluginsTable }
procedure TfrmOptionsPluginsWFX.ShowPluginsTable;
var
  I, iRow: integer;
begin
  stgPlugins.RowCount := tmpWFXPlugins.Count + stgPlugins.FixedRows;
  for I := 0 to pred(tmpWFXPlugins.Count) do
  begin
    iRow := I + stgPlugins.FixedRows;
    stgPlugins.Cells[COLNO_ACTIVE, iRow] := IfThen(tmpWFXPlugins.Enabled[I], '+', '-');
    stgPlugins.Cells[COLNO_NAME, iRow] := tmpWFXPlugins.Name[I];
    stgPlugins.Cells[COLNO_FILENAME, iRow] := tmpWFXPlugins.FileName[I];
  end;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsWFX.stgPluginsOnSelection }
procedure TfrmOptionsPluginsWFX.stgPluginsOnSelection(Sender: TObject; aCol, aRow: integer);
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
procedure TfrmOptionsPluginsWFX.btnAddPluginClick(Sender: TObject);
var
  I, J: integer;
  WfxModule: TWFXmodule;
  sFileName, sPluginName, sRootName: string;
begin
  dmComData.OpenDialog.Filter := Format('File system plugins (%s)|%s', [WfxMask, WfxMask]);
  if dmComData.OpenDialog.Execute then
  begin
    sFileName := dmComData.OpenDialog.FileName;
    if not CheckPlugin(sFileName) then
      Exit;
    sFileName := GetPluginFilenameToSave(sFileName);

    WfxModule := gWFXPlugins.LoadModule(sFileName);
    try
      if not Assigned(WfxModule) then
      begin
        MessageDlg(Application.Title, rsMsgInvalidPlugin, mtError, [mbOK], 0, mbOK);
        Exit;
      end;

      sRootName := WfxModule.VFSRootName;
      if Length(sRootName) = 0 then
      begin
        sRootName := ExtractOnlyFileName(sFileName);
      end;
      sPluginName := sRootName + '=' + sFileName;

      I := tmpWFXPlugins.AddObject(sPluginName, TObject(True));
      stgPlugins.RowCount := tmpWFXPlugins.Count + 1;
      J := stgPlugins.RowCount - 1;
      stgPlugins.Cells[COLNO_ACTIVE, J] := '+';
      stgPlugins.Cells[COLNO_NAME, J] := tmpWFXPlugins.Name[I];
      stgPlugins.Cells[COLNO_FILENAME, J] := tmpWFXPlugins.FileName[I];
      stgPlugins.Row := J; //This will trig automatically the "OnSelection" event.
      if gPluginInAutoTweak then
        btnTweakPlugin.Click;
    finally
    end;
  end;
end;

{ TfrmOptionsPluginsDSX.ActualDeletePlugin }
procedure TfrmOptionsPluginsWFX.ActualDeletePlugin(iIndex: integer);
begin
  tmpWFXPlugins.Delete(iIndex);
end;

{ TfrmOptionsPluginsWFX.ActualPluginsMove }
procedure TfrmOptionsPluginsWFX.ActualPluginsMove(iSource, iDestination: integer);
begin
  tmpWFXPlugins.Move(iSource, iDestination);
end;

{ TfrmOptionsPluginsWFX.btnEnablePluginClick }
procedure TfrmOptionsPluginsWFX.btnEnablePluginClick(Sender: TObject);
var
  bEnabled: boolean;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then
    Exit;
  bEnabled := not tmpWFXPlugins.Enabled[stgPlugins.Row - stgPlugins.FixedRows];
  stgPlugins.Cells[COLNO_ACTIVE, stgPlugins.Row] := IfThen(bEnabled, '+', '-');
  tmpWFXPlugins.Enabled[stgPlugins.Row - stgPlugins.FixedRows] := bEnabled;
  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsWFX.btnConfigPluginClick }
procedure TfrmOptionsPluginsWFX.btnConfigPluginClick(Sender: TObject);
var
  WFXmodule: TWFXmodule;
  PluginFileName: string;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then
    Exit; // no plugins
  PluginFileName := stgPlugins.Cells[COLNO_FILENAME, stgPlugins.Row];
  WFXmodule := gWFXPlugins.LoadModule(PluginFileName);
  if Assigned(WFXmodule) then
  begin
    WfxModule.VFSInit;
    WFXmodule.VFSConfigure(stgPlugins.Handle);
  end
  else
  begin
    msgError(rsMsgErrEOpen + ': ' + PluginFileName);
  end;
end;

end.

