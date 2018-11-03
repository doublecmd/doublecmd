{
   Double Commander
   -------------------------------------------------------------------------
   Plugins WCX options page

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

unit fOptionsPluginsWCX;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, ComCtrls, StdCtrls, Grids, Buttons, Controls, ExtCtrls,

  //DC
  fOptionsFrame, uWCXModule, foptionspluginsbase;

type
  { TfrmOptionsPluginsWCX }
  TfrmOptionsPluginsWCX = class(TfrmOptionsPluginsBase)
    procedure stgPluginsWCXDragDrop(Sender, {%H-}Source: TObject; X, Y: integer);
    procedure btnToggleViewClick(Sender: TObject);
    procedure btnAddPluginClick(Sender: TObject);
    procedure btnEnablePluginClick(Sender: TObject);
    procedure btnRemovePluginClick(Sender: TObject);
    procedure btnTweakPluginClick(Sender: TObject);
    procedure btnConfigPluginClick(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure Done; override;
    procedure stgPluginsOnSelection(Sender: TObject; {%H-}aCol, aRow: integer); override;
  public
    class function GetTitle: string; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
    procedure ShowPluginsTable; override;
  end;

var
  tmpWCXPlugins: TWCXModuleList;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  StrUtils, LCLProc, Forms, Dialogs,

  //DC
  uDCUtils, uLng, uGlobs, uShowMsg, fTweakPlugin, dmCommonData, DCStrUtils,
  uDefaultPlugins;

const
  COLNO_ACTIVE = 0;
  COLNO_NAME = 1;
  COLNO_EXT = 2;
  COLNO_FILENAME = 3;

{ TfrmOptionsPluginsWCX }

{ TfrmOptionsPluginsWCX.Init }
procedure TfrmOptionsPluginsWCX.Init;
begin
  PluginType := ptWCX;
  inherited Init;
  tmpWCXPlugins := TWCXModuleList.Create;
  stgPlugins.OnDragDrop := @stgPluginsWCXDragDrop;
  btnToggleOptionPlugins.OnClick := @btnToggleViewClick;
  btnToggleOptionPlugins.Visible := True;
end;

{ TfrmOptionsPluginsWCX.Load }
procedure TfrmOptionsPluginsWCX.Load;
begin
  tmpWCXPlugins.Assign(gWCXPlugins);
  ShowPluginsTable;
end;

{ TfrmOptionsPluginsWCX.Save }
function TfrmOptionsPluginsWCX.Save: TOptionsEditorSaveFlags;
begin
  gWCXPlugins.Assign(tmpWCXPlugins);
  Result := [];
end;

{ TfrmOptionsPluginsWCX.Done }
procedure TfrmOptionsPluginsWCX.Done;
begin
  FreeThenNil(tmpWCXPlugins);
end;

{ TfrmOptionsPluginsWCX.GetTitle }
class function TfrmOptionsPluginsWCX.GetTitle: string;
begin
  Result := rsOptionsEditorPlugins + ' WCX';
end;

{ TfrmOptionsPluginsWCX.ExtraOptionsSignature }
function TfrmOptionsPluginsWCX.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  Result := tmpWCXPlugins.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsPluginsWCX.ShowPluginsTable }
procedure TfrmOptionsPluginsWCX.ShowPluginsTable;
var
  I, iIndex: integer;
  sFileName, sExt: string;
  iRememberOriginalRow, iRow: integer;
begin
  iRememberOriginalRow := stgPlugins.Row;

  case gWCXConfigViewMode of
    wcvmByPlugin:
    begin
      btnToggleOptionPlugins.Caption := rsOptPluginShowByExtension;
      btnToggleOptionPlugins.Glyph.Assign(ImgByExtension.Picture.Bitmap);
      stgPlugins.RowCount := stgPlugins.FixedRows;
    end;

    wcvmByExtension:
    begin
      btnToggleOptionPlugins.Caption := rsOptPluginShowByPlugin;
      btnToggleOptionPlugins.Glyph.Assign(ImgByPlugin.Picture.Bitmap);
      stgPlugins.RowCount := succ(tmpWCXPlugins.Count);
    end;
  end;

  for I := 0 to pred(tmpWCXPlugins.Count) do
  begin
    // get associated extension
    sExt := tmpWCXPlugins.Ext[I];

    //get file name
    sFileName := tmpWCXPlugins.FileName[I];

    case gWCXConfigViewMode of
      wcvmByPlugin:
      begin
        iIndex := stgPlugins.Cols[COLNO_FILENAME].IndexOf(sFileName);
        if iIndex < 0 then
        begin
          stgPlugins.RowCount := stgPlugins.RowCount + 1;
          iRow := pred(stgPlugins.RowCount);
          stgPlugins.Cells[COLNO_ACTIVE, iRow] := IfThen(tmpWCXPlugins.Enabled[I], '+', '-');
          stgPlugins.Cells[COLNO_NAME, iRow] := ExtractOnlyFileName(sFileName);
          stgPlugins.Cells[COLNO_EXT, iRow] := sExt + #32;
          stgPlugins.Cells[COLNO_FILENAME, iRow] := sFileName;
        end
        else
        begin
          stgPlugins.Cells[COLNO_EXT, iIndex] := stgPlugins.Cells[COLNO_EXT, iIndex] + sExt + #32;
          stgPlugins.Cells[COLNO_ACTIVE, iIndex] := stgPlugins.Cells[COLNO_ACTIVE, iIndex] + IfThen(tmpWCXPlugins.Enabled[I], '+', '-');
        end;
      end;

      wcvmByExtension:
      begin
        stgPlugins.Cells[COLNO_ACTIVE, succ(I)] := IfThen(tmpWCXPlugins.Enabled[I], '+', '-');
        stgPlugins.Cells[COLNO_NAME, succ(I)] := ExtractOnlyFileName(sFileName);
        stgPlugins.Cells[COLNO_EXT, succ(I)] := sExt;
        stgPlugins.Cells[COLNO_FILENAME, succ(I)] := sFileName;
      end;
    end;
  end;

  if iRememberOriginalRow <> -1 then
    stgPlugins.Row := iRememberOriginalRow;

  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsWCX.stgPluginsOnSelection }
procedure TfrmOptionsPluginsWCX.stgPluginsOnSelection(Sender: TObject; aCol, aRow: integer);
var
  bEnable: boolean = False;
  bEnabled: boolean;
begin
  if (aRow > 0) and (aRow < stgPlugins.RowCount) then
  begin
    bEnabled := (stgPlugins.Cells[COLNO_ACTIVE, aRow][1] = '-');
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

{ TfrmOptionsPluginsWCX.stgPluginsWCXDragDrop }
procedure TfrmOptionsPluginsWCX.stgPluginsWCXDragDrop(Sender, Source: TObject; X, Y: integer);
var
  iDestCol, iDestRow, iSourceRow: integer;
begin
  case gWCXConfigViewMode of
    wcvmByPlugin:
    begin
      MsgError(rsOptPluginSortOnlyWhenByExtension);
    end;

    wcvmByExtension:
    begin
      stgPlugins.MouseToCell(X, Y, {%H-}iDestCol, {%H-}iDestRow);
      if iDestRow > 0 then
      begin
        iSourceRow := stgPlugins.Row; //We need to that because after having done the following "MoveColRow", the "stgPlugins.Row" changed! So we need to remember original index.
        stgPlugins.MoveColRow(False, iSourceRow, iDestRow);
        tmpWCXPlugins.Move(pred(iSourceRow), pred(iDestRow));
      end;
    end;
  end;
end;

{ TfrmOptionsPluginsWCX.btnToggleViewClick }
procedure TfrmOptionsPluginsWCX.btnToggleViewClick(Sender: TObject);
begin
  case gWCXConfigViewMode of
    wcvmByPlugin: gWCXConfigViewMode := wcvmByExtension;
    wcvmByExtension: gWCXConfigViewMode := wcvmByPlugin;
  end;
  ShowPluginsTable;
end;

{ TfrmOptionsPluginsWCX.btnAddPluginClick }
procedure TfrmOptionsPluginsWCX.btnAddPluginClick(Sender: TObject);
var
  J, iPluginIndex, iFlags, iNbItemOnStart: integer;
  sExt: string;
  sExts: string;
  sExtsTemp: string;
  sFileName: string;
  sPluginName: string;
  sAlreadyAssignedExts: string;
  WCXmodule: TWCXmodule;
begin
  iNbItemOnStart := tmpWCXPlugins.Count;
  dmComData.OpenDialog.Filter := Format('Archive plugins (%s)|%s', [WcxMask, WcxMask]);

  if dmComData.OpenDialog.Execute then
  begin
    sFileName := dmComData.OpenDialog.FileName;
    if not CheckPlugin(sFileName) then
      Exit;
    sFileName := GetPluginFilenameToSave(sFileName);
    WCXmodule := gWCXPlugins.LoadModule(sFileName);

    if not Assigned(WCXmodule) then
    begin
      MessageDlg(Application.Title, rsMsgInvalidPlugin, mtError, [mbOK], 0, mbOK);
      Exit;
    end;

    iFlags := WCXmodule.GetPluginCapabilities;

    sPluginName := sFileName;
    sExts := '';
    if InputQuery(rsOptEnterExt, Format(rsOptAssocPluginWith, [sFileName]), sExts) then
    begin
      sExtsTemp := sExts;
      sExts := '';
      sAlreadyAssignedExts := '';
      sExt := Copy2SpaceDel(sExtsTemp);
      repeat
        iPluginIndex := tmpWCXPlugins.Find(sPluginName, sExt);
        if iPluginIndex <> -1 then
        begin
          AddStrWithSep(sAlreadyAssignedExts, sExt);
        end
        else
        begin
          tmpWCXPlugins.AddObject(sExt + '=' + IntToStr(iFlags) + ',' + sPluginName, TObject(True));
          AddStrWithSep(sExts, sExt);
        end;
        sExt := Copy2SpaceDel(sExtsTemp);
      until sExt = '';

      if sAlreadyAssignedExts <> '' then
        MessageDlg(Format(rsOptPluginAlreadyAssigned, [sFileName]) + LineEnding + sAlreadyAssignedExts, mtWarning, [mbOK], 0);

      if iNbItemOnStart <> tmpWCXPlugins.Count then
      begin
        stgPlugins.RowCount := stgPlugins.RowCount + 1; // Add new row
        J := pred(stgPlugins.RowCount);
        stgPlugins.Cells[COLNO_ACTIVE, J] := '+'; // Enabled
        stgPlugins.Cells[COLNO_NAME, J] := ExtractOnlyFileName(sFileName);
        stgPlugins.Cells[COLNO_EXT, J] := sExts;
        stgPlugins.Cells[COLNO_FILENAME, J] := sPluginName;
        stgPlugins.Row := J; //This will trig automatically the "OnSelection" event.
        if gPluginInAutoTweak then
          btnTweakPlugin.Click;
      end;
    end;
  end;
end;

{ TfrmOptionsPluginsWCX.btnEnablePluginClick }
procedure TfrmOptionsPluginsWCX.btnEnablePluginClick(Sender: TObject);
var
  sExt, sExts, sFinalSigns: string;
  iPluginIndex: integer;
  bEnabled: boolean;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then
    Exit;

  case gWCXConfigViewMode of
    wcvmByExtension:
    begin
      tmpWCXPlugins.Enabled[pred(stgPlugins.Row)] := not tmpWCXPlugins.Enabled[pred(stgPlugins.Row)];
      stgPlugins.Cells[COLNO_ACTIVE, stgPlugins.Row] := IfThen(tmpWCXPlugins.Enabled[pred(stgPlugins.Row)], '+', '-');
    end;

    wcvmByPlugin:
    begin
      bEnabled := (stgPlugins.Cells[COLNO_ACTIVE, stgPlugins.Row][1] = '-');
      sExts := stgPlugins.Cells[COLNO_EXT, stgPlugins.Row];
      sExt := Copy2SpaceDel(sExts);
      sFinalSigns := '';
      repeat
        iPluginIndex := tmpWCXPlugins.Find(stgPlugins.Cells[COLNO_FILENAME, stgPlugins.Row], sExt);
        if iPluginIndex <> -1 then
          tmpWCXPlugins.Enabled[iPluginIndex] := bEnabled;
        sExt := Copy2SpaceDel(sExts);
        sFinalSigns := sFinalSigns + IfThen(bEnabled, '+', '-');
      until sExt = '';
      stgPlugins.Cells[COLNO_ACTIVE, stgPlugins.Row] := sFinalSigns;
    end;
  end;

  stgPluginsOnSelection(stgPlugins, 0, stgPlugins.Row);
end;

{ TfrmOptionsPluginsWCX }
procedure TfrmOptionsPluginsWCX.btnRemovePluginClick(Sender: TObject);
var
  sExt, sExts: string;
  iPluginIndex: integer;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then
    Exit;

  case gWCXConfigViewMode of
    wcvmByPlugin:
    begin
      sExts := stgPlugins.Cells[COLNO_EXT, stgPlugins.Row];
      sExt := Copy2SpaceDel(sExts);
      repeat
        iPluginIndex := tmpWCXPlugins.Find(stgPlugins.Cells[COLNO_FILENAME, stgPlugins.Row], sExt);
        if iPluginIndex <> -1 then
          tmpWCXPlugins.Delete(iPluginIndex);
        sExt := Copy2SpaceDel(sExts);
      until sExt = '';
    end;

    wcvmByExtension:
    begin
      tmpWCXPlugins.Delete(pred(stgPlugins.Row));
    end;
  end;

  ShowPluginsTable;
end;

{ TfrmOptionsPluginsWCX.btnTweakPluginClick }
procedure TfrmOptionsPluginsWCX.btnTweakPluginClick(Sender: TObject);
var
  iPluginIndex: integer;
begin
  iPluginIndex := tmpWCXPlugins.Find(stgPlugins.Cells[COLNO_FILENAME, stgPlugins.Row], Copy2Space(stgPlugins.Cells[COLNO_EXT, stgPlugins.Row]));

  if iPluginIndex < 0 then
    Exit;

  if ShowTweakPluginDlg(PluginType, iPluginIndex) then
    ShowPluginsTable;
end;

{ TfrmOptionsPluginsWCX.btnConfigPluginClick }
procedure TfrmOptionsPluginsWCX.btnConfigPluginClick(Sender: TObject);
var
  WCXmodule: TWCXmodule;
  PluginFileName: string;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then
    Exit; // no plugins

  PluginFileName := stgPlugins.Cells[COLNO_FILENAME, stgPlugins.Row];
  WCXmodule := gWCXPlugins.LoadModule(PluginFileName);

  if Assigned(WCXmodule) then
  begin
    WCXmodule.VFSConfigure(stgPlugins.Handle);
  end
  else
  begin
    msgError(rsMsgErrEOpen + ': ' + PluginFileName);
  end;
end;

end.

