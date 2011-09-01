{
   Double Commander
   -------------------------------------------------------------------------
   Implementing of Options dialog

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

   contributors:

   Radek Cervinka  <radek.cervinka@centrum.cz>

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

unit fOptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls, ComCtrls, Buttons,
  uGlobs, fOptionsFrame;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    pnlCaption: TPanel;
    btnOK: TBitBtn;
    btnApply: TBitBtn;
    btnCancel: TBitBtn;
    ilTreeView: TImageList;
    tvTreeView: TTreeView;
    nbNotebook: TNotebook;
    splOptionsSplitter: TSplitter;
    pgToolTips: TPage;
    pgArchivers: TPage;
    pgIgnoreList: TPage;
    pgIcons: TPage;
    pgAutoRefresh: TPage;
    pgMisc: TPage;
    pgColumns: TPage;
    pgQuickSearch: TPage;
    pgConfigStorage: TPage;
    pgLogFile: TPage;
    pgTabs: TPage;
    pgFileOp: TPage;
    pgLayout: TPage;
    pgPlugins: TPage;
    pgBehav: TPage;
    pgColor: TPage;
    pgFonts: TPage;
    pgHotKey: TPage;
    pgLng: TPage;
    pgTools: TPage;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure nbNotebookPageChanged(Sender: TObject);
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FOptionsEditorList: TOptionsEditorList;
    procedure CreateOptionsEditorList;
  public
    procedure LoadConfig;
    procedure SaveConfig;
  end;

implementation

{$R *.lfm}

uses
  LCLProc, LCLVersion, uLng, fMain,
  fOptionsPlugins, fOptionsToolTips, fOptionsColors, fOptionsLanguage,
  fOptionsBehaviour, fOptionsTools, fOptionsHotkeys, fOptionsLayout,
  fOptionsFonts, fOptionsFileOperations, fOptionsQuickSearchFilter,
  fOptionsTabs, fOptionsLog, fOptionsConfiguration, fOptionsColumns,
  fOptionsMisc, fOptionsAutoRefresh, fOptionsIcons, fOptionsIgnoreList,
  fOptionsArchivers;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  // tvTreeView localization
  with tvTreeView.Items do
    begin
      Item[0].Text := rsOptLanguage;
      Item[1].Text := rsOptBehav;
      Item[2].Text := rsOptTools;
      Item[3].Text := rsOptFonts;
      Item[4].Text := rsOptColors;
      Item[5].Text := rsOptHotKeys;
      Item[6].Text := rsOptPlugins;
      Item[7].Text := rsOptLayout;
      Item[8].Text := rsOptFileOp;
      Item[9].Text := rsOptFolderTabs;
      Item[10].Text := rsOptLog;
      Item[11].Text := rsOptConfig;
      Item[12].Text := rsOptQuickSearch;
      Item[13].Text := rsOptColumns;
      Item[14].Text := rsOptMiscellaneous;
      Item[15].Text := rsOptAutoRefresh;
      Item[16].Text := rsOptIcons;
      Item[17].Text := rsOptIgnoreList;
      Item[18].Text := rsOptArchivers;
      Item[19].Text := rsOptTooltips;
    end;
  tvTreeView.Items.Item[0].Selected:= True;

  // Create and fill options editor list
  CreateOptionsEditorList;

  // Load all configuration
  LoadConfig;

  // Initialize property storage
  InitPropStorage(Self);
  // Let not warning on which page save form
  nbNotebook.PageIndex := 0;

  // Below needed until after we switch to Lazarus 0.9.31.
  nbNotebook.TabStop := True;
  {$if (lcl_release) < 31}
  nbNotebook.ShowTabs := False;
  nbNotebook.OnPageChanged := @nbNotebookPageChanged;
  {$endif}
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  // save all configuration
  SaveConfig;
  // write to config file
  SaveGlobs;
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  // save all configuration
  SaveConfig;
  // write to config file
  SaveGlobs;
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  FreeThenNil(FOptionsEditorList);
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
//Load specified page or 0
  tvTreeView.Items.Item[Self.Tag].Selected:=true;
  nbNotebook.PageIndex := Self.Tag;

end;

procedure TfrmOptions.CreateOptionsEditorList;
var
  I: LongInt;
  aOptionsEditor: TOptionsEditor;
begin
  FOptionsEditorList:= TOptionsEditorList.Create;
  for I:= 0 to OptionsEditorClassList.Count - 1 do
  begin
    aOptionsEditor:= OptionsEditorClassList[I].OptionsEditorClass.Create(Self);
    aOptionsEditor.Align := alClient;
    aOptionsEditor.Parent:= nbNotebook.Page[Integer(OptionsEditorClassList[I].OptionsEditorType)];
    FOptionsEditorList.Add(aOptionsEditor);
  end;
end;

procedure TfrmOptions.nbNotebookPageChanged(Sender: TObject);
begin 
  // temporally this is hack for bug http://www.freepascal.org/mantis/view.php?id=9635
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
end;

procedure TfrmOptions.tvTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  //DebugLN('Page index == ' + IntToStr(Node.Index));
  nbNotebook.PageIndex := tvTreeView.Selected.ImageIndex; // temporally image index
  pnlCaption.Caption := tvTreeView.Selected.Text;
end;

procedure TfrmOptions.LoadConfig;
var
  I: LongInt;
begin
  { Load options to frames }
  for I:= 0 to FOptionsEditorList.Count - 1 do
    FOptionsEditorList[I].Load;
end;

procedure TfrmOptions.SaveConfig;
var
  I: LongInt;
  NeedsRestart: Boolean = False;
begin
  { Save options from frames }
  for I:= 0 to FOptionsEditorList.Count - 1 do
    if oesfNeedsRestart in FOptionsEditorList[I].Save then
      NeedsRestart := True;

  if NeedsRestart then
    MessageDlg(rsMsgRestartForApplyChanges, mtInformation, [mbOK], 0);

  frmMain.UpdateWindowView;
end;

end.
