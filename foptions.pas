{
   Double Commander
   -------------------------------------------------------------------------
   Implementing of Options dialog

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

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
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Spin, ColorBox,
  CheckLst, EditBtn,uColumns;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    bbtnApply: TBitBtn;
    bbtnHelp: TBitBtn;
    bbtnAddCategory: TBitBtn;
    bbtnDeleteCategory: TBitBtn;
    bbtnApplyCategory: TBitBtn;
    bbtnWFXAdd: TBitBtn;
    bbtnWDXAdd: TBitBtn;
    bbtnWDXApply: TBitBtn;
    bbtnWFXDelete: TBitBtn;
    bbtnWDXDelete: TBitBtn;
    bbtnWFXRename: TBitBtn;
    bbtnWFXApply: TBitBtn;
    bbtnWDXRename: TBitBtn;
    btnCategoryColor: TButton;
    btnOpen: TBitBtn;
    btnSelEditFnt: TButton;
    btnSelMainFnt: TButton;
    btnSelViewFnt: TButton;
    btSetHotKey: TButton;
    btnForeColor: TButton;
    btnBackColor: TButton;
    btClearHotKey: TButton;
    btnBackColor2: TButton;
    btnMarkColor: TButton;
    btnCursorColor: TButton;
    btnCursorText: TButton;
    btnNewColumnsSet: TButton;
    btnEditColumnsSet: TButton;
    btnDelColumnsSet: TButton;
    btnCopyColumnsSet: TButton;
    cbBackColor: TColorBox;
    cBackGrndLabel: TLabel;
    cbActions: TComboBox;
    cbCaseSensitiveSort: TCheckBox;
    cbDirSelect: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbEditorFont: TComboBox;
    cbExtDiffer: TCheckBox;
    cbExtEditor: TCheckBox;
    cbExtViewer: TCheckBox;
    cbIconsSize: TComboBox;
    cbLynxLike: TCheckBox;
    cbMainFont: TComboBox;
    cbShortFileSizeFormat: TCheckBox;
    cbViewerFont: TComboBox;
    cbExt: TComboBox;
    cbWCXPath: TComboBox;
    cbFlatInterface: TCheckBox;
    cbFlatToolBar: TCheckBox;
    cbShowIcons: TCheckBox;
    cbTabsAlwaysVisible: TCheckBox;
    cbTabsMultiLines: TCheckBox;
    cbTabsLimitOption: TCheckBox;
    cbLogWindow: TCheckBox;
    cbDirHistory: TCheckBox;
    cbCmdLineHistory: TCheckBox;
    cbFileMaskHistory: TCheckBox;
    cbSelectionByMouse: TCheckBox;
    clbWFXList: TCheckListBox;
    clbWCXList: TCheckListBox;
    cbBackColor2: TColorBox;
    cbMarkColor: TColorBox;
    cbCursorColor: TColorBox;
    cbCursorText: TColorBox;
    cbTextColor: TColorBox;
    cbCategoryColor: TColorBox;
    cbDateTimeFormat: TComboBox;
    clbWDXList: TCheckListBox;
    cbbFileSystem: TComboBox;
    cbMouseMode: TComboBox;
    cTextLabel: TLabel;
    dlgFnt: TFontDialog;
    edHotKey: TEdit;
    edtCategoryAttr: TEdit;
    edtTabsLimitLength: TEdit;
    edtCopyBufferSize: TEdit;
    edtCategoryName: TEdit;
    edtCategoryMask: TEdit;
    edtEditorSize: TSpinEdit;
    edtMainSize: TSpinEdit;
    edtRunTerm: TEdit;
    edtTerm: TEdit;
    edtTest1: TEdit;
    edtTest2: TEdit;
    edtTest3: TEdit;
    edtViewerSize: TSpinEdit;
    cbLogFile: TCheckBox;
    gbExactNameMatch: TGroupBox;
    fneLogFileName: TFileNameEdit;
    gbLogFile: TGroupBox;
    gbLogFileOp: TGroupBox;
    gbLogFileStatus: TGroupBox;
    fneExtViewer: TFileNameEdit;
    fneExtDiffer: TFileNameEdit;
    fneExtEditor: TFileNameEdit;
    gb: TGroupBox;
    gbExample: TGroupBox;
    gbFileTypesColors: TGroupBox;
    gbMisc: TGroupBox;
    gbScreenLayout: TGroupBox;
    cbFlatDiskPanel: TCheckBox;
    cbShowMainToolBar: TCheckBox;
    cbShowCmdLine: TCheckBox;
    cbShowCurDir: TCheckBox;
    cbShowDiskPanel: TCheckBox;
    cbShowDriveMenuButton: TCheckBox;
    cbShowKeysPanel: TCheckBox;
    cbShowStatusBar: TCheckBox;
    cbShowTabHeader: TCheckBox;
    cbShowTabs: TCheckBox;
    cbTwoDiskPanels: TCheckBox;
    gbCopyBufferSize: TGroupBox;
    gbGeneralOptions: TGroupBox;
    gbDateTimeFormat: TGroupBox;
    gbTabs: TGroupBox;
    gbFileSearch: TGroupBox;
    gbLocConfigFiles: TGroupBox;
    gbSaveOnExit: TGroupBox;
    lblMouseMode: TLabel;
    lblConfigColumns: TLabel;
    lblCategoryAttr: TLabel;
    lblInstalledPlugins1: TLabel;
    lstColumnsSets: TListBox;
    pnlButtons: TPanel;
    pgColumns: TPage;
    rgScrolling: TRadioGroup;
    rbCtrlAltLetterQS: TRadioButton;
    rbAltLetterQS: TRadioButton;
    rbNoneQS: TRadioButton;
    cbExactBeginning: TCheckBox;
    cbExactEnding: TCheckBox;
    rbLetterQS: TRadioButton;
    ilTreeView: TImageList;
    lblChar: TLabel;
    lblDateTimeExample: TLabel;
    lblDateTimeFormat: TLabel;
    lblCopyBufferSize: TLabel;
    lblIconsSize: TLabel;
    lblInstalledPlugins: TLabel;
    lblCategoryColor: TLabel;
    lblCategoryName: TLabel;
    lblCategoryMask: TLabel;
    lblBackground2: TLabel;
    lblMarkColor: TLabel;
    lblCursorColor: TLabel;
    lblCursorText: TLabel;
    lblAssociateWith: TLabel;
    lblExt: TLabel;
    lblAbout: TLabel;
    lblActions: TLabel;
    lblEditorFont: TLabel;
    lblHotKey: TLabel;
    lblMainFont: TLabel;
    lblRunTerm: TLabel;
    lblTerm: TLabel;
    lblViewerFont: TLabel;
    lbCategories: TListBox;
    lngList: TListBox;
    cbLogArcOp: TCheckBox;
    cbLogCpMvLn: TCheckBox;
    cbLogDelete: TCheckBox;
    cbLogErrors: TCheckBox;
    cbLogDirOp: TCheckBox;
    cbLogVFS: TCheckBox;
    cbLogInfo: TCheckBox;
    cbLogSuccess: TCheckBox;
    nbNotebook: TNotebook;
    odOpenDialog: TOpenDialog;
    optColorDialog: TColorDialog;
    pgQuickSearch: TPage;
    pgConfigStorage: TPage;
    pgLogFile: TPage;
    pgTabs: TPage;
    pgFileOp: TPage;
    pbExample: TPaintBox;
    pcPluginsTypes: TPageControl;
    pcPluginsType: TPageControl;
    pgFileTypesColors: TPage;
    pgLayout: TPage;
    pgPlugins: TPage;
    pnlCaption: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pgBehav: TPage;
    pgColor: TPage;
    pgFonts: TPage;
    pgHotKey: TPage;
    pgLng: TPage;
    pgTools: TPage;
    gbQuickSearch: TGroupBox;
    rbProgramDir: TRadioButton;
    rbUserHomeDir: TRadioButton;
    rbUseMmapInSearch: TRadioButton;
    rbUseStreamInSearch: TRadioButton;
    tsWDX: TTabSheet;
    tsWCX: TTabSheet;
    tsWFX: TTabSheet;
    tvTreeView: TTreeView;
    procedure bbtnAddCategoryClick(Sender: TObject);
    procedure bbtnApplyCategoryClick(Sender: TObject);
    procedure bbtnApplyClick(Sender: TObject);
    procedure bbtnDeleteCategoryClick(Sender: TObject);
    procedure bbtnWDXAddClick(Sender: TObject);
    procedure bbtnWDXApplyClick(Sender: TObject);
    procedure bbtnWDXDeleteClick(Sender: TObject);
    procedure bbtnWDXRenameClick(Sender: TObject);
    procedure bbtnWFXAddClick(Sender: TObject);
    procedure bbtnWFXApplyClick(Sender: TObject);
    procedure bbtnWFXDeleteClick(Sender: TObject);
    procedure bbtnWFXRenameClick(Sender: TObject);
    procedure btClearHotKeyClick(Sender: TObject);
    procedure btnBackColor2Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCopyColumnsSetClick(Sender: TObject);
    procedure btnCursorColorClick(Sender: TObject);
    procedure btnCursorTextClick(Sender: TObject);
    procedure btnCategoryColorClick(Sender: TObject);
    procedure btnDelColumnsSetClick(Sender: TObject);
    procedure btnEditColumnsSetClick(Sender: TObject);
    procedure btnMarkColorClick(Sender: TObject);
    procedure btnNewColumnsSetClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure cbCategoryColorChange(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure cbDateTimeFormatChange(Sender: TObject);
    procedure cbExtChange(Sender: TObject);
    procedure cbShowDiskPanelChange(Sender: TObject);
    procedure cbShowIconsChange(Sender: TObject);
    procedure clbWCXListClick(Sender: TObject);
    procedure cbTextColorChange(Sender: TObject);
    procedure cbColorBoxDropDown(Sender: TObject);
    procedure edtEditorSizeChange(Sender: TObject);
    procedure edtMainSizeChange(Sender: TObject);
    procedure edtViewerSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSetHotKeyClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelEditFntClick(Sender: TObject);
    procedure btnSelMainFntClick(Sender: TObject);
    procedure btnSelViewFntClick(Sender: TObject);
    procedure cbExtEditorClick(Sender: TObject);
    procedure cbExtDifferClick(Sender: TObject);
    procedure cbExtViewerClick(Sender: TObject);
    procedure cbMainFontChange(Sender: TObject);
    procedure cbEditorFontChange(Sender: TObject);
    procedure cbViewerFontChange(Sender: TObject);
    procedure edHotKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormShow(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure nbNotebookPageChanged(Sender: TObject);
    procedure pbExamplePaint(Sender: TObject);
    procedure pgBehavResize(Sender: TObject);
    procedure tsWCXShow(Sender: TObject);
    procedure tsWDXShow(Sender: TObject);
    procedure tsWFXShow(Sender: TObject);
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    vShortCut: TShortCut;
  public
    { Public declarations }
    procedure FillLngListBox;
    procedure FillFontLists;
    procedure FillActionLists;
    procedure FillFileColorsList;
    procedure FillColumnsList;
    
  end;

implementation

uses
  uLng, uGlobs, uGlobsPaths, uPixMapManager, fMain, ActnList, LCLProc, menus,
  uColorExt, uWCXModule, uWFXmodule, uDCUtils, uOSUtils,fColumnsSetConf;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  // Scrolling radio group localization
  with rgScrolling do
    begin
      Items.Strings[0] := rsOptLineByLineCursor;
      Items.Strings[1] := rsOptLineByLine;
      Items.Strings[2] := rsOptPageByPage;
    end;
  // tvTreeView localization
  with tvTreeView.Items do
    begin
      Item[0].Text := rsOptLanguage;
      Item[1].Text := rsOptBehav;
      Item[2].Text := rsOptTools;
      Item[3].Text := rsOptFonts;
      Item[4].Text := rsOptColors;
      Item[5].Text := rsOptFilePanels;
      Item[6].Text := rsOptFileTypes;
      Item[7].Text := rsOptHotKeys;
      Item[8].Text := rsOptPlugins;
      Item[9].Text := rsOptLayout;
      Item[10].Text := rsOptFileOp;
      Item[11].Text := rsOptFolderTabs;
      Item[12].Text := rsOptLog;
      Item[13].Text := rsOptConfig;
      Item[14].Text := rsOptQuickSearch;
      Item[15].Text := rsOptColumns;
    end;
  tvTreeView.Items.Item[0].Selected:= True;

  {Layout page}
  cbShowMainToolBar.Checked := gButtonBar;
  cbFlatToolBar.Checked := gToolBarFlat;
  cbShowDiskPanel.Checked := gDriveBar1;
  cbTwoDiskPanels.Checked := gDriveBar2;
  cbFlatDiskPanel.Checked := gDriveBarFlat;
  cbShowDriveMenuButton.Checked := gDriveMenuButton;
  cbShowTabs.Checked := gDirectoryTabs;
  cbShowCurDir.Checked := gCurDir;
  cbShowTabHeader.Checked := gTabHeader;
  cbShowStatusBar.Checked := gStatusBar;
  cbShowCmdLine.Checked := gCmdLine;
  cbShowKeysPanel.Checked := gKeyButtons;
  cbFlatInterface.Checked := gInterfaceFlat;
  cbLogWindow.Checked := gLogWindow;

  cbDirSelect.Checked:=gDirSelect;
  cbCaseSensitiveSort.Checked:=gCaseSensitiveSort;
  cbLynxLike.Checked:=gLynxLike;
  cbShortFileSizeFormat.Checked:=gShortFileSizeFormat;
  
  cbSelectionByMouse.Checked:=gMouseSelectionEnabled;
  cbMouseMode.ItemIndex := gMouseSelectionButton;

  cbExtEditor.Checked:=gUseExtEdit;
  cbExtViewer.Checked:=gUseExtView;
  cbExtDiffer.Checked:=gUseExtDiff;
  if gScrollMode < rgScrolling.Items.Count then
    rgScrolling.ItemIndex:=  gScrollMode
  else
    rgScrolling.ItemIndex:= 0;
  cbDateTimeFormat.Text:= gDateTimeFormat;
  lblDateTimeExample.Caption:= FormatDateTime(gDateTimeFormat, Now);

  fneExtEditor.FileName := gExtEdit;
  fneExtViewer.FileName := gExtView;
  fneExtDiffer.FileName := gExtDiff;

  fneExtEditor.Enabled:= cbExtEditor.Checked;
  fneExtDiffer.Enabled:= cbExtDiffer.Checked;
  fneExtViewer.Enabled:= cbExtViewer.Checked;

  edtRunTerm.Text:=gRunTerm;

  //tvTreeView.Items.Item[3].Text := lngGetString(clngDlgOptFonts);

  { Colors }
  cbTextColor.Selected := gForeColor;
  cbTextColor.Color := gForeColor;

  cbBackColor.Selected := gBackColor;
  cbBackColor.Color := gBackColor;

  cbBackColor2.Selected := gBackColor2;
  cbBackColor2.Color := gBackColor2;

  cbMarkColor.Selected := gMarkColor;
  cbMarkColor.Color := gMarkColor;

  cbCursorColor.Selected := gCursorColor;
  cbCursorColor.Color := gCursorColor;

  cbCursorText.Selected := gCursorText;
  cbCursorText.Color := gCursorText;

  cbShowIcons.Checked := gShowIcons;

  { File operations }
  edtCopyBufferSize.Text:= IntToStr(gCopyBlockSize div 1024);
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
  rbUseMmapInSearch.Checked := gUseMmapInSearch;

  { Log file }
  cbLogFile.Checked := gLogFile;
  fneLogFileName.FileName := gLogFileName;
  cbLogCpMvLn.Checked := (log_cp_mv_ln in gLogOptions);
  cbLogDelete.Checked := (log_delete in gLogOptions);
  cbLogDirOp.Checked := (log_dir_op in gLogOptions);
  cbLogArcOp.Checked := (log_arc_op in gLogOptions);
  cbLogVFS.Checked := (log_vfs_op in gLogOptions);
  cbLogSuccess.Checked := (log_success in gLogOptions);
  cbLogErrors.Checked := (log_errors in gLogOptions);
  cbLogInfo.Checked := (log_info in gLogOptions);

  {Folder tabs}
  cbTabsAlwaysVisible.Checked := Boolean(gDirTabOptions and tb_always_visible) and gDirectoryTabs;
  cbTabsMultiLines.Checked :=  Boolean(gDirTabOptions and tb_multiple_lines);
  cbTabsLimitOption.Checked := Boolean(gDirTabOptions and tb_text_length_limit);
  edtTabsLimitLength.Text := IntToStr(gDirTabLimit);

  {Configuration storage}
  if gUseIniInProgramDir then
    rbProgramDir.Checked := True
  else
    rbUserHomeDir.Checked := True;
  cbDirHistory.Checked := gSaveDirHistory;
  cbCmdLineHistory.Checked := gSaveCmdLineHistory;
  cbFileMaskHistory.Checked := gSaveFileMaskHistory;
  { Quick Search page}
  if gQuickSearch then
    begin
      if (gQuickSearchMode = [ssCtrl, ssAlt]) then
        rbCtrlAltLetterQS.Checked := True
      else if (gQuickSearchMode = [ssAlt]) then
        rbAltLetterQS.Checked := True
      else if gQuickSearchMode = [] then
        rbLetterQS.Checked := True;
    end
  else
    rbNoneQS.Checked := True;

  cbExactBeginning.Checked := gQuickSearchMatchBeginning;
  cbExactEnding.Checked := gQuickSearchMatchEnding;

  { Icons sizes in file panels }
  cbIconsSize.Text := IntToStr(gIconsSize) + 'x' + IntToStr(gIconsSize);

  FillActionLists;
  FillLngListBox;
  FillFontLists;
  FillFileColorsList;
  DebugLn(gTerm);
  edtTerm.Text:=gTerm;


  { Columns Set}
  //ColSet.Clear;
 // ColSet.Load(gIni);
  FillColumnsList;
  
   nbNotebook.PageIndex := 0;  //let not warning on which page save form
  
end;

procedure TfrmOptions.btSetHotKeyClick(Sender: TObject);
var vNum: integer;
    vActions: TAction;
begin
  // ToDo Black list HotKey which can't use

  for vNum := 0 to cbActions.Items.Count - 1 do
  begin
    vActions := cbActions.Items.Objects[vNum] as TAction;
    if vActions.ShortCut = vShortCut then
    begin
      ShowMessage('ShortCut used by '+vActions.Name);// ToDo lang
      Exit;
    end;
  end;

  vActions := cbActions.Items.Objects[cbActions.ItemIndex] as TAction;
  vActions.ShortCut := vShortCut;
  cbActions.Items[cbActions.ItemIndex] := vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')';
  cbActions.Text := vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')';
end;


procedure TfrmOptions.btnForeColorClick(Sender: TObject);
begin
 if optColorDialog.Execute then
   begin
     cbTextColor.Text := '';
     cbTextColor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.btnOpenClick(Sender: TObject);
var
  WCXmodule : TWCXmodule;
begin
  odOpenDialog.Filter := 'Archive plugins (*.wcx)|*.wcx';
  if odOpenDialog.Execute then
  begin
    WCXmodule := TWCXmodule.Create;
    if WCXmodule.LoadModule(odOpenDialog.FileName)then
      cbWCXPath.Text := IntToStr(WCXmodule.VFSMisc) + ',' + SetCmdDirAsEnvVar(odOpenDialog.FileName)
    else
      cbWCXPath.Text := '0,' + SetCmdDirAsEnvVar(odOpenDialog.FileName);
  WCXModule.UnloadModule;
  WCXmodule.Free;
  end;
end;

procedure TfrmOptions.btnBackColorClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbBackColor.Text := '';
     cbBackColor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.cbColorBoxChange(Sender: TObject);
begin
  (Sender as TColorBox).Color := (Sender as TColorBox).Selected;
  pbExample.Repaint;
end;

procedure TfrmOptions.cbDateTimeFormatChange(Sender: TObject);
begin
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
end;

procedure TfrmOptions.cbExtChange(Sender: TObject);
begin
  clbWCXList.ItemIndex := cbExt.Items.IndexOf(cbExt.Text)+1;
  cbWCXPath.Text:= clbWCXList.Items[clbWCXList.ItemIndex];
end;

procedure TfrmOptions.cbShowDiskPanelChange(Sender: TObject);
begin
  cbTwoDiskPanels.Enabled := cbShowDiskPanel.Checked;
  cbFlatDiskPanel.Enabled := cbShowDiskPanel.Checked;
end;

procedure TfrmOptions.cbShowIconsChange(Sender: TObject);
begin
  cbIconsSize.Enabled := cbShowIcons.Checked;
end;

procedure TfrmOptions.clbWCXListClick(Sender: TObject);
begin
  cbWCXPath.Text := clbWCXList.Items[clbWCXList.ItemIndex];
end;

procedure TfrmOptions.cbTextColorChange(Sender: TObject);
begin
  cbTextColor.Color := cbTextColor.Selected;
end;

procedure TfrmOptions.cbColorBoxDropDown(Sender: TObject);
begin
  (Sender as TColorBox).Color := clWindow;
end;

procedure TfrmOptions.edtEditorSizeChange(Sender: TObject);
begin
  edtTest2.Font.Size := edtEditorSize.Value;
end;

procedure TfrmOptions.edtMainSizeChange(Sender: TObject);
begin
  edtTest1.Font.Size := edtMainSize.Value;
end;

procedure TfrmOptions.edtViewerSizeChange(Sender: TObject);
begin
  edtTest3.Font.Size := edtViewerSize.Value;
end;

procedure TfrmOptions.FillLngListBox;
var
  fr:TSearchRec;
  iIndex:Integer;
  sLangName : String;
begin
  lngList.Clear;
  DebugLn('Language dir: ' + gpLngDir);
  if FindFirst(gpLngDir+'*.po', faAnyFile, fr)<>0 then
  begin
    FindClose(fr);
    Exit;
  end;
  repeat
    sLangName := GetLanguageName(gpLngDir + fr.Name);
    lngList.Items.Add(Format('%s = (%s)', [fr.Name, sLangName]));
  until FindNext(fr)<>0;
  
  FindClose(fr);

  iIndex:=lngList.Items.IndexOfName(gPOFileName + #32);
  if iIndex>=0 then
    lngList.Selected[iIndex]:=True;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
var
  FS : TFontStyles;
begin
  inherited;
  
  {Layout page}
  gButtonBar := cbShowMainToolBar.Checked;
  gToolBarFlat := cbFlatToolBar.Checked;
  gDriveBar1 := cbShowDiskPanel.Checked;
  gDriveBar2 := cbTwoDiskPanels.Checked;
  gDriveBarFlat := cbFlatDiskPanel.Checked;
  gDriveMenuButton := cbShowDriveMenuButton.Checked;
  gDirectoryTabs := cbShowTabs.Checked;
  gCurDir := cbShowCurDir.Checked;
  gTabHeader := cbShowTabHeader.Checked;
  gStatusBar := cbShowStatusBar.Checked;
  gCmdLine := cbShowCmdLine.Checked;
  gKeyButtons := cbShowKeysPanel.Checked;
  gInterfaceFlat := cbFlatInterface.Checked;
  gLogWindow := cbLogWindow.Checked;
  
  gTerm:=edtTerm.Text;
  if lngList.ItemIndex>-1 then
    gPOFileName := lngList.Items.Names[lngList.ItemIndex];
  gDirSelect:=cbDirSelect.Checked;
  gCaseSensitiveSort:=cbCaseSensitiveSort.Checked;
  gLynxLike:=cbLynxLike.Checked;
  gShortFileSizeFormat:=cbShortFileSizeFormat.Checked;
  gScrollMode := rgScrolling.ItemIndex;
  gDateTimeFormat := cbDateTimeFormat.Text;
  
  gMouseSelectionEnabled := cbSelectionByMouse.Checked;
  gMouseSelectionButton := cbMouseMode.ItemIndex;

  gUseExtEdit:=cbExtEditor.Checked;
  gUseExtView:=cbExtViewer.Checked;
  gUseExtDiff:=cbExtDiffer.Checked;

  gExtEdit:= fneExtEditor.FileName;
  gExtView:= fneExtViewer.FileName;
  gExtDiff:= fneExtDiffer.FileName;
  gRunTerm:= edtRunTerm.Text;
  
  gFontName:=cbMainFont.Text;
  FS := EdtTest1.Font.Style;
  Move(FS, gFontWeight, 1);

  gEditorFontName:=cbEditorFont.Text;
  gViewerFontName:=cbViewerFont.Text;
  
  {$hints off}
  gEditorSize:=Round(edtEditorSize.Value);
  gViewerSize:=Round(edtViewerSize.Value);
  gFontSize:=Round(edtMainSize.Value);
  {$hints on}

  { Colors }
  gForeColor := cbTextColor.Color;
  gBackColor := cbBackColor.Color; // background color
  gBackColor2 := cbBackColor2.Color;
  gMarkColor := cbMarkColor.Color;
  gCursorColor := cbCursorColor.Color;
  gCursorText := cbCursorText.Color;
  
  gShowIcons := cbShowIcons.Checked;

  { File operations }
  gCopyBlockSize := StrToIntDef(edtCopyBufferSize.Text, gCopyBlockSize) * 1024;
  gDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;
  gUseMmapInSearch := rbUseMmapInSearch.Checked;
  
  { Log file }
  gLogFile := cbLogFile.Checked;
  gLogFileName := fneLogFileName.FileName;
  gLogOptions := []; // Reset log options
  if cbLogCpMvLn.Checked then
    Include(gLogOptions, log_cp_mv_ln);
  if cbLogDelete.Checked then
    Include(gLogOptions, log_delete);
  if cbLogDirOp.Checked then
    Include(gLogOptions, log_dir_op);
  if cbLogArcOp.Checked then
    Include(gLogOptions, log_arc_op);
  if cbLogVFS.Checked then
    Include(gLogOptions, log_vfs_op);
  if cbLogSuccess.Checked then
    Include(gLogOptions, log_success);
  if cbLogErrors.Checked then
    Include(gLogOptions, log_errors);
  if cbLogInfo.Checked then
    Include(gLogOptions, log_info);
    
  { Folder tabs }
  gDirTabOptions := 0;  // Reset tab options
  if cbTabsAlwaysVisible.Checked then
    gDirTabOptions :=  (gDirTabOptions or tb_always_visible);
  if cbTabsMultiLines.Checked then
    gDirTabOptions := (gDirTabOptions or tb_multiple_lines);
    
  if cbTabsLimitOption.Checked then
    gDirTabOptions := (gDirTabOptions or tb_text_length_limit);
    
  gDirTabLimit := StrToIntDef(edtTabsLimitLength.Text, 32);

  { Configuration storage }
  gUseIniInProgramDir := rbProgramDir.Checked;
  gSaveDirHistory := cbDirHistory.Checked;
  gSaveCmdLineHistory := cbCmdLineHistory.Checked;
  gSaveFileMaskHistory := cbFileMaskHistory.Checked;

  { Quick Search page}
  gQuickSearch := not rbNoneQS.Checked;
  
  if rbCtrlAltLetterQS.Checked then
    gQuickSearchMode := [ssCtrl, ssAlt];
  if rbAltLetterQS.Checked then
    gQuickSearchMode := [ssAlt];
  if rbLetterQS.Checked then
    gQuickSearchMode := [];

  gQuickSearchMatchBeginning := cbExactBeginning.Checked;
  gQuickSearchMatchEnding := cbExactEnding.Checked;

  frmMain.UpdateWindowView;
  frmMain.Repaint; // for panels repaint
  frmMain.SaveShortCuts;
  
  {Columns Set}
  ColSet.Save(gIni);
  
  if (gIconsSize <> StrToInt(Copy(cbIconsSize.Text, 1, 2))) then
    begin
      gIconsSize := StrToInt(Copy(cbIconsSize.Text, 1, 2)); //file panel icons size
      SaveGlobs;
      ShowMessage('Double Commander will restart for apply changes');
      ExecCmdFork(Application.ExeName);
      frmMain.Close;
    end;
end;

procedure TfrmOptions.btnSelEditFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name:=cbEditorFont.Text;
  if dlgFnt.Execute then
    begin
      cbEditorFont.Text:=dlgFnt.Font.Name;
      edtEditorSize.Value := dlgFnt.Font.Size;
      edtTest2.Font := dlgFnt.Font;
    end;
end;

procedure TfrmOptions.btnSelMainFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name:=cbMainFont.Text;
  if dlgFnt.Execute then
    begin
      cbMainFont.Text:=dlgFnt.Font.Name;
      edtMainSize.Value := dlgFnt.Font.Size;
      edtTest1.Font := dlgFnt.Font;
    end;
end;

procedure TfrmOptions.btnSelViewFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name:=cbViewerFont.Text;
  if dlgFnt.Execute then
    begin
      cbViewerFont.Text:=dlgFnt.Font.Name;
      edtViewerSize.Value:= dlgFnt.Font.Size;
      edtTest3.Font := dlgFnt.Font;
    end;
end;

procedure TfrmOptions.cbExtEditorClick(Sender: TObject);
begin
  inherited;
  fneExtEditor.Enabled:=cbExtEditor.Checked
end;

procedure TfrmOptions.cbExtDifferClick(Sender: TObject);
begin
  inherited;
  fneExtDiffer.Enabled:=cbExtDiffer.Checked
end;

procedure TfrmOptions.cbExtViewerClick(Sender: TObject);
begin
  inherited;
  fneExtViewer.Enabled:=cbExtViewer.Checked
end;

procedure TfrmOptions.FillFontLists;
var
  FS : TFontStyles;
begin
  cbMainFont.Text := gFontName;
  cbViewerFont.Text := gViewerFontName;
  cbEditorFont.Text := gEditorFontName;

  Move(gFontWeight, FS, 1);
  EdtTest1.Font.Style := FS;
    
  edtEditorSize.Value:=gEditorSize;
  edtViewerSize.Value:=gViewerSize;
  edtMainSize.Value:=gFontSize;
  
  with edtTest1.Font do
  begin
    Name := gFontName;
    Size := gFontSize;
  end; // with
  
  with edtTest2.Font do
  begin
    Name := gEditorFontName;
    Size := gEditorSize;
  end; // with
  
  with edtTest3.Font do
  begin
    Name := gViewerFontName;
    Size := gViewerSize;
  end; // with
end;

procedure TfrmOptions.FillActionLists;
var vNum: integer;
var vActions: TAction;
begin
  for vNum := 0 to frmMain.actionLst.ActionCount -1 do
  begin
    vActions := frmMain.actionLst.Actions[vNum] as TAction;
    cbActions.Items.AddObject(vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')',vActions);
  end;
end;

procedure TfrmOptions.cbMainFontChange(Sender: TObject);
begin
//  edtTest1.Font.Name:=cbMainFont.Text;
end;

procedure TfrmOptions.cbEditorFontChange(Sender: TObject);
begin
//  edtTest2.Font.Name:=cbEditorFont.Text;
end;

procedure TfrmOptions.cbViewerFontChange(Sender: TObject);
begin
//  edtTest3.Font.Name:=cbViewerFont.Text;
end;

procedure TfrmOptions.edHotKeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  vShortCut := ShortCut(Key,Shift);
  TEdit(Sender).Text := ShortCutToText(vShortCut);
  Key := 0;
  btSetHotKey.Enabled := (edHotKey.Text <> '');
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
//Load specified page or 0
  tvTreeView.Items.Item[Self.Tag].Selected:=true;
  nbNotebook.PageIndex := Self.Tag;

end;

procedure TfrmOptions.pbExamplePaint(Sender: TObject);
var
  h, I : integer;
  sText : String;
  iTextTop, iTextLeft : Integer;
  Rect : TRect;
begin
  h := pbExample.Height div 6;
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := pbExample.Width;
  Rect.Bottom := h;

  for I := 1 to 6 do
  with pbExample.Canvas do
  begin
    case I of
    1:
      begin
        Brush.Color := cbBackColor.Color;
        Font.Color := cbMarkColor.Color;
        sText := 'Mark';
      end;
    2:
      begin
        Brush.Color := cbBackColor2.Color;
        Font.Color := cbMarkColor.Color;
        sText := 'Mark';
      end;
    3:
      begin
        Brush.Color := cbBackColor.Color;
        Font.Color := cbTextColor.Color;
        sText := 'Text';
      end;
    4:
      begin
        Brush.Color := cbBackColor2.Color;
        Font.Color := cbTextColor.Color;
        sText := 'Text';
      end;
    5:
      begin
        Brush.Color := cbCursorColor.Color;
        Font.Color := cbCursorText.Color;
        sText := 'Cursor';
      end;
    6:
      begin
        Brush.Color := cbCursorColor.Color;
        Font.Color := cbMarkColor.Color;
        sText := 'Mark + Cursor';
      end;
    end; // case
    
    Font.Style := EdtTest1.Font.Style;
    Font.Size := EdtTest1.Font.Size;
    Font.Name := EdtTest1.Font.Name;

    iTextTop := Rect.Top + (h div 2) - (TextHeight(sText) div 2);
    iTextLeft := Rect.Left + (pbExample.Width div 2) - (TextWidth(sText) div 2);
    FillRect(Rect);
    TextOut(iTextLeft, iTextTop, sText);
    Rect.Top := Rect.Bottom;
    Rect.Bottom := h * (I + 1);
  end; // for
end;

procedure TfrmOptions.pgBehavResize(Sender: TObject);
var
  iWidth: Integer;
begin
  iWidth:= (pgBehav.Width div 2) - 26;
  gb.Width:= iWidth;
  gbDateTimeFormat.Width:= iWidth;
end;

procedure TfrmOptions.tsWCXShow(Sender: TObject);
var
  I : Integer;
  sCurrPlugin,
  sExt : String;
  PosEqual : Integer;
begin
  cbExt.Items.Clear;
  cbWCXPath.Text:= '';
  gIni.ReadSectionRaw('PackerPlugins', clbWCXList.Items);
  for I := 0 to clbWCXList.Count - 1 do
  begin
    sCurrPlugin := clbWCXList.Items[I];
    PosEqual := Pos('=', sCurrPlugin);
    sExt := Copy(sCurrPlugin, 1, PosEqual - 1);
    if sExt[1] = '#' then
      Delete(sExt, 1, 1);
    cbExt.Items.Add(sExt);
    if Pos('#', clbWCXList.Items[I]) = 0 then
      begin
        clbWCXList.Items[I] := Copy(sCurrPlugin, PosEqual + 1, Length(sCurrPlugin) - PosEqual);
        clbWCXList.Checked[I] := True
      end
    else
      begin
        clbWCXList.Items[I] := Copy(sCurrPlugin, PosEqual + 1, Length(sCurrPlugin) - PosEqual);
        clbWCXList.Checked[I] := False;
      end;
  end;
  clbWCXList.Items.Insert(0, '(none)');
end;

procedure TfrmOptions.tsWDXShow(Sender: TObject);
var i:integer;
begin
   clbWDXList.Clear;
  if WdxPlugins.Count=0 then exit;
  for i:=0 to WdxPlugins.Count-1 do
    clbWDXList.Items.Add(WdxPlugins.GetWdxModule(i).Name+'='+SetCmdDirAsEnvVar(WdxPlugins.GetWdxModule(i).FileName));
end;

procedure TfrmOptions.bbtnApplyClick(Sender: TObject);
var
 I,
 iIndex : Integer;
 bChecked : Boolean;
begin
  if cbExt.Text <> '' then
  if cbExt.Items.IndexOf(cbExt.Text) < 0 then
    begin
    
      if cbWCXPath.Text = '' then
        Exit;
        
      if Pos(',', cbWCXPath.Text) = 0 then
        iIndex := clbWCXList.Items.Add('0,' + cbWCXPath.Text)
      else
        iIndex := clbWCXList.Items.Add(cbWCXPath.Text);
      clbWCXList.Checked[iIndex] := True;
      cbExt.Items.Add(cbExt.Text);
    end
  else
    begin
      iIndex := cbExt.Items.IndexOf(cbExt.Text);
      bChecked := clbWCXList.Checked[iIndex];
      
      // Delete plugin for cbExt.Items[iIndex] extension
      if cbWCXPath.Text = '(none)' then
        begin
          gIni.DeleteKey('PackerPlugins', cbExt.Items[iIndex]);
          cbExt.Items.Delete(iIndex);
          clbWCXList.Items.Delete(iIndex+1);
          cbExt.Text:= '';
          cbWCXPath.Text:= '';
          cbExt.ItemIndex:= -1;
          clbWCXList.ItemIndex:= -1;
          Exit;
        end;
        
      if Pos(',', cbWCXPath.Text) = 0 then
        clbWCXList.Items[iIndex] := '0,' + cbWCXPath.Text
      else
        clbWCXList.Items[iIndex] := cbWCXPath.Text;
      clbWCXList.Checked[iIndex] := bChecked;
    end;
  
  for I := 1 to clbWCXList.Count - 1 do
    begin
      if clbWCXList.Checked[I] then
        begin
          gIni.DeleteKey('PackerPlugins', '#' + cbExt.Items[I-1]);
          gIni.WriteString('PackerPlugins', cbExt.Items[I-1],  clbWCXList.Items[I])
        end
      else
        begin
          gIni.DeleteKey('PackerPlugins', cbExt.Items[I-1]);
          gIni.WriteString('PackerPlugins', '#' + cbExt.Items[I-1],  clbWCXList.Items[I]);
        end;
    end;
end;

{ WFX plugins }

procedure TfrmOptions.tsWFXShow(Sender: TObject);
var
  I : Integer;
  sCurrPlugin : String;
begin
  gIni.ReadSectionRaw('FileSystemPlugins', clbWFXList.Items);
  for I := 0 to clbWFXList.Count - 1 do
  begin
    sCurrPlugin := clbWFXList.Items[I];

    if Pos('#', clbWFXList.Items[I]) = 0 then
      begin
        clbWFXList.Items[I] := Copy(sCurrPlugin, 1, Length(sCurrPlugin));
        clbWFXList.Checked[I] := True
      end
    else
      begin
        clbWFXList.Items[I] := Copy(sCurrPlugin, 2, Length(sCurrPlugin) - 1);
        clbWFXList.Checked[I] := False;
      end;
  end;
end;

procedure TfrmOptions.bbtnWFXAddClick(Sender: TObject);
var
  WFXmodule : TWFXmodule;
  s,sPluginName : String;
  tmpPc:Cardinal;
begin
  odOpenDialog.Filter := 'File system plugins (*.wfx)|*.wfx';
  if odOpenDialog.Execute then
  begin
  DebugLn('Dialog executed');
    WFXmodule := TWFXmodule.Create;
    DebugLn('TWFXmodule created');
    if WFXmodule.LoadModule(odOpenDialog.FileName) then
     begin
       DebugLn('WFXModule Loaded');
       tmpPc:=WFXmodule.VFSMisc;
       if (tmpPc)>0 then
        sPluginName := PChar(Pointer(tmpPc)) + '=' + SetCmdDirAsEnvVar(odOpenDialog.FileName)
       else
         begin
           DebugLn('WFX alternate name');
           s:=ExtractFileName(odOpenDialog.FileName);
           s:=copy(s,1,pos('.',s)-1);
           sPluginName := s + '=' + SetCmdDirAsEnvVar(odOpenDialog.FileName)
         end;
     end
    else
    begin
      DebugLn('Module not loaded');
      sPluginName := ExtractFileName(odOpenDialog.FileName) +'=' + SetCmdDirAsEnvVar(odOpenDialog.FileName);
    end;

  DebugLn('WFX sPluginName='+sPluginName);
  clbWFXList.Items.Add(sPluginName);
  DebugLn('WFX Item Added');
  WFXModule.UnloadModule;
  DebugLn('WFX Module Unloaded');
  WFXmodule.Free;
  DebugLn('WFX Freed');
  end;
end;

procedure TfrmOptions.bbtnWFXApplyClick(Sender: TObject);
var
 I,
 iIndex : Integer;
 bChecked : Boolean;
begin
  gIni.EraseSection('FileSystemPlugins');
  for I := 0 to clbWFXList.Count - 1 do
    begin
      if clbWFXList.Checked[I] then
        begin
          gIni.WriteString('FileSystemPlugins', clbWFXList.Items.Names[I],  clbWFXList.Items.ValueFromIndex[I])
        end
      else
        begin
          gIni.WriteString('FileSystemPlugins', '#' + clbWFXList.Items.Names[I],  clbWFXList.Items.ValueFromIndex[I]);
        end;
    end;
end;

procedure TfrmOptions.bbtnWFXDeleteClick(Sender: TObject);
begin
 if clbWFXList.SelCount > 0 then
   clbWFXList.Items.Delete(clbWFXList.ItemIndex);
end;

procedure TfrmOptions.bbtnWFXRenameClick(Sender: TObject);
var
  iItemIndex : Integer;
  sName,
  sValue : String;
  bChecked : Boolean;
begin
  iItemIndex := clbWFXList.ItemIndex;
  if iItemIndex < 0 then exit;
  sName := clbWFXList.Items.Names[iItemIndex];
  sValue := clbWFXList.Items.ValueFromIndex[iItemIndex];
  bChecked := clbWFXList.Checked[iItemIndex]; // Save state
  clbWFXList.Items[iItemIndex] := InputBox('Double Commander', 'Rename', sName) + '=' + sValue;
  clbWFXList.Checked[iItemIndex] := bChecked; // Restore state
end;

{/ WFX Plugins }

{ File types category color }

procedure TfrmOptions.FillFileColorsList;
var
  sCategoryName : String;
  I : Integer;
begin

  for I := 0 to gColorExt.MaskItemList.Count - 1 do
    begin
      sCategoryName := TMaskItem(gColorExt.MaskItemList.Items[I]).sName;
      lbCategories.Items.AddObject(sCategoryName,TMaskItem(gColorExt.MaskItemList.Items[I]));
    end; // for
    
    
    if lbCategories.Count > 0 then
      lbCategories.ItemIndex := 0
    else
      begin
        edtCategoryName.Enabled := False;
        edtCategoryMask.Enabled := False;
        edtCategoryAttr.Enabled := False;
        cbCategoryColor.Enabled := False;
        btnCategoryColor.Enabled := False;
        bbtnDeleteCategory.Enabled := False;
        bbtnApplyCategory.Enabled := False;
      end;
    lbCategoriesClick(lbCategories);
end;

procedure TfrmOptions.FillColumnsList;
var i:Integer;
begin
 lstColumnsSets.Clear;
 If ColSet.Items.Count>0 then
   begin
     lstColumnsSets.Items.AddStrings(ColSet.Items);
   end;

end;

procedure TfrmOptions.cbCategoryColorChange(Sender: TObject);
begin
  (Sender as TColorBox).Color := (Sender as TColorBox).Selected;
end;

procedure TfrmOptions.lbCategoriesClick(Sender: TObject);
var
  MaskItem : TMaskItem;
begin

  if (lbCategories.Count > 0) and (Assigned(lbCategories.Items.Objects[lbCategories.ItemIndex])) then
    begin
      edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      MaskItem := TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]);

      edtCategoryMask.Text := MaskItem.sExt;
      cbCategoryColor.Color := MaskItem.cColor;
      edtCategoryAttr.Text := MaskItem.sModeStr;
      cbCategoryColor.Selected := cbCategoryColor.Color;
    end
  else
    begin
      if lbCategories.Count = 0 then
        edtCategoryName.Text := ''
      else
        edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      edtCategoryMask.Text := '*';
      edtCategoryAttr.Text := '';
      cbCategoryColor.ItemIndex := -1;
      cbCategoryColor.Color := clWindow;
      cbCategoryColor.Selected := cbCategoryColor.Color;
    end;
end;

procedure TfrmOptions.lbCategoriesDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  With (Control as TListBox) do
   begin
     Canvas.FillRect(ARect);
     if not Selected[Index] then
       Canvas.Font.Color:=TMaskItem(Items.Objects[Index]).cColor
     else
       Canvas.Font.Color:=gCursorText;
       
     Canvas.TextOut(ARect.Left+2,ARect.Top+1,Items[Index]);
   end;
end;

procedure TfrmOptions.nbNotebookPageChanged(Sender: TObject);
begin 
  // temporally this is hack for bug http://www.freepascal.org/mantis/view.php?id=9635
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
end;

procedure TfrmOptions.bbtnAddCategoryClick(Sender: TObject);
var
  iIndex : Integer;
begin
  if lbCategories.Count = 0 then
    begin
      edtCategoryName.Enabled := True;
      edtCategoryMask.Enabled := True;
      edtCategoryAttr.Enabled := True;
      cbCategoryColor.Enabled := True;
      btnCategoryColor.Enabled := True;
      bbtnDeleteCategory.Enabled := True;
      bbtnApplyCategory.Enabled := True;
    end;
  iIndex := lbCategories.Items.AddObject('', nil);
  lbCategories.ItemIndex := iIndex;
  edtCategoryName.Text := '';
  edtCategoryMask.Text := '*';
  edtCategoryAttr.Text := '';
  cbCategoryColor.ItemIndex := -1;
  cbCategoryColor.Color := clWindow;
end;

procedure TfrmOptions.bbtnApplyCategoryClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  I, iCount : Integer;
begin
  if bbtnDeleteCategory.Tag = 0 then // if we add or change category
    begin
      lbCategories.Items[lbCategories.ItemIndex] := edtCategoryName.Text;
      MaskItem := TMaskItem.Create;
      MaskItem.sName := edtCategoryName.Text;
      MaskItem.cColor := cbCategoryColor.Color;
      if edtCategoryMask.Text = '' then
        edtCategoryMask.Text := '*'; // because we load colors from ini by mask
      MaskItem.sExt := edtCategoryMask.Text;
      MaskItem.sModeStr := edtCategoryAttr.Text;
      lbCategories.Items.Objects[lbCategories.ItemIndex] := MaskItem;
      if lbCategories.ItemIndex >= gColorExt.MaskItemList.Count then
        gColorExt.MaskItemList.Add(MaskItem)
      else
        begin
          TMaskItem(gColorExt.MaskItemList.Items[lbCategories.ItemIndex]).Free;
          gColorExt.MaskItemList.Items[lbCategories.ItemIndex] := MaskItem;
        end;

    end
  else  // if we delete category
    begin
      iCount := gColorExt.MaskItemList.Count - 1;
      for I := iCount downto 0 do  // delete old categories
        begin
          gColorExt.MaskItemList.Delete(I);
        end;
      iCount := lbCategories.Count;
      for I := 0 to iCount - 1 do  //write new categories
        begin
          gColorExt.MaskItemList.Add(lbCategories.Items.Objects[I]);
        end;
    end; // delete category

    bbtnDeleteCategory.Tag := 0;
end;

procedure TfrmOptions.bbtnDeleteCategoryClick(Sender: TObject);
begin
  lbCategories.Items.Delete(lbCategories.ItemIndex);
  bbtnDeleteCategory.Tag := 1; // show that we delete category
  if lbCategories.Count > 0 then
    lbCategories.ItemIndex := 0;
  lbCategoriesClick(lbCategories);
end;

procedure TfrmOptions.bbtnWDXAddClick(Sender: TObject);
var
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Content plugins (*.wdx)|*.wdx';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractFileName(odOpenDialog.FileName);
      delete(sPluginName,length(sPluginName)-4,4);
      WdxPlugins.Add(sPluginName,odOpenDialog.FileName,'');

      WdxPlugins.LoadModule(sPluginName);
     // s:=WdxPlugins.GetWdxModule(sPluginName).CallContentGetDetectString;
//      WdxPlugins.GetWdxModule(sPluginName).DetectStr:=s;
      WdxPlugins.GetWdxModule(sPluginName).UnloadModule;
      //WdxPlugins.GetWdxModule(sPluginName).LoadModule;
      //WdxPlugins.GetWdxModule(sPluginName).UnloadModule;
      //To get DetectStr uncoment these 2 lines and get value of WdxPlugins.GetWdxModule(sPluginName).DetectStr;

      sPluginName:=sPluginName+'=' + SetCmdDirAsEnvVar(odOpenDialog.FileName);
      clbWDXList.Items.Add(sPluginName);
    end;
end;

procedure TfrmOptions.bbtnWDXApplyClick(Sender: TObject);
begin
WdxPlugins.Save(gIni);
end;

procedure TfrmOptions.bbtnWDXDeleteClick(Sender: TObject);
begin
  if clbWDXList.SelCount > 0 then
  begin
    WdxPlugins.DeleteItem(clbWDXList.ItemIndex);
    clbWDXList.Items.Delete(clbWDXList.ItemIndex);
  end;
end;

procedure TfrmOptions.bbtnWDXRenameClick(Sender: TObject);
var
  iItemIndex : Integer;
  sName,
  sValue,s : String;
  bChecked : Boolean;
begin
  iItemIndex := clbWDXList.ItemIndex;
  if iItemIndex < 0 then exit;
  sName := clbWDXList.Items.Names[iItemIndex];
  sValue := clbWDXList.Items.ValueFromIndex[iItemIndex];
  bChecked := clbWDXList.Checked[iItemIndex]; // Save state
  s:= InputBox('Double Commander', 'Rename', sName);
  clbWDXList.Items[iItemIndex] := s + '=' + sValue;
  clbWDXList.Checked[iItemIndex] := bChecked; // Restore state
  WdxPlugins.GetWdxModule(iItemIndex).Name:=s;
end;

procedure TfrmOptions.btnCategoryColorClick(Sender: TObject);
begin
  if optColorDialog.Execute then
   begin
     cbCategoryColor.Text := '';
     cbCategoryColor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.btnDelColumnsSetClick(Sender: TObject);
begin
  if lstColumnsSets.ItemIndex=-1 then exit;
  if lstColumnsSets.Count=1 then exit;
  ColSet.DeleteColumnSet(gIni,lstColumnsSets.Items[lstColumnsSets.ItemIndex]);
  FillColumnsList;
end;

procedure TfrmOptions.btnEditColumnsSetClick(Sender: TObject);
begin
  //TODO: may be it would be better to show error message?
  if lstColumnsSets.ItemIndex=-1 then exit;
  
  Application.CreateForm(TfColumnsSetConf, frmColumnsSetConf);
  {EDIT Set}
  frmColumnsSetConf.edtNameofColumnsSet.Text:=lstColumnsSets.Items[lstColumnsSets.ItemIndex];
  frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(lstColumnsSets.ItemIndex+1);
  frmColumnsSetConf.Tag:=lstColumnsSets.ItemIndex;
  frmColumnsSetConf.ColumnClass.Clear;
  frmColumnsSetConf.ColumnClass.Load(gIni,lstColumnsSets.Items[lstColumnsSets.ItemIndex]);
  {EDIT Set}
  frmColumnsSetConf.ShowModal;
  FreeAndNil(frmColumnsSetConf);
  FillColumnsList;
end;

{/ File types category color }

procedure TfrmOptions.btClearHotKeyClick(Sender: TObject);
var vActions: TAction;
begin
  vActions := cbActions.Items.Objects[cbActions.ItemIndex] as TAction;
  vActions.ShortCut := TextToShortCut('');
  cbActions.Items[cbActions.ItemIndex] := vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')';
  cbActions.Text := vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')';
end;

procedure TfrmOptions.btnBackColor2Click(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbBackColor2.Text := '';
     cbBackColor2.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin

end;

procedure TfrmOptions.btnCopyColumnsSetClick(Sender: TObject);
var s:string;
begin
  if lstColumnsSets.ItemIndex=-1 then exit;
  s:=lstColumnsSets.Items[lstColumnsSets.ItemIndex];
  ColSet.CopyColumnSet(gIni,s,s+'_Copy');
  FillColumnsList;

end;

procedure TfrmOptions.btnCursorColorClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbCursorColor.Text := '';
     cbCursorColor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.btnCursorTextClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbCursorText.Text := '';
     cbCursorText.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.btnMarkColorClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbMarkColor.Text := '';
     cbMarkColor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.btnNewColumnsSetClick(Sender: TObject);
begin
  Application.CreateForm(TfColumnsSetConf, frmColumnsSetConf);
   // Create new Set
  frmColumnsSetConf.edtNameofColumnsSet.Text:='New Columns';
  frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(lstColumnsSets.Count+1);
  frmColumnsSetConf.Tag:=-1;
  frmColumnsSetConf.ColumnClass.Clear;
  frmColumnsSetConf.ShowModal;
  FreeAndNil(frmColumnsSetConf);
  FillColumnsList;
end;

procedure TfrmOptions.tvTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  //DebugLN('Page index == ' + IntToStr(Node.Index));
  nbNotebook.PageIndex := tvTreeView.Selected.ImageIndex; // temporally image index
  pnlCaption.Caption := tvTreeView.Selected.Text;
end;


initialization
 {$I fOptions.lrs}

end.
