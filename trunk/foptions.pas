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
  CheckLst, EditBtn,uColumns, Grids, uDSXModule, uWCXModule, uWDXModule,
  uWFXmodule, uWLXModule;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    bbtnAddCategory: TBitBtn;
    bbtnDeleteCategory: TBitBtn;
    bbtnApplyCategory: TBitBtn;
    btnConfigPlugin: TBitBtn;
    btnTweakPlugin: TBitBtn;
    btnRemovePlugin: TBitBtn;
    btnEnablePlugin: TBitBtn;
    btnAddPlugin: TBitBtn;
    btnCategoryColor: TButton;
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
    cbTabsOpenForeground: TCheckBox;
    cbbUseInvertedSelection: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbBackColor2: TColorBox;
    cbMarkColor: TColorBox;
    cbCursorColor: TColorBox;
    cbCursorText: TColorBox;
    cbTextColor: TColorBox;
    cbCategoryColor: TColorBox;
    cbDateTimeFormat: TComboBox;
    cbbFileSystem: TComboBox;
    cbMouseMode: TComboBox;
    cbRenameSelOnlyName: TCheckBox;
    cbCutTextToColWidth: TCheckBox;
    cbProcessComments: TCheckBox;
    cbTabsConfirmCloseAll: TCheckBox;
    cbTabsLockedAsterisk: TCheckBox;
    cTextLabel: TLabel;
    dlgFnt: TFontDialog;
    edHotKey: TEdit;
    edtParam: TEdit;
    edtCategoryAttr: TEdit;
    edtTabsLimitLength: TEdit;
    edtCopyBufferSize: TEdit;
    edtCategoryName: TEdit;
    edtCategoryMask: TEdit;
    edtEditorSize: TSpinEdit;
    edtMainSize: TSpinEdit;
    edtRunTerm: TEdit;
    edtRunInTerm: TEdit;
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
    gbMisc1: TGroupBox;
    gbExample: TGroupBox;
    gbFileTypesColors: TGroupBox;
    gbMisc2: TGroupBox;
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
    gbMisc3: TGroupBox;
    gbMisc4: TGroupBox;
    ledDriveBlackList: TLabeledEdit;
    lblDSXDescription: TLabel;
    lblWLXDescription: TLabel;
    lblWCXDescription: TLabel;
    lbcategory: TLabel;
    Label3: TLabel;
    lblParam: TLabel;
    lblWipePassNumber: TLabel;
    lblMouseMode: TLabel;
    lblConfigColumns: TLabel;
    lblCategoryAttr: TLabel;
    lblWDXDescription: TLabel;
    lbtypes: TLabel;
    lbxCategories: TListBox;
    lbxCommands: TListBox;
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
    lblWFXDescription: TLabel;
    lblCategoryColor: TLabel;
    lblCategoryName: TLabel;
    lblCategoryMask: TLabel;
    lblBackground2: TLabel;
    lblMarkColor: TLabel;
    lblCursorColor: TLabel;
    lblCursorText: TLabel;
    lblEditorFont: TLabel;
    lblHotKey: TLabel;
    lblMainFont: TLabel;
    lblRunTerm: TLabel;
    lblRunInTerm: TLabel;
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
    btnApply: TBitBtn;
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
    seWipePassNumber: TSpinEdit;
    stgPlugins: TStringGrid;
    tsWLX: TTabSheet;
    tsDSX: TTabSheet;
    tsWDX: TTabSheet;
    tsWCX: TTabSheet;
    tsWFX: TTabSheet;
    tvTreeView: TTreeView;
    procedure bbtnAddCategoryClick(Sender: TObject);
    procedure bbtnApplyCategoryClick(Sender: TObject);
    procedure bbtnDeleteCategoryClick(Sender: TObject);
    procedure btnConfigPluginClick(Sender: TObject);
    procedure btnDSXAddClick(Sender: TObject);
    procedure btnEnablePluginClick(Sender: TObject);
    procedure btnWDXAddClick(Sender: TObject);
    procedure btnWFXAddClick(Sender: TObject);
    procedure btnWLXAddClick(Sender: TObject);
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
    procedure btnWCXAddClick(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure btnRemovePluginClick(Sender: TObject);
    procedure btnTweakPluginClick(Sender: TObject);
    procedure cbbUseInvertedSelectionChange(Sender: TObject);
    procedure cbCategoryColorChange(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure cbDateTimeFormatChange(Sender: TObject);
    procedure cbShowDiskPanelChange(Sender: TObject);
    procedure cbShowIconsChange(Sender: TObject);
    procedure cbTextColorChange(Sender: TObject);
    procedure cbColorBoxDropDown(Sender: TObject);
    procedure edtEditorSizeChange(Sender: TObject);
    procedure edtMainSizeChange(Sender: TObject);
    procedure edtViewerSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSetHotKeyClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
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
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
    procedure nbNotebookPageChanged(Sender: TObject);
    procedure pbExamplePaint(Sender: TObject);
    procedure pcPluginsTypesChange(Sender: TObject);
    procedure pgBehavResize(Sender: TObject);
    procedure stgPluginsBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    procedure tsDSXShow(Sender: TObject);
    procedure tsWCXShow(Sender: TObject);
    procedure tsWDXShow(Sender: TObject);
    procedure tsWFXShow(Sender: TObject);
    procedure tsWLXShow(Sender: TObject);
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    vShortCut: TShortCut;
  public
    { Public declarations }
    procedure FillLngListBox;
    procedure FillFontLists;
    procedure FillFileColorsList;
    procedure FillColumnsList;
    procedure FillCommandsPage;
    procedure LoadConfig;
    procedure SaveConfig;
  end;

var
  tmpDSXPlugins: TDSXModuleList;
  tmpWCXPlugins: TWCXModuleList;
  tmpWDXPlugins: TWDXModuleList;
  tmpWFXPlugins: TWFXModuleList;
  tmpWLXPlugins: TWLXModuleList;

implementation

uses
  uLng, uGlobs, uGlobsPaths, uPixMapManager, fMain, ActnList, LCLProc, menus,
  uColorExt, uDCUtils, uOSUtils, fColumnsSetConf, uShowMsg,
  fTweakPlugin, uhotkeymanger, uTypes, StrUtils, uFindEx;



procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  // Resize window for screen size if need
  ResizeToScreen(Self);
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

  // create plugins lists
  tmpDSXPlugins:= TDSXModuleList.Create;
  tmpWCXPlugins:= TWCXModuleList.Create;
  tmpWDXPlugins:= TWDXModuleList.Create;
  tmpWFXPlugins:= TWFXModuleList.Create;
  tmpWLXPlugins:= TWLXModuleList.Create;
  // load all configuration
  LoadConfig;
  
  nbNotebook.PageIndex := 0;  //let not warning on which page save form
end;

procedure TfrmOptions.btSetHotKeyClick(Sender: TObject);
var vNum,i: integer;
    vActions: TAction;
    Cat:string;
    st:TStringList;
begin
  // ToDo Black list HotKey which can't use
//TODO: Realize full version of hotkey's using. Allow to bind hotkeys to any controls.

if lbxCategories.ItemIndex=-1 then exit;
if lbxCommands.ItemIndex=-1 then exit;
  cat:=lbxCategories.Items[lbxCategories.ItemIndex];
 if cat='Main' then
 begin
  i:=HotMan.GetHotKeyIndex(ShortCutToTextEx(vShortCut));
  if i=-1 then
    HotMan.AddHotKey(ShortCutToTextEx(vShortCut),lbxCommands.Items[lbxCommands.ItemIndex],edtParam.Text,frmMain)
  else
   begin
     st:=TStringList.Create;
     HotMan.GetControlsListBy(ShortCutToTextEx(vShortCut),st);
     
     if st.IndexOf('frmMain')>-1 then
       begin
          HotMan.GetCommandsListBy(ShortCutToTextEx(vShortCut),st);
          ShowMessage('ShortCut used by '+st.Text);
       end
       else
         begin
            HotMan.AddHotKey(ShortCutToTextEx(vShortCut),lbxCommands.Items[lbxCommands.ItemIndex],edtParam.Text,frmMain);
            edtParam.Text:='';
            edHotKey.Text:='';
         end;

     st.free;
   end;
 end else
 if cat='Lister' then
   begin

   end;
   

   
end;


procedure TfrmOptions.btnForeColorClick(Sender: TObject);
begin
 if optColorDialog.Execute then
   begin
     cbTextColor.Text := '';
     cbTextColor.Color := optColorDialog.Color;
     pbExample.Repaint;
   end;
end;

procedure TfrmOptions.btnBackColorClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbBackColor.Text := '';
     cbBackColor.Color := optColorDialog.Color;
     pbExample.Repaint;
   end;
end;

procedure TfrmOptions.cbbUseInvertedSelectionChange(Sender: TObject);
begin
  pbExample.Repaint;
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

procedure TfrmOptions.cbShowDiskPanelChange(Sender: TObject);
begin
  cbTwoDiskPanels.Enabled := cbShowDiskPanel.Checked;
  cbFlatDiskPanel.Enabled := cbShowDiskPanel.Checked;
end;

procedure TfrmOptions.cbShowIconsChange(Sender: TObject);
begin
  cbIconsSize.Enabled := cbShowIcons.Checked;
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
  if FindFirstEx(gpLngDir+'*.po', faAnyFile, fr)<>0 then
  begin
    FindClose(fr);
    Exit;
  end;
  repeat
    sLangName := GetLanguageName(gpLngDir + fr.Name);
    lngList.Items.Add(Format('%s = (%s)', [fr.Name, sLangName]));
  until FindNextEx(fr)<>0;
  
  FindClose(fr);

  iIndex:=lngList.Items.IndexOfName(gPOFileName + #32);
  if iIndex>=0 then
    lngList.Selected[iIndex]:=True;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  // save all configuration
  SaveConfig;
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  // save all configuration
  SaveConfig;
  // write to ini file
  SaveGlobs;
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
begin
  cbMainFont.Text := gFontName;
  cbViewerFont.Text := gViewerFontName;
  cbEditorFont.Text := gEditorFontName;
    
  edtEditorSize.Value:=gEditorFontSize;
  edtViewerSize.Value:=gViewerFontSize;
  edtMainSize.Value:=gFontSize;
  
  with edtTest1.Font do
  begin
    Name := gFontName;
    Size := gFontSize;
    Style:= gFontStyle;
  end; // with
  
  with edtTest2.Font do
  begin
    Name := gEditorFontName;
    Size := gEditorFontSize;
    Style:= gEditorFontStyle;
  end; // with
  
  with edtTest3.Font do
  begin
    Name := gViewerFontName;
    Size := gViewerFontSize;
    Style:= gViewerFontStyle;
  end; // with
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
  vShortCut := ShortCutEx(Key,GetKeyShiftStateEx);
  TEdit(Sender).Text := ShortCutToTextEx(vShortCut);
  Key := 0;
  btSetHotKey.Enabled := (edHotKey.Text <> '');
  btClearHotKey.Enabled := (edHotKey.Text <> '');
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  if Assigned(tmpDSXPlugins) then
    FreeAndNil(tmpDSXPlugins);
  if Assigned(tmpWCXPlugins) then
    FreeAndNil(tmpWCXPlugins);
  if Assigned(tmpWDXPlugins) then
    FreeAndNil(tmpWDXPlugins);
  if Assigned(tmpWFXPlugins) then
    FreeAndNil(tmpWFXPlugins);
  if Assigned(tmpWLXPlugins) then
    FreeAndNil(tmpWLXPlugins);
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
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbMarkColor.Color;
            Font.Color := cbTextColor.Color;
            sText := 'Mark';
         end
       else
         begin
            Brush.Color := cbBackColor.Color;
            Font.Color := cbMarkColor.Color;
            sText := 'Mark';
         end;
      end;
    2:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbMarkColor.Color;
            Font.Color := cbTextColor.Color;
            sText := 'Mark';
         end
       else
         begin
            Brush.Color := cbBackColor2.Color;
            Font.Color := cbMarkColor.Color;
            sText := 'Mark';
         end;
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
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbCursorColor.Color;
            Font.Color :=InvertColor(cbCursorText.Color);
            sText := 'Mark + Cursor';
         end
       else
         begin
            Brush.Color := cbCursorColor.Color;
            Font.Color := cbMarkColor.Color;
            sText := 'Mark + Cursor';
         end;
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

procedure TfrmOptions.pcPluginsTypesChange(Sender: TObject);
begin
  if stgPlugins.RowCount > 1 then
    stgPluginsBeforeSelection(stgPlugins, 0, 1);
end;

procedure TfrmOptions.pgBehavResize(Sender: TObject);
var
  iWidth: Integer;
begin
  iWidth:= (pgBehav.Width div 2) - 26;
  gbMisc1.Width:= iWidth;
  gbDateTimeFormat.Width:= iWidth;
end;

{ Plugins }

procedure TfrmOptions.stgPluginsBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  if stgPlugins.Cells[0, aRow] = '+' then
    btnEnablePlugin.Caption:= rsOptDisable
  else if stgPlugins.Cells[0, aRow] = '-' then
    btnEnablePlugin.Caption:= rsOptEnable;

  btnEnablePlugin.Enabled:= (stgPlugins.Cells[0, aRow] <> '');
end;

procedure TfrmOptions.btnEnablePluginClick(Sender: TObject);
var
  sExt,
  sExts: String;
  iPluginIndex: Integer;
  bEnabled: Boolean;
begin
  if stgPlugins.RowCount <= 1 then Exit;
  if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      bEnabled:= not tmpWCXPlugins.Enabled[stgPlugins.Row - 1];
      sExts:= stgPlugins.Cells[2, stgPlugins.Row];
      sExt:= Copy2SpaceDel(sExts);
      repeat
        //DebugLn('Extension = ', sExt);
        iPluginIndex:= tmpWCXPlugins.IndexOfName(sExt);
        tmpWCXPlugins.Enabled[iPluginIndex]:= bEnabled;
        sExt:= Copy2SpaceDel(sExts);
      until sExt = '';
      stgPlugins.Cells[0, stgPlugins.Row]:= IfThen(bEnabled, '+', '-');
      btnEnablePlugin.Caption:= IfThen(bEnabled, rsOptDisable, rsOptEnable);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    begin
      bEnabled:= not tmpWFXPlugins.Enabled[stgPlugins.Row - 1];
      stgPlugins.Cells[0, stgPlugins.Row]:= IfThen(bEnabled, '+', '-');
      tmpWFXPlugins.Enabled[stgPlugins.Row - 1]:= bEnabled;
      btnEnablePlugin.Caption:= IfThen(bEnabled, rsOptDisable, rsOptEnable);
    end;
end;

procedure TfrmOptions.btnRemovePluginClick(Sender: TObject);
begin
  if stgPlugins.Row <= 0 then Exit; // no plugins

  if pcPluginsTypes.ActivePage.Name = 'tsDSX' then
    begin
      tmpDSXPlugins.DeleteItem(stgPlugins.Row - 1);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      tmpWCXPlugins.Delete(stgPlugins.Row - 1);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWDX' then
    begin
      tmpWDXPlugins.DeleteItem(stgPlugins.Row - 1);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    begin
      tmpWFXPlugins.Delete(stgPlugins.Row - 1);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWLX' then
    begin
      tmpWLXPlugins.DeleteItem(stgPlugins.Row - 1);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
end;

procedure TfrmOptions.btnTweakPluginClick(Sender: TObject);
var
  ptPluginType: TPluginType;
  iPluginIndex: Integer;
begin
  iPluginIndex:= stgPlugins.Row - 1;
  if pcPluginsTypes.ActivePage.Name = 'tsDSX' then
    ptPluginType:= ptDSX
  else if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      ptPluginType:= ptWCX;
      // get plugin index
      iPluginIndex:= tmpWCXPlugins.IndexOfName(Copy2Space(stgPlugins.Cells[2, stgPlugins.Row]));
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWDX' then
    ptPluginType:= ptWDX
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    ptPluginType:= ptWFX
  else if pcPluginsTypes.ActivePage.Name = 'tsWLX' then
    ptPluginType:= ptWLX;

  if ShowTweakPluginDlg(ptPluginType, iPluginIndex) then
    pcPluginsTypes.ActivePage.OnShow(pcPluginsTypes.ActivePage); // update info in plugin list
end;

procedure TfrmOptions.btnConfigPluginClick(Sender: TObject);
var
  WCXmodule: TWCXmodule;
  WFXmodule: TWFXmodule;
begin
  if stgPlugins.Row <= 0 then Exit; // no plugins

  if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      WCXmodule := TWCXmodule.Create;
      DebugLn('TWCXmodule created');
      if WCXmodule.LoadModule(GetCmdDirFromEnvVar(tmpWCXPlugins.FileName[stgPlugins.Row - 1])) then
       begin
         DebugLn('WCXModule Loaded');
         WCXmodule.VFSConfigure(Handle);
         DebugLn('Dialog executed');
         WCXModule.UnloadModule;
         DebugLn('WCX Module Unloaded');
         WCXmodule.Free;
         DebugLn('WCX Freed');
       end;
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    begin
      WFXmodule := TWFXmodule.Create;
      DebugLn('TWFXmodule created');
      if WFXmodule.LoadModule(GetCmdDirFromEnvVar(tmpWFXPlugins.FileName[stgPlugins.Row - 1])) then
       begin
         DebugLn('WFXModule Loaded');
         WFXmodule.VFSConfigure(Handle);
         DebugLn('Dialog executed');
         WFXModule.UnloadModule;
         DebugLn('WFX Module Unloaded');
         WFXmodule.Free;
         DebugLn('WFX Freed');
       end;
    end;
end;

{ DSX plugins }

procedure TfrmOptions.btnDSXAddClick(Sender: TObject);
var
  I, J: Integer;
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Search plugins (*.dsx)|*.dsx';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractFileName(odOpenDialog.FileName);
      Delete(sPluginName,length(sPluginName)-4,4);
      I:= tmpDSXPlugins.Add(sPluginName,odOpenDialog.FileName,'');

      stgPlugins.RowCount:= stgPlugins.RowCount + 1;
      J:= stgPlugins.RowCount-1;
      stgPlugins.Cells[1, J]:= tmpDSXPlugins.GetDsxModule(I).Name;
      stgPlugins.Cells[2, J]:= tmpDSXPlugins.GetDsxModule(I).Descr;
      stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpDSXPlugins.GetDsxModule(I).FileName);
    end;
end;

procedure TfrmOptions.tsDSXShow(Sender: TObject);
var i:integer;
begin
  btnAddPlugin.OnClick:= @btnDSXAddClick;
  stgPlugins.RowCount:= tmpDSXPlugins.Count+1;
  if tmpDSXPlugins.Count=0 then exit;

  for i:=0 to tmpDSXPlugins.Count-1 do
    begin
    stgPlugins.Cells[1, I+1]:= tmpDSXPlugins.GetDsxModule(i).Name;
    stgPlugins.Cells[2, I+1]:= tmpDSXPlugins.GetDsxModule(i).Descr;
    stgPlugins.Cells[3, I+1]:= SetCmdDirAsEnvVar(tmpDSXPlugins.GetDsxModule(i).FileName);
    end;
end;

{ WCX plugins }

procedure TfrmOptions.btnWCXAddClick(Sender: TObject);
var
  I, J: Integer;
  sExt,
  sPluginName : String;
  WCXmodule : TWCXmodule;
begin
  odOpenDialog.Filter := 'Archive plugins (*.wcx)|*.wcx';
  if odOpenDialog.Execute then
    begin
      WCXmodule := TWCXmodule.Create;
      if WCXmodule.LoadModule(odOpenDialog.FileName)then
        sPluginName := IntToStr(WCXmodule.VFSMisc) + ',' + SetCmdDirAsEnvVar(odOpenDialog.FileName)
      else
        sPluginName := '0,' + SetCmdDirAsEnvVar(odOpenDialog.FileName);

      if InputQuery(rsOptEnterExt, Format(rsOptAssocPluginWith, [odOpenDialog.FileName]), sExt) then
        begin
          I:= tmpWCXPlugins.AddObject(sExt + '=' + sPluginName, TObject(True));

          stgPlugins.RowCount:= stgPlugins.RowCount + 1;
          J:= stgPlugins.RowCount-1;
          stgPlugins.Cells[0, J]:= '+';
          stgPlugins.Cells[1, J]:= ExtractOnlyFileName(tmpWCXPlugins.FileName[I]);
          stgPlugins.Cells[2, J]:= tmpWCXPlugins.Ext[I];
          stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpWCXPlugins.FileName[I]);
        end;

      WCXModule.UnloadModule;
      WCXmodule.Free;
    end;
end;

procedure TfrmOptions.tsWCXShow(Sender: TObject);
var
  I,
  iIndex: Integer;
  sFileName,
  sExt: String;
  iRow: Integer;
begin
  btnAddPlugin.OnClick:= @btnWCXAddClick;
  iRow:= 0;
  for I := 0 to tmpWCXPlugins.Count - 1 do
  begin
    // get associated extension
    sExt := tmpWCXPlugins.Ext[I];

    //get file name
    sFileName:= tmpWCXPlugins.FileName[I];

    iIndex:= stgPlugins.Cols[3].IndexOf(sFileName);
    if iIndex < 0 then
      begin
        Inc(iRow);
        stgPlugins.RowCount:= iRow+1;
        stgPlugins.Cells[1, iRow]:= ExtractOnlyFileName(sFileName);
        stgPlugins.Cells[2, iRow]:= sExt + #32;
      end
    else
      begin
        stgPlugins.Cells[2, iIndex]:= stgPlugins.Cells[2, iIndex] + sExt + #32;
        Continue;
      end;


    if tmpWCXPlugins.Enabled[I] then // enabled
      begin
        stgPlugins.Cells[3, iRow]:= sFileName;
        stgPlugins.Cells[0, iRow]:= '+';
      end
    else // disabled
      begin
        stgPlugins.Cells[3, iRow]:= sFileName;
        stgPlugins.Cells[0, iRow]:= '-';
      end;
  end;
  if stgPlugins.RowCount > 1 then
    stgPluginsBeforeSelection(stgPlugins, 0, 1);
end;

{ WDX plugins }

procedure TfrmOptions.btnWDXAddClick(Sender: TObject);
var
  I, J: Integer;
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Content plugins (*.wdx; *.lua)|*.wdx;*.lua';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractFileName(odOpenDialog.FileName);
      delete(sPluginName,length(sPluginName)-4,4);
      I:= tmpWDXPlugins.Add(sPluginName,odOpenDialog.FileName,'');

      tmpWDXPlugins.LoadModule(sPluginName);
      tmpWDXPlugins.GetWdxModule(sPluginName).DetectStr:=tmpWDXPlugins.GetWdxModule(sPluginName).CallContentGetDetectString;

      stgPlugins.RowCount:= stgPlugins.RowCount + 1;
      J:= stgPlugins.RowCount-1;
      stgPlugins.Cells[1, J]:= tmpWDXPlugins.GetWdxModule(I).Name;
      stgPlugins.Cells[2, J]:= tmpWDXPlugins.GetWdxModule(I).DetectStr;
      stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpWDXPlugins.GetWdxModule(I).FileName);
    end;
end;

procedure TfrmOptions.tsWDXShow(Sender: TObject);
var i:integer;
begin
  btnAddPlugin.OnClick:= @btnWDXAddClick;
  stgPlugins.RowCount:= tmpWDXPlugins.Count+1;
  if tmpWDXPlugins.Count=0 then exit;

  for i:=0 to tmpWDXPlugins.Count-1 do
    begin
    stgPlugins.Cells[1, I+1]:= tmpWDXPlugins.GetWdxModule(i).Name;
    stgPlugins.Cells[2, I+1]:= tmpWDXPlugins.GetWdxModule(i).DetectStr;
    stgPlugins.Cells[3, I+1]:= SetCmdDirAsEnvVar(tmpWDXPlugins.GetWdxModule(i).FileName);
    end;
end;

{ WFX plugins }

procedure TfrmOptions.btnWFXAddClick(Sender: TObject);
var
  I, J: Integer;
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
  I:= tmpWFXPlugins.AddObject(sPluginName, TObject(True));
  stgPlugins.RowCount:= tmpWFXPlugins.Count + 1;
  J:= stgPlugins.RowCount-1;
  stgPlugins.Cells[0, J]:= '+';
  stgPlugins.Cells[1, J]:= tmpWFXPlugins.Name[I];
  stgPlugins.Cells[2, J]:= '';
  stgPlugins.Cells[3, J]:= tmpWFXPlugins.FileName[I];
  DebugLn('WFX Item Added');
  WFXModule.UnloadModule;
  DebugLn('WFX Module Unloaded');
  WFXmodule.Free;
  DebugLn('WFX Freed');
  end;
end;

procedure TfrmOptions.tsWFXShow(Sender: TObject);
var
  I: Integer;
begin
  btnAddPlugin.OnClick:= @btnWFXAddClick;
  stgPlugins.RowCount:= tmpWFXPlugins.Count + 1;
  for I:= 0 to tmpWFXPlugins.Count - 1 do
  begin
    if tmpWFXPlugins.Enabled[I] then
      begin
        stgPlugins.Cells[1, I+1]:= tmpWFXPlugins.Name[I];
        stgPlugins.Cells[3, I+1]:= tmpWFXPlugins.FileName[I];
        stgPlugins.Cells[0, I+1]:= '+';
      end
    else
      begin
        stgPlugins.Cells[1, I+1]:= tmpWFXPlugins.Name[I];
        stgPlugins.Cells[3, I+1]:= tmpWFXPlugins.FileName[I];
        stgPlugins.Cells[0, I+1]:= '-';
      end;
    stgPlugins.Cells[2, I+1]:= '';
  end;
end;

{ WLX Plugins }

procedure TfrmOptions.btnWLXAddClick(Sender: TObject);
var
  I, J: Integer;
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Viewer plugins (*.wlx)|*.wlx';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractFileName(odOpenDialog.FileName);
      delete(sPluginName,length(sPluginName)-4,4);
      I:= tmpWLXPlugins.Add(sPluginName,odOpenDialog.FileName,'');

      tmpWLXPlugins.LoadModule(sPluginName);
      tmpWLXPlugins.GetWlxModule(sPluginName).DetectStr:=tmpWLXPlugins.GetWlxModule(sPluginName).CallListGetDetectString;

      stgPlugins.RowCount:= stgPlugins.RowCount + 1;
      J:= stgPlugins.RowCount-1;
      stgPlugins.Cells[1, J]:= tmpWLXPlugins.GetWlxModule(I).Name;
      stgPlugins.Cells[2, J]:= tmpWLXPlugins.GetWlxModule(I).DetectStr;
      stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpWLXPlugins.GetWlxModule(I).FileName);
    end;
end;

procedure TfrmOptions.tsWLXShow(Sender: TObject);
var i:integer;
begin
  btnAddPlugin.OnClick:= @btnWLXAddClick;
  stgPlugins.RowCount:= tmpWLXPlugins.Count+1;
  if tmpWLXPlugins.Count=0 then exit;

  for i:=0 to tmpWLXPlugins.Count-1 do
    begin
    stgPlugins.Cells[1, I+1]:= tmpWLXPlugins.GetWlxModule(i).Name;
    stgPlugins.Cells[2, I+1]:= tmpWLXPlugins.GetWlxModule(i).DetectStr;
    stgPlugins.Cells[3, I+1]:= SetCmdDirAsEnvVar(tmpWLXPlugins.GetWlxModule(i).FileName);
    end;
end;

{ File lbtypes category color }

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

procedure TfrmOptions.FillCommandsPage;
begin
actions.GetCategoriesList(lbxCategories.Items);
if lbxCategories.Items.Count>0 then
begin
  lbxCategories.ItemIndex:=0;
  lbxCategoriesSelectionChange(nil,false);
end;

//lbxCommands.items.AddStrings(actions.CommandList);
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

procedure TfrmOptions.lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxCategories.ItemIndex=-1 then exit;
  Actions.GetCommandsByCategory( lbxCategories.items.Strings[lbxCategories.ItemIndex],lbxCommands.items);
  lbxCommands.Sorted:= True;
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

{/ File lbtypes category color }

procedure TfrmOptions.btClearHotKeyClick(Sender: TObject);
var vActions: TAction; st:TStringList; cat:string; i:integer;
begin

 //TODO: delete hotkey.
 //TODO:New interface for hotkeys

//       edtParam.Text:='';
//       edHotKey.Text:='';

       
 if lbxCategories.ItemIndex=-1 then exit;
  cat:=lbxCategories.Items[lbxCategories.ItemIndex];
 if cat='Main' then
 begin
    i:=HotMan.GetHotKeyIndex(ShortCutToTextEx(vShortCut));
    if i=-1 then exit;
    st:=TStringList.Create;
     if HotMan.GetControlsListBy(ShortCutToTextEx(vShortCut),st)>0 then
       begin
         HotMan.DeleteHotKey(ShortCutToTextEx(vShortCut),frmMain);
       end;
    st.free;
 end;
end;

procedure TfrmOptions.btnBackColor2Click(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbBackColor2.Text := '';
     cbBackColor2.Color := optColorDialog.Color;
     pbExample.Repaint;
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
     pbExample.Repaint;
   end;
end;

procedure TfrmOptions.btnCursorTextClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbCursorText.Text := '';
     cbCursorText.Color := optColorDialog.Color;
     pbExample.Repaint;
   end;
end;

procedure TfrmOptions.btnMarkColorClick(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbMarkColor.Text := '';
     cbMarkColor.Color := optColorDialog.Color;
     pbExample.Repaint;
   end;
end;

procedure TfrmOptions.btnNewColumnsSetClick(Sender: TObject);
begin
  Application.CreateForm(TfColumnsSetConf, frmColumnsSetConf);
   // Create new Set
  frmColumnsSetConf.edtNameofColumnsSet.Text:='New Columns'+inttostr(ColSet.count);
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

procedure TfrmOptions.LoadConfig;
begin
  { Layout page }
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
  cbMinimizeToTray.Checked:= gTrayIcon;
  cbDateTimeFormat.Text:= gDateTimeFormat;
  lblDateTimeExample.Caption:= FormatDateTime(gDateTimeFormat, Now);
  cbRenameSelOnlyName.Checked:= gRenameSelOnlyName;
  cbCutTextToColWidth.Checked:= gCutTextToColWidth;
  ledDriveBlackList.Text:= gDriveBlackList;

  fneExtEditor.FileName := gExtEdit;
  fneExtViewer.FileName := gExtView;
  fneExtDiffer.FileName := gExtDiff;

  fneExtEditor.Enabled:= cbExtEditor.Checked;
  fneExtDiffer.Enabled:= cbExtDiffer.Checked;
  fneExtViewer.Enabled:= cbExtViewer.Checked;

  edtRunTerm.Text:=gRunTerm;


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
  cbbUseInvertedSelection.Checked:=gUseInvertedSelection;

  { File operations }
  edtCopyBufferSize.Text:= IntToStr(gCopyBlockSize div 1024);
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
  rbUseMmapInSearch.Checked := gUseMmapInSearch;
  seWipePassNumber.Value:= gWipePassNumber;
  cbProcessComments.Checked:= gProcessComments;

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
  cbTabsConfirmCloseAll.Checked:= Boolean(gDirTabOptions and tb_confirm_close_all);
  cbTabsOpenForeground.Checked:= Boolean(gDirTabOptions and tb_open_new_in_foreground);
  cbTabsLockedAsterisk.Checked:= Boolean(gDirTabOptions and tb_show_asterisk_for_locked);
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
  cbIconsSize.Text := IntToStr(gNewIconsSize) + 'x' + IntToStr(gNewIconsSize);

  FillLngListBox;
  FillFontLists;
  FillFileColorsList;
  DebugLn(gRunInTerm);
  edtRunInTerm.Text:= gRunInTerm;


   FillColumnsList;
   
   FillCommandsPage;
  { Fill plugins lists }
  tmpDSXPlugins.Load(gIni);
  tmpWCXPlugins.Assign(gWCXPlugins);
  tmpWDXPlugins.Load(gIni);
  tmpWFXPlugins.Assign(gWFXPlugins);
  tmpWLXPlugins.Load(gIni);
end;

procedure TfrmOptions.SaveConfig;
begin
  { Layout page }
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
  
  gRunInTerm:=edtRunInTerm.Text;
  gDirSelect:=cbDirSelect.Checked;
  gCaseSensitiveSort:=cbCaseSensitiveSort.Checked;
  gLynxLike:=cbLynxLike.Checked;
  gShortFileSizeFormat:=cbShortFileSizeFormat.Checked;
  gScrollMode := rgScrolling.ItemIndex;
  gTrayIcon:= cbMinimizeToTray.Checked;
  gDateTimeFormat := cbDateTimeFormat.Text;
  gRenameSelOnlyName:= cbRenameSelOnlyName.Checked;
  gCutTextToColWidth:= cbCutTextToColWidth.Checked;
  gDriveBlackList:= ledDriveBlackList.Text;
  
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

  gEditorFontName:=cbEditorFont.Text;
  gViewerFontName:=cbViewerFont.Text;
  
  {$hints off}
  gEditorFontSize:=Round(edtEditorSize.Value);
  gViewerFontSize:=Round(edtViewerSize.Value);
  gFontSize:=Round(edtMainSize.Value);
  {$hints on}

  gFontStyle:= EdtTest1.Font.Style;
  gEditorFontStyle:= EdtTest2.Font.Style;
  gViewerFontStyle:= EdtTest3.Font.Style;

  { Colors }
  gForeColor := cbTextColor.Color;
  gBackColor := cbBackColor.Color; // background color
  gBackColor2 := cbBackColor2.Color;
  gMarkColor := cbMarkColor.Color;
  gCursorColor := cbCursorColor.Color;
  gCursorText := cbCursorText.Color;
  gUseInvertedSelection:=cbbUseInvertedSelection.Checked;
  
  gShowIcons := cbShowIcons.Checked;

  { File operations }
  gCopyBlockSize := StrToIntDef(edtCopyBufferSize.Text, gCopyBlockSize) * 1024;
  gDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;
  gUseMmapInSearch := rbUseMmapInSearch.Checked;
  gWipePassNumber:= seWipePassNumber.Value;
  gProcessComments:= cbProcessComments.Checked;
  
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
  if cbTabsConfirmCloseAll.Checked then
    gDirTabOptions := (gDirTabOptions or tb_confirm_close_all);
  if cbTabsOpenForeground.Checked then
    gDirTabOptions := (gDirTabOptions or tb_open_new_in_foreground);
  if cbTabsLockedAsterisk.Checked then
    gDirTabOptions := (gDirTabOptions or tb_show_asterisk_for_locked);

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

//-------------------------------------------------
  if (gNewIconsSize <> StrToInt(Copy(cbIconsSize.Text, 1, 2))) or ((lngList.ItemIndex>-1) and
    (Trim(gPOFileName) <> Trim(lngList.Items.Names[lngList.ItemIndex]))) then
    begin
      gNewIconsSize:= StrToInt(Copy(cbIconsSize.Text, 1, 2)); // new file panel icons size
      if lngList.ItemIndex > -1 then
        gPOFileName:= lngList.Items.Names[lngList.ItemIndex]; // new language file
      msgOk(rsMsgRestartForApplyChanges);
    end;
//-------------------------------------------------

  frmMain.UpdateWindowView;
  frmMain.Repaint; // for panels repaint
  frmMain.SaveShortCuts;
  
  {Columns Set}
  ColSet.Save(gIni);

  DebugLn('Save plugins');
  { Save plugins lists }
  tmpDSXPlugins.Save(gIni);
  gWCXPlugins.Assign(tmpWCXPlugins);
  gWDXPlugins.Assign(tmpWDXPlugins);
  gWFXPlugins.Assign(tmpWFXPlugins);
  tmpWLXPlugins.Save(gIni);
end;

initialization
 {$I fOptions.lrs}

end.
