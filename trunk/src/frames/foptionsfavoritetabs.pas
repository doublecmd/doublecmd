{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of Favorite Tabs

   Copyright (C) 2016-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsFavoriteTabs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus,
  Dialogs, ComCtrls, uFavoriteTabs, types, fOptionsFrame;
type
  { TfrmOptionsFavoriteTabs }
  TfrmOptionsFavoriteTabs = class(TOptionsEditor)
    btnRename: TBitBtn;
    cbExistingTabsToKeep: TComboBox;
    cbFullExpandTree: TCheckBox;
    cbSaveDirHistory: TComboBox;
    cbTargetPanelLeftSavedTabs: TComboBox;
    cbTargetPanelRightSavedTabs: TComboBox;
    gbFavoriteTabs: TGroupBox;
    gbFavoriteTabsOtherOptions: TGroupBox;
    gpSavedTabsRestorationAction: TGroupBox;
    lblExistingTabsToKeep: TLabel;
    lblSaveDirHistory: TLabel;
    lblTargetPanelLeftSavedTabs: TLabel;
    lblTargetPanelRightSavedTabs: TLabel;
    MenuItem1: TMenuItem;
    miImportLegacyTabFilesAtPos1: TMenuItem;
    miImportLegacyTabFilesInSubAtPos: TMenuItem;
    miImportLegacyTabFilesAccSetting: TMenuItem;
    miSeparator1: TMenuItem;
    miSeparator11: TMenuItem;
    miExportToLegacyTabsFile: TMenuItem;
    miImportLegacyTabFilesAtPos: TMenuItem;
    miRename: TMenuItem;
    miInsertSeparator: TMenuItem;
    MenuItem2: TMenuItem;
    miInsertSubMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    pnlClient: TPanel;
    tvFavoriteTabs: TTreeView;
    pnlButtons: TPanel;
    btnInsert: TBitBtn;
    btnDelete: TBitBtn;
    btnImportExport: TBitBtn;
    btnAdd: TBitBtn;
    btnSort: TBitBtn;
    pmFavoriteTabsTestMenu: TPopupMenu;
    miFavoriteTabsTestMenu: TMenuItem;
    pmTreeView: TPopupMenu;
    miAddSeparator2: TMenuItem;
    miAddSubmenu2: TMenuItem;
    miSeparator7: TMenuItem;
    miDeleteSelectedEntry2: TMenuItem;
    miSeparator8: TMenuItem;
    miSortSingleGroup2: TMenuItem;
    miSeparator9: TMenuItem;
    miCutSelection: TMenuItem;
    miPasteSelection: TMenuItem;
    pmInsertAddToFavoriteTabs: TPopupMenu;
    miAddSeparator: TMenuItem;
    miAddSubmenu: TMenuItem;
    pmDeleteFavoriteTabs: TPopupMenu;
    miDeleteSelectedEntry: TMenuItem;
    miSeparator2: TMenuItem;
    miDeleteJustSubMenu: TMenuItem;
    miDeleteCompleteSubMenu: TMenuItem;
    miSeparator3: TMenuItem;
    miDeleteAllFavoriteTabs: TMenuItem;
    pmImportExport: TPopupMenu;
    miTestResultingFavoriteTabsMenu: TMenuItem;
    miSeparator10: TMenuItem;
    miOpenAllBranches: TMenuItem;
    miCollapseAll: TMenuItem;
    pmSortFavoriteTabsList: TPopupMenu;
    miSortSingleGroup: TMenuItem;
    miCurrentLevelOfItemOnly: TMenuItem;
    miSortSingleSubMenu: TMenuItem;
    miSortSubMenuAndSubLevel: TMenuItem;
    miSortEverything: TMenuItem;
    procedure btnRenameClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    function ActualAddFavoriteTabs(ParamDispatcher: TKindOfFavoriteTabsEntry; sFavoriteTabsName: string; InsertOrAdd: TNodeAttachMode): TTreeNode;
    function MySortViaGroup(Node1, Node2: TTreeNode): integer;
    procedure RecursiveSetGroupNumbers(ParamNode: TTreeNode; ParamGroupNumber: integer; DoRecursion, StopAtFirstGroup: boolean);
    function GetNextGroupNumber: integer;
    procedure ClearCutAndPasteList;
    function TryToGetExactFavoriteTabs(const index: integer): TTreeNode;
    procedure RefreshTreeView(NodeToSelect: TTreeNode);
    procedure tvFavoriteTabsDragDrop(Sender, {%H-}Source: TObject; X, Y: integer);
    procedure tvFavoriteTabsDragOver(Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure tvFavoriteTabsEnter(Sender: TObject);
    procedure tvFavoriteTabsExit(Sender: TObject);
    procedure tvFavoriteTabsSelectionChanged(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure cbFullExpandTreeChange(Sender: TObject);
    procedure cbTabsConfigChange(Sender: TObject);
    procedure lbleditFavoriteTabsEnter(Sender: TObject);
    procedure lbleditFavoriteTabsExit(Sender: TObject);
    procedure lbleditFavoriteTabsKeyPress(Sender: TObject; var Key: char);
    procedure miInsertAddFavoriteTabsClick(Sender: TObject);
    procedure miDeleteSelectedEntryClick(Sender: TObject);
    procedure miDeleteAllFavoriteTabsClick(Sender: TObject);
    procedure miSortFavoriteTabsClick(Sender: TObject);
    function MakeUsUpToDatePriorImportExport: boolean;
    procedure miExportToLegacyTabsFileClick(Sender: TObject);
    procedure miImportLegacyTabFilesClick(Sender: TObject);
    procedure miTestResultingFavoriteTabsMenuClick(Sender: TObject);
    procedure miShowWhereItWouldGo(Sender: TObject);
    procedure miOpenAllBranchesClick(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miCutSelectionClick(Sender: TObject);
    procedure miPasteSelectionClick(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  private
    { Private declarations }
    FavoriteTabsListTemp: TFavoriteTabsList;
    CutAndPasteIndexList: TStringList;
    GlobalGroupNumber: integer;
  public
    { Public declarations }
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    destructor Destroy; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
    function ExtraOptionsSignature(CurrentSignature:dword):dword; override;
    procedure MakeUsInPositionToWorkWithActiveFavoriteTabs;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LCLProc, LCLIntf,

  //DC
  DCStrUtils, uGlobs, uLng, uDCUtils, uDebug, fmain, uShowMsg, DCOSUtils,
  uComponentsSignature;

{ TfrmOptionsFavoriteTabs.Init }
procedure TfrmOptionsFavoriteTabs.Init;
begin
  ParseLineToList(rsFavTabsPanelSideSelection, cbTargetPanelLeftSavedTabs.Items);
  ParseLineToList(rsFavTabsPanelSideSelection, cbTargetPanelRightSavedTabs.Items);
  ParseLineToList(rsFavTabsPanelSideSelection, cbExistingTabsToKeep.Items);
  ParseLineToList(rsFavTabsSaveDirHistory, cbSaveDirHistory.Items);
  OpenDialog.Filter := ParseLineToFileFilter([rsFilterLegacyTabFiles, '*.tab', rsFilterAnyFiles, '*.*']);
end;

{ TfrmOptionsFavoriteTabs.Load }
procedure TfrmOptionsFavoriteTabs.Load;
begin
  gpSavedTabsRestorationAction.Visible := gFavoriteTabsUseRestoreExtraOptions;
  cbFullExpandTree.Checked := gFavoriteTabsFullExpandOrNot;

  CutAndPasteIndexList := TStringList.Create;
  CutAndPasteIndexList.Sorted := True;
  CutAndPasteIndexList.Duplicates := dupAccept;

  if FavoriteTabsListTemp = nil then
  begin
    FavoriteTabsListTemp := TFavoriteTabsList.Create;
    gFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(FavoriteTabsListTemp);
  end;

  tvFavoriteTabs.Images := frmMain.imgLstDirectoryHotlist;
  FavoriteTabsListTemp.LoadTTreeView(tvFavoriteTabs);
  cbFullExpandTreeChange(cbFullExpandTree);
  if tvFavoriteTabs.Items.Count > 0 then tvFavoriteTabs.Items[0].Selected := True; //Select at least first one by default
end;

{ TfrmOptionsFavoriteTabs.Save }
function TfrmOptionsFavoriteTabs.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
  FavoriteTabsListTemp.CopyFavoriteTabsListToFavoriteTabsList(gFavoriteTabsList);
  gFavoriteTabsList.RefreshXmlFavoriteTabsListSection;
  gFavoriteTabsList.RefreshAssociatedMainMenu;

  gFavoriteTabsFullExpandOrNot := cbFullExpandTree.Checked;

  cbFullExpandTreeChange(cbFullExpandTree);
end;

{ TfrmOptionsFavoriteTabs.GetIconIndex }
class function TfrmOptionsFavoriteTabs.GetIconIndex: integer;
begin
  Result := 37;
end;

{ TfrmOptionsFavoriteTabs.GetTitle }
class function TfrmOptionsFavoriteTabs.GetTitle: string;
begin
  Result := rsOptionsEditorFavoriteTabs;
end;

{ TfrmOptionsFavoriteTabs.Destroy }
destructor TfrmOptionsFavoriteTabs.Destroy;
begin
  CutAndPasteIndexList.Free;
  inherited Destroy;
end;

{ TfrmOptionsFavoriteTabs.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsFavoriteTabs.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  result := False;
end;

{ TfrmOptionsFavoriteTabs.ExtraOptionsSignature }
function TfrmOptionsFavoriteTabs.ExtraOptionsSignature(CurrentSignature:dword):dword;
begin
  FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
  result := FavoriteTabsListTemp.ComputeSignature(CurrentSignature);
  result := ComputeSignatureSingleComponent(cbFullExpandTree, result);
end;

{ TfrmOptionsFavoriteTabs.MakeUsInPositionToWorkWithActiveFavoriteTabs }
procedure TfrmOptionsFavoriteTabs.MakeUsInPositionToWorkWithActiveFavoriteTabs;
var
  NodeToSelect: TTreeNode = nil;
begin
  NodeToSelect := TryToGetExactFavoriteTabs(FavoriteTabsListTemp.GetIndexLastFavoriteTabsLoaded);

  if (NodeToSelect = nil) and (tvFavoriteTabs.Items.Count > 0) then
    NodeToSelect := tvFavoriteTabs.Items.Item[0];

  RefreshTreeView(NodeToSelect);

  if not tvFavoriteTabs.Focused then
    if tvFavoriteTabs.CanFocus then
      tvFavoriteTabs.SetFocus;
end;

function CompareStringsFromTStringList(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := CompareStrings(List.Strings[Index1], List.Strings[Index2], gSortNatural, gSortSpecial, gSortCaseSensitivity);
end;

{ TfrmOptionsFavoriteTabs.btnRenameClick }
procedure TfrmOptionsFavoriteTabs.btnRenameClick(Sender: TObject);
var
  sInputText: string;
  FlagDoModif: boolean;
begin
  if tvFavoriteTabs.Selected <> nil then
  begin
    if TFavoriteTabs(tvFavoriteTabs.Selected.Data).Dispatcher in [fte_ACTUALFAVTABS, fte_STARTMENU] then
    begin
      sInputText := TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsName;
      case TFavoriteTabs(tvFavoriteTabs.Selected.Data).Dispatcher of
        fte_ACTUALFAVTABS: FlagDoModif := InputQuery(rsTitleRenameFavTabs, rsMsgRenameFavTabs, sInputText);
        fte_STARTMENU: FlagDoModif := InputQuery(rsTitleRenameFavTabsMenu, rsMsgRenameFavTabsMenu, sInputText);
      end;

      sInputText := Trim(sInputText);
      if FlagDoModif and (length(sInputText) > 0) then
      begin
        TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsName := sInputText;
        tvFavoriteTabs.Selected.Text := sInputText;
      end;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.FrameEnter }
procedure TfrmOptionsFavoriteTabs.FrameEnter(Sender: TObject);
begin
  if gpSavedTabsRestorationAction.Visible <> gFavoriteTabsUseRestoreExtraOptions then
    gpSavedTabsRestorationAction.Visible := gFavoriteTabsUseRestoreExtraOptions;
end;

{ TfrmOptionsFavoriteTabs.ActualAddFavoriteTabs }
function TfrmOptionsFavoriteTabs.ActualAddFavoriteTabs(ParamDispatcher: TKindOfFavoriteTabsEntry; sFavoriteTabsName: string; InsertOrAdd: TNodeAttachMode): TTreeNode;
var
  LocalFavoriteTabs: TFavoriteTabs;
  WorkingTreeNode: TTreeNode;
begin
  ClearCutAndPasteList;
  LocalFavoriteTabs := TFavoriteTabs.Create;
  LocalFavoriteTabs.Dispatcher := ParamDispatcher;
  LocalFavoriteTabs.FavoriteTabsName := sFavoriteTabsName;
  LocalFavoriteTabs.DestinationForSavedLeftTabs := gDefaultTargetPanelLeftSaved;
  LocalFavoriteTabs.DestinationForSavedRightTabs := gDefaultTargetPanelRightSaved;
  LocalFavoriteTabs.ExistingTabsToKeep := gDefaultExistingTabsToKeep;

  FavoriteTabsListTemp.Add(LocalFavoriteTabs);
  WorkingTreeNode := tvFavoriteTabs.Selected;
  if WorkingTreeNode <> nil then
    Result := tvFavoriteTabs.Items.AddNode(nil, WorkingTreeNode, sFavoriteTabsName, LocalFavoriteTabs, InsertOrAdd)
  else
    Result := tvFavoriteTabs.Items.AddNode(nil, nil, sFavoriteTabsName, LocalFavoriteTabs, naAddFirst);
end;

{ TfrmOptionsFavoriteTabs.MySortViaGroup }
function TfrmOptionsFavoriteTabs.MySortViaGroup(Node1, Node2: TTreeNode): integer;
begin
  if (TFavoriteTabs(Node1.Data).GroupNumber = TFavoriteTabs(Node2.Data).GroupNumber) and (TFavoriteTabs(Node1.Data).GroupNumber <> 0) then
  begin
    Result := CompareStrings(TFavoriteTabs(Node1.Data).FavoriteTabsName, TFavoriteTabs(Node2.Data).FavoriteTabsName, gSortNatural, gSortSpecial, gSortCaseSensitivity);
  end
  else
  begin
    if Node1.AbsoluteIndex < Node2.AbsoluteIndex then
      Result := -1
    else
      Result := 1;
  end;
end;

{ TfrmOptionsFavoriteTabs.RecursiveSetGroupNumbers }
// WARNING! This procedure calls itself.
procedure TfrmOptionsFavoriteTabs.RecursiveSetGroupNumbers(ParamNode: TTreeNode; ParamGroupNumber: integer; DoRecursion, StopAtFirstGroup: boolean);
var
  MaybeChild: TTreeNode;
begin
  repeat
    if DoRecursion then
    begin
      MaybeChild := ParamNode.GetFirstChild;
      if MaybeChild <> nil then
        RecursiveSetGroupNumbers(MaybeChild, GetNextGroupNumber, DoRecursion, StopAtFirstGroup);
    end;

    if TFavoriteTabs(ParamNode.Data).Dispatcher <> fte_SEPARATOR then
    begin
      TFavoriteTabs(ParamNode.Data).GroupNumber := ParamGroupNumber;
    end
    else
    begin
      ParamGroupNumber := GetNextGroupNumber;
      if StopAtFirstGroup then
        while ParamNode <> nil do
          ParamNode := ParamNode.GetNextSibling; //To exit the loop!
    end;

    if ParamNode <> nil then
      ParamNode := ParamNode.GetNextSibling;
  until ParamNode = nil;
end;

{ TfrmOptionsFavoriteTabs.GetNextGroupNumber }
function TfrmOptionsFavoriteTabs.GetNextGroupNumber: integer;
begin
  GlobalGroupNumber := GlobalGroupNumber + 1;
  Result := GlobalGroupNumber;
end;

{ TfrmOptionsFavoriteTabs.ClearCutAndPasteList }
procedure TfrmOptionsFavoriteTabs.ClearCutAndPasteList;
begin
  CutAndPasteIndexList.Clear;
  miPasteSelection.Enabled := True;
end;

{ TfrmOptionsFavoriteTabs.TryToGetExactFavoriteTabs }
function TfrmOptionsFavoriteTabs.TryToGetExactFavoriteTabs(const index: integer): TTreeNode;
var
  SearchingtvIndex: integer;
begin
  Result := nil;

  if (index >= 0) and (index < FavoriteTabsListTemp.Count) then
  begin
    SearchingtvIndex := 0;
    while (SearchingtvIndex < tvFavoriteTabs.Items.Count) and (Result = nil) do
    begin
      if tvFavoriteTabs.Items[SearchingtvIndex].Data = FavoriteTabsListTemp.Items[Index] then
        Result := tvFavoriteTabs.Items[SearchingtvIndex]
      else
        Inc(SearchingtvIndex);
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.RefreshTreeView }
procedure TfrmOptionsFavoriteTabs.RefreshTreeView(NodeToSelect: TTreeNode);
begin
  if NodeToSelect <> nil then
  begin
    tvFavoriteTabs.ClearSelection(False);
    NodeToSelect.Selected := True;
  end
  else
  begin
    tvFavoriteTabsSelectionChanged(tvFavoriteTabs); //At least to hide path, target, etc.
  end;
end;

{ TfrmOptionsFavoriteTabs.tvFavoriteTabsDragDrop }
procedure TfrmOptionsFavoriteTabs.tvFavoriteTabsDragDrop(Sender, Source: TObject; X, Y: integer);
var
  Index: longint;
  DestinationNode: TTreeNode;
begin
  DestinationNode := tvFavoriteTabs.GetNodeAt(X, Y);

  if Assigned(DestinationNode) and (tvFavoriteTabs.SelectionCount > 0) then
  begin
    //If we move toward the end, we place the moved item *after* the destination.
    //If we move toward the beginning, we place the moved item *before* the destination.
    if tvFavoriteTabs.Selections[pred(tvFavoriteTabs.SelectionCount)].AbsoluteIndex > DestinationNode.AbsoluteIndex then
    begin
      for Index := 0 to pred(tvFavoriteTabs.SelectionCount) do
      begin
        tvFavoriteTabs.Selections[Index].MoveTo(DestinationNode, naInsert);
      end;
    end
    else
    begin
      for Index := 0 to pred(tvFavoriteTabs.SelectionCount) do
      begin
        tvFavoriteTabs.Selections[Index].MoveTo(DestinationNode, naInsertBehind);
      end;
    end;
    ClearCutAndPasteList;
  end;

  miPasteSelection.Enabled := False;
end;

{ TfrmOptionsFavoriteTabs.tvFavoriteTabsDragOver }
procedure TfrmOptionsFavoriteTabs.tvFavoriteTabsDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

{ TfrmOptionsFavoriteTabs.tvFavoriteTabsEnter }
// To help to catch eye's attention, let's change color of selection when tree get/lose the focus
procedure TfrmOptionsFavoriteTabs.tvFavoriteTabsEnter(Sender: TObject);
begin
  tvFavoriteTabs.SelectionColor := clHighlight;
end;

{ TfrmOptionsFavoriteTabs.tvFavoriteTabsExit }
// To help to catch eye's attention, let's change color of selection when tree get/lose the focus
procedure TfrmOptionsFavoriteTabs.tvFavoriteTabsExit(Sender: TObject);
begin
  tvFavoriteTabs.SelectionColor := clBtnShadow;
end;

{ TfrmOptionsFavoriteTabs.tvFavoriteTabsSelectionChanged }
procedure TfrmOptionsFavoriteTabs.tvFavoriteTabsSelectionChanged(Sender: TObject);
var
  WorkingPointer: Pointer;
begin
  if tvFavoriteTabs.Selected <> nil then
  begin
    WorkingPointer := tvFavoriteTabs.Selected.Data;

    if TFavoriteTabs(WorkingPointer).Dispatcher = fte_ACTUALFAVTABS then
    begin
      cbTargetPanelLeftSavedTabs.ItemIndex := integer(TFavoriteTabs(WorkingPointer).DestinationForSavedLeftTabs);
      cbTargetPanelRightSavedTabs.ItemIndex := integer(TFavoriteTabs(WorkingPointer).DestinationForSavedRightTabs);
      cbExistingTabsToKeep.ItemIndex := integer(TFavoriteTabs(WorkingPointer).ExistingTabsToKeep);
      if TFavoriteTabs(WorkingPointer).SaveDirHistory then cbSaveDirHistory.ItemIndex := 1 else cbSaveDirHistory.ItemIndex := 0;
      gpSavedTabsRestorationAction.Enabled := True;
    end
    else
    begin
      gpSavedTabsRestorationAction.Enabled := False;
    end;

    miDeleteSelectedEntry.Enabled := not (TFavoriteTabs(WorkingPointer).Dispatcher = fte_STARTMENU);
    miDeleteJustSubMenu.Enabled := (TFavoriteTabs(WorkingPointer).Dispatcher = fte_STARTMENU);
    miDeleteCompleteSubMenu.Enabled := (TFavoriteTabs(WorkingPointer).Dispatcher = fte_STARTMENU);
    miSortSingleSubMenu.Enabled := (TFavoriteTabs(WorkingPointer).Dispatcher = fte_STARTMENU);
    miSortSubMenuAndSubLevel.Enabled := (TFavoriteTabs(WorkingPointer).Dispatcher = fte_STARTMENU);
    miDeleteSelectedEntry.Enabled := (TFavoriteTabs(WorkingPointer).Dispatcher <> fte_ENDMENU);
    miDeleteSelectedEntry2.Enabled := miDeleteSelectedEntry.Enabled;
  end //if tvFavoriteTabs.Selected<>nil then
  else
  begin
    gpSavedTabsRestorationAction.Enabled := False;
  end;
end;

{ TfrmOptionsFavoriteTabs.btnActionClick }
procedure TfrmOptionsFavoriteTabs.btnActionClick(Sender: TObject);
var
  Dispatcher: integer;
begin
  with Sender as TComponent do Dispatcher := tag;

  case Dispatcher of
    1, 2: pmInsertAddToFavoriteTabs.Tag := Dispatcher; //To help in routine to determine if it's a "Insert" or a "Add"
  end;

  case Dispatcher of
    1, 2: pmInsertAddToFavoriteTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    3: pmDeleteFavoriteTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    7: pmImportExport.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    8: pmSortFavoriteTabsList.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

{ TfrmOptionsFavoriteTabs.cbFullExpandTreeChange }
procedure TfrmOptionsFavoriteTabs.cbFullExpandTreeChange(Sender: TObject);
begin
  if cbFullExpandTree.Checked then tvFavoriteTabs.FullExpand else tvFavoriteTabs.FullCollapse;
end;

{ TfrmOptionsFavoriteTabs.cbTabsConfigChange }
procedure TfrmOptionsFavoriteTabs.cbTabsConfigChange(Sender: TObject);
begin
  if tvFavoriteTabs.Selected <> nil then
  begin
    if TFavoriteTabs(tvFavoriteTabs.Selected.Data).Dispatcher = fte_ACTUALFAVTABS then
    begin
      case TComponent(Sender).tag of
        1: TFavoriteTabs(tvFavoriteTabs.Selected.Data).DestinationForSavedLeftTabs := TTabsConfigLocation(cbTargetPanelLeftSavedTabs.ItemIndex);
        2: TFavoriteTabs(tvFavoriteTabs.Selected.Data).DestinationForSavedRightTabs := TTabsConfigLocation(cbTargetPanelRightSavedTabs.ItemIndex);
        3: TFavoriteTabs(tvFavoriteTabs.Selected.Data).ExistingTabsToKeep := TTabsConfigLocation(cbExistingTabsToKeep.ItemIndex);
        4: TFavoriteTabs(tvFavoriteTabs.Selected.Data).SaveDirHistory := (cbSaveDirHistory.ItemIndex = 1);
      end;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.lblediTFavoriteTabsEnter }
procedure TfrmOptionsFavoriteTabs.lbleditFavoriteTabsEnter(Sender: TObject);
begin
  with Sender as TLabeledEdit do
  begin
    Font.Style := [fsBold];
    EditLabel.Font.Style := [fsBold];
  end;
end;

{ TfrmOptionsFavoriteTabs.lblediTFavoriteTabsExit }
procedure TfrmOptionsFavoriteTabs.lbleditFavoriteTabsExit(Sender: TObject);
begin
  //If nothing currently selected, no need to update anything here.
  if tvFavoriteTabs.Selected <> nil then
  begin
    with Sender as TLabeledEdit do
    begin
      Font.Style := [];
      EditLabel.Font.Style := []; //Text not in bold anymore

      case tag of
        1: // Favorite Tabs name
        begin
          try
            //Make sure we actually have something, not an attempt of submenu or end of menu
            if (Text <> '') and (Text[1] <> '-') then
            begin
              //Make sure it's different than what it was
              if TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsName <> Text then
              begin
                TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsName := Text;
                tvFavoriteTabs.Selected.Text := Text;
              end;
            end;
          except
            //Just in case the "Text" is empty to don't show error with Text[1] check.
          end;
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.lblediTFavoriteTabsKeyPress }
procedure TfrmOptionsFavoriteTabs.lbleditFavoriteTabsKeyPress(Sender: TObject; var Key: char);
begin
  case Ord(Key) of
    $0D: //Enter? Let's save the field and we'll exit
    begin
      lblediTFavoriteTabsExit(Sender); //Doing this will SAVE the new typed text if it's different than what we have in memory for the entry. Then we could attempt to quit.
    end;

    $1B: //Escape? Place back the fields like they were
    begin
      with Sender as TLabeledEdit do
      begin
        //If typed text has been changed, yes we will restore it but if it was not change, we will quit so user won't have to press two times escape
        case tag of
          1: if Text <> TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsName then
              Key := #$00;
        end;

        case tag of
          1: tvFavoriteTabsSelectionChanged(tvFavoriteTabs);
        end;
      end;

      if key <> #$1B then
        tvFavoriteTabs.SetFocus;
    end;
  end;

  Application.ProcessMessages;
end;

{ TfrmOptionsFavoriteTabs.miInsertAddFavoriteTabsClick }
// Regarding the tag...
//  bit 0 = Separator.
//  bit 1 = Sub menu.
//  bit 2 = Reserved.
//  bit 3 = Reserved.
//  bit 4 = Insert something. (Before index).
//  bit 5 = Add something. (At index).
//  bit 6 = Reserved...
//  bit 7 = Reserved...
//  bit 8 = Special function.
procedure TfrmOptionsFavoriteTabs.miInsertAddFavoriteTabsClick(Sender: TObject);
var
  Dispatcher: integer;
  NodeAfterAddition: TTreeNode = nil;
  SubNodeAfterAddition: TTreeNode = nil; //For fake submenu item, at the end of the add, focus will be in the menu name.
  localFavoriteTabs: TFavoriteTabs;
begin
  Dispatcher := (TMenuItem(Sender).GetParentComponent.Tag shl 4) or TComponent(Sender).Tag;

  case Dispatcher of
    $011: NodeAfterAddition := ActualAddFavoriteTabs(fte_SEPARATOR, FAVORITETABS_SEPARATORSTRING, naInsert);
    $021: NodeAfterAddition := ActualAddFavoriteTabs(fte_SEPARATOR, FAVORITETABS_SEPARATORSTRING, naInsertBehind);

    $012, $022:
    begin
      case Dispatcher of
        $12: NodeAfterAddition := ActualAddFavoriteTabs(fte_STARTMENU, rsMsgFavoriteTabsSubMenuName, naInsert);
        $22: NodeAfterAddition := ActualAddFavoriteTabs(fte_STARTMENU, rsMsgFavoriteTabsSubMenuName, naInsertBehind);
      end;
      tvFavoriteTabs.ClearSelection(True);
      NodeAfterAddition.Selected := True;
      SubNodeAfterAddition := ActualAddFavoriteTabs(fte_ACTUALFAVTABS, rsMsgFavoriteTabsDragHereEntry, naAddChildFirst);

      SubNodeAfterAddition.Selected := True;
      SubNodeAfterAddition.Expand(True);
    end;

    $100: // Note. It is true that the new added TFavoriteTemp will be at the end of the list and not just after where we might "think" we add it. But we don't care! What we do is setting up our tree the way we need and then at the end the tree will be translated back to our valid list and that's it!
    begin
      localFavoriteTabs := TFavoriteTabs.Create;
      TFavoriteTabs(tvFavoriteTabs.Selected.Data).CopyToFavoriteTabs(localFavoriteTabs, False);
      FavoriteTabsListTemp.Add(localFavoriteTabs);
      NodeAfterAddition := tvFavoriteTabs.Items.InsertObjectBehind(tvFavoriteTabs.Selected, TFavoriteTabs(FavoriteTabsListTemp[pred(FavoriteTabsListTemp.Count)]).FavoriteTabsName, FavoriteTabsListTemp[pred(FavoriteTabsListTemp.Count)]);
    end;
  end;

  if NodeAfterAddition <> nil then
  begin
    tvFavoriteTabs.ClearSelection(True);
    NodeAfterAddition.Selected := True;

    case Dispatcher of
      $012, $022: btnRenameClick(btnRename);
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.miDeleteSelectedEntryClick }
procedure TfrmOptionsFavoriteTabs.miDeleteSelectedEntryClick(Sender: TObject);
var
  DeleteDispatcher: integer;
  FlagQuitDeleting: boolean;
  Answer: TMyMsgResult;
  NodeAfterDeletion: TTreeNode = nil;
  isTreeHadFocus: boolean = False;

  procedure DeleteSelectionAndSetNodeAfterDeletion;
  begin
    if tvFavoriteTabs.Selections[0].GetNextSibling <> nil then  NodeAfterDeletion := tvFavoriteTabs.Selections[0].GetNextSibling
    else if tvFavoriteTabs.Selections[0].GetPrevSibling <> nil then NodeAfterDeletion := tvFavoriteTabs.Selections[0].GetPrevSibling
    else if tvFavoriteTabs.Selections[0].Parent <> nil then NodeAfterDeletion := tvFavoriteTabs.Selections[0].Parent
    else NodeAfterDeletion := nil;
    tvFavoriteTabs.Selections[0].Delete;
    ClearCutAndPasteList;
  end;

begin
  if tvFavoriteTabs.SelectionCount > 0 then
  begin
    isTreeHadFocus := tvFavoriteTabs.Focused;
    tvFavoriteTabs.Enabled := False;
    try
      with Sender as TComponent do
        DeleteDispatcher := tag;
      FlagQuitDeleting := False;

      //It's funny but as long we have something selected, we delete it and it will be index 0 since when
      //deleting something, the "Selections" array is updated!
      while (tvFavoriteTabs.SelectionCount > 0) and (not FlagQuitDeleting) do
      begin
        if tvFavoriteTabs.Selections[0].GetFirstChild = nil then
        begin
          DeleteSelectionAndSetNodeAfterDeletion;
        end
        else
        begin
          case DeleteDispatcher of
            1: Answer := MsgBox(Format(rsMsgHotDirWhatToDelete, [tvFavoriteTabs.Selections[0].Text]), [msmbAll, msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
            2: Answer := mmrNo;
            3: Answer := mmrYes;
            else
              Answer := mmrCancel; //Should not happen, but just in case
          end;

          case Answer of
            mmrAll:
            begin
              DeleteDispatcher := 3;
              DeleteSelectionAndSetNodeAfterDeletion;
            end;
            mmrYes: DeleteSelectionAndSetNodeAfterDeletion;

            mmrNo:
            begin
              NodeAfterDeletion := tvFavoriteTabs.Selections[0].GetFirstChild;
              repeat
                tvFavoriteTabs.Selections[0].GetFirstChild.MoveTo(tvFavoriteTabs.Selections[0].GetFirstChild.Parent, naInsert);
              until tvFavoriteTabs.Selections[0].GetFirstChild = nil;
              tvFavoriteTabs.Selections[0].Delete;
              ClearCutAndPasteList;
            end;

            else
              FlagQuitDeleting := True;
          end;
        end;
      end;
      if (NodeAfterDeletion = nil) and (FlagQuitDeleting = False) and (tvFavoriteTabs.Items.Count > 0) then
        NodeAfterDeletion := tvFavoriteTabs.Items.Item[0];
      if (NodeAfterDeletion <> nil) and (FlagQuitDeleting = False) then
        NodeAfterDeletion.Selected := True;
    finally
      tvFavoriteTabs.Enabled := True;
      if isTreeHadFocus and tvFavoriteTabs.CanFocus then
        tvFavoriteTabs.SetFocus;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.miDeleteAllFavoriteTabsClick }
procedure TfrmOptionsFavoriteTabs.miDeleteAllFavoriteTabsClick(Sender: TObject);
begin
  if MsgBox(rsMsgFavoriteTabsDeleteAllEntries, [msmbYes, msmbNo], msmbNo, msmbNo) = mmrYes then
  begin
    tvFavoriteTabs.Items.Clear;
    gpSavedTabsRestorationAction.Enabled := False;
    ClearCutAndPasteList;
  end;
end;

{ TfrmOptionsFavoriteTabs.miSortFavoriteTabsClick }
//The trick here is that a "group number" identical has been assigned to the sibling between separator and then we sort
//Teh sort has been arrange in such way that item from different group won't be mixed.
procedure TfrmOptionsFavoriteTabs.miSortFavoriteTabsClick(Sender: TObject);
var
  Dispatcher, Index: integer;
  StartingNode: TTreeNode;
  FlagKeepGoingBack: boolean;
begin
  with Sender as TComponent do Dispatcher := tag;
  for Index := 0 to pred(tvFavoriteTabs.Items.Count) do
    TFavoriteTabs(tvFavoriteTabs.Items.Item[Index].Data).GroupNumber := 0;

  GlobalGroupNumber := 0;

  if tvFavoriteTabs.SelectionCount > 0 then
  begin
    case Dispatcher of
      1, 2: //current group only or current level
      begin
        for Index := 0 to pred(tvFavoriteTabs.SelectionCount) do
        begin
          if TFavoriteTabs(tvFavoriteTabs.Selections[Index].Data).GroupNumber = 0 then
          begin
            StartingNode := tvFavoriteTabs.Selections[Index];

            case Dispatcher of
              1: //We just need to make sure we start from first item of current level so we search the first one OR a separator
              begin
                FlagKeepGoingBack := True;
                while FlagKeepGoingBack do
                begin
                  if StartingNode.GetPrevSibling <> nil then
                  begin
                    if TFavoriteTabs(StartingNode.GetPrevSibling.Data).Dispatcher <> fte_SEPARATOR then
                      StartingNode := StartingNode.GetPrevSibling
                    else
                      FlagKeepGoingBack := False;
                  end
                  else
                  begin
                    FlagKeepGoingBack := False;
                  end;
                end;
              end;

              2: //We need to make sure we start from the first itm of current level
              begin
                while StartingNode.GetPrevSibling <> nil do
                  StartingNode := StartingNode.GetPrevSibling;
              end;
            end;

            RecursiveSetGroupNumbers(StartingNode, GetNextGroupNumber, False, (Dispatcher = 1));
          end;
        end;
      end;

      3, 4: //submenu only, recusive or not
      begin
        for Index := 0 to pred(tvFavoriteTabs.SelectionCount) do
        begin
          StartingNode := tvFavoriteTabs.Selections[Index].GetFirstChild;
          if StartingNode <> nil then
          begin
            if TFavoriteTabs(StartingNode.Data).GroupNumber = 0 then
            begin
              RecursiveSetGroupNumbers(StartingNode, GetNextGroupNumber, (Dispatcher = 4), False);
            end;
          end;
        end;
      end;
    end;
  end;

  if Dispatcher = 5 then //We start from the very first one, the top one.
  begin
    StartingNode := tvFavoriteTabs.Items.Item[0];
    RecursiveSetGroupNumbers(StartingNode, GetNextGroupNumber, True, False);
  end;

  //... and the finale!
  tvFavoriteTabs.CustomSort(@MySortViaGroup);
  ClearCutAndPasteList;
end;

{ TfrmOptionsFavoriteTabs.MakeUsUpToDatePriorImportExport }
function TfrmOptionsFavoriteTabs.MakeUsUpToDatePriorImportExport: boolean;
var
  iIndex: integer;
  Answer: TMyMsgResult;
  slRememberCurrentSelections: TStringList;
begin
  FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
  Result := (LastLoadedOptionSignature = ComputeCompleteOptionsSignature);
  if not Result then
  begin
    Answer := MsgBox(rsMsgFavoriteTabsModifiedNoImport, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
    case Answer of
      mmrYes:
      begin
        Save;
        Result := True;
      end;

      mmrNo:
      begin
        slRememberCurrentSelections := TStringList.Create;
        try
          // Saving a trace of what is selected right now.
          for iIndex := 0 to pred(tvFavoriteTabs.Items.Count) do
            if tvFavoriteTabs.Items[iIndex].Selected then
              if TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
                slRememberCurrentSelections.Add(GUIDtoString(TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).UniqueID));

          gFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(FavoriteTabsListTemp);
          FavoriteTabsListTemp.LoadTTreeView(tvFavoriteTabs);
          Result := True;

          // Restoring what was selected.
          tvFavoriteTabs.ClearSelection(False);
          for iIndex := 0 to pred(tvFavoriteTabs.Items.Count) do
            if TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
              tvFavoriteTabs.Items[iIndex].Selected := (slRememberCurrentSelections.IndexOf(GUIDtoString(TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).UniqueID)) <> -1);
        finally
          FreeAndNil(slRememberCurrentSelections);
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.miExportToLegacyTabsFileClick }
// We will not annoy user nad even if nothing has been saved yet, even if he might have move entries from a place to another,
//   we will accept to export selection anyway. But because of this, we will do it from the "Temp" list AND
//   it will be based from "UniqueID" of each.
procedure TfrmOptionsFavoriteTabs.miExportToLegacyTabsFileClick(Sender: TObject);
var
  iIndex, iFileExportedSuccessfully, iMaybeExportedIndex, iSelectionMade: integer;
  sTargetDirectory, sUserMessage: string;
begin
  if MakeUsUpToDatePriorImportExport then
  begin
    if SelectDirectory(rsSelectDir, '', sTargetDirectory, False) then
    begin
      iFileExportedSuccessfully := 0;
      iSelectionMade := 0;
      gFavoriteTabsList.LastImportationStringUniqueId.Clear;
      for iIndex := 0 to pred(tvFavoriteTabs.Items.Count) do
      begin
        if tvFavoriteTabs.Items[iIndex].Selected then
        begin
          Inc(iSelectionMade);
          if TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
          begin
            iMaybeExportedIndex := gFavoriteTabsList.GetIndexForSuchUniqueID(TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).UniqueID);
            if iMaybeExportedIndex <> -1 then
              if gFavoriteTabsList.ExportToLegacyTabsFile(iMaybeExportedIndex, sTargetDirectory) then Inc(iFileExportedSuccessfully);
          end;
        end;
      end;

      sUserMessage := '';
      for iIndex := 0 to pred(gFavoriteTabsList.LastImportationStringUniqueId.Count) do
        sUserMessage := sUserMessage + #$0A + gFavoriteTabsList.LastImportationStringUniqueId.Strings[iIndex];

      msgOk(Format(rsMsgFavoriteTabsExportedSuccessfully, [iFileExportedSuccessfully, iSelectionMade]) + #$0A + sUserMessage);
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.miImportLegacyTabFilesClick }
procedure TfrmOptionsFavoriteTabs.miImportLegacyTabFilesClick(Sender: TObject);
var
  iIndex, iFileImportedSuccessfully: integer;
  iPositionToInsert: integer = -1;
  NodeAfterAddition: TTreeNode;
  RememberUniqueIdToRemove: TGUID;
begin
  if MakeUsUpToDatePriorImportExport then
  begin
    // 1. If we need to create a sub menu, let's create it first
    if TComponent(Sender).Tag = 2 then
    begin
      NodeAfterAddition := ActualAddFavoriteTabs(fte_STARTMENU, rsMsgFavoriteTabsImportSubMenuName, naInsert);
      tvFavoriteTabs.ClearSelection(True);
      NodeAfterAddition.Selected := True;
      NodeAfterAddition := ActualAddFavoriteTabs(fte_ACTUALFAVTABS, rsMsgFavoriteTabsDragHereEntry, naAddChildFirst);
      NodeAfterAddition.Selected := True;
      NodeAfterAddition.Expand(True);
      RememberUniqueIdToRemove := TFavoriteTabs(tvFavoriteTabs.Selected.Data).UniqueID;
      FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
      Save;
    end;

    // 3. Prompt user for which file to import.
    OpenDialog.FilterIndex := 1;
    OpenDialog.Title := rsMsgFavoriteTabsImportTitle;
    if OpenDialog.Execute then
    begin
      // 4. Now let's import them one by one.
      if tvFavoriteTabs.Selected <> nil then
        if (TComponent(Sender).Tag = 1) or (TComponent(Sender).Tag = 2) then
          iPositionToInsert := gFavoriteTabsList.GetIndexForSuchUniqueID(TFavoriteTabs(tvFavoriteTabs.Selected.Data).UniqueID);
      gFavoriteTabsList.LastImportationStringUniqueId.Clear;
      iIndex := 0;
      iFileImportedSuccessfully := 0;
      while iIndex < OpenDialog.Files.Count do
      begin
        if gFavoriteTabsList.ImportFromLegacyTabsFile(OpenDialog.Files.Strings[iIndex], iPositionToInsert) then
        begin
          Inc(iFileImportedSuccessfully);
          if iPositionToInsert <> -1 then Inc(iPositionToInsert);
        end;
        Inc(iIndex);
      end;

      // 5. Before we forget, let's update our mainmenu regarding Favorite Tabs we offer.
      gFavoriteTabsList.RefreshAssociatedMainMenu;

      // 6. Refresh what we see here in our Favorite Tabs configurations screen.
      gFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(FavoriteTabsListTemp);
      FavoriteTabsListTemp.LoadTTreeView(tvFavoriteTabs);
      cbFullExpandTreeChange(cbFullExpandTree);

      if TComponent(Sender).Tag = 2 then
      begin
        for iIndex := pred(tvFavoriteTabs.Items.Count) downto 0 do // We go back since we're deleting something in a list.
          if TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
            if IsEqualGUID(TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).UniqueID, RememberUniqueIdToRemove) then
              tvFavoriteTabs.Items[iIndex].Delete;

        FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
        Save;
      end;

      // 7. Let's higlight in our trees the one(s) that have been imported so it will give feeback to user.
      tvFavoriteTabs.ClearSelection(False);
      for iIndex := 0 to pred(tvFavoriteTabs.Items.Count) do
      begin
        if TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
        begin
          if gFavoriteTabsList.LastImportationStringUniqueId.IndexOf(GUIDToString(TFavoriteTabs(tvFavoriteTabs.Items[iIndex].Data).UniqueID)) <> -1 then
            tvFavoriteTabs.Items[iIndex].Selected := True;
        end;
      end;

      // 7. Refresh our last signature since what we have is up to date.
      LastLoadedOptionSignature := ComputeCompleteOptionsSignature;

      // 8. Finally, let's inform our user we've complete importation.
      msgOk(Format(rsMsgFavoriteTabsImportedSuccessfully, [iFileImportedSuccessfully, OpenDialog.Files.Count]));

      if not tvFavoriteTabs.Focused then
        if tvFavoriteTabs.CanFocus then
          tvFavoriteTabs.SetFocus;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.miTestResultingFavoriteTabsMenuClick }
procedure TfrmOptionsFavoriteTabs.miTestResultingFavoriteTabsMenuClick(Sender: TObject);
var
  p: TPoint;
begin
  FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs); //We need to refresh our temporary Directory Hotlist in case user played with the tree and added/removed/moved item(s).
  FavoriteTabsListTemp.PopulateMenuWithFavoriteTabs(pmFavoriteTabsTestMenu, @miShowWhereItWouldGo, ftmp_JUSTFAVTABS);
  p := tvFavoriteTabs.ClientToScreen(Classes.Point(0, 0));
  p.x := p.x + tvFavoriteTabs.Width;
  pmFavoriteTabsTestMenu.PopUp(p.X, p.Y);
end;

{ TfrmOptionsFavoriteTabs.miShowWhereItWouldGo }
procedure TfrmOptionsFavoriteTabs.miShowWhereItWouldGo(Sender: TObject);
begin
  if FavoriteTabsListTemp.FavoriteTabs[tag].Dispatcher = fte_ACTUALFAVTABS then msgOK(Format(rsMsgFavoriteTabsThisWillLoadFavTabs, [FavoriteTabsListTemp.FavoriteTabs[TComponent(Sender).tag].FavoriteTabsName]));
end;

{ TfrmOptionsFavoriteTabs.miOpenAllBranchesClick }
procedure TfrmOptionsFavoriteTabs.miOpenAllBranchesClick(Sender: TObject);
begin
  tvFavoriteTabs.FullExpand;
  if tvFavoriteTabs.Selected <> nil then
  begin
    tvFavoriteTabs.Selected.MakeVisible;
    if tvFavoriteTabs.CanFocus then tvFavoriteTabs.SetFocus;
  end;
end;

{ TfrmOptionsFavoriteTabs.miCollapseAllClick }
procedure TfrmOptionsFavoriteTabs.miCollapseAllClick(Sender: TObject);
begin
  tvFavoriteTabs.FullCollapse;
  if tvFavoriteTabs.Selected <> nil then
  begin
    tvFavoriteTabs.Selected.MakeVisible;
    if tvFavoriteTabs.CanFocus then tvFavoriteTabs.SetFocus;
  end;
end;

{ TfrmOptionsFavoriteTabs.miCutSelectionClick }
procedure TfrmOptionsFavoriteTabs.miCutSelectionClick(Sender: TObject);
var
  Index: integer;
begin
  if tvFavoriteTabs.SelectionCount > 0 then
  begin
    for Index := 0 to pred(tvFavoriteTabs.SelectionCount) do
    begin
      CutAndPasteIndexList.Add(IntToStr(tvFavoriteTabs.Selections[Index].AbsoluteIndex));
    end;

    miPasteSelection.Enabled := True;
  end;
end;

{ TfrmOptionsFavoriteTabs.miPasteSelectionClick }
procedure TfrmOptionsFavoriteTabs.miPasteSelectionClick(Sender: TObject);
var
  DestinationNode: TTreeNode;
  Index: longint;
begin
  if CutAndPasteIndexList.Count > 0 then
  begin
    DestinationNode := tvFavoriteTabs.Selected;
    if DestinationNode <> nil then
    begin
      tvFavoriteTabs.ClearSelection(False);
      for Index := 0 to pred(CutAndPasteIndexList.Count) do
      begin
        tvFavoriteTabs.Items.Item[StrToInt(CutAndPasteIndexList.Strings[Index])].Selected := True;
      end;

      for Index := 0 to pred(tvFavoriteTabs.SelectionCount) do
      begin
        tvFavoriteTabs.Selections[Index].MoveTo(DestinationNode, naInsert);
      end;
      ClearCutAndPasteList;
    end;
  end;
end;

end.
