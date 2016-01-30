{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of Favorite Tabs

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

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

   -This unit has been added in 2016, inspired a lot from "foptionsDirectoryHotlist".
}

unit fOptionsFavoriteTabs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus,
  Dialogs, ComCtrls, uFavoriteTabs, types, fOptionsFrame;

type
  { TFavTabsActionAddOrConfig }
  TFavTabsActionAddOrConfig = (ftaaocAddToFavoriteTabs, ftaaocJustShowFavoriteTabsConfig, ftaaocSaveExistingShowIt);

  { TfrmOptionsFavoriteTabs }
  TfrmOptionsFavoriteTabs = class(TOptionsEditor)
    gbFavoriteTabs: TGroupBox;
    MenuItem1: TMenuItem;
    pnlClient: TPanel;
    tvFavoriteTabs: TTreeView;
    pnlButtons: TPanel;
    btnInsert: TBitBtn;
    btnDelete: TBitBtn;
    btnMiscellaneous: TBitBtn;
    btnAdd: TBitBtn;
    btnSort: TBitBtn;
    rgWhereToAdd: TRadioGroup;
    gbFavoriteTabsOtherOptions: TGroupBox;
    cbFullExpandTree: TCheckBox;
    cbSaveDirHistory: TCheckBox;
    cbNoNeedToConfigAfterSave: TCheckBox;
    lblDefaultTargetPanelSavedTabs: TLabel;
    lblDefaultTargetPanelLeftSaved: TLabel;
    lblDefaultTargetPanelRightSaved: TLabel;
    cbDefaultTargetPanelLeftSaved: TComboBox;
    cbDefaultTargetPanelRightSaved: TComboBox;
    lblDefaultExistingTabsToKeep: TLabel;
    cbDefaultExistingTabsToKeep: TComboBox;
    cbNoNeedToConfigAfterReSave: TCheckBox;
    pnlBottom: TPanel;
    lbleditFavoriteTabsName: TLabeledEdit;
    lbleditFavoriteTabsSavedFilename: TLabeledEdit;
    cbTargetPanelLeftSavedTabs: TComboBox;
    lblTargetPanelLeftSavedTabs: TLabel;
    lblTargetPanelRightSavedTabs: TLabel;
    cbTargetPanelRightSavedTabs: TComboBox;
    cbExistingTabsToKeep: TComboBox;
    lblExistingTabsToKeep: TLabel;
    pmFavoriteTabsTestMenu: TPopupMenu;
    miFavoriteTabsTestMenu: TMenuItem;
    pmTreeView: TPopupMenu;
    miAddCopyOfSelected2: TMenuItem;
    miSeparator6: TMenuItem;
    miAddSeparator2: TMenuItem;
    miAddSubmenu2: TMenuItem;
    miSeparator7: TMenuItem;
    miDeleteSelectedEntry2: TMenuItem;
    miSeparator8: TMenuItem;
    miSortSingleGroup2: TMenuItem;
    miSeparator9: TMenuItem;
    miCutSelection: TMenuItem;
    miPasteSelection: TMenuItem;
    pmAddToFavoriteTabs: TPopupMenu;
    miAddSeparator: TMenuItem;
    miAddSubmenu: TMenuItem;
    pmDeleteFavoriteTabs: TPopupMenu;
    miDeleteSelectedEntry: TMenuItem;
    miSeparator2: TMenuItem;
    miDeleteJustSubMenu: TMenuItem;
    miDeleteCompleteSubMenu: TMenuItem;
    miSeparator3: TMenuItem;
    miDeleteAllFavoriteTabs: TMenuItem;
    pmMiscellaneousFavoriteTabs: TPopupMenu;
    miTestResultingHotlistMenu: TMenuItem;
    miSeparator10: TMenuItem;
    miOpenAllBranches: TMenuItem;
    miCollapseAll: TMenuItem;
    pmSortFavoriteTabsList: TPopupMenu;
    miSortSingleGroup: TMenuItem;
    miCurrentLevelOfItemOnly: TMenuItem;
    miSortSingleSubMenu: TMenuItem;
    miSortSubMenuAndSubLevel: TMenuItem;
    miSortEverything: TMenuItem;
    function GetSettingsSignatureOfThisFrame: dword;
    function TryToGetExactFavoriteTabs(const index: integer): TTreeNode;
    function TryToGetCloserFavoriteTabs(sFavoriteTabsNameToFindAPlaceFor: string; var TypeOfAddition: TNodeAttachMode): TTreeNode;
    procedure RefreshTreeView(NodeToSelect: TTreeNode);
    procedure tvFavoriteTabsDragDrop(Sender, {%H-}Source: TObject; X, Y: integer);
    procedure tvFavoriteTabsDragOver(Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure tvFavoriteTabsEnter(Sender: TObject);
    procedure tvFavoriteTabsExit(Sender: TObject);
    procedure tvFavoriteTabsSelectionChanged(Sender: TObject);
    procedure cbFullExpandTreeChange(Sender: TObject);
    procedure miInsertAddFavoriteTabsClick(Sender: TObject);
    function ActualAddFavoriteTabs(ParamDispatcher: TKindOfFavoriteTabsEntry; sFavoriteTabsName, sFavoriteTabsSavedFilename: string; InsertOrAdd: TNodeAttachMode): TTreeNode;
    procedure miDeleteSelectedEntryClick(Sender: TObject);
    procedure miDeleteAllFavoriteTabsClick(Sender: TObject);
    procedure miSortFavoriteTabsClick(Sender: TObject);
    function MySortViaGroup(Node1, Node2: TTreeNode): integer;
    procedure RecursiveSetGroupNumbers(ParamNode: TTreeNode; ParamGroupNumber: integer; DoRecursion, StopAtFirstGroup: boolean);
    function GetNextGroupNumber: integer;
    procedure miTestResultingHotlistMenuClick(Sender: TObject);
    procedure miShowWhereItWouldGo(Sender: TObject);
    procedure miOpenAllBranchesClick(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure ClearCutAndPasteList;
    procedure miCutSelectionClick(Sender: TObject);
    procedure miPasteSelectionClick(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure lbleditFavoriteTabsEnter(Sender: TObject);
    procedure lbleditFavoriteTabsExit(Sender: TObject);
    procedure cbTabsConfigChange(Sender: TObject);
    procedure lbleditFavoriteTabsKeyPress(Sender: TObject; var Key: char);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  private
    { Private declarations }
    FavoriteTabsListTemp: TFavoriteTabsList;
    CutAndPasteIndexList: TStringList;
    GlobalGroupNumber: integer;
    FLastLoadedFavoriteTabsListSignature: dword;
  public
    { Public declarations }
    destructor Destroy; override;
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    function CanWeClose(var {%H-}WillNeedUpdateWindowView: boolean): boolean; override;
    procedure SubmitToAddOrConfigToFavoriteTabsDlg(paramActionDispatcher: TFavTabsActionAddOrConfig; paramFavoriteTabsName, paramFavoriteTabsSavedFilename: string; paramOptionalIndex: integer);
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LCLProc, LCLIntf, LCLMessageGlue,

  //DC
  fOptionsMisc, DCStrUtils, uGlobs, uLng, uDCUtils, uDebug, fmain,
  uFormCommands, uShowMsg, DCOSUtils, uSpecialDir, fOptions,
  uComponentsSignature;

{ TfrmOptionsFavoriteTabs.Init }
procedure TfrmOptionsFavoriteTabs.Init;
begin
  pnlBottom.Constraints.MinHeight := pnlBottom.Height;
  ParseLineToList(rsOptFavoriteTabsWhereToAddInList, rgWhereToAdd.Items);
end;

{ TfrmOptionsFavoriteTabs.Load }
procedure TfrmOptionsFavoriteTabs.Load;
begin
  rgWhereToAdd.ItemIndex := integer(gWhereToAddNewFavoriteTabs);
  cbFullExpandTree.Checked := gFavoriteTabsFullExpandOrNot;
  cbSaveDirHistory.Checked := gFavoriteTabsSaveDirHistory;
  cbNoNeedToConfigAfterSave.Checked := gFavoriteTabsNoNeedToConfigAfterSave;
  cbNoNeedToConfigAfterReSave.Checked := gFavoriteTabsNoNeedToConfigAfterReSave;
  cbDefaultTargetPanelLeftSaved.ItemIndex := integer(gDefaultTargetPanelLeftSaved);
  cbDefaultTargetPanelRightSaved.ItemIndex := integer(gDefaultTargetPanelRightSaved);
  cbDefaultExistingTabsToKeep.ItemIndex := integer(gDefaultExistingTabsToKeep);

  CutAndPasteIndexList := TStringList.Create;
  CutAndPasteIndexList.Sorted := True;
  CutAndPasteIndexList.Duplicates := dupAccept;

  if FavoriteTabsListTemp = nil then
  begin
    FavoriteTabsListTemp := TFavoriteTabsList.Create;
    gFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(FavoriteTabsListTemp);
  end;

  tvFavoriteTabs.Images := frmMain.imgLstDirectoryHotlist;
  FavoriteTabsListTemp.LoadTTreeView(tvFavoriteTabs, -1);
  cbFullExpandTreeChange(cbFullExpandTree);
  if tvFavoriteTabs.Items.Count > 0 then tvFavoriteTabs.Items[0].Selected := True; //Select at least first one by default

  FLastLoadedFavoriteTabsListSignature := GetSettingsSignatureOfThisFrame;
end;

{ TfrmOptionsFavoriteTabs.Save }
function TfrmOptionsFavoriteTabs.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
  FavoriteTabsListTemp.CopyFavoriteTabsListToFavoriteTabsList(gFavoriteTabsList);

  gWhereToAddNewFavoriteTabs := TPositionWhereToAddFavoriteTabs(rgWhereToAdd.ItemIndex);
  gFavoriteTabsFullExpandOrNot := cbFullExpandTree.Checked;
  gFavoriteTabsSaveDirHistory := cbSaveDirHistory.Checked;
  gFavoriteTabsNoNeedToConfigAfterSave := cbNoNeedToConfigAfterSave.Checked;
  gFavoriteTabsNoNeedToConfigAfterReSave := cbNoNeedToConfigAfterReSave.Checked;
  gDefaultTargetPanelLeftSaved := TTabsConfigLocation(cbDefaultTargetPanelLeftSaved.ItemIndex);
  gDefaultTargetPanelRightSaved := TTabsConfigLocation(cbDefaultTargetPanelRightSaved.ItemIndex);
  gDefaultExistingTabsToKeep := TTabsConfigLocation(cbDefaultExistingTabsToKeep.ItemIndex);

  cbFullExpandTreeChange(cbFullExpandTree);
  FLastLoadedFavoriteTabsListSignature := GetSettingsSignatureOfThisFrame;
end;

{ TfrmOptionsFavoriteTabs.Destroy }
destructor TfrmOptionsFavoriteTabs.Destroy;
begin
  CutAndPasteIndexList.Free;
  inherited Destroy;
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

{ TfrmOptionsFavoriteTabs.CanWeClose }
function TfrmOptionsFavoriteTabs.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := True;

  if Assigned(FavoriteTabsListTemp) then
  begin
    FavoriteTabsListTemp.RefreshFromTTreeView(tvFavoriteTabs);
    Result := (FLastLoadedFavoriteTabsListSignature = GetSettingsSignatureOfThisFrame);

    if not Result then
    begin
      ShowOptions(TfrmOptionsFavoriteTabs);
      Answer := MsgBox(rsMsgFavoriteTabsModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
      case Answer of
        mmrYes:
        begin
          Save;
          Result := True;
        end;

        mmrNo: Result := True;
        else
          Result := False;
      end;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.SubmitToAddOrConfigToFavoriteTabsDlg }
procedure TfrmOptionsFavoriteTabs.SubmitToAddOrConfigToFavoriteTabsDlg(paramActionDispatcher: TFavTabsActionAddOrConfig; paramFavoriteTabsName, paramFavoriteTabsSavedFilename: string; paramOptionalIndex: integer);
var
  TypeOfAddition: TNodeAttachMode;
  NodeToSelect: TTreeNode = nil;

  procedure AddThisFavoriteTabs;
  begin
    case gWhereToAddNewFavoriteTabs of
      afte_First: TypeOfAddition := naAddFirst;
      afte_Last: TypeOfAddition := naAdd;
    end;

    NodeToSelect := nil;
    if (tvFavoriteTabs.Items.Count > 0) then
    begin
      case gWhereToAddNewFavoriteTabs of
        afte_First: NodeToSelect := tvFavoriteTabs.Items.Item[0];
        afte_Last: NodeToSelect := tvFavoriteTabs.Items.Item[pred(tvFavoriteTabs.Items.Count)];
        afte_Alphabetical: NodeToSelect := TryToGetCloserFavoriteTabs(paramFavoriteTabsName, TypeOfAddition);
        else
          NodeToSelect := tvFavoriteTabs.Items.Item[0];
      end;
      if NodeToSelect <> nil then
        NodeToSelect.Selected := True;
    end;
    NodeToSelect := ActualAddFavoriteTabs(fte_ACTUALFAVTABS, paramFavoriteTabsName, paramFavoriteTabsSavedFilename, TypeOfAddition);
  end;

begin
  case paramActionDispatcher of
    ftaaocAddToFavoriteTabs:
    begin
      AddThisFavoriteTabs;
    end;

    ftaaocJustShowFavoriteTabsConfig:
    begin
      NodeToSelect := TryToGetExactFavoriteTabs(FavoriteTabsListTemp.GetIndexLastFavoriteTabsLoaded);
      //if tvFavoriteTabs.Items.Count > 0 then
      //  NodeToSelect := tvFavoriteTabs.Items.Item[0];
    end;

    ftaaocSaveExistingShowIt:
    begin
      NodeToSelect := TryToGetExactFavoriteTabs(paramOptionalIndex);
    end
  end;

  if (NodeToSelect = nil) and (tvFavoriteTabs.Items.Count > 0) then
    NodeToSelect := tvFavoriteTabs.Items.Item[0];
  RefreshTreeView(NodeToSelect);

  //2014-08-27: These lines are a workaround a problem present at this moment in Lazarus regarding TSpeedButton present inside a TGroupBox.
  //See on the web if the following case is solved prior to remove these lines: http://bugs.freepascal.org/view.php?id=26638
  {$IFDEF MSWINDOWS}
  if tvFavoriteTabs.CanFocus then
  begin
    LCLSendMouseDownMsg(Self, 1, 1, mbLeft, []);
    LCLSendMouseUpMsg(Self, 1, 1, mbLeft, []);
  end;
  {$ENDIF MSWINDOWS}

  if not tvFavoriteTabs.Focused then
    if tvFavoriteTabs.CanFocus then
      tvFavoriteTabs.SetFocus;

  if not lbleditFavoriteTabsName.Focused then
    if lbleditFavoriteTabsName.CanFocus then
      lbleditFavoriteTabsName.SetFocus;
end;

{ TfrmOptionsFavoriteTabs.GetSettingsSignatureOfThisFrame }
function TfrmOptionsFavoriteTabs.GetSettingsSignatureOfThisFrame: dword;
begin
  Result := FavoriteTabsListTemp.ComputeSignature;
  Result := ComputeSignatureSingleComponent(rgWhereToAdd, Result);
  Result := ComputeSignatureSingleComponent(cbFullExpandTree, Result);
  Result := ComputeSignatureSingleComponent(cbSaveDirHistory, Result);
  Result := ComputeSignatureSingleComponent(cbNoNeedToConfigAfterSave, Result);
  Result := ComputeSignatureSingleComponent(cbNoNeedToConfigAfterReSave, Result);
  Result := ComputeSignatureSingleComponent(cbDefaultTargetPanelLeftSaved, Result);
  Result := ComputeSignatureSingleComponent(cbDefaultTargetPanelRightSaved, Result);
  Result := ComputeSignatureSingleComponent(cbDefaultExistingTabsToKeep, Result);
end;

{ TfrmOptionsFavoriteTabs.TryToGetExactFavoriteTabs }
function TfrmOptionsFavoriteTabs.TryToGetExactFavoriteTabs(const index: integer): TTreeNode;
var
  SearchingtvIndex: integer;
begin
  Result := nil;

  SearchingtvIndex := 0;
  while (SearchingtvIndex < pred(tvFavoriteTabs.Items.Count)) and (Result = nil) do
  begin
    if tvFavoriteTabs.Items[SearchingtvIndex].Data = FavoriteTabsListTemp.Items[Index] then
      Result := tvFavoriteTabs.Items[SearchingtvIndex]
    else
      Inc(SearchingtvIndex);
  end;
end;

function CompareStringsFromTStringList(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := CompareStrings(List.Strings[Index1], List.Strings[Index2], gSortNatural, gSortCaseSensitivity);
end;

{ TfrmOptionsFavoriteTabs.TryToGetCloserFavoriteTabs }
// We take the whole list of "FavoriteTabsName" seen as one single alphabetical sorted list.
// We find the exact place where our string to add could fit.
// We insert it right in front of the one its is closed to go.
// If we have the last one, we placed it last!
function TfrmOptionsFavoriteTabs.TryToGetCloserFavoriteTabs(sFavoriteTabsNameToFindAPlaceFor: string; var TypeOfAddition: TNodeAttachMode): TTreeNode;
var
  I: integer;
  localFavoriteTabsNameToFindAPlaceFor: string;
  MagickSortedList: TStringList;
begin
  Result := nil;
  TypeOfAddition := naInsert;
  localFavoriteTabsNameToFindAPlaceFor := UTF8LowerCase(sFavoriteTabsNameToFindAPlaceFor);

  MagickSortedList := TStringList.Create;
  try
    // 1. We add in the list only the actual FavoriteTabsName.
    for I := 0 to pred(tvFavoriteTabs.Items.Count) do
      if TFavoriteTabs(tvFavoriteTabs.Items.Item[I].Data).Dispatcher = fte_ACTUALFAVTABS then
        MagickSortedList.Add(UTF8LowerCase(TFavoriteTabs(tvFavoriteTabs.Items.Item[I].Data).FavoriteTabsName) + DirectorySeparator + IntToStr(I));

    // 2. We sort our list.
    MagickSortedList.Add(localFavoriteTabsNameToFindAPlaceFor);

    // 3. We call a custom sort to make sure sort order will make the sequence "école - Eric - Érika"
    MagickSortedList.CustomSort(@CompareStringsFromTStringList);

    // 4. We have the spot where our string has to go.
    I := MagickSortedList.IndexOf(localFavoriteTabsNameToFindAPlaceFor);

    // 5. We now know to which node to insert it in front (or behind if we're the last one).
    if I = pred(MagickSortedList.Count) then
    begin
      Result := tvFavoriteTabs.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I - 1]))];
      TypeOfAddition := naInsertBehind;
    end
    else
    begin
      Result := tvFavoriteTabs.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I + 1]))];
    end;

  finally
    MagickSortedList.Free;
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

    case TFavoriteTabs(WorkingPointer).Dispatcher of
      fte_NULL:
      begin
      end;

      fte_ACTUALFAVTABS:
      begin
        lbleditFavoriteTabsName.EditLabel.Caption := rsMsgFavoriteTabsName;
        lbleditFavoriteTabsName.Text := TFavoriteTabs(WorkingPointer).FavoriteTabsName;
        lbleditFavoriteTabsName.Enabled := True;

        lbleditFavoriteTabsSavedFilename.Text := TFavoriteTabs(WorkingPointer).FavoriteTabsSavedFilename;
        lbleditFavoriteTabsSavedFilename.Hint := mbExpandFileName(lbleditFavoriteTabsSavedFilename.Text);

        cbTargetPanelLeftSavedTabs.ItemIndex := integer(TFavoriteTabs(WorkingPointer).DestinationForSavedLeftTabs);
        cbTargetPanelRightSavedTabs.ItemIndex := integer(TFavoriteTabs(WorkingPointer).DestinationForSavedRightTabs);
        cbExistingTabsToKeep.ItemIndex := integer(TFavoriteTabs(WorkingPointer).ExistingTabsToKeep);

        lbleditFavoriteTabsSavedFilename.Visible := True;
        cbTargetPanelLeftSavedTabs.Visible := True;
        lblTargetPanelLeftSavedTabs.Visible := True;
        lblTargetPanelRightSavedTabs.Visible := True;
        cbTargetPanelRightSavedTabs.Visible := True;
        cbExistingTabsToKeep.Visible := True;
        lblExistingTabsToKeep.Visible := True;
      end;

      fte_SEPARATOR:
      begin
        lbleditFavoriteTabsName.EditLabel.Caption := '';
        lbleditFavoriteTabsName.Text := rsMsgFavoriteTabsSimpleSeparator;
        lbleditFavoriteTabsName.Enabled := False;
      end;

      fte_STARTMENU:
      begin
        lbleditFavoriteTabsName.EditLabel.Caption := rsMsgFavoriteTabsSimpleMenu;
        lbleditFavoriteTabsName.Text := TFavoriteTabs(WorkingPointer).FavoriteTabsName;
        lbleditFavoriteTabsName.Enabled := True;
      end;

      fte_ENDMENU:
      begin
        lbleditFavoriteTabsName.EditLabel.Caption := '';
        lbleditFavoriteTabsName.Text := rsMsgFavoriteTabsEndOfMenu;
        lbleditFavoriteTabsName.Enabled := False;
      end;
    end; //case TFavoriteTabs(WorkingPointer).Dispatcher of

    if TFavoriteTabs(WorkingPointer).Dispatcher <> fte_ACTUALFAVTABS then
    begin
      lblExistingTabsToKeep.Visible := False;
      cbExistingTabsToKeep.Visible := False;
      cbTargetPanelRightSavedTabs.Visible := False;
      lblTargetPanelRightSavedTabs.Visible := False;
      lblTargetPanelLeftSavedTabs.Visible := False;
      cbTargetPanelLeftSavedTabs.Visible := False;
      lbleditFavoriteTabsSavedFilename.Visible := False;
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
    lbleditFavoriteTabsName.EditLabel.Caption := '';
    lbleditFavoriteTabsName.Text := '';
    lbleditFavoriteTabsName.Enabled := False;
    lbleditFavoriteTabsName.Text := 'Nothing...';

    lblExistingTabsToKeep.Visible := False;
    cbExistingTabsToKeep.Visible := False;
    cbTargetPanelRightSavedTabs.Visible := False;
    lblTargetPanelRightSavedTabs.Visible := False;
    lblTargetPanelLeftSavedTabs.Visible := False;
    cbTargetPanelLeftSavedTabs.Visible := False;
    lbleditFavoriteTabsSavedFilename.Visible := False;
  end;
end;

{ TfrmOptionsFavoriteTabs.cbFullExpandTreeChange }
procedure TfrmOptionsFavoriteTabs.cbFullExpandTreeChange(Sender: TObject);
begin
  if cbFullExpandTree.Checked then tvFavoriteTabs.FullExpand else tvFavoriteTabs.FullCollapse;
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
    $011: NodeAfterAddition := ActualAddFavoriteTabs(fte_SEPARATOR, FAVORITETABS_SEPARATORSTRING, '', naInsert);
    $021: NodeAfterAddition := ActualAddFavoriteTabs(fte_SEPARATOR, FAVORITETABS_SEPARATORSTRING, '', naInsertBehind);

    $012, $022:
    begin
      case Dispatcher of
        $12: NodeAfterAddition := ActualAddFavoriteTabs(fte_STARTMENU, rsMsgFavoriteTabsSubMenuName, '', naInsert);
        $22: NodeAfterAddition := ActualAddFavoriteTabs(fte_STARTMENU, rsMsgFavoriteTabsSubMenuName, '', naInsertBehind);
      end;
      tvFavoriteTabs.ClearSelection(True);
      NodeAfterAddition.Selected := True;
      SubNodeAfterAddition := ActualAddFavoriteTabs(fte_ACTUALFAVTABS, rsMsgFavoriteTabsDragHereEntry, '', naAddChildFirst);
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
    if lbleditFavoriteTabsName.CanFocus then lbleditFavoriteTabsName.SetFocus;
  end;
end;

{ TfrmOptionsFavoriteTabs.ActualAddFavoriteTabs }
function TfrmOptionsFavoriteTabs.ActualAddFavoriteTabs(ParamDispatcher: TKindOfFavoriteTabsEntry; sFavoriteTabsName, sFavoriteTabsSavedFilename: string; InsertOrAdd: TNodeAttachMode): TTreeNode;
var
  LocalFavoriteTabs: TFavoriteTabs;
  WorkingTreeNode: TTreeNode;
begin
  ClearCutAndPasteList;
  LocalFavoriteTabs := TFavoriteTabs.Create;
  LocalFavoriteTabs.Dispatcher := ParamDispatcher;
  LocalFavoriteTabs.FavoriteTabsName := sFavoriteTabsName;
  LocalFavoriteTabs.FavoriteTabsSavedFilename := sFavoriteTabsSavedFilename;
  LocalFavoriteTabs.DestinationForSavedLeftTabs := gDefaultTargetPanelLeftSaved;
  LocalFavoriteTabs.DestinationForSavedRightTabs := gDefaultTargetPanelRightSaved;
  LocalFavoriteTabs.ExistingTabsToKeep := gDefaultExistingTabsToKeep;

  FavoriteTabsListTemp.Add(LocalFavoriteTabs);
  WorkingTreeNode := tvFavoriteTabs.Selected;
  if WorkingTreeNode <> nil then
    Result := tvFavoriteTabs.Items.AddNode(nil, WorkingTreeNode, sFavoriteTabsName, LocalFavoriteTabs, InsertOrAdd)
  else
    Result := tvFavoriteTabs.Items.AddNode(nil, nil, sFavoriteTabsName, LocalFavoriteTabs, naAddFirst);

  case ParamDispatcher of
    fte_STARTMENU:
    begin
      Result.ImageIndex := ICONINDEX_SUBMENUFAVTABS;
      Result.SelectedIndex := ICONINDEX_SUBMENUFAVTABS;
      Result.StateIndex := ICONINDEX_SUBMENUFAVTABS;
    end;

    fte_ACTUALFAVTABS:
    begin
      Result.ImageIndex := ICONINDEX_NEWADDEDFAVTABS;
      Result.SelectedIndex := ICONINDEX_NEWADDEDFAVTABS;
      Result.StateIndex := ICONINDEX_NEWADDEDFAVTABS;
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
    lbleditFavoriteTabsName.Text := '';
    lblExistingTabsToKeep.Visible := False;
    cbExistingTabsToKeep.Visible := False;
    cbTargetPanelRightSavedTabs.Visible := False;
    lblTargetPanelRightSavedTabs.Visible := False;
    lblTargetPanelLeftSavedTabs.Visible := False;
    cbTargetPanelLeftSavedTabs.Visible := False;
    lbleditFavoriteTabsSavedFilename.Visible := False;
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

{ TfrmOptionsFavoriteTabs.MySortViaGroup }
function TfrmOptionsFavoriteTabs.MySortViaGroup(Node1, Node2: TTreeNode): integer;
begin
  if (TFavoriteTabs(Node1.Data).GroupNumber = TFavoriteTabs(Node2.Data).GroupNumber) and (TFavoriteTabs(Node1.Data).GroupNumber <> 0) then
  begin
    Result := CompareStrings(TFavoriteTabs(Node1.Data).FavoriteTabsName, TFavoriteTabs(Node2.Data).FavoriteTabsName, gSortNatural, gSortCaseSensitivity);
  end
  else
  begin
    if Node1.AbsoluteIndex < Node2.AbsoluteIndex then
      Result := -1
    else
      Result := 1;
  end;
end;

{ TfrmOptionsFavoriteTabs.GetNextGroupNumber }
function TfrmOptionsFavoriteTabs.GetNextGroupNumber: integer;
begin
  GlobalGroupNumber := GlobalGroupNumber + 1;
  Result := GlobalGroupNumber;
end;

{ TfrmOptionsFavoriteTabs.miTestResultingHotlistMenuClick }
procedure TfrmOptionsFavoriteTabs.miTestResultingHotlistMenuClick(Sender: TObject);
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
var
  StringToShow: string;
begin
  with Sender as TComponent do
  begin
    StringToShow := Format(rsMsgFavoriteTabsNameSample, [FavoriteTabsListTemp.FavoriteTabs[tag].FavoriteTabsName]);

    case FavoriteTabsListTemp.FavoriteTabs[tag].Dispatcher of
      fte_ACTUALFAVTABS:
      begin
        StringToShow := rsMsgFavoriteTabsThisWillLoadTabFile;
        StringToShow := StringToShow + #$0D + #$0A + mbExpandFileName(FavoriteTabsListTemp.FavoriteTabs[tag].FavoriteTabsSavedFilename);
      end;
    end;

    msgOK(StringToShow);
  end;
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

{ TfrmOptionsFavoriteTabs.ClearCutAndPasteList }
procedure TfrmOptionsFavoriteTabs.ClearCutAndPasteList;
begin
  CutAndPasteIndexList.Clear;
  miPasteSelection.Enabled := True;
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

{ TfrmOptionsFavoriteTabs.btnActionClick }
procedure TfrmOptionsFavoriteTabs.btnActionClick(Sender: TObject);
var
  Dispatcher: integer;
begin
  with Sender as TComponent do
    Dispatcher := tag;

  case Dispatcher of
    1, 2: pmAddToFavoriteTabs.Tag := Dispatcher; //To help in routine to determine if it's a "Insert" or a "Add"
  end;

  case Dispatcher of
    1, 2: pmAddToFavoriteTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    3: pmDeleteFavoriteTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    7: pmMiscellaneousFavoriteTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    8: pmSortFavoriteTabsList.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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

        2: //Favorite Tabs Saved Filename
        begin
          try
            //Make sure it's different than what it was
            if TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsSavedFilename <> Text then
            begin
              TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsSavedFilename := Text;
            end;
          except
            //Just in case we have an empty list so "FavoriteTabsListTemp.FavoriteTabs[tvFavoriteTabs.Selected.ImageIndex]" will not caused an error (since ItemIndex=-1 at this moment);
          end;
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsFavoriteTabs.cbTabsConfigChange }
procedure TfrmOptionsFavoriteTabs.cbTabsConfigChange(Sender: TObject);
begin
  with Sender as TComponent do
  begin
    //If nothing currently selected, no need to update anything here.
    if tvFavoriteTabs.Selected <> nil then
    begin
      case tag of
        1: // Combobox for destination of left saved tabs
        begin
          TFavoriteTabs(tvFavoriteTabs.Selected.Data).DestinationForSavedLeftTabs := TTabsConfigLocation(cbTargetPanelLeftSavedTabs.ItemIndex);
        end;

        2: // Combobox for destination of right saved tabs
        begin
          TFavoriteTabs(tvFavoriteTabs.Selected.Data).DestinationForSavedRightTabs := TTabsConfigLocation(cbTargetPanelRightSavedTabs.ItemIndex);
        end;

        3: // Combobox for existing tabs
        begin
          TFavoriteTabs(tvFavoriteTabs.Selected.Data).ExistingTabsToKeep := TTabsConfigLocation(cbExistingTabsToKeep.ItemIndex);
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
          2: if Text <> TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsSavedFilename then
              Key := #00;
        end;

        case tag of
          1: tvFavoriteTabsSelectionChanged(tvFavoriteTabs);
          2: Text := TFavoriteTabs(tvFavoriteTabs.Selected.Data).FavoriteTabsSavedFilename;
        end;
      end;

      if key <> #$1B then
        tvFavoriteTabs.SetFocus;
    end;
  end;

  Application.ProcessMessages;
end;

end.
