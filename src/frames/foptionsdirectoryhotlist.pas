{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of HotDir

   Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit foptionsDirectoryHotlist;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  uTotalCommander,
  {$ENDIF}
  ActnList, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  ExtCtrls, Menus, Dialogs, ComCtrls, uHotDir, types, fOptionsFrame,
  uFile;

type
  TProcedureWhenClickingAMenuItem = procedure(Sender: TObject) of object;

  { TfrmOptionsDirectoryHotlist }
  TfrmOptionsDirectoryHotlist = class(TOptionsEditor)
    actList: TActionList;
    actInsertBrowsedDir: TAction;
    actInsertTypedDir: TAction;
    actInsertActiveFrameDir: TAction;
    actInsertBothFrameDir: TAction;
    actInsertSelectionsFromFrame: TAction;
    actInsertCopyOfEntry: TAction;
    actInsertSeparator: TAction;
    actInsertSubMenu: TAction;
    actAddBrowsedDir: TAction;
    actAddTypedDir: TAction;
    actAddActiveFrameDir: TAction;
    actAddBothFrameDir: TAction;
    actAddSelectionsFromFrame: TAction;
    actAddCopyOfEntry: TAction;
    miSeparator2: TMenuItem;
    actAddSeparator: TAction;
    actAddSubMenu: TAction;
    actDeleteSelectedItem: TAction;
    actDeleteSubMenuKeepElem: TAction;
    actDeleteSubMenuAndElem: TAction;
    actDeleteAll: TAction;
    actMoveToPrevious: TAction;
    actMoveToNext: TAction;
    actCut: TAction;
    actPaste: TAction;
    actSearchAndReplaceInPath: TAction;
    actSearchAndReplaceInTargetPath: TAction;
    actSearchAndReplaceInPathAndTarget: TAction;
    actTweakPath: TAction;
    actTweakTargetPath: TAction;
    actFocusTreeWindow: TAction;
    actGotoFirstItem: TAction;
    actGoToPreviousItem: TAction;
    actGoToNextItem: TAction;
    actGotoLastItem: TAction;
    actExpandItem: TAction;
    actOpenAllBranches: TAction;
    actCollapseItem: TAction;
    actCollapseAll: TAction;
    pmInsertDirectoryHotlist: TPopupMenu;
    miInsertBrowsedDir: TMenuItem;
    miInsertTypedDir: TMenuItem;
    miInsertActiveFrameDir: TMenuItem;
    miInsertBothFrameDir: TMenuItem;
    miInsertSelectionsFromFrame: TMenuItem;
    miInsertCopyOfEntry: TMenuItem;
    miSeparator1: TMenuItem;
    miInsertSeparator: TMenuItem;
    miInsertSubMenu: TMenuItem;
    pmAddDirectoryHotlist: TPopupMenu;
    miAddBrowsedDir: TMenuItem;
    miAddTypedDir: TMenuItem;
    miAddActiveFrameDir: TMenuItem;
    miAddBothFrameDir: TMenuItem;
    miAddSelectionsFromFrame: TMenuItem;
    miAddCopyOfEntry: TMenuItem;
    miAddSeparator: TMenuItem;
    miAddSubMenu: TMenuItem;
    pmDeleteDirectoryHotlist: TPopupMenu;
    miDeleteSelectedItem: TMenuItem;
    miSeparator3: TMenuItem;
    miDeleteSubMenuKeepElem: TMenuItem;
    miDeleteSubMenuAndElem: TMenuItem;
    miSeparator4: TMenuItem;
    miDeleteAll: TMenuItem;
    pmSortDirectoryHotlist: TPopupMenu;
    miSortSingleGroup: TMenuItem;
    miCurrentLevelOfItemOnly: TMenuItem;
    miSortSingleSubMenu: TMenuItem;
    miSortSubMenuAndSubLevel: TMenuItem;
    miSortEverything: TMenuItem;
    pmMiscellaneousDirectoryHotlist: TPopupMenu;
    miTestResultingHotlistMenu: TMenuItem;
    miSeparator5: TMenuItem;
    miNavigate: TMenuItem;
    miFocusTreeWindow: TMenuItem;
    miSeparator10: TMenuItem;
    miGotoFirstItem: TMenuItem;
    miGoToPreviousItem: TMenuItem;
    miGoToNextItem: TMenuItem;
    miGotoLastItem: TMenuItem;
    miSeparator11: TMenuItem;
    miExpandItem: TMenuItem;
    miOpenAllBranches: TMenuItem;
    miCollapseItem: TMenuItem;
    miCollapseAll: TMenuItem;
    miSeparator12: TMenuItem;
    miMoveToPrevious: TMenuItem;
    miMoveToNext: TMenuItem;
    miSeparator6: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    miSeparator7: TMenuItem;
    miSearchAndReplace: TMenuItem;
    miSearchAndReplaceInPath: TMenuItem;
    miSearchAndReplaceInTargetPath: TMenuItem;
    miSearchInReplaceInBothPaths: TMenuItem;
    miSeparator8: TMenuItem;
    miTweakPath: TMenuItem;
    miTweakTargetPath: TMenuItem;
    miSeparator9: TMenuItem;
    miDetectIfPathExist: TMenuItem;
    miDetectIfPathTargetExist: TMenuItem;
    pmExportDirectoryHotlist: TPopupMenu;
    miExportToHotlistFile: TMenuItem;
    miSeparator13: TMenuItem;
    miExportToTotalCommanderk: TMenuItem;
    miExportToTotalCommandernk: TMenuItem;
    miGotoConfigureTCInfo1: TMenuItem;
    pmImportDirectoryHotlist: TPopupMenu;
    miImportFromHotlistFile: TMenuItem;
    miSeparator14: TMenuItem;
    miImportTotalCommander: TMenuItem;
    miGotoConfigureTCInfo2: TMenuItem;
    pmBackupDirectoryHotlist: TPopupMenu;
    miSaveBackupHotlist: TMenuItem;
    miRestoreBackupHotlist: TMenuItem;
    pmHotDirTestMenu: TPopupMenu;
    miHotDirTestMenu: TMenuItem;
    gbDirectoryHotlist: TGroupBox;
    pnlClient: TPanel;
    tvDirectoryHotlist: TTreeView;
    pnlButtons: TPanel;
    btnInsert: TBitBtn;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    btnSort: TBitBtn;
    btnMiscellaneous: TBitBtn;
    btnExport: TBitBtn;
    btnImport: TBitBtn;
    btnBackup: TBitBtn;
    btnHelp: TBitBtn;
    pnlBottom: TPanel;
    rgWhereToAdd: TRadioGroup;
    gbHotlistOtherOptions: TGroupBox;
    cbAddTarget: TCheckBox;
    cbFullExpandTree: TCheckBox;
    cbShowPathInPopup: TCheckBox;
    cbShowOnlyValidEnv: TCheckBox;
    lbleditHotDirName: TLabeledEdit;
    lbleditHotDirPath: TLabeledEdit;
    btnRelativePath: TSpeedButton;
    cbSortHotDirPath: TComboBox;
    lbleditHotDirTarget: TLabeledEdit;
    btnRelativeTarget: TSpeedButton;
    cbSortHotDirTarget: TComboBox;
    pmPathHelper: TPopupMenu;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure actInsertOrAddSomethingExecute(Sender: TObject);
    procedure actDeleteSomethingExecute(Sender: TObject);
    procedure actDeleteAllExecute(Sender: TObject);
    procedure actMoveToPreviousExecute(Sender: TObject);
    procedure actMoveToNextExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSearchAndReplaceExecute(Sender: TObject);
    procedure actTweakPathExecute(Sender: TObject);
    procedure actFocusTreeWindowExecute(Sender: TObject);
    procedure actGotoFirstItemExecute(Sender: TObject);
    procedure actGoToPreviousItemExecute(Sender: TObject);
    procedure actGoToNextItemExecute(Sender: TObject);
    procedure actGotoLastItemExecute(Sender: TObject);
    procedure actExpandItemExecute(Sender: TObject);
    procedure actOpenAllBranchesExecute(Sender: TObject);
    procedure actCollapseItemExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure miSortDirectoryHotlistClick(Sender: TObject);
    procedure miTestResultingHotlistMenuClick(Sender: TObject);
    procedure miDetectIfPathExistClick(Sender: TObject);
    procedure miExportToAnythingClick(Sender: TObject);
    procedure miImportFromAnythingClick(Sender: TObject);
    procedure miGotoConfigureTCInfoClick(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbFullExpandTreeChange(Sender: TObject);
    procedure lbleditHotDirNameChange(Sender: TObject);
    procedure anyRelativeAbsolutePathClick(Sender: TObject);
    procedure cbSortHotDirPathChange(Sender: TObject);
    procedure cbSortHotDirTargetChange(Sender: TObject);
    procedure pnlButtonsResize(Sender: TObject);
    procedure tvDirectoryHotlistDragDrop(Sender, {%H-}Source: TObject; X, Y: integer);
    procedure tvDirectoryHotlistDragOver(Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure tvDirectoryHotlistEnter(Sender: TObject);
    procedure tvDirectoryHotlistExit(Sender: TObject);
    procedure tvDirectoryHotlistSelectionChanged(Sender: TObject);
    procedure RefreshTreeView(NodeToSelect: TTreeNode);
    procedure PopulatePopupMenuWithCommands(pmMenuToPopulate: TPopupMenu);
    procedure miShowWhereItWouldGo(Sender: TObject);
    procedure miSimplyCopyCaption(Sender: TObject);
    procedure ClearCutAndPasteList;
    function ActualAddDirectories(ParamDispatcher: TKindOfHotDirEntry; sName, sPath, sTarget: string; InsertOrAdd: integer): TTreeNode;
    function TryToGetCloserHotDir(sDirToFindAPlaceFor: string; var TypeOfAddition: integer): TTreeNode;
    function TryToGetExactHotDir(const index: integer): TTreeNode;
    procedure RecursiveSetGroupNumbers(ParamNode: TTreeNode; ParamGroupNumber: integer; DoRecursion, StopAtFirstGroup: boolean);
    procedure RefreshExistingProperty(ScanMode: integer);
    procedure SetNormalIconsInTreeView;
    function MySortViaGroup(Node1, Node2: TTreeNode): integer;
    procedure CopyTTreeViewToAnother(tvSource, tvDestination: TTreeView);
    function GetNextGroupNumber: integer;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  private
    { Private declarations }
    pmCommandHelper: TPopupMenu;
    DirectoryHotlistTemp: TDirectoryHotlist;
    CutAndPasteIndexList: TStringList;
    GlobalGroupNumber: integer;
  public
    { Public declarations }
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
    destructor Destroy; override;
    procedure SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher: integer; paramPath, paramTarget: string; paramOptionalIndex: integer);
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LazUTF8, LCLIntf, LCLMessageGlue, helpintfs,

  //DC
  fEditSearch, fOptionsMisc, DCStrUtils, uGlobs, uLng, uDCUtils, fmain,
  uFormCommands, uFileProcs, uShowMsg, DCOSUtils, uSpecialDir,
  fhotdirexportimport, uComponentsSignature;

{ Constants used with export/import }
const
  MASK_ACTION_WITH_WHAT = $03;
  MASK_FLUSHORNOT_EXISTING = $80;

  ACTION_WITH_WINCMDINI = $00;
  ACTION_WITH_HOTLISTFILE = $01;
  ACTION_WITH_BACKUP = $02;

  ACTION_ERASEEXISTING = $80;

  { Constant used with various action }
  ACTION_INSERTHOTDIR = 1;
  ACTION_ADDHOTDIR = 2;

{ TfrmOptionsDirectoryHotlist.Init }
procedure TfrmOptionsDirectoryHotlist.Init;
begin
  pnlBottom.Constraints.MinHeight := pnlBottom.Height;
  ParseLineToList(rsOptAddFromMainPanel, rgWhereToAdd.Items);
  ParseLineToList(rsHotDirForceSortingOrderChoices, cbSortHotDirPath.Items);
  ParseLineToList(rsHotDirForceSortingOrderChoices, cbSortHotDirTarget.Items);
  OpenDialog.Filter := ParseLineToFileFilter([rsFilterDirectoryHotListFiles, '*.hotlist', rsFilterXmlConfigFiles, '*.xml', rsFilterAnyFiles, '*.*']);
  SaveDialog.Filter := ParseLineToFileFilter([rsFilterDirectoryHotListFiles, '*.hotlist']);
end;

{ TfrmOptionsDirectoryHotlist.Load }
procedure TfrmOptionsDirectoryHotlist.Load;
begin
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);

  pmCommandHelper := TPopupMenu.Create(Self);
  PopulatePopupMenuWithCommands(pmCommandHelper);

  btnRelativePath.Hint := rsMsgHotDirTipSpecialDirBut;
  btnRelativeTarget.Hint := rsMsgHotDirTipSpecialDirBut;
  cbSortHotDirPath.Hint := rsMsgHotDirTipOrderPath;
  cbSortHotDirTarget.Hint := rsMsgHotDirTipOrderTarget;
  cbAddTarget.Checked := gHotDirAddTargetOrNot;
  cbFullExpandTree.Checked := gHotDirFullExpandOrNot;
  cbShowPathInPopup.Checked := gShowPathInPopup;
  cbShowOnlyValidEnv.Checked := gShowOnlyValidEnv;

  rgWhereToAdd.ItemIndex := integer(gWhereToAddNewHotDir);

  CutAndPasteIndexList := TStringList.Create;
  CutAndPasteIndexList.Sorted := True;
  CutAndPasteIndexList.Duplicates := dupAccept;

  {$IFNDEF MSWINDOWS}
  miExportToTotalCommanderk.Free;
  miExportToTotalCommandernk.Free;
  miGotoConfigureTCInfo1.Free;
  miImportTotalCommander.Free;
  miGotoConfigureTCInfo2.Free;
  miSeparator13.Free;
  miSeparator14.Free;
  {$ENDIF}

  if DirectoryHotlistTemp = nil then
  begin
    DirectoryHotlistTemp := TDirectoryHotlist.Create;
    gDirectoryHotlist.CopyDirectoryHotlistToDirectoryHotlist(DirectoryHotlistTemp);
  end;

  tvDirectoryHotlist.Images := frmMain.imgLstDirectoryHotlist;
  DirectoryHotlistTemp.LoadTTreeView(tvDirectoryHotlist, -1);
  cbFullExpandTreeChange(cbFullExpandTree);
  if tvDirectoryHotlist.Items.Count > 0 then
    tvDirectoryHotlist.Items[0].Selected := True //Select at least first one by default
  else
    RefreshTreeView(nil); //If zero hot directory we will hide the directory path, disable export button, etc.
end;

{ TfrmOptionsDirectoryHotlist.Save }
function TfrmOptionsDirectoryHotlist.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  DirectoryHotlistTemp.RefreshFromTTreeView(tvDirectoryHotlist);
  DirectoryHotlistTemp.CopyDirectoryHotlistToDirectoryHotlist(gDirectoryHotlist);
  gHotDirAddTargetOrNot := cbAddTarget.Checked;
  gHotDirFullExpandOrNot := cbFullExpandTree.Checked;
  if gShowPathInPopup <> cbShowPathInPopup.Checked then
  begin
    gShowPathInPopup := cbShowPathInPopup.Checked;
    pmPathHelper.Items.Clear; //Let' re-populate it since option for environment variable path has changed...
    gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  end;

  if gShowOnlyValidEnv <> cbShowOnlyValidEnv.Checked then
  begin
    gShowOnlyValidEnv := cbShowOnlyValidEnv.Checked;
    LoadWindowsSpecialDir;
    pmPathHelper.Items.Clear; //Let' re-populate it since option for environment variabel path has changed...
    gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  end;
  gWhereToAddNewHotDir := TPositionWhereToAddHotDir(rgWhereToAdd.ItemIndex);
  cbFullExpandTreeChange(cbFullExpandTree);
end;

{ TfrmOptionsDirectoryHotlist.GetIconIndex }
class function TfrmOptionsDirectoryHotlist.GetIconIndex: integer;
begin
  Result := 33;
end;

{ TfrmOptionsDirectoryHotlist.GetTitle }
class function TfrmOptionsDirectoryHotlist.GetTitle: string;
begin
  Result := rsOptionsEditorDirectoryHotlist;
end;

{ TfrmOptionsDirectoryHotlist.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsDirectoryHotlist.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  Result := False;
end;

{ TfrmOptionsDirectoryHotlist.ExtraOptionsSignature }
function TfrmOptionsDirectoryHotlist.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  DirectoryHotlistTemp.RefreshFromTTreeView(tvDirectoryHotlist);
  Result := DirectoryHotlistTemp.ComputeSignature(CurrentSignature);
  Result := ComputeSignatureSingleComponent(rgWhereToAdd, Result);
  Result := ComputeSignatureSingleComponent(cbAddTarget, Result);
  Result := ComputeSignatureSingleComponent(cbFullExpandTree, Result);
  Result := ComputeSignatureSingleComponent(cbShowPathInPopup, Result);
  Result := ComputeSignatureSingleComponent(cbShowOnlyValidEnv, Result);
end;

{ TfrmOptionsDirectoryHotlist.Destroy }
destructor TfrmOptionsDirectoryHotlist.Destroy;
begin
  pmCommandHelper.Free;
  CutAndPasteIndexList.Free;
  inherited Destroy;
end;

{ TfrmOptionsDirectoryHotlist.SubmitToAddOrConfigToHotDirDlg }
procedure TfrmOptionsDirectoryHotlist.SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher: integer; paramPath, paramTarget: string; paramOptionalIndex: integer);
var
  TypeOfAddition, IndexFile: longint;
  sTempo: string;
  NodeToSelect: TTreeNode = nil;
  SelectedOrActiveDirectories: TFiles;

  procedure AddThisSubmittedDirectory(DirectoryPath: string);
  begin
    if ((paramActionDispatcher = ACTION_ADDTOHOTLIST) and (cbAddTarget.Checked)) or
      (paramActionDispatcher = ACTION_ADDBOTHTOHOTLIST) then sTempo := IncludeTrailingPathDelimiter(paramTarget) else sTempo := '';
    if gWhereToAddNewHotDir = ahdLast then TypeOfAddition := ACTION_ADDHOTDIR else TypeOfAddition := ACTION_INSERTHOTDIR;

    NodeToSelect := nil;
    if (tvDirectoryHotlist.Items.Count > 0) then
    begin
      case gWhereToAddNewHotDir of
        ahdFirst: NodeToSelect := tvDirectoryHotlist.Items.Item[0];
        ahdLast: NodeToSelect := tvDirectoryHotlist.Items.Item[pred(tvDirectoryHotlist.Items.Count)];
        ahdSmart: NodeToSelect := TryToGetCloserHotDir(DirectoryPath, TypeOfAddition);
        else
          NodeToSelect := tvDirectoryHotlist.Items.Item[0];
      end;
      if NodeToSelect <> nil then NodeToSelect.Selected := True;
    end;
    NodeToSelect := ActualAddDirectories(hd_CHANGEPATH, GetLastDir(DirectoryPath).Replace('&','&&'), DirectoryPath, sTempo, TypeOfAddition);
  end;

begin
  case paramActionDispatcher of
    ACTION_ADDTOHOTLIST, ACTION_ADDJUSTSOURCETOHOTLIST, ACTION_ADDBOTHTOHOTLIST:
    begin
      AddThisSubmittedDirectory(paramPath);
    end;

    ACTION_CONFIGTOHOTLIST:
    begin
      NodeToSelect := TryToGetCloserHotDir(paramPath, TypeOfAddition);
    end;

    ACTION_JUSTSHOWCONFIGHOTLIST:
    begin
      if tvDirectoryHotlist.Items.Count > 0 then NodeToSelect := tvDirectoryHotlist.Items.Item[0];
    end;

    ACTION_ADDSELECTEDDIR:
    begin
      SelectedOrActiveDirectories := frmMain.ActiveFrame.CloneSelectedOrActiveDirectories;
      try
        if SelectedOrActiveDirectories.Count > 0 then
        begin
          for IndexFile := 0 to pred(SelectedOrActiveDirectories.Count) do AddThisSubmittedDirectory(ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[IndexFile].FullPath));
        end;
      finally
        FreeAndNil(SelectedOrActiveDirectories);
      end;
    end;

    ACTION_DIRECTLYCONFIGENTRY:
    begin
      NodeToSelect := TryToGetExactHotDir(paramOptionalIndex);
    end
  end;

  if (NodeToSelect = nil) and (tvDirectoryHotlist.Items.Count > 0) then NodeToSelect := tvDirectoryHotlist.Items.Item[0];
  RefreshTreeView(NodeToSelect);

  //2014-08-27: These lines are a workaround a problem present at this moment in Lazarus regarding TSpeedButton present inside a TGroupBox.
  //See on the web if the following case is solved prior to remove these lines: http://bugs.freepascal.org/view.php?id=26638
  {$IFDEF MSWINDOWS}
  if tvDirectoryHotlist.CanFocus then
  begin
    LCLSendMouseDownMsg(Self, 1, 1, mbLeft, []);
    LCLSendMouseUpMsg(Self, 1, 1, mbLeft, []);
  end;
  {$ENDIF MSWINDOWS}

  if not tvDirectoryHotlist.Focused then if tvDirectoryHotlist.CanFocus then tvDirectoryHotlist.SetFocus;
  if not lbleditHotDirName.Focused then if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
end;

{ TfrmOptionsDirectoryHotlist.actInsertOrAddSomethingExecute }
procedure TfrmOptionsDirectoryHotlist.actInsertOrAddSomethingExecute(Sender: TObject);
var
  sPath, initialPath, stempo: string;
  AddOrInsertDispatcher, Dispatcher, Index: integer;
  MaybeNodeAfterAddition: TTreeNode = nil;
  NodeAfterAddition: TTreeNode = nil;
  SelectedOrActiveDirectories: TFiles;
begin
  Dispatcher := (TComponent(Sender).tag and $0F);
  AddOrInsertDispatcher := ((TComponent(Sender).tag and $F0) shr 4) + 1;

  sPath := '';

  case Dispatcher of
    1: //Directory I will browse to
    begin
      initialPath := '';
      if (tvDirectoryHotlist.Items.Count > 0) then
      begin
        if THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher = hd_CHANGEPATH then initialPath := mbExpandFileName(THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath);
      end;
      if initialPath = '' then initialPath := frmMain.ActiveFrame.CurrentPath;

      if SelectDirectory(rsSelectDir, initialPath, sPath, False) then
      begin
        NodeAfterAddition := ActualAddDirectories(hd_CHANGEPATH, GetLastDir(sPath), sPath, '', AddOrInsertDispatcher);
      end;
    end;

    2: //Directory I will type
    begin
      if cbAddTarget.Checked then
        sTempo := rsMsgHotDirTarget
      else
        sTempo := '';
      NodeAfterAddition := ActualAddDirectories(hd_CHANGEPATH, rsMsgHotDirName, rsMsgHotDirPath, sTempo, AddOrInsertDispatcher);
    end;

    3: //Directory of the active frame
    begin
      NodeAfterAddition := ActualAddDirectories(hd_CHANGEPATH, GetLastDir(frmMain.ActiveFrame.CurrentLocation), frmMain.ActiveFrame.CurrentLocation, '', AddOrInsertDispatcher);
    end;

    4: //Directory of the active AND inactive frames
    begin
      NodeAfterAddition := ActualAddDirectories(hd_CHANGEPATH, GetLastDir(frmMain.ActiveFrame.CurrentLocation), frmMain.ActiveFrame.CurrentLocation, frmMain.NotActiveFrame.CurrentLocation, AddOrInsertDispatcher);
    end;

    5: //Separator
    begin
      NodeAfterAddition := ActualAddDirectories(hd_SEPARATOR, HOTLIST_SEPARATORSTRING, '', '', AddOrInsertDispatcher);
    end;

    6: //SubMenu, a new branch
    begin
      NodeAfterAddition := ActualAddDirectories(hd_STARTMENU, rsMsgHotDirSubMenuName, '', '', AddOrInsertDispatcher);
      tvDirectoryHotlist.ClearSelection(True);
      NodeAfterAddition.Selected := True;
      ActualAddDirectories(hd_CHANGEPATH, rsMsgHotDirName, rsMsgHotDirPath, sTempo, 3);
      NodeAfterAddition.Expand(False);
      tvDirectoryHotlist.SetFocus; //The fact to set momentary the focus here, even if we will lose it later on in the function is good anyway. Why? Because when focus will be given to the TLabeledEdit later, the whole content will be selected at that moment instead of just having the cursor flashing on start. That's good because 99.9% of the time, we'll need to rename the submenu anyway.
    end;

    7: //Copy of entry
    begin
      NodeAfterAddition := ActualAddDirectories(THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher, THotDir(tvDirectoryHotlist.Selected.Data).HotDirName, THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath, THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget, AddOrInsertDispatcher);
    end;

    8: //A command
    begin
      NodeAfterAddition := ActualAddDirectories(hd_COMMAND, rsMsgHotDirCommandName, rsMsgHotDirCommandSample, '', AddOrInsertDispatcher);
    end;

    9: //Current selected directories of active frame
    begin
      SelectedOrActiveDirectories := frmMain.ActiveFrame.CloneSelectedOrActiveDirectories;
      try
        if SelectedOrActiveDirectories.Count > 0 then
        begin
          if AddOrInsertDispatcher = 1 then
          begin  //When we INSERT, which mean BEFORE the selection, let's do it this way so last insert will be just above the previous selection AND ready to edit
            for Index := 0 to pred(SelectedOrActiveDirectories.Count) do
            begin
              MaybeNodeAfterAddition := ActualAddDirectories(hd_CHANGEPATH,
                GetLastDir(ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[Index].FullPath)),
                ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[Index].FullPath),
                '',
                AddOrInsertDispatcher);
              if NodeAfterAddition = nil then NodeAfterAddition := MaybeNodeAfterAddition;
            end;
          end
          else
          begin //When we ADD, which mean AFTER the selection, let's do it this way so last addition will be just below the previous selection AND will be the first one that selected in active frame
            for Index := pred(SelectedOrActiveDirectories.Count) downto 0 do
            begin
              NodeAfterAddition := ActualAddDirectories(hd_CHANGEPATH,
                GetLastDir(ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[Index].FullPath)),
                ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[Index].FullPath),
                '',
                AddOrInsertDispatcher);

            end;
          end;
        end;
      finally
        FreeAndNil(SelectedOrActiveDirectories);
      end;
    end;
  end;

  if NodeAfterAddition <> nil then
  begin
    tvDirectoryHotlist.ClearSelection(True);
    tvDirectoryHotlist.Select(NodeAfterAddition);
    if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actDeleteSomethingExecute }
procedure TfrmOptionsDirectoryHotlist.actDeleteSomethingExecute(Sender: TObject);
var
  DeleteDispatcher: integer;
  FlagQuitDeleting: boolean;
  Answer: TMyMsgResult;
  NodeAfterDeletion: TTreeNode = nil;
  isTreeHadFocus: boolean = False;

  procedure DeleteSelectionAndSetNodeAfterDeletion;
  begin
    if tvDirectoryHotList.Selections[0].GetNextSibling <> nil then NodeAfterDeletion := tvDirectoryHotList.Selections[0].GetNextSibling  else
    if tvDirectoryHotList.Selections[0].GetPrevSibling <> nil then NodeAfterDeletion := tvDirectoryHotList.Selections[0].GetPrevSibling else
    if tvDirectoryHotList.Selections[0].Parent <> nil then NodeAfterDeletion := tvDirectoryHotList.Selections[0].Parent else
      NodeAfterDeletion := nil;
    tvDirectoryHotList.Selections[0].Delete;
    ClearCutAndPasteList;
  end;

begin
  if tvDirectoryHotlist.SelectionCount > 0 then
  begin
    isTreeHadFocus := tvDirectoryHotlist.Focused;
    tvDirectoryHotlist.Enabled := False;
    try
      with Sender as TComponent do DeleteDispatcher := tag;
      FlagQuitDeleting := False;

      //It's funny but as long we have something selected, we delete it and it will be index 0 since when
      //deleting something, the "Selections" array is updated!
      while (tvDirectoryHotList.SelectionCount > 0) and (not FlagQuitDeleting) do
      begin
        if tvDirectoryHotList.Selections[0].GetFirstChild = nil then
        begin
          DeleteSelectionAndSetNodeAfterDeletion;
        end
        else
        begin
          case DeleteDispatcher of
            1: Answer := MsgBox(Format(rsMsgHotDirWhatToDelete, [tvDirectoryHotList.Selections[0].Text]), [msmbAll, msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
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
              NodeAfterDeletion := tvDirectoryHotList.Selections[0].GetFirstChild;
              repeat
                tvDirectoryHotList.Selections[0].GetFirstChild.MoveTo(tvDirectoryHotList.Selections[0].GetFirstChild.Parent, naInsert);
              until tvDirectoryHotList.Selections[0].GetFirstChild = nil;
              tvDirectoryHotList.Selections[0].Delete;
              ClearCutAndPasteList;
            end;

            else
              FlagQuitDeleting := True;
          end;
        end;
      end;
      if (NodeAfterDeletion = nil) and (FlagQuitDeleting = False) and (tvDirectoryHotList.Items.Count > 0) then NodeAfterDeletion := tvDirectoryHotList.Items.Item[0];
      if (NodeAfterDeletion <> nil) and (FlagQuitDeleting = False) then NodeAfterDeletion.Selected := True;
    finally
      tvDirectoryHotlist.Enabled := True;
      if isTreeHadFocus and tvDirectoryHotlist.CanFocus then tvDirectoryHotlist.SetFocus;
    end;
  end;
end;


{ TfrmOptionsDirectoryHotlist.actDeleteAllExecute }
procedure TfrmOptionsDirectoryHotlist.actDeleteAllExecute(Sender: TObject);
begin
  if MsgBox(rsMsgHotDirDeleteAllEntries, [msmbYes, msmbNo], msmbNo, msmbNo) = mmrYes then
  begin
    tvDirectoryHotlist.Items.Clear;
    lbleditHotDirName.Text := '';
    lbleditHotDirPath.Text := '';
    lbleditHotDirTarget.Text := '';
    ClearCutAndPasteList;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actMoveToPreviousExecute }
procedure TfrmOptionsDirectoryHotlist.actMoveToPreviousExecute(Sender: TObject);
var
  AOriginalSelectedNode, AAboveNode: TTreeNode;
begin
  AOriginalSelectedNode := tvDirectoryHotlist.Selected;
  if AOriginalSelectedNode <> nil then
  begin
    tvDirectoryHotlist.MoveToPrevNode(False);
    AAboveNode := tvDirectoryHotlist.Selected;
    if AOriginalSelectedNode <> AAboveNode then
    begin
      AOriginalSelectedNode.MoveTo(AAboveNode, naInsert);
      tvDirectoryHotlist.Select(AOriginalSelectedNode);
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actMoveToNextExecute }
procedure TfrmOptionsDirectoryHotlist.actMoveToNextExecute(Sender: TObject);
var
  AOriginalSelectedNode, ABelowNode: TTreeNode;
begin
  AOriginalSelectedNode := tvDirectoryHotlist.Selected;
  if AOriginalSelectedNode <> nil then
  begin
    tvDirectoryHotlist.MoveToNextNode(False);
    ABelowNode := tvDirectoryHotlist.Selected;
    if AOriginalSelectedNode <> ABelowNode then
    begin
      AOriginalSelectedNode.MoveTo(ABelowNode, naInsertBehind);
      tvDirectoryHotlist.Select(AOriginalSelectedNode);
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actCutExecute }
procedure TfrmOptionsDirectoryHotlist.actCutExecute(Sender: TObject);
var
  Index: integer;
begin
  if tvDirectoryHotlist.SelectionCount > 0 then
  begin
    for Index := 0 to pred(tvDirectoryHotlist.SelectionCount) do
    begin
      CutAndPasteIndexList.Add(IntToStr(tvDirectoryHotlist.Selections[Index].AbsoluteIndex));
    end;

    actPaste.Enabled := True;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actPasteExecute }
procedure TfrmOptionsDirectoryHotlist.actPasteExecute(Sender: TObject);
var
  DestinationNode: TTreeNode;
  Index: longint;
begin
  if CutAndPasteIndexList.Count > 0 then
  begin
    DestinationNode := tvDirectoryHotlist.Selected;
    if DestinationNode <> nil then
    begin
      tvDirectoryHotlist.ClearSelection(False);
      for Index := 0 to pred(CutAndPasteIndexList.Count) do
      begin
        tvDirectoryHotlist.Items.Item[StrToInt(CutAndPasteIndexList.Strings[Index])].Selected := True;
      end;

      for Index := 0 to pred(tvDirectoryHotlist.SelectionCount) do
      begin
        tvDirectoryHotlist.Selections[Index].MoveTo(DestinationNode, naInsert);
      end;
      ClearCutAndPasteList;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actSearchAndReplaceExecute }
procedure TfrmOptionsDirectoryHotlist.actSearchAndReplaceExecute(Sender: TObject);
var
  NbOfReplacement: longint;
  sSearchText, sReplaceText: string;
  ReplaceFlags: TReplaceFlags;

  function ReplaceIfNecessary(sWorkingText: string): string;
  begin
    Result := StringReplace(sWorkingText, sSearchText, sReplaceText, ReplaceFlags);
    if Result <> sWorkingText then Inc(NbOfReplacement);
  end;

var
  Index, ActionDispatcher: integer;
  EditSearchOptionToOffer: TEditSearchDialogOption;
  EditSearchOptionReturned: TEditSearchDialogOption = [];
begin
  with Sender as TComponent do ActionDispatcher := tag;

  if ((ActionDispatcher and $01) <> 0) and (lbleditHotDirPath.Text <> '') then sSearchText := lbleditHotDirPath.Text
  else if ((ActionDispatcher and $02) <> 0) and (lbleditHotDirTarget.Text <> '') then sSearchText := lbleditHotDirTarget.Text
  else sSearchText := '';
  sReplaceText := sSearchText;

  EditSearchOptionToOffer := [];
  {$IFDEF MSWINDOWS}
  EditSearchOptionToOffer := EditSearchOptionToOffer + [eswoCaseSensitiveUnchecked];
  {$ELSE}
  EditSearchOptionToOffer := EditSearchOptionToOffer + [eswoCaseSensitiveChecked];
  {$ENDIF}

  if GetSimpleSearchAndReplaceString(self, EditSearchOptionToOffer, sSearchText, sReplaceText, EditSearchOptionReturned, glsSearchPathHistory, glsReplacePathHistory) then
  begin
    NbOfReplacement := 0;
    ReplaceFlags := [rfReplaceAll];
    if eswoCaseSensitiveUnchecked in EditSearchOptionReturned then ReplaceFlags := ReplaceFlags + [rfIgnoreCase];

    for Index := 0 to pred(gDirectoryHotlist.Count) do
    begin
      case DirectoryHotlistTemp.HotDir[Index].Dispatcher of
        hd_CHANGEPATH:
        begin
          if (ActionDispatcher and $01) <> 0 then DirectoryHotlistTemp.HotDir[Index].HotDirPath := ReplaceIfNecessary(DirectoryHotlistTemp.HotDir[Index].HotDirPath);
          if (ActionDispatcher and $02) <> 0 then DirectoryHotlistTemp.HotDir[Index].HotDirTarget := ReplaceIfNecessary(DirectoryHotlistTemp.HotDir[Index].HotDirTarget);
        end;
      end;
    end;

    if NbOfReplacement = 0 then
    begin
      msgOk(rsZeroReplacement);
    end
    else
    begin
      tvDirectoryHotlistSelectionChanged(tvDirectoryHotlist);
      msgOk(format(rsXReplacements, [NbOfReplacement]));
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actTweakPathExecute }
procedure TfrmOptionsDirectoryHotlist.actTweakPathExecute(Sender: TObject);
  procedure ShowPopupMenu(APopupMenu: TPopupMenu; ASpeedButton: TSpeedButton);
  var
    ptPopupLocation: TPoint;
  begin
    APopupMenu.tag := ASpeedButton.tag;
    ptPopupLocation := ASpeedButton.ClientToScreen(Point(ASpeedButton.Width - 10, ASpeedButton.Height - 10));
    Mouse.CursorPos := Point(ptPopupLocation.x + 8, ptPopupLocation.y + 8);
    APopupMenu.PopUp(ptPopupLocation.x, ptPopupLocation.y);
  end;

begin
  with Sender as TComponent do
  begin
    case tag of
      2:
      begin
        lbleditHotDirPath.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirPath, pfPATH);
        ShowPopupMenu(pmPathHelper,btnRelativePath);
      end;

      3:
      begin
        lbleditHotDirTarget.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirTarget, pfPATH);
        ShowPopupMenu(pmPathHelper,btnRelativeTarget);
      end;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.actFocusTreeWindowExecute }
procedure TfrmOptionsDirectoryHotlist.actFocusTreeWindowExecute(Sender: TObject);
begin
  if tvDirectoryHotlist.CanSetFocus then tvDirectoryHotlist.SetFocus;
end;

{ TfrmOptionsDirectoryHotlist.actGotoFirstItemExecute }
procedure TfrmOptionsDirectoryHotlist.actGotoFirstItemExecute(Sender: TObject);
begin
  if tvDirectoryHotlist.Items.Count > 0 then
    tvDirectoryHotlist.Select(tvDirectoryHotlist.Items[0]);
end;

{ TfrmOptionsDirectoryHotlist.actGoToPreviousItemExecute }
procedure TfrmOptionsDirectoryHotlist.actGoToPreviousItemExecute(Sender: TObject);
begin
  tvDirectoryHotlist.MoveToPrevNode(False);
end;

{ TfrmOptionsDirectoryHotlist.actGoToNextItemExecute }
procedure TfrmOptionsDirectoryHotlist.actGoToNextItemExecute(Sender: TObject);
begin
  tvDirectoryHotlist.MoveToNextNode(False);
end;

{ TfrmOptionsDirectoryHotlist.actGotoLastItemExecute }
// Go to the last item that is displayable *without* opening a branche that is not already open.
procedure TfrmOptionsDirectoryHotlist.actGotoLastItemExecute(Sender: TObject);
var
  SeekingNode: TTreeNode;
  LastGoodNode: TTreeNode = nil;
begin
  if tvDirectoryHotlist.Items.Count > 0 then
  begin
    SeekingNode := tvDirectoryHotlist.Items[0];

    while SeekingNode <> nil do
    begin
      SeekingNode := SeekingNode.GetNextSibling;
      if SeekingNode <> nil then
      begin
        LastGoodNode := SeekingNode;
      end
      else
      begin
        if LastGoodNode.Expanded then
          SeekingNode := LastGoodNode.Items[0];
      end;
    end;
  end;

  if LastGoodNode <> nil then
    tvDirectoryHotlist.Select(LastGoodNode);
end;

{ TfrmOptionsDirectoryHotlist.actExpandItemExecute }
procedure TfrmOptionsDirectoryHotlist.actExpandItemExecute(Sender: TObject);
begin
  if tvDirectoryHotlist.Selected <> nil then
    if tvDirectoryHotlist.Selected.TreeNodes.Count > 0 then
      tvDirectoryHotlist.Selected.Expand(False);
end;

{ TfrmOptionsDirectoryHotlist.actOpenAllBranchesExecute }
procedure TfrmOptionsDirectoryHotlist.actOpenAllBranchesExecute(Sender: TObject);
begin
  tvDirectoryHotlist.FullExpand;
  if tvDirectoryHotlist.Selected <> nil then
    tvDirectoryHotlist.Selected.MakeVisible;
end;

{ TfrmOptionsDirectoryHotlist.actCollapseItemExecute }
procedure TfrmOptionsDirectoryHotlist.actCollapseItemExecute(Sender: TObject);
begin
  if tvDirectoryHotlist.Selected <> nil then
    if tvDirectoryHotlist.Selected.TreeNodes.Count > 0 then
      tvDirectoryHotlist.Selected.Collapse(True);
end;

{ TfrmOptionsDirectoryHotlist.actCollapseAllExecute }
procedure TfrmOptionsDirectoryHotlist.actCollapseAllExecute(Sender: TObject);
begin
  tvDirectoryHotlist.FullCollapse;
  if tvDirectoryHotlist.Selected <> nil then
    tvDirectoryHotlist.Selected.MakeVisible;
end;

{ TfrmOptionsDirectoryHotlist.miSortDirectoryHotlistClick }
//The trick here is that a "group number" identical has been assigned to the sibling between separator and then we sort
//Teh sort has been arrange in such way that item from different group won't be mixed.
procedure TfrmOptionsDirectoryHotlist.miSortDirectoryHotlistClick(Sender: TObject);
var
  Dispatcher, Index: integer;
  StartingNode: TTreeNode;
  FlagKeepGoingBack: boolean;
begin
  with Sender as TComponent do Dispatcher := tag;
  for Index := 0 to pred(tvDirectoryHotlist.Items.Count) do THotDir(tvDirectoryHotlist.Items.Item[Index].Data).GroupNumber := 0;

  GlobalGroupNumber := 0;

  if tvDirectoryHotlist.SelectionCount > 0 then
  begin
    case Dispatcher of
      1, 2: //current group only or current level
      begin
        for Index := 0 to pred(tvDirectoryHotlist.SelectionCount) do
        begin
          if THotDir(tvDirectoryHotlist.Selections[Index].Data).GroupNumber = 0 then
          begin
            StartingNode := tvDirectoryHotlist.Selections[Index];

            case Dispatcher of
              1: //We just need to make sure we start from first item of current level so we search the first one OR a separator
              begin
                FlagKeepGoingBack := True;
                while FlagKeepGoingBack do
                begin
                  if StartingNode.GetPrevSibling <> nil then
                  begin
                    if THotDir(StartingNode.GetPrevSibling.Data).Dispatcher <> hd_SEPARATOR then
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
                while StartingNode.GetPrevSibling <> nil do StartingNode := StartingNode.GetPrevSibling;
              end;
            end;

            RecursiveSetGroupNumbers(StartingNode, GetNextGroupNumber, False, (Dispatcher = 1));
          end;
        end;
      end;

      3, 4: //submenu only, recusive or not
      begin
        for Index := 0 to pred(tvDirectoryHotlist.SelectionCount) do
        begin
          StartingNode := tvDirectoryHotlist.Selections[Index].GetFirstChild;
          if StartingNode <> nil then
          begin
            if THotDir(StartingNode.Data).GroupNumber = 0 then
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
    StartingNode := tvDirectoryHotlist.Items.Item[0];
    RecursiveSetGroupNumbers(StartingNode, GetNextGroupNumber, True, False);
  end;

  //... and the finale!
  tvDirectoryHotlist.CustomSort(@MySortViaGroup);
  ClearCutAndPasteList;
end;

{ TfrmOptionsDirectoryHotlist.miTestResultingHotlistMenuClick }
procedure TfrmOptionsDirectoryHotlist.miTestResultingHotlistMenuClick(Sender: TObject);
var
  p: TPoint;
begin
  DirectoryHotlistTemp.RefreshFromTTreeView(tvDirectoryHotlist); //We need to refresh our temporary Directory Hotlist in case user played with the tree and added/removed/moved item(s).
  DirectoryHotlistTemp.PopulateMenuWithHotDir(pmHotDirTestMenu, @miShowWhereItWouldGo, nil, mpJUSTHOTDIRS, 0);
  p := tvDirectoryHotlist.ClientToScreen(Classes.Point(0, 0));
  p.x := p.x + tvDirectoryHotlist.Width;
  pmHotDirTestMenu.PopUp(p.X, p.Y);
end;

{ TfrmOptionsDirectoryHotlist.miDetectIfPathExistClick }
procedure TfrmOptionsDirectoryHotlist.miDetectIfPathExistClick(Sender: TObject);
var
  iNodeIndex: integer;
  NodeToFocus: TTreeNode = nil;
  OriginalNode: TTreeNode;
begin
  OriginalNode := tvDirectoryHotlist.Selected;
  lbleditHotDirName.Text := '';
  lbleditHotDirName.Enabled := False;
  lbleditHotDirPath.Visible := False;
  cbSortHotDirPath.Visible := False;
  cbSortHotDirTarget.Visible := False;
  lbleditHotDirTarget.Visible := False;
  btnRelativePath.Visible := False;
  btnRelativeTarget.Visible := False;
  Application.ProcessMessages;
  try
    RefreshExistingProperty(TComponent(Sender).tag);
  finally
    lbleditHotDirName.Enabled := True;
    iNodeIndex := 0;
    while (iNodeIndex < tvDirectoryHotlist.Items.Count) and (NodeToFocus = nil) do
      if (THotDir(tvDirectoryHotlist.Items[iNodeIndex].Data).HotDirExisting = DirNotExist) and (tvDirectoryHotlist.Items[iNodeIndex].Count = 0) then NodeToFocus := tvDirectoryHotlist.Items[iNodeIndex] else Inc(iNodeIndex);

    if NodeToFocus <> nil then
      tvDirectoryHotlist.Select(NodeToFocus)
    else
    if OriginalNode <> nil then
      tvDirectoryHotlist.Select(OriginalNode);
    if lbleditHotDirName.CanSetFocus then
      lbleditHotDirName.SetFocus;
  end;
end;

{ TfrmOptionsDirectoryHotlist.miExportToAnythingClick }
//We could export to a few ways:
// 0x00:To TC and keeping existing hotlist
// 0x80:To TC after erasing existing Directory Hotlist
// 0x01:To a Directory Hotlist file (.hotlist)
// 0x02:To a backup file (all of them, to a "BACKUP_YYYY-MM-DD-HH-MM-SS.hotlist" file)
//      With the backup, we don't ask what to output, we output everything!
procedure TfrmOptionsDirectoryHotlist.miExportToAnythingClick(Sender: TObject);
var
  FlagKeepGoing: boolean = False;
  ActionDispatcher: integer;
  BackupPath: string;
  WorkingDirectoryHotlist: TDirectoryHotlist;
  Answer: integer;
begin
  WorkingDirectoryHotlist := TDirectoryHotlist.Create;
  try
    with Sender as TComponent do
      ActionDispatcher := tag;

    case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
      {$IFDEF MSWINDOWS}
      ACTION_WITH_WINCMDINI:
      begin
        if areWeInSituationToPlayWithTCFiles then
        begin
          OpenDialog.Filename := gTotalCommanderConfigFilename;
          FlagKeepGoing := True;
        end;
      end;
      {$ENDIF}

      ACTION_WITH_HOTLISTFILE:
      begin
        SaveDialog.DefaultExt := '*.hotlist';
        SaveDialog.FilterIndex := 1;
        SaveDialog.Title := rsMsgHotDirWhereToSave;
        SaveDialog.FileName := 'New Directory Hotlist';
        FlagKeepGoing := SaveDialog.Execute;
      end;

      ACTION_WITH_BACKUP:
      begin
        BackupPath := IncludeTrailingPathDelimiter(mbExpandFileName(EnvVarConfigPath)) + 'Backup';
        if mbForceDirectory(BackupPath) then
        begin
          SaveDialog.Filename := BackupPath + DirectorySeparator + 'Backup_' + GetDateTimeInStrEZSortable(now) + '.hotlist';
          if gDirectoryHotlist.ExportDoubleCommander(SaveDialog.FileName, True) then
            msgOK(Format(rsMsgHotDirTotalBackuped, [gDirectoryHotlist.Count, SaveDialog.Filename]))
          else
            msgError(rsMsgHotDirErrorBackuping);
        end;
        Exit;
      end;
    end;

    //User select what to export
    if FlagKeepGoing then
    begin
      with Tfrmhotdirexportimport.Create(Application) do
      begin
        try
          CopyTTreeViewToAnother(tvDirectoryHotlist, tvDirectoryHotlistToExportImport);
          btnSelectAll.Caption := rsMsgHotDirExportall;
          btnSelectionDone.Caption := rsMsgHotDirExportSel;
          Caption := rsMsgHotDirExportHotlist;

          Answer := ShowModal;

          if ((Answer = mrOk) and (tvDirectoryHotlistToExportImport.SelectionCount > 0)) or ((Answer = mrAll) and (tvDirectoryHotlistToExportImport.Items.Count > 0)) then
          begin
            WorkingDirectoryHotlist.AddFromAnotherTTreeViewTheSelected(nil, tvDirectoryHotlistToExportImport, (Answer = mrAll));

            if WorkingDirectoryHotlist.Count > 0 then
            begin
              case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
                {$IFDEF MSWINDOWS}
                ACTION_WITH_WINCMDINI: if WorkingDirectoryHotlist.ExportTotalCommander(OpenDialog.FileName, ((ActionDispatcher and MASK_FLUSHORNOT_EXISTING) = ACTION_ERASEEXISTING)) then msgOK(rsMsgHotDirTotalExported + IntToStr(WorkingDirectoryHotlist.Count)) else msgError(rsMsgHotDirErrorExporting);
                {$ENDIF}
                ACTION_WITH_HOTLISTFILE: if WorkingDirectoryHotlist.ExportDoubleCommander(SaveDialog.FileName, True) then msgOK(rsMsgHotDirTotalExported + IntToStr(WorkingDirectoryHotlist.Count)) else msgError(rsMsgHotDirErrorExporting);
              end;
            end
            else
            begin
              msgOK(rsMsgHotDirNothingToExport);
            end;
          end; //If user confirmed OK and have selected something...
        finally
          Free;
        end;
      end;
    end;
  finally
    WorkingDirectoryHotlist.Free;
  end;
end;

{ TfrmOptionsDirectoryHotlist.miImportFromAnythingClick }
//We could import from a few ways:
// 0x00:From TC
// 0x01:From a Directory Hotlist file (.hotlist)
// 0x02:From a backup file (all of them, to a "BACKUP_YYYY-MM-DD-HH-MM-SS.hotlist" file)
//      With the backup, we erase existing ones
procedure TfrmOptionsDirectoryHotlist.miImportFromAnythingClick(Sender: TObject);
var
  WorkingDirectoryList: TDirectoryHotlist;
  Answer, NbOfAdditional, ActionDispatcher: longint;
  FlagKeepGoing: boolean = False;
  BackupPath: string;
begin
  with Sender as TComponent do
    ActionDispatcher := tag;

  case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
    ACTION_WITH_HOTLISTFILE:
    begin
      OpenDialog.DefaultExt := '*.hotlist';
      OpenDialog.FilterIndex := 1;
      OpenDialog.Title := rsMsgHotDirLocateHotlistFile;
      FlagKeepGoing := OpenDialog.Execute;
    end;

    ACTION_WITH_BACKUP:
    begin
      BackupPath := IncludeTrailingPathDelimiter(mbExpandFileName(EnvVarConfigPath)) + 'Backup';
      if mbForceDirectory(BackupPath) then
      begin
        OpenDialog.DefaultExt := '*.hotlist';
        OpenDialog.FilterIndex := 1;
        OpenDialog.Title := rsMsgHotDirRestoreWhat;
        OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(BackupPath);
        FlagKeepGoing := OpenDialog.Execute;
      end;
    end;

    {$IFDEF MSWINDOWS}
    ACTION_WITH_WINCMDINI:
    begin
      if areWeInSituationToPlayWithTCFiles then
      begin
        OpenDialog.FileName := gTotalCommanderConfigFilename;
        FlagKeepGoing := True;
      end;
    end;
    {$ENDIF}
  end;

  if FlagKeepGoing then
  begin
    WorkingDirectoryList := TDirectoryHotlist.Create;
    try
      case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
        {$IFDEF MSWINDOWS}
        ACTION_WITH_WINCMDINI: WorkingDirectoryList.ImportTotalCommander(string(OpenDialog.Filename));
        {$ENDIF}
        ACTION_WITH_HOTLISTFILE: WorkingDirectoryList.ImportDoubleCommander(string(OpenDialog.Filename));
        ACTION_WITH_BACKUP: WorkingDirectoryList.ImportDoubleCommander(string(OpenDialog.Filename));
      end;

      with Tfrmhotdirexportimport.Create(Application) do
      begin
        try
          WorkingDirectoryList.LoadTTreeView(tvDirectoryHotlistToExportImport, -1);
          btnSelectAll.Caption := rsMsgHotDirImportall;
          btnSelectionDone.Caption := rsMsgHotDirImportSel;
          Caption := rsMsgHotDirImportHotlist;

          case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
            ACTION_WITH_HOTLISTFILE: Answer := ShowModal;
            ACTION_WITH_BACKUP:
            begin
              if MsgBox(rsHotDirWarningAbortRestoreBackup, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel) = mmrYes then Answer := mrAll else Exit;
            end;
            ACTION_WITH_WINCMDINI: Answer := ShowModal;
          end;

          if ((Answer = mrOk) and (tvDirectoryHotlistToExportImport.SelectionCount > 0)) or ((Answer = mrAll) and (tvDirectoryHotlistToExportImport.Items.Count > 0)) then
          begin
            ClearCutAndPasteList;

            if ((ActionDispatcher and MASK_ACTION_WITH_WHAT) = ACTION_WITH_BACKUP) and (Answer = mrAll) then
            begin
              DirectoryHotlistTemp.Clear;
              tvDirectoryHotlist.Items.Clear;
            end;

            NbOfAdditional := DirectoryHotlistTemp.AddFromAnotherTTreeViewTheSelected(tvDirectoryHotlist, tvDirectoryHotlistToExportImport, (Answer = mrAll));
            if NbOfAdditional > 0 then
            begin
              //DirectoryHotlistTemp.LoadTTreeView(tvDirectoryHotlist,-1);
              tvDirectoryHotlist.ClearSelection(True);
              if tvDirectoryHotlist.Items.Count > 0 then
                tvDirectoryHotlist.Select(tvDirectoryHotlist.Items[pred(tvDirectoryHotlist.Items.Count)]);
              if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
              msgOK(format(rsMsgHotDirNbNewEntries, [NbOfAdditional]));
            end;
          end; //If user confirmed OK and have selected something...
        finally
          Free;
        end;
      end;
    finally
      WorkingDirectoryList.Free;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.miGotoConfigureTCInfoClick }
procedure TfrmOptionsDirectoryHotlist.miGotoConfigureTCInfoClick(Sender: TObject);
begin
  BringUsToTCConfigurationPage;
end;

{ TfrmOptionsDirectoryHotlist.btnActionClick }
procedure TfrmOptionsDirectoryHotlist.btnActionClick(Sender: TObject);
begin
  case TComponent(Sender).tag of
    1: pmInsertDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    2: pmAddDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    3: pmDeleteDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    4: pmExportDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    5: pmImportDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    6: pmBackupDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    7: pmMiscellaneousDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    8: pmSortDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

{ TfrmOptionsDirectoryHotlist.btnHelpClick }
procedure TfrmOptionsDirectoryHotlist.btnHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', '/directoryhotlist.html');
end;

{ TfrmOptionsDirectoryHotlist.cbFullExpandTreeChange }
procedure TfrmOptionsDirectoryHotlist.cbFullExpandTreeChange(Sender: TObject);
begin
  if cbFullExpandTree.Checked then tvDirectoryHotlist.FullExpand else tvDirectoryHotlist.FullCollapse;
end;

procedure TfrmOptionsDirectoryHotlist.lbleditHotDirNameChange(Sender: TObject);
begin
  //If nothing currently selected, no need to update anything here.
  if (tvDirectoryHotlist.Selected <> nil) and (TLabeledEdit(Sender).Enabled) then
  begin
    case TLabeledEdit(Sender).tag of
      1: //Hot dir name
      begin
        try
          //Make sure we actually have something, not an attempt of submenu or end of menu
          if (TLabeledEdit(Sender).Text <> '') and (TLabeledEdit(Sender).Text[1] <> '-') and (THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher <> hd_SEPARATOR) then
          begin
            //Make sure it's different than what it was
            if THotDir(tvDirectoryHotlist.Selected.Data).HotDirName <> TLabeledEdit(Sender).Text then
            begin
              THotDir(tvDirectoryHotlist.Selected.Data).HotDirName := TLabeledEdit(Sender).Text;
              tvDirectoryHotlist.Selected.Text := TLabeledEdit(Sender).Text;
            end;
          end;
        except
          //Just in case the "Text" is empty to don't show error with Text[1] check.
        end;
      end;

      2: //Hot dir path
      begin
        try
          //if (TLabeledEdit(Sender).Text <> '') and (THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher = hd_CHANGEPATH) then
          //  TLabeledEdit(Sender).Text := IncludeTrailingPathDelimiter(TLabeledEdit(Sender).Text);

          //Make sure it's different than what it was
          if THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath <> TLabeledEdit(Sender).Text then
          begin
            THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath := TLabeledEdit(Sender).Text;
            THotDir(tvDirectoryHotlist.Selected.Data).HotDirExisting := DirExistUnknown;
          end;
        except
          //Just in case we have an empty list so "DirectoryHotlistTemp.HotDir[tvDirectoryHotlist.Selected.ImageIndex]" will not caused an error (since ItemIndex=-1 at this moment);
        end;
      end;

      3: //Hot dir target
      begin
        try
          //if (TLabeledEdit(Sender).Text <> '') and (THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher =hd_CHANGEPATH) then
          //  TLabeledEdit(Sender).Text := IncludeTrailingPathDelimiter(TLabeledEdit(Sender).Text);

          //Make sure it's different than what it was
          if THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget <> TLabeledEdit(Sender).Text then
          begin
            THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget := TLabeledEdit(Sender).Text;
          end;
        except
          //Just in case we have an empty list so "DirectoryHotlistTemp.HotDir[tvDirectoryHotlist.Selected.ImageIndex]" will not caused an error (since ItemIndex=-1 at this moment);
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.anyRelativeAbsolutePathClick }
procedure TfrmOptionsDirectoryHotlist.anyRelativeAbsolutePathClick(Sender: TObject);
begin
  if tvDirectoryHotlist.Selected<>nil then //Should not happen, but if it happens, will avoid an error.
  begin
    case TComponent(Sender).tag of
      2:
      begin
        lbleditHotDirPath.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirPath, pfPATH);
        pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        if THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath <> lbleditHotDirPath.Text then
          THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath := lbleditHotDirPath.Text;
      end;

      3:
      begin
        lbleditHotDirTarget.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirTarget, pfPATH);
        pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        if THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget <> lbleditHotDirTarget.Text then
          THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget := lbleditHotDirTarget.Text;
      end;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.cbSortHotDirPathChange }
procedure TfrmOptionsDirectoryHotlist.cbSortHotDirPathChange(Sender: TObject);
begin
  if Assigned(tvDirectoryHotlist.Selected) then
    THotDir(tvDirectoryHotlist.Selected.Data).HotDirPathSort := cbSortHotDirPath.ItemIndex;
end;

{ TfrmOptionsDirectoryHotlist.cbSortHotDirTargetChange }
procedure TfrmOptionsDirectoryHotlist.cbSortHotDirTargetChange(Sender: TObject);
begin
  if Assigned(tvDirectoryHotlist.Selected) then
    THotDir(tvDirectoryHotlist.Selected.Data).HotDirTargetSort := cbSortHotDirTarget.ItemIndex;
end;

{ TfrmOptionsDirectoryHotlist.pnlButtonsResize }
procedure TfrmOptionsDirectoryHotlist.pnlButtonsResize(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to pnlButtons.ControlCount - 1 do
  begin
    pnlButtons.Controls[I].Width := pnlButtons.ClientWidth div 2 - 3;
  end;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragDrop }
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragDrop(Sender, Source: TObject; X, Y: integer);
var
  Index: longint;
  DestinationNode: TTreeNode;
begin
  DestinationNode := tvDirectoryHotlist.GetNodeAt(X, Y);

  if Assigned(DestinationNode) and (tvDirectoryHotlist.SelectionCount > 0) then
  begin
    //If we move toward the end, we place the moved item *after* the destination.
    //If we move toward the beginning, we place the moved item *before* the destination.
    if tvDirectoryHotlist.Selections[pred(tvDirectoryHotlist.SelectionCount)].AbsoluteIndex > DestinationNode.AbsoluteIndex then
    begin
      for Index := 0 to pred(tvDirectoryHotlist.SelectionCount) do
      begin
        tvDirectoryHotlist.Selections[Index].MoveTo(DestinationNode, naInsert);
      end;
    end
    else
    begin
      for Index := 0 to pred(tvDirectoryHotlist.SelectionCount) do
      begin
        tvDirectoryHotlist.Selections[Index].MoveTo(DestinationNode, naInsertBehind);
      end;
    end;
    ClearCutAndPasteList;
  end;

  actPaste.Enabled := False;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragOver }
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistEnter }
// To help to catch eye's attention, let's change color of selection when tree get/lose the focus
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistEnter(Sender: TObject);
begin
  tvDirectoryHotlist.SelectionColor := clHighlight;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistExit }
// To help to catch eye's attention, let's change color of selection when tree get/lose the focus
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistExit(Sender: TObject);
begin
  tvDirectoryHotlist.SelectionColor := clBtnShadow;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistSelectionChanged }
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistSelectionChanged(Sender: TObject);
var
  WorkingPointer: Pointer;
begin
  if tvDirectoryHotlist.Selected <> nil then
  begin
    WorkingPointer := tvDirectoryHotlist.Selected.Data;

    case THotDir(WorkingPointer).Dispatcher of
      hd_NULL:
      begin
      end;

      hd_CHANGEPATH:
      begin
        lbleditHotDirName.EditLabel.Caption := rsMsgHotDirSimpleName;
        lbleditHotDirName.Text := THotDir(WorkingPointer).HotDirName;
        lbleditHotDirName.ReadOnly := False;

        lbleditHotDirPath.EditLabel.Caption := rsMsgHotDirJustPath;
        lbleditHotDirPath.Text := THotDir(WorkingPointer).HotDirPath; //DirectoryHotlistTemp.HotDir[IndexInHotlist].HotDirPath;
        lbleditHotDirPath.Hint := mbExpandFileName(lbleditHotDirPath.Text);
        cbSortHotDirPath.ItemIndex := THotDir(WorkingPointer).HotDirPathSort;
        lbleditHotDirPath.Visible := True;
        btnRelativePath.Tag := 2;

        lbleditHotDirTarget.Text := THotDir(WorkingPointer).HotDirTarget;
        lbleditHotDirTarget.Hint := mbExpandFileName(lbleditHotDirTarget.Text);
        cbSortHotDirTarget.ItemIndex := THotDir(WorkingPointer).HotDirTargetSort;
        lbleditHotDirTarget.Visible := True;
      end;

      hd_COMMAND:
      begin
        lbleditHotDirName.EditLabel.Caption := rsMsgHotDirSimpleName;
        lbleditHotDirName.Text := THotDir(WorkingPointer).HotDirName;
        lbleditHotDirName.ReadOnly := False;

        lbleditHotDirPath.EditLabel.Caption := rsMsgHotDirSimpleCommand;
        lbleditHotDirPath.Text := THotDir(WorkingPointer).HotDirPath;
        lbleditHotDirPath.Hint := '';
        lbleditHotDirPath.Visible := True;
        btnRelativePath.Tag := 4;
        lbleditHotDirTarget.Visible := False;
      end;

      hd_SEPARATOR:
      begin
        lbleditHotDirName.EditLabel.Caption := '';
        lbleditHotDirName.Text := rsMsgHotDirSimpleSeparator;
        lbleditHotDirName.ReadOnly := True;

        lbleditHotDirPath.Visible := False;
        lbleditHotDirTarget.Visible := False;
      end;

      hd_STARTMENU:
      begin
        lbleditHotDirName.EditLabel.Caption := rsMsgHotDirSimpleMenu;
        lbleditHotDirName.Text := THotDir(WorkingPointer).HotDirName;
        lbleditHotDirName.ReadOnly := False;

        lbleditHotDirPath.Visible := False;
        lbleditHotDirTarget.Visible := False;
      end;

      hd_ENDMENU:
      begin
        lbleditHotDirName.EditLabel.Caption := '';
        lbleditHotDirName.Text := rsMsgHotDirSimpleEndOfMenu;
        lbleditHotDirName.ReadOnly := True;

        lbleditHotDirPath.Visible := False;
        lbleditHotDirTarget.Visible := False;
      end;
    end; //case THotDir(WorkingPointer).Dispatcher of

    actDeleteSelectedItem.Enabled := not (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    actDeleteSubMenuKeepElem.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    actDeleteSubMenuAndElem.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    actAddCopyOfEntry.Enabled := ((THotDir(WorkingPointer).Dispatcher <> hd_STARTMENU) and (THotDir(WorkingPointer).Dispatcher <> hd_ENDMENU));
    actInsertCopyOfEntry.Enabled := actAddCopyOfEntry.Enabled;
    miSortSingleSubMenu.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    miSortSubMenuAndSubLevel.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    actDeleteSelectedItem.Enabled := (THotDir(WorkingPointer).Dispatcher <> hd_ENDMENU);
  end //if tvDirectoryHotlist.Selected<>nil then
  else
  begin
    lbleditHotDirName.EditLabel.Caption := '';
    lbleditHotDirName.Text := '';
    lbleditHotDirName.ReadOnly := True;
    lbleditHotDirName.Text := 'Nothing...';

    lbleditHotDirPath.Visible := False;
    lbleditHotDirTarget.Visible := False;
  end;

  btnRelativePath.Visible := lbleditHotDirPath.Visible;
  cbSortHotDirPath.Visible := lbleditHotDirPath.Visible and (THotDir(WorkingPointer).Dispatcher <> hd_COMMAND);
  btnRelativeTarget.Visible := lbleditHotDirTarget.Visible;
  cbSortHotDirTarget.Visible := lbleditHotDirTarget.Visible;

  if TForm(Self.Parent.Parent.Parent).ActiveControl.Name = 'tvTreeView' then
    if lbleditHotDirName.CanFocus then
      TForm(Self.Parent.Parent.Parent).ActiveControl := lbleditHotDirName;
end;

{ TfrmOptionsDirectoryHotlist.RefreshTreeView }
procedure TfrmOptionsDirectoryHotlist.RefreshTreeView(NodeToSelect: TTreeNode);
begin
  if NodeToSelect <> nil then
  begin
    tvDirectoryHotlist.ClearSelection(False);
    NodeToSelect.Selected := True;
  end
  else
  begin
    tvDirectoryHotlistSelectionChanged(tvDirectoryHotlist); //At least to hide path, target, etc.
  end;
  btnExport.Enabled := (tvDirectoryHotlist.Items.Count > 0);
  miSaveBackupHotlist.Enabled := (tvDirectoryHotlist.Items.Count > 0);
end;

{ TfrmOptionsDirectoryHotlist.PopulatePopupMenuWithCommands }
procedure TfrmOptionsDirectoryHotlist.PopulatePopupMenuWithCommands(pmMenuToPopulate: TPopupMenu);
var
  FFormCommands: IFormCommands;
  LocalDummyComboBox: TComboBox;
  miMainTree: TMenuItem;
  IndexCommand: longint;

  procedure LocalPopulateUntil(ParamMenuItem: TMenuItem; LetterUpTo: char);
  var
    LocalMenuItem: TMenuItem;
    MaybeItemName: string;
  begin
    MaybeItemName := '0000';
    while (IndexCommand < LocalDummyComboBox.Items.Count) and
      (MaybeItemName[4] <> LetterUpTo) do
    begin
      MaybeItemName := LocalDummyComboBox.Items.Strings[IndexCommand];
      if MaybeItemName[4] <> LetterUpTo then
      begin
        LocalMenuItem := TMenuItem.Create(ParamMenuItem);
        LocalMenuItem.Caption := MaybeItemName;
        LocalMenuItem.OnClick := @miSimplyCopyCaption;
        ParamMenuItem.Add(LocalMenuItem);
        Inc(IndexCommand);
      end;
    end;
  end;

begin
  LocalDummyComboBox := TComboBox.Create(Self);
  try
    LocalDummyComboBox.Clear;
    FFormCommands := frmMain as IFormCommands;
    FFormCommands.GetCommandsList(LocalDummyComboBox.Items);
    LocalDummyComboBox.Sorted := True;

    IndexCommand := 0;

    miMainTree := TMenuItem.Create(pmMenuToPopulate);
    miMainTree.Caption := 'cm_A..cm_C';
    pmMenuToPopulate.Items.Add(miMainTree);
    LocalPopulateUntil(miMainTree, 'D');

    miMainTree := TMenuItem.Create(pmMenuToPopulate);
    miMainTree.Caption := 'cm_D..cm_L';
    pmMenuToPopulate.Items.Add(miMainTree);
    LocalPopulateUntil(miMainTree, 'M');

    miMainTree := TMenuItem.Create(pmMenuToPopulate);
    miMainTree.Caption := 'cm_M..cm_R';
    pmMenuToPopulate.Items.Add(miMainTree);
    LocalPopulateUntil(miMainTree, 'S');

    miMainTree := TMenuItem.Create(pmMenuToPopulate);
    miMainTree.Caption := 'cm_S..cm_Z';
    pmMenuToPopulate.Items.Add(miMainTree);
    LocalPopulateUntil(miMainTree, 'A');

  finally
    LocalDummyComboBox.Free;
  end;
end;

{ TfrmOptionsDirectoryHotlist.miShowWhereItWouldGo }
procedure TfrmOptionsDirectoryHotlist.miShowWhereItWouldGo(Sender: TObject);
var
  StringToShow: string;
begin
  with Sender as TComponent do
  begin
    StringToShow := rsMsgHotDirDemoName + '"' +
      DirectoryHotlistTemp.HotDir[tag].HotDirName + '"';

    case DirectoryHotlistTemp.HotDir[tag].Dispatcher of
      hd_CHANGEPATH:
      begin
        StringToShow := StringToShow + #$0D + #$0A + #$0D + #$0A + rsMsgHotDirDemoPath;
        StringToShow := StringToShow + #$0D + #$0A +
          mbExpandFileName(DirectoryHotlistTemp.HotDir[tag].HotDirPath);

        if DirectoryHotlistTemp.HotDir[tag].HotDirTarget <> '' then
        begin
          StringToShow := StringToShow + #$0D + #$0A + #$0D + #$0A +
            rsMsgHotDirDemoTarget;
          StringToShow := StringToShow + #$0D + #$0A +
            mbExpandFileName(DirectoryHotlistTemp.HotDir[tag].HotDirTarget);
        end;
      end;

      hd_COMMAND:
      begin
        StringToShow := StringToShow + #$0D + #$0A + #$0D + #$0A +
          rsMsgHotDirDemoCommand;
        StringToShow := StringToShow + #$0D + #$0A +
          mbExpandFileName(DirectoryHotlistTemp.HotDir[tag].HotDirPath);
      end;
    end;

    msgOK(StringToShow);
  end;
end;

{ TfrmOptionsDirectoryHotlist.miSimplyCopyCaption }
procedure TfrmOptionsDirectoryHotlist.miSimplyCopyCaption(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    if lbleditHotDirPath.Text = '' then
      lbleditHotDirPath.Text := Caption
    else
      lbleditHotDirPath.Text := Caption + ' ' + lbleditHotDirPath.Text;
  end;
end;

{ TfrmOptionsDirectoryHotlist.ClearCutAndPasteList }
procedure TfrmOptionsDirectoryHotlist.ClearCutAndPasteList;
begin
  CutAndPasteIndexList.Clear;
  actPaste.Enabled := True;
end;

{ TfrmOptionsDirectoryHotlist.ActualAddDirectories }
function TfrmOptionsDirectoryHotlist.ActualAddDirectories(ParamDispatcher: TKindOfHotDirEntry; sName, sPath, sTarget: string; InsertOrAdd: integer): TTreeNode;
var
  LocalHotDir: THotDir;
  WorkingTreeNode: TTreeNode;
const
  SelectedNoAttachedMode: array[1..3] of TNodeAttachMode = (naInsert, naInsertBehind, naAddChildFirst);
begin
  ClearCutAndPasteList;
  LocalHotDir := THotDir.Create;
  LocalHotDir.Dispatcher := ParamDispatcher;
  LocalHotDir.HotDirName := sName;
  LocalHotDir.HotDirPath := IncludeTrailingPathDelimiter(sPath);
  if sTarget <> '' then LocalHotDir.HotDirTarget := IncludeTrailingPathDelimiter(sTarget);
  DirectoryHotlistTemp.Add(LocalHotDir);
  WorkingTreeNode := tvDirectoryHotlist.Selected;
  if WorkingTreeNode <> nil then
    Result := tvDirectoryHotlist.Items.AddNode(nil, WorkingTreeNode, sName, LocalHotDir, SelectedNoAttachedMode[InsertOrAdd])
  else
    Result := tvDirectoryHotlist.Items.AddNode(nil, nil, sName, LocalHotDir, naAddFirst);
  case ParamDispatcher of
    hd_STARTMENU:
    begin
      Result.ImageIndex := ICONINDEX_SUBMENU;
      Result.SelectedIndex := ICONINDEX_SUBMENU;
      Result.StateIndex := ICONINDEX_SUBMENU;
    end;

    hd_CHANGEPATH:
    begin
      Result.ImageIndex := ICONINDEX_NEWADDEDDIRECTORY;
      Result.SelectedIndex := ICONINDEX_NEWADDEDDIRECTORY;
      Result.StateIndex := ICONINDEX_NEWADDEDDIRECTORY;
    end;
  end;
end;

function CompareStringsFromTStringList(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := CompareStrings(List.Strings[Index1], List.Strings[Index2], gSortNatural, gSortCaseSensitivity);
end;

{ TfrmOptionsDirectoryHotlist.TryToGetCloserHotDir }
//This routine "tries" to find the best place to eventually add a new directory in the tree accoring to directory names and ones alreayd in the tree.
//ALSO, it will set the flag "bShouldBeAfter" to indicate if it should be "BEFORE" or "AFTER" what is returned.
//"PerfectMatchIndex" is when the directory is found *exactly* in the tree as is. In other words, already there.
//"SecondAlternative" is when the directory is not there, but one close to it is
function TfrmOptionsDirectoryHotlist.TryToGetCloserHotDir(sDirToFindAPlaceFor: string; var TypeOfAddition: integer): TTreeNode;
var
  BestOne, I: integer;
  localDirToFindAPlaceFor: string;
  sRepresentantString, sUnderPart, sOverPart: string;
  MagickSortedList: TStringList;

  function GetNumberOfIdenticalStartChars(A: string): longint;
  var
    I: integer;
  begin
    Result := 0;
    I := 1;
    while (I < UTF8Length(A)) and (I < UTF8Length(localDirToFindAPlaceFor)) do
    begin
      if A[I] = localDirToFindAPlaceFor[I] then Inc(Result) else I := UTF8Length(A);
      Inc(I);
    end;
  end;

  function GetBestDir(DirA, DirB: string): integer;
  var
    lengthA, lengthB: integer;
  begin
    lengthA := GetNumberOfIdenticalStartChars(DirA);
    lengthB := GetNumberOfIdenticalStartChars(DirB);
    if (lengthA = 0) and (lengthB = 0) then
    begin
      Result := 0;
    end
    else
    begin
      if lengthA > lengthB then Result := -1 else Result := 1;
    end;
  end;

begin
  Result := nil;
  TypeOfAddition := ACTION_ADDHOTDIR;

  localDirToFindAPlaceFor := UTF8LowerCase(IncludeTrailingPathDelimiter(sDirToFindAPlaceFor));

  //1st, let's try to see if we have an entry with the same *exact* directory
  I := 0;
  while (Result = nil) and (I < tvDirectoryHotlist.Items.Count) do
  begin
    if THotDir(tvDirectoryHotlist.Items.Item[I].Data).Dispatcher = hd_CHANGEPATH then
    begin
      if localDirToFindAPlaceFor = UTF8LowerCase(IncludeTrailingPathDelimiter(mbExpandFileName(THotDir(tvDirectoryHotlist.Items.Item[I].Data).HotDirPath))) then Result := tvDirectoryHotlist.Items.Item[I];
    end;
    Inc(I);
  end;

  //2nd, if nothing found, here is the "lazy-but-probably-easiest-to-write-method"
  if Result = nil then
  begin
    MagickSortedList := TStringList.Create;
    try
      MagickSortedList.Clear;

      for I := 0 to pred(tvDirectoryHotlist.Items.Count) do
      begin
        if THotDir(tvDirectoryHotlist.Items.Item[I].Data).Dispatcher = hd_CHANGEPATH then
        begin
          sRepresentantString := UTF8LowerCase(IncludeTrailingPathDelimiter(mbExpandFileName(THotDir(tvDirectoryHotlist.Items.Item[I].Data).HotDirPath))) + IntToStr(I);
          MagickSortedList.Add(sRepresentantString);
        end;
      end;
      MagickSortedList.Add(localDirToFindAPlaceFor);

      //We call a custom sort to make sure sort order will make the sequence "école - Eric - Érika"
      MagickSortedList.CustomSort(@CompareStringsFromTStringList);

      I := MagickSortedList.IndexOf(localDirToFindAPlaceFor);

      if I = 0 then sUnderPart := '' else sUnderPart := UTF8LowerCase(IncludeTrailingPathDelimiter(mbExpandFileName(THotDir(tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I - 1]))].Data).HotDirPath)));
      if I = pred(MagickSortedList.Count) then sOverPart := '' else sOverPart := UTF8LowerCase(IncludeTrailingPathDelimiter(mbExpandFileName(THotDir(tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I + 1]))].Data).HotDirPath)));
      BestOne := GetBestDir(sUnderPart, sOverPart);

      case BestOne of
        -1: Result := tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I - 1]))];
        1: Result := tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I + 1]))];
      end;

      if Result <> nil then
      begin
        if CompareStrings(localDirToFindAPlaceFor, UTF8LowerCase(IncludeTrailingPathDelimiter(mbExpandFileName(tHotDir(Result.Data).HotDirPath))), gSortNatural, gSortCaseSensitivity) = -1 then TypeOfAddition := ACTION_INSERTHOTDIR;
      end;
    finally
      MagickSortedList.Free;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.TryToGetExactHotDir }
function TfrmOptionsDirectoryHotlist.TryToGetExactHotDir(const index: integer): TTreeNode;
var
  SearchingtvIndex: integer;
begin
  Result := nil;

  SearchingtvIndex := 0;
  while (SearchingtvIndex < pred(tvDirectoryHotlist.Items.Count)) and (Result = nil) do
  begin
    if tvDirectoryHotlist.Items[SearchingtvIndex].Data = DirectoryHotlistTemp.Items[Index] then Result := tvDirectoryHotlist.Items[SearchingtvIndex] else Inc(SearchingtvIndex);
  end;
end;

{ TfrmOptionsDirectoryHotlist.RecursiveSetGroupNumbers }
procedure TfrmOptionsDirectoryHotlist.RecursiveSetGroupNumbers(ParamNode: TTreeNode; ParamGroupNumber: integer; DoRecursion, StopAtFirstGroup: boolean);
var
  MaybeChild: TTreeNode;
begin
  repeat
    if DoRecursion then
    begin
      MaybeChild := ParamNode.GetFirstChild;
      if MaybeChild <> nil then RecursiveSetGroupNumbers(MaybeChild, GetNextGroupNumber, DoRecursion, StopAtFirstGroup);
    end;

    if THotDir(ParamNode.Data).Dispatcher <> hd_SEPARATOR then
    begin
      THotDir(ParamNode.Data).GroupNumber := ParamGroupNumber;
    end
    else
    begin
      ParamGroupNumber := GetNextGroupNumber;
      if StopAtFirstGroup then while ParamNode <> nil do ParamNode := ParamNode.GetNextSibling; //To exit the loop!
    end;

    if ParamNode <> nil then ParamNode := ParamNode.GetNextSibling;
  until ParamNode = nil;
end;

{ TfrmOptionsDirectoryHotlist.RefreshExistingProperty }
procedure TfrmOptionsDirectoryHotlist.RefreshExistingProperty(ScanMode: integer);
var
  Index, LocalThreadCount: longint;
  ListOfAlreadyCheckDrive, ListOfNonExistingDrive: TStringList;
  RememberCursor: TCursor;
  FreezeTime: dword;

  procedure StartThreadToSeeIfThisDriveExists(const sDrive: string);
  begin
    TCheckDrivePresenceThread.Create(sDrive, ListOfNonExistingDrive, LocalThreadCount);
  end;

  //Since we do that for both "Path" and "Target", it was useful to place in a routine so we can call two times the same routine
  procedure ScanForThisDir(DirToScan: string);
  var
    localPath, localDrive: string;
  begin
    localPath := ExcludeTrailingPathDelimiter(mbExpandFileName(DirToScan));
    localDrive := UpperCase(ExtractFileDrive(localPath));

    if ListOfAlreadyCheckDrive.IndexOf(localDrive) = -1 then
    begin
      Inc(LocalThreadCount);
      StartThreadToSeeIfThisDriveExists(localDrive);
      ListOfAlreadyCheckDrive.Add(localDrive);
    end;
  end;

  procedure RecursivelySetIconFolderNotPresent(WorkingTreeNode: TTreeNode);
  begin
    if WorkingTreeNode.Parent <> nil then
    begin
      if WorkingTreeNode.Parent.ImageIndex <> ICONINDEX_SUBMENUWITHMISSING then
      begin
        THotDir(WorkingTreeNode.Parent.Data).HotDirExisting := DirNotExist;
        WorkingTreeNode.Parent.ImageIndex := ICONINDEX_SUBMENUWITHMISSING;
        WorkingTreeNode.Parent.SelectedIndex := ICONINDEX_SUBMENUWITHMISSING;
        WorkingTreeNode.Parent.StateIndex := ICONINDEX_SUBMENUWITHMISSING;
        RecursivelySetIconFolderNotPresent(WorkingTreeNode.Parent);
      end;
    end;
  end;

  //Since we do that for both "Path" and "Target", it was useful to place in a routine so we can call two times the same routine
  function CheckIfThisDirectoryExists(RequestedDirectoryToCheck: string): boolean;
  var
    localPath, localDrive: string;
  begin
    if RequestedDirectoryToCheck <> '' then
    begin
      Result := False;
      localPath := ExcludeTrailingPathDelimiter(mbExpandFileName(RequestedDirectoryToCheck));
      localDrive := UpperCase(ExtractFileDrive(localPath));
      lbleditHotDirName.Text := localPath;
      Application.ProcessMessages;

      if ListOfNonExistingDrive.IndexOf(localDrive) = -1 then
      begin
        Result := mbDirectoryExists(localPath);
      end;

      if not Result then
      begin
        THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting := DirNotExist;
        tvDirectoryHotlist.Items.Item[Index].ImageIndex := ICONINDEX_DIRECTORYNOTPRESENTHERE;
        tvDirectoryHotlist.Items.Item[Index].SelectedIndex := ICONINDEX_DIRECTORYNOTPRESENTHERE;
        tvDirectoryHotlist.Items.Item[Index].StateIndex := ICONINDEX_DIRECTORYNOTPRESENTHERE;
        RecursivelySetIconFolderNotPresent(tvDirectoryHotlist.Items.Item[Index]);
      end;
    end
    else
    begin
      Result := True;
    end;
  end;

begin
  RememberCursor := Screen.Cursor;
  SetNormalIconsInTreeView;

  try
    Screen.Cursor := crHourGlass;

    ListOfAlreadyCheckDrive := TStringList.Create;
    ListOfAlreadyCheckDrive.Sorted := False;
    ListOfAlreadyCheckDrive.Clear;

    ListOfNonExistingDrive := TStringList.Create;
    ListOfNonExistingDrive.Sorted := False;
    ListOfNonExistingDrive.Clear;

    try
      LocalThreadCount := 0;

      //First, let's build a list of the "\\ServerName" that exists and let's check them in MultiThread
      //We scan only once each drive and "\\ServerName"
      //"\\ServerName" have a long timeout so that's why we check them this way
      for Index := 0 to pred(tvDirectoryHotlist.Items.Count) do
      begin
        case THotDir(tvDirectoryHotlist.Items.Item[Index].Data).Dispatcher of
          hd_CHANGEPATH:
          begin
            ScanForThisDir(THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirPath);
            if ScanMode = 2 then ScanForThisDir(THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirTarget);
          end;
        end;
      end;

      //Let's wait all the threads to complete
      //10 seconds timeout in case it never ends for whatever reason
      FreezeTime := GetTickCount;
      while (LocalThreadCount <> 0) and ((FreezeTime + 10000) > FreezeTime) do
      begin
        lbleditHotDirName.Text := IntToStr(LocalThreadCount);
        Application.ProcessMessages;
        if LocalThreadCount = 0 then Sleep(100);
      end;

      //Second, now let's scan if the director exists!
      for Index := 0 to pred(tvDirectoryHotlist.Items.Count) do
      begin
        case THotDir(tvDirectoryHotlist.Items.Item[Index].Data).Dispatcher of
          hd_CHANGEPATH:
          begin
            if CheckIfThisDirectoryExists(THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirPath) then
            begin
              case ScanMode of
                1:
                begin
                  THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting := DirExist;
                end;

                2:
                begin
                  if CheckIfThisDirectoryExists(THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirTarget) then
                  begin
                    THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting := DirExist;
                  end;
                end;
              end; //case ScanMode
            end;
          end; //hd_CHANGEPATH:
        end; //case THotDir(tvDirectoryHotlist.Items.Item[Index].Data).Dispatcher of
      end;
    finally
      ListOfAlreadyCheckDrive.Free;
      ListOfNonExistingDrive.Free;
      lbleditHotDirName.Enabled := True;
    end;

  finally
    Screen.Cursor := RememberCursor;
  end;

  tvDirectoryHotlist.Refresh;
end;

{ TfrmOptionsDirectoryHotlist.SetNormalIconsInTreeView }
procedure TfrmOptionsDirectoryHotlist.SetNormalIconsInTreeView;
var
  Index: integer;
begin
  for Index := 0 to pred(tvDirectoryHotlist.Items.Count) do
  begin
    if tvDirectoryHotlist.Items.Item[Index].GetFirstChild = nil then
    begin
      tvDirectoryHotlist.Items.Item[Index].ImageIndex := -1;
      tvDirectoryHotlist.Items.Item[Index].SelectedIndex := -1;
      tvDirectoryHotlist.Items.Item[Index].StateIndex := -1;
    end
    else
    begin
      tvDirectoryHotlist.Items.Item[Index].ImageIndex := ICONINDEX_SUBMENU;
      tvDirectoryHotlist.Items.Item[Index].SelectedIndex := ICONINDEX_SUBMENU;
      tvDirectoryHotlist.Items.Item[Index].StateIndex := ICONINDEX_SUBMENU;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.MySortViaGroup }
function TfrmOptionsDirectoryHotlist.MySortViaGroup(Node1, Node2: TTreeNode): integer;
begin
  if (THotdir(Node1.Data).GroupNumber = THotDir(Node2.Data).GroupNumber) and (THotdir(Node1.Data).GroupNumber <> 0) then
  begin
    Result := CompareStrings(THotdir(Node1.Data).HotDirName, THotDir(Node2.Data).HotDirName, gSortNatural, gSortCaseSensitivity);
  end
  else
  begin
    if Node1.AbsoluteIndex < Node2.AbsoluteIndex then Result := -1 else Result := 1;
  end;
end;

{ TfrmOptionsDirectoryHotlist.CopyTTreeViewToAnother }
procedure TfrmOptionsDirectoryHotlist.CopyTTreeViewToAnother(tvSource, tvDestination: TTreeView);
  procedure RecursiveNodeCopy(SourceNode, DestNode: TTreeNode);
  var
    NewNode: TTreeNode;
  begin
    repeat
      NewNode := tvDestination.Items.AddChild(DestNode, SourceNode.Text);
      NewNode.Assign(SourceNode);
      if SourceNode.GetFirstChild <> nil then
      begin
        RecursiveNodeCopy(SourceNode.GetFirstChild, NewNode);
      end;
      SourceNode := SourceNode.GetNextSibling;
    until SourceNode = nil;
  end;
begin
  if tvSource.Items.GetFirstNode <> nil then RecursiveNodeCopy(tvSource.Items.GetFirstNode, nil);
end;

{ TfrmOptionsDirectoryHotlist.GetNextGroupNumber }
function TfrmOptionsDirectoryHotlist.GetNextGroupNumber: integer;
begin
  GlobalGroupNumber := GlobalGroupNumber + 1;
  Result := GlobalGroupNumber;
end;

end.
