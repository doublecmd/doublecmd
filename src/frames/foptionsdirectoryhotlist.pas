{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of HotDir

   Copyright (C) 2009-2014  Alexander Koblov (alexx2000@mail.ru)

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

unit foptionsDirectoryHotlist;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, EditBtn, ExtCtrls,
  Menus, Dialogs, ComCtrls, uHotDir, types, fOptionsFrame, uFile;

type
  TProcedureWhenClickingAMenuItem = procedure(Sender: TObject) of object;
  
  { TfrmOptionsDirectoryHotlist }
  TfrmOptionsDirectoryHotlist = class(TOptionsEditor)
    btnRelativePath: TSpeedButton;
    btnRelativeTarget: TSpeedButton;
    btnHelp: TButton;
    cbAddTarget: TCheckBox;
    cbFullExpandTree: TCheckBox;
    cbShowPathInPopup: TCheckBox;
    cbShowOnlyValidEnv: TCheckBox;
    cbSortHotDirPath: TComboBox;
    cbSortHotDirTarget: TComboBox;
    gbDirectoryHotlist: TGroupBox;
    gbHotlistOtherOptions: TGroupBox;
    lbleditHotDirName: TLabeledEdit;
    lbleditHotDirPath: TLabeledEdit;
    lbleditHotDirTarget: TLabeledEdit;
    miTypeTheDirectory3: TMenuItem;
    miCollapseAll: TMenuItem;
    miSeparator10: TMenuItem;
    miOpenAllBranches: TMenuItem;
    miSeparator11: TMenuItem;
    miSeparator9: TMenuItem;
    miSortSingleGroup2: TMenuItem;
    miCurrentSelectedOrActiveDirectories: TMenuItem;
    pmPathHelper: TPopupMenu;
    rgWhereToAdd: TRadioGroup;
    tvDirectoryHotlist: TTreeView;
    btnInsert: TBitBtn;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    btnBackup: TBitBtn;
    btnExport: TBitBtn;
    btnImport: TBitBtn;
    btnMiscellaneous: TBitBtn;
    btnSort: TBitBtn;
    pmTreeView: TPopupMenu;
    pmInsertAddDirectoryHotlist: TPopupMenu;
    miBrowseToDirectory: TMenuItem;
    miTypeTheDirectory: TMenuItem;
    miActiveFrameDirectory: TMenuItem;
    miActiveInactiveFrameDirectory: TMenuItem;
    miAddCopyOfSelected: TMenuItem;
    miSeparator1: TMenuItem;
    miAddCommand: TMenuItem;
    miAddSeparator: TMenuItem;
    miAddSubmenu: TMenuItem;
    pmDeleteDirectoryHotlist: TPopupMenu;
    miDeleteSelectedEntry: TMenuItem;
    miSeparator2: TMenuItem;
    miDeleteJustSubMenu: TMenuItem;
    miDeleteCompleteSubMenu: TMenuItem;
    miSeparator3: TMenuItem;
    miDeleteAllHotDirs: TMenuItem;
    pmExportDirectoryHotlist: TPopupMenu;
    miExportToTotalCommanderk: TMenuItem;
    miExportToTotalCommandernk: TMenuItem;
    miSeparator5: TMenuItem;
    miExportToHotlistFile: TMenuItem;
    pmImportDirectoryHotlist: TPopupMenu;
    miImportTotalCommander: TMenuItem;
    miSeparator4: TMenuItem;
    miImportFromHotlistFile: TMenuItem;
    pmBackupDirectoryHotlist: TPopupMenu;
    miSaveBackupHotlist: TMenuItem;
    miRestoreBackupHotlist: TMenuItem;
    pmMiscellaneousDirectoryHotlist: TPopupMenu;
    miTestResultingHotlistMenu: TMenuItem;
    miDetectIfPathExist: TMenuItem;
    miDetectIfPathTargetExist: TMenuItem;
    pmSortDirectoryHotlist: TPopupMenu;
    miSortSingleGroup: TMenuItem;
    miCurrentLevelOfItemOnly: TMenuItem;
    miSortSingleSubMenu: TMenuItem;
    miSortSubMenuAndSubLevel: TMenuItem;
    miSortEverything: TMenuItem;
    pmHotDirTestMenu: TPopupMenu;
    miHotDirTestMenu: TMenuItem;
    miTypeTheDirectory2: TMenuItem;
    miAddCopyOfSelected2: TMenuItem;
    miSeparator6: TMenuItem;
    miAddCommand2: TMenuItem;
    miAddSeparator2: TMenuItem;
    miAddSubmenu2: TMenuItem;
    miSeparator7: TMenuItem;
    miDeleteSelectedEntry2: TMenuItem;
    miSeparator8: TMenuItem;
    miCutSelection: TMenuItem;
    miPasteSelection: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btnHelpClick(Sender: TObject);
    procedure cbFullExpandTreeChange(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miOpenAllBranchesClick(Sender: TObject);
    procedure tvDirectoryHotlistDragDrop(Sender, {%H-}Source: TObject; X, Y: Integer);
    procedure tvDirectoryHotlistDragOver(Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: Integer; {%H-}State: TDragState; var Accept: Boolean);
    procedure tvDirectoryHotlistEnter(Sender: TObject);
    procedure tvDirectoryHotlistExit(Sender: TObject);
    procedure tvDirectoryHotlistSelectionChanged(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure miInsertAddHotDirClick(Sender: TObject);
    procedure miDeleteSelectedEntryClick(Sender: TObject);
    procedure miDeleteAllHotDirsClick(Sender: TObject);
    procedure miExportToAnythingClick(Sender: TObject);
    procedure miImportFromAnythingClick(Sender: TObject);
    procedure miSortDirectoryHotlistClick(Sender: TObject);
    procedure miTestResultingHotlistMenuClick(Sender: TObject);
    procedure miDetectIfPathExistClick(Sender: TObject);
    procedure ClearCutAndPasteList;
    procedure miCutSelectionClick(Sender: TObject);
    procedure miPasteSelectionClick(Sender: TObject);
    procedure lbleditHotDirEnter(Sender: TObject);
    procedure lbleditHotDirExit(Sender: TObject);
    procedure lbleditHotDirKeyPress(Sender: TObject; var Key: char);
    procedure lbleditHotDirMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure anyRelativeAbsolutePathClick(Sender: TObject);
    procedure cbSortHotDirPathChange(Sender: TObject);
    procedure cbSortHotDirTargetChange(Sender: TObject);
    procedure miShowWhereItWouldGo(Sender: TObject);
    procedure miSimplyCopyCaption(Sender: TObject);
    procedure PopulatePopupMenuWithCommands(pmMenuToPopulate: TPopupMenu);
    function ActualAddDirectories(ParamDispatcher: TKindOfHotDirEntry; sName, sPath, sTarget: string; InsertOrAdd:integer): TTreeNode;
    procedure RefreshTreeView(NodeToSelect:TTreeNode);
    function TryToGetCloserHotDir(sDirToFindAPlaceFor: string; var TypeOfAddition:Integer): TTreeNode;
    function TryToGetExactHotDir(const index:integer):TTreeNode;
    procedure RecursiveSetGroupNumbers(ParamNode:TTreeNode; ParamGroupNumber:integer; DoRecursion, StopAtFirstGroup:boolean);
    procedure RefreshExistingProperty(ScanMode: integer);
    procedure SetNormalIconsInTreeView;
    function MySortViaGroup(Node1, Node2: TTreeNode): integer;
    procedure CopyTTreeViewToAnother(tvSource,tvDestination:TTreeView);
    function GetNextGroupNumber:integer;
  protected
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
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    destructor Destroy; override;
    function CanWeClose: boolean; override;
    procedure SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher,paramPath,paramTarget,paramOptionalIndex: string);
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LCLProc, LCLIntf, LCLMessageGlue, helpintfs,

  //DC
  DCStrUtils, uGlobs, uLng, uDCUtils, uDebug, fmain, uFormCommands, uFileProcs,
  uShowMsg, DCOSUtils, uSpecialDir, fhotdirexportimport, fOptions;

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

  CutAndPasteIndexList:=TStringList.Create;
  CutAndPasteIndexList.Sorted:=TRUE;
  CutAndPasteIndexList.Duplicates:=dupAccept;

  {$IFNDEF MSWINDOWS}
  miExportToTotalCommanderk.Free;
  miExportToTotalCommandernk.Free;
  miSeparator4.Free;
  miSeparator5.Free;
  miImportTotalCommander.Free;
  cbShowOnlyValidEnv.Caption:='Show only valid $env__var$';
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  cbShowOnlyValidEnv.Caption:='Show only valid %env_var%';
  {$ENDIF}



  if DirectoryHotlistTemp=nil then
  begin
    DirectoryHotlistTemp := TDirectoryHotlist.Create;
    gDirectoryHotlist.CopyDirectoryHotlistToDirectoryHotlist(DirectoryHotlistTemp);
  end;

  tvDirectoryHotlist.Images:=frmMain.imgLstDirectoryHotlist;
  DirectoryHotlistTemp.LoadTTreeView(tvDirectoryHotlist,-1);
  cbFullExpandTreeChange(cbFullExpandTree);
  if tvDirectoryHotlist.Items.Count>0 then tvDirectoryHotlist.Items[0].Selected:=TRUE; //Select at least first one by default
  DirectoryHotlistTemp.FlagModified:=FALSE;
end;

{ TfrmOptionsDirectoryHotlist.Save }
function TfrmOptionsDirectoryHotlist.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  DirectoryHotlistTemp.RefreshFromTTreeView(tvDirectoryHotlist);
  DirectoryHotlistTemp.CopyDirectoryHotlistToDirectoryHotlist(gDirectoryHotlist);
  gHotDirAddTargetOrNot:=cbAddTarget.Checked;
  gHotDirFullExpandOrNot:=cbFullExpandTree.Checked;
  if gShowPathInPopup<>cbShowPathInPopup.Checked then
  begin
    gShowPathInPopup:=cbShowPathInPopup.Checked;
    pmPathHelper.Items.Clear; //Let' re-populate it since option for environment variabel path has changed...
    gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  end;

  if gShowOnlyValidEnv<>cbShowOnlyValidEnv.Checked then
  begin
    gShowOnlyValidEnv:=cbShowOnlyValidEnv.Checked;
    LoadWindowsSpecialDir;
    pmPathHelper.Items.Clear; //Let' re-populate it since option for environment variabel path has changed...
    gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  end;
  gWhereToAddNewHotDir:=TPositionWhereToAddHotDir(rgWhereToAdd.ItemIndex);
  DirectoryHotlistTemp.FlagModified:=FALSE;
  cbFullExpandTreeChange(cbFullExpandTree);
end;

{ TfrmOptionsDirectoryHotlist.CanWeClose }
function TfrmOptionsDirectoryHotlist.CanWeClose: boolean;
var
  Answer:TMyMsgResult;
begin
  result:=TRUE;

  if Assigned(DirectoryHotlistTemp) then
  begin
    result:=not DirectoryHotlistTemp.FlagModified;
    if not result then
    begin
      ShowOptions(TfrmOptionsDirectoryHotlist);
      Answer:=MsgBox(rsMsgHotDirModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
      case Answer of
        mmrYes:
          begin
            Save;
            result:=TRUE;
          end;

        mmrNo: result:=TRUE;
        else result:=FALSE;
      end;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.GetIconIndex }
class function TfrmOptionsDirectoryHotlist.GetIconIndex: Integer;
begin
  Result := 33;
end;

{ TfrmOptionsDirectoryHotlist.GetTitle }
class function TfrmOptionsDirectoryHotlist.GetTitle: String;
begin
  Result := rsOptionsEditorDirectoryHotlist;
end;

{ TfrmOptionsDirectoryHotlist.Destroy }
destructor TfrmOptionsDirectoryHotlist.Destroy;
begin
  pmCommandHelper.Free;
  CutAndPasteIndexList.Free;
  inherited Destroy;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragDrop }
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Index:longint;
  DestinationNode:TTreeNode;
begin
  DestinationNode:=tvDirectoryHotlist.GetNodeAt(X, Y);

  if tvDirectoryHotlist.SelectionCount>0 then
  begin
    //If we move toward the end, we place the moved item *after* the destination.
    //If we move toward the beginning, we place the moved item *before* the destination.
    if tvDirectoryHotlist.Selections[pred(tvDirectoryHotlist.SelectionCount)].AbsoluteIndex>DestinationNode.AbsoluteIndex then
    begin
      for Index:=0 to pred(tvDirectoryHotlist.SelectionCount) do
      begin
        tvDirectoryHotlist.Selections[Index].MoveTo(DestinationNode,naInsert);
      end;
    end
    else
    begin
      for Index:=0 to pred(tvDirectoryHotlist.SelectionCount) do
      begin
        tvDirectoryHotlist.Selections[Index].MoveTo(DestinationNode,naInsertBehind);
      end;
    end;
    DirectoryHotlistTemp.FlagModified:=TRUE;
    ClearCutAndPasteList;
  end;

  miPasteSelection.Enabled:=FALSE;
end;

{ TfrmOptionsDirectoryHotlist.miOpenAllBranchesClick }
procedure TfrmOptionsDirectoryHotlist.miOpenAllBranchesClick(Sender: TObject);
begin
  tvDirectoryHotlist.FullExpand;
  if tvDirectoryHotlist.Selected<>nil then
    begin
      tvDirectoryHotlist.Selected.MakeVisible;
      if tvDirectoryHotlist.CanFocus then tvDirectoryHotlist.SetFocus;
    end;
end;

{ TfrmOptionsDirectoryHotlist.miCollapseAllClick }
procedure TfrmOptionsDirectoryHotlist.miCollapseAllClick(Sender: TObject);
begin
  tvDirectoryHotlist.FullCollapse;
  if tvDirectoryHotlist.Selected<>nil then
    begin
      tvDirectoryHotlist.Selected.MakeVisible;
      tvDirectoryHotlist.Selected.MakeVisible;
      tvDirectoryHotlist.Selected.MakeVisible;
      tvDirectoryHotlist.Selected.MakeVisible;
      if tvDirectoryHotlist.CanFocus then tvDirectoryHotlist.SetFocus;
    end;
end;

{ TfrmOptionsDirectoryHotlist.cbFullExpandTreeChange }
procedure TfrmOptionsDirectoryHotlist.cbFullExpandTreeChange(Sender: TObject);
begin
  if cbFullExpandTree.Checked then tvDirectoryHotlist.FullExpand else tvDirectoryHotlist.FullCollapse;
end;

{ TfrmOptionsDirectoryHotlist.btnHelpClick }
procedure TfrmOptionsDirectoryHotlist.btnHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', '/directoryhotlist.html');
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragOver }
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=TRUE;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistEnter }
// To help to catch eye's attention, let's change color of selection when tree get/lose the focus
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistEnter(Sender: TObject);
begin
  tvDirectoryHotlist.SelectionColor:=clHighlight;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistExit }
// To help to catch eye's attention, let's change color of selection when tree get/lose the focus
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistExit(Sender: TObject);
begin
  tvDirectoryHotlist.SelectionColor:=clBtnShadow;
end;

{ TfrmOptionsDirectoryHotlist.tvDirectoryHotlistSelectionChanged }
procedure TfrmOptionsDirectoryHotlist.tvDirectoryHotlistSelectionChanged(Sender: TObject);
var
  WorkingPointer:Pointer;
begin
  if tvDirectoryHotlist.Selected<>nil then
  begin
    WorkingPointer:=tvDirectoryHotlist.Selected.Data;

    case THotDir(WorkingPointer).Dispatcher of
      hd_NULL:
      begin
      end;

      hd_CHANGEPATH:
      begin
        lbleditHotDirName.EditLabel.Caption := rsMsgHotDirSimpleName;
        lbleditHotDirName.Text := THotDir(WorkingPointer).HotDirName;
        lbleditHotDirName.Enabled := True;

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
        lbleditHotDirName.Enabled := True;

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
        lbleditHotDirName.Enabled := False;

        lbleditHotDirPath.Visible := False;
        lbleditHotDirTarget.Visible := False;
      end;

      hd_STARTMENU:
      begin
        lbleditHotDirName.EditLabel.Caption := rsMsgHotDirSimpleMenu;
        lbleditHotDirName.Text := THotDir(WorkingPointer).HotDirName;
        lbleditHotDirName.Enabled := True;

        lbleditHotDirPath.Visible := False;
        lbleditHotDirTarget.Visible := False;
      end;

      hd_ENDMENU:
      begin
        lbleditHotDirName.EditLabel.Caption := '';
        lbleditHotDirName.Text := rsMsgHotDirSimpleEndOfMenu;
        lbleditHotDirName.Enabled := False;

        lbleditHotDirPath.Visible := False;
        lbleditHotDirTarget.Visible := False;
      end;
    end; //case THotDir(WorkingPointer).Dispatcher of

    miDeleteSelectedEntry.Enabled := not (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    miDeleteJustSubMenu.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    miDeleteCompleteSubMenu.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    miAddCopyOfSelected.Enabled := ((THotDir(WorkingPointer).Dispatcher <> hd_STARTMENU) and (THotDir(WorkingPointer).Dispatcher <> hd_ENDMENU));
    miAddCopyOfSelected2.Enabled := miAddCopyOfSelected.Enabled;
    miSortSingleSubMenu.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    miSortSubMenuAndSubLevel.Enabled := (THotDir(WorkingPointer).Dispatcher = hd_STARTMENU);
    miDeleteSelectedEntry.Enabled := (THotDir(WorkingPointer).Dispatcher <> hd_ENDMENU);
    miDeleteSelectedEntry2.Enabled := miDeleteSelectedEntry.Enabled;
  end //if tvDirectoryHotlist.Selected<>nil then
  else
  begin
    lbleditHotDirName.EditLabel.Caption := '';
    lbleditHotDirName.Text := '';
    lbleditHotDirName.Enabled := False;
    lbleditHotDirName.Text:='Nothing...';

    lbleditHotDirPath.Visible := False;
    lbleditHotDirTarget.Visible := False;
  end;

  btnRelativePath.Visible := lbleditHotDirPath.Visible;
  cbSortHotDirPath.Visible := lbleditHotDirPath.Visible and (THotDir(WorkingPointer).Dispatcher <> hd_COMMAND);
  btnRelativeTarget.Visible := lbleditHotDirTarget.Visible;
  cbSortHotDirTarget.Visible := lbleditHotDirTarget.Visible;
end;

{ TfrmOptionsDirectoryHotlist.btnActionClick }
procedure TfrmOptionsDirectoryHotlist.btnActionClick(Sender: TObject);
var
  Dispatcher:integer;
begin
  with Sender as TComponent do Dispatcher := tag;

  case Dispatcher of
    1,2: pmInsertAddDirectoryHotlist.Tag:=Dispatcher; //To help in routine to determine if it's a "Insert" or a "Add"
  end;

  case Dispatcher of
    1: miTypeTheDirectory.ShortCut:=$4000 OR VK_F9;
    2: miTypeTheDirectory.ShortCut:=VK_F9;
  end;

  case Dispatcher of
    1,2: pmInsertAddDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    3: pmDeleteDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    4: pmExportDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    5: pmImportDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    6: pmBackupDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    7: pmMiscellaneousDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    8: pmSortDirectoryHotlist.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

{ TfrmOptionsDirectoryHotlist.miAddHotDirClick }
procedure TfrmOptionsDirectoryHotlist.miInsertAddHotDirClick(Sender: TObject);
var
  sPath, initialPath, stempo: string;
  AddOrInsertDispatcher, Dispatcher, Index: integer;
  MaybeNodeAfterAddition:TTreeNode=nil;
  NodeAfterAddition:TTreeNode=nil;
  SelectedOrActiveDirectories:TFiles;
begin
  with Sender as TComponent do Dispatcher := tag;
  with Sender as TMenuItem do AddOrInsertDispatcher:=GetParentMenu.Tag;

  sPath := '';

  case Dispatcher of
    1: //Directory I will browse to
    begin
      initialPath := '';
      if (tvDirectoryHotlist.Items.Count>0) then
      begin
        if THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher = hd_CHANGEPATH then initialPath := mbExpandFileName(THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath);
      end;
      if initialPath = '' then initialPath := frmMain.ActiveFrame.CurrentPath;

      if SelectDirectory(rsSelectDir, initialPath, sPath, False) then
      begin
        NodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH, GetLastDir(sPath), sPath, '',AddOrInsertDispatcher);
      end;
    end;

    2: //Directory I will type
    begin
      if cbAddTarget.Checked then
        sTempo := rsMsgHotDirTarget
      else
        sTempo := '';
      NodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH, rsMsgHotDirName, rsMsgHotDirPath, sTempo,AddOrInsertDispatcher);
    end;

    3: //Directory of the active frame
    begin
      NodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH, GetLastDir(frmMain.ActiveFrame.CurrentPath), frmMain.ActiveFrame.CurrentPath, '',AddOrInsertDispatcher);
    end;

    4: //Directory of the active AND inactive frames
    begin
      NodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH, GetLastDir(frmMain.ActiveFrame.CurrentPath), frmMain.ActiveFrame.CurrentPath, frmMain.NotActiveFrame.CurrentPath,AddOrInsertDispatcher);
    end;

    5:
    begin
      NodeAfterAddition:=ActualAddDirectories(hd_SEPARATOR, HOTLIST_SEPARATORSTRING, '', '',AddOrInsertDispatcher);
    end;

    6:
    begin
      NodeAfterAddition:=ActualAddDirectories(hd_STARTMENU, rsMsgHotDirSubMenuName, '', '',AddOrInsertDispatcher);
      tvDirectoryHotlist.ClearSelection(TRUE);
      NodeAfterAddition.Selected:=TRUE;
      NodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH, rsMsgHotDirName, rsMsgHotDirPath, sTempo,3);
    end;

    7:
    begin
      NodeAfterAddition:=ActualAddDirectories(THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher, THotDir(tvDirectoryHotlist.Selected.Data).HotDirName, THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath, THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget,AddOrInsertDispatcher);
    end;

    8: //A command
    begin
      NodeAfterAddition:=ActualAddDirectories(hd_COMMAND, rsMsgHotDirCommandName, rsMsgHotDirCommandSample, '',AddOrInsertDispatcher);
    end;

    9: //Current selected directories of active frame
      begin
        SelectedOrActiveDirectories:=frmMain.ActiveFrame.CloneSelectedOrActiveDirectories;
        try
          if SelectedOrActiveDirectories.count>0 then
          begin
            if AddOrInsertDispatcher = 1 then
            begin  //When we INSERT, which mean BEFORE the selection, let's do it this way so last insert will be just ab the previous selection AND ready to edit
              for Index:=0 to pred(SelectedOrActiveDirectories.Count) do
              begin
                MaybeNodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH,
                                        GetLastDir(ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[Index].FullPath)),
                                        ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[Index].FullPath),
                                        '',
                                        AddOrInsertDispatcher);
                if NodeAfterAddition=nil then NodeAfterAddition:=MaybeNodeAfterAddition;
              end;
            end
            else
            begin //When we ADD, which mean AFTER the selection, let's do it this way so last addition will be just below the previous selection AND will be the first one that selected in active frame
              for Index:=pred(SelectedOrActiveDirectories.Count) downto 0 do
              begin
                NodeAfterAddition:=ActualAddDirectories(hd_CHANGEPATH,
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

  if NodeAfterAddition<>nil then
    begin
      tvDirectoryHotlist.ClearSelection(TRUE);
      NodeAfterAddition.Selected:=TRUE;
      if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
    end;
end;

{ TfrmOptionsDirectoryHotlist.miDeleteSelectedEntryClick }
procedure TfrmOptionsDirectoryHotlist.miDeleteSelectedEntryClick(Sender: TObject);
var
  DeleteDispatcher:integer;
  FlagQuitDeleting:boolean;
  Answer:TMyMsgResult;
  NodeAfterDeletion:TTreeNode=nil;
  isTreeHadFocus:boolean=FALSE;

  procedure DeleteSelectionAndSetNodeAfterDeletion;
  begin
    if tvDirectoryHotList.Selections[0].GetNextSibling<>nil then NodeAfterDeletion:=tvDirectoryHotList.Selections[0].GetNextSibling  else
      if tvDirectoryHotList.Selections[0].GetPrevSibling<>nil then NodeAfterDeletion:=tvDirectoryHotList.Selections[0].GetPrevSibling else
        if tvDirectoryHotList.Selections[0].Parent<>nil then NodeAfterDeletion:=tvDirectoryHotList.Selections[0].Parent else
          NodeAfterDeletion:=nil;
    tvDirectoryHotList.Selections[0].Delete;
    ClearCutAndPasteList;
  end;

begin
  if tvDirectoryHotlist.SelectionCount>0 then
  begin
    isTreeHadFocus:=tvDirectoryHotlist.Focused;
    tvDirectoryHotlist.Enabled:=FALSE;
    try
      with Sender as TComponent do DeleteDispatcher:=tag;
      FlagQuitDeleting:=FALSE;

      //It's funny but as long we have something selected, we delete it and it will be index 0 since when
      //deleting something, the "Selections" array is updated!
      while (tvDirectoryHotList.SelectionCount>0) AND (not FlagQuitDeleting) do
      begin
        if tvDirectoryHotList.Selections[0].GetFirstChild=nil then
        begin
          DeleteSelectionAndSetNodeAfterDeletion;
        end
        else
        begin
          case DeleteDispatcher of
            1: Answer := MsgBox(Format(rsMsgHotDirWhatToDelete,[tvDirectoryHotList.Selections[0].Text]), [msmbAll, msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
            2: Answer := mmrNo;
            3: Answer := mmrYes;
            else
              Answer := mmrCancel; //Should not happen, but just in case
          end;

          case Answer of
            mmrAll:
              begin
                DeleteDispatcher:=3;
                DeleteSelectionAndSetNodeAfterDeletion;
              end;
            mmrYes: DeleteSelectionAndSetNodeAfterDeletion;

            mmrNo:
            begin
              NodeAfterDeletion:=tvDirectoryHotList.Selections[0].GetFirstChild;
              repeat
                tvDirectoryHotList.Selections[0].GetFirstChild.MoveTo(tvDirectoryHotList.Selections[0].GetFirstChild.Parent,naInsert);
              until tvDirectoryHotList.Selections[0].GetFirstChild=nil;
              tvDirectoryHotList.Selections[0].Delete;
              ClearCutAndPasteList;
            end;

            else
              FlagQuitDeleting:=TRUE;
          end;
        end;
      end;
      if (NodeAfterDeletion=nil) AND (FlagQuitDeleting=FALSE) AND (tvDirectoryHotList.Items.Count>0) then NodeAfterDeletion:=tvDirectoryHotList.Items.Item[0];
      if (NodeAfterDeletion<>nil) AND (FlagQuitDeleting=FALSE) then NodeAfterDeletion.Selected:=TRUE;
    finally
      tvDirectoryHotlist.Enabled:=TRUE;
      if isTreeHadFocus AND tvDirectoryHotlist.CanFocus then tvDirectoryHotlist.SetFocus;
    end;
    DirectoryHotlistTemp.FlagModified:=TRUE;
  end;
end;

{ TfrmOptionsDirectoryHotlist.miDeleteAllHotDirsClick }
procedure TfrmOptionsDirectoryHotlist.miDeleteAllHotDirsClick(Sender: TObject);
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
  MyYear, MyMonth, MyDay, MyHour, MyMin, MySec, MyMilSec: word;
  FreezeTime: TDateTime;
  WorkingDirectoryHotlist: TDirectoryHotlist;
  Answer: integer;
begin
  WorkingDirectoryHotlist := TDirectoryHotlist.Create;
  try
    with Sender as TComponent do
      ActionDispatcher := tag;

    case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
      ACTION_WITH_WINCMDINI:
      begin
        OpenDialog.DefaultExt := '*.ini';
        OpenDialog.FilterIndex := 1;
        OpenDialog.Title := rsMsgHotDirLocateTC;
        FlagKeepGoing := OpenDialog.Execute;
      end;

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
          FreezeTime := now;
          DecodeDate(Freezetime, MyYear, MyMonth, MyDay);
          DecodeTime(FreezeTime, MyHour, MyMin, MySec, MyMilSec);
          SaveDialog.Filename :=
            BackupPath + DirectorySeparator + 'Backup_' +
            Format('%d-%2.2d-%2.2d@%2.2d-%2.2d-%2.2d', [MyYear, MyMonth, MyDay,
            MyHour, MyMin, MySec]) + '.hotlist';
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
          CopyTTreeViewToAnother(tvDirectoryHotlist,tvDirectoryHotlistToExportImport);
          btnSelectAll.Caption := rsMsgHotDirExportall;
          btnSelectionDone.Caption := rsMsgHotDirExportSel;
          Caption := rsMsgHotDirExportHotlist;

          Answer := ShowModal;

          if ((Answer = mrOk) and (tvDirectoryHotlistToExportImport.SelectionCount > 0)) or  ((Answer = mrAll) and (tvDirectoryHotlistToExportImport.Items.Count > 0)) then
          begin
            WorkingDirectoryHotlist.AddFromAnotherTTreeViewTheSelected(nil,tvDirectoryHotlistToExportImport,(Answer=mrAll));

            if WorkingDirectoryHotlist.Count > 0 then
            begin
              case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
                {$IFDEF MSWINDOWS}
                ACTION_WITH_WINCMDINI: if WorkingDirectoryHotlist.ExportTotalCommander(OpenDialog.FileName, ((ActionDispatcher and MASK_FLUSHORNOT_EXISTING) =ACTION_ERASEEXISTING)) then msgOK(rsMsgHotDirTotalExported + IntToStr(WorkingDirectoryHotlist.Count)) else msgError(rsMsgHotDirErrorExporting);
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
  FlagKeepGoing: boolean;
  BackupPath: string;
begin
  with Sender as TComponent do
    ActionDispatcher := tag;

  case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
    ACTION_WITH_WINCMDINI:
    begin
      OpenDialog.DefaultExt := '*.ini';
      OpenDialog.FilterIndex := 1;
      OpenDialog.Title := rsMsgHotDirLocateTC;
      FlagKeepGoing := OpenDialog.Execute;
    end;

    ACTION_WITH_HOTLISTFILE:
    begin
      OpenDialog.DefaultExt := '*.hotlist';
      OpenDialog.FilterIndex := 3;
      OpenDialog.Title := rsMsgHotDirLocateHotlistFile;
      FlagKeepGoing := OpenDialog.Execute;
    end;

    ACTION_WITH_BACKUP:
    begin
      BackupPath := IncludeTrailingPathDelimiter(mbExpandFileName(EnvVarConfigPath)) + 'Backup';
      if mbForceDirectory(BackupPath) then
      begin
        OpenDialog.FilterIndex := 3;
        OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(BackupPath);
        OpenDialog.Title := rsMsgHotDirRestoreWhat;
        OpenDialog.Filter := 'Backup_*.hotlist';
        FlagKeepGoing := OpenDialog.Execute;
      end;
    end;
  end;

  if FlagKeepGoing then
  begin
    WorkingDirectoryList := TDirectoryHotlist.Create;
    try
      case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
        {$IFDEF MSWINDOWS}
        ACTION_WITH_WINCMDINI: WorkingDirectoryList.ImportTotalCommander(utf8string(OpenDialog.Filename));
        {$ENDIF}
        ACTION_WITH_HOTLISTFILE: WorkingDirectoryList.ImportDoubleCommander(utf8string(OpenDialog.Filename));
        ACTION_WITH_BACKUP: WorkingDirectoryList.ImportDoubleCommander(utf8string(OpenDialog.Filename));
      end;

      with Tfrmhotdirexportimport.Create(Application) do
      begin
        try
          WorkingDirectoryList.LoadTTreeView(tvDirectoryHotlistToExportImport,-1);
          btnSelectAll.Caption := rsMsgHotDirImportall;
          btnSelectionDone.Caption := rsMsgHotDirImportSel;
          Caption := rsMsgHotDirImportHotlist;

          case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
            ACTION_WITH_WINCMDINI: Answer := ShowModal;
            ACTION_WITH_HOTLISTFILE: Answer := ShowModal;
            ACTION_WITH_BACKUP:
            begin
              if MsgBox(rsHotDirWarningAbortRestoreBackup, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel) = mmrYes then Answer := mrAll else Exit;
            end;
          end;

          if ((Answer = mrOk) and (tvDirectoryHotlistToExportImport.SelectionCount > 0)) or ((Answer = mrAll) and (tvDirectoryHotlistToExportImport.Items.Count > 0)) then
          begin
            ClearCutAndPasteList;
            DirectoryHotlistTemp.FlagModified:=TRUE; //Ok.. It still might happen the list won't be modified but let's don't be too "picky"...

            if ((ActionDispatcher and MASK_ACTION_WITH_WHAT) = ACTION_WITH_BACKUP) AND (Answer = mrAll) then
            begin
              DirectoryHotlistTemp.Clear;
              tvDirectoryHotlist.Items.Clear;
            end;

            NbOfAdditional := DirectoryHotlistTemp.AddFromAnotherTTreeViewTheSelected(tvDirectoryHotlist, tvDirectoryHotlistToExportImport, (Answer = mrAll));
            if NbOfAdditional > 0 then
            begin
              //DirectoryHotlistTemp.LoadTTreeView(tvDirectoryHotlist,-1);
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

{ TfrmOptionsDirectoryHotlist.miSortDirectoryHotlistClick }
//The trick here is that a "group number" identical has been assigned to the sibling between separator and then we sort
//Teh sort has been arrange in such way that item from different group won't be mixed.
procedure TfrmOptionsDirectoryHotlist.miSortDirectoryHotlistClick(Sender: TObject);
var
  Dispatcher,Index:integer;
  StartingNode:TTreeNode;
  FlagKeepGoingBack:boolean;
begin
  with Sender as TComponent do Dispatcher:=tag;
  for Index:=0 to pred(tvDirectoryHotlist.Items.Count) do THotDir(tvDirectoryHotlist.Items.Item[Index].Data).GroupNumber:=0;

  GlobalGroupNumber:=0;

  if tvDirectoryHotlist.SelectionCount>0 then
  begin
    case Dispatcher of
      1,2: //current group only or current level
        begin
          for Index:=0 to pred(tvDirectoryHotlist.SelectionCount) do
          begin
            if THotDir(tvDirectoryHotlist.Selections[Index].Data).GroupNumber=0 then
            begin
              StartingNode:=tvDirectoryHotlist.Selections[Index];

              case Dispatcher of
                1: //We just need to make sure we start from first item of current level so we search the first one OR a separator
                  begin
                    FlagKeepGoingBack:=TRUE;
                    while FlagKeepGoingBack do
                    begin
                      if StartingNode.GetPrevSibling<>nil then
                      begin
                        if THotDir(StartingNode.GetPrevSibling.Data).Dispatcher<>hd_SEPARATOR then
                          StartingNode:=StartingNode.GetPrevSibling
                        else
                          FlagKeepGoingBack:=FALSE;
                      end
                      else
                      begin
                        FlagKeepGoingBack:=FALSE;
                      end;
                    end;
                  end;

                2: //We need to make sure we start from the first itm of current level
                  begin
                    while StartingNode.GetPrevSibling<>nil do StartingNode:=StartingNode.GetPrevSibling;
                  end;
              end;

              RecursiveSetGroupNumbers(StartingNode,GetNextGroupNumber,FALSE,(Dispatcher=1));
            end;
          end;
        end;

      3,4: //submenu only, recusive or not
        begin
          for Index:=0 to pred(tvDirectoryHotlist.SelectionCount) do
          begin
            StartingNode:=tvDirectoryHotlist.Selections[Index].GetFirstChild;
            if StartingNode<>nil then
            begin
              if THotDir(StartingNode.Data).GroupNumber=0 then
              begin
                RecursiveSetGroupNumbers(StartingNode,GetNextGroupNumber,(Dispatcher=4),FALSE);
              end;
            end;
          end;
        end;
    end;
  end;

  if Dispatcher=5 then //We start from the very first one, the top one.
  begin
    StartingNode:=tvDirectoryHotlist.Items.Item[0];
    RecursiveSetGroupNumbers(StartingNode,GetNextGroupNumber,TRUE,FALSE);
  end;

  //... and the finale!
  tvDirectoryHotlist.CustomSort(@MySortViaGroup);
  DirectoryHotlistTemp.FlagModified:=TRUE;
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
  p.x:=p.x+tvDirectoryHotlist.Width;
  pmHotDirTestMenu.PopUp(p.X, p.Y);
end;

{ TfrmOptionsDirectoryHotlist.miDetectIfPathExistClick }
procedure TfrmOptionsDirectoryHotlist.miDetectIfPathExistClick(Sender: TObject);
begin
    lbleditHotDirName.Text := '';
    lbleditHotDirName.Enabled := False;
    lbleditHotDirPath.Visible := False;
    cbSortHotDirPath.Visible := False;
    cbSortHotDirTarget.Visible := False;
    lbleditHotDirTarget.Visible := False;
    btnRelativePath.Visible := False;
    btnRelativeTarget.Visible := False;
    Application.ProcessMessages;
    with Sender as TComponent do RefreshExistingProperty(tag);
end;

{ TfrmOptionsDirectoryHotlist.ClearCutAndPasteList }
procedure TfrmOptionsDirectoryHotlist.ClearCutAndPasteList;
begin
  CutAndPasteIndexList.Clear;
  miPasteSelection.Enabled:=TRUE;
end;

{ TfrmOptionsDirectoryHotlist.miCutSelectionClick }
procedure TfrmOptionsDirectoryHotlist.miCutSelectionClick(Sender: TObject);
var
  Index:integer;
begin
  if tvDirectoryHotlist.SelectionCount>0 then
  begin
    for Index:=0 to pred(tvDirectoryHotlist.SelectionCount) do
    begin
      CutAndPasteIndexList.Add(IntToStr(tvDirectoryHotlist.Selections[Index].AbsoluteIndex));
    end;

    miPasteSelection.Enabled:=TRUE;
  end;
end;

{ TfrmOptionsDirectoryHotlist.miPasteSelectionClick }
procedure TfrmOptionsDirectoryHotlist.miPasteSelectionClick(Sender: TObject);
var
  DestinationNode:TTreeNode;
  Index:longint;
begin
  if CutAndPasteIndexList.Count>0 then
  begin
    DestinationNode:=tvDirectoryHotlist.Selected;
    if DestinationNode<>nil then
    begin
      tvDirectoryHotlist.ClearSelection(FALSE);
      for Index:=0 to pred(CutAndPasteIndexList.Count) do
      begin
        tvDirectoryHotlist.Items.Item[StrToInt(CutAndPasteIndexList.Strings[Index])].Selected:=TRUE;
      end;

      for Index:=0 to pred(tvDirectoryHotlist.SelectionCount) do
      begin
        tvDirectoryHotlist.Selections[Index].MoveTo(DestinationNode,naInsert);
      end;
      ClearCutAndPasteList;
    end;
  end;
end;


{ TfrmOptionsDirectoryHotlist.lbleditHotDirEnter }
procedure TfrmOptionsDirectoryHotlist.lbleditHotDirEnter(Sender: TObject);
begin
  with Sender as TLabeledEdit do
  begin
    pmPathHelper.Tag := tag;
    Font.Style := [fsBold];
    EditLabel.Font.Style := [fsBold];
  end;
end;

{ TfrmOptionsDirectoryHotlist.lbleditHotDirExit }
procedure TfrmOptionsDirectoryHotlist.lbleditHotDirExit(Sender: TObject);
begin
  with Sender as TLabeledEdit do
  begin
    pmPathHelper.Tag := 0;
    Font.Style := [];
    EditLabel.Font.Style := []; //Text not in bold anymore

    case tag of
      1: //Hot dir name
      begin
        try
          if (Text <> '') and (Text[1] <> '-') then
            //Make sure we actually have something, not an attempf of submenu or end of menu
          begin
            if THotDir(tvDirectoryHotlist.Selected.Data).HotDirName <> Text then
              //Make sure it's different than what it was
            begin
              DirectoryHotlistTemp.FlagModified := True;
              THotDir(tvDirectoryHotlist.Selected.Data).HotDirName := Text;
              tvDirectoryHotlist.Selected.Text:=Text;
            end;
          end;
        except
          //Just in case the "Text" is empty to don't show error with Text[1] check.
        end;
      end;

      2: //Hot dir path
      begin
        try
          if (Text <> '') and (THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher =
            hd_CHANGEPATH) then
            Text := IncludeTrailingPathDelimiter(Text);
          if THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath <> Text then
            //Make sure it's different than what it was
          begin
            DirectoryHotlistTemp.FlagModified := True;
            THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath := Text;
            THotDir(tvDirectoryHotlist.Selected.Data).HotDirExisting := DirExistUnknown;
          end;
        except
          //Just in case we have an empty list so "DirectoryHotlistTemp.HotDir[tvDirectoryHotlist.Selected.ImageIndex]" will not caused an error (since ItemIndex=-1 at this moment);
        end;
      end;

      3: //Hot dir target
      begin
        try
          if (Text <> '') and (THotDir(tvDirectoryHotlist.Selected.Data).Dispatcher =
            hd_CHANGEPATH) then
            Text := IncludeTrailingPathDelimiter(Text);
          if THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget <> Text then
            //Make sure it's different than what it was
          begin
            DirectoryHotlistTemp.FlagModified := True;
            THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget := Text;
          end;
        except
          //Just in case we have an empty list so "DirectoryHotlistTemp.HotDir[tvDirectoryHotlist.Selected.ImageIndex]" will not caused an error (since ItemIndex=-1 at this moment);
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.lbleditHotDirKeyPress }
procedure TfrmOptionsDirectoryHotlist.lbleditHotDirKeyPress(Sender: TObject; var Key: char);
begin
  case Ord(Key) of
     $0D: //Enter? Let's save the field and go to next one
     begin
       lbleditHotDirExit(Sender); //Doing this will SAVE the new typed text if it's different than what we have in memory for the entry. Then we could attempt to quit.
       //Key := #00; --> We don't do that because we want the "ENTER" key still quit the editor like it does for other config window
     end;

    $1B: //Escape? Place back the fields like they were
    begin
      //If typed text has been changed, yes we will restore it but if it was not change, we will quit so user won't have to press two times escape
      case tag of
        1: if Text <> THotDir(tvDirectoryHotlist.Selected.Data).HotDirName then Key := #$00;
        2: if Text <> THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath then Key := #00;
        3: if Text <> THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget then Key := #00;
      end;

      with Sender as TLabeledEdit do
      begin
        case tag of
          1: tvDirectoryHotlistSelectionChanged(tvDirectoryHotlist);
          2: Text := THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath;
          3: Text := THotDir(tvDirectoryHotlist.Selected.Data).HotDirTarget;
        end;
      end;

      if key <> #$1B then tvDirectoryHotlist.SetFocus;
    end;
  end;

  Application.ProcessMessages;
end;

{ TfrmOptionsDirectoryHotlist.lbleditHotDirMouseDown }
procedure TfrmOptionsDirectoryHotlist.lbleditHotDirMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  with Sender as TComponent do
    pmPathHelper.Tag := tag;
end;

{ TfrmOptionsDirectoryHotlist.anyRelativeAbsolutePathClick }
procedure TfrmOptionsDirectoryHotlist.anyRelativeAbsolutePathClick(Sender: TObject);
begin
  with Sender as TComponent do
  begin
    case tag of
      2:
      begin
        lbleditHotDirPath.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirPath, pfPATH);
        pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        if THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath <>lbleditHotDirPath.Text then
        begin
          THotDir(tvDirectoryHotlist.Selected.Data).HotDirPath := lbleditHotDirPath.Text;
          DirectoryHotlistTemp.FlagModified := True;
        end;
      end;

      4:
      begin
        lbleditHotDirPath.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirPath, pfPATH);
        pmCommandHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end;

      3:
      begin
        lbleditHotDirTarget.SetFocus;
        gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirTarget, pfPATH);
        pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.cbSortHotDirPathChange }
procedure TfrmOptionsDirectoryHotlist.cbSortHotDirPathChange(Sender: TObject);
begin
  DirectoryHotlistTemp.FlagModified := True;
  THotDir(tvDirectoryHotlist.Selected.Data).HotDirPathSort:=cbSortHotDirPath.ItemIndex;
end;

{ TfrmOptionsDirectoryHotlist.cbSortHotDirTargetChange }
procedure TfrmOptionsDirectoryHotlist.cbSortHotDirTargetChange(Sender: TObject);
begin
  DirectoryHotlistTemp.FlagModified := True;
  THotDir(tvDirectoryHotlist.Selected.Data).HotDirTargetSort:=cbSortHotDirTarget.ItemIndex;
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

{ TfrmOptionsDirectoryHotlist.ActualAddDirectories }
function TfrmOptionsDirectoryHotlist.ActualAddDirectories(ParamDispatcher: TKindOfHotDirEntry; sName, sPath, sTarget: string; InsertOrAdd:integer): TTreeNode;
var
  LocalHotDir: THotDir;
  WorkingTreeNode:TTreeNode;
const
  SelectedNoAttachedMode:array[1..3] of TNodeAttachMode = (naInsert,naInsertBehind,naAddChildFirst);
begin
  ClearCutAndPasteList;
  LocalHotDir := THotDir.Create;
  LocalHotDir.Dispatcher := ParamDispatcher;
  LocalHotDir.HotDirName := sName;
  LocalHotDir.HotDirPath := IncludeTrailingPathDelimiter(sPath);
  if sTarget <> '' then LocalHotDir.HotDirTarget := IncludeTrailingPathDelimiter(sTarget);
  DirectoryHotlistTemp.Add(LocalHotDir);
  DirectoryHotlistTemp.FlagModified := True;
  WorkingTreeNode:=tvDirectoryHotlist.Selected;
  if WorkingTreeNode<>nil then
    result:=tvDirectoryHotlist.Items.AddNode(nil,WorkingTreeNode, sName,LocalHotDir,SelectedNoAttachedMode[InsertOrAdd])
  else
    result:=tvDirectoryHotlist.Items.AddNode(nil,nil,sName,LocalHotDir,naAddFirst);
  case ParamDispatcher of
    hd_STARTMENU:
    begin
      result.ImageIndex:=ICONINDEX_SUBMENU;
      result.SelectedIndex:=ICONINDEX_SUBMENU;
      result.StateIndex:=ICONINDEX_SUBMENU;
    end;

    hd_CHANGEPATH:
    begin
      result.ImageIndex:=ICONINDEX_NEWADDEDDIRECTORY;
      result.SelectedIndex:=ICONINDEX_NEWADDEDDIRECTORY;
      result.StateIndex:=ICONINDEX_NEWADDEDDIRECTORY;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.RefreshTreeView }
procedure TfrmOptionsDirectoryHotlist.RefreshTreeView(NodeToSelect:TTreeNode);
begin
  if NodeToSelect<>nil THEN
    begin
      tvDirectoryHotlist.ClearSelection(FALSE);
      NodeToSelect.Selected:=TRUE;
    end
  else
    begin
      tvDirectoryHotlistSelectionChanged(tvDirectoryHotlist); //At least to hide path, target, etc.
    end;
  btnExport.Enabled := (tvDirectoryHotlist.Items.Count > 0);
  miSaveBackupHotlist.Enabled := (tvDirectoryHotlist.Items.Count > 0);
end;

function CompareStringsFromTStringList(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStrings(List.Strings[Index1], List.Strings[Index2], gSortNatural, gSortCaseSensitivity);
end;

{ TfrmOptionsDirectoryHotlist.TryToGetCloserHotDir }
//This routine "tries" to find the best place to eventually add a new directory in the tree accoring to directory names and ones alreayd in the tree.
//ALSO, it will set the flag "bShouldBeAfter" to indicate if it should be "BEFORE" or "AFTER" what is returned.
//"PerfectMatchIndex" is when the directory is found *exactly* in the tree as is. In other words, already there.
//"SecondAlternative" is when the directory is not there, but one close to it is
//
function TfrmOptionsDirectoryHotlist.TryToGetCloserHotDir(sDirToFindAPlaceFor: string; var TypeOfAddition:Integer): TTreeNode;
var
  BestOne, I: integer;
  localDirToFindAPlaceFor: string;
  sRepresentantString,sUnderPart,sOverPart:string;
  MagickSortedList:TStringList;

function GetNumberOfIdenticalStartChars(A:string):longint;
var
  I:integer;
begin
  result:=0;
  I:=1;
  while (I<UTF8Length(A)) AND (I<UTF8Length(localDirToFindAPlaceFor)) do
  begin
    if A[I]=localDirToFindAPlaceFor[I] then inc(result) else I:=UTF8Length(A);
    inc(I);
  end;
end;

function GetBestDir(DirA,DirB:string):integer;
var
  lengthA,lengthB:integer;
begin
  lengthA:=GetNumberOfIdenticalStartChars(DirA);
  lengthB:=GetNumberOfIdenticalStartChars(DirB);
  if (lengthA=0) AND (lengthB=0) then
  begin
    result:=0;
  end
  else
  begin
    if lengthA>lengthB then result:=-1 else result:=1;
  end;
end;

begin
  result:=nil;
  TypeOfAddition:=ACTION_ADDHOTDIR;

  localDirToFindAPlaceFor := UTF8LowerCase(IncludeTrailingPathDelimiter(sDirToFindAPlaceFor));

  //1st, let's try to see if we have an entry with the same *exact* directory
  I := 0;
  while (result=nil) AND (I < tvDirectoryHotlist.Items.Count) do
  begin
    if THotDir(tvDirectoryHotlist.Items.Item[I].Data).Dispatcher = hd_CHANGEPATH then
    begin
      if localDirToFindAPlaceFor = UTF8LowerCase(IncludeTrailingPathDelimiter(ExpandFileName(ReplaceEnvVars(THotDir(tvDirectoryHotlist.Items.Item[I].Data).HotDirPath)))) then result:=tvDirectoryHotlist.Items.Item[I];
    end;
    inc(I);
  end;

  //2nd, if nothing found, here is the "lazy-but-probably-easiest-to-write-method"
  if result=nil then
  begin
    MagickSortedList:=TStringList.Create;
    try
      MagickSortedList.Clear;

      for I:=0 to pred(tvDirectoryHotlist.Items.Count) do
      begin
        if THotDir(tvDirectoryHotlist.Items.Item[I].Data).Dispatcher = hd_CHANGEPATH then
        begin
          sRepresentantString:=UTF8LowerCase(IncludeTrailingPathDelimiter(ExpandFileName(ReplaceEnvVars(THotDir(tvDirectoryHotlist.Items.Item[I].Data).HotDirPath))))+IntToStr(I);
          MagickSortedList.Add(sRepresentantString);
        end;
      end;
      MagickSortedList.Add(localDirToFindAPlaceFor);

      //We call a custom sort to make sure sort order will make the sequence "école - Eric - Érika"
      MagickSortedList.CustomSort(@CompareStringsFromTStringList);

      I:=MagickSortedList.IndexOf(localDirToFindAPlaceFor);

      if I=0 then sUnderPart:='' else sUnderPart:=UTF8LowerCase(IncludeTrailingPathDelimiter(ExpandFileName(ReplaceEnvVars(THotDir(tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I-1]))].Data).HotDirPath))));
      if I=pred(MagickSortedList.Count) then sOverPart:='' else sOverPart:=UTF8LowerCase(IncludeTrailingPathDelimiter(ExpandFileName(ReplaceEnvVars(THotDir(tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I+1]))].Data).HotDirPath))));
      BestOne:=GetBestDir(sUnderPart,sOverPart);

      case BestOne of
        -1: result:=tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I-1]))];
         1: result:=tvDirectoryHotlist.Items.Item[StrToInt(GetLastDir(MagickSortedList.Strings[I+1]))];
      end;

      if result<>nil then
      begin
        if CompareStrings(localDirToFindAPlaceFor, UTF8LowerCase(IncludeTrailingPathDelimiter(ExpandFileName(ReplaceEnvVars(tHotDir(result.Data).HotDirPath)))), gSortNatural, gSortCaseSensitivity) = -1 then TypeOfAddition:=ACTION_INSERTHOTDIR;
      end;
    finally
      MagickSortedList.Free;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.TryToGetExactHotDir }
function TfrmOptionsDirectoryHotlist.TryToGetExactHotDir(const index:integer):TTreeNode;
var
  SearchingtvIndex:integer;
begin
  result:=nil;

  SearchingtvIndex:=0;
  while (SearchingtvIndex<pred(tvDirectoryHotlist.Items.count)) AND (result=nil) do
  begin
    if tvDirectoryHotlist.Items[SearchingtvIndex].Data = DirectoryHotlistTemp.Items[Index] then result:=tvDirectoryHotlist.Items[SearchingtvIndex] else inc(SearchingtvIndex);
  end;
end;

{ TfrmOptionsDirectoryHotlist.RecursiveSetGroupNumbers }
procedure TfrmOptionsDirectoryHotlist.RecursiveSetGroupNumbers(ParamNode:TTreeNode; ParamGroupNumber:integer; DoRecursion,StopAtFirstGroup:boolean);
var
  MaybeChild:TTreeNode;
begin
  repeat
    if DoRecursion then
    begin
      MaybeChild:=ParamNode.GetFirstChild;
      if MaybeChild<>nil then RecursiveSetGroupNumbers(MaybeChild,GetNextGroupNumber,DoRecursion,StopAtFirstGroup);
    end;

    if THotDir(ParamNode.Data).Dispatcher<>hd_SEPARATOR then
      begin
        THotDir(ParamNode.Data).GroupNumber:=ParamGroupNumber;
      end
    else
      begin
        ParamGroupNumber:=GetNextGroupNumber;
        if StopAtFirstGroup then while ParamNode<>nil do ParamNode:=ParamNode.GetNextSibling; //To exit the loop!
      end;

    if ParamNode<>nil then ParamNode:=ParamNode.GetNextSibling;
  until ParamNode=nil;
end;

{ TfrmOptionsDirectoryHotlist.RefreshExistingProperty }
procedure TfrmOptionsDirectoryHotlist.RefreshExistingProperty(ScanMode: integer);
var
  Index, LocalThreadCount: longint;
  ListOfAlreadyCheckDrive, ListOfNonExistingDrive: TStringList;
  RememberCursor: TCursor;
  FreezeTime : dword;

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

  procedure RecursivelySetIconFolderNotPresent(WorkingTreeNode:TTreeNode);
  begin
    if WorkingTreeNode.Parent<>nil then
    begin
      if WorkingTreeNode.Parent.ImageIndex<>ICONINDEX_SUBMENUWITHMISSING then
      begin
        THotDir(WorkingTreeNode.Parent.Data).HotDirExisting:=DirNotExist;
        WorkingTreeNode.Parent.ImageIndex:=ICONINDEX_SUBMENUWITHMISSING;
        WorkingTreeNode.Parent.SelectedIndex:=ICONINDEX_SUBMENUWITHMISSING;
        WorkingTreeNode.Parent.StateIndex:=ICONINDEX_SUBMENUWITHMISSING;
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
        DirectoryHotlistTemp.FlagModified := DirectoryHotlistTemp.FlagModified or (THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting <> DirNotExist);
        THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting := DirNotExist;
        tvDirectoryHotlist.Items.Item[Index].ImageIndex:=ICONINDEX_DIRECTORYNOTPRESENTHERE;
        tvDirectoryHotlist.Items.Item[Index].SelectedIndex:=ICONINDEX_DIRECTORYNOTPRESENTHERE;
        tvDirectoryHotlist.Items.Item[Index].StateIndex:=ICONINDEX_DIRECTORYNOTPRESENTHERE;
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
      while (LocalThreadCount <> 0) AND ((FreezeTime+10000)>FreezeTime) do
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
                  DirectoryHotlistTemp.FlagModified := DirectoryHotlistTemp.FlagModified or (THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting <> DirExist);
                  THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting := DirExist;
                end;

                2:
                begin
                  if CheckIfThisDirectoryExists(THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirTarget) then
                  begin
                    DirectoryHotlistTemp.FlagModified := DirectoryHotlistTemp.FlagModified or (THotDir(tvDirectoryHotlist.Items.Item[Index].Data).HotDirExisting <> DirExist);
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
    end;

  finally
    Screen.Cursor := RememberCursor;
  end;

  tvDirectoryHotlist.Refresh;
end;

{ TfrmOptionsDirectoryHotlist.SetNormalIconsInTreeView }
procedure TfrmOptionsDirectoryHotlist.SetNormalIconsInTreeView;
var
  Index:integer;
begin
  for Index:=0 to pred(tvDirectoryHotlist.Items.count) do
  begin
    if tvDirectoryHotlist.Items.Item[Index].GetFirstChild=nil then
    begin
      tvDirectoryHotlist.Items.Item[Index].ImageIndex:=-1;
      tvDirectoryHotlist.Items.Item[Index].SelectedIndex:=-1;
      tvDirectoryHotlist.Items.Item[Index].StateIndex:=-1;
    end
    else
    begin
      tvDirectoryHotlist.Items.Item[Index].ImageIndex:=ICONINDEX_SUBMENU;
      tvDirectoryHotlist.Items.Item[Index].SelectedIndex:=ICONINDEX_SUBMENU;
      tvDirectoryHotlist.Items.Item[Index].StateIndex:=ICONINDEX_SUBMENU;
    end;
  end;
end;

{ TfrmOptionsDirectoryHotlist.MySortViaGroup }
function TfrmOptionsDirectoryHotlist.MySortViaGroup(Node1, Node2: TTreeNode): integer;
begin
  if (THotdir(Node1.Data).GroupNumber=THotDir(Node2.Data).GroupNumber) AND (THotdir(Node1.Data).GroupNumber<>0) then
  begin
    result:=CompareStrings(THotdir(Node1.Data).HotDirName,THotDir(Node2.Data).HotDirName, gSortNatural, gSortCaseSensitivity)
  end
  else
  begin
    if Node1.AbsoluteIndex<Node2.AbsoluteIndex then result:=-1 else result:=1;
  end;
end;

{ TfrmOptionsDirectoryHotlist.CopyTTreeViewToAnother }
procedure TfrmOptionsDirectoryHotlist.CopyTTreeViewToAnother(tvSource,tvDestination:TTreeView);
  procedure RecursiveNodeCopy(SourceNode,DestNode:TTreeNode);
  var
    NewNode:TTreeNode;
  begin
    repeat
      NewNode:=tvDestination.Items.AddChild(DestNode,SourceNode.Text);
      NewNode.Assign(SourceNode);
      if SourceNode.GetFirstChild<>nil then
      begin
        RecursiveNodeCopy(SourceNode.GetFirstChild,NewNode);
      end;
      SourceNode:=SourceNode.GetNextSibling;
    until SourceNode=nil;
  end;
begin
  if tvSource.Items.GetFirstNode<>nil then RecursiveNodeCopy(tvSource.Items.GetFirstNode,nil);
end;

{ TfrmOptionsDirectoryHotlist.GetNextGroupNumber }
function TfrmOptionsDirectoryHotlist.GetNextGroupNumber:integer;
begin
  GlobalGroupNumber:=GlobalGroupNumber+1;
  result:=GlobalGroupNumber;
end;

{ TfrmOptionsDirectoryHotlist.SubmitToAddOrConfigToHotDirDlg }
procedure TfrmOptionsDirectoryHotlist.SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher,paramPath,paramTarget,paramOptionalIndex: string);
var
  ActualActionDispatcher, TypeOfAddition, IndexFile: longint;
  sTempo: string;
  NodeToSelect:TTreeNode=nil;
  SelectedOrActiveDirectories:TFiles;
  iOptionalIndex:integer;

  procedure AddThisSubmittedDirectory(DirectoryPath:string);
  begin
    if cbAddTarget.Checked then sTempo := IncludeTrailingPathDelimiter(paramTarget) else sTempo := '';
    if gWhereToAddNewHotDir = ahdLast then TypeOfAddition:=ACTION_ADDHOTDIR else TypeOfAddition:=ACTION_INSERTHOTDIR;

    NodeToSelect:=nil;
    if (tvDirectoryHotlist.Items.Count>0) then
    begin
      case gWhereToAddNewHotDir of
        ahdFirst: NodeToSelect:=tvDirectoryHotlist.Items.Item[0];
        ahdLast : NodeToSelect:=tvDirectoryHotlist.Items.Item[pred(tvDirectoryHotlist.Items.Count)];
        ahdSmart: NodeToSelect:=TryToGetCloserHotDir(DirectoryPath,TypeOfAddition);
        else NodeToSelect:=tvDirectoryHotlist.Items.Item[0];
      end;
      if NodeToSelect<>nil then NodeToSelect.Selected:=TRUE;
    end;
    NodeToSelect := ActualAddDirectories(hd_CHANGEPATH, GetLastDir(DirectoryPath), DirectoryPath, sTempo,TypeOfAddition);
  end;

begin
  if UTF8LowerCase(paramActionDispatcher)='add' then ActualActionDispatcher:=ACTION_ADDTOHOTLIST
  else if UTF8LowerCase(paramActionDispatcher)='addsel' then ActualActionDispatcher:=ACTION_ADDSELECTEDDIR
  else if UTF8LowerCase(paramActionDispatcher)='config' then ActualActionDispatcher:=ACTION_CONFIGTOHOTLIST
  else if UTF8LowerCase(paramActionDispatcher)='directconfig' then ActualActionDispatcher:=ACTION_DIRECTLYCONFIGENTRY
  else ActualActionDispatcher:=-1;

  iOptionalIndex:=StrToInt(paramOptionalIndex);

  case ActualActionDispatcher of
    ACTION_ADDTOHOTLIST:
    begin
      AddThisSubmittedDirectory(paramPath);
    end;

    ACTION_CONFIGTOHOTLIST:
    begin
      NodeToSelect := TryToGetCloserHotDir(paramPath,TypeOfAddition);
    end;

    ACTION_JUSTSHOWCONFIGHOTLIST:
    begin
      NodeToSelect:=tvDirectoryHotlist.Items.Item[0];
    end;

    ACTION_ADDSELECTEDDIR:
    begin
      SelectedOrActiveDirectories:=frmMain.ActiveFrame.CloneSelectedOrActiveDirectories;
      try
        if SelectedOrActiveDirectories.count>0 then
        begin
          for IndexFile:=0 to pred(SelectedOrActiveDirectories.Count) do AddThisSubmittedDirectory(ExcludeTrailingPathDelimiter(SelectedOrActiveDirectories[IndexFile].FullPath));
        end;
      finally
        FreeAndNil(SelectedOrActiveDirectories);
      end;
    end;

    ACTION_DIRECTLYCONFIGENTRY:
    begin
      NodeToSelect := TryToGetExactHotDir(iOptionalIndex);
    end
  end;

  if (NodeToSelect=nil) AND (tvDirectoryHotlist.Items.Count>0) then NodeToSelect:=tvDirectoryHotlist.Items.Item[0];
  RefreshTreeView(NodeToSelect);

  //2014-08-27: These lines are a workaround a problem present at this moment in Lazarus regarding TSpeedButton present inside a TGroupBox.
  //See on the web if the following case is solved prior to remove these lines: http://bugs.freepascal.org/view.php?id=26638
  {$IFDEF MSWINDOWS}
  if tvDirectoryHotlist.CanFocus then
  begin
    LCLSendMouseDownMsg(Self,1,1,mbLeft,[]);
    LCLSendMouseUpMsg(Self,1,1,mbLeft,[]);
  end;
  {$ENDIF MSWINDOWS}

  if not tvDirectoryHotlist.Focused then if tvDirectoryHotlist.CanFocus then tvDirectoryHotlist.SetFocus;
  if not lbleditHotDirName.Focused then if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
end;

{ TODO -oDB : Would be nice if directory does not exist to offer immediately to re-configure it. }
{ TODO -oDB : Would be nice to be able to SEARCH through. Something like "Start Search" and then "Search again". }
{ TODO -oDB : Would be nice to be able to do a kind of "Search and replace" like changing all the "X:\" by "\\whatever_server\" for example. }
{ TODO -oDB : Be able to add a quick 16x16 icon to some friendly shortcut like a little star or something to help to see a special entry. }
{ TODO -oDB : Would be nice to have also a COPY-and-PASTE in addition to CUT-and-PASTE. Also, make sure to create new THotDir entry, not just copy entries in tree otherwise it's not good. }
end.

