{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains DC actions of the main form

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit uMainCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, uFileView, uFileViewNotebook, uFileSourceOperation,
  uGlobs, uFileFunctions, uFormCommands, uFileSorting, uShellContextMenu, Menus, ufavoritetabs,ufile;

type

  TCopyFileNamesToClipboard = (cfntcPathAndFileNames, cfntcJustFileNames, cfntcJustPathWithSeparator, cfntcPathWithoutSeparator);

  { TProcedureDoingActionOnMultipleTabs }
  TProcedureDoingActionOnMultipleTabs = procedure(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer) of object;

  { TMainCommands }

  TMainCommands = class(TComponent{$IF FPC_FULLVERSION >= 020501}, IFormCommands{$ENDIF})
  private
   FCommands: TFormCommands;
   FOriginalNumberOfTabs: integer;

   // Helper routines
   procedure TryGetParentDir(FileView: TFileView; var SelectedFiles: TFiles);

   // Filters out commands.
   function CommandsFilter(Command: String): Boolean;
   procedure OnCopyOutStateChanged(Operation: TFileSourceOperation;
                                   State: TFileSourceOperationState);
   procedure OnEditCopyOutStateChanged(Operation: TFileSourceOperation;
                                       State: TFileSourceOperationState);
   procedure OnCalcStatisticsStateChanged(Operation: TFileSourceOperation;
                                          State: TFileSourceOperationState);
   procedure OnCalcChecksumStateChanged(Operation: TFileSourceOperation;
                                        State: TFileSourceOperationState);

  public
   constructor Create(TheOwner: TComponent; ActionList: TActionList = nil); reintroduce;

   property Commands: TFormCommands read FCommands{$IF FPC_FULLVERSION >= 020501} implements IFormCommands{$ENDIF};

   //---------------------
   // The Do... functions are cm_... functions' counterparts which are to be
   // executed directly from the code with specific - especially non-string
   // - arguments (instead of calling cm_... functions, in which case
   // parameters would have to be converted to and from strings).
   //
   procedure DoOpenVirtualFileSystemList(Panel: TFileView);
   procedure DoPanelsSplitterPerPos(SplitPos: Integer);
   procedure DoUpdateFileView(AFileView: TFileView; {%H-}UserData: Pointer);
   procedure DoCloseTab(Notebook: TFileViewNotebook; PageIndex: Integer);
   procedure DoCopySelectedFileNamesToClipboard(FileView: TFileView; TypeOfCopy: TCopyFileNamesToClipboard);
   procedure DoNewTab(Notebook: TFileViewNotebook);
   procedure DoRenameTab(Page: TFileViewPage);
   procedure DoContextMenu(Panel: TFileView; X, Y: Integer; Background: Boolean; UserWishForContextMenu:TUserWishForContextMenu = uwcmComplete);
   procedure DoTransferPath(SourceFrame: TFileView; TargetNotebook: TFileViewNotebook); overload;
   procedure DoTransferPath(SourcePage: TFileViewPage; TargetPage: TFileViewPage; FromActivePanel: Boolean);
   procedure DoSortByFunctions(View: TFileView; FileFunctions: TFileFunctions);
   procedure DoShowMainMenu(bShow: Boolean);
   procedure DoShowCmdLineHistory(bNextCmdLine: Boolean);
   procedure DoChangeDirToRoot(FileView: TFileView);
   procedure GetAndSetMultitabsActionFromParams(const Params: array of string; var APanelSide:TTabsConfigLocation; var ActionOnLocked:boolean; var AskForLocked:integer);
   procedure DoActionOnMultipleTabs(const Params: array of string; ProcedureDoingActionOnMultipleTabs: TProcedureDoingActionOnMultipleTabs);
   procedure DoCloseAllTabs(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
   procedure DoCloseDuplicateTabs(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
   procedure DoSetAllTabsOptionNormal(ANotebook: TFileViewNotebook; var {%H-}bAbort: boolean; {%H-}bDoLocked: boolean; var {%H-}iAskForLocked: integer);
   procedure DoSetAllTabsOptionPathLocked(ANotebook: TFileViewNotebook; var {%H-}bAbort: boolean; {%H-}bDoLocked: boolean; var {%H-}iAskForLocked: integer);
   procedure DoAllTabsOptionPathResets(ANotebook: TFileViewNotebook; var {%H-}bAbort: boolean; {%H-}bDoLocked: boolean; var {%H-}iAskForLocked: integer);
   procedure DoSetAllTabsOptionDirsInNewTab(ANotebook: TFileViewNotebook; var {%H-}bAbort: boolean; {%H-}bDoLocked: boolean; var {%H-}iAskForLocked: integer);
   procedure DoOnClickMenuJobFavoriteTabs(Sender: TObject);
   procedure DoCopyAllTabsToOppositeSide(ANotebook: TFileViewNotebook; var {%H-}bAbort: boolean; {%H-}bDoLocked: boolean; var {%H-}iAskForLocked: integer);
   procedure DoShowFavoriteTabsOptions;
   procedure DoParseParametersForPossibleTreeViewMenu(const Params: array of string; gDefaultConfigWithCommand, gDefaultConfigWithDoubleClick:boolean; var bUseTreeViewMenu:boolean; var bUsePanel:boolean; var p: TPoint);
   procedure DoComputeSizeAndPosForWindowInMiddle(var iPosX:integer; var iPosY:integer; var iWidth:integer; var iHeight:integer);
   procedure DoActualMarkApplyOnAll(const maoaDispatcher: TMarkApplyOnAllDispatcher; const Params: array of string);
   procedure DoActualMarkUnMark(const Params: array of string; bSelect: boolean);
   procedure DoActualAddToCommandLine(const Params: array of string; sAddedString:string; bAddSpaceAtTheEnd:boolean);

   //---------------------

 published
   //--------------------------------------------------------------------------
   // All commands can be split into three groups:
   // 1. Commands intended for the application (cm_VisitHomePage,
   //    cm_About, cm_Exit, ...).
   //
   // 2. Commands intended for file views (cm_QuickSearch, cm_EditPath, etc.).
   //    Those commands are simply redirected to the currently active file view by calling:
   //       frmMain.ActiveFrame.ExecuteCommand(CommandName, Param);
   //    If they are supported by the given file view they are executed there.
   //
   //    If in future there will be a need to pass specific parameters to the
   //    commands, i.e. not string, they should be implemented by creating
   //    an interface for each command, and each file view implementing those
   //    interfaces which commands it supports.
   //
   // 3. Commands intended for file sources (cm_Copy, cm_Rename, cm_MakeDir).
   //    The file operations will mostly work only for non-virtual file sources.
   //
   //--------------------------------------------------------------------------
   // RECIPE TO ADD A "cm_" COMMAND:
   //--------------------------------------------------------------------------
   // In this recipe, we use as an exemple the command "cm_SrcOpenDrives"
   // 1.  In "fMain" we add the action in the "actionLst".
   // 2.  Make sure we add it in the appropriate category.
   // 3.  The action name must start with "act" and have the exact same name as the "cm_" we want to add.
   // 4.  So if we want "cm_SrcOpenDrives", we name the action "actSrcOpenDrives".
   // 5.  By the way, "KEEP THE SAME SPELLING EVERYWHERE!".
   // 6.  The order in which the "cm_SrcOpenDrives" will appear, is the same as its position in the "actionLst".
   // 7.  So command is "cm_SrcOpenDrives", so keep writing "cm_SrcOpenDrives" and not "cm_srcopendrives" for example.
   // 8.  The only single place to have lowercases is for the icon name which will be "cm_srcopendrives" but it's the only one case.
   // 9.  Give an appropriate "caption" name for the command, so for our example "Open drive list"
   // 10. Set the "Tag" to the same number as the other command of the same category.
   // 11. In the "uMainCommands", for the type "TMainCommands", add the code for the command.
   // 12. The command name must start with "cm_" and ends with the same name as what we added for the "act".
   // 13. So with our example we add "cm_SrcOpenDrives".
   // 14. Create an icon for the command.
   // 15. Make a 24-bits with alpha .PNG file.
   // 16. Name the file with he same name of the "cm_" command.
   // 17. But write the name all in lower case so here "cm_srcopendrives".
   // 18. Store the file here: to path "pixmaps\dctheme\32x32\actions\".
   // 19. If command is a compatible on with TC, add it in unit "uTotalCommander".
   // 20. So with this example we add: "(TCCommand: 'cm_SrcOpenDrives'; TCIcon: -1; DCCommand: 'cm_SrcOpenDrives')".
   // 21. If command needs to have a shortcut, go in unit "uGlobs", go to routine "LoadDefaultHotkeyBindings"(more detailed - read instructions in head of "LoadDefaultHotkeyBindings") and add the appropriate "AddIfNotExists".
   // 22. Don't abuse on adding keyboard shortcut! We must let some user's keys for user!
   // 23. For this example, we won't add a keyboard shortcut. TC does'nt have neither.
   // 24. Edit the file "doc\en\cmds.html" to add help for the command.
   // 25. For the languages we know, translate the caption of the action added.
   // 26. For example in our example, it will be "tfrmmain.actsrcopendrives.caption" that will need to be change.
   // 27. It's important to * T E S T * the "cm_" command you add.
   // 28. Add a single button in the toolbar to test it works.
   // 29. Make sure we see the expected icon and the expected tooltip.
   // 30. Make sure the actual button you added do the expected task.
   // 31. If command is using parameters, make sure you test the most cases of parameters.
   // 32. If you added keyboard shortcut, make sure keyboard shortcut works.
   // 33. With the "cm_DoAnyCmCommand", go find in the "Internal Command Selector" the command you added.
   // 34. Make sure it's present there, under the appropriate category, sorted at the classic logical place.
   // 35. Make sure we see the shortcut if any and that the description is correct.
   // 36. Test the help for the command from there to make sure it links to the correct place in the help file.
   procedure cm_AddPathToCmdLine(const {%H-}Params: array of string);
   procedure cm_AddFilenameToCmdLine(const {%H-}Params: array of string);
   procedure cm_AddPathAndFilenameToCmdLine(const {%H-}Params: array of string);
   procedure cm_CmdLineNext(const {%H-}Params: array of string);
   procedure cm_CmdLinePrev(const {%H-}Params: array of string);
   procedure cm_ContextMenu(const Params: array of string);
   procedure cm_CopyFullNamesToClip(const {%H-}Params: array of string);
   procedure cm_CopyFileDetailsToClip(const {%H-}Params: array of string);
   procedure cm_Exchange(const {%H-}Params: array of string);
   procedure cm_FlatView(const {%H-}Params: array of string);
   procedure cm_LeftFlatView(const {%H-}Params: array of string);
   procedure cm_RightFlatView(const {%H-}Params: array of string);
   procedure cm_OpenArchive(const {%H-}Params: array of string);
   procedure cm_TestArchive(const {%H-}Params: array of string);
   procedure cm_OpenDirInNewTab(const {%H-}Params: array of string);
   procedure cm_Open(const {%H-}Params: array of string);
   procedure cm_ShellExecute(const Params: array of string);
   procedure cm_OpenVirtualFileSystemList(const {%H-}Params: array of string);
   procedure cm_TargetEqualSource(const {%H-}Params: array of string);
   procedure cm_LeftEqualRight(const {%H-}Params: array of string);
   procedure cm_RightEqualLeft(const {%H-}Params: array of string);
   procedure cm_PackFiles(const Params: array of string);
   procedure cm_ExtractFiles(const Params: array of string);
   procedure cm_QuickSearch(const Params: array of string);
   procedure cm_QuickFilter(const Params: array of string);
   procedure cm_SrcOpenDrives(const {%H-}Params: array of string);
   procedure cm_LeftOpenDrives(const {%H-}Params: array of string);
   procedure cm_RightOpenDrives(const {%H-}Params: array of string);
   procedure cm_OpenBar(const {%H-}Params: array of string);
   procedure cm_ShowButtonMenu(const Params: array of string);
   procedure cm_TransferLeft(const {%H-}Params: array of string);
   procedure cm_TransferRight(const {%H-}Params: array of string);
   procedure cm_GoToFirstFile(const {%H-}Params: array of string);
   procedure cm_GoToLastFile(const {%H-}Params: array of string);
   procedure cm_Minimize(const {%H-}Params: array of string);
   procedure cm_Wipe(const {%H-}Params: array of string);
   procedure cm_Exit(const {%H-}Params: array of string);
   procedure cm_NewTab(const {%H-}Params: array of string);
   procedure cm_RenameTab(const {%H-}Params: array of string);
   procedure cm_CloseTab(const {%H-}Params: array of string);
   procedure cm_CloseAllTabs(const Params: array of string);
   procedure cm_CloseDuplicateTabs(const Params: array of string);
   procedure cm_NextTab(const Params: array of string);
   procedure cm_PrevTab(const Params: array of string);
   procedure cm_SaveTabs(const Params: array of string);
   procedure cm_LoadTabs(const Params: array of string);
   procedure cm_SetTabOptionNormal(const Params: array of string);
   procedure cm_SetTabOptionPathLocked(const Params: array of string);
   procedure cm_SetTabOptionPathResets(const Params: array of string);
   procedure cm_SetTabOptionDirsInNewTab(const Params: array of string);
   procedure cm_Copy(const Params: array of string);
   procedure cm_CopyNoAsk(const Params: array of string);
   procedure cm_Delete(const Params: array of string);
   procedure cm_CheckSumCalc(const Params: array of string);
   procedure cm_CheckSumVerify(const Params: array of string);
   procedure cm_Edit(const Params: array of string);
   procedure cm_EditPath(const Params: array of string);
   procedure cm_MakeDir(const Params: array of string);
   procedure cm_Rename(const Params: array of string);
   procedure cm_RenameNoAsk(const Params: array of string);
   procedure cm_View(const Params: array of string);
   procedure cm_QuickView(const Params: array of string);
   procedure cm_BriefView(const Params: array of string);
   procedure cm_LeftBriefView(const Params: array of string);
   procedure cm_RightBriefView(const Params: array of string);
   procedure cm_ColumnsView(const Params: array of string);
   procedure cm_LeftColumnsView(const Params: array of string);
   procedure cm_RightColumnsView(const Params: array of string);
   procedure cm_ThumbnailsView(const Params: array of string);
   procedure cm_LeftThumbView(const Params: array of string);
   procedure cm_RightThumbView(const Params: array of string);
   procedure cm_TreeView(const Params: array of string);
   procedure cm_CopyNamesToClip(const {%H-}Params: array of string);
   procedure cm_FocusCmdLine(const {%H-}Params: array of string);
   procedure cm_FileAssoc(const {%H-}Params: array of string);
   procedure cm_HelpIndex(const {%H-}Params: array of string);
   procedure cm_Keyboard(const {%H-}Params: array of string);
   procedure cm_VisitHomePage(const {%H-}Params: array of string);
   procedure cm_About(const {%H-}Params: array of string);
   procedure cm_ShowSysFiles(const {%H-}Params: array of string);
   procedure cm_SwitchIgnoreList(const Params: array of string);
   procedure cm_Options(const Params: array of string);
   procedure cm_CompareContents(const Params: array of string);
   procedure cm_Refresh(const {%H-}Params: array of string);
   procedure cm_ShowMainMenu(const Params: array of string);
   procedure cm_DirHotList(const Params: array of string);
   procedure cm_ConfigDirHotList(const {%H-}Params: array of string);
   procedure cm_WorkWithDirectoryHotlist(const Params: array of string);
   procedure cm_MarkInvert(const Params: array of string);
   procedure cm_MarkMarkAll(const Params: array of string);
   procedure cm_MarkUnmarkAll(const Params: array of string);
   procedure cm_MarkPlus(const Params: array of string);
   procedure cm_MarkMinus(const Params: array of string);
   procedure cm_MarkCurrentName(const Params: array of string);
   procedure cm_UnmarkCurrentName(const Params: array of string);
   procedure cm_MarkCurrentNameExt(const Params: array of string);
   procedure cm_UnmarkCurrentNameExt(const Params: array of string);
   procedure cm_MarkCurrentExtension(const Params: array of string);
   procedure cm_UnmarkCurrentExtension(const Params: array of string);
   procedure cm_MarkCurrentPath(const Params: array of string);
   procedure cm_UnmarkCurrentPath(const Params: array of string);
   procedure cm_SaveSelection(const Params: array of string);
   procedure cm_RestoreSelection(const Params: array of string);
   procedure cm_SaveSelectionToFile(const Params: array of string);
   procedure cm_LoadSelectionFromFile(const Params: array of string);
   procedure cm_LoadSelectionFromClip(const Params: array of string);
   procedure cm_SyncDirs(const Params: array of string);
   procedure cm_Search(const Params: array of string);
   procedure cm_HardLink(const Params: array of string);
   procedure cm_MultiRename(const Params: array of string);
   procedure cm_ReverseOrder(const Params: array of string);
   procedure cm_LeftReverseOrder(const Params: array of string);
   procedure cm_RightReverseOrder(const Params: array of string);
   procedure cm_UniversalSingleDirectSort(const Params: array of string);
   procedure cm_SortByName(const Params: array of string);
   procedure cm_SortByExt(const Params: array of string);
   procedure cm_SortByDate(const Params: array of string);
   procedure cm_SortBySize(const Params: array of string);
   procedure cm_SortByAttr(const Params: array of string);
   procedure cm_LeftSortByName(const Params: array of string);
   procedure cm_LeftSortByExt(const Params: array of string);
   procedure cm_LeftSortByDate(const Params: array of string);
   procedure cm_LeftSortBySize(const Params: array of string);
   procedure cm_LeftSortByAttr(const Params: array of string);
   procedure cm_RightSortByName(const Params: array of string);
   procedure cm_RightSortByExt(const Params: array of string);
   procedure cm_RightSortByDate(const Params: array of string);
   procedure cm_RightSortBySize(const Params: array of string);
   procedure cm_RightSortByAttr(const Params: array of string);
   procedure cm_SymLink(const Params: array of string);
   procedure cm_CopySamePanel(const Params: array of string);
   procedure cm_DirHistory(const Params: array of string);
   procedure cm_ViewHistory(const Params: array of string);
   procedure cm_ViewHistoryPrev(const {%H-}Params: array of string);
   procedure cm_ViewHistoryNext(const {%H-}Params: array of string);
   procedure cm_EditNew(const Params: array of string);
   procedure cm_RenameOnly(const Params: array of string);
   procedure cm_RunTerm(const Params: array of string);
   procedure cm_ShowCmdLineHistory(const Params: array of string);
   procedure cm_ToggleFullscreenConsole(const Params: array of string);
   procedure cm_CalculateSpace(const Params: array of string);
   procedure cm_CountDirContent(const Params: array of string);
   procedure cm_SetFileProperties(const Params: array of string);
   procedure cm_FileProperties(const Params: array of string);
   procedure cm_FileLinker(const Params: array of string);
   procedure cm_FileSpliter(const Params: array of string);
   procedure cm_PanelsSplitterPerPos(const Params: array of string);
   procedure cm_EditComment(const Params: array of string);
   procedure cm_CopyToClipboard(const Params: array of string);
   procedure cm_CutToClipboard(const Params: array of string);
   procedure cm_PasteFromClipboard(const Params: array of string);
   procedure cm_ChangeDirToRoot(const Params: array of string);
   procedure cm_ChangeDirToHome(const Params: array of string);
   procedure cm_ChangeDirToParent(const Params: array of string);
   procedure cm_ChangeDir(const Params: array of string);
   procedure cm_ClearLogWindow(const Params: array of string);
   procedure cm_ClearLogFile(const Params: array of string);
   procedure cm_NetworkConnect(const Params: array of string);
   procedure cm_NetworkDisconnect(const Params: array of string);
   procedure cm_HorizontalFilePanels(const Params: array of string);
   procedure cm_OperationsViewer(const Params: array of string);
   procedure cm_CompareDirectories(const Params: array of string);
   procedure cm_ViewLogFile(const Params: array of string);
   procedure cm_ConfigToolbars(const Params: array of string);
   procedure cm_DebugShowCommandParameters(const Params: array of string);
   procedure cm_CopyPathOfFilesToClip(const Params: array of string);
   procedure cm_CopyPathNoSepOfFilesToClip(const Params: array of string);
   procedure cm_DoAnyCmCommand(const Params: array of string);
   procedure cm_SetAllTabsOptionNormal(const Params: array of string);
   procedure cm_SetAllTabsOptionPathLocked(const Params: array of string);
   procedure cm_SetAllTabsOptionPathResets(const Params: array of string);
   procedure cm_SetAllTabsOptionDirsInNewTab(const Params: array of string);
   procedure cm_ConfigFolderTabs(const {%H-}Params: array of string);
   procedure cm_ConfigFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_LoadFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_SaveFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_ReloadFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_PreviousFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_NextFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_ResaveFavoriteTabs(const {%H-}Params: array of string);
   procedure cm_CopyAllTabsToOpposite(const {%H-}Params: array of string);
   procedure cm_ConfigTreeViewMenus(const {%H-}Params: array of string);
   procedure cm_ConfigTreeViewMenusColors(const {%H-}Params: array of string);
   procedure cm_ConfigSaveSettings(const {%H-}Params: array of string);
   procedure cm_AddNewSearch(const Params: array of string);
   procedure cm_ViewSearches(const {%H-}Params: array of string);
   procedure cm_DeleteSearches(const {%H-}Params: array of string);
   procedure cm_ConfigSearches(const {%H-}Params: array of string);
   procedure cm_ConfigHotKeys(const {%H-}Params: array of string);
   procedure cm_ExecuteScript(const {%H-}Params: array of string);
   procedure cm_FocusSwap(const {%H-}Params: array of string);
   procedure cm_Benchmark(const {%H-}Params: array of string);

   // Internal commands
   procedure cm_ExecuteToolbarItem(const Params: array of string);
  end;

implementation

uses uFindFiles, Forms, Controls, Dialogs, Clipbrd, strutils, LCLProc, HelpIntfs, StringHashList,
     dmHelpManager, typinfo, fMain, fPackDlg, fMkDir, DCDateTimeUtils, KASToolBar, KASToolItems,
     fExtractDlg, fAbout, fOptions, fDiffer, fFindDlg, fSymLink, fHardLink, fMultiRename,
     fLinker, fSplitter, fDescrEdit, fCheckSumVerify, fCheckSumCalc, fSetFileProperties,
     uLng, uLog, uShowMsg, uOSForms, uOSUtils, uDCUtils, uBriefFileView,
     uShowForm, uShellExecute, uClipboard, uHash, uDisplayFile, uLuaPas,
     uFilePanelSelect, uFileSystemFileSource, uQuickViewPanel, Math,
     uOperationsManager, uFileSourceOperationTypes, uWfxPluginFileSource,
     uFileSystemDeleteOperation, uFileSourceExecuteOperation, uSearchResultFileSource,
     uFileSourceOperationMessageBoxesUI, uFileSourceCalcChecksumOperation,
     uFileSourceCalcStatisticsOperation, uFileSource, uFileSourceProperty,
     uVfsFileSource, uFileSourceUtil, uArchiveFileSourceUtil, uThumbFileView,
     uTempFileSystemFileSource, uFileProperty, uFileSourceSetFilePropertyOperation,
     uTrash, uFileSystemCopyOperation, fOptionsFileAssoc, fDeleteDlg,
     fViewOperations, uVfsModule, uMultiListFileSource, uExceptions, uFileProcs,
     DCOSUtils, DCStrUtils, DCBasicTypes, uFileSourceCopyOperation, fSyncDirsDlg,
     uHotDir, DCXmlConfig, dmCommonData, fOptionsFrame, foptionsDirectoryHotlist,
     fOptionsToolbar, fMainCommandsDlg, uConnectionManager, fOptionsTabs, fOptionsFavoriteTabs,
     fTreeViewMenu, fOptionsTreeViewMenu, fOptionsTreeViewMenuColor, uArchiveFileSource,
     fOptionsFileSearch, fOptionsHotKeys, fBenchmark
     {$IFDEF COLUMNSFILEVIEW_VTV}
     , uColumnsFileViewVtv
     {$ELSE}
     , uColumnsFileView
     {$ENDIF}
     ;

procedure ReadCopyRenameParams(
  const Params: array of string;
  var Confirmation: Boolean;
  out HasQueueId: Boolean;
  out QueueIdentifier: TOperationsManagerQueueIdentifier);
var
  Param, sQueueId: String;
  BoolValue: Boolean;
  iQueueId: Integer;
begin
  HasQueueId := False;
  for Param in Params do
  begin
    if GetParamBoolValue(Param, 'confirmation', BoolValue) then
      Confirmation := BoolValue
    else if GetParamValue(Param, 'queueid', sQueueId) then
    begin
      HasQueueId := TryStrToInt(sQueueId, iQueueId);
      if HasQueueId then
        QueueIdentifier := iQueueId;
    end;
  end;
end;
{ TMainCommands }

constructor TMainCommands.Create(TheOwner: TComponent; ActionList: TActionList = nil);
begin
  inherited Create(TheOwner);
  FCommands := TFormCommands.Create(Self, ActionList);
  FCommands.FilterFunc := @CommandsFilter;
end;

function TMainCommands.CommandsFilter(Command: String): Boolean;
begin
  Result := Command = 'cm_ExecuteToolbarItem';
end;

//------------------------------------------------------
procedure TMainCommands.TryGetParentDir(FileView: TFileView; var SelectedFiles: TFiles);
var
  activeFile : TFile;
  tempPath : String;
begin
  activeFile := FileView.CloneActiveFile;
  if assigned(activeFile) then begin
    if activeFile.Name = '..' then
      begin
        tempPath := activeFile.FullPath;
        activeFile.Name := ExtractFileName(ExcludeTrailingPathDelimiter(activeFile.Path));
        activeFile.Path := ExpandFileName(tempPath);
        SelectedFiles.Add(activeFile);
      end
      else
        FreeAndNil(activeFile);
    end;
end;

procedure TMainCommands.OnCopyOutStateChanged(Operation: TFileSourceOperation;
                                              State: TFileSourceOperationState);
var
  I: Integer;
  aFile: TFile;
  aFileList: TStringList;
  aFileSource: ITempFileSystemFileSource;
  aCopyOutOperation: TFileSourceCopyOperation;
  sCmd: string = '';
  sParams: string = '';
  sStartPath: string = '';
begin
  if (State = fsosStopped) and (Operation.Result = fsorFinished) then
  begin
    aFileList := TStringList.Create;
    try
      aCopyOutOperation := Operation as TFileSourceCopyOperation;
      aFileSource := aCopyOutOperation.TargetFileSource as ITempFileSystemFileSource;
      ChangeFileListRoot(aFileSource.FileSystemRoot, aCopyOutOperation.SourceFiles);

      try
        for I := 0 to aCopyOutOperation.SourceFiles.Count - 1 do
        begin
          aFile := aCopyOutOperation.SourceFiles[I];

          if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
          begin
            // Try to find 'view' command in internal associations
            if not gExts.GetExtActionCmd(aFile, 'view', sCmd, sParams, sStartPath) then
              aFileList.Add(aFile.FullPath)
            else
              begin
                if sStartPath='' then
                  sStartPath:=aCopyOutOperation.SourceFiles.Path;
                ProcessExtCommandFork(sCmd, sParams, aCopyOutOperation.SourceFiles.Path, aFile);
                // TODO:
                // If TempFileSource is used, create a wait thread that will
                // keep the TempFileSource alive until the command is finished.
              end;
          end; // if selected
        end; // for

        // if aFileList has files then view it
        if aFileList.Count > 0 then
          ShowViewerByGlobList(aFileList, aFileSource);
      except
        on e: EInvalidCommandLine do
          MessageDlg(rsToolErrorOpeningViewer,
            rsMsgInvalidCommandLine + ' (' + rsToolViewer + '):' + LineEnding + e.Message,
            mtError, [mbOK], 0);
      end;

    finally
      FreeAndNil(aFileList);
    end;
  end;
end;

procedure TMainCommands.OnEditCopyOutStateChanged(Operation: TFileSourceOperation;
                                                  State: TFileSourceOperationState);
var
  WaitData: TEditorWaitData;
begin
  if (State = fsosStopped) and (Operation.Result = fsorFinished) then
  begin
    try
      WaitData := TEditorWaitData.Create(Operation as TFileSourceCopyOperation);
      try
        ShowEditorByGlob(WaitData);
      except
        WaitData.Free;
      end;
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningEditor,
          rsMsgInvalidCommandLine + ' (' + rsToolEditor + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainCommands.OnCalcStatisticsStateChanged(Operation: TFileSourceOperation;
                                             State: TFileSourceOperationState);
var
  CalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
  CalcStatisticsOperationStatistics: TFileSourceCalcStatisticsOperationStatistics;
begin
  if (State = fsosStopped) and (Operation.Result = fsorFinished) then
  begin
    CalcStatisticsOperation := Operation as TFileSourceCalcStatisticsOperation;
    CalcStatisticsOperationStatistics := CalcStatisticsOperation.RetrieveStatistics;
    with CalcStatisticsOperationStatistics do
    begin
      msgOK(Format(rsSpaceMsg, [Files, Directories, cnvFormatFileSize(Size), Numb2USA(IntToStr(Size))]));
    end;
  end;
end;

procedure TMainCommands.OnCalcChecksumStateChanged(Operation: TFileSourceOperation;
                                           State: TFileSourceOperationState);
var
  CalcChecksumOperation: TFileSourceCalcChecksumOperation;
begin
  if (State = fsosStopped) and (Operation.Result = fsorFinished) then
  begin
    CalcChecksumOperation := Operation as TFileSourceCalcChecksumOperation;
    if CalcChecksumOperation.Mode = checksum_verify then
      ShowVerifyCheckSum(CalcChecksumOperation.Result);
  end;
end;

//------------------------------------------------------

procedure TMainCommands.DoCloseTab(Notebook: TFileViewNotebook; PageIndex: Integer);
begin
  with frmMain do
  begin
    RemovePage(Notebook, PageIndex);
    ActiveFrame.SetFocus;
  end;
end;

procedure TMainCommands.DoShowCmdLineHistory(bNextCmdLine: Boolean);
begin
  with frmMain do
  begin
    if edtCommand.Visible then
    begin
      if not (gCmdLine and frmMain.IsCommandLineVisible) then
      begin
        pnlCommand.Show;
        pnlCmdLine.Show;
      end;
      edtCommand.SetFocus;
      if bNextCmdLine then
      begin
        if edtCommand.ItemIndex > 0 then
          edtCommand.ItemIndex := edtCommand.ItemIndex - 1;
      end
      else
      begin
        if edtCommand.ItemIndex < edtCommand.Items.Count - 1 then
          edtCommand.ItemIndex := edtCommand.ItemIndex + 1;
      end;
    end;
  end;
end;

procedure TMainCommands.DoChangeDirToRoot(FileView: TFileView);
var
  Page: TFileViewPage;
begin
  with FileView do
  begin
    Page := TFileViewPage(NotebookPage);
    if Assigned(Page) then
    begin
      if Page.LockState = tlsPathResets then
        ChooseFileSource(FileView, Page.LockPath)
      else begin
        CurrentPath := FileSource.GetRootDir(CurrentPath);
      end;
    end;
  end;
end;

procedure TMainCommands.DoCopySelectedFileNamesToClipboard(FileView: TFileView; TypeOfCopy: TCopyFileNamesToClipboard);
var
  I: Integer;
  sl: TStringList = nil;
  SelectedFiles: TFiles = nil;
  PathToAdd, FileNameToAdd: String;
begin
  SelectedFiles := FileView.CloneSelectedOrActiveFiles;
  if (SelectedFiles.Count = 0) then
    TryGetParentDir(FileView, SelectedFiles);
  try
    if SelectedFiles.Count > 0 then
    begin
      sl := TStringList.Create;
      for I := 0 to SelectedFiles.Count - 1 do
      begin
        PathToAdd:='';
        FileNameToAdd:='';

        //Let's set the "PathToAdd" according to type of copy.
        case TypeOfCopy of
          cfntcPathAndFileNames, cfntcJustPathWithSeparator, cfntcPathWithoutSeparator:
            begin
              PathToAdd:= SelectedFiles[I].Path;

              // Workaround for not fully implemented TMultiListFileSource.
              if (Pos(FileView.CurrentAddress, PathToAdd) <> 1) and
                 (not FileView.FileSource.IsClass(TMultiListFileSource)) then
              begin
                PathToAdd := FileView.CurrentAddress + PathToAdd;
              end;

              if TypeOfCopy=cfntcPathWithoutSeparator then PathToAdd:=ExcludeTrailingPathDelimiter(PathToAdd);
            end;
        end;

        //Let's set the "FilenameToAdd" according to type of copy.
        case TypeOfCopy of
          cfntcPathAndFileNames, cfntcJustFileNames: FileNameToAdd:=SelectedFiles[I].Name;
        end;

        sl.Add(PathToAdd + FileNameToAdd);
      end;

      Clipboard.Clear;   // prevent multiple formats in Clipboard (specially synedit)
      ClipboardSetText(TrimRightLineEnding(sl.Text, sl.TextLineBreakStyle));
    end;

  finally
    if Assigned(sl) then
      FreeAndNil(sl);
    if Assigned(SelectedFiles) then
      FreeAndNil(SelectedFiles);
  end;
end;

procedure TMainCommands.DoNewTab(Notebook: TFileViewNotebook);
var
  NewPage: TFileViewPage;
begin
  NewPage := Notebook.NewPage(Notebook.ActiveView);
  NewPage.MakeActive;
end;

procedure TMainCommands.DoRenameTab(Page: TFileViewPage);
var
  sCaption: String;
begin
  sCaption := Page.CurrentTitle;
  if InputQuery(rsMsgTabRenameCaption, rsMsgTabRenamePrompt, sCaption) then
    Page.PermanentTitle := sCaption;
end;

procedure TMainCommands.DoOpenVirtualFileSystemList(Panel: TFileView);
var
  FileSource: IFileSource;
begin
  FileSource:= TVfsFileSource.Create(gWFXPlugins);
  if Assigned(FileSource) then
    Panel.AddFileSource(FileSource, FileSource.GetRootDir);
end;

procedure TMainCommands.DoPanelsSplitterPerPos(SplitPos: Integer);
begin
  with frmMain do
  begin
    if (SplitPos >= 0) and (SplitPos <= 100) then
    begin
      // Update splitter position
      MainSplitterPos:= SplitPos;
      pnlNotebooksResize(pnlNotebooks);
    end;
  end;
end;

procedure TMainCommands.DoUpdateFileView(AFileView: TFileView; UserData: Pointer);
begin
  AFileView.UpdateView;
end;

procedure TMainCommands.DoContextMenu(Panel: TFileView; X, Y: Integer; Background: Boolean; UserWishForContextMenu:TUserWishForContextMenu);
var
  aFile: TFile = nil;
  aFiles: TFiles = nil;
  sPath, sName: String;
begin
  with frmMain do
  begin
    if not (fspDirectAccess in Panel.FileSource.Properties) then
    begin
      if not Background then pmContextMenu.PopUp(X, Y);
      Exit;
    end;

    if not Panel.HasSelectedFiles then
    begin
      aFile:= Panel.CloneActiveFile;
      if not Assigned(aFile) then
        Background:= True
      else begin
        sName:= aFile.Name;
        FreeAndNil(aFile);
      end;
    end;

    if (Background = True) or (sName = '..') then
      begin
        sName:= ExcludeTrailingPathDelimiter(Panel.CurrentPath);
        sPath:= ExtractFilePath(sName);
        aFiles:= TFiles.Create(sPath);
        aFile:= Panel.FileSource.CreateFileObject(sPath);
        aFile.Attributes:= faFolder;
        aFile.Name:= ExtractFileName(sName);
        aFiles.Add(aFile);
      end
    else
      begin
        aFiles:= Panel.CloneSelectedOrActiveFiles;
      end;

    if Assigned(aFiles) then
    try
      if aFiles.Count > 0 then
      try
        ShowContextMenu(frmMain, aFiles, X, Y, Background, nil, UserWishForContextMenu);
      except
        on e: EContextMenuException do
          ShowException(e);
      end;

    finally
      if Assigned(aFiles) then
        FreeAndNil(aFiles);
    end;
  end;
end;

procedure TMainCommands.DoTransferPath(SourceFrame: TFileView;
  TargetNotebook: TFileViewNotebook);
begin
  if TargetNotebook.ActivePage.LockState = tlsPathLocked then
    Exit;
  if TargetNotebook.ActivePage.LockState = tlsDirsInNewTab then
  begin
    TargetNotebook.NewPage(SourceFrame).MakeActive;
    TargetNotebook.ActivePage.LockState := tlsNormal;
  end
  else
  begin
    TargetNotebook.ActivePage.FileView := nil;
    SourceFrame.Clone(TargetNotebook.ActivePage);
  end;
end;

procedure TMainCommands.DoTransferPath(SourcePage: TFileViewPage; TargetPage: TFileViewPage; FromActivePanel: Boolean);
var
  aFile: TFile;
  NewPath: String;
begin
  if FromActivePanel then
  begin
    aFile := SourcePage.FileView.CloneActiveFile;
    if Assigned(aFile) then
    try
      if (fspLinksToLocalFiles in SourcePage.FileView.FileSource.GetProperties) and
         (SourcePage.FileView.FileSource.GetLocalName(aFile)) then
        begin
          if aFile.IsDirectory then
            ChooseFileSource(TargetPage.FileView, aFile.FullPath)
          else if not ChooseFileSource(TargetPage.FileView, TargetPage.FileView.FileSource, aFile) then
            begin
              ChooseFileSource(TargetPage.FileView, aFile.Path);
              TargetPage.FileView.SetActiveFile(aFile.Name);
            end;
        end
      else if aFile.IsDirectory then
      begin
        if aFile.Name = '..' then
        begin
          NewPath := GetParentDir(SourcePage.FileView.CurrentPath);
        end
        else
        begin
          // Change to a subdirectory.
          NewPath := aFile.FullPath;
        end;

        if NewPath <> EmptyStr then
          TargetPage.FileView.AddFileSource(SourcePage.FileView.FileSource, NewPath);
      end
      else
      begin
        // Change file source, if the file under cursor can be opened as another file source.
        try
          if not ChooseFileSource(TargetPage.FileView, SourcePage.FileView.FileSource, aFile) then
            TargetPage.FileView.AddFileSource(SourcePage.FileView.FileSource,
                                              SourcePage.FileView.CurrentPath);
          TargetPage.FileView.SetActiveFile(aFile.Name);
        except
          on e: EFileSourceException do
            MessageDlg('Error', e.Message, mtError, [mbOK], 0);
        end;
      end;
    finally
      FreeAndNil(aFile);
    end;
  end
  else
  begin
    TargetPage.FileView.AddFileSource(SourcePage.FileView.FileSource, SourcePage.FileView.CurrentPath);
  end;
end;

procedure TMainCommands.DoSortByFunctions(View: TFileView; FileFunctions: TFileFunctions);
var
  NewSorting: TFileSortings = nil;
  CurrentSorting: TFileSortings;
  SortDirection: TSortDirection = sdNone;
  i: Integer;
begin
  if Length(FileFunctions) = 0 then
    Exit;

  CurrentSorting := View.Sorting;

  // Check if there is already sorting by one of the functions.
  // If it is then reverse direction of sorting.
  for i := 0 to Length(FileFunctions) - 1 do
  begin
    SortDirection := GetSortDirection(CurrentSorting, FileFunctions[i]);
    if SortDirection <> sdNone then
    begin
      SortDirection := ReverseSortDirection(SortDirection);
      Break;
    end;
  end;

  //If there is no direction currently, sort "sdDescending" for size and date
  if SortDirection = sdNone then
  begin
    case FileFunctions[0] of
      fsfSize, fsfModificationTime, fsfCreationTime, fsfLastAccessTime: SortDirection:=sdDescending;
      else SortDirection:=sdAscending;
    end;
  end;

  SetLength(NewSorting, 1);
  SetLength(NewSorting[0].SortFunctions, 1);
  NewSorting[0].SortFunctions[0] := FileFunctions[0]; // Sort by single function.
  NewSorting[0].SortDirection := SortDirection;

  View.Sorting := NewSorting;
end;

procedure TMainCommands.DoShowMainMenu(bShow: Boolean);
begin
  gMainMenu := bShow;

  with frmMain do
  begin
    if bShow then
    begin
      Menu := mnuMain;
    end
    else if Assigned(Menu) then
    begin
      Menu := nil;
      {$IFDEF MSWINDOWS}
      // Workaround: on Windows need to recreate window to properly recalculate children sizes.
      RecreateWnd(frmMain);
      {$ENDIF}
    end;
  end;
end;

//------------------------------------------------------
//Published methods
//------------------------------------------------------

{ TMainCommands.DoActualAddToCommandLine }
procedure TMainCommands.DoActualAddToCommandLine(const Params: array of string; sAddedString:string; bAddSpaceAtTheEnd:boolean);
type
  tQuoteMode = (tqmSmartQuote,tqmForceQuote,tqmNeverQuote);
var
  OldPosition: Integer;
  sParamValue: String;
  QuoteMode: tQuoteMode = tqmSmartQuote;
  DefaultButton: TMyMsgButton;
  Answer: TMyMsgResult;
begin
  if Length(Params)>0 then
  begin
    if GetParamValue(Params[0], 'mode', sParamValue) then
    begin
      if sParamValue='smartquote' then QuoteMode:=tqmSmartQuote else
        if sParamValue='forcequote' then QuoteMode:=tqmForceQuote else
          if sParamValue='neverquote' then QuoteMode:=tqmNeverQuote else
            if sParamValue='prompt' then
            begin
              if sAddedString = QuoteFilenameIfNecessary(sAddedString) then DefaultButton:=msmbNo else DefaultButton:=msmbYes;
              Answer:=MsgBox(rsMsgAskQuoteOrNot,[msmbYes, msmbNo], DefaultButton, DefaultButton);
              case Answer of
                mmrYes:QuoteMode:=tqmForceQuote;
                mmrNo:QuoteMode:=tqmNeverQuote;
              end;
            end;
    end;
  end;

  case QuoteMode of
    tqmSmartQuote : sAddedString := QuoteFilenameIfNecessary(sAddedString);
    tqmForceQuote : sAddedString := QuoteStr(sAddedString);
    tqmNeverQuote : sAddedString := sAddedString;
    else sAddedString := QuoteFilenameIfNecessary(sAddedString);
  end;

  if bAddSpaceAtTheEnd then sAddedString:=sAddedString+' ';

  OldPosition := frmMain.edtCommand.SelStart;
  frmMain.edtCommand.Text := frmMain.edtCommand.Text + sAddedString;
  frmMain.edtCommand.SelStart := OldPosition + Length(sAddedString);
end;

{ TMainCommands.cm_AddPathToCmdLine }
procedure TMainCommands.cm_AddPathToCmdLine(const Params: array of string);
begin
  DoActualAddToCommandLine(Params, frmMain.ActiveFrame.CurrentPath, False);
end;

{ TMainCommands.cm_AddFilenameToCmdLine }
procedure TMainCommands.cm_AddFilenameToCmdLine(const Params: array of string);
var
  aFile: TFile;
begin
  aFile := frmMain.ActiveFrame.CloneActiveFile;
  if Assigned(aFile) then
  try
    DoActualAddToCommandLine(Params, aFile.Name, True);
  finally
    FreeAndNil(aFile);
  end;
end;

{ TMainCommands.cm_AddPathAndFilenameToCmdLine }
procedure TMainCommands.cm_AddPathAndFilenameToCmdLine(const Params: array of string);
var
  aFile: TFile;
begin
  aFile := frmMain.ActiveFrame.CloneActiveFile;
  if Assigned(aFile) then
  try
    if aFile.Name = '..' then
      DoActualAddToCommandLine(Params, frmMain.ActiveFrame.CurrentPath, True)
    else
      DoActualAddToCommandLine(Params, aFile.FullPath, True);
  finally
    FreeAndNil(aFile);
  end;
end;

procedure TMainCommands.cm_ContextMenu(const Params: array of string);
begin
  // Let file view handle displaying context menu at appropriate position.
  frmMain.ActiveFrame.ExecuteCommand('cm_ContextMenu', Params);
end;

procedure TMainCommands.cm_CopyFullNamesToClip(const Params: array of string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, cfntcPathAndFileNames);
end;

procedure TMainCommands.cm_CopyFileDetailsToClip(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CopyFileDetailsToClip', []);
end;

procedure TMainCommands.cm_CopyNamesToClip(const Params: array of string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, cfntcJustFileNames);
end;

//------------------------------------------------------
procedure TMainCommands.cm_Exchange(const Params: array of string);
var
  ActiveView, NotActiveView: TFileView;
begin
  ActiveView:= frmMain.ActiveFrame;
  NotActiveView:= frmMain.NotActiveFrame;
  with frmMain do
  begin
    ActiveNotebook.ActivePage.RemoveComponent(ActiveView);
    NotActiveNotebook.ActivePage.RemoveComponent(NotActiveView);

    ActiveNotebook.ActivePage.FileView:= NotActiveView;
    NotActiveNotebook.ActivePage.FileView:= ActiveView;

    ActiveNotebook.ActivePage.InsertComponent(NotActiveView);
    NotActiveNotebook.ActivePage.InsertComponent(ActiveView);
  end;
  NotActiveView.SetFocus;
end;

procedure TMainCommands.cm_ExecuteToolbarItem(const Params: array of string);
var
  ToolItemID: String;
begin
  if GetParamValue(Params, 'ToolItemID', ToolItemID) then
    frmMain.MainToolBar.ClickItem(ToolItemID);
end;

procedure TMainCommands.cm_FlatView(const Params: array of string);
var
  AFile: TFile;
begin
  with frmMain do
  if not (fspListFlatView in ActiveFrame.FileSource.GetProperties) then
  begin
    msgWarning(rsMsgErrNotSupported);
  end
  else
  begin
    ActiveFrame.FlatView:= not ActiveFrame.FlatView;
    if not ActiveFrame.FlatView then
    begin
      AFile:= ActiveFrame.CloneActiveFile;
      if Assigned(AFile) and AFile.IsNameValid then
      begin
        if not mbCompareFileNames(ActiveFrame.CurrentPath, AFile.Path) then
        begin
          ActiveFrame.CurrentPath:= AFile.Path;
          ActiveFrame.SetActiveFile(AFile.Name);
        end;
      end;
      AFile.Free;
    end;
    ActiveFrame.Reload;
  end;
end;

procedure TMainCommands.cm_LeftFlatView(const Params: array of string);
begin
  if not (fspListFlatView in frmMain.FrameLeft.FileSource.GetProperties) then
  begin
    msgWarning(rsMsgErrNotSupported);
  end
  else
  begin
    frmMain.FrameLeft.FlatView:= not frmMain.FrameLeft.FlatView;
    frmMain.FrameLeft.Reload;
  end;
end;

procedure TMainCommands.cm_RightFlatView(const Params: array of string);
begin
  if not (fspListFlatView in frmMain.FrameRight.FileSource.GetProperties) then
  begin
    msgWarning(rsMsgErrNotSupported);
  end
  else
  begin
    frmMain.FrameRight.FlatView:= not frmMain.FrameRight.FlatView;
    frmMain.FrameRight.Reload;
  end;
end;

procedure TMainCommands.cm_OpenDirInNewTab(const Params: array of string);

  function OpenTab(const aFullPath: string): TFileViewPage;
  begin
    Result := FrmMain.ActiveNotebook.NewPage(FrmMain.ActiveFrame);
    // Workaround for Search Result File Source
    if Result.FileView.FileSource is TSearchResultFileSource then
      SetFileSystemPath(Result.FileView, aFullPath)
    else
      Result.FileView.CurrentPath := aFullPath;
  end;

  function OpenArchive(const aFile: TFile): TFileViewPage;
  begin
    Result := FrmMain.ActiveNotebook.NewPage(FrmMain.ActiveFrame);
    ChooseArchive(Result.FileView, Result.FileView.FileSource, aFile);
  end;

  function OpenParent: TFileViewPage;
  begin
    Result := FrmMain.ActiveNotebook.NewPage(FrmMain.ActiveFrame);
    Result.FileView.ChangePathToParent(True);
  end;

var
  aFile: TFile;
  NewPage: TFileViewPage;
begin
  aFile := FrmMain.ActiveFrame.CloneActiveFile;
  if not Assigned(aFile) then
    NewPage := OpenTab(FrmMain.ActiveFrame.CurrentPath)
  else try
    if not aFile.IsNameValid then
      NewPage := OpenParent
    else if (aFile.IsDirectory or aFile.IsLinkToDirectory) then
      NewPage := OpenTab(aFile.FullPath)
    else if FileIsArchive(aFile.FullPath) then
      NewPage := OpenArchive(aFile)
    else
      NewPage := OpenTab(aFile.Path);
  finally
    FreeAndNil(aFile);
  end;

  if tb_open_new_in_foreground in gDirTabOptions then
    NewPage.MakeActive;
end;

procedure TMainCommands.cm_TargetEqualSource(const Params: array of string);
begin
  with frmMain do
  begin
    DoTransferPath(ActiveFrame, NotActiveNotebook);
  end;
end;

procedure TMainCommands.cm_LeftEqualRight(const Params: array of string);
begin
  with frmMain do
  begin
    DoTransferPath(FrameRight, LeftTabs);

    // Destroying active view may have caused losing focus. Restore it if needed.
    if SelectedPanel = fpLeft then
      FrameLeft.SetFocus;
  end;
end;

procedure TMainCommands.cm_RightEqualLeft(const Params: array of string);
begin
  with frmMain do
  begin
    DoTransferPath(FrameLeft, RightTabs);

    // Destroying active view may have caused losing focus. Restore it if needed.
    if SelectedPanel = fpRight then
      FrameRight.SetFocus;
  end;
end;

procedure TMainCommands.cm_OpenArchive(const Params: array of string);
var
  aFile: TFile;
begin
  with frmMain.ActiveFrame do
  begin
    aFile := CloneActiveFile;
    if Assigned(aFile) then
    try
      if aFile.IsNameValid then
      begin
        if aFile.IsDirectory or aFile.IsLinkToDirectory then
          ChangePathToChild(aFile)
        else
          ChooseArchive(frmMain.ActiveFrame, FileSource, aFile, True);
      end;
    finally
      FreeAndNil(aFile);
    end;
  end;
end;

procedure TMainCommands.cm_TestArchive(const Params: array of string);
var
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      TestArchive(ActiveFrame, SelectedFiles);
    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_Open(const Params: array of string);
begin
  frmMain.ActiveFrame.OpenActiveFile;
end;

procedure TMainCommands.cm_ShellExecute(const Params: array of string);
var
  aFile: TFile;
begin
  if Length(Params) > 0 then
    with frmMain do
    ShellExecute(PrepareParameter(Params[0]))
  else
    with frmMain.ActiveFrame do
    begin
      aFile := CloneActiveFile;
      if Assigned(aFile) then
      try
        if aFile.IsNameValid then
        begin
          ShellExecute(aFile.FullPath);
        end;
      finally
        FreeAndNil(aFile);
      end;
  end;
end;

procedure TMainCommands.cm_OpenVirtualFileSystemList(const Params: array of string);
begin
  DoOpenVirtualFileSystemList(frmMain.ActiveFrame);
end;

//------------------------------------------------------
(* Pack files in archive by creating a new archive *)
procedure TMainCommands.cm_PackFiles(const Params: array of string);
var
  Param: String;
  TargetPath: String;
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      if SelectedFiles.Count = 0 then
        msgWarning(rsMsgNoFilesSelected)
      else begin
        Param := GetDefaultParam(Params);
        if Param = 'PackHere' then
          TargetPath:= ActiveFrame.CurrentPath
        else begin
          TargetPath:= NotActiveFrame.CurrentPath;
        end;
        ShowPackDlg(frmMain,
                    ActiveFrame.FileSource,
                    nil, // No specific target (create new)
                    SelectedFiles,
                    TargetPath,
                    PathDelim { Copy to root of archive } {NotActiveFrame.FileSource.GetRootString}
                   );
      end;
    finally
      FreeAndNil(SelectedFiles);
    end;
  end;
end;

// This command is needed for extracting whole archive by Alt+F9 (without opening it).
procedure TMainCommands.cm_ExtractFiles(const Params: array of string);
var
  Param: String;
  TargetPath: String;
  SelectedFiles: TFiles;
  TargetFileSource: IFileSource;
begin
  with frmMain do
  begin
    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    if Assigned(SelectedFiles) then
    try
      if SelectedFiles.Count = 0 then
        msgWarning(rsMsgNoFilesSelected)
      else begin
        Param := GetDefaultParam(Params);
        if Param = 'ExtractHere' then
        begin
          TargetPath:= ActiveFrame.CurrentPath;
          TargetFileSource:= ActiveFrame.FileSource;
        end
        else begin
          TargetPath:= NotActiveFrame.CurrentPath;
          TargetFileSource:= NotActiveFrame.FileSource;
        end;
        ShowExtractDlg(frmMain, ActiveFrame.FileSource, SelectedFiles,
                       TargetFileSource, TargetPath);
      end;
    finally
      FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_QuickSearch(const Params: array of string);
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickSearch', Params);
end;

procedure TMainCommands.cm_QuickFilter(const Params: array of string);
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickFilter', Params);
end;

procedure TMainCommands.cm_SrcOpenDrives(const Params: array of string);
begin
  frmMain.ShowDrivesList(frmMain.SelectedPanel);
end;

procedure TMainCommands.cm_LeftOpenDrives(const Params: array of string);
begin
  frmMain.ShowDrivesList(fpLeft);
end;

procedure TMainCommands.cm_RightOpenDrives(const Params: array of string);
begin
  frmMain.ShowDrivesList(fpRight);
end;

procedure TMainCommands.cm_OpenBar(const Params: array of string);
begin
  // Deprecated.
end;

{ TMainCommands.DoComputeSizeAndPosForWindowInMiddle }
procedure TMainCommands.DoComputeSizeAndPosForWindowInMiddle(var iPosX:integer; var iPosY:integer; var iWidth:integer; var iHeight:integer);
var
  pl,pr: TPoint;
begin
  pl := frmMain.FrameLeft.ClientToScreen(Classes.Point(0,0));
  pr := frmMain.FrameRight.ClientToScreen(Classes.Point(0,0));
  iWidth := (((pr.x+frmMain.FrameRight.Width)- pl.x) * 68) div 100;
  iHeight := frmMain.FrameLeft.Height;
  iPosX := pl.x + (((frmMain.FrameLeft.Width+frmMain.FrameRight.Width) - iWidth) div 2);
  iPosY := pl.y;
end;

{ TMainCommands.cm_ShowButtonMenu }
procedure TMainCommands.cm_ShowButtonMenu(const Params: array of string);
var
  WantedButtonMenu, BoolValue: boolean;
  bWantedTreeViewButtonMenu : boolean = False;
  Param : string;
  iWantedPosX: integer = 0;
  iWantedPosY: integer = 0;
  iWantedWidth: integer = 800;
  iWantedHeight: integer = 600;
  APointer: Pointer;
  iTypeDispatcher: integer = 0;
  maybeKASToolButton: TKASToolButton;
  maybeKASToolItem: TKASToolItem;
begin
  WantedButtonMenu := gButtonBar;
  if Length(Params) > 0 then
  begin
    for Param in Params do
      if GetParamBoolValue(Param, 'toolbar', BoolValue) then WantedButtonMenu := BoolValue
      else if GetParamBoolValue(Param, 'treeview', BoolValue) then bWantedTreeViewButtonMenu := BoolValue
      else WantedButtonMenu := not WantedButtonMenu;
  end
  else
  begin
    WantedButtonMenu := not WantedButtonMenu;
  end;
  if not bWantedTreeViewButtonMenu then
  begin
    if WantedButtonMenu <> gButtonBar then
    begin
      gButtonBar := WantedButtonMenu;
      frmMain.UpdateWindowView;
    end;
  end
  else
  begin
    DoComputeSizeAndPosForWindowInMiddle(iWantedPosX, iWantedPosY, iWantedWidth, iWantedHeight);
    APointer := GetUserChoiceFromKASToolBar(frmMain.MainToolBar, tvmcKASToolBar, iWantedPosX, iWantedPosY, iWantedWidth, iWantedHeight, iTypeDispatcher);
    if APointer<>nil then
    begin
      case iTypeDispatcher of
        1:
        begin
          maybeKASToolButton := TKASToolButton(APointer);
          maybeKASToolButton.OnClick(maybeKASToolButton);
        end;
        2:
        begin
          maybeKASToolItem := TKASToolItem(APointer);
          frmMain.MainToolBar.PublicExecuteToolItem(maybeKASToolItem);
        end;
      end;
    end;
  end;
end;

procedure TMainCommands.cm_TransferLeft(const Params: array of string);
begin
  DoTransferPath(frmMain.RightTabs.ActivePage,
                 frmMain.LeftTabs.ActivePage,
                 frmMain.SelectedPanel = fpRight);
end;

procedure TMainCommands.cm_TransferRight(const Params: array of string);
begin
  DoTransferPath(frmMain.LeftTabs.ActivePage,
                 frmMain.RightTabs.ActivePage,
                 frmMain.SelectedPanel = fpLeft);
end;

procedure TMainCommands.cm_GoToFirstFile(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_GoToFirstFile', []);
end;

procedure TMainCommands.cm_GoToLastFile(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_GoToLastFile', []);
end;

procedure TMainCommands.cm_Minimize(const Params: array of string);
begin
  FrmMain.MinimizeWindow;
end;

procedure TMainCommands.cm_Wipe(const Params: array of string);
var
  Message: String;
  theFilesToWipe: TFiles;
  Operation: TFileSourceOperation;
  QueueId: TOperationsManagerQueueIdentifier;
begin
  with frmMain.ActiveFrame do
  begin
    if not (fsoWipe in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    // ------------------------------------------------------

    theFilesToWipe := CloneSelectedOrActiveFiles; // free at Thread end by thread

    if Assigned(theFilesToWipe) then
    try
      if theFilesToWipe.Count = 0 then
        Exit;

      Message:= frmMain.GetFileDlgStr(rsMsgWipeSel, rsMsgWipeFlDr, theFilesToWipe);
      if not ShowDeleteDialog(Message, FileSource, QueueId) then
        Exit;

      Operation := FileSource.CreateWipeOperation(theFilesToWipe);

      if Assigned(Operation) then
      begin
        // Start operation.
        OperationsManager.AddOperation(Operation, QueueId, False);
      end
      else
      begin
        msgWarning(rsMsgNotImplemented);
      end;

    finally
      if Assigned(theFilesToWipe) then
        FreeAndNil(theFilesToWipe);
    end;
  end;
end;

procedure TMainCommands.cm_Exit(const Params: array of string);
begin
  frmMain.Close; // application.Terminate not save settings.
end;

procedure TMainCommands.cm_NewTab(const Params: array of string);
begin
  DoNewTab(frmMain.ActiveNotebook);
end;

procedure TMainCommands.cm_RenameTab(const Params: array of string);
begin
  DoRenameTab(frmMain.ActiveNotebook.ActivePage);
end;

procedure TMainCommands.cm_CloseTab(const Params: array of string);
begin
  with frmMain do
    DoCloseTab(ActiveNotebook, ActiveNotebook.PageIndex);
end;

{ TMainCommands.cm_CloseAllTabs }
procedure TMainCommands.cm_CloseAllTabs(const Params: array of string);
begin
  with frmMain do
  begin
    if (tb_confirm_close_all in gDirTabOptions) then
      if not msgYesNo(rsMsgCloseAllInActiveTabs) then Exit;

    DoActionOnMultipleTabs(Params,@DoCloseAllTabs);
  end;
end;

{ TMainCommands.cm_CloseDuplicateTabs }
procedure TMainCommands.cm_CloseDuplicateTabs(const Params: array of string);
begin
  DoActionOnMultipleTabs(Params,@DoCloseDuplicateTabs);
end;



procedure TMainCommands.cm_NextTab(const Params: array of string);
begin
  frmMain.ActiveNotebook.ActivateNextTab;
end;

procedure TMainCommands.cm_PrevTab(const Params: array of string);
begin
  frmMain.ActiveNotebook.ActivatePrevTab;
end;

{ TMainCommands.cm_SaveTabs }
// To respect legacy, we can invoke "cm_SaveTabs" with a single parameter and it will be a "DefaultParam", which means without any equal sign, directly the filename.
// With the following code, we may have more descriptive parameters like the following:
//         filename= : The giving parameter will be the output filename to save the tabs. If no "filename=" is specified, we will prompt user.
//   savedirhistory= : We indicate if we want to save dir history or not.
procedure TMainCommands.cm_SaveTabs(const Params: array of string);
var
  Config: TXmlConfig;
  Param, sValue: string;
  boolValue: boolean;
  bSaveDirHistory: boolean;
  sOutputTabsFilename: string = '';
begin
  // 1. We setup our default options.
  bSaveDirHistory := gSaveDirHistory;

  // 2. Let's parse the parameter to get the wanted ones. The default wanted parameter have been set in the "VAR" section
  //    We need to respect legacy of this command where *before* it was possible to simply and directly have the wanted output filename.
  //    Let's assume that if we have an "=" sign, it's can be a legacy usage but one with actual parameters.
  if (length(Params)>0) then
  begin
    sOutputTabsFilename := GetDefaultParam(Params);
    if pos('=',sOutputTabsFilename)<>0 then
    begin
      sOutputTabsFilename := '';
      for Param in Params do
      begin
        if GetParamValue(Param, 'filename', sValue) then sOutputTabsFilename := sValue
        else if GetParamBoolValue(Param, 'savedirhistory', boolValue) then bSaveDirHistory := boolValue;
      end;
    end;
  end;

  // 3. If no output filename has been specified so far, let's request an output filename.
  if sOutputTabsFilename='' then
  begin
    dmComData.SaveDialog.DefaultExt := 'tab';
    dmComData.SaveDialog.Filter := '*.tab|*.tab';
    if dmComData.SaveDialog.Execute then sOutputTabsFilename := dmComData.SaveDialog.FileName;
  end;

  // 4. If we get here with "sOutputTabsFilename<>''", we know what to save and where to save it.
  if sOutputTabsFilename<>'' then
  begin
    try
      Config := TXmlConfig.Create(sOutputTabsFilename);
      try
        frmMain.SaveTabsXml(Config, 'Tabs/OpenedTabs/', frmMain.LeftTabs, bSaveDirHistory);
        frmMain.SaveTabsXml(Config, 'Tabs/OpenedTabs/', frmMain.RightTabs, bSaveDirHistory);
        Config.Save;
      finally
        Config.Free;
      end;

    except
      on E: Exception do msgError(E.Message);
    end;
  end;
end;

{ TMainCommands.cm_LoadTabs }
// To respect legacy, invoking "cm_LoadTabs" with no parameter will attempt to load tabs for both panels and prompt the user for a filename.
// Still to respect lefacy, we can invoke "cm_LoadTabs" with a single parameter and it will be a "DefaultParam", which means without any equal sign, directly the filename.
// With the following code, we may have more descriptive parameters like the following:
//   filename   = The giving parameter will be the input filename to load the tabs from. If no "filename=" is specified, we will prompt user.
//   loadleftto = Indicate where to load what was saved for left panel. It could be left to be like before but also now right, active, inactive, both and none.
//   loadrightto= Indicate where to load what was saved for right panel. It could be right to be like before but also now left, active, inactive, both and none.
//   keep       = This indicates if in the target notebook where tabs will be loaded if we remove first the target present or not. When keep is "false", which is the default, we flush them first. If "keep" is "true", we add the loaded tab to the existing ones.
procedure TMainCommands.cm_LoadTabs(const Params: array of string);
var
  originalFilePanel:TFilePanelSelect;
  sInputTabsFilename: string = '';
  param, sValue: string;
  Config: TXmlConfig;
  TargetDestinationForLeft : TTabsConfigLocation = tclLeft;
  TargetDestinationForRight : TTabsConfigLocation = tclRight;
  DestinationToKeep : TTabsConfigLocation = tclNone;
  TabsAlreadyDestroyedFlags: TTabsFlagsAlreadyDestroyed = [];

  function EvaluateSideResult(sParamValue:string; DefaultValue:TTabsConfigLocation):TTabsConfigLocation;
  begin
    result:=DefaultValue;
    if sParamValue='left' then result := tclLeft else
      if sParamValue='right' then result := tclRight else
        if sParamValue='active' then result := tclActive else
          if sParamValue='inactive' then result := tclInactive else
            if sParamValue='both' then result := tclBoth else
              if sParamValue='none' then result := tclNone;
  end;
begin
  // 1. Note that most variable have been set with their default value in declaration.
  originalFilePanel := frmMain.SelectedPanel;

  // 2. Let's parse the parameter to get the wanted ones
  //    We need to respect legacy of this command where *before* it was possible to simply and directly have the wanted input filename.
  //    Let's assume that if we have an "=" sign, it's can't be a legacy usage but one with actual parameters.
  if (length(Params)>0) then
  begin
    sInputTabsFilename:=GetDefaultParam(Params);
    if pos('=',sInputTabsFilename)<>0 then
    begin
      sInputTabsFilename:='';
      for Param in Params do
      begin
        if GetParamValue(Param, 'filename', sValue) then sInputTabsFilename := sValue
        else if GetParamValue(Param, 'loadleftto', sValue) then TargetDestinationForLeft:=EvaluateSideResult(sValue,TargetDestinationForLeft)
        else if GetParamValue(Param, 'loadrightto', sValue) then TargetDestinationForRight:=EvaluateSideResult(sValue,TargetDestinationForRight)
        else if GetParamValue(Param, 'keep', sValue) then DestinationToKeep:=EvaluateSideResult(sValue,DestinationToKeep);
      end;
    end;
  end;

  // 3. If variable "sInputTabsFilename", we''ll request the user to provide an input filename.
  if sInputTabsFilename='' then
  begin
    dmComData.OpenDialog.Filter:= '*.tab|*.tab';
    dmComData.OpenDialog.FileName:= GetDefaultParam(Params);
    if dmComData.OpenDialog.Execute then
      sInputTabsFilename:=dmComData.OpenDialog.FileName;
  end;

  // 4. If we get here with "sInputTabsFilename<>''", we know what to load and from what to load it!
  if sInputTabsFilename<>'' then
  begin
    gFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange;
    try
      Config := TXmlConfig.Create(sInputTabsFilename, True);
      try
        frmMain.LoadTheseTabsWithThisConfig(Config, 'Tabs/OpenedTabs/', tclLeft, TargetDestinationForLeft, DestinationToKeep, TabsAlreadyDestroyedFlags);
        frmMain.LoadTheseTabsWithThisConfig(Config, 'Tabs/OpenedTabs/', tclRight, TargetDestinationForRight, DestinationToKeep, TabsAlreadyDestroyedFlags);
      finally
        Config.Free;
      end;
    except
      on E: Exception do msgError(E.Message);
    end;
  end;

  frmMain.SelectedPanel := originalFilePanel;
  frmMain.ActiveFrame.SetFocus;
end;

procedure TMainCommands.cm_SetTabOptionNormal(const Params: array of string);
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsNormal;
end;

procedure TMainCommands.cm_SetTabOptionPathLocked(const Params: array of string);
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsPathLocked;
end;

procedure TMainCommands.cm_SetTabOptionPathResets(const Params: array of string);
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsPathResets;
end;

procedure TMainCommands.cm_SetTabOptionDirsInNewTab(const Params: array of string);
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsDirsInNewTab;
end;

//------------------------------------------------------

procedure TMainCommands.cm_View(const Params: array of string);
var
  aFile: TFile;
  i, n: Integer;
  sl: TStringList = nil;
  ActiveFile: TFile = nil;
  AllFiles: TFiles = nil;
  SelectedFiles: TFiles = nil;
  aFileSource: IFileSource;
  sCmd: string = '';
  sParams: string = '';
  sStartPath: string = '';
  LinksResolveNeeded: Boolean;
begin
  with frmMain do
  try
    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    ActiveFile := ActiveFrame.CloneActiveFile;

    // Enter directories using View command.
    if Assigned(ActiveFile) and
       (ActiveFile.IsDirectory or ActiveFile.IsLinkToDirectory) then
    begin
      ActiveFrame.ExecuteCommand('cm_Open', []);
      Exit;
    end;

    if SelectedFiles.Count = 0 then
    begin
      msgWarning(rsMsgNoFilesSelected);
      Exit;
    end;

    // Default to using the file source directly.
    aFileSource := ActiveFrame.FileSource;

    if PrepareData(ActiveFrame.FileSource, SelectedFiles, @OnCopyOutStateChanged) <> pdrSynchronous then
      Exit;

    try
      aFile := SelectedFiles[0];
      // Try to find 'view' command in internal associations
      if gExts.GetExtActionCmd(aFile, 'view', sCmd, sParams, sStartPath) then
      begin
        ProcessExtCommandFork(sCmd, sParams, ActiveFrame.CurrentPath);
        Exit;
      end;

      sl := TStringList.Create;
      for i := 0 to SelectedFiles.Count - 1 do
      begin
        aFile := SelectedFiles[i];
        if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
        begin
          sl.Add(aFile.FullPath)
        end;
      end; // for

      // If only one file was selected then add all files in panel to the list.
      // Works only for directly accessible files and only when using internal viewer.
      if (sl.Count=1) and
         (not gExternalTools[etViewer].Enabled) and
         ([fspDirectAccess, fspLinksToLocalFiles] * ActiveFrame.FileSource.Properties <> []) then
        begin
          AllFiles := ActiveFrame.CloneFiles;

          LinksResolveNeeded := fspLinksToLocalFiles in ActiveFrame.FileSource.Properties;

          n := -1;
          for i := 0 to AllFiles.Count - 1 do
            begin
              aFile := AllFiles[i];
              if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
              begin
                if aFile.Name = ActiveFile.Name then
                  n := i;
                if LinksResolveNeeded then
                  ActiveFrame.FileSource.GetLocalName(aFile);
                if (n <> -1) and (i <> n) then
                  sl.Add(aFile.FullPath);
              end;
            end;

          for i:=0 to n-1 do
            begin
              aFile := AllFiles[i];
              if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
                sl.Add(aFile.FullPath);
            end;
        end;

      // if sl has files then view it
      if sl.Count > 0 then
        ShowViewerByGlobList(sl, aFileSource);
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningViewer,
          rsMsgInvalidCommandLine + ' (' + rsToolViewer + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;

  finally
    FreeAndNil(sl);
    FreeAndNil(AllFiles);
    FreeAndNil(SelectedFiles);
    FreeAndNil(ActiveFile);
  end;
end;

procedure TMainCommands.cm_QuickView(const Params: array of string);
var
  Param: String;
begin
  with frmMain do
  begin
    Param := GetDefaultParam(Params);
    if Assigned(QuickViewPanel) then
      begin
        QuickViewClose;
      end
    else if (param <> 'Close') then
      begin
        QuickViewShow(NotActiveNotebook.ActivePage, ActiveFrame);
      end;
  end;
end;

procedure TMainCommands.cm_BriefView(const Params: array of string);
var
  aFileView: TFileView;
begin
  with frmMain do
  begin
    aFileView:= TBriefFileView.Create(ActiveNotebook.ActivePage, ActiveFrame);
    ActiveNotebook.ActivePage.FileView:= aFileView;
    ActiveFrame.SetFocus;
  end;
end;

procedure TMainCommands.cm_LeftBriefView(const Params: array of string);
var
  aFileView: TFileView;
begin
  with frmMain do
  begin
    aFileView:= TBriefFileView.Create(LeftTabs.ActivePage, FrameLeft);
    LeftTabs.ActivePage.FileView:= aFileView;
  end;
end;

procedure TMainCommands.cm_RightBriefView(const Params: array of string);
var
  aFileView: TFileView;
begin
  with frmMain do
  begin
    aFileView:= TBriefFileView.Create(RightTabs.ActivePage, FrameRight);
    RightTabs.ActivePage.FileView:= aFileView;
  end;
end;

procedure TMainCommands.cm_ColumnsView(const Params: array of string);
var
  AParam: String;
  aFileView: TFileView;
begin
  with frmMain do
  begin
    GetParamValue(Params, 'columnset', AParam);
    if (ActiveFrame is TColumnsFileView) then
      TColumnsFileView(ActiveFrame).SetColumnSet(AParam)
    else begin
      aFileView:= TColumnsFileView.Create(ActiveNotebook.ActivePage, ActiveFrame, AParam);
      ActiveNotebook.ActivePage.FileView:= aFileView;
      ActiveFrame.SetFocus;
    end;
  end;
end;

procedure TMainCommands.cm_LeftColumnsView(const Params: array of string);
var
  AParam: String;
  aFileView: TFileView;
begin
  with frmMain do
  begin
    GetParamValue(Params, 'columnset', AParam);
    if (FrameLeft is TColumnsFileView) then
      TColumnsFileView(FrameLeft).SetColumnSet(AParam)
    else begin
      aFileView:= TColumnsFileView.Create(LeftTabs.ActivePage, FrameLeft, AParam);
      LeftTabs.ActivePage.FileView:= aFileView;
    end;
  end;
end;

procedure TMainCommands.cm_RightColumnsView(const Params: array of string);
var
  AParam: String;
  aFileView: TFileView;
begin
  with frmMain do
  begin
    GetParamValue(Params, 'columnset', AParam);
    if (FrameRight is TColumnsFileView) then
      TColumnsFileView(FrameRight).SetColumnSet(AParam)
    else begin
      aFileView:= TColumnsFileView.Create(RightTabs.ActivePage, FrameRight, AParam);
      RightTabs.ActivePage.FileView:= aFileView;
    end;
  end;
end;

procedure ToggleOrNotToOrFromThumbnailsView(WorkingFileView: TFileView; WorkingNotebook: TFileViewNotebook);
var
  aFileView: TFileView;
begin
  if WorkingFileView.ClassType <> TThumbFileView then
  begin
    // Save current file view type
    WorkingNotebook.ActivePage.BackupViewClass := TFileViewClass(WorkingFileView.ClassType);
    // Create thumbnails view
    aFileView:= TThumbFileView.Create(WorkingNotebook.ActivePage, WorkingFileView);
  end
  else
  begin
    // Restore previous file view type
    aFileView:= WorkingNotebook.ActivePage.BackupViewClass.Create(WorkingNotebook.ActivePage, WorkingFileView);
  end;
  WorkingNotebook.ActivePage.FileView:= aFileView;
end;

procedure TMainCommands.cm_ThumbnailsView(const Params: array of string);
begin
  case frmMain.SelectedPanel of
    fpLeft: ToggleOrNotToOrFromThumbnailsView(frmMain.FrameLeft, frmMain.LeftTabs);
    fpRight: ToggleOrNotToOrFromThumbnailsView(frmMain.FrameRight, frmMain.RightTabs);
  end;
  frmMain.ActiveFrame.SetFocus;
end;

procedure TMainCommands.cm_LeftThumbView(const Params: array of string);
begin
  ToggleOrNotToOrFromThumbnailsView(frmMain.FrameLeft, frmMain.LeftTabs);
  frmMain.ActiveFrame.SetFocus;
end;

procedure TMainCommands.cm_RightThumbView(const Params: array of string);
begin
  ToggleOrNotToOrFromThumbnailsView(frmMain.FrameRight, frmMain.RightTabs);
  frmMain.ActiveFrame.SetFocus;
end;

procedure TMainCommands.cm_TreeView(const Params: array of string);
begin
  gSeparateTree := not gSeparateTree;
  with frmMain do
  begin
    DisableAutoSizing;
    try
      UpdateShellTreeView;
      UpdateTreeViewPath;
      MainSplitterPos:= MainSplitterPos;
    finally
      EnableAutoSizing;
    end;
  end;
end;

procedure TMainCommands.cm_Edit(const Params: array of string);
var
  i: Integer;
  aFile: TFile;
  SelectedFiles: TFiles = nil;
  sCmd: string = '';
  sParams: string = '';
  sStartPath: string = '';
begin
  with frmMain do
  try
    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;

    for I := SelectedFiles.Count - 1 downto 0 do
    begin
      aFile := SelectedFiles[I];
      if aFile.IsDirectory or aFile.IsLinkToDirectory then
        SelectedFiles.Delete(I);
    end;

    if SelectedFiles.Count = 0 then
    begin
      msgWarning(rsMsgNoFilesSelected);
      Exit;
    end;

    if PrepareData(ActiveFrame.FileSource, SelectedFiles, @OnEditCopyOutStateChanged) <> pdrSynchronous then
      Exit;

    try

      // For now we only process one file.
      aFile := SelectedFiles[0];

      //now test if exists "EDIT" command in "extassoc.xml" :)
      if gExts.GetExtActionCmd(aFile, 'edit', sCmd, sParams, sStartPath) then
        ProcessExtCommandFork(sCmd, sParams, aFile.Path)
      else
        ShowEditorByGlob(aFile.FullPath);

    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningEditor,
          rsMsgInvalidCommandLine + ' (' + rsToolEditor + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;

  finally
    if Assigned(SelectedFiles) then
      FreeAndNil(SelectedFiles);
  end;
end;

procedure TMainCommands.cm_EditPath(const Params: array of string);
begin
  if gCurDir then frmMain.ActiveFrame.ExecuteCommand('cm_EditPath', Params);
end;

// Parameters:
// confirmation=
//   1/true            - show confirmation
//   0/false           - don't show confirmation
// queueid=            - by default put to this queue
//   <queue_identifier>
procedure TMainCommands.cm_Copy(const Params: array of string);
var
  bConfirmation, HasQueueId: Boolean;
  QueueIdentifier: TOperationsManagerQueueIdentifier;
begin
  bConfirmation := focCopy in gFileOperationsConfirmations;
  ReadCopyRenameParams(Params, bConfirmation, HasQueueId, QueueIdentifier);

  if HasQueueId then
    frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, bConfirmation, QueueIdentifier)
  else
    frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, bConfirmation);
end;

procedure TMainCommands.cm_CopyNoAsk(const Params: array of string);
begin
  frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, False);
end;

// Parameters:
// confirmation=
//   1/true            - show confirmation
//   0/false           - don't show confirmation
// queueid=            - by default put to this queue
//   <queue_identifier>
procedure TMainCommands.cm_Rename(const Params: array of string);
var
  bConfirmation, HasQueueId: Boolean;
  QueueIdentifier: TOperationsManagerQueueIdentifier;
begin
  bConfirmation := focMove in gFileOperationsConfirmations;
  ReadCopyRenameParams(Params, bConfirmation, HasQueueId, QueueIdentifier);

  if HasQueueId then
    frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, bConfirmation, QueueIdentifier)
  else
    frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, bConfirmation);
end;

procedure TMainCommands.cm_RenameNoAsk(const Params: array of string);
begin
  frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, False);
end;

procedure TMainCommands.cm_MakeDir(const Params: array of string);
var
  sPath: String;
  Files: TFiles;
  Directory: String;
  ActiveFile: TFile = nil;
  bMakeViaCopy: Boolean = False;
  Operation: TFileSourceOperation = nil;
  UI: TFileSourceOperationMessageBoxesUI = nil;
begin
  with frmMain do
  try
    if not (fsoCreateDirectory in ActiveFrame.FileSource.GetOperationsTypes) then
    begin
      if (fsoCopyIn in ActiveFrame.FileSource.GetOperationsTypes) then
        bMakeViaCopy := True
      else begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;
    end;

    ActiveFile := ActiveFrame.CloneActiveFile;
    if Assigned(ActiveFile) and ActiveFile.IsNameValid then
    begin
      if ActiveFile.IsDirectory or ActiveFile.IsLinkToDirectory then
        sPath := ActiveFile.Name
      else begin
        sPath := ActiveFile.NameNoExt;
      end;
    end
    else
      sPath := EmptyStr;

    if not frmMkDir.ShowMkDir(sPath) then Exit;   // show makedir dialog
    if (sPath = EmptyStr) then Exit;

    if bMakeViaCopy then
    begin
      Directory := GetTempName(GetTempFolderDeletableAtTheEnd);
      if not mbForceDirectory(IncludeTrailingBackslash(Directory) + sPath) then
      begin
        MessageDlg(mbSysErrorMessage(GetLastOSError), mtError, [mbOK], 0);
        Exit;
      end;
      Files := TFiles.Create(Directory);
      sPath := IncludeTrailingBackslash(Directory) + ExtractWord(1, sPath, [PathDelim]);
      Files.Add(TFileSystemFileSource.CreateFileFromFile(sPath));
      Operation := ActiveFrame.FileSource.CreateCopyInOperation(TFileSystemFileSource.GetFileSource, Files, ActiveFrame.CurrentPath);
      if Assigned(Operation) then
      begin
        OperationsManager.AddOperation(Operation);
        Operation := nil;
      end;
      Exit;
    end;

    Operation := ActiveFrame.FileSource.CreateCreateDirectoryOperation(ActiveFrame.CurrentPath, sPath);
    if Assigned(Operation) then
    begin
      // Call directly - not through operations manager.
      UI := TFileSourceOperationMessageBoxesUI.Create;
      Operation.AddUserInterface(UI);
      Operation.Execute;

      sPath := ExtractFileName(ExcludeTrailingPathDelimiter(sPath));
      ActiveFrame.SetActiveFile(sPath);
    end;
  finally
    FreeAndNil(Operation);
    FreeAndNil(UI);
    FreeAndNil(ActiveFile);
  end;
end;

// Parameters:
// trashcan=
//   1/true            - delete to trash can
//   0/false           - delete directly
//   setting           - if gUseTrash then delete to trash, otherwise delete directly
//   reversesetting    - if gUseTrash then delete directly, otherwise delete to trash
// confirmation=
//   1/true            - show confirmation
//   0/false           - don't show confirmation
//
// Deprecated:
// "recycle"           - delete to trash can
// "norecycle"         - delete directly
// "recyclesetting"    - if gUseTrash then delete to trash, otherwise delete directly
// "recyclesettingrev" - if gUseTrash then delete directly, otherwise delete to trash
procedure TMainCommands.cm_Delete(const Params: array of string);
var
  I: Integer;
  Message: String;
  theFilesToDelete: TFiles;
  // 12.05.2009 - if delete to trash, then show another messages
  MsgDelSel, MsgDelFlDr : string;
  Operation: TFileSourceOperation;
  bRecycle: Boolean;
  QueueId: TOperationsManagerQueueIdentifier;
  bConfirmation, HasConfirmationParam: Boolean;
  Param, ParamTrashCan: String;
  BoolValue: Boolean;
begin
  with frmMain.ActiveFrame do
  begin
    if not (fsoDelete in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    bRecycle := gUseTrash;
    HasConfirmationParam := False;

    for Param in Params do
    begin
      if Param = 'recycle' then
        bRecycle := True
      else if Param = 'norecycle' then
        bRecycle := False
      else if Param = 'recyclesetting' then
        bRecycle := gUseTrash
      else if Param = 'recyclesettingrev' then
        bRecycle := not gUseTrash
      else if GetParamValue(Param, 'trashcan', ParamTrashCan) then
      begin
        if ParamTrashCan = 'setting' then
          bRecycle := gUseTrash
        else if ParamTrashCan = 'reversesetting' then
          bRecycle := not gUseTrash
        else if GetBoolValue(ParamTrashCan, BoolValue) then
          bRecycle := BoolValue;
      end
      else if GetParamBoolValue(Param, 'confirmation', BoolValue) then
      begin
        HasConfirmationParam := True;
        bConfirmation := BoolValue;
      end;
    end;

    // Save parameter for later use
    BoolValue := bRecycle;

    if bRecycle then
      bRecycle := FileSource.IsClass(TFileSystemFileSource) and
                  mbCheckTrash(CurrentPath);

    if not HasConfirmationParam then
    begin
      if not bRecycle then
        bConfirmation := focDelete in gFileOperationsConfirmations
      else
        bConfirmation := focDeleteToTrash in gFileOperationsConfirmations;
    end;

    // Showing delete dialog: to trash or to /dev/null :)
    If bRecycle then
     begin
      MsgDelSel := rsMsgDelSelT;
      MsgDelFlDr := rsMsgDelFlDrT;
     end
    else
     begin
      MsgDelSel := rsMsgDelSel;
      MsgDelFlDr := rsMsgDelFlDr;
     end;

    // Special case for fspLinksToLocalFiles
    if (fspLinksToLocalFiles in FileSource.Properties) then
      bRecycle := BoolValue;

    // ------------------------------------------------------

    theFilesToDelete := CloneSelectedOrActiveFiles; // free at Thread end by thread

    if Assigned(theFilesToDelete) then
    try
      if (theFilesToDelete.Count = 0) then Exit;
      if (theFilesToDelete.Count = 1) then
        Message:= Format(MsgDelSel, [theFilesToDelete[0].Name])
      else begin
         Message:= Format(MsgDelFlDr, [theFilesToDelete.Count]) + LineEnding;
         for I:= 0 to Min(4, theFilesToDelete.Count - 1) do
         begin
           Message+= LineEnding + theFilesToDelete[I].Name;
         end;
         if theFilesToDelete.Count > 5 then Message+= LineEnding + '...';
      end;
      if (bConfirmation = False) or (ShowDeleteDialog(Message, FileSource, QueueId)) then
      begin
        if FileSource.IsClass(TFileSystemFileSource) then
        begin
          if frmMain.NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
          begin
            for I:= 0 to theFilesToDelete.Count - 1 do
            begin
              if (theFilesToDelete[I].IsDirectory or theFilesToDelete[I].IsLinkToDirectory) and
                 IsInPath(theFilesToDelete[I].FullPath, frmMain.NotActiveFrame.CurrentPath, True, True) then
              begin
                frmMain.NotActiveFrame.CurrentPath:= theFilesToDelete.Path;
                Break;
              end;
            end;
          end
          else if frmMain.NotActiveFrame.FileSource.IsClass(TArchiveFileSource) then
          begin
            Message:= (frmMain.NotActiveFrame.FileSource as TArchiveFileSource).ArchiveFileName;
            for I:= 0 to theFilesToDelete.Count - 1 do
            begin
              if IsInPath(theFilesToDelete[I].FullPath, Message, True, True) then
              begin
                SetFileSystemPath(frmMain.NotActiveFrame, theFilesToDelete.Path);
                Break;
              end;
            end;
          end;
        end;

        Operation := FileSource.CreateDeleteOperation(theFilesToDelete);

        if Assigned(Operation) then
        begin
          // Special case for filesystem - 'recycle' parameter.
          if Operation is TFileSystemDeleteOperation then
            with Operation as TFileSystemDeleteOperation do
            begin
              // 30.04.2009 - передаем параметр корзины в поток.
              Recycle := bRecycle;
            end;

          // Start operation.
          OperationsManager.AddOperation(Operation, QueueId, False);
        end
        else
        begin
          msgWarning(rsMsgNotImplemented);
        end;
      end;

    finally
      FreeAndNil(theFilesToDelete);
    end;
  end;
end;

procedure TMainCommands.cm_CheckSumCalc(const Params: array of string);
var
  I: Integer;
  sFileName: String;
  SelectedFiles: TFiles;
  HashAlgorithm: THashAlgorithm;
  QueueId: TOperationsManagerQueueIdentifier;
  Operation: TFileSourceCalcChecksumOperation;
  bSeparateFile, bOpenFileAfterJobCompleted: Boolean;
begin
  // This will work only for filesystem.
  // For other file sources use temp file system when it's done.

  with frmMain do
  begin
    if not (fsoCalcChecksum in ActiveFrame.FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgNotImplemented);
      Exit;
      // Create temp file source.
      // CopyOut ActiveFrame.FileSource to TempFileSource.
      // Do command on TempFileSource and later delete it (or leave cached on disk?)
    end;

    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      if SelectedFiles.Count = 0 then
      begin
        msgWarning(rsMsgNoFilesSelected);
        Exit;
      end;

      bSeparateFile:= False;
      bOpenFileAfterJobCompleted:= False;
      for I := 0 to SelectedFiles.Count - 1 do // find files in selection
        if not SelectedFiles[I].IsDirectory then
          begin
            bSeparateFile:= True;
            Break;
          end;

      if SelectedFiles.Count > 1 then
        sFileName:= ActiveFrame.CurrentPath + MakeFileName(ActiveFrame.CurrentPath, 'checksum')
      else
        sFileName:= ActiveFrame.CurrentPath + SelectedFiles[0].Name;

      if ShowCalcCheckSum(sFileName, bSeparateFile, HashAlgorithm, bOpenFileAfterJobCompleted, QueueId) then
      begin
        Operation := ActiveFrame.FileSource.CreateCalcChecksumOperation(
                       SelectedFiles, ActiveFrame.CurrentPath, sFileName) as TFileSourceCalcChecksumOperation;

        if Assigned(Operation) then
        begin
          Operation.Mode := checksum_calc;
          Operation.OneFile := not bSeparateFile;
          Operation.OpenFileAfterOperationCompleted := bOpenFileAfterJobCompleted;
          Operation.Algorithm := HashAlgorithm;

          // Start operation.
          OperationsManager.AddOperation(Operation, QueueId, False);
        end
        else
        begin
          msgWarning(rsMsgNotImplemented);
        end;
      end;

    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_CheckSumVerify(const Params: array of string);
var
  I: Integer;
  Hash: String;
  SelectedFiles: TFiles;
  Algorithm: THashAlgorithm;
  Operation: TFileSourceCalcChecksumOperation;
begin
  // This will work only for filesystem.
  // For other file sources use temp file system when it's done.

  with frmMain do
  begin
    if not (fsoCalcChecksum in ActiveFrame.FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgNotImplemented);
      Exit;
      // Create temp file source.
      // CopyOut ActiveFrame.FileSource to TempFileSource.
      // Do command on TempFileSource and later delete it (or leave cached on disk?)
    end;

    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      if SelectedFiles.Count = 0 then
      begin
        msgWarning(rsMsgNoFilesSelected);
        Exit;
      end;

      for I := 0 to SelectedFiles.Count - 1 do // find files in selection
        if not FileExtIsHash(SelectedFiles[I].Extension) then
        begin
          if (SelectedFiles.Count > 1) or (SelectedFiles[I].IsDirectory) or
             (SelectedFiles[I].IsLinkToDirectory) then
          begin
            msgError(rsMsgSelectOnlyCheckSumFiles);
            Exit;
          end
          else begin
            if not ShowCalcVerifyCheckSum(Hash, Algorithm) then
              Exit;
          end;
        end;

      Operation := ActiveFrame.FileSource.CreateCalcChecksumOperation(
                     SelectedFiles, Hash, '') as TFileSourceCalcChecksumOperation;

      if Assigned(Operation) then
      begin
        Operation.Algorithm := Algorithm;
        Operation.AddStateChangedListener([fsosStopped], @OnCalcChecksumStateChanged);
        Operation.Mode := checksum_verify;

        // Start operation.
        OperationsManager.AddOperation(Operation);
      end
      else
      begin
        msgWarning(rsMsgNotImplemented);
      end;

    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_FocusCmdLine(const Params: array of string);
begin
  if frmMain.edtCommand.Visible then
  begin
    // Show temporarily command line on user request.
    if (not gCmdLine) and (frmMain.IsCommandLineVisible = False) then
    begin
      frmMain.pnlCommand.Show;
      frmMain.pnlCmdLine.Show;
    end;

    frmMain.edtCommand.SetFocus;
  end;
end;

procedure TMainCommands.cm_FileAssoc(const Params: array of string);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  Options := ShowOptions(TfrmOptionsFileAssoc);
  Application.ProcessMessages;
  Editor := Options.GetEditor(TfrmOptionsFileAssoc);
  if Editor.CanFocus then Editor.SetFocus;
  TfrmOptionsFileAssoc(Editor).MakeUsInPositionToWorkWithActiveFile;
end;

procedure TMainCommands.cm_HelpIndex(const Params: array of string);
begin
  ShowHelpOrErrorForKeyword('', '/index.html');
end;

procedure TMainCommands.cm_Keyboard(const Params: array of string);
begin
  ShowHelpOrErrorForKeyword('', '/shortcuts.html');
end;

procedure TMainCommands.cm_VisitHomePage(const Params: array of string);
var
  ErrMsg: String = '';
begin
  dmHelpMgr.HTMLHelpDatabase.ShowURL('http://doublecmd.sourceforge.net','Double Commander Web Site', ErrMsg);
end;

procedure TMainCommands.cm_About(const Params: array of string);
begin
  ShowAboutBox;
end;

procedure TMainCommands.cm_ShowSysFiles(const Params: array of string);
begin
  with frmMain do
  begin
    uGlobs.gShowSystemFiles:= not uGlobs.gShowSystemFiles;
    actShowSysFiles.Checked:= uGlobs.gShowSystemFiles;
    UpdateTreeView;
    // Update all tabs
    ForEachView(@DoUpdateFileView, nil);
  end;
end;

procedure TMainCommands.cm_SwitchIgnoreList(const Params: array of string);
{$OPTIMIZATION OFF}
var
  WantedIgnoreList, BoolValue:boolean;
begin
  WantedIgnoreList:=gIgnoreListFileEnabled;
  with frmMain do
  begin
    if Length(Params)>0 then
    begin
      if GetParamBoolValue(Params[0], 'ignorelist', BoolValue) then
        WantedIgnoreList:=BoolValue
      else
        WantedIgnoreList := not WantedIgnoreList;
    end
    else
    begin
      WantedIgnoreList := not WantedIgnoreList;
    end;

    if WantedIgnoreList<>gIgnoreListFileEnabled then
    begin
      gIgnoreListFileEnabled:=WantedIgnoreList;
      actSwitchIgnoreList.Checked:= gIgnoreListFileEnabled;
      //repaint both panels
      FrameLeft.Reload;
      FrameRight.Reload;
    end;
  end;
end;
{$OPTIMIZATION DEFAULT}

// Parameter is name of TOptionsEditorClass.
procedure TMainCommands.cm_Options(const Params: array of string);
begin
  ShowOptions(GetDefaultParam(Params));
end;

procedure TMainCommands.cm_CompareContents(const Params: array of string);
var
  FilesNumber: Integer = 0;
  DirsNumber: Integer = 0;

  procedure CountFiles(const Files: TFiles);
  var I: Integer;
  begin
    if Assigned(Files) then
      for I := 0 to Files.Count - 1 do
        if Files[I].IsDirectory then
          Inc(DirsNumber)
        else
          Inc(FilesNumber);
  end;

var
  I : Integer;
  Param: String;
  ActiveSelectedFiles: TFiles = nil;
  NotActiveSelectedFiles: TFiles = nil;
  FirstFileSource: IFileSource = nil;
  FirstFileSourceFiles: TFiles = nil;
  SecondFileSource: IFileSource = nil;
  SecondFileSourceFiles: TFiles = nil;
begin
  with frmMain do
  begin
    Param := GetDefaultParam(Params);

    if Param = 'dir' then
    begin
      if gExternalTools[etDiffer].Enabled then
        ShowDifferByGlob(FrameLeft.CurrentPath, FrameRight.CurrentPath)
      else
        MsgWarning(rsMsgNotImplemented);
      Exit;
    end;

    try
      ActiveSelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;

      if ActiveSelectedFiles.Count = 1 then
      begin
        // If no files selected in the opposite panel and panels have
        // different path then try to get file with the same name.
        if (not NotActiveFrame.HasSelectedFiles) and (not mbCompareFileNames(NotActiveFrame.CurrentPath, ActiveFrame.CurrentPath)) then
        begin
          for I := 0 to NotActiveFrame.DisplayFiles.Count - 1 do
            if mbCompareFileNames(NotActiveFrame.DisplayFiles[I].FSFile.Name, ActiveSelectedFiles[0].Name) then
            begin
              NotActiveSelectedFiles := TFiles.Create(NotActiveFrame.CurrentPath);
              NotActiveSelectedFiles.Add(NotActiveFrame.DisplayFiles[I].FSFile.Clone);
              Break;
            end;
        end;

        if not Assigned(NotActiveSelectedFiles) then
          NotActiveSelectedFiles := NotActiveFrame.CloneSelectedOrActiveFiles;

        if NotActiveSelectedFiles.Count <> 1 then
        begin
          // Only one file selected in active panel.
          MsgWarning(rsMsgInvalidSelection);
          Exit;
        end;

        { compare single selected files in both panels }

        case gResultingFramePositionAfterCompare of
          rfpacActiveOnLeft:
            begin
              FirstFileSource := ActiveFrame.FileSource;
              FirstFileSourceFiles := ActiveSelectedFiles;
              SecondFileSource := NotActiveFrame.FileSource;
              SecondFileSourceFiles := NotActiveSelectedFiles;
            end;
          rfpacLeftOnLeft:
            begin
              if ActiveFrame = FrameLeft then
              begin
                FirstFileSource := ActiveFrame.FileSource;
                FirstFileSourceFiles := ActiveSelectedFiles;
                SecondFileSource := NotActiveFrame.FileSource;
                SecondFileSourceFiles := NotActiveSelectedFiles;
              end
              else begin
                FirstFileSource := NotActiveFrame.FileSource;
                FirstFileSourceFiles := NotActiveSelectedFiles;
                SecondFileSource := ActiveFrame.FileSource;
                SecondFileSourceFiles := ActiveSelectedFiles;
              end;
            end;
        end;
      end
      else if ActiveSelectedFiles.Count > 1 then
      begin
        { compare all selected files in active frame }

        FirstFileSource := ActiveFrame.FileSource;
        FirstFileSourceFiles := ActiveSelectedFiles;
      end;

      CountFiles(FirstFileSourceFiles);
      CountFiles(SecondFileSourceFiles);

      if ((FilesNumber > 0) and (DirsNumber > 0))
      or ((FilesNumber = 1) or (DirsNumber = 1)) then
        // Either files or directories must be selected and more than one.
        MsgWarning(rsMsgInvalidSelection)
      else if (FilesNumber = 0) and (DirsNumber = 0) then
        MsgWarning(rsMsgNoFilesSelected)
      else if (FilesNumber > 2) and not gExternalTools[etDiffer].Enabled then
        MsgWarning(rsMsgTooManyFilesSelected)
      else if (DirsNumber > 0) and not gExternalTools[etDiffer].Enabled then
        MsgWarning(rsMsgNotImplemented)
      else
      begin
        if not Assigned(SecondFileSource) then
          PrepareToolData(FirstFileSource, FirstFileSourceFiles,
                          @ShowDifferByGlobList)
        else
          PrepareToolData(FirstFileSource, FirstFileSourceFiles,
                          SecondFileSource, SecondFileSourceFiles,
                          @ShowDifferByGlobList);
      end;

    finally
      ActiveSelectedFiles.Free;
      NotActiveSelectedFiles.Free;
    end;
  end;
end;

{ TMainCommands.cm_ShowMainMenu }
procedure TMainCommands.cm_ShowMainMenu(const Params: array of string);
{$OPTIMIZATION OFF}
var
  WantedMainMenu, BoolValue: boolean;
  bWantedTreeViewMenu: boolean = False;
  Param: string;
  sMaybeMenuItem: TMenuItem;
  iWantedPosX: integer = 0;
  iWantedPosY: integer = 0;
  iWantedWidth: integer = 800;
  iWantedHeight: integer = 600;
begin
  WantedMainMenu:=gMainMenu;

  if Length(Params)>0 then
  begin
    for Param in Params do
      if GetParamBoolValue(Param, 'menu', BoolValue) then WantedMainMenu := BoolValue
      else if GetParamBoolValue(Param, 'treeview', BoolValue) then bWantedTreeViewMenu := BoolValue
      else WantedMainMenu := not WantedMainMenu;
  end
  else
  begin
    WantedMainMenu := not WantedMainMenu;
  end;

  if not bWantedTreeViewMenu then
  begin
  if WantedMainMenu<>gMainMenu then
  begin
    gMainMenu:=WantedMainMenu;
    DoShowMainMenu(gMainMenu);
    end;
  end
  else
  begin
    DoComputeSizeAndPosForWindowInMiddle(iWantedPosX, iWantedPosY, iWantedWidth, iWantedHeight);
    sMaybeMenuItem := GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(frmMain.mnuMain, tvmcMainMenu, iWantedPosX, iWantedPosY, iWantedWidth, iWantedHeight);
    if sMaybeMenuItem <> nil then
    begin
      if sMaybeMenuItem.Action <> nil then
      begin
        if sMaybeMenuItem.Action.OnExecute<>nil then
          sMaybeMenuItem.Action.OnExecute(sMaybeMenuItem.Action)
      end
      else
        if sMaybeMenuItem.OnClick<>nil then
          sMaybeMenuItem.OnClick(sMaybeMenuItem);
    end;
  end;
end;
{$OPTIMIZATION DEFAULT}

procedure TMainCommands.cm_Refresh(const Params: array of string);
begin
  with frmMain do
  begin
    ActiveFrame.FileSource.Reload(ActiveFrame.CurrentPath);
    ActiveFrame.Reload(True);
    if ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
    begin
      UpdateDiskCount;
      UpdateSelectedDrives;
    end;
  end;
end;

//------------------------------------------------------

{ TMainCommands.DoActualMarkUnMark }
procedure TMainCommands.DoActualMarkUnMark(const Params: array of string; bSelect: boolean);
var
  iParameter: integer;
  sWantedMask, sParamValue: string;
  sAttribute: string = '';
  bWantedCaseSensitive, bWantedIgnoreAccents, bWantedWindowsInterpretation: boolean;
  pbWantedCaseSensitive, pbWantedIgnoreAccents, pbWantedWindowsInterpretation: PBoolean;
  psAttribute: pString = nil;
  MarkSearchTemplateRec: TSearchTemplateRec;
  MarkFileChecks: TFindFileChecks;
begin
  sWantedMask := '';
  pbWantedCaseSensitive := nil;
  pbWantedIgnoreAccents := nil;
  pbWantedWindowsInterpretation := nil;

  for iParameter:=0 to pred(Length(Params)) do
  begin
    if GetParamValue(Params[iParameter], 'mask', sParamValue) then sWantedMask := sParamValue
    else if GetParamBoolValue(Params[iParameter], 'casesensitive', bWantedCaseSensitive) then pbWantedCaseSensitive := @bWantedCaseSensitive
    else if GetParamBoolValue(Params[iParameter], 'ignoreaccents', bWantedIgnoreAccents) then pbWantedIgnoreAccents := @bWantedIgnoreAccents
    else if GetParamBoolValue(Params[iParameter], 'windowsinterpretation', bWantedWindowsInterpretation) then pbWantedWindowsInterpretation := @bWantedWindowsInterpretation
    else if GetParamValue(Params[iParameter], 'attr', sAttribute) then psAttribute := @sAttribute;
  end;

  // When mask is specified, we don't prompt the user
  if sWantedMask<>'' then
  begin
    if psAttribute <> nil then MarkSearchTemplateRec.AttributesPattern := psAttribute^ else MarkSearchTemplateRec.AttributesPattern := gMarkDefaultWantedAttribute;
    AttrsPatternOptionsToChecks(MarkSearchTemplateRec, MarkFileChecks);
    frmMain.ActiveFrame.MarkGroup(sWantedMask, bSelect, pbWantedCaseSensitive, pbWantedIgnoreAccents, pbWantedWindowsInterpretation, @MarkFileChecks)
  end
  else
  begin
    frmMain.ActiveFrame.MarkGroup(bSelect, pbWantedCaseSensitive, pbWantedIgnoreAccents, pbWantedWindowsInterpretation, psAttribute)
  end;
end;

{ TMainCommands.DoActualMarkApplyOnAll }
procedure TMainCommands.DoActualMarkApplyOnAll(const maoaDispatcher: TMarkApplyOnAllDispatcher; const Params: array of string);
var
  iParameter: integer;
  sAttribute, sParam: string;
  MarkSearchTemplateRec: TSearchTemplateRec;
  MarkFileChecks: TFindFileChecks;
begin
  sAttribute := gMarkDefaultWantedAttribute;
  for iParameter:=0 to pred(Length(Params)) do
    if GetParamValue(Params[iParameter], 'attr', sParam) then sAttribute := sParam;
  MarkSearchTemplateRec.AttributesPattern := sAttribute;
  AttrsPatternOptionsToChecks(MarkSearchTemplateRec, MarkFileChecks);
  frmMain.ActiveFrame.MarkApplyOnAllFiles(maoaDispatcher, MarkFileChecks);
end;

{ TMainCommands.cm_MarkMarkAll }
procedure TMainCommands.cm_MarkMarkAll(const Params: array of string);
begin
  DoActualMarkApplyOnAll(tmaoa_Mark, Params);
end;

{ TMainCommands.cm_MarkUnmarkAll }
procedure TMainCommands.cm_MarkUnmarkAll(const Params: array of string);
begin
  DoActualMarkApplyOnAll(tmaoa_UnMark, Params);
end;

{ TMainCommands.cm_MarkInvert }
procedure TMainCommands.cm_MarkInvert(const Params: array of string);
begin
  DoActualMarkApplyOnAll(tmaoa_InvertMark, Params);
end;

{ TMainCommands.cm_MarkPlus }
procedure TMainCommands.cm_MarkPlus(const Params: array of string);
begin
  DoActualMarkUnMark(Params, True);
end;

{ TMainCommands.cm_MarkMinus }
procedure TMainCommands.cm_MarkMinus(const Params: array of string);
begin
  DoActualMarkUnMark(Params, False);
end;

procedure TMainCommands.cm_MarkCurrentName(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentName(True);
end;

procedure TMainCommands.cm_UnmarkCurrentName(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentName(False);
end;

procedure TMainCommands.cm_MarkCurrentNameExt(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentNameExt(True);
end;

procedure TMainCommands.cm_UnmarkCurrentNameExt(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentNameExt(False);
end;

procedure TMainCommands.cm_MarkCurrentExtension(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentExtension(True);
end;

procedure TMainCommands.cm_UnmarkCurrentExtension(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentExtension(False);
end;

procedure TMainCommands.cm_MarkCurrentPath(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentPath(True);
end;

procedure TMainCommands.cm_UnmarkCurrentPath(const Params: array of string);
begin
  frmMain.ActiveFrame.MarkCurrentPath(False);
end;

procedure TMainCommands.cm_SaveSelection(const Params: array of string);
begin
  frmMain.ActiveFrame.SaveSelection;
end;

procedure TMainCommands.cm_RestoreSelection(const Params: array of string);
begin
  frmMain.ActiveFrame.RestoreSelection;
end;

procedure TMainCommands.cm_SaveSelectionToFile(const Params: array of string);
begin
  frmMain.ActiveFrame.SaveSelectionToFile(GetDefaultParam(Params));
end;

procedure TMainCommands.cm_LoadSelectionFromFile(const Params: array of string);
begin
  frmMain.ActiveFrame.LoadSelectionFromFile(GetDefaultParam(Params));
end;

procedure TMainCommands.cm_LoadSelectionFromClip(const Params: array of string);
begin
  frmMain.ActiveFrame.LoadSelectionFromClipboard;
end;

{ TMainCommands.DoParseParametersForPossibleTreeViewMenu }
procedure TMainCommands.DoParseParametersForPossibleTreeViewMenu(const Params: array of string; gDefaultConfigWithCommand, gDefaultConfigWithDoubleClick:boolean; var bUseTreeViewMenu:boolean; var bUsePanel:boolean; var p: TPoint);
var
  Param, sValue: string;
  bSpecifiedPopup: boolean = false;
  bSpecifiedTreeView: boolean = false;
  bSpecifiedPanel: boolean = false;
  bSpecifiedMouse: boolean = false;
begin
  for Param in Params do
  begin
    if GetParamValue(Param, 'menutype', sValue) then
    begin
      if (sValue = 'popup') OR (sValue = 'combobox') then bSpecifiedPopup := True else
      if sValue = 'treeview' then bSpecifiedTreeView := True;
    end
    else if GetParamValue(Param, 'position', sValue) then
    begin
      if sValue = 'panel' then bSpecifiedPanel:=true else
      if sValue = 'cursor' then bSpecifiedMouse:=true;
    end;
  end;

  if (not bSpecifiedPopup) AND (bSpecifiedTreeView OR (not bSpecifiedMouse AND gDefaultConfigWithCommand) OR (bSpecifiedMouse AND gDefaultConfigWithDoubleClick)) then
    bUseTreeViewMenu:=True;
  if bSpecifiedPanel OR (not bSpecifiedMouse AND bUsePanel) then
  begin
    p := frmMain.ActiveFrame.ClientToScreen(Classes.Point(0, 0));
    bUsePanel := True;
  end
  else
  begin
    p := Mouse.CursorPos;
    bUsePanel := False;
  end;
end;

{ TMainCommands.cm_DirHotList }
// Command to SHOW the Directory Hotlist popup menu
// The directory popup hotlist is run-time continously regenerated each time command is invoken.
// If any param is provided, it is assume the popup menu as to be shown where the mouse cursor is which is friendly with user since it minimize mouse travel.
//
procedure TMainCommands.cm_DirHotList(const Params: array of string);
var
  bUseTreeViewMenu: boolean = false;
  bUsePanel: boolean = true;
  p: TPoint = (x:0; y:0);
  iWantedWidth: integer = 0;
  iWantedHeight: integer = 0;
  sMaybeMenuItem: TMenuItem = nil;
begin
  // 1. Let's parse our parameters.
  DoParseParametersForPossibleTreeViewMenu(Params, gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand, gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick, bUseTreeViewMenu, bUsePanel, p);

  // 2. No matter what, we need to fill in the popup menu structure.
  gDirectoryHotlist.PopulateMenuWithHotDir(frmMain.pmHotList,@frmMain.HotDirSelected,@frmMain.miHotAddOrConfigClick,mpHOTDIRSWITHCONFIG,0); // TODO: i thing in future this must call on create or change
    Application.ProcessMessages; //TODO: Same thing as with "cm_DirHotList", in Windows, Not sure why, but on all system I tried, this eliminate a "beep" when the popup is shown.

  // 3. Show the appropriate menu.
  if bUseTreeViewMenu then
  begin
    if not bUsePanel then
      iWantedHeight := ((frmMain.ActiveFrame.ClientToScreen(Classes.Point(0, 0)).y + frmMain.ActiveFrame.Height) - p.y)
  else
  begin
      iWantedWidth := frmMain.ActiveFrame.Width;
      iWantedHeight := frmMain.ActiveFrame.Height;
  end;

    sMaybeMenuItem := GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(frmMain.pmHotList, tvmcHotDirectory, p.X, p.Y, iWantedWidth, iWantedHeight);
    if sMaybeMenuItem <> nil then sMaybeMenuItem.OnClick(sMaybeMenuItem);
  end
  else
  begin
  frmMain.pmHotList.Popup(p.X,p.Y);
  end;
end;

{ TMainCommands.cm_ConfigDirHotList }
// Mainly present for backward compatibility since "cm_ConfigDirHotList" existed before.
//
procedure TMainCommands.cm_ConfigDirHotList(const Params: array of string);
begin
  cm_WorkWithDirectoryHotlist(['action=config', 'source='+QuoteStr(frmMain.ActiveFrame.CurrentLocation), 'target='+QuoteStr(frmMain.NotActiveFrame.CurrentLocation), 'index=0']);
end;

{ TMainCommands.cm_WorkWithDirectoryHotlist }
// The parameter 0, in text, indicate the job to do to generic "SubmitToAddOrConfigToHotDirDlg" routine.
// This way, "SubmitToAddOrConfigToHotDirDlg" is to entry point to attempt to do anything in the Directory Hotlist conifguration screen.
//
procedure TMainCommands.cm_WorkWithDirectoryHotlist(const Params: array of string);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
  SearchingIndex, WantedAction, WantedIndexToEdit: integer;
  WantedSourcePath, WantedTargetPath : string;
  Param, sValue: String;
begin
  //1o) Let's set our default values
  WantedAction := ACTION_INVALID;
  WantedSourcePath := frmMain.ActiveFrame.CurrentPath;
  WantedTargetPath := frmMain.NotActiveFrame.CurrentPath;
  WantedIndexToEdit := 0;

  //2o) Let's parse the parameter to get the wanted ones
  for Param in Params do
  begin
    if GetParamValue(Param, 'action', sValue) then
    begin
      SearchingIndex:=1;
      while ( (SearchingIndex<=length(HOTLISTMAGICWORDS)) AND (WantedAction = ACTION_INVALID) ) do
        if sValue=HOTLISTMAGICWORDS[SearchingIndex] then WantedAction:=SearchingIndex else inc(SearchingIndex);
    end
    else if GetParamValue(Param, 'source', sValue) then
    begin
      sValue:=RemoveQuotation(PrepareParameter(sValue));
      if (sValue<>'') and (not HasPathInvalidCharacters(sValue)) then WantedSourcePath:=sValue;
    end
    else if GetParamValue(Param, 'target', sValue) then
    begin
      sValue:=RemoveQuotation(PrepareParameter(sValue));
      if (sValue<>'') and (not HasPathInvalidCharacters(sValue)) then WantedTargetPath:=sValue;
    end
    else if GetParamValue(Param, 'index', sValue) then
    begin
      WantedIndexToEdit:=(strtointdef(sValue,0));
    end;
  end;
  if WantedAction=ACTION_INVALID then WantedAction:=ACTION_JUSTSHOWCONFIGHOTLIST;

  //3o) Let's do the sorting job now!
  Options := ShowOptions(TfrmOptionsDirectoryHotlist);
  Editor := Options.GetEditor(TfrmOptionsDirectoryHotlist);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
  TfrmOptionsDirectoryHotlist(Editor).SubmitToAddOrConfigToHotDirDlg(WantedAction, WantedSourcePath, WantedTargetPath, WantedIndexToEdit);
end;

procedure TMainCommands.cm_Search(const Params: array of string);
var
  TemplateName: String;
begin
  if not frmMain.ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
    msgError(rsMsgErrNotSupported)
  else begin
    if Length(Params) > 0 then
      TemplateName:= Params[0]
    else begin
      TemplateName:= gSearchDefaultTemplate;
    end;
    ShowFindDlg(frmMain.ActiveFrame, TemplateName);
  end;
end;

procedure TMainCommands.cm_SyncDirs(const Params: array of string);
var
  OperationType: TFileSourceOperationType;
begin
  with frmMain do
  begin
    if GetCopyOperationType(FrameLeft.FileSource, FrameRight.FileSource, OperationType) or
       GetCopyOperationType(FrameRight.FileSource, FrameLeft.FileSource, OperationType) then
    begin
      ShowSyncDirsDlg(FrameLeft, FrameRight);
    end
    else begin
      msgWarning(rsMsgErrNotSupported);
    end;
  end;
end;

//------------------------------------------------------

procedure TMainCommands.cm_SymLink(const Params: array of string);
var
  sExistingFile, sLinkToCreate: String;
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    // Symlinks work only for file system.
    if not (ActiveFrame.FileSource.IsClass(TFileSystemFileSource)) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
      // Or create a symlink in temp filesystem and CopyIn to target file source.
    end;

    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      if SelectedFiles.Count > 1 then
        msgWarning(rsMsgTooManyFilesSelected)
      else if SelectedFiles.Count = 0 then
        msgWarning(rsMsgNoFilesSelected)
      else
      begin
        sExistingFile := SelectedFiles[0].Path + SelectedFiles[0].Name;

        if Length(Params) > 0 then
          sLinkToCreate := Params[0]
        else
        begin
          if NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            sLinkToCreate := NotActiveFrame.CurrentPath
          else
            sLinkToCreate := ActiveFrame.CurrentPath;
        end;

        sLinkToCreate := sLinkToCreate + SelectedFiles[0].Name;

        if ShowSymLinkForm(sExistingFile, sLinkToCreate, ActiveFrame.CurrentPath) then
        begin
          ActiveFrame.Reload;
          if NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            NotActiveFrame.Reload;
        end;
      end;

    finally
      FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_HardLink(const Params: array of string);
var
  sExistingFile, sLinkToCreate: String;
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    // Hard links work only for file system.
    if not (ActiveFrame.FileSource.IsClass(TFileSystemFileSource)) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      if SelectedFiles.Count > 1 then
        msgWarning(rsMsgTooManyFilesSelected)
      else if SelectedFiles.Count = 0 then
        msgWarning(rsMsgNoFilesSelected)
      else
      begin
        sExistingFile := SelectedFiles[0].Path + SelectedFiles[0].Name;

        if Length(Params) > 0 then
          sLinkToCreate := Params[0]
        else
        begin
          if NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            sLinkToCreate := NotActiveFrame.CurrentPath
          else
            sLinkToCreate := ActiveFrame.CurrentPath;
        end;

        sLinkToCreate := sLinkToCreate + SelectedFiles[0].Name;

        if ShowHardLinkForm(sExistingFile, sLinkToCreate, ActiveFrame.CurrentPath) then
        begin
          ActiveFrame.Reload;
          if NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            NotActiveFrame.Reload;
        end;
      end;

    finally
      FreeAndNil(SelectedFiles);
    end;
  end;
end;

// Uses to change sort direction when columns header is disabled
procedure TMainCommands.cm_ReverseOrder(const Params: array of string);
begin
  with frmMain.ActiveFrame do
    Sorting := ReverseSortDirection(Sorting);
end;

procedure TMainCommands.cm_LeftReverseOrder(const Params: array of string);
begin
  with frmMain.FrameLeft do
    Sorting := ReverseSortDirection(Sorting);
end;

procedure TMainCommands.cm_RightReverseOrder(const Params: array of string);
begin
  with frmMain.FrameRight do
    Sorting := ReverseSortDirection(Sorting);
end;

procedure TMainCommands.cm_SortByName(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfNameNoExtension);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortByExt(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfExtension);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortBySize(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfSize);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortByDate(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfModificationTime);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortByAttr(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfAttr);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_LeftSortByName(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfNameNoExtension);
  DoSortByFunctions(frmMain.FrameLeft, FileFunctions);
end;

procedure TMainCommands.cm_LeftSortByExt(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfExtension);
  DoSortByFunctions(frmMain.FrameLeft, FileFunctions);
end;

procedure TMainCommands.cm_LeftSortBySize(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfSize);
  DoSortByFunctions(frmMain.FrameLeft, FileFunctions);
end;

procedure TMainCommands.cm_LeftSortByDate(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfModificationTime);
  DoSortByFunctions(frmMain.FrameLeft, FileFunctions);
end;

procedure TMainCommands.cm_LeftSortByAttr(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfAttr);
  DoSortByFunctions(frmMain.FrameLeft, FileFunctions);
end;

procedure TMainCommands.cm_RightSortByName(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfNameNoExtension);
  DoSortByFunctions(frmMain.FrameRight, FileFunctions);
end;

procedure TMainCommands.cm_RightSortByExt(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfExtension);
  DoSortByFunctions(frmMain.FrameRight, FileFunctions);
end;

procedure TMainCommands.cm_RightSortBySize(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfSize);
  DoSortByFunctions(frmMain.FrameRight, FileFunctions);
end;

procedure TMainCommands.cm_RightSortByDate(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfModificationTime);
  DoSortByFunctions(frmMain.FrameRight, FileFunctions);
end;

procedure TMainCommands.cm_RightSortByAttr(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfAttr);
  DoSortByFunctions(frmMain.FrameRight, FileFunctions);
end;

{ Command to request to sort a frame with a column with a defined order.
  This command may be user by the user via the toolbar,
  but it is definitively a nice-to-have for the "uHotDir" unit who may specify the order to be in when switching to a hotdir.}
procedure TMainCommands.cm_UniversalSingleDirectSort(const Params: array of string);
var
  Param: String;
  sValue: String;
  WantedFileView: TFileView;
  WantedSortFunction: TFileFunction;
  WantedSortDirection: TSortDirection;
  FileFunctions: TFileFunctions = nil;
  NewSorting: TFileSortings = nil;
begin
  //1o) Let's set our default values
  WantedFileView:=frmMain.ActiveFrame;
  WantedSortFunction:=fsfName;
  WantedSortDirection:=sdAscending;

  //2o) Let's parse the parameter to get the wanted ones
  for Param in Params do
  begin
    if GetParamValue(Param, 'panel', sValue) then
    begin
      if sValue='inactive' then WantedFileView:=frmMain.NotActiveFrame else
        if sValue='left' then WantedFileView:=frmMain.FrameLeft else
          if sValue='right' then WantedFileView:=frmMain.FrameRight;
    end
    else if GetParamValue(Param, 'column', sValue) then
    begin
      if sValue='ext' then WantedSortFunction:=fsfExtension else
        if sValue='size' then WantedSortFunction:=fsfSize else
          if sValue='datetime' then WantedSortFunction:=fsfModificationTime;
    end
    else if GetParamValue(Param, 'order', sValue) then
    begin
      if sValue='descending' then WantedSortDirection:=sdDescending;
    end;
  end;

  //3o) Let's do the sorting job now!
  AddSortFunction(FileFunctions, WantedSortFunction);
  SetLength(NewSorting, 1);
  SetLength(NewSorting[0].SortFunctions, 1);
  NewSorting[0].SortFunctions[0] := FileFunctions[0];
  NewSorting[0].SortDirection := WantedSortDirection;
  WantedFileView.Sorting := NewSorting;
end;

procedure TMainCommands.cm_MultiRename(const Params: array of string);
var
  aFiles: TFiles;
begin
  with frmMain do
  begin
    if not (fsoSetFileProperty in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    aFiles:= ActiveFrame.CloneSelectedOrActiveFiles;
    if Assigned(aFiles) then
      try
        if aFiles.Count > 0 then
          ShowMultiRenameForm(ActiveFrame.FileSource, aFiles)
        else
          msgWarning(rsMsgNoFilesSelected);
      finally
        FreeAndNil(aFiles);
      end;
  end;
end;

//------------------------------------------------------

procedure TMainCommands.cm_CopySamePanel(const Params: array of string);
begin
  frmMain.CopyFiles('', True);
end;

procedure TMainCommands.cm_RenameOnly(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_RenameOnly', Params);
end;

procedure TMainCommands.cm_EditNew(const Params: array of string);
var
  sNewFile: String;
  hFile: System.THandle = 0;
  aFile: TFile;
  Attrs: TFileAttrs;
  sCmd: string = '';
  sParams: string = '';
  sStartPath: string = '';
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_EditNew', Params);

  // For now only works for FileSystem.

  with frmMain do
  if ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
  begin
    aFile := ActiveFrame.CloneActiveFile;
    if Assigned(aFile) then
    try
      if aFile.IsNameValid then
        sNewFile:= aFile.Name
      else
        sNewFile:= rsEditNewFile;
    finally
      FreeAndNil(aFile);
    end;

    if not InputQuery(rsEditNewOpen, rsEditNewFileName, sNewFile) then Exit;

    // If user entered only a filename prepend it with current directory.
    if ExtractFilePath(sNewFile) = '' then
      sNewFile:= ActiveFrame.CurrentPath + sNewFile;

    Attrs := mbFileGetAttr(sNewFile);
    if Attrs = faInvalidAttributes then
    begin
      hFile := mbFileCreate(sNewFile);
      if hFile = feInvalidHandle then
      begin
        MessageDlg(rsMsgErrECreate, mbSysErrorMessage(GetLastOSError), mtWarning, [mbOK], 0);
        Exit;
      end;
      FileClose(hFile);
      ActiveFrame.FileSource.Reload(ExtractFilePath(sNewFile));
    end
    else if FPS_ISDIR(Attrs) then
    begin
      MessageDlg(rsMsgErrECreate, Format(rsMsgErrCreateFileDirectoryExists,
        [ExtractFileName(sNewFile)]), mtWarning, [mbOK], 0);
      Exit;
    end;

    aFile := TFileSystemFileSource.CreateFileFromFile(sNewFile);
    try
      // Try to find Edit command in "extassoc.xml"
      if not gExts.GetExtActionCmd(aFile, 'edit', sCmd, sParams, sStartPath) then
        ShowEditorByGlob(aFile.FullPath) // If command not found then use default editor
      else
        begin
          ProcessExtCommandFork(sCmd, sParams, aFile.Path, aFile);
        end;
    finally
      FreeAndNil(aFile);
    end;
  end
  else
    msgWarning(rsMsgNotImplemented);
end;

{ TMainCommands.cm_DirHistory }
// Shows recently visited directories (global).
procedure TMainCommands.cm_DirHistory(const Params: array of string);
var
  bUseTreeViewMenu: boolean = false;
  bUsePanel: boolean = true;
  p: TPoint = (x:0; y:0);
  iWantedWidth: integer = 0;
  iWantedHeight: integer = 0;
  sMaybeMenuItem: TMenuItem = nil;
begin
  // 1. Let's parse our parameters.
  DoParseParametersForPossibleTreeViewMenu(Params, gUseTreeViewMenuWithDirHistory, gUseTreeViewMenuWithDirHistory, bUseTreeViewMenu, bUsePanel, p);

  frmMain.CreatePopUpDirHistory;
  Application.ProcessMessages; //TODO: In Windows, Not sure why, but on all systems tried, this eliminate a "beep" when the popup is shown.

  if bUseTreeViewMenu then
  begin
    if not bUsePanel then
      iWantedHeight := ((frmMain.ActiveFrame.ClientToScreen(Classes.Point(0, 0)).y + frmMain.ActiveFrame.Height) - p.y)
    else
    begin
      iWantedWidth := frmMain.ActiveFrame.Width;
      iWantedHeight := frmMain.ActiveFrame.Height;
    end;
    sMaybeMenuItem := GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(frmMain.pmDirHistory, tvmcDirHistory, p.X, p.Y, iWantedWidth, iWantedHeight);
    if sMaybeMenuItem <> nil then sMaybeMenuItem.OnClick(sMaybeMenuItem);
  end
  else
  begin
    frmMain.pmDirHistory.Popup(p.X,p.Y);
  end;
end;

// Shows browser-like history for active file view.
procedure TMainCommands.cm_ViewHistory(const Params: array of string);
begin
  frmMain.ShowFileViewHistory(Params);
end;

procedure TMainCommands.cm_ViewHistoryPrev(const Params: array of string);
begin
  with frmMain do
  begin
    ActiveFrame.GoToPrevHistory;
  end;
end;

procedure TMainCommands.cm_ViewHistoryNext(const Params: array of string);
begin
  with frmMain do
  begin
    ActiveFrame.GoToNextHistory;
  end;
end;

{ TMainCommands.cm_ShowCmdLineHistory }
procedure TMainCommands.cm_ShowCmdLineHistory(const Params: array of string);
var
  p: TPoint = (x:0; y:0);
  sUserChoice:string;
  bUseTreeViewMenu: boolean = false;
  bUsePanel: boolean = true;
  iWantedWidth: integer = 0;
  iWantedHeight: integer = 0;
begin
  with frmMain do
  begin
    if IsCommandLineVisible then
    begin
      // 1. Let's parse our parameters.
      DoParseParametersForPossibleTreeViewMenu(Params, gUseTreeViewMenuWithCommandLineHistory, gUseTreeViewMenuWithCommandLineHistory, bUseTreeViewMenu, bUsePanel, p);

      // 2. No matter what, we need to fill in the popup menu structure.
      gFavoriteTabsList.PopulateMenuWithFavoriteTabs(frmMain.pmFavoriteTabs, @DoOnClickMenuJobFavoriteTabs, ftmp_FAVTABSWITHCONFIG);
      Application.ProcessMessages;

      // 3. Show the appropriate menu.
      if bUseTreeViewMenu then
      begin
        iWantedWidth := frmMain.edtCommand.Width;
        iWantedHeight := frmMain.ActiveFrame.Height;
        p := frmMain.edtCommand.ClientToScreen(Classes.Point(0, 0));
        p.y := p.y - iWantedHeight;

        sUserChoice := GetUserChoiceFromTStrings(edtCommand.Items, tvmcCommandLineHistory, p.x, p.y, iWantedWidth, iWantedHeight);
        if sUserChoice<>'' then
        begin
          edtCommand.ItemIndex:=edtCommand.Items.IndexOf(sUserChoice);
      edtCommand.SetFocus;
        end;

      end
      else
      begin
        edtCommand.SetFocus;
      if edtCommand.Items.Count>0 then
        edtCommand.DroppedDown:=True;
      end;
    end;

  end;
end;

procedure TMainCommands.cm_ToggleFullscreenConsole(const Params: array of string);
begin
  with frmMain do
  begin
    ToggleFullscreenConsole;
    if IsCommandLineVisible then
    begin
      edtCommand.SetFocus;
    end;
  end;
end;

procedure TMainCommands.cm_RunTerm(const Params: array of string);
begin
  with frmMain do
  if not edtCommand.Focused then
  try
    ProcessExtCommandFork(gRunTermCmd, gRunTermParams, ActiveFrame.CurrentPath);
  except
    on e: EInvalidCommandLine do
      MessageDlg(rsToolErrorOpeningTerminal,
            rsMsgInvalidCommandLine + ' (' + rsToolTerminal + '):' + LineEnding + e.Message,
            mtError, [mbOK], 0);
  end;
end;

procedure TMainCommands.cm_CalculateSpace(const Params: array of string);
var
  SelectedFiles: TFiles;
  Operation: TFileSourceOperation;
begin
  with frmMain do
  begin
    if not (fsoCalcStatistics in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      Operation := ActiveFrame.FileSource.CreateCalcStatisticsOperation(SelectedFiles);
      if not Assigned(Operation) then
        msgWarning(rsMsgErrNotSupported)
      else
        begin
          Operation.AddStateChangedListener([fsosStopped], @OnCalcStatisticsStateChanged);
          OperationsManager.AddOperation(Operation);
        end;
    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_CountDirContent(const Params: array of string);
begin
  frmMain.ActiveFrame.CalculateSpaceOfAllDirectories;
end;

procedure TMainCommands.cm_SetFileProperties(const Params: array of string);
var
  ActiveFile: TFile = nil;
  SelectedFiles: TFiles = nil;
  aFileProperties: TFileProperties;
  CreationTime: DCBasicTypes.TFileTime = 0;
  LastAccessTime : DCBasicTypes.TFileTime = 0;
  ModificationTime: DCBasicTypes.TFileTime = 0;
  Operation: TFileSourceSetFilePropertyOperation = nil;

begin
  with frmMain do
  try
    if not (fsoSetFileProperty in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    ActiveFile := ActiveFrame.CloneActiveFile;
    if Assigned(ActiveFile) and (SelectedFiles.Count > 0) then
      begin
        if fspDirectAccess in ActiveFrame.FileSource.Properties then
        begin
          if mbFileGetTime(ActiveFile.FullPath, ModificationTime, CreationTime, LastAccessTime) then
          begin
            if fpModificationTime in ActiveFile.SupportedProperties then
              ActiveFile.ModificationTime:= FileTimeToDateTime(ModificationTime);
            if fpCreationTime in ActiveFile.SupportedProperties then
              ActiveFile.CreationTime:= FileTimeToDateTime(CreationTime);
            if fpLastAccessTime in ActiveFile.SupportedProperties then
              ActiveFile.LastAccessTime:= FileTimeToDateTime(LastAccessTime);
          end;
        end;
        FillByte(aFileProperties, SizeOf(aFileProperties), 0);
        if fpAttributes in ActiveFile.SupportedProperties then
          aFileProperties[fpAttributes]:= ActiveFile.Properties[fpAttributes].Clone;
        if fpModificationTime in ActiveFile.SupportedProperties then
          aFileProperties[fpModificationTime]:= ActiveFile.Properties[fpModificationTime].Clone;
        if fpCreationTime in ActiveFile.SupportedProperties then
          aFileProperties[fpCreationTime]:= ActiveFile.Properties[fpCreationTime].Clone;
        if fpLastAccessTime in ActiveFile.SupportedProperties then
          aFileProperties[fpLastAccessTime]:= ActiveFile.Properties[fpLastAccessTime].Clone;

        Operation:= ActiveFrame.FileSource.CreateSetFilePropertyOperation(
                        SelectedFiles,
                        aFileProperties) as TFileSourceSetFilePropertyOperation;

        if Assigned(Operation) then
          begin
            if ShowChangeFilePropertiesDialog(Operation) then
              begin
                OperationsManager.AddOperation(Operation);
                Operation := nil; // So it doesn't get destroyed below.
              end;
          end;
      end;
  finally
    FreeAndNil(SelectedFiles);
    FreeAndNil(ActiveFile);
    FreeAndNil(Operation);
  end;
end;

procedure TMainCommands.cm_FileProperties(const Params: array of string);
var
  SelectedFiles: TFiles;
  Operation: TFileSourceExecuteOperation;
  aFile: TFile;
begin
  with frmMain do
  begin
    if ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
      begin
        SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
        if Assigned(SelectedFiles) then
        try
          if SelectedFiles.Count > 0 then
          try
            ShowFilePropertiesDialog(ActiveFrame.FileSource, SelectedFiles);
          except
            on e: EContextMenuException do
              ShowException(e);
          end;
        finally
          FreeAndNil(SelectedFiles);
        end;
      end
    else if (fsoExecute in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        aFile:= ActiveFrame.CloneActiveFile;
        if Assigned(aFile) then
          try
            Operation:= ActiveFrame.FileSource.CreateExecuteOperation(
                            aFile,
                            ActiveFrame.CurrentPath,
                            'properties') as TFileSourceExecuteOperation;
            if Assigned(Operation) then
              Operation.Execute;
          finally
            FreeThenNil(Operation);
            FreeThenNil(aFile);
          end;
      end;
  end;
end;

procedure TMainCommands.cm_FileLinker(const Params: array of string);
var
  I: Integer;
  aSelectedFiles: TFiles = nil;
  aFile: TFile;
  aFirstFilenameOfSeries: String;
begin
  with frmMain, frmMain.ActiveFrame do
  begin
    if not (fsoCombine in FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;
    try
      aSelectedFiles := CloneSelectedOrActiveFiles;

      for I := 0 to aSelectedFiles.Count - 1 do
        begin
          aFile := aSelectedFiles[I];
          if (aFile.IsDirectory or aFile.IsLinkToDirectory) then
          begin
            msgWarning(rsMsgInvalidSelection);
            Exit;
          end;
        end;

      if aSelectedFiles.Count > 1 then
      begin
        ShowLinkerFilesForm(FileSource, aSelectedFiles, NotActiveFrame.CurrentPath);
      end
      else
      begin
        if aSelectedFiles.Count = 1 then
        begin
          try
            if StrToInt(aSelectedFiles[0].Extension)>0 then
            begin
              aFirstFilenameOfSeries:='1';
              while length(aFirstFilenameOfSeries)<length(aSelectedFiles[0].Extension) do aFirstFilenameOfSeries:='0'+aFirstFilenameOfSeries;
              aFirstFilenameOfSeries:=aSelectedFiles[0].Path + aSelectedFiles[0].NameNoExt + ExtensionSeparator + aFirstFilenameOfSeries;
              DoDynamicFilesLinking(FileSource, aSelectedFiles, NotActiveFrame.CurrentPath, aFirstFilenameOfSeries);
            end
            else
            begin
              msgWarning(rsMsgInvalidSelection);
            end;
          except
            msgWarning(rsMsgInvalidSelection);
          end;
        end
        else
        begin
          msgWarning(rsMsgInvalidSelection);
        end;
      end;
    finally
      FreeThenNil(aSelectedFiles);
    end; // try
  end; // with
end;

procedure TMainCommands.cm_FileSpliter(const Params: array of string);
var
  aFile: TFile = nil;
begin
  with frmMain, frmMain.ActiveFrame do
  begin
    if not (fsoSplit in FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    try
      aFile := CloneActiveFile;
      if (not Assigned(aFile)) or (aFile.IsDirectory or aFile.IsLinkToDirectory) then
        msgWarning(rsMsgInvalidSelection)
      else
        ShowSplitterFileForm(FileSource, aFile, NotActiveFrame.CurrentPath);
    finally
      FreeThenNil(aFile);
    end; // try
  end; // with
end;

procedure TMainCommands.cm_PanelsSplitterPerPos(const Params: array of string);
var
  Split: Integer = 50;
  Param, SplitPct: String;
begin
  for Param in Params do
  begin
    if GetParamValue(Param, 'splitpct', SplitPct) then
      Split := StrToIntDef(SplitPct, Split);
  end;
  DoPanelsSplitterPerPos(Split);
end;

procedure TMainCommands.cm_EditComment(const Params: array of string);
var
  aFile: TFile;
begin
  with frmMain.ActiveFrame do
  begin
    if (fspDirectAccess in FileSource.GetProperties) then
      begin
        aFile:= CloneActiveFile;
        if Assigned(aFile) then
          try
            if aFile.IsNameValid then
              ShowDescrEditDlg(aFile.FullPath, frmMain.ActiveFrame)
            else
              msgWarning(rsMsgNoFilesSelected);
          finally
            FreeAndNil(aFile);
          end;
      end
    else if (fspLinksToLocalFiles in FileSource.GetProperties) then
      begin
        aFile:= CloneActiveFile;
        if Assigned(aFile) then
          try
            if aFile.IsNameValid then
              begin
                if FileSource.GetLocalName(aFile) then
                  ShowDescrEditDlg(aFile.FullPath, frmMain.ActiveFrame)
                else
                  msgWarning(rsMsgErrNotSupported);
              end
            else
              begin
                msgWarning(rsMsgNoFilesSelected);
              end;
          finally
            FreeAndNil(aFile);
          end;
      end
    else
      msgWarning(rsMsgErrNotSupported);
  end;
end;

function SendToClipboard(ClipboardMode: uClipboard.TClipboardOperation):Boolean;
var
  sl: TStringList = nil;
  i : Integer;
  theSelectedFiles: TFiles = nil;
begin
  // Only works for file system.

  Result := False;

  with frmMain.ActiveFrame do
  if (fspDirectAccess in FileSource.Properties) then
  begin
    sl := TStringList.Create;
    try
      theSelectedFiles := CloneSelectedOrActiveFiles;

      for i := 0 to theSelectedFiles.Count - 1 do
        sl.Add(theSelectedFiles[i].FullPath);

      case ClipboardMode of
        uClipboard.ClipboardCut:
            Result := uClipboard.CutToClipboard(sl);

        uClipboard.ClipboardCopy:
            Result := uClipboard.CopyToClipboard(sl);
      end;

    finally
      if Assigned(sl) then
        FreeAndNil(sl);
      if Assigned(theSelectedFiles) then
        FreeAndNil(theSelectedFiles);
    end;
  end
  else
    msgWarning(rsMsgErrNotSupported);
end;

procedure TMainCommands.cm_CopyToClipboard(const Params: array of string);
begin
  SendToClipboard(ClipboardCopy);
end;

procedure TMainCommands.cm_CutToClipboard(const Params: array of string);
begin
  SendToClipboard(ClipboardCut);
end;

procedure TMainCommands.cm_PasteFromClipboard(const Params: array of string);
var
  ClipboardOp: TClipboardOperation;
  filenamesList: TStringList;
  Files: TFiles = nil;
  Operation: TFileSourceOperation = nil;
  SourceFileSource: IFileSource = nil;
begin
  with frmMain do
  begin
    if PasteFromClipboard(ClipboardOp, filenamesList) = True then
    try
      // fill file list with files
      Files := TFileSystemFileSource.CreateFilesFromFileList(
          ExtractFilePath(filenamesList[0]), fileNamesList, True);

      if Files.Count > 0 then
      begin
        case ClipboardOp of

          uClipboard.ClipboardCut:
          begin
            SourceFileSource := TFileSystemFileSource.GetFileSource;

            if ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            begin
              if not (fsoMove in ActiveFrame.FileSource.GetOperationsTypes) then
              begin
                msgWarning(rsMsgErrNotSupported);
                Exit;
              end;

              Operation := SourceFileSource.CreateMoveOperation(
                             Files, ActiveFrame.CurrentPath);
            end
            else
            begin
              if (not (fsoCopyIn in ActiveFrame.FileSource.GetOperationsTypes)) or
                 (not (fsoDelete in SourceFileSource.GetOperationsTypes)) then
              begin
                msgWarning(rsMsgErrNotSupported);
                Exit;
              end;
  {
              // Meta-operation: CopyIn + Delete

              Operation := ActiveFrame.FileSource.CreateCopyInOperation(
                             SourceFileSource, Files,
                             ActiveFrame.CurrentPath);
  }
            end;
          end;

          uClipboard.ClipboardCopy:
          begin
            if not (fsoCopyIn in ActiveFrame.FileSource.GetOperationsTypes) then
            begin
              msgWarning(rsMsgErrNotSupported);
              Exit;
            end;

            SourceFileSource := TFileSystemFileSource.GetFileSource;

            if ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            begin
              Operation := SourceFileSource.CreateCopyOutOperation(
                             ActiveFrame.FileSource, Files,
                             ActiveFrame.CurrentPath);
            end
            else
            begin
              Operation := ActiveFrame.FileSource.CreateCopyInOperation(
                             SourceFileSource, Files,
                             ActiveFrame.CurrentPath);
            end;
          end;

          else
            // Invalid clipboard operation.
            Exit;

        end;

        if Assigned(Operation) then
        begin
          if Operation is TFileSystemCopyOperation then
            (Operation as TFileSystemCopyOperation).AutoRenameItSelf:= True;
          OperationsManager.AddOperation(Operation);

          // Files have been moved so clear the clipboard because
          // the files location in the clipboard is invalid now.
          if ClipboardOp = uClipboard.ClipboardCut then
            uClipboard.ClearClipboard;
        end
        else
          msgWarning(rsMsgNotImplemented);
      end;

    finally
      FreeAndNil(fileNamesList);
      if Assigned(Files) then
        FreeAndNil(Files);
    end;
  end;
end;

procedure TMainCommands.cm_ChangeDirToRoot(const Params: array of string);
begin
  DoChangeDirToRoot(frmMain.ActiveFrame);
end;

procedure TMainCommands.cm_ChangeDirToHome(const Params: array of string);
begin
  SetFileSystemPath(frmMain.ActiveFrame, GetHomeDir);
end;

procedure TMainCommands.cm_ChangeDirToParent(const Params: array of string);
begin
  frmMain.ActiveFrame.ChangePathToParent(True);
end;

// Parameters:
// Full path to a directory.
procedure TMainCommands.cm_ChangeDir(const Params: array of string);
var
  Param, WantedPath: string;
begin
  //1o) Let's set our default values
  WantedPath := frmMain.ActiveFrame.CurrentPath;

  //2o) Let's parse the parameter to get the wanted ones
  for Param in Params do
  begin
    if GetParamValue(Param, 'activepath', WantedPath) then
    begin
      WantedPath:= PrepareParameter(WantedPath);
      ChooseFileSource(frmMain.ActiveFrame, RemoveQuotation(WantedPath));
    end
    else
    if GetParamValue(Param, 'inactivepath', WantedPath) then
    begin
      WantedPath:= PrepareParameter(WantedPath);
      ChooseFileSource(frmMain.NotActiveFrame, RemoveQuotation(WantedPath));
    end
    else
    if GetParamValue(Param, 'leftpath', WantedPath) then
    begin
      WantedPath:= PrepareParameter(WantedPath);
      ChooseFileSource(frmMain.FrameLeft, RemoveQuotation(WantedPath));
    end
    else
    if GetParamValue(Param, 'rightpath', WantedPath) then
    begin
      WantedPath:=PrepareParameter(WantedPath);
      ChooseFileSource(frmMain.FrameRight, RemoveQuotation(WantedPath));
    end;
  end;

  //3o) Let's support the DC legacy way of working of the command
  if Length(Params)=1 then
  begin
    if (not GetParamValue(Params[0], 'activepath', WantedPath)) AND (not GetParamValue(Params[0], 'inactivepath', WantedPath)) AND (not GetParamValue(Params[0], 'leftpath', WantedPath)) AND (not GetParamValue(Params[0], 'rightpath', WantedPath)) then
      ChooseFileSource(frmMain.ActiveFrame, RemoveQuotation(ReplaceEnvVars(Params[0])));
  end;
end;

procedure TMainCommands.cm_ClearLogWindow(const Params: array of string);
begin
  frmMain.seLogWindow.Lines.Clear;
end;

procedure TMainCommands.cm_CmdLineNext(const Params: array of string);
begin
  DoShowCmdLineHistory(True);
end;

procedure TMainCommands.cm_CmdLinePrev(const Params: array of string);
begin
  DoShowCmdLineHistory(False);
end;

procedure TMainCommands.cm_ViewLogFile(const Params: array of string);
begin
  ShowViewerByGlob(GetActualLogFilename);
end;

procedure TMainCommands.cm_ClearLogFile(const Params: array of string);
begin
  if MsgBox(Format(rsMsgPopUpHotDelete,['log file ('+GetActualLogFilename+')']),[msmbYes, msmbNo], msmbNo, msmbNo ) = mmrYes then
  begin
    mbDeleteFile(GetActualLogFilename);
  end;
end;

procedure TMainCommands.cm_NetworkConnect(const Params: array of string);
begin
  DoOpenVirtualFileSystemList(frmMain.ActiveFrame);
end;

procedure TMainCommands.cm_NetworkDisconnect(const Params: array of string);
begin
  CloseNetworkConnection();
end;

procedure TMainCommands.cm_HorizontalFilePanels(const Params: array of string);
var
  sParamValue:string;
  WantedHorizontalFilePanels:boolean;
begin
  WantedHorizontalFilePanels:=gHorizontalFilePanels;

  if Length(Params)>0 then
  begin
    if GetParamValue(Params[0], 'mode', sParamValue) then
    begin
      if sParamValue='legacy' then WantedHorizontalFilePanels := not WantedHorizontalFilePanels else
        if sParamValue='vertical' then WantedHorizontalFilePanels:=FALSE else
          if sParamValue='horizontal' then WantedHorizontalFilePanels:=TRUE;
    end;
  end
  else
  begin
    WantedHorizontalFilePanels := not WantedHorizontalFilePanels;
  end;

  if WantedHorizontalFilePanels<>gHorizontalFilePanels then
  begin
    gHorizontalFilePanels:=WantedHorizontalFilePanels;
    frmMain.actHorizontalFilePanels.Checked := gHorizontalFilePanels;
    frmMain.UpdateWindowView;
  end;
end;

procedure TMainCommands.cm_OperationsViewer(const Params: array of string);
begin
  ShowOperationsViewer;
end;

procedure TMainCommands.cm_CompareDirectories(const Params: array of string);
var
  I: LongWord;
  NtfsShift: Boolean;
  SourceFile: TDisplayFile;
  TargetFile: TDisplayFile;
  SourceList: TStringHashList;
  SourceFiles: TDisplayFiles = nil;
  TargetFiles: TDisplayFiles = nil;
begin
  SourceList:= TStringHashList.Create(FileNameCaseSensitive);
  with frmMain do
  try
    NtfsShift:= gNtfsHourTimeDelay and NtfsHourTimeDelay(ActiveFrame.CurrentPath, NotActiveFrame.CurrentPath);
    SourceFiles:= ActiveFrame.DisplayFiles;
    TargetFiles:= NotActiveFrame.DisplayFiles;
    for I:= 0 to SourceFiles.Count - 1 do
    begin
      SourceFile:= SourceFiles[I];
      if SourceFile.FSFile.IsDirectory or SourceFile.FSFile.IsLinkToDirectory then
        Continue;
      ActiveFrame.MarkFile(SourceFile, True);
      SourceList.Add(SourceFile.FSFile.Name, SourceFile);
    end;
    for I:= 0 to TargetFiles.Count - 1 do
    begin
      TargetFile:= TargetFiles[I];
      if TargetFile.FSFile.IsDirectory or TargetFile.FSFile.IsLinkToDirectory then
        Continue;
      SourceFile:= TDisplayFile(SourceList.Data[TargetFile.FSFile.Name]);
      if (SourceFile = nil) then
        NotActiveFrame.MarkFile(TargetFile, True)
      else
        case FileTimeCompare(SourceFile.FSFile.ModificationTime, TargetFile.FSFile.ModificationTime, NtfsShift) of
          0:
            ActiveFrame.MarkFile(SourceFile, False);
          +1:
            NotActiveFrame.MarkFile(TargetFile, False);
          -1:
            begin
              ActiveFrame.MarkFile(SourceFile, False);
              NotActiveFrame.MarkFile(TargetFile, True);
            end;
        end;
    end;
  finally
    SourceList.Free;
    ActiveFrame.Repaint;
    NotActiveFrame.Repaint;
  end;
end;

{ TMainCommands.cm_ConfigToolbars }
procedure TMainCommands.cm_ConfigToolbars(const Params: array of string);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  Options := ShowOptions(TfrmOptionsToolbar);
  Application.ProcessMessages;
  Editor := Options.GetEditor(TfrmOptionsToolbar);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
end;

{ TMainCommands.cm_DebugShowCommandParameters }
procedure TMainCommands.cm_DebugShowCommandParameters(const Params: array of string);
var
  sMessageToshow:string;
  indexParameter:integer;
begin
  sMessageToshow:='Number of parameters: '+IntToStr(Length(Params));
  if Length(Params)>0 then
  begin
    sMessageToshow:=sMessageToshow+#$0A;
    for indexParameter:=0 to pred(Length(Params)) do
    begin
      sMessageToshow:=sMessageToshow+#$0A+'Parameter #'+IntToStr(indexParameter)+': '+Params[indexParameter]+' ==> '+PrepareParameter(Params[indexParameter]);
    end;
  end;
  msgOK(sMessageToshow);
end;

{ TMainCommands.cm_CopyPathOfFilesToClip }
procedure TMainCommands.cm_CopyPathOfFilesToClip(const Params: array of string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, cfntcJustPathWithSeparator);
end;

{ TMainCommands.cm_CopyPathNoSepOfFilesToClip }
procedure TMainCommands.cm_CopyPathNoSepOfFilesToClip(const Params: array of string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, cfntcPathWithoutSeparator);
end;

{ TMainCommands.cm_DoAnyCmCommand }
procedure TMainCommands.cm_DoAnyCmCommand(const Params: array of string);
var
  CommandReturnedToExecute:string='';
begin
  if ShowMainCommandDlgForm(gLastDoAnyCommand,CommandReturnedToExecute) then
  begin
    gLastDoAnyCommand := CommandReturnedToExecute;
    frmMain.Commands.Commands.ExecuteCommand(CommandReturnedToExecute, []);
  end;
end;

{ TMainCommands.DoCloseAllTabs }
procedure TMainCommands.DoCloseAllTabs(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  iPage: integer;
begin
  for iPage := ANotebook.PageCount - 1 downto 0 do
    if (not bAbort) AND (iPage <> ANotebook.PageIndex) then
      case frmMain.RemovePage(ANotebook, iPage, bDoLocked, iAskForLocked, True) of
        1: Continue; // skip tab
        2: bAbort := True; // cancel operation
        3: iAskForLocked := 2; // user answered to delete them all, we won't ask anymore during the rest of this command
      end;
end;

{ TMainCommands.DoCloseDuplicateTabs }
// Close tabs pointing to same dirs so at the end of action, only one tab for each dir is kept.
// Tabs that are kept follow these rules of priority:
//   -All the locked tabs are kept without asking question *except* if "bDoLocked" is set, which means we want also to elimit double lock tab.
//   -The one that has been user renamed by the user are eliminate IF a equivalent locked tab exist.
//   -If a user rename tab point the same directoy as another tab but not renamed, no matter the order, we keep the renamed tab and eliminate the other.
//   -A locked renamed tabs is stronger than a non-renamed tab locked so we eliminate the second one, the one not renamed.
//   -If two equals importance identical exist, we keep the one on left and elimitate the one on right.
// At the end of the process, we stay in a tab that has the same path as where we were initally.
procedure TMainCommands.DoCloseDuplicateTabs(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  sOriginalPath: String;
  iTabIndex, jTabIndex, jScore, tScore: Integer;
  bFlagDeleted: boolean;
begin
  // 1. We save to restore later the original directory of the active tab.
  sOriginalPath := ANoteBook.Page[ANotebook.PageIndex].FileView.CurrentPath;

  // 2. We do the actual job.
  jTabIndex := pred(ANotebook.PageCount);
  while (not bAbort) AND (jTabIndex>0) do
  begin
    jScore:=$0;
    if (ANoteBook.Page[jTabIndex].PermanentTitle <> '') then jScore := (jScore OR $01);
    if (ANoteBook.Page[jTabIndex].LockState <> tlsNormal) then jScore := (jScore OR $02);

    iTabIndex := (jTabIndex-1);
    bFlagDeleted := FALSE;
    while (not bAbort) AND (iTabIndex>=0) AND (bFlagDeleted=FALSE) do
    begin
      if mbCompareFileNames(ANoteBook.Page[iTabIndex].FileView.CurrentPath, ANoteBook.Page[jTabIndex].FileView.CurrentPath) then
      begin
        tScore:=jScore;
        if (ANoteBook.Page[iTabIndex].PermanentTitle <> '') then tScore := (tScore OR $04);
        if (ANoteBook.Page[iTabIndex].LockState <> tlsNormal) then tScore := (tScore OR $08);

        case tScore of
          $00, $04, $05, $08, $09, $0C, $0D: // We eliminate the one on right.
            begin
              frmMain.RemovePage(ANotebook, jTabIndex, False);
              bFlagDeleted:=TRUE;
            end;

          $01, $02, $03, $06, $07: // We eliminate the one on left.
            begin
              frmMain.RemovePage(ANotebook, iTabIndex, False);
              dec(jTabIndex); // If we eliminate one on left, the right tab now moved one position lower, we must take this in account.
            end;

          $0A, $0E, $0F: // We eliminate the one on right, EVEN if it is locked if specified.
            begin
              if bDoLocked then
              begin
                case frmMain.RemovePage(ANotebook, jTabIndex, bDoLocked, iAskForLocked, True) of
                  0: bFlagDeleted:=True; // Standard Removed.
                  1: begin end; // Skip tab, we keep going.
                  2: bAbort := True; // Cancel operation!
                  3:
                    begin
                      iAskForLocked := 2; // user answered to delete them all, we won't ask anymore during the rest of this command
                      bFlagDeleted:=True;
                    end;
                end;


              end;
            end;

          $0B: // We eliminate the one on left, EVEN if it is locked, if specified.
            begin
              if bDoLocked then
              begin
                case frmMain.RemovePage(ANotebook, iTabIndex, bDoLocked, iAskForLocked, True) of
                  0: dec(jTabIndex); // If we eliminate one on left, the right tab now moved one position lower, we must take this in account.
                  1: begin end; // Skip tab, we keep going.
                  2: bAbort := True; // Cancel operation!
                  3:
                    begin
                      iAskForLocked := 2; // user answered to delete them all, we won't ask anymore during the rest of this command
                      dec(jTabIndex);
                    end;
                end;
              end;
            end;
        end; // case tScore
      end;
      dec(iTabIndex);
    end;
    dec(jTabIndex);
  end;

  // 3. We attempt to select a tab with the actual original path from where we were.
  if not mbCompareFileNames(ANoteBook.Page[ANotebook.PageIndex].FileView.CurrentPath , sOriginalPath) then
  begin
    iTabIndex:=0;
    while (iTabIndex<ANotebook.PageCount) do
      if mbCompareFileNames(ANoteBook.Page[iTabIndex].FileView.CurrentPath , sOriginalPath) then
      begin
        ANotebook.PageIndex:=iTabIndex;
        iTabIndex:=ANotebook.PageCount;
      end
      else
        inc(iTabIndex);
  end;
end;

{ TMainCommands.DoSetAllTabsOptionNormal }
procedure TMainCommands.DoSetAllTabsOptionNormal(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  iPage: integer;
begin
  for iPage:=0 to pred(ANoteBook.PageCount) do
    ANoteBook.Page[iPage].LockState:=tlsNormal;
end;

{ TMainCommands.DoSetAllTabsOptionPathLocked }
procedure TMainCommands.DoSetAllTabsOptionPathLocked(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  iPage: integer;
begin
  for iPage:=0 to pred(ANoteBook.PageCount) do
    ANoteBook.Page[iPage].LockState:=tlsPathLocked;
end;

{ TMainCommands.DoAllTabsOptionPathResets }
procedure TMainCommands.DoAllTabsOptionPathResets(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  iPage: integer;
begin
  for iPage:=0 to pred(ANoteBook.PageCount) do
    ANoteBook.Page[iPage].LockState:=tlsPathResets;
end;

{ TMainCommands.DoSetAllTabsOptionDirsInNewTab }
procedure TMainCommands.DoSetAllTabsOptionDirsInNewTab(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  iPage: integer;
begin
  for iPage:=0 to pred(ANoteBook.PageCount) do
    ANoteBook.Page[iPage].LockState:=tlsDirsInNewTab;
end;

{ TMainCommands.cm_SetAllTabsOptionNormal }
procedure TMainCommands.cm_SetAllTabsOptionNormal(const Params: array of string);
begin
  DoActionOnMultipleTabs(Params,@DoSetAllTabsOptionNormal);
end;

{ TMainCommands.cm_SetAllTabsOptionPathLocked }
procedure TMainCommands.cm_SetAllTabsOptionPathLocked(const Params: array of string);
begin
  DoActionOnMultipleTabs(Params,@DoSetAllTabsOptionPathLocked);
end;

{ TMainCommands.cm_SetAllTabsOptionPathResets }
procedure TMainCommands.cm_SetAllTabsOptionPathResets(const Params: array of string);
begin
  DoActionOnMultipleTabs(Params,@DoAllTabsOptionPathResets);
end;

{ TMainCommands.cm_SetAllTabsOptionDirsInNewTab }
procedure TMainCommands.cm_SetAllTabsOptionDirsInNewTab(const Params: array of string);
begin
  DoActionOnMultipleTabs(Params,@DoSetAllTabsOptionDirsInNewTab);
end;

{ TMainCommands.DoActionOnMultipleTabs }
procedure TMainCommands.DoActionOnMultipleTabs(const Params: array of string; ProcedureDoingActionOnMultipleTabs: TProcedureDoingActionOnMultipleTabs);
var
  originalFilePanel:TFilePanelSelect;
  SideOfAction : TTabsConfigLocation = tclActive;
  bAbort: boolean = False;
  bDoLocked: boolean = False;
  iAskForLocked: integer = 0;
  iCurrentPageCount: integer;
begin
  FOriginalNumberOfTabs := -1;
  originalFilePanel := frmMain.SelectedPanel;
  GetAndSetMultitabsActionFromParams(Params, SideOfAction, bDoLocked, iAskForLocked);

  case SideOfAction of
    tclLeft: ProcedureDoingActionOnMultipleTabs(frmMain.LeftTabs, bAbort, bDoLocked, iAskForLocked);
    tclRight: ProcedureDoingActionOnMultipleTabs(frmMain.RightTabs, bAbort, bDoLocked, iAskForLocked);
    tclActive: ProcedureDoingActionOnMultipleTabs(frmMain.ActiveNotebook, bAbort, bDoLocked, iAskForLocked);
    tclInactive: ProcedureDoingActionOnMultipleTabs(frmMain.NotActiveNotebook, bAbort, bDoLocked, iAskForLocked);
    tclBoth:
      begin
        iCurrentPageCount := frmMain.RightTabs.PageCount;
        ProcedureDoingActionOnMultipleTabs(frmMain.LeftTabs, bAbort, bDoLocked, iAskForLocked);
        FOriginalNumberOfTabs := iCurrentPageCount;
        ProcedureDoingActionOnMultipleTabs(frmMain.RightTabs, bAbort, bDoLocked, iAskForLocked);
      end;
  end;

  frmMain.SelectedPanel := originalFilePanel;
  frmMain.ActiveFrame.SetFocus;
end;

{ TMainCommands.GetAndSetMultitabsActionFromParams }
procedure TMainCommands.GetAndSetMultitabsActionFromParams(const Params: array of string; var APanelSide:TTabsConfigLocation; var ActionOnLocked:boolean; var AskForLocked:integer);
var
 Param, sValue: String;
 boolValue: boolean;
begin
  Param := GetDefaultParam(Params);
  ActionOnLocked := False;
  AskForLocked := 0;

  // 1. Evaluate if we're running from legacy parameter style.
  if pos('=',Param)=0 then
  begin
    // 1.a. If yes, just watch for the magic word
    if Param = 'LeftTabs' then APanelSide := tclLeft
    else if Param = 'RightTabs' then APanelSide := tclRight
    else if Param = 'ActiveTabs' then APanelSide := tclActive
    else if Param = 'InactiveTabs' then APanelSide := tclInactive
    else if Param = 'BothTabs' then APanelSide := tclBoth
    else APanelSide := tclActive; // Legacy default is to close from Active Notebook.
  end
  else
  begin
    // 1.b. If no, let's parse it.

    // 2. Let's set our default values.
    APanelSide := tclActive;

    // 3. Parsing may now begin!
    for Param in Params do
    begin
      if GetParamValue(Param, 'side', sValue) then
      begin
        if sValue = 'left' then APanelSide := tclLeft
        else if sValue = 'right' then APanelSide := tclRight
        else if sValue = 'active' then APanelSide := tclActive
        else if sValue = 'inactive' then APanelSide := tclInactive
        else if sValue = 'both' then APanelSide := tclBoth;
      end
      else if GetParamBoolValue(Param, 'dolocked', boolValue) then ActionOnLocked := boolValue
      else if GetParamBoolValue(Param, 'confirmlocked', boolValue) then
      begin
        if boolValue then AskForLocked:=1 else AskForLocked:=2;
      end;
    end;
  end;
end;

{ TMainCommands.cm_ConfigFolderTabs }
procedure TMainCommands.cm_ConfigFolderTabs(const Params: array of string);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  Options := ShowOptions(TfrmOptionsTabs);
  Application.ProcessMessages;
  Editor := Options.GetEditor(TfrmOptionsTabs);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
end;

{ TMainCommands.DoShowFavoriteTabsOptions }
procedure TMainCommands.DoShowFavoriteTabsOptions;
var
  Options: IOptionsDialog;
  Editor: TOptionsEditor;
begin
  Options := ShowOptions(TfrmOptionsFavoriteTabs);
  Editor := Options.GetEditor(TfrmOptionsFavoriteTabs);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
  TfrmOptionsFavoriteTabs(Editor).MakeUsInPositionToWorkWithActiveFavoriteTabs;
end;

{ TMainCommands.cm_ConfigFavoriteTabs }
procedure TMainCommands.cm_ConfigFavoriteTabs(const Params: array of string);
begin
  DoShowFavoriteTabsOptions;
end;

{ TMainCommands.cm_ResaveFavoriteTabs }
procedure TMainCommands.cm_ResaveFavoriteTabs(const Params: array of string);
begin
  if gFavoriteTabsList.ReSaveTabsToXMLEntry(gFavoriteTabsList.GetIndexLastFavoriteTabsLoaded) then
    if gFavoriteTabsGoToConfigAfterReSave then
      DoShowFavoriteTabsOptions;
end;

{ TMainCommands.cm_SaveFavoriteTabs }
procedure TMainCommands.cm_SaveFavoriteTabs(const Params: array of string);
var
  sFavoriteTabsEntryName: string = '';
begin
  if gFavoriteTabsList.GetSuggestedParamsForFavoriteTabs(frmMain.ActiveNotebook.ActivePage.CurrentTitle, sFavoriteTabsEntryName) then
    if gFavoriteTabsList.SaveNewEntryFavoriteTabs(sFavoriteTabsEntryName) then
      if gFavoriteTabsGoToConfigAfterSave then
        DoShowFavoriteTabsOptions;
end;

{ TMainCommands.DoOnClickMenuJobFavoriteTabs }
//  We're supposed to jump here when we get called by an item from a favorite tabs menu item.
//  The value of "tag" will indicate us what to do.
//  -A value below TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING ($10000) indicates we want to load tabs with what was saved in our favorite tabs list at the index of "tag".
//  -A value equal or higher than TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING ($10000) indicates we want to save our current tabs OVER the existing entry in our favorite tabs list at the index of "tag MOD TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING".
procedure TMainCommands.DoOnClickMenuJobFavoriteTabs(Sender: TObject);
begin
  with Sender as TComponent do
  begin
    case tag of
      0 .. pred(TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING):  // We want to adjust our current tab from one in the favorite tabs.
      begin
        gFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange;
        gFavoriteTabsList.LoadTabsFromXmlEntry(tag);
      end;

      TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING .. pred(TAGOFFSET_FAVTABS_SOMETHINGELSE): // We want to save our current tabs an existing favorite tabs entry.
        if gFavoriteTabsList.ReSaveTabsToXMLEntry(tag mod TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING) then
          if gFavoriteTabsGoToConfigAfterReSave then
            DoShowFavoriteTabsOptions;
    end; //case iDispatcher of ...
  end;
end;

{ TMainCommands.cm_ReloadFavoriteTabs }
procedure TMainCommands.cm_ReloadFavoriteTabs(const Params: array of string);
begin
  // Here we won't call "gFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange;" because if user wants to reload it, it's because he does not want to save what he has right now...
  // Otherwise, it would be a useless action. :-/
  gFavoriteTabsList.LoadTabsFromXmlEntry(gFavoriteTabsList.GetIndexLastFavoriteTabsLoaded);
end;

{ TMainCommands.cm_PreviousFavoriteTabs }
procedure TMainCommands.cm_PreviousFavoriteTabs(const Params: array of string);
begin
  gFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange;
  gFavoriteTabsList.LoadTabsFromXmlEntry(gFavoriteTabsList.GetIndexPreviousLastFavoriteTabsLoaded);
end;

{ TMainCommands.cm_NextFavoriteTabs }
procedure TMainCommands.cm_NextFavoriteTabs(const Params: array of string);
begin
  gFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange;
  gFavoriteTabsList.LoadTabsFromXmlEntry(gFavoriteTabsList.GetIndexNextLastFavoriteTabsLoaded);
end;

{ TMainCommands.cm_LoadFavoriteTabs }
procedure TMainCommands.cm_LoadFavoriteTabs(const Params: array of string);
var
  bUseTreeViewMenu: boolean = false;
  bUsePanel: boolean = true;
  p: TPoint = (x:0; y:0);
  iWantedWidth: integer = 0;
  iWantedHeight: integer = 0;
  sMaybeMenuItem: TMenuItem = nil;
begin
  // 1. Let's parse our parameters.
  DoParseParametersForPossibleTreeViewMenu(Params, gUseTreeViewMenuWithFavoriteTabsFromMenuCommand, gUseTreeViewMenuWithFavoriteTabsFromDoubleClick, bUseTreeViewMenu, bUsePanel, p);

  // 2. No matter what, we need to fill in the popup menu structure.
  gFavoriteTabsList.PopulateMenuWithFavoriteTabs(frmMain.pmFavoriteTabs, @DoOnClickMenuJobFavoriteTabs, ftmp_FAVTABSWITHCONFIG);
  Application.ProcessMessages;

  // 3. Show the appropriate menu.
  if bUseTreeViewMenu then
  begin
    if not bUsePanel then
      iWantedHeight := ((frmMain.ActiveFrame.ClientToScreen(Classes.Point(0, 0)).y + frmMain.ActiveFrame.Height) - p.y)
    else
    begin
      iWantedWidth := frmMain.ActiveFrame.Width;
      iWantedHeight := frmMain.ActiveFrame.Height;
    end;

    sMaybeMenuItem := GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(frmMain.pmFavoriteTabs, tvmcFavoriteTabs, p.X, p.Y, iWantedWidth, iWantedHeight);
    if sMaybeMenuItem <> nil then sMaybeMenuItem.OnClick(sMaybeMenuItem);
  end
  else
  begin
    frmMain.pmFavoriteTabs.Popup(p.X, p.Y);
  end;
end;

{ TMainCommands.DoCopyAllTabsToOppositeSide }
procedure TMainCommands.DoCopyAllTabsToOppositeSide(ANotebook: TFileViewNotebook; var bAbort: boolean; bDoLocked: boolean; var iAskForLocked: integer);
var
  iPage: integer;
  localFileViewPage: TFileViewPage;
  localPath: string;
  TargetNotebook: TFileViewNotebook;
  iPageCountLimit: integer;
begin
  if FOriginalNumberOfTabs <> -1 then iPageCountLimit := FOriginalNumberOfTabs else iPageCountLimit := ANotebook.PageCount;
  if ANotebook = FrmMain.LeftTabs then TargetNotebook := FrmMain.RightTabs else TargetNotebook := FrmMain.LeftTabs;
  for iPage := 0 to pred(iPageCountLimit) do
  begin
    localPath := ANotebook.Page[iPage].FileView.CurrentPath;

    localFileViewPage := TargetNotebook.NewPage(ANotebook.Page[iPage].FileView);
    // Workaround for Search Result File Source
    if localFileViewPage.FileView.FileSource is TSearchResultFileSource then
      SetFileSystemPath(localFileViewPage.FileView, localPath)
    else
      localFileViewPage.FileView.CurrentPath := localPath;
  end;
end;

{ TMainCommands.cm_CopyAllTabsToOpposite }
procedure TMainCommands.cm_CopyAllTabsToOpposite(const Params: array of string);
begin
  DoActionOnMultipleTabs(Params, @DoCopyAllTabsToOppositeSide);
end;

{ TMainCommands.cm_ConfigTreeViewMenus }
procedure TMainCommands.cm_ConfigTreeViewMenus(const {%H-}Params: array of string);
var
  Options: IOptionsDialog;
  Editor: TOptionsEditor;
begin
  Options := ShowOptions(TfrmOptionsTreeViewMenu);
  Editor := Options.GetEditor(TfrmOptionsTreeViewMenu);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
end;


{ TMainCommands.cm_ConfigTreeViewMenusColors }
procedure TMainCommands.cm_ConfigTreeViewMenusColors(const {%H-}Params: array of string);
var
  Options: IOptionsDialog;
  Editor: TOptionsEditor;
begin
  Options := ShowOptions(TfrmOptionsTreeViewMenuColor);
  Editor := Options.GetEditor(TfrmOptionsTreeViewMenuColor);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
end;

{ TMainCommands.cm_ConfigSaveSettings }
procedure TMainCommands.cm_ConfigSaveSettings(const Params: array of string);
begin
  frmMain.ConfigSaveSettings;
end;

{ TMainCommands.cm_ExecuteScript }
procedure TMainCommands.cm_ExecuteScript(const Params: array of string);
var
  FileName: String;
  Index, Count: Integer;
  Args: array of String;
begin
  if Length(Params) > 0 then
  begin
    // Get script file name
    FileName:= PrepareParameter(Params[0]);
    if not mbFileExists(FileName) then Exit;

    // Get script arguments
    Count:= Length(Params) - 1;
    if (Count > 0) then
    begin
      SetLength(Args, Count);
      for Index := 1 to Count do begin
        Args[Index - 1]:= PrepareParameter(Params[Index]);
      end;
    end;

    // Execute script
    ExecuteScript(FileName, Args);
  end;
end;

procedure TMainCommands.cm_FocusSwap(const Params: array of string);
var
  AParam, AValue: String;
begin
  with frmMain do
  begin
    // Select opposite panel
    if Length(Params) = 0 then
    begin
      case SelectedPanel of
        fpLeft: SetActiveFrame(fpRight);
        fpRight: SetActiveFrame(fpLeft);
      end;
    end
    else begin
      AParam:= GetDefaultParam(Params);
      if GetParamValue(AParam, 'side', AValue) then
      begin
        if AValue = 'left' then
          SetActiveFrame(fpLeft)
        else if AValue = 'right' then
          SetActiveFrame(fpRight);
      end;
    end;
  end;
end;

procedure TMainCommands.cm_Benchmark(const Params: array of string);
begin
  OperationsManager.AddOperation(TBenchmarkOperation.Create(frmMain));
end;

{ TMainCommands.cm_AddNewSearch }
procedure TMainCommands.cm_AddNewSearch(const Params: array of string);
var
  TemplateName: String;
begin
  if Length(Params) > 0 then
    TemplateName:= Params[0]
  else begin
    TemplateName:= gSearchDefaultTemplate;
  end;
  ShowFindDlg(frmMain.ActiveFrame, TemplateName, True);
end;

{ TMainCommands.cm_ViewSearches }
procedure TMainCommands.cm_ViewSearches(const {%H-}Params: array of string);
var
  iIndex,iCurrentPage:integer;
  iSelectedWindow: integer = -1;
  slWindowTitleToOffer:TStringList;
  sTitleSelected:string='';
begin
  if ListOffrmFindDlgInstance.Count>0 then
  begin
  slWindowTitleToOffer:=TStringList.Create;

  try
    for iIndex:=0 to pred(ListOffrmFindDlgInstance.count) do
      slWindowTitleToOffer.Add(ListOffrmFindDlgInstance.frmFindDlgInstance[iIndex].Caption);

    if ShowInputListBox(rsListOfFindFilesWindows, rsSelectYouFindFilesWindow, slWindowTitleToOffer,sTitleSelected,iSelectedWindow) then
    begin
      if (iSelectedWindow>-1) AND (iSelectedWindow<ListOffrmFindDlgInstance.Count) then
      begin
        iCurrentPage:=ListOffrmFindDlgInstance.frmFindDlgInstance[iSelectedWindow].pgcSearch.ActivePageIndex;
        ListOffrmFindDlgInstance.frmFindDlgInstance[iSelectedWindow].ShowOnTop;
        ListOffrmFindDlgInstance.frmFindDlgInstance[iSelectedWindow].pgcSearch.ActivePageIndex:=iCurrentPage;
      end;
    end;
  finally
    FreeAndNil(slWindowTitleToOffer);
  end;
  end
  else
  begin
    msgOK(rsNoFindFilesWindowYet);
  end;
end;

{ TMainCommands.cm_DeleteSearches }
procedure TMainCommands.cm_DeleteSearches(const Params: array of string);
var
  iIndex:integer;
begin
  if ListOffrmFindDlgInstance.Count>0 then
  begin
    for iIndex := pred(ListOffrmFindDlgInstance.count) downto 0 do
      ListOffrmFindDlgInstance.frmFindDlgInstance[iIndex].CancelCloseAndFreeMem;
  end
  else
  begin
    msgOK(rsNoFindFilesWindowYet);
  end;
end;

{ TMainCommands.cm_ConfigSearches }
procedure TMainCommands.cm_ConfigSearches(const Params: array of string);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  Options := ShowOptions(TfrmOptionsFileSearch);
  Editor := Options.GetEditor(TfrmOptionsFileSearch);
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
end;

{ TMainCommands.cm_ConfigHotKeys }
procedure TMainCommands.cm_ConfigHotKeys(const Params: array of string);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
  Param, sCategoryName:string;
begin
  sCategoryName:='';
  Options := ShowOptions(TfrmOptionsHotkeys);
  Editor := Options.GetEditor(TfrmOptionsHotkeys);
  Application.ProcessMessages;
  for Param in Params do
    GetParamValue(Param, 'category', sCategoryName);
  TfrmOptionsHotkeys(Editor).TryToSelectThatCategory(sCategoryName);
  if Editor.CanFocus then  Editor.SetFocus;
end;

end.

