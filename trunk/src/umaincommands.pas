{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains DC actions of the main form

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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
  uFileFunctions, uFormCommands;

type

  { TMainCommands }

  TMainCommands = class(TComponent{$IF FPC_FULLVERSION >= 020501}, IFormCommands{$ENDIF})
  private
   FCommands: TFormCommands;

   // Filters out commands.
   function CommandsFilter(Command: String): Boolean;
   procedure OnCalcStatisticsStateChanged(Operation: TFileSourceOperation;
                                          State: TFileSourceOperationState);
   procedure OnCalcChecksumStateChanged(Operation: TFileSourceOperation;
                                        State: TFileSourceOperationState);
   procedure GetCommandFromBar (const cFilename, cParam: string;
                                var Com, Par, Pat:string);

  public
   constructor Create(TheOwner: TComponent; ActionList: TActionList = nil); reintroduce;

   property Commands: TFormCommands read FCommands{$IF FPC_FULLVERSION >= 020501} implements IFormCommands{$ENDIF};

   //---------------------
   // The Do... functions are cm_... functions' counterparts which are to be
   // executed directly from the code with specific - especially non-string
   // - arguments (instead of calling cm_... functions, in which case
   // parameters would have to be converted to and from strings).
   //
   procedure DoRemoveTab(Notebook: TFileViewNotebook; PageIndex: Integer);
   procedure DoCopySelectedFileNamesToClipboard(FileView: TFileView; FullNames: Boolean);
   procedure DoNewTab(Notebook: TFileViewNotebook);
   procedure DoContextMenu(Panel: TFileView; X, Y: Integer; Background: Boolean);
   procedure DoTransferPath(SourcePage: TFileViewPage; TargetPage: TFileViewPage);
   procedure DoSortByFunctions(View: TFileView; FileFunctions: TFileFunctions);
   procedure DoShowMainMenu(bShow: Boolean);
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
   procedure cm_AddPathToCmdLine(Param: String='');
   procedure cm_AddFilenameToCmdLine(Param: String='');
   procedure cm_AddPathAndFilenameToCmdLine(Param: String='');
   procedure cm_ContextMenu(Param: String='');
   procedure cm_CopyFullNamesToClip(Param: String='');
   procedure cm_Exchange(Param: String='');
   procedure cm_OpenArchive(Param: String='');
   procedure cm_TestArchive(Param: String='');
   procedure cm_OpenDirInNewTab(Param: String='');
   procedure cm_Open(Param: String='');
   procedure cm_OpenVirtualFileSystemList(Param: String='');
   procedure cm_TargetEqualSource(Param: String='');
   procedure cm_LeftEqualRight(Param: String='');
   procedure cm_RightEqualLeft(Param: String='');
   procedure cm_PackFiles(Param: String='');
   procedure cm_ExtractFiles(Param: String='');
   procedure cm_QuickSearch(Param: String='');
   procedure cm_QuickFilter(Param: String='');
   procedure cm_LeftOpenDrives(Param: String='');
   procedure cm_RightOpenDrives(Param: String='');
   procedure cm_OpenBar(Param: String='');
   procedure cm_ShowButtonMenu(Param: String='');
   procedure cm_TransferLeft(Param: String='');
   procedure cm_TransferRight(Param: String='');
   procedure cm_GoToFirstFile(Param: String='');
   procedure cm_GoToLastFile(Param: String='');
   procedure cm_Minimize(Param: String='');
   procedure cm_Wipe(Param: String='');
   procedure cm_Exit(Param: String='');
   procedure cm_NewTab(Param: String='');
   procedure cm_RenameTab(Param: String='');
   procedure cm_RemoveTab(Param: String='');
   procedure cm_RemoveAllTabs(Param: String='');
   procedure cm_NextTab(Param: String='');
   procedure cm_PrevTab(Param: String='');
   procedure cm_SetTabOptionNormal(Param: String='');
   procedure cm_SetTabOptionPathLocked(Param: String='');
   procedure cm_SetTabOptionPathResets(Param: String='');
   procedure cm_SetTabOptionDirsInNewTab(Param: String='');
   procedure cm_Copy(Param: String='');
   procedure cm_CopyNoAsk(Param: String='');
   procedure cm_Delete(Param: String='');
   procedure cm_CheckSumCalc(Param: String='');
   procedure cm_CheckSumVerify(Param: String='');
   procedure cm_Edit(Param: String='');
   procedure cm_EditPath(Param: String='');
   procedure cm_MakeDir(Param: String='');
   procedure cm_Rename(Param: String='');
   procedure cm_RenameNoAsk(Param: String='');
   procedure cm_View(Param: String='');
   procedure cm_QuickView(Param: String='');
   procedure cm_CopyNamesToClip(Param: String='');
   procedure cm_FocusCmdLine(Param: String='');
   procedure cm_FileAssoc(Param: String='');
   procedure cm_HelpIndex(Param: String='');
   procedure cm_Keyboard(Param: String='');
   procedure cm_VisitHomePage(Param: String='');
   procedure cm_About(Param: String='');
   procedure cm_ShowSysFiles(Param: String='');
   procedure cm_SwitchIgnoreList(Param: String='');
   procedure cm_Options(Param: String='');
   procedure cm_CompareContents(Param: String='');
   procedure cm_Refresh(Param: String='');
   procedure cm_ShowMainMenu(Param: String='');
   procedure cm_DirHotList(Param: String='');
   procedure cm_MarkInvert(Param: String='');
   procedure cm_MarkMarkAll(Param: String='');
   procedure cm_MarkUnmarkAll(Param: String='');
   procedure cm_MarkPlus(Param: String='');
   procedure cm_MarkMinus(Param: String='');
   procedure cm_MarkCurrentExtension(Param: String='');
   procedure cm_UnmarkCurrentExtension(Param: String='');
   procedure cm_SaveSelection(Param: String='');
   procedure cm_RestoreSelection(Param: String='');
   procedure cm_SaveSelectionToFile(Param: String='');
   procedure cm_LoadSelectionFromFile(Param: String='');
   procedure cm_LoadSelectionFromClip(Param: String='');
   procedure cm_Search(Param: String='');
   procedure cm_HardLink(Param: String='');
   procedure cm_MultiRename(Param: String='');
   procedure cm_ReverseOrder(Param: String='');
   procedure cm_SortByAttr(Param: String='');
   procedure cm_SortByDate(Param: String='');
   procedure cm_SortByExt(Param: String='');
   procedure cm_SortByName(Param: String='');
   procedure cm_SortBySize(Param: String='');
   procedure cm_SymLink(Param: String='');
   procedure cm_CopySamePanel(Param: String='');
   procedure cm_DirHistory(Param: String='');
   procedure cm_ViewHistory(Param: String='');
   procedure cm_ViewHistoryPrev(Param: String='');
   procedure cm_ViewHistoryNext(Param: String='');
   procedure cm_EditNew(Param: String='');
   procedure cm_RenameOnly(Param: String='');
   procedure cm_RunTerm(Param: String='');
   procedure cm_ShowCmdLineHistory(Param: String='');
   procedure cm_CalculateSpace(Param: String='');
   procedure cm_CountDirContent(Param: String='');
   procedure cm_SetFileProperties(Param: String='');
   procedure cm_FileProperties(Param: String='');
   procedure cm_FileLinker(Param: String='');
   procedure cm_FileSpliter(Param: String='');
   procedure cm_PanelsSplitterPerPos(Param: String='');
   procedure cm_EditComment(Param: String='');
   procedure cm_CopyToClipboard(Param: String='');
   procedure cm_CutToClipboard(Param: String='');
   procedure cm_PasteFromClipboard(Param: String='');
   procedure cm_ChangeDirToRoot(Param: String='');
   procedure cm_ChangeDirToParent(Param: String='');
   procedure cm_ChangeDir(Param: String='');
   procedure cm_ClearLogWindow(Param: String='');
   procedure cm_ClearLogFile(Param: String='');
   procedure cm_NetworkConnect(Param: String='');
   procedure cm_NetworkDisconnect(Param: String='');
   procedure cm_HorizontalFilePanels(Param: String='');
   procedure cm_OperationsViewer(Param: String='');

   // Internal commands
   procedure cm_Int_RunCommandFromBarFile(Param: String='');
  end;

implementation

uses Forms, Controls, Dialogs, Clipbrd, strutils, LCLProc, HelpIntfs, dmHelpManager, typinfo,
     fMain, fPackDlg, fFileOpDlg, fMkDir, fFileAssoc, fExtractDlg, fAbout,
     fOptions, fDiffer, fFindDlg, fSymLink, fHardLink, fMultiRename,
     fLinker, fSplitter, fDescrEdit, fCheckSumVerify, fCheckSumCalc, fSetFileProperties,
     fOptionsFrame,
     uGlobs, uLng, uLog, uShowMsg, uOSForms, uOSUtils, uDCUtils, uGlobsPaths,
     uClassesEx, uShowForm, uShellExecute, uClipboard, uHash,
     uFilePanelSelect, uFile, uFileSystemFileSource, uQuickViewPanel,
     uOperationsManager, uFileSourceOperationTypes, uWfxPluginFileSource,
     uFileSystemDeleteOperation, uFileSourceExecuteOperation,
     uFileSourceOperationMessageBoxesUI, uFileSourceCalcChecksumOperation,
     uFileSourceCalcStatisticsOperation, uFileSource, uFileSourceProperty,
     uVfsFileSource, uFileSourceUtil, uArchiveFileSourceUtil,
     uTempFileSystemFileSource, uFileProperty, uFileSourceSetFilePropertyOperation,
     uFileSorting, uShellContextMenu, uTrash, uFileSystemCopyOperation, uFindEx,
     uTypes, fViewOperations, uVfsModule, uMultiListFileSource, uExceptions;

{ TMainCommands }

constructor TMainCommands.Create(TheOwner: TComponent; ActionList: TActionList = nil);
begin
  inherited Create(TheOwner);
  FCommands := TFormCommands.Create(Self, ActionList);
  FCommands.FilterFunc := @CommandsFilter;
end;

function TMainCommands.CommandsFilter(Command: String): Boolean;
begin
  Result := Command = 'cm_Int_RunCommandFromBarFile';
end;

//------------------------------------------------------

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

procedure TMainCommands.DoRemoveTab(Notebook: TFileViewNotebook; PageIndex: Integer);
begin
  with frmMain do
  begin
    RemovePage(Notebook, PageIndex);
    ActiveFrame.SetFocus;
  end;
end;

procedure TMainCommands.DoCopySelectedFileNamesToClipboard(FileView: TFileView; FullNames: Boolean);
var
  I: Integer;
  sl: TStringList = nil;
  SelectedFiles: TFiles = nil;
  PathToAdd: String;
begin
  SelectedFiles := FileView.CloneSelectedFiles;
  try
    if SelectedFiles.Count > 0 then
    begin
      sl := TStringList.Create;
      for I := 0 to SelectedFiles.Count - 1 do
      begin
        if FullNames then
        begin
          // Workaround for not fully implemented TMultiListFileSource.
          if not FileView.FileSource.IsClass(TMultiListFileSource) then
            PathToAdd := FileView.CurrentAddress
          else
            PathToAdd := '';
          PathToAdd := PathToAdd + SelectedFiles[I].Path;
        end
        else
          PathToAdd := '';

        sl.Add(PathToAdd + SelectedFiles[I].Name);
      end;

      Clipboard.Clear;   // prevent multiple formats in Clipboard (specially synedit)
      Clipboard.AsText:= TrimRightLineEnding(sl.Text, sl.TextLineBreakStyle);
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
  if tb_open_new_near_current in gDirTabOptions then
    NewPage := Notebook.InsertPage(Notebook.PageIndex + 1)
  else
    NewPage := Notebook.AddPage;
  Notebook.ActiveView.Clone(NewPage);
  NewPage.MakeActive;
  NewPage.UpdateCaption(GetLastDir(ExcludeTrailingPathDelimiter(NewPage.FileView.CurrentPath)));
end;

procedure TMainCommands.DoContextMenu(Panel: TFileView; X, Y: Integer; Background: Boolean);
var
  aFile: TFile = nil;
  aFiles: TFiles = nil;
  sPath, sName: UTF8String;
begin
  // Temporarily work for Filesystem only.
  with frmMain do
  begin
    if not (fspDirectAccess in Panel.FileSource.Properties) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    if Background then
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
        aFiles:= Panel.CloneSelectedFiles;
      end;

    if Assigned(aFiles) then
    try
      if aFiles.Count > 0 then
      try
        ShowContextMenu(frmMain, aFiles, X, Y, Background, nil);
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

procedure TMainCommands.DoTransferPath(SourcePage: TFileViewPage; TargetPage: TFileViewPage);
var
  aFile: TFile;
  NewPath: String;
begin
  aFile := SourcePage.FileView.CloneActiveFile;
  if Assigned(aFile) then
  try
    if aFile.IsDirectory then
    begin
      if aFile.Name = '..' then
      begin
        NewPath := GetParentDir(SourcePage.FileView.CurrentPath)
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
        if not ChooseFileSource(TargetPage.FileView, aFile) then
          TargetPage.FileView.AddFileSource(SourcePage.FileView.FileSource,
                                            SourcePage.FileView.CurrentPath);
      except
        on e: EFileSourceException do
          MessageDlg('Error', e.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    FreeAndNil(aFile);
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

  if SortDirection = sdNone then
    SortDirection := sdAscending;

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

procedure TMainCommands.cm_AddPathToCmdLine(Param: String='');
var
  OldPosition: Integer;
  AddedString: String;
begin
  with frmMain do
    begin
      OldPosition := edtCommand.SelStart;
      AddedString := QuoteStr(ActiveFrame.CurrentPath);
      edtCommand.Text := edtCommand.Text + AddedString;
      edtCommand.SelStart := OldPosition + Length(AddedString);
    end;
end;

procedure TMainCommands.cm_AddFilenameToCmdLine(Param: String='');
var
  AddedString: String;
  OldPosition: Integer;
  aFile: TFile;
begin
  with frmMain do
    begin
      aFile := ActiveFrame.CloneActiveFile;
      if Assigned(aFile) then
      try
        OldPosition := edtCommand.SelStart;
        AddedString := QuoteStr(aFile.Name) + ' ';
        edtCommand.Text := edtCommand.Text + AddedString;
        edtCommand.SelStart := OldPosition + Length(AddedString);
      finally
        FreeAndNil(aFile);
      end;
    end;
end;

procedure TMainCommands.cm_AddPathAndFilenameToCmdLine(Param: String='');
var
  AddedString: String;
  OldPosition: Integer;
  aFile: TFile;
begin
  with frmMain do
    begin
      aFile := ActiveFrame.CloneActiveFile;
      if Assigned(aFile) then
      try
        if aFile.Name = '..' then
          AddedString := QuoteStr(ActiveFrame.CurrentPath) + ' '
        else
          AddedString := QuoteStr(aFile.FullPath) + ' ';

        OldPosition := edtCommand.SelStart;
        edtCommand.Text := edtCommand.Text + AddedString;
        edtCommand.SelStart := OldPosition + Length(AddedString);
      finally
        FreeAndNil(aFile);
      end;
    end;
end;

procedure TMainCommands.cm_ContextMenu(Param: String='');
begin
  // Let file view handle displaying context menu at appropriate position.
  frmMain.ActiveFrame.ExecuteCommand('cm_ContextMenu', '');
end;

procedure TMainCommands.cm_CopyFullNamesToClip(Param: String='');
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, True);
end;

procedure TMainCommands.cm_CopyNamesToClip(Param: String='');
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, False);
end;

//------------------------------------------------------
procedure TMainCommands.cm_Exchange(Param: String='');
var
  sDir: String;
begin
  sDir:= FrmMain.ActiveFrame.CurrentPath;
  FrmMain.ActiveFrame.CurrentPath:= FrmMain.NotActiveFrame.CurrentPath;
  FrmMain.NotActiveFrame.CurrentPath:= sDir;
end;

procedure TMainCommands.cm_OpenDirInNewTab(Param: String='');
var
  NewPage: TFileViewPage;
  NewPath: String;
  aFile: TFile;
begin
  with FrmMain do
  begin
    aFile := ActiveFrame.CloneActiveFile;
    if Assigned(aFile) then
    try
      if aFile.IsNameValid and
         (aFile.IsDirectory or aFile.IsLinkToDirectory) then
      begin
        NewPath := ActiveFrame.CurrentPath + aFile.Name;
        NewPage := ActiveNotebook.AddPage;
        ActiveFrame.Clone(NewPage);
        NewPage.FileView.CurrentPath := NewPath;
        if tb_open_new_in_foreground in gDirTabOptions then
          NewPage.MakeActive;
      end;
    finally
      FreeAndNil(aFile);
    end;
  end;
end;

procedure TMainCommands.cm_TargetEqualSource(Param: String='');
begin
  with frmMain do
  begin
    NotActiveNotebook.ActivePage.FileView := nil;
    ActiveFrame.Clone(NotActiveNotebook.ActivePage);
    NotActiveNotebook.ActivePage.UpdateCaption(GetLastDir(
      ExcludeTrailingPathDelimiter(NotActiveNotebook.ActivePage.FileView.CurrentPath)));
  end;
end;

procedure TMainCommands.cm_LeftEqualRight(Param: String='');
begin
  with frmMain do
  begin
    LeftTabs.ActivePage.FileView := nil;
    FrameRight.Clone(LeftTabs.ActivePage);
    LeftTabs.ActivePage.UpdateCaption(GetLastDir(ExcludeTrailingPathDelimiter(LeftTabs.ActivePage.FileView.CurrentPath)));

    // Destroying active view may have caused losing focus. Restore it if needed.
    if SelectedPanel = fpLeft then
      FrameLeft.SetFocus;
  end;
end;

procedure TMainCommands.cm_RightEqualLeft(Param: String='');
begin
  with frmMain do
  begin
    RightTabs.ActivePage.FileView := nil;
    FrameLeft.Clone(RightTabs.ActivePage);
    RightTabs.ActivePage.UpdateCaption(GetLastDir(ExcludeTrailingPathDelimiter(RightTabs.ActivePage.FileView.CurrentPath)));

    // Destroying active view may have caused losing focus. Restore it if needed.
    if SelectedPanel = fpRight then
      FrameRight.SetFocus;
  end;
end;

procedure TMainCommands.cm_OpenArchive(Param: String='');
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
          ChooseArchive(frmMain.ActiveFrame, aFile, True);
      end;
    finally
      FreeAndNil(aFile);
    end;
  end;
end;

procedure TMainCommands.cm_TestArchive(Param: String='');
var
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      TestArchive(ActiveFrame, SelectedFiles);
    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_Open(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_Open', param);
end;

procedure TMainCommands.cm_OpenVirtualFileSystemList(Param: String='');
var
  FileSource: IFileSource;
begin
  with frmMain do
  begin
    if (gWFXPlugins.Count = 0) and (gVfsModuleList.Count = 0) then Exit;
    FileSource:= TVfsFileSource.Create(gWFXPlugins);
    if Assigned(FileSource) then
    begin
      if dskLeft.Name = param then
        FrameLeft.AddFileSource(FileSource, FileSource.GetRootDir)
      else if dskRight.Name = param then
        FrameRight.AddFileSource(FileSource, FileSource.GetRootDir)
      else
        ActiveFrame.AddFileSource(FileSource, FileSource.GetRootDir)
    end;
  end;
end;

//------------------------------------------------------
(* Pack files in archive by creating a new archive *)
procedure TMainCommands.cm_PackFiles(Param: String='');
var
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      ShowPackDlg(ActiveFrame.FileSource,
                  nil, // No specific target (create new)
                  SelectedFiles,
                  NotActiveFrame.CurrentPath,
                  PathDelim { Copy to root of archive } {NotActiveFrame.FileSource.GetRootString}
                 );

    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

// This command is needed for extracting whole archive by Alt+F9 (without opening it).
procedure TMainCommands.cm_ExtractFiles(Param: String='');
var
  SelectedFiles: TFiles;
begin
  with frmMain do
  begin
    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    if Assigned(SelectedFiles) then
    try
      if SelectedFiles.Count > 0 then
        ShowExtractDlg(ActiveFrame.FileSource, SelectedFiles,
                       NotActiveFrame.FileSource, NotActiveFrame.CurrentPath)
      else
        msgWarning(rsMsgNoFilesSelected);

    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_QuickSearch(Param: String='');
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickSearch', param);
end;

procedure TMainCommands.cm_QuickFilter(Param: String='');
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickFilter', param);
end;

procedure TMainCommands.cm_LeftOpenDrives(Param: String='');
begin
  frmMain.ShowDrivesList(fpLeft);
end;

procedure TMainCommands.cm_RightOpenDrives(Param: String='');
begin
  frmMain.ShowDrivesList(fpRight);
end;

procedure TMainCommands.cm_OpenBar(Param: String='');
var
  sFileName: UTF8String;
  IniFile: TIniFileEx = nil;
begin
  if param = EmptyStr then Exit;
  with frmMain do
  try
    if Pos(PathDelim, param) <> 0 then
      sFileName:= GetCmdDirFromEnvVar(param)
    else
      sFileName:= gpCfgDir + param;
    IniFile:= TIniFileEx.Create(sFileName);
    MainToolBar.AutoSize:= False;
    MainToolBar.LoadFromIniFile(IniFile);
  finally
    MainToolBar.AutoSize:= True;
    FreeThenNil(IniFile);
  end;
end;

procedure TMainCommands.cm_ShowButtonMenu(Param: String='');
var
  Point: TPoint;
  sFileName: UTF8String;
  IniFile: TIniFileEx = nil;
begin
  if param = EmptyStr then Exit;
  with frmMain do
  try
    if Pos(PathDelim, param) <> 0 then
      sFileName:= GetCmdDirFromEnvVar(param)
    else
      sFileName:= gpCfgDir + param;
    IniFile:= TIniFileEx.Create(sFileName);
    pmButtonMenu.LoadFromIniFile(IniFile);
    Point:=MainToolBar.ClientToScreen(Classes.Point(0,0));
    Point.Y:=Point.Y+MainToolbar.Height;
    Point.X:=mouse.CursorPos.X-60;
    pmButtonMenu.PopUp(Point.x,Point.Y);
  finally
    FreeThenNil(IniFile);
  end;
end;

procedure TMainCommands.cm_TransferLeft(Param: String='');
begin
  if (frmMain.SelectedPanel = fpRight) then
    DoTransferPath(frmMain.RightTabs.ActivePage,
                   frmMain.LeftTabs.ActivePage);
end;

procedure TMainCommands.cm_TransferRight(Param: String='');
begin
  if (frmMain.SelectedPanel = fpLeft) then
    DoTransferPath(frmMain.LeftTabs.ActivePage,
                   frmMain.RightTabs.ActivePage);
end;

procedure TMainCommands.cm_GoToFirstFile(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_GoToFirstFile', '');
end;

procedure TMainCommands.cm_GoToLastFile(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_GoToLastFile', '');
end;

procedure TMainCommands.cm_Minimize(Param: String='');
begin
  FrmMain.MinimizeWindow;
end;

procedure TMainCommands.cm_Wipe(Param: String='');
var
  theFilesToWipe: TFiles;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
begin
  with frmMain.ActiveFrame do
  begin
    if not (fsoWipe in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    // ------------------------------------------------------

    theFilesToWipe := CloneSelectedFiles; // free at Thread end by thread

    if Assigned(theFilesToWipe) then
    try
      if theFilesToWipe.Count = 0 then
        Exit;

      if not msgYesNo(frmMain.GetFileDlgStr(rsMsgWipeSel, rsMsgWipeFlDr, theFilesToWipe)) then
        Exit;

      Operation := FileSource.CreateWipeOperation(theFilesToWipe);

      if Assigned(Operation) then
      begin
        // Start operation.
        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
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

procedure TMainCommands.cm_Exit(Param: String='');
begin
  frmMain.Close; // application.Terminate not save settings.
end;

procedure TMainCommands.cm_NewTab(Param: String='');
begin
  DoNewTab(frmMain.ActiveNotebook);
end;

procedure TMainCommands.cm_RenameTab(Param: String='');
var
  sCaption: UTF8String;
begin
  with frmMain do
  begin
    sCaption:= ActiveNotebook.Page[ActiveNotebook.PageIndex].Caption;
    if (Length(sCaption) > 0)  and (sCaption[1] = '*')  and
       (ActiveNotebook.Page[ActiveNotebook.PageIndex].LockState <> tlsNormal) and
       (tb_show_asterisk_for_locked in gDirTabOptions) then
      UTF8Delete(sCaption, 1, 1);
    if InputQuery(rsMsgTabRenameCaption, rsMsgTabRenamePrompt, sCaption) then
      ActiveNotebook.Page[ActiveNotebook.PageIndex].UpdateCaption(sCaption);
  end;
end;

procedure TMainCommands.cm_RemoveTab(Param: String='');
begin
  with frmMain do
    DoRemoveTab(ActiveNotebook, ActiveNotebook.PageIndex);
end;

procedure TMainCommands.cm_RemoveAllTabs(Param: String='');
var
  I: Integer;
  ANotebook: TFileViewNotebook;
begin
  with frmMain do
  begin
    if (tb_confirm_close_all in gDirTabOptions) then
      if not msgYesNo(rsMsgCloseAllInActiveTabs) then Exit;

    if param = 'LeftTabs' then
      ANotebook := LeftTabs
    else if param = 'RightTabs' then
      ANotebook := RightTabs
    else
      ANotebook := ActiveNotebook;

    for I := ANotebook.PageCount - 1 downto 0 do
      if I <> ANotebook.PageIndex then
        case RemovePage(ANotebook, I) of
          1: Continue; // skip tab
          2: Break;    // cancel operation
        end;

    ActiveFrame.SetFocus;
  end;
end;

procedure TMainCommands.cm_NextTab(Param: String='');
begin
  frmMain.ActiveNotebook.ActivateNextTab;
end;

procedure TMainCommands.cm_PrevTab(Param: String='');
begin
  frmMain.ActiveNotebook.ActivatePrevTab;
end;

procedure TMainCommands.cm_SetTabOptionNormal(Param: String='');
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsNormal;
end;

procedure TMainCommands.cm_SetTabOptionPathLocked(Param: String='');
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsPathLocked;
end;

procedure TMainCommands.cm_SetTabOptionPathResets(Param: String='');
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsPathResets;
end;

procedure TMainCommands.cm_SetTabOptionDirsInNewTab(Param: String='');
begin
  with frmMain.ActiveNotebook.ActivePage do
    LockState := tlsDirsInNewTab;
end;

//------------------------------------------------------

procedure TMainCommands.cm_View(Param: String='');
var
  sl: TStringList = nil;
  i, n: Integer;
  sViewCmd: String;
  ActiveFile: TFile = nil;
  AllFiles: TFiles = nil;
  SelectedFiles: TFiles = nil;
  TempFiles: TFiles = nil;
  aFile: TFile;
  TempFileSource: ITempFileSystemFileSource = nil;
  Operation: TFileSourceOperation;
  aFileSource: IFileSource;
begin
  with frmMain do
  try
    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    ActiveFile := ActiveFrame.CloneActiveFile;

    // Enter directories using View command.
    if Assigned(ActiveFile) and
       (ActiveFile.IsDirectory or ActiveFile.IsLinkToDirectory) then
    begin
      ActiveFrame.ExecuteCommand('cm_Open');
      Exit;
    end;

    if SelectedFiles.Count = 0 then
    begin
      msgWarning(rsMsgNoFilesSelected);
      Exit;
    end;

    // Default to using the file source directly.
    aFileSource := ActiveFrame.FileSource;

    // If files are links to local files
    if (fspLinksToLocalFiles in ActiveFrame.FileSource.Properties) then
      begin
        for I := 0 to SelectedFiles.Count - 1 do
          begin
            aFile := SelectedFiles[I];
            ActiveFrame.FileSource.GetLocalName(aFile);
          end;
      end
    // If files not directly accessible copy them to temp file source.
    else if not (fspDirectAccess in ActiveFrame.FileSource.Properties) then
    begin
      if not (fsoCopyOut in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

      TempFiles := SelectedFiles.Clone;

      TempFileSource := TTempFileSystemFileSource.GetFileSource;

      Operation := ActiveFrame.FileSource.CreateCopyOutOperation(
                       TempFileSource,
                       TempFiles,
                       TempFileSource.FileSystemRoot);

      if Assigned(Operation) then
      begin
        Operation.Execute;
        FreeAndNil(Operation);

        aFileSource := TempFileSource;
        ChangeFileListRoot(TempFileSource.FileSystemRoot, SelectedFiles);
      end
      else
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;
    end;

    sl := TStringList.Create;
    for i := 0 to SelectedFiles.Count - 1 do
    begin
      aFile := SelectedFiles[i];

      if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
      begin
        if (log_info in gLogOptions) then
          logWrite('View.Add: ' + aFile.FullPath, lmtInfo);

        //now test if exists View command in doublecmd.ext :)
        sViewCmd:= gExts.GetExtActionCmd(aFile, 'view');

        if (sViewCmd<>'') then
          begin
            sViewCmd := PrepareParameter(sViewCmd, aFile);
            ProcessExtCommand(sViewCmd, ActiveFrame.CurrentPath);
            // TODO:
            // If TempFileSource is used, create a wait thread that will
            // keep the TempFileSource alive until the command is finished.
          end
        else
          begin
            sl.Add(aFile.FullPath);
          end;
      end; // if selected
    end; // for

    // If only one file was selected then add all files in panel to the list.
    // Works only for directly accessible files and only when using internal viewer.
    if (sl.Count=1) and
       (not gExternalTools[etViewer].Enabled) and
       ([fspDirectAccess, fspLinksToLocalFiles] * ActiveFrame.FileSource.Properties <> []) then
      begin
        AllFiles := ActiveFrame.CloneFiles;

        if (fspLinksToLocalFiles in ActiveFrame.FileSource.Properties) then
          begin
            for I := 0 to AllFiles.Count - 1 do
              begin
                aFile := AllFiles[I];
                ActiveFrame.FileSource.GetLocalName(aFile);
              end;
          end;

        n:=0;
        for i := 0 to AllFiles.Count - 1 do
          begin
            aFile := AllFiles[i];
            if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
              begin
                if n>0 then sl.Add(aFile.FullPath);
                if aFile.Name = ActiveFile.Name then n:=i;
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

  finally
    if Assigned(sl) then
      FreeAndNil(sl);
    if Assigned(AllFiles) then
      FreeAndNil(AllFiles);
    if Assigned(SelectedFiles) then
      FreeAndNil(SelectedFiles);
    if Assigned(TempFiles) then
      FreeAndNil(TempFiles);
    if Assigned(ActiveFile) then
      FreeAndNil(ActiveFile);
  end;
end;

procedure TMainCommands.cm_QuickView(Param: String='');
begin
  with frmMain do
  begin
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

procedure TMainCommands.cm_Edit(Param: String='');
var
  i: Integer;
  sEditCmd: String;
  aFile: TFile;
  SelectedFiles: TFiles = nil;
begin
  with frmMain do
  try
    // If files are links to local files
    if (fspLinksToLocalFiles in ActiveFrame.FileSource.Properties) then
      begin
        SelectedFiles := ActiveFrame.CloneSelectedFiles;
        for I := 0 to SelectedFiles.Count - 1 do
          begin
            aFile := SelectedFiles[I];
            ActiveFrame.FileSource.GetLocalName(aFile);
          end;
      end
    // If files not directly accessible copy them to temp file source.
    else if not (fspDirectAccess in ActiveFrame.FileSource.Properties) then
      begin
        msgWarning(rsMsgNotImplemented);
        Exit;
      end
    else
      begin
        SelectedFiles := ActiveFrame.CloneSelectedFiles;
      end;

    for i := 0 to SelectedFiles.Count - 1 do
    begin
      aFile := SelectedFiles[i];

      // For now we only process one file.
      if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
      begin
        //now test if exists View command in doublecmd.ext :)
        sEditCmd:= gExts.GetExtActionCmd(aFile, 'edit');

        if (sEditCmd <> '') then
          begin
            sEditCmd := PrepareParameter(sEditCmd, aFile);
            ProcessExtCommand(sEditCmd, aFile.Path);
          end
        else
          begin
            ShowEditorByGlob(aFile.FullPath);
          end;
        Break;
      end;
    end;

  finally
    if Assigned(SelectedFiles) then
      FreeAndNil(SelectedFiles);
  end;
end;

procedure TMainCommands.cm_EditPath(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_EditPath', param);
end;

procedure TMainCommands.cm_Copy(Param: String='');
begin
  if frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, True) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_CopyNoAsk(Param: String='');
begin
  if frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, False) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_Rename(Param: String='');
begin
  if frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, True) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_RenameNoAsk(Param: String='');
begin
  if frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, False) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_MakeDir(Param: String='');
var
  sPath: UTF8String;
  ActiveFile: TFile = nil;
  Operation: TFileSourceOperation = nil;
  UI: TFileSourceOperationMessageBoxesUI = nil;
begin
  with frmMain do
  try
    if not (fsoCreateDirectory in ActiveFrame.FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    ActiveFile := ActiveFrame.CloneActiveFile;
    if Assigned(ActiveFile) and ActiveFile.IsNameValid then
      sPath := ActiveFile.Name // 21.05.2009 - pass name from cursor to makedir form
    else
      sPath := EmptyStr;

    if not frmMkDir.ShowMkDir(sPath) then Exit;   // show makedir dialog
    if (sPath = EmptyStr) then Exit;

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
// "recycle"           - delete to trash can
// "norecycle"         - delete directly
// "recyclesetting"    - if gUseTrash then delete to trash, otherwise delete directly
// "recyclesettingrev" - if gUseTrash then delete directly, otherwise delete to trash
// no parameter        - depends on gUseTrash
procedure TMainCommands.cm_Delete(Param: String='');
var
  theFilesToDelete: TFiles;
  // 12.05.2009 - if delete to trash, then show another messages
  MsgDelSel, MsgDelFlDr : string;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
  bRecycle: Boolean = False;
begin
  with frmMain.ActiveFrame do
  begin
    if not (fsoDelete in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    if (((gUseTrash = True) and ((param = '') or (param = 'recyclesetting'))) or
        ((gUseTrash = False) and (param = 'recyclesettingrev')) or
        (param = 'recycle')) and
       FileSource.IsClass(TFileSystemFileSource) and
       mbCheckTrash(CurrentPath) then
    begin
      bRecycle := True;
    end;

    // 12.05.2009
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

    // ------------------------------------------------------

    theFilesToDelete := CloneSelectedFiles; // free at Thread end by thread

    if Assigned(theFilesToDelete) then
    try
      if theFilesToDelete.Count = 0 then
        Exit;

      if not msgYesNo(frmMain.GetFileDlgStr(MsgDelSel, MsgDelFlDr, theFilesToDelete)) then
        Exit;

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
        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
      end
      else
      begin
        msgWarning(rsMsgNotImplemented);
      end;

    finally
      if Assigned(theFilesToDelete) then
        FreeAndNil(theFilesToDelete);
    end;
  end;
end;

procedure TMainCommands.cm_CheckSumCalc(Param: String='');
var
  I: Integer;
  bSeparateFile: Boolean;
  HashAlgorithm: THashAlgorithm;
  sFileName: UTF8String;
  SelectedFiles: TFiles;
  Operation: TFileSourceCalcChecksumOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      if SelectedFiles.Count = 0 then
      begin
        msgWarning(rsMsgNoFilesSelected);
        Exit;
      end;

      bSeparateFile:= False;
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

      if ShowCalcCheckSum(sFileName, bSeparateFile, HashAlgorithm) then
      begin
        Operation := ActiveFrame.FileSource.CreateCalcChecksumOperation(
                       SelectedFiles, ActiveFrame.CurrentPath, sFileName) as TFileSourceCalcChecksumOperation;

        if Assigned(Operation) then
        begin
          Operation.Mode := checksum_calc;
          Operation.OneFile := not bSeparateFile;
          Operation.Algorithm := HashAlgorithm;

          // Start operation.
          OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

          ProgressDialog := TfrmFileOp.Create(OperationHandle);
          ProgressDialog.Show;
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

procedure TMainCommands.cm_CheckSumVerify(Param: String='');
var
  I: Integer;
  SelectedFiles: TFiles;
  Operation: TFileSourceCalcChecksumOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      if SelectedFiles.Count = 0 then
      begin
        msgWarning(rsMsgNoFilesSelected);
        Exit;
      end;

      for I := 0 to SelectedFiles.Count - 1 do // find files in selection
        if not FileExtIsHash(SelectedFiles[I].Extension) then
        begin
          msgError(rsMsgSelectOnlyCheckSumFiles);
          Exit;
        end;

      Operation := ActiveFrame.FileSource.CreateCalcChecksumOperation(
                     SelectedFiles, '', '') as TFileSourceCalcChecksumOperation;

      if Assigned(Operation) then
      begin
        Operation.AddStateChangedListener([fsosStopped], @OnCalcChecksumStateChanged);
        Operation.Mode := checksum_verify;

        // Start operation.
        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
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

procedure TMainCommands.cm_FocusCmdLine(Param: String='');
begin
  if frmMain.edtCommand.Visible then
  begin
    // Show temporarily command line on user request.
    if (not gCmdLine) and (frmMain.IsCommandLineVisible = False) then
    begin
      frmMain.pnlCommand.Show;
    end;

    frmMain.edtCommand.SetFocus;
  end;
end;

procedure TMainCommands.cm_FileAssoc(Param: String='');
begin
  ShowFileAssocDlg;
end;

procedure TMainCommands.cm_HelpIndex(Param: String='');
begin
  ShowHelpOrErrorForKeyword('', '/index.html');
end;

procedure TMainCommands.cm_Keyboard(Param: String='');
begin
  ShowHelpOrErrorForKeyword('', '/shortcuts.html');
end;

procedure TMainCommands.cm_VisitHomePage(Param: String='');
var
  ErrMsg: String;
begin
  dmHelpMgr.HTMLHelpDatabase.ShowURL('http://doublecmd.sourceforge.net','Double Commander Web Site', ErrMsg);
end;

procedure TMainCommands.cm_About(Param: String='');
begin
  ShowAboutBox;
end;

procedure TMainCommands.cm_ShowSysFiles(Param: String='');
begin
  with frmMain do
  begin
    uGlobs.gShowSystemFiles:= not uGlobs.gShowSystemFiles;
    actShowSysFiles.Checked:= uGlobs.gShowSystemFiles;
    //repaint both panels
    FrameLeft.Reload;
    FrameRight.Reload;
  end;
end;

procedure TMainCommands.cm_SwitchIgnoreList(Param: String='');
begin
  with frmMain do
  begin
    gIgnoreListFileEnabled:= not gIgnoreListFileEnabled;
    actSwitchIgnoreList.Checked:= gIgnoreListFileEnabled;
    //repaint both panels
    FrameLeft.Reload;
    FrameRight.Reload;
  end;
end;

// Parameter is name of TOptionsEditorClass.
procedure TMainCommands.cm_Options(Param: String='');
var
  frmOptions: TfrmOptions = nil;
begin
  if param = '' then
    frmOptions := TfrmOptions.Create(Application)
  else
    frmOptions := TfrmOptions.Create(Application, param);

  try
    frmOptions.ShowModal;
  finally
    frmOptions.Free;
  end;
end;

procedure TMainCommands.cm_CompareContents(Param: String='');
var
  FilesToCompare: TStringList = nil;
  DirsToCompare: TStringList = nil;

  procedure AddItem(const aFile: TFile);
  begin
    if not aFile.IsDirectory then
      FilesToCompare.Add(aFile.Path + aFile.Name)
    else
      DirsToCompare.Add(aFile.Path + aFile.Name);
  end;

  procedure RunExtDiffer(CompareList: TStringList);
  var
    i : Integer;
    sCommand: String;
  begin
    with gExternalTools[etDiffer] do
    begin
      sCommand := QuoteStr(ReplaceEnvVars(Path));
      if Parameters <> EmptyStr then
        sCommand := sCommand + ' ' + Parameters;
      for i := 0 to CompareList.Count - 1 do
        sCommand := sCommand + ' ' + QuoteStr(CompareList.Strings[i]);
      ExecCmdFork(sCommand, RunInTerminal, '', KeepTerminalOpen);
    end;
  end;

var
  i : Integer;
  ActiveSelectedFiles: TFiles = nil;
  NotActiveSelectedFiles: TFiles = nil;
begin
  //Maybe this will not be dependant on file view but file source.
  //But will work only for non-virtual file sources.
  //frmMain.ActiveFrame.ExecuteCommand('cm_CompareContents', param);

  with frmMain do
  begin
    // For now work only for filesystem.
    // Later use temporary file system for other file sources.

    try
      FilesToCompare := TStringList.Create;
      DirsToCompare := TStringList.Create;

      if param = 'dir' then
      begin
        DirsToCompare.Add(FrameLeft.CurrentPath);
        DirsToCompare.Add(FrameRight.CurrentPath);
      end
      else
      begin
        // For now work only for filesystem.
        if not (ActiveFrame.FileSource.IsClass(TFileSystemFileSource)) then
        begin
          msgWarning(rsMsgNotImplemented);
          Exit;
        end;

        try
          ActiveSelectedFiles := ActiveFrame.CloneSelectedFiles;

          if ActiveSelectedFiles.Count = 1 then
          begin
            NotActiveSelectedFiles := NotActiveFrame.CloneSelectedFiles;

            if NotActiveSelectedFiles.Count = 1 then
            begin
              // For now work only for filesystem.
              if not (NotActiveFrame.FileSource.IsClass(TFileSystemFileSource)) then
              begin
                msgWarning(rsMsgNotImplemented);
                Exit;
              end;

              { compare single selected files in both panels }

              AddItem(ActiveSelectedFiles[0]);
              AddItem(NotActiveSelectedFiles[0]);
            end
            else
            begin
              // Only one file selected in active panel.
              MsgWarning(rsMsgInvalidSelection);
              Exit;
            end;
          end
          else if ActiveSelectedFiles.Count > 1 then
          begin
            { compare all selected files in active frame }

            for i := 0 to ActiveSelectedFiles.Count - 1 do
              AddItem(ActiveSelectedFiles[i]);
          end;

        finally
          if Assigned(ActiveSelectedFiles) then
            FreeAndNil(ActiveSelectedFiles);
          if Assigned(NotActiveSelectedFiles) then
            FreeAndNil(NotActiveSelectedFiles);
        end;
      end;

      if ((FilesToCompare.Count > 0) and (DirsToCompare.Count > 0))
      or ((FilesToCompare.Count = 1) or (DirsToCompare.Count = 1)) then
      begin
         // Either files or directories must be selected and more than one.
         MsgWarning(rsMsgInvalidSelection)
      end
      else if FilesToCompare.Count > 0 then
      begin
        if gExternalTools[etDiffer].Enabled then
          RunExtDiffer(FilesToCompare)
        else if FilesToCompare.Count = 2 then
          ShowDiffer(FilesToCompare.Strings[0], FilesToCompare.Strings[1])
        else
          MsgWarning(rsMsgTooManyFilesSelected);
      end
      else if DirsToCompare.Count > 0 then
      begin
        if gExternalTools[etDiffer].Enabled then
          RunExtDiffer(DirsToCompare)
        else
          MsgWarning(rsMsgNotImplemented);
      end
      else
        msgWarning(rsMsgNoFilesSelected);

    finally
      if Assigned(FilesToCompare) then
        FreeAndNil(FilesToCompare);
      if Assigned(DirsToCompare) then
        FreeAndNil(DirsToCompare);
    end;
  end;
end;

procedure TMainCommands.cm_ShowMainMenu(Param: String='');
begin
  DoShowMainMenu(not gMainMenu);
end;

procedure TMainCommands.cm_Refresh(Param: String='');
begin
  frmMain.ActiveFrame.Reload;
end;

//------------------------------------------------------

procedure TMainCommands.cm_MarkInvert(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkInvert', param);
end;

procedure TMainCommands.cm_MarkMarkAll(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkMarkAll', param);
end;

procedure TMainCommands.cm_MarkUnmarkAll(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkUnmarkAll', param);
end;

procedure TMainCommands.cm_MarkPlus(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkPlus', param);
end;

procedure TMainCommands.cm_MarkMinus(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkMinus', param);
end;

procedure TMainCommands.cm_MarkCurrentExtension(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkCurrentExtension', param);
end;

procedure TMainCommands.cm_UnmarkCurrentExtension(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_UnmarkCurrentExtension', param);
end;

procedure TMainCommands.cm_SaveSelection(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_SaveSelection', param);
end;

procedure TMainCommands.cm_RestoreSelection(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_RestoreSelection', param);
end;

procedure TMainCommands.cm_SaveSelectionToFile(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_SaveSelectionToFile', param);
end;

procedure TMainCommands.cm_LoadSelectionFromFile(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_LoadSelectionFromFile', param);
end;

procedure TMainCommands.cm_LoadSelectionFromClip(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_LoadSelectionFromClip', param);
end;

//------------------------------------------------------

procedure TMainCommands.cm_DirHotList(Param: String='');
var
  p:TPoint;
begin
  frmMain.CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p:=frmMain.ActiveFrame.ClientToScreen(Classes.Point(0,0));
  frmMain.pmHotList.Popup(p.X,p.Y);
end;

procedure TMainCommands.cm_Search(Param: String='');
begin
  ShowFindDlg(frmMain.ActiveFrame.CurrentPath);
end;


//------------------------------------------------------

procedure TMainCommands.cm_SymLink(Param: String='');
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      if SelectedFiles.Count > 1 then
        msgWarning(rsMsgTooManyFilesSelected)
      else if SelectedFiles.Count = 0 then
        msgWarning(rsMsgNoFilesSelected)
      else
      begin
        sExistingFile := SelectedFiles[0].Path + SelectedFiles[0].Name;

        if param <> '' then
          sLinkToCreate := param
        else
        begin
          if NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            sLinkToCreate := NotActiveFrame.CurrentPath
          else
            sLinkToCreate := ActiveFrame.CurrentPath
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

procedure TMainCommands.cm_HardLink(Param: String='');
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      if SelectedFiles.Count > 1 then
        msgWarning(rsMsgTooManyFilesSelected)
      else if SelectedFiles.Count = 0 then
        msgWarning(rsMsgNoFilesSelected)
      else
      begin
        sExistingFile := SelectedFiles[0].Path + SelectedFiles[0].Name;

        if param <> '' then
          sLinkToCreate := param
        else
        begin
          if NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
            sLinkToCreate := NotActiveFrame.CurrentPath
          else
            sLinkToCreate := ActiveFrame.CurrentPath
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
procedure TMainCommands.cm_ReverseOrder(Param: String='');
begin
  with frmMain.ActiveFrame do
    Sorting := ReverseSortDirection(Sorting);
end;

procedure TMainCommands.cm_SortByName(Param: String='');
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfName);
  AddSortFunction(FileFunctions, fsfNameNoExtension);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortByExt(Param: String='');
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfExtension);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortBySize(Param: String='');
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfSize);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortByDate(Param: String='');
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfModificationTime);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_SortByAttr(Param: String='');
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfAttr);
  DoSortByFunctions(frmMain.ActiveFrame, FileFunctions);
end;

procedure TMainCommands.cm_MultiRename(Param: String='');
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

    aFiles:= ActiveFrame.CloneSelectedFiles;
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

procedure TMainCommands.cm_CopySamePanel(Param: String='');
begin
  frmMain.CopyFiles('', True);
end;

procedure TMainCommands.cm_RenameOnly(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_RenameOnly', param);
end;

procedure TMainCommands.cm_EditNew(Param: String='');
var
  sNewFile: String;
  hFile: System.THandle = 0;
  aFile: TFile;
  Attrs: TFileAttrs;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_EditNew', param);

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

    ShowEditorByGlob(sNewFile);
  end
  else
    msgWarning(rsMsgNotImplemented);
end;

// Shows recently visited directories (global).
procedure TMainCommands.cm_DirHistory(Param: String='');
var
  p:TPoint;
begin
  frmMain.CreatePopUpDirHistory;
  p:=frmMain.ActiveFrame.ClientToScreen(Classes.Point(0,0));
  frmMain.pmDirHistory.Popup(p.X,p.Y);
end;

// Shows browser-like history for active file view.
procedure TMainCommands.cm_ViewHistory(Param: String='');
begin
  frmMain.ShowFileViewHistory;
end;

procedure TMainCommands.cm_ViewHistoryPrev(Param: String='');
begin
  with frmMain do
  begin
    ActiveFrame.GoToPrevHistory;
  end;
end;

procedure TMainCommands.cm_ViewHistoryNext(Param: String='');
begin
  with frmMain do
  begin
    ActiveFrame.GoToNextHistory;
  end;
end;

procedure TMainCommands.cm_ShowCmdLineHistory(Param: String='');
begin
  with frmMain do
  begin
    if IsCommandLineVisible then
    begin
      edtCommand.SetFocus;
      if edtCommand.Items.Count>0 then
        edtCommand.DroppedDown:=True;
    end;
  end;
end;

procedure TMainCommands.cm_RunTerm(Param: String='');
begin
  if not frmMain.edtCommand.Focused then
    begin
      mbSetCurrentDir(frmMain.ActiveFrame.CurrentPath);
      ExecCmdFork(gRunTerm);
    end;
end;

procedure TMainCommands.cm_CalculateSpace(Param: String='');
var
  SelectedFiles: TFiles;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
begin
  with frmMain do
  begin
    if not (fsoCalcStatistics in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      Operation := ActiveFrame.FileSource.CreateCalcStatisticsOperation(SelectedFiles);
      Operation.AddStateChangedListener([fsosStopped], @OnCalcStatisticsStateChanged);
      OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);
      ProgressDialog := TfrmFileOp.Create(OperationHandle);
      ProgressDialog.Show;
    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_CountDirContent(Param: String='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CountDirContent', param);
end;

procedure TMainCommands.cm_SetFileProperties(Param: String='');
var
  ActiveFile: TFile = nil;
  SelectedFiles: TFiles = nil;
  aFileProperties: TFileProperties;
  Operation: TFileSourceSetFilePropertyOperation = nil;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
begin
  with frmMain do
  try
    if not (fsoSetFileProperty in ActiveFrame.FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    ActiveFile := ActiveFrame.CloneActiveFile;
    if Assigned(ActiveFile) then
      begin
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
                OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);
                Operation := nil; // So it doesn't get destroyed below.
                ProgressDialog := TfrmFileOp.Create(OperationHandle);
                ProgressDialog.Show;
              end;
          end;
      end;
  finally
    if Assigned(SelectedFiles) then
      FreeAndNil(SelectedFiles);
    if Assigned(ActiveFile) then
      FreeAndNil(ActiveFile);
    if Assigned(Operation) then
      FreeAndNil(Operation);
  end;
end;

procedure TMainCommands.cm_FileProperties(Param: String='');
var
  SelectedFiles: TFiles;
  Operation: TFileSourceExecuteOperation;
  aFile: TFile;
begin
  with frmMain do
  begin
    if ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
      begin
        SelectedFiles := ActiveFrame.CloneSelectedFiles;
        if Assigned(SelectedFiles) then
        try
          if SelectedFiles.Count > 0 then
          try
            ShowFilePropertiesDialog(ActiveFrame.FileSource, SelectedFiles);
            ActiveFrame.Reload;
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

procedure TMainCommands.cm_FileLinker(Param: String='');
var
  I: Integer;
  aSelectedFiles: TFiles = nil;
  aFile: TFile;
begin
  with frmMain, frmMain.ActiveFrame do
  begin
    if not (fsoCombine in FileSource.GetOperationsTypes) then
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;

    try
      aSelectedFiles := CloneSelectedFiles;

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
        ShowLinkerFilesForm(FileSource, aSelectedFiles, NotActiveFrame.CurrentPath)
      else
        msgWarning(rsMsgInvalidSelection);
    finally
      FreeThenNil(aSelectedFiles);
    end; // try
  end; // with
end;

procedure TMainCommands.cm_FileSpliter(Param: String='');
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

procedure TMainCommands.cm_PanelsSplitterPerPos(Param: String='');
var i:integer;
begin
  with frmMain do
  begin
   if TryStrToInt(param,i) then
     begin
       if not gHorizontalFilePanels then
         pnlLeft.Width:= (pnlNoteBooks.Width-MainSplitter.Width) * i div 100
       else
         pnlLeft.Height:= (pnlNoteBooks.Height-MainSplitter.Height) * i div 100;
     end;
  end;
end;

procedure TMainCommands.cm_EditComment(Param: String='');
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
              ShowDescrEditDlg(CurrentPath + aFile.Name)
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
                  ShowDescrEditDlg(aFile.FullPath)
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
  if FileSource.IsClass(TFileSystemFileSource) then
  begin
    sl := TStringList.Create;
    try
      theSelectedFiles := CloneSelectedFiles;

      for i := 0 to theSelectedFiles.Count - 1 do
        sl.Add(CurrentPath + theSelectedFiles[i].Name);

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

procedure TMainCommands.cm_CopyToClipboard(Param: String='');
begin
  SendToClipboard(ClipboardCopy);
end;

procedure TMainCommands.cm_CutToClipboard(Param: String='');
begin
  SendToClipboard(ClipboardCut);
end;

procedure TMainCommands.cm_PasteFromClipboard(Param: String='');
var
  ClipboardOp: TClipboardOperation;
  filenamesList: TStringList;
  Files: TFiles = nil;
  Operation: TFileSourceOperation = nil;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
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
          OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);
          ProgressDialog := TfrmFileOp.Create(OperationHandle);
          ProgressDialog.Show;

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

procedure TMainCommands.cm_ChangeDirToRoot(Param: String='');
begin
  with frmMain.ActiveFrame do
  begin
    CurrentPath := FileSource.GetRootDir(CurrentPath);
  end;
end;

procedure TMainCommands.cm_ChangeDirToParent(Param: String='');
begin
  frmMain.ActiveFrame.ChangePathToParent(True);
end;

// Parameters:
// Full path to a directory.
procedure TMainCommands.cm_ChangeDir(Param: String='');
begin
  FrmMain.ActiveFrame.CurrentPath := ReplaceEnvVars(param);
end;

procedure TMainCommands.cm_ClearLogWindow(Param: String='');
begin
  frmMain.seLogWindow.Lines.Clear;
end;

procedure TMainCommands.cm_ClearLogFile(Param: String='');
begin
  mbDeleteFile(gLogFileName);
end;

procedure TMainCommands.cm_NetworkConnect(Param: String='');
begin
  {
  ShowConnectionManager(frmMain.ActiveFrame);
  }
end;

procedure TMainCommands.cm_NetworkDisconnect(Param: String='');
begin
  {
  if frmMain.ActiveFrame.FileSource.IsClass(TWfxPluginFileSource) then
  with frmMain.ActiveFrame.FileSource as IWfxPluginFileSource do
  begin
    if param <> EmptyStr then
      WfxModule.WfxNetworkCloseConnection(param);
  end;
  }
end;

procedure TMainCommands.cm_HorizontalFilePanels(Param: String='');
begin
  gHorizontalFilePanels := not gHorizontalFilePanels;
  frmMain.actHorizontalFilePanels.Checked := gHorizontalFilePanels;
  frmMain.UpdateWindowView;
end;

procedure TMainCommands.cm_OperationsViewer(Param: String='');
begin
  ShowOperationsViewer;
end;

procedure TMainCommands.cm_Int_RunCommandFromBarFile(Param: String='');
var
  SR : TSearchRecEx;
  Res : Integer;
  sCmd, sParam, sPath : string;
begin
  Res := FindFirstEx(gpCfgDir + '*.bar', faAnyFile, SR);
  while Res = 0 do
  begin
    GetCommandFromBar (gpCfgDir + Sr.Name, param, sCmd, sParam, sPath);
    if sCmd = '' then
      Res := FindNextEx(SR)
    else
      Res := -1;
  end;
  FindCloseEx(SR);
  frmMain.ExecCmd(sCmd, sParam, sPath);
end;

procedure TMainCommands.GetCommandFromBar (const cFilename, cParam: string; var Com, Par, Pat:string);
var
  i, BtnCount: integer;
  IniFile: TIniFileEx=nil;
begin
  Com :='';
  IniFile:= TIniFileEx.Create(cFilename);
  BtnCount := IniFile.ReadInteger('Buttonbar', 'Buttoncount', 0);
  for i := 1 to BtnCount do
    begin
       if IniFile.ReadString('Buttonbar', 'misk' + IntToStr(i), '') = cParam then
         begin
           Com := IniFile.ReadString('Buttonbar', 'cmd' + IntToStr(i), '');
           Par := IniFile.ReadString('Buttonbar', 'param' + IntToStr(i), '');
           Pat := IniFile.ReadString('Buttonbar', 'path' + IntToStr(i), '');
         end;
    end;
  FreeThenNil(IniFile);
end;

end.

