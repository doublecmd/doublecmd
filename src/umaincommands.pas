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
   procedure DoTransferPath(SourcePage: TFileViewPage; TargetPage: TFileViewPage; FromActivePanel: Boolean);
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
   procedure cm_AddPathToCmdLine(const Params: array of string);
   procedure cm_AddFilenameToCmdLine(const Params: array of string);
   procedure cm_AddPathAndFilenameToCmdLine(const Params: array of string);
   procedure cm_ContextMenu(const Params: array of string);
   procedure cm_CopyFullNamesToClip(const Params: array of string);
   procedure cm_Exchange(const Params: array of string);
   procedure cm_OpenArchive(const Params: array of string);
   procedure cm_TestArchive(const Params: array of string);
   procedure cm_OpenDirInNewTab(const Params: array of string);
   procedure cm_Open(const Params: array of string);
   procedure cm_OpenVirtualFileSystemList(const Params: array of string);
   procedure cm_TargetEqualSource(const Params: array of string);
   procedure cm_LeftEqualRight(const Params: array of string);
   procedure cm_RightEqualLeft(const Params: array of string);
   procedure cm_PackFiles(const Params: array of string);
   procedure cm_ExtractFiles(const Params: array of string);
   procedure cm_QuickSearch(const Params: array of string);
   procedure cm_QuickFilter(const Params: array of string);
   procedure cm_LeftOpenDrives(const Params: array of string);
   procedure cm_RightOpenDrives(const Params: array of string);
   procedure cm_OpenBar(const Params: array of string);
   procedure cm_ShowButtonMenu(const Params: array of string);
   procedure cm_TransferLeft(const Params: array of string);
   procedure cm_TransferRight(const Params: array of string);
   procedure cm_GoToFirstFile(const Params: array of string);
   procedure cm_GoToLastFile(const Params: array of string);
   procedure cm_Minimize(const Params: array of string);
   procedure cm_Wipe(const Params: array of string);
   procedure cm_Exit(const Params: array of string);
   procedure cm_NewTab(const Params: array of string);
   procedure cm_RenameTab(const Params: array of string);
   procedure cm_RemoveTab(const Params: array of string);
   procedure cm_RemoveAllTabs(const Params: array of string);
   procedure cm_NextTab(const Params: array of string);
   procedure cm_PrevTab(const Params: array of string);
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
   procedure cm_CopyNamesToClip(const Params: array of string);
   procedure cm_FocusCmdLine(const Params: array of string);
   procedure cm_FileAssoc(const Params: array of string);
   procedure cm_HelpIndex(const Params: array of string);
   procedure cm_Keyboard(const Params: array of string);
   procedure cm_VisitHomePage(const Params: array of string);
   procedure cm_About(const Params: array of string);
   procedure cm_ShowSysFiles(const Params: array of string);
   procedure cm_SwitchIgnoreList(const Params: array of string);
   procedure cm_Options(const Params: array of string);
   procedure cm_CompareContents(const Params: array of string);
   procedure cm_Refresh(const Params: array of string);
   procedure cm_ShowMainMenu(const Params: array of string);
   procedure cm_DirHotList(const Params: array of string);
   procedure cm_MarkInvert(const Params: array of string);
   procedure cm_MarkMarkAll(const Params: array of string);
   procedure cm_MarkUnmarkAll(const Params: array of string);
   procedure cm_MarkPlus(const Params: array of string);
   procedure cm_MarkMinus(const Params: array of string);
   procedure cm_MarkCurrentExtension(const Params: array of string);
   procedure cm_UnmarkCurrentExtension(const Params: array of string);
   procedure cm_SaveSelection(const Params: array of string);
   procedure cm_RestoreSelection(const Params: array of string);
   procedure cm_SaveSelectionToFile(const Params: array of string);
   procedure cm_LoadSelectionFromFile(const Params: array of string);
   procedure cm_LoadSelectionFromClip(const Params: array of string);
   procedure cm_Search(const Params: array of string);
   procedure cm_HardLink(const Params: array of string);
   procedure cm_MultiRename(const Params: array of string);
   procedure cm_ReverseOrder(const Params: array of string);
   procedure cm_SortByAttr(const Params: array of string);
   procedure cm_SortByDate(const Params: array of string);
   procedure cm_SortByExt(const Params: array of string);
   procedure cm_SortByName(const Params: array of string);
   procedure cm_SortBySize(const Params: array of string);
   procedure cm_SymLink(const Params: array of string);
   procedure cm_CopySamePanel(const Params: array of string);
   procedure cm_DirHistory(const Params: array of string);
   procedure cm_ViewHistory(const Params: array of string);
   procedure cm_ViewHistoryPrev(const Params: array of string);
   procedure cm_ViewHistoryNext(const Params: array of string);
   procedure cm_EditNew(const Params: array of string);
   procedure cm_RenameOnly(const Params: array of string);
   procedure cm_RunTerm(const Params: array of string);
   procedure cm_ShowCmdLineHistory(const Params: array of string);
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
   procedure cm_ChangeDirToParent(const Params: array of string);
   procedure cm_ChangeDir(const Params: array of string);
   procedure cm_ClearLogWindow(const Params: array of string);
   procedure cm_ClearLogFile(const Params: array of string);
   procedure cm_NetworkConnect(const Params: array of string);
   procedure cm_NetworkDisconnect(const Params: array of string);
   procedure cm_HorizontalFilePanels(const Params: array of string);
   procedure cm_OperationsViewer(const Params: array of string);
   procedure cm_CompareDirectories(const Params: array of string);

   // Internal commands
   procedure cm_Int_RunCommandFromBarFile(const Params: array of string);
  end;

implementation

uses Forms, Controls, Dialogs, Clipbrd, strutils, LCLProc, HelpIntfs, StringHashList,
     dmHelpManager, typinfo, fMain, fPackDlg, fMkDir, fFileAssoc,
     fExtractDlg, fAbout, fOptions, fDiffer, fFindDlg, fSymLink, fHardLink, fMultiRename,
     fLinker, fSplitter, fDescrEdit, fCheckSumVerify, fCheckSumCalc, fSetFileProperties,
     uGlobs, uLng, uLog, uShowMsg, uOSForms, uOSUtils, uDCUtils, uGlobsPaths,
     uClassesEx, uShowForm, uShellExecute, uClipboard, uHash, uDisplayFile,
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
          else if not ChooseFileSource(TargetPage.FileView, aFile) then
            begin
              ChooseFileSource(TargetPage.FileView, aFile.Path);
              TargetPage.FileView.SetActiveFile(aFile.Name);
            end;
        end
      else if aFile.IsDirectory then
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

procedure TMainCommands.cm_AddPathToCmdLine(const Params: array of string);
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

procedure TMainCommands.cm_AddFilenameToCmdLine(const Params: array of string);
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

procedure TMainCommands.cm_AddPathAndFilenameToCmdLine(const Params: array of string);
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

procedure TMainCommands.cm_ContextMenu(const Params: array of string);
begin
  // Let file view handle displaying context menu at appropriate position.
  frmMain.ActiveFrame.ExecuteCommand('cm_ContextMenu', []);
end;

procedure TMainCommands.cm_CopyFullNamesToClip(const Params: array of string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, True);
end;

procedure TMainCommands.cm_CopyNamesToClip(const Params: array of string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, False);
end;

//------------------------------------------------------
procedure TMainCommands.cm_Exchange(const Params: array of string);
var
  sDir: String;
begin
  sDir:= FrmMain.ActiveFrame.CurrentPath;
  FrmMain.ActiveFrame.CurrentPath:= FrmMain.NotActiveFrame.CurrentPath;
  FrmMain.NotActiveFrame.CurrentPath:= sDir;
end;

procedure TMainCommands.cm_OpenDirInNewTab(const Params: array of string);
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

procedure TMainCommands.cm_TargetEqualSource(const Params: array of string);
begin
  with frmMain do
  begin
    NotActiveNotebook.ActivePage.FileView := nil;
    ActiveFrame.Clone(NotActiveNotebook.ActivePage);
    NotActiveNotebook.ActivePage.UpdateCaption(GetLastDir(
      ExcludeTrailingPathDelimiter(NotActiveNotebook.ActivePage.FileView.CurrentPath)));
  end;
end;

procedure TMainCommands.cm_LeftEqualRight(const Params: array of string);
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

procedure TMainCommands.cm_RightEqualLeft(const Params: array of string);
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
          ChooseArchive(frmMain.ActiveFrame, aFile, True);
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
    SelectedFiles := ActiveFrame.CloneSelectedFiles;
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
  frmMain.ActiveFrame.ExecuteCommand('cm_Open', Params);
end;

procedure TMainCommands.cm_OpenVirtualFileSystemList(const Params: array of string);
var
  FileSource: IFileSource;
  Param: String;
begin
  Param := GetDefaultParam(Params);
  with frmMain do
  begin
    if (gWFXPlugins.Count = 0) and (gVfsModuleList.Count = 0) then Exit;
    FileSource:= TVfsFileSource.Create(gWFXPlugins);
    if Assigned(FileSource) then
    begin
      if dskLeft.Name = Param then
        FrameLeft.AddFileSource(FileSource, FileSource.GetRootDir)
      else if dskRight.Name = Param then
        FrameRight.AddFileSource(FileSource, FileSource.GetRootDir)
      else
        ActiveFrame.AddFileSource(FileSource, FileSource.GetRootDir)
    end;
  end;
end;

//------------------------------------------------------
(* Pack files in archive by creating a new archive *)
procedure TMainCommands.cm_PackFiles(const Params: array of string);
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
procedure TMainCommands.cm_ExtractFiles(const Params: array of string);
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

procedure TMainCommands.cm_QuickSearch(const Params: array of string);
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickSearch', Params);
end;

procedure TMainCommands.cm_QuickFilter(const Params: array of string);
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickFilter', Params);
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
var
  sFileName: UTF8String;
  IniFile: TIniFileEx = nil;
begin
  if Length(Params) > 0 then
  begin
    with frmMain do
    try
      if Pos(PathDelim, Params[0]) <> 0 then
        sFileName:= GetCmdDirFromEnvVar(Params[0])
      else
        sFileName:= gpCfgDir + Params[0];
      IniFile:= TIniFileEx.Create(sFileName);
      MainToolBar.AutoSize:= False;
      MainToolBar.LoadFromIniFile(IniFile);
    finally
      MainToolBar.AutoSize:= True;
      FreeThenNil(IniFile);
    end;
  end;
end;

procedure TMainCommands.cm_ShowButtonMenu(const Params: array of string);
var
  Point: TPoint;
  sFileName: UTF8String;
  IniFile: TIniFileEx = nil;
begin
  if Length(Params) > 0 then
  begin
    with frmMain do
    try
      if Pos(PathDelim, Params[0]) <> 0 then
        sFileName:= GetCmdDirFromEnvVar(Params[0])
      else
        sFileName:= gpCfgDir + Params[0];
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
  theFilesToWipe: TFiles;
  Operation: TFileSourceOperation;
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
        OperationsManager.AddOperation(Operation);
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

procedure TMainCommands.cm_RemoveTab(const Params: array of string);
begin
  with frmMain do
    DoRemoveTab(ActiveNotebook, ActiveNotebook.PageIndex);
end;

procedure TMainCommands.cm_RemoveAllTabs(const Params: array of string);
var
  I: Integer;
  ANotebook: TFileViewNotebook;
  Param: String;
begin
  with frmMain do
  begin
    if (tb_confirm_close_all in gDirTabOptions) then
      if not msgYesNo(rsMsgCloseAllInActiveTabs) then Exit;

    Param := GetDefaultParam(Params);
    if Param = 'LeftTabs' then
      ANotebook := LeftTabs
    else if Param = 'RightTabs' then
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

procedure TMainCommands.cm_NextTab(const Params: array of string);
begin
  frmMain.ActiveNotebook.ActivateNextTab;
end;

procedure TMainCommands.cm_PrevTab(const Params: array of string);
begin
  frmMain.ActiveNotebook.ActivatePrevTab;
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
  UI: TFileSourceOperationMessageBoxesUI = nil;
begin
  with frmMain do
  try
    SelectedFiles := ActiveFrame.CloneSelectedFiles;
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
        // Call directly - not through operations manager.
        UI := TFileSourceOperationMessageBoxesUI.Create;
        try
          Operation.AddUserInterface(UI);
          Operation.Execute;
          if Operation.Result = fsorAborted then Exit;
        finally
          FreeAndNil(Operation);
          FreeAndNil(UI);
        end;

        aFileSource := TempFileSource;
        ChangeFileListRoot(TempFileSource.FileSystemRoot, SelectedFiles);
      end
      else
      begin
        msgWarning(rsMsgErrNotSupported);
        Exit;
      end;
    end;

    try
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
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningViewer,
          rsMsgInvalidCommandLine + ' (' + rsToolViewer + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;

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

procedure TMainCommands.cm_Edit(const Params: array of string);
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

    try
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
  frmMain.ActiveFrame.ExecuteCommand('cm_EditPath', Params);
end;

procedure TMainCommands.cm_Copy(const Params: array of string);
begin
  if frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, True) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_CopyNoAsk(const Params: array of string);
begin
  if frmMain.CopyFiles(frmMain.NotActiveFrame.CurrentPath, False) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_Rename(const Params: array of string);
begin
  if frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, True) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_RenameNoAsk(const Params: array of string);
begin
  if frmMain.MoveFiles(frmMain.NotActiveFrame.CurrentPath, False) then
    frmMain.ActiveFrame.UnselectAllFiles;
end;

procedure TMainCommands.cm_MakeDir(const Params: array of string);
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
// trashcan=
//   1/true            - delete to trash can
//   0/false           - delete directly
//   setting           - if gUseTrash then delete to trash, otherwise delete directly
//   reversesetting    - if gUseTrash then delete directly, otherwise delete to trash
//
// Deprecated:
// "recycle"           - delete to trash can
// "norecycle"         - delete directly
// "recyclesetting"    - if gUseTrash then delete to trash, otherwise delete directly
// "recyclesettingrev" - if gUseTrash then delete directly, otherwise delete to trash
procedure TMainCommands.cm_Delete(const Params: array of string);
var
  theFilesToDelete: TFiles;
  // 12.05.2009 - if delete to trash, then show another messages
  MsgDelSel, MsgDelFlDr : string;
  Operation: TFileSourceOperation;
  bRecycle: Boolean;
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
      end;
    end;

    if bRecycle then
      bRecycle := FileSource.IsClass(TFileSystemFileSource) and
                  mbCheckTrash(CurrentPath);

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

      if QuestionDlg('', frmMain.GetFileDlgStr(MsgDelSel,MsgDelFlDr,theFilesToDelete),
         mtConfirmation, [mrYes, mrNo], 0) <> mrYes then Exit;

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
        OperationsManager.AddOperation(Operation);
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

procedure TMainCommands.cm_CheckSumCalc(const Params: array of string);
var
  I: Integer;
  bSeparateFile: Boolean;
  HashAlgorithm: THashAlgorithm;
  sFileName: UTF8String;
  SelectedFiles: TFiles;
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
          OperationsManager.AddOperation(Operation);
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
  SelectedFiles: TFiles;
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
    end;

    frmMain.edtCommand.SetFocus;
  end;
end;

procedure TMainCommands.cm_FileAssoc(const Params: array of string);
begin
  ShowFileAssocDlg;
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
  ErrMsg: String;
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
    //repaint both panels
    FrameLeft.Reload;
    FrameRight.Reload;
  end;
end;

procedure TMainCommands.cm_SwitchIgnoreList(const Params: array of string);
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
procedure TMainCommands.cm_Options(const Params: array of string);
begin
  if Assigned(frmOptions) then
    begin
      if frmOptions.WindowState = wsMinimized then
        frmOptions.WindowState:= wsNormal
      else
        frmOptions.BringToFront;
    end
  else
    begin
      if Length(Params) = 0 then
        frmOptions := TfrmOptions.Create(Application)
      else
        frmOptions := TfrmOptions.Create(Application, Params[0]);

      frmOptions.Show;
  end;
end;

procedure TMainCommands.cm_CompareContents(const Params: array of string);
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
      try
        ExecCmdFork(sCommand, RunInTerminal, '', KeepTerminalOpen);
      except
        on e: EInvalidCommandLine do
          MessageDlg(rsToolErrorOpeningDiffer,
            rsMsgInvalidCommandLine + ' (' + rsToolDiffer + '):' + LineEnding + e.Message,
            mtError, [mbOK], 0);
      end;
    end;
  end;

var
  i : Integer;
  ActiveSelectedFiles: TFiles = nil;
  NotActiveSelectedFiles: TFiles = nil;
  Param: String;
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
      Param := GetDefaultParam(Params);

      if Param = 'dir' then
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

procedure TMainCommands.cm_ShowMainMenu(const Params: array of string);
begin
  DoShowMainMenu(not gMainMenu);
end;

procedure TMainCommands.cm_Refresh(const Params: array of string);
begin
  frmMain.ActiveFrame.Reload;
end;

//------------------------------------------------------

procedure TMainCommands.cm_MarkInvert(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkInvert', Params);
end;

procedure TMainCommands.cm_MarkMarkAll(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkMarkAll', Params);
end;

procedure TMainCommands.cm_MarkUnmarkAll(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkUnmarkAll', Params);
end;

procedure TMainCommands.cm_MarkPlus(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkPlus', Params);
end;

procedure TMainCommands.cm_MarkMinus(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkMinus', Params);
end;

procedure TMainCommands.cm_MarkCurrentExtension(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkCurrentExtension', Params);
end;

procedure TMainCommands.cm_UnmarkCurrentExtension(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_UnmarkCurrentExtension', Params);
end;

procedure TMainCommands.cm_SaveSelection(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_SaveSelection', Params);
end;

procedure TMainCommands.cm_RestoreSelection(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_RestoreSelection', Params);
end;

procedure TMainCommands.cm_SaveSelectionToFile(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_SaveSelectionToFile', Params);
end;

procedure TMainCommands.cm_LoadSelectionFromFile(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_LoadSelectionFromFile', Params);
end;

procedure TMainCommands.cm_LoadSelectionFromClip(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_LoadSelectionFromClip', Params);
end;

//------------------------------------------------------

procedure TMainCommands.cm_DirHotList(const Params: array of string);
var
  p:TPoint;
begin
  frmMain.CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p:=frmMain.ActiveFrame.ClientToScreen(Classes.Point(0,0));
  frmMain.pmHotList.Popup(p.X,p.Y);
end;

procedure TMainCommands.cm_Search(const Params: array of string);
begin
  ShowFindDlg(frmMain.ActiveFrame.CurrentPath);
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
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
procedure TMainCommands.cm_ReverseOrder(const Params: array of string);
begin
  with frmMain.ActiveFrame do
    Sorting := ReverseSortDirection(Sorting);
end;

procedure TMainCommands.cm_SortByName(const Params: array of string);
var
  FileFunctions: TFileFunctions = nil;
begin
  AddSortFunction(FileFunctions, fsfName);
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

    ShowEditorByGlob(sNewFile);
  end
  else
    msgWarning(rsMsgNotImplemented);
end;

// Shows recently visited directories (global).
procedure TMainCommands.cm_DirHistory(const Params: array of string);
var
  p:TPoint;
begin
  frmMain.CreatePopUpDirHistory;
  p:=frmMain.ActiveFrame.ClientToScreen(Classes.Point(0,0));
  frmMain.pmDirHistory.Popup(p.X,p.Y);
end;

// Shows browser-like history for active file view.
procedure TMainCommands.cm_ViewHistory(const Params: array of string);
begin
  frmMain.ShowFileViewHistory;
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

procedure TMainCommands.cm_ShowCmdLineHistory(const Params: array of string);
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

procedure TMainCommands.cm_RunTerm(const Params: array of string);
begin
  if not frmMain.edtCommand.Focused then
    begin
      mbSetCurrentDir(frmMain.ActiveFrame.CurrentPath);
      try
        ExecCmdFork(gRunTerm);
      except
        on e: EInvalidCommandLine do
          MessageDlg(rsToolErrorOpeningTerminal,
            rsMsgInvalidCommandLine + ' (' + rsToolTerminal + '):' + LineEnding + e.Message,
            mtError, [mbOK], 0);
      end;
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

    SelectedFiles := ActiveFrame.CloneSelectedFiles;
    try
      Operation := ActiveFrame.FileSource.CreateCalcStatisticsOperation(SelectedFiles);
      Operation.AddStateChangedListener([fsosStopped], @OnCalcStatisticsStateChanged);
      OperationsManager.AddOperation(Operation);
    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TMainCommands.cm_CountDirContent(const Params: array of string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CountDirContent', Params);
end;

procedure TMainCommands.cm_SetFileProperties(const Params: array of string);
var
  ActiveFile: TFile = nil;
  SelectedFiles: TFiles = nil;
  aFileProperties: TFileProperties;
  Operation: TFileSourceSetFilePropertyOperation = nil;
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
                OperationsManager.AddOperation(Operation);
                Operation := nil; // So it doesn't get destroyed below.
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
        SelectedFiles := ActiveFrame.CloneSelectedFiles;
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
  Split: Integer = -1;
  Param, SplitPct: String;
begin
  with frmMain do
  begin
    for Param in Params do
    begin
      if GetParamValue(Param, 'splitpct', SplitPct) then
        Split := StrToIntDef(SplitPct, Split)
      else if Split = -1 then
        Split := StrToIntDef(Param, Split); // deprecated
    end;

    if (Split >= 0) and (Split <= 100) then
      begin
        if not gHorizontalFilePanels then
          pnlLeft.Width:= (pnlNoteBooks.Width-MainSplitter.Width) * Split div 100
        else
          pnlLeft.Height:= (pnlNoteBooks.Height-MainSplitter.Height) * Split div 100;
      end;
  end;
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
  with frmMain.ActiveFrame do
  begin
    CurrentPath := FileSource.GetRootDir(CurrentPath);
  end;
end;

procedure TMainCommands.cm_ChangeDirToParent(const Params: array of string);
begin
  frmMain.ActiveFrame.ChangePathToParent(True);
end;

// Parameters:
// Full path to a directory.
procedure TMainCommands.cm_ChangeDir(const Params: array of string);
begin
  if Length(Params) > 0 then
    FrmMain.ActiveFrame.CurrentPath := ReplaceEnvVars(Params[0]);
end;

procedure TMainCommands.cm_ClearLogWindow(const Params: array of string);
begin
  frmMain.seLogWindow.Lines.Clear;
end;

procedure TMainCommands.cm_ClearLogFile(const Params: array of string);
begin
  mbDeleteFile(gLogFileName);
end;

procedure TMainCommands.cm_NetworkConnect(const Params: array of string);
begin
  {
  ShowConnectionManager(frmMain.ActiveFrame);
  }
end;

procedure TMainCommands.cm_NetworkDisconnect(const Params: array of string);
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

procedure TMainCommands.cm_HorizontalFilePanels(const Params: array of string);
begin
  gHorizontalFilePanels := not gHorizontalFilePanels;
  frmMain.actHorizontalFilePanels.Checked := gHorizontalFilePanels;
  frmMain.UpdateWindowView;
end;

procedure TMainCommands.cm_OperationsViewer(const Params: array of string);
begin
  ShowOperationsViewer;
end;

procedure TMainCommands.cm_CompareDirectories(const Params: array of string);
var
  I: LongWord;
  SourceFile: TDisplayFile;
  TargetFile: TDisplayFile;
  SourceList: TStringHashList;
  SourceFiles: TDisplayFiles = nil;
  TargetFiles: TDisplayFiles = nil;
  FileTimeDiff: TDateTime;
begin
  SourceList:= TStringHashList.Create(FileNameCaseSensitive);
  with frmMain do
  try
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
        begin
          FileTimeDiff:= SourceFile.FSFile.ModificationTime - TargetFile.FSFile.ModificationTime;
          if FileTimeDiff = 0 then
            ActiveFrame.MarkFile(SourceFile, False)
          else if FileTimeDiff > 0 then
            NotActiveFrame.MarkFile(TargetFile, False)
          else if FileTimeDiff < 0 then
            begin
              ActiveFrame.MarkFile(SourceFile, False);
              NotActiveFrame.MarkFile(TargetFile, True);
            end;
        end;
    end;
  finally
    SourceList.Free;
    ActiveFrame.UpdateView;
    NotActiveFrame.UpdateView;
  end;
end;

procedure TMainCommands.cm_Int_RunCommandFromBarFile(const Params: array of string);
var
  SR : TSearchRecEx;
  Res : Integer;
  Param : String;
  sCmd, sParam, sPath : String;
begin
  Param := GetDefaultParam(Params);
  Res := FindFirstEx(gpCfgDir + '*.bar', faAnyFile, SR);
  while Res = 0 do
  begin
    GetCommandFromBar (gpCfgDir + Sr.Name, Param, sCmd, sParam, sPath);
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

