{
   Double Commander
   -------------------------------------------------------------------------
   Base class for file views which display an ordered (indexed) list of files

   Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)

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

unit uOrderedFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Menus,
  uTypes,
  fQuickSearch,
  uFileView,
  uFileViewWithPanels,
  uDisplayFile;

const
  InvalidFileIndex = PtrInt(-1);

type
  { TOrderedFileView }

  TOrderedFileView = class(TFileViewWithPanels)
  private
    pmOperationsCancel: TPopupMenu;
    procedure lblFilterClick(Sender: TObject);
    procedure pmOperationsCancelClick(Sender: TObject);
    procedure quickSearchChangeSearch(Sender: TObject; ASearchText: UTF8String; const ASearchOptions: TQuickSearchOptions; ASearchDirection: TQuickSearchDirection);
    procedure quickSearchChangeFilter(Sender: TObject; AFilterText: UTF8String; const AFilterOptions: TQuickSearchOptions);
    procedure quickSearchExecute(Sender: TObject);
    procedure quickSearchHide(Sender: TObject);
    procedure UpdateRangeSelectionState;

  protected
    lblFilter: TLabel;
    quickSearch: TfrmQuickSearch;
    FLastActiveFileIndex: Integer;
    FRangeSelecting: Boolean;
    FRangeSelectionStartIndex: Integer;
    FRangeSelectionEndIndex: Integer;
    FRangeSelectionState: Boolean;
    FUpdatingActiveFile: Boolean;
    procedure AfterChangePath; override;
    procedure CreateDefault(AOwner: TWinControl); override;
    procedure DoFileIndexChanged(NewFileIndex: PtrInt);
    procedure DoHandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoHandleKeyDownWhenLoading(var Key: Word; Shift: TShiftState); override;
    procedure DoSelectionChanged; override; overload;
    procedure DoSelectionChanged(FileIndex: PtrInt); overload;
    procedure EnsureDisplayProperties; override;
    function GetActiveDisplayFile: TDisplayFile; override;
    function GetActiveFileIndex: PtrInt; virtual; abstract;
    function GetFileRect(FileIndex: PtrInt): TRect; virtual; abstract;
    function GetVisibleFilesIndexes: TRange; virtual; abstract;
    function IsFileIndexInRange(FileIndex: PtrInt): Boolean; inline;
    {en
       If marking a single file only redraws that file.
       Otherwise files are marked and full update is performed.
    }
    procedure MarkFilesWithCheck(FromIndex, ToIndex: PtrInt; bSelect: Boolean);
    procedure RedrawFile(FileIndex: PtrInt); overload; virtual; abstract;
    {en
       Search and position in a file that matches name taking into account
       passed options.
    }
    procedure SearchFile(SearchTerm: UTF8String;
                         SearchOptions: TQuickSearchOptions;
                         SearchDirection: TQuickSearchDirection = qsdNone);
    procedure Selection(Key: Word; CurIndex: PtrInt);
    procedure SelectRange(FileIndex: PtrInt);
    procedure SetActiveFile(FileIndex: PtrInt); overload; virtual; abstract;
    procedure SetLastActiveFile(FileIndex: PtrInt);
    {en
       Sets a file as active if the file currently exists.
       @returns(@true if the file was found and selected.)
    }
    function SetActiveFileNow(aFilePath: String): Boolean;

  public
    procedure CloneTo(AFileView: TFileView); override;
    procedure SetActiveFile(aFilePath: String); override; overload;

  published  // commands
    procedure cm_QuickSearch(const Params: array of string);
    procedure cm_QuickFilter(const Params: array of string);
    procedure cm_GoToFirstFile(const Params: array of string);
    procedure cm_GoToLastFile(const Params: array of string);
  end;

implementation

uses
  LCLProc, LCLType, math, Forms, Graphics,
  DCStrUtils,
  uLng, uGlobs, uMasks,
  uFileSourceProperty,
  uPixMapManager,
  uFileViewWorker,
  uFile;

const
  CANCEL_FILTER = 0;
  CANCEL_OPERATION = 1;

{ TOrderedFileView }

procedure TOrderedFileView.AfterChangePath;
begin
  FLastActiveFileIndex := -1;
  inherited AfterChangePath;
end;

procedure TOrderedFileView.CloneTo(AFileView: TFileView);
begin
  if Assigned(AFileView) then
  begin
    inherited CloneTo(AFileView);

    with AFileView as TOrderedFileView do
    begin
      FLastActiveFileIndex := Self.FLastActiveFileIndex;
      FRangeSelectionStartIndex := Self.FRangeSelectionStartIndex;
      FRangeSelectionEndIndex := Self.FRangeSelectionEndIndex;
      FRangeSelectionState := Self.FRangeSelectionState;
    end;
  end;
end;

procedure TOrderedFileView.cm_GoToFirstFile(const Params: array of string);
begin
  if not (IsEmpty or IsLoadingFileList) then
    SetActiveFile(0);
end;

procedure TOrderedFileView.cm_GoToLastFile(const Params: array of string);
begin
  if not (IsEmpty or IsLoadingFileList) then
    SetActiveFile(FFiles.Count - 1);
end;

procedure TOrderedFileView.cm_QuickFilter(const Params: array of string);
begin
  if not IsLoadingFileList then
    quickSearch.Execute(qsFilter, Params);
end;

procedure TOrderedFileView.cm_QuickSearch(const Params: array of string);
begin
  if not IsLoadingFileList then
    quickSearch.Execute(qsSearch, Params);
end;

procedure TOrderedFileView.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);

  FLastActiveFileIndex := -1;
  FRangeSelectionStartIndex := -1;
  FRangeSelectionEndIndex := -1;

  lblFilter         := TLabel.Create(pnlFooter);
  lblFilter.Parent  := pnlFooter;
  lblFilter.Align   := alRight;
  lblFilter.Visible := False;
  lblFilter.OnClick := @lblFilterClick;

  quickSearch         := TfrmQuickSearch.Create(Self);
  quickSearch.Parent  := Self;
  quickSearch.Visible := False;
  quickSearch.Align   := alBottom;
  quickSearch.OnChangeSearch := @quickSearchChangeSearch;
  quickSearch.OnChangeFilter := @quickSearchChangeFilter;
  quickSearch.OnExecute      := @quickSearchExecute;
  quickSearch.OnHide         := @quickSearchHide;

  pmOperationsCancel := TPopupMenu.Create(Self);
  pmOperationsCancel.Parent := Self;
end;

procedure TOrderedFileView.DoFileIndexChanged(NewFileIndex: PtrInt);
begin
  if IsFileIndexInRange(NewFileIndex) and (FLastActiveFileIndex <> NewFileIndex) then
  begin
    if not FRangeSelecting then
    begin
      // Set range selection starting point.
      FRangeSelectionStartIndex := NewFileIndex;
      FRangeSelectionEndIndex   := NewFileIndex;
      UpdateRangeSelectionState;
    end;

    if not FUpdatingActiveFile then
    begin
      SetLastActiveFile(NewFileIndex);

      if Assigned(OnChangeActiveFile) then
        OnChangeActiveFile(Self, FFiles[NewFileIndex].FSFile);
    end;
  end;
end;

procedure TOrderedFileView.DoHandleKeyDown(var Key: Word; Shift: TShiftState);
var
  mi: TMenuItem;
begin
  // check if ShiftState is equal to quick search / filter modes
  if quickSearch.CheckSearchOrFilter(Key) then
    Exit;

  case Key of
    VK_ESCAPE:
      begin
        if Filtered and (GetCurrentWorkType <> fvwtNone) then
        begin
          pmOperationsCancel.Items.Clear;

          mi := TMenuItem.Create(pmOperationsCancel);
          mi.Tag := CANCEL_FILTER;
          mi.Caption := rsCancelFilter;
          mi.OnClick := @pmOperationsCancelClick;
          pmOperationsCancel.Items.Add(mi);

          mi := TMenuItem.Create(pmOperationsCancel);
          mi.Tag := CANCEL_OPERATION;
          mi.Caption := rsCancelOperation;
          mi.OnClick := @pmOperationsCancelClick;
          pmOperationsCancel.Items.Add(mi);

          pmOperationsCancel.PopUp;

          Key := 0;
        end
        else if Filtered then
        begin
          quickSearch.Finalize;
          Key := 0;
        end
        else if GetCurrentWorkType <> fvwtNone then
        begin
          StopWorkers;
          Key := 0;
        end;
      end;
  end;

  inherited DoHandleKeyDown(Key, Shift);
end;

procedure TOrderedFileView.DoHandleKeyDownWhenLoading(var Key: Word; Shift: TShiftState);
var
  bLoading: Boolean;
begin
  case Key of
    VK_ESCAPE:
      if GetCurrentWorkType <> fvwtNone then
      begin
        bLoading := IsLoadingFileList;
        StopWorkers;
        if bLoading then
          CancelLastPathChange;
        Key := 0;
      end;
  end;

  inherited DoHandleKeyDownWhenLoading(Key, Shift);
end;

procedure TOrderedFileView.DoSelectionChanged;
begin
  inherited DoSelectionChanged;
  RedrawFiles;
  UpdateRangeSelectionState;
end;

procedure TOrderedFileView.DoSelectionChanged(FileIndex: PtrInt);
begin
  inherited DoSelectionChanged;
  if IsFileIndexInRange(FileIndex) then
    RedrawFile(FileIndex);
  UpdateRangeSelectionState;
end;

procedure TOrderedFileView.EnsureDisplayProperties;
var
  VisibleFiles: TRange;
  i: Integer;
  AFileList: TFVWorkerFileList = nil;
  Worker: TFileViewWorker;
  AFile: TDisplayFile;
  HaveIcons: Boolean;
begin
  if (csDestroying in ComponentState) or
     (GetCurrentWorkType = fvwtCreate) or
     IsEmpty then
    Exit;

  VisibleFiles := GetVisibleFilesIndexes;
  HaveIcons := gShowIcons <> sim_none;

  if not gListFilesInThread then
  begin
    for i := VisibleFiles.First to VisibleFiles.Last do
    begin
      AFile := FFiles[i];
      if AFile.TextColor = clNone then
        AFile.TextColor:= gColorExt.GetColorBy(AFile.FSFile);
      if AFile.FSFile.Name <> '..' then
      begin
        if HaveIcons then
        begin
          if AFile.IconID < 0 then
            AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile,
              fspDirectAccess in FileSource.Properties, True, gShowIcons, not gIconOverlays);
          {$IF DEFINED(MSWINDOWS)}
          if gIconOverlays and (AFile.IconOverlayID < 0) then
          begin
            AFile.IconOverlayID := PixMapManager.GetIconOverlayByFile(AFile.FSFile,
                                                                      fspDirectAccess in FileSource.Properties);
          end;
          {$ENDIF}
        end;
        if FileSource.CanRetrieveProperties(AFile.FSFile, FilePropertiesNeeded) then
          FileSource.RetrieveProperties(AFile.FSFile, FilePropertiesNeeded);
      end;
    end;
  end
  else
  begin
    try
      for i := VisibleFiles.First to VisibleFiles.Last do
      begin
        AFile := FFiles[i];
        if AFile.TextColor = clNone then
          AFile.TextColor:= gColorExt.GetColorBy(AFile.FSFile);
        if (AFile.FSFile.Name <> '..') and
           (FileSource.CanRetrieveProperties(AFile.FSFile, FilePropertiesNeeded) or
            (HaveIcons and ((AFile.IconID < 0)
             {$IF DEFINED(MSWINDOWS)}
             or (gIconOverlays and (AFile.IconOverlayID < 0))
             {$ENDIF}
           ))) then
        begin
          if not Assigned(AFileList) then
            AFileList := TFVWorkerFileList.Create;
          AFileList.AddClone(AFile, AFile);
        end;
      end;

      if Assigned(AFileList) and (AFileList.Count > 0) then
      begin
        Worker := TFilePropertiesRetriever.Create(
          FileSource,
          WorkersThread,
          FilePropertiesNeeded,
          @PropertiesRetrieverOnUpdate,
          AFileList);

        AddWorker(Worker, False);
        WorkersThread.QueueFunction(@Worker.StartParam);
      end;

    finally
      AFileList.Free;
    end;
  end;
end;

function TOrderedFileView.GetActiveDisplayFile: TDisplayFile;
var
  Index: PtrInt;
begin
  Index := GetActiveFileIndex;
  if IsFileIndexInRange(Index) then
    Result := FFiles[Index]
  else
    Result := nil;
end;

function TOrderedFileView.IsFileIndexInRange(FileIndex: PtrInt): Boolean;
begin
  Result := InRange(FileIndex, 0, FFiles.Count - 1);
end;

procedure TOrderedFileView.lblFilterClick(Sender: TObject);
begin
  quickSearch.Execute(qsFilter, []);
end;

procedure TOrderedFileView.MarkFilesWithCheck(FromIndex, ToIndex: PtrInt; bSelect: Boolean);
begin
  if FromIndex = ToIndex then
  begin
    MarkFile(FFiles[FromIndex], bSelect, False);
    DoSelectionChanged(FromIndex);
  end
  else
    MarkFiles(FromIndex, ToIndex, bSelect);
end;

procedure TOrderedFileView.pmOperationsCancelClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    case (Sender as TMenuItem).Tag of
      CANCEL_FILTER:
        quickSearch.Finalize;
      CANCEL_OPERATION:
        StopWorkers;
    end;
  end;
end;

procedure TOrderedFileView.quickSearchChangeFilter(Sender: TObject; AFilterText: UTF8String; const AFilterOptions: TQuickSearchOptions);
begin
  Active := True;

  // position in file before filtering, otherwise position could be lost if
  // current file is filtered out causing jumps
  SearchFile(AFilterText, AFilterOptions);

  SetFileFilter(AFilterText, AFilterOptions);

  lblFilter.Caption := Format('(%s: %s)', [rsFilterStatus, AFilterText]);
  lblFilter.Visible := Filtered;
end;

procedure TOrderedFileView.quickSearchChangeSearch(Sender: TObject; ASearchText: UTF8String; const ASearchOptions: TQuickSearchOptions; ASearchDirection: TQuickSearchDirection);
begin
  Active := True;
  SearchFile(ASearchText, ASearchOptions, ASearchDirection);
end;

procedure TOrderedFileView.quickSearchExecute(Sender: TObject);
begin
  Active := True;
  ChooseFile(GetActiveDisplayFile);
end;

procedure TOrderedFileView.quickSearchHide(Sender: TObject);
begin
  if CanFocus then
    SetFocus;
end;

procedure TOrderedFileView.SearchFile(SearchTerm: UTF8String; SearchOptions: TQuickSearchOptions; SearchDirection: TQuickSearchDirection);
var
  StartIndex, Index: PtrInt;
  Result: Boolean;
  sFileName,
  sSearchName,
  sSearchNameNoExt,
  sSearchExt : UTF8String;
  AFile: TFile;

  function NextIndexWrap(Index: PtrInt): PtrInt;
  begin
    Result := Index + 1;
    if Result = FFiles.Count then
      Result := 0;
  end;
  function PrevIndexWrap(Index: PtrInt): PtrInt;
  begin
    Result := Index - 1;
    if Result < 0 then
      Result := FFiles.Count - 1;
  end;

begin
  if IsEmpty then
    Exit;

  if SearchOptions.SearchCase = qscInsensitive then
    sSearchName := UTF8LowerCase(SearchTerm)
  else
    sSearchName := SearchTerm;

  if Pos('.', sSearchName) <> 0 then
  begin
    sSearchNameNoExt := ExtractOnlyFileName(sSearchName);
    sSearchExt := ExtractFileExt(sSearchName);
    if not (qsmBeginning in SearchOptions.Match) then
      sSearchNameNoExt := '*' + sSearchNameNoExt;
    if not (qsmEnding in SearchOptions.Match) then
      sSearchNameNoExt := sSearchNameNoExt + '*';
    sSearchName := sSearchNameNoExt + sSearchExt + '*';
  end
  else
  begin
    if not (qsmBeginning in SearchOptions.Match) then
      sSearchName := '*' + sSearchName;
    sSearchName := sSearchName + '*';
  end;

  Index := GetActiveFileIndex; // start search from current position
  if not IsFileIndexInRange(Index) then
    Index := 0;
  case SearchDirection of
    qsdFirst:
      Index := 0;                  // begin search from first file
    qsdLast:
      Index := FFiles.Count - 1;   // begin search from last file
    qsdNext:
      Index := NextIndexWrap(Index);   // begin search from next file
    qsdPrevious:
      Index := PrevIndexWrap(Index);   // begin search from previous file
  end;

  StartIndex := Index;
  try
    repeat
      Result := True;
      AFile := FFiles[Index].FSFile;

      if (SearchOptions.Items = qsiFiles) and
         (AFile.IsDirectory or
          AFile.IsLinkToDirectory) then
        Result := False;

      if (SearchOptions.Items = qsiDirectories) and
         not AFile.IsDirectory and
         not AFile.IsLinkToDirectory then
        Result := False;

      sFileName := AFile.Name;

      if SearchOptions.SearchCase = qscInsensitive then
        sFileName := UTF8LowerCase(sFileName);

      if not MatchesMask(sFileName, sSearchName, SearchOptions.SearchCase = qscSensitive) then
        Result := False;

      if Result then
      begin
        SetActiveFile(Index);
        Exit;
      end;

      // check next file depending on search direction
      if SearchDirection in [qsdNone, qsdFirst, qsdNext] then
        Index := NextIndexWrap(Index)
      else
        Index := PrevIndexWrap(Index);

    until Index = StartIndex;

  except
    on EConvertError do; // bypass
    else
      raise;
  end;
end;

procedure TOrderedFileView.Selection(Key: Word; CurIndex: PtrInt);
  procedure OneLess;
  begin
    if CurIndex > FRangeSelectionStartIndex then
      Dec(CurIndex)
    else if CurIndex < FRangeSelectionStartIndex then
      Inc(CurIndex);
  end;
begin
  // Key value doesn't neccessarily matter.
  // It just needs to correspond to scroll positions (similar to TScrollCode).
  case Key of
    VK_HOME, VK_END: ;
    VK_PRIOR, VK_UP, VK_LEFT:
      if CurIndex > 0 then
        OneLess;
    VK_NEXT, VK_DOWN, VK_RIGHT:
      if CurIndex < FFiles.Count - 1 then
        OneLess;
    else
      Exit;
  end;

  SelectRange(CurIndex);
end;

procedure TOrderedFileView.SelectRange(FileIndex: PtrInt);
begin
  // Initially select file at starting point.
  if FRangeSelectionStartIndex = FRangeSelectionEndIndex then
    MarkFilesWithCheck(FRangeSelectionStartIndex, FRangeSelectionEndIndex, FRangeSelectionState);

  if FileIndex <> FRangeSelectionEndIndex then
    begin
      if FileIndex < FRangeSelectionStartIndex then
        begin
          // Focused file is before selection startpoint.
          // If previously selection was from startpoint forwards deselect all files after startpoint.
          if FRangeSelectionEndIndex > FRangeSelectionStartIndex then
            begin
              MarkFilesWithCheck(FRangeSelectionStartIndex + 1, FRangeSelectionEndIndex, not FRangeSelectionState);
              FRangeSelectionEndIndex := FRangeSelectionStartIndex;
            end;

          if FileIndex > FRangeSelectionEndIndex then
            // Decrease selection range.
            MarkFilesWithCheck(FRangeSelectionEndIndex, FileIndex - 1, not FRangeSelectionState)
          else if FileIndex < FRangeSelectionEndIndex then
            // Increase selection range.
            MarkFilesWithCheck(FileIndex, FRangeSelectionEndIndex - 1, FRangeSelectionState);
        end
      else
        begin
          // Focused file is after selection startpoint.
          // If previously selection was from startpoint backwards deselect all files before startpoint.
          if FRangeSelectionEndIndex < FRangeSelectionStartIndex then
            begin
              MarkFilesWithCheck(FRangeSelectionEndIndex, FRangeSelectionStartIndex - 1, not FRangeSelectionState);
              FRangeSelectionEndIndex := FRangeSelectionStartIndex;
            end;

          if FileIndex > FRangeSelectionEndIndex then
            // Increase selection range.
            MarkFilesWithCheck(FRangeSelectionEndIndex + 1, FileIndex, FRangeSelectionState)
          else if FileIndex < FRangeSelectionEndIndex then
            // Decrease selection range.
            MarkFilesWithCheck(FileIndex + 1, FRangeSelectionEndIndex, not FRangeSelectionState);
        end;

      FRangeSelectionEndIndex := FileIndex;
    end;
end;

procedure TOrderedFileView.SetActiveFile(aFilePath: String);
begin
  if GetCurrentWorkType = fvwtCreate then
  begin
    // File list is currently loading - remember requested file for later.
    RequestedActiveFile := aFilePath;
  end
  else
  begin
    // First try to select the file in the current file list.
    // If not found save it for later selection (possibly after reload).
    if SetActiveFileNow(aFilePath) then
      RequestedActiveFile := ''
    else
      RequestedActiveFile := aFilePath;
  end;
end;

function TOrderedFileView.SetActiveFileNow(aFilePath: String): Boolean;
var
  Index: PtrInt;
begin
  if aFilePath <> '' then // find correct cursor position in Panel (drawgrid)
  begin
    if FileSource.GetPathType(aFilePath) = ptAbsolute then
    begin
      for Index := 0 to FFiles.Count - 1 do
      begin
        if FFiles[Index].FSFile.FullPath = aFilePath then
        begin
          FUpdatingActiveFile := True;
          SetActiveFile(Index);
          FUpdatingActiveFile := False;
          SetLastActiveFile(Index);
          Exit(True);
        end;
      end;
    end
    else
    begin
      for Index := 0 to FFiles.Count - 1 do
      begin
        if FFiles[Index].FSFile.Name = aFilePath then
        begin
          FUpdatingActiveFile := True;
          SetActiveFile(Index);
          FUpdatingActiveFile := False;
          SetLastActiveFile(Index);
          Exit(True);
        end;
      end;
    end;
  end;
  Result := False;
end;

procedure TOrderedFileView.SetLastActiveFile(FileIndex: PtrInt);
begin
  if IsFileIndexInRange(FileIndex) then
  begin
    LastActiveFile := FFiles[FileIndex].FSFile.FullPath;
    FLastActiveFileIndex := FileIndex;
  end;
end;

procedure TOrderedFileView.UpdateRangeSelectionState;
var
  NewSelectionState: Boolean;
begin
  if not FRangeSelecting then
  begin
    if IsFileIndexInRange(FRangeSelectionStartIndex) then
    begin
      NewSelectionState := not FFiles[FRangeSelectionStartIndex].Selected;
      if (FRangeSelectionState <> NewSelectionState) and
         (FRangeSelectionStartIndex = FRangeSelectionEndIndex) then
      begin
        // Selection of starting point has changed.
      end
      else
      begin
        // Update was called but selection of starting point didn't change.
        // That means some other file's selection changed - reset starting point.
        FRangeSelectionStartIndex := GetActiveFileIndex;
        FRangeSelectionEndIndex := FRangeSelectionStartIndex;
      end;
      FRangeSelectionState := NewSelectionState;
    end;
  end;
end;

end.

