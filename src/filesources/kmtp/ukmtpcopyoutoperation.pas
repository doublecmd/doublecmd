{
   Double Commander
   -------------------------------------------------------------------------
   Copy-out operation for KDE KMTP file source

   Copyright (C) 2026 Alexander Koblov (alexx2000@mail.ru)

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

unit uKmtpCopyOutOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLog, uGlobs,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFileSourceTreeBuilder,
  uFileSystemUtil,
  uFile,
  uKmtpFileSource;

type

  { TKmtpCopyOutOperation }

  TKmtpCopyOutOperation = class(TFileSourceCopyOutOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FSourceFilesTree: TFileTree;
    FStatistics: TFileSourceCopyOperationStatistics;
    FRenameNameMask, FRenameExtMask: String;
    FRenamingFiles, FRenamingRootDir: Boolean;
    FRootDir: TFile;
    FOldDoneBytes: Int64;
    FSkipAnyError: Boolean;
    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;
    function OnProgress(Transferred, Total: UInt64): Boolean;
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function TargetExists(aNode: TFileTreeNode; var AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
    function DirExists(aFile: TFile; const AbsoluteTargetFileName: String; AllowCopyInto: Boolean): TFileSourceOperationOptionDirectoryExists;
    function FileExists(aFile: TFile; var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
    function ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
    function ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    function ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    procedure CountStatistics(aNode: TFileTreeNode);
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  Math, DateUtils, Forms, DCBasicTypes, DCDateTimeUtils, DCOSUtils, DCStrUtils,
  uFindEx, uFileProcs, uDCUtils, uLng, uShowMsg, uKmtpFileSourceUtil, uKde;

{ TKmtpCopyOutOperation }

constructor TKmtpCopyOutOperation.Create(aSourceFileSource: IFileSource;
  aTargetFileSource: IFileSource; var theSourceFiles: TFiles; aTargetPath: String);
begin
  FKmtpFileSource := aSourceFileSource as IKmtpFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;
end;

destructor TKmtpCopyOutOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TKmtpCopyOutOperation.Initialize;
var
  TreeBuilder: TKmtpTreeBuilder;
  aFile: TFile;
begin
  FStatistics := RetrieveStatistics;
  TreeBuilder := TKmtpTreeBuilder.Create(FKmtpFileSource, @AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption := SymLinkOption;
    TreeBuilder.BuildFromFiles(SourceFiles);
    FSourceFilesTree := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  SplitFileMask(RenameMask, FRenameNameMask, FRenameExtMask);
  FRenamingFiles := (RenameMask <> '*.*') and (RenameMask <> '');
  FRenamingRootDir := False;
  if (FSourceFilesTree.SubNodesCount = 1) and FRenamingFiles then
  begin
    aFile := FSourceFilesTree.SubNodes[0].TheFile;
    if (aFile.IsDirectory or aFile.IsLinkToDirectory) and (not ContainsWildcards(RenameMask)) then
    begin
      FRenamingFiles := False;
      FRenamingRootDir := True;
      FRootDir := aFile;
    end;
  end;
end;

procedure TKmtpCopyOutOperation.MainExecute;
begin
  ProcessNode(FSourceFilesTree, FTargetPath);
end;

function TKmtpCopyOutOperation.OnProgress(Transferred, Total: UInt64): Boolean;
begin
  if State = fsosStopping then
    Exit(False);
  FStatistics.CurrentFileDoneBytes := Int64(Transferred);
  if Total > 0 then
    FStatistics.CurrentFileTotalBytes := Int64(Total);
  FStatistics.DoneBytes := FOldDoneBytes + Int64(Transferred);
  UpdateStatistics(FStatistics);
  CheckOperationState;
  Result := True;
end;

procedure TKmtpCopyOutOperation.LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:   if not (log_errors in gLogOptions) then Exit;
    lmtInfo:    if not (log_info in gLogOptions) then Exit;
    lmtSuccess: if not (log_success in gLogOptions) then Exit;
  end;
  if logOptions <= gLogOptions then
    logWrite(Thread, sMessage, logMsgType);
end;

procedure TKmtpCopyOutOperation.QuestionActionHandler(Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
    ShowCompareFilesUI(FCurrentFile, FCurrentTargetFilePath);
end;

function TKmtpCopyOutOperation.DirExists(aFile: TFile; const AbsoluteTargetFileName: String;
  AllowCopyInto: Boolean): TFileSourceOperationOptionDirectoryExists;
var
  PossibleResponses: array of TFileSourceOperationUIResponse = nil;
  DefaultOkResponse: TFileSourceOperationUIResponse;

  procedure AddResponse(R: TFileSourceOperationUIResponse);
  begin
    SetLength(PossibleResponses, Length(PossibleResponses) + 1);
    PossibleResponses[High(PossibleResponses)] := R;
  end;

begin
  if FDirExistsOption <> fsoodeNone then
    Exit(FDirExistsOption);

  if AllowCopyInto then
  begin
    AddResponse(fsourCopyInto);
    AddResponse(fsourCopyIntoAll);
    DefaultOkResponse := fsourCopyInto;
  end
  else
    DefaultOkResponse := fsourSkip;
  AddResponse(fsourSkip);
  AddResponse(fsourSkipAll);
  AddResponse(fsourCancel);

  case AskQuestion(Format(rsMsgFolderExistsRwrt, [AbsoluteTargetFileName]), '',
                   PossibleResponses, DefaultOkResponse, fsourSkip) of
    fsourCopyInto:    Result := fsoodeCopyInto;
    fsourCopyIntoAll: begin FDirExistsOption := fsoodeCopyInto; Result := fsoodeCopyInto; end;
    fsourSkip:        Result := fsoodeSkip;
    fsourSkipAll:     begin FDirExistsOption := fsoodeSkip; Result := fsoodeSkip; end;
    else              RaiseAbortOperation;
  end;
end;

function TKmtpCopyOutOperation.FileExists(aFile: TFile;
  var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..9] of TFileSourceOperationUIResponse =
    (fsourOverwrite, fsourSkip, fsourRenameSource, fsourOverwriteAll,
     fsourSkipAll, fsourOverwriteOlder, fsourOverwriteSmaller,
     fsourCancel, fsouaCompare, fsourOverwriteLarger);
var
  Answer: Boolean;
  Msg, NewName: String;
  TargetStat: TSearchRecEx;

  function TargetSize: Int64;
  begin
    Result := TargetStat.Size;
  end;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if CompareDateTime(RecodeMilliSecond(aFile.ModificationTime, 0),
                       FileTimeToDateTime(TargetStat.Time)) = GreaterThanValue then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  if FindFirstEx(AbsoluteTargetFileName, 0, TargetStat) = 0 then
    FindCloseEx(TargetStat)
  else
  begin
    TargetStat.Size := 0;
    TargetStat.Time := 0;
  end;

  case FFileExistsOption of
    fsoofeNone:
      repeat
        Answer := True;
        Msg := rsMsgFileExistsOverwrite + LineEnding + AbsoluteTargetFileName + LineEnding +
               Format(rsMsgFileExistsFileInfo, [IntToStrTS(TargetSize),
                      DateTimeToStr(FileTimeToDateTime(TargetStat.Time))]) + LineEnding +
               LineEnding + rsMsgFileExistsWithFile + LineEnding + aFile.FullPath + LineEnding +
               Format(rsMsgFileExistsFileInfo, [IntToStrTS(aFile.Size), DateTimeToStr(aFile.ModificationTime)]);
        FCurrentFile := aFile;
        FCurrentTargetFilePath := AbsoluteTargetFileName;
        case AskQuestion(Msg, '', Responses, fsourOverwrite, fsourSkip, @QuestionActionHandler) of
          fsourOverwrite:        Exit(fsoofeOverwrite);
          fsourSkip:             Exit(fsoofeSkip);
          fsourOverwriteAll:     begin FFileExistsOption := fsoofeOverwrite; Exit(fsoofeOverwrite); end;
          fsourSkipAll:          begin FFileExistsOption := fsoofeSkip; Exit(fsoofeSkip); end;
          fsourOverwriteOlder:   begin FFileExistsOption := fsoofeOverwriteOlder; Exit(OverwriteOlder); end;
          fsourOverwriteSmaller:
            begin
              FFileExistsOption := fsoofeOverwriteSmaller;
              if aFile.Size > TargetSize then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
            end;
          fsourOverwriteLarger:
            begin
              FFileExistsOption := fsoofeOverwriteLarger;
              if aFile.Size < TargetSize then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
            end;
          fsourRenameSource:
            begin
              NewName := ExtractFileName(AbsoluteTargetFileName);
              Answer := ShowInputQuery(Thread, Application.Title, rsEditNewFileName, NewName);
              if Answer and (NewName <> '') then
              begin
                AbsoluteTargetFileName := ExtractFilePath(AbsoluteTargetFileName) + NewName;
                Exit(fsoofeAutoRenameSource);
              end;
            end;
          else
            RaiseAbortOperation;
        end;
      until not Answer;
    fsoofeOverwriteOlder:   Result := OverwriteOlder;
    fsoofeOverwriteSmaller: if aFile.Size > TargetSize then Result := fsoofeOverwrite else Result := fsoofeSkip;
    fsoofeOverwriteLarger:  if aFile.Size < TargetSize then Result := fsoofeOverwrite else Result := fsoofeSkip;
    else
      Result := FFileExistsOption;
  end;
end;

function TKmtpCopyOutOperation.TargetExists(aNode: TFileTreeNode;
  var AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
begin
  repeat
    if mbDirectoryExists(AbsoluteTargetFileName) then
    begin
      if DirExists(aNode.TheFile, AbsoluteTargetFileName, aNode.TheFile.IsDirectory) = fsoodeSkip then
        Exit(fsoterSkip)
      else
        Exit(fsoterAddToTarget);
    end
    else if mbFileExists(AbsoluteTargetFileName) then
    begin
      case FileExists(aNode.TheFile, AbsoluteTargetFileName) of
        fsoofeSkip:             Exit(fsoterSkip);
        fsoofeAutoRenameSource: Result := fsoterRenamed;
        fsoofeOverwrite:        Exit(fsoterDeleted);
        else                    Exit(fsoterSkip);
      end;
    end
    else
      Exit(fsoterNotExists);
  until Result <> fsoterRenamed;
end;

function TKmtpCopyOutOperation.ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
var
  I: Integer;
  SubNode: TFileTreeNode;
  aFile: TFile;
  TargetName: String;
begin
  Result := True;
  for I := 0 to aNode.SubNodesCount - 1 do
  begin
    SubNode := aNode.SubNodes[I];
    aFile := SubNode.TheFile;

    if FRenamingRootDir and (aFile = FRootDir) then
      TargetName := CurrentTargetPath + RenameMask
    else if FRenamingFiles then
      TargetName := CurrentTargetPath + uFileSystemUtil.ApplyRenameMask(FKmtpFileSource as IFileSource, aFile, FRenameNameMask, FRenameExtMask)
    else
      TargetName := CurrentTargetPath + aFile.Name;

    FStatistics.CurrentFileFrom := aFile.FullPath;
    FStatistics.CurrentFileTo := TargetName;
    FStatistics.CurrentFileTotalBytes := aFile.Size;
    FStatistics.CurrentFileDoneBytes := 0;
    UpdateStatistics(FStatistics);

    if aFile.IsDirectory then
    begin
      if not ProcessDirectory(SubNode, TargetName) then
        Result := False;
    end
    else
    begin
      if not ProcessFile(SubNode, TargetName) then
        Result := False;
    end;
    CheckOperationState;
  end;
end;

function TKmtpCopyOutOperation.ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
begin
  case TargetExists(aNode, AbsoluteTargetFileName) of
    fsoterSkip:
      begin
        CountStatistics(aNode);
        Exit(False);
      end;
    fsoterNotExists:
      begin
        if not mbForceDirectory(AbsoluteTargetFileName) then
        begin
          LogMessage(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), [log_vfs_op], lmtError);
          if not FSkipAnyError then
            if AskQuestion(Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
              RaiseAbortOperation;
          CountStatistics(aNode);
          Exit(False);
        end;
      end;
    fsoterAddToTarget: ;
  end;

  Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
end;

function TKmtpCopyOutOperation.ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  DevPath, StorPath, RelPath, ErrMsg: String;
  Level: Integer;
  FileTime: TFileTime;
begin
  Result := False;
  FOldDoneBytes := FStatistics.DoneBytes;

  if (not FKmtpFileSource.ResolveTarget(aNode.TheFile.FullPath, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
  begin
    if not FSkipAnyError then
      if AskQuestion(rsMsgErrNotSupported, '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    Exit(False);
  end;

  if TargetExists(aNode, AbsoluteTargetFileName) = fsoterSkip then
  begin
    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
    UpdateStatistics(FStatistics);
    Exit(True);
  end;

  repeat
    Result := KmtpTransferFileFD(StorPath, RelPath, AbsoluteTargetFileName, False,
                                 UInt64(Max(Int64(0), aNode.TheFile.Size)), @OnProgress, ErrMsg);
    if Result then
    begin
      if (caoCopyTime in CopyAttributesOptions) and (aNode.TheFile.ModificationTime > 0) then
      begin
        FileTime := DateTimeToFileTime(aNode.TheFile.ModificationTime);
        mbFileSetTime(AbsoluteTargetFileName, FileTime, 0, 0);
      end;
      Break;
    end;
    if State = fsosStopping then
      RaiseAbortOperation;
    if FSkipAnyError then
      Break;
    case AskQuestion(ErrMsg, '', [fsourRetry, fsourSkip, fsourSkipAll, fsourCancel], fsourRetry, fsourCancel) of
      fsourRetry:   ;
      fsourSkip:    Break;
      fsourSkipAll: begin FSkipAnyError := True; Break; end;
      else          RaiseAbortOperation;
    end;
  until False;

  if Result then
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogCopy, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]), [log_vfs_op], lmtSuccess)
  else
    LogMessage(Format(rsMsgLogError + rsMsgLogCopy, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]) + LineEnding + ErrMsg, [log_vfs_op], lmtError);

  FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
  FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
  UpdateStatistics(FStatistics);
end;

procedure TKmtpCopyOutOperation.CountStatistics(aNode: TFileTreeNode);
  procedure CountNode(N: TFileTreeNode);
  var
    I: Integer;
  begin
    if N.TheFile.IsDirectory then
      for I := 0 to N.SubNodesCount - 1 do
        CountNode(N.SubNodes[I])
    else
    begin
      FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
      FStatistics.DoneBytes := FStatistics.DoneBytes + N.TheFile.Size;
    end;
  end;
begin
  CountNode(aNode);
  UpdateStatistics(FStatistics);
end;

end.
