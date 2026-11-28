{
   Double Commander
   -------------------------------------------------------------------------
   Copy-in operation for KDE KMTP file source

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

unit uKmtpCopyInOperation;

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
  uFile,
  uKmtpFileSource;

type

  { TKmtpCopyInOperation }

  TKmtpCopyInOperation = class(TFileSourceCopyInOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FSourceFilesTree: TFileTree;
    FStatistics: TFileSourceCopyOperationStatistics;
    FRenameNameMask, FRenameExtMask: String;
    FRenamingFiles, FRenamingRootDir: Boolean;
    FRootDir: TFile;
    FOldDoneBytes: Int64;
    FSkipAnyError: Boolean;
    function OnProgress(Transferred, Total: UInt64): Boolean;
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    function EnsureTargetFolder(const AStoragePath, ARelPath: String): Boolean;
    function CheckTargetDirExists(const AStoragePath, ARelPath, AFullTarget: String): TFileSourceOperationOptionDirectoryExists;
    function CheckTargetFileExists(aSourceFile: TFile; const AStoragePath: String;
      var ARelPath, AFullTarget: String): TFileSourceOperationOptionFileExists;
    function ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
    function ProcessDirectory(aNode: TFileTreeNode; const AbsoluteTargetFileName: String): Boolean;
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
  Math, DateUtils, Forms, DCBasicTypes, DCDateTimeUtils, DCStrUtils,
  uDCUtils, uLng, uShowMsg, uFileSystemUtil, uKde;

{ TKmtpCopyInOperation }

constructor TKmtpCopyInOperation.Create(aSourceFileSource: IFileSource;
  aTargetFileSource: IFileSource; var theSourceFiles: TFiles; aTargetPath: String);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;
end;

destructor TKmtpCopyInOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TKmtpCopyInOperation.Initialize;
var
  TreeBuilder: TFileSystemTreeBuilder;
  aFile: TFile;
begin
  FStatistics := RetrieveStatistics;
  TreeBuilder := TFileSystemTreeBuilder.Create(@AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption := fsooslFollow;
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

procedure TKmtpCopyInOperation.MainExecute;
begin
  ProcessNode(FSourceFilesTree, FTargetPath);
end;

function TKmtpCopyInOperation.OnProgress(Transferred, Total: UInt64): Boolean;
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

procedure TKmtpCopyInOperation.LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:   if not (log_errors in gLogOptions) then Exit;
    lmtInfo:    if not (log_info in gLogOptions) then Exit;
    lmtSuccess: if not (log_success in gLogOptions) then Exit;
  end;
  if logOptions <= gLogOptions then
    logWrite(Thread, sMessage, logMsgType);
end;

function TKmtpCopyInOperation.EnsureTargetFolder(const AStoragePath, ARelPath: String): Boolean;
var
  Meta: TKmtpFileInfo;
begin
  if (ARelPath = '') or (ARelPath = '/') then
    Exit(True);
  if KmtpGetFileMetadata(AStoragePath, ARelPath, Meta) then
    Exit(Meta.FileType = KMTP_MIME_DIRECTORY);
  Result := KmtpCreateFolder(AStoragePath, ARelPath);
end;

function TKmtpCopyInOperation.CheckTargetDirExists(const AStoragePath, ARelPath,
  AFullTarget: String): TFileSourceOperationOptionDirectoryExists;
var
  Meta: TKmtpFileInfo;
begin
  if not KmtpGetFileMetadata(AStoragePath, ARelPath, Meta) then
    Exit(fsoodeCopyInto);
  if FDirExistsOption <> fsoodeNone then
    Exit(FDirExistsOption);
  case AskQuestion(Format(rsMsgFolderExistsRwrt, [AFullTarget]), '',
                   [fsourCopyInto, fsourCopyIntoAll, fsourSkip, fsourSkipAll, fsourCancel],
                   fsourCopyInto, fsourSkip) of
    fsourCopyInto:    Result := fsoodeCopyInto;
    fsourCopyIntoAll: begin FDirExistsOption := fsoodeCopyInto; Result := fsoodeCopyInto; end;
    fsourSkip:        Result := fsoodeSkip;
    fsourSkipAll:     begin FDirExistsOption := fsoodeSkip; Result := fsoodeSkip; end;
    else              RaiseAbortOperation;
  end;
end;

function TKmtpCopyInOperation.CheckTargetFileExists(aSourceFile: TFile; const AStoragePath: String;
  var ARelPath, AFullTarget: String): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..8] of TFileSourceOperationUIResponse =
    (fsourOverwrite, fsourSkip, fsourRenameSource, fsourOverwriteAll,
     fsourSkipAll, fsourOverwriteOlder, fsourOverwriteSmaller,
     fsourCancel, fsourOverwriteLarger);
var
  Meta: TKmtpFileInfo;
  Answer: Boolean;
  Msg, NewName: String;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if CompareDateTime(RecodeMilliSecond(aSourceFile.ModificationTime, 0),
                       UnixFileTimeToDateTime(TUnixFileTime(Meta.ModDate))) = GreaterThanValue then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  while KmtpGetFileMetadata(AStoragePath, ARelPath, Meta) do
  begin
    case FFileExistsOption of
      fsoofeNone:
        begin
          Answer := True;
          Msg := rsMsgFileExistsOverwrite + LineEnding + AFullTarget + LineEnding +
                 Format(rsMsgFileExistsFileInfo, [IntToStrTS(Int64(Meta.FileSize)),
                        DateTimeToStr(UnixFileTimeToDateTime(TUnixFileTime(Meta.ModDate)))]) + LineEnding +
                 LineEnding + rsMsgFileExistsWithFile + LineEnding + aSourceFile.FullPath + LineEnding +
                 Format(rsMsgFileExistsFileInfo, [IntToStrTS(aSourceFile.Size), DateTimeToStr(aSourceFile.ModificationTime)]);
          case AskQuestion(Msg, '', Responses, fsourOverwrite, fsourSkip) of
            fsourOverwrite:        Exit(fsoofeOverwrite);
            fsourSkip:             Exit(fsoofeSkip);
            fsourOverwriteAll:     begin FFileExistsOption := fsoofeOverwrite; Exit(fsoofeOverwrite); end;
            fsourSkipAll:          begin FFileExistsOption := fsoofeSkip; Exit(fsoofeSkip); end;
            fsourOverwriteOlder:   begin FFileExistsOption := fsoofeOverwriteOlder; Exit(OverwriteOlder); end;
            fsourOverwriteSmaller:
              begin
                FFileExistsOption := fsoofeOverwriteSmaller;
                if aSourceFile.Size > Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
              end;
            fsourOverwriteLarger:
              begin
                FFileExistsOption := fsoofeOverwriteLarger;
                if aSourceFile.Size < Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
              end;
            fsourRenameSource:
              begin
                NewName := ExtractFileName(AFullTarget);
                Answer := ShowInputQuery(Thread, Application.Title, rsEditNewFileName, NewName);
                if Answer and (NewName <> '') then
                begin
                  AFullTarget := ExtractFilePath(AFullTarget) + NewName;
                  ARelPath := ExtractFilePath(ARelPath) + NewName;
                  Continue;
                end;
              end;
            else
              RaiseAbortOperation;
          end;
        end;
      fsoofeOverwriteOlder:   Exit(OverwriteOlder);
      fsoofeOverwriteSmaller: if aSourceFile.Size > Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
      fsoofeOverwriteLarger:  if aSourceFile.Size < Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
      else
        Exit(FFileExistsOption);
    end;
  end;
  Result := fsoofeOverwrite;
end;

function TKmtpCopyInOperation.ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
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
      TargetName := CurrentTargetPath + ApplyRenameMask(SourceFileSource, aFile, FRenameNameMask, FRenameExtMask)
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

function TKmtpCopyInOperation.ProcessDirectory(aNode: TFileTreeNode; const AbsoluteTargetFileName: String): Boolean;
var
  DevPath, StorPath, RelPath: String;
  Level: Integer;
  Meta: TKmtpFileInfo;
begin
  if (not FKmtpFileSource.ResolveTarget(AbsoluteTargetFileName, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') then
  begin
    if not FSkipAnyError then
      if AskQuestion(rsMsgErrNotSupported, '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    CountStatistics(aNode);
    Exit(False);
  end;

  if KmtpGetFileMetadata(StorPath, RelPath, Meta) then
  begin
    if CheckTargetDirExists(StorPath, RelPath, AbsoluteTargetFileName) = fsoodeSkip then
    begin
      CountStatistics(aNode);
      Exit(False);
    end;
  end
  else if not EnsureTargetFolder(StorPath, RelPath) then
  begin
    LogMessage(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), [log_vfs_op], lmtError);
    if not FSkipAnyError then
      if AskQuestion(Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    CountStatistics(aNode);
    Exit(False);
  end;

  Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
end;

function TKmtpCopyInOperation.ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  DevPath, StorPath, RelPath, ErrMsg: String;
  Level: Integer;
  Meta: TKmtpFileInfo;
  Opt: TFileSourceOperationOptionFileExists;
begin
  Result := False;
  FOldDoneBytes := FStatistics.DoneBytes;

  if (not FKmtpFileSource.ResolveTarget(AbsoluteTargetFileName, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
  begin
    if not FSkipAnyError then
      if AskQuestion(rsMsgErrNotSupported, '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    Exit(False);
  end;

  Opt := CheckTargetFileExists(aNode.TheFile, StorPath, RelPath, AbsoluteTargetFileName);
  if Opt = fsoofeSkip then
  begin
    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
    UpdateStatistics(FStatistics);
    Exit(True);
  end;

  if KmtpGetFileMetadata(StorPath, RelPath, Meta) then
    KmtpDeleteObject(StorPath, RelPath);

  repeat
    Result := KmtpTransferFileFD(StorPath, RelPath, aNode.TheFile.FullPath, True,
                                 UInt64(Max(Int64(0), aNode.TheFile.Size)), @OnProgress, ErrMsg);
    if Result then
      Break;
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

procedure TKmtpCopyInOperation.CountStatistics(aNode: TFileTreeNode);
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
