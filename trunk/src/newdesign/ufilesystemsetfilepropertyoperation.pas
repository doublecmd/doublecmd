unit uFileSystemSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile,
  uFileProperty;

type

  TFileSystemSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  private
    FFullFilesTree: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetFilePropertyOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;

    function RenameFile(const OldName: UTF8String; NewName: UTF8String): TSetFilePropertyResult;

  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uGlobs, uLng, DCDateTimeUtils, uFileSystemUtil,
  uFileSourceOperationUI, DCOSUtils, DCStrUtils, DCBasicTypes
  {$IF DEFINED(MSWINDOWS)}
    , Windows, ShellAPI, LCLProc
  {$ELSEIF DEFINED(UNIX)}
    , BaseUnix, FileUtil
  {$ENDIF}
  ;

constructor TFileSystemSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                       var theTargetFiles: TFiles;
                                                       var theNewProperties: TFileProperties);
begin
  FSymLinkOption := fsooslNone;
  FFullFilesTree := nil;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);

  // Assign after calling inherited constructor.
  FSupportedProperties := [fpName,
                           fpAttributes,
                           fpModificationTime,
                           fpCreationTime,
                           fpLastAccessTime];
end;

destructor TFileSystemSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTree) then
      FreeAndNil(FFullFilesTree);
  end;
end;

procedure TFileSystemSetFilePropertyOperation.Initialize;
var
  TotalBytes: Int64;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTree := TargetFiles;
      FStatistics.TotalFiles := FFullFilesTree.Count;
    end
  else
    begin
      FillAndCount(TargetFiles, True, False,
                   FFullFilesTree,
                   FStatistics.TotalFiles,
                   TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TFileSystemSetFilePropertyOperation.MainExecute;
var
  aFile: TFile;
  aTemplateFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex];

    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    if Assigned(TemplateFiles) and (CurrentFileIndex < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[CurrentFileIndex]
    else
      aTemplateFile := nil;

    SetProperties(aFile, aTemplateFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

procedure TFileSystemSetFilePropertyOperation.Finalize;
begin
end;

function TFileSystemSetFilePropertyOperation.SetNewProperty(aFile: TFile;
                                                            aTemplateProperty: TFileProperty): TSetFilePropertyResult;
begin
  Result := sfprSuccess;

  try
    case aTemplateProperty.GetID of
      fpName:
        if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
        begin
          Result := RenameFile(
            aFile.FullPath,
            (aTemplateProperty as TFileNameProperty).Value);
        end
        else
          Result := sfprSkipped;

      fpAttributes:
        if (aTemplateProperty as TFileAttributesProperty).Value <>
           (aFile.Properties[fpAttributes] as TFileAttributesProperty).Value then
        begin
          if mbFileSetAttr(
            aFile.FullPath,
            (aTemplateProperty as TFileAttributesProperty).Value) <> 0 then
          begin
            Result := sfprError;
          end;
        end
        else
          Result := sfprSkipped;

      fpModificationTime:
        if (aTemplateProperty as TFileModificationDateTimeProperty).Value <>
           (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value then
        begin
          if not mbFileSetTime(
            aFile.FullPath,
            DateTimeToFileTime((aTemplateProperty as TFileModificationDateTimeProperty).Value),
            0,
            0) then
          begin
            Result := sfprError;
          end;
        end
        else
          Result := sfprSkipped;

      fpCreationTime:
        if (aTemplateProperty as TFileCreationDateTimeProperty).Value <>
           (aFile.Properties[fpCreationTime] as TFileCreationDateTimeProperty).Value then
        begin
          if not mbFileSetTime(
            aFile.FullPath,
            0,
            DateTimeToFileTime((aTemplateProperty as TFileCreationDateTimeProperty).Value),
            0) then
          begin
            Result := sfprError;
          end;
        end
        else
          Result := sfprSkipped;

      fpLastAccessTime:
        if (aTemplateProperty as TFileLastAccessDateTimeProperty).Value <>
           (aFile.Properties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value then
        begin
          if not mbFileSetTime(
            aFile.FullPath,
            0,
            0,
            DateTimeToFileTime((aTemplateProperty as TFileLastAccessDateTimeProperty).Value)) then
          begin
            Result := sfprError;
          end;
        end
        else
          Result := sfprSkipped;

      else
        raise Exception.Create('Trying to set unsupported property');
    end;

  except
    on e: EDateOutOfRange do
    begin
      if not gSkipFileOpError then
        case AskQuestion(rsMsgLogError + Format(rsMsgErrDateNotSupported, [DateTimeToStr(e.DateTime)]), '',
                         [fsourSkip, fsourAbort],
                         fsourSkip, fsourAbort) of
          fsourSkip:
            Result := sfprSkipped;
          fsourAbort:
            RaiseAbortOperation;
        end;
    end;
    on e: EConvertError do
    begin
      if not gSkipFileOpError then
        case AskQuestion(rsMsgLogError + e.Message, '', [fsourSkip, fsourAbort],
                         fsourSkip, fsourAbort) of
          fsourSkip:
            Result := sfprSkipped;
          fsourAbort:
            RaiseAbortOperation;
        end;
    end;
  end;
end;

function TFileSystemSetFilePropertyOperation.RenameFile(const OldName: UTF8String; NewName: UTF8String): TSetFilePropertyResult;

  function AskIfOverwrite(Attrs: TFileAttrs): TFileSourceOperationUIResponse;
  var
    sQuestion: String;
  begin
    if DCOSUtils.FPS_ISDIR(Attrs) then
      sQuestion := rsMsgFolderExistsRwrt
    else
      sQuestion := rsMsgFileExistsRwrt;

    Result := AskQuestion(Format(sQuestion, [NewName]), '',
              [fsourYes, fsourNo, fsourAbort], fsourYes, fsourNo);
  end;

  {$IFDEF MSWINDOWS}
  function ShellRename: Boolean;
  var
    wsFromName, wsToName: WideString;
    FileOpStruct: TSHFileOpStructW;
  begin
    wsFromName := UTF8Decode(OldName) + #0;
    wsToName   := UTF8Decode(NewName) + #0;
    FillByte(FileOpStruct, SizeOf(FileOpStruct), 0);
    with FileOpStruct do
    begin
      Wnd   := GetForegroundWindow;
      // Use rename operation when only file name case is changed
      if UTF8LowerCase(OldName) = UTF8LowerCase(NewName) then
        wFunc := FO_RENAME
      else
        wFunc := FO_MOVE;
      pFrom := PWideChar(wsFromName);
      pTo   := PWideChar(wsToName);
    end;
    Result := (SHFileOperationW(@FileOpStruct) = 0) and (not FileOpStruct.fAnyOperationsAborted);
  end;
  {$ENDIF}

var
{$IFDEF UNIX}
  tmpFileName: UTF8String;
  OldFileStat, NewFileStat: stat;
{$ELSE}
  NewFileAttrs: TFileAttrs;
{$ENDIF}
begin
  if FileSource.GetPathType(NewName) <> ptAbsolute then
    NewName := ExtractFilePath(OldName) + NewName;

  if OldName = NewName then
    Exit(sfprSkipped);

{$IFDEF UNIX}
  if fpLstat(UTF8ToSys(OldName), OldFileStat) <> 0 then
    Exit(sfprError);

  // Check if target file exists.
  if fpLstat(UTF8ToSys(NewName), NewFileStat) = 0 then
  begin
    // Check if source and target are the same files (same inode and same device).
    if (OldFileStat.st_ino = NewFileStat.st_ino) and
       (OldFileStat.st_dev = NewFileStat.st_dev) then
    begin
      // Check number of links.
      // If it is 1 then source and target names most probably differ only
      // by case on a case-insensitive filesystem. Direct rename() in such case
      // fails on Linux, so we use a temporary file name and rename in two stages.
      // If number of links is more than 1 then it's enough to simply unlink
      // the source file, since both files are technically identical.
      // (On Linux rename() returns success but doesn't do anything
      // if renaming a file to its hard link.)
      // We cannot use st_nlink for directories because it means "number of
      // subdirectories"; hard links to directories are not supported on Linux
      // or Windows anyway (on MacOSX they are). Therefore we always treat
      // directories as if they were a single link and rename them using temporary name.

      if (NewFileStat.st_nlink = 1) or BaseUnix.fpS_ISDIR(NewFileStat.st_mode) then
      begin
        tmpFileName := GetTempName(OldName);

        if FpRename(UTF8ToSys(OldName), UTF8ToSys(tmpFileName)) = 0 then
        begin
          if fpLstat(UTF8ToSys(NewName), NewFileStat) = 0 then
          begin
            // We have renamed the old file but the new file name still exists,
            // so this wasn't a single file on a case-insensitive filesystem
            // accessible by two names that differ by case.

            FpRename(UTF8ToSys(tmpFileName), UTF8ToSys(OldName));  // Restore old file.
{$IFDEF DARWIN}
            // If it was a directory with multiple hard links then fall through
            // to asking for overwrite and unlinking source link.
            if not (BaseUnix.fpS_ISDIR(NewFileStat.st_mode) and (NewFileStat.st_nlink > 1)) then
{$ENDIF}
            Exit(sfprError);
          end
          else if FpRename(UTF8ToSys(tmpFileName), UTF8ToSys(NewName)) = 0 then
          begin
            Exit(sfprSuccess);
          end
          else
          begin
            FpRename(UTF8ToSys(tmpFileName), UTF8ToSys(OldName));  // Restore old file.
            Exit(sfprError);
          end;
        end
        else
          Exit(sfprError);
      end;

      // Both names are hard links to the same file.

      case AskIfOverwrite(NewFileStat.st_mode) of
        fsourYes: ; // continue
        fsourNo:
          Exit(sfprSkipped);
        fsourAbort:
          RaiseAbortOperation;
      end;

      // Multiple links - simply unlink the source file.
      if fpUnLink(UTF8ToSys(OldName)) = 0 then
        Result := sfprSuccess
      else
        Result := sfprError;

      Exit;
    end
    else
    begin
      case AskIfOverwrite(NewFileStat.st_mode) of
        fsourYes: ; // continue
        fsourNo:
          Exit(sfprSkipped);
        fsourAbort:
          RaiseAbortOperation;
      end;
    end;
  end;

  if FpRename(UTF8ToSys(OldName), UTF8ToSys(NewName)) = 0 then
    Result := sfprSuccess
  else
    Result := sfprError;

{$ELSE}

  if gUseShellForFileOperations then
  begin
    if ShellRename then
      Result := sfprSuccess
    else
      Result := sfprError;
  end
  else
  begin
    // Windows XP doesn't allow two filenames that differ only by case (even on NTFS).
    if UTF8LowerCase(OldName) <> UTF8LowerCase(NewName) then
    begin
      NewFileAttrs := mbFileGetAttr(NewName);
      if NewFileAttrs <> faInvalidAttributes then  // If target file exists.
      begin
        case AskIfOverwrite(NewFileAttrs) of
          fsourYes: ; // continue
          fsourNo:
            Exit(sfprSkipped);
          fsourAbort:
            RaiseAbortOperation;
        end;
      end;
    end;

    if mbRenameFile(OldName, NewName) then
      Result := sfprSuccess
    else
      Result := sfprError;
  end;
{$ENDIF}
end;

end.

