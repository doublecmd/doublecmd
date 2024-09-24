unit uFileSystemSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uFileProperty,
  uDescr;

type

  { TFileSystemSetFilePropertyOperation }

  TFileSystemSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  private
    FFullFilesTree: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetFilePropertyOperationStatistics; // local copy of statistics
    FDescription: TDescription;
    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FFileExistsOption: TFileSourceOperationUIResponse;
    FDirExistsOption: TFileSourceOperationUIResponse;

    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;

    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function RenameFile(aFile: TFile; NewName: String): TSetFilePropertyResult;

  protected
    procedure ShowCompareFilesUI(SourceFile: TFile; const TargetFilePath: String);
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
  uGlobs, uLng, DCDateTimeUtils, uFileSystemUtil, uShowForm,
  DCOSUtils, DCStrUtils, DCBasicTypes, uAdministrator
  {$IF DEFINED(UNIX)}
    , BaseUnix, DCUnix
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
  {$IF DEFINED(UNIX)}
  // Set owner/group before MODE because it clears SUID bit.
                           fpOwner,
  {$ENDIF}
                           fpAttributes,
                           fpModificationTime,
                           fpCreationTime,
                           fpLastAccessTime];

  if gProcessComments then begin
    FDescription := TDescription.Create(False);
  end;
end;

destructor TFileSystemSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTree) then
      FreeAndNil(FFullFilesTree);
  end;

  if Assigned(FDescription) then
  begin
    FDescription.SaveDescription;
    FreeAndNil(FDescription);
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

    SetProperties(CurrentFileIndex, aFile, aTemplateFile);

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
            aFile,
            (aTemplateProperty as TFileNameProperty).Value);

          if (Result = sfprSuccess) and gProcessComments then
          begin
            FDescription.Rename(aFile.FullPath, (aTemplateProperty as TFileNameProperty).Value);
          end;
        end
        else
          Result := sfprSkipped;

      fpAttributes:
        if (aTemplateProperty as TFileAttributesProperty).Value <>
           (aFile.Properties[fpAttributes] as TFileAttributesProperty).Value then
        begin
          if not FileSetAttrUAC(
            aFile.FullPath,
            (aTemplateProperty as TFileAttributesProperty).Value) then
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
          if not FileSetTimeUAC(
            aFile.FullPath,
            DateTimeToFileTimeEx((aTemplateProperty as TFileModificationDateTimeProperty).Value),
            TFileTimeExNull,
            TFileTimeExNull) then
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
          if not FileSetTimeUAC(
            aFile.FullPath,
            TFileTimeExNull,
            DateTimeToFileTimeEx((aTemplateProperty as TFileCreationDateTimeProperty).Value),
            TFileTimeExNull) then
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
          if not FileSetTimeUAC(
            aFile.FullPath,
            TFileTimeExNull,
            TFileTimeExNull,
            DateTimeToFileTimeEx((aTemplateProperty as TFileLastAccessDateTimeProperty).Value)) then
          begin
            Result := sfprError;
          end;
        end
        else
          Result := sfprSkipped;

      {$IF DEFINED(UNIX)}
      fpOwner:
        begin
          if fplchown(aFile.FullPath, (aTemplateProperty as TFileOwnerProperty).Owner,
                      (aTemplateProperty as TFileOwnerProperty).Group) <> 0 then
          begin
            Result := sfprError;;
          end;
        end
      {$ENDIF}

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

procedure TFileSystemSetFilePropertyOperation.QuestionActionHandler(
  Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
    ShowCompareFilesUI(FCurrentFile, FCurrentTargetFilePath);
end;

function TFileSystemSetFilePropertyOperation.RenameFile(aFile: TFile; NewName: String): TSetFilePropertyResult;
var
  OldName: String;
  NewAttr: TFileAttributeData;

  function OverwriteOlder: TFileSourceOperationUIResponse;
  begin
    if aFile.ModificationTime > FileTimeToDateTime(NewAttr.LastWriteTime) then
      Result := fsourOverwrite
    else
      Result := fsourSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationUIResponse;
  begin
    if aFile.Size > NewAttr.Size then
      Result := fsourOverwrite
    else
      Result := fsourSkip;
  end;

  function OverwriteLarger: TFileSourceOperationUIResponse;
  begin
    if aFile.Size < NewAttr.Size then
      Result := fsourOverwrite
    else
      Result := fsourSkip;
  end;

  function AskIfOverwrite: TFileSourceOperationUIResponse;
  var
    sQuestion: String;
  begin
    if DCOSUtils.FPS_ISDIR(NewAttr.Attr) then
    begin
      if FDirExistsOption <> fsourInvalid then Exit(FDirExistsOption);
      Result := AskQuestion(Format(rsMsgErrDirExists, [NewName]), '',
                 [fsourSkip, fsourSkipAll, fsourAbort], fsourSkip, fsourAbort);
      if Result = fsourSkipAll then
      begin
        FDirExistsOption:= fsourSkip;
        Result:= FDirExistsOption;
      end;
    end
    else begin
      case FFileExistsOption of
        fsourNone,
        fsourInvalid:
          begin
            FCurrentFile := aFile;
            FCurrentTargetFilePath := NewName;
            sQuestion:= FileExistsMessage(NewName, aFile.FullPath, aFile.Size, aFile.ModificationTime);
            Result := AskQuestion(sQuestion, '',
                  [fsourOverwrite, fsourSkip, fsourOverwriteSmaller,
                   fsourOverwriteAll, fsourSkipAll, fsourOverwriteLarger,
                   fsourOverwriteOlder, fsourAbort, fsouaCompare
                  ], fsourOverwrite, fsourAbort, @QuestionActionHandler);
            case Result of
            fsourOverwriteAll:
              begin
                Result:= fsourOverwrite;
                FFileExistsOption:= Result;
              end;
            fsourSkipAll:
              begin
                Result:= fsourSkip;
                FFileExistsOption:= Result;
              end;
            fsourOverwriteOlder:
               begin
                 FFileExistsOption := OverwriteOlder;
                 Result:= OverwriteOlder;
               end;
            fsourOverwriteSmaller:
              begin
                FFileExistsOption := fsourOverwriteSmaller;
                Result:= OverwriteSmaller;
              end;
            fsourOverwriteLarger:
              begin
                FFileExistsOption := fsourOverwriteLarger;
                Result:= OverwriteLarger;
              end;
            end; // case
          end;
        fsourOverwriteOlder:
           begin
             Result:= OverwriteOlder;
           end;
        fsourOverwriteSmaller:
          begin
            Result:= OverwriteSmaller;
          end;
        fsourOverwriteLarger:
          begin
            Result:= OverwriteLarger;
          end;
        else
          Result := FFileExistsOption;
      end; // case
    end;
  end;

{$IFDEF UNIX}
var
  OldAttr: TFileAttributeData;
{$ENDIF}
begin
  OldName:= aFile.FullPath;

  if FileSource.GetPathType(NewName) <> ptAbsolute then
  begin
    NewName := ExtractFilePath(OldName) + TrimPath(NewName);
  end;

  if OldName = NewName then
    Exit(sfprSkipped);

{$IFDEF UNIX}
  // Check if target file exists.
  if FileGetAttrUAC(NewName, NewAttr) then
  begin
    // Cannot overwrite file by directory and vice versa
    if fpS_ISDIR(NewAttr.FindData.st_mode) <> aFile.IsDirectory then
      Exit(sfprError);
    // Special case when filenames differ only by case,
    // see comments in mbRenameFile function for details
    if (UTF8LowerCase(OldName) <> UTF8LowerCase(NewName)) then
      OldAttr.FindData.st_ino:= not NewAttr.FindData.st_ino
    else begin
      if not FileGetAttrUAC(OldName, OldAttr) then
        Exit(sfprError);
    end;
    // Check if source and target are the same files (same inode and same device).
    if (OldAttr.FindData.st_ino = NewAttr.FindData.st_ino) and
       (OldAttr.FindData.st_dev = NewAttr.FindData.st_dev) and
       // Check number of links, if it is 1 then source and target names most
       // probably differ only by case on a case-insensitive filesystem.
       ((NewAttr.FindData.st_nlink = 1) or fpS_ISDIR(NewAttr.FindData.st_mode)) then
    begin
      // File names differ only by case on a case-insensitive filesystem.
    end
    else begin
      case AskIfOverwrite of
        fsourOverwrite: ; // continue
        fsourSkip:
          Exit(sfprSkipped);
        fsourAbort:
          RaiseAbortOperation;
      end;
    end;
  end;
{$ELSE}
  // Windows doesn't allow two filenames that differ only by case (even on NTFS).
  if UTF8LowerCase(OldName) <> UTF8LowerCase(NewName) then
  begin
    if FileGetAttrUAC(NewName, NewAttr) then  // If target file exists.
    begin
      // Cannot overwrite file by directory and vice versa
      if fpS_ISDIR(NewAttr.Attr) <> aFile.IsDirectory then
        Exit(sfprError);
      case AskIfOverwrite of
        fsourOverwrite: ; // continue
        fsourSkip:
          Exit(sfprSkipped);
        fsourAbort:
          RaiseAbortOperation;
      end;
    end;
  end;
{$ENDIF}

  if RenameFileUAC(OldName, NewName) then
    Result := sfprSuccess
  else
    Result := sfprError;
end;

procedure TFileSystemSetFilePropertyOperation.ShowCompareFilesUI(
  SourceFile: TFile; const TargetFilePath: String);
var
  TargetFile: TFile;
begin
  TargetFile := FileSource.CreateFileObject(ExtractFilePath(TargetFilePath));
  try
    TargetFile.Name := ExtractFileName(TargetFilePath);
    PrepareToolData(FileSource, SourceFile, FileSource, TargetFile, @ShowDifferByGlobList, True);
  finally
    TargetFile.Free;
  end;
end;

end.

