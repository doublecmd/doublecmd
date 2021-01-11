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

    function RenameFile(aFile: TFile; NewName: String): TSetFilePropertyResult;

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
          if not FileSetTimeUAC(
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
          if not FileSetTimeUAC(
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

function TFileSystemSetFilePropertyOperation.RenameFile(aFile: TFile; NewName: String): TSetFilePropertyResult;
var
  OldName: String;

  function AskIfOverwrite(Attrs: TFileAttrs): TFileSourceOperationUIResponse;
  var
    sQuestion: String;
  begin
    if DCOSUtils.FPS_ISDIR(Attrs) then
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
      if FFileExistsOption <> fsourInvalid then Exit(FFileExistsOption);
      sQuestion:= FileExistsMessage(NewName, aFile.FullPath, aFile.Size, aFile.ModificationTime);
      Result := AskQuestion(sQuestion, '',
                  [fsourOverwrite, fsourSkip, fsourAbort, fsourOverwriteAll,
                   fsourSkipAll], fsourOverwrite, fsourAbort);
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
      end;
    end;
  end;

var
{$IFDEF UNIX}
  OldAttr, NewAttr: TFileAttributeData;
{$ELSE}
  NewFileAttrs: TFileAttrs;
{$ENDIF}
begin
  OldName:= aFile.FullPath;

  if FileSource.GetPathType(NewName) <> ptAbsolute then
    NewName := ExtractFilePath(OldName) + NewName;

  if OldName = NewName then
    Exit(sfprSkipped);

{$IFDEF UNIX}
  // Check if target file exists.
  if FileGetAttrUAC(NewName, NewAttr) then
  begin
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
       ((NewAttr.FindData.st_nlink = 1) {$IFNDEF DARWIN}or fpS_ISDIR(NewAttr.FindData.st_mode){$ENDIF}) then
    begin
      // File names differ only by case on a case-insensitive filesystem.
    end
    else begin
      case AskIfOverwrite(NewAttr.FindData.st_mode) of
        fsourOverwrite: ; // continue
        fsourSkip:
          Exit(sfprSkipped);
        fsourAbort:
          RaiseAbortOperation;
      end;
    end;
  end;
{$ELSE}
  // Windows XP doesn't allow two filenames that differ only by case (even on NTFS).
  if UTF8LowerCase(OldName) <> UTF8LowerCase(NewName) then
  begin
    NewFileAttrs := FileGetAttrUAC(NewName);
    if NewFileAttrs <> faInvalidAttributes then  // If target file exists.
    begin
      case AskIfOverwrite(NewFileAttrs) of
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

end.

