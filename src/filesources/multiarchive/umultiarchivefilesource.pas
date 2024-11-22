unit uMultiArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, DCStringHashListUtf8, uOSUtils,
  uMultiArc, uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uArchiveFileSource, uFileProperty, uFileSource, uFileSourceOperation,
  uMultiArchiveUtil, DCBasicTypes, uClassesEx;

type

  { IMultiArchiveFileSource }

  IMultiArchiveFileSource = interface(IArchiveFileSource)
    ['{71BF41D3-1E40-4E84-83BB-B6D3E0DEB6FC}']

    function GetPassword: String;
    function GetArcFileList: TThreadObjectList;
    function GetMultiArcItem: TMultiArcItem;

    function FileIsLink(ArchiveItem: TArchiveItem): Boolean;
    function FileIsDirectory(ArchiveItem: TArchiveItem): Boolean;

    procedure FillAndCount(const FileMask: String; Files: TFiles;
                           CountDirs: Boolean;
                           out NewFiles: TFiles;
                           out FilesCount: Int64; out FilesSize: Int64);

    property Password: String read GetPassword;
    property ArchiveFileList: TThreadObjectList read GetArcFileList;
    property MultiArcItem: TMultiArcItem read GetMultiArcItem;
  end;

  { TMultiArchiveFileSource }

  TMultiArchiveFileSource = class(TArchiveFileSource, IMultiArchiveFileSource)
  private
    FPassword: String;
    FOutputParser: TOutputParser;
    FArcFileList : TThreadObjectList;
    FMultiArcItem: TMultiArcItem;
    FAllDirsList,
    FExistsDirList: TStringHashListUtf8;
    FLinkAttribute,
    FDirectoryAttribute: TFileAttrs;

    function GetPassword: String;
    function GetMultiArcItem: TMultiArcItem;
    procedure OnGetArchiveItem(ArchiveItem: TArchiveItem);

    function ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

    function FileIsLink(ArchiveItem: TArchiveItem): Boolean;
    function FileIsDirectory(ArchiveItem: TArchiveItem): Boolean;

    function GetArcFileList: TThreadObjectList;

  protected
    function GetPacker: String; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    procedure DoReload(const PathsToReload: TPathsArray); override;

  public
    procedure FillAndCount(const FileMask: String; Files: TFiles;
                           CountDirs: Boolean;
                           out NewFiles: TFiles;
                           out FilesCount: Int64; out FilesSize: Int64);
  public
    constructor Create(anArchiveFileSource: IFileSource;
                       anArchiveFileName: String;
                       aMultiArcItem: TMultiArcItem); reintroduce;
    destructor Destroy; override;

    class function CreateFile(const APath: String; ArchiveItem: TArchiveItem; FormMode: Integer): TFile; overload;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;

    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;

    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;

    function CreateExecuteOperation(var ExecutableFile: TFile;
                                    BasePath, Verb: String): TFileSourceOperation; override;

    function CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation; override;

    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;

    class function CreateByArchiveSign(anArchiveFileSource: IFileSource;
                                       anArchiveFileName: String): IMultiArchiveFileSource;
    class function CreateByArchiveType(anArchiveFileSource: IFileSource;
                                       anArchiveFileName, anArchiveType: String;
                                       bIncludeHidden: Boolean = False): IMultiArchiveFileSource;
    class function CreateByArchiveName(anArchiveFileSource: IFileSource;
                                       anArchiveFileName: String;
                                       bIncludeHidden: Boolean = False): IMultiArchiveFileSource;
    {en
       Returns @true if there is an addon registered for the archive name.
    }
    class function CheckAddonByName(const anArchiveFileName: String): Boolean;

    property Password: String read GetPassword;
    property ArchiveFileList: TThreadObjectList read GetArcFileList;
    property MultiArcItem: TMultiArcItem read GetMultiArcItem;
  end;

implementation

uses
  uDebug, uGlobs, DCFileAttributes, DCOSUtils, DCStrUtils, DCDateTimeUtils,
  FileUtil, uMasks, uLng,
  uMultiArchiveListOperation,
  uMultiArchiveCopyInOperation,
  uMultiArchiveCopyOutOperation,
  uMultiArchiveDeleteOperation,
  uMultiArchiveExecuteOperation,
  uMultiArchiveTestArchiveOperation,
  uMultiArchiveCalcStatisticsOperation
  ;

class function TMultiArchiveFileSource.CreateByArchiveSign(anArchiveFileSource: IFileSource;
                                                           anArchiveFileName: String): IMultiArchiveFileSource;
var
  I: Integer;
  aMultiArcItem: TMultiArcItem;
  bFound: Boolean = False;
begin
  Result := nil;

  // Check if there is a registered addon for the archive file by content.
  for I := 0 to gMultiArcList.Count - 1 do
  begin
    aMultiArcItem:= gMultiArcList.Items[I];

    if (aMultiArcItem.FEnabled) then
    begin
      if (aMultiArcItem.FID <> EmptyStr) and aMultiArcItem.CanYouHandleThisFile(anArchiveFileName) then
        bFound:= True
      else
        bFound:= aMultiArcItem.Matches(anArchiveFileName);
      if bFound then
      begin
        Result := TMultiArchiveFileSource.Create(anArchiveFileSource,
                                                 anArchiveFileName,
                                                 aMultiArcItem);

        DCDebug('Found registered addon "' + aMultiArcItem.FDescription + '" for archive ' + anArchiveFileName);
        Break;
      end;
    end;
  end;
end;

class function TMultiArchiveFileSource.CreateByArchiveType(
  anArchiveFileSource: IFileSource; anArchiveFileName, anArchiveType: String;
  bIncludeHidden: Boolean): IMultiArchiveFileSource;
var
  I: Integer;
  aMultiArcItem: TMultiArcItem;
begin
  Result := nil;

  // Check if there is a registered addon for the extension of the archive file name.
  for I := 0 to gMultiArcList.Count - 1 do
  begin
    aMultiArcItem:= gMultiArcList.Items[I];

    if (aMultiArcItem.FEnabled) and
       ((bIncludeHidden) or not (mafHide in aMultiArcItem.FFlags)) and MatchesMaskList(anArchiveType, aMultiArcItem.FExtension, ',') then
    begin
      Result := TMultiArchiveFileSource.Create(anArchiveFileSource,
                                               anArchiveFileName,
                                               aMultiArcItem);

      DCDebug('Found registered addon "' + aMultiArcItem.FDescription + '" for archive ' + anArchiveFileName);
      Break;
    end;
  end;
end;

class function TMultiArchiveFileSource.CreateByArchiveName(
  anArchiveFileSource: IFileSource; anArchiveFileName: String;
  bIncludeHidden: Boolean): IMultiArchiveFileSource;
var
  I: Integer;
  aMultiArcItem: TMultiArcItem;
begin
  Result := nil;

  // Check if there is a registered addon for the archive file name.
  for I := 0 to gMultiArcList.Count - 1 do
  begin
    aMultiArcItem:= gMultiArcList.Items[I];

    if (aMultiArcItem.FEnabled) and
       ((bIncludeHidden) or not (mafHide in aMultiArcItem.FFlags)) and aMultiArcItem.Matches(anArchiveFileName) then
    begin
      Result := TMultiArchiveFileSource.Create(anArchiveFileSource,
                                               anArchiveFileName,
                                               aMultiArcItem);

      DCDebug('Found registered addon "' + aMultiArcItem.FDescription + '" for archive ' + anArchiveFileName);
      Break;
    end;
  end;
end;

class function TMultiArchiveFileSource.CheckAddonByName(const anArchiveFileName: String): Boolean;
var
  I: Integer;
  aMultiArcItem: TMultiArcItem;
begin
  for I := 0 to gMultiArcList.Count - 1 do
  begin
    aMultiArcItem:= gMultiArcList.Items[I];
    if (aMultiArcItem.FEnabled) and aMultiArcItem.Matches(anArchiveFileName) then
      Exit(True);
  end;
  Result := False;
end;

// ----------------------------------------------------------------------------

constructor TMultiArchiveFileSource.Create(anArchiveFileSource: IFileSource;
                                           anArchiveFileName: String;
                                           aMultiArcItem: TMultiArcItem);
begin
  inherited Create(anArchiveFileSource, anArchiveFileName);

  FMultiArcItem := aMultiArcItem;
  FArcFileList := TThreadObjectList.Create;
  FOutputParser := TOutputParser.Create(aMultiArcItem, anArchiveFileName);
  FOutputParser.OnGetArchiveItem:= @OnGetArchiveItem;

  FOperationsClasses[fsoCopyIn]          := TMultiArchiveCopyInOperation.GetOperationClass;
  FOperationsClasses[fsoCopyOut]         := TMultiArchiveCopyOutOperation.GetOperationClass;

  with FMultiArcItem do
  begin
    if (FFormMode and MAF_UNIX_ATTR) <> 0 then
      begin
        FLinkAttribute:= S_IFLNK;
        FDirectoryAttribute:= S_IFDIR;
      end
    else if (FFormMode and MAF_WIN_ATTR) <> 0 then
      begin
        FLinkAttribute:= FILE_ATTRIBUTE_REPARSE_POINT;
        FDirectoryAttribute:= FILE_ATTRIBUTE_DIRECTORY;
      end
    else
      begin
        FLinkAttribute:= faSymLink;
        FDirectoryAttribute:= faFolder;
      end;
  end;

  ReadArchive;
end;

destructor TMultiArchiveFileSource.Destroy;
begin
  inherited Destroy;

  if Assigned(FArcFileList) then
    FreeAndNil(FArcFileList);
end;

class function TMultiArchiveFileSource.CreateFile(const APath: String; ArchiveItem: TArchiveItem; FormMode: Integer): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    SizeProperty := TFileSizeProperty.Create(ArchiveItem.UnpSize);
    SizeProperty.IsValid := (ArchiveItem.UnpSize >= 0);

    CompressedSizeProperty := TFileCompressedSizeProperty.Create(ArchiveItem.PackSize);
    CompressedSizeProperty.IsValid := (ArchiveItem.PackSize >= 0);

    if (FormMode and MAF_UNIX_ATTR) <> 0 then
      AttributesProperty := TUnixFileAttributesProperty.Create(ArchiveItem.Attributes)
    else if (FormMode and MAF_WIN_ATTR) <> 0 then
      AttributesProperty := TNtfsFileAttributesProperty.Create(ArchiveItem.Attributes)
    else begin
      AttributesProperty := TFileAttributesProperty.CreateOSAttributes(ArchiveItem.Attributes);
    end;

    if AttributesProperty.IsDirectory then
    begin
      if not SizeProperty.IsValid then
      begin
        SizeProperty.IsValid := True;
        SizeProperty.Value := FOLDER_SIZE_UNKN;
      end;
      if not CompressedSizeProperty.IsValid then
      begin
        CompressedSizeProperty.IsValid := True;
        CompressedSizeProperty.Value := FOLDER_SIZE_UNKN;
      end;
    end;

    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(0);
    with ArchiveItem do
    begin
      if (Month = 0) or (Day = 0) then
        ModificationTimeProperty.IsValid:= False
      else
      begin
        try
          ModificationTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
        except
          on EConvertError do ModificationTimeProperty.IsValid:= False;
        end;
      end;
    end;

    if AttributesProperty.IsLink and (Length(ArchiveItem.FileLink) > 0) then
    begin
      LinkProperty := TFileLinkProperty.Create;
      LinkProperty.LinkTo := ArchiveItem.FileLink;
    end;

    CommentProperty := TFileCommentProperty.Create;
    CommentProperty.Value := ArchiveItem.Comment;

    // Set name after assigning Attributes property, because it is used to get extension.
    Name := ExtractFileNameEx(ArchiveItem.FileName);
    if ArchiveItem.FileExt <> EmptyStr then
      Name:= Name + '.' + ArchiveItem.FileExt;
  end;
end;

function TMultiArchiveFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoExecute];
  if (FMultiArcItem.FList <> EmptyStr) or (mafFileNameList in FMultiArcItem.FFlags) then
    Result := Result + [fsoList, fsoCalcStatistics];
  if FMultiArcItem.FAdd <> EmptyStr then
    Result := Result + [fsoCopyIn];
  if FMultiArcItem.FExtract <> EmptyStr then
    Result := Result + [fsoCopyOut];
  if FMultiArcItem.FDelete <> EmptyStr then
    Result := Result + [fsoDelete];
  if FMultiArcItem.FTest <> EmptyStr then
    Result := Result + [fsoTestArchive];
end;

function TMultiArchiveFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [];
end;

function TMultiArchiveFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties + [fpLink, fpComment];
end;

function TMultiArchiveFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
var
  I: Integer;
  AFileList: TList;
  ArchiveItem: TArchiveItem;
begin
  Result := False;
  if Length(NewDir) > 0 then
  begin
    if NewDir = GetRootDir() then
      Exit(True);

    NewDir := IncludeTrailingPathDelimiter(NewDir);

    AFileList:= FArcFileList.LockList;
    try
      // Search file list for a directory with name NewDir.
      for I := 0 to AFileList.Count - 1 do
      begin
        ArchiveItem := TArchiveItem(AFileList.Items[I]);
        if FileIsDirectory(ArchiveItem) and (Length(ArchiveItem.FileName) > 0) then
        begin
          if NewDir = IncludeTrailingPathDelimiter(GetRootDir() + ArchiveItem.FileName) then
            Exit(True);
        end;
      end;
    finally
      FArcFileList.UnlockList;
    end;
  end;
end;

function TMultiArchiveFileSource.GetArcFileList: TThreadObjectList;
begin
  Result := FArcFileList;
end;

function TMultiArchiveFileSource.GetMultiArcItem: TMultiArcItem;
begin
  Result := FMultiArcItem;
end;

function TMultiArchiveFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TMultiArchiveListOperation.Create(TargetFileSource, TargetPath);
end;

function TMultiArchiveFileSource.CreateCopyInOperation(
            SourceFileSource: IFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TMultiArchiveCopyInOperation.Create(SourceFileSource,
                                                TargetFileSource,
                                                SourceFiles, TargetPath);
end;

function TMultiArchiveFileSource.CreateCopyOutOperation(
            TargetFileSource: IFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TMultiArchiveCopyOutOperation.Create(SourceFileSource,
                                                 TargetFileSource,
                                                 SourceFiles, TargetPath);
end;

function TMultiArchiveFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TMultiArchiveDeleteOperation.Create(TargetFileSource,
                                                FilesToDelete);
end;

function TMultiArchiveFileSource.CreateExecuteOperation(var ExecutableFile: TFile;
                                                        BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TMultiArchiveExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TMultiArchiveFileSource.CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result:=  TMultiArchiveTestArchiveOperation.Create(SourceFileSource, theSourceFiles);
end;

function TMultiArchiveFileSource.CreateCalcStatisticsOperation(
  var theFiles: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TMultiArchiveCalcStatisticsOperation.Create(TargetFileSource, theFiles);
end;

procedure TMultiArchiveFileSource.OnGetArchiveItem(ArchiveItem: TArchiveItem);

  procedure CollectDirs(Path: PAnsiChar; var DirsList: TStringHashListUtf8);
  var
    I : Integer;
    Dir : AnsiString;
  begin
    // Scan from the second char from the end, to the second char from the beginning.
    for I := strlen(Path) - 2 downto 1 do
    begin
      if Path[I] = PathDelim then
      begin
        SetString(Dir, Path, I);
        if DirsList.Find(Dir) = -1 then
          // Add directory and continue scanning for parent directories.
          DirsList.Add(Dir)
        else
          // This directory is already in the list and we assume
          // that all parent directories are too.
          Exit;
      end
    end;
  end;

var
  NameLength: Integer;
begin
  // Some archivers end directories with path delimiter.
  // And not set directory attribute. So delete path
  // delimiter if present and add directory attribute.
  NameLength := Length(ArchiveItem.FileName);
  if (NameLength > 0) and (ArchiveItem.FileName[NameLength] = PathDelim) then
  begin
    Delete(ArchiveItem.FileName, NameLength, 1);
    ArchiveItem.Attributes := ArchiveItem.Attributes or FDirectoryAttribute;
  end;

  //****************************************************************************

  // Workaround for archivers that don't give a list of folders
  // or the list does not include all of the folders.
  if FileIsDirectory(ArchiveItem) then
  begin
    // Collect directories that the plugin supplies.
    if (FExistsDirList.Find(ArchiveItem.FileName) < 0) then
      FExistsDirList.Add(ArchiveItem.FileName);
  end;

  // Collect all directories.
  CollectDirs(PAnsiChar(ArchiveItem.FileName), FAllDirsList);

  //****************************************************************************

  FArcFileList.Add(ArchiveItem);
end;

function TMultiArchiveFileSource.GetPacker: String;
begin
  Result:= FMultiArcItem.FPacker;
end;

function TMultiArchiveFileSource.GetPassword: String;
begin
  Result:= FPassword;
end;

function TMultiArchiveFileSource.ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;
var
  I : Integer;
  AFileList: TList;
  ArchiveTime: TSystemTime;
  ArchiveItem: TArchiveItem;
begin
  if not mbFileAccess(ArchiveFileName, fmOpenRead) then
    begin
      Result := False;
      Exit;
    end;

  {
  if bCanYouHandleThisFile and (Assigned(WcxModule.CanYouHandleThisFile) or Assigned(WcxModule.CanYouHandleThisFileW)) then
    begin
      Result := WcxModule.WcxCanYouHandleThisFile(ArchiveFileName);
      if not Result then Exit;
    end;
  }

  { Get File List }
  AFileList:= FArcFileList.LockList;
  try
    AFileList.Clear;
    // Get archive file time
    DateTimeToSystemTime(FileTimeToDateTime(mbFileAge(ArchiveFileName)), ArchiveTime);

    if mafFileNameList in FMultiArcItem.FFlags then
    begin
      ArchiveItem:= TArchiveItem.Create;
      ArchiveItem.FileName := ExtractOnlyFileName(ArchiveFileName);
      ArchiveItem.Year:= ArchiveTime.Year;
      ArchiveItem.Month:= ArchiveTime.Month;
      ArchiveItem.Day:= ArchiveTime.Day;
      ArchiveItem.Hour:= ArchiveTime.Hour;
      ArchiveItem.Minute:= ArchiveTime.Minute;
      ArchiveItem.Second:= ArchiveTime.Second;
      ArchiveItem.Attributes := mbFileGetAttr(ArchiveFileName);
      ArchiveItem.UnpSize:= -1;
      ArchiveItem.PackSize:= mbFileSize(ArchiveFileName);
      AFileList.Add(ArchiveItem);
      Exit(True);
    end;

    FExistsDirList := TStringHashListUtf8.Create(True);
    FAllDirsList := TStringHashListUtf8.Create(True);

    try
      DCDebug('Get File List');

      FOutputParser.Password:= FPassword;
      FOutputParser.Prepare;
      FOutputParser.Execute;
      FPassword:= FOutputParser.Password;

      if FOutputParser.OpenError and (AFileList.Count < 1) then
        raise EFileSourceException.Create(rsMsgErrEOpen);

      (* if archiver does not give a list of folders *)
      for I := 0 to FAllDirsList.Count - 1 do
      begin
        // Add only those directories that were not supplied by the plugin.
        if FExistsDirList.Find(FAllDirsList.List[I]^.Key) < 0 then
        begin
          ArchiveItem:= TArchiveItem.Create;
          try
            ArchiveItem.FileName := FAllDirsList.List[I]^.Key;
            ArchiveItem.Year:= ArchiveTime.Year;
            ArchiveItem.Month:= ArchiveTime.Month;
            ArchiveItem.Day:= ArchiveTime.Day;
            ArchiveItem.Hour:= ArchiveTime.Hour;
            ArchiveItem.Minute:= ArchiveTime.Minute;
            ArchiveItem.Second:= ArchiveTime.Second;
            ArchiveItem.Attributes := FDirectoryAttribute;
            AFileList.Add(ArchiveItem);
          except
            FreeAndNil(ArchiveItem);
          end;
        end;
      end;

    finally
      FreeAndNil(FAllDirsList);
      FreeAndNil(FExistsDirList);
    end;

  finally
    FArcFileList.UnlockList;
  end;

  Result := True;
end;

function TMultiArchiveFileSource.FileIsLink(ArchiveItem: TArchiveItem): Boolean;
begin
  Result:= (ArchiveItem.Attributes and FLinkAttribute <> 0);
end;

function TMultiArchiveFileSource.FileIsDirectory(ArchiveItem: TArchiveItem): Boolean;
begin
  Result:= (ArchiveItem.Attributes and FDirectoryAttribute <> 0);
end;

procedure TMultiArchiveFileSource.DoReload(const PathsToReload: TPathsArray);
begin
  ReadArchive;
end;

procedure TMultiArchiveFileSource.FillAndCount(const FileMask: String; Files: TFiles;
  CountDirs: Boolean; out NewFiles: TFiles; out FilesCount: Int64;
  out FilesSize: Int64);
var
  aFile: TFile;
  I, J: Integer;
  AFileList: TList;
  sFileName: String;
  MaskList: TMaskList;
  ArchiveItem: TArchiveItem;
begin
  FilesSize:= 0;
  FilesCount:= 0;
  NewFiles:= TFiles.Create(Files.Path);
  if (FileMask = '*.*') or (FileMask = '*') then
    MaskList:= nil
  else begin
    MaskList:= TMaskList.Create(FileMask);
  end;
  AFileList:= ArchiveFileList.LockList;
  try
    for I := 0 to AFileList.Count - 1 do
    begin
      ArchiveItem := TArchiveItem(AFileList.Items[I]);
      sFileName:= PathDelim + ArchiveItem.FileName;

      // And name matches file mask
      if ((MaskList = nil) or MaskList.Matches(ExtractFileNameEx(ArchiveItem.FileName))) then
      begin
        for J := 0 to Files.Count - 1 do
        begin
          aFile := Files[J];

          if  (aFile.FullPath = sFileName) or // Item in the list is a file, only compare names.
              (aFile.AttributesProperty.IsDirectory and IsInPath(aFile.FullPath, sFileName, True, False)) then // Check if 'FileName' is in this directory or any of its subdirectories.
            begin
              if FileIsDirectory(ArchiveItem) then
                begin
                  if CountDirs then Inc(FilesCount);
                end
              else
                begin
                  Inc(FilesCount);
                  Inc(FilesSize, aFile.Size);
                end;
              aFile:= TMultiArchiveFileSource.CreateFile(ExtractFilePathEx(ArchiveItem.FileName), ArchiveItem, FMultiArcItem.FFormMode);
              aFile.FullPath:= ExcludeFrontPathDelimiter(aFile.FullPath);
              NewFiles.Add(aFile);
            end;
        end; // for J
      end;
    end; // for I
  finally
    ArchiveFileList.UnlockList;
  end;
  MaskList.Free;
end;

end.


