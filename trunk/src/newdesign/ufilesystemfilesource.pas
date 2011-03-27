unit uFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDCUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uLocalFileSource,
  uFileSource,
  uFileSourceProperty,
  uFileProperty,
  uFile,
  uDescr,
  uTypes
  ;

type

  {en
     Real file system.
  }

  IFileSystemFileSource = interface(ILocalFileSource)
    ['{59EDCF45-F151-4AE2-9DCE-3586E6191496}']
  end;

  { TFileSystemFileSource }

  TFileSystemFileSource = class(TLocalFileSource, IFileSystemFileSource)
  private
    FDescr: TDescription;

  protected
    function GetCurrentWorkingDirectory: String; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateFile(const APath: String): TFile; override;
    class function CreateFile(const APath: String; SearchRecord: TSearchRecEx): TFile; overload;
    {en
       Creates a file object using an existing file/directory as a template.
       All the properties will reflect the existing file.
       @param(FilePath denotes absolute path to a file to use as a template.)
    }
    class function CreateFileFromFile(const aFilePath: String): TFile;
    {en
       Creates file list from a list of template files.
       @param(FileNamesList
              A list of absolute paths to files.)
    }
    class function CreateFilesFromFileList(const APath: String; const FileNamesList: TStringList): TFiles;

    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes); override;

    class function GetFileSource: IFileSystemFileSource;

    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function GetProperties: TFileSourceProperties; override;

    function IsPathAtRoot(Path: String): Boolean; override;

    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;
    function GetPathType(sPath : String): TPathType; override;

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation; override;
    function CreateSplitOperation(var aSourceFile: TFile;
                                  aTargetPath: String): TFileSourceOperation;
    function CreateCombineOperation(var SourceFiles: TFiles;
                                    aTargetFile: String): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;

    procedure Reload(const PathsToReload: TPathsArray); override;
    // ------------------------------------------------------
  end;

  { TFileSystemFileSourceConnection }

  TFileSystemFileSourceConnection = class(TFileSourceConnection)
  protected
    procedure SetCurrentPath(NewPath: String); override;
  end;

implementation

uses
  uOSUtils, uFindEx, uDateTimeUtils, uGlobs,
{$IFDEF MSWINDOWS}
  uMyWindows, Windows,
{$ENDIF}
{$IFDEF UNIX}
  BaseUnix, uUsersGroups, FileUtil, uMyUnix,
{$ENDIF}
  uFileSystemListOperation,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation,
  uFileSystemDeleteOperation,
  uFileSystemWipeOperation,
  uFileSystemSplitOperation,
  uFileSystemCombineOperation,
  uFileSystemCreateDirectoryOperation,
  uFileSystemExecuteOperation,
  uFileSystemCalcChecksumOperation,
  uFileSystemCalcStatisticsOperation,
  uFileSystemSetFilePropertyOperation;

constructor TFileSystemFileSource.Create;
begin
  inherited Create;
  FDescr:= nil;

  FOperationsClasses[fsoList]            := TFileSystemListOperation.GetOperationClass;
  FOperationsClasses[fsoCopy]            := TFileSystemCopyOperation.GetOperationClass;
  FOperationsClasses[fsoCopyIn]          := TFileSystemCopyInOperation.GetOperationClass;
  FOperationsClasses[fsoCopyOut]         := TFileSystemCopyOutOperation.GetOperationClass;
  FOperationsClasses[fsoMove]            := TFileSystemMoveOperation.GetOperationClass;
  FOperationsClasses[fsoDelete]          := TFileSystemDeleteOperation.GetOperationClass;
  FOperationsClasses[fsoWipe]            := TFileSystemWipeOperation.GetOperationClass;
  FOperationsClasses[fsoCombine]         := TFileSystemCombineOperation.GetOperationClass;
  FOperationsClasses[fsoCreateDirectory] := TFileSystemCreateDirectoryOperation.GetOperationClass;
  FOperationsClasses[fsoCalcChecksum]    := TFileSystemCalcChecksumOperation.GetOperationClass;
  FOperationsClasses[fsoCalcStatistics]  := TFileSystemCalcStatisticsOperation.GetOperationClass;
  FOperationsClasses[fsoSetFileProperty] := TFileSystemSetFilePropertyOperation.GetOperationClass;
  FOperationsClasses[fsoExecute]         := TFileSystemExecuteOperation.GetOperationClass;
end;

destructor TFileSystemFileSource.Destroy;
begin
  if Assigned(FDescr) then
    FreeAndNil(FDescr);
  inherited Destroy;
end;

class function TFileSystemFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    CreationTimeProperty := TFileCreationDateTimeProperty.Create;
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create;
    LinkProperty := TFileLinkProperty.Create;
  end;
end;

class function TFileSystemFileSource.CreateFile(const APath: String; SearchRecord: TSearchRecEx): TFile;
{$IF DEFINED(UNIX)}
var
  StatInfo: BaseUnix.Stat; //buffer for stat info
  sFullPath: String;
{$ENDIF}
{$IF DEFINED(MSWINDOWS)}
var
  sUser, sGroup: String;
{$ENDIF}
begin
  Result := TFile.Create(APath);

  with Result do
  begin
{$IF DEFINED(MSWINDOWS)}

    AttributesProperty := TNtfsFileAttributesProperty.Create(SearchRecord.Attr);
    SizeProperty := TFileSizeProperty.Create(SearchRecord.Size);
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(
                                  WinFileTimeToDateTime(SearchRecord.FindData.ftLastWriteTime));
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(
                              WinFileTimeToDateTime(SearchRecord.FindData.ftCreationTime));
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
                                WinFileTimeToDateTime(SearchRecord.FindData.ftLastAccessTime));

    LinkProperty := TFileLinkProperty.Create;

    if AttributesProperty.IsLink then
    begin
      LinkProperty.IsLinkToDirectory := AttributesProperty.IsDirectory;
      LinkProperty.LinkTo := ReadSymLink(Path + SearchRecord.Name);
      LinkProperty.IsValid := mbFileSystemEntryExists(LinkProperty.LinkTo);
    end;

    {OwnerProperty := TFileOwnerProperty.Create;
    OwnerProperty.Owner := 0;
    OwnerProperty.Group := 0;
    if GetFileOwner(Path + SearchRecord.Name, sUser, sGroup) then
    begin
      OwnerProperty.OwnerStr := sUser;
      OwnerProperty.GroupStr := sGroup;
    end;

    TypeProperty := TFileTypeProperty.Create;
    TypeProperty.Value := GetFileDescription(Path + SearchRecord.Name);}

{$ELSEIF DEFINED(UNIX)}

    StatInfo := PUnixFindData(SearchRecord.FindHandle)^.StatRec;

    AttributesProperty := TUnixFileAttributesProperty.Create(StatInfo.st_mode);
    if AttributesProperty.IsDirectory then
      // On Unix a size for directory entry on filesystem is returned in StatInfo.
      // We don't want to use it.
      SizeProperty := TFileSizeProperty.Create(0)
    else
{$PUSH}{$R-}
      SizeProperty := TFileSizeProperty.Create(StatInfo.st_size);

    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(
                                  FileTimeToDateTime(StatInfo.st_mtime));
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(
                              FileTimeToDateTime(StatInfo.st_ctime));
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
                                FileTimeToDateTime(StatInfo.st_atime));
{$POP}

    LinkProperty := TFileLinkProperty.Create;

    if AttributesProperty.IsLink then
    begin
      sFullPath := PUnixFindData(SearchRecord.FindHandle)^.sPath + SearchRecord.Name;

      LinkProperty.LinkTo := ReadSymLink(sFullPath);
      // Stat (as opposed to Lstat) will take info of the file that the link points to (recursively).
      LinkProperty.IsValid := fpStat(PChar(UTF8ToSys(sFullPath)), StatInfo) = 0;
      if LinkProperty.IsValid then
      begin
        LinkProperty.IsLinkToDirectory := FPS_ISDIR(StatInfo.st_mode);
      end;
    end;

    {OwnerProperty := TFileOwnerProperty.Create;
    OwnerProperty.Owner := StatInfo.st_uid;
    OwnerProperty.Group := StatInfo.st_gid;
    OwnerProperty.OwnerStr := UIDToStr(StatInfo.st_uid);
    OwnerProperty.GroupStr := GIDToStr(StatInfo.st_gid);

    TypeProperty := TFileTypeProperty.Create;}

{$ELSE}

    AttributesProperty := TFileAttributesProperty.Create(SearchRecord.Attributes);
    SizeProperty := TFileSizeProperty.Create(SearchRecord.Size);
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(SearchRecord.Time);
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(SearchRecord.Time);
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(SearchRecord.Time);
    LinkProperty := TFileLinkProperty.Create;

{$ENDIF}

    // Set name after assigning Attributes property, because it is used to get extension.
    Name := SearchRecord.Name;
  end;
end;

class function TFileSystemFileSource.CreateFileFromFile(const aFilePath: String): TFile;
var
  SearchRecord: TSearchRecEx;
  FindResult: Longint;
begin
  Result := nil;

  FindResult := FindFirstEx(aFilePath, faAnyFile, SearchRecord);
  try
    if FindResult <> 0 then
      raise EFileNotFound.Create(aFilePath);

    Result := CreateFile(ExtractFilePath(aFilePath), SearchRecord);

  finally
    FindCloseEx(SearchRecord);
  end;
end;

class function TFileSystemFileSource.CreateFilesFromFileList(const APath: String; const FileNamesList: TStringList): TFiles;
var
  i: Integer;
begin
  Result := TFiles.Create(APath);
  if Assigned(FileNamesList) and (FileNamesList.Count > 0) then
  begin
    for i := 0 to FileNamesList.Count - 1 do
      Result.Add(CreateFileFromFile(FileNamesList[i]));
  end;
end;

procedure TFileSystemFileSource.RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes);
var
  sFullPath: String;
  Attrs: TFileAttrs;
  AProps: TFilePropertiesTypes;
{$IF DEFINED(UNIX)}
  StatInfo, LinkInfo: BaseUnix.Stat; //buffer for stat info
{$ELSEIF DEFINED(MSWINDOWS)}
  FindData: TWIN32FINDDATAW;
  FindHandle: THandle;
  LinkAttrs: TFileAttrs;
{$ELSE}
  SearchRec: TSearchRecEx;
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
  procedure SetOwnerProperty;
  var
    sUser, sGroup: String;
  begin
    if GetFileOwner(sFullPath, sUser, sGroup) then
      with AFile do
      begin
        OwnerProperty.OwnerStr := sUser;
        OwnerProperty.GroupStr := sGroup;
      end;
  end;
{$ENDIF}

begin
  AProps := AFile.AssignedProperties;

  // Omit properties that are already assigned.
  PropertiesToSet := PropertiesToSet - AProps;

  if PropertiesToSet = [] then
    Exit;  // Already have requested properties.

  // Assume that Name property is always present.
  sFullPath := AFile.FullPath;

  with AFile do
  begin
{$IF DEFINED(MSWINDOWS)}

    // Check if need to get file info record.
    if ([fpAttributes,
         fpSize,
         fpModificationTime,
         fpCreationTime,
         fpLastAccessTime] * PropertiesToSet <> []) or
       ((fpLink in PropertiesToSet) and (not (fpAttributes in AProps))) then
    begin
      FindHandle := FindFirstFileW(PWideChar(UTF8Decode(sFullPath)), @FindData);
      if FindHandle = INVALID_HANDLE_VALUE then
        raise EFileNotFound.Create(sFullPath);
      Windows.FindClose(FindHandle);

      if not (fpAttributes in AProps) then
        AttributesProperty := TNtfsFileAttributesProperty.Create(
          FindData.dwFileAttributes);

      if not (fpSize in AProps) then
        SizeProperty := TFileSizeProperty.Create(
          QWord(FindData.nFileSizeHigh) shl 32 + FindData.nFileSizeLow);

      if not (fpModificationTime in AProps) then
        ModificationTimeProperty := TFileModificationDateTimeProperty.Create(
          WinFileTimeToDateTime(FindData.ftLastWriteTime));

      if not (fpCreationTime in AProps) then
        CreationTimeProperty := TFileCreationDateTimeProperty.Create(
          WinFileTimeToDateTime(FindData.ftCreationTime));

      if not (fpLastAccessTime in AProps) then
        LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
          WinFileTimeToDateTime(FindData.ftLastAccessTime));
    end;

    if fpLink in PropertiesToSet then
    begin
      Attrs := TFileAttributesProperty(Properties[fpAttributes]).Value;

      LinkProperty := TFileLinkProperty.Create;

      if fpS_ISLNK(Attrs) then
      begin
        LinkAttrs := mbFileGetAttrNoLinks(sFullPath);
        LinkProperty.LinkTo := ReadSymLink(sFullPath);
        LinkProperty.IsValid := LinkAttrs <> faInvalidAttributes;
        if LinkProperty.IsValid then
          LinkProperty.IsLinkToDirectory := fpS_ISDIR(LinkAttrs)
        else
          // On Windows links to directories are marked with Directory flag on the link.
          LinkProperty.IsLinkToDirectory := fpS_ISDIR(Attrs);
      end;
    end;

    if fpOwner in PropertiesToSet then
    begin
      OwnerProperty := TFileOwnerProperty.Create;
      OwnerProperty.Owner := 0;
      OwnerProperty.Group := 0;
      SetOwnerProperty;
    end;

    if fpType in PropertiesToSet then
    begin
      TypeProperty := TFileTypeProperty.Create;
      TypeProperty.Value := GetFileDescription(sFullPath);
    end;

{$ELSEIF DEFINED(UNIX)}

    if ([fpAttributes,
         fpSize,
         fpModificationTime,
         fpCreationTime,
         fpLastAccessTime,
         fpOwner] * PropertiesToSet <> []) or
       ((uFileProperty.fpLink in PropertiesToSet) and (not (fpAttributes in AssignedProperties))) then
    begin
      if fpLstat(sFullPath, StatInfo) = -1 then
        raise EFileNotFound.Create(sFullPath);

      if not (fpAttributes in AssignedProperties) then
        AttributesProperty := TUnixFileAttributesProperty.Create(StatInfo.st_mode);

      if not (fpSize in AssignedProperties) then
      begin
        if fpS_ISDIR(StatInfo.st_mode) then
          // On Unix a size for directory entry on filesystem is returned in StatInfo.
          // We don't want to use it.
          SizeProperty := TFileSizeProperty.Create(0)
        else
          SizeProperty := TFileSizeProperty.Create(Int64(StatInfo.st_size));
      end;

      if not (fpModificationTime in AssignedProperties) then
        ModificationTimeProperty := TFileModificationDateTimeProperty.Create(
          FileTimeToDateTime(StatInfo.st_mtime));
      if not (fpCreationTime in AssignedProperties) then
        CreationTimeProperty := TFileCreationDateTimeProperty.Create(
          FileTimeToDateTime(StatInfo.st_ctime));
      if not (fpLastAccessTime in AssignedProperties) then
        LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
          FileTimeToDateTime(StatInfo.st_atime));
    end;

    if uFileProperty.fpLink in PropertiesToSet then
    begin
      LinkProperty := TFileLinkProperty.Create;

      Attrs := TFileAttributesProperty(Properties[fpAttributes]).Value;

      if fpS_ISLNK(Attrs) then
      begin
        LinkProperty.LinkTo := ReadSymLink(sFullPath);
        // Stat (as opposed to Lstat) will take info of the file that the link points to (recursively).
        LinkProperty.IsValid := fpStat(PChar(UTF8ToSys(sFullPath)), LinkInfo) = 0;
        if LinkProperty.IsValid then
        begin
          LinkProperty.IsLinkToDirectory := FPS_ISDIR(LinkInfo.st_mode);
        end;
      end;
    end;

    if fpOwner in PropertiesToSet then
    begin
      OwnerProperty := TFileOwnerProperty.Create;
      OwnerProperty.Owner := StatInfo.st_uid;
      OwnerProperty.Group := StatInfo.st_gid;
      OwnerProperty.OwnerStr := UIDToStr(StatInfo.st_uid);
      OwnerProperty.GroupStr := GIDToStr(StatInfo.st_gid);
    end;

    if fpType in PropertiesToSet then
    begin
      TypeProperty := TFileTypeProperty.Create;
      TypeProperty.Value:= GetFileMimeType(sFullPath);
    end;

{$ELSE}

    if FindFirstEx(sFullPath, 0, SearchRec) = -1 then
      raise EFileNotFound.Create(sFullPath);

    if not (fpAttributes in AssignedProperties) then
      AttributesProperty := TFileAttributesProperty.Create(SearchRec.Attr);
    if not (fpSize in AssignedProperties) then
      SizeProperty := TFileSizeProperty.Create(SearchRec.Size);
    if not (fpModificationTime in AssignedProperties) then
      ModificationTimeProperty := TFileModificationDateTimeProperty.Create(SearchRec.Time);
    if not (fpCreationTime in AssignedProperties) then
      CreationTimeProperty := TFileCreationDateTimeProperty.Create(SearchRec.Time);
    if not (fpLastAccessTime in AssignedProperties) then
      LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(SearchRec.Time);

    FindCloseEx(SearchRec);

    if fpLink in PropertiesToSet then
      LinkProperty := TFileLinkProperty.Create;
    if fpOwner in PropertiesToSet then
      OwnerProperty := TFileOwnerProperty.Create;
    if fpType in PropertiesToSet then
      TypeProperty := TFileTypeProperty.Create;

{$ENDIF}
    if fpComment in PropertiesToSet then
    begin
      CommentProperty := TFileCommentProperty.Create;
      if not Assigned(FDescr) then FDescr:= TDescription.Create(False);
      CommentProperty.Value := FDescr.ReadDescription(sFullPath);
    end;
  end;
end;

class function TFileSystemFileSource.GetFileSource: IFileSystemFileSource;
var
  aFileSource: IFileSource;
begin
  aFileSource := FileSourceManager.Find(TFileSystemFileSource, '');
  if not Assigned(aFileSource) then
    Result := TFileSystemFileSource.Create
  else
    Result := aFileSource as IFileSystemFileSource;
end;

function TFileSystemFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList,
             fsoCopy,
             fsoCopyIn,
             fsoCopyOut,
             fsoMove,
             fsoDelete,
             fsoWipe,
             fsoSplit,
             fsoCombine,
             fsoCreateDirectory,
             fsoCalcChecksum,
             fsoCalcStatistics,
             fsoSetFileProperty,
             fsoExecute];
end;

function TFileSystemFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [
    fspDirectAccess
{$IFDEF UNIX}
  , fspCaseSensitive
{$ENDIF}
  ];
end;

function TFileSystemFileSource.GetCurrentWorkingDirectory: String;
begin
  Result := mbGetCurrentDir();
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function TFileSystemFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  if not mbDirectoryExists(NewDir) then
    Result := False
  else
    Result := mbSetCurrentDir(NewDir);
end;

function TFileSystemFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result := (uDCUtils.GetParentDir(Path) = '');
end;

function TFileSystemFileSource.GetRootDir(sPath : String): String;
begin
  Result := uDCUtils.GetRootDir(sPath);
end;

function TFileSystemFileSource.GetRootDir: String;
begin
  Result := Self.GetRootDir(mbGetCurrentDir);
end;

function TFileSystemFileSource.GetPathType(sPath : String): TPathType;
begin
  Result := uDCUtils.GetPathType(sPath);
end;

function TFileSystemFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := uOSUtils.GetDiskFreeSpace(Path, FreeSize, TotalSize);
end;

function TFileSystemFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize,
             fpAttributes,
             fpModificationTime,
             fpCreationTime,
             fpLastAccessTime,
             uFileProperty.fpLink
            ];
end;

function TFileSystemFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetRetrievableFileProperties
          + [fpOwner,
             fpType,
             fpComment];
end;

function TFileSystemFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemListOperation.Create(TargetFileSource, TargetPath);
end;

function TFileSystemFileSource.CreateCopyOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
var
  FileSource: IFileSource;
begin
  FileSource := Self;
  Result := TFileSystemCopyOperation.Create(FileSource, FileSource, SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
                                                     var SourceFiles: TFiles;
                                                     TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCopyInOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
                                                      var SourceFiles: TFiles;
                                                      TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TFileSystemCopyOutOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateMoveOperation(var SourceFiles: TFiles;
                                                   TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemMoveOperation.Create(TargetFileSource, SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TFileSystemFileSource.CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemWipeOperation.Create(TargetFileSource, FilesToWipe);
end;

function TFileSystemFileSource.CreateSplitOperation(var aSourceFile: TFile;
                                                    aTargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TFileSystemSplitOperation.Create(SourceFileSource, aSourceFile, aTargetPath);
end;

function TFileSystemFileSource.CreateCombineOperation(var SourceFiles: TFiles;
                                                      aTargetFile: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TFileSystemCombineOperation.Create(SourceFileSource, SourceFiles, aTargetFile);
end;

function TFileSystemFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCreateDirectoryOperation.Create(TargetFileSource, BasePath, DirectoryPath);
end;

function TFileSystemFileSource.CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TFileSystemExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TFileSystemFileSource.CreateCalcChecksumOperation(var theFiles: TFiles;
                                                           aTargetPath: String;
                                                           aTargetMask: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCalcChecksumOperation.Create(
                TargetFileSource,
                theFiles,
                aTargetPath,
                aTargetMask);
end;

function TFileSystemFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCalcStatisticsOperation.Create(TargetFileSource, theFiles);
end;

function TFileSystemFileSource.CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                                              var theNewProperties: TFileProperties): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemSetFilePropertyOperation.Create(
                TargetFileSource,
                theTargetFiles,
                theNewProperties);
end;

procedure TFileSystemFileSource.Reload(const PathsToReload: TPathsArray);
begin
  // Don't reload if file watcher is used.
  if not IsFileSystemWatcher then
    inherited Reload(PathsToReload);
end;

{ TFileSystemFileSourceConnection }

procedure TFileSystemFileSourceConnection.SetCurrentPath(NewPath: String);
begin
  if not mbDirectoryExists(NewPath) then
    NewPath := mbGetCurrentDir
  else
    mbSetCurrentDir(NewPath);

  inherited SetCurrentPath(NewPath);
end;

end.

