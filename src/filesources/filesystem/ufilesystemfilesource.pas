unit uFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uLocalFileSource,
  uFileSource, uFileSourceManager,
  uFileSourceProperty,
  uFileProperty,
  uFile,
  uDescr,
  DCBasicTypes,
  DCStrUtils,
  uFindEx
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

    procedure DoReload(const PathsToReload: TPathsArray); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateFile(const APath: String): TFile; override;
    class function CreateFile(const APath: String; pSearchRecord: PSearchRecEx): TFile; overload;
    {en
       Creates a file object using an existing file/directory as a template.
       All the properties will reflect the existing file.
       @param(FilePath denotes absolute path to a file to use as a template.)
    }
    class function CreateFileFromFile(const aFilePath: String): TFile;
    {en
       Creates file list from a list of template files.
       @param(APath
              Path to which the files names are relative.)
       @param(FileNamesList
              A list of absolute paths to files.)
       @param(OmitNotExisting
              If @true then silently omits not existing files.
              If @false an exception is raised when file not exists.)
    }
    class function CreateFilesFromFileList(const APath: String;
                                           const FileNamesList: TStringList;
                                           OmitNotExisting: Boolean = False): TFiles;

    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String); override;

    class function GetFileSource: IFileSystemFileSource;

    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function GetProperties: TFileSourceProperties; override;

    function IsPathAtRoot(Path: String): Boolean; override;

    function GetParentDir(sPath : String): String; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;
    function GetPathType(sPath : String): TPathType; override;

    function CreateDirectory(const Path: String): Boolean; override;
    function FileSystemEntryExists(const Path: String): Boolean; override;
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
                                  aTargetPath: String): TFileSourceOperation; override;
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
    // ------------------------------------------------------
    property Description: TDescription read FDescr;
  end;

  { TFileSystemFileSourceConnection }

  TFileSystemFileSourceConnection = class(TFileSourceConnection)
  protected
    procedure SetCurrentPath(NewPath: String); override;
  end;

implementation

uses
  uOSUtils, DCOSUtils, DCDateTimeUtils, uGlobs, uGlobsPaths, uLog, uLng,
{$IFDEF MSWINDOWS}
  DCWindows, uMyWindows, Windows,
{$ENDIF}
{$IFDEF UNIX}
  BaseUnix, uUsersGroups, LazUTF8, DCUnix, uMyUnix,
  {$IFDEF DARWIN}
  uMyDarwin,
  {$ENDIF}
  {$IFDEF LINUX}
  statx,
  {$ENDIF}
{$ENDIF}
  uFileFunctions,
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

{$IF DEFINED(MSWINDOWS)}

procedure SetOwner(AFile: TFile);
var
  sUser, sGroup: String;
begin
  with AFile do
  begin
    OwnerProperty := TFileOwnerProperty.Create;
    if GetFileOwner(FullPath, sUser, sGroup) then
    begin
      OwnerProperty.OwnerStr := sUser;
      OwnerProperty.GroupStr := sGroup;
    end;
  end;
end;

procedure FillLinkProperty(const AFilePath: String; dwAttrs: DWORD; LinkProperty: TFileLinkProperty);
var
  LinkAttrs: TFileAttrs;
begin
  LinkProperty.LinkTo := ReadSymLink(AFilePath);

  if StrBegins(LinkProperty.LinkTo, 'Volume{') then
  begin
    LinkProperty.IsLinkToDirectory := True;
    LinkProperty.IsValid:= mbDriveReady(AFilePath + PathDelim);
  end
  else begin
    LinkAttrs := mbFileGetAttrNoLinks(AFilePath);
    LinkProperty.IsValid := LinkAttrs <> faInvalidAttributes;
    if LinkProperty.IsValid then
      LinkProperty.IsLinkToDirectory := fpS_ISDIR(LinkAttrs)
    else begin
      // On Windows links to directories are marked with Directory flag on the link.
      LinkProperty.IsLinkToDirectory := fpS_ISDIR(dwAttrs);
    end;
  end;
end;

procedure FillFromFindData(
  AFile: TFile;
  AFilePath: String;
  pFindData: PWIN32FINDDATAW);
begin
  with AFile do
  begin
    AttributesProperty := TNtfsFileAttributesProperty.Create(
      pFindData^.dwFileAttributes);

    SizeProperty := TFileSizeProperty.Create(
      QWord(pFindData^.nFileSizeHigh) shl 32 + pFindData^.nFileSizeLow);

    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(
      WinFileTimeToDateTime(pFindData^.ftLastWriteTime));

    CreationTimeProperty := TFileCreationDateTimeProperty.Create(
      WinFileTimeToDateTime(pFindData^.ftCreationTime));

    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
      WinFileTimeToDateTime(pFindData^.ftLastAccessTime));

    LinkProperty := TFileLinkProperty.Create;

    if fpS_ISLNK(pFindData^.dwFileAttributes) then
    begin
      FillLinkProperty(AFilePath, pFindData^.dwFileAttributes, LinkProperty);
    end;
  end;
end;

{$ELSEIF DEFINED(UNIX)}

procedure FillFromStat(
  AFile: TFile;
  AFilePath: String;
  pStatInfo: PDCStat);
var
  LinkStatInfo: BaseUnix.Stat;
begin
  with AFile do
  begin
    AttributesProperty := TUnixFileAttributesProperty.Create(pStatInfo^.st_mode);

    if fpS_ISDIR(pStatInfo^.st_mode) then
      // On Unix a size for directory entry on filesystem is returned in StatInfo.
      // We don't want to use it.
      SizeProperty := TFileSizeProperty.Create(0)
    else
      SizeProperty := TFileSizeProperty.Create(Int64(pStatInfo^.st_size));

    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(
      FileTimeToDateTimeEx(pStatInfo^.mtime));
    Properties[fpChangeTime] := TFileChangeDateTimeProperty.Create(
      FileTimeToDateTimeEx(pStatInfo^.ctime));
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
      FileTimeToDateTimeEx(pStatInfo^.atime));

    LinkProperty := TFileLinkProperty.Create;

    if fpS_ISLNK(pStatInfo^.st_mode) then
    begin
      LinkProperty.LinkTo := ReadSymLink(AFilePath);
      // Stat (as opposed to Lstat) will take info of the file that the link points to (recursively).
      LinkProperty.IsValid := fpStat(UTF8ToSys(AFilePath), LinkStatInfo) = 0;
      if LinkProperty.IsValid then
      begin
        LinkProperty.IsLinkToDirectory := FPS_ISDIR(LinkStatInfo.st_mode);
        if LinkProperty.IsLinkToDirectory then SizeProperty.Value := 0;
      end;
    end;
  end;
end;

{$ELSE}

procedure FillFromSearchRecord(
  AFile: TFile;
  AFilePath: String;
  pSearchRecord: PSearchRecEx;
  PropertiesToSet: TFilePropertiesTypes = []);
begin
  with AFile do
  begin
    AttributesProperty := TFileAttributesProperty.Create(pSearchRecord^.Attr);
    SizeProperty := TFileSizeProperty.Create(pSearchRecord^.Size);
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(pSearchRecord^.Time);
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(pSearchRecord^.Time);
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(pSearchRecord^.Time);
    LinkProperty := TFileLinkProperty.Create;
  end;
end;

{$ENDIF}

// ----------------------------------------------------------------------------

constructor TFileSystemFileSource.Create;
begin
  inherited Create;

  FDescr := TDescription.Create(False);

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
  inherited Destroy;
  FDescr.Free;
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
    OwnerProperty := TFileOwnerProperty.Create;
    TypeProperty := TFileTypeProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;

class function TFileSystemFileSource.CreateFile(const APath: String; pSearchRecord: PSearchRecEx): TFile;
var
  AFilePath: String;
  LinkAttrs: TFileAttrs;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
{$IF DEFINED(UNIX)}
    ChangeTimeProperty := TFileChangeDateTimeProperty.Create(UnixFileTimeToDateTimeEx(pSearchRecord^.FindData.ctime));
    {$IF DEFINED(DARWIN)}
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(UnixFileTimeToDateTimeEx(pSearchRecord^.FindData.birthtime));
    {$ENDIF}
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(UnixFileTimeToDateTimeEx(pSearchRecord^.FindData.mtime));
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(UnixFileTimeToDateTimeEx(pSearchRecord^.FindData.atime));
{$ELSE}
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(DCBasicTypes.TFileTime(pSearchRecord^.PlatformTime));
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(pSearchRecord^.Time);
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(DCBasicTypes.TFileTime(pSearchRecord^.LastAccessTime));
{$ENDIF}
    SizeProperty := TFileSizeProperty.Create(pSearchRecord^.Size);
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes(pSearchRecord^.Attr);

    LinkProperty := TFileLinkProperty.Create;

    AFilePath:= Path + pSearchRecord^.Name;
    if fpS_ISLNK(pSearchRecord^.Attr) then
    begin
      LinkAttrs := mbFileGetAttrNoLinks(AFilePath);
      LinkProperty.LinkTo := ReadSymLink(AFilePath);
      LinkProperty.IsValid := LinkAttrs <> faInvalidAttributes;
{$IF DEFINED(UNIX)}
      if LinkProperty.IsValid then
      begin
        LinkProperty.IsLinkToDirectory := fpS_ISDIR(LinkAttrs);
        if LinkProperty.IsLinkToDirectory then SizeProperty.Value := 0;
      end;
{$ELSE}
      if StrBegins(LinkProperty.LinkTo, 'Volume{') then
      begin
        LinkProperty.IsLinkToDirectory := True;
        LinkProperty.IsValid:= mbDriveReady(AFilePath + PathDelim);
      end
      else if LinkProperty.IsValid then
        LinkProperty.IsLinkToDirectory := fpS_ISDIR(LinkAttrs)
      else begin
        // On Windows links to directories are marked with Directory flag on the link.
        LinkProperty.IsLinkToDirectory := fpS_ISDIR(pSearchRecord^.Attr);
      end;
{$ENDIF}
    end;
    {$IFDEF DARWIN}
    FinderTagProperty := uMyDarwin.getMacOSFinderTagFileProperty(AFilePath);
    {$ENDIF}
  end;

  // Set name after assigning Attributes property, because it is used to get extension.
  Result.Name := pSearchRecord^.Name;
end;

class function TFileSystemFileSource.CreateFileFromFile(const aFilePath: String): TFile;
var
{$IF DEFINED(MSWINDOWS)}
  FindData: TWIN32FINDDATAW;
  FindHandle: THandle;
{$ELSEIF DEFINED(UNIX)}
  StatInfo: TDCStat;
{$ELSE}
  SearchRecord: TSearchRecEx;
  FindResult: Longint;
{$ENDIF}
begin
  Result := nil;

{$IF DEFINED(MSWINDOWS)}

  FindHandle := FindFirstFileW(PWideChar(UTF16LongName(aFilePath)), @FindData);
  if FindHandle = INVALID_HANDLE_VALUE then
    raise EFileNotFound.Create(aFilePath);
  Windows.FindClose(FindHandle);
  FindData.dwFileAttributes:= ExtractFileAttributes(FindData);

  Result := TFile.Create(ExtractFilePath(aFilePath));
  FillFromFindData(Result, aFilePath, @FindData);

{$ELSEIF DEFINED(UNIX)}

  if DC_fpLstat(UTF8ToSys(AFilePath), StatInfo) = -1 then
    raise EFileNotFound.Create(aFilePath);

  Result := TFile.Create(ExtractFilePath(aFilePath));
  FillFromStat(Result, aFilePath, @StatInfo);

{$ELSE}

  FindResult := FindFirstEx(aFilePath, 0, SearchRecord);
  try
    if FindResult <> 0 then
      raise EFileNotFound.Create(aFilePath);

    Result := TFile.Create(ExtractFilePath(aFilePath));
    FillFromSearchRecord(Result, aFilePath, @SearchRecord);

  finally
    FindCloseEx(SearchRecord);
  end;

{$ENDIF}

  // Set name after assigning Attributes property, because it is used to get extension.
  Result.FullPath := aFilePath;
end;

class function TFileSystemFileSource.CreateFilesFromFileList(
    const APath: String;
    const FileNamesList: TStringList;
    OmitNotExisting: Boolean): TFiles;
var
  i: Integer;
begin
  Result := TFiles.Create(APath);
  if Assigned(FileNamesList) and (FileNamesList.Count > 0) then
  begin
    for i := 0 to FileNamesList.Count - 1 do
    begin
      try
        Result.Add(CreateFileFromFile(FileNamesList[i]));
      except
        on EFileNotFound do
          if not OmitNotExisting then
          begin
            FreeAndNil(Result);
            raise;
          end;
        on Exception do
          begin
            FreeAndNil(Result);
            raise;
          end;
      end;
    end;
  end;
end;

procedure TFileSystemFileSource.RetrieveProperties(AFile: TFile;
  PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String);
var
  AIndex: Integer;
  sFullPath: String;
  Attrs: TFileAttrs;
  AProp: TFilePropertyType;
  AProps: TFilePropertiesTypes;
  AVariant: TFileVariantProperty;
{$IF DEFINED(LINUX)}
  StatXInfo: TStatX;
{$ENDIF}
{$IF DEFINED(UNIX)}
  StatInfo: TDCStat;
  LinkInfo: BaseUnix.Stat; //buffer for stat info
{$ELSEIF DEFINED(MSWINDOWS)}
  FindData: TWIN32FINDDATAW;
  FindHandle: THandle;
{$ELSE}
  SearchRec: TSearchRecEx;
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
      FindHandle := FindFirstFileW(PWideChar(UTF16LongName(sFullPath)), @FindData);
      if FindHandle = INVALID_HANDLE_VALUE then
        raise EFileNotFound.Create(sFullPath);
      Windows.FindClose(FindHandle);
      FindData.dwFileAttributes:= ExtractFileAttributes(FindData);

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
      Attrs := Attributes;

      LinkProperty := TFileLinkProperty.Create;

      if fpS_ISLNK(Attrs) then
      begin
        FillLinkProperty(sFullPath, Attrs, LinkProperty);
      end;
    end;

    if fpOwner in PropertiesToSet then
    begin
      SetOwner(AFile);
    end;

    if fpType in PropertiesToSet then
    begin
      TypeProperty := TFileTypeProperty.Create;
      TypeProperty.Value := GetFileDescription(sFullPath);
    end;

    if fpCompressedSize in PropertiesToSet then
    begin
      CompressedSizeProperty := TFileCompressedSizeProperty.Create;
      CompressedSizeProperty.Value := mbGetCompressedFileSize(sFullPath);
    end;

    if fpChangeTime in PropertiesToSet then
    begin
      ChangeTimeProperty := TFileChangeDateTimeProperty.Create(MinDateTime);
      ChangeTimeProperty.IsValid := mbGetFileChangeTime(sFullPath, FindData.ftCreationTime);
      if ChangeTimeProperty.IsValid then
      begin
        ChangeTimeProperty.Value := WinFileTimeToDateTime(FindData.ftCreationTime);
      end;
    end;
{$ELSEIF DEFINED(UNIX)}

    if ([fpAttributes,
         fpSize,
         fpModificationTime,
         fpChangeTime,
         fpLastAccessTime,
         fpOwner] * PropertiesToSet <> []) or
       ((uFileProperty.fpLink in PropertiesToSet) and (not (fpAttributes in AssignedProperties))) then
    begin
      if DC_fpLStat(UTF8ToSys(sFullPath), StatInfo) = -1 then
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
          FileTimeToDateTimeEx(StatInfo.mtime));
      if not (fpChangeTime in AssignedProperties) then
        Properties[fpChangeTime] := TFileChangeDateTimeProperty.Create(
          FileTimeToDateTimeEx(StatInfo.ctime));
      if not (fpLastAccessTime in AssignedProperties) then
        LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(
          FileTimeToDateTimeEx(StatInfo.atime));
{$IF DEFINED(DARWIN)}
      if not (fpCreationTime in AssignedProperties) then
        CreationTimeProperty := TFileCreationDateTimeProperty.Create(
          FileTimeToDateTimeEx(StatInfo.birthtime));
{$ENDIF}
    end;

    if uFileProperty.fpLink in PropertiesToSet then
    begin
      Attrs := Attributes;

      LinkProperty := TFileLinkProperty.Create;

      if fpS_ISLNK(Attrs) then
      begin
        LinkProperty.LinkTo := ReadSymLink(sFullPath);
        // Stat (as opposed to Lstat) will take info of the file that the link points to (recursively).
        LinkProperty.IsValid := fpStat(UTF8ToSys(sFullPath), LinkInfo) = 0;
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
      {$IF DEFINED(DARWIN)}
      TypeProperty.Value:= GetFileDescription(sFullPath);
      {$ELSE}
      TypeProperty.Value:= GetFileMimeType(sFullPath);
      {$ENDIF}
    end;

    {$IF DEFINED(LINUX)}
    if fpCreationTime in PropertiesToSet then
    begin
      CreationTimeProperty := TFileCreationDateTimeProperty.Create(MinDateTime);
      CreationTimeProperty.IsValid := HasStatX and (fpstatx(0, sFullPath, 0, STATX_BTIME, @StatXInfo) >= 0) and
                                      (StatXInfo.stx_mask and STATX_BTIME <> 0) and (StatXInfo.stx_btime.tv_sec > 0);
      if CreationTimeProperty.IsValid then
      begin
        CreationTimeProperty.Value:= FileTimeToDateTimeEx(TFileTimeEx.Create(Int64(StatXInfo.stx_btime.tv_sec), Int64(StatXInfo.stx_btime.tv_nsec)));
      end;
    end;
    {$ENDIF}

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
      CommentProperty.Value := FDescr.ReadDescription(sFullPath);
    end;

{$IFDEF DARWIN}
   if fpMacOSFinderTag in PropertiesToSet then begin
     FinderTagProperty := uMyDarwin.getMacOSFinderTagFileProperty(sFullPath);
   end;
{$ENDIF}

    PropertiesToSet:= PropertiesToSet * fpVariantAll;
    for AProp in PropertiesToSet do
    begin
      AIndex:= Ord(AProp) - Ord(fpVariant);
      if (AIndex >= 0) and (AIndex <= High(AVariantProperties)) then
      begin
        AVariant:= TFileVariantProperty.Create(AVariantProperties[AIndex]);
        AVariant.Value:= GetVariantFileProperty(AVariantProperties[AIndex], AFile, Self);
        Properties[AProp]:= AVariant;
      end;
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
    fspDirectAccess, fspListFlatView, fspNoneParent
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

procedure TFileSystemFileSource.DoReload(const PathsToReload: TPathsArray);
begin
  FDescr.Reset;
end;

function TFileSystemFileSource.IsPathAtRoot(Path: String): Boolean;
var
  sPath: String;
begin
  sPath := ExcludeTrailingPathDelimiter(Path);
  if (Pos('\\', sPath) = 1) and (NumCountChars(PathDelim, sPath) = 3) then
    Exit(True);
  Result := (DCStrUtils.GetParentDir(Path) = '');
end;

function TFileSystemFileSource.GetParentDir(sPath: String): String;
begin
  Result:= inherited GetParentDir(sPath);
  Result:= GetDeepestExistingPath(Result);
  if Length(Result) = 0 then Result:= gpExePath;
end;

function TFileSystemFileSource.GetRootDir(sPath : String): String;
begin
  Result := DCStrUtils.GetRootDir(sPath);
end;

function TFileSystemFileSource.GetRootDir: String;
begin
  Result := Self.GetRootDir(mbGetCurrentDir);
end;

function TFileSystemFileSource.GetPathType(sPath : String): TPathType;
begin
  Result := DCStrUtils.GetPathType(sPath);
end;

function TFileSystemFileSource.CreateDirectory(const Path: String): Boolean;
begin
  Result := mbCreateDir(Path);
  if Result then
  begin
    if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Format(rsMsgLogSuccess + rsMsgLogMkDir, [Path]), lmtSuccess);
  end
  else begin
    if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Format(rsMsgLogError + rsMsgLogMkDir, [Path]), lmtError);
  end;
end;

function TFileSystemFileSource.FileSystemEntryExists(const Path: String): Boolean;
begin
  Result:= mbFileSystemEntryExists(Path);
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
             {$IF DEFINED(MSWINDOWS)}
             fpCreationTime,
             {$ELSE}
             fpChangeTime,
             {$ENDIF}
             fpLastAccessTime,
             uFileProperty.fpLink
             {$IF DEFINED(DARWIN)}
             ,fpMacOSFinderTag
             {$ENDIF}
            ];
end;

function TFileSystemFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetRetrievableFileProperties
          + [fpSize,
             fpAttributes,
             fpModificationTime,
             {$IF DEFINED(MSWINDOWS)}
             fpCreationTime,
             {$ENDIF}
             fpChangeTime,
             fpLastAccessTime,
             uFileProperty.fpLink,
             fpOwner,
             fpType,
             fpComment
             {$IF DEFINED(MSWINDOWS)}
             , fpCompressedSize
             {$ENDIF}
             {$IF DEFINED(DARWIN)}
             ,fpMacOSFinderTag
             {$ENDIF}
             ] + fpVariantAll;
{$IF DEFINED(LINUX)}
  if HasStatX then Result += [fpCreationTime];
{$ENDIF}
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

