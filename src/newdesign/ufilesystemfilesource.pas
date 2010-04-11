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

  protected
    function GetCurrentWorkingDirectory: String; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

  public
    constructor Create; override;

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

    class function GetFileSource: IFileSystemFileSource;

    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;
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
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
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

  EFileSystemFileNotExists = class(Exception);

implementation

uses
  uOSUtils, uFindEx, uDateTimeUtils,
{$IFDEF UNIX}
  BaseUnix, uUsersGroups, FileUtil,
{$ENDIF}
  uFileSystemWatcher,
  uFileSystemListOperation,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation,
  uFileSystemDeleteOperation,
  uFileSystemWipeOperation,
  uFileSystemCreateDirectoryOperation,
  uFileSystemExecuteOperation,
  uFileSystemCalcChecksumOperation,
  uFileSystemCalcStatisticsOperation,
  uFileSystemSetFilePropertyOperation;

constructor TFileSystemFileSource.Create;
begin
  inherited Create;
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
    LinkProperty := TFileLinkProperty.Create(False);
  end;
end;

class function TFileSystemFileSource.CreateFile(const APath: String; SearchRecord: TSearchRecEx): TFile;
{$IF DEFINED(UNIX)}
var
  StatInfo: BaseUnix.Stat; //buffer for stat info
  sFullPath: String;
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
    LinkProperty := TFileLinkProperty.Create(AttributesProperty.IsLink and AttributesProperty.IsDirectory);

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

    if AttributesProperty.IsLink then
    begin
      sFullPath := PUnixFindData(SearchRecord.FindHandle)^.sPath
                 + SearchRecord.Name;

      // Stat (as opposed to Lstat) will take info of the file that the link points to (recursively).
      fpStat(PChar(UTF8ToSys(sFullPath)), StatInfo);

      LinkProperty.IsLinkToDirectory := FPS_ISDIR(StatInfo.st_mode);
    end
    else
      LinkProperty.IsLinkToDirectory := False;

  {
      iOwner:=sb.st_uid;
      iGroup:=sb.st_gid;
      sOwner:=UIDToStr(iOwner);
      sGroup:=GIDToStr(iGroup);
  }

{$ELSE}

    AttributesProperty := TFileAttributesProperty.Create(SearchRecord.Attributes);
    SizeProperty := TFileSizeProperty.Create(SearchRecord.Size);
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(SearchRecord.Time);
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(SearchRecord.Time);
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(SearchRecord.Time);
    LinkProperty := TFileLinkProperty.Create(False);

{$ENDIF}

  {
    if IsLink then
    begin
      sLinkTo := ReadSymLink(PUnixFindData(SearchRecord.FindHandle)^.sPath + SearchRecord.Name);
      if sLinkTo <> '' then
      begin
        case uDCUtils.GetPathType(sLinkTo) of
          ptNone, ptRelative:
            sLinkTo := PUnixFindData(SearchRecord.FindHandle)^.sPath + sLinkTo;
        end;
    end
    else
      sLinkTo := '';
  }

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
      raise EFileSystemFileNotExists.Create('File ' + aFilePath + ' does not exist.');

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
             fsoCreateDirectory,
             fsoCalcChecksum,
             fsoCalcStatistics,
             fsoSetFileProperty,
             fsoExecute];
end;

function TFileSystemFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  SetLength(Result, 2);

  Result[0] := TFileSizeProperty.GetDescription;
  Result[1] := TFileModificationDateTimeProperty.GetDescription;
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
  Result := GetDiskFreeSpace(Path, FreeSize, TotalSize);
end;

function TFileSystemFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize, fpAttributes, fpModificationTime, fpCreationTime,
             fpLastAccessTime, fpLink];
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

function TFileSystemFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCreateDirectoryOperation.Create(TargetFileSource, BasePath, DirectoryPath);
end;

function TFileSystemFileSource.CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
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

