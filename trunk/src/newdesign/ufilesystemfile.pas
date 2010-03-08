unit uFileSystemFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes,
  uLocalFile,
  uFile,
  uFileProperty;

type

  TFileSystemFile = class(TLocalFile)
  private
    FSize: TFileSizeProperty;
    FAttributes: TFileAttributesProperty;
    FModificationTime: TFileModificationDateTimeProperty;
    FCreationTime: TFileCreationDateTimeProperty;
    FLastAccessTime: TFileLastAccessDateTimeProperty;
    FIsLinkToDirectory: Boolean;

    procedure AssignProperties;

  protected
    function GetAttributes: Cardinal; virtual;
    procedure SetAttributes(NewAttributes: Cardinal); virtual;
    function GetSize: Int64; virtual;
    procedure SetSize(NewSize: Int64); virtual;
    function GetModificationTime: TDateTime; virtual;
    procedure SetModificationTime(NewTime: TDateTime); virtual;

  public
    constructor Create(const APath: String); override;
    constructor Create(const APath: String; SearchRecord: TSearchRecEx); overload;
    {en
       Creates a file object using an existing file/directory as a template.
       All the properties will reflect the existing file.
       @param(FilePath denotes absolute path to a file to use as a template.)
    }
    constructor CreateFromFile(const aFilePath: String); overload;

    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone: TFileSystemFile; override;
    procedure CloneTo(AFile: TFile); override;

    class function GetSupportedProperties: TFilePropertiesTypes; override;

    function IsLinkToDirectory: Boolean; override;

    property Size: Int64 read GetSize write SetSize;
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property ModificationTime: TDateTime read GetModificationTime write SetModificationTime;
  end;

  TFileSystemFiles = class(TFiles)
  protected
    function Get(Index: Integer): TFileSystemFile;

  public
    {en
       Creates file list from a list of template files.
       @param(FileNamesList
              A list of absolute paths to files.)
    }
    constructor CreateFromFiles(const APath: String; const FileNamesList: TStringList); virtual; overload;

    function CreateObjectOfSameType(const APath: String): TFiles; override;
    function CreateFileObject(const APath: String): TFile; override;
    function Clone: TFileSystemFiles; override;

    property Items[Index: Integer]: TFileSystemFile read Get{ write Put}; default;
  end;

  EFileSystemFileNotExists = class(Exception);

implementation

uses
  uFindEx, uDateTimeUtils
{$IFDEF UNIX}
  , BaseUnix, uUsersGroups, FileUtil
{$ENDIF}
  ;

constructor TFileSystemFile.Create(const APath: String);
begin
  inherited Create(APath);

  FAttributes := TFileAttributesProperty.CreateOSAttributes;
  FSize := TFileSizeProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;
  FCreationTime := TFileCreationDateTimeProperty.Create;
  FLastAccessTime := TFileLastAccessDateTimeProperty.Create;
  FIsLinkToDirectory := False;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

constructor TFileSystemFile.Create(const APath: String; SearchRecord: TSearchRecEx);
{$IF DEFINED(UNIX)}
var
  StatInfo: BaseUnix.Stat; //buffer for stat info
  sFullPath: String;
{$ENDIF}
begin
  inherited Create(APath);

{$IF DEFINED(MSWINDOWS)}

  FAttributes := TNtfsFileAttributesProperty.Create(SearchRecord.Attr);
  FSize := TFileSizeProperty.Create(SearchRecord.Size);
  FModificationTime := TFileModificationDateTimeProperty.Create(
                           WinFileTimeToDateTime(SearchRecord.FindData.ftLastWriteTime));
  FCreationTime := TFileCreationDateTimeProperty.Create(
                           WinFileTimeToDateTime(SearchRecord.FindData.ftCreationTime));
  FLastAccessTime := TFileLastAccessDateTimeProperty.Create(
                           WinFileTimeToDateTime(SearchRecord.FindData.ftLastAccessTime));

  FIsLinkToDirectory := FAttributes.IsLink and FAttributes.IsDirectory;

{$ELSEIF DEFINED(UNIX)}

  StatInfo := PUnixFindData(SearchRecord.FindHandle)^.StatRec;

  FAttributes := TUnixFileAttributesProperty.Create(StatInfo.st_mode);
  if FAttributes.IsDirectory then
    // On Unix a size for directory entry on filesystem is returned in StatInfo.
    // We don't want to use it.
    FSize := TFileSizeProperty.Create(0)
  else
{$PUSH}{$R-}
    FSize := TFileSizeProperty.Create(StatInfo.st_size);

  FModificationTime := TFileModificationDateTimeProperty.Create(
                           FileTimeToDateTime(StatInfo.st_mtime));
  FCreationTime := TFileCreationDateTimeProperty.Create(
                           FileTimeToDateTime(StatInfo.st_ctime));
  FLastAccessTime := TFileLastAccessDateTimeProperty.Create(
                           FileTimeToDateTime(StatInfo.st_atime));
{$POP}

  if FAttributes.IsLink then
  begin
    sFullPath := PUnixFindData(SearchRecord.FindHandle)^.sPath
               + SearchRecord.Name;

    // Stat (as opposed to Lstat) will take info of the file that the link points to (recursively).
    fpStat(PChar(UTF8ToSys(sFullPath)), StatInfo);

    FIsLinkToDirectory := FPS_ISDIR(StatInfo.st_mode);
  end
  else
    FIsLinkToDirectory := False;

{
    iOwner:=sb.st_uid;
    iGroup:=sb.st_gid;
    sOwner:=UIDToStr(iOwner);
    sGroup:=GIDToStr(iGroup);
}

{$ELSE}

  FAttributes := TFileAttributesProperty.Create(SearchRecord.Attributes);
  FSize := TFileSizeProperty.Create(SearchRecord.Size);
  FModificationTime := TFileModificationDateTimeProperty.Create(SearchRecord.Time);
  FCreationTime := TFileCreationDateTimeProperty.Create(SearchRecord.Time);
  FLastAccessTime := TFileLastAccessDateTimeProperty.Create(SearchRecord.Time);
  FIsLinkToDirectory := False;

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

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := SearchRecord.Name;
end;

constructor TFileSystemFile.CreateFromFile(const aFilePath: String);
var
  SearchRecord: TSearchRecEx;
  FindResult: Longint;
begin
  FindResult := FindFirstEx(aFilePath, faAnyFile, SearchRecord);
  try
    if FindResult <> 0 then
      raise EFileSystemFileNotExists.Create('File ' + aFilePath + ' does not exist.');

    Create(ExtractFilePath(aFilePath), SearchRecord);

  finally
    FindCloseEx(SearchRecord);
  end;
end;

destructor TFileSystemFile.Destroy;
begin
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);
  if Assigned(FSize) then
    FreeAndNil(FSize);
  if Assigned(FModificationTime) then
    FreeAndNil(FModificationTime);
  if Assigned(FCreationTime) then
    FreeAndNil(FCreationTime);
  if Assigned(FLastAccessTime) then
    FreeAndNil(FLastAccessTime);

  inherited Destroy;
end;

function TFileSystemFile.Clone: TFileSystemFile;
begin
  Result := TFileSystemFile.Create(Path);
  CloneTo(Result);
end;

procedure TFileSystemFile.CloneTo(AFile: TFile);
begin
  if Assigned(AFile) then
  begin
    inherited CloneTo(AFile);
    // All properties are cloned in base class.

    with AFile as TFileSystemFile do
    begin
      FIsLinkToDirectory := Self.FIsLinkToDirectory;
    end;
  end;
end;

procedure TFileSystemFile.AssignProperties;
begin
  FProperties[fpSize] := FSize;
  FProperties[fpAttributes] := FAttributes;
  FProperties[fpModificationTime] := FModificationTime;
  FProperties[fpCreationTime] := FCreationTime;
  FProperties[fpLastAccessTime] := FLastAccessTime;
end;

class function TFileSystemFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedProperties
          + [fpSize, fpAttributes, fpModificationTime, fpCreationTime,
             fpLastAccessTime];
end;

function TFileSystemFile.GetAttributes: Cardinal;
begin
  Result := FAttributes.Value;
end;

procedure TFileSystemFile.SetAttributes(NewAttributes: Cardinal);
begin
  FAttributes.Value := NewAttributes;
end;

function TFileSystemFile.GetSize: Int64;
begin
  Result := FSize.Value;
end;

procedure TFileSystemFile.SetSize(NewSize: Int64);
begin
  FSize.Value := NewSize;
end;

function TFileSystemFile.GetModificationTime: TDateTime;
begin
  Result := FModificationTime.Value;
end;

procedure TFileSystemFile.SetModificationTime(NewTime: TDateTime);
begin
  FModificationTime.Value := NewTime;
end;

function TFileSystemFile.IsLinkToDirectory: Boolean;
begin
  Result := FIsLinkToDirectory;
end;

// ----------------------------------------------------------------------------

constructor TFileSystemFiles.CreateFromFiles(const APath: String; const FileNamesList: TStringList);
var
  AFile: TFileSystemFile;
  i: Integer;
begin
  inherited Create(APath);

  if Assigned(FileNamesList) and (FileNamesList.Count > 0) then
  begin
    for i := 0 to FileNamesList.Count - 1 do
    begin
      AFile := TFileSystemFile.CreateFromFile(FileNamesList[i]);
      Add(AFile);
    end;
  end;
end;

function TFileSystemFiles.CreateObjectOfSameType(const APath: String): TFiles;
begin
  Result := TFileSystemFiles.Create(APath);
end;

function TFileSystemFiles.CreateFileObject(const APath: String): TFile;
begin
  Result := TFileSystemFile.Create(APath);
end;

function TFileSystemFiles.Clone: TFileSystemFiles;
begin
  Result := TFileSystemFiles.Create(Path);
  CloneTo(Result);
end;

function TFileSystemFiles.Get(Index: Integer): TFileSystemFile;
begin
  Result := TFileSystemFile(List.Items[Index]);
end;

end.

