unit uFileSystemFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLocalFile,
  uFile,
  uFileProperty,
  uOSUtils;

type

  TFileSystemFile = class(TLocalFile)
  private
    FSize: TFileSizeProperty;
    FAttributes: TFileAttributesProperty;
    FModificationTime: TFileModificationDateTimeProperty;
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
    constructor Create; override;
    constructor Create(SearchRecord: TSearchRec); overload;
    {en
       Creates a file using an existing file as a template.
       All the properties will reflect the existing file.
       @param(FilePath denotes absolute path to a file to use as a template.)
    }
    constructor Create(FilePath: String); overload;

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
    function CreateObjectOfSameType: TFiles; override;
    function Clone: TFileSystemFiles; override;

    {en
       Fills a files list from filenames list.
       @param(FileNamesList
              A list of absolute paths to files.)
    }
    procedure LoadFromFileNames(const FileNamesList: TStringList);

    property Items[Index: Integer]: TFileSystemFile read Get{ write Put}; default;
  end;

  EFileSystemFileNotExists = class(Exception);

implementation

uses
  uFindEx
{$IFDEF UNIX}
  , BaseUnix, uUsersGroups, uDCUtils, FileUtil
{$ENDIF}
  ;

constructor TFileSystemFile.Create;
begin
  inherited Create;

  FSize := TFileSizeProperty.Create;
  FAttributes := TNtfsFileAttributesProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;
  FIsLinkToDirectory := False;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

constructor TFileSystemFile.Create(SearchRecord: TSearchRec);
{$IF DEFINED(UNIX)}
var
  StatInfo: BaseUnix.Stat; //buffer for stat info
  sFullPath: String;
{$ENDIF}
begin
  inherited Create;

{$IF DEFINED(MSWINDOWS)}

  FAttributes := TNtfsFileAttributesProperty.Create(SearchRecord.Attr);
  FSize := TFileSizeProperty.Create(SearchRecord.Size);
  FModificationTime := TFileModificationDateTimeProperty.Create(
                           FileDateToDateTime(SearchRecord.Time));

  //Because symbolic link works on Windows 2k/XP for directories only
  FIsLinkToDirectory := FAttributes.IsLink;

  //Other times: SearchRecord.FindData.ftCreationTime ...?

{$ELSEIF DEFINED(UNIX)}

  StatInfo := PUnixFindData(SearchRecord.FindHandle)^.StatRec;

  FAttributes := TUnixFileAttributesProperty.Create(StatInfo.st_mode);
  if FAttributes.IsDirectory then
    // On Unix a size for directory entry on filesystem is returned in StatInfo.
    // We don't want to use it.
    FSize := TFileSizeProperty.Create(0)
  else
    FSize := TFileSizeProperty.Create(StatInfo.st_size);
  FModificationTime := TFileModificationDateTimeProperty.Create(
                           FileDateToDateTime(StatInfo.st_mtime));

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

constructor TFileSystemFile.Create(FilePath: String);
var
  SearchRecord: TSearchRec;
  FindResult: Longint;
begin
  FindResult := FindFirstEx(FilePath, faAnyFile, SearchRecord);
  try
    if FindResult <> 0 then
    begin
      raise EFileSystemFileNotExists.Create('File ' + FilePath + ' does not exist.');
    end
    else
      Create(SearchRecord);

    Path := ExtractFilePath(FilePath);

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

  inherited;
end;

function TFileSystemFile.Clone: TFileSystemFile;
begin
  Result := TFileSystemFile.Create;
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
end;

class function TFileSystemFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := [{fpName, }fpSize, fpAttributes, fpModificationTime];
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

function TFileSystemFiles.CreateObjectOfSameType: TFiles;
begin
  Result := TFileSystemFiles.Create;
end;

function TFileSystemFiles.Clone: TFileSystemFiles;
begin
  Result := TFileSystemFiles.Create;
  CloneTo(Result);
end;

procedure TFileSystemFiles.LoadFromFileNames(const FileNamesList: TStringList);
var
  AFile: TFileSystemFile;
  i: Integer;
begin
  Clear;

  if not Assigned(FileNamesList) or (FileNamesList.Count <= 0) then Exit;

  Path := ExtractFilePath(FileNamesList[0]);

  for i := 0 to FileNamesList.Count - 1 do
    begin
      AFile := TFileSystemFile.Create(FileNamesList[i]);
      Add(AFile);
    end;
end;

function TFileSystemFiles.Get(Index: Integer): TFileSystemFile;
begin
  Result := TFileSystemFile(List.Items[Index]);
end;

end.

