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

    property Size: Int64 read GetSize write SetSize;
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property ModificationTime: TDateTime read GetModificationTime write SetModificationTime;
  end;

  TFileSystemFiles = class(TFiles)
  public
    function CreateObjectOfSameType: TFiles; override;
    function Clone: TFileSystemFiles; override;

    {en
       Fills a files list from filenames list.
       @param(FileNamesList
              A list of absolute paths to files.)
    }
    procedure LoadFromFileNames(const FileNamesList: TStringList);
  end;

implementation

uses
  uFindEx
{$IFDEF UNIX}
  , BaseUnix, uUsersGroups
{$ENDIF}
  ;

constructor TFileSystemFile.Create;
begin
  inherited Create;

  FSize := TFileSizeProperty.Create;
  FAttributes := TNtfsFileAttributesProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

constructor TFileSystemFile.Create(SearchRecord: TSearchRec);
{$IF DEFINED(UNIX)}
var
  sb: BaseUnix.Stat; //buffer for stat info
{$ENDIF}
begin
  inherited Create;

{$IF DEFINED(MSWINDOWS)}

  FAttributes := TNtfsFileAttributesProperty.Create(SearchRecord.Attr);
  FSize := TFileSizeProperty.Create(SearchRecord.Size);
  FModificationTime := TFileModificationDateTimeProperty.Create(
                           FileDateToDateTime(SearchRecord.Time));

  //Other times: SearchRecord.FindData.ftCreationTime ...?

{$ELSEIF DEFINED(UNIX)}

  sb := PUnixFindData(SearchRecord.FindHandle)^.StatRec;

  FAttributes := TUnixFileAttributesProperty.Create(sb.st_mode);
  FSize := TFileSizeProperty.Create(sb.st_size);
  FModificationTime := TFileModificationDateTimeProperty.Create(
                           FileDateToDateTime(sb.st_mtime));

{
    iOwner:=sb.st_uid;
    iGroup:=sb.st_gid;
    sOwner:=UIDToStr(iOwner);
    sGroup:=GIDToStr(iGroup);
}

{$ELSE}

  // Create with default values.
  FAttributes := TFileAttributesProperty.Create;
  FSize := TFileSizeProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;

{$ENDIF}

{
  if IsLink then
    sLinkTo := ReadSymLink(SearchRecord.Name)
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
begin
  if FindFirstEx(FilePath, faAnyFile, SearchRecord) <> 0 then
  begin
    FindCloseEx(SearchRecord);
    raise Exception.Create('File ' + FilePath + ' does not exist.');
  end
  else
    Create(SearchRecord);

  Path := ExtractFilePath(FilePath);

  FindCloseEx(SearchRecord);
end;

destructor TFileSystemFile.Destroy;
begin
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);
  if Assigned(FSize) then
    FreeAndNil(FSize);
  if Assigned(FModificationTime) then
    FreeAndNil(FModificationTime);
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

    with AFile as TFileSystemFile do
    begin
      FSize := Self.FSize.Clone;
      FAttributes := Self.FAttributes.Clone;
      FModificationTime := Self.FModificationTime.Clone;
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

end.

