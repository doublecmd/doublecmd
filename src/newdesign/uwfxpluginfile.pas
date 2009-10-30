unit uWfxPluginFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileProperty,
  WfxPlugin,
  uOSUtils;

type

  { TWfxPluginFile }

  TWfxPluginFile = class(TFile)
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
    constructor Create(FileAttributes: TFileAttributesProperty); overload;
    constructor Create(FindData: TWin32FindData); overload;

    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone: TWfxPluginFile; override;
    procedure CloneTo(AFile: TFile); override;

    class function GetSupportedProperties: TFilePropertiesTypes; override;

    function IsLinkToDirectory: Boolean; override;

    property Size: Int64 read GetSize write SetSize;
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property ModificationTime: TDateTime read GetModificationTime write SetModificationTime;
  end;

implementation

uses
  LCLProc, FileUtil, uFileAttributes;

constructor TWfxPluginFile.Create;
begin
  inherited Create;

  FAttributes := TNtfsFileAttributesProperty.Create;
  FSize := TFileSizeProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;
  FIsLinkToDirectory := False;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

constructor TWfxPluginFile.Create(FileAttributes: TFileAttributesProperty);
begin
  inherited Create;

  FAttributes := FileAttributes.Clone;
  FSize := TFileSizeProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;
  FIsLinkToDirectory := False;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

constructor TWfxPluginFile.Create(FindData: TWin32FindData);
begin
  inherited Create;

  // Check that attributes is used
  if (FindData.dwFileAttributes and FILE_ATTRIBUTE_UNIX_MODE) = 0 then // Windows attributes
    begin
      FIsLinkToDirectory := (FindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0;
      FAttributes:= TNtfsFileAttributesProperty.Create(FindData.dwFileAttributes);
    end
  else  // Unix attributes
    begin
      FIsLinkToDirectory := ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and
                            ((FindData.dwReserved0 and S_IFMT) = S_IFLNK);
      if ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and
         ((FindData.dwReserved0 and S_IFMT) <> S_IFDIR) then
        FindData.dwReserved0:= FindData.dwReserved0 or S_IFDIR;
      FAttributes:= TUnixFileAttributesProperty.Create(FindData.dwReserved0);
    end;

  FSize := TFileSizeProperty.Create((Int64(FindData.nFileSizeHigh) * MAXDWORD) + FindData.nFileSizeLow);
  FModificationTime := TFileModificationDateTimeProperty.Create(FileTimeToDateTime(FindData.ftLastWriteTime));

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := SysToUTF8(FindData.cFileName);
end;

destructor TWfxPluginFile.Destroy;
begin
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);
  if Assigned(FSize) then
    FreeAndNil(FSize);
  if Assigned(FModificationTime) then
    FreeAndNil(FModificationTime);

  inherited Destroy;
end;

function TWfxPluginFile.Clone: TWfxPluginFile;
begin
  Result := TWfxPluginFile.Create(FAttributes);
  CloneTo(Result);
end;

procedure TWfxPluginFile.CloneTo(AFile: TFile);
begin
  if Assigned(AFile) then
  begin
    inherited CloneTo(AFile);
    // All properties are cloned in base class.

    with AFile as TWfxPluginFile do
    begin
      FIsLinkToDirectory := Self.FIsLinkToDirectory;
    end;
  end;
end;

procedure TWfxPluginFile.AssignProperties;
begin
  FProperties[fpSize] := FSize;
  FProperties[fpAttributes] := FAttributes;
  FProperties[fpModificationTime] := FModificationTime;
end;

class function TWfxPluginFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := [{fpName, }fpSize, fpAttributes, fpModificationTime];
end;

function TWfxPluginFile.GetAttributes: Cardinal;
begin
  Result := FAttributes.Value;
end;

procedure TWfxPluginFile.SetAttributes(NewAttributes: Cardinal);
begin
  FAttributes.Value := NewAttributes;
end;

function TWfxPluginFile.GetSize: Int64;
begin
  Result := FSize.Value;
end;

procedure TWfxPluginFile.SetSize(NewSize: Int64);
begin
  FSize.Value := NewSize;
end;

function TWfxPluginFile.GetModificationTime: TDateTime;
begin
  Result := FModificationTime.Value;
end;

procedure TWfxPluginFile.SetModificationTime(NewTime: TDateTime);
begin
  FModificationTime.Value := NewTime;
end;

function TWfxPluginFile.IsLinkToDirectory: Boolean;
begin
  Result := FIsLinkToDirectory;
end;

end.

