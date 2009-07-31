unit uArchiveFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLocalFile, uFile, uFileProperty;

type

  TArchiveFile = class(TLocalFile)
  private
    FSize: TFileSizeProperty;
    FCompressedSize: TFileSizeProperty; // TFileCompressedSizeProperty?
    FAttributes: TFileAttributesProperty;
    FModificationTime: TFileModificationDateTimeProperty;

    procedure AssignProperties;

  protected
    function GetAttributes: Cardinal; virtual;
    procedure SetAttributes(NewAttributes: Cardinal); virtual;
    function GetSize: Int64; virtual;
    procedure SetSize(NewSize: Int64); virtual;
    function GetCompressedSize: Int64; virtual;
    procedure SetCompressedSize(NewCompressedSize: Int64); virtual;
    function GetModificationTime: TDateTime; virtual;
    procedure SetModificationTime(NewTime: TDateTime); virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone: TArchiveFile; override;
    procedure CloneTo(AFile: TFile); override;

    class function GetSupportedProperties: TFilePropertiesTypes; override;

    property Size: Int64 read GetSize write SetSize;
    property CompressedSize: Int64 read GetCompressedSize write SetCompressedSize;
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property ModificationTime: TDateTime read GetModificationTime write SetModificationTime;
  end;

implementation

constructor TArchiveFile.Create;
begin
  FSize := TFileSizeProperty.Create;
  FCompressedSize := TFileSizeProperty.Create;
  FAttributes := TNtfsFileAttributesProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

destructor TArchiveFile.Destroy;
begin
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);
  if Assigned(FSize) then
    FreeAndNil(FSize);
  if Assigned(FCompressedSize) then
    FreeAndNil(FCompressedSize);
  if Assigned(FModificationTime) then
    FreeAndNil(FModificationTime);

  inherited;
end;

function TArchiveFile.Clone: TArchiveFile;
begin
  Result := TArchiveFile.Create;
  CloneTo(Result);
end;

procedure TArchiveFile.CloneTo(AFile: TFile);
begin
  if Assigned(AFile) then
  begin
    inherited CloneTo(AFile);
    // All properties are cloned in base class.
  end;
end;

procedure TArchiveFile.AssignProperties;
begin
  FProperties[fpSize] := FSize;
  FProperties[fpCompressedSize] := FCompressedSize;
  FProperties[fpAttributes] := FAttributes;
  FProperties[fpModificationTime] := FModificationTime;
end;

class function TArchiveFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := [{fpName, }fpSize, fpCompressedSize, fpAttributes, fpModificationTime];
end;

function TArchiveFile.GetAttributes: Cardinal;
begin
  Result := FAttributes.Value;
end;

procedure TArchiveFile.SetAttributes(NewAttributes: Cardinal);
begin
  FAttributes.Value := NewAttributes;
end;

function TArchiveFile.GetSize: Int64;
begin
  Result := FSize.Value;
end;

procedure TArchiveFile.SetSize(NewSize: Int64);
begin
  FSize.Value := NewSize;
end;

function TArchiveFile.GetCompressedSize: Int64;
begin
  Result := FCompressedSize.Value;
end;

procedure TArchiveFile.SetCompressedSize(NewCompressedSize: Int64);
begin
  FCompressedSize.Value := NewCompressedSize;
end;

function TArchiveFile.GetModificationTime: TDateTime;
begin
  Result := FModificationTime.Value;
end;

procedure TArchiveFile.SetModificationTime(NewTime: TDateTime);
begin
  FModificationTime.Value := NewTime;
end;

end.

