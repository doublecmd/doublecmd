unit uVfsFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uFileProperty;

type

  TVfsFile = class(TFile)
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
    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone: TVfsFile; override;
    procedure CloneTo(AFile: TFile); override;

    class function GetSupportedProperties: TFilePropertiesTypes; override;

    property Size: Int64 read GetSize write SetSize;
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property ModificationTime: TDateTime read GetModificationTime write SetModificationTime;
  end;

implementation

constructor TVfsFile.Create;
begin
  FSize := TFileSizeProperty.Create;
  FAttributes := TNtfsFileAttributesProperty.Create;
  FModificationTime := TFileModificationDateTimeProperty.Create;

  AssignProperties;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

destructor TVfsFile.Destroy;
begin
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);
  if Assigned(FSize) then
    FreeAndNil(FSize);
  if Assigned(FModificationTime) then
    FreeAndNil(FModificationTime);

  inherited;
end;

function TVfsFile.Clone: TVfsFile;
begin
  Result := TVfsFile.Create;
  CloneTo(Result);
end;

procedure TVfsFile.CloneTo(AFile: TFile);
begin
  if Assigned(AFile) then
  begin
    inherited CloneTo(AFile);
    // All properties are cloned in base class.
  end;
end;

procedure TVfsFile.AssignProperties;
begin
  FProperties[fpSize] := FSize;
  FProperties[fpAttributes] := FAttributes;
  FProperties[fpModificationTime] := FModificationTime;
end;

class function TVfsFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := [{fpName, }fpSize, fpAttributes, fpModificationTime];
end;

function TVfsFile.GetAttributes: Cardinal;
begin
  Result := FAttributes.Value;
end;

procedure TVfsFile.SetAttributes(NewAttributes: Cardinal);
begin
  FAttributes.Value := NewAttributes;
end;

function TVfsFile.GetSize: Int64;
begin
  Result := FSize.Value;
end;

procedure TVfsFile.SetSize(NewSize: Int64);
begin
  FSize.Value := NewSize;
end;

function TVfsFile.GetModificationTime: TDateTime;
begin
  Result := FModificationTime.Value;
end;

procedure TVfsFile.SetModificationTime(NewTime: TDateTime);
begin
  FModificationTime.Value := NewTime;
end;

end.

