unit uFileProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uOSUtils;

type

  // Forward declarations.
  IFilePropertyFormatter = interface;

  { TFileProperty }

  TFileProperty = class

  private

  public
    constructor Create; virtual;

    function Clone: TFileProperty; virtual;
    procedure CloneTo(FileProperty: TFileProperty); virtual;

    // Text description of the property.
    // Don't know if it will be really needed.
    class function GetDescription: String; virtual abstract;

    // Formats the property value as a string using some formatter object.
    function Format(Formatter: IFilePropertyFormatter): String; virtual abstract;
  end;


  TFilePropertyType = (
    //fpName,
    //fpPath,
    fpSize,           // = fpUncompressedSize?
    fpCompressedSize,
    fpAttributes,
    fpDateTime,         // non-specific - should be used?
                        // maybe it should be a default time
    fpModificationTime,
    fpCreationTime,
    fpLastAccessTime  // Last write?
  );

  TFilePropertiesTypes = set of TFilePropertyType;

  TFilePropertiesDescriptions = array of String;//TFileProperty;

  TFileProperties = array [TFilePropertyType] of TFileProperty//class(TList)
  {
    A list of TFileProperty. It would allow to query properties by index and name
    and by TFilePropertyType.

    Implement Clone if made into a class.
  }
  //end
  ;

  // -- Concrete properties ---------------------------------------------------

  TFileSizeProperty = class(TFileProperty)

  private
    FSize: Int64; // Cardinal;

  public
    constructor Create; override;
    constructor Create(Size: Int64); virtual; overload;

    function Clone: TFileSizeProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;

    // Retrieve possible values for the property.
    function GetMinimumValue: Int64; //Cardinal;
    function GetMaximumValue: Int64; //Cardinal;

    function Format(Formatter: IFilePropertyFormatter): String; override;

    property Value: Int64 read FSize write FSize;
  end;

  TFileDateTimeProperty = class(TFileProperty)

  private
    FDateTime: TDateTime;

  public
    constructor Create; override;
    constructor Create(DateTime: TDateTime); virtual; overload;

    function Clone: TFileDateTimeProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;

    // Retrieve possible values for the property.
    function GetMinimumValue: TDateTime;
    function GetMaximumValue: TDateTime;

    function Format(Formatter: IFilePropertyFormatter): String; override;

    property Value: TDateTime read FDateTime write FDateTime;
  end;

  TFileModificationDateTimeProperty = class(TFileDateTimeProperty)
  public
    function Clone: TFileModificationDateTimeProperty; override;

    class function GetDescription: String; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;
  end;

  {en
     File system attributes.
  }
  TFileAttributesProperty = class(TFileProperty)

  private
    FAttributes: TFileAttrs;

  public
    constructor Create; override;

    constructor Create(Attr: TFileAttrs); virtual; overload;

    function Clone: TFileAttributesProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    // Is the file a directory.
    function IsDirectory: Boolean; virtual;

    // Is this a system file.
    function IsSysFile: boolean; virtual abstract;

    // Is it a symbolic link.
    function IsLink: Boolean; virtual;

    // Retrieves raw attributes.
    function GetAttributes: TFileAttrs; virtual;

    // Sets raw attributes.
    procedure SetAttributes(Attributes: TFileAttrs); virtual;

    property Value: TFileAttrs read GetAttributes write SetAttributes;

  end;

  { TNtfsFileAttributesProperty }

  TNtfsFileAttributesProperty = class(TFileAttributesProperty)
  public
    function Clone: TNtfsFileAttributesProperty; override;

    // Is the file a directory.
    function IsDirectory: Boolean; override;

    // Is this a system file.
    function IsSysFile: boolean; override;

    // Is it a symbolic link.
    function IsLink: Boolean; override;

    function IsReadOnly: Boolean;
    function IsHidden: Boolean;

    class function GetDescription: String; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;
  end;

  { TUnixFileAttributesProperty }

  TUnixFileAttributesProperty = class(TFileAttributesProperty)
  public
    function Clone: TUnixFileAttributesProperty; override;

    // Is the file a directory.
    function IsDirectory: Boolean; override;

    // Is this a system file.
    function IsSysFile: boolean; override;

    // Is it a symbolic link.
    function IsLink: Boolean; override;

    function IsOwnerRead: Boolean;
    function IsOwnerWrite: Boolean;
    function IsOwnerExecute: Boolean;
    // ...

    class function GetDescription: String; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;
  end;

  // -- Property formatter interface ------------------------------------------

  IFilePropertyFormatter = interface(IInterface)
    ['{18EF8E34-1010-45CD-8DC9-678C7C2DC89F}']

    function FormatFileSize(FileProperty: TFileSizeProperty): String;
    function FormatDateTime(FileProperty: TFileDateTimeProperty): String;
    function FormatModificationDateTime(FileProperty: TFileModificationDateTimeProperty): String;
    function FormatAttributes(FileProperty: TFileAttributesProperty): String;

  end;

implementation

uses
  uFileAttributes;

resourcestring
  rsSizeDescription = 'Size';
  rsDateTimeDescription = 'DateTime';
  rsModificationDateTimeDescription = 'Modification date/time';

// ----------------------------------------------------------------------------

constructor TFileProperty.Create;
begin
  inherited;
end;

function TFileProperty.Clone: TFileProperty;
begin
  raise Exception.Create('Cannot create abstract class');
end;

procedure TFileProperty.CloneTo(FileProperty: TFileProperty);
begin
end;

// ----------------------------------------------------------------------------

constructor TFileSizeProperty.Create;
begin
  Self.Create(0);
end;

constructor TFileSizeProperty.Create(Size: Int64);
begin
  inherited Create;
  Value := Size;
end;

function TFileSizeProperty.Clone: TFileSizeProperty;
begin
  Result := TFileSizeProperty.Create;
  CloneTo(Result);
end;

procedure TFileSizeProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileSizeProperty do
    begin
      FSize := Self.FSize;
    end;
  end;
end;

class function TFileSizeProperty.GetDescription: String;
begin
  Result := rsSizeDescription;
end;

function TFileSizeProperty.GetMinimumValue: Int64;
begin
  Result := 0;
end;

function TFileSizeProperty.GetMaximumValue: Int64;
begin
  Result := 0; // maximum file size
end;

function TFileSizeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatFileSize(Self);
end;

// ----------------------------------------------------------------------------

constructor TFileDateTimeProperty.Create;
begin
  Self.Create(0);
end;

constructor TFileDateTimeProperty.Create(DateTime: TDateTime);
begin
  inherited Create;
  Value := DateTime;
end;

function TFileDateTimeProperty.Clone: TFileDateTimeProperty;
begin
  Result := TFileDateTimeProperty.Create;
  CloneTo(Result);
end;

procedure TFileDateTimeProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileDateTimeProperty do
    begin
      FDateTime := Self.FDateTime;
    end;
  end;
end;

class function TFileDateTimeProperty.GetDescription: String;
begin
  Result := rsDateTimeDescription;
end;

function TFileDateTimeProperty.GetMinimumValue: TDateTime;
begin
  Result := 0;
end;

function TFileDateTimeProperty.GetMaximumValue: TDateTime;
begin
  Result := 0; // maximum file size
end;

function TFileDateTimeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatDateTime(Self);
end;

// ----------------------------------------------------------------------------

function TFileModificationDateTimeProperty.Clone: TFileModificationDateTimeProperty;
begin
  Result := TFileModificationDateTimeProperty.Create;
  CloneTo(Result);
end;

class function TFileModificationDateTimeProperty.GetDescription: String;
begin
  Result := rsModificationDateTimeDescription;
end;

function TFileModificationDateTimeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatModificationDateTime(Self);
end;

// ----------------------------------------------------------------------------

constructor TFileAttributesProperty.Create;
begin
  Create(0);
end;

constructor TFileAttributesProperty.Create(Attr: TFileAttrs);
begin
  inherited Create;
  FAttributes := Attr;
end;

function TFileAttributesProperty.Clone: TFileAttributesProperty;
begin
  raise Exception.Create('Cannot create abstract class');
end;

procedure TFileAttributesProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileAttributesProperty do
    begin
      FAttributes := Self.FAttributes;
    end;
  end;
end;

function TFileAttributesProperty.GetAttributes: TFileAttrs;
begin
  Result := FAttributes;
end;

procedure TFileAttributesProperty.SetAttributes(Attributes: TFileAttrs);
begin
  FAttributes := Attributes;
end;

function TFileAttributesProperty.IsDirectory: Boolean;
begin
  Result := fpS_ISDIR(FAttributes);
end;

function TFileAttributesProperty.IsLink: Boolean;
begin
  Result := fpS_ISLNK(FAttributes);
end;

// ----------------------------------------------------------------------------

function TNtfsFileAttributesProperty.Clone: TNtfsFileAttributesProperty;
begin
  Result := TNtfsFileAttributesProperty.Create;
  CloneTo(Result);
end;

function TNtfsFileAttributesProperty.IsDirectory: Boolean;
begin
  Result:= ((FAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

function TNtfsFileAttributesProperty.IsSysFile: boolean;
begin
  Result := ((FAttributes and faSysFile) <> 0) or
            ((FAttributes and faHidden) <> 0);
end;

function TNtfsFileAttributesProperty.IsLink: Boolean;
begin
  Result:= ((FAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
end;

function TNtfsFileAttributesProperty.IsReadOnly: Boolean;
begin
  Result := (FAttributes and faReadOnly) <> 0;
end;

function TNtfsFileAttributesProperty.IsHidden: Boolean;
begin
  Result := (FAttributes and faHidden) <> 0;
end;

class function TNtfsFileAttributesProperty.GetDescription: String;
begin
end;

function TNtfsFileAttributesProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatAttributes(Self)
end;

// ----------------------------------------------------------------------------

function TUnixFileAttributesProperty.Clone: TUnixFileAttributesProperty;
begin
  Result := TUnixFileAttributesProperty.Create;
  CloneTo(Result);
end;

function TUnixFileAttributesProperty.IsDirectory: Boolean;
begin
  Result:= ((FAttributes and S_IFMT) = S_IFDIR);
end;

function TUnixFileAttributesProperty.IsSysFile: Boolean;
begin
  Result := False;
end;

function TUnixFileAttributesProperty.IsLink: Boolean;
begin
  Result:= ((FAttributes and S_IFMT) = S_IFLNK);
end;

function TUnixFileAttributesProperty.IsOwnerRead: Boolean;
begin
end;

function TUnixFileAttributesProperty.IsOwnerWrite: Boolean;
begin
end;

function TUnixFileAttributesProperty.IsOwnerExecute: Boolean;
begin
end;

class function TUnixFileAttributesProperty.GetDescription: String;
begin
end;

function TUnixFileAttributesProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatAttributes(Self);
end;

end.

