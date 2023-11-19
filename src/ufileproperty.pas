unit uFileProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

const
  FOLDER_SIZE_UNKN =  0;
  FOLDER_SIZE_ZERO = -1;
  FOLDER_SIZE_WAIT = -2;
  FOLDER_SIZE_CALC = -3;

type

  TFilePropertyType = (
    fpName = 0,
    fpSize = 1,             // = fpUncompressedSize
    fpCompressedSize = 2,
    fpOwner = 3,
    fpAttributes = 4,
    fpModificationTime = 5,
    fpCreationTime = 6,
    fpLastAccessTime = 7,
    fpChangeTime = 8,
    fpLink = 9,
    fpType = 10,
    fpComment = 11,
    fpInvalid = 12,
    fpVariant = 128,
    fpMaximum = 255
  );

const
  fpAll = [Low(TFilePropertyType) .. fpInvalid];
  fpVariantAll = [fpVariant .. High(TFilePropertyType)];

type

  TFilePropertiesTypes = set of TFilePropertyType;

  TFilePropertiesDescriptions = array of String;//TFileProperty;

  EInvalidFileProperty = class(Exception);

  // Forward declarations.
  IFilePropertyFormatter = interface;

  { TFileProperty }

  TFileProperty = class

  private

  public
    constructor Create; virtual;

    function Clone: TFileProperty; virtual;
    procedure CloneTo({%H-}FileProperty: TFileProperty); virtual;

    // Text description of the property.
    // Don't know if it will be really needed.
    class function GetDescription: String; virtual abstract;

    class function GetID: TFilePropertyType; virtual abstract;

    function AsString: String; virtual;

    // Formats the property value as a string using some formatter object.
    function Format(Formatter: IFilePropertyFormatter): String; virtual abstract;
  end;


  TFileVariantProperties = array of TFileProperty;
  TFileProperties = array [Low(TFilePropertyType)..fpInvalid] of TFileProperty//class(TList)
  {
    A list of TFileProperty. It would allow to query properties by index and name
    and by TFilePropertyType.

    Implement Clone if made into a class.
  }
  //end
  ;

  // -- Concrete properties ---------------------------------------------------

  TFileNameProperty = class(TFileProperty)
  private
    FName: String;   // only name, no path

    procedure SetName(NewName: String);

  public
    constructor Create; override;
    constructor Create(Name: String); virtual; overload;

    function Clone: TFileNameProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;

    property Value: String read FName write SetName;
  end;

  TFileSizeProperty = class(TFileProperty)

  private
    FSize: Int64;
    FIsValid: Boolean;

  public
    constructor Create; override;
    constructor Create(Size: Int64); virtual; overload;

    function Clone: TFileSizeProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    // Retrieve possible values for the property.
    function GetMinimumValue: Int64;
    function GetMaximumValue: Int64;

    function Format(Formatter: IFilePropertyFormatter): String; override;

    property IsValid: Boolean read FIsValid write FIsValid;
    property Value: Int64 read FSize write FSize;
  end;

  { TFileCompressedSizeProperty }

  TFileCompressedSizeProperty = class(TFileSizeProperty)
  public
    function Clone: TFileCompressedSizeProperty; override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;
  end;

  { TFileDateTimeProperty }

  TFileDateTimeProperty = class(TFileProperty)

  private
    FIsValid: Boolean;
    FDateTime: TDateTime;

  public
    constructor Create; override;
    constructor Create(FileTime: TFileTime); virtual; overload;
    constructor Create(DateTime: TDateTime); virtual; overload;

    procedure CloneTo(FileProperty: TFileProperty); override;

    function Equals(p: TObject): Boolean; override;

    // Retrieve possible values for the property.
    function GetMinimumValue: TDateTime;
    function GetMaximumValue: TDateTime;

    property IsValid: Boolean read FIsValid write FIsValid;
    property Value: TDateTime read FDateTime write FDateTime;
  end;

  TFileModificationDateTimeProperty = class(TFileDateTimeProperty)
  public
    function Clone: TFileModificationDateTimeProperty; override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;
  end;

  TFileCreationDateTimeProperty = class(TFileDateTimeProperty)
  public
    function Clone: TFileCreationDateTimeProperty; override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;
  end;

  TFileLastAccessDateTimeProperty = class(TFileDateTimeProperty)
  public
    function Clone: TFileLastAccessDateTimeProperty; override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Format(Formatter: IFilePropertyFormatter): String; override;
  end;

  { TFileChangeDateTimeProperty }

  TFileChangeDateTimeProperty = class(TFileDateTimeProperty)
  public
    function Clone: TFileChangeDateTimeProperty; override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

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

    class function CreateOSAttributes: TFileAttributesProperty; overload;
    class function CreateOSAttributes(Attr: TFileAttrs): TFileAttributesProperty; overload;

    function Clone: TFileAttributesProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    function Equals(p: TObject): Boolean; override;

    class function GetID: TFilePropertyType; override;

    function IsNativeAttributes: Boolean;

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

  { TFileLinkProperty }

  TFileLinkProperty = class(TFileProperty)

  private
    FIsLinkToDirectory: Boolean;
    FIsValid: Boolean;
    FLinkTo: String;

  public
    constructor Create; override;

    function Clone: TFileLinkProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    function Format({%H-}Formatter: IFilePropertyFormatter): String; override;

    property IsLinkToDirectory: Boolean read FIsLinkToDirectory write FIsLinkToDirectory;
    property IsValid: Boolean read FIsValid write FIsValid;
    property LinkTo: String read FLinkTo write FLinkTo;
  end;

  { TFileOwnerProperty }

  {en
     Owner of the file.
  }
  TFileOwnerProperty = class(TFileProperty)

  private
    FOwner: Cardinal;
    FGroup: Cardinal;
    FOwnerStr: String;
    FGroupStr: String;

  public
    constructor Create; override;

    function Clone: TFileOwnerProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    function Format({%H-}Formatter: IFilePropertyFormatter): String; override;

    property Owner: Cardinal read FOwner write FOwner;
    property Group: Cardinal read FGroup write FGroup;
    property OwnerStr: String read FOwnerStr write FOwnerStr;
    property GroupStr: String read FGroupStr write FGroupStr;

  end;

  { TFileTypeProperty }

  {en
     File type description.
  }
  TFileTypeProperty = class(TFileProperty)

  private
    FType: String;

  public
    constructor Create; override;

    function Clone: TFileTypeProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    function Format({%H-}Formatter: IFilePropertyFormatter): String; override;

    property Value: String read FType write FType;

  end;

  { TFileCommentProperty }

  TFileCommentProperty = class(TFileProperty)

  private
    FComment: String;

  public
    constructor Create; override;

    function Clone: TFileCommentProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    function Format({%H-}Formatter: IFilePropertyFormatter): String; override;

    property Value: String read FComment write FComment;

  end;

  { TFileVariantProperty }

  TFileVariantProperty = class(TFileProperty)

  private
    FName: String;
    FValue: Variant;

  public
    constructor Create; override;
    constructor Create(const AName: String); virtual; overload;

    function Clone: TFileVariantProperty; override;
    procedure CloneTo(FileProperty: TFileProperty); override;

    class function GetDescription: String; override;
    class function GetID: TFilePropertyType; override;

    function Equals(p: TObject): Boolean; override;

    function Format({%H-}Formatter: IFilePropertyFormatter): String; override;

    property Value: Variant read FValue write FValue;
    property Name: String read FName;
  end;

  // -- Property formatter interface ------------------------------------------

  IFilePropertyFormatter = interface(IInterface)
    ['{18EF8E34-1010-45CD-8DC9-678C7C2DC89F}']

    function FormatFileName(FileProperty: TFileNameProperty): String;
    function FormatFileSize(FileProperty: TFileSizeProperty): String;
    function FormatDateTime(FileProperty: TFileDateTimeProperty): String;
    function FormatModificationDateTime(FileProperty: TFileModificationDateTimeProperty): String;
    function FormatNtfsAttributes(FileProperty: TNtfsFileAttributesProperty): String;
    function FormatUnixAttributes(FileProperty: TUnixFileAttributesProperty): String;

  end;

implementation

uses
  Variants, uLng, DCOSUtils, DCFileAttributes, DCDateTimeUtils,
  uDefaultFilePropertyFormatter, uDebug;

resourcestring
  rsSizeDescription = 'Size';
  rsCompressedSizeDescription = 'Compressed size';
  rsDateTimeDescription = 'DateTime';
  rsModificationDateTimeDescription = 'Modification date/time';

// ----------------------------------------------------------------------------

constructor TFileProperty.Create;
begin
  inherited;
end;

function TFileProperty.Clone: TFileProperty;
begin
  Result:= nil;
  raise Exception.Create('Cannot create abstract class');
end;

procedure TFileProperty.CloneTo(FileProperty: TFileProperty);
begin
end;

function TFileProperty.AsString: String;
begin
  Result := Format(DefaultFilePropertyFormatter);
end;

// ----------------------------------------------------------------------------

constructor TFileNameProperty.Create;
begin
  Self.Create('');
end;

constructor TFileNameProperty.Create(Name: String);
begin
  inherited Create;
  Value := Name;
end;

function TFileNameProperty.Clone: TFileNameProperty;
begin
  Result := TFileNameProperty.Create;
  CloneTo(Result);
end;

procedure TFileNameProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileNameProperty do
    begin
      FName := Self.FName;
    end;
  end;
end;

class function TFileNameProperty.GetDescription: String;
begin
  Result := 'name';
end;

class function TFileNameProperty.GetID: TFilePropertyType;
begin
  Result := fpName;
end;

function TFileNameProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileNameProperty) then exit;
  if self.FName <> TFileNameProperty(p).FName then exit;
  Result:= true;
end;

function TFileNameProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatFileName(Self);
end;

procedure TFileNameProperty.SetName(NewName: String);
var
  i: Integer;
begin
  for i := 1 to Length(NewName) do
    if NewName[i] in AllowDirectorySeparators then
    begin
      DCDebug('Name cannot have directory separators: "%s"', [NewName]);
      Break;
    end;

  FName := NewName;
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
  FIsValid := True;
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
      FIsValid := Self.FIsValid;
    end;
  end;
end;

class function TFileSizeProperty.GetDescription: String;
begin
  Result := rsSizeDescription;
end;

class function TFileSizeProperty.GetID: TFilePropertyType;
begin
  Result := fpSize;
end;

function TFileSizeProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileSizeProperty) then exit;
  if self.FIsValid <> TFileSizeProperty(p).FIsValid then exit;
  if self.FIsValid and (self.FSize <> TFileSizeProperty(p).FSize) then exit;
  Result:= true;
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

function TFileCompressedSizeProperty.Clone: TFileCompressedSizeProperty;
begin
  Result := TFileCompressedSizeProperty.Create;
  CloneTo(Result);
end;

class function TFileCompressedSizeProperty.GetDescription: String;
begin
  Result:= rsCompressedSizeDescription;
end;

class function TFileCompressedSizeProperty.GetID: TFilePropertyType;
begin
  Result := fpCompressedSize;
end;

// ----------------------------------------------------------------------------

constructor TFileDateTimeProperty.Create;
begin
  Self.Create(SysUtils.Now);
end;

constructor TFileDateTimeProperty.Create(FileTime: TFileTime);
begin
  Self.Create(FileTimeToDateTime(FileTime));
end;

constructor TFileDateTimeProperty.Create(DateTime: TDateTime);
begin
  inherited Create;
  Value := DateTime;
  FIsValid := True;
end;

procedure TFileDateTimeProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileDateTimeProperty do
    begin
      FIsValid := Self.FIsValid;
      FDateTime := Self.FDateTime;
    end;
  end;
end;

function TFileDateTimeProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileDateTimeProperty) then exit;
  if self.FIsValid <> TFileDateTimeProperty(p).FIsValid then exit;
  if self.FIsValid and (self.FDateTime <> TFileDateTimeProperty(p).FDateTime) then exit;
  Result:= true;
end;

function TFileDateTimeProperty.GetMinimumValue: TDateTime;
begin
  Result := MinDateTime;
end;

function TFileDateTimeProperty.GetMaximumValue: TDateTime;
begin
  Result := MaxDateTime;
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

class function TFileModificationDateTimeProperty.GetID: TFilePropertyType;
begin
  Result := fpModificationTime;
end;

function TFileModificationDateTimeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatModificationDateTime(Self);
end;

// ----------------------------------------------------------------------------

function TFileCreationDateTimeProperty.Clone: TFileCreationDateTimeProperty;
begin
  Result := TFileCreationDateTimeProperty.Create;
  CloneTo(Result);
end;

class function TFileCreationDateTimeProperty.GetDescription: String;
begin
  Result := rsDateTimeDescription;
end;

class function TFileCreationDateTimeProperty.GetID: TFilePropertyType;
begin
  Result := fpCreationTime;
end;

function TFileCreationDateTimeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  if not FIsValid then
    Result := EmptyStr
  else
    Result := Formatter.FormatDateTime(Self);
end;

// ----------------------------------------------------------------------------

function TFileLastAccessDateTimeProperty.Clone: TFileLastAccessDateTimeProperty;
begin
  Result := TFileLastAccessDateTimeProperty.Create;
  CloneTo(Result);
end;

class function TFileLastAccessDateTimeProperty.GetDescription: String;
begin
  Result := rsDateTimeDescription;
end;

class function TFileLastAccessDateTimeProperty.GetID: TFilePropertyType;
begin
  Result := fpLastAccessTime;
end;

function TFileLastAccessDateTimeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatDateTime(Self);
end;

// ----------------------------------------------------------------------------

function TFileChangeDateTimeProperty.Clone: TFileChangeDateTimeProperty;
begin
  Result := TFileChangeDateTimeProperty.Create;
  CloneTo(Result);
end;

class function TFileChangeDateTimeProperty.GetDescription: String;
begin
  Result := rsDateTimeDescription;
end;

class function TFileChangeDateTimeProperty.GetID: TFilePropertyType;
begin
  Result := fpChangeTime;
end;

function TFileChangeDateTimeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  if not FIsValid then
    Result := EmptyStr
  else
    Result := Formatter.FormatDateTime(Self);
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

function TFileAttributesProperty.IsNativeAttributes: Boolean;
begin
{$IF DEFINED(WINDOWS)}
  Result :=  Self is TNtfsFileAttributesProperty;
{$ELSEIF DEFINED(UNIX)}
  Result := Self is TUnixFileAttributesProperty;
{$ELSE}
  Result := False;
{$ENDIF}
end;

class function TFileAttributesProperty.CreateOSAttributes: TFileAttributesProperty;
begin
  Result := CreateOSAttributes(0);
end;

class function TFileAttributesProperty.CreateOSAttributes(Attr: TFileAttrs): TFileAttributesProperty;
begin
{$IF DEFINED(WINDOWS)}
  Result := TNtfsFileAttributesProperty.Create(Attr);
{$ELSEIF DEFINED(UNIX)}
  Result := TUnixFileAttributesProperty.Create(Attr);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TFileAttributesProperty.Clone: TFileAttributesProperty;
begin
  Result:= nil;
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

class function TFileAttributesProperty.GetID: TFilePropertyType;
begin
  Result := fpAttributes;
end;

function TFileAttributesProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileAttributesProperty) then exit;
  if self.FAttributes <> TFileAttributesProperty(p).FAttributes then exit;
  Result:= true;
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
  Result := ((FAttributes and FILE_ATTRIBUTE_SYSTEM) <> 0) or
            ((FAttributes and FILE_ATTRIBUTE_HIDDEN) <> 0);
end;

function TNtfsFileAttributesProperty.IsLink: Boolean;
begin
  Result:= ((FAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
end;

function TNtfsFileAttributesProperty.IsReadOnly: Boolean;
begin
  Result := (FAttributes and FILE_ATTRIBUTE_READONLY) <> 0;
end;

function TNtfsFileAttributesProperty.IsHidden: Boolean;
begin
  Result := (FAttributes and FILE_ATTRIBUTE_HIDDEN) <> 0;
end;

class function TNtfsFileAttributesProperty.GetDescription: String;
begin
  Result:= EmptyStr;
end;

function TNtfsFileAttributesProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatNtfsAttributes(Self)
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
  Result:= False;
end;

function TUnixFileAttributesProperty.IsOwnerWrite: Boolean;
begin
  Result:= False;
end;

function TUnixFileAttributesProperty.IsOwnerExecute: Boolean;
begin
  Result:= False;
end;

class function TUnixFileAttributesProperty.GetDescription: String;
begin
  Result:= EmptyStr;
end;

function TUnixFileAttributesProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := Formatter.FormatUnixAttributes(Self);
end;

// ----------------------------------------------------------------------------

constructor TFileLinkProperty.Create;
begin
  inherited Create;
  FIsLinkToDirectory := False;
  FIsValid := True;
end;

function TFileLinkProperty.Clone: TFileLinkProperty;
begin
  Result := TFileLinkProperty.Create;
  CloneTo(Result);
end;

procedure TFileLinkProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileLinkProperty do
    begin
      FIsLinkToDirectory := Self.FIsLinkToDirectory;
      FIsValid := Self.FIsValid;
      FLinkTo := Self.FLinkTo;
    end;
  end;
end;

class function TFileLinkProperty.GetDescription: String;
begin
  Result := '';
end;

class function TFileLinkProperty.GetID: TFilePropertyType;
begin
  Result := fpLink;
end;

function TFileLinkProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileLinkProperty) then exit;
  if self.FIsValid <> TFileLinkProperty(p).FIsValid then exit;
  if self.FIsValid then
  begin
    if self.FIsLinkToDirectory <> TFileLinkProperty(p).FIsLinkToDirectory then exit;
    if self.FLinkTo <> TFileLinkProperty(p).FLinkTo then exit;
  end;
  Result:= true;
end;

function TFileLinkProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := '';
end;

// ----------------------------------------------------------------------------

constructor TFileOwnerProperty.Create;
begin
  inherited Create;
end;

function TFileOwnerProperty.Clone: TFileOwnerProperty;
begin
  Result := TFileOwnerProperty.Create;
  CloneTo(Result);
end;

procedure TFileOwnerProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileOwnerProperty do
    begin
      FOwner := Self.FOwner;
      FGroup := Self.FGroup;
      FOwnerStr := Self.FOwnerStr;
      FGroupStr := Self.FGroupStr;
    end;
  end;
end;

class function TFileOwnerProperty.GetDescription: String;
begin
  Result := '';
end;

class function TFileOwnerProperty.GetID: TFilePropertyType;
begin
  Result := fpOwner;
end;

function TFileOwnerProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileOwnerProperty) then exit;
  if self.FOwner <> TFileOwnerProperty(p).FOwner then exit;
  if self.FGroup <> TFileOwnerProperty(p).FGroup then exit;
  Result:= true;
end;

function TFileOwnerProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := '';
end;

{ TFileTypeProperty }

constructor TFileTypeProperty.Create;
begin
  inherited Create;
end;

function TFileTypeProperty.Clone: TFileTypeProperty;
begin
  Result := TFileTypeProperty.Create;
  CloneTo(Result);
end;

procedure TFileTypeProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileTypeProperty do
    begin
      FType := Self.FType;
    end;
  end;
end;

class function TFileTypeProperty.GetDescription: String;
begin
  Result := '';
end;

class function TFileTypeProperty.GetID: TFilePropertyType;
begin
  Result := fpType;
end;

function TFileTypeProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileTypeProperty) then exit;
  if self.FType <> TFileTypeProperty(p).FType then exit;
  Result:= true;
end;

function TFileTypeProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result := FType;
end;

{ TFileCommentProperty }

constructor TFileCommentProperty.Create;
begin
  inherited Create;
end;

function TFileCommentProperty.Clone: TFileCommentProperty;
begin
  Result := TFileCommentProperty.Create;
  CloneTo(Result);
end;

procedure TFileCommentProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileCommentProperty do
    begin
      FComment := Self.FComment;
    end;
  end;
end;

class function TFileCommentProperty.GetDescription: String;
begin
  Result:= '';
end;

class function TFileCommentProperty.GetID: TFilePropertyType;
begin
  Result := fpComment;
end;

function TFileCommentProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileCommentProperty) then exit;
  if self.FComment <> TFileCommentProperty(p).FComment then exit;
  Result:= true;
end;

function TFileCommentProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  Result:= FComment;
end;

{ TFileVariantProperty }

constructor TFileVariantProperty.Create;
begin
  inherited Create;
  FValue:= Unassigned;
end;

constructor TFileVariantProperty.Create(const AName: String);
begin
  Create;
  FName:= AName;
end;

function TFileVariantProperty.Clone: TFileVariantProperty;
begin
  Result := TFileVariantProperty.Create;
  CloneTo(Result);
end;

procedure TFileVariantProperty.CloneTo(FileProperty: TFileProperty);
begin
  if Assigned(FileProperty) then
  begin
    inherited CloneTo(FileProperty);

    with FileProperty as TFileVariantProperty do
    begin
      FName := Self.FName;
      FValue := Self.FValue;
    end;
  end;
end;

class function TFileVariantProperty.GetDescription: String;
begin
  Result:= EmptyStr;
end;

class function TFileVariantProperty.GetID: TFilePropertyType;
begin
  Result:= fpVariant;
end;

function TFileVariantProperty.Equals(p: TObject): Boolean;
begin
  Result:= false;
  if not (p is TFileVariantProperty) then exit;
  if self.FName <> TFileVariantProperty(p).FName then exit;
  if self.FValue <> TFileVariantProperty(p).FValue then exit;
  Result:= true;
end;

function TFileVariantProperty.Format(Formatter: IFilePropertyFormatter): String;
begin
  if not VarIsBool(FValue) then
    Result := FValue
  else
    if FValue then
      result := rsSimpleWordTrue
    else
      result := rsSimpleWordFalse;
end;

end.

