unit uFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileProperty,
  DCBasicTypes;

type

  { TFile }

  TFile = class

  private
    // Cached values for extension and name.
    // Automatically set when name changes.
    FExtension: String;     //<en Extension.
    FNameNoExt: String;     //<en Name without extension.
    FPath: String;          //<en Path to the file. Always includes trailing path delimiter.
    FProperties: TFileProperties;
    FVariantProperties: TFileVariantProperties;
    FSupportedProperties: TFilePropertiesTypes;

    procedure SplitIntoNameAndExtension(const FileName: string;
                                        var aFileNameOnly: string;
                                        var aExtension: string);
    procedure UpdateNameAndExtension(const FileName: string);

  protected
    function GetProperty(PropType: TFilePropertyType): TFileProperty;
    procedure SetProperty(PropType: TFilePropertyType; NewValue: TFileProperty);
    function GetFullPath: String;
    procedure SetFullPath(const NewFullPath: String);
    procedure SetPath(const NewPath: String);
    function GetName: String;
    procedure SetName(NewName: String);
    function GetExtension: String;
    {en
       Retrieves name without extension.
    }
    function GetNameNoExt: String;

    // Values.
    function GetAttributes: TFileAttrs;
    procedure SetAttributes(NewAttributes: TFileAttrs);
    function GetSize: Int64;
    procedure SetSize(NewSize: Int64);
    function GetCompressedSize: Int64;
    procedure SetCompressedSize(NewCompressedSize: Int64);
    function GetModificationTime: TDateTime;
    procedure SetModificationTime(NewTime: TDateTime);
    function GetCreationTime: TDateTime;
    procedure SetCreationTime(NewTime: TDateTime);
    function GetLastAccessTime: TDateTime;
    procedure SetLastAccessTime(NewTime: TDateTime);
    function GetChangeTime: TDateTime;
    procedure SetChangeTime(AValue: TDateTime);
    function GetIsLinkToDirectory: Boolean;
    procedure SetIsLinkToDirectory(NewValue: Boolean);
    function GetType: String;
    procedure SetType(NewValue: String);

    // Properties.
    function GetNameProperty: TFileNameProperty;
    procedure SetNameProperty(NewValue: TFileNameProperty);
    function GetSizeProperty: TFileSizeProperty;
    procedure SetSizeProperty(NewValue: TFileSizeProperty);
    function GetCompressedSizeProperty: TFileCompressedSizeProperty;
    procedure SetCompressedSizeProperty(NewValue: TFileCompressedSizeProperty);
    function GetAttributesProperty: TFileAttributesProperty;
    procedure SetAttributesProperty(NewValue: TFileAttributesProperty);
    function GetModificationTimeProperty: TFileModificationDateTimeProperty;
    procedure SetModificationTimeProperty(NewValue: TFileModificationDateTimeProperty);
    function GetCreationTimeProperty: TFileCreationDateTimeProperty;
    procedure SetCreationTimeProperty(NewValue: TFileCreationDateTimeProperty);
    function GetLastAccessTimeProperty: TFileLastAccessDateTimeProperty;
    procedure SetLastAccessTimeProperty(NewValue: TFileLastAccessDateTimeProperty);
    function GetChangeTimeProperty: TFileChangeDateTimeProperty;
    procedure SetChangeTimeProperty(AValue: TFileChangeDateTimeProperty);
    function GetLinkProperty: TFileLinkProperty;
    procedure SetLinkProperty(NewValue: TFileLinkProperty);
    function GetOwnerProperty: TFileOwnerProperty;
    procedure SetOwnerProperty(NewValue: TFileOwnerProperty);
    function GetTypeProperty: TFileTypeProperty;
    procedure SetTypeProperty(NewValue: TFileTypeProperty);
    function GetCommentProperty: TFileCommentProperty;
    procedure SetCommentProperty(NewValue: TFileCommentProperty);
    {$IFDEF DARWIN}
    function GetFinderTagProperty: TFileFinderTagProperty;
    procedure SetFinderTagProperty(NewValue: TFileFinderTagProperty);
    {$ENDIF}
  public
    constructor Create(const APath: String);
    constructor CreateForCloning;
    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone: TFile;
    procedure CloneTo(AFile: TFile);

    function Compare(AFile: TFile): TFilePropertiesTypes;

    {en
       Frees all properties except for Name (which is always required).
    }
    procedure ClearProperties;
    procedure ClearVariantProperties;
    function ReleaseProperty(PropType: TFilePropertyType): TFileProperty;

    {en
       Returns True if name is not '..'.
       May be extended to include other conditions.
    }
    function IsNameValid: Boolean;

    {en
       This list only contains pointers to TFileProperty objects.
       Never free element from this list!

       Choices for implementing retrieval of file properties:

       1. array [TFilePropertyType] of TFileProperty  (current implementation)

          Upside: it should be the fastest method.
          Downside: uses more memory as the array size includes properties
                    not supported by the given file type

       2. hash table indexed by TFilePropertyType key.

          It _may_ be a bit slower than the table.
          It _may_ use less memory though.

       3. a simple list

          Slowest, but the least memory usage.
    }
    //property Properties[Index: Integer];
    //property Properties[Name: String];
    //property Properties[Type: TFilePropertiesType]
    property VariantProperties: TFileVariantProperties read FVariantProperties;
    property Properties[PropType: TFilePropertyType]: TFileProperty read GetProperty write SetProperty;

    {en
       All supported properties should have an assigned Properties[propertyType].
    }
    property SupportedProperties: TFilePropertiesTypes read FSupportedProperties;
    property AssignedProperties: TFilePropertiesTypes read FSupportedProperties;

    { Accessors to each property. }

    property NameProperty: TFileNameProperty read GetNameProperty write SetNameProperty;
    property SizeProperty: TFileSizeProperty read GetSizeProperty write SetSizeProperty;
    property CompressedSizeProperty: TFileCompressedSizeProperty read GetCompressedSizeProperty write SetCompressedSizeProperty;
    property AttributesProperty: TFileAttributesProperty read GetAttributesProperty write SetAttributesProperty;
    property ModificationTimeProperty: TFileModificationDateTimeProperty read GetModificationTimeProperty write SetModificationTimeProperty;
    property CreationTimeProperty: TFileCreationDateTimeProperty read GetCreationTimeProperty write SetCreationTimeProperty;
    property LastAccessTimeProperty: TFileLastAccessDateTimeProperty read GetLastAccessTimeProperty write SetLastAccessTimeProperty;
    property ChangeTimeProperty: TFileChangeDateTimeProperty read GetChangeTimeProperty write SetChangeTimeProperty;
    property LinkProperty: TFileLinkProperty read GetLinkProperty write SetLinkProperty;
    property OwnerProperty: TFileOwnerProperty read GetOwnerProperty write SetOwnerProperty;
    property TypeProperty: TFileTypeProperty read GetTypeProperty write SetTypeProperty;
    property CommentProperty: TFileCommentProperty read GetCommentProperty write SetCommentProperty;
    {$IFDEF DARWIN}
    property FinderTagProperty: TFileFinderTagProperty read GetFinderTagProperty write SetFinderTagProperty;
    {$ENDIF}

    { Accessors to each property's value. }

    {en
       Sets/gets absolute path to file.
       On get returns Path + Name.
       On set sets Path and Name accordingly.
    }
    property FullPath: String read GetFullPath write SetFullPath;
    property Path: String read FPath write SetPath;
    property Name: String read GetName write SetName;
    property NameNoExt: String read GetNameNoExt;
    property Extension: String read GetExtension;
    property Size: Int64 read GetSize write SetSize;
    property CompressedSize: Int64 read GetCompressedSize write SetCompressedSize;
    property Attributes: TFileAttrs read GetAttributes write SetAttributes;
    property ModificationTime: TDateTime read GetModificationTime write SetModificationTime;
    property CreationTime: TDateTime read GetCreationTime write SetCreationTime;
    property LastAccessTime: TDateTime read GetLastAccessTime write SetLastAccessTime;
    property ChangeTime: TDateTime read GetChangeTime write SetChangeTime;
    property FileType: String read GetType write SetType;

    // Convenience functions.
    // We assume here that when the file has no attributes
    // the result is false for all these functions.
    // These functions should probably be moved from here and should not be methods.
    function IsDirectory: Boolean;
    function IsSpecial: Boolean;
    function IsSysFile: Boolean;
    function IsHidden: Boolean;
    function IsLink: Boolean;
    property IsLinkToDirectory: Boolean read GetIsLinkToDirectory write SetIsLinkToDirectory;
    function IsExecutable: Boolean;   // for ShellExecute
  end;

  // --------------------------------------------------------------------------

  { TFiles }

  TFiles = class { A list of TFile }

  private
    FList: TFPList;
    FFlat: Boolean;
    FOwnsObjects: Boolean;
    FPath: String; //<en path of all files

  protected
    function GetCount: Integer;
    procedure SetCount(Count: Integer);

    function Get(Index: Integer): TFile;
    procedure Put(Index: Integer; AFile: TFile);

    procedure SetPath(const NewPath: String);

  public
    constructor Create(const APath: String);
    destructor Destroy; override;

    {en
       Create a list with cloned files.
    }
    function Clone: TFiles;
    procedure CloneTo(Files: TFiles);

    function Add(AFile: TFile): Integer;
    procedure Insert(AFile: TFile; AtIndex: Integer);
    procedure Delete(AtIndex: Integer);
    procedure Clear;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TFile read Get write Put; default;
    property List: TFPList read FList;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Path: String read FPath write SetPath;
    property Flat: Boolean read FFlat write FFlat;

  end;

  {en
     Tree structure representing directories/files hierarchy.
  }
  TFileTreeNode = class
  private
    {en
       File object associated with this node.
    }
    FFile: TFile;
    {en
       Subnodes - usually files within a directory.
       This is a list of TFileTreeNode.
    }
    FSubNodes: TFPList;
    {en
       Additional data stored in the node.
       If assigned, it is automatically freed when node is destroyed.
    }
    FData: TObject;

  protected
    function Get(Index: Integer): TFileTreeNode;

    function GetCount: Integer;
    procedure SetCount(Count: Integer);

    procedure SetData(NewData: TObject);

  public
    constructor Create; overload;
    constructor Create(aFile: TFile); overload;
    constructor Create(aFile: TFile; DataClass: TClass); overload;
    destructor Destroy; override;

    function AddSubNode(aFile: TFile): Integer;
    procedure RemoveSubNode(Index: Integer);

    property SubNodesCount: Integer read GetCount write SetCount;
    property SubNodes[Index: Integer]: TFileTreeNode read Get;
    property TheFile: TFile read FFile;
    property Data: TObject read FData write SetData;
  end;

  TFileTree = TFileTreeNode;  // alias

implementation

{$IFDEF UNIX}
uses
  DCFileAttributes;
{$ENDIF}

constructor TFile.Create(const APath: String);
begin
  inherited Create;

  // Name property always present.
  NameProperty := TFileNameProperty.Create;

  Path := APath;
end;

constructor TFile.CreateForCloning;
begin
  // Create empty object.
  inherited Create;
end;

destructor TFile.Destroy;
var
  AIndex: Integer;
  PropertyType: TFilePropertyType;
begin
  inherited Destroy;

  for PropertyType := Low(FProperties) to High(FProperties) do
    FProperties[PropertyType].Free;

  for AIndex:= Low(FVariantProperties) to High(FVariantProperties) do
    FVariantProperties[AIndex].Free;
end;

function TFile.Clone: TFile;
begin
  Result := TFile.CreateForCloning;
  CloneTo(Result);
end;

procedure TFile.CloneTo(AFile: TFile);
var
  AIndex: Integer;
  PropertyType: TFilePropertyType;
begin
  if Assigned(AFile) then
  begin
    AFile.FExtension := FExtension;
    AFile.FNameNoExt := FNameNoExt;
    AFile.FPath      := FPath;
    AFile.FSupportedProperties := FSupportedProperties;

    for PropertyType := Low(FProperties) to High(FProperties) do
    begin
      if Assigned(Self.FProperties[PropertyType]) then
      begin
        AFile.FProperties[PropertyType].Free;
        AFile.FProperties[PropertyType] := Self.FProperties[PropertyType].Clone;
      end;
    end;

    SetLength(AFile.FVariantProperties, Length(FVariantProperties));
    for AIndex:= Low(FVariantProperties) to High(FVariantProperties) do
    begin
      if Assigned(Self.FVariantProperties[AIndex]) then
      begin
        AFile.FVariantProperties[AIndex].Free;
        AFile.FVariantProperties[AIndex] := Self.FVariantProperties[AIndex].Clone;
      end;
    end;
  end;
end;

function TFile.Compare(AFile: TFile): TFilePropertiesTypes;
var
  AIndex: Integer;
  PropertyType: TFilePropertyType;
begin
  Result := [];

  if self.FPath <> AFile.FPath then
  begin
    Include(Result, TFilePropertyType.fpName);
    exit;
  end;

  for PropertyType := Low(FProperties) to High(FProperties) do
  begin
    if Assigned(self.FProperties[PropertyType]) then begin
      if not self.FProperties[PropertyType].equals(AFile.FProperties[PropertyType])
        then Include(Result, PropertyType);
    end else begin
      if Assigned(AFile.FProperties[PropertyType])
        then Include(Result, PropertyType);
    end;
  end;

  if Length(self.FVariantProperties) <> Length(AFile.FVariantProperties) then
  begin
    Include(Result, TFilePropertyType.fpVariant);
    exit;
  end;

  for AIndex := Low(FVariantProperties) to High(FVariantProperties) do
  begin
    if Assigned(Self.FVariantProperties[AIndex]) then begin
      if not self.FVariantProperties[AIndex].equals(AFile.FVariantProperties[AIndex]) then begin
        Include(Result, TFilePropertyType.fpVariant);
        exit;
      end;
    end else begin
      if Assigned(AFile.FVariantProperties[AIndex]) then begin
        Include(Result, TFilePropertyType.fpVariant);
        exit;
      end;
    end;
  end;
end;

procedure TFile.ClearProperties;
var
  PropertyType: TFilePropertyType;
begin
  ClearVariantProperties;
  for PropertyType := TFilePropertyType(Ord(fpName) + 1) to High(FProperties) do
    FreeAndNil(FProperties[PropertyType]);
  FSupportedProperties := [fpName];
end;

procedure TFile.ClearVariantProperties;
var
  AIndex: Integer;
begin
  for AIndex:= Low(FVariantProperties) to High(FVariantProperties) do
    FreeAndNil(FVariantProperties[AIndex]);
  FSupportedProperties := FSupportedProperties * fpAll;
end;

function TFile.ReleaseProperty(PropType: TFilePropertyType): TFileProperty;
var
  AIndex: Integer;
begin
  if PropType in fpVariantAll then
  begin
    AIndex := Ord(PropType) - Ord(fpVariant);
    if (AIndex >= 0) and (AIndex <= High(FVariantProperties)) then
    begin
      Result := FVariantProperties[AIndex];
      FVariantProperties[AIndex] := nil;
    end;
  end
  else begin
    Result := FProperties[PropType];
    FProperties[PropType] := nil;
  end;
  Exclude(FSupportedProperties, PropType);
end;

function TFile.GetExtension: String;
begin
  Result := FExtension;
end;

function TFile.GetNameNoExt: String;
begin
  Result := FNameNoExt;
end;

function TFile.GetName: String;
begin
  Result := TFileNameProperty(FProperties[fpName]).Value;
end;

procedure TFile.SetName(NewName: String);
begin
  TFileNameProperty(FProperties[fpName]).Value := NewName;
  UpdateNameAndExtension(NewName);
end;

function TFile.GetProperty(PropType: TFilePropertyType): TFileProperty;
var
  AIndex: Integer;
begin
  if PropType < fpInvalid then
    Result := FProperties[PropType]
  else begin
    AIndex := Ord(PropType) - Ord(fpVariant);
    if (AIndex >= 0) and (AIndex <= High(FVariantProperties)) then
      Result := FVariantProperties[AIndex]
    else begin
      Result := nil;
    end;
  end;
end;

procedure TFile.SetProperty(PropType: TFilePropertyType; NewValue: TFileProperty);
var
  AIndex: Integer;
begin
  if PropType < fpInvalid then
  begin
    FProperties[PropType].Free;
    FProperties[PropType] := NewValue
  end
  else begin
    AIndex := Ord(PropType) - Ord(fpVariant);
    if AIndex > High(FVariantProperties) then
      SetLength(FVariantProperties, AIndex + 4)
    else begin
      FVariantProperties[AIndex].Free;
    end;
    FVariantProperties[AIndex]:= NewValue;
  end;
  if Assigned(NewValue) then
    Include(FSupportedProperties, PropType)
  else
    Exclude(FSupportedProperties, PropType);
end;

function TFile.GetFullPath: String;
begin
  Result := Path + TFileNameProperty(FProperties[fpName]).Value;
end;

procedure TFile.SetFullPath(const NewFullPath: String);
var
  aExtractedName: String;
begin
  if NewFullPath <> '' then
  begin
    if NewFullPath[Length(NewFullPath)] = PathDelim then
    begin
      // Only path passed.
      SetPath(NewFullPath);
      SetName('');
    end
    else
    begin
      aExtractedName := ExtractFileName(NewFullPath);
      SetPath(Copy(NewFullPath, 1, Length(NewFullPath) - Length(aExtractedName)));
      SetName(aExtractedName);
    end;
  end;
end;

procedure TFile.SetPath(const NewPath: String);
begin
  if NewPath = '' then
    FPath := ''
  else
    FPath := IncludeTrailingPathDelimiter(NewPath);
end;

function TFile.GetAttributes: TFileAttrs;
begin
  Result := TFileAttributesProperty(FProperties[fpAttributes]).Value;
end;

procedure TFile.SetAttributes(NewAttributes: TFileAttrs);
begin
  TFileAttributesProperty(FProperties[fpAttributes]).Value := NewAttributes;
  UpdateNameAndExtension(Name);
end;

function TFile.GetSize: Int64;
begin
  Result := TFileSizeProperty(FProperties[fpSize]).Value;
end;

procedure TFile.SetSize(NewSize: Int64);
begin
  TFileSizeProperty(FProperties[fpSize]).Value := NewSize;
end;

function TFile.GetCompressedSize: Int64;
begin
  Result := TFileCompressedSizeProperty(FProperties[fpCompressedSize]).Value;
end;

procedure TFile.SetCompressedSize(NewCompressedSize: Int64);
begin
  TFileCompressedSizeProperty(FProperties[fpCompressedSize]).Value := NewCompressedSize;
end;

function TFile.GetModificationTime: TDateTime;
begin
  Result := TFileModificationDateTimeProperty(FProperties[fpModificationTime]).Value;
end;

procedure TFile.SetModificationTime(NewTime: TDateTime);
begin
  TFileModificationDateTimeProperty(FProperties[fpModificationTime]).Value := NewTime;
end;

function TFile.GetCreationTime: TDateTime;
begin
  Result := TFileCreationDateTimeProperty(FProperties[fpCreationTime]).Value;
end;

procedure TFile.SetCreationTime(NewTime: TDateTime);
begin
  TFileCreationDateTimeProperty(FProperties[fpCreationTime]).Value := NewTime;
end;

function TFile.GetLastAccessTime: TDateTime;
begin
  Result := TFileLastAccessDateTimeProperty(FProperties[fpLastAccessTime]).Value;
end;

procedure TFile.SetLastAccessTime(NewTime: TDateTime);
begin
  TFileLastAccessDateTimeProperty(FProperties[fpLastAccessTime]).Value := NewTime;
end;

function TFile.GetIsLinkToDirectory: Boolean;
begin
  if fpLink in SupportedProperties then
    Result := TFileLinkProperty(FProperties[fpLink]).IsLinkToDirectory
  else
    Result := False;
end;

procedure TFile.SetIsLinkToDirectory(NewValue: Boolean);
begin
  TFileLinkProperty(FProperties[fpLink]).IsLinkToDirectory := NewValue;
end;

function TFile.GetType: String;
begin
  Result := TFileTypeProperty(FProperties[fpType]).Value;
end;

procedure TFile.SetType(NewValue: String);
begin
  TFileTypeProperty(FProperties[fpType]).Value := NewValue;
end;

function TFile.GetNameProperty: TFileNameProperty;
begin
  Result := TFileNameProperty(FProperties[fpName]);
end;

procedure TFile.SetNameProperty(NewValue: TFileNameProperty);
begin
  Properties[fpName] := NewValue;
end;

function TFile.GetAttributesProperty: TFileAttributesProperty;
begin
  Result := TFileAttributesProperty(FProperties[fpAttributes]);
end;

procedure TFile.SetAttributesProperty(NewValue: TFileAttributesProperty);
begin
  Properties[fpAttributes] := NewValue;
  if Assigned(NewValue) then
  begin
    UpdateNameAndExtension(Name);
  end;
end;

function TFile.GetSizeProperty: TFileSizeProperty;
begin
  Result := TFileSizeProperty(FProperties[fpSize]);
end;

procedure TFile.SetSizeProperty(NewValue: TFileSizeProperty);
begin
  Properties[fpSize] := NewValue;
end;

function TFile.GetCompressedSizeProperty: TFileCompressedSizeProperty;
begin
  Result := TFileCompressedSizeProperty(FProperties[fpCompressedSize]);
end;

procedure TFile.SetCompressedSizeProperty(NewValue: TFileCompressedSizeProperty);
begin
  Properties[fpCompressedSize] := NewValue;
end;

function TFile.GetModificationTimeProperty: TFileModificationDateTimeProperty;
begin
  Result := TFileModificationDateTimeProperty(FProperties[fpModificationTime]);
end;

procedure TFile.SetModificationTimeProperty(NewValue: TFileModificationDateTimeProperty);
begin
  Properties[fpModificationTime] := NewValue;
end;

function TFile.GetCreationTimeProperty: TFileCreationDateTimeProperty;
begin
  Result := TFileCreationDateTimeProperty(FProperties[fpCreationTime]);
end;

procedure TFile.SetCreationTimeProperty(NewValue: TFileCreationDateTimeProperty);
begin
  Properties[fpCreationTime] := NewValue;
end;

function TFile.GetLastAccessTimeProperty: TFileLastAccessDateTimeProperty;
begin
  Result := TFileLastAccessDateTimeProperty(FProperties[fpLastAccessTime]);
end;

procedure TFile.SetLastAccessTimeProperty(NewValue: TFileLastAccessDateTimeProperty);
begin
  Properties[fpLastAccessTime] := NewValue;
end;

function TFile.GetChangeTime: TDateTime;
begin
  Result := TFileChangeDateTimeProperty(FProperties[fpChangeTime]).Value;
end;

procedure TFile.SetChangeTime(AValue: TDateTime);
begin
  TFileChangeDateTimeProperty(FProperties[fpChangeTime]).Value := AValue;
end;

function TFile.GetChangeTimeProperty: TFileChangeDateTimeProperty;
begin
  Result := TFileChangeDateTimeProperty(FProperties[fpChangeTime]);
end;

procedure TFile.SetChangeTimeProperty(AValue: TFileChangeDateTimeProperty);
begin
  Properties[fpChangeTime] := AValue;
end;

function TFile.GetLinkProperty: TFileLinkProperty;
begin
  Result := TFileLinkProperty(FProperties[fpLink]);
end;

procedure TFile.SetLinkProperty(NewValue: TFileLinkProperty);
begin
  Properties[fpLink] := NewValue;
end;

function TFile.GetOwnerProperty: TFileOwnerProperty;
begin
  Result := TFileOwnerProperty(FProperties[fpOwner]);
end;

procedure TFile.SetOwnerProperty(NewValue: TFileOwnerProperty);
begin
  Properties[fpOwner] := NewValue;
end;

function TFile.GetTypeProperty: TFileTypeProperty;
begin
  Result := TFileTypeProperty(FProperties[fpType]);
end;

procedure TFile.SetTypeProperty(NewValue: TFileTypeProperty);
begin
  Properties[fpType] := NewValue;
end;

function TFile.GetCommentProperty: TFileCommentProperty;
begin
  Result := TFileCommentProperty(FProperties[fpComment]);
end;

procedure TFile.SetCommentProperty(NewValue: TFileCommentProperty);
begin
  Properties[fpComment] := NewValue;
end;

{$IFDEF DARWIN}
function TFile.GetFinderTagProperty: TFileFinderTagProperty;
begin
  Result := TFileFinderTagProperty(FProperties[fpMacOSFinderTag]);
end;

procedure TFile.SetFinderTagProperty(NewValue: TFileFinderTagProperty);
begin
  Properties[fpMacOSFinderTag] := NewValue;
end;
{$ENDIF}

function TFile.IsNameValid: Boolean;
begin
  if Name <> '..' then
    Result := True
  else
    Result := False;
end;

function TFile.IsDirectory: Boolean;
begin
  if fpAttributes in SupportedProperties then
    Result := TFileAttributesProperty(FProperties[fpAttributes]).IsDirectory
  else
    Result := False;
end;

function TFile.IsSpecial: Boolean;
begin
  if fpAttributes in SupportedProperties then
    Result := TFileAttributesProperty(FProperties[fpAttributes]).IsSpecial
  else
    Result := False;
end;

function TFile.IsLink: Boolean;
begin
  if fpAttributes in SupportedProperties then
    Result := TFileAttributesProperty(FProperties[fpAttributes]).IsLink
  else
    Result := False;
end;

function TFile.IsExecutable: Boolean;
var
  FileAttributes: TFileAttributesProperty;
begin
  if fpAttributes in SupportedProperties then
  begin
    FileAttributes := TFileAttributesProperty(FProperties[fpAttributes]);
{$IF DEFINED(MSWINDOWS)}
    Result := not FileAttributes.IsDirectory;
{$ELSEIF DEFINED(UNIX)}
    Result := (not FileAttributes.IsDirectory) and
              (FileAttributes.Value AND (S_IXUSR OR S_IXGRP OR S_IXOTH)>0);
{$ELSE}
    Result := False;
{$ENDIF}
  end
  else
    Result := False;
end;

function TFile.IsSysFile: Boolean;
begin
{$IF DEFINED(MSWINDOWS)}
  if fpAttributes in SupportedProperties then
    Result := TFileAttributesProperty(Properties[fpAttributes]).IsSysFile
  else
    Result := False;
{$ELSEIF DEFINED(DARWIN)}
  if (Length(Name) > 1) and (Name[1] = '.') and (Name <> '..') then exit(true);
  if Name='Icon'#$0D then exit(true);
  exit(false);
{$ELSE}
  // Files beginning with '.' are treated as system/hidden files on Unix.
  Result := (Length(Name) > 1) and (Name[1] = '.') and (Name <> '..');
{$ENDIF}
end;

function TFile.IsHidden: Boolean;
begin
  if not (fpAttributes in SupportedProperties) then
    Result := False
  else begin
    if Properties[fpAttributes] is TNtfsFileAttributesProperty then
      Result := TNtfsFileAttributesProperty(Properties[fpAttributes]).IsHidden
    else begin
      // Files beginning with '.' are treated as system/hidden files on Unix.
      Result := (Length(Name) > 1) and (Name[1] = '.') and (Name <> '..');
    end;
  end;
end;

procedure TFile.SplitIntoNameAndExtension(const FileName: string;
                                          var aFileNameOnly: string;
                                          var aExtension: string);
var
  i : longint;
begin
  I := Length(FileName);
  while (I > 0) and (FileName[I] <> ExtensionSeparator) do
    Dec(I);
  if I > 1 then
  begin
    aFileNameOnly := Copy(FileName, 1, I - 1);
    aExtension    := Copy(FileName, I + 1, MaxInt);
  end
  else
  begin
    // For files that does not have '.' or that have only
    // one '.' and beginning with '.' there is no extension.
    aFileNameOnly := FileName;
    aExtension := '';
  end;
end;

procedure TFile.UpdateNameAndExtension(const FileName: string);
begin
  // Cache Extension and NameNoExt.

  if (FileName = '') or IsDirectory or IsLinkToDirectory or IsSpecial
  then
  begin
    // For directories there is no extension.
    FExtension := '';
    FNameNoExt := FileName;
  end
  else
  begin
    SplitIntoNameAndExtension(FileName, FNameNoExt, FExtension);
  end;
end;

// ----------------------------------------------------------------------------

constructor TFiles.Create(const APath: String);
begin
  inherited Create;
  FList := TFPList.Create;
  FOwnsObjects := True;
  Path := APath;
end;

destructor TFiles.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TFiles.Clone: TFiles;
begin
  Result := TFiles.Create(Path);
  CloneTo(Result);
end;

procedure TFiles.CloneTo(Files: TFiles);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Files.Add(Get(i).Clone);
  end;
end;

function TFiles.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TFiles.SetCount(Count: Integer);
begin
  FList.Count := Count;
end;

function TFiles.Add(AFile: TFile): Integer;
begin
  Result := FList.Add(AFile);
end;

procedure TFiles.Insert(AFile: TFile; AtIndex: Integer);
begin
  FList.Insert(AtIndex, AFile);
end;

procedure TFiles.Delete(AtIndex: Integer);
var
  p: Pointer;
begin
  p := FList.Items[AtIndex];
  TFile(p).Free;
  FList.Delete(AtIndex);
end;

procedure TFiles.Clear;
var
  i: Integer;
  p: Pointer;
begin
  if OwnsObjects then
  begin
    for i := 0 to FList.Count - 1 do
    begin
      p := FList.Items[i];
      TFile(p).Free;
    end;
  end;

  FList.Clear;
end;

function TFiles.Get(Index: Integer): TFile;
begin
  Result := TFile(FList.Items[Index]);
end;

procedure TFiles.Put(Index: Integer; AFile: TFile);
begin
  FList.Items[Index] := AFile;
end;

procedure TFiles.SetPath(const NewPath: String);
begin
  if NewPath = '' then
    FPath := ''
  else
    FPath := IncludeTrailingPathDelimiter(NewPath);
end;

// ----------------------------------------------------------------------------

constructor TFileTreeNode.Create;
begin
  Create(nil);
end;

constructor TFileTreeNode.Create(aFile: TFile);
begin
  FSubNodes := nil;
  FFile := aFile;
  FData := nil;
  inherited Create;
end;

constructor TFileTreeNode.Create(aFile: TFile; DataClass: TClass);
begin
  Create(aFile);
  FData := DataClass.Create;
end;

destructor TFileTreeNode.Destroy;
var
  i: Integer;
begin
  inherited Destroy;

  FreeAndNil(FFile);

  if Assigned(FSubNodes) then
  begin
    for i := 0 to FSubNodes.Count - 1 do
      TFileTreeNode(FSubNodes.Items[i]).Free;
    FreeAndNil(FSubNodes);
  end;

  FreeAndNil(FData);
end;

function TFileTreeNode.AddSubNode(aFile: TFile): Integer;
var
  aNode: TFileTreeNode;
begin
  if not Assigned(FSubNodes) then
    FSubNodes := TFPList.Create;
  aNode := TFileTreeNode.Create(aFile);
  Result := FSubNodes.Add(aNode);
end;

procedure TFileTreeNode.RemoveSubNode(Index: Integer);
begin
  if (Index >= 0) and (Index < FSubNodes.Count) then
  begin
    TFileTreeNode(FSubNodes.Items[Index]).Free;
    FSubNodes.Delete(Index);
  end;
end;

function TFileTreeNode.Get(Index: Integer): TFileTreeNode;
begin
  Result := TFileTreeNode(FSubNodes.Items[Index]);
end;

function TFileTreeNode.GetCount: Integer;
begin
  if Assigned(FSubNodes) then
    Result := FSubNodes.Count
  else
    Result := 0;
end;

procedure TFileTreeNode.SetCount(Count: Integer);
begin
  if not Assigned(FSubNodes) then
    FSubNodes := TFPList.Create;
  FSubNodes.Count := Count;
end;

procedure TFileTreeNode.SetData(NewData: TObject);
var
  TmpData: TObject;
begin
  if Assigned(FData) then
  begin
    TmpData := FData;
    FData := NewData;
    TmpData.Free;
  end
  else
    FData := NewData;
end;

end.

