unit uFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileProperty;

type

  TFile = class

  private
    FName: String;
    FPath: String;  // Should this always include trailing path delimiter,
                    // or never include it?

    // Cached values for extension and name.
    // Automatically set when name changes.
    FExtension: String;     //<en Extension.
    FNameNoExt: String;     //<en Name without extension.

  protected
    FProperties: TFileProperties;

    function GetProperties: TFileProperties; virtual;

    function GetName: String;
    procedure SetName(Name: String);
    function GetExtension: String;
    {en
       Retrieves name without extension.
    }
    function GetNameNoExt: String;

  public
    constructor Create; virtual;

    class function GetSupportedProperties: TFilePropertiesTypes; virtual abstract;

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
    property Properties: TFileProperties read GetProperties;

    {en
       All supported properties should have an assigned Properties[propertyType].
    }
    property SupportedProperties: TFilePropertiesTypes read GetSupportedProperties;

    property Path: String read FPath write FPath;
    property Name: String read GetName write SetName;
    property NameNoExt: String read GetNameNoExt;
    property Extension: String read GetExtension;

    // Convenience functions.
    // We assume here that when the file has no attributes
    // the result is false for all these functions.
    // These functions should probably be moved from here and should not be methods.
    function IsDirectory: Boolean;
    function IsSysFile: Boolean;
    function IsLink: Boolean;
  end;

  // --------------------------------------------------------------------------

  TFiles = class { A list of TFile }

  private
    FList: TFPList;
    FPath: String; //<en path of all files

  protected
    function GetCount: Integer;
    procedure SetCount(Count: Integer);

    function Get(Index: Integer): TFile;
    procedure Put(Index: Integer; AFile: TFile);

  public
    constructor Create;
    destructor Destroy; override;

    function Add(AFile: TFile): Integer;
    procedure Clear;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TFile read Get write Put; default;
    property List: TFPList read FList;
    property Path: String read FPath write FPath;

  end;

implementation

uses
  uOSUtils;

constructor TFile.Create;
begin
end;

function TFile.GetProperties: TFileProperties;
begin
  Result := FProperties;
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
  Result := FName;
end;

procedure TFile.SetName(Name: String);
begin
  FName := Name;

  // Cache Extension and NameNoExt.

  if ((fpAttributes in SupportedProperties) and IsDirectory) or
     (Name[1] = '.')
  then
  begin
    // For directories and files beginning with '.' there is no extension.
    FExtension := '';
    FNameNoExt := FName;
  end
  else
  begin
    FExtension := ExtractFileExt(FName);
    FNameNoExt := Copy(FName, 1, Length(FName) - Length(FExtension));
  end;
end;

function TFile.IsDirectory: Boolean;
var
  FileAttributes: TFileAttributesProperty;
begin
  if fpAttributes in SupportedProperties then
  begin
    FileAttributes := Properties[fpAttributes] as TFileAttributesProperty;
    Result := FileAttributes.IsDirectory
{$IFDEF MSWINDOWS}
              //Because symbolic link works on Windows 2k/XP for directories only
              or FileAttributes.IsLink
{$ENDIF}
              ;
  end
  else
    Result := False;
end;

function TFile.IsLink: Boolean;
var
  FileAttributes: TFileAttributesProperty;
begin
  if fpAttributes in SupportedProperties then
  begin
    FileAttributes := Properties[fpAttributes] as TFileAttributesProperty;
    Result := FileAttributes.IsLink;
  end
  else
    Result := False;
end;

function TFile.IsSysFile: Boolean;
var
  FileAttributes: TFileAttributesProperty;
begin
{$IFDEF MSWINDOWS}
  if fpAttributes in SupportedProperties then
  begin
    FileAttributes := Properties[fpAttributes] as TFileAttributesProperty;
    Result := FileAttributes.IsSysFile;
  end
  else
    Result := False;
{$ELSE}
  // Files beginning with '.' are treated as system/hidden files on Unix.
  Result := (Name <> '') and
            (Name <> '..') and
            (Name[1] = '.');
{$ENDIF}
end;

// ----------------------------------------------------------------------------

constructor TFiles.Create;
begin
  inherited;
  FList := TFPList.Create;
end;

destructor TFiles.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
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

procedure TFiles.Clear;
var
  i: Integer;
  p: Pointer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    p := FList.Items[i];
    if Assigned(p) then
      TFile(p).Free;
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

end.

