unit uDisplayFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile;

type

  {en
     Describes the file displayed in the file view.
  }
  TDisplayFile = class

  private
    FFile: TFile;  // reference to file source's file

    // Other properties.
    FSelected: Boolean;      //<en If is selected
    FIconID: PtrInt;         //<en Icon ID for PixmapManager

    // Cache of displayed strings.
    FDisplayStrings: TStringList;

  public
    {en
       A reference TFile must be passed as a parameter.
       TDisplayFile object is invalid without a reference file.

       The reference file is not a copy but a pointer.
    }
    constructor Create(ReferenceFile: TFile); virtual reintroduce;

    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone(ReferenceFiles: TFiles;
                   ClonedReferenceFiles: TFiles): TDisplayFile; virtual;

    procedure CloneTo(AFile: TDisplayFile); virtual;

    property TheFile: TFile read FFile write FFile;
    property Selected: Boolean read FSelected write FSelected;
    property IconID: PtrInt read FIconID write FIconID;
    property DisplayStrings: TStringList read FDisplayStrings;

  end;

  TDisplayFiles = class

  private
    FList: TFPList;

  protected
    function GetCount: Integer;
    procedure SetCount(Count: Integer);

    function Get(Index: Integer): TDisplayFile;
    procedure Put(Index: Integer; AFile: TDisplayFile);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    {en
       Create a list with cloned files.
       @param(ReferenceFiles
              A list to which the reference file of each display file is pointing.)
       @param(ClonedReferenceFiles
              A cloned list of reference files to which each cloned display file should point.)
    }
    function Clone(ReferenceFiles: TFiles;
                   ClonedReferenceFiles: TFiles): TDisplayFiles; virtual;

    procedure CloneTo(Files: TDisplayFiles;
                      ReferenceFiles: TFiles;
                      ClonedReferenceFiles: TFiles); virtual;

    function Add(AFile: TDisplayFile): Integer;
    procedure Clear;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TDisplayFile read Get write Put; default;
    property List: TFPList read FList;

  end;

implementation

constructor TDisplayFile.Create(ReferenceFile: TFile);
begin
  if not Assigned(ReferenceFile) then
    raise Exception.Create('Reference file cannot be nil');

  FSelected := False;
  FIconID := -1;
  TheFile := ReferenceFile;
  FDisplayStrings := TStringList.Create;
end;

destructor TDisplayFile.Destroy;
begin
  inherited Destroy;
  if Assigned(FDisplayStrings) then
    FreeAndNil(FDisplayStrings);
end;

function TDisplayFile.Clone(ReferenceFiles: TFiles;
                            ClonedReferenceFiles: TFiles): TDisplayFile;
var
  ClonedFile: TFile = nil;
  i: Integer;
begin
  // Search reference list for own reference file and clone this fileview file
  // with a reference file pointing to the cloned reference file.
  for i := 0 to ReferenceFiles.Count - 1 do
  begin
    if TheFile = ReferenceFiles[i] then
    begin
      ClonedFile := ClonedReferenceFiles[i];
      break;
    end;
  end;

  if not Assigned(ClonedFile) then
    raise Exception.Create('Invalid reference file');

  Result := TDisplayFile.Create(ClonedFile);
  CloneTo(Result);
end;

procedure TDisplayFile.CloneTo(AFile: TDisplayFile);
begin
  if Assigned(AFile) then
  begin
    AFile.FSelected := FSelected;
    AFile.FIconID := FIconID;
    AFile.FDisplayStrings.AddStrings(FDisplayStrings);
  end;
end;

// ----------------------------------------------------------------------------

constructor TDisplayFiles.Create;
begin
  inherited;
  FList := TFPList.Create;
end;

destructor TDisplayFiles.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TDisplayFiles.Clone(ReferenceFiles: TFiles;
                             ClonedReferenceFiles: TFiles): TDisplayFiles;
begin
  Result := TDisplayFiles.Create;
  CloneTo(Result, ReferenceFiles, ClonedReferenceFiles);
end;

procedure TDisplayFiles.CloneTo(Files: TDisplayFiles;
                                ReferenceFiles: TFiles;
                                ClonedReferenceFiles: TFiles);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Files.Add(Get(i).Clone(ReferenceFiles, ClonedReferenceFiles));
  end;
end;

function TDisplayFiles.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TDisplayFiles.SetCount(Count: Integer);
begin
  FList.Count := Count;
end;

function TDisplayFiles.Add(AFile: TDisplayFile): Integer;
begin
  Result := FList.Add(AFile);
end;

procedure TDisplayFiles.Clear;
var
  i: Integer;
  p: Pointer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    p := FList.Items[i];
    if Assigned(p) then
      TDisplayFile(p).Free;
  end;

  FList.Clear;
end;

function TDisplayFiles.Get(Index: Integer): TDisplayFile;
begin
  Result := TDisplayFile(FList.Items[Index]);
end;

procedure TDisplayFiles.Put(Index: Integer; AFile: TDisplayFile);
begin
  FList.Items[Index] := AFile;
end;

end.

