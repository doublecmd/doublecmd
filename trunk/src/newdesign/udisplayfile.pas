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

  { TDisplayFile }

  TDisplayFile = class

  private
    FFSFile: TFile;          //<en reference to file source's file

    // Other properties.
    FSelected: Boolean;      //<en If is selected
    FIconID: PtrInt;         //<en Icon ID for PixmapManager
    FIconOverlayID: PtrInt;  //<en Overlay icon ID for PixmapManager
    {en
       Used to indicate that the file has been recently updated.
       Value goes from 100 to 0. 0 - not recently updated, 100 - just updated.
    }
    FRecentlyUpdatedPct: Integer;

    // Cache of displayed strings.
    FDisplayStrings: TStringList;

  public
    {en
       @param(ReferenceFile
              Reference file source file that will be associated with this display file.
              The TDisplayFile takes ownership and will destroy the FS file.)
    }
    constructor Create(ReferenceFile: TFile); virtual reintroduce;

    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
       @param(NewReferenceFile
              FS file to assign as the reference file (possibly a clone too).)
    }
    function Clone(NewReferenceFile: TFile): TDisplayFile;
    {en
       Creates an identical copy of the object (as far as object data is concerned).
       @param(CloneFSFile
              If @false then the reference FS file must be later manually assigned.
              Also, if @false DisplayStrings are not cloned.)
    }
    function Clone(CloneFSFile: Boolean): TDisplayFile; virtual;

    procedure CloneTo(AFile: TDisplayFile); virtual;

    property FSFile: TFile read FFSFile write FFSFile;
    property Selected: Boolean read FSelected write FSelected;
    property IconID: PtrInt read FIconID write FIconID;
    property IconOverlayID: PtrInt read FIconOverlayID write FIconOverlayID;
    property DisplayStrings: TStringList read FDisplayStrings;
    property RecentlyUpdatedPct: Integer read FRecentlyUpdatedPct write FRecentlyUpdatedPct;

  end;

  TDisplayFiles = class

  private
    FList: TFPList;
    FOwnsObjects: Boolean;

  protected
    function GetCount: Integer;
    procedure SetCount(Count: Integer);

    function Get(Index: Integer): TDisplayFile;
    procedure Put(Index: Integer; AFile: TDisplayFile);

  public
    constructor Create(AOwnsObjects: Boolean = True); virtual;
    destructor Destroy; override;

    {en
       Create a list with cloned files.
       @param(CloneFSFiles
              If @true automatically clones all FS reference files too.
              If @false does not clone reference files and some properties
              that are affected by reference FS files.)
    }
    function Clone(CloneFSFiles: Boolean): TDisplayFiles; virtual;
    procedure CloneTo(Files: TDisplayFiles; CloneFSFiles: Boolean); virtual;

    function Add(AFile: TDisplayFile): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Find(AFile: TDisplayFile): Integer;
    procedure Remove(AFile: TDisplayFile);

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TDisplayFile read Get write Put; default;
    property List: TFPList read FList;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;

  end;

implementation

constructor TDisplayFile.Create(ReferenceFile: TFile);
begin
  FIconID := -1;
  FIconOverlayID := -1;
  FFSFile := ReferenceFile;
  FDisplayStrings := TStringList.Create;
end;

destructor TDisplayFile.Destroy;
begin
  inherited Destroy;
  FDisplayStrings.Free;
  FSFile.Free;
end;

function TDisplayFile.Clone(NewReferenceFile: TFile): TDisplayFile;
begin
  Result := TDisplayFile.Create(NewReferenceFile);
  CloneTo(Result);
end;

function TDisplayFile.Clone(CloneFSFile: Boolean): TDisplayFile;
var
  AFile: TFile;
begin
  if CloneFSFile then
    AFile := FSFile.Clone
  else
    AFile := nil;
  Result := TDisplayFile.Create(AFile);
  CloneTo(Result);
end;

procedure TDisplayFile.CloneTo(AFile: TDisplayFile);
begin
  if Assigned(AFile) then
  begin
    AFile.FSelected := FSelected;
    AFile.FIconID := FIconID;
    AFile.FIconOverlayID := FIconOverlayID;

    if Assigned(AFile.FFSFile) then
    begin
      AFile.FDisplayStrings.AddStrings(FDisplayStrings);
    end;
  end;
end;

// ----------------------------------------------------------------------------

constructor TDisplayFiles.Create(AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  FList := TFPList.Create;
end;

destructor TDisplayFiles.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TDisplayFiles.Clone(CloneFSFiles: Boolean): TDisplayFiles;
begin
  Result := TDisplayFiles.Create(FOwnsObjects);
  CloneTo(Result, CloneFSFiles);
end;

procedure TDisplayFiles.CloneTo(Files: TDisplayFiles; CloneFSFiles: Boolean);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Files.Add(Get(i).Clone(CloneFSFiles));
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
  if FOwnsObjects then
  begin
    for i := 0 to FList.Count - 1 do
    begin
      p := FList.Items[i];
      TDisplayFile(p).Free;
    end;
  end;

  FList.Clear;
end;

procedure TDisplayFiles.Delete(Index: Integer);
begin
  if FOwnsObjects then
    TDisplayFile(FList.Items[Index]).Free;
  FList.Delete(Index);
end;

function TDisplayFiles.Find(AFile: TDisplayFile): Integer;
begin
  Result := FList.IndexOf(AFile);
end;

procedure TDisplayFiles.Remove(AFile: TDisplayFile);
var
  i: Integer;
begin
  i := Find(AFile);
  if i >= 0 then
    Delete(i);
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

