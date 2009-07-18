unit uColumnsFileViewFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile;

type

  {en
     Describes the displayed file (for example when viewed in columns view).
  }
  TColumnsViewFile = class

  private
    FFile: TFile;  // reference to file source's file

    // Other properties.
    FSelected: Boolean;      //<en If is selected
    FIconID: Integer;        //<en Icon ID for PixmapManager

  public
    {en
       A reference TFile must be passed as a parameter.
       TColumnsViewFile object is invalid without a reference file.

       The reference file is not a copy but a pointer
       (should it be a copy?).
    }
    constructor Create(ReferenceFile: TFile); virtual reintroduce;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone(ReferenceFiles: TFiles;
                   ClonedReferenceFiles: TFiles): TColumnsViewFile; virtual;

    procedure CloneTo(AFile: TColumnsViewFile); virtual;

    property TheFile: TFile read FFile write FFile;
    property Selected: Boolean read FSelected write FSelected;
    property IconID: Integer read FIconID write FIconID;

  end;

  TColumnsViewFiles = class//(TFiles)

  private
    FList: TFPList;

  protected
    function GetCount: Integer;
    procedure SetCount(Count: Integer);

    function Get(Index: Integer): TColumnsViewFile;
    procedure Put(Index: Integer; AFile: TColumnsViewFile);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    {en
       Create a list with cloned files.
       @param(ReferenceFiles
              A list to which reference file of each columns view file item is pointing.)
       @param(ClonedReferenceFiles
              A cloned list of reference files to which each cloned columns view file item should point.)
    }
    function Clone(ReferenceFiles: TFiles;
                   ClonedReferenceFiles: TFiles): TColumnsViewFiles; virtual;

    procedure CloneTo(Files: TColumnsViewFiles;
                      ReferenceFiles: TFiles;
                      ClonedReferenceFiles: TFiles); virtual;

    function Add(AFile: TColumnsViewFile): Integer;
    procedure Clear;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TColumnsViewFile read Get write Put; default;
    property List: TFPList read FList;

  end;

implementation

constructor TColumnsViewFile.Create(ReferenceFile: TFile);
begin
  if not Assigned(ReferenceFile) then
    raise Exception.Create('Reference file cannot be nil');

  FSelected := False;
  FIconID := 0;
  TheFile := ReferenceFile;
end;

function TColumnsViewFile.Clone(ReferenceFiles: TFiles;
                                ClonedReferenceFiles: TFiles): TColumnsViewFile;
var
  ClonedFile: TFile = nil;
  i: Integer;
begin
  // Search reference list for own reference file and clone this columns view file
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

  try
    Result := TColumnsViewFile.Create(ClonedFile);
    CloneTo(Result);
  except
    FreeAndNil(ClonedFile);
  end;
end;

procedure TColumnsViewFile.CloneTo(AFile: TColumnsViewFile);
begin
  if Assigned(AFile) then
  begin
    AFile.FSelected := FSelected;
    AFile.FIconID := FIconID;
  end;
end;

// ----------------------------------------------------------------------------

constructor TColumnsViewFiles.Create;
begin
  inherited;
  FList := TFPList.Create;
end;

destructor TColumnsViewFiles.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TColumnsViewFiles.Clone(ReferenceFiles: TFiles;
                                 ClonedReferenceFiles: TFiles): TColumnsViewFiles;
begin
  Result := TColumnsViewFiles.Create;
  CloneTo(Result, ReferenceFiles, ClonedReferenceFiles);
end;

procedure TColumnsViewFiles.CloneTo(Files: TColumnsViewFiles;
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

function TColumnsViewFiles.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TColumnsViewFiles.SetCount(Count: Integer);
begin
  FList.Count := Count;
end;

function TColumnsViewFiles.Add(AFile: TColumnsViewFile): Integer;
begin
  Result := FList.Add(AFile);
end;

procedure TColumnsViewFiles.Clear;
var
  i: Integer;
  p: Pointer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    p := FList.Items[i];
    if Assigned(p) then
      TColumnsViewFile(p).Free;
  end;

  FList.Clear;
end;

function TColumnsViewFiles.Get(Index: Integer): TColumnsViewFile;
begin
  Result := TColumnsViewFile(FList.Items[Index]);
end;

procedure TColumnsViewFiles.Put(Index: Integer; AFile: TColumnsViewFile);
begin
  FList.Items[Index] := AFile;
end;

end.

