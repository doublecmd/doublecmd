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
    constructor Create;
    destructor Destroy; override;

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

