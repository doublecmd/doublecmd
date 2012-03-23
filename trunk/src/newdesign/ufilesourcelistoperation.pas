unit uFileSourceListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFile,
  uFileSource;

type

  { TFileSourceListOperation }

  TFileSourceListOperation = class(TFileSourceOperation)

  private
    FFileSource: IFileSource;
    FPath: String;

  protected
    FFiles: TFiles;

    function GetFiles: TFiles;
    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;

  public
    constructor Create(aFileSource: IFileSource; aPath: String); virtual reintroduce;
    destructor Destroy; override;

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;

    // Retrieves files and revokes ownership of TFiles list.
    // The result of this function should be freed by the caller.
    function ReleaseFiles: TFiles;

    property Files: TFiles read GetFiles;
    property Path: String read FPath;

  end;

implementation

uses
  uLng;

constructor TFileSourceListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFileSource := aFileSource;
  FPath := aPath;
  inherited Create(FFileSource);
end;

destructor TFileSourceListOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FFiles) then
    FreeAndNil(FFiles);
end;

function TFileSourceListOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  Result := rsOperListing;
end;

function TFileSourceListOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoList;
end;

function TFileSourceListOperation.GetFiles: TFiles;
begin
  Result := FFiles;
end;

function TFileSourceListOperation.ReleaseFiles: TFiles;
begin
  Result := FFiles;
  FFiles := nil; // revoke ownership
end;

procedure TFileSourceListOperation.UpdateStatisticsAtStartTime;
begin
  // Empty.
end;

end.

