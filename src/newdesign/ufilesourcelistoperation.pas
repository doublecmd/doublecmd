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

  TFileSourceListOperation = class(TFileSourceOperation)

  private
    FFileSource: TFileSource;

  protected
    FFiles: TFiles;

    function GetFiles: TFiles;
    function GetID: TFileSourceOperationType; override;

    property FileSource: TFileSource read FFileSource;

  public
    constructor Create(var aFileSource: TFileSource); virtual reintroduce;
    destructor Destroy; override;

    // Retrieves files and revokes ownership of TFiles list.
    // The result of this function should be freed by the caller.
    function ReleaseFiles: TFiles;

    property Files: TFiles read GetFiles;

  end;

implementation

constructor TFileSourceListOperation.Create(var aFileSource: TFileSource);
begin
  FFileSource := aFileSource;
  aFileSource := nil;
  inherited Create(FFileSource, nil);
end;

destructor TFileSourceListOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FFiles) then
    FreeAndNil(FFiles);
  if Assigned(FFileSource) then
    FreeAndNil(FFileSource);
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

end.

