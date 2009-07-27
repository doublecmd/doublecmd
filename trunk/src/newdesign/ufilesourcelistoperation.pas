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

  protected
    FFiles: TFiles;

    function GetFiles: TFiles;

    function GetID: TFileSourceOperationType; override;

  public
    constructor Create(aFileSource: TFileSource); reintroduce;
    destructor Destroy; override;

    // Retrieves files and revokes ownership of TFiles list.
    // The result of this function should be freed by the caller.
    function ReleaseFiles: TFiles;

    property Files: TFiles read GetFiles;

  end;

implementation

constructor TFileSourceListOperation.Create(aFileSource: TFileSource);
begin
  inherited Create(aFileSource, nil);
end;

destructor TFileSourceListOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FFiles) then
    FreeAndNil(FFiles);
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

