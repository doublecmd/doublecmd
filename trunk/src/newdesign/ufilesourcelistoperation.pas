unit uFileSourceListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFile;

type

  TFileSourceListOperation = class(TFileSourceOperation)

  protected
    FFiles: TFiles;

    function GetFiles: TFiles;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Retrieves files and revokes ownership of TFiles list.
    // The result of this function should be freed by the caller.
    function ReleaseFiles: TFiles;

    property Files: TFiles read GetFiles;

  end;

implementation

constructor TFileSourceListOperation.Create;
begin
  FFiles := TFiles.Create;
end;

destructor TFileSourceListOperation.Destroy;
begin
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
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

