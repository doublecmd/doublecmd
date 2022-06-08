unit uTrashDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uGioDeleteOperation,
  uFileSource,
  uFile;

type

  { TTrashDeleteOperation }

  TTrashDeleteOperation = class(TGioDeleteOperation)
  public
    procedure Initialize; override;
    procedure Finalize; override;
  end;

implementation

procedure TTrashDeleteOperation.Initialize;
var
  I: Integer;
  aFile: TFile;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FFullFilesTreeToDelete:= FilesToDelete;

  for I := 0 to FFullFilesTreeToDelete.Count - 1 do
  begin
    aFile := FFullFilesTreeToDelete[I];

    with FStatistics do
    begin
      if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
        Inc(TotalFiles)
      else begin
        Inc(TotalFiles);
        Inc(TotalBytes, aFile.Size);
      end;
    end;
  end;
end;

procedure TTrashDeleteOperation.Finalize;
begin
  FFullFilesTreeToDelete:= nil;
  inherited Finalize;
end;

end.

