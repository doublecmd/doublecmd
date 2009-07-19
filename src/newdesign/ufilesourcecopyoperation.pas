unit uFileSourceCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes;

type

  // Statistics are the same for CopyIn and CopyOut operations.
  TFileSourceCopyOperationStatistics = record
    CurrentFileFrom: String;
    CurrentFileTo: String;
    CurrentFileTotalBytes: Int64;
    CurrentFileDoneBytes: Int64;
    TotalFiles: Int64;
    DoneFiles: Int64;
    TotalBytes: Int64;
    DoneBytes: Int64;
  end;

  {en
     Base class for CopyIn and CopyOut operations.
  }
  TFileSourceCopyOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceCopyOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.

  protected
    procedure UpdateStatistics(NewStatistics: TFileSourceCopyOperationStatistics);

  public
    constructor Create; override;
    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceCopyOperationStatistics;
  end;

  {en
     Operation that copies files from another file source into a file source of specific type
     (to file system for TFileSystemCopyInOperation,
      to network for TNetworkCopyInOperation, etc.).

     Source file source must be a file system file source.
     (Or is it enough if it's a file source with directly accessible files ? (DirectAccess flag))
     Target file source should match the class type.

     Example meaning of this operation:
     - archive: pack
     - network: upload
  }
  TFileSourceCopyInOperation = class(TFileSourceCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

  {en
     Operation that copies files into another file source from a file source of specific type
     (from file system for TFileSystemCopyOutOperation,
      from network for TNetworkCopyOutOperation, etc.).

     Source file source should match the class type.
     Target file source must be a file system file source.
     (Or is it enough if it's a file source with directly accessible files ? (DirectAccess flag))

     Example meaning of this operation:
     - archive: unpack
     - network: download
  }
  TFileSourceCopyOutOperation = class(TFileSourceCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

implementation

// -- TFileSourceCopyOperation ------------------------------------------------

constructor TFileSourceCopyOperation.Create;
begin
  with FStatistics do
  begin
    CurrentFileFrom := '';
    CurrentFileTo := '';

    TotalFiles := 0;
    DoneFiles := 0;
    TotalBytes := 0;
    DoneBytes := 0;
    CurrentFileTotalBytes := 0;
    CurrentFileDoneBytes := 0;
  end;

  FStatisticsLock := TCriticalSection.Create;

  inherited Create;
end;

destructor TFileSourceCopyOperation.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FStatisticsLock);
end;

procedure TFileSourceCopyOperation.UpdateStatistics(NewStatistics: TFileSourceCopyOperationStatistics);
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatistics := NewStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceCopyOperation.RetrieveStatistics: TFileSourceCopyOperationStatistics;
begin
  FStatisticsLock.Acquire;
  try
    Result := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

// -- TFileSourceCopyInOperation ----------------------------------------------

function TFileSourceCopyInOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoCopyIn;
end;

// -- TFileSourceCopyOutOperation ---------------------------------------------

function TFileSourceCopyOutOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoCopyOut;
end;

end.

