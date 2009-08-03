unit uFileSourceCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile;

type

  TFileSourceCreateDirectoryOperation = class(TFileSourceOperation)

  private
    FFileSource: TFileSource;
    FDirectoryPath: String;
    FAbsolutePath: String;
    FRelativePath: String;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure UpdateStatisticsAtStartTime; override;

    property DirectoryPath: String read FDirectoryPath;
    property AbsolutePath: String read FAbsolutePath;
    property RelativePath: String read FRelativePath;

  public
    {en
       @param(aTargetFileSource
              File source where the directory should be created.
              Class takes ownership of the pointer.)
       @param(aDirectoryPath
              Absolute or relative (to TargetFileSource.CurrentPath) path
              to a directory that should be created.
    }
    constructor Create(var aTargetFileSource: TFileSource;
                       aDirectoryPath: String); virtual reintroduce;

    destructor Destroy; override;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceCreateDirectoryOperation.Create(
                var aTargetFileSource: TFileSource;
                aDirectoryPath: String);
begin
  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FDirectoryPath := aDirectoryPath;

  if FFileSource.GetPathType(FDirectoryPath) = ptAbsolute then
  begin
    FAbsolutePath := FDirectoryPath;
    FRelativePath := ExtractDirLevel(FFileSource.CurrentPath, FDirectoryPath);
  end
  else
  begin
    FAbsolutePath := FFileSource.CurrentPath + FDirectoryPath;
    FRelativePath := FDirectoryPath;
  end;
end;

destructor TFileSourceCreateDirectoryOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FFileSource) then
    FreeAndNil(FFileSource);
end;

procedure TFileSourceCreateDirectoryOperation.UpdateStatisticsAtStartTime;
begin
  // empty
end;

function TFileSourceCreateDirectoryOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoCreateDirectory;
end;

end.

