unit uFileSourceCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource;

type

  TFileSourceCreateDirectoryOperation = class(TFileSourceOperation)

  private
    FFileSource: IFileSource;
    FBasePath: String;
    FDirectoryPath: String;
    FAbsolutePath: String;
    FRelativePath: String;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure UpdateStatisticsAtStartTime; override;

    property BasePath: String read FBasePath;
    property DirectoryPath: String read FDirectoryPath;
    property AbsolutePath: String read FAbsolutePath;
    property RelativePath: String read FRelativePath;

  public
    {en
       @param(aTargetFileSource
              File source where the directory should be created.)
       @param(aCurrentPath
              Absolute path to current directory where the new directory
              should be created (if its path is not absolute).)
       @param(aDirectoryPath
              Absolute or relative (to TargetFileSource.CurrentPath) path
              to a directory that should be created.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       aCurrentPath: String;
                       aDirectoryPath: String); virtual reintroduce;

    destructor Destroy; override;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceCreateDirectoryOperation.Create(
                aTargetFileSource: IFileSource;
                aCurrentPath: String;
                aDirectoryPath: String);
begin
  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  FBasePath := aCurrentPath;
  FDirectoryPath := aDirectoryPath;

  if FFileSource.GetPathType(FDirectoryPath) = ptAbsolute then
  begin
    FAbsolutePath := FDirectoryPath;
    FRelativePath := ExtractDirLevel(aCurrentPath, FDirectoryPath);
  end
  else
  begin
    FAbsolutePath := aCurrentPath + FDirectoryPath;
    FRelativePath := FDirectoryPath;
  end;
end;

destructor TFileSourceCreateDirectoryOperation.Destroy;
begin
  inherited Destroy;
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

