unit uFileSourceExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile;

type

  TFileSourceExecuteOperation = class(TFileSourceOperation)

  private
    FFileSource: TFileSource;
    FExecutablePath: String;
    FAbsolutePath: String;
    FRelativePath: String;
    FVerb: String;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure UpdateStatisticsAtStartTime; override;

    property ExecutablePath: String read FExecutablePath;
    property AbsolutePath: String read FAbsolutePath;
    property RelativePath: String read FRelativePath;
    property Verb: String read FVerb;

  public
    {en
       @param(aTargetFileSource
              File source where the directory should be created.
              Class takes ownership of the pointer.)
       @param(aExecutablePath
              Absolute or relative (to TargetFileSource.CurrentPath) path
              to a executable that should be executed.
    }
    constructor Create(var aTargetFileSource: TFileSource;
                       aExecutablePath, aVerb: UTF8String); virtual reintroduce;

    destructor Destroy; override;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceExecuteOperation.Create(
                var aTargetFileSource: TFileSource;
                aExecutablePath, aVerb: UTF8String);
begin
  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FExecutablePath := aExecutablePath;
  FVerb := aVerb;

  if FFileSource.GetPathType(FExecutablePath) = ptAbsolute then
  begin
    FAbsolutePath := FExecutablePath;
    FRelativePath := ExtractDirLevel(FFileSource.CurrentPath, FExecutablePath);
  end
  else
  begin
    FAbsolutePath := FFileSource.CurrentPath + FExecutablePath;
    FRelativePath := FExecutablePath;
  end;
end;

destructor TFileSourceExecuteOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FFileSource) then
    FreeAndNil(FFileSource);
end;

procedure TFileSourceExecuteOperation.UpdateStatisticsAtStartTime;
begin
  // empty
end;

function TFileSourceExecuteOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoExecute;
end;

end.

