unit uFileSourceExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource;

type

  TFileSourceExecuteOperationResult =
     (fseorSuccess,  //<en the command was executed successfully
      fseorError,    //<en execution failed
      fseorYourSelf, //<en DC should download the file and execute it locally
      fseorSymLink); //<en this was a (symbolic) link or .lnk file pointing to a different directory

  { TFileSourceExecuteOperation }

  TFileSourceExecuteOperation = class(TFileSourceOperation)

  private
    FFileSource: TFileSource;
    FAbsolutePath: UTF8String;
    FRelativePath: UTF8String;
    FVerb: UTF8String;

  protected
    FExecutablePath: UTF8String;
    FExecuteOperationResult: TFileSourceExecuteOperationResult;
    function GetID: TFileSourceOperationType; override;
    procedure UpdateStatisticsAtStartTime; override;

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

    property ExecutablePath: UTF8String read FExecutablePath write FExecutablePath;
    property AbsolutePath: UTF8String read FAbsolutePath;
    property RelativePath: UTF8String read FRelativePath;
    property Verb: UTF8String read FVerb;
    property ExecuteOperationResult: TFileSourceExecuteOperationResult read FExecuteOperationResult;
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

