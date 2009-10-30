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
    FFileSource: IFileSource;
    FCurrentPath: String;
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
              File source where the directory should be created.)
       @param(aCurrentPath
              Path of the file source where the execution should take place.)
       @param(aExecutablePath
              Absolute or relative (to aCurrentPath) path
              to a executable that should be executed.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       aCurrentPath: String;
                       aExecutablePath, aVerb: UTF8String); virtual reintroduce;

    destructor Destroy; override;

    property CurrentPath: UTF8String read FCurrentPath;
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
                aTargetFileSource: IFileSource;
                aCurrentPath: String;
                aExecutablePath, aVerb: UTF8String);
begin
  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  FCurrentPath := aCurrentPath;
  FExecutablePath := aExecutablePath;
  FVerb := aVerb;

  if FFileSource.GetPathType(FExecutablePath) = ptAbsolute then
  begin
    FAbsolutePath := FExecutablePath;
    FRelativePath := ExtractDirLevel(aCurrentPath, FExecutablePath);
  end
  else
  begin
    FAbsolutePath := aCurrentPath + FExecutablePath;
    FRelativePath := FExecutablePath;
  end;
end;

destructor TFileSourceExecuteOperation.Destroy;
begin
  inherited Destroy;
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

