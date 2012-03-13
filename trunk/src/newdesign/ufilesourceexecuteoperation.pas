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

  TFileSourceExecuteOperationResult =
     (fseorSuccess,   //<en the command was executed successfully
      fseorError,     //<en execution failed
      fseorCancelled, //<en cancelled by user (nothing happened)
      fseorYourSelf,  //<en DC should download/extract the file and execute it locally
      fseorWithAll,   //<en DC should download/extract all files and execute chosen file locally
      fseorSymLink);  //<en this was a (symbolic) link or .lnk file pointing to a different directory

  { TFileSourceExecuteOperation }

  TFileSourceExecuteOperation = class(TFileSourceOperation)

  private
    FFileSource: IFileSource;
    FCurrentPath: String;
    FExecutableFile: TFile;
    FAbsolutePath: UTF8String;
    FRelativePath: UTF8String;
    FVerb: UTF8String;

  protected
    FResultString: UTF8String;
    FExecuteOperationResult: TFileSourceExecuteOperationResult;
    function GetID: TFileSourceOperationType; override;
    procedure UpdateStatisticsAtStartTime; override;
    procedure DoReloadFileSources; override;

  public
    {en
       @param(aTargetFileSource
              File source where the file should be executed.)
       @param(aExecutableFile
              File that should be executed.)
       @param(aCurrentPath
              Path of the file source where the execution should take place.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       var aExecutableFile: TFile;
                       aCurrentPath,
                       aVerb: UTF8String); virtual reintroduce;

    destructor Destroy; override;

    property CurrentPath: UTF8String read FCurrentPath;
    property ExecutableFile: TFile read FExecutableFile;
    property ResultString: UTF8String read FResultString write FResultString;
    property AbsolutePath: UTF8String read FAbsolutePath;
    property RelativePath: UTF8String read FRelativePath;
    property Verb: UTF8String read FVerb;
    property ExecuteOperationResult: TFileSourceExecuteOperationResult read FExecuteOperationResult;
  end;

implementation

constructor TFileSourceExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: UTF8String);
begin
  inherited Create(aTargetFileSource);

  FFileSource := aTargetFileSource;
  FCurrentPath := aCurrentPath;
  FExecutableFile := aExecutableFile;
  aExecutableFile := nil;
  FVerb := aVerb;
  FExecuteOperationResult := fseorCancelled;

  FAbsolutePath := FExecutableFile.FullPath;
  FRelativePath := FExecutableFile.Name;
end;

destructor TFileSourceExecuteOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FExecutableFile);
end;

procedure TFileSourceExecuteOperation.UpdateStatisticsAtStartTime;
begin
  // empty
end;

function TFileSourceExecuteOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoExecute;
end;

procedure TFileSourceExecuteOperation.DoReloadFileSources;
begin
  if FExecuteOperationResult <> fseorCancelled then
    FFileSource.Reload(FCurrentPath);
end;

end.
