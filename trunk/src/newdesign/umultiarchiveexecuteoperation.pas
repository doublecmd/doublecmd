unit uMultiArchiveExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uFileSourceExecuteOperation,
  uMultiArchiveFileSource;

type

  { TMultiArchiveExecuteOperation }

  TMultiArchiveExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
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
                       aVerb: UTF8String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  fPackInfoDlg;

constructor TMultiArchiveExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: UTF8String);
begin
  FMultiArchiveFileSource := aTargetFileSource as IMultiArchiveFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TMultiArchiveExecuteOperation.Initialize;
begin

end;

procedure TMultiArchiveExecuteOperation.MainExecute;
begin
  FExecuteOperationResult:= ShowPackInfoDlg(FMultiArchiveFileSource, ExecutableFile);
end;

procedure TMultiArchiveExecuteOperation.Finalize;
begin

end;

end.

