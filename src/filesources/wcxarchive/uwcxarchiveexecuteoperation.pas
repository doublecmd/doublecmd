unit uWcxArchiveExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uFileSourceExecuteOperation,
  uWcxArchiveFileSource;

type

  { TWcxArchiveExecuteOperation }

  TWcxArchiveExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
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

constructor TWcxArchiveExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: UTF8String);
begin
  FWcxArchiveFileSource := aTargetFileSource as IWcxArchiveFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TWcxArchiveExecuteOperation.Initialize;
begin

end;

procedure TWcxArchiveExecuteOperation.MainExecute;
begin
  FExecuteOperationResult:= ShowPackInfoDlg(FWcxArchiveFileSource, ExecutableFile);
end;

procedure TWcxArchiveExecuteOperation.Finalize;
begin

end;

end.

