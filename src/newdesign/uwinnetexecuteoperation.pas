unit uWinNetExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uFileSourceExecuteOperation,
  uWinNetFileSource;

type

  { TWinNetExecuteOperation }

  TWinNetExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FWinNetFileSource: IWinNetFileSource;
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

    procedure MainExecute; override;

  end;

implementation

uses
  uDCUtils;

constructor TWinNetExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: UTF8String);
begin
  FWinNetFileSource := aTargetFileSource as IWinNetFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TWinNetExecuteOperation.MainExecute;
begin
  FExecuteOperationResult:= fseorSymLink;
  if NumCountChars(PathDelim, ExecutableFile.FullPath) > 2 then
    SymLinkPath:= ExecutableFile.FullPath
  else
    SymLinkPath:= '\\' + ExecutableFile.Name;
end;

end.

