unit uFileSystemExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource,
  uFileSourceExecuteOperation,
  uFileSystemFileSource,
  uFile;

type

  { TFileSystemExecuteOperation }

  TFileSystemExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FFileSystemFileSource: IFileSystemFileSource;
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
  Forms, Controls, DCOSUtils, uOSUtils;

constructor TFileSystemExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: UTF8String);
begin
  FFileSystemFileSource := aTargetFileSource as IFileSystemFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TFileSystemExecuteOperation.Initialize;
begin
  Screen.Cursor:= crHourGlass;
end;

procedure TFileSystemExecuteOperation.MainExecute;
begin
  // if file is link to folder then return fseorSymLink
  if FileIsLinkToFolder(AbsolutePath, FResultString) then
  begin
    FExecuteOperationResult:= fseorSymLink;
    Exit;
  end;
  // try to open by system
  mbSetCurrentDir(CurrentPath);
  case ShellExecute(RelativePath) of
  True:
    FExecuteOperationResult:= fseorSuccess;
  False:
    FExecuteOperationResult:= fseorError;
  end;
end;

procedure TFileSystemExecuteOperation.Finalize;
begin
  Screen.Cursor:= crDefault;
end;

end.
