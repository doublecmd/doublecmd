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
                       aVerb: String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  Forms, Controls, DCOSUtils, uOSUtils, uOSForms, uShellContextMenu, uExceptions;

constructor TFileSystemExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: String);
begin
  FFileSystemFileSource := aTargetFileSource as IFileSystemFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TFileSystemExecuteOperation.Initialize;
begin
  Screen.Cursor:= crHourGlass;
end;

procedure TFileSystemExecuteOperation.MainExecute;
var
  aFiles: TFiles;
begin
  if Verb = 'properties' then
  begin
    FExecuteOperationResult:= fseorSuccess;
    aFiles:= TFiles.Create(ExecutableFile.Path);
    try
      aFiles.Add(ExecutableFile.Clone);
      try
        Screen.Cursor:= crDefault;
        ShowFilePropertiesDialog(FFileSystemFileSource, aFiles);
      except
        on E: EContextMenuException do
          ShowException(E);
      end;
    finally
      FreeAndNil(aFiles);
    end;
    Exit;
  end;
  // if file is link to folder then return fseorSymLink
  if FileIsLinkToFolder(AbsolutePath, FResultString) then
  begin
    FExecuteOperationResult:= fseorSymLink;
    Exit;
  end;
  // try to open by system
  mbSetCurrentDir(CurrentPath);
  case ShellExecute(AbsolutePath) of
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

