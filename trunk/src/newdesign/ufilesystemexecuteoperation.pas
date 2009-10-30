unit uFileSystemExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource,
  uFileSourceExecuteOperation,
  uFileSystemFileSource;

type

  { TFileSystemExecuteOperation }

  TFileSystemExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FFileSystemFileSource: IFileSystemFileSource;
  public
    {en
       @param(aTargetFileSource
              File source where the directory should be created.
              Class takes ownership of the pointer.)
       @param(aCurrentPath
              Path of the file source where the execution should take place.)
       @param(aExecutablePath
              Absolute or relative (to aCurrentPath) path
              to a executable that should be executed.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       aCurrentPath: String;
                       aExecutablePath, aVerb: UTF8String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  Forms, Controls, uOSUtils;

constructor TFileSystemExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                aCurrentPath: String;
                aExecutablePath, aVerb: UTF8String);
begin
  FFileSystemFileSource := aTargetFileSource as IFileSystemFileSource;
  inherited Create(aTargetFileSource, aCurrentPath, aExecutablePath, aVerb);
end;

procedure TFileSystemExecuteOperation.Initialize;
begin
  Screen.Cursor:= crHourGlass;
end;

procedure TFileSystemExecuteOperation.MainExecute;
begin
  // try to open by system
  mbSetCurrentDir(CurrentPath);
  ShellExecute(RelativePath);
end;

procedure TFileSystemExecuteOperation.Finalize;
begin
  Screen.Cursor:= crDefault;
end;

end.

