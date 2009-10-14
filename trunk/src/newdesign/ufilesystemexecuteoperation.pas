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
    FFileSystemFileSource: TFileSystemFileSource;
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
                       aExecutablePath, aVerb: UTF8String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uOSUtils;

constructor TFileSystemExecuteOperation.Create(
                var aTargetFileSource: TFileSource;
                aExecutablePath, aVerb: UTF8String);
begin
  FFileSystemFileSource := aTargetFileSource as TFileSystemFileSource;
  inherited Create(aTargetFileSource, aExecutablePath, aVerb);
end;

procedure TFileSystemExecuteOperation.Initialize;
begin
end;

procedure TFileSystemExecuteOperation.MainExecute;
begin
  // try to open by system
  mbSetCurrentDir(FFileSystemFileSource.CurrentPath);
  ShellExecute(RelativePath);
end;

procedure TFileSystemExecuteOperation.Finalize;
begin
end;

end.

