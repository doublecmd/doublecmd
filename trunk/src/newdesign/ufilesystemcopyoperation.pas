unit uFileSystemCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyInOperation,
  uFileSourceCopyOutOperation,
  uFileSystemFileSource,
  uFileSource,
  uFile;

type
  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  TFileSystemCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FSourceFileSource: TFileSystemFileSource;
    FTargetFileSource: TFileSystemFileSource;
    FSourceFiles: TFiles;
    FTargetFiles: TFiles;

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetFiles: TFiles); reintroduce;

    procedure Execute; override;

  end;

  TFileSystemCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FSourceFileSource: TFileSystemFileSource;
    FTargetFileSource: TFileSystemFileSource;
    FSourceFiles: TFiles;
    FTargetFiles: TFiles;

    FProgress: Integer;

  protected
    function GetProgress: Integer; override;

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetFiles: TFiles); reintroduce;

    procedure Execute; override;

  end;

implementation

// -- TFileSystemCopyInOperation ----------------------------------------------

constructor TFileSystemCopyInOperation.Create(SourceFileSource: TFileSystemFileSource;
                                              TargetFileSource: TFileSystemFileSource;
                                              SourceFiles: TFiles;
                                              TargetFiles: TFiles);
begin
  inherited Create;

  FSourceFileSource := SourceFileSource;
  FTargetFileSource := TargetFileSource;
  FSourceFiles := SourceFiles;
  FTargetFiles := TargetFiles;
end;

procedure TFileSystemCopyInOperation.Execute;
begin
end;

// -- TFileSystemCopyOutOperation ---------------------------------------------

constructor TFileSystemCopyOutOperation.Create(SourceFileSource: TFileSystemFileSource;
                                              TargetFileSource: TFileSystemFileSource;
                                              SourceFiles: TFiles;
                                              TargetFiles: TFiles);
begin
  inherited Create;

  FSourceFileSource := SourceFileSource;
  FTargetFileSource := TargetFileSource;
  FSourceFiles := SourceFiles;
  FTargetFiles := TargetFiles;

  FProgress := 0;
end;

function TFileSystemCopyOutOperation.GetProgress: Integer;
begin
  Result := FProgress;
end;

procedure TFileSystemCopyOutOperation.Execute;
var
  i: Integer;
begin
  // Some dummy long operation for now.
  for i := 1 to 300 do
  begin
    Sleep(50);
    FProgress := (i * 100)  div  300;
  end;
end;

end.

