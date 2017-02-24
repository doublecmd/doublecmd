unit uGioExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource,
  uFileSourceExecuteOperation;

type

  { TGioExecuteOperation }

  TGioExecuteOperation = class(TFileSourceExecuteOperation)
  public
    procedure MainExecute; override;
  end;

implementation

uses
  DCFileAttributes, uGio, uGio2, uGObject2, uGLib2, fFileProperties, uFile;

procedure TGioExecuteOperation.MainExecute;
var
  AFile: PGFile;
  AFiles: TFiles;
  AInfo: PGFileInfo;
  AFileType: TGFileType;
begin
  if Verb = 'properties' then
  begin
    AFiles:= TFiles.Create(CurrentPath);
    try
      AFiles.Add(ExecutableFile.Clone);
      ShowFileProperties(FileSource as IFileSource, AFiles);
    finally
      AFiles.Free;
    end;
    Exit;
  end;

  if (ExecutableFile.Attributes and S_IFMT) = (S_IFDIR or S_IFLNK) then
  begin
    ResultString:= ExecutableFile.LinkProperty.LinkTo;
    if Length(ResultString) > 0 then
    begin
      FExecuteOperationResult:= fseorSymLink;
      Exit();
    end;
  end;

  if GioOpen(AbsolutePath) then
    FExecuteOperationResult:= fseorSuccess
  else begin
    FExecuteOperationResult:= fseorError;
  end;
end;

end.

