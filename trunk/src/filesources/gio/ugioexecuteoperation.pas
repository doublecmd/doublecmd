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
  uGio, uGio2, uGObject2, uGLib2, fFileProperties, uFile;

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

  AFile:= g_file_new_for_commandline_arg(Pgchar(AbsolutePath));
  try
    AInfo := g_file_query_info (AFile, FILE_ATTRIBUTE_STANDARD_TYPE + ',' +
                                FILE_ATTRIBUTE_STANDARD_TARGET_URI,
                                G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, nil);
    if Assigned(AInfo) then
    try
      AFileType:= g_file_info_get_file_type(AInfo);
      if AFileType in [G_FILE_TYPE_SHORTCUT, G_FILE_TYPE_MOUNTABLE] then
      begin
        ResultString:= g_file_info_get_attribute_string(AInfo, FILE_ATTRIBUTE_STANDARD_TARGET_URI);
        if Length(ResultString) > 0 then
        begin
          FExecuteOperationResult:= fseorSymLink;
          Exit();
        end;
      end;
    finally
      g_object_unref(AInfo);
    end;
  finally
    g_object_unref(PGObject(AFile));
  end;

  if GioOpen(AbsolutePath) then
    FExecuteOperationResult:= fseorSuccess
  else begin
    FExecuteOperationResult:= fseorError;
  end;
end;

end.

