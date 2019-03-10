unit uGioCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCreateDirectoryOperation,
  uFileSource;

type

  TGioCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  public
    procedure MainExecute; override;
  end;

implementation

uses
  uFileSourceOperationUI, uLog, uLng, uGlobs, uGio2, uGio, uGLib2, uGObject2;

procedure TGioCreateDirectoryOperation.MainExecute;
var
  AGFile: PGFile;
  AResult: Boolean;
  AError: PGError = nil;
begin
  AGFile:= GioNewFile(AbsolutePath);
  AResult:= g_file_make_directory_with_parents(AGFile, nil, @AError);
  g_object_unref(PGObject(AGFile));
  if Assigned(AError) then
  begin
    AResult:= g_error_matches (AError, g_io_error_quark(), G_IO_ERROR_EXISTS);
    if not AResult then;
    begin
      if g_error_matches (AError, g_io_error_quark(), G_IO_ERROR_NOT_SUPPORTED) then
        AskQuestion(rsMsgErrNotSupported, '', [fsourOk], fsourOk, fsourOk)
      else begin
        // write log error
        if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
          logWrite(Thread, Format(rsMsgLogError+rsMsgLogMkDir, [AbsolutePath]), lmtError);
      end;
    end;
    g_error_free(AError);
  end;
  if AResult then
  begin
    // write log success
    if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogSuccess+rsMsgLogMkDir, [AbsolutePath]), lmtSuccess)
  end
end;

end.

