unit uGioListOperation;

{$macro on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uGioFileSource,
  uFileSource,
  uGLib2, uGio2;

type

  { TGioListOperation }

  TGioListOperation = class(TFileSourceListOperation)
  private
    FGioFileSource: IGioFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, uFile, DCDateTimeUtils,
  DCStrUtils, uShowMsg, DCOSUtils, uOSUtils, uGioFileSourceUtil;

{$DEFINE G_IO_ERROR:= g_io_error_quark()}

constructor TGioListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FGioFileSource := aFileSource as IGioFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TGioListOperation.MainExecute;
var
  f: PGFile;
  en: PGFileEnumerator;
  error, error_shortcut: PGError;
  info: PGFileInfo;
  target_uri: Pgchar;
  aFile: TFile;
begin
  FFiles.Clear;
  with FGioFileSource do
  begin
    f:= g_file_new_for_commandline_arg (Pgchar(Path));
    if Assigned(f) then
    begin
      while True do
      begin
        error:= nil;
      en := g_file_enumerate_children (f, CONST_DEFAULT_QUERY_INFO_ATTRIBUTES,
                                         G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, @error);

      //*  Mount the target  */
      if (Assigned(error) and g_error_matches (error, G_IO_ERROR, G_IO_ERROR_NOT_MOUNTED)) then
      begin
        g_error_free (error);
        FGioFileSource.MountPath(f);
        //Result := vfs_handle_mount (globs, f);
        //f (Result <> FS_FILE_OK) then
        begin
          //g_object_unref (PGObject(f));
          //Exit;
        end;
        //else
          continue;
      end;

      info:= g_file_enumerator_next_file (en, nil, @error);
      while Assigned(info) do
      begin

        CheckOperationState;
        aFile:= TGioFileSource.CreateFile(Path, f, info);
        FFiles.Add(aFile);

        info:= g_file_enumerator_next_file (en, nil, @error);
      end;
      Break;
      end;
      end;
    end; // FGioFileSource
end;

end.

