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
  LCLProc, Dialogs, uFile, DCDateTimeUtils, uGioFileSourceUtil, uGObject2, uGio;

{$DEFINE G_IO_ERROR:= g_io_error_quark()}

constructor TGioListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FGioFileSource := aFileSource as IGioFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TGioListOperation.MainExecute;
var
  AFile: TFile;
  AFolder: PGFile;
  AInfo: PGFileInfo;
  AError: PGError = nil;
  AFileEnum: PGFileEnumerator;
begin
  FFiles.Clear;
  with FGioFileSource do
  begin
    AFolder:= GioNewFile(Path);
    try
      while True do
      begin
        AFileEnum := g_file_enumerate_children (AFolder, CONST_DEFAULT_QUERY_INFO_ATTRIBUTES,
                                                G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, @AError);
        if Assigned(AError) then
        begin
          // Mount the target
          if g_error_matches(AError, G_IO_ERROR, G_IO_ERROR_NOT_MOUNTED) then
          begin
            FreeAndNil(AError);
            if FGioFileSource.MountPath(AFolder, AError) then
              Continue
            else begin
              ShowError(AError);
              Exit;
            end;
          end
          else if g_error_matches(AError, G_IO_ERROR, G_IO_ERROR_NOT_FOUND) then
          begin
            FreeAndNil(AError);
            Exit;
          end
          else begin
            ShowError(AError);
            Exit;
          end;
        end;
        Break;
      end;
      // List files
      try
        AInfo:= g_file_enumerator_next_file(AFileEnum, nil, @AError);
        while Assigned(AInfo) do
        begin
          CheckOperationState;
          AFile:= TGioFileSource.CreateFile(Path, AFolder, AInfo);
          g_object_unref(AInfo);
          FFiles.Add(AFile);
          AInfo:= g_file_enumerator_next_file(AFileEnum, nil, @AError);
        end;
        if Assigned(AError) then ShowError(AError);
      finally
        g_object_unref(AFileEnum);
      end;
    finally
      g_object_unref(PGObject(AFolder));
    end;
  end; // FGioFileSource
end;

end.

