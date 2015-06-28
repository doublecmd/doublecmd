unit uGioCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperationTypes,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,

  DCOSUtils,
  uSearchTemplate,
  uGio2,
  uGLib2;

type

  { TGioCopyOperation }

  TGioCopyOperation = class(TFileSourceCopyOperation)

  private
    FCancel: PGCancellable;
    FFullFilesTreeToCopy: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics

protected
  function ProcessFile(const AFile: TFile; const ATargetPath: UTF8String): Boolean;

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  { TGioCopyInOperation }

  TGioCopyInOperation = class(TGioCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

  { TGioCopyOutOperation }

  TGioCopyOutOperation = class(TGioCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

implementation

uses
  uFileSystemUtil, fFileSystemCopyMoveOperationOptions, uGlobs, uGObject2, uDCUtils, DCStrUtils,
  uGioFileSourceUtil;

procedure ProgressCallback(current_num_bytes: gint64; total_num_bytes: gint64; user_data: gpointer); cdecl;
var
  Operation: TGioCopyOperation absolute user_data;
begin
  with Operation do
  begin
    if State = fsosStopping then  // Cancel operation
    begin
      g_cancellable_cancel(FCancel);
      Exit;
    end;

    FStatistics.CurrentFileDoneBytes:= current_num_bytes;
    FStatistics.CurrentFileTotalBytes:= total_num_bytes;
    UpdateStatistics(FStatistics);

    CheckOperationState;
  end;
end;

// -- TGioCopyOperation ---------------------------------------------

function TGioCopyOperation.ProcessFile(const AFile: TFile;
  const ATargetPath: UTF8String): Boolean;
var
     src, dst: PGFile;
    error: PGError = nil;
begin
  src:= g_file_new_for_commandline_arg(Pgchar(AFile.FullPath));
  dst:= g_file_new_for_commandline_arg(Pgchar(ATargetPath));



    FCancel := g_cancellable_new ();

    //* FIXME: Appending not supported */
    g_file_copy (src, dst, 0, FCancel, @ProgressCallback, Self, @error);
  {
  if (error) {
      g_print ("(EE) FsPutFile: g_file_copy() error: %s\n", error->message);
  //    res = g_error_to_TVFSResult (error);
      if (error->code == G_IO_ERROR_CANCELLED)
        res = FS_FILE_USERABORT;
      else
        res = FS_FILE_WRITEERROR;
      g_error_free (error);
    }
   }
    g_object_unref (FCancel);
    g_object_unref (PGObject(src));
    g_object_unref (PGObject(dst));
end;

constructor TGioCopyOperation.Create(aSourceFileSource: IFileSource;
                                            aTargetFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetPath: String);
begin
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TGioCopyOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TGioCopyOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(SourceFiles, False,
               FFullFilesTreeToCopy,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TGioCopyOperation.MainExecute;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
  AbsoluteTargetFileName: String;
begin

  for CurrentFileIndex:= 0 to FFullFilesTreeToCopy.Count - 1 do
  begin
    aFile := FFullFilesTreeToCopy[CurrentFileIndex];
    // Filenames must be relative to the current directory.
    AbsoluteTargetFileName := TargetPath + ExtractDirLevel(FFullFilesTreeToCopy.Path, aFile.Path);

   // if FRenamingRootDir then
  //    AbsoluteTargetFileName := AbsoluteTargetFileName + RenameMask
 //   else
      AbsoluteTargetFileName := AbsoluteTargetFileName + aFile.Name;// uFileSystemUtil.ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask);

    with FStatistics do
    begin
      CurrentFileFrom := aFile.FullPath;
      CurrentFileTo := AbsoluteTargetFileName;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    if aFile.IsDirectory or aFile.IsLinkToDirectory then
      //ProcessDirectory(aFile, AbsoluteTargetFileName)
    else
      ProcessFile(aFile, AbsoluteTargetFileName);

    CheckOperationState;
  end;
end;

procedure TGioCopyOperation.Finalize;
begin

end;

{ TGioCopyInOperation }

function TGioCopyInOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyIn;
end;

{ TGioCopyOutOperation }

function TGioCopyOutOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyOut;
end;

end.

