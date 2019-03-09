unit uGioCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uGioFileSource,
  uFile,
  uGlobs, uLog;

type

  TGioCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)

  private
    FStatistics: TFileSourceCalcStatisticsOperationStatistics;

    procedure ProcessFile(aFile: TFile);
    procedure ProcessSubDirs(const srcPath: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  uGLib2, uGObject2, uGio2, uGioFileSourceUtil;

constructor TGioCalcStatisticsOperation.Create(
                aTargetFileSource: IFileSource;
                var theFiles: TFiles);
begin
  inherited Create(aTargetFileSource, theFiles);
end;

destructor TGioCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TGioCalcStatisticsOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
end;

procedure TGioCalcStatisticsOperation.MainExecute;
var
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[CurrentFileIndex]);
    CheckOperationState;
  end;
end;

procedure TGioCalcStatisticsOperation.ProcessFile(aFile: TFile);
begin
  FStatistics.CurrentFile := aFile.Path + aFile.Name;
  UpdateStatistics(FStatistics);

  if aFile.IsLink then
  begin
    Inc(FStatistics.Links);
  end
  else if aFile.IsDirectory then
  begin
    Inc(FStatistics.Directories);
    ProcessSubDirs(aFile.Path + aFile.Name + DirectorySeparator);
  end
  else
  begin
    // Not always this will be regular file (on Unix can be socket, FIFO, block, char, etc.)
    // Maybe check with: FPS_ISREG() on Unix?

    Inc(FStatistics.Files);
    FStatistics.Size := FStatistics.Size + aFile.Size;
    if aFile.ModificationTime < FStatistics.OldestFile then
      FStatistics.OldestFile := aFile.ModificationTime;
    if aFile.ModificationTime > FStatistics.NewestFile then
      FStatistics.NewestFile := aFile.ModificationTime;
  end;

  UpdateStatistics(FStatistics);
end;

procedure TGioCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  AFile: TFile;
  AFolder: PGFile;
  AInfo: PGFileInfo;
  AFileName: Pgchar;
  AError: PGError = nil;
  AFileEnum: PGFileEnumerator;
begin
  AFolder:= g_file_new_for_commandline_arg (Pgchar(srcPath));
  try
    AFileEnum:= g_file_enumerate_children (AFolder, CONST_DEFAULT_QUERY_INFO_ATTRIBUTES,
                                           G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, @AError);

    if Assigned(AFileEnum) then
    begin
      AInfo:= g_file_enumerator_next_file (AFileEnum, nil, @AError);
      while Assigned(AInfo) do
      begin
        AFileName:= g_file_info_get_name(AInfo);

        if (aFileName <> '.') and (aFileName <> '..') then
        begin
          aFile:= TGioFileSource.CreateFile(srcPath, AFolder, AInfo);
          try
            ProcessFile(aFile);
          finally
            FreeAndNil(aFile);
          end;
        end;
        CheckOperationState;

        AInfo:= g_file_enumerator_next_file (AFileEnum, nil, @AError);
      end;
      g_object_unref(AFileEnum);
    end;
    if Assigned(AError) then
    begin
      LogMessage(AError^.message, [log_errors], lmtError);
      g_error_free(AError);
    end;
  finally
    g_object_unref(PGObject(AFolder));
  end;
end;

procedure TGioCalcStatisticsOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:
      if not (log_errors in gLogOptions) then Exit;
    lmtInfo:
      if not (log_info in gLogOptions) then Exit;
    lmtSuccess:
      if not (log_success in gLogOptions) then Exit;
  end;

  if logOptions <= gLogOptions then
  begin
    logWrite(Thread, sMessage, logMsgType);
  end;
end;

end.

