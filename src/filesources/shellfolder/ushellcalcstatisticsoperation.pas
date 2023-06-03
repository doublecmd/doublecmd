unit uShellCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows, ShlObj, ComObj, ActiveX,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uShellFileSource,
  uFile,
  uGlobs, uLog;

type

  TShellCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)
  private
    FShellFileSource: IShellFileSource;
    FStatistics: TFileSourceCalcStatisticsOperationStatistics;

    procedure ProcessFile(aFile: TFile);
    procedure ProcessSubDirs(AParent: IShellFolder2; AObject: PItemIDList);
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
  uShellFileSourceUtil, uShellFolder;

constructor TShellCalcStatisticsOperation.Create(
                aTargetFileSource: IFileSource;
                var theFiles: TFiles);
begin
  FShellFileSource:= aTargetFileSource as IShellFileSource;
  inherited Create(aTargetFileSource, theFiles);
end;

destructor TShellCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TShellCalcStatisticsOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
end;

procedure TShellCalcStatisticsOperation.MainExecute;
var
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[CurrentFileIndex]);
    CheckOperationState;
  end;
end;

procedure TShellCalcStatisticsOperation.ProcessFile(aFile: TFile);
var
  AObject: PItemIDList;
  AFolder: IShellFolder2;
begin
  FStatistics.CurrentFile := aFile.FullPath;
  UpdateStatistics(FStatistics);

  if aFile.IsDirectory then
  begin
    Inc(FStatistics.Directories);
    if Succeeded(FShellFileSource.FindFolder(AFile.Path, AFolder)) then
    begin
      if Succeeded(FShellFileSource.FindObject(AFolder, aFile.Name, AObject)) then
      try
        ProcessSubDirs(AFolder, AObject);
      finally
        CoTaskMemFree(AObject);
      end;
    end;
  end
  else begin
    Inc(FStatistics.Files);
    Inc(FStatistics.Size, aFile.Size);
    if aFile.ModificationTime < FStatistics.OldestFile then
      FStatistics.OldestFile := aFile.ModificationTime;
    if aFile.ModificationTime > FStatistics.NewestFile then
      FStatistics.NewestFile := aFile.ModificationTime;
  end;
  UpdateStatistics(FStatistics);
end;

procedure TShellCalcStatisticsOperation.ProcessSubDirs(AParent: IShellFolder2; AObject: PItemIDList);
var
  ASize: Int64;
  PIDL: PItemIDList;
  NumIDs: LongWord = 0;
  EnumIDList: IEnumIDList;
  AFolder: IShellFolder2;
begin
  try
    OleCheck(AParent.BindToObject(AObject, nil, IID_IShellFolder2, Pointer(AFolder)));
    OleCheck(AFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_STORAGE or SHCONTF_INCLUDEHIDDEN, EnumIDList));

    while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
    try
      if GetIsFolder(AParent, PIDL) then
      begin
        Inc(FStatistics.Directories);
        ProcessSubDirs(AFolder, PIDL);
      end
      else begin
        ASize:= GetDetails(AFolder, PIDL, SCID_FileSize);
        Inc(FStatistics.Size, ASize);
        Inc(FStatistics.Files);
      end;
      CheckOperationState;
      UpdateStatistics(FStatistics);
    finally
      CoTaskMemFree(PIDL);
    end;
  except
    on E: Exception do
      LogMessage(E.Message, [log_errors], lmtError);
  end;
end;

procedure TShellCalcStatisticsOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

