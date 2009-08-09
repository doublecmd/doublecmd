unit uFileSystemCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationUI,
  uFile,
  uFileSystemFile,
  uGlobs, uLog;

type

  TFileSystemCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)

  private
    FStatistics: TFileSourceCalcStatisticsOperationStatistics; // local copy of statistics

    procedure ProcessFile(aFile: TFileSystemFile);
    procedure ProcessLink(aFile: TFileSystemFile);
    procedure ProcessSubDirs(const srcPath: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(var aTargetFileSource: TFileSource;
                       var theFiles: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  uFileSourceOperationOptions,
  uDCUtils, uOSUtils, uLng,
  uFileSystemUtil, LCLProc,
  FileUtil, StrUtils, uFindEx;

constructor TFileSystemCalcStatisticsOperation.Create(
                var aTargetFileSource: TFileSource;
                var theFiles: TFiles);
begin
  inherited Create(aTargetFileSource, theFiles);
end;

destructor TFileSystemCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TFileSystemCalcStatisticsOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
end;

procedure TFileSystemCalcStatisticsOperation.MainExecute;
var
  aFile: TFileSystemFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    aFile := Files[CurrentFileIndex] as TFileSystemFile;

    ProcessFile(aFile);

    CheckOperationState;
  end;
end;

procedure TFileSystemCalcStatisticsOperation.ProcessFile(aFile: TFileSystemFile);
begin
  FStatistics.CurrentFile := aFile.Path + aFile.Name;
  UpdateStatistics(FStatistics);

  if aFile.IsDirectory then
  begin
    Inc(FStatistics.Directories);
    ProcessSubDirs(aFile.Path + aFile.Name + DirectorySeparator);
  end
  else if aFile.IsLink then
  begin
    Inc(FStatistics.Links);

    case FSymLinkOption of
      fsooslFollow:
        ProcessLink(aFile);
      fsooslDontFollow: ; // do nothing
      fsooslNone:
        begin
          case AskQuestion('', Format(rsMsgFollowSymlink, [aFile.Name]),
                         [fsourYes, fsourAll, fsourNo, fsourSkipAll],
                         fsourYes, fsourNo)
          of
            fsourYes:
              ProcessLink(aFile);
            fsourAll:
              begin
                FSymLinkOption := fsooslFollow;
                ProcessLink(aFile);
              end;
            fsourNo: ; // do nothing
            fsourSkipAll:
              FSymLinkOption := fsooslDontFollow;
          end;
        end;
    end;
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

procedure TFileSystemCalcStatisticsOperation.ProcessLink(aFile: TFileSystemFile);
var
  PathToLink: String;
  aLinkFile: TFileSystemFile = nil;
begin
  PathToLink := ReadSymLink(aFile.FullPath);
  if PathToLink <> '' then
  begin
    if uDCUtils.GetPathType(PathToLink) <> ptAbsolute then
      PathToLink := GetAbsoluteFileName(aFile.Path, PathToLink);

    try
      aLinkFile := TFileSystemFile.Create(PathToLink);
      try
        if aLinkFile.IsLink then
          ProcessLink(aLinkFile)
        else
          ProcessFile(aLinkFile);
      finally
        FreeAndNil(aLinkFile);
      end;

    except
      on EFileSystemFileNotExists do
        begin
          LogMessage(rsMsgErrInvalidLink + ': ' + aFile.FullPath + ' -> ' + PathToLink, [log_errors], lmtError);
        end;
    end;
  end
  else
  begin
    LogMessage(rsMsgErrInvalidLink + ': ' + aFile.FullPath + ' -> ' + PathToLink, [log_errors], lmtError);
  end;
end;

procedure TFileSystemCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  sr: TSearchRec;
  aFile: TFileSystemFile;
  FindResult: Longint;
begin
  FindResult := FindFirstEx(srcPath + '*', faAnyFile, sr);
  try
    if FindResult = 0 then
    repeat
      if (sr.Name='.') or (sr.Name='..') then Continue;

      aFile := TFileSystemFile.Create(sr);
      try
        aFile.Path := srcPath;

        ProcessFile(aFile);

      finally
        FreeAndNil(aFile);
      end;

      CheckOperationState;
    until FindNextEx(sr) <> 0;

  finally
    FindCloseEx(sr);
  end;
end;

procedure TFileSystemCalcStatisticsOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

