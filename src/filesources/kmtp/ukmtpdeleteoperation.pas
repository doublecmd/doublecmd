{
   Double Commander
   -------------------------------------------------------------------------
   Delete operation for KDE KMTP file source

   Copyright (C) 2026 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uKmtpDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGlobs, uLog,
  uFileSourceDeleteOperation,
  uFileSource,
  uFileSourceOperationUI,
  uFile,
  uKmtpFileSource;

type

  { TKmtpDeleteOperation }

  TKmtpDeleteOperation = class(TFileSourceDeleteOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FFullFilesTreeToDelete: TFiles;
    FStatistics: TFileSourceDeleteOperationStatistics;
    FSkipErrors: Boolean;
    procedure FillAndCount;
    function ProcessFile(aFile: TFile): Boolean;
    function ShowError(const sMessage: String): TFileSourceOperationUIResponse;
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  uLng, uKde;

{ TKmtpDeleteOperation }

constructor TKmtpDeleteOperation.Create(aTargetFileSource: IFileSource;
  var theFilesToDelete: TFiles);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  FSkipErrors := False;
  FFullFilesTreeToDelete := nil;
  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TKmtpDeleteOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFullFilesTreeToDelete);
end;

procedure TKmtpDeleteOperation.FillAndCount;
var
  I: Integer;
  aFile: TFile;

  procedure FillRec(const srcPath: String);
  var
    DevPath, StorPath, RelPath: String;
    Level, J, ExistsCode: Integer;
    KmtpFiles: TKmtpFileArray;
    ChildFile: TFile;
  begin
    if (not FKmtpFileSource.ResolveTarget(srcPath, DevPath, StorPath, RelPath, Level)) or (Level < 2) then
      Exit;
    if KmtpGetFilesAndFolders(StorPath, RelPath, KmtpFiles, ExistsCode) and (ExistsCode = 0) then
    begin
      for J := 0 to High(KmtpFiles) do
      begin
        if (KmtpFiles[J].FileName = '') or (KmtpFiles[J].FileName = '.') or (KmtpFiles[J].FileName = '..') then
          Continue;
        ChildFile := TKmtpFileSource.CreateFile(srcPath, KmtpFiles[J]);
        FFullFilesTreeToDelete.Add(ChildFile);
        if ChildFile.IsDirectory then
        begin
          Inc(FStatistics.TotalFiles);
          FillRec(srcPath + ChildFile.Name + DirectorySeparator);
        end
        else
        begin
          Inc(FStatistics.TotalFiles);
          Inc(FStatistics.TotalBytes, ChildFile.Size);
        end;
      end;
    end;
  end;

begin
  FStatistics.TotalFiles := 0;
  FStatistics.TotalBytes := 0;
  FFullFilesTreeToDelete := TFiles.Create(FilesToDelete.Path);
  for I := 0 to FilesToDelete.Count - 1 do
  begin
    aFile := FilesToDelete[I];
    FFullFilesTreeToDelete.Add(aFile.Clone);
    if aFile.IsDirectory then
    begin
      Inc(FStatistics.TotalFiles);
      FillRec(aFile.FullPath + DirectorySeparator);
    end
    else
    begin
      Inc(FStatistics.TotalFiles);
      Inc(FStatistics.TotalBytes, aFile.Size);
    end;
  end;
end;

procedure TKmtpDeleteOperation.Initialize;
begin
  FStatistics := RetrieveStatistics;
  FillAndCount;
end;

procedure TKmtpDeleteOperation.MainExecute;
var
  I: Integer;
  aFile: TFile;
begin
  for I := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[I];
    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    ProcessFile(aFile);

    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    FStatistics.DoneBytes := FStatistics.DoneBytes + aFile.Size;
    UpdateStatistics(FStatistics);
    CheckOperationState;
  end;
end;

function TKmtpDeleteOperation.ShowError(const sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError or FSkipErrors then
    Exit(fsourSkip);
  Result := AskQuestion(sMessage, '', [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort], fsourRetry, fsourAbort);
  case Result of
    fsourSkipAll: FSkipErrors := True;
    fsourAbort:   RaiseAbortOperation;
  end;
end;

procedure TKmtpDeleteOperation.LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:   if not (log_errors in gLogOptions) then Exit;
    lmtInfo:    if not (log_info in gLogOptions) then Exit;
    lmtSuccess: if not (log_success in gLogOptions) then Exit;
  end;
  if logOptions <= gLogOptions then
    logWrite(Thread, sMessage, logMsgType);
end;

function TKmtpDeleteOperation.ProcessFile(aFile: TFile): Boolean;
var
  DevPath, StorPath, RelPath, Msg: String;
  Level: Integer;
begin
  Result := False;
  if (not FKmtpFileSource.ResolveTarget(aFile.FullPath, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
    Exit(False);

  repeat
    Result := KmtpDeleteObject(StorPath, RelPath);
    if Result then
    begin
      if aFile.IsDirectory then
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogRmDir, [aFile.FullPath]), [log_vfs_op], lmtSuccess)
      else
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete, [aFile.FullPath]), [log_vfs_op], lmtSuccess);
      Exit(True);
    end;

    if aFile.IsDirectory then
      Msg := Format(rsMsgLogError + rsMsgLogRmDir, [aFile.FullPath])
    else
      Msg := Format(rsMsgLogError + rsMsgLogDelete, [aFile.FullPath]);

    LogMessage(Msg, [log_vfs_op], lmtError);
  until ShowError(Msg) <> fsourRetry;
end;

end.
